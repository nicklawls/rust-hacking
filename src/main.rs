use serde::Deserialize;
use serde_json;

#[derive(Debug, PartialEq, Eq, Deserialize)]

enum Op {
    #[serde(rename(deserialize = "AND"))]
    And,
    #[serde(rename(deserialize = "OR"))]
    Or,
}

#[derive(Debug, Deserialize)]
#[serde(tag = "operator")]
enum QueryPart<A> {
    #[serde(rename(deserialize = "="))]
    Equals { field: String, operand: String },
    #[serde(untagged)]
    SubQuery {
        #[serde(rename(deserialize = "operator"))]
        op: Op,
        operand: Vec<A>,
    },
}

#[derive(Debug, Deserialize)]
struct QueryBoxed(Box<QueryPart<QueryBoxed>>);

#[derive(Debug)]
struct QueryFlat {
    /** Topological ascending */
    storage: Vec<QueryPart<usize>>,
}

#[derive(Debug)]
enum PrintError {
    MissingIndex(usize),
    ShortSubQuery,
}

impl QueryFlat {
    fn to_sql(&self) -> Result<String, PrintError> {
        if let Some(root_node) = self.storage.first() {
            return self.to_sub_sql(root_node);
        } else {
            return Err(PrintError::MissingIndex(0));
        }
    }

    fn to_sub_sql(&self, part: &QueryPart<usize>) -> Result<String, PrintError> {
        match part {
            QueryPart::Equals { field, operand } => Ok(format!("{}={}", field, operand)),
            QueryPart::SubQuery {
                op: operator,
                operand,
            } => {
                if operand.len() < 2 {
                    return Err(PrintError::ShortSubQuery);
                }

                let op_str = if *operator == Op::And { "AND" } else { "OR" };
                operand
                    .iter()
                    .map(|op_index| {
                        self.storage
                            .get(*op_index)
                            .ok_or(PrintError::MissingIndex(*op_index))
                            .and_then(|part| self.to_sub_sql(part))
                            .map(|sub_query_str| format!("({})", sub_query_str))
                    })
                    .collect::<Result<Vec<String>, PrintError>>()
                    .map(|sub_query_parts| sub_query_parts.join(&format!(" {} ", op_str)))
            }
        }
    }
}

fn main() {
    let data = r#"
        {
            "operator": "OR",
            "operand": [
                {
                    "operator": "AND",
                    "operand": [
                        {
                            "operator": "=",
                            "field": "a",
                            "operand": "1"
                        },
                        {
                            "operator": "=",
                            "field": "b",
                            "operand": "2"
                        },
                        {
                            "operator": "=",
                            "field": "c",
                            "operand": "3"
                        }
                    ]
                },
                {
                    "operator": "=",
                    "field": "a",
                    "operand": "2"
                }
            ]
        }
    "#;
    let example2 = serde_json::from_str::<QueryBoxed>(data);
    println!("{:?}", example2);

    let example = QueryFlat {
        storage: Vec::from([
            QueryPart::SubQuery {
                op: Op::Or,
                operand: Vec::from([1, 2]),
            },
            QueryPart::Equals {
                field: "a".to_string(),
                operand: "2".to_string(),
            },
            QueryPart::Equals {
                field: "a".to_string(),
                operand: "3".to_string(),
            },
        ]),
    };

    println!("{:?}", example.to_sql())
}
