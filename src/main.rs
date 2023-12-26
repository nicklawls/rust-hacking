use serde::Deserialize;
use serde_json;

#[derive(Debug, PartialEq, Eq, Deserialize, Clone, Copy)]

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

impl QueryBoxed {
    fn to_flat(self) -> QueryFlat {
        let mut traversal = vec![self];
        let mut storage = vec![];

        while let Some(next) = traversal.pop() {
            match *(next.0) {
                QueryPart::Equals {
                    field,
                    operand,
                } => storage.push(QueryPart::Equals {
                    field,
                    operand,
                }),
                QueryPart::SubQuery { op, mut operand } => {
                    let len = operand.len();
                    traversal.append(&mut operand);
                    storage.push(QueryPart::SubQuery {
                        op,
                        operand: (0..len)
                            .map(|ix| ix + 1 + storage.len())
                            .collect(),
                    })
                }
            }
        }

        return QueryFlat { storage };
    }
}

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
        self.storage
            .first()
            .ok_or_else(|| PrintError::MissingIndex(0))
            .and_then(|root_node| self.to_sub_sql(root_node))
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
                    .rev()
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
    if let Ok(example2) = serde_json::from_str::<QueryBoxed>(data) {
        let flat = example2.to_flat();

        let sql = flat.to_sql();
        // because to_sql takes a reference, we can use the result from flat
        // after. But tradeoff, we have to copy the Strings over, they can't
        // change ownership / move.
        // Is there a way to have the QueryFlat be a guaranteed immutable "view"
        // into the same underlying data?
        // remember that String is a "smart pointer", copying it is a "move" rather
        // than a deep copy. So it is more like the "view" idea than I thought.
        // println!("{:#?}", example2); // this errors@!
        println!("{:#?}", flat);
        println!("{:#?}", sql);
    }
}
