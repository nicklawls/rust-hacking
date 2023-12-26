#[inline]
pub fn fibonacci(n: u64) -> u64 {
    match n {
        0 => 1,
        1 => 1,
        n => fibonacci(n - 1) + fibonacci(n - 2),
    }
}

use serde::Deserialize;

#[derive(Debug, PartialEq, Eq, Deserialize, Clone, Copy)]

pub enum Op {
    #[serde(rename(deserialize = "AND"))]
    And,
    #[serde(rename(deserialize = "OR"))]
    Or,
}

#[derive(Debug, Deserialize)]
#[serde(tag = "operator")]
pub enum QueryPart<A> {
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
pub struct QueryBoxed(Box<QueryPart<QueryBoxed>>);

impl QueryBoxed {
    pub fn to_sql_ref(&self) -> Result<String, PrintError> {
        self.to_flat_ref().to_sql()
    }
    pub fn to_flat_ref(&self) -> QueryFlat {
        let mut traversal = vec![self];
        let mut storage = vec![];

        while let Some(next) = traversal.pop() {
            match *(next.0) {
                QueryPart::Equals {
                    ref field,
                    ref operand,
                } => storage.push(QueryPart::Equals {
                    field: field.to_string(),
                    operand: operand.to_string(),
                }),
                QueryPart::SubQuery { op, ref operand } => {
                    let len = operand.len();
                    for mut o in operand {
                        traversal.push(&mut o)
                    }
                    storage.push(QueryPart::SubQuery {
                        op,
                        operand: (0..len).map(|ix| ix + 1 + storage.len()).collect(),
                    })
                }
            }
        }

        return QueryFlat { storage };
    }
    pub fn to_flat(self) -> QueryFlat {
        let mut traversal = vec![self];
        let mut storage = vec![];

        while let Some(next) = traversal.pop() {
            match *(next.0) {
                QueryPart::Equals { field, operand } => {
                    storage.push(QueryPart::Equals { field, operand })
                }
                QueryPart::SubQuery { op, mut operand } => {
                    let len = operand.len();
                    traversal.append(&mut operand);
                    storage.push(QueryPart::SubQuery {
                        op,
                        operand: (0..len).map(|ix| ix + 1 + storage.len()).collect(),
                    })
                }
            }
        }

        return QueryFlat { storage };
    }
}

#[derive(Debug)]
pub struct QueryFlat {
    /** Topological ascending */
    storage: Vec<QueryPart<usize>>,
}

#[derive(Debug)]
pub enum PrintError {
    MissingIndex(usize),
    ShortSubQuery,
}

impl QueryFlat {
    pub fn to_sql(&self) -> Result<String, PrintError> {
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
