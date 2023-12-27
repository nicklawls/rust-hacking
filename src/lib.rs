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
    pub fn to_sql_direct_rec(&self) -> Result<String, PrintError> {
        match *self.0 {
            QueryPart::Equals {
                ref field,
                ref operand,
            } => Ok(format!("{}={}", field, operand)),
            QueryPart::SubQuery { op, ref operand } => {
                if operand.len() < 2 {
                    return Err(PrintError::ShortSubQuery);
                }

                operand
                    .iter()
                    .map(|subquery| subquery.to_sql_direct_rec().map(|val| format!("({})", val)))
                    .collect::<Result<Vec<_>, _>>()
                    .map(|val| val.join(&format!(" {} ", if op == Op::And { "AND" } else { "OR" })))
            }
        }
    }

    pub fn to_sql_direct_stack(&self) -> Result<String, PrintError> {
        let mut result = "".to_string();

        enum StackFrame<'a> {
            Append(String),
            Query(&'a QueryBoxed),
        }
        let mut stack = vec![StackFrame::Query(self)];

        while let Some(frame) = stack.pop() {
            match frame {
                StackFrame::Append(str) => result.push_str(&str),
                StackFrame::Query(QueryBoxed(next)) => match **next {
                    QueryPart::Equals {
                        ref field,
                        ref operand,
                    } => stack.push(StackFrame::Append(format!("{}={}", field, operand))),
                    QueryPart::SubQuery { op, ref operand } => {
                        if operand.len() < 2 {
                            return Err(PrintError::ShortSubQuery);
                        }

                        for (i, subquery) in operand.iter().rev().enumerate() {
                            stack.push(StackFrame::Append(")".to_string()));
                            stack.push(StackFrame::Query(subquery));
                            stack.push(StackFrame::Append("(".to_string()));

                            // let sep = format!("{:#?}", *op).to_ascii_uppercase();
                            let sep = if op == Op::And { "AND" } else { "OR" };
                            if i != (operand.len() - 1) {
                                stack.push(StackFrame::Append(format!(" {} ", sep)))
                            }
                        }
                    }
                },
            }
        }
        return Ok(result);
    }
    pub fn to_sql(self) -> Result<String, PrintError> {
        self.to_flat().to_sql()
    }
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
