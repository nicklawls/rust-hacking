#[derive(Debug, PartialEq, Eq)]

enum Op {
    And,
    Or,
}

#[derive(Debug)]
enum QueryPart {
    Equals { field: String, operand: String },
    SubQuery { operator: Op, operand: Vec<usize> },
}

#[derive(Debug)]
struct FullQuery {
    storage: Vec<QueryPart>,
    root: usize,
}

#[derive(Debug)]
enum PrintError {
    MissingIndex(usize),
    ShortSubQuery,
}

impl FullQuery {
    fn to_sql(self: &FullQuery) -> Result<String, PrintError> {
        if let Some(root_node) = self.storage.get(self.root) {
            return self.to_sub_sql(root_node);
        } else {
            return Err(PrintError::MissingIndex(self.root));
        }
    }

    fn to_sub_sql(self: &FullQuery, part: &QueryPart) -> Result<String, PrintError> {
        match part {
            QueryPart::Equals { field, operand } => Ok(format!("{}={}", field, operand)),
            QueryPart::SubQuery { operator, operand } => {
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
    let example = FullQuery {
        root: 2,
        storage: Vec::from([
            QueryPart::Equals {
                field: "a".to_string(),
                operand: "2".to_string(),
            },
            QueryPart::Equals {
                field: "a".to_string(),
                operand: "3".to_string(),
            },
            QueryPart::SubQuery {
                operator: Op::Or,
                operand: Vec::from([0, 1]),
            },
        ]),
    };

    println!("{:?}", example.to_sql())
}
