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

impl FullQuery {
    fn to_sql(self: &FullQuery) -> Option<String> {
        if let Some(root_node) = self.storage.get(self.root) {
            return Some(self.to_sub_sql(root_node));
        } else {
            return None;
        }
    }

    fn to_sub_sql(self: &FullQuery, part: &QueryPart) -> String {
        match part {
            QueryPart::Equals { field, operand } => format!("{}={}", field, operand),
            QueryPart::SubQuery { operator, operand } => operand
                .iter()
                .filter_map(|op_index| self.storage.get(*op_index))
                .map(|subquery| format!("({})", &self.to_sub_sql(subquery) ))
                .collect::<Vec<String>>()
                .join(if *operator == Op::And { " AND " } else { " OR " }),
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

