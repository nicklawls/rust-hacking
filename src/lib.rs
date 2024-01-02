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
                    for o in operand {
                        traversal.push(o)
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

#[derive(Debug, PartialEq)]
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

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    pub fn arb_op() -> impl Strategy<Value = Op> {
        prop_oneof![Just(Op::And), Just(Op::Or)]
    }

    pub fn arb_query() -> impl Strategy<Value = QueryBoxed> {
        ("[a-z]+", "[a-z]+")
            .prop_map(|(field, operand)| QueryBoxed(Box::new(QueryPart::Equals { field, operand })))
            .prop_recursive(8, 256, 10, |inner| {
                arb_op().prop_flat_map(move |op| {
                    prop::collection::vec(inner.clone(), 0..10).prop_map(move |operand| {
                        QueryBoxed(Box::new(QueryPart::SubQuery { op, operand }))
                    })
                })
            })
    }

    proptest! {
        #[test]
        fn all_impls_match(q in arb_query()) {
            let two = q.to_sql_ref();
            let three = q.to_sql_direct_stack();
            let four = q.to_sql_direct_rec();
            let one = q.to_sql();

            assert_eq!(one, two);
            assert_eq!(one, three);
            assert_eq!(two, four);
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    /** Move Destination to Source */
    Mov { dst: Register, src: Register },
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Mov { dst, src } => {
                write!(f, "mov {dst},{src}")
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Register {
    // Low byte
    AL,
    BL,
    CL,
    DL,

    // High byte
    AH,
    BH,
    CH,
    DH,

    // Both bytes
    AX,
    BX,
    CX,
    DX,

    // ???
    SP,
    BP,
    SI,
    DI,
}

impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = format!("{self:?}").to_ascii_lowercase();
        write!(f, "{str}")
    }
}

pub fn decode_instruction_stream<I>(instruction_stream: I) -> Result<Vec<Instruction>, String>
where
    I: IntoIterator<Item = u8>,
{
    let mut result = vec![];
    let mut iter = instruction_stream.into_iter();

    while let Some(byte_1) = iter.next() {
        let opcode = byte_1 >> 2;
        let d_bit = byte_1 & 0b00000010 >> 1;
        let w_bit = byte_1 & 0b00000001;

        match opcode {
            // MOV
            0b100010 => {
                if let Some(byte_2) = iter.next() {
                    let mod_field = byte_2 >> 6;
                    let reg_field = byte_2 & 0b00111000 >> 3;
                    let r_m_field = byte_2 & 0b00000111;

                    match mod_field {
                        0b11 => {
                            // register -> register
                            let reg_register = register_encoding(reg_field, w_bit)?;
                            let r_m_register = register_encoding(r_m_field, w_bit)?;
                            // if d is 1, REG is the dest, meaning R/M is the source
                            let (dst, src) = if d_bit == 0b1 {
                                (reg_register, r_m_register)
                            } else {
                                (r_m_register, reg_register)
                            };
                            result.push(Instruction::Mov{dst, src})
                        }
                        _ => {
                            eprintln!("unknown mod field in MOV")
                        }
                    }
                } else {
                    return Err("missing byte 2".to_string());
                }
            }
            _ => {
                eprintln!("unknown opcode")
            }
        }
    }

    return Ok(result);
}

type Reg = Register;
const W_0_REGISTERS: [Register; 8] = [
    Reg::AL,
    Reg::CL,
    Reg::DL,
    Reg::BL,
    Reg::AH,
    Reg::CH,
    Reg::DH,
    Reg::BH,
];

const W_1_REGISTERS: [Register; 8] = [
    Reg::AX,
    Reg::CX,
    Reg::DX,
    Reg::BX,
    Reg::SP,
    Reg::BP,
    Reg::SI,
    Reg::DI,
];

fn register_encoding(field: u8, w_bit: u8) -> Result<Register, String> {
    let table = if w_bit == 0 {
        W_0_REGISTERS
    } else {
        W_1_REGISTERS
    };
    return table
        .get(field as usize)
        .map(|x| *x)
        .ok_or("missing register".to_string());
}
