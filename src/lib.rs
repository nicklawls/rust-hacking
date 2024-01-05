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

#[derive(Debug)]
pub enum Dst {
    Reg(Register),
    Ea(EffectiveAddress),
}

#[derive(Debug)]
pub enum Src {
    Reg(Register),
    Imm8(u8),
    Imm16(u16),
    Ea(EffectiveAddress),
}

#[derive(Debug)]
pub enum Instruction {
    /** Move Destination to Source */
    Mov { dst: Dst, src: Src },
}

/// Pretty print instructions as ASM.
pub fn pp_asm(instruction: &Instruction) -> String {
    fn pp_register(reg: &Register) -> String {
        format!("{reg:?}").to_ascii_lowercase()
    }

    fn pp_effective_address(ea: &EffectiveAddress) -> String {
        fn pp_formula(registers: Vec<Register>, displacement: Option<&Displacement>) -> String {
            let mut reg_str = registers
                .iter()
                .map(pp_register)
                .collect::<Vec<_>>()
                .join(" + ");

            match displacement {
                Some(Displacement::D8(disp) | Displacement::D16(disp)) => {
                    if disp.is_positive() {
                        reg_str.push_str(format!(" + {disp}").as_str())
                    }

                    if disp.is_negative() {
                        reg_str.push_str(format!(" - {}", disp.abs()).as_str())
                    }
                }
                None => {}
            };

            return reg_str;
        }

        type R = Register;
        type EA = EffectiveAddress;

        let formula = match ea {
            EA::BxSi(disp) => pp_formula(vec![R::BX, R::SI], disp.as_ref()),
            EA::BxDi(disp) => pp_formula(vec![R::BX, R::DI], disp.as_ref()),
            EA::BpSi(disp) => pp_formula(vec![R::BP, R::SI], disp.as_ref()),
            EA::BpDi(disp) => pp_formula(vec![R::BP, R::DI], disp.as_ref()),
            EA::SI(disp) => pp_formula(vec![R::SI], disp.as_ref()),
            EA::DI(disp) => pp_formula(vec![R::DI], disp.as_ref()),
            EA::DirectAddress(disp_16) => disp_16.to_string(),
            EA::BP(some_disp) => pp_formula(vec![R::BP], Some(some_disp)),
            EA::BX(disp) => pp_formula(vec![R::BX], disp.as_ref()),
        };

        return format!("[{formula}]");
    }

    match instruction {
        Instruction::Mov { dst, src } => {
            let dst = match dst {
                Dst::Reg(r) => pp_register(r),
                Dst::Ea(ea) => pp_effective_address(ea),
            };
            let src = match src {
                Src::Reg(x) => pp_register(x),
                Src::Imm8(x) => x.to_string(),
                Src::Imm16(x) => x.to_string(),
                Src::Ea(ea) => pp_effective_address(ea),
            };
            format!("mov {dst}, {src}")
        }
    }
}

#[derive(Debug)]

/// "If the displacement is only a single byte, the 8086 or 8088 automatically
/// sign-extends this quantity to 16-bits before using the information in
/// further address calculations"
/// implying that displacements are kind of always signed after decoding
pub enum Displacement {
    D8(i16),
    D16(i16),
}

#[derive(Debug)]
pub enum EffectiveAddress {
    BxSi(Option<Displacement>),
    BxDi(Option<Displacement>),
    BpSi(Option<Displacement>),
    BpDi(Option<Displacement>),
    SI(Option<Displacement>),
    DI(Option<Displacement>),
    DirectAddress(u16),
    BP(Displacement),
    BX(Option<Displacement>),
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

pub fn decode_instruction_stream<I>(
    instruction_stream: I,
) -> Result<Vec<Instruction>, (Vec<Instruction>, String)>
where
    I: IntoIterator<Item = u8>,
{
    let mut instructions: Vec<Instruction> = vec![];
    let mut instruction_iter = instruction_stream.into_iter();

    while let Some(byte_1) = instruction_iter.next() {
        // wanted a fn to return a result because trying to bind a result to the
        // match opcode_6 expr wasn't working right inside the while loop. was
        // it getting confused about control flow?

        // closure because I'm too lazy to type out parameters
        let mut decode_instruction = || {
            let opcode_4 = byte_1 >> 4;
            let opcode_6 = byte_1 >> 2;
            let opcode_7 = byte_1 >> 1;
            match opcode_6 {
                _ if opcode_4 == 0b1011 => {
                    let w_bit = ((byte_1 & 0b00001000) >> 3) != 0;
                    let reg_field = byte_1 & 0b00000111;
                    let dst = Dst::Reg(decode_register(reg_field, w_bit)?);

                    let byte_2 = instruction_iter
                        .next()
                        .ok_or("missing byte 2 of reg->imm")?;

                    let src = match w_bit {
                        true => {
                            let byte_3 = instruction_iter
                                .next()
                                .ok_or("missing byte 3 of reg->imm")?;
                            let imm_16 = build_u16(byte_3, byte_2);
                            Src::Imm16(imm_16)
                        }
                        false => Src::Imm8(byte_2),
                    };

                    return Ok(Instruction::Mov { dst, src });
                }
                // MOV
                0b100010 => {
                    let d_bit = ((byte_1 & 0b00000010) >> 1) != 0;
                    let w_bit = (byte_1 & 0b00000001) != 0;
                    let byte_2 = instruction_iter
                        .next()
                        .ok_or("missing byte 2 of reg->reg")?;
                    let mod_field = byte_2 >> 6;
                    let reg_field = (byte_2 & 0b00111000) >> 3;
                    let r_m_field = byte_2 & 0b00000111;

                    let reg_register = decode_register(reg_field, w_bit)?;

                    match mod_field {
                        0b11 => {
                            let r_m_register = decode_register(r_m_field, w_bit)?;
                            // if d is 1, REG is the dest, meaning R/M is the source
                            let (dst, src) = match d_bit {
                                true => (reg_register, r_m_register),
                                false => (r_m_register, reg_register),
                            };
                            return Ok(Instruction::Mov {
                                dst: Dst::Reg(dst),
                                src: Src::Reg(src),
                            });
                        }

                        _ => {
                            let address = decode_effective_address(
                                mod_field,
                                r_m_field,
                                &mut instruction_iter,
                            )?;

                            return Ok({
                                let (dst, src) = match d_bit {
                                    true => (Dst::Reg(reg_register), Src::Ea(address)),
                                    false => (Dst::Ea(address), Src::Reg(reg_register)),
                                };
                                Instruction::Mov { dst, src }
                            });
                        }
                    }
                }
                _ if opcode_7 == 0b1100011 => {
                    let w_bit = (byte_1 & 0b00000001) != 0;
                    let byte_2 = instruction_iter
                        .next()
                        .ok_or("missing byte 2 of imm->reg")?;
                    let mod_field = byte_2 >> 6;
                    let reg_field = (byte_2 & 0b00111000) >> 3; // always 000, unused
                    if reg_field != 0 {
                        return Err("mem->reg: reg field not 0b000".to_string());
                    };
                    let r_m_field = byte_2 & 0b00000111;

                    // WARN: this must appear first! it mutates instruction_iter.
                    let dst = Dst::Ea(decode_effective_address(
                        mod_field,
                        r_m_field,
                        &mut instruction_iter,
                    )?);

                    let data_low = instruction_iter
                        .next()
                        .ok_or("Missing byte 3 of imm->reg")?;

                    let src = match w_bit {
                        true => {
                            let data_high = instruction_iter
                                .next()
                                .ok_or("Missing byte 4 of imm->reg")?;
                            Src::Imm16(build_u16(data_high, data_low))
                        }
                        false => Src::Imm8(data_low),
                    };

                    return Ok(Instruction::Mov { dst, src });
                }
                _ => return Err(format!("Unknown opcode: {opcode_6:#b}")),
            }
        };

        match decode_instruction() {
            Ok(instruction) => instructions.push(instruction),
            Err(e) => return Err((instructions, e)),
        };
    }

    return Ok(instructions);
}

fn decode_effective_address<I>(
    mod_field: u8,
    r_m_field: u8,
    instruction_iter: &mut I,
) -> Result<EffectiveAddress, String>
where
    I: Iterator<Item = u8>,
{
    type EA = EffectiveAddress;

    match mod_field {
        0b00 => {
            let r_m_address = match r_m_field {
                0b000 => Ok(EA::BxSi(None)),
                0b001 => Ok(EA::BxDi(None)),
                0b010 => Ok(EA::BpSi(None)),
                0b011 => Ok(EA::BpDi(None)),
                0b100 => Ok(EA::SI(None)),
                0b101 => Ok(EA::DI(None)),
                0b110 => {
                    let byte_3 = instruction_iter.next().ok_or("special case byte 3")?;
                    let byte_4 = instruction_iter.next().ok_or("special case byte 4")?;
                    Ok(EA::DirectAddress(build_u16(byte_4, byte_3)))
                }
                0b111 => Ok(EA::BX(None)),
                _ => Err("more than 3 bits for r_m when mod = 0b00"),
            }?;

            return Ok(r_m_address);
        }
        0b01 => {
            let byte_3 = instruction_iter.next().ok_or("MOD=01 byte 3")?;
            let d = Displacement::D8(byte_3 as i8 as i16);
            let r_m_address = match r_m_field {
                0b000 => Ok(EA::BxSi(Some(d))),
                0b001 => Ok(EA::BxDi(Some(d))),
                0b010 => Ok(EA::BpSi(Some(d))),
                0b011 => Ok(EA::BpDi(Some(d))),
                0b100 => Ok(EA::SI(Some(d))),
                0b101 => Ok(EA::DI(Some(d))),
                0b110 => Ok(EA::BP(d)),
                0b111 => Ok(EA::BX(Some(d))),
                _ => Err("more than 3 bits for r_m when MOD = 0b01"),
            }?;

            return Ok(r_m_address);
        }
        0b10 => {
            let byte_3 = instruction_iter.next().ok_or("MOD=11 byte 3")?;
            let byte_4 = instruction_iter.next().ok_or("MOD=11 byte 3")?;
            let d = Displacement::D16(build_u16(byte_4, byte_3) as i16);
            let r_m_address = match r_m_field {
                0b000 => Ok(EA::BxSi(Some(d))),
                0b001 => Ok(EA::BxDi(Some(d))),
                0b010 => Ok(EA::BpSi(Some(d))),
                0b011 => Ok(EA::BpDi(Some(d))),
                0b100 => Ok(EA::SI(Some(d))),
                0b101 => Ok(EA::DI(Some(d))),
                0b110 => Ok(EA::BP(d)),
                0b111 => Ok(EA::BX(Some(d))),
                _ => Err("more than 3 bits for r_m when MOD = 0b11"),
            }?;

            return Ok(r_m_address);
        }
        // register -> register
        _ => return Err(format!("unknown field in mod: {mod_field:#b}")),
    }
}

/// In this ISA, later-coming bytes are the hight bytes
fn build_u16(high_byte: u8, low_byte: u8) -> u16 {
    ((high_byte as u16) << 8) | (low_byte as u16)
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

fn decode_register(field: u8, w_bit: bool) -> Result<Register, String> {
    let table = match w_bit {
        true => W_1_REGISTERS,
        false => W_0_REGISTERS,
    };
    return table
        .get(field as usize)
        .map(|x| *x)
        .ok_or("missing register".to_string());
}
