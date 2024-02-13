use std::collections::HashMap;

#[derive(Debug)]
pub enum Dst {
    Reg { reg: Register },
    Ea { ea: EffectiveAddress },
}

#[derive(Debug)]
pub enum Src {
    Reg { reg: Register },
    Imm8 { imm: u8 },
    Imm16 { imm: u16 },
    ImmSigned16 { imm: i16 },
    Ea { ea: EffectiveAddress },
}

#[derive(Debug)]
pub enum Op {
    Mov,
    Add,
    Sub,
    Cmp,
}

#[derive(Debug)]
pub struct Instruction {
    op: Op,
    dst: Dst,
    src: Src,
}

#[derive(Debug)]

/// "If the displacement is only a single byte, the 8086 or 8088 automatically
/// sign-extends this quantity to 16-bits before using the information in
/// further address calculations"
/// implying that displacements are kind of always signed after decoding
pub enum Displacement {
    D8 { d8: i16 },
    D16 { d16: i16 },
}

#[derive(Debug)]
pub enum EffectiveAddress {
    BxSi { disp: Option<Displacement> },
    BxDi { disp: Option<Displacement> },
    BpSi { disp: Option<Displacement> },
    BpDi { disp: Option<Displacement> },
    SI { disp: Option<Displacement> },
    DI { disp: Option<Displacement> },
    DirectAddress { disp: u16 },
    BP { disp: Displacement },
    BX { disp: Option<Displacement> },
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
                Some(Displacement::D8 { d8: disp } | Displacement::D16 { d16: disp }) => {
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
            EA::BxSi { disp } => pp_formula(vec![R::BX, R::SI], disp.as_ref()),
            EA::BxDi { disp } => pp_formula(vec![R::BX, R::DI], disp.as_ref()),
            EA::BpSi { disp } => pp_formula(vec![R::BP, R::SI], disp.as_ref()),
            EA::BpDi { disp } => pp_formula(vec![R::BP, R::DI], disp.as_ref()),
            EA::SI { disp } => pp_formula(vec![R::SI], disp.as_ref()),
            EA::DI { disp } => pp_formula(vec![R::DI], disp.as_ref()),
            EA::DirectAddress { disp: disp_16 } => disp_16.to_string(),
            EA::BP { disp } => pp_formula(vec![R::BP], Some(disp)),
            EA::BX { disp } => pp_formula(vec![R::BX], disp.as_ref()),
        };

        return format!("[{formula}]");
    }

    let Instruction { op, dst, src } = instruction;

    let dst_str = match dst {
        Dst::Reg { reg: r } => pp_register(r),
        Dst::Ea { ea } => pp_effective_address(ea),
    };

    let dst_is_wide = match dst {
        Dst::Ea { ea: _ } => true,
        Dst::Reg { reg: _ } => false,
    };

    let pp_imm_specifier = |is_word: bool, imm_str: &str| {
        if dst_is_wide {
            let specifier = if is_word { "word" } else { "byte" };
            format!("{specifier} {imm_str}")
        } else {
            imm_str.to_owned()
        }
    };

    let src_str = match src {
        Src::Reg { reg: x } => pp_register(x),
        Src::Imm8 { imm } => pp_imm_specifier(false, &imm.to_string()),
        Src::Imm16 { imm } => pp_imm_specifier(true, &imm.to_string()),
        Src::ImmSigned16 { imm } => pp_imm_specifier(true, &imm.to_string()),
        Src::Ea { ea } => pp_effective_address(ea),
    };

    let op_str = match op {
        Op::Mov => "mov",
        Op::Add => "add",
        Op::Sub => "sub",
        Op::Cmp => "cmp",
    };

    return format!("{op_str} {dst_str}, {src_str}");
}

pub fn decode_instruction_stream<I>(
    instruction_stream: I,
) -> Result<Vec<Instruction>, (Vec<Instruction>, String)>
where
    I: IntoIterator<Item = u8>,
{
    let mut instructions: Vec<Instruction> = vec![];
    let mut stream_bytes = instruction_stream.into_iter();

    while let Some(byte_1) = stream_bytes.next() {
        // Split this up int two phases for clarity and exhaustiveness
        // 1. calculate the next instruction or error, consuming from the
        //    iterator as needed
        // 2. If success, accumulate in `instruction`, else return with error

        // If we want to use ? to compose Results, step 1 can't be just a value
        // binding, because ? expands errors into a `return` to the next
        // function context. So instead, use an IIFE.

        // closure because I'm too lazy to type out parameters
        let next_instruction = (|| -> Result<Instruction, String> {
            let opcode_4 = byte_1 >> 4;
            let opcode_6 = byte_1 >> 2;
            let opcode_7 = byte_1 >> 1;

            if opcode_4 == 0b1011 {
                let w_bit = extract_bit(byte_1, 4);
                let reg_field = byte_1 & 0b00000111;
                let dst = Dst::Reg {
                    reg: decode_register(reg_field, w_bit)?,
                };

                let byte_2 = stream_bytes.next().ok_or("missing byte 2 of reg->imm")?;

                let src = if w_bit {
                    let byte_3 = stream_bytes.next().ok_or("missing byte 3 of reg->imm")?;
                    let imm_16 = build_u16(byte_3, byte_2);
                    Src::Imm16 { imm: imm_16 }
                } else {
                    Src::Imm8 { imm: byte_2 }
                };

                Ok(Instruction {
                    op: Op::Mov,
                    dst,
                    src,
                })
            } else if opcode_6 == 0b100010 {
                decode_reg_mod_rm(byte_1, &mut stream_bytes)
                    .map_err(|e| format!("MOV: {e}"))
                    .map(|(dst, src)| Instruction {
                        op: Op::Mov,
                        dst,
                        src,
                    })
            } else if opcode_6 == 0b101000 {
                // MOV memory/accumulator
                // These have two 7-bit rows in the manual, but the
                // D bit has a semantic, deciding if mem is destination
                let d_bit = extract_bit(byte_1, 2);
                let w_bit = extract_bit(byte_1, 1);
                let addr_lo = stream_bytes.next().ok_or("mem/acc addr_low")?;
                let addr_hi = stream_bytes.next().ok_or("mem/acc addr_hi")?;
                let addr = EffectiveAddress::DirectAddress {
                    disp: build_u16(addr_hi, addr_lo),
                };
                let reg = lookup_accumulator_reg(w_bit);
                let (dst, src) = if d_bit {
                    (Dst::Ea { ea: addr }, Src::Reg { reg })
                } else {
                    (Dst::Reg { reg }, Src::Ea { ea: addr })
                };

                Ok(Instruction {
                    op: Op::Mov,
                    dst,
                    src,
                })
            } else if opcode_7 == 0b1100011 {
                let w_bit = extract_bit(byte_1, 1);
                let byte_2 = stream_bytes.next().ok_or("missing byte 2 of imm->reg")?;
                let mod_field = byte_2 >> 6;
                let reg_field = (byte_2 & 0b00111000) >> 3; // always 000, unused
                if reg_field != 0b000 {
                    return Err("mem->reg: reg field not 0b000".to_string());
                };

                let r_m_field = byte_2 & 0b00000111;

                // WARN: this must appear first! it mutates stream_bytes.
                let dst = Dst::Ea {
                    ea: decode_effective_address(mod_field, r_m_field, &mut stream_bytes)?,
                };

                let data_low = stream_bytes.next().ok_or("Missing byte 3 of imm->reg")?;

                let src = if w_bit {
                    let data_high = stream_bytes.next().ok_or("Missing byte 4 of imm->reg")?;
                    Src::Imm16 {
                        imm: build_u16(data_high, data_low),
                    }
                } else {
                    Src::Imm8 { imm: data_low }
                };

                Ok(Instruction {
                    op: Op::Mov,
                    dst,
                    src,
                })
            }
            // ADD/SUB/CMP
            // 00 signals arithmetic (ish?)
            // next three are opcode extension
            // next tells you if reg/rm or accumulator
            else if let (true, Ok(op)) = (
                (byte_1 >> 6) == 0,
                lookup_opcode_extension((byte_1 >> 3) & 0b00111),
            ) {
                if extract_bit(byte_1, 3) {
                    let w_bit = extract_bit(byte_1, 1);
                    let data_low = stream_bytes.next().ok_or("missing byte 2 of arith/accum")?;
                    let dst = Dst::Reg {
                        reg: lookup_accumulator_reg(w_bit),
                    };
                    let src = if w_bit {
                        let data_high = stream_bytes.next().ok_or("missing byte 3 of arith/accum")?;
                        Src::ImmSigned16 {
                            imm: build_u16(data_high, data_low) as i16,
                        }
                    } else {
                        Src::Imm8 { imm: data_low }
                    };
                    Ok(Instruction { op, dst, src })
                } else {
                    decode_reg_mod_rm(byte_1, &mut stream_bytes)
                        .map_err(|e| format!("{op:#?}: {e}"))
                        .map(|(dst, src)| Instruction { op, dst, src })
                }
            } else if opcode_6 == 0b100000 {
                let s_bit = extract_bit(byte_1, 2);
                let w_bit = extract_bit(byte_1, 1);
                let byte_2 = stream_bytes
                    .next()
                    .ok_or("missing byte 2 of add/cmp/sub imm->reg")?;
                let mod_field = byte_2 >> 6;
                let opcode_extension = (byte_2 & 0b00111000) >> 3;

                let r_m_field = byte_2 & 0b00000111;

                // WARN: this must appear first! it mutates stream_bytes.
                let dst = if mod_field == 0b11 {
                    let reg = decode_register(r_m_field, w_bit)?;
                    Dst::Reg { reg }
                } else {
                    Dst::Ea {
                        ea: decode_effective_address(mod_field, r_m_field, &mut stream_bytes)?,
                    }
                };

                let data_low = stream_bytes.next().ok_or("Missing data of imm->reg")?;

                let src = if w_bit {
                    if s_bit {
                        Src::ImmSigned16 {
                            imm: (data_low as i8) as i16,
                        }
                    } else {
                        let data_high =
                            stream_bytes.next().ok_or("Missing data high of imm->reg")?;
                        let imm = build_u16(data_high, data_low);
                        Src::Imm16 { imm }
                    }
                } else {
                    Src::Imm8 { imm: data_low }
                };

                let op = lookup_opcode_extension(opcode_extension)?;

                return Ok(Instruction { op, dst, src });
            } else {
                Err(format!("Unknown opcode: {byte_1:#b}"))
            }
        })();

        match next_instruction {
            Ok(instruction) => instructions.push(instruction),
            Err(e) => return Err((instructions, e)),
        };
    }

    return Ok(instructions);
}

fn lookup_accumulator_reg(w_bit: bool) -> Register {
    if w_bit {
        Reg::AX
    } else {
        Reg::AL
    }
}

fn lookup_opcode_extension(opcode_extension: u8) -> Result<Op, String> {
    let op = if opcode_extension == 0b000 {
        Ok(Op::Add)
    } else if opcode_extension == 0b101 {
        Ok(Op::Sub)
    } else if opcode_extension == 0b111 {
        Ok(Op::Cmp)
    } else {
        Err("unknown little opcode for imm -> reg/mem".to_string())
    }?;
    Ok(op)
}

fn decode_reg_mod_rm<Bytes>(byte_1: u8, stream_bytes: &mut Bytes) -> Result<(Dst, Src), String>
where
    Bytes: Iterator<Item = u8>,
{
    // various MOVs
    let d_bit = extract_bit(byte_1, 2);
    let w_bit = extract_bit(byte_1, 1);
    let byte_2 = stream_bytes.next().ok_or("missing byte 2 of reg-mod-rm")?;
    let mod_field = byte_2 >> 6;
    let reg_field = (byte_2 & 0b00111000) >> 3;
    let r_m_field = byte_2 & 0b00000111;

    let reg_register = decode_register(reg_field, w_bit)?;

    if 0b11 == mod_field {
        let r_m_register = decode_register(r_m_field, w_bit)?;
        // if d is 1, REG is the dest, meaning R/M is the source
        let (dst, src) = if d_bit {
            (
                Dst::Reg { reg: reg_register },
                Src::Reg { reg: r_m_register },
            )
        } else {
            (
                Dst::Reg { reg: r_m_register },
                Src::Reg { reg: reg_register },
            )
        };
        Ok::<(Dst, Src), String>((dst, src))
    } else {
        let address = decode_effective_address(mod_field, r_m_field, stream_bytes)?;

        let (dst, src) = if d_bit {
            (Dst::Reg { reg: reg_register }, Src::Ea { ea: address })
        } else {
            (Dst::Ea { ea: address }, Src::Reg { reg: reg_register })
        };

        Ok((dst, src))
    }
}

fn decode_effective_address<Bytes>(
    mod_field: u8,
    r_m_field: u8,
    stream_bytes: &mut Bytes,
) -> Result<EffectiveAddress, String>
where
    Bytes: Iterator<Item = u8>,
{
    let formulae = HashMap::from(FORMULAE);

    // while these look structurally similar, notice how ? is inside the `else if`
    // in the first case, while outside in the second. And consider that those
    // mutate the iterator...
    match mod_field {
        0b00 => {
            if let Some(formula) = formulae.get(&r_m_field) {
                Ok(formula(None))
            } else if r_m_field == 0b110 {
                let byte_3 = stream_bytes.next().ok_or("MOD=00 byte 3")?;
                let byte_4 = stream_bytes.next().ok_or("MOD=00 case byte 4")?;
                Ok(EA::DirectAddress {
                    disp: build_u16(byte_4, byte_3),
                })
            } else {
                Err("more than 3 bits for r_m when mod = 0b00".to_string())
            }
        }
        0b01 => {
            let byte_3 = stream_bytes.next().ok_or("MOD=01 byte 3")?;
            let d = Displacement::D8 {
                d8: byte_3 as i8 as i16,
            };
            if let Some(formula) = formulae.get(&r_m_field) {
                Ok(formula(Some(d)))
            } else if r_m_field == 0b110 {
                Ok(EA::BP { disp: d })
            } else {
                Err("more than 3 bits for r_m when MOD = 0b01".to_string())
            }
        }
        0b10 => {
            let byte_3 = stream_bytes.next().ok_or("MOD=11 byte 3")?;
            let byte_4 = stream_bytes.next().ok_or("MOD=11 byte 4")?;
            let d = Displacement::D16 {
                d16: build_u16(byte_4, byte_3) as i16,
            };
            if let Some(formula) = formulae.get(&r_m_field) {
                Ok(formula(Some(d)))
            } else if r_m_field == 0b110 {
                Ok(EA::BP { disp: d })
            } else {
                Err("more than 3 bits for r_m when MOD = 0b11".to_string())
            }
        }
        // register -> register
        _ => return Err(format!("unknown field in mod: {mod_field:#b}")),
    }
}

/// Constructor for effective addresses
type Formula = fn(Option<Displacement>) -> EffectiveAddress;
type EA = EffectiveAddress;
const FORMULAE: [(u8, Formula); 7] = [
    (0b000, |disp| EA::BxSi { disp }),
    (0b001, |disp| EA::BxDi { disp }),
    (0b010, |disp| EA::BpSi { disp }),
    (0b011, |disp| EA::BpDi { disp }),
    (0b100, |disp| EA::SI { disp }),
    (0b101, |disp| EA::DI { disp }),
    (0b111, |disp| EA::BX { disp }),
];

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
    let table = if w_bit { W_1_REGISTERS } else { W_0_REGISTERS };
    return table
        .get(field as usize)
        .map(|x| *x)
        .ok_or("missing register".to_string());
}

/// 1-based indexing for some reason
fn extract_bit(byte: u8, nth_bit: u32) -> bool {
    (byte.wrapping_shr(nth_bit - 1) & 0b00000001) != 0
}
