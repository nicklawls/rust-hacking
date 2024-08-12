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
pub enum DstSrcOp {
    Mov,
    Add,
    Sub,
    Cmp,
}
#[derive(Debug, Clone, Copy)]
pub enum CJmpOp {
    Je,
    Jl,
    Jle,
    Jb,
    Jbe,
    Jp,
    Jo,
    Js,
    Jne,
    Jnl,
    Jnle,
    Jnb,
    Jnbe,
    Jnp,
    Jno,
    Jns,
    Loop,
    Loopz,
    Loopnz,
    Jcxz,
}

type IpInc8 = i8;

#[derive(Debug)]
pub enum Instruction {
    DstSrc { op: DstSrcOp, dst: Dst, src: Src },
    CJmp { op: CJmpOp, inc: IpInc8 },
}

/// "If the displacement is only a single byte, the 8086 or 8088 automatically
/// sign-extends this quantity to 16-bits before using the information in
/// further address calculations"
/// implying that displacements are kind of always signed after decoding
type Displacement = i16;

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
            let reg_str = registers
                .iter()
                .map(pp_register)
                .collect::<Vec<_>>()
                .join(" + ");

            match displacement {
                Some(&disp) if disp != 0 => {
                    let sign = if disp.is_positive() { "+" } else { "-" };
                    return format!("{reg_str} {sign} {}", disp.abs());
                }
                _ => return reg_str,
            }
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

        format!("[{formula}]")
    }

    match instruction {
        Instruction::DstSrc { op, dst, src } => {
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

            let op_str = format!("{op:?}").to_ascii_lowercase();

            format!("{op_str} {dst_str}, {src_str}")
        }
        Instruction::CJmp { op, inc } => {
            let op_str = format!("{op:?}").to_ascii_lowercase();
            // Keep an eye on this 2, it has to do with the fact that jumps are
            // relative to the next instruction, i.e. 2 bytes away.
            let inc_str = format!("$+2{inc:+}");
            format!("{op_str} {inc_str}")
        }
    }
}

pub fn decode_instruction_stream<I>(
    instruction_stream: I,
) -> impl Iterator<Item = Result<Instruction, String>>
where
    I: IntoIterator<Item = u8>,
{
    let mut stream_bytes = instruction_stream.into_iter();

    std::iter::from_fn(move || {
        let next = stream_bytes.next();
        next.map(|byte_1| -> Result<Instruction, String> {
            let opcode_4 = byte_1 >> 4;
            let opcode_6 = byte_1 >> 2;
            let opcode_7 = byte_1 >> 1;

            if opcode_4 == 0b1011 {
                let w_bit = extract_bit(byte_1, 3);
                let reg_field = byte_1 & 0b00000111;
                let dst = Dst::Reg {
                    reg: decode_register(reg_field, w_bit)?,
                };

                let src = decode_immediate(w_bit, &mut stream_bytes)?;

                Ok(Instruction::DstSrc {
                    op: DstSrcOp::Mov,
                    dst,
                    src,
                })
            } else if opcode_6 == 0b100010 {
                decode_reg_mod_rm(byte_1, &mut stream_bytes)
                    .map_err(|e| format!("MOV: {e}"))
                    .map(|(dst, src)| Instruction::DstSrc {
                        op: DstSrcOp::Mov,
                        dst,
                        src,
                    })
            } else if opcode_6 == 0b101000 {
                // MOV memory/accumulator
                // These have two 7-bit rows in the manual, but the
                // D bit has a semantic, deciding if mem is destination
                let d_bit = extract_bit(byte_1, 1);
                let w_bit = extract_bit(byte_1, 0);
                let addr_lo = stream_bytes.next().ok_or("mem/acc addr_low")?;
                let addr_hi = stream_bytes.next().ok_or("mem/acc addr_hi")?;
                let addr = EffectiveAddress::DirectAddress {
                    disp: build_u16(addr_hi, addr_lo),
                };
                let reg = decode_accumulator_reg(w_bit);
                let (dst, src) = if d_bit {
                    (Dst::Ea { ea: addr }, Src::Reg { reg })
                } else {
                    (Dst::Reg { reg }, Src::Ea { ea: addr })
                };

                Ok(Instruction::DstSrc {
                    op: DstSrcOp::Mov,
                    dst,
                    src,
                })
            } else if opcode_7 == 0b1100011 {
                let w_bit = extract_bit(byte_1, 0);
                let byte_2 = stream_bytes.next().ok_or("missing byte 2 of imm->reg")?;
                let mod_field = extract_std_mod(byte_2);
                let reg_field = extract_std_reg(byte_2); // always 000, unused
                if reg_field != 0b000 {
                    return Err("mem->reg: reg field not 0b000".to_string());
                };

                let r_m_field = extract_std_r_m(byte_2);

                // WARN: this must appear first! it mutates stream_bytes.
                let dst = Dst::Ea {
                    ea: decode_effective_address(mod_field, r_m_field, &mut stream_bytes)?,
                };

                let src = decode_immediate(w_bit, &mut stream_bytes)?;

                Ok(Instruction::DstSrc {
                    op: DstSrcOp::Mov,
                    dst,
                    src,
                })
            }
            // ADD/SUB/CMP
            // 00 signals arithmetic (ish?)
            // next three are opcode extension
            // next tells you if reg/rm or accumulator
            else if let (true, Ok(op)) = (
                (byte_1 >> 6) == 0b00,
                decode_opcode_extension(extract_bits_543(byte_1)),
            ) {
                if extract_bit(byte_1, 2) {
                    let w_bit = extract_bit(byte_1, 0);
                    let dst = Dst::Reg {
                        reg: decode_accumulator_reg(w_bit),
                    };
                    let src = decode_immediate(w_bit, &mut stream_bytes)?;
                    Ok(Instruction::DstSrc { op, dst, src })
                } else {
                    decode_reg_mod_rm(byte_1, &mut stream_bytes)
                        .map_err(|e| format!("{op:#?}: {e}"))
                        .map(|(dst, src)| Instruction::DstSrc { op, dst, src })
                }
            } else if opcode_6 == 0b100000 {
                let s_bit = extract_bit(byte_1, 1);
                let w_bit = extract_bit(byte_1, 0);
                let byte_2 = stream_bytes
                    .next()
                    .ok_or("missing byte 2 of add/cmp/sub imm->reg")?;
                let mod_field = extract_std_mod(byte_2);
                let opcode_extension = extract_bits_543(byte_2);
                let r_m_field = extract_std_r_m(byte_2);

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

                let op = decode_opcode_extension(opcode_extension)?;

                Ok(Instruction::DstSrc { op, dst, src })
            } else if let Some(op) = decode_cjmp_op(byte_1) {
                let byte_2 = stream_bytes.next().ok_or("Missing byte 2 of cjmp")?;
                Ok(Instruction::CJmp {
                    op,
                    inc: byte_2 as i8,
                })
            } else {
                Err(format!("Unknown opcode: {byte_1:#b}"))
            }
        })
    })
    .scan(false, |seen_first_error, next| {
        // stop emitting after first error
        if *seen_first_error {
            None
        } else {
            *seen_first_error = next.is_err();
            Some(next)
        }
    })
}

fn decode_immediate<Bytes>(w_bit: bool, stream_bytes: &mut Bytes) -> Result<Src, String>
where
    Bytes: Iterator<Item = u8>,
{
    let data_low = stream_bytes.next().ok_or("Missing byte 3 of imm->reg")?;
    Ok(if w_bit {
        let data_high = stream_bytes.next().ok_or("missing byte 3 of reg->imm")?;
        let imm_16 = build_u16(data_high, data_low);
        Src::Imm16 { imm: imm_16 }
    } else {
        Src::Imm8 { imm: data_low }
    })
}

fn decode_accumulator_reg(w_bit: bool) -> Register {
    if w_bit {
        Reg::AX
    } else {
        Reg::AL
    }
}

fn decode_opcode_extension(opcode_extension: u8) -> Result<DstSrcOp, String> {
    if opcode_extension == 0b000 {
        Ok(DstSrcOp::Add)
    } else if opcode_extension == 0b101 {
        Ok(DstSrcOp::Sub)
    } else if opcode_extension == 0b111 {
        Ok(DstSrcOp::Cmp)
    } else {
        Err(format!("Unknown opcode extension: {opcode_extension:#b}"))
    }
}

fn decode_reg_mod_rm<Bytes>(byte_1: u8, stream_bytes: &mut Bytes) -> Result<(Dst, Src), String>
where
    Bytes: Iterator<Item = u8>,
{
    // various MOVs
    let d_bit = extract_bit(byte_1, 1);
    let w_bit = extract_bit(byte_1, 0);
    let byte_2 = stream_bytes.next().ok_or("missing byte 2 of reg-mod-rm")?;
    let mod_field = extract_std_mod(byte_2);
    let reg_field = extract_std_reg(byte_2);
    let r_m_field = extract_std_r_m(byte_2);

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
    type EA = EffectiveAddress;

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
            let d = byte_3 as i8 as i16;
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
            let d = build_u16(byte_4, byte_3) as i16;
            if let Some(formula) = formulae.get(&r_m_field) {
                Ok(formula(Some(d)))
            } else if r_m_field == 0b110 {
                Ok(EA::BP { disp: d })
            } else {
                Err("more than 3 bits for r_m when MOD = 0b11".to_string())
            }
        }
        // register -> register
        _ => Err(format!("unknown field in mod: {mod_field:#b}")),
    }
}

const FORMULAE: [(u8, fn(Option<Displacement>) -> EffectiveAddress); 7] = {
    type EA = EffectiveAddress;
    [
        (0b000, |disp| EA::BxSi { disp }),
        (0b001, |disp| EA::BxDi { disp }),
        (0b010, |disp| EA::BpSi { disp }),
        (0b011, |disp| EA::BpDi { disp }),
        (0b100, |disp| EA::SI { disp }),
        (0b101, |disp| EA::DI { disp }),
        (0b111, |disp| EA::BX { disp }),
    ]
};

fn decode_cjmp_op(opcode: u8) -> Option<CJmpOp> {
    let opcode_map = HashMap::from(CJMP_OPCODES);
    opcode_map.get(&opcode).map(|x| *x)
}

const CJMP_OPCODES: [(u8, CJmpOp); 20] = {
    use CJmpOp::*;
    [
        (0b01110100, Je),
        (0b01111100, Jl),
        (0b01111110, Jle),
        (0b01110010, Jb),
        (0b01110110, Jbe),
        (0b01111010, Jp),
        (0b01110000, Jo),
        (0b01111000, Js),
        (0b01110101, Jne),
        (0b01111101, Jnl),
        (0b01111111, Jnle),
        (0b01110011, Jnb),
        (0b01110111, Jnbe),
        (0b01111011, Jnp),
        (0b01110001, Jno),
        (0b01111001, Jns),
        (0b11100010, Loop),
        (0b11100001, Loopz),
        (0b11100000, Loopnz),
        (0b11100011, Jcxz),
    ]
};

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
    table
        .get(field as usize)
        .map(|x| *x)
        .ok_or("missing register".to_string())
}

/// In this ISA, later-coming bytes are the hight bytes
fn build_u16(later_byte: u8, earlier_byte: u8) -> u16 {
    ((later_byte as u16) << 8) | (earlier_byte as u16)
}

/// [7 6 5 4 3 2 1 0]
fn extract_bit(byte: u8, nth_bit: u8) -> bool {
    ((byte >> nth_bit) & 0b00000001) != 0
}

/// common bit pattern: (byte & 0b00111000) >> 3
fn extract_bits_543(byte: u8) -> u8 {
    (byte & 0b00111000) >> 3
}

fn extract_std_mod(byte: u8) -> u8 {
    byte >> 6
}

fn extract_std_r_m(byte: u8) -> u8 {
    byte & 0b00000111
}

fn extract_std_reg(byte: u8) -> u8 {
    extract_bits_543(byte)
}
