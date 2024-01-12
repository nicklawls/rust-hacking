use std::{fs, io};
use rustbook::decoder;

const FILES: [&str; 4] = [
    "listing_0037_single_register_mov",
    "listing_0038_many_register_mov",
    "listing_0039_more_movs",
    "listing_0040_challenge_movs",
];

fn main() -> io::Result<()> {
    for filename in FILES {
        let fl = fs::File::open(filename)?;
        let file_len = fl.metadata()?.len();
        eprintln!("{}, {:#?} B", filename, file_len);
        let instruction_stream = io::Read::bytes(io::BufReader::new(fl))
            // have to map away results. A bit hacky, but avoids collecting
            // too eagerly.
            .map(|x| x.expect("oops"));

        for instruction in decoder::decode_instruction_stream(instruction_stream)
            .map(|instructions| instructions.iter().map(decoder::pp_asm).collect::<Vec<String>>())
            .unwrap_or_else(|(instructions, error)| {
                instructions
                    .iter()
                    .map(decoder::pp_asm)
                    .chain(std::iter::once(error))
                    .collect::<Vec<String>>()
            })
        {
            println!("{instruction}");
        }
    }
    Ok(())
}
