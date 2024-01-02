use rustbook::decode_instruction_stream;
use std::fs;
use std::io::{self, Error};

fn main() -> io::Result<()> {
    let instruction_stream = fs::read("listing_0037_single_register_mov")?;

    // read file lines, get bytes from str, then group into u16s
    let decoded_instruction_stream = decode_instruction_stream(&instruction_stream)
        .map_err(|e| Error::new(io::ErrorKind::Other, e))?;

    println!("{decoded_instruction_stream:#?}");
    Ok(())
}
