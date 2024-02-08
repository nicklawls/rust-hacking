import { $ } from 'bun';

const [file] = Bun.argv;

// take in the filename of one of the hosted asm files
// if not found, download it
// assemble it, generating the suffix-less version
// pipe the suffix-less version into `cargo run --bin decode`, pipe the output to 
// a .out.asm file
// generate a .out file by running nasm on the .out.asm
// diff the .out and the suffix-less file 
