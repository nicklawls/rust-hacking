import { $ } from "bun";

// take in the filename of one of the hosted asm files
const [, , asmFilename] = Bun.argv;

if (!asmFilename) {
  throw "No filename arg";
}
if (asmFilename.includes(".out.") || !asmFilename.endsWith(".asm")) {
  throw "Not a source ASM file ".concat(asmFilename);
}

const asmFile = Bun.file(asmFilename);

if (!asmFile.exists()) {
  throw "File does not exist";
  // TODO: if not found, download it
}

// assemble it, generating the suffix-less version
await $`nasm ${asmFile}`;

// pass the suffix-less version into `cargo run --bin decode`, pipe the output to
// a .out.asm file
const binaryFileName = asmFilename.split(".asm")[0];
if (!binaryFileName) {
  throw "bad binary filename ".concat(String(binaryFileName));
}

const outAsmFileName = `${binaryFileName}.out.asm`;

await $`cargo run --bin decode -- ${binaryFileName} > ${outAsmFileName}`;

// generate a .out file by running nasm on the .out.asm
const reassembly = await $`nasm ${outAsmFileName}`;
if (reassembly.exitCode) {
  throw "failed to reassemble"
}

// diff the .out and the suffix-less file
const outFileName = `${binaryFileName}.out`;
const { exitCode } = await $`diff ${binaryFileName} ${outFileName}`;

if (exitCode === 0) {
  console.log("Success!");
}
