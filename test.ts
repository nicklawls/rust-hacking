import { $ } from "bun";

// take in the filename of one of the hosted asm files
const files = [
  "listing_0037_single_register_mov.asm",
  "listing_0038_many_register_mov.asm",
  "listing_0039_more_movs.asm",
  "listing_0040_challenge_movs.asm",
  "listing_0041_add_sub_cmp_jnz.asm",
];

const testFile = async (asmFilename: string) => {
  if (asmFilename.includes(".out.") || !asmFilename.endsWith(".asm")) {
    throw "Not a source ASM file ".concat(asmFilename);
  }

  $.cwd("listings")

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
    throw "failed to reassemble";
  }

  // diff the .out and the suffix-less file
  const outFileName = `${binaryFileName}.out`;
  const { exitCode } = await $`diff ${binaryFileName} ${outFileName}`;

  if (exitCode === 0) {
    return "Success!";
  }

  return { exitCode };
};

const result = Object.fromEntries(
  await Promise.all(
    files.map(async (file) => [
      file,
      await testFile(file).catch((e) => ({ error: e })),
    ])
  )
);

console.log(result);