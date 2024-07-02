#!/usr/bin/env bun

import { $ } from "bun";

// take in the filename of one of the hosted asm files
const listingPrefixes = [
  "listing_0037_single_register_mov",
  "listing_0038_many_register_mov",
  "listing_0039_more_movs",
  "listing_0040_challenge_movs",
  "listing_0041_add_sub_cmp_jnz",
];

const buildPromise = $`cargo build --bin decode`;

$.cwd("listings");

const testListing = async (listingPrefix: string) => {
  if (!listingPrefix.startsWith("listing") || listingPrefix.includes(".")) {
    throw "Bad file prefix";
  }

  const asmFile = Bun.file(`${listingPrefix}.asm`);

  if (!asmFile.exists()) {
    throw "File does not exist";
    // TODO: if not found, download it and save it
  }

  // assemble it, generating the prefix-only file
  await $`nasm ${asmFile}`;

  // pass the prefix-only version into rust, pipe the output to
  // a .out.asm file

  const outAsmFileName = `${listingPrefix}.out.asm`;

  await buildPromise;

  await $`../target/debug/decode ${listingPrefix} > ${outAsmFileName}`;

  // generate a .out file by running nasm on the .out.asm
  const reassembly = await $`nasm ${outAsmFileName}`;
  if (reassembly.exitCode) {
    throw "Failed to reassemble";
  }

  // diff the .out and the suffix-less file
  const outFileName = `${listingPrefix}.out`;
  // TODO: better diff with diff <(xxd foo) <(xxd bar)
  const { exitCode } = await $`diff ${listingPrefix} ${outFileName}`;

  if (exitCode === 0) {
    return "Success!";
  }

  return { exitCode };
};

const result = Object.fromEntries(
  await Promise.all(
    listingPrefixes.map(async (prefix) => [
      prefix,
      await testListing(prefix).catch((e) => ({ error: e })),
    ])
  )
);

console.log(result);
