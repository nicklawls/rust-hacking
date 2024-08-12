#!/usr/bin/env bun

// Meant to be read top-to-bottom, but testListing is in a funky spot in order
// to handle errors nicely.

import { $ } from "bun";

// wipe ouptput bin, output ASM, and source bin so they can't
// influence results
await $`fd --full-path '.out|(^[^.]+$)' listings -x rm`;

$.cwd("listings");

// kick off the build
const promiseBuild = $`cargo build --bin decode`;

// take in the filename of one of the hosted asm files
const listingPrefixes = [
  "listing_0037_single_register_mov",
  "listing_0038_many_register_mov",
  "listing_0039_more_movs",
  "listing_0040_challenge_movs",
  "listing_0041_add_sub_cmp_jnz",
];

// Define a function to test each prefix
/**
 * let `sourceBin = assemble(sourceASM)`
 * Result resolves if `sourceBin == assemble(decode(sourceBin))`
 */
const testListing = async (listingPrefix: string): Promise<void> => {
  if (!listingPrefix.startsWith("listing") || listingPrefix.includes(".")) {
    throw "Bad listing prefix";
  }

  const pathSourceBin = `${listingPrefix}`;
  const pathSourceASM = `${listingPrefix}.asm`;
  const pathOutputBin = `${listingPrefix}.out`;
  const pathOutputASM = `${listingPrefix}.out.asm`;

  // source ASM -> source Binary
  const promiseAssembleSource = $`nasm ${pathSourceASM}`;

  // we need the decoder, so wait for the build to finish
  await Promise.all([promiseAssembleSource, promiseBuild]);

  // source Binary -> output ASM
  await $`../target/debug/decode ${pathSourceBin} > ${pathOutputASM}`;

  // output ASM -> output Binary
  const { exitCode: reassemblyExitCode } = await $`nasm ${pathOutputASM}`;
  if (Boolean(reassemblyExitCode)) {
    throw "Failed to reassemble";
  }

  // diff(source Binary, output Binary) should be nada
  // TODO: better diff with diff <(xxd foo) <(xxd bar)
  const { exitCode } = await $`diff ${pathSourceBin} ${pathOutputBin}`;

  if (exitCode === 0) {
    return;
  }

  throw { exitCode };
};

/** Process each prefix in parallel and group the results/errors  */
const resultsByListing = Object.fromEntries(
  await Promise.all(
    listingPrefixes.map(async (prefix) => [
      prefix,
      await testListing(prefix)
        .then((x) => "Success!")
        .catch((e) => ({ error: e })),
    ]),
  ),
);

console.log(resultsByListing);
