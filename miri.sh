#!/usr/bin/env sh
# TODO: Use explicit exposed provenance api
export MIRIFLAGS="-Zmiri-disable-isolation -Zmiri-ignore-leaks -Zmiri-permissive-provenance"
cargo miri test --target i686-unknown-linux-gnu
# TODO: Test on 64 bit target too (stacker causing problems)
