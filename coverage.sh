#!/usr/bin/env sh

cargo tarpaulin \
	-o Html \
	--output-dir target/tarpaulin \
	--all-features \
	--rustflags="-C opt-level=0" \
	--exclude-files src/lib.rs
