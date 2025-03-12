.PHONY: repl

repl:
	cargo build --release -p repl

test:
	cargo test -p monkey-core
