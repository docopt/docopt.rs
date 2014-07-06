RUSTC ?= rustc
RUSTDOC ?= rustdoc
BUILD ?= ./build
TARGET ?= ./target
RUST_PATH ?= -L $(BUILD) -L target
# RUSTFLAGS ?= --opt-level=3 
RUSTFLAGS ?= 
RUSTTESTFLAGS ?= 
LIB_NAME = docopt
LIB ?= $(BUILD)/.libregex.timestamp
LIB_FILES = src/lib.rs src/macro.rs src/parse.rs src/synonym.rs
TEST_FILES = $(wildcard src/test/*.rs)
ARGS ?= 

all: $(LIB)

install:
	cargo-lite install

$(LIB): $(LIB_FILES)
	@mkdir -p $(BUILD) $(TARGET)
	$(RUSTC) $(RUSTFLAGS) ./src/lib.rs --out-dir=$(TARGET)
	@touch $(LIB)

docs: $(LIB_FILES)
	rm -rf doc
	$(RUSTDOC) $(RUST_PATH) --test ./src/lib.rs
	$(RUSTDOC) $(RUST_PATH) ./src/lib.rs
	# WTF is rustdoc doing?
	chmod 755 doc
	in-dir doc fix-perms
	rscp ./doc/* gopher:~/www/burntsushi.net/rustdoc/

src/test/testcases.rs: src/test/testcases.docopt scripts/mk-testcases
	./scripts/mk-testcases ./src/test/testcases.docopt > ./src/test/testcases.rs

test: build/tests
	RUST_TEST_TASKS=1 RUST_LOG=$(LIB_NAME) ./build/tests $(ARGS)

build/tests: $(TEST_FILES) $(LIB)
	$(RUSTC) $(RUSTTESTFLAGS) $(RUST_PATH) --test src/lib.rs -o ./build/tests

scratch: build/scratch
	RUST_TEST_TASKS=1 RUST_LOG=$(LIB_NAME) ./build/scratch $(ARGS)

build/scratch: $(LIB) scratch.rs
	$(RUSTC) $(RUST_PATH) $(RUSTTESTFLAGS) scratch.rs -o ./build/scratch

ex_%: build/%
	./build/$* $(ARGS)

build/%: examples/%.rs $(LIB)
	$(RUSTC) $(RUST_PATH) --out-dir $(BUILD) $<

ctags:
	ctags --recurse --options=ctags.rust --languages=Rust

clean:
	rm -f $(BUILD)/.*.timestamp $(BUILD)/*

push:
	git push origin master
	git push github master

