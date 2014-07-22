RUSTC ?= rustc
RUSTDOC ?= rustdoc
BUILD ?= ./build
TARGET ?= ./target

RUST_PATH ?= -L $(BUILD) -L target
# RUSTFLAGS ?= --opt-level=3 
RUSTFLAGS ?= 
RUSTTESTFLAGS ?= 

LIB ?= $(BUILD)/.libdocopt.timestamp
LIB_NAME = docopt
LIB_FILES = src/lib.rs src/parse.rs src/synonym.rs
TEST_FILES = $(wildcard src/test/*.rs)

MACRO_LIB ?= $(BUILD)/.libdocopt_macros.timestamp
MACRO_LIB_NAME = docopt_macros
MACRO_LIB_FILES = docopt_macros/src/macro.rs

ARGS ?= 

all: $(LIB) $(MACRO_LIB)

install:
	cargo-lite install

$(MACRO_LIB): $(MACRO_LIB_FILES)
	@mkdir -p $(BUILD) $(TARGET)
	$(RUSTC) $(RUSTFLAGS) $(RUST_PATH) ./docopt_macros/src/macro.rs --out-dir=$(TARGET)
	@touch $(MACRO_LIB)

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
	rm -rf target

push:
	git push origin master
	git push github master

