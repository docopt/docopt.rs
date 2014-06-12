RUSTC ?= rustc
RUSTDOC ?= rustdoc
BUILD_DIR ?= ./build
RUST_PATH ?= $(BUILD_DIR)
# RUSTFLAGS ?= --opt-level=3 
RUSTFLAGS ?= 
RUSTTESTFLAGS ?= 
LIB_NAME = docopt
LIB ?= $(BUILD_DIR)/.libregex.timestamp
LIB_FILES = src/lib.rs src/parse.rs
TEST_FILES = src/test.rs

all: $(LIB)

install:
	cargo-lite install

$(LIB): $(LIB_FILES)
	@mkdir -p $(BUILD_DIR)
	$(RUSTC) $(RUSTFLAGS) ./src/lib.rs --out-dir=$(BUILD_DIR)
	@touch $(LIB)

docs: $(LIB_FILES)
	rm -rf doc
	$(RUSTDOC) -L $(RUST_PATH) --test ./src/lib.rs
	$(RUSTDOC) -L $(RUST_PATH) ./src/lib.rs
	# WTF is rustdoc doing?
	chmod 755 doc
	in-dir doc fix-perms
	rscp ./doc/* gopher:~/www/burntsushi.net/rustdoc/

test: build/tests
	RUST_TEST_TASKS=1 RUST_LOG=$(LIB_NAME) ./build/tests

build/tests: $(LIB) $(TEST_FILES)
	$(RUSTC) $(RUSTTESTFLAGS) -L $(RUST_PATH) --test src/lib.rs -o ./build/tests

scratch: build/scratch
	RUST_TEST_TASKS=1 RUST_LOG=$(LIB_NAME) ./build/scratch

build/scratch: $(LIB) scratch.rs
	$(RUSTC) -L $(BUILD_DIR) $(RUSTTESTFLAGS) scratch.rs -o ./build/scratch

ctags:
	ctags --recurse --options=ctags.rust --languages=Rust

clean:
	rm -f $(BUILD_DIR)/.*.timestamp $(BUILD_DIR)/*

push:
	git push origin master
	git push github master

