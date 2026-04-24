.PHONY: build test release coverage clean

SHELL := /bin/bash

COVERAGE_DIR := _coverage
COVERAGE_DATA_DIR := $(COVERAGE_DIR)/data
COVERAGE_HTML_DIR := $(COVERAGE_DIR)/html
ABS_COVERAGE_DATA_DIR := $(abspath $(COVERAGE_DATA_DIR))
ABS_COVERAGE_HTML_DIR := $(abspath $(COVERAGE_HTML_DIR))

define WITH_ENV
	source ./setup-env.sh && eval "$(opam env)" && $(1)
endef

build:
	@$(call WITH_ENV,dune build)

test:
	@$(call WITH_ENV,dune runtest)

release:
	@$(call WITH_ENV,dune build --profile release)

coverage:
	@rm -f bisect*.coverage
	@rm -rf $(COVERAGE_DIR)
	@$(call WITH_ENV,dune clean)
	@mkdir -p $(COVERAGE_DATA_DIR)
	@$(call WITH_ENV,BISECT_FILE=$(ABS_COVERAGE_DATA_DIR)/bisect dune runtest --instrument-with bisect_ppx)
	@$(call WITH_ENV,bisect-ppx-report summary --coverage-path $(ABS_COVERAGE_DATA_DIR) --per-file)
	@$(call WITH_ENV,bisect-ppx-report html --coverage-path $(ABS_COVERAGE_DATA_DIR) -o $(ABS_COVERAGE_HTML_DIR))
	@printf '\nHTML coverage report: %s\n' '$(ABS_COVERAGE_HTML_DIR)/index.html'

clean:
	@rm -f bisect*.coverage
	@rm -rf $(COVERAGE_DIR)
	@$(call WITH_ENV,dune clean)