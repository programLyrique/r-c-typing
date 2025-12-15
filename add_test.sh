#!/bin/bash

# Script to add a new test for a C file in the cprogs directory
# Usage: ./add_test.sh <c_file_name_without_extension>

set -e  # Exit on error

if [ $# -ne 1 ]; then
    echo "Usage: $0 <c_file_name_without_extension>"
    echo "Example: $0 mytest"
    exit 1
fi

C_FILE="$1.c"
EXPECTED_FILE="$1.expected"
C_PROGS_DIR="test/cprogs"
DUNE_INC_FILE="test/dune.inc"

# Check if the C file exists
if [ ! -f "$C_PROGS_DIR/$C_FILE" ]; then
    echo "Error: C file $C_PROGS_DIR/$C_FILE does not exist"
    exit 1
fi

# Check if the expected file already exists
if [ -f "$C_PROGS_DIR/$EXPECTED_FILE" ]; then
    echo "Error: Expected file $C_PROGS_DIR/$EXPECTED_FILE already exists"
    exit 1
fi

# Build the typing test executable
echo "Building typing_test.exe..."
dune build test/typing_test.exe

# Generate the expected output
echo "Generating expected output for $C_FILE..."
dune exec test/typing_test.exe "$C_PROGS_DIR/$C_FILE" > "$C_PROGS_DIR/$EXPECTED_FILE" 2>/dev/null || true
echo "Created expected file: $C_PROGS_DIR/$EXPECTED_FILE"

# Add rules to dune.inc
echo "Adding rules to $DUNE_INC_FILE..."

# Create the rule for generating output
RULE1="(rule
 (alias   runtest)
 (deps    cprogs/$C_FILE)
 (action  (with-stdout-to $1.output
              (run %{exe:typing_test.exe} %{dep:cprogs/$C_FILE}))))"

# Create the rule for diffing
echo "$RULE1" >> "$DUNE_INC_FILE"
echo "" >> "$DUNE_INC_FILE"

RULE2="(rule
 (alias   runtest)
 (deps    cprogs/$C_FILE cprogs/$EXPECTED_FILE)
 (action  (diff cprogs/$EXPECTED_FILE $1.output)))"

echo "$RULE2" >> "$DUNE_INC_FILE"
echo "" >> "$DUNE_INC_FILE"

echo "Added rules to $DUNE_INC_FILE"
echo "Test setup complete for $C_FILE"
echo "You can now run 'dune runtest' to test your new test case"