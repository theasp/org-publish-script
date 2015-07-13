#!/bin/bash
set -ex

TEST_DIR=./tests
for test in $(ls -1 $TEST_DIR | grep -E -e '^[a-zA-Z0-9_-]+$' | sort); do
    $TEST_DIR/$test
done
