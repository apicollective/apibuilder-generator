#!/bin/sh

PROFILE=localhost apibuilder upload bryzek apibuilder-union-of-unions apibuilder-union-of-unions.json --version `sem-info tag latest`
PROFILE=localhost apibuilder update
