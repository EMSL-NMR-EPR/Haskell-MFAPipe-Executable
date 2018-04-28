#!/usr/bin/env bash
# file: run_examples.sh

set -x
set -e

stack build ;

for i in $(find ./examples/fba-mip -name "*.json" -print); do
  stack exec MFAPipe -- fba-mip \
    --input="$i" \
    --output="${i%.json}.zip" \
    1> "${i%.json}.out.txt" \
    2> "${i%.json}.err.txt" ;
done

for i in $(find ./examples/fba-simplex -name "*.json" -print); do
  stack exec MFAPipe -- fba-simplex \
    --input="$i" \
    --output="${i%.json}.zip" \
    1> "${i%.json}.out.txt" \
    2> "${i%.json}.err.txt" ;
done

for i in $(find ./examples/mfa-levmar -name "*.json" -print); do
  stack exec MFAPipe -- mfa-levmar \
    --input="$i" \
    --output="${i%.json}.zip" \
    1> "${i%.json}.out.txt" \
    2> "${i%.json}.err.txt" ;
done
