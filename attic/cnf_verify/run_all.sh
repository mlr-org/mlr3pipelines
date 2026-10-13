#!/bin/bash
# Long-running verification driver: rotates all fuzz experiments over a seed
# range. Every experiment is independently parameterized via env vars, so this
# can be scaled to bigger machines / longer runs.
#
# Usage:
#   attic/cnf_verify/run_all.sh [SEED_FROM] [SEED_TO]
#
# Scale knobs (defaults in brackets):
#   CNF_CORES        [16]     parallel workers
#   SCALE            [1]      multiplies all per-experiment trial counts
#
# Per-seed workload at SCALE=1:
#   exp02 50k trials, exp05 20k, exp07 3k, exp08 50k, exp10 3k, exp11 20k
#
# Run from the repository root. Logs land in attic/cnf_verify/results/.

set -u
cd "$(dirname "$0")/../.."

SEED_FROM=${1:-10}
SEED_TO=${2:-19}
SCALE=${SCALE:-1}
export CNF_CORES=${CNF_CORES:-16}
DIR=attic/cnf_verify
mkdir -p "$DIR/results"

fail=0
for seed in $(seq "$SEED_FROM" "$SEED_TO"); do
  export CNF_SEED=$seed
  echo "=== seed $seed ==="
  CNF_TRIALS=$((50000 * SCALE)) Rscript $DIR/exp02_fuzz_baseline.R  > "$DIR/results/rot_exp02_s$seed.log" 2>&1 || fail=1
  CNF_TRIALS=$((20000 * SCALE)) Rscript $DIR/exp05_ops_fuzz.R       > "$DIR/results/rot_exp05_s$seed.log" 2>&1 || fail=1
  CNF_TRIALS=$((3000  * SCALE)) Rscript $DIR/exp07_scale_dpll.R     > "$DIR/results/rot_exp07_s$seed.log" 2>&1 || fail=1
  CNF_TRIALS=$((50000 * SCALE)) Rscript $DIR/exp08_invariant_fuzz.R > "$DIR/results/rot_exp08_s$seed.log" 2>&1 || fail=1
  CNF_TRIALS=$((3000  * SCALE)) Rscript $DIR/exp10_negation_scale.R > "$DIR/results/rot_exp10_s$seed.log" 2>&1 || fail=1
  CNF_TRIALS=$((20000 * SCALE)) Rscript $DIR/exp11_misc.R           > "$DIR/results/rot_exp11_s$seed.log" 2>&1 || fail=1
  grep -h "failures:" "$DIR"/results/rot_exp*_s$seed.log | sed "s/^/  seed $seed: /"
  if [ "$fail" -ne 0 ]; then
    echo "seed $seed: AT LEAST ONE EXPERIMENT REPORTED FAILURES -- inspect logs"
  fi
done
echo "rotation done (seeds $SEED_FROM..$SEED_TO), overall failure flag: $fail"
exit $fail
