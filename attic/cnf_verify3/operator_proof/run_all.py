"""Run the diagnostic scripts in separate R processes and retain exact output."""

import argparse
import pathlib
import subprocess

parser = argparse.ArgumentParser()
parser.add_argument("--r46", action="store_true")
args = parser.parse_args()
directory = pathlib.Path("attic/cnf_verify3/operator_proof")
prefix = ["podman", "exec", "cnf-review-r46"] if args.r46 else []
suffix = "r46" if args.r46 else "r36"
for script in ["constructor_boundaries", "distribution_audit", "negation_calibration", "negation_closure"]:
    command = prefix + ["Rscript", str(directory / (script + ".R"))]
    result = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, timeout=120)
    path = directory / (script + "_" + suffix + ".log")
    path.write_text(result.stdout)
    print(f"{script}: exit {result.returncode}; {path}", flush=True)
    if result.returncode:
        raise RuntimeError(result.stdout)
