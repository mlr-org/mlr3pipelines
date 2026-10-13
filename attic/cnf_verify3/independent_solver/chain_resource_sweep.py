"""Bounded process-isolated scaling measurements with exact expected outputs."""
import argparse
import json
import os
import pathlib
import subprocess
import time

HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parents[2]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--versions", nargs="+", default=["host", "r46"])
    parser.add_argument("--sizes", nargs="+", type=int, default=[16, 32, 64, 128, 256, 512])
    parser.add_argument("--out", default="chain_resource_results.json")
    args = parser.parse_args()
    start, reports = time.time(), []
    for version in args.versions:
        for n in args.sizes:
            for order in ("forward", "reverse"):
                env = dict(os.environ, CNF_CHAIN_N=str(n), CNF_CHAIN_ORDER=order)
                script = "attic/cnf_verify3/independent_solver/chain_resource_probe.R"
                if version == "host":
                    command = ["Rscript", script]
                else:
                    command = ["podman", "run", "--rm", "--network=none", "-v", str(ROOT) + ":/repo:ro", "-w", "/repo",
                               "-e", "CNF_PROOF_RLIB=/repo/attic/cnf_verify3/representation/r46-library",
                               "-e", "CNF_CHAIN_N=" + str(n), "-e", "CNF_CHAIN_ORDER=" + order,
                               "docker.io/library/r-base", "Rscript", script]
                result = subprocess.run(command, cwd=str(ROOT), env=env, text=True,
                                        stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=45)
                record = dict(version=version, n=n, order=order, exit_code=result.returncode,
                              stdout=result.stdout, stderr=result.stderr)
                reports.append(record)
                summary = dict(version=version, n=n, order=order, exit_code=result.returncode,
                               status="pass" if "PASS:" in result.stdout else "error")
                print(json.dumps(summary), flush=True)
    (HERE / args.out).write_text(json.dumps(dict(reports=reports, elapsed=time.time() - start), indent=2) + "\n")


if __name__ == "__main__":
    main()
