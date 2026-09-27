"""Process-isolated recurrence and scaling checks with analytic exact outputs."""
import json
import os
import pathlib
import subprocess
import time

HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parents[2]
SCRIPT = "attic/cnf_verify3/unit_queue/chain_probe.R"


def run(version, kind, family, n, order="reverse", measure=False):
    settings = dict(CNF_CHAIN_N=str(n), CNF_QUEUE_VARIANT=kind, CNF_CHAIN_FAMILY=family,
                    CNF_CHAIN_ORDER=order, CNF_CHAIN_MEASURE=str(int(measure)))
    env = dict(os.environ, **settings)
    if version == "host":
        command = ["Rscript", SCRIPT]
    else:
        command = ["podman", "exec"]
        for key, value in settings.items():
            command += ["-e", key + "=" + value]
        command += ["cnf-review-r46", "Rscript", SCRIPT]
    started = time.time()
    try:
        process = subprocess.run(command, cwd=str(ROOT), env=env, text=True,
                                 stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=55)
        result = dict(exit_code=process.returncode, stdout=process.stdout, stderr=process.stderr,
                      status="pass" if "PASS:" in process.stdout else "error")
    except subprocess.TimeoutExpired as error:
        result = dict(status="timeout", stdout=str(error.stdout), stderr=str(error.stderr))
    result.update(version=version, kind=kind, family=family, n=n, order=order,
                  measure=measure, wall_seconds=time.time() - started)
    return result


def main():
    results = []
    cases = []
    for version in ("host", "r46"):
        for kind in ("production", "queued"):
            for family in ("unit", "guarded"):
                for n in (8, 16, 32, 64):
                    cases.append((version, kind, family, n, "reverse", True))
        for n in (4096, 16384):
            cases.append((version, "queued", "unit", n, "reverse", False))
        cases.append((version, "production", "unit", 4096, "forward", False))
        fail_size = 256 if version == "host" else 1024
        cases.append((version, "production", "unit", fail_size, "reverse", False))
        for kind in ("production", "queued"):
            cases.append((version, kind, "guarded", fail_size, "reverse", False))
    for case in cases:
        result = run(*case)
        results.append(result)
        print(json.dumps({k: result[k] for k in ("version", "kind", "family", "n", "measure", "status")}), flush=True)
        (HERE / "resource_results.json").write_text(json.dumps(dict(reports=results), indent=2) + "\n")


if __name__ == "__main__":
    main()
