import argparse
from subprocess import STDOUT, check_output
import subprocess
from concurrent.futures import ThreadPoolExecutor # pip install futures
from concurrent import futures
import json
import git



parser = argparse.ArgumentParser(
                    prog = 'rsdd-benchmark',
                    description = '',
                    epilog = '')

parser.add_argument('filename')
parser.add_argument('-n', '--numthreads')
parser.add_argument('-t', '--timeout')

cmdargs = parser.parse_args()

def run_line(*args, **kwargs):
    """ Stub function to use with futures - your processing logic """
    try:
        r = subprocess.run(kwargs["command"], shell=True, timeout=int(cmdargs.timeout), stdout=subprocess.PIPE)
        return json.loads(r.stdout)
    except:
        return None

with ThreadPoolExecutor(max_workers=int(cmdargs.numthreads)) as executor:
    jobs = []
    results_done = []
    n = 0
    for cmd in open(cmdargs.filename, "r"):
        n += 1
        kw = {"command": cmd}
        jobs.append(executor.submit(run_line, **kw))
    
    for job in futures.as_completed(jobs):
        # Read result from future
        result_done = job.result()
        if result_done is not None:
            # Append to the list of results
            results_done.append(result_done)
    
    repo = git.Repo(search_parent_directories=True)
    sha = repo.head.object.hexsha
    result = {"file": cmdargs.filename, "total_benchmarks": n,  \
        "num_completed": len(results_done), "timeout": cmdargs.timeout, "githash": sha, "result": results_done}
    print(json.dumps(result))
