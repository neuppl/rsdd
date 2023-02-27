### renders output JSON files as graphviz
### usage example:
### `python bin/render_graphviz.py --file ~/tmp/dump.vtree --output ~/tmp/out.gz`
### this reads in the file `dump.vtree`, generates an output graphviz file `out.gz`, and displays it

import json
import pathlib
import graphviz
from optparse import OptionParser

count = 0

# walks a VTree JSON and renders it
# returns a node
def walk_vtree(j, ps):
    global count
    print(j)
    print("")
    if "Leaf" in j:
        count = count + 1
        ps.node(str(count), str(j["Leaf"]), shape="plaintext")
        return str(count)
    elif "Node" in j:
        count = count + 1
        left = walk_vtree(j["Node"]["left"], ps)
        count = count + 1
        right = walk_vtree(j["Node"]["right"], ps)
        count = count + 1
        ps.node(str(count), "", shape="point")
        ps.edge(str(count), left)
        ps.edge(str(count), right)
        return str(count)
        

def print_vtree(options, file):
    parsed = json.loads(file.read())
    g = graphviz.Digraph('G', filename=options.output)
    walk_vtree(parsed["root"], g)
    g.view()

if __name__ == "__main__":
    parser = OptionParser()
    parser.add_option("-f", "--file", dest="file",
                      help="file to load. Rendering will occur based on file extension. '.vtree' is a vtree, '.bdd' is a BDD, '.sdd' is an SDD, 'dtree' is a DTree", metavar="FILE")
    parser.add_option("-o", "--output", dest="output",
                      help="location to write .dot file", metavar="FILE")
    (options, args) = parser.parse_args()


    with open(options.file, "r") as f:
        file_extension = pathlib.Path(options.file).suffix
        if file_extension == ".vtree":
            print_vtree(options, f)
        else:
            print("Failure: No matching extension for file %s. Please use extension '.vtree', '.sdd', '.bdd', or '.dtree'." % f)
