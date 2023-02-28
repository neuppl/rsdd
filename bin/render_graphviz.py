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
        ps.edge(str(count), left, "l")
        ps.edge(str(count), right, "r")
        return str(count)
        

def render_vtree(options, file):
    parsed = json.loads(file.read())
    g = graphviz.Digraph('G', filename=options.output)
    walk_vtree(parsed["root"], g)
    g.view()


def render_bdd(options, file):
    j = json.loads(file.read())
    nodes = j["nodes"]
    ps = graphviz.Digraph('G', filename=options.output)
    # create all nodes
    for (idx, n) in enumerate(nodes):
        ps.node(str(idx), str(n["topvar"]), shape="circle")
    ps.node("True", "T", shape="square")
    ps.node("False", "F", shape="square")

    # create all edges
    for (idx, n) in enumerate(nodes):
        low = n["low"]
        high = n["high"]

        if "Ptr" in low:
            if low["Ptr"]["compl"]:
                ps.edge(str(idx), str(low["Ptr"]["index"]), style="dotted", color="red")
            else:
                ps.edge(str(idx), str(low["Ptr"]["index"]), style="dotted", color="black")
        else:
            ps.edge(str(idx), low, style="dotted")

        if "Ptr" in high:
            if high["Ptr"]["compl"]:
                ps.edge(str(idx), str(high["Ptr"]["index"]), color="red")
            else:
                ps.edge(str(idx), str(high["Ptr"]["index"]), color="black")
        else:
            ps.edge(str(idx), high)

    # create root pointers
    for (idx, r) in enumerate(j["roots"]):
        name = str(idx) + "root"
        ps.node(name, str(idx), shape="point")
        if r["Ptr"]["compl"]:
            ps.edge(name, str(r["Ptr"]["index"]), color="red")
        else:
            ps.edge(name, str(r["Ptr"]["index"]), color="black")
   

    ps.view()



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
            render_vtree(options, f)
        elif file_extension == ".bdd":
            render_bdd(options, f)
        else:
            print("Failure: No matching extension for file %s. Please use extension '.vtree', '.sdd', '.bdd', or '.dtree'." % f)
