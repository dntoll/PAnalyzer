#! /usr/bin/env python

import networkx as nx
from networkx.readwrite import json_graph
import sys

G = json_graph.loads(open(sys.argv[1]).read())
for g in G.nodes_iter():
	if type(G.node[g]['name']) == list:
		G.node[g]['name'] = 'fixme'

nx.write_graphml(G, sys.argv[2])