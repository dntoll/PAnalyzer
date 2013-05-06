import networkx as nx
from networkx.readwrite import json_graph
import glob

def findNodesOfType(T, root, nodetype, shallow=False):
	found = []
	for c in T.successors(root):
		if T.node[c]['name'] == nodetype:
			found.append(c)
		if shallow:
			found += findNodesOfType(T, c, nodetype, shallow)
	return found

def getLastChild(T, root):
	x,y,z = sorted(T.out_edges(root, data=True), key=lambda x: x[2]['pos'])[-1:][0]
	return y

def getFirstChild(T, root):
	x,y,z = sorted(T.out_edges(root, data=True), key=lambda x: x[2]['pos'])[0]
	return y

def getNthChild(T, root, n):
	x,y,z = sorted(T.out_edges(root, data=True), key=lambda x: x[2]['pos'])[n]
	return y

def processQI(T, root):
	QI = []
	for x,y,z in sorted(T.out_edges(root, data=True), key=lambda x: x[2]['pos']):
		if T.node[y]['name'] == 'Identifier':
			QI.append(T.node[getFirstChild(T, y)]['name'])
	return QI

def getClassName(T, root):
	cn = []
	nodes = findNodesOfType(T, root, 'ClassDeclaration', True)
	for n in nodes:
		cn.append(T.node[getFirstChild(T, getNthChild(T, n, 1))]['name'])
	return cn

def getInterfaceName(T, root):
	cn = []
	nodes = findNodesOfType(T, root, 'InterfaceDeclaration', True)
	for n in nodes:
		cn.append(T.node[getFirstChild(T, getNthChild(T, n, 1))]['name'])
	return cn

def getFQN(T, root):
	n = findNodesOfType(T, root, 'PackageDeclaration')
	QI = processQI(T, getNthChild(T, n[0], 1))
	cn = getClassName(T, root)
	if cn != []:
		QI.append(getClassName(T, root)[0])
	else:
		QI.append(getInterfaceName(T, root)[0])
	return QI

def getImports(T, root):
	imports = []
	for n in T.successors(root):
		if T.node[n]['name'] == 'ImportDeclaration':
			ds = False
			for c in T.successors(n):
				if T.node[c]['name'] == '*':
					ds = True
				if T.node[c]['name'] == 'QualifiedIdentifier':
					QI = processQI(T, c)					
			if ds:
				QI.append('*')
			imports.append(QI)

	return imports

def getExtends(T, root):
	nodes = findNodesOfType(T, root, 'ClassDeclaration', True)
	for n in nodes:
		for nn in T.successors(n):
			if T.node[nn]['name'] == 'ClassType':
				cn = []
				for x,y,z in sorted(T.out_edges(nn, data=True), key=lambda x: x[2]['pos']):
					if T.node[y]['name'] == 'Identifier':
						cn.append(T.node[getFirstChild(T, y)]['name'])
				return cn

def getImplements(T, root)
	nodes = findNodesOfType(T, root, 'ClassDeclaration', True)
	for n in nodes:
		for nn in T.successors(n):
			if T.node[nn]['name'] == 'ClassTypeList':
				imps = []
				for nnn in T.successors(nn):
					for nnnn in T.successors(nnn):
						cn = []
						if T.node[y]['name'] == 'Identifier':
							cn.append(T.node[getFirstChild(T, y)]['name'])
					imps.append(cn)
	return imps


asts = {}
for fn in glob.glob('jgraph-ana/*.json'):
#	print fn
	s = open(fn).read()
	G = json_graph.loads(s)

	asts['.'.join(getFQN(G, 0))] = G

G = nx.DiGraph()
for t in asts:
	T = asts[t]

	ext = getExtends(T, 0)
	if ext == None:
		continue
	imps = getImports(T, 0)

	for i in imps:
		n = i[:-1]+ext
		print '.'.join(n)
		if '.'.join(n) in asts:
			G.add_node(t, name=t)
			G.add_node('.'.join(n), name='.'.join(n))
			G.add_edge(t, '.'.join(n))

nx.write_graphml(G, 'inheritance.graphml')
