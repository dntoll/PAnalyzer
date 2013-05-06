require('jsclass');
JS.require('JS.Class')

exports.ASTNode = new JS.Class({
	initialize: function(type, children) {
		this.type = type;
		this.children = children;
	}
})

exports.Terminal = new JS.Class({
	initialize: function(value) {
		this.value = value;
	}
})

function pwalkAST(root) {
	if(root.klass == exports.ASTNode) {
		console.log('ASTNode:'+root.type+' ('+root.children.length+')');
		for(var i=0;i<root.children.length;i++) {
			pwalkAST(root.children[i]);
		}
	}
	if(root.klass == exports.Terminal) {
		console.log('Terminal:'+root.value);
	}
}

var nodes = []
var edges = []

function exportChild(root, pid, pos) {
	var myid = nodes.length;
	edges.push({"source": pid, "target": myid, "pos":pos, "value":1})

	if(root.klass == exports.Terminal) 
		nodes.push({"name":root.value})
	if(root.klass == exports.ASTNode) {
		nodes.push({"name":root.type})
		for(var i = 0; i<root.children.length; i++)
			exportChild(root.children[i], myid, i);
	}
}

exports.exportAST = function(root) {
	nodes = [ {"name":root.type} ]
	edges = []

	for(var i = 0; i<root.children.length; i++)
		exportChild(root.children[i], 0, i)

	return JSON.stringify({ "directed":true, "nodes": nodes, "links": edges })
}


exports.walkAST = function(root) {
	pwalkAST(root);
}