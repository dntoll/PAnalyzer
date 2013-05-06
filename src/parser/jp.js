#! /usr/bin/env node

var fs = require('fs');
var PEG = require("pegjs");
var nodes = require('./nodes');
var jp = require('./java')

inf = fs.readFileSync(process.argv[2], "utf8");
out = jp.parse(inf, 'CompilationUnit');

var c = new nodes.ASTNode('dummy', [ new nodes.Terminal('aa'), new nodes.ASTNode('other', [ new nodes.Terminal('bu')]) ]);

res = nodes.exportAST(out);
fs.writeFileSync(process.argv[2] + '.json', res);
//nodes.walkAST(out);
