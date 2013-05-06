// TODO
// - fix StringLiteral, broken if Escape happens, or make Escape a string?

{
	var nodes = require('/Users/mogge/Desktop/nodes');

	function flatten(input, output) {
	  input.forEach(function (value) {
    	if (Array.isArray(value)) 
      		flatten(value, output);
    	else
      		output.push(value);    
  		});

  		return output;
	}

	function pr(l) {
		tmp = flatten(l, []);
		return tmp.filter(function(e, i, a) {
			return (e.klass == nodes.ASTNode || e.klass == nodes.Terminal);
		});		
	}

	function prd(l) {
		console.log(l);
		tmp = flatten(l, []);
		console.log(tmp);
		return tmp.filter(function(e, i, a) {
			return (e.klass == nodes.ASTNode || e.klass == nodes.Terminal);
		});		
	}
}

CompilationUnit = s:Spacing p:PackageDeclaration? i:ImportDeclaration* t:TypeDeclaration* { return new nodes.ASTNode('CompilationUnit', pr([ s, p, i, t])); }
PackageDeclaration = a:Annotation* p:PACKAGE qi:QualifiedIdentifier s:SEMI { return new nodes.ASTNode('PackageDeclaration', pr([a, p, qi, s])); }
ImportDeclaration = i:IMPORT s:STATIC? qi:QualifiedIdentifier ds:( DOT STAR )? ss:SEMI { return new nodes.ASTNode('ImportDeclaration', pr([i, s, qi, ds, ss ])); }
TypeDeclaration = m:Modifier* n:( ClassDeclaration / EnumDeclaration / InterfaceDeclaration / AnnotationTypeDeclaration ) { return new nodes.ASTNode('TypeDeclaration', pr([m, n])); } / s:SEMI { return s; }
ClassDeclaration = c:CLASS i:Identifier t:TypeParameters? e:(EXTENDS ClassType)? im:(IMPLEMENTS ClassTypeList)? b:ClassBody { return new nodes.ASTNode('ClassDeclaration', pr([ c, i, t, e, im, b ])); }
ClassBody = l:LWING c:ClassBodyDeclaration* r:RWING { return new nodes.ASTNode('ClassBody', pr([l,c,r]));}
ClassBodyDeclaration = s:SEMI {return nodes.ASTNode('ClassBodyDeclaration', pr([ s ])); }
		/ s:STATIC? b:Block { return new nodes.ASTNode('ClassBodyDeclaration', pr([s, b])); }
		/ m:Modifier* md:MemberDecl { return new nodes.ASTNode('ClassBodyDeclaration', pr([m, md])); }		

MemberDecl = t:TypeParameters g:GenericMethodOrConstructorRest 		{ return new nodes.ASTNode('MemberDecl', pr([t, g])); }
		/ t:Type i:Identifier m:MethodDeclaratorRest 				{ return new nodes.ASTNode('MemberDecl', pr([t, i, m]));}
		/ t:Type v:VariableDeclarators SEMI 						{ return new nodes.ASTNode('MemberDecl', pr([t, v]));}
		/ v:VOID i:Identifier m:VoidMethodDeclaratorRest 			{ return new nodes.ASTNode('MemberDecl', pr([v, i, m]));}
		/ i:Identifier c:ConstructorDeclaratorRest 					{ return new nodes.ASTNode('MemberDecl', pr([i, c]));}
		/ i:InterfaceDeclaration 									{ return new nodes.ASTNode('MemberDecl', pr([i]));}
		/ c:ClassDeclaration 										{ return new nodes.ASTNode('MemberDecl', pr([c]));}
		/ e:EnumDeclaration 										{ return new nodes.ASTNode('MemberDecl', pr([e]));}
		/ a:AnnotationTypeDeclaration 								{ return new nodes.ASTNode('MemberDecl', pr([a]));}

GenericMethodOrConstructorRest = t:( Type / VOID ) i:Identifier m:MethodDeclaratorRest 	{ return new nodes.ASTNode('GenericMethodOrConstructorRest', pr([t, i, m]));}
		/ i:Identifier c:ConstructorDeclaratorRest 										{ return new nodes.ASTNode('GenericMethodOrConstructorRest', pr([i, c]));}

MethodDeclaratorRest = f:FormalParameters d:Dim* t:( THROWS ClassTypeList )? b:( MethodBody / SEMI ) { return new nodes.ASTNode('MethodDeclaratorRest', pr([f, d, t, b])); }
VoidMethodDeclaratorRest = f:FormalParameters t:( THROWS ClassTypeList )? b:( MethodBody / SEMI ) { return new nodes.ASTNode('VoidMethodDeclaratorRest', pr([f, t, b])); }
ConstructorDeclaratorRest = f:FormalParameters t:( THROWS ClassTypeList )? b:MethodBody { return new nodes.ASTNode('ConstructorDeclaratorRest', pr([f, t, b])); }
MethodBody = b:Block { return new nodes.ASTNode('MethodBody', [ b ]);}
InterfaceDeclaration = i:INTERFACE id:Identifier t:TypeParameters? e:( EXTENDS ClassTypeList )? b:InterfaceBody { return new nodes.ASTNode('InterfaceDeclaration', pr([i, id, t, e, b])); }
InterfaceBody = l:LWING b:InterfaceBodyDeclaration* r:RWING 		{ return new nodes.ASTNode('InterfaceBody', pr([l, b, r]));}
InterfaceBodyDeclaration = m:Modifier* i:InterfaceMemberDecl 		{ return new nodes.ASTNode('InterfaceBodyDeclaration', pr([m, i]));} 
		/ s:SEMI 													{ return new nodes.ASTNode('InterfaceBodyDeclaration', pr([ s ])); } 

InterfaceMemberDecl = i:InterfaceMethodOrFieldDecl 					{ return new nodes.ASTNode('InterfaceMemberDecl', [ i ]); } 
		/ i:InterfaceGenericMethodDecl 								{ return new nodes.ASTNode('InterfaceMemberDecl', [ i ]); } 
		/ v:VOID i:Identifier r:VoidInterfaceMethodDeclaratorsRest 	{ return new nodes.ASTNode('InterfaceMemberDecl', [ v, i, r ]); } 
		/ i:InterfaceDeclaration 									{ return new nodes.ASTNode('InterfaceMemberDecl', [ i ]); } 
		/ a:AnnotationTypeDeclaration 								{ return new nodes.ASTNode('InterfaceMemberDecl', [ a ]); } 
		/ c:ClassDeclaration 										{ return new nodes.ASTNode('InterfaceMemberDecl', [ c ]); } 
		/ e:EnumDeclaration 										{ return new nodes.ASTNode('InterfaceMemberDecl', [ e ]); } 

InterfaceMethodOrFieldDecl = t:Type i:Identifier r:InterfaceMethodOrFieldRest { return new nodes.ASTNode('InterfaceMethodOrFieldDecl', [ t, i, r ]); } 
InterfaceMethodOrFieldRest = r:ConstantDeclaratorsRest s:SEMI {return new nodes.ASTNode('InterfaceMethodOrFieldRest', [r, s]); }
		/ i:InterfaceMethodDeclaratorRest {return new nodes.ASTNode('InterfaceMethodOrFieldRest', [i]); }

InterfaceMethodDeclaratorRest = f:FormalParameters d:Dim* t:( THROWS ClassTypeList )? s:SEMI {return new nodes.ASTNode('InterfaceMethodDeclaratorRest', pr([f, d, t, s])); }
InterfaceGenericMethodDecl = p:TypeParameters t:( Type / VOID ) i:Identifier r:InterfaceMethodDeclaratorRest {return new nodes.ASTNode('InterfaceGenericMethodDecl', [p, t, i, r]); }
VoidInterfaceMethodDeclaratorsRest = f:FormalParameters t:( THROWS ClassTypeList )? s:SEMI {return new nodes.ASTNode('VoidInterfaceMethodDeclaratorsRest', pr([f, t, s])); }
ConstantDeclaratorsRest = r:ConstantDeclaratorRest c:( COMMA ConstantDeclarator )* { return new nodes.ASTNode('ConstantDeclaratorsRest', pr([r, c])); }
ConstantDeclarator = i:Identifier r:ConstantDeclaratorRest { return new nodes.ASTNode('ConstantDeclarator', [i, r]);}
ConstantDeclaratorRest = d:Dim* EQU v:VariableInitializer { return new nodes.ASTNode('ConstantDeclaratorRest', pr([d, v]));}
EnumDeclaration = e:ENUM i:Identifier im:( IMPLEMENTS ClassTypeList )? b:EnumBody { return new nodes.ASTNode('EnumDeclaration', pr([e, i, im, b])); }
EnumBody = l:LWING e:EnumConstants? c:COMMA? b:EnumBodyDeclarations? r:RWING { return new nodes.ASTNode('EnumBody', pr([l, e, c, b, r])); }
EnumConstants = e:EnumConstant r:( COMMA EnumConstant )* { return new nodes.ASTNode('EnumConstants', pr([e, r])); }
EnumConstant = a:Annotation* i:Identifier arg:Arguments? b:ClassBody? { return new nodes.ASTNode('EnumConstant', pr([a, i, arg, b]));}
EnumBodyDeclarations = s:SEMI b:ClassBodyDeclaration* { return new nodes.ASTNode('EnumBodyDeclarations', pr([s, b])); }
LocalVariableDeclarationStatement = a:( FINAL / Annotation )* t:Type v:VariableDeclarators s:SEMI { return new nodes.ASTNode('LocalVariableDeclarationStatement', pr([a, t, v, s])); }
VariableDeclarators = v:VariableDeclarator r:( COMMA VariableDeclarator )* { return new nodes.ASTNode('VariableDeclarators', pr([v, r])); }
VariableDeclarator = i:Identifier d:Dim* r:( EQU VariableInitializer )? { return new nodes.ASTNode('VariableDeclarator', pr([i, d, r]));}
FormalParameters = l:LPAR d:FormalParameterDecls? r:RPAR { return new nodes.ASTNode('FormalParameters', pr([l, d, r])); }
FormalParameter = a:( FINAL / Annotation )* t:Type v:VariableDeclaratorId { return new nodes.ASTNode('FormalParameter', pr([a, t, v])); }
FormalParameterDecls = a:( FINAL / Annotation )* t:Type r:FormalParameterDeclsRest { return new nodes.ASTNode('FormalParameterDecls', pr([a, t, r]));}
FormalParameterDeclsRest = v:VariableDeclaratorId r:( COMMA FormalParameterDecls )? { return new nodes.ASTNode('FormalParameterDeclsRest', pr([v, r])); }
	/ e:ELLIPSIS v:VariableDeclaratorId { return new nodes.ASTNode('FormalParameterDeclsRest', [e, v]); }

VariableDeclaratorId = i:Identifier d:Dim* 							{ return new nodes.ASTNode('VariableDeclaratorId', pr([i, d])); }
Block = l:LWING b:BlockStatements r:RWING 							{ return new nodes.ASTNode('Block', [l, b, r]); }
BlockStatements = b:BlockStatement* 								{ return new nodes.ASTNode('BlockStatements', pr([b])); }
BlockStatement = l:LocalVariableDeclarationStatement 				{ return new nodes.ASTNode('BlockStatement', [ l ]); }
	/ m:Modifier* d:( ClassDeclaration / EnumDeclaration ) 			{ return new nodes.ASTNode('BlockStatement', pr([m, d])); }
	/ s:Statement 													{ return new nodes.ASTNode('BlockStatement', [ s ]); }

Statement = b:Block { return new nodes.ASTNode('Statement', [ b ]); }
	/ a:ASSERT e:Expression r:( COLON Expression )? s:SEMI { return new nodes.ASTNode('Statement', pr([ a, e, r, s ])); }
	/ i:IF p:ParExpression s:Statement r:( ELSE Statement)? { return new nodes.ASTNode('Statement', pr([ i, p, s, r])); }
	/ f:FOR l:LPAR i:ForInit? s:SEMI e:Expression? SEMI u:ForUpdate? r:RPAR st:Statement { return new nodes.ASTNode('Statement', pr([f, l, i, s, e, s, u, r, st])); }
	/ f:FOR l:LPAR p:FormalParameter c:COLON e:Expression r:RPAR s:Statement { return new nodes.ASTNode('Statement', [ f, l, p, c, e, r, s ]); }
	/ w:WHILE p:ParExpression s:Statement { return new nodes.ASTNode('Statement', [ w, p, s ]); }
	/ d:DO st:Statement w:WHILE p:ParExpression s:SEMI { return new nodes.ASTNode('Statement', [ d, st, w, p, s ]); }
	/ t:TRY b:Block r:( Catch_+ Finally_? / Finally_ ) { return new nodes.ASTNode('Statement', pr([ t, b, r ])); }
	/ s:SWITCH p:ParExpression l:LWING sg:SwitchBlockStatementGroups r:RWING { return new nodes.ASTNode('Statement', [ s, p, l, sg, r ]); }
	/ s:SYNCHRONIZED p:ParExpression b:Block { return new nodes.ASTNode('Statement', [ s, p, b ]); }
	/ r:RETURN e:Expression? s:SEMI { return new nodes.ASTNode('Statement', pr([r, e, s ])); }
	/ t:THROW e:Expression s:SEMI { return new nodes.ASTNode('Statement', [ t, e, s ]); }
	/ b:BREAK i:Identifier? s:SEMI { return new nodes.ASTNode('Statement', pr([b, i, s])); }
	/ c:CONTINUE i:Identifier? s:SEMI { return new nodes.ASTNode('Statement', pr([c, i, s])); }
	/ i:Identifier c:COLON s:Statement { return new nodes.ASTNode('Statement', [ i, c, s ]); }
	/ e:StatementExpression s:SEMI { return new nodes.ASTNode('Statement', [ e, s ]); }
	/ s:SEMI { return new nodes.ASTNode('Statement', [ s ]); }


Catch_ = c:CATCH l:LPAR f:FormalParameter r:RPAR b:Block { return new nodes.ASTNode('Catch', [ c, l, f, r, b ]); }
Finally_ = FINALLY b:Block { return new nodes.ASTNode('Finally', [ b ]); } 
SwitchBlockStatementGroups = s:SwitchBlockStatementGroup* { return new nodes.ASTNode('SwitchBlockStatementGroups', pr([s])); }
SwitchBlockStatementGroup = s:SwitchLabel b:BlockStatements { return new nodes.ASTNode('SwitchBlockStatementGroup', [ s, b ]); }
SwitchLabel = c:CASE e:ConstantExpression cl:COLON { return new nodes.ASTNode('SwitchLabel', [ c, e, cl ]); }
	/ c:CASE e:EnumConstantName cl:COLON { return new nodes.ASTNode('SwitchLabel', [ c, e, cl ]); }
	/ d:DEFAULT c:COLON { return new nodes.ASTNode('SwitchLabel', [ d, c ]); }

ForInit = a:( FINAL / Annotation )* t:Type v:VariableDeclarators  	{ return new nodes.ASTNode('ForInit', pr([a, t, v])); }
	/ s:StatementExpression r:( COMMA StatementExpression )* 		{ return new nodes.ASTNode('ForInit', pr([s, r])); }

ForUpdate = s:StatementExpression r:( COMMA StatementExpression )* 	{ return new nodes.ASTNode('ForUpdate', pr([s, r])); }
EnumConstantName = i:Identifier { return new nodes.ASTNode('EnumConstantName', [ i ]);} 
StatementExpression = e:Expression { return new nodes.ASTNode('StatementExpression', [ e ]); }
ConstantExpression = e:Expression { return new nodes.ASTNode('ConstantExpression', [ e ]); }
Expression = c:ConditionalExpression r:( AssignmentOperator ConditionalExpression )* { return new nodes.ASTNode('Expression', pr([c, r])); }
AssignmentOperator = o:( EQU / PLUSEQU / MINUSEQU / STAREQU / DIVEQU / ANDEQU / OREQU / HATEQU / MODEQU / SLEQU / SREQU / SREQU / BSREQU ) { return new nodes.ASTNode('AssignmentOperator', [ o ]);}
ConditionalExpression = c:ConditionalOrExpression r:( QUERY Expression COLON ConditionalExpression )* { return new nodes.ASTNode('ConditionalExpression', pr([c, r])); }
ConditionalOrExpression = c:ConditionalAndExpression r:( OROR ConditionalAndExpression )* { return new nodes.ASTNode('ConditionalOrExpression', pr([c, r])); }
ConditionalAndExpression = c:InclusiveOrExpression r:( ANDAND InclusiveOrExpression )* { return new nodes.ASTNode('ConditionalAndExpression', pr([c, r])); }
InclusiveOrExpression = c:ExclusiveOrExpression r:( OR ExclusiveOrExpression )* { return new nodes.ASTNode('InclusiveOrExpression', pr([c, r])); }
ExclusiveOrExpression = c:AndExpression r:( HAT AndExpression )* { return new nodes.ASTNode('ExclusiveOrExpression', pr([c, r])); }
AndExpression = c:EqualityExpression r:( AND EqualityExpression )* { return new nodes.ASTNode('AndExpression', pr([c, r])); }
EqualityExpression = c:RelationalExpression r:( ( EQUAL / NOTEQUAL ) RelationalExpression )* { return new nodes.ASTNode('EqualityExpression', pr([c, r])); }
RelationalExpression = c:ShiftExpression r:( ( ( LE / GE / LT / GT ) ShiftExpression ) / INSTANCEOF ReferenceType )* { return new nodes.ASTNode('RelationalExpression', pr([c, r])); }
ShiftExpression = c:AdditiveExpression r:( ( SL / SR / BSR ) AdditiveExpression )* { return new nodes.ASTNode('ShiftExpression', pr([c, r])); }
AdditiveExpression = c:MultiplicativeExpression r:( ( PLUS / MINUS ) MultiplicativeExpression )* { return new nodes.ASTNode('AdditiveExpression', pr([c, r])); }
MultiplicativeExpression = c:UnaryExpression r:( ( STAR / DIV / MOD ) UnaryExpression )* { return new nodes.ASTNode('MultiplicativeExpression', pr([c, r])); }

UnaryExpression = op:PrefixOp e:UnaryExpression { return new nodes.ASTNode('UnaryExpression', [op, e]); }
	/ l:LPAR t:Type r:RPAR e:UnaryExpression 	{ return new nodes.ASTNode('UnaryExpression', [l, t, r, e]); }
	/ p:Primary s:Selector* op:PostFixOp* 		{ return new nodes.ASTNode('UnaryExpression', pr([p, s, op])); }

Primary = p:ParExpression 																	{ return new nodes.ASTNode('Primary', [ p ]); }
	/ n:NonWildcardTypeArguments r:( ExplicitGenericInvocationSuffix / THIS Arguments ) 	{ return new nodes.ASTNode('Primary', [n, r]); }
	/ t:THIS a:Arguments? 																	{ return new nodes.ASTNode('Primary', pr([t, a])); }
	/ s:SUPER ss:SuperSuffix 																{ return new nodes.ASTNode('Primary', [s, ss]); }
	/ l:Literal 																			{ return new nodes.ASTNode('Primary', [ l ]); }
	/ n:NEW c:Creator 																		{ return new nodes.ASTNode('Primary', [n, c]); }
	/ qi:QualifiedIdentifier i:IdentifierSuffix? 											{ return new nodes.ASTNode('Primary', pr([qi, i])); }
	/ t:BasicType d:Dim* dt:DOT c:CLASS 													{ return new nodes.ASTNode('Primary', pr([t, d, dt, c])); }
	/ v:VOID dt:DOT c:CLASS 																{ return new nodes.ASTNode('Primary', [v, dt, c]); }

IdentifierSuffix = l:LBRK r:( RBRK Dim* DOT CLASS / Expression RBRK ) 	{ return new nodes.ASTNode('IdentifierSuffix', [l, r]); }
	/ a:Arguments 														{ return new nodes.ASTNode('IdentifierSuffix', [ a ]); }
	/ d:DOT r:( CLASS / ExplicitGenericInvocation / THIS / SUPER Arguments / NEW NonWildcardTypeArguments? InnerCreator ) { return new nodes.ASTNode('IdentifierSuffix', pr([d, r])); }

ExplicitGenericInvocation = n:NonWildcardTypeArguments e:ExplicitGenericInvocationSuffix { return new nodes.ASTNode('ExplicitGenericInvocation', [n, e]); }
NonWildcardTypeArguments = l:LPOINT t:ReferenceType e:( COMMA ReferenceType )* r:RPOINT { return new nodes.ASTNode('NonWildcardTypeArguments', pr([l, t, e, r])); }
ExplicitGenericInvocationSuffix = s:SUPER ss:SuperSuffix { return new nodes.ASTNode('ExplicitGenericInvocationSuffix', [s, ss]); }
	/ i:Identifier a:Arguments { return new nodes.ASTNode('ExplicitGenericInvocationSuffix', [i, a]); }

PrefixOp = op:( INC / DEC / BANG / TILDA / PLUS / MINUS ) { return new nodes.ASTNode('PrefixOp', [ op ]); }
PostFixOp = op:( INC / DEC ) { return new nodes.ASTNode('PostFixOp', [ op ])}

Selector = d:DOT i:Identifier a:Arguments? { return new nodes.ASTNode('Selector', pr([ d, i, a])); }
	/ d:DOT e:ExplicitGenericInvocation { return new nodes.ASTNode('Selector', [d, e]); }
	/ d:DOT t:THIS { return new nodes.ASTNode('Selector', [d, t]); }
	/ d:DOT s:SUPER ss:SuperSuffix { return new nodes.ASTNode('Selector', [d, s, ss]); }
	/ d:DOT n:NEW nn:NonWildcardTypeArguments? i:InnerCreator { return new nodes.ASTNode('Selector', pr([d, n, nn, i])); }
	/ d:DimExpr { return new nodes.ASTNode('Selector', [ d ]); }

SuperSuffix = a:Arguments  { return new nodes.ASTNode('SuperSuffix', [ a ]); }
	/ d:DOT i:Identifier a:Arguments? { return new nodes.ASTNode('SuperSuffix', pr([d, i, a])); }

BasicType = t:( "byte" / "short" / "char" / "int" / "long" / "float" / "double" / "boolean" ) !LetterOrDigit Spacing { return new nodes.Terminal(t);}
Arguments = l:LPAR e:( Expression ( COMMA Expression )* )? r:RPAR { return new nodes.ASTNode('Arguments', pr([l, e, r])); }
Creator = nn:NonWildcardTypeArguments? c:CreatedName r:ClassCreatorRest { return new nodes.ASTNode('Creator', pr([nn, c, r ])); }
	/ nn:NonWildcardTypeArguments? t:( ClassType / BasicType ) r:ArrayCreatorRest { return new nodes.ASTNode('Creator', pr([ nn, t, r ])); }

CreatedName = i:Identifier nn:NonWildcardTypeArguments? r:( DOT Identifier NonWildcardTypeArguments? )* { return new nodes.ASTNode('CreatedName', pr([i, nn, r])); }
InnerCreator = i:Identifier r:ClassCreatorRest { return new nodes.ASTNode('InnerCreator', [ i, r ]); }
ArrayCreatorRest = l:LBRK r:( RBRK Dim* ArrayInitializer / Expression RBRK DimExpr* Dim* ) { return new nodes.ASTNode('ArrayCreatorRest', pr([ l, r ])); }
ClassCreatorRest = a:Arguments b:ClassBody? { return new nodes.ASTNode('ClassCreatorRest', pr([ a, b])); }
ArrayInitializer = l:LWING e:( VariableInitializer ( COMMA VariableInitializer )* )? c:COMMA? r:RWING { return new nodes.ASTNode('ArrayInitializer', [ l, e, c, r ]); }
VariableInitializer = c:(ArrayInitializer / Expression) { return new nodes.ASTNode('VariableInitializer', [ c ]); }
ParExpression = l:LPAR e:Expression r:RPAR { return new nodes.ASTNode('ParExpression', [ l, e, r ]); }
QualifiedIdentifier = h:Identifier t:( DOT Identifier )* { return new nodes.ASTNode('QualifiedIdentifier', pr([h, t]));}
Dim = l:LBRK r:RBRK { return new nodes.ASTNode('Dim', [ l, r]); }
DimExpr = l:LBRK e:Expression r:RBRK { return new nodes.ASTNode('DimExpr', [ l, e, r ]); }
Type = t:( BasicType / ClassType ) d:Dim* { return new nodes.ASTNode('Type', pr([t, d])); }
ReferenceType = t:BasicType d:Dim+ { return new nodes.ASTNode('ReferenceType', pr([ t, d])); }
	/ t:ClassType d:Dim* { return new nodes.ASTNode('ReferenceType', pr([ t, d ])); }

ClassType = i:Identifier t:TypeArguments? r:( DOT Identifier TypeArguments? )* { return new nodes.ASTNode('ClassType', pr([ i, t, r])); }
ClassTypeList = c:ClassType r:( COMMA ClassType )* { return new nodes.ASTNode('ClassTypeList', pr([ c , r])); }
TypeArguments = l:LPOINT t:TypeArgument e:( COMMA TypeArgument )* r:RPOINT { return new nodes.ASTNode('TypeArguments', pr([ l, t, e, r ])); }
TypeArgument = r:ReferenceType { return new nodes.ASTNode('TypeArgument', [ r ]); }
	/ q:QUERY r:( ( EXTENDS / SUPER ) ReferenceType )? { return new nodes.ASTNode('TypeArgument', pr([ q, r ])); }

TypeParameters = l:LPOINT t:TypeParameter e:( COMMA TypeParameter )* r:RPOINT { return new nodes.ASTNode('TypeParameters', pr([ l, t, e, r ])); }
TypeParameter = i:Identifier r:( EXTENDS Bound )? { return new nodes.ASTNode('TypeParameter', pr([ i, r ])); }
Bound = c:ClassType r:( AND ClassType )* { return new nodes.ASTNode('Bound', pr([ c, r ])); }
Modifier = a:Annotation { return new nodes.ASTNode('Modifier', pr([ a ])); }
	/ m:( 'public' / 'protected' / 'private' / 'static' / 'abstract' / 'final' / 'native' / 'synchronized' / 'transient' / 'volatile' / 'strictfp' ) !LetterOrDigit Spacing { return new nodes.ASTNode('Modifier', [ new nodes.Terminal(m) ]);}
AnnotationTypeDeclaration = a:AT i:INTERFACE id:Identifier b:AnnotationTypeBody { return new nodes.ASTNode('AnnotationTypeDeclaration', [ a, i, id, b ]); }
AnnotationTypeBody = l:LWING e:AnnotationTypeElementDeclaration* r:RWING { return new nodes.ASTNode('AnnotationTypeBody', pr([ l, e, r ])); }
AnnotationTypeElementDeclaration = m:Modifier* r:AnnotationTypeElementRest { return new nodes.ASTNode('AnnotationTypeElementDeclaration', pr([m, r ])); }
	/ s:SEMI { return new nodes.ASTNode('AnnotationTypeElementDeclaration', [ s ]); }

AnnotationTypeElementRest = t:Type a:AnnotationMethodOrConstantRest s:SEMI { return new nodes.ASTNode('AnnotationTypeElementRest', [ t, a, s ]); }
	/ c:ClassDeclaration { return new nodes.ASTNode('AnnotationTypeElementRest', [ c ]); }
	/ e:EnumDeclaration { return new nodes.ASTNode('AnnotationTypeElementRest', [ e ]); }
	/ i:InterfaceDeclaration { return new nodes.ASTNode('AnnotationTypeElementRest', [ i ]); }
	/ a:AnnotationTypeDeclaration{ return new nodes.ASTNode('AnnotationTypeElementRest', [ a ]); }

AnnotationMethodOrConstantRest = s:(AnnotationMethodRest / AnnotationConstantRest) { return new nodes.ASTNode('AnnotationMethodOrConstantRest', [ s ]); }
AnnotationMethodRest = i:Identifier l:LPAR r:RPAR d:DefaultValue? { return new nodes.ASTNode('AnnotationMethodRest', [ i, l, r, d ]); }
AnnotationConstantRest = v:VariableDeclarators { return new nodes.ASTNode('AnnotationConstantRest', [ v ]); }
DefaultValue = d:DEFAULT e:ElementValue { return new nodes.ASTNode('DefaultValue', [ d, e ]); }
Annotation = a:AT q:QualifiedIdentifier r:AnnotationRest? { return new nodes.ASTNode('Annotation', pr([ a, q, r ])); }
AnnotationRest = r:(NormalAnnotationRest / SingleElementAnnotationRest) { return new nodes.ASTNode('AnnotationRest', [ r ]); }
NormalAnnotationRest = l:LPAR e:ElementValuePairs? r:RPAR { return new nodes.ASTNode('NormalAnnotationRest', pr([ l, e, r ])); }
ElementValuePairs = e:ElementValuePair r:( COMMA ElementValuePair )* { return new nodes.ASTNode('ElementValuePairs', pr([ e, r ])); }
ElementValuePair = i:Identifier e:EQU v:ElementValue { return new nodes.ASTNode('ElementValuePair', [ i, e, v ]); }
ElementValue = e:(ConditionalExpression / Annotation / ElementValueArrayInitializer) { return new nodes.ASTNode('ElementValue', [ e ]); }
ElementValueArrayInitializer = l:LWING e:ElementValues? c:COMMA? r:RWING { return new nodes.ASTNode('ElementValueArrayInitializer', pr([ l, e, c, r ])); }
ElementValues = e:ElementValue r:( COMMA ElementValue )* { return new nodes.ASTNode('ElementValues', pr([ e, r ])); }
SingleElementAnnotationRest = l:LPAR e:ElementValue r:RPAR { return new nodes.ASTNode('SingleElementAnnotationRest', [ l, e, r ]); }
Spacing = s:( [ \t\r\n]+ / "/*" (!"*/" .)* "*/" / "//" (![\r\n] .)* [\r\n] )* { return new nodes.Terminal('Spacing');}
Identifier = !Keyword l1:Letter ls:LetterOrDigit* Spacing { return new nodes.ASTNode('Identifier', [ new nodes.Terminal(l1+ls.join('')) ] ); }
Letter = '\\' UnicodeEscape / [a-zA-Z_$]
LetterOrDigit = '\\' UnicodeEscape / [a-zA-Z0-9_$]
Keyword = k:( 'assert' / 'break' / 'break' / 'case' / 'catch' / 'class' / 'const' / 'continue' /'default' / 'do' / 'else' / 'enum' / 'extends' / 'finally' / 'final' / 'for' / 'goto' / 'if' / 'implements' / 'import' / 'interface' / 'instanceof' / 'new' / 'package' / 'return' / 'static' / 'super' / 'switch' / 'synchronized' / 'this' / 'throws' / 'try' / 'void' / 'while' ) !LetterOrDigit { return new nodes.Terminal(k); }
ASSERT = t:"assert" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
BREAK =t:"break" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
CASE = t:"case" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
CATCH = t:"catch" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
CLASS = t:"class" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
CONTINUE = t:"continue" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
DEFAULT = t:"default" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
DO = t:"do" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
ELSE = t:"else" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
ENUM = t:"enum" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
EXTENDS = t:"extends" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
FINALLY = t:"finally" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
FINAL = t:"final" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
FOR = t:"for" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
IF = t:"if" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
IMPLEMENTS = t:"implements" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
IMPORT = t:"import" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
INTERFACE = t:"interface" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
INSTANCEOF = t:"instanceof" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
NEW = t:"new" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
PACKAGE = t:"package" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
RETURN = t:"return" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
STATIC = t:"static" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
SUPER = t:"super" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
SWITCH = t:"switch" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
SYNCHRONIZED = t:"synchronized" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
THIS = t:"this" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
THROWS = t:"throws" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
THROW = t:"throw" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
TRY = t:"try" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
VOID = t:"void" !LetterOrDigit Spacing { return new nodes.Terminal(t);}
WHILE = t:"while" !LetterOrDigit Spacing { return new nodes.Terminal(t);}

Literal = l:( FloatLiteral / IntegerLiteral / CharLiteral / StringLiteral / 'true' !LetterOrDigit / "false" !LetterOrDigit / "null" !LetterOrDigit ) Spacing { return new nodes.ASTNode('Literal', [ l ]);}
IntegerLiteral = l:( HexNumeral / OctalNumeral / DecimalNumeral ) t:[Ll]? { return new nodes.ASTNode('IntegerLiteral', pr([l, t!=''?new nodes.Terminal(t):'']))}
DecimalNumeral = '0' { return new nodes.ASTNode('DecimalNumeral', [ new nodes.Terminal('0') ]); }
	/ h:[1-9] t:Digit* { return new nodes.ASTNode('DecimalNumeral', [new nodes.Terminal(h+t.join(""))]); }
HexNumeral = h:'0' x:[xX] t:HexDigit+ { return new nodes.ASTNode('HexNumeral', [ new nodes.Terminal(h+x+t.join(""))]); }
HexDigit = [a-fA-F0-9] 
OctalNumeral = h:'0' t:[0-7]+ { return new nodes.ASTNode('OctalNumeral', [ new nodes.Terminal(h+t.join(""))]); }
FloatLiteral = f:( HexFloat / DecimalFloat ) { return new nodes.ASTNode('FloatLiteral', [ f ]); }
DecimalFloat = h:Digit+ '.' t:Digit* e:Exponent? q:[fFdD]? { return new nodes.ASTNode('DecimalFloat', pr([ new nodes.Terminal(h.join("")), new nodes.Terminal('.'), t!=''?new nodes.Terminal(t.join("")):'', e, q!=''?new nodes.Terminal(q):''])); }
	/ '.' h:Digit+ e:Exponent? q:[fFdD]? { return new nodes.ASTNode('DecimalFloat', pr([ new nodes.Terminal('.'), new nodes.Terminal(h.join("")), e, q!=''?new nodes.Terminal(q):''])); }
	/ h:Digit+ e:Exponent q:[fFdD]? { return new nodes.ASTNode('DecimalFloat', pr([ new nodes.Terminal(h.join("")), e, q!=''?new nodes.Terminal(q):''])); }
	/ h:Digit+ e:Exponent? q:[fFdD] { return new nodes.ASTNode('DecimalFloat', pr([ new nodes.Terminal(h.join("")), e, q!=''?new nodes.Terminal(q):''])); }
Exponent = e:[eE] s:[+\-]? d:Digit+ { return new nodes.ASTNode('Exponent', [ new nodes.Terminal(e+s+d.join(""))]); }
Digit = [0-9]
HexFloat = h:HexSignificant b:BinaryExponent q:[fFdD]? { return new nodes.ASTNode('HexFloat', pr([ h, b, q!=''?new nodes.Terminal(q):''])); }
HexSignificant = h:'0' x:[xX] t:HexDigit* '.' tt:HexDigit+ { return new nodes.ASTNode('HexSignificant', [ new nodes.Terminal(h+x+(t!=''?t.join(""):t)+'.'+tt.join("")) ]); }
	/ h:HexNumeral d:'.'? { return new nodes.ASTNode('HexSignificant', pr([ h, d!=''?new nodes.Terminal('.'):''])); }
BinaryExponent = p:[pP] s:[+\-]? d:Digit+ { return new nodes.ASTNode('BinaryExponent', [ new nodes.Terminal(p+s+d.join("")) ]);}
CharLiteral = '\'' e:( Escape / !"'\\" .) '\'' { return new new nodes.ASTNode('CharLiteral', [ new nodes.Terminal("'"), typeof e == 'string'?new nodes.Terminal(e):e, new nodes.Terminal("'") ]); }
StringLiteral = '"' a:( Escape / ![\r\n\"\\] .)* '"' { return new nodes.ASTNode('StringLiteral', [ new nodes.Terminal(a.join("")) ]);}
Escape = '\\' q:( [btnfr\"\'\\] / OctalEscape / UnicodeEscape ) { return new nodes.ASTNode('Escape', [ new nodes.Terminal('\\'), typeof q == 'string'?new nodes.Terminal(q):q ])}
OctalEscape = x:[0-3] y:[0-7] z:[0-7] { return new nodes.ASTNode('OctalEscape', [ new nodes.Terminal(x+y+z) ]); }
	/ x:[0-7] y:[0-7] { return new nodes.ASTNode('OctalEscape', [ new nodes.Terminal(x+y) ]); }
	/ x:[0-7] { return new nodes.ASTNode('OctalEscape', [ new nodes.Terminal(x) ]); }

UnicodeEscape = 'u'+ x:HexDigit y:HexDigit z:HexDigit w:HexDigit { return new nodes.ASTNode('UnicodeEscape', [ new nodes.Terminal('u'+x+y+z+w)]); }

AT = op:"@" Spacing { return new nodes.Terminal(op); }
AND = op:"&" ![=&] Spacing { return new nodes.Terminal(op); }
ANDAND = op:"&&" Spacing { return new nodes.Terminal(op); }
ANDEQU = op:"&=" Spacing { return new nodes.Terminal(op); }
BANG = op:"!" !"=" Spacing { return new nodes.Terminal(op); }
BSR = op:">>>" !"=" Spacing { return new nodes.Terminal(op); }
BSREQU = op:">>>=" Spacing { return new nodes.Terminal(op); }
COLON = op:":" Spacing { return new nodes.Terminal(op); }
COMMA = op:"," Spacing { return new nodes.Terminal(op); }
DEC = op:"--" Spacing { return new nodes.Terminal(op); }
DIV = op:"/" !"=" Spacing { return new nodes.Terminal(op); }
DIVEQU = op:"/=" Spacing { return new nodes.Terminal(op); }
DOT = op:"." Spacing { return new nodes.Terminal(op); }
ELLIPSIS = op:"..." Spacing { return new nodes.Terminal(op); }
EQU = op:"=" !"=" Spacing { return new nodes.Terminal(op); }
EQUAL = op:"==" Spacing { return new nodes.Terminal(op); }
GE = op:">=" Spacing { return new nodes.Terminal(op); }
GT = op:">" ![=>] Spacing { return new nodes.Terminal(op); }
HAT = op:"^" !"=" Spacing { return new nodes.Terminal(op); }
HATEQU = op:"^=" Spacing { return new nodes.Terminal(op); }
INC = op:"++" Spacing { return new nodes.Terminal(op); }
LBRK = op:"[" Spacing { return new nodes.Terminal(op); }
LE = op:"<=" Spacing { return new nodes.Terminal(op); }
LPAR = op:"(" Spacing { return new nodes.Terminal(op); }
LPOINT = op:"<" Spacing { return new nodes.Terminal(op); }
LT = op:"<" ![=<] Spacing { return new nodes.Terminal(op); }
LWING = op:"{" Spacing { return new nodes.Terminal(op); }
MINUS = op:"-" ![=-] Spacing { return new nodes.Terminal(op); }
MINUSEQU = op:"-=" Spacing { return new nodes.Terminal(op); }
MOD = op:"%" !"=" Spacing { return new nodes.Terminal(op); }
MODEQU = op:"%=" Spacing { return new nodes.Terminal(op); }
NOTEQUAL = op:"!=" Spacing { return new nodes.Terminal(op); }
OR = op:"|" ![=|] Spacing { return new nodes.Terminal(op); }
OREQU = op:"|=" Spacing { return new nodes.Terminal(op); }
OROR = op:"||" Spacing { return new nodes.Terminal(op); }
PLUS = op:"+" ![=+] Spacing { return new nodes.Terminal(op); }
PLUSEQU = op:"+=" Spacing { return new nodes.Terminal(op); }
QUERY = op:"?" Spacing { return new nodes.Terminal(op); }
RBRK = op:"]" Spacing { return new nodes.Terminal(op); }
RPAR = op:")" Spacing { return new nodes.Terminal(op); }
RPOINT = op:">" Spacing { return new nodes.Terminal(op); }
RWING = op:"}" Spacing { return new nodes.Terminal(op); }
SEMI = op:";" Spacing { return new nodes.Terminal(op); }
SL = op:"<<" !"=" Spacing { return new nodes.Terminal(op); }
SLEQU = op:"<<=" Spacing { return new nodes.Terminal(op); }
SR = op:">>" ![=>] Spacing { return new nodes.Terminal(op); }
SREQU = op:">>=" Spacing { return new nodes.Terminal(op); }
STAR = op:"*" !"=" Spacing { return new nodes.Terminal(op); }
STAREQU = op:"*=" Spacing { return new nodes.Terminal(op); }
TILDA = op:"~" Spacing { return new nodes.Terminal(op); }
