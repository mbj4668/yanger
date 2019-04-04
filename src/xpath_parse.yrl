%%%                                                       -*- mode: erlang -*-
%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.0, (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.erlang.org/license/EPL1_0.txt
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Original Code is xmerl-0.6
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.
%%%
%%%----------------------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%%----------------------------------------------------------------------
%%% File:       xmerl_xpath_parse.erl
%%% Author       : Ulf Wiger <ulf.wiger@ericsson.com>
%%% Description  : Yecc spec for XPATH grammar
%%%    This version of the parser is based on the XPATH spec:
%%%    http://www.w3.org/TR/1999/REC-xpath-19991116 (XPATH version 1.0)
%%%----------------------------------------------------------------------
%%
%% Modified for tail-f by thomasl
%%
%% This is a second descendant of xmerl_xpath_parse, which generates
%% a more suitable parse tree + does not create atoms wantonly.
%%
%% Note that xp_scan gets rid of some potential tokens at lex-time.
%% For instance, "//" is rescanned as "descendant-or-self::node()",
%% and so on.
%%
%% processing-instruction() never appears
%% comment() never appears
%% <other types?>

Nonterminals
        'LocationPath'
        'AbsoluteLocationPath'
        'RelativeLocationPath'
        'Step'
%%	'AxisSpecifier'
        'NodeTest'
        'Predicate'
        'PredicateExpr'
        'AbbreviatedAbsoluteLocationPath'
        'AbbreviatedRelativeLocationPath'
        'AbbreviatedStep'
%%	'AbbreviatedAxisSpecifier'
        'Expr'
        'PrimaryExpr'
        'FunctionCall'
        'Argument'
        'UnionExpr'
        'PathExpr'
        'FilterExpr'
        'OrExpr'
        'AndExpr'
        'EqualityExpr'
        'RelationalExpr'
        'AdditiveExpr'
        'MultiplicativeExpr'
        'UnaryExpr'
%%	'Operator'
%%	'OperatorName'
        'MultiplyOperator'
        'NameTest'
        '<PredicateList>'
        '<PredicateMember>'
        '<ArgumentList>'
        '<ArgumentMember>'
        .

Terminals
        'number'
        'axis'
        'node_type'
        'literal'
        'prefix_test'
        'var_reference'
        'function_name'
        'name'
        'processing-instruction'
        'wildcard'
        '(' ')' '[' ']' '.' '..' '@' ',' '::'
        'and' 'or' 'mod' 'div'
        '/' '//' '|' '+' '-' '=' '!=' '<' '<=' '>' '>='
        '*'
        .

Rootsymbol 'Expr'.

Endsymbol '$end' .

Left 100 'or' .
Left 200 'and' .
Left 300 '=' .
Left 300 '!=' .
Left 400 '<' .
Left 400 '>=' .
Left 400 '>' .
Left 400 '<=' .
Unary 500 '-' .

%%------------------------------------------------------------
%% Clauses
%%

%% [1]
'LocationPath' -> 'RelativeLocationPath' : {relative, '$1'} .
'LocationPath' -> 'AbsoluteLocationPath' : {absolute, '$1'}.

%% [2]
'AbsoluteLocationPath' -> '/' 'RelativeLocationPath' : '$2' .
'AbsoluteLocationPath' -> '/' : [] .

%% [3]
'RelativeLocationPath' -> 'AbbreviatedAbsoluteLocationPath' : ['$1'] .
'RelativeLocationPath' -> 'Step' : ['$1'] .
'RelativeLocationPath' -> 'RelativeLocationPath' '/' 'Step' :
                              '$1' ++ ['$3'].
'RelativeLocationPath' -> 'AbbreviatedRelativeLocationPath' : ['$1'] .

%% [4]
%% - should @name be {step, attribute, Name, ...} ?

'Step' -> 'axis' '::' 'NodeTest' '<PredicateList>'
        : {step, value('$1'), '$3', '$4'} .
'Step' -> 'axis' '::' 'NodeTest'
        : {step, value('$1'), '$3', []} .
'Step' -> '@' 'name' '<PredicateList>'
        : {step, attribute, '$2', '$3'} .
'Step' -> '@' 'name'
        : {step, attribute, '$2', []} .
'Step' -> 'NodeTest' '<PredicateList>'
        : {step, child, '$1', '$2'} .
'Step' -> 'NodeTest'
        : {step, child, '$1', []} .
'Step' -> 'AbbreviatedStep' : '$1' .


'<PredicateList>' -> '<PredicateMember>' : lists:reverse('$1') .


'<PredicateMember>' -> '<PredicateMember>' 'Predicate'
        : ['$2'|'$1'] .
'<PredicateMember>' -> 'Predicate' : ['$1'] .


%% [5]
%% Not used ([4] is explicitly expaned instead)
%%'AxisSpecifier' -> 'axis' '::' : '$1' .
%%'AxisSpecifier' -> 'AbbreviatedAxisSpecifier' : '$1' .


%% [7]
%% - NB: processing-instruction(x) not handled in confd xpath,
%%     since there basically are no processing-instrs (possibly
%%     except the heading ?xml item)
'NodeTest' -> 'NameTest' : name_test('$1') .
'NodeTest' -> 'node_type' '(' ')' : {node_type, value('$1')} .
'NodeTest' -> 'processing-instruction' '(' 'literal' ')'
        : {processing_instruction, value('$3')} .


%% [8]
'Predicate' -> '[' 'PredicateExpr' ']' : {pred, '$2'} .

%% [9]
'PredicateExpr' -> 'Expr' : '$1' .

%% [10]
%% - return a list of location steps
%% - is this the right expansion of "//..."? verify

'AbbreviatedAbsoluteLocationPath'  -> '//' 'RelativeLocationPath'
        : [{step, descendant, wildcard, []}|'$2'] .

%% [11]
'AbbreviatedRelativeLocationPath' -> 'RelativeLocationPath' '//' 'Step'
        : '$1' ++ [{step, descendant, node, []}|'$3'] .

%% [12]
%% - I hope these expand to the right location steps, verify vs spec.
%%   for "." and ".."

'AbbreviatedStep' -> '.' : {step, self, node, []}.
'AbbreviatedStep' -> '..' : {step, parent, node, []}.

%% [13]
%% 'AbbreviatedAxisSpecifier' ->  '$empty' : 'child' .
%% 'AbbreviatedAxisSpecifier' ->  '@' : '$1' .

%% [14]
'Expr' -> 'OrExpr' : '$1' .

%% [15]
'PrimaryExpr' -> 'var_reference' : {var, value('$1')} .
'PrimaryExpr' -> '(' Expr ')' : '$2' .
'PrimaryExpr' -> 'literal' : {literal, value('$1')} .
'PrimaryExpr' -> 'number' : {number, value('$1')} .
'PrimaryExpr' -> 'FunctionCall' : '$1' .

%% [16]
'FunctionCall' -> 'function_name' '(' ')' : {function_call, value('$1'), []} .
'FunctionCall' -> 'function_name' '(' '<ArgumentList>' ')'
        : function_call(value('$1'), '$3') .

'<ArgumentList>' -> '<ArgumentMember>' : lists:reverse('$1') .

'<ArgumentMember>' -> '<ArgumentMember>' ',' 'Argument'
        : ['$3'|'$1'] .
'<ArgumentMember>' -> 'Argument' : ['$1'] .


%% [17]
'Argument' -> 'Expr' : '$1' .


%% [18]
'UnionExpr' -> 'PathExpr' : '$1' .
'UnionExpr' -> 'UnionExpr' '|' 'PathExpr' : mk_union('$1','$3').


%% [19]
%% - are these okay? verify, esp. FilterExpr items

'PathExpr' -> 'LocationPath' : '$1' .
'PathExpr' -> 'FilterExpr' : {path_expr, '$1'} .
'PathExpr' -> 'FilterExpr' '/' 'RelativeLocationPath'
                  : ['$1'|'$3'].
'PathExpr' -> 'FilterExpr' '//' 'RelativeLocationPath'
                  : {'$1', '//', '$3'} .

%% [20]

'FilterExpr' -> 'PrimaryExpr' : '$1' .
'FilterExpr' -> 'FilterExpr' 'Predicate' : {path, filter, {'$1', '$2'}} .

%% [21]
'OrExpr' -> 'AndExpr' : '$1' .
'OrExpr' -> 'OrExpr' 'or' 'AndExpr'
        : {bool, 'or', '$1', '$3'} .


%% [22]
'AndExpr' -> 'EqualityExpr' : '$1' .
'AndExpr' -> 'AndExpr' 'and' 'EqualityExpr'
        : {bool, 'and', '$1', '$3'} .

%% [23]
'EqualityExpr' -> 'RelationalExpr' : '$1' .
'EqualityExpr' -> 'EqualityExpr' '=' 'RelationalExpr'
        : {comp, '=', '$1', '$3'} .
'EqualityExpr' -> 'EqualityExpr' '!=' 'RelationalExpr'
        : {comp, '!=', '$1', '$3'} .

%%[24]
'RelationalExpr' -> 'AdditiveExpr' : '$1' .
'RelationalExpr' -> 'RelationalExpr' '<' 'AdditiveExpr'
        : {comp, '<', '$1', '$3'} .
'RelationalExpr' -> 'RelationalExpr' '>' 'AdditiveExpr'
        : {comp, '>', '$1', '$3'} .
'RelationalExpr' -> 'RelationalExpr' '<=' 'AdditiveExpr'
        : {comp, '<=', '$1', '$3'} .
'RelationalExpr' -> 'RelationalExpr' '>=' 'AdditiveExpr'
        : {comp, '>=', '$1', '$3'} .


%% [25]
'AdditiveExpr' -> 'MultiplicativeExpr' : '$1' .
'AdditiveExpr' -> 'AdditiveExpr' '+' 'MultiplicativeExpr'
        : {arith, '+', '$1', '$3'} .
'AdditiveExpr' -> 'AdditiveExpr' '-' 'MultiplicativeExpr'
        : {arith, '-', '$1', '$3'} .


%% [26]
'MultiplicativeExpr' -> 'UnaryExpr' : '$1' .
'MultiplicativeExpr' -> 'MultiplicativeExpr' 'MultiplyOperator' 'UnaryExpr'
        : {arith, '$2', '$1', '$3'} .
'MultiplicativeExpr' -> 'MultiplicativeExpr' 'div' 'UnaryExpr'
        : {arith, 'div', '$1', '$3'} .
'MultiplicativeExpr' -> 'MultiplicativeExpr' 'mod' 'UnaryExpr' :
        {arith, 'mod', '$1', '$3'} .


%% [27]
'UnaryExpr' -> 'UnionExpr' : '$1' .
'UnaryExpr' -> '-' UnaryExpr : {'negative', '$2'} .



%% [32]
%% Not used, since we don't have [28] ExprToken
%% 'Operator' -> 'OperatorName' : '$1' .
%% 'Operator' -> 'MultiplyOperator' : '$1' .
%% 'Operator' -> '/' : '$1' .
%% 'Operator' -> '//' : '$1' .
%% 'Operator' -> '|' : '$1' .
%% 'Operator' -> '+' : '$1' .
%% 'Operator' -> '-' : '$1' .
%% 'Operator' -> '=' : '$1' .
%% 'Operator' -> '!=' : '$1' .
%% 'Operator' -> '<' : '$1' .
%% 'Operator' -> '<=' : '$1' .
%% 'Operator' -> '>' : '$1' .
%% 'Operator' -> '>=' : '$1' .

%% [33]
%% 'OperatorName' -> 'and' : '$1' .
%% 'OperatorName' -> 'mod' : '$1' .
%% 'OperatorName' -> 'div' : '$1' .

%% [34]
'MultiplyOperator' -> '*' : '*' .


%% [37]

'NameTest' -> 'wildcard' : wildcard.
'NameTest' -> 'prefix_test' : {has_namespace, value('$1')} .
'NameTest' -> 'name' : {name, value('$1')} .

%% old rules
%% 'NameTest' -> 'prefix_test' : {prefix_test, value('$1')} .
%% 'NameTest' -> 'wildcard' : {wildcard, value('$1')} .


Erlang code.

% token({Token, _Line}) ->
% 	Token;
% token({Token, _Line, _Value}) ->
% 	Token.

%% return_error/2 is exported only to avoid a compiler warning.
-export([return_error/2]).

value({Token, _Line}) ->
        Token;
value({_Token, _Line, Value}) ->
        Value.

mk_union({union, Ps0}, {union, Ps1}) ->
    {union, Ps0 ++ Ps1};
mk_union({union, Ps0}, P) ->
    {union, Ps0 ++ [P]};
mk_union(P, {union, Ps1}) ->
    {union, [P|Ps1]};
mk_union(P0, P1) ->
    {union, [P0, P1]}.

name_test({name, {"", Tag}}) ->
    {name, Tag};
name_test({name, {NS, Tag}}) ->
    {name, NS, Tag};
name_test(wildcard) ->
    wildcard;
name_test({wildcard, W}) ->
    {wildcard, W};
name_test({has_namespace, NS}) ->
    {namespace, NS};
name_test(Other) ->
    exit({name_test_not_supported, ?MODULE, ?LINE, Other}).

%% xpath_bif:string_to_bif/1 checks if the "proposed" function name is
%% an actual builtin. If so, it returns the BIF name as an atom; otherwise
%% it returns {unknown_function, Func_name}.
%%
%% NB: how should calls to undefined functions be handled?
%% - the whole expression fails (not unreasonable)
%% - the function call behaves as if 'false()'
%%   (somewhat reasonable, but not(undefined(.)) will then be true ...)

function_call(Func_name, Args) when is_atom(Func_name) ->
    %% already done in scanner
    {function_call, Func_name, Args};
function_call({undefined_function, _} = Func , Args) ->
    {function_call, Func, Args};
function_call(Func_name , Args) ->
    Func = xpath_bif:string_to_bif(Func_name),
    {function_call, Func, Args}.
