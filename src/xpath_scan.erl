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
%%% @private
%%% File:       xmerl_xpath_scan.erl
%%% Author       : Ulf Wiger <ulf.wiger@ericsson.com>
%%% Description  : Token scanner for XPATH grammar
%%%
%%% Modules used : lists, xmerl_lib
%%%
%%%----------------------------------------------------------------------
%%%
%%% The XPATH grammar is a bit tricky, due to operator overloading.
%%% This version of the scanner is based on the XPATH spec:
%%% http://www.w3.org/TR/1999/REC-xpath-19991116 (XPATH version 1.0)
%%%
%%% Quote from the spec:
%%%
%%%  "The following special tokenization rules must be applied in the order
%%%  specified to disambiguate the ExprToken grammar:
%%%
%%%  o If there is a preceding token and the preceding token is not one of
%%%    @, ::. (, [, or an Operator, then a * must be recognized as a
%%%    MultiplyOperator and an NCName must be recognized as an OperatorName
%%%  o If the character following an NCName (possible after intervening
%%%    ExprWhiteSpace) is (, then the token must be recognized as a NodeType
%%%    or a FunctionName.
%%%  o If the two characters following an NCName (possible after intervening
%%%    ExprWhiteSpace) are ::, then the token must be recognized as an
%%%    AxisName.
%%%  o Otherwise, the token must not be recognized as a MultiplyOperator, an
%%%    OperatorName, a NodeType, a FunctionName, or an AxisName."
%%%----------------------------------------------------------------------
%%
%% Modified for tail-f by thomasl
%% - no atoms created with list_to_atom

-module(xpath_scan).
-vsn('0.6').
-date('00-09-21').
-author('ulf.wiger@ericsson.com').
-author('thomasl@tail-f.com').

%-compile(export_all).

%% main API
-export([tokens/1]).

%% exported helper functions
%-export([scan_number/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(space, 32).
-define(cr,    13).
-define(lf,    10).
-define(tab,   9).

%% whitespace consists of 'space', 'carriage return', 'line feed' or 'tab'
-define(whitespace(H), H==?space ; H==?cr ; H==?lf ; H==?tab).

-define(L, 1).

-define(is_letter(C), is_letter(C)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tokens(Str) ->
    tokens(strip_ws(Str), []).

tokens([], Acc) ->
    lists:reverse([{'$end', ?L, '$end'}|Acc]);
tokens(Str, Acc) ->
    case scan_token(Str, Acc) of
        {rescan, NewStr} ->
            tokens(NewStr, Acc);
        {Token, T} ->
            tokens(strip_ws(T), [Token|Acc])
    end.

%% Expr Tokens
scan_token("(" ++ T, _A) ->  {{'(', ?L, '('}, T};
scan_token(")" ++ T, _A) ->  {{')', ?L, ')'}, T};
scan_token("[" ++ T, _A) ->  {{'[', ?L, '['}, T};
scan_token("]" ++ T, _A) ->  {{']', ?L, ']'}, T};
scan_token(".." ++ T, _A) -> {rescan,"parent::node()" ++ T} ;
                                                % {{'..',?L,'..'}, T};
scan_token("@" ++ T, _A) ->  {rescan,"attribute::" ++ T};
                                                % {{'@',?L,'@'},T};
scan_token("," ++ T, _A) ->  {{',', ?L, ','}, T};
scan_token("::" ++ T, _A) -> {{'::', ?L, '::'}, T};

%% operators
scan_token("//" ++ T, _A) -> {rescan,"/descendant-or-self::node()/" ++ T};
                                                % {{'//',?L,'//'},T};
scan_token("/" ++ T, _A) ->  {{'/', ?L, '/'}, T};
scan_token("|" ++ T, _A) ->  {{'|', ?L, '|'}, T};
scan_token("+" ++ T, _A) ->  {{'+', ?L, '+'}, T};
scan_token("-" ++ T, _A) ->  {{'-', ?L, '-'}, T};
scan_token("=" ++ T, _A) ->  {{'=', ?L, '='}, T};
scan_token("!=" ++ T, _A) -> {{'!=', ?L, '!='}, T};
scan_token("<=" ++ T, _A) -> {{'<=', ?L, '<='}, T};
scan_token("<" ++ T, _A) ->  {{'<', ?L, '<'}, T};
scan_token(">=" ++ T, _A) -> {{'>=', ?L, '>='}, T};
scan_token(">" ++ T, _A) ->  {{'>', ?L, '>'}, T};
scan_token("&lt;" ++ T, _A) ->  {{'<', ?L, '<'}, T};

scan_token("*" ++ T, A) ->
    Tok =
        case A of
            [{X,_,_}|_] ->
                case special_token(X) of
                    false ->
                        {'*', ?L, '*'};
                    true ->
                        {'wildcard', ?L, 'wildcard'}
                end;
            _ ->
                {'wildcard', ?L, 'wildcard'}
        end,
    {Tok, T};

%% numbers
scan_token(Str = [H|_], _A) when H >= $0, H =< $9 ->
    scan_number(Str);
scan_token(Str = [$., H|_], _A) when H >= $0, H =< $9 ->
    scan_number(Str);
scan_token("." ++ T, _A) ->
%    {{'.', ?L, '.'}, T};
    {rescan, "self::node()" ++ T};

%% Variable Reference

scan_token([$$|T], _A) ->
    {{Prefix, Local}, T1} = scan_name(T),
    case Prefix of
        [] ->
            {{var_reference, ?L, Local}, T1};
        _ ->
            {{var_reference, ?L, Prefix++":"++Local}, T1}
    end;

scan_token([H|T], _A) when H == $" ; H == $' -> %"
    {Literal, T1} = scan_literal(T, H, []),
    {{literal, ?L, Literal}, T1};

scan_token(T, A) ->
    {{Prefix, Local}, T1} = scan_name(T),
    case A of
        [{X,_,_}|_] ->
            case special_token(X) of
                false ->
                    operator_name(Prefix, Local, T1);
                true ->
                    other_name(Prefix, Local, strip_ws(T1))
            end;
        _ ->
            other_name(Prefix, Local, strip_ws(T1))
    end.

operator_name([], "and", T) ->  {{'and', ?L, 'and'}, T};
operator_name([], "or", T) ->   {{'or', ?L, 'or'}, T};
operator_name([], "mod", T) ->  {{'mod', ?L, 'mod'}, T};
operator_name([], "div", T) ->  {{'div', ?L, 'div'}, T}.


other_name(Prefix, [], "*" ++ T) ->
    %% [37] NameTest ::= '*' | NCName ':' '*' | QName
    Token = {prefix_test, ?L, Prefix},
    {Token, T};
other_name(Prefix, Local, T = "(" ++ _) ->
    node_type_or_function_name(Prefix, Local, T);
other_name(Prefix, Local, T = "::" ++ _) ->
    axis(Prefix, Local, T);
other_name([], Local, T) ->
    Token = {name, ?L, {"", Local}},
    {Token, T};
other_name(Prefix, Local, T) ->
    %% use {Namespace_str, Local_atom, Local_str}
    Token = {name, ?L, {Prefix, Local}},
    {Token, T}.

%% node types
node_type_or_function_name([], "comment", T) ->
    {{node_type, ?L, comment}, T};
node_type_or_function_name([], "text", T) ->
    {{node_type, ?L, text}, T};
node_type_or_function_name([], "processing-instruction", T) ->
    {{'processing-instruction', ?L, 'processing-instruction'}, T};
node_type_or_function_name([], "node", T) ->
    {{node_type, ?L, node}, T};
node_type_or_function_name("", Local, T) ->
    %% if the function name does not exist, pseudo-fail
    %%  (ie, generate {undefined_function, NS_lst, Fn_lst})
    %% otherwise resolve to the builtin function
    Func = xpath_bif:string_to_bif(Local),
    {{function_name, ?L, Func}, T};
node_type_or_function_name(Prefix, Local, T) ->
    %% no XPath 1.0 function has a namespace prefix => pseudo-fail
    {{function_name, ?L, {undefined_function, Prefix ++ ":" ++ Local}}, T}.


%% axis names
axis([], "ancestor-or-self", T) ->      {{axis, ?L, ancestor_or_self}, T};
axis([], "ancestor", T) ->              {{axis, ?L, ancestor}, T};
axis([], "attribute", T) ->             {{axis, ?L, attribute}, T};
axis([], "child", T) ->                 {{axis, ?L, child}, T};
axis([], "descendant-or-self", T) ->    {{axis, ?L, descendant_or_self}, T};
axis([], "descendant", T) ->            {{axis, ?L, descendant}, T};
axis([], "following-sibling", T) ->     {{axis, ?L, following_sibling}, T};
axis([], "following", T) ->             {{axis, ?L, following}, T};
axis([], "namespace", T) ->             {{axis, ?L, namespace}, T};
axis([], "parent", T) ->                {{axis, ?L, parent}, T};
axis([], "preceding-sibling", T) ->     {{axis, ?L, preceding_sibling}, T};
axis([], "preceding", T) ->             {{axis, ?L, preceding}, T};
axis([], "self", T) ->                  {{axis, ?L, self}, T};
axis([], Axis, T) ->
    exit({invalid_axis, Axis, T}).




scan_literal([H|T], H, Acc) ->
    {lists:reverse(Acc), T};
scan_literal([H|T], Delim, Acc) ->
    scan_literal(T, Delim, [H|Acc]);
scan_literal([], Delim, Acc) ->
    exit({eof_in_quote, Delim, lists:reverse(Acc)}).

scan_name([H1, H2 | T]) when H1 == $: ; H1 == $_ ->
    if ?whitespace(H2) ->
            exit({invalid_name, [H1, H2 | "..."]});
       true ->
            scan_prefix(T, [H2, H1])
    end;
scan_name([H|T]) ->
    case ?is_letter(H) of
        true ->
            scan_prefix(T, [H]);
        false ->
            exit({invalid_name, lists:sublist([H|T], 1, 6)})
    end;
scan_name(Str) ->
    exit({invalid_name, lists:sublist(Str, 1, 6)}).

scan_prefix([], Acc) ->
    {{[], lists:reverse(Acc)}, []};
scan_prefix(Str = [H|_], Acc) when ?whitespace(H) ->
    {{[], lists:reverse(Acc)}, Str};
scan_prefix(T = "::" ++ _, Acc) ->
    %% This is the next token
    {{[], lists:reverse(Acc)}, T};
scan_prefix(":" ++ T, Acc) ->
    {LocalPart, T1} = scan_local_part(T, []),
    Prefix = lists:reverse(Acc),
    {{Prefix, LocalPart}, T1};
scan_prefix(Str = [H|T], Acc) ->
    case is_namechar(H) of
        true ->
            scan_prefix(T, [H|Acc]);
        false ->
            {{[], lists:reverse(Acc)}, Str}
    end.

scan_local_part([], Acc) ->
    {lists:reverse(Acc), []};
scan_local_part(Str = [H|_], Acc) when ?whitespace(H) ->
    {lists:reverse(Acc), Str};
scan_local_part(Str = [H|T], Acc) ->
    case is_namechar(H) of
        true ->
            scan_local_part(T, [H|Acc]);
        false ->
            {lists:reverse(Acc), Str}
    end.


scan_number(T) ->
    scan_number(T, []).

scan_number([], Acc) ->
    {{number, ?L, list_to_integer(lists:reverse(Acc))}, []};
scan_number("." ++ T, []) ->
    {Digits, T1} = scan_digits(T, ".0"),
    Number = list_to_float(Digits),
    {{number, ?L, Number}, T1};
scan_number("." ++ T, Acc) ->
    {Digits, T1} = scan_digits(T, "." ++ Acc),
    Number = list_to_float(Digits),
    {{number, ?L, Number}, T1};
scan_number([H|T], Acc) when H >= $0, H =< $9 ->
    scan_number(T, [H|Acc]);
scan_number(T, Acc) ->
    {{number, ?L, list_to_integer(lists:reverse(Acc))}, T}.

scan_digits([], Acc) ->
    {lists:reverse(Acc), []};
scan_digits([H|T], Acc) when H >= $0, H =< $9 ->
    scan_digits(T, [H|Acc]);
scan_digits(T, Acc) ->
    {lists:reverse(Acc), T}.


strip_ws([H|T]) when ?whitespace(H) ->
    strip_ws(T);
strip_ws(T) ->
    T.


special_token('*') -> true;
special_token(',') -> true;
special_token('@') -> true;
special_token('::') -> true;
special_token('(') -> true;
special_token('[') -> true;
special_token('/') -> true;
special_token('//') -> true;
special_token('|') -> true;
special_token('+') -> true;
special_token('-') -> true;
special_token('=') -> true;
special_token('!=') -> true;
special_token('<') -> true;
special_token('<=') -> true;
special_token('>') -> true;
special_token('>=') -> true;
special_token('and') -> true;
special_token('or') -> true;
special_token('mod') -> true;
special_token('div') -> true;
special_token(_) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% from xmerl_lib, R10B-9

is_namechar(X) when X == $. ; X == $- ; X == $_ ; X == $: ->
    true;
is_namechar(X) ->
    case is_letter(X) of
        true -> true;
        false ->
            case is_digit(X) of
                true -> true;
                false ->
                    case is_combining_char(X) of
                        true -> true;
                        false ->
                            is_extender(X)
                    end
            end
    end.

%% [84] Letter
is_letter(X) ->
    case is_base_char(X) of
        false ->
            is_ideographic(X);
        true ->
            true
    end.

%% [85] BaseChar
is_base_char(X) when X >= 16#0041, X =< 16#005A -> true;
is_base_char(X) when X >= 16#0061, X =< 16#007A -> true;
is_base_char(X) when X >= 16#00C0, X =< 16#00D6 -> true;
is_base_char(X) when X >= 16#00D8, X =< 16#00F6 -> true;
is_base_char(X) when X >= 16#00F8, X =< 16#00FF -> true;
is_base_char(X) when X >= 16#0100, X =< 16#0131 -> true;
is_base_char(X) when X >= 16#0134, X =< 16#013E -> true;
is_base_char(X) when X >= 16#0141, X =< 16#0148 -> true;
is_base_char(X) when X >= 16#014A, X =< 16#017E -> true;
is_base_char(X) when X >= 16#0180, X =< 16#01C3 -> true;
is_base_char(X) when X >= 16#01CD, X =< 16#01F0 -> true;
is_base_char(X) when X >= 16#01F4, X =< 16#01F5 -> true;
is_base_char(X) when X >= 16#01FA, X =< 16#0217 -> true;
is_base_char(X) when X >= 16#0250, X =< 16#02A8 -> true;
is_base_char(X) when X >= 16#02BB, X =< 16#02C1 -> true;
is_base_char(16#0386) -> true;
is_base_char(X) when X >= 16#0388, X =< 16#038A -> true;
is_base_char(16#038C) -> true;
is_base_char(X) when X >= 16#038E, X =< 16#03A1 -> true;
is_base_char(X) when X >= 16#03A3, X =< 16#03CE -> true;
is_base_char(X) when X >= 16#03D0, X =< 16#03D6 -> true;
is_base_char(16#03DA) -> true;
is_base_char(16#03DC) -> true;
is_base_char(16#03DE) -> true;
is_base_char(16#03E0) -> true;
is_base_char(X) when X >= 16#03E2, X =< 16#03F3 -> true;
is_base_char(X) when X >= 16#0401, X =< 16#040C -> true;
is_base_char(X) when X >= 16#040E, X =< 16#044F -> true;
is_base_char(X) when X >= 16#0451, X =< 16#045C -> true;
is_base_char(X) when X >= 16#045E, X =< 16#0481 -> true;
is_base_char(X) when X >= 16#0490, X =< 16#04C4 -> true;
is_base_char(X) when X >= 16#04C7, X =< 16#04C8 -> true;
is_base_char(X) when X >= 16#04CB, X =< 16#04CC -> true;
is_base_char(X) when X >= 16#04D0, X =< 16#04EB -> true;
is_base_char(X) when X >= 16#04EE, X =< 16#04F5 -> true;
is_base_char(X) when X >= 16#04F8, X =< 16#04F9 -> true;
is_base_char(X) when X >= 16#0531, X =< 16#0556 -> true;
is_base_char(16#0559) -> true;
is_base_char(X) when X >= 16#0561, X =< 16#0586 -> true;
is_base_char(X) when X >= 16#05D0, X =< 16#05EA -> true;
is_base_char(X) when X >= 16#05F0, X =< 16#05F2 -> true;
is_base_char(X) when X >= 16#0621, X =< 16#063A -> true;
is_base_char(X) when X >= 16#0641, X =< 16#064A -> true;
is_base_char(X) when X >= 16#0671, X =< 16#06B7 -> true;
is_base_char(X) when X >= 16#06BA, X =< 16#06BE -> true;
is_base_char(X) when X >= 16#06C0, X =< 16#06CE -> true;
is_base_char(X) when X >= 16#06D0, X =< 16#06D3 -> true;
is_base_char(16#06D5) -> true;
is_base_char(X) when X >= 16#06E5, X =< 16#06E6 -> true;
is_base_char(X) when X >= 16#0905, X =< 16#0939 -> true;
is_base_char(16#093D) -> true;
is_base_char(X) when X >= 16#0958, X =< 16#0961 -> true;
is_base_char(X) when X >= 16#0985, X =< 16#098C -> true;
is_base_char(X) when X >= 16#098F, X =< 16#0990 -> true;
is_base_char(X) when X >= 16#0993, X =< 16#09A8 -> true;
is_base_char(X) when X >= 16#09AA, X =< 16#09B0 -> true;
is_base_char(16#09B2) -> true;
is_base_char(X) when X >= 16#09B6, X =< 16#09B9 -> true;
is_base_char(X) when X >= 16#09DC, X =< 16#09DD -> true;
is_base_char(X) when X >= 16#09DF, X =< 16#09E1 -> true;
is_base_char(X) when X >= 16#09F0, X =< 16#09F1 -> true;
is_base_char(X) when X >= 16#0A05, X =< 16#0A0A -> true;
is_base_char(X) when X >= 16#0A0F, X =< 16#0A10 -> true;
is_base_char(X) when X >= 16#0A13, X =< 16#0A28 -> true;
is_base_char(X) when X >= 16#0A2A, X =< 16#0A30 -> true;
is_base_char(X) when X >= 16#0A32, X =< 16#0A33 -> true;
is_base_char(X) when X >= 16#0A35, X =< 16#0A36 -> true;
is_base_char(X) when X >= 16#0A38, X =< 16#0A39 -> true;
is_base_char(X) when X >= 16#0A59, X =< 16#0A5C -> true;
is_base_char(16#0A5E) -> true;
is_base_char(X) when X >= 16#0A72, X =< 16#0A74 -> true;
is_base_char(X) when X >= 16#0A85, X =< 16#0A8B -> true;
is_base_char(16#0A8D) -> true;
is_base_char(X) when X >= 16#0A8F, X =< 16#0A91 -> true;
is_base_char(X) when X >= 16#0A93, X =< 16#0AA8 -> true;
is_base_char(X) when X >= 16#0AAA, X =< 16#0AB0 -> true;
is_base_char(X) when X >= 16#0AB2, X =< 16#0AB3 -> true;
is_base_char(X) when X >= 16#0AB5, X =< 16#0AB9 -> true;
is_base_char(16#0ABD) -> true;
is_base_char(16#0AE0) -> true;
is_base_char(X) when X >= 16#0B05, X =< 16#0B0C -> true;
is_base_char(X) when X >= 16#0B0F, X =< 16#0B10 -> true;
is_base_char(X) when X >= 16#0B13, X =< 16#0B28 -> true;
is_base_char(X) when X >= 16#0B2A, X =< 16#0B30 -> true;
is_base_char(X) when X >= 16#0B32, X =< 16#0B33 -> true;
is_base_char(X) when X >= 16#0B36, X =< 16#0B39 -> true;
is_base_char(16#0B3D) -> true;
is_base_char(X) when X >= 16#0B5C, X =< 16#0B5D -> true;
is_base_char(X) when X >= 16#0B5F, X =< 16#0B61 -> true;
is_base_char(X) when X >= 16#0B85, X =< 16#0B8A -> true;
is_base_char(X) when X >= 16#0B8E, X =< 16#0B90 -> true;
is_base_char(X) when X >= 16#0B92, X =< 16#0B95 -> true;
is_base_char(X) when X >= 16#0B99, X =< 16#0B9A -> true;
is_base_char(16#0B9C) -> true;
is_base_char(X) when X >= 16#0B9E, X =< 16#0B9F -> true;
is_base_char(X) when X >= 16#0BA3, X =< 16#0BA4 -> true;
is_base_char(X) when X >= 16#0BA8, X =< 16#0BAA -> true;
is_base_char(X) when X >= 16#0BAE, X =< 16#0BB5 -> true;
is_base_char(X) when X >= 16#0BB7, X =< 16#0BB9 -> true;
is_base_char(X) when X >= 16#0C05, X =< 16#0C0C -> true;
is_base_char(X) when X >= 16#0C0E, X =< 16#0C10 -> true;
is_base_char(X) when X >= 16#0C12, X =< 16#0C28 -> true;
is_base_char(X) when X >= 16#0C2A, X =< 16#0C33 -> true;
is_base_char(X) when X >= 16#0C35, X =< 16#0C39 -> true;
is_base_char(X) when X >= 16#0C60, X =< 16#0C61 -> true;
is_base_char(X) when X >= 16#0C85, X =< 16#0C8C -> true;
is_base_char(X) when X >= 16#0C8E, X =< 16#0C90 -> true;
is_base_char(X) when X >= 16#0C92, X =< 16#0CA8 -> true;
is_base_char(X) when X >= 16#0CAA, X =< 16#0CB3 -> true;
is_base_char(X) when X >= 16#0CB5, X =< 16#0CB9 -> true;
is_base_char(16#0CDE) -> true;
is_base_char(X) when X >= 16#0CE0, X =< 16#0CE1 -> true;
is_base_char(X) when X >= 16#0D05, X =< 16#0D0C -> true;
is_base_char(X) when X >= 16#0D0E, X =< 16#0D10 -> true;
is_base_char(X) when X >= 16#0D12, X =< 16#0D28 -> true;
is_base_char(X) when X >= 16#0D2A, X =< 16#0D39 -> true;
is_base_char(X) when X >= 16#0D60, X =< 16#0D61 -> true;
is_base_char(X) when X >= 16#0E01, X =< 16#0E2E -> true;
is_base_char(16#0E30) -> true;
is_base_char(X) when X >= 16#0E32, X =< 16#0E33 -> true;
is_base_char(X) when X >= 16#0E40, X =< 16#0E45 -> true;
is_base_char(X) when X >= 16#0E81, X =< 16#0E82 -> true;
is_base_char(16#0E84) -> true;
is_base_char(X) when X >= 16#0E87, X =< 16#0E88 -> true;
is_base_char(16#0E8A) -> true;
is_base_char(16#0E8D) -> true;
is_base_char(X) when X >= 16#0E94, X =< 16#0E97 -> true;
is_base_char(X) when X >= 16#0E99, X =< 16#0E9F -> true;
is_base_char(X) when X >= 16#0EA1, X =< 16#0EA3 -> true;
is_base_char(16#0EA5) -> true;
is_base_char(16#0EA7) -> true;
is_base_char(X) when X >= 16#0EAA, X =< 16#0EAB -> true;
is_base_char(X) when X >= 16#0EAD, X =< 16#0EAE -> true;
is_base_char(16#0EB0) -> true;
is_base_char(X) when X >= 16#0EB2, X =< 16#0EB3 -> true;
is_base_char(16#0EBD) -> true;
is_base_char(X) when X >= 16#0EC0, X =< 16#0EC4 -> true;
is_base_char(X) when X >= 16#0F40, X =< 16#0F47 -> true;
is_base_char(X) when X >= 16#0F49, X =< 16#0F69 -> true;
is_base_char(X) when X >= 16#10A0, X =< 16#10C5 -> true;
is_base_char(X) when X >= 16#10D0, X =< 16#10F6 -> true;
is_base_char(16#1100) -> true;
is_base_char(X) when X >= 16#1102, X =< 16#1103 -> true;
is_base_char(X) when X >= 16#1105, X =< 16#1107 -> true;
is_base_char(16#1109) -> true;
is_base_char(X) when X >= 16#110B, X =< 16#110C -> true;
is_base_char(X) when X >= 16#110E, X =< 16#1112 -> true;
is_base_char(16#113C) -> true;
is_base_char(16#113E) -> true;
is_base_char(16#1140) -> true;
is_base_char(16#114C) -> true;
is_base_char(16#114E) -> true;
is_base_char(16#1150) -> true;
is_base_char(X) when X >= 16#1154, X =< 16#1155 -> true;
is_base_char(16#1159) -> true;
is_base_char(X) when X >= 16#115F, X =< 16#1161 -> true;
is_base_char(16#1163) -> true;
is_base_char(16#1165) -> true;
is_base_char(16#1167) -> true;
is_base_char(16#1169) -> true;
is_base_char(X) when X >= 16#116D, X =< 16#116E -> true;
is_base_char(X) when X >= 16#1172, X =< 16#1173 -> true;
is_base_char(16#1175) -> true;
is_base_char(16#119E) -> true;
is_base_char(16#11A8) -> true;
is_base_char(16#11AB) -> true;
is_base_char(X) when X >= 16#11AE, X =< 16#11AF -> true;
is_base_char(X) when X >= 16#11B7, X =< 16#11B8 -> true;
is_base_char(16#11BA) -> true;
is_base_char(X) when X >= 16#11BC, X =< 16#11C2 -> true;
is_base_char(16#11EB) -> true;
is_base_char(16#11F0) -> true;
is_base_char(16#11F9) -> true;
is_base_char(X) when X >= 16#1E00, X =< 16#1E9B -> true;
is_base_char(X) when X >= 16#1EA0, X =< 16#1EF9 -> true;
is_base_char(X) when X >= 16#1F00, X =< 16#1F15 -> true;
is_base_char(X) when X >= 16#1F18, X =< 16#1F1D -> true;
is_base_char(X) when X >= 16#1F20, X =< 16#1F45 -> true;
is_base_char(X) when X >= 16#1F48, X =< 16#1F4D -> true;
is_base_char(X) when X >= 16#1F50, X =< 16#1F57 -> true;
is_base_char(16#1F59) -> true;
is_base_char(16#1F5B) -> true;
is_base_char(16#1F5D) -> true;
is_base_char(X) when X >= 16#1F5F, X =< 16#1F7D -> true;
is_base_char(X) when X >= 16#1F80, X =< 16#1FB4 -> true;
is_base_char(X) when X >= 16#1FB6, X =< 16#1FBC -> true;
is_base_char(16#1FBE) -> true;
is_base_char(X) when X >= 16#1FC2, X =< 16#1FC4 -> true;
is_base_char(X) when X >= 16#1FC6, X =< 16#1FCC -> true;
is_base_char(X) when X >= 16#1FD0, X =< 16#1FD3 -> true;
is_base_char(X) when X >= 16#1FD6, X =< 16#1FDB -> true;
is_base_char(X) when X >= 16#1FE0, X =< 16#1FEC -> true;
is_base_char(X) when X >= 16#1FF2, X =< 16#1FF4 -> true;
is_base_char(X) when X >= 16#1FF6, X =< 16#1FFC -> true;
is_base_char(16#2126) -> true;
is_base_char(X) when X >= 16#212A, X =< 16#212B -> true;
is_base_char(16#212E) -> true;
is_base_char(X) when X >= 16#2180, X =< 16#2182 -> true;
is_base_char(X) when X >= 16#3041, X =< 16#3094 -> true;
is_base_char(X) when X >= 16#30A1, X =< 16#30FA -> true;
is_base_char(X) when X >= 16#3105, X =< 16#312C -> true;
is_base_char(X) when X >= 16#ac00, X =< 16#d7a3 -> true;
is_base_char(_) ->
    false.

%% [86] Ideographic
is_ideographic(X) when X >= 16#4e00, X =< 16#9fa5 -> true;
is_ideographic(16#3007) -> true;
is_ideographic(X) when X >= 16#3021, X =< 16#3029 -> true;
is_ideographic(_) ->
    false.

%% [87] CombiningChar
is_combining_char(X) when X >= 16#0300, X =< 16#0345 -> true;
is_combining_char(X) when X >= 16#0360, X =< 16#0361 -> true;
is_combining_char(X) when X >= 16#0483, X =< 16#0486 -> true;
is_combining_char(X) when X >= 16#0591, X =< 16#05a1 -> true;
is_combining_char(X) when X >= 16#05a3, X =< 16#05b9 -> true;
is_combining_char(X) when X >= 16#05bb, X =< 16#05bd -> true;
is_combining_char(16#05bf) -> true;
is_combining_char(X) when X >= 16#05c1, X =< 16#05c2 -> true;
is_combining_char(16#05c4) -> true;
is_combining_char(X) when X >= 16#064b, X =< 16#0652 -> true;
is_combining_char(16#0670) -> true;
is_combining_char(X) when X >= 16#06d6, X =< 16#06dc -> true;
is_combining_char(X) when X >= 16#06dd, X =< 16#06df -> true;
is_combining_char(X) when X >= 16#06e0, X =< 16#06e4 -> true;
is_combining_char(X) when X >= 16#06e7, X =< 16#06e8 -> true;
is_combining_char(X) when X >= 16#06ea, X =< 16#06ed -> true;
is_combining_char(X) when X >= 16#0901, X =< 16#0903 -> true;
is_combining_char(16#093c) -> true;
is_combining_char(X) when X >= 16#093e, X =< 16#094c -> true;
is_combining_char(16#094d) -> true;
is_combining_char(X) when X >= 16#0951, X =< 16#0954 -> true;
is_combining_char(X) when X >= 16#0962, X =< 16#0963 -> true;
is_combining_char(X) when X >= 16#0981, X =< 16#0983 -> true;
is_combining_char(16#09bc) -> true;
is_combining_char(16#09be) -> true;
is_combining_char(16#09bf) -> true;
is_combining_char(X) when X >= 16#09c0, X =< 16#09c4 -> true;
is_combining_char(X) when X >= 16#09c7, X =< 16#09c8 -> true;
is_combining_char(X) when X >= 16#09cb, X =< 16#09cd -> true;
is_combining_char(16#09d7) -> true;
is_combining_char(X) when X >= 16#09e2, X =< 16#09e3 -> true;
is_combining_char(16#0a02) -> true;
is_combining_char(16#0a3c) -> true;
is_combining_char(16#0a3e) -> true;
is_combining_char(16#0a3f) -> true;
is_combining_char(X) when X >= 16#0a40, X =< 16#0a42 -> true;
is_combining_char(X) when X >= 16#0a47, X =< 16#0a48 -> true;
is_combining_char(X) when X >= 16#0a4b, X =< 16#0a4d -> true;
is_combining_char(X) when X >= 16#0a70, X =< 16#0a71 -> true;
is_combining_char(X) when X >= 16#0a81, X =< 16#0a83 -> true;
is_combining_char(16#0abc) -> true;
is_combining_char(X) when X >= 16#0abe, X =< 16#0ac5 -> true;
is_combining_char(X) when X >= 16#0ac7, X =< 16#0ac9 -> true;
is_combining_char(X) when X >= 16#0acb, X =< 16#0acd -> true;
is_combining_char(X) when X >= 16#0b01, X =< 16#0b03 -> true;
is_combining_char(16#0b3c) -> true;
is_combining_char(X) when X >= 16#0b3e, X =< 16#0b43 -> true;
is_combining_char(X) when X >= 16#0b47, X =< 16#0b48 -> true;
is_combining_char(X) when X >= 16#0b4b, X =< 16#0b4d -> true;
is_combining_char(X) when X >= 16#0b56, X =< 16#0b57 -> true;
is_combining_char(X) when X >= 16#0b82, X =< 16#0b83 -> true;
is_combining_char(X) when X >= 16#0bbe, X =< 16#0bc2 -> true;
is_combining_char(X) when X >= 16#0bc6, X =< 16#0bc8 -> true;
is_combining_char(X) when X >= 16#0bca, X =< 16#0bcd -> true;
is_combining_char(16#0bd7) -> true;
is_combining_char(X) when X >= 16#0c01, X =< 16#0c03 -> true;
is_combining_char(X) when X >= 16#0c3e, X =< 16#0c44 -> true;
is_combining_char(X) when X >= 16#0c46, X =< 16#0c48 -> true;
is_combining_char(X) when X >= 16#0c4a, X =< 16#0c4d -> true;
is_combining_char(X) when X >= 16#0c55, X =< 16#0c56 -> true;
is_combining_char(X) when X >= 16#0c82, X =< 16#0c83 -> true;
is_combining_char(X) when X >= 16#0cbe, X =< 16#0cc4 -> true;
is_combining_char(X) when X >= 16#0cc6, X =< 16#0cc8 -> true;
is_combining_char(X) when X >= 16#0cca, X =< 16#0ccd -> true;
is_combining_char(X) when X >= 16#0cd5, X =< 16#0cd6 -> true;
is_combining_char(X) when X >= 16#0d02, X =< 16#0d03 -> true;
is_combining_char(X) when X >= 16#0d3e, X =< 16#0d43 -> true;
is_combining_char(X) when X >= 16#0d46, X =< 16#0d48 -> true;
is_combining_char(X) when X >= 16#0d4a, X =< 16#0d4d -> true;
is_combining_char(16#0d57) -> true;
is_combining_char(16#0e31) -> true;
is_combining_char(X) when X >= 16#0e34, X =< 16#0e3a -> true;
is_combining_char(X) when X >= 16#0e47, X =< 16#0e4e -> true;
is_combining_char(16#0eb1) -> true;
is_combining_char(X) when X >= 16#0eb4, X =< 16#0eb9 -> true;
is_combining_char(X) when X >= 16#0ebb, X =< 16#0ebc -> true;
is_combining_char(X) when X >= 16#0ec8, X =< 16#0ecd -> true;
is_combining_char(X) when X >= 16#0f18, X =< 16#0f19 -> true;
is_combining_char(16#0f35) -> true;
is_combining_char(16#0f37) -> true;
is_combining_char(16#0f39) -> true;
is_combining_char(16#0f3e) -> true;
is_combining_char(16#0f3f) -> true;
is_combining_char(X) when X >= 16#0f71, X =< 16#0f84 -> true;
is_combining_char(X) when X >= 16#0f86, X =< 16#0f8b -> true;
is_combining_char(X) when X >= 16#0f90, X =< 16#0f95 -> true;
is_combining_char(16#0f97) -> true;
is_combining_char(X) when X >= 16#0f99, X =< 16#0fad -> true;
is_combining_char(X) when X >= 16#0fb1, X =< 16#0fb7 -> true;
is_combining_char(16#0fb9) -> true;
is_combining_char(X) when X >= 16#20d0, X =< 16#20dc -> true;
is_combining_char(16#20e1) -> true;
is_combining_char(X) when X >= 16#302a, X =< 16#302f -> true;
is_combining_char(16#3099) -> true;
is_combining_char(16#309a) -> true;
is_combining_char(_) -> false.

%% [88] Digit
is_digit(X) when X >= 16#0030, X =< 16#0039 -> true;
is_digit(X) when X >= 16#0660, X =< 16#0669 -> true;
is_digit(X) when X >= 16#06F0, X =< 16#06F9 -> true;
is_digit(X) when X >= 16#0966, X =< 16#096f -> true;
is_digit(X) when X >= 16#09e6, X =< 16#09ef -> true;
is_digit(X) when X >= 16#0a66, X =< 16#0a6f -> true;
is_digit(X) when X >= 16#0ae6, X =< 16#0aef -> true;
is_digit(X) when X >= 16#0b66, X =< 16#0b6f -> true;
is_digit(X) when X >= 16#0be7, X =< 16#0bef -> true;
is_digit(X) when X >= 16#0c66, X =< 16#0c6f -> true;
is_digit(X) when X >= 16#0ce6, X =< 16#0cef -> true;
is_digit(X) when X >= 16#0d66, X =< 16#0d6f -> true;
is_digit(X) when X >= 16#0e50, X =< 16#0e59 -> true;
is_digit(X) when X >= 16#0ed0, X =< 16#0ed9 -> true;
is_digit(X) when X >= 16#0f20, X =< 16#0f29 -> true;
is_digit(_) -> false.

%% [89] Extender
is_extender(16#00b7) -> true;
is_extender(16#02d0) -> true;
is_extender(16#02d1) -> true;
is_extender(16#0387) -> true;
is_extender(16#0640) -> true;
is_extender(16#0e46) -> true;
is_extender(16#0ec6) -> true;
is_extender(16#3005) -> true;
is_extender(X) when X >= 16#3031, X =< 16#3035 -> true;
is_extender(X) when X >= 16#309d, X =< 16#309e -> true;
is_extender(X) when X >= 16#30fc, X =< 16#30fe -> true;
is_extender(_) -> false.
