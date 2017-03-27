-module(yang_if_feature).
-export([parse/1,
         resolve_prefixes/4,
         eval/2,
         get_features/1]).

-export([map_expr/2, fold_expr/3]).
-export([expr2str/1]).

-export_type([expr/0]).

-include_lib("yanger/include/yang.hrl").

-type expr() :: {'or', expr(), expr()}
              | {'and', expr(), expr()}
              | {'not', expr()}
              | {identifier, yang:yang_identifier()}.

%%   if-feature-expr     = "(" if-feature-expr ")" /
%%                         if-feature-expr sep boolean-operator sep
%%                           if-feature-expr /
%%                         not-keyword sep if-feature-expr /
%%                         identifier-ref-arg
%%
%% Can be rewritten as:
%%   x = y ("and"/"or" y)*
%%   y = "not" x /
%%       "(" x ")"
%%       identifier
%%
%%   expr    = term "or" expr
%%   term    = factor "and" term
%%   factor  = "not" factor /
%%             "(" expr ")" /
%%             identifier
%%
%% NOTE: the if-feature syntax is context sensitive - the following
%%       is ok:  "or or and" - meaning the feature 'or' || the feature 'and'
%%
%% Recursive descent parser follows.

-spec parse(list()) -> {true, expr()} | false.
parse(Str0) ->
    try
        {Expr, Str1} = expr(Str0),
        eof = get_tok(Str1),
        {true, Expr}
    catch
        _X:_Y ->
            false
    end.

expr(Str0) ->
    {TermL, Str1} = term(Str0),
    case get_tok(Str1) of
        {'or', Str2} ->
            {TermR, Str3} = expr(Str2),
            {{'or', TermL, TermR}, Str3};
        _ ->
            {TermL, Str1}
    end.

term(Str0) ->
    {FactL, Str1} = factor(Str0),
    case get_tok(Str1) of
        {'and', Str2} ->
            {FactR, Str3} = term(Str2),
            {{'and', FactL, FactR}, Str3};
        _ ->
            {FactL, Str1}
    end.

factor(Str0) ->
    case get_tok(Str0) of
        {'not', Str1} when Str1 /= [] ->
            {Fact, Str2} = factor(Str1),
            {{'not', Fact}, Str2};
        {'(', Str1} ->
            {Expr, Str2} = expr(Str1),
            {')', Str3} = get_tok(Str2),
            {Expr, Str3};
        {Identifier, Str1} ->
            {{identifier, Identifier}, Str1}
    end.


-define(is_ws(X), (X == $\s orelse X == $\t orelse
                   X == $\n orelse X == $\r)).
-define(is_sep(X), (?is_ws(X) orelse (X) == $( orelse (X) == $))).
-define(is_identifier1(X), ((X >= $a andalso X =< $z)
                            orelse (X >= $A andalso X =< $Z)
                            orelse (X == $_))).
-define(is_identifier2(X), ((X >= $a andalso X =< $z)
                            orelse (X >= $A andalso X =< $Z)
                            orelse (X >= $0 andalso X =< $9)
                            orelse (X == $_)
                            orelse (X == $.)
                            orelse (X == $-))).

get_tok(S0) ->
    S1 = skip_ws(S0),
    case S1 of
        [$( | S2] ->
            {'(', S2};
        [$) | S2] ->
            {')', S2};
        "or" ++ [H | T] when ?is_sep(H) ->
            {'or', [H | T]};
        "and" ++ [H | T] when ?is_sep(H) ->
            {'and', [H | T]};
        "not" ++ [H | T] when ?is_sep(H) ->
            {'not', [H | T]};
        [H | S2] when ?is_identifier1(H) ->
            get_identifier(S2, [H], undefined);
        [] ->
            'eof'
    end.

skip_ws([H | T]) when ?is_ws(H) ->
    skip_ws(T);
skip_ws(S) ->
    S.

get_identifier([H | T], Acc, Pre) when ?is_identifier2(H) ->
    get_identifier(T, [H | Acc], Pre);
get_identifier([H | _] = S, Acc, Pre) when ?is_sep(H) ->
    return_identifier(S, Acc, Pre);
get_identifier([$: | T], Acc, undefined) ->
    Pre = list_to_atom(lists:reverse(Acc)),
    get_identifier(T, [], Pre);
get_identifier([], Acc, Pre) ->
    return_identifier([], Acc, Pre).

return_identifier(S, Acc, Pre) ->
    New = list_to_atom(lists:reverse(Acc)),
    case Pre of
        undefined ->
            {New, S};
        _ ->
            {{Pre, New}, S}
    end.

-spec resolve_prefixes(expr(), yang:pos(), #module{}, #yctx{}) ->
          {undefined | expr(), #yctx{}}.
resolve_prefixes(Expr, Pos, M, Ctx0) ->
    case Expr of
        {Op, ExprL, ExprR} ->
            {NExprL, Ctx1} = resolve_prefixes(ExprL, Pos, M, Ctx0),
            {NExprR, Ctx2} = resolve_prefixes(ExprR, Pos, M, Ctx1),
            case {NExprL, NExprR} of
                {undefined, _} ->
                    {undefined, Ctx2};
                {_, undefined} ->
                    {undefined, Ctx2};
                _ ->
                    {{Op, NExprL, NExprR}, Ctx2}
            end;
        {'not', ExprR} ->
            case resolve_prefixes(ExprR, Pos, M, Ctx0) of
                {undefined, Ctx1} ->
                    {undefined, Ctx1};
                {NExprR, Ctx1} ->
                    {{'not', NExprR}, Ctx1}
            end;
        {identifier, IdRef} ->
            case yang:resolve_raw_idref(IdRef, Pos, M, Ctx0) of
                {self, Name, Ctx1} ->
                    {{identifier, {M#module.modulename, Name}}, Ctx1};
                {{imported, #module{modulename = ModuleName}}, Name, Ctx1} ->
                    {{identifier, {ModuleName, Name}}, Ctx1};
                {undefined, _, Ctx1} ->
                    {undefined, Ctx1}
            end
    end.

-spec eval(expr(),
           none | yang:map(ModName :: atom(), [FeatureName :: atom()])) ->
          boolean().
eval(_, none) ->
    false;
eval(Expr, FMap) ->
    case Expr of
        {'or', ExprL, ExprR} ->
            eval(ExprL, FMap) orelse eval(ExprR, FMap);
        {'and', ExprL, ExprR} ->
            eval(ExprL, FMap) andalso eval(ExprR, FMap);
        {'not', ExprR} ->
            not(eval(ExprR, FMap));
        {identifier, {ModuleName, Name}} ->
            case yang:map_lookup(ModuleName, FMap) of
                {value, GivenFeatures} ->
                    lists:member(Name, GivenFeatures);
                none ->
                    true
            end
    end.

get_features(Expr) ->
    lists:usort(get_features0(Expr)).

get_features0(Expr) ->
    case Expr of
        {_Op, ExprL, ExprR} ->
            get_features0(ExprL) ++ get_features0(ExprR);
        {'not', ExprR} ->
            get_features0(ExprR);
        {identifier, Identifier} ->
            [Identifier]
    end.

-spec map_expr(fun((expr()) -> expr()), expr()) -> expr().
map_expr(F, Expr) ->
    case Expr of
        {Op, E1, E2} ->
            F({Op, map_expr(F, E1), map_expr(F, E2)});
        {'not', E1} ->
            F({'not', map_expr(F, E1)});
        {identifier, _} ->
            F(Expr)
    end.

-spec fold_expr(fun((expr(), term()) -> term()), term(), expr()) -> term().
fold_expr(F, Acc, Expr) ->
    case Expr of
        {_Op, E1, E2} ->
            F(Expr, fold_expr(F, fold_expr(F, Acc, E1), E2));
        {'not', E1} ->
            F(Expr, fold_expr(F, Acc, E1));
        {identifier, _} ->
            F(Expr, Acc)
    end.

-spec expr2str(expr()) -> iolist().
expr2str(Expr) ->
    case Expr of
        {Op, E1, E2} ->
            [$(, expr2str(E1), $\s, ?a2l(Op), $\s, expr2str(E2), $)];
        {'not', E1} ->
            ["not ", expr2str(E1)];
        {identifier, Id} ->
            case Id of
                {Pre, Name} ->
                    [?a2l(Pre), $:, ?a2l(Name)];
                Name ->
                    ?a2l(Name)
            end
    end.
