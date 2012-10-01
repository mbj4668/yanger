-module(yang_error).
-export([fmt_error/1]).

-include("yang.hrl").

-define(ilf, io_lib:format).

fmt_error(#yerror{level = Level,
                  pos = Pos,
                  code = Code,
                  args = Args}) ->
    ?ilf("~s: ~w: ~s", [fmt_pos(Pos), Level, fmt_code(Code, Args)]).

fmt_pos({Filename, Line}) ->
    [Filename, $:, ?i2l(Line)];
fmt_pos({Filename, Line, Col}) ->
    [Filename, $:, ?i2l(Line), $:, ?i2l(Col)].

fmt_yang_identifier({ModuleName, Name}) ->
    ?ilf("'~s' in module '~s'", [Name, ModuleName]);
fmt_yang_identifier(Name) ->
    ?ilf("'~s'", [Name]).

fmt_code(Code, Args) ->
    case Code of
        'YANG_ERR_CIRCULAR_DEPENDENCY' ->
            ?ilf("circular dependency for ~s '~s'", Args);
        'YANG_ERR_MODULE_NOT_FOUND' ->
            case Args of
                [ModuleName, undefined] ->
                    ?ilf("module '~s' not found", [ModuleName]);
                _ ->
                    ?ilf("module '~s', revision '~p' not found", Args)
            end;
        'YANG_ERR_MODULENAME_MISMATCH' ->
            ?ilf("expected module name '~s', got '~s'", Args);
        'YANG_ERR_REVISION_MISMATCH' ->
            ?ilf("expected revision '~s', got '~s'", Args);
        'YANG_ERR_BAD_INCLUDE' ->
            [SubPos] = Args,
            ?ilf("expected submodule, found module at ~s", [fmt_pos(SubPos)]);
        'YANG_ERR_BAD_BELONGS_TO' ->
            ?ilf("'~s' includes '~s', but it does not specifiy"
                 " a correct belongs-to", Args);
        'YANG_ERR_DUPLICATE_DEFINITION' ->
            [Keyword, Name, OtherPos] = Args,
            ?ilf("~s '~s' already defined at ~s",
                 [Keyword, Name, fmt_pos(OtherPos)]);
        'YANG_ERR_MISSING_INCLUDE' ->
            ?ilf("submodule '~s' not included by module '~s'", Args);
        'YANG_ERR_DEFINITION_NOT_FOUND' ->
            ?ilf("~s '~s' not found", Args);
        'YANG_ERR_PREFIX_NOT_FOUND' ->
            ?ilf("prefix '~s' not found", Args);
        'YANG_ERR_REVISION_ORDER' ->
            ?ilf("revision statements not given in reverse"
                 " chronological order", Args);
        'YANG_ERR_INVALID_CONFIG' ->
            "config true cannot be set when the parent is config false";
        'YANG_ERR_INVALID_CONFIG_USES' ->
            [OtherPos] = Args,
            ?ilf("config false uses cannot use grouping with config true node "
                 "defined at ~s", [fmt_pos(OtherPos)]);
        'YANG_ERR_REFINE_NOT_FOUND' ->
            [Id] = Args,
            ?ilf("the refinement node ~s is not found",
                 [fmt_yang_identifier(Id)]);
        'YANG_ERR_ILLEGAL_REFINE' ->
            ?ilf("the statement '~s' cannot be refined in a '~s'", Args);
        'YANG_ERR_BAD_REFINE' ->
            ?ilf("the statement '~s' cannot be refined", Args);
        'YANG_ERR_AUGMENT_NOT_FOUND' ->
            [Id] = Args,
            ?ilf("the augment target node ~s is not found",
                 [fmt_yang_identifier(Id)]);
        'YANG_ERR_BAD_AUGMENT_NODE_TYPE' ->
            [Kind, Id] = Args,
            ?ilf("cannot augment ~s node ~s ",
                 [Kind, fmt_yang_identifier(Id)]);
        'YANG_ERR_NODE_NOT_FOUND' ->
            [Id] = Args,
            ?ilf("node ~s not found", [fmt_yang_identifier(Id)]);
        _ ->
            case Args of
                {str, Str} ->
                    Str;
                _ ->
                    ?ilf("unknown error: ~p", [Code])
            end
    end.
