-module(yanger_depend).
-behaviour(yanger_plugin).

-import(lists,[merge/1]).
-import(lists,[usort/1]).
-export([init/1]).

-include_lib("yanger/include/yang.hrl").

init(Ctx0) ->
    Ctx1 = yanger_plugin:register_output_format(Ctx0,
                                                depend,
                                                _AllowErrors = false,
                                                fun emit/3),
    yanger_plugin:register_option_specs(Ctx1, option_specs()).

option_specs() ->
    [{"Depend output specific options:",
      [{depend_target, undefined, "depend-target", string,
        "Makefile rule target"},
       {depend_no_submodules, undefined, "depend-no-submodules", boolean,
        "Do not generate dependencies for included submodules"},
       {depend_recurse, undefined, "depend-recurse", boolean,
        "Generate dependencies to all imports recursively"},
       {depend_extension, undefined, "depend-extension", string,
        "YANG module file name extension"},
       {depend_include_path, undefined, "depend-include-path", boolean,
        "Include file path in the prerequisites"},
       {depend_double_colon, undefined, "depend-double-colon", boolean,
        "Use double colon (::) for the rule"},
       {depend_ignore, undefined, "depend-ignore-module", string,
        "(sub)module to ignore in the prerequisites."
        " This option can be given multiple times."}]
     }].

-spec emit(#yctx{}, [#module{}], io:device()) -> [].
emit(Ctx, Mods, Fd) ->
    emit_mods(Ctx, Ctx#yctx.options, Mods, Fd),
    [].

emit_mods(Ctx, Opts, [M | T], Fd) ->
    DepTgt = proplists:get_value(depend_target, Opts, yang:get_filename(M)),
    ColonStr =
        case proplists:get_value(depend_double_colon, Opts, false) of
            true -> "::";
            false -> ":"
        end,
    PreReqs = get_prereqs(Ctx, Opts, M, []),
    io:format(Fd, "~s ~s", [DepTgt, ColonStr]),
    emit_prereqs(Ctx, Opts, PreReqs, Fd),
    emit_mods(Ctx, Opts, T, Fd);
emit_mods(_Ctx, _Opts, [], _Fd) ->
    ok.

emit_prereqs(Ctx, Opts, [PreReq | T], Fd) ->
    PreReqStr = ?a2l(PreReq),
    case lists:member({depend_ignore, PreReqStr}, Opts) of
        true ->
            ok;
        false ->
            PreReqStr1 =
                case proplists:get_value(depend_include_path, Opts, false) of
                    true ->
                        {value, M} = yang:get_module(PreReq, undefined, Ctx),
                        FName0 = yang:get_filename(M),
                        {ok, Cwd} = file:get_cwd(),
                        FName =
                            case filelib:safe_relative_path(FName0, Cwd) of
                                unsafe -> FName0;
                                FName1 -> FName1
                            end,
                        case proplists:get_value(depend_extension, Opts) of
                            undefined ->
                                FName;
                            Ext ->
                                filename:rootname(FName) ++ Ext
                        end;
                    false ->
                        case proplists:get_value(depend_extension, Opts) of
                            undefined ->
                                PreReq;
                            Ext ->
                                PreReqStr ++ Ext
                        end
                end,
            io:format(Fd, " ~s", [PreReqStr1])
    end,
    emit_prereqs(Ctx, Opts, T, Fd);
emit_prereqs(_Ctx, _Opts, [], Fd) ->
    io:format(Fd, "~n", []).

get_prereqs(Ctx, Opts, #module{stmt = Stmt}, PreReqs) ->
    Imports = yang:search_all_stmts('import', Stmt),
    Includes =
        case proplists:get_value(depend_no_submodules, Opts, false) of
            true ->
                [];
            false ->
                yang:search_all_stmts('include', Stmt)
        end,
    NewPreReqs = [ T || T <- [yang:stmt_arg(S) || S <- Imports ++ Includes],
                        lists:member(T, PreReqs) == false],
    case proplists:get_value(depend_recurse, Opts, false) of
        true ->
            Models = [ yang:get_module(S, undefined, Ctx) || S <- NewPreReqs ],
            usort(PreReqs ++
                      merge([get_prereqs(Ctx, Opts, M, PreReqs ++ NewPreReqs) ||
                                {value, M}  <- Models]));
        false ->
            PreReqs ++ NewPreReqs
    end.
