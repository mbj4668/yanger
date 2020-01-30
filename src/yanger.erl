%%%-------------------------------------------------------------------
%%% Front-end module to yanger.
%%  Called from the 'yanger' escript.
%%%-------------------------------------------------------------------
-module(yanger).

-export([main/1, main/2]).
-export([vsn/0]).
-export([create_ctx/1, convert_options/2, run/3]).
-export([print_error/1, print_crash/2]).

-export_type([error/0, opts/0]).

-include_lib("yanger/include/yang.hrl").

-record(opts, {
          canonical = false,
          strict = false,
          print_error_code = false,
          path = [],
          warnings = [],
          no_warnings = [],
          errors = [],
          emit :: 'undefined' | yanger_plugin:emit_fun(),
          emit_allow_errors = false :: boolean(),
          transform = [] :: [yanger_plugin:transform_fun()],
          deviations = [],
          features = [],
          conformances = [],
          outfile,
          no_deviation_apply = false,
          max_status :: 'undefined' | yang:yang_status(),
          debug_print = false,
          ignore_unknown_features = false
         }).

-type error() :: {bad_format, string()}
               | {bad_transform, string()}
               | {bad_features, string()}
               | errors_printed
               | {file_error, FileName :: string(), FileError :: term()}
               | {fmt, Fmt :: string, Args :: list()}
               | format_no_modules
               .
-opaque opts() :: #opts{}.

vsn() ->
    ?VSN.

-spec main(list(), list(list())) -> any().
%% escript front-end's entry point.
%% Interactive use (e.g. for debugger):
%%  yanger:main("yanger", ["foo.yang", "-f", "tree"])
main(Name, Args) ->
    case lists:member("--debug-internal", Args) of
        true ->
            hidden_apply(debugger,
                         quick,
                         [?MODULE, main, [Name, Args -- ["--debug-internal"]]]);
        false ->
            case do_main(Name, Args) of
                ok ->
                    ok;
                error ->
                    delayed_halt(1)
            end
    end.

%% NOTE: Hack to skip xref error...
hidden_apply(M, F, A) ->
    apply(fun() -> M end(), F, A).

-spec main(list(list())) -> ok | error.
%% Like main/2 but with fixed name and no halt
main(Args) ->
    do_main("yanger", Args).

-spec do_main(list(), list(list())) -> ok | error.
do_main(Name, Args) ->
    %% Peek into the command line and load the plugin code
    PluginPath = get_plugin_path(Args),
    Ctx0 = create_ctx(PluginPath),

    %% Parse the command line
    case getopt:parse(Ctx0#yctx.option_specs, Args) of
        {ok, {Options, Files}} ->
            chk_interactive_options(Name, Options, Ctx0),
            try
                {Ctx1, Opts} = convert_options(Ctx0, Options),
                run(Ctx1, Opts, Files),
                ok
            catch
                throw:{error, Error} ->
                    print_error(Error),
                    error;
                ?CATCH_STACKTRACE(_, X, Stacktrace)
                    print_crash(X, Stacktrace),
                    error
            end;
        {error, {Reason, Data}} ->
            io:format(standard_error, "error: ~p ~s\n", [Reason, Data]),
            print_usage(Name, Ctx0),
            error
    end.

-spec create_ctx([string()]) -> #yctx{}.
%% Non-interactive entry point.
%% Must call convert_options() after this call.
create_ctx(PluginPath) ->
    Plugins = yang:load_plugins(PluginPath),
    %% Create a new context and initialize the plugins.
    %% This is done before command line parsing, in order to
    %% let the plugins register options and output formats.
    Ctx0 = yang:new_ctx(Plugins),
    Ctx1 = Ctx0#yctx{option_specs = option_specs(Ctx0) ++
                         Ctx0#yctx.option_specs},
    Ctx1.

-spec convert_options(#yctx{}, [getopt:option()]) -> {#yctx{}, opts()}.
%% Non-interactive function to run the compiler.
%% Must call run() after this call.
%% throws: {error, error()}.
convert_options(Ctx1, Options) ->
    Ctx2 = Ctx1#yctx{options = Options},
    Opts = opts(Options, Ctx2),
    %% Figure out what to do.
    WarningAsError = lists:member(error, Opts#opts.warnings),
    NoPrintWarnings = lists:member(none, Opts#opts.warnings),
    Ctx3 = Ctx2#yctx{warnings = {WarningAsError,
                                 NoPrintWarnings,
                                 Opts#opts.no_warnings,
                                 Opts#opts.warnings,
                                 Opts#opts.errors}},
    Ctx4 = Ctx3#yctx{canonical = Opts#opts.canonical,
                     strict = Opts#opts.strict,
                     conformances = Opts#opts.conformances,
                     apply_deviations = not Opts#opts.no_deviation_apply},
    Ctx5 = yang:init_ctx(Ctx4, Opts#opts.path),
    FeaturesMap =
        lists:foldl(
          fun({ModName, Features0}, Map) ->
                  Features =
                      case yang:map_lookup(ModName, Map) of
                          none ->
                              Features0;
                          {value, Features1} ->
                              Features0 ++ Features1
                      end,
                  yang:map_update(ModName, Features, Map)
          end, yang:map_new(), opts_features(Opts)),
    Ctx6 = Ctx5#yctx{features = FeaturesMap},
    {Ctx6, Opts}.

%% Note that inlining this call makes dialyzer (in OTP18) lose the
%% opaque information of the opts().  This does not happen in OTP19.
-spec opts_features(opts()) -> [any()].
opts_features(#opts{features = Fs}) -> Fs.

-spec option_specs(#yctx{}) -> [getopt:option_spec()].
option_specs(Ctx) ->
    [{help,             $h, "help", undefined,
      "Show this help message and exit."},
     {version,          $v, "version", undefined,
      "Show version number and exit."},

     {print_error_code, undefined, "print-error-code", undefined,
      "On errors, print the error code instead of the error message."},
     {path,             $p, "path", string,
      "colon-separated path for YANG modules."},
     {plugindir,        $P, "plugindir", string,
      "Load yanger plugins from PLUGINDIR."},
     {canonical,        $c, "canonical", undefined,
      "Validate the module(s) according to the canonical YANG order."},
     {strict,           undefined, "strict", undefined,
      "Force strict YANG compliance."},
     {list_errors,      $e, "list-errors", undefined,
      "Print a listing of all error and warning codes and exit."},
     {warning,          $W, "warning", atom,
      "If WARNING is 'error', treat all warnings as errors, "
      "except any listed WARNING. If WARNING is 'none', do "
      "not report any warnings."},
     {no_warning,       $w, "no-warning", atom,
      "Do not report a warning for WARNING. For a list of "
      "warnings, use --list-errors."},
     {error,            $E, "error", atom,
      "Treat each WARNING as an error.  For a list of "
      "warnings, use --list-errors."},
     {format,           $f, "format", atom,
      "Convert to FORMAT.  Supported formats are: " ++
          string:join([?a2l(F) || {F, _AE, _M} <- Ctx#yctx.fmts], ",") ++ "."},
     {transform,        $t, "transform", atom,
      "Run transformation TRANSFORM (more than one is allowed). "
      "Available transformations: " ++
          string:join([?a2l(N) || {N, _M} <- Ctx#yctx.transforms], ",") ++ "."},
     {max_status,       undefined, "max-status", atom,
      "Prune all nodes with status less than the given max status.  One of: "
      "current, deprecated, obsolete"},
     {deviation_module, undefined, "deviation-module", string,
      "Apply deviations from MODULE"},
     %% NOTE: If deviations and/or features are specified, the parsed tree
     %% will be modified.  This needs to be properly documented.
     {features,         $F, "features", string,
      "Features to support, default all."
      " Format: <modname>:[<feature>,]*"},
     {ignore_unknown_features, undefined, "ignore-unknown-features", boolean,
      "Use to suppress errors regarding unknown features"},

     {conformance,      $C, "conformance", string,
      "Conformance, default implement."
      " Format: [<modname>:]implement|import"},
     {outfile,          $o, "output", string,
      "Write the output to OUTFILE instead of stdout."},
     {no_deviation_apply, undefined, "no-deviation-apply", undefined,
      "Do not apply deviations."},

     {debug_print,      undefined, "print", undefined,
      "DEBUG - print internal format"},
     {debug_print_all,  undefined, "print-all", undefined,
      "DEBUG - print internal format"}
    ].

chk_interactive_options(Name, Options, Ctx) ->
    lists:foreach(
      fun(Opt) ->
              case Opt of
                  help ->
                      print_usage(Name, Ctx),
                      delayed_halt(0);
                  version ->
                      print_version(Name),
                      delayed_halt(0);
                  list_errors ->
                      yang_error:print_error_codes(Ctx),
                      delayed_halt(0);
                  _ ->
                      ok
              end
      end, Options).

-spec opts([getopt:option()], #yctx{}) -> opts().
opts(Options, Ctx) ->
    lists:foldl(
      fun(Opt, Opts) ->
              case Opt of
                  canonical ->
                      Opts#opts{canonical = true};
                  strict ->
                      Opts#opts{strict = true};
                  print_error_code ->
                      Opts#opts{print_error_code = true};
                  {path, P} ->
                      Opts#opts{path = Opts#opts.path ++ string:tokens(P, ":")};
                  {warning, W} ->
                      Opts#opts{warnings = [W | Opts#opts.warnings]};
                  {no_warning, W} ->
                      Opts#opts{no_warnings = [W | Opts#opts.no_warnings]};
                  {error, W} ->
                      Opts#opts{errors = [W | Opts#opts.errors]};
                  {format, F} ->
                      case lists:keyfind(F, 1, Ctx#yctx.fmts) of
                          {F, AllowErrors, EmitFun} ->
                              Opts#opts{emit = EmitFun,
                                        emit_allow_errors = AllowErrors};
                          false ->
                              throw({error, {bad_format, F}})
                      end;
                  {transform, T} ->
                      case lists:keyfind(T, 1, Ctx#yctx.transforms) of
                          {T, TransformFun} ->
                              Opts#opts{transform = Opts#opts.transform ++
                                            [TransformFun]};
                          false ->
                              throw({error, {bad_transform, T}})
                      end;
                  {max_status, MaxStatus} ->
                      %% implemented as a built-in transform which is run first
                      case MaxStatus of
                          current -> ok;
                          deprecated -> ok;
                          obsolete -> ok;
                          _ -> throw({error, {bad_status, MaxStatus}})
                      end,
                      Opts#opts{transform =
                                    [fun(Ctx1, M) ->
                                             t_max_status(Ctx1, M, MaxStatus)
                                     end | Opts#opts.transform]};
                  {deviation_module, Dev} ->
                      Opts#opts{deviations = [Dev | Opts#opts.deviations]};
                  {features, FStr} ->
                      case string:tokens(FStr, ":") of
                          [ModName] ->
                              Opts#opts{features = [{?l2a(ModName), []} |
                                                    Opts#opts.features]};
                          [ModName, Rest] ->
                              Features =
                                  [?l2a(F) || F <- string:tokens(Rest, ",")],
                              Opts#opts{features = [{?l2a(ModName), Features} |
                                                    Opts#opts.features]};
                          _ ->
                              throw({error, {bad_features, FStr}})
                      end;
                  {conformance, CStr} ->
                      case string:tokens(CStr, ":") of
                          ["import"] ->
                              Opts#opts{conformances =
                                            [import |
                                             Opts#opts.conformances]};
                          ["implement"] ->
                              Opts#opts{conformances =
                                            [implement |
                                             Opts#opts.conformances]};
                          [ModName, "import"] ->
                              Opts#opts{conformances =
                                            [{?l2a(ModName), import} |
                                             Opts#opts.conformances]};
                          [ModName, "implement"] ->
                              Opts#opts{conformances =
                                            [{?l2a(ModName), implement} |
                                             Opts#opts.conformances]};
                          _ ->
                              throw({error, {bad_conformance, CStr}})
                      end;
                  {outfile, F} ->
                      Opts#opts{outfile = F};
                  no_deviation_apply ->
                      Opts#opts{no_deviation_apply = true};
                  debug_print ->
                      Opts#opts{debug_print = true};
                  debug_print_all ->
                      Opts#opts{debug_print = all};
                  {ignore_unknown_features, true} ->
                      Opts#opts{ignore_unknown_features = true};
                  _ ->
                      Opts
              end
      end, #opts{}, Options).

-spec run(#yctx{}, opts(), [string()]) -> ok.
%% throws: {error, error()}.
run(Ctx0, Opts, Files) ->
    {Success0, Ctx1, ModRevs} =
        lists:foldl(
          fun(File, {Success1, Ctx00, ModRevs0}) ->
                  case yang:add_file(Ctx00, File) of
                      {true, Ctx01, #module{name = Name, revision = Rev}} ->
                          {Success1, Ctx01, [{Name, Rev} | ModRevs0]};
                      {false, Ctx01, _} ->
                          {false, Ctx01, ModRevs0}
                  end
          end, {true, Ctx0, []}, Files),
    {Success, Ctx2} =
        lists:foldl(
          fun(File, {Success1, Ctx00}) ->
                  case yang:add_file(Ctx00, File, _AddCause = deviation) of
                      {true, Ctx01, _} ->
                          {Success1, Ctx01};
                      {false, Ctx01, _} ->
                          {false, Ctx01}
                  end
          end, {Success0, Ctx1}, Opts#opts.deviations),

    Ctx3 = yang:post_add_modules(Ctx2),

    %% Get the complete (augmented) modules from the context)
    Modules0 = [yang:map_get(ModRev, Ctx3#yctx.modrevs) ||
                   ModRev <- ModRevs],
    Modules =
        lists:foldl(
          fun (TransformFun, Modules1) ->
                  TransformFun(Ctx3, Modules1)
          end, Modules0, Opts#opts.transform),

    validate_features(Opts#opts.ignore_unknown_features, Ctx3),

    if Opts#opts.emit == undefined ->
            case Opts#opts.debug_print of
                true ->
                    debug_print(Ctx3, Modules, standard_io);
                all ->
                    ?iof("~p\n", [Modules]);
                _ ->
                    ok
            end;
       true ->
            ok
    end,
    HasTransformErrors = yang_error:has_errors(Ctx3),
    %% Output plugins are allowed to be executed in presence of
    %% model/transform errors, in case they sign up for that
    %% through yanger_plugin:register_output_format/4.
    if ((not Success) or HasTransformErrors) and
       (not Opts#opts.emit_allow_errors) ->
            yang_error:print_errors(Ctx3, Opts#opts.print_error_code),
            throw({error, errors_printed});
       Opts#opts.emit == undefined ->
            %% Print warnings from before the emit function
            yang_error:print_errors(Ctx3, Opts#opts.print_error_code),
            ok;
       Modules /= [] ->
            EmitFun = Opts#opts.emit,
            OutFd =
                case Opts#opts.outfile of
                    undefined ->
                        standard_io;
                    Outfile ->
                        case file:open(Outfile,
                                       [delayed_write, write]) of
                            {ok, Fd} ->
                                Fd;
                            {error, Error} ->
                                throw({error,
                                       {file_error, Outfile, Error}})
                        end
                end,
            %% Collect format warnings and errors. Print both
            %% transformation and format warnings and errors
            %% at once (in order to sort)
            FormatWarningsErrors =
                try
                    EmitFun(Ctx3, Modules, OutFd)
                catch
                    throw:EmitError ->
                        throw(EmitError);
                    ?CATCH_STACKTRACE_WHEN(Class, Reason, Stacktrace,
                                           OutFd /= standard_io)
                        %% don't leave the file behind on error/throw
                        file:delete(Opts#opts.outfile),
                        erlang:raise(Class, Reason, Stacktrace);
                    ?CATCH_STACKTRACE(_Class, Reason, Stacktrace)
                        EmitError = {emit_crash,
                                     Ctx3,
                                     Opts,
                                     Reason,
                                     Stacktrace},
                        throw({error, EmitError})
                end,
            Ctx4 = Ctx3#yctx{errors = Ctx3#yctx.errors ++ FormatWarningsErrors},
            HasFormatErrors =
                yang_error:print_errors(Ctx4, Opts#opts.print_error_code),
            if OutFd /= standard_io ->
                    file:close(OutFd);
               true ->
                    ok
            end,
            case HasFormatErrors of
                true ->
                    %% If there is an output file and errors have
                    %% been printed from the emit function, then
                    %% delete the output file.
                    case Opts#opts.outfile of
                        undefined ->
                            ok;
                        OutfileEmit ->
                            file:delete(OutfileEmit)
                    end,
                    throw({error, errors_printed});
                false ->
                    ok
            end;
       true ->
            throw({error, format_no_modules})
    end.

validate_features(true = _IgnoreUnknownFeatures, _Ctx) ->
    ok;
validate_features(_IgnoreUnknownFeatures, #yctx{features = none}) ->
    ok;
validate_features(false = _IgnoreUnknownFeatures,
                  #yctx{features = Features} = Ctx) ->
    FeaturesLs = yang:map_to_list(Features),
    [validate_feature(F, Ctx) ||  F <- FeaturesLs].

validate_feature({ModuleName, FeatureNames}, #yctx{} = Ctx) ->
    case yang:search_module(Ctx, ModuleName, undefined) of
        {true, _Ctx, #module{name = ModuleName, features = Features}} ->
            validate_feature1(FeatureNames, Features);
        _ ->
            ErrMsg = "Module " ++ ?a2l(ModuleName) ++ " is not found",
            throw({error, {bad_features, ErrMsg}})
    end.

validate_feature1(FeatureNames, Features) ->
    EvalFun = fun(FeatureName) ->
                      case yang:map_lookup(FeatureName, Features) of
                          none ->
                              throw({error, {bad_features,
                                             "Feature not found in the module"
                                            }});
                          _ ->
                              ok
                      end
              end,
    lists:foreach(EvalFun, FeatureNames).

t_max_status(Ctx, [#module{children = Chs, identities = Ids,
                           local_augments = LAug,
                           remote_augments = RAug} = M | T],
             MaxStatus) ->
    MaxStatusN = n(MaxStatus),
    [M#module{children = prune_sn_status(Chs, MaxStatusN)
              , identities = prune_identities_status(Ids, MaxStatusN)
              , local_augments = prune_augment_status(LAug, MaxStatusN)
              , remote_augments = prune_remote_augment_status(RAug, MaxStatusN)
             } |
     t_max_status(Ctx, T, MaxStatus)];
t_max_status(_Ctx, [], _) ->
    [].

n(undefined) -> 0;
n(current) -> 1;
n(deprecated) -> 2;
n(obsolete) -> 3.

prune_sn_status([#sn{status = Status, children = Chs} = H | T], MaxStatusN) ->
    case n(Status) =< MaxStatusN of
        true ->
            [H#sn{children = prune_sn_status(Chs, MaxStatusN)} |
             prune_sn_status(T, MaxStatusN)];
        false ->
             prune_sn_status(T, MaxStatusN)
    end;
prune_sn_status([], _) ->
    [].

prune_identities_status(Ids, MaxStatusN) ->
    Deletes =
        yang:map_foldl(
          fun(Key, #identity{status = Status}, Acc) ->
                  case n(Status) =< MaxStatusN of
                      true ->
                          Acc;
                      false ->
                          [Key | Acc]
                  end
          end, [], Ids),
    lists:foldl(
      fun(Key, Map) ->
              yang:map_delete(Key, Map)
      end, Ids, Deletes).

prune_augment_status(Augs, MaxStatusN) ->
    lists:zf(
      fun(#augment{children = Chs, status = Status} = A) ->
              case n(Status) =< MaxStatusN of
                  true ->
                      case prune_sn_status(Chs, MaxStatusN) of
                          [] ->
                              false;
                          Chs1 ->
                              {true, A#augment{children = Chs1}}
                      end;
                  false ->
                      false
              end
      end, Augs).

prune_remote_augment_status(RAugs, MaxStatusN) ->
    lists:zf(
      fun({RemoteModule, Augs}) ->
              case prune_augment_status(Augs, MaxStatusN) of
                  [] ->
                      false;
                  Augs1 ->
                      {true, {RemoteModule, Augs1}}
              end
      end, RAugs).

print_version(Name) ->
    io:format("~s ~s\n", [Name, vsn()]).

print_usage(Name, Ctx) ->
    getopt:usage(
      Ctx#yctx.option_specs, Name,
      "<file>...",
      [{"<file>",
        "Validate and optionally convert the YANG module in <file>"}]).

print_error(Error) ->
    case Error of
        {bad_format, F} ->
            io:format(standard_error, "Unknown format '~s'\n", [F]);
        {bad_transform, T} ->
            io:format(standard_error, "Unknown transform '~s'\n", [T]);
        {bad_status, Status} ->
            io:format(standard_error, "Bad status '~w'\n", [Status]);
        {bad_features, FStr} ->
            io:format(standard_error, "Bad features spec '~s'\n", [FStr]);
        {bad_conformance, CStr} ->
            io:format(standard_error, "Bad conformance '~s'\n", [CStr]);
        errors_printed ->
            ok;
        {file_error, FileName, FileError} ->
            io:format(standard_error, "error: ~s: ~s\n",
                      [FileName, file:format_error(FileError)]);
        {fmt, Fmt, Args} ->
            io:format(standard_error, "error: " ++ Fmt, Args);
        format_no_modules ->
            io:format(standard_error,
                      "error: -f but no module file name given\n", []);
        {emit_crash, Ctx, Opts, Reason, StackTrace} ->
            yang_error:print_errors(Ctx, Opts#opts.print_error_code),
            io:format(standard_error,
                      "internal error: the output plugin experienced "
                      "a fatal error.\n",
                      []),
            case get_error_mask() of
                false ->
                    case Reason of
                        {error, WrappedError} ->
                            print_error(WrappedError);
                        _ ->
                            ok
                    end;
                _ ->
                    print_crash({yang_internal_error, {Reason, StackTrace}},
                                StackTrace)
            end
    end.

print_crash(X, Stacktrace0) ->
    %% since we keep the entire data structure on the heap,
    %% our crashes can be huge.  mask the error.
    %% FIXME: use another mechanism than env var?
    case get_error_mask() of
        false ->
            io:format(standard_error, "internal error\n", []);
        YANGERROR ->
            case X of
                {yang_internal_error, {Reason, StackTrace}} ->
                    print_internal_error(YANGERROR, Reason, StackTrace);
                _ ->
                    print_internal_error(YANGERROR, X, Stacktrace0)
            end
    end.

print_internal_error(YANGERROR, Reason, StackTrace) ->
    case YANGERROR of
        "stack" ->
            io:format(standard_error,
                      "internal error\n~p\n~p\n",
                      [Reason, StackTrace]);
        "pruned-stack" ->
            io:format(standard_error,
                      "internal error: ~p\n~p\n",
                      [prune_reason(Reason),
                       prune_stacktrace(StackTrace)]);
        "super-pruned" ->
            io:format(standard_error,
                      "internal error: ~p\n~p\n",
                      [prune_reason(Reason),
                       superprune_stacktrace(StackTrace)]);
        DepthStr ->
            case (catch ?l2i(DepthStr)) of
                {'EXIT', _} ->
                    Depth = 10;
                Depth ->
                    ok
            end,
            io:format(standard_error, "~P\n~p\n",
                      [Reason, Depth, StackTrace])
    end.

prune_reason(T) when is_tuple(T) ->
    element(1, T);
prune_reason(_) ->
    [].

superprune_stacktrace([{M,F,A,L}|_]) ->
    [{M,F,prune_args(A),L}].

prune_stacktrace(Trace) ->
    [{M,F,prune_args(A),L} || {M,F,A,L} <- Trace].

prune_args(A) when is_list(A) ->
    length(A);
prune_args(A) ->
    A.

%% parse Args for --plugindir/-P in order for the plugins to
%% register themselves.
get_plugin_path(["--plugindir", Dir | T]) ->
    [Dir | get_plugin_path(T)];
get_plugin_path(["-P", Dir | T]) ->
    [Dir | get_plugin_path(T)];
get_plugin_path([_ | T]) ->
    get_plugin_path(T);
get_plugin_path([]) ->
    [].

-spec get_error_mask() -> false | string().
get_error_mask() ->
    os:getenv("YANGERROR").

delayed_halt(Code) ->
    erlang:halt(Code, [{flush,true}]).

debug_print(_, Modules, Fd) ->
    lists:foreach(fun(M) -> pp_module(M, Fd) end, Modules).

pp_module(M, Fd) ->
    io:format(Fd, "module: ~p\n", [M#module.name]),
    io:format(Fd, "typedefs:\n", []),
    pp_typedefs(yang:map_to_list((M#module.typedefs)#typedefs.map), Fd, "  "),
    io:format(Fd, "identities:\n", []),
    pp_identites(yang:map_to_list(M#module.identities), Fd, "  "),
    io:format(Fd, "children:\n", []),
    pp_children(M#module.children, Fd, "  ").

-define(format_record(Rec, Name),
        format_record(Rec, Name, record_info(fields, Name))).
-define(frec(Rec, Name),?format_record(Rec,Name)).

pp_children([H | T], Fd, Indent) ->
    io:format(Fd, "~s~s ~p (cfg: ~p)\n",
              [Indent, H#sn.kind, H#sn.name, H#sn.config]),
    if H#sn.type /= undefined ->
            io:format(Fd, "~s  type: ~s\n", [Indent, ?frec(H#sn.type, type)]);
       true ->
            ok
    end,
    pp_children(H#sn.children, Fd, "  " ++ Indent),
    pp_children(T, Fd, Indent);
pp_children([], _Fd, _Indent) ->
    ok.

pp_typedefs([{_, H} | T], Fd, Indent) ->
    io:format(Fd, "~s~s\n", [Indent, ?frec(H, typedef)]),
    pp_typedefs(T, Fd, Indent);
pp_typedefs([], _Fd, _Indent) ->
    ok.

pp_identites([{_, H} | T], Fd, Indent) ->
    io:format(Fd, "~s~s\n", [Indent, ?frec(H, identity)]),
    pp_identites(T, Fd, Indent);
pp_identites([], _Fd, _Indent) ->
    ok.


format_record(Record, Name, Fields) ->
    case tuple_to_list(Record) of
        [Name | Rest] ->
            io_lib:format("record ~w {\n~s", [Name,
                                            format_record(Rest, Fields)]);
        _X ->
            "badrecord"
    end.

format_record([], []) ->
    [];
format_record([Val|Vals], [F|Fs]) when is_integer(Val);
                                       Val == [];
                                       is_atom(Val);
                                       is_float(Val)->
    Curly = if Vals == [] -> " }";
               true -> ","
            end,
    [io_lib:format("     ~w = ~w~s\n", [F,Val,Curly]),
     format_record(Vals, Fs)];
format_record([Val|Vals], [F|Fs]) ->
    case is_string(Val) of
        true ->
            [io_lib:format("     ~w = \"~s\"\n", [F,Val]),
             format_record(Vals, Fs)];
        false ->
            [io_lib:format("     ~w = ~p~n", [F, Val]),
             format_record(Vals, Fs)]
    end.

is_string(L) when is_list(L) ->
    all_integer(L);
is_string(_) ->
    false.

all_integer([H|T]) when is_integer(H),
                        $A < H, H < $z ->
    all_integer(T);
all_integer([_|_]) ->
    false;
all_integer([]) ->
    true.
