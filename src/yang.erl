-module(yang).

-compile({no_auto_import,[map_get/2]}).

-export([load_plugins/1, new_ctx/1, init_ctx/2,
         add_file/2, add_file/3, post_add_modules/1]).
-export([add_llerrors/2]).

-export([kwd_class/2]).
-export([stmt_keyword/1, stmt_arg/1, stmt_pos/1, stmt_substmts/1]).
-export([search_one_stmt/2, search_one_stmt/3]).
-export([search_one_substmt/2, search_one_substmt/3]).
-export([search_all_stmts/2]).
-export([get_substmt_arg/3, get_substmt_arg/4,
         get_stmts_arg/3, get_stmts_arg/4,
         substmts/1, substmt/2,
         substmt_present/2,
         if_substmt/4, if_substmt/5]).
-export([common_substmts/4]).

-export([chk_mandatory/1]).
-export([sort_targets/3, patch_target/3,
         update_conditional_hooks/2, run_mk_sn_hooks/6]).
-export([get_module/3, get_imported_module/3, get_module_from_pos/2]).
-export([get_grouping/3, get_typedef/3]).
-export([find_typedef_raw/4]).
-export([is_builtin_type/1, get_filename/1, local_name/1]).
-export([mark_import_as_used/3]).
-export([parse_idref/3, resolve_raw_idref/4]).
-export([get_identity/5,
         get_feature_expr/4, check_if_features/3]).
-export([combine_prefix_maps/3]).

-export([parse_schema_nodeid/4, parse_schema_nodeid/5,
         parse_absolute_schema_nodeid/4,
         parse_descendant_schema_nodeid/4]).
-export([mk_cursor/6, cursor_reset/2, cursor_follow_path/3, cursor_move/3,
         find_child/2, find_child/5]).
-export([has_mandatory_descendant/1]).

-export([map_new/0, map_is_empty/1, map_insert/3, map_update/3, map_keys/1,
         map_lookup/2, map_get/2, map_get/3,
         map_is_key/2, map_delete/2, map_to_list/1,
         map_iterator/1, map_next/1,
         map_foldl/3, map_foreach/2]).
-export([topo_sort/1, get_schema_node/2]).
-export([pre_mk_sn_xpath/5, search_module/3]).

-ifdef(debugX).
-export([tst/1, pp_module/1]).
-endif.

%% Everything is done in one process, so that we can use pointers to terms
%% in the tree.
%%
%% 1.  stmttree = parse module
%% 2.  build prefix map
%% 3.  make #module{}
%% 4.  for each import, add the module
%% 5.  verify that all includes are correct:
%%       no loops
%%       all submodule's recursive includes are also included by module
%%       all submodules belongs-to the module
%%     <maybe do this in step 6>
%% 6.  for each include:
%%       parse the submodule
%%       build prefix map
%%       for each include:
%%          recurse
%%       add global typedefs, groupings, ... to #module{}
%%
%% Plugins:
%%   o  dynamic discovery of plugins (beam / erl files -
%%           maybe with special names?)
%%   o  call plugin_init() in each plugin
%%      This function can register, by using the API module yanger_plugin:
%%      + grammar, arg types
%%      - extension module handling
%%      - validation hooks
%%      + error codes
%%      + output formats
%%      + command line options
%%      - handling for certain types (e.g. inet:ipv4-address)

-include_lib("yanger/include/yang.hrl").

-include("yang_llerror.hrl").

-export_type([builtin_type_name/0, verbosity/0,
              cursor_path/0, cursor_type/0, cursor_skipped/0,
              hookfun/0, keyword/0, kind/0,
              map/2, map0/0, module_rec/0, pos/0, prefix_map/0,
              revision/0, stmt/0, sn_name/0,
              grouping_rec/0, typedef_rec/0, augment_rec/0,
              validate_status/0, yang_identifier/0, yang_status/0,
              import/0, yang_version/0, modrev/0, conformance/0]).

-type verbosity() :: ?V_SILENT | ?V_NORMAL.

-type yang_version() :: '1' | '1.1'.

-type pos() :: {FileName :: string(), Line :: integer()}
             | {FileName :: string(), Line :: integer(), Col :: integer()}
             | {uses, 'undefined' | pos(), pos()}.

-type keyword() :: CoreName :: atom()
                 | {ModuleName :: atom(), ExtensionName :: atom()}.

-type yang_identifier() :: LocalName :: atom()
                         | {ModuleName :: atom(), ExternalName :: atom()}.

-type revision() :: 'undefined' | binary().

-type modrev() :: {ModuleName :: atom(), Revision :: revision()}.

-type import() ::
        {ModuleName :: atom(), Revision :: revision(),
         Prefix :: atom(), RequestedRevision :: revision()}.

-type builtin_type_name() ::
        'binary'
      | 'bits'
      | 'boolean'
      | 'decimal64'
      | 'empty'
      | 'enumeration'
      | 'identityref'
      | 'instance-identifier'
      | 'int8'
      | 'int16'
      | 'int32'
      | 'int64'
      | 'leafref'
      | 'string'
      | 'uint8'
      | 'uint16'
      | 'uint32'
      | 'uint64'
      | 'union'.

-type cursor_type() ::
        'schema'
      | 'data'.
-type cursor_skipped() ::
        'undefined'
      | 'input'
      | 'output'.
-type cursor_path() :: [cursor_path_element()].
-type cursor_path_element() ::
        'parent'
      | {'child', yang:yang_identifier()}.

-type sn_name() ::
        atom()                          % local nodes
      | {ModuleName :: atom(), atom()}. % augmented nodes

%% atom() can be used by plugins.
-type kind() ::
        'leaf' | 'leaf-list' | 'container' | 'list'
      | 'choice' | 'case'
      | 'operation' | 'input' | 'output'
      | 'notification'
      | 'anyxml' | 'anydata'
      | '__tmp_augment__'
      | atom().

-type yang_status() :: 'current' | 'deprecated' | 'obsolete'.

-type conformance() :: 'implement' | 'import'.

-type map(Key, Val) :: gb_trees:tree(Key, Val).
-type map0() :: map(term(), term()).

%% Each module and submodule has its own prefix map
%% '$self' represents the prefix to the current module.
-type prefix_map() :: map(Prefix :: atom(),
                          '$self' | atom()).

%% used to find cyclic definitions
-type validate_status() :: 'undefined' | 'processing' | 'done'.

%% We really would like to say:
%%-type hookfun() :: fun((#yctx{}, term(), ...) -> {#yctx{}, term()}).
%-type hookfun() :: fun((...) -> {#yctx{}, term()}).
-type hookfun() :: term().

%% see yang_parser_nif.c
-type no_arg() :: [].
-type arg_type() :: no_arg() | binary() | atom() | integer() | identifier_ref().

-type stmt() :: {keyword(), Arg :: arg_type(), pos(), [stmt()]}.

-type raw_identifier_ref() :: atom() | {Prefix :: atom(), Name :: atom()}.

-type identifier_ref() :: atom() | {ModuleName :: atom(), Name :: atom()}.

%% Ugh!  stupid erlang compiler!! I cannot refer to #grouping{} before
%% it has been defined, but I can refer to grouping_rec()...
-type grouping_rec() :: #grouping{}.
-type typedef_rec() :: #typedef{}.
-type augment_rec() :: #augment{}.
-type module_rec() :: #module{}.

-spec load_plugins(PluginPath :: [Dir :: string()]) ->
        Plugins :: [ModuleName :: atom()].
%% @doc Loads all available plugin modules.
%% Call this function once!
load_plugins(PluginPath0) ->
    PluginPath1 =
        %% Always add plugins from the priv dir
        [code:priv_dir(yanger) |
         %% Always add plugins from an environment variable
         case os:getenv("YANGER_PLUGINPATH") of
             Str when is_list(Str) ->
                 PluginPath0 ++ string:tokens(Str, ":");
             false ->
                 PluginPath0
         end],
    load_plugins_from_path(PluginPath1).

load_plugins_from_path(PluginPath) ->
    Plugins =
        lists:foldl(
          fun(Dir, Acc) ->
                  case file:list_dir(Dir) of
                      {ok, Files} ->
                          lists:foldl(
                            fun(FName, Acc0) ->
                                    case load_file(Dir, FName) of
                                        {true, Module} ->
                                            case is_plugin(Module) of
                                                true ->
                                                    [Module | Acc0];
                                                false ->
                                                    Acc0
                                            end;
                                        false ->
                                            Acc0
                                    end
                            end, Acc, Files);
                      _ ->
                          Acc
                  end
          end, [], PluginPath),
    Plugins.

load_file(Dir, FName) ->
    case filename:extension(FName) of
        ".beam" ->
            FullFName = filename:join(Dir, FName),
            case file:read_file(FullFName) of
                {ok, Bin} ->
                    Module = ?l2a(filename:rootname(FName)),
                    %% this lets us use the debugger to load plugins before
                    %% loading them from the plugin path like this
                    case code:is_loaded(Module) of
                        false ->
                            case code:load_binary(Module, FullFName, Bin) of
                                {module, Module} ->
                                    {true, Module};
                                _ ->
                                    false
                            end;
                        _ ->
                            {true, Module}
                    end;
                _ ->
                    false
            end;
        _ ->
            false
    end.

is_plugin(Module) ->
    Attrs = Module:module_info(attributes),
    case lists:keyfind(behaviour, 1, Attrs) of
        {behaviour, Behaviours} ->
            lists:member(yanger_plugin, Behaviours);
        false ->
            false
    end.

-spec new_ctx(Plugins :: [ModuleName :: atom()]) -> #yctx{}.
%% @doc Creates a new YANG compiler context and initializes the plugins.
new_ctx(Plugins) ->
    ErrorCodes =
        yang_error:codes() ++
        yang_llerror:codes(),
    Env = mk_env(),
    PEMARAAD = fun post_expand_module_after_remote_augments_and_deviations/2,
    Ctx0 = #yctx{error_codes = ErrorCodes,
                 plugins = Plugins,
                 %% NOTE: the hooks will be called in reverse order
                 hooks = #hooks{pre_mk_sn = [fun pre_mk_sn_config/5,
                                             fun pre_mk_sn_xpath/5],
                                post_mk_sn =
                                    [fun post_mk_sn_unique/5,
                                     fun post_mk_sn_keys/5,
                                     fun post_mk_sn_v_default/5,
                                     fun post_mk_sn_type/5,
                                     fun post_mk_sn_v_choice_default/5],
                                post_expand_module = [PEMARAAD],
                                post_expand_sn = [fun post_expand_sn/4],
                                post_parse_module =
                                    [fun post_parse_module_conformance/2]
                               },
                 modrevs = map_new(),
                 revs = map_new(),
                 typemap = map_new(),
                 files = map_new(),
                 bad_typedefs = map_new(),
                 env = Env},
    Ctx2 = lists:foldl(fun(M, Ctx1) -> M:init(Ctx1) end, Ctx0, Plugins),
    Ctx2#yctx{env = (Ctx2#yctx.env)#env{
                      extension_modules =
                          yang_parser:get_grammar_module_names()}}.

mk_env() ->
    DataDefMap =
        lists:foldl(fun({DataDefKeyword, Kind}, Map) ->
                            map_insert(DataDefKeyword, Kind, Map);
                       (DataDefKeyword, Map) ->
                            map_insert(DataDefKeyword, DataDefKeyword, Map)
                    end, map_new(), data_definition_stmts()),
    RefinementsMap =
        add_refinements(map_new()),
    #env{data_definition_stmts = DataDefMap,
         copy_from_grouping_stmts = map_new(),
         deviates = map_new(),
         refinements = RefinementsMap}.

data_definition_stmts() ->
    ['leaf', 'leaf-list', 'container', 'list',
     'choice', 'case',
     {'rpc', 'operation'}, {'action', 'operation'},
     'input', 'output',
     'notification',
     'anyxml', 'anydata'].

-spec init_ctx(#yctx{}, SearchPath :: [Dir :: string()]) ->
        #yctx{}.
%% @doc Initialize the YANG compiler context.
init_ctx(Ctx0, ExtraSearchPath) ->
    SearchPath = ExtraSearchPath ++ search_path(),
    case do_verbose(?V_NORMAL, Ctx0) of
        true ->
            io:format("YANG module search path:\n"),
            lists:foreach(fun(Dir) ->
                                  io:format("  ~s\n", [Dir])
                          end, SearchPath);
        false ->
            ok
    end,
    Files = lists:foldl(
              fun(Dir, Acc) ->
                      case file:list_dir(Dir) of
                          {ok, L} ->
                              Acc ++ [filename:join(Dir, FName) || FName <- L];
                          _ ->
                              Acc
                      end
              end, [], SearchPath),
    Fm = lists:foldl(
           fun(FName, M) ->
                   case filename:extension(FName) of
                       ".yang" ->
                           case parse_file_name(FName) of
                               {ok, Modname, Revision} ->
                                   add_mod(M, Modname, Revision, FName);
                               _ ->
                                   M
                           end;
                       _ ->
                           M
                   end
           end, map_new(), Files),
    Ctx1 = Ctx0#yctx{search_path = SearchPath,
                     files = Fm, % FIXME: scan path, add 'parse_to_body()'
                     features = case Ctx0#yctx.features of
                                    undefined ->
                                        map_new();
                                    FMap ->
                                        FMap
                                end
                    },
    Ctx2 = yang_types:register_builtin_types(Ctx1),
    run_hooks(#hooks.post_init_ctx, Ctx2).

add_mod(M0, Modname, Revision, FName) ->
    M1 =
        case map_lookup({Modname, latest}, M0) of
            {value, {_, LatestRevision}} when Revision =< LatestRevision;
                                              Revision == 'undefined' ->
                M0;
            _ ->
                map_update({Modname, latest}, {FName, Revision}, M0)
        end,
    try
        map_insert({Modname, Revision}, {FName, Revision}, M1)
    catch
        _:_ ->
            M1
    end.


search_path() ->
    ["."]
        ++ case os:getenv("YANG_MODPATH") of
               false ->
                   [];
               ModPath ->
                   string:tokens(ModPath, ":")
           end
        ++ case os:getenv("HOME") of
               false ->
                   [];
               Home ->
                   [filename:join([Home, "yang", "modules"])]
           end
        ++ case os:getenv("YANG_INSTALL_DIR") of
               false ->
                   [filename:join(["/usr", "share", "yang", "modules"])];
               InstallDir ->
                   [filename:join([InstallDir, "yang", "modules"])]
           end.

%% called when a filename is given on the cmdline
-spec add_file(#yctx{}, FileName :: string()) ->
        {Success :: boolean(), #yctx{}, #module{} | undefined}.
add_file(Ctx, FileName) ->
    add_file(Ctx, FileName, _AddCause = primary).
add_file(Ctx, FileName, AddCause) ->
    try
        add_file0(Ctx, FileName, AddCause)
    catch
        ?CATCH_STACKTRACE(error, _X, Stacktrace)
            %% since we keep the entire data structure on the heap,
            %% our crashes can be huge.  mask the error.
            %% FIXME: use another mechanism than env var?
            case os:getenv("YANGERROR") of
                false ->
                    erlang:error({yang_internal_error, Stacktrace});
                _ ->
                    erlang:error({yang_internal_error, {_X, Stacktrace}})
            end
    end.

get_filename(#module{stmt = {_, _, Pos, _}}) ->
    element(1, Pos).

post_add_modules(Ctx0) ->
    %% Running post_expand_module here instead of at the end of
    %% parse_body() means that augments and deviations from other
    %% modules have already been done, and thus it is possible to detect
    %% e.g. breakage due to deviating away leafref targets (or nodes
    %% referenced by when/must, when yang.erl validates those). But it
    %% means that post_expand_xxx hooks will not report errors for the
    %% "original" module if they are "hidden" by deviations - i.e. we
    %% assume that at some point every module is compiled *without*
    %% deviations... #sn{}'s resulting from augments from other modules
    %% are actually filtered out from the post_expand_sn hook
    %% invocations, but post_expand_module hooks will see them.
    Ctx1 = map_foldl(
             fun (_ModRev, M, #yctx{hooks = OrigHooks} = Ctx_0) ->
                     IsModule = stmt_keyword(M#module.stmt) == 'module',
                     if M#module.yang_version == '1' orelse IsModule ->
                             Ctx_1 = update_conditional_hooks(Ctx_0, M),
                             Ctx_2 = post_expand_module(M, Ctx_1),
                             Ctx_2#yctx{hooks = OrigHooks};
                        true -> % submodule, version >= 1.1, don't check here;
                            %% instead, checks on the SNs that are defined in
                            %% this submodule, are done while processing the
                            %% module that this submodule belongs to. See:
                            %% post_expand_sn/4 function.
                             Ctx_0
                     end
             end, Ctx0, Ctx0#yctx.modrevs),
    Ctx2 = v_unique_namespaces(Ctx1),
    run_hooks(#hooks.post_add_modules, Ctx2).

%% run before post_add_modules hooks - don't require unique
%% namespace for "pseudo" modules added by plugins in the hooks
v_unique_namespaces(#yctx{modrevs = ModRevs} = Ctx) ->
    {Ctx1, _} =
        map_foldl(
          fun (_ModRev,
               #module{kind = module, modulename = Name,
                       namespace = Ns, stmt = Stmt},
               {Ctx_0, NsList}) ->
                  case lists:keyfind(Ns, 1, NsList) of
                      {_, Name} ->
                          %% different revision of same module
                          {Ctx_0, NsList};
                      {_, OtherName} ->
                          NsStmt = search_one_substmt('namespace', Stmt),
                          {add_error(Ctx_0,
                                     stmt_pos(NsStmt),
                                     'YANG_ERR_DUPLICATE_NAMESPACE',
                                     [Ns, OtherName]),
                           NsList};
                      false ->
                          {Ctx_0, [{Ns, Name}|NsList]}
                  end;
              (_ModRev, _M, Acc) ->
                  Acc
          end, {Ctx, []}, ModRevs),
    Ctx1.

add_file0(Ctx, FileName, AddCause) ->
    verbose(?V_NORMAL, Ctx, "Read file ~s\n", [FileName]),
    case yang_parser:parse(FileName, Ctx#yctx.canonical) of
        {ok, Stmts, LLErrors} ->
            Ctx1 = add_llerrors(LLErrors, Ctx),
            case parse_file_name(FileName) of
                {ok, FileModuleName, FileRevision} ->
                    add_parsed_stmt_tree(Ctx1, Stmts, FileName,
                                         AddCause,
                                         FileModuleName, FileRevision,
                                         _ExpectFailLevel = warning,
                                         _IncludingModRev = undefined);
                error ->
                    add_parsed_stmt_tree(Ctx1, Stmts, FileName,
                                                 AddCause,
                                         undefined, undefined,
                                         _ExpectFailLevel = warning,
                                         _IncludingModRev = undefined)
            end;
        {error, LLErrors} ->
            {false, add_llerrors(LLErrors, Ctx), undefined}
    end.

do_verbose(Lvl, #yctx{verbosity = V}) ->
    Lvl =< V.

verbose(Lvl, Ctx, Fmt, Args) ->
    case do_verbose(Lvl, Ctx) of
        true ->
            io:format(Fmt, Args);
        false ->
            ok
    end.

parse_file_name(FileName) ->
    BaseName = filename:basename(FileName, ".yang"),
    case string:tokens(BaseName, "@") of
        [ModuleName] ->
            {ok, ?l2a(ModuleName), undefined};
        [ModuleName, Revision] ->
            {ok, ?l2a(ModuleName), ?l2b(Revision)};
        _ ->
            error
    end.

get_module(ModuleName, Revision, Ctx) ->
    case map_lookup(ModuleName, Ctx#yctx.revs) of
        {value, [LatestRevision | _]} when Revision == undefined ->
            map_lookup({ModuleName, LatestRevision}, Ctx#yctx.modrevs);
        {value, _} ->
            map_lookup({ModuleName, Revision}, Ctx#yctx.modrevs);
        none ->
            none
    end.

get_module_from_pos({uses, _UsesPos, StmtPos}, Ctx) ->
    get_module_from_pos(StmtPos, Ctx);
get_module_from_pos({Filename, _Line}, Ctx) ->
    get_module_from_filename(Filename, Ctx);
get_module_from_pos({Filename, _Line, _Col}, Ctx) ->
    get_module_from_filename(Filename, Ctx).

get_module_from_filename(Filename, Ctx) ->
    get_module_from_filename(map_iterator(Ctx#yctx.modrevs), Filename, Ctx).

get_module_from_filename(Iter0, Filename, Ctx) ->
    case map_next(Iter0) of
        {{_ModuleName, _Revision}, #module{filename = Filename} = M, _Iter1} ->
            M;
        {{_ModuleName, _Revision}, _M, Iter1} ->
            get_module_from_filename(Iter1, Filename, Ctx);
        none ->
            undefined
    end.

search_module(Ctx, ModuleName, Revision) ->
    search_module(Ctx, undefined, undefined, ModuleName, Revision, undefined).

%% Searches for ModuleName with Revision (which can be undefined).
%% If the module is not already present, it is added to the ctx.
search_module(Ctx, FromPos, ModKeyword, ModuleName, Revision,
              IncludingModuleRevision) ->
    case map_lookup(ModuleName, Ctx#yctx.revs) of
        {value, [LatestRevision | _]} when Revision == undefined ->
            get_modrev(Ctx, FromPos, ModKeyword, ModuleName, LatestRevision,
                       IncludingModuleRevision);
        {value, _} ->
            get_modrev(Ctx, FromPos, ModKeyword, ModuleName, Revision,
                       IncludingModuleRevision);
        none ->
            %% if not, check if the exact file is available
            search_file(Ctx, FromPos, ModKeyword, ModuleName, Revision,
                        IncludingModuleRevision)
    end.

get_modrev(Ctx, FromPos, ModKeyword, ModuleName, Revision,
           IncludingModuleRevision) ->
    %% first check if the module is present already
    case map_lookup({ModuleName, Revision}, Ctx#yctx.modrevs) of
        {value, M} when is_record(M, module) ->
            {true, Ctx, M};
        {value, processing} ->
            {false, add_error(Ctx, FromPos,
                              'YANG_ERR_CIRCULAR_DEPENDENCY',
                              [ModKeyword, ModuleName])};
        none ->
            %% if not, check if the exact file is available
            search_file(Ctx, FromPos, ModKeyword, ModuleName, Revision,
                        IncludingModuleRevision)
    end.

search_file(Ctx, FromPos, ModKeyword, ModuleName, Revision,
            IncludingModuleRevision) ->
    SearchRevision =
        if Revision == undefined ->
                latest;
           true ->
                Revision
        end,
    FileName =
        case map_lookup({ModuleName, SearchRevision}, Ctx#yctx.files) of
            {value, {FileName0, _}} ->
                FileName0;
            none ->
                %% just get the latest version
                case
                    map_lookup({ModuleName, latest}, Ctx#yctx.files)
                of
                    {value, {FileName0, _}} ->
                        FileName0;
                    none ->
                        none
                end
        end,
    if FileName /= none ->
            %% When we import we don't want to check canonical,
            %% but if we are checking canonical for our module,
            %% we want to do it also for our submodules.
            if ModKeyword == 'submodule' ->
                    Canonical = Ctx#yctx.canonical,
                    AddCause = 'include';
               true ->
                    Canonical = false,
                    AddCause = 'import'
            end,
            verbose(?V_NORMAL, Ctx, "Read file ~s\n", [FileName]),
            case yang_parser:parse(FileName, Canonical) of
                {ok, Stmts, LLErrors} ->
                    Ctx1 = add_llerrors(LLErrors, Ctx),
                    %% Use the new canonical when parsing
                    %% this (sub)module.
                    Ctx2 = Ctx1#yctx{canonical = Canonical},
                    add_parsed_stmt_tree(Ctx2, Stmts, FileName,
                                         AddCause,
                                         ModuleName, Revision,
                                         _ExpectFailLevel = error,
                                         IncludingModuleRevision);
                {error, LLErrors} ->
                    {false, add_llerrors(LLErrors, Ctx)}
            end;
       true ->
            Ctx1 =
                case Revision of
                    undefined ->
                        add_error(Ctx, FromPos,
                                  'YANG_ERR_MODULE_NOT_FOUND',
                                  [ModuleName]);
                    _ ->
                        add_error(Ctx, FromPos,
                                  'YANG_ERR_MODULE_REV_NOT_FOUND',
                                  [ModuleName, Revision])
                end,
            {false, Ctx1}
    end.

%% called for modules and submodules
add_parsed_stmt_tree(Ctx00, [{ModKeyword, ModuleName, Pos, Substmts} = Stmt],
                     FileName, AddCause,
                     ExpectedModuleName, ExpectedRevision, ExpectFailLevel,
                     IncludingModuleRevision) ->
    ModuleRevision = get_latest_revision(Substmts, Ctx00),
    %% check that the module and revision is really the one we're looking for
    Ctx0 = if AddCause == primary ->
                   Primary = Ctx00#yctx.primary_module_names,
                   Ctx00#yctx{primary_module_names = [ModuleName | Primary]};
              true ->
                   Ctx00
           end,
    {Ctx1, DoFail} =
        if ModuleName /= ExpectedModuleName, ExpectedModuleName /= undefined ->
                {add_error(ExpectFailLevel, Ctx0, Pos,
                           'YANG_ERR_MODULENAME_MISMATCH',
                           [ExpectedModuleName, ModuleName]),
                 ExpectFailLevel == error};
           ModuleRevision /= ExpectedRevision, ExpectedRevision /= undefined ->
                {add_error(ExpectFailLevel, Ctx0, {FileName, 0},
                           'YANG_ERR_REVISION_MISMATCH',
                           [ExpectedRevision, ModuleRevision]),
                 ExpectFailLevel == error};
           true ->
                {Ctx0, false}
        end,
    case DoFail of
        true ->
            {false, Ctx1};
        false ->
            case map_lookup({ModuleName, ModuleRevision}, Ctx1#yctx.modrevs) of
                {value, processing} ->
                    {false, add_error(Ctx1, Pos,
                                      'YANG_ERR_CIRCULAR_DEPENDENCY',
                                      [ModKeyword, ModuleName])};
                {value, M} ->
                    %% already added, just return the module
                    {true, Ctx1, M};
                none ->
                    ModRevs = map_insert({ModuleName, ModuleRevision},
                                         processing, Ctx1#yctx.modrevs),
                    Revs0 = Ctx1#yctx.revs,
                    Revs1 = case map_lookup(ModuleName, Revs0) of
                                {value, RevList} ->
                                    map_update(ModuleName,
                                               lists:reverse(
                                                 lists:sort([ModuleRevision |
                                                             RevList])),
                                               Revs0);
                                none ->
                                    map_insert(ModuleName, [ModuleRevision],
                                              Revs0)
                            end,
                    Ctx2 = Ctx1#yctx{modrevs = ModRevs, revs = Revs1},
                    {Ctx3, [Stmt1]} =
                        run_hooks(#hooks.post_parse_stmt_tree, Ctx2, [Stmt]),
                    IncludingModuleRevision1 =
                        case ModKeyword of
                            'module' ->
                                ModuleRevision;
                            'submodule' ->
                                IncludingModuleRevision
                        end,
                    PrefixMap = mk_prefix_map(substmts(Stmt1), Ctx3),
                    M0 = #module{filename = FileName,
                                 kind = ModKeyword,
                                 name = ModuleName,
                                 modulename = ModuleName,
                                 stmt = Stmt1,
                                 prefix_map = PrefixMap,
                                 revision = ModuleRevision,
                                 modulerevision = IncludingModuleRevision1,
                                 add_cause = AddCause},
                    {Ctx4, M1} = parse_module(Stmt1, M0, Ctx3),
                    {Ctx5, M2} = post_parse_module(Ctx4, M1),
                    ModRevs2 = map_update({ModuleName, ModuleRevision}, M2,
                                          Ctx5#yctx.modrevs),
                    Ctx6 = Ctx5#yctx{modrevs = ModRevs2},
                    {true, Ctx6, M2}
            end
    end.

%% Pre: All statements are grammatically correct
%% This implies that we don't have to check that mandatory statements
%% are present, and that arguments are of correct type.
%% parse_module :: {boolean(), Ctx, M}
parse_module({_ModuleKeyword, _Name, _Pos, Substmts}, M, Ctx) ->
    parse_header(Substmts, M, Ctx).

%% Handle header stmts
%% returns a context
parse_header([{Kwd, Arg, _Pos, Substmts} = _H | T] = Stmts, M, Ctx) ->
    case Kwd of
        'namespace' ->
            parse_header(T, M#module{namespace = Arg}, Ctx);
        'prefix' ->
            parse_header(T, M#module{prefix = Arg}, Ctx);
        'belongs-to' ->
            {_, Prefix, _, _} = search_one_stmt('prefix', Substmts),
            parse_header(T, M#module{modulename = Arg, prefix = Prefix}, Ctx);
        'yang-version' ->
            case Arg of
                <<"1.1">> ->
                    parse_header(T, M#module{yang_version = '1.1'}, Ctx);
                _ ->
                    parse_header(T, M, Ctx)
            end;
        _ ->
            case kwd_class(Kwd, Ctx) of
                header ->
                    parse_header(T, M, Ctx);
                extension ->
                    case get_extension_handler(Kwd, Ctx) of
%% avoid dialyzer complaint
%                        {true, HandlerMod} ->
%                            {M1, Ctx1} = HandlerMod:parse_header(H, M, Ctx),
%                            parse_header(T, M1, Ctx1);
                        false ->
                            parse_header(T, M, Ctx)
                    end;
                _ ->
                    parse_linkage(Stmts, M, Ctx)
            end
    end;
parse_header([], M, Ctx) ->
    parse_linkage([], M, Ctx).

%% Pre: header stmts are handled
%% Handle linkage stmts
parse_linkage([{Kwd, Arg, Pos, Substmts} = _H | T] = Stmts, M, Ctx) ->
    case kwd_class(Kwd, Ctx) of
        linkage ->
            %% handle import/include of module/submodule Arg
            case search_one_stmt('revision-date', Substmts) of
                {_, Revision, _, _} ->
                    ok;
                false ->
                    Revision = undefined
            end,
            {ModKeyword, IncRev, YangVersion} =
                case Kwd of
                    'import' ->
                        {'module', undefined, M#module.yang_version};
                    'include' ->
                        {'submodule', M#module.modulerevision, M#module.yang_version}
                end,
            case search_module(Ctx, Pos, ModKeyword, Arg, Revision, IncRev) of
                {true, Ctx1, SubM} when Kwd == 'include' ->
                    Ctx2 = v_include(M, Pos, SubM, YangVersion, Ctx1),
                    M1 = M#module{submodules =
                                      M#module.submodules ++ [{SubM, Pos}]},
                    parse_linkage(T, M1, Ctx2);
                {true, Ctx1, ImpM} ->
                    {_, Prefix, _, _} = search_one_stmt('prefix', Substmts),
                    {M1, Ctx2} = v_import(M, Pos, ImpM, Prefix, Revision, YangVersion, Ctx1),
                    parse_linkage(T, M1, Ctx2);
                {false, Ctx1} ->
                    parse_linkage(T, M, Ctx1)
            end;
        extension ->
            case get_extension_handler(Kwd, Ctx) of
%% avoid dialyzer complaint
%                {true, HandlerMod} ->
%                    {M1, Ctx1} = HandlerMod:parse_linkage(H, M, Ctx),
%                    parse_linkage(T, M1, Ctx1);
                false ->
                    parse_linkage(T, M, Ctx)
            end;
        _ ->
            Ctx1 = v_all_includes(M, Ctx),
            Ctx2 = add_unused_imports(M, Ctx1),
            parse_meta(Stmts, M, Ctx2)
    end;
parse_linkage([], M, Ctx) ->
    Ctx1 = add_unused_imports(M, Ctx),
    parse_meta([], M, Ctx1).

add_unused_imports(#module{name = ModuleName, imports = Imports},
                   #yctx{unused_imports = Unused} = Ctx) ->
    Ctx#yctx{unused_imports = [{ModuleName, Imports} | Unused]}.

%% Pre: meta stmts are handled
%% Handle meta stmts
parse_meta([{Kwd, _Arg, _Pos, _Substmts} = _H | T] = Stmts, M, Ctx) ->
    case kwd_class(Kwd, Ctx) of
        meta ->
            %% meta statements are not handled specifically; they are
            %% just kept as substmts.
            %% FIXME: make proper record fields for them?
            parse_meta(T, M, Ctx);
        extension ->
            case get_extension_handler(Kwd, Ctx) of
%% avoid dialyzer complaint
%                {true, HandlerMod} ->
%                    {M1, Ctx1} = HandlerMod:parse_meta(H, M, Ctx),
%                    parse_meta(T, M1, Ctx1);
                false ->
                    parse_meta(T, M, Ctx)
            end;
        _ ->
            parse_revision(Stmts, M, Ctx)
    end;
parse_meta([], M, Ctx) ->
    parse_revision([], M, Ctx).

v_import(M, Pos, ImpM, Prefix, Revision, YangVersion, Ctx) ->
    if Revision /= undefined andalso YangVersion == '1' andalso ImpM#module.yang_version == '1.1' ->
           {M, add_error(Ctx, Pos, 'YANG_ERR_BAD_IMPORT_YANG_VERSION',
                         [YangVersion, ImpM#module.yang_version])};
       true ->
            if ImpM#module.kind == 'module' ->
                    NewImport =
                        {ImpM#module.name, ImpM#module.revision, Prefix, Revision},
                    {M#module{imports = [NewImport | M#module.imports]}, Ctx};
               true ->
                    {_, _, ModPos, _} = ImpM#module.stmt,
                    {M, add_error(Ctx, Pos, 'YANG_ERR_BAD_IMPORT',
                                  [yang_error:fmt_pos(ModPos)])}
            end
    end.

v_include(M, Pos, SubM, YangVersion, Ctx1) ->
    if SubM#module.yang_version /= YangVersion ->
            add_error(Ctx1, Pos, 'YANG_ERR_BAD_INCLUDE_YANG_VERSION',
                      [SubM#module.yang_version, YangVersion]);
       SubM#module.kind /= 'submodule' ->
            {_, _, SubPos, _} = SubM#module.stmt,
            add_error(Ctx1, Pos, 'YANG_ERR_BAD_INCLUDE',
                      [yang_error:fmt_pos(SubPos)]);
       SubM#module.modulename /= M#module.modulename ->
            {_, _, SubPos, _} = SubM#module.stmt,
            add_error(Ctx1, SubPos, 'YANG_ERR_BAD_BELONGS_TO',
                      [M#module.name, SubM#module.name]);
       true ->
            Ctx1
    end.

%% check that each submodule included by this submodule is also
%% included by the module
v_all_includes(M, Ctx) when M#module.kind == 'module' ->
    MySubmodules = [SubM || {SubM, _Pos} <- M#module.submodules],
    lists:foldl(
      fun(#module{submodules = SubSubmodules}, Ctx0) ->
              lists:foldl(
                fun({SubM, Pos}, Ctx1) ->
                        case lists:member(SubM, MySubmodules) of
                            false ->
                                add_error(Ctx1, Pos, 'YANG_ERR_MISSING_INCLUDE',
                                          [SubM#module.name, M#module.name]);
                            true ->
                                Ctx1
                        end
                end, Ctx0, SubSubmodules)
      end, Ctx, MySubmodules);
v_all_includes(_SubM, Ctx) ->
    Ctx.

%% Pre: meta stmts are handled
%% Handle revision stmts
%% Note: M#module.revision is already set; see get_latest_revision().
parse_revision(Stmts, M, Ctx) ->
    parse_revision(Stmts, M, Ctx, undefined).

parse_revision([{'revision', Arg, Pos, _Substmts} | T], M, Ctx, PrevRevision) ->
    if Arg > PrevRevision, PrevRevision /= undefined ->
            Ctx1 = add_error(Ctx, Pos, 'YANG_ERR_REVISION_ORDER', []),
            parse_revision(T, M, Ctx1, Arg);
       true ->
            parse_revision(T, M, Ctx, Arg)
    end;
parse_revision([{Kwd, _Arg, _, _} = _H | T] = Stmts, M, Ctx, PrevRevision) ->
    case kwd_class(Kwd, Ctx) of
        extension ->
            case get_extension_handler(Kwd, Ctx) of
%% avoid dialyzer complaint
%                {true, HandlerMod} ->
%                    {M1, Ctx1} = HandlerMod:parse_revision(H, M, Ctx),
%                    parse_revision(T, M1, Ctx1, PrevRevision);
                false ->
                    parse_revision(T, M, Ctx, PrevRevision)
            end;
        _ ->
            parse_body(Stmts, M, Ctx)
    end;
parse_revision([], M, Ctx, _PrevRevision) ->
    parse_body([], M, Ctx).

%% Pre: revision stmts are handled
%% Handle body stmts
%% DEBUG yanger_ncs - uncomment next two lines
%parse_body(Stmts, M0, Ctx0) when Stmts /= x ->
%    {Ctx0, M0};
parse_body(Stmts, M0, Ctx0) ->
    M1 = M0#module{xpath_ns_map = yang_xpath:mk_ns_map(M0)},
    OrigHooks = Ctx0#yctx.hooks,
    Ctx1 = update_conditional_hooks(Ctx0, M1),
    %% Scan all stmts and build the maps for typedefs etc, that
    %% later will be used in the other stmts.
    {M2, Ctx2} = mk_module_maps(Stmts, M1, Ctx1),
    %% Add all schema nodes from all submodules
    SubChildren = get_children_from_submodules(M2),
    %% Create schema node records for all children.
    {Children00, XAcc, _, Ctx3} =
        mk_children(Stmts, undefined,
                    M2#module.typedefs, M2#module.groupings,
                    undefined, undefined, M2,
                    _IsInGrouping = false, Ctx2,
                    _Mode = final, _Ancestors = [M2],
                    _Acc = SubChildren, []),
    Children0 = prune_status(Ctx3,
                             fun prune_sn_status/2,
                             Children00),
    {ModKW, ModArg, ModPos, ModSubs} = M2#module.stmt,
    Conformance = get_conformance(M2#module.modulename, Ctx3),
    Identities = prune_status(Ctx3,
                              fun prune_identities_status/2,
                              M2#module.identities),
    M3 = M2#module{stmt = {ModKW, ModArg, ModPos, ModSubs ++ XAcc},
                   conformance = Conformance,
                   identities = Identities},
    %% Build the augments list.
    {Augments, Ctx4} =
        mk_augments(Stmts, M3#module.typedefs, M2#module.groupings, M3, Ctx3,
                    _IsTopLevel = true, []),
    {LocalAugments0, RemoteAugments00} =
        sort_augments(Augments, M3#module.modulename),
    LocalAugments = prune_status(Ctx4,
                                 fun prune_augment_status/2,
                                 LocalAugments0),
    RemoteAugments0 =
        prune_status(Ctx4,
                     fun prune_remote_augment_status/2,
                     RemoteAugments00),

    %% Apply local augments and augments from the submodules
    SavedHooks = Ctx4#yctx.hooks,
    {Children1, UndefAugNodes1, Ctx7} =
        lists:foldl(
          fun({SM, _}, {Children2, UndefAugNodes0, Ctx5}) ->
                  %% Ensure that the hooks are properly set for the submodule
                  Ctx6 =
                      update_conditional_hooks(Ctx5#yctx{hooks = OrigHooks},
                                               SM),
                  augment_children(SM#module.local_augments,
                                   Children2, UndefAugNodes0,
                                   final, [M3], M3, Ctx6)
          end, {Children0, [], Ctx4}, M3#module.submodules),
    {AugmentedChildren, UndefAugNodes2, Ctx8} =
        augment_children(LocalAugments, Children1, UndefAugNodes1,
                         final, [M3], M3, Ctx7),
    Ctx9 = report_undef_augment_nodes(UndefAugNodes2,
                                      Ctx8#yctx{hooks = SavedHooks}),

    %% Validate that all children have unique names
    Ctx10 = v_unique_names(AugmentedChildren, Ctx9),
    %% Build the deviations list
    %% Track imports used only in deviations separately, to allow plugins
    %% to record dependencies based on the combination of #module.imports,
    %% #module.unused_imports, and #module.deviation_imports
    PreDeviationsUnused = Ctx10#yctx.unused_imports,
    {Deviations, Ctx11} = mk_deviations(Stmts, M3, Ctx10, []),
    PostDeviationsUnused = Ctx11#yctx.unused_imports,
    Ctx12 = Ctx11#yctx{unused_imports = PreDeviationsUnused},
    {LocalDeviations, RemoteDeviations} =
        sort_deviations(Deviations, M3#module.modulename),
    SubModuleDeviations =
        lists:concat([SM#module.local_deviations ||
                         {SM,_} <- M3#module.submodules]),
    %% Apply local deviations and deviations from the submodules
    %% Do the validation even if not actually applying
    {DeviatedChildren, Ignored, Ctx13} =
        deviate_children(SubModuleDeviations ++ LocalDeviations,
                         AugmentedChildren, [], [M3], Ctx12),
    M4 = M3#module{local_deviations = LocalDeviations,
                   remote_deviations = RemoteDeviations},
    M5 = if Conformance == import ->
                 M4#module{children = [],
                           ignored = M4#module.children ++ Ignored ++
                               DeviatedChildren,
                           local_augments = []};
            Ctx9#yctx.apply_deviations ->
                 M4#module{children = DeviatedChildren,
                           ignored = Ignored,
                           local_augments = LocalAugments};
            true ->
                 M4#module{children = AugmentedChildren,
                           local_augments = LocalAugments}
         end,
    %% Apply all remote augments
    {Ctx15, M6} =
        if Conformance == import ->
                {Ctx13, M5#module{remote_augments = []}};
           true ->
                {Ctx14, RemoteAugments1} =
                    apply_remote_augments(RemoteAugments0, M5, Ctx13, []),
                RemoteAugments2 =
                    lists:usort(
                      RemoteAugments1 ++
                          lists:append(
                            [SM#module.remote_augments ||
                                {SM, _Pos} <- M5#module.submodules])),
                {Ctx14, M5#module{remote_augments = RemoteAugments2}}
         end,
    %% Update the module in the context before applying remote deviations,
    %% to allow the deviations to reference definitions from this module
    ModRevs = map_update({M6#module.name, M6#module.revision}, M6,
                          Ctx15#yctx.modrevs),
    Ctx16 = Ctx15#yctx{modrevs = ModRevs},
    %% Apply all remote deviations - after remote augments
    Ctx17 = apply_remote_deviations(RemoteDeviations, M6, [], Ctx16),
    %% Imports that are in #yctx.unused_imports but not in
    %% PostDeviationsUnused have been used only for deviations
    {_, MyPostDeviationsUnused} =
        lists:keyfind(M6#module.name, 1, PostDeviationsUnused),
    {value, {ModuleName, MyUnused}, RemUnused} =
        lists:keytake(M6#module.name, 1, Ctx17#yctx.unused_imports),
    MyDeviationsOnly = MyUnused -- MyPostDeviationsUnused,
    Unused = [{ModuleName, MyUnused -- MyDeviationsOnly} | RemUnused],
    DeviationImports =
        [{ModuleName, MyDeviationsOnly} | Ctx17#yctx.deviation_imports],
    Ctx18 = Ctx17#yctx{unused_imports = Unused,
                       deviation_imports = DeviationImports,
                       hooks = OrigHooks},
    {Ctx18, M6}.

get_conformance(ModName, #yctx{conformances = ConfL}) ->
    case lists:keyfind(ModName, 1, ConfL) of
        {_ModName, Conformance} ->
            Conformance;
        _ ->
            case lists:member(import, ConfL) of
                true ->
                    import;
                false ->
                    implement
            end
    end.

-spec get_imported_module(ModuleName :: atom(), #module{}, #yctx{}) ->
        {value, #module{}} | none.
get_imported_module(ModuleName, #module{imports = Imports}, Ctx) ->
    case lists:keyfind(ModuleName, 1, Imports) of
        {_ModuleName, Revision, _Prefix, _RequestedRevision} ->
            map_lookup({ModuleName, Revision}, Ctx#yctx.modrevs);
        false ->
            none
    end.

-spec get_grouping(Name :: yang_identifier(), #module{} | undefined, #yctx{}) ->
        {value, #grouping{}} | none.
%% @doc M can be undefined only if Name is on the form {ModName,LocalName}
get_grouping(Name, M, Ctx) ->
    case Name of
        {ModName, LocalName} ->
            case get_module(ModName, undefined, Ctx) of
                {value, M1} ->
                    grouping_lookup(LocalName, M1#module.groupings);
                none ->
                    none
            end;
        LocalName when M /= undefined ->
            grouping_lookup(LocalName, M#module.groupings)
    end.

-spec get_typedef(Name :: yang_identifier(), #module{} | undefined, #yctx{}) ->
        {value, #typedef{}} | none.
%% @doc M can be undefined only if Name is on the form {ModName,LocalName}
get_typedef(Name, M, Ctx) ->
    case Name of
        {ModName, LocalName} ->
            case get_module(ModName, undefined, Ctx) of
                {value, M1} ->
                    typedef_lookup(LocalName, M1#module.typedefs);
                none ->
                    none
            end;
        LocalName when M /= undefined ->
            typedef_lookup(LocalName, M#module.typedefs)
    end.

apply_remote_augments([{TargetModuleName, Augments0} | T], M, Ctx0, Acc) ->
    %% Find the imported revision for this modulename.
    case get_imported_module(TargetModuleName, M, Ctx0) of
        {value, TargetM} ->
            %% Update the the augment tree with names on the form
            %% {ModuleName, Name}.  And update the target_node
            %% identifier so that it matches the target schema node
            %% tree; i.e., nodes in the target module have just a
            %% name, and other nodes have {ModuleName, Name}.
            Augments1 =
                lists:map(
                  fun(#augment{target_node = TargetNode0,
                               children = Children0} = A) ->
                          TargetNode1 = patch_target(TargetNode0,
                                                     M#module.modulename,
                                                     TargetModuleName),
                          {Children1, TargetSn} =
                              case get_schema_node(TargetNode1, TargetM) of
                                  {true, #sn{kind = 'choice'} = Sn, _} ->
                                      {mk_choice_children_from_shorthand(
                                         M, Children0, Sn#sn.config,
                                         M#module.groupings,
                                         M#module.typedefs), Sn};
                                  {true, Sn, _} ->
                                      {Children0, Sn};
                                  _ ->
                                      {Children0, #sn{}}
                              end,
                          Children2 = set_module_name_and_config(
                                        Ctx0, Children1,
                                        M#module.modulename, [TargetSn]),
                          A#augment{target_node = TargetNode1,
                                    children = Children2}
                  end, Augments0),
            %% Perform the augment
            {AugmentedChildren, Ctx1} =
                augment_children(Augments1,
                                 TargetM#module.children,
                                 final, [TargetM], M, Ctx0),
            %% Update the module in the context
            TargetModRev = {TargetModuleName, TargetM#module.revision},
            TargetM1 = TargetM#module{children = AugmentedChildren},
            ModRevs = map_update(TargetModRev, TargetM1, Ctx1#yctx.modrevs),
            Ctx2 = Ctx1#yctx{modrevs = ModRevs},
            apply_remote_augments(T, M, Ctx2,
                                  [{TargetModuleName, Augments1} |
                                   Acc]);
        none ->
            %% Some error with the target module; already reported.
            apply_remote_augments(T, M, Ctx0, Acc)
    end;
apply_remote_augments([], _M, Ctx, Acc) ->
    {Ctx, Acc}.

set_module_name_and_config(Ctx,
                           [#sn{name = Name, children = Children} = Sn0 | T],
                           ModuleName, Ancestors) ->
    {_, Sn1} = pre_mk_sn_config(Ctx, Sn0, final, undefined, Ancestors),
    [Sn1#sn{name = {ModuleName, Name},
            children = set_module_name_and_config(Ctx, Children, ModuleName,
                                                  [Sn1 | Ancestors])} |
     set_module_name_and_config(Ctx, T, ModuleName, Ancestors)];
set_module_name_and_config(_, [], _, _) ->
    [].

patch_target([{child, {TargetModuleName, NodeName}} | T],
             AugmentingModuleName, TargetModuleName) ->
    [{child, NodeName} |
     patch_target(T, AugmentingModuleName, TargetModuleName)];
patch_target([{child, {_M,_N}} = H | T], AugmentingModuleName,
             TargetModuleName) ->
    [H | patch_target(T, AugmentingModuleName, TargetModuleName)];
patch_target([{child, '*'} | T], AugmentingModuleName, TargetModuleName) ->
    [{child, '*'} |
     patch_target(T, AugmentingModuleName, TargetModuleName)];
patch_target([{child, H} | T], AugmentingModuleName, TargetModuleName) ->
    [{child, {AugmentingModuleName, H}} |
     patch_target(T, AugmentingModuleName, TargetModuleName)];
patch_target([], _, _) ->
    [].

update_conditional_hooks(#yctx{hooks = Hooks} = Ctx, Module) ->
    FilterF = conditional_hook_filter(Ctx, Module),
    PreMkSn = lists:zf(FilterF, Hooks#hooks.pre_mk_sn),
    PostMkSn = lists:zf(FilterF, Hooks#hooks.post_mk_sn),
    PostExpandModule = lists:zf(FilterF, Hooks#hooks.post_expand_module),
    PostExpandSn = lists:zf(FilterF, Hooks#hooks.post_expand_sn),
    Ctx#yctx{hooks = Hooks#hooks{pre_mk_sn = PreMkSn,
                                 post_mk_sn = PostMkSn,
                                 post_expand_module = PostExpandModule,
                                 post_expand_sn = PostExpandSn}}.

conditional_hook_filter(#yctx{primary_module_names = Primary},
                        #module{name = Name,
                                modulename = ModuleName,
                                imports = Imports}) ->
    IsPrimary = lists:member(Name, Primary) orelse
        lists:member(ModuleName, Primary),
    %% Should we add a condition 'once' or something that is true only
    %% when the (sub)module is initially handled; i.e., children from
    %% included submodules should NOT be called?
    fun({Conditions, F}) ->
            check_conditions(Conditions, IsPrimary, Imports, F);
       (_) ->
            %% unconditional hook
            true
    end.

check_conditions([primary | T], IsPrimary, Imports, F) ->
    if IsPrimary ->
            check_conditions(T, IsPrimary, Imports, F);
       true ->
            false
    end;
check_conditions([{imports_any, ImpL} | T], IsPrimary, Imports, F) ->
    case lists:any(fun (Imp) -> lists:keymember(Imp, 1, Imports) end, ImpL) of
        true ->
            check_conditions(T, IsPrimary, Imports, F);
        false ->
            false
    end;
check_conditions([{imports, Imp} | T], IsPrimary, Imports, F) ->
    case lists:keymember(Imp, 1, Imports) of
        true ->
            check_conditions(T, IsPrimary, Imports, F);
        false ->
            false
    end;
check_conditions([], _, _, F) ->
    {true, F}.

mk_module_maps(Stmts, M, Ctx0) ->
    mk_module_maps(Stmts, map_new(), map_new(), map_new(),
                   map_new(), map_new(), M, Ctx0).

mk_module_maps([{Kwd, Arg, _, Substmts} = S | Stmts],
               Ts, Gs, Is, Fs, Es, M, Ctx0) ->
    MRef = {M#module.modulename, M#module.modulerevision},
    %% Build maps with non-validated items.  These are validated below.
    Status = get_stmts_arg(Substmts, 'status', 'current'),
    case Kwd of
        'typedef' ->
            T = #typedef{name = Arg, stmt = S, status = Status,
                         moduleref = MRef, is_top_level = true},
            {Ts1, Ctx1} =
                add_to_definitions_map(Arg, T, #typedef.stmt, Ts, Ctx0),
            mk_module_maps(Stmts, Ts1, Gs, Is, Fs, Es, M, Ctx1);
        'grouping' ->
            G = #grouping{name = Arg, stmt = S, status = Status,
                          moduleref = MRef, is_top_level = true},
            {Gs1, Ctx1} =
                add_to_definitions_map(Arg, G, #grouping.stmt, Gs, Ctx0),
            mk_module_maps(Stmts, Ts, Gs1, Is, Fs, Es, M, Ctx1);
        'identity' ->
            I = #identity{name = Arg, stmt = S, status = Status,
                          moduleref = MRef},
            {Is1, Ctx1} =
                add_to_definitions_map(Arg, I, #identity.stmt, Is, Ctx0),
            mk_module_maps(Stmts, Ts, Gs, Is1, Fs, Es, M, Ctx1);
        'feature' ->
            F = #feature{name = Arg, stmt = S, status = Status, moduleref = MRef},
            {Fs1, Ctx1} =
                add_to_definitions_map(Arg, F, #feature.stmt, Fs, Ctx0),
            mk_module_maps(Stmts, Ts, Gs, Is, Fs1, Es, M, Ctx1);
        'extension' ->
            ExtArg =
                case search_one_stmt('argument', Substmts) of
                    {_, ArgName, _, _} ->
                        ArgName;
                    false ->
                        []
                end,
            E = #extension{name = Arg, stmt = S, moduleref = MRef,
                           arg = ExtArg},
            {Es1, Ctx1} =
                add_to_definitions_map(Arg, E, #extension.stmt, Es, Ctx0),
            mk_module_maps(Stmts, Ts, Gs, Is, Fs, Es1, M, Ctx1);
        _ ->
            mk_module_maps(Stmts, Ts, Gs, Is, Fs, Es, M, Ctx0)
    end;
mk_module_maps([], Ts, Gs, Is, Fs, Es, M0, Ctx0) ->
    %% Start with the map with non-validated items.  Copy the
    %% submodules' definitions into the map, so they are known to our
    %% definitions.  Then validate our definitions, and update the
    %% map.
    {FsMap0, Ctx1} =
        add_from_submodules(fun(SubM) -> SubM#module.features end,
                            fun(Item) -> Item#feature.stmt end, M0, Fs, Ctx0),
    {FsMap1, Ctx2} = mk_features_map(Fs, FsMap0, M0, Ctx1),
    M1 = M0#module{features = FsMap1},


    {IsMap0, Ctx3} =
        add_from_submodules(fun(SubM) -> SubM#module.identities end,
                            fun(Item) -> Item#identity.stmt end, M1, Is, Ctx2),
    {IsMap1, Ctx4} = mk_identities_map(Is, IsMap0, M1, Ctx3),
    M2 = M1#module{identities = IsMap1},

    {EsMap1, Ctx5} =
        add_from_submodules(fun(SubM) -> SubM#module.extensions end,
                            fun(Item) -> Item#extension.stmt end, M2, Es, Ctx4),
    %% For extensions, we don't need to do any extra processing
    M3 = M2#module{extensions = EsMap1},

    {TsMap0, Ctx6} =
        add_from_submodules(fun(SubM) -> SubM#module.typedefs#typedefs.map end,
                            fun(Item) -> Item#typedef.stmt end, M3, Ts, Ctx5),
    %% identityref typedefs need the identities map
    {Typedefs, Ctx7} = mk_typedefs(Ts, TsMap0, undefined, M3, Ctx6),
    M4 = M3#module{typedefs = Typedefs},

    {GsMap0, Ctx8} =
        add_from_submodules(
          fun(SubM) -> SubM#module.groupings#groupings.map end,
          fun(Item) -> Item#grouping.stmt end, M4, Gs, Ctx7),
    {Groupings, Ctx9} = mk_groupings(Gs, GsMap0, Typedefs, undefined, M4, Ctx8),

    % only groupings needs ot be added here, all other are already replaced.
    {M4#module{groupings = Groupings}, Ctx9}.

add_to_definitions_map(Name, Item, StmtPos, Map, Ctx) ->
    try
        {map_insert(Name, Item, Map), Ctx}
    catch
        _:_ ->
            {Keyword, Name, Pos, _} = element(StmtPos, Item),
            {Keyword, Name, OtherPos, _} = element(StmtPos, map_get(Name, Map)),
            {Map, add_dup_error(Ctx, Keyword, Name, Pos, OtherPos)}
    end.

add_dup_error(Ctx, Keyword, Name, NewPos, PrevPos) ->
    add_error(Ctx, NewPos, 'YANG_ERR_DUPLICATE_DEFINITION',
              [yang_error:fmt_keyword(Keyword), Name,
               yang_error:fmt_pos(PrevPos)]).

add_dup_sn_error(Ctx, Name, NewPos, PrevPos) ->
    add_error(Ctx, NewPos, 'YANG_ERR_DUPLICATE_SCHEMA_NODE',
              [local_name(Name), yang_error:fmt_pos(PrevPos)]).

add_from_submodules(GetMapF, GetStmtF, M, RawMap, Ctx0) ->
    %% First, create a new map with all items from all submodules
    {Map1, Ctx1} =
        lists:foldl(
          fun({SubM, _Pos}, {Map2, Ctx2}) ->
                  SubMap = GetMapF(SubM),
                  map_foldl(
                    fun(SubName, SubItem, {Map3, Ctx3}) ->
                            try
                                case map_lookup(SubName, Map3) of
                                    {value, SubItem} ->
                                        %% exactly this definition is already
                                        %% added (from another include)
                                        {Map3, Ctx3};
                                    _ ->
                                        {map_insert(SubName, SubItem, Map3),
                                         Ctx3}
                                end
                            catch
                                _:_ ->
                                    {value, Prev} = map_lookup(SubName, Map3),
                                    {Keyword, SubName, NewPos, _} =
                                        GetStmtF(SubItem),
                                    {Keyword, SubName, PrevPos, _} =
                                        GetStmtF(Prev),
                                    Ctx4 = add_dup_error(Ctx3, Keyword, SubName,
                                                         NewPos, PrevPos),
                                    {Map3, Ctx4}
                            end
                    end, {Map2, Ctx2}, SubMap)
          end, {map_new(), Ctx0}, M#module.submodules),
    %% Add the (raw, non-validated) items defined in our module, and
    %% check for errors.
    map_foldl(
      fun(Name, Raw, {Map2, Ctx2}) ->
              try
                  {map_insert(Name, Raw, Map2), Ctx2}
              catch
                  _:_ ->
                      {value, Prev} = map_lookup(Name, Map2),
                      {Keyword, Name, NewPos, _} = GetStmtF(Raw),
                      {Keyword, Name, PrevPos, _} = GetStmtF(Prev),
                      Ctx3 =
                          add_dup_error(Ctx2, Keyword, Name, NewPos, PrevPos),
                      {Map2, Ctx3}
              end
      end, {Map1, Ctx1}, RawMap).


%% Post: all #identity.v_status /= undefined
%% Detects cycles and references to undefined definitions.
mk_identities_map(RawMap, ResMap, M, Ctx) ->
    %% Loop over all identity names, and update Map for each
    %% identity found.
    mk_identities_map0(map_iterator(RawMap), ResMap, M, Ctx).

mk_identities_map0(Iter0, Map0, M, Ctx0) ->
    case map_next(Iter0) of
        {Name, _Id, Iter1} ->
            {value, Id} = map_lookup(Name, Map0),
            {Map1, Ctx1} = add_identity(Id, Map0, M, Ctx0),
            mk_identities_map0(Iter1, Map1, M, Ctx1);
        none ->
            {Map0, Ctx0}
    end.

add_identity(Id0, Map0, M, Ctx0) ->
    {_, Name, Pos, Substmts} = Id0#identity.stmt,
    case Id0#identity.v_status of
        done ->
            %% this statement is already handled
            {Map0, Ctx0};
        processing ->
            Ctx1 = add_error(Ctx0, Pos, 'YANG_ERR_CIRCULAR_DEPENDENCY',
                             ['identity', Name]),
            {Map0, Ctx1};
        undefined ->
            %% unhandled statement
            {FeatureL, Ctx1} =
                lists:foldl(
                  fun({'if-feature', Arg, IFPos, _} = Stmt, {Acc, Ctx01}) ->
                          case yang:get_feature_expr(Arg, IFPos, M, Ctx01) of
                              {undefined, Ctx02} ->
                                  {Acc, Ctx02};
                              {Expr, Ctx02} ->
                                  {[{Expr, local, Stmt} | Acc], Ctx02}
                          end;
                     (_, Acc) ->
                          Acc
                  end, {[], Ctx0}, Substmts),
            case search_all_stmts('base', Substmts) of
                [_, {_, _, BPos, _} | _] when M#module.yang_version == '1' ->
                    {Ctx1,
                     add_error(Ctx1, BPos, 'YANG_ERR_MULTIPLE_BASES', [])};
                Bases ->
                    Map1 = map_update(Name, Id0#identity{v_status = processing},
                                      Map0),
                    {Map3, Ctx2, IdRefs1} =
                        lists:foldl(
                          fun(BaseStmt, {Map2, Ctx01, IdRefs0}) ->
                                  chk_base(BaseStmt, Map2, M, Ctx01, IdRefs0)
                          end,
                          {Map1, Ctx1, []},
                          Bases),
                    {IfFeatureRes, Ctx3} = check_if_features(FeatureL, M, Ctx2),
                    Id1 =
                        Id0#identity{bases = IdRefs1, v_status = done,
                                     if_feature_result = IfFeatureRes == true},
                    Map4 = map_update(Name, Id1, Map3),
                    {Map4, Ctx3}
            end
    end.

%% Will add to the map only if non-validated identities are found.
%% If called after mk_identities_map(), the map will not be modified.
chk_base({_, BaseArg, Pos, _}, Map0, M, Ctx0, Bases) ->
    case get_identity(BaseArg, Pos, Map0, M, Ctx0) of
        {BaseIdentity0, Ctx1} when is_record(BaseIdentity0, identity) ->
            {Map1, Ctx2} = add_identity(BaseIdentity0, Map0, M, Ctx1),
            {BaseIdentity, _} = get_identity(BaseArg, Pos, Map1, M, Ctx0),
            {Map1, Ctx2, [BaseIdentity | Bases]};
        {undefined, Ctx1} ->
            {Map0, Ctx1, Bases}
    end.

mk_features_map(RawMap, ResMap, M, Ctx) ->
    %% Loop over all feature names, and update Map for each
    %% feature found.
    mk_features_map0(map_iterator(RawMap), ResMap, M, Ctx).

mk_features_map0(Iter0, Map0, M, Ctx0) ->
    case map_next(Iter0) of
        {_Name, F, Iter1} ->
            {Map1, Ctx1} = add_feature(F, Map0, M, Ctx0),
            mk_features_map0(Iter1, Map1, M, Ctx1);
        none ->
            {Map0, Ctx0}
    end.

add_feature(F0, Map0, M, Ctx0) ->
    {_, Name, Pos, Substmts} = F0#feature.stmt,
    case F0#feature.v_status of
        done ->
            %% this statement is already handled
            {Map0, Ctx0};
        processing ->
            Ctx1 = add_error(Ctx0, Pos, 'YANG_ERR_CIRCULAR_DEPENDENCY',
                             ['feature', Name]),
            {Map0, Ctx1};
        undefined ->
            %% unhandled statement
            Map1 = map_update(Name, F0#feature{v_status = processing}, Map0),
            Status = get_stmts_arg(Substmts, 'status', 'current'),
            {Map2, Ctx1} =
                iterate_stmt(
                  fun(RefStmt, {Map, Ctx}) ->
                          {continue, chk_if_feature(RefStmt, Map, M, Status, Ctx)}
                  end, 'if-feature', {Map1, Ctx0}, Substmts),
            F1 = F0#feature{v_status = done},
            Map3 = map_update(Name, F1, Map2),
            {Map3, Ctx1}
    end.

chk_if_feature({_, RefArg, Pos, _}, Map0, M, Status, Ctx0) ->
    case
        get_feature_expr0(RefArg, Pos, M#module{features = Map0}, Ctx0,
                         _DoResolve = false)
    of
        {undefined, Ctx1} ->
            {Map0, Ctx1, undefined};
        {Expr, Ctx1} ->
            Fs = yang_if_feature:get_features(Expr),
            lists:foldl(
              fun(Ref, {Map1, Ctx2}) ->
                      case get_feature(Ref, Pos, Map1, M, Ctx2) of
                          {Feature, Ctx3} when is_record(Feature, feature) ->
                              case resolve_raw_idref(Ref, Pos, M, Ctx2) of
                                  {self, _, _} ->
                                      RefStatus = Feature#feature.status,
                                      Ctx4 =
                                          chk_status(Status, RefStatus, 'feature', 'feature',
                                                     Pos, Ctx3),
                                          add_feature(Feature, Map1, M, Ctx4);
                                  _ ->
                                      add_feature(Feature, Map1, M, Ctx3)
                              end;
                          {undefined, Ctx3} ->
                              {Map1, Ctx3}
                      end
              end, {Map0, Ctx1}, Fs)
    end.

-spec get_feature(raw_identifier_ref(), pos(), Features :: map0(),
                  #module{}, #yctx{}) ->
        {#feature{} | undefined, #yctx{}}.
%% @doc Search for a #feature with a given name.
%% Adds an error on failure.
get_feature(RawRef, Pos, Features, M, Ctx) ->
    get_definition(RawRef, Pos, Features, M, Ctx,
                   fun(ImpM) -> ImpM#module.features end, 'feature').

-spec get_feature_expr(binary(), pos(), #module{}, #yctx{}) ->
        {yang_if_feature:expr() | undefined, #yctx{}}.
%% @doc Search for a #feature with a given name.
%% Adds an error on failure.
get_feature_expr(Str, Pos, M, Ctx0) ->
    get_feature_expr0(Str, Pos, M, Ctx0, true).
get_feature_expr0(Str, Pos, M, Ctx0, DoResolve) ->
    %% syntax is already checked by the C parser; this call will not fail
    {true, Expr0} = yang_if_feature:parse(?b2l(Str)),
    if DoResolve ->
            yang_if_feature:resolve_prefixes(Expr0, Pos, M, Ctx0);
       true ->
            {Expr0, Ctx0}
    end.

-spec get_identity(raw_identifier_ref(), pos(), Identities :: map0(),
                   #module{}, #yctx{}) ->
        {#identity{} | undefined, #yctx{}}.
%% @doc Search for a #identity with a given name.
%% Adds an error on failure.
get_identity(RawRef, Pos, Identites, M, Ctx) ->
    get_definition(RawRef, Pos, Identites, M, Ctx,
                   fun(ImpM) -> ImpM#module.identities end, 'identity').

%% FIXME: unused function
%% -spec get_extension(raw_identifier_ref(), pos(), Extensions :: map(),
%%                     PrefixMap :: map(), #yctx{}) ->
%%         {#extension{} | undefined, #yctx{}}.
%% %% @doc Search for a #extension with a given name.
%% %% Adds an error on failure.
%% get_extension(RawRef, Pos, Extensions, PrefixMap, Ctx) ->
%%     get_definition(RawRef, Pos, Extensions, PrefixMap, Ctx,
%%                    fun(M) -> M#module.extensions end, 'extension').

get_definition(RawRef, Pos, DefinitionMap, M, Ctx0, GetMapF, Kind) ->
    case resolve_raw_idref(RawRef, Pos, M, Ctx0) of
        {self, RefName, Ctx1} ->
            %% reference to local definition
            get_definition2(RefName, Pos, DefinitionMap, Ctx1, Kind);
        {{imported, RefM}, RefName, Ctx1} ->
            get_definition2({RefM#module.name, RefName},
                            Pos, GetMapF(RefM), Ctx1, Kind);
        {undefined, _RefName, Ctx1} ->
            {undefined, Ctx1}
    end.

get_definition2(DefinitionYangIdentifier, Pos, DefinitionMap, Ctx0, Kind) ->
    DefinitionName = local_name(DefinitionYangIdentifier),
    case map_lookup(DefinitionName, DefinitionMap) of
        {value, Definition} ->
            {Definition, Ctx0};
        none ->
            Ctx1 =
                add_error(Ctx0, Pos, 'YANG_ERR_DEFINITION_NOT_FOUND',
                          [Kind, yang_error:fmt_yang_identifier(
                                   DefinitionYangIdentifier)]),
            {undefined, Ctx1}
    end.

-spec mk_typedefs_and_groupings([stmt()], #typedefs{}, #groupings{},
                                #module{}, boolean(), #yctx{}) ->
          {#typedefs{}, #groupings{}, #yctx{}}.
mk_typedefs_and_groupings(Stmts, ParentTypedefs, ParentGroupings, M,
                          IsInGrouping, Ctx0) ->
    MRef = {M#module.modulename, M#module.modulerevision},
    {TsMap, GsMap, Ctx1} =
        mk_typedefs_and_groupings0(Stmts, map_new(), map_new(), MRef,
                                   IsInGrouping, Ctx0),
    {Typedefs, Ctx2} =
        mk_typedefs(TsMap, TsMap, ParentTypedefs, M, Ctx1),
    {Groupings, Ctx3} =
        mk_groupings(GsMap, GsMap, Typedefs, ParentGroupings, M, Ctx2),
    {Typedefs, Groupings, Ctx3}.

mk_typedefs_and_groupings0([{Kwd, Arg, _, Substmts} = S | Stmts],
                           Ts, Gs, MRef, IsInGrouping, Ctx0) ->
    %% Build maps with non-validated items.  These are validated
    %% in mk_typedefs() and mk_groupings().
    Status = get_stmts_arg(Substmts, 'status', 'current'),
    case Kwd of
        'typedef' ->
            T = #typedef{name = Arg, stmt = S, v_status = undefined,
                         status = Status, is_top_level = false,
                         moduleref = MRef, is_in_grouping = IsInGrouping},
            {Ts1, Ctx1} =
                add_to_definitions_map(Arg, T, #typedef.stmt, Ts, Ctx0),
            mk_typedefs_and_groupings0(Stmts, Ts1, Gs, MRef, IsInGrouping,Ctx1);
        'grouping' ->
            G = #grouping{name = Arg, stmt = S, v_status = undefined,
                          is_top_level = false,
                          status = Status,  moduleref = MRef},
            {Gs1, Ctx1} =
                add_to_definitions_map(Arg, G, #grouping.stmt, Gs, Ctx0),
            mk_typedefs_and_groupings0(Stmts, Ts, Gs1, MRef, IsInGrouping,Ctx1);
        _ ->
            mk_typedefs_and_groupings0(Stmts, Ts, Gs, MRef, IsInGrouping, Ctx0)
    end;
mk_typedefs_and_groupings0([], Ts, Gs, _MRef, _IsInGrouping, Ctx) ->
    {Ts, Gs, Ctx}.

mk_typedefs(FromMap, Map, ParentTypedefs, M, Ctx) ->
    %% Now, loop over the map and validate each definition, and check
    %% for circular definitions.
    mk_typedefs0(map_iterator(FromMap), Map, ParentTypedefs, M, Ctx).

mk_typedefs0(Iter0, Map0, ParentTypedefs, M, Ctx0) ->
    case map_next(Iter0) of
        {Name, _Typedef, Iter1} ->
            %% Look up the typedef in the result map, not in the input
            %% map, in order to check v_status.
            {value, Typedef} = map_lookup(Name, Map0),
            Ctx1 =
                case typedef_lookup(Name, ParentTypedefs) of
                    {value, OtherTypedef} ->
                        add_dup_error(Ctx0, 'typedef', Name,
                                      stmt_pos(Typedef#typedef.stmt),
                                      stmt_pos(OtherTypedef#typedef.stmt));
                    none ->
                        Ctx0
                end,
            {Map1, Ctx2} = add_typedef(Typedef, Map0, ParentTypedefs, M, Ctx1),
            mk_typedefs0(Iter1, Map1, ParentTypedefs, M, Ctx2);
        none ->
            case map_is_empty(Map0) of
                true when ParentTypedefs /= undefined ->
                    {ParentTypedefs#typedefs{same_as_parent = true}, Ctx0};
                _ ->
                    {#typedefs{map = Map0, parent = ParentTypedefs}, Ctx0}
            end
    end.

add_typedef(T0, Map0, ParentTypedefs, M, Ctx0) ->
    {_, Name, Pos, Substmts} = T0#typedef.stmt,
    case T0#typedef.v_status of
        done ->
            %% this statement is already handled
            {Map0, Ctx0};
        processing ->
            Ctx1 = add_error(Ctx0, Pos, 'YANG_ERR_CIRCULAR_DEPENDENCY',
                             ['typedef', Name]),
            {Map0, Ctx1};
        undefined ->
            %% unhandled statement
            Map1 = map_update(Name, T0#typedef{v_status = processing}, Map0),
            TypeStmt = search_one_stmt('type', Substmts),
            TypedefName = {M#module.modulename, Name},
            Status = get_stmts_arg(Substmts, 'status', 'current'),

            {Map2, Ctx1, Type} =
                mk_type(TypeStmt, Map1, ParentTypedefs, M, TypedefName,
                        'typedef', Status, Ctx0),
            DefaultStmt = search_one_stmt('default', Substmts),
            {Default, Ctx2} = yang_types:mk_default(DefaultStmt, Type, M, Ctx1),
            MRef = {M#module.modulename, M#module.modulerevision},
            T1 = T0#typedef{type = Type, default = Default, moduleref = MRef,
                            v_status = done},
            {Ctx3, T2} = run_hooks(#hooks.mk_typedef, Ctx2, T1, M),
            Map3 = map_update(Name, T2, Map2),
            {Map3, Ctx3}
    end.

%% If called outside of a typedef, TypedefMap is 'undefined'
mk_type(Stmt, TypedefMap, ParentTypedefs, M, TypedefName,
        Keyword, Status, Ctx0) ->
    {_, TypeArg, Pos, Substmts} = Stmt,
    Map0 = TypedefMap,
    %% First, figure out our base type name
    IsBuiltinType = is_builtin_type(TypeArg),
    {Map3, Ctx4, BaseTypeRef} =
        case IsBuiltinType of
            true ->
                {Map0, Ctx0, TypeArg};
            false ->
                case resolve_raw_idref(TypeArg, Pos, M, Ctx0) of
                    {self, TypeName, Ctx1} ->
                        %% reference to local definition
                        case
                            Map0 == undefined orelse map_lookup(TypeName, Map0)
                        of
                            {value, RefTypedef} ->
                                {Map1, Ctx2} =
                                    add_typedef(RefTypedef, Map0,
                                                ParentTypedefs, M, Ctx1),
                                HandledRefTypedef = map_get(TypeName, Map1),
                                Ctx3 =
                                    chk_status(Status,
                                               HandledRefTypedef#typedef.status,
                                               Keyword, 'typedef',
                                               Pos, Ctx2),
                                {Map1, Ctx3, HandledRefTypedef};
                            _ -> % true or none
                                {Ctx2, RefTypedef} =
                                    mk_type_from_typedef(TypeName,
                                                         ParentTypedefs,
                                                         Pos, Ctx1),
                                Ctx3 =
                                    if RefTypedef /= undefined ->
                                            chk_status(
                                              Status,
                                              RefTypedef#typedef.status,
                                              Keyword, 'typedef',
                                              Pos, Ctx2);
                                       true ->
                                            Ctx2
                                    end,
                                {Map0, Ctx3, RefTypedef}
                        end;
                    {{imported, RefM}, TypeName, Ctx1} ->
                        {Ctx2, RefTypedef} =
                            mk_type_from_typedef(
                              {RefM#module.name, TypeName},
                              RefM#module.typedefs,
                              Pos, Ctx1),
                        {Map0, Ctx2, RefTypedef};
                    {undefined, _BaseName, Ctx1} ->
                        %% could not resolve prefix; error is already reported
                        {Map0, Ctx1, undefined}
                end
        end,
    %% union requires special treatment, since it contains nested types.
    {Map4, Ctx5, BaseTypeArg} =
        case BaseTypeRef of
            'union' ->
                lists:foldl(
                  fun(S, {MapN, CtxN, Types}) ->
                          {MapN1, CtxN1, Type} =
                              mk_type(S, MapN, ParentTypedefs, M,
                                      undefined, Keyword, Status, CtxN),
                          {MapN1, CtxN1, Types ++ [Type]}
                  end,
                  {Map3, Ctx4, []},
                  search_all_stmts('type', Substmts));
            _ when IsBuiltinType ->
                {value, BuiltinType} = yang_types:lookup_type(TypeArg, Ctx0),
                {Map3, Ctx4, BuiltinType};
            _ ->
                {Map3, Ctx4, BaseTypeRef}
        end,
    %% Then, scan for restrictions, and make sure they are allowed,
    %% and build a new type.

    Type0 = #type{base = BaseTypeRef, stmt = Stmt},
    {Ctx8, Type1} =
        if BaseTypeRef /= undefined ->
                {TypeSpecF0, TypeSpec0, Ctx6} =
                    yang_types:mk_type_spec(Stmt, BaseTypeArg, M, Ctx5),
                %% Check for "override" by plugin
                %% - *after* validating the typedef!
                case TypedefName /= undefined andalso
                    yang_types:lookup_type(TypedefName, Ctx6) of
                    {value, RegType} ->
                        {TypeSpecF, TypeSpec, Ctx7} =
                            yang_types:mk_type_spec(Stmt, RegType, M, Ctx6),
                        {Ctx7, Type0#type{base = TypedefName,
                                          type_spec_fun = TypeSpecF,
                                          type_spec = TypeSpec}};
                    _ -> % false or none
                        {Ctx6, Type0#type{type_spec_fun = TypeSpecF0,
                                          type_spec = TypeSpec0}}
                end;
           true ->
                {Ctx5, Type0}
        end,
    {Ctx9, Type2} = run_hooks(#hooks.mk_type, Ctx8, Type1, M),
    {Map4, Ctx9, Type2}.

mk_type_from_typedef(TypeYangIdentifier, Typedefs, Pos, Ctx) ->
    case typedef_lookup(local_name(TypeYangIdentifier), Typedefs) of
        {value, RefTypedef} ->
            {Ctx, RefTypedef};
        none ->
            Ctx1 =
                add_error(
                  Ctx, Pos,
                  'YANG_ERR_DEFINITION_NOT_FOUND',
                  ['typedef',
                   yang_error:fmt_yang_identifier(TypeYangIdentifier)]),
            {Ctx1, undefined}
    end.

is_builtin_type(Type) ->
    case Type of
        'binary' -> true;
        'bits' -> true;
        'boolean' -> true;
        'decimal64' -> true;
        'empty' -> true;
        'enumeration' -> true;
        'identityref' -> true;
        'instance-identifier' -> true;
        'int8' -> true;
        'int16' -> true;
        'int32' -> true;
        'int64' -> true;
        'leafref' -> true;
        'string' -> true;
        'uint8' -> true;
        'uint16' -> true;
        'uint32' -> true;
        'uint64' -> true;
        'union' -> true;
        _ ->
            false
    end.

mk_groupings(FromMap, Map, Typedefs, ParentGroupings, M, Ctx) ->
    %% Now, loop over the map and validate each definition, and check
    %% for circular definitions.
    %% Add the groupings in dependency order, so that all groupings
    %% are handled when they are used.
    Order = mk_grouping_order(map_iterator(FromMap), FromMap, [], [], M, Ctx),
    mk_groupings0(Order, Map, Typedefs, ParentGroupings,
                  M, Ctx).

mk_grouping_order(Iter0, Map, Acc0, RawOrder, M, Ctx) ->
    case map_next(Iter0) of
        {Name, Grouping, Iter1} ->
            Substmts = stmt_substmts(Grouping#grouping.stmt),
            Acc1 =
                case grouping_deps(Substmts, Map, M, Ctx) of
                    [] ->
                        [{Name, '$dummy'} | Acc0];
                    Deps ->
                        [{Dep, Name} || Dep <- Deps] ++ Acc0
                end,
            mk_grouping_order(Iter1, Map, Acc1, [Name | RawOrder], M, Ctx);
        none ->
            case topo_sort(Acc0) of
                {ok, Order} ->
                    lists:delete('$dummy', Order);
                _cycle ->
                    RawOrder
            end
    end.

grouping_deps(Grouping, Map, M, Ctx) ->
    grouping_deps(Grouping, [], Map, M, Ctx).
grouping_deps([{Kwd, Arg, Pos, Substmts} | T], Deps0, Map, M, Ctx) ->
    case kwd_body_class(Kwd, Ctx) of
        'uses' ->
            case resolve_raw_idref(Arg, Pos, M, Ctx) of
                {self, GroupingName, _Ctx} ->
                    Deps1 =
                        case map_is_key(GroupingName, Map) of
                            true ->
                                case lists:member(GroupingName, Deps0) of
                                    true ->
                                        Deps0;
                                    false ->
                                        [GroupingName | Deps0]
                                end;
                            false ->
                                %% this is not a sibling grouping
                                Deps0
                        end,
                    grouping_deps(T, Deps1, Map, M, Ctx);
                _ ->
                    grouping_deps(T, Deps0, Map, M, Ctx)
            end;
        _ ->
            Deps1 = grouping_deps(Substmts, Deps0, Map, M, Ctx),
            grouping_deps(T, Deps1, Map, M, Ctx)
    end;
grouping_deps([], Deps, _, _, _) ->
    Deps.

mk_groupings0([Name | T], Map0, Typedefs, ParentGroupings, M, Ctx0) ->
    %% Look up the grouping in the result map, not in the
    %% input map, in order to check v_status.
    {value, Grouping} = map_lookup(Name, Map0),
    Ctx1 =
        case grouping_lookup(Name, ParentGroupings) of
            {value, OtherGrouping} ->
                add_dup_error(Ctx0, 'grouping', Name,
                              stmt_pos(Grouping#grouping.stmt),
                              stmt_pos(OtherGrouping#grouping.stmt));
            none ->
                Ctx0
        end,
    MyGroupings = #groupings{map = Map0, parent = ParentGroupings},
    {Map1, Ctx2} =
        add_grouping(Grouping, Map0, Typedefs, MyGroupings,
                     M, Ctx1),
    mk_groupings0(T, Map1, Typedefs, ParentGroupings, M, Ctx2);
mk_groupings0([], Map0, _, ParentGroupings, _M, Ctx0) ->
    case map_is_empty(Map0) of
        true when ParentGroupings /= undefined ->
            {ParentGroupings#groupings{same_as_parent = true}, Ctx0};
        _ ->
            {#groupings{map = Map0, parent = ParentGroupings}, Ctx0}
    end.

add_grouping(G0, Map0, ParentTypedefs, ParentGroupings, M, Ctx0) ->
    {Kwd, Name, Pos, Substmts} = G0#grouping.stmt,
    case G0#grouping.v_status of
        done ->
            %% this statement is already handled
            {Map0, Ctx0};
        processing ->
            Ctx1 = add_error(Ctx0, Pos, 'YANG_ERR_CIRCULAR_DEPENDENCY',
                             ['grouping', Name]),
            {Map0, Ctx1};
        undefined ->
            %% unhandled statement
            Map1 = map_update(Name, G0#grouping{v_status = processing}, Map0),
            {Children, XSubstmts, Map2, Typedefs, Groupings, Ctx1} =
                mk_grouping_children(Substmts, Map1,
                                     ParentTypedefs, ParentGroupings,
                                     M, Ctx0),
            G1 = G0#grouping{v_status = done,
                             typedefs = Typedefs,
                             groupings = Groupings,
                             children = Children,
                             stmt = {Kwd, Name, Pos, Substmts ++ XSubstmts}},
            {Ctx2, G2} = run_hooks(#hooks.mk_grouping, Ctx1, G1, M),
            Map3 = map_update(Name, G2, Map2),
            {Map3, Ctx2}
    end.

get_children_from_submodules(M) ->
    %% Add children from one submodule at the time.  If we find a
    %% child that we have already added, we ignore it.  This child
    %% might have been augmented by the submodule we already
    %% processed, thus it is correct to ignore it.
    lists:foldl(
      fun({SubM, _}, Acc) ->
              lists:foldl(
                fun(Ch, Acc1) ->
                        case
                            lists:keymember(Ch#sn.name, #sn.name, Acc)
                        of
                            true ->
                                Acc1;
                            false ->
                                [Ch | Acc1]
                        end
                end, Acc, SubM#module.children)
      end, [], M#module.submodules).

mk_grouping_children(Stmts, GroupingMap, ParentTypedefs, ParentGroupings,
                     M, Ctx0) ->
    {Typedefs, Groupings, Ctx1} =
        mk_typedefs_and_groupings(Stmts, ParentTypedefs, ParentGroupings,
                                  M, _IsInGrouping = true, Ctx0),
    {Children, XSubstmts, GroupingMap1, Ctx2} =
        mk_children(Stmts, GroupingMap, Typedefs, Groupings,
                    ParentTypedefs, ParentGroupings,
                    M, _IsInGrouping = true, Ctx1,
                    _Mode = grouping, _Ancestors = [], _Acc = [], _XAcc = []),
    {Children, XSubstmts, GroupingMap1, Typedefs, Groupings, Ctx2}.

%% GroupingMap0 is 'undefined' iff called outside of a grouping definition
mk_children([{Kwd, Arg, Pos, Substmts} = Stmt | T], GroupingMap0,
            Typedefs, Groupings,
            ParentTypedefs, ParentGroupings,
            M, IsInGrouping, Ctx0, Mode, Ancestors, Acc, XAcc) ->
    case kwd_body_class(Kwd, Ctx0) of
        'uses' ->
            case resolve_raw_idref(Arg, Pos, M, Ctx0) of
                {self, GroupingName, Ctx1} ->
                    %% reference to local definition
                    case
                        GroupingMap0 == undefined orelse
                        map_lookup(GroupingName, GroupingMap0)
                    of
                        {value, G0} ->
                            %% make sure the grouping is added
                            {GroupingMap1, Ctx2} =
                                add_grouping(G0, GroupingMap0,
                                             ParentTypedefs, ParentGroupings,
                                             M, Ctx1),
                            %% find it again (the properly added grouping)
                            case map_lookup(GroupingName, GroupingMap1) of
                                {value, G1} ->
                                    Status = get_stmts_arg(Substmts, 'status',
                                                           'current'),
                                    Ctx3 = chk_status(Status,
                                                      G1#grouping.status,
                                                      'uses', 'grouping',
                                                      Pos, Ctx2),
                                    mk_children_uses(G1, Stmt, GroupingMap1,
                                                     Typedefs, Groupings,
                                                     M, IsInGrouping, Ctx3,
                                                     Mode, Ancestors, T,
                                                     Acc, XAcc);
                                none ->
                                    %% This means add_grouping
                                    %% returned an error, do not add
                                    %% another one.
                                    mk_children(T, GroupingMap1,
                                                Typedefs, Groupings,
                                                ParentTypedefs, ParentGroupings,
                                                M, IsInGrouping, Ctx2,
                                                Mode, Ancestors, Acc, XAcc)
                            end;
                        _ -> % true or none
                            case grouping_lookup(GroupingName, Groupings) of
                                {value, G} ->
                                    Status = get_stmts_arg(Substmts, 'status',
                                                           'current'),
                                    Ctx2 = chk_status(Status,
                                                      G#grouping.status,
                                                      'uses', 'grouping',
                                                      Pos, Ctx1),
                                    mk_children_uses(G, Stmt, GroupingMap0,
                                                     Typedefs, Groupings,
                                                     M, IsInGrouping, Ctx2,
                                                     Mode, Ancestors, T,
                                                     Acc, XAcc);
                                none ->
                                    Ctx2 =
                                        add_error(
                                          Ctx1, Pos,
                                          'YANG_ERR_DEFINITION_NOT_FOUND',
                                          ['grouping',
                                           yang_error:fmt_yang_identifier(
                                             GroupingName)]),
                                    mk_children(T, GroupingMap0,
                                                Typedefs, Groupings,
                                                ParentTypedefs, ParentGroupings,
                                                M, IsInGrouping, Ctx2,
                                                Mode, Ancestors, Acc, XAcc)
                            end
                    end;
                {{imported, RefM}, GroupingName, Ctx1} ->
                    case grouping_lookup(GroupingName, RefM#module.groupings) of
                        {value, G} ->
                            mk_children_uses(G, Stmt, GroupingMap0,
                                             Typedefs, Groupings, M,
                                             IsInGrouping, Ctx1, Mode,
                                             Ancestors, T, Acc, XAcc);
                        none ->
                            Ctx2 =
                                add_error(
                                  Ctx1, Pos,
                                  'YANG_ERR_DEFINITION_NOT_FOUND',
                                  ['grouping',
                                   yang_error:fmt_yang_identifier(
                                     {RefM#module.name, GroupingName})]),
                            mk_children(T, GroupingMap0,
                                        Typedefs, Groupings,
                                        ParentTypedefs, ParentGroupings,
                                        M, IsInGrouping, Ctx2, Mode,
                                        Ancestors, Acc, XAcc)
                    end;
                {undefined, _GroupingName, Ctx1} ->
                    %% could not resolve prefix; error is already reported
                    mk_children(T, GroupingMap0, Typedefs, Groupings,
                                ParentTypedefs, ParentGroupings,
                                M, IsInGrouping, Ctx1, Mode, Ancestors,
                                Acc, XAcc)
            end;
        {data_definition_stmt, Kind} ->
            Name = if Kwd == 'input' orelse Kwd == 'output' ->
                           Kwd;
                      true ->
                           Arg
                   end,
            {FeatureL, WhenL, MustL, Status, Ctx1} =
                common_substmts(Substmts, 'local', M, Ctx0),
            {IfFeatureRes, Ctx1_1} = check_if_features(FeatureL, M, Ctx1),
            if IfFeatureRes == error ->
                    Ctx2 = Ctx1_1;
               true ->
                    Ctx2 = validate_features_status(FeatureL, M, Status, Ctx1_1, Kind)
            end,
            Sn0 = #sn{name = Name,
                      kind = Kind,
                      module = M,
                      typedefs = Typedefs,
                      groupings = Groupings,
                      if_feature = FeatureL,
                      if_feature_result = (IfFeatureRes == true),
                      'when' = WhenL,
                      must = MustL,
                      status = Status,
                      stmt = Stmt},
            if Kwd == 'leaf' orelse Kwd == 'leaf-list' ->
                    {Ctx3, Sn1} =
                        run_mk_sn_hooks(Ctx2, Sn0, #hooks.pre_mk_sn, Mode,
                                        undefined, Ancestors),
                    {Ctx4, Sn2} =
                        run_mk_sn_hooks(Ctx3, Sn1, #hooks.post_mk_sn, Mode,
                                        undefined, Ancestors),
                    mk_children(T, GroupingMap0, Typedefs, Groupings,
                                ParentTypedefs, ParentGroupings,
                                M, IsInGrouping, Ctx4,
                                Mode, Ancestors, [Sn2 | Acc], XAcc);
               true ->
                    {Typedefs1, Groupings1, Ctx3} =
                        if Kwd == 'choice' orelse Kwd == 'case' ->
                                %% No typedefs or groupings allowed
                                {Typedefs, Groupings, Ctx2};
                           true ->
                                mk_typedefs_and_groupings(Substmts,
                                                          Typedefs, Groupings,
                                                          M, IsInGrouping,
                                                          Ctx2)
                        end,
                    {Ctx4, Sn1} =
                        run_mk_sn_hooks(Ctx3, Sn0, #hooks.pre_mk_sn, Mode,
                                        undefined, Ancestors),
                    {SubChildren, XSubstmts, GroupingMap1, Ctx5} =
                        mk_children(Substmts, GroupingMap0,
                                    Typedefs1, Groupings1,
                                    Typedefs1, Groupings1,
                                    M, IsInGrouping, Ctx4, Mode,
                                    [Sn1 | Ancestors], [], []),
                    Sn2 = Sn1#sn{typedefs = Typedefs1,
                                 groupings = Groupings1,
                                 children = SubChildren,
                                 stmt = {Kwd, Arg, Pos, Substmts ++ XSubstmts}},
                    Sn3 = mk_case_from_shorthand(Sn2),
                    Ctx6 = v_input_output(Ctx5, Sn3, Ancestors),
                    Sn4 = mk_operation_default_children(Sn3),
                    {Ctx7, Sn5} =
                        run_mk_sn_hooks(Ctx6, Sn4, #hooks.post_mk_sn, Mode,
                                        undefined, Ancestors),
                    mk_children(T, GroupingMap1, Typedefs, Groupings,
                                ParentTypedefs, ParentGroupings,
                                M, IsInGrouping, Ctx7,
                                Mode, Ancestors, [Sn5 | Acc], XAcc)
            end;
        _ ->
            %% ignore other statements
            mk_children(T, GroupingMap0, Typedefs, Groupings,
                        ParentTypedefs, ParentGroupings,
                        M, IsInGrouping, Ctx0,
                        Mode, Ancestors, Acc, XAcc)
    end;
mk_children([], GroupingMap, _Typedefs, _Groupings, _, _, _M,
            _IsInGrouping, Ctx, _Mode, _Ancestors, Acc, XAcc) ->
    {lists:reverse(Acc), XAcc, GroupingMap, Ctx}.

mk_children_uses(Grouping, UsesStmt, GroupingMap, Typedefs, Groupings, M,
                 IsInGrouping, Ctx0, Mode, Ancestors, RestStmts, Acc, XAcc) ->
    {_, _, Pos, Substmts} = UsesStmt,
    {FeatureL, WhenL, [] = _MustL, _Status, Ctx1} =
        common_substmts(Substmts, 'uses', M, Ctx0),
    {IfFeatureRes, Ctx2} = check_if_features(FeatureL, M, Ctx1),
    GroupingChildren1 =
        if FeatureL /= [] orelse WhenL /= [] ->
                [Ch#sn{if_feature = FeatureL ++ Ch#sn.if_feature,
                       if_feature_result =
                           (IfFeatureRes == true)
                           andalso Ch#sn.if_feature_result,
                       'when' = WhenL ++ Ch#sn.'when'} ||
                    Ch <- Grouping#grouping.children];
           true ->
                Grouping#grouping.children
        end,
    {ExpandedChildren, Ctx3} =
        expand_uses(GroupingChildren1, Substmts,
                    Typedefs, Groupings, Pos, M, Ctx2, Mode, Ancestors),
    %% possibly copy stms from the grouping
    CopyMap = (Ctx3#yctx.env)#env.copy_from_grouping_stmts,
    CopySubstmts = [S || S <- stmt_substmts(Grouping#grouping.stmt),
                         map_is_key(stmt_keyword(S), CopyMap)],
    mk_children(RestStmts, GroupingMap,
                Typedefs, Groupings,
                Typedefs, Groupings, M,
                IsInGrouping, Ctx3, Mode, Ancestors,
                ExpandedChildren ++ Acc, XAcc ++ CopySubstmts).

expand_uses(GroupingChildren, UsesSubstmts, Typedefs, Groupings,
            UsesPos, M, Ctx, Mode, Ancestors) ->
    %% Build a tree of the refinements target paths.  As the grouping
    %% is expanded, walk down tree and apply refinements.  This way we
    %% refine as we expand, instead of first expand, and then apply
    %% each refinement in order.
    {RefTree, Ctx1} = mk_refinement_tree(UsesSubstmts, M, [], Ctx),
    {ExpandedChildren, Ctx2} =
        expand_uses2(lists:reverse(GroupingChildren), RefTree, M, Ctx1, []),
    %% Build the augments list.
    {Augments, Ctx3} =
        mk_augments(UsesSubstmts, Typedefs, Groupings, M, Ctx2,
                    _IsTopLevel = false, []),
    %% Apply the hook on the subtree
    {ExpandedChildren1, Ctx6} =
        lists:mapfoldl(
          fun(Ch, Ctx4) ->
                  {Ctx5, Ch1} =
                      run_mk_sn_hooks_rec(Ctx4, Ch, Mode, UsesPos, Ancestors),
                  {Ch1, Ctx5}
          end, Ctx3, ExpandedChildren),
    %% Apply the augments.
    {_AugmentedChildren, _Ctx7} =
        augment_children(Augments, ExpandedChildren1, Mode, Ancestors, M, Ctx6).

expand_uses2([Sn0 | T], RefTree0, M, Ctx0, Acc) ->
    %% Check if there are any refinements for this node
    {{Sn1, Ctx1}, Subtree, RefTree2} =
        case take_from_tree(Sn0#sn.name, RefTree0) of
            {value, Refs, Subtree0, RefTree1} ->
                %% apply the refinements on this node
                {apply_refinements(Refs, Sn0, M, Ctx0), Subtree0, RefTree1};
            false ->
                {{Sn0, Ctx0}, [], RefTree0}
        end,
    %% Expand children
    {ExpChildren, Ctx2} = expand_uses2(Sn1#sn.children, Subtree, M, Ctx1, []),
    Sn2 = Sn1#sn{children = ExpChildren, module = M},
    %% Expand siblings
    expand_uses2(T, RefTree2, M, Ctx2, Acc ++ [Sn2]);
expand_uses2([], RefTree, _M, Ctx0, Acc) ->
    %% Report errors if there are refinements left
    Ctx4 = iterate_tree(
             fun(Tag, Values, Ctx1) ->
                     Ctx2 =
                         lists:foldl(
                           fun({Pos, _}, Ctx3) ->
                                   add_error(Ctx3, Pos,
                                             'YANG_ERR_REFINE_NOT_FOUND',
                                             [yang_error:fmt_yang_identifier(
                                                Tag)])
                           end, Ctx1, Values),
                     {recurse, Ctx2}
             end, Ctx0, RefTree),
    {Acc, Ctx4}.

%% Augment 'Children' with the schema nodes from 'Augments'.
%% Return the augmented 'Children'.
augment_children(Augments, Children, Mode, Ancestors, M, Ctx0) ->
    {AugmentedChildren, UndefAugNodes, Ctx1} =
        augment_children(Augments, Children, [], Mode, Ancestors, M, Ctx0),
    Ctx2 = report_undef_augment_nodes(UndefAugNodes, Ctx1),
    {AugmentedChildren, Ctx2}.

augment_children(Augments, Children, UndefAugNodes, Mode, Ancestors, M, Ctx0) ->
    lists:foldl(
      fun(Augment, {AccChildren, UndefAugNodes00, Ctx00}) ->
              augment_children0(Augment#augment.target_node,
                                AccChildren, [], Mode, Ancestors,
                                Augment, UndefAugNodes00, M, Ctx00)
      end, {Children, UndefAugNodes, Ctx0}, Augments).

%% Report errors for all undefined augment nodes
report_undef_augment_nodes(UndefAugNodes, Ctx) ->
    lists:foldl(
      fun({Pos, Id}, Ctx1) ->
              add_error(Ctx1, Pos, 'YANG_ERR_NODE_NOT_FOUND',
                        [yang_error:fmt_yang_identifier(Id)])
      end, Ctx, UndefAugNodes).

augment_children0([], Sns, _Acc = [], Mode,
                  Ancestors, Augment, UndefAugNodes, _M, Ctx) ->
    %% We found the children to augment.
    %% Update the children with the augment's children, while looking for
    %% any tmp #sn{}.
    IsFinal = (Mode == final andalso
               Ancestors /= [] andalso
               not lists:keymember('__tmp_augment__', #sn.kind, Ancestors)),
    insert_children(Augment#augment.children, Sns, IsFinal, Ancestors,
                    UndefAugNodes, append, Ctx);
augment_children0([{child, Id} | Ids],
                  [#sn{name = Name, children = Children} = Sn | Sns],
                  Acc, Mode, Ancestors, Augment,
                  UndefAugNodes, M, Ctx)
  when Id == Name ->
    %% We found a node in the augment path; we must update it
    %% with new children.
    {AugmentedChildren, UndefAugNodes1, Ctx1} =
        augment_children0(Ids, Children, [], Mode,
                          [Sn | Ancestors], Augment, UndefAugNodes, M, Ctx),
    Sn1 = mk_case_from_shorthand(Sn#sn{children = AugmentedChildren}),
    Sn2 = if Ids == [] ->
                  %% This is the target node
                  Sn1#sn{augmented_by = [Augment | Sn1#sn.augmented_by]};
             true ->
                  Sn1
          end,
    {lists:reverse(Acc, [Sn2 | Sns]), UndefAugNodes1, Ctx1};
augment_children0(Ids, [Sn | Sns], Acc, Mode, Ancestors, Augment,
                  UndefAugNodes, M, Ctx) ->
    augment_children0(Ids, Sns, [Sn | Acc], Mode, Ancestors, Augment,
                      UndefAugNodes, M, Ctx);
augment_children0([{child, Id} | Ids], [], Acc, Mode, _Ancestors, Augment,
                  UndefAugNodes, M, Ctx0) ->
    %% We didn't find the node, create a tmp #sn{}, and keep track of it.
    %% At this point, we don't know the Ancestors
    AugPos = stmt_pos(Augment#augment.stmt),
    UndefAugNodes1 = [{AugPos, Id} | UndefAugNodes],
    {AugmentedChildren, UndefAugNodes2, Ctx1} =
        augment_children0(Ids, [], [], Mode, [],
                          Augment, UndefAugNodes1, M, Ctx0),
    {lists:reverse(Acc, [#sn{name = Id,
                             module = M,
                             kind = '__tmp_augment__',
                             stmt = {'__tmp_augment__', undefined, AugPos, []},
                             augmented_by = [Augment],
                             children = AugmentedChildren}]),
     UndefAugNodes2,
     Ctx1}.

%% Add NewSns after/before all ExistsingSns, but if a __tmp_augment__ is
%% found, we need to copy our children before the __tmp_augment__'s
%% children.
insert_children(NewSns, ExistsingSns0, IsFinal, Ancestors,
                UndefAugNodes0, How, Ctx0) ->
    lists:foldl(
      fun(NewSn, {ExistsingSns1, UndefAugNodes1, Ctx1}) ->
              insert_child(NewSn, ExistsingSns1, [], IsFinal, Ancestors,
                           UndefAugNodes1, How, Ctx1)
      end, {ExistsingSns0, UndefAugNodes0, Ctx0}, NewSns).

insert_child(NewSn0, [Sn | Sns], Acc, IsFinal, Ancestors, UndefAugNodes, _How,
             Ctx0)
  when NewSn0#sn.name == Sn#sn.name,
       Sn#sn.kind == '__tmp_augment__' ->
    %% We found a tmp node to replace.
    AugPos = sn_pos(Sn),
    UndefAugNodes1 = lists:keydelete(AugPos, 1, UndefAugNodes),
    NewSn1 = NewSn0#sn{augmented_by = NewSn0#sn.augmented_by ++
                           Sn#sn.augmented_by},
    if NewSn1#sn.kind == 'leaf'; NewSn1#sn.kind == 'leaf-list' ->
            %% The augment that produced the __tmp_augment__ is bad;
            %% it tried to augment a leaf or leaf-list
            {lists:reverse(Acc, [NewSn1 | Sns]),
             UndefAugNodes1,
             add_error(Ctx0, AugPos, 'YANG_ERR_BAD_AUGMENT_NODE_TYPE',
                       [NewSn1#sn.kind,
                        yang_error:fmt_yang_identifier(NewSn1#sn.name)])};
       true ->
            %% Do *not* run the hook when we insert these nodes; instead
            %% run it recursively after the insert.
            {NewChildren, UndefAugNodes2, Ctx1} =
                insert_children(lists:reverse(NewSn1#sn.children),
                                Sn#sn.children, false,
                                [NewSn1 | Ancestors], UndefAugNodes1, prepend,
                                Ctx0),
            NewSn2 = mk_case_from_shorthand(NewSn1#sn{children = NewChildren}),
            NewSn3 = NewSn2#sn{is_augment_top_node = true},
            {Ctx2, NewSn4} =
                if IsFinal ->
                        run_mk_sn_hooks_rec(Ctx1, NewSn3,
                                            final, undefined, Ancestors);
                   true ->
                        {Ctx1, NewSn3}
                end,
            {lists:reverse(Acc, [NewSn4 | Sns]), UndefAugNodes2, Ctx2}
    end;
insert_child(NewSn, [Sn | Sns], Acc, _IsFinal, _Ancestors, UndefAugNodes, _How,
             Ctx)
  when NewSn#sn.name == Sn#sn.name ->
    %% duplicate child found
    NewPos = sn_pos(NewSn),
    Pos = sn_pos(Sn),
    if NewPos == Pos ->
            %% this means that we found the same node twice; this happens
            %% when a node is augmented from a submodule which is included
            %% from other submodules.
            {lists:reverse(Acc, [Sn | Sns]), UndefAugNodes, Ctx};
       true ->
            %% report error
            {lists:reverse(Acc, [Sn | Sns]),
             UndefAugNodes,
             add_dup_sn_error(Ctx, Sn#sn.name, sn_pos(NewSn), sn_pos(Sn))}
    end;
insert_child(NewSn, [Sn | Sns], Acc, IsFinal, Ancestors, UndefAugNodes, How,
             Ctx) ->
    insert_child(NewSn, Sns, [Sn | Acc], IsFinal, Ancestors,
                 UndefAugNodes, How, Ctx);
insert_child(NewSn, [], Acc, IsFinal, Ancestors, UndefAugNodes, How, Ctx0) ->
    case Ancestors of
        [#sn{kind = Kind, name = Name} | _] ->
            ok;
        [] ->
            Kind = Name = undefined
    end,
    if Kind == 'leaf';
       Kind == 'leaf-list';
       NewSn#sn.kind == 'case'
       andalso (Kind /= 'choice'
                andalso Kind /= undefined
                andalso Kind /= '__tmp_augment__');
       NewSn#sn.kind == 'choice' andalso Kind == 'choice' ->
            Ctx1 = add_error(Ctx0, sn_pos(NewSn),
                             'YANG_ERR_BAD_AUGMENT_NODE_TYPE2',
                             [NewSn#sn.kind, Kind,
                              yang_error:fmt_yang_identifier(Name)]),
            {lists:reverse(Acc), UndefAugNodes, Ctx1};
       true ->
            %% insert this subtree at the end.  call the hook
            %% recursively for this subtree; the hook hasn't been
            %% called before.
            NewSn1 = mk_case_from_shorthand(NewSn),
            NewSn2 = NewSn1#sn{is_augment_top_node = true},
            {Ctx2, NewSn3} =
                if IsFinal ->
                        run_mk_sn_hooks_rec(Ctx0, NewSn2,
                                            final, undefined, Ancestors);
                   true ->
                        {Ctx0, NewSn2}
                end,
            case How of
                prepend ->
                    {[NewSn3 | lists:reverse(Acc)], UndefAugNodes, Ctx2};
                append ->
                    {lists:reverse(Acc, [NewSn3]), UndefAugNodes, Ctx2}
            end
    end.

check_if_features([], _, Ctx) ->
    {true, Ctx};
check_if_features(Features, M, #yctx{features = FMap} = Ctx) ->
    case validate_features_exist(Features, M, Ctx) of
        ok ->
            if FMap == none ->
                    {false, Ctx};
               true ->
                    {check_all_if_features(Features, FMap), Ctx}
            end;
        {error, _Ctx1} = Error ->
            Error
    end.

validate_features_status([{Expr, _, Stmt} | T], M, Status, Ctx, Kind) ->
    Ctx1 =
        lists:foldl(
          fun(Feature, Acc) ->
                  validate_feature_status(Feature, Stmt, M, Status, Acc, Kind)
          end, Ctx,
          yang_if_feature:get_features(Expr)),
    validate_features_status(T, M, Status, Ctx1, Kind);

validate_features_status([], _, _, Ctx1, _) ->
    Ctx1.

validate_feature_status({ModName, Name}, Stmt, M, Status, Ctx, Kind) ->
    if ModName == M#module.modulename ->
           Map = M#module.features,
           {value, Definition} = map_lookup(Name, Map),
           chk_status(Status, Definition#feature.status,
                      Kind, 'feature', stmt_pos(Stmt), Ctx);
       true ->
            Ctx
    end.

validate_features_exist(Features, M, Ctx) ->
    validate_features_exist0(Features, M, true, Ctx).

validate_features_exist0([{Expr, _, Stmt} | T], M, NoErrors0, Ctx0) ->
    {NoErrors1, Ctx1} =
        lists:foldl(
          fun(Feature, Acc) ->
                  validate_feature_exist(Feature, Stmt, M, Acc)
          end,
          {NoErrors0, Ctx0},
          yang_if_feature:get_features(Expr)),
    validate_features_exist0(T, M, NoErrors1, Ctx1);
validate_features_exist0([], _, true, _) ->
    ok;
validate_features_exist0([], _, false, Ctx1) ->
    {error, Ctx1}.

validate_feature_exist({ModName, Name} = Id, Stmt, M, {NoErrors, Ctx}) ->
    Map =
        if ModName == M#module.modulename ->
                M#module.features;
           true ->
                {value, ImpM} = get_imported_module(ModName, M, Ctx),
                ImpM#module.features
        end,
    case map_lookup(Name, Map) of
        {value, _} ->
            {NoErrors, Ctx};
        none ->
            {false,
             add_error(Ctx, stmt_pos(Stmt), 'YANG_ERR_DEFINITION_NOT_FOUND',
                       ['feature', yang_error:fmt_yang_identifier(Id)])}
    end.

check_all_if_features([{Expr, _, _} | T], FMap) ->
    case yang_if_feature:eval(Expr, FMap) of
        true ->
            check_all_if_features(T, FMap);
        false ->
            false
    end;
check_all_if_features([], _) ->
    true.

%% Ret: {FeatureL, WhenL, MustL, Status, Ctx}
common_substmts(Substmts, Origin, M, Ctx) ->
    common_substmts(Substmts, Origin, M, Ctx, [], [], [], 'current').

common_substmts([{'if-feature', Arg, Pos, _} = Stmt | T], Origin, M, Ctx0,
                FeatureL, WhenL, MustL, Status) ->
    case get_feature_expr(Arg, Pos, M, Ctx0) of
        {undefined, Ctx1} ->
            common_substmts(T, Origin, M, Ctx1, FeatureL, WhenL, MustL, Status);
        {Expr, Ctx1} ->
            common_substmts(T, Origin, M, Ctx1,
                            [{Expr, Origin, Stmt} | FeatureL], WhenL, MustL,
                            Status)
    end;
common_substmts([{'when', Arg, Pos, _} = Stmt | T], Origin, M, Ctx0,
                FeatureL, WhenL, MustL, Status) ->
    %% NOTE: Keep unprefixed names unprefixed, since they can be
    %% bound when a grouping is used.
    case yang_xpath:compile(Arg, Pos, M, Ctx0#yctx.strict, Ctx0) of
        {ok, CompiledXPath, Ctx1} ->
            common_substmts(T, Origin, M, Ctx1, FeatureL,
                            [{CompiledXPath, [], Origin, Stmt} |
                             WhenL],
                            MustL, Status);
        {error, Ctx1} ->
            common_substmts(T, Origin, M, Ctx1, FeatureL, WhenL, MustL, Status)
    end;
common_substmts([{'must', Arg, Pos, _} = Stmt | T], Origin, M, Ctx0,
                FeatureL, WhenL, MustL, Status) ->
    %% NOTE: Keep unprefixed names unprefixed, since they can be
    %% bound when a grouping is used.
    case yang_xpath:compile(Arg, Pos, M, Ctx0#yctx.strict, Ctx0) of
        {ok, CompiledXPath, Ctx1} ->
            common_substmts(T, Origin, M, Ctx1, FeatureL,
                            WhenL, [{CompiledXPath, [], Stmt} |
                                    MustL],
                            Status);
        {error, Ctx1} ->
            common_substmts(T, Origin, M, Ctx1, FeatureL, WhenL, MustL, Status)
    end;
common_substmts([{'status', Arg, _, _} | T], Origin, M, Ctx,
                FeatureL, WhenL, MustL, _Status) ->
    common_substmts(T, Origin, M, Ctx, FeatureL, WhenL, MustL, Arg);
common_substmts([_ | T], Origin, M, Ctx, FeatureL, WhenL, MustL, Status) ->
    common_substmts(T, Origin, M, Ctx, FeatureL, WhenL, MustL, Status);
common_substmts([], _Origin, _M, Ctx, FeatureL, WhenL, MustL, Status) ->
    {FeatureL, WhenL, MustL, Status, Ctx}.

run_mk_sn_hooks_rec(Ctx, #sn{if_feature_result = false} = Sn,
                    _Mode, _UsesPos, _Ancestors) ->
    {Ctx, Sn};
run_mk_sn_hooks_rec(Ctx0, Sn0, Mode, UsesPos, Ancestors0)
  when Sn0#sn.kind /= '__tmp_augment__' ->
    {Ctx1, Sn1} =
        run_mk_sn_hooks(Ctx0, Sn0, #hooks.pre_mk_sn, Mode, UsesPos, Ancestors0),
    Ancestors1 = [Sn1 | Ancestors0],
    {Children, Ctx4} =
        lists:mapfoldl(
          fun(Ch, Ctx2) ->
                  {Ctx3, Ch1} =
                      run_mk_sn_hooks_rec(Ctx2, Ch, Mode, UsesPos, Ancestors1),
                  {Ch1, Ctx3}
          end, Ctx1, Sn1#sn.children),
    run_mk_sn_hooks(Ctx4, Sn1#sn{children = Children}, #hooks.post_mk_sn,
                    Mode, UsesPos, Ancestors0);
run_mk_sn_hooks_rec(Ctx, Sn, _Mode, _UsesPos, _Ancestors) ->
    {Ctx, Sn}.

run_mk_sn_hooks(Ctx, #sn{if_feature_result = false} = Sn, _HookField,
                _Mode, _UsesPos, _Ancestors) ->
    {Ctx, Sn};
run_mk_sn_hooks(#yctx{hooks = Hooks} = Ctx0, Sn0, HookField,
                Mode, UsesPos, Ancestors) ->
    %% NOTE: foldr is important here in order to run the built-in hooks
    %% before the plugins'.
    lists:foldr(
      fun(HookF, {Ctx1, Sn1}) ->
              HookF(Ctx1, Sn1, Mode, UsesPos, Ancestors)
      end, {Ctx0, Sn0}, element(HookField, Hooks)).

%% This is a builtin pre_mk_sn hook.
%% When mode is 'final', rewrite xpath expressions so that unprefixed
%% names refer to current module.
pre_mk_sn_xpath(Ctx0, Sn = #sn{must = MustL0, 'when' = WhenL0,
                               kind = Kind, module = M},
                final, _UsesPos, _Ancestors)
  when MustL0 /= [] orelse WhenL0 /= [] ->
    {MustL1, Ctx1} =
        lists:foldl(
          fun({Q, _Deps, Stmt}, {Acc, Ctx0_0}) ->
                  %% _Deps is [] initially, but may be /= [] if we're doing
                  %% a deviation.
                  case
                      yang_xpath:set_default_namespace(Q, M#module.xpath_ns_map)
                  of
                      fail = CompiledXPath ->
                          Ctx0_1 = add_error(Ctx0_0, yang:stmt_pos(Stmt),
                                             'YANG_ERR_XPATH_FAIL', []),
                          {[{CompiledXPath, [], Stmt} | Acc], Ctx0_1};
                      CompiledXPath ->
                          Deps = yang_xpath:get_dep_paths(CompiledXPath,
                                                          M#module.modulename),
                          {[{CompiledXPath, Deps, Stmt} | Acc], Ctx0_0}
                  end
          end, {[], Ctx0}, MustL0),
    {WhenL1, Ctx2} =
        lists:foldl(
          fun({Q, _Deps, Origin, Stmt}, {Acc, Ctx1_0}) ->
                  %% _Deps is [] initially, but may be /= [] if we're doing
                  %% a deviation.
                  case
                      yang_xpath:set_default_namespace(Q, M#module.xpath_ns_map)
                  of
                      fail = CompiledXPath ->
                          Ctx1_1 = add_error(Ctx1_0, yang:stmt_pos(Stmt),
                                             'YANG_ERR_XPATH_FAIL', []),
                          {[{CompiledXPath, [], Origin, Stmt} | Acc],
                           Ctx1_1};
                      CompiledXPath ->
                          Deps0 =
                              yang_xpath:get_dep_paths(CompiledXPath,
                                                       M#module.modulename),
                          Deps =
                              if Origin /= 'local',
                                 Kind /= 'choice', Kind /= 'case',
                                 Deps0 /= false ->
                                      [add_parent(Dep) || Dep <- Deps0];
                                 true ->
                                      Deps0
                              end,
                          {[{CompiledXPath, Deps, Origin, Stmt} | Acc], Ctx1_0}
                  end
          end, {[], Ctx1}, WhenL0),
    {Ctx2, Sn#sn{must = MustL1, 'when' = WhenL1}};
pre_mk_sn_xpath(Ctx, Sn, _M, _, _) ->
    {Ctx, Sn}.

add_parent(['..' | _] = RelPath) ->
    ['..' | RelPath];
add_parent(['.' | T]) ->
    ['..' | T];
add_parent(AbsPath) ->
    AbsPath.

%% This is a builtin pre_mk_sn hook.
pre_mk_sn_config(Ctx, Sn, _Mode = augment, _, _) ->
    {Ctx, Sn};
pre_mk_sn_config(Ctx, #sn{kind = Kind} = Sn, _Mode, _UsesPos, _Ancestors)
  when Kind == 'operation'; Kind == 'notification' ->
    {Ctx, Sn#sn{config = ignore}};
pre_mk_sn_config(Ctx, Sn, _Mode, UsesPos, [#sn{config = ParentConfig} | _]) ->
    case search_one_substmt('config', Sn#sn.stmt) of
        {_, Config, Pos, _} ->
            ok;
        false ->
            %% By default, inherit from parent
            Config = ParentConfig,
            Pos = sn_pos(Sn)
    end,
    if ParentConfig == false,
       Config == true ->
            {add_error(Ctx, {uses, UsesPos, Pos},
                       'YANG_ERR_INVALID_CONFIG', []),
             Sn#sn{config = Config}};
       ParentConfig == ignore ->
            {Ctx, Sn#sn{config = ignore}};
       true ->
            {Ctx, Sn#sn{config = Config}}
    end;
pre_mk_sn_config(Ctx, Sn, _Mode, _UsesPos, Ancestors) ->
    case search_one_substmt('config', Sn#sn.stmt) of
        {_, Config, _, _} ->
            {Ctx, Sn#sn{config = Config}};
        false when Ancestors == [] ->
            {Ctx, Sn};
        false ->
            %% Ancestors = [#module{}]
            {Ctx, Sn#sn{config = true}}
    end.

%% This is a builtin post_mk_sn hook.
%% Will be called for the first definition and every uses.  Make sure
%% we don't re-calculate this if we've done it once; for this we
%% check if PrevType is 'undefined'.
%% A deviation that changes the type can set .type to 'undefined' in order
%% for this function to run again.
post_mk_sn_type(Ctx0,
                #sn{stmt = Stmt, kind = Kind, type = PrevType,
                    typedefs = Typedefs, status = Status, module = M} = Sn,
                _Mode, _UsesPos, _Ancestors)
  when (Kind == 'leaf' orelse Kind == 'leaf-list') andalso
       PrevType == undefined ->
    TypeStmt = search_one_substmt('type', Stmt),
    {undefined, Ctx1, Type} =
        mk_type(TypeStmt, undefined, Typedefs, M,
                undefined, Kind, Status, Ctx0),
    Ctx2 =
        if Kind == 'leaf-list',
           is_record(Type#type.type_spec, empty_type_spec) ->
                add_error(
                  Ctx1, stmt_pos(TypeStmt),
                  'YANG_ERR_TYPE_EMPTY_IN_LEAF_LIST',
                  []);
           true ->
                Ctx1
        end,
    %% Force reprocessing of default value in case when the type was changed
    {Ctx2, Sn#sn{type = Type, default = undefined}};
post_mk_sn_type(Ctx, Sn, _, _, _) ->
    {Ctx, Sn}.

%% This is a builtin post_mk_sn hook.
%% Will be called for the first definition and every uses.  Make sure
%% we don't re-calculate this if we've done it once and got an error.
post_mk_sn_v_default(Ctx0,
                     #sn{kind = 'leaf', type = Type, stmt = Stmt,
                         default = PrevDefault, module = M} = Sn,
                     _Mode, _UsesPos, _Ancestors)
  when PrevDefault /= invalid ->
    DefaultStmt = search_one_substmt('default', Stmt),
    Ctx1 = case search_one_substmt('mandatory', Stmt) of
               {_, true, _, _} when DefaultStmt /= false andalso
                                    (PrevDefault == undefined orelse
                                     element(1, PrevDefault) =/= DefaultStmt) ->
                   add_error(Ctx0, stmt_pos(DefaultStmt),
                             'YANG_ERR_DEFAULT_AND_MANDATORY', []);
               _ ->
                   Ctx0
           end,
    if PrevDefault == undefined ->
            {Default, Ctx2} = yang_types:mk_default(DefaultStmt, Type, M, Ctx1),
            {Ctx2, Sn#sn{default = Default}};
       true ->
            {Ctx1, Sn}
    end;
post_mk_sn_v_default(Ctx0,
                     #sn{kind = 'leaf-list', type = Type, stmt = Stmt,
                         default = PrevDefault, module = M,
                         config = Config} = Sn,
                     Mode, UsesPos, _Ancestors)
  when PrevDefault /= invalid ->
    if PrevDefault == undefined ->
            MinElems = get_substmt_arg(Stmt, 'min-elements', 0),
            {Default0, Ctx1} =
                case search_all_stmts('default', Stmt) of
                    DefaultStmts0 when MinElems =:= 0 ->
                        DefaultStmts = if DefaultStmts0 == [] ->
                                               %% try inherit from typedef
                                               [false];
                                          true ->
                                               DefaultStmts0
                                       end,
                        lists:foldr(
                          fun (DefaultStmt, {Defaults, Ctx_0}) ->
                                  {OneDefault, Ctx_1} =
                                      yang_types:mk_default(DefaultStmt, Type,
                                                            M, Ctx_0),
                                  {[OneDefault|Defaults], Ctx_1}
                          end, {[], Ctx0}, DefaultStmts);
                    [] ->
                        {[undefined], Ctx0};
                    DefaultStmts ->
                        {[invalid],
                         lists:foldr(
                           fun (DefaultStmt, Ctx_0) ->
                                   add_error(
                                     Ctx_0, stmt_pos(DefaultStmt),
                                     'YANG_ERR_DEFAULT_AND_MIN_ELEMENTS', [])
                           end, Ctx0, DefaultStmts)}
                end,
            {Default, Ctx2} =
                case Default0 of
                    [undefined] ->
                        %% no default found
                        {undefined, Ctx1};
                    _ ->
                        {Default0,
                         v_unique_defaults(Ctx1, Mode, Config,
                                           UsesPos, Default0)}
                end,
            {Ctx2, Sn#sn{default = Default}};
       is_list(PrevDefault) ->
            {v_unique_defaults(Ctx0, Mode, Config, UsesPos, PrevDefault), Sn};
       true ->
            {Ctx0, Sn}
    end;
post_mk_sn_v_default(Ctx, Sn, _, _, _) ->
    {Ctx, Sn}.

v_unique_defaults(Ctx0, final, true, UsesPos, Defaults) ->
    {_, Ctx1} =
        lists:foldl(
          fun (invalid, Acc) ->
                  %% bad value - ignore
                  Acc;
              ({_Stmt, undefined}, Acc) ->
                  %% leafref default - can't check values here
                  Acc;
              ({Stmt, Value} = Default, {DefAcc, Ctx_0}) ->
                  case lists:keyfind(Value, 2, DefAcc) of
                      {AccStmt, _} ->
                          {DefAcc,
                           add_error(Ctx_0, {uses, UsesPos, stmt_pos(Stmt)},
                                     'YANG_ERR_DUPLICATE_DEFAULT_VALUE',
                                     [yang_error:fmt_pos(stmt_pos(AccStmt))])};
                      false ->
                          {[Default|DefAcc], Ctx_0}
                  end
          end, {[], Ctx0}, Defaults),
    Ctx1;
v_unique_defaults(Ctx, _Mode, _Config, _UsesPos, _Defaults) ->
    Ctx.

%% This is a builtin post_mk_sn hook.
post_mk_sn_keys(Ctx0, #sn{kind = 'list', children = Children, stmt = Stmt,
                          keys = undefined, config = Config, module = M} = Sn,
                _Mode, _UsesPos, _Ancestors) ->
    %% Only do this the first time; i.e., when keys = undefined.
    case search_one_substmt('key', Stmt) of
        {_, KeyArg, KeyPos, _} ->
            KeyNames = re:split(KeyArg, "\\s+", [{return, binary}]),
            {Keys1, Ctx2} =
                lists:foldl(
                  fun(KeyName, {Keys0, Ctx1}) ->
                          case find_sn(?b2a(KeyName), Children) of
                              #sn{name = ChName, kind = 'leaf',
                                  type = Type, stmt = ChStmt,
                                  module = M} ->
                                  Ctx2 =
                                      if is_record(Type#type.type_spec,
                                                   empty_type_spec) ->
                                      %% FIXME: allow and handle for 1.1
                                      %% ,  M#module.yang_version == '1' ->
                                              add_error(
                                                Ctx1, KeyPos,
                                                'YANG_ERR_TYPE_EMPTY_IN_KEY',
                                                [KeyName]);
                                         true ->
                                              Ctx1
                                      end,
                                  case chk_key_substmts(ChStmt, M, Ctx2) of
                                      {true, Ctx3} ->
                                          {[ChName | Keys0], Ctx3};
                                      {false, Ctx3} ->
                                          {Keys0, Ctx3}
                                  end;
                              _ ->
                                  {Keys0,
                                   add_error(Ctx1, KeyPos, 'YANG_ERR_BAD_KEY',
                                             [KeyName])}
                          end
                  end, {[], Ctx0}, KeyNames),
            {Ctx2, Sn#sn{keys = lists:reverse(Keys1)}};
        false when Config == true ->
            {add_error(Ctx0, stmt_pos(Stmt), 'YANG_ERR_NEED_KEY', []), Sn};
        false ->
            {Ctx0, Sn#sn{keys = []}}
    end;
post_mk_sn_keys(Ctx, #sn{kind = 'list', stmt = Stmt,
                         keys = [], config = true} = Sn,
                final, UsesPos, _Ancestors) ->
    {add_error(Ctx, {uses, UsesPos, stmt_pos(Stmt)},
               'YANG_ERR_NEED_KEY_USES', []),
     Sn};
post_mk_sn_keys(Ctx, Sn, _Mode, _UsesPos, _Ancestors) ->
    {Ctx, Sn}.

chk_key_substmts(Stmt, M, Ctx0) ->
    BadSubstmts =
        if M#module.yang_version == '1' ->
                [];
           true ->
                ['when', 'if-feature']
        end,
    {Continue, Ctx2} =
        lists:foldl(
          fun(Keyword, {Continue0, Ctx1}) ->
                  case search_one_substmt(Keyword, Stmt) of
                      false ->
                          {Continue0, Ctx1};
                      SubStmt ->
                          {false,
                           add_error(Ctx1, stmt_pos(SubStmt),
                                     'YANG_ERR_KEY_BAD_SUBSTMT', [Keyword])}
                  end
          end,
          {true, Ctx0},
          BadSubstmts),
    if Continue ->
            case search_one_substmt('default', Stmt) of
                false ->
                    {true, Ctx2};
                DefaultStmt ->
                    {true,
                     add_error(
                       Ctx2, stmt_pos(DefaultStmt),
                       'YANG_ERR_KEY_HAS_DEFAULT', [])}
            end;
       true ->
            {false, Ctx2}
    end.

%% This is a builtin post_mk_sn hook.
post_mk_sn_unique(Ctx0,
                  Sn = #sn{kind = 'list', stmt = {_, _, _, Substms}},
                  final, _UsesPos, Ancestors) ->
    Ctx1 =
        lists:foldl(fun(Substmt, Ctx_0) ->
                            post_mk_sn_unique0(Substmt, Sn, Ctx_0, Ancestors)
                    end, Ctx0, search_all_stmts('unique', Substms)),
    {Ctx1, Sn};
post_mk_sn_unique(Ctx, Sn, _, _, _) ->
    {Ctx, Sn}.

post_mk_sn_unique0({'unique', Arg, Pos, _}, Sn, Ctx0, Ancestors) ->
    Leafs = re:split(Arg, "\\s+", [{return, binary}]),
    M = Sn#sn.module,
    Cursor = mk_cursor(Sn, Ancestors, Pos, M, schema, Ctx0),
    {_, LeafSns, Ctx1} =
        lists:foldl(
          fun(Leaf, Acc) ->
                  check_unique_path(Leaf, Pos, Cursor, M, Acc)
          end, {undefined, [], Ctx0}, Leafs),
    case Sn#sn.keys of
        undefined ->
            %% 'YANG_ERR_NEED_KEY' is already reported
            Ctx1;
        [] ->
            %% config false without keys, or
            %% all keys missing (already reported)
            Ctx1;
        Keys ->
            case Keys -- LeafSns of
                [] ->
                    add_error(Ctx0, Pos, 'YANG_ERR_UNIQUE_IS_KEY', []);
                _ ->
                    Ctx1
            end
    end.

check_unique_path(Leaf, Pos, Cursor, M, {IsConfig, Found, Ctx0}) ->
    case parse_descendant_schema_nodeid(Leaf, Pos, M, Ctx0) of
        {ok, SchemaNodeId, Ctx1} ->
            case follow_unique_path(SchemaNodeId, Cursor, Ctx1) of
                {true, #cursor{cur = #sn{kind = leaf, config = LeafConfig,
                                         name = Name} = Sn}} ->
                    Dup = lists:member(Sn, Found),
                    if Dup ->
                            {LeafConfig, Found,
                             add_error(Ctx1, Pos,
                                       'YANG_ERR_DUPLICATE_UNIQUE_LEAF',
                                       [yang_error:fmt_yang_identifier(Name)])};
                       LeafConfig == false,
                       IsConfig == true ->
                            {LeafConfig, Found,
                             add_error(Ctx1, Pos,
                                       'YANG_ERR_BAD_UNIQUE_CONFIG',
                                       [])};
                       true ->
                            {LeafConfig, [Name | Found], Ctx1}
                    end;
                {true, #cursor{cur = #sn{name = Name}}} ->
                    {IsConfig, Found,
                     add_error(Ctx1, Pos,
                               'YANG_ERR_BAD_UNIQUE_LEAF',
                               [yang_error:fmt_yang_identifier(Name)])};
                {false, Ctx2} ->
                    {IsConfig, Found, Ctx2}
            end;
        {error, Ctx1} ->
            {IsConfig, Found, Ctx1}
    end.

follow_unique_path([H | T], Cursor0, Ctx) ->
    case cursor_move(H, Cursor0, Ctx) of
        {true, Cursor1 = #cursor{cur = #sn{kind = Kind, name = Name}}} ->
            case lists:member(Kind, ['leaf', 'container', 'choice', 'case']) of
                true ->
                    follow_unique_path(T, Cursor1, Ctx);
                false ->
                    {false, add_error(Ctx, Cursor0#cursor.pos,
                                      'YANG_ERR_BAD_UNIQUE_PART',
                                      [yang_error:fmt_yang_identifier(Name),
                                       Kind])}
            end;
        {false, Error} ->
            {false, yang_error:add_error(Error, Ctx)}
    end;
follow_unique_path([], Cursor, _) ->
    {true, Cursor}.

find_sn(Name, [#sn{name = Name} = Sn | _]) ->
    Sn;
find_sn(Name, [#sn{name = {_ModuleName, Name}} = Sn | _]) ->
    Sn;
find_sn(Name, [_ | T]) ->
    find_sn(Name, T);
find_sn(_, []) ->
    false.

%% max status prune code follows
prune_status(#yctx{max_status = undefined}, _PruneFun, Items) ->
    Items;
prune_status(#yctx{max_status = MaxStatus}, PruneFun, Items) ->
    PruneFun(Items, n(MaxStatus)).

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
        map_foldl(
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
              map_delete(Key, Map)
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

%% This is a builtin post_mk_sn hook.
post_mk_sn_v_choice_default(Ctx0,
                            #sn{kind = 'choice', children = Children,
                                stmt = Stmt, default = undefined} = Sn,
                            _Mode, _UsesPos, _Ancestors) ->
    case search_one_substmt('default', Stmt) of
        {_, BinDefault, Pos, _} = DefaultStmt ->
            Ctx1 = case search_one_substmt('mandatory', Stmt) of
                       {_, true, _, _} ->
                           add_error(Ctx0, stmt_pos(Stmt),
                                     'YANG_ERR_DEFAULT_AND_MANDATORY', []);
                       _ ->
                           Ctx0
                   end,
            case
                re:run(BinDefault, ["^", identifier(), "$"],
                       [{capture, none}])
            of
                match ->
                    Default = ?b2a(BinDefault),
                    case
                        lists:keyfind(Default, #sn.name, Children)
                    of
                        #sn{children = DefCaseChildren} ->
                            Ctx2 = v_no_mandatory(
                                     DefCaseChildren,
                                     _PruneAugment = false,
                                     _PruneConfigFalse = false,
                                     'YANG_ERR_MANDATORY_NODE_IN_DEFAULT_CASE',
                                     Ctx1),
                            {Ctx2, Sn#sn{default = {DefaultStmt, Default}}};
                        false ->
                            Ctx2 = add_error(Ctx1, Pos,
                                             'YANG_ERR_DEFAULT_CASE_NOT_FOUND',
                                             [BinDefault]),
                            {Ctx2, Sn#sn{default = 'invalid'}}
                    end;
                nomatch ->
                    Ctx2 = add_error(Ctx1, Pos, 'YANG_ERR_BAD_ARGUMENT',
                                     [BinDefault, "identifier"]),
                    {Ctx2, Sn#sn{default = 'invalid'}}
            end;
        false ->
            {Ctx0, Sn}
    end;
post_mk_sn_v_choice_default(Ctx, Sn, _Mode, _UsesPos, _Ancestors) ->
    {Ctx, Sn}.

apply_refinements([{_Pos, Stmts} | T], Sn, M, Ctx) ->
    {Sn2, Ctx2} =
        lists:foldl(
          fun(Stmt, {Sn1, Ctx1}) ->
                  apply_refinement(Stmt, Sn1, M, Ctx1)
          end, {Sn, Ctx}, Stmts),
    apply_refinements(T, Sn2, M, Ctx2);
apply_refinements([], Sn, _M, Ctx) ->
    {Sn, Ctx}.

apply_refinement({Keyword, _Arg, Pos, _Substmts} = Stmt, Sn, M, Ctx) ->
    ParentKeyword = stmt_keyword(Sn#sn.stmt),
    case find_refinement(ParentKeyword, Keyword, Ctx) of
        {true, Op, F} ->
            RefinedSubstmts =
                case Op of
                    replace ->
                        lists:keystore(Keyword, ?STMT_KEYWORD,
                                       stmt_substmts(Sn#sn.stmt), Stmt);
                    add ->
                        stmt_substmts(Sn#sn.stmt) ++ [Stmt]
                end,
            RefinedStmt =
                setelement(?STMT_SUBSTMTS, Sn#sn.stmt, RefinedSubstmts),
            F(Stmt, M, {Sn#sn{stmt = RefinedStmt}, Ctx});
        false ->
            {Sn, add_error(Ctx, Pos, 'YANG_ERR_ILLEGAL_REFINE',
                           [yang_error:fmt_keyword(Keyword), ParentKeyword])}
    end.

%% The grammar specifies which statements are allowed to be refined.
%% If a certain keyword isn't allowed in 'refine' (according to the grammar),
%% we won't end up here.  This code checks that the keyword is allowed
%% in the parent/target node.
find_refinement(ParentKeyword, Keyword, #yctx{env = Env}) ->
    IdF = fun(_, _, SnAndCtx) -> SnAndCtx end,
    case yang_parser:get_statement_spec(ParentKeyword) of
        {value, {_, _, Rules, _}} ->
            case lists:keyfind(Keyword, 1, Rules) of
                {_, Occurence} ->
                    Op =
                        if Occurence == '?'; Occurence == '1' ->
                                replace;
                           true ->
                                add
                        end,
                    case map_lookup(Keyword, Env#env.refinements) of
                        {value, F} ->
                            {true, Op, F};
                        _ ->
                            {true, Op, IdF}
                    end;
                _ ->
                    %% We know the grammar of the parent.  If we also know
                    %% the grammar of the child, this is an illegal refinement.
                    ExtModules = Env#env.extension_modules,
                    case Keyword of
                        {OtherModule, _} ->
                            case lists:member(OtherModule, ExtModules) of
                                true ->
                                    false;
                                false ->
                                    {true, replace, IdF}
                            end
                    end
            end;
        _ ->
            %% We don't know the grammar, accept the refinement
            {true, replace, IdF}
    end.

add_refinements(Map0) ->
    Map1 = map_insert('default', fun reset_default/3, Map0),
    Map2 = map_insert('must', fun add_must/3, Map1),
    Map2.

reset_default(_Stmt, _, {Sn, Ctx}) ->
    {Sn#sn{default = undefined}, Ctx}.

add_must({'must', Arg, Pos, _} = Stmt, M,
         {#sn{must = MustL} = Sn, Ctx0}) ->
    case yang_xpath:compile(Arg, Pos, M, Ctx0#yctx.strict, Ctx0) of
        {ok, CompiledXPath, Ctx1} ->
            {Sn#sn{must = [{CompiledXPath, [], Stmt} | MustL]},
             Ctx1};
        {error, Ctx1} ->
            {Sn, Ctx1}
    end.

mk_refinement_tree([{'refine', Arg, Pos, Substmts} | T], M, Tree, Ctx) ->
    case parse_descendant_schema_nodeid(Arg, Pos, M, Ctx) of
        {ok, SchemaNodeId, Ctx1} ->
            Tree1 = add_to_tree(SchemaNodeId, {Pos, Substmts}, Tree),
            mk_refinement_tree(T, M, Tree1, Ctx1);
        {error, Ctx1} ->
            mk_refinement_tree(T, M, Tree, Ctx1)
    end;
mk_refinement_tree([_ | T], M, Tree, Ctx) ->
    mk_refinement_tree(T, M, Tree, Ctx);
mk_refinement_tree([], _, Tree, Ctx) ->
    {Tree, Ctx}.

-type tree() :: [{Id :: yang_identifier(), Values :: list(), [tree()]}].

-spec add_to_tree([{child, Id :: yang_identifier()}],
                  Value :: term(), tree()) -> tree().
add_to_tree([{child, Id}], Val, [{Id, Vs, Ch0} | T]) ->
    [{Id, [Val | Vs], Ch0} | T];
add_to_tree([{child, Id} | T2], Val, [{Id, Vs, Ch0} | T]) ->
    Ch1 = add_to_tree(T2, Val, Ch0),
    [{Id, Vs, Ch1} | T];
add_to_tree(L, Val, [H | T]) ->
    [H | add_to_tree(L, Val, T)];
add_to_tree(L, Val, []) ->
    mk_tree(L, Val).

mk_tree([{child, H}], Val) ->
    [{H, [Val], []}];
mk_tree([{child, H} | T], Val) ->
    [{H, [], mk_tree(T, Val)}].

-spec take_from_tree(Id :: yang_identifier(), tree()) ->
        {value, Values :: list(), Subtree :: tree(), RestTree :: tree()}
      | false.
take_from_tree(Id, Tree) ->
    take_from_tree0(Id, Tree, []).

take_from_tree0(Id, [{Id, Vs, Subtree} | T], Acc) ->
    {value, Vs, Subtree, lists:reverse(Acc, T)};
take_from_tree0(Id, [H | T], Acc) ->
    take_from_tree0(Id, T, [H | Acc]);
take_from_tree0(_Id, [], _Acc) ->
    false.

iterate_tree(F, S, Tree) ->
    {_, S1} = iterate_tree0(F, S, Tree),
    S1.

iterate_tree0(F, S, [{Id, Values, Subtree} | T]) ->
    case F(Id, Values, S) of
%% avoid dialyzer complaint
%        {stop, S1} ->
%            {stop, S1};
%        {continue, S1} ->
%            iterate_tree0(F, S1, T);
        {recurse, S1} ->
            case iterate_tree0(F, S1, Subtree) of
%                {stop, S2} ->
%                    {stop, S2};
                {_, S2} ->
                    iterate_tree0(F, S2, T)
            end
    end;
iterate_tree0(_, S, []) ->
    {recurse, S}.

%% RFC 6020, sect. 7.9.2
mk_case_from_shorthand(#sn{kind = 'choice', children = Children0, module = M,
                           groupings = Groupings, config = Config,
                           typedefs = Typedefs} = ChSn) ->
    Children1 =
        mk_choice_children_from_shorthand(M, Children0, Config,
                                          Groupings, Typedefs),
    ChSn#sn{children = Children1};
mk_case_from_shorthand(Sn) ->
    Sn.

mk_choice_children_from_shorthand(M, Children, Config, Groupings, Typedefs) ->
    lists:map(
      fun(#sn{kind = 'case'} = Sn) ->
              Sn;
         (#sn{name = Name, stmt = {_Keyword, Arg, Pos, _} = Stmt} = Sn) ->
              %% shorthand, add case
              #sn{name = Name, kind = 'case', children = [Sn],
                  module = M,
                  config = Config,
                  groupings = Groupings,
                  typedefs = Typedefs,
                  stmt = {'case', Arg, Pos, [Stmt]}}
      end, Children).


v_input_output(Ctx,
               #sn{kind = Kind, stmt = Stmt, children = []},
               [#sn{stmt = {Keyword, _, _, _}} | _])
  when (Keyword == 'rpc' orelse Keyword == 'action')
       andalso (Kind == 'input' orelse Kind == 'output') ->
    %% NOTE: This is done only for 'rpc' and 'action', not for
    %% tailf:action, in order to be bug-compatible with pyang.
    add_error(Ctx, yang:stmt_pos(Stmt), 'YANG_ERR_EXPECTED_DATA_DEF',
              [Kind]);
v_input_output(Ctx, _, _) ->
    Ctx.

v_no_mandatory(Sns, PruneAugment, PruneConfigFalse, ErrCode, Ctx) ->
    lists:foldl(
      fun(CaseCh, Ctx1) ->
              case chk_mandatory(CaseCh, PruneAugment, PruneConfigFalse) of
                  {true, Pos} ->
                      add_error(Ctx1, Pos, ErrCode, []);
                  false ->
                      Ctx1
              end
      end, Ctx, Sns).

%% RFC 6020, sect. 7.13
mk_operation_default_children(#sn{kind = 'operation',
                                  children = Children0} = Sn) ->
    Children1 =
        lists:foldl(
          fun(Name, Acc) ->
                  case lists:keymember(Name, #sn.name, Children0) of
                      true ->
                          Acc;
                      false ->
                          %% construct a fake schema node for this
                          [#sn{name = Name, kind = Name,
                               module = Sn#sn.module,
                               typedefs = Sn#sn.typedefs,
                               groupings = Sn#sn.groupings,
                               stmt = {Name, undefined, sn_pos(Sn), []}} |
                           Acc]
                  end
          end, Children0, ['input', 'output']),
    Sn#sn{children = Children1};
mk_operation_default_children(Sn) ->
    Sn.

mk_augments([{'augment', Arg, Pos, Substmts} = Stmt | T], Typedefs, Groupings,
            M, Ctx0, IsTopLevel, Acc) ->
    {FeatureL, WhenL, [] = _MustL, _Status, Ctx1} =
        common_substmts(Substmts, 'augment', M, Ctx0),
    {IfFeatureRes, Ctx2} = check_if_features(FeatureL, M, Ctx1),
    {Children0, _, _, Ctx3} =
        %% Create the children w/o explicit config property.
        %% The #sn{} nodes will get the correct config later when
        %% they are inserted into the tree.
        mk_children(Substmts, undefined, Typedefs, Groupings,
                    undefined, undefined,
                    M, _IsInGrouping = false, Ctx2,
                    _Mode = augment, _Ancestors = [], _Acc = [], []),
    Ctx4 = v_unique_names(Children0, Ctx3),
    case parse_schema_nodeid(IsTopLevel, Arg, Pos, M, Ctx4) of
        {ok, SchemaNodeId, Ctx5} ->
            %% Propagate when/if-feature from the augment statement
            %% to the children. (must is not allowed in augment)
            Children1 =
                if FeatureL /= [] orelse WhenL /= [] ->
                        [Ch#sn{if_feature = FeatureL ++ Ch#sn.if_feature,
                               if_feature_result =
                                   (IfFeatureRes == true)
                                   andalso Ch#sn.if_feature_result,
                               'when' = WhenL ++ Ch#sn.'when'}
                         || Ch <- Children0];
                   true ->
                        Children0
                end,
            Status = get_stmts_arg(Substmts, 'status', 'current'),
            Aug = #augment{target_node = SchemaNodeId,
                           stmt = Stmt,
                           status = Status,
                           has_when = WhenL /= [],
                           children = Children1},
            mk_augments(T, Typedefs, Groupings, M, Ctx5, IsTopLevel,
                        [Aug | Acc]);
        {error, Ctx5} ->
            mk_augments(T, Typedefs, Groupings, M, Ctx5, IsTopLevel,
                        Acc)
    end;
mk_augments([_ | T], Typedefs, Groupings, M, Ctx, IsTopLevel, Acc) ->
    mk_augments(T, Typedefs, Groupings, M, Ctx, IsTopLevel, Acc);
mk_augments([], _Typedefs, _Groupings, _M, Ctx, _IsTopLevel, Acc) ->
    {Acc, Ctx}.

mk_deviations([{'deviation', Arg, Pos, Substmts} = Stmt | T], M, Ctx0, Acc) ->
    case parse_schema_nodeid(true, Arg, Pos, M, Ctx0) of
        {ok, SchemaNodeId, Ctx1} ->
            {Deviates, Ctx2} = mk_deviates(Substmts, M, Ctx1, []),
            Deviation = #deviation{target_node = SchemaNodeId,
                                   stmt = Stmt,
                                   deviates = Deviates},
            mk_deviations(T, M, Ctx2, [Deviation | Acc]);
        {error, Ctx1} ->
            mk_deviations(T, M, Ctx1, Acc)
    end;
mk_deviations([_ | T], M, Ctx, Acc) ->
    mk_deviations(T, M, Ctx, Acc);
mk_deviations([], _M, Ctx, Acc) ->
    {Acc, Ctx}.

mk_deviates([{'deviate', Arg, _Pos, Substmts} | T], M, Ctx0, Acc) ->
    case Arg of
        'not-supported' ->
            mk_deviates(T, M, Ctx0, [not_supported | Acc]);
        'replace' ->
            Acc1 = [{replace, Substmts} | Acc],
            mk_deviates(T, M, Ctx0, Acc1);
        _ ->
            mk_deviates(T, M, Ctx0, [{Arg, Substmts} | Acc])
    end;
mk_deviates([_ | T], M, Ctx, Acc) ->
    mk_deviates(T, M, Ctx, Acc);
mk_deviates([], _, Ctx, Acc) ->
    {Acc, Ctx}.

apply_remote_deviations([{TargetModuleName, Deviations0} | T], M,
                        DeviatedModuleNames, Ctx0) ->
    %% Find the imported revision for this modulename.
    case get_imported_module(TargetModuleName, M, Ctx0) of
        {value, TargetM0} ->
            %% Update the target_node
            %% identifier so that it matches the target schema node
            %% tree; i.e., nodes in the target module have just a
            %% name, and other nodes have {ModuleName, Name}.
            Deviations1 =
                lists:map(
                  fun(#deviation{target_node = TargetNode0} = D) ->
                          TargetNode1 = patch_target(TargetNode0,
                                                     M#module.modulename,
                                                     TargetModuleName),
                          D#deviation{target_node = TargetNode1}
                  end, Deviations0),
            %% If we're deviating an augmented node, it is
            %% the augmenting module that is deviated, not
            %% the target module.
            DevModF = fun (#deviation{target_node = TN}) ->
                              case lists:last(TN) of
                                  {child, {ModuleName, _}} ->
                                      ModuleName;
                                  _ ->
                                      TargetModuleName
                              end
                      end,
            NewDeviatedModuleNames0 =
                lists:usort([DevModF(D) || D <- Deviations1]),
            NewDeviatedModuleNames =
                NewDeviatedModuleNames0 -- DeviatedModuleNames,
            %% We need to update the prefix maps of all deviated modules,
            %% to also include those from the deviation module.
            {DeviatedModules, TargetM, Ctx2} =
                update_prefix_maps(NewDeviatedModuleNames,
                                   TargetModuleName, TargetM0, M, Ctx0),
            %% Perform the deviation - for validation even if not applying
            {DeviatedChildren, Ignored, Ctx3} =
                deviate_children(Deviations1,
                                 TargetM#module.children,
                                 TargetM#module.ignored,
                                 [TargetM], Ctx2),
            Ctx4 = if Ctx3#yctx.apply_deviations ->
                           %% Record the deviation.
                           %% Update the modules in the context.
                           deviation_update_modules(DeviatedModules, M,
                                                    TargetM, DeviatedChildren,
                                                    Ignored, Ctx3);
                      true ->
                           Ctx3
                   end,
            AllDeviatedModuleNames =
                NewDeviatedModuleNames ++ DeviatedModuleNames,
            apply_remote_deviations(T, M, AllDeviatedModuleNames, Ctx4);
        none ->
            %% Some error with the target module; already reported.
            Ctx0
    end;
apply_remote_deviations([], _M, _DeviatedModuleNames, Ctx) ->
    Ctx.

update_prefix_maps(DeviatedModuleNames, TargetModuleName, TargetM0, M, Ctx0) ->
    %% Always do the target module - it must be done first,
    %% outside the loop, to preserve the updates
    {TargetM1, Ctx1} = combine_prefix_maps(TargetM0, M, Ctx0),
    %% Do the actually deviated modules
    {DeviatedModules, TargetM, Ctx2} =
        update_prefix_maps(lists:delete(TargetModuleName,
                                        DeviatedModuleNames),
                           TargetM1, M, Ctx1),
    {[TargetM|DeviatedModules], TargetM, Ctx2}.

update_prefix_maps(DeviatedModuleNames, TargetM, M, Ctx) ->
    lists:foldl(
      fun (DeviatedModuleName, {DeviatedModules, TargetM0, Ctx0} = Acc) ->
              case get_imported_module(DeviatedModuleName, M, Ctx0) of
                  {value, DeviatedM0} ->
                      {DeviatedM, TargetM1, Ctx1} =
                          combine_prefix_maps(DeviatedM0, M, TargetM0, Ctx0),
                      {[DeviatedM|DeviatedModules], TargetM1, Ctx1};
                  none ->
                      %% Some error with the deviated module; already reported.
                      Acc
              end
      end,
      {[], TargetM, Ctx},
      DeviatedModuleNames).

deviation_update_modules(DeviatedModules,
                         #module{modulename = DeviatingModuleName,
                                 revision = DeviatingModuleRev} = DeviatingM,
                         #module{modulename = TargetModuleName} = TargetM,
                         DeviatedChildren, Ignored,
                         #yctx{modrevs = ModRevs} = Ctx) ->
    deviation_update_modules(DeviatedModules, TargetModuleName, ModRevs,
                             [{DeviatingModuleName, DeviatingModuleRev}],
                             TargetM#module{children = DeviatedChildren,
                                            ignored = Ignored},
                             DeviatingM, Ctx).

deviation_update_modules([#module{modulename = TargetModuleName} | T],
                         TargetModuleName, ModRevs,
                         DeviatingModRevL, TargetM, M, Ctx) ->
    NewModRevs = update_deviated_by(TargetModuleName, TargetM,
                                    DeviatingModRevL, ModRevs),
    deviation_update_modules(T, '$done', NewModRevs,
                             DeviatingModRevL, TargetM, M, Ctx);
deviation_update_modules([DeviatedM | T],
                         TargetModuleName, ModRevs,
                         DeviatingModRevL, TargetM, M, Ctx) ->
    #module{modulename = DeviatedModuleName} = DeviatedM,
    NewModRevs = update_deviated_by(DeviatedModuleName, DeviatedM,
                                    DeviatingModRevL, ModRevs),
    deviation_update_modules(T, TargetModuleName, NewModRevs,
                             DeviatingModRevL, TargetM, M, Ctx);
deviation_update_modules([], '$done', ModRevs,
                         _DeviatingModRevL, _TargetM, _M, Ctx) ->
    Ctx#yctx{modrevs = ModRevs};
deviation_update_modules([], TargetModuleName, ModRevs,
                         _DeviatingModRevL,
                         #module{revision = TargetModuleRev} = TargetM,
                         _M, Ctx) ->
    %% target module wasn't deviated, but it must be updated in modrevs
    NewModRevs = map_update({TargetModuleName, TargetModuleRev},
                            TargetM, ModRevs),
    Ctx#yctx{modrevs = NewModRevs}.

update_deviated_by(DeviatedModuleName,
                   #module{revision = DeviatedModuleRev,
                           deviated_by = DeviatedBy} = DeviatedM,
                   DeviatingModRevL, ModRevs) ->
    NewDeviatedBy = lists:umerge(DeviatingModRevL, DeviatedBy),
    map_update({DeviatedModuleName, DeviatedModuleRev},
               DeviatedM#module{deviated_by = NewDeviatedBy},
               ModRevs).


%% This is a builtin post_expand_module hook.
post_expand_module_after_remote_augments_and_deviations(
  Ctx, #module{modulename = MName, yang_version = YangVersion} = M) ->
    lists:foldl(
      fun({TargetName, RAugments}, Ctx_0) ->
              lists:foldl(
                fun(#augment{target_node = TN, children = Chs0,
                             has_when = HasWhen},
                    Ctx_1) ->
                        %% Since patch_target() has been done for target_node,
                        %% nodes in the augment target module have just a name,
                        %% while other nodes have {ModuleName, Name}
                        %% - i.e. we need to check unless the final node is
                        %% in the augmenting module
                        case lists:last(TN) of
                            {child, {MName, _}} ->
                                %% no check
                                Ctx_1;
                            _ ->
                                %% check - need to get the "final" children
                                %% that may have been updated by deviations
                                {Ancestors, Chs} =
                                    get_updated_children(TargetName, TN, Chs0,
                                                         M, Ctx_1),
                                Ctx_2 =
                                    if HasWhen andalso YangVersion /= '1' ->
                                            Ctx_1;
                                       true ->
                                            v_no_mandatory_aug(
                                              Chs, YangVersion, Ctx_1)
                                    end,
                                %% Since deviations might have changed nodes
                                %% (e.g. not-supported) we need to run this
                                %% again.
                                lists:foldl(
                                  fun(Sn0, Ctx_3) ->
                                          post_expand_child(Sn0, Ancestors,
                                                            M, Ctx_3)
                                  end, Ctx_2, Chs)
                        end
                end, Ctx_0, RAugments)
      end, Ctx, M#module.remote_augments).

v_no_mandatory_aug(Chs, YangVersion, Ctx_1) ->
    v_no_mandatory(Chs,
                   _PruneAugment = children,
                   _PruneConfigFalse = YangVersion /= '1',
                   'YANG_ERR_MANDATORY_NODE_IN_AUGMENT',
                   Ctx_1).

get_updated_children(TargetName, TargetNode, Children, M, Ctx) ->
    case get_imported_module(TargetName, M, Ctx) of
        {value, TargetM} ->
            case get_schema_node(TargetNode, TargetM) of
                {true, #sn{children = AllChildren} = TargetSn, Ancestors} ->
                    %% get the (successfully) augmented children
                    {[TargetSn | Ancestors],
                     lists:zf(
                       fun (#sn{name = Name, stmt = {_, _, Pos, _}}) ->
                               %% if Pos differs, it's a different child
                               %% - "our" child was dropped as duplicate
                               case lists:keyfind(Name, #sn.name, Children) of
                                   #sn{stmt = {_, _, Pos, _}} ->
                                       true;
                                   _ ->
                                       false
                               end
                       end, AllChildren)};
                _ ->
                    %% error already reported
                    {[], []}
            end;
        _ ->
            %% error already reported
            {[], []}
    end.

%% Given two modules, combine their prefix maps.
%%     When combining the maps, if a prefix is
%%     present in both maps, and the imported modules
%%     associated with the prefix in each map are not the same:
%%     Give an error, and use the prefix-module pair from the
%%     target module, _not_ the source module.
-spec combine_prefix_maps(TargetM :: #module{}, SrcM :: #module{}, #yctx{}) ->
                                 {NewTargetM :: #module{}, #yctx{}}.
combine_prefix_maps(TargetM, SrcM, Ctx) ->
    {NewTargetM, _undefined, Ctx1} =
        combine_prefix_maps(TargetM, SrcM, undefined, Ctx),
    {NewTargetM, Ctx1}.

%% If ParentM == undefined, update the #sn{} children of the (updated) TargetM,
%% otherwise update the #sn{} children of ParentM.
-spec combine_prefix_maps(TargetM :: #module{}, SrcM :: #module{},
                          ParentM :: undefined | #module{}, #yctx{}) ->
                                 {NewTargetM :: #module{},
                                  NewParentM :: undefined | #module{},
                                  #yctx{}} |
                                 {false, #yctx{}}.
combine_prefix_maps(#module{modulename = TName,
                            prefix_map = TPrefixMap,
                            imports    = TImports,
                            stmt       = {_, _, _, TModSubStmts}} = TargetM0,
                    #module{modulename = SName,
                            prefix_map = SPrefixMap,
                            imports    = SImports,
                            revision   = SRev,
                            stmt       = {_, _, _, SModSubStmts}},
                    ParentM,
                    #yctx{unused_imports = U} = Ctx0) ->
    %% Get the target module's unused_imports entry
    {value, {TName, TUnusedL}, UnusedRem} = lists:keytake(TName, 1, U),
    {NewPrefixMap, NewImports, NewUnusedL, Updated, Ctx1} =
        map_foldl(
          fun (SPrefix, SModuleName0,
               {TPrefixMap0, TImports0, TUnusedL0, Updated0, Ctx01} = Acc) ->
                  {SModuleName, ImportL} =
                      case SModuleName0 of
                          '$self' ->
                              %% SPrefix is src module's prefix for itself
                              %% - should be "normal" import in target
                              {SName, [{SName, SRev, SPrefix, undefined}]};
                          TName ->
                              %% SPrefix is (a) src module prefix for target
                              %% - should be '$self'-entry w/o import in target
                              {'$self', []};
                          _ ->
                              {SModuleName0,
                               [lists:keyfind(SPrefix, 3, SImports)]}
                      end,
                  case map_lookup(SPrefix, TPrefixMap0) of
                      none ->
                          {map_insert(SPrefix, SModuleName, TPrefixMap0),
                           ImportL ++ TImports0,
                           ImportL ++ TUnusedL0,
                           true,
                           Ctx01};
                      {value, SModuleName} ->
                          Acc;
                      {value, _} ->
                          %% Search the stmt tree of the source and
                          %% target modules to get the error positions
                          SPos =
                              if SModuleName0 == '$self' ->
                                      stmt_pos(search_one_stmt('prefix',
                                                               SModSubStmts));
                                 true ->
                                      find_prefix_pos(SPrefix, SModSubStmts)
                              end,
                          TPosArg =
                              try
                                  TPos = find_prefix_pos(SPrefix,
                                                         TModSubStmts),
                                  ["defined at ", yang_error:fmt_pos(TPos)]
                              catch
                                  _:_ ->
                                      %% If it's not from an import statement
                                      %% in the target module, it must be:
                                      "added by another module"
                              end,
                          {TPrefixMap0,
                           TImports0,
                           TUnusedL0,
                           Updated0,
                           add_error(Ctx01, SPos,
                                     'YANG_ERR_PREFIX_MOD_MISMATCH',
                                     [SPrefix, TPosArg])}
                  end
          end,
          {TPrefixMap, TImports, TUnusedL, false, Ctx0},
          SPrefixMap),
    if Updated ->
            TargetM1 = TargetM0#module{prefix_map = NewPrefixMap,
                                       imports    = NewImports},
            TargetM2 = TargetM1#module{xpath_ns_map =
                                           yang_xpath:mk_ns_map(TargetM1)},
            if ParentM == undefined ->
                    NewChs =
                        update_sn_mod(TargetM2#module.children, TargetM2, []),
                    NewTargetM = TargetM2#module{children = NewChs},
                    NewParentM = ParentM;
               true ->
                    NewChs =
                        update_sn_mod(ParentM#module.children, TargetM2, []),
                    NewTargetM = TargetM2,
                    NewParentM = ParentM#module{children = NewChs}
            end,
            Ctx2 = Ctx1#yctx{unused_imports = [{TName, NewUnusedL}|UnusedRem]},
            {NewTargetM, NewParentM, Ctx2};
       true ->
            if ParentM == undefined ->
                    NewChs =
                        update_sn_mod(TargetM0#module.children, TargetM0, []),
                    NewTargetM = TargetM0#module{children = NewChs},
                    NewParentM = ParentM;
               true ->
                    NewChs =
                        update_sn_mod(ParentM#module.children, TargetM0, []),
                    NewTargetM = TargetM0,
                    NewParentM = ParentM#module{children = NewChs}
            end,
            {NewTargetM, NewParentM, Ctx1}
    end.

find_prefix_pos(PArg, [{import, _, _, Subs} | T]) ->
    case search_one_stmt('prefix', Subs) of
        {prefix, PArg, Pos, _} ->
            Pos;
        _ ->
            find_prefix_pos(PArg, T)
    end;
find_prefix_pos(PArg, [_ | T]) ->
    find_prefix_pos(PArg, T).

%% Recursively update #sn.module
update_sn_mod([#sn{module = #module{modulename = SnModuleName} = SnMod,
                   children = Chs} = Sn | Rest],
              #module{modulename = NewModuleName} = NewMod,
              MergedModL) ->
    if SnModuleName == NewModuleName ->
            %% Replace the module
            NewSnMod = NewMod,
            NewMergedModL = MergedModL;
       true ->
            %% Replace the prefix maps with merged versions
            case lists:keyfind(SnModuleName, #module.modulename, MergedModL) of
                #module{} = NewSnMod ->
                    %% already have #module{} with merged maps
                    NewMergedModL = MergedModL;
                false ->
                    %% do the merge
                    NewSnMod = merge_prefix_maps(SnMod, NewMod),
                    NewMergedModL = [NewSnMod | MergedModL]
            end
    end,
    NewChs = update_sn_mod(Chs, NewMod, NewMergedModL),
    [Sn#sn{module = NewSnMod, children = NewChs} |
     update_sn_mod(Rest, NewMod, NewMergedModL)];
update_sn_mod([], _NewMod, _MergedModL) ->
    [].

%% "Simplified version" of combine_prefix_maps/3
merge_prefix_maps(#module{prefix_map = SnPrefixMap} = SnMod,
                  #module{modulename = NewModuleName,
                          prefix_map = NewModPrefixMap}) ->
    %% fix up '$self' entries in NewModPrefixMap
    PrefixMap0 = map_foldl(fun (Prefix, '$self', Map) ->
                                   map_insert(Prefix, NewModuleName, Map);
                               (Prefix, ModuleName, Map) ->
                                   map_insert(Prefix, ModuleName, Map)
                           end,
                           map_new(),
                           NewModPrefixMap),
    %% let the "original" map win in case of duplicates
    PrefixMap = map_foldl(fun (Prefix, ModuleName, Map) ->
                                  map_update(Prefix, ModuleName, Map)
                          end,
                          PrefixMap0,
                          SnPrefixMap),
    NewSnMod = SnMod#module{prefix_map = PrefixMap},
    NewSnMod#module{xpath_ns_map = yang_xpath:mk_ns_map(NewSnMod)}.

%% Deviate 'Children' with the statements from 'Deviations'.
%% Return the deviated 'Children'.
deviate_children(Deviations, Children, Ignored0, Ancestors, Ctx0) ->
    {DeviatedChildren, Ignored2, Ctx1} =
        lists:foldl(
          fun(Deviation, {AccChildren, Ignored1, Ctx00}) ->
                  deviate_children0(Deviation#deviation.target_node,
                                    AccChildren, [], Ignored1, Ancestors,
                                    Deviation, Ctx00)
          end, {Children,Ignored0, Ctx0}, Deviations),
    {DeviatedChildren, Ignored2, Ctx1}.

%% Nodes with if_feature_result == false are considered to *exist*,
%% in order to not give an error when deviating such nodes.
%% It *might* be possible to optimize by not actually doing the
%% deviation in that case, but probably not worth the effort.
deviate_children0([{child, Name}],
                  [#sn{name = Name} = Sn | Sns],
                  Acc, IgnAcc, Ancestors, Deviation,
                  Ctx) ->
    %% This is the node to deviate
    case lists:member(not_supported, Deviation#deviation.deviates) of
        true ->
            Ctx1 =
                case Ancestors of
                    [#sn{kind = 'choice', default = {DefaultStmt, Name}} | _] ->
                        %% Give error if we remove the default case in a choice
                        %% Not 100% perfect - it should be OK to remove the
                        %% default case if we remove/modify the choice
                        %% 'default' stmt too - and it is, but only if the
                        %% latter deviation is done *first*
                        add_error(Ctx, stmt_pos(DefaultStmt),
                                  'YANG_ERR_DEFAULT_CASE_NOT_FOUND',
                                  [stmt_arg(DefaultStmt)]);
                    [#sn{kind = 'list', keys = Keys, stmt = ListStmt} | _] ->
                        %% Give error if we remove a key leaf
                        case lists:member(Name, Keys) of
                            true ->
                                %% key stmt must exist since Keys /= []
                                KeyStmt = search_one_substmt('key', ListStmt),
                                add_error(Ctx, stmt_pos(KeyStmt),
                                          'YANG_ERR_BAD_KEY', [Name]);
                            false ->
                                Ctx
                        end;
                    _ ->
                        Ctx
                end,
            {lists:reverse(Acc, Sns), [Sn | IgnAcc], Ctx1};
        false ->
            {Ctx1, Sn1} = apply_deviation_sn(Sn, Deviation,
                                             Ancestors, Ctx),
            {lists:reverse(Acc, [Sn1 | Sns]), IgnAcc, Ctx1}
    end;
deviate_children0([{child, Name} | Ids],
                  [#sn{name = Name, children = Children,
                       ignored = Ignored0} = Sn | Sns],
                  Acc, IgnAcc, Ancestors, Deviation,
                  Ctx) ->
    %% We found a node in the deviation path,
    %% we must update it with new children
    {DeviatedChildren, Ignored1, Ctx1} =
        deviate_children0(Ids, Children, [], Ignored0,
                          [Sn | Ancestors], Deviation, Ctx),
    Sn1 = Sn#sn{children = DeviatedChildren, ignored = Ignored1},
    {lists:reverse(Acc, [Sn1 | Sns]), IgnAcc, Ctx1};
deviate_children0(Ids, [Sn | Sns], Acc, IgnAcc, Ancestors, Deviation, Ctx) ->
    deviate_children0(Ids, Sns, [Sn | Acc],  IgnAcc, Ancestors, Deviation, Ctx);
deviate_children0([{child, Id} | _], [], Acc, IgnAcc, _, Deviation, Ctx0) ->
    %% We didn't find the node, report error.
    case lists:keymember(Id, #sn.name, IgnAcc) of
        false ->
            #deviation{stmt = {_, _, Pos, _}} = Deviation,
            {lists:reverse(Acc), IgnAcc,
             add_error(Ctx0, Pos, 'YANG_ERR_NODE_NOT_FOUND',
                       [yang_error:fmt_yang_identifier(Id)])};
        true ->
            %% already ignored by some other deviation
            {lists:reverse(Acc), IgnAcc, Ctx0}
    end.

apply_deviation_sn(Sn0, #deviation{deviates = Deviates}, Ancestors, Ctx0) ->
    {Ctx1, Sn1} =
        lists:foldl(fun apply_deviate/2, {Ctx0, Sn0}, Deviates),
    {Ctx2, Sn2} =
        run_mk_sn_hooks(Ctx1, Sn1, #hooks.pre_mk_sn, final,
                        undefined, Ancestors),
    %% config is special - if it is changed to false we apply it
    %% recursively to all children
    {Ctx4, Sn4} =
        if Sn0#sn.config /= Sn1#sn.config,
           Sn1#sn.config == false ->
                TmpHooks = #hooks{pre_mk_sn = [fun pre_mk_sn_config/5]},
                {Ctx3, Sn3} =
                    run_mk_sn_hooks_rec(Ctx2#yctx{hooks = TmpHooks}, Sn2,
                                        final, undefined, Ancestors),
                {Ctx3#yctx{hooks = Ctx2#yctx.hooks}, Sn3};
           true ->
                {Ctx2, Sn2}
        end,
    run_mk_sn_hooks(Ctx4, Sn4, #hooks.post_mk_sn, final,
                    undefined, Ancestors).

apply_deviate({How, Stmts}, Acc0) ->
    lists:foldl(fun(Stmt, Acc) -> apply_deviate2(How, Stmt, Acc) end,
                Acc0, Stmts).

apply_deviate2(How, {Kwd, Arg, Pos, _} = Stmt,
               {Ctx, #sn{kind = Kind, stmt = SnStmt} = Sn}) ->
    Substmts = stmt_substmts(SnStmt),
    case {chk_valid_deviation(Kwd, stmt_keyword(SnStmt), Ctx#yctx.env), How} of
        {{true, IsSingleton}, add} ->
            case IsSingleton of
                true ->
                    case lists:keymember(Kwd, 1, Substmts) of
                        true ->
                            {add_error(Ctx, Pos,
                                       'YANG_ERR_BAD_DEVIATE_ADD',
                                       [yang_error:fmt_keyword(Kwd)]),
                             Sn};
                        false ->
                            add_stmt(Stmt, Sn, Ctx)
                    end;
                false ->
                    add_stmt(Stmt, Sn, Ctx)
            end;
        {{true, IsSingleton}, replace} ->
            case IsSingleton of
                true ->
                    case lists:keytake(Kwd, 1, Substmts) of
                        {value, _, L} ->
                            SnStmt1 = set_substmts(SnStmt, L),
                            add_stmt(Stmt, Sn#sn{stmt = SnStmt1}, Ctx);
                        false when Kwd == 'config' ->
                            %% config is special - all nodes have a
                            %% config property; thus it can always be
                            %% replaced.  FIXME: pyang and 6020 allows
                            %% to add a config, so we do that as well.
                            %% This should be clarified.
                            add_stmt(Stmt, Sn, Ctx);
                        false ->
                            {add_error(Ctx, Pos,
                                       'YANG_ERR_BAD_DEVIATE_REPLACE_NOT_FOUND',
                                       [yang_error:fmt_keyword(Kwd)]),
                             Sn}
                    end;
                false ->
                    {add_error(Ctx, Pos,
                               'YANG_ERR_BAD_DEVIATE_REPLACE',
                               [yang_error:fmt_keyword(Kwd)]),
                     Sn}
            end;
        {{true, _}, delete} ->
            case search_one_stmt(Kwd, Arg, Substmts) of
                false ->
                    {add_error(Ctx, Pos,
                               'YANG_ERR_BAD_DEVIATE_DELETE',
                               [yang_error:fmt_keyword(Kwd), Arg]),
                     Sn};
                Found ->
                    SnStmt1 = set_substmts(SnStmt,
                                           lists:delete(Found, Substmts)),
                    Sn1 = Sn#sn{stmt = SnStmt1},
                    case Kwd of
                        'must' ->
                            {value, _, MustL} =
                                lists:keytake(Found,
                                              ?MUST_STMT, Sn#sn.must),
                            {Ctx, Sn1#sn{must = MustL}};
                        'default' ->
                            {Ctx, Sn1#sn{default = undefined}};
                        _ ->
                            {Ctx, Sn1}
                    end
            end;
        {false, _} ->
            {add_error(Ctx, stmt_pos(Stmt), 'YANG_ERR_BAD_DEVIATION',
                       [yang_error:fmt_keyword(Kwd), Kind]),
            Sn}
    end.

add_stmt(Stmt, #sn{stmt = SnStmt, must = MustL, module = M} = Sn, Ctx0) ->
    {_, Arg, Pos, _} = Stmt,
    Substmts = stmt_substmts(SnStmt),
    Sn1 = Sn#sn{stmt = set_substmts(SnStmt, [Stmt | Substmts])},
    case stmt_keyword(Stmt) of
        'type' ->
            {Ctx0, Sn1#sn{type = undefined}};
        'default' ->
            {Ctx0, Sn1#sn{default = undefined}};
        'must' ->
            case
                yang_xpath:compile(Arg, Pos, M, Ctx0#yctx.strict, Ctx0)
            of
                {ok, CompiledXPath, Ctx1} ->
                    {Ctx1,
                     Sn1#sn{must = [{CompiledXPath, [], Stmt} | MustL]}};
                {error, Ctx1} ->
                    {Ctx1, Sn}
            end;
        'config' ->
            {Ctx0, Sn1#sn{config = Arg}};
        _ ->
            {Ctx0, Sn1}
    end.

%% The grammar specifies which statements are allowed in 'deviate'.
%% If a certain keyword isn't allowed in 'deviate' (according to the grammar),
%% we won't end up here.  This code checks that the keyword is allowed
%% in the parent/target node.
chk_valid_deviation(Keyword, ParentKeyword, Env) ->
    case yang_parser:get_statement_spec(ParentKeyword) of
        {value, {_, _, Rules, _}} ->
            case lists:keyfind(Keyword, 1, Rules) of
                {_, Occurence} ->
                    IsSingleton =
                        if Occurence == '?'; Occurence == '1' ->
                                true;
                           true ->
                                false
                        end,
                    {true, IsSingleton};
                _ ->
                    %% We know the grammar of the parent.  If we also know
                    %% the grammar of the child, this is an illegal deviation
                    ExtModules = Env#env.extension_modules,
                    case Keyword of
                        {OtherModule, _} ->
                            case lists:member(OtherModule, ExtModules) of
                                true ->
                                    false;
                                false ->
                                    %% Unknown module, allow this
                                    {true, false}
                            end;
                        _ ->
                            false
                    end
            end;
        _ ->
            %% We don't know the grammar, accept the deviation
            {true, false}
    end.

set_substmts({K,A,P,_}, S) ->
    {K,A,P,S}.

%% FIXME Need to run post_expand_typedefs() also for
%% (at least) toplevel typedefs in groupings
post_expand_module(#module{children = Children0, typedefs = Typedefs} = M,
                   #yctx{hooks = #hooks{post_expand_module = ModHookFs,
                                        post_expand_sn = SnHookFs}} = Ctx0)
  when ModHookFs /= [] orelse SnHookFs /= [] ->
    Ctx1 = run_post_expand_module_hooks(Ctx0, M),
    Ctx2 = post_expand_typedefs(Typedefs, [], M, Ctx1),
    lists:foldl(fun(Sn, Ctx_0) ->
                        post_expand_child(Sn, [], M, Ctx_0)
                end, Ctx2, Children0);
post_expand_module(_M, Ctx) ->
    Ctx.

post_expand_child(#sn{if_feature_result = false}, _Ancestors, _M, Ctx) ->
    Ctx;
post_expand_child(#sn{module = #module{modulename = ModName,
                                       modulerevision = ModRev},
                      kind = Kind,
                      typedefs = Typedefs} = Sn,
                  Ancestors0,
                  #module{modulename = ModName,
                          modulerevision = ModRev} = M,
                  Ctx0) ->
    Ctx1 =
        case lists:member(Kind, ['leaf', 'leaf-list', 'choice', 'case']) of
            false ->
                post_expand_typedefs(Typedefs, Ancestors0, M, Ctx0);
            true ->
                Ctx0
        end,
    Ctx2 = run_post_expand_sn_hooks(Ctx1, Sn, M, Ancestors0),
    Ancestors1 = [Sn | Ancestors0],
    lists:foldl(fun(ChSn, Ctx_0) ->
                           post_expand_child(ChSn, Ancestors1, M, Ctx_0)
                end, Ctx2, Sn#sn.children);
post_expand_child(_Sn, _Ancestors, _M, Ctx) ->
    %% #sn{} augmented from other module
    Ctx.

run_post_expand_module_hooks(Ctx0, M) ->
    #yctx{hooks = #hooks{post_expand_module = HookFs}} = Ctx0,
    lists:foldl(
      fun(HookF, Ctx1) ->
              HookF(Ctx1, M)
      end, Ctx0, HookFs).

run_post_expand_sn_hooks(#yctx{hooks = #hooks{post_expand_sn = HookFs}} = Ctx0,
                         Sn, M, Ancestors) ->
    lists:foldl(
      fun(HookF, Ctx1) ->
              HookF(Ctx1, Sn, M, Ancestors)
      end, Ctx0, HookFs).

%% This is a builtin post_expand_sn hook.
post_expand_sn(Ctx,
               #sn{module = #module{name = ModName}} = Sn,
               #module{name = ModName} = M,
               Ancestors) ->
    %% This clause matches when we're expanding the (sub)module where this
    %% Sn was defined.  This ensures this function is called only once.
    do_post_expand_sn(Ctx, Sn, M, Ancestors);
post_expand_sn(Ctx,
               #sn{module = #module{yang_version = '1.1',
                                    modulename = ModName,
                                    stmt = {'submodule', _, _, _}}} = Sn,
               #module{name = ModName} = M,
               Ancestors) ->
    %% This clause is added since for yang 1.1 submodules, post_expand_module/2
    %% is not called at all and thus we match module name to the name of module
    %% that the submodule belongs to.
    do_post_expand_sn(Ctx, Sn, M, Ancestors);
post_expand_sn(Ctx, _Sn, _M, _Ancestors) ->
    Ctx.

do_post_expand_sn(Ctx0, Sn, M, Ancestors) ->
    Ctx1 = post_expand_sn_leafref(Ctx0, Sn, M, Ancestors),
    post_expand_sn_xpath_dep(Ctx1, Sn, M, Ancestors).

post_expand_sn_leafref(Ctx,
                       #sn{type = #type{type_spec = TypeSpec, base = Base},
                           default = Default} = Sn,
                       M, Ancestors) ->
    if is_record(TypeSpec, leafref_type_spec) ->
            validate_leafref_path_and_default(Base, Default, TypeSpec, Sn,
                                              Sn#sn.status, Sn#sn.stmt,
                                              M, Ancestors, Ctx);
       true ->
            Ctx
    end;
post_expand_sn_leafref(Ctx, _Sn, _M, _Ancestors) ->
    Ctx.

%% called during post_expand; just validates the dependency paths
%% when we have expanded Ancestors, but cannot modify the #sn.
post_expand_sn_xpath_dep(Ctx, #sn{must = [], 'when' = []}, _M, _Ancestors) ->
    Ctx;
post_expand_sn_xpath_dep(Ctx, #sn{must = MustL, 'when' = WhenL} = Sn,
                         M, Ancestors) ->
    L = [{Deps, Stmt} || {_, Deps, Stmt} <- MustL,
                         is_list(Deps)] ++
        [{Deps, Stmt} || {_, Deps, _, Stmt} <- WhenL,
                         is_list(Deps)],
    lists:foldl(
      fun({Deps, Stmt}, Ctx0) ->
              lists:foldl(
                fun(Dep, Ctx1) ->
                        case
                            yang_xpath:v_dep_path(Dep, Stmt,
                                                  Ctx1, Sn, M, Ancestors)
                        of
                            {true, DepSn} ->
                                if Sn#sn.config == true andalso
                                   DepSn#sn.config == false ->
                                        Pos = stmt_pos(Stmt),
                                        add_xpath_ref_config_error(Ctx1, Pos,
                                                                   DepSn);
                                   true ->
                                        Ctx1
                                end;
                            {false, Errors} ->
                                %% This means the calculated
                                %% dep points to something
                                %% that can never exist, which
                                %% means the original xpath is
                                %% wrong.  Warn.
                                lists:foldl(
                                  fun add_xpath_bad_ref/2,
                                  Ctx1, Errors)
                        end
                end, Ctx0, Deps)
      end, Ctx, L).

add_xpath_ref_config_error(Ctx, Pos, TargetSn) ->
    #sn{name = TargetName, stmt = TargetStmt} = TargetSn,
    add_error(Ctx, Pos, 'YANG_ERR_XPATH_REF_BAD_CONFIG',
              [yang_error:fmt_yang_identifier(TargetName),
               yang_error:fmt_pos(stmt_pos(TargetStmt))]).

add_leafref_config_error(Ctx, Pos, TargetSn) ->
    #sn{name = TargetName, stmt = TargetStmt} = TargetSn,
    add_error(Ctx, Pos, 'YANG_ERR_LEAFREF_BAD_CONFIG',
              [yang_error:fmt_yang_identifier(TargetName),
               yang_error:fmt_pos(stmt_pos(TargetStmt))]).

add_xpath_bad_ref(#yerror{code = 'YANG_ERR_REFER_TOP_NODE' = Code,
                          args = Args, pos = Pos},
                  Ctx) ->
    add_error(warning, Ctx, Pos, Code, Args);
add_xpath_bad_ref(#yerror{code = Code, args = Args, pos = Pos},
                  #yctx{error_codes = Codes} = Ctx) ->
    add_error(Ctx, Pos, 'YANG_ERR_XPATH_BAD_REF',
              [yang_error:fmt_code(Codes, Code, Args)]).


post_parse_module_conformance(Ctx, M) ->
    case M#module.conformance of
        import ->
            {Ctx, M#module{children = [],
                           remote_augments = []}};
        implement ->
            {Ctx, M}
    end.

%% FIXME: should we make a hook out of this, just like post_expand_sn?
post_expand_typedefs(#typedefs{same_as_parent = false, map = Map},
                     Ancestors, M, Ctx)
  when M#module.conformance == implement ->
    map_foldl(
      fun(_, #typedef{type = #type{type_spec = TypeSpec, base = Base},
                      moduleref = {ModuleName, _},
                      status = Status, stmt = Stmt,
                      default = Default}, Ctx0)
            when is_record(TypeSpec, leafref_type_spec),
                 (Ancestors == [] andalso M#module.name == ModuleName) orelse
                 Ancestors /= [] ->
              %% If this is a top-level typedef (Ancestors == []), we do this
              %% only if the typedef is defined in the (sub)module we're
              %% validating.
              validate_leafref_path_and_default(Base, Default, TypeSpec,
                                                _Sn = undefined,
                                                Status, Stmt,
                                                M, Ancestors, Ctx0);
         (_, _, Ctx0) ->
              Ctx0
      end, Ctx, Map);
post_expand_typedefs(_, _, _, Ctx) ->
    Ctx.

%% FIXME: For groupings, the errors detected by validate_leafref_xxx
%% will be reported for each 'uses', with the grouping's pos()...

validate_leafref_path_and_default(#typedef{default = BaseDefault,
                                           moduleref = MRef} = Base, Default,
                                  TypeSpec, Sn, _, Stmt, M, Ancestors, Ctx0) ->
    %% Validate the default if possible,
    %% ignoring leafref path validation failure
    case yang_types:validate_leafref_path(TypeSpec, Sn, M, Ancestors, Ctx0) of
        {true, TargetSn, FinalSn} ->
            Ctx1 = validate_leafref_config(Sn, TargetSn, TypeSpec, Ctx0, true),
            if Default /= BaseDefault ->
                    validate_leafref_default(Default, Sn, FinalSn, M, Ctx1);
               true ->
                    Ctx1
            end;
        {false, Ctx1} when is_record(Ctx1, yctx) ->
            %% error is reported in "root" typedef check unless its module
            %% has conformance 'import'
            {TypedefModName, TypedefModRev} = MRef,
            if TypedefModName /= M#module.name ->
                    case get_module(TypedefModName, TypedefModRev, Ctx1) of
                        {value, #module{conformance = 'import'}} ->
                            %% report error
                            Ctx1;
                        _ ->
                            Ctx0
                    end;
               true ->
                    Ctx0
            end;
        {false, Error} when is_record(Error, yerror) andalso Sn == undefined ->
            update_bad_typedefs(Stmt, M, Error, Ctx0);
        {false, Error} when is_record(Error, yerror) ->
            maybe_add_error(Base, Error, Ctx0)
    end;
validate_leafref_path_and_default(_, Default, TypeSpec,
                                  Sn, Status, Stmt, M, Ancestors, Ctx0) ->
    %% Validate the path - and the default if any
    case yang_types:validate_leafref_path(TypeSpec, Sn, M, Ancestors, Ctx0) of
        {true, TargetSn, FinalSn} ->
            Ctx1 = chk_status(Status, TargetSn#sn.status,
                              stmt_keyword(Stmt), TargetSn#sn.kind,
                              stmt_pos(Stmt), Ctx0),
            Ctx2 = validate_leafref_config(Sn, TargetSn, TypeSpec, Ctx1, false),
            validate_leafref_default(Default, Sn, FinalSn, M, Ctx2);
        {false, Ctx1} when is_record(Ctx1, yctx) ->
            Ctx1;
        {false, Error} when is_record(Error, yerror) andalso Sn == undefined ->
            update_bad_typedefs(Stmt, M, Error, Ctx0);
        {false, Error} when is_record(Error, yerror) ->
            yang_error:add_error(Error, Ctx0)
    end.

update_bad_typedefs({_, Name, _, _},
                    #module{modulename = ModName, modulerevision = ModRev},
                    Error, #yctx{bad_typedefs = Map} = Ctx) ->
    NewMap = map_insert({Name, {ModName, ModRev}}, Error, Map),
    Ctx#yctx{bad_typedefs = NewMap}.

maybe_add_error(#typedef{name = Name, moduleref = ModRef},
                Error, #yctx{bad_typedefs = Map} = Ctx) ->
    case map_lookup({Name, ModRef}, Map) of
        none ->
            Ctx;
        _ ->
            %% Add error and delete the typedef in bad_typedefs to make sure
            %% we only add error once.
            NewMap = map_delete({Name, ModRef}, Map),
            yang_error:add_error(Error, Ctx#yctx{bad_typedefs = NewMap})
    end.

validate_leafref_default(Defaults,
                         #sn{kind = 'leaf-list', config = Config},
                         #sn{type = Type},
                         M, Ctx0)
  when is_list(Defaults) ->
    {DefVals, Ctx1} =
        lists:foldr(
          fun ({DefaultStmt, undefined}, {DefVals_0, Ctx_0}) ->
                  {Default, Ctx_1} =
                      yang_types:mk_default(DefaultStmt, Type, M, Ctx_0),
                  {[Default|DefVals_0], Ctx_1};
              (_, Acc) ->
                  Acc
          end, {[], Ctx0}, Defaults),
    v_unique_defaults(Ctx1, final, Config, undefined, DefVals);
validate_leafref_default({DefaultStmt, undefined}, _Sn, #sn{type = Type},
                         M, Ctx0) ->
    %% can't save the parsed value here
    {_Default, Ctx1} = yang_types:mk_default(DefaultStmt, Type, M, Ctx0),
    Ctx1;
validate_leafref_default(_Default, _Sn, _TargetSn, _M, Ctx) ->
    Ctx.

validate_leafref_config(Sn, TargetSn, TypeSpec, Ctx, IsTypedef) ->
    if Sn#sn.config andalso not TargetSn#sn.config andalso
       TypeSpec#leafref_type_spec.require_instance ->
            Pos =
                if IsTypedef ->
                        stmt_pos((Sn#sn.type)#type.stmt);
                   true ->
                        #leafref_type_spec{path_stmt = PathStmt} = TypeSpec,
                        stmt_pos(PathStmt)
                end,
            add_leafref_config_error(Ctx, Pos, TargetSn);
       true ->
            Ctx
    end.

post_parse_module(Ctx = #yctx{hooks = #hooks{post_parse_module = HookFs}},
                  Module) ->
    FilterF = conditional_hook_filter(Ctx, Module),
    lists:foldr(
      fun (Hook, {Ctx0, M}) ->
              case FilterF(Hook) of
                  true ->
                      Hook(Ctx0, M);
                  {true, HookFun} ->
                      HookFun(Ctx0, M);
                  false ->
                      {Ctx0, M}
              end
      end, {Ctx, Module}, HookFs).

-spec get_schema_node(cursor_path(), #module{} | #grouping{}) ->
                             {true, #sn{}, Ancestors :: [#sn{}]} | false.
%% Nodes with if_feature_result == false are considered to *exist*,
%% in order to not give an error when such a node is the target node of
%% 'augment' (or an extension with similar semantics).
%% We still need to actually apply the augment, to get the same effect
%% for augments of augments.
get_schema_node(SchemaNodeId, #grouping{children = Children}) ->
    get_schema_node(SchemaNodeId, Children, []);
get_schema_node(SchemaNodeId, #module{children = Children}) ->
    get_schema_node(SchemaNodeId, Children, []).

get_schema_node([{child, Id}], [#sn{name = Id} = Sn | _], Ancestors) ->
    {true, Sn, Ancestors};
get_schema_node([{child, Id} | Ids],
                [#sn{name = Id, children = Children} = Sn | _],
                 Ancestors) ->
    get_schema_node(Ids, Children, [Sn | Ancestors]);
get_schema_node(Ids, [_ | Sns], Ancestors) ->
    get_schema_node(Ids, Sns, Ancestors);
get_schema_node(_, [], _) ->
    false.

chk_status(XStatus, YStatus, XKeyword, YKeyword, Pos, Ctx) ->
    if (XStatus == 'current' andalso YStatus /= 'current' orelse
        (XStatus == 'deprecated' andalso YStatus == 'obsolete')) ->
            add_error(Ctx, Pos, 'YANG_BAD_STATUS_REFERENCE',
                      [XKeyword, XStatus, YKeyword, YStatus]);
       true ->
            Ctx
    end.

%% sort all augments into local and remote augments
-spec sort_augments([#augment{}], LocalName :: atom()) ->
        {LocalAugments :: [#augment{}], [{RemoteName :: atom(), [#augment{}]}]}.
sort_augments(Augments, LocalName) ->
    sort_targets(Augments, fun(R) -> R#augment.target_node end, LocalName).

%% sort all deviations into local and remote deviations
-spec sort_deviations([#deviation{}], LocalName :: atom()) ->
        {LocalDeviations :: [#deviation{}],
         [{RemoteName :: atom(), [#deviation{}]}]}.
sort_deviations(Deviations, LocalName) ->
    sort_targets(Deviations, fun(R) -> R#deviation.target_node end, LocalName).

sort_targets(L, TargetNodeF, LocalName) ->
    G = group_per_module(
          lists:keysort(1, [add_module(R, TargetNodeF, LocalName) || R <- L])),
    case lists:keytake([], 1, G) of
        {value, {[], LocalL}, RemoteL} ->
            {LocalL, RemoteL};
        false ->
            {[], G}
    end.

add_module(R, TargetNodeF, LocalName) ->
    [{child, H} | _] = TargetNodeF(R),
    case H of
        {LocalName, R} ->
            {[], R};
        {Module, _} ->
            {Module, R};
        _ ->
            {[], R}
    end.

group_per_module([]) ->
    [];
group_per_module([{Mod, R} | T]) ->
    group_per_module(T, Mod, [R]).

group_per_module([{Mod, R} | T], Mod, Acc) ->
    group_per_module(T, Mod, [R | Acc]);
group_per_module([{Mod2, R} | T], Mod, Acc) ->
    [{Mod, Acc} | group_per_module(T, Mod2, [R])];
group_per_module([], Mod, Acc) ->
    [{Mod, Acc}].

%% After a module (or grouping) has been built, and everything is expanded,
%% validate that all sibling names are unique.
v_unique_names(Sns, Ctx0) ->
    D = map_new(),
    {Ctx1, _D1} = v_unique_names2(Sns, Ctx0, D),
    Ctx1.

v_unique_names2(Sns, Ctx0, D0) ->
    lists:foldl(
      fun(#sn{name = Name, kind = Kind, stmt = Stmt, children = Children},
          {Ctx00, D00}) ->
              {_, _, Pos, _} = Stmt,
              {Ctx01, D02} =
                  try map_insert(Name, Pos, D00) of
                      D01 ->
                          if Kind == 'choice' ->
                                  %% need to peek down into all cases
                                  %% RFC 6020, sect. 6.2.1, bullet 7.
                                  lists:foldl(
                                    fun(#sn{children = CaseChs},
                                        {Ctx10, D10}) ->
                                            v_unique_names2(CaseChs, Ctx10, D10)
                                    end, {Ctx00, D01}, Children);
                             true ->
                                  {Ctx00, D01}
                          end
                  catch
                      _:_ when Kind /= '__tmp_augment__' ->
                          %% Don't report duplicate names for __tmp_augment__;
                          %% they represent augments that couldn't be found,
                          %% and they are reported elsewhere.
                          {value, OtherPos} = map_lookup(Name, D00),
                          [MinPos, MaxPos] = lists:sort([Pos, OtherPos]),
                          {add_dup_sn_error(Ctx00, Name, MaxPos, MinPos),
                           D00};
                      _:_ ->
                          {Ctx00, D00}
                  end,
              Ctx02 = v_unique_names(Children, Ctx01),
              {Ctx02, D02}
      end, {Ctx0, D0}, Sns).

typedef_lookup(_Name, undefined) ->
    none;
typedef_lookup(Name, Typedefs) ->
    case map_lookup(Name, Typedefs#typedefs.map) of
        {value, _} = Found ->
            Found;
        none ->
            typedef_lookup(Name, Typedefs#typedefs.parent)
    end.

grouping_lookup(_Name, undefined) ->
    none;
grouping_lookup(Name, Groupings) ->
    case map_lookup(Name, Groupings#groupings.map) of
        {value, _} = Found ->
            Found;
        none ->
            grouping_lookup(Name, Groupings#groupings.parent)
    end.

%% FIXME: spec.  think about where to put api-functions.
find_typedef_raw(Name, Pos, #sn{module = M, typedefs = Typedefs}, Ctx) ->
    find_typedef_raw(Name, Pos, M, Typedefs, Ctx);
find_typedef_raw(Name, Pos, #module{typedefs = Typedefs} = M, Ctx) ->
    find_typedef_raw(Name, Pos, M, Typedefs, Ctx).

find_typedef_raw(Name, Pos, M, Typedefs, Ctx0) ->
    case resolve_raw_idref(Name, Pos, M, Ctx0) of
        {self, TypeName, Ctx1} ->
            case typedef_lookup(Name, Typedefs) of
                {value, _} ->
                    {value, TypeName, Ctx1};
                none ->
                    {none,
                     add_error(Ctx1, Pos, 'YANG_ERR_DEFINITION_NOT_FOUND',
                               ['typedef',
                                yang_error:fmt_yang_identifier(TypeName)])}
            end;
        {{imported, RefM}, TypeName, Ctx1} ->
            RefName = {RefM#module.name, TypeName},
            case typedef_lookup(TypeName, RefM#module.typedefs) of
                {value, _} ->
                    {value, RefName, Ctx1};
                none ->
                    {none,
                     add_error(
                       Ctx1, Pos,
                       'YANG_ERR_DEFINITION_NOT_FOUND',
                       ['typedef',
                        yang_error:fmt_yang_identifier(RefName)])}
            end;
        {undefined, _BaseName, Ctx1} ->
            %% could not resolve prefix; error is already reported
            {none, Ctx1}
    end.

iterate_stmt(F, Keyword, Acc, L) ->
    try
        iterate_stmt0(L, Keyword, Acc, F)
    catch
        throw:Acc1 ->
            Acc1
    end.

iterate_stmt0([{Keyword, _, _, _Substmts} = H | T], Keyword, Acc0, F) ->
    case F(H, Acc0) of
%% avoid dialyzer complaint
%        {recurse, Acc1} ->
%            Acc2 = iterate_stmt0(Substmts, Keyword, Acc1, F),
%            iterate_stmt0(T, Keyword, Acc2, F);
        {continue, Acc1} ->
            iterate_stmt0(T, Keyword, Acc1, F)
%        {stop, Acc1} ->
%            throw(Acc1)
    end;
iterate_stmt0([_ | T], Keyword, Acc, F) ->
    iterate_stmt0(T, Keyword, Acc, F);
iterate_stmt0([], _, Acc, _F) ->
    Acc.

-spec parse_idref(binary(), pos(), #yctx{}) ->
                         {ok, raw_identifier_ref(), #yctx{}} |
                         {error, #yctx{}}.
parse_idref(Str, Pos, Ctx) ->
    case re:run(Str, ["^", identifier_ref(), "$"],
                [{capture, [prefix, identifier], binary}]) of
        {match, [<<>>, Name]} ->
            {ok, ?b2a(Name), Ctx};
        {match, [Prefix, Name]} ->
            {ok, {?b2a(Prefix), ?b2a(Name)}, Ctx};
        _ ->
            {error, add_error(Ctx, Pos, 'YANG_ERR_BAD_ARGUMENT',
                              [Str, "identifier-ref"])}
    end.

-spec resolve_raw_idref(raw_identifier_ref(), pos(), #module{}, #yctx{}) ->
    {self
     | undefined
     | {imported, #module{}},
     Name :: atom(),
     #yctx{}}.
%% Resolves a <prefix>:<name> string into a #module{} and name.
resolve_raw_idref({Prefix, Name}, Pos,
                  #module{prefix_map = PrefixMap,
                          name = MyModuleName} = M, Ctx0) ->
    case map_lookup(Prefix, PrefixMap) of
        {value, '$self'} ->
            {self, Name, Ctx0};
        {value, ModuleName} ->
            Ctx1 = mark_import_as_used(MyModuleName, Prefix, Ctx0),
            case get_imported_module(ModuleName, M, Ctx1) of
                {value, Module} ->
                    {{imported, Module}, Name, Ctx1};
                none ->
                    %% error already reported in import
                    {undefined, Name, Ctx1}
            end;
        none ->
            {undefined, Name, add_error(Ctx0, Pos, 'YANG_ERR_PREFIX_NOT_FOUND',
                                        [Prefix])}
    end;
resolve_raw_idref(Name, _Pos, _M, Ctx) ->
    {self, Name, Ctx}.

mark_import_as_used(MyModuleName, Prefix, #yctx{unused_imports = U0} = Ctx) ->
    case U0 of
        [{MyModuleName, L} | T] ->
            %% optimize for the normal case that our module is first in the list
            case lists:keytake(Prefix, 3, L) of
                false ->
                    %% in the best case, we don't modify ctx
                    Ctx;
                {value, _Import, RemImports} ->
                    U1 = [{MyModuleName, RemImports} | T],
                    Ctx#yctx{unused_imports = U1}
            end;
        _ ->
            %% otherwise, replace it and put it first in the list
            {value, {MyModuleName, L}, T} = lists:keytake(MyModuleName, 1, U0),
            U1 = [{MyModuleName, lists:keydelete(Prefix, 3, L)} | T],
            Ctx#yctx{unused_imports = U1}
    end.

-spec local_name(raw_identifier_ref() | identifier_ref()) -> atom().
local_name({_, Name}) ->
    Name;
local_name(Name) ->
    Name.

mk_prefix_map(Stmts, Ctx) ->
    mk_prefix_map(Stmts, Ctx, map_new()).

%% Pre: All statements are grammatically correct, so we don't have to check
%%      e.g. that 'prefix' is present in 'import'.
mk_prefix_map([{Kwd, Arg, _Pos, Substmts}|Stmts], Ctx, Map) ->
    if Kwd == 'prefix' ->
            mk_prefix_map(Stmts, Ctx, map_insert(Arg, '$self', Map));
       Kwd == 'belongs-to' ->
            {_, Prefix, _, _} = search_one_stmt('prefix', Substmts),
            mk_prefix_map(Stmts, Ctx, map_insert(Prefix, '$self', Map));
       Kwd == 'import' ->
            {_, Prefix, _, _} = search_one_stmt('prefix', Substmts),
            mk_prefix_map(Stmts, Ctx, map_insert(Prefix, Arg, Map));
       true ->
            case kwd_class(Kwd, Ctx) of
                header ->
                    mk_prefix_map(Stmts, Ctx, Map);
                linkage ->
                    mk_prefix_map(Stmts, Ctx, Map);
                extension ->
                    mk_prefix_map(Stmts, Ctx, Map);
                _ ->
                    Map
            end
    end;
mk_prefix_map([], _Ctx, Map) ->
    Map.

chk_mandatory(Sn) ->
    chk_mandatory(Sn, _PruneAugment = false, _PruneConfigFalse = false).

chk_mandatory(#sn{is_augment_top_node = true},
              _PruneAugment = true, _PruneConfigFalse) ->
    false;
chk_mandatory(#sn{config = Config},
              _PruneAugment, _PruneConfigFalse = true)
  when Config /= true ->
    false;
chk_mandatory(#sn{kind = Kind, stmt = Stmt}, _PruneAugment, _PruneConfigFalse)
  when Kind == 'leaf'; Kind == 'choice'; Kind == 'anyxml'; Kind == 'anydata' ->
    case yang:search_one_substmt('mandatory', Stmt) of
        {_, true, _, _} ->
            {true, stmt_pos(Stmt)};
        _ ->
            false
    end;
chk_mandatory(#sn{kind = Kind, stmt = Stmt}, _PruneAugment, _PruneConfigFalse)
  when Kind == 'list'; Kind == 'leaf-list' ->
    case yang:search_one_substmt('min-elements', Stmt) of
        {_, Arg, _, _} when Arg > 0 ->
            {true, stmt_pos(Stmt)};
        _ ->
            false
    end;
chk_mandatory(#sn{kind = 'container', stmt = Stmt, children = Chs},
              PruneAugment, PruneConfigFalse) ->
    case yang:search_one_substmt('presence', Stmt) of
        {_, _Arg, _, _} ->
            false;
        _ ->
            chk_any_mandatory(Chs, PruneAugment, PruneConfigFalse)
    end;
chk_mandatory(_, _, _) ->
    false.

chk_any_mandatory(Chs) ->
    chk_any_mandatory(Chs, _PruneAugment = false, _PruneConfigFalse = false).

chk_any_mandatory(Chs, children, PruneConfigFalse) ->
    chk_any_mandatory(Chs, true, PruneConfigFalse);
chk_any_mandatory([H | T], PruneAugment, PruneConfigFalse) ->
    case chk_mandatory(H, PruneAugment, PruneConfigFalse) of
        false ->
            chk_any_mandatory(T, PruneAugment, PruneConfigFalse);
        Res ->
            Res
    end;
chk_any_mandatory([], _, _) ->
    false.

build_error(Ctx, Pos, ErrCode, Args) ->
    yang_error:build_error(Ctx, Pos, ErrCode, Args).

add_error(Ctx, Pos, ErrCode, Args) ->
    yang_error:add_error(Ctx, Pos, ErrCode, Args).

add_error(Level, Ctx, Pos, ErrCode, Args) ->
    yang_error:add_error(Level, Ctx, Pos, ErrCode, Args).

add_llerrors([{Code, FName, LineNo, Offset, Str} | T], Ctx) ->
    Pos = case Offset of
              -1 -> {FName, LineNo};
              _ -> {FName, LineNo, Offset}
          end,
    Ctx1 = add_error(Ctx, Pos, yang_llerror:code2err(Code), [Str]),
    add_llerrors(T, Ctx1);
add_llerrors([], Ctx) ->
    Ctx.

-spec mk_cursor(#sn{} | undefined, [#sn{}], yang:pos(),
                #module{}, yang:cursor_type(), #yctx{}) ->
          #cursor{}.
mk_cursor(#sn{kind = Kind} = Sn, Ancestors, Pos, M, data, Ctx) ->
    %% Possibly adjust the cur node so that we're not starting in a
    %% non-data node.
    C = #cursor{pos = Pos, cur = Sn, init_modulename = M#module.modulename,
                ancestors = Ancestors, module = M, type = data},
    if Kind == 'choice' orelse Kind == 'case' ->
            {true, NewC} = cursor_move(parent, C, Ctx),
            NewC;
       true ->
            C
    end;
mk_cursor(Sn, Ancestors, Pos, M, Type, _Ctx) ->
    #cursor{pos = Pos, cur = Sn, init_modulename = M#module.modulename,
            ancestors = Ancestors, module = M, type = Type}.

-spec cursor_reset(#sn{}, #cursor{}) -> #cursor{}.
%% Must be called before a new path is followed using a cursor
%% returned by a cursor_* function.
cursor_reset(Sn, Cursor) ->
    Cursor#cursor{init_modulename = Sn#sn.module#module.modulename}.

-spec cursor_follow_path(cursor_path(), #cursor{}, #yctx{}) ->
          {true, #cursor{}}
        | {false, #yctx{}}.
%% Nodes with with if_feature_result == false are
%% considered to *not* exist, see find_child/5.
cursor_follow_path([H | T], Cursor0, Ctx) ->
    case cursor_move(H, Cursor0, Ctx) of
        {true, Cursor1} ->
            cursor_follow_path(T, Cursor1, Ctx);
        Error ->
            Error
    end;
cursor_follow_path([], #cursor{cur = {top, _}, pos = Pos}, Ctx) ->
    {false, build_error(Ctx, Pos, 'YANG_ERR_REFER_TOP_NODE', [])};
cursor_follow_path([], Cursor, _Ctx) ->
    {true, Cursor}.

-spec cursor_move(cursor_path_element(), #cursor{}, #yctx{}) ->
          {true, #cursor{}}
        | {false, #yerror{}}.
%% Moves up or down in the module trees using a cursor.
%% Nodes with with if_feature_result == false are
%% considered to *not* exist, see find_child/5.
cursor_move(parent, #cursor{ancestors = [#sn{kind = Kind} = Parent | T],
                            type = Type} = C, Ctx) ->
    NewC = C#cursor{cur = Parent, ancestors = T},
    %% If we're in the data tree, skip non data nodes as we move upwards,
    %% and remember the last one we skipped, if it is input or output.
    %% The reason is that we can't move down into input from output and
    %% vice versa.
    case Type == data andalso (not is_data_node(Kind)) of
        true ->
            LastSkipped =
                if Kind == 'input' orelse Kind == 'output' ->
                        Kind;
                   true ->
                        undefined
                end,
            cursor_move(parent, NewC#cursor{last_skipped = LastSkipped}, Ctx);
        false ->
            {true, NewC}
    end;
cursor_move(parent, #cursor{ancestors = [Parent | T]} = C, Ctx) ->
    {true, cursor_set_sn(Parent, C#cursor{ancestors = T}, Ctx)};
cursor_move(parent, #cursor{cur = #sn{} = Sn, ancestors = []} = C, _Ctx) ->
    %% move up to the top-level
    {true, C#cursor{cur = {top, (Sn#sn.module)#module.modulename},
                    last_skipped = undefined}};
cursor_move(parent, C, Ctx) ->
    %% move up from the top-level
    {false, build_error(Ctx, C#cursor.pos, 'YANG_ERR_TOO_MANY_UP', [])};

cursor_move({child, Id},
            #cursor{cur = #sn{children = Chs} = CurSn,
                    type = Type,
                    last_skipped = LastSkipped,
                    ancestors = A,
                    init_modulename = InitModName} = C,
            Ctx) ->
    case find_child(Chs, Id, Type, InitModName, LastSkipped) of
        {value, Sn} ->
            C1 = C#cursor{ancestors = [CurSn | A], last_skipped = undefined},
            {true, cursor_set_sn(Sn, C1, Ctx)};
        {skipped, SkippedSnL, Sn} ->
            C1 = C#cursor{ancestors = SkippedSnL ++ [CurSn | A],
                          last_skipped = undefined},
            {true, cursor_set_sn(Sn, C1, Ctx)};
        false ->
            case Id of
                {Mod, Name} ->
                    ok;
                Name ->
                    Mod = InitModName
            end,
            case CurSn#sn.module of
                undefined ->
                    {false, build_error(Ctx, C#cursor.pos,
                                        'YANG_ERR_NODE_NOT_FOUND2',
                                        [yang_error:fmt_yang_identifier(Name),
                                         Mod])};
                M ->
                    {false,
                     build_error(Ctx, C#cursor.pos,
                                 'YANG_ERR_NODE_NOT_FOUND3',
                                 [yang_error:fmt_yang_identifier(Name),
                                  Mod,
                                  yang_error:fmt_yang_identifier(CurSn#sn.name),
                                  M#module.modulename])}
            end
    end;
cursor_move({child, {Mod, Name}}, #cursor{cur = {top, OtherMod}} = C, Ctx)
  when Mod /= OtherMod ->
    %% trying to move down in wrong module with relative path
    {false, build_error(Ctx, C#cursor.pos,
                        'YANG_ERR_NODE_NOT_FOUND2',
                        [yang_error:fmt_yang_identifier(Name), Mod])};
cursor_move({child, {Mod, Name} = Id}, C, Ctx) ->
    %% move down from the top-level
    if (C#cursor.module)#module.modulename == Mod ->
            #module{children = Chs} = C#cursor.module;
       true ->
            case get_module(Mod, _Revision = undefined, Ctx) of
                {value, #module{children = Chs}} ->
                    ok;
                {value, processing} ->
                    #module{children = Chs} = C#cursor.module;
                none ->
                    Chs = []
            end
    end,
    case find_child(Chs, Id, C#cursor.type, '$undefined', undefined) of
        {value, Sn} ->
            {true, C#cursor{cur = Sn, ancestors = [],
                            last_skipped = undefined}};
        false ->
            {false, build_error(Ctx, C#cursor.pos,
                                'YANG_ERR_NODE_NOT_FOUND2',
                                [yang_error:fmt_yang_identifier(Name), Mod])}
    end;
cursor_move({child, Name}, C, Ctx) ->
    %% move down from top-level in current module
    cursor_move({child, {C#cursor.init_modulename, Name}}, C, Ctx).

%% FIXME: is this really the right solution?  The problem occurs when
%% we call yanger_fxs:find_final_target_sn(), which tries to follow all
%% leafrefs.  The problem is that the #cursor.module is used for absolute
%% paths, so it must be set to the module of the target.
%% Example:
%%   a:a is leafref to /b:b
%%   b:b is leafref to /b:c
%% Follow to final from a:a would fail when follow /b:b, b/c the cursor
%% module is a.
cursor_set_sn(#sn{module = SnMod} = Sn, #cursor{module = CurM} = C, Ctx) ->
    if SnMod#module.modulename == CurM#module.modulename ->
            M = CurM;
       true ->
            %% we're going into a module; and we need to get it from the
            %% context b/c the module in the sn might not be expanded.
            #module{modulename = Name} = SnMod,
            case get_module(Name, _Revision = undefined, Ctx) of
                {value, processing} ->
                    %% this is the case that we have a leafref into
                    %% something we augmented into some other module
                    M = SnMod;
                {value, M} ->
                    ok;
                none ->
                    %% this will happen when we follow a path from a
                    %% submodule, via a different module, and then (now)
                    %% into a submodule of the same main module again,
                    %% while the main module hasn't been loaded.
                    %% yanger_fxs will never end up here since it always
                    %% needs the main module.
                    M = SnMod
            end
    end,
    C#cursor{cur = Sn, module = M}.

-spec find_child([#sn{}], sn_name()) ->
                        {value, #sn{}} | {skipped, [#sn{}], #sn{}} | false.
%% Nodes with with if_feature_result == false are
%% considered to *not* exist, see find_child/5.
find_child(Sns, Id) ->
    find_child(Sns, Id, schema, '$undefined', undefined).

-spec find_child([#sn{}], sn_name(), cursor_type(), atom(), cursor_skipped()) ->
                        {value, #sn{}} | {skipped, [#sn{}], #sn{}} | false.
%% Nodes with with if_feature_result == false are considered to *not* exist,
%% to make it possible to give an error/warning when 'leafref'-path, 'must',
%% 'when', or 'unique' (or extensions with similar semantics) reference such
%% nodes from nodes with if_feature_result == true.
find_child([#sn{if_feature_result = false} | T],
            Id, Type, InitMod, LastSkipped) ->
    find_child(T, Id, Type, InitMod, LastSkipped);
find_child([#sn{kind = Kind, children = Chs} = H | T] = Sns,
           Id, Type, InitMod, LastSkipped) ->
    %% If we're in the data tree, we may need to check non-data nodes'
    %% children.
    case Type == data andalso can_skip_node(Kind, LastSkipped) of
        true ->
            case find_child(Chs, Id, Type, InitMod, undefined) of
                {value, V} ->
                    {skipped, [H], V};
                {skipped, L, V} ->
                    {skipped, [H | L], V};
                false ->
                    find_child(T, Id, Type, InitMod, LastSkipped)
            end;
        false ->
            find_child0(Sns, Id, Type, InitMod, LastSkipped)
    end;
find_child([], _, _, _, _) ->
    false.

find_child0([#sn{name = {_, _} = Id} = Sn | _], Id, _Type, _InitMod, _) ->
    {value, Sn};
find_child0([#sn{name = Id, module = #module{modulename = InitMod}} = Sn | _],
            Id, _Type, InitMod, _) ->
    {value, Sn};
find_child0([#sn{name = Name, module = #module{modulename = Mod}} = Sn | _],
           {Mod, Name}, _Type, _InitMod, _) ->
    {value, Sn};
find_child0([#sn{name = Id} = Sn | _], Id, _Type, '$undefined', _) ->
    {value, Sn};
find_child0([#sn{name = {InitMod, Id}} = Sn | _], Id, _Type, InitMod, _) ->
    {value, Sn};
find_child0([_ | T], Id, Type, InitMod, LastSkipped) ->
    %% call find_child/5 here in order to check non-data nodes' children
    find_child(T, Id, Type, InitMod, LastSkipped).

is_data_node('choice') -> false;
is_data_node('case')   -> false;
is_data_node('input')  -> false;
is_data_node('output') -> false;
is_data_node(_) -> true.

can_skip_node('choice', _) -> true;
can_skip_node('case',   _) -> true;
can_skip_node(LastSkipped, LastSkipped) -> true;
can_skip_node(_, _) -> false.

-spec map_new() -> map(term(), term()).
map_new() ->
    gb_trees:empty().

-spec map_is_empty(map0()) -> boolean().
map_is_empty(Map) ->
    gb_trees:is_empty(Map).

-spec map_insert(Key :: term(), Val :: term(), map0()) -> map0().
%% crashes if Key is already present
map_insert(Key, Val, Map) ->
    gb_trees:insert(Key, Val, Map).

-spec map_update(Key :: term(), Val :: term(), map0()) -> map0().
%% updates Key if it is already present
map_update(Key, Val, Map) ->
    gb_trees:enter(Key, Val, Map).

-spec map_lookup(Key :: term(), map0()) -> {value, Val :: term()} | none.
map_lookup(Key, Map) ->
    gb_trees:lookup(Key, Map).

-spec map_get(Key :: term(), map0()) -> Val :: term().
%% crashes if Key is not present
map_get(Key, Map) ->
    gb_trees:get(Key, Map).

-spec map_get(Key :: term(), map0(), DefVal :: term()) -> Val :: term().
map_get(Key, Map, DefVal) ->
    case gb_trees:lookup(Key, Map) of
        {value, Val} ->
            Val;
        none ->
            DefVal
    end.

-spec map_is_key(term(), map0()) -> boolean().
map_is_key(Key, Map) ->
    gb_trees:is_defined(Key, Map).

map_delete(Key, Map) ->
    gb_trees:delete(Key, Map).

map_to_list(Map) ->
    gb_trees:to_list(Map).

map_iterator(Map) ->
    gb_trees:iterator(Map).

map_next(Iter) ->
    gb_trees:next(Iter).

map_foreach(F, Map) ->
    map_foreach0(F, map_iterator(Map)).

map_foreach0(F, Iter0) ->
    case map_next(Iter0) of
        {Key, Value, Iter1} ->
            F(Key, Value),
            map_foreach0(F, Iter1);
        none ->
            ok
    end.

map_foldl(F, Acc, Map) ->
    map_foldl0(F, Acc, map_iterator(Map)).

map_foldl0(F, Acc, Iter0) ->
    case map_next(Iter0) of
        {Key, Value, Iter1} ->
            map_foldl0(F, F(Key, Value, Acc), Iter1);
        none ->
            Acc
    end.

map_keys(Map) ->
    gb_trees:keys(Map).

get_latest_revision(Stmts, Ctx) ->
    get_latest_revision(Stmts, undefined, Ctx).
get_latest_revision([{Kwd, Arg, _Pos, _Substmts} | Stmts], Latest, Ctx) ->
    if Kwd == 'revision' ->
            if (Arg > Latest) orelse (Latest == undefined) ->
                    get_latest_revision(Stmts, Arg, Ctx);
               true ->
                    get_latest_revision(Stmts, Latest, Ctx)
            end;
       true ->
            case kwd_class(Kwd, Ctx) of
                header ->
                    get_latest_revision(Stmts, Latest, Ctx);
                linkage ->
                    get_latest_revision(Stmts, Latest, Ctx);
                meta ->
                    get_latest_revision(Stmts, Latest, Ctx);
                extension ->
                    get_latest_revision(Stmts, Latest, Ctx);
                body ->
                    Latest
            end
    end;
get_latest_revision([], Latest, _Ctx) ->
    Latest.

%% FIXME: unused function
%% get_namespace([{Kwd, Arg, _Pos, _Substmts} | Stmts]) ->
%%     if Kwd == 'namespace' ->
%%             Arg;
%%        true ->
%%             case kwd_class(Kwd, Ctx) of
%%                 header ->
%%                     get_namespace(Stmts);
%%                 extension ->
%%                     get_namespace(Stmts);
%%                 _ ->
%%                     undefined
%%             end
%%     end;
%% get_namespace([]) ->
%%     undefined.

-spec kwd_class(keyword(), #yctx{}) ->
          header | linkage | meta | extension | body.
kwd_class('yang-version', _Ctx)  -> header;
kwd_class('namespace', _Ctx)     -> header;
kwd_class('prefix', _Ctx)        -> header;
kwd_class('belongs-to', _Ctx)    -> header;
kwd_class('import', _Ctx)        -> linkage;
kwd_class('include', _Ctx)       -> linkage;
kwd_class('organization', _Ctx)  -> meta;
kwd_class('contact', _Ctx)       -> meta;
kwd_class('description', _Ctx)   -> meta;
kwd_class('reference', _Ctx)     -> meta;
kwd_class('revision', _Ctx)      -> revision;
kwd_class({_Module, _Kwd} = Kwd, Ctx) ->
    #yctx{env = #env{data_definition_stmts = Map}} = Ctx,
    case map_lookup(Kwd, Map) of
        {value, _} ->
            body;
        none ->
            extension
    end;
kwd_class(_, _)               -> body.

kwd_body_class('uses', _Ctx) ->
    uses;
kwd_body_class(Kwd, #yctx{env = #env{data_definition_stmts = Map}}) ->
    case map_lookup(Kwd, Map) of
        {value, Kind} ->
            {data_definition_stmt, Kind};
        none ->
            undefined
    end.

has_mandatory_descendant(#sn{children = Chs}) ->
    chk_any_mandatory(Chs) /= false.

get_substmt_arg(Stmt, Keyword, DefVal) ->
    get_substmt_arg(Stmt, Keyword, fun(X) -> X end, DefVal).
get_substmt_arg(Stmt, Keyword, ValF, DefVal) ->
    Substmts = stmt_substmts(Stmt),
    get_stmts_arg(Substmts, Keyword, ValF, DefVal).

get_stmts_arg(Substmts, Keyword, DefVal) ->
    get_stmts_arg(Substmts, Keyword, fun(X) -> X end, DefVal).
get_stmts_arg(Substmts, Keyword, ValF, DefVal) ->
    case search_one_stmt(Keyword, Substmts) of
        false ->
            DefVal;
        SubStmt ->
            ValF(stmt_arg(SubStmt))
    end.

if_substmt(Keyword, Arg, What, Then, Else) ->
    case search_one_stmt(Keyword, Arg, substmts(What)) of
        false ->
            Else;
        _found ->
            Then
    end.

if_substmt(Keyword, What, Then, Else) ->
    case search_one_stmt(Keyword, substmts(What)) of
        false ->
            Else;
        _found ->
            Then
    end.

substmt(Keyword, What) ->
    yang:search_one_stmt(Keyword, substmts(What)).

substmts(#sn{stmt = Stmt}) ->
    yang:stmt_substmts(Stmt);
substmts(Stmt) when ?is_stmt(Stmt) ->
    yang:stmt_substmts(Stmt);
substmts(Stmts) when is_list(Stmts) ->
    Stmts.

substmt_present(Keyword, What) ->
    if_substmt(Keyword, What, true, false).

search_one_stmt(Kwd, [H | _]) when element(1, H) == Kwd->
    H;
search_one_stmt(Kwd, [_ | T]) ->
    search_one_stmt(Kwd, T);
search_one_stmt(_, []) ->
    false.

search_one_stmt(Kwd, Arg, [{Kwd, Arg, _, _} = H | _]) ->
    H;
search_one_stmt(Kwd, Arg, [_ | T]) ->
    search_one_stmt(Kwd, Arg, T);
search_one_stmt(_, _, []) ->
    false.

search_one_substmt(Kwd, {_, _, _, Stmts}) ->
    search_one_stmt(Kwd, Stmts).

search_one_substmt(Kwd, Arg, {_, _, _, Stmts}) ->
    search_one_stmt(Kwd, Arg, Stmts).

search_all_stmts(Kwd, Stmts) when is_list(Stmts) ->
    [S || S <- Stmts, element(1, S) == Kwd];
search_all_stmts(Kwd, {_, _, _, Stmts}) ->
    search_all_stmts(Kwd, Stmts).

get_extension_handler(_Kwd, _Ctx) ->
    false.

run_hooks(HookField, Ctx0) ->
    lists:foldl(fun(HookF, Ctx1) ->
                        HookF(Ctx1)
                end, Ctx0,
                element(HookField, Ctx0#yctx.hooks)).

run_hooks(HookField, Ctx0, Arg0) ->
    lists:foldl(fun(HookF, {Ctx1, Arg1}) ->
                        HookF(Ctx1, Arg1)
                end, {Ctx0, Arg0},
                element(HookField, Ctx0#yctx.hooks)).

run_hooks(HookField, Ctx0, Arg0, Extra) ->
    lists:foldl(fun(HookF, {Ctx1, Arg1}) ->
                        HookF(Ctx1, Arg1, Extra)
                end, {Ctx0, Arg0},
                element(HookField, Ctx0#yctx.hooks)).

sn_pos(#sn{stmt = Stmt}) ->
    stmt_pos(Stmt).

stmt_keyword({Keyword, _Arg, _Pos, _Substmts}) ->
    Keyword.

stmt_arg({_Keyword, Arg, _Pos, _Substmts}) ->
    Arg.

stmt_pos({_Keyword, _Arg, Pos, _Substmts}) ->
    Pos.

stmt_substmts({_Keyword, _Arg, _Pos, Substmts}) ->
    Substmts.

parse_schema_nodeid(_IsTopLevel = true, Arg, Pos, M, Ctx) ->
    parse_absolute_schema_nodeid(Arg, Pos, M, Ctx);
parse_schema_nodeid(_IsTopLevel = false, Arg, Pos, M, Ctx) ->
    parse_descendant_schema_nodeid(Arg, Pos, M, Ctx).

parse_absolute_schema_nodeid(<<$/, Arg/binary>>, Pos, M, Ctx) ->
    parse_schema_nodeid(Arg, Pos, M, Ctx);
parse_absolute_schema_nodeid(Arg, Pos, _, Ctx) ->
    {error, add_error(Ctx, Pos, 'YANG_ERR_BAD_ARGUMENT',
                      [Arg, "absolute-schema-nodeid"])}.

parse_descendant_schema_nodeid(<<$/, _/binary>> = Arg, Pos, _M, Ctx) ->
    {error, add_error(Ctx, Pos, 'YANG_ERR_BAD_ARGUMENT',
                      [Arg, "descendant-schema-nodeid"])};
parse_descendant_schema_nodeid(Arg, Pos, M, Ctx) ->
    parse_schema_nodeid(Arg, Pos, M, Ctx).

parse_schema_nodeid(Arg, Pos, M, Ctx0) ->
    #module{name = MyModuleName, prefix_map = PrefixMap} = M,
    SchemaNodeId = parse_schema_nodeid_str(Arg),
    try
        {NormalizedSchemaNodeId, Ctx3} =
            lists:mapfoldl(
              fun({Prefix, Name}, Ctx1) ->
                      case map_lookup(Prefix, PrefixMap) of
                          {value, '$self'} ->
                              {{child, Name}, Ctx1};
                          {value, ModuleName} ->
                              Ctx2 = mark_import_as_used(MyModuleName,
                                                         Prefix, Ctx1),
                              {{child, {ModuleName, Name}}, Ctx2};
                          none ->
                              throw({error,
                                     add_error(Ctx1, Pos,
                                               'YANG_ERR_PREFIX_NOT_FOUND',
                                               [Prefix])})
                      end;
                 (Name, Ctx1) ->
                      {{child, Name}, Ctx1}
              end, Ctx0, SchemaNodeId),
        {ok, NormalizedSchemaNodeId, Ctx3}
    catch
        throw:{error, Ctx1} ->
            {error, Ctx1}
    end.

%% Pre: arg is syntactically correct
parse_schema_nodeid_str(Arg) ->
    NodeIds = re:split(Arg, <<"/">>, [{return, binary}]),
    [parse_nodeid(NodeId) || NodeId <- NodeIds].

parse_nodeid(Str) ->
    case re:split(Str, ":", [{return, binary}]) of
        [LocalName] ->
            ?b2a(LocalName);
        [Prefix, Name] ->
            {?b2a(Prefix), ?b2a(Name)}
    end.

-define(iolf(Fmt, Args), io_lib:format(Fmt, Args)).

identifier() ->
    "[_A-Za-z][._\\-A-Za-z0-9]*".

prefix() ->
    identifier().

keyword() ->
    %% Note, named subexpressions means we can only have one of these
    ?iolf("((?<prefix>~s):)?(?<identifier>~s)", [prefix(), identifier()]).

node_id() ->
    keyword().

identifier_ref() ->
    node_id().

topo_sort(Pairs) ->
    topo_iterate(Pairs, [], topo_all(Pairs)).

topo_iterate([], L, All) ->
    {ok, topo_remove_duplicates(L ++ topo_subtract(All, L))};
topo_iterate(Pairs, L, All) ->
    case topo_subtract(topo_lhs(Pairs), topo_rhs(Pairs)) of
        []  ->
            {cycle, Pairs};
        Lhs ->
            topo_iterate(topo_remove_pairs(Lhs, Pairs), L ++ Lhs, All)
    end.

topo_all(L) -> topo_lhs(L) ++ topo_rhs(L).
topo_lhs(L) -> [X || {X,_} <- L].
topo_rhs(L) -> [Y || {_,Y} <- L].

topo_subtract(L1, L2) ->
    [X || X <- L1, not lists:member(X, L2)].

topo_remove_duplicates([H|T]) ->
  case lists:member(H, T) of
      true  -> topo_remove_duplicates(T);
      false -> [H|topo_remove_duplicates(T)]
  end;
topo_remove_duplicates([]) ->
    [].

topo_remove_pairs(L1, L2) ->
    [{X,Y} || {X,Y} <- L2, not lists:member(X, L1)].

-ifdef(debugX).
tst([FileName]) when is_atom(FileName) ->
    tst(?a2l(FileName));
tst(FileName) ->
    case add_file(init_ctx(new_ctx(load_plugins([])), []), FileName) of
        {false, Ctx, _} ->
            yang_error:print_errors(Ctx, false),
            error;
        {true, Ctx, _M} ->
            pp_module(_M),
            yang_error:print_errors(Ctx, false)
    end.

pp_module(M) ->
    ?iof("module: ~p\n", [M#module.name]),
    ?iof("children: \n"),
    pp_children(M#module.children, "  ").

pp_children([H | T], Indent) ->
    ?iof("~s~p ~p~s\n", [Indent, H#sn.kind, H#sn.name, pp_type(H#sn.type)]),
    pp_default(H#sn.default, " " ++ Indent),
    pp_children(H#sn.children, "  " ++ Indent),
    pp_children(T, Indent);
pp_children([], _Indent) ->
    ok.

pp_type(undefined) ->
    "";
%pp_type(#type{base = Typedef} = Type) when is_record(Typedef, typedef) ->
%    pp_type(Typedef#typedef.name, Type#type.type_spec);
pp_type(Type) ->
    pp_type(Type#type.base, Type#type.type_spec).

pp_type(Name, Spec) ->
    io_lib:format(" ~p ~p", [Name, Spec]).

pp_default(undefined, _Indent) ->
    ok;
pp_default(Default, Indent) ->
    ?iof("~sdefault: ~p~n", [Indent, Default]).
-endif.

%pp_children([H | T], Indent) ->
%    ?iof("~s~p ~p\n", [Indent, H#sn.kind, H#sn.name]),
%    pp_children(H#sn.children, "  " ++ Indent),
%    pp_children(T, Indent);
%pp_children([], _Indent) ->
%    ok.
