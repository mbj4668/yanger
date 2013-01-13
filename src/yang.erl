-module(yang).

-export([load_plugins/1, new_ctx/1, init_ctx/2,
         add_file/2]).
-export([tst/1, a/0]).

-export([stmt_keyword/1, stmt_arg/1, stmt_pos/1, stmt_substmts/1]).
-export([search_one_stmt/2, search_one_stmt/3]).

-export([map_new/0, map_insert/3, map_update/3,
         map_lookup/2, map_get/2, map_delete/2, map_to_list/1,
         map_iterator/1, map_next/1,
         map_foldl/3]).

%% TAILF:
%%   reject choice in case

%% handle revisions.  we cannot handle more than one revision in confd anyway.

%% do all this in one process, so that we can use pointers to terms
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

%% parse a data def. statement; in a module, submodule, grouping, ...
%%   need to pass the prefix map
%%   keep track of parent's config property (undefined in groupings)
%% 1. build #typedefs{} and #groupings{}, or reuse parent's if there are
%%    no new on this level
%% 2. set #sn.prefix_map to point to our prefix map.  this will be used
%%    to validate 'when', 'must', 'leafref' (and extensions like tailf:symlink)
%% 3. leaf:
%%      find correct base type, build a #type{} or #typedef record
%%      chk mandatory & default
%%      verify default value against type
%%    list:
%%      do all leafs
%%      check keys and unique, set .keys to be pointers into .children
%%    leaf-list:
%%      find correct base type, build a #type{} or #typedef record
%%    uses:
%%      find the grouping, inline it, while applying any refines
%%      apply relative augments

%% Deviations: first create the #module.  Then modify the tree from
%%   deviations.  Maybe modify the #sn.stmt.children list?

%% Annotations: Add to #sn.stmt.children list.

%% Plugins:
%%   o  dynamic discovery of plugins (beam / erl files -
%%           maybe with special names?)
%%   o  call plugin_init() in each plugin
%%   o  let each plugin return a record with "registrations"
%%      - grammar, arg types
%%      - extension module handling
%%      - validation hooks
%%      - error codes
%%      - output formats
%%      - command line options
%%      - handling for certain types (e.g. inet:ipv4-address)

-include("yang.hrl").

-export_type([keyword/0, builtin_type_name/0, map/2, map/0, prefix_map/0,
              validate_status/0, stmt/0, pos/0, yang_identifier/0]).

-type pos() :: {FileName :: string(), Line :: integer()}
             | {FileName :: string(), Line :: integer(), Col :: integer()}.

-type keyword() :: CoreName :: atom()
                 | {ModuleName :: atom(), ExtensionName :: atom()}.

-type yang_identifier() :: LocalName :: atom()
                         | {ModuleName :: atom(), ExternalName :: atom()}.

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

-type map(_Key, _Val) :: gb_tree().
-type map() :: map(term(), term()).

%% Each module and submodule has its own prefix map
%% 'self' represents the prefix to the current module.
-type prefix_map() :: map(Prefix :: atom(),
                          {ModuleName :: atom(), Revision :: atom()}
                        | 'self').

%% used to find cyclic definitions
-type validate_status() :: 'undefined' | 'processing' | 'done'.

%% see yang_parser_nif.c
-type arg_type() :: string() | atom() | integer() | identifier_ref().

-type stmt() :: {keyword(), Arg :: arg_type(), pos(), [stmt()]}.

-type raw_identifier_ref() :: atom() | {Prefix :: atom(), Name :: atom()}.

-type identifier_ref() :: atom() | {ModuleName :: atom(), Name :: atom()}.

-type schema_nodeid() :: yang_identifier().

%% Ugh!  stupid erlang compiler!! I cannot refer to #grouping{} before
%% it has been defined, but I can refer to grouping_rec()...
-type grouping_rec() :: #grouping{}.
-type typedef_rec() :: #typedef{}.

-spec load_plugins(PluginPath :: [Dir :: string()]) ->
        Plugins :: [ModuleName :: atom()].
%% @doc Loads all available plugin modules.
%% Call this function once!
load_plugins(PluginPath0) ->
    PluginPath1 =
        %% Always add plugins from the priv dir
        code:priv_dir(yanger) ++
        %% Always add plugins from an environment variable
        case os:getenv("YANGER_PLUGINPATH") of
            Str when is_list(Str) ->
                PluginPath0 ++ string:tokens(Str);
            false ->
                PluginPath0
        end,
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
          end, [], PluginPath1),
    Plugins.

load_file(Dir, FName) ->
    case filename:extension(FName) of
        ".beam" ->
            FullFName = filename:join(Dir, FName),
            case file:read_file(FullFName) of
                {ok, Bin} ->
                    Module = filename:basename(FName),
                    case code:load_binary(Module, FullFName, Bin) of
                        {module, Module} ->
                            {true, Module};
                        _ ->
                            false
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
    Ctx0 = #yctx{error_codes = ErrorCodes,
                 plugins = Plugins},
    lists:foldl(fun(M, Ctx1) -> M:init(Ctx1) end, Ctx0, Plugins).

-spec init_ctx(#yctx{}, SearchPath :: [Dir :: string()]) ->
        #yctx{}.
%% @doc Initialize the YANG compiler context.
init_ctx(Ctx0, ExtraSearchPath) ->
    SearchPath = ExtraSearchPath ++ search_path(),
    Files = lists:foldl(
              fun(Dir, Acc) ->
                      case file:list_dir(Dir) of
                          {ok, L} ->
                              L ++ Acc;
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
                                   map_insert({Modname, Revision}, FName, M);
                               _ ->
                                   M
                           end;
                       _ ->
                           M
                   end
           end, map_new(), Files),
    Ctx1 = Ctx0#yctx{search_path = SearchPath,
                     modrevs = map_new(),
                     files = Fm % FIXME: scan path, add 'parse_to_body()'
                    },
    %% FIXME: call plugin hooks?
    Ctx1.

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
                   filename:join([Home, "yang", "modules"])
           end
        ++ case os:getenv("YANG_INSTALL_DIR") of
               false ->
                   filename:join(["/usr", "share", "yang", "modules"]);
               InstalLDir ->
                   filename:join([InstalLDir, "yang", "modules"])
           end.

%% called when a filename is given on the cmdline
-spec add_file(#yctx{}, FileName :: string()) ->
        {Success :: boolean(), #yctx{}}.
add_file(Ctx, FileName) ->
    try
        add_file0(Ctx, FileName)
    catch
        error:_X ->
            %% since we keep the entire data structure on the heap,
            %% our crashes can be huge.  mask the error.
            %% FIXME: use another mechanism than env var?
            case os:getenv("YANGERROR") of
                false ->
                    erlang:error({yang_internal_error,
                                  erlang:get_stacktrace()});
                _ ->
                    erlang:error({_X, erlang:get_stacktrace()})
            end
    end.

add_file0(Ctx, FileName) ->
    case yang_parser:parse(FileName, Ctx#yctx.canonical) of
        {ok, Stmts} ->
            case parse_file_name(FileName) of
                {ok, FileModuleName, FileRevision} ->
                    add_parsed_module(Ctx, Stmts, FileName,
                                      FileModuleName, FileRevision,
                                      _ExpectFailLevel = warning);
                error ->
                    add_parsed_module(Ctx, Stmts, FileName,
                                      undefined, undefined,
                                      _ExpectFailLevel = warning)
            end;
        {error, LLErrors} ->
            {false, add_llerrors(LLErrors, Ctx)}
    end.

%% START DEBUG CODE
a() ->
    tst("big.yang").

tst([FileName]) when is_atom(FileName) ->
    tst(?a2l(FileName));
tst(FileName) ->
    case add_file(init_ctx([], load_plugins([])), FileName) of
        {false, Ctx} ->
            yang_errors:print_errors(Ctx, false),
            error;
        {true, Ctx, _M} ->
%            pp_module(_M),
            yang_error:print_errors(Ctx, false)
    end.
%% END DEBUG CODE

parse_file_name(FileName) ->
    BaseName = filename:basename(FileName, ".yang"),
    case string:tokens(BaseName, "@") of
        [ModuleName] ->
            {ok, ?l2a(ModuleName), undefined};
        [ModuleName, Revision] ->
            {ok, ?l2a(ModuleName), Revision};
        _ ->
            error
    end.

%% Searches for ModuleName with Revision (which can be undefined).
%% If the module is not already present, it is added to the ctx.
search_module(Ctx, FromPos, ModKeyword, ModuleName, Revision) ->
    %% first check if the module is present already
    case map_lookup({ModuleName, Revision}, Ctx#yctx.modrevs) of
        {value, processing} ->
            {false, add_error(Ctx, FromPos,
                              'YANG_ERR_CIRCULAR_DEPENDENCY',
                              [ModKeyword, ModuleName])};
        {value, M} ->
            {true, Ctx, M};
        none ->
            %% if not, check if the exact file is available
            case map_lookup({ModuleName, Revision}, Ctx#yctx.files) of
                {value, FileName} ->
                    %% FIXME:
                    %% when we import we don't want to check the canonical
                    %% order.  however, if we include one of our own
                    %% submodules, we do.
                    case yang_parser:parse(FileName, _Canonical = false) of
                        {ok, Stmts} ->
                            add_parsed_module(Ctx, Stmts, FileName,
                                              ModuleName, Revision,
                                              _ExpectFailLevel = error);
                        {error, LLErrors} ->
                            {false, add_llerrors(LLErrors, Ctx)}
                    end;
                none ->
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
            end
    end.

%% called for modules and submodules
add_parsed_module(Ctx0, [{ModKeyword, ModuleName, Pos, Substmts}] = S, FileName,
                  ExpectedModuleName, ExpectedRevision, ExpectFailLevel) ->
    ModuleRevision = get_latest_revision(Substmts),
    %% check that the module and revision is really the one we're looking for
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
                    Mods1 = map_insert({ModuleName, ModuleRevision}, processing,
                                       Ctx1#yctx.modrevs),
                    Ctx2 = Ctx1#yctx{modrevs = Mods1},
                    S1 = run_hooks(#hooks.pre_parse_module, Ctx2, S),
                    case parse_module(S1, Ctx2) of
                        {true, Ctx3, M0} ->
                            M = M0#module{filename = FileName,
                                          revision = ModuleRevision},
                            Mods2 = map_update({ModuleName, ModuleRevision}, M,
                                              Ctx3#yctx.modrevs),
                            {true, Ctx3#yctx{modrevs = Mods2}, M};
                        Else ->
                            Else
                    end
            end
    end.

%% Pre: All statements are grammatically correct
%% This implies that we don't have to check that mandatory statements
%% are present, and that arguments are of correct type.
parse_module([{Keyword, ModuleName, _Pos, Substmts} = Stmt], Ctx) ->
    PrefixMap = mk_prefix_map(Substmts),
    M = #module{name = ModuleName,
                modulename = ModuleName,
                kind = Keyword,
                stmt = Stmt,
                prefix_map = PrefixMap},
    parse_header(Substmts, M, Ctx).

%% Handle header stmts
parse_header([{Kwd, Arg, _Pos, Substmts} = H | T] = Stmts, M, Ctx) ->
    case Kwd of
        'namespace' ->
            parse_header(T, M#module{namespace = Arg}, Ctx);
        'prefix' ->
            parse_header(T, M#module{prefix = Arg}, Ctx);
        'belongs-to' ->
            {_, Prefix, _, _} = search_one_stmt('prefix', Substmts),
            parse_header(T, M#module{modulename = Arg, prefix = Prefix}, Ctx);
        _ ->
            case kwd_class(Kwd) of
                header ->
                    parse_header(T, M, Ctx);
                extension ->
                    case get_extension_handler(Kwd, Ctx) of
                        {true, HandlerMod} ->
                            {M1, Ctx1} = HandlerMod:parse_header(H, M, Ctx),
                            parse_header(T, M1, Ctx1);
                        false ->
                            parse_header(T, M, Ctx)
                    end;
                _ ->
                    parse_meta(Stmts, M, Ctx)
            end
    end;
parse_header([], M, Ctx) ->
    {true, Ctx, M}.

%% Pre: header stmts are handled
%% Handle meta stmts
parse_meta([{Kwd, _Arg, _Pos, _Substmts} = H | T] = Stmts, M, Ctx) ->
    case kwd_class(Kwd) of
        meta ->
            %% meta statements are not handled specifically; they are
            %% just kept as substmts.
            %% FIXME: make proper record fields for them?
            parse_meta(T, M, Ctx);
        extension ->
            case get_extension_handler(Kwd, Ctx) of
                {true, HandlerMod} ->
                    {M1, Ctx1} = HandlerMod:parse_meta(H, M, Ctx),
                    parse_meta(T, M1, Ctx1);
                false ->
                    parse_meta(T, M, Ctx)
            end;
        _ ->
            parse_linkage(Stmts, M, Ctx)
    end;
parse_meta([], M, Ctx) ->
    {true, Ctx, M}.

%% Pre: meta stmts are handled
%% Handle linkage stmts
parse_linkage([{Kwd, Arg, Pos, Substmts} = H | T] = Stmts, M, Ctx) ->
    case kwd_class(Kwd) of
        linkage ->
            %% handle import/include of module/submodule Arg
            case search_one_stmt('revision', Substmts) of
                {_, Revision, _, _} ->
                    ok;
                false ->
                    Revision = undefined
            end,
            ModKeyword =
                case Kwd of
                    'import' -> 'module';
                    'include' -> 'submodule'
                end,
            case search_module(Ctx, Pos, ModKeyword, Arg, Revision) of
                {true, Ctx1, SubM} when Kwd == 'include' ->
                    Ctx2 = v_include(M, Pos, SubM, Ctx1),
                    M1 = M#module{submodules = [{SubM, Pos} |
                                                M#module.submodules]},
                    parse_linkage(T, M1, Ctx2);
                {true, Ctx1, _} ->
                    parse_linkage(T, M, Ctx1);
                {false, Ctx1} ->
                    parse_linkage(T, M, Ctx1)
            end;
        extension ->
            case get_extension_handler(Kwd, Ctx) of
                {true, HandlerMod} ->
                    {M1, Ctx1} = HandlerMod:parse_linkage(H, M, Ctx),
                    parse_linkage(T, M1, Ctx1);
                false ->
                    parse_linkage(T, M, Ctx)
            end;
        _ ->
            Ctx1 = v_all_includes(M, Ctx),
            parse_revision(Stmts, M, Ctx1)
    end;
parse_linkage([], M, Ctx) ->
    {true, Ctx, M}.

v_include(M, Pos, SubM, Ctx1) ->
    if SubM#module.kind /= 'submodule' ->
            {_, _, SubPos, _} = SubM#module.stmt,
            add_error(Ctx1, Pos, 'YANG_ERR_BAD_INCLUDE',
                      [yang_error:fmt_pos(SubPos)]);
       SubM#module.modulename /= M#module.modulename ->
            add_error(Ctx1, Pos, 'YANG_ERR_BAD_BELONGS_TO',
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

%% Pre: linkage stmts are handled
%% Handle revision stmts
parse_revision(Stmts, M, Ctx) ->
    parse_revision(Stmts, M, Ctx, undefined).

parse_revision([{'revision', Arg, Pos, _Substmts} | T], M, Ctx, PrevRevision) ->
    if Arg > PrevRevision, PrevRevision /= undefined ->
            Ctx1 = add_error(Ctx, Pos, 'YANG_ERR_REVISION_ORDER', []),
            parse_revision(T, M, Ctx1, Arg);
       true ->
            parse_revision(T, M, Ctx, Arg)
    end;
parse_revision([{Kwd, _Arg, _, _} = H | T] = Stmts, M, Ctx, PrevRevision) ->
    case kwd_class(Kwd) of
        extension ->
            case get_extension_handler(Kwd, Ctx) of
                {true, HandlerMod} ->
                    {M1, Ctx1} = HandlerMod:parse_revision(H, M, Ctx),
                    parse_revision(T, M1, Ctx1, PrevRevision);
                false ->
                    parse_revision(T, M, Ctx, PrevRevision)
            end;
        _ ->
            parse_body(Stmts, M, Ctx)
    end;
parse_revision([], M, Ctx, _PrevRevision) ->
    {true, Ctx, M}.

%% Pre: revision stmts are handled
%% Handle body stmts
parse_body(Stmts, M0, Ctx0) ->
    %% Scan all stmts and build the maps for typedefs etc, that
    %% later will be used in the other stmts.
    {M1, Ctx1} = mk_module_maps(Stmts, M0, Ctx0),
    %% Create schema node records for all children.
    {Children, _, Ctx2} =
        mk_children0(Stmts, undefined,
                     M1#module.typedefs, M1#module.groupings, M1, Ctx1,
                     _Config = true, []),
    %% Validate that all children have unique names
    Ctx3 = v_unique_names(Children, Ctx2),
    %% Build the augments list.
    {Augments, Ctx4} =
        mk_augments(Stmts, M1#module.typedefs, M1#module.groupings, M1, Ctx3,
                    []),
    {LocalAugments, RemoteAugments} = sort_augments(Augments),
    %% Apply augments.
    {AugmentedChildren, Ctx5} =
        augment_children(LocalAugments, Children, Ctx4),
    M2 = M1#module{children = AugmentedChildren, augments = Augments},
    {true, Ctx5, M2}.

mk_module_maps(Stmts, M, Ctx0) ->
    mk_module_maps(Stmts, map_new(), map_new(), map_new(),
                   map_new(), map_new(), M, Ctx0).

mk_module_maps([{Kwd, Arg, _, Substmts} = S | Stmts],
               Ts, Gs, Is, Fs, Es, M, Ctx0) ->
    %% Build maps with non-validated items.  These are validated below.
    case Kwd of
        'typedef' ->
            T = #typedef{name = Arg, stmt = S, v_status = undefined},
            {Ts1, Ctx1} =
                add_to_definitions_map(Arg, T, #typedef.stmt, Ts, Ctx0),
            mk_module_maps(Stmts, Ts1, Gs, Is, Fs, Es, M, Ctx1);
        'grouping' ->
            G = #grouping{name = Arg, stmt = S, v_status = undefined},
            {Gs1, Ctx1} =
                add_to_definitions_map(Arg, G, #grouping.stmt, Gs, Ctx0),
            mk_module_maps(Stmts, Ts, Gs1, Is, Fs, Es, M, Ctx1);
        'identity' ->
            I = #identity{name = Arg, stmt = S, v_status = undefined},
            {Is1, Ctx1} =
                add_to_definitions_map(Arg, I, #identity.stmt, Is, Ctx0),
            mk_module_maps(Stmts, Ts, Gs, Is1, Fs, Es, M, Ctx1);
        'feature' ->
            F = #feature{name = Arg, stmt = S, v_status = undefined},
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
            E = #extension{name = Arg, arg = ExtArg, stmt = S},
            {Es1, Ctx1} =
                add_to_definitions_map(Arg, E, #extension.stmt, Es, Ctx0),
            mk_module_maps(Stmts, Ts, Gs, Is, Fs, Es1, M, Ctx1);
        _ ->
            mk_module_maps(Stmts, Ts, Gs, Is, Fs, Es, M, Ctx0)
    end;
mk_module_maps([], Ts, Gs, Is, Fs, Es, M, Ctx0) ->
    %% Start with the map with non-validated items.  Copy the
    %% submodules' definitions into the map, so they are known to our
    %% definitions.  Then validate our definitions, and update the
    %% map.
    {IsMap0, Ctx1} =
        add_from_submodules(fun(SubM) -> SubM#module.identities end,
                            fun(Item) -> Item#identity.stmt end, M, Is, Ctx0),
    {IsMap1, Ctx2} = mk_identities_map(Is, IsMap0, M, Ctx1),

    {FsMap0, Ctx3} =
        add_from_submodules(fun(SubM) -> SubM#module.features end,
                            fun(Item) -> Item#feature.stmt end, M, Fs, Ctx2),
    {FsMap1, Ctx4} = mk_features_map(Fs, FsMap0, M, Ctx3),

    {EsMap1, Ctx5} =
        add_from_submodules(fun(SubM) -> SubM#module.extensions end,
                            fun(Item) -> Item#extension.stmt end, M, Es, Ctx4),
    %% For extensions, we don't need to do any extra processing

    {TsMap0, Ctx6} =
        add_from_submodules(fun(SubM) -> SubM#module.typedefs#typedefs.map end,
                            fun(Item) -> Item#typedef.stmt end, M, Ts, Ctx5),
    {Typedefs, Ctx7} = mk_typedefs(Ts, TsMap0, undefined, M, Ctx6),

    {GsMap0, Ctx8} =
        add_from_submodules(
          fun(SubM) -> SubM#module.groupings#groupings.map end,
          fun(Item) -> Item#grouping.stmt end, M, Gs, Ctx7),
    {Groupings, Ctx9} = mk_groupings(Gs, GsMap0, Typedefs, undefined, M, Ctx8),

    {M#module{identities = IsMap1,
              features = FsMap1,
              extensions = EsMap1,
              typedefs = Typedefs,
              groupings = Groupings
             },
     Ctx9}.

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
              [Keyword, Name, yang_error:fmt_pos(PrevPos)]).

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
            %% unhandled statment
            Map1 = map_update(Name, Id0#identity{v_status = processing}, Map0),
            {Map2, Ctx1, IdRef} =
                case search_one_stmt('base', Substmts) of
                    BaseStmt when ?is_stmt(BaseStmt) ->
                        chk_base(BaseStmt, Map1, M, Ctx0);
                    false ->
                        {Map1, Ctx0, undefined}
                end,
            Id1 = Id0#identity{base = IdRef, v_status = done},
            Map3 = map_update(Name, Id1, Map2),
            {Map3, Ctx1}
    end.

%% Will add to the map only if non-validated identities are found.
%% If called after mk_identities_map(), the map will not be modified.
chk_base({_, BaseArg, Pos, _}, Map0, M, Ctx0) ->
    case resolve_idref(BaseArg, Pos, M#module.prefix_map, Ctx0) of
        {self, BaseName, Ctx1} ->
            %% reference to local definition
            case map_lookup(BaseName, Map0) of
                {value, BaseId} ->
                    {Map1, Ctx2} = add_identity(BaseId, Map0, M, Ctx1),
                    {Map1, Ctx2, {M#module.modulename, BaseName}};
                none ->
                    Ctx2 = add_error(Ctx1, Pos, 'YANG_ERR_DEFINITION_NOT_FOUND',
                                     ['identity', BaseName]),
                    {Map0, Ctx2, undefined}
            end;
        {{ImportedModuleName, _}, BaseName, Ctx1} ->
            {Map0, Ctx1, {ImportedModuleName, BaseName}};
        {undefined, _BaseName, Ctx1} ->
            %% could not resolve prefix; error is already reported
            {Map0, Ctx1, undefined}
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
            %% unhandled statment
            Map1 = map_update(Name, F0#feature{v_status = processing}, Map0),
            {Map2, Ctx1, _} =
                iterate_stmt(
                  fun(RefStmt, {Map, Ctx, _}) ->
                          {continue, chk_if_feature(RefStmt, Map, M, Ctx)}
                  end, 'if-feature', {Map1, Ctx0, []}, Substmts),
            F1 = F0#feature{v_status = done},
            Map3 = map_update(Name, F1, Map2),
            {Map3, Ctx1}
    end.

chk_if_feature({_, RefArg, Pos, _}, Map0, M, Ctx0) ->
    case resolve_idref(RefArg, Pos, M#module.prefix_map, Ctx0) of
        {self, RefName, Ctx1} ->
            %% reference to local definition
            case map_lookup(RefName, Map0) of
                {value, RefFeature} ->
                    {Map1, Ctx2} = add_feature(RefFeature, Map0, M, Ctx1),
                    {Map1, Ctx2, {M#module.modulename, RefName}};
                none ->
                    Ctx2 = add_error(Ctx1, Pos, 'YANG_ERR_DEFINITION_NOT_FOUND',
                                     ['feature', RefName]),
                    {Map0, Ctx2, undefined}
            end;
        {{ImportedModuleName, _}, RefName, Ctx1} ->
            {Map0, Ctx1, {ImportedModuleName, RefName}};
        {undefined, _RefName, Ctx1} ->
            %% could not resolve prefix; error is already reported
            {Map0, Ctx1, undefined}
    end.

-spec mk_typedefs_and_groupings([stmt()], #typedefs{}, #groupings{},
                                #module{}, #yctx{}) ->
          {#typedefs{}, #groupings{}, #yctx{}}.
mk_typedefs_and_groupings(Stmts, ParentTypedefs, ParentGroupings, M, Ctx0) ->
    {TsMap, GsMap, Ctx1} =
        mk_typedefs_and_groupings(Stmts, map_new(), map_new(), Ctx0),
    {Typedefs, Ctx2} =
        mk_typedefs(TsMap, TsMap, ParentTypedefs, M, Ctx1),
    {Groupings, Ctx3} =
        mk_groupings(GsMap, GsMap, Typedefs, ParentGroupings, M, Ctx2),
    {Typedefs, Groupings, Ctx3}.

mk_typedefs_and_groupings([{Kwd, Arg, _, _Substmts} = S | Stmts],
                          Ts, Gs, Ctx0) ->
    %% Build maps with non-validated items.  These are validated below.
    case Kwd of
        'typedef' ->
            T = #typedef{name = Arg, stmt = S, v_status = undefined},
            {Ts1, Ctx1} =
                add_to_definitions_map(Arg, T, #typedef.stmt, Ts, Ctx0),
            mk_typedefs_and_groupings(Stmts, Ts1, Gs, Ctx1);
        'grouping' ->
            G = #grouping{name = Arg, stmt = S, v_status = undefined},
            {Gs1, Ctx1} =
                add_to_definitions_map(Arg, G, #grouping.stmt, Gs, Ctx0),
            mk_typedefs_and_groupings(Stmts, Ts, Gs1, Ctx1);
        _ ->
            mk_typedefs_and_groupings(Stmts, Ts, Gs, Ctx0)
    end;
mk_typedefs_and_groupings([], Ts, Gs, Ctx) ->
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
                    {ParentTypedefs, Ctx0};
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
            %% unhandled statment
            Map1 = map_update(Name, T0#typedef{v_status = processing}, Map0),
            TypeStmt = search_one_stmt('type', Substmts),
            {Map2, Ctx1, Type} =
                mk_type(TypeStmt, Map1, ParentTypedefs, M, Ctx0),
            T1 = T0#typedef{type = Type, v_status = done},
            Map3 = map_update(Name, T1, Map2),
            {Map3, Ctx1}
    end.

%% If called outside of a typedef, TypedefMap is 'undefined'
mk_type({_, TypeArg, Pos, Substmts}, TypedefMap, ParentTypedefs, M, Ctx0) ->
    Map0 = TypedefMap,
    %% First, figure out our base type name
    {Map3, Ctx3, BaseTypeRef} =
        case is_builtin_type(TypeArg) of
            true ->
                {Map0, Ctx0, TypeArg};
            false ->
                case resolve_idref(TypeArg, Pos, M#module.prefix_map, Ctx0) of
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
                                {Map1, Ctx2, HandledRefTypedef};
                            _ -> % true or none
                                case typedef_lookup(TypeName, ParentTypedefs) of
                                    {value, RefTypedef} ->
                                        {Map0, Ctx1, RefTypedef};
                                    none ->
                                        Ctx2 =
                                            add_error(
                                              Ctx1, Pos,
                                              'YANG_ERR_DEFINITION_NOT_FOUND',
                                              ['typedef', TypeName]),
                                        {Map0, Ctx2, undefined}
                                end
                        end;
                    {{ImportedModuleName, _}, TypeName, Ctx1} ->
                        %% FIXME: lookup the remote typedef
                        {Map0, Ctx1, {ImportedModuleName, TypeName}};
                    {undefined, _BaseName, Ctx1} ->
                        %% could not resolve prefix; error is already reported
                        {Map0, Ctx1, undefined}
                end
        end,
    %% union requires special treatment, since it contains nested types.
    {Map4, Ctx4} =
        case BaseTypeRef of
            'union' ->
                lists:foldl(
                  fun(S, {MapN, CtxN}) ->
                          {MapN1, CtxN1, _} =
                              mk_type(S, MapN, ParentTypedefs, M, CtxN),
                          {MapN1, CtxN1}
                  end,
                  {Map3, Ctx3},
                  search_stmts('type', Substmts));
            _ ->
                {Map3, Ctx3}
        end,
    %% Then, scan for restrictions, and make sure they are allowed,
    %% and build a new type.

    %% FIXME: if BaseTypeRef /= undefined -> yang_types:mk_type_spec()

    {Map4, Ctx4, #type{base = BaseTypeRef}}.

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
    mk_groupings0(map_iterator(FromMap), Map, Typedefs, ParentGroupings,
                  M, Ctx).

mk_groupings0(Iter0, Map0, Typedefs, ParentGroupings, M, Ctx0) ->
    case map_next(Iter0) of
        {Name, _Grouping, Iter1} ->
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
            {Map1, Ctx2} =
                add_grouping(Grouping, Map0, Typedefs, ParentGroupings,
                             M, Ctx1),
            mk_groupings0(Iter1, Map1, Typedefs, ParentGroupings, M, Ctx2);
        none ->
            case map_is_empty(Map0) of
                true when ParentGroupings /= undefined ->
                    {ParentGroupings, Ctx0};
                _ ->
                    {#groupings{map = Map0, parent = ParentGroupings}, Ctx0}
            end
    end.

add_grouping(G0, Map0, ParentTypedefs, ParentGroupings, M, Ctx0) ->
    {_, Name, Pos, Substmts} = G0#grouping.stmt,
    case G0#grouping.v_status of
        done ->
            %% this statement is already handled
            {Map0, Ctx0};
        processing ->
            Ctx1 = add_error(Ctx0, Pos, 'YANG_ERR_CIRCULAR_DEPENDENCY',
                             ['grouping', Name]),
            {Map0, Ctx1};
        undefined ->
            %% unhandled statment
            Map1 = map_update(Name, G0#grouping{v_status = processing}, Map0),
            {Children, Map2, Typedefs, Groupings, Ctx1} =
                mk_children(Substmts, Map1,
                            ParentTypedefs, ParentGroupings, M, Ctx0),
            G1 = G0#grouping{v_status = done,
                             typedefs = Typedefs,
                             groupings = Groupings,
                             children = Children},
            Map3 = map_update(Name, G1, Map2),
            {Map3, Ctx1}
    end.

%% GroupingMap is 'undefined' iff called outside of a grouping definition
mk_children(Stmts, GroupingMap, ParentTypedefs, ParentGroupings, M, Ctx0) ->
    {Typedefs, Groupings, Ctx1} =
        mk_typedefs_and_groupings(Stmts, ParentTypedefs, ParentGroupings,
                                  M, Ctx0),
    {Children, GroupingMap1, Ctx2} =
        mk_children0(Stmts, GroupingMap, Typedefs, Groupings, M, Ctx1,
                     undefined, []),
    {Children, GroupingMap1, Typedefs, Groupings, Ctx2}.

mk_children0([{Kwd, Arg, Pos, Substmts} = Stmt | T], Map0,
             Typedefs, Groupings, M, Ctx, ParentConfig, Acc) ->
    if Kwd == 'uses' ->
            case
                Map0 == undefined orelse map_lookup(Arg, Map0)
            of
                {value, G0} ->
                    {Map1, Ctx1} =
                        add_grouping(G0, Map0, Typedefs, Groupings, M, Ctx),
                    case map_lookup(Arg, Map1) of
                        {value, G1} ->
                            {Acc1, Ctx2} =
                                expand_uses(G1#grouping.children, Substmts,
                                            Typedefs, Groupings, Pos,
                                            M, Ctx1, ParentConfig, Acc),
                            mk_children0(T, Map1, Typedefs, Groupings, M, Ctx2,
                                         ParentConfig, Acc1);
                        none ->
                            %% This means add_grouping returned an error,
                            %% do not add another one.
                            mk_children0(T, Map1, Typedefs, Groupings, M, Ctx1,
                                         ParentConfig, Acc)
                    end;
                _ -> % true or none
                    case grouping_lookup(Arg, Groupings) of
                        {value, G} ->
                            {Acc1, Ctx1} =
                                expand_uses(G#grouping.children, Substmts,
                                            Typedefs, Groupings, Pos,
                                            M, Ctx, ParentConfig, Acc),
                            mk_children0(T, Map0, Typedefs, Groupings, M, Ctx1,
                                         ParentConfig, Acc1);
                        none ->
                            Ctx1 =
                                add_error(
                                  Ctx, Pos,
                                  'YANG_ERR_DEFINITION_NOT_FOUND',
                                  ['grouping', Arg]),
                            mk_children0(T, Map0, Typedefs, Groupings, M, Ctx1,
                                         ParentConfig, Acc)
                    end
            end;
       ?is_data_definition_stmt(Kwd) ->
            {ChConfig, Ctx1} =
                case search_one_stmt('config', Substmts) of
                    {_, Config, CPos, _} ->
                        if ParentConfig == false,
                           Config == true ->
                                {Config,
                                 add_error(Ctx, CPos,
                                           'YANG_ERR_INVALID_CONFIG', [])};
                           true ->
                                {Config, Ctx}
                        end;
                    false ->
                        {ParentConfig, Ctx}
                end,
            Sn0 = #sn{name = Arg,
                      kind = Kwd,
                      prefix_map = M#module.prefix_map,
                      config = ChConfig,
                      stmt = Stmt},
            if Kwd == 'leaf' orelse Kwd == 'leaf-list' ->
                    mk_children0(T, Map0, Typedefs, Groupings, M, Ctx1,
                                 ParentConfig, [Sn0 | Acc]);
               true ->
                    {Typedefs1, Groupings1, Ctx2} =
                        if Kwd == 'choice'
                           orelse Kwd == 'case' ->
                                %% No typedefs or groupings allowed
                                {Typedefs, Groupings, Ctx1};
                           true ->
                                mk_typedefs_and_groupings(Substmts,
                                                          Typedefs, Groupings,
                                                          M, Ctx1)
                        end,
                    {SubChildren, _, Ctx3} =
                        mk_children0(Substmts, Map0, Typedefs1, Groupings1,
                                     M, Ctx2, ChConfig, []),
                    Sn1 = Sn0#sn{typedefs = Typedefs1,
                                 groupings = Groupings1,
                                 children = SubChildren},
                    Sn2 = mk_case_from_shorthand(Sn1),
                    mk_children0(T, Map0, Typedefs, Groupings, M, Ctx3,
                                 ParentConfig, [Sn2 | Acc])
            end;
       true ->
            mk_children0(T, Map0, Typedefs, Groupings, M, Ctx,
                         ParentConfig, Acc)
    end;
mk_children0([], GroupingMap, _Typedefs, _Groupings, _M, Ctx,
             _ParentConfig, Acc) ->
    {Acc, GroupingMap, Ctx}.

expand_uses(GroupingChildren, UsesSubstmts, Typedefs, Groupings,
            UsesPos, M, Ctx, ParentConfig, Children) ->
    %% Build a tree of the refinements target paths.  As the grouping
    %% is expanded, walk down tree and apply refinements.  This way we
    %% refine as we expand, instead of first expand, and then apply
    %% each refinement in order.
    {RefTree, Ctx1} =
        mk_refinement_tree(UsesSubstmts, M#module.prefix_map, [], Ctx),
    {ExpandedChildren, Ctx2} =
        expand_uses2(GroupingChildren, RefTree, UsesPos,
                     M, Ctx1, ParentConfig, Children),
    %% Build the augments list.
    {Augments, Ctx3} =
        mk_augments(UsesSubstmts, Typedefs, Groupings, M, Ctx2, []),
    %% Apply the augments.
    {AugmentedChildren, Ctx4} =
        augment_children(Augments, ExpandedChildren, Ctx3),
    {AugmentedChildren, Ctx4}.

expand_uses2([Sn0 | T], RefTree0, UsesPos, M, Ctx0, ParentConfig, Acc) ->
    %% Check if there are any refinements for this node
    {{Sn1, Ctx1}, Subtree, RefTree2} =
        case take_from_tree(Sn0#sn.name, RefTree0) of
            {value, Refs, Subtree0, RefTree1} ->
                %% apply the refinements on this node
                {apply_refinements(Refs, Sn0, Ctx0), Subtree0, RefTree1};
            false ->
                {{Sn0, Ctx0}, [], RefTree0}
        end,
    %% Possibly update the config property in the expanded node
    {Sn2, Ctx2} =
        case chk_config(ParentConfig, Sn1#sn.config) of
            keep ->
                %% optimization to avoid copy
                {Sn1, Ctx1};
            update ->
                {Sn1#sn{config = ParentConfig}, Ctx1};
            error ->
                {Sn1,
                 add_error(Ctx1, UsesPos,
                           'YANG_ERR_INVALID_CONFIG_USES',
                           [yang_error:fmt_pos(sn_pos(Sn1))])}
        end,
    %% Expand children
    {ExpChildren, Ctx3} =
        expand_uses2(Sn2#sn.children, Subtree, UsesPos, M, Ctx2,
                     Sn2#sn.config, []),
    Sn3 = Sn2#sn{children = ExpChildren},
    %% Expand siblings
    expand_uses2(T, RefTree2, UsesPos, M, Ctx3, ParentConfig, [Sn3 | Acc]);
expand_uses2([], RefTree, _, _, Ctx0, _, Acc) ->
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

%% Augment 'Children' with the schema nodes from 'Augement'.
%% Return the augmented 'Children'.
augment_children(Augments, Children, Ctx0) ->
    {AugmentedChildren, UndefAugNodes, Ctx1} =
        lists:foldl(
          fun(Augment, {AccChildren, UndefAugNodes00, Ctx00}) ->
                  augment_children0(Augment#augment.target_node,
                                    AccChildren, [], undefined,
                                    Augment, UndefAugNodes00, Ctx00)
          end, {Children, [], Ctx0}, Augments),
    %% Report errors for all undefined augment nodes
    Ctx2 =
        lists:foldl(
          fun({Pos, Id}, Ctx00) ->
                  add_error(Ctx00, Pos, 'YANG_ERR_NODE_NOT_FOUND',
                            [yang_error:fmt_yang_identifier(Id)])
          end, Ctx1, UndefAugNodes),
    {AugmentedChildren, Ctx2}.

augment_children0([], Sns, _Acc = [], ParentConfig, Augment,
                  UndefAugNodes, Ctx) ->
    %% We found the children to augment.
    %% Update the children with the augment's children, while looking for
    %% any tmp #sn{}.  Also, check and patch the config property.
    insert_children(Augment#augment.children, Sns, ParentConfig,
                    UndefAugNodes, Ctx);
augment_children0([Id | Ids], [Sn | Sns], Acc, _ParentConfig, Augment,
                  UndefAugNodes, Ctx)
  when Sn#sn.name == Id ->
    %% We found a node in the augment path; we must update it
    %% with new children.
    {AugmentedChildren, UndefAugNodes1, Ctx1} =
        augment_children0(Ids, Sn#sn.children, [], Sn#sn.config, Augment,
                          UndefAugNodes, Ctx),
    {lists:reverse(Acc, [Sn#sn{children = AugmentedChildren} | Sns]),
     UndefAugNodes1, Ctx1};
augment_children0(Ids, [Sn | Sns], Acc, _ParentConfig, Augment,
                  UndefAugNodes, Ctx) ->
    augment_children0(Ids, Sns, [Sn | Acc], Sn#sn.config, Augment,
                      UndefAugNodes, Ctx);
augment_children0([Id | Ids], [], Acc, ParentConfig, Augment,
                  UndefAugNodes, Ctx0) ->
    %% We didn't find the node, create a tmp #sn{}, and keep track of it.
    AugPos = stmt_pos(Augment#augment.stmt),
    UndefAugNodes1 = [{AugPos, Id} | UndefAugNodes],
    {AugmentedChildren, UndefAugNodes2, Ctx1} =
        augment_children0(Ids, [], [], ParentConfig, Augment,
                          UndefAugNodes1, Ctx0),
    {lists:reverse(Acc, [#sn{name = Id,
                             kind = '__tmp_augment__',
                             config = ParentConfig,
                             stmt = {'__tmp_augment__', undefined, AugPos, []},
                             children = AugmentedChildren}]),
     UndefAugNodes2,
     Ctx1}.

insert_children(NewSns, ExistsingSns, ParentConfig, UndefAugNodes0, Ctx0) ->
    lists:foldl(
      fun(NewSn, {Sns, UndefAugNodes1, Ctx1}) ->
              insert_child(NewSn, Sns, [], ParentConfig, UndefAugNodes1, Ctx1)
      end, {ExistsingSns, UndefAugNodes0, Ctx0}, NewSns).

insert_child(NewSn, [Sn | Sns], Acc, ParentConfig, UndefAugNodes, Ctx0)
  when NewSn#sn.name == Sn#sn.name,
       Sn#sn.kind == '__tmp_augment__' ->
    %% We found a tmp node to replace.
    %% Remove the tmp node marker.
    AugPos = sn_pos(Sn),
    UndefAugNodes1 =lists:keydelete(AugPos, 1, UndefAugNodes),
    if NewSn#sn.kind == 'leaf'; NewSn#sn.kind == 'leaf-list' ->
            %% The augment that produced the __tmp_augment__ is bad;
            %% it tried to augment a leaf or leaf-list
            {lists:reverse(Acc, [NewSn | Sns]),
             UndefAugNodes1,
             add_error(Ctx0, AugPos, 'YANG_ERR_BAD_AUGMENT_NODE_TYPE',
                       [NewSn#sn.kind,
                        yang_error:fmt_yang_identifier(NewSn#sn.name)])};
       true ->
            case chk_config(ParentConfig, NewSn#sn.config) of
                error ->
                    {lists:reverse(Acc, [NewSn | Sns]),
                     UndefAugNodes1,
                     add_error(Ctx0, sn_pos(NewSn),
                               'YANG_ERR_INVALID_CONFIG', [])};
                How ->
                    NewConfig =
                        case How of
                            keep -> NewSn#sn.config;
                            update -> ParentConfig
                        end,
                    {NewChildren, UndefAugNodes2, Ctx1} =
                        insert_children(NewSn#sn.children, Sn#sn.children,
                                        NewConfig, UndefAugNodes1, Ctx0),
                    NewSn1 = NewSn#sn{config = NewConfig,
                                      children = NewChildren},
                    {lists:reverse(Acc, [NewSn1 | Sns]), UndefAugNodes2, Ctx1}
            end
    end;
insert_child(NewSn, [Sn | Sns], Acc, _ParentConfig, UndefAugNodes, Ctx)
  when NewSn#sn.name == Sn#sn.name ->
    {lists:reverse(Acc, [Sn | Sns]),
     UndefAugNodes,
     add_dup_error(Ctx, Sn#sn.kind, Sn#sn.name, sn_pos(NewSn), sn_pos(Sn))};
insert_child(NewSn, [Sn | Sns], Acc, ParentConfig, UndefAugNodes, Ctx) ->
    insert_child(NewSn, Sns, [Sn | Acc], ParentConfig, UndefAugNodes, Ctx);
insert_child(NewSn, [], Acc, ParentConfig, UndefAugNodes, Ctx) ->
    case chk_config(ParentConfig, NewSn#sn.config) of
        error ->
            {lists:reverse(Acc, [NewSn]),
             UndefAugNodes,
             add_error(Ctx, sn_pos(NewSn),
                       'YANG_ERR_INVALID_CONFIG', [])};
        keep ->
            {lists:reverse(Acc, [NewSn]), UndefAugNodes, Ctx};
        update ->
            {NewSn1, Ctx1} = update_config(NewSn, ParentConfig, Ctx),
            {lists:reverse(Acc, [NewSn1]), UndefAugNodes, Ctx1}
    end.

chk_config(ParentConfig, CurConfig) ->
    if ParentConfig == false,
       CurConfig == true ->
            error;
       CurConfig == ParentConfig ->
            keep;
       ParentConfig == undefined ->
            keep;
       true ->
            update
    end.

update_config(Sn, ParentConfig, Ctx) ->
    case chk_config(ParentConfig, Sn#sn.config) of
        keep ->
            %% no need to recurse if the config didn't change
            {Sn, Ctx};
        update ->
            {NewChildren, Ctx2} =
                lists:mapfoldl(
                  fun(Ch, Ctx1) ->
                          update_config(Ch, ParentConfig, Ctx1)
                  end,
                  Ctx, Sn#sn.children),
            {Sn#sn{children = NewChildren}, Ctx2};
        error ->
            {Sn,
             add_error(Ctx, sn_pos(Sn),
                       'YANG_ERR_INVALID_CONFIG', [])}
    end.

apply_refinements([{_Pos, Stmts} | T], Sn, Ctx) ->
    {Sn2, Ctx2} =
        lists:foldl(
          fun(Stmt, {Sn1, Ctx1}) ->
                  apply_refinement(Stmt, Sn1, Ctx1)
          end, {Sn, Ctx}, Stmts),
    apply_refinements(T, Sn2, Ctx2);
apply_refinements([], Sn, Ctx) ->
    {Sn, Ctx}.

apply_refinement({Keyword, _Arg, Pos, _Substmts} = Stmt, Sn, Ctx) ->
    case find_refinement(Keyword, Ctx) of
        {ParentKeywords, Op, PostRefinemenetFun} ->
            case lists:member(Sn#sn.kind, ParentKeywords) of
                true ->
                    RefinedSubstmts =
                        case Op of
                            replace ->
                                lists:keystore(Keyword, ?STMT_KEYWORD,
                                               stmt_substmts(Sn#sn.stmt), Stmt);
                            add ->
                                [Stmt | stmt_substmts(Sn#sn.stmt)]
                        end,
                    RefinedStmt =
                        setelement(?STMT_SUBSTMTS, Sn#sn.stmt, RefinedSubstmts),
                    Sn1 = Sn#sn{stmt = RefinedStmt},
                    if PostRefinemenetFun /= undefined ->
                            PostRefinemenetFun(Sn1, Ctx);
                       true ->
                            {Sn1, Ctx}
                    end;
                false ->
                    {Sn, add_error(Ctx, Pos, 'YANG_ERR_ILLEGAL_REFINE',
                                   [Keyword, Sn#sn.kind])}
            end;
        false ->
            {Sn, add_error(Ctx, Pos, 'YANG_ERR_BAD_REFINE', [Keyword])}
    end.

find_refinement(Keyword, Ctx) ->
    case Keyword of
        'description' ->
            {['container', 'leaf', 'leaf-list', 'list', 'choice',
              'case', 'anyxml'], replace, undefined};
        'reference' ->
            {['container', 'leaf', 'leaf-list', 'list', 'choice',
              'case', 'anyxml'], replace, undefined};
        'config' ->
            {['container', 'leaf', 'leaf-list', 'list', 'choice',
              'anyxml'], replace, fun post_config_refinement/2};
        'presence' ->
            {['container'], replace, undefined};
        'must' ->
            {['container', 'leaf', 'leaf-list', 'list', 'anyxml'],
             add, undefined};
        'default' ->
            %% FIXME: validate default value here!!
            {['leaf', 'choice'], replace, undefined};
        'mandatory' ->
            {['leaf', 'choice', 'anyxml'], replace, undefined};
        'min-elements' ->
            {['leaf-list', 'list'], replace, undefined};
        'max-elements' ->
            {['leaf-list', 'list'], replace, undefined};
        _ ->
            %% FIXME: check for plugin-registered refinements in the ctx
            false
    end.

%% Invoked after a config statement has been refined.  Copy the new
%% config value to the #sn record.
post_config_refinement(Sn, Ctx) ->
    case search_one_stmt('config', stmt_substmts(Sn#sn.stmt)) of
        {_, Bool, _, _} when Sn#sn.config /= Bool ->
            {Sn#sn{config = Bool}, Ctx};
        _ ->
            {Sn, Ctx}
    end.

mk_refinement_tree([{'refine', Arg, Pos, Substmts} | T],
                   PrefixMap, Tree, Ctx) ->
    case parse_descendant_schema_nodeid(Arg, Pos, PrefixMap, Ctx) of
        {ok, SchemaNodeId} ->
            Tree1 = add_to_tree(SchemaNodeId, {Pos, Substmts}, Tree),
            mk_refinement_tree(T, PrefixMap, Tree1, Ctx);
        {error, Ctx1} ->
            mk_refinement_tree(T, PrefixMap, Tree, Ctx1)
    end;
mk_refinement_tree([_ | T], PrefixMap, Tree, Ctx) ->
    mk_refinement_tree(T, PrefixMap, Tree, Ctx);
mk_refinement_tree([], _, Tree, Ctx) ->
    {Tree, Ctx}.


-type tree() :: [{Id :: yang_identifier(), Values :: list(), [tree()]}].

-spec add_to_tree([Id :: yang_identifier()], Value :: term(), tree()) -> tree().
add_to_tree([Id], Val, [{Id, Vs, Ch0} | T]) ->
    [{Id, [Val | Vs], Ch0} | T];
add_to_tree([Id | T2], Val, [{Id, Vs, Ch0} | T]) ->
    Ch1 = add_to_tree(T2, Val, Ch0),
    [{Id, Vs, Ch1} | T];
add_to_tree(L, Val, [H | T]) ->
    [H | add_to_tree(L, Val, T)];
add_to_tree(L, Val, []) ->
    mk_tree(L, Val).

mk_tree([H], Val) ->
    [{H, [Val], []}];
mk_tree([H | T], Val) ->
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
        {stop, S1} ->
            {stop, S1};
        {continue, S1} ->
            iterate_tree0(F, S1, T);
        {recurse, S1} ->
            case iterate_tree0(F, S1, Subtree) of
                {stop, S2} ->
                    {stop, S2};
                {_, S2} ->
                    iterate_tree0(F, S2, T)
            end
    end;
iterate_tree0(_, S, []) ->
    {recurse, S}.


%% RFC 6020, sect. 7.9.2
mk_case_from_shorthand(#sn{kind = 'choice', children = Children0} = ChSn) ->
    Children1 =
        lists:map(
          fun(#sn{kind = 'case'} = Sn) ->
                  Sn;
             (#sn{name = Name, stmt = {_Keyword, Arg, Pos, _} = Stmt} = Sn) ->
                  %% shorthand, add case
                  #sn{name = Name, kind = 'case', children = [Sn],
                      stmt = {'case', Arg, Pos, [Stmt]}}
          end, Children0),
    ChSn#sn{children = Children1};
mk_case_from_shorthand(Sn) ->
    Sn.

mk_augments([{'augment', Arg, Pos, Substmts} = Stmt| T], Typedefs, Groupings,
            M, Ctx0, Acc) ->
    {Children, _, Ctx1} =
        %% Create the children w/o explicit config property.
        %% The #sn{} nodes will get the correct config later when
        %% they are inserted into the tree.
        mk_children0(Substmts, undefined, Typedefs, Groupings,
                     M, Ctx0, _Config = undefined, []),
    Ctx2 = v_unique_names(Children, Ctx1),
    case parse_schema_nodeid(Arg, Pos, M#module.prefix_map, Ctx2) of
        {ok, SchemaNodeId} ->
            Aug = #augment{target_node = SchemaNodeId,
                           stmt = Stmt,
                           children = Children},
            mk_augments(T, Typedefs, Groupings, M, Ctx2, [Aug | Acc]);
        {error, Ctx3} ->
            mk_augments(T, Typedefs, Groupings, M, Ctx3, Acc)
    end;
mk_augments([_ | T], Typedefs, Groupings, M, Ctx, Acc) ->
    mk_augments(T, Typedefs, Groupings, M, Ctx, Acc);
mk_augments([], _Typedefs, _Groupings, _M, Ctx, Acc) ->
    {Acc, Ctx}.

%% sort all augments into local and remote augments
-spec sort_augments(#augment{}) ->
        {LocalAugments :: [#augment{}], [{RemoteName :: atom(), [#augment{}]}]}.
sort_augments(Augments) ->
    L = group_per_module(
          lists:keysort(1, [add_module(Aug) || Aug <- Augments])),
    case lists:keytake([], 1, L) of
        {value, {[], LocalAugments}, RemoteAugments} ->
            {LocalAugments, RemoteAugments};
        false ->
            {[], L}
    end.

add_module(#augment{target_node = [H | _]} = Aug) ->
    case H of
        {Module, _} ->
            {Module, Aug};
        _ ->
            {[], Aug}
    end.

group_per_module([]) ->
    [];
group_per_module([{Mod, Aug} | T]) ->
    group_per_module(T, Mod, [Aug]).

group_per_module([{Mod, Aug} | T], Mod, Acc) ->
    group_per_module(T, Mod, [Aug | Acc]);
group_per_module([{Mod2, Aug} | T], Mod, Acc) ->
    [{Mod, Acc} | group_per_module(T, Mod2, [Aug])];
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
                      _:_ ->
                          {value, OtherPos} = map_lookup(Name, D00),
                          [MinPos, MaxPos] = lists:sort([Pos, OtherPos]),
                          {add_dup_error(Ctx00, Kind, Name, MaxPos, MinPos),
                           D00}
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


iterate_stmt(F, Keyword, Acc, L) ->
    try
        iterate_stmt0(L, Keyword, Acc, F)
    catch
        throw:Acc1 ->
            Acc1
    end.

iterate_stmt0([{Keyword, _, _, Substmts} = H | T], Keyword, Acc0, F) ->
    case F(H, Acc0) of
        {recurse, Acc1} ->
            Acc2 = iterate_stmt0(Substmts, Keyword, Acc1, F),
            iterate_stmt0(T, Keyword, Acc2, F);
        {continue, Acc1} ->
            iterate_stmt0(T, Keyword, Acc1, F);
        {stop, Acc1} ->
            throw(Acc1)
    end;
iterate_stmt0([], _, Acc, _F) ->
    Acc.

-spec resolve_idref(raw_identifier_ref(), pos(), prefix_map(), #yctx{}) ->
    {self
     | undefined
     | {ModuleName :: atom(), Revision :: string() | undefined},
     Name :: atom(),
     #yctx{}}.
resolve_idref({Prefix, Name}, Pos, PrefixMap, Ctx) ->
    case map_lookup(Prefix, PrefixMap) of
        {value, ModRevOrSelf} ->
            {ModRevOrSelf, Name, Ctx};
        none ->
            {undefined, Name, add_error(Ctx, Pos, 'YANG_ERR_PREFIX_NOT_FOUND',
                                        [Prefix])}
    end;
resolve_idref(Name, _Pos, _PrefixMap, Ctx) ->
    {self, Name, Ctx}.

get_module(self, M, _Ctx) ->
    {value, M};
get_module(ModRev, _M, Ctx) ->
    map_lookup(ModRev, Ctx#yctx.modrevs).

mk_prefix_map(Stmts) ->
    mk_prefix_map(Stmts, map_new()).

%% Pre: All statements are grammatically correct, so we don't have to check
%%      e.g. that 'prefix' is present in 'import'.
mk_prefix_map([{Kwd, Arg, _Pos, Substmts}|Stmts], Map) ->
    if Kwd == 'prefix' ->
            mk_prefix_map(Stmts, map_insert(Arg, self, Map));
       Kwd == 'belongs-to' ->
            {_, Prefix, _, _} = search_one_stmt('prefix', Substmts),
            mk_prefix_map(Stmts, map_insert(Prefix, self, Map));
       Kwd == 'import' ->
            {_, Prefix, _, _} = search_one_stmt('prefix', Substmts),
            case search_one_stmt('revision', Substmts) of
                {_, Revision, _, _} ->
                    ok;
                false ->
                    Revision = undefined
            end,
            mk_prefix_map(Stmts, map_insert(Prefix, {Arg, Revision}, Map));
       true ->
            case kwd_class(Kwd) of
                header ->
                    mk_prefix_map(Stmts, Map);
                linkage ->
                    mk_prefix_map(Stmts, Map);
                extension ->
                    mk_prefix_map(Stmts, Map);
                _ ->
                    Map
            end
    end;
mk_prefix_map([], Map) ->
    Map.

add_error(Ctx, Pos, ErrCode, Args) ->
    yang_error:add_error(Ctx, Pos, ErrCode, Args).

add_error(Level, Ctx, Pos, ErrCode, Args) ->
    yang_error:add_error(Level, Ctx, Pos, ErrCode, Args).

add_llerrors([{Code, FName, LineNo, Offset, Str} | T], Ctx) ->
    Pos = case Offset of
              -1 -> {FName, LineNo};
              _ -> {FName, LineNo, Offset}
          end,
    Ctx1 = add_error(error, Ctx, Pos, yang_llerror:code2err(Code), {str, Str}),
    add_llerrors(T, Ctx1);
add_llerrors([], Ctx) ->
    Ctx.

-spec map_new() -> map(term(), term()).
map_new() ->
    gb_trees:empty().

-spec map_is_empty(map()) -> boolean().
map_is_empty(Map) ->
    gb_trees:is_empty(Map).

-spec map_insert(term(), term(), map()) -> map().
%% crashes if Key is already present
map_insert(Key, Val, Map) ->
    gb_trees:insert(Key, Val, Map).

-spec map_update(term(), term(), map()) -> map().
%% updates Key if it is already present
map_update(Key, Val, Map) ->
    gb_trees:enter(Key, Val, Map).

-spec map_lookup(term(), map()) -> {value, term()} | none.
map_lookup(Key, Map) ->
    gb_trees:lookup(Key, Map).

-spec map_get(term(), map()) -> term().
%% crashes if Key is not present
map_get(Key, Map) ->
    gb_trees:get(Key, Map).

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

get_latest_revision(Stmts) ->
    get_latest_revision(Stmts, undefined).
get_latest_revision([{Kwd, Arg, _Pos, _Substmts} | Stmts], Latest) ->
    if Kwd == 'revision' ->
            if (Arg > Latest) orelse (Latest == undefined) ->
                    get_latest_revision(Stmts, Arg);
               true ->
                    get_latest_revision(Stmts, Latest)
            end;
       true ->
            case kwd_class(Kwd) of
                header ->
                    get_latest_revision(Stmts, Latest);
                linkage ->
                    get_latest_revision(Stmts, Latest);
                meta ->
                    get_latest_revision(Stmts, Latest);
                extension ->
                    get_latest_revision(Stmts, Latest);
                body ->
                    Latest
            end
    end;
get_latest_revision([], Latest) ->
    Latest.

get_namespace([{Kwd, Arg, _Pos, _Substmts} | Stmts]) ->
    if Kwd == 'namespace' ->
            Arg;
       true ->
            case kwd_class(Kwd) of
                header ->
                    get_namespace(Stmts);
                extension ->
                    get_namespace(Stmts);
                _ ->
                    undefined
            end
    end;
get_namespace([]) ->
    undefined.


-spec kwd_class(keyword()) -> header | linkage | meta | extension | body.
kwd_class('yang-version')  -> header;
kwd_class('namespace')     -> header;
kwd_class('prefix')        -> header;
kwd_class('belongs-to')    -> header;
kwd_class('organization')  -> meta;
kwd_class('contact')       -> meta;
kwd_class('description')   -> meta;
kwd_class('reference')     -> meta;
kwd_class('import')        -> linkage;
kwd_class('include')       -> linkage;
kwd_class('revision')      -> revision;
kwd_class({_Module, _Kwd}) -> extension;
kwd_class(_)               -> body.


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

search_stmts(Kwd, Stmts) ->
    [S || S <- Stmts, element(1, S) == Kwd].

get_extension_handler(_Kwd, _Ctx) ->
    false.

run_hooks(HookNumber, Ctx, Arg0) ->
    lists:foldl(fun(HookF, Arg1) ->
                        HookF(Ctx, Arg1)
                end, Arg0, element(HookNumber, Ctx#yctx.hooks)).

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

%parse_absolute_schema_nodeid(Arg, Pos, PrefixMap, Ctx) ->
%    parse_schema_nodeid(Arg, Pos, PrefixMap, Ctx).

parse_descendant_schema_nodeid(Arg, Pos, PrefixMap, Ctx) ->
    parse_schema_nodeid(Arg, Pos, PrefixMap, Ctx).

parse_schema_nodeid(Arg, Pos, PrefixMap, Ctx) ->
    SchemaNodeId = parse_schema_nodeid_str(Arg),
    try
        NormalizedSchemaNodeId =
            lists:map(
              fun({Prefix, Name}) ->
                      case map_lookup(Prefix, PrefixMap) of
                          {value, 'self'} ->
                              Name;
                          {value, {ModuleName, _Rev}} ->
                              {ModuleName, Name};
                          none ->
                              throw({error,
                                     add_error(Ctx, Pos,
                                               'YANG_ERR_PREFIX_NOT_FOUND',
                                               [Prefix])})
                      end;
                 (Name) ->
                      Name
              end, SchemaNodeId),
        {ok, NormalizedSchemaNodeId}
    catch
        throw:{error, Ctx1} ->
            {error, Ctx1}
    end.

%% Pre: arg is syntactically correct
parse_schema_nodeid_str(Arg) ->
    NodeIds = string:tokens(Arg, "/"),
    [parse_nodeid(NodeId) || NodeId <- NodeIds].

parse_nodeid(Str) ->
    case string:tokens(Str, ":") of
        [LocalName] ->
            ?l2a(LocalName);
        [Prefix, Name] ->
            {?l2a(Prefix), ?l2a(Name)}
    end.


pp_module(M) ->
    ?iof("module: ~p\n", [M#module.name]),
    ?iof("children: \n"),
    pp_children(M#module.children, "  ").

pp_children([H | T], Indent) ->
    ?iof("~s~p ~p\n", [Indent, H#sn.kind, H#sn.name]),
    pp_children(H#sn.children, "  " ++ Indent),
    pp_children(T, Indent);
pp_children([], _Indent) ->
    ok.
