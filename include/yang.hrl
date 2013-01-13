-define(l2a, list_to_atom).
-define(a2l, atom_to_list).
-define(i2l, integer_to_list).
-define(l2i, list_to_integer).
-define(l2b, list_to_binary).
-define(b2l, binary_to_list).
-define(iof(X), io:format(X, [])).
-define(iof(X, Y), io:format(X, Y)).
-define(stack(), try throw(1) catch _:_ -> erlang:get_stacktrace() end).

-record(type, {
          base :: yang:builtin_type_name() | yang:typedef_rec(),
          restrictions :: [term()], %% see exs.hrl; one of #pattern{}, ...
          type_spec :: term(),
          type_spec_fun :: yang_types:type_spec_fun()
         }).

-record(typedef, {
          name :: atom(),
          type :: #type{},
          default :: 'undefined' | term(),
          v_status :: yang:validate_status(),
          stmt :: yang:stmt(),
          pmap = yang:map_new() :: yang:map() % used by plugins
         }).

-record(identity, {
          name :: atom(),
          base :: yang:identifier_ref(),
          v_status :: yang:validate_status(),
          stmt :: yang:stmt(),
          pmap = yang:map_new() :: yang:map() % used by plugins
         }).

-record(feature, {
          name :: atom(),
          v_status :: yang:validate_status(),
          stmt :: yang:stmt(),
          pmap = yang:map_new() :: yang:map() % used by plugins
         }).

-record(extension, {
          name :: atom(),
          arg = [] :: [] | atom(),
          stmt :: yang:stmt(),
          pmap = yang:map_new() :: yang:map() % used by plugins
         }).

%% Represents all typedefs in scope in a statement.
-record(typedefs, {
          %% the set of typedefs defined at this level
          map = yang:map_new() :: yang:map(Name :: atom(), yang:typedef_rec()),
          %% parent is a pointer to a set of typedefs in scope in
          %% the parent statement
          parent :: 'undefined' | #typedefs{}
         }).

%% Represents all groupings in scope in a statement.
-record(groupings, {
          %% the set of groupings defined at this level
          map = yang:map_new() :: yang:map(Name :: atom(), yang:grouping_rec()),
          %% parent is a pointer to a set of groupings in scope in
          %% the parent statement
          parent :: 'undefined' | #groupings{}
         }).

%% schema node
-record(sn, {
          name :: atom()                          % local nodes
                | {ModuleName :: atom(), atom()}, % augmented nodes
          kind :: 'leaf' | 'leaf-list' | 'container' | 'list'
                | 'choice' | 'case'
                | 'rpc' | 'input' | 'output'
                | 'notification'
                | '__tmp_augment__',
          %% pointer to our module's prefix_map
          prefix_map :: yang:prefix_map(),
          config :: 'undefined' | true | false,
    %% container, list, rpc, input, output, notification:
          typedefs :: 'undefined' | #typedefs{},
          groupings :: 'undefined' | #groupings{},
    %% leaf, leaf-list:
          type :: 'undefined'
                 | yang:builtin_type_name()
                 | yang:typedef_rec() % pointer to typedef record
                 | #type{}, % inline type
          default :: 'undefined' | term(),
    %% list:
          keys :: 'undefined' | [#sn{}],
    %% container, list, choice, case, input, output, notification:
          children = [] :: [#sn{}],

          stmt :: yang:stmt(), % pointer to raw statement
          pmap = yang:map_new() :: yang:map() % used by plugins
         }).

-record(augment, {
          %% top-level augments always have absolute schema nodeids,
          %% and augments in uses always have relative schema nodeids.
          %% NOTE: target_node is an unresolved reference.  The actual
          %% #sn{} needs to be looked up.
          target_node :: yang:schema_nodeid(),
          children = [] :: [#sn{}],
          stmt :: yang:stmt(),
          pmap = yang:map_new() :: yang:map() % used by plugins
         }).

-record(grouping, {
          name :: atom(),
          v_status :: yang:validate_status(),
          typedefs :: 'undefined' | #typedefs{},
          groupings :: 'undefined' | #groupings{},
          children = [] :: [#sn{}],
          stmt :: yang:stmt(),
          pmap = yang:map_new() :: yang:map() % used by plugins
         }).

%% contains the complete module, i.e., included submodules are
%% expanded into this record.
%% this tree is also expanded with all augmented nodes
%% if we're compiling a submodule standalone, we will add
%% a dummy #module{} for the module.
-record(module, {
          %% name of the module or submodule
          name :: atom(),
          %% for modules, same as name.  for submodules, the name
          %% of the module it belongs to.
          modulename :: atom(),
          filename :: string(),
          kind :: 'module' | 'submodule',
          revision :: string(),
          namespace :: atom(),
          prefix :: atom(),
          submodules = [] :: [{#module{}, yang:pos()}],
          typedefs = #typedefs{} :: #typedefs{},
          groupings = #groupings{} :: #groupings{},
          features = yang:map_new() :: yang:map(Name :: atom(), #feature{}),
          identities = yang:map_new() :: yang:map(Name :: atom(), #identity{}),
          extensions = yang:map_new() :: yang:map(Name :: atom(), #extension{}),
          prefix_map :: yang:prefix_map(),
          stmt :: yang:stmt(), %% pointer to raw statement
          children = [] :: [#sn{}],
          augments = [] :: [#augment{}],
          pmap = yang:map_new() :: yang:map() % used by plugins
         }).

-record(yerror, {
          level = error :: 'error' | 'warning',
          pos :: yang:pos(),
          code :: atom(),
          args :: list()
         }).

%% Plugins can register hooks (TBD how).  A hook is called as:
%%   HookFun(Ctx :: opaque(), Arg :: term()) -> Arg' :: term()
-record(hooks, {
          %% called with argument [stmt()]
          pre_parse_module = []
         }).

-record(yctx, {
          search_path = [] :: [DirName :: string()],
          %% contains all modules and submodules found
          modrevs :: yang:map({Name :: atom(), Revision :: atom()}, #module{}),
          revs :: yang:map(Name :: atom(), [Revision :: atom()]),
          files :: yang:map({Name :: atom(), Revision :: atom()},
                            FileName :: string()),
          typemap :: yang:map(), % FIXME map/2
          error_codes = [] :: [{ErrCode :: atom(),
                                Level :: 'error' | 'warning',
                                Fmt :: string()}],
          %% FIXME: make ets tables of errors and unused_augments?
          %% then we don't have to return an updated Ctx all over the code...
          errors = [] :: [#yerror{}],
          %% List of loaded and initialized plugin modules.
          plugins = [] :: [Module :: atom()],
          %% List of option spec used by getopt.  Used by the frontend script.
          option_specs = [] :: [getopt:option_spec()],
          %% List of parsed options.  If the frontend script is not used,
          %% the caller can fill in this list directly.
          options = [] :: [getopt:option()],
          %% The available output formats.
          fmts = [] :: [{Fmt :: string(), Module :: atom()}],
          canonical = false :: boolean(),
          warnings = {false, false, [], []} :: {WarningsAsErrors :: boolean(),
                                                NoPrintWarnings :: boolean(),
                                                TreatAsWarning :: [atom()],
                                                TreatAsError :: [atom()]},
          hooks = #hooks{} :: #hooks{},
          pmap = yang:map_new() :: yang:map() % used by plugins
          %% FIXME: see class ctx in __init__.py
         }).

-define(is_data_definition_stmt(X),
        ((X) == 'leaf'
         orelse (X) == 'leaf-list'
         orelse (X) == 'container'
         orelse (X) == 'list'
         orelse (X) == 'choice'
         orelse (X) == 'case'
         orelse (X) == 'rpc'
         orelse (X) == 'input'
         orelse (X) == 'output'
         orelse (X) == 'notification')).

-define(is_stmt(S), tuple_size(S) == 4).
-define(STMT_KEYWORD, 1).
-define(STMT_ARG, 2).
-define(STMT_POS, 3).
-define(STMT_SUBSTMTS, 4).
