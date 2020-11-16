-ifndef(l2a).
-define(l2a, list_to_atom).
-define(a2l, atom_to_list).
-define(i2l, integer_to_list).
-define(l2i, list_to_integer).
-define(l2b, list_to_binary).
-define(b2l, binary_to_list).
-define(b2a(X), binary_to_atom(X, latin1)).
-define(a2b(X), atom_to_binary(X, latin1)).
-define(iof(X), io:format(X, [])).
-define(iof(X, Y), io:format(X, Y)).
-define(liof(Fmt, Args), io:format("~w:~w " ++ Fmt,[?MODULE,?LINE|Args])).
-define(iof_bt(Fmt, Args), io:format("~s ~p\n",
                                     [io_lib:format(Fmt, Args),
                                      ?stacktrace()])).
-define(liof_bt(Fmt, Args), io:format("~w:~w ~s ~p\n",
                                      [?MODULE, ?LINE,
                                       io_lib:format(Fmt, Args),
                                       ?stacktrace()])).
-endif.

%% verbosity level
-define(V_SILENT, 0).
-define(V_NORMAL, 1).

-record(type, {
          %% 'builtin' means "really" builtin #type{} in #yctx.typemap
          %% - never used for #type{} in type_spec(), #typedef{}, or #sn{}
          base :: 'builtin' | yang:builtin_type_name() | yang:typedef_rec() |
                  {ModuleName :: atom(), TypeName :: atom()}, % plugin-defined
          type_spec :: yang_types:type_spec(),
          type_spec_fun :: 'undefined' | yang_types:type_spec_fun(),
          stmt :: 'undefined' | yang:stmt(),
          pmap = yang:map_new() :: yang:map0() % used by plugins
         }).

-record(typedef, {
          name :: atom(),
          %% moduleref is the name and revision where this item is defined
          moduleref :: yang:modrev(),
          status :: yang:yang_status(),
          type :: 'undefined' | #type{},
          %% the default may be inherited from the base type
          %% 'undefined': no default given; 'invalid': invalid default given
          default :: 'undefined' | 'invalid' | {yang:stmt(), term()},
          v_status :: yang:validate_status(),
          is_in_grouping = false :: boolean(),
          is_top_level = false :: boolean(),
          id = make_ref() :: reference(), % unique identifier
          stmt :: yang:stmt(),
          pmap = yang:map_new() :: yang:map0() % used by plugins
         }).

-record(feature, {
          name :: atom(),
          %% moduleref is the name and revision where this item is defined
          moduleref :: yang:modrev(),
          status :: 'undefined' | yang:yang_status(),
          v_status :: yang:validate_status(),
          stmt :: yang:stmt(),
          pmap = yang:map_new() :: yang:map0() % used by plugins
         }).

-record(identity, {
          name :: atom(),
          %% moduleref is the name and revision where this item is defined
          moduleref :: yang:modrev(),
          bases = [] :: [#identity{}],
          if_feature = [] :: [{#feature{},
                               Origin :: 'local',
                               yang:stmt()}],
          %% Nodes for which 'if-feature' evaluation resulted in 'false' are
          %% retained in the map.
          if_feature_result = true :: boolean(),
          status :: 'undefined' | yang:yang_status(),
          v_status :: yang:validate_status(),
          stmt :: yang:stmt(),
          pmap = yang:map_new() :: yang:map0() % used by plugins
         }).

-record(extension, {
          name :: atom(),
          %% moduleref is the name and revision where this item is defined
          moduleref :: yang:modrev(),
          arg = [] :: [] | atom(),
          stmt :: yang:stmt(),
          pmap = yang:map_new() :: yang:map0() % used by plugins
         }).

%% Represents all typedefs in scope in a statement.
-record(typedefs, {
          %% the set of typedefs defined at this level
          map = yang:map_new() :: yang:map(Name :: atom(), yang:typedef_rec()),
          same_as_parent = false :: boolean(),
          %% parent is a pointer to a set of typedefs in scope in
          %% the parent statement
          parent :: 'undefined' | #typedefs{}
         }).

%% Represents all groupings in scope in a statement.
-record(groupings, {
          %% the set of groupings defined at this level
          map = yang:map_new() :: yang:map(Name :: atom(), yang:grouping_rec()),
          same_as_parent = false :: boolean(),
          %% parent is a pointer to a set of groupings in scope in
          %% the parent statement
          parent :: 'undefined' | #groupings{}
         }).

%% schema node
-record(sn, {
          name :: yang:sn_name(),
          kind :: 'undefined' | yang:kind(),
          %% pointer to our module.  NOTE: the module record is not
          %% yet complete; specifically its .children is [].
          module :: 'undefined' | yang:module_rec(),
          %% 'ignore' means that the config statement should be ignored
          %% for this subtree.
          config :: 'undefined' | 'ignore' | 'true' | 'false',
          %% pointer to the typedefs in scope
          typedefs = #typedefs{} :: #typedefs{},
          %% pointer to the groupings in scope
          groupings = #groupings{} :: #groupings{},
          status :: 'undefined' | yang:yang_status(),
    %% leaf, leaf-list:
          type :: 'undefined' | #type{},
    %% leaf, leaf-list, choice:
          %% the default may be inherited from the leaf(-list)'s type.
          %% value is a list for leaf-list.
          %% term() is identifier atom() for choice.
          default :: 'undefined' |
                     'invalid' |
                     {yang:stmt(), term()} |
                     [{yang:stmt(), term()}],
    %% list:
          %% names of child nodes
          keys :: 'undefined' | [Name :: atom()],
    %% container, list, choice, case, input, output, notification:
          children = [] :: [#sn{}],
          ignored = [] :: [#sn{}],

          %% 'when' expressions from a 'uses' or 'augment' are propagated
          %% into the child schema nodes.
          'when' = [] :: [{CompiledXPath,
                           Deps :: false | [Dep],
                           Origin :: 'uses' | 'augment' | 'local',
                           yang:stmt()}],
          must = [] :: [{CompiledXPath,
                         Deps :: false | [Dep],
                         yang:stmt()}],
          %% 'if-feature' expressions from a 'uses' or 'augment' are propagated
          %% into the child schema nodes.
          if_feature = [] :: [{#feature{},
                               Origin :: 'uses' | 'augment' | 'local',
                               yang:stmt()}],
          %% Nodes for which 'if-feature' evaluation resulted in 'false' are
          %% retained in the schema tree.
          %% This makes it possible to ignore 'augment' and 'deviate' (and
          %% extensions with similar semantics) when the target node should
          %% be removed due to 'if-feature' evaluation, instead of requiring
          %% exactly matching 'if-feature' substatements for those statements.
          %% Plugins based on the schema tree will thus typically need to
          %% filter out nodes that have if_feature_result == false.
          if_feature_result = true :: boolean(),
          augmented_by = [] :: [yang:augment_rec()],
          is_augment_top_node = false :: boolean(),
          stmt :: 'undefined' | yang:stmt(), % pointer to raw statement
          pmap = yang:map_new() :: yang:map0() % used by plugins
         }).

-record(augment, {
          %% top-level augments always have absolute schema nodeids,
          %% and augments in uses always have relative schema nodeids.
          %% NOTE: target_node is an unresolved reference.  The actual
          %% #sn{} needs to be looked up.
          target_node :: yang:cursor_path(),
          children = [] :: [#sn{}],
          stmt :: yang:stmt(),
          status :: yang:yang_status(),
          has_when = false :: boolean(),
          pmap = yang:map_new() :: yang:map0() % used by plugins
         }).

-record(grouping, {
          name :: atom(),
          %% moduleref is the name and revision where this item is defined
          moduleref :: yang:modrev(),
          status :: yang:yang_status(),
          v_status :: yang:validate_status(),
          typedefs :: 'undefined' | #typedefs{},
          groupings :: 'undefined' | #groupings{},
          is_top_level :: boolean(),
          children = [] :: [#sn{}],
          stmt :: yang:stmt(),
          pmap = yang:map_new() :: yang:map0() % used by plugins
         }).

-record(deviation, {
          target_node :: yang:cursor_path(),
          deviates = [] :: [not_supported
                          | {add|replace|delete, yang:stmt()}
                          | {{replace_type, #type{}}, yang:stmt()}],
          stmt :: yang:stmt(),
          pmap = yang:map_new() :: yang:map0() % used by plugins
         }).

%% contains the complete module, i.e., included submodules are
%% expanded into this record.
%% this tree is also expanded with all augmented nodes
%% if we're compiling a submodule standalone, we will add
%% a dummy #module{} for the module.
-record(module, {
          %% name of the module or submodule
          name :: atom(),
          yang_version = '1' :: yang:yang_version(),
          %% for modules, same as name.  for submodules, the name
          %% of the module it belongs to.
          modulename :: atom(),
          namespace :: atom(),
          prefix :: atom(),
          %% for modules, same as revision.  for submodules, the revision
          %% of the module it belongs to.
          modulerevision :: yang:revision(),
          filename :: string(),
          kind :: 'module' | 'submodule',
          revision :: yang:revision(),
          prefix_map :: yang:prefix_map(),
          %% The ns map is used to map XPath prefixes to namespaces.  However,
          %% instead of using the 'namespace' value as namespace, we use the
          %% 'modulename'.
          xpath_ns_map :: yang_xpath:ns_map(),
          submodules = [] :: [{#module{}, yang:pos()}],
          %% from the module and all submodules
          imports = [] :: [yang:import()],
          %% from the module and all submodules
          typedefs = #typedefs{} :: #typedefs{},
          %% from the module and all submodules
          groupings = #groupings{} :: #groupings{},
          %% from the module and all submodules
          features = yang:map_new() :: yang:map(Name :: atom(), #feature{}),
          %% from the module and all submodules
          identities = yang:map_new() :: yang:map(Name :: atom(), #identity{}),
          %% from the module and all submodules
          extensions = yang:map_new() :: yang:map(Name :: atom(), #extension{}),
          %% from the module and all submodules
          children = [] :: [#sn{}],
          ignored = [] :: [#sn{}],
          conformance = 'implement' :: yang:conformance(),
          %% from the module and all submodules
          remote_augments = [] :: [{ModuleName :: atom(), [#augment{}]}],
          local_augments = [] :: [#augment{}],
          local_deviations = [] :: [#deviation{}],
          remote_deviations = [] :: [{ModuleName :: atom(), [#deviation{}]}],
          deviated_by = [] :: [yang:modrev()],
          stmt :: yang:stmt(), %% pointer to raw statement
          add_cause :: 'primary' | 'import' | 'include' | 'deviation' |
                       {ModuleName :: atom(), AddCase :: atom()}, % for plugins
          pmap = yang:map_new() :: yang:map0() % used by plugins
         }).

%% Parsed default value: integer()
-record(integer_type_spec, {
          %% this type's min value; for the 'min' keyword
          min :: integer(),
          %% this type's max value; for the 'max' keyword
          max :: integer(),
          range :: [{Min :: integer(), Max :: integer()} | (Val :: integer())],
          range_stmt :: 'undefined' | yang:stmt()
         }).

%% Parsed default value: binary()
-record(string_type_spec, {
          parent :: 'undefined' | #type{},
          %% this type's min length; for the 'min' keyword
          min :: integer(),
          %% this type's max length; for the 'max' keyword
          max :: yang_types:max_length_type(),
          length :: 'undefined' |
                    [{Min :: integer(), Max :: yang_types:max_length_type()} |
                     (Val :: yang_types:max_length_type())],
          patterns = [] :: [{CompiledRegexp :: term(), Regexp :: binary(),
                             InvertMatch :: boolean()}],
          length_stmt :: 'undefined' | yang:stmt(),
          pattern_stmts = [] :: [yang:stmt()]
         }).

%% Parsed default value: base64-decoded binary()
-record(binary_type_spec, {
          %% this type's min length; for the 'min' keyword
          min :: integer(),
          %% this type's max length; for the 'max' keyword
          max :: yang_types:max_length_type(),
          length :: 'undefined' |
                    [{Min :: integer(), Max :: yang_types:max_length_type()} |
                     (Val :: yang_types:max_length_type())],
          length_stmt :: 'undefined' | yang:stmt()
         }).

%% Parsed default value: 'true' | 'false'
-record(boolean_type_spec, {}).

%% Parsed default value: integer() (shifted by fraction_digits)
-record(decimal64_type_spec, {
          fraction_digits :: 'undefined' | integer(),
          %% this type's min value shifted by fraction_digits
          min :: integer(),
          %% this type's max value shifted by fraction_digits
          max :: integer(),
          %% range values shifted by fraction_digits
          range :: [{Min :: integer(), Max :: integer()} | (Val :: integer())],
          range_stmt :: 'undefined' | yang:stmt()
         }).

%% Parsed default value: integer() (from 'value')
-record(enumeration_type_spec, {
          enums = [] :: [{Name :: atom(), Value :: integer()}],
          enum_stmts = [] :: [yang:stmt()],
          %% These includes those dropped due to if-feature
          all_enums = [] :: [{Name :: atom(), Value :: integer()}],
          all_enum_stmts = [] :: [yang:stmt()]
         }).

%% Parsed default value: [integer()] (list of positions for set bits)
-record(bits_type_spec, {
          bits = [] :: [{Name :: atom(), Position :: integer()}],
          bit_stmts = [] :: [yang:stmt()],
          %% These includes those dropped due to if-feature
          all_bits = [] :: [{Name :: atom(), Position :: integer()}],
          all_bit_stmts = [] :: [yang:stmt()]
         }).

%% Parsed default value: {#type{}, term()} (term() is parsed value for #type{})
-record(union_type_spec, {
          types = [] :: [#type{}]
         }).

-record(instance_identifier_type_spec, {
          require_instance :: boolean()
         }).

%% Parsed default value: #identity{}
-record(identityref_type_spec, {
          bases = [] :: [#identity{}]
         }).

-record(empty_type_spec, {}).

%% No parsed default value (i.e. always 'undefined')
-record(leafref_type_spec, {
          path :: 'undefined' | yang_types:extended_leafref_path(),
          path_stmt :: 'undefined' | yang:stmt(),
          parsed_xpath,
          %% Available in YANG 1.1
          require_instance = true :: boolean()
         }).

-record(yerror, {
          level = error :: 'error' | 'warning' | 'none',
          pos :: yang:pos(),
          code :: atom(),
          args :: list()
         }).

%% Plugins can register hooks through yanger_plugin:register_hook().
%%   HookFun(Ctx :: #yctx{}, Arg :: term(), ...) ->
%%       {Ctx1 :: #yctx{}, Arg' :: term()}
%%   Arg' is a modified Arg (same type).
-record(hooks, {
          %% HookF(#yctx{}) -> #yctx{}.
          %%
          %% post_init_ctx is called after the compiler context has been
          %% initialized and command line argumend have been parsed
          %% into options.
          post_init_ctx = [] :: [yang:hookfun()],

          %% HookF(#yctx{}, yang:stmt()) -> {#yctx{}, yang:stmt()}.
          %%
          %% post_parse_stmt_tree is called with the raw statement
          %% tree, after parsing of the module is completed.
          post_parse_stmt_tree = [] :: [yang:hookfun()],

          %% HookF(#yctx{}, #module{}) -> {#yctx{}, #module{}}.
          %%
          %% post_parse_module is called for each module after parsing has
          %% been completed.
          post_parse_module = [] :: [yang:hookfun()],

          %% HookF(#yctx{}, #sn{}, Mode, UsesPos, Ancestors) ->
          %%   {#yctx{}, #sn{}}.
          %%
          %% Mode :: final | grouping
          %% UsesPos :: 'undefined' | yang:pos()
          %% Ancestors :: [#sn{} | #module{}]}
          %%
          %% *_mk_sn is called for each schema node when it is created,
          %% and when it is expanded from a "uses" statement, but it is
          %% not called for nodes that have if_feature_result == false.
          %%
          %% pre_mk_sn is called before the node's children have been
          %% expanded.  All fields in #sn except children have been
          %% set when this function is called.  This means that for
          %% leafs and leaf-lists, pre_mk_sn is called right before
          %% post_mk_sn.  When pre_mk_sn is called for a node, it has
          %% been called for the node's Ancestors.
          %%
          %% post_mk_sn is called after the node's children have been
          %% expanded. When post_mk_sn is called for a node, it has
          %% not yet been called for the node's Ancestors.
          %%
          %% Ancestors is the reversed list of parents to this node.
          %%
          %% Mode 'final' means that all ancestors are known, and all
          %% ancestors have been called with Mode 'final'.
          %%
          %% Mode 'grouping' means that the node is defined in a grouping.
          %%
          %% If UsesPos is 'undefined', the node is initially defined.  If
          %% UsesPos is a position, the grouping where the node was defined
          %% is used.  Note that if a grouping is used inside another
          %% grouping and then used in the data tree, this function is called
          %% three times.
          %%
          %% The intention is that plugins can do validation that
          %% requires the expanded tree, and access to parents.  The
          %% #sn.pmap field can be updated by the plugins.
          %%
          %% *_mk_sn can be registered with
          %% yanger_plugin:register_conditional_hook().
          pre_mk_sn = [] :: [yang:hookfun()
                           | {[Condition::term()] | yang:hookfun()}],
          post_mk_sn = [] :: [yang:hookfun()
                            | {[Condition::term()] | yang:hookfun()}],

          %% HookF(#yctx{}, #type{}, #module{}) -> {#yctx{}, #type{}}.
          %%
          %% mk_type is called for each #type{} in the expanded
          %% module, when it is created.
          %%
          %% The intention is that plugins can do validation that
          %% requires the created #type{}, and access to substatemets.
          %% The .pmap can be updated by the plugins.
          mk_type = [] :: [yang:hookfun()],

          %% HookF(#yctx{}, #typedef{}, #module{}) -> {#yctx{}, #typedef{}}.
          %%
          %% mk_typedef is called for each #typedef{} in the expanded
          %% module, when it is created.
          %%
          %% The intention is that plugins can do validation that
          %% requires the created #typedef{}, and access to substatemets.
          %% The .pmap can be updated by the plugins.
          mk_typedef = [] :: [yang:hookfun()],

          %% HookF(#yctx{}, #grouping{}, #module{}) -> {#yctx{}, #grouping{}}.
          %%
          %% mk_grouping is called for each #grouping{} in the expanded
          %% module, when it is created.
          %%
          %% The intention is that plugins can do validation that
          %% requires the created #grouping{}, and access to substatemets.
          %% The .pmap can be updated by the plugins.
          mk_grouping = [] :: [yang:hookfun()],

          %% HookF(#yctx{}, #module{}) -> #yctx{}.
          %%
          %% post_expand_module is called after expansion of
          %% the entire module, before post_expand_sn. Augments and
          %% deviations from other modules have been applied when
          %% the function is called.
          post_expand_module = [] :: [yang:hookfun()
                                      | {[Condition::term()] | yang:hookfun()}],

          %% HookF(#yctx{}, #sn{}, #module{}, Ancestors :: [#sn{}]) ->
          %%   #yctx{}.
          %%
          %% post_expand_sn is called for each #sn after expansion of
          %% the entire module, but it is not called for nodes that
          %% have if_feature_result == false.
          %% Augments and deviations from other modules have been applied
          %% when the function is called, but it is not called for #sn{}'s
          %% resulting from augments from other modules - it *is* called
          %% for those after expansion of the *augmenting* module.
          %% This function is called for all children in a submodule when
          %% the submodule is defined, but also when the submodule is
          %% included in some other submodule or the module.
          %% Compare (Sn#sn.module)#module.name with M#module.name to
          %% avoid doing work multiple times.
          %% Note that this function cannot modify the #sn{}, since there
          %% are pointers to #sn{} stored all over the tree.  If an #sn{}
          %% is modified, these pointers would refer to the old #sn{}.
          post_expand_sn = [] :: [yang:hookfun()
                                  | {[Condition::term()] | yang:hookfun()}],

          %% HookF(#yctx{}) -> #yctx{}.
          %%
          %% post_add_modules is called once after all modules have been
          %% added.
          post_add_modules = [] :: [yang:hookfun()]

         }).

-record(env, {
          extension_modules = [] :: [atom()],
          data_definition_stmts :: yang:map(yang:keyword(), []),
          copy_from_grouping_stmts :: yang:map(yang:keyword(), []),
          refinements :: yang:map(yang:keyword(), []),
          deviates :: yang:map(yang:keyword(), [])
         }).

-record(yctx, {
          search_path = [] :: [DirName :: string()],
          verbosity = ?V_SILENT :: yang:verbosity(),
          xpath_functions = [] :: [{Name :: atom(), Arity :: integer()}],
          primary_module_names = [] :: [atom()],
          %% contains all modules and submodules found
          modrevs :: yang:map(yang:modrev(), #module{}),
          %% The revision list is kept sorted with latest revision first
          revs :: yang:map(Name :: atom(), [yang:revision()]),
          files :: yang:map({Name :: atom(),
                             yang:revision() | latest | undefined},
                            {FileName :: string(),
                             yang:revision() | undefined}),
          features :: 'undefined'
                    | 'none' % 'none' means compile for no features at all
                    | yang:map(ModName :: atom(), [FeatureName :: atom()]),
          conformances = [] :: [yang:conformance() |
                                {ModName :: atom(), yang:conformance()}],
          typemap :: yang:map(yang:builtin_type_name()
                              | {ModuleName :: atom(), TypeName :: atom()},
                              #type{}),
          error_codes = [] :: [{ErrCode :: atom(),
                                Level :: 'error' | 'warning',
                                Fmt :: string()}],
          errors = [] :: [#yerror{}],
          %% List of loaded and initialized plugin modules.
          plugins = [] :: [Module :: atom()],
          %% List of option spec used by getopt.  Used by the frontend script.
          option_specs = [] :: [getopt:option_spec()],
          %% List of parsed options.  If the frontend script is not used,
          %% the caller can fill in this list directly.
          options = [] :: [getopt:option()],
          %% The available output formats.
          fmts = [] :: [{Fmt :: atom(),
                         AllowErrors :: boolean(),
                         yanger_plugin:emit_fun()}],
          transforms = [] :: [{Name :: atom(), yanger_plugin:transform_fun()}],
          canonical = false :: boolean(),
          strict = false :: boolean(),
          apply_deviations = true :: boolean(),
          warnings = {false, false, [], [], []} ::
            {WarningsAsErrors :: boolean(),
             NoPrintWarnings :: boolean(),
             TreatAsNone :: [atom()],
             TreatAsWarning :: [atom()],
             TreatAsError :: [atom()]},
          env :: #env{},
          hooks = #hooks{} :: #hooks{},
          unused_imports = [] :: [{ModuleName :: atom(), [yang:import()]}],
          %% Imports that have only been used in deviations
          deviation_imports = [] :: [{ModuleName :: atom(), [yang:import()]}],
          %% Typedef leafrefs that failed path validation will be added here.
          %% These typedefs are only reported as errors if there are any nodes
          %% using them.
          bad_typedefs :: yang:map({Name :: atom(), yang:modrev()}, #yerror{}),
          max_status :: 'undefined' | yang:yang_status(),
          pmap = yang:map_new() :: yang:map0() % used by plugins
         }).

%% See yang:cursor_move()
-record(cursor, {
          %% The position for which errors are reported.
          pos :: yang:pos(),
          %% The module currently being processed.
          module :: #module{},
          init_modulename :: atom(),
          %% Specify if the cursor moves in the full schema tree, or only
          %% the data nodes.
          type = schema :: yang:cursor_type(),
          %% current position in the tree.  'undefined' means
          %% at any top-level.
          cur :: 'undefined' | {'top', ModuleName :: atom()} | #sn{},
          ancestors = [] :: [#sn{}],
          %% internal; keep track of last parent skipped
          last_skipped :: yang:cursor_skipped()
         }).

-define(is_stmt(S), tuple_size(S) == 4).
-define(STMT_KEYWORD, 1).
-define(STMT_ARG, 2).
-define(STMT_POS, 3).
-define(STMT_SUBSTMTS, 4).

-define(MUST_XPATH, 1).
-define(MUST_DEPS, 2).
-define(MUST_STMT, 3).

%% with OTP-21 erlang:get_stacktrace/0 is depricated
%% NOTE: heavily inspired by solution in hawk/lux
-ifndef(CATCH_STACKTRACE).
-ifdef(OTP_RELEASE).
    -define(stacktrace(),
            fun() -> try throw(1) catch _:_:StAcK -> StAcK end end()).
    -define(CATCH_STACKTRACE(Class, Reason, Stacktrace),
            Class:Reason:Stacktrace ->
           ).
    -define(CATCH_STACKTRACE_WHEN(Class, Reason, Stacktrace, When),
            Class:Reason:Stacktrace when (When) ->
           ).
-else.
    -define(stacktrace(),
            try throw(1) catch _:_ -> erlang:get_stacktrace() end).
    -define(CATCH_STACKTRACE(Class, Reason, Stacktrace),
            Class:Reason ->
                Stacktrace = erlang:get_stacktrace(),
           ).
    -define(CATCH_STACKTRACE_WHEN(Class, Reason, Stacktrace, When),
            Class:Reason when (When) ->
                Stacktrace = erlang:get_stacktrace(),
           ).
-endif.
-endif.
