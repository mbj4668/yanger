%%%----------------------------------------------------------------%%%
%%% @doc Yanger Sample XML Skeleton Output Plugin                  %%%
%%% @author Lisa Bevemyr, lbevemyr@cisco.com (summer intern 2021)  %%%
%%% Prints a sample xml skeleton from a YANG model according to    %%%
%%% the rules in RFC 7950. Inspiration for this plugin came from a %%%
%%% similar plugin for Pyang.                                      %%%
%%%----------------------------------------------------------------%%%

-module(yanger_sample_xml_skeleton).
-behaviour(yanger_plugin).

-export([init/1, help/0]).

-include_lib("yanger/include/yang.hrl").

-define(in, lists:member).

-spec help() -> binary().
help() ->
    <<"Each module is printed as a sample XML skeleton document according \n"
      "to RFC 7950.\n\n"
      "The following options can be used to modify the output:\n\n"
      "  --sample-xml-skeleton-doctype     Should be followed by data to\n"
      "                                    exclude config nodes and should be\n"
      "                                    followed by config to only include\n"
      "                                    config nodes. The default setting\n"
      "                                    is to only exclude config nodes.\n\n"
      "  --sample-xml-skeleton-defaults    Insert leafs with default values\n"
      "                                    from the module.\n\n"
      "  --sample-xml-skeleton-annotaions  Add annotations as XML comments.\n\n"
      "  --sample-xml-skeleton-path        Should be followed by the path to\n"
      "                                    a subtree to output.\n\n"
      "  --sample-xml-skeleton-populate    Should be followed by the name of\n"
      "                                    a file containing a map of types\n"
      "                                    and corresponding values to\n"
      "                                    populate the module with.\n"
>>.

init(Ctx0) ->
    Ctx1 = yanger_plugin:register_output_format(
             Ctx0, 'sample-xml-skeleton', _AllowErrors = true, fun emit/3),
    Ctx2 = yanger_plugin:register_hook(
             Ctx1, #hooks.post_init_ctx, fun post_init_ctx/1),
    Ctx3 = yanger_plugin:register_error_codes(
             Ctx2,
             [{'SAMPLE_XML_SKELETON_UNKNOWN_FILE', error,
               "No file with the correct format could be found."}]),
    yanger_plugin:register_option_specs(Ctx3, option_specs()).

option_specs() ->
    [{"sample-xml-skeleton output specific options:",
      [{sample_doctype, undefined,
        "sample-xml-skeleton-doctype", string,
        "Type of sample XML document (data or config)."},
       {sample_defaults, undefined,
        "sample-xml-skeleton-defaults", boolean,
        "Insert leafs with default values."},
       {sample_annotations, undefined,
        "sample-xml-skeleton-annotations", boolean,
        "Add annotations as XML comments."},
       {sample_path, undefined,
        "sample-xml-skeleton-path", string, "Subtree to print"},
       {sample_populate, undefined,
        "sample-xml-skeleton-populate", string,
        "Populate with default datatypes in given file."},
       {sample_help, undefined, "sample-xml-skeleton-help",
        boolean, "Print help in tree symbols and exit."}]
     }].

post_init_ctx(Ctx) ->
    case proplists:get_value(sample_help, Ctx#yctx.options, false) of
        true ->
            io:put_chars(help()),
            halt();
        false ->
            ok
    end,
    Ctx.

-spec emit(Ctx::#yctx{}, [Mods::#module{}], Fd::io:device()) -> [].
emit(Ctx, Mods, Fd) ->
    PathStr = proplists:get_value(sample_path, Ctx#yctx.options, ""),
    Def = proplists:get_value(sample_defaults, Ctx#yctx.options, false),
    Anno = proplists:get_value(sample_annotations, Ctx#yctx.options, false),
    Doc = proplists:get_value(sample_doctype, Ctx#yctx.options, "data"),
    Path = [?l2a(Name) || Name <- string:tokens(PathStr, "/")],
    Populate = proplists:get_value(sample_populate, Ctx#yctx.options, false),
    case Populate of
        false ->
            emit_sample_xml_skeleton(
              Ctx, Mods, Fd, Path, Def, Anno, Doc, Populate),
            [];
        _ ->
            case catch file:consult(Populate) of
                {ok, P} ->
                    Pop = maps:from_list(P),
                    emit_sample_xml_skeleton(
                      Ctx, Mods, Fd, Path, Def, Anno, Doc, Pop),
                    [];
                _ ->
                    CtxE = yanger_plugin:add_error(
                             Ctx, _ChildPos = {Populate, 0},
                             'SAMPLE_XML_SKELETON_UNKNOWN_FILE', []),
                    CtxE#yctx.errors
            end
    end.

-spec emit_sample_xml_skeleton(Ctx::#yctx{}, [Module::#module{}],
                               Fd::io:device(), [Path::atom()],
                               Def::boolean(), Anno::boolean(),
                               Doc::string(), Pop::boolean() | map()) -> ok.
%% @doc Main emit function. Works through a list of modules and calls
%%      other sub emit functions appropriately.
emit_sample_xml_skeleton(_Ctx, [], _Fd,
                         _Path, _Def, _Anno, _Doc, _Pop) -> ok;
emit_sample_xml_skeleton(Ctx, [Mod|Mods], Fd,
                         Path, Def, Anno, Doc, Pop) ->
    ExistingChildren = existing_children(Mod#module.children),
    Chs = [C || C <- ExistingChildren, is_data_def(C#sn.kind, Ctx)],
    Buf = print_header(Chs, Mod, Doc),
    LastBuf = print_children(Chs, Mod, Fd, _Prefix="", Path, Ctx,
                             "", Def, Anno, Doc, Pop, Buf),
    print_tail(Mod, Fd, Doc, Buf, LastBuf),
    case Mods of
        [] ->
            ok;
        _ ->
            io:format(Fd, "~n", []),
            emit_sample_xml_skeleton(Ctx, Mods, Fd, Path, Def, Anno, Doc, Pop)
    end.

existing_children(Chs) ->
    [C || #sn{if_feature_result = true} = C <- Chs].

print_header(_Chs, _Mod, Doc) ->
    ["<?xml version='1.0' encoding='UTF-8'?>\n",
     "<", Doc, " xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">\n"].

print_tail(_Mod, Fd, Doc, Buf, LastBuf) when Buf == LastBuf ->
    io:format(Fd, "<?xml version='1.0' encoding='UTF-8'?>\n", []),
    io:format(Fd, "<~s xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\"/>\n",
              [Doc]);
print_tail(_Mod, Fd, Doc, _Buf, LastBuf) ->
    print_buf(Fd, LastBuf),
    io:format(Fd, "</~s>\n", [Doc]).

%% @doc Print a list of children, delegates to print_node.
print_children(Children, Mod, Fd, Prefix, Path, Ctx, CurrentNS, Def, Anno, Doc,
               Pop, Buf) ->
    {Children1, Path1} =
        case Path of
            [H|T] ->
                {[Sn || Sn <- Children, Sn#sn.name == H], T};
            _ ->
                {Children, Path}
        end,

    F = fun(Child, BufIn) ->
                case
                    (?in(Child#sn.kind,[input,output]))
                    andalso (Child#sn.children == [])
                of
                    true ->
                        BufOut = BufIn;
                    false ->
                        ME = yang:search_all_stmts(
                               'min-elements', Child#sn.stmt),
                        MEExists = length(ME) == 1,
                        Kind = Child#sn.kind,
                        if MEExists ->
                                Copies = element(2, hd(ME)),
                                case Kind of
                                    'leaf-list' ->
                                        FirstCopy = true;
                                    _ ->
                                        FirstCopy = false
                                end,
                                BufOut = add_copies(
                                           Child, Mod, Fd, Prefix, Path1, Ctx,
                                           CurrentNS, Def, Anno, Doc, Pop,
                                           BufIn, FirstCopy, Copies);
                           true ->
                                BufOut = print_node(Child, Mod, Fd, Prefix,
                                                    Path1, Ctx, CurrentNS,
                                                    _IsComment=true, Def, Anno,
                                                    Doc, Pop, BufIn)
                        end
                end,
                BufOut
        end,
    lists:foldl(F, Buf, Children1).

print_node(Sn, Mod, Fd, OldPrefix, Path, Ctx, CurrentNS, Comment, Def, Anno,
           Doc, Pop, Buf) ->
    {KW, _StmtArg, _PathTyp, _Subs} = Sn#sn.stmt,
    Config = Sn#sn.config,
    Prefix = ["  ", OldPrefix],
    Name = name(Sn, Mod),
    Default = Sn#sn.default,
    case Default of
        {{_, DV, _, _}, _} ->
            DefaultValue = DV;
        _ ->
            DefaultValue = Default
    end,
    if Doc == "config" andalso not(Config) ->
            ReturnBuf = Buf;
       KW == 'choice' orelse
       KW == 'case' ->
            ReturnBuf = print_children(existing_children(Sn#sn.children), Mod,
                                       Fd, OldPrefix, Path, Ctx, CurrentNS, Def,
                                       Anno, Doc, Pop, Buf);
       DefaultValue =/= undefined ->
            if Def andalso
               ((Doc == "config" andalso Config)
                orelse (Doc == "data" andalso not(Config))) ->
                    DefaultLine = [Prefix, "<", Name, ">",
                                   ?b2l(DefaultValue), "</", Name, ">\n"],
                    ReturnBuf = [Buf, DefaultLine];
               true ->
                    ReturnBuf = Buf
            end;

       true ->
            NameLine = [Prefix, "<", Name],
            HasChildren = length(Sn#sn.children) > 0,
            {_, _, {Filename, _}, _} =  Sn#sn.stmt,
            {NewNS, NamespaceLine} =
                new_namespace(Mod, Ctx, Filename, CurrentNS, Filename),
            {AnnoAdded, AnnoLine} = add_annotations(Sn, Prefix, Comment, Anno),
            {PopAdded, PopLine, NewLine, EndPrefix} =
                populate(Sn, Prefix, Pop, AnnoAdded),
            EndLineOpen = ">",
            EndLineClose = "/>",
            if (HasChildren orelse (KW == 'leaf-list' andalso Comment)
                orelse (AnnoAdded andalso Comment)) ->
                    if KW == 'leaf-list' orelse
                       (KW == 'list' andalso Anno) ->
                            NCommentLine = get_min_max_elem_comment(Sn, Prefix);
                       true ->
                            NCommentLine = ""
                    end;
               true ->
                    NCommentLine = ""
            end,
            UpdatedBufClosed =
                [Buf, [X || X <- [NameLine, NamespaceLine,
                                  EndLineClose, NewLine],
                            X =/= ""]],
            UpdatedBufOpen =
                [Buf, [X || X <- [NameLine, NamespaceLine,
                                  EndLineOpen, NewLine, AnnoLine,
                                  PopLine, NCommentLine],
                            X =/= ""]],
            Children = existing_children(Sn#sn.children),
            if Children == [] ->
                    if Doc == "data" andalso not(Config) orelse
                       Doc == "config" andalso Config ->
                            if PopAdded orelse
                               AnnoAdded orelse
                               NCommentLine =/= "" ->
                                    ClosingElement =
                                        [EndPrefix, "</", Name, ">\n"],
                                    EndBuf = [UpdatedBufOpen, ClosingElement];
                               true ->
                                    if Doc == "data" andalso not(Config) orelse
                                       Doc == "config" andalso Config ->
                                            EndBuf = UpdatedBufClosed;
                                       true ->
                                            EndBuf = Buf
                                    end
                            end,
                            print_buf(Fd, EndBuf),
                            ReturnBuf = [];
                       true ->
                            ReturnBuf = Buf
                    end;
               true ->
                    ChildBuf = print_children(existing_children(Sn#sn.children),
                                              Mod, Fd, Prefix, Path, Ctx,
                                              NewNS, Def, Anno, Doc, Pop,
                                              UpdatedBufOpen),
                    ReturnBuf = get_new_buf(ChildBuf, UpdatedBufOpen, Name, Buf,
                                            Config, AnnoLine, NCommentLine,
                                            EndPrefix, Doc, UpdatedBufClosed)
            end
    end,
    ReturnBuf.

get_new_buf(ChildBuf, UpdatedBufOpen, Name, Buf, Config, AnnoLine, NCommentLine,
               EndPrefix, Doc, UpdatedBufClosed) ->
    if ChildBuf =/= UpdatedBufOpen orelse
       (((Doc == "data" andalso not(Config))
         orelse (Doc == "config" andalso Config))
        andalso (AnnoLine =/= "" orelse
                 NCommentLine =/= "")) ->
            ClosingElement = [EndPrefix, "</", Name,
                              ">\n"],
            [ChildBuf, ClosingElement];
       true ->
            if (Doc == "data" andalso not(Config)) orelse
               (Doc == "config" andalso Config) ->
                    UpdatedBufClosed;
               true ->
                    Buf
            end
    end.

get_min_max_elem_comment(Sn, Prefix) ->
    MinE = yang:search_all_stmts(
             'min-elements', Sn#sn.stmt),
    if length(MinE) == 1 ->
            Lo = integer_to_list(element(2, hd(MinE)));
       true ->
            Lo = "0"
    end,
    MaxE = yang:search_all_stmts(
             'max-elements', Sn#sn.stmt),
    if length(MaxE) == 1 ->
            Hi = integer_to_list(element(2, hd(MaxE)));
       true ->
            Hi = ""
    end,
    [Prefix, "  <!-- # entries: ", Lo, "..", Hi, " -->\n"].

add_copies(Sn, Mod, Fd, Prefix, Path, Ctx, CurrentNS, Def, Anno, Doc, Pop, Buf,
           FirstCopy, Counter) when FirstCopy == true ->
    NewBuf = print_node(Sn, Mod, Fd, Prefix, Path, Ctx, CurrentNS, true, Def,
                        Anno, Doc, Pop, Buf),
    add_copies(Sn, Mod, Fd, Prefix, Path, Ctx, CurrentNS, Def, Anno, Doc, Pop,
               NewBuf, false, Counter-1);
add_copies(_Sn, _Mod, _Fd, _Prefix, _Path, _Ctx, _CurrentNS, _Def, _Anno, _Doc,
           _Pop, Buf, _FirstCopy, 0) ->
    Buf;
add_copies(Sn, Mod, Fd, Prefix, Path, Ctx, CurrentNS, Def, Anno, Doc, Pop, Buf,
           _FirstCopy, Counter) ->
    NewBuf = print_node(Sn, Mod, Fd, Prefix, Path, Ctx, CurrentNS, false, Def,
                        Anno, Doc, Pop, Buf),
    add_copies(Sn, Mod, Fd, Prefix, Path, Ctx, CurrentNS, Def, Anno, Doc, Pop,
               NewBuf, false, Counter-1).

new_namespace(Mod, Ctx, NSFilenamePath, CurrentNS, Filename) ->
    case Filename of
        CurrentNS ->
            NewNS = CurrentNS;
        _ ->
            Name = Mod#module.name,
            Imports = Mod#module.imports,
            SplitNSFilenamePath =
                [X || X <-binary:split(?l2b(NSFilenamePath),
                                       [<<"/">>, <<".yang">>],
                                       [global]), X /= <<"">>],
            NSFilename = ?b2l(lists:last(SplitNSFilenamePath)),
            Equal = NSFilename == ?a2l(Name),
            if Equal ->
                    Namespace = Mod#module.namespace,
                    NewNS = ?a2l(Namespace);
               true ->
                    NewNS = new_namespace_find(
                              Imports, Ctx, NSFilename, CurrentNS)
            end
    end,
    if NewNS =/= CurrentNS ->
            NamespaceLine = [" xmlns=\"", NewNS, "\""];
       true ->
            NamespaceLine = ""
    end,
    {NewNS, NamespaceLine}.

new_namespace_find([], _Ctx, _Filename, CurrentNS) ->
    CurrentNS;
new_namespace_find([Import|Imports], Ctx, Filename, CurrentNS) ->
    {TNamespace, _, _TPrefix, _} = Import,
    {_Value, TargetM} = yang:get_module(TNamespace, undefined, Ctx),
    TargetMNamespace = TargetM#module.namespace,
    Equal = ?a2l(TNamespace) == Filename,
    if Equal ->
            ?a2l(TargetMNamespace);
       true ->
            new_namespace_find(Imports, Ctx, Filename, CurrentNS)
    end.

print_buf(_Fd, []) ->
    skip;
print_buf(Fd, [H|T]) ->
    io:format(Fd, "~s", [H]),
    print_buf(Fd, T).

-spec is_data_def(atom(), #yctx{}) -> boolean().
is_data_def(Keyword, #yctx{env = #env{data_definition_stmts = D}}) ->
    not(?in(Keyword,['rpc','action','notification']))
        andalso yang:map_is_key(Keyword, D).

name(Sn, Mod) ->
    LocalName =
        case Sn#sn.name of
            {_, B} -> ?a2l(B);
            A      -> ?a2l(A)
        end,
    if (Sn#sn.module)#module.modulename == Mod#module.modulename ->
            LocalName;
       true ->
            [?a2l((Sn#sn.module)#module.prefix), ":", LocalName]
    end.

add_annotations(_,  _,      false, _) -> {false, ""};
add_annotations(_,  _,      _,     false) -> {false, ""};
add_annotations(Sn, Prefix, true,  true) ->
    TypeExists = yang:search_all_stmts('type', Sn#sn.stmt),
    KeysExists = Sn#sn.keys,
    PresenceExists = yang:search_all_stmts('presence', Sn#sn.stmt),
    case TypeExists of
        [{_, Type, _, _}|_] ->
            if is_tuple(Type) ->
                    {H, T} = Type,
                    TypeLine = [Prefix, "  <!-- type: ", ?a2l(H),
                                ":", ?a2l(T), " -->\n"];
               true ->
                    TypeLine = [Prefix, "  <!-- type: ", ?a2l(Type) , " -->\n"]
            end;
        _ ->
            TypeLine = ""
    end,
    case KeysExists of
        [Key|_] ->
            KeyLine = [Prefix, "  <!-- # keys: ", ?a2l(Key), " -->\n"];
        _ ->
            KeyLine = ""
    end,
    case PresenceExists of
        [{_, Presence, _, _}|_] ->
            PresenceLine =  [Prefix, "  <!-- presence: ",
                             ?b2l(Presence), " -->\n"];
        _ ->
            PresenceLine = ""
    end,
    AnnoBuf = [TypeLine, KeyLine, PresenceLine],
    {AnnoBuf =/= [[],[],[]], AnnoBuf}.

populate(_, Prefix, false, _) -> {false, "", "\n", Prefix};
populate(Sn, Prefix, Pop, AnnoAdded) ->
    TypeExists = yang:search_all_stmts('type', Sn#sn.stmt),
    case AnnoAdded of
        true ->
            Front = ["  ", Prefix],
            End = "\n";
        _ ->
            Front = "",
            End = ""
    end,
    case TypeExists of
        [] ->
            PopAdded = false,
            PopLine = "";
        _ ->
            {_, Type, _, _} = hd(TypeExists),
            case maps:find(Type, Pop) of
                {ok, Find} ->
                    PopAdded = true,
                    PopLine = [Front, Find, End];
                _ ->
                    PopAdded = false,
                    PopLine = ""
            end
    end,
    case PopAdded andalso not(AnnoAdded)of
        true ->
            NewLine = "",
            EndPrefix = "";
        _ ->
            NewLine = "\n",
            EndPrefix = Prefix
    end,
    {PopAdded, PopLine, NewLine, EndPrefix}.
