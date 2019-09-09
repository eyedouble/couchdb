%% @private
-module(edoc_modern_doc).
-export([
	from_source/3
]).

-export([
	to_html/1
]).

-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

-include_lib("xmerl/include/xmerl.hrl").
-include("./edoc_modern_doc.hrl").

-spec from_source(string(), term(), proplists:proplist()) -> {Module :: atom(), #module{}}.
from_source(Source, Env, Options) ->
	?PRINT(Options),
	?PRINT(Env),
	{_Module, Doc} = edoc:get_doc(Source, Env, Options),
	module(Doc).

-spec to_html(#module{}) -> [tuple()].
to_html(#module{} = Doc) ->
	edoc_modern_doc_html:module(Doc).

%% @private
module(#xmlElement{name = module, attributes = Attributes, content = Content}) ->
	lists:foldl(fun
		(#xmlAttribute{name = name, value = Value}, Acc) ->
			Acc#module{name = Value};
		(#xmlAttribute{name = private, value = Value}, Acc) ->
			Acc#module{private = yes_no(Value)};
		(#xmlAttribute{name = hidden, value = Value}, Acc) ->
			Acc#module{hidden = yes_no(Value)};
		(#xmlAttribute{name = encoding, value = Value}, Acc) ->
			Acc#module{encoding = Value};
		(#xmlAttribute{name = root, value = Value}, Acc) ->
			Acc#module{root = Value};

		(#xmlElement{name = description} = Description, Acc) ->
			Acc#module{description = description(Description)};
		(#xmlElement{name = author} = Author, #module{authors = Authors} = Acc) ->
			Acc#module{authors = Authors ++ [author(Author)]};
		(#xmlElement{name = copyright, content = [#xmlText{value = Text}]}, Acc) ->
			Acc#module{copyright = Text};
		(#xmlElement{name = version, content = [#xmlText{value = Text}]}, Acc) ->
			Acc#module{version = Text};
		(#xmlElement{name = since, content = [#xmlText{value = Text}]}, Acc) ->
			Acc#module{since = Text};
		(#xmlElement{name = deprecated} = Deprecated, Acc) ->
			Acc#module{deprecated = description(get_child(description, Deprecated))};
		(#xmlElement{name = see} = See, #module{see = Sees} = Acc) ->
			Acc#module{see = Sees ++ [#see{
				name = get_attr(name, See),
				href = get_attr(href, See, undefined)
			}]};
		(#xmlElement{name = reference, content = [#xmlText{value = Text}]}, #module{references = References} = Acc) ->
			Acc#module{references = References ++ [Text]};
		(#xmlElement{name = todo, content = [#xmlText{value = Text}]}, Acc) ->
			Acc#module{todo = Text};
		(#xmlElement{name = behaviour, content = [#xmlText{value = Text}]} = Behaviour, #module{behaviours = Behaviours} = Acc) ->
			Acc#module{behaviours = Behaviours ++ [#behaviour{
				href = get_attr(href, Behaviour, undefined),
				details = Text
			}]};
		%% TODO: add Callbacks
		(#xmlElement{name = typedecls, content = Typedecls}, Acc) ->
			Acc#module{types = [typedecl(Typedecl) || Typedecl <- Typedecls]};
		(#xmlElement{name = functions, content = Functions}, Acc) ->
			Acc#module{functions = [function(Function) || Function <- Functions]};
		(_, Acc) ->
			Acc
	end, #module{name = ""}, Attributes ++ Content).

%% @private
description(#xmlElement{name = description, content = Content}) ->
	lists:foldl(fun
		(#xmlElement{name = briefDescription, content = [#xmlText{value = Text}]}, Acc) ->
			Acc#description{brief = Text};
		(#xmlElement{name = fullDescription, content = [#xmlText{value = Text}]}, Acc) ->
			Acc#description{full = Text};
		(_, Acc) ->
			Acc
	end, #description{brief = ""}, Content).

%% @private
author(#xmlElement{name = author, attributes = Attributes}) ->
	lists:foldl(fun
		(#xmlAttribute{name = name, value = Value}, Acc) ->
			Acc#author{name = Value};
		(#xmlAttribute{name = email, value = Value}, Acc) ->
			Acc#author{email = Value};
		(#xmlAttribute{name = website, value = Value}, Acc) ->
			Acc#author{email = Value};
		(_, Acc) ->
			Acc
	end, #author{name = ""}, Attributes).

%% @private
typedecl(#xmlElement{name = typedecl} = Typedecl) ->
	Typedef = get_child(typedef, Typedecl),
	#typedecl{
		label = get_attr(label, Typedecl, ""),
		name = get_attr(name, get_child(erlangName, Typedef)),
		args = [type(Type) || Type <- get_children(type, get_child(argtypes, Typedef))],
		description = get_description(Typedecl)
	}.

%% @private
type(#xmlElement{name = type, attributes = Attributes, content = [Type]}) ->
	Name = case lists:keyfind(name, #xmlAttribute.name, Attributes) of
		#xmlAttribute{value = Value} -> Value;
		false -> undefined
	end,
	#type{name = Name, type = type_(Type)}.

%% @private
type_(#xmlElement{name = typevar} = Element) ->
	#type_var{name = get_attr(name, Element)};
type_(#xmlElement{name = atom} = Element) ->
	#type_atom{value = get_attr(value, Element)};
type_(#xmlElement{name = integer} = Element) ->
	#type_integer{value = get_attr(value, Element)};
type_(#xmlElement{name = float} = Element) ->
	#type_float{value = get_attr(value, Element)};
type_(#xmlElement{name = range} = Element) ->
	#type_range{value = get_attr(value, Element)};
type_(#xmlElement{name = binary} = Element) ->
	#type_binary{value = get_attr(value, Element)};
type_(#xmlElement{name = paren} = Element) ->
	#type_paren{type = type(get_child(type, Element))};
type_(#xmlElement{name = list} = Element) ->
	#type_list{type = type(get_child(type, Element))};
type_(#xmlElement{name = nonempty_list} = Element) ->
	#type_list{type = type(get_child(type, Element)), non_empty = true};
type_(#xmlElement{name = map}) ->
	#type_map{}; % TODO: add fields
type_(#xmlElement{name = tuple, content = Content}) ->
	#type_tuple{types = [type(Element) || Element <- Content]};
type_(#xmlElement{name = 'fun'} = Element) ->
	#type_fun{
		arg_types = [type(Type) || Type <- get_children(type, get_child(argtypes, Element))],
		type = type(get_child(type, Element))
	};
type_(#xmlElement{name = record} = Element) ->
	#type_record{name = get_attr(value, get_child(atom, Element))}; %% TODO: add fields
type_(#xmlElement{name = abstype} = Element) ->
	#type_abstract{
		name = get_attr(name, get_child(erlangName, Element)),
		types = [type(Type) || Type <- get_children(type, Element)]
	};
type_(#xmlElement{name = union, content = Content}) ->
	#type_union{types = [type(Type) || Type <- Content]};
type_(#xmlElement{name = nil}) ->
	#type_nil{}.

%% @private
function(#xmlElement{name = function, content = Content} = Function) ->
	lists:foldl(fun
		(#xmlElement{name = args} = Args, Acc) ->
			Acc#function{args = [arg(Arg) || Arg <- get_children(arg, Args)]};
		(#xmlElement{name = typespec}, Acc) ->
			Acc;
		(#xmlElement{name = returns}, Acc) ->
			Acc;
		(#xmlElement{name = throws}, Acc) ->
			Acc;
		(#xmlElement{name = equiv}, Acc) ->
			Acc;
		(#xmlElement{name = description} = Description, Acc) ->
			Acc#function{description = description(Description)};
		(#xmlElement{name = since, content = [#xmlText{value = Text}]}, Acc) ->
			Acc#function{since = Text};
		(#xmlElement{name = deprecated} = Deprecated, Acc) ->
			Acc#function{deprecated = description(get_child(description, Deprecated))};
		(#xmlElement{name = see} = See, #function{see = Sees} = Acc) ->
			Acc#function{see = Sees ++ [#see{
				name = get_attr(name, See),
				href = get_attr(href, See, undefined)
			}]};
		(#xmlElement{name = todo, content = [#xmlText{value = Text}]}, Acc) ->
			Acc#function{todo = Text};
		(_, Acc) ->
			Acc
	end, #function{
		label = get_attr(label, Function, ""),
		name = get_attr(name, Function),
		arity = get_attr(arity, Function),
		exported = yes_no(get_attr(exported, Function, "no"))
	}, Content).

%% @private
arg(#xmlElement{name = arg} = Arg) ->
	#argument{
		name = get_text(argName, Arg),
		description = get_description(Arg)
	}.

%% @private
yes_no("yes") ->
	true;
yes_no(_) ->
	false.

%% @private
get_attr(Name, #xmlElement{attributes = Attributes}) ->
	#xmlAttribute{value = Value} = lists:keyfind(Name, #xmlAttribute.name, Attributes),
	Value.

%% @private
get_attr(Name, #xmlElement{attributes = Attributes}, Default) ->
	case lists:keyfind(Name, #xmlAttribute.name, Attributes) of
		#xmlAttribute{value = Value} -> Value;
		false -> Default
	end.

%% @private
get_child(Name, #xmlElement{content = Elements}) ->
	#xmlElement{} = lists:keyfind(Name, #xmlElement.name, Elements).

%% @private
get_text(Name, #xmlElement{content = Elements}) ->
	#xmlElement{content = Content} = lists:keyfind(Name, #xmlElement.name, Elements),
	case Content of
		[] -> "";
		[#xmlText{value = Text}] -> Text
	end.

%% @private
get_children(Name, #xmlElement{content = Elements}) ->
	[Element || Element <- Elements, Element#xmlElement.name =:= Name].

%% @private
get_description(Element) ->
	case get_children(description, Element) of
		[Description] -> description(Description);
		[] -> undefined
	end.


