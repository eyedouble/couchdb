%% @private
-module(edoc_modern_doc_html).
-export([
	module/1
]).

-include("./edoc_modern_doc.hrl").

-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

-spec module(#module{}) -> [tuple()].
module(#module{name = Name, description = Description, types = Types, functions = Functions}=Module) ->
	[{article, [{class, "module"}], [
		{h1, [Name]},
		layout_module_doc(Description),
		layout_module_summary(Types, Functions),
		layout_module_types(Types),
		layout_module_functions(Functions),
		?PRINT(Module)

	]}].

%% @private
layout_module_doc(undefined) ->
	[];
layout_module_doc(#description{brief = Brief, full = Full}) ->
	{section, [{class, "description"}], [
		{p, Brief},
		if
			Full =:= undefined -> [];
			is_list(Full) -> {p, Full}
		end
	]}.

%% @private
layout_module_summary([], []) ->
	[];
layout_module_summary(Types, Functions) ->
	{section, [{class, "summary"}], [
		{h1, [anchor("#summary", "Summary")]},
		layout_summary("types", "Types", Types),
		layout_summary("functions", "Functions", Functions)
		%% TODO: add callbacks
	]}.

%% @private
layout_summary(_, _, []) ->
	[];
layout_summary(Id, Title, Items) ->
	{'div', [{class, "summary summary-" ++ Id}], [
		{h2, [anchor("#" ++ Id, Title)]} |
		[layout_summary_row(Item) || Item <- Items]
	]}.

%% @private
layout_summary_row(#typedecl{label = Label, name = Name, args = Args, description = Description}) ->
	{'div', [{class, "summary-row"}], [
		{'div', [{class, "summary-signature"}], [anchor("#" ++ Label, type_name(Name, Args))]},
		layout_description_brief("summary-synopsis", Description)
	]};
layout_summary_row(#function{label = Label, name = Name, args = Args, description = Description}) ->
	{'div', [{class, "summary-row"}], [
		{'div', [{class, "summary-signature"}], [anchor("#" ++ Label, function_name(Name, Args))]},
		layout_description_brief("summary-synopsis", Description)
	]}.

%% @private
layout_module_types([]) ->
	[];
layout_module_types(Types) ->
	{section, [{id, "types"}, {class, "types details-list"}], [
		{h1, [{class, "section-heading"}], [anchor("#types", "Types")]},
		{'div', [{class, "types-list"}], [
			layout_module_type(Type) || Type <- Types
		]}
	]}.

%% @private
layout_module_type(#typedecl{label = Label, name = Name, args = Args, description = Description}) ->
	{'div', [{id, Label}, {class, "type-detail"}], [
		{pre, [{code, [{class, "erlang"}], [type_spec(Name, Args)]}]},
		layout_description("typespec-doc", Description)
	]}.

%% @private
layout_module_functions([]) ->
	[];
layout_module_functions(Functions) ->
	{section, [{id, "functions"}, {class, "functions details-list"}], [
		{h1, [{class, "section-heading"}], [anchor("#functions", "Functions")]},
		{'div', [{class, "functions-list"}], [
			layout_module_function(Function) || Function <- Functions
		]}
	]}.

%% @private
layout_module_function(#function{label = Label, name = Name, args = Args, description = Description}=Func) ->
	?PRINT(Func),
	{'div', [{id, Label}, {class, "detail"}], [
		{'div', [{class, "detail-header"}], [
			{span, [{class, "signature"}], [function_spec(Name, Args)]}
		]},
		%% TODO: add specs
		{'dev', [], ["ZZZZZ"]},
		layout_description("docstring", Description)
	]}.

%% @private
layout_description_brief(_, undefined) ->
	[];
layout_description_brief(Class, Description) ->
	{'div', [{class, Class}], [Description#description.brief]}.

%% @private
layout_description(_, undefined) ->
	[];
layout_description(Class, Description) ->
	{'div', [{class, Class}], [Description#description.full]}.

anchor(Href, Title) ->
	{a, [{href, Href}], [Title]}.

type_name(Name, Args) ->
	Name ++ "/" ++ integer_to_list(length(Args)).

type_spec(Name, Args) ->
	Name ++ "/" ++ integer_to_list(length(Args)).

function_name(Name, Args) ->
	Name ++ "(" ++ string:join([Arg#argument.name || Arg <- Args], ", ") ++ ")".

function_spec(Name, Args) ->
	Name ++ "(" ++ string:join([Arg#argument.name || Arg <- Args], ", ") ++ ")".
