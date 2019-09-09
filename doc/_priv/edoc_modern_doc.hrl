-record(description, {
	brief = required :: string(),
	full = required :: string()
}).

-record(author, {
	name = required :: string(),
	email :: string() | undefined,
	website :: string() | undefined
}).

-record(see, {
	name = required :: string(),
	href :: string() | undefined
}).

-record(behaviour, {
	href :: string() | undefined,
	details = required :: string()
}).

-record(callback, {
	name = required :: string(),
	arity = required :: non_neg_integer(),
	optional = false :: boolean()
}).

-record(type, {
	name :: string() | undefined,
	type :: type()
}).
-record(type_var, { name = required :: string() }).
-record(type_atom, { value = required :: string() }).
-record(type_integer, { value = required :: string() }).
-record(type_float, { value = required :: string() }).
-record(type_range, { value = required :: string() }).
-record(type_binary, { value = required :: string() }).
-record(type_paren, { type = required :: #type{} }).
-record(type_list, { type = required :: #type{}, non_empty = false :: boolean() }).
-record(type_map, { }).
-record(type_tuple, { types = required :: [#type{}] }).
-record(type_fun, { arg_types = required :: [#type{}], type = required :: #type{} }).
-record(type_record, { name = required :: string() }).
-record(type_abstract, { name = required :: string(), types = required :: [#type{}] }).
-record(type_union, { types = required :: [#type{}] }).
-record(type_nil, { }).
-type type() :: #type_var{} | #type_atom{} | #type_integer{} |
	#type_float{} | #type_range{} | #type_binary{} |
	#type_paren{} | #type_list{} | #type_map{} | #type_tuple{} |
	#type_fun{} | #type_record{} | #type_abstract{} | #type_union{} |
	#type_nil{}.

-record(typedecl, {
	label = required :: string(),
	name = required :: string(),
	args = [] :: [#type{}],
	description :: #description{} | undefined
}).

-record(argument, {
	name = required :: string(),
	description :: #description{} | undefined
}).
-record(function, {
	label = required :: string(),
	name = required :: string(),
	arity = required :: string(),
	exported = required :: boolean(),
	args = [] :: [#argument{}],
	description :: #description{} | undefined,
	since :: string() | undefined,
	deprecated :: #description{} | undefined,
	see = [] :: [#see{}],
	todo :: string() | undefined
}).

-record(module, {
	name = required :: string(),
	private = false :: boolean(),
	hidden = false :: boolean(),
	encoding = "UTF-8" :: string(),
	root = "" :: string(),
	arguments = [] :: [string()],
	description :: #description{} | undefined,
	authors = [] :: [#author{}],
	copyright :: string() | undefined,
	version :: string() | undefined,
	since :: string() | undefined,
	deprecated :: #description{} | undefined,
	see = [] :: [#see{}],
	references = [] :: [string()],
	todo :: string() | undefined,
	behaviours = [] :: [#behaviour{}],
	callbacks = [] :: [#callback{}],
	types = [] :: [#typedecl{}],
	functions = [] :: [#function{}]
}).
