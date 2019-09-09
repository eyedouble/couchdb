%% @private
-module(edoc_modern_xmerl_html5).
-export([
	'#xml-inheritance#'/0
]).

-export([
	'#root#'/4,
	'#element#'/5,
	'#text#'/1,
	p/4
]).


'#xml-inheritance#'() ->
	[].

'#text#'(Text) ->
	xmerl_lib:export_text(Text).

'#root#'(Data, _Attrs, [], _E) ->
	Data.

'#element#'(Tag, Data, Attrs, _Parents, _E) ->
	case is_void_element(Tag) of
		false -> closing_element(Tag, Attrs, Data);
		true -> void_element(Tag, Attrs, Data)
	end.

p(Data, Attrs, _Parents, _E) ->
	case xmerl_lib:is_empty_data(Data) of
		true ->
			%% Paragraph elements should never be completely empty.
			closing_element(p, Attrs, "\s");
		false ->
			closing_element(p, Attrs, Data)
	end.

-spec is_void_element(atom()) -> boolean().
is_void_element(area) -> true;
is_void_element(base) -> true;
is_void_element(br) -> true;
is_void_element(col) -> true;
is_void_element(embed) -> true;
is_void_element(hr) -> true;
is_void_element(img) -> true;
is_void_element(input) -> true;
is_void_element(keygen) -> true;
is_void_element(link) -> true;
is_void_element(meta) -> true;
is_void_element(param) -> true;
is_void_element(source) -> true;
is_void_element(track) -> true;
is_void_element(wbr) -> true;
is_void_element(_) -> false.

closing_element(Tag, Attrs, Data) ->
	[xmerl_lib:start_tag(Tag, Attrs), Data, xmerl_lib:end_tag(Tag)].

void_element(Tag, Attrs, Data) ->
	[xmerl_lib:start_tag(Tag, Attrs), Data].
