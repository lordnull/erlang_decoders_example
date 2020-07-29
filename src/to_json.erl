-module(to_json).

-type encoder(A) :: fun((A) -> from_json:json()).

-export([string/1, integer/1, number/1, boolean/1, null/0, maybe/2, array/2, object/1]).
-export_type([encoder/1]).

-spec string(Input :: binary() | atom()) -> from_json:json_string().
string(Binary) when is_binary(Binary) ->
	Binary;
string(Atom) when is_atom(Atom) ->
	atom_to_binary(Atom, utf8).

-spec integer(N :: integer()) -> from_json:json_number().
integer(N) when is_integer(N) ->
	N.

-spec number(N :: number()) -> from_json:json_number().
number(N) when is_integer(N); is_float(N) ->
	N.

-spec boolean(true) -> true;
	(false) -> false.
boolean(true) -> true;
boolean(false) -> false.

-spec null() -> from_json:json_null().
null() ->
	null.

-spec maybe(Encoder :: encoder(A), Value :: A | undefined) -> from_json:json().
maybe(_Encoder, undefined) ->
	null();
maybe(Encoder, Value) ->
	Encoder(Value).

-spec array(Encoder :: encoder(A), Values :: [ A ]) -> from_json:json_array().
array(Encoder, Values) ->
	lists:map(Encoder, Values).

-spec object(from_json:json_object() | proplists:proplist()) -> from_json:json_object().
object(Map) when is_map(Map) ->
	Map;
object(Proplist) ->
	object(maps:from_list(Proplist)).

