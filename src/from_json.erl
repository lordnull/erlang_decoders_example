%%% Composable json decoding functions for erlang. Heavily inspired by
%%% (Elm Decoders)[https://package.elm-lang.org/packages/elm/json/latest/].
%%% This module exposes functions to generate json decoders that convert
%%% json values to erlang values. The goal is to allow the conversions to
%%% be verifialbe by dialyzer as well as reasonably read-able by humans all
%%% without the overhead of a parse-transform.
%%%
%%% The primary entry point is either `decode_string/2` if you have not
%%% passed a binary through something like `jsx:decode/2`, or
%%% `decode_json/2` if you have.
%%%
%%% The second argument of the `decode_*/2` functions is a 'decoder'. A
%%% decoder is a function that takes in a json value and outputs a result,
%%% either `{ok, A}` or `{error, iolist()}`. There are many provided
%%% primative decoders, such as `number/0`, 'string/0`, `null/0`, and
%%% 'boolean/0':
%%% 
%%%     primative() ->
%%%         {ok, 7} = decode_string(<<"7">>, number()),
%%%         {error, _} = decode_string(<<"\"not a bool\"">>, boolean()).
%%%
%%% For more complex structures, there are composable decoders. For example,
%%% a simple proper list would use the `array/1` decoder, passing in the
%%% decoder for each item:
%%% 
%%%     list_example() ->
%%%         array(number()).
%%%     
%%%     example() ->
%%%          {ok, [1,2,3]} = decode_string(<<"[1,2,3]">>, list_example()),
%%%          {error, _} = decode_string(<<"[\"not a number\"]">>, list_exmpale()).
%%%
%%% Sometimes a value can be null. The default decoder turns a null into the
%%% more idomatic `undefined`.
%%% 
%%%     null_1() ->
%%%         {ok, undefined} = decode_string(<<"null">>, null()),
%%%         {error, _} = decode_string(<<"7">>, null()),
%%%         {ok, undefined} = decode_string(<<"null">>, nullable(integer())),
%%%         {ok, 7} = decode_string(<<"7">>, nullable(integer())).
%%%
%%% One of the most common tasks is turning json into a record. This is
%%% done with a combination of the `field/2`, `optional_field/3`, and
%%% `tuple/2` functions:
%%% 
%%%     -record(dude, {
%%%         id :: integer(),
%%%         name :: binary(),
%%%         pants_names :: [ binary() ]
%%%     }.
%%%     
%%%     dude_decoder() ->
%%%         InitialDude = #dude{},
%%%         IdDecoder = {#dude.id, field(id, number())},
%%%         NameDecoder = {#dude.name, field(name, string())},
%%%         PantsDecoder = {#dude.pants_names, optional_field(pants, [], array(string()))},
%%%         TupleFields = [ IdDecoder, NameDecoder, PantsDecoder ],
%%%         tuple(InitialDude, TupleFields).
%%%    
%%%     example() ->
%%%         {ok _Dude} = decode_json(#{ <<"id">> => 7, <<"name">> => <<"jerry">>}, dude_decoder()),
%%%         {error, _} = decode_json(#{}, dude_decoder()),
%%%         {ok, _} = decode_json(#{ id => 23, name => <<"joe">>, pants => [ <<"jeff">> ]}, dude_decoder()).

-module(from_json).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type json_number() :: number().
-type json_string() :: binary().
-type json_null() :: null.
-type json_boolean() :: true | false.
-type json_array() :: [ json() ].
-type json_array(T) :: [ T ].
-type json_key() :: atom() | binary().
-type json_object() :: #{ json_key() => json() }.
-type json() :: json_number() | json_string() | json_null() | json_boolean() | json_array() | json_object().
-type decoder(T) :: fun((json()) -> decode_result(T)).
-type decode_result(T) :: {ok, T} | {error, iolist()}.

-export_type([ json_number/0, json_string/0, json_null/0, json_boolean/0,
	json_array/0, json_array/1, json_object/0, json/0, decoder/1 ]).

-export([ decode_string/2, decode_json/2 ]).
-export([ number/0, integer/0, string/0, null/0, boolean/0]).
-export([ array/1, object/1, proplist/1]).
-export([ map/2, field/2, optional_field/3, at/2, index/2 ]).
-export([ nullable/1, maybe/1, one_of/1, as_is/0, ok/1, fail/1]).
-export([ fold_over/2, tuple/2, validate/2]).

%% @doc Take a string and, using jsx, decode it into json, then calls
%% `decode_json/2`.
-spec decode_string(binary(), decoder(T)) -> decode_result(T).
decode_string(String, Decoder) ->
	decode_json(jsx:decode(String, [return_maps]), Decoder).

%% @doc decodes the given json. Remember, a decoder is just a function that
%% takes json and outputs `{ok, T} | {error, iolist}'.
-spec decode_json(json(), decoder(T)) -> decode_result(T).
decode_json(Json, Decoder) ->
	Decoder(Json).

-spec number() -> decoder(number()).
number() ->
	fun number/1.

-ifdef(TEST).
number_test_() ->
	[ fun() -> {ok, 3} = decode_json(3, number()) end
	, fun() -> {error, _} = decode_json(null, number()) end
	, fun() -> {ok, 27.5} = decode_json(27.5, number()) end
	].
-endif.

number(Json) when is_integer(Json); is_float(Json) ->
	{ok, Json};
number(_) ->
	{error, <<"not number">>}.

-spec integer() -> decoder(integer()).
integer() ->
	fun integer/1.

-ifdef(TEST).
integer_test_() ->
	[ fun() -> {ok, 67} = decode_json(67, integer()) end
	, fun() -> {error, _} = decode_json(35.8, integer()) end
	, fun() -> {error, _} = decode_json(2.0, integer()) end
	].
-endif.

integer(Json) when is_integer(Json) ->
	{ok, Json};
integer(_) ->
	{error, <<"not an integer">>}.

-spec string() -> decoder(binary()).
string() ->
	fun string/1.

-ifdef(TEST).
string_test_() ->
	[ fun() -> {ok, <<"hi!">>} = decode_json(<<"hi!">>, string()) end
	, fun() -> {error, _} = decode_json(32, string()) end
	, fun() -> {error, _} = decode_json(null, string()) end
	].
-endif.

string(Json) when is_binary(Json) ->
	{ok, Json};
string(_) ->
	{error, <<"not string">>}.

-spec null() -> decoder(undefined).
null() ->
	fun null/1.

-ifdef(TEST).
null_test_() ->
	[ fun() -> {ok, undefined} = decode_json(null, null()) end
	, fun() -> {error, _} = decode_json(<<"hi!">>, null()) end
	, fun() -> {error, _} = decode_json(#{}, null()) end
	].
-endif.

null(null) ->
	{ok, undefined};
null(_) ->
	{error, <<"not null">>}.

-spec boolean() -> decoder(boolean()).
boolean() ->
	fun boolean/1.

-ifdef(TEST).
boolean_test_() ->
	[ fun() -> {ok, true} = decode_json(true, boolean()) end
	, fun() -> {ok, false} = decode_json(false, boolean()) end
	, fun() -> {error, _} = decode_json(5, boolean()) end
	].
-endif.

boolean(Json) when Json =:= true; Json =:= false ->
	{ok, Json};
boolean(_) ->
	{error, <<"not boolean">>}.

%% @doc apply a given decoder to each element of a json array and return a
%% decoder for an erlang list.
-spec array(decoder(T)) -> decoder([T]).
array(Decoder) ->
	fun(Json) ->
		array(Json, Decoder)
	end.

-ifdef(TEST).
array_test_() ->
	[ fun() -> {ok, [1,4,7]} = decode_json([1,4,7], array(number())) end
	, fun() -> {ok, [<<"hi">>, <<"yo">>]} = decode_json([<<"hi">>, <<"yo">>], array(string())) end
	, fun() -> {error, _} = decode_json([<<"hi">>], array(number())) end
	, fun() -> {ok, [false]} = decode_json([false], array(boolean())) end
	, fun() -> {error, _} = decode_json(#{}, array(null())) end
	].
-endif.

array(Json, Validator) when is_list(Json) ->
	array_map(Json, Validator, 0, []);
array(_Json, _Validator) ->
	{error, <<"not array">>}.

array_map([], _Validator, _Count, Acc) ->
	{ok, lists:reverse(Acc)};
array_map([Hd | Tail], Validator, Count, Acc) ->
	case Validator(Hd) of
		{ok, NewVal} ->
			array_map(Tail, Validator, Count + 1, [NewVal | Acc]);
		{error, Wut} ->
			{error, [Wut, <<" in array at item">>, integer_to_list(Count)]}
	end.

%% @doc Upon successful completion of the given decoder, transform the
%% value, returning a decoder for the new type. Generally used to make
%% move complex decoders.
%% 
%%     example() ->
%%         {ok, 9} = decode_json(3, map(fun(N) -> N * N end, integer()),
%%         {error, _} = decode_json([], map(fun(N) -> N * N end, integer()).
-spec map(fun((A) -> B), decoder(A)) -> decoder(B).
map(ValueTransform, Decoder) ->
	fun(Json) ->
		case Decoder(Json) of
			{ok, MidValue} ->
				{ok, ValueTransform(MidValue)};
			Error ->
				Error
		end
	end.

-ifdef(TEST).
map_test_() ->
	[ fun() -> {ok, pants} = decode_json(3, map(fun(_) -> pants end, number())) end
	, fun() -> {error, _} = decode_json(null, map(fun(_) -> pants end, number())) end
	].
-endif.

%% @doc If the json value is null, succeed with `undefined`, else try the
%% given decoder.
-spec nullable(decoder(T)) -> decoder(T | undefined).
nullable(Decoder) ->
	fun
		(null) ->
			{ok, undefined};
		(Json) ->
			Decoder(Json)
	end.

-ifdef(TEST).
nullable_test_() ->
	[ fun() -> {ok, undefined} = decode_json(null, nullable(number())) end
	, fun() -> {ok, 7} = decode_json(7, nullable(number())) end
	, fun() -> {error, _} = decode_json(<<"hi">>, nullable(number())) end
	].
-endif.

%% @doc Turn a json object into a map using the given map as the source
%% keys and decoders for each field in the json object. Keys not in the
%% source map are ignored.
%% 
%%     example() ->
%%         {ok, _} = decode_json(#{ <<"k">> => 7}, object(#{ k => integer() }),
%%         {error, _} = decode_json(#{ }, object(#{ k => integer() })),
%%         {ok, _} = decode_json(#{ <<"a">> => 9, <<"k">> => 7}, object(${ k => integer() })).
-spec object(#{json_key() => decoder(T)}) -> decoder(#{json_key() => decoder(T)}).
object(DecodeMap) when is_map(DecodeMap) ->
	fun(Json) ->
		decode_object(Json, DecodeMap, #{})
	end.

-ifdef(TEST).
object_test_() ->
	Input = #{
		<<"int">> => 7,
		<<"str">> => <<"goober">>,
		<<"bool">> => false,
		<<"obj">> => #{}
	}, [
	fun() ->
		Expected = #{<<"int">> => 7, <<"bool">> => false},
		Decoder = #{<<"int">> => number(), <<"bool">> => boolean()},
		{ok, Expected} = decode_json(Input, object(Decoder))
	end,
	fun() ->
		Decoder = #{<<"int">> => string()},
		{error, _} = decode_json(Input, object(Decoder))
	end,
	fun() ->
		Decoder = #{},
		{ok, #{}} = decode_json(Input, object(Decoder))
	end,
	fun() ->
		Decoder = #{<<"obj">> => object(#{})},
		{ok, #{<<"obj">> := #{}}} = decode_json(Input, object(Decoder))
	end ].
-endif.

decode_object(Json, DecodeMap, AccObj) when is_map(Json), is_map(DecodeMap) ->
	DecodeList = maps:to_list(DecodeMap),
	decode_object(Json, DecodeList, AccObj);
decode_object(_Json, [], Acc) ->
	{ok, Acc};
decode_object(Json, [{Key, Decoder} | Tail], Acc) when is_map(Json) ->
	case Decoder(maps:get(Key, Json, undefined)) of
		{ok, Val} ->
			decode_object(Json, Tail, Acc#{Key => Val});
		{error, Wut} ->
			{error, [<<"invalid value at key ">>, to_json_key(Key), " ", Wut]}
	end.

to_json_key(Atom) when is_atom(Atom) ->
	atom_to_binary(Atom, utf8);
to_json_key(Binary) when is_binary(Binary) ->
	Binary.

%% @doc Essentially the same as `object/1`, but outputs a proplist instead
%% of a map.
-spec proplist(#{json_key() => decoder(T)}) -> decoder([{ json_key(), decoder(T)}]).
proplist(DecodeMap) when is_map(DecodeMap) ->
	DecodeList = maps:to_list(DecodeMap),
	fun(Json) ->
		decode_to_proplist(Json, DecodeList, [])
	end.

-ifdef(TEST).
proplist_test_() ->
	[ fun() ->
		{ok, [{<<"a">>, 3}]} = decode_json(#{<<"a">> => 3}, proplist(#{<<"a">> => number()}))
	end
	, fun() ->
		{ok, []} = decode_json(#{<<"a">> => 3}, proplist(#{}))
	end
	, fun() ->
		{error, _} = decode_json(#{<<"a">> => <<"hi">>}, proplist(#{<<"a">> => number()}))
	end ].
-endif.

decode_to_proplist(Json, _Decoders, _Acc) when not is_map(Json) ->
	{error, <<"not an object">>};
decode_to_proplist(_Json, [], Acc) ->
	{ok, Acc};
decode_to_proplist(Json, [{Key, Decoder} | Tail], Acc) ->
	case Decoder(maps:get(Key, Json, undefined)) of
		{ok, Val} ->
			decode_to_proplist(Json, Tail, [{Key, Val} | Acc]);
		{error, Wut} ->
			{error, [<<"error at key ">>, to_json_key(Key), " ", Wut]}
	end.

%% @doc Extract a field from a json object. If the json value is not an
%% object, or the field is missing, the decode fails.
-spec field(json_key(), decoder(T)) -> decoder(T).
field(Key, Decoder) ->
	fun(Json) ->
		decode_field(Json, Key, Decoder)
	end.

-ifdef(TEST).
field_test_() ->
	[ fun() -> {ok, 3} = decode_json(#{<<"a">> => 3}, field(<<"a">>, number())) end
	, fun() -> {error, _} = decode_json(#{<<"a">> => 3}, field(<<"a">>, string())) end
	].
-endif.

decode_field(Json, _Key, _Validator) when not is_map(Json) ->
	{error, <<"not an object">>};
decode_field(Json, Key, Validator) when is_atom(Key) ->
	case maps:find(Key, Json) of
		error ->
			decode_field(Json, atom_to_binary(Key, utf8), Validator);
		{ok, Value} ->
			Validator(Value)
	end;
decode_field(Json, Key, Validator) ->
	case maps:find(Key, Json) of
		error ->
			{error, [Key, <<" is missing">>]};
		{ok, Value} ->
			Validator(Value)
	end.

%% @doc The optional version of `field/2`, allowing you to supply the value
%% to use if the field is missing. If the json value is not an object, the
%% decode fails.
-spec optional_field(json_key(), T, decoder(T)) -> decoder(T).
optional_field(Key, IfMissing, Decoder) ->
	fun(Json) ->
		decode_optional_field(Json, Key, IfMissing, Decoder)
	end.

-ifdef(TEST).
optional_field_test_() ->
	[ fun() ->
		{ok, 3} = decode_json(#{}, optional_field(<<"a">>, 3, number()))
	end,
	fun() ->
		{ok, 7} = decode_json(#{<<"a">> => 7}, optional_field(<<"a">>, 3, number()))
	end,
	fun() ->
		{error, _} = decode_json(#{<<"a">> => <<"hi">>}, optional_field(<<"a">>, 3, number()))
	end
	].
-endif.

decode_optional_field(Json, Key, IfMissing, Validator) ->
	case maps:find(Key, Json) of
		error ->
			{ok, IfMissing};
		{ok, J} ->
			Validator(J)
	end.

%% @doc attempt to decode a field in a nested object.
-spec at([json_key()], decoder(T)) -> decoder(T).
at(KeyPath, Decoder) ->
	fun(Json) ->
		at(Json, KeyPath, [], Decoder)
	end.

-ifdef(TEST).
at_test_() ->
	[ fun() ->
		{ok, 3} = decode_json(#{a => #{ b => 3}}, at([a,b], number()))
	end,
	fun() ->
		{error, _} = decode_json(3, at([a,b], number()))
	end,
	fun() ->
		{error, _} = decode_json(#{a => #{ b => <<"hi">>}}, at([a,b],number()))
	end ].
-endif.

at(Json, [], _PathWalked, Validator) ->
	Validator(Json);
at(Json, [LastPath], PathWalked, Decoder) when is_map(Json) ->
	case Decoder(maps:get(LastPath, Json, undefined)) of
		{ok, _} = Ok ->
			Ok;
		{error, Wut} ->
			{error, [pretty_print_key([LastPath | PathWalked]), <<" did not pass decoder: ">>, Wut]}
	end;
at(Json, [Key | Tail], PathWalked, Validator) when is_map(Json) ->
	case maps:get(Key, Json, undefined) of
		Obj when is_map(Obj) ->
			at(Obj, Tail, [Key | PathWalked], Validator);
		_NotObj ->
			{error, [pretty_print_key([Key | PathWalked]), <<" is not an object">>]}
	end;
at(_Json, _Keys, PathWalked, _Decoder) ->
	{error, [pretty_print_key(PathWalked), <<" is not an object">>]}.

pretty_print_key([]) ->
	[];
pretty_print_key([Key]) ->
	[Key];
pretty_print_key([H | T]) ->
	pretty_print_key(T, [H]).

pretty_print_key([P], Acc) ->
	[P, "." | Acc];
pretty_print_key([P | Tail], Acc) ->
	pretty_print_key(Tail, [P, ".", Acc]).

%% @doc Attempt to decode a value at a given index of a json array. The
%% index starts at 0 since we are using the concept of 'json array' rather
%% than 'erlang list' for the input.
-spec index(non_neg_integer(), decoder(T)) -> decoder(T).
index(Idx, Decoder) ->
	fun(Json) ->
		index(Json, Idx, Decoder)
	end.

-ifdef(TEST).
index_test_() ->
	[ fun() -> {error, _} = decode_json([], index(0, number())) end
	, fun() -> {ok, 3} = decode_json([1,2,3], index(2, number())) end
	, fun() -> {error, _} = decode_json([1,2,<<"hi">>], index(2, number())) end
	].
-endif.

index([], _Idx, _Decoder) ->
	{error, <<"empty list">>};
index(Json, Idx, _Decoder) when is_list(Json), length(Json) < Idx ->
	{error, <<"list too short">>};
index(Json, Idx, Decoder) when is_list(Json) ->
	Decoder(lists:nth(Idx + 1, Json));
index(_Json, _Idx, _Decoder) ->
	{error, <<"not a list">>}.

-spec maybe(decoder(T)) -> decoder(T | undefined).
maybe(Decoder) ->
	fun(Json) ->
		maybe(Json, Decoder)
	end.

-ifdef(TEST).
maybe_test_() ->
	[ fun() -> {ok, undefined} = decode_json(3, maybe(string())) end
	, fun() -> {ok, 6} = decode_json(6, maybe(number())) end
	].
-endif.

maybe(Json, Decoder) ->
	case Decoder(Json) of
		{error, _} ->
			{ok, undefined};
		Ok ->
			Ok
	end.

%% @doc Given a list of decoders, keep trying them until either one works,
%% or all have been tried.
%% 
%%     timeout() ->
%%         one_of([map(fun(_) -> infinity end, null()), integer()]).
%%     
%%     example() ->
%%         {ok, infinity} = decode_json(null, timeout()),
%%         {ok, 77} = decode_json(77, timeout()),
%%         {error, _} = decode_json(<<"not a timeout">>, timeout()).
-spec one_of([decoder(T)]) -> decoder(T).
one_of(Decoders) when is_list(Decoders) ->
	fun(Json) ->
		one_of(Json, Decoders)
	end.

-ifdef(TEST).
one_of_test_() ->
	[ fun() -> {ok, 3} = decode_json(3, one_of([number(), string()])) end
	, fun() -> {ok, <<"hi">>} = decode_json(<<"hi">>, one_of([number(), string()])) end
	, fun() -> {error, _} = decode_json(false, one_of([number(), string()])) end
	].
-endif.

one_of(_Json, []) ->
	{error, <<"no valid conversions">>};
one_of(Json, [ValidatorHead | Tail]) ->
	case ValidatorHead(Json) of
		{error, _} ->
			one_of(Json, Tail);
		{ok, _} = Ok ->
			Ok
	end.

%% @doc Just return a json value. Can be useful if you want to do processing
%% later, or just don't care about it and are passing it through.
-spec as_is() -> decoder(json()).
as_is() ->
	fun(Json) -> Json end.

-ifdef(TEST).
as_is_test() ->
	[ fun() -> {ok, 3} = decode_json(3, as_is()) end
	, fun() -> {ok, null} = decode_json(null, as_is()) end
	].
-endif.

%% @doc A decoder that always successed with the given value. Can be useful
%% to provide hardcoded values in a `object/2` or `tuple/2` decoder.
-spec ok(T) -> decoder(T).
ok(Value) ->
	fun(_) -> {ok, Value} end.

-ifdef(TEST).
ok_test() ->
	{ok, {1,5}} = decode_json(false, ok({1,5})).
-endif.

%% @doc A decoder that always fails with the given reason.
-spec fail(binary()) -> decoder(none()).
fail(Why) ->
	fun(_) -> {error, Why} end.

-ifdef(TEST).
fail_test() ->
	{error, <<"ugh">>} = decode_json(true, fail(<<"ugh">>)).
-endif.

%% @doc Allows a series of decoders to fold into one value using an
%% accumulator. If any decoder fails, the whole thing fails.
%% 
%%     example() ->
%%         InitialHeading = headings:empty(),
%%         SetHeading = headings:set_angle/2,
%%         SetMag = headings:set_magnitude/2,
%%         fold_over(InitialHeading, [
%%             {field(angle, number()), SetHeading},
%%             {field(mag, number()), SetMag}
%%          ]).
-spec fold_over(S, [{decoder(T), fun((T,S) -> S)}]) -> decoder(S).
fold_over(InitState, DecodeAndUpdates) when is_list(DecodeAndUpdates) ->
	fun(Json) ->
		fold_over(Json, InitState, 1, DecodeAndUpdates)
	end.

-ifdef(TEST).
fold_over_test_() ->
	UDs = [{number(), fun(N,S) -> N * S end} || _ <- lists:seq(1,5)],
	[ fun() ->
		{ok, 32} = decode_json(2, fold_over(1, UDs))
	end,
	fun() ->
		{error, _} = decode_json(1, fold_over(1, [{string(), fun(_, _) -> ok end} | UDs]))
	end ].
-endif.

fold_over(_Json, FinalState, _Step, []) ->
	{ok, FinalState};
fold_over(Json, State, Step, [{Decode, Update} | Tail]) ->
	case Decode(Json) of
		{ok, Value} ->
			fold_over(Json, Update(Value, State), Step + 1, Tail);
		{error, Wut} ->
			{error, [<<"fold failed at step ">>, integer_to_list(Step), <<"due to ">>, Wut]}
	end.

%% @doc Used primarily to generate records from json, since you can
%% reference a record's field number by name using the
%% `#RecordName.FieldName' syntax if you have access to the record
%% definition. An example of that is at the top. Here we'll do a point
%% example.
%% 
%%     point_decoder() ->
%%         tuple({0,0}, [
%%             {1, number()},
%%             {2, number()}
%%          ]).
-spec tuple(tuple(), [{pos_integer(), decoder(_)}]) -> decoder(tuple()).
tuple(InitialRecord, Decoders) ->
	fun(Json) ->
		tuple(Json, InitialRecord, Decoders)
	end.

-ifdef(TEST).
tuple_test_() ->
	[ fun() ->
		{ok, {r, 7}} = decode_json(#{f => 7}, tuple({r, undefined}, [{2, field(f, number())}]))
	end,
	fun() ->
		{ok, {r, 7}} = decode_json(7, tuple({r, undefined}, [{2, number()}]))
	end,
	fun() ->
		{error, _} = decode_json(#{f => <<"hi">>}, tuple({r, undefined}, [{2, number()}]))
	end ].
-endif.

tuple(_Json, Record, []) ->
	{ok, Record};
tuple(Json, Record, [{FieldIdx, Decoder} | Tail]) ->
	case Decoder(Json) of
		{ok, Value} ->
			tuple(Json, setelement(FieldIdx, Record, Value), Tail);
		{error, Wut} ->
			{error, [<<"for field idx ">>, integer_to_list(FieldIdx), Wut]}
	end.

%% @doc Apply additional validation to a decoder, potentially returning a
%% new result.
%% 
%%     example() ->
%%        LessThan3 = fun(N) when N < 3 -> {ok, N * 2};
%%                       (_) -> {error, <<"number too big!">> end,
%%        Decoder = validate(LessThan3, number()),
%%        {ok, 2} = decode_value(1, Decoder),
%%        {error, <<"number too big!">>} = decode_value(7, Decoder),
%%        {ok, -7} = decode_value(-3.5, Decoder).
-spec validate(fun((A) -> decode_result(B)), decoder(A)) -> decoder(B).
validate(Map, Decoder) ->
	fun(Json) ->
		validate(Json, Map, Decoder)
	end.

-ifdef(TEST).
validate_test_() ->
	Validate = fun(N) when N < 0 ->	 {error, too_low};
		(N) when N < 5 -> {ok, low};
		(N) when is_integer(N) -> {ok, high}
	end,
	[ fun() -> {ok, low} = decode_json(1, validate(Validate, number())) end
	, fun() -> {ok, high} = decode_json(200, validate(Validate, number())) end
	, fun() -> {error, _} = decode_json(<<"hi">>, validate(Validate, number())) end
	, fun() -> {error, too_low} = decode_json(-70, validate(Validate, number())) end
	].
-endif.

validate(Json, Map, Decoder) ->
	case Decoder(Json) of
		{ok, Value} ->
			Map(Value);
		Error ->
			Error
	end.
