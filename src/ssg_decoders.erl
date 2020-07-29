-module(ssg_decoders).

-export([
	point/0,
	power_context/3
]).

-spec point() -> from_json:decoder({number(), number()}).
point() ->
	from_json:tuple({0, 0}, [{1, from_json:field(<<"x">>, from_json:number())}, {2, from_json:field(<<"y">>, from_json:number())}]).

power_context(BattleId, PowerId, CallingPlayerId) ->
	ShipIdDecoder = {1, from_json:optional_field(<<"target_ship">>, undefined, from_json:nullable(from_json:integer()))},
	PointDecoder = {2, from_json:optional_field(<<"target_point">>, undefined, from_json:nullable(point()))},
	FieldDecoders = [ ShipIdDecoder, PointDecoder],
	MappingFun = fun({ShipId, Point}) ->
		ssg_btl_power:make_context(BattleId, PowerId, CallingPlayerId, Point, ShipId)
	end,
	from_json:map(MappingFun, from_json:tuple({undefined, undefined}, FieldDecoders)).

