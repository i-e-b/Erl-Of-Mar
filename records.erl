-module(records).
-compile(export_all).

-record(robot, {name,
                type=industrial,
                hobbies,
                details=[]}).

-record(user, {id, name, group, age}).

% filtering by record
admin_panel(#user{name=Name, group=admin}) ->
    Name ++ " is allowed";
admin_panel(#user{name=Name}) ->
    Name ++ " is not allowed".

% don't need to define all of #user, so can be extended easily
is_adult(U = #user{}) when U#user.age >= 18 ->
    allowed;
is_adult(_) ->
    forbidden.

first_robot() ->
    #robot{name="Mechatron",
           type=handmade,
           details=["Moved by a small man inside"]}.

car_factory(CorpName)->
    #robot{name=CorpName, hobbies="building cars"}.

