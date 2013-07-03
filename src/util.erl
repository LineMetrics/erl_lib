%% Author: heyoka
%% Created: 27.09.2010
%% Description: TODO: Add description to util
-module(util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
%% -export([unix_timestamp/0, unix_timestamp/1, erl_stamp/0, current_micro/0, get_date/0]).
%% -export([c_md5/1, hexstring/1]).
%% -export([get_host_name/0, get_host_name/1, build_node_name/1]).
-compile[export_all].
%%
%% API Functions
%%

% get secs since 1970
unix_timestamp() ->
	LocalDateTime = calendar:datetime_to_gregorian_seconds({date(),time()}),
   UnixEpoch = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
   LocalDateTime - UnixEpoch
	.
unix_timestamp(bin) ->
	list_to_binary(integer_to_list(unix_timestamp())).

% get sec since 0
erl_stamp() -> 
	calendar:datetime_to_gregorian_seconds({date(),time()}).	

% get micro secs since 0
current_micro() ->
  {MegaSecs, Secs, MicroSecs} = os:timestamp(),
	(MegaSecs*1000000 + Secs) * 1000000 + MicroSecs.

% get date in the form DayMonthYear
get_date() ->
	{Year, Month, Day} = erlang:date(),
	io_lib:format("~p~p~p",[Day, Month, Year]).

%% string to hex
hexstring(<<X:128/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~32.16.0b", [X])).

c_md5(String) ->
	hexstring(erlang:md5(String)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% lists %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
shuffle(List) -> 
	[X || {_K, X} <- shuffle_list(List)].

shuffled_sublist(List, N) -> 
	L1 = lists:sublist(shuffle_list(List), N),
	[X || {_K, X} <- L1].

shuffle_list(List) when is_list(List), length(List) > 1 ->
	List1 = [{random:uniform(), X} || X <- List],
	lists:keysort(1, List1);
shuffle_list(L) ->
	L.

%%
%% make a numbered tuple-list out of a list
%% [{1,El1}, {2,El2},{3,El3}, ....]
tuplize(List) ->
	tuplize(List,1,[]).

tuplize([E|T], Count, Acc) ->	
	NewAcc = [{Count, E}| Acc],
	tuplize(T, Count+1, NewAcc);
tuplize([], _, Acc) ->
	Acc.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% node funcs %%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% get a globally registered proc-name on the same host
registered_server_name(Name) when is_list(Name)->
	String = Name ++ "_" ++ get_host_name(),
	list_to_atom(String).

%%%% build a node name on the same host form an erlang -sname
build_node_name(Name) ->
	build_node_name(get_host_name(), Name)
	.

build_node_name(HostName, Name) ->
	list_to_atom(Name ++ "@" ++ HostName).									

build_equalnamed_node(Host) ->
	list_to_atom(get_nodename() ++ "@" ++ Host).

%%%%%%

%% get name part of this host
get_nodename() ->
	get_nodename(node()).
get_nodename(NodeName) ->
	get_nodename_part(NodeName, 1).

%% get_host_namepart for this node
get_host_name() ->
	get_host_name(node()).

%% get host-name part of nodename
get_host_name(NodeName) when is_atom(NodeName) ->
	get_host_name(atom_to_list(NodeName));
get_host_name(NodeName) when is_list(NodeName) ->
	get_nodename_part(NodeName, 2).

get_nodename_part(NodeName, Pos) when is_atom(NodeName) ->
	get_nodename_part(atom_to_list(NodeName), Pos);
get_nodename_part(NodeName, Pos) when is_list(NodeName) ->
	Tokens = string:tokens(NodeName, "@"),
	lists:nth(Pos,Tokens).	

%%
%% Local Functions
%%
