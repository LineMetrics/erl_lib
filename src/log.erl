%% LineMetrics 2013
-module(log).

-export([put/3, put/2]).

-define(LOG, log:put).

%% 0: nothing
%% 1: important things
%% 2: even more things
%% 3: most things
%% 4: all

-define(LEVEL,4).

put(NeededLevel, Message, Parameter) ->
    if
       NeededLevel =< ?LEVEL ->
	    %%error_logger:info_report([{message, Message}, {parameter, Parameter}]),
	    io:format(("Log: " ++ Message++"~n"), Parameter);
       true -> {}
    end.

put(NeededLevel, Message) ->
    if
       NeededLevel =< ?LEVEL ->
	    io:format(("Log: " ++ Message++"~n"));
       true -> {}
    end.
			   
