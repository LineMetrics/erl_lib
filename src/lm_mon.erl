%% Copyright LineMetrics 2013
-module(lm_mon).
-author("Alexander Minichmair").

-behaviour(gen_server).

%% API
-export([start_link/0, stat/3, start_link/1]).

%% gen_server callbacks
-export([init/1,
   handle_call/3,
   handle_cast/2,
   handle_info/2,
   terminate/2,
   code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket, prefix, target_host, target_port}).

-define(HOST, "localhost").
-define(PORT, 4444).
-define(PORT_OWN, 4465).

-define(SEP, <<":">>).

%% socket options for upd socket
-define(SOCKET_OPTIONS, [binary, {active, false}, {reuseaddr, true}]).

%%%===================================================================
%%% API
%%%===================================================================

stat(Type, TypeId, Value) ->
   gen_server:cast(?SERVER, {stats, Type, TypeId, Value}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
   {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
   {ok, App} =  application:get_application(),
   start_link([?HOST, ?PORT, list_to_binary(atom_to_list(App))]).

start_link([_TargetHost, _TargetPort, _ServiceNameBinary] = Args) ->
   gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
   {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
   {stop, Reason :: term()} | ignore).
init([TargetHost, TargetPort, ServiceName]) when is_binary(ServiceName)->
   Prefix = list_to_binary(util:get_host_name()),
   NewState = #state{target_host = TargetHost, target_port = TargetPort},
   {ok, NewState#state{socket = open(?PORT_OWN), prefix = [Prefix, ?SEP, ServiceName, ?SEP]}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
   {reply, Reply :: term(), NewState :: #state{}} |
   {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
   {noreply, NewState :: #state{}} |
   {noreply, NewState :: #state{}, timeout() | hibernate} |
   {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
   {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
   {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
   {noreply, NewState :: #state{}} |
   {noreply, NewState :: #state{}, timeout() | hibernate} |
   {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({stats, Type, TypeId, Value}=Req, #state{socket= Socket, prefix = Prefix} = State) ->
%%    lager:debug("got stats req: ~p~n",[Req]),
   Message = [Prefix, Type, <<"_">>, TypeId, ?SEP, <<"*">>, ?SEP, Value],
%%    lager:debug("Message will be: ~p~n",[iolist_to_binary(Message)]),
   case gen_udp:send(Socket, State#state.target_host, State#state.target_port, Message) of

      ok                ->    NewState = State;
      {error, closed}   ->    NewSocket = open(?PORT_OWN),
                              gen_udp:send(NewSocket, State#state.target_host, State#state.target_port, Message),
                              NewState = State#state{socket = NewSocket}
   end,
   {noreply, NewState}
;
handle_cast(_Request, State) ->
   {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
   {noreply, NewState :: #state{}} |
   {noreply, NewState :: #state{}, timeout() | hibernate} |
   {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
   {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
   ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
   {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% open an udp
open(Port) ->
   {ok, Socket} = gen_udp:open(Port, ?SOCKET_OPTIONS),
   Socket.