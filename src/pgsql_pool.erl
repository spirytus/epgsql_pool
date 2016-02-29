-module(pgsql_pool).

-export([start_link/2, start_link/3, stop/1]).
-export([get_connection/1, get_connection/2, return_connection/2]).
-export([connection_info/1, resize_pool/2, change_timeout/2]).

-export([init/1, code_change/3, terminate/2]). 
-export([handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {id, size, connections, monitors, waiting, opts, timer}).

 %% -- client interface --

opts(Opts) ->
    Defaults = [{host, "localhost"},
                {port, 5432},
                {password, ""},
                {username, os:getenv("USER")},
                {database, "not_given"}],
    lists:ukeymerge(1,
                    lists:ukeysort(1, proplists:unfold(Opts)),
                    lists:ukeysort(1, Defaults)).


start_link(Size, Opts) ->
    gen_server:start_link(?MODULE, {undefined, Size, opts(Opts)}, []).

start_link(undefined, Size, Opts) ->
    start_link(Size, Opts);
start_link(Name, Size, Opts) ->
    gen_server:start_link({local, Name}, ?MODULE, {Name, Size, opts(Opts)}, []).

%% @doc Stop the pool, close all db connections
stop(P) ->
    gen_server:cast(P, stop).

%% @doc Get a db connection, wait at most 10 seconds before giving up.
get_connection(P) ->
    get_connection(P, 10000).

%% @doc Get a db connection, wait at most Timeout seconds before giving up.
get_connection(P, Timeout) ->
	try
    	gen_server:call(P, get_connection, Timeout)
	catch 
        exit:{noproc, {gen_server, call, _}} ->
            {error, no_such_pool};
		exit:{timeout, {gen_server, call, _}} ->
            gen_server:cast(P, {cancel_wait, self()}),
            {error, timeout}
	end.

%% @doc Return a db connection back to the connection pool.
return_connection(P, C) ->
    gen_server:cast(P, {return_connection, C}).

connection_info(C) ->
    gen_server:call(C, connection_info).

resize_pool(C, Size) ->
    gen_server:call(C, {resize_pool, Size}).

change_timeout(C, Timeout) ->
    gen_server:call(C, {change_timeout, Timeout}).

%% -- gen_server implementation --

init({Name, Size, Opts}) ->
    process_flag(trap_exit, true),
    Id = case Name of 
			undefined -> self();
			_Name -> Name
		 end,
    {ok, Connection} = connect(Opts),
	{ok, TRef} = timer:send_interval(60000, close_unused),
    State = #state{
      id          = Id,
      size        = Size,
      opts        = Opts,
      connections = [{Connection, now_secs()}],
      monitors    = [],
      waiting     = queue:new(),
      timer       = TRef},
    io:format("Pool ~p ~p ~p ~n", [Name, Size, Opts]),
    {ok, State}.

%% Requestor wants a connection. When available then immediately return, otherwise add to the waiting queue.
handle_call(get_connection, From, #state{connections = Connections, waiting = Waiting} = State) ->
    case Connections of
        [{C,_} | T] -> 
			% Return existing unused connection
			{noreply, deliver(From, C, State#state{connections = T})};
        [] ->
			case length(State#state.monitors) < State#state.size of
				true ->
					% Allocate a new connection and return it.
					{ok, C} = connect(State#state.opts),
				    {noreply, deliver(From, C, State)};
				false ->
					% Reached max connections, let the requestor wait
	 				{noreply, State#state{waiting = queue:in(From, Waiting)}}
			end
    end;

handle_call(connection_info, _From, #state{connections = Connections,
                                           waiting = Waiting,
                                           monitors = Monitors,
                                           size = Size,
                                           opts = Opts} = State) ->
    {reply, [{used, length(Monitors)},
             {available, length(Connections)},
             {waiting, queue:len(Waiting)},
             {size, Size},
             {timeout, case lists:keyfind(timeout, 1, Opts) of
                           false -> 5000;
                           {timeout, Timeout} -> Timeout
                       end}], State};

handle_call({resize_pool, NewSize}, _From, #state{connections = Connections,
                                                  waiting = Waiting,
                                                  monitors = Monitors} = State) ->
    {reply, [{used, length(Monitors)},
             {available, length(Connections)},
             {waiting, queue:len(Waiting)}], State#state{size = NewSize}};


handle_call({change_timeout, NewTimeout}, _From, #state{opts = Opts0} = State) ->
    Opts = case lists:keyfind(timeout, 1, Opts0) of
               false ->
                   [{timeout, NewTimeout} | Opts0];
               _ ->
                   lists:keyreplace(timeout, 1, Opts0, {timeout, NewTimeout})
           end,
    {reply, ok, State#state{opts = Opts}};

%% Trap unsupported calls
handle_call(Request, _From, State) ->
    {stop, {unsupported_call, Request}, State}.

%% Connection returned from the requestor, back into our pool.  Demonitor the requestor.
handle_cast({return_connection, C}, #state{monitors = Monitors} = State) ->
    case lists:keytake(C, 1, Monitors) of
        {value, {C, M}, Monitors2} ->
            erlang:demonitor(M),
            {noreply, return(C, State#state{monitors = Monitors2})};
        false ->
            {noreply, State}
    end;

%% Requestor gave up (timeout), remove from our waiting queue (if any).
handle_cast({cancel_wait, Pid}, #state{waiting = Waiting} = State) ->
    Waiting2 = queue:filter(fun({QPid, _Tag}) -> QPid =/= Pid end, Waiting),
    {noreply, State#state{waiting = Waiting2}};

%% Stop the connections pool.
handle_cast(stop, State) ->
    {stop, normal, State};

%% Trap unsupported casts
handle_cast(Request, State) ->
    {stop, {unsupported_cast, Request}, State}.

%% Close all connections that are unused for longer than a minute.
handle_info(close_unused, State) ->
	Old = now_secs() - 60,
	{Unused, Used} = lists:partition(fun({_C,Time}) -> Time < Old end, State#state.connections),
	[ pgsql:close(C) || {C,_} <- Unused ],
	{noreply, State#state{connections=Used}};

%% Requestor we are monitoring went down. Kill the associated connection, as it might be in an unknown state.
handle_info({'DOWN', M, process, _Pid, _Info}, #state{monitors = Monitors} = State) ->
    case lists:keytake(M, 2, Monitors) of
        {value, {C, M}, Monitors2} ->
			pgsql:close(C),
            {noreply, State#state{monitors = Monitors2}};
        false ->
            {noreply, State}
    end;

%% One of our database connections went down. Clean up our administration.
handle_info({'EXIT', ConnectionPid, _Reason}, State) ->
    #state{connections = Connections, monitors = Monitors} = State,
    Connections2 = proplists:delete(ConnectionPid, Connections),
    F = fun({C, M}) when C == ConnectionPid -> erlang:demonitor(M), false;
           ({_, _}) -> true
        end,
    Monitors2 = lists:filter(F, Monitors),
    {noreply, State#state{connections = Connections2, monitors = Monitors2}};

%% Trap unsupported info calls.
handle_info(Info, State) ->
    {stop, {unsupported_info, Info}, State}.

terminate(_Reason, State) ->
	timer:cancel(State#state.timer),
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.

%% -- internal functions --

connect(Opts) ->
    Host     = proplists:get_value(host, Opts),
    Username = proplists:get_value(username, Opts),
    Password = proplists:get_value(password, Opts),
    pgsql:connect(Host, Username, Password, Opts).

deliver({Pid,_Tag} = From, C, #state{monitors=Monitors} = State) ->
    M = erlang:monitor(process, Pid),
	gen_server:reply(From, {ok, C}),
	State#state{ monitors=[{C, M} | Monitors] }.

return(C, #state{connections = Connections, monitors = Monitors, waiting = Waiting, size = Size} = State) ->
    %%slog:info("POOL: Connection returned: Conns:~p Waiting:~p", [length(Connections), queue:len(Waiting)]),
    case Size =< length(Monitors) of
        true ->
            pgsql:close(C),
            State;
        false ->
            case queue:out(Waiting) of
                {{value, From}, Waiting2} ->
                    %%slog:info("Giving Conn to ~p", [From]),
                    State2 = deliver(From, C, State),
                    State2#state{waiting = Waiting2};
                {empty, _Waiting} ->
                    Connections2 = [{C, now_secs()} | Connections],
                    %%slog:info("No Waiters, adding to available ~p", [length(Connections2)]),
                    State#state{connections = Connections2}
            end
    end.


%% Return the current time in seconds, used for timeouts.
now_secs() ->
    {M,S,_M} = erlang:timestamp(),
    M*1000 + S.
