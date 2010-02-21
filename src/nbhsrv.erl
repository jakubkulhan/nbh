%% @doc Neighbour nodes search server
%% @author Jakub Kulhan <jakub.kulhan@gmail.com>

-module(nbhsrv).
-behaviour(gen_server).
-export([start_link/2, start_link/3]).
-export([discover/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {sendsock, recvsock, addr, port}).
-record(msg, {sum, msg}).
-record(me, {me}).

%% @doc Starts nbh server
%% @spec (ip_address(), integer()) -> {ok, pid()} | {error, Reason}
%% @equiv start_link(Addr, Port, 1)
start_link(Addr, Port) ->
    start_link(Addr, Port, 1).

%% @doc Starts nbh server
%% @spec (ip_address(), integer(), interger()) -> {ok, pid()} | {error, Reason}
start_link(Addr, Port, TTL) ->
    crypto:start(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Addr, Port, TTL], []).

%% @doc Discovers neighbour nodes
%% @spec () -> term()
discover() ->
    gen_server:call(?MODULE, discover).

%% @doc Initializes nbh server
%% @spec (term()) -> {ok, State}
init([Addr, Port, TTL]) ->
    {ok, RecvSock} = gen_udp:open(Port, [{active, true},
                                         {ip, Addr},
                                         {reuseaddr, true},
                                         {add_membership, {Addr, {0, 0, 0, 0}}}]),

    {ok, SendSock} = gen_udp:open(Port, [{reuseaddr, true},
                                         {multicast_loop, true},
                                         {multicast_ttl, TTL}]),
    {ok, #state{
            sendsock = SendSock,
            recvsock = RecvSock,
            addr = Addr,
            port = Port
        }
    }.

%% @doc Handles discover call
%% @spec (discover, pid(), State) -> {reply, ok, State}
handle_call(discover, _From, State) ->
    sendmsg(State#state.sendsock, State#state.addr, State#state.port,
        #me{me = node()}),
    {reply, ok, State}.

%handle_call(_Request, _From, State) -> {noreply, State}.

%% @doc Handles all casts
%% @spec (any(), State) -> {noreply, State}
handle_cast(_Request, State) -> {noreply, State}.

%% @doc Handles UDP packet reception
%% @spec ({udp, socket(), ip_address(), integer(), list()}, State) -> {noreply, State}
handle_info({udp, _Socket, _RemoteAddr, _RemotePort, Packet}, State) ->
    case parsemsg(list_to_binary(Packet)) of
        {ok, #me{me = Other}} ->
            net_adm:ping(Other);
        _ -> true
    end,
    {noreply, State}.

%handle_info(_Msg, State) -> {noreply, State}.

%% @doc Gen_server behaviour implementation
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% @doc Gen_server behaviour implementation
terminate(_Reason, _State = #state{recvsock = RecvSock, sendsock = SendSock}) ->
    gen_udp:close(RecvSock),
    gen_udp:close(SendSock),
    ok.

%% @doc Sends message over UDP
%% @spec (socket(), ip_address(), integer(), term()) -> ok | {error, Reason}
%%       Reason = not_owner | posix()
sendmsg(Socket, Addr, Port, Term) ->
    gen_udp:send(Socket, Addr, Port, erlang:term_to_binary(#msg{
        sum = crypto:sha_mac(crypto:sha(erlang:term_to_binary(erlang:get_cookie())),
            erlang:term_to_binary(Term)),
        msg = Term
    })).

%% @doc Parse received message
%% @spec (msg()) -> {ok, term()} | {error, Reason}
%%       Reason = badsum
parsemsg(Msg) ->
    #msg{sum = Sum, msg = Term} = erlang:binary_to_term(Msg),
    RealSum = crypto:sha_mac(crypto:sha(erlang:term_to_binary(erlang:get_cookie())),
        erlang:term_to_binary(Term)),
    if
          Sum == RealSum ->
            {ok, Term};
        true ->
            {error, badsum}
    end.
