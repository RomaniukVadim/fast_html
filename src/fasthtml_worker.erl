-module(fasthtml_worker).
-behaviour(gen_server).

-moduledoc("A gen_server that manages a port to the fasthtml C worker.").

%%====================================================================
%% Exports
%%====================================================================

%% Public API
-export([start_link/0, decode/1, decode/2, decode_fragment/1, decode_fragment/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% Types & Records
%%====================================================================

-record(state, {port :: port()}).

-type tag() :: binary() | atom().
-type attr() :: {binary(), binary()}.
-type attr_list() :: [attr()].
-type tree() :: {tag(), attr_list(), tree()} | {tag(), attr_list(), nil} | any().

%%====================================================================
%% Public API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec decode(Html :: binary()) -> {ok, tree()} | {error, binary() | atom()}.
decode(Bin) ->
    decode(Bin, []).

-spec decode(Html :: binary(), Opts :: list()) -> {ok, tree()} | {error, binary() | atom()}.
decode(Bin, Opts) ->
    Flags = proplists:get_value(format, Opts, []),
    Timeout = proplists:get_value(timeout, Opts, 10000),
    gen_server:call(?MODULE, {decode, Bin, Flags}, Timeout).

-spec decode_fragment(Html :: binary()) -> {ok, tree()} | {error, term()}.
decode_fragment(Bin) ->
    decode_fragment(Bin, []).

-spec decode_fragment(Html :: binary(), Opts :: list()) -> {ok, tree()} | {error, term()}.
decode_fragment(Bin, Opts) ->
    Flags = proplists:get_value(format, Opts, []),
    Timeout = proplists:get_value(timeout, Opts, 10000),
    Context = proplists:get_value(context, Opts, <<"div">>),
    gen_server:call(?MODULE, {decode_fragment, Bin, Flags, Context}, Timeout).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    Port = open_port(),
    {ok, #state{port = Port}}.

%% This handles calls from the public API functions.
handle_call(TermCommand, _From, State = #state{port = Port}) ->
    Command = term_to_binary(TermCommand),
    erlang:port_command(Port, Command),
    receive
        {Port, {data, Res}} ->
            {reply, binary_to_term(Res), State}
    after 5000 -> % A failsafe timeout
            {reply, {error, timeout}, State}
    end.

%% This handles unexpected messages from the port.
handle_info({Port, {data, Data}}, State = #state{port = Port}) ->
    io:format("~p received unexpected port data: ~p~n", [?MODULE, Data]),
    {noreply, State};
handle_info({'EXIT', Port, Reason}, State = #state{port = Port}) ->
    io:format("~p port terminated with reason: ~p~n", [?MODULE, Reason]),
    {stop, normal, State}.

%% This is called when the process is shutting down. It closes the port.
terminate(_Reason, #state{port = Port}) ->
    erlang:port_close(Port),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
open_port() ->
    PrivDir = code:priv_dir(fast_html),
    Executable = filename:join([PrivDir, "fasthtml_worker"]),
    erlang:open_port({spawn_executable, Executable}, [
        binary,
        {packet, 4},
        use_stdio,
        exit_status
    ]).
