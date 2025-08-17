-module(fasthtml_app).
-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1]).
-export([init/1]). 

%%====================================================================
%% Application Behaviour
%%====================================================================

start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.

%%====================================================================
%% Supervisor Behaviour
%%====================================================================

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    },

    Children = [
        #{
            id => fasthtml_worker,
            start => {fasthtml_worker, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [fasthtml_worker]
        }
    ],

    {ok, {SupFlags, Children}}.
