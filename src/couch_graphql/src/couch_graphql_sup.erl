-module(couch_graphql_sup).
-behaviour(supervisor).

-export([init/1]).
-export([start_link/1]).


start_link(Args) ->
    io:format('leo3~n'),
    supervisor:start_link({local,?MODULE}, ?MODULE, Args).

init([]) ->
    io:format('leo4~n'),
    {ok, {{one_for_one, 3, 10}, couch_epi:register_service(couch_graphql_epi, [])}}.
