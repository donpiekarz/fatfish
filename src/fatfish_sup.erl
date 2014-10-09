-module(fatfish_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link()->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

init(_Args)->
    Children = [
                {pong,{pong, start_link, []}, permanent, 5000, worker, [pong]}
    ],
    {ok,
     {
       {one_for_one, 3, 10},
       Children
     }
    }.
