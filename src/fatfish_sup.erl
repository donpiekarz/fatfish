-module(fatfish_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link()->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

init(_Args)->
    Children = [
                {fatfish_mta,{gen_smtp_server, start, [fatfish_mta]}, permanent, 5000, worker, dynamic}
    ],
    {ok,
     {
       {one_for_one, 3, 10},
       Children
     }
    }.
