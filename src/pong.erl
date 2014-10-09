-module(pong).

-export([start_link/0, foo/0]).

start_link()->
    {ok, spawn_link(?MODULE, foo, [])}.

foo()->
    io:fwrite("tutaj foo~n", []),
    foo_end().

foo_end()->
    receive
        done ->
            ok
    end.
