-module (node_supervisor).
-behaviour (supervisor).

-export ([start_link/1,child_specs/0]).
-export ([init/1]).

start_link(Node) ->
    supervisor:start_link(?MODULE,[Node]).


init([Node]) ->
    ok.