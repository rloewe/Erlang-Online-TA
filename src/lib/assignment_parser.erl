-module(assignment_parser).

-export([parse_assignment/1]).

parse_assignment(ConfigFile) ->
    ConfigFile.