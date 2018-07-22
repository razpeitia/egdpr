-module(gdpr).
-author("Ricardo Azpeitia Pimentel").

-export([
    new/1
]).

-spec new(Params :: #{} | binary() | string()) -> ok.
new(_Params) ->
    ok.