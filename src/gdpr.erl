-module(gdpr).
-author("Ricardo Azpeitia Pimentel").

-record(consent, {
    version
}).

-export([
    new/1
]).

-spec new(Params :: #{} | binary() | string()) -> ok.
new(Params) ->
    ok.

-spec consent_string()
consent_string()