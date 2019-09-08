-module('Elixir.CouchDB').

-export([
    server_record/1
    ,server_record/2
]).

server_record(Host) -> couchdb:server_record(Host).
server_record(Host, Options) -> couchdb:server_record(Host, Options).
