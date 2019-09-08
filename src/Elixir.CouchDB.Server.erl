-module('Elixir.CouchDB.Server').

-export([
    info/1
]).

info(Server) -> couchdb_server:info(Server).
