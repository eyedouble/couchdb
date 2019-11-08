# CouchDB

[![Hex pm](https://img.shields.io/hexpm/v/couchdb.svg?style=flat-square&labelColor=5c676d&color=714a94)](https://hex.pm/packages/couchdb)
[![Build Status](https://secure.travis-ci.org/eyedouble/couchdb.svg?branch=master
"Build Status")](https://secure.travis-ci.org/eyedouble/couchdb)
[![License](https://img.shields.io/github/license/eyedouble/couchdb?color=007ec6&style=flat-square)](LICENSE)



**Erlang / Elixir library for [Apache CouchDB](http://couchdb.apache.org) or [IBM Cloudant](https://cloudant.com). CouchDB provides you a full featured and easy-to-use client to access and manage multiple nodes.** 

- [Install from hex.pm](https://hex.pm/packages/couchdb)
- [Documentation available on hexdoc](https://hexdocs.pm/couchdb)

---

## Prerequisites
- Working C/C++ compiler in environment
- Rebar3 or Mix

## Usage

Most of the Erlang/Elixir CouchDB API takes Database or Server record as an argument. This record contains information about the 
server's FQDN, the database name, etc. Parts of the API dealing with 'the server' will take a `server record`, while functions 
performing actions on databases and documents take a `database record`. Consider:

``` erlang
{ok, Server} = couchdb:server_record(<<"http://localhost:5984">>, []).
```

``` elixir
{:ok, server} = :couchdb.server_record("http://localhost:5984", [])
```

After a server or database record has been created one can now make the first actual call:

``` erlang
{ok, Info} = couchdb_server:info(Server).
```

``` elixir
{:ok, info} = :couchdb_server.info(server)
```

## Contribute

For issues, comments or feedback please [create an issue](http://github.com/eyedouble/couchdb/issues).

## Notes
### V2.1.0 JSON library is now Jiffy
Version 2.1.0 switched to Jiffy as the JSON library. This fixes a bug with strings containing UTF-8 characters.
Furthermore it improves performance and brings this CouchDB client closer to Apache CouchDB itself as it uses Jiffy too.

It should be noted that this change means a prerequisite is now introduced, the target environment must have 
a working C/C++ compiler.

### Forked from Couchbeam
**CouchDB** is a a fork of [`couchbeam`](http://github.com/benoitc/couchbeam. Backwards incompatible API changes have been introduced as well as a complete new structure. Therefore the decision was made to release this version under a new name: **CouchDB**.

### Semantic versioning
To comply with [Semantic Versioning](https://semver.org/) the version number has been bumped to `2.0.0`. 

### New uuid implementation
In Beta6 the original Apache CouchDB uuid implementation was added. 
