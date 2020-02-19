-module(couch_graphql_httpd).
-include_lib("couch/include/couch_db.hrl").

-export([handle_req/2]).


handle_req(#httpd{} = Req, Db0) ->
    Db = set_user_ctx(Req, Db0),
    handle_req_int(Req, Db).


handle_req_int(#httpd{path_parts=[_, <<"_hello">> | _]} = Req, Db) ->
    handle_index_req(Req, Db);
handle_req_int(_, _) ->
    throw({not_found, missing}).


handle_index_req(#httpd{method='GET', path_parts=[_, _]}=Req, _Db) ->
	chttpd:send_json(Req, {[{hello, 42}]}).


set_user_ctx(#httpd{user_ctx=Ctx}, Db) ->
    {ok, NewDb} = couch_db:set_user_ctx(Db, Ctx),
    NewDb.
