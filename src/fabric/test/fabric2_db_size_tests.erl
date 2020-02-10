% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(fabric2_db_size_tests).


-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("fabric2_test.hrl").


-define(DIFF(Db, Change, Fun), begin
        ((fun() ->
            __Before = db_size(Db),
            __Result = Fun(),
            __After = db_size(Db),
            ?debugFmt("~p - ~p == ~p ?= ~p", [
                __After,
                __Before,
                __After - __Before,
                Change
            ]),
            ?assertEqual(Change, __After - __Before),
            __Result
        end)())
    end
).


db_size_test_() ->
    {
        "Test document CRUD operations",
        {
            setup,
            fun setup/0,
            fun cleanup/1,
            with([
                ?TDEF(empty_size),
                ?TDEF(new_doc),
                ?TDEF(edit_doc),
                ?TDEF(del_doc),
                ?TDEF(conflicted_doc),
                ?TDEF(del_winner),
                ?TDEF(del_conflict)
            ])
        }
    }.


setup() ->
    Ctx = test_util:start_couch([fabric]),
    {ok, Db} = fabric2_db:create(?tempdb(), [{user_ctx, ?ADMIN_USER}]),
    {Db, Ctx}.


cleanup({Db, Ctx}) ->
    ok = fabric2_db:delete(fabric2_db:name(Db), []),
    test_util:stop_couch(Ctx).


empty_size({Db, _}) ->
    ?assertEqual(2, db_size(Db)).


new_doc({Db, _}) ->
    % UUID doc id: 32
    % Revision: 2 + 16
    % Deleted: 1
    % Body: {} = 2
    ?DIFF(Db, 53, fun() ->
        create_doc(Db)
    end).


edit_doc({Db, _}) ->
    DocId = fabric2_util:uuid(),
    {ok, RevId1} = ?DIFF(Db, 53, fun() ->
        create_doc(Db, DocId)
    end),
    % {} -> {"foo":"bar"} = 13 - 2
    {ok, RevId2} = ?DIFF(Db, 11, fun() ->
        update_doc(Db, DocId, RevId1, {[{<<"foo">>, <<"bar">>}]})
    end),
    ?DIFF(Db, -11, fun() ->
        update_doc(Db, DocId, RevId2)
    end).


del_doc({Db, _}) ->
    DocId = fabric2_util:uuid(),
    {ok, RevId} = ?DIFF(Db, 64, fun() ->
        create_doc(Db, DocId, {[{<<"foo">>, <<"bar">>}]})
    end),
    % The change here is -11 becuase we're going from
    % {"foo":"bar"} == 13 bytes to {} == 2 bytes.
    % I.e., 2 - 13 == -11
    ?DIFF(Db, -11, fun() ->
        delete_doc(Db, DocId, RevId)
    end).


% need to check both new conflict is new winner
% and that new conflict is not a winner and that
% the sizes don't interfere which should be doable
% with different sized bodies.

conflicted_doc({Db, _}) ->
    DocId = fabric2_util:uuid(),
    {ok, RevId1} = ?DIFF(Db, 64, fun() ->
        create_doc(Db, DocId, {[{<<"foo">>, <<"bar">>}]})
    end),
    ?DIFF(Db, 64, fun() ->
        create_conflict(Db, DocId, RevId1, {[{<<"foo">>, <<"bar">>}]})
    end).


del_winner({Db, _}) ->
    DocId = fabric2_util:uuid(),
    {ok, RevId1} = ?DIFF(Db, 64, fun() ->
        create_doc(Db, DocId, {[{<<"foo">>, <<"bar">>}]})
    end),
    {ok, RevId2} = ?DIFF(Db, 64, fun() ->
        create_conflict(Db, DocId, RevId1, {[{<<"foo">>, <<"bar">>}]})
    end),
    [_ConflictRev, WinnerRev] = lists:sort([RevId1, RevId2]),
    ?DIFF(Db, -11, fun() ->
        {ok, _RevId3} = delete_doc(Db, DocId, WinnerRev),
        ?debugFmt("~n~w~n~w~n~w~n", [RevId1, RevId2, _RevId3])
    end).


del_conflict({Db, _}) ->
    DocId = fabric2_util:uuid(),
    {ok, RevId1} = ?DIFF(Db, 64, fun() ->
        create_doc(Db, DocId, {[{<<"foo">>, <<"bar">>}]})
    end),
    {ok, RevId2} = ?DIFF(Db, 64, fun() ->
        create_conflict(Db, DocId, RevId1, {[{<<"foo">>, <<"bar">>}]})
    end),
    [ConflictRev, _WinnerRev] = lists:sort([RevId1, RevId2]),
    ?DIFF(Db, -11, fun() ->
        {ok, _RevId3} = delete_doc(Db, DocId, ConflictRev),
        ?debugFmt("~n~w~n~w~n~w~n", [RevId1, RevId2, _RevId3])
    end).


% replicate with attachment
% replicate removing attachment
% replicate reusing attachment
% replicate adding attachment with stub
% for each, replicate to winner vs non-winner
% for each, replicate extending winner, vs extending conflict vs new branch



create_doc(Db) ->
    create_doc(Db, fabric2_util:uuid()).


create_doc(Db, DocId) when is_binary(DocId) ->
    create_doc(Db, DocId, {[]});
create_doc(Db, {Props} = Body) when is_list(Props) ->
    create_doc(Db, fabric2_util:uuid(), Body).


create_doc(Db, DocId, Body) ->
    Doc = #doc{
        id = DocId,
        body = Body
    },
    fabric2_db:update_doc(Db, Doc).


create_conflict(Db, DocId, RevId) ->
    create_conflict(Db, DocId, RevId, {[]}).


create_conflict(Db, DocId, RevId, Body) ->
    {Pos, _} = RevId,
    % Only keep the first 16 bytes of the UUID
    % so that we match the normal sized revs
    <<NewRev:16/binary, _/binary>> = fabric2_util:uuid(),
    Doc = #doc{
        id = DocId,
        revs = {Pos, [NewRev]},
        body = Body
    },
    fabric2_db:update_doc(Db, Doc, [replicated_changes]).


update_doc(Db, DocId, RevId) ->
    update_doc(Db, DocId, RevId, {[]}).


update_doc(Db, DocId, {Pos, Rev}, Body) ->
    Doc = #doc{
        id = DocId,
        revs = {Pos, [Rev]},
        body = Body
    },
    fabric2_db:update_doc(Db, Doc).


delete_doc(Db, DocId, RevId) ->
    delete_doc(Db, DocId, RevId, {[]}).


delete_doc(Db, DocId, {Pos, Rev}, Body) ->
    Doc = #doc{
        id = DocId,
        revs = {Pos, [Rev]},
        deleted = true,
        body = Body
    },
    fabric2_db:update_doc(Db, Doc).


db_size(Info) when is_list(Info) ->
    {sizes, {Sizes}} = lists:keyfind(sizes, 1, Info),
    {<<"external">>, External} = lists:keyfind(<<"external">>, 1, Sizes),
    External;
db_size(Db) when is_map(Db) ->
    {ok, Info} = fabric2_db:get_db_info(Db),
    db_size(Info).
