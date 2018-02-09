-module(simhash_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([init_per_suite/1, end_per_suite/1, all/0]).

-export([closer_type/1, commutative_distance/1, id_has_0_distance/1,
         custom_features/1, custom_hash/1]).
all() -> [closer_type, commutative_distance, id_has_0_distance,
          custom_features, custom_hash].

init_per_suite(Config) ->
    Bytes = crypto:strong_rand_bytes(32),
    case {simhash:hash(Bytes), simhash:hash(Bytes, fun erlang:md5/1, 128)} of
        {X,X} -> Config;
        {_,_} -> {skip, "Tests assume the default hash algorithm is MD5"}
    end.

end_per_suite(_Config) ->
    ok.

%% fortunately, similar strings end up closer than binary
%% versions of unrelated data types.
closer_type(_Config) ->
    Voice1 = simhash:hash(<<"My voice is my password.">>),
    Voice2 = simhash:hash(<<"My voice is my passport.">>),
    Pid = simhash:hash(term_to_binary(self())),
    true = simhash:distance(Voice1, Pid) > simhash:distance(Voice2, Pid).

%% The distance between hash should be commutative.
commutative_distance(_Config) ->
    V1 = simhash:hash(<<"My voice is my password.">>),
    V2 = simhash:hash(<<"My voice is my passport.">>),
    true = simhash:distance(V1, V2) == simhash:distance(V2, V1).

%% Identical items should have identical simhashes
id_has_0_distance(_Config) ->
    Hash = simhash:hash(<<"My voice is my password.">>),
    0 = simhash:distance(Hash, Hash).

%% Features are lists of weighed binaries to be hashed according
%% to such weight. When simhash:hash/1 doesn't receive a binary,
%% we assume it's a feature list.
custom_features(_Config) ->
    H1 = simhash:hash([{1,<<"my">>}, {1,<<"car">>}, {1,<<"is">>},
                       {1,<<"black">>}]),
    H2 = simhash:hash([{1,<<"my">>}, {1,<<"car">>}, {1,<<"is">>},
                       {1,<<"blue">>}]),
    H3 = simhash:hash([{1,<<"my">>}, {1,<<"car">>}, {1,<<"is">>},
                       {5,<<"blue">>}]),
    H4 = simhash:hash([{1,<<"my">>}, {1,<<"car">>}, {1,<<"is">>},
                       {5,<<"black">>}]),
    H5 = simhash:hash([{1,<<"my">>}, {1,<<"car">>}, {1,<<"is">>},
                       {0,<<"blue">>}]),
    H6 = simhash:hash([{1,<<"my">>}, {1,<<"car">>}, {1,<<"is">>},
                       {0,<<"black">>}]),
    true = simhash:distance(H1, H2) =/= simhash:distance(H3, H4),
    0 = simhash:distance(H5, H6).

custom_hash(_Config) ->
    Term1 = [{1,<<"my">>}, {1,<<"car">>}, {1,<<"is">>}, {5,<<"black">>}],
    Term2 = <<"some binary string">>,
    Sha512 = fun(X) ->
         crypto:hash_final(crypto:hash_update(crypto:hash_init(sha512), X))
    end,
    Phash2 = fun(X) -> <<(erlang:phash2(X,4294967296)):32>> end,
    true = simhash:hash(Term1, Sha512, 512) =/= simhash:hash(Term1),
    true = simhash:hash(Term2, Sha512, 512) =/= simhash:hash(Term2),
    true = simhash:hash(Term1, Phash2, 32) =/= simhash:hash(Term1),
    true = simhash:hash(Term2, Phash2, 32) =/= simhash:hash(Term2),
    true = simhash:hash(Term2, Phash2, 32) =:= simhash:hash(Term2, Phash2, 32),
    true = simhash:hash(Term1, fun erlang:md5/1, 128) =:= simhash:hash(Term1),
    true = simhash:hash(Term2, fun erlang:md5/1, 128) =:= simhash:hash(Term2).

