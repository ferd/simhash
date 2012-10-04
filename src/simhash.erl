-module(simhash).
-export([hash/1, closest/2, distance/2]).
-compile([native]).

-ifdef(TEST).
-export([test/0, testlog/0, teststruct/0]).
-endif.

%% The shingle size determines how large the sliding
%% window goes over binary data. A size of two is slower
%% but is likely the most accurate we can have.
-define(SHINGLE_SIZE, 2).
%% Each time the same shingle is met more than once in
%% a given bit of text, its weight is increased in the final
%% simhash.
-define(DUP_WEIGHT_ADD,1).

%% Default random hash used by simhash is sha-160
-ifndef(PHASH). -ifndef(MD5). -ifndef(SHA).
-define(SHA, true).
-endif. -endif. -endif.

%% erlang:phash2 is the fastest, but sadly
%% only uses 32 bits, which is not very accurate on
%% larger binary sizes ot hash.
-ifdef(PHASH).
-define(SIZE, 32). % bits
-define(HASH(X), <<(erlang:phash2(X,4294967296)):32>>).
-endif.
%% MD5 is faster and less precise than SHA in tests ran
%% by the author, but it's slower and more precise than
%% PHASH.
-ifdef(MD5).
-define(SIZE, 128). % bits
-define(HASH(X), erlang:md5(X)).
-endif.
%% SHA-160 seemed to give the best result in terms of
%% accuracy. In a few contrived tests, it was
%% the slowest mode.
-ifdef(SHA).
-define(SIZE, 160).
-define(HASH(X), crypto:sha(X)).
-endif.

%%%%%%%%%%%%%%
%%% PUBLIC %%%
%%%%%%%%%%%%%%
-type simhash() :: binary().
-export_type([simhash/0]).

%% Takes any binary and returns a simhash for that data.
-spec hash(binary()) -> simhash().
hash(Bin) ->
    hashed_shingles(Bin, ?SHINGLE_SIZE).

%% Takes a given simhash and returns the closest simhash
%% in a second list, based on their Hamming distance.
-spec closest(simhash(), [simhash(),...]) -> {non_neg_integer(), simhash()}.
closest(Hash, [H|T]) ->
    closest(Hash, hamming(Hash,H), H, T).

%% Takes two simhashes and returns their distance.
-spec distance(simhash(), simhash()) -> non_neg_integer().
distance(X,Y) -> hamming(X,Y).

%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%

%% Returns a set of shingles, hashed according to the algorithm
%% used when compiling the module.
hashed_shingles(Bin, Size) ->
    Hashes = [{W,?HASH(X)} || {W,X} <- shingles(Bin, Size)],
    to_sim(reduce(Hashes, ?SIZE-1)).

%% The vector returned from reduce/2 is taken and flattened
%% by its content -- values greater or equal to 0 end up being 1,
%% and those smaller than 0 end up being 0.
to_sim(HashL) ->
    << <<(case Val >= 0 of
              true -> 1;
              false -> 0
          end):1>> || Val <- HashL >>.

%% Takes individually hashed shingles and flattens them
%% as the numeric simhash.
%% Each N bit hash is treated as an N-vector, which is
%% added bit-per-bit over an empty N-vector. The resulting
%% N-vector can be used to create the sim hash.
reduce(_, -1) -> [];
reduce(L, Size) -> [add(L, Size, 0) | reduce(L, Size-1)].

%% we add it left-to-right through shingles,
%% rather than shingle-by-shingle first.
add([], _, Acc) -> Acc;
add([{W,Bin}|T], Pos, Acc) ->
    <<_:Pos, Bit:1, _Rest/bitstring>> = Bin,
    add(T, Pos,
        case Bit of
            1 -> Acc+W;
            0 -> Acc-W
        end).

%% shingles are built using a sliding window of ?SIZE bytes,
%% moving 1 byte at a time over the data. It might be interesting
%% to move to a bit size instead.
shingles(Bin, Size) ->
    build(shingles(Bin, Size, (byte_size(Bin)-1)-Size, [])).

shingles(Bin, Size, Pos, Acc) when Pos > 0 ->
    <<_:Pos/binary, X:Size/binary, _/binary>> = Bin,
    shingles(Bin, Size, Pos-1, [X|Acc]);
shingles(_, _, _, Acc) -> Acc.

build(Pieces) ->
    build(lists:sort(Pieces), []).

build([], Acc) -> Acc;
build([H|T], [{N,H}|Acc]) -> build(T, [{N+?DUP_WEIGHT_ADD,H}|Acc]);
build([H|T], Acc) -> build(T,[{1,H}|Acc]).

%% Runs over the list of simhashes and returns the best
%% match available.
closest(_, D, Match, []) ->
    {D, Match};
closest(Hash, D, Old, [H|T]) ->
    case hamming(Hash, H) of
        Dist when Dist > D -> closest(Hash, D, Old, T);
        Dist -> closest(Hash, Dist, H, T)
    end.

%% Finds the hamming distance between two different
%% binaries.
hamming(X,Y) ->
    true = bit_size(X) == bit_size(Y),
    hamming(X,Y,bit_size(X)-1,0).

%% Hammign distance between two strings can be calculated
%% by checking each of the characters and incrementing the
%% counter by 1 each time the two values are different.
%% In this case, we do it over individual bits of a binary.
hamming(_,_,-1,Sum) -> Sum;
hamming(X,Y,Pos,Sum) ->
    case {X,Y} of
        {<<_:Pos, Bit:1, _/bitstring>>,
         <<_:Pos, Bit:1, _/bitstring>>} ->
            hamming(X,Y,Pos-1,Sum);
        {_,_} ->
            hamming(X,Y,Pos-1,Sum+1)
    end.

-ifdef(TEST).
%%%%%%%%%%%%%
%%% TESTS %%%
%%%%%%%%%%%%%
%% ad-hoc tests/benchmarks

test() ->
    L = [<<"the cat sat on the mat">>,<<"the cat sat on a mat">>,
          <<"my car is grey">>,<<"we all scream for ice cream">>,
          <<"my heart will go on">>,
          <<"bless this mess with all the might you have">>,
          <<"the fat cat is a rat">>,
          <<"a scream for the supreme cream">>,<<"my car is great">>],
    DB = [{simhash:hash(Str), Str} || Str <- L],
    F = fun(Str) -> {Dist, Hash} = simhash:closest(simhash:hash(Str), [Hash || {Hash, _} <- DB]),
            {Dist, Str, proplists:get_value(Hash, DB)}
    end,
    [F(<<"my fat hat is a rat">>),
      F(<<"unrelated text, entirely!!!">>),
      F(<<"my cart will go long">>),
      F(<<"the crop and top of the cream">>),
      F(<<"I dream of ice cream">>)].

teststruct() ->
    L = [
            [a,b,c,d,e,f]
             ,{{case_clause,{match,[[<<1:20>>],[<<0:3>>]]}},[{adgear_gateway_filters,is_blacklisted,1,[{file,"src/adgear_gateway_filters.erl"},{line,49}]},{context_web,generate_response,2,[{file,"src/context_web.erl"},{line,25}]}]}
             ,{cowboy_http_protocol,request,[{http_response,{1,1},200,<<79,75>>},{state,"<0.945.0>","#Port<0.28861495>",cowboy_tcp_transport,[{'_',[{'_',rtb_handler,{config,cassanderl_dispatch,800,ets,61473,65570}}]}],{rtb_handler,{config,cassanderl_dispatch,800,ets,61473,65570}},0,5,10000,keepalive,<<67,97,99,104,101,45,67,111,110,116,114,111,108,58,32,110,111,45,99,97,99,104,101,44,32,110,111,45,115,116,111,114,101,44,32,109,117,115,116,45,114,101,118,97,108,105,100,97,116,101,44,32,112,114,111,120,121,45,114,101,118,97,108,105,100,97,116,101,13,10,67,111,110,110,101,99,116,105,111,110,58,32,107,101,101,112,45,97,108,105,118,101,13,10,67,111,110,116,101,110,116,45,76,101,110,103,116,104,58,32,57,50,53,13,10,67,111,110,116,101,110,116,45,84,121,112,101,58,32,97,112,112,108,105,99,97,116,105,111,110,47,106,97,118,97,115,99,114,105,112,116,13,10,68,97,116,101,58,32,70,114,105,44,32,49,52,32,83,101,112,32,50,48,49,50,32,48,49,58,48,53,58,48,57,32,71,77,84,13,10,80,51,80,58,32,67,80,61,34,78,79,73,32,79,84,67,32,79,84,80,32,79,85,82,32,78,79,82,34,13,10,80,114,97,103,109,97,58,32,110,111,45,99,97,99,104,101,13,10,83,101,114,118,101,114,58,32,67,111,119,98,111,121,13,10,88,45,83,101,114,118,101,114,58,32,104,48,49,49,13,10,88,45,65,110,116,105,118,105,114,117,115,58,32,97,118,97,115,116,33,32,52,13,10,88,45,65,110,116,105,118,105,114,117,115,45,83,116,97,116,117,115,58,32,67,108,101,97,110,13,10,13,10>>,2}],[{file,[115,114,99,47,99,111,119,98,111,121,95,104,116,116,112,95,112,114,111,116,111,99,111,108,46,101,114,108]},{line,97}]}
             ,{a,
               {b,[],[]},
               {c,{d,{e,[],[]}},
                  {f,[],[]}}}
             ,<<"a b c d">>
             ,self()
        ],
    Format = fun erlang:term_to_binary/1,
    DB = [{simhash:hash(Format(Str)), Str} || Str <- L],
    F = fun(Str) ->
            {Dist, Hash} = simhash:closest(simhash:hash(Format(Str)),
                                           [Hash || {Hash, _} <- DB]),
            {Dist, Str, proplists:get_value(Hash, DB)}
    end,
    [F([e,f,g,h])
    ,F("a b c d e f")
    ,F({'EXIT',{badarith,[{erlang,'/',[1,0],[]},
                          {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,576}]},
                          {erl_eval,expr,5,[{file,"erl_eval.erl"},{line,360}]},
                          {shell,exprs,7,[{file,"shell.erl"},{line,668}]},
                          {shell,eval_exprs,7,[{file,"shell.erl"},{line,623}]},
                          {shell,eval_loop,3,[{file,"shell.erl"},{line,608}]}]}})
    ,F({a,
        {b,[],[]},
        {c,{g,{e,[],[]}},
         {f,[],[]}}})
    ,F(spawn(fun() -> ok end))
    ].
-endif.
