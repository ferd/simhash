Simhash
=======

This module implements simhashing in Erlang.

While hash functions such as MD5 or SHA try to get unique value
for unique pieces of data, there is no way for them to represent
how similar they are -- it's not a design concern for these functions,
and in fact, it is something they usually want to avoid.
Similarly for cryptographic hash functions like bcrypt or scrypt.

Simhashing, on the otherhand, tries to provide a signature for some
piece of data while allowing different signatures to be similar when
the data they hash is similar.

Simhashes are then useful in order to figure out duplicates or near-
duplicates between different pieces of data by being able to find
the distance between two given hashes.

For more resources on simhashing, you may read the following:

- http://matpalm.com/resemblance/simhash/
- http://www.cs.princeton.edu/courses/archive/spring04/cos598B/bib/CharikarEstim.pdf
- http://irl.cs.tamu.edu/people/sadhan/papers/cikm2011.pdf

How To Build
------------
The module can be compiled using `./rebar compile` or `make`.

By default, the simhash library will use MD5 as the function to
hash the shingles made from the binary structure. It is the second most
accurate one, but also the second slowest one, striking a decent balance.

By passing macros, other hashing algorithms can be used:
- `PHASH` for Erlang's `phash2` (32 bits, fastest, least accurate)
- `MD5` for MD5 (default) (128 bits, slower, more accurate)
- `SHA` for SHA-160 (slowest, most accurate).

If you want to use SHA-160 or phash2 hashing by default, it is recommended you
provide the macros in your own `rebar` config or whatever other
tool that lets you declare them when compiling (`{d,'SHA'}` for
example).

To run tests, call `make test`.

How To Use It
-------------

To hash a binary:

    1> VoiceHash = simhash:hash(<<"My voice is my password.">>).
    <<194,5,119,237,104,38,63,181,151,39,73,226,19,230,140,89,
      33,12,178,125>>

To hash any other Erlang term:

    2> PidHash = simhash:hash(term_to_binary(self())).
    <<128,255,187,43,142,160,234,204,110,124,209,236,156,227,
      43,35,236,151,89,57>>

You can then find the distance between these as follows:

    3> simhash:distance(VoiceHash, PidHash).
    86
    4> simhash:distance(simhash:hash(<<"My voice is my passport.">>), VoiceHash).
    27

This value is somewhat arbitrary, and can be more useful when you want
to compare more than two elements to find the closest match:

    5> DB = [{simhash:hash(Txt), Txt}
    5>      || Txt <- [term_to_binary([a,b,c,d,e,f]),
    5>                 <<"a b c d e f">>, term_to_binary("a b c d e f"),
    5>                 <<"My voice is my password.">>]].
    ...
    6> {Distance1, Hash1} = simhash:closest(
    6>      simhash:hash(<<"My voice is my passport.">>),
    6>      [Hash || {Hash,_Txt} <- DB]).
    ...
    7> {Distance1, proplists:get_value(Hash1, DB)}.
    {27, <<"My voice is my password.">>}
    
    7> {Distance2, Hash2} = simhash:closest(
    7>      simhash:hash(<<"d e f g h i">>),
    7>      [Hash || {Hash,_Txt} <- DB]).
    ...
    8> {Distance2, proplists:get_value(Hash2, DB)}.
    {62, <<"a b c d e f">>}
    
    8> {Distance3, Hash3} = simhash:closest(
    8>      simhash:hash(term_to_binary({a,b,c,d,e,f})),
    8>      [Hash || {Hash,_Txt} <- DB]).
    ...
    9> {Distance3, binary_to_term(proplists:get_value(Hash1, DB))}.
    {22, [a,b,c,d,e,f]}

What you consider to be an acceptable treshold for distance in order
to consider two structures as near-duplicates or duplicates is highly
dependent on the kind (and size) of data you have and the hashing
algorithm chosen when compiling.

If the default shingling mechanism isn't what you need (and it is
unlikely to be with larger data sets or with particular vocabularies
you want to sort by frequency), you can also pass in your own
weighed features, so that some items are worth more than others:

    10> simhash:distance(
    10>   simhash:hash([{1,<<"my">>},{1,<<"car">>}, {1,<<"is">>}, {1,<<"black">>}]),
    10>   simhash:hash([{1,<<"my">>},{1,<<"car">>}, {1,<<"is">>}, {1,<<"blue">>}])).
    6
    11> simhash:distance(
    11>   simhash:hash([{1,<<"my">>},{1,<<"car">>}, {1,<<"is">>}, {5,<<"blue">>}]),
    11>   simhash:hash([{1,<<"my">>},{1,<<"car">>}, {1,<<"is">>}, {5,<<"black">>}])).
    17

In the tests above, you can see that by giving more weigh to the color, it's possible to make the simhash behave differently to the same original string.

Finally, it is possible to use the simhash library with your own hash function if you wish to do so. The hash function must accept a binary and return a binary. You will also need to provide an argument explaining how many bits is contained in your hashes:

    12> F = fun(X) -> crypto:hash_final(crypto:hash_update(crypto:hash_init(sha512), X)) end.
    #Fun<erl_eval.6.82930912>
    13> F(<<"abc">>).
    <<221,175,53,161,147,...>>

The function `F` defines a simple way to call sha512 hashes from the crypto module. It can be used with simhashes as follows:

    14> simhash:hash(<<"abcdef">>, F, 512).
    <<60,149,116,223,113,...>>
    15> simhash:hash([{5,<<"ab">>},{2, <<"cdef">>}], F, 512).
    <<180,232,215,0,38,245,...>>

Notes
-----

As of now, this library is rather experimental and hasn't made it
to production anywhere else. Handle with caution.

Changelog
---------

### 0.3.0: ###
- MD5 is the default simhashing algorithm, for the accuracy/speed balance
- Added a way to customize the hashing algorithm at run time.
- Common Test tests!

### 0.2.0: ###
- Adding a way to submit a user's own features/shingles with weight.
