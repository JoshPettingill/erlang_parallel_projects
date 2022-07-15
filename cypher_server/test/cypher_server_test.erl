-module(cypher_server_test).
-include_lib("eunit/include/eunit.hrl").
-import(cypher_server, [start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2]).
-export([]).

setup() -> 
    {ok, MyServer} = start_link(),
    MyServer.

cleanup(MyServer) ->
    gen_server:call(MyServer, stop).

setup_cleanup_test() ->
    MyServer = setup(),
    ok = cleanup(MyServer),
    ?assertExit({noproc,_}, gen_server:call(MyServer, none)).

gen_server_fail_test() ->
    MyServer = setup(),
    {failure, "You didn't call the server correctly"} = gen_server:call(MyServer, fail),
    cleanup(MyServer).

encrypt_nothing_test() ->
    MyServer = setup(),
    "" = gen_server:call(MyServer, {encrypt, ""}),
    cleanup(MyServer).

encrypt_single_char_test() ->
    MyServer = setup(),
    "G" = gen_server:call(MyServer, {encrypt, "A"}),
    cleanup(MyServer).

encrypt_single_word_test() ->
    MyServer = setup(),
    "mgmmotm" = gen_server:call(MyServer, {encrypt, "gagging"}),
    cleanup(MyServer).

encrypt_alphabet_test() ->
    MyServer = setup(),
    Sentence = "abcdefghijklmnopqrstuvwx",
    Ciphered_Sentece = "ghijklmnopqrstuvwxyz{|}\~",
    Ciphered_Sentece = gen_server:call(MyServer, {encrypt, Sentence}),
    cleanup(MyServer).

encrypt_last_letter_in_ASCII_test() -> 
    MyServer = setup(),
    "%" = gen_server:call(MyServer, {encrypt, "\~"}),
    cleanup(MyServer).

encrypt_first_letter_in_ASCII_test() -> 
    MyServer = setup(),
    " " = gen_server:call(MyServer, {encrypt, "y"}),
    cleanup(MyServer).

encrypt_sentence_test() ->
    MyServer = setup(),
    Sentence = "Hello, my name is Josh.",
    Ciphered_Sentence = "Nkrru2&s &tgsk&oy&Puyn4",
    Ciphered_Sentence = gen_server:call(MyServer, {encrypt, Sentence}),
    cleanup(MyServer).

decrypt_nothing_test() ->
    MyServer = setup(),
    "" = gen_server:call(MyServer, {decrypt, ""}),
    cleanup(MyServer).

decrypt_single_char_test() ->
    MyServer = setup(),
    "A" = gen_server:call(MyServer, {decrypt, "G"}),
    cleanup(MyServer).

decrypt_single_word_test() ->
    MyServer = setup(),
    "gagging" = gen_server:call(MyServer, {decrypt, "mgmmotm"}),
    cleanup(MyServer).

decrypt_alphabet_test() ->
    MyServer = setup(),
    Sentence = "abcdefghijklmnopqrstuvwx",
    Ciphered_Sentence = "ghijklmnopqrstuvwxyz{|}\~",
    Sentence = gen_server:call(MyServer, {decrypt, Ciphered_Sentence}),
    cleanup(MyServer).

decrypt_last_letter_in_ASCII_test() -> 
    MyServer = setup(),
    "\~" = gen_server:call(MyServer, {decrypt, "%"}),
    cleanup(MyServer).

decrypt_first_letter_in_ASCII_test() -> 
    MyServer = setup(),
    " " = gen_server:call(MyServer, {decrypt, "&"}),
    cleanup(MyServer).

decrypt_sentence_test() ->
    MyServer = setup(),
    Sentence = "Hello, my name is Josh.",
    Ciphered_Sentence = "Nkrru2&s &tgsk&oy&Puyn4",
    Sentence = gen_server:call(MyServer, {decrypt, Ciphered_Sentence}),
    cleanup(MyServer).




