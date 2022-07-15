-module(cypher_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2]).

-define(SERVER, ?MODULE). 

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({encrypt, Plaintext}, _From, State) ->
    Ciphertext = encrypt_cipher(Plaintext),
    {reply, Ciphertext, State};

handle_call({decrypt, Ciphertext}, _From, State) ->
    Plaintext = decrypt_cipher(Ciphertext),
    {reply, Plaintext, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    Reply = {failure, "You didn't call the server correctly"},
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

encrypt_cipher("") ->
    "";
encrypt_cipher(Text) ->
    lists:map(fun (X) -> Y = ((X+6) rem 127),
                        case Y < 32 of
                            true -> Y+32;
                            _ -> Y 
                        end
                    end, Text).

decrypt_cipher(Text) ->
    lists:map(fun (X) -> Y = (X-6),
                        case Y < 32 of
                            true -> Y+95;
                            _ -> Y 
                        end
                    end, Text).
