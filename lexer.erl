-file("/usr/lib/erlang/lib/parsetools-2.2/include/leexinc.hrl", 0).
%% The source of this file is part of leex distribution, as such it
%% has the same Copyright as the other files in the leex
%% distribution. The Copyright is defined in the accompanying file
%% COPYRIGHT. However, the resultant scanner generated by leex is the
%% property of the creator of the scanner and is not covered by that
%% Copyright.

-module(lexer).

-export([string/1,string/2,token/2,token/3,tokens/2,tokens/3]).
-export([format_error/1]).

%% User code. This is placed here to allow extra attributes.
-file("./lexer.xrl", 109).
-export([reserved_word/1]).

% Tipos de variable
reserved_word('int')-> green;
reserved_word('float')-> green;
reserved_word('double')-> green;
reserved_word('bool')-> green;
reserved_word('char')-> green;
reserved_word('string')-> green;
reserved_word('true')-> green;
reserved_word('false')-> green;

% Variables
reserved_word('void')-> blond;
reserved_word('const')-> blond;
reserved_word('static')-> blond;
reserved_word('struct')-> blond;
reserved_word('enum')-> blond;
reserved_word('array')-> blond;

% Extensión de memoria para variables
reserved_word('short')-> persian_green;
reserved_word('long')-> persian_green;
reserved_word('signed')-> persian_green;
reserved_word('unsigned')-> persian_green;

% Condicionales
reserved_word('if')-> pink;
reserved_word('else')-> pink;
reserved_word('switch')-> pink;
reserved_word('case')-> pink;
reserved_word('continue')-> pink;
reserved_word('break')-> pink;
reserved_word('default')-> pink;

% Ciclos
reserved_word('for')-> purple;
reserved_word('auto')-> purple;
reserved_word('while')-> purple;
reserved_word('do')-> purple;

% Palabras reservadas
reserved_word('include')-> yellow;
reserved_word('define')-> yellow;
reserved_word('main')-> yellow;
reserved_word('goto')-> yellow;
reserved_word('sizeof')-> yellow;
reserved_word('return')-> yellow;
reserved_word('exit')-> yellow;

% IDS/Variables
reserved_word(_)-> darkblue.
-file("/usr/lib/erlang/lib/parsetools-2.2/include/leexinc.hrl", 14).

format_error({illegal,S}) -> ["illegal characters ",io_lib:write_string(S)];
format_error({user,S}) -> S.

string(String) -> string(String, 1).

string(String, Line) -> string(String, Line, String, []).

%% string(InChars, Line, TokenChars, Tokens) ->
%% {ok,Tokens,Line} | {error,ErrorInfo,Line}.
%% Note the line number going into yystate, L0, is line of token
%% start while line number returned is line of token end. We want line
%% of token start.

string([], L, [], Ts) ->                     % No partial tokens!
    {ok,yyrev(Ts),L};
string(Ics0, L0, Tcs, Ts) ->
    case yystate(yystate(), Ics0, L0, 0, reject, 0) of
        {A,Alen,Ics1,L1} ->                  % Accepting end state
            string_cont(Ics1, L1, yyaction(A, Alen, Tcs, L0), Ts);
        {A,Alen,Ics1,L1,_S1} ->              % Accepting transistion state
            string_cont(Ics1, L1, yyaction(A, Alen, Tcs, L0), Ts);
        {reject,_Alen,Tlen,_Ics1,L1,_S1} ->  % After a non-accepting state
            {error,{L0,?MODULE,{illegal,yypre(Tcs, Tlen+1)}},L1};
        {A,Alen,Tlen,_Ics1,L1,_S1} ->
            Tcs1 = yysuf(Tcs, Alen),
            L2 = adjust_line(Tlen, Alen, Tcs1, L1),
            string_cont(Tcs1, L2, yyaction(A, Alen, Tcs, L0), Ts)
    end.

%% string_cont(RestChars, Line, Token, Tokens)
%% Test for and remove the end token wrapper. Push back characters
%% are prepended to RestChars.

-dialyzer({nowarn_function, string_cont/4}).

string_cont(Rest, Line, {token,T}, Ts) ->
    string(Rest, Line, Rest, [T|Ts]);
string_cont(Rest, Line, {token,T,Push}, Ts) ->
    NewRest = Push ++ Rest,
    string(NewRest, Line, NewRest, [T|Ts]);
string_cont(Rest, Line, {end_token,T}, Ts) ->
    string(Rest, Line, Rest, [T|Ts]);
string_cont(Rest, Line, {end_token,T,Push}, Ts) ->
    NewRest = Push ++ Rest,
    string(NewRest, Line, NewRest, [T|Ts]);
string_cont(Rest, Line, skip_token, Ts) ->
    string(Rest, Line, Rest, Ts);
string_cont(Rest, Line, {skip_token,Push}, Ts) ->
    NewRest = Push ++ Rest,
    string(NewRest, Line, NewRest, Ts);
string_cont(_Rest, Line, {error,S}, _Ts) ->
    {error,{Line,?MODULE,{user,S}},Line}.

%% token(Continuation, Chars) ->
%% token(Continuation, Chars, Line) ->
%% {more,Continuation} | {done,ReturnVal,RestChars}.
%% Must be careful when re-entering to append the latest characters to the
%% after characters in an accept. The continuation is:
%% {token,State,CurrLine,TokenChars,TokenLen,TokenLine,AccAction,AccLen}

token(Cont, Chars) -> token(Cont, Chars, 1).

token([], Chars, Line) ->
    token(yystate(), Chars, Line, Chars, 0, Line, reject, 0);
token({token,State,Line,Tcs,Tlen,Tline,Action,Alen}, Chars, _) ->
    token(State, Chars, Line, Tcs ++ Chars, Tlen, Tline, Action, Alen).

%% token(State, InChars, Line, TokenChars, TokenLen, TokenLine,
%% AcceptAction, AcceptLen) ->
%% {more,Continuation} | {done,ReturnVal,RestChars}.
%% The argument order is chosen to be more efficient.

token(S0, Ics0, L0, Tcs, Tlen0, Tline, A0, Alen0) ->
    case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
        %% Accepting end state, we have a token.
        {A1,Alen1,Ics1,L1} ->
            token_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline));
        %% Accepting transition state, can take more chars.
        {A1,Alen1,[],L1,S1} ->                  % Need more chars to check
            {more,{token,S1,L1,Tcs,Alen1,Tline,A1,Alen1}};
        {A1,Alen1,Ics1,L1,_S1} ->               % Take what we got
            token_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline));
        %% After a non-accepting state, maybe reach accept state later.
        {A1,Alen1,Tlen1,[],L1,S1} ->            % Need more chars to check
            {more,{token,S1,L1,Tcs,Tlen1,Tline,A1,Alen1}};
        {reject,_Alen1,Tlen1,eof,L1,_S1} ->     % No token match
            %% Check for partial token which is error.
            Ret = if Tlen1 > 0 -> {error,{Tline,?MODULE,
                                          %% Skip eof tail in Tcs.
                                          {illegal,yypre(Tcs, Tlen1)}},L1};
                     true -> {eof,L1}
                  end,
            {done,Ret,eof};
        {reject,_Alen1,Tlen1,Ics1,L1,_S1} ->    % No token match
            Error = {Tline,?MODULE,{illegal,yypre(Tcs, Tlen1+1)}},
            {done,{error,Error,L1},Ics1};
        {A1,Alen1,Tlen1,_Ics1,L1,_S1} ->       % Use last accept match
            Tcs1 = yysuf(Tcs, Alen1),
            L2 = adjust_line(Tlen1, Alen1, Tcs1, L1),
            token_cont(Tcs1, L2, yyaction(A1, Alen1, Tcs, Tline))
    end.

%% token_cont(RestChars, Line, Token)
%% If we have a token or error then return done, else if we have a
%% skip_token then continue.

-dialyzer({nowarn_function, token_cont/3}).

token_cont(Rest, Line, {token,T}) ->
    {done,{ok,T,Line},Rest};
token_cont(Rest, Line, {token,T,Push}) ->
    NewRest = Push ++ Rest,
    {done,{ok,T,Line},NewRest};
token_cont(Rest, Line, {end_token,T}) ->
    {done,{ok,T,Line},Rest};
token_cont(Rest, Line, {end_token,T,Push}) ->
    NewRest = Push ++ Rest,
    {done,{ok,T,Line},NewRest};
token_cont(Rest, Line, skip_token) ->
    token(yystate(), Rest, Line, Rest, 0, Line, reject, 0);
token_cont(Rest, Line, {skip_token,Push}) ->
    NewRest = Push ++ Rest,
    token(yystate(), NewRest, Line, NewRest, 0, Line, reject, 0);
token_cont(Rest, Line, {error,S}) ->
    {done,{error,{Line,?MODULE,{user,S}},Line},Rest}.

%% tokens(Continuation, Chars, Line) ->
%% {more,Continuation} | {done,ReturnVal,RestChars}.
%% Must be careful when re-entering to append the latest characters to the
%% after characters in an accept. The continuation is:
%% {tokens,State,CurrLine,TokenChars,TokenLen,TokenLine,Tokens,AccAction,AccLen}
%% {skip_tokens,State,CurrLine,TokenChars,TokenLen,TokenLine,Error,AccAction,AccLen}

tokens(Cont, Chars) -> tokens(Cont, Chars, 1).

tokens([], Chars, Line) ->
    tokens(yystate(), Chars, Line, Chars, 0, Line, [], reject, 0);
tokens({tokens,State,Line,Tcs,Tlen,Tline,Ts,Action,Alen}, Chars, _) ->
    tokens(State, Chars, Line, Tcs ++ Chars, Tlen, Tline, Ts, Action, Alen);
tokens({skip_tokens,State,Line,Tcs,Tlen,Tline,Error,Action,Alen}, Chars, _) ->
    skip_tokens(State, Chars, Line, Tcs ++ Chars, Tlen, Tline, Error, Action, Alen).

%% tokens(State, InChars, Line, TokenChars, TokenLen, TokenLine, Tokens,
%% AcceptAction, AcceptLen) ->
%% {more,Continuation} | {done,ReturnVal,RestChars}.

tokens(S0, Ics0, L0, Tcs, Tlen0, Tline, Ts, A0, Alen0) ->
    case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
        %% Accepting end state, we have a token.
        {A1,Alen1,Ics1,L1} ->
            tokens_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline), Ts);
        %% Accepting transition state, can take more chars.
        {A1,Alen1,[],L1,S1} ->                  % Need more chars to check
            {more,{tokens,S1,L1,Tcs,Alen1,Tline,Ts,A1,Alen1}};
        {A1,Alen1,Ics1,L1,_S1} ->               % Take what we got
            tokens_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline), Ts);
        %% After a non-accepting state, maybe reach accept state later.
        {A1,Alen1,Tlen1,[],L1,S1} ->            % Need more chars to check
            {more,{tokens,S1,L1,Tcs,Tlen1,Tline,Ts,A1,Alen1}};
        {reject,_Alen1,Tlen1,eof,L1,_S1} ->     % No token match
            %% Check for partial token which is error, no need to skip here.
            Ret = if Tlen1 > 0 -> {error,{Tline,?MODULE,
                                          %% Skip eof tail in Tcs.
                                          {illegal,yypre(Tcs, Tlen1)}},L1};
                     Ts == [] -> {eof,L1};
                     true -> {ok,yyrev(Ts),L1}
                  end,
            {done,Ret,eof};
        {reject,_Alen1,Tlen1,_Ics1,L1,_S1} ->
            %% Skip rest of tokens.
            Error = {L1,?MODULE,{illegal,yypre(Tcs, Tlen1+1)}},
            skip_tokens(yysuf(Tcs, Tlen1+1), L1, Error);
        {A1,Alen1,Tlen1,_Ics1,L1,_S1} ->
            Token = yyaction(A1, Alen1, Tcs, Tline),
            Tcs1 = yysuf(Tcs, Alen1),
            L2 = adjust_line(Tlen1, Alen1, Tcs1, L1),
            tokens_cont(Tcs1, L2, Token, Ts)
    end.

%% tokens_cont(RestChars, Line, Token, Tokens)
%% If we have an end_token or error then return done, else if we have
%% a token then save it and continue, else if we have a skip_token
%% just continue.

-dialyzer({nowarn_function, tokens_cont/4}).

tokens_cont(Rest, Line, {token,T}, Ts) ->
    tokens(yystate(), Rest, Line, Rest, 0, Line, [T|Ts], reject, 0);
tokens_cont(Rest, Line, {token,T,Push}, Ts) ->
    NewRest = Push ++ Rest,
    tokens(yystate(), NewRest, Line, NewRest, 0, Line, [T|Ts], reject, 0);
tokens_cont(Rest, Line, {end_token,T}, Ts) ->
    {done,{ok,yyrev(Ts, [T]),Line},Rest};
tokens_cont(Rest, Line, {end_token,T,Push}, Ts) ->
    NewRest = Push ++ Rest,
    {done,{ok,yyrev(Ts, [T]),Line},NewRest};
tokens_cont(Rest, Line, skip_token, Ts) ->
    tokens(yystate(), Rest, Line, Rest, 0, Line, Ts, reject, 0);
tokens_cont(Rest, Line, {skip_token,Push}, Ts) ->
    NewRest = Push ++ Rest,
    tokens(yystate(), NewRest, Line, NewRest, 0, Line, Ts, reject, 0);
tokens_cont(Rest, Line, {error,S}, _Ts) ->
    skip_tokens(Rest, Line, {Line,?MODULE,{user,S}}).

%%skip_tokens(InChars, Line, Error) -> {done,{error,Error,Line},Ics}.
%% Skip tokens until an end token, junk everything and return the error.

skip_tokens(Ics, Line, Error) ->
    skip_tokens(yystate(), Ics, Line, Ics, 0, Line, Error, reject, 0).

%% skip_tokens(State, InChars, Line, TokenChars, TokenLen, TokenLine, Tokens,
%% AcceptAction, AcceptLen) ->
%% {more,Continuation} | {done,ReturnVal,RestChars}.

skip_tokens(S0, Ics0, L0, Tcs, Tlen0, Tline, Error, A0, Alen0) ->
    case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
        {A1,Alen1,Ics1,L1} ->                  % Accepting end state
            skip_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline), Error);
        {A1,Alen1,[],L1,S1} ->                 % After an accepting state
            {more,{skip_tokens,S1,L1,Tcs,Alen1,Tline,Error,A1,Alen1}};
        {A1,Alen1,Ics1,L1,_S1} ->
            skip_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline), Error);
        {A1,Alen1,Tlen1,[],L1,S1} ->           % After a non-accepting state
            {more,{skip_tokens,S1,L1,Tcs,Tlen1,Tline,Error,A1,Alen1}};
        {reject,_Alen1,_Tlen1,eof,L1,_S1} ->
            {done,{error,Error,L1},eof};
        {reject,_Alen1,Tlen1,_Ics1,L1,_S1} ->
            skip_tokens(yysuf(Tcs, Tlen1+1), L1, Error);
        {A1,Alen1,Tlen1,_Ics1,L1,_S1} ->
            Token = yyaction(A1, Alen1, Tcs, Tline),
            Tcs1 = yysuf(Tcs, Alen1),
            L2 = adjust_line(Tlen1, Alen1, Tcs1, L1),
            skip_cont(Tcs1, L2, Token, Error)
    end.

%% skip_cont(RestChars, Line, Token, Error)
%% Skip tokens until we have an end_token or error then return done
%% with the original rror.

-dialyzer({nowarn_function, skip_cont/4}).

skip_cont(Rest, Line, {token,_T}, Error) ->
    skip_tokens(yystate(), Rest, Line, Rest, 0, Line, Error, reject, 0);
skip_cont(Rest, Line, {token,_T,Push}, Error) ->
    NewRest = Push ++ Rest,
    skip_tokens(yystate(), NewRest, Line, NewRest, 0, Line, Error, reject, 0);
skip_cont(Rest, Line, {end_token,_T}, Error) ->
    {done,{error,Error,Line},Rest};
skip_cont(Rest, Line, {end_token,_T,Push}, Error) ->
    NewRest = Push ++ Rest,
    {done,{error,Error,Line},NewRest};
skip_cont(Rest, Line, skip_token, Error) ->
    skip_tokens(yystate(), Rest, Line, Rest, 0, Line, Error, reject, 0);
skip_cont(Rest, Line, {skip_token,Push}, Error) ->
    NewRest = Push ++ Rest,
    skip_tokens(yystate(), NewRest, Line, NewRest, 0, Line, Error, reject, 0);
skip_cont(Rest, Line, {error,_S}, Error) ->
    skip_tokens(yystate(), Rest, Line, Rest, 0, Line, Error, reject, 0).

-compile({nowarn_unused_function, [yyrev/1, yyrev/2, yypre/2, yysuf/2]}).

yyrev(List) -> lists:reverse(List).
yyrev(List, Tail) -> lists:reverse(List, Tail).
yypre(List, N) -> lists:sublist(List, N).
yysuf(List, N) -> lists:nthtail(N, List).

%% adjust_line(TokenLength, AcceptLength, Chars, Line) -> NewLine
%% Make sure that newlines in Chars are not counted twice.
%% Line has been updated with respect to newlines in the prefix of
%% Chars consisting of (TokenLength - AcceptLength) characters.

-compile({nowarn_unused_function, adjust_line/4}).

adjust_line(N, N, _Cs, L) -> L;
adjust_line(T, A, [$\n|Cs], L) ->
    adjust_line(T-1, A, Cs, L-1);
adjust_line(T, A, [_|Cs], L) ->
    adjust_line(T-1, A, Cs, L).

%% yystate() -> InitialState.
%% yystate(State, InChars, Line, CurrTokLen, AcceptAction, AcceptLen) ->
%% {Action, AcceptLen, RestChars, Line} |
%% {Action, AcceptLen, RestChars, Line, State} |
%% {reject, AcceptLen, CurrTokLen, RestChars, Line, State} |
%% {Action, AcceptLen, CurrTokLen, RestChars, Line, State}.
%% Generated state transition functions. The non-accepting end state
%% return signal either an unrecognised character or end of current
%% input.

-file("./lexer.erl", 359).
yystate() -> 14.

yystate(29, [46|Ics], Line, Tlen, _, _) ->
    yystate(13, Ics, Line, Tlen+1, 1, Tlen);
yystate(29, [C|Ics], Line, Tlen, _, _) when C >= 48, C =< 57 ->
    yystate(29, Ics, Line, Tlen+1, 1, Tlen);
yystate(29, Ics, Line, Tlen, _, _) ->
    {1,Tlen,Ics,Line,29};
yystate(28, [95|Ics], Line, Tlen, _, _) ->
    yystate(12, Ics, Line, Tlen+1, 7, Tlen);
yystate(28, [62|Ics], Line, Tlen, _, _) ->
    yystate(7, Ics, Line, Tlen+1, 7, Tlen);
yystate(28, [61|Ics], Line, Tlen, _, _) ->
    yystate(27, Ics, Line, Tlen+1, 7, Tlen);
yystate(28, [46|Ics], Line, Tlen, _, _) ->
    yystate(23, Ics, Line, Tlen+1, 7, Tlen);
yystate(28, [39|Ics], Line, Tlen, _, _) ->
    yystate(12, Ics, Line, Tlen+1, 7, Tlen);
yystate(28, [34|Ics], Line, Tlen, _, _) ->
    yystate(12, Ics, Line, Tlen+1, 7, Tlen);
yystate(28, [C|Ics], Line, Tlen, _, _) when C >= 48, C =< 57 ->
    yystate(12, Ics, Line, Tlen+1, 7, Tlen);
yystate(28, [C|Ics], Line, Tlen, _, _) when C >= 65, C =< 90 ->
    yystate(12, Ics, Line, Tlen+1, 7, Tlen);
yystate(28, [C|Ics], Line, Tlen, _, _) when C >= 97, C =< 122 ->
    yystate(12, Ics, Line, Tlen+1, 7, Tlen);
yystate(28, Ics, Line, Tlen, _, _) ->
    {7,Tlen,Ics,Line,28};
yystate(27, Ics, Line, Tlen, _, _) ->
    {7,Tlen,Ics,Line};
yystate(26, [126|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [125|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [124|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [123|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [95|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [94|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [93|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [91|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [62|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [61|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [60|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [59|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [58|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [47|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [46|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [45|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [44|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [43|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [42|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [41|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [40|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [39|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [38|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [37|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [36|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [35|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [34|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [33|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [C|Ics], Line, Tlen, _, _) when C >= 48, C =< 57 ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [C|Ics], Line, Tlen, _, _) when C >= 65, C =< 90 ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, [C|Ics], Line, Tlen, _, _) when C >= 97, C =< 122 ->
    yystate(26, Ics, Line, Tlen+1, 11, Tlen);
yystate(26, Ics, Line, Tlen, _, _) ->
    {11,Tlen,Ics,Line,26};
yystate(25, Ics, Line, Tlen, _, _) ->
    {15,Tlen,Ics,Line};
yystate(24, [95|Ics], Line, Tlen, _, _) ->
    yystate(8, Ics, Line, Tlen+1, 9, Tlen);
yystate(24, [C|Ics], Line, Tlen, _, _) when C >= 65, C =< 90 ->
    yystate(8, Ics, Line, Tlen+1, 9, Tlen);
yystate(24, [C|Ics], Line, Tlen, _, _) when C >= 97, C =< 122 ->
    yystate(8, Ics, Line, Tlen+1, 9, Tlen);
yystate(24, Ics, Line, Tlen, _, _) ->
    {9,Tlen,Ics,Line,24};
yystate(23, [95|Ics], Line, Tlen, Action, Alen) ->
    yystate(23, Ics, Line, Tlen+1, Action, Alen);
yystate(23, [62|Ics], Line, Tlen, Action, Alen) ->
    yystate(7, Ics, Line, Tlen+1, Action, Alen);
yystate(23, [39|Ics], Line, Tlen, Action, Alen) ->
    yystate(4, Ics, Line, Tlen+1, Action, Alen);
yystate(23, [34|Ics], Line, Tlen, Action, Alen) ->
    yystate(4, Ics, Line, Tlen+1, Action, Alen);
yystate(23, [C|Ics], Line, Tlen, Action, Alen) when C >= 65, C =< 90 ->
    yystate(23, Ics, Line, Tlen+1, Action, Alen);
yystate(23, [C|Ics], Line, Tlen, Action, Alen) when C >= 97, C =< 122 ->
    yystate(23, Ics, Line, Tlen+1, Action, Alen);
yystate(23, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,23};
yystate(22, Ics, Line, Tlen, _, _) ->
    {14,Tlen,Ics,Line};
yystate(21, [95|Ics], Line, Tlen, Action, Alen) ->
    yystate(21, Ics, Line, Tlen+1, Action, Alen);
yystate(21, [39|Ics], Line, Tlen, Action, Alen) ->
    yystate(15, Ics, Line, Tlen+1, Action, Alen);
yystate(21, [34|Ics], Line, Tlen, Action, Alen) ->
    yystate(15, Ics, Line, Tlen+1, Action, Alen);
yystate(21, [C|Ics], Line, Tlen, Action, Alen) when C >= 65, C =< 90 ->
    yystate(21, Ics, Line, Tlen+1, Action, Alen);
yystate(21, [C|Ics], Line, Tlen, Action, Alen) when C >= 97, C =< 122 ->
    yystate(21, Ics, Line, Tlen+1, Action, Alen);
yystate(21, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,21};
yystate(20, Ics, Line, Tlen, _, _) ->
    {10,Tlen,Ics,Line};
yystate(19, [61|Ics], Line, Tlen, _, _) ->
    yystate(27, Ics, Line, Tlen+1, 9, Tlen);
yystate(19, Ics, Line, Tlen, _, _) ->
    {9,Tlen,Ics,Line,19};
yystate(18, [61|Ics], Line, Tlen, _, _) ->
    yystate(20, Ics, Line, Tlen+1, 8, Tlen);
yystate(18, Ics, Line, Tlen, _, _) ->
    {8,Tlen,Ics,Line,18};
yystate(17, [124|Ics], Line, Tlen, _, _) ->
    yystate(27, Ics, Line, Tlen+1, 9, Tlen);
yystate(17, Ics, Line, Tlen, _, _) ->
    {9,Tlen,Ics,Line,17};
yystate(16, [61|Ics], Line, Tlen, _, _) ->
    yystate(27, Ics, Line, Tlen+1, 10, Tlen);
yystate(16, Ics, Line, Tlen, _, _) ->
    {10,Tlen,Ics,Line,16};
yystate(15, Ics, Line, Tlen, _, _) ->
    {3,Tlen,Ics,Line};
yystate(14, [126|Ics], Line, Tlen, Action, Alen) ->
    yystate(1, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [125|Ics], Line, Tlen, Action, Alen) ->
    yystate(1, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [124|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [123|Ics], Line, Tlen, Action, Alen) ->
    yystate(1, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [95|Ics], Line, Tlen, Action, Alen) ->
    yystate(11, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [94|Ics], Line, Tlen, Action, Alen) ->
    yystate(1, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [93|Ics], Line, Tlen, Action, Alen) ->
    yystate(1, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [91|Ics], Line, Tlen, Action, Alen) ->
    yystate(1, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [62|Ics], Line, Tlen, Action, Alen) ->
    yystate(0, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [61|Ics], Line, Tlen, Action, Alen) ->
    yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [60|Ics], Line, Tlen, Action, Alen) ->
    yystate(28, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [59|Ics], Line, Tlen, Action, Alen) ->
    yystate(1, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [58|Ics], Line, Tlen, Action, Alen) ->
    yystate(1, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [47|Ics], Line, Tlen, Action, Alen) ->
    yystate(10, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [46|Ics], Line, Tlen, Action, Alen) ->
    yystate(1, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [45|Ics], Line, Tlen, Action, Alen) ->
    yystate(18, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [44|Ics], Line, Tlen, Action, Alen) ->
    yystate(1, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [43|Ics], Line, Tlen, Action, Alen) ->
    yystate(18, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [42|Ics], Line, Tlen, Action, Alen) ->
    yystate(18, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [41|Ics], Line, Tlen, Action, Alen) ->
    yystate(1, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [40|Ics], Line, Tlen, Action, Alen) ->
    yystate(1, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [39|Ics], Line, Tlen, Action, Alen) ->
    yystate(5, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [38|Ics], Line, Tlen, Action, Alen) ->
    yystate(3, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [37|Ics], Line, Tlen, Action, Alen) ->
    yystate(18, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [36|Ics], Line, Tlen, Action, Alen) ->
    yystate(1, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [35|Ics], Line, Tlen, Action, Alen) ->
    yystate(24, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [34|Ics], Line, Tlen, Action, Alen) ->
    yystate(5, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [33|Ics], Line, Tlen, Action, Alen) ->
    yystate(19, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [32|Ics], Line, Tlen, Action, Alen) ->
    yystate(25, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [13|Ics], Line, Tlen, Action, Alen) ->
    yystate(9, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [10|Ics], Line, Tlen, Action, Alen) ->
    yystate(6, Ics, Line+1, Tlen+1, Action, Alen);
yystate(14, [9|Ics], Line, Tlen, Action, Alen) ->
    yystate(22, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [C|Ics], Line, Tlen, Action, Alen) when C >= 48, C =< 57 ->
    yystate(29, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [C|Ics], Line, Tlen, Action, Alen) when C >= 65, C =< 90 ->
    yystate(11, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [C|Ics], Line, Tlen, Action, Alen) when C >= 97, C =< 122 ->
    yystate(11, Ics, Line, Tlen+1, Action, Alen);
yystate(14, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,14};
yystate(13, [C|Ics], Line, Tlen, Action, Alen) when C >= 48, C =< 57 ->
    yystate(2, Ics, Line, Tlen+1, Action, Alen);
yystate(13, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,13};
yystate(12, [95|Ics], Line, Tlen, Action, Alen) ->
    yystate(12, Ics, Line, Tlen+1, Action, Alen);
yystate(12, [62|Ics], Line, Tlen, Action, Alen) ->
    yystate(7, Ics, Line, Tlen+1, Action, Alen);
yystate(12, [46|Ics], Line, Tlen, Action, Alen) ->
    yystate(23, Ics, Line, Tlen+1, Action, Alen);
yystate(12, [39|Ics], Line, Tlen, Action, Alen) ->
    yystate(4, Ics, Line, Tlen+1, Action, Alen);
yystate(12, [34|Ics], Line, Tlen, Action, Alen) ->
    yystate(4, Ics, Line, Tlen+1, Action, Alen);
yystate(12, [C|Ics], Line, Tlen, Action, Alen) when C >= 48, C =< 57 ->
    yystate(12, Ics, Line, Tlen+1, Action, Alen);
yystate(12, [C|Ics], Line, Tlen, Action, Alen) when C >= 65, C =< 90 ->
    yystate(12, Ics, Line, Tlen+1, Action, Alen);
yystate(12, [C|Ics], Line, Tlen, Action, Alen) when C >= 97, C =< 122 ->
    yystate(12, Ics, Line, Tlen+1, Action, Alen);
yystate(12, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,12};
yystate(11, [95|Ics], Line, Tlen, _, _) ->
    yystate(11, Ics, Line, Tlen+1, 0, Tlen);
yystate(11, [C|Ics], Line, Tlen, _, _) when C >= 48, C =< 57 ->
    yystate(11, Ics, Line, Tlen+1, 0, Tlen);
yystate(11, [C|Ics], Line, Tlen, _, _) when C >= 65, C =< 90 ->
    yystate(11, Ics, Line, Tlen+1, 0, Tlen);
yystate(11, [C|Ics], Line, Tlen, _, _) when C >= 97, C =< 122 ->
    yystate(11, Ics, Line, Tlen+1, 0, Tlen);
yystate(11, Ics, Line, Tlen, _, _) ->
    {0,Tlen,Ics,Line,11};
yystate(10, [61|Ics], Line, Tlen, _, _) ->
    yystate(20, Ics, Line, Tlen+1, 8, Tlen);
yystate(10, [47|Ics], Line, Tlen, _, _) ->
    yystate(26, Ics, Line, Tlen+1, 8, Tlen);
yystate(10, Ics, Line, Tlen, _, _) ->
    {8,Tlen,Ics,Line,10};
yystate(9, Ics, Line, Tlen, _, _) ->
    {13,Tlen,Ics,Line};
yystate(8, [95|Ics], Line, Tlen, _, _) ->
    yystate(8, Ics, Line, Tlen+1, 6, Tlen);
yystate(8, [C|Ics], Line, Tlen, _, _) when C >= 65, C =< 90 ->
    yystate(8, Ics, Line, Tlen+1, 6, Tlen);
yystate(8, [C|Ics], Line, Tlen, _, _) when C >= 97, C =< 122 ->
    yystate(8, Ics, Line, Tlen+1, 6, Tlen);
yystate(8, Ics, Line, Tlen, _, _) ->
    {6,Tlen,Ics,Line,8};
yystate(7, Ics, Line, Tlen, _, _) ->
    {5,Tlen,Ics,Line};
yystate(6, Ics, Line, Tlen, _, _) ->
    {12,Tlen,Ics,Line};
yystate(5, [95|Ics], Line, Tlen, _, _) ->
    yystate(21, Ics, Line, Tlen+1, 9, Tlen);
yystate(5, [39|Ics], Line, Tlen, _, _) ->
    yystate(15, Ics, Line, Tlen+1, 9, Tlen);
yystate(5, [34|Ics], Line, Tlen, _, _) ->
    yystate(15, Ics, Line, Tlen+1, 9, Tlen);
yystate(5, [C|Ics], Line, Tlen, _, _) when C >= 65, C =< 90 ->
    yystate(21, Ics, Line, Tlen+1, 9, Tlen);
yystate(5, [C|Ics], Line, Tlen, _, _) when C >= 97, C =< 122 ->
    yystate(21, Ics, Line, Tlen+1, 9, Tlen);
yystate(5, Ics, Line, Tlen, _, _) ->
    {9,Tlen,Ics,Line,5};
yystate(4, [62|Ics], Line, Tlen, Action, Alen) ->
    yystate(7, Ics, Line, Tlen+1, Action, Alen);
yystate(4, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,4};
yystate(3, [38|Ics], Line, Tlen, _, _) ->
    yystate(27, Ics, Line, Tlen+1, 9, Tlen);
yystate(3, Ics, Line, Tlen, _, _) ->
    {9,Tlen,Ics,Line,3};
yystate(2, [C|Ics], Line, Tlen, _, _) when C >= 48, C =< 57 ->
    yystate(2, Ics, Line, Tlen+1, 2, Tlen);
yystate(2, Ics, Line, Tlen, _, _) ->
    {2,Tlen,Ics,Line,2};
yystate(1, Ics, Line, Tlen, _, _) ->
    {9,Tlen,Ics,Line};
yystate(0, [61|Ics], Line, Tlen, _, _) ->
    yystate(27, Ics, Line, Tlen+1, 7, Tlen);
yystate(0, Ics, Line, Tlen, _, _) ->
    {7,Tlen,Ics,Line,0};
yystate(S, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,S}.

%% yyaction(Action, TokenLength, TokenChars, TokenLine) ->
%% {token,Token} | {end_token, Token} | skip_token | {error,String}.
%% Generated action function.

yyaction(0, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_0(TokenChars, TokenLine);
yyaction(1, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_1(TokenChars, TokenLine);
yyaction(2, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_2(TokenChars, TokenLine);
yyaction(3, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_3(TokenChars, TokenLine);
yyaction(4, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_4(TokenChars, TokenLine);
yyaction(5, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_5(TokenChars, TokenLine);
yyaction(6, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_6(TokenChars, TokenLine);
yyaction(7, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_7(TokenChars, TokenLine);
yyaction(8, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_8(TokenChars, TokenLine);
yyaction(9, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_9(TokenChars, TokenLine);
yyaction(10, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_10(TokenChars, TokenLine);
yyaction(11, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_11(TokenChars, TokenLine);
yyaction(12, _, _, TokenLine) ->
    yyaction_12(TokenLine);
yyaction(13, _, _, TokenLine) ->
    yyaction_13(TokenLine);
yyaction(14, _, _, TokenLine) ->
    yyaction_14(TokenLine);
yyaction(15, _, _, TokenLine) ->
    yyaction_15(TokenLine);
yyaction(_, _, _, _) -> error.

-compile({inline,yyaction_0/2}).
-file("./lexer.xrl", 57).
yyaction_0(TokenChars, TokenLine) ->
     Atom = list_to_atom (TokenChars),
     { token,
     case reserved_word (Atom) of
     yellow -> { 'RW', "<span class=\"RW\">" ++ TokenChars ++ "</span>", TokenLine } ;
     green -> { 'TYPE_VARIABLES', "<span class=\"TYPE_VARIABLES\">" ++ TokenChars ++ "</span>", TokenLine } ;
     persian_green -> { 'EXT_TYPE_VARIABLES', "<span class=\"EXT_TYPE_VARIABLES\">" ++ TokenChars ++ "</span>", TokenLine } ;
     pink -> { 'CONDITIONALS', "<span class=\"CONDITIONALS\">" ++ TokenChars ++ "</span>", TokenLine } ;
     blond -> { 'VARIABLES', "<span class=\"VARIABLES\">" ++ TokenChars ++ "</span>", TokenLine } ;
     purple -> { 'CICLES', "<span class=\"CICLES\">" ++ TokenChars ++ "</span>", TokenLine } ;
     darkblue -> { 'ID', "<span class=\"ID\">" ++ TokenChars ++ "</span>", TokenLine }
     end } .

-compile({inline,yyaction_1/2}).
-file("./lexer.xrl", 70).
yyaction_1(TokenChars, TokenLine) ->
     { token, { 'INTEGER', "<span class=\"INT\">" ++ TokenChars ++ "</span>", TokenLine } } .

-compile({inline,yyaction_2/2}).
-file("./lexer.xrl", 73).
yyaction_2(TokenChars, TokenLine) ->
     { token, { 'FLOAT', "<span class=\"FLOAT\">" ++ TokenChars ++ "</span>", TokenLine } } .

-compile({inline,yyaction_3/2}).
-file("./lexer.xrl", 76).
yyaction_3(TokenChars, TokenLine) ->
     { token, { 'STRING', "<span class=\"STRING\">" ++ TokenChars ++ "</span>", TokenLine } } .

-compile({inline,yyaction_4/2}).
-file("./lexer.xrl", 79).
yyaction_4(TokenChars, TokenLine) ->
     { token, { 'CHAR', "<span class=\"CHAR\">" ++ TokenChars ++ "</span>", TokenLine } } .

-compile({inline,yyaction_5/2}).
-file("./lexer.xrl", 82).
yyaction_5(TokenChars, TokenLine) ->
     { token, { 'LIBRARY', "<span class=\"LIBRARY\">" ++ TokenChars ++ "</span>", TokenLine } } .

-compile({inline,yyaction_6/2}).
-file("./lexer.xrl", 85).
yyaction_6(TokenChars, TokenLine) ->
     { token, { 'DEFINE', "<span class=\"DEFINE\">" ++ TokenChars ++ "</span>", TokenLine } } .

-compile({inline,yyaction_7/2}).
-file("./lexer.xrl", 88).
yyaction_7(TokenChars, TokenLine) ->
     { token, { 'COMPARATIONS', "<span class=\"COMPARATIONS\">" ++ TokenChars ++ "</span>", TokenLine } } .

-compile({inline,yyaction_8/2}).
-file("./lexer.xrl", 91).
yyaction_8(TokenChars, TokenLine) ->
     { token, { 'MATH_OPERATORS', "<span class=\"MATH_OPERATORS\">" ++ TokenChars ++ "</span>", TokenLine } } .

-compile({inline,yyaction_9/2}).
-file("./lexer.xrl", 94).
yyaction_9(TokenChars, TokenLine) ->
     { token, { 'SPECIALS', "<span class=\"SPECIALS\">" ++ TokenChars ++ "</span>", TokenLine } } .

-compile({inline,yyaction_10/2}).
-file("./lexer.xrl", 97).
yyaction_10(TokenChars, TokenLine) ->
     { token, { 'ASSIGNATION', "<span class=\"ASSIGNATION\">" ++ TokenChars ++ "</span>", TokenLine } } .

-compile({inline,yyaction_11/2}).
-file("./lexer.xrl", 100).
yyaction_11(TokenChars, TokenLine) ->
     { token, { 'COMMENT', "<span class=\"COMMENT\">" ++ TokenChars ++ "</span>", TokenLine } } .

-compile({inline,yyaction_12/1}).
-file("./lexer.xrl", 102).
yyaction_12(TokenLine) ->
     { token, { 'WS', "\n<br />", TokenLine } } .

-compile({inline,yyaction_13/1}).
-file("./lexer.xrl", 103).
yyaction_13(TokenLine) ->
     { token, { 'WS', "\r", TokenLine } } .

-compile({inline,yyaction_14/1}).
-file("./lexer.xrl", 104).
yyaction_14(TokenLine) ->
     { token, { 'WS', "\t&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", TokenLine } } .

-compile({inline,yyaction_15/1}).
-file("./lexer.xrl", 105).
yyaction_15(TokenLine) ->
     { token, { 'WS', "&nbsp;", TokenLine } } .

-file("/usr/lib/erlang/lib/parsetools-2.2/include/leexinc.hrl", 313).