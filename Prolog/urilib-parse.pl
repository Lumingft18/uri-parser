%  Cristiano Rotunno, Matricola: "914317"
%  Alessandro Rutigliano, Matricola: "909971"

urilib_parse(URIString, Out) :-
    string_codes(URIString, Codes),
    scheme_parse(Codes, Scheme, SchemeTail),
    urilib_parse_scheme_based(Scheme, SchemeTail, Out).

urilib_parse_scheme_based("mailto", SchemeTail, uri("mailto", Ui, Host)) :-
    !,
    userinfo_mailto_parse(SchemeTail, Ui, UserTail),
    host_mailto_parse(UserTail, Host).

urilib_parse_scheme_based("news", SchemeTail, uri("news", Host)) :-
    !,
    host_parse(SchemeTail, Host, []).

urilib_parse_scheme_based("tel", SchemeTail, uri("tel", Userinfo)) :-
    !,
    identifier(SchemeTail),
    string_codes(Userinfo, SchemeTail).

urilib_parse_scheme_based("fax", SchemeTail, uri("fax", Userinfo)) :-
    !,
    identifier(SchemeTail),
    string_codes(Userinfo, SchemeTail).

urilib_parse_scheme_based("zos", ST, uri("zos", Ui, H, Port, Path, Q, Frag)) :-
    !,
    authority_parse(ST, Ui, H, Port, AuthTail),
    path_zos_parse(AuthTail, Path, PathTail),
    query_parse(PathTail, Q, QueryTail),
    fragment_parse(QueryTail, Frag).


urilib_parse_scheme_based(Scheme, ST, uri(Scheme, U, H, Port, Path, Q, Frag)) :-
    authority_parse(ST, U, H, Port, AuthTail),
    path_parse(AuthTail, Path, PathTail),
    query_parse(PathTail, Q, QueryTail),
    fragment_parse(QueryTail, Frag).

%
% Scheme Parser
%

scheme_parse([F | T], Scheme, Tail) :-
    character(F),
    scheme_r_parse(T, SchemeCode, Tail),
    string_codes(Scheme, [F | SchemeCode]).

scheme_r_parse([], _, _) :-
    false.

scheme_r_parse([58 | Rest], [], Rest) :-
    !.

scheme_r_parse([First | Tail], [First | Scheme], Rest) :-
    character(First),
    scheme_r_parse(Tail, Scheme, Rest).

%
% Authority Parser
%

% 47 = '/'
authority_parse([47, 47 | Codes], Userinfo, Host, Port, Tail) :-
    userinfo_parse(Codes, Userinfo, UserTail),
    host_parse(UserTail, Host, HostTail),
    port_parse(HostTail, Port, Tail),
    !.

authority_parse([47 | Codes], [], [], 80, Codes) :-
    !.

authority_parse(Codes, [], [], 80, Codes) :- !.


userinfo_parse([F | C], User, Tail) :-
    character(F),
    userinfo_r_parse(C, UserCode, Tail),
    !,
    string_codes(User, [F | UserCode]).

userinfo_parse(Rest, [], Rest).

userinfo_r_parse([], _, _) :-
    false.

% 64 = @
userinfo_r_parse([64 | Rest], [], Rest) :-
    !.

userinfo_r_parse([First | Tail], [First | User], Rest) :-
    character(First),
    userinfo_r_parse(Tail, User, Rest).

%
% HOST PARSE
%

host_parse(Codes, Host, [Separator | Rest]) :-
    append(HostCodes, [Separator | Rest], Codes),
    isHostSeparator(Separator),
    !,
    isHostValid(HostCodes),
    string_codes(Host, HostCodes).

host_parse(Codes, Host, []) :-
    isHostValid(Codes),
    string_codes(Host, Codes),
    !.

port_parse([], 80, []) :-
    !.

port_parse([58 | Codes], Port, [Separator | Rest]) :-
    append(PortCodes, [Separator | Rest], Codes),
    isPortSeparator(Separator),
    isPortValid(PortCodes),
    !,
    number_codes(Port, PortCodes).

port_parse([58 | Codes], Port, []) :-
    isPortValid(Codes),
    number_codes(Port, Codes),
    !.

port_parse([First | Codes], 80, [First | Codes]) :-
    First \= 58,
    !.

% 58 = ':'
isHostSeparator(58) :- !.
% 47 = '/'
isHostSeparator(47) :- !.
% 63 = '?'
isHostSeparator(63) :- !.
% 35 = '#'
isHostSeparator(35) :- !.

% 47 = '/'
isPortSeparator(47) :- !.
% 63 = '?'
isPortSeparator(63) :- !.
% 35 = '#'
isPortSeparator(35) :- !.


%
% Path Parser
%

path_parse([], [], []) :- !.

path_parse([47], [], []) :- !.

path_parse([47, S | T], [], [S | T]) :-
    isPathSeparator(S),
    !.

path_parse([S | T], [], [S | T]) :-
    isPathSeparator(S),
    !.

path_parse([47 | Codes], Path, [Separator | Tail]) :-
    append(PathCode, [Separator | Tail], Codes),
    isPathSeparator(Separator),
    !,
    isPathValid(PathCode),
    string_codes(Path, PathCode).

path_parse([47 | Codes], Path, []) :-
    !,
    isPathValid(Codes),
    string_codes(Path, Codes).

path_parse(Codes, Path, [Separator | Tail]) :-
    append(PathCode, [Separator | Tail], Codes),
    isPathSeparator(Separator),
    !,
    isPathValid(PathCode),
    string_codes(Path, PathCode).

path_parse(Codes, Path, []) :-
    !,
    isPathValid(Codes),
    string_codes(Path, Codes).

path_parse(Codes, [], Codes) :- !.

%
% MAILTO 
%

userinfo_mailto_parse(Codes, User, [64 | Tail]) :-
    append(UserCodes, [64 | Tail], Codes),
    !,
    identifier(UserCodes),
    string_codes(User, UserCodes).

userinfo_mailto_parse(Codes, User, []) :-
    identifier(Codes),
    string_codes(User, Codes).

host_mailto_parse([], []) :- !.

host_mailto_parse([64 | Codes], Host) :-
    host_parse(Codes, Host, []).

%
% ZOS Path Parser
%

path_zos_parse([], [], []) :- !.

path_zos_parse([47], [], []) :- !.

path_zos_parse([47, S | T], [], [S | T]) :-
    isPathSeparator(S),
    !.

path_zos_parse([S | T], [], [S | T]) :-
    isPathSeparator(S),
    !.

path_zos_parse([47 | Codes], Path, [Separator | Tail]) :-
    append(PathCode, [Separator | Tail], Codes),
    isPathSeparator(Separator),
    !,
    isZosPathValid(PathCode),
    string_codes(Path, PathCode).

path_zos_parse([47 | Codes], Path, []) :-
    !,
    isZosPathValid(Codes),
    string_codes(Path, Codes).

path_zos_parse(Codes, Path, [Separator | Tail]) :-
    append(PathCode, [Separator | Tail], Codes),
    isPathSeparator(Separator),
    !,
    isZosPathValid(PathCode),
    string_codes(Path, PathCode).

path_zos_parse(Codes, Path, []) :-
    !,
    isZosPathValid(Codes),
    string_codes(Path, Codes).


path_zos_parse(Codes, [], Codes) :- !.



% 63 = '?'
isPathSeparator(63) :- !.
% 35 = '#'
isPathSeparator(35) :- !.

%
% Query Parser
%

query_parse([63 | Codes], Query, [35 | Tail]) :-
    append(QueryCode, [35 | Tail], Codes),
    characters(QueryCode),
    string_codes(Query, QueryCode),
    !.

query_parse([63 | Codes], Query, []) :-
    characters(Codes),
    string_codes(Query, Codes),
    !.

query_parse([], [], []) :-
    !.

query_parse([First | Tail], [], [First | Tail]) :-
    First \= 63,
    !.

%
% Fragment Parser
%

fragment_parse([35 | Codes], Fragment) :-
    characters(Codes),
    string_codes(Fragment, Codes),
    !.

fragment_parse([], []) :-
    !.

%
%  Valid
%

isPathValid(Codes) :-
    append(First, [47 | Second], Codes),
    !,
    identifier(First),
    isPathValid(Second).

isPathValid(Codes) :-
    identifier(Codes).

isZosPathValid(Codes) :-
    id44_parse(Codes, Tail),
    id8_parse(Tail).


% 40 = '('
id44_parse(Codes, [40 | Tail]) :-
    append(Id44Code, [40 | Tail], Codes),
    !,
    length(Id44Code, L),
    L =< 44,
    id44_valid(Id44Code).

id44_parse(Codes, []) :-
    !,
    length(Codes, L),
    L =< 44,
    id44_valid(Codes).

id8_parse([]) :-
    !,
    true.

id8_parse([40 | Codes]) :-
    append(Id8, [41], Codes),
    !,
    id8_valid(Id8).

% 46 = '.'
id44_valid(Codes) :-
    alphanums(Codes),
    !.

id44_valid(Codes) :-
    append(Chars, [46 | Rest], Codes),
    alphanums(Chars),
    id44_valid(Rest),
    !.

id8_valid(Codes) :-
    alphanums(Codes),
    !.


isHostValid(Codes) :-
    getsIPnumbers(Codes, A, B, C, D),
    isIPnumberValid(A),
    isIPnumberValid(B),
    isIPnumberValid(C),
    isIPnumberValid(D),
    !.

isHostValid(Codes) :-
    host_identifier(Codes),
    !.

isHostValid(Codes) :-
    append(A, [46 | B], Codes),
    host_identifier(A),
    isHostValid(B),
    !.

% 46 = '.'
getsIPnumbers(Codes, A, B, C, D) :-
    append(A, [46 | ATail], Codes),
    append(B, [46 | BTail], ATail),
    append(C, [46 | D], BTail).


isIPnumberValid(Codes) :-
    digit(Codes),
    number_codes(Number, Codes),
    Number >= 0,
    Number =< 255.


isPortValid(Codes) :-
    digit(Codes),
    !.


%
%
%
% GRAMMAR
%
%
%

identifier(Codes) :- characters(Codes).

host_identifier([First | Rest]) :-
    letter(First),
    characters(Rest).

host_identifier([First]) :-
    letter(First).

% 95 = '_'
character(95) :- !.
% 61 = '='
character(61) :- !.
% 43 = '+'
character(43) :- !.
% 45 = '-'
character(45) :- !.
% 58 = ':'
character(58) :- !.
character(Code) :- letter(Code), !.
character(Code) :- digit(Code), !.

characters(Code) :-
    character(Code),
    !.

characters([Code]) :-
    character(Code),
    !.

characters([Code|Rest]) :-
    character(Code),
    characters(Rest),
    !.


% UpperCase
letter(Code) :-
    integer(Code),
    Code >= 65,
    Code =< 90,
    !.

% LowerCase
letter(Code) :-
    integer(Code),
    Code >= 97,
    Code =< 122,
    !.

digit([]) :-
    !,
    false.

digit([Code]) :-
    !,
    digit(Code).

digit([First | Rest]) :-
    digit(First),
    digit(Rest),
    !.

digit(Code) :-
    integer(Code),
    Code >= 48,
    Code =< 57,
    !.

alphanums([]) :-
    false,
    !.

alphanums([Code]) :-
    alphanums(Code),
    !.

alphanums([First | Rest]) :-
    alphanums(First),
    alphanums(Rest),
    !.

alphanums(Code) :-
    letter(Code),
    !.

alphanums(Code) :-
    digit(Code),
    !.


%
% DISPLAY FUNCTIONS
%

urilib_display(UriString) :-
    urilib_parse(UriString, uri(S, Ui, Host, Port, Path, Query, Fragment)),
    format("Scheme: ~w~n", [S]),
    format("Userinfo: ~w~n", [Ui]),
    format("Host: ~w~n", [Host]),
    format("Port: ~w~n", [Port]),
    format("Path: ~w~n", [Path]),
    format("Query: ~w~n", [Query]),
    format("Fragment: ~w~n", [Fragment]).

urilib_display(In, Stream) :-
    urilib_parse(In, uri(S, Ui, Host, Port, Path, Query, Fragment)),
    format(Stream, "Scheme: ~w~n", [S]),
    format(Stream, "Userinfo: ~w~n", [Ui]),
    format(Stream, "Host: ~w~n", [Host]),
    format(Stream, "Port: ~w~n", [Port]),
    format(Stream, "Path: ~w~n", [Path]),
    format(Stream, "Query: ~w~n", [Query]),
    format(Stream, "Fragment: ~w~n", [Fragment]).
