%  Cristiano Rotunno, Matricola: "914317"
%  Alessandro Rutigliano, Matricola: "909971"

urilib_parse(URIString, Out) :-
    string_codes(URIString, Codes),
    scheme_parse(Codes, Scheme, SchemeTail),
    urilib_parse_scheme_based(Scheme, SchemeTail, Out).

urilib_parse_scheme_based("mailto", SchemeTail, uri("mailto", Ui, Host)) :-
    !,
    userinfo_parse(SchemeTail, Ui, UserTail),
    host_parse(UserTail, Host, []).

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

path_parse([C | T], [], [C | T]) :-
    isPathSeparator(C),
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
% ZOS Path Parser
%

path_zos_parse([], [], []) :- !.

path_zos_parse([47], [], []) :- !.

path_zos_parse([C | T], [], [C | T]) :-
    isPathSeparator(C),
    !.


% 40 = '('
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


%
% Tests
%

:- begin_tests(urilib_parse_extracted).

    %% Test esistenti (mantenuti per completezza)

    test('fax:+1-816-555-1212') :-
        urilib_parse('fax:+1-816-555-1212', Result),
        assertion(Result == uri("fax", "+1-816-555-1212")).

    test('fax:0123456789') :-
        urilib_parse('fax:0123456789', Result),
        assertion(Result == uri("fax", "0123456789")).

    test('fax:0184-567345') :-
        urilib_parse('fax:0184-567345', Result),
        assertion(Result == uri("fax", "0184-567345")).

    test('ftp://192..168.1.1', [fail]) :-
        urilib_parse('ftp://192..168.1.1', _).

    test('ftp://192.168.1', [fail]) :-
        urilib_parse('ftp://192.168.1', _).

    test('ftp://192.abc.1.1', [fail]) :-
        urilib_parse('ftp://192.abc.1.1', _).

    test('ftp://ftp.example.com/files') :-
        urilib_parse('ftp://ftp.example.com/files', Result),
        assertion(Result == uri("ftp", [], "ftp.example.com", 80, "files", [], [])).

    test('ftp://ftp.is.co.za/rfc/rfc1808txt') :-
        urilib_parse('ftp://ftp.is.co.za/rfc/rfc1808txt', Result),
        assertion(Result == uri("ftp", [], "ftp.is.co.za", 80, "rfc/rfc1808txt", [], [])).

    test('http://192.168.1.1') :-
        urilib_parse('http://192.168.1.1', Result),
        assertion(Result == uri("http", [], "192.168.1.1", 80, [], [], [])).

    test('http://256.100.50.25', [fail]) :-
        urilib_parse('http://256.100.50.25', _).

    test('http://disco.unimib.it') :-
        urilib_parse('http://disco.unimib.it', Result),
        assertion(Result == uri("http", [], "disco.unimib.it", 80, [], [], [])).

    test('http://user@server.example.org:8080/path/to/resource?key=value#section1') :-
        urilib_parse('http://user@server.example.org:8080/path/to/resource?key=value#section1', Result),
        assertion(Result == uri("http", "user", "server.example.org", 8080, "path/to/resource", "key=value", "section1")).

    test('http://www.ietf.org/rfc/rfc2396txt') :-
        urilib_parse('http://www.ietf.org/rfc/rfc2396txt', Result),
        assertion(Result == uri("http", [], "www.ietf.org", 80, "rfc/rfc2396txt", [], [])).

    test('https://255.255.255.255:443/path?query=123#frag') :-
        urilib_parse('https://255.255.255.255:443/path?query=123#frag', Result),
        assertion(Result == uri("https", [], "255.255.255.255", 443, "path", "query=123", "frag")).

    test('https://example.com') :-
        urilib_parse('https://example.com', Result),
        assertion(Result == uri("https", [], "example.com", 80, [], [], [])).

    test('https://www.ietf.org/rfc/rfc2396txt') :-
        urilib_parse('https://www.ietf.org/rfc/rfc2396txt', Result),
        assertion(Result == uri("https", [], "www.ietf.org", 80, "rfc/rfc2396txt", [], [])).

    test('mailto:JohnDoe@example.com') :-
        urilib_parse('mailto:JohnDoe@example.com', Result),
        assertion(Result == uri("mailto", "JohnDoe", "example.com")).

    test('mailto:user@example.com') :-
        urilib_parse('mailto:user@example.com', Result),
        assertion(Result == uri("mailto", "user", "example.com")).

    test('news:comp.infosystems.www.servers.unix') :-
        urilib_parse('news:comp.infosystems.www.servers.unix', Result),
        assertion(Result == uri("news", "comp.infosystems.www.servers.unix")).

    test('news:comp.lang.julia') :-
        urilib_parse('news:comp.lang.julia', Result),
        assertion(Result == uri("news", "comp.lang.julia")).

    test('news:my.news.server') :-
        urilib_parse('news:my.news.server', Result),
        assertion(Result == uri("news", "my.news.server")).

    test('tel:+1-816-555-1212') :-
        urilib_parse('tel:+1-816-555-1212', Result),
        assertion(Result == uri("tel", "+1-816-555-1212")).

    test('tel:+39-3541237567') :-
        urilib_parse('tel:+39-3541237567', Result),
        assertion(Result == uri("tel", "+39-3541237567")).

    test('tel:0123456789') :-
        urilib_parse('tel:0123456789', Result),
        assertion(Result == uri("tel", "0123456789")).

    test('zos://u@hh:80/DATASETNAME') :-
        urilib_parse('zos://u@hh:80/DATASETNAME', Result),
        assertion(Result == uri("zos", "u", "hh", 80, "DATASETNAME", [], [])).

    test('zos://user@mainframe.host:1234/DATASET.NAME(ENTRY)?q=test#frag') :-
        urilib_parse('zos://user@mainframe.host:1234/DATASET.NAME(ENTRY)?q=test#frag', Result),
        assertion(Result == uri("zos", "user", "mainframe.host", 1234, "DATASET.NAME(ENTRY)", "q=test", "frag")).

    %% Test aggiuntivi per stringhe VALID

    %% Tests per HTTPS
    test('https://user1@google.com') :-
        urilib_parse('https://user1@google.com', Result),
        assertion(Result == uri("https", "user1", "google.com", 80, [], [], [])).

    test('https://google.com') :-
        urilib_parse('https://google.com', Result),
        assertion(Result == uri("https", [], "google.com", 80, [], [], [])).

    test('https://user1@google.com:123') :-
        urilib_parse('https://user1@google.com:123', Result),
        assertion(Result == uri("https", "user1", "google.com", 123, [], [], [])).

    test('https://user1@google.com.c81z:123') :-
        urilib_parse('https://user1@google.com.c81z:123', Result),
        assertion(Result == uri("https", "user1", "google.com.c81z", 123, [], [], [])).

    test('https://user1@1.234.12.98') :-
        urilib_parse('https://user1@1.234.12.98', Result),
        assertion(Result == uri("https", "user1", "1.234.12.98", 80, [], [], [])).

    test('https://user1@1.234.12.98:98') :-
        urilib_parse('https://user1@1.234.12.98:98', Result),
        assertion(Result == uri("https", "user1", "1.234.12.98", 98, [], [], [])).

    test('https://user1@g.c.c:45') :-
        urilib_parse('https://user1@g.c.c:45', Result),
        assertion(Result == uri("https", "user1", "g.c.c", 45, [], [], [])).

    test('https://google.com:123') :-
        urilib_parse('https://google.com:123', Result),
        assertion(Result == uri("https", [], "google.com", 123, [], [], [])).

    test('https:/') :-
        urilib_parse('https:/', Result),
        assertion(Result == uri("https", [], [], 80, [], [], [])).

    test('https:/path/to/res') :-
        urilib_parse('https:/path/to/res', Result),
        assertion(Result == uri("https", [], [], 80, "path/to/res", [], [])).

    test('https:/?query=value') :-
        urilib_parse('https:/?query=value', Result),
        assertion(Result == uri("https", [], [], 80, [], "query=value", [])).

    test('https:/?query=value#frag') :-
        urilib_parse('https:/?query=value#frag', Result),
        assertion(Result == uri("https", [], [], 80, [], "query=value", "frag")).

    test('https:/#frag') :-
        urilib_parse('https:/#frag', Result),
        assertion(Result == uri("https", [], [], 80, [], [], "frag")).

    test('https:/path/to/res?query=value') :-
        urilib_parse('https:/path/to/res?query=value', Result),
        assertion(Result == uri("https", [], [], 80, "path/to/res", "query=value", [])).

    test('https:/path/to/res#frag') :-
        urilib_parse('https:/path/to/res#frag', Result),
        assertion(Result == uri("https", [], [], 80, "path/to/res", [], "frag")).

    test('https:/path/to/res?query=value#frag') :-
        urilib_parse('https:/path/to/res?query=value#frag', Result),
        assertion(Result == uri("https", [], [], 80, "path/to/res", "query=value", "frag")).

    test('https:') :-
        urilib_parse('https:', Result),
        assertion(Result == uri("https", [], [], 80, [], [], [])).

    test('https:path/to/res') :-
        urilib_parse('https:path/to/res', Result),
        assertion(Result == uri("https", [], [], 80, "path/to/res", [], [])).

    test('https:?query=value') :-
        urilib_parse('https:?query=value', Result),
        assertion(Result == uri("https", [], [], 80, [], "query=value", [])).

    test('https:#frag') :-
        urilib_parse('https:#frag', Result),
        assertion(Result == uri("https", [], [], 80, [], [], "frag")).

    test('https:path/to/res?query=value') :-
        urilib_parse('https:path/to/res?query=value', Result),
        assertion(Result == uri("https", [], [], 80, "path/to/res", "query=value", [])).

    test('https:path/to/res#frag') :-
        urilib_parse('https:path/to/res#frag', Result),
        assertion(Result == uri("https", [], [], 80, "path/to/res", [], "frag")).

    test('https:path/to/res?query=value#frag') :-
        urilib_parse('https:path/to/res?query=value#frag', Result),
        assertion(Result == uri("https", [], [], 80, "path/to/res", "query=value", "frag")).

    test('https:?query=value#frag') :-
        urilib_parse('https:?query=value#frag', Result),
        assertion(Result == uri("https", [], [], 80, [], "query=value", "frag")).

    test('https://google.com/') :-
        urilib_parse('https://google.com/', Result),
        assertion(Result == uri("https", [], "google.com", 80, [], [], [])).

    test('https://google.comgoogle.com/path/to/res') :-
        urilib_parse('https://google.comgoogle.com/path/to/res', Result),
        assertion(Result == uri("https", [], "google.comgoogle.com", 80, "path/to/res", [], [])).

    test('https://google.com/?query=value') :-
        urilib_parse('https://google.com/?query=value', Result),
        assertion(Result == uri("https", [], "google.com", 80, [], "query=value", [])).

    test('https://google.com/?query=value#frag') :-
        urilib_parse('https://google.com/?query=value#frag', Result),
        assertion(Result == uri("https", [], "google.com", 80, [], "query=value", "frag")).

    test('https://google.com/#frag') :-
        urilib_parse('https://google.com/#frag', Result),
        assertion(Result == uri("https", [], "google.com", 80, [], [], "frag")).

    test('https://google.com/path/to/res?query=value') :-
        urilib_parse('https://google.com/path/to/res?query=value', Result),
        assertion(Result == uri("https", [], "google.com", 80, "path/to/res", "query=value", [])).

    test('https://google.com/path/to/res#frag') :-
        urilib_parse('https://google.com/path/to/res#frag', Result),
        assertion(Result == uri("https", [], "google.com", 80, "path/to/res", [], "frag")).

    test('https://google.com/path/to/res?query=value#frag') :-
        urilib_parse('https://google.com/path/to/res?query=value#frag', Result),
        assertion(Result == uri("https", [], "google.com", 80, "path/to/res", "query=value", "frag")).

    %% Tests per ZOS
    test('zos://user1@google.com') :-
        urilib_parse('zos://user1@google.com', Result),
        assertion(Result == uri("zos", "user1", "google.com", 80, [], [], [])).

    test('zos://google.com') :-
        urilib_parse('zos://google.com', Result),
        assertion(Result == uri("zos", [], "google.com", 80, [], [], [])).

    test('zos://user1@google.com:123') :-
        urilib_parse('zos://user1@google.com:123', Result),
        assertion(Result == uri("zos", "user1", "google.com", 123, [], [], [])).

    test('zos://user1@google.com.c81z:123') :-
        urilib_parse('zos://user1@google.com.c81z:123', Result),
        assertion(Result == uri("zos", "user1", "google.com.c81z", 123, [], [], [])).

    test('zos://user1@1.234.12.98') :-
        urilib_parse('zos://user1@1.234.12.98', Result),
        assertion(Result == uri("zos", "user1", "1.234.12.98", 80, [], [], [])).

    test('zos://user1@1.234.12.98:98') :-
        urilib_parse('zos://user1@1.234.12.98:98', Result),
        assertion(Result == uri("zos", "user1", "1.234.12.98", 98, [], [], [])).

    test('zos://user1@g.c.c:45') :-
        urilib_parse('zos://user1@g.c.c:45', Result),
        assertion(Result == uri("zos", "user1", "g.c.c", 45, [], [], [])).

    test('zos://google.com:123') :-
        urilib_parse('zos://google.com:123', Result),
        assertion(Result == uri("zos", [], "google.com", 123, [], [], [])).

    test('zos:/') :-
        urilib_parse('zos:/', Result),
        assertion(Result == uri("zos", [], [], 80, [], [], [])).

    test('zos:/PATH.ZOS.EXAMPLE(CIAO)') :-
        urilib_parse('zos:/PATH.ZOS.EXAMPLE(CIAO)', Result),
        assertion(Result == uri("zos", [], [], 80, "PATH.ZOS.EXAMPLE(CIAO)", [], [])).

    test('zos:/?query=value') :-
        urilib_parse('zos:/?query=value', Result),
        assertion(Result == uri("zos", [], [], 80, [], "query=value", [])).

    test('zos:/?query=value#frag') :-
        urilib_parse('zos:/?query=value#frag', Result),
        assertion(Result == uri("zos", [], [], 80, [], "query=value", "frag")).

    test('zos:/#frag') :-
        urilib_parse('zos:/#frag', Result),
        assertion(Result == uri("zos", [], [], 80, [], [], "frag")).

    test('zos:/PATH.ZOS.EXAMPLE(CIAO)?query=value') :-
        urilib_parse('zos:/PATH.ZOS.EXAMPLE(CIAO)?query=value', Result),
        assertion(Result == uri("zos", [], [], 80, "PATH.ZOS.EXAMPLE(CIAO)", "query=value", [])).

    test('zos:/PATH.ZOS.EXAMPLE(CIAO)#frag') :-
        urilib_parse('zos:/PATH.ZOS.EXAMPLE(CIAO)#frag', Result),
        assertion(Result == uri("zos", [], [], 80, "PATH.ZOS.EXAMPLE(CIAO)", [], "frag")).

    test('zos:/PATH.ZOS.EXAMPLE(CIAO)?query=value#frag') :-
        urilib_parse('zos:/PATH.ZOS.EXAMPLE(CIAO)?query=value#frag', Result),
        assertion(Result == uri("zos", [], [], 80, "PATH.ZOS.EXAMPLE(CIAO)", "query=value", "frag")).

    test('zos:') :-
        urilib_parse('zos:', Result),
        assertion(Result == uri("zos", [], [], 80, [], [], [])).

    test('zos:PATH.ZOS.EXAMPLE(CIAO)') :-
        urilib_parse('zos:PATH.ZOS.EXAMPLE(CIAO)', Result),
        assertion(Result == uri("zos", [], [], 80, "PATH.ZOS.EXAMPLE(CIAO)", [], [])).

    test('zos:?query=value') :-
        urilib_parse('zos:?query=value', Result),
        assertion(Result == uri("zos", [], [], 80, [], "query=value", [])).

    test('zos:#frag') :-
        urilib_parse('zos:#frag', Result),
        assertion(Result == uri("zos", [], [], 80, [], [], "frag")).

    test('zos:PATH.ZOS.EXAMPLE(CIAO)?query=value') :-
        urilib_parse('zos:PATH.ZOS.EXAMPLE(CIAO)?query=value', Result),
        assertion(Result == uri("zos", [], [], 80, "PATH.ZOS.EXAMPLE(CIAO)", "query=value", [])).

    test('zos:PATH.ZOS.EXAMPLE(CIAO)#frag') :-
        urilib_parse('zos:PATH.ZOS.EXAMPLE(CIAO)#frag', Result),
        assertion(Result == uri("zos", [], [], 80, "PATH.ZOS.EXAMPLE(CIAO)", [], "frag")).

    test('zos:PATH.ZOS.EXAMPLE(CIAO)?query=value#frag') :-
        urilib_parse('zos:PATH.ZOS.EXAMPLE(CIAO)?query=value#frag', Result),
        assertion(Result == uri("zos", [], [], 80, "PATH.ZOS.EXAMPLE(CIAO)", "query=value", "frag")).

    test('zos:?query=value#frag') :-
        urilib_parse('zos:?query=value#frag', Result),
        assertion(Result == uri("zos", [], [], 80, [], "query=value", "frag")).

    test('zos://google.com/') :-
        urilib_parse('zos://google.com/', Result),
        assertion(Result == uri("zos", [], "google.com", 80, [], [], [])).

    test('zos://google.comgoogle.com/PATH.ZOS.EXAMPLE(CIAO)') :-
        urilib_parse('zos://google.comgoogle.com/PATH.ZOS.EXAMPLE(CIAO)', Result),
        assertion(Result == uri("zos", [], "google.comgoogle.com", 80, "PATH.ZOS.EXAMPLE(CIAO)", [], [])).

    test('zos://google.com/?query=value') :-
        urilib_parse('zos://google.com/?query=value', Result),
        assertion(Result == uri("zos", [], "google.com", 80, [], "query=value", [])).

    test('zos://google.com/?query=value#frag') :-
        urilib_parse('zos://google.com/?query=value#frag', Result),
        assertion(Result == uri("zos", [], "google.com", 80, [], "query=value", "frag")).

    test('zos://google.com/#frag') :-
        urilib_parse('zos://google.com/#frag', Result),
        assertion(Result == uri("zos", [], "google.com", 80, [], [], "frag")).

    test('zos://google.com/PATH.ZOS.EXAMPLE(CIAO)?query=value') :-
        urilib_parse('zos://google.com/PATH.ZOS.EXAMPLE(CIAO)?query=value', Result),
        assertion(Result == uri("zos", [], "google.com", 80, "PATH.ZOS.EXAMPLE(CIAO)", "query=value", [])).

    test('zos://google.com/PATH.ZOS.EXAMPLE(CIAO)#frag') :-
        urilib_parse('zos://google.com/PATH.ZOS.EXAMPLE(CIAO)#frag', Result),
        assertion(Result == uri("zos", [], "google.com", 80, "PATH.ZOS.EXAMPLE(CIAO)", [], "frag")).

    test('zos://google.com/PATH.ZOS.EXAMPLE(CIAO)?query=value#frag') :-
        urilib_parse('zos://google.com/PATH.ZOS.EXAMPLE(CIAO)?query=value#frag', Result),
        assertion(Result == uri("zos", [], "google.com", 80, "PATH.ZOS.EXAMPLE(CIAO)", "query=value", "frag")).

    %% Tests per MAILTO
    test('mailto:user') :-
        urilib_parse('mailto:user', Result),
        assertion(Result == uri("mailto", "user", [])).

    test('mailto:user@google.com') :-
        urilib_parse('mailto:user@google.com', Result),
        assertion(Result == uri("mailto", "user", "google.com")).

    test('mailto:user@192.12.124.21') :-
        urilib_parse('mailto:user@192.12.124.21', Result),
        assertion(Result == uri("mailto", "user", "192.12.124.21")).

    %% Tests per NEWS
    test('news:google') :-
        urilib_parse('news:google', Result),
        assertion(Result == uri("news", "google")).

    test('news:google.com') :-
        urilib_parse('news:google.com', Result),
        assertion(Result == uri("news", "google.com")).

    test('news:192.12.124.21') :-
        urilib_parse('news:192.12.124.21', Result),
        assertion(Result == uri("news", "192.12.124.21")).

    %% Tests per TEL
    test('tel:+39-3209439015') :-
        urilib_parse('tel:+39-3209439015', Result),
        assertion(Result == uri("tel", "+39-3209439015")).

    test('tel:giacomo') :-
        urilib_parse('tel:giacomo', Result),
        assertion(Result == uri("tel", "giacomo")).

    %% Tests per FAX
    test('fax:+39-3209439015') :-
        urilib_parse('fax:+39-3209439015', Result),
        assertion(Result == uri("fax", "+39-3209439015")).

    test('fax:giacomo') :-
        urilib_parse('fax:giacomo', Result),
        assertion(Result == uri("fax", "giacomo")).

    %% Test aggiuntivi per stringhe INVALID

    %% Tests INVALID per HTTPS
    test('invalid_https://user1@1.234.12.256', [fail]) :-
        urilib_parse('https://user1@1.234.12.256', _).

    test('invalid_https://user1@1.12.98:98', [fail]) :-
        urilib_parse('https://user1@1.12.98:98', _).

    test('invalid_https://user1@google.1com.com:123', [fail]) :-
        urilib_parse('https://user1@google.1com.com:123', _).

    test('invalid_https://user1@1oogle.com:123', [fail]) :-
        urilib_parse('https://user1@1oogle.com:123', _).

    test('invalid_https://user1@:123', [fail]) :-
        urilib_parse('https://user1@:123', _).

    test('invalid_https://user1@google.com:abc', [fail]) :-
        urilib_parse('https://user1@google.com:abc', _).

    test('invalid_https://user1@google.com:', [fail]) :-
        urilib_parse('https://user1@google.com:', _).

    test('invalid_https:user1@', [fail]) :-
        urilib_parse('https:user1@', _).

    test('invalid_https://user1@', [fail]) :-
        urilib_parse('https://user1@', _).

    test('invalid_https::user@google.com', [fail]) :-
        urilib_parse(':user@google.com', _).

    test('invalid_https::', [fail]) :-
        urilib_parse(':', _).

    test('invalid_https::google.com', [fail]) :-
        urilib_parse(':google.com', _).

    test('invalid_https::/path/to/resource', [fail]) :-
        urilib_parse(':/path/to/resource', _).

    %% Tests INVALID per ZOS
    test('invalid_zos:(CIAO)?query=value', [fail]) :-
        urilib_parse('zos:(CIAO)?query=value', _).

    test('invalid_zos://user1@1.234.12.256', [fail]) :-
        urilib_parse('zos://user1@1.234.12.256', _).

    test('invalid_zos://user1@1.12.98:98', [fail]) :-
        urilib_parse('zos://user1@1.12.98:98', _).

    test('invalid_zos://user1@google.1com.com:123', [fail]) :-
        urilib_parse('zos://user1@google.1com.com:123', _).

    test('invalid_zos://user1@1oogle.com:123', [fail]) :-
        urilib_parse('zos://user1@1oogle.com:123', _).

    test('invalid_zos://user1@:123', [fail]) :-
        urilib_parse('zos://user1@:123', _).

    test('invalid_zos://user1@google.com:abc', [fail]) :-
        urilib_parse('zos://user1@google.com:abc', _).

    test('invalid_zos://user1@google.com:', [fail]) :-
        urilib_parse('zos://user1@google.com:', _).

    test('invalid_zos:user1@', [fail]) :-
        urilib_parse('zos:user1@', _).

    %% Tests INVALID per MAILTO
    test('invalid_mailto:', [fail]) :-
        urilib_parse('mailto:', _).

    test('invalid_mailto:user@', [fail]) :-
        urilib_parse('mailto:user@', _).

    test('invalid_mailto:@google.com', [fail]) :-
        urilib_parse('mailto:@google.com', _).

    test('invalid_mailto:user@google.com/path/to/res', [fail]) :-
        urilib_parse('mailto:user@google.com/path/to/res', _).

    test('invalid_mailto:user@google.com?query', [fail]) :-
        urilib_parse('mailto:user@google.com?query', _).

    test('invalid_mailto:user@google.com#fragment', [fail]) :-
        urilib_parse('mailto:user@google.com#fragment', _).

    %% Tests INVALID per NEWS
    test('invalid_news:user@google', [fail]) :-
        urilib_parse('news:user@google', _).

    test('invalid_news:user@google.com', [fail]) :-
        urilib_parse('news:user@google.com', _).

    test('invalid_news:user@192.12.124.21', [fail]) :-
        urilib_parse('news:user@192.12.124.21', _).

    test('invalid_news:mailto:google.com/path/to/res', [fail]) :-
        urilib_parse('mailto:google.com/path/to/res', _).

    test('invalid_news:mailto:google.com?query', [fail]) :-
        urilib_parse('mailto:google.com?query', _).

    test('invalid_news:mailto:google.com#fragment', [fail]) :-
        urilib_parse('mailto:google.com#fragment', _).

    %% Tests INVALID per TEL
    test('invalid_tel:tel:+39-320943#9015', [fail]) :-
        urilib_parse('tel:+39-320943#9015', _).

    test('invalid_tel:tel:giacomo@host', [fail]) :-
        urilib_parse('tel:giacomo@host', _).

    test('invalid_tel:tel:host.com', [fail]) :-
        urilib_parse('tel:host.com', _).

    test('invalid_tel:tel:192.12.124.21', [fail]) :-
        urilib_parse('tel:192.12.124.21', _).

    %% Tests INVALID per FAX
    test('invalid_fax:fax:+39-320943#9015', [fail]) :-
        urilib_parse('fax:+39-320943#9015', _).

    test('invalid_fax:fax:giacomo@host', [fail]) :-
        urilib_parse('fax:giacomo@host', _).

    test('invalid_fax:fax:host.com', [fail]) :-
        urilib_parse('fax:host.com', _).

    test('invalid_fax:fax:192.12.124.21', [fail]) :-
        urilib_parse('fax:192.12.124.21', _).

:- end_tests(urilib_parse_extracted).
