%  Cristiano Rotunno, Matricola: "null"
%  Alessandro Rutigliano, Matricola: "909971"

urilib_parse(URIString, Out) :-
    string_codes(URIString, Codes),
    scheme_parse(Codes, Scheme, SchemeTail),
    urilib_parse_scheme_based(Scheme, SchemeTail, Out).

urilib_parse_scheme_based("mailto", SchemeTail, uri("mailto", Userinfo, Host)) :-
    !,
    userinfo_parse(SchemeTail, Userinfo, UserTail),
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

urilib_parse_scheme_based("zos", SchemeTail, uri("zos", Userinfo, Host, Port, Path, Query, Fragment)) :-
    !,
    authority_parse(SchemeTail, Userinfo, Host, Port, AuthTail),
    path_zos_parse(AuthTail, Path, PathTail),
    query_parse(PathTail, Query, QueryTail),
    fragment_parse(QueryTail, Fragment).


urilib_parse_scheme_based(Scheme, SchemeTail, uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment)) :-
    authority_parse(SchemeTail, Userinfo, Host, Port, AuthTail),
    path_parse(AuthTail, Path, PathTail),
    query_parse(PathTail, Query, QueryTail),
    fragment_parse(QueryTail, Fragment).


%
% Parser dello Schema
%

% 58 = ':'

scheme_parse(Codes, Scheme, Tail) :-
    append(SchemeCode, [58|Tail], Codes),
    !,
    identifier(SchemeCode),
    string_codes(Scheme, SchemeCode).

%
% Parser dell'Autorita
%

% 47 = '/'

authority_parse([47, 47 | Codes], Userinfo, Host, Port, Tail) :-
    userinfo_parse(Codes, Userinfo, UserTail),
    host_parse(UserTail, Host, HostTail),
    port_parse(HostTail, Port, Tail),
    !.

authority_parse(Codes, [], [], [], Codes) :- !.

% 64 = '@'
userinfo_parse(Codes, Userinfo, UserTail) :-
    append(UserCode, [64 | UserTail], Codes),
    identifier(UserCode),
    string_codes(Userinfo, UserCode),
    !.

userinfo_parse(Codes, [], Codes) :- !.

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
% Parser del Percorso
%

path_parse([47 | Codes], Path, [Separator | Tail]) :-
    append(PathCode, [Separator | Tail], Codes),
    isPathSeparator(Separator),
    !,
    isPathValid(PathCode),
    !,
    string_codes(Path, PathCode).

path_parse([47 | Codes], Path, []) :-
    !,
    isPathValid(Codes),
    !,
    string_codes(Path, Codes).

path_parse(Codes, [], Codes) :- !.


%
% Parser del Percorso ZOS
%

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

path_zos_parse(Codes, [], Codes) :- !.


% 63 = '?'
isPathSeparator(63) :- !.
% 35 = '#'
isPathSeparator(35) :- !.

%
% Parser della Query
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
% Parser del Fragmento
%

fragment_parse([35 | Codes], Fragment) :-
    characters(Codes),
    string_codes(Fragment, Codes),
    !.

fragment_parse([], []) :-
    !.

fragment_parse([First | _], []) :-
    First \= 35,
    !.


%
% Validazione
%

isPathValid(Codes) :-
    append(First, [47 | Second], Codes),
    identifier(First),
    isPathValid(Second).

isPathValid(Codes) :-
    identifier(Codes).

isZosPathValid(Codes) :-
    id44_parse(Codes, Tail),
    id8_parse(Tail).


% 41 = '('
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
% Grammatica
%


identifier(Codes) :- characters(Codes).

host_identifier([First | Rest]) :-
    letter(First),
    characters(Rest).

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


% Maiuscole
letter(Code) :-
    integer(Code),
    Code >= 65,
    Code =< 90,
    !.

% Minuscole
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
% Funzioni per la visualizzazione della URI
%

% Funzioni per stampare una URI sullo schermo
urilib_display(uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment)) :-
    write('Schema: '), write(Scheme), nl,
    write('Informazioni utente: '), (Userinfo \= [] -> write(Userinfo); write('N/A')), nl,
    write('Host: '), write(Host), nl,
    write('Porta: '), write(Port), nl,
    write('Percorso: '), (Path \= [] -> write(Path); write('N/A')), nl,
    write('Query: '), (Query \= [] -> write(Query); write('N/A')), nl,
    write('Fragmento: '), (Fragment \= [] -> write(Fragment); write('N/A')), nl.

% Funzioni per stampare una URI su un file
urilib_display(uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment), Stream) :-
    write(Stream, 'Schema: '), write(Stream, Scheme), nl(Stream),
    write(Stream, 'Informazioni utente: '), (Userinfo \= [] -> write(Stream, Userinfo); write(Stream, 'N/A')), nl(Stream),
    write(Stream, 'Host: '), write(Stream, Host), nl(Stream),
    write(Stream, 'Porta: '), write(Stream, Port), nl(Stream),
    write(Stream, 'Percorso: '), (Path \= [] -> write(Stream, Path); write(Stream, 'N/A')), nl(Stream),
    write(Stream, 'Query: '), (Query \= [] -> write(Stream, Query); write(Stream, 'N/A')), nl(Stream),
    write(Stream, 'Fragmento: '), (Fragment \= [] -> write(Stream, Fragment); write(Stream, 'N/A')), nl(Stream).


%
% Tests
%

:- begin_tests(urilib_parse_extracted).

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

    test('display_uri_on_screen') :-
        urilib_parse('http://disco.unimib.it', URI),
        with_output_to(string(Output),
                       urilib_display(URI)),
        ExpectedOutput = "Schema: http\nInformazioni utente: N/A\nHost: disco.unimib.it\nPorta: 80\nPercorso: N/A\nQuery: N/A\nFragmento: N/A\n",
        assertion(Output == ExpectedOutput).

    test('display_uri_on_file') :-
        urilib_parse('http://disco.unimib.it', URI),
        tmp_file_stream(text, TextFile, Stream), 
        urilib_display(URI, Stream),
        close(Stream),
        open(TextFile, read, In),
        read_string(In, _, FileContent),
        close(In),
        delete_file(TextFile),
        ExpectedContent = "Schema: http\nInformazioni utente: N/A\nHost: disco.unimib.it\nPorta: 80\nPercorso: N/A\nQuery: N/A\nFragmento: N/A\n",
        assertion(FileContent == ExpectedContent).

:- end_tests(urilib_parse_extracted).