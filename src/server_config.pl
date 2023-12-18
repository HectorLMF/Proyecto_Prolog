:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

:- initialization(main).

main :-
    http_server(http_dispatch, [port(8080)]),
    writeln('Server started on port 8080').