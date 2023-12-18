:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)). % Agregado para habilitar CORS
:- use_module(library(settings)).
:- use_module(library(lists)).

% Definir un diccionario de palabras
diccionario(['hola', 'como', 'estas', 'perro', 'gato', 'casa', 'jugando', 'prolog', 'web', 'statica', 'programacion', 'desarrollo', 'javascript', 'python', 'inteligencia', 'artificial', 'ciencia', 'datos', 'tecnologia', 'informatica', 'computadora', 'algoritmo', 'estructura', 'diseno', 'base', 'conocimiento', 'base', 'datos', 'analisis', 'software', 'hardware', 'redes', 'programacion', 'desarrollo', 'javascript', 'python', 'inteligencia', 'artificial', 'ciencia', 'datos', 'tecnologia', 'informatica', 'computadora', 'algoritmo', 'estructura', 'diseno', 'base', 'conocimiento', 'base', 'datos', 'analisis', 'software', 'hardware', 'redes']).

% Manejar la ruta /consultar
:- http_handler(root(consultar), consultar_handler, [prefix]). % Agregado [prefix] para manejar CORS
:- http_cors_enable. % Agregado para habilitar CORS

% Configuración de registro
:- dynamic log_file/1.
log_file('debug_log.txt').

% Predicado para abrir el archivo de registro
open_log_file(Stream) :-
    log_file(FileName),
    open(FileName, append, Stream).

% Predicado para escribir en el archivo de registro
write_to_log(Message) :-
    open_log_file(Stream),
    write(Stream, Message),
    nl(Stream),
    close(Stream).

% Filtrar las palabras por longitud
filtrar_por_longitud([], _, []).
filtrar_por_longitud([Palabra|Resto], Longitud, Resultado) :-
    atom_length(Palabra, LongAct),
    (LongAct =:= Longitud -> 
        Resultado = [Palabra|RestoFiltrado]
    ;
        Resultado = RestoFiltrado
    ),
    write_to_log('DEBUG: Palabra: ~w, Longitud actual: ~w, Longitud deseada: ~w' - [Palabra, LongAct, Longitud]),
    write_to_log('Resultado filtrado:' - RestoFiltrado),
    write_to_log('--------------------------'),
    filtrar_por_longitud(Resto, Longitud, RestoFiltrado).

% Modificado para recibir un término JSON
consultar_handler(Request) :-
    cors_enable(Request, [methods([post])]), % Agregado para habilitar CORS en la solicitud
    http_read_json_dict(Request, JSON),
    write_to_log('DEBUG: JSON recibido:'),
    write_to_log(JSON),
    ( get_dict(caracteres, JSON, Caracteres), is_list(Caracteres) ->
        % Filtrar las palabras que contienen al menos un carácter de la lista "caracteres"
        incluir_palabras_con_caracteres(Caracteres, PalabrasFiltradas),        
        % Obtener la longitud deseada
        ( get_dict(longitud, JSON, Longitud), is_integer(Longitud), Longitud > 0 ->
            % Filtrar las palabras por longitud
            filtrar_por_longitud(PalabrasFiltradas, Longitud, PalabrasFiltradasFinal),
            % Responder con la lista de palabras filtradas
            responder_success(PalabrasFiltradasFinal)
        ; % Si no se proporciona la longitud o no es válida, responder con error
            responder_error('Longitud inválida')
        )
    ; % Otros casos pueden ser manejados de manera similar
        write_to_log('DEBUG: Parámetros inválidos'),
        responder_error('Parámetros inválidos')
    ).

% Regla adicional para manejar las respuestas con éxito
responder_success(PalabrasFiltradas) :-
    format('Content-type: application/json~n~n'),
    format('{ "status": "success", "all_words": ~w }', [PalabrasFiltradas]).

% Regla adicional para manejar las respuestas de error
responder_error(Mensaje) :-
    format('Content-type: application/json~n~n'),
    format('{ "status": "error", "message": "~w" }', [Mensaje]).

% Filtrar las palabras que contienen al menos un carácter de la lista "caracteres"
incluir_palabras_con_caracteres(Caracteres, PalabrasFiltradas) :-
    diccionario(Diccionario),
    include(contiene_caracteres(Caracteres), Diccionario, PalabrasFiltradasConRepetidos),
    sort(PalabrasFiltradasConRepetidos, PalabrasFiltradas),
    write_to_log('DEBUG: Palabras filtradas:'),
    write_to_log(PalabrasFiltradas).

% Verificar si una palabra contiene al menos un carácter de la lista dada
contiene_caracteres(Caracteres, Palabra) :-
    atom_chars(Palabra, ListaPalabra),
    write_to_log('DEBUG: Lista de caracteres de la palabra ~w: ~w' - [Palabra, ListaPalabra]),
    write_to_log('Caracteres seleccionados:' - Caracteres),

    % Convertir todas las letras a minúsculas
    maplist(downcase_atom, ListaPalabra, ListaPalabraMinuscula),
    maplist(downcase_atom, Caracteres, CaracteresMinuscula),

    intersection(ListaPalabraMinuscula, CaracteresMinuscula, CaracteresComunes),
    write_to_log('Caracteres comunes:' - CaracteresComunes),
   
    (   CaracteresComunes \= [] ->
        write_to_log('DEBUG: Palabra: ~w, Caracteres comunes: ~w' - [Palabra, CaracteresComunes]),
        true
    ;   write_to_log('DEBUG: Palabra: ~w, Sin caracteres comunes'),
        fail
    ),
    write_to_log('--------------------------').

% Verificar si un término es un entero
is_integer(Term) :-
    integer(Term).

% Iniciar el servidor
:- initialization(main).

main :-
    http_server(http_dispatch, [port(3050)]).
