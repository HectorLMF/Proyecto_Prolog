% Configuración e importación de módulos
% Parámetros: Ninguno
% Descripción: Este bloque de código configura y carga los módulos necesarios para el funcionamiento del servidor web en Prolog.
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)). % Agregado para habilitar CORS
:- use_module(library(settings)).
:- use_module(library(lists)).

% Definir un diccionario de palabras
% Parámetros: Ninguno
% Descripción: Este predicado define el diccionario utilizado por el servidor web. Puedes extenderlo agregando más palabras según sea necesario.
diccionario(['hola', 'como', 'estas', 'perro', 'gato', 'casa', 'jugando', 'prolog', 'web', 'statica', 'programacion', 'desarrollo', 'javascript', 'python', 'inteligencia', 'artificial', 'ciencia', 'datos', 'tecnologia', 'informatica', 'computadora', 'algoritmo', 'estructura', 'diseno', 'base', 'conocimiento', 'base', 'datos', 'analisis', 'software', 'hardware', 'redes', 'programacion', 'desarrollo', 'javascript', 'python', 'inteligencia', 'artificial', 'ciencia', 'datos', 'tecnologia', 'informatica', 'computadora', 'algoritmo', 'estructura', 'diseno', 'base', 'conocimiento', 'base', 'datos', 'analisis', 'software', 'hardware', 'redes']).

% Manejar la ruta /consultar
% Parámetros: Request (solicitud HTTP)
% Descripción: Este manejador procesa la solicitud HTTP en la ruta /consultar, filtrando palabras según los parámetros recibidos y respondiendo con la lista de palabras filtradas.
:- http_handler(root(consultar), consultar_handler, [prefix]). % Agregado [prefix] para manejar CORS
:- http_cors_enable. % Agregado para habilitar CORS

% Configuración de registro
% Parámetros: Ninguno
% Descripción: Este predicado define el archivo de registro para el servidor web.
:- dynamic log_file/1.
log_file('debug_log.txt').

% Predicado para abrir el archivo de registro
% Parámetros: Stream (flujo de archivo)
% Descripción: Este predicado abre el archivo de registro en modo de apendizaje.
open_log_file(Stream) :-
    log_file(FileName),
    open(FileName, append, Stream).

% Predicado para escribir en el archivo de registro
% Parámetros: Message (mensaje a escribir en el archivo de registro)
% Descripción: Este predicado escribe un mensaje en el archivo de registro y agrega un salto de línea.
write_to_log(Message) :-
    open_log_file(Stream),
    write(Stream, Message),
    nl(Stream),
    close(Stream).

% Filtrar las palabras por longitud
% Parámetros: Lista de palabras, Longitud deseada, Lista filtrada
% Descripción: Este predicado filtra una lista de palabras según la longitud deseada.
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

% Manejador para la consulta
% Parámetros: Request (solicitud HTTP)
% Descripción: Este manejador procesa la solicitud HTTP en la ruta /consultar, filtrando palabras según los parámetros recibidos y respondiendo con la lista de palabras filtradas o un mensaje de error.
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
% Parámetros: PalabrasFiltradas (lista de palabras filtradas)
% Descripción: Esta regla responde con un JSON que indica éxito y contiene la lista de palabras filtradas.
responder_success(PalabrasFiltradas) :-
    format('Content-type: application/json~n~n'),
    format('{ "status": "success", "all_words": ~w }', [PalabrasFiltradas]).

% Regla adicional para manejar las respuestas de error
% Parámetros: Mensaje (mensaje de error)
% Descripción: Esta regla responde con un JSON que indica un error y contiene el mensaje de error.
responder_error(Mensaje) :-
    format('Content-type: application/json~n~n'),
    format('{ "status": "error", "message": "~w" }', [Mensaje]).

% Filtrar las palabras que contienen al menos un carácter de la lista "caracteres"
% Parámetros: Caracteres (lista de caracteres a buscar), PalabrasFiltradas (lista de palabras filtradas)
% Descripción: Este predicado filtra las palabras del diccionario que contienen al menos un carácter de la lista "caracteres".
incluir_palabras_con_caracteres(Caracteres, PalabrasFiltradas) :-
    diccionario(Diccionario),
    include(contiene_caracteres(Caracteres), Diccionario, PalabrasFiltradasConRepetidos),
    sort(PalabrasFiltradasConRepetidos, PalabrasFiltradas),
    write_to_log('DEBUG: Palabras filtradas:'),
    write_to_log(PalabrasFiltradas).

% Verificar si una palabra contiene al menos un carácter de la lista dada
% Parámetros: Caracteres (lista de caracteres a buscar), Palabra (palabra a verificar)
% Descripción: Este predicado verifica si una palabra contiene al menos un carácter de la lista dada.
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
% Parámetros: Término (término a verificar)
% Descripción: Este predicado verifica si un término es un número entero.
is_integer(Term) :-
    integer(Term).

% Iniciar el servidor
% Parámetros: Ninguno
% Descripción: Este bloque de código inicia el servidor web en el puerto 3050.
:- initialization(main).

main :-
    http_server(http_dispatch, [port(3050)]).
