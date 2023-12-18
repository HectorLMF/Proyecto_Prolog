% Predicado principal para iniciar el juego
% Parámetros: Ninguno
% Descripción: Este predicado solicita al usuario que introduzca una palabra. Si la palabra no está instanciada, muestra un mensaje de error y vuelve a solicitar la entrada. Luego, inicializa el diccionario y llama al predicado jugarConDiccionario con la palabra introducida por el usuario.
jugar :-
    write('Introduce una palabra: '),
    read(UsuarioPalabra),
    (var(UsuarioPalabra) -> write('Debes introducir una palabra.'), nl, jugar; % Verificar que la palabra esté instanciada
     dictionary(D),
     asserta(diccionario_actual(D)), % Inicializar la variable global
     jugarConDiccionario(UsuarioPalabra)).

% Jugar con el diccionario actual
% Parámetros: UsuarioPalabra (palabra introducida por el usuario)
% Descripción: Este predicado verifica que haya palabras disponibles en el diccionario actual. Si es así, llama al predicado repetirJuego con la palabra introducida por el usuario. Si el diccionario está vacío, imprime un mensaje de finalización.
jugarConDiccionario(UsuarioPalabra) :-
    diccionario_actual(Diccionario), % Obtener el diccionario actualizado
    length(Diccionario, LongitudDiccionario),
    LongitudDiccionario > 0, % Verificar que haya palabras disponibles en el diccionario
    repetirJuego(UsuarioPalabra).

% Si el diccionario está vacío, imprimir mensaje de finalización
% Parámetros: No se espera ningún parámetro
% Descripción: Este predicado imprime un mensaje indicando que se han agotado las palabras en el diccionario y que el juego ha terminado.
jugarConDiccionario(_) :-
    writeln('¡Se acabaron las palabras en el diccionario! El juego ha terminado.').

% Comparar si las palabras son iguales y generar array de caracteres comunes
% Parámetros: PalabraUsuario (palabra introducida por el usuario), PalabraAleatoria (palabra seleccionada aleatoriamente), ArrayComun (lista de caracteres comunes)
% Descripción: Este predicado compara si las palabras PalabraUsuario y PalabraAleatoria son iguales y genera un array con los caracteres comunes entre ambas palabras.
compararPalabras(PalabraUsuario, PalabraAleatoria, ArrayComun) :-
    generarArrayComun(PalabraUsuario, PalabraAleatoria, ArrayComun),
    PalabraUsuario == PalabraAleatoria.

% Generar un array con los caracteres comunes entre dos palabras
% Parámetros: PalabraUsuario (palabra introducida por el usuario), PalabraAleatoria (palabra seleccionada aleatoriamente), ArrayComun (lista de caracteres comunes)
% Descripción: Este predicado genera un array con los caracteres comunes entre las palabras PalabraUsuario y PalabraAleatoria.
generarArrayComun(PalabraUsuario, PalabraAleatoria, ArrayComun) :-
    string_chars(PalabraUsuario, CharsUsuario),
    string_chars(PalabraAleatoria, CharsAleatoria),
    intersection(CharsUsuario, CharsAleatoria, ArrayComun).

% Elegir una palabra aleatoria de las candidatas
% Parámetros: PalabraUsuario (palabra introducida por el usuario), Diccionario (lista de palabras disponibles), ArrayComun (lista de caracteres comunes), NuevaPalabra (palabra seleccionada aleatoriamente)
% Descripción: Este predicado elige una nueva palabra aleatoria del diccionario que tenga la misma longitud que la palabra del usuario y comparta caracteres comunes. Luego, purga el diccionario de palabras que no contienen todos los caracteres comunes y verifica la victoria llamando a comprobarVictoria.
elegirNuevaPalabra(PalabraUsuario, Diccionario, ArrayComun, NuevaPalabra) :-
    % Verificar que PalabraUsuario esté instanciada
    nonvar(PalabraUsuario),
    atom_length(PalabraUsuario, Longitud),

    % Elegir palabra de la misma longitud con caracteres comunes
    palabraDeLongitudConComunes(Longitud, Diccionario, ArrayComun, NuevaPalabra),
    
    % Purgar el diccionario
    purgarDiccionario(NuevaPalabra, PalabraUsuario, Diccionario, DiccionarioFiltrado),
    
    % Elegir una palabra aleatoria de las candidatas
    member(NuevaPalabra, DiccionarioFiltrado),    
    comprobarVictoria(PalabraUsuario, NuevaPalabra).

% Purgar el diccionario de palabras que no contienen todos los caracteres comunes
% Parámetros: NuevaPalabra (palabra seleccionada aleatoriamente), PalabraUsuario (palabra introducida por el usuario), Diccionario (lista de palabras disponibles), DiccionarioFiltrado (lista de palabras purgadas)
% Descripción: Este predicado purga el diccionario de palabras que no contienen todos los caracteres comunes con la palabra seleccionada aleatoriamente y la palabra introducida por el usuario.
purgarDiccionario(NuevaPalabra, PalabraUsuario, Diccionario, DiccionarioFiltrado) :-
    string_chars(PalabraUsuario, CharsUsuario),
    string_chars(NuevaPalabra, CharsAleatoria),
    
    writeln('Nuevo turno! ------------------------------'),
    write('Palabra elegida: '),writeln(NuevaPalabra),
    
    subtract(CharsUsuario, CharsAleatoria, RestoCaracteres),
   
    write('Diferencia de caracteres: '), writeln(RestoCaracteres),
    
    % Filtrar palabras candidatas que contengan todos los caracteres comunes
    include(contieneTodosCaracteres(RestoCaracteres), Diccionario, DiccionarioFiltrado).

% Verificar si una palabra contiene todos los caracteres de la lista dada
% Parámetros: RestoCaracteres (lista de caracteres a verificar), Palabra (palabra a verificar)
% Descripción: Este predicado verifica si una palabra contiene todos los caracteres de la lista RestoCaracteres.
contieneTodosCaracteres(RestoCaracteres, Palabra) :-
    string_chars(Palabra, Chars),
    intersection(Chars, RestoCaracteres, RestoCaracteres).

% Repetir el juego hasta que la palabra elegida por Prolog sea igual a la introducida por el usuario
% Parámetros: PalabraUsuario (palabra introducida por el usuario)
% Descripción: Este predicado repite el juego llamando al predicado elegirNuevaPalabra hasta que la palabra elegida por Prolog sea igual a la introducida por el usuario. Verifica la victoria llamando a comprobarVictoria después de cada elección de palabra.
repetirJuego(PalabraUsuario) :-
    diccionario_actual(Diccionario), % Obtener el diccionario actualizado
    elegirNuevaPalabra(PalabraUsuario, Diccionario, _, NuevaPalabra),
    comprobarVictoria(PalabraUsuario, NuevaPalabra).

% Comprobar victoria al elegir la primera palabra
% Parámetros: PalabraUsuario (palabra introducida por el usuario), PalabraElegida (palabra seleccionada aleatoriamente)
% Descripción: Este predicado compara la palabra introducida por el usuario con la palabra seleccionada aleatoriamente. Si son iguales, imprime un mensaje de felicitación y reinicia el juego llamando a jugar.
comprobarVictoria(PalabraUsuario, PalabraElegida) :-
    compararPalabras(PalabraUsuario, PalabraElegida, _),
    writeln('¡Felicidades, has ganado!'),
    writeln('Volviendo al principio...'),
    jugar. % Llamada recursiva para reiniciar el juego

% Definir un predicado para obtener palabras de una longitud dada y con caracteres comunes
% Parámetros: Longitud (longitud de la palabra), Diccionario (lista de palabras disponibles), ArrayComun (lista de caracteres comunes), Palabra (palabra seleccionada)
% Descripción: Este predicado selecciona una palabra del diccionario con la longitud dada y que comparta caracteres comunes con la lista ArrayComun.
palabraDeLongitudConComunes(Longitud, Diccionario, ArrayComun, Palabra) :-
    member(Palabra, Diccionario),
    atom_length(Palabra, Longitud),
    string_chars(Palabra, Chars),
    intersection(Chars, ArrayComun, _). % Verificar que tenga caracteres comunes

% Diccionario de palabras (puedes extenderlo según sea necesario)
% Parámetros: Ninguno
% Descripción: Este es el diccionario de palabras utilizado en el juego. Puedes extenderlo agregando más palabras según sea necesario.
dictionary([
    'casa', 'perro', 'gato', 'mesa', 'silla', 'coche', 'playa', 'libro', 'agua', 'sol',
    'oso', 'robot', 'nube', 'cielo', 'luz', 'radio', 'hoja', 'puente', 'nido', 'techo',
    'roca', 'fuego', 'llama', 'abeja', 'viento', 'flor', 'tren', 'nieve', 'ciudad', 'planta',
    'tigre', 'mariposa', 'río', 'globo', 'relámpago', 'niebla', 'fruta', 'tierra', 'campo', 'arcoiris',
    'diente', 'sombrero', 'escalera', 'pájaro', 'delfín', 'manzana', 'teléfono', 'tormenta', 'reloj', 'camino'
]).

% Iniciar el juego
% Parámetros: Ninguno
% Descripción: Este predicado inicia el juego llamando al predicado jugar.
:- jugar.
