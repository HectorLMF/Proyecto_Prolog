% Predicado principal para iniciar el juego
% Parámetros: Ninguno
% Funcionalidad: Solicita al usuario que introduzca una palabra y comienza el juego con el diccionario predeterminado.
jugar :-
    write('Introduce una palabra: '),
    read(UsuarioPalabra),
    dictionary(D),
    jugarConDiccionario(UsuarioPalabra, D).

% Jugar con un diccionario dado
% Parámetros: UsuarioPalabra (palabra introducida por el usuario), Diccionario (diccionario de palabras)
% Funcionalidad: Verifica que haya palabras disponibles en el diccionario y repite el juego con la palabra introducida por el usuario y el diccionario dado.

jugarConDiccionario(UsuarioPalabra, Diccionario) :-
    length(Diccionario, LongitudDiccionario),
    LongitudDiccionario > 0, % Verificar que haya palabras disponibles en el diccionario
    repetirJuego(UsuarioPalabra, Diccionario).

% Si el diccionario está vacío, imprimir mensaje de finalización
% Parámetros: Ninguno
% Funcionalidad: Imprime un mensaje indicando que se han agotado las palabras en el diccionario y reinicia el juego.
jugarConDiccionario(_, _) :-
    writeln('¡Se acabaron las palabras en el diccionario! El juego ha terminado.'),
	jugar.

% Comparar si las palabras son iguales y generar array de caracteres comunes
% Parámetros: PalabraUsuario (palabra introducida por el usuario), PalabraAleatoria (palabra elegida aleatoriamente), ArrayComun (array de caracteres comunes)
% Funcionalidad: Compara las palabras del usuario y aleatoria, genera un array con caracteres comunes y verifica si ambas palabras son iguales.
compararPalabras(PalabraUsuario, PalabraAleatoria, ArrayComun) :-
    writeln("Comparando palabras:"),
    writeln(PalabraUsuario = PalabraAleatoria),
    generarArrayComun(PalabraUsuario, PalabraAleatoria, ArrayComun),
    PalabraUsuario == PalabraAleatoria,
    writeln('Diccionario actualizado después de la purga: '), writeln(DiccionarioPurgado).

% Generar un array con los caracteres comunes entre dos palabras
% Parámetros: PalabraUsuario (palabra introducida por el usuario), PalabraAleatoria (palabra elegida aleatoriamente), ArrayComun (array de caracteres comunes)
% Funcionalidad: Convierte las palabras en arrays de caracteres y genera un array con los caracteres comunes entre ambas.

generarArrayComun(PalabraUsuario, PalabraAleatoria, ArrayComun) :-
    string_chars(PalabraUsuario, CharsUsuario),
    string_chars(PalabraAleatoria, CharsAleatoria),
    intersection(CharsUsuario, CharsAleatoria, ArrayComun),
    writeln('Array de caracteres comunes: '), writeln(ArrayComun).

% Elegir una palabra aleatoria de la misma longitud y con caracteres comunes
% Parámetros: PalabraUsuario (palabra introducida por el usuario), Diccionario (diccionario de palabras), ArrayComun (array de caracteres comunes), NuevaPalabra (palabra elegida aleatoriamente)
% Funcionalidad: Elige una nueva palabra aleatoria del diccionario con la misma longitud y caracteres comunes, y comprueba la victoria.
elegirNuevaPalabra(PalabraUsuario, Diccionario, ArrayComun, NuevaPalabra) :-
    writeln('Elegir nueva palabra...'),
    writeln('Palabra del usuario: '), writeln(PalabraUsuario),
    writeln('Diccionario: '), writeln(Diccionario),
    writeln('Array de caracteres comunes: '), writeln(ArrayComun),
    atom_length(PalabraUsuario, Longitud),
    palabraDeLongitudConComunes(Longitud, Diccionario, ArrayComun, NuevaPalabra),
    writeln('Palabra aleatoria elegida: '), writeln(NuevaPalabra),
    comprobarVictoria(PalabraUsuario, NuevaPalabra).

% Repetir el juego hasta que la palabra elegida por Prolog sea igual a la introducida por el usuario
% Parámetros: PalabraUsuario (palabra introducida por el usuario), Diccionario (diccionario de palabras)
% Funcionalidad: Repite el juego hasta que la palabra elegida por Prolog sea igual a la introducida por el usuario.
repetirJuego(PalabraUsuario, Diccionario) :-
    writeln('Repetir juego'),
    elegirNuevaPalabra(PalabraUsuario, Diccionario, _, NuevaPalabra),
    writeln('Palabra aleatoria: '), writeln(NuevaPalabra),
    compararPalabras(PalabraUsuario, NuevaPalabra, _),
    (PalabraUsuario \= NuevaPalabra ->
        writeln('Sigue intentándolo...'),
        jugarConDiccionario(PalabraUsuario, Diccionario)
    ).

% Comprobar victoria al elegir la primera palabra
% Parámetros: PalabraUsuario (palabra introducida por el usuario), PalabraElegida (palabra elegida aleatoriamente)
% Funcionalidad: Comprueba si la primera palabra elegida aleatoriamente es igual a la introducida por el usuario y reinicia el juego.
comprobarVictoria(PalabraUsuario, PalabraElegida) :-
    writeln('------------------Nuevo turno--------------'),
    compararPalabras(PalabraUsuario, PalabraElegida, _),
    writeln('¡Felicidades, has ganado!'),
    writeln('Volviendo al principio...'),
    jugar. % Llamada recursiva para reiniciar el juego

% Definir un predicado para obtener palabras de una longitud dada y con caracteres comunes
% Parámetros: Longitud (longitud deseada de la palabra), Diccionario (diccionario de palabras), ArrayComun (array de caracteres comunes), Palabra (palabra que cumple con los requisitos)
% Funcionalidad: Selecciona una palabra del diccionario con la longitud dada y caracteres comunes con la palabra del usuario.
palabraDeLongitudConComunes(Longitud, Diccionario, ArrayComun, Palabra) :-
    writeln('Palabra de longitud: '), writeln(Longitud),
    writeln('Diccionario: '), writeln(Diccionario),
    member(Palabra, Diccionario),
    atom_length(Palabra, Longitud),
    string_chars(Palabra, Chars),
    intersection(Chars, ArrayComun, _). % Verificar que tenga caracteres comunes

% Diccionario de palabras (puedes extenderlo según sea necesario)
% Parámetros: Ninguno
% Funcionalidad: Diccionario de palabras predeterminado para el juego.
dictionary(['casa', 'perro', 'gato', 'mesa', 'silla', 'coche', 'playa', 'libro', 'agua', 'sol']).

% Iniciar el juego
:- jugar.
