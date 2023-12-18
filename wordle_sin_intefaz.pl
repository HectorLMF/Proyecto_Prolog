% Predicado principal para iniciar el juego
jugar :-
    write('Introduce una palabra: '),
    read(UsuarioPalabra),
    dictionary(D),
    jugarConDiccionario(UsuarioPalabra, D).

% Jugar con un diccionario dado
jugarConDiccionario(UsuarioPalabra, Diccionario) :-
    length(Diccionario, LongitudDiccionario),
    LongitudDiccionario > 0, % Verificar que haya palabras disponibles en el diccionario
    repetirJuego(UsuarioPalabra, Diccionario).

% Si el diccionario está vacío, imprimir mensaje de finalización
jugarConDiccionario(_, _) :-
    writeln('¡Se acabaron las palabras en el diccionario! El juego ha terminado.').

% Comparar si las palabras son iguales y generar array de caracteres comunes
compararPalabras(PalabraUsuario, PalabraAleatoria, ArrayComun) :-
    writeln("Comparando palabras:"),
    writeln(PalabraUsuario = PalabraAleatoria),
    generarArrayComun(PalabraUsuario, PalabraAleatoria, ArrayComun),
    PalabraUsuario == PalabraAleatoria.

% Generar un array con los caracteres comunes entre dos palabras
generarArrayComun(PalabraUsuario, PalabraAleatoria, ArrayComun) :-
    string_chars(PalabraUsuario, CharsUsuario),
    string_chars(PalabraAleatoria, CharsAleatoria),
    intersection(CharsUsuario, CharsAleatoria, ArrayComun),
    writeln('Array de caracteres comunes: '), writeln(ArrayComun).

% Elegir una palabra aleatoria de la misma longitud y con caracteres comunes
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
% Repetir el juego hasta que la palabra elegida por Prolog sea igual a la introducida por el usuario
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
comprobarVictoria(PalabraUsuario, PalabraElegida) :-
    compararPalabras(PalabraUsuario, PalabraElegida, _),
    writeln('¡Felicidades, has ganado!'),
    writeln('Volviendo al principio...'),
    jugar. % Llamada recursiva para reiniciar el juego

% Definir un predicado para obtener palabras de una longitud dada y con caracteres comunes
palabraDeLongitudConComunes(Longitud, Diccionario, ArrayComun, Palabra) :-
    writeln('Palabra de longitud: '), writeln(Longitud),
    writeln('Diccionario: '), writeln(Diccionario),
    member(Palabra, Diccionario),
    atom_length(Palabra, Longitud),
    string_chars(Palabra, Chars),
    intersection(Chars, ArrayComun, _). % Verificar que tenga caracteres comunes

% Diccionario de palabras (puedes extenderlo según sea necesario)
dictionary(['casa', 'perro', 'gato', 'mesa', 'silla', 'coche', 'playa', 'libro', 'agua', 'sol']).

% Iniciar el juego
:- jugar.
