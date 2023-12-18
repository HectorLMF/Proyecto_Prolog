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
    repetirJuego(UsuarioPalabra, Diccionario),
    writeln('Diccionario actualizado: '), writeln(Diccionario). % Imprimir diccionario actualizado tras cada turno

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
    intersection(CharsUsuario, CharsAleatoria, ArrayComun).


% Elegir una palabra aleatoria de la misma longitud y con caracteres comunes
elegirNuevaPalabra(PalabraUsuario, Diccionario, ArrayComun, NuevaPalabra) :-
    writeln('Elegir nueva palabra...'),
    writeln('Palabra del usuario: '), writeln(PalabraUsuario),
    writeln('Diccionario: '), writeln(Diccionario),
    % Verificar que PalabraUsuario esté instanciada
    nonvar(PalabraUsuario),
    atom_length(PalabraUsuario, Longitud),
    palabraDeLongitudConComunes(Longitud, Diccionario, ArrayComun, NuevaPalabra),
    writeln('---------------nuevo turno------------------'),
    writeln('Palabra aleatoria elegida: '), writeln(NuevaPalabra),
    purgarDiccionario(NuevaPalabra, PalabraUsuario, Diccionario, DiccionarioFiltrado),
    write('Palabras candidatas: '),
    writeln(DiccionarioFiltrado),
    comprobarVictoria(PalabraUsuario, NuevaPalabra).
    



purgarDiccionario(NuevaPalabra, PalabraUsuario, Diccionario, DiccionarioFiltrado) :-
    string_chars(PalabraUsuario, CharsUsuario),
    string_chars(NuevaPalabra, CharsAleatoria),
    intersection(CharsUsuario, CharsAleatoria, ArrayComun),
    list_to_set(ArrayComun, ArrayComunSinRepetir),
    
    writeln('Vamos a purgar, como Stalin...'),
    write(NuevaPalabra), write(" =? "), writeln(PalabraUsuario),
    write('Array de caracteres comunes sin repeticiones: '), writeln(ArrayComunSinRepetir),

    % Filtrar el diccionario eliminando las palabras que no contienen al menos un carácter común
    include(tieneCaracterComun(ArrayComunSinRepetir), Diccionario, DiccionarioFiltrado),
    write('Palabras candidatas para el proximo turno: '), writeln(DiccionarioFiltrado).

% Predicado auxiliar para verificar si una palabra tiene al menos un carácter común
tieneCaracterComun(ArrayComun, Palabra) :-
    member(C, ArrayComun),
    sub_atom(Palabra, _, 1, _, C).



% Repetir el juego hasta que la palabra elegida por Prolog sea igual a la introducida por el usuario
repetirJuego(PalabraUsuario, Diccionario) :-
    writeln('Repetir juego'),
    elegirNuevaPalabra(PalabraUsuario, Diccionario, _, NuevaPalabra),
    writeln('Palabra aleatoria: '), writeln(NuevaPalabra),
    compararPalabras(PalabraUsuario, NuevaPalabra, _),
    writeln('Diccionario actualizado: '), writeln(Diccionario), % Imprimir diccionario actualizado tras cada turno
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
dictionary([
    'casa', 'perro', 'gato', 'mesa', 'silla', 'coche', 'playa', 'libro', 'agua', 'sol',
    'oso', 'robot', 'nube', 'cielo', 'luz', 'radio', 'hoja', 'puente', 'nido', 'techo',
    'roca', 'fuego', 'llama', 'abeja', 'viento', 'flor', 'tren', 'nieve', 'ciudad', 'planta',
    'tigre', 'mariposa', 'río', 'globo', 'relámpago', 'niebla', 'fruta', 'tierra', 'campo', 'arcoiris',
    'diente', 'sombrero', 'escalera', 'pájaro', 'delfín', 'manzana', 'teléfono', 'tormenta', 'reloj', 'camino'
]).

% Iniciar el juego
:- jugar.
