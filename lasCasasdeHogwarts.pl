
casa(slytherin).
casa(hufflepuff).
casa(griffyndor).
casa(ravenclaw).

sangre(harry, mestiza).
sangre(draco, pura).
sangre(hermione, impura).

mago(Mago):- sangre(Mago, _).

permiteEntrar(Casa, Mago):- mago(Mago),casa(Casa), Casa \= slytherin.
permiteEntrar(slytherin, Mago):- sangre(Mago, Sangre), Sangre \= impura.


caracteristicaBuscada(griffyndor, coraje).
caracteristicaBuscada(slytherin, orgullo).
caracteristicaBuscada(slytherin, inteligencia).
caracteristicaBuscada(ravenclaw, inteligencia).
caracteristicaBuscada(hufflepuff, amistad).

caracteristicas(harry, [coraje, amistad, orgullo, inteligencia]).
caracteristicas(draco, [inteligencia, orgullo]).
caracteristicas(hermione, [inteligencia, orgullo, responsabilidad]).


%tieneCaracterApropiado(Mago, Casa):- caracteristicas(Mago, Caracteristicas), casa(Casa), forall(caracteristicaBuscada(Casa, Caracteristica), member(Caracteristica, Caracteristicas)).

tieneCaracteristica(Mago, Caracteristica):- caracteristicas(Mago, Caracteristicas), 
                                            member(Caracteristica, Caracteristicas).

tieneCaracterApropiado(Mago, Casa):- casa(Casa), mago(Mago),
                            forall(caracteristicaBuscada(Casa, Caracteristica), 
                            tieneCaracteristica(Mago, Caracteristica)).

odiariaEntrar(harry, slytherin).
odiariaEntrar(draco, hufflepuff).

puedeQuedarSeleccionadoPara(Mago, Casa):- 
    tieneCaracterApropiado(Mago, Casa),
    permiteEntrar(Casa, Mago),
    not(odiariaEntrar(Mago, Casa)).
puedeQuedarSeleccionadoPara(hermione, griffyndor).

%cadenaDeAmistades(Magos):-
%   todos los magos son amistosos
%   y cada uno podria estar en la casa del siguente
 
 cadenaDeAmistades(Magos):-
    todosAmistosos(Magos),
    cadenaDeCasas(Magos).

todosAmistosos(Magos):- 
    forall(member(Mago, Magos), amistoso(Mago)).

amistoso(Mago):-
    tieneCaracteristica(Mago, amistad).

cadenaDeCasas([Mago1, Mago2 | MagosSiguientes]):-
    puedeQuedarSeleccionadoPara(Mago1, Casa),
    puedeQuedarSeleccionadoPara(Mago2, Casa),
    cadenaDeCasas([Mago2, MagosSiguientes]).
cadenaDeCasas([_]).
cadenaDeCasas([]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%ELIPSIS

hizo(harry, fueraDeCama).
hizo(hermione, irA(tercerPiso)).
hizo(hermione, irA(seccionRestringida)).
hizo(harry, irA(bosque)).
hizo(harry, irA(tercerPiso)).
hizo(draco, irA(mazmorras)).
hizo(ron, buenaAccion(50, ganarAlAjedrezMagico)).
hizo(hermione, buenaAccion(50, salvarASusAmigos)).
hizo(harry, buenaAccion(60, ganarleAVoldemort)).
hizo(cedric, buenaAccion(100, ganarAlQuidditch)).

hizoAlgunaAccion(Mago):-
  hizo(Mago, _).
hizoAlgoMalo(Mago):-
  hizo(Mago, Accion),
  puntajeQueGenera(Accion, Puntaje),
  Puntaje < 0.

puntajeQueGenera(fueraDeCama, -50).
puntajeQueGenera(irA(Lugar), PuntajeQueResta):-
  lugarProhibido(Lugar, Puntos),
  PuntajeQueResta is Puntos * -1.
puntajeQueGenera(buenaAccion(Puntaje, _), Puntaje).

lugarProhibido(bosque, 50).
lugarProhibido(seccionRestringida, 10).
lugarProhibido(tercerPiso, 75).

esBuenAlumno(Mago):-
  hizoAlgunaAccion(Mago),
  not(hizoAlgoMalo(Mago)).
%% 1b

% hizo(Mago, Accion).

esRecurrente(Accion):-
  hizo(Mago, Accion),
  hizo(OtroMago, Accion),
  Mago \= OtroMago.

% 2

esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).
esDe(cedric, hufflepuff).

puntajeTotalDeCasa(Casa, PuntajeTotal):-
  esDe(_, Casa),
  findall(Puntos,
    (esDe(Mago, Casa), puntosQueObtuvo(Mago, _, Puntos)),
    ListaPuntos),
  sum_list(ListaPuntos, PuntajeTotal).

puntosQueObtuvo(Mago, Accion, Puntos):-
  hizo(Mago, Accion),
  puntajeQueGenera(Accion, Puntos).

% 3

casaGanadora(Casa):-
  puntajeTotalDeCasa(Casa, PuntajeMayor),
  forall((puntajeTotalDeCasa(OtraCasa, PuntajeMenor), Casa \= OtraCasa),
         PuntajeMayor > PuntajeMenor).

casaGanadora2(Casa):-
  puntajeTotalDeCasa(Casa, PuntajeMayor),
  not((puntajeTotalDeCasa(_, OtroPuntaje), OtroPuntaje > PuntajeMayor)).