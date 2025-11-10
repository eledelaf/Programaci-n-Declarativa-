% Hoja 3 programación declarativa 

progenitor(angel,elena).
progenitor(maite,elena).

progenitor(angel1,angel).
progenitor(teresa,angel).
progenitor(angel1,baby).
progenitor(teresa,baby).
progenitor(angel1,ramon). 
progenitor(teresa,ramon).

progenitor(ramon,ramonin).

hombre(angel).
hombre(angel1).
hombre(ramon).

mujer(maite).
mujer(teresa).

% X es padre de Y
padre(X,Y):- 
    progenitor(X,Y), 
    hombre(X).

% X es madre de Y
madre(X,Y):-
    progenitor(X,Y), 
    mujer(X).

% X e Y son hermanos/as
hermanos(X,Y):-
    madre(Z,X),
    madre(Z,Y),
    padre(T,X),
    padre(T,Y).

%X es el tio de Y 
tio(X,Y) :-
    hermanos(X,Z),
    progenitor(Z,Y).

%X e Y son primos 
primos(X,Y):-
    progenitor(Z,X),
    tio(Z,Y).
