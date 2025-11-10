% Ejercicio1
longitud([], 0).
longitud([_|Xs], N+1) :- longitud(Xs, N).

% ¿Permite este predicado calcular la longitud de la lista?
% Si pero devuelve una suma: 0+1+1+1+1...
% ¿Qué solución se devuelve al ejecutar el objetivo longitud([a,b,c,d],X)?
% X = 0+1+1+1+1
% ¿Y el objetivo longitud(X,4)?
% Supongo que la idea es que devuelva una lista de longitud 4, 
% pero devuelve false.

% Ejercicio2

% Ejercicio3
% Recursivo
% Si es una lista de un solo elemento devuelve el elemento, 
% en caso contrario devuelve la función recursiva con la lista 
% sin el primer elemento.
ult([X],X).
ult([_|Xs],Y) :- ult(Xs,Y).

% Append 
% Devuelve el elemento final de la lista Xs.
ult1(Xs,Y) :- append(_,[Y],Xs).

% Ejercicio4:
% Append:
% Member: coge el primer elemento de la lista y mira si hay otro elemento 
% igual en el resto de la lista. Si eso no funciona devuelve la función 
% recursiva sin el primer elemento de la lista.
duplicado([X|Xs]) :- member(X,Xs).
duplicado([_|Xs]) :- duplicado(Xs).

% Ejercicio5:
% Supongo que es sin repeticiones
% insertar(X,Xs,Ys) ⇔ YseselresultadodeinsertarXenlalistaordenadaXs
insertar(X,[],[X]).
insertar(X,[Y|Xs],[X,Y|Xs]) :- X<Y.
insertar(X,[Y|Xs],[Y|Zs]) :- insertar(X,Xs,Zs).

ordIns([],[]).
ordIns([X|Xs],Ys) :- ordIns(Xs,Y1s),insertar(X,Y1s,Ys),!.

% Ejercicio 6:
repartir([],_,_).
repartir([X],[X|Xs],Ys) :- repartir([],Xs,Ys).
repartir([X,Y|Zs],[X|Xs],[Y|Ys]) :- repartir(Zs,Xs,Ys).

% Ejercicio 7:
mezclar([],[],_).
mezclar([],Xs,Xs).
mezclar(Xs,[],Xs).
mezclar([X|Xs],[Y|Ys],[X,Y|Zs]) :- mezclar(Xs,Ys,Zs).

% Algoritmo Mergesort

% Ejercicio8
% particion(P,Xs,Ls,Gs) ⇔ 
% Ls contiene los elementos de Xs que son menores o iguales que P y 
% Gs contiene los elementos de Xs mayores que P.
particion(_,[],[],[]).
particion(P, [X|Xs],[X|Ls],Gs) :- P>=X, particion(P,Xs,Ls,Gs).
particion(P,[X|Xs],Ls,[X|Gs]) :- P<X, particion(P,Xs,Ls,Gs).

conc([],B,B).
conc([X|D],B,[X|E]) :- conc(D,B,E).

conc3([],[],[],[]).
conc3(A,B,C,D) :- conc(A,B,D1), conc(D1,C,D).

quicksort([],[]). 
quicksort([X|Xs],Ys) :- particion(X, Xs, Me,Ma),quicksort(Me,Me1), quicksort(Ma,Ma1), conc3(Me1,[X],Ma1,Ys).


% Ejercicio9
% miembro_arb(X,T) ⇔ XesunelementodelárbolbinarioT
ArbolB =:= vacioB |nodoB(_, ArbolB,ArbolB).

% Arbol binario no es de busqueda.
% Compara X con todos los elementos del arbol, primero prueba en la rama Iz, y luego en la Dr.
miembroArb(X, nodoB(X,_,_)).
miembroArb(X, nodoB(_,Iz,_)) :- miembroArb(X, Iz).
mimebroArb(X, nodoB(_,_,Dr)) :- miembroArb(X, Dr).

% Arbol binario de busqueda.
% Compara X con las ramas del arból en función de si X es mayor o menor al nodo con el que se compara.
miembroArbb(X, nodoB(X,_,_)).
miembroArbb(X, nodoB(Y,Iz,_)) :- X<Y, miembroArbb(X,Iz).
miembroArbb(X, nodoB(Y,_,Dr)) :- X>Y, miembroArbb(X,Dr).

% Ejercicio10
% inorden(T,L) ⇔ L contiene los elementos del árbol binario T visitados en inorden.
% Si el arból es vacio devuelve la lista []. 
% Si el arból no es vacio, devuelve la función recursiva de las ramas Iz, Dr y concatena las listas, primero la Iz, nodo, Dr.
inorden(vacioB, []). 
inorden(nodoB(X,Iz,Dr),Xs) :- inorden(Iz,Is),inorden(Dr,Ds), conc3(Is,[X],Ds,Xs).

% Ejercicio11
% Casos base, si son dos arboles iguales, devuelve true.
% Los arboles son isomorfos si tienen el mismo nodo pero las ramas al reves.
% Caso recursivo: si alguno de las ramas es isomorfa a la otra.
isoArbol(nodoB(X,Iz,Dr),nodoB(X,Iz,Dr)).
isoArbol(nodoB(X,Iz,Dr),nodoB(X,Dr,Iz)).
isoArbol(nodoB(X,Iz,Dr),nodoB(X,Iz1,Dr1)) :- isoArbol(Iz,Iz1), isoArbol(Iz,Dr1).

% Ejercicio12
% camino(X,T,Ys) ⇔ Ys es una lista con los elementos del camino desde la raíz del árbol binario T hasta el nodo X.
% Asumo que es un arbol binario de busqueda.
% Caso base, cuando el elemento es igual al nodo, devuelve la lista con el elemento [X].
% Caso recursivo, Cuando X<nodo Y, devuelve la funcion recursiva de la rama Iz del arbol, analogo cuando X>nodo Y.

camino(X,nodoB(X,_,_),[X]).
camino(X,nodoB(Y,Iz,_),[Y|Xs]) :- X<Y, camino(X,Iz,Xs).
camino(X,nodoB(Y,_,Dr),[Y|Xs]) :- X>Y,camino(X,Dr,Xs).
