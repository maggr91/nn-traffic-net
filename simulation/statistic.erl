-module(statistic).

-define(Euler, 2.7182818284590452353602874713527).
-compile(export_all).
%-export ([poisson/2]).

poisson(X, Lambda) when X > -1 ->
    math:pow(?Euler, -Lambda) * math:pow(Lambda, X) / factorial(X);
    
poisson(0, _Lambda) -> 0 .
    
factorial(N) when N > 0 ->
    N * factorial (N - 1);
factorial(0) -> 1.


% ENTRADAS: Valor de lambda para Poisson
% SALIDAS: Lista de tuplas para cada de los valores entre cero y 3*lambda
% Restricciones: La entrada tieneq que ser un entero positivo
tablaPoisson(N)-> lists:reverse(tablaPoisson(N, 3*N, [], 0)).

tablaPoisson(Lambda, X, Tabla, X)->[{X, poisson(X,Lambda)}|Tabla];
tablaPoisson(Lambda, Tope, Tabla, X)->tablaPoisson(Lambda, Tope, [{X,poisson(X,Lambda)}|Tabla], X+1).


% ENTRADAS: Probabilidad de éxito
% SALIDAS: Tabla de probabilidades de la geométrica
% Restricciones: P debe de estar entre 0 y 1

tablaGeometrica(P) -> lists:reverse(tablaGeometrica(1-P,[{0,P}],10*round(-1*math:log10(P)), 1)).

tablaGeometrica(Q, [{X,Px}|Resto], N, N) -> [{N, Px * Q} |[{X,Px}|Resto] ];
tablaGeometrica(Q, [{X,Px}|Resto],N,X1) ->tablaGeometrica(Q,[{X1, Px * Q} |[{X,Px}|Resto]], N, X1+1).

%%
%% CASO: Pasarse a carril adyacente
%%

%%
%% CASO: Sigue directo o va a doblar, presencia de lluvia: uniforme con un alfa de consistencia
%%

%%
%% CASO: llegadas de los vehículos: Poisson
%%

%%
%% CASO: 
%%


%%  ____    _____          _____  ___
%% |       |      |\   |  |      |   |     /\    |
%% |  __   |__    | \  |  |___   |___|    /__\   |
%% |    |  |      |  \ |  |      |  \    /    \  |
%% |____|  |_____ |   \|  |_____ |   \  /      \ |____

%Entradas: Tablas de distribucion 
%Salidas: Tabla acumulada
%Restricciones: Las tuplas de la tabla de entrada debe de ser de la forma {Valor,P(Valor)}
tablaAcumulada(TablaComun)->tablaAcumulada([],TablaComun).
tablaAcumulada([{MaxVal,PMV}|Resto],[])->[{MaxVal+1,1}|[{MaxVal,PMV}|Resto]];
tablaAcumulada([], [Elem|Resto])->tablaAcumulada([Elem], Resto);
tablaAcumulada([{Val, PVal}|RestoAcum], [{ValEle,PValElem}|Resto])->
	tablaAcumulada([{ValEle,PValElem+PVal}|[{Val, PVal}|RestoAcum]], Resto).
