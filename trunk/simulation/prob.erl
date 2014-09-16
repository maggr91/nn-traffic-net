-module(prob).

-define(Euler, 2.7182818284590452353602874713527).
%-compile(export_all).
-export ([poisson/2,start/1, reply/2, valorExponencial/1,fixedTable/0]).

-export ([init/1]).



%%INICIO DEL PROCESO
start(Args) ->
    spawn(prob,init, [Args]).
    
init({Distribution, ProbData}) ->
    {A1,A2,A3} = now(), 
    random:seed(A1, A2, A3),
    io:format("Invocando prob para poisson: ~p~n",[{Distribution, ProbData}]),
    case Distribution of
        poisson    -> serverTabla(tablaAcumulada(tablaPoisson(ProbData)));
        geometrica -> serverTabla(tablaAcumulada(tablaGeometrica(ProbData)))
    end.

reply (Pid, Reply) ->
    Pid ! {reply, Reply}.
	
reply (ReplyKey, Pid, Reply) ->
    Pid ! {ReplyKey, Reply}.


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

tablaGeometrica(P) -> lists:reverse(tablaGeometrica(1-P,[{0,P}],15+10*round(-1*math:log10(P)), 1)).

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


%%  ____   _____          _____  ___
%% |      |      |\   |  |      |   |     /\    |
%% |  __  |__    | \  |  |___   |___|    /__\   |
%% |    | |      |  \ |  |      |  \    /    \  |
%% |____| |_____ |   \|  |_____ |   \  /      \ |____
%%

%Entradas: Tablas de distribucion 
%Salidas: Tabla acumulada
%Restricciones: Las tuplas de la tabla de entrada debe de ser de la forma {Valor,P(Valor)}
tablaAcumulada(TablaComun)->tablaAcumulada([],TablaComun).
tablaAcumulada([{MaxVal,PMV}|Resto],[])->lists:reverse([{MaxVal+1,1}|[{MaxVal,PMV}|Resto]]);
tablaAcumulada([], [Elem|Resto])->tablaAcumulada([Elem], Resto);
tablaAcumulada([{Val, PVal}|RestoAcum], [{ValEle,PValElem}|Resto])->
	tablaAcumulada([{ValEle,PValElem+PVal}|[{Val, PVal}|RestoAcum]], Resto).
	
valorExponencial(Media)->-1*Media*math:log(random:uniform()).

fixedTable() ->
    [random:uniform(60) || _ <- lists:seq(1, 1000)].

serverTabla(TablaAcum)->	
	receive
		{valor, Cliente} ->		    
		    %%io:format("Buscando valor... mi tabla era: ~p~n",[TablaAcum]),		    
		    %%reply(Cliente, valorTabla(TablaAcum)), 	
		    reply(Cliente, encuentraValorTabla(TablaAcum, random:uniform())),
		    serverTabla(TablaAcum);
		{valor, ClientPID, ClientID} ->
			reply(replyProb, ClientPID, encuentraValorTablaExt(TablaAcum, random:uniform(), ClientID)),
			serverTabla(TablaAcum);
		killyou -> 
		    %%io:format("Saliendo... mi tabla era: ~p~n",[TablaAcum]);
		    {normal, ok};
		X -> 
		    io:format("Llamado no esperado... ~p~n",[X]),
		    serverTabla(TablaAcum)
	end.

%valorTabla(TablaAcum) -> 
%	Valor = encuentraValorTabla(TablaAcum, random:uniform()),
%	case Valor of
%		no_found -> 
%			io:format("no se encontro el valor... buscando otro~n",[]),
%			valorTabla(TablaAcum);
%		_V -> Valor		    
%	end.
		    
encuentraValorTabla([], _ValAleatorio) -> no_found;
encuentraValorTabla([{X,Px}|_Resto], ValAleatorio) when ValAleatorio < Px ->X;
encuentraValorTabla([{_X,_Px}|Resto], ValAleatorio)->encuentraValorTabla(Resto,ValAleatorio).

encuentraValorTablaExt(TablaAcum, ValAleatorio, CLientID) ->
	encuentraValorTabla(TablaAcum, ValAleatorio).
	


get_raw_data(Distribution) ->
	FileDistNamePath = "/sources/" ++ atom_to_list(Distribution),
	FileRelPath = FileDistNamePath ++ ".txt",
	filemanager:get_data(FileRelPath).

	
