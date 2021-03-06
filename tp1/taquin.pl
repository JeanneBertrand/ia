%:- lib(listut).       % Placer cette directive en commentaire si vous utilisez Swi-Prolog 
   
                      % Sinon ne pas modifier si vous utilisez ECLiPSe Prolog :
                      % -> permet de disposer du predicat nth1(N, List, E)
                      % -> permet de disposer du predicat sumlist(List, S)
                      % (qui sont predefinis en Swi-Prolog)

                      
%***************************
%DESCRIPTION DU JEU DU TAKIN
%***************************

   %********************
   % ETAT INITIAL DU JEU
   %********************   
   % format :  initial_state(+State) ou State est une matrice (liste de listes)
  



initial_state([ [b, h, c],       % C'EST L'EXEMPLE PRIS EN COURS
                [a, f, d],       % 
                [g,vide,e] ]).   % h1=4,   h2=5,   f*=5



initial_state_dix([ [b, c, d],
                [a,vide,g],
                [f, h, e]  ]). % h2=10 f*=10

% AUTRES EXEMPLES POUR LES TESTS DE  A*


initial_state_deux([ [ a, b, c],        
                [ g, h, d],
                [vide,f, e] ]). % h2=2, f*=2



		
initial_state_v([ [f, g, a],
                [h,vide,b],
                [d, c, e]  ]). % h2=16, f*=20
	
initial_state_t([ [e, f, g],
                [d,vide,h],
                [c, b, a]  ]). % h2=24, f*=30 

initial_state_imp([ [a, b, c],
                [g,vide,d],
               [h, f, e]]). % etat non connexe avec l'etat final (PAS DE SOLUTION)

initial_state_4x4([[ 2,3, 4,vide],
    [1,5, 6, 7],
    [10, 11,12,8],
    [9, 13, 14, 15]]).
 
initial_state_4x4_imp([[ 2,3,vide, 4],
    [1,5, 6, 7],
    [10, 11,12,8],
    [9, 13, 14, 15]]).

   %******************
   % ETAT FINAL DU JEU
   %******************
   % format :  final_state(+State) ou State est une matrice (liste de listes)
   

final_state_4x4([[1, 2, 3, 4],
    [5, 6, 7,8],
    [9, 10, 11,12],
    [13, 14, 15, vide]]).

final_state([[a, b,  c],
             [h,vide, d],
             [g, f,  e]]).


			 
/* pour récupérer toute les cases qui sont déjà a la bonne place (état final)
-initial_state(Ini), nth1(L,Ini,Ligne), nth1(C,Ligne, d).?-final_state(Fin), nth1(3,Fin,Ligne), nth1(2,Ligne,P)*/


   %********************
   % AFFICHAGE D'UN ETAT
   %********************
   % format :  write_state(?State) ou State est une liste de lignes a afficher

write_state([]).
write_state([Line|Rest]) :-
   writeln(Line),
   write_state(Rest).
   

%**********************************************
% REGLES DE DEPLACEMENT (up, down, left, right)             
%**********************************************
   % format :   rule(+Rule_Name, ?Rule_Cost, +Current_State, ?Next_State)

/* 
requete pour savoir toutes les situations accessibles depuis etat initial
 initial_state(Ini), findall(E, rule(_,1,Ini,E), List_Of_Terms).

couple [A,E] de situations pouvant résulter des actions A 
initial_state(Ini), findall([A,E], rule(A,1,Ini,E), List_Of_Terms).
*/
   
rule(up,   1, S1, S2) :-
   vertical_permutation(_X,vide,S1,S2).

rule(down, 1, S1, S2) :-
   vertical_permutation(vide,_X,S1,S2).

rule(left, 1, S1, S2) :-
   horizontal_permutation(_X,vide,S1,S2).

rule(right,1, S1, S2) :-
   horizontal_permutation(vide,_X,S1,S2).

   %***********************
   % Deplacement horizontal            
   %***********************
    % format :   horizontal_permutation(?Piece1,?Piece2,+Current_State, ?Next_State)
	
horizontal_permutation(X,Y,S1,S2) :-
   append(Above,[Line1|Rest], S1),
   exchange(X,Y,Line1,Line2),
   append(Above,[Line2|Rest], S2).

   %***********************************************
   % Echange de 2 objets consecutifs dans une liste             
   %***********************************************
   
exchange(X,Y,[X,Y|List], [Y,X|List]).
exchange(X,Y,[Z|List1],  [Z|List2] ):-
   exchange(X,Y,List1,List2).

   %*********************
   % Deplacement vertical            
   %*********************
   
vertical_permutation(X,Y,S1,S2) :-
   append(Above, [Line1,Line2|Below], S1), % decompose S1
   delete(N,X,Line1,Rest1),    % enleve X en position N a Line1,   donne Rest1
   delete(N,Y,Line2,Rest2),    % enleve Y en position N a Line2,   donne Rest2
   delete(N,Y,Line3,Rest1),    % insere Y en position N dans Rest1 donne Line3
   delete(N,X,Line4,Rest2),    % insere X en position N dans Rest2 donne Line4
   append(Above, [Line3,Line4|Below], S2). % recompose S2 

   %***********************************************************************
   % Retrait d'une occurrence X en position N dans une liste L (resultat R) 
   %***********************************************************************
   % use case 1 :   delete(?N,?X,+L,?R)
   % use case 2 :   delete(?N,?X,?L,+R)   
   
delete(1,X,[X|L], L).
delete(N,X,[Y|L], [Y|R]) :-
   delete(N1,X,L,R),
   N is N1 + 1.


   
   
   %*******************
   % PARTIE A COMPLETER
   %*******************
   
   %*******************************************************************
   % Coordonnees X(colonne),Y(Ligne) d'une piece P dans une situation U
   %*******************************************************************
	% format : coordonnees(?Coord, +Matrice, ?Element)
	% Définit la relation entre des coordonnees [Ligne, Colonne] et un element de la matrice
	/*
	Exemples
	
	?- coordonnees(Coord, [[a,b,c],[d,e,f]],  e).        % quelles sont les coordonnees de e ?
	Coord = [2,2]
	yes
	
	?- coordonnees([2,3], [[a,b,c],[d,e,f]],  P).        % qui a les coordonnees [2,3] ?
	P=f
	yes
	*/

	coordonnees([L,C], Mat, Elt) :-       
        nth1(L,Mat,Ligne), nth1(C,Ligne, Elt).
											 
   %*************
   % HEURISTIQUES
   %*************
   
heuristique(U,H) :-
    %heuristique1(U, H).  % au debut on utilise l'heuristique 1 
     heuristique2(U, H).  % ensuite utilisez plutot l'heuristique 2  
   

%****************
%HEURISTIQUE no 1
%****************
% Nombre de pieces mal placees dans l'etat courant U
% par rapport a l'etat final F


bienplace(X,M1,M2) :- 
    coordonnees([L1,C1],M1,X),
    coordonnees([L2,C2],M2,X),
    (L1==L2, C1==C2).

heuristique1(U, H) :- 
    final_state(Fin), 
    findall(X, bienplace(X,U,Fin), BienPlace),
    delete(BienPlace, vide, NewBienPlace),
    length(NewBienPlace, R),
    H is 8-R.
        
        
        
    
   
%****************
%HEURISTIQUE no 2
%****************

% Somme des distances de Manhattan à parcourir par chaque piece
% entre sa position courante et sa positon dans l'etat final


dist_manhattan(X,M1,M2,Res) :-
    coordonnees([L1,C1],M1,X),
    coordonnees([L2,C2],M2,X),
    X \== vide,
    Res is (abs(L2-L1) + abs(C2-C1)).

heuristique2(U, H) :-
    final_state(Fin), 
    findall(R,dist_manhattan(_,U,Fin,R),ResultList),
    sumlist(ResultList, H).
       
        

						





































			
									
