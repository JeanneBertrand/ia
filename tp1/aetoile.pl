%*******************************************************************************
%                                    AETOILE
%*******************************************************************************

/*
Rappels sur l'algorithme
 
- structures de donnees principales = 2 ensembles : P (etat pendants) et Q (etats clos)
- P est dedouble en 2 arbres binaires de recherche equilibres (AVL) : Pf et Pu
 
   Pf est l'ensemble des etats pendants (pending states), ordonnes selon
   f croissante (h croissante en cas d'egalite de f). Il permet de trouver
   rapidement le prochain etat a developper (celui qui a f(U) minimum).
   
   Pu est le meme ensemble mais ordonne lexicographiquement (selon la donnee de
   l'etat). Il permet de retrouver facilement n'importe quel etat pendant

   On gere les 2 ensembles de fa�on synchronisee : chaque fois qu'on modifie
   (ajout ou retrait d'un etat dans Pf) on fait la meme chose dans Pu.

   Q est l'ensemble des etats deja developpes. Comme Pu, il permet de retrouver
   facilement un etat par la donnee de sa situation.
   Q est modelise par un seul arbre binaire de recherche equilibre.

Predicat principal de l'algorithme :

   aetoile(Pf,Pu,Q)

   - reussit si Pf est vide ou bien contient un etat minimum terminal
   - sinon on prend un etat minimum U, on genere chaque successeur S et les valeurs g(S) et h(S)
	 et pour chacun
		si S appartient a Q, on l'oublie
		si S appartient a Ps (etat deja rencontre), on compare
			g(S)+h(S) avec la valeur deja calculee pour f(S)
			si g(S)+h(S) < f(S) on reclasse S dans Pf avec les nouvelles valeurs
				g et f 
			sinon on ne touche pas a Pf
		si S est entierement nouveau on l'insere dans Pf et dans Ps
	- appelle recursivement etoile avec les nouvelles valeurs NewPF, NewPs, NewQs

*/

%*******************************************************************************

:- ['avl.pl'].       % predicats pour gerer des arbres bin. de recherche   
:- ['taquin.pl'].    % predicats definissant le systeme a etudier

%*******************************************************************************

main:-
    initial_final_state(S0),
    heuristique(S0,H0),
    
	% initialisations Pf, Pu et Q 
/*
    Pf = avl(nil,[[H0,H0,0],S0],nil,0),
    Pu = avl(nil,[S0, [H0,H0,0],nil,nil],nil,0),
    Q = avl(nil,nil,nil,-1),*/

    empty(Q), empty(Pf), empty(Pu),
    insert([[H0,H0,0],S0],Pf,Pf2),
    insert([S0, [H0,H0,0],nil,nil], Pu, Pu2),
    aetoile(Pf2, Pu2, Q).

/*
	% lancement de Aetoile
  
    expand(S0, List,0), 
    write(List),
    put_flat(Pu),
    put_flat(Pf),
    write("coucou"),
    loop_successors(List, S0, Pu, Pf, NewPu, NewPf,Q),
    put_flat(Q),   
    put_flat(NewPu),
    put_flat(NewPf).
*/
%*******************************************************************************

aetoile(Pf,Pu,_) :- 
    empty(Pf), 
    empty(Pu),
    writeln("PAS DE SOLUTION : etat final non atteignable").
aetoile(_,Pu,Q) :- 
    final_state(Fin), 
    belongs([Fin,[F1,H1,G1],P, A], Pu),
    insert([Fin,[F1,H1,G1],P,A],Q,Q1),
    affiche_solution(Fin,Pu,Q1).

aetoile(Pf, Ps, Qs) :-
    %suppression de U
    write("pasfin\n"),
    suppress_min([Val, U], Pf, New_Pf), 
    suppress([U, Val, Pere, A], Ps, New_Pu),
    %d�veloppement de U
    Val = [_, _, Gu],
    expand(U, List_Succes, Gu),
    loop_successors(List_Succes,U, New_Pu, New_Pf, FinalPu, FinalPf,Qs),

    insert([U,Val, Pere, A], Qs, Q),
    aetoile(FinalPf, FinalPu, Q).



%*******************************************************************************

%affiche_solution([[_, _, nil, _]]).
/*affiche_solution(Q) :-
    put_flat(Q).   
    affiche_solution(Q),	
    R is [U, _,_, _],
	nl, write_state(U).*/



affiche_solution(U,_Pu,_Q) :-
    initial_state(U),
    !.

affiche_solution(U,Pu,Q) :-
    %writeln("SOLUTION"),
    not(initial_state(U)),
    belongs([U,[_,_,_],Pere,A],Q),
    affiche_solution(Pere,Pu,Q),
    write(A),
    write(" -> ").

expand(U,List, Gu) :-
    findall([[F,H,G],S, A], (rule(A,1,U,S), param(S, Gu, [F,H,G])), List).

param(S, Gu, [F,H,G]) :-
    heuristique(S, H), 
    G is (Gu +1) , 
    F is (G + H).

loop_successors([],_,Pu, Pf, Pu, Pf,_).
loop_successors([D|R],U, Pu, Pf, FinalPu, FinalPf,Q) :-
write("loop\n"),
    handle_succes(D,U, Pu, Pf, TmpPu, TmpPf,Q),    
    loop_successors(R,U, TmpPu, TmpPf, FinalPu, FinalPf,Q).


handle_succes([_, S, _],_, _, _, _, _, Q):-    
    belongs([S, _, _, _], Q),write("belongs\n").

handle_succes([[F,H,G], S,A], U, Pu, Pf, FinalPu, FinalPf,_) :-
    belongs([S,[F1, H1, G1], _, _], Pu),
    F < F1,
    write("plus petit\n"),
    suppress([S,[F1, H1, G1], _, _], Pu, NewPu),
    suppress([[F1, H1, G1],S], Pf, NewPf),

    insert([[F,H,G],S],NewPf,FinalPf),
    insert([S,[F,H,G],U,A],NewPu,FinalPu).

handle_succes([[F,H,G], S,A], U, Pu, Pf, FinalPu, FinalPf,_) :-
    insert([[F,H,G],S],Pf,FinalPf),
    insert([S,[F,H,G],U,A],Pu,FinalPu),
    write("insere\n").


















	

	
   
