  
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

% POUR TESTER LE PROGRAMME UTILISER LE PREDICAT MAIN EN CHOISISSANT UNE SITUATION initiale

main :-
    statistics(runtime, [Start1,_]),
    

    %situations de depart (3x3) a tester :


    %initial_state_deux(S0),
    initial_state_dix(S0),
    %initial_state_v(S0),
    %initial_state_t(S0),
    %initial_state_imp(S0),

    %situations de depart (4x4) a tester :
    %ATTENTION pour tester en 4x4 il faut aussi changer le final_state dans le predicat aetoile (deuxieme clause)

    %initial_state_4x4(S0),
    
    launch(S0),

    statistics(runtime, [Stop1,_]),
    Runtime1 is Stop1 - Start1, % resultat en ms
    write("\n execution time : "),
    write(Runtime1),
    write(" ms \n").


launch(S0):-
    heuristique(S0,H0),
    
    % initialisations Pf, Pu et Q 

    G0 is 0,
    F0 is H0+G0,
    empty(Q), empty(Pf), empty(Pu),

    insert([[F0,H0,G0],S0],Pf,Pf2),
    insert([S0, [F0,H0,G0],nil,nil], Pu, Pu2),
    
    % lancement de Aetoile
    aetoile(Pf2, Pu2, Q,S0).


  
  

%*******************************************************************************
aetoile(Pf,Pu,_,_) :- 
    empty(Pf), 
    empty(Pu),
    writeln("PAS DE SOLUTION : etat final non atteignable").

aetoile(_,Pu,Q,Ini) :- 

    %Pour passer en 4x4 decommenter la ligne suivante et commentez l'autre final_state:
   % final_state_4x4(Fin), 

    final_state(Fin),
    belongs([Fin,[F1,H1,G1],P, A], Pu),
    insert([Fin,[F1,H1,G1],P,A],Q,Q1),
    write("solution : "),
    affiche_solution(Fin,Q1,Ini).


aetoile(Pf, Pu, Qs,Ini) :-
    
    suppress_min([[FU, HU, Gu], U], Pf, New_Pf),
    suppress([U, [FU, HU, Gu], Pere, A], Pu, New_Pu),
    

    %d�veloppement de U
    expand(U, List_Succes, Gu),
  
    loop_successors(List_Succes, U, New_Pu, New_Pf, FinalPu, FinalPf,Qs),

    insert([U,[FU, HU, Gu], Pere, A], Qs, Q),
    aetoile(FinalPf, FinalPu, Q,Ini).



%*******************************************************************************
%*************************************************
%   Affichage de la solution : affiche_solution
%*************************************************

% lorsque on a remmonté tout l'arbre Q jusqu'a la situation initiale : fin
affiche_solution(Ini,_Q,Ini) :-
    !.

affiche_solution(U,Q,Ini) :-
    belongs([U,[_,_,_],Pere,A],Q),
    affiche_solution(Pere,Q,Ini),
    write(A),
    write(" -> ").


%************************************************************
%   Determination des successeurs d'une situation et de leur
%   évalution : expand 
%************************************************************
expand(U,List, Gu) :-
    findall([S,[F,H,G],U, A], (rule(A,1,U,S), param(S, Gu, [F,H,G])), List).

param(S, Gu, [F,H,G]) :-
    heuristique(S, H), 
    G is (Gu +1) , 
    F is (G + H).


test_expand :-
  initial_state(S0),

  write("Situation initiale : \n"),
  write(S0),

  expand(S0, List_Succes, 0),

  write("\n Liste de successeurs possibles : \n"),
  write(List_Succes).






%*********************************************************
%   Traitement des noeuds successeurs : loop_successors (recursif)
%*********************************************************

%condition d'arret, lorsque tout les successeurs ont étés traités
loop_successors([],_,Pu, Pf, FinalPu, FinalPf,_):-
   FinalPu = Pu,
   FinalPf = Pf.

loop_successors([[S,[Fs,Hs,Gs],U,A]|R],U, Pu, Pf, FinalPu, FinalPf,Q) :-

    %si la situation appartient déjà a Q, alors on l'ignore (déjà développé)
    (belongs([S,_,_,_],Q)->
        Puaux = Pu,
        Pfaux = Pf
    ;
        %si S est contenu dans Pu alors on garde sa meilleure évalution
        (suppress([S,[Fs1,Hs1,Gs1],_,_],Pu,Putemp)->
            ([Fs,Hs,Gs] @< [Fs1,Hs1,Gs1] ->
                suppress([[Fs1,Hs1,Gs1],S],Pf,Pftemp),
                insert([[Fs,Hs,Gs],S],Pftemp,Pfaux),
                insert([S,[Fs,Hs,Gs],U,A],Putemp,Puaux)   
            ; %else
            Puaux = Pu,
            Pfaux = Pf
            )
        ; 
        %sinon, on insere un nouveau terme dans Pu et Pf
        insert([[Fs,Hs,Gs],S],Pf,Pfaux),
        insert([S,[Fs,Hs,Gs],U,A],Pu,Puaux)
        )
    ),  
    loop_successors(R,U, Puaux, Pfaux, FinalPu, FinalPf,Q).



