:- [negamax].
:- [tictactoe].


/*Différentes situations tests*/
situation_nulle([[x,o,x], 
                [x,x,o],
                [o,x,o]]).

situation_gagnantex([[x,o,x], 
                    [_,x,o],
                    [o,x,x]]).

situation_gagnanteo([[o,_,_], 
                    [o,x,_],
                    [o,x,x]]).
                

/*Alignements de tests*/
alignements([[_,x,_],[o,_,_],[_,_,_],[_,o,x], [x,x,x],[o,o,o]]). 

/*Tests unitaires du prédicat possible(Ali, J) */
test_possible :-
    possible([_,x,_], x),
    not(possible([_,x,_], o)),

    not(possible([o,_,_], x)),
    possible([o,_,_], o), 

    possible([_,_,_], x),
    possible([_,_,_], o),

    not(possible([_,o,x], x)),
    not(possible([_,o,x], o)), 

    possible([x,x,x], x),
    not(possible([x,x,x], o)), 

    not(possible([o,o,o], x)),
    possible([o,o,o], o),
    
    write("test_possible is complete ! \n").
    

/*Tests unitaires du prédicat alignement_gagnant(Ali, J)*/
test_gagnant :-
    not(alignement_gagnant([_,x,_], x)),
    not(alignement_gagnant([_,x,_], o)),

    not(alignement_gagnant([o,_,_], x)),
    not(alignement_gagnant([o,_,_], o)), 

    not(alignement_gagnant([_,_,_], x)),
    not(alignement_gagnant([_,_,_], o)),

    not(alignement_gagnant([_,o,x], x)),
    not(alignement_gagnant([_,o,x], o)), 

    alignement_gagnant([x,x,x], x),
    not(alignement_gagnant([x,x,x], o)), 

    not(alignement_gagnant([o,o,o], x)),
    alignement_gagnant([o,o,o], o),
        
    write("test_gagnant is complete ! \n").

/*Tests unitaires du prédicat alignement_perdant(Ali, J)*/
test_perdant :- 
    not(alignement_perdant([_,x,_], x)),
    not(alignement_perdant([_,x,_], o)),

    not(alignement_perdant([o,_,_], x)),
    not(alignement_perdant([o,_,_], o)), 

    not(alignement_perdant([_,_,_], x)),
    not(alignement_perdant([_,_,_], o)),

    not(alignement_perdant([_,o,x], x)),
    not(alignement_perdant([_,o,x], o)), 

    not(alignement_perdant([x,x,x], x)),
    alignement_perdant([x,x,x], o), 

    alignement_perdant([o,o,o], x),
    not(alignement_perdant([o,o,o], o)),
        
    write("test_perdant is complete ! \n").

test_heuristique(H1, H2, H3, H4) :-
    situation_gagnanteo(SO),
    situation_gagnantex(SX),
    situation_nulle(SN), 
    heuristique(x, SX, H1),
    %H1 = 1000, 
    heuristique(o, SO, H2),
    %H2 = 1000, 
    heuristique(x, SN, H3),
    heuristique(o, SN, H4),
    
    write("test_heuristique is complete ! \n").


main :-
    alignements(Ali),
    write("alignements utilisés pour ces tests : \n"),
    write(Ali),
    write("\n"),
    test_possible,
    test_gagnant,
    test_perdant,
    test_heuristique.

