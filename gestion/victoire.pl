% Fichier : gestion/victoire.pl

% Vérifier si un joueur a gagné
verifier_victoire(Plateau, Joueur) :-
    (ligne_victoire(Plateau, Joueur) ->
    ;
    colonne_victoire(Plateau, Joueur) ->
    ;
    diagonale_victoire(Plateau, Joueur) ->
    ).
    (ligne_victoire(Plateau, Joueur) ->
        writeln('Victoire par ligne')
    ;
    colonne_victoire(Plateau, Joueur) ->
        writeln('Victoire par colonne')
    ;
    diagonale_victoire(Plateau, Joueur) ->
        writeln('Victoire par diagonale')
    ).

% Vérifier la victoire sur une ligne
ligne_victoire(Plateau, Joueur) :-
    transpose(Plateau, Lignes),
    member(Ligne, Lignes),
    sous_liste([Joueur, Joueur, Joueur, Joueur], Ligne).

% Vérifier la victoire sur une colonne
colonne_victoire(Plateau, Joueur) :-
    member(Colonne, Plateau),
    sous_liste([Joueur, Joueur, Joueur, Joueur], Colonne).

% Vérifier la victoire en diagonale
diagonale_victoire(Plateau, Joueur) :-
    diagonales(Plateau, Diagonales),
    member(Diagonale, Diagonales),
    sous_liste([Joueur, Joueur, Joueur, Joueur], Diagonale).

% Trouver toutes les diagonales possibles
diagonales(Plateau, Diagonales) :-
    diagonales_gauche_droite(Plateau, DG),
    diagonales_droite_gauche(Plateau, DD),
    append(DG, DD, Diagonales).

% Diagonales de gauche à droite
diagonales_gauche_droite(Plateau, Diagonales) :-
    findall(Diagonale, (between(1, 6, N), diagonale_gauche_droite(Plateau, N, Diagonale)), Diagonales).
    findall(Diagonale, (between(1, 6, N), diagonale_gauche_droite(Plateau, N, Diagonale)), Diagonales).

% Diagonales de droite à gauche
diagonales_droite_gauche(Plateau, Diagonales) :-
    findall(Diagonale, (between(1, 6, N), diagonale_droite_gauche(Plateau, N, Diagonale)), Diagonales).
    findall(Diagonale, (between(1, 6, N), diagonale_droite_gauche(Plateau, N, Diagonale)), Diagonales).

% Calculer une diagonale de gauche à droite
diagonale_gauche_droite(Plateau, N, Diagonale) :-
    diagonale_aux(Plateau, N, 1, Diagonale).
diagonale_gauche_droite(Plateau, N, Diagonale) :-
    diagonale_aux(Plateau, N, 1, Diagonale).

% Calculer une diagonale de droite à gauche
diagonale_droite_gauche(Plateau, N, Diagonale) :-
    reverse(Plateau, PlateauInverse),
    diagonale_aux(PlateauInverse, N, 1, Diagonale).

% Extraire les diagonales
diagonale_aux(_, _, 7, []) :- !.
diagonale_aux([], _, _, []) :- !.
diagonale_aux([Col|Cols], N, M, [Elem|Diagonale]) :-
    nth1(N, Col, Elem, _),
diagonale_aux(_, _, 7, []) :- !.
diagonale_aux([], _, _, []) :- !.
diagonale_aux([Col|Cols], N, M, [Elem|Diagonale]) :-
    nth1(N, Col, Elem, _),
    N1 is N + 1,
    M1 is M + 1,
    diagonale_aux(Cols, N1, M1, Diagonale).
diagonale_aux([_|Cols], N, M, Diagonale) :-
    diagonale_aux(Cols, N, M, Diagonale).
    M1 is M + 1,
    diagonale_aux(Cols, N1, M1, Diagonale).
diagonale_aux([_|Cols], N, M, Diagonale) :-
    diagonale_aux(Cols, N, M, Diagonale).

% Vérifier une sous-liste
sous_liste(SousListe, Liste) :-
    append(_, Suffixe, Liste),
    append(SousListe, _, Suffixe).