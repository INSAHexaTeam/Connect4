% Fichier : gestion/joueurs.pl

% Alterner les joueurs
changer_joueur('X', 'O').
changer_joueur('O', 'X').

% Demander une colonne
demander_colonne(Joueur, Colonne) :-
    format("Joueur ~w, choisissez une colonne (1-7) : ", [Joueur]),
    catch(read(Input), _, Input = invalide),
    (integer(Input), between(1, 7, Input) -> Colonne = Input;
        writeln("Entrée invalide, veuillez choisir un numéro entre 1 et 7."),
        demander_colonne(Joueur, Colonne)
    ).

% Ajouter un pion dans une colonne
ajouter_pion(Plateau, Colonne, Joueur, NouveauPlateau) :-
    nth1(Colonne, Plateau, ListeColonne),
    length(ListeColonne, Taille), Taille < 6,
    append(ListeColonne, [Joueur], NouvelleColonne),
    remplacer_colonne(Plateau, Colonne, NouvelleColonne, NouveauPlateau).

% Remplacer une colonne dans le plateau
remplacer_colonne([_|T], 1, NouvelleColonne, [NouvelleColonne|T]).
remplacer_colonne([H|T], Colonne, NouvelleColonne, [H|R]) :-
    Colonne > 1,
    Colonne1 is Colonne - 1,
    remplacer_colonne(T, Colonne1, NouvelleColonne, R).
