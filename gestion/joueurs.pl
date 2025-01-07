% Alterner les joueurs
changer_joueur('X', 'O').
changer_joueur('O', 'X').

% Demander une colonne
demander_colonne(Joueur, Colonne, TypeJoueur) :-
    (TypeJoueur = 'humain' ->
        format("Joueur ~w, choisissez une colonne (1-7) : ", [Joueur]),
        catch(read(Input), _, Input = invalide),
        (integer(Input), between(1, 7, Input) -> Colonne = Input;
            writeln("Entrée invalide, veuillez choisir un numéro entre 1 et 7."),
            demander_colonne(Joueur, Colonne, TypeJoueur)
        )
    ; TypeJoueur = 'ia' ->
        writeln("L'IA réfléchit..."),
        choisir_colonne_ia(Colonne),
        format("L'IA a choisi la colonne ~w.", [Colonne])
    ).


choisir_colonne_ia(Colonne) :-
    repeat,
    random(1, 8, Colonne),  % Choix aléatoire entre 1 et 7
    joueur_peut_jouer(Colonne),
    !.

% Vérifie si une colonne est valide pour jouer
joueur_peut_jouer(Colonne) :-
    plateau_actuel(Plateau),  % Récupère le plateau actuel
    nth1(Colonne, Plateau, ListeColonne),
    length(ListeColonne, Taille),
    Taille < 6. 


ajouter_pion(Plateau, Colonne, Joueur, NouveauPlateau) :-
    nth1(Colonne, Plateau, ListeColonne),
    length(ListeColonne, Taille), Taille < 6, 
    append(ListeColonne, [Joueur], NouvelleColonne),
    remplacer_colonne(Plateau, Colonne, NouvelleColonne, NouveauPlateau).


remplacer_colonne([_|T], 1, NouvelleColonne, [NouvelleColonne|T]).
remplacer_colonne([H|T], Colonne, NouvelleColonne, [H|R]) :-
    Colonne > 1,
    Colonne1 is Colonne - 1,
    remplacer_colonne(T, Colonne1, NouvelleColonne, R).
