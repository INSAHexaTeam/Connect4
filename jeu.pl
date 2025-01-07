% Fichier principal : jeu.pl
:- consult('gestion/affichage.pl').

% Plateau initial
plateau([[], [], [], [], [], [], []]).

% Démarrer le jeu
jouer :-
    plateau(Plateau),
    jouer_tour(Plateau, 'X').

% Gérer un tour de jeu
jouer_tour(Plateau, Joueur) :-
    afficher_plateau(Plateau),
    format("Joueur ~w, choisissez une colonne (1-7) : ", [Joueur]),
    catch(read(Colonne), _, Colonne = invalide), % Lire l'entrée et gérer les erreurs
    (integer(Colonne), Colonne >= 1, Colonne =< 7 -> % Vérifier que l'entrée est valide
        (ajouter_pion(Plateau, Colonne, Joueur, NouveauPlateau) ->
            (gagner(NouveauPlateau, Joueur) ->
                afficher_plateau(NouveauPlateau),
                format("Le joueur ~w a gagné !", [Joueur])
            ;
                changer_joueur(Joueur, ProchainJoueur),
                jouer_tour(NouveauPlateau, ProchainJoueur)
            )
        ;
            writeln("Colonne invalide ou pleine, réessayez."),
            jouer_tour(Plateau, Joueur)
        )
    ;
        writeln("Entrée invalide, veuillez choisir un numéro de colonne entre 1 et 7."),
        jouer_tour(Plateau, Joueur)
    ).

% Alterner les joueurs
changer_joueur('X', 'O').
changer_joueur('O', 'X').

% Ajouter un pion dans une colonne
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

% Vérifier si un joueur a gagné
gagner(Plateau, Joueur) :-
    ligne_victoire(Plateau, Joueur);
    colonne_victoire(Plateau, Joueur);
    diagonale_victoire(Plateau, Joueur).

% Vérifier la victoire sur une ligne
ligne_victoire(Plateau, Joueur) :-
    transpose(Plateau, Lignes), % On travaille sur les lignes
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
    findall(Diagonale, diagonale_gauche_droite(Plateau, Diagonale), Diagonales).

% Diagonales de droite à gauche
diagonales_droite_gauche(Plateau, Diagonales) :-
    findall(Diagonale, diagonale_droite_gauche(Plateau, Diagonale), Diagonales).

% Calculer une diagonale de gauche à droite
diagonale_gauche_droite(Plateau, Diagonale) :-
    diagonale_aux(Plateau, 1, Diagonale).

% Calculer une diagonale de droite à gauche
diagonale_droite_gauche(Plateau, Diagonale) :-
    reverse(Plateau, PlateauInverse),
    diagonale_aux(PlateauInverse, 1, Diagonale).

% Récupérer les diagonale 
diagonale_aux([], _, []).
diagonale_aux([Col|Cols], N, [Elem|Diagonale]) :-
    nth1(N, Col, Elem), % Récupère l
    N1 is N + 1,
    diagonale_aux(Cols, N1, Diagonale).
diagonale_aux([_|Cols], N, Diagonale) :-
    diagonale_aux(Cols, N, Diagonale).

% Extraire une sous-liste pour vérifier une victoire
sous_liste(SousListe, Liste) :-
    append(_, Suffixe, Liste),
    append(SousListe, _, Suffixe).
