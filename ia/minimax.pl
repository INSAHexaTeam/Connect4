:- module(minimax, [choisir_colonne_minimax/2]).

:- use_module('../gestion/joueurs', [joueur_peut_jouer/1, changer_joueur/2]).
% Profondeur maximale pour la recherche Minimax
profondeur_max(4). % Ajustez la profondeur selon vos besoins


% Utilite, determine la valeur d'une position donnée sur le plateau
utilite(Plateau, Utilite) :-
    victoire(Plateau, 'X'), % Si le joueur 'x' gagne
    Utilite = -1, !.

utilite(Plateau, Utilite) :-
    victoire(Plateau, 'O'), % Si l'IA gagne
    Utilite = 1, !.

utilite(Plateau, Utilite) :-
    Utilite = 0. % Si la partie est nulle



% Cas de base (profondeur = 0) : choix aléatoire d'une colonne
% Profondeur : profondeur de recherche restante
% Plateau : état du plateau de jeu
% Joueur : joueur courant
% Position : colonne choisie
% Utilite : utilité de la position choisie
minimax(Profondeur, Plateau, Joueur, Position, Utilite) :-
    case_vide(Plateau, Position), % Vérifier que la colonne n'est pas pleine
    random(0, 7, Position), % Choisir une colonne aléatoire
    !.

% Cas général : recherche Minimax
minimax(Profondeur, Plateau, Joueur, Position, Utilite) :-
    NouvelleProfondeur is Profondeur + 1,
    coups_possibles(Plateau, ListeCoups), % Lister les coups possibles
    !,
    meilleur_coup(NouvelleProfondeur, Plateau, Joueur, ListeCoups, Position, Utilite). % Déterminer le meilleur coup possible pour l'IA
    !.


% Si aucun coup n'est possible, la valeur minimax est l'utilité de la position actuelle
minimax(Profondeur, Plateau, Joueur, Position, Utilite) :-
    utilite(Plateau, Utilite).


% Déterminer le meilleur coup possible pour l'IA
meilleur_coup(Profondeur, Plateau, Joueur, [Coups|ListeCoups], Position, Utilite) :-
    appliquer_coup(Plateau, Coups, Joueur, NouveauPlateau), % Appliquer le coup
    changer_joueur(Joueur, NouveauJoueur), % Changer de joueur
    !, % Arrêter la recherche si un coup gagnant est trouvé
    minimax(Profondeur, NouveauPlateau, NouveauJoueur, _Position, Utilite), % Rechercher le meilleur coup pour l'autre joueur
    meilleur_coup(Profondeur, Plateau, Joueur, ListeCoups, _, MeilleurUtilite), % Rechercher le meilleur coup pour l'IA
    afficher_valeur(Profondeur, Coup, Utilite), % Afficher la valeur du coup - pas forcement utile
    choisir_meilleur_coup(Joueur, Utilite, Coup, MeilleurUtilite, Coups, Position, Utilite). % Choisir le meilleur coup



%.......................................
% choisir_meilleur_coup
%.......................................
% Retourne le meilleur coup entre deux en fonction de leurs valeurs d'utilité respectives.
%
% Si les deux coups ont la même valeur, l'un est choisi aléatoirement.

choisir_meilleur_coup(Profondeur,Joueur,Coup1,Utilite1,Coup2,Utilite2,Position,Utilite) :-
    maximiser(Joueur),                      %%% Si le joueur maximise
    Utilite1 > Utilite2,                    %%% alors une valeur plus grande est meilleure.
    Position = Coup1,
    Utilite = Utilite1,
    !
    .

choisir_meilleur_coup(Profondeur,Joueur,Coup1,Utilite1,Coup2,Utilite2,Position,Utilite) :-
    minimiser(Joueur),                      %%% Si le joueur minimise,
    Utilite1 < Utilite2,                    %%% alors une valeur plus petite est meilleure.
    Position = Coup1,
    Utilite = Utilite1, 
    !.

choisir_meilleur_coup(Profondeur,Joueur,Coup1,Utilite1,Coup2,Utilite2,Position,Utilite) :-
    Utilite1 == Utilite2,                   %%% Si les coups ont la même valeur,
    probabilite_aleatoire(10,Random),       %%% alors en choisir un au hasard
    choisir_egalite(Profondeur,Random,Joueur,Coup1,Utilite1,Coup2,Utilite2,Position,Utilite),    
    !. 

choisir_meilleur_coup(Profondeur,Joueur,Coup1,Utilite1,Coup2,Utilite2,Position,Utilite) :-  
    Position = Coup2,
    Utilite = Utilite2,
    !.


%.......................................
% choisir_egalite
%.......................................
% Sélectionne au hasard entre deux coups de même utilité selon une probabilité
%

choisir_egalite(Profondeur,Random,Joueur,Coup1,Utilite1,Coup2,Utilite2,Position,Utilite) :-
    Random < 6,
    Position = Coup1,
    Utilite = Utilite1, 
    !.

choisir_egalite(Profondeur,Random,Joueur,Coup1,Utilite1,Coup2,Utilite2,Position,Utilite) :-
    Position = Coup2,
    Utilite = Utilite2,
    !.

% choisir_colonne_minimax(+Joueur, -Colonne)
% Détermine la meilleure colonne à jouer pour l'IA Minimax
choisir_colonne_minimax(Joueur, Colonne) :-
    % Récupère le plateau actuel
    plateau_actuel(Plateau),
    % Définit la profondeur maximale
    profondeur_max(ProfondeurMax),
    % Liste les colonnes jouables
    findall(C, joueur_peut_jouer(C), ColonnesJouables),
    % Parcourt toutes les colonnes jouables et applique minimax pour chaque
    trouver_meilleure_colonne(ProfondeurMax, Plateau, Joueur, ColonnesJouables, Colonne).

% trouver_meilleure_colonne(+Profondeur, +Plateau, +Joueur, +Colonnes, -MeilleureColonne)
% Parcourt les colonnes jouables pour trouver la meilleure selon minimax
trouver_meilleure_colonne(Profondeur, Plateau, Joueur, Colonnes, MeilleureColonne) :-
    maplist(minimax_colonne(Profondeur, Plateau, Joueur), Colonnes, Resultats),
    % Trouve la colonne avec la meilleure utilité
    max_member((UtiliteMax, MeilleureColonne), Resultats).

% minimax_colonne(+Profondeur, +Plateau, +Joueur, +Colonne, -Resultat)
% Applique minimax pour une colonne donnée
minimax_colonne(Profondeur, Plateau, Joueur, Colonne, (Utilite, Colonne)) :-
    appliquer_coup(Plateau, Colonne, Joueur, NouveauPlateau),
    changer_joueur(Joueur, Adversaire),
    minimax(Profondeur, NouveauPlateau, Adversaire, _, Utilite).

% appliquer_coup(+Plateau, +Colonne, +Joueur, -NouveauPlateau)
% Simule un coup dans une colonne donnée
appliquer_coup(Plateau, Colonne, Joueur, NouveauPlateau) :-
    nth1(Colonne, Plateau, ColonneActuelle),
    append(ColonneActuelle, [Joueur], NouvelleColonne),
    nth1(Colonne, NouveauPlateau, NouvelleColonne, Plateau).