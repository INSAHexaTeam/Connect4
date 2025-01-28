% Fichier : ia/aleatoire.pl

:- module(aleatoire, [choisir_colonne_aleatoire/2]).

% Importation uniquement de joueur_peut_jouer/1
:- use_module('../gestion/joueurs', [joueur_peut_jouer/1]).

% IA : choisir une colonne aléatoire valide
choisir_colonne_aleatoire(_Plateau, Colonne) :-
    random(1, 8, ColonneCandidate),  % Génère un nombre aléatoire entre 1 et 7
    (joueur_peut_jouer(ColonneCandidate) ->
        Colonne = ColonneCandidate
    ;
        choisir_colonne_aleatoire(__Plateau, Colonne)
    ).


