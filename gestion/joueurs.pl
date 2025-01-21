:- module(joueurs, [
    joueur_peut_jouer/1, 
    demander_colonne/3, 
    changer_joueur/2
]).

:- use_module('../ia/minimax_poids_colonnes', [choisir_colonne_minimax_poids_colonnes/2]).
:- use_module('../ia/minimax_defensive', [choisir_colonne_minimax_defensive/2]).


% Alterne entre les joueurs X et O
changer_joueur('X', 'O').
changer_joueur('O', 'X').

% Demande à un joueur (humain ou IA) de choisir une colonne
demander_colonne(Joueur, Colonne, TypeJoueur) :-
    (TypeJoueur = 'humain' ->
        demander_colonne_humain(Joueur, Colonne)
    ; TypeJoueur = 'ia_aleatoire' ->
        demander_colonne_ia_aleatoire(Joueur, Colonne)
    ; TypeJoueur = 'ia_minimax' ->
        demander_colonne_ia_minimax(Joueur, Colonne)
    ; TypeJoueur = 'ia_minimax_poids_colonnes' ->
        demander_colonne_ia_minimax_poids_colonnes(Joueur, Colonne)
    ; TypeJoueur = 'ia_minimax_defensive' ->
        demander_colonne_ia_minimax_defensive(Joueur, Colonne)
    ; writeln("[ERREUR] Type de joueur non reconnu : "),
      writeln(TypeJoueur),
      fail).

% Gestion du choix pour un joueur humain
demander_colonne_humain(_Joueur, Colonne) :-
    format("Joueur ~w, choisissez une colonne (1-7 ou 'stop' pour arrêter) : ", [_Joueur]),
    read(Input),
    (   Input = stop ->
        writeln("Partie arrêtée."),
        halt  % Arrête le programme
    ;   integer(Input), between(1, 7, Input) ->
        Colonne = Input
    ;   writeln("Entrée invalide, veuillez choisir un numéro entre 1 et 7 ou 'stop'."),
        demander_colonne_humain(_Joueur, Colonne)
    ).


% Gestion du choix pour une IA aléatoire
demander_colonne_ia_aleatoire(_Joueur, Colonne) :-
    % writeln('L IA (aléatoire) réfléchit...'),
    aleatoire:choisir_colonne_ia(Colonne).  % Appel au module aléatoire

% Gestion du choix pour une IA utilisant Minimax
demander_colonne_ia_minimax(_Joueur, Colonne) :-
    % writeln("L'IA (Minimax) réfléchit..."),
    plateau_actuel(Plateau),  % Récupère le plateau actuel
    (minimax:choisir_colonne_minimax(Plateau, Colonne) ->
        true
    ;
        writeln('[ERREUR] L IA n a pas pu jouer : aucun mouvement possible.'),
        fail).

% Gestion du choix pour une IA utilisant Minimax avec poids des colonnes
demander_colonne_ia_minimax_poids_colonnes(Joueur, Colonne) :-
    writeln("L'IA (Minimax - Poids des colonnes) réfléchit..."),
    sleep(1),  % Simule un délai pour rendre l'IA plus naturelle
    plateau_actuel(Plateau),  % Récupère le plateau actuel
    (minimax_poids_colonnes:choisir_colonne_minimax_poids_colonnes(Plateau, Colonne) ->
        format('L IA (~w) a choisi la colonne ~w.\n', [Joueur, Colonne])
    ;
        writeln('[ERREUR] L IA n a pas pu jouer : aucun mouvement possible.'),
        fail).


% Gestion du choix pour une IA utilisant Minimax avec defense mechanism
demander_colonne_ia_minimax_defensive(Joueur, Colonne) :-
    writeln("L'IA (Minimax - Poids des colonnes) réfléchit..."),
    sleep(1),  % Simule un délai pour rendre l'IA plus naturelle
    plateau_actuel(Plateau),  % Récupère le plateau actuel
    (minimax_defensive:choisir_colonne_minimax_defensive(Plateau, Colonne) ->
        format('L IA (~w) a choisi la colonne ~w.\n', [Joueur, Colonne])
    ;
        writeln('[ERREUR] L IA n a pas pu jouer : aucun mouvement possible.'),
        fail).


% Vérifie si une colonne est valide pour jouer
joueur_peut_jouer(Colonne) :-
    plateau_actuel(Plateau),  % Récupère le plateau actuel
    nth1(Colonne, Plateau, ListeColonne),
    length(ListeColonne, Taille),
    Taille < 6.  % La colonne est valide si elle contient moins de 6 pions
