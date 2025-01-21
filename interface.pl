:- use_module(library(pce)).
:- encoding(utf8).

% Charger tous les fichiers nécessaires
:- consult('jeu.pl').

:- consult('gestion/joueurs.pl').
:- consult('gestion/ia.pl').
:- consult('ia/aleatoire.pl').
:- consult('ia/minimax.pl').

% État de la dernière partie (pour l'affichage décoratif)
:- dynamic derniere_partie/1.
% Compteurs de victoires
:- dynamic score/2.  % score(Couleur, NombreVictoires)

% Initialiser les scores
:- (score(rouge, _) -> true ; assertz(score(rouge, 0))),
   (score(jaune, _) -> true ; assertz(score(jaune, 0))).

% Initialiser l'état de la dernière partie (grille vide)
:- plateau_vide(PlateauVide),
   assertz(derniere_partie(PlateauVide)).

% Prédicat principal pour démarrer l'interface
demarrer_interface :-
    % Vérifier si un menu existe déjà et le détruire
    (object(@menu) -> send(@menu, destroy) ; true),
    
    new(@menu, dialog('Menu - Puissance 4')),
    send(@menu, name, menu),  % Donner un nom à la fenêtre pour la retrouver plus tard
    % Ajouter les gestionnaires d'événements séparément
    send(@menu, recogniser, 
        click_gesture(left, '', single, 
            message(@prolog, afficher_coordonnees, @event?position))),
    % Gestionnaire pour le mouvement de la souris
    send(@menu, recogniser,
        handler(loc_move, message(@prolog, suivre_souris, @event?position))),
    send(@menu, gap, size(80, 40)),
    send(@menu, size, size(400, 400)),


    % Espace
    send(@menu, append, label(space, '')),
    
    % Création de la grille décorative
    new(Grille, picture),
    send(Grille, size, size(195, 165)),
    send(@menu, append, Grille, below), % Position relative au dernier élément
    
    % Dessin du fond bleu
    new(Fond, box(210, 180)),
    send(Fond, fill_pattern, colour(blue)),
    send(Grille, display, Fond),
    
    % Création des cercles et affichage de la dernière partie
    derniere_partie(Plateau),
    forall(between(0, 5, Ligne),
           forall(between(0, 6, Col),
                  creer_cercle_menu(Grille, Plateau, Ligne, Col))),
    
    % Affichage des scores
    
    % Création d'un conteneur vertical pour les boutons
    new(VBox, dialog_group('')),

    new(Titre, label(titre, 'PUISSANCE 4')),
    send(Titre, font, bold),
    send(VBox, append, Titre),

    send(VBox, size, size(400, 400)),
    send(VBox, gap, size(10, 20)),  % Espacement entre les boutons

    
    
    % % Score Jaune
    score(jaune, ScoreJaune),
    atomic_list_concat([ScoreJaune,' :'], TexteJaune),
    new(JauneLabel, text(TexteJaune)),
    send(JauneLabel, colour, black),
    send(JauneLabel, font, font(helvetica, bold, 12)),
    send(VBox, display, JauneLabel,point(205,301)),

    % Score Rouge
    score(rouge, ScoreRouge),
    atomic_list_concat([': ', ScoreRouge], TexteRouge),
    new(RougeLabel, text(TexteRouge)),
    send(RougeLabel, colour, black),
    send(RougeLabel, font, font(helvetica, bold, 12)),
    send(VBox, display, RougeLabel,point(150,301)),

    % Text VS
    new(VSLabel, text('VS')),
    send(VSLabel, colour, black),
    send(VSLabel, font, font(helvetica, bold, 12)),
    send(VBox, display, VSLabel,point(178,301)),

    send(@menu, append, VBox),

    %Cercle jaune
    new(Cercle, circle(12)),
    send(Cercle, center, point(230, 310)),
    send(Cercle, fill_pattern, colour(yellow)),
    send(@menu, display, Cercle),

    %Cercle rouge
    new(Cercle2, circle(12)),
    send(Cercle2, center, point(140, 310)),
    send(Cercle2, fill_pattern, colour(red)),
    send(@menu, display, Cercle2),

    % Ajout du sélecteur d'IA avant les boutons
    new(SelecteurIA, menu('IA', cycle)),
    send(SelecteurIA, append, aleatoire),
    send(SelecteurIA, append, minmax),
    send(SelecteurIA, default, aleatoire),
    send(VBox, display, SelecteurIA, point(280, 60)),
    nb_setval(selecteur_ia, SelecteurIA),

    % Boutons du menu
    creer_bouton(VBox, 'JOUER', nouvelle_partie,point(40,350)),
    creer_bouton(VBox, 'REGLES DU JEU', afficher_regles,point(130,350)),
    creer_bouton(VBox, 'QUITTER', quitter_jeu,point(280,350)),
    
    send(@menu, open_centered).

% Création d'un cercle dans la grille du menu
creer_cercle_menu(Grille, Plateau, Ligne, Col) :-
    X is Col * 30 + 15,
    Y is Ligne * 30 + 15,
    get_pion(Plateau, Ligne, Col, Pion),
    couleur_pion(Pion, Couleur),
    new(Cercle, circle(12)),
    send(Cercle, center, point(X, Y)),
    send(Cercle, fill_pattern, colour(Couleur)),
    send(Grille, display, Cercle).

% Création d'un bouton standardisé
creer_bouton(Dialog, Texte, Action,Pos) :-
    new(B, button(Texte, message(@prolog, Action))),
    send(B, font, font(helvetica, normal, 14)),
    send(B, size, size(200, 40)),
    send(Dialog, display, B,Pos).

% Création de la grille de jeu
nouvelle_partie :-
    % Récupérer le choix de l'IA avant de fermer le menu
    nb_getval(selecteur_ia, SelecteurIA),
    get(SelecteurIA, selection, TypeIA),
    nb_setval(type_ia, TypeIA),  % Stocker le type d'IA pour la partie
    
    send(@menu, free),
    new(F, frame('Puissance 4')),
    nb_setval(frame_ref, F),
    send(F, size, size(800, 650)),
    
    new(D, dialog),
    send(F, append, D),

    % Création de l'indicateur de tour
    new(TourBox, dialog_group('')),
    send(TourBox, size, size(420, 40)),
    send(D, append, TourBox),
    
    % Texte "Tour du joueur : "
    new(TexteTour, text('Tour du joueur')),
    send(TexteTour, font, font(helvetica, bold, 14)),
    send(TourBox, display, TexteTour, point(150, 10)),
    
    % Cercle indicateur
    new(Indicateur, circle(20)),
    send(Indicateur, center, point(280, 20)),
    send(Indicateur, fill_pattern, colour(red)),  % Rouge commence
    send(TourBox, display, Indicateur),
    
    % Stocker la référence à l'indicateur
    nb_setval(cercle_tour, Indicateur),

    % Ajout de l'indicateur du type d'IA
    new(IABox, dialog_group('')),
    send(IABox, size, size(420, 30)),
    send(D, append, IABox),
    
    % Texte pour l'IA
    atomic_list_concat(['IA: ', TypeIA], TexteIA),
    new(IALabel, text(TexteIA)),
    send(IALabel, font, font(helvetica, bold, 12)),
    send(IABox, display, IALabel, point(180, 5)),
    
    % Création de la grille visuelle (6x7)
    new(Grille, picture),
    send(Grille, size, size(420, 360)),
    % Ajouter un gestionnaire de clics sur la grille
    send(Grille, recogniser,
        click_gesture(left, '', single,
            message(@prolog, detecter_colonne, @event?position))),
    % Ajouter un gestionnaire pour le survol
    send(Grille, recogniser,
        handler(loc_move, message(@prolog, survol_colonne, @event?position, Grille))),
    send(D, append, Grille, below),
    
    % Dessin du fond bleu
    new(Fond, box(420, 360)),
    send(Fond, fill_pattern, colour(blue)),
    send(Grille, display, Fond),
    
    % Création des cercles
    forall(between(0, 5, Ligne),
           forall(between(0, 6, Col),
                  creer_cercle(Grille, Ligne, Col))),
    
    demarrer_jeu,  % Initialise une nouvelle partie
    
    % Stocke la référence à la grille pour les mises à jour
    nb_setval(grille_ref, Grille),
    
    send(F, open_centered).

% Variable globale pour stocker la dernière colonne survolée
:- nb_setval(derniere_colonne, -1).

% Prédicat pour gérer le survol d'une colonne
survol_colonne(Position, Grille) :-
    % Vérifier que la grille existe toujours
    (object(Grille) ->
        get(Position, x, X),
        Col is floor(X / 60),
        % Vérifier si on a changé de colonne
        nb_getval(derniere_colonne, DerniereCol),
        (Col \= DerniereCol ->
            % Restaurer la colonne précédente si elle existe
            (DerniereCol >= 0, DerniereCol =< 6 ->
                restaurer_colonne(Grille, DerniereCol)
            ; true),
            % Mettre en surbrillance la nouvelle colonne si elle est valide
            (Col >= 0, Col =< 6 ->
                mettre_en_surbrillance(Grille, Col),
                nb_setval(derniere_colonne, Col)
            ; nb_setval(derniere_colonne, -1))
        ; true)
    ; true).

% Mettre une colonne en surbrillance
mettre_en_surbrillance(Grille, Col) :-
    % Vérifier que la grille existe toujours
    object(Grille),
    etat_jeu(Plateau, _),
    forall(between(0, 5, Ligne),
           (get_pion(Plateau, Ligne, Col, Pion),
            X is Col * 60 + 30,
            Y is Ligne * 60 + 30,
            (Pion = ' ' ->
                % Créer et afficher le nouveau cercle
                new(C, circle(25)),
                send(C, center, point(X, Y)),
                send(C, fill_pattern, colour(grey)),
                send(Grille, display, C)
            ; true))).

% Restaurer l'état normal d'une colonne
restaurer_colonne(Grille, Col) :-
    % Vérifier que la grille existe toujours
    object(Grille),
    etat_jeu(Plateau, _),
    forall(between(0, 5, Ligne),
           (get_pion(Plateau, Ligne, Col, Pion),
            X is Col * 60 + 30,
            Y is Ligne * 60 + 30,
            (Pion = ' ' ->
                % Créer et afficher le nouveau cercle
                new(C, circle(25)),
                send(C, center, point(X, Y)),
                send(C, fill_pattern, colour(white)),
                send(Grille, display, C)
            ; true))).

% Création d'un cercle dans la grille
creer_cercle(Grille, Ligne, Col) :-
    X is Col * 60 + 30,
    Y is Ligne * 60 + 30,
    new(Cercle, circle(25)),  % Diamètre de 50 pixels
    send(Cercle, center, point(X, Y)),
    send(Cercle, fill_pattern, colour(white)),
    send(Grille, display, Cercle).

% Gestion du clic sur une colonne
jouer_colonne(Col) :-
    jouer_coup_interface(Col).

% Action pour quitter
quitter_jeu :-
    (   send(@display, confirm, 'Voulez-vous vraiment quitter ?', @default, @default, 'utf8')
    ->  send(@display, reset),  % Nettoie toutes les fenêtres
        retractall(grille(_)),  % Nettoie les données du jeu
        halt                    % Quitte Prolog
    ;   true                   % Ne rien faire si l'utilisateur annule
    ).

% Afficher les règles du jeu
afficher_regles :-
    new(@menu, dialog('Règles du Puissance 4')),
    send(@menu, size, size(300, 200)),
    
    new(Texte, text('Règles du jeu :')),
    send(Texte, font, font(helvetica, bold, 14)),
    send(@menu, append, Texte),
    send(@menu, append, label(space, '')),
    
    new(Regles, text),
    send(Regles, font, font(helvetica, normal, 12)),
    send(Regles, append, '- Les joueurs jouent tour à tour\n'),
    send(Regles, append, '- Le premier à aligner 4 jetons gagne\n'),
    send(Regles, append, '- Les jetons tombent en bas de la colonne'),
    send(@menu, append, Regles),
    
    new(Bouton, button('OK', message(@menu, destroy))),
    send(Bouton, font, font(helvetica, normal, 12)),
    send(@menu, append, Bouton),
    
    send(@menu, open_centered).

% Prédicat pour mettre à jour l'interface graphique
mettre_a_jour_interface(Plateau) :-
    nb_getval(grille_ref, Grille),
    forall(between(0, 5, Ligne),
           forall(between(0, 6, Col),
                  mettre_a_jour_case(Grille, Plateau, Ligne, Col))).

% Mettre à jour une case spécifique
mettre_a_jour_case(Grille, Plateau, Ligne, Col) :-
    X is Col * 60 + 30,
    Y is Ligne * 60 + 30,
    get_pion(Plateau, Ligne, Col, Pion),
    couleur_pion(Pion, Couleur),
    send(Grille, display, new(C, circle(25))),
    send(C, center, point(X, Y)),
    send(C, fill_pattern, colour(Couleur)).

% Définir la couleur selon le pion
couleur_pion('X', red).
couleur_pion('O', yellow).
couleur_pion(_, white).

% Obtenir le pion à une position donnée
get_pion(Plateau, Ligne, Col, Pion) :-
    nth0(Col, Plateau, Colonne),
    Pos is 5 - Ligne,  % Inverser la ligne car l'affichage est de haut en bas
    (nth0(Pos, Colonne, Pion) -> true ; Pion = ' ').

% Annoncer la victoire
annoncer_victoire(Joueur) :-
    (Joueur = 'X' -> CouleurJoueur = 'Rouge' ; CouleurJoueur = 'Jaune'),
    atomic_list_concat(['Le joueur ', CouleurJoueur, ' a gagné !'], Message),
    send(@display, inform, Message, @default, @default, 'utf8'),
    % Sauvegarder l'état final de la partie
    etat_jeu(Plateau, _),
    retractall(derniere_partie(_)),
    assertz(derniere_partie(Plateau)),
    % Mettre à jour le score
    (Joueur = 'X' -> Couleur = rouge ; Couleur = jaune),
    score(Couleur, Score),
    NouveauScore is Score + 1,
    retract(score(Couleur, Score)),
    assertz(score(Couleur, NouveauScore)),
    % Fermer la fenêtre de jeu
    (nb_current(frame_ref, Frame) -> 
        send(Frame, destroy),
        nb_delete(frame_ref)
    ; true),
    (nb_current(grille_ref, _) -> 
        nb_delete(grille_ref)
    ; true),
    % Mettre à jour la grille du menu
    demarrer_interface.


% Démarrage du menu au chargement
:- initialization(demarrer_interface).

% Prédicat pour afficher les coordonnées d'un clic
afficher_coordonnees(Position) :-
    get(Position, x, X),
    get(Position, y, Y),
    format('Clic aux coordonnées: (~w, ~w)~n', [X, Y]).

% Action pour actualiser le menu
actualiser_menu :-
    catch(
        (   
            (object(@menu) -> send(@menu, destroy) ; true),
            % Relancer l'interface
            demarrer_interface
        ),
        Error,
        (   format('Erreur lors de l\'actualisation: ~w~n', [Error]),
            demarrer_interface  % Relancer quand même l'interface en cas d'erreur
        )
    ).

% Prédicat pour suivre la position de la souris
suivre_souris(Position) :-
    get(Position, x, X),
    get(Position, y, Y),
    format('\rPosition de la souris: (~w, ~w)', [X, Y]).

% Prédicat pour détecter la colonne cliquée
detecter_colonne(Position) :-
    get(Position, x, X),
    % Calculer la colonne en fonction de la position X
    Col is floor(X / 60),
    % Vérifier que la colonne est valide (0-6)
    (Col >= 0, Col =< 6 ->
        % Convertir en numéro de colonne (1-7) et jouer
        ColNum is Col + 1,
        jouer_coup_interface(ColNum)
    ; true).

% Ajouter cette fonction pour mettre à jour l'indicateur de tour
mettre_a_jour_indicateur_tour(Joueur) :-
    nb_getval(cercle_tour, CercleTour),
    (Joueur = 'X' -> 
        send(CercleTour, fill_pattern, colour(red))
    ; 
        send(CercleTour, fill_pattern, colour(yellow))
    ).

% Modifier la fonction jouer_coup_interface pour mettre à jour l'indicateur
% jouer_coup_interface(Col) :-
%     etat_jeu(Plateau, JoueurActuel),
%     coup_valide(Plateau, Col),
%     jouer_coup(Col),
%     mettre_a_jour_interface(Plateau),
%     % Mettre à jour l'indicateur pour le prochain joueur
%     (JoueurActuel = 'X' -> ProchainJoueur = 'O' ; ProchainJoueur = 'X'),
%     mettre_a_jour_indicateur_tour(ProchainJoueur).
