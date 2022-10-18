﻿%%%%%%%%%%%% ia.pl %%%%%%%%%%%%
% Deux "moteurs" d'IA :
% - "Aléatoire" jouant aléatoirement ;
% - "Minimax", implémentation de minimax assez paramétrable.

:- module(ia, [iaAleatoire/1
			  ,iaMinimax/7
			  ,toujoursMilieu/1
			  ,toujoursMilieuContre/2
			  ,miroir/3
			  ,poidsPuissance3/1
			  ,poidsPosition/1
			  ,poidsDensite/1
			  ,poidsAdjacence/1]
).

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- use_module(jeu).
:- use_module(util).
:- use_module(miniMax).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats dynamiques %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic poidsPuissance3/1.
:- dynamic poidsPosition/1.
:- dynamic poidsDensite/1.
:- dynamic poidsAdjacence/1.

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

iaAleatoire(Coup) :-
	nbColonnes(NBCOLONNES),
	Coup is random(NBCOLONNES)+1,
	coupValide(Coup).
% AI Aléatoire a choisi une colonne pleine, donc on la fait recommencer.
iaAleatoire(Coup) :-
	iaAleatoire(Coup).

iaMinimax(JoueurCourant,Coup,Profondeur,PoidsPosition,PoidsPuissance3,PoidsDensite,PoidsAdjacence) :-
	assert(poidsPosition(PoidsPosition)),
	assert(poidsPuissance3(PoidsPuissance3)),
	assert(poidsDensite(PoidsDensite)),
	assert(poidsAdjacence(PoidsAdjacence)),
	parcoursArbre(JoueurCourant,Profondeur,Coup,_).

%% Joue toujours au milieu. Si occupé, colonne la plus proche du milieu
toujoursMilieu(Coup) :-
	Coup is 4,
	coupValide(Coup).
toujoursMilieu(Coup) :-
	Coup is 5,
	coupValide(Coup).
toujoursMilieu(Coup) :-
	Coup is 3,
	coupValide(Coup).
toujoursMilieu(Coup) :-
	Coup is 6,
	coupValide(Coup).
toujoursMilieu(Coup) :-
	Coup is 2,
	coupValide(Coup).
toujoursMilieu(Coup) :-
	Coup is 7,
	coupValide(Coup).
toujoursMilieu(Coup) :-
	Coup is 1,
	coupValide(Coup).


%% En priorité, essaye de contrer la victoire imminente de l'adversaire.
%% Si pas de victoire imminente en cours, joue toujours au milieu. Si occupé, colonne la plus proche du milieu
toujoursMilieuContre(JoueurCourant,Coup) :-
	evalVictoireAdversaire(JoueurCourant,Coup),
	coupValide(Coup).

toujoursMilieuContre(JoueurCourant,Coup) :-
	toujoursMilieu(Coup).

%% Joue le même coup que le coup précédent de l'adversaire. Sinon, applique la stratégie toujoursMilieu.
miroir(JoueurCourant,Coup,CoupPrecedent) :-
	coupValide(CoupPrecedent),
	Coup is CoupPrecedent.

miroir(JoueurCourant,Coup,CoupPrecedent) :-
	toujoursMilieu(Coup).