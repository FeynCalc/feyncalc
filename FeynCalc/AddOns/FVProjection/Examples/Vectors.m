(* ::Package:: *)

(* :Title: Vectors															*)

(*
	This software is covered by the GNU Lesser General Public License 3.
	Copyright (C) 1990-2015 Rolf Mertig
	Copyright (C) 1997-2015 Frederik Orellana
	Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Simple operations with 4-vectors using FVProjection			*)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FVProjection*)


(* This is for the case that we are in the terminal *)
If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Simple operations with 4-vectors using FVProjection."];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];



$LoadAddOns = {"FVProjection"};
<<FeynCalc`


(* ::Section:: *)
(*Examples*)


FVProjectionT[x,mu,p]


FVProjectionL[x,mu,p]


FVProjectionT[x,mu,p]+FVProjectionL[x,mu,p]


sp=FCI[SP[x,y]]+FCI[SP[x,z]]


sp/.FCI[SP[x,a_]]:>Contract[FV[a,mu]FVProjectionT[x,mu,p]]


sp/.FCI[SP[x,a_]]:>Contract[FV[a,mu]FVProjectionL[x,mu,p]]


FVProjectorT[mu,nu,p]


FVProjectorL[mu,nu,p]


FVProjectorT[mu,nu,p]+FVProjectorL[mu,nu,p]


Contract[FVProjectorT[mu,nu,p]FVProjectorL[mu,nu,p]]
