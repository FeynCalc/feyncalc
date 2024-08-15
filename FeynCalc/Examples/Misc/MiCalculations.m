(* ::Package:: *)

(* :Title: MiCalculations                                         *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of some simple loop integrals using FeynCalc only. *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of some tensor decompositions for multi-loop integrals
			that are needed to renormalize Gross-Neveu model at 4 loops."];
];
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*tad1L*)


intRaw=SFAD[{l,m1^2}]


{int,topo}=FCLoopFindTopologies[intRaw,{l},FinalSubstitutions->{},Head->Identity,Names->Function[{x},tad1L]]


{fpInt,pref}=FCFeynmanParametrize[int,topo,Names->x,FCReplaceD->{D->4-2ep},
FeynmanIntegralPrefactor->"Multiloop2"][[1;;2]]


res= fpInt pref


res


finalResult={int->res,topo}


(*Put[finalResult,FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Tadpoles","tad1L.m"}]]*)


(* ::Section:: *)
(*prop1L00*)


intRaw=SFAD[l,l-q]


{int,topo}=FCLoopFindTopologies[intRaw,{l},FinalSubstitutions->{SPD[q]->qq},Head->Identity,Names->Function[{x},prop1L00]]


{fpInt,pref}=FCFeynmanParametrize[int,topo,Names->x,FCReplaceD->{D->4-2ep},EtaSign->True,
FeynmanIntegralPrefactor->"Multiloop2"][[1;;2]]


aux1=Integrate[fpInt/.SMP["Eta"]->0/.x[2]->1,{x[1],0,Infinity},Assumptions->{qq>0,m>0,ep>0,ep<1}]/.qq->qq+ I eta


res= aux1 pref


finalResult={int->res,topo}


(*Put[finalResult,FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Mincer","prop1L00.m"}]]*)
