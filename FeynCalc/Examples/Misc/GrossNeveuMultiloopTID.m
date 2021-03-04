(* ::Package:: *)

(* :Title: GrossNeveuMultiLoopTID                                         *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of tensor decompositions for multi-loop integrals
			that are needed to renormalize Gross-Neveu model at 3 loops. *)

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
(*Compute the decompositions*)


(* ::Text:: *)
(*For simplicity here we disable parallelization and the advice to use TIDL.*)


$FCAdvice=0;
SetOptions[Tdec,Parallelize->False];


(* ::Text:: *)
(*The following tensor decompositions were obtained by Gracey in 2008  (c.f. arXiv:0804.1241)  and also used by*)
(*Gracey, Luthe and Schroeder in 2016 (c.f. arXiv:1609.05071)*)


input1=FVD[k,mu]FVD[k,nu]f1[k^2]
int1=(input1/.FVD[k,mu]FVD[k,nu]->Tdec[{{k,mu},{k,nu}},{},List->False])


input2=FVD[k1,mu1]FVD[k2,mu2]FVD[k3,mu3]FVD[k4,mu4]f2[k^2,l^2]
int2=Collect2[input2/.FVD[k1,mu1]FVD[k2,mu2]FVD[k3,mu3]FVD[k4,mu4]->Tdec[{{k1,mu1},{k2,mu2},{k3,mu3},{k4,mu4}},{},List->False],{MTD},
Factoring->FullSimplify]


input3=FVD[k1,mu1]FVD[k2,mu2]FVD[k3,mu3]FVD[k4,mu4]FVD[k5,mu5]FVD[k6,mu6]f3[k^2,l^2,q^2]
int3=Collect2[input3/.FVD[k1,mu1]FVD[k2,mu2]FVD[k3,mu3]FVD[k4,mu4]FVD[k5,mu5]FVD[k6,mu6]->
Tdec[{{k1,mu1},{k2,mu2},{k3,mu3},{k4,mu4},{k5,mu5},{k6,mu6}},{},List->False],{MTD,Subscript[f, 3]},Factoring->FullSimplify];
If[ $FrontEnd =!= Null,
	Print[int3]
];


(* ::Text:: *)
(*The full result is pretty large, which is why in the paper only one part of it was explicitly given, namely*)


int3Part=(Coefficient[int3,MTD[mu1,mu2]MTD[mu3,mu4]MTD[mu5,mu6]]MTD[mu1,mu2]MTD[mu3,mu4]MTD[mu5,mu6])//FullSimplify


(* ::Section:: *)
(*Compare to the literature*)


int1Gracey=MTD[mu,nu]/D SPD[k,k] f1[k^2]


int2Gracey=1/(D(D-1)(D+2))f2[k^2,l^2](
((D+1)SPD[k1,k2]SPD[k3,k4]-SPD[k1,k3]SPD[k2,k4]-SPD[k1,k4]SPD[k2,k3])MTD[mu1,mu2]MTD[mu3,mu4]
+((D+1)SPD[k1,k3]SPD[k2,k4]-SPD[k1,k2]SPD[k3,k4]-SPD[k1,k4]SPD[k2,k3])MTD[mu1,mu3]MTD[mu2,mu4]
+((D+1)SPD[k1,k4]SPD[k2,k3]-SPD[k1,k2]SPD[k3,k4]-SPD[k1,k3]SPD[k2,k4])MTD[mu1,mu4]MTD[mu2,mu3])


int3PartGracey=MTD[mu1,mu2]MTD[mu3,mu4]MTD[mu5,mu6]/(D(D-1)(D-2)(D+2)(D+4))f3[k^2,l^2,q^2](
(D^2+3D-2)SPD[k1,k2]SPD[k3,k4]SPD[k5,k6]
-(D+2)SPD[k1,k2]SPD[k3,k5]SPD[k6,k4]
-(D+2)SPD[k1,k2]SPD[k3,k6]SPD[k5,k4]
-(D+2)SPD[k1,k3]SPD[k2,k4]SPD[k5,k6]
+2SPD[k1,k3]SPD[k2,k5]SPD[k6,k4]
+2SPD[k1,k3]SPD[k2,k6]SPD[k5,k4]
-(D+2)SPD[k1,k4]SPD[k2,k3]SPD[k5,k6]
+2SPD[k1,k4]SPD[k2,k5]SPD[k3,k6]
+2SPD[k1,k4]SPD[k2,k6]SPD[k3,k5]
+2SPD[k1,k5]SPD[k2,k3]SPD[k4,k6]
+2SPD[k1,k5]SPD[k2,k4]SPD[k3,k6]
-(D+2)SPD[k1,k5]SPD[k2,k6]SPD[k3,k4]
+2SPD[k1,k6]SPD[k2,k3]SPD[k4,k5]
+2SPD[k1,k6]SPD[k2,k4]SPD[k5,k3]
-(D+2)SPD[k1,k6]SPD[k2,k5]SPD[k4,k3]
)


Print["Check with Gracey (arXiv:0804.1241), Eqq. 4.1-4.3, ", If[Simplify[(int1Gracey-int1)]===0 &&
Simplify[(int2Gracey-int2)]===0 && Simplify[(int3PartGracey-int3Part)]===0,
		"CORRECT.", "!!! WRONG !!!"]];
