(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DoPolarizationSums												*)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2015 Rolf Mertig
   Copyright (C) 1997-2015 Frederik Orellana
   Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Compute polarization sums of vector bosons *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fcdevel`DoPolarizationSums`",{"HighEnergyPhysics`FeynCalc`"}];

DoPolarizationSums::"usage"=" DoPolarizationSums[exp,k,n] sums over physical (transverse)
polarizations of external massless vector bosons with momentum k. Here, n is an auxiliary four
vector that goes into the gauge-dependent polarization sum to ensure that we are
summing only over physical polarizations. \n\n
DoPolarizationSums[exp,k,0] replaces the polarization sum of external massless vector
bosons with momentum k by -g(mu,nu). This corresponds to summing over all four
physical and unphysical polarizations. \n\n
DoPolarizationSums[exp,k] sums over polarizations
of external massive vector bosons with momentum k and mass k^2.
";

PolarizationUncontract::"usage"= "PolarizationUncontract[exp,k] does Uncontract
on scalar products involving polarization vectors that depend on k.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


DoPolarizationSums::"noresolv" =
    "Could not resolve polarization structure of `1`. Evaluation aborted!";

Dimension       := Dimension    = MakeContext["CoreOptions","Dimension"];
DiracGamma      := DiracGamma   = MakeContext["CoreObjects","DiracGamma"];
ExtraFactor     := ExtraFactor  = MakeContext["CoreOptions","ExtraFactor"];
FCI             := FCI          = MakeContext["FeynCalcInternal"];
LorentzIndex    := LorentzIndex = MakeContext["CoreObjects","LorentzIndex"];
Momentum        := Momentum     = MakeContext["CoreObjects","Momentum"];
Pair            := Pair         = MakeContext["CoreObjects","Pair"];
Polarization    := Polarization = MakeContext["CoreObjects","Polarization"];

MakeContext[
    Contract,
	EpsEvaluate,
	MomentumExpand,
	PolarizationSum,
	ScalarProductExpand,
	Uncontract
	];

Options[DoPolarizationSums] = { ExtraFactor -> 1, Contract -> True };
Options[DoPolarizationSum] = { Contract -> True };

(*	This is done for performance reasons. Instead of uncontracting every terms that involves k (what Uncontract would
by default), we uncontract only contractions with polarization vectors.	*)
PolarizationUncontract[expr_, k_, opts:OptionsPattern[]] :=
    Block[{temp,polvecmom1,polvecmom2,op1,op2},
          temp = (# // EpsEvaluate// Uncontract[#,polvecmom1,polvecmom2,FilterRules[Join[{opts},{Pair->All}], Options[Uncontract]]]&)& /@
          (Expand[ScalarProductExpand[MomentumExpand[expr]],k]/.{
          Polarization[k,I,op___Rule]:> (op1=op; polvecmom1),Polarization[k,-I,op___Rule]:> (op2=op; polvecmom2)});
          temp = temp /.{polvecmom1 :> Polarization[k, I,op1],polvecmom2 :> Polarization[k, -I,op2]}
	];
(*	Polarization sums for massless vector bosons.	*)
DoPolarizationSum[expr_,k_, n:Except[_?OptionQ], OptionsPattern[]] :=
Block[{temp},
    Which[
        Count[expr, Polarization[k,__], Infinity, Heads -> True] === 0,
        	If [ k=!=0 && n=!=0,
            2 expr,
            4 expr
        	],
        Count[expr, Polarization[k,__], Infinity, Heads -> True] // EvenQ,
            temp = (expr /. Pair[LorentzIndex[rho1_, dim_:4],
            Momentum[Polarization[k, -I, OptionsPattern[]], dim_:4]] Pair[
            LorentzIndex[rho2_, dim_:4],
            Momentum[Polarization[k, I, OptionsPattern[]], dim_:4]] :>
            PolarizationSum[rho1,rho2,k,n, Dimension->dim]);
            If [OptionValue[Contract],
                Contract[temp],
                temp
            ],
        True,
              Message[DoPolarizationSums::"noresolv", InputForm[expr]];
        ]
]/; (k=!=0 && n=!=0) || (k=!=0 && n===0);
(*	Polarization sums for massive vector bosons with mass k^2.	*)
DoPolarizationSum[expr_,k:Except[_?OptionQ], OptionsPattern[]] :=
Block[{temp},
    Which[
        Count[expr, Polarization[k,__], Infinity, Heads -> True] === 0,
        	3 expr,
        Count[expr, Polarization[k,__], Infinity, Heads -> True] // EvenQ,
            temp = (expr //. Pair[LorentzIndex[rho1_, dim_:4],
            Momentum[Polarization[p_, -I, OptionsPattern[]], dim_:4]] Pair[
            LorentzIndex[rho2_, dim_:4],
            Momentum[Polarization[p_, I, OptionsPattern[]], dim_:4]] :>
            PolarizationSum[rho1,rho2,k, Dimension->dim]);
             If [OptionValue[Contract],
                Contract[temp],
                temp
            ],
        True,
              Message[DoPolarizationSums::"noresolv", InputForm[expr]];
        ]
]/; k=!=0;
DoPolarizationSums[expr_, vectors:Except[_?OptionQ].., opts:OptionsPattern[]] :=
    Block[{exp1, exp2,polvecmom1,polvecmom2,res},
		exp1 = PolarizationUncontract[FCI[expr],{vectors}[[1]],opts];
		If[Head[exp1] === Plus,
		    exp2 = List @@ exp1,
		    exp2 = {exp1}];
      	OptionValue[ExtraFactor]*Total[Map[DoPolarizationSum[#,vectors,FilterRules[{opts},Options[DoPolarizationSum]]]&,exp2]]
      	]/; Length[{vectors}] === 1 || Length[{vectors}] === 2;

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DoPolarizationSums | \n "]];
Null
