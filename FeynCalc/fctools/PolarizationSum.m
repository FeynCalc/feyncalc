(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PolarizationSum                                                   *)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2015 Rolf Mertig
   Copyright (C) 1997-2015 Frederik Orellana
   Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Polarization sums for massive and massless vector
			  bosons, e.g. photons, W's, Z's and gluons. The formulas
			  used here can be found e.g. in M. Boehm, A. Denner and H. Joos,
			  Gauge Theories of the Strong and Electroweak Interaction,
			  Eq. A.1.45 and Eq. A.1.46.	*)

(* ------------------------------------------------------------------------ *)


BeginPackage["HighEnergyPhysics`fctools`PolarizationSum`",{"HighEnergyPhysics`FeynCalc`"}];

PolarizationSum::"usage"=
"PolarizationSum[ mu,nu, ... ] defines
(as abbreviations) different polarization sums.
PolarizationSum[mu, nu] = -g(mu nu);
PolarizationSum[mu, nu, k] = -g(mu nu) + k(mu) k(nu)/k^2;
PolarizationSum[mu, nu, k, n] = polarization sum for spin 1 fields;
(n = external momentum).
PolarizationSum[mu, nu, k, 0] is equivalent to -g(mu nu)";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

Collect2			:= Collect2				= MakeContext["Collect2"];
Dimension			:= Dimension			= MakeContext["CoreOptions","Dimension"];
ExpandScalarProduct	:= ExpandScalarProduct	= MakeContext["ExpandScalarProduct"];
FCI					:= FCI					= MakeContext["FeynCalcInternal"];
Factor2				:= Factor2				= MakeContext["Factor2"];
LorentzIndex		:= LorentzIndex			= MakeContext["CoreObjects","LorentzIndex"];
Momentum			:= Momentum				= MakeContext["CoreObjects","Momentum"];
Pair				:= Pair					= MakeContext["CoreObjects","Pair"];
ScalarProduct		:= ScalarProduct		= MakeContext["ScalarProduct"];

Options[PolarizationSum] = {Dimension -> 4};

(*	Polarization sum for massless vector bosons with gauge terms omitted,
   	e.g. for photons in QED.	 *)
PolarizationSum[mu_,nu_, OptionsPattern[]] :=
    -Pair[LorentzIndex[mu,OptionValue[Dimension]],LorentzIndex[nu,OptionValue[Dimension]]];

(*	Same as above.	*)
PolarizationSum[mu_,nu_, k_, 0, OptionsPattern[]] :=
    -Pair[LorentzIndex[mu,OptionValue[Dimension]],LorentzIndex[nu,OptionValue[Dimension]]]/; k=!=0;

(* 	Polarization sum for massive vector bosons, e.g. W's and Z's in the
	Electroweak Theory. Note that the particle mass enters as k^2, where k
	is the four-momentum of the boson.	*)
PolarizationSum[mu_,nu_, k_, OptionsPattern[]] :=
    Block[ {dim = OptionValue[Dimension]},
        -Pair[LorentzIndex[mu,dim],LorentzIndex[nu,dim]] +
        (Pair[Momentum[k,dim],LorentzIndex[mu,dim]] *
        Pair[Momentum[k,dim],LorentzIndex[nu,dim]])/
        Factor2[ExpandScalarProduct[Momentum[k,dim],Momentum[k,dim]]]
    ]/; k=!=0;

(*	Polarization sum for massless vector bosons with gauge terms included,
   	e.g. for gluons in QCD. The auxiliary four-vector n^mu must satisfy
   	n^mu eps_mu = 0 and n^mu k_mu != 0. Note that FeynCalc doesn't enforce
   	this two conditions by itself, i.e. the user must ensure that they are
   	satisfied.	*)
PolarizationSum[mu_,nu_, k_, n_, OptionsPattern[]] :=
    Block[ {dim = OptionValue[Dimension]},
        (-Pair[LorentzIndex[mu,dim],LorentzIndex[nu,dim]] -
        (Pair[Momentum[k,dim],LorentzIndex[mu,dim]] *
        Pair[Momentum[k,dim],LorentzIndex[nu,dim]] *
        Factor2[ExpandScalarProduct[Momentum[n,dim],Momentum[n,dim]]])/
        Factor2[ExpandScalarProduct[Momentum[k,dim],Momentum[n,dim]]^2] +
        (Pair[Momentum[n,dim],LorentzIndex[mu,dim]] Pair[Momentum[k,dim],LorentzIndex[nu,dim]] +
         Pair[Momentum[k,dim],LorentzIndex[mu,dim]] Pair[Momentum[n,dim],LorentzIndex[nu,dim]])/
        Factor2[ExpandScalarProduct[Momentum[k,dim],Momentum[n,dim]]])//Collect2[#,Pair]&
    ]/; k=!=0 && n=!=0;

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PolarizationSum | \n "]];
Null
