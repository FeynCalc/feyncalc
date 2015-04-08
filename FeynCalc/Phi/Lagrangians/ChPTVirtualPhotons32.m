(* *************************************************************** *)
(*                                                                 *)
(*                      ChPTVirtualPhotons32                       *)
(*                                                                 *)
(* *************************************************************** *)

(*
Author:              F.Orellana

Year:                2001

Mathematica Version: 4.0

Requirements:        FeynCalc > 3, PHI

Summary:             Lagrangian for PHI

Description:         The leading order ChPT lagrangian with
						electromagnetic couplings.

						Taken from Res Urech (1994), hep-ph/9405341
*)


Begin["Phi`Objects`"];

ChPTVirtualPhotons32::usage =
"ChPTVirtualPhotons32 is the name of the file containing the definitions for \
Lagrangian[ChPTVirtualPhotons3[2]], which is the leading order mesonic \
SU(3) ChPT lagrangian with couplings to virtual photons. \
To evaluate use ArgumentsSupply.";

(* --------------------------------------------------------------- *)

End[];

(* ---------------------------------------------------------------- *)

(* Box definitions *)

pt/:MakeBoxes[pt[a_],TraditionalForm]:=MakeBoxes[TraditionalForm[a]];
pt/:MakeBoxes[pt[],TraditionalForm]:="";
pt/:MakeBoxes[pt[RenormalizationState[1]],TraditionalForm]:="r";
pt/:MakeBoxes[pt[RenormalizationState[0]],TraditionalForm]:="";

CouplingConstant/:
MakeBoxes[
	CouplingConstant[ChPTVirtualPhotons3[2],st___RenormalizationState,
	  sc___RenormalizationScheme,qs___QuarkMassExpansionState],
	TraditionalForm]:=
SuperscriptBox[MakeBoxes[StyleForm["C",FontSlant->"Italic"]][[1]],
	RowBox[Join[{MakeBoxes[TraditionalForm[pt[st]]]},{
		  MakeBoxes[TraditionalForm[pt[sc]]]},{
		  MakeBoxes[TraditionalForm[pt[qs]]]}]]];

(* --------------------------------------------------------------- *)


Lagrangian[ChPTVirtualPhotons3[2]] :=
	1/4*DecayConstant[PhiMeson]^2*

	(UTrace[ NM[CDr[MM,{\[Mu]}],Adjoint[CDr[MM,{\[Mu]}]]] ] +

	UTrace[ NM[UChiMatrix,Adjoint[MM]]+NM[Adjoint[UChiMatrix],MM] ]) -

	1/4*
	NM[FieldStrengthTensor[LorentzIndex[\[Mu]],
	QuantumField[Particle[Photon],LorentzIndex[\[Nu]]]],
	FieldStrengthTensor[LorentzIndex[\[Mu]],
	QuantumField[Particle[Photon],LorentzIndex[\[Nu]]]]]-

	$Gauge/2*
	FDr[QuantumField[Particle[Photon],LorentzIndex[\[Mu]]],{\[Mu]}]*
	FDr[QuantumField[Particle[Photon],LorentzIndex[\[Nu]]],{\[Nu]}]+

	CouplingConstant[ChPTVirtualPhotons3[2]]*
	UTrace[NM[UMatrix[UChiralSpurionRight],MM,
	UMatrix[UChiralSpurionLeft],Adjoint[MM]]];

(* --------------------------------------------------------------- *)

FieldsSet[ChPTVirtualPhotons3[2]] :=
	{IsoVector[QuantumField[Particle[PhiMeson]]],
	QuantumField[Particle[Photon]]};

$Lagrangians = Union[$Lagrangians,{ChPTVirtualPhotons3[2]}];

