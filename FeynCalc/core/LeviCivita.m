(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: LeviCivita *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`LeviCivita`",{"HighEnergyPhysics`FeynCalc`"}];

LeviCivita::"usage" =
"LeviCivita[mu, nu, ro, si] is an input  function for the
totally antisymmetric Levi-Civita tensor.
It evaluates automatically
to the internal representation Eps[ LorentzIndex[mu],  LorentzIndex[nu],
LorentzIndex[ro], LorentzIndex[si] ]
(or with a second argument in LorentzIndex for the Dimension,
if the option Dimension of LeviCivita is changed).  \n
LeviCivita[mu, nu ...][ p, ...] evaluates to
Eps[LorentzIndex[mu], LorentzIndex[nu], ..., Momentum[p], ...].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

Dimension = MakeContext["CoreOptions","Dimension"];
FCI = MakeContext["FeynCalcInternal"];

MakeContext[FreeQ2];

Options[LeviCivita] = {Dimension -> 4, FCI->True};

LeviCivita[x:Except[_?OptionQ].., opts:OptionsPattern[LeviCivita]][y:Except[_?OptionQ]..,
    opts:OptionsPattern[LeviCivita]]/; (Length[{x,y}] =!= 4) && (FreeQ2[{x,y,opts},{Pattern,
	Blank,BlankSequence,BlankNullSequence}]) :=
	Message[LeviCivita::argrx, "LeviCivita["<>ToString[{x,opts}]<>"]["<>ToString[{y,opts}]<>"]", Length[{x,y}], 4];

LeviCivita[x:Except[_?OptionQ] ..., opts:OptionsPattern[]]/; (Length[{x}] > 4) && (FreeQ2[{x,opts},{Pattern,
Blank,BlankSequence,BlankNullSequence}]) :=
	Message[LeviCivita::argrx, "LeviCivita["<>ToString[{x,opts}]<>"]", Length[{x}], 4];

LeviCivita[ a:Except[_?OptionQ].., opts:OptionsPattern[]]:=
	FCI[LeviCivita[a,Join[{FCI->False},FilterRules[{opts},Except[FCI]]]]]/; Length[{a}] === 4 && OptionValue[FCI];

LeviCivita[x:Except[_?OptionQ]..., opts1:OptionsPattern[LeviCivita]][y:Except[_?OptionQ]..., opts2:OptionsPattern[LeviCivita]] :=
	FCI[LeviCivita[x,Join[{FCI->False},FilterRules[{opts1},Except[FCI]]]][y,Join[{FCI->False},FilterRules[{opts2},Except[FCI]]]]]/;
	Length[{x,y}] === 4 && OptionValue[LeviCivita,{opts1},FCI] && OptionValue[LeviCivita,{opts2},FCI];

LeviCivita /:
	MakeBoxes[LeviCivita[(a:Except[_?OptionQ]..)/;Length[{a}] === 4, opts:OptionsPattern[LeviCivita]/;!OptionValue[LeviCivita,{opts},FCI]], TraditionalForm]:=
		ToBoxes[FCI[LeviCivita[a,Join[{FCI->False},FilterRules[{opts},Except[FCI]]]]],TraditionalForm]

LeviCivita /:
	MakeBoxes[LeviCivita[x:Except[_?OptionQ]...,
	opts1:OptionsPattern[LeviCivita]][y:Except[_?OptionQ]...,
	opts2:OptionsPattern[LeviCivita]], TraditionalForm]:=
		ToBoxes[FCI[LeviCivita[x,Join[{FCI->False},FilterRules[{opts1},Except[FCI]]]][y,Join[{FCI->False},FilterRules[{opts2},Except[FCI]]]]],TraditionalForm]/;
		Length[{x,y}] === 4 && !OptionValue[LeviCivita,{opts1,opts2},FCI];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "LeviCivita | \n "]];
Null
