(* ************************************************************** *)
(*                                                                *)
(*                           PhiStart.m                           *)
(*                                                                *)
(* ************************************************************** *)
(*

This is the startup file of Phi.

You'll most likely only need to edit the section
"CHOICE OF CONFIGURATION AND LAGRANGIANS" below.

If you want to customize further, put here model independent
definitions and/or definitions common to all models.

This file is read after loading the sub-packages.
To have extra definitions read before anything else, put them
in 'First.m'.

Model specific definitions and usage definitions for classes of
lagrangians should be put in the relevant configuration file in
the directory 'Configurations'.

Coupling files for Phi and FeynArts should be placed in the
directory 'CouplingVectors'.

Lagrangians for Phi should be placed in the directory
'Lagrangians' and renormalization factors in the
direcory 'Factors'.

*)

(* ************************************************************** *)

(* ************************************************************** *)

(* Abbreviations *)

HighEnergyPhysics`Phi`Objects`PMV::"usage"=
"PMV[x_,opts___] := PhiMesonIsoVector[opts][x];";
HighEnergyPhysics`Phi`Objects`FDr::"usage"=
"FDr is the shorthand notation for FieldDerivative";
HighEnergyPhysics`Phi`Objects`CDr::"usage"=
"CDr is the shorthand notation for
CovariantFieldDerivative";

fcpd:=HighEnergyPhysics`FeynCalc`PartialD`PartialD;
fcli:=HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex;
fcsuni:=HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex;
fcqf:=HighEnergyPhysics`FeynCalc`QuantumField`QuantumField;

PMV[x_,opts___]:=PhiMesonIsoVector;
FDr:=FieldDerivative;
CDr:=CovariantFieldDerivative;

(* ************************************************************** *)

(* Adjoints and conjugates *)

Adjoint[UMatrix[UQuarkMass[a___],b___]]:=
UMatrix[UQuarkMass[a],b];

DecayConstant/:Conjugate[ax_DecayConstant]=ax;
ParticleMass/:Conjugate[ax_ParticleMass]=ax;
QuarkCondensate/:Conjugate[ax_QuarkCondensate]=ax;
UCouplingConstant/:Conjugate[ax_UCouplingConstant]=ax;

(* ************************************************************** *)

(* Variable boxes *)

VariableBoxes["p",ParticlesNumber->12];
VariableBoxes["q",ParticlesNumber->2];
VariableBoxes["i",ParticlesNumber->12];
VariableBoxes["I",ParticlesNumber->12];
VariableBoxes["n",ParticlesNumber->12];
VariableBoxes["j",ParticlesNumber->12];
VariableBoxes["\[Mu]",ParticlesNumber->12];

k/: Format[k,TraditionalForm] :=
  StyleForm["k",FontSlant->"Italic"];

DiracTrace/:
MakeBoxes[
DiracTrace[a__,DiracTraceEvaluate->False],TraditionalForm]:=
MakeBoxes[DiracTrace[a],TraditionalForm];

(* ************************************************************** *)

(* Labels for lines of Feynman diagrams.
   If the FeynArts TeXToPS works on your system,
   you can use e.g. "\\phi instead of "\[CurlyPhi]",
   so that TeX comes out right too *)

(*FALabel[_,phii_]:="\[Psi]"<>ToString[phii];*)
FALabel[PhiMeson,0]:="\[CurlyPhi]";
FALabel[PhiMeson[0],_]:="\[CurlyPhi]";
HighEnergyPhysics`FeynArts`TheLabel[PseudoScalar1]:="\[CurlyPhi]";
FALabel[Pion,0]:="\[Pi]";
FALabel[Pion[0],_]:="\[Pi]";
HighEnergyPhysics`FeynArts`TheLabel[PseudoScalar2]:="\[Pi]";
FALabel[Kaon,0]:="K";
FALabel[Kaon[0],_]:="K";
HighEnergyPhysics`FeynArts`TheLabel[PseudoScalar6]:="K";
FALabel[KaonPlus,0]:="K+";
FALabel[KaonPlus[0],_]:="K+";
HighEnergyPhysics`FeynArts`TheLabel[PseudoScalar7]:="K+";
FALabel[KaonZero,0]:="K0";
FALabel[KaonZero[0],_]:="K0";
HighEnergyPhysics`FeynArts`TheLabel[PseudoScalar8]:="K0";
FALabel[KaonZeroBar,0]:="K0_";
FALabel[KaonZeroBar[0],_]:="K0_";
HighEnergyPhysics`FeynArts`TheLabel[PseudoScalar9]:="K0_";
FALabel[KaonMinus,0]:="K-";
FALabel[KaonMinus[0],_]:="K-";
HighEnergyPhysics`FeynArts`TheLabel[PseudoScalar10]:="K-";
FALabel[Photon,0]:="\[Gamma]";
FALabel[Photon[0],_]:="\[Gamma]";
HighEnergyPhysics`FeynArts`TheLabel[Vector1]:="\[Gamma]";
FALabel[Vector[0],0]:="V";
FALabel[Vector[0][0],_]:="V";
HighEnergyPhysics`FeynArts`TheLabel[Vector0]:="V";
FALabel[AxialVector[0],0]:="A";
FALabel[AxialVector[0][0],_]:="A";
HighEnergyPhysics`FeynArts`TheLabel[AxialVector0]:="A";
FALabel[Fermion[1],0]:="\[Psi]";
FALabel[Fermion[1][0],_]:="\[Psi]";
HighEnergyPhysics`FeynArts`TheLabel[Fermion1]:="\[Psi]";
FALabel[Nucleon,0]:="N";
FALabel[Nucleon[0],_]:="N";
HighEnergyPhysics`FeynArts`TheLabel[Fermion20]:="N";
FALabel[Electron,0]:="e";
FALabel[Electron[0],_]:="e";
HighEnergyPhysics`FeynArts`TheLabel[Fermion7]:="e";
FALabel[Scalar[0],0]:="S";
FALabel[Scalar[0][0],_]:="S";
HighEnergyPhysics`FeynArts`TheLabel[Scalar0]:="S";
FALabel[Scalar[1],0]:="S";
FALabel[Scalar[1][0],_]:="S";
HighEnergyPhysics`FeynArts`TheLabel[Scalar1]:="S";
FALabel[PseudoScalar[0],0]:="P";
FALabel[PseudoScalar[0][0],_]:="P";
HighEnergyPhysics`FeynArts`TheLabel[PseudoScalar0]:="P";

(* ************************************************************** *)

(* If FeynCalc is loaded, change some options *)

If[StringQ[$FeynCalcDirectory],

SetOptions[HighEnergyPhysics`fctables`B0`B0,
BReduce->False,B0Unique->True,B0Real->False];

SetOptions[HighEnergyPhysics`fctools`SetMandelstam`SetMandelstam,
Dimension->{4,D,SpaceTimeDimensions}];

SetOptions[HighEnergyPhysics`fctools`FeynRule`FeynRule,
InitialFunction->PhiToFC];

SetOptions[HighEnergyPhysics`fctools`OneLoop`OneLoop,
WriteOutPaVe -> ToFileName[
{HighEnergyPhysics`FeynCalc`$FeynCalcDirectory, "Phi"}, "Storage"] <>
$PathnameSeparator];

SetOptions[HighEnergyPhysics`fctools`PaVeReduce`PaVeReduce,
WriteOutPaVe -> ToFileName[
{HighEnergyPhysics`FeynCalc`$FeynCalcDirectory, "Phi"}, "Storage"] <>
$PathnameSeparator];

HighEnergyPhysics`FeynCalc`$Abbreviations=
Union[HighEnergyPhysics`FeynCalc`$Abbreviations,
{"Momentum" -> "", "Pair" -> "", "RenormalizationState" -> "",
"ParticleMass" -> "m", "PseudoScalar" -> "PS", "Scalar" -> "S",
"Vector" -> "V", "AxialVector" -> "AV", "Fermion" -> "F"}];

Clear[HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex];
HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex/:
MakeBoxes[HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex[p_],
TraditionalForm]:=ToBoxes[p, TraditionalForm],

Remove[$FeynCalcDirectory]

];

(* ************************************************************** *)

(* Recursive definition of multiple derivatives *)

(* Should usually not be altered *)

Options[CovariantFieldDerivative]=
{DiagonalToU->True,
RemoveIntegerIndices->True,
GaugeGroup->2,UDimension->Automatic};

CovariantFieldDerivative[aa_,x_,loris__fcli,lori1_fcli]:=
(newfuncc[1]=CovariantFieldDerivative[aa,x,lori1];
Do[newfuncc[rep+1]=CovariantFieldDerivative[
newfuncc[rep],x,##]&@@Take[{loris},{-rep}],
{rep,1,Length[{loris}]}];newfuncc[Length[{loris}]+1]);

(* ************************************************************** *)

(* (Re-)setting of options and $-variables *)

(*
   Put definitions here you always want different from the
   default settings.  Model specific definitions should be
   put in the relevant configuration file
*)

(* Which representation should be used for the
   pion/meson matrix - the default is the exponential *)

$UExpansionCoefficients=
{1, 1, 1/2, 1/6, 1/24, 1/120, 1/720, 1/5040, 1/40320,
  1/362880, 1/3628800};

(* ************************************************************** *)
(* ************ CHOOSE CONFIGURATION AND LAGRANGIANS ************ *)
(* ************************************************************** *)

(* Which configuration should be used? *)

  $Configuration=
    "ChPT2";      (*standard SU(2) ChPT*)
    (*"ChPT2Photon";*)(*standard SU(2) ChPT with coupling to a photon*)
    (*"ChPT3";*)      (*Standard SU(3) ChPT*)
    (*"ChPTW3";*)     (*Weak SU(3) ChPT*)
    (*"BChPT2";*)     (*Relativistic baryon SU(2) ChPT*)
    (*"HBChPT2";*)    (*Heavy baryon SU(2) ChPT*)
    (*"QED";*)        (*QED with one lepton*)
    (*"QED2";*)       (*QED with three leptons*)

(* Actual loading of configuration *)
If[$PaletteConfiguration=!="None"&&$Phi===True,
VerbosePrint[2,"Using ",$PaletteConfiguration," chosen from palette"];
$Configuration=Evaluate[$PaletteConfiguration]];
VerbosePrint[2,"Loading configuration ",$Configuration];
tmp`olddir1=tmp`olddir;
LoadConfiguration[$Configuration];
tmp`olddir=tmp`olddir1;

(* Which lagrangians should be loaded? *)

  $ULagrangians=
    {ChPT2[2],ChPT2[4]};
    (*{ChPT2[2],ChPT2[4]};*)
    (*{ChPT2Photon[2],ChPT2Photon[4]};*)
    (*{ChPT3[2],ChPT3[4]};*)
    (*{ChPTW3[2],ChPTW3[4]};*)
    (*{BChPT2[2]};*)
    (*{HBChPT2[2]};*)
    (*{QED[1],QED[2]};*)
    (*{QED2[1],QED2[2]};*)

(* Actual loading of lagrangians *)
VerbosePrint[2,"Loading lagrangians ",$ULagrangians];
LoadLagrangian/@$ULagrangians;

(* ************************************************************** *)
(* ****** END OF CHOICE OF CONFIGURATION AND LAGRANGIANS ******** *)
(* ************************************************************** *)

(* Add the palettes to the palette menu of Mathematica.
   (Requires a restart of the frontend) and fix red brackets *)

If[AtomQ[$FrontEnd]=!=True,

   SetOptions[$FrontEnd, PalettePath ->
   Union[PalettePath /. Options[$FrontEnd, PalettePath],
   {ToFileName[{$FeynCalcDirectory, "Phi"}, "Palettes"]}]];

  (*Under Mathematica 4, the default highlighting unmatched brackets
    highlights all right brackets gererated by Phi, so we disable it.
    This is the default: {"UnmatchedBracketStyle" -> "UnmatchedBracket"}*)

   SetOptions[$FrontEnd, AutoStyleOptions ->
   {"UnmatchedBracketStyle" -> None}];
];
