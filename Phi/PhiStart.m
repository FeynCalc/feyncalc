(* ************************************************************** *)
(*                                                                *)
(*                           PhiStart.m                           *)
(*                                                                *)
(* ************************************************************** *)
(*

This is the startup file of PHI.

You'll most likely only need to edit the section
"CHOOSE CONFIGURATION AND LAGRANGIANS" below.

If you want to customize further, put here model independent
definitions and/or definitions common to all models.

This file is read after loading the sub-packages.
To have extra definitions read before anything else, put them
in 'First.m'.

Model specific definitions and usage definitions for classes of
lagrangians are in the relevant configuration file in
the directory 'Configurations'.
You can override these in the present file
(see "EXTRA CONFIGURATION").
If you are running UNIX (Linux), you can put a copy of
the present file in
~/.Mathematica/<version>/AddOns/Applications/HighEnergyPhysics/Phi/
(create the necessary directories) and it will override
the file in the installation directory.

Coupling files for PHI and FeynArts are in the
directory 'CouplingVectors'.

Lagrangians for PHI are in the directory
'Lagrangians' and renormalization factors in the
directory 'Factors'.

*)

(* ************************************************************** *)

(* ************************************************************** *)

(* Abbreviations *)

HighEnergyPhysics`Phi`Objects`PMV::"usage"=
"PMV[x_,opts___] := PhiMesonIsoVector[opts][x].";

PMV[x_,opts___]:=PhiMesonIsoVector[opts][x];

(* ************************************************************** *)

(* Adjoints and conjugates *)

Adjoint[UMatrix[UQuarkMass[a___],b___]]:=
UMatrix[UQuarkMass[a],b];

DecayConstant/:Conjugate[ax_DecayConstant]=ax;
ParticleMass/:Conjugate[ax_ParticleMass]=ax;
QuarkCondensate/:Conjugate[ax_QuarkCondensate]=ax;
CouplingConstant/:Conjugate[ax_CouplingConstant]=ax;

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

(* Change a few FeynArts definitions *)

(* FeynArts 3 uses CTOrder instead of CountertermOrder *)

If[FileNames["*.jar", {ToFileName[$FeynCalcDirectory, "FeynArts"]}]=!=
{}, CountertermOrder = HighEnergyPhysics`FeynArts`CTOrder];

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
FALabel[Fermion[1,1],1]:="\[Nu]";
FALabel[Fermion[1,1][1],_]:="\[Nu]";
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
FALabel[Scalar[2],0]:="s";
FALabel[Scalar[2][0],_]:="s";
HighEnergyPhysics`FeynArts`TheLabel[Scalar2]:="s";
FALabel[PseudoScalar[0],0]:="P";
FALabel[PseudoScalar[0][0],_]:="P";
HighEnergyPhysics`FeynArts`TheLabel[PseudoScalar0]:="P";

(* ************************************************************** *)

(* Change a few FeynCalc options *)

If[StringQ[$FeynCalcDirectory],

  $FCS = Union[$FCS, {PMV}];

   HighEnergyPhysics`fctables`Lagrangian`Lagrangian::"usage" =
    "Lagrangian[m[pars]] returns the raw form of the lagrangian of the model \
m and with parameters pars (e.g. the dimension of the gauge group and the \
order in the perturbative expansion).  To get the full form, use \
ArgumentsSupply.  Lagrangian[\"oqu\"] gives the unpolarized quark operator. \
Lagrangian[\"oqp\"] gives the polarized quark operator.";

   HighEnergyPhysics`FeynCalc`CouplingConstant`CouplingConstant::"usage" =
    "CouplingConstant is the head of coupling constants.  CouplingConstant \
takes three extra optional arguments, with head RenormalizationState, \
RenormalizationScheme and ExpansionState respectively.  E.g. \
CouplingConstant[QED[1]] is the unit charge, CouplingConstant[ChPT2[4],1] \
is the first of the coupling constants of the lagrangian ChPT2[4].  \
CouplingConstant[a_,b_,c___][i_] := \
CouplingConstant[a,b,RenormalizationState[i],c].  \
CouplingConstant is also an option for several Feynman rule functions and for \
CovariantD and FieldStrength.";

   $Multiplications = Union[$Multiplications,{Times, NM, IsoDot, IsoCross,
                            IsoSymmetricCross}];
   
   $DistributiveFunctions = Union[$DistributiveFunctions,
   {Conjugate, ComplexConjugate, Transpose, Adjoint, UTrace, UTrace1, Iso}];
   
   $Containers = {IsoVector, UVector, UMatrix};

   Options[CovariantFieldDerivative] = {Explicit->True, DiagonalToU->True,
      (*RemoveIntegerIndices->False,*)  SUNN->2, UDimension->Automatic};

   CovariantFieldDerivative::"usage" = 
    "CovariantFieldDerivative[f[x],x,{li1,li2,...},opts] is the covariant \
derivative of f[x] with respect to space-time variables x and with Lorentz \
indices li1, li2,... The default CovariantFieldDerivative is taken from \
mesonic ChPT and includes the external sources Vector and AxialVector, which \
by default are zero.  The definitions of the defaults are in the file \
PhiStart.m and in the relevant configuration files and should be set \
according to the process under consideration.  Also it may be neccesary to \
introduce several covariant derivatives (see CovariantNabla).  \
CovariantFieldDerivative is recognized by ArgumentsSupply and partly by \
UNMSplit, that is, for UNMSplit to work, the 'extra' part apart from the \
derivative of CovariantFieldDerivative[f[x],x,{li1,li2,...},opts] should not \
have a meson-field dependence.";
   
   SetOptions[ILimit, FunctionLimits -> {Log -> Log, 
   LeutwylerJBar -> (LeutwylerJBar[
   Sequence @@ Select[Expand /@ {##}, ((! MatchQ[#, _Rule | _List]) &)],
   LeutwylerJBarEvaluation -> "subthreshold",
   ExplicitLeutwylerSigma -> True]&)}];
   
   SetOptions[B0,
   BReduce->False,B0Unique->True,B0Real->False];
   
   SetOptions[SetMandelstam,
   Dimension->{4,D,SpaceTimeDimensions}];
   
   SetOptions[FeynRule,
   InitialFunction->PhiToFC];
   
   SetOptions[OneLoop,
   WriteOutPaVe -> ToFileName[
   {HighEnergyPhysics`FeynCalc`$FeynCalcDirectory, "Phi"}, "Storage"] <>
   $PathnameSeparator];
   
   SetOptions[PaVeReduce,
   WriteOutPaVe -> ToFileName[
   {HighEnergyPhysics`FeynCalc`$FeynCalcDirectory, "Phi"}, "Storage"] <>
   $PathnameSeparator];

  (*Compapatibility with PartialD-operator notation*)
  SetOptions[ExpandPartialD,
  PartialDRelations -> Union[
   (*Original stuff*)(PartialDRelations /. Options[ExpandPartialD]),
   (*Have ExpandPartialD use FieldDerivative for NM products*)
    {NM[a___, (PartialD|RightPartialD)[x_,LorentzIndex[mu_]], b___] :>
    NM[a, FieldDerivative[NM[b], x, LorentzIndex[mu]]],
    NM[a___, (PartialD|RightPartialD)[LorentzIndex[mu_]], b___] :> 
    NM[a, FieldDerivative[NM[b], z, LorentzIndex[mu]]] /; (go = 
        False; {b} /. ((QuantumField[__][
                  y_?AtomQ] | _[___, QuantumField[__], ___][
                  y_?AtomQ]) :> (go = True; z = y)); go),
    NM[a___, LeftPartialD[x_,LorentzIndex[mu_]], b___] :>
    NM[FieldDerivative[NM[a], x, LorentzIndex[mu]], b],
    NM[a___, LeftPartialD[LorentzIndex[mu_]], b___] :> 
    NM[FieldDerivative[NM[a], z, LorentzIndex[mu]], b] /; (go = 
        False; {a} /. ((QuantumField[__][
                  y_?AtomQ] | _[___, QuantumField[__], ___][
                  y_?AtomQ]) :> (go = True; z = y)); go),
    NM[a___, CovariantD[x_,LorentzIndex[mu_]], b___] :>
    NM[a, CovariantFieldDerivative[NM[b], x, LorentzIndex[mu]]],
    NM[a___, CovariantD[LorentzIndex[mu_]], b___] :> 
    NM[a, CovariantFieldDerivative[NM[b], z, LorentzIndex[mu]]] /; (go = 
        False; {b} /. ((QuantumField[__][
                  y_?AtomQ] | _[___, QuantumField[__], ___][
                  y_?AtomQ]) :> (go = True; z = y)); go),
    NM[UMatrix[UIdentity, ___],
       (PartialD | RightPartialD | LeftPartialD)[__]] :> 0}
  ]];

  (* A static hack. Not very pretty. Just to have ExpandPartialD work for
     a few PHI examples (will have to be reevaluated when changing
     $UMatrices if ExpandPartialD is to work for the new UMatrices) *)
   DeclareNonCommutative /@ $UMatrices;
   
   HighEnergyPhysics`Phi`Objects`FunctionalDerivative::"usage" = 
       "FunctionalDerivative is FunctionalD adapted for use with Phi";
   HighEnergyPhysics`Phi`Objects`FunctionalDerivative[x_, o__] :=
       Block[{tr, r, nm, c, d, i, f}, i = 0; f := (++i);
         FunctionalD[
             PhiToFC[x /. HoldPattern[NM[d__]] :> DOT[nm[f], d]] /.
               UTrace1 -> ((tr*#) &), o] //. {DOT[nm[_], c__] :> NM[c],
             tr*r__ :> UTrace[Times[r]]}];
                       
   $Abbreviations=
   Union[$Abbreviations,
   {"Momentum" -> "", "Pair" -> "", "RenormalizationState" -> "",
   "ParticleMass" -> "m", "PseudoScalar" -> "PS", "Scalar" -> "S",
   "Vector" -> "V", "AxialVector" -> "AV", "Fermion" -> "F"}];
      
   (* Commented out 5/3-2000.
      I think its redundant and definitions on SUNIndex slow down everything. *)
   (* Well - not quite so. Uncommented again 21/5-2001 *)
   
   NM[a___, b : IsoVector[__][_], c___][SUNIndex[i_]] ^:= NM[a, b[SUNIndex[i]], c];
   NM[a___, b : IsoVector[__], c___][SUNIndex[i_]] ^:= NM[a, b[SUNIndex[i]], c];
   Times[a___, b : IsoVector[__][_], c___][SUNIndex[i_]] ^:= Times[a, b[SUNIndex[i]], c];
   Times[a___, b : IsoVector[__], c___][SUNIndex[i_]] ^:= Times[a, b[SUNIndex[i]], c];
   
   NM[a___, b : IsoVector[__][_], c___][UIndex[i_]] ^:= NM[a, b[UIndex[i]], c];
   NM[a___, b : IsoVector[__], c___][UIndex[i_]] ^:= NM[a, b[UIndex[i]], c];
   Times[a___, b : IsoVector[__][_], c___][UIndex[i_]] ^:= Times[a, b[UIndex[i]], c];
   Times[a___, b : IsoVector[__], c___][UIndex[i_]] ^:= Times[a, b[UIndex[i]], c],
   
   Remove[$FeynCalcDirectory]

];


(*Add PHI vertices to Amplitudes database*)
(*First make sure amplist has a value*)
Amplitude[];
(*Get the PHI amps*)
Block[{phiAmpList, dum, names, dir},
dir = ToFileName[{$FeynCalcDirectory, "Phi", "CouplingVectors"}];
 names = 
  StringReplace[StringReplace[#, dir -> ""], ".Gen" -> ""] & /@ 
    FileNames["*.Gen", 
      dir]; phiAmpList = (# :> 
          FAToFC[{CheckF[dum, # <> ".Gen", ForceSave -> False, 
                    NoSave -> True].((#[[1]]) & /@ 
                      CheckF[dum, # <> ".Mod", ForceSave -> False, 
                        NoSave -> True])}][[1]]) & /@ names;
(*Modify amplist*)
HighEnergyPhysics`fctables`Amplitude`Private`amplist = 
    Union[HighEnergyPhysics`fctables`Amplitude`Private`amplist,
          phiAmpList]];


(* ************************************************************** *)

(* Recursive definition of multiple derivatives *)

(* Should usually not be altered *)

Options[CovariantFieldDerivative]=
  {Explicit->True, DiagonalToU->True,
   (*RemoveIntegerIndices->True,*)
   SUNN->2, UDimension->Automatic};

CovariantFieldDerivative[aa_,x_,loris__LorentzIndex,lori1_LorentzIndex]:=
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

(* Check for unpatched FeynArts *)

$FAPatch=True;

(* Which representation should be used for the
   pion/meson matrix - the default is the exponential *)

$UExpansionCoefficients=
{1, 1, 1/2, 1/6, 1/24, 1/120, 1/720, 1/5040, 1/40320,
  1/362880, 1/3628800};

(* ************************************************************** *)
(* ************ CHOOSE CONFIGURATION AND LAGRANGIANS ************ *)
(* ************************************************************** *)

(* Which configuration should be used? *)
(* Overridden is set before loading FeynCalc *)

If[ValueQ[Global`$Configuration] =!= True || Global`$Configuration === None ||
   Global`$Configuration === "None",

Global`$Configuration=
    "ChPT2";     (*standard SU(2) ChPT*)
    (*"ChPTPhoton2";*)(*standard SU(2) ChPT with coupling to a photon source*)
    (*"ChPT3";*)      (*Standard SU(3) ChPT*)
    (*"ChPTW3";*)     (*Weak SU(3) ChPT*)
    (*"BChPT2";*)     (*Relativistic baryon SU(2) ChPT*)
    (*"HBChPT2";*)    (*Heavy baryon SU(2) ChPT*)
    (*"ChPTEM2";*)    (*Standard SU(2) ChPT with virtual photons - Meissner, Steininger*)
    (*"ChPTVirtualPhotons2";*)        (*Standard SU(2) ChPT with virtual photons - Urech, Knecht*)
    (*"ChPTVirtualPhotons3";*)        (*Standard SU(2) ChPT with virtual photons - Urech, Knecht*)
    (*"QED";*)        (*QED with one lepton*)
    (*"QED2";*)       (*QED with three leptons*)

]

(* Actual loading of configuration *)
If[$PaletteConfiguration=!="None"&&$PaletteConfiguration=!=None&&$Phi===True,
VerbosePrint[2,"Using ",$PaletteConfiguration," chosen from palette"];
Global`$Configuration=Evaluate[$PaletteConfiguration]];
VerbosePrint[2,"Loading configuration ",Global`$Configuration];
tmp`olddir1=tmp`olddir;
LoadConfiguration[Global`$Configuration];
tmp`olddir=tmp`olddir1;

(* Which lagrangians should be loaded? *)

 If[ValueQ[Global`$Lagrangians] =!= True || Global`$Lagrangians === {},

 Global`$Lagrangians=
    {"ChPT2"[2],"ChPT2"[4]};
    (*{"ChPTPhoton2"[2],"ChPTPhoton2"[4]};*)
    (*{"ChPT3"[2],"ChPT3"[4]};*)
    (*{"ChPTW3"[2],"ChPTW3"[4]};*)
    (*{"BChPT2"[2]};*)
    (*{"HBChPT2"[2]};*)
    (*{"ChPTEM2"[2],"ChPTEM2"[4]};*)
    (*{"ChPTVirtualPhotons2"[2],"ChPTVirtualPhotons2"[4]};*)
    (*{"ChPTVirtualPhotons3"[2],"ChPTVirtualPhotons3"[4]};*)
    (*{"QED"[1],"QED"[2]};*)
    (*{"QED2"[1],"QED2"[2]};*)

]

(* Actual loading of lagrangians *)
VerbosePrint[2,"Loading lagrangians ",Global`$Lagrangians];
LoadLagrangian/@Global`$Lagrangians;

(* Setting of $VerticesSpecifications using all stored vertices
   belonging to the chosen configuration *)
$VerticesSpecifications = 
  VerticesSpecifications[$Configuration, $FAParticlesInUse, $ParticleTypes];

(* ************************************************************** *)
(* ****** END OF CHOOSE CONFIGURATION AND LAGRANGIANS *********** *)
(* ************************************************************** *)

(* ************************************************************** *)
(* ******************* EXTRA CONFIGURATION ********************** *)
(* ************************************************************** *)

(* Source fields *)
(* Fields not needed should be set to 0 to speed up things *)

(* Fields *)
(*QuantumField[___,Particle[Scalar[0],___],___][_]:=0;*)
(*QuantumField[___,Particle[Scalar[1],___],___][_]:=0;*)
(*QuantumField[___,Particle[Scalar[2],___],___][_]:=0;*)
(*QuantumField[___,Particle[PseudoScalar[0],___],___][_]:=0;*)
(*QuantumField[___,Particle[Vector[0],___],___][_]:=0;*)
(*QuantumField[___,Particle[AxialVector[0],___],___][_]:=0;*)

(* Isovectors *)
(*IsoVector[
QuantumField[Particle[Scalar[0],___],___],___][_]:=0;*)
(*IsoVector[
QuantumField[Particle[Scalar[1],___],___],___][_]:=0;*)
(*IsoVector[
QuantumField[Particle[Scalar[2],___],___],___][_]:=0;*)
(*IsoVector[
QuantumField[Particle[PseudoScalar[0],___],___],___][_]:=0;*)
(*IsoVector[
QuantumField[Particle[Vector[0],___],___],___][_]:=0;*)
(*IsoVector[
QuantumField[Particle[AxialVector[0],___],___],___][_]:=0;*)

(* Isosinglets *)
(*QuantumField[___,Particle[Scalar[0],___],___,
  SUNIndex[0],___][_]:=0;*)
(*QuantumField[___,Particle[Scalar[1],___],___,
  SUNIndex[0],___][_]:=0;*)
(*QuantumField[___,Particle[Scalar[2],___],___,
  SUNIndex[0],___][_]:=0;*)
(*QuantumField[___,Particle[PseudoScalar[0],___],___,
  SUNIndex[0],___][_]:=0;*)
QuantumField[___,Particle[AxialVector[0],___],___,
  SUNIndex[0],___][_]:=0;
QuantumField[___,Particle[Vector[0],___],___,
  SUNIndex[0],___][_]:=0;
QuantumField[___,Particle[LeftComponent[0],___],___,
  SUNIndex[0],___][_]:=0;
QuantumField[___,Particle[RightComponent[0],___],___,
  SUNIndex[0],___][_]:=0;

(* The setting below expands the zero'th Scalar[0]
   source around the quark mass. The Scalar[2] source is
   then the perturbation. (- in ChPTW3 we use Scalar[1] for the
   hamiltonian source) *)

IsoVector[QuantumField[Particle[
Scalar[0],ar___RenormalizationState,
br___RenormalizationScheme,cr___ExpansionState,
opts___Rule|opts___List]],opts1___][x_]:=
IsoVector[QuantumField[Particle[Scalar[2],ar,br,cr]],opts1][x];
                        
QuantumField[Particle[
Scalar[0],ar___RenormalizationState,
br___RenormalizationScheme,cr___ExpansionState,
opts___Rule|opts___List], SUNIndex[0]][x_]:=
UQuarkMassMatrix[ar,br,cr,opts]+
QuantumField[Particle[Scalar[2],ar,br,cr,
   Sequence@@OptionsSelect[Particle,opts]],
SUNIndex[0]][x]*UIdentityMatrix[opts];

(* If you're working with the weak lagrangian; have a momentum
   carrying lagrangian or not *)

$Substitutions = DeleteCases[$Substitutions,
UNablaHatDelta[mu_] :> _];

$Substitutions = Append[$Substitutions, UNablaHatDelta[mu_] :>

(* The standard definition *)
(*-I*NM[SMM,
UGeneratorMatrixIsoDot[QuantumField[Particle[
LeftComponent[0]],{mu}]],
UGeneratorMatrix[HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex[6]],
Adjoint[SMM]]+
I*NM[SMM,
UGeneratorMatrix[HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex[6]],
UGeneratorMatrixIsoDot[QuantumField[Particle[
LeftComponent[0]],{mu}]],
Adjoint[SMM]]*)

(* Including a scalar 'source' with momentum *)
NM[SMM,NM[
FDr[QuantumField[Particle[Scalar[1]]],{mu}],
UGeneratorMatrix[HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex[6]],
Adjoint[SMM]]]
];

(* ************************************************************** *)
(* ***************** END OF EXTRA CONFIGURATION ***************** *)
(* ************************************************************** *)

(* Add the palettes to the palette menu of Mathematica
   (Requires a restart of the frontend) and fix red brackets *)

If[AtomQ[$FrontEnd]=!=True,
   SetOptions[$FrontEnd, PalettePath ->
   Append[Select[Options[$FrontEnd,
      PalettePath][[1, -1]], ((If[
            StringQ[#], !StringMatchQ[#, "*HighEnergyPhysics*Phi*"],
            FreeQ[#, "HighEnergyPhysics"] && FreeQ[#, "Phi"]] === True)&)],
  ToFileName[{$FeynCalcDirectory, "Phi"}, "Palettes"]]];

  (*Under Mathematica 4, the default highlighting unmatched brackets
    highlights all right brackets generated by PHI, so we disable it.
    This is the default: {"UnmatchedBracketStyle" -> "UnmatchedBracket"}*)

   SetOptions[$FrontEnd, AutoStyleOptions ->
   {"UnmatchedBracketStyle" -> None}];
];
