(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: internal head of dirac matrices *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`DiracGamma`",{"HighEnergyPhysics`FeynCalc`"}];

DiracGamma::"usage" =
"DiracGamma[x, dim] is the way all Dirac \
matrices and slashes are represented (in the internal representation). \
Use DiracMatrix (or GA, GAD) and DiracSlash (or GS, GSD) \
for manual (short) input. \
DiraGamma[x, 4] simplifies to DiracGamma[x].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
MakeContext[ LorentzIndex, ExplicitLorentzIndex, Momentum,
             DeclareNonCommutative, DiracGammaT];

DeclareNonCommutative[DiracGamma];

SetAttributes[DiracGamma, Constant];

DiracGamma /: Transpose[DiracGamma[a__]] := DiracGammaT[a];

DiracGamma[] = 1;

DiracGamma[x_ Momentum[pe_, di___], dii___]  :=
x DiracGamma[Momentum[pe, di], dii];
DiracGamma[x_ LorentzIndex[pe_, di___], dii___]  :=
x DiracGamma[LorentzIndex[pe, di], dii];
DiracGamma[x_[y_, di___], 4]              := DiracGamma[x[y,di]];
DiracGamma[5, __] := DiracGamma[5];
DiracGamma[6, __] := DiracGamma[6];
DiracGamma[7, __] := DiracGamma[7];
DiracGamma[_, 0]   := 0;
(*Why?? F.Orellana, 21/11-2003*)
(*DiracGamma[0]       = 0;
DiracGamma[0, _]   := 0;*)
DiracGamma[a_Plus] := Map[DiracGamma, a];
DiracGamma[Momentum[x_,dix___], Momentum[y_,diy___]] := DOT[
  DiracGamma[Momentum[x,dix], dix], DiracGamma[Momentum[y,diy], diy]];
DiracGamma[Momentum[x_,dix___], Momentum[y_,diy___], z__] := DOT[
  DiracGamma[Momentum[x,dix], dix], DiracGamma[Momentum[y,diy], diy],
DiracGamma[z]];

DiracGamma[LorentzIndex[x_,dix___], LorentzIndex[y_,diy___]] := DOT[
  DiracGamma[LorentzIndex[x,dix], dix],
    DiracGamma[LorentzIndex[y,diy], diy]];
DiracGamma[LorentzIndex[x_,dix___], LorentzIndex[y_,diy___], z__] := DOT[
  DiracGamma[LorentzIndex[x,dix], dix],
  DiracGamma[LorentzIndex[y,diy], diy], DiracGamma[z]];

DiracGamma[LorentzIndex[x_,dix___], Momentum[y_,diy___]] := DOT[
  DiracGamma[LorentzIndex[x,dix], dix], DiracGamma[Momentum[y,diy], diy]];
DiracGamma[LorentzIndex[x_,dix___], Momentum[y_,diy___], z__] := DOT[
  DiracGamma[LorentzIndex[x,dix], dix], DiracGamma[Momentum[y,diy], diy],
   DiracGamma[z]];

DiracGamma[Momentum[x_,dix___], LorentzIndex[y_,diy___]] := DOT[
  DiracGamma[Momentum[x,dix], dix], DiracGamma[LorentzIndex[y,diy], diy]];
DiracGamma[Momentum[x_,dix___], LorentzIndex[y_,diy___], z__] := DOT[
  DiracGamma[Momentum[x,dix], dix], DiracGamma[LorentzIndex[y,diy], diy],
   DiracGamma[z]];

DiracGamma[LorentzIndex[x_], di_Symbol-4 ] := 0;  (*    4, D-4 *)
DiracGamma[Momentum[x_], di_Symbol-4 ] := 0;  (*    4, D-4 *)
DiracGamma[Momentum[x_, di_Symbol -4]] := 0;  (*  D-4, 4   *)
DiracGamma[LorentzIndex[x_, di_Symbol -4]] := 0;  (*  D-4, 4   *)
DiracGamma[LorentzIndex[x_, di_], di_Symbol-4] :=
  DiracGamma[LorentzIndex[x, di-4], di-4];
DiracGamma[Momentum[x_, di_], di_Symbol-4]:=
DiracGamma[Momentum[x, di-4], di-4];
(* D-4, D *)
DiracGamma[LorentzIndex[x_, di_Symbol-4], di_Symbol] :=
 DiracGamma[LorentzIndex[x,di-4], di-4];
 DiracGamma[Momentum[x_, di_Symbol-4], di_Symbol] :=
   DiracGamma[Momentum[x,di-4], di-4];
DiracGamma[ LorentzIndex[x_], di_Symbol]:= DiracGamma[LorentzIndex[x]];
DiracGamma[ n_. Momentum[x_], di_Symbol] :=
(n DiracGamma[Momentum[x]]) /; NumberQ[n];
DiracGamma[Momentum[x_,di_Symbol]]       :=  DiracGamma[Momentum[x]];
(* D, 4 *)
DiracGamma[LorentzIndex[x_,di_Symbol]] :=
  DiracGamma[LorentzIndex[x]];  (* D, 4 *)

   Pair = MakeBoxes["Pair"];

DiracGamma /:
  MakeBoxes[ DiracGamma[a_, di___],
             TraditionalForm ] :=
  MakeBoxes[
  HighEnergyPhysics`FeynCalc`Pair`Pair[
  HighEnergyPhysics`FeynCalc`Momentum`Momentum["\[Gamma]",di],a],
  TraditionalForm
           ] /; !FreeQ[a, Momentum];

DiracGamma /:
  MakeBoxes[ DiracGamma[n_Integer, ___], TraditionalForm ] :=
   SuperscriptBox["\[Gamma]", n];

DiracGamma /:
  MakeBoxes[ DiracGamma[lo_[in_], ru___Rule], TraditionalForm ] :=
   (SuperscriptBox[RowBox[{OverscriptBox["\[Gamma]", "_"]}], Tbox[in]]
   ) /; $BreitMaison === True && (lo === LorentzIndex || lo === ExplicitLorentzIndex);

DiracGamma /:
  MakeBoxes[ DiracGamma[lo_[in_], ru___Rule], TraditionalForm ] :=
   (SuperscriptBox["\[Gamma]", Tbox[lo[in]]]
   ) /; $BreitMaison === False && (lo === LorentzIndex || lo === ExplicitLorentzIndex);

DiracGamma /:
  MakeBoxes[ DiracGamma[lo_[in_,d_Symbol], _Symbol,
           ru___Rule], TraditionalForm ] :=
   (SuperscriptBox["\[Gamma]", Tbox[lo[in,d]]]
   ) /; (lo === LorentzIndex || lo === ExplicitLorentzIndex);

DiracGamma /:
  MakeBoxes[ DiracGamma[lo_[in_, d_Symbol-4], d_Symbol-4,
             ru___Rule], TraditionalForm
           ] :=
      SuperscriptBox[RowBox[{OverscriptBox["\[Gamma]","^"]}], Tbox[in]
                    ] /; (lo === LorentzIndex || lo === ExplicitLorentzIndex);

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracGamma | \n "]];
Null
