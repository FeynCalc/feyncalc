(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ComplexConjugate *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 21 February '99 at 2:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: construct the complex conjugate amplitude,
             introducing complex conjugated indices automatically
*)

(* :Comments: ComplexConjugate does NOT work if complex
              quantities are in denominators!!!!!!!!!!!!!!!
*)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`ComplexConjugate`",
             "HighEnergyPhysics`FeynCalc`"];

ComplexConjugate::"usage"=
"ComplexConjugate[expr] complex conjugates expr.
It operates on  Fermion-lines, i.e., products
of Spinor[..] .DiracMatrix[..] . Spinor[..], and changes all
occuring LorentzIndex[mu] into LorentzIndex[ComplexIndex[mu]].
For taking the spin sum (i.e. constructing the traces) use
FermionSpinSum.\n\n
WARNING: In expr should be NO explicit I in denominators!";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


ccm                 = MakeContext["ChargeConjugationMatrix"];
ccmi                = MakeContext["ChargeConjugationMatrixInv"];
DiracGamma          = MakeContext["DiracGamma"];
Expanding           = MakeContext["Expanding"];
dot                 = MakeContext["DOT"];
DotSimplify         = MakeContext["DotSimplify"];
fci                 = MakeContext["FeynCalcInternal"];
FreeQ2              = MakeContext["FreeQ2"];
Isolate             = MakeContext["Isolate"];
IsolateNames         = MakeContext["IsolateNames"];
IsolateSplit        = MakeContext["IsolateSplit"];
LorentzIndex        = MakeContext["LorentzIndex"];
ComplexIndex        = MakeContext["ComplexIndex"];
Spinor              = MakeContext["Spinor"];
SUND                = MakeContext["SUND"];
SUNDelta            = MakeContext["SUNDelta"];
SUNDeltaContract    = MakeContext["SUNDeltaContract"];
SUNF                = MakeContext["SUNF"];
SUNT                = MakeContext["SUNT"];
SUNIndex            = MakeContext["SUNIndex"];
SUNTrace            = MakeContext["SUNTrace"];
Polarization        = MakeContext["Polarization"];

FRH = FixedPoint[ReleaseHold, #]&;

dotlin[x_] := DotSimplify[x, Expanding -> False];
(*
HoldPattern[ rev[yz__ /; FreeQ2[{yz}, {SUNT}] ] ]:=

*)
(* CHANGE 26/07/94 *)
HoldPattern[ rev[yz__] ]:=
Isolate[
dot @@ (Reverse[FRH[{ yz }]]/.
(*Changed 28/2-2001 by F.Orellana, because of bug report by T.Rashba*)(*
                    DiracGamma[5]->(-DiracGamma[5])/.
                   {DiracGamma[6] :> DiracGamma[7],
                    DiracGamma[7]:>DiracGamma[6]}/.*)
                     ccm-> (-ccm) /.  ccmi -> (-ccmi)
       ), IsolateNames->c$CC, IsolateSplit->Infinity
       ] /; Length[Position[{yz}, Spinor]] < 3;

c$CCfrh /: HoldForm[c$CCfrh[ii_]] := c$CC[ii];

cLIndex[x_, dime___] := LorentzIndex[ComplexIndex[x], dime];
cSIndex[x_]          := SUNIndex[ComplexIndex[x]];

conpa[x__] := Pair[x] /. {Polarization[k_, a_, in___] :>
                          Polarization[k, Conjugate[a], in]};
(* ComplexConjugatedef *)
sunfcomp[a___] := SUNF @@ ({a}/.ComplexIndex -> Identity);
sundeltacomp[a___] := SUNDelta @@ ({a}/.ComplexIndex -> Identity);
(*Added SumOver stuff. F.Orellana. 20/8-2002*)
sumovercomp[a___] := HighEnergyPhysics`FeynArts`SumOver @@ ({a}/.ComplexIndex -> Identity);

ugencomp[a__] := HighEnergyPhysics`Phi`Objects`UGenerator @@ ({a}/.ComplexIndex -> Identity);
(*
nenenen
sundcomp[a___] := SUND @@ ({a}/.ComplexIndex -> Identity);
*)

ComplexConjugate[b_HoldForm] := b /; FreeQ2[fci[FRH[b]],
                                {dot,LorentzIndex,SUNIndex,Complex}];

ComplexConjugate[x_ /; (Head[x] =!= HoldForm)] :=
                 compcon[fci[x]/.SUNTrace->suntrac
                        ] /. SUNF -> sunfcomp /.
                    HighEnergyPhysics`FeynArts`SumOver -> sumovercomp /.
                    HighEnergyPhysics`Phi`Objects`UGenerator -> ugencomp /.
                    SUNDelta -> SUNDeltaContract /.
                       SUNDeltaContract -> sundeltacomp/.
                          compcon -> compcon2 /. compcon2 ->
                             ComplexConjugate /. suntrac->
                              SUNTrace;

compcon2[x_/;!FreeQ[x, HoldForm]] := compcon[FRH[x]];
compcon[x_^n_?NumberQ] := compcon[x]^n;
compcon[x_Plus] := compcon /@ x;
compcon[x_Times]:= compcon /@ x;

compcon[b_HoldForm] := b /;
             FreeQ2[FRH[b], {dot,LorentzIndex,SUNIndex,Complex}];
compcon[x_ /; (Head[x] =!= Plus) && (Head[x] =!= Times)] :=
             Block[{nx=x,oone, suntrac},
                  If[!FreeQ[nx, SUNF], nx = Expand[nx, SUNF]];
(*CHANGE 26/07/94 *)
                    If[!FreeQ[nx,SUNT],
                        nx = DotSimplify[nx, Expanding -> False];
                      ];
(* this is wrong if nx had Head List ... (change 02/99)
                    nx = (dot[oone, nx] /. dot -> rev /. rev -> dot);
*)
                    nx = nx /. DiracGamma[a__]:>
                    dot[oone, DiracGamma[a]](*Added 28/2-2001 by F.Orellana - see above*)/;
                    FreeQ[{5,6,7},Evaluate[{a}[[1]]]]/.
                    DiracGamma[5]->(-DiracGamma[5])/.
                   {DiracGamma[6] :> DiracGamma[7],
                    DiracGamma[7]:>DiracGamma[6]}(**)/.
                         dot -> rev /. rev -> dot;
                    nx = nx //. c$CC -> c$CCfrh /. oone -> 1;
                    nx = nx /.
                          LorentzIndex -> cLIndex /.
                          SUNIndex  -> cSIndex /.
                          Complex[a_, b_] -> Complex[a, -b] ;
                    nx = dotlin[nx];
                nx] /; FreeQ[x, HoldForm];

(* CAREFUL: Complex[a_, b_] -> Complex[a, -b] is only true if no complex
   variables are in denominators!!!!, (which is the case in HEP, unless you
   have width in the propagators ...)
*)
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ComplexConjugate | \n "]];
Null
