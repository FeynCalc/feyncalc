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
of Spinor[..] .DiracMatrix[..] . Spinor[..].
For taking the spin sum (i.e. constructing the traces) use
FermionSpinSum.\n\n
WARNING: In expr should be NO explicit I in denominators!";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


ccm                 = MakeContext["ChargeConjugationMatrix"];
ccmi                = MakeContext["ChargeConjugationMatrixInv"];
DiracGamma          = MakeContext["DiracGamma"];
Expanding           = MakeContext["Expanding"];
DotSimplify         = MakeContext["DotSimplify"];
fci                 = MakeContext["FeynCalcInternal"];
FreeQ2              = MakeContext["FreeQ2"];
Isolate             = MakeContext["Isolate"];
IsolateNames         = MakeContext["IsolateNames"];
IsolateSplit        = MakeContext["IsolateSplit"];
LorentzIndex        = MakeContext["LorentzIndex"];
(*ComplexIndex        = MakeContext["ComplexIndex"];*)
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
(*
HoldPattern[ rev[yz__] ]:=
*)
rev[yz__]:=
(*
Isolate[
*)
(DOT @@ (Reverse[FRH[{ yz }]]/.
(*Changed 28/2-2001 by F.Orellana, because of bug report by T.Rashba*)(*
                    DiracGamma[5]->(-DiracGamma[5])/.
                   {DiracGamma[6] :> DiracGamma[7],
                    DiracGamma[7]:>DiracGamma[6]}/.*)
                     {ccm :> (-ccm),  ccmi -> (-ccmi)}
        )(*, IsolateNames->c$CC, IsolateSplit->Infinity
       ]*) 
) /; Length[Position[{yz}, Spinor]] < 3;

c$CCfrh /: HoldForm[c$CCfrh[ii_]] := c$CC[ii];

(*cLIndex[x_, dime___] := LorentzIndex[ComplexIndex[x], dime];
cSIndex[x_]          := SUNIndex[ComplexIndex[x]];*)

conpa[x__] := conpa[x] = Pair[x] 

(* ComplexConjugatedef *)
(*sunfcomp[a___] := SUNF @@ ({a}/.ComplexIndex -> Identity);
sundeltacomp[a___] := SUNDelta @@ ({a}/.ComplexIndex -> Identity);*)
(*Added SumOver stuff. F.Orellana. 20/8-2002*)
(*Dropped again 19/1-2003. Let's try and keep some hierarchy:
  FeynCalc -> PHI -> FeynArts. Dropped this whole business and ComplexIndex also;
  it is not maintainable (say you want another real function - you don't want to
  have to to type it in here in this file...)*)
(*sumovercomp[a___] := HighEnergyPhysics`FeynArts`SumOver @@ ({a}/.ComplexIndex -> Identity);
ugencomp[a__] := HighEnergyPhysics`Phi`Objects`UGenerator @@ ({a}/.ComplexIndex -> Identity);*)

(*
nenenen
sundcomp[a___] := SUND @@ ({a}/.ComplexIndex -> Identity);
*)

(* for large expressions it is better to not use DotSimplify *)
Options[ComplexConjugate] = {DotSimplify -> True};

ComplexConjugate[b_HoldForm,opts___?OptionQ] := b /; FreeQ2[fci[FRH[b]],
                                {DOT,LorentzIndex,SUNIndex,Complex}];

ComplexConjugate[x_ /; (Head[x] =!= HoldForm), opts___?OptionQ] :=
                 compcon[fci[x]/.SUNTrace->suntrac, opts
                        ] /. (*SUNF -> sunfcomp /.*)
                    (*HighEnergyPhysics`FeynArts`SumOver -> sumovercomp /.
                    HighEnergyPhysics`Phi`Objects`UGenerator -> ugencomp /.*)
                    SUNDelta -> SUNDeltaContract /.
                       (*SUNDeltaContract -> sundeltacomp/.*)
                          compcon -> compcon2 /. compcon2 ->
                             ComplexConjugate /. suntrac->
                              SUNTrace;

compcon2[x_/;!FreeQ[x, HoldForm], opts___?OptionQ] := compcon[FRH[x], opts];
compcon[x_^n_?(Element[#,Reals]===True)&, opts___?OptionQ] := compcon[x,opts]^n;
compcon[x_Plus, opts___?OptionQ]  := compcon[#,opts]& /@ x;
compcon[x_Times, opts___?OptionQ] := compcon[#,opts]& /@ x;


compcon[b_HoldForm, opts___?OptionQ] := b /;
             FreeQ2[FRH[b], {DOT,LorentzIndex,SUNIndex,Complex}];
compcon[x_ /; (Head[x] =!= Plus) && (Head[x] =!= Times), opts___?OptionQ] :=
If[FreeQ[x, DOT | Complex | DiracGamma], x, 
             Block[{nx=x,oone, suntrac, dotsim},
                  dotsim = DotSimplify /. {opts} /. Options[ComplexConjugate];
                  If[!FreeQ[nx, SUNF], nx = Expand[nx, SUNF]];
                  If[!FreeQ[nx,SUNT], If[dotsim, nx = dotlin[nx]]];
(* this is wrong if nx had Head List ... (change 02/99)
                    nx = (DOT[oone, nx] /. DOT -> rev /. rev -> DOT);
*)
                    nx = nx /. DiracGamma[a__]:>
                    DOT[oone, DiracGamma[a]](*Added 28/2-2001 by F.Orellana - see above*)/;
                    FreeQ[{5,6,7},Evaluate[{a}[[1]]]]/.
                    DiracGamma[5]->(-DiracGamma[5])/.
                   {DiracGamma[6] :> DiracGamma[7],
                    DiracGamma[7]:>DiracGamma[6]}(**)/.
                         DOT -> rev /. rev -> DOT /. oone -> 1;
(*
I think this Isolate-optimization is outdated and causes too much overhead, RM, 14.10.2003

                    nx = nx //. c$CC -> c$CCfrh /. oone -> 1;
*)
                    nx = nx /.
                          (*LorentzIndex -> cLIndex /.
                          SUNIndex  -> cSIndex /.*)
                          Complex[a_, b_] -> Complex[a, -b] ;
                    If[dotsim,  nx = dotlin[nx]];
                nx]] /; FreeQ[x, HoldForm];

(* CAREFUL: Complex[a_, b_] -> Complex[a, -b] is only true if no complex
   variables are in denominators!!!!, (which is the case in HEP, unless you
   have width in the propagators ...)
*)
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ComplexConjugate | \n "]];
Null
