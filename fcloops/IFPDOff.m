(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: IFPDOff *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: IFPDOff *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fcloops`IFPDOff`",
             {"HighEnergyPhysics`FeynCalc`"}];

IFPDOff::"usage" =
"IFPDOff[exp_,q1_, q2_, ...] changes from
IFPD representation to FeynAmpDenominator[ ...].
The q1, q2, ... are the integration momenta.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[Cases2,
ExpandScalarProduct,
FeynAmpDenominator,
FeynAmpDenominatorSimplify,
IFPD,
Momentum,
MomentumExpand,
Pair,
PropagatorDenominator,
Select1,
Select2];

SetAttributes[IFPDOff, HoldAll];
SetAttributes[unset, HoldAll];

(* this is a side effect on Pair ... *)
(* it is undone by IFPDOff *)

unasign[quu__][pp_Plus, m_] :=
  If[Head[Select2[pp,quu]] === Plus,
     unasign[quu][First[Select2[pp,quu]], Rest[Select2[pp,quu]] +
                Select1[pp,quu], m],
     unasign[quu][Select2[pp,quu], Select1[pp,quu], m]
    ];

unassign0[quu__][Momentum[q_, di___],m_] :=
 If[!FreeQ[{quu},q],
 If[Head[Pair[Momentum[q,di],Momentum[q,di]]]=!=Pair,
    uns[pair[Momentum[q,di], Momentum[q,di]]]/.
    uns->unset/.pair->Pair/.unset->Unset;
  ]];

pair[-Momentum[a__], Momentum[b__]] := pair[Momentum[a],Momentum[b]];
pair[-Momentum[a__],-Momentum[b__]] := pair[Momentum[a],Momentum[b]];
pair[ Momentum[a__],-Momentum[b__]] := pair[Momentum[a],Momentum[b]];

unassign1[quu__][Momentum[q_, di___], pes_, m_] :=
 If[!FreeQ[{quu},q],
 If[(Head[Pair[Momentum[q,di],pes]]=!=Pair) &&
    (Head[-Pair[Momentum[q,di],pes]]=!=Pair),
    uns[pair@@Sort[{Momentum[q,di], pes}]]/.
    uns->unset/.pair->Pair/.unset->Unset;
   ];
   ];

unassign2[quu__][-Momentum[q_, di___], pes_, m_] :=
 If[!FreeQ[{quu},q],
 If[Head[Pair[-Momentum[q,di],pes]]=!=Pair,
    uns[pair@@Sort[{-Momentum[q,di], pes}]]/.
    uns->unset/.pair->Pair/.unset->Unset;
  ]];

ifex[a_,b_] := ifex[a,b] = Pair[a,a] - b^2;

FP[y__] := FeynAmpDenominator[PropagatorDenominator[y]];

IFPDOff[exp_,qu__] :=
If[FreeQ[exp, IFPD],
   exp,
Block[{int,qq,sub,t1,t2,t3,t4},
int = Apply[Hold, {exp}];
qq = {qu} /. Momentum[a_,___] :> a;
t2 = Cases2[int, IFPD];
t3 = Map[(#  -> (1/#/.IFPD->FEP))&,t2];
(* unassign *)
Cases2[DownValues@@{Pair},IFPD]/.
IFPD -> unasign[qq] /. unasign -> unassign0 /.
        unassign0 -> unassign1 /. unassign1 -> unassign2;
sub = Dispatch[t3];
int = int /. sub;
int = Operate[# /. Hold -> Identity&, int];
int = int /. FEP[a_, b_]^n_Integer?Negative :>
           (ExpandScalarProduct[a, a] - b^2)^(-n);
int = int /. {FEP :> FP, IFPD :> ifex};
int
                           ]
  ];

Off[Unset::norep];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "IFPDOff | \n "]];
Null