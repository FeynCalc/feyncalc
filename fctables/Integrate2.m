(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Integrate2 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 10 December '98 at 20:35 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  Integrate2 is like Integrate, but better *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctables`Integrate2`",
             "HighEnergyPhysics`FeynCalc`"];

Integrate2::"usage"=
"Integrate2 is like Integrate, but, schematically,
Integrate2[a_Plus, b__] := Map[Integrate2[#, b]&, a]  and
Integrate[f[x] DeltaFunction[1-x], {x,0,1}] --> f[1].
Integrate2[1/(1-y),{y,x,1}] is intepreted as distribution, i.e. as
Integrate2[1/(1-y),{y,x,1}] --> Log[1-x].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[Collect2, DeltaFunction, DOT, Epsilon,Expand2,Expanding,
            Factor2, Factoring, FreeQ2,
            Integrate3,Integrate4,MemSet,Nielsen,PowerSimplify,
            SimplifyPolyLog,
            PlusDistribution, Select1, Select2, Solve2, Trick, Zeta2];

Options[Integrate2] = Options[Integrate];

Integrate2[a_, b_List, c__List,opt___Rule] :=
   Integrate2[Integrate2[a,b], c,opt] /; FreeQ[Select2[a, b[[1]]],
                                           DeltaFunction];

polylogc[yai_][nm_,z_] :=
               If[FreeQ[z, Plus], PolyLog[nm,z],
                  PolyLog[nm, Factor2[z]]
                 ];


logc[yai_][z_] := Block[{in},
                        If[FreeQ[z, Plus], Log[z],
                        in = Factor2[z];
                        If[Head[in] === Times, Log[in],
                           Log[Collect2[Expand[z],yai,
                                Denominator->True, Expanding->False]
                              ]
                          ]
                          ]
                       ];

(*
niel[a__] := (niel[a] = Nielsen[a, PolyLog->True]) /; !MatchQ[First[{a}],_^2];
*)

niel[1,2, xyz_^2] :=  -(Zeta2*Log[1 - xyz]) + Log[1 - xyz]^2*Log[xyz] -
  3*Zeta2*Log[1 + xyz] - Log[xyz]*Log[1 + xyz]^2 + (2*Log[1 + xyz]^3)/3 +
  2*Log[1 - xyz]*PolyLog[2, 1 - xyz] + 2*Log[1 + xyz]*PolyLog[2, 1 - xyz] -
  2*Log[1 - xyz]*PolyLog[2, -xyz] - 2*Log[1 + xyz]*PolyLog[2, -xyz] -
  4*PolyLog[3, 1 - xyz] - 4*PolyLog[3, (1 + xyz)^(-1)] -
  2*PolyLog[3, -((1 - xyz)/(1 + xyz))] + 2*PolyLog[3, (1 - xyz)/(1 + xyz)] +
  (9*Zeta[3])/2;

niel[other__] := Nielsen[other];

Integrate2[a_, b_, c___List,opt___Rule] :=
If[FreeQ[a, DOT],
   iIntegrate2[a/.Nielsen->niel, b, c, opt],
   iIntegrate2[(a/.Nielsen->niel)//Trick, b, c, opt]
  ];
simplifypoly[z_] :=
(
(* Print["BLA"];  *)
If[FreeQ[z,Integrate3],
   SimplifyPolyLog[ z /. Nielsen -> niel ]/.Nielsen->niel
   ,
   z/. (hold_[Integrate3])[aa_,b_]:>
(*
         Expand[Integrate3[Expand[aa/.Nielsen->niel],b]]
*)
         Expand[Integrate3[Expand[SimplifyPolyLog[aa]/.Nielsen->niel],b]
               ]  /; hold === Hold
  ]);



(*
simplifypoly[z_] :=
If[FreeQ[z,Integrate3], SimplifyPolyLog[z](*//SimplifyPolyLog*),z];
*)

iIntegrate2[a_, b_, c___List,opt___Rule] :=
(* FISHY *)
Expand2[
simplifypoly[
simplifypoly[
(
If[{opt} =!= {} && {opt} =!= {Assumptions ->{True}},
   SetOptions@@Prepend[Integrate, {opt}]
  ];
PowerSimplify[
Expand[Factor2[
                            If[FreeQ[a, Plus], integrateD[a, b, c],
                              If[Head[b] =!= List,
                                 integrateD[a, b, c],
                                 integrateD[
Expand[
Collect2[
                     If[!FreeQ[a, b[[1]]^ww_/;Head[ww] =!= Integer],
                        a, Apart[a,b[[1]]]
                       ], b[[1]],Denominator->True
                                        ] /. {
                             Log :> logc[b[[1]]],
                         PolyLog :> polylogc[b[[1]]]
                                             } /.
                         Log[uu_ - uu_^2] :> Log[1-uu] + Log[uu]
      ,Log]
        , b, c
                                           ]
                                ]
                              ] ] /.
                              {Pi^2 :> (6 Zeta2),
                               Zeta2^2 :> Zeta[2]^2,
                               PolyGamma[2,1] :> (-2 Zeta[3])
                              }/.
                              Integrate -> Integrate3 /.
                               (Hold@@{Integrate3}) ->
                              Integrate4]]
)/.Hold[Integrate] -> Integrate3]], First[Flatten[{b}]]];

integrateD[a_Plus,b__] := Map[integrateD[#,b]&, a];
integrateD[a_, b_, c___] := MemSet[integrateD[a,b,c],
 Block[{i3, tt, nop, n1, n2},
If[Head[i3 = Integrate3[a, b, c] ] =!=
      (Hold @@ {Integrate3}), i3,
     integrate2[a, b, c]
]]                                ];

getx[bb_] := If[Head[bb] === List, bb[[1]], bb];

integrate2[a_, b_, c___] :=
integrate3[
 If[FreeQ[a, DeltaFunction], Expand[a, getx[b]],
    Expand[a, DeltaFunction]
   ], b, c];


integrate3[a_Plus, b_, c___] :=  Map[Integrate2[#, b, c]&, a];


integrate3[a_, b_, c___] := (If[Head[b] === List,
                  Select1[a, b[[1]]] *
                  Integrate3[Select2[a, b[[1]]], b, c
                            ] /. (Hold@@{Integrate3})-> Integrate4,
                  Select1[a, b] *
                  Integrate3[Select2[a, b], b
                            ] /. (Hold@@{Integrate3})-> Integrate4
                 ] // PowerSimplify
               ) /; ((Head[a] =!= Plus) && FreeQ[a, DeltaFunction]);

integrate3[a_] := a;
HoldPattern[integrate3[f_. de_[x_], {x_, x0_, x1_}]]:=
 (f /. x -> 0) /;( x0 >= 0)  && (de === DeltaFunction);
Integrate2[w_] := w;

Integrate2[f_. DeltaFunction[1-x_], {x_,x0_, x1_}] :=
 (f /. x -> 1) /; x0 >= 0;

Integrate2[f_. DeltaFunction[x_], {x_,x0_, x1_}] :=
  (f /. x -> 0) /; x0 >= 0;

Integrate2[f_. DeltaFunction[x_ + y_], {x_,x0_, x1_}] :=
 (f /. x -> -y) /; FreeQ[y,x];

(*The two lines below added 2/5-2001. F.Orellana*)
Integrate2[f_. DeltaFunction[a_*x_ + y_], {x_,x0_, x1_}] :=
 (f/Abs[a] /. x -> -y) /; FreeQ[y,x];

abs[h_Symbol] := h;
abs[- h_Symbol] := h;

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Integrate2 | \n "]];
Null
