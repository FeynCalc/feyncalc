(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Solve3 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: First version written 1995 for Tdec, slight modifications later*)
(* ------------------------------------------------------------------------ *)

(* :Summary:  Solve3 is like Solve, but only for linear equations *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`Solve3`",
             "HighEnergyPhysics`FeynCalc`"];

Solve3::"usage"=
"Solve3 is equivalent to Solve, except that it works only for
linear equations (and returns just a list)
and uses the \"high school algorithm\" and is sometimes better than
Solve for systems involving rational polynomials.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   
MakeContext[ Collect2, Combine,Expanding, Factor2,
             FinalSubstitutions,
             Factoring, Isolate, IsolateNames, IsolateSplit,
             Select1,Solve2
           ];

Options[Solve3] = {Factoring -> False, FinalSubstitutions -> {}};

Solve3[a_/;Head[a]=!=List, b__] := Solve3[{a}, b];

Solve3[eqq_List, cli_List, ops___Rule] := Block[
{factor , finsub,newel, lneq, neqh,isol,plh,
 neq, newneq, col, coll, new, res = {}},
factor = Factoring /. {ops} /. Options[Solve3];
finsub = FinalSubstitutions/. {ops} /. Options[Solve3];
(* High - school algorithm *)
(*
col[x_] := Collect2[x, cli, Factoring -> factor];
*)

isol[xy__] := If[Length[{xy}] < 10,
                isol[xy] = Isolate[Plus[xy],cli,IsolateNames->LL,
                                   IsolateSplit->Infinity],
                Isolate[Plus[xy],cli,IsolateNames->LL,
                                   IsolateSplit->Infinity]
               ];


(* for cancelling *)
plh[a_,b_] := a+b;
plh[a_,b_,c_] := a+b+c;
plh[a_,b_,c_,d_] := a+b+c+d;
col[x_] := (*col[x] =*) Block[{ccit,rr, lin, null1, null2, iii, lccit,fah},
If[LeafCount[x]<100000 && $VersionNumber >2.2, 
If[$VeryVerbose > 1, Print[" Collect with Factor2 "]];
   ccit = Collect[x, cli, Factor2];
If[$VeryVerbose > 1, Print[" Collect with Factor2 done ..."]];
   ccit
   ,
   ccit = Collect2[x, cli, Factoring -> False];
If[$VeryVerbose > 2, Print["collect done"]];
                  lin = Select1[ccit + null1 + null2,cli
                               ] /. null1 -> 0 /. null2 -> 0;
                  ccit = ccit - lin;
If[factor === False,
   lin = Expand[lin],
If[$VeryVerbose > 2, Print["factoring lin ",LeafCount[lin]]];
(*If[$Version === 2.2, lin>>"lin.s", Global`LIN=lin];*)
If[Head[lin] === Plus,
(*
                  lin = Map[Factor, lin];
*)
                  lin = Map[Factor2[#,Method->4]&, lin];

If[$VeryVerbose > 2, Print["factoring lin1 done"]];
  ];
(*
                  lin = Factor[lin];
*)
                  lin = Factor2[lin,Method->4];
If[$VeryVerbose > 2, Print["factoring lin2 nearly done"]];
                  lin = Factor2[Cancel[lin /. Plus->plh]];
If[$VeryVerbose > 2, Print["factoring lin2 done"]];
(*
                  lin = Map[Factor, lin//Expand//Combine];
If[$VeryVerbose > 2, Print["factoring lin done"]];
*)
];
fah[yy_] := If[FreeQ[yy,HoldForm], yy,
               Factor2[FixedPoint[ReleaseHold,yy] ,Method->4]
(*
               Factor[FixedPoint[ReleaseHold,yy]]
*)
              ];
                  If[Head[ccit] =!= Plus,
(*
                     rr = Factor2[ccit],
*)
                     rr = Factor[ccit],
              rr = 0;
             lccit = Length[ccit];
             hccit = Hold@@{ccit};
            For[iii = 1, iii <= lccit, iii++,
If[$VeryVerbose > 1, Print["iii out of ",lccit," = ",iii]];
(*
                hccitiii = Factor2[(hccit[[1,iii]]/.Plus->isol),Method->4];
*)
(*
                hccitiii = Factor[(hccit[[1,iii]]/.Plus->isol)];
*)
                hccitiii = Factor2[hccit[[1,iii]], Method -> 4];
If[$VeryVerbose > 2,Print["first hccitiii Factor2 done"]];
                hccitiii = Cancel[hccitiii /. Plus->plh];
If[$VeryVerbose > 2,Print["hccitiii Cancel done"]];
(*
                hccitii = Factor2[hccitiii, Method -> 4];
If[$VeryVerbose > 2,Print["second hccitiii Factor2 done"]];
*)
If[factor =!= False,
   rr = rr + fah[Factor2[ReleaseHold[hccitiii],Method->4]],
   rr = rr + Expand[ReleaseHold[hccitiii]]
  ];
(*
                rr = rr + hccitiii
*)
               ];
               ]; 
rr = (rr+lin)//.plh->Plus;
If[$VeryVerbose > 2, Print["exiting col with ",rr//InputForm]];
rr]];

specsimp[{}, b_Rule] := {b};
specsimp[a_List, b_Rule] :=
  Map[(#[[1]] -> (coll[#[[2]] /. b]))&, a] /. coll -> col;

neq = eqq /. Equal[a_, b_] :> (a-b);
For[i = 1, i <= Length[eqq], i++, 
If[!FreeQ[neq, cli[[i]]],
    If[ $VeryVerbose > 0, Print["solve3 i = ",i] ];
    While[FreeQ[neq1 = (*col[*)neq[[1]] /. res(*]*), cli[[i]]],
If[ $VeryVerbose > 1, Print["rotating ", i]];
          neq = RotateLeft[neq = Prepend[Rest[neq],neq1]]
         ];
If[ $VeryVerbose > 1, Print["solving for ",cli[[i]]]];
(*{neq1,cli[[i]]}>>"neq1.s";*)
    new = Solve2[neq1, cli[[i]], Factoring -> False][[1]];
(*
If[$VeryVerbose > 2, Print["solution = ",new//InputForm]];
new >>"new.s";
*)
(*
    new = new[[1]] -> Collect2[new[[2]], cli, Factoring -> col];
*)
    new = new[[1]] -> Collect2[new[[2]], cli, Factoring -> Factor2];
    If[!FreeQ2[new[[2]], cli],
       new = new[[1]] -> Map[Cancel, new[[2]]];
      ];
If[$VeryVerbose > 2, Print["solution = ",new//InputForm]];
    neq = Rest[neq];
If[i>1,
   res = Append[specsimp[res, new], new],
   res = {new}
  ];
  If[i<Length[eqq],
     If[ $VeryVerbose > 1, Print["UPDATING"] ];
If[N[MemoryInUse[]/10^6,2] > 40,
     Share[];
     If[ $VeryVerbose > 0, Print["MemoryInUse after Share = ", 
                                 N[MemoryInUse[]/10^6,2], " MB"] 
       ];
  ];
     newneq = {};
     neqh = Hold@@{neq};
     lneq = Length[neq];
     For[iij = 1, iij <= lneq, iij++,
         If[ $VeryVerbose > 1, 
             Print["updating " , iij , " out of ",Length[neq]] 
           ];
         newel = neqh[[1, iij]]/.res;
         If[newel === neqh[[1, iij]],
            AppendTo[newneq, newel],
(*
            AppendTo[newneq, Expand[newel]]
*)
            AppendTo[newneq, col[newel]]
           ];
         Clear[newel];
        ];
     neq = newneq;
If[$VeryVerbose>1,Print["leafcount neq = ", LeafCount[neq]]];
     ];
  ];
   ];
res = res /. finsub;
If[factor =!= False, res = Map[(#[[1]] -> factor[#[[2]]])&, res]];
res
];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Solve3 | \n "]];
Null
