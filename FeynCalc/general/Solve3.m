(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Solve3 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: First version written 1995 for Tdec, slight modifications later*)
(* ------------------------------------------------------------------------ *)

(* :Summary:  Solve3 is like Solve, but only for linear equations *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`Solve3`",{"HighEnergyPhysics`FeynCalc`"}];

Solve3::"usage"=
"Solve3 is equivalent to Solve, except that it works only for
linear equations (and returns just a list)
and uses the \"high school algorithm\" and is sometimes better than
Solve for systems involving rational polynomials.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

Expanding = MakeContext["CoreOptions","Expanding"];
Factoring = MakeContext["CoreOptions","Factoring"];
FinalSubstitutions = MakeContext["CoreOptions","FinalSubstitutions"];
IsolateNames = MakeContext["CoreOptions","IsolateNames"];

MakeContext[
    Collect2,
    Combine,
    FCPrint,
    Factor2,
    FreeQ2,
    Isolate,
    IsolateSplit,
    Select1,
    Solve2
];

Options[Solve3] = {Factoring -> False, FinalSubstitutions -> {}, ParallelMap -> False};

Solve3[a_/;Head[a]=!=List, b__] := Solve3[{a}, b];

Solve3[eqq_List, clii_List, ops___Rule] := Block[
{cli = clii, factor , finsub,newel, lneq, neqh,isol,
 neq, newneq, col,  new, res = {}, parmap, pmap, starttime = AbsoluteTime[]},

factor = Factoring /. {ops} /. Options[Solve3];
finsub = FinalSubstitutions/. {ops} /. Options[Solve3];
parmap = ParallelMap /. {ops} /. Options[Solve3];
(* High - school algorithm *)


isol[xy__] := If[Length[{xy}] < 10,
                isol[xy] = Isolate[Plus[xy],cli,IsolateNames->LL,
                                   IsolateSplit->Infinity],
                Isolate[Plus[xy],cli,IsolateNames->LL,
                                   IsolateSplit->Infinity]
               ];

With[{cli = cli},
col = ( FCPrint[2," Collect with Factor "];
   Collect[#, cli, Factor] ) &];

If[TrueQ[parmap],
  pmap = ParallelMap;
   DistributeDefinitions[ cli, col, FreeQ2, $VeryVerbose,
HighEnergyPhysics`FeynCalc`FreeQ2`FreeQ2 ] , pmap = Map
  ];


(*
If[$VeryVerbose > 0,
Print["PAREVAL = ", ParallelEvaluate[{$VeryVerbose, col}]];
];
*)

specsimp[{}, b_Rule] := {b};
specsimp[a_List, b_Rule] := pmap[(#[[1]] -> (col[#[[2]] /. b]))&, a];

neq = eqq /. Equal[a_, b_] :> (a-b);
For[i = 1, i <= Length[eqq], i++,
If[!FreeQ[neq, cli[[i]]],
    FCPrint[1,"solve3 i = ",i,"    time used : ",
        Round[(starttime-AbsoluteTime[])/60], " minutes" ];
    While[FreeQ[neq1 = (*col[*)neq[[1]] /. res(*]*), cli[[i]]],
FCPrint[2,"rotating ", i];
          neq = RotateLeft[neq = Prepend[Rest[neq],neq1]]
         ];
FCPrint[2,"solving for ",cli[[i]]];
(*{neq1,cli[[i]]}>>"neq1.s";*)
    new = Solve2[neq1, cli[[i]], Factoring -> False][[1]];
(*
If[$VeryVerbose > 2, Print["solution = ",new//InputForm]];
new >>"new.s";
*)
(*
    new = new[[1]] -> Collect2[new[[2]], cli, Factoring -> col];
*)
(*
CHANGE 20100110
    new = new[[1]] -> Collect2[new[[2]], cli, Factoring -> Factor2];
*)
    new = new[[1]] -> Collect[new[[2]], cli, Factor];
    If[!FreeQ2[new[[2]], cli],
       new = new[[1]] -> Map[Cancel, new[[2]]];
      ];
FCPrint[3,"solution = ",new//InputForm];
    neq = Rest[neq];
If[i>1,
   res = Append[specsimp[res, new], new],
   res = {new}
  ];
  If[i<Length[eqq],
     FCPrint[1,"UPDATING ", LeafCount @ neq];
     newneq = {};
(*
     neqh = Hold@@{neq};
     lneq = Length[neq];
*)
If[AbsoluteTime[] > Global`$quit , Quit[]];

With[{col=col, neqres = neq /. res},
neq = pmap[ col, neqres ];
];

(*
     For[iij = 1, iij <= lneq, iij++,
         If[ $VeryVerbose > 1,
             Print["updating " , iij , " out of ",Length[neq]]
           ];
         newel = neqh[[1, iij]] /. res;
         If[newel === neqh[[1, iij]],
            AppendTo[newneq, newel],
            AppendTo[newneq, col[newel]]
           ];
         Clear[newel];
        ];
     neq = newneq;
*)
FCPrint[1,"leafcount neq = ", LeafCount[neq]];
     ];
  ];
   ];
res = res /. finsub;
If[factor =!= False, res = pmap[(#[[1]] -> factor[#[[2]]])&, res]];
res
];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Solve3 | \n "]];
Null
