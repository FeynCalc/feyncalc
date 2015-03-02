(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCRenameDummyIndices												*)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2015 Rolf Mertig
   Copyright (C) 1997-2015 Frederik Orellana
   Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Rename dummy Lorentz and SU(N) indices *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fcdevel`FCRenameDummyIndices`",{"HighEnergyPhysics`FeynCalc`"}];

FCRenameDummyIndices::"usage" = "
FCRenameDummyIndices[expr] identifies all dummy Lorentz and SU(N) indices and
changes their names pairwise to random symbols. This can be useful if you have
an expression that contains dummy indices and want to compute the square of it.
For example, the square of GA[a, l, a] equals 16. However, if you forget to rename
the dummy indices and compute GA[a, l, a, a, l, a] instead of GA[a, l, a, b, l, b],
you will get 64.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

LorentzIndex = MakeContext["CoreObjects","LorentzIndex"];
Upper = MakeContext["CoreObjects","Upper"];
Lower = MakeContext["CoreObjects","Lower"];
SUNIndex = MakeContext["CoreObjects","SUNIndex"];
SUNFIndex = MakeContext["CoreObjects","SUNFIndex"];
FCI = MakeContext["FeynCalcInternal"];
MakeContext[ DotSimplify];

FCRenameDummyIndices[expr_List] :=
    Map[FCRenameDummyIndices[#] &, expr];

FCRenameDummyIndices[expr_] :=
    Block[ {indexList={}, replacementList, exprFCI},
        exprFCI = expr // FCI // DotSimplify // Expand;

        If [Head[exprFCI]===Plus,
        indexList =
          Map[Tally, Map[Cases[#, (LorentzIndex | SUNIndex | SUNFIndex)[ind_]/;(Head[ind]=!=Upper &&
              Head[ind]=!=Lower) :> ind,
              Infinity]&,Apply[List, exprFCI]]]// Flatten[#, 1] & // Union;
              ,
        indexList =
           Cases[exprFCI, (LorentzIndex | SUNIndex | SUNFIndex)[ind_]/;(Head[ind]=!=Upper &&
              Head[ind]=!=Lower) :> ind,
              Infinity] // Tally;

        ];
    FCPrint[1,"List of indices to be randomized: ", StandardForm[indexList]];
	If[Select[indexList, ((#[[2]]) > 2) &]=!={},
	    FCPrint[0,"Warning! The expression  ", exprFCI, " violates Einstein notation!"];
	    FCPrint[0,"Following indices appear more than twice: ", Select[indexList, ((#[[2]]) > 2) &]];
	   ];

        replacementList = Map[Rule[#[[1]], $AL[Unique[]]] &, Cases[indexList, {_, 2}]];
        FCPrint[2,"List of replacement rules: ", replacementList];
        result = FCI[expr] //. replacementList;
        result
    ]

End[];
EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[ $VeryVerbose > 0,
    WriteString["stdout", "FCRenameDummyIndices | \n "]
];
Null
