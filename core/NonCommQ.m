(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: test for non-commutativity *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`NonCommQ`",
             "HighEnergyPhysics`FeynCalc`"];

NonCommQ::"usage" =
"NonCommQ[exp] yields True if exp contains non-commutative objects \
(i.e. those objects which are listed in $NonComm) not inside \
DiracTrace's or SUNTrace's.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ DiracTrace, FreeQ2, SUNTrace, MemSet];

(* Have traces treated as commutating objects. F.Orellana, 11/9-2002. *)
excludeTraces = a : (DiracTrace |  SUNTrace)[__] :>
                (a /. (Rule[#, ToString[#]] & /@
                {DiracTrace, SUNTrace, Sequence @@ $NonComm}));

NonCommQ[x_?NumberQ]   := False; (*True*)(*Should be False?? 11/9-2002 , F.Orellana.*)
NonCommQ[x_]           := MemSet[NonCommQ[x],
                          FreeQ2[x /. excludeTraces, $NonComm]=!=True];
                       (*The =!= above put in 11/9-2002, F.Orellana.*)

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "NonCommQ | \n "]];
Null
