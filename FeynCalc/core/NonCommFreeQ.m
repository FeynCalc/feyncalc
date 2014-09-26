(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: test for non-commutativity *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`NonCommFreeQ`",{"HighEnergyPhysics`FeynCalc`"}];

NonCommFreeQ::"usage" =
"NonCommFreeQ[exp] yields True if exp contains no non-commutative objects \
(i.e. those objects which are listed in $NonComm) or only non-commutative \
objects inside DiracTrace's or SUNTrace's.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ DiracTrace, FreeQ2, SUNTrace, MemSet];


(* Have traces treated as commutating objects. F.Orellana, 11/9-2002. *)
excludeTraces = a : (DiracTrace |  SUNTrace)[__] :>
                (a /. (Rule[#, ToString[#]] & /@
                {DiracTrace, SUNTrace, Sequence @@ $NonComm}));

NonCommFreeQ[x_?NumberQ]   := True;
NonCommFreeQ[x_]           := MemSet[NonCommFreeQ[x],
                              FreeQ2[x /. excludeTraces, $NonComm]];

(*Comment: Because of this MemSet, NonCommFreeQ is updated when setting a new
  DataType[x,NonCommutative]=True or DataType[x,NonCommutative]=False.
  Rolf implemented it this way to gain speed I suppose. F.Orellana*)

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "NonCommFreeQ | \n "]];
Null
