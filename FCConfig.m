(* global Mathematica changes *) 

Off[Needs::nocont]; 
Off[Get::noopen]; 
Off[General::spell]; 
Off[General::spell1];
Off[DeclarePackage::aldec];
Off[Attributes::locked];
Off[Set::write];

Format[Continuation[_], StringForm] := "";
Format[Continuation[_]] := "";

StringBreak[_] := "";
SetOptions[ToString, PageWidth -> 67 ];

Unprotect[SuperscriptBox];
SuperscriptBox[FormBox[SubscriptBox[x_,y_],f_],z_] :=
SubsuperscriptBox[x, y, z];

(*

This is the default installation directory which will be set
in FeynCalc.m 

HighEnergyPhysics`FeynCalc`$FeynCalcDirectory = $TopDirectory <> $PathnameSeparator <> "AddOns" <>
                     $PathnameSeparator <> "Applications" <> 
                     $PathnameSeparator <> "HighEnergyPhysics";
*)

(* Optional modules *)

If[!ValueQ[$LoadTARCER], $LoadTARCER = False];
If[!ValueQ[$LoadPhi], $LoadPhi = False];
If[!ValueQ[$LoadFeynArts], $LoadFeynArts = False];

(* $LoadTARCER = True; *)
(* $LoadPhi = True; *)
(* $LoadFeynArts = True; *)
