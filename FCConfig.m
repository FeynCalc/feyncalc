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
(*
SetOptions[ToString, PageWidth -> 67 ];
*)
SetOptions[ToString, PageWidth -> 137 ];

Unprotect[SuperscriptBox];
SuperscriptBox[FormBox[SubscriptBox[x_,y_],f_],z_] :=
SubsuperscriptBox[x, y, z];


(*

The default installation directory will be the first one found on 
$Path.

If you do not want to load the first installation on $Path you
can either provide the full path, e.g.:

<</opt/test/HighEnergyPhysics/FeynCalc.m

or set

HighEnergyPhysics`FeynCalc`$FeynCalcDirectory =
"/opt/test/HighEnergyPhysics"

here in FCConfig.m

*)


(* Optional modules. By default all are loaded *)

(* $LoadTARCER = False; *)
(* $LoadPhi = False; *)
(* $LoadFeynArts = False; *)

(* You may specify the directory of your FeynArts installation,
   the default is $FeynCalcDirectory *)
   
(* HighEnergyPhysics`FeynCalc`$FeynArtsDirectory = "/tmp/fa" *)

