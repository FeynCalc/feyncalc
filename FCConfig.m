(* global Mathematica changes *) 

Off[Needs::nocont]; 
Off[Get::noopen]; 
Off[General::spell]; 
Off[General::spell1];
Off[DeclarePackage::aldec];
Off[Attributes::locked];
Off[Set::write];
Off[SetOptions::optnf];

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


(* Optional modules. By default all are loaded.
   Set to False to load somewhat faster. *)

(* Global`$LoadTARCER = False; *)
(* Global`$LoadPhi = False; *)
(* Global`$LoadFeynArts = False; *)

(* You can put the directory for your FeynArts installation here
   if set to Automatic, then FeynArts on the path is loaded.
   This is the default *)
   
(* HighEnergyPhysics`FeynCalc`$FeynArtsDirectory = "/opt/feynarts" *)

(* Put this flag to True to have startup messages are printed.
   The default is False *)

(* Global`$FeynCalcStartupMessages = True; *)
