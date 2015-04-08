(* global Mathematica changes *)

(* SetOptions[Integrate, GenerateConditions -> False]; *)
(*
Off[General::dirdep];
Off[Needs::nocont];
Off[Get::noopen];
Off[General::spell];
Off[General::spell1];
Off[Attributes::locked];
Off[Set::write];
Off[SetOptions::optnf];*)

(*Off[DeclarePackage::aldec];*)


(*Format[Continuation[_], StringForm] := "";
Format[Continuation[_]] := "";*)
(*
StringBreak[_] := "";
*)
(*
SetOptions[ToString, PageWidth -> 67 ];
*)
(*
SetOptions[ToString, PageWidth -> 137 ];

Unprotect[SuperscriptBox];
SuperscriptBox[FormBox[SubscriptBox[x_,y_],f_],z_] :=
SubsuperscriptBox[x, y, z];
*)

(*

The default installation directory will be the first one found on
$Path.

If you do not want to load the first installation on $Path you
can either provide the full path, e.g.:

<</opt/test/FeynCalc/FeynCalc.m

or set

FeynCalc`$FeynCalcDirectory =
"/opt/test/FeynCalc"

here in FCConfig.m

*)


(* Optional modules.
	Defined defaults here.
	Set to False to load somewhat faster. *)
(*
If[!ValueQ[Global`$LoadTARCER],
	Global`$LoadTARCER = False];
If[!ValueQ[Global`$LoadPhi],
	Global`$LoadPhi = False];
If[!ValueQ[Global`$LoadFeynArts],
	Global`$LoadFeynArts = False];*)

(* You can put the directory for your FeynArts installation here
	if set to Automatic, then FeynArts on the path is loaded.
	This is the default *)

(* FeynCalc`$FeynArtsDirectory = "/opt/feynarts" *)

(* Put this flag to True to have startup messages are printed.
	The default is True *)
(*
If[!ValueQ[Global`$FeynCalcStartupMessages],
	Global`$FeynCalcStartupMessages = True];*)
