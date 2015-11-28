(* Wolfram Language package *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCClearScalarProducts												*)

(*
	This software is covered by the GNU Lesser General Public License 3.
	Copyright (C) 1990-2015 Rolf Mertig
	Copyright (C) 1997-2015 Frederik Orellana
	Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Clears definitions of scalar products						    *)

(* ------------------------------------------------------------------------ *)

FCClearScalarProducts::usage =
"FCClearScalarProducts[] removes all user-performed specific settings for \
ScalarProduct's.";

ClearScalarProducts::usage =
"ClearScalarProducts is a shortcut to FCClearScalarProducts[]. It is \
needed mainly for compatibility reasons, so that old codes that use \
ClearScalarProducts instead of FCClearScalarProducts[] still work.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCClearScalarProducts`Private`"]

(* For compatibility with the old syntax	*)
ClearScalarProducts:=FCClearScalarProducts[];

FCClearScalarProducts[OptionsPattern[]] :=
	(
		DownValues[Pair] = initialPairDownValues;
		DownValues[ScalarProduct] = initialScalarProductDownValues;
		UpValues[ScalarProduct] = initialScalarProductUpValues;
		DownValues[SP] = initialSPDownValues;
		DownValues[SPD] = initialSPDDownValues;
	);

FCPrint[1,"FCClearScalarProducts.m loaded"];
End[]
