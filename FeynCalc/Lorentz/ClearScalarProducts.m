(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ClearScalarProducts *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 21 December '98 at 0:06 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: clearing definitions of scalar products *)

(* ------------------------------------------------------------------------ *)

ClearScalarProducts::usage =
"ClearScalarProducts removes all user-performed
specific settings for ScalarProduct's.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ClearScalarProducts`Private`"]

ClearScalarProducts :=
	(
		DownValues[Pair] = initialPairDownValues;
		DownValues[ScalarProduct] = initialScalarProductDownValues;
		UpValues[ScalarProduct] = initialScalarProductUpValues;
		DownValues[SP] = initialSPDownValues;
		DownValues[SPD] = initialSPDDownValues;
	);

FCPrint[1,"ClearScalarProducts.m loaded"];
End[]
