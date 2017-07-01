(* Wolfram Language package *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCClearScalarProducts												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
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
		DownValues[CartesianPair] = initialCartesianPairDownValues;
		DownValues[TemporalPair] = initialTemporalPairDownValues;
		DownValues[ScalarProduct] = initialScalarProductDownValues;
		UpValues[ScalarProduct] = initialScalarProductUpValues;
		DownValues[CartesianScalarProduct] = initialCartesianScalarProductDownValues;
		UpValues[CartesianScalarProduct] = initialCartesianScalarProductUpValues;
		DownValues[SP] = initialSPDownValues;
		DownValues[SPD] = initialSPDDownValues;
		DownValues[SPE] = initialSPEDownValues;
		DownValues[CSP] = initialCSPDownValues;
		DownValues[CSPD] = initialCSPDDownValues;
		DownValues[CSPE] = initialCSPEDownValues;
		DownValues[TC] = initialTCDownValues;
		DownValues[Momentum] = initialMomentumDownValues;
		DownValues[TemporalMomentum] = initialTemporalMomentumDownValues;
		DownValues[CartesianMomentum] = initialCartesianMomentumDownValues;
		$ScalarProducts = initialScalarProducts;
	);

FCPrint[1,"FCClearScalarProducts.m loaded"];
End[]
