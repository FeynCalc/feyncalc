(* Wolfram Language package *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCClearScalarProducts												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  Clears definitions of scalar products						    *)

(* ------------------------------------------------------------------------ *)

FCClearScalarProducts::usage =
"FCClearScalarProducts[] removes all user-performed specific settings for
ScalarProduct's.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCClearScalarProducts`Private`"]

FCClearScalarProducts[OptionsPattern[]] :=
	(
		DownValues[Pair] = FeynCalc`Package`initialPairDownValues;
		DownValues[CartesianPair] = FeynCalc`Package`initialCartesianPairDownValues;
		DownValues[TemporalPair] = FeynCalc`Package`initialTemporalPairDownValues;
		DownValues[ScalarProduct] = FeynCalc`Package`initialScalarProductDownValues;
		UpValues[ScalarProduct] = FeynCalc`Package`initialScalarProductUpValues;
		DownValues[CartesianScalarProduct] = FeynCalc`Package`initialCartesianScalarProductDownValues;
		UpValues[CartesianScalarProduct] = FeynCalc`Package`initialCartesianScalarProductUpValues;
		DownValues[SP] = FeynCalc`Package`initialSPDownValues;
		DownValues[SPD] = FeynCalc`Package`initialSPDDownValues;
		DownValues[SPLR] = FeynCalc`Package`initialSPLRDownValues;
		DownValues[SPLRD] = FeynCalc`Package`initialSPLRDDownValues;
		DownValues[SPE] = FeynCalc`Package`initialSPEDownValues;
		DownValues[CSP] = FeynCalc`Package`initialCSPDownValues;
		DownValues[CSPD] = FeynCalc`Package`initialCSPDDownValues;
		DownValues[CSPE] = FeynCalc`Package`initialCSPEDownValues;
		DownValues[TC] = FeynCalc`Package`initialTCDownValues;
		DownValues[Momentum] = FeynCalc`Package`initialMomentumDownValues;
		DownValues[TemporalMomentum] = FeynCalc`Package`initialTemporalMomentumDownValues;
		DownValues[CartesianMomentum] = FeynCalc`Package`initialCartesianMomentumDownValues;
		DownValues[LightConePerpendicularComponent] = FeynCalc`Package`initialLightConePerpendicularComponentDownValues;

		$ScalarProducts = FeynCalc`Package`initialScalarProducts;
	);

FCPrint[1,"FCClearScalarProducts.m loaded"];
End[]
