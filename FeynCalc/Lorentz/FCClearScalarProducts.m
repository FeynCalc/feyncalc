(* Wolfram Language package *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCClearScalarProducts											*)

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

Options[FCClearScalarProducts] = {
	FCParallelize			-> True,
	FCVerbose				-> False
};


FCClearScalarProducts[OptionsPattern[]] :=
	Block[{fccspVerbose},

		If [OptionValue[FCVerbose]===False,
				fccspVerbose=$VeryVerbose,
				If[MatchQ[OptionValue[FCVerbose], _Integer],
					fccspVerbose=OptionValue[FCVerbose]
				];
		];


		If[	$ParallelizeFeynCalc && OptionValue[FCParallelize],

				FCPrint[1,"FCClearScalarProducts: Clearing scalar products and other up or down values on subkernels.", FCDoControl->fccspVerbose];
				ParallelEvaluate[(
					DownValues[Pair] 							= FeynCalc`Package`initialPairDownValues;
					DownValues[CartesianPair]					= FeynCalc`Package`initialCartesianPairDownValues;
					DownValues[TemporalPair]					= FeynCalc`Package`initialTemporalPairDownValues;
					DownValues[ScalarProduct]					= FeynCalc`Package`initialScalarProductDownValues;
					UpValues[ScalarProduct]						= FeynCalc`Package`initialScalarProductUpValues;
					DownValues[CartesianScalarProduct]			= FeynCalc`Package`initialCartesianScalarProductDownValues;
					UpValues[CartesianScalarProduct]			= FeynCalc`Package`initialCartesianScalarProductUpValues;
					DownValues[SP] 								= FeynCalc`Package`initialSPDownValues;
					DownValues[SPD] 							= FeynCalc`Package`initialSPDDownValues;
					DownValues[SPLR] 							= FeynCalc`Package`initialSPLRDownValues;
					DownValues[SPLRD] 							= FeynCalc`Package`initialSPLRDDownValues;
					DownValues[SPE] 							= FeynCalc`Package`initialSPEDownValues;
					DownValues[CSP] 							= FeynCalc`Package`initialCSPDownValues;
					DownValues[CSPD] 							= FeynCalc`Package`initialCSPDDownValues;
					DownValues[CSPE] 							= FeynCalc`Package`initialCSPEDownValues;
					DownValues[TC] 								= FeynCalc`Package`initialTCDownValues;
					DownValues[Momentum] 						= FeynCalc`Package`initialMomentumDownValues;
					DownValues[TemporalMomentum] 				= FeynCalc`Package`initialTemporalMomentumDownValues;
					DownValues[CartesianMomentum] 				= FeynCalc`Package`initialCartesianMomentumDownValues;
					DownValues[LightConePerpendicularComponent]	= FeynCalc`Package`initialLightConePerpendicularComponentDownValues;
					$ScalarProducts								= FeynCalc`Package`initialScalarProducts;
					);,	DistributedContexts -> None]
		];

		FCPrint[1,"FCClearScalarProducts: Clearing scalar products and other up or down values on the master kernel", FCDoControl->fccspVerbose];

		(
			DownValues[Pair] 							= FeynCalc`Package`initialPairDownValues;
			DownValues[CartesianPair]					= FeynCalc`Package`initialCartesianPairDownValues;
			DownValues[TemporalPair]					= FeynCalc`Package`initialTemporalPairDownValues;
			DownValues[ScalarProduct]					= FeynCalc`Package`initialScalarProductDownValues;
			UpValues[ScalarProduct]						= FeynCalc`Package`initialScalarProductUpValues;
			DownValues[CartesianScalarProduct]			= FeynCalc`Package`initialCartesianScalarProductDownValues;
			UpValues[CartesianScalarProduct]			= FeynCalc`Package`initialCartesianScalarProductUpValues;
			DownValues[SP] 								= FeynCalc`Package`initialSPDownValues;
			DownValues[SPD] 							= FeynCalc`Package`initialSPDDownValues;
			DownValues[SPLR] 							= FeynCalc`Package`initialSPLRDownValues;
			DownValues[SPLRD] 							= FeynCalc`Package`initialSPLRDDownValues;
			DownValues[SPE] 							= FeynCalc`Package`initialSPEDownValues;
			DownValues[CSP] 							= FeynCalc`Package`initialCSPDownValues;
			DownValues[CSPD] 							= FeynCalc`Package`initialCSPDDownValues;
			DownValues[CSPE] 							= FeynCalc`Package`initialCSPEDownValues;
			DownValues[TC] 								= FeynCalc`Package`initialTCDownValues;
			DownValues[Momentum] 						= FeynCalc`Package`initialMomentumDownValues;
			DownValues[TemporalMomentum] 				= FeynCalc`Package`initialTemporalMomentumDownValues;
			DownValues[CartesianMomentum] 				= FeynCalc`Package`initialCartesianMomentumDownValues;
			DownValues[LightConePerpendicularComponent]	= FeynCalc`Package`initialLightConePerpendicularComponentDownValues;
			$ScalarProducts								= FeynCalc`Package`initialScalarProducts;
		);
		FCPrint[1,"FCClearScalarProducts: Done clearing scalar products and other up or down values.", FCDoControl->fccspVerbose];
	];



FCPrint[1,"FCClearScalarProducts.m loaded"];
End[]
