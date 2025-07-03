(* Wolfram Language package *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCClearDataTypes											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  Clears definitions of scalar products						    *)

(* ------------------------------------------------------------------------ *)

FCClearDataTypes::usage =
"FCClearDataTypes[] removes all user-defined attachments of data types to
variables.

To remove all existing cache values use FCClearCache[All].";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCClearDataTypes`Private`"]

Options[FCClearDataTypes] = {
	FCParallelize			-> True,
	FCVerbose				-> False
};


FCClearDataTypes[OptionsPattern[]] :=
	Block[{fccspVerbose},

		If [OptionValue[FCVerbose]===False,
				fccspVerbose=$VeryVerbose,
				If[MatchQ[OptionValue[FCVerbose], _Integer],
					fccspVerbose=OptionValue[FCVerbose]
				];
		];


		If[	$ParallelizeFeynCalc && OptionValue[FCParallelize],

				FCPrint[1,"FCClearDataTypes: Clearing DataType values on subkernels.", FCDoControl->fccspVerbose];
				ParallelEvaluate[(
					DownValues[DataType] = FeynCalc`Package`initialDataTypeDownValues;
					);,	DistributedContexts -> None]
		];

		FCPrint[1,"FCClearDataTypes: Clearing DataType values on the master kernel.", FCDoControl->fccspVerbose];

		DownValues[DataType] = FeynCalc`Package`initialDataTypeDownValues;
		FCPrint[1,"FCClearDataTypes: Done clearing DataType values on the master kernel.", FCDoControl->fccspVerbose];
	];



FCPrint[1,"FCClearDataTypes.m loaded"];
End[]
