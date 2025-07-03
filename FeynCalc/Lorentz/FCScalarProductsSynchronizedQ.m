(* Wolfram Language package *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCScalarProductsSynchronizedQ											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  Compares definitions of scalar products between kernels	    *)

(* ------------------------------------------------------------------------ *)

FCScalarProductsSynchronizedQ::usage =
"FCScalarProductsSynchronizedQ[] compares up and down values of scalar products
and other kinematic-related symbols such as Momentum, CartesianMomentum, TC
etc.
between the master kernel and the subkernels and returns True if all of them
identical.

This routine is only relevant in the parallel mode of FeynCalc. It helps to
avoid inconsistencies through definitions that were introduced before
activating the parallel mode and not correctly propagated to the subkernels";

FCScalarProductsSynchronizedQ::nokernels =
"FeynCalc is currently not in the parallel mode, so no checks are being done.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCScalarProductsSynchronizedQ`Private`"]

Options[FCScalarProductsSynchronizedQ] = {
	FCVerbose->False
};


compareValues[head_,vtype_,spsVerbose_]:=
	Block[{currentKernel,parallelKernels},
	currentKernel = vtype[head];
		parallelKernels = With[{xxx=head,yyy=vtype},ParallelEvaluate[yyy[xxx], DistributedContexts -> None]];
		Table[
		If[	Length[currentKernel]=!=Length[parallelKernels[[i]]],
			FCPrint[1,"FCScalarProductsSynchronizedQ: Length missmatch between downvalues for "<>ToString[head]<>" on the master kernel and a subkernel: ",
				Length[currentKernel]," vs ",Length[parallelKernels[[i]]], FCDoControl->spsVerbose];
			Throw[False]
		];
		If[	currentKernel=!=parallelKernels[[i]],
			FCPrint[1,"FCScalarProductsSynchronizedQ: Value missmatch between downvalues for "<>ToString[head]<>" on the master kernel and a subkernel.", FCDoControl->spsVerbose];
			Throw[False]
		];,{i,1,$KernelCount}];
		True
	];

FCScalarProductsSynchronizedQ[OptionsPattern[]] :=
	Block[{currentKernel,parallelKernels, res, spsVerbose, lensPK},

		If[	OptionValue[FCVerbose]===False,
				spsVerbose=$VeryVerbose,
				If[MatchQ[OptionValue[FCVerbose], _Integer],
					spsVerbose=OptionValue[FCVerbose]
				];
		];

		FCPrint[1,"FCScalarProductsSynchronizedQ: Comparing downvalues for Pair." FCDoControl->spsVerbose];

		If[	$ParallelizeFeynCalc,
			res = Catch[
				Join[Map[compareValues[#,DownValues,spsVerbose]&,{
				Pair,
				CartesianPair,
				TemporalPair,
				ScalarProduct,
				CartesianScalarProduct,
				SP,
				SPD,
				SPLR,
				SPLRD,
				SPE,
				CSP,
				CSPD,
				CSPE,
				TC,
				Momentum,
				TemporalMomentum,
				CartesianMomentum,
				LightConePerpendicularComponent}],

				Map[compareValues[#,UpValues,spsVerbose]&,{
				ScalarProduct,
				CartesianScalarProduct}](*,

				Map[compareValues[#,OwnValues,spsVerbose]&,{
				$ScalarProducts}]*)


			]],

			Message[FCScalarProductsSynchronizedQ::nokernels];
			Return[True]
		];

		If[	MatchQ[res,{True..}],
				res=True,
				res=False
		];

		res
	];



FCPrint[1,"FCScalarProductsSynchronizedQ.m loaded"];
End[]


