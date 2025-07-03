(* Wolfram Language package *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCValuesSynchronizedQ											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  Compares definitions of scalar products between kernels	    *)

(* ------------------------------------------------------------------------ *)

FCValuesSynchronizedQ::usage =
"FCValuesSynchronizedQ[{val1, val2}, type] compares  definitions of val1, val2,
...
between the master kernel and the subkernels and returns True if all of them
identical. Supported types are DownValues, UpValues and OwnValues.

This routine is only relevant in the parallel mode of FeynCalc. It helps to
avoid inconsistencies through definitions that were introduced before
activating the parallel mode and not correctly propagated to the subkernels";

FCValuesSynchronizedQ::nokernels =
"FeynCalc is currently not in the parallel mode, so no checks are being done.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCValuesSynchronizedQ`Private`"]

Options[FCValuesSynchronizedQ] = {
	FCVerbose->False
};


compareValues[head_,vtype_,spsVerbose_]:=
	Block[{currentKernel,parallelKernels},
	currentKernel = vtype[head];
		parallelKernels = With[{xxx=head,yyy=vtype},ParallelEvaluate[yyy[xxx], DistributedContexts -> None]];
		Table[
		If[	Length[currentKernel]=!=Length[parallelKernels[[i]]],
			FCPrint[1,"FCValuesSynchronizedQ: Length missmatch between downvalues for "<>ToString[head]<>" on the master kernel and a subkernel: ",
				Length[currentKernel]," vs ",Length[parallelKernels[[i]]], FCDoControl->spsVerbose];
			Throw[False]
		];
		If[	currentKernel=!=parallelKernels[[i]],
			FCPrint[1,"FCValuesSynchronizedQ: Value missmatch between downvalues for "<>ToString[head]<>" on the master kernel and a subkernel.", FCDoControl->spsVerbose];
			Throw[False]
		];,{i,1,$KernelCount}];
		True
	];

FCValuesSynchronizedQ[s_/;Head[s]=!=List, rest___] :=
	FCValuesSynchronizedQ[{s}, rest];

FCValuesSynchronizedQ[symbols_List, vtype_/;MemberQ[{DownValues,UpValues,OwnValues},vtype],OptionsPattern[]] :=
	Block[{currentKernel,parallelKernels, res, spsVerbose, lensPK},

		If[	OptionValue[FCVerbose]===False,
				spsVerbose=$VeryVerbose,
				If[MatchQ[OptionValue[FCVerbose], _Integer],
					spsVerbose=OptionValue[FCVerbose]
				];
		];

		FCPrint[1,"FCValuesSynchronizedQ: Comparing " <> ToString[vtype] <> " for the given symbols", FCDoControl->spsVerbose];

		If[	$ParallelizeFeynCalc,
			res = Catch[
				Join[Map[compareValues[#,vtype,spsVerbose]&,symbols]
			]],

			Message[FCValuesSynchronizedQ::nokernels];
			Return[True]
		];

		If[	MatchQ[res,{True..}],
				res=True,
				res=False
		];

		res
	];



FCPrint[1,"FCValuesSynchronizedQ.m loaded"];
End[]


