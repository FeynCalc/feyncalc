(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopFindSectors											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary:  	Identifies sectors in a list of GLI integrals				*)

(* ------------------------------------------------------------------------ *)

FCLoopFindSectors::usage =
"FCLoopFindSectors[{GLI[...], ...}] analyzes the indices of the GLI integrals
in the given list and identifies sectors to which they belong. Notice that
only GLIs with integer indices are supported.

If the option GatherBy is set to True (default), the output will be a list of
two lists, where the former contains the original integrals sorted w.r.t the
identified sectors, while the latter is a list of all available sectors.

For GatherBy->False, the output is a list containing all identified sectors
without the original integrals.

Setting the option Last to Truewill return only the top sector.";

FCLoopFindSectors::failmsg = "Error! FCLoopFindSectors has encountered a fatal problem and must abort the computation. \n
The problem reads: `1`";

Begin["`Package`"]
End[]

Begin["`FCLoopFindSectors`Private`"];

fclfsVerbose::usage="";


Options[FCLoopFindSectors] = {
	FCVerbose		-> False,
	GatherBy		-> True,
	Last			-> False,
	MaxIterations	-> Infinity
};

FCLoopFindSectors[{}, OptionsPattern[]] :=
	{};

FCLoopFindSectors[glis : {__GLI}, OptionsPattern[]] :=
	Block[{	sectorsList, optGatherBy, auxList, simplifiedList,
			reducedList, tmp, idold, repRule, finalList,
			res, time, optMaxIterations},

		optMaxIterations	= OptionValue[MaxIterations];
		optGatherBy			= OptionValue[GatherBy];

		If [OptionValue[FCVerbose]===False,
				fclfsVerbose=$VeryVerbose,
				If[MatchQ[OptionValue[FCVerbose], _Integer],
					fclfsVerbose=OptionValue[FCVerbose]
				];
		];

		FCPrint[1,"FCLoopFindSectors: Entering.", FCDoControl->fclfsVerbose];
		FCPrint[3,"FCLoopFindSectors: Entering with: ", glis, FCDoControl->fclfsVerbose];


		time=AbsoluteTime[];
		FCPrint[1,"FCLoopFindSectors: Extracting sectors. ", FCDoControl->fclfsVerbose];
		sectorsList = getSector /@ glis;
		FCPrint[1, "FCLoopFindSectors: Done extracting sectors, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclfsVerbose];

		If[!FreeQ[sectorsList, HeavisideTheta],
			Message[FCLoopFindSectors::failmsg,"GLIs with noninteger indices are not supported."];
			Abort[];
		];

		If[	TrueQ[optGatherBy],

			time=AbsoluteTime[];
			FCPrint[1,"FCLoopFindSectors: Generating sorted list of GLIs using GatherBy. ", FCDoControl->fclfsVerbose];
			auxList = Transpose[{sectorsList, glis}];
			auxList = GatherBy[auxList, First];
			auxList = Transpose/@auxList;
			auxList = Map[{#[[1]][[1]], #[[2]]} &, auxList];
			auxList = SortBy[auxList, Count[First[#], 1, Infinity] &];
			FCPrint[1, "FCLoopFindSectors: Done generating sorted list of GLIs, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclfsVerbose];
			res = {auxList,First/@auxList};
			If[	OptionValue[Last],
				res = Last[res[[1]]]
			],
			(*otherwise*)
			res = SortBy[Union[sectorsList], Count[First[#], 1, Infinity] &];
			If[	OptionValue[Last],
				res = Last[res]
			]
		];

		FCPrint[1,"FCLoopFindSectors: Leaving.", FCDoControl->fclfsVerbose];
		res
	];

(*Safe for memoization*)
getSector[GLI[id_, pows:{__Integer}]]:=
	MemSet[getSector[GLI[id, pows]],
		HeavisideTheta[pows - 1/2]
	];



End[]
