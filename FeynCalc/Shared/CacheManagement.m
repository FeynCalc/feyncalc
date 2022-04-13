(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CacheManagement													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2022 Rolf Mertig
	Copyright (C) 1997-2022 Frederik Orellana
	Copyright (C) 2014-2022 Vladyslav Shtabovenko
*)

(* :Summary: 	Functions for managing memoization in FeynCalc				*)

(* ------------------------------------------------------------------------ *)

FCUseCache::usage=
"FCUseCache[func,{arg1,...},{opt1...}] evaluates func[arg1,...,opt1,...] and
caches the result such that the next evaluation of same expressions occurs
almost immediately. This caching also takes into account DownValues and global
variables that enter into evaluation of func.

For example, ExpandScalarProduct can't be naively cached, because its result
depends on the DownValues of Pair and ScalarProduct, which may be changed
multiple times during the session by setting and erasing values of scalar
products. With FCUseCache, however, caching will work properly, as FCUseCache
knows the dependence on ExpandScalarProduct on those DownValues. For all this
to work, a function should be explicitly white-listed in FCUseCache.";

FCShowCache::usage =
"FCShowCache[func] shows existing cached values for the function func, that
were introduced by FCUseCache.";

FCClearCache::usage =
"FCClearCache[func] removes existing cached values for the function func that
were introduced by FCUseCache.

To remove all existing cache values use FCClearCache[All].";

FCUseCache::blacklist=
"The function `1` is not whitelisted for FCUseCache. Evaluation aborted!";

FCUseCache::fail=
"Error! FCUseCache has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";


Begin["`Package`"];
End[]
(* ------------------------------------------------------------------------ *)

Begin["`CacheManagement`Private`"];

whiteListNames = {
	ExpandScalarProduct,
	FactorList2,
	PairContract,
	FCFastContract,
	FeynCalc`NPointTo4Point`Private`getDet,
	FeynCalc`SimplifyPolyLog`Private`simplifyArgument,
	FeynCalc`FCApart`Private`pfracRaw,
	FeynCalc`Package`momentumRoutingDenner
};

standardSetAssociation = {};

If[	$VersionNumber >= 10. ,
	standardSetAssociation = Association[{}];
];

FCUseCache[fcFunc_, args_List, opts_List: {}] :=
	Block[{fullOpts, cachedHead,depArgs, standardSet, savedStandardSet},
		fullOpts = Sort[Flatten[Join[opts, FilterRules[Options[fcFunc], Except[opts]]]]];
		cachedHead=ToExpression["cacheFunc"<>ToString[fcFunc]];

		If[	MemberQ[whiteListNames,fcFunc],

			cachedHead[arg_, cargs_, ops_] :=
				MemSet[cachedHead[arg, cargs, ops], fcFunc[Sequence @@ arg, ops]],
			Message[FCUseCache::blacklist,fcFunc];
			Abort[]
		];

		standardSet = DownValues[#]&/@{
				Pair, CartesianPair, TemporalPair, ScalarProduct, CartesianScalarProduct,
				Momentum, CartesianMomentum, TemporalMomentum, SP, SPD, SPE, CSP, CSPD, CSPE, TC
		};
		standardSet = Join[standardSet,{FeynCalc`Package`DiracGammaScheme, FeynCalc`Package`PauliSigmaScheme}];

		Which[
			fcFunc === FactorList2,
				depArgs = Hash[{}],
			fcFunc === ExpandScalarProduct,
				depArgs = Hash[standardSet],
			fcFunc === PairContract,
				depArgs = Hash[standardSet],
			fcFunc === FCFastContract,
				depArgs = Hash[standardSet],
			fcFunc === FeynCalc`NPointTo4Point`Private`getDet,
				depArgs = Hash[standardSet],
			fcFunc === FeynCalc`SimplifyPolyLog`Private`simplifyArgument,
				depArgs = Hash[standardSet],
			fcFunc === FeynCalc`FCApart`Private`pfracRaw,
				depArgs = Hash[standardSet],
			fcFunc === FeynCalc`Package`momentumRoutingDenner,
				depArgs = Hash[standardSet],
			True,
				Message[FCUseCache::blacklist,fcFunc];
				Abort[]
		];

		If[	$VersionNumber >= 10. ,
			savedStandardSet = Key[depArgs][standardSetAssociation];
			If[	MissingQ[savedStandardSet],
				standardSetAssociation = Append[standardSetAssociation,{depArgs -> standardSet}],
				If[	savedStandardSet=!=standardSet,
					Message[FCUseCache::fail,"Hash collision detected."];
					Abort[]
				];
			];
		];

		cachedHead[args,depArgs,fullOpts]
	];

FCShowCache[fcFunc_] :=
	With[{i = ToExpression["cacheFunc" <> ToString[fcFunc]]},DownValues[i]];

FCClearCache[fcFunc_] :=
	With[{i = ToExpression["cacheFunc" <> ToString[fcFunc]]},DownValues[i] = {};];


FCClearCache[All]:=
	FCClearCache /@ whiteListNames;


FCPrint[1,"CacheManagement.m loaded"];
End[]
