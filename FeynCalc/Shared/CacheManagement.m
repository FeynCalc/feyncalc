(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CacheManagement													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary: 	Functions for managing memoization in FeynCalc				*)

(* ------------------------------------------------------------------------ *)

FCUseCache::usage=
"FCUseCache[func,{arg1,...},{opt1...}] evaluates \
func[arg1,...,opt1,...] and caches the result such that the \
next evaluation of same expressions occurs almost immediately. \
This caching also takes into account DownValues and global variables \
that enter into evaluation of func. For example, ExpandScalarProduct \
can't be naively cached, because it's result depends on the DownValues \
of Pair and ScalarProduct, which may be changed multiple times during \
the session by setting and erasing values of scalar products. With \
FCUseCache, however, caching will work properly, as FCUseCache knows \
the dependence on ExpandScalarProduct on those DownValues. \
\
For all this to work, a function should be explicitly whitelisted in FCUseCache.";

FCShowCache::usage =
"FCShowCache[func] shows existing cached values for the function func, that \
were introduced by FCUseCache";

FCClearCache::usage =
"FCClearCache[func] removes existing cached values for the function func, that \
were introduced by FCUseCache";

FCUseCache::blacklist=
"The function `1` is not whitelisted for FCUseCache. Evaluation aborted!"


Begin["`Package`"]
End[]
(* ------------------------------------------------------------------------ *)

Begin["`CacheManagement`Private`"]


SetAttributes[cachedToString, HoldAll]


whiteListNames = {
	ExpandScalarProduct,
	PairContract,
	FCFastContract
};

FCUseCache[fcFunc_, args_List, opts_List: {}] :=
	Block[{fullOpts, cachedHead,depArgs},
		fullOpts = Sort[Flatten[Join[opts, FilterRules[Options[fcFunc], Except[opts]]]]];
		cachedHead=ToExpression["cacheFunc"<>ToString[fcFunc]];

		If[	MemberQ[whiteListNames,fcFunc],
			cachedHead[arg_, cargs_String, ops_] :=
				MemSet[cachedHead[arg, cargs, ops], fcFunc[Sequence @@ arg, ops]],
			Message[FCUseCache::blacklist,fcFunc];
			Abort[]
		];

		Which[
			fcFunc === ExpandScalarProduct,
				depArgs = cachedToString[{	DownValues[Pair],
											DownValues[ScalarProduct],
											DownValues[SP],
											DownValues[SPD],
											DownValues[SPE],
											$BreitMaison,
											$Larin}],
			fcFunc === PairContract,
				depArgs = cachedToString[{	DownValues[Pair],
											DownValues[ScalarProduct],
											DownValues[SP],
											DownValues[SPD],
											DownValues[SPE],
											$BreitMaison,
											$Larin}],
			fcFunc === FCFastContract,
				depArgs = cachedToString[{	DownValues[Pair],
											DownValues[ScalarProduct],
											DownValues[SP],
											DownValues[SPD],
											DownValues[SPE],
											$BreitMaison,
											$Larin}],
			True,
				Message[FCUseCache::blacklist,fcFunc];
				Abort[]
		];
		cachedHead[args,depArgs,fullOpts]
	];

FCShowCache[fcFunc_] :=
	With[{i = ToExpression["cacheFunc" <> ToString[fcFunc]]},DownValues[i]];

FCClearCache[fcFunc_] :=
	With[{i = ToExpression["cacheFunc" <> ToString[fcFunc]]},DownValues[i] = {};];


cachedToString[x_] :=
	cachedToString[x] = ToString[x];


FCPrint[1,"CacheManagement.m loaded"];
End[]
