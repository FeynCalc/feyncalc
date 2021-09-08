(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCMatchSolve														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: 	Solves for special variables								*)

(* ------------------------------------------------------------------------ *)

FCMatchSolve::usage=
"FCMatchSolve[expr, {notvar1, notvar2, ...}] assumes that expr is a sum that
must vanish term-wise and converts it to a system of linear equations. The
function automatically determines which variables to solve for, excluding
notvar1, notvar2, ... from the list.

FCMatchSolve can also handle overdetermined systems of equations. This
function is useful e.g. for determining renormalization constants or matching
coefficients, where looking at each term separately and determining the values
of the constants/coefficients by hand is too tedious.

The input (say a sum or a difference of amplitudes) should be prepared using
Collect2 by collecting w.r.t distinct objects, e.g. matrix elements or
coupling constants so that each term must vanish separately.";

FCMatchSolve::failmsg =
"FCMatchSolve has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

FCMatchSolve::multsol =
"The solutions is not unique!";


Begin["`Package`"];

End[]
(* ------------------------------------------------------------------------ *)

Begin["`FCMatchSolve`Private`"];

fcmsVerbose::usage="";

Options[FCMatchSolve] = {
	FCE			-> False,
	FCI			-> False,
	FCVerbose	-> False,
	Method		-> Automatic,
	MaxIterations -> Infinity,
	Factoring	-> Factor2,
	Reduce		-> True
};


FCMatchSolve[expr_, notvars_List/; (!OptionQ[notvars] || notvars==={}), OptionsPattern[]] :=
	Block[{	ex, equals, eqSys, eqSol, vars, varsToRemove, nVarsToRemove,
			nVarsAlreadyRemoved, optFactoring, allEqVars, trivialEqs, preSol},

		optFactoring = OptionValue[Factoring];
		preSol = {};

		If [OptionValue[FCVerbose]===False,
			fcmsVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fcmsVerbose=OptionValue[FCVerbose]
			];
		];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		FCPrint[1, "FCMatchSolve: Entering.", FCDoControl->fcmsVerbose];
		FCPrint[3, "FCMatchSolve: Entering with", ex, FCDoControl->fcmsVerbose];

		If[	ex === 0,
			(* Nothing to do*)
			Return[{}]
		];

		If[	Head[ex]===Plus,
			eqSys = Map[equals[#, 0] &, (List @@ ex)],
			eqSys = {equals[ex,0]}
		];
		FCPrint[3, "FCMatchSolve: Raw system of equations: ", eqSys, FCDoControl->fcmsVerbose];

		vars = SelectFree[Variables2[eqSys /. equals->Equal], notvars];
		FCPrint[3, "FCMatchSolve: Raw  list of variables: ", vars, FCDoControl->fcmsVerbose];


		eqSys = DeleteDuplicates[eqSys/.equals[a_,0]:>equals[Last[FCProductSplit[a,vars]],0]];

		eqSys = SortBy[eqSys /. equals->Equal,LeafCount];
		FCPrint[2, "FCMatchSolve: Final system of equations: ", eqSys, FCDoControl->fcmsVerbose];

		If[	!FreeQ[eqSys,False],
			FCPrint[0, Style["FCMatchSolve: One or more terms contain no variables to solve for. No solutions possible.",
				{Darker[Red,0.55], Bold}], FCDoControl->fcmsVerbose];
			Return[{}]
		];

		(*Presolve the system by removing trivial equations*)
		FCPrint[3, "FCMatchSolve: Removing trivial equation.", FCDoControl->fcmsVerbose];
		{eqSys,vars,preSol} = Most[FixedPoint[preSolve@@#&,{eqSys,vars,{},notvars}, OptionValue[MaxIterations]]];

		If[	!FreeQ[eqSys,False],
			FCPrint[0, Style["FCMatchSolve: One or more terms contain no variables to solve for. No solutions possible.",
				{Darker[Red,0.55], Bold}], FCDoControl->fcmsVerbose];
			Return[{}]
		];

		If[	preSol=!={},
			FCPrint[0, "FCMatchSolve: Following coefficients trivially vanish: ", preSol, FCDoControl->fcmsVerbose];
			eqSys = SortBy[eqSys /. equals->Equal,LeafCount];
			FCPrint[2, "FCMatchSolve: Updated system of equations: ", eqSys, FCDoControl->fcmsVerbose];
		];

		allEqVars = (SelectFree[Variables[#], notvars] & /@ (First /@ eqSys));

		nVarsToRemove = Length[eqSys] - Length[vars];
		If[	nVarsToRemove > 0 && OptionValue[Reduce],

			FCPrint[1, "FCMatchSolve: Seemingly overdetermined system of equations. Reducing the list of variables.", FCDoControl->fcmsVerbose];
			varsToRemove = {};
			nVarsAlreadyRemoved = 0;
			Map[
				If[	canRemoveVarsQ[allEqVars, Join[varsToRemove, {#}]] && (nVarsAlreadyRemoved < nVarsToRemove),
					nVarsAlreadyRemoved++;
					varsToRemove = Join[varsToRemove, {#}]
				]&, vars];
			FCPrint[0, "FCMatchSolve: Following variables will be treated as free parameters: ", varsToRemove, FCDoControl->fcmsVerbose];
			vars = SelectFree[vars, varsToRemove];

		];

		FCPrint[0, "FCMatchSolve: Solving for: ", vars, FCDoControl->fcmsVerbose];

		Quiet[eqSol = Solve[eqSys, vars, Method->OptionValue[Method]];,{Solve::svars}];

		If[	eqSol === {},
			FCPrint[0, Style["FCMatchSolve: No solutions found.", {Darker[Red,0.55], Bold}], FCDoControl->fcmsVerbose];
			Return[{}],

			If[	Length[eqSol]>1,
				Message[FCMatchSolve::multsol],
				eqSol = First[eqSol]
			]
		];

		eqSol = Join[preSol,eqSol];

		If[ !MatchQ[Last/@eqSol,{0..}],
				FCPrint[0, Style["FCMatchSolve: A solution exists.", {Darker[Green,0.55], Bold}] , FCDoControl->fcmsVerbose],
				FCPrint[0, Style["FCMatchSolve: Only a trivial solution exists.", {Darker[Yellow,0.55], Bold}] , FCDoControl->fcmsVerbose];
		];

		If[ optFactoring=!=False,
			eqSol = optFactoring[eqSol]
		];


		If[	OptionValue[FCE],
			eqSol = FCE[eqSol]
		];

		FCPrint[1, "FCMatchSolve: Leaving.", FCDoControl->fcmsVerbose];

		FCPrint[3, "FCMatchSolve: Leaving with ", eqSol,  FCDoControl->fcmsVerbose];

		eqSol
	];

preSolve[eqSys_List, vars_List, zeroVars_List, notvars_List]:=
	{eqSys,vars,zeroVars,notvars}/;!FreeQ[eqSys,False];

preSolve[eqSys_List, vars_List, zeroVars_List, notvars_List]:=
Block[{	trivialEqs,newZeroVars, newVars, newEqSys, res},

	trivialEqs = SelectFree[eqSys,Plus];
	FCPrint[3, "FCMatchSolve: preSolve: Trivial equations: ", trivialEqs, FCDoControl->fcmsVerbose];

	If[	TrueQ[trivialEqs=!={}],
		If[	And@@(MatchQ[#,(c_. x_)/;FreeQ2[c,vars] && MemberQ[vars,x]]/@trivialEqs),
			Message[FCMatchSolve::failmsg,"Failed to extract trivial equatons"];
			Abort[]
		];

		newZeroVars = Union[zeroVars, Thread[Rule[SelectFree[Variables2[trivialEqs], notvars],0]]];
		FCPrint[3, "FCMatchSolve: preSolve: Trivially vanishing coefficients: ", newZeroVars, FCDoControl->fcmsVerbose];

		newEqSys = ReplaceAll[Together/@(eqSys/.Dispatch[newZeroVars]), True->Unevaluated[Sequence[]]];

		FCPrint[2, "FCMatchSolve: preSolve: Updated system of equations: ", newEqSys, FCDoControl->fcmsVerbose];

		newVars = SelectFree[Variables2[newEqSys], notvars];
		FCPrint[3, "FCMatchSolve: preSolve: Updated raw list of variables: ", vars, FCDoControl->fcmsVerbose];

		res = {newEqSys, newVars, newZeroVars, notvars},

		res = {eqSys,vars,zeroVars,notvars}
	];

	res

]/; FreeQ[eqSys,False];

canRemoveVarsQ[allEqVars_, vars_] :=
	FreeQ[SelectFree[#, Sequence @@ vars] & /@ allEqVars, {}];


FCPrint[1,"FCMatchSolve.m loaded"];
End[]
