(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopFindTensorBasis											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  	Suggests a basis for zero Gram determinants					*)

(* ------------------------------------------------------------------------ *)

FCLoopFindTensorBasis::usage =
"FCLoopFindTensorBasis[{moms}, {rules}, n] checks if the external momenta moms
form a basis by calculating their Gram determinant and inserting the supplied
rules for the scalar products.

A vanishing Gram determinant signals a linear dependence between those
momenta. In this case a loop integral depending on these momenta cannot be
tensor reduced in the usual way.

To circumvent this issue the function will suggest an alternative set of
external vectors with respect to which the tensor reduction should be done. If
some of the old vectors can be expressed in terms of the new ones, the
corresponding rules will be provided as well.

If some of the external momenta are light-like (i.e. their scalar products
vanish), then an auxiliary vector n must be added to the basis. The scalar
products of this vector with the existing momenta will form new kinematic
invariants appearing in the result of the tensor reduction. The values of
these invariants can be arbitrary, except that they must be nonvanishing. Upon
doing the tensor reduction in this way, one will still need to perform an IBP
reduction of the resulting scalar integrals. These integrals will depend on
the new kinematic invariants but as the invariants should cancel in the final
result for the reduced tensor integral. To see this cancellation explicitly
one might need to use the linear relations between the external momenta
uncovered by FCLoopFindTensorBasis<<Feyn

Using the option All one can get all possible sets of new basis vectors. This
can be useful if one needs to select one of them for the tensor reduction.";

FCLoopFindTensorBasis::failmsg =
"Error! FCLoopFindTensorBasis has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

Begin["`Package`"]
End[]

Begin["`FCLoopFindTensorBasis`Private`"]

Options[FCLoopFindTensorBasis] = {
	Abort						-> True,
	FCI 						-> False,
	FCE							-> False,
	Dimension 					-> D,
	FCVerbose					-> False,
	All							-> False,
	Head						-> FCGV["Prefactor"],
	"NoZeroVectors"				-> False
};

(*
	Generic algorithm (for n>=2 external momenta):

	1)	Search for all sets of linearly dependent vectors by calculating the Gramian from
		all subsets of external momenta from 2 up to n

	2) 	Choose one of the momenta to be our basis vector. It is advisable to choose a vector
		that is not light-like (if possible). By analyzing the subsets from	1) work out a
		set of linearly independent and linearly dependent momenta. The former are our new
		basis vectors.

	3) 	Use subsets from 1) to express all linearly dependent momenta in terms of the new
		basis vectors. Notice that the available external momenta might not be sufficient
		to solve the resulting vector equations. In that case we may also add the auxiliary
		vector n^mu, that is understood to be chosen such, that n.k_i =!= 0 for every
		external momentum k_i.

	4) 	Return the list of linearly independent external momenta as well as a set of relations
		that express the remaining external momenta in terms of the linearly independent ones.
		Now the tensor reduction can be done with respect to the linearly independent momenta,
		while scalar products involving the other momenta can be eliminated using our rules.
*)

FCLoopFindTensorBasis[{}, __, OptionsPattern[]] :=
	{{},{},{}}

FCLoopFindTensorBasis[extmoms_List/;extmoms=!={}, kinRulesRaw_List, auxVec_, OptionsPattern[]] :=
	Block[{	gramDet,  kinRules, optDimension, newBasis, res, optAll,
			linDepRules, optHead, candidates, remVectors, linDepSets,
			linIndepSets, linDepVecs, linIndepVecs, candidatesAll, time, fclftbVerbose,
			linDepTwoVecs, tmp, eliminatedVecs, decompositions, extMomSquares,
			nonZeroSquares, firstBasisVector, optNoZeroVectors, check},

		If[	OptionValue[FCVerbose] === False,
			fclftbVerbose = $VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
			fclftbVerbose = OptionValue[FCVerbose]];
		];

		optDimension 		= OptionValue[Dimension];
		optHead				= OptionValue[Head];
		optNoZeroVectors	= OptionValue["NoZeroVectors"];
		optAll				= OptionValue[All];

		If[	OptionValue[FCI],
			kinRules = kinRulesRaw,
			kinRules = FCI[kinRulesRaw]
		];

		FCPrint[1, "FCLoopFindTensorBasis: Entering.", FCDoControl -> fclftbVerbose];
		FCPrint[2, "FCLoopFindTensorBasis: Set of external momenta: ", extmoms,  FCDoControl -> fclftbVerbose];

		gramDet = Together[FCGramDeterminant[extmoms, Dimension -> optDimension, FinalSubstitutions->kinRules]];

		FCPrint[2, "FCLoopFindTensorBasis: Gram determinant: ", gramDet,  FCDoControl -> fclftbVerbose];

		If[	TrueQ[gramDet=!=0],
			FCPrint[1, "FCLoopFindTensorBasis: The Gram determinant is not zero, leaving.", FCDoControl -> fclftbVerbose];
			Return[{extmoms,{},{}}]
		];

		If[	TrueQ[Length[extmoms]===1],

			(* Only one external momentum *)
			(* {linIndepVecs, linDepVecs, decompositions} *)
			res = {Join[extmoms,{auxVec}],{},{}};
			If[	optAll,
				res = {res}
			],

			(* More than one external momentum *)

			(* Building subsets of external momenta and calculating their Gram determinants *)
			candidates	= Subsets[extmoms,{2,Length[extmoms]}];
			tmp 		= (Together[FCGramDeterminant[#, Dimension -> optDimension, FinalSubstitutions->kinRules]])&/@candidates;
			candidates	= Transpose[{candidates, tmp}];


			extMomSquares = (Pair[Momentum[#,optDimension],Momentum[#,optDimension]]&/@extmoms) /. kinRules;
			extMomSquares = Transpose[{extmoms,extMomSquares}];
			nonZeroSquares = Select[extMomSquares,(#[[2]]=!=0)&];

			If[	nonZeroSquares=!={},
				firstBasisVector = nonZeroSquares[[1]][[1]],
				firstBasisVector = extmoms[[1]]
			];

			FCPrint[3, "FCLoopFindTensorBasis: First basis vector: ", firstBasisVector,  FCDoControl -> fclftbVerbose];

			FCPrint[3, "FCLoopFindTensorBasis: Non light-like external momenta: ", nonZeroSquares,  FCDoControl -> fclftbVerbose];

			FCPrint[3, "FCLoopFindTensorBasis: Subsets of all external momenta and their Gramians: ", candidates,  FCDoControl -> fclftbVerbose];

			(* Select subsets with vanishing Gramians *)
			linDepSets 		= Select[candidates, (#[[2]] === 0) &];
			linIndepSets	= Complement[candidates,linDepSets];

			FCPrint[3, "FCLoopFindTensorBasis: Vanishing Gramians: ", linDepSets,  FCDoControl -> fclftbVerbose];
			FCPrint[3, "FCLoopFindTensorBasis: Remaining subsets: ", linIndepSets,  FCDoControl -> fclftbVerbose];

			linDepSets 		= Transpose[linDepSets][[1]];

			If[linIndepSets=!={},
				linIndepSets 	= Transpose[linIndepSets];
			];

			If[	TrueQ[linIndepSets=!={}],
				linIndepSets = linIndepSets[[1]]
			];

			FCPrint[2, "FCLoopFindTensorBasis: Sets of linearly independent vectors: ", linIndepSets,  FCDoControl -> fclftbVerbose];
			FCPrint[2, "FCLoopFindTensorBasis: Sets of linearly dependent vectors: ", linDepSets,  FCDoControl -> fclftbVerbose];

			tmp = findNewBasis[#, extmoms, linIndepSets, linDepSets, optDimension, kinRules, optHead, optNoZeroVectors,
				auxVec, fclftbVerbose]&/@ extmoms;

			FCPrint[2, "FCLoopFindTensorBasis: Possible new  bases: ", tmp,  FCDoControl -> fclftbVerbose];

			If[	TrueQ[!optAll],
				(*It's better to have a basis that introduces the smallest number of additional invariants *)
				tmp = SortBy[tmp, Count[#, optHead] &];

				(*If possible, we want to have a basis without an auxiliary vector *)
				check = SelectFree[tmp,auxVec];
				If[	check=!={},
					tmp = check
				];

				(* {linIndepVecs, linDepVecs, decompositions} *)
				res = tmp[[1]],
				res = tmp
			]
		];



		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "FCLoopFindTensorBasis: Leaving.", FCDoControl -> fclftbVerbose];
		FCPrint[3, "FCLoopFindTensorBasis: Leaving with: ", res, FCDoControl -> fclftbVerbose];

		res
	];


findNewBasis[firstBasisVector_, extmoms_List, linIndepSets_List, linDepSets_List,  dim_, kinRules_List, head_, noZeroVecs_, auxVec_, fclftbVerbose_] :=
		Block[{time, linIndepVecs, linDepVecs, tmp, decompositions, eliminatedVecs },

			FCPrint[4, "FCLoopFindTensorBasis: findNewBasis: Entering with ", firstBasisVector, " as first basis vector.",FCDoControl -> fclftbVerbose];

			time=AbsoluteTime[];
			FCPrint[4, "FCLoopFindTensorBasis: findNewBasis: Applying findBasisVectors.", FCDoControl -> fclftbVerbose];

			(* By definition linIndepSets start with sets that contain firstBasisVector *)
			{linIndepVecs, linDepVecs} = (List@@findBasisVectors[SortBy[linIndepSets, FreeQ[#, firstBasisVector] &],linDepSets,{firstBasisVector},{}, fclftbVerbose])[[3;;4]];


			If[	Sort[Complement[extmoms,linIndepVecs]]=!=linDepVecs,
				linDepVecs = Complement[extmoms,linIndepVecs]
			];


			If[	linIndepVecs==={},
				linIndepVecs = {First[linDepVecs]};
				linDepVecs = Rest[linDepVecs];
			];

			FCPrint[4, "FCLoopFindTensorBasis: findNewBasis: Done applying findBasisVectors, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl -> fclftbVerbose];

			FCPrint[4, "FCLoopFindTensorBasis: findNewBasis: Linearly dependent vectors: ", linDepVecs,  FCDoControl -> fclftbVerbose];
			FCPrint[4, "FCLoopFindTensorBasis: findNewBasis: Linearly independent vectors: ", linIndepVecs,  FCDoControl -> fclftbVerbose];


			time=AbsoluteTime[];
			FCPrint[4, "FCLoopFindTensorBasis: findNewBasis: Eliminating linearly dependent momenta.", FCDoControl -> fclftbVerbose];
			(* The ordering of linDepSets can decide whether zero vectors can be identified or not! *)
			tmp = eliminateLinearlyDependentMomenta[linDepSets, linIndepVecs, linDepVecs, {}, {}, dim, kinRules, head, noZeroVecs, auxVec, fclftbVerbose];
			FCPrint[4, "FCLoopFindTensorBasis: findNewBasis: Done eliminating linearly dependent momenta, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl -> fclftbVerbose];


			{eliminatedVecs, decompositions} = (List@@tmp)[[4;;5]];
			FCPrint[4, "FCLoopFindTensorBasis: findNewBasis: Obtained decompositions: ", decompositions,  FCDoControl -> fclftbVerbose];

			FCPrint[4, "FCLoopFindTensorBasis: findNewBasis: Eliminated vectors: ", eliminatedVecs,  FCDoControl -> fclftbVerbose];

			If[	Sort[linDepVecs]=!=Sort[eliminatedVecs],
				Message[FCLoopFindTensorBasis::failmsg, "Failed to eliminate all linearly dependent vectors."];
				Abort[]
			];

			(* If the auxiliary vector is needed to remove linearly dependent vectors, we need to add it to our basis *)
			If[	!FreeQ[decompositions,auxVec],
				linIndepVecs = Join[linIndepVecs,{auxVec}]
			];

			{Sort[linIndepVecs], Sort[linDepVecs], Sort[decompositions]}
	];



(*
	We work through each set of linearly dependent vectors by picking one of such vectors
	and expressing it through the other ones.
*)
eliminateLinearlyDependentMomenta[linDepSets_List, linIndepVecs_List, linDepVecs_List, eliminatedVecs_List, decompositions_List, dim_, kinRules_List, head_, noZeroVecs_, auxVec_, fclftbVerbose_] :=
	Block[{	tmp, check, eq, CC, linDepVecInThisSet, res, eqRaw, vars, zeroVecs},

		FCPrint[4, "FCLoopFindTensorBasis: eliminateLinearlyDependentMomenta: Entering with: ", {linDepSets,linIndepVecs,linDepVecs,eliminatedVecs,decompositions}, FCDoControl -> fclftbVerbose];

		(* Consider the first set of linearly dependent vectors *)
		tmp = linDepSets[[1]];

		(* Extract new linearly dependent vectors *)
		linDepVecInThisSet = Complement[tmp, linIndepVecs, eliminatedVecs];

		(* Check, if the relation involves only already known linearly dependent vectors*)
		check = Complement[tmp, linDepVecs];

		If[(check === {}) || Length[linDepVecInThisSet] =!= 1,
			(*The relation doesn't contain any new linearly dependent vectors or their number is more than one*)
			FCPrint[4, "FCLoopFindTensorBasis: eliminateLinearlyDependentMomenta: There are no new linearly dependent vectors to be extracted from this relation.", FCDoControl -> fclftbVerbose];
			Return[eliminateLinearlyDependentMomenta[linDepSets[[2 ;;]], linIndepVecs, linDepVecs, eliminatedVecs, decompositions, dim, kinRules, head,  noZeroVecs, auxVec, fclftbVerbose]]
		];

		(* Consider the first new linearly dependent vector *)
		linDepVecInThisSet = First[linDepVecInThisSet];
		FCPrint[4, "FCLoopFindTensorBasis: eliminateLinearlyDependentMomenta: New linearly dependent vector: ", linDepVecInThisSet, FCDoControl -> fclftbVerbose];

		(* Generate a vector equation for our new vector *)
		{eqRaw, vars} = generateEquation[tmp, linDepVecInThisSet, CC];
		FCPrint[4, "FCLoopFindTensorBasis: eliminateLinearlyDependentMomenta: Equation for this vector: ", {eqRaw, vars}, FCDoControl -> fclftbVerbose];

		zeroVecs = Select[decompositions,MatchQ[#,Rule[_,0]]&];
		If[	zeroVecs=!={},
			eqRaw = eqRaw /. zeroVecs;
			vars = Cases2[eqRaw,CC];
			FCPrint[4, "FCLoopFindTensorBasis: eliminateLinearlyDependentMomenta: After eliminating zero vectors: ", {eqRaw, vars}, FCDoControl -> fclftbVerbose];
		];



		res = solveEqSys[{eqRaw, vars}, linDepVecInThisSet, linIndepVecs, linDepVecs, dim, kinRules, head, noZeroVecs, auxVec, fclftbVerbose];
		FCPrint[4, "FCLoopFindTensorBasis: eliminateLinearlyDependentMomenta: Solution: ", res, FCDoControl -> fclftbVerbose];

		eliminateLinearlyDependentMomenta[linDepSets[[2 ;;]], linIndepVecs, linDepVecs,	Join[eliminatedVecs, {linDepVecInThisSet}], Join[decompositions, {res}], dim, kinRules, head,  noZeroVecs, auxVec, fclftbVerbose]

	] /; Length[linDepSets] > 0;


generateEquation[linDepSet_List, linDepVec_, propConst_] :=
	Block[{lhs, rhs, vars},
		rhs = SelectFree[linDepSet, linDepVec];
		vars = Table[propConst[i], {i, 1, Length[rhs]}];
		rhs = Total[MapThread[#1 #2 &, {rhs, vars}]];
		{linDepVec == rhs, vars}
	];





solveEqSys[{eqRaw_Equal, vars_List}, linDepVec_, linIndepMoms_List, linDepVecs_List, dim_, kinRules_List, head_, noZeroVecs_, auxVec_, fclftbVerbose_] :=
	Block[{	len, trialVecs, candidates, dummyInd, allMoms,
			pairs, pairRule, eq, eqSys, sols, finalSol, trialsols},


		FCPrint[4, "FCLoopFindTensorBasis: solveEqSys: Entering with: ", {{eqRaw, vars}, linDepVec, linIndepMoms}, FCDoControl -> fclftbVerbose];

		len = Length[vars];
		allMoms = Join[linIndepMoms, linDepVecs, {linDepVec}, {auxVec}];


		pairs =	Pair[Momentum[#, dim], LorentzIndex[dummyInd, dim]] & /@ allMoms;
		pairRule = Thread[Rule[allMoms, pairs]];

		{eq, candidates} = {eqRaw, Subsets[Join[linIndepMoms,linDepVecs,{auxVec}], {len}]} /. Dispatch[pairRule];
		candidates = Reverse[SortBy[candidates, FreeQ[#, auxVec] &]];

		FCPrint[4, "FCLoopFindTensorBasis: solveEqSys: Possible sets of external momenta for contractions: ", candidates, FCDoControl -> fclftbVerbose];

		(*
			Generate different systems of equations that involve contractions with various external
			momenta, including the auxiliary one.
		*)

		eqSys =	Map[Function[x, Map[eq # &, x]], candidates] /.	Pair -> PairContract /. Dispatch[kinRules];
		FCPrint[4, "FCLoopFindTensorBasis: solveEqSys: Equation systems to solve: ", eqSys, FCDoControl -> fclftbVerbose];

		Quiet[sols = Solve[#, vars] & /@ eqSys;];
		(* Select first nontrivial solution in each set of solutions *)
		sols = (sols //. {} -> Unevaluated[Sequence[]]);

		FCPrint[4, "FCLoopFindTensorBasis: solveEqSys: Raw solutions: ", sols, FCDoControl -> fclftbVerbose];

		sols = Select[First/@sols, (Length[#] ===Length[vars]) &];

		If[	noZeroVecs,
			trialsols = Select[sols, !(Length[#]===Length[vars] && MatchQ[#,{Rule[_,0]..}])&];
			If[	trialsols=!={},
				sols = trialsols
			]
		];

		FCPrint[4, "FCLoopFindTensorBasis: solveEqSys: Found solutions: ", sols, FCDoControl -> fclftbVerbose];


		If[	Length[sols] === 0,
			Message[FCLoopFindTensorBasis::failmsg, "Failed to resolve a linear dependence between external momenta."];
			Abort[]
		];

		finalSol = sols[[1]] /. Rule[a_, b_] :> Rule[a, head[b]]/. head[0] -> 0;
		FCPrint[4, "FCLoopFindTensorBasis: solveEqSys: Final solution: ", finalSol, FCDoControl -> fclftbVerbose];
		eqRaw /. Dispatch[finalSol] /. Equal -> Rule
	];

(*
	Given a sets of linearly independent and dependent vectors, this function constructs a set
	of linearly independent momenta (that form a basis) and a set of linearly dependent momenta
	(that need to be expressed in terms of the new basis vectors). E.g.

	findBasisVectors[{}, {{k1, k2}}, {}, {}]
	findBasisVectors[{{k1, k2}}, {{k1, k3}, {k2, k3}}, {}, {}]
	findBasisVectors[{{k1, k2}, {k3, k4}}, {{k1, k3}, {k2, k4}}, {}, {}]

*)
findBasisVectors[linIndepSets_List, linDepSets_List, linIndepMoms_List, linDepMoms_List, fclftbVerbose_] :=
	Block[{	tmp, aux, newLinIndep, newLinDep, twoSets},

		FCPrint[4,"FCLoopFindTensorBasis: findBasisVectors: Entering with  ", {linIndepSets,linDepSets,linIndepMoms,linDepMoms}, FCDoControl -> fclftbVerbose];

		(* By definition linIndepSets start with sets that contain firstBasisVector *)

		(* Get all momenta from this set that are not in our lists of identified momenta *)
		tmp = Complement[linIndepSets[[1]], linIndepMoms,linDepMoms];

		FCPrint[4,"FCLoopFindTensorBasis: findBasisVectors: Vectors to check: ", tmp, FCDoControl -> fclftbVerbose];

		(* If there are no new momenta to be found, we discard this set and continue *)
		If[	tmp === {},
			FCPrint[4,"FCLoopFindTensorBasis: findBasisVectors: No new vectors found.", FCDoControl -> fclftbVerbose];
			Return[findBasisVectors[linIndepSets[[2 ;;]], linDepSets, linIndepMoms,	linDepMoms, fclftbVerbose]]
		];

		(* If there are new momenta, we must check that they are independent from the momenta in our linIndepMoms list  *)

		(* Get all sets of linearly dependent momenta that contain both new candidates and momenta from our linIndepMoms list  *)
		aux = SelectNotFree[SelectNotFree[linDepSets, tmp], linIndepMoms];
		FCPrint[4,"FCLoopFindTensorBasis: findBasisVectors: Relations involving candidate vectors: ", aux, FCDoControl -> fclftbVerbose];

		(*
			Every 2-set in this list signals that one of our candidate is linearly dependent on one of our momenta from
			the linIndepMoms list. Such candidates must be rejected. We put them into the newLinDep list.
		*)

		twoSets = Select[aux, (Length[#] == 2) &];
		newLinDep =	Map[If[! FreeQ[twoSets, #], #, Unevaluated[Sequence[]]] &, tmp];

		FCPrint[4,"FCLoopFindTensorBasis: findBasisVectors: New linearly dependent vectors: ", newLinDep, FCDoControl -> fclftbVerbose];

		tmp = Complement[tmp, newLinDep];
		FCPrint[4,"FCLoopFindTensorBasis: findBasisVectors: Candidate vectors after sorting out linearly dependent ones: ", tmp, FCDoControl -> fclftbVerbose];

		(*
			Remove all longer sets that contain a dependent momentum. Such sets do not automatically
			signal that a candidate is linearly	dependent on other momenta
		*)
		aux = SelectFree[Complement[aux, twoSets], linDepMoms];

		FCPrint[4,"FCLoopFindTensorBasis: findBasisVectors: Remaining relations to analyze: ", aux, FCDoControl -> fclftbVerbose];
		(* Acceptable candidates are those that are not contained in these sets *)

		newLinIndep = Map[If[FreeQ[aux, #], #, Unevaluated[Sequence[]]] &, tmp];
		newLinDep = Join[Complement[tmp, newLinIndep], newLinDep];

		FCPrint[4,"FCLoopFindTensorBasis: findBasisVectors: New linearly independent vectors: ", newLinIndep, FCDoControl -> fclftbVerbose];
		FCPrint[4,"FCLoopFindTensorBasis: findBasisVectors: New linearly dependent vectors: ", newLinDep, FCDoControl -> fclftbVerbose];


		findBasisVectors[linIndepSets[[2 ;;]], linDepSets, Join[linIndepMoms, newLinIndep], Join[linDepMoms, newLinDep], fclftbVerbose]
	] /; Length[linIndepSets] > 0;


(*
	There are no sets of linearly independent momenta left and we don't have a single independent momentum.
	This can happen only at a start, so let's take one our of linearly dependent vectors as a basis vector.
	Notice that for this it's preferable to choose a vector that is not light-like
*)
findBasisVectors[{}, linDepSets_List /; Length[linDepSets]===1, {}, {}, fclftbVerbose_] :=
	findBasisVectors[{}, {}, {First[linDepSets[[1]]]}, Rest[linDepSets[[1]]], fclftbVerbose];




FCPrint[1,"FCLoopFindTensorBasis.m loaded."];
End[]
