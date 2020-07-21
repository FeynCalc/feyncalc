(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCSymanzikPolynomials											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:  	Calculates Symanzik polynomials U and V for the given loop
				integral													*)

(* ------------------------------------------------------------------------ *)

FCSymanzikPolynomials::usage =
"FCSymanzikPolynomials[int,{q1,q2,...}] returns {U,F,P,M}, \
where U and F are the Symanzik polynomials, with U = det M, while \
P shows the powers of the occurring propagators. Notice that int \
can be a single loop integral or a list of propagators. \n
The algorithm for deriving the UF-parametrization of a loop integral \
was adopted from the UF generator available in multiple codes of \
Alexander Smirnov, such as FIESTA (arXiv:1511.03614) and FIRE \
(arXiv:1901.07808). The code UF.m was also mentioned in the book \
\"Analytic Tools for Feynman Integrals\" by Vladimir Smirnov, Chapter 2.3";

FCSymanzikPolynomials::failmsg =
"Error! FCSymanzikPolynomials has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCSymanzikPolynomials`Private`"]

dim::usage="";
fcszVerbose::usage="";
null1::usage="";
null2::usage="";
isCartesian::usage="";

Options[FCSymanzikPolynomials] = {
	FCE						-> False,
	FCI						-> False,
	FCVerbose				-> False,
	Factoring 				-> Factor2,
	FinalSubstitutions		-> {},
	Indexed					-> True,
	Names					-> FCGV["x"],
	Reduce					-> False,
	"FeynmanParameterJoin"	-> False
};


FCSymanzikPolynomials[expr_, lmoms_List /; ! OptionQ[lmoms], OptionsPattern[]] :=
	Block[{	feynX, propProduct, tmp, symF, symU, ex, spd, mtmp,
			matrix, nDenoms, res, constraint, tmp0, powers, optFactoring,
			optFinalSubstitutions, optNames, aux1, aux2, nProps},

		optNames				= OptionValue[Names];
		optFactoring 			= OptionValue[Factoring];
		optFinalSubstitutions	= OptionValue[FinalSubstitutions];

		If [OptionValue[FCVerbose]===False,
			fcszVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fcszVerbose=OptionValue[FCVerbose]
			];
		];

		If[OptionValue[FCI],
			ex = expr,
			{ex,optFinalSubstitutions} = FCI[{expr,optFinalSubstitutions}]
		];

		If [!FreeQ2[$ScalarProducts, {lmoms}],
			Message[FCSymanzikPolynomials::failmsg, "Some of the loop momenta have scalar product rules attached to them."];
			Abort[]
		];

		If[	!MatchQ[ex,{__}|_. _FeynAmpDenominator],
			Message[FCSymanzikPolynomials::failmsg, "The input expression is not a proper integral or list of propagators"];
			Abort[]

		];

		FCPrint[1,"FCSymanzikPolynomials: Entering. ", FCDoControl->fcszVerbose];
		FCPrint[3,"FCSymanzikPolynomials: Entering  with: ", ex, FCDoControl->fcszVerbose];

		Which[
			!FreeQ[ex,Momentum] && FreeQ[ex,CartesianMomentum],
			isCartesian=False,
			(*Lorentzian integral *)
			FreeQ[ex,Momentum] && !FreeQ[ex,CartesianMomentum],
			isCartesian=True,
			(*Cartesian integral *)
			!FreeQ[ex,Momentum] && !FreeQ[ex,CartesianMomentum],
			(*Mixed integral*)
			Message[FCSymanzikPolynomials::failmsg,"Integrals that simultaneously depend on Lorentz and Cartesian vectors are not supported."];
			Abort[]
		];

		dim = FCGetDimensions[ex];

		If[	Length[dim]=!=1,
			Message[FCSymanzikPolynomials::failmsg,"The loop integrals contains momenta in different dimensions."];
			Abort[]
		];
		dim = First[dim];

		If[	Union[FreeQ[ex,#]&/@lmoms]=!={False},
			Message[FCSymanzikPolynomials::failmsg,"Some of the specified loop momenta are not contained in the input expression."];
			Abort[]
		];


		tmp = FCLoopBasisExtract[ex, lmoms, SetDimensions->{dim}];


		FCPrint[3,"FCSymanzikPolynomials: List of denominators: ", tmp, FCDoControl->fcszVerbose];


		nDenoms = Length[tmp[[1]]];
		feynX 	= Table[optNames[i],{i,1,nDenoms}];

		If[	!OptionValue[Indexed],
			feynX = feynX /. s_Symbol[i_Integer] :> ToExpression[ToString[s]<>ToString[i]]
		];

		powers 	= Table[{feynX[[i]],tmp[[4]][[i]],tmp[[3]][[i]]},{i,1,nDenoms}];
		tmp = Sum[feynX[[i]] tmp[[1]][[i]],{i,1,nDenoms}];


		FCPrint[3,"FCSymanzikPolynomials: Powers of denominators: ", powers, FCDoControl->fcszVerbose];

		FCPrint[3,"FCSymanzikPolynomials: After introducing the Feynman paramters: ", tmp, FCDoControl->fcszVerbose];


		If[	OptionValue["FeynmanParameterJoin"]===True,
			aux1 = (Times @@ Map[Power[#[[1]],(#[[3]]-1)] &, powers]);
			aux2 = Last[Transpose[powers]];
			aux1 = aux1*Gamma[Total[aux2]]/(Times@@(Gamma/@aux2));
			res = {FeynAmpDenominator[GenericPropagatorDenominator[tmp, {Total[aux2], 1}]], aux1, feynX};
			Return[res]
		];

		If[ !isCartesian,

			tmp0 = tmp //. {
				Pair[Momentum[a_, dim], Momentum[b_, dim]] /; MemberQ[lmoms, a] && MemberQ[lmoms, b] :> spd[a, b]
			},

			tmp0 = tmp //. {
				CartesianPair[CartesianMomentum[a_, dim], CartesianMomentum[b_, dim]] /; MemberQ[lmoms, a] && MemberQ[lmoms, b] :> spd[a, b]
			}
		];

		(* symmetrization, otherwise the M-matrix will not come out right! *)
		tmp0 = tmp0 /. spd[a_,b_]:> 1/2 (spd[a,b] + spd[b,a]);

		FCPrint[3,"FCSymanzikPolynomials: tmp0: ", tmp0, FCDoControl->fcszVerbose];

		mtmp = SelectNotFree2[tmp0 + null1 + null2, spd];

		FCPrint[3,"FCSymanzikPolynomials: mtmp: ", mtmp, FCDoControl->fcszVerbose];

		matrix = (Outer[spd, lmoms, lmoms] /. a_spd :> Coefficient[mtmp, a]);
		FCPrint[3,"FCSymanzikPolynomials: M: ", matrix, FCDoControl->fcszVerbose];


		{symU, symF} = Fold[buildF, {1, tmp}, lmoms];

		FCPrint[3,"FCSymanzikPolynomials: Raw U: ", symU, FCDoControl->fcszVerbose];
		FCPrint[3,"FCSymanzikPolynomials: Raw F: ", symF, FCDoControl->fcszVerbose];

		If[	Simplify[Det[matrix]-symU]=!=0 || !SymmetricMatrixQ[matrix],
			Message[FCSymanzikPolynomials::failmsg,"Something went wrong when calculating the matrix M!"];

		];


		If[ !isCartesian,
			(*The extra minus sign in symF comes from pulling out (-1)^N in the Minkowski case *)
			res = {symU, -Together[symU symF], powers, matrix},
			res = {symU, Together[symU symF], powers, matrix};
		];

		FCPrint[3,"FCSymanzikPolynomials: Preliminary result: ", res, FCDoControl->fcszVerbose];

		If[	optFinalSubstitutions=!={},
			res = res /. optFinalSubstitutions
		];


		If[	optFactoring=!=False,
			res = optFactoring[res];
			FCPrint[3,"FCSymanzikPolynomials: After applying Simplify to the result: ", res, FCDoControl->fcszVerbose];
		];

		If[	nDenoms>1 && OptionValue[Reduce],

			constraint = (Sum[feynX[[i]],{i,1,nDenoms}]==1);

			FCPrint[3,"FCSymanzikPolynomials: Constraint on the values of the Feynman parameters: ", constraint, FCDoControl->fcszVerbose];

			res = Simplify[res,constraint]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1,"FCSymanzikPolynomials: Leaving.", FCDoControl->fcszVerbose];
		FCPrint[3,"FCSymanzikPolynomials: Leaving with: ", res, FCDoControl->fcszVerbose];

		res
];

buildF[{oldSymU_, oldSymF_}, lmom_] :=
	Block[{tmp, lmsq, lambda, J, num, res},

		tmp = ExpandScalarProduct[oldSymF, Momentum -> lmom, FCI -> True];


		If[ !isCartesian,
			(*The extra minus sign in symF comes from pulling out (-1)^N *)
			tmp = tmp /. Pair[Momentum[lmom, dim], Momentum[lmom, dim]] :> lmsq,
			tmp = tmp /. CartesianPair[CartesianMomentum[lmom, dim], CartesianMomentum[lmom, dim]] :> lmsq
		];



		FCPrint[3,"FCSymanzikPolynomials: buildF: tmp: ", tmp, FCDoControl->fcszVerbose];
		FCPrint[3,"FCSymanzikPolynomials: buildF: Current loop momentum: ", lmom, FCDoControl->fcszVerbose];

		lambda = Coefficient[tmp, lmsq];

		If[	lambda===0,
			Message[FCSymanzikPolynomials::failmsg,"The coefficient of one of the loop momenta squared is zero."];
			Abort[]
		];

		tmp = Expand2[tmp, {Pair, CartesianPair, lmom}];

		If[ !isCartesian,
		tmp = tmp //.
			{a_. Pair[Momentum[lmom, dim], x_] + b_. Pair[Momentum[lmom, dim], y_] /; FreeQ[{x, y}, lmom] :>
				Pair[Momentum[lmom, dim], a x + b y]} /.
			{Pair[Momentum[lmom, dim], Momentum[lmom, dim]] :> lmsq},

		tmp = tmp //.
			{a_. CartesianPair[CartesianMomentum[lmom, dim], x_] + b_. CartesianPair[CartesianMomentum[lmom, dim], y_] /; FreeQ[{x, y}, lmom] :>
				CartesianPair[CartesianMomentum[lmom, dim], a x + b y]} /.
			{CartesianPair[CartesianMomentum[lmom, dim], CartesianMomentum[lmom, dim]] :> lmsq}

		];

		FCPrint[3,"FCSymanzikPolynomials: buildF: tmp after combinging the scalar products: ", tmp, FCDoControl->fcszVerbose];

		num = (SelectNotFree2[tmp + null1 + null2, lmom])^2 /. null1 | null2 -> 0;

		If[ !isCartesian,
			num = num /. Pair[Momentum[lmom, ___], y_]^2 /; FreeQ[y, lmom] :> ExpandScalarProduct[Pair[y, y], FCI -> True],
			num = num /. CartesianPair[CartesianMomentum[lmom, ___], y_]^2 /; FreeQ[y, lmom] :> ExpandScalarProduct[CartesianPair[y, y], FCI -> True]
		];


		J = SelectFree2[tmp + null1 + null2, {lmom, lmsq}] /. null1 | null2 -> 0;

		FCPrint[3,"FCSymanzikPolynomials: buildF: J: ", J, FCDoControl->fcszVerbose];

		res = J - num/(4 lambda);

		FCPrint[3,"FCSymanzikPolynomials: buildF: res: ", res, FCDoControl->fcszVerbose];

		If[	!FreeQ2[res,{lmsq,lmom}],
			Message[FCSymanzikPolynomials::failmsg,"buildF failed to eliminate one of the loop momenta."];
			Abort[]
		];

		{oldSymU lambda, J - num/(4 lambda)}
];


FCPrint[1,"FCSymanzikPolynomials.m loaded."];
End[]
