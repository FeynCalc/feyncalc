(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DoPolarizationSums						*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary:  Compute polarization sums of vector bosons *)

(* ------------------------------------------------------------------------ *)

DoPolarizationSums::usage =
"DoPolarizationSums[exp, k, ...] acts on an expression exp that must contain a
polarization vector $\\varepsilon(k)$  and its complex conjugate (e.g. exp can
be a matrix element squared).

Depending on the arguments of the function, it will perform a sum over the
polarization of $\\varepsilon(k)$ and its c.c.

- DoPolarizationSums[exp, k] sums over the three physical polarizations of an
external massive vector boson with the $4$-momentum k and the mass $k^2$.
- DoPolarizationSums[exp, k, 0] replaces the polarization sum of an external
massless vector boson with the momentum k by $-g^{\\mu \\nu}$. This corresponds
to the summation over all 4 polarizations, including the unphysical ones.
- DoPolarizationSums[exp, k, n] sums over physical (transverse) polarizations
of an external massless vector boson with the momentum k, where n is an
auxiliary 4-vector from the gauge-dependent polarization sum formula.

Cf. PolarizationSum for more examples and explanations on different
polarizations.

DoPolarizationSums also work with $D$-dimensional amplitudes.";

NumberOfPolarizations::usage =
"NumberOfPolarizations is an option for DoPolarizationSums. It specifies the
number of polarizations to sum over in the expression.
This is relevant only for expressions that contain terms free of polarization
vectors. This may occur e.g. if the scalar products involving
polarization vectors have already been assigned some particular values. In
this case the corresponding terms will be multiplied by the
corresponding number of polarizations.

The default value is Automatic which means that the function will attempt to
recognize the correct value automatically by
extracting the dimension dim of the polarization vectors and putting (dim-2)
for massless and (dim-1) for massive vector bosons.
Notice that if the input expression is free of polarization vectors, the
setting Automatic will fail, and the user must specify the correct
dimension by hand.";

DoPolarizationSums::failmsg =
"Error! DoPolarizationSums has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

DoPolarizationSums::noauto =
"The option NumberOfPolarizations is set to Automatic but since the input expression \
contains no polarization vectors, the function cannot determine the number of polarizations \
to sum over in the expression automatically. Please set NumberOfPolarizations to the \
appropriate value e.g. 2 or D-2 etc. by hand."

DoPolarizationSums::noauto =
"The option NumberOfPolarizations is set to Automatic but since the input expression \
contains no polarization vectors, the function cannot determine the number of polarizations \
to sum over in the expression automatically. Please set NumberOfPolarizations to the \
appropriate value e.g. 2 or D-2 etc. by hand."

DoPolarizationSums::mutidim =
"The input expression contains pairs of polarization vectors in different \
dimensions. This introduces an ambiguity regarding the correct dimension of \
the polarization sum to be employed for the summation. Please make sure that the \
input expression is unambiguous in this respect."

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
polarizationVectorCorrectQ;
End[]

Begin["`DoPolarizationSums`Private`"]

dpsVerbose::usage="";

Options[DoPolarizationSums] = {
	Contract				-> True,
	ExtraFactor 			-> 1,
	FCE						-> False,
	FCI						-> False,
	FCVerbose				-> False,
	Head					-> Identity,
	NumberOfPolarizations 	-> Automatic,
	VirtualBoson			-> False
};

DoPolarizationSums[expr_, bosonMomentum_, opts:OptionsPattern[]]:=
	DoPolarizationSums[expr, bosonMomentum, -1, opts];

DoPolarizationSums[expr_, bosonMomentum_, auxMomentum_, OptionsPattern[]] :=
	Block[ {polInd1, polInd2, res, ex, tmp, dim, polVectorsList, freePart,
			polPart, optNumberOfPolarizations, nPolarizations,
			optVirtualBoson, dummyInd1, dummyInd2},

		If [OptionValue[FCVerbose]===False,
			dpsVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				dpsVerbose=OptionValue[FCVerbose]
			];
		];

		optNumberOfPolarizations	= OptionValue[NumberOfPolarizations];
		optVirtualBoson 			= OptionValue[VirtualBoson];

		FCPrint[1,"DoPolarizationSums: Entering.", FCDoControl->dpsVerbose];
		FCPrint[3,"DoPolarizationSums: Entering with: ", expr, FCDoControl->dpsVerbose];

		FCPrint[1,"DoPolarizationSums: Vector boson momentum: ", bosonMomentum, FCDoControl->dpsVerbose];
		FCPrint[1,"DoPolarizationSums: Auxiliary momentum: ", auxMomentum, FCDoControl->dpsVerbose];

		If[	Internal`SyntacticNegativeQ[bosonMomentum] || NumericQ[bosonMomentum]
			|| MemberQ[{Times,Plus}, Head[bosonMomentum]],
			Message[DoPolarizationSums::failmsg, "Illegal variable denoting the vector boson momentum."];
			Abort[]
		];

		If[	!MemberQ[{0,-1},auxMomentum],
			If[	Internal`SyntacticNegativeQ[auxMomentum] || NumericQ[auxMomentum]
				|| MemberQ[{Times,Plus}, Head[auxMomentum]],
				Message[DoPolarizationSums::failmsg, "Illegal variable denoting the auxiliary momentum."];
				Abort[]
			];
		];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	ex===0,
			Return[0]
		];

		polInd1 = $MU[Unique[]];
		polInd2 = $MU[Unique[]];

		If[	FeynCalc`Package`polarizationVectorCorrectQ[ex]=!=True,
			Message[DoPolarizationSums::failmsg, "Illegal variable denoting the vector boson momentum."];
			Abort[]
		];


		ex = ex /.


			{
			CartesianPair[CartesianMomentum[Polarization[bosonMomentum,rest1__],di_:3],CartesianMomentum[Polarization[bosonMomentum,rest2__],di_:3]] :>
				(dummyInd1=Unique["polInd"]; dummyInd2=Unique["polInd"];
				Pair[Momentum[Polarization[bosonMomentum,rest1],di+1],LorentzIndex[dummyInd1,di+1]]*
				Pair[Momentum[Polarization[bosonMomentum,rest2],di+1],LorentzIndex[dummyInd2,di+1]]*
				Pair[LorentzIndex[dummyInd1,di+1],LorentzIndex[dummyInd2,di+1]])
			} /. {
				TemporalPair[ExplicitLorentzIndex[0],TemporalMomentum[Polarization[bosonMomentum,rest__]]] :>
				(dummyInd1=Unique["polInd"]; Pair[Momentum[Polarization[bosonMomentum,rest]],LorentzIndex[dummyInd1]]*
					Pair[ExplicitLorentzIndex[0],LorentzIndex[dummyInd1]]),

				CartesianPair[slot_,CartesianMomentum[Polarization[bosonMomentum,rest__],di_:3]] :>
				(dummyInd1=Unique["polInd"]; Pair[Momentum[Polarization[bosonMomentum,rest],di+1],LorentzIndex[dummyInd1,di+1]]*
					Pair[slot,LorentzIndex[dummyInd1,di+1]])
		};

		FCPrint[1,"DoPolarizationSums: Intermediate expression: ", ex, FCDoControl->dpsVerbose];

		polVectorsList = SelectNotFree[SelectNotFree[Sort[DeleteDuplicates[Cases[ex ,_Momentum | _CartesianMomentum | _TemporalMomentum,
			Infinity]]],Polarization],bosonMomentum];

		FCPrint[1,"DoPolarizationSums: Polarization vectors present in the expression: ", polVectorsList, FCDoControl->dpsVerbose];

		If[	polVectorsList==={},

			(* No polarization vectors in the expression *)
			freePart = ex;
			polPart = 0;
			If[	optNumberOfPolarizations===Automatic,
				Message[DoPolarizationSums::noauto];
				Abort[]
			],

			(* Polarization vectors present *)

			FCPrint[1,"DoPolarizationSums: Polarization vectors in the expression: ", polVectorsList, FCDoControl->dpsVerbose];

			If[	!MatchQ[polVectorsList, {
					(Momentum)[Polarization[bosonMomentum,Complex[0,1], ___Rule],di___],
					(Momentum)[Polarization[bosonMomentum,Complex[0,-1], ___Rule],di___]} | {
					(Momentum)[Polarization[bosonMomentum,Complex[0,-1], ___Rule],di___],
					(Momentum)[Polarization[bosonMomentum,Complex[0,1], ___Rule],di___]}],
				Message[DoPolarizationSums::failmsg,"Polarization vectors do not seem to appear in a proper way in the expression."];
				Abort[]
			];

			dim = FCGetDimensions[polVectorsList,ChangeDimension->True];

			FCPrint[1,"DoPolarizationSums: Spacetime dimension of polarization vectors: ", dim, FCDoControl->dpsVerbose];

			If[	Length[dim]=!=1,
				Message[DoPolarizationSums::mutidim];
				Abort[],
				dim = First[dim]
			];



			tmp = ex /.{
				Momentum[Polarization[bosonMomentum,Complex[0,1], ___Rule],dim] :>
					LorentzIndex[polInd1,dim],
				Momentum[Polarization[bosonMomentum,Complex[0,-1], ___Rule],dim] :>
					LorentzIndex[polInd2,dim]
			};

			FCPrint[3,"DoPolarizationSums: Intermediate expression: ", tmp, FCDoControl->dpsVerbose];

			{freePart, polPart} = FCSplit[tmp,{polInd1,polInd2}]
		];


		Which[
			(*massive vector boson*)
			bosonMomentum=!=0 && auxMomentum===-1,
				FCPrint[1,"DoPolarizationSums: Inserting polarization sum for a massive vector boson.", FCDoControl->dpsVerbose];
				If[ polPart=!=0,
					polPart = OptionValue[Head][PolarizationSum[polInd1,polInd2,bosonMomentum, Dimension->dim, Heads->{LorentzIndex,LorentzIndex}]] polPart
				];
				If[	optNumberOfPolarizations=!=Automatic,
					nPolarizations = optNumberOfPolarizations,
					nPolarizations = dim-1
				],

			(*massless vector boson with the gauge trick*)
			bosonMomentum=!=0 && auxMomentum===0,
				FCPrint[1,"DoPolarizationSums: Inserting polarization sum for a massless vector boson with 4 polarizations (2 of which are unphysical).", FCDoControl->dpsVerbose];
				If[	polPart=!=0,
					polPart = OptionValue[Head][PolarizationSum[polInd1,polInd2,bosonMomentum, auxMomentum, Dimension->dim, VirtualBoson-> optVirtualBoson, Heads->{LorentzIndex,LorentzIndex}]] polPart;
				];
				If[	optNumberOfPolarizations=!=Automatic,
					nPolarizations = optNumberOfPolarizations,
					(*A virtual vector boson has all 4 polarization. A real on-shell boson may have only 2.*)
					If[	TrueQ[optVirtualBoson],
						nPolarizations = dim,
						nPolarizations = dim-2
					]
				],

			(*true massless vector boson*)
			bosonMomentum=!=0 && !MemberQ[{0,-1}, auxMomentum],
				FCPrint[1,"DoPolarizationSums: Inserting polarization sum for a massless vector boson with 2 physical polarizations (axial gauge).", FCDoControl->dpsVerbose];
				If[	polPart=!=0,
					polPart = OptionValue[Head][PolarizationSum[polInd1,polInd2,bosonMomentum, auxMomentum, Dimension->dim, VirtualBoson-> optVirtualBoson, Heads->{LorentzIndex,LorentzIndex}]] polPart;
				];
				If[	optNumberOfPolarizations=!=Automatic,
					nPolarizations = optNumberOfPolarizations,
					nPolarizations = dim-2
				],
			True,
				Message[DoPolarizationSums::failmsg,"Unknown polarization sum"];
				Abort[]
		];

		If[	freePart=!=0,
			FCPrint[0,"DoPolarizationSums: The input expression contains terms free of polarization vectors. " <>
				"Those will be multiplied with the number of polarizations given by ", nPolarizations, ".", FCDoControl->dpsVerbose];
			freePart = nPolarizations freePart
		];


		If[ OptionValue[Contract],
			polPart = Contract[polPart,FCI->True]
		];

		res = OptionValue[ExtraFactor] freePart + OptionValue[ExtraFactor] polPart;

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res

	];


polarizationVectorCorrectQ[ex_]:=
	Block[{polarizations, correct, complex, momenta, check, null1, null2},
		correct=True;
		polarizations = Cases2[ex+null1+null2,Polarization];
		polarizations = MomentumExpand[polarizations]/.Complex->complex;

		If[	polarizations=!={},
			momenta = First/@polarizations;
			correct = FreeQ2[Head /@ momenta, {Times, Plus}] && MatchQ[Internal`SyntacticNegativeQ /@ momenta, {False ..}] &&
			MatchQ[NumericQ /@ momenta, {False ..}];
		];
		correct
	];

FCPrint[1,"DoPolarizationSums.m loaded."];
End[]
