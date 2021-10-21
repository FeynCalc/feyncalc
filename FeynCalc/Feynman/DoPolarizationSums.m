(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DoPolarizationSums						*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
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
massless vector boson with the momentum k by $-g^{\\mu \\nu}$.
This corresponds to the summation over all 4 polarizations, including the
unphysical ones.
- DoPolarizationSums[exp, k, n] sums over physical (transverse) polarizations
of an external massless vector boson with the momentum k, where n is an
auxiliary 4-vector from the gauge-dependent polarization sum formula.

Cf. PolarizationSum for more examples and explanations on different
polarizations.

DoPolarizationSums also work with $D$-dimensional amplitudes.";

GaugeTrickN::usage =
"GaugeTrickN is an option for DoPolarizationSums. It specifies the number of
polarizations over which you are summing when you do the gauge trick, (i.e.
replace the polarization sum by $- g^{\\mu \\nu}$).

The default value is 2, which is correct e.g. for real photons as external
states. However, if the external states are virtual photons, then GaugeTrickN
should be set to 4.";

DoPolarizationSums::failmsg =
"Error! DoPolarizationSums has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DoPolarizationSums`Private`"]

dpsVerbose::usage="";

Options[DoPolarizationSums] = {
	Contract		-> True,
	ExtraFactor 	-> 1,
	FCE				-> False,
	FCI				-> False,
	FCVerbose		-> False,
	GaugeTrickN 	-> 2,
	Head			-> Identity,
	VirtualBoson	-> False
};

DoPolarizationSums[expr_, vectors:Except[_?OptionQ].., OptionsPattern[]] :=
	Block[ {polInd1,polInd2,res,ex ,tmp,dim,bosonMomentum,polVectorsList,freePart,polPart,head1=Null,head2=Null},

		If [OptionValue[FCVerbose]===False,
			dpsVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				dpsVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1,"ComplexConjugate: Entering.", FCDoControl->dpsVerbose];
		FCPrint[3,"ComplexConjugate: Entering with: ", expr, FCDoControl->dpsVerbose];

		bosonMomentum = {vectors}[[1]];

		FCPrint[1,"ComplexConjugate: Vector boson momentum: ", bosonMomentum, FCDoControl->dpsVerbose];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		polInd1 = $MU[Unique[]];
		polInd2 = $MU[Unique[]];

		polVectorsList = SelectNotFree[SelectNotFree[Sort[DeleteDuplicates[Cases[ex,_Momentum | _CartesianMomentum,Infinity]]],Polarization],bosonMomentum];

		FCPrint[1,"ComplexConjugate: Polarization vectors present in the expression: ", polVectorsList, FCDoControl->dpsVerbose];

		If[	polVectorsList=!={},

			If[	!MatchQ[polVectorsList, {
					(CartesianMomentum|Momentum)[Polarization[bosonMomentum,Complex[0,1], ___Rule],di___],
					(CartesianMomentum|Momentum)[Polarization[bosonMomentum,Complex[0,-1], ___Rule],di___]} | {
					(CartesianMomentum|Momentum)[Polarization[bosonMomentum,Complex[0,-1], ___Rule],di___],
					(CartesianMomentum|Momentum)[Polarization[bosonMomentum,Complex[0,1], ___Rule],di___]}],
				Print[StandardForm[polVectorsList]];
				Message[DoPolarizationSums::failmsg,"Polarization vectors do not seem to appear in a proper way in the expression."];
				Abort[]
			];

			dim = FCGetDimensions[polVectorsList,ChangeDimension->True]//First;

			FCPrint[1,"ComplexConjugate: Spacetime dimension: ", dim, FCDoControl->dpsVerbose];

			tmp = ex/.{
				Momentum[Polarization[bosonMomentum,Complex[0,1], ___Rule],dim] :>
					(head1=LorentzIndex; LorentzIndex[polInd1,dim]),
				CartesianMomentum[Polarization[bosonMomentum,Complex[0,1], ___Rule],dim-1] :>
					(head1=CartesianIndex; CartesianIndex[polInd1,dim-1]),

				Momentum[Polarization[bosonMomentum,Complex[0,-1], ___Rule],dim] :>
					(head2=LorentzIndex; LorentzIndex[polInd2,dim]),
				CartesianMomentum[Polarization[bosonMomentum,Complex[0,-1], ___Rule],dim-1] :>
					(head2=CartesianIndex; CartesianIndex[polInd2,dim-1])
			};

			{freePart, polPart} = FCSplit[tmp,{polInd1,polInd2}],

			(* No polarization vectors in the expression *)
			freePart = ex;
			polPart = 0;
		];

		If[	freePart=!=0,
			FCPrint[0,"ComplexConjugate: The input expression contains terms free of polarization vectors. Those will be multiplied with the number of polarizations.", FCDoControl->dpsVerbose]
		];

		Which[
			(*massive vector boson*)
			Length[{vectors}] === 1 && bosonMomentum=!=0,
				If[ polPart=!=0,
					polPart = OptionValue[Head][PolarizationSum[polInd1,polInd2,bosonMomentum, Dimension->dim, Heads->{head1,head2}]] polPart
				];
				freePart = (dim - 1) freePart,

			(*massless vector boson*)
			Length[{vectors}] === 2 && bosonMomentum=!=0,
				If[	polPart=!=0,
					polPart = OptionValue[Head][PolarizationSum[polInd1,polInd2,bosonMomentum, {vectors}[[2]], Dimension->dim, VirtualBoson-> OptionValue[VirtualBoson], Heads->{head1,head2}]] polPart;
				];
				If[{vectors}[[2]]=!=0,
					(* propex axiliary vector*)
					freePart = (dim - 2) freePart,
					(* g^{mu nu} trick*)
					freePart = OptionValue[GaugeTrickN] freePart
				],
			True,
				Message[DoPolarizationSums::failmsg,"Unknown polarization sum"];
				Abort[]
		];

		If[ OptionValue[Contract],
			polPart = Contract[polPart,FCI->True]
		];

		res = OptionValue[ExtraFactor] freePart + OptionValue[ExtraFactor] polPart;

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res

	]/; Length[{vectors}] === 1 || Length[{vectors}] === 2;

FCPrint[1,"DoPolarizationSums.m loaded."];
End[]
