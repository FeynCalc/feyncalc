(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCPrepareFAAmp                                                   *)

(*
	This software is covered by the GNU Lesser General Public License 3.
	Copyright (C) 1990-2015 Rolf Mertig
	Copyright (C) 1997-2015 Frederik Orellana
	Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  FCPrepareFAAmp converts a FeynArts amplitude to FeynCalc      *)

(* ------------------------------------------------------------------------ *)

FCPrepareFAAmp::usage =
"FCPrepareFAAmp[exp] converts a FeynArts amplitude to FeynCalc.";

UndoChiralSplittings::usage =
"UndoChiralSplittings is an option of FCPrepareFAAmp. When set to True, it attempts
to undo splittings of couplings into left and right handed pieces, e.g
(a*GA[6].GA[mu] + a*GA[7].GA[mu]) will be converted back to a*GA[mu]";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]


Begin["`FCPrepareFAAmp`Private`"]

Options[FCPrepareFAAmp] = {UndoChiralSplittings -> False};

FCPrepareFAAmp[expr_, OptionsPattern[]] :=
	Block[ {replist1,replist2,replist3,tempvar,temp},
		replist1 = {FeynArts`Index[Global`Lorentz, x_] :> LorentzIndex[ToExpression["Lor" <> ToString[x]]],
					FeynArts`Index[Global`Gluon, x_] :> SUNIndex[ToExpression["Glu" <> ToString[x]]],
					FeynArts`Index[Global`Colour, x_] :> SUNFIndex[ToExpression["Col" <> ToString[x]]],
					FeynArts`FourMomentum[FeynArts`Incoming,x_] :> ToExpression["InMom" <> ToString[x]],
					FeynArts`FourMomentum[FeynArts`Outgoing,x_] :> ToExpression["OutMom" <> ToString[x]],
					FeynArts`FourMomentum[FeynArts`Internal,x_] :> ToExpression["LoopMom" <> ToString[x]]
					};
		replist2 = Dispatch[{
					Conjugate[Global`FAPolarizationVector][_, x_, y_] :> Pair[LorentzIndex[y],Momentum[Polarization[x,-I]]],
					Global`FAChiralityProjector[-1] :> DiracGamma[7],
					Global`FAChiralityProjector[1] :> DiracGamma[6],
					Global`FADiracMatrix :> DiracGamma,
					Global`FADiracSlash[x_] :> DiracGamma[Momentum[x]],
					Global`FADiracSpinor :> Spinor,
					Global`FADiracTrace :> DiracTrace,
					Global`FAFourVector[x_,y_] :> Pair[Momentum[x],y],
					Global`FAGS :> Gstrong,
					Global`FAMetricTensor :> Pair,
					Global`FAPolarizationVector[_, x_, y_] :> Pair[LorentzIndex[y],Momentum[Polarization[x,I]]],
					Global`FASUNF[a_,b_,c_, d_] :> (Clear[tempvar];
													tempvar = Unique[$AL];
													SUNF[a,b,tempvar]SUNF[tempvar,c,d]),
					Global`FASUNF[a_,b_,c_] :> SUNF[a,b,c],
					Global`FASUNT :> SUNTF,
					FeynArts`IndexDelta[a_SUNFIndex, b_SUNFIndex]:> SUNFDelta[a,b],
					FeynArts`IndexDelta[a_, b_]/; Head[a]=!=SUNFindex && Head[b]=!=SUNFIndex :> SUNFDelta[SUNFIndex[a],SUNFIndex[b]],
					FeynArts`FAFeynAmp :> FeynAmp,
					FeynArts`FAFeynAmpDenominator[x__] :> (FeynAmpDenominator[x]/.FeynArts`FAPropagatorDenominator :> PropagatorDenominator),
					FeynArts`FAGaugeXi :> GaugeXi,
					FeynArts`FANonCommutative :> DOT,
					FeynArts`FermionChain :> DOT,
					FeynArts`MatrixTrace :> DiracTrace
					}];
		replist3 = {FeynArts`FAPropagatorDenominator[x__] :> FeynAmpDenominator[PropagatorDenominator[x]]};
		temp = expr /. replist1 /. replist2 /. replist3;
		If[ OptionValue[UndoChiralSplittings],
			temp = temp/.{(a1__ DiracGamma[x_].DiracGamma[6] a2__ + a1__ DiracGamma[x_].DiracGamma[7] a2__) :> a1 DiracGamma[x] a2}
		];
		temp
	];

FCPrint[1,"FCPrepareFAAmp.m loaded."];
End[]
