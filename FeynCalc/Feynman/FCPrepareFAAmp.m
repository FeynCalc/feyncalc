(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCPrepareFAAmp                                                   *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  FCPrepareFAAmp converts a FeynArts amplitude to FeynCalc      *)

(* ------------------------------------------------------------------------ *)

FCPrepareFAAmp::usage =
"FCPrepareFAAmp[exp] is an auxiliary function for a partial conversion of a \
FeynArts amplitude to FeynCalc.";

UndoChiralSplittings::usage =
"UndoChiralSplittings is an option of FCPrepareFAAmp. When set to True, it attempts \
to undo splittings of couplings into left and right handed pieces, e.g \
(a*GA[6].GA[mu] + a*GA[7].GA[mu]) will be converted back to a*GA[mu]";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]


Begin["`FCPrepareFAAmp`Private`"]

Options[FCPrepareFAAmp] = {
	SMP -> False,
	UndoChiralSplittings -> False
};

FCPrepareFAAmp[expr_, OptionsPattern[]] :=
	Block[ {replist0,replist1,replist2,replist3,repListSMP,tempvar,temp},

		repListSMP = {
			FCGV["EL"] -> SMP["e"],
			FCGV["CW"] -> SMP["cos_W"],
			FCGV["SW"] -> SMP["sin_W"],
			FCGV["ME"] -> SMP["m_e"],
			FCGV["MM"] -> SMP["m_mu"],
			FCGV["ML"] -> SMP["m_tau"],
			FCGV["MU"] -> SMP["m_u"],
			FCGV["MC"] -> SMP["m_c"],
			FCGV["MT"] -> SMP["m_t"],
			FCGV["MD"] -> SMP["m_d"],
			FCGV["MS"] -> SMP["m_s"],
			FCGV["MB"] -> SMP["m_b"],
			FCGV["MH"] -> SMP["m_H"],
			FCGV["MW"] -> SMP["m_W"],
			FCGV["MZ"] -> SMP["m_Z"]
		};



		replist0 = {NonCommutative[x__] :> FeynArts`FANonCommutative[x]

					};

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
					Global`FAScalarProduct[x_,y_] :> Pair[Momentum[x],Momentum[y]],
					Global`FADiracSlash[x_] :> DiracGamma[Momentum[x]],
					Global`FADiracSpinor :> Spinor,
					FeynArts`FALeviCivita :> Eps,
					Global`FADiracTrace :> DiracTrace,
					Global`FAFourVector[x_,y_] :> Pair[Momentum[x],y],
					Global`FAFourVector[x_] :> Momentum[x],
					Global`FAGS :> SMP["g_s"],
					Global`FAMetricTensor :> Pair,
					Global`FAPolarizationVector[_, x_, y_] :> Pair[LorentzIndex[y],Momentum[Polarization[x,I]]],
					Global`FASUNF[a_,b_,c_, d_] :> (Clear[tempvar];
													tempvar = Unique[$AL];
													SUNF[a,b,tempvar]SUNF[tempvar,c,d]),
					Global`FASUNF[a_,b_,c_] :> SUNF[a,b,c],
					Global`dSUN[a_,b_,c_] :> SUND[a,b,c],
					Global`FASUNT :> SUNTF,
					FeynArts`IndexDelta[a_SUNFIndex, b_SUNFIndex]:> SUNFDelta[a,b],
					FeynArts`IndexDelta[a_SUNIndex, b_SUNIndex]:> SUNDelta[a,b],
					FeynArts`FAFeynAmp :> FeynAmp,
					FeynArts`FAFeynAmpDenominator[x__] :> (FeynAmpDenominator[x]/.FeynArts`FAPropagatorDenominator :> PropagatorDenominator),
					FeynArts`FAGaugeXi :> GaugeXi,
					FeynArts`FANonCommutative :> DOT,
					FeynArts`FermionChain :> DOT,
					FeynArts`MatrixTrace :> DiracTrace
					}];
		replist3 = {FeynArts`FAPropagatorDenominator[x__] :> FeynAmpDenominator[PropagatorDenominator[x]]};
		temp = expr //. replist0 //. replist1 //. replist2 //. replist3;

		If[	OptionValue[SMP],
			temp = temp /.repListSMP
		];

		If[ OptionValue[UndoChiralSplittings],
			temp = temp//.{(a1__ DiracGamma[x_].DiracGamma[6] a2__ + a1__ DiracGamma[x_].DiracGamma[7] a2__) :> a1 DiracGamma[x] a2,
			(a1__ DiracGamma[6] a2__ + a1__ DiracGamma[7] a2__) :> a1 a2}
		];
		temp
	];

FCPrint[1,"FCPrepareFAAmp.m loaded."];
End[]
