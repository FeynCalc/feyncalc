(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCPrepareFAAmp                                                   *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  FCPrepareFAAmp converts a FeynArts amplitude to FeynCalc      *)

(* ------------------------------------------------------------------------ *)

FCPrepareFAAmp::usage =
"FCPrepareFAAmp[exp]  is an auxiliary function for a partial conversion of a
FeynArts amplitude to FeynCalc.";

UndoChiralSplittings::usage =
"UndoChiralSplittings is an option of FCPrepareFAAmp. When set to True, it
attempts to undo splittings of couplings into left and right handed pieces,
e.g. $a P_L \\gamma^\\mu + a P_R \\gamma^\\mu$ will be replaced by $a \\gamma^\\mu$.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]


Begin["`FCPrepareFAAmp`Private`"]

tmp::usage="";

Options[FCPrepareFAAmp] = {
	FeynAmpDenominatorCombine	-> True,
	SMP 						-> False,
	UndoChiralSplittings 		-> False
};

FCPrepareFAAmp[expr_, OptionsPattern[]] :=
	Block[{	replist0,replist1,replist2,replist3,repListSMP,tempvar,temp,holdDOT,
			optFeynAmpDenominatorCombine},

			optFeynAmpDenominatorCombine = OptionValue[FeynAmpDenominatorCombine];

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
			FCGV["MZ"] -> SMP["m_Z"],
			HoldPattern[FeynArts`CKM[1,1]] -> SMP["V_ud",I],
			HoldPattern[FeynArts`CKM[1,2]] -> SMP["V_us",I],
			HoldPattern[FeynArts`CKM[1,3]] -> SMP["V_ub",I],
			HoldPattern[FeynArts`CKM[2,1]] -> SMP["V_cd",I],
			HoldPattern[FeynArts`CKM[2,2]] -> SMP["V_cs",I],
			HoldPattern[FeynArts`CKM[2,3]] -> SMP["V_cb",I],
			HoldPattern[FeynArts`CKM[3,1]] -> SMP["V_td",I],
			HoldPattern[FeynArts`CKM[3,2]] -> SMP["V_ts",I],
			HoldPattern[FeynArts`CKM[3,3]] -> SMP["V_tb",I]
		};



		replist0 = {
			NonCommutative[x__] :> FeynArts`FANonCommutative[x],
			FeynArts`IndexSum[x_, {ind_, _, _}, moreinds__] /; Length[Cases[x, ind, Infinity, Heads -> True]] == 2 && FreeQ[x, FeynArts`IndexSum] :>
				(tmp = Unique["Ind"]; FeynArts`IndexSum[x /. ind -> tmp, moreinds]),
			FeynArts`IndexSum[x_, {ind_, _, _}] /; Length[Cases[x, ind, Infinity, Heads -> True]] == 2 && FreeQ[x,FeynArts`IndexSum] :>
				(tmp = Unique["Ind"] ; x /. ind -> tmp)
		};

		replist1 = {FeynArts`Index[Global`Lorentz, x_] :> LorentzIndex[ToExpression["Lor" <> ToString[x]]],
					FeynArts`Index[Global`Gluon, x_] :> SUNIndex[ToExpression["Glu" <> ToString[x]]],
					FeynArts`Index[Global`Colour, x_] :> SUNFIndex[ToExpression["Col" <> ToString[x]]],
					FeynArts`Index[Global`Dirac, x_] :> DiracIndex[ToExpression["Dir" <> ToString[x]]],
					FeynArts`FourMomentum[FeynArts`Incoming,x_] :> ToExpression["InMom" <> ToString[x]],
					FeynArts`FourMomentum[FeynArts`Outgoing,x_] :> ToExpression["OutMom" <> ToString[x]],
					FeynArts`FourMomentum[FeynArts`Internal,x_] :> ToExpression["LoopMom" <> ToString[x]]
					};
		replist2 = Dispatch[{
					Conjugate[Global`FAPolarizationVector][_, x_, y_] :> Pair[LorentzIndex[y],Momentum[Polarization[x,-I]]],
					Global`FAChiralityProjector[-1] :> DiracGamma[7],
					Global`FAChiralityProjector[1] :> DiracGamma[6],
					Global`FADiracMatrix[x_] :> DiracGamma[LorentzIndex[x]],
					Global`FAScalarProduct[x_,y_] :> Pair[Momentum[x],Momentum[y]],
					Global`FADiracSlash[x_] :> DiracGamma[Momentum[x]],
					Global`FADiracSpinor[x_,re__] :> Spinor[Momentum[x], re],
					Global`MajoranaSpinor[x_,re__] :> Spinor[Momentum[x], re],
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
					Global`FASUNT[a_,b_,c_,d_] :> SUNTF[{a,b},c,d],
					Global`FASUNT[a_,b_,c_] :> SUNTF[a,b,c],
					FeynArts`IndexDelta[a: (_SUNFIndex| _ExplicitSUNFIndex), b:(_SUNFIndex| _ExplicitSUNFIndex)]:> SUNFDelta[a,b],
					FeynArts`IndexDelta[a: (_SUNIndex| _ExplicitSUNIndex), b:(_SUNIndex| _ExplicitSUNIndex)]:> SUNDelta[a,b],
					FeynArts`FAFeynAmp :> FeynAmp,
					FeynArts`FAFeynAmpDenominator[x__] :> (
						FeynAmpDenominator@@({x}/. z_Momentum:>z[[1]]/. {
						FeynArts`FAPropagatorDenominator[a_,b_] :> PropagatorDenominator[Momentum[a],b],
						FeynArts`FAPropagatorDenominator[a_,b_,n_Integer] :> Sequence@@Table[PropagatorDenominator[Momentum[a],b],{j,1,n}]
						})),
					FeynArts`FAGaugeXi :> GaugeXi,
					FeynArts`FANonCommutative :> DOT
					}];
		replist3 = {
			FeynArts`FermionChain :> DOT,
			FeynArts`DiracObject[a__][i_DiracIndex] :> DiracChain[DOT[a],i],
			FeynArts`DiracObject[a__][i_DiracIndex, j_DiracIndex] :> DiracChain[DOT[a],i,j],
			FeynArts`MatrixTrace :> DiracTrace,
			FeynArts`FAPropagatorDenominator[x_,y_] :> FeynAmpDenominator[PropagatorDenominator[Momentum[(x/. z_Momentum:>z[[1]])],y]],
			FeynArts`FAPropagatorDenominator[x_,y_,n_Integer]/;optFeynAmpDenominatorCombine :>
				FeynAmpDenominator@@Table[PropagatorDenominator[Momentum[(x/. z_Momentum:>z[[1]])],y],{j,1,n}],
			FeynArts`FAPropagatorDenominator[x_,y_,n_Integer]/;!optFeynAmpDenominatorCombine :>
				Product[FeynAmpDenominator[PropagatorDenominator[Momentum[(x/. z_Momentum:>z[[1]])],y]] ,{j,1,n}]
		};
		temp = FCReplaceRepeated[expr, replist0, replist1, replist2, replist3];

		If[	OptionValue[SMP],
			temp = temp /.repListSMP
		];

		If[ OptionValue[UndoChiralSplittings],
			temp = temp//.{

				(*Single chains*)

				(* g^mu P_R + g^mu R_L -> g^mu *)
				(a1__ DiracGamma[x_].DiracGamma[6] a2__ + a1__ DiracGamma[x_].DiracGamma[7] a2__) :>
					a1 DiracGamma[x] a2,

				(* (g^mu P_R)_ij + (g^mu R_L_ij -> (g^mu)_ij *)
				(a_. DiracChain[(c_. DiracGamma[x_]).DiracGamma[6], i_DiracIndex, j_DiracIndex] + a_. DiracChain[(c_. DiracGamma[x_]).DiracGamma[7], i_DiracIndex, j_DiracIndex] ) :>
					a DiracChain[c DiracGamma[x], i, j],
				(a1__ DiracGamma[6] a2__ + a1__ DiracGamma[7] a2__) :> a1 a2,

				(* (P_R)_ij + (R_L)_ij -> (1)_ij *)
				(a_. DiracChain[DiracGamma[6], i_DiracIndex, j_DiracIndex] + a_. DiracChain[DiracGamma[7], i_DiracIndex, j_DiracIndex]) :>
					a DiracChain[1, i, j],

				(*Double chains*)
				(* (g^mu P_x)_ij (g^mu P_R)_kl + (g^nu P_x)_ij (g^nu P_L)_kl -> (g^mu P_x)_ij (g^mu)_kl  *)
				(a_. DiracChain[DiracGamma[x1_LorentzIndex].DiracGamma[(h:7|6)], i_DiracIndex, j_DiracIndex] *
					DiracChain[DiracGamma[x1_LorentzIndex].DiracGamma[6], k_DiracIndex, l_DiracIndex] +
				a_. DiracChain[DiracGamma[x2_LorentzIndex].DiracGamma[(h:7|6)], i_DiracIndex, j_DiracIndex] *
					DiracChain[DiracGamma[x2_LorentzIndex].DiracGamma[7], k_DiracIndex, l_DiracIndex] ) :>
					a DiracChain[DiracGamma[x1].DiracGamma[h], i, j] DiracChain[DiracGamma[x1], k, l],

				(* (g^mu)_ij (g^mu P_R)_kl + (g^nu)_ij (g^nu P_L)_kl -> (g^mu)_ij (g^mu)_kl  *)
				(a_. DiracChain[DiracGamma[x1_LorentzIndex], i_DiracIndex, j_DiracIndex] *
					DiracChain[DiracGamma[x1_LorentzIndex].DiracGamma[6], k_DiracIndex, l_DiracIndex]

				+ a_. DiracChain[DiracGamma[x2_LorentzIndex], i_DiracIndex, j_DiracIndex] *
					DiracChain[DiracGamma[x2_LorentzIndex].DiracGamma[7], k_DiracIndex, l_DiracIndex] ) :>
					a DiracChain[DiracGamma[x1], i, j] DiracChain[DiracGamma[x1], k, l]


			} /. DOT -> holdDOT /. {
				a_. holdDOT[-DiracGamma[x_], DiracGamma[6]] + a_. holdDOT[-DiracGamma[x_], DiracGamma[7]] :> -a DiracGamma[x],

				a_. DiracChain[holdDOT[-DiracGamma[x_], DiracGamma[6]], i_DiracIndex, j_DiracIndex] + a_. DiracChain[holdDOT[-DiracGamma[x_], DiracGamma[7]], i_DiracIndex, j_DiracIndex] :>
					-a DiracChain[DiracGamma[x], i, j]

			}/. holdDOT -> DOT
		];

		temp
	];

FCPrint[1,"FCPrepareFAAmp.m loaded."];
End[]
