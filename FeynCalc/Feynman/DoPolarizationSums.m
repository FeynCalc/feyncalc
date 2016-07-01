(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DoPolarizationSums						*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Compute polarization sums of vector bosons *)

(* ------------------------------------------------------------------------ *)

DoPolarizationSums::usage =
"DoPolarizationSums[exp,k,n] sums over physical (transverse) \
polarizations of external massless vector bosons with momentum k. \
Here, n is an auxiliary four vector that goes into the gauge-dependent \
polarization sum to ensure that we are summing only over physical polarizations.

DoPolarizationSums[exp,k,0] replaces the polarization sum of external massless vector \
bosons with momentum k by -g(mu,nu). This corresponds to summing over all four \
physical and unphysical polarizations.

DoPolarizationSums[exp,k] sums over polarizations \
of external massive vector bosons with momentum k and mass k^2.
";

PolarizationUncontract::usage =
"PolarizationUncontract[exp,k] does Uncontract
on scalar products involving polarization vectors that depend on k.";

GaugeTrickN::usage =
"GaugeTrickN is an option for DoPolarizationSums. It specifies the number \
of polarizations over which you are summing when you do the gauge trick, \
(i.e. replace the polarization sum by - MT[mu,nu]). The default value is 2, \
which is correct e.g. for real photons as external states. However, if the \
external states are virtual photons, then GaugeTrickN should be set to 4.
"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DoPolarizationSums`Private`"]


DoPolarizationSums::"noresolv" =
	"Could not resolve polarization structure of `1`. Evaluation aborted!";

Options[DoPolarizationSums] = {
	Contract -> True,
	ExtraFactor -> 1,
	GaugeTrickN -> 2,
	VirtualBoson -> False
};



Options[doPolarizationSum] = {
	Contract -> True,
	VirtualBoson -> False,
	GaugeTrickN -> 2
};

(*    This is done for performance reasons. Instead of uncontracting every terms that involves k (what Uncontract would
by default), we uncontract only contractions with polarization vectors.    *)
PolarizationUncontract[expr_, k_, opts:OptionsPattern[]] :=
	Block[ {temp,polvecmom1,polvecmom2,op1,op2, tmp},

		(*TODO Caching! *)
		tmp  = Collect2[ExpandScalarProduct[EpsEvaluate[expr],Momentum->{k}],k,Factoring->False];
		tmp = tmp/.{
			Polarization[k,Complex[0,1],op___Rule]:> (op1 = op; polvecmom1),
			Polarization[k,Complex[0,-1],op___Rule]:> (op2 = op; polvecmom2)
		};
		temp = Uncontract[tmp,polvecmom1,polvecmom2,FilterRules[Join[{opts},{Pair->All}], Options[Uncontract]]];
		temp /.{polvecmom1 :> Polarization[k, Complex[0,1],op1],polvecmom2 :> Polarization[k, Complex[0,-1],op2]}
	];

(*    Polarization sums for massless vector bosons.    *)
doPolarizationSum[expr_,k_, n:Except[_?OptionQ], opts:OptionsPattern[]] :=
	Block[ {temp,viBo},
		viBo = OptionValue[VirtualBoson];
		Which[
			Count[expr, Polarization[k,__], Infinity, Heads -> True] === 0,
				If[ k=!=0 && n=!=0,
					2 expr,
					OptionValue[GaugeTrickN] expr
				],
			Count[expr, Polarization[k,__], Infinity, Heads -> True] // EvenQ,
				temp = (expr /. Pair[LorentzIndex[rho1_, dim_:4],
				Momentum[Polarization[k, -I, OptionsPattern[]], dim_:4]] Pair[
				LorentzIndex[rho2_, dim_:4],
				Momentum[Polarization[k, I, OptionsPattern[]], dim_:4]] :>
				PolarizationSum[rho1,rho2,k,n, Dimension->dim, VirtualBoson-> viBo]);
				If[ OptionValue[Contract],
					Contract[temp],
					temp
				],
			True,
					Message[DoPolarizationSums::"noresolv", InputForm[expr]];
		]
	]/; (k=!=0 && n=!=0) || (k=!=0 && n===0);
(*    Polarization sums for massive vector bosons with mass k^2.    *)
doPolarizationSum[expr_,k:Except[_?OptionQ], OptionsPattern[]] :=
	Block[ {temp},
		Which[
			Count[expr, Polarization[k,__], Infinity, Heads -> True] === 0,
				3 expr,
			Count[expr, Polarization[k,__], Infinity, Heads -> True] // EvenQ,
				temp = (expr //. Pair[LorentzIndex[rho1_, dim_:4],
				Momentum[Polarization[k, -I, OptionsPattern[]], dim_:4]] Pair[
				LorentzIndex[rho2_, dim_:4],
				Momentum[Polarization[k, I, OptionsPattern[]], dim_:4]] :>
				PolarizationSum[rho1,rho2,k, Dimension->dim]);
				If[ OptionValue[Contract],
					Contract[temp],
					temp
				],
			True,
					Message[DoPolarizationSums::"noresolv", InputForm[expr]];
		]
	]/; k=!=0;
DoPolarizationSums[expr_, vectors:Except[_?OptionQ].., opts:OptionsPattern[]] :=
	Block[ {exp1, exp2,polvecmom1,polvecmom2,res},
		exp1 = PolarizationUncontract[FCI[expr],{vectors}[[1]],opts];
		If[ Head[exp1] === Plus,
			exp2 = List @@ exp1,
			exp2 = {exp1}
		];
		OptionValue[ExtraFactor]*Total[Map[doPolarizationSum[#,vectors,FilterRules[{opts},Options[doPolarizationSum]]]&,exp2]]
	]/; Length[{vectors}] === 1 || Length[{vectors}] === 2;

FCPrint[1,"DoPolarizationSums.m loaded."];
End[]
