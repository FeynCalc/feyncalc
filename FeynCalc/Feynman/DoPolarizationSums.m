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

GaugeTrickN::usage =
"GaugeTrickN is an option for DoPolarizationSums. It specifies the number \
of polarizations over which you are summing when you do the gauge trick, \
(i.e. replace the polarization sum by - MT[mu,nu]). The default value is 2, \
which is correct e.g. for real photons as external states. However, if the \
external states are virtual photons, then GaugeTrickN should be set to 4.
"

DoPolarizationSums::failmsg =
"Error! DoPolarizationSums has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DoPolarizationSums`Private`"]

Options[DoPolarizationSums] = {
	Contract -> True,
	ExtraFactor -> 1,
	FCE -> False,
	FCI -> False,
	GaugeTrickN -> 2,
	Head -> Identity,
	VirtualBoson -> False
};

DoPolarizationSums[expr_, vectors:Except[_?OptionQ].., OptionsPattern[]] :=
	Block[ {polInd1,polInd2,res,ex ,tmp,dim,kk,polList,freePart,polPart},

		kk = {vectors}[[1]];

		If[	!FreeQ2[expr, FeynCalc`Package`NRStuff],
			Message[FeynCalc::nrfail];
			Abort[]
		];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		polInd1 = $MU[Unique[]];
		polInd2 = $MU[Unique[]];

		polList = SelectNotFree[SelectNotFree[Sort[DeleteDuplicates[Cases[ex,_Momentum,Infinity]]],Polarization],kk];

		If[	polList=!={},

			If[	!MatchQ[polList,
				{Momentum[Polarization[kk,Complex[0,1], ___Rule],di___],Momentum[Polarization[kk,Complex[0,-1], ___Rule],di___]} |
				{Momentum[Polarization[kk,Complex[0,-1], ___Rule],di___],Momentum[Polarization[kk,Complex[0,1], ___Rule],di___]}],
				Print[StandardForm[polList]];
				Message[DoPolarizationSums::failmsg,"Polarization vector do not seem to appear in a proper way in the expression."];
				Abort[]
			];

			dim = FCGetDimensions[polList]//First;

			tmp = ex/.{
				Momentum[Polarization[kk,Complex[0,1], ___Rule],dim] :> LorentzIndex[polInd1,dim],
				Momentum[Polarization[kk,Complex[0,-1], ___Rule],dim] :> LorentzIndex[polInd2,dim]
			};

			{freePart, polPart} = FCSplit[tmp,{polInd1,polInd2}],

			(* No polarization vectors in the expression *)
			freePart = ex;
			polPart = 0;
		];

		Which[
			Length[{vectors}] === 1 && kk=!=0,
				If[ polPart=!=0,
					polPart = OptionValue[Head][PolarizationSum[polInd1,polInd2,kk, Dimension->dim]] polPart
				];
				freePart = 3 freePart,

			Length[{vectors}] === 2 && kk=!=0,
				If[	polPart=!=0,
					polPart = OptionValue[Head][PolarizationSum[polInd1,polInd2,kk, {vectors}[[2]], Dimension->dim, VirtualBoson-> OptionValue[VirtualBoson]]] polPart;
				];
				If[{vectors}[[2]]=!=0,
					freePart = 2 freePart,
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
