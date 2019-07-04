(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DoPolarizationSums						*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2019 Rolf Mertig
	Copyright (C) 1997-2019 Frederik Orellana
	Copyright (C) 2014-2019 Vladyslav Shtabovenko
*)

(* :Summary:  Compute polarization sums of vector bosons *)

(* ------------------------------------------------------------------------ *)

DoPolarizationSums::usage =
"DoPolarizationSums[exp,k,n] sums over physical (transverse) \
polarizations of an external massless vector boson with momentum k, \
where n is an auxiliary 4-vector from the gauge-dependent \
polarization sum formula.\n
DoPolarizationSums[exp,k,0] replaces the polarization sum of an external massless vector \
boson with momentum k by - MT[mu,nu]. This corresponds to the summation over all four \
polarizations, including the unphysical ones. \n
DoPolarizationSums[exp,k] sums over the three polarizations \
of an external massive vector boson with momentum k and mass SP[k,k].";

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
	Block[ {polInd1,polInd2,res,ex ,tmp,dim,kk,polList,freePart,polPart,head1=Null,head2=Null},

		kk = {vectors}[[1]];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		polInd1 = $MU[Unique[]];
		polInd2 = $MU[Unique[]];

		polList = SelectNotFree[SelectNotFree[Sort[DeleteDuplicates[Cases[ex,_Momentum | _CartesianMomentum,Infinity]]],Polarization],kk];

		If[	polList=!={},

			If[	!MatchQ[polList, {
					(CartesianMomentum|Momentum)[Polarization[kk,Complex[0,1], ___Rule],di___],
					(CartesianMomentum|Momentum)[Polarization[kk,Complex[0,-1], ___Rule],di___]} | {
					(CartesianMomentum|Momentum)[Polarization[kk,Complex[0,-1], ___Rule],di___],
					(CartesianMomentum|Momentum)[Polarization[kk,Complex[0,1], ___Rule],di___]}],
				Print[StandardForm[polList]];
				Message[DoPolarizationSums::failmsg,"Polarization vector do not seem to appear in a proper way in the expression."];
				Abort[]
			];

			dim = FCGetDimensions[polList,ChangeDimension->True]//First;

			tmp = ex/.{
				Momentum[Polarization[kk,Complex[0,1], ___Rule],dim] :> (head1=LorentzIndex; LorentzIndex[polInd1,dim]),
				CartesianMomentum[Polarization[kk,Complex[0,1], ___Rule],dim-1] :> (head1=CartesianIndex; CartesianIndex[polInd1,dim-1]),

				Momentum[Polarization[kk,Complex[0,-1], ___Rule],dim] :> (head2=LorentzIndex; LorentzIndex[polInd2,dim]),
				CartesianMomentum[Polarization[kk,Complex[0,-1], ___Rule],dim-1] :> (head2=CartesianIndex; CartesianIndex[polInd2,dim-1])
			};

			{freePart, polPart} = FCSplit[tmp,{polInd1,polInd2}],

			(* No polarization vectors in the expression *)
			freePart = ex;
			polPart = 0;
		];

		Which[
			Length[{vectors}] === 1 && kk=!=0,
				If[ polPart=!=0,
					polPart = OptionValue[Head][PolarizationSum[polInd1,polInd2,kk, Dimension->dim, Heads->{head1,head2}]] polPart
				];
				freePart = 3 freePart,

			Length[{vectors}] === 2 && kk=!=0,
				If[	polPart=!=0,
					polPart = OptionValue[Head][PolarizationSum[polInd1,polInd2,kk, {vectors}[[2]], Dimension->dim, VirtualBoson-> OptionValue[VirtualBoson], Heads->{head1,head2}]] polPart;
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
