(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ChangeDimension													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Change dimension of Lorentz and Cartesian tensors				*)

(* ------------------------------------------------------------------------ *)

ChangeDimension::usage=
"ChangeDimension[exp, dim] changes all LorentzIndex and Momentum objects in \
exp to dimension dim (and thus also Dirac slashes and Dirac matrices \
in FeynCalcInternal-representation). CIndex and CMomentum objects become \
changed to dimension dim-1.";

ChangeDimension::failmsg =
"Error! ChangeDimension has encountered a fatal problem and must abort \
the computation. The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ChangeDimension`Private`"]

Options[ChangeDimension] ={
	FCI -> False,
	FCE -> False
};

ChangeDimension[ex_, dim_, OptionsPattern[]] :=
	Block[
		{	expr, res, tmp, lorentzDim, cartesianDim,
			holdPair, holdDiracGamma, holdCPair, holdEps, holdPauliSigma},

		If[ OptionValue[FCI],
			expr = ex,
			expr = FCI[ex]
		];

		Switch[ dim,
			4,
				lorentzDim = 4;
				cartesianDim = 3,
			_Symbol,
				lorentzDim = dim;
				cartesianDim = dim-1,
			_Symbol -4,
				lorentzDim = dim;
				cartesianDim = dim,
			_,
			Message[ChangeDimension::failmsg, "Unsupported choice of dimension!"];
			Abort[]
		];

		tmp = expr /. DiracGamma -> holdDiracGamma /. PauliSigma -> holdPauliSigma /. Pair-> holdPair /. CPair->holdCPair  /. Eps -> holdEps;

		tmp = tmp /. holdDiracGamma[(z: 5|6|7)] :> DiracGamma[z];

		tmp = tmp /. {
			LorentzIndex[z_,___] :> LorentzIndex[z,lorentzDim],
			CIndex[z_,___] :> CIndex[z,cartesianDim],
			Momentum[z_,___] :> Momentum[z,lorentzDim],
			CMomentum[z_,___] :> CMomentum[z,cartesianDim]
		};

		tmp = tmp /. {
			holdDiracGamma[z_,___] :> holdDiracGamma[z,lorentzDim],
			holdPauliSigma[z_,___] :> holdPauliSigma[z,cartesianDim]
		};

		res = tmp /. holdDiracGamma -> DiracGamma /. holdPauliSigma -> PauliSigma /. holdPair -> Pair /. holdCPair -> CPair /. holdEps -> Eps;


		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res

	];

FCPrint[1,"ChangeDimension.m loaded"];
End[]
