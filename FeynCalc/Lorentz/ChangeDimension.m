(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ChangeDimension													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Change dimension of Lorentz and Cartesian tensors				*)

(* ------------------------------------------------------------------------ *)

ChangeDimension::usage=
"ChangeDimension[exp, dim] changes all LorentzIndex and Momentum symbols in exp
to dimension dim (and also Levi-Civita-tensors, Dirac slashes and Dirac
matrices).

Notice that the dimension of CartesianIndex and CartesianMomentum objects will
be changed to dim-1, not dim.";

ChangeDimension::failmsg =
"Error! ChangeDimension has encountered a fatal problem and must abort \
the computation. The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ChangeDimension`Private`"]

Options[ChangeDimension] ={
	FCI -> False,
	FCE -> False
};

ChangeDimension[ex_, dim_, OptionsPattern[]] :=
	Block[{	expr, res, tmp, lorentzDim, cartesianDim,
			holdPair, holdDiracChain, holdPauliChain,
			holdDiracGamma, holdCartesianPair, holdEps, holdPauliSigma},

		If[ OptionValue[FCI],
			expr = ex,
			expr = FCI[ex]
		];


		Switch[Together[dim+4],
			(* For dim being D-4 we need to put both Lorentz and Cartesian dimensions to D-4 *)
			_Symbol,
				lorentzDim = dim;
				cartesianDim = dim,
			(* Otherwise the Cartesian dimension is one unit less than the Lorentz dimension *)
			_,
				lorentzDim = dim;
				cartesianDim = dim-1
		];

		tmp = expr /. DiracChain -> holdDiracChain  /. PauliChain -> holdPauliChain  /. DiracGamma -> holdDiracGamma /. PauliSigma -> holdPauliSigma /. Pair-> holdPair /. CartesianPair->holdCartesianPair  /. Eps -> holdEps;

		tmp = tmp /. holdDiracGamma[(z: 5|6|7)] :> DiracGamma[z];

		tmp = tmp /. {
			LorentzIndex[z_,___] :> LorentzIndex[z,lorentzDim],
			CartesianIndex[z_,___] :> CartesianIndex[z,cartesianDim],
			Momentum[z_,___] :> Momentum[z,lorentzDim],
			CartesianMomentum[z_,___] :> CartesianMomentum[z,cartesianDim]
		};

		tmp = tmp /. {
			holdDiracGamma[z_,___] :> holdDiracGamma[z,lorentzDim],
			holdPauliSigma[z_,___] :> holdPauliSigma[z,cartesianDim]
		};

		res = tmp /. holdDiracGamma -> DiracGamma /. holdPauliSigma -> PauliSigma /. holdPair -> Pair /. holdCartesianPair -> CartesianPair /. holdEps -> Eps /. holdDiracChain -> DiracChain /. holdPauliChain -> PauliChain;


		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res

	];

FCPrint[1,"ChangeDimension.m loaded"];
End[]
