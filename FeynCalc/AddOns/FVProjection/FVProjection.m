(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FVProjection														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary: 	Toy add-on for FeynCalc for obtaining transverse and
				longitudinal projections of a 4-vector w.r.t the given
				direction													*)

(* ------------------------------------------------------------------------ *)

FVProjectionT::usage=
"FVProjectionT[x_,mu_,p_] returns the transverse component of the 4-vector \
x^mu with respect to the 4-vector p";

FVProjectionL::usage=
"FVProjectionL[x_,mu_,p_] returns the longitudinal component of the 4-vector \
x^mu with respect to the 4-vector p";

FVProjectorT::usage=
"FVProjectorT[mu,nu,p] returns the transverse projector with respect to \
the 4-vector p";

FVProjectorL::usage=
"FVProjectorL[mu,nu,p] returns the longitudinal projector with respect to \
the 4-vector p";

$FVProjectionVersion::usage=
"$FVProjectionVersion is the string that represents the version of FVProjection";

$FVProjectionDirectory::usage=
"$FVProjectionDirectory is the string that represents the full path to the FVProjection \
directory";

FVProjection::nullvec=
"Error! Cannot compute projections with respect to a null vector! Evaluation aborted.";

FVProjection::fail=
"Unknown error! Evaluation aborted.";

Begin["`Package`"]
End[]

Begin["`FVProjection`Private`"];

$FVProjectionVersion="1.0.0";

$FVProjectionDirectory =
ToFileName[{$FeynCalcDirectory, "AddOns", "FVProjection"}];

Options[FVProjectionT] = {
	Dimension -> 4,
	FCE -> False
};

Options[FVProjectionL] = {
	Dimension -> 4,
	FCE -> False
};

Options[FVProjectorT] = {
	Dimension -> 4,
	FCE -> False
};

Options[FVProjectorL] = {
	Dimension -> 4,
	FCE -> False
};


FVProjectionT[x_,mu_,p_, OptionsPattern[]]:=
	projection[x,p,mu,0,FVProjectionT,{OptionValue[Dimension],OptionValue[FCE]}];

FVProjectionL[x_,mu_,p_, OptionsPattern[]]:=
	projection[x,p,mu,0,FVProjectionL,{OptionValue[Dimension],OptionValue[FCE]}];

FVProjectorT[mu_,nu_, p_, OptionsPattern[]]:=
	projection[0,p,mu,nu,FVProjectorT,{OptionValue[Dimension],OptionValue[FCE]}];

FVProjectorL[mu_,nu_, p_, OptionsPattern[]]:=
	projection[0,p,mu,nu,FVProjectorL,{OptionValue[Dimension],OptionValue[FCE]}];

projection[x_, p_, mu_, nu_,  type_, {dim_,fce_}]:=
	Block[{res},

		If [ p===0 || Pair[Momentum[p,dim],Momentum[p,dim]]===0,
			Message[FVProjection::nullvec];
			Abort[]
		];

		Switch[type,
			FVProjectionT,
				res = Pair[Momentum[x,dim],LorentzIndex[mu,dim]] -
				Pair[Momentum[p,dim],LorentzIndex[mu,dim]]*
				Pair[Momentum[x,dim],Momentum[p,dim]]/Pair[Momentum[p,dim],Momentum[p,dim]],
			FVProjectionL,
				res = Pair[Momentum[p,dim],LorentzIndex[mu,dim]]*
				Pair[Momentum[x,dim],Momentum[p,dim]]/Pair[Momentum[p,dim],Momentum[p,dim]],
			FVProjectorT,
				res = Pair[LorentzIndex[mu,dim],LorentzIndex[nu,dim]] -
				Pair[Momentum[p,dim],LorentzIndex[mu,dim]]*
				Pair[Momentum[p,dim],LorentzIndex[nu,dim]]/Pair[Momentum[p,dim],Momentum[p,dim]],
			FVProjectorL,
				res = Pair[Momentum[p,dim],LorentzIndex[mu,dim]]*
				Pair[Momentum[p,dim],LorentzIndex[nu,dim]]/Pair[Momentum[p,dim],Momentum[p,dim]],
			_,
			Message[FVProjection::fail];
			Abort[]
		];

		If[fce,
			res = FCE[res]
		];

		res
	];


(* Print startup message *)
If[ $FeynCalcStartupMessages =!= False,
	Print[Style["FVProjector ", "Text", Bold], Style[$FVProjectionVersion <> " loaded.", "Text"]]
];


End[]
