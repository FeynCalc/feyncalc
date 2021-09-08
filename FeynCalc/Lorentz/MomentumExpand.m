(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: MomentumExpand                                                   *)

(*
		This software is covered by the GNU General Public License 3.
		Copyright (C) 1990-2021 Rolf Mertig
		Copyright (C) 1997-2021 Frederik Orellana
		Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Expands momenta                                               *)

(* ------------------------------------------------------------------------ *)

MomentumExpand::usage =
"MomentumExpand[expr] expands Momentum[a+b+ ...] in expr into Momentum[a] +
Momentum[b] + ....";

MomentumExpand::failmsg =
"Error! MomentumExpand has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`MomentumExpand`Private`"];
pair::usage="";
noexpand::usage="";

Options[MomentumExpand] = {
	Momentum -> All
};

(*Fast mode*)
MomentumExpand[(head:(Momentum|CartesianMomentum|TemporalMomentum))[args__], OptionsPattern[]]:=
	(momSplit[head, OptionValue[Momentum]][args] /. head-> expandVec[head] /. linearizeVec -> head /. noexpand -> Identity);


MomentumExpand[expr_List, opts:OptionsPattern[]] :=
	MomentumExpand[#, opts]&/@expr;

(*Normal mode*)
MomentumExpand[expr_/;!MemberQ[{Momentum,CartesianMomentum,TemporalMomentum,List},Head[expr]], OptionsPattern[]] :=
	Block[{aux,res,hold1,hold2,listOrig,listEval,repRule,optMomentum},

		optMomentum = OptionValue[Momentum];

		aux = expr /. hd:(Spinor|FeynAmpDenominator|PropagatorDenominator) :> hold1[hd] /.
			hd:(Pair|CartesianPair|TemporalPair) :> hold2[hd];

		If[	!FreeQ[aux, Momentum],
			aux = momExpand[aux,Momentum,optMomentum];
			If[	!FreeQ2[aux,{expandVec,linearizeVec,noexpand}],
				Message[MomentumExpand::failmsg, "Something went wrong during the expansion of 4-momenta."];
				Abort[]
			]
		];

		If[	!FreeQ[aux, CartesianMomentum],
			aux = momExpand[aux,CartesianMomentum,optMomentum];
			If[	!FreeQ2[aux,{expandVec,linearizeVec,noexpand}],
				Message[MomentumExpand::failmsg, "Something went wrong during the expansion of 3-momenta."];
				Abort[]
			]
		];

		If[	!FreeQ[aux, TemporalMomentum],
			aux = momExpand[aux,TemporalMomentum,optMomentum];
			If[	!FreeQ2[aux,{expandVec,linearizeVec,noexpand}],
				Message[MomentumExpand::failmsg, "Something went wrong during the expansion of 0th momentum components."];
				Abort[]
			]
		];

		res = aux /. hold2 -> Identity /. hold1 -> Identity;

		res

	];

momExpand[ex_, head_, optMomentum_]:=
	Block[{listOrig,listEval,repRule},
		listOrig = Cases2[ex,head];
		listEval = listOrig/. head -> momSplit[head, optMomentum] /. head -> expandVec[head] /. linearizeVec -> head /. noexpand -> Identity;
		repRule = Thread[Rule[listOrig,listEval]];
		(ex /. Dispatch[repRule])
	];

hold[]:=Sequence[];

momSplit[head_, All][a__]:=
	head[a];

momSplit[head_, moms_List][a_,dim___]:=
	Function[{x, y}, head[noexpand[x]+y, dim]] @@ FCSplit[a, moms];


(*
	Since the following functions cannot be normally assigned downvalues, they
	are safe for memoization.
*)


expandVec[head_][y_,dim___] :=	expandVec[head][y,dim] =
	Distribute[linearizeVec[Expand[y, head], hold[dim]]] /. hold->Identity;

linearizeVec[n_?NumberQ z_, dim___] :=
	n linearizeVec[z, dim];




FCPrint[1,"MomentumExpand.m loaded."];
End[]
