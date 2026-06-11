(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: TrickMandelstam															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:	Simplification of expressions involving Mandelstam variables

				Supports parallel evaluation [X]
*)

(* ------------------------------------------------------------------------ *)

TrickMandelstam::usage =
"TrickMandelstam[expr, {s, t, u, m1^2 + m2^2 + m3^2 + m4^2}] simplifies all
sums in expr so that one of the Mandelstam variables $s$, $t$ or $u$ is
eliminated by the relation $s + t + u = m_1^2 + m_2^2 + m_3^2 + m_4^2$ . The
trick is that the resulting sum has the most short number of terms.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`TrickMandelstam`Private`"]


Options[TrickMandelstam] = {
	FCParallelize			-> False,
	FCVerbose				-> False
};

TrickMandelstam[ex_, {}, OptionsPattern[]] :=
	ex;

TrickMandelstam[ex_,s_,t_,u_, mm_/;!OptionQ[mm], opts:OptionsPattern[]] :=
	TrickMandelstam[ex, {s,t,u,mm}, opts];

TrickMandelstam[ex_List,y__] :=
	Map[TrickMandelstam[#,y]&, ex];


TrickMandelstam[expr_List, {s,t,u,mm}/;!OptionQ[{s,t,u,mm}], opts:OptionsPattern[]] :=
	Block[{optVerbose, res, time},

		If [OptionValue[FCVerbose]===False,
			optVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				optVerbose=OptionValue[FCVerbose]
			];
		];

		time=AbsoluteTime[];

		If[	$ParallelizeFeynCalc && OptionValue[FCParallelize],
			FCPrint[1,"TrickMandelstam: Applying TrickMandelstam in parallel.", FCDoControl->optVerbose];
			res = ParallelMap[TrickMandelstam[#, {s,t,u,mm}, FilterRules[{opts}, Except[FCParallelize | FCVerbose]]]&,expr,
			DistributedContexts -> None, Method->"ItemsPerEvaluation" -> Ceiling[N[Length[expr]/$KernelCount]/10]];
			FCPrint[1, "TrickMandelstam: Done applying TrickMandelstam in parallel, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose],

			FCPrint[1,"TrickMandelstam: Applying TrickMandelstam.", FCDoControl->optVerbose];
			res = Map[TrickMandelstam[#, {s,t,u,mm},FilterRules[{opts}, Except[FCParallelize | FCVerbose]]]&,expr];
			FCPrint[1, "TrickMandelstam: Done applying TrickMandelstam, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose]
		];

		res
	];

TrickMandelstam[expr_/;Head[expr]=!=List, {s_, t_, u_, mm_}/;!OptionQ[{s,t,u,mm}], OptionsPattern[]] :=
	Block[ {tmp, null},

		tmp = Factor2[expr];

		If[	FreeQ[tmp,Plus],
			Return[tmp]
		];

		Switch[Head[tmp],
			Times,
			tmp = Map[TrickMandelstam[#, {s,t,u,mm}]&, tmp],
			Power,
			tmp = TrickMandelstam[tmp[[1]], {s,t,u,mm}]^tmp[[2]],
			Plus,
			tmp = trickPlus[tmp, {s,t,u,mm}],
			_,
			tmp = trickPlus[tmp+null, {s,t,u,mm}]/.null->0
		];

		If[ LeafCount[tmp]<2000,
			tmp = Cancel[tmp]
		];

		Factor2[tmp]
	];

trickPlus[x_Plus,args_List] :=
	Block[{	tricktemp, merk, nx = x, plusch, plusch0},

		plusch0[z__] :=
			Plus[z] /; !FreeQ[{z},plusch0];

		(* This is for arguments of D0, etc. ... *)
		plusch[z__] :=
			drickstu[Plus[z],args]/; (Length[{z}]===(Length[Plus@@args]-1))&& FreeQ[{z},Plus];

		plusch[z__] :=
			(Factor2 /@ Collect2[ Plus[z], Take[args, 3] ] ) /; Length[{z}]=!=(Length[Plus@@args]-1);

		tricktemp = drickstu[nx,args];

		(tricktemp/.Plus->plusch0/.plusch0->plusch /. plusch->Plus)

	]/;(Length[args]===4 || args==={});



drickstu[exp_,{}] :=
	exp;

drickstu[exp_,{s_,t_,u_,_}] :=
	exp /; !FreeQ[{s,t,u},Plus];

drickstu[x_Plus, {s_,t_,u_,m_}] :=
	Block[{	result, tristemp, eM, otherVars, null, trickargs, takeShortest, nsortQ},

		takeShortest[xx_Plus,es_,te_,uu_,ma_] :=
			(Sort[{xx, Expand[ xx/.te->(ma-es-uu) ], Expand[xx/.uu->(ma-te-es)]},nsortQ]//First );

		takeShortest[a_*b_,c__] :=
			takeShortest[a,c] takeShortest[b,c];

		takeShortest[a_^n_,c__] :=
			takeShortest[a,c]^n;

		takeShortest[xx_,__] :=
			xx/;(Head[xx]=!=Plus) && (Head[xx]=!=Times) && (Head[xx]=!=Power);

		nsortQ[xx_,y_] :=
			If[	TrueQ[NTerms[xx]<=NTerms[y]],
				True,
				False
			];

		(* Check if an overall factorization is possible *)
		tristemp = Factor2[ x/.s->(m-t-u) ];
		If[ Head[tristemp]=!=Plus,
			(*Factorization found, returning back to the main function*)
			result = TrickMandelstam[tristemp,{s,t,u,m}],

			(*No factorization, need to try more tricks*)

			(*Check if there are other variables than the Mandelstam ones *)
			otherVars = Complement[Variables[tristemp], Variables[s+t+u+m]];

			If[ otherVars =!= {},
				(* Yes, so exploit the fact that simplifications cannot occur outside certain coefficients *)
				result = Factor2/@ (Collect2[eM tristemp, Append[otherVars,eM]]);
				result = Map[takeShortest[#,s,t,u,m]&,result+null]/.null->0/.eM->1;

				(* No, so just try to make each factor as short as possible *)
				result = Map[Factor2, result],
				result = takeShortest[tristemp, s,t,u,m]
			]
		];
		result
	];

FCPrint[1,"TrickMandelstam.m loaded."];
End[]
