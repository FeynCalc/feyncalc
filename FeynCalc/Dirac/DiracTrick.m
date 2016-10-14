(* ::Package:: *)



(* :Title: DiracTrick                                                       *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Contraction and simplification rules for Dirac matrices                                        *)

(* ------------------------------------------------------------------------ *)

DiracTrick::usage =
"DiracTrick[exp] contracts gamma matrices with each other and \
performs several simplifications (no expansion, use DiracSimplify for this).";

DiracTrick::failmsg =
"Error! DiracTrick has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"


(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DiracTrick`Private`"]

diTrVerbose::usage="";
diracTraceSimplify::usage="";
insideDiracTrace::usage="";
diga::usage="";

Options[DiracTrick] = {
	DiracGammaCombine -> False,
	Expanding -> False,
	FCDiracIsolate -> True,
	FCI -> False,
	FCVerbose -> False,
	InsideDiracTrace -> False
};

(* TODO: Bad syntax that one should get rid off*)
DiracTrick[] = 1;

DiracTrick[y__ /; FreeQ[{y}, Rule, 1],z_/;Head[z]=!=Rule] :=
	DiracTrick[DOT[y,z],FCI->True];

DiracTrick[expr_,OptionsPattern[]] :=
	Block[{	res, tmp, ex, null1, null2, holdDOT, freePart, dsPart, diracObjects,
			diracObjectsEval, repRule, time},

		(*	Algorithm of DiracTrick:

			x)	Isolate all Dirac objects and handle them separately. Of course there is an extra option
				to skip this if the input is already a single object (e.g. when DiracTrick is called by
				DiracTrace or DiracSimplify) Then apply the evaluating function to each of the objects,
				create replacement rules (standard)	and substitute the results back.

			x) The inner working of the evaluating function:
				xx)		Check the dimension of the chain, could be purely 4-dim, purely D-dim or mixed (BMHV).
						Also check that BMHV was indeed activated if mixed dimensions appear

				xx)		Then we
							1) 	Handle g^5 if it is present
							2) 	Ensure that the expression is of the form .... g^5 or contains no g^5 at all,
								unless there are unknown non-commutative objects inside the chain or we use BMHV
							3)	Handle everything apart from g^5.
							4)	If we use BMHV, handle g^5 again, this time using anticommuting rules.
							5)	Notice that we use different functions for 4-dim, D-dim and mixed expressions
								to save time and ensure that e.g. no rules for mixed expressions are unsuccesfully
								applied to a purely 4-dim chain

				xx)		The handling of g^5 of course depends a lot on dimensions, the scheme and whether we are
						inside a trace or not.

						xxx) If we are purely 4-dim, then we do all the g^5 related simplifications in this step.
						xxx) If we are purely D-dim, not inside trace, and use NDR, then g^5 is anticommuted
						xxx) If we are purely D-dim, not inside trace, and use Larin or BMHV, then g^5 is left alone
						xxx) If we are purely D-dim, inside trace and use Larin, then g^5 is anticommuted and cyclicity is used
						xxx) If we are purely D-dim, inside trace and use BMHV, then only cyclicity is used

						xxx) If we are mixed (always BMHV), not inside trace, then g^5 is left alone
						xxx) If we are mixed (always BMHV), inside trace, then only cyclicity is used
						xxx) Notice that in all cases we use some generic properties of g^5, namely that
						(g^5)^2 = 1, g^6 g^7 = 0 etc.


		*)

		If [OptionValue[FCVerbose]===False,
			diTrVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				diTrVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "DiracTrick. Entering.", FCDoControl->diTrVerbose];
		FCPrint[3, "DiracTrick: Entering with ", expr, FCDoControl->diTrVerbose];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	OptionValue[InsideDiracTrace],
			insideDiracTrace = True,
			insideDiracTrace = False
		];

		If[ FreeQ2[ex,DiracHeadsList],
			Return[ex]
		];



		If[	OptionValue[FCDiracIsolate],
			(*	This is the standard mode for calling DiracTrick	*)

			(* 	First of all we need to extract all the Dirac structures in the input. *)
			ex = FCDiracIsolate[ex,FCI->True,Head->dsHead, DotSimplify->True, DiracGammaCombine->OptionValue[DiracGammaCombine],Lorentz->True];

			{freePart,dsPart} = FCSplit[ex,{dsHead}];
			FCPrint[3,"DiracTrick: dsPart: ",dsPart , FCDoControl->diTrVerbose];
			FCPrint[3,"DiracTrick: freePart: ",freePart , FCDoControl->diTrVerbose];

			diracObjects = Cases[dsPart+null1+null2, dsHead[_], Infinity]//Union;
			FCPrint[3,"DiracTrick: diracObjects: ",diracObjects , FCDoControl->diTrVerbose];

			time=AbsoluteTime[];
			FCPrint[1, "DiracTrick: Applying diracTrickEval", FCDoControl->diTrVerbose];
			diracObjectsEval = Map[(diracTrickEvalFast[#]/. diracTrickEvalFast->diracTrickEval)&, (diracObjects/.dsHead->Identity)];
			FCPrint[1,"DiracTrace: diracTrickEval done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->diTrVerbose];

			If[ !FreeQ2[diracObjectsEval,{diracTrickEvalFast,diracTrickEval}],
				Message[DiracTrick::failmsg,"Evaluation of isolated objects failed."];
				Abort[]
			];

			repRule = MapThread[Rule[#1,#2]&,{diracObjects,diracObjectsEval}];
			FCPrint[3,"DiracTrick: repRule: ",repRule , FCDoControl->diTrVerbose];
			res = freePart + ( dsPart/.repRule),

			(* 	This is a fast mode for input that is already isolated, e.g. for calling DiracTrick/@exprList
				from internal functions	*)
			res = diracTrickEvalFast[ex] /. diracTrickEvalFast->diracTrickEval
		];


		If[	OptionValue[Expanding],
			time=AbsoluteTime[];
			FCPrint[1, "DiracTrick: Expanding the result.", FCDoControl->diTrVerbose];
			res = Expand[res];
			FCPrint[1,"DiracTrace: Expanding done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->diTrVerbose];
			FCPrint[3, "DiracTrick: After expanding: ", res, FCDoControl->diTrVerbose]
		];

		FCPrint[1, "DiracTrick. Leaving.", FCDoControl->diTrVerbose];
		FCPrint[3, "DiracTrick: Leaving with ", res, FCDoControl->diTrVerbose];

		res
	];

diracTrickEvalFast[ex:DiracGamma[__]]:=
	ex/; !insideDiracTrace

diracTrickEvalFast[DOT[x__DiracGamma]]:=
	0/; FreeQ2[{x},{DiracGamma[5],DiracGamma[6],DiracGamma[7]}] && OddQ[Length[{x}]] && insideDiracTrace;

diracTrickEvalFast[DOT[x___DiracGamma,DiracGamma[5],y___DiracGamma]]:=
	0/; Length[{x,y}]<4 && FreeQ2[{x,y},{DiracGamma[5],DiracGamma[6],DiracGamma[7]}] && insideDiracTrace;

diracTrickEvalFast[DOT[x___DiracGamma,DiracGamma[6|7],y___DiracGamma]]:=
	0/; OddQ[Length[{x,y}]] && FreeQ2[{x,y},{DiracGamma[5],DiracGamma[6],DiracGamma[7]}] && insideDiracTrace;

diracTrickEvalFast[DiracGamma[5]]:=
	0/; insideDiracTrace;

diracTrickEvalFast[DiracGamma[6|7]]:=
	1/2/; insideDiracTrace;


diracTrickEval[ex_/;Head[ex]=!=DiracGamma]:=
	Which[
		(*	NDR, inside Trace	*)
		!$BreitMaison && !$Larin && insideDiracTrace,
		diracTrickEvalCachedNDRInsideTrace[ex],
		(*	Larin, inside Trace	*)
		!$BreitMaison && $Larin && insideDiracTrace,
		diracTrickEvalCachedLarinInsideTrace[ex],
		(* BMHV, inside Trace *)
		$BreitMaison && !$Larin && insideDiracTrace,
		diracTrickEvalCachedBMHVInsideTrace[ex],
		(*	NDR	*)
		!$BreitMaison && !$Larin,
		diracTrickEvalCachedNDR[ex],
		(*	Larin	*)
		!$BreitMaison && $Larin,
		diracTrickEvalCachedLarin[ex],
		(* BMHV *)
		$BreitMaison && !$Larin,
		diracTrickEvalCachedBMHV[ex],
		(* Else *)
		True,
		Message[DiracTrick::failmsg,"Incorrect combination of dimensions and g^5 scheme!"];
		Abort[]
	];

diracTrickEvalCachedNDRInsideTrace[x_] :=
	diracTrickEvalInternal[x];

diracTrickEvalCachedLarinInsideTrace[x_] :=
	diracTrickEvalInternal[x];

diracTrickEvalCachedBMHVInsideTrace[x_] :=
	diracTrickEvalInternal[x];

diracTrickEvalCachedNDR[x_] :=
	diracTrickEvalInternal[x];

diracTrickEvalCachedLarin[x_] :=
	diracTrickEvalInternal[x];

diracTrickEvalCachedBMHV[x_] :=
	diracTrickEvalInternal[x];

(*
diracTrickEvalCachedNDRInsideTrace[x_] :=
	MemSet[diracTrickEvalCachedNDRInsideTrace[x], diracTrickEvalInternal[x]];

diracTrickEvalCachedLarinInsideTrace[x_] :=
	MemSet[diracTrickEvalCachedLarinInsideTrace[x], diracTrickEvalInternal[x]];

diracTrickEvalCachedBMHVInsideTrace[x_] :=
	MemSet[diracTrickEvalCachedBMHVInsideTrace[x], diracTrickEvalInternal[x]];

diracTrickEvalCachedNDR[x_] :=
	MemSet[diracTrickEvalCachedNDR[x], diracTrickEvalInternal[x]];

diracTrickEvalCachedLarin[x_] :=
	MemSet[diracTrickEvalCachedLarin[x], diracTrickEvalInternal[x]];

diracTrickEvalCachedBMHV[x_] :=
	MemSet[diracTrickEvalCachedBMHV[x], diracTrickEvalInternal[x]];
*)

diracTrickEvalInternal[ex_/;Head[ex]=!=DiracGamma]:=
	Block[{res=ex, holdDOT, time, dim, gamma5Present,noncommPresent,null1,null2},

		FCPrint[1, "DiracTrick: diracTrickEval: Entering.", FCDoControl->diTrVerbose];
		FCPrint[3, "DiracTrick: diracTrickEval: Entering with", ex , FCDoControl->diTrVerbose];

		gamma5Present = !FreeQ2[ex,{DiracGamma[5],DiracGamma[6],DiracGamma[7]}];
		noncommPresent = !NonCommFreeQ[ex/.DiracGamma->diga];
		dim = FCGetDimensions[ex/.DiracGamma[5|6|7]:>diga];

		FCPrint[3, "DiracTrick: diracTrickEval: g^5 present:", gamma5Present, FCDoControl->diTrVerbose];
		FCPrint[3, "DiracTrick: diracTrickEval: unknown non-commutative objects present:", noncommPresent, FCDoControl->diTrVerbose];

		res = res/. DOT -> holdDOT;

		If[	gamma5Present,
			res = res /. holdDOT -> commonGamma5Properties /. commonGamma5Properties -> holdDOT;
			gamma5Present = !FreeQ2[res,{DiracGamma[5],DiracGamma[6],DiracGamma[7]}];
		];

		If[ FreeQ2[res,DiracHeadsList],
			Return[res]
		];

		If[	insideDiracTrace,
			time=AbsoluteTime[];
			FCPrint[1, "DiracTrick: diracTrickEval: Applying diracTraceSimplify ", FCDoControl->diTrVerbose];
			res = diracTraceSimplify[res] /. DOT -> holdDOT /. diracTraceSimplify[holdDOT[x__]] :> diracTraceSimplify[x]/.
			diracTraceSimplify -> commonGamma5Properties /. commonGamma5Properties -> holdDOT;
			FCPrint[1,"DiracTrace: diracTrickEval: diracTraceSimplify done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->diTrVerbose];
			FCPrint[3, "DiracTrick: diracTrickEval: After diracTraceSimplify ", res, FCDoControl->diTrVerbose],
			res = res /. DOT -> holdDOT;
		];

		If[	!FreeQ[res,Pair],
			res = Expand2[res,Pair]/. Pair->PairContract /. PairContract->Pair;
		];

		If[ FreeQ2[res,DiracHeadsList],
			Return[res]
		];

		dim = FCGetDimensions[res/.DiracGamma[5|6|7]:>diga];
		noncommPresent = !NonCommFreeQ[res/.DiracGamma->diga];

		FCPrint[3, "DiracTrick: diracTrickEval: Dimensions:", dim, FCDoControl->diTrVerbose];

		If[	gamma5Present,

			time=AbsoluteTime[];
			FCPrint[1, "DiracTrick: diracTrickEval: Applying chiralTrick ", FCDoControl->diTrVerbose];
			Which[
					(* Purely 4-dimensional -> use anticommuting g^5 *)
					dim==={4},
						FCPrint[1, "DiracTrick: diracTrickEval: Purely 4-dim.", FCDoControl->diTrVerbose];
						res = res /. holdDOT -> chiralTrickAnticommuting4Dim;
						res = FixedPoint[(# /. chiralTrickAnticommuting4Dim -> commonGamma5Properties /.
							commonGamma5Properties -> chiralTrickAnticommuting4Dim)&,res];
						res = res /. chiralTrickAnticommuting4Dim -> holdDOT,
					(* Purely D-dimensional and NDR -> use anticommuting g^5 *)
					MatchQ[dim,{_Symbol}] && !$BreitMaison && !$Larin,
						FCPrint[1, "DiracTrick: diracTrickEval: Purely D-dim, NDR.", FCDoControl->diTrVerbose];
						res = res /. holdDOT -> chiralTrickAnticommutingDDim;
						res = FixedPoint[(# /. chiralTrickAnticommutingDDim -> commonGamma5Properties /.
							commonGamma5Properties -> chiralTrickAnticommutingDDim)&,res];
						res = res /. chiralTrickAnticommutingDDim -> holdDOT,
					(* Purely D-dimensional and Larin or BMHV -> don't move anything around *)
					MatchQ[dim,{_Symbol}] && (!$BreitMaison && $Larin) || ($BreitMaison && !$Larin),
						FCPrint[1, "DiracTrick: diracTrickEval: Purely D-dim and Larin or BMHV.", FCDoControl->diTrVerbose];
						Null,
					(* Mixed and BMHV -> don't move g^5 around *)
					MatchQ[dim, {_Symbol - 4} | {4, _Symbol} | {4, _Symbol-4} | {_Symbol-4, _Symbol} | {4, _Symbol-4, _Symbol}] && $BreitMaison && !$Larin,
						FCPrint[1, "DiracTrick: diracTrickEval: Mixed and BMHV.", FCDoControl->diTrVerbose],
					(* Anything else is most likely an error *)
					True,
						Message[DiracTrick::failmsg,"Incorrect combination of dimensions and g^5 scheme!"];
						Abort[]
			];

			FCPrint[1,"DiracTrace: diracTrickEval: chiralTrick done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->diTrVerbose];
			FCPrint[3, "DiracTrick: diracTrickEval: After chiralTrick ", res, FCDoControl->diTrVerbose];
			gamma5Present = !FreeQ2[res,{DiracGamma[5],DiracGamma[6],DiracGamma[7]}]
		];

		(* Here we do all the simplifications not realted to g^5	*)
		Which[
			(* Purely 4-dimensional *)
			dim==={4},
				FCPrint[1, "DiracTrick: diracTrickEval: Purely 4-dim.", FCDoControl->diTrVerbose];
				FCPrint[1, "DiracTrick: diracTrickEval: Applying diracology4Dim.", FCDoControl->diTrVerbose];
				res = res /. holdDOT -> diracology4Dim /. diracology4Dim -> holdDOT;
				FCPrint[3, "DiracTrick: diracTrickEval: After diracology4Dim: ", res, FCDoControl->diTrVerbose],
			(* Purely D-dimensional *)
			MatchQ[dim,{_Symbol}],
				FCPrint[1, "DiracTrick: diracTrickEval: Purely D-dim.", FCDoControl->diTrVerbose];
				res = res /. holdDOT -> diracologyDDim;
				res = FixedPoint[(# /. diracologyDDim -> diracologyDDim2 /. diracologyDDim2 -> diracologyDDim)&,res];
				res = res /. diracologyDDim -> holdDOT,
			(* Purely D-4-dimensional and BMHV *)
			MatchQ[dim,{_Symbol - 4}] && $BreitMaison && !$Larin,
				FCPrint[1, "DiracTrick: diracTrickEval: Purely D-4-dim.", FCDoControl->diTrVerbose];
				res = res /. holdDOT -> diracologyDDim;
				res = FixedPoint[(# /. diracologyDDim -> diracologyDDim2 /. diracologyDDim2 -> diracologyDDim)&,res];
				res = res /. diracologyDDim -> holdDOT,
			(* Mixed and BMHV *)
			MatchQ[dim, {4, _Symbol} | {4, _Symbol-4} | {_Symbol-4, _Symbol} | {4, _Symbol-4, _Symbol}] && $BreitMaison && !$Larin,
				FCPrint[1, "DiracTrick: diracTrickEval: Mixed and BMHV.", FCDoControl->diTrVerbose];
				res = res /. holdDOT -> diracologyBMHV1;
				res = FixedPoint[(# /. diracologyBMHV1 -> diracologyBMHV2 /. diracologyBMHV2 -> diracologyBMHV1)&,res];
				res = res /. diracologyBMHV1 -> holdDOT,
			(* Anything else is most likely an error *)
			True,
				Message[DiracTrick::failmsg,"Incorrect combination of dimensions and g^5 scheme!"];
				Abort[]
		];

		If[ FreeQ2[res,DiracHeadsList],
			Return[res]
		];

		(*	For BMHV we need to handle g^5 again here*)
		If[	gamma5Present && $BreitMaison && !$Larin,
			res = res /. holdDOT -> gamma5MoveBMHV;
						res = FixedPoint[(# /. gamma5MoveBMHV -> commonGamma5Properties /.
							commonGamma5Properties -> chiralTrickAnticommuting4Dim /. chiralTrickAnticommuting4Dim -> gamma5MoveBMHV)&,res];
			FCPrint[1, "DiracTrick: diracTrickEval: Additional simplifications of g^5 in BMHV.", FCDoControl->diTrVerbose];
			res = res /. gamma5MoveBMHV -> diracologyBMHV1;
			res = FixedPoint[(# /. diracologyBMHV1 -> diracologyBMHV2 /. diracologyBMHV2 -> diracologyBMHV1)&,res];
			res = res /. diracologyBMHV1 -> holdDOT
		];

		If[ FreeQ2[res,DiracHeadsList],
			Return[res]
		];

		res = res /. holdDOT -> DOT;

		If[	insideDiracTrace && res=!=0,
			time=AbsoluteTime[];
			FCPrint[1, "DiracTrick: diracTrickEval: Applying diracTraceSimplify again ", FCDoControl->diTrVerbose];
			res = ReplaceAll[diracTraceSimplify/@((res/. DOT -> holdDOT)+null1+null2),{diracTraceSimplify[null1|null2]->0,
				diracTraceSimplify[c_ d_holdDOT ]/; NonCommFreeQ[c]:> c diracTraceSimplify[d]}];
			res = res /. diracTraceSimplify[holdDOT[x__]] :> diracTraceSimplify[x]/. diracTraceSimplify ->DOT /. holdDOT->DOT;
			FCPrint[1,"DiracTrace: diracTrickEval: diracTraceSimplify done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->diTrVerbose];
			FCPrint[3, "DiracTrick: diracTrickEval: After diracTraceSimplify ", res, FCDoControl->diTrVerbose]
		];

		FCPrint[1, "DiracTrick: diracTrickEval: Leaving.", FCDoControl->diTrVerbose];
		FCPrint[3, "DiracTrick: diracTrickEval: Leaving with ", res, FCDoControl->diTrVerbose];
		res
	];


(* ------------------------------------------------------------------------ *)

diracology4Dim[]:=
	1;

(*	g^mu g^mu	*)
diracology4Dim[b___,DiracGamma[l_LorentzIndex], DiracGamma[l_LorentzIndex], d___] :=
	4 diracology4Dim[ b,d ];

(*	g^mu g^nu g_mu	*)
diracology4Dim[b___ , DiracGamma[c_LorentzIndex],
			DiracGamma[(x: LorentzIndex | ExplicitLorentzIndex | Momentum)[y_]],
			DiracGamma[c_LorentzIndex], d___] :=
	- 2 diracology4Dim[b,DiracGamma[x[y]], d];

(*	g^mu g^nu g^rho g_mu	*)
diracology4Dim[b___ , DiracGamma[c_LorentzIndex],
			DiracGamma[(x1: LorentzIndex | ExplicitLorentzIndex | Momentum)[y1_]],
			DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_]],
			DiracGamma[c_LorentzIndex], d___] :=
	4 FCUseCache[FCFastContract,{Pair[x1[y1],x2[y2]] diracology4Dim[b, d]},{}];

(*	g^mu g^nu g^rho g^sigma g_mu	*)
diracology4Dim[b___ , DiracGamma[c_LorentzIndex],
			(dg1: DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]]),
			(dg2: DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]]),
			(dg3: DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]]), DiracGamma[c_LorentzIndex], d___] :=
	- 2 diracology4Dim[b, dg3, dg2, dg1, d];

diracology4Dim[b___ , DiracGamma[c_LorentzIndex],
			(dg1: DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]]),
			(dg2: DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]]),
			(dg3: DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]]),
			(dg4: DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]]),
			DiracGamma[c_LorentzIndex], d___] :=
	2 diracology4Dim[b, dg3, dg2, dg1, dg4, d] + 2 diracology4Dim[b, dg4, dg1, dg2, dg3, d];

(*	g^mu g^nu_1 ... g^nu_i g_mu  -> -2 g^nu_i ... g^nu_1, where i is odd	*)
diracology4Dim[ b___,  DiracGamma[c_LorentzIndex],
		ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]].., DiracGamma[c_LorentzIndex], f___ ] :=
	-2 diracology4Dim @@ Join[ {b},Reverse[{ch}],{f} ] /; OddQ[Length[{ch}]] && Length[{ch}]>3;

(*	g^mu g^nu_1 ... g^nu_i g_mu  -> 2 g^nu_i-1 ... g^nu_1 g^nu_i + 2 g^nu_i g^nu_1 ... g^nu_i-1, where i is even	*)
diracology4Dim[ b___,  DiracGamma[c_LorentzIndex],
		ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]]..,
		end : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]],
		DiracGamma[c_LorentzIndex], f___ ] :=
	(2 diracology4Dim @@ Join[ {b},Reverse[{ch}],{end}, {f}] + 2 diracology4Dim[ b,end,ch,f ])/; OddQ[Length[{ch}]]  && Length[{ch}]>4;

(*	Slash(p).Slash(p)	*)
diracology4Dim[b___,DiracGamma[c_Momentum], DiracGamma[c_Momentum], d___ ] :=
	FCUseCache[ExpandScalarProduct,{Pair[c,c]},{}] diracology4Dim[b,d];




(*	Slash(p) g^nu Slash(p)	*)
diracology4Dim[b___ , DiracGamma[c_Momentum], DiracGamma[(x: LorentzIndex | ExplicitLorentzIndex | Momentum)[y_]],
		DiracGamma[c_Momentum], d___] :=
	- FCUseCache[ExpandScalarProduct,{Pair[c,c]},{}] diracology4Dim[b,DiracGamma[x[y]], d] + 2 FCUseCache[FCFastContract,{Pair[c,x[y]] diracology4Dim[b, DiracGamma[c], d]},{}];

(* Slash(p) g^nu_1 ... g^nu_n Slash(p), purely 4-dim; Eq 2.10 of R. Mertig, M. Boehm, A. Denner. Comp. Phys. Commun., 64 (1991) *)
diracology4Dim[b___, DiracGamma[c_Momentum],ch:DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]]..,
			DiracGamma[c_Momentum],f___ ] :=
	Block[ {iVar, len = Length[{ch}]},
		(-1)^len FCUseCache[ExpandScalarProduct,{Pair[c,c]},{}] diracology4Dim[b,ch,f]
		+ 2 Sum[(-1)^(iVar+1)FCUseCache[FCFastContract,{Pair[c,{ch}[[iVar,1]]] diracology4Dim@@Join[{b},
			Drop[{ch},{iVar, iVar}],{DiracGamma[c],f}]},{}],{iVar, 1,len}]
	]/; (Length[{ch}]>0);

(* ------------------------------------------------------------------------ *)


diracologyDDim[]:=
	1;

(*	g^mu g^mu	*)
diracologyDDim[b___,DiracGamma[l_LorentzIndex, dim_], DiracGamma[l_LorentzIndex, dim_], d___] :=
	dim diracologyDDim[ b,d ];

(*	g^mu g^nu g_mu	*)
diracologyDDim[b___ , DiracGamma[c_LorentzIndex, dim_],
			DiracGamma[(x: LorentzIndex | ExplicitLorentzIndex | Momentum)[y_, dim_], dim_],
			DiracGamma[c_LorentzIndex, dim_], d___] :=
	(2 - dim) diracologyDDim[b,DiracGamma[x[y, dim], dim], d];

(*	g^mu g^nu g^rho g_mu *)
diracologyDDim[b___ , DiracGamma[c_LorentzIndex, dim_ ],
			DiracGamma[(x1: LorentzIndex | ExplicitLorentzIndex | Momentum)[y1_, dim_], dim_],
			DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim_], dim_],
			DiracGamma[c_LorentzIndex, dim_], d___] :=
	(dim - 4) diracologyDDim[b,DiracGamma[x1[y1, dim], dim], DiracGamma[x2[y2, dim], dim], d] +
	4 FCUseCache[FCFastContract,{Pair[x1[y1,dim],x2[y2,dim]] diracologyDDim[b, d]},{}];

(*	g^mu g^nu g^rho g^sigma g_mu *)
diracologyDDim[b___ , DiracGamma[c_LorentzIndex, dim_],
			DiracGamma[(x1: LorentzIndex | ExplicitLorentzIndex | Momentum)[y1_, dim_], dim_],
			DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim_], dim_],
			DiracGamma[(x3: LorentzIndex | ExplicitLorentzIndex | Momentum)[y3_, dim_], dim_],
			DiracGamma[c_LorentzIndex, dim_], d___] :=
	-(dim - 4) diracologyDDim[b, DiracGamma[x1[y1, dim], dim], DiracGamma[x2[y2, dim], dim], DiracGamma[x3[y3, dim], dim], d] -
	2 diracologyDDim[b, DiracGamma[x3[y3, dim], dim], DiracGamma[x2[y2, dim], dim], DiracGamma[x1[y1, dim], dim], d];

diracologyDDim[b___ , DiracGamma[c_LorentzIndex, dim_],
			DiracGamma[(x1: LorentzIndex | ExplicitLorentzIndex | Momentum)[y1_, dim_], dim_],
			DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim_], dim_],
			DiracGamma[(x3: LorentzIndex | ExplicitLorentzIndex | Momentum)[y3_, dim_], dim_],
			DiracGamma[(x4: LorentzIndex | ExplicitLorentzIndex | Momentum)[y4_, dim_], dim_],
			DiracGamma[c_LorentzIndex, dim_], d___] :=
	(dim - 4) diracologyDDim[b, DiracGamma[x1[y1, dim], dim], DiracGamma[x2[y2, dim], dim], DiracGamma[x3[y3, dim], dim],
					DiracGamma[x4[y4, dim], dim], d] +
					2 diracologyDDim[b, DiracGamma[x3[y3, dim], dim], DiracGamma[x2[y2, dim], dim], DiracGamma[x1[y1, dim], dim],
							DiracGamma[x4[y4, dim], dim], d] +
				2 diracologyDDim[b, DiracGamma[x4[y4, dim], dim], DiracGamma[x1[y1, dim], dim], DiracGamma[x2[y2, dim], dim],
							DiracGamma[x3[y3, dim], dim], d];

(*	Simplification for g^mu g^nu_1 ... g^nu_n g_mu where all matrices are in D-dims;
	Eq 2.9 of R. Mertig, M. Boehm, A. Denner. Comp. Phys. Commun., 64 (1991)	*)
diracologyDDim2[ b___,DiracGamma[c_LorentzIndex,dim_],
	ch:DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim_],dim_]..,
	DiracGamma[c_LorentzIndex,dim_],f___ ] :=
	Block[ {iVar,jVar,len = Length[{ch}],dsTemp},
		FCUseCache[FCFastContract,{(-1)^len ( dim - 2 len ) dsTemp[b,ch,f] - 4 (-1)^len Sum[ (-1)^(jVar-iVar) *  Pair[{ch}[[iVar,1]],
			{ch}[[jVar,1]] ]*dsTemp@@Join[{b}, Delete[{ch}, {{iVar},{jVar}}], {f}],{iVar,1,len-1},{jVar,iVar+1,len}]},{}]/.
			dsTemp->diracologyDDim/. Pair[aa__]/;!FreeQ[{a},Momentum] :> FCUseCache[ExpandScalarProduct,{Pair[aa]},{}]
	] /; (Length[{ch}]>4);

(*	Slash(p) Slash(p)	*)
diracologyDDim[b___,DiracGamma[c_Momentum, dim_], DiracGamma[c_Momentum, dim_], d___] :=
	FCUseCache[ExpandScalarProduct,{Pair[c,c]},{}] diracologyDDim[b,d];

(*	Slash(p) g^nu Slash(p)	*)
diracologyDDim[b___ , DiracGamma[c_Momentum, dim_],
		DiracGamma[(x: LorentzIndex | ExplicitLorentzIndex | Momentum)[y_, dim_] ,dim_],
		DiracGamma[c_Momentum, dim_], d___] :=
	- FCUseCache[ExpandScalarProduct,{Pair[c,c]},{}] diracologyDDim[b,DiracGamma[x[y, dim], dim], d] +
	2 FCUseCache[FCFastContract,{Pair[c,x[y,dim]] diracologyDDim[b, DiracGamma[c, dim], d]},{}];


(* Slash(p) g^nu_1 ... g^nu_n Slash(p), purely D-dim; Eq 2.10 of R. Mertig, M. Boehm, A. Denner. Comp. Phys. Commun., 64 (1991) *)
diracologyDDim2[b___, DiracGamma[c_Momentum, dim_], ch:DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, _], dim_]..,
			DiracGamma[c_Momentum, dim_],f___] :=
	Block[ {iVar, len = Length[{ch}]},
		(-1)^len FCUseCache[ExpandScalarProduct,{Pair[c,c]},{}] diracologyDDim[b,ch,f]
		+ 2 Sum[(-1)^(iVar+1) FCUseCache[FCFastContract,{Pair[c,{ch}[[iVar,1]]] diracologyDDim@@Join[{b},Drop[{ch},{iVar, iVar}],{DiracGamma[c,dim],f}]},{}],
			{iVar, 1,len}]]/; (Length[{ch}]>0);

(* ------------------------------------------------------------------------ *)

diracTraceSimplify[] :=
	1;

diracTraceSimplify[___,0,___] :=
	0;

(* Trace cyclicity*)
diracTraceSimplify[b___,di_,c__] :=
	diracTraceSimplify[c,b, di]/; !FreeQ2[{di},{DiracGamma[5],DiracGamma[6],DiracGamma[7]}] &&
	FreeQ2[{b,c},{DiracGamma[5],DiracGamma[6],DiracGamma[7]}];

diracTraceSimplify[DiracGamma[(a:5|6|7)],b___,DiracGamma[(a:5|6|7)]] :=
	diracTraceSimplify[b];

diracTraceSimplify[DiracGamma[6],___,DiracGamma[7]] :=
	0;

diracTraceSimplify[DiracGamma[7],___,DiracGamma[6]] :=
	0;

diracTraceSimplify[DiracGamma[5],b___,DiracGamma[6]] :=
	diracTraceSimplify[b,DiracGamma[6]];

diracTraceSimplify[DiracGamma[6],b___,DiracGamma[5]] :=
	diracTraceSimplify[b,DiracGamma[6]];

diracTraceSimplify[DiracGamma[5],b___,DiracGamma[7]] :=
	-diracTraceSimplify[b,DiracGamma[7]];

diracTraceSimplify[DiracGamma[7],b___,DiracGamma[5]] :=
	-diracTraceSimplify[b,DiracGamma[7]];

diracTraceSimplify[ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_,___],___]..] :=
	0/; OddQ[Length[{ch}]];

diracTraceSimplify[ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_,___],___].., DiracGamma[5|6|7]] :=
	0/; OddQ[Length[{ch}]];

diracTraceSimplify[ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_,___],___].., DiracGamma[5]] :=
	0/; Length[{ch}] < 4;

diracTraceSimplify[DiracGamma[_[_,___],___], DiracGamma[_[_,___],___], DiracGamma[5]] :=
	0;

diracTraceSimplify[a:DiracGamma[_[_,___],___], b:DiracGamma[_[_,___],___], DiracGamma[6|7]] :=
	diracTraceSimplify[a,b]/2;

diracTraceSimplify[DiracGamma[5]]:=
	0;

(* ------------------------------------------------------------------------ *)

commonGamma5Properties[]:=
	1;

commonGamma5Properties[___,0,___]:=
	0;

commonGamma5Properties[b___,DiracGamma[5],DiracGamma[5],c___] :=
	commonGamma5Properties[ b,c ];

commonGamma5Properties[b___,DiracGamma[5],DiracGamma[6],c___] :=
	commonGamma5Properties[b,DiracGamma[6],c];

commonGamma5Properties[b___,DiracGamma[5],DiracGamma[7],c___] :=
	-commonGamma5Properties[b,DiracGamma[7],c];

commonGamma5Properties[b___,DiracGamma[6], DiracGamma[5], c___] :=
	commonGamma5Properties[b,DiracGamma[6],c];

commonGamma5Properties[b___,DiracGamma[7],DiracGamma[5],c___] :=
	-commonGamma5Properties[b, DiracGamma[7], c];

commonGamma5Properties[___,DiracGamma[6], DiracGamma[7], ___] :=
	0;

commonGamma5Properties[___,DiracGamma[7], DiracGamma[6], ___] :=
	0;

commonGamma5Properties[b___,DiracGamma[6],DiracGamma[6],c___] :=
	commonGamma5Properties[b, DiracGamma[6], c];

commonGamma5Properties[b___,DiracGamma[7],DiracGamma[7],c___] :=
	commonGamma5Properties[b, DiracGamma[7], c];

commonGamma5Properties[b___,DiracGamma[6]+DiracGamma[7],c___] :=
	commonGamma5Properties[b, c];

(* ------------------------------------------------------------------------ *)

chiralTrickAnticommuting4Dim[b___,DiracGamma[5], c:DiracGamma[_[_]].. ,d___] :=
	(-1)^Length[{c}] chiralTrickAnticommuting4Dim[ b,c,DiracGamma[5],d];

chiralTrickAnticommuting4Dim[b___,DiracGamma[(h:6|7)], c:DiracGamma[_[_]].. ,d___] :=
	chiralTrickAnticommuting4Dim[ b,c,DiracGamma[h],d]/; EvenQ[Length[{c}]];

chiralTrickAnticommuting4Dim[b___,DiracGamma[6], c:DiracGamma[_[_]].. ,d___] :=
	chiralTrickAnticommuting4Dim[ b,c,DiracGamma[7],d]/; OddQ[Length[{c}]];

chiralTrickAnticommuting4Dim[b___,DiracGamma[7], c:DiracGamma[_[_]].. ,d___] :=
	chiralTrickAnticommuting4Dim[ b,c,DiracGamma[6],d]/; OddQ[Length[{c}]];

chiralTrickAnticommuting4Dim[b___, DiracGamma[5], (dd1_. (dg:DiracGamma[_[_]]) + dd2_),d___ ] :=
	chiralTrickAnticommuting4Dim[b,(- dd1 dg + dd2), DiracGamma[5],d ]/; NonCommFreeQ[{dd1,dd2}];

chiralTrickAnticommuting4Dim[b___, (cc1_. DiracGamma[6] + cc2_. DiracGamma[7]),(dd1_. DiracGamma[6] + dd2_. DiracGamma[7]),d___ ] :=
	chiralTrickAnticommuting4Dim[b, (cc1 dd1 DiracGamma[6] + cc2 dd2 DiracGamma[7]),d ]/; NonCommFreeQ[{cc1,cc2,dd1,dd2}];

chiralTrickAnticommuting4Dim[b___, (cc1_ + cc2_. DiracGamma[6]),DiracGamma[x_[c_]],d___ ] :=
	chiralTrickAnticommuting4Dim[b, DiracGamma[x[c]], (cc1 + cc2 DiracGamma[7]),d ]/; NonCommFreeQ[{cc1,cc2}];

chiralTrickAnticommuting4Dim[b___, (cc1_ + cc2_. DiracGamma[7]),DiracGamma[x_[c_]],d___ ] :=
	chiralTrickAnticommuting4Dim[b, DiracGamma[x[c]], (cc1 + cc2 DiracGamma[6]),d ]/; NonCommFreeQ[{cc1,cc2}];

chiralTrickAnticommuting4Dim[b___, (cc1_. DiracGamma[6] + cc2_. DiracGamma[7]),DiracGamma[x_[c_]],d___ ] :=
	chiralTrickAnticommuting4Dim[b, DiracGamma[x[c]], (cc1 DiracGamma[7] + cc2 DiracGamma[6]),d ]/; NonCommFreeQ[{cc1,cc2}];

chiralTrickAnticommuting4Dim[b___, (cc1_ + cc2_. DiracGamma[6]),(dd1_. DiracGamma[x_[c_]] + dd2_),d___ ] :=
	chiralTrickAnticommuting4Dim[b,(dd1 DiracGamma[x[c]] + dd2), (cc1 + cc2 DiracGamma[7]),d ]/; NonCommFreeQ[{cc1,cc2,dd1,dd2}];

chiralTrickAnticommuting4Dim[b___, (cc1_ + cc2_. DiracGamma[7]),(dd1_. DiracGamma[x_[c_]] + dd2_),d___ ] :=
	chiralTrickAnticommuting4Dim[b,(dd1 DiracGamma[x[c]] + dd2), (cc1 + cc2 DiracGamma[6]),d ]/; NonCommFreeQ[{cc1,cc2,dd1,dd2}];

chiralTrickAnticommuting4Dim[b___, (cc1_. DiracGamma[6] + cc2_. DiracGamma[7]),(dd1_. DiracGamma[x_[c_]] + dd2_),d___ ] :=
	chiralTrickAnticommuting4Dim[b,(dd1 DiracGamma[x[c]] + dd2), (cc1 DiracGamma[7] + cc2 DiracGamma[6]),d]/; NonCommFreeQ[{cc1,cc2,dd1,dd2}];

chiralTrickAnticommuting4Dim[b___, DiracGamma[7],DiracGamma[_[_]] + mass_:0, xy:DiracGamma[_[_]].. , DiracGamma[6], c___] :=
	mass chiralTrickAnticommuting4Dim[b, xy, DiracGamma[6], c]/; OddQ[Length[{xy}]] && NonCommFreeQ[mass];

chiralTrickAnticommuting4Dim[b___,DiracGamma[6],DiracGamma[_[_]] + mass_:0, xy:DiracGamma[_[_]].. , DiracGamma[7], c___] :=
	mass chiralTrickAnticommuting4Dim[b, xy, DiracGamma[7], c]/; OddQ[Length[{xy}]] && NonCommFreeQ[mass];

chiralTrickAnticommuting4Dim[b___,DiracGamma[6],DiracGamma[_[_]] + mass_:0, xy:DiracGamma[_[_]].. , DiracGamma[6], c___] :=
	mass chiralTrickAnticommuting4Dim[b, xy, DiracGamma[6], c]/; EvenQ[Length[{xy}]] && NonCommFreeQ[mass];

chiralTrickAnticommuting4Dim[b___,DiracGamma[7],DiracGamma[_[_]] + mass_:0, xy:DiracGamma[_[_]].. , DiracGamma[7], c___] :=
	mass chiralTrickAnticommuting4Dim[b, xy, DiracGamma[7], c]/; EvenQ[Length[{xy}]] && NonCommFreeQ[mass];

chiralTrickAnticommuting4Dim[b___,DiracGamma[6],DiracGamma[_[_]] + mass_:0, DiracGamma[6], c___] :=
	mass chiralTrickAnticommuting4Dim[b, DiracGamma[6], c]/; NonCommFreeQ[mass];

chiralTrickAnticommuting4Dim[b___,DiracGamma[7],DiracGamma[_[_]] + mass_:0, DiracGamma[7], c___] :=
	mass chiralTrickAnticommuting4Dim[b, DiracGamma[7], c]/; NonCommFreeQ[mass];

chiralTrickAnticommuting4Dim[b___,DiracGamma[6],(dg:DiracGamma[_[_]]) + mass_:0, DiracGamma[7], c___] :=
	chiralTrickAnticommuting4Dim[b, dg, DiracGamma[7], c]/; NonCommFreeQ[mass];

chiralTrickAnticommuting4Dim[b___,DiracGamma[7],(dg:DiracGamma[_[_]]) + mass_:0, DiracGamma[6], c___] :=
	chiralTrickAnticommuting4Dim[b, dg, DiracGamma[6], c] /; NonCommFreeQ[mass];

chiralTrickAnticommuting4Dim[b___,DiracGamma[6],(dg:DiracGamma[_[_]]) + mass_:0, xy:DiracGamma[_[_]].. , DiracGamma[7], c___] :=
	chiralTrickAnticommuting4Dim[b, dg, xy, DiracGamma[7], c] /; EvenQ[Length[{xy}]] && NonCommFreeQ[mass];

chiralTrickAnticommuting4Dim[b___,DiracGamma[7],(dg:DiracGamma[_[_]]) + mass_:0, xy:DiracGamma[_[_]].. , DiracGamma[6], c___] :=
	chiralTrickAnticommuting4Dim[b, dg, xy, DiracGamma[6], c] /; EvenQ[Length[{xy}]] && NonCommFreeQ[mass];

chiralTrickAnticommuting4Dim[b___,DiracGamma[6],(dg:DiracGamma[_[_]]) + mass_:0, xy:DiracGamma[_[_]].. , DiracGamma[6], c___] :=
	chiralTrickAnticommuting4Dim[b, dg, xy, DiracGamma[6], c] /; OddQ[Length[{xy}]] && NonCommFreeQ[mass];

chiralTrickAnticommuting4Dim[b___,DiracGamma[7],(dg:DiracGamma[_[_]]) +  mass_:0,	xy:DiracGamma[_[_]].. , DiracGamma[7], c___] :=
	chiralTrickAnticommuting4Dim[b, dg, xy, DiracGamma[7], c] /; OddQ[Length[{xy}]] && NonCommFreeQ[mass];

chiralTrickAnticommuting4Dim[b___,DiracGamma[6],(dg:DiracGamma[_[_]]) + mass_, c___] :=
	(chiralTrickAnticommuting4Dim[b, dg, DiracGamma[7], c] +
	mass chiralTrickAnticommuting4Dim[b, DiracGamma[6], c])/; NonCommFreeQ[mass];

chiralTrickAnticommuting4Dim[b___,DiracGamma[7],(dg:DiracGamma[_[_]]) + mass_, c___] :=
	(chiralTrickAnticommuting4Dim[b, dg, DiracGamma[6], c] +
	mass chiralTrickAnticommuting4Dim[b, DiracGamma[7], c])/; NonCommFreeQ[mass];

(* ------------------------------------------------------------------------ *)

chiralTrickAnticommutingDDim[b___,DiracGamma[5],c:DiracGamma[_[_,_],_].. ,d___] :=
	(-1)^Length[{c}] chiralTrickAnticommutingDDim[ b,c,DiracGamma[5],d];

chiralTrickAnticommutingDDim[b___,DiracGamma[(h:6|7)],c:DiracGamma[_[_,_],_].. ,d___] :=
	chiralTrickAnticommutingDDim[ b,c,DiracGamma[h],d]/; EvenQ[Length[{c}]];

chiralTrickAnticommutingDDim[b___,DiracGamma[6],c:DiracGamma[_[_,_],_].. ,d___] :=
	chiralTrickAnticommutingDDim[ b,c,DiracGamma[7],d]/; OddQ[Length[{c}]];

chiralTrickAnticommutingDDim[b___,DiracGamma[7],c:DiracGamma[_[_,_],_].. ,d___] :=
	chiralTrickAnticommutingDDim[ b,c,DiracGamma[6],d]/; OddQ[Length[{c}]];

chiralTrickAnticommutingDDim[b___, DiracGamma[5],(dd1_. (dg:DiracGamma[_[_,_],_]) + dd2_),d___ ] :=
	chiralTrickAnticommutingDDim[b,(- dd1 dg + dd2), DiracGamma[5],d ]/; NonCommFreeQ[{dd1,dd2}];


chiralTrickAnticommutingDDim[b___, (cc1_. DiracGamma[6] + cc2_. DiracGamma[7]),(dd1_. DiracGamma[6] + dd2_. DiracGamma[7]),d___ ] :=
	chiralTrickAnticommutingDDim[b, (cc1 dd1 DiracGamma[6] + cc2 dd2 DiracGamma[7]),d ]/; NonCommFreeQ[{cc1,cc2,dd1,dd2}];

chiralTrickAnticommutingDDim[b___, (cc1_ + cc2_. DiracGamma[6]), (dg:DiracGamma[_[_,_],_]), d___ ] :=
	chiralTrickAnticommutingDDim[b, dg, (cc1 + cc2 DiracGamma[7]),d ]/; NonCommFreeQ[{cc1,cc2}];

chiralTrickAnticommutingDDim[b___, (cc1_ + cc2_. DiracGamma[7]),(dg:DiracGamma[_[_,_],_]), d___ ] :=
	chiralTrickAnticommutingDDim[b, dg, (cc1 + cc2 DiracGamma[6]),d ]/; NonCommFreeQ[{cc1,cc2}];

chiralTrickAnticommutingDDim[b___, (cc1_. DiracGamma[6] + cc2_. DiracGamma[7]), (dg:DiracGamma[_[_,_],_]), d___ ] :=
	chiralTrickAnticommutingDDim[b, dg, (cc1 DiracGamma[7] + cc2 DiracGamma[6]),d ]/; NonCommFreeQ[{cc1,cc2}];

chiralTrickAnticommutingDDim[b___, (cc1_ + cc2_. DiracGamma[6]),(dd1_. (dg:DiracGamma[_[_,_],_]) + dd2_),d___ ] :=
	chiralTrickAnticommutingDDim[b,(dd1 dg + dd2), (cc1 + cc2 DiracGamma[7]),d ]/; NonCommFreeQ[{cc1,cc2,dd1,dd2}];

chiralTrickAnticommutingDDim[b___, (cc1_ + cc2_. DiracGamma[7]),(dd1_. (dg:DiracGamma[_[_,_],_]) + dd2_),d___ ] :=
	chiralTrickAnticommutingDDim[b,(dd1 dg + dd2), (cc1 + cc2 DiracGamma[6]),d ]/; NonCommFreeQ[{cc1,cc2,dd1,dd2}];

chiralTrickAnticommutingDDim[b___, (cc1_. DiracGamma[6] + cc2_. DiracGamma[7]),(dd1_. (dg:DiracGamma[_[_,_],_]) + dd2_),d___ ] :=
	chiralTrickAnticommutingDDim[b,(dd1 dg + dd2), (cc1 DiracGamma[7] + cc2 DiracGamma[6]),d ]/; NonCommFreeQ[{cc1,cc2,dd1,dd2}];

chiralTrickAnticommutingDDim[b___, DiracGamma[7], DiracGamma[_[_,_], _] + mass_:0, xy:DiracGamma[_[_,_], _].. , DiracGamma[6], c___] :=
	mass chiralTrickAnticommutingDDim[b, xy, DiracGamma[6], c]/; OddQ[Length[{xy}]] && NonCommFreeQ[mass];

chiralTrickAnticommutingDDim[b___,DiracGamma[6],DiracGamma[_[_,_], _] + mass_:0, xy:DiracGamma[_[_,_], _].. , DiracGamma[7], c___] :=
	mass chiralTrickAnticommutingDDim[b, xy, DiracGamma[7], c]/; OddQ[Length[{xy}]] && NonCommFreeQ[mass];

chiralTrickAnticommutingDDim[b___,DiracGamma[6],DiracGamma[_[_,_], _] + mass_:0, xy:DiracGamma[_[_,_], _].. , DiracGamma[6], c___] :=
	mass chiralTrickAnticommutingDDim[b, xy, DiracGamma[6], c]/; EvenQ[Length[{xy}]] && NonCommFreeQ[mass];

chiralTrickAnticommutingDDim[b___,DiracGamma[7],DiracGamma[_[_,_], _] + mass_:0, xy:DiracGamma[_[_,_], _].. , DiracGamma[7], c___] :=
	mass chiralTrickAnticommutingDDim[b, xy, DiracGamma[7], c]/; EvenQ[Length[{xy}]] && NonCommFreeQ[mass];

chiralTrickAnticommutingDDim[b___,DiracGamma[6],DiracGamma[_[_,_], _] + mass_:0, DiracGamma[6], c___] :=
	mass chiralTrickAnticommutingDDim[b, DiracGamma[6], c]/; NonCommFreeQ[mass];

chiralTrickAnticommutingDDim[b___,DiracGamma[7],DiracGamma[_[_,_], _] + mass_:0, DiracGamma[7], c___] :=
	mass chiralTrickAnticommutingDDim[b, DiracGamma[7], c]/; NonCommFreeQ[mass];

chiralTrickAnticommutingDDim[b___,DiracGamma[6],(dg:DiracGamma[_[_,_], _]) + mass_:0, DiracGamma[7], c___] :=
	chiralTrickAnticommutingDDim[b, dg, DiracGamma[7], c]/; NonCommFreeQ[mass];

chiralTrickAnticommutingDDim[b___,DiracGamma[7],(dg:DiracGamma[_[_,_], _]) + mass_:0, DiracGamma[6], c___] :=
	chiralTrickAnticommutingDDim[b, dg, DiracGamma[6], c] /; NonCommFreeQ[mass];

chiralTrickAnticommutingDDim[b___,DiracGamma[6],(dg:DiracGamma[_[_,_], _]) + mass_:0, xy:DiracGamma[_[_,_], _].. , DiracGamma[7], c___] :=
	chiralTrickAnticommutingDDim[b, dg, xy, DiracGamma[7], c] /; EvenQ[Length[{xy}]] && NonCommFreeQ[mass];

chiralTrickAnticommutingDDim[b___,DiracGamma[7],(dg:DiracGamma[_[_,_], _]) + mass_:0, xy:DiracGamma[_[_,_], _].. , DiracGamma[6], c___] :=
	chiralTrickAnticommutingDDim[b, dg, xy, DiracGamma[6], c] /; EvenQ[Length[{xy}]] && NonCommFreeQ[mass];

chiralTrickAnticommutingDDim[b___,DiracGamma[6],(dg:DiracGamma[_[_,_], _]) + mass_:0, xy:DiracGamma[_[_,_], _].. , DiracGamma[6], c___] :=
	chiralTrickAnticommutingDDim[b, dg, xy, DiracGamma[6], c] /; OddQ[Length[{xy}]] && NonCommFreeQ[mass];

chiralTrickAnticommutingDDim[b___,DiracGamma[7],(dg:DiracGamma[_[_,_], _]) +  mass_:0,	xy:DiracGamma[_[_,_], _].. , DiracGamma[7], c___] :=
	chiralTrickAnticommutingDDim[b, dg, xy, DiracGamma[7], c] /; OddQ[Length[{xy}]] && NonCommFreeQ[mass];

chiralTrickAnticommutingDDim[b___,DiracGamma[6],(dg:DiracGamma[_[_,_], _]) + mass_, c___] :=
	(chiralTrickAnticommutingDDim[b, dg, DiracGamma[7], c] +
	mass chiralTrickAnticommutingDDim[b, DiracGamma[6], c])/; NonCommFreeQ[mass];

chiralTrickAnticommutingDDim[b___,DiracGamma[7],(dg:DiracGamma[_[_,_], _]) + mass_, c___] :=
	(chiralTrickAnticommutingDDim[b, dg, DiracGamma[6], c] +
	mass chiralTrickAnticommutingDDim[b, DiracGamma[7], c])/; NonCommFreeQ[mass];

(* ------------------------------------------------------------------------ *)


gamma5MoveBMHV[]:=
	1;

gamma5MoveBMHV[b___,DiracGamma[(h:5|6|7)],DiracGamma[x_[y__],d_Symbol -4] + mass_:0,f___] :=
	gamma5MoveBMHV[ b,mass + DiracGamma[x[y],d-4],DiracGamma[h],f ]/; NonCommFreeQ[mass];

gamma5MoveBMHV[b___,DiracGamma[5],DiracGamma[x_[y__],d_Symbol], f___] :=
	2 gamma5MoveBMHV[b,DiracGamma[x[y],d-4],DiracGamma[5],f] -
	gamma5MoveBMHV[b,DiracGamma[x[y],d],DiracGamma[5],f];

gamma5MoveBMHV[b___,DiracGamma[5],DiracGamma[x_[y__],d_Symbol] + mass_, f___] :=
	2 gamma5MoveBMHV[b,DiracGamma[x[y],d-4],DiracGamma[5],f] +
	gamma5MoveBMHV[b,(mass - DiracGamma[x[y],d]),DiracGamma[5],f]/; NonCommFreeQ[mass];

gamma5MoveBMHV[b___,DiracGamma[6],DiracGamma[x_[y__],d_Symbol] + mass_:0, f___] :=
	(gamma5MoveBMHV[b,DiracGamma[x[y],d-4],DiracGamma[5],f] +
	gamma5MoveBMHV[b,DiracGamma[x[y],d],DiracGamma[7],f] +
	mass gamma5MoveBMHV[b,DiracGamma[6],f])/; NonCommFreeQ[mass];

gamma5MoveBMHV[b___,DiracGamma[7],DiracGamma[x_[y__],d_Symbol] + mass_:0, f___] :=
	(- gamma5MoveBMHV[b,DiracGamma[x[y],d-4],DiracGamma[5],f] +
	gamma5MoveBMHV[b,DiracGamma[x[y],d],DiracGamma[6],f] +
	mass gamma5MoveBMHV[b,DiracGamma[7],f])/; NonCommFreeQ[mass];


(* ------------------------------------------------------------------------ *)

diracologyBMHV1[]:=
	1;

diracologyBMHV2[]:=
	1;

(* Simplification for g^mu ... g_mu where the first and the last
	matrix are in different dimensions                                         *)

(* D and 4  -> 4 *)
diracologyBMHV1[ b___, DiracGamma[LorentzIndex[c_, dimD_Symbol], dimD_Symbol], d:DiracGamma[__].. ,
	DiracGamma[LorentzIndex[c_]], f___ ] :=
	diracologyBMHV1[b, DiracGamma[LorentzIndex[c]], d, DiracGamma[LorentzIndex[c]], f];

(* 4 and D -> 4 *)
diracologyBMHV1[ b___, DiracGamma[LorentzIndex[c_]], d:DiracGamma[__].. ,
	DiracGamma[LorentzIndex[c_, dimD_Symbol], dimD_Symbol], f___ ] :=
	diracologyBMHV1[b, DiracGamma[LorentzIndex[c]], d, DiracGamma[LorentzIndex[c]], f];

(* D and D-4 -> D-4 *)
diracologyBMHV1[ b___, DiracGamma[LorentzIndex[c_, dimD_Symbol], dimD_Symbol], d:DiracGamma[__].. ,
	DiracGamma[LorentzIndex[c_, dimD_Symbol-4], dimD_Symbol-4], f___ ] :=
	diracologyBMHV1[b,DiracGamma[LorentzIndex[c, dimD-4], dimD-4], d,DiracGamma[LorentzIndex[c, dimD-4], dimD-4], f];

(* D-4 and D -> D-4 *)
diracologyBMHV1[ b___, DiracGamma[LorentzIndex[c_, dimD_Symbol-4], dimD_Symbol-4], d:DiracGamma[__].. ,
	DiracGamma[LorentzIndex[c_, dimD_Symbol],dimD_Symbol],f___ ] :=
	diracologyBMHV1[b,DiracGamma[LorentzIndex[c, dimD-4], dimD-4], d, DiracGamma[LorentzIndex[c, dimD-4], dimD-4],f];

(* 4 and D-4 -> 0 *)
diracologyBMHV1[ ___, DiracGamma[LorentzIndex[c_, dimD_Symbol-4], dimD_Symbol-4], DiracGamma[__].. ,
	DiracGamma[LorentzIndex[c_]], ___ ] :=
	0;

(* D-4 and 4 -> 0 *)
diracologyBMHV1[ ___, DiracGamma[LorentzIndex[c_]], DiracGamma[__].. ,
	DiracGamma[LorentzIndex[c_, dimD_Symbol-4], dimD_Symbol-4], ___ ] :=
	0;

(* Contractions of neighbouring Dirac matrices in D, 4 and D-4 dimensions     *)
(* ------------------------------------------------------------------------ *)

diracologyBMHV2[b___,DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_:  4],
		DiracGamma[LorentzIndex[c_, dim2_ : 4], dim2_: 4], d___] :=
	(PairContract[LorentzIndex[c, dim1],LorentzIndex[c, dim2]]/. PairContract->Pair) diracologyBMHV2[ b,d ];

(* Simplifications for g^mu g^nu_1 ... g^nu_n g_mu where g^mu is in 4 and
	g^nu_i are in D-4 dimensions or vice versa.                                *)
(* ------------------------------------------------------------------------ *)

(* 4, (... D-4 ... ), 4 *)
diracologyBMHV2[b___ , DiracGamma[LorentzIndex[c_]],
			ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim_Symbol-4] , dim_Symbol-4]..,
			DiracGamma[LorentzIndex[c_]], d___] :=
	4 (-1)^Length[{ch}] diracologyBMHV2[b,ch, d];

(* D-4, (... 4 ... ), D-4 *)
diracologyBMHV2[b___ , DiracGamma[LorentzIndex[c_, dim_Symbol-4], dim_Symbol-4],
			ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]]..,
			DiracGamma[LorentzIndex[c_, dim_Symbol-4], dim_Symbol-4], d___] :=
	(dim - 4) (-1)^Length[{ch}] diracologyBMHV2[b,ch, d];

(* Simplification for g^mu g^nu_1 ... g^nu_n g_mu where g^mu is in D and
	g^nu_i are in 4 dimensions. Applies for n>4, since for n<=4 we have
	explicit expressions in the code                                        *)
(* ------------------------------------------------------------------------ *)

diracologyBMHV2[b___ , DiracGamma[LorentzIndex[c_, dim_Symbol], dim_Symbol],
			ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]]..,
			DiracGamma[LorentzIndex[c_, dim_Symbol], dim_Symbol], d___] :=
	diracologyBMHV2[b,DiracGamma[LorentzIndex[c]], ch, DiracGamma[LorentzIndex[c]], d] +
	(dim - 4) (-1)^Length[{ch}] diracologyBMHV2[b,ch, d]/; Length[{ch}]>4;

(* Simplification for g^mu g^nu_1 ... g^nu_n g_mu where g^mu is in D and
	g^nu_i are in D-4 dimensions. Applies for n>4, since for n<=4 we have
	explicit expressions in the code                                        *)
(* ------------------------------------------------------------------------ *)

diracologyBMHV2[b___ , DiracGamma[LorentzIndex[c_, dim_Symbol], dim_Symbol],
			ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_,
			dim_Symbol-4],dim_Symbol-4]..,
			DiracGamma[LorentzIndex[c_, dim_Symbol], dim_Symbol], d___] :=
	diracologyBMHV2[b,DiracGamma[LorentzIndex[c,dim-4],dim-4], ch, DiracGamma[LorentzIndex[c,dim-4],dim-4], d] +
	4 (-1)^Length[{ch}] diracologyBMHV2[b,ch, d]/; Length[{ch}]>4;


(* Evaluation of g^mu g^nu g_mu for Dirac matrices in different
	dimensions                                                                *)
(* ------------------------------------------------------------------------ *)


(* D, D, D or 4, 4, 4 or D-4, D-4, D-4
	or D, D-4, D or D, 4, D *)
diracologyBMHV2[b___ , DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4],
			DiracGamma[(x: LorentzIndex | ExplicitLorentzIndex | Momentum)[y_, dim2_ : 4] ,dim2_ : 4],
			DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4], d___] :=
	(2 - dim1) diracologyBMHV2[b,DiracGamma[x[y, dim2], dim2], d]/;
					(dim1===dim2 || dim2 === dim1-4 || dim2 ===4) &&
					MatchQ[dim1, _Symbol | _Symbol-4 | 4 ] &&
					MatchQ[dim2, _Symbol | _Symbol-4 | 4 ];

(* D-4, D, D-4 or 4, D, 4 *)
diracologyBMHV2[b___ , DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4],
			DiracGamma[(x: LorentzIndex | ExplicitLorentzIndex | Momentum)[y_, dim2_Symbol], dim2_Symbol],
			DiracGamma[LorentzIndex[c_, dim1_ : 4],  dim1_ : 4], d___] :=
	- dim1 diracologyBMHV2[b,DiracGamma[x[y,dim2],dim2],d] +
	2 diracologyBMHV2[b,DiracGamma[x[y,dim1],dim1],d]/; (dim1 === dim2-4 || dim1 ===4);

(* Evaluation of g^mu g^nu g^rho g_mu for Dirac matrices in different
	dimensions                                                                *)
(* ------------------------------------------------------------------------ *)


(* D, D, D, D or 4, 4, 4, 4 or D-4, D-4, D-4, D-4
	or D, D-4, D-4, D or D, 4, 4, D *)
diracologyBMHV2[b___ , DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4],
			DiracGamma[(x1: LorentzIndex | ExplicitLorentzIndex | Momentum)[y1_, dim2_ : 4] ,dim2_ : 4],
			DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim2_ : 4] ,dim2_ : 4],
			DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4], d___] :=
	(dim1 - 4) diracologyBMHV2[b,DiracGamma[x1[y1, dim2], dim2],
					DiracGamma[x2[y2, dim2], dim2], d] +
					4 FCUseCache[FCFastContract,{Pair[x1[y1,dim1],x2[y2,dim2]] diracologyBMHV2[b, d]},{}]/;
					(dim1===dim2 || dim2 === dim1-4 || dim2 ===4) &&
					MatchQ[dim1, _Symbol | _Symbol-4 | 4 ] &&
					MatchQ[dim2, _Symbol | _Symbol-4 | 4 ];

(* D-4, D, D, D-4 or 4, D, D, 4 *)
diracologyBMHV2[b___ , DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4],
			DiracGamma[(x1: LorentzIndex | ExplicitLorentzIndex | Momentum)[y1_, dim2_Symbol], dim2_Symbol],
			DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim2_Symbol], dim2_Symbol],
			DiracGamma[LorentzIndex[c_, dim1_: 4], dim1_ : 4], d___] :=
	dim1 diracologyBMHV2[b,DiracGamma[x1[y1, dim2], dim2],DiracGamma[x2[y2, dim2], dim2], d] -
	2 diracologyBMHV2[b,DiracGamma[x1[y1, dim1], dim1],DiracGamma[x2[y2, dim2], dim2], d] +
	2 diracologyBMHV2[b,DiracGamma[x2[y2, dim1], dim1],DiracGamma[x1[y1, dim2], dim2], d]/;
	(dim1 === dim2-4 || dim1 ===4);


(* Evaluation of g^mu g^nu g^rho g^sigma g_mu for Dirac matrices in different
	dimensions                                                                *)
(* ------------------------------------------------------------------------ *)

(* D, D, D, D, D or 4, 4, 4, 4, 4 or D-4, D-4, D-4, D-4, D-4
	or D, D-4, D-4, D-4, D or D, 4, 4, 4, D *)
diracologyBMHV2[b___ , DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4],
			DiracGamma[(x1: LorentzIndex | ExplicitLorentzIndex | Momentum)[y1_, dim2_ : 4] , dim2_ : 4],
			DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim2_ : 4] , dim2_ : 4],
			DiracGamma[(x3: LorentzIndex | ExplicitLorentzIndex | Momentum)[y3_, dim2_ : 4] , dim2_ : 4],
			DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4], d___] :=
	-(dim1 - 4) diracologyBMHV2[b, DiracGamma[x1[y1, dim2], dim2],
						DiracGamma[x2[y2, dim2], dim2],
						DiracGamma[x3[y3, dim2], dim2], d] -
					2 diracologyBMHV2[b, DiracGamma[x3[y3, dim2], dim2],
							DiracGamma[x2[y2, dim2], dim2],
							DiracGamma[x1[y1, dim2], dim2], d]/;
							(dim1===dim2 || dim2 === dim1-4 || dim2 ===4) &&
							MatchQ[dim1, _Symbol | _Symbol-4 | 4 ] &&
							MatchQ[dim2, _Symbol | _Symbol-4 | 4 ];

(* D-4, D, D, D, D-4 or 4, D, D, D, 4 *)
diracologyBMHV2[b___ , DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4],
			DiracGamma[(x1: LorentzIndex | ExplicitLorentzIndex | Momentum)[y1_, dim2_Symbol], dim2_Symbol],
			DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim2_Symbol], dim2_Symbol],
			DiracGamma[(x3: LorentzIndex | ExplicitLorentzIndex | Momentum)[y3_, dim2_Symbol], dim2_Symbol],
			DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4], d___] :=
	-dim1 diracologyBMHV2[b, DiracGamma[x1[y1, dim2], dim2],
				DiracGamma[x2[y2, dim2], dim2],
				DiracGamma[x3[y3, dim2], dim2], d] +
	2 diracologyBMHV2[b, DiracGamma[x1[y1, dim1], dim1],
			DiracGamma[x2[y2, dim2], dim2],
			DiracGamma[x3[y3, dim2], dim2], d] -
	2 diracologyBMHV2[b, DiracGamma[x2[y2, dim1], dim1],
			DiracGamma[x1[y1, dim2], dim2],
			DiracGamma[x3[y3, dim2], dim2], d] +
	2 diracologyBMHV2[b, DiracGamma[x3[y3, dim1], dim1],
			DiracGamma[x1[y1, dim2], dim2],
			DiracGamma[x2[y2, dim2], dim2], d]/;
			(dim1 === dim2-4 || dim1 ===4);

(* Evaluation of g^mu g^nu g^rho g^sigma g^tau g_mu for Dirac matrices
	in different dimensions                                                    *)
(* ------------------------------------------------------------------------ *)

(* D, D, D, D, D, D or 4, 4, 4, 4, 4, 4 or D-4, D-4, D-4, D-4, D-4, D-4
	or D, D-4, D-4, D-4, D-4, D or D, 4, 4, 4, 4, D *)
diracologyBMHV2[b___ , DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4],
			DiracGamma[(x1: LorentzIndex | ExplicitLorentzIndex | Momentum)[y1_, dim2_ : 4] , dim2_ : 4],
			DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim2_ : 4] , dim2_ : 4],
			DiracGamma[(x3: LorentzIndex | ExplicitLorentzIndex | Momentum)[y3_, dim2_ : 4] , dim2_ : 4],
			DiracGamma[(x4: LorentzIndex | ExplicitLorentzIndex | Momentum)[y4_, dim2_ : 4] , dim2_ : 4],
			DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4], d___] :=
	(dim1 - 4) diracologyBMHV2[b, DiracGamma[x1[y1, dim2], dim2],
					DiracGamma[x2[y2, dim2], dim2],
					DiracGamma[x3[y3, dim2], dim2],
					DiracGamma[x4[y4, dim2], dim2], d] +
					2 diracologyBMHV2[b, DiracGamma[x3[y3, dim2], dim2],
							DiracGamma[x2[y2, dim2], dim2],
							DiracGamma[x1[y1, dim2], dim2],
							DiracGamma[x4[y4, dim2], dim2], d] +
				2 diracologyBMHV2[b, DiracGamma[x4[y4, dim2], dim2],
							DiracGamma[x1[y1, dim2], dim2],
							DiracGamma[x2[y2, dim2], dim2],
							DiracGamma[x3[y3, dim2], dim2], d]/;
							(dim1===dim2 || dim2 === dim1-4 || dim2 ===4) &&
							MatchQ[dim1, _Symbol | _Symbol-4 | 4 ] &&
							MatchQ[dim2, _Symbol | _Symbol-4 | 4 ];

(* D-4, D, D, D, D, D-4 or 4, D, D, D, D, 4 *)
diracologyBMHV2[b___ , DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4],
			DiracGamma[(x1: LorentzIndex | ExplicitLorentzIndex | Momentum)[y1_, dim2_Symbol], dim2_Symbol],
			DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim2_Symbol], dim2_Symbol],
			DiracGamma[(x3: LorentzIndex | ExplicitLorentzIndex | Momentum)[y3_, dim2_Symbol], dim2_Symbol],
			DiracGamma[(x4: LorentzIndex | ExplicitLorentzIndex | Momentum)[y4_, dim2_Symbol], dim2_Symbol],
			DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4], d___] :=
	dim1  diracologyBMHV2[b, DiracGamma[x1[y1, dim2], dim2],
				DiracGamma[x2[y2, dim2], dim2],
				DiracGamma[x3[y3, dim2], dim2],
				DiracGamma[x4[y4, dim2], dim2], d] -
	2 diracologyBMHV2[b, DiracGamma[x1[y1, dim1], dim1],
			DiracGamma[x2[y2, dim2], dim2],
			DiracGamma[x3[y3, dim2], dim2],
			DiracGamma[x4[y4, dim2], dim2], d] +
	2 diracologyBMHV2[b, DiracGamma[x2[y2, dim1], dim1],
			DiracGamma[x1[y1, dim2], dim2],
			DiracGamma[x3[y3, dim2], dim2],
			DiracGamma[x4[y4, dim2], dim2], d] -
	2 diracologyBMHV2[b, DiracGamma[x3[y3, dim1], dim1],
			DiracGamma[x1[y1, dim2], dim2],
			DiracGamma[x2[y2, dim2], dim2],
			DiracGamma[x4[y4, dim2], dim2], d] +
	2 diracologyBMHV2[b, DiracGamma[x4[y4, dim1], dim1],
			DiracGamma[x1[y1, dim2], dim2],
			DiracGamma[x2[y2, dim2], dim2],
			DiracGamma[x3[y3, dim2], dim2], d]/;
			(dim1 === dim2-4 || dim1 ===4);

(* Evaluation of a string of 4 dimensional Dirac matrices
	g^mu g^nu_1 ... g^nu_i g_mu  -> -2 g^nu_i ... g^nu_1,
	where i is odd *)
diracologyBMHV2[ b___,  DiracGamma[LorentzIndex[c_]],
		ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]]..,
		DiracGamma[LorentzIndex[c_]], f___ ] :=
	-2 diracologyBMHV2 @@ Join[ {b},Reverse[{ch}],{f} ] /; OddQ[Length[{ch}]];


(* Evaluation of a string of 4 dimensional Dirac matrices
	g^mu g^nu_1 ... g^nu_i g_mu  ->
	2 g^nu_i-1 ... g^nu_1 g^nu_i + 2 g^nu_i g^nu_1 ... g^nu_i-1,
	where i is even *)
diracologyBMHV2[ b___,  DiracGamma[LorentzIndex[c_]],
		ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]]..,
		end : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]],
		DiracGamma[LorentzIndex[c_]], f___ ] :=
	( 2 diracologyBMHV2 @@ Join[ {b},Reverse[{ch}],{end}, {f}] +
	2 diracologyBMHV2[ b,end,ch,f ]
	)/; OddQ[Length[{ch}]];

(* Slash(p).Slash(p), where both objects have the same dimension *)
diracologyBMHV2[b___,DiracGamma[Momentum[c_, dim_ : 4], dim_ : 4],
		DiracGamma[Momentum[c_, dim_ : 4], dim_ : 4],d___ ] :=
	FCUseCache[ExpandScalarProduct,{Pair[Momentum[c,dim],Momentum[c,dim]]},{}] diracologyBMHV2[b,d];

(* Simplifications for Slash(p) g^nu_1 ... g^nu_n Slash(p) where the slashes
	are in 4 and g^nu_i are in D-4 dimensions or vice versa.                 *)
(* ------------------------------------------------------------------------ *)

(* 4, (... D-4 ... ), 4 *)
diracologyBMHV2[b___ , DiracGamma[Momentum[c_]],
			ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim_Symbol-4] , dim_Symbol-4]..,
			DiracGamma[Momentum[c_]], d___] :=
	(-1)^Length[{ch}] FCUseCache[ExpandScalarProduct,{Pair[Momentum[c],Momentum[c]]},{}] diracologyBMHV2[b,ch, d];

(* D-4, (... 4 ... ), D-4 *)
diracologyBMHV2[b___ , DiracGamma[Momentum[c_, dim_Symbol-4], dim_Symbol-4],
			ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]]..,
			DiracGamma[Momentum[c_, dim_Symbol-4], dim_Symbol-4], d___] :=
	(-1)^Length[{ch}] FCUseCache[ExpandScalarProduct,{Pair[Momentum[c,dim-4],Momentum[c,dim-4]]},{}] diracologyBMHV2[b,ch, d];

(* Simplification for Slash(p) g^nu_1 ... g^nu_n Slash(p) where the slashes
	are in D and g^nu_i are in 4 dimensions. This applies for n>1.           *)
(* ------------------------------------------------------------------------ *)

diracologyBMHV2[b___ , DiracGamma[Momentum[c_, dim_Symbol], dim_Symbol],
			ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]]..,
			DiracGamma[Momentum[c_, dim_Symbol], dim_Symbol], d___] :=
	diracologyBMHV2[b,DiracGamma[Momentum[c]], ch, DiracGamma[Momentum[c]], d] +
	(-1)^Length[{ch}] FCUseCache[ExpandScalarProduct,{Pair[Momentum[c,dim-4],Momentum[c,dim-4]]},{}] diracologyBMHV2[b,ch, d]/; Length[{ch}]>1;

(* Simplification for Slash(p) g^nu_1 ... g^nu_n Slash(p) where the slashes are
	in D and g^nu_i are in D-4 dimensions. This applies for n>1.             *)
(* ------------------------------------------------------------------------ *)

diracologyBMHV2[b___ , DiracGamma[Momentum[c_, dim_Symbol], dim_Symbol],
			ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_,
			dim_Symbol-4],dim_Symbol-4]..,
			DiracGamma[Momentum[c_, dim_Symbol], dim_Symbol], d___] :=
	diracologyBMHV2[b,DiracGamma[Momentum[c,dim-4],dim-4], ch, DiracGamma[Momentum[c,dim-4],dim-4], d] +
	(-1)^Length[{ch}] FCUseCache[ExpandScalarProduct,{Pair[Momentum[c],Momentum[c]]},{}]  diracologyBMHV2[b,ch, d]/; Length[{ch}]>1;


(* Evaluation of Slash(p) g^nu Slash(p) for Dirac matrices in different
	dimensions                                                               *)
(* ------------------------------------------------------------------------ *)

diracologyBMHV2[b___ , DiracGamma[Momentum[c_, dim1_ : 4], dim1_ : 4],
		DiracGamma[(x: LorentzIndex | ExplicitLorentzIndex | Momentum)[y_, dim2_ : 4] ,dim2_ : 4],
		DiracGamma[Momentum[c_, dim1_ : 4], dim1_ : 4], d___] :=
	- FCUseCache[ExpandScalarProduct,{Pair[Momentum[c,dim1],Momentum[c,dim1]]},{}] diracologyBMHV2[b,DiracGamma[x[y, dim2], dim2], d] +
	2 FCUseCache[FCFastContract,{Pair[Momentum[c,dim1],x[y,dim2]] diracologyBMHV2[b, DiracGamma[Momentum[c, dim1], dim1], d]},{}]/;
					(dim1===dim2 || dim2 === dim1-4 || dim2 ===4 || dim1 === dim2-4 || dim1 ===4) &&
					MatchQ[dim1, _Symbol | _Symbol-4 | 4 ] &&
					MatchQ[dim2, _Symbol | _Symbol-4 | 4 ];

(* Simplification for g^mu g^nu_1 ... g^nu_n g_mu where all
	matrices are  in D or D-4 dimensions. Applies for n>5, since
	for n<=5 we have explicit expressions in the code. The
	formula is given in Eq 2.9 of R. Mertig, M. Boehm,
	A. Denner. Comp. Phys. Commun., 64 (1991)                *)
diracologyBMHV2[ b___,DiracGamma[LorentzIndex[c_,dim_],dim_],
	ch:DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_,
	dim_],dim_]..,
	DiracGamma[LorentzIndex[c_,dim_],dim_],f___ ] :=
	Block[ {iVar,jVar,len = Length[{ch}],dsTemp},
		FCUseCache[FCFastContract,{(-1)^len ( dim - 2 len ) dsTemp[b,ch,f] - 4 (-1)^len *
		Sum[ (-1)^(jVar-iVar) *  Pair[{ch}[[iVar,1]],
			{ch}[[jVar,1]] ]*dsTemp@@Join[{b},
			Delete[{ch}, {{iVar},{jVar}}], {f}],{iVar,1,len-1},{jVar,iVar+1,len}
		]},{}]/.dsTemp->diracologyBMHV2/. Pair[aa__] :> FCUseCache[ExpandScalarProduct,{Pair[aa]},{}]
	] /;(Length[{ch}]>4) && MatchQ[dim, _Symbol | _Symbol-4 ];


(* Simplification for Slash(p) g^nu_1 ... g^nu_n Slash(p) where all
	matrices are in D, 4 or D-4 dimensions. The
	formula is given in Eq 2.10 of R. Mertig, M. Boehm,
	A. Denner. Comp. Phys. Commun., 64 (1991)                *)
diracologyBMHV2[b___, DiracGamma[Momentum[c_, dim_ : 4], dim_ : 4],
			ch:DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim_ : 4], dim_ : 4]..,
			DiracGamma[Momentum[c_, dim_ : 4], dim_ : 4],f___
	] :=
	Block[ {iVar, len = Length[{ch}]},
		(-1)^len FCUseCache[ExpandScalarProduct,{Pair[Momentum[c,dim],Momentum[c,dim]]},{}] diracologyBMHV2[b,ch,f]
		+ 2 Sum[(-1)^(iVar+1) FCUseCache[FCFastContract,{ Pair[Momentum[c, dim],{ch}[[iVar,1]] ]
				* diracologyBMHV2@@Join[{b},Drop[{ch},{iVar, iVar}],{DiracGamma[Momentum[c, dim],dim],f}]
										},{}],{iVar, 1,len}]
	]/;(Length[{ch}]>0) && MatchQ[dim, _Symbol | _Symbol-4 | 4 ];

(* g^mu g^nu_1 ... g^nu_n g_mu, where g^mu is in 4 or D-4
	and g^nu_i are in D dimensions is simplfied by repeatedly
	applying anticommuation relations. Applies for n>5,
	since for n<=5 we have explicit expressions in the code *)

diracologyBMHV2[b___, DiracGamma[LorentzIndex[c_, dim_Symbol-4], dim_Symbol-4],
		ch1 : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim_Symbol] , dim_Symbol]..,
		ch2 : DiracGamma[(h : LorentzIndex | ExplicitLorentzIndex | Momentum)[x_, dim_Symbol] , dim_Symbol],
		DiracGamma[LorentzIndex[c_, dim_Symbol-4], dim_Symbol-4], d___] :=
	(-diracologyBMHV2[b, DiracGamma[LorentzIndex[c, dim-4],dim-4], ch1, DiracGamma[LorentzIndex[c, dim-4], dim-4], ch2, d]
	+ 2 diracologyBMHV2[b, DiracGamma[h[x,dim-4],dim-4], ch1, d] );

diracologyBMHV2[b___, DiracGamma[LorentzIndex[c_]],
		ch1 : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim_Symbol] , dim_Symbol]..,
		ch2 : DiracGamma[(h : LorentzIndex | ExplicitLorentzIndex | Momentum)[x_, dim_Symbol] , dim_Symbol],
		DiracGamma[LorentzIndex[c_]], d___] :=
	(-diracologyBMHV2[b, DiracGamma[LorentzIndex[c]], ch1, DiracGamma[LorentzIndex[c]], ch2, d]
	+ 2 diracologyBMHV2[b, DiracGamma[h[x]], ch1, d] );

(* Slash(p) g^nu_1 ... g^nu_n Slash(p), where the slashes
	are in 4 or D-4 and g^nu_i are in D dimensions is simplfied
	by repeatedly applying anticommuation relations.             *)

diracologyBMHV2[b___, DiracGamma[Momentum[c_, dim_Symbol-4], dim_Symbol-4],
		ch1 : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim_Symbol] , dim_Symbol]..,
		ch2 : DiracGamma[(h : LorentzIndex | ExplicitLorentzIndex | Momentum)[x_, dim_Symbol] , dim_Symbol],
		DiracGamma[Momentum[c_, dim_Symbol-4], dim_Symbol-4], d___] :=
	(-diracologyBMHV2[b, DiracGamma[Momentum[c, dim-4],dim-4], ch1, DiracGamma[Momentum[c, dim-4], dim-4], ch2, d]
	+ 2 FCUseCache[FCFastContract,{Pair[Momentum[c,dim-4],h[x,dim-4]] diracologyBMHV2[b, DiracGamma[Momentum[c, dim-4],dim-4], ch1, d]},{}]);

diracologyBMHV2[b___, DiracGamma[Momentum[c_]],
		ch1 : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim_Symbol] , dim_Symbol]..,
		ch2 : DiracGamma[(h : LorentzIndex | ExplicitLorentzIndex | Momentum)[x_, dim_Symbol] , dim_Symbol],
		DiracGamma[Momentum[c_]], d___] :=
	(-diracologyBMHV2[b, DiracGamma[Momentum[c]], ch1, DiracGamma[Momentum[c]], ch2, d]
	+ 2 FCUseCache[FCFastContract,{Pair[Momentum[c],h[x]] diracologyBMHV2[b, DiracGamma[Momentum[c]], ch1, d]},{}] );

(* g^mu ... g^nu g^rho g_mu, where g^mu is in D, 4, or D-4
	and g^nu and g^rho are in different dimensions is simplfied
	by repeatedly applying anticommuation relations.            *)

diracologyBMHV2[b___ , DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4],    w___,
			DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim3_ : 4] ,dim3_ : 4],
			DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4], d___] :=
	((
	-diracologyBMHV2[b,  DiracGamma[LorentzIndex[c, dim1], dim1], w,
			DiracGamma[LorentzIndex[c, dim1], dim1],
			DiracGamma[x2[y2,dim3],dim3], d]
		+ 2 FCUseCache[FCFastContract,{Pair[LorentzIndex[c, dim1], x2[y2,dim3]] diracologyBMHV2[b, DiracGamma[LorentzIndex[c, dim1], dim1], w, d]},{}]))/;
				(dim1===dim3 || dim3 === dim1-4 || dim1 === dim3-4 || dim3 === 4 || dim1 === 4) &&
				MatchQ[dim1, _Symbol | _Symbol-4 | 4 ] &&
				MatchQ[dim3, _Symbol | _Symbol-4 | 4 ] &&
				!MatchQ[{w, DiracGamma[x2[y2,dim3],dim3]},{DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim3] , dim3]..} |
					{___, DiracGamma[a__], ___, DiracGamma[a__], ___}] && NonCommFreeQ[{w}/.DiracGamma->diga];

(* Slash(p) ... g^nu g^rho Slash(p), where the slashes are in D, 4, or D-4
	and g^nu and g^rho are in different dimensions is simplfied
	by repeatedly applying anticommuation relations.            *)

diracologyBMHV2[b___ , DiracGamma[Momentum[c_, dim1_ : 4], dim1_ : 4], w___,
	DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim3_ : 4] ,dim3_ : 4],
	DiracGamma[Momentum[c_, dim1_ : 4], dim1_ : 4], d___] :=
	((-diracologyBMHV2[b,  DiracGamma[Momentum[c, dim1], dim1], w,
	DiracGamma[Momentum[c, dim1], dim1],
	DiracGamma[x2[y2,dim3],dim3], d]
	+ 2 FCUseCache[FCFastContract,{Pair[Momentum[c, dim1], x2[y2,dim3]] diracologyBMHV2[b, DiracGamma[Momentum[c, dim1], dim1], w, d]},{}]))/;
		(dim1===dim3 || dim3 === dim1-4 || dim1 === dim3-4 || dim3 === 4 || dim1 === 4) &&
		MatchQ[dim1, _Symbol | _Symbol-4 | 4 ] &&
		MatchQ[dim3, _Symbol | _Symbol-4 | 4 ] &&
		!MatchQ[{w, DiracGamma[x2[y2,dim3],dim3]},{DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim3] , dim3]..} |
			{___, DiracGamma[a__], ___, DiracGamma[a__], ___}] && NonCommFreeQ[{w}/.DiracGamma->diga];

FCPrint[1,"DiracTrick.m loaded."];
End[]
