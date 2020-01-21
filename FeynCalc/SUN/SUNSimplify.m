(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SUNSimplify *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 19 January '99 at 20:38 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: SUNSimplify *)

(* ------------------------------------------------------------------------ *)

SUNSimplify::usage =
"SUNSimplify simplifies products of SUNT (and complex conjugated) \
matrices. Renaming of dummy indices may be performed. \
If the option SUNTrace is set to False, then any SUNT-matrices are \
taken out of DiracTrace[...]; otherwise a color-trace is taken (by \
SUNTrace) before taking the SUN-objects in front of DiracTrace[...]. \
Whether SUNF is replaced by traces is determined by the option Explicit.";

SUNFJacobi::usage="SUNFJacobi is an option for SUNSimplify, indicating \
whether the Jacobi identity should be used.";

SUNIndexRename::usage= "SUNIndexRename is an option of SUNSimplify. If set to \
False, no automatic renaming of dummy indices is done.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`SUNSimplify`Private`"]

sunSiVerbose::usage="";
dummy::usage="";
dotT::usage="";

(* repeat basically SUNTrace here, since several subfunctions are
	needed anyway
*)

(* *********************************************************************** *)

(* change SUNT' which are multiplied with each other to lambdaT's *)
lambdaT[1] =
	1;
gm2lambdaT[] =
	1;
gm2lambdaT[x__] :=
	(gmlin@@( {x}/.SUNT->lambdaT ) )/.gmlin->Dot;
(********************* linearity  ********************************* *)
(* noncomQdef : checking non-commutativity *)
noncomQ[z_] :=
	TrueQ[noncQ[z]];

noncQ[_?NumberQ] :=
	True;

noncQ[_suntrace] :=
	True;

noncQ[x_] :=
	If[ FreeQ2[FixedPoint[ReleaseHold, x], $NonComm],
		True,
		False
	];

gmlin/:
	HoldPattern[gmlin[gmlin[x__]]] :=
		gmlin[x];

gmlin[ a___, b_ c_, d___ ] :=
	b gmlin[a,c,d]/;FreeQ[b, lambdaT] && noncomQ[b];

gmlin[ a___, b_ , d___ ] :=
	b gmlin[a,d]/;FreeQ[b, lambdaT] && noncomQ[b];

gmlin[] =
	1;

gellm1[x_, y__] :=
	gellm1[DOT[x, y]];

gellm2[x_, y__] :=
	gellm2[DOT[x, y]];

(******************* cyclicity *********************************** *)
gmcyc[x__] :=
	gellm1 @@ First[NestList[RotateLeft, {x}, Length[{x}]-1]//Sort];
(************* define the properties of trace of T-matrices *)
gellm2[ ] =
	gmcyc[ ] = SUNN;         (* unit trace  *)
(************** each single T-matrix has vanishing trace *)
gellm2[ lambdaT[_] ] :=
	0;
(************** Cvitanovic - rules ******************************* *)
gellm2[HoldPattern[Dot[a___, lambdaT[i_], b___, lambdaT[i_], c___]]] :=
	(1/2 gmcyc[b] gmcyc[a, c] - 1/2/SUNN gmcyc[a, b, c]) /; Head[i] === SUNIndex;

gellm2/:
	gellm2[HoldPattern[Dot[a___, lambdaT[i_], b___]]]^2 :=
		(1/2 gmcyc[a, b, a, b] - 1/2/SUNN gmcyc[a, b]^2) /; Head[i] === SUNIndex;

gellm2/:
	gellm2[HoldPattern[Dot[a___, lambdaT[i_], b___]]] *
	gellm2[HoldPattern[Dot[c___, lambdaT[i_], d___]]]:=
		(1/2 gmcyc[a, d, c, b] - 1/2/SUNN gmcyc[a, b] gmcyc[c, d]) /; Head[i] === SUNIndex;

f2tr[i_,j_,k_,___] :=
	2 I (gmcyc @@ lambdaT/@{i,k,j} - gmcyc @@ lambdaT/@{i,j,k});
(* do the application of the Cvitanovic - relations step by step *)
cvit[x_Plus] :=
	cvit/@x;
cvit[x_] :=
	(cvit[x] = ExpandAll[ x /. gellm1 -> gellm2 ]);

gellm1[x_Plus] :=
	gellm1 /@ x;

gellm1/:
	gellm1[x_ y_] :=
		x gellm1[y]/;FreeQ[x,lambdaT];

gellm1/:
	gellm1[x_Dot gellm1[y___]] :=
		gellm1[y] gellm1[x];

gellex[z_] :=
	gellm1[ExpandAll[z]];

fixgell[x_] :=
	(fixgell[x] = FixedPoint[cvit, ( gellm1[ExpandAll[x/.SUNTrace->gellex/.
	DOT->gm2lambdaT/.SUNF->f2tr]])/.gellm1->gellm2, 9]/.lambdaT->SUNT);

(* *********************************************************************** *)

(*Renamed*)
Options[Rename] = {
	Expanding -> False
};


(* Added check for integers - noint. F.Orellana, 24/9-2000 *)
noint[x___] :=
	Not[Or @@
		Join[IntegerQ /@ {x}, IntegerQ /@
	({x} /. {SUNIndex -> Identity,
			ExplicitSUNIndex -> Identity})]];

SetAttributes[Rename, Listable];

Rename[exp_,ru___Rule] :=
	Block[ {new = exp,old,subst,uh,ne,uhc = 0, uuh, suI, expan, suh = {},
		sub = {}, sam, dummy = FCGV[ToString[Unique["c"]]]},
		expan = Expanding /. {ru} /. Options[Rename];
		sam[iii_, uuh_ ] :=
			(AppendTo[sub, SUNIndex[uuh] -> SUNIndex[iii]]) /; FreeQ[sub, SUNIndex[uuh]];
		uuh[] :=
			SUNIndex[dummy[uhc++]];
		suii[0] = 0;
		suii[a_Plus] :=
			suii /@ a;
		suii[y_] :=
			Block[ {ste, ste2, dumy},
				ste = Select[y dumy, !FreeQ2[#, {SUNIndex}]&];
				ste2 = Select[y dumy, FreeQ2[#, {SUNIndex}]&];
				(ste2 suI[ste])/.dumy->1
			](* /. suI -> Identity*);

		subst = {
			suI[SUNT[SUNIndex[ii_]] ff_] :>
				((sam[ii, uh[]]; SUNT[SUNIndex[ii]] (ff /. suI -> Identity) /.
					SUNIndex[ii]-> uh[]))/; !FreeQ[ff,SUNIndex[ii]] && FreeQ[ii,dummy[_]],

			DOT[ A___,SUNT[SUNIndex[ii_]], B___, SUNT[SUNIndex[ii_]], Z___] :>
				(sam[ii, uh[]]; DOT[ A,SUNT[SUNIndex[ii]],B,SUNT[SUNIndex[ii]],Z ] /.
					SUNIndex[ii] -> uh[]) /; FreeQ[ii, dummy[_]],

			suI[DOT[ A___,SUNT[SUNIndex[jj_]], B___] ff_] :>
				(sam[jj, uh[]]; (DOT[ A,SUNT[SUNIndex[jj]],B ] ff) /. SUNIndex[jj] -> uh[])/;
					!FreeQ[ff,SUNIndex[jj]] && FreeQ[jj,dummy[_]],

			suI[SUNF[A___,SUNIndex[ij_],B___] SUNF[V___,SUNIndex[ij_],W___] ff_] :>
				(sam[ij, uh[]]; (SUNF[A, SUNIndex[ij], B] SUNF[V, SUNIndex[ij], W] ff)/. SUNIndex[ij] -> uh[])/; FreeQ[ij, dummy[_]],

			suI[ SUNF[SUNIndex[a_], SUNIndex[ci4_], SUNIndex[ci6_]] SUNF[SUNIndex[b_], SUNIndex[ci4_], SUNIndex[ci7_]]*
			SUNF[SUNIndex[c_], SUNIndex[ci6_], SUNIndex[ci7_]] ef_.] :>
				(ef SUNN/2 SUNF[SUNIndex[a],SUNIndex[b],SUNIndex[c]]) /; noint[ci4,ci6,ci7],

			suI[ SUNF[SUNIndex[ci4_], SUNIndex[ci6_],SUNIndex[a_]] SUNF[SUNIndex[ci4_], SUNIndex[ci7_],SUNIndex[b_]]*
			SUNF[SUNIndex[ci6_], SUNIndex[ci7_],SUNIndex[c_]] ef_.] :>
				(ef SUNN/2 SUNF[SUNIndex[a],SUNIndex[b],SUNIndex[c]]) /; noint[ci4,ci6,ci7],

			suI[ SUNF[SUNIndex[a_], SUNIndex[ci4_], SUNIndex[ci6_]]	SUNF[SUNIndex[b_], SUNIndex[ci4_], SUNIndex[ci6_]] ef_.] :>
				(ef SUNN SUNDelta[SUNIndex[a], SUNIndex[b]]) /; noint[ci4,ci6],

			suI[SUNF[A___,SUNIndex[ij_],B___] ff_] :>
				(sam[ij, uh[]]; (SUNF[A,SUNIndex[ij],B] ff)/. SUNIndex[ij] -> uh[])/;
				!FreeQ[ff, SUNIndex[ij]] && FreeQ[ij, dummy[_]]
					,
			suI[SUNF[SUNIndex[a1_], SUNIndex[ci4_], SUNIndex[ci5_]] SUNF[SUNIndex[a2_], SUNIndex[ci5_], SUNIndex[e_]]*
			SUNF[SUNIndex[a3_], SUNIndex[ci4_], SUNIndex[e_]] ef_.] :>
				-ef CA/2 SUNF[SUNIndex[a1],SUNIndex[a2],SUNIndex[a3]] /; noint[ci4,ci5]
					,
			suI[SUNF[SUNIndex[a1_], SUNIndex[ci4_], SUNIndex[ci5_]]	SUNF[SUNIndex[a2_], SUNIndex[ci4_], SUNIndex[e_]]*
			SUNF[SUNIndex[a3_], SUNIndex[ci5_], SUNIndex[e_]] ef_.] :>
				ef CA/2 SUNF[SUNIndex[a1],SUNIndex[a2],SUNIndex[a3]] /; noint[ci4,ci5]
					,
			suI[SUNF[SUNIndex[a1_], SUNIndex[ci4_], SUNIndex[ci5_]]	SUNF[SUNIndex[a2_], SUNIndex[a3_], SUNIndex[e_]]*
			SUNF[SUNIndex[ci4_], SUNIndex[ci5_], SUNIndex[e_]] ef_.] :>
				ef CA SUNF[SUNIndex[a1],SUNIndex[a2],SUNIndex[a3]] /; noint[ci4,ci5],

			SUNF[a_,b_,c_]^2 :> 2 CA^2 CF /; noint[a,b,c]
					,
		(* SUNDRULES*)
			SUND[a_,b_,c_]^2 :> -2 (4-CA^2) CF /; noint[a,b,c],
			SUND[a_,b_,c_] SUND[d_,b_,c_] :> -(4 - CA^2) (CA - 2 CF) SUNDelta[a,d] /; noint[b,c],
			SUNF[_,b_,c_] SUND[_,b_,c_] :> 0 /; noint[b,c],
			SUNF[b_,_,c_] SUND[_,b_,c_] :> 0 /; noint[b,c],
			SUNF[b_,c_,_] SUND[_,b_,c_] :> 0 /; noint[b,c]
		(*
			SUNF[a_,b_,e_] SUNF[c_,d_,e_] :>  2/SUNN SUNDelta[a,b] SUNDelta[c,d] +
			SUND[a,c,e] SUND[b,d,e] - SUND[a,d,e] SUND[b,c,e]
		*)
		};

		suff[x_] :=
			x /; FreeQ[x, SUNIndex];

		(*
		CHANGE Rolf Mertig 15.2.2006;
		This is dangerous:
		when subst is applied multiple times and matches several dummy indices then
		things go wrong, see
		http://www.feyncalc.org/forum/0366.html

		suff[x_] := FixedPoint[(uh[] = uuh[];
								(suii[#]//.subst)/.suI->Identity)&, x, 42];

		so, changing back to the original:
		*)
		suff[x_] :=
			FixedPoint[(uh[] = uuh[];
						(suii[#]/.subst)/.suI->Identity)&, x, 42];

		If[ expan === True,
			FCPrint[2,"expanding w.r.t. sunf done "];
			new = Expand2[new, SUNIndex];
			FCPrint[2,"expanding w.r.t. sunf done "];
		];
		If[ Head[new] === Plus,
			new = SelectFree[new, SUNIndex] + suff[SelectNotFree[new, SUNIndex]],
			new = suff[new];
		];
		new = backsubfun[new, sub];
		new
	];

backsubfun[xxx_, {}] :=
	xxx;

backsubfun[xxx_, {a_ -> b_, c___Rule}] :=
	If[ FreeQ[xxx, b],
		backsubfun[xxx /. a -> b,{c}],
		backsubfun[xxx, {c}]
	];

(* *********************************************************************** *)
SetAttributes[setdel, HoldRest];

setdel[x_, y_] :=
	SetDelayed[x ,y];

setdel[
HoldPattern[sunTRACEcyc[dottt[z:sunttt[_]..]]] /.dottt->DOT/.sunttt->SUNT ,
sunTRACE[DOT@@RotateLeft[{z}, Position[{z},Last[Sort[{z}]]][[1,1]]]]];


	sunsi3 = {
				(* f^abc T^a T^b T^c + permutations *)

				sunf[r1___, zz_SUNIndex, r2___, yy_SUNIndex, r3___] dotT[aa___, SUNT[xx_SUNIndex], SUNT[yy_SUNIndex], SUNT[zz_SUNIndex], bb___] :>
					(*(-1)^(Length[{r2}]+1) sunf[r3, r1, r2, yy, zz] dotT[aa, SUNT[xx], SUNT[yy], SUNT[zz], bb]*)
					(-1)^(Length[{r2}]+1) I/2 CA CF dotT[aa,bb] /; {r3,r1,r2} === {xx},


				sunf[r1___, yy_SUNIndex, r2___, zz_SUNIndex, r3___] dotT[aa___, SUNT[xx_SUNIndex], SUNT[yy_SUNIndex], SUNT[zz_SUNIndex], bb___] :>
					(*(-1)^(Length[{r2}]) sunf[r3, r1, r2, yy, zz] dotT[aa, SUNT[xx], SUNT[yy], SUNT[zz], bb]*)
					(-1)^(Length[{r2}]) I/2 CA CF dotT[aa,bb] /; {r3,r1,r2} === {xx}
			};

			sunsi2 = {
				(* f^abc T^a T^b + permutations *)

				sunf[r1___, zz_SUNIndex, r2___, yy_SUNIndex, r3___] dotT[aa___, SUNT[yy_SUNIndex], SUNT[zz_SUNIndex], bb___] :>
					(* (-1)^(Length[{r2}]+1) sunf[r3, r1, r2, yy, zz] dotT[aa, SUNT[yy], SUNT[zz], bb] *)
					(-1)^(Length[{r2}]+1) I/2 SUNN dotT[aa,SUNT[r3, r1, r2],bb],

				sunf[r1___, yy_SUNIndex, r2___, zz_SUNIndex, r3___] dotT[aa___, SUNT[yy_SUNIndex], SUNT[zz_SUNIndex], bb___] :>
					(* (-1)^(Length[{r2}]) sunf[r3, r1, r2, yy, zz] dotT[aa, SUNT[yy], SUNT[zz], bb] *)
					(-1)^(Length[{r2}]) I/2 SUNN dotT[aa,SUNT[r3, r1, r2],bb]
			};

			sunsi1 = {
				(* f^abc T^a + permutations *)
				sunf[r1___,xx_SUNIndex, r2___] SUNT[xx_SUNIndex]  :>
					I DOT[SUNT[Last[{r2,r1}]] , SUNT[First[{r2,r1}]]] - I DOT[SUNT[First[{r2,r1}]] , SUNT[Last[{r2,r1}]]]/; Length[{r2,r1}]===2
			};


			sunsi = {
				(* T^a T^a *)
				dotT[xx___, SUNT[a_SUNIndex], SUNT[a_SUNIndex], yy___] :>
					((SUNN^2 -1)/(2 SUNN) dotT[xx,yy]),
				(* T^a T^b T^a *)
				dotT[xx___, SUNT[a_SUNIndex], SUNT[b_SUNIndex], SUNT[a_SUNIndex], yy___] :>
					((-1)/(2 SUNN) dotT[xx, SUNT[b], yy]),

				(* general algorithm: T_i T_a = I T_c f_iac + T_a T_i *)

				dotT[xx___, SUNT[i_SUNIndex], SUNT[a_SUNIndex], d:SUNT[_SUNIndex].. , SUNT[i_SUNIndex], e___] :>
					(dummy=Unique["cc"];
					I sunf[i,a,SUNIndex[dummy]]  DOT[xx, SUNT[SUNIndex[dummy]], d, SUNT[i], e] +
					dotT[xx, SUNT[a], SUNT[i], d, SUNT[i], e]),


				sunf[r1___, i_SUNIndex, r2___, j_SUNIndex, r3___] dotT[xx___, SUNT[i_SUNIndex], SUNT[a_SUNIndex], d:SUNT[_SUNIndex]... , SUNT[j_SUNIndex], e___] :>
					(dummy=Unique["cc"];
					I sunf[r1, i, r2, j, r3] sunf[i,a,SUNIndex[dummy]]  DOT[xx, SUNT[SUNIndex[dummy]], d, SUNT[j], e] +
					sunf[r1, i, r2, j, r3] dotT[xx, SUNT[a], SUNT[i], d, SUNT[j], e])/; FreeQ2[{a,d},{r1,r2,r3,i,j}],


				sunf[r1___, j_SUNIndex, r2___, i_SUNIndex, r3___] dotT[xx___, SUNT[i_SUNIndex], SUNT[a_SUNIndex], d:SUNT[_SUNIndex]... , SUNT[j_SUNIndex], e___] :>
					(dummy=Unique["cc"];
					I sunf[r1, j, r2, i, r3] sunf[i,a,SUNIndex[dummy]]  DOT[xx, SUNT[SUNIndex[dummy]], d, SUNT[j], e] +
					sunf[r1, j, r2, i, r3] dotT[xx, SUNT[a], SUNT[i], d, SUNT[j], e])/; FreeQ2[{a,d},{r1,r2,r3,i,j}]


			};



(*SUNSimplifydef*)
SetAttributes[SUNSimplify, Listable];

Options[SUNSimplify] = {
	Expanding		-> False,
	Explicit		-> False,
	Factoring		-> False,
	FCI 			-> False,
	FCVerbose 		-> False,
	SUNIndexRename	-> True,
	SUNFJacobi		-> False,
	SUNNToCACF		-> True,
	SUNTrace		-> False
};



(* Main entry point *)
SUNSimplify[x_, opts:OptionsPattern[]] :=
	FixedPoint[sunsimp[#, Flatten[Join[{opts}, FilterRules[Options[SUNSimplify], Except[{opts}]]]]]&, x, 6] /. dtr -> DiracTrace;

Options[sunsimp] = Options[SUNSimplify];

sunsimp[expr_, opts:OptionsPattern[]] :=
	Block[ {sunntocacf, ex, temp, explicit, sunf, suntraceoption,surule, diractr,doot,
		expan, expanding, factoring,ntemp, tfac = 1, time},

		expanding = OptionValue[SUNSimplify,{opts},Expanding];
		factoring = OptionValue[SUNSimplify,{opts},Factoring];
		sunntocacf = OptionValue[SUNSimplify,{opts},SUNNToCACF];
		explicit = OptionValue[SUNSimplify,{opts},Explicit];
		suntraceoption = OptionValue[SUNSimplify,{opts},SUNTrace];

		If [OptionValue[FCVerbose]===False,
			sunSiVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				sunSiVerbose=OptionValue[FCVerbose]
			];
		];

		If[	OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];
		temp = ex;

		FCPrint[1, "SUNSimplify: Entering.", FCDoControl->sunSiVerbose];
		FCPrint[3, "SUNSimplify: Entering with ", temp, FCDoControl->sunSiVerbose];

		If[ Head[temp] === Times,
			tfac = Select[temp,  FreeQ2[#, {SUNIndex, SUNFIndex, SUNN, CA, CF}]&];
			temp = Select[temp, !FreeQ2[#, {SUNIndex, SUNFIndex, SUNN, CA, CF}]&];
		];


		If[ !FreeQ[temp, SUNFIndex],
			time=AbsoluteTime[];
			FCPrint[1, "SUNSimplify: Applying SUNFSimplify.", FCDoControl->sunSiVerbose];
			temp = SUNFSimplify[temp, SUNNToCACF->sunntocacf, Explicit->explicit];
			FCPrint[1, "SUNSimplify: Done applying SUNFSimplify, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->sunSiVerbose];
		];

		(*TODO Add

		SUNF[a,b,c,d]
		SUNF[a,b]
		SUNF[]

		*)

		time=AbsoluteTime[];
		FCPrint[1, "SUNSimplify: Applying SUNDeltaContract.", FCDoControl->sunSiVerbose];
		temp = temp /. SUNDelta -> SUNDeltaContract/. SUNF[a_,b_,c_,d_SUNIndex] :> SUNF[a,b,c,d, Explicit->True];
		FCPrint[1, "SUNSimplify: Done applying SUNDeltaContract, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->sunSiVerbose];
		FCPrint[3, "SUNSimplify: After  SUNDeltaContract", temp, FCDoControl->sunSiVerbose];

		If[ !FreeQ[temp, SUNIndex] || !FreeQ[temp, SUNN] || suntraceoption,


			If[ explicit,
				sunfL[a__] :=
					SUNF[a, Explicit -> True];

				sundL[a__] :=
					SUND[a, Explicit -> True],
(*
				SetOptions[SUND, Explicit-> False];
				SetOptions[SUNF, Explicit-> False];
*)
				sundL[a__] :=
					SUND[a];
				sunfL[a__] :=
					SUNF[a]
			];

			time=AbsoluteTime[];
			FCPrint[1, "SUNSimplify: Applying simplification rules.", FCDoControl->sunSiVerbose];
			temp = FixedPoint[Expand2[(# /. SUNTrace -> sunTrace /. DOT -> dotT /. SUNF -> sunf //. sunsi //. sunsi3 //. sunsi2 //. sunsi1 //. sunsi /. dotT[] -> 1 //.
				{dotT[a___,1,b___] :> dotT[a,b]} /. dotT[] -> 1),{SUNF,SUNT}]&, temp]/. dotT -> DOT /. sunf ->SUNF /. sunTrace ->SUNTrace;
			FCPrint[1, "SUNSimplify: Done applying simplification rules, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->sunSiVerbose];
			FCPrint[3, "SUNSimplify: After applying simplification rules, ", temp, FCDoControl->sunSiVerbose];


			If[ OptionValue[SUNSimplify,{opts},SUNIndexRename],
				time=AbsoluteTime[];
				FCPrint[1, "SUNSimplify: Renaming.", FCDoControl->sunSiVerbose];
				temp = Rename[temp, Expanding -> expanding];
				FCPrint[1, "SUNSimplify: Renaming done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->sunSiVerbose];
				FCPrint[3, "SUNSimplify: After renaming, ", temp, FCDoControl->sunSiVerbose];
			];


			If[ !FreeQ[temp, DiracTrace],
				If[ suntraceoption === True,
					surule = {(* Added 4/9-2002. Frederik Orellana.
								Expressions without SUNT
								(proportional to the identity matrix)
								were not SUNTrace'd *)
							diractr[dd_Times , dops___Rule] :>
							SUNTrace[SelectNotFree[dd, SUNIndex]]*
								dtr[SelectFree[dd, SUNIndex], dops] /;
								FreeQ[dd, SUNT[___]],
							diractr[dd_?((Head[#]=!=Times)&) , ___Rule] :>
							SUNTrace[dtr[dd]]/;
								FreeQ[dd, SUNT[___]],

							(*Added Times to avoid SelectNotFree[a+b,SUNIndex] --> 0*)
							diractr[doot[xx__sunt] dd_. , dops___Rule] :>
							SUNTrace[DOT[xx] SelectNotFree[dd, SUNIndex]]*
								DiracTrace[SelectFree[dd, SUNIndex], dops],
							diractr[doot[xx__sunt, y__] dd_., dops___Rule] :>
							SUNTrace[DOT[xx] SelectNotFree[dd, SUNIndex]] *
								DiracTrace[doot[y] SelectFree[dd, SUNIndex], dops ] /;
								FreeQ[{y}, SUNIndex],
							diractr[doot[SUNT[_], dd_], ___Rule] :> 0 /;
								FreeQ2[dd,{SUNIndex,SUNT}],
							diractr[SUNT[_]  dd_., ___Rule] :> 0 /;
								FreeQ2[dd,{SUNIndex,SUNT}]
							},
					surule = {diractr[doot[xx__sunt] dd_. , dops___Rule] :>
								DOT[xx] SelectNotFree[dd, SUNIndex] *
								DiracTrace[SelectFree[dd, SUNIndex], dops],
							diractr[doot[xx__sunt, y__] dd_. , dops___Rule] :>
								DOT[xx] SelectNotFree[dd, SUNIndex] *
								DiracTrace[doot[y] SelectFree[dd, SUNIndex], dops] /;
								FreeQ[{y}, SUNIndex]
							}
				];
				temp = temp /. DiracTrace -> diractr /.
											DOT -> doot /. surule /.
											doot -> DOT /. diractr -> DiracTrace/.
											SUNDelta -> SUNDeltaContract /.
											SUNDeltaContract->SUNDelta
			];

			If[ FreeQ2[temp, {SUNTrace}] && !explicit,
				expan = Identity,
				expan = Expand2(*All*)[#, SUNIndex]&
			];
			temp = FixedPoint[expan, temp /. SUNTrace -> sunTRACEcyc /. DOT -> gm2lambdaT /.lambdaT -> SUNT /.
				sunTRACEcyc -> SUNTrace /. sunTRACE -> SUNTrace /. {SUNF :> sunfL, SUND :> sundL} ];

			If[ OptionValue[SUNSimplify,{opts},SUNFJacobi] && !FreeQ[temp, SUNF],
				temp = temp /. SUNF -> sUNF /. (sUNF[a_, b_, c_] sUNF[d_, c_, e_] ) ->
					(- sUNF[a, b, c] sUNF[d, e, c]) /. (sUNF[a_, b_, c_] sUNF[c_, d_, e_] ) ->
									(sUNF[a, b, c] sUNF[d, e, c])/.
								{ sUNF[SUNIndex[a_], SUNIndex[c_], SUNIndex[e_]]*
								sUNF[SUNIndex[b_], SUNIndex[d_], SUNIndex[e_]] :>
								(sUNF[SUNIndex[a], SUNIndex[b], SUNIndex[e]]*
								sUNF[SUNIndex[c], SUNIndex[d], SUNIndex[e]] +
									sUNF[SUNIndex[b], SUNIndex[c], SUNIndex[e]]*
									sUNF[SUNIndex[a], SUNIndex[d], SUNIndex[e]]
								) /; noint[e] && Sort[{ {a,c,e, b,d,e}, {a,b,e, c,d,e},
											{b,c,e, a,d,e}
											}][[1]] === {a,c,e, b,d,e}
								} /. sUNF -> SUNF;
			];


			If[ !FreeQ[temp, SUNIndex],
				time=AbsoluteTime[];
				FCPrint[1, "SUNSimplify: Applying simplification rules.", FCDoControl->sunSiVerbose];
				temp = FixedPoint[Expand2[(# /. SUNTrace -> sunTrace /. DOT -> dotT /. SUNF -> sunf //. sunsi //. sunsi3 //. sunsi2 //. sunsi1 //. sunsi /. dotT[] -> 1 //.
				{dotT[a___,1,b___] :> dotT[a,b]} /. dotT[] -> 1),{SUNF,SUNT}]&, temp]/. dotT -> DOT /. sunf ->SUNF /. sunTrace ->SUNTrace;
				FCPrint[1, "SUNSimplify: Done applying simplification rules, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->sunSiVerbose];
				FCPrint[3, "SUNSimplify: After applying simplification rules, ", temp, FCDoControl->sunSiVerbose]
			];

			If[ expanding,
				temp = Expand[temp],
				If[ LeafCount[temp] < 242 && Head[temp] === Plus,
					ntemp = Expand[temp];
					If[ LeafCount[ntemp] < LeafCount[temp],
						temp = ntemp
					]
				];
			];
			If[ factoring === True,
				temp = Factor2[temp, FactorFull -> False]
			];
		](*thatsthemainIf*);
		If[ sunntocacf,
			If[ LeafCount[temp] < 1442(* RM20120113: exteneded the limit *),
				temp = Factor2[temp /. {CA ->SUNN, CF -> (SUNN^2-1)/(2 SUNN)}, FactorFull -> False]
			];
			temp = temp /. (1-SUNN^2) -> (-CF 2 CA) /. SUNN -> CA /. (-1 + CA^2)->(2 CA CF);
			(* RM20120113 added this in response to http://www.feyncalc.org/forum/0682.html, which is not a real bug, but well *)
			temp = temp /. (((2 - CA^2) CF )/CA ) ->(CF (CA - 4 CF));
			temp = temp /. (1-CA^2) -> (-2 CA CF) /. (1/CA) -> (CA - 2 CF) /. ((1 - CA^2)*(CA - 2*CF)) -> (-2*CF) /.
				(CA (CA-2 CF)) -> 1,
			temp = temp /. CA -> SUNN /. CF -> ((SUNN^2-1)/(2 SUNN))
		];
		If[ tfac =!= 1,
			temp = temp tfac
		];
		temp = temp /. SUNDeltaContract -> SUNDelta;
		If[ !FreeQ[temp, CA],
			temp = temp /. (CA (CA-2 CF)) -> 1
		];
		temp = DotSimplify[temp, Expanding -> False];
		temp
	];

FCPrint[1,"SUNSimplify.m loaded."];
End[]
