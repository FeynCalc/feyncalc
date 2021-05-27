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
		sam[iii_, xh_ ] :=
			(AppendTo[sub, SUNIndex[xh] -> SUNIndex[iii]]) /; FreeQ[sub, SUNIndex[xh]];
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


			(* Product of 3 SUNFs  f^aij f^bik f^cjk = N/2 f^abc (all permutation shd work as well!!!)  *)
			suI[ SUNF[SUNIndex[a_], SUNIndex[ci4_], SUNIndex[ci6_]] SUNF[SUNIndex[b_], SUNIndex[ci4_], SUNIndex[ci7_]]*
			SUNF[SUNIndex[c_], SUNIndex[ci6_], SUNIndex[ci7_]] ef_.] :>
				(ef SUNN/2 SUNF[SUNIndex[a],SUNIndex[b],SUNIndex[c]]) /; noint[ci4,ci6,ci7],

			suI[ SUNF[SUNIndex[ci4_], SUNIndex[ci6_],SUNIndex[a_]] SUNF[SUNIndex[ci4_], SUNIndex[ci7_],SUNIndex[b_]] SUNF[SUNIndex[ci6_], SUNIndex[ci7_],SUNIndex[c_]] ef_.] :>
				(ef SUNN/2 SUNF[SUNIndex[a],SUNIndex[b],SUNIndex[c]]) /; noint[ci4,ci6,ci7],


			(* Product of 2 SUNFs  f^aij f^bij = N d^ab *)
			suI[ SUNF[SUNIndex[a_], SUNIndex[ci4_], SUNIndex[ci6_]]	SUNF[SUNIndex[b_], SUNIndex[ci4_], SUNIndex[ci6_]] ef_.] :>
				(ef SUNN SUNDelta[SUNIndex[a], SUNIndex[b]]) /; noint[ci4,ci6],

			(* Product of 2 SUNFs  (f^abc)^2 = 2 CA^2 CF *)
			SUNF[a_,b_,c_]^2 :> 2 CA^2 CF /; noint[a,b,c],

			suI[SUNF[A___,SUNIndex[ij_],B___] ff_] :>
				(sam[ij, uh[]]; (SUNF[A,SUNIndex[ij],B] ff)/. SUNIndex[ij] -> uh[])/; !FreeQ[ff, SUNIndex[ij]] && FreeQ[ij, dummy[_]]
					,
			(* Products of 3 SUNFs *)

			(**)
			suI[SUNF[SUNIndex[a1_], SUNIndex[ci4_], SUNIndex[ci5_]] SUNF[SUNIndex[a2_], SUNIndex[ci5_], SUNIndex[e_]] SUNF[SUNIndex[a3_], SUNIndex[ci4_], SUNIndex[e_]] ef_.] :>
				-ef CA/2 SUNF[SUNIndex[a1],SUNIndex[a2],SUNIndex[a3]] /; noint[ci4,ci5]
					,
			suI[SUNF[SUNIndex[a1_], SUNIndex[ci4_], SUNIndex[ci5_]]	SUNF[SUNIndex[a2_], SUNIndex[ci4_], SUNIndex[e_]] SUNF[SUNIndex[a3_], SUNIndex[ci5_], SUNIndex[e_]] ef_.] :>
				ef CA/2 SUNF[SUNIndex[a1],SUNIndex[a2],SUNIndex[a3]] /; noint[ci4,ci5]
					,
			suI[SUNF[SUNIndex[a1_], SUNIndex[ci4_], SUNIndex[ci5_]]	SUNF[SUNIndex[a2_], SUNIndex[a3_], SUNIndex[e_]] SUNF[SUNIndex[ci4_], SUNIndex[ci5_], SUNIndex[e_]] ef_.] :>
				ef CA SUNF[SUNIndex[a1],SUNIndex[a2],SUNIndex[a3]] /; noint[ci4,ci5]					,
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


	(*	applies Cvitanovic's algorithm to simplify chains that contain common indices *)
	ruleCvitanovic = {

		(* ... T^a T^a ... *)
		dotT[xx___, SUNT[a_SUNIndex], SUNT[a_SUNIndex], yy___] :>
			((SUNN^2 -1)/(2 SUNN) dotT[xx,yy]),

		(* ... T^a T^b T^a ... *)
		dotT[xx___, SUNT[a_SUNIndex], SUNT[b_SUNIndex], SUNT[a_SUNIndex], yy___] :>
			((-1)/(2 SUNN) dotT[xx, SUNT[b], yy]),

		(* ... T^a T^b ... T^a ... *)
		dotT[A___, SUNT[i_SUNIndex], SUNT[a_SUNIndex], B:SUNT[_SUNIndex].. , SUNT[i_SUNIndex], C___] :>
				1/2 dotT[A,C] sunTrace[dotT[SUNT[a],B]] - 1/(2 SUNN) dotT[A,SUNT[a],B,C],


		(* ... T^a ... Tr[ ... T^a ... ] *)
		SUNT[a_SUNIndex] sunTrace[dotT[xc___, SUNT[a_SUNIndex], xd___]] :>
			1/2 dotT[xd,xc] - 1/(2 SUNN) sunTrace[dotT[xc,xd]],


		(* ... T^a ... Tr[ ... T^a ... ] *)
		dotT[xa___, SUNT[a_SUNIndex], xb___] sunTrace[dotT[xc___, SUNT[a_SUNIndex], xd___]] :>
			1/2 dotT[xa,xd,xc,xb] - 1/(2 SUNN) dotT[xa,xb] sunTrace[dotT[xc,xd]],

		(* Tr[... T^a ...] Tr[ ... T^a ... ] *)
		sunTrace[dotT[xa___, SUNT[a_SUNIndex], xb___]] sunTrace[dotT[xc___, SUNT[a_SUNIndex], xd___]] :>
			1/2 sunTrace[dotT[xa,xd,xc,xb]] - 1/(2 SUNN) sunTrace[dotT[xa,xb]] sunTrace[dotT[xc,xd]]

	};

	ruleSUNTSUNFSUND = {

		(* f^abc T^a *)
		sunf[r1___,a_SUNIndex, r2___] SUNT[a_SUNIndex]  :>
			(I dotT[SUNT[Last[{r2,r1}]] , SUNT[First[{r2,r1}]]] - I dotT[SUNT[First[{r2,r1}]] , SUNT[Last[{r2,r1}]]])/; Length[{r2,r1}]===2,

		(* f^abc (... T^a ...) *)
		sunf[r1___,a_SUNIndex, r2___] dotT[r3___,SUNT[a_SUNIndex],r4___]  :>
			(I dotT[r3,SUNT[Last[{r2,r1}]] , SUNT[First[{r2,r1}]],r4] - I dotT[r3,SUNT[First[{r2,r1}]] , SUNT[Last[{r2,r1}]],r4])/; Length[{r2,r1}]===2,

		(* f^abc Tr(... T^a ...) *)
		sunf[r1___,a_SUNIndex, r2___] sunTrace[dotT[r3___,SUNT[a_SUNIndex],r4___]]  :>
			(I sunTrace[dotT[r3,SUNT[Last[{r2,r1}]] , SUNT[First[{r2,r1}]],r4]] - I sunTrace[dotT[r3,SUNT[First[{r2,r1}]] , SUNT[Last[{r2,r1}]],r4]])/; Length[{r2,r1}]===2,

		(* d^abc T^a *)
		sund[r1___,a_SUNIndex, r2___] SUNT[a_SUNIndex]  :>
			(dotT[SUNT[Last[{r2,r1}]] , SUNT[First[{r2,r1}]]] + dotT[SUNT[First[{r2,r1}]] , SUNT[Last[{r2,r1}]]] -
			(1/SUNN) SUNDeltaContract[r1,r2]) /; Length[{r2,r1}]===2,

		(* d^abc (... T^a ...) *)
		sund[r1___,a_SUNIndex, r2___] dotT[r3___,SUNT[a_SUNIndex],r4___]  :>
			(dotT[r3,SUNT[Last[{r2,r1}]] , SUNT[First[{r2,r1}]],r4] + dotT[r3,SUNT[First[{r2,r1}]] , SUNT[Last[{r2,r1}]],r4] -
			(1/SUNN) SUNDeltaContract[r1,r2] dotT[r3,r4])/; Length[{r2,r1}]===2,

		(* d^abc Tr (... T^a ...) *)
		sund[r1___,a_SUNIndex, r2___] sunTrace[dotT[r3___,SUNT[a_SUNIndex],r4___]]  :>
			(sunTrace[dotT[r3,SUNT[Last[{r2,r1}]] , SUNT[First[{r2,r1}]],r4]] + sunTrace[dotT[r3,SUNT[First[{r2,r1}]] , SUNT[Last[{r2,r1}]],r4]] -
			(1/SUNN) SUNDeltaContract[r1,r2] sunTrace[dotT[r3,r4]])/; Length[{r2,r1}]===2
	};

	SetAttributes[sund,Orderless];

	sund[a_SUNIndex,b_SUNIndex,c_SUNIndex]:=
		0/; (Signature[{a,b,c}] === 0) && FreeQ2[{a,b,c},{Pattern, Blank, BlankSequence, BlankNullSequence}];

	sunf[a_SUNIndex,b_SUNIndex,c_SUNIndex]:=
		0/; (Signature[{a,b,c}] === 0) && FreeQ2[{a,b,c},{Pattern, Blank, BlankSequence, BlankNullSequence}];

	(* properties of f_2:
		f_2^abcd = - f_2^bacd
		f_2^abcd = - f_2^abdc
		f_2^abcd = f_2^cdab

		f_2^aabc = 0
		f_2^abcc = 0

		f_2^abac = N d^bc
		f_2^abcb = N d^ac

		f_2^abca = -N d^bc
		f_2^abbc = -N d^ac

	*)
	sunf2[a_SUNIndex,a_SUNIndex,_,_]:=
		0;
	sunf2[_,_,a_SUNIndex,a_SUNIndex]:=
		0;

	sunf2[a_,b_,c_,d_]:=
		-sunf2[b,a,c,d]/; !OrderedQ[{a,b}] && FreeQ2[{a,b,c,d},{Pattern, Blank, BlankSequence, BlankNullSequence}];
	sunf2[a_,b_,c_, d_]:=
		-sunf2[a,b,d,c]/; !OrderedQ[{c,d}] && FreeQ2[{a,b,c,d},{Pattern, Blank, BlankSequence, BlankNullSequence}];
	sunf2[a_,b_,c_,d_]:=
		-sunf2[c,d,a,b]/; !OrderedQ[{{a,b},{c,d}}] && FreeQ2[{a,b,c,d},{Pattern, Blank, BlankSequence, BlankNullSequence}];

	sunf2[a_SUNIndex,b_,a_SUNIndex,c_]:=
		SUNN SUNDeltaContract[b,c];

	sunf2[a_,b_SUNIndex,c_,b_SUNIndex]:=
		SUNN SUNDeltaContract[a,c];

	sunf2[a_SUNIndex,b_,c_,a_SUNIndex]:=
		- SUNN SUNDeltaContract[b,c];

	sunf2[a_,b_SUNIndex,b_SUNIndex,c_]:=
		- SUNN SUNDeltaContract[a,c];

	(* properties of d_2:
		d_2^abcd =  d_2^bacd
		d_2^abcd =  d_2^abdc
		d_2^abcd = d_2^cdab

		f_2^aabc = 0
		f_2^abcc = 0

		f_2^abac = N d^bc
		f_2^abcb = N d^ac

		f_2^abca = -N d^bc
		f_2^abbc = -N d^ac

	*)

	sund2[a_,b_,c_,d_]:=
		sund2[b,a,c,d]/; !OrderedQ[{a,b}] && FreeQ2[{a,b,c,d},{Pattern, Blank, BlankSequence, BlankNullSequence}];
	sund2[a_,b_,c_, d_]:=
		sund2[a,b,d,c]/; !OrderedQ[{c,d}] && FreeQ2[{a,b,c,d},{Pattern, Blank, BlankSequence, BlankNullSequence}];
	sund2[a_,b_,c_,d_]:=
		sund2[c,d,a,b]/; !OrderedQ[{{a,b},{c,d}}] && FreeQ2[{a,b,c,d},{Pattern, Blank, BlankSequence, BlankNullSequence}];





	ruleSUNFSUNDp1 = {

			(* (f^abc)^2 = 2 CA^2 CF *)
			sunf[_SUNIndex,_SUNIndex,_SUNIndex]^2 :> 2 CA^2 CF,
			(* (d^abc)^2 = - 2 (4 - CA^2) CF *)
			sund[_SUNIndex,_SUNIndex,_SUNIndex]^2 :> -2 (4-CA^2) CF,

			(* f^abc f^abd = N d^{cd} *)
			sunf[r1___,a_SUNIndex,r2___,b_SUNIndex,r3___] sunf[r4___,a_SUNIndex,r5___,b_SUNIndex,r6___] :>
				SUNN (-1)^(Length[{r2}] + Length[{r5}]) SUNDeltaContract[r2,r3,r1,r5,r6,r4],

			sunf[r1___,a_SUNIndex,r2___,b_SUNIndex,r3___] sunf[r4___,b_SUNIndex,r5___,a_SUNIndex,r6___] :>
				SUNN (-1)^(Length[{r2}] + Length[{r5}]+1) SUNDeltaContract[r2,r3,r1,r5,r6,r4],

			(* d^abc d^abd = (N^2-4)/N d^{cd} *)
			sund[a_SUNIndex,b_SUNIndex,c_SUNIndex] sund[a_SUNIndex,b_SUNIndex,d_SUNIndex] :>
				(SUNN^2 - 4)/SUNN SUNDelta[c,d],

			(* f^iab f^icd = f_2^abcd*)
			sunf[r1___, a_SUNIndex, r2___] sunf[r3___, a_SUNIndex, r4____]/; Signature[{r1,r2,r3,r4}]=!=0 :> sunf2[r2,r1,r4,r3],

			(* d^iab d^icd = d_2^abcd*)
			sund[r1___, a_SUNIndex, r2___] sund[r3___, a_SUNIndex, r4____]/; Signature[{r1,r2,r3,r4}]=!=0 :> sund2[r2,r1,r4,r3],

			(* f^abc d^abe = 0*)
			sunf[___,a_SUNIndex,___,b_SUNIndex,___] sund[a_SUNIndex,b_SUNIndex,_] :> 0

	};

	ruleSUNFSUNDp2 = {

			(* f_2^abcd f^abe = N f^ecd *)
			sunf2[a_SUNIndex, b_SUNIndex, c_, d_] sunf[r1___,a_SUNIndex,r2___,b_SUNIndex,r3___] :>
			SUNN (-1)^(Length[{r2}]) sunf[r2,r3,r1,c,d],

			(* f_2^bacd f^abe = - N f^ecd *)
			sunf2[b_SUNIndex, a_SUNIndex, c_, d_] sunf[r1___,a_SUNIndex,r2___,b_SUNIndex,r3___] :>
			SUNN (-1)^(Length[{r2}]+1) sunf[r2,r3,r1,c,d],

			(* f_2^abcd f^cde = N f^eab *)
			sunf2[c_, d_, a_SUNIndex, b_SUNIndex] sunf[r1___,a_SUNIndex,r2___,b_SUNIndex,r3___] :>
			SUNN (-1)^(Length[{r2}]) sunf[r2,r3,r1,c,d],

			(* f_2^abdc f^cde = - N f^eab *)
			sunf2[c_, d_, b_SUNIndex, a_SUNIndex] sunf[r1___,a_SUNIndex,r2___,b_SUNIndex,r3___] :>
			SUNN (-1)^(Length[{r2}]+1) sunf[r2,r3,r1,c,d],


			(* f_2^abcd f^ace = N/2 f^bde *)
			sunf2[a_SUNIndex, b_, c_SUNIndex, d_] sunf[r1___,a_SUNIndex,r2___,c_SUNIndex,r3___] :>
			SUNN/2 (-1)^(Length[{r2}]) sunf[b,d,r2,r3,r1],

			(* f_2^cbad f^ace = - N/2 f^bde *)
			sunf2[c_SUNIndex, b_, a_SUNIndex, d_] sunf[r1___,a_SUNIndex,r2___,c_SUNIndex,r3___] :>
			SUNN/2 (-1)^(Length[{r2}]+1) sunf[b,d,r2,r3,r1],

			(* f_2^abdc f^ace = - N/2 f^bde *)
			sunf2[a_SUNIndex, b_, d_, c_SUNIndex] sunf[r1___,a_SUNIndex,r2___,c_SUNIndex,r3___] :>
			SUNN/2 (-1)^(Length[{r2}]+1) sunf[b,d,r2,r3,r1],

			(* f_2^cbda f^ace = - N/2 f^bde *)
			sunf2[c_SUNIndex, b_, d_, a_SUNIndex] sunf[r1___,a_SUNIndex,r2___,c_SUNIndex,r3___] :>
			SUNN/2 (-1)^(Length[{r2}]) sunf[b,d,r2,r3,r1],

			(* f_2^bacd f^ace = - N/2 f^bde *)
			sunf2[b_, a_SUNIndex, c_SUNIndex, d_] sunf[r1___,a_SUNIndex,r2___,c_SUNIndex,r3___] :>
			SUNN/2 (-1)^(Length[{r2}]+1) sunf[b,d,r2,r3,r1],

			(* f_2^bcad f^ace = - N/2 f^bde *)
			sunf2[b_, a_SUNIndex, c_SUNIndex, d_] sunf[r1___,a_SUNIndex,r2___,c_SUNIndex,r3___] :>
			SUNN/2 (-1)^(Length[{r2}]) sunf[b,d,r2,r3,r1],

			(* f_2^badc f^ace = - N/2 f^bde *)
			sunf2[b_, a_SUNIndex, d_, c_SUNIndex] sunf[r1___,a_SUNIndex,r2___,c_SUNIndex,r3___] :>
			SUNN/2 (-1)^(Length[{r2}]) sunf[b,d,r2,r3,r1],

			(* f_2^bcda f^ace = - N/2 f^bde *)
			sunf2[b_, c_SUNIndex, d_, a_SUNIndex] sunf[r1___,a_SUNIndex,r2___,c_SUNIndex,r3___] :>
			SUNN/2 (-1)^(Length[{r2}]+1) sunf[b,d,r2,r3,r1],

			(*  d_2^abcl + d_2^alcb + d_2^acbl = 2(N^2-4)/(N(N^2+1)) *)
			coeff_. sund2[a_,b_,c_,l_] + coeff_. sund2[a_,l_,c_,b_] + coeff_. sund2[a_,c_,b_,l_] :>
				(2 (-4 + SUNN^2))/(SUNN + SUNN^3) coeff *
				(SUNDelta[a,b] SUNDelta[c,l] + SUNDelta[a,c] SUNDelta[b,l] + SUNDelta[a,l] SUNDelta[b,c])



	};












	sunsi3 = {
				(* f^abc T^a T^b T^c + permutations *)

				sunf[r1___, zz_SUNIndex, r2___, yy_SUNIndex, r3___] dotT[aa___, SUNT[xx_SUNIndex], SUNT[yy_SUNIndex], SUNT[zz_SUNIndex], bb___] :>
					(*(-1)^(Length[{r2}]+1) sunf[r3, r1, r2, yy, zz] dotT[aa, SUNT[xx], SUNT[yy], SUNT[zz], bb]*)
					(-1)^(Length[{r2}]+1) I/2 CA CF dotT[aa,bb] /; {r3,r1,r2} === {xx} && zz=!=yy && xx=!=yy  && zz=!=yy,


				sunf[r1___, yy_SUNIndex, r2___, zz_SUNIndex, r3___] dotT[aa___, SUNT[xx_SUNIndex], SUNT[yy_SUNIndex], SUNT[zz_SUNIndex], bb___] :>
					(*(-1)^(Length[{r2}]) sunf[r3, r1, r2, yy, zz] dotT[aa, SUNT[xx], SUNT[yy], SUNT[zz], bb]*)
					(-1)^(Length[{r2}]) I/2 CA CF dotT[aa,bb] /; {r3,r1,r2} === {xx} && zz=!=yy && xx=!=yy  && zz=!=yy
			};

			sunsi2 = {
				(* f^abc T^a T^b + permutations *)

				sunf[r1___, zz_SUNIndex, r2___, yy_SUNIndex, r3___] dotT[aa___, SUNT[yy_SUNIndex], SUNT[zz_SUNIndex], bb___] :>
					(* (-1)^(Length[{r2}]+1) sunf[r3, r1, r2, yy, zz] dotT[aa, SUNT[yy], SUNT[zz], bb] *)
					(-1)^(Length[{r2}]+1) I/2 SUNN dotT[aa,SUNT[r3, r1, r2],bb]/; yy=!=zz,

				sunf[r1___, yy_SUNIndex, r2___, zz_SUNIndex, r3___] dotT[aa___, SUNT[yy_SUNIndex], SUNT[zz_SUNIndex], bb___] :>
					(* (-1)^(Length[{r2}]) sunf[r3, r1, r2, yy, zz] dotT[aa, SUNT[yy], SUNT[zz], bb] *)
					(-1)^(Length[{r2}]) I/2 SUNN dotT[aa,SUNT[r3, r1, r2],bb]/; yy=!=zz
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
					sunf[r1, i, r2, j, r3] dotT[xx, SUNT[a], SUNT[i], d, SUNT[j], e])/; FreeQ2[{a,d},{r1,r2,r3,i,j}] && i=!=j,


				sunf[r1___, j_SUNIndex, r2___, i_SUNIndex, r3___] dotT[xx___, SUNT[i_SUNIndex], SUNT[a_SUNIndex], d:SUNT[_SUNIndex]... , SUNT[j_SUNIndex], e___] :>
					(dummy=Unique["cc"];
					I sunf[r1, j, r2, i, r3] sunf[i,a,SUNIndex[dummy]]  DOT[xx, SUNT[SUNIndex[dummy]], d, SUNT[j], e] +
					sunf[r1, j, r2, i, r3] dotT[xx, SUNT[a], SUNT[i], d, SUNT[j], e])/; FreeQ2[{a,d},{r1,r2,r3,i,j}] && i=!=j


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

(* Description of the algorithm:

	1) 	Perform color contractions by eliminating all the Kronecker deltas with
		color indices contracted to other colored objects. After this step only
		SUNDelta and SUNDelta with free indices may remain

	2) 	Apply Cvitanonic untill all color matrices with common adjoint indices are
		eliminated. Deeply nested matrices or traces require special handling and
		might lead to warnings, e.g. MyFunc[SUNT[a,b]]SUNT[a,b]

	3) 	Apply known rules for simplifying products of structure constatns. The goal is to
		eliminate as many structure constatns with commond indices as possible. In general,
		not everything can be eliminated. For example, in SUNF[a,b,r]SUNF[c,d,r] we cannot
		completely eliminate the dummy index r. At most we can trade it for similar combinations
		of SUNDs. The actual behavior (if SUNFs should be traded for SUNDs or vice versa) must be
		controlled via options.

	4)	Eliminate all contractions of structure constants with color matrices.
		After this step no SUND/SUNF having an index common with a SUNT should remain

	5) 	The previous step might introduce new color matrices with commond adjoiunt indices.
		Thus, apply Cvitanovic again. After this step the only colored objects that might
		have common color indices are structure constants.

	6) 	Up to irreducible SUNF/SUND combinations we end up with SUNTs, SUNDs, SUNFs and SUNTraces
		that contain objects with distinct indices. Now one could evaluate SUNTraces, which again
		should be controlled by an option.

	7) 	If SUNTraces are evaluated, we get even more SUNDs and SUNFs. However, the previous steps guarantee
		that those have no common indices with already existing SUNDs/SUNFs. The resulting expressions also
		do not contain any SUNTs, so at this point we only need to apply rules for simplifying products of
		structure constatns. So we are left only with SUNT-chains, SUNDs and SUNFs. SUNT chains consist of
		all free indices. Some of SUNDs and SUNFs however have some common indices, as it is impossible to
		eliminate those completely.



*)


(* Main entry point *)
(*
SUNSimplify[x_, opts:OptionsPattern[]] :=
	FixedPoint[sunsimp[#, Flatten[Join[{opts}, FilterRules[Options[SUNSimplify], Except[{opts}]]]]]&, x, 6] /. dtr -> DiracTrace;
*)

SUNSimplify[x_, opts:OptionsPattern[]] :=
	sunsimp[x, Flatten[Join[{opts}, FilterRules[Options[SUNSimplify], Except[{opts}]]]]];

Options[sunsimp] = Options[SUNSimplify];

sunsimp[expr_, opts:OptionsPattern[]] :=
	Block[ {sunntocacf, ex, temp, explicit, sunf, suntraceoption,surule, diractr,doot,
		expan, expanding, factoring,ntemp, tfac = 1, time, sunsiIso, uniqueExpressions,
		simplifiedExpressions,finalRepRule,originalInput},

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

		(* TODO: First isolate, then canonicalize!!! *)
		(*If[ Head[temp] === Times,
			tfac = Select[temp,  FreeQ2[#, {SUNIndex, SUNFIndex, SUNN, CA, CF}]&];
			temp = Select[temp, !FreeQ2[#, {SUNIndex, SUNFIndex, SUNN, CA, CF}]&];
		];*)


		(* It is better to canonicalize the indices at the very beginning. FCCanonicalizeDummyIndices can handle this automatically*)
		If[ OptionValue[SUNSimplify,{opts},SUNIndexRename],
				time=AbsoluteTime[];
				FCPrint[1, "SUNSimplify: Renaming.", FCDoControl->sunSiVerbose];
				temp = FCCanonicalizeDummyIndices[temp, FCI -> True, Head ->{SUNIndex,SUNFIndex}];
				FCPrint[1, "SUNSimplify: Renaming done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->sunSiVerbose];
				FCPrint[3, "SUNSimplify: After renaming, ", temp, FCDoControl->sunSiVerbose];
		];

		(* Isolate everything except for the color structures that we are interested in*)
		time=AbsoluteTime[];
		FCPrint[1, "SUNSimplify: Collecting terms w.r.t colored objects.", FCDoControl->sunSiVerbose];
		temp = FCColorIsolate[temp, FCI->True,Isolate->True, IsolateFast->True, IsolateNames->sunsiIso,Head->sunObj,ClearHeads->{sunObj}];
		FCPrint[1,"SUNSimplify: collecting done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->sunSiVerbose];
		FCPrint[3, "SUNSimplify: After collecting terms w.r.t colored objects: ",temp, FCDoControl->sunSiVerbose];
		originalInput = temp;
		uniqueExpressions = Cases2[temp, sunObj];


		(* The show begins *)
		simplifiedExpressions = uniqueExpressions /. sunObj->Identity;

		(* SUNFIndex-contractions *)
		(* TODO For performance reasons SUNFSimplify should be better done internally *)
		If[ !FreeQ[simplifiedExpressions, SUNFIndex],
			time=AbsoluteTime[];
			FCPrint[1, "SUNSimplify: Applying SUNFSimplify.", FCDoControl->sunSiVerbose];
			simplifiedExpressions = Map[SUNFSimplify[#, SUNNToCACF->sunntocacf, Explicit->explicit],simplifiedExpressions];
			FCPrint[1, "SUNSimplify: Done applying SUNFSimplify, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->sunSiVerbose];
		];

		(* SUNIndex-contractions *)
		time=AbsoluteTime[];
		FCPrint[1, "SUNSimplify: Applying SUNDeltaContract.", FCDoControl->sunSiVerbose];
		simplifiedExpressions = simplifiedExpressions /. SUNDelta -> SUNDeltaContract/. SUNF[a_,b_,c_,d_SUNIndex] :> SUNF[a,b,c,d, Explicit->True];
		FCPrint[1, "SUNSimplify: Done applying SUNDeltaContract, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->sunSiVerbose];
		FCPrint[3, "SUNSimplify: After  SUNDeltaContract", simplifiedExpressions, FCDoControl->sunSiVerbose];


		(* Now the expression should be free of SUNDelta/SUNFDelta with dummy indices *)


		(* First of all, let us get rid of SUN traces *)
		simplifiedExpressions = Map[FixedPoint[Function[farg,Expand2[farg,SUNTrace]],#]&,simplifiedExpressions];



		simplifiedExpressions = simplifiedExpressions /. SUNTrace -> sunTrace /. DOT -> dotT /. SUNF -> sunf /. SUND -> sund;

		(* Next step: Cvitanovic *)
		simplifiedExpressions = Map[FixedPoint[Function[farg,ReplaceRepeated[Expand2[farg,{suntTrace,dotT,SUNT,sunf,sund}],ruleCvitanovic]],#]&,simplifiedExpressions];

		(* Next step: f^abc T^a and d^abc T^a *)
		simplifiedExpressions = Map[FixedPoint[Function[farg,ReplaceRepeated[ReplaceRepeated[Expand2[farg,{suntTrace,dotT,SUNT,sunf,sund}],ruleSUNFSUNDp1],ruleSUNFSUNDp2]],#]&,simplifiedExpressions];

		(* Next step: Cvitanovic again *)
		simplifiedExpressions = Map[FixedPoint[Function[farg,ReplaceRepeated[Expand2[farg,{suntTrace,dotT,SUNT,sunf,sund}],ruleCvitanovic]],#]&,simplifiedExpressions];

		(* Next step: Contractions of structure constants *)
		simplifiedExpressions = Map[FixedPoint[Function[farg,ReplaceRepeated[Expand2[farg,{suntTrace,dotT,SUNT,sunf,sund}],ruleSUNTSUNFSUND]],#]&,simplifiedExpressions];

		(* Next step: Cvitanovic again *)
		simplifiedExpressions = Map[FixedPoint[Function[farg,ReplaceRepeated[Expand2[farg,{suntTrace,dotT,SUNT,sunf,sund}],ruleCvitanovic]],#]&,simplifiedExpressions];



		(*simplifiedExpressions //. {
			dotT[xx___,SUNT[a_SUNIndex],y___]
			};*)

		simplifiedExpressions = simplifiedExpressions /. sunf -> SUNF /. sund -> SUND /. dotT[] -> 1 /. dotT -> DOT /. sunTrace -> SUNTrace /. SUNDeltaContract -> SUNDelta;
		finalRepRule = Thread[Rule[uniqueExpressions, simplifiedExpressions]];
		(*Print[finalRepRule];*)

		temp = originalInput /. finalRepRule /. sunObj->Identity;

		temp = FRH[temp,IsolateNames->sunsiIso];

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

		Return[temp];


		If[ !FreeQ[simplifiedExpressions, SUNIndex] || !FreeQ[simplifiedExpressions, SUNN] || suntraceoption,

			If[ explicit,
				sunfL[a__] :=
					SUNF[a, Explicit -> True];

				sundL[a__] :=
					SUND[a, Explicit -> True],

				sundL[a__] :=
					SUND[a];
				sunfL[a__] :=
					SUNF[a]
			];

			time=AbsoluteTime[];
			FCPrint[1, "SUNSimplify: Applying simplification rules.", FCDoControl->sunSiVerbose];
			simplifiedExpressions = FixedPoint[Expand2[(# /. SUNTrace -> sunTrace /. DOT -> dotT /. SUNF -> sunf //. sunsi //. sunsi3 //. sunsi2 //. sunsi1 //. sunsi /. dotT[] -> 1 //.
				{dotT[a___,1,b___] :> dotT[a,b]} /. dotT[] -> 1),{SUNF,SUNT,sunTrace}]&, simplifiedExpressions]/. dotT -> DOT /. sunf ->SUNF /. sunTrace ->SUNTrace;




			FCPrint[1, "SUNSimplify: Done applying simplification rules, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->sunSiVerbose];
			FCPrint[3, "SUNSimplify: After applying simplification rules, ", simplifiedExpressions, FCDoControl->sunSiVerbose];

			finalRepRule = Thread[Rule[uniqueExpressions, simplifiedExpressions]];
			temp = originalInput /. finalRepRule /. sunObj->Identity;


			If[ OptionValue[SUNSimplify,{opts},SUNIndexRename],
				time=AbsoluteTime[];
				FCPrint[1, "SUNSimplify: Renaming.", FCDoControl->sunSiVerbose];
				temp = Rename[temp, Expanding -> expanding];
				FCPrint[1, "SUNSimplify: Renaming done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->sunSiVerbose];
				FCPrint[3, "SUNSimplify: After renaming, ", temp, FCDoControl->sunSiVerbose];
			];



			If[ !FreeQ[temp, DiracTrace],
				If[ suntraceoption,
					surule = {(* Added 4/9-2002. Frederik Orellana.
								Expressions without SUNT
								(proportional to the identity matrix)
								were not SUNTrace'd *)
							diractr[dd_Times , dops___Rule] :>
								SUNTrace[SelectNotFree[dd, SUNIndex]] dtr[SelectFree[dd, SUNIndex], dops] /; FreeQ[dd, SUNT[___]],
							diractr[dd_?((Head[#]=!=Times)&) , ___Rule] :>
								SUNTrace[dtr[dd]]/; FreeQ[dd, SUNT[___]],
							(*Added Times to avoid SelectNotFree[a+b,SUNIndex] --> 0*)
							diractr[doot[xx__sunt] dd_. , dops___Rule] :>
								SUNTrace[DOT[xx] SelectNotFree[dd, SUNIndex]] DiracTrace[SelectFree[dd, SUNIndex], dops],
							diractr[doot[xx__sunt, y__] dd_., dops___Rule] :>
								SUNTrace[DOT[xx] SelectNotFree[dd, SUNIndex]] DiracTrace[doot[y] SelectFree[dd, SUNIndex], dops ] /; FreeQ[{y}, SUNIndex],

							diractr[doot[SUNT[_], dd_], ___Rule] :>
								0 /; FreeQ2[dd,{SUNIndex,SUNT}],
							diractr[SUNT[_]  dd_., ___Rule] :>
								0 /; FreeQ2[dd,{SUNIndex,SUNT}]
							},
					surule = {diractr[doot[xx__sunt] dd_. , dops___Rule] :>
								DOT[xx] SelectNotFree[dd, SUNIndex] DiracTrace[SelectFree[dd, SUNIndex], dops],
							diractr[doot[xx__sunt, y__] dd_. , dops___Rule] :>
								DOT[xx] SelectNotFree[dd, SUNIndex] DiracTrace[doot[y] SelectFree[dd, SUNIndex], dops] /; FreeQ[{y}, SUNIndex]
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
				expan = Expand2[#, SUNIndex]&
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
		temp = FRH[temp,IsolateNames->sunsiIso]/. sunObj -> Identity;
		(*temp = Collect2[temp,{SUNT,SUNDelta}];*)
		temp
	];

FCPrint[1,"SUNSimplify.m loaded."];
End[]
