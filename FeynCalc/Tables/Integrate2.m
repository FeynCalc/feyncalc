(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Integrate2 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 10 December '98 at 20:35 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  Integrate2 is like Integrate, but better *)

(* ------------------------------------------------------------------------ *)

Integrate2::usage=
"Integrate2 is like Integrate, but Integrate2[a_Plus, b__] := Map[Integrate2[#,
b]&, a] ( more linear algebra and partial fraction decomposition is done)

Integrate2[f[x] DeltaFunction[x], x] -> f[0]

Integrate2[f[x] DeltaFunction[x0-x], x] -> f[x0]

Integrate2[f[x] DeltaFunction[a + b x], x] -> Integrate[f[x] (1/Abs[b])
DeltaFunction[a/b + x], x], where Abs[b] -> b, if b is a symbol, and if b =
-c, then Abs[-c] -> c, i.e., the variable contained in b is supposed to be
positive.

 $\\pi ^2$ is replaced by 6 Zeta2.

Integrate2[1/(1-y),{y,x,1}] is interpreted as distribution, i.e. as
Integrate2[-1/(1-y)],{y, 0, x}] -> Log[1-y].

Integrate2[1/(1-x),{x,0,1}] -> 0

Since Integrate2 does do a reordering and partial fraction decomposition
before calling the integral table of Integrate3, it will in general be slower
compared to Integrate3 for sums of integrals. I.e., if the integrand has
already an expanded form and if partial fraction decomposition is not
necessary it is more effective to use Integrate3.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Integrate2`Private`"]

Options[Integrate2] = {Table:>{}};

Integrate2[a_, b_List, c__List,opt___Rule] :=
	Integrate2[Integrate2[a,b], c,opt] /; FreeQ[SelectNotFree[a, b[[1]]],
																					DeltaFunction];

polylogc[_][nm_,z_] :=
							If[FreeQ[z, Plus], PolyLog[nm,z],
									PolyLog[nm, Factor2[z]]
								];


logc[yai_][z_] := Block[{in},
												If[FreeQ[z, Plus], Log[z],
												in = Factor2[z];
												If[Head[in] === Times, Log[in],
													Log[Collect2[Expand[z],yai,
																Denominator->True, Expanding->False]
															]
													]
													]
											];

(*
niel[a__] := (niel[a] = Nielsen[a, PolyLog->True]) /; !MatchQ[First[{a}],_^2];
*)

niel[1,2, xyz_^2] :=  -(Zeta2*Log[1 - xyz]) + Log[1 - xyz]^2*Log[xyz] -
	3*Zeta2*Log[1 + xyz] - Log[xyz]*Log[1 + xyz]^2 + (2*Log[1 + xyz]^3)/3 +
	2*Log[1 - xyz]*PolyLog[2, 1 - xyz] + 2*Log[1 + xyz]*PolyLog[2, 1 - xyz] -
	2*Log[1 - xyz]*PolyLog[2, -xyz] - 2*Log[1 + xyz]*PolyLog[2, -xyz] -
	4*PolyLog[3, 1 - xyz] - 4*PolyLog[3, (1 + xyz)^(-1)] -
	2*PolyLog[3, -((1 - xyz)/(1 + xyz))] + 2*PolyLog[3, (1 - xyz)/(1 + xyz)] +
	(9*Zeta[3])/2;

niel[other__] := Nielsen[other];

Integrate2[a_, b_, c___List,opt___Rule] :=
If[FreeQ[a, DOT],
	iIntegrate2[a/.Nielsen->niel, b, c, opt],
	iIntegrate2[(a/.Nielsen->niel)//Trick, b, c, opt]
	];
simplifypoly[z_] :=
(
(* Print["BLA"];  *)
If[FreeQ[z,Integrate3],
	SimplifyPolyLog[ z /. Nielsen -> niel ]/.Nielsen->niel
	,
	z/. (hold_[Integrate3])[aa_,b_]:>
(*
				Expand[Integrate3[Expand[aa/.Nielsen->niel],b]]
*)
				Expand[Integrate3[Expand[SimplifyPolyLog[aa]/.Nielsen->niel],b]
							]  /; hold === Hold
	]);



(*
simplifypoly[z_] :=
If[FreeQ[z,Integrate3], SimplifyPolyLog[z](*//SimplifyPolyLog*),z];
*)

iIntegrate2[a_, b_, c___List,opts___?OptionQ] :=
(* FISHY *)
Expand2[
simplifypoly[
simplifypoly[
(
(*
If[{opt} =!= {} && {opt} =!= {Assumptions ->{True}},
	SetOptions@@Prepend[Integrate, {opt}]
	];
*)
PowerSimplify[
Expand[Factor2[
														If[FreeQ[a, Plus], integrateD[a, b, c, opts],
															If[Head[b] =!= List,
																integrateD[a, b, c, opts],
																integrateD[
Expand[
Collect2[
										If[!FreeQ[a, b[[1]]^ww_/;Head[ww] =!= Integer],
												a, Apart[a,b[[1]]]
											], b[[1]],Denominator->True
																				] /. {
														Log :> logc[b[[1]]],
												PolyLog :> polylogc[b[[1]]]
																						} /.
												Log[uu_ - uu_^2] :> Log[1-uu] + Log[uu]
			,Log]
				, b, c, opts
																					]
																]
															] ] /.
															{Pi^2 :> (6 Zeta2),
															Zeta2^2 :> Zeta[2]^2,
															PolyGamma[2,1] :> (-2 Zeta[3])
															}/.(*  Integrate -> Integrate3 /.*)
															(Hold@@{Integrate3}) ->
															Integrate4]]
)/.Hold[Integrate] -> Integrate3]], First[Flatten[{b}]]];

integrateD[a_Plus,b__] := Map[integrateD[#,b]&, a];
integrateD[a_, b_, c___] := MemSet[integrateD[a,b,c],
Block[{i3, tt, nop, n1, n2},
If[Head[i3 = Integrate3[a, b, c] ] =!=
			(Hold @@ {Integrate3}), i3,
		integrate2[a, b, c]
]]                                ];

getx[bb_] := If[Head[bb] === List, bb[[1]], bb];

integrate2[a_, b_, c___] :=
integrate3[
If[FreeQ[a, DeltaFunction], Expand[a, getx[b]],
		Expand[a, DeltaFunction]
	], b, c];


integrate3[a_Plus, b_, c___] :=  Map[Integrate2[#, b, c]&, a];


integrate3[a_, b_, c___] := (If[Head[b] === List,
									SelectFree[a, b[[1]]] *
									Integrate3[SelectNotFree[a, b[[1]]], b, c
														] /. (Hold@@{Integrate3})-> Integrate4,
									SelectFree[a, b] *
									Integrate3[SelectNotFree[a, b], b
														] /. (Hold@@{Integrate3})-> Integrate4
								] // PowerSimplify
							) /; ((Head[a] =!= Plus) && FreeQ[a, DeltaFunction]);

integrate3[a_] := a;
HoldPattern[integrate3[f_. de_[x_], {x_, x0_, _}]]:=
(f /. x -> 0) /;( x0 >= 0)  && (de === DeltaFunction);
Integrate2[w_] := w;

Integrate2[f_. DeltaFunction[1-x_], {x_,x0_, _}] :=
(f /. x -> 1) /; x0 >= 0;

Integrate2[f_. DeltaFunction[x_], {x_,x0_, _}] :=
	(f /. x -> 0) /; x0 >= 0;

Integrate2[f_. DeltaFunction[x_ + y_], {x_,_, _}] :=
(f /. x -> -y) /; FreeQ[y,x];

(*The two lines below added 2/5-2001. F.Orellana*)
Integrate2[f_. DeltaFunction[a_*x_ + y_], {x_,_, _}] :=
(f/Abs[a] /. x -> -y) /; FreeQ[y,x];

abs[h_Symbol] := h;
abs[- h_Symbol] := h;

FCPrint[1,"Integrate2.m loaded."];
End[]
