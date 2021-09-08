(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Integrate5 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 13 April '98 at 11:12 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  Integrate5 is like Integrate, but better *)

(* ------------------------------------------------------------------------ *)

Integrate5::usage=
"Integrate5 is an alternative implementation along the lines of Integrate2.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Integrate5`Private`"]

sing::usage="";

Integrate5[a_, b_List, c__List, opts___?OptionQ] :=
	Integrate5[Integrate5[a,b, opts], c, opts] /; FreeQ[SelectNotFree[a, b[[1]]], DeltaFunction];

(*
logsimp = { a_. Log[1-x_] - a_. Log[x_] :> (a Log[(1-x)/x])};
*)


Integrate5[a_, b_, c___] := (
														If[FreeQ[a, Plus], integrateD[a, b, c],
(*
															If[Head[b] =!= List,
*)
																integrateD[a, b, c],Null
(*
																integrateD[Collect2[a, b], b, c],
																integrateD[Collect2[a,b[[1]]], b, c]
																]
*)
															] ) /. Pi^2 :> (6 Zeta2)(* /.
															Integrate -> Integrate3 /. Integrate3 ->
															Integrate *);

integrateD[a_, b_, c___] :=
	Block[{	i3, tt, nop, n1, n2,pd},
		If[Head[i3 = Integrate3[a, b, c]] =!= Integrate3,
			i3,
			If[	FreeQ[a, PlusDistribution],
				(*
				Dialog[a];
				*)
				integrate2[a, b, c],
				(*
				Dialog[a];
				*)
				tt = Collect2[a, PlusDistribution, Factoring -> True];
				FCPrint[2,"integrating ",tt//InputForm];
				nop = SelectFree[n1 + n2 + tt, PlusDistribution]/.{n1:>0, n2:>0};
				pd = tt - nop;
				pd = Collect2[
				If[	Head[pd] === Plus,
					Map[intDistribution[#, b, c]&, pd],
					intDistribution[pd,b,c]
				] + integrate2[nop, b, c], {Log, PolyLog}];
				pd
			]
		]
	];

(* 10 *)
intDistribution[ PlusDistribution[1/(1-x2_)] *
								Log[1-x1_] DeltaFunction[x_ - (x1_ x2_)],
								{x1_,0,1},{x2_,0,1}
							] := -Log[x] Log[1-x] + Log[1-x]^2 - Zeta2;

(* 11 *)
intDistribution[ PlusDistribution[Log[1-x2_]/(1-x2_)] *
								DeltaFunction[x_ - (x1_ x2_)],
								{x1_,0,1},{x2_,0,1}
							] := PolyLog[2, x] - Zeta2 + 1/2 Log[1-x]^2;

(* 12 *)
intDistribution[ PlusDistribution[1/(1-x2_)] *
								x1_ Log[1-x1_] DeltaFunction[x_ - (x1_ x2_)],
								{x1_,0,1},{x2_,0,1}
							] := (1-x) Log[1-x] - (1-x) - x Log[x] Log[1-x] +
										x Log[1-x]^2 - x Zeta2;

(* 13 *)
intDistribution[ PlusDistribution[Log[1-x2_]/(1-x2_)] *
								x1_ *
								DeltaFunction[x_ - (x1_ x2_)],
								{x1_,0,1},{x2_,0,1}
							] := (1-x) Log[1-x] + x Log[x] + x PolyLog[2, x] -
										x Zeta2 + x/2 Log[1-x]^2;
(* 16 *)
intDistribution[ PlusDistribution[1/(1-x2_)] *
								Log[x1_]/(1-x1_) DeltaFunction[x_ - (x1_ x2_)],
								{x1_,0,1},{x2_,0,1}
							] := 1/(1-x1) ( Log[x] Log[1-x] - 1/2 Log[x]^2);


(* 19 *)
intDistribution[ PlusDistribution[1/(1-x2_)] *
								Log[x1_] DeltaFunction[x_ - (x1_ x2_)],
								{x1_,0,1},{x2_,0,1}
							] := -1/2 Log[x]^2 + Zeta2 - PolyLog[2,x];

(* 21 *)
intDistribution[ PlusDistribution[1/(1-x2_)] *
								x1_ Log[x1_] DeltaFunction[x_ - (x1_ x2_)],
								{x1_,0,1},{x2_,0,1}
							] := -x Log[x] - (1-x) - x/2 Log[x]^2 +
										x Zeta2 - x PolyLog[2,x];


intDistribution[f_. PlusDistribution[nu_ /(1-x1_)] *
								DeltaFunction[x_ - (x1_ x2_)], {x1_,0,1},{x2_,0,1}
							] := integrate1[1/x1 (f/.x2 -> (x/x1)) *
															PlusDistribution[nu/(1-x1)], {x1,x,1}
															] /; FreeQ[f, PlusDistribution];
intDistribution[f_. PlusDistribution[nu_ /(1-x2_)] *
								DeltaFunction[x_ - (x1_ x2_)], {x1_,0,1},{x2_,0,1}
							] := integrate1[1/x2 (f/.x1 -> (x/x2)) *
															PlusDistribution[nu/(1-x2)], {x2,x,1}
															] /; FreeQ[f, PlusDistribution];

intDistribution[f_. PlusDistribution[1/(1-x1_)] *
								DeltaFunction[x_ - (x1_ x2_)], {x1_,0,1},{x2_,0,1}
							] := integrate1[1/x1 (f/.x2 -> (x/x1)) *
															PlusDistribution[1/(1-x1)], {x1,x,1}
															] /; FreeQ[f, PlusDistribution];
intDistribution[f_. PlusDistribution[1/(1-x2_)] *
								DeltaFunction[x_ - (x1_ x2_)], {x1_,0,1},{x2_,0,1}
							] := integrate1[ 1/x2 (f/.x1 -> (x/x2)) *
															PlusDistribution[1/(1-x2)],
															{x2,x,1}
															] /; FreeQ[f, PlusDistribution];


(* TABLE *)
(* two distributions *)

intDistribution[f_. PlusDistribution[1/(1-x1_)] *
										PlusDistribution[1/(1-x2_)] *
								DeltaFunction[x_ - (x1_ x2_)], {x1_,0,1},{x2_,0,1}
							] := (2 PlusDistribution[Log[1-x]/(1 - x)] -
											PlusDistribution[Log[x]/(1-x)]  -
										Zeta2 DeltaFunction[1 - x]
										) /; FreeQ2[f, {x1, x2}];

intDistribution[_. PlusDistribution[1/(1-x1_)] *
										PlusDistribution[Log[1-x2_]/(1-x2_)] *
								DeltaFunction[x_ - (x1_ x2_)], {x1_,0,1},{x2_,0,1}
							] := -Log[x] Log[1-x]/(1-x) + 3/2 Log[1-x]^2/(1-x) -
										Zeta2/(1-x) + DeltaFunction[1-x] Zeta[3];


(* one distribution *)

integrate1[f_.  PlusDistribution[1 / (1-x1_)], {x1_, 0 ,1}]:=
integrate2[Factor2[(f - (f /. x1 -> 1)) / (1 - x1)
									], {x1,0,1}
					];

integrate1[f_. PlusDistribution[Log[_] / (1-x1_)], {x1_,0,1}
					] := 0 /; FreeQ[f, x1];

integrate1[Log[1 - z_ / x1_]  PlusDistribution[1/(1 - x1_)],
					{x1_,z_,1}] := -Zeta2 + PolyLog[2,1-z];

integrate1[f_  PlusDistribution[Log[y_] / (1-x1_)],
					{x1_,x_ /; x=!=0,1}
					]:=
Block[{tt, normal, null},
				tt = Map[Factor2, Apart[f Log[y]/(1-x1), x1]];
				normal = SelectFree[tt + null, (1-x1)^(-1)] /. null -> 0;
				sing   = tt - normal;
				integrate2[normal, {x1,x,1}] +
				integrate1[sing, {x1,0,1}]
			];

integrate1[f_  PlusDistribution[1 / (1-x1_)],
					{x1_,x_ /; x=!=0,1}
					]:=
Block[{tt, normal, null},
				tt = Map[Factor2, Apart[f 1/(1-x1), x1]];
				normal = SelectFree[tt + null, (1-x1)^(-1)] /. null -> 0;
				sing   = tt - normal;
				integrate2[normal, {x1,x,1}] +
(*
				integrate1[Factor2[sing (1-x1)] *
									PlusDistribution[1 / (1-x1)],
									{x1,x,1}
									] +
				(f /. x1 -> 1) Log[1-x]
*)
				integrate1[Factor2[sing (1-x1)] *
									PlusDistribution[1 / (1-x1)],
									{x1,0,1}
									] -
				integrate2[sing, {x1,0,x}]
			];
(*
/; Factor2[Apart[f 1/(1-x1), x1]] =!= Factor2[f/(1-x1)];
*)

getx[bb_] := If[Head[bb] === List, bb[[1]], bb];

integrate2[a_, b_, c___] :=
integrate3[
If[FreeQ[a, DeltaFunction],
		Collect2[a, getx[b], Denominator->True],
		Collect2[a, DeltaFunction]
	], b, c];

integrate3[a_Plus, b_, c___] :=  Map[Integrate5[#, b, c]&, a];

integrate3[a_ /;(  (Head[a] =!= Plus) && FreeQ[a, DeltaFunction] ),
					b_, c___
					] := If[Head[b] === List,
									SelectFree[a, b[[1]]] *
									Integrate3[SelectNotFree[a, b[[1]]], b, c
														] /. Integrate3 -> Integrate,
									SelectFree[a, b] *
									Integrate3[SelectNotFree[a, b], b
														] /. Integrate3 -> Integrate
								];

integrate3[a_] := a;
integrate3[f_. DeltaFunction[x_], x_, y___] :=
integrate1[f /. x -> 0, y];
integrate3[f_. DeltaFunction[x_], {x_,_, _},  y___] :=
integrate1[f /. x -> 0, y];

integrate3[f_. DeltaFunction[x0_ -x_], {x_,_,1}
					] := (f /. x -> x0) /; FreeQ[x0, x];

integrate3[f_. DeltaFunction[x0_ + x_], {x_,_,1}
					] := (f /. x -> (-x0)) /; FreeQ[x0, x];

integrate3[f_. DeltaFunction[(x0_ /; FreeQ[x0, x])-x_],
					{x_, 0, 1}, {y_, 0, 1}
					] :=
integrate3[f /. x -> x0, {y, y /. Solve2[x0 - 1, y], 1}
					] /; !FreeQ[x0, y];

integrate3[f_. DeltaFunction[x0_ + x_],
					{x_,0,1}, {y_, 0, 1}
					] :=
integrate3[f /. x -> (-x0), {y, y /. Solve2[x0 + 1, y], 1}
					] /; (!FreeQ[x0, y]) && FreeQ[x0, x];

abs[h_Symbol] := h;
abs[- h_Symbol] := h;

integrate3[f_. DeltaFunction[(x0_ /; FreeQ[x0, x]) +
														(b_/;Head[b]=!=Integer) x_],
					{x_,0,1}, {z_,0,1}
					] :=
integrate3[f/abs[b] DeltaFunction[x0/b + x], {x,0,1}, {z,0,1}]

FCPrint[1,"Integrate5.m loaded."];
End[]
