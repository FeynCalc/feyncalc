(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* :Title: RTL*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 19 April '98 at 17:15 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

RTL::usage= "RTL[exp] inserts the list of known TLI integrals into \
exp, substitutes D -> 4 + Epsilon and expands around Epsilon up \
to the finite part.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`RTL`Private`"]

Options[RTL] = {Dimension             -> D,
								FeynmanParameterNames -> FCGV["x"],
								Momentum              -> FCGV["p"],
								QuarkMass             -> FCGV["M"]
							};

RTL[exp_, opt___Rule] := Block[{lsimp, MCH, so, p, x, M, dim, res},
dim = Dimension /. {opt} /. Options[RTL];
x = FeynmanParameterNames /. {opt} /. Options[RTL];
p = Momentum    /. {opt} /. Options[RTL];
M = QuarkMass   /. {opt} /. Options[RTL];

lsimp[0]=0;
lsimp[a_Plus] := Map[lsimp, a];
lsimp[t_Times] := Block[{mm},
									If[FreeQ[t, OPEm], mm = 1, mm = OPEm];
								((SelectNotFree[t, {Epsilon,p,Smu,CA,CF,Tf,Gstrong}] *
							x^(mm-1))/.dum[Epsilon]->1)  *
				Collect2[SimplifyDeltaFunction[Expand[Apart3[
							SelectFree[t, {Epsilon,p,Smu,CA,CF,Tf,Gstrong}]/x^(mm-1),x]
											]](* /. {DeltaFunction[1-x] f_ :>
															(f/.x->1) DeltaFunction[1-x]
													} *) ,
							{Log, PolyLog, Zeta,Zeta2,DeltaFunction,DeltaFunctionPrime},
									Factoring->False
								]     ];

(*??
MCH[y_] := If[Head[y] =!= Integer, If[FreeQ[y,OPEi],True,False], False];
*)
MCH[y_] := If[Head[y] =!= Integer, True, False];
(*
MCH[_]:=True;
*)

so[] = Pair[Momentum[p], Momentum[OPEDelta]];

(*
		RTLI[{1, 0, 0, 0, m_}, {a_, b_, b_, a_, c_},oot___Rule] :=
1/2 RTLI[{0, 0, 0, 0, m}, {a, b, b, a, c}] so[oot] +
1/2 RTLI[{0, 0, 0, 0, m+1}, {a, b, b, a, c}];
*)

tlilist = {
(* r1i1.r *)
RTLI[{m_ /; MCH[m], -2, 0, 0, 0}, {{2, M_}, 0, 0, 1, 1}, r___] :>
	Smu^2*x^(-1 + m)*((-1 + x^(-1))/Epsilon - 2*Log[1 - x] +
		(2*Log[1 - x])/x + Log[x])*so[r]^(-2 + m)
,
(* r1i2.r *)
RTLI[{m_ /; MCH[m], -2, 0, 0, 0}, {{2, M_}, {1, M_}, 0, 1, 1}, r___] :>
	(Smu^2*x^(-1 + m)*(1 - x^(-1) + Zeta2/x)*so[r]^(-2 + m))/M^2
,
(* r1i3.r *)
RTLI[{m_ /; MCH[m], -1, 0, 0, 0}, {{1, M_}, {1, M_}, 0, 1, 1}, r___] :>
	Smu^2*x^(-1 + m)*(-2 + 2*x + (-2 + 2*x)/Epsilon^2 + (3*Zeta2)/2 -
		(3*x*Zeta2)/2 + 4*Log[1 - x] - 4*x*Log[1 - x] - 4*Log[1 - x]^2 +
		4*x*Log[1 - x]^2 + 2*x*Log[x] - 4*x*Log[1 - x]*Log[x] -
		(x*Log[x]^2)/2 + (2 - 2*x - 4*Log[1 - x] + 4*x*Log[1 - x] -
				2*x*Log[x])/Epsilon - PolyLog[2, 1 - x] - 3*x*PolyLog[2, 1 - x])*
	so[r]^(-1 + m)
,
(* r1i4.r *)
RTLI[{m_ /; MCH[m], -1, 0, 0, 0}, {{2, M_}, 0, 0, 1, 1}, r___] :>
	Smu^2*x^(-1 + m)*(-(Log[x]/Epsilon) - 2*Log[1 - x]*Log[x] + Log[x]^2/4 -
		PolyLog[2, 1 - x])*so[r]^(-1 + m)
,
(* r1i5.r *)
RTLI[{m_ /; MCH[m], -1, 0, 0, 0}, {{2, M_}, {1, M_}, 0, 1, 0},___] :> 0
,
(* r1i6.r *)
RTLI[{m_ /; MCH[m], -1, 0, 0, 0}, {{2, M_}, {1, M_}, 0, 1, 1}, r___] :>
	(Smu^2*x^(-1 + m)*Zeta2*so[r]^(-1 + m))/M^2
,
(* r1i7.r *)
RTLI[{m_ /; MCH[m], 0, 0, 0, 0}, {{1, M_}, {1, M_}, 0, 1, 1}, r___] :>
	Smu^2*x^(-1 + m)*so[r]^m*((2*x*Log[x])/Epsilon^2 + (x*Zeta2*Log[x])/2 +
		4*x*Log[1 - x]^2*Log[x] - (x*Log[x]^3)/12 +
		8*x*Log[1 - x]*PolyLog[2, 1 - x] +
		(4*x*Log[1 - x]*Log[x] + (x*Log[x]^2)/2 + 4*x*PolyLog[2, 1 - x])/
			Epsilon - 8*x*PolyLog[3, 1 - x] - 2*x*PolyLog[3, x] + 2*x*Zeta[3])
,
(* r1i8.r *)
RTLI[{m_ /; MCH[m], 0, 0, 0, 0}, {{1, M_}, {2, M_}, 0, 1, 1}, r___] ->
	(Smu^2*x^(-1 + m)*((-2*x)/Epsilon^2 - (x*Zeta2)/2 - 4*x*Log[1 - x]^2 +
			4*x*Log[1 - x]*Log[x] + (-4*x*Log[1 - x] + 2*x*Log[x])/Epsilon +
			2*x*PolyLog[2, 1 - x])*so[r]^m)/M^2
,
(* r1i9.r *)
RTLI[{m_ /; MCH[m], 0, 0, 0, 0}, {{2, M_}, 0, 0, 1, 1}, r___] :>
	Smu^2*x^(-1 + m)*(-1 + (1 - x)/Epsilon + x + 2*Log[1 - x] -
		2*x*Log[1 - x] - Log[x]/2 + (x*Log[x])/2)*so[r]^m
,
(* r1i10.r *)
RTLI[{m_ /; MCH[m], 0, 0, 0, 0}, {{2, M_}, {1, M_}, 0, 1, 0},___] :> 0
,
(* r1i11.r *)
RTLI[{m_ /; MCH[m], 0, 0, 0, 0}, {{2, M_}, {1, M_}, 0, 1, 1}, r___] :>
	(Smu^2*x^(-1 + m)*(x*Zeta2 + (x*Log[x]^2)/2 + x*PolyLog[2, 1 - x])*so[r]^m)/
	M^2
,
(* r1i12.r *)
RTLI[{m_ /; MCH[m],  0, 0, 0, 0}, {{2, M_}, {2, M_}, 0, 1, 0},___] :> 0
,
(* r1i13.r *)
RTLI[{m_ /; MCH[m], 0, 0, 0, 0}, {{2, M_}, {2, M_}, 0, 1, 1}, r___] :>
	(Smu^2*x^(-1 + m)*(-1 + (1 - x)^(-1) +
			(1 - (1 - x)^(-1) + DeltaFunction[1 - x]/2)/Epsilon +
			DeltaFunction[1 - x]/2 - DeltaFunction[1 - x]/(2*Epsilon^2) -
			(5*Zeta2*DeltaFunction[1 - x])/8 + 2*Log[1 - x] -
			(2*Log[1 - x])/(1 - x) - Log[x] + Log[x]/(1 - x))*so[r]^m)/M^4
,
(* r1i14.r *)
RTLI[{m_ /; MCH[m], 1, 0, 0, 0}, {{1, M_}, {1, M_}, 0, 1, 1}, r___] :>
	Smu^2*x^(-1 + m)*(-2*x + 2*x^2 + (-2*x + 2*x^2)/Epsilon^2 +
		(3*x*Zeta2)/2 - (3*x^2*Zeta2)/2 + 4*x*Log[1 - x] - 4*x^2*Log[1 - x] -
		4*x*Log[1 - x]^2 + 4*x^2*Log[1 - x]^2 + x*Log[x] + x^2*Log[x] -
		2*x*Log[1 - x]*Log[x] - 2*x^2*Log[1 - x]*Log[x] + (x*Log[x]^2)/4 -
		(3*x^2*Log[x]^2)/4 + (2*x - 2*x^2 - 4*x*Log[1 - x] +
				4*x^2*Log[1 - x] - x*Log[x] - x^2*Log[x])/Epsilon -
		2*x*PolyLog[2, 1 - x] - 2*x^2*PolyLog[2, 1 - x])*so[r]^(1 + m)
,
(* r1i15.r *)
RTLI[{m_ /; MCH[m], 1, 0, 0, 0}, {{1, M_}, {2, M_}, 0, 1, 1}, r___] :>
	(Smu^2*x^(-1 + m)*((-2*x)/Epsilon^2 + (x*Zeta2)/2 - x^2*Zeta2 -
			(4*x*Log[1 - x])/Epsilon - 4*x*Log[1 - x]^2 - (x^2*Log[x]^2)/2 -
			x*PolyLog[2, 1 - x] - x^2*PolyLog[2, 1 - x])*so[r]^(1 + m))/M^2
,
(* n1i1.r *)
RTLI[{m_, 1, 0, 0, 0}, {{2, M}, {2, M}, 0, 1, 1}, r___] :>
	(Smu^2*x^(-1 + m)*(-1 + (1 - x)^(-1) - x + x^2*Zeta2 +
			(1 - (1 - x)^(-1) + DeltaFunction[1 - x]/2)/Epsilon +
			DeltaFunction[1 - x]/2 - DeltaFunction[1 - x]/(2*Epsilon^2) -
			(5*Zeta2*DeltaFunction[1 - x])/8 + 2*Log[1 - x] -
			(2*Log[1 - x])/(1 - x) - Log[x] + Log[x]/(1 - x) - x*Log[x] +
			(x^2*Log[x]^2)/2 + x^2*PolyLog[2, 1 - x])*so[r]^(1 + m))/M^4
,
(* n1i2.r *)
RTLI[{m_, 2, 0, 0, 0}, {{1, M}, {2, M}, 0, 1, 1}, r___] :>
	(Smu^2*x^(-1 + m)*(-2*x - (2*x)/Epsilon^2 + 2*x^2 + (x*Zeta2)/2 -
			x^3*Zeta2 + 4*x*Log[1 - x] - 4*x^2*Log[1 - x] - 4*x*Log[1 - x]^2 +
			(2*x - 2*x^2 - 4*x*Log[1 - x])/Epsilon + 2*x^2*Log[x] -
			(x^3*Log[x]^2)/2 - x*PolyLog[2, 1 - x] - x^3*PolyLog[2, 1 - x])*
		so[r]^(2 + m))/M^2
,
(* r2i1.r *)
RTLI[{0, m_ /; MCH[m], 0, 0, -1}, {{1, M_}, {1, M_}, 0, 1, 1}, r___] :>
	Smu^2*x^(-1 + m)*so[r]^(-1 + m)*
	(4/Epsilon^3 + (6*Log[1 - x])/Epsilon^2 + (7*Zeta2*Log[1 - x])/2 +
		(29*Log[1 - x]^3)/12 + (Zeta2 + (9*Log[1 - x]^2)/2)/Epsilon +
		Log[1 - x]*PolyLog[2, 1 - x] - 2*PolyLog[3, 1 - x] + (7*Zeta[3])/3)
,
(* r2i2.r *)
RTLI[{m_ /; MCH[m], 0, 0, 0, -1}, {{1, M_}, {1, M_}, 0, 1, 1}, r___] :>
	Smu^2*x^(-1 + m)*so[r]^(-1 + m)*
	(4/Epsilon^3 + (8*Log[1 - x])/Epsilon^2 - 2*Zeta2*Log[1 - x] +
		(16*Log[1 - x]^3)/3 - Zeta2*Log[x] + (Log[1 - x]*Log[x]^2)/2 +
		4*Log[1 - x]*PolyLog[2, 1 - x] + Log[x]*PolyLog[2, 1 - x] +
		(-Zeta2 + 8*Log[1 - x]^2 + 2*PolyLog[2, 1 - x])/Epsilon -
		4*PolyLog[3, 1 - x] + PolyLog[3, x] + (13*Zeta[3])/3)
,
(* r2i3.r *)
RTLI[{-1 - OPEi + OPEm, 1 + OPEi, 0, 0, 0}, {{2, M_}, {1, M_}, 1, 0, 1},
	r___] :> (Smu^2*x^(-1 + OPEm)*so[r]^OPEm*
		(2 - 2/(1 - x) + (7*Zeta2)/2 - (7*Zeta2)/(2*(1 - x)) + 4*x*Zeta2 +
			2*DeltaFunction[1 - x] - (3*Zeta2*DeltaFunction[1 - x])/2 +
			(-2 + 2/(1 - x) + 2*DeltaFunction[1 - x])/Epsilon^2 - 2*Log[1 - x] +
			(2*Log[1 - x])/(1 - x) - 7*Log[1 - x]^2 + (7*Log[1 - x]^2)/(1 - x) +
			x*Log[1 - x]^2 + 3*Log[1 - x]*Log[x] -
			(3*Log[1 - x]*Log[x])/(1 - x) - 2*x*Log[1 - x]*Log[x] +
			(-2 + 2/(1 - x) - 2*DeltaFunction[1 - x] -
					2*Zeta2*DeltaFunction[1 - x] - 6*Log[1 - x] +
					(6*Log[1 - x])/(1 - x) + 2*Log[x] - (2*Log[x])/(1 - x))/Epsilon -
			PolyLog[2, 1 - x] + PolyLog[2, 1 - x]/(1 - x) -
			2*x*PolyLog[2, 1 - x] + 2*DeltaFunction[1 - x]*Zeta[3]))/M^2
,
(* n2i1.r *)
RTLI[{0, m_ /; MCH[m], 0, 0, -1}, {{1, M_}, {2, M_}, 0, 1, 1}, r___] :>
	(Smu^2*x^(-1 + m)*so[r]^(-1 + m)*
		(2 + (2*x)/(1 - x) + Zeta2 + (3*x*Zeta2)/(1 - x) +
			DeltaFunction[1 - x] + (3*DeltaFunction[1 - x])/Epsilon^3 +
			(5*Zeta2*DeltaFunction[1 - x])/4 +
			(4 + (4*x)/(1 - x) + DeltaFunction[1 - x])/Epsilon^2 + 4*Log[1 - x] +
			(4*x*Log[1 - x])/(1 - x) + (9*Log[1 - x]^2)/2 +
			(5*x*Log[1 - x]^2)/(1 - x) +
			(2 + (2*x)/(1 - x) + DeltaFunction[1 - x] +
					(7*Zeta2*DeltaFunction[1 - x])/4 + 6*Log[1 - x] +
					(6*x*Log[1 - x])/(1 - x))/Epsilon -
			(x*Log[1 - x]*Log[x])/(1 - x) - (x*PolyLog[2, 1 - x])/(1 - x) +
			(5*DeltaFunction[1 - x]*Zeta[3])/4))/M^2
,
(* n2i2.r *)
RTLI[{m_ /; MCH[m], 0, 0, 0, -1}, {{1, M_}, {2, M_}, 0, 1, 1}, r___] :>
	(Smu^2*x^(-1 + m)*so[r]^(-1 + m)*
		(2/(1 - x) - Zeta2/2 - Zeta2/(2*(1 - x)) + DeltaFunction[1 - x] +
			(3*DeltaFunction[1 - x])/Epsilon^3 -
			(3*Zeta2*DeltaFunction[1 - x])/4 +
			(-2 + 6/(1 - x) + DeltaFunction[1 - x])/Epsilon^2 +
			(4*Log[1 - x])/(1 - x) - 4*Log[1 - x]^2 + (12*Log[1 - x]^2)/(1 - x) +
			4*Log[1 - x]*Log[x] - (4*Log[1 - x]*Log[x])/(1 - x) +
			(2/(1 - x) + DeltaFunction[1 - x] -
					(Zeta2*DeltaFunction[1 - x])/4 - 4*Log[1 - x] +
					(12*Log[1 - x])/(1 - x) + 2*Log[x] - (2*Log[x])/(1 - x))/Epsilon +
			2*PolyLog[2, 1 - x] + (13*DeltaFunction[1 - x]*Zeta[3])/4))/M^2
,
(* n2i3.r *)
RTLI[{m_ /; MCH[m], 2, 0, 0, -1}, {{1, M_}, {2, M_}, 0, 1, 1}, r___] :>
	(Smu^2*x^(-1 + m)*so[r]^(1 + m)*
		(-5/2 + 5/(2*(1 - x)) - (5*x)/2 + Zeta2/2 - Zeta2/(2*(1 - x)) +
			x^2*Zeta2 + (11*DeltaFunction[1 - x])/8 +
			(3*DeltaFunction[1 - x])/Epsilon^3 -
			(3*Zeta2*DeltaFunction[1 - x])/4 +
			(-6 + 6/(1 - x) - 4*x + DeltaFunction[1 - x])/Epsilon^2 -
			4*Log[1 - x] + (4*Log[1 - x])/(1 - x) - 4*x*Log[1 - x] -
			12*Log[1 - x]^2 + (12*Log[1 - x]^2)/(1 - x) - 8*x*Log[1 - x]^2 +
			4*Log[1 - x]*Log[x] - (4*Log[1 - x]*Log[x])/(1 - x) +
			4*x*Log[1 - x]*Log[x] + (x^2*Log[x]^2)/2 +
			(-2 + 2/(1 - x) - 2*x + (5*DeltaFunction[1 - x])/4 -
					(Zeta2*DeltaFunction[1 - x])/4 - 12*Log[1 - x] +
					(12*Log[1 - x])/(1 - x) - 8*x*Log[1 - x] + 2*Log[x] -
					(2*Log[x])/(1 - x) + 2*x*Log[x])/Epsilon + x*PolyLog[2, 1 - x] +
			x^2*PolyLog[2, 1 - x] + (13*DeltaFunction[1 - x]*Zeta[3])/4))/M^2
,
(*r3i1.r*)
RTLI[{0, m_ /; MCH[m], 0, 0, 0}, {{1, M_}, {3, M_}, 0, 1, 1}, r___] :>
	(Smu^2*x^(-1 + m)*(1 - 3/(2*(1 - x)^2) + 1/(2*(1 - x)) + 2*x*Zeta2 -
			DeltaFunction[1 - x] - (5*Zeta2*DeltaFunction[1 - x])/8 +
			(-DeltaFunction[1 - x]/2 - DeltaFunctionPrime[1 - x])/Epsilon^2 -
			(3*DeltaFunctionPrime[1 - x])/2 -
			(Zeta2*DeltaFunctionPrime[1 - x])/4 +
			((1 - x)^(-2) - (1 - x)^(-1) + DeltaFunction[1 - x]/2 +
					(3*DeltaFunctionPrime[1 - x])/2)/Epsilon + Log[1 - x] +
			Log[1 - x]/(1 - x)^2 - (2*Log[1 - x])/(1 - x) + (x*Log[1 - x]^2)/2 -
			x*Log[1 - x]*Log[x] - x*PolyLog[2, 1 - x])*so[r]^m)/M^4
,
(*r3i2.r*)
RTLI[{0, m_ /; MCH[m], 0, 0, 0}, {{1, M_}, {2, M_}, 0, 1, 1}, r___] :>
	(Smu^2*x^(-1 + m)*(-2 + 2/(1 - x) - 2*x*Zeta2 - 2*DeltaFunction[1 - x] -
			(2*DeltaFunction[1 - x])/Epsilon^2 -
			(Zeta2*DeltaFunction[1 - x])/2 +
			(2 - 2/(1 - x) + 2*DeltaFunction[1 - x])/Epsilon + 2*Log[1 - x] -
			(2*Log[1 - x])/(1 - x) - (x*Log[1 - x]^2)/2 + x*Log[1 - x]*Log[x] +
			x*PolyLog[2, 1 - x])*so[r]^m)/M^2
,
(*r3i3.r*)
RTLI[{0, m_ /; MCH[m], 0, 0, 0}, {{1, M_}, {1, M_}, 0, 1, 1}, r___] :>
	Smu^2*x^(-1 + m)*(-2*x - (2*x)/Epsilon^2 + (3*x*Zeta2)/2 +
		3*x*Log[1 - x] - (7*x*Log[1 - x]^2)/4 +
		(2*x - 3*x*Log[1 - x])/Epsilon - x*Log[1 - x]*Log[x] -
		x*PolyLog[2, 1 - x])*so[r]^m
,
(*r3i4.r*)
RTLI[{0, m_ /; MCH[m], 0, 0, 0}, {{1, M_}, 1, 0, 1, 1}, r___] :>
	Smu^2*x^(-1 + m)*(-2*x - (2*x)/Epsilon^2 - I*Pi*x + (9*x*Zeta2)/2 -
		Log[1 - x] + 2*x*Log[1 - x] + I*Pi*x*Log[1 - x] - (x*Log[1 - x]^2)/4 +
		x*Log[x] + I*Pi*x*Log[x] - 2*x*Log[1 - x]*Log[x] - (x*Log[x]^2)/2 +
		(2*x + 2*I*Pi*x - x*Log[1 - x] - 2*x*Log[x])/Epsilon -
		x*PolyLog[2, 1 - x])*so[r]^m
,
(*r3i5.r*)
RTLI[{0, m_ /; MCH[m], 0, 0, 0}, {{1, M_}, {3, M_}, 0, 1, 0}, r___] :>
	(Smu^2*x^(-1 + m)*(-(1 - x)^(-2) + (1 - x)^(-1) -
			DeltaFunction[1 - x]/2 - (Zeta2*DeltaFunction[1 - x])/4 +
			(-DeltaFunction[1 - x] - DeltaFunctionPrime[1 - x])/Epsilon^2 -
			DeltaFunctionPrime[1 - x]/2 - (Zeta2*DeltaFunctionPrime[1 - x])/4 +
			((1 - x)^(-2) - (1 - x)^(-1) + DeltaFunction[1 - x] +
					DeltaFunctionPrime[1 - x])/Epsilon + Log[1 - x]/(1 - x)^2 -
			Log[1 - x]/(1 - x))*so[r]^m)/M^2
,
(*r3i6.r*)
RTLI[{0, m_ /; MCH[m], 0, 0, 0}, {{1, M_}, {2, M_}, 0, 1, 0}, r___] :>
	Smu^2*x^(-1 + m)*(-1 + (1 - x)^(-1) - DeltaFunction[1 - x]/2 -
		(2*DeltaFunction[1 - x])/Epsilon^2 - (Zeta2*DeltaFunction[1 - x])/2 +
		(2 - 2/(1 - x) + DeltaFunction[1 - x])/Epsilon + 2*Log[1 - x] -
		(2*Log[1 - x])/(1 - x))*so[r]^m
,
(*r3i7.r*)
RTLI[{0, m_ /; MCH[m], 0, 0, 0}, {{1, M_}, {1, M_}, 0, 1, 0}, r___] :>
	M^2*Smu^2*x^(-1 + m)*(-x - (4*x)/Epsilon^2 - x*Zeta2 + 2*x*Log[1 - x] -
		2*x*Log[1 - x]^2 + (2*x - 4*x*Log[1 - x])/Epsilon)*so[r]^m
,
(*r3i8.r*)
RTLI[{0, m_ /; MCH[m], 0, 0, 0}, {{1, M_}, 1, 0, 1, 0}, r___] :>
	M^2*Smu^2*x^(-1 + m)*(-x - (4*x)/Epsilon^2 - I*Pi*x + 2*x*Zeta2 +
		x*Log[1 - x] + I*Pi*x*Log[1 - x] - (x*Log[1 - x]^2)/2 + x*Log[x] +
		I*Pi*x*Log[x] - x*Log[1 - x]*Log[x] - (x*Log[x]^2)/2 +
		(2*x + 2*I*Pi*x - 2*x*Log[1 - x] - 2*x*Log[x])/Epsilon)*so[r]^m
(* r4begin
,
(* r4i1.r *)
RTLI[{0, OPEi, 0, 0, 1}, {{1, M_}, {1, M_}, 0, 1, 1}, r___] :>
	Smu^2*x^(-1 + OPEm)*so[r]^(-1 + OPEm)*
	(1 - (1 - x)^(-1) - (3*Zeta2)/4 + (3*Zeta2)/(4*(1 - x)) +
		(1 - (1 - x)^(-1) - DeltaFunction[1 - x])/Epsilon^2 -
		(19*DeltaFunction[1 - x])/4 + (3*Zeta2*DeltaFunction[1 - x])/4 -
		(3*Log[1 - x])/2 + Log[1 - x]/(1 - x) + (7*Log[1 - x]^2)/8 -
		(7*Log[1 - x]^2)/(8*(1 - x)) +
		(-1 + (1 - x)^(-1) + (5*DeltaFunction[1 - x])/2 + (3*Log[1 - x])/2 -
				(3*Log[1 - x])/(2*(1 - x)))/Epsilon + (Log[1 - x]*Log[x])/2 -
		(Log[1 - x]*Log[x])/(2*(1 - x)) + PolyLog[2, 1 - x]/2 -
		PolyLog[2, 1 - x]/(2*(1 - x)) + DeltaFunction[1 - x]*Zeta[3])
(* r4i4.r *)
	r4end *)
,
(* r7i1.r *)
RTLI[{m_ /; MCH[m], 2, -1, 0, 0}, {{1, M_}, 0, 1, 1, 1}, r___] :>
	Smu^2*x^(-1 + m)*so[r]^(1 + m)*
	(2 - 2/(1 - x) + (34*x)/27 + (20*x^2)/27 + Zeta2/2 - Zeta2/(2*(1 - x)) +
		(x*Zeta2)/3 + (x^2*Zeta2)/6 + DeltaFunction[1 - x] -
		DeltaFunction[1 - x]/Epsilon^3 + (Zeta2*DeltaFunction[1 - x])/4 +
		(2 - 2/(1 - x) + (4*x)/3 + (2*x^2)/3 + DeltaFunction[1 - x])/Epsilon^2 -
		4*Log[1 - x] + (4*Log[1 - x])/(1 - x) - (23*x*Log[1 - x])/9 -
		(13*x^2*Log[1 - x])/9 + 4*Log[1 - x]^2 - (4*Log[1 - x]^2)/(1 - x) +
		(8*x*Log[1 - x]^2)/3 + (4*x^2*Log[1 - x]^2)/3 + Log[x] -
		Log[x]/(1 - x) + (23*x*Log[x])/36 + (13*x^2*Log[x])/36 -
		2*Log[1 - x]*Log[x] + (2*Log[1 - x]*Log[x])/(1 - x) -
		(4*x*Log[1 - x]*Log[x])/3 - (2*x^2*Log[1 - x]*Log[x])/3 + Log[x]^2/4 -
		Log[x]^2/(4*(1 - x)) + (x*Log[x]^2)/6 + (x^2*Log[x]^2)/12 +
		(-2 + 2/(1 - x) - (23*x)/18 - (13*x^2)/18 - DeltaFunction[1 - x] -
				(Zeta2*DeltaFunction[1 - x])/4 + 4*Log[1 - x] -
				(4*Log[1 - x])/(1 - x) + (8*x*Log[1 - x])/3 +
				(4*x^2*Log[1 - x])/3 - Log[x] + Log[x]/(1 - x) - (2*x*Log[x])/3 -
				(x^2*Log[x])/3)/Epsilon - (7*DeltaFunction[1 - x]*Zeta[3])/12)
,
(* r7i2.r *)
RTLI[{m_ /; MCH[m], 2, -2, 0, 0}, {{1, M_}, 0, 1, 1, 1}, r___] :>
	Smu^2*x^(-1 + m)*so[r]^m*(-2 - 2/(1 - x)^2 + 4/(1 - x) - (20*x)/27 -
		Zeta2/2 - Zeta2/(2*(1 - x)^2) + Zeta2/(1 - x) - (x*Zeta2)/6 -
		3*DeltaFunction[1 - x] - (3*Zeta2*DeltaFunction[1 - x])/4 +
		(-2 - 2/(1 - x)^2 + 4/(1 - x) - (2*x)/3 - 3*DeltaFunction[1 - x] -
				2*DeltaFunctionPrime[1 - x])/Epsilon^2 -
		2*DeltaFunctionPrime[1 - x] - (Zeta2*DeltaFunctionPrime[1 - x])/2 +
		(2*DeltaFunction[1 - x] + 2*DeltaFunctionPrime[1 - x])/Epsilon^3 +
		4*Log[1 - x] + (4*Log[1 - x])/(1 - x)^2 - (8*Log[1 - x])/(1 - x) +
		(13*x*Log[1 - x])/9 - 4*Log[1 - x]^2 - (4*Log[1 - x]^2)/(1 - x)^2 +
		(8*Log[1 - x]^2)/(1 - x) - (4*x*Log[1 - x]^2)/3 - Log[x] -
		Log[x]/(1 - x)^2 + (2*Log[x])/(1 - x) - (13*x*Log[x])/36 +
		2*Log[1 - x]*Log[x] + (2*Log[1 - x]*Log[x])/(1 - x)^2 -
		(4*Log[1 - x]*Log[x])/(1 - x) + (2*x*Log[1 - x]*Log[x])/3 - Log[x]^2/4 -
		Log[x]^2/(4*(1 - x)^2) + Log[x]^2/(2*(1 - x)) - (x*Log[x]^2)/12 +
		(2 + 2/(1 - x)^2 - 4/(1 - x) + (13*x)/18 + 3*DeltaFunction[1 - x] +
				(Zeta2*DeltaFunction[1 - x])/2 + 2*DeltaFunctionPrime[1 - x] +
				(Zeta2*DeltaFunctionPrime[1 - x])/2 - 4*Log[1 - x] -
				(4*Log[1 - x])/(1 - x)^2 + (8*Log[1 - x])/(1 - x) -
				(4*x*Log[1 - x])/3 + Log[x] + Log[x]/(1 - x)^2 -
				(2*Log[x])/(1 - x) + (x*Log[x])/3)/Epsilon +
		(7*DeltaFunction[1 - x]*Zeta[3])/6 +
		(7*DeltaFunctionPrime[1 - x]*Zeta[3])/6)
,
(* r7i3.r *)
RTLI[{m_ /; MCH[m], 1, 0, 0, 0}, {{1, M_}, 0, 1, 1, 1}, r___] :>
	Smu^2*x^(-1 + m)*(-x - x^2 + (-x - x^2)/Epsilon^2 - (x*Zeta2)/4 -
		(x^2*Zeta2)/4 + 2*x*Log[1 - x] + 2*x^2*Log[1 - x] - 2*x*Log[1 - x]^2 -
		2*x^2*Log[1 - x]^2 - (x*Log[x])/2 - (x^2*Log[x])/2 +
		x*Log[1 - x]*Log[x] + x^2*Log[1 - x]*Log[x] - (x*Log[x]^2)/8 -
		(x^2*Log[x]^2)/8 + (x + x^2 - 2*x*Log[1 - x] - 2*x^2*Log[1 - x] +
				(x*Log[x])/2 + (x^2*Log[x])/2)/Epsilon)*so[r]^(1 + m)
,
(* r7i4.r *)
RTLI[{m_ /; MCH[m], 1, -1, 0, 0}, {{1, M_}, 0, 1, 1, 1}, r___] :>
	Smu^2*x^(-1 + m)*so[r]^m*(2 - 2/(1 - x) + x + Zeta2/2 - Zeta2/(2*(1 - x)) +
		(x*Zeta2)/4 + DeltaFunction[1 - x] - DeltaFunction[1 - x]/Epsilon^3 +
		(Zeta2*DeltaFunction[1 - x])/4 +
		(2 - 2/(1 - x) + x + DeltaFunction[1 - x])/Epsilon^2 - 4*Log[1 - x] +
		(4*Log[1 - x])/(1 - x) - 2*x*Log[1 - x] + 4*Log[1 - x]^2 -
		(4*Log[1 - x]^2)/(1 - x) + 2*x*Log[1 - x]^2 + Log[x] - Log[x]/(1 - x) +
		(x*Log[x])/2 - 2*Log[1 - x]*Log[x] + (2*Log[1 - x]*Log[x])/(1 - x) -
		x*Log[1 - x]*Log[x] + Log[x]^2/4 - Log[x]^2/(4*(1 - x)) +
		(x*Log[x]^2)/8 + (-2 + 2/(1 - x) - x - DeltaFunction[1 - x] -
				(Zeta2*DeltaFunction[1 - x])/4 + 4*Log[1 - x] -
				(4*Log[1 - x])/(1 - x) + 2*x*Log[1 - x] - Log[x] + Log[x]/(1 - x) -
				(x*Log[x])/2)/Epsilon - (7*DeltaFunction[1 - x]*Zeta[3])/12)
,
(* r7i5.r *)
RTLI[{m_ /; MCH[m], 1, -2, 0, 0}, {{1, M_}, 0, 1, 1, 1}, r___] :>
	Smu^2*x^(-1 + m)*so[r]^(-1 + m)*
	(-1 - 2/(1 - x)^2 + 3/(1 - x) - Zeta2/4 - Zeta2/(2*(1 - x)^2) +
		(3*Zeta2)/(4*(1 - x)) - (5*DeltaFunction[1 - x])/2 -
		(5*Zeta2*DeltaFunction[1 - x])/8 +
		(-1 - 2/(1 - x)^2 + 3/(1 - x) - (5*DeltaFunction[1 - x])/2 -
				2*DeltaFunctionPrime[1 - x])/Epsilon^2 -
		2*DeltaFunctionPrime[1 - x] - (Zeta2*DeltaFunctionPrime[1 - x])/2 +
		((3*DeltaFunction[1 - x])/2 + 2*DeltaFunctionPrime[1 - x])/Epsilon^3 +
		2*Log[1 - x] + (4*Log[1 - x])/(1 - x)^2 - (6*Log[1 - x])/(1 - x) -
		2*Log[1 - x]^2 - (4*Log[1 - x]^2)/(1 - x)^2 + (6*Log[1 - x]^2)/(1 - x) -
		Log[x]/2 - Log[x]/(1 - x)^2 + (3*Log[x])/(2*(1 - x)) +
		Log[1 - x]*Log[x] + (2*Log[1 - x]*Log[x])/(1 - x)^2 -
		(3*Log[1 - x]*Log[x])/(1 - x) - Log[x]^2/8 - Log[x]^2/(4*(1 - x)^2) +
		(3*Log[x]^2)/(8*(1 - x)) +
		(1 + 2/(1 - x)^2 - 3/(1 - x) + (5*DeltaFunction[1 - x])/2 +
				(3*Zeta2*DeltaFunction[1 - x])/8 + 2*DeltaFunctionPrime[1 - x] +
				(Zeta2*DeltaFunctionPrime[1 - x])/2 - 2*Log[1 - x] -
				(4*Log[1 - x])/(1 - x)^2 + (6*Log[1 - x])/(1 - x) + Log[x]/2 +
				Log[x]/(1 - x)^2 - (3*Log[x])/(2*(1 - x)))/Epsilon +
		(7*DeltaFunction[1 - x]*Zeta[3])/8 +
		(7*DeltaFunctionPrime[1 - x]*Zeta[3])/6)
,
(* r7i6.r *)
RTLI[{m_ /; MCH[m], 0, 0, 0, 0}, {{2, M_}, 0, 1, 1, 1}, r___] :>
	(Smu^2*x^(-1 + m)*(-2 + 2/(1 - x) - DeltaFunction[1 - x] -
			DeltaFunction[1 - x]/Epsilon^2 - (Zeta2*DeltaFunction[1 - x])/4 +
			(2 - 2/(1 - x) + DeltaFunction[1 - x])/Epsilon + 4*Log[1 - x] -
			(4*Log[1 - x])/(1 - x) - Log[x] + Log[x]/(1 - x))*so[r]^m)/M^2
,
(* r7i7.r *)
RTLI[{m_ /; MCH[m], 0, 0, 0, 0}, {{1, M_}, 0, 1, 1, 1}, r___] :>
	Smu^2*x^(-1 + m)*(-2*x - (2*x)/Epsilon^2 - (x*Zeta2)/2 + 4*x*Log[1 - x] -
		4*x*Log[1 - x]^2 - x*Log[x] + 2*x*Log[1 - x]*Log[x] - (x*Log[x]^2)/4 +
		(2*x - 4*x*Log[1 - x] + x*Log[x])/Epsilon)*so[r]^m
,
(* r7i8.r *)
RTLI[{m_ /; MCH[m], 0, -1, 0, 0}, {{1, M_}, 0, 1, 1, 1}, r___] :>
	Smu^2*x^(-1 + m)*so[r]^(-1 + m)*
	(2 - 2/(1 - x) + Zeta2/2 - Zeta2/(2*(1 - x)) + DeltaFunction[1 - x] -
		DeltaFunction[1 - x]/Epsilon^3 + (Zeta2*DeltaFunction[1 - x])/4 +
		(2 - 2/(1 - x) + DeltaFunction[1 - x])/Epsilon^2 - 4*Log[1 - x] +
		(4*Log[1 - x])/(1 - x) + 4*Log[1 - x]^2 - (4*Log[1 - x]^2)/(1 - x) +
		Log[x] - Log[x]/(1 - x) - 2*Log[1 - x]*Log[x] +
		(2*Log[1 - x]*Log[x])/(1 - x) + Log[x]^2/4 - Log[x]^2/(4*(1 - x)) +
		(-2 + 2/(1 - x) - DeltaFunction[1 - x] -
				(Zeta2*DeltaFunction[1 - x])/4 + 4*Log[1 - x] -
				(4*Log[1 - x])/(1 - x) - Log[x] + Log[x]/(1 - x))/Epsilon -
		(7*DeltaFunction[1 - x]*Zeta[3])/12)
,
(* r7i9.r *)
RTLI[{m_ /; MCH[m], 0, -2, 0, 0}, {{1, M_}, 0, 1, 1, 1}, r___] :>
	Smu^2*x^(-1 + m)*so[r]^(-2 + m)*
	(-2/(1 - x)^2 + 2/(1 - x) - Zeta2/(2*(1 - x)^2) + Zeta2/(2*(1 - x)) -
		2*DeltaFunction[1 - x] - (Zeta2*DeltaFunction[1 - x])/2 +
		(-2/(1 - x)^2 + 2/(1 - x) - 2*DeltaFunction[1 - x] -
				2*DeltaFunctionPrime[1 - x])/Epsilon^2 -
		2*DeltaFunctionPrime[1 - x] - (Zeta2*DeltaFunctionPrime[1 - x])/2 +
		(DeltaFunction[1 - x] + 2*DeltaFunctionPrime[1 - x])/Epsilon^3 +
		(4*Log[1 - x])/(1 - x)^2 - (4*Log[1 - x])/(1 - x) -
		(4*Log[1 - x]^2)/(1 - x)^2 + (4*Log[1 - x]^2)/(1 - x) -
		Log[x]/(1 - x)^2 + Log[x]/(1 - x) + (2*Log[1 - x]*Log[x])/(1 - x)^2 -
		(2*Log[1 - x]*Log[x])/(1 - x) - Log[x]^2/(4*(1 - x)^2) +
		Log[x]^2/(4*(1 - x)) + (2/(1 - x)^2 - 2/(1 - x) +
				2*DeltaFunction[1 - x] + (Zeta2*DeltaFunction[1 - x])/4 +
				2*DeltaFunctionPrime[1 - x] + (Zeta2*DeltaFunctionPrime[1 - x])/2 -
				(4*Log[1 - x])/(1 - x)^2 + (4*Log[1 - x])/(1 - x) +
				Log[x]/(1 - x)^2 - Log[x]/(1 - x))/Epsilon +
		(7*DeltaFunction[1 - x]*Zeta[3])/12 +
		(7*DeltaFunctionPrime[1 - x]*Zeta[3])/6)
,
(* r7i10.r *)
RTLI[{m_ /; MCH[m], 0, 0, 0, 0}, {{1, M_}, 0, 2, 1, 0}, r___] :> 0
,
(* r11i1.r *)
RTLI[{m_ /; MCH[m], 1, 0, 0, 0}, {{1, M_}, 1, 1, {1, M_}, {1, M_}}, r___] :>
	(Smu^2*x^(-1 + m)*(-(x*Log[1 - x]) + (1 - (1 - x)^(-1) + x)*Log[x])*
		so[r]^(1 + m))/M^2
,
(* r11i10.r *)
RTLI[{m_ /; MCH[m], 0, 0, 0, 0}, {{1, M_}, 1, 1, {1, M_}, 0}, r___] :>
	Smu^2*x^(-1 + m)*(-4*x - (4*x)/Epsilon^2 - x*Zeta2 + 4*x*Log[1 - x] -
		2*x*Log[1 - x]^2 + (4*x - 4*x*Log[1 - x])/Epsilon)*so[r]^m
,
(* r11i11.r *)
RTLI[{m_ /; MCH[m], 0, 0, 0, 0}, {{1, M_}, 1, 0, {1, M_}, {1, M_}}, r___] :>
	Smu^2*x^(-1 + m)*so[r]^m*((2*x*Log[x])/Epsilon^2 - (3*x*Zeta2*Log[x])/2 +
		(5*x*Log[x]^2)/(2*Epsilon) + 2*x*Log[1 - x]*Log[x]^2 +
		(7*x*Log[x]^3)/12 + 2*x*Log[x]*PolyLog[2, 1 - x] -
		4*x*Log[x]*PolyLog[2, -x] + 8*x*PolyLog[3, -x] + 4*x*PolyLog[3, x] +
		2*x*Zeta[3])
,
(* r11i12.r *)
RTLI[{m_ /; MCH[m], 0, 0, 0, 0}, {{1, M_}, 0, 1, {1, M_}, {1, M_}}, r___] :>
	Smu^2*x^(-1 + m)*(-2*x - (2*x)/Epsilon^2 - (x*Zeta2)/2 -
		2*x*Log[1 - x]^2 + 4*Log[x] - (4*Log[x])/(1 - x) + 3*x*Log[x] -
		(x*Log[x]^2)/4 + (2*x - 4*x*Log[1 - x] + x*Log[x])/Epsilon)*so[r]^m
,
(* r11i13.r *)
RTLI[{m_ /; MCH[m], 0, 0, 0, 0}, {0, 1, 1, {1, M_}, {1, M_}}, r___] :>
	Smu^2*x^(-1 + m)*so[r]^m*((2*x*Log[1 - x])/Epsilon^2 +
		(5*x*Zeta2*Log[1 - x])/2 + (x*Log[1 - x]^3)/12 + 4*x*Zeta2*Log[x] -
		2*x*Log[1 - x]*Log[x]^2 - x*Log[1 - x]*PolyLog[2, 1 - x] -
		4*x*Log[x]*PolyLog[2, 1 - x] +
		(4*x*Zeta2 + (x*Log[1 - x]^2)/2 - 4*x*PolyLog[2, 1 - x])/Epsilon -
		2*x*PolyLog[3, 1 - x] - 4*x*PolyLog[3, x] + 2*x*Zeta[3])
,
(* r11i14.r *)
RTLI[{m_ /; MCH[m], -1, 0, 0, 0}, {{1, M_}, 1, 0, {1, M_}, {1, M_}}, r___] :>
	Smu^2*x^(-1 + m)*(-2 + 2*x + (-2 + 2*x)/Epsilon^2 - (3*Zeta2)/2 +
		(3*x*Zeta2)/2 - 4*Log[x] + 2*x*Log[x] - 2*Log[x]^2 - (x*Log[x]^2)/2 +
		(-2 + 2*x - 4*Log[x] + 2*x*Log[x])/Epsilon - 2*Log[x]*Log[1 + x] +
		2*x*Log[x]*Log[1 + x] + PolyLog[2, 1 - x] - x*PolyLog[2, 1 - x] -
		2*PolyLog[2, -x] + 2*x*PolyLog[2, -x])*so[r]^(-1 + m)
,
(* r11i15.r *)
RTLI[{m_ /; MCH[m], 0, 0, 0, 0}, {{2, M_}, 1, 0, {1, M_}, 0}, r___] :> 0
,
(* r11i16.r *)
RTLI[{m_ /; MCH[m], 0, 0, 0, 0}, {{2, M_}, 0, 1, {1, M_}, 0}, r___] :>
	Smu^2*x^(-1 + m)*(-1 + (1 - x)^(-1) - DeltaFunction[1 - x]/2 -
		(2*DeltaFunction[1 - x])/Epsilon^2 - (Zeta2*DeltaFunction[1 - x])/2 +
		(2 - 2/(1 - x) + DeltaFunction[1 - x])/Epsilon + 2*Log[1 - x] -
		(2*Log[1 - x])/(1 - x))*so[r]^m
,
(* r11i17.r *)
RTLI[{m_ /; MCH[m], 0, 0, 0, 0}, {{2, M_}, 0, 0, {1, M_}, {1, M_}}, r___] :>
	Smu^2*x^(-1 + m)*(-1 + (1 - x)/Epsilon + x - Log[x]/2 - (3*x*Log[x])/2)*
	so[r]^m
,
(* r11i18.r *)
RTLI[{m_ /; MCH[m], 0, 0, 0, 0}, {{1, M_}, 0, 2, {1, M_}, 0}, r___] :>
	Smu^2*x^(-1 + m)*(1 + (1 - x)^(-2) - 2/(1 - x) + DeltaFunction[1 - x] +
		Zeta2*DeltaFunction[1 - x] +
		(-2 - 2/(1 - x)^2 + 4/(1 - x) - 2*DeltaFunction[1 - x] -
				DeltaFunctionPrime[1 - x])/Epsilon + DeltaFunctionPrime[1 - x]/2 +
		(Zeta2*DeltaFunctionPrime[1 - x])/2 +
		(4*DeltaFunction[1 - x] + 2*DeltaFunctionPrime[1 - x])/Epsilon^2 -
		2*Log[1 - x] - (2*Log[1 - x])/(1 - x)^2 + (4*Log[1 - x])/(1 - x))*so[r]^m
,
(* r11i19.r *)
RTLI[{m_ /; MCH[m], -1, 0, 0, 0}, {{2, M_}, 1, 0, {1, M_}, 0}, r___] :> 0
,
(* r11i2.r *)
RTLI[{m_ /; MCH[m], 0, 0, 0, 0}, {{2, M_}, 1, 1, {1, M_}, {1, M_}}, r___] :>
	(Smu^2*x^(-1 + m)*(Zeta2/(1 - x)^2 - Zeta2/(1 - x) - (3*x*Zeta2)/2 +
			DeltaFunction[1 - x]/2 - DeltaFunction[1 - x]/(2*Epsilon^2) +
			(3*Zeta2*DeltaFunction[1 - x])/8 - Log[1 - x]/2 +
			Log[1 - x]/(2*(1 - x)) - (x*Log[1 - x]^2)/4 + Log[x]/2 -
			Log[x]/(2*(1 - x)^2) + Log[x]/(2*(1 - x)) - Log[x]/(2*(1 + x)) -
			(Log[1 - x]*Log[x])/(2*(1 - x)^2) +
			(Log[1 - x]*Log[x])/(2*(1 - x)) + (x*Log[1 - x]*Log[x])/2 -
			(3*Log[x]^2)/(4*(1 - x)^2) + (3*Log[x]^2)/(4*(1 - x)) +
			(3*x*Log[x]^2)/4 + (2*Log[x]*Log[1 + x])/(1 - x)^2 -
			(2*Log[x]*Log[1 + x])/(1 - x) - x*Log[x]*Log[1 + x] -
			PolyLog[2, 1 - x]/(2*(1 - x)^2) + PolyLog[2, 1 - x]/(2*(1 - x)) +
			x*PolyLog[2, 1 - x] + (2*PolyLog[2, -x])/(1 - x)^2 -
			(2*PolyLog[2, -x])/(1 - x) - x*PolyLog[2, -x])*so[r]^m)/M^4
,
(* r11i20.r *)
RTLI[{m_ /; MCH[m], -1, 0, 0, 0}, {{2, M_}, 0, 0, {1, M_}, {1, M_}}, r___] :>
	Smu^2*x^(-1 + m)*(-Zeta2 - Log[x]/Epsilon + Log[x]^2/4 -
		2*Log[x]*Log[1 + x] + PolyLog[2, 1 - x] - 2*PolyLog[2, -x])*
	so[r]^(-1 + m)
,
(* r11i21.r *)
RTLI[{m_ /; MCH[m], -2, 0, 0, 0}, {{2, M_}, 0, 0, {1, M_}, {1, M_}}, r___] :>
	Smu^2*x^(-1 + m)*(-1 + (-1 + x^(-1))/Epsilon + x^(-1) - Log[x] +
		(2*Log[x])/(1 + x))*so[r]^(-2 + m)
,
(* r11i22.r *)
RTLI[{0, m_ /; MCH[m], 0, 0, 0}, {{1, M_}, {1, M_}, 0, 2, 0}, r___] :>
	Smu^2*x^(-1 + m)*(1 + (1 - x)^(-2) - 2/(1 - x) + DeltaFunction[1 - x] +
		Zeta2*DeltaFunction[1 - x] +
		(-2 - 2/(1 - x)^2 + 4/(1 - x) - 2*DeltaFunction[1 - x] -
				DeltaFunctionPrime[1 - x])/Epsilon + DeltaFunctionPrime[1 - x]/2 +
		(Zeta2*DeltaFunctionPrime[1 - x])/2 +
		(4*DeltaFunction[1 - x] + 2*DeltaFunctionPrime[1 - x])/Epsilon^2 -
		2*Log[1 - x] - (2*Log[1 - x])/(1 - x)^2 + (4*Log[1 - x])/(1 - x))*so[r]^m
,
(* r11i3.r *)
RTLI[{m_ /; MCH[m], 0, 0, 0, 0}, {{1, M_}, 1, 1, {1, M_}, {1, M_}}, r___] :>
	(Smu^2*x^(-1 + m)*((2 - 2/(1 - x) + 3*x)*Zeta2 + (x*Log[1 - x]^2)/2 +
			(-1 + (1 - x)^(-1) - x)*Log[1 - x]*Log[x] +
			(-3/2 + 3/(2*(1 - x)) - (3*x)/2)*Log[x]^2 +
			(4 - 4/(1 - x) + 2*x)*Log[x]*Log[1 + x] +
			(-1 + (1 - x)^(-1) - 2*x)*PolyLog[2, 1 - x] +
			(4 - 4/(1 - x) + 2*x)*PolyLog[2, -x])*so[r]^m)/M^2
,
(* r11i4.r *)
RTLI[{m_ /; MCH[m], 1, 0, 0, 0}, {{1, M_}, 1, 0, {1, M_}, {1, M_}}, r___] :>
	Smu^2*x^(-1 + m)*(-2*x + 2*x^2 + (-2*x + 2*x^2)/Epsilon^2 - (x*Zeta2)/2 +
		(x^2*Zeta2)/2 + x*Log[x] - 3*x^2*Log[x] + (x*Log[x]^2)/4 +
		(9*x^2*Log[x]^2)/4 + (2*x - 2*x^2 - x*Log[x] + 3*x^2*Log[x])/Epsilon)*
	so[r]^(1 + m)
,
(* r11i5.r *)
RTLI[{m_ /; MCH[m], 1, 0, 0, 0}, {{1, M_}, 0, 1, {1, M_}, {1, M_}}, r___] :>
	Smu^2*x^(-1 + m)*(-x - x^2 + (-x - x^2)/Epsilon^2 - (x*Zeta2)/4 -
		(x^2*Zeta2)/4 - x*Log[1 - x]^2 - x^2*Log[1 - x]^2 + 4*Log[x] -
		(4*Log[x])/(1 - x) + (7*x*Log[x])/2 + (3*x^2*Log[x])/2 -
		(x*Log[x]^2)/8 - (x^2*Log[x]^2)/8 +
		(x + x^2 - 2*x*Log[1 - x] - 2*x^2*Log[1 - x] + (x*Log[x])/2 +
				(x^2*Log[x])/2)/Epsilon)*so[r]^(1 + m)
,
(* r11i6.r *)
RTLI[{m_ /; MCH[m], 1, 0, 0, 0}, {0, 1, 1, {1, M_}, {1, M_}}, r___] :>
	Smu^2*x^(-1 + m)*so[r]^(1 + m)*
	(2*x^2 + 4*x*Zeta2 - (3*x^2*Zeta2)/2 + 2*x*Log[1 - x] - 3*x^2*Log[1 - x] +
		(5*x*Zeta2*Log[1 - x])/2 + (x*Log[1 - x]^2)/2 - (x^2*Log[1 - x]^2)/4 +
		(x*Log[1 - x]^3)/12 + (2*x^2 + 2*x*Log[1 - x])/Epsilon^2 +
		4*x*Zeta2*Log[x] + x^2*Log[1 - x]*Log[x] + 2*x^2*Log[x]^2 -
		2*x*Log[1 - x]*Log[x]^2 - 4*x*PolyLog[2, 1 - x] +
		x^2*PolyLog[2, 1 - x] - x*Log[1 - x]*PolyLog[2, 1 - x] -
		4*x*Log[x]*PolyLog[2, 1 - x] +
		(-2*x^2 + 4*x*Zeta2 + 2*x*Log[1 - x] - x^2*Log[1 - x] +
				(x*Log[1 - x]^2)/2 + 4*x^2*Log[x] - 4*x*PolyLog[2, 1 - x])/Epsilon -
		2*x*PolyLog[3, 1 - x] - 4*x*PolyLog[3, x] + 2*x*Zeta[3])
,
(* r11i7.r *)
RTLI[{m_ /; MCH[m], 0, 0, 0, 0}, {{2, M_}, 1, 1, {1, M_}, 0}, r___] :>
	(Smu^2*x^(-1 + m)*(-2 + 2/(1 - x) - 2*DeltaFunction[1 - x] -
			(2*DeltaFunction[1 - x])/Epsilon^2 -
			(Zeta2*DeltaFunction[1 - x])/2 +
			(2 - 2/(1 - x) + 2*DeltaFunction[1 - x])/Epsilon + 2*Log[1 - x] -
			(2*Log[1 - x])/(1 - x))*so[r]^m)/M^2
,
(* r11i8.r *)
RTLI[{m_ /; MCH[m], 0, 0, 0, 0}, {{2, M_}, 1, 0, {1, M_}, {1, M_}}, r___] :>
	(Smu^2*x^(-1 + m)*(-(x*Zeta2) + (2 - 2/(1 + x))*Log[x] +
			(3*x*Log[x]^2)/2 - 2*x*Log[x]*Log[1 + x] + x*PolyLog[2, 1 - x] -
			2*x*PolyLog[2, -x])*so[r]^m)/M^2
,
(* r11i9.r *)
RTLI[{m_ /; MCH[m], 0, 0, 0, 0}, {{2, M_}, 0, 1, {1, M_}, {1, M_}}, r___] :>
	(Smu^2*x^(-1 + m)*(-2 + (2 - 2/(1 - x))/Epsilon + 2/(1 - x) -
			(2*DeltaFunction[1 - x])/Epsilon^2 -
			(Zeta2*DeltaFunction[1 - x])/2 + 2*Log[1 - x] -
			(2*Log[1 - x])/(1 - x) + Log[x] + (2*Log[x])/(1 - x)^2 -
			(3*Log[x])/(1 - x))*so[r]^m)/M^2
,
(* n11i1.r *)
RTLI[{m_ /; MCH[m], 1, 0, 0, 0}, {{2, M_}, 1, 1, {1, M_}, {1, M_}}, r___] :>
	(Smu^2*x^(-1 + m)*(-Zeta2/(3*(1 - x)^2) + Zeta2/(3*(1 - x)) +
			(x*Zeta2)/2 + x^2*Zeta2 + DeltaFunction[1 - x] -
			DeltaFunction[1 - x]/Epsilon - Log[1 - x]/6 +
			Log[1 - x]/(6*(1 - x)) + (x*Log[1 - x])/3 + (x*Log[1 - x]^2)/12 +
			(x^2*Log[1 - x]^2)/6 + (5*Log[x])/6 + (7*Log[x])/(6*(1 - x)^2) -
			(2*Log[x])/(1 - x) - (x*Log[x])/3 +
			(Log[1 - x]*Log[x])/(6*(1 - x)^2) -
			(Log[1 - x]*Log[x])/(6*(1 - x)) - (x*Log[1 - x]*Log[x])/6 -
			(x^2*Log[1 - x]*Log[x])/3 + Log[x]^2/(4*(1 - x)^2) -
			Log[x]^2/(4*(1 - x)) - (x*Log[x]^2)/4 - (x^2*Log[x]^2)/2 -
			(2*Log[x]*Log[1 + x])/(3*(1 - x)^2) +
			(2*Log[x]*Log[1 + x])/(3*(1 - x)) + (x*Log[x]*Log[1 + x])/3 +
			(2*x^2*Log[x]*Log[1 + x])/3 + PolyLog[2, 1 - x]/(6*(1 - x)^2) -
			PolyLog[2, 1 - x]/(6*(1 - x)) - (x*PolyLog[2, 1 - x])/3 -
			(2*x^2*PolyLog[2, 1 - x])/3 - (2*PolyLog[2, -x])/(3*(1 - x)^2) +
			(2*PolyLog[2, -x])/(3*(1 - x)) + (x*PolyLog[2, -x])/3 +
			(2*x^2*PolyLog[2, -x])/3)*so[r]^(1 + m))/M^4
,
(* n11i2.r *)
RTLI[{m_ /; MCH[m], 1, 0, 0, 0}, {{2, M_}, 1, 1, {1, M_}, 0}, r___] :>
	(Smu^2*x^(-1 + m)*(-1/2 + 1/(2*(1 - x)) +
			(1 - (1 - x)^(-1) + DeltaFunction[1 - x]/2)/Epsilon -
			DeltaFunction[1 - x]/4 - DeltaFunction[1 - x]/Epsilon^2 -
			(Zeta2*DeltaFunction[1 - x])/4 + Log[1 - x] - Log[1 - x]/(1 - x))*
		so[r]^(1 + m))/M^2
,
(* n11i3.r *)
RTLI[{m_ /; MCH[m], 1, 0, 0, 0}, {{2, M_}, 0, 1, {1, M_}, {1, M_}}, r___] :>
	(Smu^2*x^(-1 + m)*(-2 + 2/(1 - x) - x + (2 - 2/(1 - x) + x)/Epsilon -
			(2*DeltaFunction[1 - x])/Epsilon^2 -
			(Zeta2*DeltaFunction[1 - x])/2 + 2*Log[1 - x] -
			(2*Log[1 - x])/(1 - x) + x*Log[1 - x] + 2*Log[x] +
			(2*Log[x])/(1 - x)^2 - (4*Log[x])/(1 - x) + (x*Log[x])/2)*
		so[r]^(1 + m))/M^2
,
(* r13i1.r *)
RTLI[{0, 0, 0, 0, m_ /; MCH[m]}, {{1, M_}, 1, 1, {1, M_}, 0}, r___] :>
	Smu^2*x^(-1 + m)*(-16*x - 16*(-1)^m*x + 16*x^2 + 16*(-1)^m*x^2 +
		(-4*x - 4*(-1)^m*x + 4*x^2 + 4*(-1)^m*x^2)/Epsilon^2 + 3*x*Zeta2 +
		3*(-1)^m*x*Zeta2 - 3*x^2*Zeta2 + 5*(-1)^m*x^2*Zeta2 + 16*x*Log[1 - x] -
		16*x^2*Log[1 - x] - 8*x*Log[1 - x]^2 + 8*x^2*Log[1 - x]^2 -
		16*(-1)^m*x^2*Log[x] + 4*(-1)^m*x^2*Log[x]^2 +
		(8*x + 8*(-1)^m*x - 8*x^2 - 8*(-1)^m*x^2 - 8*x*Log[1 - x] +
				8*x^2*Log[1 - x] + 8*(-1)^m*x^2*Log[x])/Epsilon +
		8*(-1)^m*x*Log[x]*Log[1 + x] + 8*(-1)^m*x^2*Log[x]*Log[1 + x] +
		8*(-1)^m*x*PolyLog[2, -x] + 8*(-1)^m*x^2*PolyLog[2, -x])*so[r]^m
,
(* r13i2.r *)
RTLI[{0, 0, 0, 0, m_ /; MCH[m]}, {{1, M_}, 1, 1, {1, M_}, {1, M_}}, r___] :>
	(Smu^2*x^(-1 + m)*((2*x + 2*(-1)^m*x)*Zeta2 +
			(1 + 3*(-1)^m + (-1 + x)^(-1) + x - 3*(-1)^m*x -
					(3*(-1)^m)/(1 + x))*Log[x]^2 + 4*(-1)^m*x*Log[x]*Log[1 + x] +
			(4*(-1)^m + 2*x - 2*(-1)^m*x - (4*(-1)^m)/(1 + x))*
				PolyLog[2, 1 - x] + 4*(-1)^m*x*PolyLog[2, -x])*so[r]^m)/M^2
,
(* r13i3.r *)
RTLI[{0, 0, 0, 0, m_ /; MCH[m]}, {{1, M_}, 1, 1, {1, M_}, {2, M_}}, r___] :>
	(Smu^2*x^(-1 + m)*(2/3 + (2*(-1)^m)/3 - 2/(3*(1 - x)) +
			(4*(-1)^m)/(3*(1 + x)^2) - (2*(-1)^m)/(1 + x) - (2*x*Zeta2)/3 -
			(2*(-1)^m*x*Zeta2)/3 - DeltaFunction[1 - x]/2 -
			DeltaFunction[1 - x]/(2*Epsilon) + Zeta2*DeltaFunction[1 - x] +
			(2*Log[x])/3 + (2*(-1)^m*Log[x])/3 + (2*Log[x])/(3*(1 - x)^2) -
			(4*Log[x])/(3*(1 - x)) + (4*(-1)^m*Log[x])/(3*(1 + x)^3) -
			(2*(-1)^m*Log[x])/(1 + x)^2 + Log[x]^2/(3*(1 - x)^3) -
			Log[x]^2/(3*(1 - x)^2) - (x*Log[x]^2)/3 + (-1)^m*x*Log[x]^2 +
			((-1)^m*Log[x]^2)/(1 + x)^3 - ((-1)^m*Log[x]^2)/(1 + x)^2 -
			(4*(-1)^m*x*Log[x]*Log[1 + x])/3 - (2*x*PolyLog[2, 1 - x])/3 +
			(2*(-1)^m*x*PolyLog[2, 1 - x])/3 +
			(4*(-1)^m*PolyLog[2, 1 - x])/(3*(1 + x)^3) -
			(4*(-1)^m*PolyLog[2, 1 - x])/(3*(1 + x)^2) -
			(4*(-1)^m*x*PolyLog[2, -x])/3)*so[r]^m)/M^4
,
(* n13i2.r*)
RTLI[{m_ /; MCH[m], 1, 0, 0, 0}, {{2, M_}, 1, 0, {1, M_}, {1, M_}}, r___] :>
	(Smu^2*x^(-1 + m)*(x^2*Zeta2 - x*Log[x] - (3*x^2*Log[x]^2)/2 +
			2*x^2*Log[x]*Log[1 + x] - x^2*PolyLog[2, 1 - x] +
			2*x^2*PolyLog[2, -x])*so[r]^(1 + m))/M^2
,
(* n13i3.r*)
RTLI[{m_ /; MCH[m], 1, 0, 0, 0}, {{2, M_}, {1, M_}, 0, 1, 1}, r___] :>
	(Smu^2*x^(-1 + m)*(x^2*Zeta2 - x*Log[x] + (x^2*Log[x]^2)/2 +
			x^2*PolyLog[2, 1 - x])*so[r]^(1 + m))/M^2
,
(* n13i4.r*)
RTLI[{2, 0, 0, 0, m_ /; MCH[m]}, {{1, M_}, 1, 1, {1, M_}, {1, M_}}, r___] :>
	(Smu^2*x^(-1 + m)*(-x - (-1)^m*x + x^2 + (-1)^m*x^2 +
			(x + (-1)^m*x + x^3 + (-1)^m*x^3)*Zeta2 +
			(-x^2 - (-1)^m*x^2)*Log[x] +
			(1 + 3*(-1)^m - (1 - x)^(-1) + x - 3*(-1)^m*x + x^2/2 +
					(3*(-1)^m*x^2)/2 + x^3/2 - (3*(-1)^m*x^3)/2 - (3*(-1)^m)/(1 + x)
)*Log[x]^2 + (2*(-1)^m*x + 2*(-1)^m*x^3)*Log[x]*Log[1 + x] +
			(4*(-1)^m + x - 3*(-1)^m*x + 2*(-1)^m*x^2 + x^3 - (-1)^m*x^3 -
					(4*(-1)^m)/(1 + x))*PolyLog[2, 1 - x] +
			(2*(-1)^m*x + 2*(-1)^m*x^3)*PolyLog[2, -x])*so[r]^(2 + m))/M^2
,
(* r14i1.r*)
RTLI[{m_ /; MCH[m], 1, 0, 0, 0}, {{1, M_}, 1, 1, {1, M_}, 0}, r___] :>
	Smu^2*x^(-1 + m)*(-x/2 - (2*x)/Epsilon^2 - (x*Zeta2)/2 + x*Log[1 - x] -
		x*Log[1 - x]^2 + (x - 2*x*Log[1 - x])/Epsilon)*so[r]^(1 + m)
,
(*r14i2.r*)
RTLI[{m_/;MCH[m], 2, 0, 0, 0}, {0, 1, 1, {1, M_}, {1, M_}}, r___] :>
	Smu^2*x^(-1 + m)*so[r]^(2 + m)*
	(x^2/2 + x^3 + 6*x*Zeta2 - (3*x^2*Zeta2)/2 - (3*x^3*Zeta2)/4 +
		(5*x*Log[1 - x])/2 - 2*x^2*Log[1 - x] - (3*x^3*Log[1 - x])/2 +
		(5*x*Zeta2*Log[1 - x])/2 + (3*x*Log[1 - x]^2)/4 - (x^2*Log[1 - x]^2)/4 -
		(x^3*Log[1 - x]^2)/8 + (x*Log[1 - x]^3)/12 +
		(2*x^2 + x^3 + 2*x*Log[1 - x])/Epsilon^2 + 2*x^2*Log[x] +
		4*x*Zeta2*Log[x] + x^2*Log[1 - x]*Log[x] + (x^3*Log[1 - x]*Log[x])/2 +
		2*x^2*Log[x]^2 + x^3*Log[x]^2 - 2*x*Log[1 - x]*Log[x]^2 -
		6*x*PolyLog[2, 1 - x] + x^2*PolyLog[2, 1 - x] +
		(x^3*PolyLog[2, 1 - x])/2 - x*Log[1 - x]*PolyLog[2, 1 - x] -
		4*x*Log[x]*PolyLog[2, 1 - x] +
		(-x^2 - x^3 + 4*x*Zeta2 + 3*x*Log[1 - x] - x^2*Log[1 - x] -
				(x^3*Log[1 - x])/2 + (x*Log[1 - x]^2)/2 + 4*x^2*Log[x] +
				2*x^3*Log[x] - 4*x*PolyLog[2, 1 - x])/Epsilon -
		2*x*PolyLog[3, 1 - x] - 4*x*PolyLog[3, x] + 2*x*Zeta[3])
,
(* r14i3.r *)
RTLI[{m_ /; MCH[m], 2, 0, 0, 0}, {{1, M_}, 1, 0, {1, M_}, {1, M_}}, r___] :>
	Smu^2*x^(-1 + m)*(x/2 - (3*x^2)/2 + x^3 + (-x + x^3)/Epsilon^2 -
		(x*Zeta2)/4 + (x^3*Zeta2)/4 + (x*Log[x])/2 + (x^2*Log[x])/2 -
		(3*x^3*Log[x])/2 + (x*Log[x]^2)/8 + (9*x^3*Log[x]^2)/8 +
		(x^2 - x^3 - (x*Log[x])/2 + (3*x^3*Log[x])/2)/Epsilon)*so[r]^(2 + m)
,
(* n14i1.r *)
RTLI[{m_ /; MCH[m], 2, 0, 0, 0}, {{1, M_}, 1, 1, {1, M_}, {1, M_}}, r___] :>
	(Smu^2*x^(-1 + m)*(-(x^2*Log[1 - x])/2 +
			(1 - (1 - x)^(-1) + x + x^2/2)*Log[x])*so[r]^(2 + m))/M^2
,
(* r17i01.r *)
RTLI[{0, -2 - OPEi + OPEm, 0, 0, OPEi}, {0, {1, M_}, {1, M_}, 1, {1, M_}},
	r___] :> (Smu^2*x^(-1 + OPEm)*so[r]^(-2 + OPEm)*
		(-(Zeta2*Log[x])/(2*(1 - x)) - ((-1)^OPEm*Zeta2*Log[x])/(1 + x) +
			(2*Log[1 - x]^2*Log[x])/(1 - x) + (Log[1 - x]*Log[x]^2)/(2*(1 - x)) +
			(2*(-1)^OPEm*Log[1 - x]*Log[x]^2)/(1 + x) + Log[x]^3/(12*(1 - x)) +
			((-1)^OPEm*Log[x]^3)/(2*(1 + x)) +
			((2*Log[x])/(1 - x) + (4*(-1)^OPEm*Log[x])/(1 + x))/Epsilon^2 -
			(2*(-1)^OPEm*Zeta2*Log[1 + x])/(1 + x) +
			(3*(-1)^OPEm*Log[x]^2*Log[1 + x])/(1 + x) +
			(2*(-1)^OPEm*Log[1 + x]^3)/(3*(1 + x)) +
			(Log[x]*PolyLog[2, 1 - x])/(1 - x) +
			(2*(-1)^OPEm*Log[x]*PolyLog[2, 1 - x])/(1 + x) +
			(2*(-1)^OPEm*Log[x]*PolyLog[2, -x])/(1 + x) +
			((2*(-1)^OPEm*Zeta2)/(1 + x) + (4*Log[1 - x]*Log[x])/(1 - x) -
					Log[x]^2/(2*(1 - x)) + (3*(-1)^OPEm*Log[x]^2)/(1 + x) +
					(4*(-1)^OPEm*Log[x]*Log[1 + x])/(1 + x) +
					(2*PolyLog[2, 1 - x])/(1 - x) +
					(4*(-1)^OPEm*PolyLog[2, -x])/(1 + x))/Epsilon +
			(2*PolyLog[3, 1 - x])/(1 - x) +
			(2*(-1)^OPEm*PolyLog[3, -x])/(1 + x) + PolyLog[3, x]/(1 - x) +
			(4*(-1)^OPEm*PolyLog[3, x])/(1 + x) -
			(4*(-1)^OPEm*PolyLog[3, (1 + x)^(-1)])/(1 + x) - Zeta[3]/(1 - x) +
			((-1)^OPEm*Zeta[3])/(1 + x)))/(-1)^OPEi
,
(* r17i02.r *)
RTLI[{0, -2 - OPEi + OPEm, 0, 0, OPEi}, {1, 0, {1, M_}, 1, {1, M_}}, r___] :>
	(Smu^2*x^(-1 + OPEm)*(Zeta2*Log[1 - x] + (11*Log[1 - x]^3)/6 +
			(4*Log[1 - x] - 4*Log[x])/Epsilon^2 - 2*Zeta2*Log[x] -
			(5*Log[1 - x]^2*Log[x])/2 - (Log[1 - x]*Log[x]^2)/2 - Log[x]^3/2 +
			(2*Zeta2 + 3*Log[1 - x]^2 - 2*Log[1 - x]*Log[x] - 3*Log[x]^2 -
					4*PolyLog[2, 1 - x])/Epsilon - 7*Log[1 - x]*PolyLog[2, 1 - x] +
			Log[x]*PolyLog[2, 1 - x] + 5*PolyLog[3, 1 - x] + 3*PolyLog[3, x])*
		so[r]^(-2 + OPEm))/(-1)^OPEi
,
(* r17i03.r *)
RTLI[{0, -2 - OPEi + OPEm, 0, 0, OPEi}, {1, {1, M_}, {1, M_}, 1, 0}, r___] :>
	(Smu^2*x^(-1 + OPEm)*so[r]^(-2 + OPEm)*
		(-7*Zeta2*Log[1 - x] + (14*Log[1 - x]^3)/3 + (-1)^OPEm*Zeta2*Log[x] +
			(2*(-1)^OPEm*Log[x]^3)/3 +
			(4*Log[1 - x] + 4*(-1)^OPEm*Log[x])/Epsilon^2 -
			8*(-1)^OPEm*Zeta2*Log[1 + x] + 4*(-1)^OPEm*Log[x]^2*Log[1 + x] +
			(8*(-1)^OPEm*Log[1 + x]^3)/3 + 8*(-1)^OPEm*Log[x]*PolyLog[2, -x] +
			(-4*Zeta2 + 4*(-1)^OPEm*Zeta2 + 6*Log[1 - x]^2 +
					2*(-1)^OPEm*Log[x]^2 + 8*(-1)^OPEm*Log[x]*Log[1 + x] +
					8*(-1)^OPEm*PolyLog[2, -x])/Epsilon - 8*(-1)^OPEm*PolyLog[3, -x] -
			16*(-1)^OPEm*PolyLog[3, (1 + x)^(-1)] + 8*Zeta[3] +
			8*(-1)^OPEm*Zeta[3]))/(-1)^OPEi
,
(* r17i04.r *)
RTLI[{0, -1 - OPEi + OPEm, 0, 0, OPEi}, {0, {1, M_}, {1, M_}, 1, {1, M_}},
	r___] :> (Smu^2*x^(-1 + OPEm)*so[r]^(-1 + OPEm)*
		((Zeta2*Log[x])/2 - ((-1)^OPEm*Zeta2*Log[x])/2 -
			(Zeta2*Log[x])/(2*(1 - x)) - ((-1)^OPEm*Zeta2*Log[x])/(1 + x) -
			2*Log[1 - x]^2*Log[x] + (2*Log[1 - x]^2*Log[x])/(1 - x) -
			(Log[1 - x]*Log[x]^2)/2 + (Log[1 - x]*Log[x]^2)/(2*(1 - x)) +
			(2*(-1)^OPEm*Log[1 - x]*Log[x]^2)/(1 + x) - Log[x]^3/12 +
			((-1)^OPEm*Log[x]^3)/12 + Log[x]^3/(12*(1 - x)) +
			((-1)^OPEm*Log[x]^3)/(2*(1 + x)) +
			(-2*Log[x] - 2*(-1)^OPEm*Log[x] + (2*Log[x])/(1 - x) +
					(4*(-1)^OPEm*Log[x])/(1 + x))/Epsilon^2 +
			2*(-1)^OPEm*Zeta2*Log[1 + x] -
			(2*(-1)^OPEm*Zeta2*Log[1 + x])/(1 + x) -
			3*(-1)^OPEm*Log[x]^2*Log[1 + x] +
			(3*(-1)^OPEm*Log[x]^2*Log[1 + x])/(1 + x) -
			(2*(-1)^OPEm*Log[1 + x]^3)/3 +
			(2*(-1)^OPEm*Log[1 + x]^3)/(3*(1 + x)) - Log[x]*PolyLog[2, 1 - x] +
			(Log[x]*PolyLog[2, 1 - x])/(1 - x) +
			(2*(-1)^OPEm*Log[x]*PolyLog[2, 1 - x])/(1 + x) -
			6*(-1)^OPEm*Log[x]*PolyLog[2, -x] +
			(2*(-1)^OPEm*Log[x]*PolyLog[2, -x])/(1 + x) +
			(-2*(-1)^OPEm*Zeta2 + (2*(-1)^OPEm*Zeta2)/(1 + x) -
					4*Log[1 - x]*Log[x] + (4*Log[1 - x]*Log[x])/(1 - x) +
					Log[x]^2/2 - ((-1)^OPEm*Log[x]^2)/2 - Log[x]^2/(2*(1 - x)) +
					(3*(-1)^OPEm*Log[x]^2)/(1 + x) - 4*(-1)^OPEm*Log[x]*Log[1 + x] +
					(4*(-1)^OPEm*Log[x]*Log[1 + x])/(1 + x) - 2*PolyLog[2, 1 - x] +
					(2*PolyLog[2, 1 - x])/(1 - x) - 4*(-1)^OPEm*PolyLog[2, -x] +
					(4*(-1)^OPEm*PolyLog[2, -x])/(1 + x))/Epsilon -
			2*PolyLog[3, 1 - x] + (2*PolyLog[3, 1 - x])/(1 - x) +
			6*(-1)^OPEm*PolyLog[3, -x] + (2*(-1)^OPEm*PolyLog[3, -x])/(1 + x) -
			PolyLog[3, x] + PolyLog[3, x]/(1 - x) +
			(4*(-1)^OPEm*PolyLog[3, x])/(1 + x) +
			4*(-1)^OPEm*PolyLog[3, (1 + x)^(-1)] -
			(4*(-1)^OPEm*PolyLog[3, (1 + x)^(-1)])/(1 + x) + Zeta[3] +
			(-1)^OPEm*Zeta[3] - Zeta[3]/(1 - x) + ((-1)^OPEm*Zeta[3])/(1 + x)))/
	(-1)^OPEi
,
(* r17i05.r *)
RTLI[{0, -1 - OPEi + OPEm, 0, 0, OPEi}, {1, 0, {1, M_}, 1, {1, M_}}, r___] :>
	(Smu^2*x^(-1 + OPEm)*so[r]^(-1 + OPEm)*
		(x*Zeta2*Log[1 - x] + (11*x*Log[1 - x]^3)/6 - (Zeta2*Log[x])/2 -
			2*x*Zeta2*Log[x] - 4*Log[1 - x]^2*Log[x] -
			(5*x*Log[1 - x]^2*Log[x])/2 - (x*Log[1 - x]*Log[x]^2)/2 +
			Log[x]^3/12 - (x*Log[x]^3)/2 +
			(4*x*Log[1 - x] - 2*Log[x] - 4*x*Log[x])/Epsilon^2 -
			8*Log[1 - x]*PolyLog[2, 1 - x] - 7*x*Log[1 - x]*PolyLog[2, 1 - x] +
			x*Log[x]*PolyLog[2, 1 - x] +
			(2*x*Zeta2 + 3*x*Log[1 - x]^2 - 4*Log[1 - x]*Log[x] -
					2*x*Log[1 - x]*Log[x] - Log[x]^2/2 - 3*x*Log[x]^2 -
					4*PolyLog[2, 1 - x] - 4*x*PolyLog[2, 1 - x])/Epsilon +
			8*PolyLog[3, 1 - x] + 5*x*PolyLog[3, 1 - x] + 2*PolyLog[3, x] +
			3*x*PolyLog[3, x] - 2*Zeta[3]))/(-1)^OPEi
,
(* r17i06.r *)
RTLI[{0, -1 - OPEi + OPEm, 0, 0, OPEi}, {1, {1, M_}, 0, 1, {1, M_}}, r___] :>
	(Smu^2*x^(-1 + OPEm)*((-11*Zeta2*Log[1 - x])/2 + (35*Log[1 - x]^3)/12 +
			(2*Log[1 - x] - 2*Log[x])/Epsilon^2 - (3*Zeta2*Log[x])/2 -
			4*Log[1 - x]^2*Log[x] + (Log[1 - x]*Log[x]^2)/2 + Log[x]^3/12 +
			(-2*Zeta2 + (7*Log[1 - x]^2)/2 - 4*Log[1 - x]*Log[x] -
					Log[x]^2/2 - 2*PolyLog[2, 1 - x])/Epsilon -
			5*Log[1 - x]*PolyLog[2, 1 - x] + Log[x]*PolyLog[2, 1 - x] +
			6*PolyLog[3, 1 - x] + 3*PolyLog[3, x])*so[r]^(-1 + OPEm))/(-1)^OPEi
,
(* r17i07.r *)
RTLI[{0, -1 - OPEi + OPEm, 0, 0, OPEi}, {1, {1, M_}, {1, M_}, 1, 0}, r___] :>
	(Smu^2*x^(-1 + OPEm)*so[r]^(-1 + OPEm)*
		(16 - 16*(-1)^OPEm - 16*x + 16*(-1)^OPEm*x - 3*Zeta2 +
			3*(-1)^OPEm*Zeta2 + 3*x*Zeta2 + 5*(-1)^OPEm*x*Zeta2 - 16*Log[1 - x] +
			16*x*Log[1 - x] - 7*x*Zeta2*Log[1 - x] + 8*Log[1 - x]^2 -
			8*x*Log[1 - x]^2 + (14*x*Log[1 - x]^3)/3 - 16*(-1)^OPEm*x*Log[x] -
			(-1)^OPEm*x*Zeta2*Log[x] + 4*(-1)^OPEm*x*Log[x]^2 -
			(2*(-1)^OPEm*x*Log[x]^3)/3 +
			(4 - 4*(-1)^OPEm - 4*x + 4*(-1)^OPEm*x + 4*x*Log[1 - x] -
					4*(-1)^OPEm*x*Log[x])/Epsilon^2 + 8*(-1)^OPEm*x*Zeta2*Log[1 + x] +
			8*(-1)^OPEm*Log[x]*Log[1 + x] + 8*(-1)^OPEm*x*Log[x]*Log[1 + x] -
			4*(-1)^OPEm*x*Log[x]^2*Log[1 + x] - (8*(-1)^OPEm*x*Log[1 + x]^3)/3 +
			8*(-1)^OPEm*PolyLog[2, -x] + 8*(-1)^OPEm*x*PolyLog[2, -x] -
			8*(-1)^OPEm*x*Log[x]*PolyLog[2, -x] +
			(-8 + 8*(-1)^OPEm + 8*x - 8*(-1)^OPEm*x - 4*x*Zeta2 -
					4*(-1)^OPEm*x*Zeta2 + 8*Log[1 - x] - 8*x*Log[1 - x] +
					6*x*Log[1 - x]^2 + 8*(-1)^OPEm*x*Log[x] -
					2*(-1)^OPEm*x*Log[x]^2 - 8*(-1)^OPEm*x*Log[x]*Log[1 + x] -
					8*(-1)^OPEm*x*PolyLog[2, -x])/Epsilon +
			8*(-1)^OPEm*x*PolyLog[3, -x] +
			16*(-1)^OPEm*x*PolyLog[3, (1 + x)^(-1)] + 8*x*Zeta[3] -
			8*(-1)^OPEm*x*Zeta[3]))/(-1)^OPEi
,
(* r17i08.r *)
RTLI[{0, -1 - OPEi + OPEm, 0, 0, OPEi}, {1, {1, M_}, {1, M_}, 1, {1, M_}},
	r___] :> ((-1)^OPEi*Smu^2*x^(-1 + OPEm)*so[r]^(-1 + OPEm)*
		((-3 + (-1)^OPEm + (1 - x)^(-1) + (-1)^OPEm/(1 + x))*Zeta2 +
			(1 - (-1)^OPEm + (1 - x)^(-2) - 2/(1 - x) +
					(3*(-1)^OPEm)/(1 + x)^2 - (2*(-1)^OPEm)/(1 + x))*Log[x]^2 +
			(-2 + 2*(-1)^OPEm + 2/(1 - x) + (2*(-1)^OPEm)/(1 + x))*Log[x]*
				Log[1 + x] + (-2/(1 - x) + (4*(-1)^OPEm)/(1 + x)^2 -
					(2*(-1)^OPEm)/(1 + x))*PolyLog[2, 1 - x] +
			(-2 + 2*(-1)^OPEm + 2/(1 - x) + (2*(-1)^OPEm)/(1 + x))*
				PolyLog[2, -x] + DeltaFunction[1 - x]*(2*Zeta2 + 2*Zeta[3])))/M^2
,
(* r17i09.r *)
RTLI[{0, -OPEi + OPEm, 0, 0, OPEi}, {0, {1, M_}, {1, M_}, 1, {1, M_}},
	r___] :> (Smu^2*x^(-1 + OPEm)*so[r]^OPEm*
		(2*(-1)^OPEm - 2*(-1)^OPEm*x + ((-1)^OPEm*Zeta2)/2 -
			((-1)^OPEm*x*Zeta2)/2 - (-1)^OPEm*Log[x] + 3*(-1)^OPEm*x*Log[x] +
			(Zeta2*Log[x])/2 - ((-1)^OPEm*Zeta2*Log[x])/2 -
			(Zeta2*Log[x])/(2*(1 - x)) + (x*Zeta2*Log[x])/2 +
			((-1)^OPEm*x*Zeta2*Log[x])/2 - ((-1)^OPEm*Zeta2*Log[x])/(1 + x) -
			2*Log[1 - x]^2*Log[x] + (2*Log[1 - x]^2*Log[x])/(1 - x) -
			2*x*Log[1 - x]^2*Log[x] - ((-1)^OPEm*Log[x]^2)/4 -
			(9*(-1)^OPEm*x*Log[x]^2)/4 - (Log[1 - x]*Log[x]^2)/2 +
			(Log[1 - x]*Log[x]^2)/(2*(1 - x)) - (x*Log[1 - x]*Log[x]^2)/2 +
			(2*(-1)^OPEm*Log[1 - x]*Log[x]^2)/(1 + x) - Log[x]^3/12 +
			((-1)^OPEm*Log[x]^3)/12 + Log[x]^3/(12*(1 - x)) - (x*Log[x]^3)/12 -
			((-1)^OPEm*x*Log[x]^3)/12 + ((-1)^OPEm*Log[x]^3)/(2*(1 + x)) +
			(2*(-1)^OPEm - 2*(-1)^OPEm*x - 2*Log[x] - 2*(-1)^OPEm*Log[x] +
					(2*Log[x])/(1 - x) - 2*x*Log[x] + 2*(-1)^OPEm*x*Log[x] +
					(4*(-1)^OPEm*Log[x])/(1 + x))/Epsilon^2 +
			2*(-1)^OPEm*Zeta2*Log[1 + x] - 2*(-1)^OPEm*x*Zeta2*Log[1 + x] -
			(2*(-1)^OPEm*Zeta2*Log[1 + x])/(1 + x) -
			3*(-1)^OPEm*Log[x]^2*Log[1 + x] + 3*(-1)^OPEm*x*Log[x]^2*Log[1 + x] +
			(3*(-1)^OPEm*Log[x]^2*Log[1 + x])/(1 + x) -
			(2*(-1)^OPEm*Log[1 + x]^3)/3 + (2*(-1)^OPEm*x*Log[1 + x]^3)/3 +
			(2*(-1)^OPEm*Log[1 + x]^3)/(3*(1 + x)) - Log[x]*PolyLog[2, 1 - x] +
			(Log[x]*PolyLog[2, 1 - x])/(1 - x) - x*Log[x]*PolyLog[2, 1 - x] +
			(2*(-1)^OPEm*Log[x]*PolyLog[2, 1 - x])/(1 + x) -
			6*(-1)^OPEm*Log[x]*PolyLog[2, -x] +
			6*(-1)^OPEm*x*Log[x]*PolyLog[2, -x] +
			(2*(-1)^OPEm*Log[x]*PolyLog[2, -x])/(1 + x) +
			(-2*(-1)^OPEm + 2*(-1)^OPEm*x - 2*(-1)^OPEm*Zeta2 +
					2*(-1)^OPEm*x*Zeta2 + (2*(-1)^OPEm*Zeta2)/(1 + x) +
					(-1)^OPEm*Log[x] - 3*(-1)^OPEm*x*Log[x] - 4*Log[1 - x]*Log[x] +
					(4*Log[1 - x]*Log[x])/(1 - x) - 4*x*Log[1 - x]*Log[x] +
					Log[x]^2/2 - ((-1)^OPEm*Log[x]^2)/2 - Log[x]^2/(2*(1 - x)) +
					(x*Log[x]^2)/2 + ((-1)^OPEm*x*Log[x]^2)/2 +
					(3*(-1)^OPEm*Log[x]^2)/(1 + x) - 4*(-1)^OPEm*Log[x]*Log[1 + x] +
					4*(-1)^OPEm*x*Log[x]*Log[1 + x] +
					(4*(-1)^OPEm*Log[x]*Log[1 + x])/(1 + x) - 2*PolyLog[2, 1 - x] +
					(2*PolyLog[2, 1 - x])/(1 - x) - 2*x*PolyLog[2, 1 - x] -
					4*(-1)^OPEm*PolyLog[2, -x] + 4*(-1)^OPEm*x*PolyLog[2, -x] +
					(4*(-1)^OPEm*PolyLog[2, -x])/(1 + x))/Epsilon -
			2*PolyLog[3, 1 - x] + (2*PolyLog[3, 1 - x])/(1 - x) -
			2*x*PolyLog[3, 1 - x] + 6*(-1)^OPEm*PolyLog[3, -x] -
			6*(-1)^OPEm*x*PolyLog[3, -x] + (2*(-1)^OPEm*PolyLog[3, -x])/(1 + x) -
			PolyLog[3, x] + PolyLog[3, x]/(1 - x) - x*PolyLog[3, x] +
			(4*(-1)^OPEm*PolyLog[3, x])/(1 + x) +
			4*(-1)^OPEm*PolyLog[3, (1 + x)^(-1)] -
			4*(-1)^OPEm*x*PolyLog[3, (1 + x)^(-1)] -
			(4*(-1)^OPEm*PolyLog[3, (1 + x)^(-1)])/(1 + x) + Zeta[3] +
			(-1)^OPEm*Zeta[3] - Zeta[3]/(1 - x) + x*Zeta[3] -
			(-1)^OPEm*x*Zeta[3] + ((-1)^OPEm*Zeta[3])/(1 + x)))/(-1)^OPEi
,
(* r17i10.r *)
RTLI[{0, -OPEi + OPEm, 0, 0, OPEi}, {1, {1, M_}, 0, 1, {1, M_}}, r___] :>
	(Smu^2*x^(-1 + OPEm)*(2 - 2*x - (3*Zeta2)/2 + (3*x*Zeta2)/2 -
			4*Log[1 - x] + 4*x*Log[1 - x] - (11*x*Zeta2*Log[1 - x])/2 +
			4*Log[1 - x]^2 - 4*x*Log[1 - x]^2 + (35*x*Log[1 - x]^3)/12 -
			Log[x] - x*Log[x] - (3*x*Zeta2*Log[x])/2 + 2*Log[1 - x]*Log[x] +
			2*x*Log[1 - x]*Log[x] - 4*x*Log[1 - x]^2*Log[x] - Log[x]^2/4 +
			(3*x*Log[x]^2)/4 + (x*Log[1 - x]*Log[x]^2)/2 + (x*Log[x]^3)/12 +
			(2 - 2*x + 2*x*Log[1 - x] - 2*x*Log[x])/Epsilon^2 +
			2*PolyLog[2, 1 - x] + 2*x*PolyLog[2, 1 - x] -
			5*x*Log[1 - x]*PolyLog[2, 1 - x] + x*Log[x]*PolyLog[2, 1 - x] +
			(-2 + 2*x - 2*x*Zeta2 + 4*Log[1 - x] - 4*x*Log[1 - x] +
					(7*x*Log[1 - x]^2)/2 + Log[x] + x*Log[x] -
					4*x*Log[1 - x]*Log[x] - (x*Log[x]^2)/2 - 2*x*PolyLog[2, 1 - x])/
				Epsilon + 6*x*PolyLog[3, 1 - x] + 3*x*PolyLog[3, x])*so[r]^OPEm)/
	(-1)^OPEi
,
(* r17i11.r *)
RTLI[{1, -2 - OPEi + OPEm, 0, 0, OPEi}, {0, {1, M_}, {1, M_}, 1, {1, M_}},
	r___] :> (Smu^2*x^(-1 + OPEm)*so[r]^(-1 + OPEm)*
		(-2 - Zeta2/2 - 2*Log[1 - x]^2 + 3*Log[x] - (4*Log[x])/(1 - x) -
			(3*(-1)^OPEm*Zeta2*Log[x])/2 - Log[x]^2/4 +
			2*(-1)^OPEm*Log[1 - x]*Log[x]^2 + (7*(-1)^OPEm*Log[x]^3)/12 +
			(-2 + 2*(-1)^OPEm*Log[x])/Epsilon^2 +
			(2 - 4*Log[1 - x] + Log[x] + (5*(-1)^OPEm*Log[x]^2)/2)/Epsilon +
			2*(-1)^OPEm*Log[x]*PolyLog[2, 1 - x] -
			4*(-1)^OPEm*Log[x]*PolyLog[2, -x] + 8*(-1)^OPEm*PolyLog[3, -x] +
			4*(-1)^OPEm*PolyLog[3, x] + 2*(-1)^OPEm*Zeta[3]))/(-1)^OPEi
,
(* r17i12.r *)
RTLI[{1, -2 - OPEi + OPEm, 0, 0, OPEi}, {1, 0, {1, M_}, 1, {1, M_}}, r___] :>
	(Smu^2*x^(-1 + OPEm)*((5*Zeta2*Log[1 - x])/2 + Log[1 - x]^3/12 +
			(2*Log[1 - x] - 2*Log[x])/Epsilon^2 + (7*Zeta2*Log[x])/2 -
			4*Log[1 - x]^2*Log[x] - 2*Log[1 - x]*Log[x]^2 + Log[x]^3/12 +
			(4*Zeta2 + Log[1 - x]^2/2 - 4*Log[1 - x]*Log[x] - Log[x]^2/2 -
					8*PolyLog[2, 1 - x])/Epsilon - 9*Log[1 - x]*PolyLog[2, 1 - x] -
			4*Log[x]*PolyLog[2, 1 - x] + 6*PolyLog[3, 1 - x] - 2*PolyLog[3, x])*
		so[r]^(-1 + OPEm))/(-1)^OPEi
,
(* r17i13.r *)
RTLI[{1, -2 - OPEi + OPEm, 0, 0, OPEi}, {1, {1, M_}, {1, M_}, 1, 0}, r___] :>
	(Smu^2*x^(-1 + m)*(12*x^(-m + OPEm) - 16*(-1)^OPEm*x^(-m + OPEm) -
			16*x^(1 - m + OPEm) + 16*(-1)^OPEm*x^(1 - m + OPEm) +
			(-4*(-1)^OPEm*x^(-m + OPEm) - 4*x^(1 - m + OPEm) +
					4*(-1)^OPEm*x^(1 - m + OPEm))/Epsilon^2 - 4*x^(-m + OPEm)*Zeta2 +
			3*(-1)^OPEm*x^(-m + OPEm)*Zeta2 + 3*x^(1 - m + OPEm)*Zeta2 +
			5*(-1)^OPEm*x^(1 - m + OPEm)*Zeta2 - 12*x^(-m + OPEm)*Log[1 - x] +
			16*x^(1 - m + OPEm)*Log[1 - x] + 6*x^(-m + OPEm)*Log[1 - x]^2 -
			8*x^(1 - m + OPEm)*Log[1 - x]^2 -
			16*(-1)^OPEm*x^(1 - m + OPEm)*Log[x] +
			4*(-1)^OPEm*x^(1 - m + OPEm)*Log[x]^2 +
			(-4*x^(-m + OPEm) + 8*(-1)^OPEm*x^(-m + OPEm) +
					8*x^(1 - m + OPEm) - 8*(-1)^OPEm*x^(1 - m + OPEm) +
					4*x^(-m + OPEm)*Log[1 - x] - 8*x^(1 - m + OPEm)*Log[1 - x] +
					8*(-1)^OPEm*x^(1 - m + OPEm)*Log[x])/Epsilon +
			8*(-1)^OPEm*x^(-m + OPEm)*Log[x]*Log[1 + x] +
			8*(-1)^OPEm*x^(1 - m + OPEm)*Log[x]*Log[1 + x] +
			8*(-1)^OPEm*x^(-m + OPEm)*PolyLog[2, -x] +
			8*(-1)^OPEm*x^(1 - m + OPEm)*PolyLog[2, -x])*so[r]^(-1 + OPEm))/
	(-1)^OPEi
,
(* r17i14.r *)
RTLI[{1, -2 - OPEi + OPEm, 0, 0, OPEi}, {1, {1, M_}, {1, M_}, 1, {1, M_}},
	r___] :> (Smu^2*x^(-1 + OPEm)*
		((1 + 2*(-1)^OPEm - 2/(1 - x))*Zeta2 + Log[1 - x]^2/2 +
			(-1 + (1 - x)^(-1))*Log[1 - x]*Log[x] +
			(-5/2 - 3*(-1)^OPEm + 5/(2*(1 - x)) + (3*(-1)^OPEm)/(1 + x))*
				Log[x]^2 + (2 + 4*(-1)^OPEm - 4/(1 - x))*Log[x]*Log[1 + x] +
			(-4 - 2*(-1)^OPEm + (1 - x)^(-1) + (4*(-1)^OPEm)/(1 + x))*
				PolyLog[2, 1 - x] + (2 + 4*(-1)^OPEm - 4/(1 - x))*PolyLog[2, -x])*
		so[r]^(-1 + OPEm))/((-1)^OPEi*M^2)
,
(* r17i15.r *)
RTLI[{1, -1 - OPEi + OPEm, 0, 0, OPEi}, {0, {1, M_}, {1, M_}, 1, {1, M_}},
	r___] :> (Smu^2*x^(-1 + OPEm)*so[r]^OPEm*
		(2*(-1)^OPEm - 2*x - 2*(-1)^OPEm*x + ((-1)^OPEm*Zeta2)/2 -
			(x*Zeta2)/2 - ((-1)^OPEm*x*Zeta2)/2 - 2*x*Log[1 - x]^2 + 4*Log[x] -
			(-1)^OPEm*Log[x] - (4*Log[x])/(1 - x) + 3*x*Log[x] +
			3*(-1)^OPEm*x*Log[x] - (3*(-1)^OPEm*Zeta2*Log[x])/2 -
			((-1)^OPEm*Log[x]^2)/4 - (x*Log[x]^2)/4 -
			(9*(-1)^OPEm*x*Log[x]^2)/4 + 2*(-1)^OPEm*Log[1 - x]*Log[x]^2 +
			(7*(-1)^OPEm*Log[x]^3)/12 +
			(2*(-1)^OPEm - 2*x - 2*(-1)^OPEm*x + 2*(-1)^OPEm*Log[x])/Epsilon^2 +
			(-2*(-1)^OPEm + 2*x + 2*(-1)^OPEm*x - 4*x*Log[1 - x] +
					(-1)^OPEm*Log[x] + x*Log[x] - 3*(-1)^OPEm*x*Log[x] +
					(5*(-1)^OPEm*Log[x]^2)/2)/Epsilon +
			2*(-1)^OPEm*Log[x]*PolyLog[2, 1 - x] -
			4*(-1)^OPEm*Log[x]*PolyLog[2, -x] + 8*(-1)^OPEm*PolyLog[3, -x] +
			4*(-1)^OPEm*PolyLog[3, x] + 2*(-1)^OPEm*Zeta[3]))/(-1)^OPEi
,
(* r17i16.r *)
RTLI[{1, -1 - OPEi + OPEm, 0, 0, OPEi}, {1, 0, {1, M_}, 1, {1, M_}}, r___] :>
	(Smu^2*x^(-1 + OPEm)*so[r]^OPEm*
		(-2 + 2*x + (3*Zeta2)/2 - (3*x*Zeta2)/2 + 4*Log[1 - x] -
			4*x*Log[1 - x] + (5*x*Zeta2*Log[1 - x])/2 - 4*Log[1 - x]^2 +
			4*x*Log[1 - x]^2 + (x*Log[1 - x]^3)/12 + Log[x] + x*Log[x] -
			(Zeta2*Log[x])/2 + (7*x*Zeta2*Log[x])/2 - 2*Log[1 - x]*Log[x] -
			2*x*Log[1 - x]*Log[x] - 4*Log[1 - x]^2*Log[x] -
			4*x*Log[1 - x]^2*Log[x] + Log[x]^2/4 - (3*x*Log[x]^2)/4 -
			2*x*Log[1 - x]*Log[x]^2 + Log[x]^3/12 + (x*Log[x]^3)/12 +
			(-2 + 2*x + 2*x*Log[1 - x] - 2*Log[x] - 2*x*Log[x])/Epsilon^2 -
			2*PolyLog[2, 1 - x] - 2*x*PolyLog[2, 1 - x] -
			8*Log[1 - x]*PolyLog[2, 1 - x] - 9*x*Log[1 - x]*PolyLog[2, 1 - x] -
			4*x*Log[x]*PolyLog[2, 1 - x] +
			(2 - 2*x + 4*x*Zeta2 - 4*Log[1 - x] + 4*x*Log[1 - x] +
					(x*Log[1 - x]^2)/2 - Log[x] - x*Log[x] - 4*Log[1 - x]*Log[x] -
					4*x*Log[1 - x]*Log[x] - Log[x]^2/2 - (x*Log[x]^2)/2 -
					4*PolyLog[2, 1 - x] - 8*x*PolyLog[2, 1 - x])/Epsilon +
			8*PolyLog[3, 1 - x] + 6*x*PolyLog[3, 1 - x] + 2*PolyLog[3, x] -
			2*x*PolyLog[3, x] - 2*Zeta[3]))/(-1)^OPEi
,
(* r17i17.r *)
RTLI[{1, -1 - OPEi + OPEm, 0, 0, OPEi}, {1, {1, M_}, 0, 1, {1, M_}}, r___] :>
	(Smu^2*x^(-1 + OPEm)*(2 + (2 - 4*x)/Epsilon^2 - 4*x - (3*Zeta2)/2 +
			3*x*Zeta2 - 4*Log[1 - x] + 7*x*Log[1 - x] + 4*Log[1 - x]^2 -
			(23*x*Log[1 - x]^2)/4 - Log[x] - x*Log[x] + 2*Log[1 - x]*Log[x] +
			x*Log[1 - x]*Log[x] - Log[x]^2/4 + (3*x*Log[x]^2)/4 +
			(-2 + 4*x + 4*Log[1 - x] - 7*x*Log[1 - x] + Log[x] + x*Log[x])/
				Epsilon + 2*PolyLog[2, 1 - x] + x*PolyLog[2, 1 - x])*so[r]^OPEm)/
	(-1)^OPEi
,
(* r17i18.r *)
RTLI[{1, -1 - OPEi + OPEm, 0, 0, OPEi}, {1, {1, M_}, {1, M_}, 1, {1, M_}},
	r___] :> (Smu^2*x^(-1 + OPEm)*
		((1 + (-1)^OPEm - 2/(1 - x) + 2*x - (-1)^OPEm*x)*Zeta2 +
			(x*Log[1 - x]^2)/2 + (-1 + (1 - x)^(-1) - x)*Log[1 - x]*Log[x] +
			(-5/2 - 3*(-1)^OPEm + 5/(2*(1 - x)) - 2*x + (3*(-1)^OPEm*x)/2 +
					(3*(-1)^OPEm)/(1 + x))*Log[x]^2 +
			(4 + 2*(-1)^OPEm - 4/(1 - x) + 2*x - 2*(-1)^OPEm*x)*Log[x]*
				Log[1 + x] + (-2 - 3*(-1)^OPEm + (1 - x)^(-1) - 3*x +
					(-1)^OPEm*x + (4*(-1)^OPEm)/(1 + x))*PolyLog[2, 1 - x] +
			(4 + 2*(-1)^OPEm - 4/(1 - x) + 2*x - 2*(-1)^OPEm*x)*PolyLog[2, -x])*
		so[r]^OPEm)/((-1)^OPEi*M^2)
,
(* r17i19.r *)
RTLI[{2, -2 - OPEi + OPEm, 0, 0, OPEi}, {1, 0, {1, M_}, 1, {1, M_}}, r___] :>
	(Smu^2*x^(-1 + OPEm)*(-2 + 4*x + (11*Zeta2)/2 - 3*x*Zeta2 +
			6*Log[1 - x] - 7*x*Log[1 - x] + (5*Zeta2*Log[1 - x])/2 -
			(7*Log[1 - x]^2)/2 + (15*x*Log[1 - x]^2)/4 + Log[1 - x]^3/12 +
			(-2 + 4*x + 2*Log[1 - x] - 2*Log[x])/Epsilon^2 + Log[x] + x*Log[x] +
			(7*Zeta2*Log[x])/2 - 2*Log[1 - x]*Log[x] - x*Log[1 - x]*Log[x] -
			4*Log[1 - x]^2*Log[x] + Log[x]^2/4 + (5*x*Log[x]^2)/4 -
			2*Log[1 - x]*Log[x]^2 + Log[x]^3/12 +
			(2 - 4*x + 4*Zeta2 - 2*Log[1 - x] + 3*x*Log[1 - x] +
					Log[1 - x]^2/2 - Log[x] + 3*x*Log[x] - 4*Log[1 - x]*Log[x] -
					Log[x]^2/2 - 8*PolyLog[2, 1 - x])/Epsilon - 6*PolyLog[2, 1 - x] -
			x*PolyLog[2, 1 - x] - 9*Log[1 - x]*PolyLog[2, 1 - x] -
			4*Log[x]*PolyLog[2, 1 - x] + 6*PolyLog[3, 1 - x] - 2*PolyLog[3, x])*
		so[r]^OPEm)/(-1)^OPEi
,
(* r17i20.r *)
RTLI[{-2 - OPEi + OPEm, 1, 0, 0, OPEi}, {{1, M_}, 1, 0, {1, M_}, {1, M_}},
	r___] :> Smu^2*x^(-1 + OPEm)*so[r]^(-1 + OPEm)*
	((-3*Zeta2*Log[x])/2 - (3*(-1)^OPEm*Zeta2*Log[x])/2 +
		2*Log[1 - x]*Log[x]^2 + 2*(-1)^OPEm*Log[1 - x]*Log[x]^2 +
		(7*Log[x]^3)/12 + (7*(-1)^OPEm*Log[x]^3)/12 +
		(2*Log[x] + 2*(-1)^OPEm*Log[x])/Epsilon^2 +
		((5*Log[x]^2)/2 + (5*(-1)^OPEm*Log[x]^2)/2)/Epsilon +
		2*Log[x]*PolyLog[2, 1 - x] + 2*(-1)^OPEm*Log[x]*PolyLog[2, 1 - x] -
		4*Log[x]*PolyLog[2, -x] - 4*(-1)^OPEm*Log[x]*PolyLog[2, -x] +
		8*PolyLog[3, -x] + 8*(-1)^OPEm*PolyLog[3, -x] + 4*PolyLog[3, x] +
		4*(-1)^OPEm*PolyLog[3, x] + 2*Zeta[3] + 2*(-1)^OPEm*Zeta[3])
,
(* r17i21.r *)
RTLI[{-2 - OPEi + OPEm, 2, 0, 0, OPEi}, {{1, M_}, 1, 0, {1, M_}, {1, M_}},
	r___] :> Smu^2*x^(-1 + OPEm)*
	(-2 - 2*(-1)^OPEm + 2*x + 2*(-1)^OPEm*x +
		(-2 - 2*(-1)^OPEm + 2*x + 2*(-1)^OPEm*x)/Epsilon^2 - Zeta2/2 -
		((-1)^OPEm*Zeta2)/2 + (x*Zeta2)/2 + ((-1)^OPEm*x*Zeta2)/2 + Log[x] +
		(-1)^OPEm*Log[x] - 3*x*Log[x] - 3*(-1)^OPEm*x*Log[x] + Log[x]^2/4 +
		((-1)^OPEm*Log[x]^2)/4 + (9*x*Log[x]^2)/4 +
		(9*(-1)^OPEm*x*Log[x]^2)/4 +
		(2 + 2*(-1)^OPEm - 2*x - 2*(-1)^OPEm*x - Log[x] - (-1)^OPEm*Log[x] +
				3*x*Log[x] + 3*(-1)^OPEm*x*Log[x])/Epsilon)*so[r]^OPEm
,
(* r17i22.r *)
RTLI[{-1 - OPEi + OPEm, 0, 0, 0, OPEi}, {{1, M_}, 1, 0, {1, M_}, {1, M_}},
	r___] :> Smu^2*x^(-1 + OPEm)*so[r]^(-1 + OPEm)*
	(-2 + 2*(-1)^OPEm + 2*x - 2*(-1)^OPEm*x - (3*Zeta2)/2 +
		(3*(-1)^OPEm*Zeta2)/2 + (3*x*Zeta2)/2 - (3*(-1)^OPEm*x*Zeta2)/2 -
		4*Log[x] + 4*(-1)^OPEm*Log[x] + 2*x*Log[x] - 2*(-1)^OPEm*x*Log[x] -
		(3*(-1)^OPEm*Zeta2*Log[x])/2 - 2*Log[x]^2 + 2*(-1)^OPEm*Log[x]^2 -
		(x*Log[x]^2)/2 + ((-1)^OPEm*x*Log[x]^2)/2 +
		2*(-1)^OPEm*Log[1 - x]*Log[x]^2 + (7*(-1)^OPEm*Log[x]^3)/12 +
		(-2 + 2*(-1)^OPEm + 2*x - 2*(-1)^OPEm*x + 2*(-1)^OPEm*Log[x])/
			Epsilon^2 + (-2 + 2*(-1)^OPEm + 2*x - 2*(-1)^OPEm*x - 4*Log[x] +
				4*(-1)^OPEm*Log[x] + 2*x*Log[x] - 2*(-1)^OPEm*x*Log[x] +
				(5*(-1)^OPEm*Log[x]^2)/2)/Epsilon - 2*Log[x]*Log[1 + x] +
		2*(-1)^OPEm*Log[x]*Log[1 + x] + 2*x*Log[x]*Log[1 + x] -
		2*(-1)^OPEm*x*Log[x]*Log[1 + x] + PolyLog[2, 1 - x] -
		(-1)^OPEm*PolyLog[2, 1 - x] - x*PolyLog[2, 1 - x] +
		(-1)^OPEm*x*PolyLog[2, 1 - x] + 2*(-1)^OPEm*Log[x]*PolyLog[2, 1 - x] -
		2*PolyLog[2, -x] + 2*(-1)^OPEm*PolyLog[2, -x] + 2*x*PolyLog[2, -x] -
		2*(-1)^OPEm*x*PolyLog[2, -x] - 4*(-1)^OPEm*Log[x]*PolyLog[2, -x] +
		8*(-1)^OPEm*PolyLog[3, -x] + 4*(-1)^OPEm*PolyLog[3, x] +
		2*(-1)^OPEm*Zeta[3])
,
(* r17i23.r *)
RTLI[{-1 - OPEi + OPEm, 1, 0, 0, OPEi}, {{1, M_}, 1, 0, {1, M_}, {1, M_}},
	r___] :> Smu^2*x^(-1 + OPEm)*so[r]^OPEm*
	(-2*(-1)^OPEm + 2*(-1)^OPEm*x - ((-1)^OPEm*Zeta2)/2 +
		((-1)^OPEm*x*Zeta2)/2 + (-1)^OPEm*Log[x] - 3*(-1)^OPEm*x*Log[x] -
		(3*x*Zeta2*Log[x])/2 + (3*(-1)^OPEm*x*Zeta2*Log[x])/2 +
		((-1)^OPEm*Log[x]^2)/4 + (9*(-1)^OPEm*x*Log[x]^2)/4 +
		2*x*Log[1 - x]*Log[x]^2 - 2*(-1)^OPEm*x*Log[1 - x]*Log[x]^2 +
		(7*x*Log[x]^3)/12 - (7*(-1)^OPEm*x*Log[x]^3)/12 +
		(-2*(-1)^OPEm + 2*(-1)^OPEm*x + 2*x*Log[x] - 2*(-1)^OPEm*x*Log[x])/
			Epsilon^2 + (2*(-1)^OPEm - 2*(-1)^OPEm*x - (-1)^OPEm*Log[x] +
				3*(-1)^OPEm*x*Log[x] + (5*x*Log[x]^2)/2 - (5*(-1)^OPEm*x*Log[x]^2)/2
)/Epsilon + 2*x*Log[x]*PolyLog[2, 1 - x] -
		2*(-1)^OPEm*x*Log[x]*PolyLog[2, 1 - x] - 4*x*Log[x]*PolyLog[2, -x] +
		4*(-1)^OPEm*x*Log[x]*PolyLog[2, -x] + 8*x*PolyLog[3, -x] -
		8*(-1)^OPEm*x*PolyLog[3, -x] + 4*x*PolyLog[3, x] -
		4*(-1)^OPEm*x*PolyLog[3, x] + 2*x*Zeta[3] - 2*(-1)^OPEm*x*Zeta[3])
,
(* n17i1.r *)
RTLI[{0, -OPEi + OPEm, 0, 0, OPEi},
		{1, {1, M_}, {1, M_}, 1, {1, M_}}, r___] :>
((-1)^OPEi*Smu^2*x^(-1 + OPEm)*so[r]^OPEm*
		((-2 + (1 - x)^(-1) - 4*x - 2*(-1)^OPEm*x + (-1)^OPEm/(1 + x))*Zeta2 +
			(1 - (-1)^OPEm + (1 - x)^(-2) - 2/(1 - x) + x/2 +
					(5*(-1)^OPEm*x)/2 + (3*(-1)^OPEm)/(1 + x)^2 -
					(2*(-1)^OPEm)/(1 + x))*Log[x]^2 +
			(-2 + 2/(1 - x) - 2*x - 4*(-1)^OPEm*x + (2*(-1)^OPEm)/(1 + x))*
				Log[x]*Log[1 + x] + (1 - (-1)^OPEm - 2/(1 - x) - x + (-1)^OPEm*x +
(4*(-1)^OPEm)/(1 + x)^2 - (2*(-1)^OPEm)/(1 + x))*PolyLog[2, 1 - x] +
(-2 + 2/(1 - x) - 2*x - 4*(-1)^OPEm*x + (2*(-1)^OPEm)/(1 + x))*
				PolyLog[2, -x] + DeltaFunction[1 - x]*(2*Zeta2 + 2*Zeta[3])))/M^2
,
(* n17i2.r *)
RTLI[{2, -2 - OPEi + OPEm, 0, 0, OPEi},
		{1, {1, M_}, {1, M_}, 1, {1, M_}},
	r___] :> (Smu^2*x^(-1 + OPEm)*
		((-1 + (-1)^OPEm + x + (-1)^OPEm*x)*Zeta2 - Log[1 - x] +
			(1 - (1 - x)^(-1))*Log[x] + (x/2 - (3*(-1)^OPEm*x)/2)*Log[x]^2 +
			(2*(-1)^OPEm + 2*(-1)^OPEm*x)*Log[x]*Log[1 + x] +
			(-1 + (-1)^OPEm + x - (-1)^OPEm*x)*PolyLog[2, 1 - x] +
			(2*(-1)^OPEm + 2*(-1)^OPEm*x)*PolyLog[2, -x])*so[r]^OPEm)/
	((-1)^OPEi*M^2)
,
(* r18i01.r *)
RTLI[{0, -1, -1, 0, m_ /; MCH[m]}, {{1, M_}, 1, 1, {1, M_}, 0}, r___] :>
	Smu^2*x^(-1 + m)*so[r]^(-2 + m)*
	((8 - 8/(1 - x))/Epsilon^3 - (4*DeltaFunction[1 - x])/Epsilon^4 +
		(59*Pi^4*DeltaFunction[1 - x])/480 - 12*Zeta2*Log[1 - x] +
		(12*Zeta2*Log[1 - x])/(1 - x) + (32*Log[1 - x]^3)/3 -
		(32*Log[1 - x]^3)/(3*(1 - x)) + 2*(-1)^m*Zeta2*Log[x] -
		(2*(-1)^m*Zeta2*Log[x])/(1 + x) + (4*(-1)^m*Log[x]^3)/3 -
		(4*(-1)^m*Log[x]^3)/(3*(1 + x)) +
		(3*Zeta2*DeltaFunction[1 - x] + 16*Log[1 - x] -
				(16*Log[1 - x])/(1 - x) + 8*(-1)^m*Log[x] -
				(8*(-1)^m*Log[x])/(1 + x))/Epsilon^2 - 16*(-1)^m*Zeta2*Log[1 + x] +
		(16*(-1)^m*Zeta2*Log[1 + x])/(1 + x) + 8*(-1)^m*Log[x]^2*Log[1 + x] -
		(8*(-1)^m*Log[x]^2*Log[1 + x])/(1 + x) + (16*(-1)^m*Log[1 + x]^3)/3 -
		(16*(-1)^m*Log[1 + x]^3)/(3*(1 + x)) + 16*(-1)^m*Log[x]*PolyLog[2, -x] -
		(16*(-1)^m*Log[x]*PolyLog[2, -x])/(1 + x) - 16*(-1)^m*PolyLog[3, -x] +
		(16*(-1)^m*PolyLog[3, -x])/(1 + x) -
		32*(-1)^m*PolyLog[3, (1 + x)^(-1)] +
		(32*(-1)^m*PolyLog[3, (1 + x)^(-1)])/(1 + x) + (50*Zeta[3])/3 +
		16*(-1)^m*Zeta[3] - (50*Zeta[3])/(3*(1 - x)) -
		(16*(-1)^m*Zeta[3])/(1 + x) +
		(-6*Zeta2 + 8*(-1)^m*Zeta2 + (6*Zeta2)/(1 - x) -
				(8*(-1)^m*Zeta2)/(1 + x) + 16*Log[1 - x]^2 -
				(16*Log[1 - x]^2)/(1 - x) + 4*(-1)^m*Log[x]^2 -
				(4*(-1)^m*Log[x]^2)/(1 + x) + 16*(-1)^m*Log[x]*Log[1 + x] -
				(16*(-1)^m*Log[x]*Log[1 + x])/(1 + x) + 16*(-1)^m*PolyLog[2, -x] -
				(16*(-1)^m*PolyLog[2, -x])/(1 + x) -
				(25*DeltaFunction[1 - x]*Zeta[3])/3)/Epsilon)
,
(* r18i02.r *)
RTLI[{1, -1, -1, 0, m_ /; MCH[m]}, {{1, M_}, 1, 1, {1, M_}, 0}, r___] :>
	Smu^2*x^(-1 + m)*so[r]^(-1 + m)*
	((8 - 8/(1 - x) + 4*x)/Epsilon^3 - (4*DeltaFunction[1 - x])/Epsilon^4 +
		(59*Pi^4*DeltaFunction[1 - x])/480 - 12*Zeta2*Log[1 - x] +
		(12*Zeta2*Log[1 - x])/(1 - x) - 6*x*Zeta2*Log[1 - x] +
		(32*Log[1 - x]^3)/3 - (32*Log[1 - x]^3)/(3*(1 - x)) +
		(16*x*Log[1 - x]^3)/3 + 2*(-1)^m*Zeta2*Log[x] - (-1)^m*x*Zeta2*Log[x] -
		(2*(-1)^m*Zeta2*Log[x])/(1 + x) + (4*(-1)^m*Log[x]^3)/3 -
		(2*(-1)^m*x*Log[x]^3)/3 - (4*(-1)^m*Log[x]^3)/(3*(1 + x)) +
		(3*Zeta2*DeltaFunction[1 - x] + 16*Log[1 - x] -
				(16*Log[1 - x])/(1 - x) + 8*x*Log[1 - x] + 8*(-1)^m*Log[x] -
				4*(-1)^m*x*Log[x] - (8*(-1)^m*Log[x])/(1 + x))/Epsilon^2 -
		16*(-1)^m*Zeta2*Log[1 + x] + 8*(-1)^m*x*Zeta2*Log[1 + x] +
		(16*(-1)^m*Zeta2*Log[1 + x])/(1 + x) + 8*(-1)^m*Log[x]^2*Log[1 + x] -
		4*(-1)^m*x*Log[x]^2*Log[1 + x] -
		(8*(-1)^m*Log[x]^2*Log[1 + x])/(1 + x) + (16*(-1)^m*Log[1 + x]^3)/3 -
		(8*(-1)^m*x*Log[1 + x]^3)/3 - (16*(-1)^m*Log[1 + x]^3)/(3*(1 + x)) +
		16*(-1)^m*Log[x]*PolyLog[2, -x] - 8*(-1)^m*x*Log[x]*PolyLog[2, -x] -
		(16*(-1)^m*Log[x]*PolyLog[2, -x])/(1 + x) - 16*(-1)^m*PolyLog[3, -x] +
		8*(-1)^m*x*PolyLog[3, -x] + (16*(-1)^m*PolyLog[3, -x])/(1 + x) -
		32*(-1)^m*PolyLog[3, (1 + x)^(-1)] +
		16*(-1)^m*x*PolyLog[3, (1 + x)^(-1)] +
		(32*(-1)^m*PolyLog[3, (1 + x)^(-1)])/(1 + x) + (50*Zeta[3])/3 +
		16*(-1)^m*Zeta[3] - (50*Zeta[3])/(3*(1 - x)) + (25*x*Zeta[3])/3 -
		8*(-1)^m*x*Zeta[3] - (16*(-1)^m*Zeta[3])/(1 + x) +
		(-6*Zeta2 + 8*(-1)^m*Zeta2 + (6*Zeta2)/(1 - x) - 3*x*Zeta2 -
				4*(-1)^m*x*Zeta2 - (8*(-1)^m*Zeta2)/(1 + x) + 16*Log[1 - x]^2 -
				(16*Log[1 - x]^2)/(1 - x) + 8*x*Log[1 - x]^2 + 4*(-1)^m*Log[x]^2 -
				2*(-1)^m*x*Log[x]^2 - (4*(-1)^m*Log[x]^2)/(1 + x) +
				16*(-1)^m*Log[x]*Log[1 + x] - 8*(-1)^m*x*Log[x]*Log[1 + x] -
				(16*(-1)^m*Log[x]*Log[1 + x])/(1 + x) + 16*(-1)^m*PolyLog[2, -x] -
				8*(-1)^m*x*PolyLog[2, -x] - (16*(-1)^m*PolyLog[2, -x])/(1 + x) -
				(25*DeltaFunction[1 - x]*Zeta[3])/3)/Epsilon)
,
(* r18i03.r *)
RTLI[{m_ /; MCH[m], 0, -1, -1, 0}, {{1, M_}, {1, M_}, 1, 1, 0}, r___] :>
	Smu^2*x^(-1 + m)*so[r]^(-2 + m)*
	((4 - 4/(1 - x))/Epsilon^3 - (4*DeltaFunction[1 - x])/Epsilon^4 -
		(7*Pi^4*DeltaFunction[1 - x])/1440 + Zeta2*Log[1 - x] -
		(Zeta2*Log[1 - x])/(1 - x) + (2*Log[1 - x]^3)/3 -
		(2*Log[1 - x]^3)/(3*(1 - x)) +
		(-(Zeta2*DeltaFunction[1 - x]) + 4*Log[1 - x] - (4*Log[1 - x])/(1 - x))/
			Epsilon^2 + Zeta[3]/3 - Zeta[3]/(3*(1 - x)) +
		(Zeta2 - Zeta2/(1 - x) + 2*Log[1 - x]^2 - (2*Log[1 - x]^2)/(1 - x) -
				(DeltaFunction[1 - x]*Zeta[3])/3)/Epsilon)
,
(* r18i04.r *)
RTLI[{m_ /; MCH[m], 0, -1, 0, 0}, {{1, M_}, {1, M_}, 1, 1, 0}, r___] :>
	Smu^2*x^(-1 + m)*so[r]^(-1 + m)*
	(4 - 4/(1 - x) + Zeta2 - Zeta2/(1 - x) + 4*DeltaFunction[1 - x] -
		(4*DeltaFunction[1 - x])/Epsilon^3 + Zeta2*DeltaFunction[1 - x] +
		(4 - 4/(1 - x) + 4*DeltaFunction[1 - x])/Epsilon^2 - 4*Log[1 - x] +
		(4*Log[1 - x])/(1 - x) + 2*Log[1 - x]^2 - (2*Log[1 - x]^2)/(1 - x) +
		(-4 + 4/(1 - x) - 4*DeltaFunction[1 - x] -
				Zeta2*DeltaFunction[1 - x] + 4*Log[1 - x] - (4*Log[1 - x])/(1 - x))/
			Epsilon - (DeltaFunction[1 - x]*Zeta[3])/3)
,
(* r18i05.r *)
RTLI[{m_ /; MCH[m], 0, 0, -1, -1}, {{1, M_}, {1, M_}, 1, 1, 0}, r___] :>
	Smu^2*x^(-1 + m)*so[r]^(-2 + m)*
	((-4 + 4/(1 - x))/Epsilon^3 + (2*DeltaFunction[1 - x])/Epsilon^4 -
		(67*Pi^4*DeltaFunction[1 - x])/960 + 14*Zeta2*Log[1 - x] -
		(14*Zeta2*Log[1 - x])/(1 - x) - (16*Log[1 - x]^3)/3 +
		(16*Log[1 - x]^3)/(3*(1 - x)) + Zeta2*Log[x] - (Zeta2*Log[x])/(1 - x) +
		2*Log[1 - x]^2*Log[x] - (2*Log[1 - x]^2*Log[x])/(1 - x) +
		((-7*Zeta2*DeltaFunction[1 - x])/2 - 8*Log[1 - x] +
				(8*Log[1 - x])/(1 - x) + 4*Log[x] - (4*Log[x])/(1 - x))/Epsilon^2 -
		4*Log[1 - x]*PolyLog[2, 1 - x] +
		(4*Log[1 - x]*PolyLog[2, 1 - x])/(1 - x) - 4*PolyLog[3, 1 - x] +
		(4*PolyLog[3, 1 - x])/(1 - x) - Zeta[3]/3 + Zeta[3]/(3*(1 - x)) +
		(7*Zeta2 - (7*Zeta2)/(1 - x) - 8*Log[1 - x]^2 +
				(8*Log[1 - x]^2)/(1 - x) + 4*Log[1 - x]*Log[x] -
				(4*Log[1 - x]*Log[x])/(1 - x) - 4*PolyLog[2, 1 - x] +
				(4*PolyLog[2, 1 - x])/(1 - x) + (DeltaFunction[1 - x]*Zeta[3])/6)/
			Epsilon)
,
(* r18i06.r *)
RTLI[{m_ /; MCH[m], 0, 0, 0, -1}, {{1, M_}, {1, M_}, 1, 1, 0}, r___] :>
	Smu^2*x^(-1 + m)*(-15*x*Zeta2*Log[1 - x] + (14*x*Log[1 - x]^3)/3 -
		x*Zeta2*Log[x] - 2*x*Log[1 - x]^2*Log[x] +
		(4*x*Log[1 - x] - 4*x*Log[x])/Epsilon^2 +
		4*x*Log[1 - x]*PolyLog[2, 1 - x] +
		(-8*x*Zeta2 + 6*x*Log[1 - x]^2 - 4*x*Log[1 - x]*Log[x] +
				4*x*PolyLog[2, 1 - x])/Epsilon + 4*x*PolyLog[3, 1 - x])*so[r]^(-1 + m)
,
(* r18i07.r *)
RTLI[{m_ /; MCH[m], 0, -1, 0, -1}, {{1, M_}, {1, M_}, 1, 1, 0}, r___] :>
	Smu^2*x^(-1 + m)*so[r]^(-2 + m)*
	((-2*DeltaFunction[1 - x])/Epsilon^4 -
		(43*Pi^4*DeltaFunction[1 - x])/576 + 15*Zeta2*Log[1 - x] -
		(15*Zeta2*Log[1 - x])/(1 - x) - (14*Log[1 - x]^3)/3 +
		(14*Log[1 - x]^3)/(3*(1 - x)) + Zeta2*Log[x] - (Zeta2*Log[x])/(1 - x) +
		2*Log[1 - x]^2*Log[x] - (2*Log[1 - x]^2*Log[x])/(1 - x) +
		((-9*Zeta2*DeltaFunction[1 - x])/2 - 4*Log[1 - x] +
				(4*Log[1 - x])/(1 - x) + 4*Log[x] - (4*Log[x])/(1 - x))/Epsilon^2 -
		4*Log[1 - x]*PolyLog[2, 1 - x] +
		(4*Log[1 - x]*PolyLog[2, 1 - x])/(1 - x) - 4*PolyLog[3, 1 - x] +
		(4*PolyLog[3, 1 - x])/(1 - x) +
		(8*Zeta2 - (8*Zeta2)/(1 - x) - 6*Log[1 - x]^2 +
				(6*Log[1 - x]^2)/(1 - x) + 4*Log[1 - x]*Log[x] -
				(4*Log[1 - x]*Log[x])/(1 - x) - 4*PolyLog[2, 1 - x] +
				(4*PolyLog[2, 1 - x])/(1 - x) - (DeltaFunction[1 - x]*Zeta[3])/6)/
			Epsilon)
,
(* r18i08.r *)
RTLI[{0, 0, -1, -1, 0}, {{1, M_}, {1, M_}, 1, 1, 0}, r___] :>
	(Smu^2*x^(-1 + OPEm)*((-4*DeltaFunction[1 - x])/Epsilon^4 -
			(7*Pi^4*DeltaFunction[1 - x])/1440 -
			(Zeta2*DeltaFunction[1 - x])/Epsilon^2 -
			(DeltaFunction[1 - x]*Zeta[3])/(3*Epsilon)))/so[r]^2
,
(* r18i09.r *)
RTLI[{0, 0, 0, -1, 0}, {{1, M_}, {1, M_}, 1, 1, 0}, r___] :>
	(Smu^2*x^(-1 + OPEm)*(4*DeltaFunction[1 - x] -
			(4*DeltaFunction[1 - x])/Epsilon^3 +
			(4*DeltaFunction[1 - x])/Epsilon^2 + Zeta2*DeltaFunction[1 - x] +
			(-4*DeltaFunction[1 - x] - Zeta2*DeltaFunction[1 - x])/Epsilon -
			(DeltaFunction[1 - x]*Zeta[3])/3))/so[r]
,
(* r18i10.r *)
RTLI[{0, 0, 0, 0, 0}, {{1, M_}, {1, M_}, 1, 1, 0}, r___] :>
	Smu^2*x^(-1 + OPEm)*(-12*DeltaFunction[1 - x] -
		(4*DeltaFunction[1 - x])/Epsilon^2 + (8*DeltaFunction[1 - x])/Epsilon -
		Zeta2*DeltaFunction[1 - x])
,
(* r18i11.r *)
RTLI[{0, -1, 0, 0, m_}, {{1, M_}, 1, 1, {1, M_}, 0}, r___] :>
	Smu^2*x^(-1 + m)*so[r]^(-1 + m)*
	((-4*x)/Epsilon^3 + 6*x*Zeta2*Log[1 - x] - (16*x*Log[1 - x]^3)/3 +
		(-1)^m*x*Zeta2*Log[x] + (2*(-1)^m*x*Log[x]^3)/3 +
		(-8*x*Log[1 - x] + 4*(-1)^m*x*Log[x])/Epsilon^2 -
		8*(-1)^m*x*Zeta2*Log[1 + x] + 4*(-1)^m*x*Log[x]^2*Log[1 + x] +
		(8*(-1)^m*x*Log[1 + x]^3)/3 + 8*(-1)^m*x*Log[x]*PolyLog[2, -x] +
		(3*x*Zeta2 + 4*(-1)^m*x*Zeta2 - 8*x*Log[1 - x]^2 +
				2*(-1)^m*x*Log[x]^2 + 8*(-1)^m*x*Log[x]*Log[1 + x] +
				8*(-1)^m*x*PolyLog[2, -x])/Epsilon - 8*(-1)^m*x*PolyLog[3, -x] -
		16*(-1)^m*x*PolyLog[3, (1 + x)^(-1)] - (25*x*Zeta[3])/3 +
		8*(-1)^m*x*Zeta[3])
,
(* r20i1.r *)
RTLI[{OPEi - OPEj, 0, 0, 0, -2 - OPEi + OPEm}, {0, 1, 1, {1, M_}, {1, M_}},
	r___] :> Smu^2*x^(-1 + OPEm)*so[r]^(-2 - OPEj + OPEm)*
	((9*Pi^4*DeltaFunction[1 - x])/40 + Zeta2*Log[1 - x] +
		(3*Zeta2*Log[1 - x])/(2*(1 - x)) + (11*Log[1 - x]^3)/6 -
		(7*Log[1 - x]^3)/(4*(1 - x)) - 2*Zeta2*Log[x] +
		(6*Zeta2*Log[x])/(1 - x) - (5*Log[1 - x]^2*Log[x])/2 +
		(5*Log[1 - x]^2*Log[x])/(2*(1 - x)) - (Log[1 - x]*Log[x]^2)/2 -
		(3*Log[1 - x]*Log[x]^2)/(2*(1 - x)) - Log[x]^3/2 +
		Log[x]^3/(2*(1 - x)) + (4*Zeta2*DeltaFunction[1 - x] + 4*Log[1 - x] -
				(2*Log[1 - x])/(1 - x) - 4*Log[x] + (4*Log[x])/(1 - x))/Epsilon^2 -
		7*Log[1 - x]*PolyLog[2, 1 - x] +
		(6*Log[1 - x]*PolyLog[2, 1 - x])/(1 - x) + Log[x]*PolyLog[2, 1 - x] -
		(5*Log[x]*PolyLog[2, 1 - x])/(1 - x) + 5*PolyLog[3, 1 - x] -
		(7*PolyLog[3, 1 - x])/(1 - x) + 3*PolyLog[3, x] -
		(7*PolyLog[3, x])/(1 - x) + (2*Zeta[3])/(1 - x) +
		(2*Zeta2 + (2*Zeta2)/(1 - x) + 3*Log[1 - x]^2 -
				(5*Log[1 - x]^2)/(2*(1 - x)) - 2*Log[1 - x]*Log[x] +
				(2*Log[1 - x]*Log[x])/(1 - x) - 3*Log[x]^2 + (3*Log[x]^2)/(1 - x) -
				4*PolyLog[2, 1 - x] - 8*DeltaFunction[1 - x]*Zeta[3])/Epsilon)

,
(* r20i2.r *)
RTLI[{0, 0, 0, 1 + OPEi - OPEj, -2 - OPEi + OPEm},
	{0, 1, 1, {1, M_}, {1, M_}}, r___] :>
	Smu^2*x^(-1 + OPEm)*so[r]^(-1-OPEj+OPEm)*(2 - 2/(1 - x) - (3*Zeta2)/2 +
		(3*Zeta2)/(2*(1 - x)) - (19*DeltaFunction[1 - x])/2 +
		(17*Pi^4*DeltaFunction[1 - x])/90 + (3*Zeta2*DeltaFunction[1 - x])/2 -
		3*Log[1 - x] + (3*Log[1 - x])/(1 - x) - (11*Zeta2*Log[1 - x])/2 +
		(11*Zeta2*Log[1 - x])/(2*(1 - x)) + (7*Log[1 - x]^2)/4 -
		(7*Log[1 - x]^2)/(4*(1 - x)) + (35*Log[1 - x]^3)/12 -
		(35*Log[1 - x]^3)/(12*(1 - x)) - (3*Zeta2*Log[x])/2 +
		(3*Zeta2*Log[x])/(2*(1 - x)) + Log[1 - x]*Log[x] -
		(Log[1 - x]*Log[x])/(1 - x) - 4*Log[1 - x]^2*Log[x] +
		(4*Log[1 - x]^2*Log[x])/(1 - x) + (Log[1 - x]*Log[x]^2)/2 -
		(Log[1 - x]*Log[x]^2)/(2*(1 - x)) + Log[x]^3/12 -
		Log[x]^3/(12*(1 - x)) + (2 - 2/(1 - x) - 2*DeltaFunction[1 - x] +
				2*Zeta2*DeltaFunction[1 - x] + 2*Log[1 - x] -
				(2*Log[1 - x])/(1 - x) - 2*Log[x] + (2*Log[x])/(1 - x))/Epsilon^2 +
		PolyLog[2, 1 - x] - PolyLog[2, 1 - x]/(1 - x) -
		5*Log[1 - x]*PolyLog[2, 1 - x] +
		(5*Log[1 - x]*PolyLog[2, 1 - x])/(1 - x) + Log[x]*PolyLog[2, 1 - x] -
		(Log[x]*PolyLog[2, 1 - x])/(1 - x) + 6*PolyLog[3, 1 - x] -
		(6*PolyLog[3, 1 - x])/(1 - x) + 3*PolyLog[3, x] -
		(3*PolyLog[3, x])/(1 - x) + 2*DeltaFunction[1 - x]*Zeta[3] +
		(-2 + 2/(1 - x) - 2*Zeta2 + (2*Zeta2)/(1 - x) +
				5*DeltaFunction[1 - x] + 3*Log[1 - x] - (3*Log[1 - x])/(1 - x) +
				(7*Log[1 - x]^2)/2 - (7*Log[1 - x]^2)/(2*(1 - x)) -
				4*Log[1 - x]*Log[x] + (4*Log[1 - x]*Log[x])/(1 - x) - Log[x]^2/2 +
				Log[x]^2/(2*(1 - x)) - 2*PolyLog[2, 1 - x] +
				(2*PolyLog[2, 1 - x])/(1 - x) - 7*DeltaFunction[1 - x]*Zeta[3])/
			Epsilon)
,
(* r20i3.r *)
RTLI[{OPEi - OPEj, 1, 0, 0, -2 - OPEi + OPEm}, {0, 1, 1, {1, M_}, {1, M_}},
	r___] :> Smu^2*x^(-1 + OPEm)*so[r]^(-OPEj+OPEm-1)*
	(-2 + 2/(1 - x) + (3*Zeta2)/2 + (5*Zeta2)/(2*(1 - x)) +
		(19*DeltaFunction[1 - x])/2 + (89*Pi^4*DeltaFunction[1 - x])/360 -
		(3*Zeta2*DeltaFunction[1 - x])/2 + 3*Log[1 - x] - Log[1 - x]/(1 - x) +
		(5*Zeta2*Log[1 - x])/2 + Log[1 - x]^2/4 + Log[1 - x]^2/(4*(1 - x)) +
		Log[1 - x]^3/12 + (7*Zeta2*Log[x])/2 + (Zeta2*Log[x])/(2*(1 - x)) -
		Log[1 - x]*Log[x] + (Log[1 - x]*Log[x])/(1 - x) -
		4*Log[1 - x]^2*Log[x] + (4*Log[1 - x]^2*Log[x])/(1 - x) - 2*Log[x]^2 +
		(2*Log[x]^2)/(1 - x) - 2*Log[1 - x]*Log[x]^2 + Log[x]^3/12 -
		Log[x]^3/(12*(1 - x)) + (-2 + 2/(1 - x) + 2*DeltaFunction[1 - x] +
				2*Zeta2*DeltaFunction[1 - x] + 2*Log[1 - x] - 2*Log[x] +
				(2*Log[x])/(1 - x))/Epsilon^2 - PolyLog[2, 1 - x] -
		(3*PolyLog[2, 1 - x])/(1 - x) - 9*Log[1 - x]*PolyLog[2, 1 - x] +
		(8*Log[1 - x]*PolyLog[2, 1 - x])/(1 - x) - 4*Log[x]*PolyLog[2, 1 - x] +
		6*PolyLog[3, 1 - x] - (8*PolyLog[3, 1 - x])/(1 - x) - 2*PolyLog[3, x] -
		(2*PolyLog[3, x])/(1 - x) + (2*Zeta[3])/(1 - x) -
		2*DeltaFunction[1 - x]*Zeta[3] +
		(2 - 2/(1 - x) + 4*Zeta2 - 5*DeltaFunction[1 - x] +
				4*Zeta2*DeltaFunction[1 - x] + Log[1 - x] + Log[1 - x]/(1 - x) +
				Log[1 - x]^2/2 - 4*Log[x] + (4*Log[x])/(1 - x) -
				4*Log[1 - x]*Log[x] + (4*Log[1 - x]*Log[x])/(1 - x) - Log[x]^2/2 +
				Log[x]^2/(2*(1 - x)) - 8*PolyLog[2, 1 - x] +
				(4*PolyLog[2, 1 - x])/(1 - x) - 9*DeltaFunction[1 - x]*Zeta[3])/
			Epsilon)
};

(*
tlist =
tlist/.(a_ :> b_) :> (ruledel[a/. {M:> (M_/;M=!=0), OPEi:> OPEi_}, b] /.
											ruledel:>RuleDelayed);
*)

res =
(
(rere =
	Collect[Expand[dum[Epsilon] Series2[
FeynCalcInternal[ChangeDimension[exp /. TLI -> RTLI, 4] /.tlilist
								] /. dim -> (4 + Epsilon),
															Epsilon, 0]],Epsilon, Factor
				]) // lsimp
) /. RTLI -> TLI;

(*
If[Head[res] =!= Plus, pref = 1,
	pref = SelectNotFree[dummy First[res]/. (-1)^OPEm:>bla ,
									{M,p,Smu,CA,CF,Tf,Gstrong, OPEm}];
	res = pref . Map[(#/pref)&, res]
	];
*)
	res];

FCPrint[1,"RTL.m loaded."];
End[]
