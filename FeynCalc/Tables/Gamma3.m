(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Gamma3*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 28 August '98 at 20:31 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

Gamma3::usage=
"Gamma3[al, be, ga, ep] is a special product of Gamma functions expanded up to
order Epsilon^n when positive integer arguments are given (the order n is
determined by the option EpsilonOrder).";


Begin["`Package`"]
End[]

(* ------------------------------------------------------------------------ *)

Begin["`Gamma3`Private`"]

gamma3::usage="";

Options[Gamma3] = {
	EpsilonOrder -> 3
};

Gamma3[al_ /;Head[al]=!=Integer, be_, ga_, ep_,___Rule] :=
Gamma[al+be+ga+ep-4-Epsilon]/Gamma[al+be+ep-2-Epsilon/2]/Gamma[ga] *
Gamma[be+ep-2-Epsilon/2]/Gamma[be]/Gamma[ep];

Gamma3[al_Integer, be_Integer /; be > 0,
			ga_Integer /; ga > 0, ep_Integer /; ep > 0,
			opt___Rule
			] :=
Which[(EpsilonOrder /. {opt} /. Options[Gamma3]) === 3,
			gamma3[al, be,ga,ep],
			(EpsilonOrder /. {opt} /. Options[Gamma3]) ===2 ,
			gamma3[al,be,ga,ep] /. Epsilon^3 -> 0,
			(EpsilonOrder /. {opt} /. Options[Gamma3]) < 2 ,
			gamma3[al,be,ga,ep] /. {Epsilon^3 :> 0,Epsilon^2:>0},
			(EpsilonOrder /. {opt} /. Options[Gamma3]) > 3 ,
			simp[
			SimplifyPolyGamma[Expand[
			Normal[Gamma[al+be+ga+ep-4-Epsilon]/
							Gamma[al+be+ep-2-Epsilon/2]/Gamma[ga] *
							Gamma[be+ep-2-Epsilon/2]/Gamma[be]/Gamma[ep] +
							O[Epsilon]^(1+(EpsilonOrder /. {opt} /. Options[Gamma3]))
						]          ]      ]
					]
		] /. gamrules;

simp[z_] := (*toholdform[*)Collect[Expand[z/.grules], Epsilon](*]*);

toholdform[a_Plus] := Map[toholdform, a];
toholdform[b_Times]:= SelectNotFree[b,Epsilon] Apply[HoldForm,
											{SelectFree[b,Epsilon]}];
toholdform[c_ /; Head[c] =!= Plus && Head[c]=!=Times] := c;

grules = {Pi^2 -> 6Zeta2};

gamrules =
{gamma3[-1, 1, 1, 1] :>
	-1/2 + 1/(2*Epsilon) + Epsilon*(1/2 + Zeta2/4) +
		Epsilon^3*(1/2 + Pi^4/320 + Zeta2/4 - Zeta[3]/6) +
		Epsilon^2*(-1/2 - Zeta2/4 + Zeta[3]/6),
	gamma3[-1, 1, 1, 2] :>
	-1/2 + Epsilon/2 + Epsilon^2*(-1/2 - Zeta2/4) +
		Epsilon^3*(1/2 + Zeta2/4 - Zeta[3]/6),
	gamma3[-1, 1, 2, 2] :>
		1/2 + (Epsilon^2*Zeta2)/4 + (Epsilon^3*Zeta[3])/6,
	gamma3[-1, 2, 1, 1] :>
	-1/2 + Epsilon/2 + Epsilon^2*(-1/2 - Zeta2/4) +
		Epsilon^3*(1/2 + Zeta2/4 - Zeta[3]/6),
	gamma3[-1, 2, 1, 2] :>
	1/2 - Epsilon^(-1) - (Epsilon*Zeta2)/2 +
		Epsilon^2*(Zeta2/4 - Zeta[3]/3) + Epsilon^3*(-Pi^4/160 + Zeta[3]/6),
	gamma3[-1, 2, 2, 1] :>
	1/2 + (Epsilon^2*Zeta2)/4 + (Epsilon^3*Zeta[3])/6,
	gamma3[0, 1, 1, 1] :>
	-1 + Epsilon^(-1) + Epsilon*(1 + Zeta2/2) +
		Epsilon^2*(-1 - Zeta2/2 + Zeta[3]/3),
	gamma3[0, 1, 1, 2] :>
	-Epsilon^(-1) - (Epsilon*Zeta2)/2 - (Epsilon^2*Zeta[3])/3,
	gamma3[0, 2, 1, 1] :>
	-Epsilon^(-1) - (Epsilon*Zeta2)/2 - (Epsilon^2*Zeta[3])/3,
	gamma3[0, 2, 1, 2] :> 1 + (Epsilon^2*Zeta2)/2 + (Epsilon^3*Zeta[3])/3,
	gamma3[0, 2, 2, 1] :>  1 + (Epsilon^2*Zeta2)/2 + (Epsilon^3*Zeta[3])/3,
	gamma3[1, 1, 1, 1] :>
	2/Epsilon^2 + (Epsilon^2*Pi^4)/80 + Zeta2 + (2*Epsilon*Zeta[3])/3 +
		Epsilon^3*((Zeta2*Zeta[3])/3 + (2*Zeta[5])/5),
	gamma3[1, 1, 1, 2] :>
	1 + Epsilon/2 + Epsilon^2*(1/4 + Zeta2/2) +
		Epsilon^3*(1/8 + Zeta2/4 + Zeta[3]/3),
	gamma3[1, 1, 2, 1] :>
	-2/Epsilon - (Epsilon^3*Pi^4)/80 - Epsilon*Zeta2 - (2*Epsilon^2*Zeta[3])/3\
, gamma3[1, 1, 3, 1] :>
	1 - Epsilon^(-1) - (Epsilon*Zeta2)/2 + Epsilon^2*(Zeta2/2 - Zeta[3]/3) +
		Epsilon^3*(-Pi^4/160 + Zeta[3]/3),
	gamma3[1, 2, 1, 1] :>
	1 + Epsilon/2 + Epsilon^2*(1/4 + Zeta2/2) +
		Epsilon^3*(1/8 + Zeta2/4 + Zeta[3]/3),
	gamma3[1, 2, 1, 2] :>
	1/2 - (3*Epsilon)/8 + Epsilon^2*(-3/32 + Zeta2/4) +
		Epsilon^3*(-3/128 - (3*Zeta2)/16 + Zeta[3]/6),
	gamma3[1, 2, 2, 1] :>
	1 - Epsilon/2 + Epsilon^2*(-1/4 + Zeta2/2) +
		Epsilon^3*(-1/8 - Zeta2/4 + Zeta[3]/3),
	gamma3[1, 2, 3, 1] :>
	1 - Epsilon + (Epsilon^2*Zeta2)/2 +
(Epsilon^3*(-Zeta2 + (2*Zeta[3])/3))/2,
	gamma3[2, 1, 1, 1] :>
	-1 - 2/Epsilon + Epsilon*(-1/2 - Zeta2) +
		Epsilon^2*(-1/4 - Zeta2/2 - (2*Zeta[3])/3) +
		Epsilon^3*(-1/8 - Pi^4/80 - Zeta2/4 - Zeta[3]/3),
	gamma3[2, 1, 1, 2] :>
	1/2 - Epsilon/8 + Epsilon^2*(-5/32 + Zeta2/4) +
		Epsilon^3*(-13/128 - Zeta2/16 + Zeta[3]/6),
	gamma3[2, 1, 3, 1] :>
	2 - 2/Epsilon - (Epsilon^3*Pi^4)/80 - Epsilon*Zeta2 + Epsilon^2*
	(Zeta2 - (2*Zeta[3])/3) + (2*Epsilon^3*Zeta[3])/3,
	gamma3[2, 2, 1, 1] :>
	1/2 - Epsilon/8 + Epsilon^2*(-5/32 + Zeta2/4) +
		Epsilon^3*(-13/128 - Zeta2/16 + Zeta[3]/6),
	gamma3[2, 2, 1, 2] :>
	1/3 - (13*Epsilon)/36 + Epsilon^2*(1/432 + Zeta2/6) +
		Epsilon^3*(83/5184 - (13*Zeta2)/72 + Zeta[3]/9),
	gamma3[3, 1, 1, 1] :>
	1/4 - Epsilon^(-1) + Epsilon*(5/16 - Zeta2/2) +
		Epsilon^2*(13/64 + Zeta2/8 - Zeta[3]/3) +
		Epsilon^3*(29/256 - Pi^4/160 + (5*Zeta2)/32 + Zeta[3]/12),
	gamma3[3, 1, 1, 2] :>
	1/3 - (7*Epsilon)/36 + Epsilon^2*(-41/432 + Zeta2/6) +
		Epsilon^3*(-163/5184 - (7*Zeta2)/72 + Zeta[3]/9),
	gamma3[3, 2, 1, 1] :>
	1/3 - (7*Epsilon)/36 + Epsilon^2*(-41/432 + Zeta2/6) +
		Epsilon^3*(-163/5184 - (7*Zeta2)/72 + Zeta[3]/9),
	gamma3[3, 2, 1, 2] :>
	1/4 - (31*Epsilon)/96 + Epsilon^2*(119/2304 + Zeta2/8) +
		Epsilon^3*(989/55296 - (31*Zeta2)/192 + Zeta[3]/12),
	gamma3[0, 1, 2, 1] :>
	-Epsilon^(-1) - (Epsilon^3*Pi^4)/160 - (Epsilon*Zeta2)/2 -
	(Epsilon^2*Zeta[3])/3,
	gamma3[2, 1, 2, 1] :>
	1 - 2/Epsilon + Epsilon^3/8 - (Epsilon^3*Pi^4)/80 -
	2*Epsilon*(-1/4 + Zeta2/2) + (Epsilon^3*Zeta2)/4 -
	2*Epsilon^2*(-1/8 - Zeta2/4 + Zeta[3]/3) + (Epsilon^3*Zeta[3])/3
};

FCPrint[1,"Gamma3.m loaded."];
End[]
