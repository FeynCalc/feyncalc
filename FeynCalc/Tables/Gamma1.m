(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Gamma1*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

Gamma1::usage=
"Gamma1[al, ga, be, de] is a special product of Gamma functions expanded up to
order Epsilon^2.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Gamma1`Private`"]

Options[Gamma1] = {EpsilonOrder -> 2};

Gamma1[al_Symbol, ga_Symbol, be_Symbol, de_Symbol] :=
( (Gamma[-2 + be + de - Epsilon/2]*Gamma[-2 + al - Epsilon/2 + ga])/
	(Gamma[al]*Gamma[be]*Gamma[de]*Gamma[ga])
);

Gamma1[al_Integer?Positive, ga_Integer?Positive,
			be_Integer?Positive, de_Integer?Positive,
			opt___Rule] := Gamma1[al,ga,be,de,opt] =
Which[(al + ga > 2) && (be + de >2),
(*gr = *)( (Gamma[-2 + be + de]*Gamma[-2 + al + ga])/
	(Gamma[al]*Gamma[be]*Gamma[de]*Gamma[ga]) -
	(Epsilon*Gamma[-2 + be + de]*Gamma[-2 + al + ga]*
		(PolyGamma[0, -2 + be + de] + PolyGamma[0, -2 + al + ga]))/
	(2*Gamma[al]*Gamma[be]*Gamma[de]*Gamma[ga]) +
	(Epsilon^2*Gamma[-2 + be + de]*Gamma[-2 + al + ga]*
		(PolyGamma[0, -2 + be + de]^2 +
			2*PolyGamma[0, -2 + be + de]*PolyGamma[0, -2 + al + ga] +
			PolyGamma[0, -2 + al + ga]^2 + PolyGamma[1, -2 + be + de] +
			PolyGamma[1, -2 + al + ga]))/
	(8*Gamma[al]*Gamma[be]*Gamma[de]*Gamma[ga])
			),

			(al === 1) && (ga === 1) && (be+de>2),
(*ag1 =*) ( (-2*Gamma[-2 + be + de])/(Epsilon*Gamma[be]*Gamma[de]) -
	(Gamma[-2 + be + de]*(EulerGamma - PolyGamma[0, -2 + be + de]))/
	(Gamma[be]*Gamma[de]) - (Epsilon*Gamma[-2 + be + de]*
		(6*EulerGamma^2 + Pi^2 - 12*EulerGamma*PolyGamma[0, -2 + be + de] +
			6*PolyGamma[0, -2 + be + de]^2 + 6*PolyGamma[1, -2 + be + de]))/
	(24*Gamma[be]*Gamma[de]) - (Epsilon^2*Gamma[-2 + be + de]*
		(2*EulerGamma^3 + EulerGamma*Pi^2 -
			6*EulerGamma^2*PolyGamma[0, -2 + be + de] -
			Pi^2*PolyGamma[0, -2 + be + de] +
			6*EulerGamma*PolyGamma[0, -2 + be + de]^2 -
			2*PolyGamma[0, -2 + be + de]^3 +
			6*EulerGamma*PolyGamma[1, -2 + be + de] -
			6*PolyGamma[0, -2 + be + de]*PolyGamma[1, -2 + be + de] -
			2*PolyGamma[2, -2 + be + de] + 4*Zeta[3]))/(48*Gamma[be]*Gamma[de])
			),
			(be === 1) && (de === 1) && (al+ga>2),
(*bd1 = *)( (-2*Gamma[-2 + al + ga])/(Epsilon*Gamma[al]*Gamma[ga]) -
	(Gamma[-2 + al + ga]*(EulerGamma - PolyGamma[0, -2 + al + ga]))/
	(Gamma[al]*Gamma[ga]) - (Epsilon*Gamma[-2 + al + ga]*
		(6*EulerGamma^2 + Pi^2 - 12*EulerGamma*PolyGamma[0, -2 + al + ga] +
			6*PolyGamma[0, -2 + al + ga]^2 + 6*PolyGamma[1, -2 + al + ga]))/
	(24*Gamma[al]*Gamma[ga]) - (Epsilon^2*Gamma[-2 + al + ga]*
		(2*EulerGamma^3 + EulerGamma*Pi^2 -
			6*EulerGamma^2*PolyGamma[0, -2 + al + ga] -
			Pi^2*PolyGamma[0, -2 + al + ga] +
			6*EulerGamma*PolyGamma[0, -2 + al + ga]^2 -
			2*PolyGamma[0, -2 + al + ga]^3 +
			6*EulerGamma*PolyGamma[1, -2 + al + ga] -
			6*PolyGamma[0, -2 + al + ga]*PolyGamma[1, -2 + al + ga] -
			2*PolyGamma[2, -2 + al + ga] + 4*Zeta[3]))/(48*Gamma[al]*Gamma[ga])
			),
		(al===1) && (be===1) && (ga===1) && (de===1),
(*ag1bd1 = *)( 4/Epsilon^2 + (4*EulerGamma)/Epsilon +
	(12*EulerGamma^2 + Pi^2)/6 +
	(Epsilon*(4*EulerGamma^3 + EulerGamma*Pi^2 + 2*Zeta[3]))/6 +
	(Epsilon^2*(240*EulerGamma^4 + 120*EulerGamma^2*Pi^2 + 7*Pi^4 +
			480*EulerGamma*Zeta[3]))/1440
			)] /. EulerGamma -> 0;

FCPrint[1,"Gamma1.m loaded."];
End[];
