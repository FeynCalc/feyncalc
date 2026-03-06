(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Lagrangian *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

Lagrangian::usage=
"Lagrangian[\"oqu\"] gives the unpolarized OPE quark operator.

Lagrangian[\"oqp\"] gives the polarized quark OPE operator.

Lagrangian[\"ogu\"] gives the unpolarized gluon OPE operator.

Lagrangian[\"ogp\"] gives the polarized gluon OPE operator.

 Lagrangian[\"ogd\"] gives the sigma-term part of the QCD Lagrangian.

 Lagrangian[\"QCD\"] gives the gluon self interaction part of the QCD
Lagrangian.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Lagrangian`Private`"]

Lagrangian[x_] /;Depth[x]===1:= Block[{na,lali,a,b,c,d,al,be,ga,mu,nu,la,rho},
	a = FCGV["a"];
	b = FCGV["b"];

If[$Notebooks,
	al = FCGV["\[Alpha]"];
	be = FCGV["\[Beta]"];
	ga = FCGV["\[Gamma]"];
	mu = FCGV["\[Mu]"];
	nu = FCGV["\[Nu]"];
	la = FCGV["\[Lambda]"];
	rho = FCGV["\[Rho]"];
	,
	al = FCGV["Al"];
	be = FCGV["Be"];
	mu = FCGV["Mu"];
	nu = FCGV["Nu"];
	la = FCGV["La"];
	rho= FCGV["Rho"];
	ga = FCGV["Ga"]
	];

na = ToString[x];
lali = {
"oqu" :> I^(OPEm)   DOT[QuantumField[AntiQuarkField],
											DiracGamma[Momentum[OPEDelta]],
											(CovariantD[OPEDelta]^(OPEm-1)),
												QuantumField[QuarkField]]
,
"oqp" :> I^OPEm   DOT[QuantumField[AntiQuarkField],
												DiracGamma[5],
												DiracGamma[Momentum[OPEDelta]],
											(CovariantD[OPEDelta]^(OPEm-1)),
											QuantumField[QuarkField]
									]
,
"ogu" :> I^(OPEm-1)/2 DOT[FieldStrength[al, OPEDelta,a] ,
												(CovariantD[OPEDelta,a,b]^(OPEm-2)),
												FieldStrength[al, OPEDelta,b]
											]
,
"ogp" :> I^OPEm/2 DOT[LC[al, be, ga][OPEDelta] ,
				FieldStrength[be, ga, a] ,
							(CovariantD[OPEDelta, a, b]^(OPEm-2)),
				FieldStrength[al, OPEDelta, b]
											]
,
"ogd" :> DOT[LC[mu,nu,la,rho] ,
				FieldStrength[mu,nu, a] ,
				FieldStrength[la,rho,a]
				]
,
"QCD" :> -1/4 DOT[FieldStrength[al, be, a] ,
							FieldStrength[al, be, a]]
			};
na /. lali];


FCPrint[1,"Lagrangian.m loaded."];
End[]
