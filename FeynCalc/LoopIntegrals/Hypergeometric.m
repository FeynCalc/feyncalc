(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: HypergeometricAC *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

HypergeometricAC::usage =
"HypergeometricAC[n][exp] analytically continues Hypergeometric2F1 functions in
exp. The second argument n refers to the equation number ($n$) in chapter 2.10
of \"Higher Transcendental Functions\" by Erdelyi, Magnus, Oberhettinger,
Tricomi. In case of eq. (6) (p.109) the last line is returned for
HypergeometricAC[6][exp], while the first equality is given by
HypergeometricAC[61][exp].

(2.10.1) is identical to eq. (9.5.7) of \"Special Functions & their
Applications\" by N.N.Lebedev.";

HypergeometricIR::usage =
"HypergeometricIR[exp, t] substitutes for all Hypergeometric2F1[a,b,c,z] in exp
by its Euler integral representation. The factor Integratedx[t, 0, 1] can be
omitted by setting the option Integratedx -> False.";

HypergeometricSE::usage =
"HypergeometricSE[exp, nu] expresses Hypergeometric functions by their series
expansion in terms of a sum (the Sum is omitted and nu, running from $0$ to
$\\infty$, is the summation index).";

HypExplicit::usage =
"HypExplicit[exp, nu] expresses Hypergeometric functions in exp by their
definition in terms of a sum (the Sum is omitted and nu is the summation
index).";

HypInt::usage =
"HypInt[exp, t] substitutes all Hypergeometric2F1[a,b,c,z] in exp with
Gamma[c]/(Gamma[b] Gamma[c-b]) Integratedx[t,0,1]  t^(b-1) (1-t)^(c-b-1) (1-t
z)^(-a).";

ToHypergeometric::usage =
"ToHypergeometric[t^b (1 - t)^c (1+tz)^a,t] returns u^a Gamma[b+1]
Gamma[c+1]/Gamma[b+c+2] Hypergeometric2F1[-a,b+1,b+c+2,-z/u]. Remember that
$\\textrm{Re}(b) >0$ and $\\textrm{Re} (c-b) > 0$ should hold (need not be set
in Mathematica).";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Hypergeometric`Private`"]

tohyp::usage="";

Options[HypergeometricAC] = {Collect2 -> True};

HypergeometricAC[n_Integer][exp_, opt___Rule] :=
	Block[ {g = Gamma, A1, A2, B1, B2, f21 = Hypergeometric2F1,
	res, colopt, vars, hyp},
		colopt = Collect2 /. {opt} /. Options[HypergeometricAC];
		A1[a_,b_,c_] :=
			g[c] g[c-a-b]/g[c-a]/g[c-b];
		A2[a_,b_,c_] :=
			g[c] g[a+b-c]/g[a]/g[b];
		B1[a_,b_,c_] :=
			g[c] g[b-a]/g[b]/g[c-a];
		B2[a_,b_,c_] :=
			g[c] g[a-b]/g[a]/g[c-b];
		res =
		exp /.
			Which[ n === 1,
						f21[a_,b_,c_,z_]  :>
						A1[a,b,c] f21[a,b,a+b-c+1,1-z] +
						A2[a,b,c] (1-z)^(c-a-b) f21[c-a,c-b,c-a-b+1,1-z]
						,
				n === 2,
						f21[a_,b_,c_,z_]  :>
						B1[a,b,c] (-z)^(-a) f21[a,1-c+a,1-b+a,1/z] +
						B2[a,b,c] (-z)^(-b) f21[b,1-c+b,1-a+b,1/z]
						,
				n === 3,
						f21[a_,b_,c_,z_]  :>
						B1[a,b,c] (1-z)^(-a) f21[a,c-b,a-b+1,Factor[1/(1-z)]] +
						B2[a,b,c] (1-z)^(-b) f21[b,c-a,b-a+1,Factor[1/(1-z)]]
						,
				n === 4,
						f21[a_,b_,c_,z_]  :>
						A1[a,b,c]  (z)^(-a) f21[a,a+1-c,a+b+1-c,Factor[1-1/z]] +
						A2[a,b,c]  (z)^(a-c) (1-z)^(c-a-b) *
							f21[c-a,1-a,c+1-a-b,Factor[1-1/z]]
						,
				n === 6,
						f21[a_,b_,c_,z_]  :>
						Factor[1-z]^(-b) f21[b,c-a,c, Factor[z/(z-1)]]
						,
				n === 61,
						f21[a_,b_,c_,z_]  :>
						Factor[1-z]^(-a) f21[a,c-b,c, Factor[z/(z-1)]]
				]/.(1/u_)^ep_ :> u^(-ep);
		If[ colopt === True,
			vars = Variables[Last/@ Cases2[exp /. f21 -> hyp, hyp]];
			res = Factor2 /@ Collect2[res, vars]
		];
		res
	];

Options[HypergeometricIR] = {Integratedx -> False};

HypergeometricIR[exp_, t_, opt___Rule] :=
	(
	exp /. Hypergeometric2F1[a_, b_, c_, -(1-x_)^2/(4 x_)] :>
	If[ Integratedx /. {opt} /.
	Options[HypergeometricIR],
		Integratedx[t,0,1],
		1
	]*
	( 2^(2*b)*Gamma[c]/(Gamma[b] Gamma[c - b])*
	((1 - t)^(Expand[-1 - 2*b + 2*c])*
	t^(Expand[-1 + b])*(1 + t)^(Expand[1 + 2*a - 2*c]))/
	((1 + t/x)^a*(1 + t*x)^Expand[a])
	) /.
	Hypergeometric2F1[a_,b_,c_,z_] :>
	FunctionExpand[Gamma[c]/(Gamma[b] Gamma[c-b])]*
	If[ Integratedx /. {opt} /.
	Options[HypergeometricIR],
		Integratedx[t,0,1],
		1
	] t^(b-1) (1-t)^(c-b-1) Factor2[1-t z]^(-a)
	);

Options[HypergeometricSE] = {Simplify -> FullSimplify};

p2g[a_, n_] :=
	Gamma[a + n] / Gamma[a];

HypergeometricSE[exp_, nu_, opt___Rule] :=
	Block[ {simp},
		simp = Simplify /. {opt} /. Options[HypergeometricSE];
		exp /.
			{HypergeometricPFQ[{a_, b_}, {c_}, z_] :>
				simp[ p2g[a, nu] p2g[b, nu]/p2g[c,nu]/Gamma[nu+1] ] z^nu,
			Hypergeometric2F1[a_, b_, c_, z_] :>
				simp[ p2g[a, nu] p2g[b, nu]/p2g[c,nu]/Gamma[nu+1] ] z^nu,
			HypergeometricPFQ[ numlist_, denlist_, z_] :>
				simp[ (Times@@Map[p2g[#,nu]&, numlist])/
							(Times@@Map[p2g[#,nu]&, denlist])/Gamma[nu+1]
							] z^nu
			}
	];

p2g[a_, n_] :=
	Gamma[a + n] / Gamma[a];

HypExplicit[exp_, nu_] :=
	exp /.
	{HypergeometricPFQ[{a_, b_}, {c_}, z_] :>
	FullSimplify[ p2g[a, nu] p2g[b, nu]/p2g[c,nu]/Gamma[nu+1] ] z^nu,
	Hypergeometric2F1[a_, b_, c_, z_] :>
	FullSimplify[ p2g[a, nu] p2g[b, nu]/p2g[c,nu]/Gamma[nu+1] ] z^nu,
	HypergeometricPFQ[ numlist_, denlist_, z_] :>
	FullSimplify[ (Times@@Map[p2g[#,nu]&, numlist])/
	(Times@@Map[p2g[#,nu]&, denlist])/Gamma[nu+1]
	] z^nu
	};

HypInt[exp_, t_] :=
	exp /. Hypergeometric2F1[a_,b_,c_,z_] :>
	FunctionExpand[Gamma[c]/(Gamma[b] Gamma[c-b])] Integratedx[t,0,1] *
	t^(b-1) (1-t)^(c-b-1) (1-t Factor[z])^(-a);

ToHypergeometric[exp_, t_] :=
	If[ FreeQ[exp, t^b_. (1-t)^c_. ((u_/;FreeQ[u,t]) + t z_)^a_.],
		exp,
		If[ Head[exp] === Plus,
			ToHypergeometric[#, t] &/@ exp,
			Select[exp, FreeQ[#, t]&] tohyp[Select[exp, !FreeQ[#, t]&]]/.
			tohyp[t^b_. (1-t)^c_. ((u_/;FreeQ[u,t]) + t z_)^a_.] :>
				u^a Gamma[b+1] Gamma[c+1]/Gamma[b+c+2] *
				Hypergeometric2F1[-a,b+1,b+c+2,Factor[-z/u]]
		]
	];

FCPrint[1, "Hypergeometric loaded"];
End[]
