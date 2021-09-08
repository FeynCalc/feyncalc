(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SimplifyPolyLog													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Simplifies expressions containing polylogarithms				*)

(* ------------------------------------------------------------------------ *)


SimplifyPolyLog::usage =
"SimplifyPolyLog[y] performs several simplifications assuming that the
variables occuring in the Log and PolyLog functions are between 0 and 1.

The simplifications will in general not be valid if the arguments are complex
or outside the range between 0 and 1.";

SPL::usage=
"SPL is an abbreviation for SimplifyPolyLog.";

SimplifyPolyLog::failmsg =
"Error! SimplifyPolyLog has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`SimplifyPolyLog`Private`"]

SPL=SimplifyPolyLog;

optEulerGamma::usage="";
optLog::usage="";
optPolyLog::usage="";
optSqrt::usage="";
optTrig::usage="";
splVerbose::usage="";

Options[SimplifyPolyLog] = {
	EulerGamma 		-> True,
	FCVerbose		-> False,
	Log 			-> True,
	MaxIterations	-> 6,
	Nielsen			-> True,
	PolyLog			-> True,
	Sqrt			-> True,
	Trig			-> True
};

SimplifyPolyLog[expr_, OptionsPattern[]]/; FCPatternFreeQ[{expr}] :=
	Block[{logSimp, tmp, repRule={}, var},


		If [OptionValue[FCVerbose]===False,
			splVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				splVerbose=OptionValue[FCVerbose]
			];
		];

		optEulerGamma 	= OptionValue[EulerGamma];
		optLog 			= OptionValue[Log];
		optPolyLog 		= OptionValue[PolyLog];
		optSqrt 		= OptionValue[Sqrt];
		optTrig 		= OptionValue[Trig];

		logSimp = {Log :> simplifyArgumentLog, PolyLog :> simplifyArgumentPolyLog};

		FCPrint[1, "SimplifyPolyLog: Entering.", FCDoControl->splVerbose];
		FCPrint[3, "SimplifyPolyLog: Entering with ", expr, FCDoControl->splVerbose];


		tmp = expr;

		tmp = tmp /. Zeta2->(Pi^2/6)/. logSimp;

		FCPrint[3, "SimplifyPolyLog: After logSimp: ", tmp, FCDoControl->splVerbose];

		tmp = FixedPoint[(# /. logSimp /. simptab)&, tmp, OptionValue[MaxIterations]];

		tmp = tmp /. logSimp /. Pi^2->6 Zeta2;

		FCPrint[3, "SimplifyPolyLog: Intermediate result: ", tmp, FCDoControl->splVerbose];

		If[	!FreeQ2[tmp,{simplifyArgument,simplifyArgumentPolyLog}],
			Message[SimplifyPolyLog::failmsg, "Simplification of the arguments of Polylogs failed."];
			Abort[]
		];


		tmp = tmp/. PolyLog[a_,b_,c_]:> Nielsen[a,b,c];

		If[	!OptionValue[Nielsen],
			tmp = tmp/. Nielsen[z__]:> Nielsen[z,PolyLog->True]
		];

		tmp = Expand2[tmp,{Log,Pi,Zeta2,Zeta4,Zeta6}];

		tmp = tmp /. {
				Pi^2 :> 6 Zeta2,
				Pi^3 :> 6 Pi Zeta2,
				Pi^4 :> 90 Zeta4,
				Pi^5 :> 90 Pi Zeta4,
				Pi^6 :> 945 Zeta6,
				Pi^7 :> 945 Pi Zeta6,
				Pi^8 :> 9450 Zeta8,
				Pi^9 :> 9450 Pi Zeta8,
				Pi^10 :> 93555 Zeta10,
				Pi^11 :> 93555 Pi Zeta10,
				Log[x_Integer?EvenQ] :> PowerExpand[Log[x]]
		};

		tmp = tmp //. {
			Zeta2^2 -> 5 Zeta4/2,
			Zeta2^3 -> 35 Zeta6/8,
			Zeta2^4 -> 175 Zeta8/24,
			Zeta2^5 -> 385 Zeta10/32,

			Zeta4^2 -> 7 Zeta8/6,
			Zeta2*Zeta4 -> 7 Zeta6/4,
			Zeta2*Zeta6 -> 5 Zeta8/3,
			Zeta2*Zeta8 -> 33 Zeta10/20,
			Zeta4*Zeta6 -> 11 Zeta10/10,

			Zeta2^n_/; n>5 -> Zeta[2]^n,
			Zeta4^n_/; n>2 -> Zeta[4]^n,
			Zeta6^n_/; n>1 -> Zeta[6]^n,
			Zeta8^n_/; n>1 -> Zeta[8]^n,
			Zeta10^n_/; n>1 -> Zeta[10]^n
		};

		FCPrint[1, "SimplifyPolyLog: Leaving.", FCDoControl->splVerbose];
		FCPrint[3, "SimplifyPolyLog: Leaving with ", tmp, FCDoControl->splVerbose];

		tmp

	];

(*The arguments of logs and polylogs might contain scalar products, so a naive memoization is not safe here! *)

simplifyArgument[su_, head_, {}] :=
	If[	FreeQ[su,Plus],
		head[su],
		head[Factor2[Cancel[Factor2[su]]]]
	];

simplifyArgument[su1_, su2_, head_, {}] :=
	If[	FreeQ[su2,Plus],
		head[su1,su2],
		head[su1,Factor2[Cancel[Factor2[su2]]]]
	];

simplifyArgumentLog[su_, OptionsPattern[]] :=
	FCUseCache[simplifyArgument,{su, Log}];

simplifyArgumentPolyLog[su1_,su2_] :=
	FCUseCache[simplifyArgument,{su1, su2, PolyLog}];

(*Currently no simplifications for Nielsen's polylogs*)
simplifyArgumentPolyLog[su1_,su2_, su3_] :=
	PolyLog[su1,su2,su3];

polyGammaExpand[PolyGamma[a_,b_]]:=
	Expand[FunctionExpand[PolyGamma[a,b]] /. EulerGamma->0]/; !optEulerGamma;

polyGammaExpand[PolyGamma[a_,b_]]:=
	Expand[FunctionExpand[PolyGamma[a,b]]]/; optEulerGamma;

(*
	These relations follow from the basic properties of polylogs.
	Many of them can be found in L. Lewin's "Polylogarithms and Associated Functions"
	and "Table of Integrals and Formulae for Feynman Diagram Calculations" by Devoto and Duke.
	Furthermore, new relations can be easily derived by differentiating the given polylog
	w.r.t x and then integrating in x from 0 to t or from 1 to z (whatever converges better).
	For example

	Integrate[D[PolyLog[2, 1 - x], x], {x, 0, t},  Assumptions -> {t > 0, t < 1}]

	and so on. The correctness of the expression can be then checked via
	Plot[exp1-exp2,{x,0,1}]. This is the only way, as Mathematica will not recognize most
	of these transformations.
*)

simptab =
{
	PolyGamma[a_Integer, b_?NumberQ] :>
		polyGammaExpand[PolyGamma[a,b]],

	(*The duplication formula *)
	PolyLog[s_, -Sqrt[x_Symbol]]/; optSqrt && optPolyLog  :>
		2^(1-s) PolyLog[s, x] - PolyLog[s, Sqrt[x]],(*,

	PolyLog[s_, -x_Symbol] :>
		2^(1-s) PolyLog[s, x^2] - PolyLog[s, x],
	*)

	PolyLog[2, -((1 - 2*t_Symbol)/t_Symbol)]/; optPolyLog :>
		(
		-Zeta2 +
		2*Log[2]*Log[1 - 2*t] +
		2*Log[1 - 2*t]*Log[t] +
		2*Log[1 - t]*Log[t] -
		Log[t]^2 -
		2*Log[2]*Log[-1 + 2*t] -
		2*Log[1 - t]*Log[-1 + 2*t] +
		2*PolyLog[2, 1 - t] -
		2*PolyLog[2, 2*(1 - t)] +
		2*PolyLog[2, 2*t]
		)/2,

	PolyLog[2, (2*(1 - t_Symbol))/(1 - 2*t_Symbol)]/; optPolyLog :>
		(
		6*Zeta2 -
		2*Log[2]*Log[-(1 - 2*t)^(-1)] +
		2*I*Pi*Log[1 - 2*t] -
		Log[1 - 2*t]^2 -
		2*I*Pi*Log[1 - t] +
		2*Log[1 - 2*t]*Log[1 - t] -
		2*Log[-(1 - 2*t)^(-1)]*Log[(1 - t)/(1 - 2*t)]  -
		2*Log[(1 - t)/(1 - 2*t)]*Log[t] +
		2*Log[(1 - t)/(1 - 2*t)]*Log[-(t/(1 - 2*t))] -
		2*Log[2]*Log[-1 + 2*t] -
		2*Log[1 - t]*Log[-1 + 2*t] -
		2*PolyLog[2, 2*(1 - t)]
		)/ 2,

	PolyLog[2, (1 - t_Symbol)^2/(1 - 2*t_Symbol)]/; optPolyLog :>
		(
		2*I*Pi*Log[1 - 2*t] +
		4*Log[2]*Log[1 - 2*t] -
		Log[1 - 2*t]^2 -
		4*I*Pi*Log[1 - t] +
		4*Log[1 - 2*t]*Log[1 - t] +
		4*Log[1 - 2*t]*Log[t] -
		4*Log[2]*Log[-1 + 2*t] -
		4*Log[1 - t]*Log[-1 + 2*t] +
		8*PolyLog[2, 1 - t] -
		4*PolyLog[2,2*(1 - t)] +
		4*PolyLog[2, 2*t]
		)/2,

	PolyLog[2, (1 - t_Symbol)/(1 - 2*t_Symbol)]/; optPolyLog :>
		(
		3*Zeta2 +
		2*I*Pi*Log[1 - 2*t] +
		2*Log[2]*Log[1 - 2*t] -
		Log[1 - 2*t]^2 -
		2*I*Pi*Log[1 - t] +
		2*Log[1 - 2*t]*Log[1 - t] +
		2*Log[1 - 2*t]*Log[t] -
		2*Log[2]*Log[-1 + 2*t] -
		2*Log[1 - t]*Log[-1 + 2*t] +
		2*PolyLog[2, 1 - t] -
		2*PolyLog[2, 2*(1 - t)] +
		2*PolyLog[2, 2*t]
		)/2,

	Log[-(t_Symbol^2/(1 - 2*t_Symbol))] :>
		Log[t] + Log[-(t/(1 - 2*t))],

	PolyLog[2, -(t_Symbol/(1 - 2*t_Symbol))]/; optPolyLog :>
		(
		Zeta2 -
		Log[(1 - t)/(1 - 2*t)]*Log[-(t/(1 - 2*t))] -
		PolyLog[2, (1 - t)/(1 - 2*t)]
		),

	PolyLog[2, -(t_Symbol^2/(1 - 2*t_Symbol))]/; optPolyLog :>
		(
		Zeta2 -
		Log[(1 - t)^2/(1 - 2*t)]*Log[-(t^2/(1 - 2*t))] -
		PolyLog[2, (1 - t)^2/(1 - 2*t)]
		),

	PolyLog[3, (1 - t_Symbol)^2/t_Symbol^2]/; optPolyLog :>
		(
		2*Pi^2*Log[1 - t]/3 -
		4*Log[1 - t]^2*Log[-((1 - 2*t)/t)] +
		8*Log[1 - t]*Log[-((1 - 2*t)/t)]*Log[t] -
		2*Log[1 - t]*Log[t]^2 -
		4*Log[-((1 - 2*t)/t)]*Log[t]^2 +
		(2*Log[t]^3)/3 -
		4*Log[1 - t]*PolyLog[2, -((1 - 2*t)/t)] +
		4*Log[t]*PolyLog[2, -((1 - 2*t)/t)] -
		4*Log[1 - t]*PolyLog[2, (1 - t)/t] + 4*Log[t]*PolyLog[2, (1 - t)/t] -
		4*PolyLog[3, 1 - t] + 4*PolyLog[3, (1 - t)/t] - 4*PolyLog[3, t] +
		4*Zeta[3]
		),

	PolyLog[2, 2 - x_Symbol]/; optPolyLog :>
		(
		Zeta2 -
		I*Pi*Log[2 - x] -
		Log[1 - x]*Log[2 - x] -
		PolyLog[2, -1 + x]
		),

	PolyLog[2, (1 + x_Symbol)/x_Symbol]/; optPolyLog :>
		(
		2*Zeta2 +
		I*Pi*Log[x] -
		Log[x]^2/2 -
		I*Pi*Log[1 + x] +
		Log[x]*Log[1 + x] +
		PolyLog[2, -x]
		),

	PolyLog[2, (1 - x_Symbol)/2]/; optPolyLog :>
		(
		Zeta2 -
		Log[2]^2 + 2*Log[2]*Log[1 + x] + 2*Log[x]*Log[1 + x] -
		Log[1 + x]^2 + 2*PolyLog[2, 1 - x] + 2*PolyLog[2, -x] -
		2*PolyLog[2, (1 - x)/(1 + x)]
		)/2,

	PolyLog[2, (1 - Sqrt[x_Symbol])/2]/; optSqrt && optPolyLog :>
		(
		-Log[2]^2/2 + Log[2]*Log[1 + Sqrt[x]] - Log[1 + Sqrt[x]]^2/2 +
		2*PolyLog[2, 1 - Sqrt[x]] - PolyLog[2, (1 - Sqrt[x])/(1 + Sqrt[x])] -
		PolyLog[2, 1 - x]/2
		),

	PolyLog[2, (1 - Sqrt[x_Symbol])/2]/; optSqrt  && optPolyLog :>
		(
		- PolyLog[2, (1 + Sqrt[x])/2] +
		Zeta2 -
		Log[2]^2 + Log[1 + Sqrt[x]]^2 + Log[2] Log[1 - x] -
		Log[1 + Sqrt[x]] Log[1 - x]
		),

	PolyLog[2, -((1 - x_Symbol)/(2*x_Symbol))]/; optPolyLog :>
		(
		4*Zeta2 +
		4*I*Pi*Log[2] -
		3*Log[2]^2 +
		4*Log[2]*Log[1 - x] +
		4*I*Pi*Log[x] -
		6*Log[2]*Log[x] +
		4*Log[1 - x]*Log[x] -
		3*Log[x]^2 -
		4*I*Pi*Log[1 + x] +
		2*Log[2]*Log[1 + x] -
		4*Log[1 - x]*Log[1 + x] +
		2*Log[x]*Log[1 + x] +
		Log[1 + x]^2 +
		2*PolyLog[2, (1 - x)/(1 + x)] -
		4*PolyLog[2, (1 + x)/(2*x)]
		)/2,

	PolyLog[3, -(x_^2/(1 - x_^2))]/; optPolyLog  :>
		(
		7*Zeta2*Log[1 - x] +
		I*Pi*Log[1 - x]^2 +
		Log[1 - x]^3/6 -
		Log[1 - x]^2*Log[x] +
		Zeta2*Log[1 + x] -
		2*I*Pi*Log[1 - x]*Log[1 + x] +
		(Log[1 - x]^2*Log[1 + x])/2 -
		2*Log[1 - x]*Log[x]*Log[1 + x] -
		I*Pi*Log[1 + x]^2 +
		(Log[1 - x]*Log[1 + x]^2)/2 -
		Log[x]*Log[1 + x]^2 +
		Log[1 + x]^3/6 -
		4*PolyLog[3, 1 - x] -
		4*PolyLog[3, -x] -
		4*PolyLog[3, x] -
		4*PolyLog[3, 1 + x] -
		2*PolyLog[3, -((1 + x)/(1 - x))] +
		2*PolyLog[3, (1 + x)/(1 - x)] +
		(9*Zeta[3])/2
		),

	PolyLog[2, -(x_^2/(1 - x_^2))]/; optPolyLog  :>
		(
		-2*Zeta2 -
		Log[1 - x]^2/2 +
		2*Log[1 - x]*Log[x] -
		Log[1 - x]*Log[1 + x] -
		Log[1 + x]^2/2 +
		2*PolyLog[2, 1 - x] -
		2*PolyLog[2, -x]
		),

	PolyLog[3, -(1 - x_Symbol)^(-1)]/; optPolyLog  :>
		Log[1 - x]^3/6 + Zeta2*Log[1 - x] + PolyLog[3, -1 + x],

	PolyLog[3, (1 - x_Symbol)^2]/; optPolyLog  :>
		4*PolyLog[3, 1 - x] + 4*PolyLog[3, -1 + x],

	PolyLog[2, -1 + 2*x_Symbol]/; optPolyLog  :>
		Zeta2 - Log[2]*Log[-1 + 2*x] - Log[1 - x]*Log[-1 + 2*x] - PolyLog[2, 2*(1 - x)],

	PolyLog[2, (1 - 2*x_Symbol)/(1 - x_Symbol)]/; optPolyLog  :>
		-Log[1 - x]^2/2 + Log[1 - x]*Log[x] - Log[x]^2/2 - PolyLog[2, -((1 - 2*x)/x)],

	PolyLog[2, (1 - 2*x_Symbol)/(2*(1 - x_Symbol))]/; optPolyLog  :>
		-Log[2]^2/2 - Log[2]*Log[1 - x] - Log[1 - x]^2/2 - PolyLog[2, -1 + 2*x],

	PolyLog[2, (1 - x_Symbol)/x_Symbol]/; optPolyLog  :>
		(
		Zeta2 -
		I*Pi*Log[1 - x] -
		Log[1 - 2*x]*Log[1 - x] +
		I*Pi*Log[x] +
		Log[1 - 2*x]*Log[x] +
		Log[1 - x]*Log[x] -
		Log[x]^2 -
		PolyLog[2, -((1 - 2*x)/x)]
		),

	PolyLog[2, 1/(2*(1 - x_Symbol))]/; optPolyLog  :>
		(
		Zeta2 -
		Log[2]^2/2 +
		Log[2]*Log[1 - 2*x] -
		Log[2]*Log[1 - x] +
		Log[1 - 2*x]*Log[1 - x] -
		Log[1 - x]^2/2 +
		PolyLog[2, -1 + 2*x]
		),

	PolyLog[2, x_Symbol/(1 + x_Symbol)]/; optPolyLog  :>
			-Pi^2/6 + I*Pi*Log[1 + x] + Log[x]*Log[1 + x] - Log[1 + x]^2/2 + PolyLog[2, 1 + x],

	PolyLog[3, x_^(-2)]/; optPolyLog  :>
		(
		2*Pi^2*Log[x] -
		12*Zeta2*Log[x] -
		2*I*Pi*Log[x]^2 -
		4*Log[1 - x]*Log[x]^2 +
		(4*Log[x]^3)/3 -
		4*Log[x]*PolyLog[2, 1 - x] -
		4*Log[x]*PolyLog[2, x] +
		4*PolyLog[3, -x] +
		4*PolyLog[3, x]
		),

	PolyLog[3, (1 - x_Symbol^2)^(-1)]/; optPolyLog  :>
		(
		-8*Zeta2*Log[1 - x] -
		(3*I)/2*Pi*Log[1 - x]^2 +
		Log[1 - x]^3/6 -
		2*Zeta2*Log[1 + x] +
		I*Pi*Log[1 - x]*Log[1 + x] +
		(Log[1 - x]^2*Log[1 + x])/2 +
		I/2*Pi*Log[1 + x]^2 +
		(Log[1 - x]*Log[1 + x]^2)/2 +
		Log[1 + x]^3/6 +
		4*PolyLog[3, 1 - x] +
		4*PolyLog[3, 1 + x] +
		2*PolyLog[3, -((1 + x)/(1 - x))] -
		2*PolyLog[3, (1 + x)/(1 - x)] -
		(7*Zeta[3])/2
		),

	PolyLog[3, 1 - x_Symbol^2]/; optPolyLog  :>
		(
		-6*Zeta2*Log[1 - x] -
		I*Pi*Log[1 - x]^2 +
		2*I*Pi*Log[1 - x]*Log[1 + x] +
		I*Pi*Log[1 + x]^2 +
		4*PolyLog[3, 1 - x] +
		4*PolyLog[3, 1 + x] +
		2*PolyLog[3, -((1 + x)/(1 - x))] -
		2*PolyLog[3, (1 + x)/(1 - x)] -
		(7*Zeta[3])/2
		),

	PolyLog[2, 2/(1 - x_Symbol)]/; optPolyLog  :>
		(
		7*Zeta2 -
		2*I*Pi*Log[2] +
		4*I*Pi*Log[1 - x] +
		2*Log[2]*Log[1 - x] -
		2*Log[1 - x]^2 -
		2*I*Pi*Log[1 + x] -
		2*Log[2]*Log[1 + x] +
		2*Log[1 - x]*Log[1 + x] -
		2*Log[x]*Log[1 + x] -
		2*PolyLog[2, 1 - x] -
		2*PolyLog[2, -x] -
		2*PolyLog[2, (1 + x)/(1 - x)]
		)/2,

	PolyLog[2, 2/(1 + x_Symbol)]/; optPolyLog  :>
		(
		-(Log[-1 + x]*Log[2/(1 + x)]) +
		(Log[2/(1 + x)]*Log[2*(1 + x)])/2 +
		PolyLog[2, (1 - x)/2] + Zeta2
		),

	PolyLog[2, -((1 + x_Symbol)/(1 - x_Symbol))]/; optPolyLog  :>
		(
		-5*Pi^2 -
		12*I*Pi*Log[1 - x] +
		12*I*Pi*Log[1 + x] +
		12*Log[x]*Log[1 + x] +
		12*PolyLog[2, 1 - x] +
		12*PolyLog[2, -x] +
		12*PolyLog[2, (1 + x)/(1 - x)]
		)/12,

	PolyLog[2, (1 + x_Symbol)/(-1 + x_Symbol)]/; optPolyLog  :>
		(
		-5*Pi^2 -
		12*I*Pi*Log[1 - x] +
		12*I*Pi*Log[1 + x] +
		12*Log[x]*Log[1 + x] +
		12*PolyLog[2, 1 - x] +
		12*PolyLog[2, -x] +
		12*PolyLog[2, (1 + x)/(1 - x)]
		)/12,

	PolyLog[2, 1 + x_Symbol]/; optPolyLog  :>
		Zeta2 - I*Pi*Log[1 + x] - Log[x]*Log[1 + x] - PolyLog[2, -x],

	PolyLog[2, 1 + Sqrt[x_Symbol]] /; optSqrt && optPolyLog  :>
		(
		- PolyLog[2, -Sqrt[x]] +
		Zeta2 - I Pi Log[1 + Sqrt[x]] - 1/2 Log[1 + Sqrt[x]] Log[x]
		),


	PolyLog[2,1/x_]/; optPolyLog  :>
		Log[(x-1)/x] Log[x] + Pi^2/3 - PolyLog[2,x] - Log[x] Log[1-x] + 1/2 Log[x]^2,

	PolyLog[2,-1/x_]/; optPolyLog  :>
		Log[(+x+1)/x] Log[-x] + Pi^2/3 - PolyLog[2,-x] - Log[-x] Log[1+x] + 1/2 Log[-x]^2,

	(* a matter of taste
	PolyLog[2, 1 - x_] -> Zeta2 - Log[1 - x] Log[x] - PolyLog[2, x], *)

	PolyLog[2, x_ /; FreeQ2[x,{Plus,Times,Power}]] && optPolyLog :>
		Zeta2 - Log[1 - x] Log[x] - PolyLog[2, 1 - x],

	PolyLog[2, Sqrt[x_Symbol]]/; optSqrt  && optPolyLog :>
		(
		- PolyLog[2, 1 - Sqrt[x]] +
		Zeta2 + 1/2 Log[1 + Sqrt[x]] Log[x] - 1/2 Log[1 - x] Log[x]
		),

	PolyLog[2, x_^(-2)]/; optPolyLog :>
		2*Zeta2 + 2*I*Pi*Log[x] + 2*Log[1 - x]*Log[x] - Log[x]^2 + 2*PolyLog[2, 1 - x] + 2*PolyLog[2, -x^(-1)],

	PolyLog[2, (1 + x_Symbol)/(1 - x_Symbol^2)]/; optPolyLog :>
		2*Zeta2 + I*Pi*Log[1 - x] - Log[1 - x]^2/2 - PolyLog[2, 1 - x],

	PolyLog[2,1-x_^2]/; optPolyLog :>
		PolyLog[2,1-x] - PolyLog[2,x]-2 PolyLog[2,-x]- Log[x] Log[1-x] - 2 Log[x] Log[1+x],

	PolyLog[2, -((1 - x_Symbol)/(1 + x_Symbol))]/; optPolyLog :>
		(-3*Zeta2)/2 - Log[x]*(-Log[1 - x] + Log[1 + x]) - PolyLog[2, -x] + PolyLog[2, x] + PolyLog[2, (1 - x)/(1 + x)],

	PolyLog[2, ((x_Symbol -1)/(1 + x_Symbol))]/; optPolyLog :>
		(-3*Zeta2)/2 - Log[x]*(-Log[1 - x] + Log[1 + x]) - PolyLog[2, -x] + PolyLog[2, x] + PolyLog[2, (1 - x)/(1 + x)],

	PolyLog[2, (1 + x_Symbol)/2]/; optPolyLog :>
		(
		Zeta2/2 -
		Log[2]^2/2 +
		Log[2]*Log[1 - x] -
		Log[1 - x]*Log[1 + x] -
		Log[x]*Log[1 + x] +
		Log[1 + x]^2/2 -
		PolyLog[2, 1 - x] -
		PolyLog[2, -x] +
		PolyLog[2, (1 - x)/(1 + x)]
		),

	PolyLog[2, (1 + Sqrt[x_Symbol])/2]/; optSqrt && optPolyLog :>
		(
		Zeta2 - Log[2]^2/2 - Log[2] Log[1 + Sqrt[x]] +
		3/2 Log[1 + Sqrt[x]]^2 + Log[2] Log[1 - x] -
		Log[1 + Sqrt[x]] Log[1 - x] - 2 PolyLog[2, 1 - Sqrt[x]] +
		PolyLog[2, (1 - Sqrt[x])/(1 + Sqrt[x])] + 1/2 PolyLog[2, 1 - x]
		),

	PolyLog[2, (1 + x_Symbol)/(1 - x_Symbol)]/; optPolyLog :>
		(
		2*Zeta2 +
		I*Pi*Log[1 - x] -
		Log[1 - x]^2/2 -
		I*Pi*Log[1 + x] +
		Log[1 - x]*Log[1 + x] -
		Log[1 + x]^2/2 -
		PolyLog[2, (1 - x)/(1 + x)]
		),

	PolyLog[2, (-2*x_Symbol)/(1 - x_Symbol)]/; optPolyLog :>
		(
		Zeta2 +
		I*Pi*Log[1 - x] +
		Log[2]*Log[1 - x] -
		Log[1 - x]^2 +
		Log[1 - x]*Log[x] -
		I*Pi*Log[1 + x] -
		Log[2]*Log[1 + x] +
		Log[1 - x]*Log[1 + x] -
		Log[x]*Log[1 + x] -
		PolyLog[2, (1 + x)/(1 - x)]
		),

	PolyLog[2, x_^2]/; optPolyLog :>
		Zeta2 - Log[1 - x]*Log[x] - PolyLog[2, 1 - x] + 2*PolyLog[2, -x] + PolyLog[2, x],

	PolyLog[2, (1 + x_Symbol)/(2*x_Symbol)]/; optPolyLog :>
		(
		2*Zeta2 +
		2*I*Pi*Log[2] -
		Log[2]^2 +
		2*Log[2]*Log[1 - x] +
		2*I*Pi*Log[x] -
		2*Log[2]*Log[x] +
		2*Log[1 - x]*Log[x] -
		Log[x]^2 -
		2*I*Pi*Log[1 + x] -
		2*Log[1 - x]*Log[1 + x] +
		Log[1 + x]^2 +
		2*PolyLog[2, (1 - x)/(1 + x)]
		)/2,


	PolyLog[2, x_Symbol/(1 + x_Symbol)]/; optPolyLog :>
		(-Log[1 + x]^2 - 2*PolyLog[2, -x])/2,

	PolyLog[2, 2 x_Symbol/(1 + x_Symbol)]/; optPolyLog :>
		(
		-Pi^2/12 + Log[2]^2/2 - Log[1 - x]*Log[2/(1 + x)] - Log[1 + x]^2/2 + PolyLog[2, (1 - x)/2] - PolyLog[2, -x] + PolyLog[2, x]
		),

	PolyLog[2, -x_Symbol/(1-x_Symbol)]/; optPolyLog :>
		-1/2 Log[1-x]^2 - PolyLog[2, x],

	PolyLog[2, 1 - 1/x_Symbol]/; optPolyLog :>
		-Log[x]^2/2 - PolyLog[2, 1 - x],

	PolyLog[2, (x_Symbol - 1)/x_Symbol]/; optPolyLog :>
		-Log[x]^2/2 - PolyLog[2, 1 - x],

	PolyLog[2, -(1 - x_Symbol)/x_Symbol]/; optPolyLog :>
		-Log[x]^2/2 - PolyLog[2, 1 - x],

	PolyLog[2,  x_Symbol/(x_Symbol -1)]/; optPolyLog :>
		-1/2 Log[1-x]^2 - PolyLog[2, x],

	PolyLog[3, x_Symbol]/; optPolyLog :>
		(2*Zeta2*Log[x] - Log[1 - x]*Log[x]^2 - 2*Nielsen[1, 2, 1 - x] - 2*Log[x]*PolyLog[2, 1 - x] + 2*Zeta[3])/2,

	(*
	Nielsen[1, 2, x_Symbol] :> (Log[1 - x]^2*Log[x])/2 +
		Log[1 - x]*PolyLog[2, 1 - x] - PolyLog[3, 1 - x] + Zeta[3], *)
	Nielsen[1,2, -x_/(1-x_)] :>
		-1/6 Log[1-x]^3 + Nielsen[1,2,x],

	PolyLog[2, -(x_^2/(1 - x_^2))]/; optPolyLog :>
		-2*Zeta2 - Log[1 - x]^2/2 + 2*Log[1 - x]*Log[x] - Log[1 - x]*Log[1 + x] - Log[1 + x]^2/2 + 2*PolyLog[2, 1 - x] - 2*PolyLog[2, -x],

	PolyLog[2, 1 - 2*x_Symbol]/; optPolyLog :>
		-Zeta2/2 + Log[1 - x]*Log[x] - Log[x]^2/2 + PolyLog[2, 1 - x] + PolyLog[2, -1 + 2*x] - PolyLog[2, (-1 + 2*x)/x],

	PolyLog[3, x_Symbol^(-1)]/; optPolyLog :>
		(-2*Pi^2*Log[x] - 3*I*Pi*Log[x]^2 + Log[x]^3 + 6*PolyLog[3, x])/6,

	PolyLog[3, (1 - x_Symbol)^(-1)]/; optPolyLog :>
		-2*Zeta2*Log[1 - x] - I/2*Pi*Log[1 - x]^2 + Log[1 - x]^3/6 + PolyLog[3, 1 - x],

	PolyLog[3, (1 + x_Symbol)/x_Symbol]/; optPolyLog :>
		(
		-12*Zeta2*Log[x] -
		3*I*Pi*Log[x]^2 +
		Log[x]^3 +
		18*Zeta2*Log[1 + x] +
		6*I*Pi*Log[x]*Log[1 + x] -
		3*Log[x]^2*Log[1 + x] -
		6*I*Pi*Log[1 + x]^2 -
		6*PolyLog[3, -x] -
		6*PolyLog[3, 1 + x] +
		6*Zeta[3]
		)/6,

		(* do it the other way round
		PolyLog[3, (1 + x_Symbol)^(-1)] :>
			(-9*I*Pi*Log[1 + x]^2 - 12*Log[x]*Log[1 + x]^2 + Log[1 + x]^3 -
		12*Log[1 + x]*PolyLog[2, -x] - 12*Log[1 + x]*PolyLog[2, 1 + x] +
					6*PolyLog[3, 1 + x])/6
		,*)
	PolyLog[3, 1 + x_Symbol]/; optPolyLog :>
		(
		12*Zeta2*Log[1 + x] -
		3*I*Pi*Log[1 + x]^2 -
		Log[1 + x]^3 +
		6*PolyLog[3, (1 + x)^(-1)]
		)/6,

	PolyLog[3, 1 + Sqrt[x_Symbol]]/; optSqrt && optPolyLog :>
		(
		2*Zeta2*Log[1 + Sqrt[x]] -
		(I/2)*Pi*Log[1 + Sqrt[x]]^2 -
		Log[1 + Sqrt[x]]^3/6 +
		PolyLog[3, (1 + Sqrt[x])^(-1)]
		),

	PolyLog[3, - (x_/;FreeQ[x,Plus])^(-1)]/; optPolyLog :>
		Zeta2*Log[x] + Log[x]^3/6 + PolyLog[3, -x],
	(*
	,
	PolyLog[3,1-x_Symbol] :>
		(Log[1 - x]^2*Log[x] - 2*Nielsen[1, 2, x] +
				2*Log[1 - x]*PolyLog[2, 1 - x] + 2*Zeta[3])/2
	*)

	PolyLog[3, 2/(1 + x_Symbol)]/; optPolyLog :>
		(
		(Zeta2*Log[2])/2 -
		Log[2]^3/6 -
		2*Zeta2*Log[1 + x] +
		I*Pi*Log[2]*Log[1 + x] +
		(Log[2]^2*Log[1 + x])/2 -
		(I/2)*Pi*Log[1 + x]^2 -
		(Log[2]*Log[1 + x]^2)/2 +
		Log[1 + x]^3/6 +
		PolyLog[3, 2] +
		PolyLog[3, (1 + x)/2] -
		(7*Zeta[3])/8
		),

	PolyLog[3, z_Symbol^2]/; optPolyLog :>
		(
		-4*((Log[1 - z]*Log[z]^2)/8 +
		(Log[z]^2*Log[1 + z])/8 -
		(Log[z]^2*Log[1 - z^2])/8 -
		PolyLog[3, -z] - PolyLog[3, z])
		),

	PolyLog[3, -x_Symbol/(1-x_Symbol)]/; optPolyLog :>
		-PolyLog[3, x] - PolyLog[3,1-x] + 1/6 Log[1-x]^3 - 1/2 Log[x] Log[1-x]^2 + Zeta2 Log[1-x] + Zeta[3],

	PolyLog[3,1 - 1/(x_/;FreeQ[x,Plus])]/; optPolyLog :>
		(
		Nielsen[1,2,x] -
		PolyLog[3,x] +
		Log[1-x] PolyLog[2,x] +
		1/6 Log[x]^3 +
		Zeta2 (Log[x]-Log[1-x])+
		1/2Log[x] Log[1-x] (Log[1-x] - Log[x])
		),

	PolyLog[3,-((1 - x_Symbol)/x_Symbol)]/; optPolyLog :>
		(
		Nielsen[1,2,x] -
		PolyLog[3,x] +
		Log[1-x] PolyLog[2,x] +
		1/6 Log[x]^3 +
		Zeta2 (Log[x]-Log[1-x])+
		1/2Log[x] Log[1-x] (Log[1-x] - Log[x])
		),

	PolyLog[3,(x_Symbol-1)/x_Symbol]/; optPolyLog :>
		(
		Nielsen[1,2,x] -
		PolyLog[3,x] +
		Log[1-x] PolyLog[2,x] +
		1/6 Log[x]^3 +
		Zeta2 (Log[x]-Log[1-x])+
		1/2Log[x] Log[1-x] (Log[1-x] - Log[x])
		),

	PolyLog[3, (1 + x_Symbol)/(2*x_Symbol)]/; optPolyLog :>
		(
		3*I*Pi*Log[2]^2 -
		4*Log[2]^3 +
		3*Log[2]^2*Log[1 - x] +
		6*I*Pi*Log[2]*Log[x] -
		12*Log[2]^2*Log[x] +
		6*Log[2]*Log[1 - x]*Log[x] +
		3*I*Pi*Log[x]^2 -
		12*Log[2]*Log[x]^2 +
		3*Log[1 - x]*Log[x]^2 -
		4*Log[x]^3 -
		6*I*Pi*Log[2]*Log[1 + x] +
		9*Log[2]^2*Log[1 + x] -
		6*Log[2]*Log[1 - x]*Log[1 + x] -
		6*I*Pi*Log[x]*Log[1 + x] +
		18*Log[2]*Log[x]*Log[1 + x] -
		6*Log[1 - x]*Log[x]*Log[1 + x] +
		9*Log[x]^2*Log[1 + x] +
		3*I*Pi*Log[1 + x]^2 -
		6*Log[2]*Log[1 + x]^2 +
		3*Log[1 - x]*Log[1 + x]^2 -
		6*Log[x]*Log[1 + x]^2 + Log[1 + x]^3 -
		6*Log[2]*PolyLog[2, -(1 - x)/(2*x)] -
		6*Log[x]*PolyLog[2, -(1 - x)/(2*x)] +
		6*Log[1 + x]*PolyLog[2, -(1 - x)/(2*x)] -
		6*Log[2]*PolyLog[2, (1 + x)/(2*x)] -
		6*Log[x]*PolyLog[2, (1 + x)/(2*x)] +
		6*Log[1 + x]*PolyLog[2, (1 + x)/(2*x)] -
		6*PolyLog[3, -(1 - x)/(2*x)] -
		6*PolyLog[3, (1 - x)/(1 + x)] +
		6*Zeta[3]
		)/6,

	PolyLog[3, (-2*x_Symbol)/(1 - x_Symbol)]/; optPolyLog :>
		(
		-6*Zeta2*Log[2] -
		Log[2]^3 +
		6*Zeta2*Log[1 - x] +
		3*Log[2]^2*Log[1 - x] -
		3*Log[2]*Log[1 - x]^2 +
		Log[1 - x]^3 -
		6*Zeta2*Log[x] -
		3*Log[2]^2*Log[x] +
		6*Log[2]*Log[1 - x]*Log[x] -
		3*Log[1 - x]^2*Log[x] -
		3*Log[2]*Log[x]^2 + 3*Log[1 - x]*Log[x]^2 -
		Log[x]^3 +
		6*PolyLog[3, -(1 - x)/(2*x)]
		)/6,


	PolyLog[3, (-2*Sqrt[x_Symbol])/(1 - Sqrt[x_Symbol])]/; optSqrt && optPolyLog :>
		(
		-(Zeta2*Log[2]) - Log[2]^3/6 + Zeta2*Log[1 - Sqrt[x]] + (Log[2]^2*Log[1 - Sqrt[x]])/2 - (Log[2]*Log[1 - Sqrt[x]]^2)/2 + Log[1 - Sqrt[x]]^3/6 -
		Zeta2*Log[Sqrt[x]] - (Log[2]^2*Log[Sqrt[x]])/2 + Log[2]*Log[1 - Sqrt[x]]*Log[Sqrt[x]] - (Log[1 - Sqrt[x]]^2*Log[Sqrt[x]])/2 - (Log[2]*Log[Sqrt[x]]^2)/2 +
		(Log[1 - Sqrt[x]]*Log[Sqrt[x]]^2)/2 - Log[Sqrt[x]]^3/6 + PolyLog[3, -((1 - Sqrt[x])/(2*Sqrt[x]))]
		),


	PolyLog[3, -(((1 + x_Symbol)/ (1 - x_Symbol)))]/; optPolyLog :>
		(
		Zeta2*Log[1 - x] +
		Log[1 - x]^3/6 -
		Zeta2*Log[1 + x] -
		(Log[1 - x]^2*Log[1 + x])/2 +
		(Log[1 - x]*Log[1 + x]^ 2)/2 -
		Log[1 + x]^3/6 +
		PolyLog[3, -((1 - x)/ (1 + x))]
		),

	PolyLog[3, (1 + x_Symbol)/(1 - x_Symbol)]/; optPolyLog :>
		+I/6*(
		12*I*Zeta2*Log[1 - x] -
		3*Pi*Log[1 - x]^2 - I*Log[1 - x]^3 -
		12*I*Zeta2*Log[1 + x] +
		6*Pi*Log[1 - x]*Log[1 + x] +
		3*I*Log[1 - x]^2*Log[1 + x] -
		3*Pi*Log[1 + x]^2 -
		3*I*Log[1 - x]*Log[1 + x]^2 +
		I*Log[1 + x]^3 -
		6*I*PolyLog[3, (1 - x)/(1 + x)]
		),


	PolyLog[3, -((1 - x_Symbol)/(1 + x_Symbol))]/; optPolyLog :>
		(
		-(Zeta2*Log[2]) + Log[2]^3/3 - (Log[2]^2*Log[1 - x])/2 + Zeta2*Log[1 + x] - (Log[2]^2*Log[1 + x])/2 + Log[2]*Log[1 - x]*Log[1 + x] -
		(Log[1 - x]*Log[1 + x]^2)/2 + Log[1 + x]^3/6 - PolyLog[3, (1 - x)/2] - PolyLog[3, (1 + x)/2] + Zeta[3]
		),

	PolyLog[3, -((1 - Sqrt[x_Symbol])/(1 + Sqrt[x_Symbol]))]/; optSqrt && optPolyLog :>
		(
		-(Zeta2*Log[2]) + Log[2]^3/3 - (Log[2]^2*Log[1 - Sqrt[x]])/2 + Zeta2*Log[1 + Sqrt[x]] - (Log[2]^2*Log[1 + Sqrt[x]])/2 +
		Log[2]*Log[1 - Sqrt[x]]*Log[1 + Sqrt[x]] - (Log[1 - Sqrt[x]]*Log[1 + Sqrt[x]]^2)/2 + Log[1 + Sqrt[x]]^3/6 - PolyLog[3, (1 - Sqrt[x])/2] -
		PolyLog[3, (1 + Sqrt[x])/2] + Zeta[3]
		),

		(*XY*)
		(*
		PolyLog[3, (1 - x_Symbol)/(1 + x_Symbol)] :>
		-I/6*(12*I*Zeta2*Log[1 - x] - 3*Pi*Log[1 - x]^2 - I*Log[1 - x]^3 -
				12*I*Zeta2*Log[1 + x] + 6*Pi*Log[1 - x]*Log[1 + x] +
				3*I*Log[1 - x]^2*Log[1 + x] - 3*Pi*Log[1 + x]^2 -
				3*I*Log[1 - x]*Log[1 + x]^2 + I*Log[1 + x]^3 +
				6*I*PolyLog[3, (1 + x)/(1 - x)])
		*)

	PolyLog[2, (1 + x_Symbol)/(2*x_Symbol)]/; optPolyLog :>
		(
		Pi^2/6 +
		I*Pi*Log[2] -
		Log[2]^2/2 +
		Log[2]*Log[1 - x] +
		I*Pi*Log[x] -
		Log[2]*Log[x] +
		Log[1 - x]*Log[x] -
		Log[x]^2/2 -
		I*Pi*Log[1 + x] -
		Log[1 - x]*Log[1 + x] +
		Log[1 + x]^2/2 +
		PolyLog[2, (1 - x)/(1 + x)]
		),

	PolyLog[3, 2/(1 - x_Symbol)]/; optPolyLog :>
		(
		Pi^2*Log[2] -
		3*I*Pi*Log[2]^2 +
		Log[2]^3 -
		Pi^2*Log[1 - x] +
		6*I*Pi*Log[2]*Log[1 - x] -
		3*I*Pi*Log[1 - x]^2 -
		3*Log[2]*Log[1 - x]^2 +
		2*Log[1 - x]^3 -
		3*Log[2]^2*Log[1 + x] +
		6*Log[2]*Log[1 - x]*Log[1 + x] -
		3*Log[1 - x]^2*Log[1 + x] -
		6*PolyLog[3, (1 + x)/2] -
		6*PolyLog[3, -((1 + x)/(1 - x))] +
		6*Zeta[3]
		)/6,

	(* some weird formula, a shorter version below ... *)

	PolyLog[3, x_Symbol/(1 + x_Symbol)]/; optPolyLog :>
		(
		3*Zeta2*Log[1 + x] -
		Log[-x]*Log[1 + x]^2 +
		Log[x]*Log[1 + x]^2 +
		Log[x]*PolyLog[2, -x] +
		Log[x]*PolyLog[2, x/(1 + x)] -
		PolyLog[3, -x] +
		PolyLog[3, (1 + x)^(-1)] -
		2*PolyLog[3, 1 + x] +
		Zeta[3]
		),

	PolyLog[3, Sqrt[x_Symbol]/(1 + Sqrt[x_Symbol])]/; optSqrt && optPolyLog :>
		(
		-(Zeta2*Log[1 + Sqrt[x]]) +
		Log[1 + Sqrt[x]]^3/3 -
		(Log[1 + Sqrt[x]]^2*Log[Sqrt[x]])/2 -
		PolyLog[3, (1 + Sqrt[x])^(-1)] -
		PolyLog[3, -Sqrt[x]] + Zeta[3]
		),


	PolyLog[3, 2 x_Symbol/(1 + x_Symbol)]/; optPolyLog :>
		(
		2*Zeta2*Log[2] - Log[2]^3/6 + (Log[2]^2*Log[x])/2 + (Log[2]*Log[x]^2)/2 + (Log[1 - x]*Log[x]^2)/2 + Log[x]^3/6 - Zeta2*Log[1 + x] + (Log[2]^2*Log[1 + x])/2 -
		Log[2]*Log[1 - x]*Log[1 + x] - Log[2]*Log[x]*Log[1 + x] - Log[1 - x]*Log[x]*Log[1 + x] - Log[x]^2*Log[1 + x] + (Log[2]*Log[1 + x]^2)/2 +
		Log[1 - x]*Log[1 + x]^2 + (3*Log[x]*Log[1 + x]^2)/2 - (5*Log[1 + x]^3)/6 + Log[x]*PolyLog[2, (1 - x)/(1 + x)] - Log[1 + x]*PolyLog[2, (1 - x)/(1 + x)] +
		Log[x]*PolyLog[2, (2*x)/(1 + x)] - Log[1 + x]*PolyLog[2, (2*x)/(1 + x)] + PolyLog[3, (1 - x)/2] - PolyLog[3, -(1 - x)/(2*x)] + PolyLog[3, -((1 - x)/(1 + x))] -
		PolyLog[3, (1 - x)/(1 + x)] + PolyLog[3, (1 + x)/2]
		),


	PolyLog[3, 2 Sqrt[x_Symbol]/(1 + Sqrt[x_Symbol])]/; optSqrt && optPolyLog :>
		(
		2*Zeta2*Log[2] - Log[2]^3/6 - 2*Zeta2*Log[1 + Sqrt[x]] + (Log[2]^2*Log[1 + Sqrt[x]])/2 - (Log[2]*Log[1 + Sqrt[x]]^2)/2 + Log[1 + Sqrt[x]]^3/6 +
		Zeta2*Log[Sqrt[x]] + (Log[2]^2*Log[Sqrt[x]])/2 - Log[2]*Log[1 - Sqrt[x]]*Log[Sqrt[x]] + Log[1 - Sqrt[x]]*Log[1 + Sqrt[x]]*Log[Sqrt[x]] -
		(Log[1 + Sqrt[x]]^2*Log[Sqrt[x]])/2 + (Log[2]*Log[Sqrt[x]]^2)/2 - (Log[1 - Sqrt[x]]*Log[Sqrt[x]]^2)/2 + Log[Sqrt[x]]^3/6 + PolyLog[3, (1 - Sqrt[x])/2] +
		PolyLog[3, -((1 - Sqrt[x])/(1 + Sqrt[x]))] - PolyLog[3, (1 - Sqrt[x])/(1 + Sqrt[x])] + PolyLog[3, (1 + Sqrt[x])/2] - PolyLog[3, -(1 - Sqrt[x])/(2*Sqrt[x])]
		),


	PolyLog[3, (1-Sqrt[z_Symbol])/(1 + Sqrt[z_Symbol])]/; optSqrt && optPolyLog :>
		(
		-(Zeta2*Log[2]) + Log[2]^3/3 + 2*Zeta2*Log[1 + Sqrt[z]] - Log[2]*Log[1 + Sqrt[z]]^2 + Log[1 + Sqrt[z]]^3/3 - (Log[2]^2*Log[1 - z])/2 +
		Log[2]*Log[1 + Sqrt[z]]*Log[1 - z] - (Log[1 + Sqrt[z]]^2*Log[1 - z])/2 - PolyLog[3, (1 - Sqrt[z])/2] + 2*PolyLog[3, 1 - Sqrt[z]] +
		2*PolyLog[3, (1 + Sqrt[z])^(-1)] - PolyLog[3, (1 + Sqrt[z])/2] - PolyLog[3, 1 - z]/2 - (3*Zeta[3])/4
		),

	PolyLog[3, x_Symbol/(1 + x_Symbol)]/; optPolyLog  :>
		(
		-Zeta2 Log[1 + x] - 1/2 Log[x] Log[1 + x]^2 + 1/3 Log[1 + x]^3 -
		PolyLog[3, -x] - PolyLog[3, 1/(1 + x)] + Zeta[3]
		),

	PolyLog[4, -x_/(1-x_)]/; optPolyLog  :>
		(
		-Log[1 - x]^4/24 -
		Nielsen[1, 3, x] +
		Nielsen[2, 2, x] -
		(Log[1 - x]^2*PolyLog[2, x])/2 +
		Log[1 - x]*(-Nielsen[1, 2, x] + PolyLog[3, x]) -
		PolyLog[4, x]
		),

	Log[1/x_Symbol]/; optLog :>
		-Log[x],

	Log[1/x_Symbol^2]/; optLog :>
		- 2 Log[x],

	Log[1/(x_Symbol+1)]/; optLog :>
		-Log[x+1],

	Log[n_Integer?Positive/(x_Symbol+1)]/; optLog :>
		Log[n]-Log[x+1],

	Log[Power[(x_Symbol+1),n_Integer]]/; n<0 && optLog :>
		- n Log[x+1],

	Log[Power[(x_Symbol^2+1),n_Integer]]/; n<0 && optLog :>
		- n Log[x^2+1],

	Log[x_Symbol/(x_Symbol+1)]/; optLog :>
		Log[x]-Log[x+1],

	Log[1/(1-x_Symbol)]/; optLog :>
		-Log[1-x],

	Log[n_Integer?Positive/(1-x_Symbol)]/; optLog :>
		Log[n]-Log[1-x],

	Log[Power[(1-x_Symbol),n_Integer]]/; n<0  && optLog :>
		- n Log[1-x],

	Log[Power[(1-x_Symbol^2),n_Integer]]/; n<0  && optLog :>
		- n Log[1-x^2],

	Log[(n1_Integer?Positive - x_Symbol)/(n2_Integer?Positive - x_Symbol)] /; n1 >= 1 && n2 >= 1  && optLog :>
		Log[n1 - x] - Log[n2 - x],

	Log[-1/x_Symbol]/; optLog :>
		-Log[x] + I Pi,

	Log[-1/(1-x_Symbol)]/; optLog :>
		-Log[1-x] + I Pi,

	Log[-x_Symbol]/; optLog :>
		Log[x] + I Pi,

	Log[n_Integer?Negative x_Symbol]/; optLog :>
		Log[-n] + Log[x] + I Pi,

	Log[-Sqrt[x_Symbol]]/; optSqrt :>
		1/2 Log[x] + I Pi,

	Log[(x_Symbol)^n_Integer?Positive]/; optLog :>
		n Log[x],

	Log[-x_Symbol^2]/; optLog :>
		2 Log[x] + I Pi,

	Log[x_Symbol-1]/; optLog :>
		Log[1-x] + I Pi,

	Log[(x_Symbol-1)/x_Symbol]/; optLog :>
		Log[1-x]-Log[x]+I Pi,

	Log[-((1-x_Symbol)/x_Symbol)]/; optLog :>
		Log[1-x]-Log[x]+I Pi,

	Log[-1-x_Symbol]/; optLog :>
		Log[1 + x] + I Pi,

	Log[1-x_Symbol^2]/; optLog :>
		Log[1-x] + Log[1+x],

	Log[(1-x_Symbol) x_Symbol]/; optLog :>
		Log[1-x] + Log[x],

	Log[(1-x_Symbol)/x_Symbol]/; optLog :>
		Log[1-x] - Log[x],

	Log[(1-x_Symbol)/(1+x_Symbol)]/; optLog :>
		Log[1-x] - Log[1+x],

	Log[(1+x_Symbol)/(1-x_Symbol)]/; optLog :>
		Log[1+x] - Log[1-x],

	Log[(x_Symbol + 1)/x_Symbol]/; optLog :>
		Log[x+1] - Log[x],

	Log[x_Symbol/(1-x_Symbol)]/; optLog :>
		Log[x] -Log[1-x],

	Log[x_Symbol/(x_Symbol-1)]/; optLog :>
		Log[x] -Log[1-x] + I Pi,

	Log[x_Symbol/(x_Symbol+1)]/; optLog :>
		Log[x] -Log[1+x],

	Log[-x_Symbol/(1-x_Symbol)]/; optLog :>
		Log[x] -Log[1-x] + I Pi,

	Log[(r_?NumberQ/; FreeQ[r, Complex]) x_]/; r>0  && optLog :>
		Log[r] + Log[x] ,

	Log[1-Sqrt[x_Symbol]]/; optSqrt && optLog :>
		Log[1-x] - Log[1+Sqrt[x]],

	Log[Sqrt[x_Symbol]]/; optSqrt && optLog :>
		1/2 Log[x],

	Log[-((Sqrt[1 - x_Symbol] - Sqrt[-x_Symbol]) Power[-x_Symbol,-1/2])]/; optSqrt && optLog :>
		Log[-(Sqrt[1 - x] - Sqrt[-x])] - Log[Sqrt[-x]],

	Log[Rational[-1, n_Integer?Positive] ((Sqrt[1 - x_Symbol] - Sqrt[-x_Symbol]) Power[-x_Symbol, -1/2])]/; optSqrt && optLog :>
		Log[-(Sqrt[1 - x] - Sqrt[-x])] - Log[n Sqrt[-x]],

	Log[(Sqrt[1 - x_Symbol] + Sqrt[-x_Symbol]) Power[-x_Symbol,-1/2]]/; optSqrt && optLog :>
		Log[(Sqrt[1 - x] + Sqrt[-x])] - Log[Sqrt[-x]],

	Log[(Sqrt[-x_Symbol] - Sqrt[1 - x_Symbol])]/; optSqrt && optLog :>
		-Log[(Sqrt[-x] + Sqrt[1 - x])] + I Pi,

	Log[(Sqrt[1 - x_Symbol] - Sqrt[-x_Symbol])]/; optSqrt && optLog :>
		-Log[Sqrt[1 - x] + Sqrt[-x]],

	Log[Sqrt[1 - x_Symbol] Power[-x_Symbol,-1/2]]/; optSqrt && optLog :>
		Log[Sqrt[1 - x]] - Log[Sqrt[-x]],

	Log[Sqrt[1 - x_Symbol]/(Sqrt[1 - x_Symbol] + Sqrt[-x_Symbol])]/; optSqrt && optLog :>
		1/2 Log[1-x]- Log[Sqrt[1 - x] + Sqrt[-x]],

	Log[Sqrt[-x_Symbol]/(Sqrt[1 - x_Symbol] + Sqrt[-x_Symbol])]/; optSqrt && optLog :>
		Log[Sqrt[-x]] - Log[Sqrt[1 - x] + Sqrt[-x]],

	Log[((Sqrt[1 - x_Symbol] - Sqrt[-x_Symbol]) Power[1-x_Symbol,-1/2])]/; optSqrt && optLog :>
		Log[Sqrt[1-x]-Sqrt[-x]]-Log[Sqrt[1-x]],


	Log[((Sqrt[1 - x_Symbol] + Sqrt[-x_Symbol]) Power[1-x_Symbol,-1/2])]/; optSqrt && optLog :>
		Log[Sqrt[1-x]+Sqrt[-x]]-Log[Sqrt[1-x]],

	Log[Sqrt[- x_Symbol] Power[1-x_Symbol,-1/2]]/; optSqrt && optLog :>
		Log[Sqrt[- x]] - Log[Sqrt[1-x]],


	Log[Sqrt[-x_Symbol]]/; optSqrt && optLog :>
		(I/2)*Pi + Log[x]/2,

	Log[-Sqrt[-x_Symbol]]/; optSqrt && optLog :>
		-I Pi/2 + Log[x]/2,

	Log[Sqrt[1 - x_Symbol]]/; optSqrt && optLog :>
		1/2 Log[1 - x],

	Log[-Sqrt[1 - x_Symbol] Power[-x_Symbol,-1/2]]/; optSqrt && optLog :>
		Log[-Sqrt[1 - x]] - Log[Sqrt[-x]],

	Log[-Sqrt[1 - x_Symbol]]/; optSqrt && optLog :>
		Log[Sqrt[1 - x]] + I Pi,

	Log[Sqrt[x_Symbol]/(1 + Sqrt[x_Symbol])]/; optSqrt && optLog :>
		Log[Sqrt[x]] - Log[1 + Sqrt[x]],

	Log[(1 - Sqrt[x_Symbol])/(1 + Sqrt[x_Symbol])]/; optSqrt && optLog :>
		Log[1-Sqrt[x]] - Log[1+Sqrt[x]],

	Log[(1 + Sqrt[x_Symbol])/(1 - Sqrt[x_Symbol])]/; optSqrt && optLog :>
		Log[1+Sqrt[x]] - Log[1-Sqrt[x]],

	Log[1/(1 - Sqrt[x_Symbol])]/; optSqrt && optLog :>
		- Log[1-Sqrt[x]],

	Log[1/(1 + Sqrt[x_Symbol])]/; optSqrt && optLog :>
		- Log[1+Sqrt[x]],

	Log[-(Sqrt[x_Symbol]/(1-Sqrt[x_Symbol]))]/; optSqrt && optLog :>
		Log[-Sqrt[x]]-Log[1-Sqrt[x]],

	Log[(1 - 2 Sqrt[x_Symbol])/(1 - Sqrt[x_Symbol])]/; optSqrt && optLog :>
		Log[1 - 2 Sqrt[x]] - Log[1 - Sqrt[x]],


	Pi^2 :>
		6 Zeta2,

	Pi^3 :>
		Pi 6 Zeta2,

	ArcSinh[z_Symbol]/; optTrig :>
		Log[z + Sqrt[z^2 + 1]],

	ArcCosh[z_Symbol]/; optTrig :>
		Log[z + Sqrt[z^2 - 1]],

	ArcTanh[z_Symbol]/; optTrig :>
		1/2 Log[(z+1)/(z-1)],

	Log[1-Complex[0,1]]/; optLog :>
		(-I/4)*Pi + Log[2]/2,

	Log[1+Complex[0,1]]/; optLog :>
		(I/4)*Pi + Log[2]/2,

	PolyLog[3,Complex[Rational[1,2],Rational[1,2]]]/; optPolyLog :>
		((21*I)/64)*Pi*Zeta2 + (Zeta2*Log[2])/32 + ((3*I)/32)*Pi*Log[2]^2 + Log[2]^3/48 - PolyLog[3, 1 + I] + (35*Zeta[3])/32,

	PolyLog[3,Complex[1,-1]]/; optPolyLog :>
		(3*Zeta2*Log[2])/8 - PolyLog[3, 1 + I] + (35*Zeta[3])/32,

	PolyLog[3,Complex[0,1]]/; optPolyLog :>
		((3*I)/16)*Pi*Zeta2 - (3*Zeta[3])/32

};

FCPrint[1,"SimplifyPolyLog.m loaded."];
End[]
