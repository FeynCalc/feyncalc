(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ConvoluteTable *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 10 March '98 at 15:35 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  Convolute convolutes *)

(* ------------------------------------------------------------------------ *)

ConvoluteTable::usage=
"ConvoluteTable[f, g, x] yields the convolution of f and g. ConvoluteTable is
called by Convolute.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ConvoluteTable`Private`"]

cT::usage="";
cTcT::usage="";

ConvoluteTable[f_, g_, opt___Rule] :=
ConvoluteTable[f, g, FCGV["x"], opt];

ConvoluteTable[if_, ig_, x_,___Rule] :=
cTab[if,ig,x];

(* symmetric tablelookup *)

cTab[a_,b_,x_] := MemSet[cTab[a,b,x],
	If[Head[cTcT=(cT[a,b,x]/.tablenoeps)] =!= cT, cTcT,
		If[Head[cTcT=(cT[b,a,x] /. tablenoeps)] =!= cT, cTcT, False[a,b]]
		]                   ];

tablenoeps = Dispatch[{
(*  1 *)
			cT[PlusDistribution[1/(1-x_)], PlusDistribution[1/(1-x_)], x_
				]                                    :>
-(Zeta2*DeltaFunction[1 - x]) + Log[x]/(-1 + x) +
	2*PlusDistribution[Log[1 - x]/(1 - x)]
,
(*  2 *)
			cT[1, PlusDistribution[1/(1-x_)], x_]      :> Log[1 - x] - Log[x]
,
(*  3 *)
			cT[x_, PlusDistribution[1/(1-x_)], x_]     :>
1 - x + x Log[1 - x] - x Log[x]
,
(*  4 *)
			cT[1, 1, x_]                               :> -Log[x]
,
(*  5 *)
			cT[x_, x_, x_]                             :> -x Log[x]
,
(*  6 *)
			cT[1, x_, x_]                              :> 1 - x
,
(*  7 *)
			cT[PlusDistribution[1/(1-x_)],
					PlusDistribution[Log[1-x_]/(1-x_)], x_] :>
-Zeta2 PlusDistribution[1/(1 - x)] + (Log[1 - x]*Log[x])/(-1 + x) +
(3*PlusDistribution[Log[1 - x]^2/(1 - x)])/2 + DeltaFunction[1 - x]*Zeta[3]
,
(*  8 *)
			cT[1, Log[1 - x_], x_]                     :>
-(Log[1 - x]*Log[x]) - PolyLog[2, 1 - x]
,
(*  9 *)
			cT[x_, x_ Log[1 - x_], x_]                 :>
-(x*Log[1 - x]*Log[x]) - x*PolyLog[2, 1 - x]
,
(* 10 *)
			cT[PlusDistribution[1/(1-x_)], Log[1-x_], x_] :>
-Zeta2 + Log[1 - x]^2 - Log[1 - x]*Log[x]
,
(* 11 *)
			cT[1, PlusDistribution[Log[1-x_]/(1-x_)], x_] :>
Log[1 - x]^2/2 - Log[1 - x]*Log[x] - PolyLog[2, 1 - x]
,
(* 12 *)
			cT[PlusDistribution[1/(1-x_)], x_ Log[1-x_], x_] :>
-1 + x - x*Zeta2 + (1 - x)*Log[1 - x] + x*Log[1 - x]^2 - x*Log[1 - x]*Log[x]
,
(* 13 *)
			cT[x_, PlusDistribution[Log[1-x_]/(1-x_)], x_] :>
(1 - x)*Log[1 - x] + (x*Log[1 - x]^2)/2 + x*Log[x] - x*Log[1 - x]*Log[x] -
	x*PolyLog[2, 1 - x]
,
(* 14 *)
			cT[1, x_ Log[1-x_], x_] :> (1 - x) Log[1 - x] -1 + x
,
(* 15 *)
			cT[x_, Log[1-x_], x_] :>  (1-x) Log[1-x] + x Log[x]
,
(* 16 *)
			cT[PlusDistribution[1/(1-x_)], Log[x_]/(1-x_), x_] :>
-((Log[1 - x]*Log[x])/(-1 + x)) + Log[x]^2/(2*(-1 + x))
,
(* 17 *)
			cT[1, Log[x_], x_] :> -1/2 Log[x]^2
,
(* 18 *)
			cT[x_, x_ Log[x_], x_] :> -x/2 Log[x]^2
,
(* 19 *)
			cT[PlusDistribution[1/(1-x_)], Log[x_], x_] :>
Log[1 - x]*Log[x] - Log[x]^2/2 + PolyLog[2, 1 - x]
,
(* 20 *)
			cT[1, Log[x_]/(1-x_), x_] :> -Log[x]^2/2 - PolyLog[2, 1 - x]
,
(* 20b *)
			cT[1,Log[x_] PlusDistribution[1/(1-x_)],x_]  :>
																-Log[x]^2/2 - PolyLog[2, 1 - x]
,
(* 21 *)
			cT[PlusDistribution[1/(1-x_)], x_ Log[x_], x_] :>
-1 + x - x*Log[x] + x*Log[1 - x]*Log[x] - (x*Log[x]^2)/2 +
	x*PolyLog[2, 1 - x]
(*
,
(* 22 *)
			cT[Log[x_]/(1-x_)], x_ , x_] :>
*)

,
(* 23 *)
			cT[1, x_ Log[x_], x_] :> -1 + x - x Log[x]
,
(* 24 *)
			cT[Log[x_], x_, x_] :> 1 - x + Log[x]
,
(* 25 *)
			cT[1/x_, Log[x_], x_] :> 1 - x^(-1) - Log[x]
,
(* 26 *)
			cT[x_, Log[1-x_]/x_, x_] :>
-1/2 + x/2 + Log[1 - x]/(2*x) - (x*Log[1 - x])/2 + (x*Log[x])/2
,
(* 27 *)
			cT[1/x_, PlusDistribution[1/(1-x_)], x_]      :> Log[1-x]/x
,
(* 29 *)
			cT[1, Log[1-x_]/x_, x_] :> (-1 + x^(-1))*Log[1 - x] + Log[x]
,
(* 30 *)
			cT[1, Log[x_]/x_, x_] :> -1 + x^(-1) + Log[x]/x
,
(* 31 *)
			cT[1, Log[1-x_]/(1-x_), x_] :>
				Log[1 - x]^2/2 - Log[1 - x]*Log[x] - PolyLog[2, 1 - x]
,
(* 32 *)
			cT[1/x_, x_, x_] :> 1/(2 x) - x/2
,
(* 33 *)
			cT[x_, Log[1-x_]^2/x_, x_] :>
	(-1 + x)*Log[1 - x] + (1/(2*x) - x/2)*Log[1 - x]^2 - x*Log[x] +
	x*Log[1 - x]*Log[x] + x*PolyLog[2, 1 - x]
,
(* 34 *)
			cT[Log[x_]/x_, x_, x_] :> 1/(4*x) - x/4 + Log[x]/(2*x)
,
(* 35 *)
			cT[(Log[1 - x_]*Log[x_])/x_, x_, x_] :>
-3/4 + (3*x)/4 + (x*Zeta2)/2 + (1/(4*x) - x/4)*Log[1 - x] +
	(-1/2 + x/4)*Log[x] + (1/(2*x) - x/2)*Log[1 - x]*Log[x] +
(x*Log[x]^2)/4 - (x*PolyLog[2, x])/2
,
(* 36 *)
			cT[Log[x_]^2/x_, x_, x_] :>
1/(4*x) - x/4 + Log[x]/(2*x) + Log[x]^2/(2*x)
,
(* 37 *)
			cT[1, x_^2, x_] :> 1/2 - x^2/2
,
(* 38 *)
			cT[x_^2, Log[x_], x_] :> 1/4 - x^2/4 + Log[x]/2
,
(* 39 *)
			cT[x_^2, Log[1 - x_], x_] :>
-x/2 + x^2/2 + (1/2 - x^2/2)*Log[1 - x] + (x^2*Log[x])/2
,
(* 40 *)
			cT[x_^2, Log[1 - x_]^2, x_] :>
(-x + x^2)*Log[1 - x] + (1/2 - x^2/2)*Log[1 - x]^2 - x^2*Log[x] +
	x^2*Log[1 - x]*Log[x] + x^2*PolyLog[2, 1 - x]
,
(* 41 *)
			cT[x_^2, Log[x_]^2, x_] :>
1/4 - x^2/4 + Log[x]/2 + Log[x]^2/2
,
(* 42 *)
			cT[x_^2,Log[1 - x_]*Log[x_], x_] :>
(3*(-1 + x)*x)/4 + (-1/4 + 1/(4*x^2))*x^2*Log[1 - x] +
	((-2 + x)*x*Log[x])/4 + (Log[1 - x]*Log[x])/2 + (x^2*Log[x]^2)/4 +
	(x^2*PolyLog[2, 1 - x])/2
,
(* 43 *)
			cT[x_^2, Log[x_]/x_ ,x_] :>
1/(9*x) - x^2/9 + Log[x]/(3*x)
,
(* 44 *)
			cT[x_^2, Log[1-x_]/x_  ,x_] :>
-1/6 - x/3 + x^2/2 + (1/(3*x) - x^2/3)*Log[1 - x] + (x^2*Log[x])/3
,
(* 45 *)
			cT[x_^2, Log[1-x_]^2/x_ ,x_] :>
x/3 - x^2/3 + (-1/3 - (2*x)/3 + x^2)*Log[1 - x] +
	(1/(3*x) - x^2/3)*Log[1 - x]^2 - x^2*Log[x] +
	(2*x^2*Log[1 - x]*Log[x])/3 + (2*x^2*PolyLog[2, 1 - x])/3
,
(* 46 *)
			cT[x_^2, Log[1-x_] Log[x_]/x_ ,x_] :>
-5/36 - (4*x)/9 + (7*x^2)/12 + (1/(9*x) - x^2/9)*Log[1 - x] +
	(-1/6 - x/3 + x^2/9)*Log[x] + (Log[1 - x]*Log[x])/(3*x) +
	(x^2*Log[x]^2)/6 + (x^2*PolyLog[2, 1 - x])/3
,
(* 47 *)
			cT[1/x_, x_^2, x_] :> 1/(3*x) - x^2/3
,
(* 48 *)
			cT[x_^2, Log[x_]^2/x_, x_] :>
2/(27*x) - (2*x^2)/27 + (2*Log[x])/(9*x) + Log[x]^2/(3*x)
,
(* 49 *)
			cT[1, 1/x_, x_] :> -1 + x^(-1)
,
(* 50 *)
			cT[Log[1 - x_], x_^(-1), x_] :> 1 - x^(-1) + (-1 + x^(-1))*Log[1 - x]
,
(* 53 *)
			cT[Log[1-x_], Log[1-x_]/x_, x_] :>
	-(((1 - x)*Zeta2)/x) - ((1 - x)*Log[1 - x])/x + ((1 - x)*Log[1 - x]^2)/x -
	Log[x] + Log[1 - x]*Log[x] + PolyLog[2, 1 - x]/x
,
(* 54 *)
			cT[Log[x_]^2/x_, Log[x_]^2  , x_] :>
-24 + 24/x + (12 + 12/x)*Log[x] + (-2 + 2/x)*Log[x]^2
,
(* 55 *)
			cT[Log[x_]/x_, Log[x_]  , x_] :> 2 - 2/x + (-1 - x^(-1))*Log[x]
,
(* 56 *)
			cT[Log[x_]/x_, Log[1-x_]  , x_] :>
2 - 2/x + (-1 + x^(-1))*Log[1 - x] - Log[x]/x +
	(Log[1 - x]*Log[x])/x + PolyLog[2, 1 - x]/x
,
(* 57 *)
			cT[Log[1-x_]/x_,Log[x_]  , x_] :>
(1 - x^(-1))*Log[1 - x] - Log[x] - Log[1 - x]*Log[x] + Log[x]^2/2 -
	PolyLog[2, 1 - x]
,
(* 58 *)
			cT[Log[1-x_]/x_, Log[1-x_]  , x_] :>
Zeta2 - Zeta2/x + (1 - x^(-1))*Log[1 - x] +
	(-1 + x^(-1))*Log[1 - x]^2 - Log[x] + Log[1 - x]*Log[x] +
	PolyLog[2, 1 - x]/x
,
(* 59 *)
			cT[Log[1-x_] Log[x_]/x_, 1 , x_] :>
((1 - x)*Log[1 - x])/x + Log[x] + Log[1 - x]*Log[x] -
	((-1 + x)*Log[1 - x]*Log[x])/x + Log[x]^2/2 + PolyLog[2, 1 - x]
,
(* 60 *)
			cT[Log[1 - x_]^2/x_, 1  , x_] :>
(-1 + x^(-1))*Log[1 - x]^2 + 2*Log[1 - x]*Log[x] +
	2*PolyLog[2, 1 - x]
,
(* 61 *)
			cT[Log[x_]^2/x_, 1  , x_] :>
-2 + 2/x + (2*Log[x])/x + Log[x]^2/x
,
(* 62 *)
			cT[x_^(-1), Log[1 - x_]^2 , x_] :>
-2 + 2/x + (2 - 2/x)*Log[1 - x] + (-1 + x^(-1))*Log[1 - x]^2
,
(* 63 *)
			cT[x_^(-1), Log[1 - x_]*Log[x_], x_] :>
-2 + 2/x + (1 - x^(-1))*Log[1 - x] + Log[x] - Log[1 - x]*Log[x] -
	PolyLog[2, 1 - x]/x
,
(* 64 *)
			cT[x_^(-1), Log[x_]^2  , x_] :>
-2 + 2/x + 2*Log[x] - Log[x]^2
,
(* 65 *)
			cT[x_^(-1), x_ Log[x_]  , x_] :> -1/(4*x) + x/4 - (x*Log[x])/2
,
(* 66 *)
			cT[x_^(-1), x_^2 Log[x_]   , x_] :> -1/(9*x) + x^2/9 - (x^2*Log[x])/3
,
(* 67 *)
			cT[x_^(-1), x_ Log[1-x_]  , x_] :>
1/2 - 3/(4*x) + x/4 + (1/(2*x) - x/2)*Log[1 - x]
,
(* 68 *)
			cT[x_^(-1), x_^2 Log[1-x_]  , x_] :>
1/3 - 11/(18*x) + x/6 + x^2/9 + (1/(3*x) - x^2/3)*Log[1 - x]
,
(* 69 *)
			cT[1, PlusDistribution[Log[1-x_]^2/(1-x_)]  , x_] :>
Log[1 - x]^3/3 - Log[1 - x]^2*Log[x] -
	2*Log[1 - x]*PolyLog[2, 1 - x] + 2*PolyLog[3, 1 - x]
,
(* 70 *)
			cT[1,Log[1 - x_]*Log[x_], x_] :>
Zeta2*Log[x] - Log[1 - x]*Log[x]^2 - Log[x]*PolyLog[2, 1 - x] -
	PolyLog[3, x] + Zeta[3]
,
(* 71 *)
			cT[1, Log[x_] Log[1-x_]/(1-x_) , x_] :>
-(Log[1 - x]*PolyLog[2, 1 - x]) + Log[x]*PolyLog[2, x] +
	PolyLog[3, 1 - x] - PolyLog[3, x] + Zeta[3]
,
(* 72 *)
			cT[1, Log[x_]^2 , x_] :> -Log[x]^3/3
,
(* 73 *)
			cT[1, Log[x_]^2/(1-x_), x_] :>
2*Zeta2*Log[x] - Log[1 - x]*Log[x]^2 - Log[x]^3/3 -
	2*Log[x]*PolyLog[2, 1 - x] - 2*PolyLog[3, x] + 2*Zeta[3]
,
(* HAMBERG *)
(* 74 *)
			cT[Log[1-x_], Log[1-x_]  , x_] :>
2 PolyLog[3,1-x] + Log[x] PolyLog[2,1-x] +
Log[1-x] Log[x]^2 - Zeta2 Log[x] + 2 PolyLog[3,x] -
2 Log[1-x] PolyLog[2,1-x] - Log[1-x]^2 Log[x] - 2 Zeta[3]
,
(* 75 *)
			cT[Log[1-x_], PlusDistribution[Log[1-x_]/(1-x_)], x_] :>
PolyLog[3,1-x] + 2 PolyLog[3,x] - Zeta[3] +
(Log[x] - Log[1-x]) PolyLog[2,1-x] - Zeta2 (Log[1-x] + Log[x]) +
Log[x]^2 Log[1-x] - Log[1-x]^2 Log[x] + 1/2 Log[1-x]^3
,
(* 76 *)
			cT[Log[x_], Log[1 - x_]  , x_] :>
-(Zeta2*Log[x]) + PolyLog[3, x] - Zeta[3]
,
(* 77 *)
			cT[Log[x_]/(1 - x_), Log[1 - x_]  , x_] :>
-(Zeta2*Log[1 - x]) + Zeta2*Log[x] + Log[1 - x]^2*Log[x] -
	(Log[1 - x]*Log[x]^2)/2 + (Log[-x^(-1)]*Log[x]^2)/2 + Log[x]^3/3 +
	Log[1 - x]*PolyLog[2, x] - Log[x]*PolyLog[2, x] + PolyLog[3, 1 - x] +
	PolyLog[3, x^(-1)] + PolyLog[3, x] - 2*Zeta[3]
,
(* 78 *)
			cT[1, Log[1 - x_]^2 , x_] :>
-(Log[1 - x]^2*Log[x]) - 2*Log[1 - x]*PolyLog[2, 1 - x] +
	2*PolyLog[3, 1 - x]
,
(* 79 *)
			cT[x_^2, x_  , x_] :> x (1 - x)
,
(* 80 *)
			cT[x_^2, x_*Log[1 - x_]  , x_] :> x (1-x)*Log[1 - x] + x^2*Log[x]
,
(* 81 *)
			cT[x_^2, x_*Log[x_]  , x_] :>x - x^2 + x*Log[x]
,
(* 82 *)
			cT[x_^2*Log[1 - x_],x_  , x_] :> -x + x^2 + (x - x^2)*Log[1 - x]
,
(* 83 *)
			cT[x_^2*Log[x_], x_  , x_] :> -x + x^2 - x^2*Log[x]
,
(* 84 *)
			cT[1, x_ Log[1-x_]^2  , x_] :>
2 - 2*x + (-2 + 2*x)*Log[1 - x] + (1 - x)*Log[1 - x]^2
,
(* 85 *)
			cT[1, Log[1 - (x_)]*Log[x_]*(x_), x_] :>
	2 - 2*x + (-1 + x)*Log[1 - x] + x*Log[x] - x*Log[1 - x]*Log[x] -
	PolyLog[2, 1 - x]
,
(* 86 *)
			cT[1, Log[x_]^2*(x_), x_] :> 2 - 2*x + 2*x*Log[x] - x*Log[x]^2
,
(* 87 *)
			cT[x_, Log[1 - (x_)]^2, x_] :>
	(1 - x)*Log[1 - x]^2 + 2*x*Log[1 - x]*Log[x] + 2*x*PolyLog[2, 1 - x]
,
(* 88 *)
cT[x_, Log[1 - (x_)]^2*(x_), x_] :>
	-(x*Log[1 - x]^2*Log[x]) - 2*x*Log[1 - x]*PolyLog[2, 1 - x] +
	2*x*PolyLog[3, 1 - x]
,
(* 89 *)
cT[x_, Log[1 - (x_)]*Log[x_], x_] :>
	(1 - x)*Log[1 - x] + x*Log[x] + Log[1 - x]*Log[x] + (x*Log[x]^2)/2 +
	x*PolyLog[2, 1 - x]
,
(* 90 *)
cT[x_, Log[1 - (x_)]*Log[x_]*(x_), x_] :>
	x*Zeta2*Log[x] - x*Log[1 - x]*Log[x]^2 - x*Log[x]*PolyLog[2, 1 - x] -
	x*PolyLog[3, x] + x*Zeta[3]
,
(* 91 *)
cT[x_, Log[x_]^2, x_] :> 2 - 2*x + 2*Log[x] + Log[x]^2
,
(* 92 *)
cT[x_, Log[x_]^2*(x_), x_] :> -(x*Log[x]^3)/3
,
(* 93 *)
cT[Log[1 - (x_)], Log[1 - (x_)]*(x_), x_] :>
	-Zeta2 + x*Zeta2 + (-1 + x)*Log[1 - x] + (1 - x)*Log[1 - x]^2 - x*Log[x] +
	x*Log[1 - x]*Log[x] + PolyLog[2, 1 - x]
,
(* 94 *)
cT[Log[1 - (x_)], Log[x_]*(x_), x_] :>
	(-1 + x)*Log[1 - x] - x*Log[x] - x*Log[1 - x]*Log[x] + (x*Log[x]^2)/2 -
	x*PolyLog[2, 1 - x]
,
(* 95 *)
cT[Log[1 - (x_)], Log[1 - (x_)]*(x_), x_] :>
	-Zeta2 + x*Zeta2 + (-1 + x)*Log[1 - x] + (1 - x)*Log[1 - x]^2 - x*Log[x] +
	x*Log[1 - x]*Log[x] + PolyLog[2, 1 - x]
,
(* 96 *)
cT[Log[x_], x_ Log[1 - x_], x_] :> -2 + 2 x + (1 - x) Log[1 - x] -
	Log[x] + Log[1 - x] Log[x] +   PolyLog[2, 1 - x]
,
(* 98 *)
cT[Log[1 - (x_)]^2, x_, x_] :>
	(1 - x)*Log[1 - x]^2 + 2*x*Log[1 - x]*Log[x] + 2*x*PolyLog[2, 1 - x]
,
(* 99 *)
cT[Log[1 - (x_)]^2*(x_), 1, x_] :>
	2 - 2*x + (-2 + 2*x)*Log[1 - x] + (1 - x)*Log[1 - x]^2
,
(* 100 *)
cT[Log[1 - (x_)]^2*(x_), x_, x_] :>
	-(x*Log[1 - x]^2*Log[x]) - 2*x*Log[1 - x]*PolyLog[2, 1 - x] +
	2*x*PolyLog[3, 1 - x]
,
(* 101 *)
cT[Log[x_], Log[1 - (x_)]*(x_), x_] :>
	-2 + 2*x + (1 - x)*Log[1 - x] - Log[x] + Log[1 - x]*Log[x] +
	PolyLog[2, 1 - x]
,
(* 102*)
cT[Log[x_], Log[x_], x_] :> -Log[x]^3/6
,
(* 103*)
cT[Log[x_], Log[x_]*(x_), x_] :> -2 + 2*x + (-1 - x)*Log[x]
,
(* 104*)
cT[Log[x_]*(x_), Log[1 - (x_)], x_] :>
	(-1 + x)*Log[1 - x] - x*Log[x] - x*Log[1 - x]*Log[x] + (x*Log[x]^2)/2 -
	x*PolyLog[2, 1 - x]
,
(* 105*)
cT[Log[x_]*(x_), Log[1 - (x_)]*(x_), x_] :>
	-(x*Zeta2*Log[x]) + x*PolyLog[3, x] - x*Zeta[3]
,
(* 106*)
cT[Log[x_]*(x_), Log[x_], x_] :> -2 + 2*x + (-1 - x)*Log[x]
,
(* 107*)
cT[Log[x_]*(x_), Log[x_]*(x_), x_] :> -(x*Log[x]^3)/6
,
(* 108*)
cT[Log[1 - (x_)]*Log[x_], x_, x_] :>
(1 - x)*Log[1 - x] + x*Log[x] + Log[1 - x]*Log[x] +
	(x*Log[x]^2)/2 + x*PolyLog[2, 1 - x]
,
(* 109*)
cT[Log[1 - (x_)]*Log[x_]*(x_), 1, x_]:>
2 - 2*x + (-1 + x)*Log[1 - x] + x*Log[x] - x*Log[1 - x]*Log[x] -
	PolyLog[2, 1 - x]
,
(* 110*)
cT[Log[1 - (x_)]*Log[x_]*(x_), x_, x_] :>
	x*Zeta2*Log[x] - x*Log[1 - x]*Log[x]^2 - x*Log[x]*PolyLog[2, 1 - x] -
	x*PolyLog[3, x] + x*Zeta[3]
,
(* 111*)
cT[Log[x_]^2, x_, x_] :> 2 - 2*x + 2*Log[x] + Log[x]^2
,
(* 112*)
cT[Log[x_]^2*(x_), 1, x_] :> 2 - 2*x + 2*x*Log[x] - x*Log[x]^2
,
(* 113*)
cT[Log[x_]^2*(x_), x_, x_] :> -(x*Log[x]^3)/3
,
(* 115 *)
cT[Log[x_], Log[x_]/(1 - (x_)), x_] :>
	-2*Zeta2*Log[x] + Log[1 - x]*Log[x]^2 - Log[x]^3/6 +
	Log[x]*PolyLog[2, 1 - x] + 2*PolyLog[3, x] - 2*Zeta[3]
,
(* 115 *)
cT[(Log[1 - (x_)]*Log[x_])/(1 - (x_)), 1, x_] :>
Zeta2*Log[x] - Log[1 - x]*Log[x]^2 - Log[1 - x]*PolyLog[2, 1 - x] -
	Log[x]*PolyLog[2, 1 - x] + PolyLog[3, 1 - x] - PolyLog[3, x] + Zeta[3]
,
(* 116 *)
cT[1, Log[x_]^2/(1-x_), x_] :>
2*Zeta2*Log[x] - Log[1 - x]*Log[x]^2 - Log[x]^3/3 -
	2*Log[x]*PolyLog[2, 1 - x] - 2*PolyLog[3, x] + 2*Zeta[3]
,
(* 117 *)
cT[Log[x_]^2, PlusDistribution[(1 - x_)^(-1)], x_] :>
2*Zeta2*Log[x] - Log[1 - x]*Log[x]^2 - Log[x]^3/3 + Log[x]^2*Log[1 - x] -
	2*PolyLog[3, x] + 2*Zeta[3]
,
(* 118 *)
cT[ x_ Log[1-x_], x_ Log[1-x_], x_ ] :>
x*Zeta2*Log[x] - x*Log[1 - x]^2*Log[x] - x*Log[1 - x]*Log[x]^2 -
	2*x*Log[1 - x]*PolyLog[2, 1 - x] - x*Log[x]*PolyLog[2, 1 - x] -
	2*x*Log[x]*PolyLog[2, x] + 2*x*PolyLog[3, 1 - x] + 2*x*PolyLog[3, x] -
	2*x*Zeta[3]
,
(* 119 *)
cT[Log[x_]/(1 - x_), x_^(-1), x_] :> -(PolyLog[2, 1 - x]/x)
,
(* 120 *)
cT[ 1/x_, 1/x_, x_ ] :>  -Log[x]/x
,

(* 121 *)
cT[ 1/x_, Log[1-x_]/x_, x_ ] :>
-((Log[1 - x]*Log[x])/x) - PolyLog[2, 1 - x]/x
,

(* 122 *)
cT[ 1/x_, Log[x_]/x_, x_] :> -Log[x]^2/(2*x)
,

(* 123 *)
cT[Log[1-x_]/x_, PlusDistribution[1/(1-x_)], x_] :>
Log[1-x]^2/x - Zeta2/x + PolyLog[2,1-x]/x
,
(* 124 *)
cT[Log[x_]/x_, PlusDistribution[1/(1-x_)], x_] :>
Log[x] Log[1-x]/x + PolyLog[2,1-x]/x
,
(* 125 *)
cT[Log[x_ (1-x_)]/x_, PlusDistribution[1/(1-x_)], x_] :>
-Zeta2 + Log[1 - x]^2 - Log[x]^2/2 + PolyLog[2, 1 - x]
,
(* 126 *)
cT[Log[x_]/(1 - x_), x_, x_] :>
1 - x + Log[x] - (x*Log[x]^2)/2 - x*PolyLog[2, 1 - x]
,
(* 127 *)
cT[(Log[1 - (x_)]*Log[x_])/(1 - (x_)), (x_)^(-1), x_] :>
-((Log[1 - x]*PolyLog[2, 1 - x])/x) + PolyLog[3, 1 - x]/x
,
(* 128 *)
cT[Log[x_]^2/(1 - x_), x_^(-1), x_] :>
(Log[1 - x]*Log[x]^2)/x + (2*Log[x]*PolyLog[2, x])/x -
	(2*PolyLog[3, x])/x + (2*Zeta[3])/x
,
(* 129 *)
cT[ PlusDistribution[Log[1 - x_]/(1 - x_)], x_^(-1), x_] :>
Log[1 - x]^2/(2*x)
,
(* 130 *)
cT[PlusDistribution[Log[1 - x_]^2/(1 - x_)], x_^(-1), x_] :>
Log[1 - x]^3/(3*x)
,
(* 131 *)
cT[Log[1 - x_]^2/x_, x_^(-1), x_] :>
-((Log[1 - x]^2*Log[x])/x) - (2*Log[1 - x]*PolyLog[2, 1 - x])/x +
	(2*PolyLog[3, 1 - x])/x
,
(* 132 *)
cT[x_*Log[1 - x_]^2, x_^(-1), x_] :>
-3/2 + 7/(4*x) - x/4 + Log[1 - x] - (3*Log[1 - x])/(2*x) +
	(x*Log[1 - x])/2 + Log[1 - x]^2/(2*x) - (x*Log[1 - x]^2)/2
,
(* 133 *)
cT[x_*Log[1 - x_]^2, x_^2, x_] :>
(1 - x)*x*Log[1 - x]^2 + 2*x^2*Log[1 - x]*Log[x] +
	2*x^2*PolyLog[2, 1 - x]
,
(* 134 *)
cT[(Log[1 - x_]*Log[x_])/(1 - x_), x_, x_] :>
Log[1 - x] - x*Log[1 - x] + x*Log[x] + Log[1 - x]*Log[x] +
	(x*Log[x]^2)/2 + x*PolyLog[2, 1 - x] - x*Log[1 - x]*PolyLog[2, 1 - x] +
	x*Log[x]*PolyLog[2, x] + x*PolyLog[3, 1 - x] -
x*PolyLog[3, x] + x*Zeta[3]
,
(* 134 *)
cT[(Log[1 - x_]*Log[x_])/x_, x_^(-1), x_] :>
(Zeta2*Log[x])/x - (Log[1 - x]*Log[x]^2)/x -
	(Log[x]*PolyLog[2, 1 - x])/x - PolyLog[3, x]/x + Zeta[3]/x
,
(* 135 *)
cT[x_*Log[1 - x_]*Log[x_], x_^(-1), x_] :>
-3/4 + x^(-1) - x/4 - Log[1 - x]/(4*x) + (x*Log[1 - x])/4 +
	Log[x]/2 + (x*Log[x])/4 - (x*Log[1 - x]*Log[x])/2 - PolyLog[2, 1 - x]/(2*x)
,
(* 136 *)
cT[x_*Log[1 - x_]*Log[x_], x_^2, x_] :>
x*Log[1 - x] - x^2*Log[1 - x] + x^2*Log[x] + x*Log[1 - x]*Log[x] +
	(x^2*Log[x]^2)/2 + x^2*PolyLog[2, 1 - x]
,
(* 137 *)
cT[Log[x_]^2/(1 - x_), x_, x_] :>
2 - 2*x + 2*Log[x] + 2*x*Zeta2*Log[x] + Log[x]^2 -
	x*Log[1 - x]*Log[x]^2 - (x*Log[x]^3)/3 - 2*x*Log[x]*PolyLog[2, 1 - x] -
	2*x*PolyLog[3, x] + 2*x*Zeta[3]
,
(* 138 *)
cT[Log[x_]^2/x_, x_^(-1), x_] :> -Log[x]^3/(3*x)
,
(* 139 *)
cT[x_*Log[x_]^2, x_^(-1), x_] :>
1/(4*x) - x/4 + (x*Log[x])/2 - (x*Log[x]^2)/2
,
(* 140 *)
cT[x_*Log[x_]^2, x_^2 ,x_] :> 2*x - 2*x^2 + 2*x*Log[x] + x*Log[x]^2
,
(* 141 *)
cT[PlusDistribution[Log[1 - x_]^2/(1 - x_)], x_ ,x_] :>
Log[1 - x]^2 - x*Log[1 - x]^2 + (x*Log[1 - x]^3)/3 +
	2*x*Log[1 - x]*Log[x] - x*Log[1 - x]^2*Log[x] + 2*x*PolyLog[2, 1 - x] -
	2*x*Log[1 - x]*PolyLog[2, 1 - x] + 2*x*PolyLog[3, 1 - x]
,
cT[1, (x_)^(-2), x_] :>  -1/2 + 1/(2*x^2)
,
cT[(x_)^(-1), (x_)^(-2), x_] :> (1 - x)/x^2
,
cT[x_, (x_)^(-2), x_] :> (1 - x^3)/(3*x^2)
,
cT[(x_)^2, (x_)^(-2), x_] :> 1/(4*x^2) - x^2/4
,
cT[(x_)^2, (x_)^2, x_] :> -(x^2*Log[x])
,
cT[ 1, x_^2*Log[1 - x_], x_] :>
-3/4 + x/2 + x^2/4 + Log[1 - x]/2 - (x^2*Log[1 - x])/2
,
cT[ 1, x_^2*Log[x_], x_] :> -1/4 + x^2/4 - (x^2*Log[x])/2
,
cT[x_^2, x_^2*Log[1 - x_],  x_] :>
-(x^2*Log[1 - x]*Log[x]) - x^2*PolyLog[2, 1 - x]
,
cT[x_^2, Log[x_]/(1 - x_), x_] :>
1/4 + x - (5*x^2)/4 + Log[x]/2 + x*Log[x] - (x^2*Log[x]^2)/2 -
	x^2*PolyLog[2, 1 - x]
,
cT[x_^2, PlusDistribution[(1 - x_)^(-1)], x_] :>
1/2 + x - (3*x^2)/2 + x^2*Log[1 - x] - x^2*Log[x]
,
cT[x_^(-2), PlusDistribution[(1 - x_)^(-1)], x_] :>
-x^(-2) + x^(-1) + Log[1 - x]/x^2
,
cT[x_^2, PlusDistribution[Log[1-x_]/(1 - x_)], x_] :>
-x/2 + x^2/2 + Log[1 - x]/2 + x*Log[1 - x] - (3*x^2*Log[1 - x])/2 +
	(x^2*Log[1 - x]^2)/2 + (3*x^2*Log[x])/2 - x^2*Log[1 - x]*Log[x] -
	x^2*PolyLog[2, 1 - x]
,
cT[x_^2 Log[x_], PlusDistribution[(1 - x_)^(-1)], x_] :>
-1/4 - x + (5*x^2)/4 - (3*x^2*Log[x])/2 + x^2*Log[1 - x]*Log[x] -
	(x^2*Log[x]^2)/2 + x^2*PolyLog[2, 1 - x]
,
cT[x_^2 Log[1-x_], PlusDistribution[(1 - x_)^(-1)], x_] :>
-3/4 - x/2 + (5*x^2)/4 - x^2*Zeta2 + Log[1 - x]/2 + x*Log[1 - x] -
	(3*x^2*Log[1 - x])/2 + x^2*Log[1 - x]^2 - x^2*Log[1 - x]*Log[x]
,
cT[x_^2 , x_^2 Log[x_], x_] :>
-(x^2*Log[x]^2)/2
,
cT[(x_)^(-2), Log[1 - (x_)]*(x_), x_] :>
1/6 - 11/(18*x^2) + 1/(3*x) + x/9 + Log[1 - x]/(3*x^2) -
	(x*Log[1 - x])/3
,
cT[(x_)^(-2), Log[x_]*(x_), x_] :> -1/(9*x^2) + x/9 - (x*Log[x])/3
,
cT[Log[1 - (x_)]/(x_)^2, x_, x_] :>
	-1/3 - 1/(6*x) + x/2 + Log[1 - x]/(3*x^2) - (x*Log[1 - x])/3 + (x*Log[x])/3
,
cT[x_, Log[1 - (x_)]/(x_)^2, x_] :>
	-1/3 - 1/(6*x) + x/2 + Log[1 - x]/(3*x^2) - (x*Log[1 - x])/3 + (x*Log[x])/3
,
cT[x_, Log[x_]/(x_)^2, x_] :> 1/(9*x^2) - x/9 + Log[x]/(3*x^2)
,
cT[(x_)^(-2), Log[1 - (x_)]^2*(x_), x_] :>
	-5/18 + 85/(54*x^2) - 11/(9*x) - (2*x)/27 + Log[1 - x]/3 -
	(11*Log[1 - x])/(9*x^2) + (2*Log[1 - x])/(3*x) + (2*x*Log[1 - x])/9 +
	Log[1 - x]^2/(3*x^2) - (x*Log[1 - x]^2)/3
,
cT[(x_)^(-2), Log[1 - (x_)]*Log[x_]*(x_), x_] :>
	-5/36 + 71/(108*x^2) - Pi^2/(18*x^2) - 4/(9*x) - (2*x)/27 -
	Log[1 - x]/(9*x^2) + (x*Log[1 - x])/9 + Log[x]/6 + Log[x]/(3*x) +
	(x*Log[x])/9 + (Log[1 - x]*Log[x])/(3*x^2) - (x*Log[1 - x]*Log[x])/3 +
	PolyLog[2, x]/(3*x^2)
,
cT[(x_)^(-2), Log[x_]^2*(x_), x_] :>
	2/(27*x^2) - (2*x)/27 + (2*x*Log[x])/9 - (x*Log[x]^2)/3
,
cT[Log[x_]/((1 - x_)*x_), x_^(-1) ,x_] :>
-Log[x]^2/(2*x) - PolyLog[2, 1 - x]/x
,
(*CHECKITAGAIN
cT[PlusDistribution[(1 - x_)^(-1)]/x_, x_^(-1) ,x_] :>
Log[1 - x]/x - Log[x]/x
,
cT[PlusDistribution[(1 - x_)^(-1)]/x_, Log[1 - x_]/x_ ,x_] :>
-(Zeta2/x) + Log[1 - x]^2/x - (Log[1 - x]*Log[x])/x
,
cT[PlusDistribution[(1 - x_)^(-1)]/x_, Log[x_]/x_ ,x_] :>
(Log[1 - x]*Log[x])/x - Log[x]^2/(2*x) + PolyLog[2, 1 - x]/x
,
cT[PlusDistribution[Log[1 - x_]/(1 - x_)]/x_, x_^(-1) ,x_] :>
Log[1 - x]^2/(2*x) - (Log[1 - x]*Log[x])/x - PolyLog[2, 1 - x]/x
,
*)

cT[x_^(-1), Log[x_]/(1 - x_) ,x_] :> -(PolyLog[2, 1 - x]/x)
,
cT[Log[x_]/(1 - x_), Log[1 - x_]/x_ ,x_] :>
(Zeta2*Log[1 - x])/x - (Zeta2*Log[x])/x - (Log[1 - x]^2*Log[x])/(2*x) +
	(Log[1 - x]*Log[x]^2)/(2*x) - Nielsen[1, 2, x]/x -
	(Log[1 - x]*PolyLog[2, 1 - x])/x + (Log[x]*PolyLog[2, 1 - x])/x -
	(Log[1 - x]*PolyLog[2, x])/x + PolyLog[3, x]/x
(*
,
CHECKITAGAIN
cT[Log[x_]^2*PlusDistribution[(1 - x_)^(-1)], x_^(-1) ,x_] :>
	(Log[1 - x]*Log[x]^2)/x + (2*Log[x]*PolyLog[2, x])/x -
	(2*PolyLog[3, x])/x + (2*Zeta[3])/x
,
cT[Log[x_]*PlusDistribution[Log[1 - x_]/(1 - x_)], x_^(-1), x_] :>
-((Log[1 - x]*PolyLog[2, 1 - x])/x) + PolyLog[3, 1 - x]/x
*)
,
cT[x_^(-3),1 ,x_] :> -1/3 + 1/(3*x^3)
,

cT[(x_)^(-3), Log[1 - (x_)], x_] :>
1/9 - 11/(18*x^3) + 1/(3*x^2) + 1/(6*x) - Log[1 - x]/3 +
	Log[1 - x]/(3*x^3)
,
cT[(1 + (x_))^(-1), 1, x_] :>
	-Log[2] - Log[x] + Log[1 + x]
,
cT[(1 + (x_))^(-1), x_, x_] :>
	1 - x + x*Log[2] + x*Log[x] - x*Log[1 + x]
,
cT[Log[1 - (x_)]/(1 + (x_)), 1, x_] :>
	Zeta2/2 - Log[2]^2/2 - Log[2]*Log[1 - x] - Log[1 - x]*Log[x] +
	Log[2]*Log[1 + x] + Log[1 - x]*Log[1 + x] + Log[x]*Log[1 + x] -
	Log[1 + x]^2/2 + PolyLog[2, -x] - PolyLog[2, (1 - x)/(1 + x)]
,
cT[Log[1 - (x_)]/(1 + (x_)), x_, x_] :>
		-(x*Zeta2)/2 + (x*Log[2]^2)/2 + Log[1 - x] - x*Log[1 - x] +
		x*Log[2]*Log[1 - x] + x*Log[x] + x*Log[1 - x]*Log[x] -
		x*Log[2]*Log[1 + x] - x*Log[1 - x]*Log[1 + x] - x*Log[x]*Log[1 + x] +
		(x*Log[1 + x]^2)/2 - x*PolyLog[2, -x] + x*PolyLog[2, (1 - x)/(1 + x)]
,
cT[Log[x_]/(1 + (x_)), 1, x_] :>
		Zeta2/2 - Log[x]^2/2 + Log[x]*Log[1 + x] + PolyLog[2, -x]
,
cT[Log[x_]/(1 + (x_)), x_, x_] :>
		1 - x - (x*Zeta2)/2 + Log[x] + (x*Log[x]^2)/2 - x*Log[x]*Log[1 + x] -
		x*PolyLog[2, -x]

(*
,
cT[ ,x_] :>
*)
							}];

(*

,
(* 117 *)

*)

FCPrint[1,"ConvoluteTable.m loaded."];
End[]
