(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* :Title: Integrate3 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 17 December '98 at 15:07 *)
(* ------------------------------------------------------------------------ *)


(* :Summary:  Integrate3 is an integral table *)

(* ------------------------------------------------------------------------ *)

Integrate3::usage=
"Integrate3 contains the integral table used by Integrate2. Integration is
performed in a distributional sense. Integrate3 works more effectively on a
sum of expressions if they are expanded or collected with respect to the
integration variable. See the examples in Integrate2.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Integrate3`Private`"]

mcheck[_Integer?Positive] := True;
mcheck[y_] := If[Variables[y] === {}, False,
										DataType[First[Variables[y]], PositiveInteger]
									];
Options[Integrate3] = {Table -> { }};

Integrate3[a_, {x_,0,1}, ___?OptionQ] := a /; FreeQ[a, x];
Integrate3[a_, {x_,y_,1}, ___?OptionQ] := (1-y) a /; FreeQ[a, x];

polys[z_ ,y_] := polys[z,y] =
Expand[FunctionExpand[PolyGamma[z,y]] /. {Log[4] :> 2 Log[2],
	Log[8] :> 3 Log[2], Log[16] :> 4 Log[2], EulerGamma:>0}];

Integrate3[e_,{x_,b__}, opts___?OptionQ] := Block[{a,min1,res3, zetsub, extratab},
(*
	zetsub = {Pi^2:>6 Zeta2, Pi^4 :> 36 Zeta2^2};
*)
(* inherit options for simplicity ... *)
extratab =  Join[Table /. {opts} /. Options[Integrate3],
									Table /. Options[Integrate2] ];

	zetsub = {Pi^2:>6 Zeta2, Zeta2^2:> Pi^4/36,
						PolyGamma[a_Integer,c_?NumberQ] :> polys[a,c],
						EulerGamma:>0, Log[4] :> 2 Log[2],
	Log[8] :> 3 Log[2], Log[16] :> 4 Log[2], Log[32] :> 5 Log[2]};

	a = e /. {(1/(xx_ - 1)) :> (min1/(1-xx)),
						(x + bla_ x + blabla_.) :> (x (1+bla) +blabla)
						};
	res3 =
	If[Head[a] === Plus,
(* this is quicker than linearity *)
			(Map[((SelectFree[# dummy, x] iT[SelectNotFree[# dummy, x],{x,b}])/.dummy->1
					)&, a]  /. supertab /. zetsub
			) /. Log[1/xxx_] :> (-Log[xxx]) /. iT -> (Hold@@{Integrate3})

(*      Integrate3[#,{x,b}]& /@ a*)
			,
			If[Head[a] === Times,
				SelectFree[a,x] (iT[SelectNotFree[a,x],{x,b}]/.supertab/.zetsub),
					iT[a, {x,b}] /.  supertab  /. zetsub
				]/.Log[1/xxx_]:>(-Log[xxx])/. iT -> (Hold@@{Integrate3})
		] /. {min1 -> (-1), Pi^2 -> (6 Zeta2), Pi^4 -> (36 Zeta2^2)};
	If[(!FreeQ[res3, Hold[Integrate3]]) && (!FreeQ[res3, PolyLog]),
			If[Head[res3]===Plus,
				res3 = SelectFree[res3, Integrate3] +
								Apart[SelectNotFree[res3, Integrate3], x]]
		];
	If[extratab =!= {},
			If[!FreeQ[res3, Hold[Integrate3]],
				res3 = res3 /. extratab]
		];
	res3];

(*
	supertab = Identity[  ]{}
*)

(*SPECIAL*)
iT[(Log[1 + (x_)]*PolyLog[2, -(x_)])/(1 - (x_)), {x_, 0, 1}] = Pi^4/240 +
		iT[(Log[1 - (x)]*PolyLog[2, -(x)])/(1 + (x)), {x, 0, 1}];

	supertab = Dispatch@{
iT[(Log[x_]*Log[x_*(1 - y_) + y_])/(1 - x_)^2,{x_, 0, 1}] :>
(Zeta2 - y*Zeta2 - (y*Log[y]^2)/2 +
PolyLog[2, 1 - y] - y*PolyLog[2, 1 - y]) /; FreeQ[y,x],


iT[DeltaFunctionPrime[1-x_] f_., {x_, 0, 1}] :> (D[f, x]/.x->1),

(*
iT[DeltaFunctionDoublePrime[1-x_] f_., {x_, 0, 1}] :> (D[f, x, x]/.x->1),
*)

iT[en_Integer,{_,0,1}] :> en,

iT[PolyLog[2,1-x_] / (1-x_)^2,{x_,0,1}] :>
		(FCPrint[3,"DISTRIBUTION sense "];
-Zeta2 + 2),

(*I1I*)

(*X*)iT[ (1 - x_)^(-1 + Epsilon/2)*x_^(Epsilon/2), {x_, 0, 1}] :>
	Gamma[Epsilon/2]^2/(2*Gamma[Epsilon]),

(* in a DISTRIBUTION sense : *)

(*I2I*)
(*X*)iT[1/(1-x_),{x_,0,1}] :>
			(FCPrint[3,"DISTRIBUTION sense "];0),
(*I3I*)
(*X*)iT[1/x_,{x_,0,1}] :>
			(FCPrint[3,"DISTRIBUTION sense "];0),

(*I4I*)
(*X*)iT[Log[x_]/x_,{x_,0,1}] :>
			(FCPrint[3,"DISTRIBUTION sense "];0),

(*I4I*)
(*X*)iT[Log[x_]/(1-x_)^2,{x_,0,1}] :>
			(FCPrint[3,"DISTRIBUTION sense "];-1),

(*I4bI*)
(*X*)iT[Log[x_]^2/x_,{x_,0,1}] :>
			(FCPrint[3,"DISTRIBUTION sense "];0),

(*I5I*)
(*X*)iT[Log[1-x_]/(1-x_),{x_,0,1}] :>
			(FCPrint[3,"DISTRIBUTION sense "];0),

(*I6I*)
(*X*)iT[Log[1-x_]^2/(1-x_),{x_,0,1}] :>
			(FCPrint[3,"DISTRIBUTION sense "];0),
(*I7I*)
(*X*)iT[(Log[1 - (x_)]*(x_))/(1 - (x_)), {x_, 0, 1}] :> 1,
(*I8I*)
(*X*)iT[(Log[1 - (x_)]^2*(x_))/(1 - (x_)), {x_, 0, 1}] :> -2,
(*I9I*)
(*X*)iT[(Log[x_]*(x_))/(1 - (x_)), {x_, 0, 1}] :> 1 - Zeta2,
(*I10I*)
(*X*)iT[(Log[1 - (x_)]*Log[x_]*(x_))/(1 - (x_)), {x_, 0, 1}] :>
	-2 + Zeta2 + Zeta[3],
(*I11I*)
(*X*)iT[(Log[x_]^2*(x_))/(1 - (x_)), {x_, 0, 1}] :> -2 + 2*Zeta[3],

(*I12I*)
(*X*)iT[1/(1-y_), {y_,x_/;!NumberQ[x],1}] :> Log[1-x],

(*I13I*)
(*X*)iT[Log[y_]/(1 - x_ y_),{y_,0,1}] :>
			(-PolyLog[2,x]/x) /; FreeQ[x,y],

(*I14I*)
(*X*)iT[Log[1 - y_]/(1 - x_*y_),{y_,0,1}] :>
			(-(Zeta2/x) - Log[1 - x]^2/(2*x) + (Log[1 - x]*Log[x])/x +
	PolyLog[2, 1 - x]/x) /; FreeQ[x,y],

(* abbreviations *)
(*I15I*)
(*X*)iT[Log[1-x_]/x_, {x_,0,1}] :> -Zeta2,

(*I16I*)
(*X*)iT[Log[1 - y_] Log[y_ - z_], {y_, z_/;!NumberQ[z], 1}] :>
		(1 - z)*(2 - Zeta2) - 2*(1 - z)*Log[1 - z] + (1 - z)*Log[1 - z]^2,

(*I17I*)
(*X*)iT[Log[y_]^2 , {y_, z_/;!NumberQ[z], 1}] :>
	2*(1 - z) + 2*z*Log[z] - z*Log[z]^2,

(*I18I*)
(*X*)iT[Log[y_]^2/y_^2, {y_, z_/;!NumberQ[z], 1}] :>
	(2*(1 - z))/z + (2*Log[z])/z + Log[z]^2/z,

(*I19I*)
(*X*)iT[Log[(y_) - (z_)], {y_, z_/;!NumberQ[z], 1}] :>
	-1 + z + (1 - z)*Log[1 - z],

(*I20I*)
(*X*)iT[Log[y_], {y_, z_/;!NumberQ[z], 1}] :> -1 + z - z*Log[z],

(*I21I*)
(*X*)iT[Log[y_]/(y_), {y_, z_/;!NumberQ[z], 1}] :>  -Log[z]^2/2,

(*I22I*)
(*X*)iT[Log[y_]/(y_)^2, {y_, z_/;!NumberQ[z], 1}] :> (1 - z + Log[z])/z,

(*I23I*)
(*X*)iT[Log[1 - (y_)], {y_, z_/;!NumberQ[z], 1}] :>
	-1 + z + (1 - z)*Log[1 - z],

(*I24I*)
(*X*)iT[Log[1 - (y_)]/(y_), {y_, z_/;!NumberQ[z], 1}] :>
	-(Log[1 - z]*Log[z]) - PolyLog[2, 1 - z],

(*I25I*)
(*X*)iT[Log[(y_) - (z_)]^2, {y_, z_/;!NumberQ[z], 1}] :>
	2*(1 - z) - 2*(1 - z)*Log[1 - z] + (1 - z)*Log[1 - z]^2,

(*I26I*)
(*X*)iT[Log[y_]/(1 - (y_)), {y_, z_/;!NumberQ[z], 1}] :>
	-PolyLog[2,1-z],

(* w47 *)
(*I27I*)
(*X*)iT[PolyLog[2, 1 - (y_)], {y_, z_/;!NumberQ[z], 1}] :>
	-1 + z - z*Log[z] + (1 - z)*PolyLog[2, 1 - z],

(* SMALLEPSILON & SMALLDELTA BEGIN*)


(* w56  *)
(*I28I*)
(*X*)iT[Log[1 - (y_)]^2/((y_) - (z_)), {y_, z_ + SmallEpsilon, 1}] :>
	-2*Zeta2*Log[1 - z] - Log[SmallEpsilon]*Log[1 - z]^2 +
		Log[1 - z]^3 + 2*Zeta[3],

(* w55  *)
(*I29I*)
(*X*)iT[(Log[1 - (y_)]*Log[(y_) - (z_)])/((y_) - (z_)),
						{y_, z_ + SmallEpsilon, 1}] :>
	-(Zeta2*Log[1 - z]) - (Log[SmallEpsilon]^2*Log[1 - z])/2 +
		Log[1 - z]^3/2 + Zeta[3],

(* w54  *)
(*I30I*)
(*X*)iT[(Log[1 - (y_)]*Log[y_])/((y_) - (z_)),
							{y_, z_ + SmallEpsilon, 1}] :>
	-(Zeta2*Log[z]) - Log[SmallEpsilon]*Log[1 - z]*Log[z] +
		Log[1 - z]^2*Log[z] + (Log[1 - z]*Log[z]^2)/2 +
		(Log[1 - z] + Log[z])*PolyLog[2, 1 - z] - PolyLog[3, 1 - z] +
		2*((Log[1 - z]*Log[z]^2)/2 + Log[z]*PolyLog[2, z] -
	PolyLog[3, z] + Zeta[3]),

(* w52 *)
(*I31I*)
(*X*)iT[Log[(y_) - (z_)]^2/(1 - (y_)),
	{y_, z_/;!NumberQ[z], 1-SmallDelta}] :>
	-2*Zeta2*Log[1 - z] - Log[SmallDelta]*Log[1 - z]^2 + Log[1 - z]^3 +
		2*Zeta[3],

(* w51 *)
(*I32I*)
(*X*)iT[(Log[1 - (y_)]*Log[(y_) - (z_)])/(1 - (y_)),
	{y_, z_/;!NumberQ[z], 1-SmallDelta}] :>
	-(Zeta2*Log[1 - z]) - (Log[SmallDelta]^2*Log[1 - z])/2 +
		Log[1 - z]^3/2 + Zeta[3],

(* w46 *)
(*I33I*)
(*X*)iT[Log[1 - (y_)]/((y_) - (z_)), {y_, z_ + SmallEpsilon, 1}] :>
	-Zeta2 - Log[SmallEpsilon]*Log[1 - z] + Log[1 - z]^2,

(* w45 *)
(*I34I*)
(*X*)iT[PolyLog[2, 1 - (y_)]/(y_), {y_, z_/;!NumberQ[z], 1}] :>
	-(Log[z]*PolyLog[2, 1 - z]) -
		2*((Log[1 - z]*Log[z]^2)/2 + Log[z]*PolyLog[2, z] -
	PolyLog[3, z] + Zeta[3]),

(* w44 *)
(*I35I*)
(*X*)iT[PolyLog[2, 1 - (y_)]/((y_) - (z_)),
						{y_, z_ + SmallEpsilon, 1}] :>
	-(Log[1 - z]*Log[z]^2)/2 - Log[SmallEpsilon]*PolyLog[2, 1 - z] +
		Log[1 - z]*PolyLog[2, 1 - z] - Log[z]*PolyLog[2, z] -
	PolyLog[3, 1 - z] + PolyLog[3, z] - Zeta[3],

(* w43 *)
(*I36I*)
(*X*)iT[Log[(y_) - (z_)]/(1 - (y_)),
	{y_, z_/;!NumberQ[z], 1-SmallDelta}] :>
	-Zeta2 - Log[SmallDelta]*Log[1 - z] + Log[1 - z]^2,

(* w42 *)
(*I37I*)
(*X*)iT[(1 - (y_))^(-1), {y_, z_/;!NumberQ[z], 1-SmallDelta}] :>
	-Log[SmallDelta] + Log[1 - z],

(* w38 *)
(*I38I*)
(*X*)iT[(Log[y_]*Log[(y_) - (z_)])/((y_) - (z_)),
						{y_, z_ + SmallEpsilon, 1}] :>
	-(Log[SmallEpsilon]^2*Log[z])/2 + (Log[1 - z]^2*Log[z])/2 +
		Log[1 - z]*Log[z]^2 + Log[z]^3/6 +
		(Log[1 - z] + Log[z])*PolyLog[2, 1 - z] + Log[z]*PolyLog[2, z] -
		PolyLog[3, 1 - z] - PolyLog[3, z] + Zeta[3],

(* w37 *)
(*I39I*)
(*X*)iT[Log[y_]^2/((y_) - (z_)), {y_, z_ + SmallEpsilon, 1}] :>
	-(Log[SmallEpsilon]*Log[z]^2) + Log[1 - z]*Log[z]^2 +
		(2*Log[z]^3)/3 + 2*Log[z]*PolyLog[2, 1 - z] +
		2*((Log[1 - z]*Log[z]^2)/2 + Log[z]*PolyLog[2, z] -
	PolyLog[3, z] + Zeta[3]),

(* w36 *)
(*I40I*)
(*X*)iT[Log[(y_) - (z_)]/(1 - (y_)),
	{y_, z_/;!NumberQ[z], 1-SmallDelta}] :>
	-Zeta2 - Log[SmallDelta]*Log[1 - z] + Log[1 - z]^2,

(* w35 *)
(*I41I*)
(*X*)iT[Log[1 - (z_)]/(1 - (y_)),
	{y_, z_/;!NumberQ[z], 1-SmallDelta}] :>
	-(Log[SmallDelta]*Log[1 - z]) + Log[1 - z]^2,

(* w33 *)
(*I42I*)
(*X*)iT[Log[(y_) - (z_)]/((y_) - (z_)), {y_, z_ + SmallEpsilon, 1}] :>
	-Log[SmallEpsilon]^2/2 + Log[1 - z]^2/2,

(* w32 *)
(*I43I*)
(*X*)iT[Log[1 - (z_)]/((y_) - (z_)), {y_, z_ + SmallEpsilon, 1}] :>
	-(Log[SmallEpsilon]*Log[1 - z]) + Log[1 - z]^2,

(* w31*)
(*I44I*)
(*X*)iT[Log[y_]/((y_) - (z_)), {y_, z_ + SmallEpsilon, 1}] :>
	-(Log[SmallEpsilon]*Log[z]) + Log[1 - z]*Log[z] + Log[z]^2/2 +
		PolyLog[2, 1 - z],


(* SMALLEPSILON & SMALLDELTA END *)
(*I45I*)
(*X*)iT[Log[1 - (y_)]/(y_)^2, {y_, SmallDelta, 1}] :>
	-1 + Log[SmallDelta],

(*I46I*)
(*X*)iT[Log[y_] PlusDistribution[1/(1-y_)],{y_, 0, 1}] :> - Zeta2,
(*I47I*)
(*X*)iT[Log[y_]  PlusDistribution[Log[1-y_]/(1-y_)],{y_, 0, 1}] :>
		Zeta[3],
(*I48I*)
(*X*)iT[Log[y_]^2 PlusDistribution[1/(1-y_)],{y_, 0, 1}] :> 2 Zeta[3],

(* NIELSEN : 2 S12 *)

(*I49I*)
(*X*)iT[Log[y_]^2/(1 - (y_)), {y_, z_/;!NumberQ[z], 1}] :>
	Log[1 - z]*Log[z]^2 + 2*Log[z]*PolyLog[2, z] -
	2*PolyLog[3, z] + 2*Zeta[3],
	(* = 2 S12[1-z] *)

(* ## temporary problems in V2.3 *)


(* w52  *)
(* z = 0 is also o.k. *)
(*I50I*)
(*X*)iT[(Log[y_]*Log[1-y_])/(1 - (y_)), {y_, z_, 1}] :>
	-(Log[1 - z]*PolyLog[2, 1 - z]) + PolyLog[3, 1 - z],

(* w50 *)
(*I51I*)
(*X*)iT[PolyLog[2, -(((1 - (y_))*(z_))/((y_) - (z_)))]/(y_)^2,
						{y_, z_/;!NumberQ[z], 1}] :> -(((1 - z)*Zeta2)/z),

(* w49 *)
(*I52I*)
(*X*)iT[PolyLog[2, -(((1 - (y_))*(z_))/((y_) - (z_)))],
	{y_, z_/;!NumberQ[z], 1}] :>
	(1 - z)*(-Zeta2 + PolyLog[2, 1 - z]),

(* w48 *)
(*I53I*)
(*X*)iT[PolyLog[2, -(((1 - (y_))*(z_))/((y_) - (z_)))]/(y_),
						{y_, z_/;!NumberQ[z], 1}] :>
	Zeta2*Log[z] - Log[z]*PolyLog[2, 1 - z] -
	2*((Log[1 - z]*Log[z]^2)/2 + Log[z]*PolyLog[2, z]
	- PolyLog[3, z]+Zeta[3]),

(* w41  *)
(*I54I*)
(*X*)iT[(Log[y_]*Log[(y_) - (z_)])/(1 - (y_)),
	{y_, z_/;!NumberQ[z], 1}] :>
	(Log[1 - z]*Log[z]^2)/2 - Log[1 - z]*PolyLog[2, 1 - z] +
		Log[z]*PolyLog[2, z] + PolyLog[3, 1 - z] - PolyLog[3, z] + Zeta[3],

(* w30 *)
(*I55I*)
(*X*)iT[Log[(y_) - (z_)]^2/(y_)^2, {y_, z_/;!NumberQ[z], 1}] :>
	((1 - z)*Log[1 - z]^2 + 2*Log[1 - z]*Log[z] + Log[z]^2 +
			2*PolyLog[2, 1 - z])/z,

(* w29 *)
(*I56I*)
(*X*)iT[Log[(y_) - (z_)]^2/(y_), {y_, z_/;!NumberQ[z], 1}] :>
	-(Log[1 - z]^2*Log[z]) - Log[1 - z]*Log[z]^2 - Log[z]^3/3 -
		2*Log[1 - z]*PolyLog[2, 1 - z] - 2*Log[z]*PolyLog[2, 1 - z] +
		2*PolyLog[3, 1 - z] - 2*((Log[1 - z]*Log[z]^2)/2 +
		Log[z]*PolyLog[2, z] - PolyLog[3, z] + Zeta[3]),

(* w18 *)
(*I57I*)
(*X*)iT[Log[(y_) - (z_)]/(y_)^2, {y_, z_/;!NumberQ[z], 1}] :>
	((1 - z)*Log[1 - z] + Log[z])/z,

(* w17 *)
(*I58I*)
(*X*)iT[Log[(y_) - (z_)]/(y_), {y_, z_/;!NumberQ[z], 1}] :>
	-(Log[1 - z]*Log[z]) - Log[z]^2/2 - PolyLog[2, 1 - z],

(* w15 *)
(*I59I*)
(*X*)iT[(Log[y_]*Log[(y_) - (z_)])/(y_)^2, {y_, z_/;!NumberQ[z], 1}] :>
	((1 - z)*Log[1 - z] + Log[z] + Log[1 - z]*Log[z] + Log[z]^2 +
			PolyLog[2, 1 - z])/z,

(* w14 *)
(*I60I*)
(*X*)iT[(Log[y_]*Log[(y_) - (z_)])/(y_), {y_, z_/;!NumberQ[z], 1}] :>
	-(Log[1 - z]*Log[z]^2) - Log[z]^3/3 - Log[z]*PolyLog[2, 1 - z] -
		Log[z]*PolyLog[2, z] + PolyLog[3, z] - Zeta[3],

(* w13 *)
(*I61I*)
(*X*)iT[Log[y_]*Log[(y_) - (z_)], {y_, z_/;!NumberQ[z], 1}] :>
	-((1 - z)*(-2 + Log[1 - z])) -
		z*(-Log[z] + Log[1 - z]*Log[z] + Log[z]^2/2 + PolyLog[2, 1 - z]),

(* w6 *)
(*I62I*)
(*X*)iT[Log[1-y_] Log[y_ - z_] / y_^2, {y_,z_/;!NumberQ[z],1}] :>
	-(((1 - z)*Zeta2)/z) + ((1 - z)*Log[1 - z]^2)/z +
		((1 + z)*Log[1 - z]*Log[z])/z + Log[z]^2/2 +
	((1 + z)*PolyLog[2, 1 - z])/z,

(* w5 *)
(*I63I*)
(*X*)iT[Log[1-y_] Log[y_ - z_] / y_, {y_,z_/;!NumberQ[z],1}] :>
	Zeta2*Log[z] - Log[1 - z]^2*Log[z] - Log[1 - z]*Log[z]^2 -
		(2*Log[1 - z] + Log[z])*PolyLog[2, 1 - z] - Log[z]*PolyLog[2, z] +
		2*PolyLog[3, 1 - z] + PolyLog[3, z] - Zeta[3],

(*I64I*)
(*X*)iT[Log[x_/y_] Log[y_] / ( (1-y_ ) y_ ),
	{y_,x_/;!NumberQ[x],1}] :>
	-(Zeta2*Log[x]) - Log[x]^3/6 - Log[x]*PolyLog[2, x] +
		2*PolyLog[3, x] - 2*Zeta[3],

(*I65I*)
(*X*)iT[Log[1 - y_]^2/y_^2, {y_, x_/;!NumberQ[x], 1}] :>
	-2*(-((1 - x)*Log[1 - x]^2)/(2*x) - Log[1 - x]*Log[x] -
			PolyLog[2, 1 - x]),

(*I66I*)
(*X*)iT[Log[1 - x_/y_]^2/y_, {y_, x_/;!NumberQ[x], 1}] :>
	-(Log[1 - x]^2*Log[x]) - 2*Log[1 - x]*PolyLog[2, 1 - x] +
		2*PolyLog[3, 1 - x],

(*I67I*)
(*X*)iT[Log[1 - y_]^2/y_, {y_, x_/;!NumberQ[x], 1}] :>
	-2*((Log[1 - x]^2*Log[x])/2 + Log[1 - x]*PolyLog[2, 1 - x] -
			PolyLog[3, 1 - x]),

(*I68I*)
(*X*)iT[(Log[1 - x_/y_]*Log[y_])/(1 - y_),
	{y_,x_/;!NumberQ[x],1}]:>
	-(Zeta2*Log[1 - x]) + Log[1 - x]^2*Log[x] -
		(Log[1 - x]*Log[x]^2)/2 + (Log[1 - x] - Log[x])*PolyLog[2, x] +
		PolyLog[3, 1 - x] + PolyLog[3, x] - Zeta[3],

(*I69I*)
(*X*)iT[(Log[y_]*Log[-x_ + y_])/(1 - y_), {y_,x_/;!NumberQ[x],1}] :>
	-(Zeta2*Log[1 - x]) + Log[1 - x]^2*Log[x] + (Log[1 - x]*Log[x]^2)/2 +
		(Log[1 - x] + Log[x])*PolyLog[2, x] + PolyLog[3, 1 - x] -
			PolyLog[3, x] + Zeta[3],

(*I70I*)
(*X*)iT[(Log[1 - x_/y_]*Log[y_])/y_, {y_, x_/;!NumberQ[x], 1}] :>
	Zeta2*Log[x] + (Log[-x^(-1)]*Log[x]^2)/2 + Log[x]^3/3 +
		PolyLog[3, x^(-1)] - Zeta[3],

(*I71I*)
(*X*)iT[(Log[1 - y_]*Log[x_/y_])/y_, {y_, x_/;!NumberQ[x], 1}] :>
	-(Zeta2*Log[x]) + PolyLog[3, x] - Zeta[3],


(*I72I*)
(*X*)iT[Log[1-y_] Log[y_]/y_, {y_,x_/;!NumberQ[x],1}] :>
	Log[x]*PolyLog[2, x] - PolyLog[3, x] + Zeta[3],

(*I73I*)
(*X*)iT[Log[y_] Log[1-y_], {y_,x_/;!NumberQ[x],1}] :>
	2 - 2*x + x*Log[x] + Log[1 - x]*(-1 + x - x*Log[x]) -
		PolyLog[2, 1 - x],

(*I74I*)
(*X*)iT[Log[1-y_]^2/y_,{y_,x_ /; x =!= 0,1}] :>
	-(Log[1 - x]^2*Log[x]) - 2*Log[1 - x]*PolyLog[2, 1 - x] +
		2*PolyLog[3, 1 - x],

(*I75I*)
(*X*)iT[Log[1-y_] Log[y_], {y_,x_/;!NumberQ[x],1}] :>
	2 - 2 x + x Log[x] - Log[1 - x] (1 - x + x Log[x]) - PolyLog[2, 1 - x],

(*I76I*)
(*X*)iT[Log[1 - y_]^2, {y_, x_/;!NumberQ[x], 1}] :>
	2*(1 - x) - 2*(1 - x)*Log[1 - x] + (1 - x)*Log[1 - x]^2,

(*I77I*)
(*X*)iT[Log[x_/ y_]^2, {y_,x_/;!NumberQ[x],1}] :>
	2 - 2*x + 2*Log[x] + Log[x]^2,

(*I78I*)
(*X*)iT[Log[1-y_] Log[y_]/y_^2,{y_,x_/;!NumberQ[x],1}] :>
	((1 - x)*Log[1 - x])/x + Log[x] + Log[1 - x]*Log[x] -
		((-1 + x)*Log[1 - x]*Log[x])/x + Log[x]^2/2 + PolyLog[2, 1 - x],

(*I79I*)
(*X*)iT[Log[1 - x_/x3_]^2, {x3_, x_/;!NumberQ[x], 1}] :>
	Log[1 - x]*((1 - x)*Log[1 - x] + 2*x*Log[x]) + 2*x*PolyLog[2, 1 - x],

(*I80I*)
(*X*)iT[Log[1 - x_/y_]*Log[y_], {y_, x_/;!NumberQ[x], 1}] :>
	-(Pi^2*x)/6 - Log[1 - x] + x*Log[1 - x] - x*Log[x] +
		(x*Log[x]^2)/2 + x*PolyLog[2, x],

(*I81I*)
(*X*)iT[Log[x_/y_] Log[1-y_], {y_, x_/;!NumberQ[x], 1}] :>
	-2 + 2*x - Log[1 - x]*(-1 + x - Log[x]) - Log[x] + PolyLog[2, 1 - x],

(*I82I*)
(*X*)iT[Log[1 - x_/y_]*Log[1 - y_], {y_, x_/;!NumberQ[x], 1}] :>
	-Zeta2 + x*Zeta2 + (-1 + x)*Log[1 - x] + (1 - x)*Log[1 - x]^2 - x*Log[x] +
		x*Log[1 - x]*Log[x] + PolyLog[2, 1 - x],

(*I83I*)
(*X*)iT[ Log[1-y_]^2/y_^4, {y_, x_/;!NumberQ[x], 1}] :>
		-1/3 + 1/(3*x) + ((-1/3 - (2*x)/3 + x^2)*Log[1 - x])/x^2 +
		((1/(3*x) - x^2/3)*Log[1 - x]^2)/x^2 - Log[x] +
		(2*Log[1 - x]*Log[x])/3 + (2*PolyLog[2, 1 - x])/3,

(*I84I*)
(*X*)iT[ Log[1-y_]^2/y_^3, {y_, x_/;!NumberQ[x], 1}] :>
		Log[1 - x] - Log[1 - x]/x - Log[1 - x]^2/2 + Log[1 - x]^2/(2*x^2) -
		Log[x] + Log[1 - x]*Log[x] + PolyLog[2, 1 - x],

(*I85I*)
(*X*)iT[ (Log[x_]*Log[1 - x_ + x_*z_])/x_, {x_,0,1}
						] :> PolyLog[3, 1-z],

(*I86I*)
(*X*)iT[ (Log[x_]*Log[1 - x_ (1-z_)])/x_, {x_,0,1}
						] :> PolyLog[3, 1-z],

(*I87I*)
(*X*)iT[
	(Log[x_]*Log[1 - (x_) (1 - (z_))])/(1 - (x_)*(1 - (z_))),
						{x_,0,1}] :>
	(Log[1 - z]*Log[z]^2)/(2*(1 - z)) + (Log[z]*PolyLog[2, z])/(1 - z) -
		PolyLog[3, z]/(1 - z) + Zeta[3]/(1 - z),


(*I88I*)
(*X*)iT[Log[1 - y_]/y_^4, {y_, x_/;!NumberQ[x], 1}] :>
	1/2 - 1/(6*x^2) - 1/(3*x) - Log[1 - x]/3 + Log[1-x]/(3*x^3)+Log[x]/3,

(*I89I*)
(*X*)iT[Log[1-x_/x2_]/x2_^3,{x2_,x_/;!NumberQ[x],1}] :>
	(-3 + 2*x + x^2 + 2*Log[1 - x] - 2*x^2*Log[1 - x])/(4*x^2),

(*I90I*)
(*X*)iT[Log[1 - y_]/y_^3, {y_, x_/;!NumberQ[x], 1}] :>
		1/2 - 1/(2*x) - Log[1 - x]/2 + Log[1 - x]/(2*x^2) + Log[x]/2,

(*I91I*)
(*X*)iT[Log[1 - y_]/y_^2, {y_, x_/;!NumberQ[x], 1}] :>
			-Log[1 - x] + Log[1 - x]/x + Log[x],

(*I92I*)
(*X*)iT[(Log[1 - x_/y_] + Log[x_/y_])/((1 + x_/y_)*y_),
	{y_, x_/;!NumberQ[x], 1}
						] :>
		-Pi^2/6 - Log[-1 + x^(-1)]*Log[2*x] + Log[1 - x]*Log[2*x] -
		Log[x]*(-Log[4] + 2*Log[2*x]) + Log[x]*Log[1 + x] +
		Log[1 - x]*Log[(1 + x)/(2*x)] - 2*PolyLog[2, -x^(-1)] +
		PolyLog[2, (-1 + x)/(2*x)],

(* ## temporary problems in V2.3 *)

(*I93I*)
(*X*)iT[1/(1 - y_),{y_, 0, x_}] :> -Log[1 - x],

(*I94I*)
(*X*)iT[Log[x_ (1-x_)]^3, {x_,0,1}] :> -48 + 2 Pi^2 + 12 Zeta[3],
(*I95I*)
(*X*)iT[Log[x_ (1-x_)]^2, {x_,0,1}] :> 8 - 2 Zeta2,
(*I96I*)
(*X*)iT[Log[x_ (1-x_)], {x_,0,1}] :> - 2,

	(* NEW 05/95*)

(*I97I*)
(*X*)iT[Log[1 + ((1 - x_)*y_)/x_]/(1 - (1 - x_^(-1))*y_), {y_,0,1}] :>
	(x*Log[x]^2)/(2*(1 - x)),

(*I98I*)
(*X*)iT[Log[1+a_ y_]/(1+b_ y_), {y_, 0,1}] :>
	((Log[1 + a]*Log[1 - ((1 + a)*b)/(-a + b)] - PolyLog[2, b/(-a + b)] +
		PolyLog[2, ((1 + a)*b)/(-a + b)])/b
	) /; FreeQ[{a,b},y] && (Factor2[a] =!= Factor2[b]) &&
							(Simplify[a] =!= -1) &&
							(Factor2[1 - ((1 + a)*b)/(-a + b)]=!=0),

(* in a distributional sense; i.e. Log[SmallDelta] -> 0 *)
(*I98bI*)iT[Log[1- y_ x_]/(1 - x_), {x_, 0, 1}] :>
(Zeta2+Log[1-y]^2/2-Log[1 - y]*Log[y]-PolyLog[2, 1 - y]
) /; FreeQ[y, x],

(* in a distributional sense; i.e. Log[SmallDelta] -> 0 *)
(*I98cI*)iT[(Log[1 - y_]*Log[1 - y_ u_])/(1 - y_), {y_, 0, 1}
					] :> (
Zeta2*Log[1 - u] + Log[1 - u]^3/6 -
	(Log[1 - u]^2*Log[u])/2 - PolyLog[3, 1 - u] -
		PolyLog[3, u] + Zeta[3]
								) /; FreeQ[u, y],

(*I99I*)
(*X*)iT[Log[1 + ((1 - x_)*y_)/x_]/(1 - (1 - x_^(-1))*y_) ,{y_,0,1}] :>
		(x*Log[x]^2)/(2*(1 - x)) /; FreeQ[x,y],

(*I100I*)
(*X*)iT[Log[1+a_ y_]/(1+ a_ y_),{y_,0,1}] :>
		(1/2/a Log[1+a]^2)/;FreeQ[a,y] && (Simplify[a]=!=-1),

(*I101I*)
(*X*)iT[Log[1+a_ y_]^2/(1+ a_ y_),{y_,0,1}] :>
		(1/3/a Log[1+a]^3)/;FreeQ[a,y] && (Simplify[a]=!=-1),

(*I102I*)
(*X*)iT[Log[1-y_]/(1-(1-x_) y_),{y_,0, 1}] :>
	(-1/2 Log[x]^2/(1-x) - PolyLog[2,1-x]/(1-x)) /; FreeQ[x,y],

(*I103I*)
(*X*)iT[(x_ (1-x_))^(Epsilon/2),{x_,0,1}] :>
	Epsilon/4 Gamma[Epsilon/2]^2/(1+Epsilon)/Gamma[Epsilon],

(*I104I*)
(*X*)iT[(x_ (1-x_))^(Epsilon/2)/x_,{x_,0,1}] :>
	1/2 Gamma[Epsilon/2]^2/Gamma[Epsilon],

(*I105I*)
(*X*)iT[(x_ (1-x_))^(Epsilon/2)/x_^2,{x_,0,1}] :>
	Gamma[1+Epsilon/2] Gamma[-1+Epsilon/2]/Gamma[Epsilon],


(*I106I*)
(*X*)iT[((-2 + x2_)*(-2*x_ + x2_)*Log[1 - x2_]) (x2_)^(-2),
						{x2_, x_/;!NumberQ[x], 1}]:>
	-1 + x + 5*(1 - x)*Log[1 - x] + 4*x*Log[x] + 2*(1 + x)*Log[1 - x]*Log[x] +
		2*(1 + x)*PolyLog[2, 1 - x],

(*I107I*)
(*X*)iT[Log[y_]/(y_ - y_^2), {y_,x_/;!NumberQ[x],1}] :>
						-Log[x]^2/2 - PolyLog[2, 1 - x],

(*I108I*)
(*X*)iT[Log[y_]/y_/(1 - y_), {y_,x_/;!NumberQ[x],1}] :>
						-Log[x]^2/2 - PolyLog[2, 1 - x],

(*I109I*)
(*X*)iT[y_ Log[x_/y_]/(y_-x_),{y_,x_/;!NumberQ[x],1}] :>
						1 - x + (Pi^2*x)/6 + Log[x] + x*Log[1 - x^(-1)]*Log[x] -
		x*PolyLog[2, x^(-1)],

(*I110I*)
(*X*)iT[Log[1 - x_/x1_]/(1 - x1_), {x1_, 0, x_ /; x =!= 1}] :>
	-Log[1 - x]^2/2 + Log[(1 - x)^(-1)]*Log[-x] + Log[1 - x]*Log[x],

(*I111I*)
(*X*)iT[Log[1 - y_]/y_,{y_,x_/;!NumberQ[x],1}] :>
	-Zeta2 + PolyLog[2, x],

(*I112I*)
(*X*)iT[Log[1-x_/y_]/y_ , {y_, x_/;!NumberQ[x], 1}] :>
	-Zeta2 + PolyLog[2,x],

(*I113I*)
(*X*)iT[Log[1-x_/y_]/y_^2 , {y_, x_/;!NumberQ[x], 1}] :>
	1 - x^(-1) - Log[1 - x] + Log[1 - x]/x,

(*I114I*)
(*X*)iT[PolyLog[2, 1-x_], {x_, 0, 1}] :> -1 + Zeta2,
(*I115I*)
(*X*)iT[x_ PolyLog[2, 1-x_], {x_, 0, 1}] :> -5/8 + 1/2 Zeta2,

(*I116I*)
(*X*)iT[x_^2 Log[ 1-x_] Log[x_], {x_,0,1}] :>
				71/108 -  Zeta2/3,

(*I117I*)
(*X*)iT[x_^3 Log[ 1-x_] Log[x_], {x_,0,1}] :>
				35/72 - Zeta2/4,

(*I118I*)
(*X*)iT[x_^2 Log[ 1+x_] Log[x_], {x_,0,1}] :>
				-2/9 Log[2] - 1/6 Zeta2 + 41/108,

(*I119I*)
(*X*)iT[x_^3 Log[ 1+x_] Log[x_], {x_,0,1}] :>
						1/8 Zeta2 - 17/72,

(*I120I*)
(*X*)iT[Log[x_] Log[1+x_]/(1+x_), {x_,0,1}] :>
							-1/8 Zeta[3],

(*I121I*)
(*X*)iT[x_ PolyLog[2, -x_], {x_,0,1}] :>
							-1/4 Zeta2 + 1/8,

(* from Barbieri, Miganaco  && Remmiddi *)
(*I122I*)
(*X*)iT[PolyLog[2, -x_]/(1+x_), {x_,0,1}] :>
								1/4 Zeta[3] - 1/2 Zeta2 Log[2],

(*I123I*)
(*X*)iT[x_^2 PolyLog[2, -x_], {x_,0,1}] :>
							-1/6 Zeta2 + 2/9 Log[2] - 5/54,

(*I124I*)
(*X*)iT[x_^3 PolyLog[2, -x_], {x_,0,1}] :>
							-1/8 Zeta2 + 7/192,

(*I125I*)
(*X*)iT[Log[1-x_] Log[x_]/(1-x_), {x_,0,1}] :> Zeta[3],
(*I126I*)
(*X*)iT[Log[x_]^2/(1-x_), {x_,0,1}] :> 2 Zeta[3],

(* Wlist  ( n = m-1) *)

(* xto1 *)
(* alista2 *)
(*I127I*)
(*X*)iT[1/(1-y_) Log[y_/x_], {y_,0,x_}] :> -PolyLog[2, x],

(* alista3 *)
(*I128I*)
(*X*)iT[ 1/(1-y) Log[1-y/x], {y,0,x}] :>
	-PolyLog[2, x] - 1/2 Log[1-x]^2,
(* w-1 *)
(*I129I*)
(*X*)iT[f_. DeltaFunction[1-x_],{x_,0,1}] :> (f /. x -> 1),
(*I130I*)
(*X*)iT[PlusDistribution[1/(1-x_)],{x_,0,1}] :> 0,
(*I131I*)
(*X*)iT[x_ PlusDistribution[1/(1-x_)],{x_,0,1}] :> -1,
(*I132I*)
(*X*)iT[x_^2 PlusDistribution[1/(1-x_)],{x_,0,1}] :> -3/2,
(*I133I*)
(*X*)iT[x_^4 PlusDistribution[1/(1-x_)],{x_,0,1}] :> -25/12,
(*I134I*)
(*X*)iT[PlusDistribution[Log[1-x_]/(1-x_)],{x_,0,1}] :> 0,
(*I135I*)
(*X*)iT[PlusDistribution[Log[1-x_]^2/(1-x_)],{x_,0,1}] :> 0,

(* w0 *)
(*I136I*)
(*X*)iT[x_^n_?mcheck , {x_,0,1}] :> 1/(n+1),

(* w1 *)
(*I137I*)
(*X*)iT[(x_^_?mcheck) DeltaFunction[1-x_],
						{x_,0,1}] :> 1,

(* w2 *)

(*I138I*)
(*X*)iT[(x_^n_?mcheck) PlusDistribution[1/(1-x_)], {x_,0,1}] :>
	- SumS[1, n],
(*X*)iT[(x_^n_?mcheck) 1/(1-x_), {x_,0,1}] :>
	- SumS[1, n],

(* w3 *)
(*I139I*)
(*X*)iT[(x_^n_ /; mcheck[n]) 1/(1+x_), {x_,0,1}] :>
	-(-1)^(n+1) (SumT[1,n] + Log[2]),

(* w4 *)
(*I140I*)
(*X*)iT[(x_^n_ /; mcheck[n]) Log[x_], {x_,0,1}] :>
	-1/(n+1)^2,

(* w5 *)
(*I141I*)
(*X*)iT[(x_^n_ /; mcheck[n]) Log[x_]/(1-x_), {x_,0,1}] :>
	-Zeta2 + SumS[2, n],
(*X*)iT[(x_^n_ /; mcheck[n]) Log[x_] PlusDistribution[1/(1-x_)],
				{x_,0,1}
			] :>
	-Zeta2 + SumS[2, n],

(* w6 *)
(*I142I*)
(*X*)iT[x_^n_?mcheck Log[1-x_], {x_,0,1}] :> - 1/(n+1)^2 - SumS[1,n]/(n+1),

(* w7 *)
(*I143I*)
(*X*)iT[x_^n_?mcheck PlusDistribution[Log[1-x_]/(1-x_)], {x_, 0, 1}] :>
	SumS[1,1,n],

(* w7n *)
(*I143aI*)
(*X*)iT[x_^n_?mcheck Log[1-x_]/(1-x_), {x_, 0, 1}] :>
	SumS[1,1,n],

(* w7v *)
(*I143bI*)
(*X*)iT[x_^n_?mcheck Log[1-x_]^2/(1-x_), {x_, 0, 1}] :>
-SumS[1, 1 + n]^3/3 - SumS[1, 1 + n]* SumS[2, 1 + n] +
(SumS[1, 1 + n]^2 + SumS[2, 1 + n])/ (1 + n) -
(2*SumS[3, 1 + n])/3
,

(* w8 *)
(*I144I*)
(*X*)iT[x_^n_?mcheck Log[1-x_]^2, {x_, 0, 1}] :>
		1/(n+1) SumS[1,n+1]^2 + 1/(n+1) SumS[2, n+1],

(* r80 *)
(*I144aI*)
(*X*)iT[x_^n_?mcheck Log[1-x_]^2 Log[x_], {x_, 0, 1}] :>
	-4/n^3 - 6/(1 + n)^4 + 4/(1 + n)^3 + (2*Zeta2)/n +
(2*Zeta2)/(1 + n)^2 -
	(2*SumS[1, -1 + n])/n^2 - (4*SumS[1, -1 + n])/(1 + n)^3 +
	(2*SumS[1, -1 + n])/(1 + n)^2 + (2*Zeta2*SumS[1, -1 + n])/(1 + n) -
	SumS[1, -1 + n]^2/(1 + n)^2 - (2*SumS[2, -1 + n])/n -
	(3*SumS[2, -1 + n])/(1 + n)^2 + (2*SumS[2, -1 + n])/(1 + n) -
	(2*SumS[1, -1 + n]*SumS[2, -1 + n])/(1 + n) -
	(2*SumS[3, -1 + n])/(1 + n) - (2*(Zeta2 - Zeta[3]))/(1 + n)
,
(*
-(6*EulerGamma^2 + Pi^2 -
		12*EulerGamma*(1 + n)*PolyGamma[0, 1 + n]^2 +
		12*EulerGamma*(1 + n)*PolyGamma[0, 2 + n]^2 +
		6*(1 + n)*PolyGamma[0, 2 + n]^3 - 18*PolyGamma[1, 2 + n] -
		12*EulerGamma*PolyGamma[1, 2 + n] -
		12*EulerGamma*n*PolyGamma[1, 2 + n] -
		6*PolyGamma[0, 1 + n]*((1 + n)*PolyGamma[0, 2 + n]^2 +
				2*(EulerGamma + (1 + n)*PolyGamma[1, 2 + n])) +
		6*PolyGamma[2, 2 + n] + 6*n*PolyGamma[2, 2 + n])/(6*(1 + n)^2),
*)

(* r8a *)
(*I144bI*)
(*X*)iT[x_^n_?mcheck Log[1-x_]^3, {x_, 0, 1}] :>
-6/n^3 - (6*EulerGamma)/n^2 - (3*EulerGamma^2)/n - 6/(1 + n)^4 +
	6/(1 + n)^3 - (6*EulerGamma)/(1 + n)^3 +
	(6*EulerGamma)/(1 + n)^2 - (3*EulerGamma^2)/(1 + n)^2 +
	(3*EulerGamma^2)/(1 + n) - EulerGamma^3/(1 + n) - Pi^2/(2*n) -
	Pi^2/(2*(1 + n)^2) + Pi^2/(2*(1 + n)) -
	(EulerGamma*Pi^2)/(2*(1 + n)) - (6*PolyGamma[0, n])/n^2 -
	(6*EulerGamma*PolyGamma[0, n])/n -
	(6*PolyGamma[0, n])/(1 + n)^3 + (6*PolyGamma[0, n])/(1 + n)^2 -
	(6*EulerGamma*PolyGamma[0, n])/(1 + n)^2 +
	(6*EulerGamma*PolyGamma[0, n])/(1 + n) -
	(3*EulerGamma^2*PolyGamma[0, n])/(1 + n) -
	(Pi^2*PolyGamma[0, n])/(2*(1 + n)) - (3*PolyGamma[0, n]^2)/n -
	(3*PolyGamma[0, n]^2)/(1 + n)^2 +
	(3*PolyGamma[0, n]^2)/(1 + n) -
	(3*EulerGamma*PolyGamma[0, n]^2)/(1 + n) -
	PolyGamma[0, n]^3/(1 + n) + (3*PolyGamma[1, n])/n +
	(3*PolyGamma[1, n])/(1 + n)^2 - (3*PolyGamma[1, n])/(1 + n) +
	(3*EulerGamma*PolyGamma[1, n])/(1 + n) +
	(3*PolyGamma[0, n]*PolyGamma[1, n])/(1 + n) -
	PolyGamma[2, n]/(1 + n) - (2*Zeta[3])/(1 + n)
,

(* w9 *)
(*I145I*)
(*X*)iT[x_^n_?mcheck PlusDistribution[Log[1-x_]^2/(1-x_)],
				{x_, 0, 1}] :>
	-2 SumS[1,1,1,n],

(* w10 *)
(*I146I*)
(*X*)iT[x_^n_?mcheck Log[x_]^p_Integer?Positive,
				{x_, 0, 1}] :> (-1)^p*(1 + n)^(-1 - p)*Gamma[1 + p],

(* w11 *)
(*I147I*)
(*X*)iT[x_^n_?mcheck Log[x_]^2/(1-x_), {x_, 0, 1}] :>
				2 Zeta[3] - 2 SumS[3, n],
(*I148I*)
(*X*)iT[x_^n_?mcheck Log[x_]^2 PlusDistribution[1/(1-x_)],
		{x_, 0, 1}] :> 2 Zeta[3] - 2 SumS[3, n],

(* w12 *)
(*I149I*)
(*X*)iT[x_^n_?mcheck Log[x_]^2/(1+x_), {x_, 0, 1}] :> - (-1)^(n+1) *
		(2 SumT[3, n] + 3/2 Zeta[3]),

(* w13 *)
(*I150I*)
(*X*)iT[x_^n_?mcheck Log[x_] Log[1+x_], {x_, 0, 1}] :>
			(-1)^(n+1) * (
		1/(n+1)^2 SumT[1,n+1] + 1/(n+1) SumT[2,n+1] + 1/(2 (n+1)) Zeta2 +
		1/(n+1)^2 Log[2] ) - 1/(n+1)^2 Log[2],

(*  w14 *)
(*I151I*)
(*X*)iT[x_^n_?mcheck Log[x_] Log[1+x_]/(1+x_), {x_, 0, 1}] :>
			- (-1)^(n+1) * (
		Log[2] (SumS[2,n] - SumT[2,n]) + SumT[2,1,n] + SumT[1,2,n] +
		1/2 Zeta2 SumS[1,n] - 1/8 Zeta[3]),

(* w15 *)
(*I152I*)
(*X*)iT[x_^n_?mcheck PolyLog[2, x_], {x_, 0, 1}] :>
		1/(n+1) Zeta2 - 1/(n+1)^2 SumS[1,n+1],

(* w16 *)
(*I153I*)
(*X*)iT[x_^n_?mcheck PolyLog[2, x_]/(1-x_), {x_, 0, 1}] :>
		SumS[2,1,n] - Zeta2 SumS[1,n] - 2 Zeta[3],

(* w17 *)
(*I154I*)
(*X*)iT[x_^n_?mcheck PolyLog[2, 1-x_], {x_, 0, 1}] :>
		1/(n+1) Zeta2 -1/(n+1) SumS[2,n+1],

(* w18 *)
(*I155I*)
(*X*)iT[x_^n_?mcheck PolyLog[2, 1-x_]/(1-x_), {x_, 0, 1}] :>
		SumS[1,2,n] - Zeta2 SumS[1,n] + Zeta[3],
(*I156I*)
(*X*)iT[x_^n_?mcheck PolyLog[2, 1-x_] PlusDistribution[1/(1-x_)],
		{x_, 0, 1}] :> SumS[1,2,n] - Zeta2 SumS[1,n] + Zeta[3],

(* w19 *)
(*I157I*)
(*X*)iT[x_^n_?mcheck PolyLog[2, -x_], {x_, 0, 1}] :>
		(-1)^(n+1) ( - 1/(n+1)^2 Log[2] - 1/(n+1)^2 SumT[1,n+1] ) -
		1/(2 (n+1)) Zeta2 + 1/(n+1)^2 Log[2],

(* w20 *)
(*I158I*)
(*X*)iT[x_^n_?mcheck PolyLog[2, -x_]/(1+x_), {x_, 0, 1}] :>
		- (-1)^(n+1) ( -1/2 Zeta2 SumT[1,n] - Log[2] (SumS[2,n] - SumT[2,n]) -
			SumT[2,1,n] + 1/4 Zeta[3] - 1/2 Zeta2 Log[2]),

(* w21 *)
(*I159I*)
(*X*)iT[x_^n_?mcheck Log[x_] Log[1-x_], {x_, 0, 1}] :>
		-1/(n+1) Zeta2 + 1/(n+1) SumS[2,n+1] + 1/(n+1)^2 SumS[1,n+1],

(* special case of w22 *)
(*I160I*)
(*X*)iT[Log[x_] Log[1-x_]/(1-x_), {x_, 0, 1}] :> Zeta[3],
(*I161I*)
(*X*)iT[Log[x_] Log[1-x_] PlusDistribution[1/(1-x_)],
		{x_, 0, 1}] :> Zeta[3],
(* w22 *)
(*I162I*)
(*X*)iT[x_^n_?mcheck Log[x_] Log[1-x_]/(1-x_), {x_, 0, 1}] :>
		Zeta2 SumS[1,n] - SumS[1,2,n] - SumS[2,1,n] + Zeta[3],
(*I163I*)
(*X*)iT[x_^n_?mcheck Log[x_] Log[1-x_] PlusDistribution[1/(1-x_)],
		{x_, 0, 1}] :>
		Zeta2 SumS[1,n] - SumS[1,2,n] - SumS[2,1,n] + Zeta[3],

(* w23 *)
(*I164I*)
(*X*)iT[x_^n_?mcheck Log[x_]/(1+x_), {x_, 0, 1}] :>
		(-1)^(n+1) ( 1/2 Zeta2 + SumT[2, n]),

(* w24, done by M. *)
(*I164aI*)
(*X*)iT[x_^n_?mcheck PolyLog[3,x_], {x_,0,1}]:>
n^(-1) + (1 + n)^(-4) - (1 + n)^(-3) - (1 + n)^(-2) - (1 + n)^(-1) -
	Zeta2/(1 + n)^2 + SumS[1, -1 + n]/(1 + n)^3 + Zeta[3]/(1 + n),

(* w25, done by R.M. *)
(*I164bI*)
(*X*)iT[x_^n_?mcheck PolyLog[3,1-x_],{x_,0,1}]:>
(Zeta[3]/(n+1)-Zeta2 (n^(-1)+(1+n)^(-2)-(1+n)^(-1)+
SumS[1,-1+n]/(1+n)) +(SumS[1,2,n+1]/(n+1) )),
(* =
(Gamma[1 + n]*HypergeometricPFQ[{1, 1, 1, 1}, {2, 2, 3 + n}, 1]
)/Gamma[3 + n]
*)

(*NN*)(* w27 *)
(*X*)iT[x_^n_?mcheck Log[1+x_], {x_,0,1}]:>
(2*Log[2] + PolyGamma[0, (2 + n)/2] -
	PolyGamma[0, (3 + n)/2])/(2*(1 + n)),

(*NN*)(* w28  (4.2.11) of Devoto & Duke*)
(*X*)iT[x_^n_?mcheck Log[x_]^2 Log[1-x_], {x_,0,1}]:>
2/(n+1)(Zeta[3] + Zeta2/(n+1)-SumS[1,n+1]/(n+1)^2-
SumS[2,n+1]/(n+1)-SumS[3,n+1])
,
(*NN*)(* w29  (4.3.3) of Devoto & Duke*)
(*X*)iT[x_^n_?mcheck Log[x_] PolyLog[2,x_], {x_,0,1}]:>
1/(n+1)^2 (-2 Zeta2+2 SumS[1,n+1]/(n+1)+SumS[2,n+1])
,
(*NN*)(* w30  (4.3.4) of Devoto & Duke*)
(*X*)iT[x_^n_?mcheck Log[1-x_] PolyLog[2,x_], {x_,0,1}]:>
1/(n+1)(1-2Zeta[3]-Zeta2 SumS[1,n+1]+2/(n+1)SumS[1,1,n+1]+
SumS[2,1,n+1]-1)
,
(*NN*)(*w31 *)
(*X*)iT[Log[1 - (x_)]*(x_)^n_?mcheck*PolyLog[2, 1 - (x_)], {x_, 0, 1}] :>
	4/n^3 + 6/(1 + n)^4 - 4/(1 + n)^3 - (2*Zeta2)/n - (3*Zeta2)/(1 + n)^2 +
	(2*Zeta2)/(1 + n) + (2*SumS[1, -1 + n])/n^2 + (4*SumS[1, -1 + n])/(1 + n)^3 -
	(2*SumS[1, -1 + n])/(1 + n)^2 - (2*Zeta2*SumS[1, -1 + n])/(1 + n) +
	SumS[1, -1 + n]^2/(1 + n)^2 - (Zeta2*SumS[1, n])/(1 + n) +
	(Zeta2*SumS[1, 1 + n])/(1 + n) + (2*SumS[2, -1 + n])/n +
	(3*SumS[2, -1 + n])/(1 + n)^2 - (2*SumS[2, -1 + n])/(1 + n) +
	(2*SumS[1, -1 + n]*SumS[2, -1 + n])/(1 + n) + (2*SumS[3, -1 + n])/(1 + n) -
	(2*SumS[1, 1, 1 + n])/(1 + n)^2 - SumS[2, 1, 1 + n]/(1 + n)
,
(*NN*)(*w31 *)
(*X*)iT[Log[x_]*(x_)^n_?mcheck*PolyLog[2, 1 - (x_)], {x_, 0, 1}] :>
	-(Zeta2/(1 + n)^2) + SumS[2, 1 + n]/(1 + n)^2 +
	(2*SumS[3, 1 + n])/(1 + n) - (2*Zeta[3])/(1 + n)
,
(*I165I*)
(*X*)iT[(Log[1-z_/x_] - Log[1-z_])/(1-x_), {x_, z_/;!NumberQ[z], 1}] :>
			- Zeta2 + PolyLog[2, 1-z],

(* ****************************************** *)
(*   Table of Integrals  (W. v. Neerven) *)
(* ****************************************** *)

(*I166I*)
(*X*)iT[y_ Log[1-y_],{y_,x_/;!NumberQ[x],1}] :>
	(-1 + x)/2 + (-1 + x^2)/4 + ((1 - x^2)*Log[1 - x])/2,

(*I167I*)
(*X*)iT[Log[(x_) - (z_)]*(x_), {x_, z_/;!NumberQ[z], 1}] :>
	(1 - z)/2 - (3*(1 - z^2))/4 + ((1 - z^2)*Log[1 - z])/2,

(*I168I*)
(*X*)iT[Log[x_]*(x_), {x_, z_/;!NumberQ[z], 1}] :>
	(-1 + z^2)/4 - (z^2*Log[z])/2,

(*I169I*)
(*X*)iT[Log[x_]^2*(x_), {x_, z_/;!NumberQ[z], 1}] :>
	(1 - z^2)/4 + (z^2*Log[z])/2 - (z^2*Log[z]^2)/2,

(* *)
(*I170I*)
(*X*)iT[Log[x_]*Log[(x_) - (z_)]*(x_), {x_, z_/;!NumberQ[z], 1}] :>
	(3*(1 - z)*z)/4 + (1 - z^2)/4 - ((1 - z^2)*Log[1 - z])/4 +
		(3*z^2*Log[z])/4 - (z^2*Log[1 - z]*Log[z])/2 - (z^2*Log[z]^2)/4 -
		(z^2*PolyLog[2, 1 - z])/2,

(*I171I*)
(*X*)iT[Log[1 - (x_)]*Log[(x_) - (z_)]*(x_),
	{x_, z_/;!NumberQ[z], 1}] :>
	((1 - z^2)*(2 - Zeta2))/2 - (1 - z^2)*Log[1 - z] +
		((1 - z^2)*Log[1 - z]^2)/2,

(*I172I*)
(*X*)iT[Log[1 - (x_)]^2*(x_), {x_, z_/;!NumberQ[z], 1}] :>
	(3*(1 - z))/2 + (1 - z^2)/4 - ((1 - z)*(3 + z)*Log[1 - z])/2 +
		((1 - z^2)*Log[1 - z]^2)/2,

(*I173I*)
(*X*)iT[Log[1 - (x_)]*Log[x_]*(x_), {x_, z_/;!NumberQ[z], 1}] :>
	(3*(1 - z))/4 + (1 - z^2)/4 - ((1 - z^2)*Log[1 - z])/4 +
		(z*(2 + z)*Log[z])/4 - (z^2*Log[1 - z]*Log[z])/2 - PolyLog[2, 1 - z]/2,

(* page 2 *)
(*I174I*)
(*X*)iT[PolyLog[2, x_], {x_, z_/;!NumberQ[z], 1}] :>
	-((1 - z)*(1 - Zeta2)) + (1 - z)*Log[1 - z] + z*Log[1 - z]*Log[z] +
		z*PolyLog[2, 1 - z],

(*I175I*)
(*X*)iT[PolyLog[2, (z_)/(x_)], {x_, z_/;!NumberQ[z], 1}] :>
	(1 - z)*Zeta2 - (1 - z)*Log[1 - z] - z*Log[z] - Log[1 - z]*Log[z] -
		PolyLog[2, 1 - z],

(*I176I*)
(*X*)iT[PolyLog[2, x_]/(x_), {x_, z_/;!NumberQ[z], 1}] :>
	-PolyLog[3, z] + Zeta[3],

(* interesting , the same as above *)
(*I177I*)
(*X*)iT[PolyLog[2, (z_)/(x_)]/(x_), {x_, z_/;!NumberQ[z], 1}] :>
	-PolyLog[3, z] + Zeta[3],

(*I178I*)
(*X*)iT[Log[x_]*(x_)^2, {x_, z_/;!NumberQ[z], 1}] :>
	(-1 + z^3)/9 - (z^3*Log[z])/3,

(*I179I*)
(*X*)iT[(x_)*PolyLog[2, x_], {x_, z_/;!NumberQ[z], 1}] :>
	(-1 + z)/4 + (-1 + z^2)/8 + ((1 - z^2)*Zeta2)/2 +
		((1 - z^2)*Log[1 - z])/4 + (z^2*Log[1 - z]*Log[z])/2 +
		(z^2*PolyLog[2, 1 - z])/2,


(* 0To1 *)
(* *)
(*I180I*)
(*X*)iT[z_ Log[1-z_] PolyLog[2,1-z_], {z_, 0, 1}] :>
	37/16 - (3*Zeta2)/2,

(*I181I*)
(*X*)iT[z_ Log[z_] PolyLog[2,1-z_], {z_, 0, 1}] :>
	23/16 - Zeta2/4 - Zeta[3],

(*I182I*)
(*X*)iT[Log[1-z_]*PolyLog[2, z_], {z_, 0, 1}] :>
	3 - Zeta2 - 2*Zeta[3],

(*I183I*)
(*X*)iT[z_ Log[1-z_]*PolyLog[2, z_], {z_, 0, 1}] :>
	25/16 - (3*Zeta2)/4 - Zeta[3],

(*I184I*)
(*X*)iT[PolyLog[3,x_], {x_,0,1}] :> 1 - Zeta2 + Zeta[3] ,

(*I185I*)
(*X*)iT[PolyLog[3,1-x_], {x_,0,1}] :> 1 - Zeta2 + Zeta[3] ,

(*I186I*)
(*X*)iT[x_ PolyLog[3,1-x_], {x_,0,1}] :> 13/16 - (3*Zeta2)/4 + Zeta[3]/2,
(* *)

(*I187I*)
(*X*)iT[Log[z_]*PolyLog[2, z_], {z_, 0, 1}] :>
	3 - 2*Zeta2,

(*I188I*)
(*X*)iT[Log[1 - (z_)]^2*Log[z_], {z_, 0, 1}] :>
	2*Zeta2 - 2*(3 - Zeta[3]),

(*I189I*)
(*X*)iT[Log[z_]*PolyLog[2, 1 - (z_)], {z_, 0, 1}] :>
	3 - Zeta2 - 2*Zeta[3],

(*I190I*)
(*X*)iT[PolyLog[3, z_], {z_, 0, 1}] :>
	1 - Zeta2 + Zeta[3],

(*I191I*)
(*X*)iT[Nielsen[1,2, z_], {z_, 0, 1}] :> Zeta[3] -1,

(*I192I*)
(*X*)iT[Nielsen[1,2, 1-z_], {z_, 0, 1}] :> Zeta[3] -1,

(*I192bI*)
(*X*)iT[z_ Nielsen[1,2, 1-z_], {z_, 0, 1}] :> -9/16 +Zeta[3]/2,

(*I193I*)
(*X*)iT[PolyLog[2, z_], {z_, 0, 1}] :> -1 + Zeta2,

(*I194I*)
(*X*)iT[Log[z_]*PolyLog[2, z_], {z_, 0, 1}] :> 3 - 2 Zeta2,

(*I195I*)
(*X*)iT[Log[1 - (z_)]*Log[z_], {z_, 0, 1}] :> 2 - Zeta2,

(*I196I*)
(*X*)iT[Log[1 - (z_)]*PolyLog[2, 1 - (z_)], {z_, 0, 1}] :>3-2 Zeta2,

(*I197I*)
(*X*)iT[Log[z_]^n_?mcheck, {z_, 0, 1}] :> n! (-1)^n,

(*I198I*)
(*X*)iT[Log[1 - (z_)]*Log[z_]^2, {z_, 0, 1}] :>
	-6 + 2 Zeta2 + 2 Zeta[3],

(*I199I*)
(*X*)iT[(z_)*PolyLog[2, z_], {z_, 0, 1}] :> -3/8 + Zeta2/2,

(*I200I*)
(*X*)iT[Log[z_]^2*(z_), {z_, 0, 1}] :> 1/4,

(*I201I*)
(*X*)iT[Log[1 - (z_)]*Log[z_]*(z_), {z_, 0, 1}] :> 1 - Zeta2/2,

(*I202I*)
(*X*)iT[(z_)*PolyLog[3, z_], {z_, 0, 1}] :> 3/16 - Zeta2/4 + Zeta[3]/2,

(* UNCHECKED *)
(*I203I*)
(*X*)iT[Nielsen[1, 2, z_]*(z_), {z_, 0, 1}] :> Zeta[3]/2 - 7/16,

(*I204I*)
(*X*)iT[Log[z_]*(1 - (z_))*PolyLog[2, 1 - (z_)], {z_, 0, 1}] :>
	25/16 - 3/4 Zeta2 - Zeta[3],

(*I205I*)
(*X*)iT[Log[z_]*(z_)*PolyLog[2, z_], {z_, 0, 1}] :> 11/16 - Zeta2/2,

(*I206I*)
(*X*)iT[Log[z_]^3*(z_), {z_, 0, 1}] :> -3/8,

(*I207I*)
(*X*)iT[Log[1 - (z_)]*Log[z_]^2*(z_), {z_, 0, 1}] :>
	-17/8 + Zeta2/2 + Zeta[3],

(*I208I*)
(*X*)iT[Log[1 - (z_)]^2*Log[z_]*(z_), {z_, 0, 1}] :>
	-31/8 + (3*Zeta2)/2 + Zeta[3],

(*I209I*)
(*X*)iT[Log[z_]*Log[1 + (z_)]*(z_), {z_, 0, 1}] :> -1/2 + Zeta2/4,

(*I210I*)
(*X*)iT[Log[z_]*(z_)^n_?mcheck, {z_, 0, 1}] :> -1/(n+1)^2,

(*I211I*)
(*X*)iT[Log[1 - (z_)]*(z_), {z_, 0, 1}] :> -3/4,

(*I212I*)
(*X*)iT[Log[1 - (z_)]^2*(z_), {z_, 0, 1}] :> 7/4,

(*I213I*)
(*X*)iT[PolyLog[2, -(z_)], {z_, 0, 1}] :> -1 - Zeta2/2 + 2*Log[2],

(*I214I*)
(*X*)iT[Log[z_]*Log[1 + (z_)], {z_, 0, 1}] :> 2 - Zeta2/2 - 2*Log[2],

(*I215I*)
(*X*)iT[ 1/(1- t_ x_), {t_,0,1}] :> -(Log[1 - x]/x) /; FreeQ[x,t], (*a1*)

(*I216I*)
(*X*)iT[Log[t_]^2/(1 - t_  x_), {t_, 0, 1}] :>
	(2*PolyLog[3, x])/x /; FreeQ[x,t],

(*I217I*)
(*X*)iT[(Log[1 - t_]*Log[t_])/(1 - t_*x_), {t_, 0, 1}] :>
	(2 PolyLog[3, x] + PolyLog[3,1-x] + 1/2 Log[1-x]^2 Log[x] -
	Zeta[3])/x /; FreeQ[x,t],

(*I218I*)
(*X*)iT[Log[1 - t_]^2/(1 - t_*x_), {t_, 0, 1}] :>
(*
	-2 PolyLog[3, -x/(1-x)]/x
*)
	(-2*Zeta2*Log[1 - x])/x - Log[1 - x]^3/(3*x) +
		(Log[1 - x]^2*Log[x])/x + (2*PolyLog[3, 1 - x])/x +
		(2*PolyLog[3, x])/x - (2*Zeta[3])/x /; FreeQ[x,t],

(*I219I*)
(*X*)iT[Log[t_]^3/(1 - t_*x_), {t_, 0, 1}] :>
	(-6*PolyLog[4, x])/x /; FreeQ[x,t],

(*I220I*)
(*X*)iT[(Log[1 - t_]*Log[t_]^2)/(1 - t_*x_), {t_, 0, 1}] :>
	(2*Nielsen[2, 2, x] + 2*Zeta2*PolyLog[2, x] - 6*PolyLog[4, x] +
		2*Log[1 - x]*(PolyLog[3, x] - Zeta[3]))/x  /; FreeQ[x,t],


(* a9 *)
(*I221I*)
(*X*)iT[Log[t_] Log[1-t_]^2/(1-t_ x_), {t_,0,1}] :>
	(-2*Nielsen[1, 3, x] + 4*Nielsen[2, 2, x] +
			Log[1 - x]^2*(Zeta2 - PolyLog[2, x]) + 2*Zeta2*PolyLog[2, x] +
			Log[1 - x]*(-2*Zeta2 + 2*Nielsen[1, 2, x] + 4*PolyLog[3, x]) -
			6*PolyLog[4, x])/x /; FreeQ[x,t],

(* a10 *)
(*I222I*)
(*X*)iT[Log[1-t_]^3/(1-t_ x_), {t_,0,1}] :>
( (6*PolyLog[4, x/(-1 + x)])/x ) /; FreeQ[x, t],

(*
	(-1/(4*Log[1 - x]^4) - 6*Nielsen[1, 3, x] + 6*Nielsen[2, 2, x] -
			3*Log[1 - x]^2*PolyLog[2, x] +
			6*Log[1 - x]*(-Nielsen[1, 2, x] + PolyLog[3, x]) -
			6*PolyLog[4, x])/x  /; FreeQ[x,t],
*)

(*I223I*)
(*X*)iT[(Log[1 - (z_)]*Log[1 - (y_)*(z_)])/(z_)^2, {z_, 0, 1}] :>
	-((1 - y)*Log[1 - y]^2)/2 + y*(Zeta2 + PolyLog[2, y]) /; FreeQ[y,z],

(*I224I*)
(*X*)iT[PolyLog[2, 1 - (z_)]/(1 - x_*(z_)), {z_, 0, 1}] :>
	(-(Zeta2*Log[1 - x]) + 2*Nielsen[1, 2, x] +
			Log[1 - x]*PolyLog[2, x] - PolyLog[3, x])/x /; FreeQ[x,z],

(*I225I*)
(*X*)iT[1/x_ PolyLog[2, x_/(1-(1-x_)(1-z_))], {x_,0,1}] :>
	-2*Zeta2*Log[z] + (Log[1 - z]*Log[z]^2)/2 +
		Log[z]*PolyLog[2, 1 - z] - PolyLog[3, 1 - z] +
		PolyLog[3, z] /; FreeQ[z,x],

(*I226I*)
(*X*)iT[PlusDistribution[(1 - x_)^(-1)]*
		PolyLog[2, x_/(1 - (1 - x_)*(1 - z_))], {x_, 0, 1}] :>
	Zeta2*Log[z] - (Log[1 - z]*Log[z]^2)/2 - Log[z]*PolyLog[2, 1 - z] +
		PolyLog[3, 1 - z] - PolyLog[3, z] - Zeta[3] /; FreeQ[z,x],

(*I227I*)
(*X*)iT[PlusDistribution[(1 - x_)^(-1)]*
		PolyLog[2, (1 - z_)/(1 - (1 - x_)*z_)], {x_, 0, 1}]:>
	Zeta2*Log[1 - z] - 2*Nielsen[1, 2, z] - Log[1 - z]*PolyLog[2, z] -
		Log[z]*PolyLog[2, z] + 2*PolyLog[3, z]  /; FreeQ[z,x],
(* -(Zeta2*Log[z]) + Log[1 - z]*Log[z]^2 -
		Log[1 - z]*PolyLog[2, 1 - z] + Log[z]*PolyLog[2, 1 - z] +
		2*PolyLog[3, 1 - z] + 2*PolyLog[3, z] - 2*Zeta[3] *)

(*I228I*)
(*X*)iT[(Log[t_]*Log[1 - t_*x_])/(1 - t_*x_), {t_, 0, 1}] :>
	Nielsen[1,2,x]/x /; FreeQ[x,t],

(*I229I*)
(*X*)iT[(Log[1 - t_]*Log[1 - t_*x_])/(1 - t_*x_), {t_, 0, 1}] :>
	(-Log[1 - x]^3/3 - Nielsen[1, 2, x] -
			Log[1 - x]*PolyLog[2, x])/x  /; FreeQ[x,t],

(*I230I*)
(*X*)iT[PolyLog[2, t_]/(1 - t_*x_), {t_, 0, 1}] :>
	(-(Zeta2*Log[1 - x]) - Nielsen[1, 2, x] -
			PolyLog[3, x])/x  /; FreeQ[x,t],

(*I231I*)
(*X*)iT[Log[1 + (y_*(1 - z_))/z_]/y_, {y_, 0, 1}] :>
	1/2 Log[z]^2 + PolyLog[2,1-z] /; FreeQ[z, y],

(*I232I*)
(*X*)iT[(Log[y_]*Log[1 + (y_*(1 - z_))/z_])/y_, {y_, 0, 1}] :>
	Zeta2*Log[z] - (Log[1 - z]*Log[z]^2)/2 + Log[z]^3/6 -
		PolyLog[3, 1 - z] - PolyLog[3, z] + Zeta[3]  /; FreeQ[z, y],

(*I233I*)
(*X*)iT[(Log[1 - (y_)]*Log[1 + ((y_)*(1 - (z_)))/(z_)])/(y_),
		{y_, 0, 1}] :>
	2*Zeta2*Log[z] - Log[1 - z]*Log[z]^2 - Log[z]*PolyLog[2, 1 - z] -
		PolyLog[3, 1 - z] - 2*PolyLog[3, z] + 2*Zeta[3] /; FreeQ[z, y],

(*I234I*)
(*X*)iT[PlusDistribution[(1 - (y_))^(-1)]*
		PolyLog[2, (y_)*(1 - (z_))], {y_, 0, 1}] :>
	-(Zeta2*Log[z]) + (Log[1 - z]*Log[z]^2)/2 +
		Log[z]*PolyLog[2, 1 - z] - PolyLog[3, 1 - z] +
			PolyLog[3, z] - Zeta[3] /; FreeQ[z, y],

(*I235I*)
(*X*)iT[PlusDistribution[(1 - (y_))^(-1)]*
		PolyLog[2, ((y_)*(z_))/((1 - (y_))*(1 - (z_)) + (z_))], {y_, 0, 1}]:>
	2*Zeta2*Log[z] - Log[1 - z]*Log[z]^2 - Log[z]*PolyLog[2, 1 - z] -
		PolyLog[3, 1 - z] - 2*PolyLog[3, z] /; FreeQ[z, y],

(*I236I*)
(*X*)iT[PlusDistribution[(1 - (y_))^(-1)]*
		PolyLog[2, ((y_)*(1 - (z_)))/(1 - (1 - (y_))*(1 - (z_)))], {y_, 0, 1}
						] :>
	Zeta2*Log[z] - (Log[1 - z]*Log[z]^2)/2 - Log[z]*PolyLog[2, 1 - z] -
		PolyLog[3, 1 - z] - PolyLog[3, z] + Zeta[3] /; FreeQ[z, y],

(*I237I*)
(*X*)iT[PlusDistribution[(1 - (y_))^(-1)]*
		PolyLog[2, (y_)/((y_)*(1 - (z_)) + (z_))], {y_, 0, 1}]:>
	Zeta2*Log[z] - (Log[1 - z]*Log[z]^2)/2 - Log[z]*PolyLog[2, 1 - z] +
		PolyLog[3, 1 - z] - PolyLog[3, z] - Zeta[3] /; FreeQ[z, y],

(*I238I*)
(*X*)iT[PolyLog[2, ((y_)*(z_))/(1 - (y_)*(1 - (z_)))]/(y_),
		{y_, 0, 1}] :>
	-(Zeta2*Log[z]) + Log[1 - z]*Log[z]^2 + Log[z]*PolyLog[2, 1 - z] +
		PolyLog[3, 1 - z] + 2*PolyLog[3, z] - Zeta[3] /; FreeQ[z, y],

(*I239I*)
(*X*)iT[PolyLog[2, ((1 - (y_))*(1 - (z_)))/(1 - (y_)*(1 - (z_)))
										]/(1 - (y_)), {y_, 0, 1}
						] :>
	-2*Zeta2*Log[z] + Log[1 - z]*Log[z]^2 + Log[z]*PolyLog[2, 1 - z] +
		PolyLog[3, 1 - z] + 2*PolyLog[3, z] - 2*Zeta[3] /; FreeQ[z, y],

(* numerically somewhat UNCHECKABLE, but seems to be right *)
(*I240I*)
(*X*)iT[(PolyLog[2, y_] -
			PolyLog[2, ((y_)*(z_))/(1 - (y_)*(1 - (z_)))])/(1 - (y_)),
						{y_, 0, 1}] :>
	-2*Zeta2*Log[z] + Log[1 - z]*Log[z]^2 + Log[z]*PolyLog[2, 1 - z] +
		PolyLog[3, 1 - z] + 2*PolyLog[3, z] - 2*Zeta[3] /; FreeQ[z, y],

(*I241I*)
(*X*)iT[Log[1 + ((y_)*(1 - (z_)))/(z_)]^2/(y_), {y_, 0, 1}]:>
(* -1/3 Log[z]^3 + 2 Nielsen[1,2,z] *)
	2*Zeta2*Log[z] - Log[1 - z]*Log[z]^2 - Log[z]^3/3 -
		2*Log[z]*PolyLog[2, 1 - z] - 2*PolyLog[3, z] + 2*Zeta[3] /; FreeQ[z, y],

(*I242I*)
(*X*)iT[(-PolyLog[2, 1 - (z_)] +
			PolyLog[2, (1 - (y_))*(1 - (z_))])/(y_), {y_, 0, 1}] :>
	-(Zeta2*Log[z]) + (Log[1 - z]*Log[z]^2)/2 +
		Log[z]*PolyLog[2, 1 - z] - PolyLog[3, 1 - z] + PolyLog[3, z] -
			Zeta[3]  /; FreeQ[z, y],

(*I243I*)
(*X*)iT[PlusDistribution[(1 - (y_))^(-1)]*
		(-PolyLog[2, 1 - (z_)] + PolyLog[2, (y_)*(1 - (z_))]), {y_, 0, 1}]:>
	-(Zeta2*Log[z]) + (Log[1 - z]*Log[z]^2)/2 +
		Log[z]*PolyLog[2, 1 - z] - PolyLog[3, 1 - z] + PolyLog[3, z] -
			Zeta[3] /; FreeQ[z, y],

(*I244I*)
(*X*)iT[Log[1 - (y_)*(1 - (z_))], {y_, 0, 1}] :>
	-1 - (z*Log[z])/(1 - z) /; FreeQ[z, y],

(*I245I*)
(*X*)iT[Log[y_]*Log[1 - (y_)*(1 - (z_))], {y_, 0, 1}] :>
	2 + (z*Log[z])/(1 - z) - PolyLog[2, 1 - z]/(1 - z) /; FreeQ[z, y],

(*I246I*)
(*X*)iT[Log[1 - (y_)]*Log[1 - (y_)*(1 - (z_))], {y_, 0, 1}] :>
	2 + (z*Log[z])/(1 - z) - (z*Log[z]^2)/(2*(1 - z)) -
		(z*PolyLog[2, 1 - z])/(1 - z) /; FreeQ[z, y],

(*I247I*)
(*X*)iT[Log[1 - (y_)*(1 - (z_))]^2, {y_, 0, 1}]:>
	2 + (2*z*Log[z])/(1 - z) - (z*Log[z]^2)/(1 - z) /; FreeQ[z, y],

(*I248I*)
(*X*)iT[PolyLog[2, (y_)*(1 - (z_))], {y_, 0, 1}] :>
	-1 - (z*Log[z])/(1 - z) + PolyLog[2, 1 - z] /; FreeQ[z, y],

(*I249I*)
(*X*)iT[PolyLog[2, (y_)*(1 - (z_))]/(1 - (y_)*(1 - (z_))),
		{y_, 0, 1}] :>
	(-2*Zeta2*Log[z] + Log[1 - z]*Log[z]^2 + Log[z]*PolyLog[2, 1 - z] +
			2*PolyLog[3, z] - 2*Zeta[3])/(1 - z) /; FreeQ[z, y],

(*I250I*)
(*X*)iT[PolyLog[2,
		((1 - (y_))*(1 - (z_)))/(1 - (y_)*(1 - (z_)))], {y_, 0, 1}] :>
	-(z*Log[z]^2)/(2*(1 - z)) + PolyLog[2, 1 - z] /; FreeQ[z, y],

(*I251I*)
(*X*)iT[(1 - (y_)*(1 - (z_)))^(-2), {y_, 0, 1}]:> 1/z /; FreeQ[z, y],

(*I252I*)
(*X*)iT[Log[y_]/(1 - (y_)*(1 - (z_)))^2, {y_, 0, 1}] :>
	Log[z]/(1 - z) /; FreeQ[z, y],

(*I253I*)
(*X*)iT[Log[1 - (y_)]^2/(1 - (y_)*(1 - (z_)))^2, {y_, 0, 1}] :>
	(2*(Log[z]^2/2 + PolyLog[2, 1 - z]))/((1 - z)*z) /; FreeQ[z, y],

(*I254I*)
(*X*)iT[(Log[1 - (y_)]*Log[y_])/(1 - (y_)*(1 - (z_)))^2,
		{y_, 0, 1}] :>
	-(Zeta2/z) + Log[z]^2/(2*(1 - z)) +
		((1 + z)*PolyLog[2, 1 - z])/((1 - z)*z) /; FreeQ[z, y],

(*I255I*)
(*X*)iT[PolyLog[2, (y_)*(1 - (z_))]/(1 - (y_)*(1 - (z_)))^2,
		{y_, 0, 1}] :>
	-Log[z]^2/(2*(1 - z)) + PolyLog[2, 1 - z]/z /; FreeQ[z, y],

(*I256I*)
(*X*)iT[Log[1 - (y_)*(1 - (z_))]/(1 - (y_)*(1 - (z_)))^2,
		{y_, 0, 1}] :> z^(-1) + Log[z]/((1 - z)*z) /; FreeQ[z, y],

(*I257I*)
(*X*)iT[(y_)*PolyLog[2, (y_)*(1 - (z_))], {y_, 0, 1}] :>
	-(3 - z)/(8*(1 - z)) + ((-2*z + z^2)*Log[z])/(4*(1 - z)^2) +
		PolyLog[2, 1 - z]/2 /; FreeQ[z, y],

(*I258I*)
(*X*)iT[PolyLog[2, y_]/(1 - (y_)*(1 - (z_)))^2, {y_, 0, 1}] :>
	Zeta2/z - (Log[z]^2/2 + PolyLog[2, 1 - z])/(1 - z) /; FreeQ[z, y],

(*I259I*)
(*X*)iT[Log[1 - (y_)*(1 - (z_))]^2*(y_), {y_, 0, 1}] :>
	(7 - z)/(4*(1 - z)) + ((4 - z)*z*Log[z])/(2*(1 - z)^2) -
		((2 - z)*z*Log[z]^2)/(2*(1 - z)^2) /; FreeQ[z, y],

(*I260I*)
(*X*)iT[Log[y_]*Log[1 - (y_)*(1 - (z_))]*(y_), {y_, 0, 1}] :>
	(4 - z)/(4*(1 - z)) + ((2 - z)*z*Log[z])/(4*(1 - z)^2) -
		PolyLog[2, 1 - z]/(2*(1 - z)^2) /; FreeQ[z, y],

(*I261I*)
(*X*)iT[Log[1 - (y_)]*Log[1 - (y_)*(1 - (z_))]*(y_), {y_, 0, 1}] :>
	(7/4 - z)/(1 - z) + (3*(4/3 - z)*z*Log[z])/(4*(1 - z)^2) -
		((2 - z)*z*Log[z]^2)/(4*(1 - z)^2) -
		((2 - z)*z*PolyLog[2, 1 - z])/(2*(1 - z)^2) /; FreeQ[z, y],

(*I262I*)
(*X*)iT[PolyLog[2, -(((1 - (y_))*(1 - (z_)))/(z_))
										]/(1 - (y_)*(1 - (z_))), {y_, 0, 1}] :>
	(2*Zeta2*Log[z] - Log[1 - z]*Log[z]^2 + Log[z]^3/6 -
		Log[z]*PolyLog[2, 1 - z] - 2*PolyLog[3, z] + 2*Zeta[3]
	)/(1-z) /; FreeQ[z, y],

(*I263I*)
(*X*)iT[(y_)*
		PolyLog[2, ((y_)*(1 - (z_)))/(1 - (1 - (y_))*(1 - (z_)))],
		{y_, 0, 1}] :>
	z/(2*(1 - z)) + (z*Log[z])/(2*(1 - z)^2) +
		(z^2*Log[z]^2)/(4*(1 - z)^2) + PolyLog[2, 1 - z]/2 /; FreeQ[z, y],

(*I264I*)
(*X*)iT[Log[(1 - (1 - (y_))*(1 - (z_)))/(z_)]*(y_), {y_, 0, 1}] :>
	(1-7*z) /(4*(1 - z)) + ((1 - 4*z)*Log[z])/(2*(1 - z)^2) +
		((1 - 2*z)*Log[z]^2)/(2*(1 - z)^2) /; FreeQ[z, y],

(*I265I*)
(*X*)iT[PolyLog[2, (1 - (y_))/(1 - (y_)*(1 - (z_)))]/
	(1 - (y_)*(1 - (z_))), {y_, 0, 1}] :>
	(-2*Zeta2*Log[z] + (Log[1 - z]*Log[z]^2)/2 +
		Log[z]*PolyLog[2, 1 - z] - PolyLog[3, 1 - z] + PolyLog[3, z] - Zeta[3])/
		(1 - z) /; FreeQ[z, y],

(*I266I*)
(*X*)iT[PolyLog[2, (1 - (y_))/(1 - (y_)*(1 - (z_)))], {y_, 0, 1}] :>
	Zeta2 - (z*(Log[z]^2/2 + PolyLog[2, 1 - z]))/(1 - z) /; FreeQ[z, y],

(*I267I*)
(*X*)iT[Log[1 - (y_)*(1 - (z_))]^2/(1 - (y_)*(1 - (z_)))^2,
	{y_, 0, 1}] :>
	2/z + (2*Log[z])/((1 - z)*z) + Log[z]^2/((1 - z)*z) /; FreeQ[z, y],

(*I268I*)
(*X*)iT[(Log[y_]*Log[1 - (y_)*(1 - (z_))])/(1 - (y_)*(1 - (z_)))^2,
						{y_, 0, 1}] :>
	(Log[z] + Log[z]^2/2 + PolyLog[2, 1 - z])/(1 - z) /; FreeQ[z, y],

(*I269I*)
(*X*)iT[(Log[1 - (y_)]*Log[1 - (y_)*(1 - (z_))])/
	(1 - (y_)*(1 - (z_)))^2, {y_, 0, 1}] :>
	(Log[z] + Log[z]^2 + PolyLog[2, 1 - z])/((1 - z)*z) /; FreeQ[z, y],


(*I270I*)
(*X*)iT[(1 - (y_)*(1 - (z_)))^(-3), {y_, 0, 1}] :>
	1/(2*z^2) + 1/(2*z) /; FreeQ[z, y],

(*I271I*)
(*X*)iT[Log[1 - (y_)*(1 - (z_))]/(1 - (y_)*(1 - (z_)))^3,
	{y_, 0, 1}] :>
	1/(4*z^2) + 1/(4*z) + Log[z]/(2*(1 - z)*z^2) /; FreeQ[z, y],

(*I272I*)
(*X*)iT[Log[1 - (y_)*(1 - (z_))]^2/(1 - (y_)*(1 - (z_)))^3,
	{y_, 0, 1}] :>
	1/(4*z^2) + 1/(4*z) + Log[z]/(2*(1 - z)*z^2) +
		Log[z]^2/(2*(1 - z)*z^2) /; FreeQ[z, y],

(*I273I*)
(*X*)iT[PolyLog[2, -((1 - (y_))/((y_)*(z_)))]/
						(1 - (y_)*(1 - (z_))), {y_, 0, 1}] :>
	(3*Zeta2*Log[z] - Log[1 - z]*Log[z]^2 + Log[z]^3/6 -
			Log[z]*PolyLog[2, 1 - z] - 2*PolyLog[3, z] +
				2*Zeta[3])/(1 - z) /; FreeQ[z, y],

(*I274I*)
(*X*)iT[PolyLog[2, -(((1 - (y_))*(z_))/(y_))], {y_, 0, 1}] :>
	-Zeta2 + PolyLog[2, 1 - z] /; FreeQ[z, y],

(*I275I*)
(*X*)iT[Log[y_]/(1 - (y_)*(1 - (z_)))^3, {y_, 0, 1}] :>
	-1/(2*z) + Log[z]/(2*(1 - z)) /; FreeQ[z, y],

(*I276I*)
(*X*)iT[(Log[y_]*Log[1 - (y_)*(1 - (z_))])/(1 - (y_)*(1 - (z_)))^3,
						{y_, 0, 1}] :>
	-3/(4*z) - Log[z]/(2*(1 - z)*z) +
		(Log[z]/4 + Log[z]^2/4 + PolyLog[2, 1 - z]/2)/(1 - z) /; FreeQ[z, y],

(*I277I*)
(*X*)iT[(1 - (y_)*(1 - (z_)))^(-4), {y_, 0, 1}] :>
	1/(3*z^3) + 1/(3*z^2) + 1/(3*z) /; FreeQ[z, y],

(*I278I*)
(*X*)iT[Log[1 - (y_)*(1 - (z_))]/(1 - (y_)*(1 - (z_)))^4, {y_, 0, 1}] :>
	1/(9*z^3) + 1/(9*z^2) + 1/(9*z) + Log[z]/(3*(1 - z)*z^3) /; FreeQ[z, y],

(*I279I*)
(*X*)iT[Log[1 - (y_)*(1 - (z_))]^2/(1 - (y_)*(1 - (z_)))^4,
						{y_, 0, 1}] :>
	2/(27*z^3) + 2/(27*z^2) + 2/(27*z) + (2*Log[z])/(9*(1 - z)*z^3) +
		Log[z]^2/(3*(1 - z)*z^3) /; FreeQ[z, y],

(*I280I*)
(*X*)iT[Log[y_]/(1 - (y_)*(1 - (z_)))^4, {y_, 0, 1}] :>
	-1/(6*z^2) - 1/(2*z) + Log[z]/(3*(1 - z)) /; FreeQ[z, y],

(*I281I*)
(*X*)iT[(Log[y_]*Log[1 - (y_)*(1 - (z_))])/(1 - (y_)*(1 - (z_)))^4,
						{y_, 0, 1}] :>
	-5/(36*z^2) - 7/(12*z) - Log[z]/(6*(1 - z)*z^2) -
		Log[z]/(3*(1 - z)*z) + (Log[z]/9 + Log[z]^2/6 +
	PolyLog[2, 1 - z]/3)/(1 - z) /; FreeQ[z, y],

(*I282I*)
(*X*)iT[Log[1 - (y_)]/(1 - (y_)*(1 - (z_)))^3, {y_, 0, 1}] :>
	-1/(2*z^2) + Log[z]/(2*(1 - z)*z^2) /; FreeQ[z, y],

(*I283I*)
(*X*)iT[(Log[1 - (y_)]*Log[1 - (y_)*(1 - (z_))])/
	(1 - (y_)*(1 - (z_)))^3, {y_, 0, 1}] :>
	(-3/2 + (3*z)/2 - Log[z]/2 + Log[z]^2 + PolyLog[2, 1 - z])/
		(2*(1 - z)*z^2) /; FreeQ[z, y],

(*I284I*)
(*X*)iT[PolyLog[2, -(((1 - (y_))*(1 - (z_)))/(z_))], {y_, 0, 1}] :>
	-1 - Log[z]/(1 - z) - Log[z]^2/2 - PolyLog[2, 1 - z] /; FreeQ[z, y],

(*I285I*)
(*X*)iT[PolyLog[2, -(((1 - (y_))*(1 - (z_)))/(z_))]/
	(1 - (y_)*(1 - (z_)))^2, {y_, 0, 1}] :>
	Log[z]^2/(2*(1 - z)) - PolyLog[2, 1 - z]/z /; FreeQ[z, y],

(*I286I*)
(*X*)iT[1/(1 - (y_)*(z_))^2, {y_, 0, 1}] :> 1/(1-z) /; FreeQ[z, y],

(*I287I*)
(*X*)iT[(Log[y_]*(z_))/(1 - (y_))^2, {y_, 0, 1}] :>
			Log[1 - z]/z /; FreeQ[z, y],

(*I288I*)
(*X*)iT[Log[1 - (y_)]/(1 - (y_)*(z_))^2, {y_, 0, 1}] :>
	Log[1 - z]/((1 - z)*z) /; FreeQ[z, y],

(*I289I*)
(*X*)iT[Log[y_]^2/(1 - (y_)*(z_))^2, {y_, 0, 1}] :>
	(2*PolyLog[2, z])/z /; FreeQ[z, y],

(*I290I*)
(*X*)iT[Log[1 - (y_)]^2/(1 - (y_)*(z_))^2, {y_, 0, 1}] :>
	(2*(Log[1 - z]^2/2 + PolyLog[2, z]))/((1 - z)*z) /; FreeQ[z, y],

(*I291I*)
(*X*)iT[(Log[1 - (y_)]*Log[y_])/(1 - (y_)*(z_))^2, {y_, 0, 1}] :>
	(Zeta2 + Log[1 - z]^2/2 + PolyLog[2, z] +
			(-Zeta2 + PolyLog[2, z])/(1 - z))/z /; FreeQ[z, y],

(*I292I*)
(*X*)iT[(1 - (y_)*(z_))^(-3), {y_, 0, 1}] :>
	1/(2*(1 - z)^2) + 1/(2*(1 - z)) /; FreeQ[z, y],

(*I293I*)
(*X*)iT[Log[y_]/(1 - (y_)*(z_))^3, {y_, 0, 1}] :>
	-1/(2*(1 - z)) + Log[1 - z]/(2*z) /; FreeQ[z, y],

(*I294I*)
(*X*)iT[Log[1 - (y_)]/(1 - (y_)*(z_))^3, {y_, 0, 1}] :>
	-1/(2*(1 - z)^2) + Log[1 - z]/(2*(1 - z)^2*z) /; FreeQ[z, y],

(*I295I*)
(*X*)iT[Log[y_]^2/(1 - (y_)*(z_))^3, {y_, 0, 1}] :>
	(-Log[1 - z] + PolyLog[2, z])/z /; FreeQ[z, y],

(*I296I*)
(*X*)iT[Log[1 - (y_)]^2/(1 - (y_)*(z_))^3, {y_, 0, 1}] :>
	(-Log[1 - z] + Log[1 - z]^2/2 +
		PolyLog[2, z])/((1 - z)^2*z) /; FreeQ[z, y],

(*I297I*)
(*X*)iT[(Log[1 - (y_)]*Log[y_])/(1 - (y_)*(z_))^3, {y_, 0, 1}] :>
	(Zeta2/2 - Zeta2/(2*(1 - z)^2) - Log[1 - z]/(1 - z) +
		Log[1 - z]^2/4 + PolyLog[2, z]/2 +
			PolyLog[2, z]/(2*(1 - z)^2))/z  /; FreeQ[z, y],

(*I298I*)
(*X*)iT[(1 - (y_)*(z_))^(-4), {y_, 0, 1}] :>
	1/(3*(1 - z)^3) + 1/(3*(1 - z)^2) + 1/(3*(1 - z)) /; FreeQ[z, y],

(*I299I*)
(*X*)iT[Log[y_]/(1 - (y_)*(z_))^4, {y_, 0, 1}] :>
	-(4 - 3*z)/(6*(1 - z)^2) + Log[1 - z]/(3*z) /; FreeQ[z, y],

(*I300I*)
(*X*)iT[Log[1 - (y_)]/(1 - (y_)*(z_))^4, {y_, 0, 1}] :>
	-(4 - z)/(6*(1 - z)^3) + Log[1 - z]/(3*(1 - z)^3*z) /; FreeQ[z, y],

(*I301I*)
(*X*)iT[Log[y_]^2/(1 - (y_)*(z_))^4, {y_, 0, 1}] :>
	-1/(3*z) + 1/(3*(1 - z)*z) - Log[1 - z]/z +
			(2*PolyLog[2, z])/(3*z) /; FreeQ[z, y],

(*I302I*)
(*X*)iT[Log[1 - (y_)]^2/(1 - (y_)*(z_))^4, {y_, 0, 1}] :>
	(z/(3*(1 - z)^3) - Log[1 - z]/(1 - z)^3 +
			Log[1 - z]^2/(3*(1 - z)^3) +
				(2*PolyLog[2, z])/(3*(1 - z)^3))/z /; FreeQ[z, y],

(*I303I*)
(*X*)iT[(Log[1 - (y_)]*Log[y_])/(1 - (y_)*(z_))^4, {y_, 0, 1}] :>
	(1/(3*(1 - z)^2) - 1/(3*(1 - z)) + ((1 - (1 - z)^(-3))*Zeta2)/3 -
			Log[1 - z]/(2*(1 - z)^2) - Log[1 - z]/(2*(1 - z)) + Log[1 - z]^2/6 +
			((1 + (1 - z)^(-3))*PolyLog[2, z])/3)/z /; FreeQ[z, y],

(*I304I*)
(*X*)iT[Log[1 - (y_)*(1 - (z_))]^2/(1 - (y_)*(1 - (z_)))^2,
						{y_, 0, 1}] :>
	2/z + (2*Log[z])/((1 - z)*z) + Log[z]^2/((1 - z)*z) /; FreeQ[z, y],

(*I305I*)
(*X*)iT[(Log[y_]*Log[1 - (y_)*(1 - (z_))])/(1 - (y_)*(1 - (z_)))^2,
						{y_, 0, 1}] :>
	Log[z]/(1 - z) + Log[z]^2/(2*(1 - z)) +
		PolyLog[2, 1 - z]/(1 - z) /; FreeQ[z, y],

(*I306I*)
(*X*)iT[Log[1 - (y_)*(1 - (z_))]/(1 - (y_)*(1 - (z_)))^3, {y_, 0, 1}
						] :> (1 + z)/(4*z^2) + Log[z]/(2*(1 - z)*z^2) /; FreeQ[z, y],

(*I307I*)
(*X*)iT[Log[1 - (y_)*(1 - (z_))]^2/(1 - (y_)*(1 - (z_)))^3,
						{y_, 0, 1}] :>
	(1 + z)/(4*z^2) + Log[z]/(2*(1 - z)*z^2) +
		Log[z]^2/(2*(1 - z)*z^2) /; FreeQ[z, y],

(*I308I*)
(*X*)iT[(Log[y_]*Log[1 - (y_)*(1 - (z_))])/(1 - (y_)*(1 - (z_)))^3,
						{y_, 0, 1}] :>
	-3/(4*z) + Log[z]/(4*(1 - z)) - Log[z]/(2*(1 - z)*z) +
		Log[z]^2/(4*(1 - z)) + PolyLog[2, 1 - z]/(2*(1 - z)) /; FreeQ[z, y],

(*I309I*)
(*X*)iT[Log[1 - (y_)*(1 - (z_))]/(1 - (y_)*(1 - (z_)))^4, {y_, 0, 1}] :>
	1/(9*z^3) + 1/(9*z^2) + 1/(9*z) + Log[z]/(3*(1 - z)*z^3) /; FreeQ[z, y],

(*I310I*)
(*X*)iT[Log[1 - (y_)*(1 - (z_))]^2/(1 - (y_)*(1 - (z_)))^4, {y_, 0, 1}
						] :>
	2/(27*z^3) + 2/(27*z^2) + 2/(27*z) + (2*Log[z])/(9*(1 - z)*z^3) +
		Log[z]^2/(3*(1 - z)*z^3) /; FreeQ[z, y],

(*I311I*)
(*X*)iT[Log[y_]/(1 - (y_)*(1 - (z_)))^4, {y_, 0, 1}] :>
	-1/(6*z^2) - 1/(2*z) + Log[z]/(3*(1 - z)) /; FreeQ[z, y],

(*I312I*)
(*X*)iT[(Log[y_]*Log[1 - (y_)*(1 - (z_))])/(1 - (y_)*(1 - (z_)))^4,
			{y_, 0, 1}] :>
	5/(36*(1 - z)) - 5/(36*(1 - z)*z^2) - 4/(9*z) -
		Log[z]/(6*(1 - z)*z^2) - Log[z]/(3*(1 - z)*z) +
		(Log[z]/9 + Log[z]^2/6 + PolyLog[2, 1 - z]/3)/(1 - z) /; FreeQ[z, y],

(*I313I*)
(*X*)iT[Log[y_]^2/(1 - (y_)*(1 - (z_)))^3, {y_, 0, 1}] :>
	-(Log[z]/(1 - z)) + PolyLog[2, 1 - z]/(1 - z) /; FreeQ[z, y],

(*I314I*)
(*X*)iT[Log[1 - (y_)]^2/(1 - (y_)*(1 - (z_)))^3, {y_, 0, 1}] :>
	(-Log[z] + Log[z]^2/2 + PolyLog[2, 1 - z])/((1 - z)*z^2) /; FreeQ[z, y],

(*I315I*)
(*X*)iT[(Log[1 - (y_)]*Log[y_])/(1 - (y_)*(1 - (z_)))^3, {y_, 0, 1}] :>
	(-1/(2*z^2) - 1/(2*z))*Zeta2 - Log[z]/((1 - z)*z) +
		Log[z]^2/(4*(1 - z)) + (1/(2*(1 - z)) +
			1/(2*(1 - z)*z^2))*PolyLog[2, 1 - z] /; FreeQ[z, y],

(*I316I*)
(*X*)iT[Log[1 + ((y_)*(1 - (z_)))/(z_)]/(1 - (y_)*(1 - (z_))),
						{y_, 0, 1}] :>
	(-Zeta2 + Log[z]^2 - 2*Log[z]*Log[1 + z] -
			2*PolyLog[2, -z])/(1 - z) /; FreeQ[z, y],

(*I317I*)
(*X*)iT[(Log[1 - (y_)*(1 - (z_))]*Log[1 + ((y_)*(1 - (z_)))/(z_)])/
	(y_), {y_, 0, 1}] :>
	Zeta2*Log[z] - Log[1 - z]*Log[z]^2 - 2*Log[z]*PolyLog[2, -z] -
		2*Log[z]*PolyLog[2, z] + 4*PolyLog[3, -z] +
			2*PolyLog[3, z] + Zeta[3] /; FreeQ[z, y],

(* UNCHECKED wg. Nielsen[1,2,-z] *)
(*I318I*)
(*X*)iT[Log[1 + ((y_)*(1 - (z_)))/(z_)]^2/(1 - (y_)*(1 - (z_))),
						{y_, 0, 1}] :>
	(2*Zeta2*Log[z] - Log[z]^3 - 2*Zeta2*Log[1 + z] +
		3*Log[z]^2*Log[1 + z] - 2*Log[z]*Log[1 + z]^2 - 4*Nielsen[1, 2, -z] +
		2*Log[z]*PolyLog[2, -z] - 4*Log[1 + z]*PolyLog[2, -z] +
		2*PolyLog[3, -z] + 2*Zeta[3])/(1-z) /; FreeQ[z, y],

(* UNCHECKED wg. Nielsen[1,2,-z] *)
(*I319I*)
(*X*)iT[(Log[y_]*Log[1 + ((y_)*(1 - (z_)))/(z_)])/
	(1 - (y_)*(1 - (z_))), {y_, 0, 1}] :>
	(Log[z]^3/2 - Zeta2*Log[1 + z] - (Log[z]^2*Log[1 + z])/2 -
			Log[z]*Log[1 + z]^2 - 2*Nielsen[1, 2, -z] - Log[z]*PolyLog[2, -z] -
			2*Log[1 + z]*PolyLog[2, -z] + PolyLog[3, -z] +
		Zeta[3])/(1 - z) /; FreeQ[z, y],

(*I320I*)
(*X*)iT[(Log[1 - (y_)]*Log[1 + ((y_)*(1 - (z_)))/(z_)])/
	(1 - (y_)*(1 - (z_))), {y_, 0, 1}] :>
	((-5*Zeta[3])/2 - Zeta2*Log[z] + Log[z]^3/2 + Zeta2*Log[1 + z] -
			(Log[z]^2*Log[1 + z])/2 + Log[z]*Log[1 + z]^2 +
			2*Nielsen[1, 2, 1 - z] +
			2*Nielsen[1, 2, -z] + 2*Log[z]*PolyLog[2, 1 - z] +
			Log[z]*PolyLog[2, -z] + 2*Log[1 + z]*PolyLog[2, -z] -
			2*PolyLog[3, 1 - z] - 3*PolyLog[3, -z] -
			2*PolyLog[3, -((1 - z)/(1 + z))] + 2*PolyLog[3, (1 - z)/(1 + z)]
	)/(1 - z) /; FreeQ[z, y],

(*I321I*)
(*X*)iT[Log[1 + ((y_)*(1 - (z_)))/(z_)]/(1 - (y_)*(1 - (z_)))^2,
	{y_, 0, 1}] :> -(Log[z]/(z*(1 + z))) /; FreeQ[z, y],

(*I322I*)
(*X*)iT[Log[1 + (y_)], {y_, 0, 1}] :> -1 + 2*Log[2],

(*I323I*)
(*X*)iT[Log[x_]*Log[1 + (x_)], {x_, z_/;!NumberQ[z], 1}] :>
	Zeta2/2 - 2*Log[2] - Log[z] - Log[z]^2/2 + ((1 + z)*Log[1 + z])/z +
		((1 + z)*Log[z]*Log[1 + z])/z + PolyLog[2, -z],

(*I324I*)
(*X*)iT[PolyLog[2, -(x_)]/(x_)^2, {x_, z_/;!NumberQ[z], 1}] :>
	Zeta2/2 + 2*Log[2] + Log[z] - ((1 + z)*Log[1 + z])/z +
		PolyLog[2, -z]/z,

(*I325I*)
(*X*)iT[Log[x_]*Log[1 + (x_)], {x_, z_/;!NumberQ[z], 1}] :>
	2*(1 - z) - Zeta2/2 - 2*Log[2] + z*Log[z] + (1 + z)*Log[1 + z] -
		(1 + z)*Log[z]*Log[1 + z] - PolyLog[2, -z],

(*I326I*)
(*X*)iT[PolyLog[2, -(x_)], {x_, z_, 1}] :>
-1 + z - Zeta2/2 + 2*Log[2] - Log[1 + z] - z*Log[1 + z] -
	z*PolyLog[2, -z],

(*I327I*)
(*X*)iT[(Log[x_]*(x_))/(1 + (x_)), {x_, z_/;!NumberQ[z], 1}] :>
	-1 + z + Zeta2/2 - z*Log[z] + Log[z]*Log[1 + z] + PolyLog[2, -z],

(*I328I*)
(*X*)iT[Log[x_]*Log[1 + (x_)]*(x_), {x_, z_/;!NumberQ[z], 1}] :>
	(-3*(1 - z))/4 + (1 - z^2)/4 + Zeta2/4 - ((2 - z)*z*Log[z])/4 -
		((1 - z^2)*Log[1 + z])/4 + ((1 - z^2)*Log[z]*Log[1 + z])/2 +
		PolyLog[2, -z]/2,

(*I329I*)
(*X*)iT[(x_)*PolyLog[2, -(x_)], {x_, z_, 1}] :>
	(1 - z)/4 + (-1 + z^2)/8 - Zeta2/4 + ((1 - z^2)*Log[1 + z])/4 -
		(z^2*PolyLog[2, -z])/2,

(*I330I*)
(*X*)iT[Log[x_]*Log[1 + (x_)]*(x_)^2, {x_, z_/;!NumberQ[z], 1}] :>
	(4*(1 - z))/9 - (5*(1 - z^2))/36 + (2*(1 - z^3))/27 - Zeta2/6 -
		(2*Log[2])/9 + (z*Log[z])/3 - (z^2*Log[z])/6 + (z^3*Log[z])/9 +
		((1 + z^3)*Log[1 + z])/9 - ((1 + z^3)*Log[z]*Log[1 + z])/3 -
		PolyLog[2, -z]/3,

(*I331I*)
(*X*)iT[(x_)^2*PolyLog[2, -(x_)], {x_, z_, 1}] :>
	(-1 + z)/9 + (1 - z^2)/18 + (-1 + z^3)/27 - Zeta2/6 + (2*Log[2])/9 -
		((1 + z^3)*Log[1 + z])/9 - (z^3*PolyLog[2, -z])/3,

(*I332I*)
(*X*)iT[Log[x_]^2*(x_)^2, {x_, z_/;!NumberQ[z], 1}] :>
	(2*(1 - z^3))/27 + (2*z^3*Log[z])/9 - (z^3*Log[z]^2)/3,

(*I333I*)
(*X*)iT[PlusDistribution[(1 - (x_))^(-1)]*
	PolyLog[2, (1 - (z_))/(1 - (1 - (x_))*(z_))],
			{x_, z_/;!NumberQ[z], 1}] :>
	-(Zeta2*Log[z]) + Log[1 - z]*Log[z]^2 -
		Log[1 - z]*PolyLog[2, 1 - z] + Log[z]*PolyLog[2, 1 - z] +
		2*PolyLog[3, 1 - z] + 2*PolyLog[3, z] - 2*Zeta[3],

(*I334I*)
(*X*)iT[PolyLog[2, (1 - (y_))*(1 - (z_))]/(1 - (y_)*(1 - (z_))),
				{y_, 0, 1}] :>
	(-(Zeta2*Log[z]) + Log[1 - z]*Log[z]^2 + Log[z]*PolyLog[2, 1 - z] -
		2*Log[z]*PolyLog[2, -z] + 4*PolyLog[3, -z] + 2*PolyLog[3, z] + Zeta[3])/
		(1 - z) /; FreeQ[z, y],

(*I335I*)
(*X*)iT[PolyLog[2, ((y_)*(1 - (z_)))/((y_)*(1 - (z_)) + (z_))]/
	(1 - (y_)*(1 - (z_))), {y_, 0, 1}] :>
	(-2*Zeta2*Log[z] + Log[1 - z]*Log[z]^2 + Zeta2*Log[1 + z] -
		(3*Log[z]^2*Log[1 + z])/2 + Log[-z]*Log[1 + z]^2 + Log[z]*Log[1 + z]^2 +
			Log[z]*PolyLog[2, 1 - z] - 3*Log[z]*PolyLog[2, -z] +
			2*Log[1 + z]*PolyLog[2, -z] + 2*Log[1 + z]*PolyLog[2, 1 + z] +
			3*PolyLog[3, -z] + 2*PolyLog[3, z] - 2*PolyLog[3, 1 + z] + 2*Zeta[3])/
		(1 - z) /; FreeQ[z, y],

(*I336I*)
(*X*)iT[PolyLog[2, ((1 - (y_))*(1 - (z_)))/(1 - (y_)*(1 - (z_)))]/
	(1 - (y_)*(1 - (z_))), {y_, 0, 1}] :>
	(-2*Zeta2*Log[z] + Log[1 - z]*Log[z]^2 + Log[z]*PolyLog[2, 1 - z] +
			2*PolyLog[3, z] - 2*Zeta[3])/(1 - z) /; FreeQ[z, y],

(*I337I*)
(*X*)iT[PolyLog[2, -(((y_)*(z_))/(1 - (z_)))]/(y_), {y_, 0, 1}] :>
	Zeta2*Log[1 - z] + Log[1 - z]^3/6 - (Log[1 - z]^2*Log[z])/2 -
		PolyLog[3, 1 - z] - PolyLog[3, z] + Zeta[3] /; FreeQ[z, y],

(*
(*I338I*)
(*X*)iT[PolyLog[2, -(((y_)*(z_))/(1 - (z_)))]/(1 - (y_)*(1 - (z_))),
		{y_, 0, 1}] :>
*)

(*I339I*)
(*X*)iT[PlusDistribution[(1 - (y_))^(-1)]*
	PolyLog[2, (1 - (z_))/(1 - (y_)*(z_))], {y_, 0, 1}] :>
	Zeta2*Log[1 - z] - Zeta2*Log[z] + Log[z]*PolyLog[2, 1 - z] -
		PolyLog[3, 1 - z] - Zeta[3] +
		2*((Log[1 - z]*Log[z]^2)/2 + Log[z]*PolyLog[2, z] - PolyLog[3, z] +
	Zeta[3]) /; FreeQ[z, y],

(*I340I*)
(*X*)iT[PlusDistribution[(1 - (y_))^(-1)]*
	PolyLog[2, 1 - (1 - (y_))*(z_)], {y_, 0, 1}] :>
	Zeta2*Log[z] - Log[1 - z]*Log[z]^2 - Log[z]*PolyLog[2, 1 - z] -
		2*PolyLog[3, z] /; FreeQ[z, y],

(*I341I*)
(*X*)iT[PlusDistribution[(1 - (y_))^(-1)]*PolyLog[2, 1 - (y_)*(z_)],
		{y_, 0, 1}] :>
	-(Zeta2*Log[z]) + Log[1 - z]*Log[z]^2 + Log[z]*PolyLog[2, 1 - z] +
		PolyLog[3, 1 - z] + 2*PolyLog[3, z] - Zeta[3] /; FreeQ[z, y],

(*I342I*)
(*X*)iT[Log[(x_)/(z_)]/(x_), {x_, z_, 1}] :> 1/2 Log[z]^2,

(* TRIVIAL *)

(* that's just for the record or for  *)
(*I343I*)
(*X*)iT[1, {_,0,1}] :> 1,
(*I344I*)
(*X*)iT[x_, {x_,0,1}] :> 1/2,
(*I345I*)
(*X*)iT[x_^n_/;n>-1, {x_,0,1}] :> 1/(n+1),
(*I346I*)
(*X*)iT[Log[1-x_], {x_, 0, 1}] :> -1,
(*I347I*)
(*X*)iT[Log[x_] x_^n_Integer?Positive, {x_, 0, 1}] :> -(1+n)^(-2),
(*I348I*)
(*X*)iT[Log[1-x_] x_^n_Integer?Positive, {x_, 0, 1}] :> -(1+n)^(-2),
(*I349I*)
(*X*)iT[Log[x_], {x_, 0, 1}  ] :> -1,
(*I350I*)
(*X*)iT[Log[x_]^n_Integer?Positive, {x_, 0, 1}  ] :> (-1)^n Gamma[1+n],
(*I351I*)
(*X*)iT[Log[1-x_]^n_Integer?Positive, {x_, 0, 1}  ] :> (-1)^n Gamma[1+n],
(*I352I*)
(*X*)iT[x_ Log[x_], {x_, 0, 1}  ] :> -1/4,

(*I353I*)
(*X*)iT[Log[y_]^2/y_, {y_,x_ /; x=!=0, 1}] :> -Log[x]^3/3,
(*I354I*)
(*X*)iT[Log[y_]^3/y_, {y_,x_ /; x=!=0, 1}] :>
			-1/4 - (-1 - 2*Log[x])/(4*x^2),

(*I355I*)
(*X*)iT[y_^(-1), {y_,x_/;x=!=0,1}] :> -Log[x],
(*I356I*)
(*X*)iT[Log[1-y_]^2 y_ ^2, {y_,x_,1}] :>
85/54 - (11*x)/9 - (5*x^2)/18 - (2*x^3)/27 - (11*Log[1 - x])/9 +
	(2*x*Log[1 - x])/3 + (x^2*Log[1 - x])/3 + (2*x^3*Log[1 - x])/9 +
	Log[1 - x]^2/3 - (x^3*Log[1 - x]^2)/3
	,
(*I357I*)
(*X*)iT[Log[y_]/y_^4,{y_,x_/;x=!=0,1}] :>
			-1/9 + 1/(9*x^3) + Log[x]/(3*x^3),

(*I358I*)
(*X*)iT[Log[y_]^2/(1 - x_*y_), {y_, x_/;x=!=0, 1}] :>
	(Log[x]^2*Log[1 - x^2])/x + (2*Log[x]*PolyLog[2, x^2])/x +
		(2*PolyLog[3, x])/x - (2*PolyLog[3, x^2])/x,

(*I359I*)
(*X*)iT[Log[1 - y_]/y_^2,{y_, x_ /;x=!=0, 1}] :>
	-Log[1 - x] + Log[1 - x]/x + Log[x],

(*I360I*)
(*X*)iT[Log[1 - y_]/y_^3,{y_, x_ /;x=!=0, 1}] :>
		1/2 - 1/(2*x) - Log[1 - x]/2 + Log[1 - x]/(2*x^2) + Log[x]/2,

(* or :
(*I361I*)
(*X*)iT[Log[y_]^2/(1 - x_*y_), {y_, x_ /; !NumberQ[x], 1}] :>
	(4*Zeta2*Log[x])/x - (3*Log[1 - x]*Log[x]^2)/x +
		(Log[x]^2*Log[1 + x])/x - (4*Log[x]*PolyLog[2, 1 - x])/x +
(4*Log[x]*PolyLog[2, -x])/x - (8*PolyLog[3, -x])/x -
(6*PolyLog[3, x])/x
*)

(*I362I*)
(*X*)iT[Log[1 - (x_)*(z_)]/(x_), {x_, z_/;!NumberQ[z], 1}] :>
	Zeta2 - Log[1 - z]*Log[z] - PolyLog[2, 1 - z] + 2*PolyLog[2, -z],

(*I363I*)
(*X*)iT[Log[x_]/(1 - (x_)*(z_)), {x_, z_/;!NumberQ[z], 1}] :>
	Zeta2/z + (Log[z]*Log[1 + z])/z - PolyLog[2, 1 - z]/z +
		(2*PolyLog[2, -z])/z,

(*I364I*)
(*X*)iT[(1 - (z_)*(x_))^(-1), {x_, z_, 1}] :> Log[1+z] ,

(*I365I*)
(*X*)iT[Log[1 - (x_)]/(1 - (x_)*(z_)), {x_, z_ /;!NumberQ[z], 1}] :>
(Log[1 - z]*Log[1 + z] + PolyLog[2, -z])/z,

(*I366I*)
(*X*)iT[Log[(x_) - (z_)]/(1 - (x_)*(z_)),
				{x_, z_/;!NumberQ[z], 1}] :>
(Log[1 - z]*Log[1 + z])/z + Log[1 + z]^2/(2*z) + PolyLog[2, -z]/z,

(*I367I*)
(*X*)iT[Log[1 - (x_)*(z_)]/(1 - (x_)*(z_)),
				{x_, z_/;!NumberQ[z], 1}] :>
(Log[1 - z]*Log[1 + z])/z + Log[1 + z]^2/(2*z),

(*I368I*)
(*X*)iT[(Log[x_]*Log[1 - (x_)*(z_)])/(x_), {x_, z_/;!NumberQ[z], 1}] :>
2*Zeta2*Log[z] - 2*Log[1 - z]*Log[z]^2 - 2*Log[z]*PolyLog[2, 1 - z] +
	2*Log[z]*PolyLog[2, -z] - 4*PolyLog[3, -z] - 3*PolyLog[3, z],

(*I369I*)
(*X*)iT[(Log[1 - (x_)]*Log[x_])/(1 - (x_)*(z_)),
				{x_, z_/;!NumberQ[z], 1}] :>
(Zeta2*Log[1 - z])/z + (Zeta2*Log[z])/z - (Log[1 - z]*Log[z]^2)/z +
	(Log[1 - z]*Log[z]*Log[1 + z])/z - (Log[1 - z]*PolyLog[2, 1 - z])/z -
	(Log[z]*PolyLog[2, 1 - z])/z + (2*Log[1 - z]*PolyLog[2, -z])/z +
	(Log[z]*PolyLog[2, -z])/z + PolyLog[3, 1 - z]/z - (4*PolyLog[3, -z])/z -
	(2*PolyLog[3, z])/z - Zeta[3]/z,

(*I370I*)
(*X*)iT[(Log[x_]*Log[1 - (x_)*(z_)])/(1 - (x_)*(z_)),
				{x_, z_/;!NumberQ[z], 1}] :>
(2*Zeta2*Log[1 - z] + 2*Zeta2*Log[1 + z] +
			2*Log[1 - z]*Log[z]*Log[1 + z] + 3*Log[z]*Log[1 + z]^2)/(2*z) -
	((Log[1 - z] + 2*Log[1 + z])*PolyLog[2, 1 - z])/z +
	(2*(Log[1 - z] + Log[1 + z])*PolyLog[2, -z])/z - PolyLog[3, 1 - z]/z +
	PolyLog[3, 1 - z^2]/z,

(*
,

(*I371I*)
(*X*)iT[(Log[y - (x_)]*Log[1 - (x_)*(z_)])/(x_), {x_, z_, 1}] :>
*)
(*DD3.15*)
(*I372I*)
(*X*)iT[Log[1+a_ y_] Log[1+ b_ y_]/y_, {y_,0,1}] :>
(
Nielsen[1,2,-a]+
Nielsen[1,2,-b]-
1/2 Log[a/b]^2 Log[1+b] + Log[a/b] ( PolyLog[2,(a-b)/a]-
PolyLog[2,(a-b)/(a (1+b))]) -
Nielsen[1,2,(a-b)/a] +
Nielsen[1,2,(a-b)/(a (1+b))]-
Nielsen[1,2,(b-a)/(1+b)]
) /; FreeQ[{a,b},y] && (Factor2[a] =!= Factor2[b]) &&
											(Factor2[a] =!= Factor2[c]) &&
											(Factor2[b+1] =!= 0 ) &&
											(Factor2[b] =!= Factor2[c])
,
(*I373I*)
(*X*)iT[Log[1+a_ y_]^2 /(1 + c_ y_), {y_,0,1}] :>
(
1/c (
Log[(c-a)/c]^2 Log[1+c] + 2 Log[(c-a)/c] *
(PolyLog[2,a/(a-c)]-PolyLog[2,a (1+c)/(a-c)]) +
2 Nielsen[1,2,a (1+c)/(a-c)] -
2 Nielsen[1,2,a /(a-c)])
) /; FreeQ[{a,c},y] && (Factor2[a] =!= Factor2[c]) &&
		Factor2[c]=!= -1
,
(*I373bI*)
(*X*)iT[Log[1-u_ y_]^2 /(1 - y_), {y_,0,1}] :>
2*Zeta2*Log[1 - u] + (2*Log[1 - u]^3)/3 -
Log[1 - u]^2*Log[u] - 2*PolyLog[3, 1 - u] + 2*Zeta[3]
(* -  Log[SmallDelta] Log[1-u]^2 *)
,
(*I374I*)
(*X*)iT[1/(1+c_ y_) Log[(1+a_ y_)/(1+b_ y)], {y_,0,1}] :>
(
1/c ( Log[a/b]^2 Log[1+b] +
Log[(a-c)/(b-c)]^2 Log[(1+c)/(1+b)] +
2 Log[a/b](PolyLog[2, (a-b)/(a (1+b))] -
PolyLog[2,(a-b)/a]) + 2 Log[(a-c)/(b-c)](
PolyLog[2,(a-b)/(a-c)] -
PolyLog[2, ( (a-b) (1+c) )/( (a-c) (1+b) )] ) +
2(Nielsen[1,2,(a-b)/a]-
	Nielsen[1,2,(a-b)/(a (1+b))] -
	Nielsen[1,2,(a-b)/(a-c)] +
	Nielsen[1,2,(a-b)(1+c)/((a-c)(1+b))]
) )
) /; FreeQ[{a,b,c},y] && (Factor2[a] =!= Factor2[b]) &&
											(Factor2[a] =!= Factor2[c]) &&
											(Factor2[b] =!= Factor2[c])
,
(*I375I*)
(*X*)iT[Log[1+a_ y_] Log[1+b_ y_]/(1+c_ y_), {y_,0,1}] :>
(
1/c(1/2 Log[(c-a)/c] Log[1+c] +
1/2 Log[(c-b)/c]^2 Log[1+c] - 1/2 Log[a/b]^2 Log[1+b] +
1/2 Log[(a-c)/(b-c)]^2 Log[(1+b)/(1+c)] +
Log[(c-a)/c] ( PolyLog[2,a/(a-c)]-PolyLog[2,a (1+c)/(a-c)]) +
Log[(c-b)/c] ( PolyLog[2,b/(b-c)]-PolyLog[2,b (1+c)/(b-c)])+
Nielsen[1,2,a (1+c)/(a-c)]-
Nielsen[1,2,a/(a-c)] +Nielsen[1,2,b(1+c)/(b-c)]-
Nielsen[1,2,b/(b-c)]-Nielsen[1,2,(a-b)/a]+
Nielsen[1,2,(a-b)/(a (1+b))] -
Nielsen[1,2,(a-b) (1+c)/((a-c) (1+b))]+
Nielsen[1,2,(a-b)/(a-c)]+
Log[a/b](PolyLog[2,(a-b)/a]-PolyLog[2,(a-b)/(a (1+b))]+
Log[(a-c)/(b-c)] (PolyLog[2,(a-b) (1+c)/(a-c)/(1+b)]-
									PolyLog[2,(a-b)/(a-c)])
									) )
) /; FreeQ[{a,b,c},y]  &&
(Factor2[a] =!= Factor2[b]) &&
(Factor2[a] =!= Factor2[c]) && (Factor2[b] =!= Factor2[c])
,
(*I376I*)
(*X*)iT[Log[y_] Log[1+a_ y_]/(1+b_ y_), {y_,0,1}]  :>
(
1/b( -1/2 Log[1+b] ( Log[1+a]^2-Log[(b-a)/b]^2)+
Log[1+a] ( PolyLog[2,(a-b)/(1+a)]-PolyLog[2,a/(1+a)])-
Log[(b-a)/b] ( PolyLog[2,a (1+b)/(a-b)]-
							PolyLog[2,a/(a-b)]) - PolyLog[3,-b]+
PolyLog[3,(a-b)/(1+a)]-PolyLog[3,a/(1+a)]-
Nielsen[1,2,a/(a-b)] +
Nielsen[1,2,a (1+b)/(a-b)]
)) /; FreeQ[{a,b}, y] && (Factor2[a] =!= Factor2[b]) &&
			(Factor2[a] =!= -1) && (Factor2[b] =!= -1)
,

(*I377I*)
(*X*)iT[(Log[1 - y_]*Log[1 + ((1 - x_)*y_)/x_])/(1 - (1 - x_^(-1))*y_),
		{y_,0,1}] :>
((Pi^2*x*Log[x])/(6*(1 - x)) - (x*Log[1 - x]*Log[x]^2)/(2*(1 - x)) -
	(x*PolyLog[3, x])/(1 - x) + (x*Zeta[3])/(1 - x)
) /; FreeQ[x,y]
,
(*FIxITLATERMAYBE (ln[(-1+x)/x])*)
(*I378I*)
(*X*)iT[(Log[y_]*Log[1 + ((1 - x_)*y_)/x_])/(1 - (1 - x_^(-1))*y_) ,
			{y_,0,1}] :>
	(Pi^2*x*Log[x])/(6*(1 - x)) + (x*Log[(-1 + x)/x]*Log[x]^2)/(2*(1 - x)) +
	(x*Log[x]^3)/(2*(1 - x)) + (x*Log[x]*PolyLog[2, 1 - x])/(1 - x) +
	(x*PolyLog[3, x^(-1)])/(1 - x) - (x*Zeta[3])/(1 - x)
,

(*FIxITLATERMAYBE*)
(*I379I*)
(*X*)iT[(Log[1 - (1 - x_)*y_]*Log[1 + ((1 - x_)*y_)/x_])/(1 - (1 - x_)*y_),
{y_,0,1}] :>
-Log[x]^3/(2*(-1 + x)) + (Log[x]^2*Log[1 + x])/(2*(-1 + x)) -
	(Log[x]*PolyLog[2, x/(1 + x)])/(-1 + x) -
	PolyLog[3, (1 + x)^(-1)]/(-1 + x) + PolyLog[3, x/(1 + x)]/(-1 + x)
,
(*FIxITLATERMAYBE (ln[-x])*)
(*I380I*)
(*X*)iT[(Log[1 - (1 - x_)*y_]*Log[1 + ((1 - x_)*y_)/x_])/
				(1 - (1 - x_^(-1))*y_), {y_,0,1}] :>
	-(Pi^2*x*Log[x])/(3*(-1 + x)) + (x*Log[-x]*Log[x]*Log[1 + x])/(-1 + x) -
	(3*x*Log[x]^2*Log[1 + x])/(2*(-1 + x)) +
	(x*Log[x]*Log[1 + x]^2)/(2*(-1 + x)) +
	(x*Log[x]*PolyLog[2, 1 + x])/(-1 + x) -
	(x*PolyLog[3, (1 + x)^(-1)])/(-1 + x) + (x*PolyLog[3, x/(1 + x)])/(-1 + x)
,

(*I381I*)
(*X*)iT[Log[1 + ((1 - x_)*y_)/x_]^2/(1 - (1 - x_^(-1))*y_),
{y_,0,1}] :> -(x*Log[x]^3)/(3*(1 - x))
,

(*I382I*)
(*X*)iT[Log[y_]*Log[x_ + y_ - x_*y_]*PlusDistribution[(1 - y_)^(-1)],
				{y_,0,1}] :>
	Zeta2*Log[1 - x] - (Log[1 - x]^2*Log[x])/2 - Nielsen[1, 2, 1 - x] -
	Nielsen[1, 2, x] - Log[1 - x]*PolyLog[2, x] + Zeta[3]
,
(*I383I*)
(*X*)iT[Log[x_ + y_ - x_*y_]^2*PlusDistribution[(1 - y_)^(-1)], {y_,0,1}] :>
	2*Zeta2*Log[x] - Log[1 - x]*Log[x]^2 - 2*Log[x]*PolyLog[2, 1 - x] -
	2*PolyLog[3, x] + 2*Zeta[3]
,
(*I384I*)
(*X*)iT[ Log[x_ + y_ - x_*y_]*PlusDistribution[Log[1 - y_]/(1 - y_)],
				{y_,0,1} ] :>
	(Log[1 - x]^2*Log[x] - 2*Nielsen[1, 2, x] +
			2*Log[1 - x]*PolyLog[2, 1 - x] + 2*Zeta[3])/2
,
(*I385I*)
(*X*)iT[Log[x_ + y_ - x_*y_]*PlusDistribution[(1 - y_)^(-1)],{y_,0,1}] :>
-PolyLog[2, 1 - x]
,
(*
PolyLog[3,1-x]
*)

(*I386I*)
(*X*)iT[Log[1-x_ y_]^2/y_, {y_,0,1}] :>
	Log[1 - x]^2*Log[x] + 2*Log[1 - x]*PolyLog[2, 1 - x] -
	2*PolyLog[3, 1 - x] + 2*Zeta[3]
,

(*I387I*)
(*X*)iT[Log[y_] Log[1-x_ y_] /y_ , {y_,0,1}] :>PolyLog[3,x]
,

(* REMIDDI *)
(*I388I*)
(*X*)iT[Log[x_]/(1 - (x_)), {x_, 0, 1}] :> -Zeta2(*,*)
,
(*I389I*)
(*X*)iT[1/(1+x_),{x_,0,1}] :> Log[2]
,
(*I390I*)
(*X*)iT[Log[x_]^2/(1+x_),{x_,0,1}] :> (3 Zeta[3]/2)
,
(*I391I*)
(*X*)iT[x_ *PolyLog[2, 1 - x_^2], {x_,0,1}] :> -1/2+Zeta2/2
,
(*I392I*)
(*X*)iT[PolyLog[2, 1 - x_]/(1 + x_),{x_,0,1}] :>
				(3*Zeta2*Log[2])/2 - Zeta[3]
,
(*I393I*)
(*X*)iT[Log[1 - x_]/(1 + x_), {x_, 0, 1}] :>  -Pi^2/12 + Log[2]^2/2
,
(*I394I*)
(*X*)iT[Log[1 - x_]^2/(1 + x_), {x_, 0, 1}] :>
	-(Zeta2*Log[2]) + Log[2]^3/3 + (7*Zeta[3])/4
,
(*I395I*)
(*X*)iT[Log[x_]/(1 + x_), {x_, 0, 1}] :> -Zeta2/2
,
(*I396I*)
(*X*)iT[Log[1-x_]/(1 + x_), {x_, 0, 1}] :> -Zeta2/2 + Log[2]^2/2
,
(*I397I*)
(*X*)iT[Log[1-x_] Log[x_]/(1+x_),  {x_, 0, 1}] :>
-(Pi^2*Log[2])/12 - Zeta2*Log[2] + (13*Zeta[3])/8
,
(*I398I*)
(*X*)iT[Log[1 - (x_)]*Log[x_]^2*(x_)^2, {x_, 0, 1}] :>
(-131 + 4*Pi^2 + 72*Zeta[3])/108
,
(*I399I*)
(*X*)iT[Log[1 - (x_)]*(x_)^2*PolyLog[2, 1 - (x_)], {x_, 0, 1}] :>
413/216 - (11*Pi^2)/54
,
(*I400I*)
(*X*)iT[Log[x_]*(x_)^2*PolyLog[2, 1 - (x_)], {x_, 0, 1}] :>
25/27 - Pi^2/54 - (2*Zeta[3])/3
,
(*I401I*)
(*X*)iT[Log[x_]*(x_)^2*PolyLog[2, x_], {x_, 0, 1}] :>
(31 - 4*Pi^2)/108
,
(*I402I*)
(*X*)iT[x_^2*PolyLog[3, 1 - x_], {x_, 0, 1}] :>
(449 - 66*Pi^2 + 216*Zeta[3])/648
,
(*I403I*)
(*X*)iT[(x_)^2*PolyLog[3, x_], {x_, 0, 1}] :>
(11 - 3*Pi^2 + 54*Zeta[3])/162
,
(*RX2*)
(*X*)iT[Log[x_]^3/(1-x_), {x_,0,1}] :> - 12/5*Zeta2^2
,
(*R3*)
(*X*)iT[Log[x_]^3/(1+x_), {x_,0,1}] :> - 21/10*Zeta2^2
,
(*R4*)
(*X*)iT[Log[x_]^2 Log[1-x_]/x_, {x_,0,1}] :> - 4/5*Zeta2^2
,
(*R5*)
(*X*)iT[Log[x_]^2*Log[1-x_]/(1-x_), {x_,0,1}] :> - 1/5*Zeta2^2
,
(*R6*)
(*X*)iT[Log[x_]^2 Log[1-x_]/(1+x_), {x_,0,1}] :>
2/5 Zeta2^2 - 4 PolyLog[4,1/2] + Log[2]^2 Zeta2 -Log[2]^4/6
,
(*R7*)
(*X*)iT[Log[x_]^2 Log[1+x_]/x_, {x_,0,1}] :>
(7*Zeta2^2)/10
,
(*R8*)
(*X*)iT[Log[x_]^2 Log[1+x_]/(1-x_), {x_,0,1}] :>
- 19/20*Zeta2^2 + 7/2*Log[2]*Zeta[3]
,
(*R9*)
(*X*)iT[Log[x_]^2 Log[1+x_]/(1+x_), {x_,0,1}] :>
- 3/2*Zeta2^2 + 4*PolyLog[4,1/2] +
7/2*Log[2]*Zeta[3]- Log[2]^2*Zeta2 + 1/6*Log[2]^4
,
(*RX10*)
(*X*)iT[(Log[1 - (x_)]^2*Log[x_])/(x_), {x_,0,1}] :>
- 1/5*Zeta2^2
,
(*RX11*)
(*X*)iT[Log[x_]*Log[1-x_]^2*(1-x_)^-1, {x_,0,1}] :>
- 4/5*Zeta2^2
,
(*RX12*)
(*X*)iT[(Log[1 - (x_)]^2*Log[x_])/(1 + (x_)), {x_,0,1}] :>
11/10*Zeta2^2 - 6*PolyLog[4,1/2] - 1/4*Log[2]^4
,
(*RX13*)
(*X*)iT[(Log[x_]*Log[1 + (x_)]^2)/(x_), {x_,0,1}] :>
3/2*Zeta2^2 - 4*PolyLog[4,1/2] - 7/2*Log[2]*Zeta[3] +
Log[2]^2*Zeta2 - 1/6*Log[2]^4
,
(*RX14*)
(*X*)iT[(Log[x_]*Log[1 + (x_)]^2)/(1 - (x_)), {x_,0,1}] :>
- 7/4*Zeta2^2 + 4*PolyLog[4,1/2] + 21/4*Log[2]*Zeta[3] -
5/2*Log[2]^2*Zeta2 + 1/6*Log[2]^4
,
(*RX15*)
(*X*)iT[(Log[x_]*Log[1 + (x_)]^2)/(1 + (x_)), {x_,0,1}] :>
- 4/5*Zeta2^2 + 2*PolyLog[4,1/2] + 7/4*Log[2]*Zeta[3] -
1/2*Log[2]^2*Zeta2 + 1/12*Log[2]^4
,
(*RX16*)
(*X*)iT[(Log[1 - (x_)]*Log[x_]*Log[1 + (x_)])/(x_), {x_,0,1}] :>
- 27/40*Zeta2^2 + 2*PolyLog[4,1/2] + 7/4*Log[2]*Zeta[3] -
1/2*Log[2]^2*Zeta2 + 1/12*Log[2]^4
,
(*RX17*)
(*X*)iT[(Log[1 - (x_)]*Log[x_]*Log[1 + (x_)])/(1 - (x_)), {x_,0,1}] :>
+ 17/40*Zeta2^2 - 2*PolyLog[4,1/2] + 7/8*Log[2]*Zeta[3] -
1/4*Log[2]^2*Zeta2 - 1/12*Log[2]^4
,
(*RX18*)
(*X*)iT[(Log[1 - (x_)]*Log[x_]*Log[1 + (x_)])/(1 + (x_)), {x_,0,1}] :>
- 4/5*Zeta2^2 + 2*PolyLog[4,1/2] + 21/8*Log[2]*Zeta[3] -
5/4*Log[2]^2*Zeta2 + 1/12*Log[2]^4
,
(*RX19*)
(*X*)iT[Log[1 - (x_)]^3/(x_), {x_,0,1}] :> - 12/5*Zeta2^2
,
(*RX21*)
(*X*)iT[Log[1 - (x_)]^3/(1 + (x_)), {x_,0,1}] :> - 6*PolyLog[4,1/2]
,
(*RX22*)
(*X*)iT[Log[1-x_]^2*Log[1+x_]/x_, {x_,0,1}] :>
- 1/4*Zeta2^2 + 2*PolyLog[4,1/2] + 7/4*Log[2]*Zeta[3] -
1/2*Log[2]^2*Zeta2 + 1/12*Log[2]^4
,
(*RX24*)
(*X*)iT[(Log[1 - (x_)]^2*Log[1 + (x_)])/(1 + (x_)), {x_,0,1}] :>
- 1/10*Zeta2^2 + 2*Log[2]*Zeta[3] - Log[2]^2*Zeta2 + 1/4*Log[2]^4
,
(*RX25*)
(*X*)iT[ (Log[1 - (x_)]*Log[1 + (x_)]^2)/(x_), {x_,0,1}] :>
- 3/20*Zeta2^2
,
(*RX27*)
(*X*)iT[(Log[1 - (x_)]*Log[1 + (x_)]^2)/(1 + (x_)), {x_,0,1}] :>
- 4/5*Zeta2^2 + 2*PolyLog[4,1/2] + 2*Log[2]*Zeta[3] -
Log[2]^2*Zeta2 + 1/3*Log[2]^4
,
(*RX28*)
(*X*)iT[Log[1 + (x_)]^3/(x_), {x_,0,1}] :>
+ 12/5*Zeta2^2 - 6*PolyLog[4,1/2] - 21/4*Log[2]*Zeta[3] +
3/2*Log[2]^2*Zeta2 - 1/4*Log[2]^4
,
(*RX30*)
(*X*)iT[ Log[1 + (x_)]^3/(1 + (x_)), {x_,0,1}] :> 1/4*Log[2]^4
,
(*RX34*)
(*X*)iT[(Log[1-x_]^2)/x_, {x_,0,1}] :>2*Zeta[3]
,
(*RX34*)
(*X*)iT[Log[1-x_]^2*x_^-1, {x_,0,1}] :> 2*Zeta[3]
,
(*RX39*)
(*X*)iT[ Log[1 + (x_)]^2/(x_), {x_,0,1}] :> 1/4*Zeta[3]
,
(*RX44*)
(*X*)iT[(Log[x_]*Log[1 + (x_)])/(1 - (x_)), {x_,0,1}] :>
Zeta[3] - 3/2*Log[2]*Zeta2
,
(*RX46*)
(*X*)iT[(Log[1 - (x_)]*Log[1 + (x_)])/(1 + (x_)), {x_,0,1}] :>
1/8*Zeta[3] - 1/2*Log[2]*Zeta2 + 1/3*Log[2]^3
,
(*RX48*)
(*X*)iT[(Log[1 - (x_)]*Log[1 + (x_)])/(x_), {x_,0,1}] :>
- 5/8*Zeta[3]
,
(*RX54*)
(*X*)iT[Log[1 - (x_)]/(1 + (x_)), {x_,0,1}] :>
- 1/2*Zeta2 + 1/2*Log[2]^2
,
(* TEMPORARILY 11/95 *)
(*X*)iT[Log[y_]*Log[1 + y_]^2, {y_, 0, 1}] :>
-6 + Pi^2/6 + 8*Log[2] - 2*Log[2]^2 - Zeta[3]/4
,
(*X*)iT[Log[y_]*Log[1 + y_]^2*y_, {y_, 0, 1}] :>
25/8 - Pi^2/8 - 3*Log[2] + Zeta[3]/8
,
(* M can do it, but still .. *)
(*X*)iT[PolyLog[3,y_]/(1-y_),{y_,0,1}] :> -(Pi^4/72)
,
(*X*)iT[PolyLog[3,1-y_]/(1-y_),{y_,0,1}] :> Pi^4/90
,
(*X*)iT[PolyLog[2,1-y_]/(1-y_),{y_,0,1}] :> Zeta[3]
,
(*X*)iT[Log[x_ + (1 - x_)*y_]/(1 - y_), {y_, 0, 1}] :>
	-PolyLog[2, 1 - x] /; FreeQ[x, y]
,
(*X*)iT[(Log[1 - y_]*Log[x_ + (1 - x_)*y_])/(1 - y_), {y_, 0, 1}
			] :>
	(Log[1 - x]^2*Log[x])/2 - Nielsen[1, 2, x] +
		Log[1 - x]*PolyLog[2, 1 - x] + Zeta[3] /; FreeQ[x, y]
,
(*X*) iT[(Log[y_]*Log[x_ + (1 - x_)*y_])/(1 - y_), {y_, 0, 1}] :>
	(Pi^2*Log[1 - x])/2 - 3*Zeta2*Log[1 - x] + Zeta2*Log[x] +
		(Log[1 - x]^2*Log[x])/2 - (Log[1 - x]*Log[x]^2)/2 - Nielsen[1, 2, x] +
Log[1 - x]*PolyLog[2, 1 - x] - Log[x]*PolyLog[2, 1 - x] - PolyLog[3, x] +
		2*Zeta[3] /; FreeQ[x, y]
,
(*X*)iT[Log[x_ + (1 - x_)*y_]^2/(1 - y_), {y_, 0, 1}] :>
2*Zeta2*Log[x] - Log[1 - x]*Log[x]^2 - 2*Log[x]*PolyLog[2, 1 - x] -
		2*PolyLog[3, x] + 2*Zeta[3] /; FreeQ[x, y]
,
(*X*)iT[Log[1-x_]^2/x_^2,{x_,0,1}] :> 2 Zeta2
,
(*X*)iT[Log[x_]^2/(1-x_)^2,{x_,0,1}] :> 2 Zeta2
,
(*X*)iT[Log[x_*(1 - y_) + y_]/x_,{x_,0,1}] :>
Log[y]^2/2 + PolyLog[2, 1 - y] /; FreeQ[y, x]
,
(*X*)iT[Log[x_ y_ + c_],{y_,0,1}] :>
(-1 + Log[c + x] + (-(c*Log[c]) + c*Log[c + x])/x
) /; FreeQ[{x,c},y] && (Factor2[c+x]=!=0)
,
(*X*)iT[Log[1 - y_]*Log[a_ + b_*y_], {y_, 0, 1}]:>
(-((-2*b - a*Log[a] + a*Log[a + b] + b*Log[a + b] +
			(a + b)*PolyLog[2, b/(a + b)])/b)
) /; FreeQ[{a,b},y] && Factor2[a+b]=!=0
,
(*X*)iT[Log[y_] Log[a_ + b_ y_], {y_, 0, 1}] :>
( (2*b + a*Log[a] - a*Log[a + b] - b*Log[a + b] +
a*PolyLog[2, -(b/a)])/b
) /; FreeQ[{a, b},y] && Factor2[a + b] =!=0
,
(*X*)iT[Log[a_+b_ x_]^2, {x_,0,1}] :>
(
	-((a*(2 - 2*Log[a] + Log[a]^2))/b) +
	((a + b)*(2 - 2*Log[a + b] + Log[a + b]^2))/b
) /; FreeQ[{a, b},y] && Factor2[a + b] =!=0

,
(*X*)iT[Log[1-x_] Log[x_]/x_,{x_,0,1}] :> Zeta[3]

,
(* in a distribution sense ... *)
(*X*)iT[(Log[x_]*Log[x_*(1 - y_) + y_])/x_,{x_,0,1}] :>
(Zeta2*Log[y] - (Log[1 - y]*Log[y]^2)/2 +
Log[y]^3/6 - PolyLog[3, 1 - y] - PolyLog[3, y] + Zeta[3]
) /; FreeQ[y,x]
,
(*X*)iT[(Log[1 - x_]*PolyLog[2, 1 - x_])/(1 - x_),
				{x_, 0, 1}] :> -Pi^4/90
,
(*X*)iT[(Log[x_]*PolyLog[2, 1 - x_])/(1 - x_), {x_, 0, 1}
			]:> -Pi^4/72
,
(* in a distribution sense ... *)
(*X*)iT[PolyLog[2,1-x_]/x_, {x_,0,1}] :> -2 Zeta[3]
,
(* in a distribution sense ... *)
(*X*)iT[PolyLog[2,x_]/(1-x_), {x_,0,1}] :> -2 Zeta[3]
,
(*X*)iT[1/(x_ a_. + b_),{x_,0,1}] :>
(-(Log[b]/a) + Log[a + b]/a) /; FreeQ[{a, b},x] &&
	(Factor[b] =!= 0) && (Factor[a+b] =!= 0)
,
(*X*)iT[Log[x_]/(x_ a_. + b_),{x_,0,1}] :>
	PolyLog[2, -a/b]/a /; FreeQ[{a, b},x]
,
(*X*)iT[Log[1-x_]/(x_ a_. + b_),{x_,0,1}] :>
	-PolyLog[2, a/(a+b)]/a  /; FreeQ[{a, b},x] &&
	(Factor[a+b] =!= 0)
,
(*X*)iT[Log[a_. x_+ b_]/(x_ a_. + b_),{x_,0,1}] :>
-(Log[b]^2/(2*a)) + Log[a + b]^2/(2*a) /;FreeQ[{a, b},x]
, (* NEW March 96 *)
(*X*)iT[Log[a_. x_ + b_]/x_, {x_,0,1}] :>
-PolyLog[2,-a/b]  /; FreeQ[{a,b},x]
,
(*X*)iT[Log[a_. x_+ b_]/(x_ c_. + d_),{x_,0,1}] :>
((-(Log[b]*Log[(a*d)/(-(b*c) + a*d)]) +
		Log[a + b]*Log[(a*c + a*d)/(-(b*c) + a*d)] -
		PolyLog[2, -((b*c)/(-(b*c) + a*d))] +
		PolyLog[2, (-(a*c) - b*c)/(-(b*c) + a*d)])/c
) /; FreeQ[{a,b,c,d},x] && (Factor[a d-b c]=!=0) &&
			(Factor[a+b]=!=0) && Factor[c+d] =!= 0
,
(*X*)iT[Log[1-x_]^2/(a_ + b_. x_)^2, {x_,0,1}] :>
	2 PolyLog[2,b/(a+b)]/b/(a+b) /; FreeQ[{a,b},x] &&
	(Factor[a+b] =!= 0)
,
(*X*)iT[(Log[x_]*Log[1 - x_*y_])/(1 - x_), {x_,0,1}] :>
(-(Zeta2*Log[y]) - Log[1 - y]^2*Log[y] + (Log[1 - y]*Log[y]^2)/2 +
Nielsen[1, 2, 1 - y] + 2*Nielsen[1, 2, y] - Log[1 - y]*PolyLog[2, 1 - y] +
	Log[y]*PolyLog[2, 1 - y] - Zeta[3]
) /; FreeQ[y,x]
,
(*X*)iT[PolyLog[2,x_]/(1-x_)^2, {x_,0,1}] :> -1 - Zeta2
,
(*X*)iT[PolyLog[2,1-x_]/x_^2, {x_,0,1}] :> -1 - Zeta2
,
(*X*)iT[1/(1-x_)^2,{x_,0,1}] :> -1
,
(*X*)iT[1/x_^2,{x_,0,1}] :> -1
,
(*X*)iT[Log[1-x_]/(1-x_)^2,{x_,0,1}] :> -1
,
(*X*)iT[Log[x_]/x_^2,{x_,0,1}] :> -1
,
(*X*)iT[Log[1-x_] Log[x_]^2/(1-x_)^2 ,{x_,0,1}] :>
	-4Zeta[3]+2Zeta2
,
(*X*)iT[Log[1-x_] Log[x_]^2/(1-x_)^2 ,{x_,0,1}] :>
	-4Zeta[3]+2Zeta2
,
(*X*)iT[Log[x_]^3/(1-x_)^2 ,{x_,0,1}] :> -6 Zeta[3]
,
(*X*)iT[Log[1-x_]^3/x_^2 ,{x_,0,1}] :> -6 Zeta[3]
,
(*X*)iT[Log[1-x_] Log[x_]/x_^2 ,{x_,0,1}] :> Zeta2-1
,
(*X*)iT[(Log[(1 - y_)^(-1) - 1/((1 - y_)*x_)]*
	Log[-(y_/(1 - y_)) + 1/((1 - y_)*x_)])/
		(1 - x_), {x_, 0, 1}
	] :>(
	2*I*Pi*Zeta2 - 2*Zeta2*Log[1 - y] + I/2*Pi*Log[1 - y]^2 - Log[1 - y]^3/3 -
	I*Pi*Log[1 - y]*Log[y] + (Log[1 - y]^2*Log[y])/2 -
	I*Pi*PolyLog[2, 1 - y] + PolyLog[3, 1 - y]
			) /; FreeQ[y,x]
,
(*X*)iT[PolyLog[2, 1 + (1 - x_)/(x_*(1 - y_))]/(1 - x_), {x_, 0, 1}] :>
( -2*I*Pi*Zeta2 - I/2*Pi*Log[1 - y]^2 + Log[1 - y]^3/6 +
	I*Pi*Log[1 - y]*Log[y] - (Log[1 - y]^2*Log[y])/2 +
	I*Pi*PolyLog[2, 1 - y] - Log[1 - y]*PolyLog[2, 1 - y] + PolyLog[3, 1 - y]
) /; FreeQ[y, x]

,
(*X*)iT[Log[1 - x_]^2/(x_*y_ - y_ + 1), {x_, 0, 1}] :>
2 PolyLog[3,y]/y /; FreeQ[y, x]
,
(*X*)iT[Log[x_*y_ - y_ + 1]^2/(x_*y_ - y_ + 1), {x_, 0, 1}] :>
-Log[1-y]^3/3/y /;  FreeQ[y, x]
,
(*X*)iT[(Log[1 - x_]*Log[1 - y_ + x_*y_])/(1 - y_ + x_*y_),
				{x_,0,1}] :> (
(Log[1 - y]^2*Log[y] + 2*Log[1 - y]*PolyLog[2, 1 - y] -
		2*PolyLog[3, 1 - y] + 2*Zeta[3])/(2*y)
										) /; FreeQ[y, x]
,
(*X*)iT[ PolyLog[3, 1 + x_], {x_, 0, 1}] :>
1 - I*Pi - Pi^2/3 + 2*I*Pi*Log[2] + (Pi^2*Log[2])/2 - I*Pi*Log[2]^2 +
	(3*Zeta[3])/4
,
(*X*)iT[x_ PolyLog[3, 1 + x_], {x_, 0, 1}] :>
-11/16 + (5*I)/8*Pi + Pi^2/8 - I*Pi*Log[2] + Zeta[3]/2
,
(*X*)iT[x_ Log[1+x_] PolyLog[2, 1 + x_], {x_, 0, 1}] :>
-31/16 + (5*I)/4*Pi + (3*Pi^2)/16 + Log[2] - 2*I*Pi*Log[2]
,
(*X*)iT[x_ Log[1 + x_] PolyLog[2, -x_], {x_, 0, 1}] :>
-19/16 - Pi^2/48 + 2*Log[2] - Zeta[3]/8
,
(*X*)iT[Log[((x_)*(1 - (y_)))/(1 - (x_)*(y_))]/(1 - (x_)), {x_, 0, 1}]:>
-2*Zeta2 - Log[1 - y]^2/2 + Log[1 - y]*Log[y] + PolyLog[2, 1 - y] /;
FreeQ[y, x]
,
(*X*)iT[(Log[1 - (x_)]*Log[((x_)*(1 - (y_)))/(1 - (x_)*(y_))])/(1 - (x_)),
	{x_, 0, 1}] :>
-(Zeta2*Log[1 - y]) - Log[1 - y]^3/6 + Zeta2*Log[y] +
	Log[1 - y]^2*Log[y] - (Log[1 - y]*Log[y]^2)/2 - Nielsen[1, 2, 1 - y] -
	Nielsen[1, 2, y] + Log[1 - y]*PolyLog[2, 1 - y] -
	Log[y]*PolyLog[2, 1 - y] + 2*Zeta[3]/;
FreeQ[y, x]
,
(*X*)iT[(Log[x_]*Log[((x_)*(1 - (y_)))/(1 - (x_)*(y_))])/(1 - (x_)),
	{x_, 0, 1}] :>
-(Zeta2*Log[1 - y]) + Zeta2*Log[y] + Log[1 - y]^2*Log[y] -
	(Log[1 - y]*Log[y]^2)/2 - Nielsen[1, 2, 1 - y] - 2*Nielsen[1, 2, y] +
	Log[1 - y]*PolyLog[2, 1 - y] - Log[y]*PolyLog[2, 1 - y] + 3*Zeta[3] /;
FreeQ[y, x]
,
(*X*)iT[(Log[(1 - (x_))/(1 - (x_)*(y_))]*
			Log[((x_)*(1 - (y_)))/(1 - (x_)*(y_))])/(1 - (x_)), {x_, 0, 1}]:>
2*Zeta2*Log[y] + Log[1 - y]^2*Log[y] - Log[1 - y]*Log[y]^2 -
2*Nielsen[1, 2, 1 - y] - Nielsen[1, 2, y] + Log[1 - y]*PolyLog[2, 1 - y] -
	2*Log[y]*PolyLog[2, 1 - y] + 3*Zeta[3]/;
FreeQ[y, x]
,
(*X*)iT[Log[(1-y_) x_ / (1-y_ x_)]^2/(1-x_),{x_,0,1}] :>
	-(Pi^2*Log[1 - y])/3 - Log[1 - y]^3/3 + Log[1 - y]^2*Log[y] +
	2*PolyLog[3, 1 - y] + 2*PolyLog[3, y]/;
FreeQ[y, x]
,
(*X*)iT[(Log[((x_)*(1 - (y_)))/(1 - (x_)*(y_))]*Log[1 - (x_)*(y_)])/
		(1 - (x_)), {x_, 0, 1}] :>
-(Zeta2*Log[1 - y]) - Log[1 - y]^3/6 - Zeta2*Log[y] +
	(Log[1 - y]*Log[y]^2)/2 + Nielsen[1, 2, 1 - y] +
	Log[y]*PolyLog[2, 1 - y] - Zeta[3]/;
FreeQ[y, x]
,
(*X*)iT[PolyLog[2, ((x_)*(1 - (y_)))/(1 - (x_)*(y_))]/(1 - (x_)),
	{x_, 0, 1}] :>
Zeta2*Log[1 - y] - Zeta2*Log[y] - Log[1 - y]^2*Log[y] +
	(Log[1 - y]*Log[y]^2)/2 + Nielsen[1, 2, 1 - y] + 2*Nielsen[1, 2, y] -
	Log[1 - y]*PolyLog[2, 1 - y] + Log[y]*PolyLog[2, 1 - y] - 3*Zeta[3]/;
FreeQ[y, x]
,
(*X*)iT[Log[(1-(x_)*(y_))/((1-(x_))*(y_))]/(1-(y_)),{y_,0,1}]:>
2*Zeta2+Log[1-x]^2/2-Log[1-x]*Log[x]-PolyLog[2,1-x] /;
FreeQ[x,y]
,
(*X*)iT[(Log[1-(y_)]*Log[(1-(x_)*(y_))/((1-(x_))*(y_))])/(1-(y_)),
{y_,0,1}]:>
Zeta2*Log[1-x]+Log[1-x]^3/6-Zeta2*Log[x]-Log[1-x]^2*Log[x]+
(Log[1-x]*Log[x]^2)/2+Nielsen[1,2,1-x]+Nielsen[1,2,x]-
Log[1-x]*PolyLog[2,1-x]+Log[x]*PolyLog[2,1-x]-2*Zeta[3] /;
FreeQ[x,y]
,
(*X*)iT[(Log[-((1-(y_))/((1-(x_))*(y_)))]*
Log[(1-(x_)*(y_))/((1-(x_))*(y_))])/(1-(y_)),{y_,0,1}]:>
2*I*Pi*Zeta2-2*Zeta2*Log[1-x]+I/2*Pi*Log[1-x]^2-
Log[1-x]^3/3-I*Pi*Log[1-x]*Log[x]+Log[1-x]^2*Log[x]-
Nielsen[1,2,x]-I*Pi*PolyLog[2,1-x]+
Log[1-x]*PolyLog[2,1-x]+Zeta[3] /;
FreeQ[x,y]
,
(*X*)iT[(Log[y_]*Log[(1-(x_)*(y_))/((1-(x_))*(y_))])/(1-(y_)),
{y_,0,1}]:>
Zeta2*Log[1-x]-Zeta2*Log[x]-Log[1-x]^2*Log[x]+
(Log[1-x]*Log[x]^2)/2+Nielsen[1,2,1-x]+2*Nielsen[1,2,x]-
Log[1-x]*PolyLog[2,1-x]+Log[x]*PolyLog[2,1-x]-3*Zeta[3] /;
FreeQ[x,y]
,
(*X*)iT[Log[(1-(x_)*(y_))/((1-(x_))*(y_))]^2/(1-(y_)),{y_,0,1}]:>
-2*Zeta2*Log[1-x]-Log[1-x]^3/3+2*Zeta2*Log[x]+
2*Log[1-x]^2*Log[x]-Log[1-x]*Log[x]^2-2*Nielsen[1,2,1-x]-
2*Nielsen[1,2,x]+2*Log[1-x]*PolyLog[2,1-x]-
2*Log[x]*PolyLog[2,1-x]+4*Zeta[3] /;
FreeQ[x,y]
,
(*X*)iT[PolyLog[2,(1-(x_)*(y_))/((1-(x_))*(y_))]/(1-(y_)),{y_,0,1}]:>
-2*I*Pi*Zeta2-I/2*Pi*Log[1-x]^2+Log[1-x]^3/6+
I*Pi*Log[1-x]*Log[x]-Nielsen[1,2,x]+I*Pi*PolyLog[2,1-x]+
Zeta[3] /;
FreeQ[x,y]
,
(*X*)iT[Log[-((1 - (x_))/((x_)*(1 - (y_))))]/(1 - (x_)),
				{x_, 0, 1}] :> Zeta2 /; FreeQ[y,x]
,
(*X*)iT[(Log[1 - (x_)]*Log[-((1 - (x_))/((x_)*(1 - (y_))))])/
			(1 - (x_)), {x_, 0, 1}] :> -Zeta[3] /; FreeQ[y,x]
,
(*X*)iT[Log[-((1 - (x_))/((x_)*(1 - (y_))))]^2/(1 - (x_)),
			{x_, 0, 1}] :>
		2*I*Pi*Zeta2 - 2*Zeta2*Log[1 - y] /; FreeQ[y,x]
,
(*X*)iT[(Log[x_]*Log[-((1 - (x_))/((x_)*(1 - (y_))))])/(1 - (x_)),
		{x_, 0, 1}] :> -I*Pi*Zeta2 + Zeta2*Log[1 - y] - Zeta[3]/; FreeQ[y,x]
,
(*X*)iT[(Log[-((1 - (x_))/((x_)*(1 - (y_))))]*
				Log[1 - (x_)*(y_)])/(1 - (x_)), {x_, 0, 1}] :>
		I*Pi*Zeta2 + I/2*Pi*Log[1 - y]^2 - Log[1 - y]^3/3 -
		I*Pi*Log[1 - y]*Log[y] + Log[1 - y]^2*Log[y] - Nielsen[1, 2, y] -
		I*Pi*PolyLog[2, 1 - y] + Log[1 - y]*PolyLog[2, 1 - y] /;
		FreeQ[y,x]
,
(*X*)iT[PolyLog[2, -((1 - x_)/(x_*(1 - y_)))]/(1 - x_), {x_, 0, 1}] :>
	(12*Zeta2*Log[1 - y] + Log[1 - y]^3 - 6*Log[1 - y]^2*Log[y] +
12*Nielsen[1, 2, y] - 6*Log[1 - y]*PolyLog[2, 1 - y] - 12*Zeta[3])/6 /;
FreeQ[y,x]
,
(* this is only for connaiseurs ... *)
(*X*)iT[x_^(m_?mcheck)/(1-x_) Hypergeometric2F1[1,em1_,em2_,x_],
				{x_,0,1}
			] :> (-(m+1)(-Zeta2-SumS[1,1,m]+SumS[2,m]))/;
						((em1-1===m) && (em2-2===m))
,
(*X*)iT[Log[1 - x_ + x_*y_]/(1 - y_), {y_, 0, 1}] :>
-Zeta2 + Log[1 - x]*Log[x] + PolyLog[2, 1 - x] /; FreeQ[x,y]
,
(*X*)iT[(Log[1 - (x_)]*Log[1 - (y_) + (x_)*(y_)])/(1 - (x_)),
		{x_, 0, 1}] :> PolyLog[3, y] /; FreeQ[y, x]
,
(*X*)iT[(Log[x_]*Log[1 - (y_) + (x_)*(y_)])/(1 - (x_)),
		{x_, 0, 1}] :>
(Log[1 - y]^2*Log[y])/2 + Log[1 - y]*PolyLog[2, 1 - y] -
		PolyLog[3, 1 - y] + PolyLog[3, y] + Zeta[3] /;
FreeQ[y, x]
,
(*X*)iT[Log[1 - (y_) + (x_)*(y_)]^2/(1 - (x_)), {x_, 0, 1}] :>
Log[1 - y]^2*Log[y] + 2*Log[1 - y]*PolyLog[2, 1 - y] -
2*PolyLog[3, 1 - y] + 2*Zeta[3]  /; FreeQ[y, x]
,
(*X*)iT[(Log[1 - u_]*Log[u_])/(1 - x_ + u_*x_), {u_,0,1}] :>
(Log[1 - x]^2*Log[x])/(2*x) + PolyLog[3, 1 - x]/x +
	(2*PolyLog[3, x])/x - Zeta[3]/x /; FreeQ[x,u]
,
(*X*)iT[Log[u_]^2/(u_*x_ - x_ + 1), {u_,0,1}] :>
(-2*Zeta2*Log[1 - x])/x - Log[1 - x]^3/(3*x) +
	(Log[1 - x]^2*Log[x])/x + (2*PolyLog[3, 1 - x])/x + (2*PolyLog[3, x])/x -
	(2*Zeta[3])/x/; FreeQ[x,u]
,
(*X*)iT[(Log[u_]*Log[1 - x_ + u_*x_])/(1 - x_ + u_*x_), {u_,0,1}] :>
-((Zeta2*Log[1 - x])/x) - Log[1 - x]^3/(3*x) +
	(Log[1 - x]^2*Log[x])/(2*x) + PolyLog[3, 1 - x]/x - Zeta[3]/x /;
FreeQ[x,u]
,
(*X*)iT[PolyLog[2,u_]/(1 - x_ + u_*x_), {u_,0,1}] :>
(Log[1 - x]*PolyLog[2, 1 - x])/x - (2*PolyLog[3, 1 - x])/x -
	PolyLog[3, x]/x + (2*Zeta[3])/x /; FreeQ[x,u]
,
(*X*)iT[PolyLog[3,u_]/(1-u_)^2,{u_,0,1}] :>
Zeta[3]-Zeta2
,
(*X*)iT[PolyLog[3,1-u_]/(1-u_)^2,{u_,0,1}] :>
3-Zeta[3]-Zeta2
,
(*X*)iT[Log[1-u_] PolyLog[2,u_]/(1-u_)^2,{u_,0,1}] :>
2 Zeta[3]-Zeta[2]-1
,
(*X*)iT[Log[1-u_]^2 Log[u_]/(1-u_)^2,{u_,0,1}] :>
-2 + 2 Zeta2 - 2 Zeta[3]
,
(*X*)iT[ (Log[u_]*PolyLog[2, u_])/(1 - (u_))^2,{u_,0,1}]:>
-Zeta2 + 3 Zeta[3]
,
(*X*)iT[Log[u_]/(1 - (u_)*(x_))^2, {u_, 0, 1}] :>
		Log[1 - x]/x /; FreeQ[u, x]
,
(*X*)iT[Log[1 - (u_)*(x_)]/(1 - (u_)*(x_))^2, {u_, 0, 1}] :>
	(1 - x)^(-1) + Log[1 - x]/(1 - x) +
	Log[1 - x]/x/; FreeQ[u, x]
,
(*X*)iT[(Log[1 - (u_)]*Log[1 - (u_)*(x_)])/(1 - (u_)*(x_))^2,
			{u_, 0, 1}] :>
	Zeta2/(1 - x) + Zeta2/x + Log[1 - x]/(1 - x) + Log[1 - x]/x +
	Log[1 - x]^2/(1 - x) + Log[1 - x]^2/x - (Log[1 - x]*Log[x])/(1 - x) -
(Log[1 - x]*Log[x])/x - PolyLog[2, 1 - x]/(1 - x) -
PolyLog[2, 1 - x]/x /; FreeQ[u, x]
,
(*X*)iT[(Log[u_]*Log[1 - (u_)*(x_)])/(1 - (u_)*(x_))^2, {u_, 0, 1}] :>
Zeta2/x + Log[1 - x]/x + Log[1 - x]^2/(2*x) - (Log[1 - x]*Log[x])/x -
	PolyLog[2, 1 - x]/x /; FreeQ[u, x]
,
(*X*)iT[Log[1 - (u_)*(x_)]^2/(1 - (u_)*(x_))^2, {u_, 0, 1}] :>
	2/(1 - x) + (2*Log[1 - x])/(1 - x) + (2*Log[1 - x])/x +
	Log[1 - x]^2/(1 - x) + Log[1 - x]^2/x/; FreeQ[u, x]
,
(*X*)iT[PolyLog[2, 1 - (u_)]/(1 - (u_)*(x_))^2, {u_, 0, 1}] :>
	-(Zeta2/x) + (Log[1 - x]*Log[x])/(1 - x) + (Log[1 - x]*Log[x])/x +
	PolyLog[2, 1 - x]/(1 - x) + PolyLog[2, 1 - x]/x /; FreeQ[u, x]
,

(*X*)iT[Log[1 - (u_)*(x_)]/(1 - (u_)*(x_))^3, {u_, 0, 1}] :>
1/(4*(1 - x)^2) + 1/(4*(1 - x)) + Log[1 - x]/(2*(1 - x)^2) +
Log[1 - x]/(2*(1 - x)) + Log[1 - x]/(2*x) /; FreeQ[x,u]
,
(*X*)iT[(Log[1 - (u_)]*Log[1 - (u_)*(x_)])/(1 - (u_)*(x_))^3, {u_, 0, 1}]:>
	-(3 - 2*Zeta2)/(4*(1 - x)^2) + Zeta2/(2*(1 - x)) + Zeta2/(2*x) -
		Log[1 - x]/(4*(1 - x)^2) - Log[1 - x]/(4*(1 - x)) - Log[1 - x]/(4*x) +
		Log[1 - x]^2/(2*(1 - x)^2) + Log[1 - x]^2/(2*(1 - x)) +
		Log[1 - x]^2/(2*x) - (Log[1 - x]*Log[x])/(2*(1 - x)^2) -
		(Log[1 - x]*Log[x])/(2*(1 - x)) - (Log[1 - x]*Log[x])/(2*x) -
		PolyLog[2, 1 - x]/(2*(1 - x)^2) - PolyLog[2, 1 - x]/(2*(1 - x)) -
		PolyLog[2, 1 - x]/(2*x)  /; FreeQ[x,u]
,
(*X*)iT[(Log[u_]*Log[1 - (u_)*(x_)])/
		(1 - (u_)*(x_))^3, {u_, 0, 1}] :>
	-3/(4*(1 - x)) + Zeta2/(2*x) - Log[1 - x]/(2*(1 - x)) -
		Log[1 - x]/(4*x) + Log[1 - x]^2/(4*x) - (Log[1 - x]*Log[x])/(2*x) -
		PolyLog[2, 1 - x]/(2*x) /; FreeQ[x,u]
,
(*X*)iT[Log[1 - (u_)*(x_)]^2/(1 - (u_)*(x_))^3,
		{u_, 0, 1}] :> 1/(4*(1 - x)^2) + 1/(4*(1 - x)) +
		Log[1 - x]/(2*(1 - x)^2) + Log[1 - x]/(2*(1 - x)) + Log[1 - x]/(2*x) +
		Log[1 - x]^2/(2*(1 - x)^2) + Log[1 - x]^2/(2*(1 - x)) +
		Log[1 - x]^2/(2*x) /; FreeQ[x,u]
,
(*X*)iT[PolyLog[2, 1 - (u_)]/(1 - (u_)*(x_))^3,
		{u_, 0, 1}] :>
		-Zeta2/(2*x) + Log[1 - x]/(2*(1 - x)) + Log[1 - x]/(2*x) +
		(Log[1 - x]*Log[x])/(2*(1 - x)^2) + (Log[1 - x]*Log[x])/(2*(1 - x)) +
		(Log[1 - x]*Log[x])/(2*x) + PolyLog[2, 1 - x]/(2*(1 - x)^2) +
		PolyLog[2, 1 - x]/(2*(1 - x)) + PolyLog[2, 1 - x]/(2*x) /; FreeQ[x,u]
,
(*X*)iT[(1 - (x_) + (t_)*(x_))^(-2), {t_, 0, 1}] :> (1 - x)^(-1) /;
	FreeQ[x,t]
,
(*X*)iT[Log[1 - (t_)]/(1 - (x_) + (t_)*(x_))^2, {t_, 0, 1}] :>
	Log[1 - x]/x /; FreeQ[x, t]
,
(*X*)iT[Log[t_]/(1 - (x_) + (t_)*(x_))^2, {t_, 0, 1}] :>
	Log[1 - x]/(1 - x) + Log[1 - x]/x /; FreeQ[x,t]
,
(*X*)iT[Log[1 - (x_) + (t_)*(x_)]/(1 - (x_) + (t_)*(x_))^2,
	{t_, 0, 1}] :>
	(1 - x)^(-1) + Log[1 - x]/(1 - x) + Log[1 - x]/x /; FreeQ[x, t]
,
(*X*)iT[Log[1-x_]^2/(1-x_)^2, {x_, 0, 1}] :> -2
,
(*X*)iT[Log[1 - (x_)]^3/(1 - (x_))^2, {x_, 0, 1}] :> -6
,
(*X*)iT[Log[1 - (x_)]^3/(1 - (x_)), {x_, 0, 1}] :> 0
,
(*X*)iT[Log[1 - (x_)]^3*(x_), {x_, 0, 1}] :> -45/8
,
(*X*)iT[Log[1 - (t_)*(u_)]*(t_), {t_, 0, 1}] :>
	-1/4 - 1/(2*u) + Log[1 - u]/2 - Log[1 - u]/(2*u^2) /;
FreeQ[u, t]
,
(*X*)iT[Log[1 - (t_)]*Log[1 - (t_)*(u_)]*(t_), {t_, 0, 1}] :>
	3/(4*u) - Zeta2/(2*u^2) + (2 + Zeta2)/2 - (3*Log[1 - u])/4 +
		Log[1 - u]/(4*u^2) + Log[1 - u]/(2*u) + Log[1 - u]^2/4 -
		Log[1 - u]^2/(4*u^2) - (Log[1 - u]*Log[u])/2 +
		(Log[1 - u]*Log[u])/(2*u^2) - PolyLog[2, 1 - u]/2 +
		PolyLog[2, 1 - u]/(2*u^2) /; FreeQ[u,t]
,
(*X*)iT[Log[t_]*Log[1 - (t_)*(u_)]*(t_), {t_, 0, 1}] :>
1/4 + 3/(4*u) - Zeta2/(2*u^2) - Log[1 - u]/4 + Log[1 - u]/(4*u^2) +
		(Log[1 - u]*Log[u])/(2*u^2) + PolyLog[2, 1 - u]/(2*u^2) /;
	FreeQ[u, t]
,
(*X*)iT[Log[1 - (t_)*(u_)]^2*(t_), {t_, 0, 1}] :>
	1/4 + 3/(2*u) - Log[1 - u]/2 +
	(3*Log[1 - u])/(2*u^2) - Log[1 - u]/u +
		Log[1 - u]^2/2 - Log[1 - u]^2/(2*u^2) /; FreeQ[u, t]
,
(*X*)iT[1/u_^3,{u_,0,1}] :> -1/2
,
(*X*)iT[1/(1-u_)^3,{u_,0,1}] :> -1/2
,
(*X*)iT[Log[1-y_]/y_^3,{y_,0,1}] :> 1/4
,
(*X*)iT[Log[y_]/(1-y_)^3,{y_,0,1}] :> 1/4
,
(*X*)iT[Log[1-y_]^2/y_^3,{y_,0,1}] :> 3/2+Zeta2
,
(*X*)iT[Log[y_]^2/(1-y_)^3,{y_,0,1}] :> 3/2+Zeta2
,
(*X*)iT[Log[y_] Log[1-y_]/(1-y_)^3,{y_,0,1}] :> 5/8+Zeta2/2
,
(*X*)iT[Log[y_] Log[1-y_]/y_^3,{y_,0,1}] :> 5/8+Zeta2/2
,
(*X*)iT[Log[1 - (u_)*(x_)]/(u_)^2, {u_,0,1}] :>
	-x - Log[1 - x] + x*Log[1 - x] /; FreeQ[x,u]
,
(*X*)iT[(Log[1 - (u_)]*Log[1 - (u_)*(x_)])/(u_)^3, {u_,0,1}] :>
	(3*x)/2 + x^2*Zeta2 + Log[1 - x]/2 -
	(x*Log[1 - x])/2 - Log[1 - x]^2/4 +
		(x^2*Log[1 - x]^2)/4 - (x^2*Log[1 - x]*Log[x])/2 -
		(x^2*PolyLog[2, 1 - x])/2 /; FreeQ[x,u]
,
(*X*) iT[PolyLog[2, 1 - (u_)]/(u_)^3, {u_,0,1}] :>
	-5/8 - Zeta2/2
,
(*X*) iT[PolyLog[2, (u_)]/(1-u_)^3, {u_,0,1}] :>
	-5/8 - Zeta2/2
,
(*X*)iT[(2 - (t_))^(-2), {t_, 0, 1}] :> 1/2
,
(*X*)iT[Log[1 - (t_)]/(2 - (t_))^2, {t_, 0, 1}] :> -Log[2]
,
(*X*)iT[Log[2 - (t_)]/(1 - (t_)), {t_, 0, 1}] :> Zeta2/2
,
(*X*)iT[Log[2 - (t_)]/(2 - (t_))^2, {t_, 0, 1}] :> (1 - Log[2])/2
,
(*X*)iT[Log[(1 + x_ - (t_))/x_]/(1 - (t_)), {t_, 0, 1}] :>
	Zeta2 + Log[x]^2/2 + PolyLog[2, -x] /; FreeQ[x, t]
,
(*X*)iT[Log[(1 + x_ - (t_))/x_]/(2 - (t_))^2, {t_, 0, 1}] :>
	-(Log[2]/(1 - x)) + Log[x]/2 - Log[x]/(1 - x) - Log[1 + x]/2 +
		Log[1 + x]/(1 - x) /; FreeQ[x, t]
,
(*X*)iT[Log[1 + x_ - x_*(t_)]/(1 - (t_)), {t_, 0, 1}] :>
	-Zeta2 + I*Pi*Log[1 + x] + Log[x]*Log[1 + x] +
		PolyLog[2, 1 + x] /; FreeQ[x, t]
,
(*X*)  iT[Log[1 + x_ - x_*(t_)]/(2 - (t_))^2, {t_, 0, 1}] :>
	-Log[2] + Log[2]/(1 - x) + Log[1 + x]/2 -
		Log[1 + x]/(1 - x) /; FreeQ[x, t]
,
(*X*)iT[PolyLog[2, -x_/(1-x_)^2/(1-y_)/y_], {y_,0,1}] :>
4*Log[1 - x] - 2*Log[1 - x]^2 - 4*Log[x] + (4*Log[x])/(1 - x) +
	2*Log[1 - x]*Log[x] /; FreeQ[x,y]
,
(*X*)iT[y_ PolyLog[2, -x_/(1-x_)^2/(1-y_)/y_], {y_,0,1}] :>
2*Log[1 - x] - Log[1 - x]^2 - 2*Log[x] + (2*Log[x])/(1 - x) +
	Log[1 - x]*Log[x] /; FreeQ[x,y]
,
(*X*)iT[y_^2 PolyLog[2, -x_/(1-x_)^2/(1-y_)/y_], {y_,0,1}] :>
1/3*-2*Log[1 - x]^2 + 2/3*Log[x]*Log[1 - x] + 13/9*Log[1 - x] +
	(5*Log[x])/(3*(1 - x)) - (2*Log[x])/(3*(1 - x)^2) +
	(4*Log[x])/(9*(1 - x)^3) - (13*Log[x])/9 - 4/(9*(1 - x)) +
4/(9*(1 - x)^2) /; FreeQ[x,y]
,
(*X*)iT[y_^3 PolyLog[2, -x_/(1-x_)^2/(1-y_)/y_], {y_,0,1}] :>
-(1/2)*Log[1 - x]^2 + 1/2*Log[x]*Log[1 - x] + 7/6*Log[1 - x] +
(3*Log[x])/(2*(1 - x)) - Log[x]/(1 - x)^2 + (2*Log[x])/(3*(1 - x)^3) -
	(7*Log[x])/6 - 2/(3*(1 - x)) + 2/(3*(1 - x)^2) /; FreeQ[x,y]
,
(*X*)iT[y_^4 PolyLog[2, -x_/(1-x_)^2/(1-y_)/y_], {y_,0,1}] :>
-(2/5)*Log[1 - x]^2 + 2/5*Log[x]*Log[1 - x] + 149/150*Log[1 - x] +
	(7*Log[x])/(5*(1 - x)) - (13*Log[x])/(10*(1 - x)^2) +
	(17*Log[x])/(15*(1 - x)^3) - (2*Log[x])/(5*(1 - x)^4) +
	(4*Log[x])/(25*(1 - x)^5) - (149*Log[x])/150 - 62/(75*(1 - x)) +
	74/(75*(1 - x)^2) - 8/(25*(1 - x)^3) + 4/(25*(1 - x)^4) /; FreeQ[x,y]
,
(*X*)iT[y_^5 PolyLog[2, -x_/(1-x_)^2/(1-y_)/y_], {y_,0,1}] :>
-(1/3)*Log[1 - x]^2 + 1/3*Log[x]*Log[1 - x] + 157/180*Log[1 - x] +
	(4*Log[x])/(3*(1 - x)) - (19*Log[x])/(12*(1 - x)^2) +
(31*Log[x])/(18*(1 - x)^3) - Log[x]/(1 - x)^4 + (2*Log[x])/(5*(1 - x)^5) -
(157*Log[x])/180 - 43/(45*(1 - x)) + 61/(45*(1 - x)^2) - 4/(5*(1 - x)^3) +
	2/(5*(1 - x)^4) /; FreeQ[x,y]
,
(*X*)iT[y_^6 PolyLog[2, -x_/(1-x_)^2/(1-y_)/y_], {y_,0,1}] :>
4/(49*(1 - x)^6) - 12/(49*(1 - x)^5) + 692/(735*(1 - x)^4) -
1084/(735*(1 - x)^3) + 5179/(2940*(1 - x)^2) - 3131/(2940*(1 - x)) +
	(383*Log[1 - x])/490 - (2*Log[1 - x]^2)/7 - (383*Log[x])/490 +
	(4*Log[x])/(49*(1 - x)^7) - (2*Log[x])/(7*(1 - x)^6) +
	(37*Log[x])/(35*(1 - x)^5) - (27*Log[x])/(14*(1 - x)^4) +
	(17*Log[x])/(7*(1 - x)^3) - (13*Log[x])/(7*(1 - x)^2) +
	(9*Log[x])/(7*(1 - x)) + (2*Log[1 - x]*Log[x])/7 /; FreeQ[x,y]
,
(*X*)iT[Log[(u_) - (t_)*(u_) + (x_) - (u_)*(x_)]/(1 - (t_)),
				{t_, 0, 1}] :>
	Log[1 - u]^2/2 + Log[1 - u]*Log[x] + Log[x]^2/2 -
	Log[1 - u]*Log[u + x - u*x] - Log[x]*Log[u + x - u*x] +
	Log[u + x - u*x]^2/2 + PolyLog[2, u/(u + x - u*x)] /;
FreeQ[u,t] && FreeQ[x,t]
,
(*X*)iT[PolyLog[2, (u_)/((u_)*(1 - (x_)) + (x_))], {u_, 0, 1}] :>
	Zeta2 + Log[x]^2/2 - Log[x]^2/(2*(1 - x)) + PolyLog[2, 1 - x] -
	PolyLog[2, 1 - x]/(1 - x) /; FreeQ[x,u]
,
(*X*)iT[Log[(u_)*(1 - (x_)) + (x_)]*(u_), {u_, 0, 1}] :>
	-3/4 + 1/(2*(1 - x)) + Log[x]/2 +
		Log[x]/(2*(1 - x)^2) - Log[x]/(1 - x) /; FreeQ[x,u]
,
(*X*)iT[Log[1 - (u_)]*Log[(u_)*(1 - (x_)) + (x_)]*(u_), {u_, 0, 1}] :>
	7/4 - 3/(4*(1 - x)) - (3*Log[x])/4 - Log[x]/(4*(1 - x)^2) +
		Log[x]/(1 - x) + PolyLog[2, 1 - x]/(2*(1 - x)^2) -
		PolyLog[2, 1 - x]/(1 - x) /; FreeQ[x,u]
,
(*X*)iT[Log[(u_)*(1 - (x_)) + (x_)]^2*(u_), {u_, 0, 1}] :>
	7/4 - 3/(2*(1 - x)) - (3*Log[x])/2 - (3*Log[x])/(2*(1 - x)^2) +
		(3*Log[x])/(1 - x) + Log[x]^2/2 + Log[x]^2/(2*(1 - x)^2) -
		Log[x]^2/(1 - x) /; FreeQ[x,u]
,
(*X*)iT[(u_)*PolyLog[2, (u_)/((u_)*(1 - (x_)) + (x_))], {u_, 0, 1}] :>
	Zeta2/2 + Log[x]/(2*(1 - x)^2) - Log[x]/(2*(1 - x)) + Log[x]^2/4 +
		Log[x]^2/(4*(1 - x)^2) - Log[x]^2/(2*(1 - x)) +
		PolyLog[2, 1 - x]/2 +
		PolyLog[2, 1 - x]/(2*(1 - x)^2) -
		PolyLog[2, 1 - x]/(1 - x)/; FreeQ[x,u]
,
(*X*)iT[(Log[1 - (u_)]*Log[(u_)*(1 - (x_)) + (x_)])/(1 - (u_) + (u_)*(x_)),
	{u_, 0, 1}] :>  (
	(7*Zeta2*Log[1 - x])/(1 - x) + (I*Pi*Log[1 - x]^2)/(1 - x) -
	(Zeta2*Log[x])/(1 - x) + (I*Pi*Log[1 - x]*Log[x])/(1 - x) -
	(Log[1 - x]^2*Log[x])/(1 - x) + (Log[1 - x]*Log[x]^2)/(1 - x) -
	(2*Zeta2*Log[1 + x])/(1 - x) - (2*I*Pi*Log[1 - x]*Log[1 + x])/(1 - x) -
	(Log[1 - x]*Log[x]*Log[1 + x])/(1 - x) -
	(Log[x]^2*Log[1 + x])/(2*(1 - x)) + (I*Pi*PolyLog[2, 1 - x])/(1 - x) -
	(Log[1 - x]*PolyLog[2, 1 - x])/(1 - x) +
	(Log[x]*PolyLog[2, 1 - x])/(1 - x) -
	(Log[1 + x]*PolyLog[2, 1 - x])/(1 - x) +
	(Log[x]*PolyLog[2, -x])/(1 - x) +
	(I*Pi*PolyLog[2, x])/(1 - x) - (Log[1 - x]*PolyLog[2, x])/(1 - x) +
	(2*Log[x]*PolyLog[2, x])/(1 - x) - (Log[1 + x]*PolyLog[2, x])/(1 - x) -
	(2*PolyLog[3, 1 - x])/(1 - x) - (3*PolyLog[3, -x])/(1 - x) -
	(2*PolyLog[3, x])/(1 - x) - (2*PolyLog[3, 1 + x])/(1 - x) -
	(2*PolyLog[3, -((1 + x)/(1 - x))])/(1 - x) +
	(2*PolyLog[3, (1 + x)/(1 - x)])/(1 - x) -
	(2*I*Pi*Zeta2 - 3*Zeta[3])/(2*(1 - x))
								) /; FreeQ[x, u]
,
(*X*)iT[Log[(u_)*(1 - (x_)) + (x_)]^2/(1 - (u_) + (u_)*(x_)),
				{u_, 0, 1}] :>
	(6*Zeta2*Log[1 + x])/(-1 + x) + (Log[x]^2*Log[1 + x])/(-1 + x) -
	(2*I*Pi*Log[1 + x]^2)/(-1 + x) + (2*Log[x]*PolyLog[2, -x])/(-1 + x) -
	(2*PolyLog[3, -x])/(-1 + x) - (4*PolyLog[3, 1 + x])/(-1 + x) +
	(2*Zeta[3])/(-1 + x) /; FreeQ[x, u]
,
(*X*)iT[PolyLog[2, (u_)/((u_)*(1 - (x_)) + (x_))]/(1 - (u_) + (u_)*(x_)),
	{u_, 0, 1}] :> (
(I/6*Pi^3)/(1 - x) - (I*Pi*Zeta2)/(1 - x) + (8*Zeta2*Log[1 - x])/(1 - x) +
	(I*Pi*Log[1 - x]^2)/(1 - x) - (Zeta2*Log[x])/(1 - x) -
	(Log[1 - x]^2*Log[x])/(1 - x) - (5*Log[1 - x]*Log[x]^2)/(2*(1 - x)) +
	(2*Zeta2*Log[1 + x])/(1 - x) - (3*I*Pi*Log[1 - x]*Log[1 + x])/(1 - x) +
	(I*Pi*Log[x]*Log[1 + x])/(1 - x) -
(2*Log[1 - x]*Log[x]*Log[1 + x])/(1 - x) - (Log[x]^2*Log[1 + x])/(1 - x) -
	(2*I*Pi*Log[1 + x]^2)/(1 - x) - (Log[x]*Log[1 + x]^2)/(1 - x) -
	(Log[1 - x]*PolyLog[2, 1 - x])/(1 - x) -
	(Log[x]*PolyLog[2, 1 - x])/(1 - x) -
	(Log[1 + x]*PolyLog[2, 1 - x])/(1 - x) -
	(Log[1 - x]*PolyLog[2, -x])/(1 - x) - (3*Log[x]*PolyLog[2, -x])/(1 - x) -
(Log[1 + x]*PolyLog[2, -x])/(1 - x) - (Log[1 - x]*PolyLog[2, x])/(1 - x) -
	(4*Log[x]*PolyLog[2, x])/(1 - x) - (Log[1 + x]*PolyLog[2, x])/(1 - x) -
	(Log[1 - x]*PolyLog[2, 1 + x])/(1 - x) +
	(Log[x]*PolyLog[2, 1 + x])/(1 - x) -
	(Log[1 + x]*PolyLog[2, 1 + x])/(1 - x) - (3*PolyLog[3, 1 - x])/(1 - x) +
	(4*PolyLog[3, -x])/(1 - x) + (3*PolyLog[3, x])/(1 - x) -
	(4*PolyLog[3, 1 + x])/(1 - x) -
	(2*PolyLog[3, -((1 + x)/(1 - x))])/(1 - x) +
	(2*PolyLog[3, (1 + x)/(1 - x)])/(1 - x) + (7*Zeta[3])/(2*(1 - x))
							) /; FreeQ[x,u]
,
(*X*)iT[Log[x_] PolyLog[2,-x_],{x_,0,1}] :> 3 - 4 Log[2]
,
(*X*)iT[PolyLog[3,-x_],{x_,0,1}] :> 1 + Zeta2/2 - 2*Log[2] - (3*Zeta[3])/4
,
(*X*)iT[Log[x_]^2*Log[x_ + 1],{x_,0,1}] :>
			-6 + Zeta2 + 4*Log[2] + (3*Zeta[3])/2
,
(*X*)iT[(Log[x_]*PolyLog[2, x_])/(1-x_),{x_,0,1}] :> -Pi^4/120
,
(*X*)iT[(Log[1-x_]*PolyLog[2, 1-x_])/(x_),{x_,0,1}] :> -Pi^4/120
,
(*X*)iT[Log[-x_] Log[x_] Log[x_+1],{x_,0,1}] :>
		-6 + 2*I*Pi + Pi^2/6 - I/12*Pi^3 + 4*Log[2] - 2*I*Pi*Log[2] +
	(3*Zeta[3])/2
,
(*X*)(*BMR54*)iT[PolyLog[3,-x_]/(1-x_),{x_,0,1}] :>
-(-11/10Zeta2^2+ 2 PolyLog[4,1/2]+7/4 Zeta[3] Log[2]-1/2 Zeta2 Log[2]^2
					+ 1/12 Log[2]^4)
,
(*X*)(*BMR36*)iT[Log[x_] PolyLog[2,-x_]/(1-x_),{x_,0,1}] :>
(71*Zeta2^2)/40 + Zeta2*Log[2]^2 - Log[2]^4/6 - 4*PolyLog[4, 1/2] -
	(7*Log[2]*Zeta[3])/2
,
(*X*)iT[Log[(1 - (t_) + (x_))/(x_)]/(1 - (t_) + (x_)),
				{t_, 0, 1}] :>
	Log[x]^2/2 - Log[x]*Log[1 + x] + Log[1 + x]^2/2 /; FreeQ[x,t]

,

(*X*)iT[Log[(1 - (t_) + (x_))/(x_)]/(1 + (x_) - (t_)*(x_)),
			{t_, 0, 1}] :>
(Log[1 - x]*Log[x])/x + Log[1 + x]^2/(2*x) + (2*PolyLog[2, -x])/x +
		PolyLog[2, x]/x /; FreeQ[x, t]
,
(*X*)iT[Log[1 - (u_)*(1 - 2*(x_)) - (x_)]*Log[(u_)*(1 - 2*(x_)) + (x_)],
		{u_, 0, 1}] :>
		2 + Zeta2/(1 - 2*x) - Log[1 - x] - Log[1 - x]/(1 - 2*x) - Log[x] +
		Log[x]/(1 - 2*x) + Log[1 - x]*Log[x] - (Log[1 - x]*Log[x])/(1 - 2*x) -
		(2*PolyLog[2, 1 - x])/(1 - 2*x) /; FreeQ[x,u]
,
(*X*)iT[PolyLog[2, (1 - (u_))*(1 - (x_))], {u_, 0, 1}] :>
		-1 + Log[x] - Log[x]/(1 - x) + PolyLog[2, 1 - x] /; FreeQ[x,u]
,
(*X*)iT[PolyLog[2, -((x_)/((u_)*(1 - (x_))))], {u_, 0, 1}] :>
		-Zeta2 + Log[1 - x] - Log[1 - x]^2/2 - Log[x] + Log[x]/(1 - x) +
		Log[1 - x]*Log[x] + PolyLog[2, 1 - x] /; FreeQ[x,u]
,
(*X*)iT[PolyLog[2, (u_)*(1 - 2*(x_)) + (x_)], {u_, 0, 1}] :>
		(-2 + Zeta2)/2 - Zeta2/(2*(1 - 2*x)) + Log[1 - x]/2 +
		Log[1 - x]/(2*(1 - 2*x)) + Log[x]/2 - Log[x]/(2*(1 - 2*x)) -
		(Log[1 - x]*Log[x])/2 + (Log[1 - x]*Log[x])/(2*(1 - 2*x)) +
		PolyLog[2, 1 - x]/(1 - 2*x) /; FreeQ[x,u]
,
(*X*)iT[PolyLog[2, (u_)*(1 - (x_)) + (x_)], {u_, 0, 1}] :>
		(-6 + Pi^2)/6 + Log[1 - x] - Log[1 - x]*Log[x] +
		(Log[1 - x]*Log[x])/(1 - x) - PolyLog[2, 1 - x] +
		PolyLog[2, 1 - x]/(1 - x) /; FreeQ[x,u]
,
(*X*)iT[PolyLog[2, (x_)/(1 - (u_) + (u_)*(x_))], {u_, 0, 1}] :>
		Zeta2 - Log[1 - x] + Log[x] - Log[x]/(1 - x) -
		(Log[1 - x]*Log[x])/(1 - x) - PolyLog[2, 1 - x]/(1 - x) /; FreeQ[x,u]
,
(*X*)iT[PolyLog[2, ((1 - (u_))*(x_))/(1 - (u_) + (u_)*(x_))],
				{u_, 0, 1}] :>
		Zeta2 - I*Pi*Log[1 - x] + (I*Pi*Log[1 - x])/(1 - x) -
		Log[1 - 2*x]*Log[1 - x] + (Log[1 - 2*x]*Log[1 - x])/(1 - x) +
		I*Pi*Log[x] - (I*Pi*Log[x])/(1 - x) + Log[1 - 2*x]*Log[x] -
		(Log[1 - 2*x]*Log[x])/(1 - x) + Log[1 - x]*Log[x] -
		(2*Log[1 - x]*Log[x])/(1 - x) - Log[x]^2/2 + Log[x]^2/(2*(1 - x)) +
		PolyLog[2, 1 - x] - (2*PolyLog[2, 1 - x])/(1 - x) -
		PolyLog[2, (1 - x)/x] + PolyLog[2, (1 - x)/x]/(1 - x) /; FreeQ[x,u]
,
(*X*)iT[PolyLog[2, 1 - (u_) - (x_) + 2*(u_)*(x_)], {u_, 0, 1}] :>
		(-2 + Zeta2)/2 - Zeta2/(2*(1 - 2*x)) + Log[1 - x]/2 +
		Log[1 - x]/(2*(1 - 2*x)) + Log[x]/2 - Log[x]/(2*(1 - 2*x)) -
		(Log[1 - x]*Log[x])/2 + (Log[1 - x]*Log[x])/(2*(1 - 2*x)) +
		PolyLog[2, 1 - x]/(1 - 2*x) /; FreeQ[x,u]
,
(*X*)iT[PolyLog[2, -((1 - (x_))/((1 - (u_))*(u_)*(x_)^2))],
		{u_, 0, 1}] :>
	-4*Log[1 - x] + (4*Log[1 - x])/x + 4*Log[x] + 2*Log[1 - x]*Log[x] -
	2*Log[x]^2 /; FreeQ[x,u]
,
(*X*)iT[PolyLog[2, -x_]/(1-x_), {x_,0,1}] :>  5 Zeta[3]/8
,
(*X*)iT[PolyLog[2, -x_]/(1-x_)^2, {x_,0,1}] :>  Log[2] + Log[2]^2/2
,
(*X*)iT[Log[x_] Log[x_+1]/(1-x_)^2,{x_,0,1}] :>
		(3*Zeta2)/4 - Log[2] - Log[2]^2/2
,
(*X*)iT[Log[-((t_)*(1 - (x_))) +
				(1 - (u_)*(x_))*(1 - (x_) + (u_)*(x_))]/
		(1 - (t_)), {t_, 0, 1}] :>
	-PolyLog[2, (1 - x)/((-1 + u)*u*x^2)] /; FreeQ[{u,x}, t]
,
(*X*)iT[(Log[1 - (t_)]*Log[-((t_)*(1 - (x_))) +
				(1 - (u_)*(x_))*(1 - (x_) + (u_)*(x_))])/(1 - (t_)),
				{t_, 0, 1}] :>
PolyLog[3, -((1 - x)/((1 - u)*u*x^2))] /; FreeQ[{u,x}, t]
,
(*X*)
iT[Log[-((t_)*(1 - (x_))) +
			(1 - (u_)*(x_))*(1 - (x_) + (u_)*(x_))]^2/
		(1 - (t_)), {t_, 0, 1}] :>
			(
	-2*Zeta2*Log[1 - u] - I*Pi*Log[1 - u]^2 + Log[1 - u]^3 -
	2*Zeta2*Log[u] -
	2*I*Pi*Log[1 - u]*Log[u] + 3*Log[1 - u]^2*Log[u] - I*Pi*Log[u]^2 +
	3*Log[1 - u]*Log[u]^2 + Log[u]^3 - Log[1 - u]^2*Log[1 - x] -
	2*Log[1 - u]*Log[u]*Log[1 - x] - Log[u]^2*Log[1 - x] -
	4*Zeta2*Log[x] -
	4*I*Pi*Log[1 - u]*Log[x] + 6*Log[1 - u]^2*Log[x] -
	4*I*Pi*Log[u]*Log[x] +
	12*Log[1 - u]*Log[u]*Log[x] + 6*Log[u]^2*Log[x] -
	4*Log[1 - u]*Log[1 - x]*Log[x] - 4*Log[u]*Log[1 - x]*Log[x] -
	4*I*Pi*Log[x]^2 + 12*Log[1 - u]*Log[x]^2 + 12*Log[u]*Log[x]^2 -
	4*Log[1 - x]*Log[x]^2 + 8*Log[x]^3 + I*Pi*Log[1 - u*x]^2 -
	Log[1 - u]*Log[1 - u*x]^2 - Log[u]*Log[1 - u*x]^2 +
	Log[1 - x]*Log[1 - u*x]^2 - 2*Log[x]*Log[1 - u*x]^2 +
	2*I*Pi*Log[1 - u*x]*Log[1 - x + u*x] -
	2*Log[1 - u]*Log[1 - u*x]*Log[1 - x + u*x] -
	2*Log[u]*Log[1 - u*x]*Log[1 - x + u*x] +
	2*Log[1 - x]*Log[1 - u*x]*Log[1 - x + u*x] -
4*Log[x]*Log[1 - u*x]*Log[1 - x + u*x] + I*Pi*Log[1 - x + u*x]^2 -
	Log[1 - u]*Log[1 - x + u*x]^2 - Log[u]*Log[1 - x + u*x]^2 +
	Log[1 - x]*Log[1 - x + u*x]^2 - 2*Log[x]*Log[1 - x + u*x]^2 +
2*Log[1 - u*x]*PolyLog[2, ((1 - u*x)*(1 - x + u*x))/((1 - u)*u*x^2)] +
	2*Log[1 - x + u*x]*
	PolyLog[2, ((1 - u*x)*(1 - x + u*x))/((1 - u)*u*x^2)] -
	2*PolyLog[3, ((1 - u*x)*(1 - x + u*x))/((1 - u)*u*x^2)] + 2*Zeta[3]
				) /; FreeQ[{u,x},t]
,
(*X*)iT[Log[x_] Log[x_ +1]/x_, {x_,0,1}] :> -3/4 Zeta[3]
,
(*X*)iT[PolyLog[2,-x_]/x_, {x_,0,1}] :> -3/4 Zeta[3]
,
(*X*)iT[Log[1-(x_)+(u_)*(x_)]*(u_),{u_,0,1}]:>
	-3/4+1/(2*x)+Log[1-x]/2+Log[1-x]/(2*x^2)-Log[1-x]/x /;
FreeQ[x, u]
,
(*X*)iT[Log[1-(u_)]*Log[1-(x_)+(u_)*(x_)]*(u_),{u_,0,1}]:>
			7/4+Zeta2/(2*x^2)-(3+4*Zeta2)/(4*x)-(3*Log[1-x])/4-
	Log[1-x]/(4*x^2)+Log[1-x]/x-(Log[1-x]*Log[x])/(2*x^2)+
(Log[1-x]*Log[x])/x-PolyLog[2,1-x]/(2*x^2)+PolyLog[2,1-x]/x /;
FreeQ[x, u]
,
(*X*)iT[Log[u_]*Log[1-(x_)+(u_)*(x_)]*(u_),{u_,0,1}]:>
Zeta2/(2*x^2)+(2+Zeta2)/2-(3+4*Zeta2)/(4*x)-Log[1-x]/4-
Log[1-x]/(4*x^2)+Log[1-x]/(2*x)+Log[1-x]^2/4+
Log[1-x]^2/(4*x^2)-Log[1-x]^2/(2*x)-(Log[1-x]*Log[x])/2-
(Log[1-x]*Log[x])/(2*x^2)+(Log[1-x]*Log[x])/x-
PolyLog[2,1-x]/2-PolyLog[2,1-x]/(2*x^2)+PolyLog[2,1-x]/x /;
FreeQ[x, u]
,
(*X*)iT[Log[1-(x_)+(u_)*(x_)]^2*(u_),{u_,0,1}]:>
7/4-3/(2*x)-(3*Log[1-x])/2-(3*Log[1-x])/(2*x^2)+
(3*Log[1-x])/x+Log[1-x]^2/2+Log[1-x]^2/(2*x^2)-Log[1-x]^2/x /;
FreeQ[x, u]
,
(*X*)iT[(u_)*PolyLog[2,-((1-(x_))/((1-(u_))*(u_)*(x_)^2))],{u_,0,1}]:>
-2*Log[1-x]+(2*Log[1-x])/x+2*Log[x]+Log[1-x]*Log[x]-Log[x]^2 /;
FreeQ[x, u]
,
(*X*)iT[(1 + (t_))^(-2), {t_, 0, 1}] :> 1/2
,
(*X*)iT[Log[t_]/(1 + (t_))^2, {t_, 0, 1}] :> -Log[2]
,
(*X*)iT[Log[1 + (t_)]/(1 + (t_))^2, {t_, 0, 1}] :> (1 - Log[2])/2
,
(*X*)iT[Log[(t_) + (x_)]/(t_), {t_, 0, 1}] :> -PolyLog[2, -x^(-1)] /;
	FreeQ[x, t]
,
(*X*)iT[Log[(t_) + (x_)]/(1 + (t_))^2, {t_, 0, 1}] :>
		-(Log[2]/(1 - x)) + Log[x] - Log[x]/(1 - x) - Log[1 + x]/2 +
		Log[1 + x]/(1 - x) /; FreeQ[x, t]
,
(*X*)iT[Log[1 + (t_)*(x_)]/(1 + (t_))^2, {t_, 0, 1}] :>
		-Log[2] + Log[2]/(1 - x) + Log[1 + x]/2 - Log[1 + x]/(1 - x) /;
	FreeQ[x, t]
,
(*X*)iT[Log[1 + t_ x_]/t_, {t_,0,1}] :> -PolyLog[2, -x] /;
	FreeQ[t, x]
,
(*X*)iT[Log[-((u_)*(1 - (x_))) - (x_)]*(u_), {u_, 0, 1}] :>
(-3 + 2*I*Pi)/4 + 1/(2*(1 - x)) + Log[x]/2 + Log[x]/(2*(1 - x)^2) -
	(Log[x]/(1 - x)) /; FreeQ[x,u]
,
(*X*)iT[PolyLog[2, -((1 - (x_))/((1 - (u_))*(u_)*(x_)^2))
							]/(1 - (u_)*(x_)), {u_, 0, 1}] :>
	(4*Zeta2*Log[x])/x - (2*Log[1 - x]*Log[x]^2)/x +
	(2*Log[1 - x]*PolyLog[2, 1 - x])/x - (4*Log[x]*PolyLog[2, 1 - x])/x -
	(2*PolyLog[3, 1 - x])/x - (4*PolyLog[3, x])/x + (2*Zeta[3])/x /;
		FreeQ[x,u]
,
(*X*)iT[Log[(u_)*(1 - (x_)) + (x_)]/(1 - (u_) + (u_)*(x_))^2, {u_, 0, 1}
			] :> (Log[x]/(1 + x)) /; FreeQ[x,u]
,
(*X*)iT[Log[u_*(1 - x_) + x_]^2/
		((u_)*(x_) -u_ + 1)^2, {u_, 0, 1}] :>
Zeta2/(1 - x) + Zeta2/(1 + x) - Log[x]^2/(2*(1 - x)) +
	Log[x]^2/(2*(1 + x)) + (2*Log[x]*Log[1 + x])/(1 - x) +
	(2*Log[x]*Log[1 + x])/(1 + x) + (2*PolyLog[2, -x])/(1 - x) +
	(2*PolyLog[2, -x])/(1 + x) /; FreeQ[x,u]
,
(*X*)iT[(Log[1 - u_]*Log[u_*(1 - x_) + x_])/
		((u_)*(x_) - (u_) + 1)^2, {u_, 0, 1}] :>
Zeta2/(1 - x) + Zeta2/x + Log[x]^2/(4*(1 - x)) +
Log[x]^2/(4*(1 + x)) +
	(2*Log[x]*Log[1 + x])/(1 - x) + (2*Log[x]*Log[1 + x])/x -
	PolyLog[2, 1 - x]/x + PolyLog[2, 1 - x]/(1 + x) +
	(2*PolyLog[2, -x])/(1 - x) + (2*PolyLog[2, -x])/x /; FreeQ[x,u]
,
(*X*)iT[PolyLog[2, (u_)/((u_)*(1 - (x_)) + (x_))
							]/(1 - (u_) + (u_)*(x_))^2,
		{u_, 0, 1}] :>
	Zeta2/(2*(1 - x)) + Zeta2/x - Zeta2/(2*(1 + x)) - Log[x]^2/(1 - x) +
		Log[x]^2/(1 + x) + (Log[x]*Log[1 + x])/(1 - x) -
		(Log[x]*Log[1 + x])/(1 + x) - PolyLog[2, 1 - x]/(1 - x) +
		PolyLog[2, 1 - x]/(1 + x) + PolyLog[2, -x]/(1 - x) -
		PolyLog[2, -x]/(1 + x) /; FreeQ[x, u]
,
(*X*)iT[Log[(u_)*(1 - (x_)) + (x_)]*Log[1 - (u_) + (u_)*(x_)],
					{u_, 0, 1}] :>
	2 + Zeta2 - (2*Zeta2)/(1 - x) - 2*Log[x] + (2*Log[x])/(1 - x) +
2*Log[x]*Log[1 + x] - (4*Log[x]*Log[1 + x])/(1 - x) + 2*PolyLog[2, -x] -
	(4*PolyLog[2, -x])/(1 - x) /; FreeQ[x,u]
,
(*X*)iT[((t_)*(1 - (u_)) + s_ (u_))^(-2), {t_, 0, 1}] :>
	-(1/(s*(-1 + u)*u)) - 1/((-1 + u)*(-1 + u - s*u)) /;FreeQ[{s,u},t]
,
(*X*)iT[Log[-1 + (u_) - (s_)*(u_)]*(s_), {s_, 0, 1}] :>
	(-3 + 2*I*Pi)/4 + 1/(2*u) + Log[1 - u]/2 + Log[1 - u]/(2*u^2) -
		Log[1 - u]/u   /; FreeQ[u,s]
,
(*X*)iT[Log[-1 + (u_) - (s_)*(u_)]*(s_)^2, {s_, 0, 1}] :>
	(-11 + 6*I*Pi)/18 - 1/(3*u^2) + 5/(6*u) + Log[1 - u]/3 -
		Log[1 - u]/(3*u^3) + Log[1 - u]/u^2 - Log[1 - u]/u /; FreeQ[u,s]
,
(*X*)iT[Log[1 - (s_)*(u_)*(1 - (x_))]*(s_)^2, {s_, 0, 1}] :>
	-1/9 - 1/(3*u^2*(1 - x)^2) - 1/(6*u*(1 - x)) + Log[1 - u + u*x]/3 -
		Log[1 - u + u*x]/(3*u^3*(1 - x)^3) /; FreeQ[{u,x},s]
,
(*X*)iT[Log[(u_) - (s_)*(u_)*(1 - (x_)) + (x_) - (u_)*(x_)]*(s_),
		{s_, 0, 1}] :>
	-3/4 - (x^2*Log[x])/(2*u^2*(1 - x)^2) -
		(x*(1 + 2*Log[x]))/(2*u*(1 - x)) + Log[u + x - u*x]/2 +
		(x*Log[u + x - u*x])/(u*(1 - x)) +
		(x^2*Log[u + x - u*x])/(2*u^2*(1 - x)^2) /; FreeQ[{u,x},s]
,
(*X*)iT[Log[(u_) - (s_)*(u_)*(1 - (x_)) + (x_) - (u_)*(x_)]*(s_)^2,
		{s_, 0, 1}] :>
	-11/18 - (x^3*Log[x])/(3*u^3*(1 - x)^3) -
		(x^2*(1 + 3*Log[x]))/(3*u^2*(1 - x)^2) -
		(x*(5 + 6*Log[x]))/(6*u*(1 - x)) + Log[u + x - u*x]/3 +
		(x*Log[u + x - u*x])/(u*(1 - x)) +
		(x^2*Log[u + x - u*x])/(u^2*(1 - x)^2) +
		(x^3*Log[u + x - u*x])/(3*u^3*(1 - x)^3) /; FreeQ[{u,x},s]
,
(*X*)iT[((u_)*(1 - (x_)) + (x_))^(-2), {u_, 0, 1}] :> x^(-1) /;
			FreeQ[x,u]
,
(*X*)iT[Log[1 - (u_)]/((u_)*(1 - (x_)) + (x_))^2, {u_, 0, 1}] :>
			Log[x]/(1 - x) /; FreeQ[x,u]
,
(*X*)iT[Log[u_]/((u_)*(1 - (x_)) + (x_))^2, {u_, 0, 1}] :>
	Log[x]/(1 - x) + Log[x]/x  /; FreeQ[x,u]
,
(*X*)iT[Log[1 - (u_)*(1 - (x_))]/
		((u_)*(1 - (x_)) + (x_))^2, {u_, 0, 1}] :> (Log[x]/(1 + x)) /;
				FreeQ[x,u]
,
(*X*)iT[Log[u_]/((u_)*(1 - (x_)) + (x_))^2, {u_, 0, 1}] :>
	(Log[x]/(1 - x) + Log[x]/x) /; FreeQ[x,u]
,
(*X*)iT[Log[1 - (u_)*(1 - (x_))]/
		((u_)*(1 - (x_)) + (x_))^2, {u_, 0, 1}] :> (Log[x]/(1 + x)) /;
				FreeQ[x,u]
,
(*X*)iT[Log[t_]/((t_)*(1 - (u_)) + (s_)*(u_))^2, {t_, 0, 1}] :>
	Log[-(s*u)]/(s*(1 - u)*u) - Log[-1 + u - s*u]/(s*(1 - u)*u) /;
		FreeQ[{u,s},t]
,
(*X*)iT[Log[t_]^2/((t_)*(1 - (u_)) + (s_)*(u_))^2, {t_, 0, 1}] :>
	(-2*PolyLog[2, -((1 - u)/(s*u))])/(s*(1 - u)*u) /;
		FreeQ[{u,s},t]
,
(*X*)iT[Log[t_]^2/((t_)*(1 - (u_)) + (s_)*(u_)), {t_, 0, 1}] :>
	(-2*PolyLog[3, -((1 - u)/(s*u))])/(1 - u) /; FreeQ[{s,u},t]
,
(*X*)iT[Log[(t_)*(1 - (u_)) + (s_)*(u_)]/((t_)*(1 - (u_)) + (s_)*(u_))^2,
		{t_, 0, 1}] :>
	1/(s*(1 - u)*u) - 1/((1 - u)*(1 - u + s*u)) + Log[s*u]/(s*(1 - u)*u) -
		Log[1 - (1 - s)*u]/((1 - u)*(1 - u + s*u)) /; FreeQ[{s,u},t]
,
(*X*)iT[(Log[t_]*Log[(t_)*(1 - (u_)) + (s_)*(u_)])/
		((t_)*(1 - (u_)) + (s_)*(u_))^2, {t_, 0, 1}] :>
	Log[s*u]^2/(2*s*(1 - u)*u) - Log[1 - (1 - s)*u]^2/(2*s*(1 - u)*u) -
		Log[(1 - (1 - s)*u)/(s*u)]/(s*(1 - u)*u) -
		PolyLog[2, -((1 - u)/(s*u))]/(s*(1 - u)*u) /; FreeQ[{s,u},t]
,
(*X*)iT[(Log[t_]*Log[(t_)*(1 - (u_)) + (s_)*(u_)])/
		((t_)*(1 - (u_)) + (s_)*(u_)), {t_, 0, 1}] :>
	(Pi^2*Log[s*u])/(6*(1 - u)) +
		(Log[-((1 - u)/(s*u))]*Log[s*u]^2)/(2*(1 - u)) -
		(Log[-((1 - u)/(s*u))]*Log[1 - (1 - s)*u]^2)/(2*(1 - u)) -
		(Log[1 - (1 - s)*u]*PolyLog[2, (1 - (1 - s)*u)/(s*u)])/(1 - u) +
		PolyLog[3, (1 - (1 - s)*u)/(s*u)]/(1 - u) - Zeta[3]/(1 - u)/;
			FreeQ[{s,u},t]
,
(*X*)iT[Log[(t_)*(1 - (u_)) + (s_)*(u_)]^2/((t_)*(1 - (u_)) + (s_)*(u_))^2,
		{t_, 0, 1}] :>
	2/(s*(1 - u)*u) - 2/((1 - u)*(1 - u + s*u)) +
		(2*Log[s*u])/(s*(1 - u)*u) + Log[s*u]^2/(s*(1 - u)*u) -
		(2*Log[1 - (1 - s)*u])/((1 - u)*(1 - u + s*u)) -
		Log[1 - (1 - s)*u]^2/((1 - u)*(1 - u + s*u))
,
(*X*)iT[Log[(t_)*(1 - (u_)) + (s_)*(u_)]^2/((t_)*(1 - (u_)) + (s_)*(u_)),
		{t_, 0, 1}] :>
	-Log[s*u]^3/(3*(1 - u)) + Log[1 - (1 - s)*u]^3/(3*(1 - u)) /;
				FreeQ[{u,s},t]
,
(*X*) iT[(s_)*PolyLog[2, -((1 - (u_))/((s_)*(u_)))], {s_, 0, 1}] :>
	1/4 - 1/(4*u) - Log[1 - u]/4 - Log[1 - u]/(4*u^2) + Log[1 - u]/(2*u) +
		Log[u]/4 - Log[u]^2/4 - PolyLog[2, 1 - u]/2 /; FreeQ[u,s]
,
(*X*)iT[Log[s_]*(s_)*PolyLog[2, -((1 - (u_))/((s_)*(u_)))], {s_, 0, 1}] :>
	(-2 - Zeta2)/4 - Zeta2/(4*u^2) + (1 + Zeta2)/(2*u) + Log[1 - u]/4 +
		Log[1 - u]/(4*u^2) - Log[1 - u]/(2*u) - Log[1 - u]^2/8 -
		Log[1 - u]^2/(8*u^2) + Log[1 - u]^2/(4*u) - Log[u]/4 +
		(Log[1 - u]*Log[u])/4 + (Log[1 - u]*Log[u])/(4*u^2) -
		(Log[1 - u]*Log[u])/(2*u) + Log[u]^2/8 + PolyLog[2, 1 - u]/2 +
		PolyLog[2, 1 - u]/(4*u^2) - PolyLog[2, 1 - u]/(2*u) /;
	FreeQ[u, s]
,
(*X*)iT[(s_)*PolyLog[3, -((1 - (u_))/((s_)*(u_)))], {s_, 0, 1}] :>
1/8 - 1/(8*u) - Log[1 - u]/8 - Log[1 - u]/(8*u^2) + Log[1 - u]/(4*u) +
		Log[u]/8 - Log[u]^2/8 + (Log[1 - u]*Log[u]^2)/4 + Log[u]^3/12 -
		PolyLog[2, 1 - u]/4 + (Log[u]*PolyLog[2, 1 - u])/2 +
		(Log[u]*PolyLog[2, u])/2 - PolyLog[3, 1 - u]/2 - PolyLog[3, u]/2 +
		Zeta[3]/2 /; FreeQ[u,s]
,
(*X*)iT[Log[1 - (u_) + (s_)*(u_)]*(s_)*
		PolyLog[2, (1 - (u_) + (s_)*(u_))/((s_)*(u_))], {s_, 0, 1}] :>
	-5/4 - (3*I)/4*Pi + 5/(4*u) + ((3*I)/4*Pi)/u - 2*Zeta2 + I/2*Pi*Zeta2 -
		(5*Zeta2)/(4*u^2) + (I/2*Pi*Zeta2)/u^2 + (3*Zeta2)/u - (I*Pi*Zeta2)/u -
		Log[1 - u]/4 + (5*I)/4*Pi*Log[1 - u] + Log[1 - u]/(2*u^2) +
		((5*I)/4*Pi*Log[1 - u])/u^2 - Log[1 - u]/(4*u) -
		((5*I)/2*Pi*Log[1 - u])/u + Zeta2*Log[1 - u] + (Zeta2*Log[1 - u])/u^2 -
		(2*Zeta2*Log[1 - u])/u + (5*Log[1 - u]^2)/8 - I/4*Pi*Log[1 - u]^2 +
		(5*Log[1 - u]^2)/(8*u^2) - (I/4*Pi*Log[1 - u]^2)/u^2 -
		(5*Log[1 - u]^2)/(4*u) + (I/2*Pi*Log[1 - u]^2)/u - Log[1 - u]^3/12 -
		Log[1 - u]^3/(12*u^2) + Log[1 - u]^3/(6*u) + (3*Log[u])/4 -
		(3*I)/4*Pi*Log[u] - (3*Log[u])/(4*u) + (I/2*Pi*Log[u])/u -
		(Zeta2*Log[u])/2 - (Zeta2*Log[u])/(2*u^2) + (Zeta2*Log[u])/u -
		(3*Log[1 - u]*Log[u])/4 + (Log[1 - u]*Log[u])/(2*u) + (3*Log[u]^2)/8 -
		Log[u]^2/(4*u) + (Log[1 - u]*Log[u]^2)/4 +
		(Log[1 - u]*Log[u]^2)/(4*u^2) - (Log[1 - u]*Log[u]^2)/(2*u) +
		PolyLog[2, 1 - u]/2 - I/2*Pi*PolyLog[2, 1 - u] +
		(5*PolyLog[2, 1 - u])/(4*u^2) - (I/2*Pi*PolyLog[2, 1 - u])/u^2 -
		(2*PolyLog[2, 1 - u])/u + (I*Pi*PolyLog[2, 1 - u])/u -
		(Log[1 - u]*PolyLog[2, 1 - u])/2 -
		(Log[1 - u]*PolyLog[2, 1 - u])/(2*u^2) +
		(Log[1 - u]*PolyLog[2, 1 - u])/u + (Log[u]*PolyLog[2, 1 - u])/2 +
		(Log[u]*PolyLog[2, 1 - u])/(2*u^2) - (Log[u]*PolyLog[2, 1 - u])/u +
		PolyLog[3, 1 - u]/2 + PolyLog[3, 1 - u]/(2*u^2) - PolyLog[3, 1 - u]/u +
		PolyLog[3, u]/2 + PolyLog[3, u]/(2*u^2) - PolyLog[3, u]/u - Zeta[3]/2 -
		Zeta[3]/(2*u^2) + Zeta[3]/u /; FreeQ[u,s]
,
(*X*)iT[(s_)*PolyLog[3, (1 - (u_) + (s_)*(u_))/((s_)*(u_))], {s_, 0, 1}] :>
	-Zeta2 + I/2*Pi*Zeta2 - Zeta2/(2*u^2) + (I/2*Pi*Zeta2)/u^2 +
		(3*Zeta2)/(2*u) - (I*Pi*Zeta2)/u + I/2*Pi*Log[1 - u] +
		(I/2*Pi*Log[1 - u])/u^2 - (I*Pi*Log[1 - u])/u + (Zeta2*Log[1 - u])/2 +
		(Zeta2*Log[1 - u])/(2*u^2) - (Zeta2*Log[1 - u])/u + Log[1 - u]^2/4 +
		Log[1 - u]^2/(4*u^2) - Log[1 - u]^2/(2*u) - I/2*Pi*Log[u] +
		(I/2*Pi*Log[u])/u - (3*Zeta2*Log[u])/2 - (Zeta2*Log[u])/(2*u^2) +
		(Zeta2*Log[u])/u - (Log[1 - u]*Log[u])/2 + (Log[1 - u]*Log[u])/(2*u) +
		Log[u]^2/4 - I/4*Pi*Log[u]^2 - Log[u]^2/(4*u) + (Log[1 - u]*Log[u]^2)/4 +
		(Log[1 - u]*Log[u]^2)/(4*u^2) - (Log[1 - u]*Log[u]^2)/(2*u) +
		Log[u]^3/12 - I/2*Pi*PolyLog[2, 1 - u] + PolyLog[2, 1 - u]/(2*u^2) -
		(I/2*Pi*PolyLog[2, 1 - u])/u^2 - PolyLog[2, 1 - u]/(2*u) +
		(I*Pi*PolyLog[2, 1 - u])/u - (Log[1 - u]*PolyLog[2, 1 - u])/2 -
		(Log[1 - u]*PolyLog[2, 1 - u])/(2*u^2) +
		(Log[1 - u]*PolyLog[2, 1 - u])/u + (Log[u]*PolyLog[2, 1 - u])/2 +
		(Log[u]*PolyLog[2, 1 - u])/(2*u^2) - (Log[u]*PolyLog[2, 1 - u])/u +
		PolyLog[3, 1 - u] + PolyLog[3, 1 - u]/u^2 - (2*PolyLog[3, 1 - u])/u +
		PolyLog[3, u] + PolyLog[3, u]/(2*u^2) - PolyLog[3, u]/u - Zeta[3] -
		Zeta[3]/u^2 + (2*Zeta[3])/u /; FreeQ[u,s]
,
(*X*)iT[Log[s_]*Log[-1 + (u_) - (s_)*(u_)]*(s_), {s_, 0, 1}] :>
	Zeta2/(2*u^2) + (4 - I*Pi + 2*Zeta2)/4 - (3 + 4*Zeta2)/(4*u) -
		Log[1 - u]/4 - Log[1 - u]/(4*u^2) + Log[1 - u]/(2*u) + Log[1 - u]^2/4 +
		Log[1 - u]^2/(4*u^2) - Log[1 - u]^2/(2*u) - (Log[1 - u]*Log[u])/2 -
		(Log[1 - u]*Log[u])/(2*u^2) + (Log[1 - u]*Log[u])/u -
		PolyLog[2, 1 - u]/2 - PolyLog[2, 1 - u]/(2*u^2) +
		PolyLog[2, 1 - u]/u   /; FreeQ[u,s]
,
(*X*)iT[Log[s_]^2*Log[-1 + (u_) - (s_)*(u_)]*(s_), {s_, 0, 1}] :>
	((1 + 4*Zeta2)*Log[1 - u])/4 + ((1 + 4*Zeta2)*Log[1 - u])/(4*u^2) -
		((1 + 4*Zeta2)*Log[1 - u])/(2*u) - Log[1 - u]^2/4 -
		Log[1 - u]^2/(4*u^2) + Log[1 - u]^2/(2*u) + Log[1 - u]^3/6 +
		Log[1 - u]^3/(6*u^2) - Log[1 - u]^3/(3*u) - Zeta2*Log[u] -
		(Zeta2*Log[u])/u^2 + (2*Zeta2*Log[u])/u + (Log[1 - u]*Log[u])/2 +
		(Log[1 - u]*Log[u])/(2*u^2) - (Log[1 - u]*Log[u])/u -
		(Log[1 - u]^2*Log[u])/2 - (Log[1 - u]^2*Log[u])/(2*u^2) +
		(Log[1 - u]^2*Log[u])/u + Log[1 - u]*Log[u]^2 +
		(Log[1 - u]*Log[u]^2)/u^2 - (2*Log[1 - u]*Log[u]^2)/u +
		PolyLog[2, 1 - u]/2 + PolyLog[2, 1 - u]/(2*u^2) - PolyLog[2, 1 - u]/u +
		Log[u]*PolyLog[2, 1 - u] + (Log[u]*PolyLog[2, 1 - u])/u^2 -
		(2*Log[u]*PolyLog[2, 1 - u])/u + Log[u]*PolyLog[2, u] +
		(Log[u]*PolyLog[2, u])/u^2 - (2*Log[u]*PolyLog[2, u])/u -
		PolyLog[3, 1 - u] - PolyLog[3, 1 - u]/u^2 + (2*PolyLog[3, 1 - u])/u -
		PolyLog[3, u] - PolyLog[3, u]/u^2 + (2*PolyLog[3, u])/u +
		(7 + 4*Zeta2 - 8*Zeta[3])/(4*u) - (Zeta2 - 2*Zeta[3])/(2*u^2) +
		(-17 + 2*I*Pi - 4*Zeta2 + 8*Zeta[3])/8   /; FreeQ[u,s]
,
(*X*)iT[Log[s_]*Log[1 - (u_) + (s_)*(u_)]^2*(s_), {s_, 0, 1}] :>
	(2 + Zeta2)*Log[1 - u] + ((2 + Zeta2)*Log[1 - u])/u^2 -
		(2*(2 + Zeta2)*Log[1 - u])/u - Log[1 - u]^2 - Log[1 - u]^2/u^2 +
		(2*Log[1 - u]^2)/u + Log[1 - u]^3/3 + Log[1 - u]^3/(3*u^2) -
		(2*Log[1 - u]^3)/(3*u) + (3*Log[1 - u]*Log[u])/2 +
		(3*Log[1 - u]*Log[u])/(2*u^2) - (3*Log[1 - u]*Log[u])/u -
		(Log[1 - u]^2*Log[u])/2 - (Log[1 - u]^2*Log[u])/(2*u^2) +
		(Log[1 - u]^2*Log[u])/u + (3*PolyLog[2, 1 - u])/2 +
		(3*PolyLog[2, 1 - u])/(2*u^2) - (3*PolyLog[2, 1 - u])/u -
		PolyLog[3, 1 - u] - PolyLog[3, 1 - u]/u^2 + (2*PolyLog[3, 1 - u])/u +
		(7 + 6*Zeta2 - 4*Zeta[3])/(2*u) - (3*Zeta2 - 2*Zeta[3])/(2*u^2) +
		(-31 - 12*Zeta2 + 8*Zeta[3])/8  /; FreeQ[u,s]
,
(*X*)iT[Log[1 - (u_) + (s_)*(u_)]^3*(s_), {s_, 0, 1}] :>
	-45/8 + 21/(4*u) + (21*Log[1 - u])/4 + (21*Log[1 - u])/(4*u^2) -
		(21*Log[1 - u])/(2*u) - (9*Log[1 - u]^2)/4 - (9*Log[1 - u]^2)/(4*u^2) +
		(9*Log[1 - u]^2)/(2*u) + Log[1 - u]^3/2 + Log[1 - u]^3/(2*u^2) -
		Log[1 - u]^3/u   /; FreeQ[u,s]
,
(*X*)iT[Log[1-u_]^2 Log[u_]/u_^2, {u_,0,1}] :> 2 Zeta2 - 4 Zeta[3]
,
(*X*)iT[Log[u_]^2 Log[1-u_]/u_^2, {u_,0,1}] :> -2 + 2 Zeta2 - 2 Zeta[3]
,
(*X*)iT[Log[1 - (u_)*(1 - (x_))]/(1 - (u_))^2, {u_, 0, 1}] :>
			-1 + x^(-1) - Log[x] + Log[x]/x
,
(*X*)iT[Log[(u_)*(1 - (x_)) + (x_)]/(1 - (u_))^2, {u_, 0, 1}] :>
	-1 + x - x*Log[x]
,
(*X*)iT[Log[(u_)*(1 - (x_)) + (x_)]/(u_)^2, {u_, 0, 1}] :>
-1 + x^(-1) - Log[x] + Log[x]/x
,
(*X*)iT[(Log[1 - (u_)]*PolyLog[2, 1 - (u_)])/(1 - (u_))^2, {u_, 0, 1}]:>
	3 - 2*Zeta2
,
(*X*)iT[(Log[1 - (u_)]*PolyLog[2, 1 - (u_)])/(u_)^2, {u_, 0, 1}] :>
-Zeta2 + 3*Zeta[3]
,
(*X*)iT[(Log[u_]*PolyLog[2, 1 - (u_)])/(1 - (u_))^2, {u_, 0, 1}]:>
-2*Zeta2 + Zeta[3]
,
(*X*)iT[(Log[u_]*PolyLog[2, 1 - (u_)])/(u_)^2, {u_, 0, 1}] :>
	-1 - Zeta2 + 2 Zeta[3]
,
(*X*)iT[(Log[u_]*PolyLog[2, u_])/(u_)^2, {u_, 0, 1}] :>
	3 - 2 Zeta2
,
(*X*)iT[PolyLog[3, (u_)]/(u_)^2, {u_, 0, 1}] :> 3 - Zeta2 - Zeta[3]
,
(*X*)iT[PolyLog[3, (1-u_)]/(1-u_)^2, {u_, 0, 1}] :> 3 - Zeta2 - Zeta[3]
,
(*X*)iT[Log[x_]/(1 + (x_))^3, {x_, 0, 1}] :> -1/4 - Log[2]/2
,
(*X*)iT[Log[x_]^2/(1 + (x_))^3, {x_, 0, 1}] :> Zeta2/2 + Log[2]
,
(*X*)iT[Log[x_]^2/(1 + (x_))^2, {x_, 0, 1}] :> Zeta2
,
(*X*)iT[PolyLog[2, 1 - (x_)]/(1 + (x_))^3, {x_, 0, 1}] :>
		(5*Zeta2)/16 - Log[2]/4
,
(*X*)iT[PolyLog[2, 1 - (x_)]/(1 + (x_))^2,
		{x_, 0, 1}] :> Zeta2/4
,
(*March7*)
(*X*)iT[(Log[1 - (t_)]*Log[(u_) - (t_)*(u_) + (x_) - (u_)*(x_)]
				)/(1 - (t_)),
		{t_, 0, 1}] :> PolyLog[3, -(u/((1 - u)*x))] /; FreeQ[{u,x},t]
,
(*X*)iT[Log[(u_) - (t_)*(u_) + (x_) - (u_)*(x_)]^2/(1 - (t_)), {t_, 0, 1}
	] :>
	-2*Zeta2*Log[1 - u] - I*Pi*Log[1 - u]^2 + Log[1 - u]^3 -
		Log[1 - u]^2*Log[u] - 2*Zeta2*Log[x] - 2*I*Pi*Log[1 - u]*Log[x] +
		3*Log[1 - u]^2*Log[x] - 2*Log[1 - u]*Log[u]*Log[x] - I*Pi*Log[x]^2 +
		3*Log[1 - u]*Log[x]^2 - Log[u]*Log[x]^2 + Log[x]^3 +
		I*Pi*Log[u + x - u*x]^2 - Log[1 - u]*Log[u + x - u*x]^2 +
		Log[u]*Log[u + x - u*x]^2 - Log[x]*Log[u + x - u*x]^2 +
		2*Log[u + x - u*x]*PolyLog[2, (u + x - u*x)/((1 - u)*x)] -
		2*PolyLog[3, (u + x - u*x)/((1 - u)*x)] + 2*Zeta[3] /; FreeQ[{u,x},t]
,
(*X*)iT[Log[((u_)*(1 - (x_)) + (x_))/(x_)]/(1 - (u_) + (u_)*(x_)),
				{t_, 0, 1}] :>
	-(Zeta2/(1 - x)) + Log[x]^2/(1 - x) - (2*Log[x]*Log[1 + x])/(1 - x) -
		(2*PolyLog[2, -x])/(1 - x) /; FreeQ[{u,x},t]
,
(*X*)iT[((s_) - (s_)*(x_) + t_*(u_)*(x_))^(-2), {t_, 0, 1}] :>
	1/(s*u*(1 - x)*x) - 1/(u*x*(s - s*x + u*x)) /;
		FreeQ[{s,u,x},t]
,
(*X*)iT[(t_*u_ + s_*x_ - t_*u_*x_)^(-2), {t_,0,1}] :>
	1/(s*u*(1 - x)*x) - 1/(u*(1 - x)*(u + s*x - u*x)) /;
		FreeQ[{s,u,x},t]
(* graph 11,25*)
,
(*X*)iT[Log[1 - (s_)*(1 - (x_))]*(s_)^2, {s_, 0, 1}] :>
	-(11 - 18*x + 9*x^2 - 2*x^3 + 18*x*Log[x] - 18*x^2*Log[x] + 6*x^3*Log[x])/
		(18*(1 - x)^3) /; FreeQ[x,s]
,
(*X*)iT[Log[1 - (s_)*(x_)]*(s_)^2, {s_, 0, 1}] :>
	(-6*x - 3*x^2 - 2*x^3 - 6*Log[1 - x] + 6*x^3*Log[1 - x])/(18*x^3) /;
				FreeQ[x, s]
,
(*X*)iT[Log[-((u_)*(1 - (x_))) - (s_)*(x_)]*(s_), {s_, 0, 1}] :>
	-1/4 + (u*(1 - x))/(2*x) + (u^2*(1 - x)^2*Log[u])/(2*x^2) +
		(u^2*(1 - x)^2*(I*Pi + Log[1 - x]))/(2*x^2) + Log[-u - x + u*x]/2 -
		(u^2*(1 - x)^2*Log[-u - x + u*x])/(2*x^2) /;FreeQ[{u,x}, s]
,
(*X*)iT[Log[-((u_)*(1 - (x_))) - (s_)*(x_)]*(s_)^2, {s_, 0, 1}] :>
	-1/9 - (u^2*(1 - x)^2)/(3*x^2) + (u*(1 - x))/(6*x) -
		(u^3*(1 - x)^3*Log[u])/(3*x^3) -
		(u^3*(1 - x)^3*(I*Pi + Log[1 - x]))/(3*x^3) + Log[-u - x + u*x]/3 +
		(u^3*(1 - x)^3*Log[-u - x + u*x])/(3*x^3) /;FreeQ[{u,x}, s]
,
(*X*)iT[Log[1 - (s_)*(1 - (x_)) - (u_)*(x_)]*(s_), {s_, 0, 1}] :>
	(u*x*Log[1 - u])/(1 - x)^2 - ((2 - x)*x*Log[1 - u])/(2*(1 - x)^2) -
		(u^2*x^2*Log[1 - u])/(2*(1 - x)^2) - (u^2*x^2*Log[x])/(2*(1 - x)^2) +
		(u*x*(1 - x + 2*Log[x]))/(2*(1 - x)^2) -
		(3 - 4*x + x^2 + 4*x*Log[x] - 2*x^2*Log[x])/(4*(1 - x)^2) +
		Log[1 - u*x]/(2*(1 - x)^2) - (u*x*Log[1 - u*x])/(1 - x)^2 +
		(u^2*x^2*Log[1 - u*x])/(2*(1 - x)^2)/;FreeQ[{u,x}, s]
,
(*X*)iT[Log[1 - (s_)*(1 - (x_)) - (u_)*(x_)]*(s_)^2, {s_, 0, 1}] :>
	(u*x*Log[1 - u])/(1 - x)^3 - (u^2*x^2*Log[1 - u])/(1 - x)^3 +
		(u^3*x^3*Log[1 - u])/(3*(1 - x)^3) -
		(x*(3 - 3*x + x^2)*Log[1 - u])/(3*(1 - x)^3) +
		(u^3*x^3*Log[x])/(3*(1 - x)^3) -
		(u^2*x^2*(1 - x + 3*Log[x]))/(3*(1 - x)^3) +
		(u*x*(5 - 6*x + x^2 + 6*Log[x]))/(6*(1 - x)^3) -
		(11 - 18*x + 9*x^2 - 2*x^3 + 18*x*Log[x] - 18*x^2*Log[x] +
			6*x^3*Log[x])/(18*(1 - x)^3) + Log[1 - u*x]/(3*(1 - x)^3) -
		(u*x*Log[1 - u*x])/(1 - x)^3 + (u^2*x^2*Log[1 - u*x])/(1 - x)^3 -
		(u^3*x^3*Log[1 - u*x])/(3*(1 - x)^3)/;FreeQ[{u,x}, s]
,
(*X*)iT[Log[(s_)*(1 - (x_)) + (u_)*(x_)]*(s_), {s_, 0, 1}] :>
	-1/4 + (u*x)/(2*(1 - x)) + (u^2*x^2*Log[u])/(2*(1 - x)^2) +
		(u^2*x^2*Log[x])/(2*(1 - x)^2) + Log[1 - x + u*x]/2 -
		(u^2*x^2*Log[1 - x + u*x])/(2*(1 - x)^2)/;FreeQ[{u,x}, s]
,
(*X*)iT[Log[(s_)*(1 - (x_)) + (u_)*(x_)]*(s_)^2, {s_, 0, 1}] :>
	-1/9 + (u*x)/(6*(1 - x)) - (u^2*x^2)/(3*(1 - x)^2) -
		(u^3*x^3*Log[u])/(3*(1 - x)^3) -
		(u^3*x^3*(I*Pi + Log[x]))/(3*(1 - x)^3) +
		(u^3*x^3*Log[-1 + x - u*x])/(3*(1 - x)^3) + Log[1 - x + u*x]/3 /;
			FreeQ[{u,x}, s]
,
(*X*)iT[Log[1 - (u_) - (s_)*(x_) + (u_)*(x_)]*(s_), {s_, 0, 1}] :>
	(u*(1 - x)*Log[1 - u])/x^2 - (u^2*(1 - x)^2*Log[1 - u])/(2*x^2) -
		((1 - x^2)*Log[1 - u])/(2*x^2) - (u^2*(1 - x)^2*Log[1 - x])/(2*x^2) +
		(u*(1 - x)*(x + 2*Log[1 - x]))/(2*x^2) -
		(2*x + x^2 + 2*Log[1 - x] - 2*x^2*Log[1 - x])/(4*x^2) +
		Log[1 - u + u*x]/(2*x^2) - (u*(1 - x)*Log[1 - u + u*x])/x^2 +
		(u^2*(1 - x)^2*Log[1 - u + u*x])/(2*x^2)/;FreeQ[{u,x}, s]
,
(*X*)iT[Log[1 - (u_) - (s_)*(x_) + (u_)*(x_)]*(s_)^2, {s_, 0, 1}] :>
	(u*(1 - x)*Log[1 - u])/x^3 - (u^2*(1 - x)^2*Log[1 - u])/x^3 +
		(u^3*(1 - x)^3*Log[1 - u])/(3*x^3) -
		((1 - x)*(1 + x + x^2)*Log[1 - u])/(3*x^3) +
		(u^3*(1 - x)^3*Log[1 - x])/(3*x^3) -
		(u^2*(1 - x)^2*(x + 3*Log[1 - x]))/(3*x^3) +
		(u*(1 - x)*(4*x + x^2 + 6*Log[1 - x]))/(6*x^3) -
		(6*x + 3*x^2 + 2*x^3 + 6*Log[1 - x] - 6*x^3*Log[1 - x])/(18*x^3) +
		Log[1 - u + u*x]/(3*x^3) - (u*(1 - x)*Log[1 - u + u*x])/x^3 +
		(u^2*(1 - x)^2*Log[1 - u + u*x])/x^3 -
		(u^3*(1 - x)^3*Log[1 - u + u*x])/(3*x^3) /;FreeQ[{u,x}, s]

,
(*X*)iT[Log[-((u_)*(1 - (x_))) - (x_)]/(1 - (u_) + (u_)*(x_))^2,
				{u_, 0, 1}] :> (I*Pi)/x + Log[x]/(1 + x) /; FreeQ[u,x]
,
(*X*)iT[Log[1 - (u_)*(x_)]/
		(1 - (u_) + (u_)*(x_))^2, {u_, 0, 1}] :>
	Log[1 - x]/(1 - 2*x) + Log[1 - x]/x - Log[x]/(1 - 2*x) +
			Log[x]/(1 - x) /;  FreeQ[u,x]
,
(*X*)iT[Log[-1 + (x_) - (u_)*(x_)]/(1 - (u_) + (u_)*(x_))^2, {u_, 0, 1}] :>
	(I*Pi)/x - Log[1 - x]/(1 - x + x^2) + (x*Log[1 - x])/(1 - x + x^2) +
		Log[x]/(1 - x) - Log[x]/(1 - x + x^2) + (x*Log[x])/(1 - x + x^2) /;
			FreeQ[u,x]
,
(*XU*)iT[Log[1 - (x_) + (u_)*(x_)]/(u_)^2, {u_, 0, 1}] :>
	-1 + (1 - x)^(-1) - Log[1 - x] + Log[1 - x]/(1 - x) /;  FreeQ[u,x]
,
(*X*)iT[Log[1 - (x_) + (u_)*(x_)]/(u_), {u_, 0, 1}] :>
	Zeta2 + Log[1 - x]^2/2 - Log[1 - x]*Log[x] -
			PolyLog[2, 1 - x] /; FreeQ[u,x]
,
(*X*)iT[Log[1 - (x_) + (u_)*(x_)]/(1 - (u_) + (u_)*(x_))^2, {u_, 0, 1}] :>
	-(Log[1 - x]/(1 - x + x^2)) + (x*Log[1 - x])/(1 - x + x^2) +
		Log[x]/(1 - x) - Log[x]/(1 - x + x^2) +
		(x*Log[x])/(1 - x + x^2) /; FreeQ[u,x]
,
(*X*) iT[Log[t_]/((t_)*(u_) + (s_)*(x_) - (t_)*(u_)*(x_))^2,
					{t_, 0, 1}] :>
	Log[s]/(s*u*(1 - x)*x) + (I*Pi + Log[x])/(s*u*(1 - x)*x) -
		Log[-u - s*x + u*x]/(s*u*(1 - x)*x) /; FreeQ[{s, u, x}, t]
,
(*X*)iT[Log[t_]/((s_) - (s_)*(x_) + (t_)*(u_)*(x_))^2, {t_, 0, 1}] :>
	Log[s]/(s*u*(1 - x)*x) + (I*Pi + Log[1 - x])/(s*u*(1 - x)*x) -
		Log[-s + s*x - u*x]/(s*u*(1 - x)*x) /; FreeQ[{s, u, x}, t]
,
(*X*)iT[Log[1 - (t_)*(u_)*(1 - (x_)) - (s_)*(x_)]/
		((t_)*(u_) + (s_)*(x_) - (t_)*(u_)*(x_))^2, {t_, 0, 1}] :>
	Log[s]/(u*(1 - x)) + (I*Pi + Log[x])/(u*(1 - x)) -
		Log[1 - s*x]/(u*(1 - x)) + Log[1 - s*x]/(s*u*(1 - x)*x) -
	Log[-u - s*x + u*x]/(u*(1 - x)) + Log[1 - u - s*x + u*x]/(u*(1 - x)) -
		Log[1 - u - s*x + u*x]/(u*(1 - x)*(u + s*x - u*x)) /;
					FreeQ[{s, u, x}, t]
,
(*X*)iT[Log[(t_)*(u_)*(1 - (x_)) + (s_)*(x_)]/
		((t_)*(u_) + (s_)*(x_) - (t_)*(u_)*(x_))^2, {t_, 0, 1}] :>
	-(1/(u*(1 - x)*(u + s*x - u*x))) + Log[s]/(s*u*(1 - x)*x) +
		(1 + Log[x])/(s*u*(1 - x)*x) -
		Log[u + s*x - u*x]/(u*(1 - x)*(u + s*x - u*x)) /; FreeQ[{s, u, x}, t]
,
(*X*)iT[Log[1 - (s_) + (s_)*(x_) - (t_)*(u_)*(x_)]/
		((s_) - (s_)*(x_) + (t_)*(u_)*(x_))^2, {t_, 0, 1}] :>
	Log[s]/(u*x) + (I*Pi + Log[1 - x])/(u*x) - Log[1 - s + s*x]/(u*x) +
		Log[1 - s + s*x]/(s*u*(1 - x)*x) - Log[-s + s*x - u*x]/(u*x) +
		Log[1 - s + s*x - u*x]/(u*x) -
		Log[1 - s + s*x - u*x]/(u*x*(s - s*x + u*x)) /; FreeQ[{s, u, x}, t]
,
(*X*)iT[Log[(s_)*(1 - (x_)) + (t_)*(u_)*(x_)]/
		((s_) - (s_)*(x_) + (t_)*(u_)*(x_))^2, {t_, 0, 1}] :>
	-(1/(u*x*(s - s*x + u*x))) + Log[s]/(s*u*(1 - x)*x) +
		(1 + Log[1 - x])/(s*u*(1 - x)*x) -
		Log[s - s*x + u*x]/(u*x*(s - s*x + u*x)) /; FreeQ[{s, u, x}, t]
,
(*X*)iT[((t_)*(1 - (x_)) + (u_)*(x_)^2 - (u_)^2*(x_)^2)^(-2), {t_, 0, 1}]:>
	1/((1 - u)*(1 - x)*x^2) + 1/(u*(1 - x)*x^2) -
		1/((1 - x)*(2 - x)*(1 - u*x)) - 1/((1 - x)*(2 - x)*(1 - x + u*x)) /;
			FreeQ[{u,x},t]
,
(*X*)iT[Log[t_]/((t_)*(1 - (x_)) + (u_)*(x_)^2 - (u_)^2*(x_)^2)^2,
		{t_, 0, 1}] :>
	Log[1 - u]/((1 - u)*(1 - x)*x^2) + Log[1 - u]/(u*(1 - x)*x^2) +
		Log[u]/((1 - u)*(1 - x)*x^2) + Log[u]/(u*(1 - x)*x^2) +
		(2*Log[x])/((1 - u)*(1 - x)*x^2) + (2*Log[x])/(u*(1 - x)*x^2) -
		Log[1 - u*x]/((1 - u)*(1 - x)*x^2) - Log[1 - u*x]/(u*(1 - x)*x^2) -
		Log[1 - x + u*x]/((1 - u)*(1 - x)*x^2) -
			Log[1 - x + u*x]/(u*(1 - x)*x^2) /; FreeQ[{u,x},t]
,
(*X*)iT[Log[(t_)*(1 - (x_)) + (1 - (u_))*(u_)*(x_)^2]/((t_)*(1 - (x_)) +
				(u_)*(x_)^2 - (u_)^2*(x_)^2)^2, {t_, 0, 1}] :>
	-(1/((1 - x)*(2 - x)*(1 - u*x))) - 1/((1 - x)*(2 - x)*(1 - x + u*x)) +
		Log[1 - u]/((1 - u)*(1 - x)*x^2) + Log[1 - u]/(u*(1 - x)*x^2) +
		Log[u]/((1 - u)*(1 - x)*x^2) + Log[u]/(u*(1 - x)*x^2) +
		(1 + 2*Log[x])/((1 - u)*(1 - x)*x^2) + (1 + 2*Log[x])/(u*(1 - x)*x^2) -
		Log[1 - u*x]/((1 - x)*(2 - x)*(1 - u*x)) -
		Log[1 - u*x]/((1 - x)*(2 - x)*(1 - x + u*x)) -
		Log[1 - x + u*x]/((1 - x)*(2 - x)*(1 - u*x)) -
		Log[1 - x + u*x]/((1 - x)*(2 - x)*(1 - x + u*x)) /; FreeQ[{u,x},t]
,
(*X*)iT[(Log[1 - (t_)]*Log[t_])/(1 - (x_) + (t_)*(x_))^2, {t_, 0, 1}] :>
	(2*Zeta2)/x + Log[1 - x]^2/(2*x) - (Log[1 - x]*Log[x])/(1 - x) -
		(2*Log[1 - x]*Log[x])/x - PolyLog[2, 1 - x]/(1 - x) -
		(2*PolyLog[2, 1 - x])/x  /; FreeQ[x,t]
,
(*X*)iT[Log[t_]^2/(1 - (x_) + (t_)*(x_))^2,
		{t_, 0, 1}] :>
(2*Zeta2)/(1 - x) + (2*Zeta2)/x + Log[1 - x]^2/(1 - x) + Log[1 - x]^2/x -
		(2*Log[1 - x]*Log[x])/(1 - x) - (2*Log[1 - x]*Log[x])/x -
		(2*PolyLog[2, 1 - x])/(1 - x) - (2*PolyLog[2, 1 - x])/x /; FreeQ[x,t]
,
(*X*)iT[(Log[1 - (t_)]*Log[1 - (x_) + (t_)*(x_)])/(1 - (x_) + (t_)*(x_))^2,
		{t_, 0, 1}] :>
	Zeta2/x + Log[1 - x]/x + Log[1 - x]^2/(2*x) - (Log[1 - x]*Log[x])/x -
		PolyLog[2, 1 - x]/x /; FreeQ[x,t]
,
(*X*)iT[(Log[t_]*Log[1 - (x_) + (t_)*(x_)])/
		(1 - (x_) + (t_)*(x_))^2, {t_, 0, 1}] :>
	Zeta2/(1 - x) + Zeta2/x + Log[1 - x]/(1 - x) + Log[1 - x]/x +
		Log[1 - x]^2/(1 - x) + Log[1 - x]^2/x - (Log[1 - x]*Log[x])/(1 - x) -
		(Log[1 - x]*Log[x])/x - PolyLog[2, 1 - x]/(1 - x) -
	PolyLog[2, 1 - x]/x /; FreeQ[x,t]
,
(*X*)iT[Log[1 - (x_) + (t_)*(x_)]^2/(1 - (x_) + (t_)*(x_))^2,
				{t_, 0, 1}] :>
	2/(1 - x) + (2*Log[1 - x])/(1 - x) + (2*Log[1 - x])/x +
		Log[1 - x]^2/(1 - x) + Log[1 - x]^2/x /; FreeQ[x,t]
,
(*X*)iT[PolyLog[2, -(((1 - u_)*u_*x_^2)/(1 - x_))]/(1 - u_),
				{u_, 0, 1}] :>
	Log[1 - x]^3/6 - Log[1 - x]^2*Log[x] - 2*Log[1 - x]*PolyLog[2, 1 - x] +
	2*PolyLog[3, 1 - x] - 2*Zeta[3] /; FreeQ[x, u]
,
(*X*)iT[PolyLog[2, -(((1 - (u_))*(u_)*(x_)^2)/(1 - (x_)))]/
		(1 - (x_) + (u_)*(x_)), {u_, 0, 1}] :> Log[1 - x]^3/(6*x) /;
			FreeQ[x, u]
,
(*X*)iT[Log[1 - u_*(1 - x_)]^2/(u_*(1 - x_) + x_), {u_, 0, 1}] :>
	(2*Zeta2*Log[x])/(1 - x) - (10*Zeta2*Log[1 + x])/(1 - x) -
		(2*I*Pi*Log[x]*Log[1 + x])/(1 - x) - (3*Log[x]^2*Log[1 + x])/(1 - x) +
		(6*I*Pi*Log[1 + x]^2)/(1 - x) + (4*Log[x]*Log[1 + x]^2)/(1 - x) -
		(4*Log[x]*PolyLog[2, -x])/(1 - x) +
		(4*Log[1 + x]*PolyLog[2, -x])/(1 - x) -
		(2*Log[x]*PolyLog[2, 1 + x])/(1 - x) +
		(4*Log[1 + x]*PolyLog[2, 1 + x])/(1 - x) + (2*PolyLog[3, -x])/(1 - x) +
		(4*PolyLog[3, 1 + x])/(1 - x) - (2*Zeta[3])/(1 - x) /; FreeQ[x,u]
,
(*X*)iT[(Log[1 - u_*(1 - x_)]*Log[u_*(1 - x_) + x_])/(u_*(1 - x_) + x_),
				{u_, 0, 1}] :>
	(Zeta2*Log[x])/(1 - x) - (5*Zeta2*Log[1 + x])/(1 - x) -
(I*Pi*Log[x]*Log[1 + x])/(1 - x) - (3*Log[x]^2*Log[1 + x])/(2*(1 - x)) +
		(3*I*Pi*Log[1 + x]^2)/(1 - x) + (2*Log[x]*Log[1 + x]^2)/(1 - x) -
		(2*Log[x]*PolyLog[2, -x])/(1 - x) +
		(2*Log[1 + x]*PolyLog[2, -x])/(1 - x) -
		(Log[x]*PolyLog[2, 1 + x])/(1 - x) +
		(2*Log[1 + x]*PolyLog[2, 1 + x])/(1 - x) + PolyLog[3, -x]/(1 - x) +
		(2*PolyLog[3, 1 + x])/(1 - x) - Zeta[3]/(1 - x) /; FreeQ[x,u]
,
(*X*)iT[Log[u_*(1 - x_) + x_]^2/(u_*(1 - x_) + x_), {u_, 0, 1}] :>
	-Log[x]^3/(3*(1 - x)) /; FreeQ[x,u]
,
(*X*)iT[Log[1+x_]^2, {x_,0,1}] :> 2 - 4*Log[2] + 2*Log[2]^2
,
(*X*)iT[Log[1+x_]^3, {x_,0,1}] :> -6 + 12*Log[2] - 6*Log[2]^2 + 2*Log[2]^3
,
(*X*)iT[PolyLog[3,1/(1+x_)],{x_,0,1}] :>
		6*Log[2] - (Pi^2*Log[2])/6 - Log[2]^2 + Log[2]^3/3 - 2*Log[4] +
	(3*Zeta[3])/4
,
(*X*)iT[Log[1+x_] PolyLog[2,-x_], {x_,0,1}] :>
			3 + Zeta2/2 - 6*Log[2] - Zeta2*Log[2] + 2*Log[2]^2 + Zeta[3]/4
,
(*X*)iT[Log[1+x_] Li2[1+x_] ,{x_,0,1}] :>
			3 - 2*I*Pi - (5*Zeta2)/2 - 2*Log[2] + 4*I*Pi*Log[2] +
	3*Zeta2*Log[2] - 2*I*Pi*Log[2]^2
,
(*X*)iT[PolyLog[2,u_]/u_, {u_,0,1}] :> Zeta[3]
,
(*X*)iT[Log[1 + (x_)]*(x_), {x_, 0, 1}] :> 1/4
,
(*X*)iT[Log[x_]^2*Log[1 + (x_)]*(x_), {x_, 0, 1}] :>
	11/8 - Zeta2/4 - (3*Zeta[3])/4
,
(*X*)iT[Log[1 + (x_)]^3*(x_), {x_, 0, 1}] :> 39/8 - 9*Log[2] + 3*Log[2]^2
,
(*X*)iT[Log[x_]*(x_)*PolyLog[2, -(x_)], {x_, 0, 1}] :> -5/16 + Zeta2/4
,
(*X*)iT[(x_)*PolyLog[3, -(x_)], {x_, 0, 1}] :> -1/16 + Zeta2/8 - (3*Zeta[3])/8
,
(*X*)iT[(x_)*PolyLog[3, (1 + (x_))^(-1)], {x_, 0, 1}] :>
	1/8 + Zeta2/4 - (3*Log[2])/2 + Log[2]^2/2 + Zeta[3]/2
,
(*X*)iT[PolyLog[2, (((u_) + (x_) - (u_)*(x_))*(1 - (u_) + (u_)*(x_)))/
		((1 - (u_))*(u_)*(1 - (x_))^2)], {u_, 0, 1}] :>
	3*Zeta2 + 2*I*Pi*Log[1 - x] - 2*Log[1 - x]^2 - 2*I*Pi*Log[x] +
	(2*I*Pi*Log[x])/(1 - x) + 4*Log[1 - x]*Log[x] -
	(4*Log[1 - x]*Log[x])/(1 - x) - Log[x]^2 + Log[x]^2/(1 - x) +
	2*PolyLog[2, 1 - x] - (4*PolyLog[2, 1 - x])/(1 - x) /;
			FreeQ[x, u]
,
(*X*)iT[Log[-((t_)*(x_)) + ((u_) + (x_) - (u_)*(x_))*
				(1 - (u_) + (u_)*(x_))]/(1 - (t_)), {t_, 0, 1}] :>
	-Zeta2 - I*Pi*Log[1 - u] + Log[1 - u]^2 - I*Pi*Log[u] +
	2*Log[1 - u]*Log[u] + Log[u]^2 - 2*I*Pi*Log[1 - x] +
	4*Log[1 - u]*Log[1 - x] + 4*Log[u]*Log[1 - x] + 4*Log[1 - x]^2 -
	Log[1 - u]*Log[x] - Log[u]*Log[x] - 2*Log[1 - x]*Log[x] +
	I*Pi*Log[u + x - u*x] - Log[1 - u]*Log[u + x - u*x] -
	Log[u]*Log[u + x - u*x] - 2*Log[1 - x]*Log[u + x - u*x] +
	Log[x]*Log[u + x - u*x] + I*Pi*Log[1 - u + u*x] -
	Log[1 - u]*Log[1 - u + u*x] - Log[u]*Log[1 - u + u*x] -
	2*Log[1 - x]*Log[1 - u + u*x] + Log[x]*Log[1 - u + u*x] +
	PolyLog[2, ((u + x - u*x)*(1 - u + u*x))/((1 - u)*u*(1 - x)^2)] /;
		FreeQ[{u, x}, t]
,
(*X*)iT[PolyLog[2, (((u_)*(1 - (x_)) + (x_))*(1 - (u_) + (u_)*(x_)))/
		((1 - (u_))*(u_)*(1 - (x_))^2)], {u_, 0, 1}] :>
	Pi^2/2 + 2*I*Pi*Log[1 - x] - 2*Log[1 - x]^2 - 2*I*Pi*Log[x] +
	(2*I*Pi*Log[x])/(1 - x) + 4*Log[1 - x]*Log[x] -
	(4*Log[1 - x]*Log[x])/(1 - x) - Log[x]^2 + Log[x]^2/(1 - x) +
	2*PolyLog[2, 1 - x] - (4*PolyLog[2, 1 - x])/(1 - x) /; FreeQ[x,u]
,
(*X*)iT[Log[1 - (u_)]^2/((u_)*(1 - (x_)) + (x_)), {u_, 0, 1}] :>
	(2*PolyLog[3, 1 - x])/(1 - x) /; FreeQ[x, u]
,
(*X*)iT[(Log[1 - (u_)]*Log[u_])/((u_)*(1 - (x_)) + (x_)), {u_, 0, 1}] :>
	(Log[1 - x]*Log[x]^2)/(2*(1 - x)) + (2*PolyLog[3, 1 - x])/(1 - x) +
		PolyLog[3, x]/(1 - x) - Zeta[3]/(1 - x) /; FreeQ[x, u]
,
(*X*)iT[Log[u_]^2/((u_)*(1 - (x_)) + (x_)), {u_, 0, 1}] :>
	(-2*Zeta2*Log[x])/(1 - x) + (Log[1 - x]*Log[x]^2)/(1 - x) -
		Log[x]^3/(3*(1 - x)) + (2*PolyLog[3, 1 - x])/(1 - x) +
		(2*PolyLog[3, x])/(1 - x) - (2*Zeta[3])/(1 - x) /; FreeQ[x, u]
,
(*X*)iT[(Log[1 - (u_)]*Log[1 - (u_)*(1 - (x_))])/((u_)*(1 - (x_)) + (x_)),
		{u_, 0, 1}] :>
	(6*Zeta2*Log[1 - x])/(1 - x) + (I*Pi*Log[1 - x]^2)/(1 - x) -
		(7*Zeta2*Log[1 + x])/(1 - x) - (2*I*Pi*Log[1 - x]*Log[1 + x])/(1 - x) -
		(Log[x]^2*Log[1 + x])/(2*(1 - x)) + (I*Pi*Log[1 + x]^2)/(1 - x) +
		Log[1 + x]^3/(3*(1 - x)) - (Log[x]*PolyLog[2, -x])/(1 - x) -
		(2*PolyLog[3, 1 - x])/(1 - x) + PolyLog[3, -x]/(1 - x) -
		(2*PolyLog[3, (1 + x)^(-1)])/(1 - x) -
		(2*PolyLog[3, -((1 + x)/(1 - x))])/(1 - x) +
		(2*PolyLog[3, (1 + x)/(1 - x)])/(1 - x) + (5*Zeta[3])/(2*(1 - x)) /;
				FreeQ[x, u]
,
(*X*)iT[(Log[u_]*Log[1 - (u_)*(1 - (x_))])/((u_)*(1 - (x_)) + (x_)),
		{u_, 0, 1}] :>
	(6*Zeta2*Log[1 - x])/(1 - x) + (I*Pi*Log[1 - x]^2)/(1 - x) +
		(Zeta2*Log[x])/(1 - x) - (Log[1 - x]*Log[x]^2)/(1 - x) -
		(7*Zeta2*Log[1 + x])/(1 - x) - (2*I*Pi*Log[1 - x]*Log[1 + x])/(1 - x) -
		(Log[x]^2*Log[1 + x])/(2*(1 - x)) + (I*Pi*Log[1 + x]^2)/(1 - x) +
		Log[1 + x]^3/(3*(1 - x)) - (Log[x]*PolyLog[2, 1 - x])/(1 - x) +
		(Log[x]*PolyLog[2, -x])/(1 - x) - (2*PolyLog[3, 1 - x])/(1 - x) -
		(3*PolyLog[3, -x])/(1 - x) - (2*PolyLog[3, x])/(1 - x) -
		(2*PolyLog[3, (1 + x)^(-1)])/(1 - x) -
		(2*PolyLog[3, -((1 + x)/(1 - x))])/(1 - x) +
		(2*PolyLog[3, (1 + x)/(1 - x)])/(1 - x) + (3*Zeta[3])/(2*(1 - x)) /;
				FreeQ[x, u]
,
(*X*)iT[(Log[1 - (u_)]*Log[(u_)*(1 - (x_)) + (x_)])/((u_)*(1 - (x_)) + (x_)),
		{u_, 0, 1}] :>
	(Zeta2*Log[x])/(1 - x) - (Log[1 - x]*Log[x]^2)/(2*(1 - x)) -
		(Log[x]*PolyLog[2, 1 - x])/(1 - x) - PolyLog[3, x]/(1 - x) +
		Zeta[3]/(1 - x) /; FreeQ[x, u]
,
(*X*)iT[(Log[u_]*Log[(u_)*(1 - (x_)) + (x_)])/
		(1 - (u_) + (u_)*(x_)), {u_, 0, 1}] :>
	(6*Zeta2*Log[1 - x])/(1 - x) + (I*Pi*Log[1 - x]^2)/(1 - x) -
		(7*Zeta2*Log[1 + x])/(1 - x) - (2*I*Pi*Log[1 - x]*Log[1 + x])/(1 - x) -
		(Log[x]^2*Log[1 + x])/(2*(1 - x)) + (I*Pi*Log[1 + x]^2)/(1 - x) +
		Log[1 + x]^3/(3*(1 - x)) - (Log[x]*PolyLog[2, -x])/(1 - x) -
		(2*PolyLog[3, 1 - x])/(1 - x) + PolyLog[3, -x]/(1 - x) -
		(2*PolyLog[3, (1 + x)^(-1)])/(1 - x) -
		(2*PolyLog[3, -((1 + x)/(1 - x))])/(1 - x) +
		(2*PolyLog[3, (1 + x)/(1 - x)])/(1 - x) + (5*Zeta[3])/(2*(1 - x)) /;
			FreeQ[x, u]
,
(*X*)iT[PolyLog[2, (u_)/((u_)*(1 - (x_)) + (x_))]/((u_)*(1 - (x_)) + (x_)),
		{u_, 0, 1}] :>
	(-2*Zeta2*Log[x])/(1 - x) + (Log[1 - x]*Log[x]^2)/(2*(1 - x)) +
		(Log[x]*PolyLog[2, 1 - x])/(1 - x) - PolyLog[3, 1 - x]/(1 - x) +
		PolyLog[3, x]/(1 - x) - Zeta[3]/(1 - x) /; FreeQ[x, u]
,
(*X*)iT[(Log[1 - (x_)]*PolyLog[2, x_])/(1-x_), {x_, 0, 1}] :> Pi^4/30
,
(*X*)iT[(Log[1 - (x_)]*PolyLog[2, x_])/(x_), {x_, 0, 1}] :> -Pi^4/72
,
(*X*)iT[PolyLog[3, 1 - (x_)]/(x_), {x_, 0, 1}] :> -Pi^4/72
,
(*X*)iT[PolyLog[2, -((x_)/((1 - (u_))*(u_)*(1 - (x_))^2))]/
		(1 - (u_) + (u_)*(x_)), {u_, 0, 1}] :>
	(2*Zeta2*Log[x])/(1 - x) + (2*Log[1 - x]^2*Log[x])/(1 - x) -
	(2*Log[1 - x]*Log[x]^2)/(1 - x) +
	(4*Log[1 - x]*PolyLog[2, 1 - x])/(1 - x) -
	(2*Log[x]*PolyLog[2, 1 - x])/(1 - x) - (4*PolyLog[3, 1 - x])/(1 - x) -
	(2*PolyLog[3, x])/(1 - x) + (2*Zeta[3])/(1 - x) /; FreeQ[x,u]
,
(*X*)iT[PolyLog[2, -((u_)/((1 - (u_))*(x_)))]/((u_)*(1 - (x_)) + (x_)),
		{u_, 0, 1}] :>
	(3*Zeta2*Log[x])/(1 - x) - (Log[1 - x]*Log[x]^2)/(1 - x) +
		Log[x]^3/(6*(1 - x)) - (Log[x]*PolyLog[2, 1 - x])/(1 - x) -
		(2*PolyLog[3, x])/(1 - x) + (2*Zeta[3])/(1 - x)  /;
			FreeQ[x,u]
,
(*X*)iT[PolyLog[2, -((u_)/((1 - (u_))*(x_)))]/(1 - (u_) + (u_)*(x_)),
		{u_, 0, 1}] :>
	(5*Zeta2*Log[x])/(1 - x) - (3*Log[1 - x]*Log[x]^2)/(1 - x) +
		(7*Log[x]^3)/(6*(1 - x)) - (3*Log[x]*PolyLog[2, 1 - x])/(1 - x) +
		(4*Log[x]*PolyLog[2, -x])/(1 - x) - (8*PolyLog[3, -x])/(1 - x) -
		(6*PolyLog[3, x])/(1 - x) /; FreeQ[x,u]
,
(*X*)iT[(Log[u_]*Log[(u_)*(1 - (x_)) + (x_)])/(u_)^2, {u_, 0, 1}] :>
	-1 + x^(-1) - Log[x] + Log[x]/x - Log[x]^2/2 + Log[x]^2/(2*x) -
	PolyLog[2, 1 - x] + PolyLog[2, 1 - x]/x /; FreeQ[x,u]
,
(*UX*)iT[(Log[u_]*Log[1 - (u_)*(1 - (x_))])/(u_)^2, {u_, 0, 1}] :>
	-1 + x - x*Log[x] - (-1 + x)*PolyLog[2, 1 - x] /; FreeQ[x,u]
,
(*UX*)iT[(Log[u_]*Log[1 - (x_) + (u_)*(x_)])/(u_)^2, {u_, 0, 1}] :>
	-1 - Zeta2 + (1 + Zeta2)/(1 - x) - Log[1 - x] + Log[1 - x]/(1 - x) -
	Log[1 - x]^2/2 + Log[1 - x]^2/(2*(1 - x)) + Log[1 - x]*Log[x] -
	(Log[1 - x]*Log[x])/(1 - x) + PolyLog[2, 1 - x] -
	PolyLog[2, 1 - x]/(1 - x) /; FreeQ[x,u]
,
(*X*)iT[(Log[1 - (u_)]*Log[1 - (x_) + (u_)*(x_)])/(u_), {u_, 0, 1}] :>
	Log[1 - x]*PolyLog[2, 1 - x] - 2*PolyLog[3, 1 - x] - PolyLog[3, x] +
		2*Zeta[3] /; FreeQ[x,u]
,
(*X*)iT[Log[1 - (x_) + (u_)*(x_)]^2/(u_), {u_, 0, 1}] :>
	2*Zeta2*Log[1 - x] + (2*Log[1 - x]^3)/3 - Log[1 - x]^2*Log[x] -
		2*PolyLog[3, 1 - x] + 2*Zeta[3] /; FreeQ[x,u]
,
(*X*)iT[PolyLog[2, ((1 - (u_)*(x_))*(1 - (x_) + (u_)*(x_)))/
			((1 - (u_))*(u_)*(x_)^2)]/(u_), {u_, 0, 1}] :>
-I*Pi*Zeta2 + Zeta2*Log[1 - x] - I/2*Pi*Log[1 - x]^2 - Log[1 - x]^3/6 +
		2*Zeta2*Log[x] - 2*PolyLog[3, 1 - x] /; FreeQ[x,u]
,
(*X*)iT[Log[(1 - (x_))/(2 - (x_))]*Log[2 - (x_)], {x_, 0, 1}] :>
	-Zeta2/2 + 2*Log[2] - 2*Log[2]^2
,
(*X*)iT[PolyLog[2, (1 - (x_))/(2 - (x_))], {x_, 0, 1}] :>
				Zeta2/2 - Log[2]^2
,
(*X*)iT[PolyLog[2, -1 + (x_)], {x_, 0, 1}] :> -1 - Zeta2/2 + 2*Log[2]
,
(*X*)iT[PolyLog[2, -((u_)/((1 - (u_))*(x_)))], {u_, 0, 1}] :>
				-Zeta2 - Log[x]^2/2 - PolyLog[2, 1 - x] /; FreeQ[x,u]
,
(*X*)iT[Log[x_]^2/x_^2, {x_,0,1}] :> -2
,
(*X*)iT[Log[x_]*Log[(x_)*(1 - (y_)) + (y_)]*(x_), {x_, 0, 1}] :>
	1 - 3/(4*(1 - y)) - Log[y]/4 - Log[y]/(4*(1 - y)^2) + Log[y]/(2*(1 - y)) +
		Log[y]^2/4 + Log[y]^2/(4*(1 - y)^2) - Log[y]^2/(2*(1 - y)) +
		PolyLog[2, 1 - y]/2 + PolyLog[2, 1 - y]/(2*(1 - y)^2) -
		PolyLog[2, 1 - y]/(1 - y) /; FreeQ[y,x]
,
(*X*)iT[(Log[1 - (x_)*(1 - (y_))]*Log[(x_)*(1 - (y_)) + (y_)])/(1 - (x_)),
	{x_, 0, 1}] :>
Zeta2*Log[y] - 2*Nielsen[1, 2, 1 - y] - Log[y]*PolyLog[2, 1 - y] -
	2*Log[y]*PolyLog[2, -y] + 4*PolyLog[3, -y] + 3*Zeta[3] /; FreeQ[y,x]
,
(*X*)iT[Log[1 - (x_)*(1 - (y_))]*Log[(x_)*(1 - (y_)) + (y_)],
				{x_, 0, 1}] :>
	2 + Zeta2 - (2*Zeta2)/(1 - y) - 2*Log[y] + (2*Log[y])/(1 - y) +
2*Log[y]*Log[1 + y] - (4*Log[y]*Log[1 + y])/(1 - y) + 2*PolyLog[2, -y] -
(4*PolyLog[2, -y])/(1 - y)
,
(*X*)iT[(Log[y_]*PolyLog[2, 1 - (y_)])/(y_), {y_, 0, 1}] :> Pi^4/30
,
(*X*)iT[(Log[y_]*PolyLog[2, -(y_)])/(y_), {y_, 0, 1}] :> (7*Pi^4)/720
,
(*X*)iT[PolyLog[3, -(y_)]/(y_), {y_, 0, 1}] :> (-7*Pi^4)/720
,
(*X*)iT[PolyLog[3, y_]/(y_), {y_, 0, 1}] :> Pi^4/90
,
(*X*)iT[Log[(x_) + (1 - (x_))*(z_)]/((x_) + (1 - (x_))*(z_))^2,
			{z_, 0, 1}] :> x^(-1) + Log[x]/(1 - x) + Log[x]/x /; FreeQ[x,z]
,
(*X*)iT[PolyLog[2, x_]/x_^2, {x_, 0, 1}] :> 2 - Pi^2/6
,
(*X*)iT[PolyLog[3, -x_]/x_^2, {x_, 0, 1}] :>
		-3 + Pi^2/12 + 2*Log[2] + (3*Zeta[3])/4
,
(*X*)iT[(Log[x_]*PolyLog[2, -x_])/x_^2, {x_, 0, 1}] :>
-3 + 4 Log[2]
,
(*X*)iT[(Log[t_]*Log[(t_) + (x_)])/(1 + (t_))^4, {t_, 0, 1}] :>
	Zeta2/6 - Zeta2/(6*(1 - x)^3) + Log[2]/(2*(1 - x)^2) +
	(1 + 3*Log[2])/(6*(1 - x)) - Log[x]/2 + Log[x]/(6*(1 - x)^2) +
	Log[x]/(3*(1 - x)) + Log[x]^2/6 - Log[x]^2/(6*(1 - x)^3) -
	Log[1 + x]/(6*(1 - x)^2) - Log[1 + x]/(3*(1 - x)) +
	((5 - 8*Log[2])*Log[1 + x])/24 - (Log[x]*Log[1 + x])/3 + Log[1 + x]^2/6 -
	PolyLog[2, -x]/(3*(1 - x)^3) + PolyLog[2, (1 - x)/(1 + x)]/3 /;
		FreeQ[x,t] && (Factor[x] =!= 1)
,
(*X*)iT[(Log[t_ + x_]*Log[1 + t_*x_])/(1 + t_)^4, {t_, 0, 1}] :>
	Zeta2/6 - Zeta2/(2*(1 - x)^3) - (1 - 3*Zeta2 + 3*Log[2])/(6*(1 - x)^2) +
		(1 - 3*Zeta2 + 3*Log[2])/(6*(1 - x)) - Log[x]/2 + Log[x]/(6*(1 - x)^3) -
		(5*Log[x])/(6*(1 - x)^2) + (7*Log[x])/(6*(1 - x)) +
		((5 - 8*Log[2])*Log[1 + x])/24 +
		((5 - 6*Log[2])*Log[1 + x])/(6*(1 - x)^2) -
		((5 - 6*Log[2])*Log[1 + x])/(6*(1 - x)) -
		(2*Log[x]*Log[1 + x])/(3*(1 - x)^3) + (7*Log[1 + x]^2)/24 +
		Log[1 + x]^2/(1 - x)^2 - Log[1 + x]^2/(1 - x) - PolyLog[2, 1 - x]/3 +
		PolyLog[2, 1 - x]/(3*(1 - x)^3) - PolyLog[2, 1 - x]/(1 - x)^2 +
		PolyLog[2, 1 - x]/(1 - x) + PolyLog[2, -x]/3 - PolyLog[2, -x]/(1 - x)^3 +
		PolyLog[2, -x]/(1 - x)^2 - PolyLog[2, -x]/(1 - x) +
		PolyLog[2, (1 - x)/(1 + x)]/3 + PolyLog[2, (1 - x)/(1 + x)]/(1 - x)^2 -
		PolyLog[2, (1 - x)/(1 + x)]/(1 - x) /; FreeQ[x,t] && (Factor[x] =!= 1)
,
(*X*)iT[(Log[t_]*Log[1 + (t_)*(x_)])/(1 + (t_))^4, {t_, 0, 1}] :>
	-Zeta2/(6*(1 - x)^3) + (Zeta2 + Log[2])/(2*(1 - x)^2) +
		(1 + Zeta2 + 6*Log[2])/6 - (1 + 3*Zeta2 + 9*Log[2])/(6*(1 - x)) -
		Log[1 + x]/(6*(1 - x)^2) + (2*Log[1 + x])/(3*(1 - x)) -
		((7 + 8*Log[2])*Log[1 + x])/24 - (Log[x]*Log[1 + x])/3 + Log[1 + x]^2/6 -
		PolyLog[2, 1 - x]/3 - PolyLog[2, -x]/(3*(1 - x)^3) +
		PolyLog[2, -x]/(1 - x)^2 - PolyLog[2, -x]/(1 - x) +
		PolyLog[2, (1 - x)/(1 + x)]/3  /; FreeQ[x, t]&& (Factor[x] =!= 1)
,
(*X*)iT[(Log[(t_) + (x_)]*Log[1 + (t_)*(x_)])/(1 + (t_))^4, {t_, 0, 1}] :>
	-1/(6*(1 - x)^2) + 1/(6*(1 - x)) + Zeta2/6 - Zeta2/(2*(1 - x)^3) +
		Zeta2/(2*(1 - x)^2) - Zeta2/(2*(1 - x)) - Log[2]/(2*(1 - x)^2) +
		Log[2]/(2*(1 - x)) - Log[x]/2 + Log[x]/(6*(1 - x)^3) -
		(5*Log[x])/(6*(1 - x)^2) + (7*Log[x])/(6*(1 - x)) + (5*Log[1 + x])/24 +
		(5*Log[1 + x])/(6*(1 - x)^2) - (5*Log[1 + x])/(6*(1 - x)) -
		(Log[2]*Log[1 + x])/3 - (Log[2]*Log[1 + x])/(1 - x)^2 +
		(Log[2]*Log[1 + x])/(1 - x) - (2*Log[x]*Log[1 + x])/(3*(1 - x)^3) +
		(7*Log[1 + x]^2)/24 + Log[1 + x]^2/(1 - x)^2 - Log[1 + x]^2/(1 - x) -
		PolyLog[2, 1 - x]/3 + PolyLog[2, 1 - x]/(3*(1 - x)^3) -
		PolyLog[2, 1 - x]/(1 - x)^2 + PolyLog[2, 1 - x]/(1 - x) +
		PolyLog[2, -x]/3 - PolyLog[2, -x]/(1 - x)^3 + PolyLog[2, -x]/(1 - x)^2 -
		PolyLog[2, -x]/(1 - x) + PolyLog[2, (1 - x)/(1 + x)]/3 +
		PolyLog[2, (1 - x)/(1 + x)]/(1 - x)^2 -
		PolyLog[2, (1 - x)/(1 + x)]/(1 - x) /; FreeQ[x,t] && (Factor[x] =!= 1)
,
(*X*)iT[Log[1 - (x_) + (x_)*(y_)]*(y_)^2, {y_, 0, 1}] :>
	-11/18 - 1/(3*x^2) + 5/(6*x) + Log[1 - x]/3 - Log[1 - x]/(3*x^3) +
		Log[1 - x]/x^2 - Log[1 - x]/x
,
(*X*)iT[(Log[t_]*Log[1 + (t_)*(x_)])/(1 + (t_))^3, {t_, 0, 1}] :>
	Zeta2/(4*(1 - x)^2) - (Zeta2 + 2*Log[2])/(2*(1 - x)) +
		(Zeta2 + 4*Log[2])/4 + Log[1 + x]/(2*(1 - x)) -
((1 + 2*Log[2])*Log[1 + x])/4 - (Log[x]*Log[1 + x])/2 + Log[1 + x]^2/4 -
		PolyLog[2, 1 - x]/2 + PolyLog[2, -x]/(2*(1 - x)^2) -
		PolyLog[2, -x]/(1 - x) + PolyLog[2, (1 - x)/(1 + x)]/2 /;
			FreeQ[x,t] && (Factor[x] =!= 1)
,
(*X*)iT[(Log[(t_) + (x_)]*Log[1 + (t_)*(x_)])/(1 + (t_))^3,
		{t_, 0, 1}] :>
	Zeta2/4 - Zeta2/(4*(1 - x)^2) - Zeta2/(2*(1 - x)) - Log[2]/(1 - x)^2 +
		Log[2]/(1 - x) - Log[x]/2 - Log[x]/(2*(1 - x)^2) + Log[x]/(1 - x) +
		Log[1 + x]/4 + Log[1 + x]/(1 - x)^2 - Log[1 + x]/(1 - x) -
		(Log[2]*Log[1 + x])/2 - (Log[2]*Log[1 + x])/(1 - x)^2 +
		(Log[2]*Log[1 + x])/(1 - x) - (Log[x]*Log[1 + x])/(1 - x)^2 +
		(3*Log[1 + x]^2)/8 + Log[1 + x]^2/(1 - x)^2 - Log[1 + x]^2/(1 - x) -
		PolyLog[2, 1 - x]/2 - PolyLog[2, 1 - x]/(2*(1 - x)^2) +
		PolyLog[2, 1 - x]/(1 - x) + PolyLog[2, -x]/2 -
		PolyLog[2, -x]/(2*(1 - x)^2) - PolyLog[2, -x]/(1 - x) +
		PolyLog[2, (1 - x)/(1 + x)]/2 + PolyLog[2, (1 - x)/(1 + x)]/(1 - x)^2 -
		PolyLog[2, (1 - x)/(1 + x)]/(1 - x) /; FreeQ[t, x] &&
				(Factor[x] =!= 1)
,
(*X*)iT[Log[x_]/(1-x_)^4, {x_,0,1}] :> 7/18
,
(*X*)iT[(y_)*PolyLog[2, (1 - (x_)*(y_))/((1 - (x_))*(y_))], {y_, 0, 1}] :>
	(I/2*Pi)/x + Zeta2/2 + (I/2*Pi*Log[1 - x])/x^2 - Log[1 - x]/(2*x) -
		Log[1 - x]^2/(4*x^2) /; FreeQ[x,y]
,
(*X*)iT[(y_)^2*PolyLog[2, (1 - (x_)*(y_))/((1 - (x_))*(y_))], {y_, 0, 1}] :>
		(I/3*Pi)/x^2 - (1 - I*Pi)/(6*x) + Zeta2/3 + (I/3*Pi*Log[1 - x])/x^3 -
		Log[1 - x]/(3*x^2) - Log[1 - x]/(6*x) - Log[1 - x]^2/(6*x^3) /;
			FreeQ[x,y]
,
(*X*)iT[Log[1 - (y_)]*Log[1 - (x_)*(y_)]*(y_)^2, {y_, 0, 1}] :>
		4/(9*x^2) + 17/(36*x) - Zeta2/(3*x^3) + (71 + 36*Zeta2)/108 -
		(11*Log[1 - x])/18 + Log[1 - x]/(9*x^3) + Log[1 - x]/(6*x^2) +
		Log[1 - x]/(3*x) + Log[1 - x]^2/6 - Log[1 - x]^2/(6*x^3) -
		(Log[1 - x]*Log[x])/3 + (Log[1 - x]*Log[x])/(3*x^3) -
		PolyLog[2, 1 - x]/3 + PolyLog[2, 1 - x]/(3*x^3) /; FreeQ[x,y]
,
(*X*)iT[Log[y_]*Log[1 - (x_)*(y_)]*(y_)^2, {y_, 0, 1}] :>
		2/27 + 4/(9*x^2) + 5/(36*x) - Zeta2/(3*x^3) - Log[1 - x]/9 +
		Log[1 - x]/(9*x^3) + (Log[1 - x]*Log[x])/(3*x^3) +
		PolyLog[2, 1 - x]/(3*x^3) /; FreeQ[x,y]
,
(*X*)iT[PolyLog[2, (1 - (x_)*(y_))/((1 - (x_))*(y_))], {y_, 0, 1}] :>
	Zeta2 + (I*Pi*Log[1 - x])/x - Log[1 - x]^2/(2*x) /; FreeQ[x,y]
,
(*X*)iT[PolyLog[2, (1 - (x_)*(y_))/((1 - (x_))*(y_))]/(1 - (x_)*(y_)),
		{y_, 0, 1}] :>
	(-I*Pi*Zeta2)/x - (Zeta2*Log[1 - x])/x - (I/2*Pi*Log[1 - x]^2)/x +
		Log[1 - x]^3/(6*x) + (I*Pi*Log[1 - x]*Log[x])/x - Nielsen[1, 2, x]/x +
		(I*Pi*PolyLog[2, 1 - x])/x /; FreeQ[x,y]
,
(*X*)iT[(Log[y_]*Log[(x_) + (1 - (x_))*(y_)])/((x_) + (1 - (x_))*(y_)),
		{y_, 0, 1}] :>
		-Log[x]^3/(3*(1 - x)) - Nielsen[1, 2, 1 - x]/(1 - x) -
		(Log[x]*PolyLog[2, 1 - x])/(1 - x) /; FreeQ[x,y]
,
(*X*)iT[PolyLog[2, 1 - (y_)]/((x_) + (1 - (x_))*(y_)), {y_, 0, 1}] :>
		-((Zeta2*Log[x])/(1 - x)) - (Log[1 - x]^2*Log[x])/(2*(1 - x)) -
		Nielsen[1, 2, 1 - x]/(1 - x) + Nielsen[1, 2, x]/(1 - x) -
		(Log[1 - x]*PolyLog[2, 1 - x])/(1 - x) - Zeta[3]/(1 - x) /;
			FreeQ[x,y]
,
(*X*)iT[Log[1-x_]/(1-x_)^3, {x_,0,1}] :> -1/4
,
(*X*)iT[Log[1-x_]^2/(1-x_)^3, {x_,0,1}] :> -1/4
,
(*X*)iT[y_^m_. PolyLog[2,- ((1-y_)/((1-x_) y_))],
				{y_,0,1}
			] :> Apart3[Expand[SimplifyPolyLog[
-((x*Hypergeometric2F1[1, 1, 3 + m, -(x/(1 - x))])/
			((1 + m)^2*(2 + m)*(-1 + x))) -
	(x*Hypergeometric2F1[1, 2 + m, 3 + m, x])/
		((1 + m)^2*(2 + m)) +
	(x*HypergeometricPFQ[{1, 1, 1}, {2, 2 + m}, -(x/(1 - x))])/
		((1 + m)^2*(-1 + x)) - Log[1 - x]/((1 + m)^2*(-1 + x)) +
	(x*Log[1 - x])/((1 + m)^2*(-1 + x)) +
(Log[1 - x]*(-1/(m+1)))/
		(1 + m) - (1 + Zeta2 + 2*m*Zeta2 + m^2*Zeta2 -
			SumS[2, 1 + m] - 2*m*SumS[2, 1 + m] - m^2*SumS[2, 1 + m]
)/(1 + m)^3]],x] /; FreeQ[x,y]
,
(*X*)iT[(Log[1 - (u_)]*Log[(u_) (1 - x_) + (x_)])/(u_),
			{u_, 0, 1}] :>
	Zeta2*Log[x] - Log[1 - x]*Log[x]^2 - Log[x]*PolyLog[2, 1 - x] -
		PolyLog[3, 1 - x] - 2*PolyLog[3, x] + 2*Zeta[3] /; FreeQ[x, u]
,
(*X*)iT[Log[(u_) (1 - x_) + (x_)]^2/(u_), {u_, 0, 1}] :>
	2*Zeta2*Log[x] - Log[1 - x]*Log[x]^2 + (2*Log[x]^3)/3 -
			2*PolyLog[3, x] + 2*Zeta[3] /; FreeQ[x, u]
,
(*X*)iT[(Log[x_]*PolyLog[2, -(x_)])/(1 + (x_)), {x_, 0, 1}] :>
	(13*Pi^4)/288 + (Pi^2*Log[2]^2)/6 - Log[2]^4/6 - 4*PolyLog[4, 1/2] -
		(7*Log[2]*Zeta[3])/2
,
(*X*)iT[PolyLog[3, -(x_)]/(1 + (x_)), {x_, 0, 1}] :>
	Pi^4/288 - (3*Log[2]*Zeta[3])/4
,
(*X*)iT[PolyLog[3, (1 + (x_))^(-1)]/(1 + (x_)), {x_, 0, 1}] :>
	Pi^4/90 - PolyLog[4, 1/2]
,
(*X*)iT[(Log[x_]*PolyLog[2, 1 - (x_)])/(1 + (x_)),
		{x_, 0, 1}] :>
	(-11*Pi^4)/480 - Zeta2*Log[2]^2 + Log[2]^4/6 + 4*PolyLog[4, 1/2]
,
(*X*)iT[PolyLog[3, x_]/(1 + (x_)), {x_, 0, 1}] :>
	Pi^4/60 + (Zeta2*Log[2]^2)/2 - Log[2]^4/12 - 2*PolyLog[4, 1/2] -
		(3*Log[2]*Zeta[3])/4
,
(*X*)iT[PolyLog[2, u_]/((u_)*(1 - (x_)) + (x_)), {u_, 0, 1}] :>
	(Zeta2*Log[x])/(1 - x) - (Log[1 - x]*Log[x]^2)/(1 - x) -
		(Log[x]*PolyLog[2, 1 - x])/(1 - x) - PolyLog[3, 1 - x]/(1 - x) -
		(2*PolyLog[3, x])/(1 - x) + (2*Zeta[3])/(1 - x) /; FreeQ[x, u]
,
(*X*)iT[PolyLog[2, ((u_)*(1 - (x_)) + (x_))/((1 - (u_))*(x_))],
				{u_, 0, 1}] :>
	Zeta2 + (I*Pi*Log[x])/(1 - x) - Log[x]^2/(2*(1 - x)) /; FreeQ[x,u]
,
(*X*)iT[(u_)*PolyLog[2, ((u_)*(1 - (x_)) + (x_))/((1 - (u_))*(x_))],
		{u_, 0, 1}] :>
	(-I/2*Pi)/(1 - x) + Zeta2/2 - (I/2*Pi*Log[x])/(1 - x)^2 +
		((1 + 2*I*Pi)*Log[x])/(2*(1 - x)) + Log[x]^2/(4*(1 - x)^2) -
		Log[x]^2/(2*(1 - x)) /; FreeQ[x,u]
,
(*X*)iT[PolyLog[2,
			((u_)*(1 - (x_)) + (x_))/((1 - (u_))*(x_))]/(1 - (u_) + (u_)*(x_)),
		{u_, 0, 1}] :>
	(I*Pi*Zeta2)/(1 - x) - (4*Zeta2*Log[x])/(1 - x) -
		((3*I)/2*Pi*Log[x]^2)/(1 - x) + (3*Log[1 - x]*Log[x]^2)/(2*(1 - x)) +
		(7*Log[x]^3)/(6*(1 - x)) + (2*I*Pi*Log[x]*Log[1 + x])/(1 - x) -
		(2*Log[x]^2*Log[1 + x])/(1 - x) - (I*Pi*PolyLog[2, 1 - x])/(1 - x) +
		(3*Log[x]*PolyLog[2, 1 - x])/(1 - x) + (2*I*Pi*PolyLog[2, -x])/(1 - x) -
		(4*Log[x]*PolyLog[2, -x])/(1 - x) + (4*PolyLog[3, -x])/(1 - x) +
		(3*PolyLog[3, x])/(1 - x)/; FreeQ[x,u]
,
(*X*)iT[(x_)^2*PolyLog[3, (1 + (x_))^(-1)], {x_, 0, 1}] :>
		-17/108 - Pi^2/36 + (35*Log[2])/27 - (Pi^2*Log[2])/18 - (4*Log[2]^2)/9 +
		Log[2]^3/9 + Zeta[3]/4
,
(*X*)iT[Log[x_]^2*Log[1 + (x_)]*(x_)^2,
		{x_, 0, 1}] :> -31/36 + Pi^2/54 + (4*Log[2])/27 + Zeta[3]/2
,
(*X*)iT[Log[x_]^2*Log[1 + (x_)]*(x_)^2, {x_, 0, 1}] :>
		-31/36 + Pi^2/54 + (4*Log[2])/27 + Zeta[3]/2
,
(*X*)iT[Log[x_]*(x_)^2*PolyLog[2, -(x_)], {x_, 0, 1}] :>
		17/108 - (4*Log[2])/27, iT[(x_)^2*PolyLog[3, -(x_)], {x_, 0, 1}] :>
		5/162 + Pi^2/108 - (2*Log[2])/27 - Zeta[3]/4
,
(*X*)iT[PolyLog[2, -((u_)/((1 - (u_))*(x_)))]/(1 - (u_) + (u_)*(x_))^2,
		{u_, 0, 1}] :>
	(-2*Log[x]^2)/x + (2*Log[x]*Log[1 + x])/x - (2*PolyLog[2, 1 - x])/x +
		(2*PolyLog[2, -x])/x /; FreeQ[x,u]
,
(*X*)iT[(Log[1 - (u_)*(1 - (x_))]*Log[(u_)*(1 - (x_)) + (x_)])/
		(1 - (u_) + (u_)*(x_)), {u_, 0, 1}] :>
	(Zeta2*Log[1 + x])/(1 - x) - (Log[x]^2*Log[1 + x])/(2*(1 - x)) -
	Log[1 + x]^3/(3*(1 - x)) - (Log[x]*PolyLog[2, -x])/(1 - x) +
	PolyLog[3, -x]/(1 - x) + (2*PolyLog[3, (1 + x)^(-1)])/(1 - x) -
	Zeta[3]/(1 - x) /; FreeQ[x, u]
,
(*X*)iT[(u_)*PolyLog[2, -((u_)/((1 - (u_))*(x_)))], {u_, 0, 1}] :>
	-Zeta2/2 + Log[x]/(2*(1 - x)) - Log[x]^2/4 - PolyLog[2, 1 - x]/2 /;
		FreeQ[x, u]
,
(*X*)iT[Log[1 + (t_)]^2/(1 - (t_)), {t_, 0, 1}] :>
	-(Zeta2*Log[2]) + (2*Log[2]^3)/3 + Zeta[3]/4
,
(*X*)iT[PolyLog[2, (t_)/(1 + (t_))], {t_, 0, 1}] :> Zeta2/2 - Log[2]^2
,
(*X*)iT[PolyLog[2, (t_)/(1 + (t_))]/(1 - (t_)), {t_, 0, 1}] :>
	(Zeta2*Log[2])/2 - Log[2]^3/3 - (3*Zeta[3])/4
,
(*X*)iT[Log[1 + (t_)]^2*(t_), {t_, 0, 1}] :> -5/4 + 2*Log[2]
,
(*X*)iT[(t_)*PolyLog[2, (t_)/(1 + (t_))], {t_, 0, 1}] :>
			1/2 + Zeta2/4 - Log[2]
,
(*X*)iT[(Log[u_]*Log[1 - (t_) + (t_)*(u_)])/(u_), {u_, 0, 1}] :>
	Zeta2*Log[1 - t] + Log[1 - t]^3/6 - (Log[1 - t]^2*Log[t])/2 -
		PolyLog[3, 1 - t] - PolyLog[3, t] + Zeta[3] /; FreeQ[t,u]
,
(*X*)iT[(Log[1 - (t_)*(u_)]*Log[1 - (t_) + (t_)*(u_)])/(u_),
				{u_, 0, 1}] :>
	-(Log[1 - t]*PolyLog[2, 1 - t]) - 2*Log[1 - t]*PolyLog[2, -1 + t] +
		2*PolyLog[3, 1 - t] + 4*PolyLog[3, -1 + t] + Zeta[3] /; FreeQ[t,u]
,
(*X*)iT[Log[1 - (t_)]*PolyLog[2, -1 + (t_)], {t_, 0, 1}] :> 3 - 4*Log[2]
,
(*X*)iT[Log[1 - (t_)]*(t_)*PolyLog[2, -1 + (t_)], {t_, 0, 1}] :>
	53/16 - Pi^2/24 - 4*Log[2]
,
(*X*)iT[PolyLog[3, -1 + (t_)], {t_, 0, 1}] :>
	1 + Zeta2/2 - 2*Log[2] - (3*Zeta[3])/4
,
(*X*)iT[(t_)*PolyLog[3, -1 + (t_)], {t_, 0, 1}] :>
	17/16 + (3*Zeta2)/8 - 2*Log[2] - (3*Zeta[3])/8
,
(*X*)iT[PolyLog[2,-t_/(1-2 t_)], {t_,0,1}] :> Zeta2/4
,
(*X*)iT[PolyLog[2,-(t_)^2/(1-2 t_)], {t_,0,1}] :> Zeta2-1
,
(*X*)iT[t_*Log[t_*(1 - u_) + u_]*Log[1 - t_ + t_*u_],{t_,0,1}] :>
-(Zeta2/(1 - u)) + (2 + Zeta2)/2 - Log[u] + Log[u]/(1 - u) +
	Log[u]*Log[1 + u] - (2*Log[u]*Log[1 + u])/(1 - u) + PolyLog[2, -u] -
	(2*PolyLog[2, -u])/(1 - u) /; FreeQ[u,t]
,
(*X*) iT[PolyLog[2, ((x_) + (1 - (x_))*(y_))/((x_)*(1 - (y_)))]/(y_),
		{y_, 0, 1}] :>
		-I*Pi*Zeta2 - Zeta2*Log[x] - I/2*Pi*Log[x]^2 + (Log[1 - x]*Log[x]^2)/2 +
		Log[x]^3/6 - I*Pi*PolyLog[2, 1 - x] + Log[x]*PolyLog[2, 1 - x] +
		PolyLog[3, x] /; FreeQ[x,y]
,
(*X*)iT[PolyLog[2,
			((x_) + (1 - (x_))*(y_))/((x_)*(1 - (y_)))]/((x_) + (1 - (x_))*(y_)),
		{y_, 0, 1}] :>
		(-2*Zeta2*Log[x])/(1 - x) - (I/2*Pi*Log[x]^2)/(1 - x) +
		(Log[1 - x]*Log[x]^2)/(2*(1 - x)) + Log[x]^3/(6*(1 - x)) -
		(I*Pi*PolyLog[2, 1 - x])/(1 - x) + (Log[x]*PolyLog[2, 1 - x])/(1 - x) +
		PolyLog[3, x]/(1 - x) - Zeta[3]/(1 - x)  /; FreeQ[x,y]
,
(*X*)iT[Nielsen[1,2,x_]/(1-x_),{x_,0,1}] :> -Pi^4/30
,
(*X*)iT[Nielsen[1,2,1-x_]/(x_),{x_,0,1}] :> -Pi^4/30
,
(*X*)iT[Nielsen[1,2,1-x_]/(1-x_),{x_,0,1}] :> Pi^4/360
,
(*X*)iT[(Log[t_]*Log[1 - (t_)*(1 - (z_))])/((t_) + (z_) - (t_)*(z_)),
	{t_, 0, 1}] :>
	(6*Zeta2*Log[1 - z])/(1 - z) + (I*Pi*Log[1 - z]^2)/(1 - z) +
	(Zeta2*Log[z])/(1 - z) - (Log[1 - z]*Log[z]^2)/(1 - z) -
	(7*Zeta2*Log[1 + z])/(1 - z) - (2*I*Pi*Log[1 - z]*Log[1 + z])/(1 - z) -
	(Log[z]^2*Log[1 + z])/(2*(1 - z)) + (I*Pi*Log[1 + z]^2)/(1 - z) +
	Log[1 + z]^3/(3*(1 - z)) - (Log[z]*PolyLog[2, 1 - z])/(1 - z) +
	(Log[z]*PolyLog[2, -z])/(1 - z) - (2*PolyLog[3, 1 - z])/(1 - z) -
	(3*PolyLog[3, -z])/(1 - z) - (2*PolyLog[3, z])/(1 - z) -
	(2*PolyLog[3, (1 + z)^(-1)])/(1 - z) -
	(2*PolyLog[3, -((1 + z)/(1 - z))])/(1 - z) +
	(2*PolyLog[3, (1 + z)/(1 - z)])/(1 - z) + (3*Zeta[3])/(2*(1 - z)) /;
FreeQ[z, t]
,
(*X*)iT[(Log[1 - (t_)]*Log[1 - (t_)*(1 - (x_))])/
		((t_)*(1 - (x_)) + (x_)), {t_, 0, 1}] :>
	(6*Zeta2*Log[1 - x])/(1 - x) + (I*Pi*Log[1 - x]^2)/(1 - x) -
		(Log[1 - x]^2*Log[x])/(1 - x) - (7*Zeta2*Log[1 + x])/(1 - x) -
		(2*I*Pi*Log[1 - x]*Log[1 + x])/(1 - x) -
		(Log[x]^2*Log[1 + x])/(2*(1 - x)) + (I*Pi*Log[1 + x]^2)/(1 - x) +
		Log[1 + x]^3/(3*(1 - x)) + (2*Nielsen[1, 2, x])/(1 - x) -
		(2*Log[1 - x]*PolyLog[2, 1 - x])/(1 - x) -
		(Log[x]*PolyLog[2, -x])/(1 - x) + PolyLog[3, -x]/(1 - x) -
		(2*PolyLog[3, (1 + x)^(-1)])/(1 - x) -
		(2*PolyLog[3, -((1 + x)/(1 - x))])/(1 - x) +
		(2*PolyLog[3, (1 + x)/(1 - x)])/(1 - x) + Zeta[3]/(2*(1 - x)) /;
FreeQ[x, t]
,
(*X*)iT[PolyLog[2, (((t_)*(1 - (x_)) + (x_))*(1 - (t_) + (t_)*(x_)))/
			(x_)]/(1 - (t_) + (t_)*(x_)), {t_, 0, 1}] :>
	(-10*Zeta2*Log[1 - x])/(1 - x) - (2*I*Pi*Log[1 - x]^2)/(1 - x) -
		(3*Zeta2*Log[x])/(1 - x) - (I/2*Pi*Log[x]^2)/(1 - x) +
		Log[x]^3/(6*(1 - x)) + (14*Zeta2*Log[1 + x])/(1 - x) +
		(4*I*Pi*Log[1 - x]*Log[1 + x])/(1 - x) +
		(2*I*Pi*Log[x]*Log[1 + x])/(1 - x) +
		(4*Log[1 - x]*Log[x]*Log[1 + x])/(1 - x) -
		(Log[x]^2*Log[1 + x])/(1 - x) - (2*I*Pi*Log[1 + x]^2)/(1 - x) -
		(2*Log[1 + x]^3)/(3*(1 - x)) + (2*I*Pi*PolyLog[2, -x])/(1 - x) +
		(4*Log[1 - x]*PolyLog[2, -x])/(1 - x) -
		(2*Log[x]*PolyLog[2, -x])/(1 - x) + (4*PolyLog[3, 1 - x])/(1 - x) +
		(2*PolyLog[3, -x])/(1 - x) + (2*PolyLog[3, x])/(1 - x) +
		(4*PolyLog[3, (1 + x)^(-1)])/(1 - x) +
		(4*PolyLog[3, -((1 + x)/(1 - x))])/(1 - x) -
		(4*PolyLog[3, (1 + x)/(1 - x)])/(1 - x) +
		(I*Pi*Zeta2 - 4*Zeta[3])/(1 - x)  /;  FreeQ[x,t]
,
(*X*)iT[PolyLog[2, (1 - (t_)*(x_))/(1 - (t_))]/(t_), {t_, 0, 1}] :>
	-I*Pi*Log[1 - x]*Log[x] - Nielsen[1, 2, x] - I*Pi*PolyLog[2, 1 - x] +
		Zeta[3] /; FreeQ[x,t]
,
(*X*)iT[PolyLog[2, (1 - (t_)*(x_))/(1 - (t_))]/(1 - (t_)*(x_)),
		{t_, 0, 1}] :>
	(-I*Pi*Zeta2)/x - (Zeta2*Log[1 - x])/x + (I*Pi*Log[1 - x]*Log[x])/x +
		Nielsen[1, 2, x]/x + (I*Pi*PolyLog[2, 1 - x])/x  /; FreeQ[x,t]
,
(*X*)iT[PolyLog[2, (((t_)*(1 - (x_)) + (x_))*(1 - (t_) + (t_)*(x_)))/(x_)],
		{t_, 0, 1}] :>
	-4 + 2*I*Pi + Zeta2 + 4*Log[1 - x] - (2 + I*Pi)*Log[x] +
		(2*I*Pi*Log[x])/(1 - x) - 2*Log[1 - x]*Log[x] +
		(4*Log[1 - x]*Log[x])/(1 - x) + Log[x]^2/2 - Log[x]^2/(1 - x) -
		2*PolyLog[2, 1 - x] + (4*PolyLog[2, 1 - x])/(1 - x) /; FreeQ[x,t]
,
(*X*)iT[PolyLog[2, (((t_)*(1 - (x_)) + (x_))*(1 - (t_) + (t_)*(x_)))/(x_)]/
		(t_), {t_, 0, 1}] :>
	-I/2*Pi*Log[x]^2 - Log[1 - x]*Log[x]^2 + Log[x]^3/6 -
		2*Nielsen[1, 2, 1 - x] - 2*Log[x]*PolyLog[2, 1 - x] /; FreeQ[x,t]
,
(*SPECIAL*)
(*X*)iT[y_^k_Integer PolyLog[2, (x_ + y_ (1-x_))/(x_ (1-y_))],{y_,0,1}] :>
Zeta2/(1 + k) - (I*Pi*x*Integrate[y^k/(x + (1 - x)*y), {y, 0, 1},
			GenerateConditions->False])/
		(1 + k) + (x*Integrate[(y^k*Log[1 - y])/(x + (1 - x)*y), {y, 0, 1},
										GenerateConditions->False])/
		(1 + k) - (x*Integrate[(y^k*Log[y])/(x + (1 - x)*y), {y, 0, 1},
			GenerateConditions->False])/
		(1 + k) + (I*Pi*Log[x])/(1 + k) +
	(x*Integrate[y^k/(x + (1 - x)*y), {y, 0, 1},
								GenerateConditions->False]*Log[x])/(1 + k) -
	Log[x]^2/(2*(1 + k)) - (I*Pi*SumS[1, k])/(1 + k) + (Log[x]*SumS[1, k])/(1 + k) +
	SumS[2, k]/(1 + k) - SumS[1, 1, k]/(1 + k)   /; FreeQ[x,y]
,
(*X*)iT[(1-x_)^n_Integer?Negative,{x_,0,1}] :> 1/(1+n) /; n < -1
,
(*X*)iT[Log[1-x_] (1-x_)^n_Integer?Negative,{x_,0,1}] :>
		(-1/(1+n)^2) /; n < -1
,
(*X*)iT[Log[x_] x_^n_Integer?Negative,{x_,0,1}] :> (-1/(1+n)^2) /; n < -1
,
(*X*)iT[Log[x_] (1-x_)^n_Integer?Negative,{x_,0,1}] :>
			(-1/(-n-1) ( -SumS[1,-n-2]+1/(-n-1) )) /; n < -1
,
(*X*)iT[Log[1-x_] x_^n_Integer?Negative,{x_,0,1}] :>
			(-1/(-n-1) ( -SumS[1,-n-2]+1/(-n-1) )) /; n < -1
,
(*X*)iT[Log[x_] Log[1-x_](1-x_)^n_Integer?Negative,{x_,0,1}] :>
(1 + n)^(-3) - Zeta2/(1 + n) + SumS[1, -2 - n]/(1 + n)^2 -
	SumS[2, -2 - n]/(1 + n)
,
(*X*)iT[PolyLog[2,1-x_](1-x_)^n_Integer?Negative,{x_,0,1}] :>
	-2/(1 + n)^3 + Zeta2/(1 + n) - SumS[1, -2 - n]/(1 + n)^2
,
(*X*)iT[Log[1 - (x_)]^2/(x_)^4, {x_, 0, 1}] :> 5/6 + Pi^2/9
,
(*X*)iT[Log[1 + (x_)]/(x_)^3, {x_, 0, 1}] :> -3/4
,
(*X*)iT[(Log[1 - (x_)]*Log[1 + (x_)])/(x_)^4, {x_, 0, 1}] :>
		1/3 - Pi^2/36 + (2*Log[2])/3 - Log[2]^2/3
,
(*X*)iT[Log[1 + (x_)]^2/(x_)^4, {x_, 0, 1}] :>
		-3/2 + Pi^2/18 + (4*Log[2])/3 - (2*Log[2]^2)/3
,
(*X*)iT[(Log[(x_) + (1 - (x_))*(y_)]*Log[1 - (y_) + (x_)*(y_)])/(y_),
		{y_, 0, 1}] :>
(  Zeta2*Log[x] - Log[x]*PolyLog[2, 1 - x] - 2*Log[x]*PolyLog[2, -x] +
		4*PolyLog[3, -x] + 3*Zeta[3] -
		2*(Zeta2*Log[x] - (Log[1 - x]*Log[x]^2)/2 - Log[x]*PolyLog[2, 1 - x] -
			PolyLog[3, x] + Zeta[3])
) /; FreeQ[x, y]
(*0897*)
,
(*X*)iT[Log[1 - (s_)]*Log[s_]^3, {s_, 0, 1}] :>
24 - Pi^4/15 - 6*Zeta2 - 6*Zeta[3]
,
(*X*)iT[Log[s_]*Log[1-s_]^3, {s_, 0, 1}] :>
24 - Pi^4/15 - 6*Zeta2 - 6*Zeta[3]
,
(*X*) iT[Log[s_]^2*PolyLog[2, 1 - (s_)], {s_, 0, 1}] :>
-12 + Pi^4/15 + 2*Zeta2 + 4*Zeta[3]
,
(*X*)iT[Log[s_]*PolyLog[3, s_], {s_, 0, 1}] :> -4 + 3*Zeta2 - Zeta[3]
,
(*X*)iT[Log[1-s_]*PolyLog[3, s_], {s_, 0, 1}] :>
-4 - Pi^4/72 + 2*Zeta2 + Zeta[3]
,
(*X*)iT[PolyLog[4, s_], {s_, 0, 1}] :> -1 + Pi^4/90 + Zeta2 - Zeta[3]
,
(*X*)iT[PolyLog[4, 1-s_], {s_, 0, 1}] :> -1 + Pi^4/90 + Zeta2 - Zeta[3]
,
(*X*)iT[PolyLog[4, 1/s_], {s_, 0, 1}] :> -I*Pi + Pi^4/90 + Zeta2 + Zeta[3]
,
(*X*)iT[PolyLog[4, 1/(1-s_)], {s_, 0, 1}] :>
				-I*Pi + Pi^4/90 + Zeta2 + Zeta[3]
,
(*X*)iT[PolyLog[4, -(s_/(1 - s_))], {s_,0,1}] :> -3 Zeta[4]
,
(*X*)iT[Log[1-s_ t_]^3/t_, {t_,0,1}] :> (
-Pi^4/15 + Log[1 - s]^3*Log[s] + 3*Log[1 - s]^2*PolyLog[2, 1 - s] -
	6*Log[1 - s]*PolyLog[3, 1 - s] + 6*PolyLog[4, 1 - s]
																				) /; FreeQ[s,t]
,
(*X*)iT[(Log[t_]*Log[1 - s_ + s_*t_]^2)/t_, {t_,0,1}] :> (
-Pi^4/45 - Zeta2*Log[1 - s]^2 - I/3*Pi*Log[1 - s]^3 +
	Log[1 - s]^4/4 - (Log[1 - s]^3*Log[s])/3 + 2*PolyLog[4, (1 - s)^(-1)] -
	2*PolyLog[4, s] - 2*PolyLog[4, -(s/(1 - s))] + 2*Log[1 - s]*Zeta[3]
																				) /; FreeQ[s,t]
,
(*X*)iT[Log[s_] PolyLog[3, -s_], {s_, 0, 1}] :>
-4 - Zeta2/2 + 6*Log[2] + (3*Zeta[3])/4
,
(*X*)iT[Log[1-s_] PolyLog[3, -1+s_], {s_, 0, 1}] :>
-4 - Zeta2/2 + 6*Log[2] + (3*Zeta[3])/4
,
(*X*)iT[Log[s_]*PolyLog[3, s_], {s_, 0, 1}] :>
-4 + 3*Zeta2 - Zeta[3]
,
(*X*)iT[Log[1-s_]*PolyLog[3, 1-s_], {s_, 0, 1}] :>
-4 + 3*Zeta2 - Zeta[3]
,
(*X*)iT[Log[s_]^2*PolyLog[2, -s_], {s_, 0, 1}] :>
-12 + Zeta2 + 12*Log[2] + (3*Zeta[3])/2
,
(*X*)iT[Log[1-s_]^2*PolyLog[2, -1+s_], {s_, 0, 1}] :>
-12 + Zeta2 + 12*Log[2] + (3*Zeta[3])/2
,
(*X*)iT[Log[s_]^2*PolyLog[2, s_], {s_, 0, 1}] :>
-12 + Pi^2 + 2*Zeta[3]
,
(*X*)iT[Log[1-s_]^2*PolyLog[2, 1-s_], {s_, 0, 1}] :>
-12 + Pi^2 + 2*Zeta[3]
,
(*X*)iT[Log[1-s_ t_]^3, {s_, 0, 1}] :>
-6 + 6*Log[1 - t] - (6*Log[1 - t])/t - 3*Log[1 - t]^2 +
	(3*Log[1 - t]^2)/t + Log[1 - t]^3 - Log[1 - t]^3/t
,
(*X*)iT[Log[1 - (s_)*(t_)]*Log[1 - (s_) + (s_)*(t_)], {s_, 0, 1}] :>
(
2 + (3*Zeta2)/(2*(1 - t)) - (3*Zeta2)/(2*t) -
	(Log[2]*Log[1 - 2*t])/(1 - t) + (Log[2]*Log[1 - 2*t])/t - Log[1 - t] +
	(I*Pi*Log[1 - t])/(1 - t) + ((1 - I*Pi)*Log[1 - t])/t -
	(Log[1 - 2*t]*Log[1 - t])/(1 - t) + (Log[1 - 2*t]*Log[1 - t])/t - Log[t] +
	Log[t]/(1 - t) - (Log[1 - 2*t]*Log[t])/(1 - t) + (Log[1 - 2*t]*Log[t])/t +
	Log[1 - t]*Log[t] - (Log[1 - t]*Log[t])/(1 - t) -
	(Log[1 - t]*Log[-(t/(1 - 2*t))])/(1 - t) +
	(Log[1 - t]*Log[-(t/(1 - 2*t))])/t + (Log[2]*Log[-1 + 2*t])/(1 - t) -
	(Log[2]*Log[-1 + 2*t])/t + (Log[1 - t]*Log[-1 + 2*t])/(1 - t) -
	(Log[1 - t]*Log[-1 + 2*t])/t - (3*PolyLog[2, 1 - t])/(1 - t) +
	(3*PolyLog[2, 1 - t])/t + PolyLog[2, 2*(1 - t)]/(1 - t) -
	PolyLog[2, 2*(1 - t)]/t - PolyLog[2, 2*t]/(1 - t) + PolyLog[2, 2*t]/t
) /; FreeQ[t,s]
,
(*X*)iT[Log[1 - (s_)*(t_)]^2*Log[1 - (s_) + (s_)*(t_)], {s_, 0, 1}]:>
(
-6 - (3*Zeta2)/(1 - t) + (3*Zeta2)/t +
	(2*Log[2]*Log[1 - 2*t])/(1 - t) - (2*Log[2]*Log[1 - 2*t])/t +
	4*Log[1 - t] - ((2*I)*Pi*Log[1 - t])/(1 - t) -
	(2*(2 - I*Pi)*Log[1 - t])/t +
	(2*(1 - I*Pi - 2*Log[2])*Log[1 - 2*t]*Log[1 - t])/(1 - t) -
	(2*(1 - I*Pi - 2*Log[2])*Log[1 - 2*t]*Log[1 - t])/t +
	(Log[1 - 2*t]^2*Log[1 - t])/(1 - t) - (Log[1 - 2*t]^2*Log[1 - t])/t -
	Log[1 - t]^2 + ((4*I)*Pi*Log[1 - t]^2)/(1 - t) +
	((1 - (4*I)*Pi)*Log[1 - t]^2)/t - (4*Log[1 - 2*t]*Log[1 - t]^2)/(1 - t) +
	(4*Log[1 - 2*t]*Log[1 - t]^2)/t + 2*Log[t] - (2*Log[t])/(1 - t) +
	(2*Log[1 - 2*t]*Log[t])/(1 - t) - (2*Log[1 - 2*t]*Log[t])/t -
	2*Log[1 - t]*Log[t] + (2*Log[1 - t]*Log[t])/(1 - t) -
	(4*Log[1 - 2*t]*Log[1 - t]*Log[t])/(1 - t) +
	(4*Log[1 - 2*t]*Log[1 - t]*Log[t])/t + Log[1 - t]^2*Log[t] -
	(Log[1 - t]^2*Log[t])/(1 - t) +
	(2*Log[1 - t]*Log[-(t/(1 - 2*t))])/(1 - t) -
	(2*Log[1 - t]*Log[-(t/(1 - 2*t))])/t -
	(Log[1 - t]^2*Log[-(t/(1 - 2*t))])/(1 - t) +
	(Log[1 - t]^2*Log[-(t/(1 - 2*t))])/t - (2*Log[2]*Log[-1 + 2*t])/(1 - t) +
	(2*Log[2]*Log[-1 + 2*t])/t -
	(2*(1 - 2*Log[2])*Log[1 - t]*Log[-1 + 2*t])/(1 - t) +
	(2*(1 - 2*Log[2])*Log[1 - t]*Log[-1 + 2*t])/t +
	(4*Log[1 - t]^2*Log[-1 + 2*t])/(1 - t) -
	(4*Log[1 - t]^2*Log[-1 + 2*t])/t + (6*PolyLog[2, 1 - t])/(1 - t) -
	(6*PolyLog[2, 1 - t])/t - (8*Log[1 - t]*PolyLog[2, 1 - t])/(1 - t) +
	(8*Log[1 - t]*PolyLog[2, 1 - t])/t - (2*PolyLog[2, 2*(1 - t)])/(1 - t) +
	(2*PolyLog[2, 2*(1 - t)])/t +
	(4*Log[1 - t]*PolyLog[2, 2*(1 - t)])/(1 - t) -
	(4*Log[1 - t]*PolyLog[2, 2*(1 - t)])/t + (2*PolyLog[2, 2*t])/(1 - t) -
	(2*PolyLog[2, 2*t])/t - (4*Log[1 - t]*PolyLog[2, 2*t])/(1 - t) +
	(4*Log[1 - t]*PolyLog[2, 2*t])/t -
	(2*PolyLog[3, (1 - t)/(1 - 2*t)])/(1 - t) +
	(2*PolyLog[3, (1 - t)/(1 - 2*t)])/t +
	(2*PolyLog[3, (1 - t)^2/(1 - 2*t)])/(1 - t) -
	(2*PolyLog[3, (1 - t)^2/(1 - 2*t)])/t
)/; FreeQ[t,s]
,
(*X*)iT[Log[1 - (s_)*(t_)]*Log[1 - (s_) + (s_)*(t_)]^2, {s_, 0, 1}]:>
(
-6 + 2*Log[1 - t] - (2*Log[1 - t])/t + 4*Log[t] -
	(4*Log[t])/(1 - t) - 2*Log[1 - t]*Log[t] - Log[t]^2 + Log[t]^2/(1 - t) +
	Log[1 - t]*Log[t]^2 - (Log[1 - t]*Log[t]^2)/(1 - t) +
	(Log[(1 - t)^2/(1 - 2*t)]*Log[t]^2)/(1 - t) +
	(2*Log[1 - t]*Log[-(t^2/(1 - 2*t))])/(1 - t) +
	(2*Log[t]*Log[(1 - 2*t + t^2)/(1 - 2*t)])/t -
	(Log[t]^2*Log[(1 - 2*t + t^2)/(1 - 2*t)])/t -
	(2*PolyLog[2, (1 - t)/(1 - 2*t)])/(1 - t) +
	(2*PolyLog[2, (1 - t)^2/(1 - 2*t)])/(1 - t) -
	(2*PolyLog[2, -(t/(1 - 2*t))])/t + (2*PolyLog[2, -(t^2/(1 - 2*t))])/t +
	(2*Log[t]*PolyLog[2, -(t^2/(1 - 2*t))])/(1 - t) -
	(2*Log[t]*PolyLog[2, -(t^2/(1 - 2*t))])/t +
	(2*PolyLog[3, -(t/(1 - 2*t))])/(1 - t) -
	(2*PolyLog[3, -(t/(1 - 2*t))])/t -
	(2*PolyLog[3, -(t^2/(1 - 2*t))])/(1 - t) +
	(2*PolyLog[3, -(t^2/(1 - 2*t))])/t
)/; FreeQ[t,s]
,
(*X*)iT[Log[1 - (s_) + (s_)*(t_)]^3, {s_, 0, 1}]:>
(
-6 + 6*Log[t] - (6*Log[t])/(1 - t) - 3*Log[t]^2 +
	(3*Log[t]^2)/(1 - t) + Log[t]^3 - Log[t]^3/(1 - t)
)/; FreeQ[t,s]
,
(*X*)iT[PolyLog[4,-(1 - s_)/s_], {s_, 0, 1}]:> -Pi^4/30
,
(*X*)iT[PolyLog[4,-s_/(1-s_)], {s_, 0, 1}]:> -Pi^4/30
,
(*X*)iT[(Log[t_]^2*Log[1 - (s_)*(t_)])/(t_), {t_, 0, 1}] :>
-2 PolyLog[4, s] /; FreeQ[s,t]
,
(*X*)iT[(Log[t_]*Log[1 - (s_)*(t_)]^2)/(t_),
	{t_, 0, 1}] :>
(
-Pi^4/45 - Zeta2*Log[1 - s]^2 -
	Log[1 - s]^4/12 + (Log[1 - s]^3*Log[s])/3 +
	2*Log[1 - s]*PolyLog[3, s] +
	2*PolyLog[4, 1 - s] - 2*PolyLog[4, s] -
	2*PolyLog[4, -(s/(1 - s))] -
	2*Log[1 - s]*Zeta[3]
)/;FreeQ[s,t]
,
(*X*)iT[(Log[t_]^2*Log[(s_)*(t_)-s_+1])/(t_), {t_, 0, 1}] :>
-2*PolyLog[4, -(s/(1 - s))]
,
(*X*)iT[Log[(s_)*(t_)-s_+1]^3/(t_), {t_, 0, 1}] :>
(
-Pi^4/15 - 3*Zeta2*Log[1 - s]^2 -
	I*Pi*Log[1 - s]^3 + Log[1 - s]^4 -
	Log[1 - s]^3*Log[s] +
	6*PolyLog[4, (1 - s)^(-1)] +
	6*Log[1 - s]*Zeta[3]
) /; FreeQ[s, t]
,
(*X*)iT[(Log[1 - (s_)]*Log[2 - (s_)])/(1 - (s_)),
	{s_, 0, 1}] :> -Zeta[3]/4
,
(*X*)iT[Log[2 - (s_)]^2/(1 - (s_)), {s_, 0, 1}] :>
	Zeta[3]/4
,
(*X*)iT[Log[2 - (s_)]^2*(s_), {s_, 0, 1}] :>
	13/4 - 6*Log[2] + 2*Log[2]^2
,
(*X*)iT[PolyLog[2, (1 - (s_))/(2 - (s_))]/
		(1 - (s_)), {s_, 0, 1}] :> (5*Zeta[3])/8
,
(*X*)iT[(s_)*PolyLog[2, (1 - (s_))/(2 - (s_))],
	{s_, 0, 1}] :>
	-1/2 + Zeta2/4 + Log[2] - Log[2]^2
,
(*X*)iT[PolyLog[2, -1 + (s_)]/(1 - (s_)),
	{s_, 0, 1}] :> (-3*Zeta[3])/4
,
(*X*)iT[(s_)*PolyLog[2, -1 + (s_)], {s_, 0, 1}] :>
	-9/8 - Zeta2/4 + 2*Log[2]
,
(*X*)iT[Log[1 - (s_)*(t_)]*
		Log[1 - (s_) + (s_)*(t_)], {t_, 0, 1}] :>
(
	(4*s - 4*Zeta2 + 2*s*Zeta2 + 4*Log[1 - s] -
		4*s*Log[1 - s] -
		8*Log[1 - s]*Log[2 - s] +
		4*s*Log[1 - s]*Log[2 - s] +
		2*Log[2 - s]^2 - s*Log[2 - s]^2 +
		4*PolyLog[2, (1 - s)/(2 - s)] -
		2*s*PolyLog[2, (1 - s)/(2 - s)] -
		4*PolyLog[2, -1 + s] +
		2*s*PolyLog[2, -1 + s])/(2*s)
) /; FreeQ[s,t]
,
(*X*)iT[s_ Log[1-s_] Log[2-s_],{s_,0,1}] :>
5/2 - (3*Zeta2)/4 - 2*Log[2]
,
(*X*)iT[Log[1 - (s_) + (s_)*(x_) - (u_)*(x_)]*
			Log[(s_)*(1 - (x_)) + (u_)*(x_)],
		{s_, 0, 1}] :>
	(I*Pi*u*x)/(1 - x) + (x*Log[1 - u])/(1 - x) -
		(u*x*Log[1 - u])/(1 - x) +
		(u*x*Log[u])/(1 - x) + (2 - 2*x + x*Log[x])/(1 - x) +
		(Log[u]*Log[1 - u*x])/(1 - x) - (u*x*Log[u]*Log[1 - u*x])/(1 - x) -
		((1 - Log[x])*Log[1 - u*x])/(1 - x) +
		(u*x*(1 - Log[x])*Log[1 - u*x])/(1 - x) -
		(u*x*Log[-1 + x - u*x])/(1 - x) -
		(x*Log[1 - u]*Log[1 - x + u*x])/(1 - x) +
		(u*x*Log[1 - u]*Log[1 - x + u*x])/(1 - x) +
		(u*x*Log[x]*Log[1 - x + u*x])/(1 - x) -
		((1 - x + x*Log[x])*Log[1 - x + u*x])/(1 - x) +
		PolyLog[2, u*x]/(1 - x) - PolyLog[2, 1 - x + u*x]/(1 - x) /;
		FreeQ[u, s] && FreeQ[x, s]
,
(*X*)iT[Log[(u_) + (s_)*(x_) - (u_)*(x_)]*
		Log[1 - (u_) - (s_)*(x_) + (u_)*(x_)], {s_, 0, 1}] :>
	((1 - x)*Log[1 - u])/x - (u*(1 - x)*Log[1 - u])/x +
		(u*(1 - x)*Log[u])/x + (2*x + Log[1 - x] - x*Log[1 - x])/x +
		Log[1 - u]*Log[u + x - u*x] +
		(u*(1 - x)*Log[1 - u]*Log[u + x - u*x])/x -
		(1 - Log[1 - x])*Log[u + x - u*x] -
(u*(1 - x)*(1 - Log[1 - x])*Log[u + x - u*x])/x - Log[1 - u + u*x]/x -
		(u*(1 - x)*Log[u]*Log[1 - u + u*x])/x +
		(u*(1 - x)*(1 - Log[1 - x])*Log[1 - u + u*x])/x +
		PolyLog[2, (1 - u)*(1 - x)]/x - PolyLog[2, 1 - u + u*x]/x /;
		FreeQ[u, s] && FreeQ[x, s]
,
(*X*)iT[PolyLog[2, -(((u_)*(1 - (x_)))/((s_)*(x_)))], {s_, 0, 1}] :>
	(u*(1 - x)*Log[u])/x + (u*(1 - x)*(I*Pi + Log[1 - x]))/x + Log[x] -
		Log[u + x - u*x] - (u*(1 - x)*Log[-u - x + u*x])/x +
		PolyLog[2, -((u*(1 - x))/x)] /; FreeQ[u, s] && FreeQ[x, s]
,
(*X*)iT[PolyLog[2, (s_)*(x_)], {s_, 0, 1}] :>
	(-x + x*Zeta2 - Log[1 - x] + x*Log[1 - x] - x*Log[1 - x]*Log[x] -
			x*PolyLog[2, 1 - x])/x /; FreeQ[u, s] && FreeQ[x, s]
,
(*X*)iT[PolyLog[2, -(((u_)*(x_))/((s_)*(1 - (x_))))], {s_, 0, 1}] :>
	(u*x*Log[u])/(1 - x) + Log[1 - x] + (u*x*(I*Pi + Log[x]))/(1 - x) -
		(u*x*Log[-1 + x - u*x])/(1 - x) - Log[1 - x + u*x] +
		PolyLog[2, -((u*x)/(1 - x))] /; FreeQ[u, s] && FreeQ[x, s]
,
(*X*)iT[PolyLog[2, ((u_)*(1 - (x_)))/(1 - (s_)*(x_))], {s_, 0, 1}] :>
	((1 - x)*Log[1 - u])/x - (u*(1 - x)*Log[1 - u])/x +
		((1 - x)*Log[1 - u]*Log[u])/x + Log[u]^2/(2*x) -
		(u*(1 - x)*Log[1 - x])/x + (Log[u]*Log[1 - x])/x +
		(2*x*Zeta2 + Log[1 - x]^2)/(2*x) + (u*(1 - x)*Log[1 - u + u*x])/x -
		(Log[u]*Log[1 - u + u*x])/x - ((1 + Log[1 - x])*Log[1 - u + u*x])/x +
		((1 - x)*PolyLog[2, 1 - u])/x +
		PolyLog[2, -((1 - u + u*x)/(u*(1 - x)))]/x /;
	FreeQ[u, s] && FreeQ[x, s]
,
(*X*)iT[PolyLog[2, 1 - (s_)*(x_)], {s_, 0, 1}] :>
	(-x + Zeta2 + x*Log[x] - PolyLog[2, 1 - x] + x*PolyLog[2, 1 - x])/x /;
		FreeQ[u, s] && FreeQ[x, s]
,
(*X*)iT[PolyLog[2, ((u_)*(x_))/(1 - (s_) + (s_)*(x_))], {s_, 0, 1}] :>
	(x*Log[1 - u])/(1 - x) - (u*x*Log[1 - u])/(1 - x) +
		(x*Log[1 - u]*Log[u])/(1 - x) + Log[u]^2/(2*(1 - x)) -
		(u*x*Log[x])/(1 - x) + (Log[u]*Log[x])/(1 - x) +
		(2*Zeta2 - 2*x*Zeta2 + Log[x]^2)/(2*(1 - x)) +
		(u*x*Log[1 - u*x])/(1 - x) - (Log[u]*Log[1 - u*x])/(1 - x) -
		((1 + Log[x])*Log[1 - u*x])/(1 - x) + (x*PolyLog[2, 1 - u])/(1 - x) +
		PolyLog[2, -((1 - u*x)/(u*x))]/(1 - x) /; FreeQ[u, s] && FreeQ[x, s]
,
(*X*)iT[PolyLog[2, 1 - (s_) + (s_)*(x_)], {s_, 0, 1}] :>
	-((1 - x - Zeta2 + x*Zeta2 - Log[1 - x] + x*Log[1 - x] -
				x*Log[1 - x]*Log[x] - x*PolyLog[2, 1 - x])/(1 - x)) /;
		FreeQ[u, s] && FreeQ[x, s]
,
(*X*)iT[PolyLog[2, 1 - (s_) + (s_)*(x_) - (u_)*(x_)], {s_, 0, 1}] :>
	-1 - (u*x*Log[u])/(1 - x) - (u*x*Log[x])/(1 - x) + Log[1 - x + u*x] +
		(u*x*Log[1 - x + u*x])/(1 - x) - (x*PolyLog[2, (1 - u)*x])/(1 - x) +
		(u*x*PolyLog[2, (1 - u)*x])/(1 - x) + PolyLog[2, 1 - u*x]/(1 - x) -
		(u*x*PolyLog[2, 1 - u*x])/(1 - x) /; FreeQ[u, s] && FreeQ[x, s]
,
(*X*)iT[PolyLog[2, (u_) + (s_)*(x_) - (u_)*(x_)], {s_, 0, 1}] :>
	-(((1 - x)*Log[1 - u])/x) + (u*(1 - x)*Log[1 - u])/x +
		(u*(1 - x)*Log[1 - x])/x - (x + Log[1 - x] - x*Log[1 - x])/x +
		Log[1 - u + u*x]/x - (u*(1 - x)*Log[1 - u + u*x])/x -
		(u*(1 - x)*PolyLog[2, u*(1 - x)])/x + PolyLog[2, u + x - u*x] +
		(u*(1 - x)*PolyLog[2, u + x - u*x])/x /; FreeQ[u, s] && FreeQ[x, s]
,
(*X*)iT[PolyLog[2, (s_)*(1 - (x_)) + (u_)*(x_)], {s_, 0, 1}] :>
	-((x*Log[1 - u])/(1 - x)) + (u*x*Log[1 - u])/(1 - x) +
		(u*x*Log[x])/(1 - x) - (1 - x + x*Log[x])/(1 - x) +
		Log[1 - u*x]/(1 - x) - (u*x*Log[1 - u*x])/(1 - x) -
		(u*x*PolyLog[2, u*x])/(1 - x) + PolyLog[2, 1 - x + u*x] +
		(u*x*PolyLog[2, 1 - x + u*x])/(1 - x) /; FreeQ[u, s] && FreeQ[x, s]
,
(*X*)iT[PolyLog[2, 1 - (u_) - (s_)*(x_) + (u_)*(x_)], {s_, 0, 1}] :>
	-1 - (u*(1 - x)*Log[u])/x - (u*(1 - x)*Log[1 - x])/x + Log[u + x - u*x] +
		(u*(1 - x)*Log[u + x - u*x])/x -
		((1 - x)*PolyLog[2, (1 - u)*(1 - x)])/x +
		(u*(1 - x)*PolyLog[2, (1 - u)*(1 - x)])/x + PolyLog[2, 1 - u + u*x]/x -
		(u*(1 - x)*PolyLog[2, 1 - u + u*x])/x /; FreeQ[u, s] && FreeQ[x, s]
,
(*X*)iT[x_^2 Log[1+x_]^3,{x_,0,1}] :>
		-461/108 + (70*Log[2])/9 - (8*Log[2]^2)/3 + (2*Log[2]^3)/3
,
(*X*)iT[x_^2 Log[1+x_]^3,{x_,0,1}] :>
		4411/1152 - (41*Log[2])/6 + 2*Log[2]^2
,
(*X*) iT[Log[x_]^2* Log[1 + (x_)]*(x_)^3, {x_, 0, 1}] :>
	1957/3456 - Zeta2/16 - (3*Zeta[3])/8
,
(*X*)iT[Log[1 + (x_)]^3* (x_)^3, {x_, 0, 1}] :> 4411/1152 -
		(41*Log[2])/6 + 2*Log[2]^2
,
(*X*)iT[Log[x_]*(x_)^3* PolyLog[2, -(x_)], {x_, 0, 1}] :>
	-157/2304 + Zeta2/16
,
(*X*)iT[(x_)^3* PolyLog[3, -(x_)], {x_, 0, 1}] :>
	-7/768 + Zeta2/32 - (3*Zeta[3])/16
,
(*X*)iT[(x_)^3* PolyLog[3, (1 + (x_))^(-1)], {x_, 0, 1}] :>
	199/1152 + (3*Zeta2)/16 - (41*Log[2])/36 +
	Log[2]^2/3 + Zeta[3]/4
,
(*X*)iT[Log[1 + (x_)]^2*(x_)^2, {x_, 0, 1}] :>
	55/54 - (16*Log[2])/9 + (2*Log[2]^2)/3
,
(*X*)iT[Log[1 + (x_)]^2*(x_)^3, {x_, 0, 1}] :> -241/288 + (4*Log[2])/3
,
(*X*)iT[Log[1 + (x_)]^2/ (1 + (x_)), {x_, 0, 1}] :> Log[2]^3/3
,
(*X*)iT[Log[u_]^3/u_, {u_,0,1}] :> 0
,
(*X*)iT[PolyLog[2, -((1 - (x_))/ ((1 - (u_))*(u_)* (x_)^2))]/(u_),
	{u_, 0, 1}] :> (
	-(Zeta2*Log[1 - x]) - Log[1 - x]^3/6 + 2*Zeta2*Log[x] +
	Log[1 - x]^2*Log[x] + 2*Log[1 - x]* PolyLog[2, 1 - x] -
2*PolyLog[3, 1 - x] ) /; FreeQ[x,u]
,
(*X*)iT[PolyLog[2, -(((1 - (u_))*(u_)* (x_)^2)/(1 - (x_)))]/(u_),
		{u_, 0, 1}] :> ( Log[1 - x]^3/6 - Log[1 - x]^2*Log[x] -
	2*Log[1 - x]* PolyLog[2, 1 - x] + 2*PolyLog[3, 1 - x] - 2*Zeta[3]
										) /; FreeQ[x,u]
,
(*X*)iT[PolyLog[2,
		-(((1 - (u_))*(u_)*(x_)^2)/(1 - (x_)))]/ (1 - (u_)*(x_)), {u_, 0, 1}
			] :> (Log[1 - x]^3/(6*x) ) /; FreeQ[x,u]
,
(*X*)iT[Log[1-x_] Log[x_ + 1], {x_,0,1}] :> 2 - Zeta2 - 2*Log[2] + Log[2]^2
,
(*X*)iT[Log[x_ + 1]/(1-x_), {x_,0,1}] :> -Zeta2/2 + Log[2]^2/2
,
(*X*)iT[PolyLog[2,1-x_] Log[x_ + 1], {x_,0,1}] :>
3 - (3*Zeta2)/2 - 2*Log[2] + 3*Zeta2*Log[2] - 2*Zeta[3]
,
(*X*)iT[Log[1-x_] Log[1+x_],{x_,0,1}] :> 2 - Zeta2 - 2*Log[2] + Log[2]^2
,
(*X*)iT[PolyLog[2,-x_] Log[1-x_], {x_,0,1}] :>
3 - Zeta2/2 - 4*Log[2] + Log[2]^2 + (5*Zeta[3])/8
,
(*X*)iT[Log[1-x_] Log[1+x_]/(1-x_),{x_,0,1}] :>
-(Zeta2*Log[2])/2 + Log[2]^3/6 + (7*Zeta[3])/8
,
(*X*)iT[PolyLog[2,(1-x_)/(1+x_)] /(1-x_), {x_,0,1}] :>
-(Zeta2*Log[2]) + (13*Zeta[3])/8
,
(*X*)iT[PolyLog[2,(1-x_)/(1+x_)] /(1+x_), {x_,0,1}] :>
Zeta2*Log[2] - (5*Zeta[3])/8
,
(*X*)iT[PolyLog[3,-(1+x_)/(1-x_)], {x_,0,1}] :>
-(Zeta2*Log[2]) - (11*Zeta[3])/4
,
(*X*)iT[PolyLog[3, (1+x_)/(1-x_)], {x_,0,1}] :>
-I*Pi*Zeta2 + 2*Zeta2*Log[2] + (3*Zeta[3])/4
,
(*X*)iT[Nielsen[1,2, x_^2],{x_,0,1}] :>
-8 + 2*Zeta2 + 8*Log[2] - 4*Log[2]^2 + Zeta[3]
,
(*X*)iT[Nielsen[1,2, -x_]/(1+x_),{x_,0,1}] :>
-Pi^4/30 - (3*Zeta2*Log[2]^2)/4 +
	Log[2]^4/8 + 3*PolyLog[4, 1/2] + (11*Log[2]*Zeta[3])/4
,
(*X*)iT[(Log[1 + x_]* PolyLog[2, -x_])/(1 + x_), {x_, 0, 1}]:>
Pi^4/30 + (Zeta2*Log[2]^2)/2 - Log[2]^4/8 - 3*PolyLog[4, 1/2] -
	(21*Log[2]*Zeta[3])/8
,
(*X*)iT[Log[x_] Log[1-x_] Log[1+x_],{x_,0,1}]:>
-6 + (5*Zeta2)/2 + 4*Log[2] -
	3*Zeta2*Log[2] - Log[2]^2 + (21*Zeta[3])/8
,
(*X*)iT[PolyLog[2,-x_]/(1+x_)^2,{x_,0,1}] :> -Zeta2/4 + Log[2]^2/2
,
(*X*)iT[Log[x_+1] PolyLog[2,-x_]/(1+x_),{x_,0,1}] :>
Pi^4/30 + (Zeta2*Log[2]^2)/2 - Log[2]^4/8 - 3*PolyLog[4, 1/2] -
	(21*Log[2]*Zeta[3])/8
,
(*X*)iT[Log[x_+1]/(x_+1),{x_,0,1}] :> Log[2]^2/2
,
(*X*)iT[x_ Log[1-x_] Log[x_+1],{x_,0,1}] :> 1/4-Log[2]
,
(*X*)iT[x_ Log[1-x_] Log[x_] Log[x_+1],{x_,0,1}] :>
-3/8 - Zeta2/8 + (3*Log[2])/2 -
	(5*Zeta[3])/16
,
(*X*)iT[Log[1 + (x_)]*(x_)*PolyLog[2, 1 - (x_)],{x_,0,1}] :>
9/16 + Zeta2/8 - Log[2]
,
(*X*)iT[Log[1 - (x_)]*(x_)* PolyLog[2, -(x_)], {x_, 0, 1}]:>
9/16 + (3*Zeta2)/8 - (3*Log[2])/2 +
	(5*Zeta[3])/16
,
(*X*)iT[(x_)*PolyLog[3, -((1 + (x_))/(1 - (x_)))],
	{x_, 0, 1}] :> (-3*Zeta2)/2 - Log[2]^2 - (3*Zeta[3])/8
,
(*X*)iT[(x_)*PolyLog[3, (1 + (x_))/(1 - (x_))],
	{x_, 0, 1}] :> Zeta2 - 2*I*Pi*Log[2] - Log[2]^2 +
	Zeta[3]/2
,
(*X*)iT[Log[1+x_] PolyLog[2,1-x_]/(1+x_), {x_,0,1}] :>
	7/8*Zeta2^2 - 21/8*Log[2]*Zeta[3] + 5/4*Log[2]^2*Zeta2 -
		1/12*Log[2]^4 - 2 PolyLog[4,1/2]
,
(*X*)iT[Nielsen[1,2,x_^2]/(1+x_) , {x_,0,1}] :>
- 37/20*Zeta2^2 + 9/2*Log[2]*Zeta[3] - Log[2]^2*Zeta2 +
	1/6*Log[2]^4 + 4*PolyLog[4,1/2]
,
(*X*)iT[(Log[1 - (x_)]^2* Log[1 + (x_)])/(1 - (x_)),
	{x_, 0, 1}] :> -2*PolyLog[4, 1/2]
,
(*X*) iT[(Log[1 - (x_)]* Log[1 + (x_)]^2)/(1 - (x_)),
	{x_, 0, 1}] :> -Pi^4/360 - Zeta2*Log[2]^2 +
Log[2]^4/4 + 2*Log[2]*Zeta[3]
,
(*X*)iT[Log[1 + (x_)]^3/(1 - (x_)), {x_, 0, 1}] :>
-Pi^4/15 - 3*Zeta2*Log[2]^2 + Log[2]^4 +
6*PolyLog[4, 1/2] + 6*Log[2]*Zeta[3]
,
(*X*)iT[x_^2 Nielsen[1,2,1-x_],{x_,0,1}] :> -251/648 + Zeta[3]/3
,
(*X*)iT[Nielsen[1, 2, 1 - (x_)]*(x_)^2, {x_, 0, 1}] :>
			-251/648 + Zeta[3]/3
,
(*X*)iT[Nielsen[1, 2, 1 - (x_)]/(1 + (x_)), {x_, 0, 1}] :>
	(-19*Pi^4)/1440 + (7*Log[2]*Zeta[3])/4
,
(*X*)iT[Nielsen[1, 2, -(x_)], {x_, 0, 1}] :> -1 + 2*Log[2] -
		Log[2]^2 + Zeta[3]/8
,
(*X*)iT[Nielsen[1, 2, -(x_)]* (x_), {x_, 0, 1}] :>
	5/16 - Log[2]/2 + Zeta[3]/16
,
(*X*)iT[Nielsen[1, 2, (x_)^2]* (x_), {x_, 0, 1}] :>
-1/2 + Zeta[3]/2
,
			(* by Jos Vermaseren *)
(*X*)iT[PolyLog[3, 1 - (x_)]/(1 + (x_)), {x_, 0, 1}] :>
	(Pi^2*Log[2]^2)/12 + (7*Log[2]*Zeta[3])/8 - Pi^4/80 + Log[2]^4/24 +
	PolyLog[4, 1/2]
,
(*X*)iT[(Log[1 + (x_)]*PolyLog[2, 1 - (x_)])/ (1 - (x_)), {x_, 0, 1}]:>
	(Pi^2*Log[2]^2)/12 + (7*Log[2]*Zeta[3])/8 - Pi^4/80 + Log[2]^4/24 +
	PolyLog[4, 1/2]
,
(*X*)iT[(Log[1 - (x_)]*PolyLog[2, 1 - (x_)])/(1 + (x_)), {x_, 0, 1}] :>
(-7*Pi^4)/288 + (3*Zeta2*Log[2]^2)/4 + Log[2]^4/8 + 3*PolyLog[4, 1/2]
,
(*X*)iT[Log[1 - (x_)]^2*Log[1 + (x_)], {x_, 0, 1}] :>
	-6 + 2*Zeta2 + 4*Log[2] - 2*Zeta2*Log[2] - 2*Log[2]^2 +
		(2*Log[2]^3)/3 + (7*Zeta[3])/2
,
(*X*)iT[Log[1 - (x_)]*Log[1 + (x_)]^2, {x_, 0, 1}] :>
	-6 + 2*Zeta2 - 2*Zeta2*Log[2] - 4*Log[2]^2 +
		(4*Log[2]^3)/3 + 8 Log[2] + Zeta[3]/2
,
(*X*)iT[Log[-1 + (x_) - (u_)*(x_)]/(u_)^2, {u_, 0, 1}] :>
	(-1 - I*Pi + (1 - x)^(-1) - Log[1 - x] + Log[1 - x]/(1 - x))/;FreeQ[x,u]
,
(*X*)iT[y_^m_?mcheck Log[x_ + (1-x_)y_],{y_,0, 1}]:>
		Apart[SimplifyPolyLog[((x - 1)*LerchPhi[1 - 1/x, 1, m + 2])/((m + 1)*x)
							],x] /; FreeQ[x,y]
,
(*X*)iT[Log[-1 + x_ - x_ *y_]*(y_)^(n_?mcheck), {y_, 0, 1}] :>
	(Apart[SimplifyPolyLog[(I*Pi)/(1 + n) -
		(x*LerchPhi[-(x/(1 - x)), 1, 2 + n])/ ((1 + n)*(1 - x))], x]
	) /; FreeQ[y,x]
,
(*X*)iT[Log[-1 + (x_) - (x_)*(y_)]*(y_)^(n_?mcheck), {y_, 0, 1}] :>
	Expand[Apart[SimplifyPolyLog[(I*Pi)/(1 + n) +
			(x*LerchPhi[x/(-1 + x), 1, 2 + n])/
			((1 + n)*(-1 + x))], x]] /; FreeQ[x,y]
,
(*X*)iT[x_^n_Integer?Negative, {x_,0,1}] :> 1/(1+n) /; n<-1
,
(*X*)iT[Log[-(u_) - (x_) + (u_)*(x_)]* (u_)^2, {u_, 0, 1}] :>
(  -11/18 + I/3*Pi - 1/(3*(1 - x)^2) +
	5/(6*(1 - x)) + Log[x]/3 - Log[x]/(3*(1 - x)^3) +
	Log[x]/(1 - x)^2 - Log[x]/(1 - x) ) /; FreeQ[x,u]
,
(*X*)iT[Log[t_]^2*Log[1 - (t_)*(u_)], {t_, 0, 1}] :> (
	-6 + (2*Zeta2)/u + 2*Log[1 - u] - (2*Log[1 - u])/u -
		(2*Log[1 - u]*Log[u])/u - (2*PolyLog[2, 1 - u])/u +
	(2*PolyLog[3, u])/u ) /; FreeQ[u, t]
,
(*X*)iT[Log[t_]*Log[1 - (t_)*(u_)]^2, {t_, 0, 1}] :> (
	-6 + (2*Zeta2)/u + 4*Log[1 - u] - (4*Log[1 - u])/u - Log[1 - u]^2 +
		Log[1 - u]^2/u - (2*Log[1 - u]*Log[u])/u + (Log[1 - u]^2*Log[u])/u -
		(2*PolyLog[2, 1 - u])/u + (2*Log[1 - u]*PolyLog[2, 1 - u])/u -
		(2*PolyLog[3, 1 - u])/u + (2*Zeta[3])/u ) /; FreeQ[u, t]
,
(*X*)iT[Log[1 - (x_)*(y_)]*(x_)^m_?mcheck, {x_, 0, 1}] :>
	Apart3[(y*LerchPhi[y, 1, 2 + m] + Log[1 - y])/ (1 + m),y] /; FreeQ[y,x]
,
(*X*)iT[(Log[1 - (x_) + (x_)*(y_)]*(x_)^m_?mcheck)/ (1 - (x_)),
				{x_, 0, 1}] :> (
	(-((-1 + y)*Hypergeometric2F1[2 + m, 1,
				3 + m, 1 - y]) + (2 + m)*Log[y])/ ((1 + m)*(2 + m))
											) /; FreeQ[y, x]
,
(*X*)iT[PolyLog[2, ((x_) + (1 - (x_))*(y_))/((x_)*(1 - (y_)))]/(1 - (y_)),
	{y_, 0, 1}] :> (
	-(Zeta2*Log[x]) - (Log[1 - x]^2*Log[x])/2 + Nielsen[1, 2, 1 - x] +
Nielsen[1, 2, x] + I*Pi*PolyLog[2, 1 - x] - Log[1 - x]*PolyLog[2, 1 - x]+
	PolyLog[3, 1 - x] - 2*Zeta[3]) /; FreeQ[x, y]

,
(*X*)iT[(Log[1 + (x_)]*PolyLog[2, (1 - (x_))/(1 + (x_))])/(1 + (x_)),
	{x_, 0, 1}] :> Pi^4/480 + (Zeta2*Log[2]^2)/2 - (5*Log[2]*Zeta[3])/8
,
(*X*)iT[(Log[x_]*PolyLog[2, (1 - (x_))/(1 + (x_))])/ (1 - (x_)), {x_, 0, 1}
			] :> Pi^4/48 + (5*Zeta2*Log[2]^2)/ 4 - (5*Log[2]^4)/24 -
	5*PolyLog[4, 1/2] - (7*Log[2]*Zeta[3])/4

,
(*X*)iT[Log[x_ + 1] PolyLog[2,-x_]/x_ , {x_,0,1}] :> -Pi^4/288
,
(*X*)iT[(Log[1 + (x_)]*PolyLog[2, 1 - (x_)])/(x_), {x_, 0, 1}] :>
(71*Pi^4)/1440 + Zeta2*Log[2]^2 - Log[2]^4/6 -
	4*PolyLog[4, 1/2] - (7*Log[2]*Zeta[3])/2
,
(*CORRECTED 1998 *)
(*X*)iT[PolyLog[3, -((1 + (x_))/ (1 - (x_)))]/(1 + (x_)),
	{x_, 0, 1}] :> -Pi^4/288 + Zeta2^2/4 - (3*Log[2]*Zeta[3])/4
,
(*SPEC*)
(*X*)iT[(Log[x_]*PolyLog[2, (1 - (x_))/(1 + (x_))])/(1 + (x_)),
				{x_, 0, 1}] :>
		(-43*Pi^4)/720 +
	iT[(Log[1 - x]*PolyLog[2, -x])/(1 + x), {x, 0, 1}] -
	(3*Zeta2*Log[2]^2)/2 + (7*Log[2]^4)/24 +
	7*PolyLog[4, 1/2] + (21*Log[2]*Zeta[3])/8
,
(*X*)iT[(Log[1 + (x_)]*PolyLog[2, (1 - (x_))/(1 + (x_))])/(x_),
				{x_, 0, 1}] :>
(41*Pi^4)/720 - iT[(Log[1 - (x)]*PolyLog[2, -(x)])/(1 + (x)),
		{x, 0, 1}] + Zeta2*Log[2]^2 - (5*Log[2]^4)/24 -
	5*PolyLog[4, 1/2] - (7*Log[2]*Zeta[3])/2
,
(*X*)iT[(Log[1 + (x_)]*PolyLog[2, (1 - (x_))/(1 + (x_))])/(1 - (x_)),
			{x_, 0, 1}] :> (-13*Pi^4)/480 +
	iT[(Log[1 - x]*PolyLog[2, -x])/(1 + x), {x, 0, 1}] -
	(3*Zeta2*Log[2]^2)/4 + Log[2]^4/12 + 2*PolyLog[4, 1/2] +
	(5*Log[2]*Zeta[3])/2
,
(*X*)iT[PolyLog[3, (1 + (x_))/(1 - (x_))]/(1 + (x_)),
		{x_, 0, 1}] :> -Pi^4/144 +
	iT[(Log[1 - x]*PolyLog[2, -x])/(1 + x), {x, 0, 1}] -
	(Zeta2*Log[2]^2)/4 + Log[2]^4/12 + 2*PolyLog[4, 1/2] -
	(3*I)/4*Pi*Zeta[3] + (15*Log[2]*Zeta[3])/8
,
(*X*)iT[PolyLog[2,-x_] Log[1-x_]/(1+x_),{x_,0,1}] :>
3/2 Zeta2^2 + 1/8*Pi^2*Log[2]^2 - Log[2]^4/6 - 4*PolyLog[4, 1/2] -
	21/8*Log[2]*Zeta[3]
,
iT[Log[z_]^(n_Integer)?Positive, {z_, 0, 1}] :> RR["$BK[5]", -Gamma[1 + n]],
iT[Log[z_]^(n_Integer)?Positive/(1 - z_), {z_, 0, 1}] :>
	RR["$BK[12]", (-1)^(1 + n)*Gamma[1 + n]*(SumS[1 + n, 0] - Zeta[1 + n])],
iT[Log[z_]^(n_Integer)?Positive/(1 + z_), {z_, 0, 1}] :>
	RR["$BK[15]", (-1)^n*Gamma[1 + n]*(-SumS[1 + n, 0] +
		(SumS[1 + n, 0] - Zeta[1 + n])/2^n + Zeta[1 + n])],
iT[PolyLog[2, z_]/(1 + z_), {z_, 0, 1}] :>
	RR["$BK[36]", Zeta2*Log[2] - (5*Zeta[3])/8],
iT[PolyLog[3, 1 - z_], {z_, 0, 1}] :> RR["$BK[65]", 1 - Zeta2 + Zeta[3]],
iT[PolyLog[3, 1 - z_]/(1 - z_), {z_, 0, 1}] :> RR["$BK[66]", (2*Zeta2^2)/5],

iT[PolyLog[3, (1 - z_)/(1 + z_)], {z_, 0, 1}] :>
RR["68", iT[PolyLog[3, -((1 - z)/(1 + z))], {z, 0, 1}] - 3*Zeta2*Log[2] +
	(7*Zeta[3])/2],


iT[PolyLog[3, (1 - z_)/(1 + z_)]/(1 + z_), {z_, 0, 1}] :>
	RR["$BK[69]", (19*Zeta2^2)/40 + (Zeta2*Log[2]^2)/2 - Log[2]^4/12 +
		iT[PolyLog[3, -((1 - z)/(1 + z))]/(1 + z),{z,0,1}] - 2*PolyLog[4, 1/2]] ,

iT[Nielsen[1, 2, z_]/(1 + z_), {z_, 0, 1}] :>
RR["$BK[72]", Pi^4/288 + (Zeta2*Log[2]^2)/4 - Log[2]^4/24 -
	PolyLog[4, 1/2] + (Log[2]*Zeta[3])/8],


iT[Log[z_]^n_Integer?Positive*z_^(m_.), {z_, 0, 1}] :>
	RR["$BK[5]", (-1)^(1 + m)*(1 + m)^(-1 - n)*Gamma[1 + n]] /; mcheck[m],
iT[(Log[1 - z_]^3*z_^(m_.))/(1 - z_), {z_, 0, 1}] :>
	RR["$BK[11]", SumS[1, m]^4/4 + (3*SumS[1, m]^2*SumS[2, m])/2 +
		(3*SumS[2, m]^2)/4 + 2*SumS[1, m]*SumS[3, m] + (3*SumS[4, m])/2] /;
mcheck[m], iT[(Log[z_]^n_Integer?Positive*z_^(m_.))/(1 - z_), {z_, 0, 1}] :>
	RR["$BK[12]", (-1)^(1 + n)*Gamma[1 + n]*(SumS[1 + n, m] - Zeta[1 + n])] /;
	mcheck[m],

iT[Log[1 + z_]^2*z_^(m_.), {z_, 0, 1}] :>
	RR["$BK[14]", (Log[2]^2 - 2*
(
-((-1)^m*Log[2]^2)/2 -
((1 + (-1)^m)*Log[2]*SumS[1, m/2])/2 +
((1 - (-1)^m)*Log[2]*SumS[1, (1 + m)/2])/2 +
2*(-1)^m*Log[2]*SumS[1, 1 + m] +
((1 + (-1)^m)*SumS[1, m/2]*SumS[1, 1 + m])/2 -
((1 - (-1)^m)*SumS[1, (1 + m)/2]*SumS[1, 1 + m])/2 -
(-1)^m*SumS[1, 1 + m]^2 +
((1 + (-1)^m)*SumS[2, m/2])/4 -
((1 - (-1)^m)*SumS[2, (1 + m)/2])/4 -
(-1)^m*SumS[2, 1 + m] - (-1)^m*SumS[-1, 1, 1 + m]
)
)/(1 + m)] /; mcheck[m],

iT[(Log[z_]^n_Integer?Positive*z_^(m_.))/(1 + z_), {z_, 0, 1}] :>
	RR["$BK[15]", (-1)^n*Gamma[1 + n]*
		(-(2^(-1 - n)*(1 - (-1)^m)*(SumS[1 + n, (-1 + m)/2] - Zeta[1 + n])) +
			2^(-1 - n)*(1 + (-1)^m)*(SumS[1 + n, m/2] - Zeta[1 + n]) +
			(-1)^(1 + m)*(SumS[1 + n, m] - Zeta[1 + n]))] /; mcheck[m],
iT[(Log[1 - z_]*z_^(m_.))/(1 + z_), {z_, 0, 1}] :>
	RR["$BK[16]", (-1)^(1 + m)*(Zeta2/2 - Log[2]^2/2 + SumS[-1, 1, m])] /;
	mcheck[m], iT[(Log[1 + z_]*z_^(m_.))/(1 + z_), {z_, 0, 1}] :>
	RR["$BK[17]", ((1 - (-1)^m)*(-(Log[2]*SumS[1, (-1 + m)/2]) +
				SumS[1, (-1 + m)/2]*SumS[1, m] + SumS[2, (-1 + m)/2]/2))/2 -
		((1 + (-1)^m)*(-(Log[2]*SumS[1, m/2]) + SumS[1, m/2]*SumS[1, m] +
				SumS[2, m/2]/2))/2 + (-1)^m*(Log[2]^2/2 - 2*Log[2]*SumS[1, m] +
			SumS[1, m]^2 + SumS[2, m] + SumS[-1, 1, m])] /; mcheck[m],
iT[(Log[1 - z_]^2*Log[z_]*z_^(m_.))/(1 - z_), {z_, 0, 1}] :>
	RR["$BK[24]", (-4*Zeta2^2)/5 - Zeta2*SumS[2, m] + SumS[2, m]^2 +
		SumS[1, m]^2*(-Zeta2 + SumS[2, m]) + 2*SumS[4, m] +
		2*SumS[1, m]*(SumS[3, m] - Zeta[3])] /; mcheck[m],
iT[(Log[1 - z_]*Log[z_]^2*z_^(m_.))/(1 - z_), {z_, 0, 1}] :>
	RR["$BK[25]", -Zeta2^2/5 - 2*Zeta2*SumS[2, m] + SumS[2, m]^2 +
		2*SumS[1, m]*SumS[3, m] + 3*SumS[4, m] - 2*SumS[1, m]*Zeta[3]] /;
	mcheck[m], iT[(Log[1 - z_]*Log[z_]^2*z_^(m_.))/(1 + z_), {z_, 0, 1}] :>
	RR["$BK[26]", 2*(-1)^(1 + m)*(-Zeta2^2/5 - (Zeta2*Log[2]^2)/2 +
			Log[2]^4/12 + 2*PolyLog[4, 1/2] - Zeta2*SumS[-2, m] + SumS[-3, 1, m] +
			SumS[-2, 2, m] + SumS[-1, 3, m] - SumS[-1, m]*Zeta[3])] /; mcheck[m],
iT[(Log[1 - z_]^2*Log[z_]*z_^(m_.))/(1 + z_), {z_, 0, 1}] :>
	RR["$BK[27]", 2*(-1)^(1 + m)*((-11*Zeta2^2)/20 + Log[2]^4/8 +
			3*PolyLog[4, 1/2] - Zeta2*SumS[-1, 1, m] + SumS[-2, 1, 1, m] +
			SumS[-1, 1, 2, m] + SumS[-1, 2, 1, m] - SumS[-1, m]*Zeta[3])] /;
	mcheck[m], iT[Log[z_]^2*Log[1 + z_]*z_^(m_.), {z_, 0, 1}] :>
	RR["$BK[28]", ((1 + (-1)^m)*((4*Log[2])/(1 + m)^2 + (2*SumS[1, m/2])/
				(1 + m)^2 + SumS[2, m/2]/(1 + m) + SumS[3, m/2]/2))/(2*(1 + m)) -
		((1 - (-1)^m)*((2*SumS[1, (1 + m)/2])/(1 + m)^2 +
				SumS[2, (1 + m)/2]/(1 + m) + SumS[3, (1 + m)/2]/2))/(2*(1 + m)) +
		(2*(-1)^(1 + m)*(-Zeta2/(2*(1 + m)) + SumS[1, 1 + m]/(1 + m)^2 +
				SumS[2, 1 + m]/(1 + m) + SumS[3, 1 + m] - (3*Zeta[3])/4))/(1 + m)] /;
	mcheck[m], iT[(Log[z_]^2*Log[1 + z_]*z_^(m_.))/(1 + z_), {z_, 0, 1}] :>
	RR["$BK[29]", (-1)^(1 + m)*((3*Zeta2^2)/2 + Zeta2*Log[2]^2 - Log[2]^4/6 -
			4*PolyLog[4, 1/2] - 2*Log[2]*SumS[-3, m] + Zeta2*SumS[2, m] +
			2*Log[2]*SumS[3, m] + 2*SumS[1, -3, m] + 2*SumS[2, -2, m] +
			2*SumS[3, -1, m] - (7*Log[2]*Zeta[3])/2 + (3*SumS[1, m]*Zeta[3])/2)] /;
	mcheck[m], iT[(Log[z_]*Log[1 + z_]^2*z_^(m_.))/(1 + z_), {z_, 0, 1}] :>
	RR["$BK[30]", 2*(-1)^(1 + m)*((2*Zeta2^2)/5 + (Zeta2*Log[2]^2)/4 -
			Log[2]^4/24 - PolyLog[4, 1/2] + (Log[2]^2*SumS[-2, m])/2 +
			(Zeta2*SumS[1, m]^2)/4 + (Zeta2*SumS[2, m])/4 -
			(Log[2]^2*SumS[2, m])/2 + Log[2]*SumS[1, m]*SumS[2, m] +
			Log[2]*SumS[3, m] - Log[2]*SumS[1, -2, m] - Log[2]*SumS[2, -1, m] +
			SumS[1, 1, -2, m] + SumS[1, 2, -1, m] + SumS[2, 1, -1, m] -
			(7*Log[2]*Zeta[3])/8 - (SumS[1, m]*Zeta[3])/8)] /; mcheck[m],
iT[Log[z_]*Log[1 + z_]^2*z_^(m_.), {z_, 0, 1}] :>
	RR["$BK[31]",
		(-2*(-((1 - (-1)^(1 + m))*(-Log[2]^2 - Log[2]*SumS[1, m/2] +
						SumS[1, m/2]*SumS[1, 1 + m] + SumS[2, m/2]/2))/2 +
				((1 + (-1)^(1 + m))*(-(Log[2]*SumS[1, (1 + m)/2]) +
					SumS[1, (1 + m)/2]*SumS[1, 1 + m] + SumS[2, (1 + m)/2]/2))/2 +
				(-1)^m*(-2*Log[2]*SumS[1, 1 + m] + SumS[1, 1 + m]^2 +
					SumS[2, 1 + m] + SumS[-1, 1, 1 + m])))/(1 + m)^2 -
		(2*(-((1 - (-1)^(1 + m))*(-(Log[2]*SumS[2, m/2]) + SumS[1, 1 + m]*
						SumS[2, m/2] + 2*SumS[1, m/2]*SumS[2, 1 + m] + SumS[3, m/2]))/
				4 + ((1 + (-1)^(1 + m))*(-(Log[2]*SumS[2, (1 + m)/2]) +
					SumS[1, 1 + m]*SumS[2, (1 + m)/2] + 2*SumS[1, (1 + m)/2]*
						SumS[2, 1 + m] + SumS[3, (1 + m)/2]))/4 +
				(-1)^(1 + m)*((Zeta2*SumS[1, 1 + m])/2 + 2*Log[2]*SumS[2, 1 + m] -
					2*SumS[1, 1 + m]*SumS[2, 1 + m] - 2*SumS[3, 1 + m] -
					SumS[-2, 1, 1 + m] - SumS[-1, 2, 1 + m] - Zeta[3]/8)))/(1 + m)] /;
	mcheck[m], iT[Log[1 - z_]*Log[z_]*Log[1 + z_]*z_^(m_.), {z_, 0, 1}] :>
	RR["$BK[32]", -((-Zeta2/2 + Log[2]^2/2 + Log[2]*SumS[-1, 1 + m] +
				SumS[-1, 1 + m]^2/2 - Log[2]*SumS[1, 1 + m] + SumS[2, 1 + m]/2)/
			(1 + m)^2) - ((-1)^(1 + m)*(Zeta2/2 - Log[2]^2/2 +
				SumS[-1, 1, 1 + m]))/(1 + m)^2 -
		((3*Zeta2*Log[2])/2 + Log[2]*SumS[-2, 1 + m] + (Zeta2*SumS[-1, 1 + m])/
				2 + SumS[-2, 1 + m]*SumS[-1, 1 + m] - Log[2]*SumS[2, 1 + m] +
			SumS[3, 1 + m] - Zeta[3])/(1 + m) -
		((-1)^(1 + m)*((-3*Zeta2*Log[2])/2 - Zeta2*SumS[-1, 1 + m] +
				SumS[-2, 1, 1 + m] + SumS[-1, 2, 1 + m] + (13*Zeta[3])/8))/
			(1 + m)] /; mcheck[m],
iT[(Log[1 - z_]*Log[z_]*Log[1 + z_]*z_^(m_.))/(1 + z_), {z_, 0, 1}] :>
	RR["$BK[33]", (-1)^(1 + m)*((4*Zeta2^2)/5 + (5*Zeta2*Log[2]^2)/4 -
			Log[2]^4/12 - 2*PolyLog[4, 1/2] - ((Zeta2 - Log[2]^2)*SumS[-2, m])/2 +
			Log[2]*SumS[-2, m]*SumS[-1, m] + (Zeta2*SumS[-1, m]^2)/4 +
			((3*Zeta2)/4 - Log[2]^2/2)*SumS[2, m] + Log[2]*SumS[3, m] -
			Log[2]*SumS[-2, 1, m] - Log[2]*SumS[-1, 2, m] - Zeta2*SumS[1, -1, m] +
			SumS[-2, -1, -1, m] + SumS[-1, -2, -1, m] + SumS[-1, -1, -2, m] +
			SumS[1, -2, 1, m] + SumS[1, -1, 2, m] + SumS[2, -1, 1, m] -
			(21*Log[2]*Zeta[3])/8 - SumS[-1, m]*((-3*Zeta2*Log[2])/2 + Zeta[3]) +
			SumS[1, m]*((-3*Zeta2*Log[2])/2 + (13*Zeta[3])/8))] /; mcheck[m],

iT[(z_^(m_.)*PolyLog[2, z_])/(1 + z_), {z_, 0, 1}] :>
	RR["$BK[36]", (-1)^m*(Zeta2*Log[2] + Zeta2*SumS[-1, m] - SumS[-2, 1, m] -
			(5*Zeta[3])/8)] /; mcheck[m],

iT[(Log[z_]*z_^(m_.)*PolyLog[2, z_])/(1 - z_), {z_, 0, 1}] :>
	RR["$BK[37]", (-3*Zeta2^2)/10 + 2*Zeta2*SumS[2, m] - SumS[2, m]^2/2 -
		SumS[4, m]/2 - 2*SumS[3, 1, m]] /; mcheck[m],

iT[(Log[1 - z_]*z_^(m_.)*PolyLog[2, z_])/(1 - z_), {z_, 0, 1}] :>
	RR["$BK[40]", (6*Zeta2^2)/5 + (Zeta2*SumS[1, m]^2)/2 +
		(Zeta2*SumS[2, m])/2 - SumS[1, 2, 1, m] - 2*SumS[2, 1, 1, m] +
		2*SumS[1, m]*Zeta[3]] /; mcheck[m],

iT[(Log[1 - x_]*x_^m_*PolyLog[2, 1 - x_])/ (1 - x_), {x_, 0, 1}] :>
RR["$BK[40v]",
	-Pi^4/90 + (Zeta2*SumS[1, m]^2)/2 + (Zeta2*SumS[2, m])/2 - SumS[1, m]^2*
	SumS[2, m] - SumS[2, m]^2 - 2*SumS[1, m]*SumS[3, m] - 2*SumS[4, m] +
	Zeta2*SumS[1, 1, m] + SumS[1, 2, 1, m] + 2*SumS[2, 1, 1, m]
	] /; mcheck[m],

iT[Log[z_]*z_^(m_.)*PolyLog[2, -z_], {z_, 0, 1}] :>
	RR["$BK[43]", -((1 + (-1)^m)*((4*Log[2])/(1 + m) + (2*SumS[1, m/2])/
					(1 + m) + SumS[2, m/2]/2))/(2*(1 + m)^2) +
		((1 - (-1)^m)*(Zeta2 + (2*SumS[1, (1 + m)/2])/(1 + m) +
				SumS[2, (1 + m)/2]/2))/(2*(1 + m)^2) +
		((-1)^m*((2*SumS[1, 1 + m])/(1 + m) + SumS[2, 1 + m]))/(1 + m)^2] /;
	mcheck[m], iT[(Log[z_]*z_^(m_.)*PolyLog[2, -z_])/(1 - z_), {z_, 0, 1}] :>
	RR["$BK[44]", (71*Zeta2^2)/40 + Zeta2*Log[2]^2 - Log[2]^4/6 -
		4*PolyLog[4, 1/2] - 2*Log[2]*SumS[-3, m] - (Zeta2*SumS[-2, m])/2 -
		SumS[-2, m]^2/2 - (Zeta2*SumS[2, m])/2 + 2*Log[2]*SumS[3, m] -
		SumS[4, m]/2 - 2*SumS[-3, -1, m] - (7*Log[2]*Zeta[3])/2] /; mcheck[m],
iT[(Log[z_]*z_^(m_.)*PolyLog[2, -z_])/(1 + z_), {z_, 0, 1}] :>
	RR["$BK[45]", (-1)^m*((13*Zeta2^2)/8 + Zeta2*Log[2]^2 - Log[2]^4/6 -
			4*PolyLog[4, 1/2] - 2*Log[2]*SumS[-3, m] + (Zeta2*SumS[-2, m])/2 +
			(Zeta2*SumS[2, m])/2 + 2*Log[2]*SumS[3, m] + SumS[2, -2, m] +
			2*SumS[3, -1, m] - (7*Log[2]*Zeta[3])/2)] /; mcheck[m],
iT[Log[1 - z_]*z_^(m_.)*PolyLog[2, -z_], {z_, 0, 1}] :>
	RR["$BK[46]", (-Zeta2/2 + Log[2]^2/2 + Log[2]*SumS[-1, 1 + m] +
			SumS[-1, 1 + m]^2/2 - Log[2]*SumS[1, 1 + m] + SumS[2, 1 + m]/2)/
			(1 + m)^2 + ((-1)^(1 + m)*(Zeta2/2 - Log[2]^2/2 + SumS[-1, 1, 1 + m]))/
			(1 + m)^2 + (Log[2]*SumS[-2, 1 + m] + (Zeta2*SumS[1, 1 + m])/2 -
			Log[2]*SumS[2, 1 + m] + SumS[-2, -1, 1 + m] + (5*Zeta[3])/8)/
			(1 + m)] /; mcheck[m], iT[Log[1 + z_]*z_^(m_.)*PolyLog[2, -z_],
	{z_, 0, 1}] :>
	RR["$BK[47]", (2*(-1)^(1 + m)*(2*Log[2]*SumS[1, 1 + m] - SumS[1, 1 + m]^2 +
				((1 - (-1)^(1 + m))*(-Log[2]^2 - Log[2]*SumS[1, m/2] +
					SumS[1, m/2]*SumS[1, 1 + m] + SumS[2, m/2]/2))/2 +
				((1 + (-1)^(1 + m))*(-(Log[2]*SumS[1, (1 + m)/2]) +
					SumS[1, (1 + m)/2]*SumS[1, 1 + m] + SumS[2, (1 + m)/2]/2))/2 -
				SumS[2, 1 + m] - SumS[-1, 1, 1 + m]))/(1 + m)^2 +
		((-1)^(1 + m)*(((1 - (-1)^(1 + m))*Zeta2*Log[2])/2 -
				Log[2]*(((1 - (-1)^(1 + m))*SumS[2, m/2])/4 +
					((1 + (-1)^(1 + m))*SumS[2, (1 + m)/2])/4 - 2*SumS[2, 1 + m]) +
				(((1 - (-1)^(1 + m))*SumS[1, m/2])/2 + ((1 + (-1)^(1 + m))*
						SumS[1, (1 + m)/2])/2 - SumS[1, 1 + m])*
				(Zeta2/2 + SumS[2, 1 + m]) + ((1 - (-1)^(1 + m))*SumS[3, m/2])/8 +
				((1 + (-1)^(1 + m))*SumS[3, (1 + m)/2])/8 - SumS[3, 1 + m] -
				SumS[-1, 2, 1 + m] - Zeta[3]/4))/(1 + m)] /; mcheck[m],
iT[(Log[1 - z_]*z_^(m_.)*PolyLog[2, -z_])/(1 + z_), {z_, 0, 1}] :>
	RR["$BK[48]", (-1)^m*((3*Zeta2^2)/2 + (3*Zeta2*Log[2]^2)/4 - Log[2]^4/6 -
			4*PolyLog[4, 1/2] + Log[2]*SumS[-2, m]*SumS[-1, m] +
			((Zeta2 - Log[2]^2)*(-SumS[-2, m] + SumS[2, m]))/2 +
			Log[2]*SumS[3, m] - Log[2]*SumS[-2, 1, m] + (Zeta2*SumS[-1, 1, m])/2 -
			Log[2]*SumS[-1, 2, m] + SumS[-2, -1, -1, m] + SumS[-1, -2, -1, m] +
			SumS[2, -1, 1, m] - (21*Log[2]*Zeta[3])/8 + (5*SumS[-1, m]*Zeta[3])/
			8)] /; mcheck[m], iT[(Log[1 + z_]*z_^(m_.)*PolyLog[2, -z_])/(1 + z_),
	{z_, 0, 1}] :> RR["$BK[49]", (-1)^m*((6*Zeta2^2)/5 + (Zeta2*Log[2]^2)/2 -
			Log[2]^4/8 - 3*PolyLog[4, 1/2] - Log[2]^2*(-SumS[-2, m] + SumS[2, m]) +
			(Zeta2*SumS[1, -1, m])/2 + Log[2]*(-(Zeta2*SumS[-1, m])/2 +
				SumS[1, m]*SumS[2, m] + SumS[3, m] - SumS[1, -2, m] -
				2*SumS[2, -1, m] + SumS[2, 1, m]) + SumS[1, 2, -1, m] +
			2*SumS[2, 1, -1, m] - SumS[1, m]*(-(Zeta2*Log[2])/2 + Zeta[3]/4) -
			(21*Log[2]*Zeta[3])/8)] /; mcheck[m],
iT[(Log[z_]*z_^(m_.)*PolyLog[2, 1 - z_])/(1 - z_), {z_, 0, 1}] :>
	RR["$BK[55]", -Zeta2^2/2 + Zeta2*SumS[2, m] - SumS[2, m]^2/2 -
		2*SumS[1, m]*SumS[3, m] - (5*SumS[4, m])/2 + 2*SumS[3, 1, m] +
		2*SumS[1, m]*Zeta[3]] /; mcheck[m],
iT[(Log[z_]*z_^(m_.)*PolyLog[2, 1 - z_])/(1 + z_), {z_, 0, 1}] :>
	RR["$BK[56]", (-1)^m*((-33*Zeta2^2)/40 - Zeta2*Log[2]^2 + Log[2]^4/6 +
			4*PolyLog[4, 1/2] - Zeta2*SumS[-2, m] + SumS[-2, 2, m] +
			2*SumS[-1, 3, m] - 2*SumS[-1, m]*Zeta[3])] /; mcheck[m],
iT[(z_^(m_.)*PhiTilde[z_])/(1 + z_), {z_, 0, 1}] :>
	RR["$BK[58]", (-1)^m*(-(Zeta2*SumS[1, m]) - 2*SumS[1, m]*
			(((1 - (-1)^m)*SumS[2, (-1 + m)/2])/4 + ((1 + (-1)^m)*SumS[2, m/2])/
				4 - SumS[2, m]) - ((1 - (-1)^m)*SumS[3, (-1 + m)/2])/8 -
			((1 + (-1)^m)*SumS[3, m/2])/8 + SumS[3, m] + 2*SumS[-2, 1, m] +
			Zeta[3]/2)] /; mcheck[m],
iT[(Log[1 - z_]*z_^(m_.)*PhiTilde[z_])/(1 + z_), {z_, 0, 1}] :>
	RR["$BK[59]", (-1)^m*((-7*Zeta2^2)/10 + Zeta2*Log[2]^2 + Log[2]^4/12 +
			2*PolyLog[4, 1/2] + Zeta2*SumS[-2, m] + (Zeta2*SumS[-1, m]^2)/2 +
			(Zeta2*SumS[2, m])/2 - SumS[-3, 1, m] - SumS[-2, 2, m] -
			SumS[-1, 3, m] - 2*Zeta2*SumS[1, -1, m] + 2*SumS[-1, -1, -2, m] +
			2*SumS[1, -2, 1, m] + 2*SumS[1, -1, 2, m] -
			SumS[-1, m]*(-3*Zeta2*Log[2] + (9*Zeta[3])/4) +
			SumS[1, m]*(-3*Zeta2*Log[2] + (13*Zeta[3])/4))] /; mcheck[m],

iT[z_^(m_.)*PolyLog[3, z_], {z_, 0, 1}] :>
	RR["$BK[60]", (-(Zeta2/(1 + m)) + SumS[1, 1 + m]/(1 + m)^2 + Zeta[3])/
		(1 + m)] /; mcheck[m], iT[(z_^(m_.)*PolyLog[3, z_])/(1 - z_),
	{z_, 0, 1}] :> RR["$BK[61]", -Zeta2^2/2 + Zeta2*SumS[2, m] -
		SumS[3, 1, m] - SumS[1, m]*Zeta[3]] /; mcheck[m],


iT[z_^(m_.)*PolyLog[3, -z_], {z_, 0, 1}] :>
	RR["$BK[62]", Zeta2/(2*(1 + m)^2) -
		((1 + (-1)^m)*(2*Log[2] + SumS[1, m/2]))/(2*(1 + m)^3) +
		((1 - (-1)^m)*SumS[1, (1 + m)/2])/(2*(1 + m)^3) +
		((-1)^m*SumS[1, 1 + m])/(1 + m)^3 - (3*Zeta[3])/(4*(1 + m))] /;
	mcheck[m],

iT[(z_^(m_.)*PolyLog[3, -z_])/(1 - z_), {z_, 0, 1}] :>
	RR["$BK[63]", (11*Zeta2^2)/10 + (Zeta2*Log[2]^2)/2 - Log[2]^4/12 -
		2*PolyLog[4, 1/2] - (Zeta2*SumS[2, m])/2 +
		Log[2]*(-SumS[-3, m] + SumS[3, m]) - SumS[-3, -1, m] -
		(7*Log[2]*Zeta[3])/4 + (3*SumS[1, m]*Zeta[3])/4] /; mcheck[m],
iT[(z_^(m_.)*PolyLog[3, -z_])/(1 + z_), {z_, 0, 1}] :>
	RR["$BK[64]", (-1)^m*(Zeta2^2/8 + (Zeta2*SumS[-2, m])/2 +
			Log[2]*(-SumS[-3, m] + SumS[3, m]) + SumS[3, -1, m] -
			(3*Log[2]*Zeta[3])/4 - (3*SumS[-1, m]*Zeta[3])/4)] /; mcheck[m],
iT[z_^(m_.)*PolyLog[3, 1 - z_], {z_, 0, 1}] :>
	RR["$BK[65]", (-(Zeta2*SumS[1, 1 + m]) + SumS[1, 1 + m]*SumS[2, 1 + m] +
			SumS[3, 1 + m] - SumS[2, 1, 1 + m] + Zeta[3])/(1 + m)] /; mcheck[m],
iT[(z_^(m_.)*PolyLog[3, 1 - z_])/(1 - z_), {z_, 0, 1}] :>
	RR["$BK[66]", (2*Zeta2^2)/5 + (Zeta2*SumS[1, m]^2)/2 +
		(Zeta2*SumS[2, m])/2 - SumS[1, 1, 2, m] - SumS[1, m]*Zeta[3]] /;
	mcheck[m], iT[(z_^(m_.)*PolyLog[3, 1 - z_])/(1 + z_), {z_, 0, 1}] :>
	RR["$BK[67]", (-1)^m*((-9*Zeta2^2)/20 + (Zeta2*Log[2]^2)/2 + Log[2]^4/24 +
			PolyLog[4, 1/2] - Zeta2*SumS[-1, 1, m] + SumS[-1, 1, 2, m] +
			(7*Log[2]*Zeta[3])/8 + SumS[-1, m]*Zeta[3])] /; mcheck[m],

iT[z_^(m_.)*( PolyLog[3, (1 - z_)/(1 + z_)]), {z_, 0, 1}] :>
	RR["$BK[68]",
iT[z^m PolyLog[3, -((1 - z)/(1 + z))],{z,0,1}]+
((-1)^(1 + m)*((3*Zeta2*Log[2])/2 + SumS[-3, 1 + m] +
				Zeta2*SumS[-1, 1 + m] + (Zeta2*SumS[1, 1 + m])/2 +
				SumS[-2, 1 + m]*SumS[1, 1 + m] - SumS[-2, 1, 1 + m] -
				SumS[-1, 2, 1 + m] - (7*Zeta[3])/8))/(1 + m) +
		((-3*Zeta2*Log[2])/2 - (Zeta2*SumS[-1, 1 + m])/2 -
			Zeta2*SumS[1, 1 + m] + SumS[1, 1 + m]*SumS[2, 1 + m] +
			SumS[3, 1 + m] - SumS[-1, -2, 1 + m] - SumS[2, 1, 1 + m] +
			(21*Zeta[3])/8)/(1 + m)] /; mcheck[m],

iT[(z_^(m_.)*PolyLog[3, (1 - z_)/(1 + z_)])/(1 + z_), {z_, 0, 1}] :>
	RR["$BK[69]", iT[z^m PolyLog[3, -((1 - z)/(1 + z))]/(1 + z),{z,0,1}] +
		(-1)^m*((19*Zeta2^2)/40 + (Zeta2*Log[2]^2)/2 - Log[2]^4/12 -
			2*PolyLog[4, 1/2] - Zeta2*SumS[-2, m] - (Zeta2*SumS[-1, m]^2)/4 -
			Zeta2*SumS[-1, m]*SumS[1, m] + (Zeta2*SumS[1, m]^2)/4 +
			2*Zeta2*SumS[1, -1, m] - SumS[-1, -1, -2, m] + SumS[-1, 1, 2, m] -
			SumS[1, -1, 2, m] + SumS[1, 1, -2, m] -
			SumS[1, m]*((-3*Zeta2*Log[2])/2 + (7*Zeta[3])/8) +
			SumS[-1, m]*((-3*Zeta2*Log[2])/2 + (21*Zeta[3])/8))] /; mcheck[m],

iT[Nielsen[1, 2, z_]*z_^(m_.), {z_, 0, 1}] :>
	RR["$BK[70]", -(SumS[1, 1 + m]^2 + SumS[2, 1 + m])/(2*(1 + m)^2) +
		Zeta[3]/(1 + m)] /; mcheck[m],
iT[(Nielsen[1, 2, z_]*z_^(m_.))/(1 - z_), {z_, 0, 1}] :>
	RR["$BK[71]", (-6*Zeta2^2)/5 + SumS[2, 1, 1, m] - SumS[1, m]*Zeta[3]] /;
	mcheck[m], iT[(Nielsen[1, 2, z_]*z_^(m_.))/(1 + z_), {z_, 0, 1}] :>
	RR["$BK[72]", (-1)^(1 + m)*(-Zeta2^2/8 - (Zeta2*Log[2]^2)/4 + Log[2]^4/24 +
			PolyLog[4, 1/2] + SumS[-2, 1, 1, m] - (Log[2]*Zeta[3])/8 -
			SumS[-1, m]*Zeta[3])] /; mcheck[m],
iT[Nielsen[1, 2, -z_]*z_^(m_.), {z_, 0, 1}] :>
	RR["$BK[73]", ((1 - (-1)^(1 + m))*(-Log[2]^2 - Log[2]*SumS[1, m/2] +
				SumS[1, m/2]*SumS[1, 1 + m] + SumS[2, m/2]/2))/(2*(1 + m)^2) -
		((1 + (-1)^(1 + m))*(-(Log[2]*SumS[1, (1 + m)/2]) +
				SumS[1, (1 + m)/2]*SumS[1, 1 + m] + SumS[2, (1 + m)/2]/2))/
			(2*(1 + m)^2) + ((-1)^(1 + m)*(-2*Log[2]*SumS[1, 1 + m] +
				SumS[1, 1 + m]^2 + SumS[2, 1 + m] + SumS[-1, 1, 1 + m]))/(1 + m)^2 +
		Zeta[3]/(8*(1 + m))] /; mcheck[m],

iT[(Nielsen[1, 2, -z_]*z_^(m_.))/(1 + z_), {z_, 0, 1}] :>
	RR["$BK[74]", (-1)^(1 + m)*((6*Zeta2^2)/5 + (3*Zeta2*Log[2]^2)/4 -
			Log[2]^4/8 - 3*PolyLog[4, 1/2] - (Log[2]^2*(-SumS[-2, m] + SumS[2, m]))/
			2 + Log[2]*(-SumS[2, -1, m] + SumS[2, 1, m]) + SumS[2, 1, -1, m] -
			(11*Log[2]*Zeta[3])/4 - (SumS[-1, m]*Zeta[3])/8)] /; mcheck[m],
iT[Nielsen[1, 2, 1 - z_]*z_^(m_.), {z_, 0, 1}] :>
	RR["$BK[75]", -((SumS[3, 1 + m] - Zeta[3])/(1 + m))] /; mcheck[m],
iT[(Nielsen[1, 2, 1 - z_]*z_^(m_.))/(1 - z_), {z_, 0, 1}] :>
	RR["$BK[76]", Zeta2^2/10 + SumS[1, m]*SumS[3, m] + SumS[4, m] -
		SumS[3, 1, m] - SumS[1, m]*Zeta[3]] /; mcheck[m],
iT[(Nielsen[1, 2, 1 - z_]*z_^(m_.))/(1 + z_), {z_, 0, 1}] :>
	RR["$BK[77]", (-1)^(1 + m)*((19*Zeta2^2)/40 + SumS[-1, 3, m] -
			(7*Log[2]*Zeta[3])/4 - SumS[-1, m]*Zeta[3])] /; mcheck[m],
iT[PolyLog[2, u_ + (z_) - u_*(z_)]/(1 - (z_)) , {z_, 0, 1}] :>
	RR["$BK[78]", Log[1 - u]*PolyLog[2, 1 - u] - 2*PolyLog[3, 1 - u]]/;
	FreeQ[u,z],
iT[Nielsen[1, 2, z_]/z_ , {z_, 0, 1}] :> Pi^4/360

(* YYY *)
(*
,
(*X*)
*)
	};

RR[a_,b_] := (FCPrint[3,a];b);

FCPrint[1,"Integrate3.m loaded."];
End[]
