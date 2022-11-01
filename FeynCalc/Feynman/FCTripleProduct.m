(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCTripleProduct													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2022 Rolf Mertig
	Copyright (C) 1997-2022 Frederik Orellana
	Copyright (C) 2014-2022 Vladyslav Shtabovenko
*)

(* :Summary: Ghost propagator												*)

(* ------------------------------------------------------------------------ *)

FCTP::usage =
"FCTP is an alias for FCTripleProduct.";

FCTripleProduct::usage =
"FCTripleProduct[a,b,c] returns the triple product $a \\cdot (b \\times c)$. By
default a,b and c are assumed to be Cartesian vectors. Wrapping the arguments
with CartesianIndex will create an expression with open indices.

If any of the arguments is noncommutative, DOT will be used instead of Times
and the function will introduce dummy indices. To give those indices some
specific names, use the option CartesianIndexNames.

If the arguments already contain free CartesianIndices, the first such index
will be used for the contraction.

To obtain an explicit expression you need to set the option Explicit to True
or apply the function Explicit";

FCTripleProduct::failmsg =
"Error! ExpandPartialD has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCTripleProduct`Private`"]

FCTP = FCTripleProduct;

Options[FCTripleProduct] = {
	FCE					-> False,
	CartesianIndexNames	-> {},
	Dimension			-> 3,
	Explicit			-> False,
	NonCommutative		-> Automatic
};

getFirstFreeCartesianIndex[x_]:=
	Select[Tally[Cases[x, CartesianIndex[__], Infinity]], (Last[#] === 1) &];

FCTripleProduct[a_, b_, c_, OptionsPattern[]] :=
	Block[{	dummyI,dummyJ,dummyK,vecA,vecB,vecC,tmp, optAtomQ,holdDOT,res,
			optCartesianIndexNames, dim, optNonCommutative},

		{dummyI,dummyJ,dummyK} = {Unique[$AL], Unique[$AL], Unique[$AL]};

		optCartesianIndexNames	= OptionValue[CartesianIndexNames];
		optNonCommutative 		= OptionValue[NonCommutative];
		dim 					= OptionValue[Dimension];


		If[optCartesianIndexNames =!= {},
			Switch[
				Length[optCartesianIndexNames],
				1,
				{dummyI}= optCartesianIndexNames,
				2,
				{dummyI, dummyJ} = optCartesianIndexNames,
				_,
				{dummyI,dummyJ,dummyK}= optCartesianIndexNames[[1;;3]]
			]
		];
		If[	TrueQ[Head[a]=!=CartesianIndex],
			tmp = getFirstFreeCartesianIndex[a];
			If[	TrueQ[Length[tmp]===1],
					dummyI = First@First@First@tmp;
					vecA = a,
					vecA = CartesianPair[CartesianMomentum[a,dim],CartesianIndex[dummyI,dim]]
			],
			vecA = CartesianPair[CartesianIndex[First[a],dim],CartesianIndex[dummyI,dim]]
		];

		If[	TrueQ[Head[b]=!=CartesianIndex],
			tmp = getFirstFreeCartesianIndex[b];
			If[	TrueQ[Length[tmp]===1],
					dummyJ = First@First@First@tmp;
					vecB = b,
					vecB = CartesianPair[CartesianMomentum[b,dim],CartesianIndex[dummyJ,dim]]
			],
			vecB = CartesianPair[CartesianIndex[First[b],dim],CartesianIndex[dummyJ,dim]]
		];

		If[	TrueQ[Head[c]=!=CartesianIndex],
			tmp = getFirstFreeCartesianIndex[c];
			If[	TrueQ[Length[tmp]===1],
					dummyK = First@First@First@tmp;
					vecC = c,
					vecC = CartesianPair[CartesianMomentum[c,dim],CartesianIndex[dummyK,dim]]
			],
			vecC = CartesianPair[CartesianIndex[First[c],dim],CartesianIndex[dummyK,dim]]
		];


		res = Eps[CartesianIndex[dummyI,dim],CartesianIndex[dummyJ,dim],CartesianIndex[dummyK,dim]] holdDOT[vecA,vecB,vecC];

		Switch[optNonCommutative,
			Automatic,
			If[	NonCommFreeQ[res],
				res = res /. holdDOT -> Times,
				res = res /. holdDOT -> DOT
			],
			True,
			res = res /. holdDOT -> DOT,
			False,
			res = res /. holdDOT -> Times,
			_,
			Message[FCTripleProduct::failmsg,"Unrecognized value for the NonCommutative option."];
			Abort[]
		];

		res = res /. CartesianPair -> CartesianPairContract /. CartesianPairContract -> CartesianPair;

		If[OptionValue[FCE],
			res = FCE[res]
		];

		res

	] /; OptionValue[Explicit];

FCTripleProduct /:
	MakeBoxes[FCTripleProduct[a_,b_,c_, OptionsPattern[]], TraditionalForm]/; (Head[a]=!=CartesianIndex) && (Head[b]=!=CartesianIndex) && (Head[c]=!=CartesianIndex)  :=
		RowBox[{TBox[CartesianMomentum[a]], "\[CenterDot]","(", TBox[CartesianMomentum[b]],"\[Cross]", TBox[CartesianMomentum[c]], ")"}];

FCTripleProduct /:
	MakeBoxes[FCTripleProduct[a_CartesianIndex,b_,c_, OptionsPattern[]], TraditionalForm]/; (Head[b]=!=CartesianIndex) && (Head[c]=!=CartesianIndex)  :=
		SuperscriptBox[RowBox[{"(", TBox[CartesianMomentum[b]],"\[Cross]", TBox[CartesianMomentum[c]], ")"}],TBox[a]];

FCTripleProduct /:
	MakeBoxes[FCTripleProduct[a_, b_CartesianIndex, c_, OptionsPattern[]], TraditionalForm]/; (Head[a]=!=CartesianIndex) && (Head[c]=!=CartesianIndex)  :=
		SuperscriptBox[RowBox[{"-(", TBox[CartesianMomentum[a]], "\[Cross]", TBox[CartesianMomentum[c]], ")"}], TBox[b]];

FCTripleProduct /:
	MakeBoxes[FCTripleProduct[a_,b_,c_CartesianIndex, OptionsPattern[]], TraditionalForm]/; (Head[a]=!=CartesianIndex) && (Head[b]=!=CartesianIndex)  :=
		SuperscriptBox[RowBox[{"(", TBox[CartesianMomentum[a]],"\[Cross]", TBox[CartesianMomentum[b]], ")"}], TBox[c]];

FCTripleProduct /:
	MakeBoxes[FCTripleProduct[a_CartesianIndex,b_CartesianIndex,c_, OptionsPattern[]], TraditionalForm]/; (Head[c]=!=CartesianIndex)  :=
		ToBoxes[Eps[a,b,c],TraditionalForm];

FCTripleProduct /:
	MakeBoxes[FCTripleProduct[a_CartesianIndex,b_,c_CartesianIndex, OptionsPattern[]], TraditionalForm]/; (Head[b]=!=CartesianIndex)  :=
		ToBoxes[Eps[a,b,c],TraditionalForm];

FCTripleProduct /:
	MakeBoxes[FCTripleProduct[a_,b_CartesianIndex,c_CartesianIndex, OptionsPattern[]], TraditionalForm]/; (Head[a]=!=CartesianIndex)  :=
		ToBoxes[Eps[a,b,c],TraditionalForm];

FCTripleProduct /:
	MakeBoxes[FCTripleProduct[a_CartesianIndex,b_CartesianIndex,c_CartesianIndex, OptionsPattern[]], TraditionalForm]  :=
		ToBoxes[Eps[a,b,c],TraditionalForm];

FCPrint[1,"FCTripleProduct.m loaded"];
End[]
