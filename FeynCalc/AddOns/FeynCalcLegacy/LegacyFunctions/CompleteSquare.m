(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* :Title: CompleteSquare *)

(* :Author: Frederik Orellana *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 5 July 2001 at 13:15 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

CompleteSquare::usage =
"CompleteSquarep[exp, x] completes the square of a second order polynomial in
the momentum x.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`CompleteSquare`Private`"]

CompleteSquare[e_, x_ ,y_:Null] :=
	Module[ {a, b, c, xx, ex, exp, dims, dim, rul, pa},

		If[	!FreeQ2[{e,x,y}, FeynCalc`Package`NRStuff],
			Message[FeynCalc::nrfail];
			Abort[]
		];

		(* Make sure all momenta have the same dimension *)
		dims = FCGetDimensions[e];

		Which[
			Length[dims] == 1,
				dim = dims[[1]];
				xx = Momentum[x,dim];
				ex = e,
			True,
				dim = dims[[1]];
				xx = Momentum[x, dim];
				rul = ((Rule@@#)& /@ Transpose[{dims, Table[dim,{Length[dims]}]}]);
				ex = e //. rul;
		];

		exp = Expand[ExpandScalarProduct[Contract[ex]]]/. {
			Pair[pp:Momentum[x,___],p:Momentum[_?(FreeQ[#,x]&),___]]:>p pp,
			Pair[p:Momentum[_?(FreeQ[#,x]&),___],pp:Momentum[x,___]]:>p pp
		};

		pa = Pair[xx,xx];
		a = Coefficient[exp, pa, 1];
		If[ Length[CoefficientList[exp,x]]>3 || Length[CoefficientList[exp,pa]]>2||a===0,
			exp,

			b = Coefficient[exp, xx, 1 ];
			c = Coefficient[Coefficient[exp, xx, 0 ], pa, 0 ];
			If[ y===Null,
				-Pair[b,b]/(4 a) + c + a Pair[(b/(2a)+xx),(b/(2a)+xx)],
				{-Pair[b,b]/(4 a) + c + a Pair[Momentum[y,dim],Momentum[y,dim]],
					Momentum[y,dim]->(b/(2a)+xx)}
			]
		]
	];

FCPrint[1,"CompleteSquare.m loaded."];
End[]
