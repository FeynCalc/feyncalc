(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* :Title: CompleteSquare *)

(* :Author: Frederik Orellana *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 5 July 2001 at 13:15 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`CompleteSquare`",
             "HighEnergyPhysics`FeynCalc`"];

CompleteSquare::usage=
"Completes the square of a second order polynomial in the momentum x. \
CompleteSquare[a q^2+b q+c,q] -> -b^2/(4 a)+c+a (b/(2a)+x)^2. \
CompleteSquare[a q^2+b q+c,q,y] -> {-b^2/(4 a)+c+ay^2,y->b/(2a)+x}.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

Momentum = MakeContext["Momentum"];
Pair = MakeContext["Pair"];
Contract = MakeContext["Contract"];
ScalarProductExpand = MakeContext["ScalarProductExpand"];
Dimension = MakeContext["Dimension"];


CompleteSquare[e_, x_ ,y_:Null] :=
Module[ {a, b, c, xx, ex, exp, dims, dim, rul, pa,p},

  (* Make sure all momenta have the same dimension *)
  dims = Union[Cases[e, (Dimension->_)|(Momentum[_,_]),Infinity]];
  If[dims =!= {}, dims = Union[(#[[2]])& /@ dims]];
  Which[
    Length[dims] == 0,
    dim = Sequence[]; xx = Momentum[x]; ex = e;,
    Length[dims] == 1,
    dim = dims[[1]]; xx = Momentum[x,dim]; ex = e;,
    True,
    dim = dims[[1]];
    xx = Momentum[x, dim];
    rul = ((Rule@@#)& /@ Transpose[
    {dims, Table[dim,{Length[dims]}]}]);
    ex = e //. rul;
  ];

  exp = Expand[ScalarProductExpand[Contract[ex]]]/.
  {Pair[pp:Momentum[x,___],p:Momentum[_?(FreeQ[#,x]&),___]]:>p*pp,
  Pair[p:Momentum[_?(FreeQ[#,x]&),___],pp:Momentum[x,___]]:>p*pp};

 pa = Pair[xx,xx];

 a = Coefficient[exp, pa, 1];

 If[Length[CoefficientList[exp,x]]>3||
    Length[CoefficientList[exp,pa]]>2||a===0,

    exp,

    b = Coefficient[exp, xx, 1 ];
    c = Coefficient[Coefficient[exp, xx, 0 ], pa, 0 ] ;
    If[y===Null,
    -Pair[b,b]/(4 a)+c +a Pair[(b/(2a)+xx),(b/(2a)+xx)],
    {-Pair[b,b]/(4 a)+c +a Pair[Momentum[y,dim],Momentum[y,dim]],
       Momentum[y,dim]->(b/(2a)+xx)}]]
];


End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "CompleteSquare | \n "]];
Null
