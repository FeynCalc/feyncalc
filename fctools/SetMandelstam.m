(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SetMandelstam *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)

(* :Summary: define kinematical invariants *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`SetMandelstam`",
             "HighEnergyPhysics`FeynCalc`"];
SetMandelstam::"usage"=
"SetMandelstam[s, t, u, p1, p2, p3, p4, m1, m2, m3, m4] defines the
Mandelstam variables  s=(p1+p2)^2, t=(p1+p3)^2, u=(p1+p4)^2 and sets
the pi on-shell: p1^2=m1^2, p2^2=m2^2, p3^2=m3^2, p4^2=m4^2.
Note that p1 +  p2 + p3 + p4 = 0 is assumed.\n\n

SetMandelstam[x, {p1, p2, p3, p4, p5}, {m1, m2, m3, m4, m5}]
defines x[i, j] = (pi+pj)^2 and sets the pi on-shell.
The pi satisfy: p1 + p2 + p3 + p4 + p5 = 0.";


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   
ExpandScalarProduct = MakeContext["ExpandScalarProduct"];
FeynCalcExternal = MakeContext["FeynCalcExternal"];
SP = MakeContext["SP"];
SPD = MakeContext["SPD"];
Cases2 = MakeContext["Cases2"];
ChangeDimension = MakeContext["ChangeDimension"];
FreeQ2 = MakeContext["FreeQ2"];
Dimension = MakeContext["Dimension"];
fci = MakeContext["FeynCalcInternal"];
Momentum = MakeContext["Momentum"];
NumericalFactor = MakeContext["NumericalFactor"];
Pair            = MakeContext["Pair"];
ScalarProduct = MakeContext["ScalarProduct"];
small = MakeContext["SmallVariable"];

Options[SetMandelstam] = {Dimension -> {4, D, ___}};

(*sma*)
small2/: small2[x_]^n_ := small2[x^2] /; n > 0;
small2/: small2[_] a_ :=0;
small3/: small3[_] + a_ :=a;
small4[x_^m_]:=small[x]^m;
   sma[x_]:=x/;FreeQ[x,small];
   sma[x_]:=x/.small->small2/.small2->small3/.
                         small3->small4/.small4->small;


setit[a_,b_,___]:=set[a,sma[(b//Expand)]]/.set->Set;
(* SetMandelstamdef *)
SetMandelstam[s_,t_,u_, { {p1_, m12_}, {p2_, m22_} } ->
                        { {p3_, m32_}, {p4_, m42_} }] :=
SetMandelstam[s,t,u, p1, p2, -p3, -p4, Sqrt[m12], Sqrt[m22],
                                       Sqrt[m32], Sqrt[m42]];
SetMandelstam[s_,t_,u_,p1_,p2_,p3_,p4_,m1_,m2_,m3_,m4_,opt___Rule]:=
 Block[ {settemp,oldmem,setvars,sol,pp1, pp2, pp3, pp4, dims, $MemoryAvailable = 0},
       dims = Flatten[{Dimension /. {opt} /. Options[SetMandelstam]}];
       If[Head[dims] =!= List, dims = { dims }];
      (* note that p1, p2, p3, p4 may have have a minus - sign *)
      {pp1, pp2, pp3, pp4} = #/NumericalFactor[#] & /@ {p1, p2, p3, p4} ;
     scd[a_,b_, 4] := ScalarProduct[a,b];
     scd[a_,b_, d_] := ScalarProduct[a,b,Dimension->d];
      settemp = Union[Flatten[Join[Table[{
                scd[p1,p1,dims[[i]]] == m1^2,
                scd[p2,p2,dims[[i]]] == m2^2,
                scd[p3,p3,dims[[i]]] == m3^2,
                scd[p4,p4,dims[[i]]] == m4^2,
                scd[p1,p2,dims[[i]]] == sma[1/2 s - 1/2 m1^2 - 1/2 m2^2],
                scd[p1,p3,dims[[i]]] == sma[1/2 t - 1/2 m1^2 - 1/2 m3^2],
                scd[p1,p4,dims[[i]]] == sma[1/2 u - 1/2 m1^2 - 1/2 m4^2],
                scd[p2,p3,dims[[i]]] == sma[1/2 u - 1/2 m2^2 - 1/2 m3^2],
                scd[p2,p4,dims[[i]]] == sma[1/2 t - 1/2 m2^2 - 1/2 m4^2],
                scd[p3,p4,dims[[i]]] == sma[1/2 s - 1/2 m3^2 - 1/2 m4^2]
                      }, {i, Length[dims]}
                    ]]]];
    If[FreeQ2[{p1,p2,p3,p4}, {Plus,Times}],
       settemp = Union[settemp, FeynCalcExternal[settemp]];
       sol = settemp /. Equal ->setit,
       (* else *)
       settemp = settemp//fci;
       settemp = settemp//ExpandScalarProduct;
       settemp = settemp//Expand;
       settemp = Union[settemp, FeynCalcExternal[settemp]];
(* want also for 4 or D dimensions to set SP[p,p] = ... etc. *)
      setvars = Cases2[settemp, {Pair, SP, SPD}];
Global`SE=setvars;
      If[Complement[Head/@setvars,{Pair, SP, SPD}] === {}, 
         sol = Solve[ settemp,setvars ]/.Rule->setit
        ];
      ];
     sol
   ];

(* #################################################################### *)
(*                             Main56                                   *)
(* #################################################################### *)

scalarproduct[a_, b_] := fci[ScalarProduct[a,b]]//ExpandScalarProduct;

SetMandelstam[x_, pl_List, ml_List, opt___?OptionQ]:=Block[
          {settemp,oldmem,setvars,sol,n=Length[ml], psu,pkl,sq2,eqq,ppl,dims},
      oldmem = $MemoryAvailable;
      $MemoryAvailable = 0;
     dims = Flatten[{Dimension /. {opt} /. Options[SetMandelstam]}];
     ppl = #/NumericalFactor[#] & /@ pl;
(*
     Table[ setdel[ Momentum[ppl[[ij]], _Symbol], Momentum[ppl[[ij]]] ],
            {ij, Length[ppl]} ] /. setdel -> SetDelayed;
*)

      settemp = Join[ Table[scalarproduct[pl[[i]], pl[[i]]] == ml[[i]]^2,
                            {i,1,n}],
                      Table[scalarproduct[pl[[j]], pl[[j+1]]] == 
                    sma[1/2 x[j,j+1] - 1/2 ml[[j]]^2 - 1/2 ml[[j+1]]^2],
                            {j,1,n-1}],
                     {scalarproduct[ pl[[1]],pl[[n]] ] ==
                    sma[1/2 x[1,n] - 1/2 ml[[1]]^2 - 1/2 ml[[n]]^2]}
                    ]//ExpandScalarProduct//Expand;

      setvars = Cases[settemp, _Pair, -1];
      settemp = Union[Join@@Map[ChangeDimension[settemp, #]&, dims]];
      settemp = Union[Join[settemp, FeynCalcExternal[settemp]]];
      setvars = Union[Join@@Map[ChangeDimension[setvars, #]&, dims]];
      setvars = Union[Join[setvars, FeynCalcExternal[setvars]]];
      sol = Solve[ settemp,setvars ]/.Rule->setit;

  sq2[y_]:=scalarproduct[y, y]//ExpandScalarProduct//Expand;
  pkl = {};
  For[ k=1, k<=n, k++,
       For[ l=k+1, l<= n, l++,
            npk = scalarproduct[ pl[[k]], pl[[l]] ]//ExpandScalarProduct;
            If[ (Head[npk] === Pair) || (Head[-npk]=== Pair),
                AppendTo[pkl,{pl[[k]], pl[[l]]}] 
              ]
          ]
     ];          
            
  psu = Plus@@pl;
  enm[a_]:=Expand[ - Apply[ Plus, Drop[pl,{a,a}] ]  ];
 (* p46 *)

Do[
  eqq = {sq2[psu] == 0};
  eqq = Join[ eqq, Table[ sq2[pl[[ii]] +  pl[[n]]] -
                          sq2[enm[ii] + pl[[n]]] ==0 , {ii, 2,n-3}]
            ];
  For[ j1 = 1, j1<n-2, j1++,
       For[ j2 = j1 + 2, j2<n, j2++,
            If[ EvenQ[j2-j1],
                AppendTo[ eqq, sq2[pl[[j1]] + pl[[j2]]] -
                               sq2[pl[[j1]] + enm[j2] ] ==0
                        ],
                AppendTo[ eqq, sq2[pl[[j1]] + pl[[j2]]] -
                               sq2[enm[j1]  + pl[[j2]]] ==0
                        ]
     ]    ]   ];
  var =  ExpandScalarProduct[scalarproduct@@#&/@pkl];
  var = Select[ Variables[var], Head[#]===Pair&];
  If[Length[dims]>0,
     eqq = Union[Join@@Map[ChangeDimension[eqq, #]&, dims]];
     eqq = Union[Join[eqq, FeynCalcExternal[eqq]]];
     var = Union[Join@@Map[ChangeDimension[var, #]&, dims]];
     var = Union[Join[var, FeynCalcExternal[var]]];
    ];
  If[Length[var] > 0,
  nsol = Solve[ eqq, var ];
  nsol = nsol /. Rule -> setit
    ]
, {2}];

  $MemoryAvailable = oldmem;
  MapAll[ Expand, Append[sol, nsol]//Flatten ]
   ];

End[]; EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SetMandelstam | \n "]];
Null
