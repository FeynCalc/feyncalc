(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Collect2 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: last changed: 10th Jan. 2010;  19th July 2000*)
(* ------------------------------------------------------------------------ *)

(* :Summary: Extension of the Mathematica Collect *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`Collect2`",{"HighEnergyPhysics`FeynCalc`"}];

Collect2::"usage"=
"Collect2[expr, x] collects together terms which are not free of any 
occurrence of x. 
Collect2[expr, {x1, x2, ...}]  (or also Collect2[expr, x1, x2,  ...]) 
collects together terms which are not free of any occurrence of 
x1, x2, ... . 
The coefficients are put over a common denominator.
 If expr is expanded before collecting depends on the option  Factoring, 
which may be set to Factor, Factor2, or any other function, 
which is applied to the coefficients. 
If expr is already expanded with respect to x, the 
option Expanding can be set to False.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"]

   

MakeContext[ Combine, Expanding ,
Expand2, Factor2, Factoring, FeynCalcForm, FreeQ2,
Isolate, IsolateNames, ComplexConjugate];

Options[Collect2] = {Denominator -> False,
                     Dot -> False,
                     Expanding -> True,
                     Factoring -> Factor, IsolateNames -> False } ;

(* for the CCollect stuff *)
If[!ValueQ[$FeynC], $FeynC = True];

Collect2[a_ == b_, y__] := Collect2[a,y] == Collect2[b,y];
Collect2[x_List, y__]      := Collect2[#, y]& /@ x;
Collect2[x_, y_, r___Rule] := Collect2[x, {y}, r] /; Head[y]=!=List;
Collect2[x_, z__, y_, r___Rule] := Collect2[x, {z,y}, r] /; 
                                    (Head[y]=!=List) && 
                                    (Head[y]=!=Rule);
(* Collect2[x_, y_List, ___]  := x /; FreeQ2[x, y]; *)

Collect2[ expr_, vv_List,r___Rule ] := Block[
{v,ru,nx,lk,fa,in,ih,pr,wr,tog,fr0,frx,lin,tv,mp,mp2,cd,i,co,ara,dde,
 tim,new = 0, einss,re,compCON,ccflag = False, thc, ish, factor,exo,
 times},
{fa, ih, exo, dde} = {Factoring, IsolateNames, Expanding,Denominator}/. 
                Join[{r}, Options[Collect2]];
Which[(Dot /. {r}/.Options[Collect2]) === True,
   times = Dot,
   (Dot /. {r}/.Options[Collect2]) === False,
   times = Times,
   True,
   times = (Dot /. {r}/.Options[Collect2])
  ];
If[fa === True || fa === Factor2, factor = Factor2,
   If[fa =!= False, factor = fa; fa = True, factor = Identity];
  ];
v = Select[ vv, ((Head[#] =!= Plus) && (Head[#] =!= Times) && 
               (!NumberQ[#]))& ];
If[Length[v] =!= Length[Union[v]], v = Union[v]];
     
v = Select[ v, !FreeQ[expr, #]&];

If[Length[v] === 0,  re = expr,

tim = Timing[
(* nx = Operate[Hold, expr]; *)
nx = expr;
(* Hm, that's a problem, maybe *)
If[CheckContext["ComplexConjugate"],
   If[!FreeQ[nx, ComplexConjugate], 
     ccflag = True;
     nx = nx /. ComplexConjugate -> compCON;
     v = v /. ComplexConjugate -> compCON;
     ];
  ];

nx = nx/. HoldForm[k_[ii_]] -> lk[k][ii];
in = $VeryVerbose; If[!NumberQ[in], in = 0];
SetAttributes[{pr, wr}, HoldAll]; 
pr[i_, x__] := Print[x] /; in >= i;
wr[i_, x__] := WriteString["stdout", x] /; in >= i;
If[ fa === False,  
    tog[x_] := FixedPoint[ReleaseHold, x] (*/. Power2->Power*)
   ,
    fr0[x__] := Plus[x] /; !FreeQ2[{x}, v];
    tog[x_]  := factor[FixedPoint[ReleaseHold, x]](* /. Power2->Power*);
    frx[x__] := HoldForm[Plus[x]];

    nx = nx /. Plus -> fr0 /. fr0 -> frx 
  ];     
If[ exo =!= False, wr[2,"expanding. "]; 
    nx  = Expand2[nx,v]
  ];

(* lin denotes the part free of v *)
lin = Select[nx + ze1 + ze2, FreeQ2[#, v]&] /. ze1 -> 0 /. ze2 -> 0;
nx  = nx - lin;
If[fa =!= False,   wr[2, "inhomogeneous part; LeafCount = ", LeafCount[lin]];
   lin = tog[lin]; wr[2, "; factored. "] 
  ];
tv = {}; (* tv is the list of all monomials to collect *)
mp[x_] := ((* "tv" is calculated as a side effect ! *)
                If[FreeQ2[x, v], x, t1 = Select[x t2, !FreeQ2[#, v]&];
                If[!MemberQ[tv, mp2[t1]], AppendTo[tv, mp2[t1]] ];
                (Select[x t2, FreeQ2[#, v]&]/t2) mp2[t1] ] 
          )(*endBlock*);
nx = (mp /@ (nx + ze) ) /. ze -> 0 /. mp -> mp2; 
pr[2,"length ",nx//Length,"."];

If[dde === True,
(* In case of denominators containing variables to be collected *)
cd[x_] := ((Numerator[#]/(factor[Denominator[#]] /.
   Plus-> (Collect2[Plus[##], v, r]&)))& @ x ) /; 
   (!FreeQ[Denominator[x], Plus]) && (!FreeQ2[Denominator[x], v])
  ];

If[Length[tv]>1, pr[2, "collecting ",Length[tv], " terms."]];
For[ i = 1, i <= Length[tv], i++, wr[2, "#",i];
     co = (*tog[*) Coefficient[ nx, tv[[i]] ] (*]*);
(*
     If[Head[co] === Plus, co = tog[einss co] ];
*)
     co = tog[einss co];
     nx = nx /. tv[[i]] -> 0;
     If[ ih =!= False, 
         co = Isolate[co /. {einss:>1, lk[ka_][j_] :> HoldForm[ka[j]]},ara , 
                      IsolateNames -> ih];
         If[dde =!= True,
         new = new + ( times[ Isolate[FixedPoint[ReleaseHold, tv[[i]] /.
                              lk[ka_][j_] -> HoldForm[ka[j]]] /. 
                              mp2 -> Identity, v, IsolateNames -> ih 
                              ], co ] /. einss -> 1) ,
         new = new + ( times[ Isolate[FixedPoint[ReleaseHold, tv[[i]] /.
                              lk[ka_][j_] -> HoldForm[ka[j]]] /. 
                              mp2 -> cd /. 
                              cd -> Identity, v, IsolateNames -> ih 
                                     ], co]
                     ) /. einss -> 1
           ],
         If[dde =!= True,
         new = new + (times[ FixedPoint[ReleaseHold, tv[[i]]] /. 
                                mp2 -> cd /. 
                                cd -> Identity ,co
                           ] /. einss->1
                     ),
         new = new + (times[ FixedPoint[ReleaseHold, tv[[i]]] /. 
                                mp2 -> cd /. 
                                cd -> Identity  ,co
                           ] /. einss->1
                     )
           ]
   ]  ]    ][[1]];
If[tim/Second > 1,
wr[2,".\n"]; 
pr[2, "collected. time needed = ", tim //FeynCalcForm];
  ];
If[ ih =!= False, 
    lin = Isolate[ FixedPoint[ReleaseHold, lin], v, IsolateNames->ih ],
    lin = FixedPoint[ReleaseHold, lin] ];
re = ((nx + new + lin) /. lk[ka_][j_] -> HoldForm[ka[j]] /. 
     frx->Plus);
If[ccflag, re = re /. compCON -> ComplexConjugate];
  ](*endIf*);
einss=1;
re];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Collect2 | \n "]];
Null
