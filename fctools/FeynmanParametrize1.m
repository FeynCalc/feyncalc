(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* :Title: FeynmanParametrize1 *)

(* :Author: Frederik Orellana *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 16 March 2001 at 15:36 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`FeynmanParametrize1`",
             "HighEnergyPhysics`FeynCalc`"];

FeynmanParametrize1::usage=
"FeynmanParametrize1[exp,k,Method->Denominator] introduces Feynman \
parameters for all one-loop integrals in exp (k = integration momentum) using \
formula (11.A.1) from \"The Quantum Theory of Fields\" vol. 1 by \
Steven Weinberg.
FeynmanParametrize1[exp,k,Method->Exp] introduces Feynman \
parameters for all one-loop integrals in exp (k = integration momentum) using \
1/(A-I eps) = I Integrate[Exp[-I x (A-I eps)],{x,0,Infinity}, \
    Assumptions->{Arg[A]==0,Arg[eps]==0}]. \
In this case, when the option Integrate is set to True, odd factors of \
k-tensors are dropped and even factors are replaced according to \
Itzykson&Zuber (8-117).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];



FeynAmpDenominatorCombine = MakeContext["FeynAmpDenominatorCombine"];
FeynAmpDenominator = MakeContext["FeynAmpDenominator"];
FeynmanParameterNames = MakeContext["FeynmanParameterNames"];
FeynCalcInternal = MakeContext["FeynCalcInternal"];
Integratedx = MakeContext["Integratedx"];
Momentum = MakeContext["Momentum"];
Pair = MakeContext["Pair"];
PropagatorDenominator = MakeContext["PropagatorDenominator"];
Dimension = MakeContext["Dimension"];
LorentzIndex = MakeContext["LorentzIndex"];
Uncontract = MakeContext["Uncontract"];
Contract = MakeContext["Contract"];
ScalarProductExpand = MakeContext["ScalarProductExpand"];
FeynmanParametrize1 = MakeContext["FeynmanParametrize1"];
Dimension = MakeContext["Dimension"];
LorentzIndex = MakeContext["LorentzIndex"];

Options[FeynmanParametrize1] =
 {FeynmanParameterNames -> {Global`a, Global`b, Global`c, Global`d, Global`e},
 Method -> Denominator, Integrate -> True, Flatten -> True};

(* Completes the square of a second order polynomial e in x:
   CompleteTheSquare[a q^2+b q+c,q]->
           -b^2/(4 a)+c+a (b/(2a)+x)^2
   CompleteTheSquare[a q^2+b q+c,q,y]->
          {-b^2/(4 a)+c+ay^2,y->b/(2a)+x} *)

CompleteTheSquare[e_, x_ ,y_:Null] :=
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

fpar[{par__}][k_][
      f:FeynAmpDenominator[PropagatorDenominator[__]..]] :=
  Block[{i, n=Length[f], dum, pars=dum@@{par}},
pars=ReplacePart[pars,0,n];

(n-1)! * (Dot@@Table[Integratedx[pars[[i]],0,pars[[i-1]]],{i,n-1}]) .
Sum[(Pair[f[[i,1]],f[[i,1]]]-f[[i,2]]^2)*
      (pars[[n-i]]-pars[[n-i+1]]),{i,n}]^(-n)/.dum->1

   ];

epar[{par__}][k_][(f:FeynAmpDenominator[PropagatorDenominator[__]..])]:=
    epar[{par}][k][dum*f]/.dum->1;

epar[{par__}][k_][
     (rest___)(f:FeynAmpDenominator[PropagatorDenominator[__]..])] :=
Block[{i, n=Length[f], pars={par}, res, y, ee, rr, cc, exp,
       mom, res1, rest1, dum, k2coeff, p},

(* Exponential factors already there *)
k2coeff=0;
Which[
MatchQ[Times[rest],_*E^(_?(!FreeQ[#,k]&))]===True,
rest1=dum*rest /. rr_*E^(ee_?(!FreeQ[#,k]&)) :> {rr,ee} /. dum -> 1;
k2coeff=-Coefficient[rest1[[2]]/.Momentum[k,_] :> Momentum[k],Momentum[k]],
MatchQ[Times[rest],E^(_?(!FreeQ[#,k]&))]===True,
rest1=dum*rest /. E^(ee_?(!FreeQ[#,k]&)) :> {1,ee} /. dum -> 1;
k2coeff=-Coefficient[rest1[[2]]/.Momentum[k,_] :> Momentum[k],
                    Pair[Momentum[k],Momentum[k]]],
True,
rest1={rest,0}];

If[$VeryVerbose>=2,Print["Found coefficient of ", k^2, ": ", k2coeff, " in exponent"]];

If[k2coeff===0,k2coeff=1];

res1 = rest1[[1]]*(*(1/k2coeff)*)k2coeff
(Dot@@Table[Integratedx[pars[[i]],0,Infinity],{i,n}]) .
(Exp[rest1[[2]]] * Product[Exp[-k2coeff (Pair[f[[i,1]],f[[i,1]]]-f[[i,2]]^2)*
      pars[[i]] ],{i,n}]);

res = res1 /. Exp[ee_] :>
    (If[$VeryVerbose>=2,Print["Completing the square of ", k, " in the exponent ", ee]];
     cc=CompleteTheSquare[ee,k,Unique["y"]];
     (* The rules substituting the old with the
        new integration momentum and back *)
     squarerule=Solve[Equal@@cc[[2]],
     mom=Union[Cases[cc[[2]],Momentum[k,___],Infinity]][[1]]][[1,1]];
     endrule=cc[[2,1]]->mom;
     Exp[cc[[1]]])

   ];

(* ********************************************************** *)

FeynmanParametrize1[exp_,q_,opt___Rule] :=
Block[{dim,dims,rul,aa,aaa,ee,qfacs,noqfacs,qq,res,par,
efpar,rr,t,cc,ef,co,y,lis,sil,liss,re,wrap,b,qfac,noqfac,ints,res1,dd},
    endrule={};
    qq = q/.Momentum[aaa_,___]:>aaa;
    par = FeynmanParameterNames/.{opt}/.Options[FeynmanParametrize1];

  (* Make sure all momenta have the same dimension *)
  dims = Union[Cases[exp, (Dimension->_)|(Momentum[_,_]),Infinity]];
  If[dims =!= {}, dims = Union[(#[[2]])& /@ dims]];
  Which[
    Length[dims] == 0,
    dim = Sequence[],
    Length[dims] == 1,
    dim = dims[[1]],
    True,
    dim = dims[[1]];
    rul = ((Rule@@#)& /@ Transpose[
    {dims, Table[dim,{Length[dims]}]}]);
    exp = exp //. rul;
  ];

    (* Choose method *)
    Which[
    (Method/.{opt}/.Options[FeynmanParametrize1])===Denominator,
      efpar=fpar,
    (Method/.{opt}/.Options[FeynmanParametrize1])===Exp,
      efpar=epar,
    True,
      efpar=fpar
    ];

    If[$VeryVerbose>=1,Print["Using Method ", (Method/.{opt}/.Options[FeynmanParametrize1])]];

    (* Add extra parameter names if necessary *)
    If[(len=Max[Length[Cases[#,PropagatorDenominator[_?(!FreeQ[#,qq]&)],Infinity]]& /@
         Cases[exp,_FeynAmpDenominator,Infinity]])>
      (len1=Length[par]),
      par=Join[par,Table[Unique["x"],{len-len1}]];
      If[$VeryVerbose>=1,Print["Added extra parameter names ", par]]];

    If[$VeryVerbose>=2,Print["Simplifying expression"]];
    res1 = ScalarProductExpand[FeynAmpDenominatorCombine[exp//.
           (*First flatten out Dot products with Integratedx's*)
            If[(Flatten/.Flatten[{opt}]/.Options[FeynmanParametrize1])===True,
            (b_?(((!FreeQ[#,q])&&FreeQ[#,Integratedx])&))*
            Dot[ints:(Integratedx[_,_,_]..),r_?((!FreeQ[#,q]&&FreeQ[#,Integratedx])&)] :>
            (If[Head[b]===Times,
                     qfac=Select[List@@{b},(!FreeQ[#,q]&)];noqfac=Complement[b,qfac],
                     qfac=b;noqfac=1];
             noqfac*Dot[ints,qfac*r]),
             {}]
          ]];
   If[$VeryVerbose>=2,Print["Finished simplifying expression. ",
                Length[Cases[{res1},
                HighEnergyPhysics`FeynCalc`FeynAmpDenominator`FeynAmpDenominator,
                Infinity,Heads->True]], " FeynAmpDenominator(s) present"]];


    res = res1 /.

    (rr___)*FeynAmpDenominator[aa__] :>

    (

          (* Operate only on denominator fators containing q *)
           qfacs=Select[{aa},((!FreeQ[#,qq])&)];
           noqfacs=Complement[{aa},qfacs];
           qfacs=FeynAmpDenominator@@qfacs/.FeynAmpDenominator[]->1;
           noqfacs=FeynAmpDenominator@@noqfacs/.FeynAmpDenominator[]->1;

If[ (Integrate/.{opt}/.Options[FeynmanParametrize1])=!=True ||
              efpar=!=epar,
   (* No integration *)
   ef=efpar[par][qq][FeynCalcInternal[qfacs]],
   (* Integration *)
   ef=efpar[par][qq][FeynCalcInternal[qfacs*Times@@Cases[{rr},E^(ee_?(!FreeQ[#,qq]&))]]] ]*

   (* wrap is just so we can later absorb into the Dot[Integratedx...] stuff*)
   wrap[If[(Integrate/.{opt}/.Options[FeynmanParametrize1])=!=True ||
              efpar=!=epar,

           (* No integration *)
           ScalarProductExpand[Times[rr]/.If[efpar===fpar,{},squarerule]],

          (* Integration *)
          (*The integration momentum y*)
          y=endrule[[1]];
          (* Get the coefficient on y^2 in the exponential *)
          co=-Coefficient[(ef/._*dd_Dot:>dd)[[-1]]/.E^(ee_)->ee,Pair[y,y]];
          (* Do the y-integrals *)
           If[$VeryVerbose>=1,Print["Expanding and replacing tensor terms with integrated terms, using integration momentum ", endrule[[2]], " and coefficient ", co]];
           Uncontract[Expand[ScalarProductExpand[Times@@Replace[{rr},E^(ee_?(!FreeQ[#,qq]&))->1,1] /.
                    squarerule]],y[[1]],Pair->All]/.

            (HoldPattern[Times[t:(HighEnergyPhysics`FeynCalc`Pair`Pair[
                         HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[_,___],_]..),
                         re__?(FreeQ[#,y]&)]]):>
              Contract[
              lis1=Cases[{t},HighEnergyPhysics`FeynCalc`Pair`Pair[
                             HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[_,___],y]];
              sil=Complement[{t},lis1];
              lis=(#[[1]])&/@lis1;
              cc=Length[lis];
              Which[
                OddQ[cc],0,
                cc==0,(2 Pi)^4/((4 Pi co)^(dim/2)),
                cc==2,(2 Pi)^4/(2 co (4 Pi co)^(dim/2)) Pair@@lis,
                cc==4,(2 Pi)^4/(4 co^2 (4 Pi co)^(dim/2))*
                 Plus@@(Times[Pair@@#[[1]],Pair@@#[[2]]]&/@
                      Union[Sort/@((Sort/@Partition[#,2])&/@Permutations[lis])])]*
                      (Times@@sil)*Times[re]
               ]
             ] *
          (*Remove the integrated out momentum*)
           If[(Integrate/.{opt}/.Options[FeynmanParametrize1])===True &&
               efpar===epar,E^(co Pair[y,y]),1]
       ] * noqfacs

    );

   FeynAmpDenominatorCombine[res/.rr_Dot*wrap[aa_]:>ReplacePart[rr,rr[[-1]]aa,-1]/.
   endrule]/.wrap[rr_]:>rr/.efpar[{__}][_][1]:>1
                ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FeynmanParametrize1 | \n "]];
Null
