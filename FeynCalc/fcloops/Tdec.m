(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Tdec *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fcloops`Tdec`",
             {"HighEnergyPhysics`FeynCalc`"}];

Tdec::"usage" = "Tdec[{q,mu}, {p}];
Tdec[{{qi, mu}, {qj, nu}, ...}, {p1, p2, ...}] or
Tdec[exp, {{qi, mu}, {qj, nu}, ...}, {p1, p2, ...}]
calculates the tensorial decomposition formulas.
The more common ones are saved in TIDL.";

NumberOfMetricTensors::"usage"=
"NumberOfMetricTensors is an option of Tdec.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

Dimension = MakeContext["CoreOptions","Dimension"];
EpsContract = MakeContext["CoreOptions","EpsContract"];
Factoring = MakeContext["CoreOptions","Factoring"];
LorentzIndex = MakeContext["CoreObjects","LorentzIndex"];
Momentum = MakeContext["CoreObjects","Momentum"];
Pair = MakeContext["CoreObjects","Pair"];

MakeContext[
	Cases2,
	Collect2,
	Contract,
	EpsEvaluate,
	ExpandScalarProduct,
	Factor2,
	FeynCalcForm,
	FeynCalcExternal,
	FreeQ2,
	PairContract,
	SelectFree,
	SelectNotFree,
	Solve3
];

Options[Tdec] = {Dimension -> D, Factoring -> Factor2,
FeynCalcExternal -> True,
(*
                 IsolateNames -> KK,
*)
                 List -> True,
                 NumberOfMetricTensors -> Infinity};

cc[a__] := 0 /; OddQ[Count[{a}, 0, Infinity]];
(* ccfix is NECESSARY, since otherwise multiloop decompositions
   are wrong
*)
Clear[ccfix, cfix, pcfix];
ccfix[exp_] := cfix[Expand[exp]];
ccf[xx__,1] := ccf[xx];
(* aa is always a sum of terms with coefficient 1 *)
cfix[aa_Plus] := Map[cfix, aa];
cfix[cc[a__] pairs_] := pairs *
  Join[(cc[a]/.{0,__} -> nix[]) /. nix -> Sequence,
       (ccf @@ (Flatten[{pairs /. Pair-> pcfix} /. Times -> List]
              ) /. liS -> List
       ) /. ccf -> cc
      ];
pcfix[___,Momentum[__],___] := 1;
pcfix[LorentzIndex[a__], LorentzIndex[b__]] :=
 Sort[liS[0,0,LorentzIndex[a], LorentzIndex[b]]];


gfix[ w_Times] := 0 /; OddQ[Count[w, pe[0,_]]];
gfix[ w_Times] := 0 /; Count[w, pe[0,_]] > 2 (
                  NumberOfMetricTensors /. Options[Tdec]);
gfix[0] = 0;
gfix[ff_] := ff /; FreeQ[ff, pe[0,_]];

HoldPattern[
gfix[pe[0, mu_] pe[0, nu_] *
     cc[c:{0, _}..]
    ]  ] := (Pair[mu, nu] cc[{0, mu}, {0,nu}]
            ) /; Length[{c}] === 2;

HoldPattern[
gfix[pe[0, mu_] pe[0, nu_] *
     cc[v___, {0, mu_}, w___, {0, nu_}, z___] f_.
    ]  ] := (f Pair[mu, nu] cc[v, {0,mu}, w, {0,nu}, z]
         ) /; FreeQ[f, pe[0, _]] (* && FreeQ[{v, w, z}, 0]*);

ccjoin /: ccjoin[a__] ccjoin[b__] := ccjoin[a, b];

ccj[xx_] := xx /. (cc[aa__]^_Integer) :>
            cc[aa] /. cc -> ccjoin /. ccjoin -> cc;

HoldPattern[
gfix[pe[0, mu_] *
     cc[v___, {0, mu_}, w___, {0, nu_}, z___] f_Times
    ]  ] :=  (
FixedPoint[ccj,
 Sum[gfix[pe[0,mu] f[[i]] cc[v,{0,mu},w,{0,nu},z]] gfix[
            Drop[f,{i,i}] cc[v,{0,mu},w,{0,nu},z]         ],
     {i, Length[f]}
    ], 100]
             ) /; (!FreeQ[f, pe[0, _]]) && (!FreeQ[{v, w, z}, 0]);


(*
HoldPattern[gfix[pe[0,mu_] cc[ac___ /;FreeQ[{ac}, {0,_}],
                          c:{0,_}..,
                          nc___ /;FreeQ[{nc}, {0,_}]
                         ] f_Times]
       ] :=
FixedPoint[ccj,
 Sum[gfix[pe[0,mu] f[[i]] cc[ac,c,nc]] gfix[Drop[f,{i,i}] cc[ac,c,nc]],
     {i, Length[f]}
    ] /. cc -> ccjoin /. ccjoin -> cc, 100];

HoldPattern[gfix[pe[0,mu_] cc[ac___ /;FreeQ[{ac}, {0,_}],
                          c:{0,_}..,
                          nc___ /;FreeQ[{nc}, {0,_}],
                          d:{0,_}..
                         ] f_Times]
       ] :=
FixedPoint[ccj,
 Sum[gfix[pe[0,mu] f[[i]] cc[ac,c,nc,d]] gfix[Drop[f,{i,i}
                                             ] cc[ac,c,nc,d]],
     {i, Length[f]}
    ] /. cc -> ccjoin /. ccjoin -> cc, 100];
*)

(* li = {{q1,mu}, {q2,nu}, ...} pli = {p1,p2, ...} *)

(* Tdecdef *)
Tdec[exp_:1, {a_/;Head[a] =!=List, b_/;Head[b]=!=List}, pli_,
     opt___Rule] := Tdec[exp, {{a,b}}, Flatten[{pli}]];

Tdec[exp_:1, li_List, pli_List, opt___Rule] := Block[
{tt,fv,  factor, dim, pe, proj, proli, nccli, ccli, ctt,
 nullccli, kli, eqli, neqli,  nttt,listlabel, fce,
veqli, seqli, scqli, solu, xy, ce, byby, symms, sy},

dim = Dimension /. {opt} /. Options[Tdec];
listlabel = List /. {opt} /. Options[Tdec];
fce = FeynCalcExternal /. {opt} /. Options[Tdec];
factor = Factoring /. {opt} /. Options[Tdec];
kli = Union[Map[First,li]];
fv[x_,y_] := Pair[Momentum[x,dim], LorentzIndex[y, dim]];
pe[j_ /; j>0, muu_] := Pair[Momentum[pli[[j]], dim], muu];
tt = Product[fv@@li[[i]],
             {i,Length[li]}
            ] ==
FixedPoint[ccj,
ccfix[
     (Sum @@ Join[
           { gfixx[ Product[pe[j[ij], LorentzIndex[li[[ij,2]], dim]],
                            {ij, Length[li]}
                           ] *
             Apply[cc, Table[{j[iji], LorentzIndex[li[[iji,2]], dim]},
                             {iji, Length[li]}
                            ]
                  ]
                  ]
            }, Array[{j[#],0,Length[pli]}&, Length[li]]
                 ]
     ) /. gfixx -> gfix
     ],100];

If[!FreeQ[tt, gfix], Print["MIST"];
   tt>>"ttdec.s"; Quit[];
  ];
tt = tt /. cc -> ccf /. ccf -> cc;

(* build in later maybe more symmetries among the cc's *)
ccli = Cases2[tt, cc];
If[Length[pli] === 0 && Length[kli] === 1,
   For[ic = 2, ic <= Length[ccli], ic++,
       Apply[Set, {ccli[[ic]], ccli//First}]
      ];
   ccli = Union[ccli]
  ];
(* DOes not work somehow ..
(* if exp is =!=1 then some cc's might vanish *)
If[exp === 1,
   nullccli = {}; nccli = ccli,
   ctt = Map[EpsEvaluate[ExpandScalarProduct[
             Contract[# exp, EpsContract -> False]]]&, tt
            ];
   nccli  = Cases2[ctt, cc];
   nullccli = SelectFree[ccli, nccli];
If[$VeryVerbose >0  && Length[nullccli]>0,
   Print["nullccli = ", nullccli] ]
  ];

tt = tt /. Map[# -> 0&, nullccli];
*)

proj[] = 1;

(*
proj[{0, m_LorentzIndex}, aa___, {0, n_LorentzIndex}, bb___] :=
 Pair[m, n] proj[aa, bb];
*)
proj[{0, 0, m_, n_}, bb___] :=
 (PairContract[LorentzIndex[m,dim], LorentzIndex[n,dim]] proj[bb]
 ) /; (Head[m] =!= LorentzIndex) && (Head[n] =!= LorentzIndex);

proj[{j_ /; j > 0, r_}, jh___] :=
 (pe[j, LorentzIndex[r,dim]] proj[jh]) /; Head[r] =!= LorentzIndex;

newcc[ii__] := cc[ii] /. LorentzIndex[w_,_] :> w;
tt = tt /. cc -> newcc;
symms = getsymmetries[tt];
    FCPrint[1,"symms"]; FCPrint[1,Length[ccli]];


If[symms =!=  False,
   sy = solvesymms[tt[[2]] - (tt[[2]]/.symms)];
  ,
   sy = {}
  ];

ccli = ccli /. cc -> newcc;
ccli = Union[ccli//.sy];
tt = tt //. sy;
proli = ccli /. cc -> proj;

If[Length[pli] === 0 && Length[kli]===1,
   proli = {First[proli]}
  ];


FCPrint[1,"contracting"];
eqli = Table[EpsEvaluate[ExpandScalarProduct[
FCPrint[1,"il = ",il];
                 (tt proli[[il]]) /. Pair->PairContract /.
                                     PairContract -> Pair
                                            ]]
             , {il, Length[proli]}
            ];
FCPrint[1,"Length of eqli = ",Length[eqli]];
FCPrint[1,"solving ", Length[ccli]];
xy[ii_] := ToExpression["X" <> ToString[ii]];
ce[ii_] := ToExpression["cC" <> ToString[ii]];
veqli = Union[Join @@ Map[Variables, Flatten[eqli /. Equal -> List]]];
veqli = SelectFree[veqli, ccli];
seqli = Table[veqli[[ij]] -> xy[ij], {ij, Length[veqli]}];
scqli = Table[ ccli[[ji]] -> ce[ji], {ji, Length[ccli]}];
neqli = eqli /. seqli /. scqli;
ccli = ccli /. scqli;

(*
neqli = Collect2[neqli, ccli];
*)

solu = Solve3[neqli, ccli, Factoring -> factor];
	FCPrint[1,"solve3 done ",MemoryInUse[]];
   FCPrint[1,"SOLVE3 Bytecount", byby= ByteCount[solu]];


If[listlabel =!= True,
   solu = solu /. FeynCalcExternal[Map[Reverse, seqli]];
  ];

solu = solu /. Dispatch[Map[Reverse, scqli]];

nttt = Collect[tt[[2]], Map[First, scqli]];
If[fce, nttt = FeynCalcExternal[nttt]];
(*
tt = tt[[2]] /. solu;
*)
tt = nttt /. Dispatch[solu];
FCPrint[1,"after solu substitution ",
         N[MemoryInUse[]/10^6,3], " MB ; time used ",
           TimeUsed[]//FeynCalcForm];

If[exp =!= 1, tt = Contract[exp tt, EpsContract -> False]];
(*
qli = Map[First,li];
plq[yy__]  := If[FreeQ2[{yy}, qli], Plus[yy], Collect2[Plus[yy],qli]];
If[Length[li]<5,
   If[Length[li]>1,
      tt = Map[Cancel,Map[#/.Plus->plq&, tt]],
      tt = Map[Cancel,Collect2[tt, LorentzIndex]]
     ];
  ];
*)
If[listlabel === True,
  {FeynCalcExternal[Map[Reverse, seqli]], tt}
  ,
   tt
  ]
];

getsymmetries[aa_ /; Head[aa] =!= Times==_] := {};

getsymmetries[aa_Times == _] :=
 Block[{t1,t2,t3,t4},
       t1 = Cases2[aa, LorentzIndex];
       t2 = Union[Map[Sort,Map[Take[#,2]&,Permutations[t1]]]];
       t3 = Map[{Apply[RuleDelayed,{#[[1]], #[[2]]}],
                 Apply[RuleDelayed,{#[[2]], #[[1]]}]}&, t2];
       t4 = {};
       For[i = 1, i <= Length[t3], i++,
           If[ aa === (aa /. t3[[i]]),
               AppendTo[t4, t3[[i]]]
             ]
          ];
       t4];

solvesymms[xx_] := Block[{s1,s2,s3,res,var,du,nx=xx,ff,vv,titi},
If[Head[nx] === List,
   nx = Sum[Expand[nx[[iji]] du[iji],LorentzIndex],{iji,Length[nx]}]
  ];
FCPrint[1,"collecting in solvesymms ", Length[nx]];
nx = Map[(SelectFree[#, {du, LorentzIndex}] ff[
            SelectNotFree[#, {du, LorentzIndex}] ]
         )&, nx];
vv = Cases2[nx, ff];
FCPrint[1,"collecting in solvesymms ",
      Length[vv], "terms" ];


titi = TimeUsed[];

s2 = Table[If[IntegerQ[ij/1000],FCPrint[1,ij]];
           D[nx,vv[[ij]]], {ij,Length[vv]}
          ];

(*
s1 = Collect[nx, vv, Factor2] /. ff -> Identity;
*)
FCPrint[1,"time for collect in solvesymms = ",
    TimeUsed[] - titi];

(*
If[Head[s1] =!= Plus,
   s2 = SelectFree[s1, {du, Pair}],
   s2 = Map[SelectFree[#, {du, Pair}]&, List@@s1]
  ];
*)
 ij = 0;
res = {};
While[(nx =!= 0)&& (ij < Length[s2]), ij ++;
      var = Variables[s2[[ij]]];
      If[var =!= {},
      s3 = Solve[s2[[ij]]==0, var//First,
                 VerifySolutions -> False]//First//First;
      If[!MemberQ[res,s3], AppendTo[res,s3];
         nx = nx /. s3
        ];
        ];
     ];res];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Tdec | \n "]];
Null
