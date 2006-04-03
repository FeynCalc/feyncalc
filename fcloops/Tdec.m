(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Tdec *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fcloops`Tdec`",
             "HighEnergyPhysics`FeynCalc`"];

Tdec::"usage" = "Tdec[{q,mu}, {p}];
Tdec[{{qi, mu}, {qj, nu}, ...}, {p1, p2, ...}] or
Tdec[exp, {{qi, mu}, {qj, nu}, ...}, {p1, p2, ...}] 
calculates the tensorial decomposition formulas.
The more common ones are saved in TIDL.";

NumberOfMetricTensors::"usage"=
"NumberOfMetricTensors is an option of Tdec.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   
MakeContext[
Cases2,
Collect2,
Contract, 
ChangeDimension,
Dimension,
EpsContract, 
EpsEvaluate,
ExpandScalarProduct,
Expanding, 
Factoring,
Factor2,
FeynCalcForm,
FeynCalcExternal,
FreeQ2,
KK,
(*
IsolateNames,
*)
LorentzIndex,  
Momentum,
Pair, 
PairContract,
SelectFree, SelectNotFree,Solve2,Solve3
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
veqli, seqli, scqli, solu, xy, ce},

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
If[$VeryVerbose > 0,
    Print["symms"]; Print[Length[ccli]];
  ];

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


If[$VeryVerbose > 0, Print["contracting"]];
eqli = Table[EpsEvaluate[ExpandScalarProduct[
If[$VeryVerbose > 1, Print["il = ",il]];
                 (tt proli[[il]]) /. Pair->PairContract /. 
                                     PairContract -> Pair
                                            ]]
             , {il, Length[proli]}
            ];
If[$VeryVerbose > 0, Print["Length of eqli = ",Length[eqli]]];
If[$VeryVerbose > 0, Print["solving ", Length[ccli]]];
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

If[Length[neqli] > 50, Share[]];
solu = Solve3[neqli, ccli, Factoring -> factor];
If[$VeryVerbose > 0, Print["solve3 done ",MemoryInUse[]];
   Print["SOLVE3 Bytecount", byby= ByteCount[solu]];
  ];

If[listlabel =!= True,
   solu = solu /. FeynCalcExternal[ChangeDimension[Map[Reverse, seqli],4]];
  ];

solu = solu /. Dispatch[Map[Reverse, scqli]];

nttt = Collect[tt[[2]], Map[First, scqli]];
If[fce, nttt = FeynCalcExternal[ChangeDimension[nttt,4]]];
(*
tt = tt[[2]] /. solu;
*)
tt = nttt /. Dispatch[solu];
If[$VeryVerbose > 0, Print["after solu substitution ", 
         N[MemoryInUse[]/10^6,3], " MB ; time used ", 
           TimeUsed[]//FeynCalcForm]
  ];
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
  {FeynCalcExternal[ChangeDimension[Map[Reverse, seqli],4]], tt}
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

solvesymms[xx_] := Block[{s1,s2,s3,res,var,du,nx=xx,ff,vv},
If[Head[nx] === List,
   nx = Sum[Expand[nx[[iji]] du[iji],LorentzIndex],{iji,Length[nx]}]
  ];
If[$VeryVerbose > 0, Print["collecting in solvesymms ", Length[nx]]];
nx = Map[(SelectFree[#, {du, LorentzIndex}] ff[
            SelectNotFree[#, {du, LorentzIndex}] ]
         )&, nx];
vv = Cases2[nx, ff];
If[$VeryVerbose > 0, Print["collecting in solvesymms ",
      Length[vv], "terms" ]
  ];

titi = TimeUsed[];

s2 = Table[If[$VeryVerbose>0, If[IntegerQ[ij/1000],Print[ij]]];
           D[nx,vv[[ij]]], {ij,Length[vv]}
          ];

(*
s1 = Collect[nx, vv, Factor2] /. ff -> Identity;
*)
If[$VeryVerbose > 0, Print["time for collect in solvesymms = ",
    TimeUsed[] - titi]
  ];
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

Tdec[{{q_,mu_}},{p_},opt___Rule] := Block[{n},
  n = Dimension /. {opt} /. Options[Tdec];
   (Pair[LorentzIndex[mu, n], Momentum[p, n]]*
     Pair[Momentum[p, n], Momentum[q, n]])/
   Pair[Momentum[p, n], Momentum[p, n]]
                                         ];

Tdec[{{q_,mu_},{q_,nu_}},{p_},opt___Rule] := Block[{n},
  n = Dimension /. {opt} /. Options[Tdec];
 (Pair[LorentzIndex[mu, n], LorentzIndex[nu, n]]*
      (Pair[Momentum[p, n], Momentum[q, n]]^2 - 
        Pair[Momentum[p, n], Momentum[p, n]]*
         Pair[Momentum[q, n], Momentum[q, n]]))/
    ((1 - n)*Pair[Momentum[p, n], Momentum[p, n]]) - 
   (Pair[LorentzIndex[mu, n], Momentum[p, n]]*
      Pair[LorentzIndex[nu, n], Momentum[p, n]]*
      (n*Pair[Momentum[p, n], Momentum[q, n]]^2 - 
        Pair[Momentum[p, n], Momentum[p, n]]*
         Pair[Momentum[q, n], Momentum[q, n]]))/
    ((1 - n)*Pair[Momentum[p, n], Momentum[p, n]]^2)]


(* 1 *)
Tdec[{qi_,al_},{p_,k_},opt___Rule] := 
  Tdec[{{qi,al}},{p,k},opt] /; Head[qi]=!=List;

Tdec[{{qi_,al_}},{p_,k_},opt___Rule] := Block[{n},
 n = Dimension /. {opt} /. Options[Tdec];
  (Pair[LorentzIndex[al, n], Momentum[p, n]]*
       Pair[Momentum[k, n], Momentum[p, n]]*
       Pair[Momentum[k, n], Momentum[qi, n]] - 
      Pair[LorentzIndex[al, n], Momentum[p, n]]*
       Pair[Momentum[k, n], Momentum[k, n]]*
       Pair[Momentum[p, n], Momentum[qi, n]])/ 
(Pair[Momentum[k, n], Momentum[p, n]]^2 - 
      Pair[Momentum[k, n], Momentum[k, n]]*
       Pair[Momentum[p, n], Momentum[p, n]]) + 
   (-(Pair[LorentzIndex[al, n], Momentum[k, n]]*
         Pair[Momentum[k, n], Momentum[qi, n]]*
         Pair[Momentum[p, n], Momentum[p, n]]) + 
      Pair[LorentzIndex[al, n], Momentum[k, n]]*
       Pair[Momentum[k, n], Momentum[p, n]]*
       Pair[Momentum[p, n], Momentum[qi, n]])/
    (Pair[Momentum[k, n], Momentum[p, n]]^2 - 
      Pair[Momentum[k, n], Momentum[k, n]]*
       Pair[Momentum[p, n], Momentum[p, n]])] /;
   Head[qi]=!=List;
 (* 2 *)

Tdec[{{qi_,al_},{qj_,be_}},{p_,z_},opt___Rule] := 
Block[
{dD,a,b,c,d,e,f,g,i,j,k,l,m,n},
dD =  Dimension /. {opt} /. Options[Tdec];
a = Pair[LorentzIndex[al, dD], LorentzIndex[be, dD]];
b = Pair[LorentzIndex[al, dD], Momentum[z, dD]];
c = Pair[LorentzIndex[al, dD], Momentum[p, dD]];
d = Pair[LorentzIndex[be, dD], Momentum[z, dD]];
e = Pair[LorentzIndex[be, dD], Momentum[p, dD]];
f = Pair[Momentum[z, dD], Momentum[z, dD]];
g = Pair[Momentum[z, dD], Momentum[p, dD]];
h = Pair[Momentum[z, dD], Momentum[qi, dD]];
i = Pair[Momentum[z, dD], Momentum[qj, dD]];
k = Pair[Momentum[p, dD], Momentum[p, dD]];
l = Pair[Momentum[p, dD], Momentum[qi, dD]];
m = Pair[Momentum[p, dD], Momentum[qj, dD]];
n = Pair[Momentum[qi, dD], Momentum[qj, dD]];
(a*(h*i*k - g*i*l - g*h*m + f*l*m + g^2*n - f*k*n))/
((-2 + dD)*(g^2 - f*k)) +
(c*e*(-2*g^2*h*i + dD*g^2*h*i + f*h*i*k + f*g*i*l - dD*f*g*i*l + f*g*h*m -
dD*f*g*h*m - f^2*l*m + dD*f^2*l*m + f*g^2*n - f^2*k*n))/
((-2 + dD)*(g^2 - f*k)^2) + (c*d*
(g*h*i*k - dD*g*h*i*k + g^2*i*l - 2*f*i*k*l + dD*f*i*k*l - g^2*h*m +
dD*g^2*h*m + f*g*l*m - dD*f*g*l*m - g^3*n + f*g*k*n))/
((-2 + dD)*(g^2 - f*k)^2) + (b*e*
(g*h*i*k - dD*g*h*i*k - g^2*i*l + dD*g^2*i*l + g^2*h*m - 2*f*h*k*m +
dD*f*h*k*m + f*g*l*m - dD*f*g*l*m - g^3*n + f*g*k*n))/
((-2 + dD)*(g^2 - f*k)^2) + (b*d*
(-(h*i*k^2) + dD*h*i*k^2 + g*i*k*l - dD*g*i*k*l + g*h*k*m - 
dD*g*h*k*m -
2*g^2*l*m + dD*g^2*l*m + f*k*l*m + g^2*k*n - f*k^2*n))/
((-2 + dD)*(g^2 - f*k)^2)
] /; Head[qi]=!=List;

Tdec[{{qi_, mu_}, {qj_, nu_}, {qk_, rho_}}, {p1_, p2_},opt___Rule
    ]:=
Block[{
z,a,b,c,d,e,f,g,h,i,k,l,m,n,o,p,q,r,s,t,u,v },
z=Dimension/.{opt}/.Options[Tdec];
a=Pair[LorentzIndex[mu,z],LorentzIndex[nu,z]];
b=Pair[LorentzIndex[mu,z],LorentzIndex[rho,z]];
c=Pair[LorentzIndex[mu,z],Momentum[p1,z]];
d=Pair[LorentzIndex[mu,z],Momentum[p2,z]];
e=Pair[LorentzIndex[nu,z],LorentzIndex[rho,z]];
f=Pair[LorentzIndex[nu,z],Momentum[p1,z]];
g=Pair[LorentzIndex[nu,z],Momentum[p2,z]];
h=Pair[LorentzIndex[rho,z],Momentum[p1,z]];
i=Pair[LorentzIndex[rho,z],Momentum[p2,z]];
k=Pair[Momentum[p1,z],Momentum[p1,z]];
l=Pair[Momentum[p1,z],Momentum[p2,z]];
m=Pair[Momentum[p1,z],Momentum[qi,z]];
n=Pair[Momentum[p1,z],Momentum[qj,z]];
o=Pair[Momentum[p1,z],Momentum[qk,z]];
p=Pair[Momentum[p2,z],Momentum[p2,z]];
q=Pair[Momentum[p2,z],Momentum[qi,z]];
r=Pair[Momentum[p2,z],Momentum[qj,z]];
s=Pair[Momentum[p2,z],Momentum[qk,z]];
t=Pair[Momentum[qi,z],Momentum[qj,z]];
u=Pair[Momentum[qi,z],Momentum[qk,z]];
v=Pair[Momentum[qj,z],Momentum[qk,z]];
(a*i*(l*o-k*s)*(m*n*p-l*n*q-l*m*r+k*q*r+l^2*t-k*p*t))/
((-2+z)*(l^2-k*p)^2)+(a*h*(-(o*p)+l*s)*
(m*n*p-l*n*q-l*m*r+k*q*r+l^2*t-k*p*t))/
((-2+z)*(l^2-k*p)^2)+(b*g*(l*n-k*r)*
(m*o*p-l*o*q-l*m*s+k*q*s+l^2*u-k*p*u))/
((-2+z)*(l^2-k*p)^2)+(b*f*(-(n*p)+l*r)*
(m*o*p-l*o*q-l*m*s+k*q*s+l^2*u-k*p*u))/
((-2+z)*(l^2-k*p)^2)+(d*e*(l*m-k*q)*
(n*o*p-l*o*r-l*n*s+k*r*s+l^2*v-k*p*v))/
((-2+z)*(l^2-k*p)^2)+(c*e*(-(m*p)+l*q)*
(n*o*p-l*o*r-l*n*s+k*r*s+l^2*v-k*p*v))/
((-2+z)*(l^2-k*p)^2)+(d*g*i*
(-2*l^3*m*n*o+z*l^3*m*n*o+3*k*l*m*n*o*p-z*k*l^2*n*o*q-
k^2*n*o*p*q-z*k*l^2*m*o*r-k^2*m*o*p*r+k^2*l*o*q*r+
z*k^2*l*o*q*r-z*k*l^2*m*n*s-k^2*m*n*p*s+k^2*l*n*q*s+
z*k^2*l*n*q*s+k^2*l*m*r*s+z*k^2*l*m*r*s-k^3*q*r*s-
z*k^3*q*r*s+k*l^3*o*t-k^2*l*o*p*t-k^2*l^2*s*t+k^3*p*s*t+
k*l^3*n*u-k^2*l*n*p*u-k^2*l^2*r*u+k^3*p*r*u+k*l^3*m*v-
k^2*l*m*p*v-k^2*l^2*q*v+k^3*p*q*v))/((-2+z)*(l^2-k*p)^3)+
(d*g*h*(-(z*l^2*m*n*o*p)-k*m*n*o*p^2+l^3*n*o*q+z*k*l*n*o*p*q+
l^3*m*o*r+z*k*l*m*o*p*r-2*k*l^2*o*q*r+k^2*o*p*q*r-
z*k^2*o*p*q*r+z*l^3*m*n*s+k*l*m*n*p*s-k*l^2*n*q*s-
z*k*l^2*n*q*s-k*l^2*m*r*s-z*k*l^2*m*r*s+k^2*l*q*r*s+
z*k^2*l*q*r*s-k*l^2*o*p*t+k^2*o*p^2*t+k*l^3*s*t-
k^2*l*p*s*t-l^4*n*u+k*l^2*n*p*u+k*l^3*r*u-k^2*l*p*r*u-
l^4*m*v+k*l^2*m*p*v+k*l^3*q*v-k^2*l*p*q*v))/
((-2+z)*(l^2-k*p)^3)+(d*f*i*
(-(z*l^2*m*n*o*p)-k*m*n*o*p^2+l^3*n*o*q+z*k*l*n*o*p*q+
z*l^3*m*o*r+k*l*m*o*p*r-k*l^2*o*q*r-z*k*l^2*o*q*r+
l^3*m*n*s+z*k*l*m*n*p*s-2*k*l^2*n*q*s+k^2*n*p*q*s-
z*k^2*n*p*q*s-k*l^2*m*r*s-z*k*l^2*m*r*s+k^2*l*q*r*s+
z*k^2*l*q*r*s-l^4*o*t+k*l^2*o*p*t+k*l^3*s*t-k^2*l*p*s*t-
k*l^2*n*p*u+k^2*n*p^2*u+k*l^3*r*u-k^2*l*p*r*u-l^4*m*v+
k*l^2*m*p*v+k*l^3*q*v-k^2*l*p*q*v))/((-2+z)*(l^2-k*p)^3)+
(c*g*i*(-(z*l^2*m*n*o*p)-k*m*n*o*p^2+z*l^3*n*o*q+k*l*n*o*p*q+
l^3*m*o*r+z*k*l*m*o*p*r-k*l^2*o*q*r-z*k*l^2*o*q*r+
l^3*m*n*s+z*k*l*m*n*p*s-k*l^2*n*q*s-z*k*l^2*n*q*s-
2*k*l^2*m*r*s+k^2*m*p*r*s-z*k^2*m*p*r*s+k^2*l*q*r*s+
z*k^2*l*q*r*s-l^4*o*t+k*l^2*o*p*t+k*l^3*s*t-k^2*l*p*s*t-
l^4*n*u+k*l^2*n*p*u+k*l^3*r*u-k^2*l*p*r*u-k*l^2*m*p*v+
k^2*m*p^2*v+k*l^3*q*v-k^2*l*p*q*v))/((-2+z)*(l^2-k*p)^3)+
(c*f*i*(l*m*n*o*p^2+z*l*m*n*o*p^2-l^2*n*o*p*q-z*l^2*n*o*p*q-
l^2*m*o*p*r-z*l^2*m*o*p*r+z*l^3*o*q*r+k*l*o*p*q*r-
2*l^2*m*n*p*s+k*m*n*p^2*s-z*k*m*n*p^2*s+l^3*n*q*s+
z*k*l*n*p*q*s+l^3*m*r*s+z*k*l*m*p*r*s-z*k*l^2*q*r*s-
k^2*p*q*r*s+l^3*o*p*t-k*l*o*p^2*t-k*l^2*p*s*t+k^2*p^2*s*t+
l^3*n*p*u-k*l*n*p^2*u-l^4*r*u+k*l^2*p*r*u+l^3*m*p*v-
k*l*m*p^2*v-l^4*q*v+k*l^2*p*q*v))/((-2+z)*(l^2-k*p)^3)+
(c*g*h*(l*m*n*o*p^2+z*l*m*n*o*p^2-l^2*n*o*p*q-z*l^2*n*o*p*q-
2*l^2*m*o*p*r+k*m*o*p^2*r-z*k*m*o*p^2*r+l^3*o*q*r+
z*k*l*o*p*q*r-l^2*m*n*p*s-z*l^2*m*n*p*s+z*l^3*n*q*s+
k*l*n*p*q*s+l^3*m*r*s+z*k*l*m*p*r*s-z*k*l^2*q*r*s-
k^2*p*q*r*s+l^3*o*p*t-k*l*o*p^2*t-l^4*s*t+k*l^2*p*s*t+
l^3*n*p*u-k*l*n*p^2*u-k*l^2*p*r*u+k^2*p^2*r*u+l^3*m*p*v-
k*l*m*p^2*v-l^4*q*v+k*l^2*p*q*v))/((-2+z)*(l^2-k*p)^3)+
(d*f*h*(l*m*n*o*p^2+z*l*m*n*o*p^2-2*l^2*n*o*p*q+k*n*o*p^2*q-
z*k*n*o*p^2*q-l^2*m*o*p*r-z*l^2*m*o*p*r+l^3*o*q*r+
z*k*l*o*p*q*r-l^2*m*n*p*s-z*l^2*m*n*p*s+l^3*n*q*s+
z*k*l*n*p*q*s+z*l^3*m*r*s+k*l*m*p*r*s-z*k*l^2*q*r*s-
k^2*p*q*r*s+l^3*o*p*t-k*l*o*p^2*t-l^4*s*t+k*l^2*p*s*t+
l^3*n*p*u-k*l*n*p^2*u-l^4*r*u+k*l^2*p*r*u+l^3*m*p*v-
k*l*m*p^2*v-k*l^2*p*q*v+k^2*p^2*q*v))/((-2+z)*(l^2-k*p)^3)+
(c*f*h*(-(m*n*o*p^3)-z*m*n*o*p^3+l*n*o*p^2*q+z*l*n*o*p^2*q+
l*m*o*p^2*r+z*l*m*o*p^2*r-z*l^2*o*p*q*r-k*o*p^2*q*r+
l*m*n*p^2*s+z*l*m*n*p^2*s-z*l^2*n*p*q*s-k*n*p^2*q*s-
z*l^2*m*p*r*s-k*m*p^2*r*s-2*l^3*q*r*s+z*l^3*q*r*s+
3*k*l*p*q*r*s-l^2*o*p^2*t+k*o*p^3*t+l^3*p*s*t-k*l*p^2*s*t-
l^2*n*p^2*u+k*n*p^3*u+l^3*p*r*u-k*l*p^2*r*u-l^2*m*p^2*v+
k*m*p^3*v+l^3*p*q*v-k*l*p^2*q*v))/((-2+z)*(l^2-k*p)^3)
] /; Head[qi]=!=List;

mt[de_][a_,b_] := mt[de][a,b] = Pair[LorentzIndex[a,de], LorentzIndex[b,de]];
fv[de_][p_,m_] := fv[de][p,m] = Pair[LorentzIndex[m,de], Momentum[p,de]];

(* fourth rank *)
symtens[{},{m1_,m2_,m3_,m4_}, d_] :=
( mt[d][m1,m2] mt[d][m3,m4] + mt[d][m1,m3] mt[d][m2,m4] + 
  mt[d][m1,m4] mt[d][m2,m3]);

symtens[{{p_,m1_},{p_,m2_}},{m3_,m4_}, d_] :=
fv[d][p,m1] fv[d][p,m2] mt[d][m3,m4] +
fv[d][p,m1] fv[d][p,m3] mt[d][m2,m4] + fv[d][p,m1] fv[d][p,m4] mt[d][m2,m3];

symtens[{{p_,m1_},{p_,m2_},{p_,m3_},{p_,m4_}}, {}, d_] :=
fv[d][p,m1] fv[d][p,m2] fv[d][p,m3] fv[d][p,m4];

(* sixth rank *)
symtens[{},{m1_,m2_,m3_,m4_,m5_,m6_}, d_] :=
( mt[d][m1,m2]*mt[d][m3,m4]*mt[d][m5,m6] + mt[d][m1,m2]*mt[d][m3,m5]*
  mt[d][m4,m6] + mt[d][m1,m2]*mt[d][m3,m6]*mt[d][m4,m5] + 
  mt[d][m1,m3]*mt[d][m2,m4]*
  mt[d][m5,m6] + mt[d][m1,m3]*mt[d][m2,m5]*mt[d][m4,m6] + 
  mt[d][m1,m3]*mt[d][m2,m6]*
  mt[d][m4,m5] + mt[d][m1,m4]*mt[d][m2,m3]*mt[d][m5,m6] +
  mt[d][m1,m4]*mt[d][m2,m5]*
  mt[d][m3,m6] + mt[d][m1,m4]*mt[d][m2,m6]*mt[d][m3,m5] + 
  mt[d][m1,m5]*mt[d][m2,m3]*
  mt[d][m4,m6] + mt[d][m1,m5]*mt[d][m2,m4]*mt[d][m3,m6] + 
  mt[d][m1,m5]*mt[d][m2,m6]*
  mt[d][m3,m4] + mt[d][m1,m6]*mt[d][m2,m3]*mt[d][m4,m5] + 
  mt[d][m1,m6]*mt[d][m2,m4]*
  mt[d][m3,m5] + mt[d][m1,m6]*mt[d][m2,m5]*mt[d][m3,m4] 
);
(* sixth rank *)
symtens[{{p_,m1_},{p_,m2_}},{m3_,m4_,m5_,m6_}, d_] :=
Block[{mm = {m1, m2, m3, m4, m5, m6}},
 Sum[fv[d][p,mm[[i]]] fv[d][p,mm[[j]]] *
        symtens[{}, SelectFree[mm,{mm[[i]],mm[[j]]}],d],
     {i, 1, 5}, {j, i+1, 6}
    ]];

symtens[{{p_,m1_},{p_,m2_}, {p_,m3_}, {p_,m4_}}, 
         {m5_,m6_}, d_
       ] := Block[{mm = {m1, m2, m3, m4, m5, m6}},
 Sum[mt[d][mm[[i]], mm[[j]]] *
     Map[fv[d][p,#]&, SelectFree[mm,{mm[[i]],mm[[j]]}]],
     {i, 1, 5}, {j, i+1, 6}
    ]            ];
    
(* eigth rank *)
symtens[{},{m1_,m2_,m3_,m4_,m5_,m6_,m7_,m8_}, d_] :=
(
(
  x1*x18*x21*x23 + x13*x2*x21*x23 + x1*x17*x22*x23 + x12*x2*x22*x23 + 
   x1*x18*x20*x24 + x13*x2*x20*x24 + x1*x16*x22*x24 + x11*x2*x22*x24 + 
   x1*x17*x20*x25 + x12*x2*x20*x25 + x1*x16*x21*x25 + x11*x2*x21*x25 + 
   x1*x18*x19*x26 + x13*x19*x2*x26 + x1*x15*x22*x26 + x10*x2*x22*x26 + 
   x1*x14*x25*x26 + x1*x17*x19*x27 + x12*x19*x2*x27 + x1*x15*x21*x27 + 
   x10*x2*x21*x27 + x1*x14*x24*x27 + x1*x16*x19*x28 + x11*x19*x2*x28 + 
   x1*x15*x20*x28 + x10*x2*x20*x28 + x1*x14*x23*x28 + x13*x17*x23*x3 + 
   x12*x18*x23*x3 + x13*x16*x24*x3 + x11*x18*x24*x3 + x12*x16*x25*x3 + 
   x11*x17*x25*x3 + x13*x15*x26*x3 + x10*x18*x26*x3 + x12*x15*x27*x3 + 
   x10*x17*x27*x3 + x11*x15*x28*x3 + x10*x16*x28*x3 + x13*x17*x20*x4 + 
   x12*x18*x20*x4 + x13*x16*x21*x4 + x11*x18*x21*x4 + x12*x16*x22*x4 + 
   x11*x17*x22*x4 + x13*x14*x26*x4 + x12*x14*x27*x4 + x11*x14*x28*x4 + 
   x13*x17*x19*x5 + x12*x18*x19*x5 + x13*x15*x21*x5 + x10*x18*x21*x5 + 
   x12*x15*x22*x5 + x10*x17*x22*x5 + x13*x14*x24*x5 + x12*x14*x25*x5 + 
   x10*x14*x28*x5 + x13*x16*x19*x6 + x11*x18*x19*x6 + x13*x15*x20*x6 + 
   x10*x18*x20*x6 + x11*x15*x22*x6 + x10*x16*x22*x6 + x13*x14*x23*x6 + 
   x11*x14*x25*x6 + x10*x14*x27*x6 + x12*x16*x19*x7 + x11*x17*x19*x7 + 
   x12*x15*x20*x7 + x10*x17*x20*x7 + x11*x15*x21*x7 + x10*x16*x21*x7 + 
   x12*x14*x23*x7 + x11*x14*x24*x7 + x10*x14*x26*x7 + x25*x26*x3*x8 + 
   x24*x27*x3*x8 + x23*x28*x3*x8 + x22*x26*x4*x8 + x21*x27*x4*x8 + 
   x20*x28*x4*x8 + x22*x24*x5*x8 + x21*x25*x5*x8 + x19*x28*x5*x8 + 
   x22*x23*x6*x8 + x20*x25*x6*x8 + x19*x27*x6*x8 + x21*x23*x7*x8 + 
   x20*x24*x7*x8 + x19*x26*x7*x8 + x2*x25*x26*x9 + x2*x24*x27*x9 + 
   x2*x23*x28*x9 + x18*x26*x4*x9 + x17*x27*x4*x9 + x16*x28*x4*x9 + 
   x18*x24*x5*x9 + x17*x25*x5*x9 + x15*x28*x5*x9 + x18*x23*x6*x9 + 
   x16*x25*x6*x9 + x15*x27*x6*x9 + x17*x23*x7*x9 + x16*x24*x7*x9 + 
   x15*x26*x7*x9
) /. {x1 -> mt[d][m1, m2], x2 -> mt[d][m1, m3], x3 -> mt[d][m1, m4], 
   x4 -> mt[d][m1, m5], x5 -> mt[d][m1, m6], x6 -> mt[d][m1, m7], 
   x7 -> mt[d][m1, m8], x8 -> mt[d][m2, m3], x9 -> mt[d][m2, m4], 
   x10 -> mt[d][m2, m5], x11 -> mt[d][m2, m6], x12 -> mt[d][m2, m7], 
   x13 -> mt[d][m2, m8], x14 -> mt[d][m3, m4], x15 -> mt[d][m3, m5], 
   x16 -> mt[d][m3, m6], x17 -> mt[d][m3, m7], x18 -> mt[d][m3, m8], 
   x19 -> mt[d][m4, m5], x20 -> mt[d][m4, m6], x21 -> mt[d][m4, m7], 
   x22 -> mt[d][m4, m8], x23 -> mt[d][m5, m6], x24 -> mt[d][m5, m7], 
   x25 -> mt[d][m5, m8], x26 -> mt[d][m6, m7], x27 -> mt[d][m6, m8], 
   x28 -> mt[d][m7, m8]}
);

symtens[{{p_,m1_},{p_,m2_}},{m3_,m4_,m5_,m6_,m7_,m8_}, d_] :=
Block[{mm = {m1, m2, m3, m4, m5, m6, m7, m8}},
 Sum[fv[d][p,mm[[i]]] fv[d][p,mm[[j]]] *
        symtens[{}, SelectFree[mm,{mm[[i]],mm[[j]]}],d],
     {i, 1, 7}, {j, i+1, 8}
    ]];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Tdec | \n "]];
Null
