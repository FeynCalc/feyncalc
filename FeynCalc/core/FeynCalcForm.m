(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: FeynCalcForm[expr] formats expr in a short form.
             In FeynCalc.m  $PrePrint can be set to
             $PrePrint = FeynCalcForm
*)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`FeynCalcForm`",{"HighEnergyPhysics`FeynCalc`"}];

FCF::uasge=
"FCF is a short form for FeynCalcForm.";

FeynCalcForm::"usage"=
"FeynCalcForm[expr] changes the printed output to a an easy to read
form. Whether the result of FeynCalcForm[expr] is displayed
or not, depends on the setting of $PrePrint.
$PrePrint = FeynCalcForm forces displaying everything
after applying FeynCalcForm. In order to change to the normal
(internal) Mathematica OutputForm, do: ($PrePrint=.).";

(* ------------------------------------------------------------------------ *)


Begin["`Private`"];

FCF = FeynCalcForm;

bra = "(";
ket = ")";

ca           := ca = MakeContext["CA"];
cf           := cf = MakeContext["CF"];
chiralityprojector := chiralityprojector =
                                MakeContext["ChiralityProjector"];
DeltaFunction:=DeltaFunction = MakeContext["DeltaFunction"];
dimension    := dimension     = MakeContext["CoreOptions","Dimension"];
diracgamma   := diracgamma    = MakeContext["DiracGamma"];
diracmatrix  := diracmatrix   = MakeContext["DiracMatrix"];
DiracSigma   := DiracSigma    = MakeContext["DiracSigma"];
diracslash   := diracslash    = MakeContext["DiracSlash"];
diractrace   := diractrace    = MakeContext["DiracTrace"];
DiracGammaT  := DiracGammaT   = MakeContext["DiracGammaT"];
eps          := eps           = MakeContext["Eps"];
Epsilon      := Epsilon       = MakeContext["Epsilon"];
FinalSubstitutions :=FinalSubstitutions = MakeContext["CoreOptions","FinalSubstitutions"];
freeq2       := freeq2        = MakeContext["FreeQ2"];
Gstrong      := Gstrong        = MakeContext["Gstrong"];
propagatordenominator := propagatordenominator =
                                MakeContext["PropagatorDenominator"];
feynampdenominator := feynampdenominator =
                                MakeContext["FeynAmpDenominator"];
fourvector   := fourvector    = MakeContext["FourVector"];
GluonOperator := GluonOperator= MakeContext["Twist2GluonOperator"];
lorentzindex := lorentzindex  = MakeContext["LorentzIndex"];
levicivita   := levicivita    = MakeContext["LeviCivita"];
Lower        := Lower         = MakeContext["Lower"];
metrictensor := metrictensor  = MakeContext["MetricTensor"];
momentum     := momentum      = MakeContext["Momentum"];
NumericalFactor:= NumericalFactor=MakeContext["NumericalFactor"];
OPEDelta     := OPEDelta      = MakeContext["OPEDelta"];
OPEi         := OPEi          = MakeContext["OPEi"];
OPEj         := OPEj          = MakeContext["OPEj"];
OPEk         := OPEk          = MakeContext["OPEk"];
OPEm         := OPEm          = MakeContext["OPEm"];
OPESum       := OPESum        = MakeContext["OPESum"];
pair         := pair          = MakeContext["Pair"];
partial      := partial       = MakeContext["PartialD"];
field        := field         = MakeContext["QuantumField"];
polarization := polarization  = MakeContext["Polarization"];
polarizationvector := polarizationvector =
                                MakeContext["PolarizationVector"];
Power2       := Power2        = MakeContext["Power2"];
PlusDistribution := PlusDistribution =
                                MakeContext["PlusDistribution"];
RHO          := RHO           = MakeContext["RHO"];
RHI          := RHI           = MakeContext["RHI"];
scalarproduct:= scalarproduct = MakeContext["ScalarProduct"];
Sn           := Sn            = MakeContext["Sn"];
spinor       := spinor        = MakeContext["Spinor"];
sundelta     := sundelta      = MakeContext["SUNDelta"];
sund         := sund          = MakeContext["SUND"];
sunF         := sunF          = MakeContext["SUNF"];
sunindex     := sunindex      = MakeContext["SUNIndex"];
sunt         := sunt          = MakeContext["SUNT"];
suntrace     := suntrace      = MakeContext["SUNTrace"];
tf           := tf            = MakeContext["Tf"];
nf           := nf            = MakeContext["Nf"];
Zeta2        := Zeta2         = MakeContext["Zeta2"];
Upper        := Upper         = MakeContext["Upper"];

Options[FeynCalcForm] = {FinalSubstitutions -> {}};

(* for future changes ... *)
cdf = Symbol["CommonDefaultFormatTypes"];

Off[Rule::rhs];
FeynCalcForm[x_,opts___] :=
Block[{re}, Off[Rule::rhs];re = 
If[$Notebooks === True,
   If[$PrePrint === FeynCalcForm,
      If[MemberQ[{TraditionalForm, StandardForm, InputForm},
                  "Output" /. (
                  cdf /.
                  Options[$FrontEnd, "CommonDefaultFormatTypes"])
                ]
         ,
         Unset[$PrePrint]; (*Print["UNSET"]; *)x
         ,
(*i.e., in OutputForm one can have $PrePrint=FeynCalcForm *)
         feynCalcForm[x,opts]
        ],
      x
     ]
   ,
   feynCalcForm[x,opts]
  ];
On[Rule::rhs];
re
];

(* timefixdef : a more physics - like timing function *)
tim[a_, b_] := If[$VersionNumber > 2.2 && $Notebooks===True,
        SequenceForm[StringInsert[ToString[Floor[10 a]],".",-2]," ",b],
                  a b
                 ];

(* due to Dave Withoff ... *)
feynCalcForm[InputForm[f_]]:=InputForm[f];

SetAttributes[feynCalcForm, HoldAll];
SetAttributes[FeynCalcForm, HoldAll];
(*Unprotect[TimeUsed];*)

(*TimeUsed /:*) HoldPattern[feynCalcForm[TimeUsed[]]] :=
                timefix[TimeUsed[]];
(*
Protect[TimeUsed];
*)

timefix[n_]:= Which[ 0.<=n<0.02,      tim[" < 0.02","s"],
                     0.02<=n<9.5,   tim[N[n,2], "s"],
                     9.5<=n<59.5,  tim[N[n,2], "s"],
                     59.5<=n<600,  tim[N[n/60,2], "min"],
                     600<=n<3570,  tim[N[n/60,2], "min"],
                     3569<n<36000, tim[N[n/3600,2], "h"],
                     36000<n,      tim[N[n/3600,4], "h"]
                   ];
sunfuser[a_,b_,c_,___]:=fsunU[a, b, c]/.fsunU->"f";
sumst[x_Plus]:=SequenceForm["(",x,")"];  sumst[y_]:=y;


diracsldi[di_][x__,dimension -> di_] :=
   diracslash[x, dimension -> di];
diracmadi[di_][x__,dimension -> di_] :=
   If[!FreeQ[{x}, Rule], diracmatrix[x],
      diracmatrix[x, dimension -> di]];
diracmadi[di_][x__] :=
   If[!FreeQ[{x}, Rule],diracmatrix[x],
      diracmatrix[x, dimension -> di]];
iDentity[a_,___] := a;
sunident[a_] := a;

didm[x_,y___]:=x;
didl[x_,y___]:=x;
   (*Format[fcdot2[a_,b__]] := Infix[fcdot2[a,b], " ", 320];*)
   (*Not an allowed syntax in mma 4.1. F.Orellana*)
     Format[fcdot2[a_,b__]] := Infix[fcdot2[a,b], " "];
fcdot2[x-y,x-rd];
   Format[fcdot2[a_]] := a;

diF[x_-4]:=StringJoin[ToString[x],"-4"];
diF[x_]:=x;

double[{a___, x_, x_, b___}] := {a,x,x,b};

dea[yy__]     := double[Map[denfa,{yy}]] /. double -> Identity;

denfa[_[Subscripted[x_[s_]],0]] := SequenceForm["(",x[s]^2,")"];

denfa[_[momentum[Subscripted[x_[s_]],___],0]] :=
      SequenceForm["(",x[s]^2,")"];

denfa[_[x_]] := SequenceForm["(",x^2,")"];

denfa[_[x_,0]] := SequenceForm["(",x^2,")"];

denfa[_[x_,y_]] := SequenceForm["(",x^2,"- ",y^2,")"];

feynden[x__]    := 1 / fcdot2 @@ ( dea @@ {x} );
ditr[x_,___]    := "tr"[x];

fdprop[a__]   := 1 / denfa[dudu[a]];
compind[a_]     := If[Head[a] === Symbol,
                   StringJoin[ToString[a],"*"], a "*"];
myscriptsbox[x_] := x;

SetAttributes[sub, HoldAll];
sub[a_String, b_] := If[CheckContext[a], MakeContext[a] :> b, {}];

CC[x_]     := CheckContext[x];
CC[x_,y__] := CheckContext[x] && CC[y];

(* change as a side effect the ordering Attribute of Plus and Times,
   but reinstall it again at the end.
*)
  epsd[a___, (b_/;(Head[b] ===lorentzindex) ||
                  (Head[b] === momentum)
                 )[c_,di_], d___] :=
      Subscripted["eps"[di//diF]][a,b[c,di],d];
  epsd[a__] := "eps"[a];

(* display only one dimension (for readability) *)
ni[di_]:=ToString[di];
ni[di__]:=ToString[{di}[[1]]];

diracslm[a_] := diracslash[a];
diracslm[a_, rul___Rule] := diracslash[a, rul];
diracslm[a_, b__, rul_Rule] := SequenceForm @@
                                Map[diracslash[#, rul]&, {a, b}];
diracslm[a_, b__] := SequenceForm @@ Map[diracslash[#]&, {a, b}];

feynCalcForm[x_,opt___Rule]:=Block[{xxxx = Evaluate[x], subs},
                 subs = FinalSubstitutions /. {opt} /. Options[FeynCalcForm];
                  xxxx = xxxx /. subs;
                  xxxx = xxxx/.(n_Real Second)->timefix[n];
Global`XX=xxxx;
                  xxxx = (xxxx/.
         DOT:>fcdot /.
         sub["SUNN", "N"]/.
         If[CC["SUNTrace"],  suntrace :> "tr", {}]  /.
         If[CC["LeviCivita"],  levicivita[lv__] :> epsd[lv], {}] /.
         If[CC["Eps"],  eps[vl__] :> epsd[vl], {}] /.
         If[CC["MetricTensor"], metrictensor[v_, w_, ___Rule] :> "g"[v, w], {}
           ] /.
(*
         If[CC["FourVector"], fourvector[Subscripted[p_[s_]], mu_] :>
          (SequenceForm@@Flatten[ {sumst[p[s]],"[",mu,"]"}]), {}
           ] /.
*)
        If[ CC["ScalarProduct"], scalarproduct[ v_,v_ ] :> v^2, {}] /.
        If[ CC["ScalarProduct"], scalarproduct[v_ w_] :>
               (SequenceForm@@Flatten[ {v//sumst ,{"."},w//sumst} ]),{}
          ] /.
(*
        If[CC["PolarizationVecvtor"],
           polarizationvector[ka_, mu_, ___] :> "ep"[ka, mu],
           {}
          ] /.
*)
        If[CC["Polarization"],
           {pair[momentum[polarization[v_,-I,sun___]],
                  lorentzindex[w_] ] :> ("ep(*)"[v,w,sun] ),
            pair[ momentum[polarization[v_,-I,___]] ,
                   lorentzindex[w_] ]:> "ep(*)"[v, w] ,
            pair[ momentum[polarization[v_,I,sun___]],
                   lorentzindex[w_] ]:>
             ("ep"[v,w,sun] (*/.sunindex:>iDentity*))
           } , {}
          ] /.
        If[CC["Pair"],
           {pair[ lorentzindex[v_],lorentzindex[w_] ] :> "g"[v, w],
            pair[ lorentzindex[v_,di_],lorentzindex[w_,di_] ] :>
               (Subscripted["g"[di//diF]][v, w])
           }, {}
          ] /.
        If[CC["Pair"],
           pair[ momentum[v_,___],momentum[v_,___] ] :> v^2,
           {}
          ] /.
       If[CC["Pair"],
          pair[ momentum[v_,___],momentum[w_,___] ] :>
               (SequenceForm@@Flatten[ {v//sumst ,{"."},w//sumst} ]),
           {}
         ] /.
       If[CC["Pair"],
        pair[ momentum[v_,di_Symbol-4],momentum[w_,di_Symbol-4] ] :>
                 Subscripted[
                  (SequenceForm@@Flatten[{"(",v//sumst ,{"."},w//sumst,")"}]
                  )[di//diF]] ,
           {}
         ] /.
       If[CC["Pair"],
        pair[ lorentzindex[w_,___],momentum[Subscripted[v_[s_]],___ ]]:>
             (SequenceForm@@Flatten[ {sumst[v[s]],"[",w,"]"} ]),
          {}
         ] /.
       If[CC["Pair"],
        pair[ lorentzindex[w_, ___],momentum[v_, ___] ] :>
             (SequenceForm@@Flatten[ {sumst[v],"[",w,"]"} ]),
          {}
         ] /.
        If[CC["Polarization"],
           {polarization[ka_,-I,___]:>"ep(*)"[ka],
             polarization[ka_,I,___]:>"ep"[ka]
           }, {}
          ] /.
         (*If[CC["ComplexIndex"],
            {MakeContext["ComplexIndex"][i__] :> compind[i]},
            {}
           ] /.*)
         If[CC["ChiralityProjector"],
            chiralityprojector[+1] :> diracgamma[6],
            {}
           ] /.
         If[CC["ChiralityProjector"],
            chiralityprojector[-1] :> diracgamma[7],
            {}
           ] /.
         If[CC["OPEDelta"], MakeContext["OPEDelta"] :> "De",
            {}
           ]/.
         If[CC["DiracMatrix"],
            diracmatrix[6] :> diracgamma[6], {}
           ] /.
         If[CC["DiracGamma"],
            {diracgamma[lorentzindex[v_]]        :>
             diracmatrix[v, dimension -> 4],
            diracgamma[lorentzindex[v_,di_],di_] :>
             diracmatrix[v, dimension -> 4],
            diracgamma[momentum[v_]]             :>
             diracslash[v, dimension -> 4],
            diracgamma[momentum[v_,di_],di_]     :>
             diracslash[v, dimension -> 4]
            },
            {}
           ]/.
         If[CC["DiracGammaT"],
            DiracGammaT[aa_,___]:> "gat"[aa],
            {}
           ] /.
         If[CC["DiracGamma"],
            {diracgamma[5] :> "ga[5]",
             diracgamma[6] :> "ga[6]",
             diracgamma[7] :> "ga[7]"
            }, {}
           ] /.
         If[CC["DiracMatrix"],
            If[(dimension /. Options[diracmatrix]) =!= 4,
               diracmatrix[v_] :>
                 diracmadi[(dimension /. Options[diracmatrix])][v],
               {}
              ],{}
           ]/.
         If[CC["DiracMatrix"],
            diracmatrix[dmv__, dimension -> 4] :> diracmatrix[dmv],
            {}
           ] /.
         If[CC["DiracSlash"],
            diracslash[vsv__, dimension -> 4] :> diracslash[vsv],
            {}
           ] /.
         If[CC["DiracSigma"],
            {
            DiracSigma[_[a_], _[b_]]:> "Sigma"[a,b],
            DiracSigma[_[a_, b_]]   :> "Sigma"[a,b]
            },{}
           ] /.
         If[CC["DiracMatrix"],
            {
             diracmatrix[v__, dimension -> di_] :>
              Subscripted["ga"[v][di]],
             diracmatrix[v__]  :>
              "ga"[v]
            },{}
           ] /.
         If[CC["DiracGamma"],
            If[(dimension /. Options[diracslash]) =!= 4,
               diracslash[v__]:>
                diracsldid[(dimension /. Options[diracslash])][v]/.
                 diracsldid :> diracsldi,
               {}
              ],
            {}
           ]/.
         If[CC["DiracGamma"], diracslash[aa_] :> diracslm[aa], {} ] /.
         If[CC["DiracGamma"],
             {diracslash[v_, dimension -> di_] :>
               Subscripted[ToString["gs"][di//diF] ][v] ,
              diracslash[Subscripted[v_[s_]]] :> "gs"[v[s]] ,
             diracslash[v_]:> ToString["gs"[v]]
             }
            , {}
           ] /.
         If[CC["Spinor"],
            {
             fcdot[spinor[-p_, 0, ___], a__] :>
               DOT["v"[-p/.momentum->iDentity], a],
             fcdot[spinor[p_, 0, ___], a__]  :>
               DOT["u"[p/.momentum->iDentity], a],
             fcdot[a__,spinor[-p_, 0, ___] ] :>
               DOT["v"[-p/.momentum->iDentity], a],
             fcdot[a__, spinor[p_, 0, ___]]  :>
               DOT[a, "u"[p/.momentum->iDentity]]
            }, {}
           ]/.
         If[CC["Spinor"],
            {
             fcdot[spinor[-p_, mas_, _], a__] :>
               DOT["v"[-p/.momentum->iDentity,mas], a],
             fcdot[spinor[p_, mas_, _], a__]  :>
               DOT["u"[p/.momentum->iDentity,mas], a],
             fcdot[a__,spinor[-p_, mas_, _] ] :>
               DOT[a, "v"[-p/.momentum->iDentity,mas]],
             fcdot[a__, spinor[p_, mas_, _]]  :>
               DOT[a, "u"[p/.momentum->iDentity,mas]]
            }, {}
           ]/.
         If[CC["Spinor"],
            {
            spinor[-p_,0,___] :> "v"[p /. momentum -> iDentity],
            spinor[p_,0,___]  :> "u"[p /. momentum -> iDentity],
            spinor[-p_,ma_,_] :> "v"[p /. momentum -> iDentity,ma],
            spinor[p_,ma_,_]  :> "u"[p /. momentum -> iDentity,ma]
            }, {}
           ]/.
         If[CC["SUNDelta"],
            sundelta[a_, b_] :> "d"[a, b],
            {}
           ] /.
         If[CC["SUND"],
            sund[a_, b_, c_] :> "d"[a, b, c],
            {}
           ] /.
         If[CC["SUNF"],
            sunF[a_, b_, c_] :>  "f"[a, b, c],
            {}
           ] /.
         If[CC["SUNT"],
            {
            sunt[a_] :>  "T"[a],
            sunt[a_,b__] :> (fcdot2 @@ Map["T"[#]&,{a, b}])
            }, {}
           ] /.
         If[CC["OPEm"], OPEm :> "m", {} ] /.
         If[CC["OPEi"], OPEi :> "i", {} ] /.
         If[CC["OPEj"], OPEj :> "j", {} ] /.
         If[CC["OPEl"], OPEl :> "l", {} ] /.
         If[CC["OPEk"], OPEk :> "k", {} ] /.
         If[CC["QuantumField"],
            {
             field[a_] :> a,
             field[a_, lori___momentum, suni___sunindex][p___] :>
               "Q"[a, lori,suni][p],
             field[a_, lori___lorentzindex, suni___sunindex][p___] :>
               "Q"[a, lori,suni][p],
             field[a_, lori___lorentzindex, suni___sunindex] :>
               "Q"[a, lori,suni],
             field[a_, lori___momentum, suni___sunindex] :>
               "Q"[a, lori,suni],
             field[pa:partial[_].., a_, lori___lorentzindex,
                                          suni___sunindex][p___] :>
             "Q"[pa, a, lori, suni][p],
             field[pa:partial[_].., a_, lori___momentum,
                                          suni___sunindex][p___] :>
             "Q"[pa, a, lori, suni][p],
             field[pa:partial[_].., a_, lori___lorentzindex,
                                          suni___sunindex]  :>
             ("Q"[pa, a, lori, suni]/.partial -> "P"),
             field[pa:partial[_].., a_, lori___momentum,
                                          suni___sunindex] :>

             ("Q"[pa, a, lori, suni]/.partial -> "P")
            }, {}
            ]/.
         If[CC["QuantumField"],
            { partial[a_] :> "P"[a]
            }, {}
           ]/.
        fcdot:>fcdot2/. (*fcdot2 -> DOT /.*)
         If[CC["DiracTrace"], diractrace[v__] :> ditr[v], {}] /.
         lorentzindex[vv__] :> didl[vv]  /.
         If[CC["QuantumField"],
            field[v__] :> "Q"[v],
            {}
           ] /.
         If[CC["PartialD"],
            partial[v_] :> "P"[v],
            {}
           ] /.
         If[CC["PlusDistribution"],
            PlusDistribution[v_] :> plusdi[v],
            {}
           ] /.
         If[CC["SUNIndex"],
            sunindex[i_] :> sunident[i],
            {}
           ]/.
         If[CC["OPESum"], OPESum :> "OPESum", {} ]/.
         If[CC["DeltaFunction"],  DeltaFunction :> "delta", {}
           ] /.
         If[CC["Twist2GluonOperator"], GluonOperator:>"GO",{}
           ] /.
         If[CC["SunIndex"], sunident :> sunindex, {} ] /.
         If[CC["FeynAmpDenominator"],
            feynampdenominator[v__] :> feynden[v], {}
           ] /.
         If[CC["PropagatorDenominator"],
            propagatordenominator[v__] :> fdprop[v], {}
           ] /.
         If[CC["Lower"], Lower[v_,___] :> v, {}] /.
         If[CC["Upper"], Upper[v_,___] :> v, {}] /.
         If[CC["Momentum"], momentum[v__] :> didm[v], {}]  /.
         lorentzindex[v__] :> didl[v]  /.
         {didm :> momentum, didl :> lorentzindex}
       );
         xxxx];

plusdi[a_] := Subscripted[SequenceForm["(",a,")"][" + "]];


End[]; EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FeynCalcForm | \n "]];
Null
