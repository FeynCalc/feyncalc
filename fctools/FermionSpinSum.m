(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* :Title: FermionSpinSum *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: do the trace-formation (i.e. fermionic spin-sums) *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`FermionSpinSum`",
             "HighEnergyPhysics`FeynCalc`"];

FermionSpinSum::usage=
"FermionSpinSum[x] constructs the Traces out of squared ampliudes.";

SpinorCollect::usage=
"SpinorCollect is an option for FermionSpinSum. If set to False the
 argument of FermionSpinSum has to be already collected w.r.t. Spinor.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   SetAttributes[FermionSpinSum, ReadProtected];

Collect2      = MakeContext["Collect2"];  
Contract      = MakeContext["Contract"];  
dot           = MakeContext["DOT"];
DiracGamma    = MakeContext["DiracGamma"];
DiracTrace    = MakeContext["DiracTrace"];  
Eps           = MakeContext["Eps"];  
Expanding     = MakeContext["Expanding"];  
ExtraFactor   = MakeContext["ExtraFactor"];
Factor2       = MakeContext["Factor2"];
FreeQ2        = MakeContext["FreeQ2"];  
Isolate       = MakeContext["Isolate"];
IsolateNames  = MakeContext["IsolateNames"];
IsolateSplit  = MakeContext["IsolateSplit"];
LorentzIndex  = MakeContext["LorentzIndex"];  
Momentum      = MakeContext["Momentum"];  
Spinor        = MakeContext["Spinor"];  
SpinPolarizationSum =
                MakeContext["SpinPolarizationSum"];
SUNSimplify   = MakeContext["SUNSimplify"];  
DiracOrder    = MakeContext["DiracOrder"];  
DiracSimplify = MakeContext["DiracSimplify"];  
DotSimplify   = MakeContext["DotSimplify"];
Tr            = MakeContext["Tr"];

FRH = FixedPoint[ReleaseHold, #]&;
dotLin[x_] := DotSimplify[x, Expanding -> False];

(* FermionSpinSumdef *)
trsimp[a_. DiracGamma[_,___]] := 0 /; FreeQ[a, DiracGamma];
(*
trsimp[dot[d__]] := Tr[dot[d] ] /; Length[{d}] < 4;
*)
trsimp[dot[d__]] := DiracTrace[dot[d] ] /; Length[{d}] < 4;
Options[FermionSpinSum] = {SpinPolarizationSum -> Identity,
                           SpinorCollect -> False,
                           ExtraFactor -> 1};
FermionSpinSum[x_,ops___] := Block[
{spsf,spir,spir2,dirtri, nx,nnx, is=1, sufu,exf,
 plsp,lis, cOL, spinorCollect},
            nx = x;
            If[!FreeQ[x, Spinor], 
               spsf = SpinPolarizationSum /. {ops} /. 
                        Options[FermionSpinSum];
               exf = ExtraFactor/. {ops} /. Options[FermionSpinSum];
               spinorCollect= SpinorCollect/. {ops} /. 
                        Options[FermionSpinSum];

(* FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF *)
(* fermion polarization sums *)
   spir = { (* ubar u , vbar v *)
            Spinor[s_. Momentum[pe1_], arg__ ]^2 :>
           (dotLin[spsf[ (DiracGamma[Momentum[pe1]] + s First[{arg}]) ] ]),
           (Spinor[s_. Momentum[pe1_], arg__] . dots___ ) *
           (dots2___ . Spinor[s_. Momentum[pe1_], arg__ ] )  :>
            dots2 . dotLin[spsf[(DiracGamma[Momentum[pe1]] + 
                                 s First[{arg}])]] . dots
          };

   spir2 = Spinor[s_. Momentum[pe_], arg__] . dots___ .
           Spinor[s_. Momentum[pe_], arg__] :> DiracTrace[(
           dotLin[spsf[(DiracGamma[Momentum[pe]] + 
                        s First[{arg}])]] . dots)        ] /; 
           FreeQ[{dots}, Spinor] ;

   dirtri = DiracTrace[n_. a_dot] DiracTrace[m_. b_dot] :>
             DiracTrace[ DiracTrace[n a] m b] /; Length[a] <= Length[b] &&
               Head[n] =!= dot && Head[m] =!= dot;
(* FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF *)
uNi=Unique[System`C];
kK = Unique[System`C];
(*
uNi=Unique[cCC];
kK = Unique[ckC];
*)
pliui[xxx__] := pliui[xxx] = If[Length[{xxx}] < 5,
                                Isolate[Factor2[Plus[xxx]]],
                                Isolate[Plus[xxx], IsolateNames -> kK,
                                IsolateSplit -> 444I]
                               ];
$isoFlag = True;

cOL[xy_] := Block[{temP = xy, nodot = 0, ntemP},
print3["entering cOL"];
temp0 = temP;
                   If[Head[temP] === Plus,
                      nodot = Select[temP, FreeQ[#, dot]&];
                      temP = temP - nodot;
                      nodot = nodot/. {a_ DiracGamma[5] :> 0 /; 
                               FreeQ[a, DiracGamma]};
                     ];
(*
If[$isoFlag,
                   If[Head[temP] === Plus,
                      temP = Map[#/.Plus -> pliui&, temP],
                      temP = temP /. Plus -> pliui;
                     ];
  ];
*)
temp1 = temP;
                   temP = Collect2[temP, DiracGamma, Factoring -> False 
                                  ];
print3["collected in cOL"];
(*
                   If[Head[temP] === Plus,
(* this step by step factorization is ESSENTIAL!!!!! *)
(* because this bloody Mma is not able to it directly ...*)
                      ntemP = 0;  lntemP = Length[temP];
                      For[ijn = 1, ijn <= Length[temP], ijn++,
                          print3["ijn = ",ijn,"(", lntemP,")"];
If[$isoFlag,
                          ntemP = ntemP + Factor2[FRH[Factor2[temP[[ijn]]]]]
           ,              ntemP = ntemP + Factor2[temP[[ijn]]]
  ];
            

                         ];
                      temP = ntemP ,
If[$isoFlag,
                      temP = Factor2[Factor2[temP]//FRH],
                      temP = Factor2[temP]
  ]
                     ];
*)
print3["exiting cOL"];
                   temP + nodot
                 ];

dirtracesep[xy_] := If[Head[xy] =!= Times, DiracTrace[xy//cOL],
    SUNSimplify[Select[xy, FreeQ2[#, {DiracGamma, LorentzIndex, Eps}]&]] * 
       DiracTrace[Select[xy,!FreeQ2[#, 
                             {DiracGamma, LorentzIndex, Eps}]&]//cOL]
                      ];

sufu[xyx_] := Block[{tsuf,spif,mulEx,mulEx2,epSimp,doT, xx=xyx, memm},
 print2[is++, "out of", lis, " 
         Mem = [",memm = N[MemoryInUse[]/10^6,3],"]"];
If[(memm > $MemoryAvailable) &&IntegerQ[is/10], 
   print2["sharing "];Share[]; print2[" done"]];
                       spif = Select[xx, !FreeQ[#, Spinor]&];
                       tsuf = xx / spif;
(*
epSimp[xxx_] := If[FreeQ[xxx, Eps], xxx, DiracSimplify[xxx]];
*)
epSimp[xxx_] := DiracSimplify[DiracOrder[xxx] /. dot -> doT /.
    {doT[a__, DiracGamma[5]] :> 0 /; Length[{a}] < 4,
     doT[DiracGamma[5]] :> 0,
     doT[a__DiracGamma] :> 0 /; FreeQ2[{a}, {DiracGamma[5], DiracGamma[6], 
              DiracGamma[7]}] && OddQ[Length[{a}]]
    } /. doT -> dot, Expanding -> False    ];
                        
If[$VersionNumber > 2.2,
HoldPattern[mulEx[mul_. DiracTrace[xy_]]] := 
  If[!FreeQ[(exf tsuf), LorentzIndex], 
                            dirtracesep[DiracSimplify[
     Contract[mul xy tsuf, exf], Expanding -> False]//epSimp],
     dirtracesep[DiracSimplify[ mul exf tsuf xy,Expanding -> False]//epSimp]
    ],

HoldPattern[mulEx[mul_. DiracTrace[xy_]]] := 
  If[!FreeQ[(exf tsuf), LorentzIndex], 
                            dirtracesep[DiracSimplify[
     Contract[mul xy tsuf, exf], Expanding -> False]//epSimp],
     dirtracesep[DiracSimplify[ mul exf tsuf xy,Expanding -> False]//epSimp]
    ]
  ];

mulEx2[ xy_ ] := mul tsuf exf xy;

(mulEx[(((spif)//.spir//.spir2//.dirtri) /. DiracTrace->trsimp/.
              trsimp->DiracTrace /. $MU->uNi )
      ] /.mulEx->mulEx2 
)                  ]; (* endofsufu *)

(*
 nx = Expand[nx, Spinor];
*)
plsphold = Unique[System`C];
plsp[xyx__] := If[FreeQ[{xyx}, Spinor], plsphold[xyx], Plus[xyx]];

If[spinorCollect === True,
print2["collectinsufu"];
 nx = Collect2[nx /. Plus -> plsp, Spinor, Factoring -> False
              ] /. plsphold -> Plus;
print2["collectinsufudone"];
  ];

onx = nx;
               If[!FreeQ[ nx, $MU], nx = nx /. $MU->Unique[System`C]];
  If[Head[nx] === Plus,  
     lis = Length[nx];
     nnx = 0;
     For[iin = 1, iin <= lis , iin++, 
         nnx = nnx + sufu[nx[[iin]]];
        ];
     nx = nnx,
     lis = 1; nx = sufu[nx]
    ];

(* in case somthing went wrong .. *)
If[nx =!= 0 && FreeQ[nx, DiracTrace], Print[MIST];Dialog[]; nx = x exf];
              ] (* endIfFreeQ[x, Spinor]*);
(*
If[!FreeQ2[exf, {LorentzIndex, Eps}],  
    nx = Contract[ nx exf ], nx = nx exf ];
*)
nx/.mul->1];
           
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FermionSpinSum | \n "]];
Null
