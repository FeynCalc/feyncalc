(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FermionSpinSum												*)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2014 Rolf Mertig
   Copyright (C) 1997-2014 Frederik Orellana
   Copyright (C) 2014 Vladyslav Shtabovenko
*)

(* :Summary:  Do the trace-formation (i.e. fermionic spin-sums) *)

(* ------------------------------------------------------------------------ *)


BeginPackage["HighEnergyPhysics`fctools`FermionSpinSum`",{"HighEnergyPhysics`FeynCalc`"}];

FermionSpinSum::"usage" =
"FermionSpinSum[x] constructs Traces out of squared ampliudes in x.";

SpinorCollect::"usage" =
"SpinorCollect is an option for FermionSpinSum. If set to False the
 argument of FermionSpinSum has to be already collected w.r.t. Spinor.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

Collect2      = MakeContext["Collect2"];
Contract      = MakeContext["Contract"];
DiracGamma    = MakeContext["CoreObjects","DiracGamma"];
DiracTrace    = MakeContext["DiracTrace"];
Eps           = MakeContext["CoreObjects","Eps"];
Expanding     = MakeContext["CoreOptions","Expanding"];
ExtraFactor   = MakeContext["CoreOptions","ExtraFactor"];
Factor2       = MakeContext["Factor2"];
FreeQ2        = MakeContext["FreeQ2"];
Isolate       = MakeContext["Isolate"];
IsolateNames  = MakeContext["CoreOptions","IsolateNames"];
IsolateSplit  = MakeContext["IsolateSplit"];
LorentzIndex  = MakeContext["CoreObjects","LorentzIndex"];
Momentum      = MakeContext["CoreObjects","Momentum"];
Spinor        = MakeContext["CoreObjects","Spinor"];
SpinPolarizationSum =
                MakeContext["CoreOptions","SpinPolarizationSum"];
SUNSimplify   = MakeContext["SUNSimplify"];
DiracOrder    = MakeContext["DiracOrder"];
DiracSimplify = MakeContext["DiracSimplify"];
DotSimplify   = MakeContext["DotSimplify"];
TR            = MakeContext["TR"];

MakeContext [FCPrint]

(*FRH = FixedPoint[ReleaseHold, #]&;*)
trsimp[a_. DiracGamma[_,___]] := 0 /; FreeQ[a, DiracGamma];
trsimp[DOT[expr__]] := DiracTrace[DOT[expr] ] /; Length[{expr}] < 4;

Options[FermionSpinSum] = {SpinPolarizationSum -> Identity,
                           SpinorCollect -> False,
                           ExtraFactor -> 1};

(*if the expression contains spinors, apply the Dirac equation*)

cOL[xy_] :=
    Block[ {temP = xy, nodot = 0, ntemP},
        FCPrint[3,"entering cOL"];
        If[ Head[temP] === Plus,
            nodot = Select[temP, FreeQ[#, DOT]&];
            temP = temP - nodot;
            nodot = nodot/. {a_ DiracGamma[5] :> 0 /;
                     FreeQ[a, DiracGamma]};
        ];
        temP = Collect2[temP, DiracGamma, Factoring -> False];
        FCPrint[3,"collected in cOL"];
        FCPrint[3,"exiting cOL"];
        temP + nodot
    ];

dirtracesep[xy_] :=
    If[ Head[xy] =!= Times,
        DiracTrace[xy//cOL],
        SUNSimplify[Select[xy, FreeQ2[#, {DiracGamma, LorentzIndex, Eps}]&]] *
           DiracTrace[Select[xy,!FreeQ2[#,
                                 {DiracGamma, LorentzIndex, Eps}]&]//cOL]
    ];


epSimp[expr_] :=
    DiracSimplify[DiracOrder[expr] /. DOT -> doT /.
    {doT[a__, DiracGamma[5]] :> 0 /; Length[{a}] < 4,
    doT[DiracGamma[5]] :> 0,
    doT[a__DiracGamma] :> 0 /; FreeQ2[{a}, {DiracGamma[5], DiracGamma[6],
    DiracGamma[7]}] && OddQ[Length[{a}]]
    } /. doT -> DOT, Expanding -> False    ];


FermionSpinSum[expr_Plus, opts:OptionsPattern[]]:= Map[FermionSpinSum[#,opts]&,expr];
FermionSpinSum[expr_List, opts:OptionsPattern[]]:= Map[FermionSpinSum[#,opts]&,expr];
FermionSpinSum[expr_, OptionsPattern[]]:= (OptionValue[ExtraFactor] expr )/; FreeQ[expr,Spinor];



FermionSpinSum[expr_, opts:OptionsPattern[]] :=
    Block[ {spinPolarizationSum,spinorCollect,extraFactor,
        spir,spir2,dirtri, nx, sufu },

        nx = expr;

	    (* Parse options and warn about unrecognized options *)
        Catch[
            Check[
                {spinPolarizationSum, extraFactor, spinorCollect} =
                    OptionValue[{SpinPolarizationSum,ExtraFactor,SpinorCollect}],
                FCPrint[0,"The above error occured in ",HoldComplete[FermionSpinSum[expr,opts]]];
                Throw["Invalid options: " <> ToString[FilterRules[Flatten[{opts},1], Except[Options[FermionSpinSum]]]],"fcFermionSpinSumInvalidOptions" ],
                OptionValue::nodef
            ],
            "fcFermionSpinSumInvalidOptions"];

		(* ----------------------------------------------------------------------------- *)
		(* fermion polarization sums *)
            spir = { (* ubar u , vbar v *)
                     Spinor[s_. Momentum[pe1_], arg__ ]^2 :>
                    (DotSimplify[spinPolarizationSum[ (DiracGamma[Momentum[pe1]] + s First[{arg}]) ], Expanding -> False]),
                    (Spinor[s_. Momentum[pe1_], arg__] . dots___ ) *
                    (dots2___ . Spinor[s_. Momentum[pe1_], arg__ ] )  :>
                     dots2 . DotSimplify[spinPolarizationSum[(DiracGamma[Momentum[pe1]] +
                                          s First[{arg}])],Expanding->False] . dots
                   };
            spir2 = Spinor[s_. Momentum[pe_], arg__] . dots___ .
                    Spinor[s_. Momentum[pe_], arg__] :> DiracTrace[(
                    DotSimplify[spinPolarizationSum[(DiracGamma[Momentum[pe]] +
                                 s First[{arg}])], Expanding -> False] . dots)        ] /;
                    FreeQ[{dots}, Spinor];
            dirtri = DiracTrace[n_. DOT[a1_,a2__]] DiracTrace[m_. DOT[b1_,b2__]] :>
                      DiracTrace[ DiracTrace[n DOT[a1,a2]] m DOT[b1,b2]] /;
                        Length[DOT[a1,a2]] <= Length[DOT[b1,b2]] &&
                        Head[n] =!= DOT && Head[m] =!= DOT;
		(* ----------------------------------------------------------------------------- *)
            uNi = Unique[System`C];
            plsphold = Unique[System`C];

            sufu[xyx_] :=
                Block[ {tsuf, spif, mulEx, mulEx2, epSimp, doT, xx = xyx},
                    spif = Select[xx, !FreeQ[#, Spinor]&];
                    tsuf = xx / spif;

                    HoldPattern[mulEx[mul_. DiracTrace[xy_]]] :=
                            If[ !FreeQ[(extraFactor tsuf), LorentzIndex],
                                dirtracesep[DiracSimplify[
                                Contract[mul xy tsuf, extraFactor], Expanding -> False]//epSimp],
                                dirtracesep[DiracSimplify[ mul extraFactor tsuf xy,Expanding -> False]//epSimp]
                            ];

                    mulEx2[xy_] := mul tsuf extraFactor xy;
                    (mulEx[(((spif)//.spir//.spir2//.dirtri) /. DiracTrace->trsimp/.
                                  trsimp->DiracTrace /. $MU->uNi )
                          ] /.mulEx->mulEx2
                    )
                ]; (* endofsufu *)

			(* Entry point of the function *)

            If[ spinorCollect === True,
                FCPrint[2,"Collecting terms w.r.t spinors in   ", nx];
                nx = Collect2[nx /. {Plus[xyx__] :>
                    If[ FreeQ[{xyx}, Spinor],
                    plsphold[xyx],
                    Plus[xyx]
                ]}, Spinor, Factoring -> False
                             ] /. plsphold -> Plus;
                FCPrint[2,"Collecting terms w.r.t spinors done:   ", nx];
            ];

            If[ !FreeQ[ nx, $MU],
                nx = nx /. $MU->Unique[System`C]
            ];

            If[ Head[nx] === Plus,
                nx = Map[sufu, nx],
                nx = sufu[nx]
            ];

        	(* in case somthing went wrong .. *)
            If[ nx =!= 0 && FreeQ[nx, DiracTrace],
                FCPrint[0,"Something went wrong while computing the fermions spin sum! Returning unevaluated expression:"];
                nx = expr extraFactor
            ];

        nx/.mul->1
    ]/; !FreeQ[expr,Spinor];

End[];
EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[ $VeryVerbose > 0,
    WriteString["stdout", "FermionSpinSum | \n "]
];
Null
