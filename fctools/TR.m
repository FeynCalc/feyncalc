(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: TR  *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)

(* :Summary: Dirac trace calculation  (see also DiracTrace) *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`fctools`TR`",
             "HighEnergyPhysics`FeynCalc`"];

TR::"usage"=
"TR[exp] calculates the Dirac trace of exp.
Depending on the setting of the option SUNTrace also
a trace over SU(N) objects is performed.
TR is identical to
DiracTrace, up to the default setting of DiracTraceEvaluate.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

fci = MakeContext["FeynCalcInternal"];

MakeContext[ DiracTrace, DiracTraceEvaluate,
Factoring, FeynCalcExternal, LeviCivitaSign, Mandelstam,
PairCollect, Schouten, Explicit, SUNIndex, ExplicitSUNIndex,
SUNSimplify, SUNT, SUNTrace, TraceOfOne, Trick, SUNNToCACF];

Options[ TR ] = { DiracTraceEvaluate -> True,
                  Factoring          -> False,
                  FeynCalcExternal   -> False,
                  LeviCivitaSign     -> I,
                  Mandelstam         -> {},
                  PairCollect        -> False,
                  Schouten           -> 442,
                  SUNTrace           -> False,
                  TraceOfOne         -> 4,
                  (*Added 27/8-2002, F.Orellana*)
                  SUNNToCACF -> False
                };

TR[x_, rul___Rule] := Block[{tt, doot, diractr, dit, fcex, diractrev},
                          diractrev = DiracTraceEvaluate /. {rul} /. Options[TR];
                             tt = fci[x];

                          If[(SUNTrace /. {rul} /. Options[Tr])=== True &&
                             (!FreeQ[tt, SUNIndex|ExplicitSUNIndex]),

                             tt = DiracTrace[tt,
                                 (*Added 27/8-2002, F.Orellana*)
                                  Sequence@@Join[{DiracTraceEvaluate -> False},
                                                 {rul}, Options[TR]]];
                             tt = SUNSimplify[tt,
                                              SUNTrace -> True,
                                              Explicit -> False
                                             ]; (**)
                             tt = tt /. 
                             (DiracTraceEvaluate -> False) :>
                             (DiracTraceEvaluate -> diractrev) //
                                     SUNSimplify[#, SUNTrace -> False,
                                                    Explicit -> False]&,

                             If[FreeQ[tt, SUNIndex|ExplicitSUNIndex],
                                tt = DiracTrace[tt,
                                     (**)Sequence@@Join[{DiracTraceEvaluate -> False},
                                                        {rul},Options[TR]]] // 
                                    (*Added 27/8-2002, F.Orellana*)
                                     If[(SUNTrace /. {rul} /. Options[TR])=== True,
                                     SUNSimplify[#, SUNTrace -> True,
                                                    Explicit -> False], #]&;
                                tt = tt /.
                                     (DiracTraceEvaluate -> False) :>
                                     (DiracTraceEvaluate -> diractrev) //
                                     SUNSimplify[#, SUNTrace -> False,
                                                    Explicit -> False]&,
                                (*!FreeQ[tt, SUNIndex|ExplicitSUNIndex] -> !SUNTrace*)
                                tt = DiracTrace[Trick[tt]//
                                     SUNSimplify[#,
                                     (**)Sequence@@Join[{rul},Options[TR]]]&,
                                     (**)Sequence@@Join[{rul},Options[TR]]]
                               ]
                            ];

                          
                          If[!FreeQ[tt, SUNIndex|ExplicitSUNIndex],
                             tt = tt /. (*Added 23/1-2003. F.Orellana.
                             If a spursav is left from DiracTrace it means
                             that SU(N) stuff is there in the trace*)
                             HighEnergyPhysics`fctools`DiracTrace`Private`spursav :>
                             (SUNTrace[DOT@@{##}]&) /.
                             DiracTrace-> dit /.DOT -> doot;
                             tt = tt /. {doot[a__SUNT, b__] :>
                                         (doot[a] doot[b]) /;
                                         FreeQ[{b}, SUNIndex|ExplicitSUNIndex]
                                        } /. doot -> DOT /.
                                         dit -> DiracTrace;
                            ];
                          diractr[y__] := (DiracTrace @@
                            Join[{y}, {rul}, Options[TR]]);

                          tt = tt /. DiracTrace -> diractr;
                          If[FeynCalcExternal /. {rul} /. Options[TR],
                             tt = FeynCalcExternal[tt]
                            ];
                           tt];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "TR | \n "]];
Null
