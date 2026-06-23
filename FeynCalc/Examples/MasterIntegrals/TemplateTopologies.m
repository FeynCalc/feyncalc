(* ::Package:: *)

{
FCTopology[tad1L, {FeynAmpDenominator[StandardPropagatorDenominator[
      Momentum[k1, D], 0, -m1^2, {1, 1}]]}, {k1}, {}, {}, {}],
FCTopology[tad2L, 
  {FAD[{k1, m1}], FAD[{k2,m2}], FAD[{k1 - k2,m3}]}, {k1, k2}, {}, {}, {}],
FCTopology[prop2Lv1, 
  {FAD[{k1}], FAD[{k2}], FAD[{k1-k2}] ,FAD[{k1-q}],FAD[{k2-q}]}, {k1, k2}, {q}, {Hold[SPD][q,q]->qq}, {}]        
                       
}

