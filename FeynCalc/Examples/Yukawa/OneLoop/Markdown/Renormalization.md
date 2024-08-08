---
title: 1-loop Yukawa renormalization in the minimal subtraction schemes
---


## Load FeynCalc and the necessary add-ons or other packages

This example uses a custom QED model created with FeynRules. Please evaluate the file
FeynCalc/Examples/FeynRules/QED/GenerateModelYukawa.m before running it for the first time.

```mathematica
description = "Renormalization, Yukawa, MS and MSbar, 1-loop";
If[ $FrontEnd === Null, 
  	$FeynCalcStartupMessages = False; 
  	Print[description]; 
  ];
If[ $Notebooks === False, 
  	$FeynCalcStartupMessages = False 
  ];
$LoadAddOns = {"FeynArts"};
<< FeynCalc`
$FAVerbose = 0; 
 
FCCheckVersion[10, 0, 0];
```

$$\text{FeynCalc }\;\text{10.0.0 (dev version, 2024-08-07 16:59:34 +02:00, 2f62a22c). For help, use the }\underline{\text{online} \;\text{documentation},}\;\text{ visit the }\underline{\text{forum}}\;\text{ and have a look at the supplied }\underline{\text{examples}.}\;\text{ The PDF-version of the manual can be downloaded }\underline{\text{here}.}$$

$$\text{If you use FeynCalc in your research, please evaluate FeynCalcHowToCite[] to learn how to cite this software.}$$

$$\text{Please keep in mind that the proper academic attribution of our work is crucial to ensure the future development of this package!}$$

$$\text{FeynArts }\;\text{3.11 (3 Aug 2020) patched for use with FeynCalc, for documentation see the }\underline{\text{manual}}\;\text{ or visit }\underline{\text{www}.\text{feynarts}.\text{de}.}$$

$$\text{If you use FeynArts in your research, please cite}$$

$$\text{ $\bullet $ T. Hahn, Comput. Phys. Commun., 140, 418-431, 2001, arXiv:hep-ph/0012260}$$

## Configure some options

We keep scaleless B0 functions, since otherwise the UV part would not come out right.

```mathematica
$KeepLogDivergentScalelessIntegrals = True;
```

```mathematica
FAPatch[PatchModelsOnly -> True];

(*Patched 4 FeynArts models.*)
```

## Generate Feynman diagrams

```mathematica
params = {InsertionLevel -> {Particles}, Model -> FileNameJoin[{"LY", "LY"}], 
    GenericModel -> FileNameJoin[{"LY", "LY"}], ExcludeParticles -> {}};
top[i_, j_] := CreateTopologies[1, i -> j, 
    ExcludeTopologies -> {Tadpoles, WFCorrections, WFCorrectionCTs}];
topCT[i_, j_] := CreateCTTopologies[1, i -> j, 
     ExcludeTopologies -> {Tadpoles, WFCorrections, WFCorrectionCTs}]; 
 
{diagFermionSE, diagFermionSECT} = InsertFields[#, {F[10]} -> {F[10]}, 
      Sequence @@ params] & /@ {top[1, 1], topCT[1, 1]};
{diagScalarSE, diagScalarSECT} = InsertFields[#, {S[1]} -> {S[1]}, 
      Sequence @@ params] & /@ {top[1, 1], topCT[1, 1]};
{diagVertexFFS, diagVertexFFSCT} = InsertFields[#,  {F[10], S[1]} -> {F[10]}, 
      Sequence @@ params] & /@ {top[2, 1], topCT[2, 1]};
{diagVertexSSSS, diagVertexSSSSCT} = InsertFields[#,  {S[1], S[1]} -> {S[1], S[1]}, 
      Sequence @@ params] & /@ {top[2, 2], topCT[2, 2]};
```

```mathematica
diag1[0] = diagFermionSE[[0]][diagFermionSE[[1]], diagFermionSECT[[1]]];
diag2[0] = diagScalarSE[[0]][diagScalarSE[[1]], diagScalarSECT[[1]]];
diag3[0] = diagVertexFFS[[0]][diagVertexFFS[[1]], diagVertexFFSCT[[1]]];
diag4[0] = diagVertexSSSS[[0]][diagVertexSSSS[[1]], diagVertexSSSSCT[[1]]];
```

```mathematica
Paint[diag1[0], ColumnsXRows -> {2, 1}, SheetHeader -> None, 
   Numbering -> Simple, ImageSize -> 256 {2, 1}];
```

![0f0jiq05y6868](img/0f0jiq05y6868.svg)

```mathematica
Paint[diag2[0], ColumnsXRows -> {2, 1}, SheetHeader -> None, 
   Numbering -> Simple, ImageSize -> 256 {2, 1}];
```

![1g0ppesv184of](img/1g0ppesv184of.svg)

```mathematica
Paint[diag3[0], ColumnsXRows -> {2, 1}, SheetHeader -> None, 
   Numbering -> Simple, ImageSize -> 256 {2, 1}];
```

![1nolf6xwdpe1a](img/1nolf6xwdpe1a.svg)

```mathematica
Paint[diag4[0], ColumnsXRows -> {3, 1}, SheetHeader -> None, 
   Numbering -> Simple, ImageSize -> 256 {3, 1}];
```

![101xx4gtoi8zd](img/101xx4gtoi8zd.svg)

## Obtain the amplitudes

The 1/(2Pi)^D prefactor is implicit.

Fermion self-energy including the counter-term

```mathematica
amp1[0] = FCFAConvert[CreateFeynAmp[diag1[0], Truncated -> True, 
   	GaugeRules -> {}, PreFactor -> 1], 
  	IncomingMomenta -> {p}, OutgoingMomenta -> {p},	
  	LoopMomenta -> {l}, UndoChiralSplittings -> True, 
  	ChangeDimension -> D, List -> False, SMP -> True, 
  	FinalSubstitutions -> {}, Contract -> True]
```

$$-\frac{(-i g).(\gamma \cdot l+\text{Mx}).(-i g)}{\left(l^2-\text{Mx}^2\right).\left((l-p)^2-\text{Mphi}^2 \xi _{S(1)}\right)}-i \;\text{Mx} (\text{Zmx} \;\text{Zx}-1)+i (\text{Zx}-1) \gamma \cdot p$$

Scalar self-energy including the counter-term

```mathematica
amp2[0] = FCFAConvert[CreateFeynAmp[diag2[0], Truncated -> True, 
   	GaugeRules -> {}, PreFactor -> 1], 
  	IncomingMomenta -> {p}, OutgoingMomenta -> {p},	
  	LoopMomenta -> {l}, UndoChiralSplittings -> True, 
  	ChangeDimension -> D, List -> False, SMP -> True, Contract -> True]
```

$$\frac{\text{la}}{2 \left(l^2-\text{Mphi}^2 \xi _{S(1)}\right)}-i \;\text{Mphi}^2 (\text{Zmphi} \;\text{Zphi}-1)+i p^2 (\text{Zphi}-1)$$

Fermion-scalar vertex including the counter-term

```mathematica
amp3[0] = FCFAConvert[CreateFeynAmp[diag3[0], Truncated -> True, 
   	GaugeRules -> {}, PreFactor -> 1], 
  	IncomingMomenta -> {p1, k}, OutgoingMomenta -> {p2}, 
  	LoopMomenta -> {l}, UndoChiralSplittings -> True, ChangeDimension -> D, 
  	List -> False, SMP -> True, Contract -> True]
```

$$-\frac{i (-i g).(\gamma \cdot (k+l)+\text{Mx}).(-i g).(\gamma \cdot l+\text{Mx}).(-i g)}{\left(l^2-\text{Mx}^2\right).\left((k+l)^2-\text{Mx}^2\right).\left((k+l-\text{p2})^2-\text{Mphi}^2 \xi _{S(1)}\right)}-i g \left(\text{Zg} \sqrt{\text{Zphi}} \;\text{Zx}-1\right)$$

Scalar self-interaction vertex including the counter-term

```mathematica
amp4[0] = FCFAConvert[CreateFeynAmp[diag4[0], Truncated -> True, 
   	GaugeRules -> {}, PreFactor -> 1], 
  	IncomingMomenta -> {p1, p2}, OutgoingMomenta -> {p3, p4}, 
  	LoopMomenta -> {l}, UndoChiralSplittings -> True, ChangeDimension -> D, 
  	List -> False, SMP -> True, Contract -> True]
```

$$-\frac{2 \;\text{tr}((\text{Mx}-\gamma \cdot l).(-i g).(\gamma \cdot (-l-\text{p2})+\text{Mx}).(-i g).(\gamma \cdot (-l-\text{p2}+\text{p4})+\text{Mx}).(-i g).(\gamma \cdot (-l-\text{p2}+\text{p3}+\text{p4})+\text{Mx}).(-i g))}{\left(l^2-\text{Mx}^2\right).\left((l+\text{p2})^2-\text{Mx}^2\right).\left((l+\text{p2}-\text{p4})^2-\text{Mx}^2\right).\left((l+\text{p2}-\text{p3}-\text{p4})^2-\text{Mx}^2\right)}-i \;\text{la} \left(\text{Zla} \;\text{Zphi}^2-1\right)$$

## Calculate the amplitudes

### Fermion self-energy

```mathematica
amp1[1] = amp1[0] // ReplaceAll[#, {Zx -> 1 + alpha dZx, 
         Zmx -> 1 + alpha dZmx}] & // Series[#, {alpha, 0, 1}] & // 
    Normal // ReplaceAll[#, alpha -> 1] &
```

$$-\frac{(-i g).(\gamma \cdot l+\text{Mx}).(-i g)}{\left(l^2-\text{Mx}^2\right).\left((l-p)^2-\text{Mphi}^2 \xi _{S(1)}\right)}-i \;\text{Mx} (\text{dZmx}+\text{dZx})+i \;\text{dZx} \gamma \cdot p$$

Tensor reduction allows us to express the electron self-energy in tems of the Passarino-Veltman coefficient functions.

```mathematica
amp1[2] = TID[amp1[1], l, ToPaVe -> True]
```

$$\frac{i \pi ^2 g^2 \left(\gamma \cdot p \left(\text{Mphi}^2 \left(-\xi _{S(1)}\right)+\text{Mx}^2+p^2\right)+2 \;\text{Mx} p^2\right) \;\text{B}_0\left(p^2,\text{Mx}^2,\text{Mphi}^2 \xi _{S(1)}\right)}{2 p^2}+\frac{i \pi ^2 g^2 \gamma \cdot p \;\text{A}_0\left(\text{Mphi}^2 \xi _{S(1)}\right)}{2 p^2}-\frac{i \pi ^2 g^2 \;\text{A}_0\left(\text{Mx}^2\right) \gamma \cdot p}{2 p^2}-i (\text{dZmx} \;\text{Mx}+\text{dZx} \;\text{Mx}-\text{dZx} \gamma \cdot p)$$

Discard all the finite pieces of the 1-loop amplitude

```mathematica
amp1Div[0] = PaVeUVPart[amp1[2], Prefactor -> 1/(2 Pi)^D] // 
         FCReplaceD[#, D -> 4 - 2 Epsilon] & // Series[#, {Epsilon, 0, 0}] & // Normal // 
      FCHideEpsilon // SelectNotFree2[#, {SMP["Delta"], dZx, 
        dZmx}] & // Simplify // Collect2[#, DiracGamma] &
```

$$\frac{i \left(32 \pi ^2 \;\text{dZx}+\Delta  g^2\right) \gamma \cdot p}{32 \pi ^2}-\frac{i \;\text{Mx} \left(16 \pi ^2 \;\text{dZmx}+16 \pi ^2 \;\text{dZx}-\Delta  g^2\right)}{16 \pi ^2}$$

Equating the result to zero and solving for dZx and dZmx we obtain  the renormalization constants in 
the minimal subtraction schemes.

```mathematica
solMSbar1 = FCMatchSolve[amp1Div[0], {g, la, Mx, DiracGamma, SMP}];
solMS1 = solMSbar1 /. SMP["Delta"] -> 1/Epsilon
```

$$\text{FCMatchSolve: Solving for: }\{\text{dZmx},\text{dZx}\}$$

$$\text{FCMatchSolve: A solution exists.}$$

$$\left\{\text{dZmx}\to \frac{3 g^2}{32 \pi ^2 \varepsilon },\text{dZx}\to -\frac{g^2}{32 \pi ^2 \varepsilon }\right\}$$

### Scalar self-energy

```mathematica
amp2[0]
```

$$\frac{\text{la}}{2 \left(l^2-\text{Mphi}^2 \xi _{S(1)}\right)}-i \;\text{Mphi}^2 (\text{Zmphi} \;\text{Zphi}-1)+i p^2 (\text{Zphi}-1)$$

```mathematica
amp2[1] = amp2[0] // ReplaceRepeated[#, {Zphi -> 1 + alpha dZphi, 
         Zmphi -> 1 + alpha dZmphi}] & // Series[#, {alpha, 0, 1}] & //
    Normal // ReplaceAll[#, alpha -> 1] &
```

$$\frac{\text{la}}{2 \left(l^2-\text{Mphi}^2 \xi _{S(1)}\right)}-i \;\text{Mphi}^2 (\text{dZmphi}+\text{dZphi})+i \;\text{dZphi} p^2$$

Tensor reduction allows us to express the scalar self-energy in tems of the Passarino-Veltman coefficient functions.

```mathematica
amp2[2] = TID[amp2[1], l, ToPaVe -> True]
```

$$\frac{1}{2} i \pi ^2 \;\text{la} \;\text{A}_0\left(\text{Mphi}^2 \xi _{S(1)}\right)-i \left(\text{dZmphi} \;\text{Mphi}^2+\text{dZphi} \;\text{Mphi}^2-\text{dZphi} p^2\right)$$

Discard all the finite pieces of the 1-loop amplitude

```mathematica
amp2Div[0] = PaVeUVPart[amp2[2], Prefactor -> 1/(2 Pi)^D] // 
         FCReplaceD[#, D -> 4 - 2 Epsilon] & // Series[#, {Epsilon, 0, 0}] & // Normal // 
      FCHideEpsilon // SelectNotFree2[#, {SMP["Delta"], dZphi, dZmphi}] & // Simplify // 
   Collect2[#, p, Mphi] &
```

$$i \;\text{dZphi} p^2-\frac{i \;\text{Mphi}^2 \left(32 \pi ^2 \;\text{dZmphi}+32 \pi ^2 \;\text{dZphi}-\Delta  \;\text{la} \xi _{S(1)}\right)}{32 \pi ^2}$$

Equating this to zero and solving for dZphi and dZmphi  obtain  the renormalization constants in the minimal subtraction schemes.

```mathematica
solMSbar2 = FCMatchSolve[amp2Div[0], {g, la, Mphi, p, SMP, GaugeXi}]
solMS2 = solMSbar2 /. SMP["Delta"] -> 1/Epsilon;
```

$$\text{FCMatchSolve: Following coefficients trivially vanish: }\{\text{dZphi}\to 0\}$$

$$\text{FCMatchSolve: Solving for: }\{\text{dZmphi}\}$$

$$\text{FCMatchSolve: A solution exists.}$$

$$\left\{\text{dZphi}\to 0,\text{dZmphi}\to \frac{\Delta  \;\text{la} \xi _{S(1)}}{32 \pi ^2}\right\}$$

### Fermion-scalar vertex

```mathematica
amp3[1] = amp3[0] // ReplaceRepeated[#, {Zphi -> 1 + alpha dZphi, 
         Zx -> 1 + alpha dZx, Zg -> 1 + alpha dZg}] & // 
     Series[#, {alpha, 0, 1}] & // Normal // ReplaceAll[#, alpha -> 1] &
```

$$-\frac{i (-i g).(\gamma \cdot (k+l)+\text{Mx}).(-i g).(\gamma \cdot l+\text{Mx}).(-i g)}{\left(l^2-\text{Mx}^2\right).\left((k+l)^2-\text{Mx}^2\right).\left((k+l-\text{p2})^2-\text{Mphi}^2 \xi _{S(1)}\right)}-i g \left(\text{dZg}+\frac{\text{dZphi}}{2}+\text{dZx}\right)$$

The result of the tensor reduction is quite large, since we keep the full gauge dependence and do not specify the kinematics

```mathematica
amp3[2] = TID[amp3[1], l, ToPaVe -> True, UsePaVeBasis -> True]
```

$$i \pi ^2 g^3 \;\text{B}_0\left(\text{p2}^2,\text{Mx}^2,\text{Mphi}^2 \xi _{S(1)}\right)+i \pi ^2 g^3 \left(-(\gamma \cdot k).(\gamma \cdot k)-\text{Mx} \gamma \cdot k+2 \;\text{Mx}^2\right) \;\text{C}_0\left(k^2,\text{p2}^2,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,\text{Mx}^2,\text{Mx}^2,\text{Mphi}^2 \xi _{S(1)}\right)-i \pi ^2 g^3 ((\gamma \cdot k).(\gamma \cdot k)+2 \;\text{Mx} \gamma \cdot k) \;\text{C}_1\left(k^2,-2 (k\cdot \;\text{p2})+k^2+\text{p2}^2,\text{p2}^2,\text{Mx}^2,\text{Mx}^2,\text{Mphi}^2 \xi _{S(1)}\right)-i \pi ^2 g^3 ((\gamma \cdot k).(\gamma \cdot \;\text{p2})+2 \;\text{Mx} \gamma \cdot \;\text{p2}) \;\text{C}_1\left(\text{p2}^2,-2 (k\cdot \;\text{p2})+k^2+\text{p2}^2,k^2,\text{Mx}^2,\text{Mphi}^2 \xi _{S(1)},\text{Mx}^2\right)-\frac{1}{2} i g (2 \;\text{dZg}+\text{dZphi}+2 \;\text{dZx})$$

Discard all the finite pieces of the 1-loop amplitude

```mathematica
amp3Div[0] = PaVeUVPart[amp3[2], Prefactor -> 1/(2 Pi)^D] // DiracSimplify // 
          FCReplaceD[#, D -> 4 - 2 Epsilon] & // Series[#, {Epsilon, 0, 0}] & // Normal // 
       FCHideEpsilon // SelectNotFree2[#, {SMP["Delta"], dZphi, 
         dZx, dZg}] & // ReplaceAll[#, Join[solMSbar1, solMSbar2]] & //Simplify // FCFactorOut[#, g] &
```

$$g \left(\frac{3 i \Delta  g^2}{32 \pi ^2}-i \;\text{dZg}\right)$$

Equating this to zero and solving for dZg we  obtain  the renormalization constant in the minimal subtraction schemes.

```mathematica
solMSbar3 = FCMatchSolve[amp3Div[0], {g, SMP}]
solMS3 = solMSbar3 /. SMP["Delta"] -> 1/Epsilon;
```

$$\text{FCMatchSolve: Solving for: }\{\text{dZg}\}$$

$$\text{FCMatchSolve: A solution exists.}$$

$$\left\{\text{dZg}\to \frac{3 \Delta  g^2}{32 \pi ^2}\right\}$$

### Scalar self-interaction vertex

```mathematica
amp4[1] = amp4[0] // ReplaceRepeated[#, {Zphi -> 1 + alpha dZphi, 
         Zla -> 1 + alpha dZla}] & // 
     Series[#, {alpha, 0, 1}] & // Normal // ReplaceAll[#, alpha -> 1] &
```

$$-\frac{2 \;\text{tr}((\text{Mx}-\gamma \cdot l).(-i g).(\gamma \cdot (-l-\text{p2})+\text{Mx}).(-i g).(\gamma \cdot (-l-\text{p2}+\text{p4})+\text{Mx}).(-i g).(\gamma \cdot (-l-\text{p2}+\text{p3}+\text{p4})+\text{Mx}).(-i g))}{\left(l^2-\text{Mx}^2\right).\left((l+\text{p2})^2-\text{Mx}^2\right).\left((l+\text{p2}-\text{p4})^2-\text{Mx}^2\right).\left((l+\text{p2}-\text{p3}-\text{p4})^2-\text{Mx}^2\right)}-i \;\text{la} (\text{dZla}+2 \;\text{dZphi})$$

The result of the tensor reduction is quite large, since we keep the full gauge dependence and do not specify the kinematics

```mathematica
amp4[2] = TID[amp4[1], l, ToPaVe -> True, UsePaVeBasis -> True]
```

$$4 i \pi ^2 \;\text{B}_0\left(\text{p2}^2,\text{Mx}^2,\text{Mx}^2\right) g^4-4 i \pi ^2 \;\text{B}_0\left(\text{p2}^2-2 (\text{p2}\cdot \;\text{p4})+\text{p4}^2,\text{Mx}^2,\text{Mx}^2\right) g^4-4 i \pi ^2 \;\text{B}_0\left(\text{p3}^2+2 (\text{p3}\cdot \;\text{p4})+\text{p4}^2,\text{Mx}^2,\text{Mx}^2\right) g^4+4 i \pi ^2 \;\text{B}_0\left(\text{p2}^2-2 (\text{p2}\cdot \;\text{p3})-2 (\text{p2}\cdot \;\text{p4})+\text{p3}^2+2 (\text{p3}\cdot \;\text{p4})+\text{p4}^2,\text{Mx}^2,\text{Mx}^2\right) g^4-4 i \pi ^2 \;\text{C}_0\left(\text{p2}^2,\text{p4}^2,\text{p2}^2-2 (\text{p2}\cdot \;\text{p4})+\text{p4}^2,\text{Mx}^2,\text{Mx}^2,\text{Mx}^2\right) \left(4 \;\text{Mx}^2-\text{p2}\cdot \;\text{p4}\right) g^4-4 i \pi ^2 \;\text{C}_0\left(\text{p3}^2,\text{p2}^2-2 (\text{p2}\cdot \;\text{p4})+\text{p4}^2,\text{p2}^2-2 (\text{p2}\cdot \;\text{p3})-2 (\text{p2}\cdot \;\text{p4})+\text{p3}^2+2 (\text{p3}\cdot \;\text{p4})+\text{p4}^2,\text{Mx}^2,\text{Mx}^2,\text{Mx}^2\right) \left(4 \;\text{Mx}^2+\text{p2}\cdot \;\text{p3}-\text{p3}^2-\text{p3}\cdot \;\text{p4}\right) g^4-4 i \pi ^2 \;\text{C}_0\left(\text{p3}^2,\text{p4}^2,\text{p3}^2+2 (\text{p3}\cdot \;\text{p4})+\text{p4}^2,\text{Mx}^2,\text{Mx}^2,\text{Mx}^2\right) \left(4 \;\text{Mx}^2+\text{p3}\cdot \;\text{p4}\right) g^4-4 i \pi ^2 \;\text{C}_0\left(\text{p2}^2,\text{p3}^2+2 (\text{p3}\cdot \;\text{p4})+\text{p4}^2,\text{p2}^2-2 (\text{p2}\cdot \;\text{p3})-2 (\text{p2}\cdot \;\text{p4})+\text{p3}^2+2 (\text{p3}\cdot \;\text{p4})+\text{p4}^2,\text{Mx}^2,\text{Mx}^2,\text{Mx}^2\right) \left(2 \;\text{Mx}^2-\text{p2}^2+\text{p2}\cdot \;\text{p3}+\text{p2}\cdot \;\text{p4}+\text{p3}^2+2 (\text{p3}\cdot \;\text{p4})+\text{p4}^2\right) g^4-4 i \pi ^2 \;\text{D}_0\left(\text{p2}^2,\text{p4}^2,\text{p3}^2,\text{p2}^2-2 (\text{p2}\cdot \;\text{p3})-2 (\text{p2}\cdot \;\text{p4})+\text{p3}^2+2 (\text{p3}\cdot \;\text{p4})+\text{p4}^2,\text{p2}^2-2 (\text{p2}\cdot \;\text{p4})+\text{p4}^2,\text{p3}^2+2 (\text{p3}\cdot \;\text{p4})+\text{p4}^2,\text{Mx}^2,\text{Mx}^2,\text{Mx}^2,\text{Mx}^2\right) \left(16 \;\text{Mx}^4-4 \;\text{p2}^2 \;\text{Mx}^2+4 (\text{p2}\cdot \;\text{p3}) \;\text{Mx}^2+4 (\text{p2}\cdot \;\text{p4}) \;\text{Mx}^2-4 \;\text{p3}^2 \;\text{Mx}^2-4 (\text{p3}\cdot \;\text{p4}) \;\text{Mx}^2-4 \;\text{p4}^2 \;\text{Mx}^2+(\text{p2}\cdot \;\text{p4}) \;\text{p3}^2-\text{p2}^2 (\text{p3}\cdot \;\text{p4})+2 (\text{p2}\cdot \;\text{p4}) (\text{p3}\cdot \;\text{p4})-(\text{p2}\cdot \;\text{p3}) \;\text{p4}^2\right) g^4-8 i \pi ^2 (\text{p2}\cdot \;\text{p3}+\text{p2}\cdot \;\text{p4}) \;\text{C}_1\left(\text{p2}^2,\text{p2}^2-2 (\text{p2}\cdot \;\text{p3})-2 (\text{p2}\cdot \;\text{p4})+\text{p3}^2+2 (\text{p3}\cdot \;\text{p4})+\text{p4}^2,\text{p3}^2+2 (\text{p3}\cdot \;\text{p4})+\text{p4}^2,\text{Mx}^2,\text{Mx}^2,\text{Mx}^2\right) g^4-8 i \pi ^2 \left(\text{p3}^2+2 (\text{p3}\cdot \;\text{p4})+\text{p4}^2\right) \;\text{C}_1\left(\text{p3}^2+2 (\text{p3}\cdot \;\text{p4})+\text{p4}^2,\text{p2}^2-2 (\text{p2}\cdot \;\text{p3})-2 (\text{p2}\cdot \;\text{p4})+\text{p3}^2+2 (\text{p3}\cdot \;\text{p4})+\text{p4}^2,\text{p2}^2,\text{Mx}^2,\text{Mx}^2,\text{Mx}^2\right) g^4-8 i D \pi ^2 \;\text{C}_{00}\left(\text{p2}^2,\text{p3}^2+2 (\text{p3}\cdot \;\text{p4})+\text{p4}^2,\text{p2}^2-2 (\text{p2}\cdot \;\text{p3})-2 (\text{p2}\cdot \;\text{p4})+\text{p3}^2+2 (\text{p3}\cdot \;\text{p4})+\text{p4}^2,\text{Mx}^2,\text{Mx}^2,\text{Mx}^2\right) g^4-8 i \pi ^2 \;\text{p2}^2 \;\text{C}_{11}\left(\text{p2}^2,\text{p2}^2-2 (\text{p2}\cdot \;\text{p3})-2 (\text{p2}\cdot \;\text{p4})+\text{p3}^2+2 (\text{p3}\cdot \;\text{p4})+\text{p4}^2,\text{p3}^2+2 (\text{p3}\cdot \;\text{p4})+\text{p4}^2,\text{Mx}^2,\text{Mx}^2,\text{Mx}^2\right) g^4-8 i \pi ^2 \left(\text{p3}^2+2 (\text{p3}\cdot \;\text{p4})+\text{p4}^2\right) \;\text{C}_{11}\left(\text{p3}^2+2 (\text{p3}\cdot \;\text{p4})+\text{p4}^2,\text{p2}^2-2 (\text{p2}\cdot \;\text{p3})-2 (\text{p2}\cdot \;\text{p4})+\text{p3}^2+2 (\text{p3}\cdot \;\text{p4})+\text{p4}^2,\text{p2}^2,\text{Mx}^2,\text{Mx}^2,\text{Mx}^2\right) g^4-16 i \pi ^2 (\text{p2}\cdot \;\text{p3}+\text{p2}\cdot \;\text{p4}) \;\text{C}_{12}\left(\text{p2}^2,\text{p2}^2-2 (\text{p2}\cdot \;\text{p3})-2 (\text{p2}\cdot \;\text{p4})+\text{p3}^2+2 (\text{p3}\cdot \;\text{p4})+\text{p4}^2,\text{p3}^2+2 (\text{p3}\cdot \;\text{p4})+\text{p4}^2,\text{Mx}^2,\text{Mx}^2,\text{Mx}^2\right) g^4-i (\text{dZla}+2 \;\text{dZphi}) \;\text{la}$$

Discard all the finite pieces of the 1-loop amplitude

```mathematica
amp4Div[0] = PaVeUVPart[amp4[2], Prefactor -> 1/(2 Pi)^D] // DiracSimplify // 
         FCReplaceD[#, D -> 4 - 2 Epsilon] & // Series[#, {Epsilon, 0, 0}] & // Normal // 
      FCHideEpsilon // SelectNotFree2[#, {SMP["Delta"], dZphi, 
        dZla}] & // ReplaceAll[#, Join[solMSbar1, solMSbar2]] & // Simplify
```

$$-\frac{1}{2} i \left(2 \;\text{dZla} \;\text{la}+\frac{\Delta  g^4}{\pi ^2}\right)$$

Equating this to zero and solving for dZg we  obtain  the renormalization constant in the minimal subtraction schemes.

```mathematica
solMSbar4 = FCMatchSolve[amp4Div[0], {g, SMP, la}]
solMS4 = solMSbar4 /. SMP["Delta"] -> 1/Epsilon;
```

$$\text{FCMatchSolve: Solving for: }\{\text{dZla}\}$$

$$\text{FCMatchSolve: A solution exists.}$$

$$\left\{\text{dZla}\to -\frac{\Delta  g^4}{2 \pi ^2 \;\text{la}}\right\}$$

```mathematica
Join[solMSbar1, solMSbar2, solMSbar3, solMSbar4] // TableForm
```

$$\begin{array}{l}
 \;\text{dZmx}\to \frac{3 \Delta  g^2}{32 \pi ^2} \\
 \;\text{dZx}\to -\frac{\Delta  g^2}{32 \pi ^2} \\
 \;\text{dZphi}\to 0 \\
 \;\text{dZmphi}\to \frac{\Delta  \;\text{la} \xi _{S(1)}}{32 \pi ^2} \\
 \;\text{dZg}\to \frac{3 \Delta  g^2}{32 \pi ^2} \\
 \;\text{dZla}\to -\frac{\Delta  g^4}{2 \pi ^2 \;\text{la}} \\
\end{array}$$