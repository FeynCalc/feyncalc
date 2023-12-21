---
title: QCD gluon self-energy
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Gl -> Gl, massless QCD, 2-loops";
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

$$\text{FeynCalc }\;\text{10.0.0 (dev version, 2023-12-20 22:40:59 +01:00, dff3b835). For help, use the }\underline{\text{online} \;\text{documentation}}\;\text{, check out the }\underline{\text{wiki}}\;\text{ or visit the }\underline{\text{forum}.}$$

$$\text{Please check our }\underline{\text{FAQ}}\;\text{ for answers to some common FeynCalc questions and have a look at the supplied }\underline{\text{examples}.}$$

$$\text{If you use FeynCalc in your research, please evaluate FeynCalcHowToCite[] to learn how to cite this software.}$$

$$\text{Please keep in mind that the proper academic attribution of our work is crucial to ensure the future development of this package!}$$

$$\text{FeynArts }\;\text{3.11 (3 Aug 2020) patched for use with FeynCalc, for documentation see the }\underline{\text{manual}}\;\text{ or visit }\underline{\text{www}.\text{feynarts}.\text{de}.}$$

$$\text{If you use FeynArts in your research, please cite}$$

$$\text{ $\bullet $ T. Hahn, Comput. Phys. Commun., 140, 418-431, 2001, arXiv:hep-ph/0012260}$$

## Generate Feynman diagrams

Nicer typesetting

```mathematica
diags = InsertFields[CreateTopologies[2, 1 -> 1, ExcludeTopologies -> {Tadpoles}], {V[5]} -> {V[5]}, 
    InsertionLevel -> {Particles}, ExcludeParticles -> {V[1 | 2 | 3], S[_], F[1 | 4], F[3, {2 | 3}]}, Model -> SMQCD];
```

```mathematica
Paint[DiagramExtract[diags, {2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 20,39}], ColumnsXRows -> {4, 1}, SheetHeader -> False,   
   Numbering -> True, ImageSize -> {1024, 256}];
```

![1izbjdmo028st](img/1izbjdmo028st.svg)

![1gwtdibzvkwav](img/1gwtdibzvkwav.svg)

![0zks7qpbolljd](img/0zks7qpbolljd.svg)

![04pp8487m5zpn](img/04pp8487m5zpn.svg)

![1bzq5z1x0b8rv](img/1bzq5z1x0b8rv.svg)

## Obtain the amplitude

```mathematica
ampRaw = FCFAConvert[CreateFeynAmp[DiagramExtract[diags, {2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 20, 39}], Truncated -> True, 
     	PreFactor -> 1], IncomingMomenta -> {p}, OutgoingMomenta -> {p},LoopMomenta -> {q1, q2}, 
    	UndoChiralSplittings -> True, ChangeDimension -> D, List -> True, SMP -> True, 
    	DropSumOver -> True] // SMPToSymbol;
```

## Fix the kinematics

```mathematica
FCClearScalarProducts[];
ScalarProduct[p, p] = pp;
```

## Calculate the amplitude

```mathematica
projector = MTD[Lor1, Lor2] 1/((D - 1) SPD[p]) 1/(2 CA CF) SUNDelta[Glu1, Glu2]
```

$$\frac{g^{\text{Lor1}\;\text{Lor2}} \delta ^{\text{Glu1}\;\text{Glu2}}}{2 (D-1) \;\text{pp} C_A C_F}$$

```mathematica
AbsoluteTiming[ampSimp = (projector ampRaw /. mu -> 0) // Contract // DiracSimplify // SUNSimplify;]
```

$$\{2.15491,\text{Null}\}$$

## Identify and minimize the topologies

```mathematica
{amp, topos} = FCLoopFindTopologies[ampSimp, {q1, q2}];
```

$$\text{FCLoopFindTopologies: Number of the initial candidate topologies: }6$$

$$\text{FCLoopFindTopologies: Number of the identified unique topologies: }5$$

$$\text{FCLoopFindTopologies: Number of the preferred topologies among the unique topologies: }0$$

$$\text{FCLoopFindTopologies: Number of the identified subtopologies: }1$$

```mathematica
subtopos = FCLoopFindSubtopologies[topos];
```

```mathematica
mappings = FCLoopFindTopologyMappings[topos, PreferredTopologies -> subtopos];
```

$$\text{FCLoopFindTopologyMappings: }\;\text{Found }4\text{ mapping relations }$$

$$\text{FCLoopFindTopologyMappings: }\;\text{Final number of independent topologies: }1$$

## Rewrite the amplitude in terms of GLIs

```mathematica
AbsoluteTiming[ampReduced = FCLoopTensorReduce[amp, topos];]
```

$$\{0.348365,\text{Null}\}$$

```mathematica
AbsoluteTiming[ampPreFinal = FCLoopApplyTopologyMappings[ampReduced, mappings];]
```

$$\{0.245714,\text{Null}\}$$

```mathematica
AbsoluteTiming[ampFinal = ampPreFinal // DiracSimplify // SUNSimplify;]
```

$$\{0.17423,\text{Null}\}$$

```mathematica
(*FCReloadAddOns[{"FeynHelpers"}];
FIREPrepareStartFile[mappings[[2]],FCGetNotebookDirectory[]]
FIRECreateStartFile[FCGetNotebookDirectory[],mappings[[2]]]
FIRECreateConfigFile[mappings[[2]],FCGetNotebookDirectory[]]
FIRECreateIntegralFile[Cases2[ampPreFinal,GLI],mappings[[2]],FCGetNotebookDirectory[]]
FIRERunReduction[FCGetNotebookDirectory[],mappings[[2]]]
tables=FIREImportResults[mappings[[2]],FCGetNotebookDirectory[]]//Flatten;
Put[tables,FileNameJoin[{FCGetNotebookDirectory[],"ReductionTable-Gl-Gl.m"}]];*)
```

```mathematica
reductionTable = Get[FileNameJoin[{FCGetNotebookDirectory[], "ReductionTable-Gl-Gl.m"}]];
```

```mathematica
resPreFinal = Collect2[Total[ampFinal /. reductionTable], GLI]
```

$$\frac{i \;\text{gs}^4 G^{\text{fctopology1}}(1,1,0,1,1) \left(D^3 \left(-C_A\right)+D^2 C_A^2+7 D^2 C_A-4 D C_A^2-18 D C_A-2 C_A^2+16 C_A+2 D^3 C_F-18 D^2 C_F+60 D C_F-64 C_F\right)}{2 (D-4) (D-1)}+\frac{1}{4 (D-4)^2 (D-1) \;\text{pp}}i \;\text{gs}^4 G^{\text{fctopology1}}(1,0,1,1,0) \left(2 D^4 C_A+23 D^3 C_A^2-8 D^3 C_A-200 D^2 C_A^2+528 D C_A^2+16 D C_A-368 C_A^2-4 D^4 C_F+32 D^3 C_F-128 D^2 C_F+288 D C_F-256 C_F\right)-\frac{1}{4 (D-6) (D-4)^2 (D-1) \;\text{pp}}i \;\text{gs}^4 G^{\text{fctopology1}}(0,1,1,0,1) \left(6 D^5 C_A^2-2 D^5 C_A-95 D^4 C_A^2+8 D^4 C_A+414 D^3 C_A^2+232 D^3 C_A-440 D^2 C_A^2-1888 D^2 C_A-240 D C_A^2+4928 D C_A-288 C_A^2-4224 C_A+12 D^5 C_F-184 D^4 C_F+1088 D^3 C_F-3232 D^2 C_F+4928 D C_F-3072 C_F\right)$$

```mathematica
integralMappings = FCLoopFindIntegralMappings[Cases2[resPreFinal, GLI], mappings[[2]]]
```

$$\left\{\left\{G^{\text{fctopology1}}(1,0,1,1,0)\to G^{\text{fctopology1}}(0,1,1,0,1)\right\},\left\{G^{\text{fctopology1}}(0,1,1,0,1),G^{\text{fctopology1}}(1,1,0,1,1)\right\}\right\}$$

```mathematica
resFinal = Collect2[resPreFinal /. integralMappings[[1]], GLI]
```

$$-\frac{i \;\text{gs}^4 G^{\text{fctopology1}}(1,1,0,1,1) \left(D^3 C_A-D^2 C_A^2-7 D^2 C_A+4 D C_A^2+18 D C_A+2 C_A^2-16 C_A-2 D^3 C_F+18 D^2 C_F-60 D C_F+64 C_F\right)}{2 (D-4) (D-1)}-\frac{1}{2 (D-6) (D-4)^2 (D-1) \;\text{pp}}i \;\text{gs}^4 G^{\text{fctopology1}}(0,1,1,0,1) \left(3 D^5 C_A^2-2 D^5 C_A-59 D^4 C_A^2+14 D^4 C_A+376 D^3 C_A^2+92 D^3 C_A-1084 D^2 C_A^2-952 D^2 C_A+1648 D C_A^2+2512 D C_A-1248 C_A^2-2112 C_A+8 D^5 C_F-120 D^4 C_F+704 D^3 C_F-2144 D^2 C_F+3456 D C_F-2304 C_F\right)$$

```mathematica
(*
aux1=FCFeynmanParametrize[integralMappings[[2]][[1]],mappings[[2]],Names->x,FCReplaceD->{D->4-2ep}]
preInt1=Integrate[aux1[[1]]/.x[1]->1,{x[2],0,Infinity},Assumptions->{ep>0,x[3]>=0,pp>0,ep<1}]
preInt2=Integrate[preInt1,{x[3],0,Infinity},Assumptions->{ep>0,pp>0,ep<1}]
int2L=SelectNotFree[preInt2,pp](Series[E^(2ep*EulerGamma)SelectFree[preInt2,pp]aux1[[2]],{ep,0,2}]//Normal//SimplifyPolyLog)
*)
```

```mathematica
(*
aux1=FCFeynmanParametrize[SFAD[q1,q1+p],{q1},Names->x,FCReplaceD->{D->4-2ep}]
preInt1=Integrate[aux1[[1]]/.x[1]->1-x[2],{x[2],0,1},Assumptions->{ep>0,pp>0,ep<1}]
int1L=SelectNotFree[preInt1,pp](Series[E^(ep*EulerGamma)SelectFree[preInt1,pp]aux1[[2]],{ep,0,2}]//Normal//SimplifyPolyLog)
*)
```

```mathematica
(*ruleMasters=Thread[Rule[integralMappings[[2]],{ int2L, int1L^2}]]*)
```

```mathematica
ruleMasters = {
   GLI["fctopology1", {0, 1, 1, 0, 1}] -> (-pp)^(1 - 2*ep)*(13/8 + 1/(4*ep) + (115*ep)/16 + (49*ep^2)/2 - (ep*Zeta2)/4 - (13*ep^2*Zeta2)/8 + (9*ep^2*(9/4 - 2*Zeta[3]))/8 - (5*ep^2*Zeta[3])/12), 
   GLI["fctopology1", {1, 1, 0, 1, 1}] -> (2 + ep^(-1) + 4*ep + (16*ep^2)/3 - (ep*Zeta2)/2 - ep^2*Zeta2 + (4*ep^2*(2 - 2*Zeta[3]))/3 + (ep^2*Zeta[3])/3)^2/(-pp)^(2*ep) 
  }
```

$$\left\{G^{\text{fctopology1}}(0,1,1,0,1)\to (-\text{pp})^{1-2 \;\text{ep}} \left(-\frac{13}{8} \;\text{ep}^2 \zeta (2)-\frac{5 \;\text{ep}^2 \zeta (3)}{12}+\frac{9}{8} \;\text{ep}^2 \left(\frac{9}{4}-2 \zeta (3)\right)+\frac{49 \;\text{ep}^2}{2}-\frac{1}{4} \;\text{ep} \zeta (2)+\frac{115 \;\text{ep}}{16}+\frac{1}{4 \;\text{ep}}+\frac{13}{8}\right),G^{\text{fctopology1}}(1,1,0,1,1)\to (-\text{pp})^{-2 \;\text{ep}} \left(\text{ep}^2 (-\zeta (2))+\frac{\text{ep}^2 \zeta (3)}{3}+\frac{4}{3} \;\text{ep}^2 (2-2 \zeta (3))+\frac{16 \;\text{ep}^2}{3}-\frac{1}{2} \;\text{ep} \zeta (2)+4 \;\text{ep}+\frac{1}{\text{ep}}+2\right)^2\right\}$$

```mathematica
resEpPre = FCReplaceD[resFinal /. ruleMasters, D -> 4 - 2 ep]
```

$$\frac{1}{4 (3-2 \;\text{ep}) \;\text{ep}}i \;\text{gs}^4 (-\text{pp})^{-2 \;\text{ep}} \left(\text{ep}^2 (-\zeta (2))+\frac{\text{ep}^2 \zeta (3)}{3}+\frac{4}{3} \;\text{ep}^2 (2-2 \zeta (3))+\frac{16 \;\text{ep}^2}{3}-\frac{1}{2} \;\text{ep} \zeta (2)+4 \;\text{ep}+\frac{1}{\text{ep}}+2\right)^2 \left((4-2 \;\text{ep})^3 C_A-(4-2 \;\text{ep})^2 C_A^2-7 (4-2 \;\text{ep})^2 C_A+4 (4-2 \;\text{ep}) C_A^2+18 (4-2 \;\text{ep}) C_A+2 C_A^2-16 C_A-2 (4-2 \;\text{ep})^3 C_F+18 (4-2 \;\text{ep})^2 C_F-60 (4-2 \;\text{ep}) C_F+64 C_F\right)-\frac{1}{8 (-2 \;\text{ep}-2) (3-2 \;\text{ep}) \;\text{ep}^2 \;\text{pp}}i \;\text{gs}^4 (-\text{pp})^{1-2 \;\text{ep}} \left(-\frac{13}{8} \;\text{ep}^2 \zeta (2)-\frac{5 \;\text{ep}^2 \zeta (3)}{12}+\frac{9}{8} \;\text{ep}^2 \left(\frac{9}{4}-2 \zeta (3)\right)+\frac{49 \;\text{ep}^2}{2}-\frac{1}{4} \;\text{ep} \zeta (2)+\frac{115 \;\text{ep}}{16}+\frac{1}{4 \;\text{ep}}+\frac{13}{8}\right) \left(3 (4-2 \;\text{ep})^5 C_A^2-2 (4-2 \;\text{ep})^5 C_A-59 (4-2 \;\text{ep})^4 C_A^2+14 (4-2 \;\text{ep})^4 C_A+376 (4-2 \;\text{ep})^3 C_A^2+92 (4-2 \;\text{ep})^3 C_A-1084 (4-2 \;\text{ep})^2 C_A^2-952 (4-2 \;\text{ep})^2 C_A+1648 (4-2 \;\text{ep}) C_A^2+2512 (4-2 \;\text{ep}) C_A-1248 C_A^2-2112 C_A+8 (4-2 \;\text{ep})^5 C_F-120 (4-2 \;\text{ep})^4 C_F+704 (4-2 \;\text{ep})^3 C_F-2144 (4-2 \;\text{ep})^2 C_F+3456 (4-2 \;\text{ep}) C_F-2304 C_F\right)$$

```mathematica
resEp = Collect2[Series[FCReplaceD[Cancel[resEpPre/(-pp)^(-2 ep)], D -> 4 - 2 ep], {ep, 0, 0}] // Normal // SUNSimplify[#, SUNNToCACF -> False] &, ep]
```

$$-\frac{5 i \;\text{gs}^4 N (5 N-2)}{12 \;\text{ep}^2}-\frac{i \;\text{gs}^4 \left(583 N^3-238 N^2+36\right)}{72 \;\text{ep} N}+\frac{i \;\text{gs}^4 \left(900 \zeta (2) N^3+432 N^3 \zeta (3)-14311 N^3-360 \zeta (2) N^2+5902 N^2+1728 \zeta (3)-1980\right)}{432 N}$$

## Check the final results

```mathematica
funJ[2, xi] = (1 - 1/12 Pi^2 ep^2)^2 (CA^2 gs^4 (1/ep^2 (-25/12 + 5/24 xi + 1/4 xi^2) + 1/ep (-583/72 + 113/144 xi - 19/24 xi^2 + 3/8 xi^3) - 14311/432 + Zeta[3] + 425/864 xi + 2 xi Zeta[3] - 71/72 xi^2 + 9/16 xi^3 + 1/16 xi^4));
funJ[2, q] = (1 - 1/12 Pi^2 ep^2)^2 (CA Tf gs^4 (1/ep^2 (5/3 - 2/3 xi) + 1/ep (101/18 + 8/9 xi - 2/3 xi^2) + 1961/108 + 8 Zeta[3] + 142/27 xi - 22/9 xi^2 ) + CF Tf gs^4 (2/ep + 55/3 - 16 Zeta[3]));
```

```mathematica
resLit = Collect2[Series[ I (funJ[2, xi] + funJ[2, q]) /. {xi -> 0, Tf -> 1/2}, {ep, 0, 0}] // Normal // SimplifyPolyLog // SUNSimplify[#, SUNNToCACF -> False] &, ep]
```

$$-\frac{5 i \;\text{gs}^4 N (5 N-2)}{12 \;\text{ep}^2}-\frac{i \;\text{gs}^4 \left(583 N^3-238 N^2+36\right)}{72 \;\text{ep} N}+\frac{i \;\text{gs}^4 \left(900 \zeta (2) N^3+432 N^3 \zeta (3)-14311 N^3-360 \zeta (2) N^2+5902 N^2+1728 \zeta (3)-1980\right)}{432 N}$$

```mathematica
FCCompareResults[resLit, resEp, 
       Text -> {"\tCompare to Davydychev, Osland and Tarasov,     hep-ph/9801380, Eqs. 6.10-6.11:", "CORRECT.", "WRONG!"}, 
       Interrupt -> {Hold[Quit[1]], Automatic}, 
       Factoring -> Simplify]; 
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], 
       " s."];

```

$$\text{$\backslash $tCompare to Davydychev, Osland and Tarasov,     hep-ph/9801380, Eqs. 6.10-6.11:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }25.412\text{ s.}$$