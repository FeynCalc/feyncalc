---
title: Gluon-gluon to gluon-gluon scattering
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Gl Gl -> Gl Gl, QCD, matrix element squared, tree";
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
 
FCCheckVersion[9, 3, 1];
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
MakeBoxes[k1, TraditionalForm] := "\!\(\*SubscriptBox[\(k\), \(1\)]\)";
MakeBoxes[k2, TraditionalForm] := "\!\(\*SubscriptBox[\(k\), \(2\)]\)";
MakeBoxes[k3, TraditionalForm] := "\!\(\*SubscriptBox[\(k\), \(3\)]\)";
MakeBoxes[k4, TraditionalForm] := "\!\(\*SubscriptBox[\(k\), \(4\)]\)";
```

```mathematica
diags = InsertFields[CreateTopologies[0, 2 -> 2], {V[5], V[5]} -> 
     		{V[5], V[5]}, InsertionLevel -> {Classes}, Model -> "SMQCD"]; 
 
Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 256}];
```

![18zdhyjg2qrmr](img/18zdhyjg2qrmr.svg)

![1684q24zsrp7y](img/1684q24zsrp7y.svg)

## Obtain the amplitude

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta -> {k1, k2}, 
  	OutgoingMomenta -> {k3, k4}, UndoChiralSplittings -> True, ChangeDimension -> 4, 
  	TransversePolarizationVectors -> {k1, k2, k3, k4}, List -> True, SMP -> True, 
  	Contract -> True, DropSumOver -> True]
```

$$\left\{-i \left(-i \left(\bar{\varepsilon }\left(k_1\right)\cdot \bar{\varepsilon }\left(k_2\right)\right) \left(\bar{\varepsilon }^*\left(k_3\right)\cdot \bar{\varepsilon }^*\left(k_4\right)\right) \left(f^{\text{Glu1}\;\text{Glu4}\;\text{\$AL\$13693}} f^{\text{Glu2}\;\text{Glu3}\;\text{\$AL\$13693}}+f^{\text{Glu1}\;\text{Glu3}\;\text{\$AL\$13692}} f^{\text{Glu2}\;\text{Glu4}\;\text{\$AL\$13692}}\right) g_s^2-i \left(\bar{\varepsilon }\left(k_1\right)\cdot \bar{\varepsilon }^*\left(k_4\right)\right) \left(\bar{\varepsilon }\left(k_2\right)\cdot \bar{\varepsilon }^*\left(k_3\right)\right) \left(-f^{\text{Glu1}\;\text{Glu3}\;\text{\$AL\$13691}} f^{\text{Glu2}\;\text{Glu4}\;\text{\$AL\$13691}}-f^{\text{Glu1}\;\text{Glu2}\;\text{\$AL\$13690}} f^{\text{Glu3}\;\text{Glu4}\;\text{\$AL\$13690}}\right) g_s^2-i \left(\bar{\varepsilon }\left(k_1\right)\cdot \bar{\varepsilon }^*\left(k_3\right)\right) \left(\bar{\varepsilon }\left(k_2\right)\cdot \bar{\varepsilon }^*\left(k_4\right)\right) \left(f^{\text{Glu1}\;\text{Glu2}\;\text{\$AL\$13694}} f^{\text{Glu3}\;\text{Glu4}\;\text{\$AL\$13694}}-f^{\text{Glu1}\;\text{Glu4}\;\text{\$AL\$13695}} f^{\text{Glu2}\;\text{Glu3}\;\text{\$AL\$13695}}\right) g_s^2\right),\frac{2 \left(\overline{k_2}\cdot \bar{\varepsilon }^*\left(k_3\right)-\overline{k_1}\cdot \bar{\varepsilon }^*\left(k_3\right)\right) \left(\overline{k_3}\cdot \bar{\varepsilon }^*\left(k_4\right)\right) \left(\bar{\varepsilon }\left(k_1\right)\cdot \bar{\varepsilon }\left(k_2\right)\right) g_s^2 f^{\text{Glu1}\;\text{Glu2}\;\text{Glu5}} f^{\text{Glu3}\;\text{Glu4}\;\text{Glu5}}}{(\overline{k_3}+\overline{k_4}){}^2}-\frac{2 \left(\overline{k_2}\cdot \bar{\varepsilon }^*\left(k_4\right)-\overline{k_1}\cdot \bar{\varepsilon }^*\left(k_4\right)\right) \left(\overline{k_4}\cdot \bar{\varepsilon }^*\left(k_3\right)\right) \left(\bar{\varepsilon }\left(k_1\right)\cdot \bar{\varepsilon }\left(k_2\right)\right) g_s^2 f^{\text{Glu1}\;\text{Glu2}\;\text{Glu5}} f^{\text{Glu3}\;\text{Glu4}\;\text{Glu5}}}{(\overline{k_3}+\overline{k_4}){}^2}+\frac{2 \left(\overline{k_3}\cdot \bar{\varepsilon }^*\left(k_4\right)\right) \left(\overline{k_1}\cdot \bar{\varepsilon }\left(k_2\right)+\overline{k_3}\cdot \bar{\varepsilon }\left(k_2\right)+\overline{k_4}\cdot \bar{\varepsilon }\left(k_2\right)\right) \left(\bar{\varepsilon }\left(k_1\right)\cdot \bar{\varepsilon }^*\left(k_3\right)\right) g_s^2 f^{\text{Glu1}\;\text{Glu2}\;\text{Glu5}} f^{\text{Glu3}\;\text{Glu4}\;\text{Glu5}}}{(\overline{k_3}+\overline{k_4}){}^2}-\frac{2 \left(\overline{k_1}\cdot \bar{\varepsilon }\left(k_2\right)+\overline{k_3}\cdot \bar{\varepsilon }\left(k_2\right)+\overline{k_4}\cdot \bar{\varepsilon }\left(k_2\right)\right) \left(\overline{k_4}\cdot \bar{\varepsilon }^*\left(k_3\right)\right) \left(\bar{\varepsilon }\left(k_1\right)\cdot \bar{\varepsilon }^*\left(k_4\right)\right) g_s^2 f^{\text{Glu1}\;\text{Glu2}\;\text{Glu5}} f^{\text{Glu3}\;\text{Glu4}\;\text{Glu5}}}{(\overline{k_3}+\overline{k_4}){}^2}+\frac{2 \left(\overline{k_3}\cdot \bar{\varepsilon }^*\left(k_4\right)\right) \left(-\left(\overline{k_2}\cdot \bar{\varepsilon }\left(k_1\right)\right)-\overline{k_3}\cdot \bar{\varepsilon }\left(k_1\right)-\overline{k_4}\cdot \bar{\varepsilon }\left(k_1\right)\right) \left(\bar{\varepsilon }\left(k_2\right)\cdot \bar{\varepsilon }^*\left(k_3\right)\right) g_s^2 f^{\text{Glu1}\;\text{Glu2}\;\text{Glu5}} f^{\text{Glu3}\;\text{Glu4}\;\text{Glu5}}}{(\overline{k_3}+\overline{k_4}){}^2}-\frac{2 \left(-\left(\overline{k_2}\cdot \bar{\varepsilon }\left(k_1\right)\right)-\overline{k_3}\cdot \bar{\varepsilon }\left(k_1\right)-\overline{k_4}\cdot \bar{\varepsilon }\left(k_1\right)\right) \left(\overline{k_4}\cdot \bar{\varepsilon }^*\left(k_3\right)\right) \left(\bar{\varepsilon }\left(k_2\right)\cdot \bar{\varepsilon }^*\left(k_4\right)\right) g_s^2 f^{\text{Glu1}\;\text{Glu2}\;\text{Glu5}} f^{\text{Glu3}\;\text{Glu4}\;\text{Glu5}}}{(\overline{k_3}+\overline{k_4}){}^2}-\frac{\left(-\left(\overline{k_2}\cdot \bar{\varepsilon }\left(k_1\right)\right)-\overline{k_3}\cdot \bar{\varepsilon }\left(k_1\right)-\overline{k_4}\cdot \bar{\varepsilon }\left(k_1\right)\right) \left(\overline{k_3}\cdot \bar{\varepsilon }\left(k_2\right)-\overline{k_4}\cdot \bar{\varepsilon }\left(k_2\right)\right) \left(\bar{\varepsilon }^*\left(k_3\right)\cdot \bar{\varepsilon }^*\left(k_4\right)\right) g_s^2 f^{\text{Glu1}\;\text{Glu2}\;\text{Glu5}} f^{\text{Glu3}\;\text{Glu4}\;\text{Glu5}}}{(\overline{k_3}+\overline{k_4}){}^2}-\frac{\left(\overline{k_3}\cdot \bar{\varepsilon }\left(k_1\right)-\overline{k_4}\cdot \bar{\varepsilon }\left(k_1\right)\right) \left(\overline{k_1}\cdot \bar{\varepsilon }\left(k_2\right)+\overline{k_3}\cdot \bar{\varepsilon }\left(k_2\right)+\overline{k_4}\cdot \bar{\varepsilon }\left(k_2\right)\right) \left(\bar{\varepsilon }^*\left(k_3\right)\cdot \bar{\varepsilon }^*\left(k_4\right)\right) g_s^2 f^{\text{Glu1}\;\text{Glu2}\;\text{Glu5}} f^{\text{Glu3}\;\text{Glu4}\;\text{Glu5}}}{(\overline{k_3}+\overline{k_4}){}^2}-\frac{\left(-\left(\overline{k_1}\cdot \overline{k_3}\right)+\overline{k_1}\cdot \overline{k_4}+\overline{k_2}\cdot \overline{k_3}-\overline{k_2}\cdot \overline{k_4}\right) \left(\bar{\varepsilon }\left(k_1\right)\cdot \bar{\varepsilon }\left(k_2\right)\right) \left(\bar{\varepsilon }^*\left(k_3\right)\cdot \bar{\varepsilon }^*\left(k_4\right)\right) g_s^2 f^{\text{Glu1}\;\text{Glu2}\;\text{Glu5}} f^{\text{Glu3}\;\text{Glu4}\;\text{Glu5}}}{(\overline{k_3}+\overline{k_4}){}^2},-\frac{2 \left(\overline{k_2}\cdot \bar{\varepsilon }^*\left(k_4\right)\right) \left(\overline{k_1}\cdot \bar{\varepsilon }^*\left(k_3\right)-\overline{k_2}\cdot \bar{\varepsilon }^*\left(k_3\right)+\overline{k_4}\cdot \bar{\varepsilon }^*\left(k_3\right)\right) \left(\bar{\varepsilon }\left(k_1\right)\cdot \bar{\varepsilon }\left(k_2\right)\right) g_s^2 f^{\text{Glu1}\;\text{Glu3}\;\text{Glu5}} f^{\text{Glu2}\;\text{Glu4}\;\text{Glu5}}}{(\overline{k_4}-\overline{k_2}){}^2}-\frac{2 \left(\overline{k_2}\cdot \bar{\varepsilon }^*\left(k_4\right)\right) \left(-\left(\overline{k_1}\cdot \bar{\varepsilon }\left(k_2\right)\right)-\overline{k_3}\cdot \bar{\varepsilon }\left(k_2\right)\right) \left(\bar{\varepsilon }\left(k_1\right)\cdot \bar{\varepsilon }^*\left(k_3\right)\right) g_s^2 f^{\text{Glu1}\;\text{Glu3}\;\text{Glu5}} f^{\text{Glu2}\;\text{Glu4}\;\text{Glu5}}}{(\overline{k_4}-\overline{k_2}){}^2}-\frac{2 \left(-\left(\overline{k_1}\cdot \bar{\varepsilon }^*\left(k_4\right)\right)-\overline{k_3}\cdot \bar{\varepsilon }^*\left(k_4\right)\right) \left(\overline{k_4}\cdot \bar{\varepsilon }\left(k_2\right)\right) \left(\bar{\varepsilon }\left(k_1\right)\cdot \bar{\varepsilon }^*\left(k_3\right)\right) g_s^2 f^{\text{Glu1}\;\text{Glu3}\;\text{Glu5}} f^{\text{Glu2}\;\text{Glu4}\;\text{Glu5}}}{(\overline{k_4}-\overline{k_2}){}^2}-\frac{2 \left(\overline{k_4}\cdot \bar{\varepsilon }\left(k_2\right)\right) \left(\overline{k_1}\cdot \bar{\varepsilon }^*\left(k_3\right)-\overline{k_2}\cdot \bar{\varepsilon }^*\left(k_3\right)+\overline{k_4}\cdot \bar{\varepsilon }^*\left(k_3\right)\right) \left(\bar{\varepsilon }\left(k_1\right)\cdot \bar{\varepsilon }^*\left(k_4\right)\right) g_s^2 f^{\text{Glu1}\;\text{Glu3}\;\text{Glu5}} f^{\text{Glu2}\;\text{Glu4}\;\text{Glu5}}}{(\overline{k_4}-\overline{k_2}){}^2}-\frac{2 \left(\overline{k_2}\cdot \bar{\varepsilon }^*\left(k_4\right)\right) \left(\overline{k_2}\cdot \bar{\varepsilon }\left(k_1\right)+\overline{k_3}\cdot \bar{\varepsilon }\left(k_1\right)-\overline{k_4}\cdot \bar{\varepsilon }\left(k_1\right)\right) \left(\bar{\varepsilon }\left(k_2\right)\cdot \bar{\varepsilon }^*\left(k_3\right)\right) g_s^2 f^{\text{Glu1}\;\text{Glu3}\;\text{Glu5}} f^{\text{Glu2}\;\text{Glu4}\;\text{Glu5}}}{(\overline{k_4}-\overline{k_2}){}^2}-\frac{\left(\overline{k_2}\cdot \bar{\varepsilon }\left(k_1\right)+\overline{k_3}\cdot \bar{\varepsilon }\left(k_1\right)-\overline{k_4}\cdot \bar{\varepsilon }\left(k_1\right)\right) \left(-\left(\overline{k_2}\cdot \bar{\varepsilon }^*\left(k_3\right)\right)-\overline{k_4}\cdot \bar{\varepsilon }^*\left(k_3\right)\right) \left(\bar{\varepsilon }\left(k_2\right)\cdot \bar{\varepsilon }^*\left(k_4\right)\right) g_s^2 f^{\text{Glu1}\;\text{Glu3}\;\text{Glu5}} f^{\text{Glu2}\;\text{Glu4}\;\text{Glu5}}}{(\overline{k_4}-\overline{k_2}){}^2}-\frac{\left(-\left(\overline{k_2}\cdot \bar{\varepsilon }\left(k_1\right)\right)-\overline{k_4}\cdot \bar{\varepsilon }\left(k_1\right)\right) \left(\overline{k_1}\cdot \bar{\varepsilon }^*\left(k_3\right)-\overline{k_2}\cdot \bar{\varepsilon }^*\left(k_3\right)+\overline{k_4}\cdot \bar{\varepsilon }^*\left(k_3\right)\right) \left(\bar{\varepsilon }\left(k_2\right)\cdot \bar{\varepsilon }^*\left(k_4\right)\right) g_s^2 f^{\text{Glu1}\;\text{Glu3}\;\text{Glu5}} f^{\text{Glu2}\;\text{Glu4}\;\text{Glu5}}}{(\overline{k_4}-\overline{k_2}){}^2}-\frac{\left(\overline{k_1}\cdot \overline{k_2}+\overline{k_1}\cdot \overline{k_4}+\overline{k_2}\cdot \overline{k_3}+\overline{k_3}\cdot \overline{k_4}\right) \left(\bar{\varepsilon }\left(k_1\right)\cdot \bar{\varepsilon }^*\left(k_3\right)\right) \left(\bar{\varepsilon }\left(k_2\right)\cdot \bar{\varepsilon }^*\left(k_4\right)\right) g_s^2 f^{\text{Glu1}\;\text{Glu3}\;\text{Glu5}} f^{\text{Glu2}\;\text{Glu4}\;\text{Glu5}}}{(\overline{k_4}-\overline{k_2}){}^2}-\frac{2 \left(\overline{k_2}\cdot \bar{\varepsilon }\left(k_1\right)+\overline{k_3}\cdot \bar{\varepsilon }\left(k_1\right)-\overline{k_4}\cdot \bar{\varepsilon }\left(k_1\right)\right) \left(\overline{k_4}\cdot \bar{\varepsilon }\left(k_2\right)\right) \left(\bar{\varepsilon }^*\left(k_3\right)\cdot \bar{\varepsilon }^*\left(k_4\right)\right) g_s^2 f^{\text{Glu1}\;\text{Glu3}\;\text{Glu5}} f^{\text{Glu2}\;\text{Glu4}\;\text{Glu5}}}{(\overline{k_4}-\overline{k_2}){}^2},-\frac{2 \left(\overline{k_2}\cdot \bar{\varepsilon }^*\left(k_3\right)\right) \left(\overline{k_1}\cdot \bar{\varepsilon }^*\left(k_4\right)-\overline{k_2}\cdot \bar{\varepsilon }^*\left(k_4\right)+\overline{k_3}\cdot \bar{\varepsilon }^*\left(k_4\right)\right) \left(\bar{\varepsilon }\left(k_1\right)\cdot \bar{\varepsilon }\left(k_2\right)\right) g_s^2 f^{\text{Glu1}\;\text{Glu4}\;\text{Glu5}} f^{\text{Glu2}\;\text{Glu3}\;\text{Glu5}}}{(\overline{k_3}-\overline{k_2}){}^2}-\frac{2 \left(\overline{k_3}\cdot \bar{\varepsilon }\left(k_2\right)\right) \left(\overline{k_1}\cdot \bar{\varepsilon }^*\left(k_4\right)-\overline{k_2}\cdot \bar{\varepsilon }^*\left(k_4\right)+\overline{k_3}\cdot \bar{\varepsilon }^*\left(k_4\right)\right) \left(\bar{\varepsilon }\left(k_1\right)\cdot \bar{\varepsilon }^*\left(k_3\right)\right) g_s^2 f^{\text{Glu1}\;\text{Glu4}\;\text{Glu5}} f^{\text{Glu2}\;\text{Glu3}\;\text{Glu5}}}{(\overline{k_3}-\overline{k_2}){}^2}-\frac{2 \left(\overline{k_2}\cdot \bar{\varepsilon }^*\left(k_3\right)\right) \left(-\left(\overline{k_1}\cdot \bar{\varepsilon }\left(k_2\right)\right)-\overline{k_4}\cdot \bar{\varepsilon }\left(k_2\right)\right) \left(\bar{\varepsilon }\left(k_1\right)\cdot \bar{\varepsilon }^*\left(k_4\right)\right) g_s^2 f^{\text{Glu1}\;\text{Glu4}\;\text{Glu5}} f^{\text{Glu2}\;\text{Glu3}\;\text{Glu5}}}{(\overline{k_3}-\overline{k_2}){}^2}-\frac{2 \left(\overline{k_3}\cdot \bar{\varepsilon }\left(k_2\right)\right) \left(-\left(\overline{k_1}\cdot \bar{\varepsilon }^*\left(k_3\right)\right)-\overline{k_4}\cdot \bar{\varepsilon }^*\left(k_3\right)\right) \left(\bar{\varepsilon }\left(k_1\right)\cdot \bar{\varepsilon }^*\left(k_4\right)\right) g_s^2 f^{\text{Glu1}\;\text{Glu4}\;\text{Glu5}} f^{\text{Glu2}\;\text{Glu3}\;\text{Glu5}}}{(\overline{k_3}-\overline{k_2}){}^2}-\frac{\left(-\left(\overline{k_2}\cdot \bar{\varepsilon }\left(k_1\right)\right)-\overline{k_3}\cdot \bar{\varepsilon }\left(k_1\right)\right) \left(\overline{k_1}\cdot \bar{\varepsilon }^*\left(k_4\right)-\overline{k_2}\cdot \bar{\varepsilon }^*\left(k_4\right)+\overline{k_3}\cdot \bar{\varepsilon }^*\left(k_4\right)\right) \left(\bar{\varepsilon }\left(k_2\right)\cdot \bar{\varepsilon }^*\left(k_3\right)\right) g_s^2 f^{\text{Glu1}\;\text{Glu4}\;\text{Glu5}} f^{\text{Glu2}\;\text{Glu3}\;\text{Glu5}}}{(\overline{k_3}-\overline{k_2}){}^2}-\frac{\left(-\left(\overline{k_2}\cdot \bar{\varepsilon }^*\left(k_4\right)\right)-\overline{k_3}\cdot \bar{\varepsilon }^*\left(k_4\right)\right) \left(\overline{k_2}\cdot \bar{\varepsilon }\left(k_1\right)-\overline{k_3}\cdot \bar{\varepsilon }\left(k_1\right)+\overline{k_4}\cdot \bar{\varepsilon }\left(k_1\right)\right) \left(\bar{\varepsilon }\left(k_2\right)\cdot \bar{\varepsilon }^*\left(k_3\right)\right) g_s^2 f^{\text{Glu1}\;\text{Glu4}\;\text{Glu5}} f^{\text{Glu2}\;\text{Glu3}\;\text{Glu5}}}{(\overline{k_3}-\overline{k_2}){}^2}-\frac{\left(\overline{k_1}\cdot \overline{k_2}+\overline{k_1}\cdot \overline{k_3}+\overline{k_2}\cdot \overline{k_4}+\overline{k_3}\cdot \overline{k_4}\right) \left(\bar{\varepsilon }\left(k_1\right)\cdot \bar{\varepsilon }^*\left(k_4\right)\right) \left(\bar{\varepsilon }\left(k_2\right)\cdot \bar{\varepsilon }^*\left(k_3\right)\right) g_s^2 f^{\text{Glu1}\;\text{Glu4}\;\text{Glu5}} f^{\text{Glu2}\;\text{Glu3}\;\text{Glu5}}}{(\overline{k_3}-\overline{k_2}){}^2}-\frac{2 \left(\overline{k_2}\cdot \bar{\varepsilon }^*\left(k_3\right)\right) \left(\overline{k_2}\cdot \bar{\varepsilon }\left(k_1\right)-\overline{k_3}\cdot \bar{\varepsilon }\left(k_1\right)+\overline{k_4}\cdot \bar{\varepsilon }\left(k_1\right)\right) \left(\bar{\varepsilon }\left(k_2\right)\cdot \bar{\varepsilon }^*\left(k_4\right)\right) g_s^2 f^{\text{Glu1}\;\text{Glu4}\;\text{Glu5}} f^{\text{Glu2}\;\text{Glu3}\;\text{Glu5}}}{(\overline{k_3}-\overline{k_2}){}^2}-\frac{2 \left(\overline{k_3}\cdot \bar{\varepsilon }\left(k_2\right)\right) \left(\overline{k_2}\cdot \bar{\varepsilon }\left(k_1\right)-\overline{k_3}\cdot \bar{\varepsilon }\left(k_1\right)+\overline{k_4}\cdot \bar{\varepsilon }\left(k_1\right)\right) \left(\bar{\varepsilon }^*\left(k_3\right)\cdot \bar{\varepsilon }^*\left(k_4\right)\right) g_s^2 f^{\text{Glu1}\;\text{Glu4}\;\text{Glu5}} f^{\text{Glu2}\;\text{Glu3}\;\text{Glu5}}}{(\overline{k_3}-\overline{k_2}){}^2}\right\}$$

## Fix the kinematics

```mathematica
FCClearScalarProducts[];
SetMandelstam[s, t, u, k1, k2, -k3, -k4, 0, 0, 0, 0];
```

## Square the amplitude

```mathematica
polsums[x_, vec_, aux_, spinfac_] := x // Collect2[#, Pair[_, 
         Momentum[Polarization[vec, __]]]] & // Isolate[#, {Polarization[vec, __]}] & // 
    DoPolarizationSums[#, vec, aux, ExtraFactor -> spinfac] & // FixedPoint[ReleaseHold, #] &
```

```mathematica
ClearAll[re];
Table[Print["    calculating color factors in products of the amplitudes ", i, 
    " and ", j, " (CC), time = ", 
    Timing[re[i, j] = (amp[0][[i]] ComplexConjugate[amp[0]][[j]] // 
          FeynAmpDenominatorExplicit // 
         SUNSimplify[#, Explicit -> True, SUNNToCACF -> False] &)][[1]]]; re[i, j], {i, 4}, {j, i}];
```

$$\text{    calculating color factors in products of the amplitudes }1\text{ and }1\text{ (CC), time = }0.235391$$

$$\text{    calculating color factors in products of the amplitudes }2\text{ and }1\text{ (CC), time = }0.194861$$

$$\text{    calculating color factors in products of the amplitudes }2\text{ and }2\text{ (CC), time = }0.130752$$

$$\text{    calculating color factors in products of the amplitudes }3\text{ and }1\text{ (CC), time = }0.185037$$

$$\text{    calculating color factors in products of the amplitudes }3\text{ and }2\text{ (CC), time = }0.138698$$

$$\text{    calculating color factors in products of the amplitudes }3\text{ and }3\text{ (CC), time = }0.132859$$

$$\text{    calculating color factors in products of the amplitudes }4\text{ and }1\text{ (CC), time = }0.185816$$

$$\text{    calculating color factors in products of the amplitudes }4\text{ and }2\text{ (CC), time = }0.133119$$

$$\text{    calculating color factors in products of the amplitudes }4\text{ and }3\text{ (CC), time = }0.133023$$

$$\text{    calculating color factors in products of the amplitudes }4\text{ and }4\text{ (CC), time = }0.13281$$

```mathematica
ClearAll[pre];
Table[Print["    calculating product of the amplitudes ", i, " and ", j, 
    " (CC), time = ", Timing[pre[i, j] = re[i, j] // polsums[#, k1, k2, 
              1/2] & // polsums[#, k2, k1, 1/2] & // polsums[#, k3, k4, 1] & // 
         polsums[#, k4, k3, 1] & // Simplify][[1]]]; pre[i, j], {i, 4}, {j, i}];
```

$$\text{    calculating product of the amplitudes }1\text{ and }1\text{ (CC), time = }0.496876$$

$$\text{    calculating product of the amplitudes }2\text{ and }1\text{ (CC), time = }0.557545$$

$$\text{    calculating product of the amplitudes }2\text{ and }2\text{ (CC), time = }1.30451$$

$$\text{    calculating product of the amplitudes }3\text{ and }1\text{ (CC), time = }0.959513$$

$$\text{    calculating product of the amplitudes }3\text{ and }2\text{ (CC), time = }2.00888$$

$$\text{    calculating product of the amplitudes }3\text{ and }3\text{ (CC), time = }1.78583$$

$$\text{    calculating product of the amplitudes }4\text{ and }1\text{ (CC), time = }0.884629$$

$$\text{    calculating product of the amplitudes }4\text{ and }2\text{ (CC), time = }1.71058$$

$$\text{    calculating product of the amplitudes }4\text{ and }3\text{ (CC), time = }1.9904$$

$$\text{    calculating product of the amplitudes }4\text{ and }4\text{ (CC), time = }1.79517$$

```mathematica
fpre[i_, j_] := pre[i, j] /; (i >= j);
fpre[i_, j_] := ComplexConjugate[pre[j, i]] /; (i < j);
ampSquared[0] = 1/((SUNN^2 - 1)^2) (Sum[fpre[i, j], {i, 1, 4}, {j, 1, 4}]) // 
    TrickMandelstam[#, {s, t, u, 0}] & // Simplify
```

$$\frac{4 N^2 g_s^4 \left(t^2+t u+u^2\right)^3}{\left(N^2-1\right) s^2 t^2 u^2}$$

```mathematica
ampSquaredSUNN3[0] = ampSquared[0] /. SUNN -> 3
```

$$\frac{9 g_s^4 \left(t^2+t u+u^2\right)^3}{2 s^2 t^2 u^2}$$

```mathematica
ampSquaredMassless[0] = ampSquared[0] // ReplaceAll[#, {SMP["m_u"] -> 0}] & // 
  	TrickMandelstam[#, {s, t, u, 0}] &
```

$$-\frac{4 N^2 g_s^4 \left(t^2+t u+u^2\right)^3}{\left(1-N^2\right) s^2 t^2 u^2}$$

```mathematica
ampSquaredMasslessSUNN3[0] = ampSquaredMassless[0] /. SUNN -> 3
```

$$\frac{9 g_s^4 \left(t^2+t u+u^2\right)^3}{2 s^2 t^2 u^2}$$

## Check the final results

```mathematica
knownResults = {
   	(9/2) SMP["g_s"]^4 (3 - t u/s^2 - s u/t^2 - s t/u^2) 
   };
FCCompareResults[{ampSquaredMasslessSUNN3[0]}, {knownResults}, 
  Text -> {"\tCompare to Ellis, Stirling and Weber, QCD and Collider Physics, Table 7.1:", "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}, Factoring -> 
   Function[x, Simplify[TrickMandelstam[x, {s, t, u, 0}]]]]
Print["\tCPU Time used: ", Round[N[TimeUsed[], 3], 0.001], " s."];

```mathematica

$$\text{$\backslash $tCompare to Ellis, Stirling and Weber, QCD and Collider Physics, Table 7.1:} \;\text{CORRECT.}$$

$$\text{True}$$

$$\text{$\backslash $tCPU Time used: }44.42\text{ s.}$$