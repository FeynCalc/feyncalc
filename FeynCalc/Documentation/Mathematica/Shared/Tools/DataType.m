(* ::Package:: *)

 


(* ::Section:: *)
(*DataType*)


(* ::Text:: *)
(*`DataType[exp, type] = True` defines the object `exp` to have data-type `type`.*)


(* ::Text:: *)
(*`DataType[exp1, exp2, ..., type]` defines the objects `exp1, exp2, ...` to have data-type `type`.*)


(* ::Text:: *)
(*The default setting is `DataType[__, _] := False`.*)


(* ::Text:: *)
(*To assign a certain data-type, do, e.g., `DataType[x, PositiveInteger] = True`. Currently used `DataTypes`: *)


(* ::Text:: *)
(*- `NonCommutative`*)


(* ::Text:: *)
(*- `PositiveInteger`*)


(* ::Text:: *)
(*- `NegativeInteger`*)


(* ::Text:: *)
(*- `PositiveNumber`*)


(* ::Text:: *)
(*- `FreeIndex`*)


(* ::Text:: *)
(*- `GrassmannParity`*)


(* ::Text:: *)
(*If loaded, PHI adds the `DataTypes`: `UMatrix`, `UScalar`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DeclareNonCommutative](DeclareNonCommutative.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*`NonCommutative` is just a data-type.*)


DataType[f,g, NonCommutative] = True;
t=f . g-g . (2a) . f


(* ::Text:: *)
(*Since `f`"and `g` have `DataType` `NonCommutative`, the function `DotSimplify` extracts only `a` out of the noncommutative product.*)


DotSimplify[t]


DataType[m,odd]=DataType[a,even]=True;
ptest1[x_]:=x/.(-1)^n_/;DataType[n,odd]:>-1;
ptest2[x_]:=x/.(-1)^n_/;DataType[n,even]:>1;
t=(-1)^m+(-1)^a+(-1)^z


ptest1[t]
ptest2[%]


Clear[ptest1,ptest2,t,a,m];


DataType[m,integer]=True;
f[x_]:=x/.{(-1)^p_/;DataType[p,integer]:>1};


test=(-1)^m+(-1)^n x


f[test]


Clear[f,test];
DataType[f,g, NonCommutative] = False;
DataType[m,odd]=DataType[a,even]=False;


(* ::Text:: *)
(*Certain FeynCalc objects have `DataType` `PositiveInteger` set to `True`.*)


DataType[OPEm,PositiveInteger]


(* ::Text:: *)
(*PowerSimplify uses the DataType information.*)


PowerSimplify[ (-1)^(2OPEm)]


PowerSimplify[ (- SO[q])^OPEm]
