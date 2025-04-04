(* ::Package:: *)

 


(* ::Section:: *)
(*ApartFF*)


(* ::Text:: *)
(*`ApartFF[amp, {q1, q2, ...}]` partial fractions loop integrals by decomposing them into simpler integrals that contain only linearly independent propagators. It uses `FCApart` as a backend and is equally suitable for 1-loop and  multi-loop integrals.*)


(* ::Text:: *)
(*`FCApart`  implements an algorithm based on [arXiv:1204.2314](https://arxiv.org/abs/1204.2314) by F. Feng that seems to employ a variety Leinartas's algorithm (cf. [arXiv:1206.4740](https://arxiv.org/abs/1206.4740)). Unlike Feng's [$Apart](https://github.com/F-Feng/APart) that is applicable to general multivariate polynomials, `FCApart` is tailored to work only with FeynCalc's `FeynAmpDenominator`, `Pair` and `CartesianPair` symbols, i.e. it is less general in this respect.*)


(* ::Text:: *)
(*`ApartFF[amp * extraPiece1, extraPiece2, {q1, q2, ...}]` is a special working mode of `ApartFF`, where the final result of partial fractioning `amp*extraPiece1` is multiplied by `extraPiece2`. It is understood, that `extraPiece1*extraPiece2` should be unity, e. g. when `extraPiece1` is an `FAD`, while extraPiece is an `SPD` inverse to it. This mode should be useful for nonstandard integrals where the desired partial fraction decomposition can be performed only after multiplying `amp` with `extraPiece1`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCApart](FCApart.md), [FeynAmpDenominatorSimplify](FeynAmpDenominatorSimplify.md), [FCLoopFindTensorBasis](FCLoopFindTensorBasis.md).*)


(* ::Subsection:: *)
(*Examples*)


FCClearScalarProducts[]


SPD[q,q]FAD[{q,m}]

ApartFF[%,{q}]


SPD[q,p]SPD[q,r]FAD[{q},{q-p},{q-r}]

ApartFF[%,{q}]


FAD[{q},{q-p},{q+p}]

ApartFF[%,{q}]


SPD[p,q1]SPD[p,q2]^2FAD[{q1,m},{q2,m},q1-p,q2-p,q1-q2]

ApartFF[%,{q1,q2}]


SPD[q,p] FAD[{q,m},{q-p,0}]

ApartFF[%,{q}]


(* ::Text:: *)
(*If the propagators should not be altered via momentum shifts (e.g. because they belong to a previously identified topology), use the option `FDS->False`*)


int=SPD[q2,p]SPD[q1,p] FAD[{q1,m},{q2,m},q1-p,q2-p,q2-q1]


ApartFF[int,{q1,q2}]


ApartFF[int,{q1,q2},FDS->False]


(* ::Text:: *)
(*If the partial fractioning should be performed only w. r. t. the denominators but not numerators, use the option `Numerator->False`*)


int=FAD[k,p-k,{k,m}]SPD[p,k]


ApartFF[int,{k}]


ApartFF[int,{k},Numerator->False]


(* ::Text:: *)
(*Using the option `FeynAmpDenominator ->False` we can specify that integrals without numerators should not be partial fractioned*)


int=FAD[k,p-k,{k,m}](SPD[q]+SPD[p,k])


ApartFF[int,{k}]


ApartFF[int,{k},FeynAmpDenominator->False]


(* ::Text:: *)
(*The `extraPiece`-trick is useful for cases where a direct partial fractioning is not possible*)


int=(SFAD[{{0,k . l}},p-k]SPD[k,p])


(* ::Text:: *)
(*Here `ApartFF` cannot do anything*)


ApartFF[int,{k}]


(* ::Text:: *)
(*Multiplying the integral with unity `FAD[k]*SPD[k]` we can cast into a more desirable form*)


ApartFF[int FAD[k],SPD[k],{k}]//ApartFF[#,{k}]&


(* ::Text:: *)
(*Here we need a second call to `ApartFF` since the first execution doesn't drop scaleless integrals or perform any shifts in the denominators.*)


(* ::Text:: *)
(*Other examples of doing partial fraction decomposition for eikonal integrals (e.g. in SCET) *)


int1=SFAD[{{0,nb . k2}},{{0,nb . (k1+k2)}}]


ApartFF[int1,{k1,k2}]


ApartFF[ApartFF[SFAD[{{0,nb . k1}}]int1,SPD[nb,k1],{k1,k2}],{k1,k2}]


int2=SPD[nb,k2]SFAD[{{0,nb . (k1+k2)}}]


ApartFF[int2,{k1,k2}]


ApartFF[ApartFF[SFAD[{{0,nb . k1}}]int2,SPD[nb,k1],{k1,k2}],{k1,k2}]


(* ::Text:: *)
(*If we are working with a subset of propagators from a full integral, one should better turn off loop momentum shifts and the dropping of scaleless integrals*)


ApartFF[ApartFF[SFAD[{{0,nb . k1}}]int2,SPD[nb,k1],{k1,k2}],{k1,k2},FDS->False,
DropScaleless->False]


(* ::Text:: *)
(*When we need to deal with linearly dependent external momenta, there might be some relations between scalar products involving those momenta and loop momenta. Since the routine cannot determine those relations automatically, we need to supply them by hand via the option `FinalSubstitutions`*)


FCClearScalarProducts[];
int3 = SPD[p1,q]FAD[{q,m},{q-p1-p2}]


(* ::Text:: *)
(*Supplying the kinematics alone doesn't work*)


kinRules = {SPD[p1]->M^2,SPD[p2]->M^2,SPD[p1,p2]->M^2}


ApartFF[SPD[p1,q]FAD[{q,m},{q-p1-p2}],{q},FinalSubstitutions->kinRules]


(* ::Text:: *)
(*Using `FCLoopFindTensorBasis` we see that `p2` is proportional to `p1`. Hence, we have an equality between `p1.q` and `p2.q`*)


FCLoopFindTensorBasis[{p1,p2},kinRules,n]


(* ::Text:: *)
(*Supplying this information to `ApartFF` we can finally achieve the desired simplification*)


ApartFF[SPD[p1,q]FAD[{q,m},{q-p1-p2}],{q},FinalSubstitutions->Join[kinRules,
{SPD[q,p2]->SPD[q,p1]}]]
