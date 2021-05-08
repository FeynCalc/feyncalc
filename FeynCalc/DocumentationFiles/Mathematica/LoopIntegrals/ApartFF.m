 
(* ::Section:: *)
(* ApartFF *)
(* ::Text:: *)
(*ApartFF[amp, {q1, q2, ...}] partial fractions loop integrals by decomposing them into simpler integrals that contain only linearly independent propagators. It uses FCApart as a backend and works and is suitable also for multiloop integrals.ApartFF uses FCApart as the backend for partial fractioning. The latter implements an algorithm which is very much based on the work and code of F. Feng (arXiv:1204.2314) that seems	to employ a variety Leinartas's algorithm (see arXiv:1206.4740 for its description). Unlike Feng's $Apart that works on general multivariate polynomials,	 FCApart is tailored to work only with FeynCalc's FeynAmpDenominator and Pair objects, i.e. it is less general. For the original $Apart see https://github.com/F-Feng/APartApartFF[amp * extraPiece1, extraPiece2, {q1, q2, ...}] is a special working mode of ApartFF, where the final result of partial fractioning amp*extraPiece1 is multiplied by extraPiece2. It is understood, that extraPiece1*extraPiece2 should be unity, e.g. when extraPiece1 is an FAD, while extraPiece is an SPD inverse to it. This mode should be useful for nonstandard integrals where the desired partial fraction decomposition can be performed only after multiplying amp with extraPiece1.*)


(* ::Subsection:: *)
(* Examples *)
FCApart
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
(*If the propagators should not altered via momentum shifts (e.g. because they belong to a previously identified topology), use $text{FDS}to text{False}$*)


ApartFF[%%,{q},FDS->False]

SPD[q2,p]SPD[q1,p] FAD[{q1,m},{q2,m},q1-p,q2-p,q2-q1]

ApartFF[%,{q1,q2}]

ApartFF[%%,{q1,q2},FDS->False]


(* ::Text:: *)
(*If the partial fractioning should be performed only w. r. t. the denominators but not numerators, use $text{Numerator}to text{False}$*)


int=FAD[k,p-k,{k,m}]SPD[p,k]

ApartFF[int,{k}]

ApartFF[int,{k},Numerator->False]

ClearAll[int]

(* ::Text:: *)
(*Using $text{FeynAmpDenominator}to text{False}$ we can specify that integrals without numerators should not be partial fractioned*)


int=FAD[k,p-k,{k,m}](SPD[q]+SPD[p,k])

ApartFF[int,{k}]

ApartFF[int,{k},FeynAmpDenominator->False]

ClearAll[int]

(* ::Text:: *)
(*The extraPiece-trick is useful for cases where a direct partial fractioning is not possible*)


int=(SFAD[{{0,k.l}},p-k]SPD[k,p])


(* ::Text:: *)
(*Here ApartFF cannot do anything *)


ApartFF[int,{k}]


(* ::Text:: *)
(*Multiplying the integral with unity FAD[k]*SPD[k] we can cast into a more desirable form*)


ApartFF[int FAD[k],SPD[k],{k}]//ApartFF[#,{k}]&


(* ::Text:: *)
(*We need a second call to ApartFF since the first execution doesn't drop scaleless integrals or perform any shifts in the denominators.*)


ClearAll[int]