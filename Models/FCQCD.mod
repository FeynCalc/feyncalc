(*
*)
M$ClassesDescription = M$CouplingMatrices = {};

IndexRange[ Index[Gluon] ] = NoUnfold[Range[8]]

M$ClassesDescription = Join[ M$ClassesDescription, {

  V[5] == {
	SelfConjugate -> True,
	Indices -> {Index[Gluon]},
	Mass -> 0,
	PropagatorLabel -> "",
	PropagatorType -> Cycles,
	PropagatorArrow -> None },

 F[3] == {
        SelfConjugate -> False,
        Indices -> {},
        Mass -> MQU,
        QuantumNumbers -> 2/3 Charge,
        PropagatorLabel -> "",
        PropagatorType -> Straight,
        PropagatorArrow -> Forward },

  U[5] == {
	SelfConjugate -> False,
	Indices -> {Index[Gluon]},
	Mass -> 0,
	QuantumNumbers -> GhostNumber,
	PropagatorLabel -> ""(* ComposedChar["u", "g"]*),
	PropagatorType -> GhostDash,
	PropagatorArrow -> Forward }
} ]

M$CouplingMatrices = Join[ M$CouplingMatrices, {

(*--- gluon-gluon-gluon-gluon ------------------------------------------*)

  C[ V[5, {g1}], V[5, {g2}], V[5, {g3}], V[5, {g4}] ] == 1*
    { {1} },


(*--- gluon-gluon-gluon ------------------------------------------------*)

  C[ V[5, {g1}], V[5, {g2}], V[5, {g3}] ] == 1 *
    { {1} },


(*--- ghost-ghost-gluon ------------------------------------------------*)

  C[ -U[5, {g1}], U[5, {g2}], V[5, {g3}] ] == 1 * 
    { {1} },


(*--- quark-quark-gluon ------------------------------------------------*)

  C[ -F[3], F[3], V[5, {g1}] ] == 1 *
    { {1} }

} ]

(***********************************************************************)
