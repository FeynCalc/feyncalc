 
(* ::Section:: *)
(* SetMandelstam *)
(* ::Text:: *)
(*SetMandelstam[s, t, u, p , p , p , p , m , m , m , m ] defines the Mandelstam variables  $s=\left(p_1+p_2\right){}^2$, $t=\left(p_1+p_3\right){}^2$, $u=\left(p_1+p_4\right){}^2$ and sets the momenta on-shell: $p_1{}^2=m_1{}^2$, $p_2{}^2=m_2{}^2$, $p_3{}^2=m_3{}^2$, $p_4{}^2=m_4{}^2$. Notice that $p_1+p_2+p_3+p_4=0$ is assumed..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*Mandelstam.*)



(* ::Subsection:: *)
(* Examples *)
(* ::Text:: *)
(*SetMandelstam assumes all momenta to be ingoing. For scattering processes with p1+p2=p3+p4, the outgoing momenta should be written with a minus sign.*)


FCClearScalarProducts[]
SetMandelstam[s,t,u,p1,p2,-p3,-p4,m1,m2,m3,m4]

SP[p1,p2]
SP[p1,p3]
SP[p1,p4]




(* ::Text:: *)
(*SetMandelstam simultaneously sets scalar products in 4 and D dimensions. This is controlled by the option Dimension.*)


SPD[p1,p2]
SPD[p1,p3]



(* ::Text:: *)
(*It is also possible to have more than just 4 momenta. For example, for p1+p2=p3+p4+p5 we can obtain x[i, j] = (pi+pj)^2*)


FCClearScalarProducts[];
SetMandelstam[x, {p1, p2, -p3, -p4, -p5}, {m1, m2, m3, m4, m5}] 

SPD[p4,p5]
