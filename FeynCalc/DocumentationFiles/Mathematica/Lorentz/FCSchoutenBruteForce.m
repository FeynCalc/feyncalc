 
(* ::Section:: *)
(* FCSchoutenBruteForce *)
(* ::Text:: *)
(*FCSchoutenBruteForce[exp, {}, {}]  can be used to show that certain terms are zero by repeatedly Schouten identity in a brute force way. The algorithm tries to find replacements which follow from the Schouten identity and make the length of the given expression shorter. It is not guaranteed to terminate and in general can often get stucked. Still, with some luck it is often possible to show that certain terms vanish by a sequence of transformations that would be otherwise very difficult to find..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*One may not recognize it easily, but the following expression is zero by Schouten's identity*)



(* ::Subsection:: *)
(* Examples *)



FCClearScalarProducts[]
LC[][p1,p2,p3,p4] SP[p5,p6]+LC[][p2,p3,p4,p5] SP[p1,p6]+LC[][p3,p4,p5,p1] SP[p2,p6]+LC[][p4,p5,p1,p2] SP[p3,p6]-LC[][p1,p2,p3,p5] SP[p4,p6]

FCSchoutenBruteForce[%,{},{}]
FCSchoutenBruteForce: The following rule was applied: Overscript[\[Epsilon], _]^(Overscript[p2, _]Overscript[p3, _]Overscript[p4, _]Overscript[p5, _]) (Overscript[p1, _]\[CenterDot]Overscript[p6, _]):>Overscript[\[Epsilon], _]^(Overscript[p1, _]Overscript[p3, _]Overscript[p4, _]Overscript[p5, _]) (Overscript[p2, _]\[CenterDot]Overscript[p6, _])-Overscript[\[Epsilon], _]^(Overscript[p1, _]Overscript[p2, _]Overscript[p4, _]Overscript[p5, _]) (Overscript[p3, _]\[CenterDot]Overscript[p6, _])+Overscript[\[Epsilon], _]^(Overscript[p1, _]Overscript[p2, _]Overscript[p3, _]Overscript[p5, _]) (Overscript[p4, _]\[CenterDot]Overscript[p6, _])-Overscript[\[Epsilon], _]^(Overscript[p1, _]Overscript[p2, _]Overscript[p3, _]Overscript[p4, _]) (Overscript[p5, _]\[CenterDot]Overscript[p6, _]) 
FCSchoutenBruteForce: The numbers of terms in the expression decreased by: 5
FCSchoutenBruteForce: Current length of the expression: 0
