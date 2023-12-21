(* ::Package:: *)

 


(* ::Section:: *)
(*FCSetDiracGammaScheme*)


(* ::Text:: *)
(*`FCSetDiracGammaScheme[scheme]` allows you to specify how Dirac matrices will be handled in `D` dimensions. This is mainly relevant to the treatment of the 5th Dirac matrix $\gamma^5$, which is not well-defined in dimensional regularization.*)


(* ::Text:: *)
(*Following schemes are supported:*)


(* ::Text:: *)
(*"NDR" - This is the default value. In the naive dimensional regularization (also known as conventional dimensional regularization or CDR) $\gamma^5$ is assumed to anticommute with all Dirac matrices in $D$ dimensions. Hence, every Dirac trace can be rewritten in such a way, that it contains either just one or not a single $\gamma^5$ matrix. The latter traces are obviously unambiguous. The traces with one $\gamma^5$ are not well-defined in this scheme. It usually depends on the physics of the process, whether and how they can contribute to the final result. Therefore, FeynCalc will keep such traces unevaluated, leaving it to the user to decide how to treat them. Notice that traces with an odd number of the usual Dirac matrices and one $\gamma^5$, that vanish in $4$ dimensions, will be also put to zero in this scheme.*)


(* ::Text:: *)
(*"NDR-Discard" - This is a special version of the NDR scheme. The Dirac algebra is evaluated in the same way as with "NDR", but the remaining traces with one $\gamma^5$ are put to zero. This assumes that such traces do not contribute to the final result, which is obviously true only for specific calculations.*)


(* ::Text:: *)
(*"BMHV" - The Breitenlohner-Maison extension of the t'Hooft-Veltman scheme. This scheme introduces Dirac and Lorentz tensors living in $4$, $D$ or $D-4$ dimensions, while $\gamma^5$ is a purely $4$-dimensional object. BMHV is algebraically consistent but often suffers from nonconservation of currents in the final results. The conservation must be then enforced by introducing finite counter-terms. The counter-terms are to be supplied by the user, since FeynCalc does not do this automatically.*)


(* ::Text:: *)
(*"Larin" - Special prescription developed by S. Larin, also known as the Larin-Gorishny-Atkyampo-DelBurgo scheme. Essentially, it is a shortcut (mostly used in QCD) for obtaining the same results as in BMHV but without the necessity to deal with tensors from different dimensions. In this scheme $\gamma^5$ is treated as nonanticommuting, while Dirac traces are still cyclic. If a chain of Dirac matrices contains a single $\gamma^5$, it is essentially left untouched. When computing the trace of such a chain, the cyclicity is used to put $\gamma^5$ to the very end of the chain. Then, the trace is evaluated using the Moch-Vermaseren-Vogt formula, Eq.(10) from [arXiv:1506.04517](https://arxiv.org/pdf/1506.04517.pdf). If a chain contains more than one $\gamma^5$, all but one $\gamma^5$ will be eliminated using the replacement $\gamma_\mu \gamma^5 \to i/6 \varepsilon_{\mu \nu \rho \sigma} \gamma^\nu \gamma^\rho \gamma^\sigma$. This way every trace with multiple occurrences of $\gamma^5$ can be converted to a linear combination of traces with a single $\gamma^5$. Such traces are then treated as described above. Notice that Levi-Civita tensors generated during the calculation of traces are $D$-dimensional. For example, a product of two such tensors with all their indices contracted yields a polynomial in $D$'s. This scheme is often used for performance reasons and is assumed to give the same results as the BMHV scheme. However, this is not a rigorous statement and so when in doubt it might be better to use BMHV instead.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCGetDiracGammaScheme](FCGetDiracGammaScheme.md), [DiracTrace](DiracTrace.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*In NDR chiral traces remain unevaluated. You decide how to treat them.*)


FCSetDiracGammaScheme["NDR"]

DiracTrace[GAD[\[Mu],\[Nu],\[Rho],\[Sigma],\[Tau],\[Kappa],5]]

DiracSimplify[%]


(* ::Text:: *)
(*If you know that such traces do not contribute, use NDR-Discard scheme to put them to zero*)


FCSetDiracGammaScheme["NDR-Discard"]

DiracSimplify[DiracTrace[GAD[\[Mu],\[Nu],\[Rho],\[Sigma],\[Tau],\[Kappa],5]]]


(* ::Text:: *)
(*In BMHV chiral traces are algebraically well-defined*)


FCSetDiracGammaScheme["BMHV"]

res1=DiracSimplify[DiracTrace[GAD[\[Mu],\[Nu],\[Rho],\[Sigma],\[Tau],\[Kappa],5]]]


(* ::Text:: *)
(*Larin's scheme reproduces the results of the BMHV scheme, but this may not be immediately obvious*)


FCSetDiracGammaScheme["Larin"]

res2=DiracSimplify[DiracTrace[GAD[\[Mu],\[Nu],\[Rho],\[Sigma],\[Tau],\[Kappa],5]]]


(* ::Text:: *)
(*Owing to Schouten identities, proving the equivalence of chiral traces is not so simple, especially for many terms. `FCSchoutenBruteForce` can be helpful here*)


diff=ChangeDimension[res1-res2,D]

Contract[FV[p1,\[Mu]]FV[p2,\[Nu]]FV[p3,\[Rho]]FV[p4,\[Sigma]]FV[p5,\[Tau]]FV[p6,\[Kappa]]diff]

FCSchoutenBruteForce[%,{},{}]


FCSetDiracGammaScheme["NDR"]
