## Renormalization

### See also

[Overview](FeynCalc.md).

### Renormalization constants and mass dimensions

The procedure of renormalization allows us to get rid of the UV poles in a renormalizable theory. However, for that aim we need to know explicit values of the renormalization constants.

The first step is to rewrite the (bare) Lagrangian of our theory in terms of the renormalized Lagrangian $\mathcal{L}_{\textrm{ren}}$ and the counter-term Lagrangian $\mathcal{L}_{\textrm{ct}}$

\begin{equation}
\mathcal{L}  = \mathcal{L}_{\textrm{ren}} + \mathcal{L}_{\textrm{ct}}
\end{equation}

The renormalization constants $Z_x$ establish a connection between bare and renormalized quantities
appearing in the Lagrangian. For some generic  masses, coupling constants and fields we can write

\begin{align*}
g_{\textrm{bare}} &=  \mu^{n \varepsilon} Z_g g_{\textrm{ren}}, \\
m_{\textrm{bare}} &=  Z_m m_{\textrm{ren}}, \\
\psi_{\textrm{bare}} &=  \sqrt{Z_m} \psi_{\textrm{ren}}, \\
A^\mu_{\textrm{bare}} &=  \sqrt{Z_A} A^\mu_{\textrm{ren}}. \\
\end{align*}

The renormalization scale $\mu$ is needed to account for the fact, that when going from $4$ to $D$ dimensions, dimensionless coupling constants suddenly become dimensionful.

For example, a scalar field (mass dimension 1 in 4 dimensions) gets mass dimension $(D-2)/2$ in $D$ dimensions. This can be deduced by looking at the kinetic term and using that the mass dimension of the partial derivative remains unity,

\begin{align*}
[(\partial_\mu \phi_{\textrm{bare}})^2] &= D \Rightarrow [\partial_\mu \phi_{\textrm{bare}}] = \frac{D}{2},  \\
[\partial_\mu] & = 1 \Rightarrow [\phi_{\textrm{bare}}] = \frac{D-2}{2}
\end{align*}

Then for the  $\lambda \phi^4$-vertex we follow that

\begin{equation}
[\lambda_{\textrm{bare}} \phi_{\textrm{bare}}^4] = D \Rightarrow [\lambda_{\textrm{bare}}] = D - 4 \frac{D-2}{2} = 4 - D,
\end{equation}

For $D = 4- 2 \varepsilon$ this yields $[\lambda_{\textrm{bare}}] = 2 \varepsilon$, which is precisely compensated by imposing

\begin{equation}
\lambda_{\textrm{bare}} =  \mu^{2 \varepsilon} Z_\lambda \lambda_{\textrm{ren}},
\end{equation}

so that $[\lambda_{\textrm{ren}}] = 0$.

A similar exercise for the fermion yields

\begin{equation}
[\bar{\psi}_{\textrm{bare}} i \gamma \cdot \partial \psi_{\textrm{bare}} ] = D \Rightarrow [\psi_{\textrm{bare}}] = \frac{D-1}{2}
\end{equation}

The splitting of the bare Lagrangian $\mathcal{L}$ into $\mathcal{L}_{\textrm{ren}}$ and $\mathcal{L}_{\textrm{ct}}$
is done by first replacing all bare quantities by the renormalized ones and then using the trivial decompositions

\begin{align*}
\sqrt{Z_x} &= (\sqrt{Z_x} - 1) + 1, \\
Z_x &= (Z_x - 1) + 1, \\
Z_x^2 &= (Z_x^2 - 1) + 1, \\
Z_y Z_x^2 &= (Z_y Z_x^2 - 1) + 1. \\
\end{align*}

Each of the renormalization constants can be written as

\begin{equation}
Z_x = 1 + \delta Z_x,
\end{equation}

where $\delta Z_x$ contains poles in $\varepsilon$ and possibly also finite pieces (depending on the chosen renormalization scheme). Parametrically, $\delta Z_x$ is of order of the small coupling constant so that we can "expand" in it as if $\delta Z_x \ll 1$.

### Examples of renormalized and counter-term Lagrangians

In the following, for the sake of convenience we drop the subscript "ren" in the renormalized Lagrangian

In the case of the **real $\phi^4$-theory** (cf. e.g. [arXiv:1606.0921](https://arxiv.org/pdf/1606.09210.pdf)) we have

\begin{align*}
\mathcal{L}_{\phi^4} &= \frac{1}{2} \partial_\mu \phi_{\textrm{bare}} \partial^\mu \phi_{\textrm{bare}} - \frac{1}{2} m_{\textrm{bare}}^2 \phi_{\textrm{bare}}^2 - \frac{\lambda}{4!} \phi_{\textrm{bare}}^4 \\
& = \frac{1}{2} Z_\phi \partial_\mu \phi \partial^\mu \phi - \frac{1}{2} Z_m^2 Z_\phi m^2 \phi^2 - \mu^{2 \varepsilon} Z_\lambda Z_\phi^2 \frac{\lambda}{4!} \phi^4 \\
 & = \mathcal{L}_{\phi,\textrm{ren}} + \mathcal{L}_{\phi,\textrm{ct}}
\end{align*}

with

\begin{equation}
\mathcal{L}_{\phi,{\textrm{ren}}} = \frac{1}{2} \partial_\mu \phi \partial^\mu \phi - \frac{1}{2} m^2 \phi^2 - \frac{\lambda}{4!} \phi^4
\end{equation}

and

\begin{equation}
\mathcal{L}_{\phi,{\textrm{ct}}} = \frac{1}{2} (Z_\phi - 1) \partial_\mu \phi \partial^\mu \phi - \frac{1}{2} (Z_m^2 Z_\phi - 1) m^2 \phi^2 - (Z_\lambda Z_\phi^2 - 1) \frac{\lambda}{4!} \phi^4
\end{equation}

Another simple example is the **scalar Yukawa theory** with

\begin{equation}
\mathcal{L}_{Y,{\textrm{ren}}} = \bar{\psi} (i \gamma \cdot \partial - M) \psi + \frac{1}{2} \partial_\mu \phi \partial^\mu \phi - \frac{1}{2} m^2 \phi^2 - \frac{\lambda}{4!} \phi^4 - g \bar{\psi} \psi \phi
\end{equation}

and

\begin{align*}
\mathcal{L}_{Y,{\textrm{ct}}} &= (Z_\psi - 1) \bar{\psi} i \gamma \cdot \partial \psi - (Z_M Z_\psi - 1) M \bar{\psi} \psi + \frac{1}{2} (Z_\phi - 1) \partial_\mu \phi \partial^\mu \phi \\
&- \frac{1}{2} (Z_m Z_\phi -1) m^2 \phi^2 -  (Z_\lambda Z_\phi^2 -1) \frac{\lambda}{4!} \phi^4 - (Z_g Z_x \sqrt{Z_\phi} -1) g \bar{\psi} \psi \phi
\end{align*}

Finally, in the case of **QED** we have

\begin{equation}
  \mathcal{L}_{\textrm{QED},\textrm{ren}} = -\frac{1}{4} F_{\mu \nu}F^{\mu \nu} - \frac{1}{2 \xi}(\partial^\mu A_\mu)^2 +
  \bar{\psi} (i \gamma \cdot \partial -m) \psi + e  \bar{\psi} \gamma \cdot A \psi ,\\
\end{equation}

and

\begin{align*}
  \mathcal{L}_{\textrm{QED},\textrm{ct}}  & = - (Z_A-1) \frac{1}{4}F_{\mu \nu}F^{\mu \nu} - \frac{1}{2\xi} (Z_A Z^{-1}_\xi - 1) (\partial^\mu A_\mu)^2 \nonumber \\
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    & + (Z_\psi-1) \bar{\psi} i \gamma \cdot \partial \psi- (Z_\psi Z_m -1) m \bar{\psi} \psi + (Z_\psi \sqrt{Z_A} Z_e -1) e \bar{\psi} \gamma^\mu \psi A_\mu.
\end{align*}

Notice that Ward identities for the photon propagator and the electron-photon vertex link some of the renromalization constants to each other

\begin{equation}
Z_\xi = Z_A, \quad Z_e = 1/\sqrt{Z_A}.
\end{equation}

This way we only need to determine $Z_A$, $Z_\psi$ and $Z_m$, which can be done by looking at the self-energies of the electron and photon fields.

### Feynman rules

Having clarified the situation with the Lagrangian, let us discuss the derivation of the Feynman rules. The main difference as compared to the usual calculations is that here we also need to derive additional Feynman rules for the counter terms.

Although those can be always derived by hand, doing so automatically is more convenient and allows to avoid many stupid mistakes. To this aim it is useful to employ [FeynRules](https://feynrules.irmp.ucl.ac.be/) for generating the corresponding FeynArts model. When writing down the Lagrangian of our model we need to multiply every term in the counter term Lagragnian by `FR$CT`, for example

```mathematica
LPhi4 = LPhi4R + LPhi4CT;

LPhi4R = 1/2 del[phi, mu] del[phi, mu] - 1/2 Mphi^2*phi^2 - 1/(4!) g*phi^4;
LPhi4CT =  1/2 FR$CT (Zphi-1) del[phi, mu] del[phi, mu] - 1/2 Mphi^2 FR$CT (Zm Zphi-1)*phi^2 - 1/(4!) FR$CT (Zg Zphi^2-1)*g*phi^4;
```

Furthermore, before saving the FeynArts model via `WriteFeynArtsOutput` we need to set the global variable `FR$Loop` to `True`. For example,

```mathematica
FR$Loop=True;
SetDirectory[FileNameJoin[{$UserBaseDirectory,"Applications","FeynCalc","FeynArts","Models"}]];
WriteFeynArtsOutput[LPhi4,Output->"Phi4",CouplingRename->False];
```

### Renormalization schemes

From the computational point of view, the most convenient scheme is Modified Minimal Subtraction $\overline{\textrm{MS}}$. In this scheme all $\delta Z_x$ are chosen such, that they subtract the poles and certain terms involving $\gamma_E$ and $\log(4\pi)$. More precisely, at 1-loop for $D= 4 - 2 \varepsilon$ we subtract

\begin{equation}
\frac{1}{\varepsilon} - \gamma_E + \log (4\pi)
\end{equation}

$\overline{\textrm{MS}}$ renormalization constants are comparably easy to calculate, since we only need to extract the UV-poles of the occurring loop integrals and can ignore their finite parts.

On the other hand, we should be very careful to regularize all IR-divergences in such a way, that they do not show up as $\varepsilon$ poles. Otherwise we would include IR poles into our renormalization constants and obtain wrong results. In practice, this issue arises when dealing with theories containing massless particles. It can be avoided by giving those particles fictitious masses or using more advances techniques such as infrared rearrangement.

Unfortunately, in most cases $\overline{\textrm{MS}}$ renormalization constants alone are not sufficient to make physical predictions. First of all, according to the LSZ reduction formula, in physical observables such as cross sections or decay rates, external legs must be renormalized in the on-shell (OS) scheme. This is why we at least need to know $Z^{\textrm{OS}}$ for all external fields in our computation.

Second, in many effective field theories the expansion in heavy masses relies on the fact that those masses are also defined in the OS scheme.

Apart from that there are many other renormalization schemes such as Momentum Subtraction (MOM) etc. They can be useful in special cases, but usually what we are most interested in are the $\overline{\textrm{MS}}$ and OS schemes.

#### Renormalization conditions for the OS scheme

Following the presentation in [Gauge theories of the strong and electroweak interaction](https://inspirehep.net/literature/571258) by Boehm, Denner and Joos let us provide explicit formulas for the on-shell renormalization conditions when dealing with two-point functions of different field types

##### Scalar field

The bare 2-point vertex function for the scalar field is defined as

\begin{equation}
\Gamma(q^2) = (q^2 - m^2) + \Pi(q^2)
\end{equation}

and the renormalized one reads

\begin{equation}
\Gamma_R(q^2) = \Gamma(q^2) + \textrm{CT} = (q^2 - m^2) + \Pi(q^2) + \delta Z_{\phi} q^2 - m^2 (\delta Z_{\phi} + \delta Z_{m}   )
\end{equation}

For convenience we also introduce

\begin{equation}
\tilde{\Gamma}_R(q^2) = \Pi(q^2) + \delta Z_{\phi} q^2 - m^2 (\delta Z_{\phi} + \delta Z_{m}   )
\end{equation}

which corresponds to what one actually calculates when considering the sum of a bare amplitude and the corresponding counter-term. 

The renormalization condition reads

\begin{equation}
\lim_{q^2 \to m^2} \left [ \frac{\Gamma_R (q^2)}{q^2 - m^2}  = 1 \right ]
\end{equation}

Multiplying both sides of the equality by $q^2-m^2$ we get

\begin{equation}
\Gamma_R (q^2)  = ( q^2-m^2) \overset{q^2 \to m^2}{=} 0 \Rightarrow \Gamma_R (q^2) \biggl |_{q^2=m^2}  = 0
\end{equation}

which implies that $m$ is the physical mass of the particle and that the renormalized particle propagator has residue 1.

Using

\begin{equation}
\Gamma_R (q^2) =  \Gamma_R (m^2) + (q^2 - m^2) \Gamma'_R (m^2) + \mathcal{O}( (q^2 - m^2)^2)
\end{equation}

we arrive at

\begin{equation}
\Gamma_R(m^2) = 0, \quad \Gamma_R'(m^2) = 1
\end{equation}

which is equivalent to

\begin{equation}
\tilde{\Gamma}_R(m^2) = 0, \quad \tilde{\Gamma}_R'(m^2) = 0
\end{equation}

because setting $q^2 = m^2$ kills the $(q^2-m^2)$-term.

##### Massless vector field

The bare 2-point function for the massless vector field is defined as

\begin{equation}
	\Gamma^{\mu \nu} (q) = -g^{\mu \nu} q^2 - \frac{1-\xi}{\xi} q^{\mu} q^{\nu}  - \Pi^{\mu \nu} (q)
\end{equation}

and the renormalized one reads

\begin{equation}
	\Gamma_R^{\mu \nu} (q) = \Gamma^{\mu \nu} (q) + \;\text{CT}.
\end{equation}

For convenience we also introduce

\begin{equation}
	\tilde{\Gamma}^{\mu \nu}_R(q) = - \Pi^{\mu \nu} (q) + \;\text{CT}
\end{equation}

which corresponds to what one actually calculates when considering the sum of a bare amplitude and the corresponding counter-term.

The renormalization condition reads,

\begin{equation}
\lim_{q^2 \to 0} \left [ \frac{\Gamma_R^{\mu \nu} (q) \varepsilon(q)_\nu}{q^2}  = - \varepsilon_{\mu}(q) \right ]
\end{equation}

where the minus sign comes from the fact that $\varepsilon^{\ast \mu} \varepsilon_\mu = -1$ and $\varepsilon^\mu q_\mu =0$ for the projection onto the physical degrees of freedom of an on-shell massless vector boson.

Multiplying both sides of the equality by $q^2$ we get

\begin{equation}
\Gamma_R^{\mu \nu} (q^2)   \varepsilon_\nu (q) = - q^2 \varepsilon^{\mu}(q) \overset{q^2 \to 0}{=} 0 \Rightarrow \Gamma_R^{\mu \nu} (q^2) \varepsilon_\nu (q) \biggl|_{q^2=0}  = 0,
\end{equation}

which implies that the physical particle is massless and that the renormalized particle propagator has residue 1.

It is always possible to decompose this into transverse and longitudinal parts

\begin{equation}
\Gamma_R^{\mu \nu} (q) = \left ( g^{\mu \nu} - \frac{q^\mu q^\nu}{q^2} \right ) \Gamma_{R,T} (q^2) + \frac{q^\mu q^\nu}{q^2} \Gamma_{R,L} (q^2).
\end{equation}

Plugging this into the renormalization condition and using that $\varepsilon^\mu q_\mu = 0$, we end up with

\begin{equation}
		\lim_{q^2 \to 0} \left [ \frac{\varepsilon_{\mu}(q) \Gamma_{R,T} (q)}{q^2}  = - \varepsilon_{\mu}(q) \right ]
\end{equation}

which implies

\begin{equation}
		\lim_{q^2 \to 0} \left [ \frac{\Gamma_{R,T} (q^2)}{q^2}   \right ] = - 1 \Leftrightarrow \frac{\partial \Gamma_{R,T} (q^2) }{\partial q^2} \biggl |_{q^2=0} = - 1
\end{equation}

and is equivalent to

\begin{equation}
		\frac{\partial \tilde{\Gamma}_{R,T} (q^2) }{\partial q^2} \biggl |_{q^2=0} = 0
\end{equation}

Notice that if our $\Gamma_R^{\mu \nu}$ does not happen to have a transverse structure, then most likely something went wrong in the calculation, since the longitudinal part is not supposed to receive higher-order corrections! 

In practice, we will of course use projectors to extract the transverse and longitudinal components
\begin{align*}
		P^{\mu \nu}_T &= \frac{1}{D-1}	\left ( g^{\mu \nu} - \frac{q^\mu q^\nu}{q^2} \right ), \\
		P^{\mu \nu}_L &= \frac{q^\mu q^\nu}{q^2}.
\end{align*}

In the special case of the QED photon things can be further simplified by making use of the Ward identity for the renormalized propagator as Green function

\begin{equation}
	-\frac{1}{Z_\xi} \frac{1}{\xi} q^2 q_\mu Z_A \Gamma_R^{\mu \nu} (q) = i q^\nu,
\end{equation}

which implies that $Z_A/Z_{\xi}$ must be finite so that one can fix $Z_\xi = Z_A$.

Decomposing the propagator into transverse and longitudinal parts and applying the Ward identity for the renormalized propagator as vertex function (1PI-part of the Green function)

\begin{equation}
	q_\mu	\Gamma_R^{\mu \nu} (q) = - \frac{1}{\xi} q^2 q^\nu 
\end{equation}

we get

\begin{equation}
	q_\mu	\Gamma_R^{\mu \nu} (q) = q_\mu \Gamma_{R,L}^{\mu \nu} (q) = - \frac{1}{\xi} q^2 q^\nu \Rightarrow  \Gamma_{R,L} (q^2) = - \frac{1}{\xi} q^2
\end{equation}

which means that the longitudinal part does not receive higher-order corrections.

From the Ward identity and the absence of poles in vertex functions one can follow that the transverse part of the vertex function vanishes at $q^2=0$

\begin{equation}
	\Gamma_{R,T} (0) = \Gamma_{R,L} (0) = 0.
\end{equation}


##### Massive vector field renormalization

The bare 2-point function for the massive vector field is defined as

\begin{equation}
	\Gamma^{\mu \nu} (q) = -g^{\mu \nu} (q^2 - m^2) - \frac{1-\xi}{\xi} q^{\mu} q^{\nu}  - \Pi^{\mu \nu} (q)
\end{equation}

and the renormalized one reads

\begin{equation}
	\Gamma_R^{\mu \nu} (q) = \Gamma^{\mu \nu} (q) + \;\text{CT}
\end{equation}

For convenience we also introduce

\begin{equation}
	\tilde{\Gamma}^{\mu \nu}_R(q) = - \Pi^{\mu \nu} (q) + \;\text{CT}
\end{equation}

which corresponds to what one actually calculates when considering the sum of a bare amplitude and the corresponding counter-term.

The renormalization condition reads,

\begin{equation}
	\lim_{q^2 \to m^2} \left [ \frac{\Gamma_R^{\mu \nu} (q) \varepsilon(q)_\nu}{q^2 - m^2}  = - \varepsilon^{\mu}(q) \right ]
\end{equation}

Multiplying both sides of the equality by $q^2$ we get

\begin{equation}
	\Gamma_R^{\mu \nu} (q^2)   \varepsilon_\nu (q) = - (q^2 - m^2) \varepsilon^{\mu}(q) \overset{q^2 \to m^2}{=} 0 \Rightarrow \Gamma_R^{\mu \nu} (q^2) \varepsilon_\nu(q) \biggl|_{q^2=m^2}  = 0.
\end{equation}

Decomposition into transverse and longitudinal parts

\begin{equation}
	\Gamma_R^{\mu \nu} (q) = \left ( g^{\mu \nu} - \frac{q^\mu q^\nu}{q^2} \right ) \Gamma_{R,T} (q^2) + \frac{q^\mu q^\nu}{q^2} \Gamma_{R,L} (q^2),
\end{equation}

Contracting with the polarization vector  we find

\begin{equation}
	 \Gamma_R^{\mu \nu} (q^2)  \varepsilon_\nu (q) \biggl |_{q^2=m^2}  = 0 \Rightarrow \varepsilon^\mu (q) \, \Gamma_{R,T} (m^2) = 0 \Rightarrow \Gamma_{R,T} (m^2) = 0
\end{equation}

Plugging the decomposition into the renormalization condition and choosing the vector to be transverse we end up with

\begin{equation}
		\lim_{q^2 \to m^2} \left [ \frac{\varepsilon_{\mu}(q) \Gamma_{R,T} (q)}{q^2 -m ^2}  = - \varepsilon_{\mu}(q) \right ]
\end{equation}

Using

\begin{equation}
	\Gamma_{R,T} (q^2) =  \Gamma_{R,T} (m^2) + (q^2 - m^2) \Gamma'_{R,T} (m^2) + \mathcal{O}( (q^2 - m^2)^2)
\end{equation}

we arrive at

\begin{equation}
	\Gamma_{R,T}(m^2) = 0, \quad \Gamma_{R,T}'(m^2) = -1
\end{equation}

which is equivalent to

\begin{equation}
	\tilde{\Gamma}_{R,T}(m^2) = 0, \quad \tilde{\Gamma}_{R,T}'(m^2) = 0
\end{equation}


##### Fermion field renormalization
The bare 2-point vertex function for the fermion field is defined as

\begin{equation}
	\Gamma(p) = (\gamma \cdot p - m) + \Sigma(p)
\end{equation}

and the renormalized one reads

\begin{equation}
	\Gamma_R(p) = \Gamma(p) + \textrm{CT} =  (\gamma \cdot p - m)  + \Sigma(p) + (\gamma \cdot p - m) \delta Z_{\psi} - m  \delta Z_{m} 
\end{equation}

For convenience we also introduce

\begin{equation}
		\tilde{\Gamma}_R(p) = \Sigma(p) + (\gamma \cdot p - m) \delta Z_{\psi} - m  \delta Z_{m} \equiv (\gamma \cdot p - m) {\Sigma}_{V,R}(p^2) + m ({\Sigma}_{V,R}(p^2) + {\Sigma}_{S,R}(p^2))
\end{equation}

which corresponds to what one actually calculates when considering the sum of a bare amplitude and the corresponding counter-term.

The renormalization constants $Z_m$ and $Z_\psi$ are fixed by the condition

\begin{equation}
	\lim_{p^2 \to m^2} \left [ \frac{(\gamma \cdot p + m )\Gamma_R (p) u(p)}{p^2-m^2}  = u(p) \right ],
\end{equation}

where 

\begin{equation}
		(\gamma \cdot p-m) u (p) = 0, \quad  (\gamma \cdot p+m) (\gamma \cdot p-m) = p^2 - m^2.
\end{equation}

Multiplying both sides of the equality by $p^2-m^2$ we get

\begin{equation}
		(\gamma \cdot p + m )\Gamma_R (p) u(p) = u(p)( p^2-m^2) \overset{p^2 \to m^2}{=} 0 \Rightarrow \Gamma_R (p) u(p) \biggl|_{p^2=m^2}  = 0,
\end{equation}

which implies that $m$ is the physical mass of the particle and that the renormalized particle propagator has residue 1.

We can apply the usual decomposition

\begin{equation}
	\Sigma (\gamma \cdot p) = \gamma \cdot p \Sigma_V (p^2) + m \Sigma_S (p^2) = (\gamma \cdot p -m) \Sigma_V (p^2) + m (\Sigma_V (p^2) + \Sigma_S (p^2)) ,
\end{equation}

Expanding it around $\gamma \cdot p=m$ we find

\begin{align*}
	\Sigma (\gamma \cdot p) & =  \Sigma (\gamma \cdot p) \biggl|_{\gamma \cdot p=m} + (\gamma \cdot p-m) \frac{ \partial \Sigma (\gamma \cdot p)}{\partial \gamma \cdot p } \biggl|_{\gamma \cdot p=m} + \mathcal{O} ((\gamma \cdot p-m)^2) \\
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	& =   m (\Sigma_V(m^2) + \Sigma_S(m^2) ) +   (\gamma \cdot p - m) \left [ \Sigma_V(m^2) + 2 m^2 \left ( \frac{\partial \Sigma_S (p^2) }{\partial p^2}  + \frac{\partial \Sigma_V (p^2) }{\partial p^2}  \right )\biggl|_{p^2 = m^2} \right ] + \mathcal{O} ((\gamma \cdot p-m)^2) \\
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	& =   m (\Sigma_V(m^2) + \Sigma_S(m^2) ) +   (\gamma \cdot p - m) \left [ \Sigma_V(m^2) + 2 m^2 \left ( \Sigma'_S (m^2) + \Sigma'_V (m^2)  \right ) \right ] + \mathcal{O} ((\gamma \cdot p-m)^2).
\end{align*}

Plugging the expansion into the renormalization condition we arrive at

\begin{equation}
	\lim_{p^2 \to m^2} \left [ \frac{2m^2}{p^2-m^2} (\Sigma_V(m^2) + \Sigma_S(m^2) - \delta Z_m) + 
	\left ( \Sigma_V(m^2) + 2 m^2 \left ( \Sigma'_S (m^2) + \Sigma'_V (m^2)  \right) + \delta Z_\psi \right ) = 0 \right ],
\end{equation}

which amounts to the requirements

\begin{equation}
		\Gamma_{R,V}(m^2) + \Gamma_{R,S}(m^2)  = 0, \\
		\left ( \Gamma_{R,V}(m^2) + 2 m^2 \left ( \Gamma'_{R,S} (m^2) + \Gamma'_{R,V} (m^2)  \right) \right ) = 1,
\end{equation}
	or equivalently

\begin{equation}
		 \Sigma_{V,R}(m^2) + \Sigma_{S,R}(m^2)  = 0, \\
		\left ( \Sigma_{V,R}(m^2) + 2 m^2 \left ( \Sigma'_{S,R} (m^2) + \Sigma'_{V,R} (m^2)  \right) \right ) = 0,
\end{equation}
where the first one fixes $Z_m$ and the second one $Z_\psi$.

In practical calculations it is better to introduce a projector that can extract $\Sigma_i(m^2)$ ( or well  $\Sigma_{i,R}(m^2)$) directly

\begin{align*}
\mathrm{Tr} \left( \frac{\gamma \cdot p+m}{4m^2} \Sigma (\gamma \cdot p) \right ) & = \Sigma_S (p^2) + \frac{p^2}{m^2} \Sigma_V(p^2) = \Sigma_S (p^2) + \Sigma_V(p^2) + \frac{p^2-m^2}{m^2} \Sigma_V(p^2) \\
	&  = \Sigma_1 (p^2) + \frac{p^2-m^2}{m^2} \Sigma_2(p^2).
\end{align*}

Applying the projector to the series expansion yields

\begin{equation}
	\mathrm{Tr} \left( \frac{\gamma \cdot p+m}{4m^2} \Sigma (\gamma \cdot p) \right )  = \Sigma_1(m^2) +
	\frac{p^2-m^2}{m^2}  \left [ \Sigma_2(m^2) + 2 m^2 \Sigma'_1 (m^2)  \right ] + \mathcal{O}((p^2 -m^2)^2)
\end{equation}

Of course, one can also get $\Sigma_V$ and $\Sigma_S$ separately using projectors

\begin{equation}
\Sigma_V = \frac{1}{4} \mathrm{Tr}(\gamma \cdot p \Sigma (\gamma \cdot p)), \quad \Sigma_S = \frac{1}{4 m} \mathrm{Tr}(\Sigma (\gamma \cdot p))
\end{equation}

Notice also that

\begin{equation}
 \frac{\partial \Sigma_{S,V}(p^2)}{\partial p^\mu} = \frac{\partial \Sigma_{S,V}(p^2)}{\partial p^2} \frac{\partial p^2}{\partial p^\mu} = 2 p^\mu \frac{\partial \Sigma_{S,V}(p^2)}{\partial p^2} \Rightarrow 
  \frac{\partial \Sigma_{S,V}(p^2)}{\partial p^2} = \frac{p_\mu}{2 p^2 } \frac{\partial \Sigma_{S,V}(p^2)}{\partial p^\mu}
\end{equation}

### One-loop renormalization

At one loop the calculations on renormalization constants can be very much streamlined using the capabilities of FeynCalc and FeynHelpers. 

As far as the $\overline{\textrm{MS}}$ scheme is concerned, it is not really necessary to regularize IR divergences with fake masses. Instead, we can set the global variable `$KeepLogDivergentScalelessIntegrals`  to `True`, which will prevent FeynCalc from setting $\log$-divergent integrals (i.e. the ones that are proportional to $1/\varepsilon_{\textrm{UV}} - 1/\varepsilon_{\textrm{IR}}$). Then, we can either use Package-X or directly employ the built-in function `PaVeUVPart` to extract the UV poles of all occurring 1-loop integrals. 

As long as we are dealing with amplitudes containing only quadratic propagators, this toolset is fully sufficient to determine the 
$\overline{\textrm{MS}}$ renormalization constants of any theory at one loop.

The OS renormalization usually requires slightly more effort, but using the formulas provided in this document and the capabilities of Package-X provided via FeynHelpers, such calculations are always doable.

