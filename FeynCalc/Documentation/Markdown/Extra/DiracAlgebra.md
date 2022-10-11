## Dirac algebra

### See also



[Overview](FeynCalc.md).

This section contains some explicit formulas used by FeynCalc when simpifying chains of Dirac matrices. Such relations can be found e.g.
in [Veltman's Gammatrica](https:/XXX) or rederived by hand.

### BMHV algebra

In the Breitenlohner-Maison-'t Hooft-Veltman scheme we are dealing with matrices
in $D$, $4$ and $D-4$ dimensions. Many identities of the BMHV algebra can be proven by
by decomposing Dirac matrices into two pieces

$$
\begin{align}
\begin{split}
\dim(\gamma^\mu) &= D, \\
\dim(\bar{\gamma}^\mu) &= 4, \\
\dim(\hat{\gamma}^\mu) &= D-4 .
\end{split}
\begin{split}
\gamma^\mu &= \bar{\gamma}^\mu +\hat{\gamma}^\mu, \\
g^{\mu \nu} &= \bar{g}^{\mu \nu} + \hat{g}^{\mu \nu}, \\
p^\mu & = \bar{p}^\mu + \hat{p}^\mu.
\end{split}
\end{align}
$$

The anticommuatators between Dirac matrices in different dimensions are given by

$$
\begin{align}
\{ \gamma^\mu, \gamma^\nu \} &= 2 g^{\mu \nu}, \\
\{ \bar{\gamma}^\mu, \bar{\gamma}^\nu \} &= \{ \gamma^\mu, \bar{\gamma}^\nu \} = 2 \bar{g}^{\mu \nu}, \\
\{ \hat{\gamma}^\mu, \hat{\gamma}^\nu \} &= \{ \gamma^\mu, \hat{\gamma}^\nu \} = 2 \hat{g}^{\mu \nu}, \\
\{ \bar{\gamma}^\mu, \hat{\gamma}^\nu \} &= 0.
\end{align}
$$

Notice that while $\gamma^5$ anticommutes with all other Dirac matrices in $4$ dimensions, it commutes with them in $D-4$ dimensions. However, in $D$ dimensions the anticommutator is not zero

$$
\begin{align}
\{ \bar{\gamma}^\mu, \gamma^5 \} &=[ \hat{\gamma}^\mu, \gamma^5 ] = 0 \\
\{ \gamma^\mu, \gamma^5 \} & = \{ \hat{\gamma}^\mu, \gamma^5 \}= 2 \hat{\gamma}^\mu \gamma^5 = 2 \gamma^5 \hat{\gamma}^\mu.
\end{align}
$$

For the chiral projectors we obtain

$$
\begin{align}
P_{L/R} \bar{\gamma}^\mu  & = \bar{\gamma}^\mu P_{R/L} \\
P_{L/R} \hat{\gamma}^\mu  &= \hat{\gamma}^\mu P_{L/R} \\
P_{L/R} \gamma^\mu  & = \bar{\gamma}^\mu P_{R/L} + \hat{\gamma}^\mu P_{L/R} = \gamma^\mu P_{R/L} \mp \hat{\gamma}^\mu \gamma^5 \\
\gamma^\mu P_{L/R}  & =  P_{R/L} \bar{\gamma}^\mu +  P_{L/R} \hat{\gamma}^\mu = P_{R/L} \gamma^\mu  \mp \gamma^5 \hat{\gamma}^\mu
\end{align}
$$

and

$$
\begin{align}
P_{L/R} (\bar{\gamma} \cdot \bar{p} + m) &= \bar{\gamma} \cdot \bar{p} P_{R/L} + m P_{L/R} = (\bar{\gamma} \cdot \bar{p} - m) P_{R/L}  + m \\
P_{L/R} (\bar{\gamma} \cdot \hat{p} + m) & = (\bar{\gamma} \cdot \hat{p} + m) P_{L/R} \\
P_{L/R} (\bar{\gamma} \cdot p + m) &= (\bar{\gamma} \cdot \hat{p} + m) P_{L/R} + \bar{\gamma} \cdot \bar{p} P_{R/L} =
(\bar{\gamma} \cdot \bar{p} - m) P_{R/L} + \bar{\gamma} \cdot \hat{p} P_{L/R} + m \\ \nonumber \\ \nonumber
%%%%%%%%%%%%%%
 (\bar{\gamma} \cdot \bar{p} + m) P_{L/R} &= P_{R/L} \bar{\gamma} \cdot \bar{p}  + P_{L/R} m  = P_{R/L} (\bar{\gamma} \cdot \bar{p} - m)   + m \\
(\bar{\gamma} \cdot \hat{p} + m) P_{L/R}  & = P_{L/R}  (\bar{\gamma} \cdot \hat{p} + m) \\
 (\bar{\gamma} \cdot p + m) P_{L/R} &=  P_{L/R} ( \bar{\gamma} \cdot \hat{p} + m) + P_{R/L} \bar{\gamma} \cdot \bar{p}  =
P_{R/L} (\bar{\gamma} \cdot \bar{p} - m)  + P_{L/R} \bar{\gamma} \cdot \hat{p}  + m
\end{align}
$$

Notice that package `TRACER` resolves the redundancy of having $\gamma^\mu = \bar{\gamma}^\mu + \hat{\gamma}^\mu$ by eliminating $\bar{\gamma}^\mu$ and offering a function that reintroduces it at the end of the calculation.

Contractions of Dirac matrices and vectors with the metric read

$$
\begin{split}
g^{\mu \nu} \gamma_\nu &= \gamma^\mu, \\
\bar{g}^{\mu \nu} \bar{\gamma}_\nu &= g^{\mu \nu} \bar{\gamma}_\nu = \bar{g}^{\mu \nu} \gamma_\nu = \bar{\gamma}^\mu, \\
\hat{g}^{\mu \nu} \hat{\gamma}_\nu &= g^{\mu \nu} \hat{\gamma}_\nu = \hat{g}^{\mu \nu} \gamma_\nu = \hat{\gamma}^\mu, \\
\bar{g}^{\mu \nu} \hat{\gamma}_\nu &= \hat{g}^{\mu \nu} \bar{\gamma}_\nu = 0,
\end{split} 
\begin{split}
g^{\mu \nu} p_\nu &= p^\mu, \\
\bar{g}^{\mu \nu} \bar{p}_\nu &= g^{\mu \nu} \bar{p}_\nu = \bar{g}^{\mu \nu} p_\nu = \bar{p}^\mu, \\
\hat{g}^{\mu \nu} \hat{p}_\nu &= g^{\mu \nu} \hat{p}_\nu = \hat{g}^{\mu \nu} p_\nu = \hat{p}^\mu, \\
\bar{g}^{\mu \nu} \hat{p}_\nu &= \hat{g}^{\mu \nu} \bar{p}_\nu = 0.
\end{split}
$$

Contractions of the metric with itself 

$$
\begin{split}
g^{\mu \nu} g_{\nu \rho} & = g^\mu_\rho \\
\bar{g}^{\mu \nu} \bar{g}_{\nu \rho} & =g^{\mu \nu} \bar{g}_{\nu \rho} = \bar{g}^{\mu \nu} g_{\nu \rho} = \bar{g}^\mu_\rho \\
\hat{g}^{\mu \nu} \hat{g}_{\nu \rho} & = g^{\mu \nu} \hat{g}_{\nu \rho} = \hat{g}^{\mu \nu} g_{\nu \rho}= \hat{g}^\mu_\rho \\
\bar{g}^{\mu \nu} \hat{g}_{\nu \rho} &= \hat{g}^{\mu \nu} \bar{g}_{\nu \rho}  = 0,
\end{split}
\begin{split}
g^{\mu \nu} g_{\mu \nu} & = d, \\
\bar{g}^{\mu \nu} \bar{g}_{\mu \nu} & = g^{\mu \nu} \bar{g}_{\mu \nu} = \bar{g}^{\mu \nu} g_{\mu \nu} = 4, \\
\hat{g}^{\mu \nu} \hat{g}_{\mu \nu} & = g^{\mu \nu} \hat{g}_{\mu \nu} = \hat{g}^{\mu \nu} g_{\mu \nu} = d-4, \\
\bar{g}^{\mu \nu} \hat{g}_{\mu \nu} &= \hat{g}^{\mu \nu} \bar{g}_{\mu \nu}  = 0.
\end{split}
$$

Contractions of Dirac matrices and vectors with themselves

$$
\begin{split}
\gamma^\mu \gamma_\mu  &=  D, \\
\bar{\gamma}^\mu \bar{\gamma}_\mu  &= \gamma^\mu \bar{\gamma}_\mu  = \bar{\gamma}^\mu \gamma_\mu = 4, \\
\hat{\gamma}^\mu \hat{\gamma}_\mu  &= \gamma^\mu \hat{\gamma}_\mu = \hat{\gamma}^\mu \gamma_\mu = D-4, \\
\bar{\gamma}^\mu \hat{\gamma}_\mu &=  \hat{\gamma}^\mu \bar{\gamma}_\mu = 0,
\end{split}
\begin{split}
p^\mu p_\mu &= p^2, \\
\bar{p}^\mu \bar{p}_\mu &= \bar{p}^\mu p_\mu = p^\mu \bar{p}_\mu =  \bar{p}^2, \\
\hat{p}^\mu \hat{p}_\mu &= \hat{p}^\mu p_\mu = p^\mu \hat{p}_\mu =  \hat{p}^2, \\
\bar{p}^\mu \hat{p}_\mu &= \hat{p}^\mu \bar{p}_\mu = 0.
\end{split}
$$

Dirac slashes

$$
\begin{split}
\gamma^\mu p_\mu &= \gamma \cdot p, \\
\bar{\gamma}^\mu \bar{p}_\mu &= \bar{\gamma}^\mu p_\mu = \gamma^\mu \bar{p}_\mu = \bar{\gamma} \cdot \bar{p}, \\
\hat{\gamma}^\mu p_\mu &= \hat{\gamma}^\mu p_\mu = \gamma^\mu \hat{p}_\mu = \hat{\gamma} \cdot \hat{p}, \\
\bar{\gamma}^\mu \hat{p}_\mu &= \hat{\gamma}^\mu \bar{p}_\mu = 0. \\
\end{split}
$$

Index pairs with one, two, three, four or five free indices

$$
\begin{align}
\begin{split}
\gamma^\mu \gamma^\nu \gamma_\mu &= -(d-2) \gamma^\nu, \\
\gamma^\mu \bar{\gamma}^\nu \gamma_\mu &= -(d-2) \bar{\gamma}^\nu, \\
\gamma^\mu \hat{\gamma}^\nu \gamma_\mu &= -(d-2) \hat{\gamma}^\nu,
\end{split} \quad \quad 
\begin{split}
\bar{\gamma}^\mu \bar{\gamma}^\nu \bar{\gamma}_\mu &= -2 \bar{\gamma}^\nu, \\
\bar{\gamma}^\mu \gamma^\nu \bar{\gamma}_\mu &= -4 \gamma^\nu +2 \bar{\gamma}^\nu, \\
\bar{\gamma}^\mu \hat{\gamma}^\nu \bar{\gamma}_\mu &= -4 \hat{\gamma}^\nu, \\
\end{split} \quad \quad \quad \quad 
\begin{split}
\hat{\gamma}^\mu \hat{\gamma}^\nu \hat{\gamma}_\mu &= -(d-6) \hat{\gamma}^\nu, \\
\hat{\gamma}^\mu \gamma^\nu \hat{\gamma}_\mu &= -(d-4) \gamma^\nu + 2\hat{\gamma}^\nu, \\
\hat{\gamma}^\mu \bar{\gamma}^\nu \hat{\gamma}_\mu &= -(d-4) \bar{\gamma}^\nu,
\end{split}
\end{align}
$$

$$
\begin{align}
\gamma^\mu \gamma^\nu \gamma^\rho \gamma_\mu &= (d-4) \gamma^\nu \gamma^\rho + 4 g^{\nu \rho} I, \\
\gamma^\mu \bar{\gamma}^\nu \bar{\gamma}^\rho \gamma_\mu &= (d-4) \bar{\gamma}^\nu \bar{\gamma}^\rho + 4 \bar{g}^{\nu \rho} I, \\
\gamma^\mu \hat{\gamma}^\nu \hat{\gamma}^\rho\gamma_\mu &= (d-4) \hat{\gamma}^\nu \hat{\gamma}^\rho + 4 \hat{g}^{\nu \rho} I, \\
\bar{\gamma}^\mu \bar{\gamma}^\nu \bar{\gamma}^\rho \bar{\gamma}_\mu &= 4 \bar{g}^{\nu \rho}, \\
\bar{\gamma}^\mu \gamma^\nu \gamma^\rho \bar{\gamma}_\mu &= 4 \gamma^\nu \gamma^\rho - 2 \bar{\gamma}^\nu \gamma^\rho + 2\bar{\gamma}^\rho
 \gamma^\nu, \\
\bar{\gamma}^\mu \hat{\gamma}^\nu \hat{\gamma}^\rho \bar{\gamma}_\mu &= 4 \hat{\gamma}^\nu \hat{\gamma}^\rho, \\
\hat{\gamma}^\mu \hat{\gamma}^\nu \hat{\gamma}^\rho \hat{\gamma}_\mu &= (d-8) \hat{\gamma}^\nu \hat{\gamma}^\rho + 4 \hat{g}^{\nu \rho} I, \\
\hat{\gamma}^\mu \gamma^\nu \gamma^\rho \hat{\gamma}_\mu &= (d-4) \gamma^\nu \gamma^\rho - 2 \hat{\gamma}^\nu \gamma^\rho + 2\hat{\gamma}^\rho \gamma^\nu, \\
\hat{\gamma}^\mu \bar{\gamma}^\nu \bar{\gamma}^\rho \hat{\gamma}_\mu &= (d-4) \bar{\gamma}^\nu \bar{\gamma}^\rho,
\end{align}
$$

$$
\begin{align}
\gamma^\mu \gamma^\nu \gamma^\rho \gamma^\sigma \gamma_\mu &= -(d-4) \gamma^\nu \gamma^\rho \gamma^\sigma -2 \gamma^\sigma \gamma^\rho \gamma^\nu \\
\gamma^\mu \bar{\gamma}^\nu \bar{\gamma}^\rho \bar{\gamma}^\sigma \gamma_\mu &=-(d-4) \bar{\gamma}^\nu \bar{\gamma}^\rho \bar{\gamma}^\sigma -2 \bar{\gamma}^\sigma \bar{\gamma}^\rho \bar{\gamma}^\nu  \\
\gamma^\mu \hat{\gamma}^\nu \hat{\gamma}^\rho \hat{\gamma}^\sigma \gamma_\mu &= -(d-4) \hat{\gamma}^\nu \hat{\gamma}^\rho \hat{\gamma}^\sigma -2 \hat{\gamma}^\sigma \hat{\gamma}^\rho \hat{\gamma}^\nu  \\
\bar{\gamma}^\mu \bar{\gamma}^\nu \bar{\gamma}^\rho \bar{\gamma}^\sigma \bar{\gamma}_\mu &= -2 \bar{\gamma}^\sigma \bar{\gamma}^\rho \bar{\gamma}^\nu \\
\bar{\gamma}^\mu \gamma^\nu \gamma^\rho  \gamma^\sigma \bar{\gamma}_\mu &= -4 \gamma^\nu \gamma^\rho \gamma^\sigma + 2 \bar{\gamma}^\nu \gamma^\rho \gamma^\sigma - 2\bar{\gamma}^\rho \gamma^\nu \gamma^\sigma + 2 \bar{\gamma}^\sigma \gamma^\nu \gamma^\rho, \\
\bar{\gamma}^\mu \hat{\gamma}^\nu \hat{\gamma}^\rho \hat{\gamma}^\sigma \bar{\gamma}_\mu &= - 4 \hat{\gamma}^\nu \hat{\gamma}^\rho \hat{\gamma}^\sigma , \\
\hat{\gamma}^\mu \hat{\gamma}^\nu \hat{\gamma}^\rho \hat{\gamma}^\sigma \hat{\gamma}_\mu &= -(d-8) \hat{\gamma}^\nu \hat{\gamma}^\rho \hat{\gamma}^\sigma -2  \hat{\gamma}^\sigma \hat{\gamma}^\rho \hat{\gamma}^\nu \\
\hat{\gamma}^\mu \gamma^\nu \gamma^\rho \gamma^\sigma \hat{\gamma}_\mu &= -(d-4) \gamma^\nu \gamma^\rho \gamma^\sigma + 2 \hat{\gamma}^\nu \gamma^\rho \gamma^\sigma - 2\hat{\gamma}^\rho \gamma^\nu \gamma^\sigma + 2 \hat{\gamma}^\sigma \gamma^\nu \gamma^\rho,  \\
\hat{\gamma}^\mu \bar{\gamma}^\nu \bar{\gamma}^\rho \bar{\gamma}^\sigma \hat{\gamma}_\mu &= -(d-4) \bar{\gamma}^\nu \bar{\gamma}^\rho \bar{\gamma}^\sigma,
\end{align}
$$

$$
\begin{align}
\gamma^\mu \gamma^\nu \gamma^\rho \gamma^\sigma \gamma^\tau \gamma_\mu &= (d-4) \gamma^\nu \gamma^\rho \gamma^\sigma \gamma^\tau + 2 \gamma^\sigma \gamma^\rho \gamma^\nu \gamma^\tau + 2 \gamma^\tau \gamma^\nu \gamma^\rho \gamma^\sigma \\
\gamma^\mu \bar{\gamma}^\nu \bar{\gamma}^\rho \bar{\gamma}^\sigma \bar{\gamma}^\tau \gamma_\mu &= (d-4) \bar{\gamma}^\nu \bar{\gamma}^\rho \bar{\gamma}^\sigma \bar{\gamma}^\tau + 2 \bar{\gamma}^\sigma \bar{\gamma}^\rho \bar{\gamma}^\nu  \bar{\gamma}^\tau + 2 \bar{\gamma}^\tau \bar{\gamma}^\nu \bar{\gamma}^\rho  \bar{\gamma}^\sigma \\
\gamma^\mu \hat{\gamma}^\nu \hat{\gamma}^\rho \hat{\gamma}^\sigma \hat{\gamma}^\tau \gamma_\mu &= (d-4) \hat{\gamma}^\nu \hat{\gamma}^\rho \hat{\gamma}^\sigma \hat{\gamma}^\tau +2 \hat{\gamma}^\sigma \hat{\gamma}^\rho \hat{\gamma}^\nu \hat{\gamma}^\tau + 2\hat{\gamma}^\tau \hat{\gamma}^\nu \hat{\gamma}^\rho \hat{\gamma}^\sigma  \\
\bar{\gamma}^\mu \bar{\gamma}^\nu \bar{\gamma}^\rho \bar{\gamma}^\sigma \bar{\gamma}^\tau \bar{\gamma}_\mu &= 2 \bar{\gamma}^\sigma \bar{\gamma}^\rho \bar{\gamma}^\nu \bar{\gamma}^\tau + 2 \bar{\gamma}^\tau \bar{\gamma}^\nu \bar{\gamma}^\rho \bar{\gamma}^\sigma \\
\bar{\gamma}^\mu \gamma^\nu \gamma^\rho  \gamma^\sigma \gamma^\tau \bar{\gamma}_\mu &= 4 \gamma^\nu \gamma^\rho \gamma^\sigma \gamma^\tau \\
&- 2 \bar{\gamma}^\nu \gamma^\rho \gamma^\sigma \gamma^\tau + 2\bar{\gamma}^\rho \gamma^\nu \gamma^\sigma \gamma^\tau - 2 \bar{\gamma}^\sigma \gamma^\nu \gamma^\rho \gamma^\tau + 2 \bar{\gamma}^\tau \gamma^\nu \gamma^\rho \gamma^\sigma, \\
\bar{\gamma}^\mu \hat{\gamma}^\nu \hat{\gamma}^\rho \hat{\gamma}^\sigma \hat{\gamma}^\tau \bar{\gamma}_\mu &= 4 \hat{\gamma}^\nu \hat{\gamma}^\rho \hat{\gamma}^\sigma \hat{\gamma}^\tau , \\
\hat{\gamma}^\mu \hat{\gamma}^\nu \hat{\gamma}^\rho \hat{\gamma}^\sigma \hat{\gamma}^\tau \hat{\gamma}_\mu &= (d-8) \hat{\gamma}^\nu \hat{\gamma}^\rho \hat{\gamma}^\sigma \hat{\gamma}^\tau + 2  \hat{\gamma}^\sigma \hat{\gamma}^\rho \hat{\gamma}^\nu \hat{\gamma}^\tau + 2 \hat{\gamma}^\tau \hat{\gamma}^\nu \hat{\gamma}^\rho \hat{\gamma}^\sigma \\
\hat{\gamma}^\mu \gamma^\nu \gamma^\rho \gamma^\sigma \gamma^\tau \hat{\gamma}_\mu &= (d-4) \gamma^\nu \gamma^\rho \gamma^\sigma \gamma^\tau \\
&- 2 \hat{\gamma}^\nu \gamma^\rho \gamma^\sigma \gamma^\tau + 2\hat{\gamma}^\rho \gamma^\nu \gamma^\sigma \gamma^\tau - 2 \hat{\gamma}^\sigma \gamma^\nu \gamma^\rho \gamma^\tau + 2 \hat{\gamma}^\tau \gamma^\nu \gamma^\rho \gamma^\sigma,  \\
\hat{\gamma}^\mu \bar{\gamma}^\nu \bar{\gamma}^\rho \bar{\gamma}^\sigma \bar{\gamma}^\tau \hat{\gamma}_\mu &= (d-4) \bar{\gamma}^\nu \bar{\gamma}^\rho \bar{\gamma}^\sigma \bar{\gamma}^\tau,
\end{align}
$$

$$
\begin{align}
\gamma^\mu \gamma^\nu \gamma^\rho \gamma^\sigma \gamma^\tau \gamma^\kappa \gamma_\mu =& -(d-4) \gamma^\nu \gamma^\rho \gamma^\sigma \gamma^\tau \gamma^\kappa \\
&- 2 \gamma^\sigma \gamma^\rho \gamma^\nu \gamma^\tau \gamma^\kappa - 2 \gamma^\tau \gamma^\nu \gamma^\rho \gamma^\sigma \gamma^\kappa + 2\gamma^\kappa \gamma^\nu \gamma^\rho \gamma^\sigma \gamma^\tau \\
\gamma^\mu \bar{\gamma}^\nu \bar{\gamma}^\rho \bar{\gamma}^\sigma \bar{\gamma}^\tau \bar{\gamma}^\kappa \gamma_\mu =& -(d-4) \bar{\gamma}^\nu \bar{\gamma}^\rho \bar{\gamma}^\sigma \bar{\gamma}^\tau \bar{\gamma}^\kappa \\
& - 2 \bar{\gamma}^\sigma \bar{\gamma}^\rho \bar{\gamma}^\nu \bar{\gamma}^\tau \bar{\gamma}^\kappa - 2 \bar{\gamma}^\tau \bar{\gamma}^\nu \bar{\gamma}^\rho \bar{\gamma}^\sigma \bar{\gamma}^\kappa + 2\bar{\gamma}^\kappa \bar{\gamma}^\nu \bar{\gamma}^\rho \bar{\gamma}^\sigma \bar{\gamma}^\tau\\
\gamma^\mu \hat{\gamma}^\nu \hat{\gamma}^\rho \hat{\gamma}^\sigma \hat{\gamma}^\tau \hat{\gamma}^\kappa \gamma_\mu =& -(d-4) \hat{\gamma}^\nu \hat{\gamma}^\rho \hat{\gamma}^\sigma \hat{\gamma}^\tau \hat{\gamma}^\kappa \\
& - 2 \hat{\gamma}^\sigma \hat{\gamma}^\rho \hat{\gamma}^\nu \hat{\gamma}^\tau \hat{\gamma}^\kappa - 2 \hat{\gamma}^\tau \hat{\gamma}^\nu \hat{\gamma}^\rho \hat{\gamma}^\sigma \hat{\gamma}^\kappa + 2\hat{\gamma}^\kappa \hat{\gamma}^\nu \hat{\gamma}^\rho \hat{\gamma}^\sigma \hat{\gamma}^\tau \\
\bar{\gamma}^\mu \bar{\gamma}^\nu \bar{\gamma}^\rho \bar{\gamma}^\sigma \bar{\gamma}^\tau \bar{\gamma}^\kappa \bar{\gamma}_\mu & = 2 \bar{\gamma}^\tau \bar{\gamma}^\sigma \bar{\gamma}^\rho \bar{\gamma}^\nu \bar{\gamma}^\kappa + 2 \bar{\gamma}^\kappa \bar{\gamma}^\nu \bar{\gamma}^\rho \bar{\gamma}^\sigma \bar{\gamma}^\tau = - 2 \bar{\gamma}^\kappa \bar{\gamma}^\tau \bar{\gamma}^\sigma \bar{\gamma}^\rho \bar{\gamma}^\nu  \\
\bar{\gamma}^\mu \gamma^\nu \gamma^\rho  \gamma^\sigma \gamma^\tau \gamma^\kappa \bar{\gamma}_\mu =& -4 \gamma^\nu \gamma^\rho \gamma^\sigma \gamma^\tau \gamma^\kappa + 2 \bar{\gamma}^\nu \gamma^\rho \gamma^\sigma \gamma^\tau \gamma^\kappa - 2\bar{\gamma}^\rho \gamma^\nu \gamma^\sigma \gamma^\tau \gamma^\kappa \\
&+ 2 \bar{\gamma}^\sigma \gamma^\nu \gamma^\rho \gamma^\tau \gamma^\kappa - 2 \bar{\gamma}^\tau \gamma^\nu \gamma^\rho \gamma^\sigma \gamma^\kappa + 2\bar{\gamma}^\kappa \gamma^\nu \gamma^\rho \gamma^\sigma \gamma^\tau \\
\bar{\gamma}^\mu \hat{\gamma}^\nu \hat{\gamma}^\rho \hat{\gamma}^\sigma \hat{\gamma}^\tau \hat{\gamma}^\kappa \bar{\gamma}_\mu =& -4 \hat{\gamma}^\nu \hat{\gamma}^\rho \hat{\gamma}^\sigma \hat{\gamma}^\tau \hat{\gamma}^\kappa , \\
\hat{\gamma}^\mu \hat{\gamma}^\nu \hat{\gamma}^\rho \hat{\gamma}^\sigma \hat{\gamma}^\tau \hat{\gamma}^\kappa \hat{\gamma}_\mu =& -(d-8) \hat{\gamma}^\nu \hat{\gamma}^\rho \hat{\gamma}^\sigma \hat{\gamma}^\tau \hat{\gamma}^\kappa  \\
&- 2  \hat{\gamma}^\sigma \hat{\gamma}^\rho \hat{\gamma}^\nu \hat{\gamma}^\tau \hat{\gamma}^\kappa - 2 \hat{\gamma}^\tau \hat{\gamma}^\nu \hat{\gamma}^\rho \hat{\gamma}^\sigma \hat{\gamma}^\kappa + 2 \hat{\gamma}^\kappa \hat{\gamma}^\nu \hat{\gamma}^\rho \hat{\gamma}^\sigma \hat{\gamma}^\tau \\
\hat{\gamma}^\mu \gamma^\nu \gamma^\rho \gamma^\sigma \gamma^\tau \gamma^\kappa \hat{\gamma}_\mu =& -(d-4) \gamma^\nu \gamma^\rho \gamma^\sigma \gamma^\tau \gamma^\kappa +  2 \hat{\gamma}^\nu \gamma^\rho \gamma^\sigma \gamma^\tau  \gamma^\kappa - 2\hat{\gamma}^\rho \gamma^\nu \gamma^\sigma \gamma^\tau \gamma^\kappa \\
& + 2 \hat{\gamma}^\sigma \gamma^\nu \gamma^\rho \gamma^\tau \gamma^\kappa  - 2 \hat{\gamma}^\tau \gamma^\nu \gamma^\rho \gamma^\sigma \gamma^\kappa +2 \hat{\gamma}^\kappa \gamma^\nu \gamma^\rho \gamma^\sigma \gamma^\tau,  \\
\hat{\gamma}^\mu \bar{\gamma}^\nu \bar{\gamma}^\rho \bar{\gamma}^\sigma \bar{\gamma}^\tau \bar{\gamma}^\kappa \hat{\gamma}_\mu =& -(d-4) \bar{\gamma}^\nu \bar{\gamma}^\rho \bar{\gamma}^\sigma \bar{\gamma}^\tau \bar{\gamma}^\kappa,
\end{align}
$$

#### Index contractions

If the first and the last matrix are in different dimensions, we can always write them as

$$
\begin{align}
\gamma^\mu \dots \bar{\gamma}_\mu & = \bar{\gamma}^\mu \dots \gamma_\mu = \bar{\gamma}^\mu \dots \bar{\gamma}_\mu, \\
\gamma^\mu \dots \hat{\gamma}_\mu & = \hat{\gamma}^\mu \dots \gamma_\mu = \hat{\gamma}^\mu \dots \hat{\gamma}_\mu, \\
\bar{\gamma}^\mu \dots \hat{\gamma}_\mu & = \hat{\gamma}^\mu \dots \bar{\gamma}_\mu = 0.
\end{align}
$$

For general index pairs we have that

$$
\begin{align}
\gamma^\mu \bar{\gamma}^{\nu_1} \dots \bar{\gamma}^{\nu_n} \gamma_\mu &= \bar{\gamma}^\mu \bar{\gamma}^{\nu_1} \dots \bar{\gamma}^{\nu_n} \bar{\gamma}_\mu + (-1)^n (D-4) \bar{\gamma}^{\nu_1} \dots \bar{\gamma}^{\nu_n}, \\
\gamma^\mu \hat{\gamma}^{\nu_1} \dots \hat{\gamma}^{\nu_n} \gamma_\mu &= \hat{\gamma}^\mu \hat{\gamma}^{\nu_1} \dots \hat{\gamma}^{\nu_n} \hat{\gamma}_\mu + 4 (-1)^n  \hat{\gamma}^{\nu_1} \dots \hat{\gamma}^{\nu_n}, \\
\bar{\gamma}^\mu \hat{\gamma}^{\nu_1} \dots \hat{\gamma}^{\nu_n} \bar{\gamma}_\mu &= 4 (-1)^n \, \hat{\gamma}^{\nu_1} \dots \hat{\gamma}^{\nu_n}, \\
\hat{\gamma}^\mu \bar{\gamma}^{\nu_1} \dots \bar{\gamma}^{\nu_n} \hat{\gamma}_\mu &= (D-4) (-1)^n  \, \bar{\gamma}^{\nu_1} \dots \bar{\gamma}^{\nu_n},
\end{align}
$$

This means that if we have a general formula for $\gamma^\mu \gamma^{\nu_1} \dots \gamma^{\nu_n} \gamma_\mu$ in $D$ dimensions, we can easily obtain 7 of 9 possible combinations of dimensions. The other two cases are special and related with each other

$$
\begin{align}
\bar{\gamma}^\mu \gamma^{\nu_1} \dots \gamma^{\nu_n} \bar{\gamma}_\mu = \gamma^\mu \gamma^{\nu_1} \dots \gamma^{\nu_n} \gamma_\mu - \hat{\gamma}^\mu \gamma^{\nu_1} \dots \gamma^{\nu_n} \hat{\gamma}_\mu.
\end{align}
$$

If we know $\bar{\gamma}^\mu \gamma^{\nu_1} \dots \gamma^{\nu_n} \bar{\gamma}_\mu$ we can easily compute $\hat{\gamma}^\mu \gamma^{\nu_1} \dots \gamma^{\nu_n} \hat{\gamma}_\mu$ and vice versa.

For purely four dimensional chains it is known that if the number of the Dirac matrices between the index pair is odd, then

$$
\begin{align}
\bar{\gamma}^\mu \bar{\gamma}^{\nu_1} \dots \bar{\gamma}^{\nu_{2n+1}} \bar{\gamma}_\mu & = -2 \bar{\gamma}^{\nu_{2n+1}} \dots \bar{\gamma}^{\nu_1}.
\end{align}
$$

This can be trivially generalized to even chains, i.e.

$$
\begin{align}
\bar{\gamma}^\mu \bar{\gamma}^{\nu_1} \dots \bar{\gamma}^{\nu_{2n+1}} \bar{\gamma}^\rho \bar{\gamma}_\mu & = 2 \bar{\gamma}^{\nu_{2n+1}} \dots \bar{\gamma}^{\nu_1} \bar{\gamma}^\rho + 2 \bar{\gamma}^\rho \bar{\gamma}^{\nu_1} \dots \bar{\gamma}^{\nu_{2n+1}}
\end{align}
$$

Following formula for $\gamma^\mu \gamma^{\nu_1} \dots \gamma^{\nu_n} \gamma_\mu$ in d dimensions and $n\geq 3$ is given in Veltman's Gammatrica

$$
\begin{align}
\gamma^\mu \gamma^{\nu_1} \dots \gamma^{\nu_n} \gamma_\mu &= (d-4)(-1)^n \gamma^{\nu_1} \dots \gamma^{\nu_n} + 2(-1)^n \gamma^{\nu_3} \gamma^{\nu_2} \gamma^{\nu_1} \gamma^{\nu_4}\dots \gamma^{\nu_n} \\
& + 2 \sum_{j=4}^m (-1)^{n-j} \gamma^{\nu_j} \gamma^{\nu_1} \dots \gamma^{\nu_{j-1}} \gamma^{\nu_{j+1}} \dots \gamma^{\nu_{n}}.
\end{align}
$$

Another useful and more compact formula for the same expression with $n \geq 2$ was derived my R. Mertig

$$
\begin{align}
\gamma^\mu \gamma^{\nu_1} \dots \gamma^{\nu_n} \gamma_\mu &= (-1)^n \biggl \{ (d-2n) \gamma^{\nu_1} \dots \gamma^{\nu_n} \\
& -4 \sum_{i=1}^{l-1} \sum_{j=i+1}^{l} (-1)^{j-i}  \gamma^{\nu_1} \dots \gamma^{\nu_{i-1}} \gamma^{\nu_{i+1}} \dots \gamma^{\nu_{j-1}} \gamma^{\nu_{j+1}} \dots \gamma^{\nu_n} g_{{\mu_i} {\mu_j}} \biggr \}
\end{align}
$$