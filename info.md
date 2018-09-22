---
title: "Law of the Iterated Logarithm"
author: "LUQIN GAN"
date: "11/27/2017"
output: html_document
---

# Law of Iterated Logarithm

## Theorem

Let ${X_n}$ be independent and identically distributed random variable with mean zero and variance one. Let $S_n = X_1 + ... + X_n$. Then 

$$
\limsup_{n\to{\infty}}\frac{S_n}{\sqrt{n\log{\log{n}}}} = \sqrt{2}~~~ \text{a.s.}
$$

$$
\liminf_{n\to{\infty}}\frac{S_n}{\sqrt{n\log{\log{n}}}} = -\sqrt{2}~~~ \text{a.s.}, 
$$

Therefore, $\frac{S_n}{\sqrt{n\log{\log{n}}}}$ oscillates between ±$\sqrt{2}$. 


## Connection with CLT and LLN

### Central limit theorem 
Let ${X_n}$ be independent and identically distributed random variable with mean zero and variance one. Let $S_n = X_1 + ... + X_n$. Then 
$$
\lim_{n\to{\infty}}\frac{S_n}{\sqrt{n}} \to N(0,1)
$$

### Strong Law of Large Number 
Let ${X_n}$ be independent and identically distributed random variable with mean zero and variance one. Let $S_n = X_1 + ... + X_n$. Then 
$$
\lim_{n\to{\infty}}\frac{S_n}{n}\to~~~0 ~~~\text{a.s.}, 
$$



So in some sense $S_n/n$ "squeezes" down to one point whereas $S_n/\sqrt{n}$ "spreads out", roughly between -3 and 3, and $S_n/\sqrt{n}$ is a continous distirbution. 

The sequence given by the law of iterated logarithm form a truncated distribution, which is 'in-between' the central limit theorem and strong law of large number. 




## Sampling to Foregone Conclusion 

The asymptotic property of the law of the iterated logarithm also explains sampling to foregone conclusion in sequential analysis. Considering  a sample of independent and identically distributed random variable $X_1$, $X_2$, $\cdots$, $X_n$ with mean $0$ and variance $1$, $S_n = \sum_{i}^{n} X_i$, similar to above.  We would like to test the null hypothesis $H_0: \mu = 0$, which is indeed true in this case.  An example method of sampling to a foregone conclusion would be as follows: 1) set a threshold $t > 0$, 2) take sample of size $n$ and reject if $\frac{|S_n|}{\sqrt{n}} \geq t$, 3) if $\frac{|S_n|}{\sqrt{n}} < t$, take an additional observation.  Thus, the sample size grows and more tests are done.  After diving by $\sqrt{\log\log(n)}$, we see that this test is equivalent to:
$$
\frac{|S_n|}{\sqrt{n \log\log(n)}} \geq \frac{t}{\sqrt{\log\log(n)}}
$$

The right hand side converges to $0$ as $t$ is constant with respect to $n$. The LIL tells us that $\frac{|S_n|}{\sqrt{n \log\log(n)}}$ will be bounded away from zero as $n \to \infty$.  Thus, we will reject the null hypothesis with probability $1$ as $n \to \infty$ \citep{armitage1967some}. Thu, we would always reject $H_0$ eventually in this sequential testing procedure, even though null hypothesis is true. 

However, it is also ensured that we cannot prove any significant difference with probability 1, by the law of iterated logarithm. In the Sampling to Foregone Conclusion tabPanel, user can explore this property by changing the value of significant difference delta. 





