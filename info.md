---
title: "Law of the Iterated Logarithm"
author: "LUQIN GAN"
date: "11/27/2017"
output: html_document
---

# Law of Iterated Logarithm

## Theorem

#### Let ${X_n}$ be independent and identically distributed random variable with mean zero and variance one. Let $S_n = X_1 + ... + X_n$. Then 

$$
\limsup_{n\to{\infty}}\frac{S_n}{\sqrt{n\log{\log{n}}}} = \sqrt{2}~~~ \text{a.s.}
$$

$$
\liminf_{n\to{\infty}}\frac{S_n}{\sqrt{n\log{\log{n}}}} = -\sqrt{2}~~~ \text{a.s.}, 
$$

#### Therefore, $\frac{S_n}{\sqrt{n\log{\log{n}}}}$ oscillates between ±$\sqrt{2}$. 


## Connection with CLT and LLN

### Central limit theorem 
#### Let ${X_n}$ be independent and identically distributed random variable with mean zero and variance one. Let $S_n = X_1 + ... + X_n$. Then 
$$
\lim_{n\to{\infty}}\frac{S_n}{\sqrt{n}} \to N(0,1)
$$

### Strong Law of Large Number 
#### Let ${X_n}$ be independent and identically distributed random variable with mean zero and variance one. Let $S_n = X_1 + ... + X_n$. Then 
$$
\lim_{n\to{\infty}}\frac{S_n}{n}\to~~~0 ~~~\text{a.s.}, 
$$



#### So in some sense $S_n/n$ "squeezes" down to one point whereas $S_n/\sqrt{n}$ "spreads out", roughly between -3 and 3, and $S_n/\sqrt{n}$ is a continous distirbution. 

#### The sequence given by the law of iterated logarithm form a truncated distribution, which is 'in-between' the central limit theorem and strong law of large number. 










