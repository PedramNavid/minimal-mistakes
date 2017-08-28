---
title: "A/B Testing Overview"
author: "Pedram Roshdinavid"
date: '2017-08-27'
output: html_notebook
layout: single
excerpt: An easy walkthrough of the basics of A/B testing, using R. 
---

## Overview and Definitions

The purpose of A/B testing is to determine through the use
of statistical methods whether an experiment generates enough
of a practically significant effect to support implementation.

This is not as simple as seeing if the rates of two different groups
are different, because of the inherent randomness in sampling from a population.

Consider this toy example:


{% highlight r %}
library(scales)
set.seed(1234)
pop_1 <- rnorm(100, 0, 1)
pop_2 <- rnorm(100, 0, 1)

paste("The mean of pop_1 is: ", comma(mean(pop_1)))
{% endhighlight %}



{% highlight text %}
## [1] "The mean of pop_1 is:  -0.1567617"
{% endhighlight %}



{% highlight r %}
paste("The mean of pop_2 is: ", comma(mean(pop_2)))
{% endhighlight %}



{% highlight text %}
## [1] "The mean of pop_2 is:  0.04124318"
{% endhighlight %}

Even though both these samples where taken from a population with mean 0
and standard deviation 1, the mean of both populations appears different.

Shown graphically:

{% highlight r %}
library(ggplot2)
library(dplyr)
df <- data_frame(p1 = pop_1, p2 = pop_2)
ggplot(df) +
  geom_density(aes(p1, color = 'pop_1')) +
  geom_density(aes(p2, color = 'pop_2'))
{% endhighlight %}

![center](/figs/2017-08-27-Intro-to-AB-Testing-R/unnamed-chunk-2-1.png)

That difference is due to chance, and not because of an inherent difference
in the population of the two samples. A/B Testing is all about trying to
detect whether we've been able to have an effect on the population and to tease
apart the size of that effect, if it exists.

## A Little Demo

Let's pretend we are running a website that has
made a change to the shopping cart checkout process. We want to know
if the change we've introduced to a subset of our visitors has made
a signficant enough change to warrant implementing on our site.

Let $n = $ *the number of unique visitors to our site*, and
$X = $ *the number of unique visitors that placed an order*.

Then $\hat{p} = X / n$ is the probability of a checkout for a unique visitor.

The distribution of this event is binomial, so a unique visitor
will have had a checkout (a success) or not (a failure).

Because these events are independent and identically distributed, we should
be able to compare our control-group (the website without change) with our
experimental-group (the website following a change) to see
whether there is a significant difference.

If we assume a binomial distribution with a sufficient sample size,
then we can use the normal distributions Z-scores along with
the standard error to help calculate a margin of error.

A margin of error gives us the confidence interval for a point estimate.
Because we are pulling a sample from a population, we can give a range
for our point estimate with a certain level of confidence, say 95%.

What this means is that 95% of the time, the actual population mean will be within
the confidence interval we provided, assuming a normal distribution (of course).

Our margin of error is $m = Z * SE$
and $SE = \sqrt{\frac{\hat{p}(1 - \hat{p})}{N}}$
Our confidence interval would then be our sample estimate +/- the
margin of error.

We'll walk through a real example to help illustrate this now.

## Sample Distributions

Before running our experiment, our web-team provided us with these numbers for
the previous month. This is only to get us familiar with some concepts,
and we'll be performing the actual test next month, so the results aren't
as important as the concepts here.

We had 20,000 unique visitors, and of those 20,000 unique visitors, 3,000
of them placed an order. We'd like to know what the probability of any
unique visitor placing an order is.


{% highlight r %}
library(scales)
x = 3000
n = 20000
p_hat <- x / n
paste("Probability of an order for a given unique customer:", p_hat)
{% endhighlight %}



{% highlight text %}
## [1] "Probability of an order for a given unique customer: 0.15"
{% endhighlight %}

The point-estimate is 15%, but what is the margin of error for this estimate?
Our 20,000 unique visitors is only a sample of the entire population, so
let's calculate our confidence interval, at 95% confidence.


{% highlight r %}
SE = sqrt(p_hat * (1 - p_hat) / n)
m = qnorm(1 - 0.025) * SE # 2.5% with two-tails gives us 95% confidence
lower = p_hat - m
upper = p_hat + m
paste("Lower bound: ", percent(lower),
      "Upper bound: ", percent(upper))
{% endhighlight %}



{% highlight text %}
## [1] "Lower bound:  14.5% Upper bound:  15.5%"
{% endhighlight %}

Our estimate for the true probability of an order, with 95% confidence,
is 14.5% to 15.5%.

## Hypothesis Testing

Now that we understand confidence intervals, let's perform our experiment and see
if our experiment performs better than a control.

The following month the web team gives us this data. The control group is the
website without any modifications, the experiment group is the website with
a change. Note how both groups have different size populations? This is very
common with web-traffic experiments.

$$X_{control} = 974$$

$$N_{control} = 10,072$$

$$X_{experiment} = 9,886$$

$$N_{experiment} = 1,242$$

We'll need the stanard error for these two measures. One easy way to get
the standard error is to pool the results together and get an overall
probability of a click, and use that number to get what is called a pooled
standard error.

The pooled probability $\hat{p}_{pool}$ is the overall probability of a click
across both control and experiments: $\frac{974 + 1242}{10072 + 9886}$.


{% highlight r %}
x_ctrl <- 974
n_ctrl <- 10072
x_exp <- 1241
n_exp <- 9886
p_pool = (x_ctrl + x_exp) / (n_ctrl + n_exp)
p_pool
{% endhighlight %}



{% highlight text %}
## [1] 0.1109831
{% endhighlight %}

The standard error of the pool is:

$$ \sqrt{\hat{P}_{pool} * (1 - \hat{P}_{pool}) * (\frac{1}{N_{ctrl}} + \frac{1}{Nexp})} $$



{% highlight r %}
SEpool = sqrt(p_pool * (1-p_pool) * (1/n_exp + 1/n_ctrl))
SEpool
{% endhighlight %}



{% highlight text %}
## [1] 0.004447067
{% endhighlight %}

Now that we have the standard error, we can calculate our point estimates
for the two different groups, and use the standard error to generate confidence
intervals. If the estimates are within the confidence intervals of each other
then we'll have to conclude our experiment had no observable effect. If, however,
the bounds of the confidence intervals are separated, then we'll be able to say
with a certain degree of confidence that there is a measurable effect.

In other words, we're looking at two distributions, $ d_{experiment} $ and $ d_{control} $, and calculating whether $ \hat{d} = d_{experiment} - d_{control} = 0 $, where $ \hat{d} $ is the difference between the two distributions.



{% highlight r %}
# Point estimates for experiment and control
p_exp = x_exp / n_exp
p_ctrl = x_ctrl / n_ctrl

# Difference between the two
d_hat = p_exp - p_ctrl
percent(d_hat)
{% endhighlight %}



{% highlight text %}
## [1] "2.88%"
{% endhighlight %}

The difference between our experiment and control is an increase of 2.88%.
Let's compute the margin of error using the standard error.


{% highlight r %}
m = qnorm(1 - 0.025) * SEpool
paste("Lower bound:", percent(d_hat - m),
       "Upper bound:", percent(d_hat + m))
{% endhighlight %}



{% highlight text %}
## [1] "Lower bound: 2.01% Upper bound: 3.75%"
{% endhighlight %}

With 95% confidence, we can say that there was an increase of 2.0% to 3.8%
following this experiment. Since the margin of error is a safe distance away
from 0%, we can conclude with some confidence that our experiment is a success.

## The Easy Way

What if we didn't remember all these formulas but still wanted an answer?
Let's try to see if we can replicate what we just did


{% highlight r %}
prop.test(c(x_exp, x_ctrl), c(n_exp, n_ctrl), p = NULL)
{% endhighlight %}



{% highlight text %}
## 
## 	2-sample test for equality of proportions with continuity
## 	correction
## 
## data:  c(x_exp, x_ctrl) out of c(n_exp, n_ctrl)
## X-squared = 41.729, df = 1, p-value = 1.049e-10
## alternative hypothesis: two.sided
## 95 percent confidence interval:
##  0.02001096 0.03764369
## sample estimates:
##     prop 1     prop 2 
## 0.12553105 0.09670373
{% endhighlight %}

There we go, in one line we've tested whether the two groups have the
same probability as our null hypothesis. Our alternative hypothesis is that
there is a statistically significant difference between the two.

Looking at the summary results, we can see that the p-value is practically 0,
which means there's a very low likelihood of seeing a difference that big
merely due to chance. Further, we can see the confidence interval of the
difference between the two groups to be between 2.0% and 3.8%, matching our
results above. The sample estimates for each population is also given,
with our experimental group at 12.6% and our control group at 9.7%.
