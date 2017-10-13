---
layout: single
title: "Baby Name Trends, Revisited"
description: "The latest in baby naming trends"
output: html_document
date: 2017-10-12
category: r
tags: [r, baby names]
comments: true

---



I often get asked by co-op students at work about how they can get started with
using R. While sites like Kaggle are great for finding lots of datasets and
entering competitions to see how many tenths of a point you can extract from
your model, my advice to those starting it is to pick a topic or question
that actually interests you. It's a hundred times easier to do an analysis
on something that you've been pondering than on fifty columns of anonymized,
standardized numbers.

One example I gave was baby names. I have this feeling that the way people
are naming their babies have changed over the years. Hilary Parker wrote,
infamously perhaps, about [The Most Poisoned Name in US History](https://hilaryparker.com/2013/01/30/hilary-the-most-poisoned-baby-name-in-us-history/).

The Province of Ontario, which happens to be the Province in which I live,
has a nice open dataset on baby names. Any name that has at least 5 registrations
in that year will appear on this list, so it's a pretty extensive dataset.

What's nice is that it's fairly easy to use, with only three columns, in a
decent CSV format. It will lend itself well to analysis, as well as an easy
way to show some of the fundamentals of the R language.

## Get the data

The first step in any R project is to get some data. Fortunately, with the
readr package and a direct-link this is easy. We skip the first line
as it is just a title, and not the headers.


{% highlight r %}
# If you haven't installed the tidyverse yet, you may wish to do so
# install.packages(tidyverse)
library(tidyverse)
female <- read_csv("https://files.ontario.ca/opendata/ontariotopbabynames_female_1917-2013_english.csv",
                  skip = 1)
male <- read_csv("https://files.ontario.ca/opendata/ontariotopbabynames_male_1917-2013_english.csv",
                 skip = 1)

glimpse(female)
{% endhighlight %}



{% highlight text %}
## Observations: 87,585
## Variables: 3
## $ Year      <int> 2013, 2013, 2011, 2013, 2011, 2005, 2006, 2007, 2008...
## $ Name      <chr> "AADHYA", "AADYA", "AAHANA", "AAIMA", "AAIRA", "AALI...
## $ Frequency <int> 13, 6, 7, 6, 7, 7, 5, 5, 7, 5, 8, 7, 6, 6, 18, 11, 2...
{% endhighlight %}

## Clean the data

We have between 70,000 and 87,000 names depending on the gender of the child at
birth. We'll combine these into one table so that we can manipulate and plot
against the entire dataset, rather than repeating functions.

We'll convert gender to a factor too, for easier summaries.


{% highlight r %}
library(magrittr)
male %<>% # The compound assignment operator pipes the object forward then updates the object
  mutate(Gender = "Male")

female <- female %>% # The above is equivalent to using <- with %>%
  mutate(Gender = "Female")

babynames <- bind_rows(female, male) %>%
  mutate(Gender = factor(Gender))

summary(babynames)
{% endhighlight %}



{% highlight text %}
##       Year          Name             Frequency          Gender     
##  Min.   :1917   Length:157818      Min.   :   5.00   Female:87585  
##  1st Qu.:1958   Class :character   1st Qu.:   7.00   Male  :70233  
##  Median :1982   Mode  :character   Median :  13.00                 
##  Mean   :1977                      Mean   :  63.78                 
##  3rd Qu.:2000                      3rd Qu.:  37.00                 
##  Max.   :2013                      Max.   :3946.00                 
##                                    NA's   :2
{% endhighlight %}

Running summary, we see that there two NA's, which is worth investigating.


{% highlight r %}
babynames %>%
  filter(is.na(Frequency))
{% endhighlight %}



{% highlight text %}
## # A tibble: 2 x 4
##    Year              Name Frequency Gender
##   <int>             <chr>     <int> <fctr>
## 1  1958 CATHERINE-ELIZABE        NA Female
## 2  1963 MARIA DA CONCEICA        NA Female
{% endhighlight %}

Interesting that these both seem to be cut off. There must be something wrong
with the source dataset. A quick investigation confirms this belief. We have
these two lines in our data.

```
1958,CATHERINE-ELIZABE,TH           5
1963,MARIA DA CONCEICA,O            5
```

Let's clean this up with a simple ifelse function. There are a lot of
ways of doing this, but we'll just check for Year and Name and replace. We could
also replace all NAs with 5, but if more NAs creep into our data in the future
this might not be ideal.


{% highlight r %}
babynames %<>%
  mutate(Frequency = ifelse((Year == 1958 & Name == "CATHERINE-ELIZABE") |
                               (Year == 1963 & Name == "MARIA DA CONCEICA"), 5, Frequency))
summary(babynames)
{% endhighlight %}



{% highlight text %}
##       Year          Name             Frequency          Gender     
##  Min.   :1917   Length:157818      Min.   :   5.00   Female:87585  
##  1st Qu.:1958   Class :character   1st Qu.:   7.00   Male  :70233  
##  Median :1982   Mode  :character   Median :  13.00                 
##  Mean   :1977                      Mean   :  63.77                 
##  3rd Qu.:2000                      3rd Qu.:  37.00                 
##  Max.   :2013                      Max.   :3946.00
{% endhighlight %}

There, that looks better. Now we're ready to explore.

## Data Exploration

### Suffixation

One of the first trends I feel I've noticed is that the last letter of first
names is changing over time. It seems like we've hit a trend with the -A suffix
especially for female names. Let's see if that holds true.

We use proportions rather than frequency to balance out for the increase
in population over time.

We use the excellent gghighlight package to help seperate out the top
letters from the rest of the pack.


{% highlight r %}
library(stringr)
library(gghighlight)
library(scales)

last_count <- babynames %>%
  mutate(Last_Letter = str_extract(Name, "[A-Z]$")) %>%
  count(Gender, Year, Last_Letter, wt = Frequency, sort = TRUE) %>%
  group_by(Gender, Year) %>%
  mutate(Prop = n / sum(n))

source <- "Source: ServiceOntario Top Baby Names, January 1, 1917 - December 31, 2013"

g <- gghighlight_line(last_count, aes(Year, Prop, color = Last_Letter),
                 predicate = max(Prop) > 0.2) +
  facet_wrap(~ Gender) +
  scale_y_continuous(labels=percent) +
  labs(y = "Proportion", title = "Proportion of the Last Letter in Ontario Baby First Names",
       caption = source)
g
{% endhighlight %}

<img src="/figs/2017-10-12-Baby-Names-Revisited/unnamed-chunk-5-1.png" title="center" alt="center" width="80%" style="display: block; margin: auto;" />

The trend is pretty interesting. With Females, names that end in -A have
skyrocketed, the explosion starting in the early 60s, just as names ending in -E
became less popular. Names that end with -Y or -N have not seen much of a change.

For males, nothing even comes close to names ending with -N. For a while names
with -D were leading in the 40s, but that fell out of a fashion in the 50s,
and -N names make up 35% of all baby names in Ontario.

Let's add some representative names for effect. We'll get the top names for the
top 5 last letters and add them to our plot.


{% highlight r %}
library(ggrepel)

top_letters <- c('A', 'D', 'E', 'N', 'Y')
last_letter_prop <- last_count %>%
    filter(Last_Letter %in% top_letters) %>%
    group_by(Gender, Last_Letter) %>%
    filter(Prop == max(Prop)) %>%
    select(Gender, Year, Last_Letter, Prop)

top_name_by_year <- babynames %>%
  mutate(Last_Letter = str_extract(Name, "[A-Z]$")) %>%
  group_by(Year, Gender, Last_Letter) %>%
  top_n(1, wt = Frequency)

last_letter_labels <- inner_join(last_letter_prop, top_name_by_year)

g +
  geom_label_repel(aes(x = Year, y = Prop, color = Last_Letter, label = Name),
                  data = last_letter_labels, nudge_x = -10, size = 3)
{% endhighlight %}

<img src="/figs/2017-10-12-Baby-Names-Revisited/unnamed-chunk-6-1.png" title="center" alt="center" width="80%" style="display: block; margin: auto;" />

### The Snowflake Effect

Another theory I had is that more and more, parents are giving their children
unique names. There are a few ways to test this. We could look at how
common the most common names are, or how many unique names are introduced in
a given year.


{% highlight r %}
snowflakes <- babynames %>%
  group_by(Gender, Year) %>%
  mutate(Prop = Frequency / sum(Frequency)) %>%
  top_n(1, wt = Prop)

ggplot(snowflakes, aes(Year, Prop, color = Gender)) +
  geom_line() +
  scale_y_continuous(labels = percent) +
  labs(y = "Top Name as % of All Names",
       title = "Trend of Most Popular Name as % of all Given Names in Ontario",
       caption = source)
{% endhighlight %}

<img src="/figs/2017-10-12-Baby-Names-Revisited/unnamed-chunk-7-1.png" title="center" alt="center" width="80%" style="display: block; margin: auto;" />


{% highlight r %}
name_year_count <- babynames %>%
  count(Name) %>%
  arrange(n)

first_name_occurence <- babynames %>%
  group_by(Name) %>%
  summarise(min_year = min(Year))

inner_join(name_year_count, first_name_occurence) %>%
  filter(n==1) %>%
  ggplot(aes(min_year)) +
  geom_bar()  +
  labs(x = "Year", y = "# of Unique Names Introduced",
       title = "Unique names introduced in a given year significantly increased since the 90s",
       subtitle = "Aggregate count of given names introduced in a given year",
       caption = source)
{% endhighlight %}

<img src="/figs/2017-10-12-Baby-Names-Revisited/unnamed-chunk-8-1.png" title="center" alt="center" width="80%" style="display: block; margin: auto;" />

### Gender Neutral

Another question I had was if names are becoming more or less gendered over time.
One way to judge the gender-neutralness of a name would be to see if the same name
appears in both Male and Female lists for a given year.


{% highlight r %}
gendered_names <- babynames %>%
  count(Name, Year) %>%
  filter(n == 2) %>%
  count(Year, sort = TRUE)

gendered_names %>%
  ggplot(aes(Year, nn)) +
  geom_line() +
  labs(y = "# of gender-neutral names",
       title = "Increasing trend of gender-neutral names",
       subtitle = "Gender neutral identified as a name that appears in both
       male and female lists in a given year",
       caption = source)
{% endhighlight %}

<img src="/figs/2017-10-12-Baby-Names-Revisited/unnamed-chunk-9-1.png" title="center" alt="center" width="80%" style="display: block; margin: auto;" />

We can see that starting in the mid-80s, there's been a sharp rise in gender
neutral names, however that trend seems to have peaked in 2008, and decreasing
since.

## Finishing Off

So! Hopefully you saw how much you can do with 'small-data' using the power of
R and the Tidyverse. With very little code I was able to generate many charts,
spending more time thinking about questions than trying to figure out how to
answer it.
