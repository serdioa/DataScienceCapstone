---
title: 'About Next Word Prediction: Stupid Backoff'
author: "Alexey Serdyuk"
date: "31/07/2019"
output: 
  html_document: 
    keep_md: yes
---

#### Alexey Serdyuk
#### 31.07.2019

Content
=======

-   [Synopsis](#synopsis)
-   [Model](#model)
-   [Implementation](#implementation)
-   [Optimization](#optimization)
-   [Extensions](#extensions)
-   [Validation](#validation)
-   [Application](#application)
-   [References](#references)

<a name="synopsis"></a>Synopsis
===============================

This document provides the background and describes the approach used to
implement the application “Next Word Prediction: Stupid Backoff”.

The application was developed as a capstone project for the for the
cycle of courses [Data Science
Specialization](https://www.coursera.org/specializations/jhu-data-science)
offered on [Coursera](https://www.coursera.org) by [Johns Hopkins
University](https://www.jhu.edu). The purpose of the capstone project is
to build a Natural Language Processing (NLP) application that, given a
chunk of text, predicts the next most probable word. The application may
be used, for example, in mobile devices to provide suggestions as the
user enters text, or as a part of a spelling correction module in a text
editor.

You may find the following links useful:

-   Full version of this document:
    <a href="https://serdioa.github.io/DataScienceCapstone/ImplementationReport.html" class="uri">https://serdioa.github.io/DataScienceCapstone/ImplementationReport.html</a>

-   Presentation:
    <a href="https://serdioa.github.io/DataScienceCapstone/presentation/index.html" class="uri">https://serdioa.github.io/DataScienceCapstone/presentation/index.html</a>

-   Source code:
    <a href="https://github.com/serdioa/DataScienceCapstone" class="uri">https://github.com/serdioa/DataScienceCapstone</a>

<a name="model"></a>Model
=========================

Our application predicts the next word in a text given a prefix by using
a Markov Chain model simplified to n-grams.

The Markov Chain model assumes that in a natural language sentence a
probability of each word depends only on previous words. The n-gram
model simplifies the Markov Chain model by considering each word to
depend only on the previous N words, thus ignoring long-range
dependency. The n-gram model combines simplicity with an acceptable
prediction quality, making it a model of choice for many Natural
Language Processing (NLP) applications.

<a name="implementation"></a>Implementation
===========================================

Coursera provides a training text corpora HC Corpora [(1)](#hc_corpora).
The corpora contains texts in several languages collected from various
sources in Web, including blogs, news web sites and Twitter. The English
corpora consists of approximately 4.2 millions lines of text.

We splitted the English corpus on 3 parts: training (60%), testing (20%)
and validation (20%). The training part was used to train the prediction
algorithm, the testing to optimize meta-parameters, and validation for
the final validation of the algorithm.

The training corpus was used to build 1- to 5-grams. Each n-gram was
further split on a (n-1)-gram prefix (empty for 1-grams) and a
single-word suffix. Our model attempts to predict the last word (suffix)
based on previous 4 words (prefix). For each n-gram
*w<sub>1</sub>, w<sub>2</sub>, …, w<sub>n</sub>* we store in a
lookup table the prefix
*w<sub>1</sub>, w<sub>2</sub>, …, w<sub>n−1</sub>*, the suffix
*w<sub>n</sub>*, as well as
*P<sub>ML</sub>(w<sub>n</sub>\|w<sub>1</sub>, …, w<sub>n−1</sub>)*,
that is the maximum likehood estimate of the conditional probability of
the suffix *w<sub>n</sub>* given the prefix
*w<sub>1</sub>, …, w<sub>n−1</sub>*.

The prediction algorithm consists of 2 steps: choosing candidates, and
scoring candidates to select the best matches.

We decided to use very simple choosing algorithm: we choose all
available candidates, proceeding from 5-grams to 1-grams and skipping
suffixes which we have already collected. Given the prefix
*w<sub>1</sub>, w<sub>2</sub>, …, w<sub>n−1</sub>*, we choose
all 5-grams starting with the 4-gram prefix
*w<sub>n−4</sub>, w<sub>n−3</sub>, w<sub>n−2</sub>, w<sub>n−1</sub>*,
all 4-grams starting wihh the 3-gram prefix
*w<sub>n−3</sub>, w<sub>n−2</sub>, w<sub>n−1</sub>*, and
so on.

The scoring algorithm selects best N matches from all candidates by
calculating a numerical score of each candidate and choosing N
candidates with top score. The score may be a probability, but it may be
a different type of a numerical quantifier, as it is the case for the
Stupid Backoff algorithm.

The Stupid Backoff algorithm is a high-efficient scoring algorithm
proposed in 2007 by Thorsten Brants et al [(2)](#stupid_backoff). On
large data sets the algorithm gives scoring close to [Kneser-Ney
algorithm](https://en.wikipedia.org/wiki/Kneser%E2%80%93Ney_smoothing),
but is significantly faster. The Stupid Backoff algorithm returns not
probabilities, but relative scores of words (they do not sum to 1),
which is sufficient for our purposes.

The Stupid Backoff algorithm is described by the following formula:

*SB(w<sub>n</sub>\|w<sub>1</sub>, w<sub>2</sub>, …, w<sub>n−1</sub>)* =

-   *P<sub>ML</sub>(w<sub>n</sub>\|w<sub>1</sub>, w<sub>2</sub>, …, w<sub>n−1</sub>)*
    if
    *P<sub>ML</sub>(w<sub>n</sub>\|w<sub>1</sub>, w<sub>2</sub>, …, w<sub>n−1</sub>)* &gt; 0

-   *λ SB(w<sub>n</sub>\|w<sub>2</sub>, …, w<sub>n−1</sub>)* otherwise.

where
*P<sub>ML</sub>(w<sub>n</sub>\|w<sub>1</sub>, w<sub>2</sub>, …, w<sub>n−1</sub>)*
is the maximum likehood estimate of the conditional probability of the
suffix *w<sub>n</sub>* given the prefix
*w<sub>1</sub>, w<sub>2</sub>, …, w<sub>n−1</sub>*.
Authors of the Stupid Backoff algorithm recommend to use *&lambda;* = 0.4.

In other words, first we attempt to look up the n-gram in the table for
the largest n available. If the n-gram is found, than the score of the
last word is the maximum likehood estimation of the conditional
probability of the last word given the prefix. Otherwise, we back off
(hence the algorithm name) to a table with (n-1)-grams, do the same
calculations and multiply the result by *&lambda;* = 0.4. If the shorterned
prefix is not found as well, the recursion goes deeper, concluding on
1-grams.

<a name="optimization"></a>Optimization
=======================================

In our implementation of the Stupid Backoff algorithm we have applied
several optimizations to reduce the memory usage and latency.

-   In prefixes of n-grams we use only 2<sup>16</sup> − 2 words (stems)
    which appear in the training text corpus most often. These words
    cover 99.4% of the corpora. Words not included in the top list are
    replaced with a special token `UNK` (“Unknown”) when appear in
    n-gram prefixes. This optimization applies only to prefixes, the
    last word of each n-gram remains “as is”.

-   Using 2<sup>16</sup> − 2 stems allows us to encode each stem, as
    well as 2 special tokens, using 2 bytes. One of the special tokens
    is mentioned above `UNK` token, another is the `STOS`
    (“Start-Of-Sentence”) token.

-   R is not especially rich on low-level data types. It provides a type
    `raw` which claims to work with bytes, but this type is very
    storage-inefficient. We have to use available primitive data types
    `integer` and `numeric` to encode n-gram prefixes. R stores the data
    type `integer` using 4 bytes, and we have used it to encode 1- and
    2-stem prefixes. The data type `numeric` has 8 bytes, and we have
    encoded 3- and 4-stem prefixes in it. Using binary encoding allows
    to noticeably reduce memory for storing n-grams. All n-gram tables
    without binary encoding of prefixes require 7,265.2 MiB, whereas
    after encoding prefixes they require only 2,645.9 MiB, reducing the
    memory usage by 63.58%.

-   Conditional probabilities in n-gram tables are naturally represented
    by R `numeric` type which requires 8 bytes. We may reduce the
    storage by half without loosing much precision by applying a
    scaled-Log transformation: instead of storing a probability *P*, we
    store the value *int(log(P) &times; 10<sup>6</sup>)*. The R
    type `integer` require only 4 bytes, and we are storing logarithm
    with 6 digits after the decimal point (transformed to an integer).
    It is easy to check that the precision lost caused by such
    transformation is negligible. Applying this optimization reduces the
    total memory usage to 2,205.9 MiB, that is by further 16.63%.

-   We may further reduce memory requirements by removing seldom
    n-grams, that is n-grams which appear *m* times or less. Our tests
    demonstrated that we get the best prediction rate when excluding
    n-grams which appear only once and keeping all the rest. On the
    other hand, if we keep only n-grams which appear at least 6 times,
    the prediction quality falls by approximately 1.5%, but the memory
    usage is dramatically reduced. After removing seldom n-grams our
    memory requirements fall down to 53.8 MiB, that is just to 0.74% of
    the original size.

By using all optimization techniques mentioned above, we were able to
reduce the memory required to keep n-gram tables to 53.8 MiB (12 MiB in
compressed RDS format). The average time required to predict 10
top-scoring candidates was under 20 ms on our hardware.

<a name="extensions"></a>Extensions
===================================

We added to the base algoritm a simple optional extension which allows
to predict a word the user is currently typing. As long as the last
typed character is not the space character, the algorithm assumes that
the user continues to type the current word, and predicts it. Only after
a space character is typed the algorithm predicts the next word.

The extension pretty easy integrates in the step when we choose
candidate words. Given the prefix
*w<sub>1</sub>, w<sub>2</sub>, …, w<sub>n−1</sub>* and a
partially typed word *w′<sub>n</sub>* we choose all 5-gram
candidates which starts with the prefix
*w<sub>1</sub>, w<sub>2</sub>, …, w<sub>n−1</sub>* and the 5th
word starts with the word prefix *w′<sub>n</sub>*, and
similarly for 4- to 1-grams.

Some words may be missing in our dictionary, and the user may misspell
some words. If we can’t find any candidates using our n-grams, we
attempt to predict the next word using the partialy entered word by
applying a spelling correction algorithm provided by the R package
`hunspell`. For example, if the user enters a misspelled word “mashine”,
we propose the spell-corrected word “machine”.

<a name="validation"></a>Validation
===================================

As you may remember from the [Implementation](#implementation), we have
preserved 20% of the data for an off-sample validation test. The test
was done as follows:

-   Choose 100.000 random sample sentences from each source (blogs,
    news, Twitter). Split each selection on 100 batches, each of 1000
    sentences.

-   Create an aggregated test set of 100.000 sentences by choosing 1/3
    of sentences from each source. Split the aggregated test set on
    batches as well.

-   Choose a random word in a sentence, but not the very first word. Use
    the part of the sentence before the selected word as a prefix, and
    attempt to predict the selected word.

-   Run the prediction algorithm for all samples, predicting top 5
    candidates.

-   For each batch of 1000 sentences, calculate percentage of cases when
    the word actually present in the sentence was in top 1, top 3 or top
    5 of predicted candidates.

The chart below shows results of the algorithm validation. For example,
for blogs our algorithm correctly predicted the next word in 15.42% of
cases, the correct result was in top 3 predictions in 25.43% of cases,
and in top 5 in 30.50% of the cases.

![](validation.png)

The following table shows the mean quality of our prediction algorithm
(in which percentage of cases the right word was in top 1, top 3 and top
5), as well as 95% confidential interval.

<table class="table table-striped table-bordered table-hover" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Prediction precision (validation)
</caption>
<thead>
<tr>
<th style="border-bottom:hidden" colspan="1">
</th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2">
Word correctly predicted

</th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2">
Word in Top 3

</th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2">
Word in Top 5

</th>
</tr>
<tr>
<th style="text-align:left;">
Source
</th>
<th style="text-align:left;">
Mean, %
</th>
<th style="text-align:left;">
Conf. Int. 95%
</th>
<th style="text-align:left;">
Mean, %
</th>
<th style="text-align:left;">
Conf. Int. 95%
</th>
<th style="text-align:left;">
Mean, %
</th>
<th style="text-align:left;">
Conf. Int. 95%
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Aggregated
</td>
<td style="text-align:left;">
15.40
</td>
<td style="text-align:left;">
(15.15, 15.65)
</td>
<td style="text-align:left;">
25.00
</td>
<td style="text-align:left;">
(24.73, 25.26)
</td>
<td style="text-align:left;">
29.86
</td>
<td style="text-align:left;">
(29.58, 30.14)
</td>
</tr>
<tr>
<td style="text-align:left;">
Blogs
</td>
<td style="text-align:left;">
15.42
</td>
<td style="text-align:left;">
(15.19, 15.66)
</td>
<td style="text-align:left;">
25.43
</td>
<td style="text-align:left;">
(25.13, 25.73)
</td>
<td style="text-align:left;">
30.50
</td>
<td style="text-align:left;">
(30.20, 30.81)
</td>
</tr>
<tr>
<td style="text-align:left;">
News
</td>
<td style="text-align:left;">
16.16
</td>
<td style="text-align:left;">
(15.91, 16.41)
</td>
<td style="text-align:left;">
25.67
</td>
<td style="text-align:left;">
(25.41, 25.94)
</td>
<td style="text-align:left;">
30.28
</td>
<td style="text-align:left;">
(30.00, 30.55)
</td>
</tr>
<tr>
<td style="text-align:left;">
Twitter
</td>
<td style="text-align:left;">
14.77
</td>
<td style="text-align:left;">
(14.55, 14.98)
</td>
<td style="text-align:left;">
24.18
</td>
<td style="text-align:left;">
(23.95, 24.41)
</td>
<td style="text-align:left;">
29.06
</td>
<td style="text-align:left;">
(28.81, 29.31)
</td>
</tr>
</tbody>
</table>
As the table demonstrates, our predictions are more precise for news,
less precise for blogs and Twitter. This is to be expected: news use
more formal language where common expressions tend to repeat, whereas in
blogs and especially on Twitter authors often prefer brevity to
correctness.

<a name="application"></a>Application
=====================================

A [Shiny application](https://shiny.rstudio.com) using our prediction
algorithm is available online:
<a href="https://serdioa.shinyapps.io/sb-predict" class="uri">https://serdioa.shinyapps.io/sb-predict</a>.

The application GUI provides the following elements:

-   Text area: enter the text here, and predicted words will appear on
    the chart below. The chart shows multiple candidates ordered by
    score, providing a visual clue on how probable each candidate is.
    When “Predict partially entered words” is activated, **you have to
    type the space character to predict the next word**, otherwise an
    ending of the current word is predicted.

-   Number of predictions: choose from 1 to 10 candidates to predict.

-   Predict partially entered words: select the checkbox to activate the
    extension which predicts partially entered words.

-   Random sample: populate the text area with a random sample from a
    selection of over 1000 prepared sample texts.

![](application.png)

-   The application shows predictions as you type, displaying a chart
    with ranks of predicted words.

![](prediction.png)

-   The prediction itself takes less than 20 ms, most of the observed
    delay is due to the Shiny framework and network latency.

<a name="references"></a>References
===================================

<a name="hc_corpora"></a>(1) HC Corpora provided by
[corpora.epizy.com](http://corpora.epizy.com). [About the
corpora](http://corpora.epizy.com/about.html). [Download the
corpora](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip).

<a name="stupid_backoff"></a>(2) Thorsten Brants, Ashok C. Popat, Peng
Xu, Franz J. Och, Jeffrey Dean. 2007. Large Language Models in Machine
Translation. <https://www.aclweb.org/anthology/D07-1090>.
