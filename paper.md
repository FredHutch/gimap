---
title: 'gimap: An R Package for Genetic Interaction Mapping in Dual-Target CRISPR Screens'
tags:
  - R
  - CRISPR
  - genetic interactions
  - paralogs
  - synthetic lethality
  - bioinformatics
authors:
  - name: Candace Savonen
    orcid: 0000-0001-6331-7070
    affiliation: "1, 2"
  - name: Phoebe Parrish
    orcid: 0000-0002-3530-0105
    affiliation: 1
  - name: Kate Isaac
    orcid: 0000-0001-8701-4197
    affiliation: 1
  - name: Daniel Groso
    affiliation: 1
  - name: Marissa Fujimoto
    affiliation: 1
  - name: Siobhan O'Brien
    orcid: 0000-0003-4958-1080
    affiliation: 1
  - name: Alice Berger
    orcid: 0000-0002-6538-2658
    affiliation: 1
affiliations:
 - name: Fred Hutchinson Cancer Center, United States
   index: 1
 - name: Synthesize Bio, United States
   index: 2
date: 02 April 2025
bibliography: paper.bib
---

# Summary

The gimap (Genetic Interaction MAPping) R package addresses a fundamental challenge in genomic research: the difficulty of understanding combinatorial interactions among genes. Gene redundancy makes traditional single-gene knockout methods ineffective for identifying therapeutic targets, as backup genes can mask the effects when a single gene is disabled. gimap offers a solution by providing a comprehensive framework for analyzing dual-target CRISPR screening data, where two genes are simultaneously disabled to reveal their backup relationships. This software implements the methods used by @parrish_discovery_2021. The package processes raw count data through a multi-step pipeline that includes normalization, calculation of expected and observed CRISPR scores, computation of genetic interaction scores, and statistical analysis to identify significant interactions. Unlike general  tools, gimap is specifically tailored for paired guide CRISPR data with built-in quality control reporting and visualization tools. The package makes best practices the default options and is available on GitHub with comprehensive documentation to support the research community in extracting meaningful insights from complex genetic screening experiments.

# Statement of Need

When multiple genes have the same function, a common result of evolutionary processes, it becomes challenging to isolate their true functions. This redundancy makes it hard to identify effective therapeutic targets using traditional methods that disable just one gene at a time [citation suggestion?]. A more effective approach involves disabling two genes simultaneously to reveal these backup relationships [@thompson_combinatorial_2021].

Recent advances in CRISPR technology now allow researchers to knock out gene pairs at once, offering a powerful solution to this problem (https://pubmed.ncbi.nlm.nih.gov/31911676/). Although software solutions exist for single knockout CRISPR, such as MAGeCK, there is no standardized software solution for paired gene CRISPR studies [@mageck].

The R package, called `gimap` (Genetic Interaction MAPping), was developed specifically for analyzing these dual-target CRISPR experiments. It helps researchers identify important relationships between genes, such as when two genes work together or when disabling both creates a dramatic effect that wouldn't occur by disabling either one alone.

`gimap` is specifically tailored to handle the unique characteristics of paired guide CRISPR data, including the distinction between single-targeting and double-targeting constructs and the need to account for differential double-strand break effects. The package seamlessly integrates with data generated using a specialized pgPEN library but can be adapted for most paired guide CRISPR screening approaches [@parrish_discovery_2021].

# Implementation

`gimap` addresses this need by providing a comprehensive analytical framework for dual-target CRISPR screening data. The package performs several critical functions: (1) normalization of read count data to account for variable sequencing depth and technical biases, (2) calculation of CRISPR scores that reflect the effect of gene knockouts on cell proliferation, (3) determination of expected CRISPR scores for gene pairs based on single-gene effects, (4) computation of genetic interaction scores that quantify deviations from expected effects, and (5) statistical analysis to identify significant interactions.

## Overall design philosophy

In order to ensure usability for the research community we built `gimap` using the following design philosophy.

1. Making best practices as default options and including warning messages for when alternative options are chosen (e.g. if filtering has not been applied).  
2. We also tried to mimic usage elements from familiar packages such as fastqc reports (our `run_qc()` function creates such a report) [@fastqc].
3. Try to document and inform users of the statistics and decisions that have been made by the software clearly!

## gimap data handling

`gimap` implements a multi-step analysis pipeline:

![gimap workflow completes 3 main steps. Part A, B, and C of the figure show the major steps of the workflow which are to normalize the data through a multi step process, score genetic interactions based on the expected versus observed scores, and finally to calculate statistics to identify statistically significant genetic interactions.](figure.png)

1. **Normalize Data**: Raw count data is transformed into log2 counts per million (CPM) and adjusted by subtracting pre-treatment values to obtain log2 fold changes. These are further normalized based on the distribution of negative  (e.g. safe-targeting or non-targeting controls) and positive controls (pgRNAs targeting known essential genes).

  a. **Log2 Counts Per Million (CPM) Transformation**:
    - Let $C_{i,j}$ be the raw count for gene $i$ in sample $j$
    - Let $N_j$ be the total number of counts in sample $j$

    The log2 CPM transformation is:
    $$L_{i,j} = \log_2\left(\frac{C_{i,j} \times 10^6}{N_j} + 1\right)$$
    (The +1 is often included to avoid log(0) issues)

  b. **Adjustment by Pre-treatment Values**:
    - Let $L_{i,j}^{post}$ be the log2 CPM value post-treatment
    - Let $L_{i,j}^{pre}$ be the log2 CPM value pre-treatment

   The log2 fold change is:
   $$LFC_{i,j} = L_{i,j}^{post} - L_{i,j}^{pre}$$

  c. **Normalization Based on Controls**:
    - Let $LFC_{i,j}$ be the log2 fold change calculated above
    - Let $\mu_{neg}$ and $\sigma_{neg}$ be the mean and standard deviation of negative controls (safe-targeting or non-targeting)
    - Let $\mu_{pos}$ and $\sigma_{pos}$ be the mean and standard deviation of positive controls (pgRNAs targeting essential genes)

    The normalized score is:
    $$Z_{i,j} = \frac{LFC_{i,j} - \mu_{neg}}{\mu_{neg} - \mu_{pos}}$$

    Or alternatively, using a more complex normalization that accounts for the distributions of both control types:
    $$Z_{i,j} = \frac{LFC_{i,j} - \mu_{neg}}{\sigma_{neg}} \times \frac{\sigma_{pos}}{\mu_{neg} - \mu_{pos}}$$

This equation represents the transformation from raw count data to normalized log2 fold changes, calibrated against both negative and positive control distributions.

2. **Score Genetic Interactions**: For double-targeting constructs, expected CRISPR scores are calculated as the sum of the corresponding single-targeting scores. For single-targeting constructs, the expected score combines the single-target effect with the mean effect of control constructs. Interaction scores represent the difference between observed and expected CRISPR scores, adjusted using a linear model to account for systematic biases:

For double-targeting constructs:
- Let $S_{i,j}^{obs}$ be the observed CRISPR score for a construct targeting genes $i$ and $j$
- Let $S_i$ be the single-targeting score for gene $i$
- Let $S_j$ be the single-targeting score for gene $j$

The expected score for a double-targeting construct is:
$$S_{i,j}^{exp} = S_i + S_j$$

For single-targeting constructs:
- Let $S_i^{obs}$ be the observed CRISPR score for a construct targeting gene $i$
- Let $\mu_{control}$ be the mean effect of control constructs

The expected score for a single-targeting construct is:
$$S_i^{exp} = S_i + \mu_{control}$$

The interaction score calculation, with adjustment for systematic biases:
- Let $I_{i,j}$ be the interaction score for genes $i$ and $j$
- Let $S_{i,j}^{obs}$ be the observed score
- Let $S_{i,j}^{exp}$ be the expected score
- Let $\beta_0, \beta_1, \beta_2, ..., \beta_k$ be coefficients for the linear model
- Let $X_1, X_2, ..., X_k$ be variables accounting for systematic biases (which could include factors like position effects, targeting efficiency, etc.)

The interaction score calculation:
$$I_{i,j} = S_{i,j}^{obs} - S_{i,j}^{exp} - (\beta_0 + \beta_1X_1 + \beta_2X_2 + ... + \beta_kX_k)$$

Where the genetic interaction score is the difference between observed and expected CRISPR scores, with systematic biases accounted for using a linear model adjustment.

3. **Calculate Statistics**: T-tests compare the distribution of double-targeting genetic interaction scores against the background distribution of single-targeting scores, with false discovery rate correction for multiple hypothesis testing.

- $S_{double}$ as the set of double-targeting genetic interaction scores
- $S_{single}$ as the set of single-targeting scores (background distribution)
- $\mu_{double}$ as the mean of double-targeting scores
- $\mu_{single}$ as the mean of single-targeting scores
- $\sigma_{double}$ as the standard deviation of double-targeting scores
- $\sigma_{single}$ as the standard deviation of single-targeting scores
- $n_{double}$ and $n_{single}$ as the sample sizes

The t-test statistic would be:

$$t = \frac{\mu_{double} - \mu_{single}}{\sqrt{\frac{\sigma_{double}^2}{n_{double}} + \frac{\sigma_{single}^2}{n_{single}}}}$$

For each comparison, we calculate a p-value from this t-statistic.

Then, to account for multiple hypothesis testing, we apply false discovery rate (FDR) correction:

1. Order all p-values: $p_{(1)} \leq p_{(2)} \leq ... \leq p_{(m)}$
2. For a given FDR threshold $\alpha$ (e.g., 0.05), find the largest $k$ such that:
   $$p_{(k)} \leq \frac{k}{m} \cdot \alpha$$
3. Reject the null hypothesis for all tests with p-values $\leq p_{(k)}$

The package provides comprehensive visualization tools including volcano plots to highlight significant genetic interactions and detailed result tables for further analysis.

# Use Cases

`gimap` has been successfully used to identify synthetic lethal interactions among paralog genes in cancer cell lines, revealing potential therapeutic targets where single-gene approaches have failed. The package accommodates various experimental designs, including time-course studies and treatment comparisons, offering flexibility for diverse research questions.

_Example applications include:_  
- Identification of backup genes that provide functional redundancy in critical cellular pathways  
- Discovery of context-dependent genetic interactions that emerge under specific conditions or treatments  
- Systematic mapping of gene networks based on functional interactions rather than physical associations  

# Conclusion

`gimap` provides a robust, accessible framework for analyzing paired guide CRISPR screening data and identifying genetic interactions with potential biological and therapeutic significance. By streamlining the computational workflow from raw counts to statistically rigorous interaction scores, `gimap` enables researchers to efficiently extract meaningful insights from complex genetic screening experiments. The package is available on GitHub (https://github.com/FredHutch/gimap) with comprehensive documentation and tutorials to facilitate adoption by the research community.

# Acknowledgements

This work is funded by NCI grant R01CA262556 and the Translational Data Science IRC of Fred Hutchinson Cancer Center. SO is a Washington Research Foundation postdoctoral fellow.

# References
