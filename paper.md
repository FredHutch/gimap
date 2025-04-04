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
  - name: Daniel Grosso
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

Gene redundancy, particularly among paralog genes, presents significant challenges in understanding gene function through single-gene knockout experiments []. Paired guide CRISPR screening has emerged as a powerful approach to address this challenge by enabling simultaneous knockout of two genes[]. However, analyzing the resulting data requires sophisticated computational methods. GIMAP (Genetic Interaction MAPping) is an R package specifically designed to analyze dual-target CRISPR screening data, with a focus on identifying meaningful genetic interactions such as synthetic lethality and gene cooperativity.

# Statement of Need

The discovery of genetic interactions—particularly synthetic lethal interactions where simultaneous disruption of two genes causes cell death while individual disruptions do not—has significant implications for understanding cellular pathways and developing targeted therapeutics, especially in cancer research [citation]. While experimental techniques like the pgPEN (paired guide RNAs for genetic interaction mapping) library enable high-throughput screening for such interactions, extracting meaningful insights from the resulting data requires specialized computational tools [citation].

GIMAP addresses this need by providing a comprehensive analytical framework for dual-target CRISPR screening data. The package performs several critical functions: (1) normalization of read count data to account for variable sequencing depth and technical biases, (2) calculation of CRISPR scores that reflect the effect of gene knockouts on cell proliferation, (3) determination of expected CRISPR scores for gene pairs based on single-gene effects, (4) computation of genetic interaction scores that quantify deviations from expected effects, and (5) statistical analysis to identify significant interactions.

Unlike general-purpose RNA-seq analysis tools, GIMAP is specifically tailored to handle the unique characteristics of paired guide CRISPR data, including the distinction between single-targeting and double-targeting constructs and the need to account for differential double-strand break effects. The package seamlessly integrates with data generated using a specialized pgPEN library but can be adapted for any paired-guide CRISPR screening approach [citation].

# Implementation

GIMAP implements a multi-step analysis pipeline:

1. **Normalization**: Raw count data is transformed into log2 counts per million (CPM) and adjusted by subtracting pre-treatment values to obtain log2 fold changes. These are further normalized based on the distribution of negative and positive controls.

2. **Expected Score Calculation**: For double-targeting constructs, expected CRISPR scores are calculated as the sum of the corresponding single-targeting scores. For single-targeting constructs, the expected score combines the single-target effect with the mean effect of control constructs.

3. **Genetic Interaction Scoring**: Interaction scores represent the difference between observed and expected CRISPR scores, adjusted using a linear model to account for systematic biases:
   ```
   GI score = observed score - (intercept + slope × expected score)
   ```

4. **Statistical Analysis**: T-tests compare the distribution of double-targeting genetic interaction scores against the background distribution of single-targeting scores, with false discovery rate correction for multiple hypothesis testing.

The package provides comprehensive visualization tools including volcano plots to highlight significant genetic interactions and detailed result tables for further analysis.

# Use Cases

`gimap` has been successfully used to identify synthetic lethal interactions among paralog genes in cancer cell lines, revealing potential therapeutic targets where single-gene approaches have failed. The package accommodates various experimental designs, including time-course studies and treatment comparisons, offering flexibility for diverse research questions.

_Example applications include:_
- Identification of backup genes that provide functional redundancy in critical cellular pathways
- Discovery of context-dependent genetic interactions that emerge under specific conditions or treatments
- Systematic mapping of gene networks based on functional interactions rather than physical associations

# Conclusion

`gimap` provides a robust, accessible framework for analyzing paired guide CRISPR screening data and identifying genetic interactions with potential biological and therapeutic significance. By streamlining the computational workflow from raw counts to statistically rigorous interaction scores, GIMAP enables researchers to efficiently extract meaningful insights from complex genetic screening experiments. The package is available on GitHub (https://github.com/FredHutch/gimap) with comprehensive documentation and tutorials to facilitate adoption by the research community.

# Acknowledgements

This work is funded by NCI grant R01CA262556 and the Translational Data Science IRC of Fred Hutchinson Cancer Center.

# References
