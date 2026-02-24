
# Algorithms, Accidental Encounters and Intentional Exposure  
## Pathways to Online Self-Harm Content in 30,000 Adolescents  
(OxWell Student Survey 2025)

## Introduction

This GitHub repository contains the code necessary to replicate findings from the following paper:

**Bear et al. (under review).**  
*Algorithms, Accidental Encounters and Intentional Exposure: Pathways to Online Self-Harm Content in 30,000 Adolescents.*

---

## The OxWell Student Survey

The OxWell Student Survey is a repeated cross-sectional survey of primary and secondary school students in England. The survey asks students to self-report on a wide range of factors related to mental health, wellbeing, and online experiences. The study protocol is available at BMJ Open.

---

## Datasets

This analysis used data from the 2025 wave of the OxWell Student Survey (version R13).

This repository does not contain the dataset used for this analysis, as access is restricted due to data protection requirements. Researchers may apply to use the dataset by contacting the OxWell Student Survey study team at oxwell@psych.ox.ac.uk.

---

## Code

The repository contains R scripts used for:

- Data preparation and variable construction  
- Descriptive analyses (overall, by exposure frequency, and by pathway)  
- Multinomial regression modelling of exposure pathway  
- Predicted probability estimation  
- Figure generation  

The main analysis script is:

- `Algorithms (final).R`

The script requires a local configuration file (`local.R`) specifying secure data paths. This file is not included in the repository.

---

## License

Creative Commons License  
This work is licensed under a Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
