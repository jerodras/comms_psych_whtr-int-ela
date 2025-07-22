# Body Composition and Internalizing Problems in Adolescence  
**Moderation by the Early Life Environment**

This repository contains code and analysis supporting the manuscript:  
**"Body Composition and Internalizing Problems in Adolescence: Moderation by the Early Life Environment"**  
by Claudia Buss, Alice M. Graham, Lauren E. Gyllenhammer, Pathik D. Wadhwa, and Jerod M. Rasmussen.

## Overview

Adolescence is a critical period when risk for metabolic and mental health problems increases. In this study, we used data from 10,446 youth in the Adolescent Brain Cognitive Development (ABCD) Study (31,418 total observations) to:

1. Identify whether **waist-to-height ratio (WHtR)** or **body mass index (BMI)** better predicts **internalizing symptoms**.
2. Test whether **early-life adversity** (ACEs) strengthens the within-person association (coupling) between WHtR and internalizing symptoms.
3. Evaluate whether **protective environments** (family, community, peers, school) buffer this coupling.

## Key Findings

- **WHtR** is a stronger and more interpretable predictor of internalizing symptoms than BMI.
- **ACEs** significantly amplify the within-person coupling between WHtR and internalizing symptoms (r² = 4.6%).
- **Protective environments**, particularly in the **family** and **community** domains, reduce the impact of ACEs on this coupling.

## Figures

| Figure | Description |
|--------|-------------|
| **Figure 1** | Data inclusion flowchart |
| **Figure 2** | WHtR outperforms BMI; results from sibling discordance and multiverse analyses |
| **Figure 3** | ACEs are associated with stronger WHtR–internalizing coupling |
| **Figure 4** | Protective environments moderate the ACE–WHtR–internalizing relationship |

## Repository Contents

- `code/` – Scripts to reproduce the core analyses (GAMM models, random slope estimation, moderation tests)
- `data/` – Derived data files (e.g., individual-level coupling estimates), if permissible under ABCD data use agreements
- `figures/` – Publication-ready figures
- `MetPsych_v1_1_COMMSPSYCH_wfigs.pdf` – Full manuscript with embedded figures

## Citation

If you use this repository or build upon its analyses, please cite:

> Rasmussen JM et al. (2025). *Body Composition and Internalizing Problems in Adolescence: Moderation by the Early Life Environment*. Communications Psychology.

## Data Access

The ABCD Study data used in this work are publicly available from the [NIMH Data Archive (NDA)](https://nda.nih.gov/abcd) with appropriate data use agreements. Derived variables such as random slope estimates will be shared via the [NIMH Brain Development Cohorts (NBDC) platform](https://nda.nih.gov/edit_collection.html?id=3147) upon publication.

---

For questions, contact: [rasmussj@hs.uci.edu](mailto:rasmussj@hs.uci.edu)
