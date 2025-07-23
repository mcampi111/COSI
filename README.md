# COSI
This repository is linked to the methodology developed in the paper with title **"Beyond the Audiogram: Using PROMs and AI Modelling to Characterise Age-and Sex-Specific Hearing Loss Needs in a Nationwide Study"**.  

The pdf for the paper is available at this url .... and provided in the folder "paper", where it is also possibile to find the Supplementary Materials.

## **Abstract**

**Background**: Hearing loss (HL) affects 1.5 billion people globally, with significant economic and psychosocial costs. Traditional audiometric assessments often overlook psychosocial impacts, limiting intervention effectiveness. This study uses Patient-Reported Outcome Measures (PROMs), specifically the Client Oriented Scale of Improvement (COSI), and AI-based Natural Language Processing (NLP) to identify age-and sex-specific patterns in hearing-related needs and inform personalised care strategies.

**Methods**: We conducted a cross-sectional analysis of 273,861 COSI responses from a national database. Participants aged 18-99 years with symmetrical HL and first-time hearing aid use were included. NLP modelling classified responses into 16 standardized listening categories. Statistical analyses (Chi-Squared, ANOVA, regressions) examined associations between hearing needs, age, sex, and audiological outcomes, with clustering analyses exploring broader patterns.

**Findings**: Among 91,297 participants, PROMs revealed key needs-understanding group conversations in noise, media listening, and reducing communication-related embarrassment-regardless of HL severity. Several PROMs significantly predicted pure-tone average, highlighting their clinical value. Signal-to-noise ratio emerged as an early marker of HL in younger adults. Sex-specific patterns showed women prioritized social and emotional needs, while men emphasized structured settings such as meetings and television. These differences varied by age and HL degree, underscoring the need for individualised care.

**Interpretation**: Integrating PROMs into audiological practice enhances decision-making by uncovering hidden needs, enabling more targeted and comprehensive interventions. PROMs provide essential context for understanding the lived experience of HL and support a shift toward personalised hearing care.

**Funding**: This research was funded by a grant from Fondation Pour l’Audition (FPA) to Hung Thai-Van and Paul Avan, number FPA IDA09 and FPA IDA10 and Amplifon France.


## Contributions of the paper
The paper has the following contributions:
1. Assess the benefits of PROMs in audiology for evaluating patient experiences and outcomes, and the impact of hearing loss on individuals.
2. Utilize NLP techniques to analyze the specific needs and experiences of a subset of hearing-impaired patients in France, providing insights for tailored interventions and improved patient care.


## Motivations For this Study

Integrating Natural Language Processing (NLP) techniques into PROM analysis is increasingly recommended. NLP has been effectively applied across various health domains, demonstrating its potential to enhance the interpretation and utilization of patient feedback in clinical settings.

The importance of PROMs in capturing patient experiences and improving patient-professional communication is increasingly recognized. These tools improve care quality and aligning treatments with individual needs. In audiology, the COSI questionnaire exemplifies the value of personalized approaches to hearing difficulties. Additionally, integrating NLP techniques enables the analysis of PROMs data, offering deeper insights into patient needs and experiences. Building on these insights, this study aims to characterize hearing loss in France using PROMs


## Organization of the Repository
The repository is organized in the following folders:

```diff
+ 1) code
```
The code folder contains all the code used for the implementation of the methods and the results section, inlcuding tables and figures. We used both R and Python and the details of each folder structure is given below.

1.  **python**. This folder contains the ptyhon code used for the NLP fine tune and the prediction of the COSI need labels. Note that this are Jupiter Notebook even though the code run in parallel on GPUs hence this are only showcasing the use of the employed models for the NLP task.
2. **R**. This folder contains R code used to implement the plots, the statistics, the statistical tests and the statistical models.


```diff
+ 3) paper 
```

The paper folder contains the following pdf:

1.  **Main Paper**. This pdf represents the main body of the paper.
2.  **Supplementary Information**. This pdf provides all the Supplementary Information.



## Cite

If you use this code in your project, please cite:

@article{morvan2025beyond,
  title={Beyond the Audiogram: Using PROMs and AI Modelling to Characterise Age-and Sex-Specific Hearing Loss Needs in a Nationwide Study},
  author={Morvan, Perrine and Campi, Marta and Peters, Gareth and Thai-Van, Hung},
  journal={Available at SSRN 5282029},
  year={2025}
}
