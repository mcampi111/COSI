# COSI
This repository is linked to the methodology developed in the paper with title **"Hearing Loss Characterisation Using Natural Language Processing Modeling on large-scale Patient-Reported Outcomes Measures"**.  

The pdf for the paper is available at this url .... and provided in the folder "paper", where it is also possibile to find the Supplementary Materials.

## **Abstract**

**Background**: Hearing loss affects 1.5 billion people globally, including over 50% of French individuals aged 65 and older. Traditional assessment like tonal and speech gains fail to capture the broader impact on daily life. Patient-Reported Outcome Measures (PROMs), such as the COSI questionnaire, provide personalized insights to enhance patient care. This study integrates Natural-Language Processing (NLP) techniques to analyze COSI data, assessing PROMs' benefit in audiology and the specific needs of hearing-impaired patients in France.

**Methods**: We conducted a population-based, cross-sectional, comparative analysis with 190,213 adults with symmetric hearing loss in France, collecting data between September 2020 and December 2022. Expert-labelled data was used to train an advanced French NLP model known in the literature as CamemBERT. Advanced regression analysis examined the contributions of speech tests, age, sex, and COSI responses to pure-tone average (PTA) hearing loss categories. 

**Findings**:  Patients with slight hearing loss and those aged 40-49 prioritize understanding speech in noise, highlighting the need for early testing and speech-in-noise assessments. Females emphasize social and emotional aspects earlier than males.

**Interpretation**: Incorporating PROMs in audiology provided significant benefits in assessing patient needs, surpassing the limitations of audiograms and speech tests alone. The analysis of responses across the COSI enabled a nuanced understanding of individual hearing difficulties and rehabilitation goals. NLP integration into clinical systems can improve policies and interventions for hearing loss.

**Funding**: This research was funded by a grant from Fondation Pour l’Audition (FPA) to Hung Thai-Van and Paul Avan, number FPA IDA09 and FPA IDA10.


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

@article{..,
  title={..},
  author={..},
  journal={..},
  year={..}
}

