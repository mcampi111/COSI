# COSI: Beyond the Audiogram

This repository contains the code for the manuscript "Beyond the Audiogram: Patient-Reported Listening Needs and Their Relation to Audiometry, Age, and Sex in a Nationwide Cohort of Hearing Aid Candidates" (Morvan, Campi, Peters, Thai-Van).

No data are included. The data are owned by Amplifon France and are not publicly available (see the Data Sharing Statement of the article). The scripts read the data files from local paths set at the top of each script, and they write aggregated results only.

## Repository structure

- `classifier/`: Jupyter notebooks for fine-tuning CamemBERT on expert-labelled COSI responses and for predicting the need category of all responses (eMethods 2). The models were trained and run on GPUs; the notebooks show the pipeline and are provided without outputs.
- `analysis/`: Python scripts for all statistical analyses, tables, and figures of the manuscript (below).

## Analysis scripts (`analysis/`)

| Script | Purpose | Manuscript items |
|---|---|---|
| `cosi_reanalysis.py` | Main analysis: nested multinomial logistic models (age and sex, audiometry, both, sex interactions), likelihood ratio tests, McFadden pseudo-R2, 5-fold cross-validation (log-loss, accuracy, macro and per-need AUC), predicted probabilities with simulation CIs, normalised entropy by degree of hearing loss, sensitivity analyses (excluding Other, any mention among the first 3 ranks, SRT) | Table 1; eTables 2 to 10 |
| `make_figures.py` | Figures from the aggregated outputs of `cosi_reanalysis.py` | Figures 1 to 3; eFigures 2 and 3 |
| `cosi_cmu_analysis.py` | Socioeconomic analysis: means-tested health coverage (Complémentaire santé solidaire) vs other patients; nested adjustments, g-computation, within-centre (Mundlak) models with SEs clustered by centre, exact standardisation, any mention | eTables 12 to 14 |
| `cosi_pta_ear_sensitivity.py` | Sensitivity analysis with the PTA of the left ear, the mean of both ears, and the better ear | eTable 9 |
| `cosi_data_checks.py` | Data checks: ear used for the PTA, classifier category counts | eMethods 1 and 2 |

## Running

```
cd analysis
pip install -r requirements.txt
python cosi_reanalysis.py
python make_figures.py
python cosi_cmu_analysis.py
python cosi_pta_ear_sensitivity.py
python cosi_data_checks.py
```

Edit the data paths at the top of each script before running. Outputs are written to `cosi_reanalysis_out/`, `cosi_cmu_out/`, `figs/`, and text summaries in the working directory.

## Models

The first-ranked COSI need (8 standard categories and Other) is modelled with multinomial logistic regression, Other as reference. Age and PTA enter as natural cubic splines with 4 degrees of freedom and the speech-in-noise SNR with 3 degrees of freedom. Models are fitted by maximum likelihood (BFGS, statsmodels); the covariance used for the simulation CIs is the inverse of the observed information at the estimate.

## Cite

If you use this code, please cite the article (citation to be updated on publication):

```
@article{morvan_beyond_audiogram,
  title  = {Beyond the Audiogram: Patient-Reported Listening Needs and Their Relation to Audiometry, Age, and Sex in a Nationwide Cohort of Hearing Aid Candidates},
  author = {Morvan, Perrine and Campi, Marta and Peters, Gareth W. and Thai-Van, Hung},
  year   = {2026}
}
```
