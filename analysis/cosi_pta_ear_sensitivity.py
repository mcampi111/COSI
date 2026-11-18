#!/usr/bin/env python3
"""
Sensitivity analysis for the JAMA manuscript: does the choice of ear for the PTA matter?

The analysis file uses the left-ear PTA. This script recovers, for each patient, the test record
used (left-ear PTA equal to the analysis PTA), computes the mean of both ears and the better ear,
and refits the main models with each PTA definition:
  - McFadden pseudo-R2 and likelihood ratio test of audiometry given age and sex
  - 5-fold cross-validated macro AUC (age + sex, audiometry, both)
  - predicted change in each first-ranked need from PTA 30 to 60 dB HL (age and SNR at median)
Prints aggregated results only (also saved to pta_ear_sensitivity.txt).

Usage:  python3 cosi_pta_ear_sensitivity.py
"""
import os
import warnings

import numpy as np
import pandas as pd
import statsmodels.formula.api as smf
from scipy import stats
from sklearn.metrics import roc_auc_score
from sklearn.model_selection import StratifiedKFold

warnings.filterwarnings("ignore")
BASE = os.path.expanduser("~/Desktop/Postdoc_Pasteur/Perrine")
ANALYSIS = f"{BASE}/code/excel_results/df_COSI2_sym.csv"
TESTS = f"{BASE}/data/Cosi_Labelled/COSI_pmorvan_HL_Tests.xlsx"
CAT_MAP = {
    "Conversation with 1 or 2 in silence": "Conv12_quiet", "Group conversation in silence": "Group_quiet",
    "Group conversation in noise": "Group_noise", "Television/radio at normal volume": "TV_radio",
    "Unfamiliar interlocutor on the phone": "Phone_unfamiliar", "Increase social contact": "Social_contact",
    "Feeling embarrassed or stupid": "Embarrassed", "Church or meeting": "Church_meeting",
    "Others": "Other", "Other": "Other",
}
out = []


def log(s=""):
    print(s)
    out.append(str(s))


an = pd.read_csv(ANALYSIS, low_memory=False)
d = pd.DataFrame({
    "id": an.ID.astype(str).str.replace(r"\.0$", "", regex=True).str.strip(),
    "age": pd.to_numeric(an.Age, errors="coerce"),
    "female": an.Sex.astype(str).str.strip().str.upper().str[0].isin(["F", "W"]).astype(int),
    "pta_left": pd.to_numeric(an.PTA, errors="coerce"),
    "snr": pd.to_numeric(an.SNR, errors="coerce"),
    "q1": an.Q1.astype(str).str.strip().map(CAT_MAP),
})
print("Reading the hearing test file (a few minutes)...")
t = pd.read_excel(TESTS, usecols=["CUSTOMER_CODE", "PTA_G", "PTA_D"])
t["id"] = t.CUSTOMER_CODE.astype(str).str.replace(r"\.0$", "", regex=True).str.strip()
t["PTA_G"] = pd.to_numeric(t.PTA_G, errors="coerce")
t["PTA_D"] = pd.to_numeric(t.PTA_D, errors="coerce")
m = d.merge(t[["id", "PTA_G", "PTA_D"]], on="id", how="left")
m = m[(m.PTA_G - m.pta_left).abs() < 0.01].dropna(subset=["PTA_D"])
m = m.drop_duplicates("id")            # first matching record per patient
d = m.reset_index(drop=True)
d["pta_mean"] = (d.PTA_G + d.PTA_D) / 2
d["pta_better"] = d[["PTA_G", "PTA_D"]].min(axis=1)
d = d.dropna(subset=["age", "snr", "q1"])
log(f"Patients with both ears recovered: {len(d)}")
log(f"Interaural PTA difference, mean (SD): {(d.PTA_G - d.PTA_D).abs().mean():.1f} "
    f"({(d.PTA_G - d.PTA_D).abs().std():.1f}) dB; correlation left vs mean: "
    f"{np.corrcoef(d.pta_left, d.pta_mean)[0, 1]:.3f}")

cats = ["Other"] + sorted(c for c in d.q1.unique() if c != "Other")
d["y"] = pd.Categorical(d.q1, categories=cats).codes
K = len(cats)


def fit(formula, data):
    return smf.mnlogit(formula, data).fit(method="bfgs", maxiter=3000, disp=False)


demo = "cr(age, df=4) + female"
skf = StratifiedKFold(5, shuffle=True, random_state=2026)
rows = []
for name in ["pta_left", "pta_mean", "pta_better"]:
    aud = f"cr({name}, df=4) + cr(snr, df=3)"
    r0, r1, r2 = fit(f"y ~ {demo}", d), fit(f"y ~ {aud}", d), fit(f"y ~ {demo} + {aud}", d)
    lr = 2 * (r2.llf - r0.llf)
    dfl = r2.params.size - r0.params.size
    aucs = {}
    for lab, f in [("demo", f"y ~ {demo}"), ("aud", f"y ~ {aud}"), ("both", f"y ~ {demo} + {aud}")]:
        P = np.zeros((len(d), K))
        for tr, te in skf.split(d, d.y):
            P[te] = np.asarray(fit(f, d.iloc[tr]).predict(d.iloc[te]))
        aucs[lab] = roc_auc_score(d.y, P, multi_class="ovr", average="macro")
    g = pd.DataFrame({"age": d.age.median(), "female": [0, 0, 1, 1], "snr": d.snr.median(),
                      name: [30, 60, 30, 60]})
    p = np.asarray(r2.predict(g)) * 100
    diff = ((p[1] - p[0]) + (p[3] - p[2])) / 2
    row = {"PTA": name, "R2_aud": 1 - r1.llf / r1.llnull, "R2_both": 1 - r2.llf / r2.llnull,
           "LRT_aud_chi2": lr, "df": dfl, "AUC_demo": aucs["demo"], "AUC_aud": aucs["aud"],
           "AUC_both": aucs["both"]}
    row.update({f"d30to60_{c}": diff[k] for k, c in enumerate(cats)})
    rows.append(row)
res = pd.DataFrame(rows).set_index("PTA").T
log("\nMain results with each PTA definition (d30to60 = change in percentage points, sex-averaged):")
log(res.round(3).to_string())
with open("pta_ear_sensitivity.txt", "w") as f:
    f.write("\n".join(out))
print("\nSaved pta_ear_sensitivity.txt")
