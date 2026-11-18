#!/usr/bin/env python3
"""
Two checks for the open () items of the JAMA manuscript. Prints aggregated counts only.

1. Which ear does the PTA column of df_COSI2_sym.csv refer to?
   Compares it with PTA_G (left), PTA_D (right), PTA_B, MAX_PTA, the better ear, the worse ear
   and the mean of both ears in COSI_pmorvan_HL_Tests.xlsx, matched by patient identifier.
2. Which COSI categories does the classifier output contain?
   Label counts in COSI_LABELS_Final.xlsx (classifier output) and COSI_LABELS_3000.xlsx (expert labels).

Usage:  python3 cosi_checks_.py
"""
import os
import numpy as np
import pandas as pd

BASE = os.path.expanduser("~/Desktop/Postdoc_Pasteur/Perrine")
ANALYSIS = f"{BASE}/code/excel_results/df_COSI2_sym.csv"
TESTS = f"{BASE}/data/Cosi_Labelled/COSI_pmorvan_HL_Tests.xlsx"
LABELS_FINAL = f"{BASE}/data/Cosi_Labelled/COSI_LABELS_Final.xlsx"
LABELS_3000 = f"{BASE}/data/Cosi_Labelled/all_petit_perrinelabels/COSI_LABELS_3000.xlsx"

# ---------------------------------------------------------------- 1. PTA ear
an = pd.read_csv(ANALYSIS, low_memory=False, usecols=["ID", "PTA"])
an["ID"] = an.ID.astype(str).str.replace(r"\.0$", "", regex=True).str.strip()
print("Reading the hearing test file (this takes a few minutes)...")
cols = ["CUSTOMER_CODE", "PTA_G", "PTA_D", "PTA_B", "MAX_PTA"]
t = pd.read_excel(TESTS, usecols=cols)
t["ID"] = t.CUSTOMER_CODE.astype(str).str.replace(r"\.0$", "", regex=True).str.strip()
for c in cols[1:]:
    t[c] = pd.to_numeric(t[c], errors="coerce")
t["better_ear"] = t[["PTA_G", "PTA_D"]].min(axis=1)
t["worse_ear"] = t[["PTA_G", "PTA_D"]].max(axis=1)
t["mean_ears"] = t[["PTA_G", "PTA_D"]].mean(axis=1)
m = an.merge(t, on="ID", how="inner")
print(f"\nPatients of the analysis file found in the test file: {m.ID.nunique()} of {an.ID.nunique()}")
print("Share of patients whose PTA equals the candidate in at least one test record (tolerance 0.01 dB):")
for c in ["PTA_G", "PTA_D", "PTA_B", "MAX_PTA", "better_ear", "worse_ear", "mean_ears"]:
    hit = (m[c] - m.PTA).abs() < 0.01
    share = hit.groupby(m.ID).any().mean() * 100
    print(f"  {c:12s} {share:6.1f}%")

# ---------------------------------------------------------------- 2. classifier categories
for name, path in [("Classifier output (COSI_LABELS_Final)", LABELS_FINAL),
                   ("Expert labels (COSI_LABELS_3000)", LABELS_3000)]:
    try:
        d = pd.read_excel(path)
    except Exception as e:
        print(f"\n{name}: could not read ({e})")
        continue
    col = next((c for c in d.columns if str(c).lower() in ("label", "labels", "categorie", "category")), None)
    if col is None:
        print(f"\n{name}: no label column found; columns: {list(d.columns)}")
        continue
    vc = d[col].astype(str).value_counts()
    print(f"\n{name}: {len(d)} rows, {vc.size} distinct labels")
    print(vc.to_string())
