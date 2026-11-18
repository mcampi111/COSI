#!/usr/bin/env python3
"""
CMU vs control: do first-ranked COSI needs differ once age, sex and audiometry are accounted for?

Run LOCALLY. Reads the patient-level extraction (CMU/AME/RESEAU flags) and the analysis
file of the main paper (df_COSI2_sym.csv: classified needs, PTA, SNR), links them by
patient identifier, and writes ONLY aggregated outputs to OUT_DIR. Paste summary_cmu.txt
into the chat.

Group rules (Perrine, 17 Nov 2026):
  CMU flag true (with or without AME, with or without RESEAU)  -> CMU group
  AME true and CMU false                                      -> removed
  neither CMU nor AME (RESEAU true or false)                   -> control group

Analyses (within the analysis sample of the main paper: adults, first-time candidates,
symmetrical hearing, PTA and speech in noise available):
  1. Descriptives by group (age, sex, PTA, SNR, degree of HL, first-ranked need)
  2. Unadjusted comparison of the need distribution (chi-square, Cramer V)
  3. Multinomial model of the first-ranked need: LRT for CMU in three nested adjustments
     (none; age + sex; age + sex + PTA + SNR)
  4. Standardised (g-computation) difference in the probability of each need, CMU vs control,
     for each adjustment set, with 95% simulation intervals
  5. Sensitivity: controls reweighted to the CMU distribution of 5-year age band x sex x HL degree
     (exact standardisation), bootstrap 95% CI
  6. Per-need logistic models (fully adjusted), odds ratio for CMU, Benjamini-Hochberg

Usage:  python3 cosi_cmu_analysis.py
"""

import csv
import io
import os
import sys
import warnings

import numpy as np
import pandas as pd
import patsy
import statsmodels.formula.api as smf
from scipy import stats
from statsmodels.stats.multitest import multipletests

warnings.filterwarnings("ignore")

# =============================================================================
# CONFIG
# =============================================================================
DETAILS_PATH = os.path.expanduser("~/Downloads/COSI_Social_Preliminary/COSI_20221220_DETAILS.xlsx")
DETAILS_SHEET = 0
ANALYSIS_PATH = os.path.expanduser(
    "~/Desktop/Postdoc_Pasteur/Perrine/code/excel_results/df_COSI2_sym.csv")
OUT_DIR = "cosi_cmu_out"

# Column names in the DETAILS extraction (None = auto-detect from the candidates below)
DET_ID, DET_CMU, DET_AME, DET_RESEAU = None, None, None, None
CANDIDATES = {
    "id": ["CUSTOMER_CODE", "CUSTOMER", "ID"],
    "cmu": ["CMU_FL", "CMU", "FLAG_CMU"],
    "ame": ["AME_FL", "AME", "FLAG_AME"],
    "reseau": ["RESEAU", "RESEAU_FL", "FLAG_RESEAU"],
}
# Columns in the analysis file
AN = {"id": "ID", "age": "Age", "sex": "Sex", "pta": "PTA", "snr": "SNR", "hl": "HL", "q1": "Q1",
      "q2": "Q2", "q3": "Q3", "shop": "Shop"}

TRUE_VALUES = {"TRUE", "VRAI", "1", "1.0", "O", "OUI", "Y", "YES", "T"}
SEX_MAP = {"F": "F", "FEMALE": "F", "FEMME": "F", "W": "F", "2": "F",
           "M": "M", "MALE": "M", "HOMME": "M", "H": "M", "1": "M"}
CAT_MAP = {
    "Conversation with 1 or 2 in silence": "Conv12_quiet",
    "Group conversation in silence": "Group_quiet",
    "Group conversation in noise": "Group_noise",
    "Television/radio at normal volume": "TV_radio",
    "Unfamiliar interlocutor on the phone": "Phone_unfamiliar",
    "Increase social contact": "Social_contact",
    "Feeling embarrassed or stupid": "Embarrassed",
    "Church or meeting": "Church_meeting",
    "Others": "Other", "Other": "Other",
}
REF = "Other"
N_SIM, N_BOOT, SEED = 1000, 500, 2026
# =============================================================================

rng = np.random.default_rng(SEED)
os.makedirs(OUT_DIR, exist_ok=True)
lines = []


def log(s=""):
    print(s)
    lines.append(str(s))


def finish():
    with open(f"{OUT_DIR}/summary_cmu.txt", "w") as f:
        f.write("\n".join(lines))
    print(f"\nAggregated outputs in {OUT_DIR}/ ; paste summary_cmu.txt into the chat.")


def pick(df, given, key):
    if given:
        if given not in df.columns:
            sys.exit(f"Column {given!r} not in DETAILS file. Columns: {list(df.columns)}")
        return given
    for c in CANDIDATES[key]:
        if c in df.columns:
            return c
    sys.exit(f"No column found for {key} (tried {CANDIDATES[key]}). Columns: {list(df.columns)}")


def is_true(s):
    return s.astype("string").str.strip().str.upper().isin(TRUE_VALUES)


def norm_id(s):
    s = s.astype("string").str.strip()
    return s.str.replace(r"\.0$", "", regex=True)


# -----------------------------------------------------------------------------
# 1. Groups in the full extraction
# -----------------------------------------------------------------------------
det = (pd.read_csv(DETAILS_PATH, low_memory=False) if DETAILS_PATH.endswith(".csv")
       else pd.read_excel(DETAILS_PATH, sheet_name=DETAILS_SHEET, header=None, dtype=str))
if not DETAILS_PATH.endswith(".csv"):
    first = str(det.iloc[0, 0])
    if "," in first and "CMU_FL" in first:
        # CSV text pasted into column A, possibly with quoted line breaks that Excel split
        # over several rows. Rebuild the text and parse it with a real CSV reader.
        header = next(csv.reader([first]))
        nf = len(header)
        i_open = header.index("OPEN_ANSWER")
        n_right = nf - i_open - 1
        rows_txt = [",".join(str(v) for v in r if isinstance(v, str))
                    for r in det.iloc[1:].itertuples(index=False)]
        recs, n_fixed, n_bad = [], 0, 0
        for rec in csv.reader(io.StringIO("\n".join(rows_txt))):
            if len(rec) == nf:
                recs.append(rec)
            elif len(rec) > nf:              # unquoted commas inside the free-text answer
                recs.append(rec[:i_open] + [",".join(rec[i_open:len(rec) - n_right])] + rec[-n_right:])
                n_fixed += 1
            else:
                n_bad += 1
        det = pd.DataFrame(recs, columns=header)
        ok = det["CMU_FL"].str.strip().isin(["0", "1"]) & det["AME_FL"].str.strip().isin(["0", "1"])
        log(f"Parsed {len(det)} records (repaired {n_fixed} with commas in the answer, "
            f"{n_bad} fragments dropped); records with invalid CMU/AME flags dropped: {(~ok).sum()}")
        det = det[ok]
    else:
        det.columns = det.iloc[0]
        det = det.iloc[1:]
c_id, c_cmu = pick(det, DET_ID, "id"), pick(det, DET_CMU, "cmu")
c_ame, c_res = pick(det, DET_AME, "ame"), pick(det, DET_RESEAU, "reseau")
log(f"DETAILS: {len(det)} rows; columns used: id={c_id}, cmu={c_cmu}, ame={c_ame}, reseau={c_res}")
for c in [c_cmu, c_ame, c_res]:
    log(f"  raw values of {c}: {det[c].astype('string').value_counts(dropna=False).head(8).to_dict()}")

g = pd.DataFrame({"id": norm_id(det[c_id]), "cmu": is_true(det[c_cmu]),
                  "ame": is_true(det[c_ame]), "reseau": is_true(det[c_res])})
pat = g.groupby("id").agg(cmu=("cmu", "max"), ame=("ame", "max"), reseau=("reseau", "max"))
inconsistent = g.groupby("id").cmu.nunique().gt(1).sum()
log(f"Patients in extraction: {len(pat)}; patients with CMU flag differing across rows: {inconsistent}")
pat["group"] = np.where(pat.cmu, "CMU", np.where(pat.ame, "removed", "Control"))
log("\nFlag combinations (patients):")
log(pat.groupby(["cmu", "ame", "reseau"]).size().rename("n").to_string())
log("\nGroups in the full extraction:\n" + pat.group.value_counts().to_string())

# -----------------------------------------------------------------------------
# 2. Link to the analysis sample
# -----------------------------------------------------------------------------
an = pd.read_csv(ANALYSIS_PATH, low_memory=False)
d = pd.DataFrame({
    "id": norm_id(an[AN["id"]]),
    "age": pd.to_numeric(an[AN["age"]], errors="coerce"),
    "sex": an[AN["sex"]].astype("string").str.strip().str.upper().map(SEX_MAP),
    "pta": pd.to_numeric(an[AN["pta"]], errors="coerce"),
    "snr": pd.to_numeric(an[AN["snr"]], errors="coerce"),
    "hl": an[AN["hl"]].astype("string").str.strip(),
    "q1": an[AN["q1"]].astype("string").str.strip().map(CAT_MAP),
    "q2": an[AN["q2"]].astype("string").str.strip().map(CAT_MAP),
    "q3": an[AN["q3"]].astype("string").str.strip().map(CAT_MAP),
    "shop": an[AN["shop"]].astype("string"),
})
if d.sex.isna().any() or d.q1.isna().any():
    sys.exit(f"Unmapped values: sex {an.loc[d.sex.isna(), AN['sex']].unique()[:5]}, "
             f"Q1 {an.loc[d.q1.isna(), AN['q1']].unique()[:5]}")
d = d.merge(pat[["group"]], left_on="id", right_index=True, how="left")
log(f"\nAnalysis sample: {len(d)}; linked to extraction: {d.group.notna().sum()} "
    f"({d.group.notna().mean() * 100:.1f}%)")
if d.group.notna().mean() < 0.5:
    log("WARNING: fewer than half of the analysis sample linked. Check that the identifiers match "
        f"(examples of format only, analysis vs extraction lengths: "
        f"{d.id.str.len().value_counts().head(3).to_dict()} vs "
        f"{pat.index.str.len().value_counts().head(3).to_dict()}).")
log("Groups in the linked analysis sample:\n" + d.group.value_counts(dropna=False).to_string())
d = d[d.group.isin(["CMU", "Control"])].reset_index(drop=True)
d["cmu"] = (d.group == "CMU").astype(int)
d["female"] = (d.sex == "F").astype(int)
if d.cmu.sum() < 100:
    log("Too few CMU patients in the linked sample for modelling.")
    finish()
    sys.exit()

cats = [REF] + sorted(c for c in d.q1.unique() if c != REF)
d["y"] = pd.Categorical(d.q1, categories=cats).codes
K = len(cats)

# -----------------------------------------------------------------------------
# 3. Descriptives
# -----------------------------------------------------------------------------
log("\n== Descriptives by group ==")
desc = d.groupby("group").agg(n=("id", "size"), age_mean=("age", "mean"), age_sd=("age", "std"),
                              female_pct=("female", lambda x: x.mean() * 100),
                              pta_mean=("pta", "mean"), pta_sd=("pta", "std"),
                              snr_mean=("snr", "mean"), snr_sd=("snr", "std")).T
log(desc.round(2).to_string())
age_bins = pd.cut(d.age, [17, 45, 60, 75, 99], labels=["18-45", "46-60", "61-75", "76-99"])
log("\nAge band (% within group):\n" +
    (pd.crosstab(age_bins, d.group, normalize="columns") * 100).round(1).to_string())
log("\nDegree of HL (% within group):\n" +
    (pd.crosstab(d.hl, d.group, normalize="columns") * 100).round(1).to_string())
prof = pd.crosstab(d.q1, d.group)
prof_pct = (pd.crosstab(d.q1, d.group, normalize="columns") * 100).round(1)
log("\nFirst-ranked need, counts:\n" + prof.to_string())
log("\nFirst-ranked need, % within group:\n" + prof_pct.to_string())
chi2, p, df_, _ = stats.chi2_contingency(prof)
log(f"\nUnadjusted: chi2 = {chi2:.1f}, df = {df_}, p = {p:.2e}, "
    f"Cramer V = {np.sqrt(chi2 / prof.values.sum() / (min(prof.shape) - 1)):.3f}")
desc.to_csv(f"{OUT_DIR}/descriptives_by_group.csv")
prof.to_csv(f"{OUT_DIR}/q1_counts_by_group.csv")

# -----------------------------------------------------------------------------
# 4. Multinomial models: LRT for CMU and standardised differences
# -----------------------------------------------------------------------------
ADJ = {
    "Unadjusted": "1",
    "Age and sex": "cr(age, df=4) + female",
    "Age, sex, PTA, SNR": "cr(age, df=4) + female + cr(pta, df=4) + cr(snr, df=3)",
}


def fit(formula, data):
    return smf.mnlogit(formula, data).fit(method="bfgs", maxiter=3000, disp=False)


def design_info(res):
    dat = res.model.data
    return getattr(dat, "design_info", None) or getattr(dat, "model_spec")


def probs(X, B):
    eta = np.column_stack([np.zeros(len(X)), X @ B])
    eta -= eta.max(1, keepdims=True)
    e = np.exp(eta)
    return e / e.sum(1, keepdims=True)


log("\n== Multinomial models: does CMU status add information? ==")
std_rows, lrt_rows = [], []
for name, rhs in ADJ.items():
    r0 = fit(f"y ~ {rhs}", d)
    r1 = fit(f"y ~ {rhs} + cmu", d)
    st = 2 * (r1.llf - r0.llf)
    dfl = r1.params.size - r0.params.size
    lrt_rows.append({"adjustment": name, "chi2": st, "df": dfl, "p": stats.chi2.sf(st, dfl),
                     "converged": r1.mle_retvals.get("converged")})
    # g-computation: everyone as CMU vs everyone as control
    B = r1.params.values
    H = r1.model.hessian(B.flatten(order="F"))
    cov = np.linalg.pinv(-H)
    cov = (cov + cov.T) / 2
    draws = rng.multivariate_normal(B.flatten(order="F"), cov, size=N_SIM)
    draws = draws.reshape(N_SIM, B.shape[1], B.shape[0]).transpose(0, 2, 1)
    di = design_info(r1)
    X1 = np.asarray(patsy.build_design_matrices([di], d.assign(cmu=1))[0])
    X0 = np.asarray(patsy.build_design_matrices([di], d.assign(cmu=0))[0])
    pt1, pt0 = probs(X1, B).mean(0), probs(X0, B).mean(0)
    sims = np.stack([probs(X1, b).mean(0) - probs(X0, b).mean(0) for b in draws]) * 100
    for k, c in enumerate(cats):
        std_rows.append({"adjustment": name, "need": c, "p_cmu": pt1[k] * 100, "p_control": pt0[k] * 100,
                         "diff_pp": (pt1[k] - pt0[k]) * 100,
                         "lo": np.percentile(sims[:, k], 2.5), "hi": np.percentile(sims[:, k], 97.5)})
lrt = pd.DataFrame(lrt_rows)
std = pd.DataFrame(std_rows)
log(lrt.to_string(index=False))
log("\nStandardised probability of each first-ranked need, CMU vs control "
    "(percentage points, 95% simulation interval):")
log(std.round(2).to_string(index=False))
lrt.to_csv(f"{OUT_DIR}/lrt_cmu.csv", index=False)
std.to_csv(f"{OUT_DIR}/standardised_differences.csv", index=False)

# -----------------------------------------------------------------------------
# 5. Sensitivity: controls reweighted to the CMU strata (5-year age x sex x HL degree)
# -----------------------------------------------------------------------------
log("\n== Sensitivity: exact standardisation on 5-year age band x sex x HL degree ==")
d["stratum"] = (pd.cut(d.age, np.arange(15, 105, 5)).astype(str) + "|" + d.sex + "|" + d.hl)


def standardised(dd):
    cm, co = dd[dd.cmu == 1], dd[dd.cmu == 0]
    w_target = cm.stratum.value_counts(normalize=True)
    co = co[co.stratum.isin(w_target.index)]
    n_s = co.stratum.value_counts()
    w = co.stratum.map(w_target / n_s)
    p_co = (pd.get_dummies(co.q1).reindex(columns=cats, fill_value=0).mul(w, axis=0).sum()
            / w.sum() * 100)
    p_cm = cm.q1.value_counts(normalize=True).reindex(cats, fill_value=0) * 100
    covered = cm.stratum.isin(n_s.index).mean()
    return p_cm, p_co, covered


p_cm, p_co, covered = standardised(d)
boot = []
idx_cm, idx_co = np.where(d.cmu == 1)[0], np.where(d.cmu == 0)[0]
for _ in range(N_BOOT):
    bb = d.iloc[np.concatenate([rng.choice(idx_cm, len(idx_cm)), rng.choice(idx_co, len(idx_co))])]
    a, b, _ = standardised(bb)
    boot.append((a - b).values)
boot = np.array(boot)
sens = pd.DataFrame({"p_cmu": p_cm, "p_control_standardised": p_co, "diff_pp": p_cm - p_co,
                     "lo": np.percentile(boot, 2.5, axis=0), "hi": np.percentile(boot, 97.5, axis=0)})
log(f"CMU patients in strata with at least one control: {covered * 100:.1f}%")
log(sens.round(2).to_string())
sens.to_csv(f"{OUT_DIR}/sensitivity_standardised.csv")

# -----------------------------------------------------------------------------
# 6. Per-need logistic models, fully adjusted
# -----------------------------------------------------------------------------
log("\n== Per-need logistic models (age, sex, PTA, SNR), OR for CMU ==")
rows = []
for c in cats:
    dd = d.assign(yk=(d.q1 == c).astype(int))
    m = smf.logit(f"yk ~ {ADJ['Age, sex, PTA, SNR']} + cmu", dd).fit(disp=False)
    b, se = m.params["cmu"], m.bse["cmu"]
    rows.append({"need": c, "OR": np.exp(b), "lo": np.exp(b - 1.96 * se), "hi": np.exp(b + 1.96 * se),
                 "p": m.pvalues["cmu"]})
orr = pd.DataFrame(rows).set_index("need")
orr["p_FDR"] = multipletests(orr.p, method="fdr_bh")[1]
log(orr.round(4).to_string())
orr.to_csv(f"{OUT_DIR}/or_cmu_per_need.csv")


# -----------------------------------------------------------------------------
# 7. Centre: concentration of CMU patients and within-centre comparison
# -----------------------------------------------------------------------------

log("\n== Centres ==")
cs = d.groupby("shop").agg(n=("cmu", "size"), n_cmu=("cmu", "sum"))
cs["pct_cmu"] = cs.n_cmu / cs.n * 100
log(f"Centres: {len(cs)}; centres with at least 1 CMU patient: {(cs.n_cmu > 0).sum()}; "
    f"share of CMU patients seen in the 10% of centres with most CMU patients: "
    f"{cs.n_cmu.sort_values(ascending=False).head(max(1, len(cs) // 10)).sum() / cs.n_cmu.sum() * 100:.1f}%")
log("Percentage of CMU patients per centre, quantiles (10/25/50/75/90): " +
    ", ".join(f"{q:.1f}" for q in np.percentile(cs.pct_cmu, [10, 25, 50, 75, 90])))
# centre-level Q1 heterogeneity: how much do centres differ in their need mix?
ct = pd.crosstab(d.shop, d.q1)
ct = ct[ct.sum(1) >= 30]
chi2c, pc, dfc, _ = stats.chi2_contingency(ct)
log(f"Need mix differs between centres (centres with >=30 patients, n = {len(ct)}): "
    f"chi2 = {chi2c:.0f}, df = {dfc}, p = {pc:.1e}, Cramer V = "
    f"{np.sqrt(chi2c / ct.values.sum() / (min(ct.shape) - 1)):.3f}")

log("\n== Within-centre comparison (Mundlak: centre means added, SE clustered by centre), OR for CMU ==")
dc = d[d.shop.notna()].reset_index(drop=True)
for v in ["cmu", "age", "female", "pta", "snr"]:
    dc[f"{v}_cbar"] = dc.groupby("shop")[v].transform("mean")
RHS_W = ADJ["Age, sex, PTA, SNR"] + " + cmu + cmu_cbar + age_cbar + female_cbar + pta_cbar + snr_cbar"
groups = pd.factorize(dc.shop)[0]
rows = []
for c in cats:
    dd = dc.assign(yk=(dc.q1 == c).astype(int))
    m = smf.logit(f"yk ~ {RHS_W}", dd).fit(disp=False, cov_type="cluster", cov_kwds={"groups": groups})
    b, se = m.params["cmu"], m.bse["cmu"]
    rows.append({"need": c, "OR_within_centre": np.exp(b), "lo": np.exp(b - 1.96 * se),
                 "hi": np.exp(b + 1.96 * se), "p": m.pvalues["cmu"],
                 "OR_centre_share_per_10pct": np.exp(m.params["cmu_cbar"] * 0.10)})
orc = pd.DataFrame(rows).set_index("need")
orc["p_FDR"] = multipletests(orc.p, method="fdr_bh")[1]
orc = orc.join(orr[["OR"]].rename(columns={"OR": "OR_overall"}))
log(orc.round(4).to_string())
log("OR_within_centre compares CMU and control patients of the same centre; "
    "OR_centre_share_per_10pct is the association with the centre's share of CMU patients.")
orc.to_csv(f"{OUT_DIR}/or_cmu_within_centre.csv")

# -----------------------------------------------------------------------------
# 8. Any mention among Q1 to Q3, fully adjusted, centre-clustered SE
# -----------------------------------------------------------------------------
log("\n== Any mention among Q1 to Q3 (age, sex, PTA, SNR), OR for CMU, SE clustered by centre ==")
rows = []
for c in cats:
    if c == REF:
        continue
    dd = dc.assign(yk=((dc.q1 == c) | (dc.q2 == c) | (dc.q3 == c)).astype(int))
    m = smf.logit(f"yk ~ {ADJ['Age, sex, PTA, SNR']} + cmu", dd).fit(
        disp=False, cov_type="cluster", cov_kwds={"groups": pd.factorize(dd.shop)[0]})
    b, se = m.params["cmu"], m.bse["cmu"]
    rows.append({"need": c, "pct_cmu": dd.loc[dd.cmu == 1, "yk"].mean() * 100,
                 "pct_control": dd.loc[dd.cmu == 0, "yk"].mean() * 100,
                 "OR": np.exp(b), "lo": np.exp(b - 1.96 * se), "hi": np.exp(b + 1.96 * se),
                 "p": m.pvalues["cmu"]})
anym = pd.DataFrame(rows).set_index("need")
anym["p_FDR"] = multipletests(anym.p, method="fdr_bh")[1]
log(anym.round(4).to_string())
anym.to_csv(f"{OUT_DIR}/or_cmu_any_mention_q1q3.csv")

finish()
