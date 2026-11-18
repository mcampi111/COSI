#!/usr/bin/env python3
"""
COSI x audiometry reanalysis (Amplifon, Morvan / Campi / Peters / Thai-Van).

Run LOCALLY. The script reads the patient-level Excel file and writes ONLY
aggregated outputs (tables, figures, summary.txt) to OUT_DIR.
Paste summary.txt (or the CSVs) into the chat, never the raw data.

Questions addressed
  C1  How much does audiometry (PTA, SNR; SRT in sensitivity) explain the first-ranked need (Q1),
      beyond age and sex?  -> nested multinomial models, LRT, 5-fold CV
  C2  Does need heterogeneity change with HL severity?  -> normalised entropy
      by ASHA band with bootstrap CI, plus the need profile by band
  C3  Do needs vary with age and sex?  -> LRT for sex and age x sex,
      predicted probabilities by PTA and age, per sex, with simulation CIs
  Sensitivity: excluding "Other"; any mention in Q1-Q3 (one logistic per need, BH FDR)

Requirements: pandas numpy scipy statsmodels scikit-learn matplotlib openpyxl patsy
Usage:  python3 cosi_reanalysis.py
"""

import os
import sys
import warnings

import numpy as np
import pandas as pd
import patsy
import statsmodels.api as sm
import statsmodels.formula.api as smf
from scipy import stats
from sklearn.metrics import log_loss, roc_auc_score
from sklearn.model_selection import StratifiedKFold

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

warnings.filterwarnings("ignore")

# =============================================================================
# CONFIG: edit this block only
# =============================================================================
DATA_PATH = os.path.expanduser(
    "~/Desktop/Postdoc_Pasteur/Perrine/code/excel_results/df_COSI2_sym.csv")
SHEET = 0                               # only used for .xlsx files
OUT_DIR = "cosi_reanalysis_out"

# Column names in the Excel file
COL = {
    "age": "Age",
    "sex": "Sex",          # any coding; map below
    "pta": "PTA",          # dB HL, the PTA used for the ASHA classification
    "srt": "SRT",          # dB (46% missing in df_COSI2_sym.csv: used in sensitivity only)
    "snr": "SNR",          # dB
    "hl": "HL",            # ASHA degree already in the file; set to None to recompute from PTA
    "q1": "Q1",            # classified COSI category, first-ranked need
    "q2": "Q2",            # set to None if absent
    "q3": "Q3",            # set to None if absent
}

# Map raw sex values to "F"/"M"
SEX_MAP = {"F": "F", "Female": "F", "female": "F", "Femme": "F", "FEMME": "F", "W": "F", "2": "F",
           "M": "M", "Male": "M", "male": "M", "Homme": "M", "HOMME": "M", "H": "M", "1": "M"}
HL_MAP = {"Normal": "Normal", "Slight": "Slight", "Mild": "Mild", "Moderate": "Moderate",
          "Moderately severe": "Mod_severe", "Severe": "Severe", "Profound": "Profound"}

# Map raw category labels to short labels. Unmapped labels are listed in
# summary.txt so the map can be completed. Missing / "NA" -> NaN.
CAT_MAP = {
    "Conversation with one or two in quiet": "Conv12_quiet",
    "Conversation with 1 or 2 in silence": "Conv12_quiet",
    "Conversation with group in quiet": "Group_quiet",
    "Group conversation in silence": "Group_quiet",
    "Conversation with group in noise": "Group_noise",
    "Group conversation in noise": "Group_noise",
    "Television/radio at normal volume": "TV_radio",
    "Unfamiliar speaker on phone": "Phone_unfamiliar",
    "Unfamiliar interlocutor on the phone": "Phone_unfamiliar",
    "Increased social contact": "Social_contact",
    "Increase social contact": "Social_contact",
    "Feel embarrassed or stupid": "Embarrassed",
    "Feeling embarrassed or stupid": "Embarrassed",
    "Listening in church or meeting": "Church_meeting",
    "Church or meeting": "Church_meeting",
    "Other": "Other",
    "Others": "Other",
    "Autres": "Other",
}
REF_CAT = "Other"            # reference category of the multinomial model

# Optional extra filters (set to None to skip). The final dataset should
# already satisfy them; the flow is printed so counts can be checked.
AGE_RANGE = (18, 99)
SNR_RANGE = (-10, 20)
DROP_NORMAL_HEARING = False  # True drops PTA <= 15 dB HL (ASHA "Normal")

SPLINE_DF = 4                # natural cubic spline df for age and PTA
N_FOLDS = 5
N_SIM = 1000                 # parameter draws for prediction CIs
N_BOOT = 2000                # bootstrap draws for entropy CIs
SEED = 2026
# =============================================================================

rng = np.random.default_rng(SEED)
os.makedirs(OUT_DIR, exist_ok=True)
summary_lines = []


def log(s=""):
    print(s)
    summary_lines.append(str(s))


def asha_band(pta):
    bins = [-np.inf, 15, 25, 40, 55, 70, 90, np.inf]
    labels = ["Normal", "Slight", "Mild", "Moderate", "Mod_severe", "Severe", "Profound"]
    return pd.cut(pta, bins=bins, labels=labels, right=True)


def norm_entropy(counts):
    counts = np.asarray(counts, dtype=float)
    p = counts / counts.sum()
    p = p[p > 0]
    return float(-(p * np.log(p)).sum() / np.log(len(counts)))


def to_tex(df, path, caption, label, float_fmt="%.3f"):
    with open(path, "w") as f:
        f.write(df.to_latex(index=True, float_format=lambda x: float_fmt % x,
                            caption=caption, label=label, escape=True))


# -----------------------------------------------------------------------------
# 1. Load and prepare
# -----------------------------------------------------------------------------
raw = (pd.read_csv(DATA_PATH, low_memory=False) if DATA_PATH.endswith(".csv")
       else pd.read_excel(DATA_PATH, sheet_name=SHEET))
log(f"Loaded {len(raw)} rows, {raw.shape[1]} columns")
missing_cols = [v for v in COL.values() if v is not None and v not in raw.columns]
if missing_cols:
    sys.exit(f"Columns not found in Excel file: {missing_cols}. Edit COL in the config.")

d = pd.DataFrame({
    "age": pd.to_numeric(raw[COL["age"]], errors="coerce"),
    "sex": raw[COL["sex"]].astype("string").str.strip().map(SEX_MAP),
    "pta": pd.to_numeric(raw[COL["pta"]], errors="coerce"),
    "srt": pd.to_numeric(raw[COL["srt"]], errors="coerce"),
    "snr": pd.to_numeric(raw[COL["snr"]], errors="coerce"),
})
bad_sex = raw.loc[d.sex.isna() & raw[COL["sex"]].notna(), COL["sex"]]
if len(bad_sex):
    sys.exit(f"Unmapped sex values {bad_sex.value_counts().to_dict()}: add them to SEX_MAP.")
if COL.get("hl"):
    d["band_file"] = raw[COL["hl"]].astype("string").str.strip().map(HL_MAP)
unmapped = {}
for q in ["q1", "q2", "q3"]:
    if COL.get(q) is None:
        continue
    rawq = raw[COL[q]].astype("string").str.strip()
    mapped = rawq.map(CAT_MAP)
    bad = rawq[mapped.isna() & rawq.notna() & ~rawq.isin(["NA", "nan", ""])]
    if len(bad):
        unmapped[q] = bad.value_counts().to_dict()
    d[q] = mapped

if unmapped:
    log("WARNING unmapped category labels (counts), add them to CAT_MAP:")
    for q, vc in unmapped.items():
        log(f"  {q}: {vc}")

log("\n== Flow ==")
log(f"Start: {len(d)}")
d = d.dropna(subset=["age", "sex", "pta", "snr", "q1"])
log(f"Complete age/sex/PTA/SNR/Q1: {len(d)}  (SRT missing in {d.srt.isna().sum()})")
if AGE_RANGE:
    d = d[d.age.between(*AGE_RANGE)]
    log(f"Age in {AGE_RANGE}: {len(d)}")
if SNR_RANGE:
    d = d[d.snr.between(*SNR_RANGE)]
    log(f"SNR in {SNR_RANGE}: {len(d)}")
d["band"] = asha_band(d.pta)
if "band_file" in d:
    agree = (d.band.astype(str) == d.band_file.astype(str)).mean()
    log(f"Agreement of recomputed ASHA band with file HL column: {agree * 100:.2f}%")
    d["band"] = pd.Categorical(d.band_file, categories=list(HL_MAP.values()))
if DROP_NORMAL_HEARING:
    d = d[d.band != "Normal"]
    log(f"Dropping normal hearing: {len(d)}")
d = d.reset_index(drop=True)
N = len(d)
log(f"Analysis sample: N = {N}")

cats = [REF_CAT] + sorted(c for c in d.q1.unique() if c != REF_CAT)
d["q1"] = pd.Categorical(d.q1, categories=cats)
d["y"] = d.q1.cat.codes
d["female"] = (d.sex == "F").astype(int)

# -----------------------------------------------------------------------------
# 2. Descriptives (Table 1)
# -----------------------------------------------------------------------------
log("\n== Descriptives ==")
desc = []
for name, sub in [("Overall", d), ("Female", d[d.sex == "F"]), ("Male", d[d.sex == "M"])]:
    row = {"N": len(sub)}
    for v in ["age", "pta", "srt", "snr"]:
        row[f"{v} mean"] = sub[v].mean()
        row[f"{v} SD"] = sub[v].std()
    desc.append(pd.Series(row, name=name))
desc = pd.DataFrame(desc)
log(desc.round(2).to_string())
desc.to_csv(f"{OUT_DIR}/table1_continuous.csv")

q1_tab = pd.crosstab(d.q1, d.sex, margins=True)
q1_pct = (pd.crosstab(d.q1, d.sex, normalize="columns") * 100).round(1)
log("\nQ1 counts by sex:\n" + q1_tab.to_string())
q1_tab.to_csv(f"{OUT_DIR}/table1_q1_by_sex_counts.csv")
q1_pct.to_csv(f"{OUT_DIR}/table1_q1_by_sex_pct.csv")
log("\nHL band counts:\n" + d.band.value_counts().sort_index().to_string())

# -----------------------------------------------------------------------------
# 3. C2: heterogeneity and need profile by severity
# -----------------------------------------------------------------------------
log("\n== C2: heterogeneity by ASHA band (Severe and Profound merged) ==")
d["band2"] = d.band.astype(str).replace({"Profound": "Severe"})
band_order = [b for b in ["Normal", "Slight", "Mild", "Moderate", "Mod_severe", "Severe"]
              if b in d.band2.unique()]
ent_rows, prof_rows = [], []
for b in band_order:
    for with_other in [True, False]:
        sub = d[d.band2 == b]
        cc = sub.q1.value_counts().reindex(cats, fill_value=0)
        if not with_other:
            cc = cc.drop(REF_CAT)
        n = int(cc.sum())
        h = norm_entropy(cc.values)
        boot = [norm_entropy(rng.multinomial(n, cc.values / n)) for _ in range(N_BOOT)]
        ent_rows.append({"band": b, "incl_Other": with_other, "n": n, "H_norm": h,
                         "CI_low": np.percentile(boot, 2.5), "CI_high": np.percentile(boot, 97.5)})
    prof_rows.append((d[d.band2 == b].q1.value_counts(normalize=True)
                      .reindex(cats, fill_value=0) * 100).rename(b))
ent = pd.DataFrame(ent_rows)
profile = pd.DataFrame(prof_rows).T.round(1)
log(ent.round(3).to_string(index=False))
log("\nNeed profile by band (% of patients, Q1):\n" + profile.to_string())
ent.to_csv(f"{OUT_DIR}/c2_entropy_by_band.csv", index=False)
profile.to_csv(f"{OUT_DIR}/c2_profile_by_band.csv")
to_tex(profile, f"{OUT_DIR}/c2_profile_by_band.tex",
       "First-ranked COSI need by degree of hearing loss (\\% of patients).",
       "tab:profile_band", "%.1f")

# chi-square trend of the profile with band (descriptive)
ct = pd.crosstab(d.band2, d.q1).loc[band_order]
chi2, pchi, dfchi, _ = stats.chi2_contingency(ct)
cramer = np.sqrt(chi2 / (ct.values.sum() * (min(ct.shape) - 1)))
log(f"\nBand x Q1: chi2 = {chi2:.1f}, df = {dfchi}, p = {pchi:.2e}, Cramer's V = {cramer:.3f}")

# -----------------------------------------------------------------------------
# 4. C1 and C3: nested multinomial models
# -----------------------------------------------------------------------------
sp = SPLINE_DF
AUD = f"cr(pta, df={sp}) + cr(snr, df=3)"          # SRT added in sensitivity 3
F_DEMO = f"y ~ cr(age, df={sp}) + female"
F_AUD = f"y ~ {AUD}"
F_FULL = f"y ~ cr(age, df={sp}) + female + {AUD}"
F_FULLX = F_FULL + f" + female:cr(age, df={sp}) + female:pta"
MODELS = {"M0 demographics": F_DEMO, "M1 audiometry": F_AUD,
          "M2 demo + audiometry": F_FULL, "M3 + sex interactions": F_FULLX}


def fit_mnl(formula, data):
    return smf.mnlogit(formula, data).fit(method="bfgs", maxiter=3000, disp=False)


log("\n== C1/C3: multinomial models on Q1 (reference = %s) ==" % REF_CAT)
fits = {}
null_ll = None
for name, f in MODELS.items():
    fits[name] = fit_mnl(f, d)
    if null_ll is None:
        null_ll = fits[name].llnull
fit_tab = pd.DataFrame({
    name: {"logLik": r.llf, "df": r.params.size, "AIC": r.aic, "BIC": r.bic,
           "McFadden_R2": 1 - r.llf / null_ll, "converged": r.mle_retvals.get("converged", None)}
    for name, r in fits.items()}).T
log(fit_tab.to_string())
fit_tab.to_csv(f"{OUT_DIR}/c1_model_fit.csv")


def lrt(small, big):
    stat = 2 * (big.llf - small.llf)
    df = big.params.size - small.params.size
    return stat, df, stats.chi2.sf(stat, df)


tests = {
    "Audiometry block | age, sex (M2 vs M0)": lrt(fits["M0 demographics"], fits["M2 demo + audiometry"]),
    "Age + sex block | audiometry (M2 vs M1)": lrt(fits["M1 audiometry"], fits["M2 demo + audiometry"]),
    "Sex interactions (M3 vs M2)": lrt(fits["M2 demo + audiometry"], fits["M3 + sex interactions"]),
}
# sex main effect: M2 vs M2 without female
r_nosex = fit_mnl(F_FULL.replace(" + female", ""), d)
tests["Sex main effect (M2 vs M2 without sex)"] = lrt(r_nosex, fits["M2 demo + audiometry"])
lrt_tab = pd.DataFrame(tests, index=["chi2", "df", "p"]).T
log("\nLikelihood ratio tests:\n" + lrt_tab.to_string())
lrt_tab.to_csv(f"{OUT_DIR}/c1_lrt.csv")

# share of explained log-likelihood attributable to each block
dev_demo = fits["M0 demographics"].llf - null_ll
dev_aud = fits["M1 audiometry"].llf - null_ll
dev_full = fits["M2 demo + audiometry"].llf - null_ll
log(f"\nLog-likelihood gain over null: demographics {dev_demo:.1f}, audiometry {dev_aud:.1f}, "
    f"both {dev_full:.1f}")
log(f"Unique to audiometry: {(dev_full - dev_demo) / dev_full * 100:.1f}% of the full-model gain; "
    f"unique to age+sex: {(dev_full - dev_aud) / dev_full * 100:.1f}%")

# 5-fold cross-validated performance
log("\n== C1: 5-fold cross-validation ==")
skf = StratifiedKFold(n_splits=N_FOLDS, shuffle=True, random_state=SEED)
K = len(cats)
cv_rows = []
prior_ll, prior_auc = [], []
oof = {name: np.zeros((N, K)) for name in MODELS if name != "M3 + sex interactions"}
for tr, te in skf.split(d, d.y):
    dtr, dte = d.iloc[tr], d.iloc[te]
    prior = np.bincount(dtr.y, minlength=K) / len(dtr)
    prior_ll.append(log_loss(dte.y, np.tile(prior, (len(dte), 1)), labels=range(K)))
    for name in oof:
        r = fit_mnl(MODELS[name], dtr)
        oof[name][te] = np.asarray(r.predict(dte))
for name, P in oof.items():
    cv_rows.append({"model": name, "log_loss": log_loss(d.y, P, labels=range(K)),
                    "macro_AUC_ovr": roc_auc_score(d.y, P, multi_class="ovr", average="macro"),
                    "accuracy": float((P.argmax(1) == d.y).mean())})
cv = pd.DataFrame(cv_rows).set_index("model")
cv.loc["Prevalence only"] = [np.mean(prior_ll), 0.5, float((d.y == np.bincount(d.y).argmax()).mean())]
log(cv.round(4).to_string())
cv.to_csv(f"{OUT_DIR}/c1_cv_performance.csv")

# per-need AUC: which needs does audiometry discriminate?
auc_rows = []
for k, c in enumerate(cats):
    yk = (d.y == k).astype(int)
    auc_rows.append({"need": c, "n": int(yk.sum()),
                     **{f"AUC {name.split()[0]}": roc_auc_score(yk, P[:, k]) for name, P in oof.items()}})
auc_tab = pd.DataFrame(auc_rows).set_index("need")
log("\nOne-vs-rest cross-validated AUC per need:\n" + auc_tab.round(3).to_string())
auc_tab.to_csv(f"{OUT_DIR}/c1_auc_per_need.csv")
to_tex(auc_tab, f"{OUT_DIR}/c1_auc_per_need.tex",
       "Cross-validated one-vs-rest AUC per first-ranked need for demographic (M0), "
       "audiometric (M1) and combined (M2) multinomial models.", "tab:auc_need")

# -----------------------------------------------------------------------------
# 5. Predicted probabilities with simulation CIs (model M3)
# -----------------------------------------------------------------------------
res = fits["M3 + sex interactions"]
_data = res.model.data
design_info = getattr(_data, "design_info", None) or getattr(_data, "model_spec")
beta = res.params.values                    # (p, K-1)
# covariance from the observed information at the MLE (BFGS does not store it);
# parameter vector is beta flattened column by column (one outcome after another)
H = res.model.hessian(beta.flatten(order="F"))
cov = np.linalg.pinv(-H)
cov = (cov + cov.T) / 2
p_, km1 = beta.shape
draws = rng.multivariate_normal(beta.flatten(order="F"), cov, size=N_SIM)
draws = draws.reshape(N_SIM, km1, p_).transpose(0, 2, 1)   # (sim, p, K-1)


def predict_with_ci(grid):
    X = np.asarray(patsy.build_design_matrices([design_info], grid)[0])
    def probs(b):
        eta = np.column_stack([np.zeros(len(X)), X @ b])
        eta -= eta.max(1, keepdims=True)
        e = np.exp(eta)
        return e / e.sum(1, keepdims=True)
    point = probs(beta)
    sims = np.stack([probs(b) for b in draws])
    return point, np.percentile(sims, 2.5, axis=0), np.percentile(sims, 97.5, axis=0), sims


med = d[["age", "srt", "snr"]].median()
q_pta = d.pta.quantile([0.02, 0.98]).values
q_age = d.age.quantile([0.02, 0.98]).values


def grid_over(var, values, sex):
    g = pd.DataFrame({"age": med.age, "srt": med.srt, "snr": med.snr, "pta": d.pta.median(),
                      "female": int(sex == "F")}, index=range(len(values)))
    g[var] = values
    if var == "pta":   # keep SRT and SNR at their expected value given PTA
        for v in ["srt", "snr"]:
            lm = sm.OLS(d[v], sm.add_constant(d.pta)).fit()
            g[v] = lm.params["const"] + lm.params["pta"] * g.pta
    return g


pred_rows = []
for var, rngv in [("pta", np.linspace(*q_pta, 40)), ("age", np.linspace(*q_age, 40))]:
    fig, axes = plt.subplots(2, int(np.ceil(K / 2)), figsize=(16, 7), sharex=True)
    for sex, colr in [("F", "tab:red"), ("M", "tab:blue")]:
        g = grid_over(var, rngv, sex)
        pt, lo, hi, _ = predict_with_ci(g)
        for k, c in enumerate(cats):
            ax = axes.flat[k]
            ax.plot(rngv, pt[:, k] * 100, color=colr, label=sex)
            ax.fill_between(rngv, lo[:, k] * 100, hi[:, k] * 100, color=colr, alpha=0.2)
            ax.set_title(c, fontsize=9)
            for i, x in enumerate(rngv):
                pred_rows.append({"over": var, "x": x, "sex": sex, "need": c,
                                  "p": pt[i, k], "lo": lo[i, k], "hi": hi[i, k]})
    for ax in axes.flat[K:]:
        ax.axis("off")
    axes.flat[0].legend()
    fig.supxlabel("PTA (dB HL)" if var == "pta" else "Age (years)")
    fig.supylabel("Predicted probability of first-ranked need (%)")
    fig.tight_layout()
    fig.savefig(f"{OUT_DIR}/pred_prob_by_{var}.pdf")
    plt.close(fig)
pd.DataFrame(pred_rows).to_csv(f"{OUT_DIR}/pred_prob_curves.csv", index=False)

# Contrasts: PTA 30 vs 60 dB HL, age 60 vs 80, female vs male (others at median)
log("\n== Predicted-probability contrasts (M3; percentage points, 95% simulation CI) ==")
contr_rows = []


def contrast(label, g_a, g_b):
    _, _, _, sa = predict_with_ci(g_a)
    _, _, _, sb = predict_with_ci(g_b)
    diff = (sb - sa)[:, 0, :] * 100
    pa = predict_with_ci(g_a)[0][0] * 100
    pb = predict_with_ci(g_b)[0][0] * 100
    for k, c in enumerate(cats):
        contr_rows.append({"contrast": label, "need": c, "p_a": pa[k], "p_b": pb[k],
                           "diff_pp": pb[k] - pa[k],
                           "lo": np.percentile(diff[:, k], 2.5), "hi": np.percentile(diff[:, k], 97.5)})


for sex in ["F", "M"]:
    contrast(f"PTA 30 -> 60 dB ({sex})", grid_over("pta", [30], sex), grid_over("pta", [60], sex))
    contrast(f"Age 60 -> 80 ({sex})", grid_over("age", [60], sex), grid_over("age", [80], sex))
contrast("Male -> Female", grid_over("age", [med.age], "M"), grid_over("age", [med.age], "F"))
contr = pd.DataFrame(contr_rows)
log(contr.round(2).to_string(index=False))
contr.to_csv(f"{OUT_DIR}/contrasts.csv", index=False)
wide = contr.assign(txt=lambda x: x.diff_pp.map("{:+.1f}".format) + " (" +
                    x.lo.map("{:.1f}".format) + ", " + x.hi.map("{:.1f}".format) + ")"
                    ).pivot(index="need", columns="contrast", values="txt")
with open(f"{OUT_DIR}/contrasts.tex", "w") as f:
    f.write(wide.to_latex(caption="Change in predicted probability of each first-ranked need "
                          "(percentage points, 95\\% simulation interval), other covariates at the median.",
                          label="tab:contrasts", escape=True))

# -----------------------------------------------------------------------------
# 6. Sensitivity analyses
# -----------------------------------------------------------------------------
log("\n== Sensitivity 1: excluding 'Other' ==")
d_no = d[d.q1 != REF_CAT].copy()
cats_no = sorted(d_no.q1.astype(str).unique())
d_no["y"] = pd.Categorical(d_no.q1.astype(str), categories=cats_no).codes
r0 = fit_mnl(F_DEMO, d_no)
r2 = fit_mnl(F_FULL, d_no)
r1 = fit_mnl(F_AUD, d_no)
s_stat, s_df, s_p = lrt(r0, r2)
log(f"N = {len(d_no)}; McFadden R2: demo {1 - r0.llf / r0.llnull:.4f}, "
    f"audiometry {1 - r1.llf / r1.llnull:.4f}, both {1 - r2.llf / r2.llnull:.4f}; "
    f"audiometry block LRT chi2 = {s_stat:.1f}, df = {s_df}, p = {s_p:.2e}")

if COL.get("q2") and COL.get("q3"):
    log("\n== Sensitivity 2: any mention in Q1-Q3 (one logistic model per need) ==")
    ml_rows = []
    for c in cats:
        if c == REF_CAT:
            continue
        dd = d.copy()
        dd["any"] = ((dd.q1.astype(str) == c) | (dd.q2 == c) | (dd.q3 == c)).astype(int)
        f0 = f"any ~ cr(age, df={sp}) + female"
        f2 = f0 + f" + cr(snr, df=3) + pta"
        m0 = smf.logit(f0, dd).fit(disp=False)
        m2 = smf.logit(f2, dd).fit(disp=False)
        st, df_, p = lrt(m0, m2)
        b, se = m2.params["pta"], m2.bse["pta"]
        ml_rows.append({"need": c, "prevalence_%": dd["any"].mean() * 100,
                        "OR_per_10dB_PTA": np.exp(10 * b),
                        "OR_lo": np.exp(10 * (b - 1.96 * se)), "OR_hi": np.exp(10 * (b + 1.96 * se)),
                        "OR_female": np.exp(m2.params["female"]),
                        "audiometry_LRT_p": p,
                        "AUC_demo": roc_auc_score(dd["any"], m0.predict(dd)),
                        "AUC_full": roc_auc_score(dd["any"], m2.predict(dd))})
    ml = pd.DataFrame(ml_rows).set_index("need")
    from statsmodels.stats.multitest import multipletests
    ml["audiometry_p_FDR"] = multipletests(ml.audiometry_LRT_p, method="fdr_bh")[1]
    log(ml.round(4).to_string())
    ml.to_csv(f"{OUT_DIR}/sens_any_mention_Q1Q3.csv")
    to_tex(ml, f"{OUT_DIR}/sens_any_mention_Q1Q3.tex",
           "Sensitivity analysis: any mention of each need in Q1 to Q3. Odds ratios from logistic "
           "models adjusted for age (spline), sex and SNR; audiometry block tested by LRT with "
           "Benjamini-Hochberg correction. AUCs are in-sample.", "tab:sens_any")

log("\n== Sensitivity 3: adding SRT, complete cases only ==")
d_srt = d.dropna(subset=["srt"]).copy()
F_AUD_S = F_AUD + " + cr(srt, df=3)"
F_FULL_S = F_FULL + " + cr(srt, df=3)"
s0, s1, s2 = (fit_mnl(F_DEMO, d_srt), fit_mnl(F_AUD_S, d_srt), fit_mnl(F_FULL_S, d_srt))
s_nosrt = fit_mnl(F_FULL, d_srt)
st, sdf, sp_ = lrt(s0, s2)
st2, sdf2, sp2 = lrt(s_nosrt, s2)
log(f"N = {len(d_srt)}; McFadden R2: demo {1 - s0.llf / s0.llnull:.4f}, "
    f"audiometry incl. SRT {1 - s1.llf / s1.llnull:.4f}, both {1 - s2.llf / s2.llnull:.4f}")
log(f"Audiometry block (PTA, SNR, SRT) | age, sex: chi2 = {st:.1f}, df = {sdf}, p = {sp_:.2e}")
log(f"SRT added to age, sex, PTA, SNR: chi2 = {st2:.1f}, df = {sdf2}, p = {sp2:.2e}")
cmp_ = pd.crosstab(d.srt.isna(), d.q1, normalize="index").T * 100
cmp_.columns = ["SRT available", "SRT missing"]
log("Q1 profile (%) by SRT availability:\n" + cmp_.round(1).to_string())
log(f"Mean PTA: SRT available {d.loc[d.srt.notna(), 'pta'].mean():.1f}, "
    f"missing {d.loc[d.srt.isna(), 'pta'].mean():.1f}; mean age: "
    f"{d.loc[d.srt.notna(), 'age'].mean():.1f} vs {d.loc[d.srt.isna(), 'age'].mean():.1f}")

with open(f"{OUT_DIR}/summary.txt", "w") as f:
    f.write("\n".join(summary_lines))
print(f"\nDone. Aggregated outputs in {OUT_DIR}/ ; paste summary.txt into the chat.")
