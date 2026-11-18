#!/usr/bin/env python3
"""
Figures for the JAMA Network Open manuscript "Beyond the Audiogram".

Main text
  figs/fig1_need_profile_by_hl.pdf   Need profile and diversity by degree of hearing loss
  figs/fig2_auc_by_need.pdf          How well age/sex and audiometry identify each need
  figs/fig3_composition_pta_age.pdf  Predicted need mix across PTA and age
Supplement
  figs/efig_pred_prob_by_pta.pdf     Predicted probability of each need by PTA, by sex, 95% CI
  figs/efig_pred_prob_by_age.pdf     Same by age

Figures 1 and 2 use aggregated results of cosi_reanalysis.py (13 Nov 2026), typed below.
Figure 3 and the eFigures read cosi_reanalysis_out/pred_prob_curves.csv (model predictions,
no patient data). PNG previews are written next to each PDF.

Usage:  python3 make_figures.py [path/to/pred_prob_curves.csv]
"""

import os
import sys

import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib import font_manager
from matplotlib.lines import Line2D
from matplotlib.patches import Patch

CSV = sys.argv[1] if len(sys.argv) > 1 else "cosi_reanalysis_out/pred_prob_curves.csv"
OUT = "figs"
os.makedirs(OUT, exist_ok=True)

# ----------------------------------------------------------------------------- style
INK, INK2, MUTED, GRID, SURF = "#1f1f1e", "#4a4944", "#77766f", "#e6e5e0", "#ffffff"
available = {f.name for f in font_manager.fontManager.ttflist}
FAMILY = next((f for f in ["Arial", "Helvetica", "Liberation Sans", "DejaVu Sans"] if f in available),
              "DejaVu Sans")
plt.rcParams.update({
    "font.family": FAMILY, "font.size": 8, "axes.titlesize": 9, "axes.labelsize": 8,
    "xtick.labelsize": 7.5, "ytick.labelsize": 7.5, "axes.edgecolor": MUTED,
    "axes.labelcolor": INK, "xtick.color": INK2, "ytick.color": INK2, "text.color": INK,
    "pdf.fonttype": 42, "ps.fonttype": 42, "axes.linewidth": 0.6,
})

# Needs in a fixed order with fixed colours (validated categorical palette, Other in neutral gray)
NEEDS = [
    ("Group_noise", "Group conversation in noise", "#2a78d6"),
    ("Conv12_quiet", "Conversation with 1 or 2 in quiet", "#eb6834"),
    ("TV_radio", "Television or radio", "#1baf7a"),
    ("Embarrassed", "Feeling embarrassed or stupid", "#eda100"),
    ("Group_quiet", "Group conversation in quiet", "#e87ba4"),
    ("Church_meeting", "Church or meeting", "#008300"),
    ("Phone_unfamiliar", "Unfamiliar speaker on the phone", "#6250d6"),
    ("Social_contact", "Increased social contact", "#e34948"),
    ("Other", "Other (outside the COSI categories)", "#c3c2b7"),
]
KEYS = [k for k, _, _ in NEEDS]
NAME = {k: n for k, n, _ in NEEDS}
COLOR = {k: c for k, _, c in NEEDS}


def text_on(hexcolor):
    """Ink or white text, whichever reads better on a fill."""
    r, g, b = (int(hexcolor[i:i + 2], 16) / 255 for i in (1, 3, 5))
    lum = 0.2126 * r ** 2.2 + 0.7152 * g ** 2.2 + 0.0722 * b ** 2.2
    return INK if lum > 0.32 else "white"


def clean(ax, left=True, bottom=True):
    for side in ["top", "right"]:
        ax.spines[side].set_visible(False)
    ax.spines["left"].set_visible(left)
    ax.spines["bottom"].set_visible(bottom)
    ax.tick_params(width=0.6, length=2.5)


def save(fig, name):
    fig.savefig(f"{OUT}/{name}.pdf")
    fig.savefig(f"{OUT}/{name}.png", dpi=220)
    plt.close(fig)
    print(f"wrote {OUT}/{name}.pdf")


def need_legend(fig, y=1.0, ncol=3):
    handles = [Patch(facecolor=COLOR[k], edgecolor="none", label=NAME[k]) for k in KEYS]
    fig.legend(handles=handles, loc="upper center", ncol=ncol, frameon=False,
               bbox_to_anchor=(0.5, y), handlelength=1.1, handleheight=0.9,
               columnspacing=1.4, fontsize=7.5)


# ----------------------------------------------------------------------------- Figure 1
# % of patients per first-ranked need, by degree of hearing loss (eTable 2 counts)
PROFILE = pd.DataFrame({
    "PTA ≤15 dB HL": [27.6, 7.5, 6.1, 7.2, 4.2, 4.8, 2.1, 0.4, 40.0],
    "Slight":             [27.9, 10.7, 9.3, 7.9, 4.3, 5.9, 1.8, 0.5, 31.6],
    "Mild":               [24.9, 15.8, 12.5, 9.2, 6.1, 5.4, 1.4, 0.8, 24.0],
    "Moderate":           [21.2, 20.8, 12.6, 10.4, 7.8, 4.1, 1.2, 1.1, 20.7],
    "Moderately severe":  [19.3, 25.6, 9.8, 10.1, 9.9, 3.1, 1.8, 1.2, 19.2],
    "Severe or profound": [16.2, 28.7, 6.8, 8.8, 10.2, 3.3, 2.1, 1.7, 22.2],
}, index=KEYS)
N_BAND = [667, 4597, 33924, 40749, 10430, 920]
ENTROPY = [(0.76, 0.72, 0.78), (0.81, 0.80, 0.82), (0.86, 0.85, 0.86),
           (0.87, 0.87, 0.87), (0.87, 0.86, 0.87), (0.86, 0.83, 0.87)]


def fig1():
    bands = list(PROFILE.columns)
    fig = plt.figure(figsize=(7.2, 4.1))
    ax = fig.add_axes([0.20, 0.10, 0.62, 0.66])
    axe = fig.add_axes([0.85, 0.10, 0.13, 0.66], sharey=ax)
    y = np.arange(len(bands))[::-1]
    for yi, b in zip(y, bands):
        vals = PROFILE[b].values / PROFILE[b].sum() * 100
        left = 0.0
        for k, v in zip(KEYS, vals):
            ax.barh(yi, v, left=left, height=0.68, color=COLOR[k], edgecolor=SURF, linewidth=1.2)
            if v >= 6:
                ax.text(left + v / 2, yi, f"{v:.0f}", ha="center", va="center",
                        fontsize=7, color=text_on(COLOR[k]), fontweight="bold")
            left += v
    ax.set_xlim(0, 100)
    ax.set_xticks([0, 25, 50, 75, 100])
    ax.set_xlabel("Patients, %")
    ax.set_yticks(y)
    ax.set_yticklabels([f"{b}\n(n = {n:,})" for b, n in zip(bands, N_BAND)], fontsize=7.5)
    ax.tick_params(axis="y", length=0)
    clean(ax, left=False)
    ax.set_title("First-ranked need", loc="left", fontweight="bold")

    # need diversity: normalised entropy with 95% CI
    for yi, (h, lo, hi) in zip(y, ENTROPY):
        axe.plot([lo, hi], [yi, yi], color=INK2, lw=1.4, solid_capstyle="round")
        axe.plot(h, yi, "o", ms=6, color=INK, mec=SURF, mew=1.2)
        axe.text(h, yi + 0.32, f"{h:.2f}", ha="center", fontsize=7, color=INK)
    axe.set_xlim(0.70, 0.90)
    axe.set_xticks([0.7, 0.8, 0.9])
    axe.set_xlabel("Index (0 to 1)")
    axe.grid(axis="x", color=GRID, lw=0.6)
    axe.set_axisbelow(True)
    axe.tick_params(axis="y", left=False, labelleft=False)
    clean(axe, left=False)
    axe.set_title("Diversity of needs", loc="left", fontweight="bold")
    need_legend(fig, y=0.995)
    save(fig, "fig1_need_profile_by_hl")


# ----------------------------------------------------------------------------- Figure 2
# Cross-validated one-vs-rest AUC per need (Table 3): age+sex, audiometry, both
AUC = pd.DataFrame({
    "demo": [0.547, 0.561, 0.586, 0.532, 0.536, 0.681, 0.623, 0.504, 0.544],
    "aud":  [0.544, 0.583, 0.538, 0.522, 0.571, 0.579, 0.521, 0.564, 0.548],
    "both": [0.560, 0.589, 0.608, 0.546, 0.577, 0.684, 0.624, 0.563, 0.556],
}, index=KEYS)
MACRO = {"demo": 0.568, "aud": 0.552, "both": 0.590}
MODEL_STYLE = {
    "demo": dict(color="#eb6834", marker="o", label="Age and sex"),
    "aud":  dict(color="#2a78d6", marker="s", label="Audiogram and speech in noise"),
    "both": dict(color=INK, marker="D", label="All combined"),
}


def fig2():
    rows = [("macro", "All needs (average)")] + \
           [(k, NAME[k].replace(" (outside the COSI categories)", "")) for k in
            AUC.sort_values("both", ascending=False).index]
    fig, ax = plt.subplots(figsize=(7.2, 4.0))
    fig.subplots_adjust(left=0.30, right=0.97, top=0.84, bottom=0.13)
    y = np.arange(len(rows))[::-1].astype(float)
    y[1:] -= 0.5                          # gap below the summary row
    ax.axvspan(0.70, 1.0, color="#e9f3ec", lw=0)
    ax.text(0.705, y[0] + 0.75, "Acceptable discrimination\n(AUC ≥0.70)", fontsize=7,
            color=INK2, va="top")
    ax.axvline(0.5, color=MUTED, lw=0.9, ls=(0, (3, 2)))
    ax.text(0.503, y[0] + 0.75, "Chance", fontsize=7, color=INK2, va="top")
    for yi, (k, label) in zip(y, rows):
        vals = MACRO if k == "macro" else AUC.loc[k].to_dict()
        ax.plot([min(vals.values()), max(vals.values())], [yi, yi], color="#cfcec8", lw=2.2,
                solid_capstyle="round", zorder=1)
        for m, st in MODEL_STYLE.items():
            dy = {"demo": 0.16, "aud": -0.16, "both": 0.0}[m]
            ax.plot(vals[m], yi + dy, st["marker"], color=st["color"], ms=6.5 if m != "both" else 6,
                    mec=SURF, mew=1.0, zorder=3 if m != "both" else 4)
    ax.set_yticks(y)
    ax.set_yticklabels([r[1] for r in rows])
    ax.get_yticklabels()[0].set_fontweight("bold")
    ax.tick_params(axis="y", length=0)
    ax.set_xlim(0.48, 0.82)
    ax.set_ylim(y[-1] - 0.7, y[0] + 0.9)
    ax.set_xlabel("Ability to identify the patient's first-ranked need (cross-validated AUC)")
    ax.grid(axis="x", color=GRID, lw=0.6)
    ax.set_axisbelow(True)
    clean(ax, left=False)
    handles = [Line2D([], [], ls="", marker=st["marker"], color=st["color"], ms=6.5,
                      mec=SURF, label=st["label"]) for st in MODEL_STYLE.values()]
    fig.legend(handles=handles, loc="upper center", ncol=3, frameon=False,
               bbox_to_anchor=(0.62, 0.99), title="Information used to predict the need",
               title_fontsize=7.5, fontsize=7.5)
    save(fig, "fig2_auc_by_need")


# ----------------------------------------------------------------------------- Figure 3 and eFigures
def load_curves():
    if not os.path.exists(CSV):
        print(f"{CSV} not found: Figure 3 and eFigures skipped")
        return None
    d = pd.read_csv(CSV)
    missing = set(KEYS) - set(d.need)
    if missing:
        sys.exit(f"Needs not found in {CSV}: {missing}")
    return d


W_FEMALE = 44525 / 91287


def fig3(d):
    fig, axes = plt.subplots(1, 2, figsize=(7.2, 3.9), sharey=True)
    fig.subplots_adjust(left=0.08, right=0.98, top=0.78, bottom=0.14, wspace=0.08)
    for ax, (over, xlabel, ticks, title) in zip(axes, [
            ("pta", "Pure-tone average, dB HL", [20, 30, 40, 50, 60, 70], "By hearing level"),
            ("age", "Age, y", [40, 50, 60, 70, 80, 90], "By age")]):
        s = d[d.over == over]
        w = s.pivot_table(index=["x", "need"], columns="sex", values="p").reset_index()
        w["p"] = W_FEMALE * w["F"] + (1 - W_FEMALE) * w["M"]
        m = w.pivot(index="x", columns="need", values="p")[KEYS] * 100
        m = m.div(m.sum(axis=1), axis=0) * 100
        x = m.index.values
        ax.stackplot(x, *[m[k].values for k in KEYS], colors=[COLOR[k] for k in KEYS],
                     edgecolor=SURF, linewidth=0.8)
        # labels inside the two largest and most changing bands, at both ends
        cum = m.cumsum(axis=1)
        for k in ["Group_noise", "Conv12_quiet", "TV_radio"]:
            for xi, ha, dx in [(0, "left", 0.02), (-1, "right", -0.02)]:
                top = cum[k].iloc[xi]
                v = m[k].iloc[xi]
                if v >= 7:
                    xx = x[xi] + dx * (x[-1] - x[0])
                    ax.text(xx, top - v / 2, f"{v:.0f}%", ha=ha, va="center", fontsize=7,
                            color=text_on(COLOR[k]), fontweight="bold")
        ax.set_xlim(x[0], x[-1])
        ax.set_ylim(0, 100)
        ax.set_xticks([t for t in ticks if x[0] <= t <= x[-1]])
        ax.set_xlabel(xlabel)
        ax.set_title(title, loc="left", fontweight="bold")
        clean(ax)
    axes[0].set_ylabel("Predicted share of first-ranked needs, %")
    need_legend(fig, y=0.995)
    save(fig, "fig3_composition_pta_age")


def efig(d, over, xlabel, ticks, name):
    sub = d[d.over == over]
    sexcol, sexlab = {"F": "#eb6834", "M": "#2a78d6"}, {"F": "Women", "M": "Men"}
    fig, axes = plt.subplots(3, 3, figsize=(7.2, 6.6), sharex=True)
    for ax, k in zip(axes.flat, KEYS):
        s = sub[sub.need == k]
        for sex in ["F", "M"]:
            g = s[s.sex == sex].sort_values("x")
            ax.fill_between(g.x, g.lo * 100, g.hi * 100, color=sexcol[sex], alpha=0.18, lw=0)
            ax.plot(g.x, g.p * 100, color=sexcol[sex], lw=1.6)
        lo, hi = s.lo.min() * 100, s.hi.max() * 100
        pad = 0.08 * (hi - lo) if hi > lo else 1
        ax.set_ylim(max(0, lo - pad), hi + pad)
        ax.set_title(NAME[k].replace(" (outside the COSI categories)", ""), loc="left",
                     fontweight="bold", fontsize=8)
        ax.grid(axis="y", color=GRID, lw=0.6)
        ax.set_axisbelow(True)
        ax.set_xticks(ticks)
        clean(ax)
    handles = [Line2D([], [], color=sexcol[s], lw=1.6, label=sexlab[s]) for s in ["F", "M"]]
    handles.append(Patch(color=MUTED, alpha=0.25, label="95% CI"))
    fig.legend(handles=handles, loc="upper center", ncol=3, frameon=False, bbox_to_anchor=(0.5, 1.0))
    fig.supxlabel(xlabel, fontsize=8)
    fig.supylabel("Predicted probability of first-ranked need, %", fontsize=8)
    fig.tight_layout(rect=(0.01, 0, 1, 0.965), h_pad=1.2, w_pad=1.0)
    save(fig, name)


fig1()
fig2()
curves = load_curves()
if curves is not None:
    fig3(curves)
    efig(curves, "pta", "Pure-tone average, dB HL", [20, 30, 40, 50, 60, 70], "efig_pred_prob_by_pta")
    efig(curves, "age", "Age, y", [40, 50, 60, 70, 80, 90], "efig_pred_prob_by_age")
