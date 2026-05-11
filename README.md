# 🏥 Convenience Absenteeism in the Portuguese NHS
### Analysing Self-Declared Disease Notifications (ADD) from the SNS Transparency Portal

> **Laboratory Project in Health Data Science** - Faculty of Medicine - University of Porto, 2026  
> *Ana Correia · André Vicente · Filipa Carneiro*

---

## 📋 Research Question

> *Are workers using the Self-Declared Disease Notification (ADD) system strategically - to extend weekends and bridge public holidays - rather than for genuine illness?*

Since 2022, the SNS 24 app and portal allow Portuguese workers to self-declare up to 3 consecutive sick days without a doctor's appointment, up to twice per year. This project uses time-series modelling to **disentangle clinical demand (driven by the proxy flu) from behavioural demand (driven by calendar incentives)**.

---

## 📁 Repository Structure

```
├── Pipeline v5/                        ← Pipeline version 5
│   ├── code/
│   │   ├── pipeline_sarimax_v5.R
│   ├── outputs/
│   │   ├── grafico_ADD_vs_previsto_v5.png
│   │   ├── grafico_perfil_semanal_v5.png
│   │   ├── impacto_economico_v5.csv
│   │   ├── outliers_identificados_v5.csv
│   │   └── resultados_coeficientes_v5.csv
├── README.md                          ← This file
├── prompts.md                         ← LLM interactions
├── data/
│   ├── autodeclaracoes-de-doenca-dos-utentes.csv   ← ADD data (SNS Portal)
│   └── atendimentos-nos-csp-gripe.csv              ← Flu consultations (SNS Portal)
├── code/
│   ├── Project.R                      ← Original SARIMAX model
│   ├── pipeline_sarimax_v2.R          ← SARIMAX model with calendar dummies
│   └── pipeline_sarimax_v3.R          ← v3: outlier pass + Wednesday dummy + 5yr forecast
├── outputs/
│   ├── resultados_coeficientes.csv    ← Model coefficients table
│   ├── impacto_economico.csv          ← Economic impact estimates
│   ├── outliers_identificados.csv     ← Detected outlier days (v3)
│   ├── grafico_ADD_vs_previsto.png    ← Observed vs fitted ADD series
│   └── grafico_perfil_semanal.png     ← Weekly profile of ADD by day type
├── dashboard/
│   └── convenience_absenteeism.pbix   ← Power BI interactive dashboard
└── presentation/
    └── Convenience_Absenteeism_SNS.pptx
```

---

## 🗂️ Data Sources

| Dataset | Source | Variables used | Period |
|---|---|---|---|
| ADD — Self-Declared Disease Notifications | [SNS Transparency Portal](https://transparencia.sns.gov.pt) | `Nº ADD Emitidas` (daily total) | 2023 – 2026 |
| Flu Consultations at Primary Care | [SNS Transparency Portal](https://transparencia.sns.gov.pt) | `Nº Consultas Gripe nos CSP` (daily, by region) | 2016 – 2026 |

Both datasets are publicly available and were downloaded as CSV. The two series are merged by date (`inner_join`). Days present in ADD data but absent from flu data are audited before exclusion.

**Study population:** All SNS users who submitted an ADD via SNS 24 App, Portal SNS 24, or Linha SNS 24.

---

## ⚙️ Methodology

### Model

A **SARIMAX model with Fourier terms** was estimated in R using the `forecast` package:

```
ADDₜ = ARIMA(p,d,q)(P,D,Q)_s
     + β₁·Gripe_CSPₜ          # exogenous: flu consultations (clinical signal)
     + β₂·Feriadoₜ             # binary: public holiday
     + β₃·Tolerânciaₜ          # binary: government tolerance day (Carnival, etc.)
     + β₄·Ponteₜ               # binary: bridge day between holiday and weekend
     + β₅·Segunda_Comumₜ       # binary: regular Monday (not holiday/bridge)
     + β₆·Quarta_Comumₜ        # binary: regular Wednesday  [added in v3]
     + Σ Fourier(K=5, f=365.25) # annual seasonality
     + εₜ
```

### Calendar dummy hierarchy

Priority rule (to avoid overlap): **Public Holiday > Tolerance Day > Bridge Day > Monday / Wednesday**

| Dummy | Description |
|---|---|
| `Feriado` | 10 fixed + 3 movable Portuguese public holidays (Easter algorithm) |
| `Tolerancia` | Official government tolerance days (e.g. Carnival, Holy Thursday, 26 Dec) |
| `Ponte` | Working day directly adjacent to a holiday/tolerance with weekend on other side |
| `Segunda_Comum` | Regular Mondays not covered by higher-priority flags |
| `Quarta_Comum` | Regular Wednesdays (v3 addition) |

### Version history

| Version | Key changes |
|---|---|
| v1 | Initial SARIMAX prototype |
| v2 | Bug fixes (typo, Ljung-Box df, Fourier terms K=3, alignment checks, reproducibility seed) |
| v3 | + Wednesday dummy · outlier two-pass (±3σ) · 5-year cost forecast · weekly profile plot |

### Economic translation

```
Excess ADDs  = β_dummy × N_days_in_period
Economic cost = Excess ADDs × €150 / day
95% CI       = (β ± 1.96 × SE) × N_days × €150
```

> ⚠️ **Assumption:** €150/day average labour cost per absent worker. Only statistically significant variables (p < 0.05) are interpreted causally.

---

## 🚀 How to Run

### Prerequisites

```r
# R ≥ 4.2
install.packages(c("tidyverse", "lubridate", "forecast", "tseries", "xts", "openxlsx"))
```

### Steps

1. Clone the repository
2. Place the two CSVs in the working directory (or update paths in the script)
3. Run the pipeline:

```r
# Recommended: run v3 for the full analysis
source("code/pipeline_sarimax_v3.R")
```

4. Outputs are saved automatically to the working directory
5. Open `dashboard/convenience_absenteeism.pbix` in Power BI Desktop

> **Reproducibility note:** `set.seed(42)` is set before `auto.arima()`. Full exploration mode (`stepwise = FALSE`, `approximation = FALSE`) is used — runtime ~3–8 minutes.

---

## 📊 Key Results

| Variable | β | p-value | Interpretation |
|---|---|---|---|
| Flu Consultations (Gripe) | 1.00 | < 0.001 | Clinical signal — genuine illness driver |
| Public Holiday (Feriado) | −316​ | < 0.001 | Day off → fewer ADDs submitted |
| Tolerance Day (Tolerância) | −584 | < 0.001 | Bridge around tolerance days |
| Bridge Day (Ponte) | +458 | < 0.001 | Strategic absence for long weekend |
| **Monday (Segunda_Comum)** | **+1379** | **< 0.001** | **Post-weekend extension — strongest effect** |
| **Wednesday (Quarta_Comum)** | **+697​** | **< 0.001** | **Mid-week split strategy** |

---

## 📈 Dashboard

The Power BI dashboard (`dashboard/convenience_absenteeism.pbix`) allows:

- Filtering by **sex**, **age group**, and **time period** (Year / Quarter / Month)
- Visualising **ADD vs flu consultations** over time (national)
- Exploring ADD **origin** (SNS 24 App · Portal SNS 24 · Linha SNS 24)
- Comparing **flu consultation rates by NUTS II region**

---

## ⚠️ Limitations

- **No individual-level data** — aggregate counts only; association ≠ individual behaviour
- **Labour cost assumption** — €150/day is a placeholder; results are sensitive to this
- **National aggregation** — regional heterogeneity is not modelled (dashboard shows differences)
- **Short series** — ADD system started 2023 (~3–4 flu seasons), limiting forecast stability
- **Unmeasured confounders** — remote work rates, sector mix, and age structure are not controlled
- **Forecast uncertainty** — 5-year projections assume constant β and ADD growth trends

---

## 🤖 LLM Usage

See [`prompts.md`](prompts.md) for the complete record of:
- All prompts submitted to Gemini, Claude and ChatGPT

We used LLMs to assist with (1) research question formulation, (2) data source identification, (3) methodology planning, and (4) code debugging — not to perform the analysis autonomously.

---

## 📬 Contact

For questions about this project, open an issue in this repository.

---

*Data source: [SNS Transparency Portal](https://transparencia.sns.gov.pt) — open data, Ministry of Health, Portugal.*
