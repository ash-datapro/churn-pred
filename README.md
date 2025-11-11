# Post-Sales Customer Churn (R + Shiny + Plumber)

<p align="center">
  <img src="media/demo.gif" width="100%" alt="App demo">
</p>

<p align="center">
  <a href="#"><img src="https://img.shields.io/badge/R-%3E%3D4.3-blue"></a>
  <a href="#"><img src="https://img.shields.io/badge/Shiny-UI-%2300ADD8"></a>
  <a href="#"><img src="https://img.shields.io/badge/plumber-API-%23E76F51"></a>
  <a href="#"><img src="https://img.shields.io/badge/PostgreSQL-%3E%3D13-%234169E1"></a>
  <a href="#"><img src="https://img.shields.io/badge/tidymodels-xgboost-success"></a>
  <a href="#"><img src="https://img.shields.io/badge/Docker-optional-lightgrey"></a>
</p>

**A production-style, end-to-end churn analytics project**:

* **Frontend**: Dark-themed Shiny dashboard with filters, KPIs, insights, and a live prediction form.
* **Backend**: `plumber` REST API serving a tidymodels gradient boosting model.
* **Data**: PostgreSQL with reproducible load/feature engineering scripts.
* **Reports**: Confusion matrix, ROC curve, and a markdown report for the trained model.

---

## Table of Contents

* [Architecture](#architecture)
* [Features](#features)
* [Screenshots](#screenshots)
* [Getting Started](#getting-started)

  * [Prerequisites](#prerequisites)
  * [Repository Layout](#repository-layout)
  * [Environment](#environment)
  * [1) Load Data](#1-load-data)
  * [2) Train Model](#2-train-model)
  * [3) Run the API](#3-run-the-api)
  * [4) Run the Shiny App](#4-run-the-shiny-app)
* [API](#api)
* [Usage Notes & Tips](#usage-notes--tips)
* [Troubleshooting](#troubleshooting)
* [Roadmap](#roadmap)
* [Contributing](#contributing)
* [License](#license)

---

## Architecture

```
PostgreSQL  ←  data/load-data.R
      ↑
      │                   ┌──────────────────────────────┐
      │                   │        Shiny Frontend        │
      │                   │  KPIs, filters, insights,    │
      │                   │  predictions (dark theme)    │
      │                   └──────────────┬───────────────┘
      │                                  │ HTTP (JSON)
      │                           /predict (plumber)
      │                                  │
      └──────────  backend/api/train-model.R  ──►  model.rds
```

* **Modeling**: tidymodels workflow with `xgboost` tuning, F1-optimized threshold, saved as `model.rds`.
* **API**: `plumber` exposes `/health` and `/predict`, loading `model.rds`.
* **UI**: Shiny dashboard (dark, compact), filterable, with insights plots and a guided prediction form.

---

## Features

* **Interactive dashboard**: KPIs, churn mix, tenure/charges visuals, contract vs churn, “quick insights” chips, and a downloadable filtered CSV.
* **Live predictions**: Enter customer attributes → get class & probability from the API.
* **Reproducible training**: Train script generates reports (`reports/`) and the serialized model.
* **Database-backed**: Load Telco churn CSV into Postgres and query from the app.

---

## Screenshots

* Dashboard (dark mode), filters & KPIs <img src="media/demo.gif" width="100%" alt="Demo">

---

## Getting Started

### Prerequisites

* **R** ≥ 4.3 with packages listed in `frontend/R/requirements.txt` and `backend/api/requirements.txt` (if you maintain one there).
* **PostgreSQL** ≥ 13
* **Optional**: Docker / docker-compose (a `docker-compose.yml` is included but local run works fine).

> R style in this repo uses `=` for assignment.

### Repository Layout

```
backend/
  api/
    main.R            # plumber API (serves /health, /predict)
    train-model.R     # tidymodels + xgboost training
    model.rds         # trained model artifact
  reports/            # confusion matrix, ROC, report.md

data/
  load-data.R         # create schema & load Telco CSV into Postgres
  Telco-Customer-Churn.csv

frontend/
  app.R               # Shiny app entry
  R/
    about.R           # About tab (cards)
    insights.R        # Correlation / feature-importance
    metrics.R         # KPI value boxes
    plots.R           # Plot helpers + dark theming
    prediction.R      # Prediction form + API call
    retrain.R         # (optional) hooks to retrain
    sidebar.R         # Filters module (selectize + slider)
    utils.R           # helpers

postgres/
  create_schema.sql   # optional schema helper

media/
  demo.gif
```

### Environment

Create a local `.Renviron` (or export in shell) with your DB settings:

```sh
export DB_USER="user"
export DB_PASSWORD="password"
export DB_HOST="localhost"   # or 'postgres' if running under docker-compose
export DB_PORT="5432"
export DB_NAME="churn_db"
```

> In RStudio, you can also use **Tools → Global Options → Environment** or put these in `~/.Renviron`.

### 1) Load Data

```r
# from repo root or the data/ folder
source("data/load-data.R")
# This reads Telco-Customer-Churn.csv and populates the 'customers' table.
```

### 2) Train Model

```r
# from backend/api/
setwd("backend/api")
source("train-model.R")
# Outputs:
# - backend/api/model.rds
# - backend/reports/{confusion_matrix.png, roc_curve.png, classification_report.txt, model_report.md}
```

### 3) Run the API

```r
# from backend/api/
setwd("backend/api")
library(plumber)
pr = plumb("main.R")
pr$run(host = "0.0.0.0", port = 8000)
# Swagger UI: http://127.0.0.1:8000/__docs__/
```

Sanity test:

```sh
curl -X POST "http://127.0.0.1:8000/predict" \
  -H "Content-Type: application/json" \
  -d '{
    "tenure": 15,
    "monthly_charges": 70,
    "contract": "Month-to-month",
    "gender": "Female",
    "senior_citizen": 0,
    "partner": "No",
    "dependents": "No",
    "internet_service": "Fiber optic",
    "paperless_billing": "Yes",
    "payment_method": "Electronic check",
    "services_list": ["Multiple Lines","Streaming TV"]
  }'
```

### 4) Run the Shiny App

```r
# from frontend/
setwd("frontend")
shiny::runApp("app.R", launch.browser = TRUE)
```

---

## API

**Base**: `http://127.0.0.1:8000`

### `GET /health`

* **200** → `{ "status": "ok" }`

### `POST /predict`

**Body (JSON):**

```json
{
  "tenure": 12,
  "monthly_charges": 70,
  "contract": "Month-to-month",
  "gender": "Female",
  "senior_citizen": 0,
  "partner": "Yes",
  "dependents": "No",
  "internet_service": "DSL",
  "paperless_billing": "Yes",
  "payment_method": "Electronic check",
  "services_list": ["Streaming TV", "Online Security"]
}
```

**Response (JSON):**

```json
{
  "prediction": "No",
  "probability": 0.1673
}
```

> The API auto-derives engineered fields (e.g., total/avg charges, tenure buckets, service flags) to match the training pipeline.

---

## Usage Notes & Tips

* **Filters**: Multi-select dropdowns accept empty selection = “All”. Tenure range slider updates the dataset reactively.
* **Dark theme**: Custom CSS fixes for `selectize` ensure selected chips remain visible.
* **Model threshold**: Training step chooses a probability threshold that maximizes F1 on the test split; the API uses 0.50 by default (you can expose the tuned threshold if desired).

---

## Troubleshooting

* **“Could not resolve host: backend” in Shiny prediction**
  Use the full URL `http://127.0.0.1:8000/predict` in `prediction_server(..., api_url = ...)` when running locally. Use `http://backend:8000/predict` only inside docker-compose networks.

* **Swagger “Invalid JSON body”**
  Click **Try it out** → paste a valid JSON object in the request body (don’t post empty).

* **“Could not derive probability from model prediction”**
  Ensure the trained workflow is saved as `model.rds` and predictions support `type="prob"`. This repo uses tidymodels (`.pred_1`) and includes a robust `get_positive_proba()` in `main.R`.

* **Connection refused on port 5432**
  Make sure Postgres is running and credentials/host (`localhost` vs `postgres`) match how you launched the DB.

---

## Roadmap

* Add segmented lift/ICE plots for top features.
* Expose tuned threshold from training into API response.
* Optional authentication for the API.
* Dockerized one-click stack (compose: db + API + Shiny).

---

## Contributing

PRs and issues are welcome. Keep styles consistent with the repo (e.g., R assignments with `=`). If you add a feature in the UI, please include a short GIF and update this README.

---

## License

See **`LICENSE`** in the repository root.

---

### Built With

* R, Shiny, bslib (`darkly`)
* plumber
* tidymodels (recipes, workflows, tune, yardstick), xgboost
* dplyr, ggplot2, plotly, DT
* PostgreSQL

---
