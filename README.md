# Dredging‑Carbon‑Impact 🚢🌍

![targets](https://github.com/<USER>/<REPO>/actions/workflows/targets.yaml/badge.svg)
![license](https://img.shields.io/badge/license-Apache%202.0-blue)

Pipeline reproductible pour estimer l’impact des navires de dragage
sur les stocks de carbone sédimentaire et leurs émissions CO₂ associées.

## Démarrage rapide
```r
renv::restore()          # réinstalle les packages figés
targets::tar_make()      # exécute tout le pipeline
browseURL("reports/pipeline_report.html")