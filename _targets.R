library(targets)
source("R/utils.R", local = TRUE)

# packages à charger pour tous les targets
tar_option_set(packages = c(
  "data.table", "sf", "dplyr", "yaml",
  "mclust", "ranger", "geosphere", "exactextractr", "terra"))

list(
  tar_target(cfg, yaml::read_yaml("config.yml")),

  # 1. AIS brut → core
  tar_target(ais_raw, load_ais(cfg$ais_dir)),
  tar_target(ais_core, preprocess_ais(ais_raw, cfg$core_years)),

  # 2. jointure sédiment
  tar_target(ais_pl, enrich_pl(ais_core, cfg$sediment_file, cfg$max_dist_km)),

  # 3. subset Mumbai & fi
  tar_target(ais_mumbai, subset_bbox(ais_pl, cfg$mumbai_bbox)),
  tar_target(fi_grid,   compute_fi(ais_mumbai, cfg)),

  # 4. C₀ & Cri
  tar_target(cri_grid,  compute_cri(fi_grid, cfg)),

  # 5. Rapport
  tarchetypes::tar_render(report, "reports/pipeline_report.Rmd")
)