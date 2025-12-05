# TraitCH
Methods and plots pipelines for the TraitCH dataset. Contains:

- TraitCH raw data --> "data/raw_traits"
  
- TraitCH medata --> "traits_metadata.xlsx"
  
- Methods & plots pipelines for the TraitCH dataset:
  - Run R from the folder (select --> "open_r.bat")
  - Run R scripts to reproduce pipelines:
    - TCH1_traits_imputation.R --> Imputation pipelines (example outputs is available in --> "data/missRanger_imputed_traits")
    - TCH2_perct_missing.R --> Missing trait data information (example outputs is available in --> "data/missRanger_evaluations")
    - TCH3_plot_figures.R --> How to plot sub-figures 1 & 4
    - TCH4_plot_IUCN_eu.R --> How to plot sub-figures 2

- Extra data can be found in "bonus_materials". Mainly:
    - European species checklist (EUR) --> "EUR_checklist.txt"
    - IUCN species range area information --> "sp_range_area_km2"
