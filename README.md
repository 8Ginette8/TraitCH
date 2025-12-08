# TraitCH

Data & pipelines for the TraitCH dataset:
- TraitCH raw version ("outputs/raw_traits")
- TraitCH imputed version ("outputs/missRanger_imputed_traits")
- TraitCH evaluation ("outputs/missRanger_evaluations")
- TraitCH medata ("traits_metadata.xlsx")

<img width="594" height="665" alt="image" src="https://github.com/user-attachments/assets/9197b527-3bda-4919-a85d-fa44cde71bb8" />

Contains also:

- Comprehensive European species checklist for 17 taxonomic groups:
  - Apocrita
  - Arachnida (Araneae & Opiliones)
  - Coleoptera
  - Ephemeroptera/Plecoptera/Trichoptera
  - Lepidoptera
  - Odonata
  - Orthoptera
  - Bryobiotina
  - Fungi (Swiss checklist only)
  - Lichen (Swiss checklist only)
  - Mollusca (Gasteropoda & Bivalvia)
  - Pisces (Freshwater)
  - Amphibia
  - Aves
  - Mammalia
  - Reptilia
  - Tracheophyta
  
- Methods & plots pipelines for the TraitCH dataset:
  - Run R from the folder (select --> "open_r.bat" or "open_r_maclinus.sh")
  - Run R scripts to reproduce pipelines:
    - Tch0_compile_sp_traits.R               --> Main data compilation
    - Tch1_traits_imputation.R               --> Imputation pipelines
    - Tch2_format_tables.R                   --> Format data to clean version
    - Tch3_compile_IUCN.R                    --> Pipelines to create the diversity IUCN maps
    - Tch4_plot_figures.R                    --> How to plot Fig. 1 & 4
    - Tch5_plot_IUCN_eu.R                    --> How to plot Fig. 2
