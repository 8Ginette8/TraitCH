# TraitCH

Here, we present TraitCH, a comprehensive dataset of functional traits spanning over 71,874 species (≥ 1 functional trait) across 17 major taxonomic groups: Apocrita (2,278), Arachnida (3,728), Coleoptera (8,565), Ephemeroptera/Plecoptera/Trichoptera (1,349), Lepidoptera (3,757), Odonata (234), Orthoptera (1,283), Bryobiotina (2,285), Fungi (12,469), Lichen (2,435), Mollusca (7,493), Pisces (838), Amphibia (151), Aves (1,356), Mammalia (522), Reptilia (298), and Tracheophyta (22,833). Compiled from 43 published and unpublished sources, TraitCH provides a robust representation of total species richness and composition for Switzerland and Europe.

<img width="594" height="665" alt="image" src="https://github.com/user-attachments/assets/9197b527-3bda-4919-a85d-fa44cde71bb8" />

Data & pipelines for the TraitCH dataset:
- TraitCH raw version ("outputs/raw_traits")
- TraitCH imputed version ("outputs/missRanger_imputed_traits")
- TraitCH evaluation ("outputs/missRanger_evaluations")
- TraitCH medata ("traits_metadata.xlsx")

Contains also:

- Comprehensive European species checklist (~210,000) for 17 taxonomic groups:
**(1)** Apocrita, **(2)** Arachnida (Araneae & Opiliones), **(3)** Coleoptera, **(4)** Ephemeroptera/Plecoptera/Trichoptera, **(5)** Lepidoptera, **(6)** Odonata, **(7)** Orthoptera, **(8)** Bryobiotina, **(9)** Fungi (Swiss checklist only), **(10)** Lichen (Swiss checklist only), **(11)** Mollusca (Gasteropoda & Bivalvia), **(12)** Pisces (Freshwater), **(13)** Amphibia, **(14)** Aves, **(15)** Mammalia, **(16)** Reptilia, **(17)** Tracheophyta
  
- Methods & plots pipelines for the TraitCH dataset:
  - Run R from the folder (select --> "open_r.bat" or "open_r_maclinus.sh")
  - Run R scripts to reproduce pipelines:
    - Tch0_compile_sp_traits.R               --> Main data compilation
    - Tch1_traits_imputation.R               --> Imputation pipelines
    - Tch2_format_tables.R                   --> Format data to clean version
    - Tch3_compile_IUCN.R                    --> Pipelines to create the diversity IUCN maps
    - Tch4_plot_figures.R                    --> How to plot Fig. 1 & 4
    - Tch5_plot_IUCN_eu.R                    --> How to plot Fig. 2
