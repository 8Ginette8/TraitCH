# ####################################################################
# TraitCH: Compile species traits
#
# $Date: 2023-10-09
#
# Author: Yohann Chauvier, yohann.chauvier@wsl.ch
# Evolutionary Genetics Group
# Swiss Federal Research Institute EAWAG
# 
# Description: Let's compile traits from known databases of mammals,
# birds etc. Due to copyright reasons and data restriction, this
# script cannot be provided with the main the input trait data and
# therefore cannot run. See output folder'outputs/raw_traits' for
# compilation results.
#
# ####################################################################

### ==================================================================
### Initialise system
### ==================================================================


# Cleaning
rm(list = ls()); graphics.off()

# max.print
options(max.print=500)

# Functions
scr = list.files("R/functions",full.names=TRUE)
invisible(lapply(scr, source))

# Library
library(readxl)
library(gbif.range)
library(stringr)
library(raster)

# Open the European checklist
s2z.sp = read.table("data/EUR_checklist.txt",header=TRUE)


### ==================================================================
### Trait sources that cover multiple taxa
### ==================================================================


# # Open the vertlife tetrapod database and format a bit
# vertlife = read.csv("./tetrapod_traits_DatabaseCSVFiles/TetrapodTraits_1.0.0.csv")
# vertlife$Scientific.Name = gsub("_"," ",vertlife$Scientific.Name)
# vertlife[vertlife$Diu>0.3,"Diu"] = 1
# vertlife[vertlife$Diu<0.3,"Diu"] = 0
# vertlife[vertlife$Noc>0.3,"Noc"] = 1
# vertlife[vertlife$Noc<0.3,"Noc"] = 0
# vertlife[vertlife$Fos>0.3,"Fos"] = 1
# vertlife[vertlife$Fos<0.3,"Fos"] = 0
# vertlife[vertlife$Ter>0.3,"Ter"] = 1
# vertlife[vertlife$Ter<0.3,"Ter"] = 0
# vertlife[vertlife$Aqu>0.3,"Aqu"] = 1
# vertlife[vertlife$Aqu<0.3,"Aqu"] = 0
# vertlife[vertlife$Arb>0.3,"Arb"] = 1
# vertlife[vertlife$Arb<0.3,"Arb"] = 0
# vertlife[vertlife$Aer>0.3,"Aer"] = 1
# vertlife[vertlife$Aer<0.3,"Aer"] = 0

# # Remove imputed values from Vertlife to dp a clean imputation after
# vertlife[vertlife$ImputedLength==1,"BodyLength_mm"] = NA
# vertlife[vertlife$ImputedMass==1,"BodyMass_g"] = NA
# vertlife[vertlife$ImputedActTime==1,"Nocturnality"] = NA
# vertlife[vertlife$ImputedHabitat==1,c("Fos","Ter","Aqu","Arb","Aer")] = NA
# vertlife[vertlife$ImputedMajorHabitat%in%1,names(vertlife)[grepl("Major_",names(vertlife))]] = NA

# # Add information
# vertlife$Activity = NA
# vertlife[vertlife$Nocturnality%in%1,"Activity"] = "Nocturnal"
# vertlife[vertlife$Nocturnality%in%0,"Activity"] = "Diurnal"
# vertlife[vertlife$Nocturnality%in%0.5,"Activity"] = "Cathemeral"
# vert.traits = c("Scientific.Name","Genus","Family","Order","BodyLength_mm","BodyMass_g","Activity","RangeSize_km2",
# 	"Fos","Ter","Aqu","Arb","Aer","MajorHabitatSum",names(vertlife)[grepl("Prop",names(vertlife))],
# 	names(vertlife)[grepl("MajorHabitat_",names(vertlife))],"Elevation","AnnuMeanTemp","AnnuPrecip","TempSeasonality",
# 	"PrecipSeasonality","Verticality","EcoTer","EcoFresh","EcoMar","AssessedStatus")
# vertlife[vertlife$AssessedStatus%in%"CR","AssessedStatus"] = "CRITICALLY_ENDANGERED"
# vertlife[vertlife$AssessedStatus%in%"DD","AssessedStatus"] = "DATA_DEFICIENT"
# vertlife[vertlife$AssessedStatus%in%"LC","AssessedStatus"] = "LEAST_CONCERN"
# vertlife[vertlife$AssessedStatus%in%"VU","AssessedStatus"] = "VULNERABLE"
# vertlife[vertlife$AssessedStatus%in%"EN","AssessedStatus"] = "ENDANGERED"
# vertlife[vertlife$AssessedStatus%in%"EW","AssessedStatus"] = "EXTINCT_IN_THE_WILD"
# vertlife[vertlife$AssessedStatus%in%"NT","AssessedStatus"] = "NEAR_THREATENED"
# vertlife[vertlife$AssessedStatus%in%"NE","AssessedStatus"] = "NOT_EVALUATED"
# vertlife[vertlife$AssessedStatus%in%"EX","AssessedStatus"] = "EXTINCT"
# vertlife$RangeSize_km2 = vertlife$RangeSize*110^2 # Each count = 110 x 110 km pixel!!!!
# vertlife$AnnuMeanTemp = vertlife$AnnuMeanTemp/10 # Convert to proper °C

# # Common columns for tetrapods
# t.tetra = c("species","Genus","Family","Order","BodyMass_g","BodyLength_mm","Litter_size","Reproductive_mode",
# 	"Age_at_maturity_y","First_breeding_y","Longevity_max_y","Activity","Diet","Diet_Inv","Diet_Vert","Diet_Scav",
# 	"Diet_Fruit","Diet_Nect","Diet_Seed","Diet_Plant","Fos","Ter","Aqu","Arb","Aer","Verticality","EcoTer",
# 	"EcoFresh","EcoMar",names(vertlife)[grepl("MajorHabitat_",names(vertlife))],"MajorHabitatSum",
# 	"HabitatBreadth",names(vertlife)[grepl("Prop",names(vertlife))],"AnnuMeanTemp","AnnuPrecip",
# 	"TempSeasonality","PrecipSeasonality","Elevation","Dispersal_km","RangeSize_km2","IUCN_status")

# # Open the harmonized arthropods database from Gossner
# goss.arthrop = read.table("./insects/Gossner_arthropods/ArthropodSpeciesTraits.txt",sep="\t",header=TRUE)
# names(goss.arthrop)[4] = "species"
# goss.arthrop$Feeding_plant_part = gsub("ro","ROOTS",goss.arthrop$Feeding_plant_part)
# goss.arthrop$Feeding_plant_part = gsub("se","SEEDS",goss.arthrop$Feeding_plant_part)
# 	#
# goss.arthrop$Diet = goss.arthrop$Feeding_guild_short
# goss.arthrop[goss.arthrop$Diet%in%"c","Diet"] = "Carnivorous"
# goss.arthrop[goss.arthrop$Diet%in%"d","Diet"] = "Scavenger"
# goss.arthrop[goss.arthrop$Diet%in%"f","Diet"] = "Fungivorous"
# goss.arthrop[goss.arthrop$Diet%in%"h","Diet"] = "Herbivorous"
# goss.arthrop[goss.arthrop$Diet%in%"o","Diet"] = "Omnivorous"
# 	#
# goss.arthrop[goss.arthrop$Feeding_mode%in%"c","Feeding_mode"] = "Chewer"
# goss.arthrop[goss.arthrop$Feeding_mode%in%"e","Feeding_mode"] = "Extraintestinal"
# goss.arthrop[goss.arthrop$Feeding_mode%in%"s","Feeding_mode"] = "Sucker"
# 	#
# goss.arthrop[,c("Diet_Carniv","Diet_Scav","Diet_Fungi","Diet_Root","Diet_Seed","Diet_Leaf","Diet_Flower","Diet_Stem")] = 0
# goss.arthrop[grepl("c",goss.arthrop$Feeding_guild),"Diet_Carniv"] = 1
# goss.arthrop[grepl("d",goss.arthrop$Feeding_guild),"Diet_Scav"] = 1
# goss.arthrop[grepl("f",goss.arthrop$Feeding_guild),"Diet_Fungi"] = 1
# goss.arthrop[grepl("h",goss.arthrop$Feeding_guild)&grepl("ROOTS",goss.arthrop$Feeding_plant_part),"Diet_Root"] = 1
# goss.arthrop[grepl("h",goss.arthrop$Feeding_guild)&grepl("SEEDS",goss.arthrop$Feeding_plant_part),"Diet_Seed"] = 1
# goss.arthrop[grepl("h",goss.arthrop$Feeding_guild)&grepl("l",goss.arthrop$Feeding_plant_part),"Diet_Leaf"] = 1
# goss.arthrop[grepl("h",goss.arthrop$Feeding_guild)&grepl("r",goss.arthrop$Feeding_plant_part),"Diet_Flower"] = 1
# goss.arthrop[grepl("h",goss.arthrop$Feeding_guild)&grepl("s",goss.arthrop$Feeding_plant_part),"Diet_Stem"] = 1
# 	#
# goss.arthrop[,c("Fos","Ter","Aqu","Arb","Aer")] = NA
# goss.arthrop[grepl("s",goss.arthrop$Stratum_use),"Fos"] = 1
# goss.arthrop[grepl("g|h",goss.arthrop$Stratum_use),"Ter"] = 1
# goss.arthrop[grepl("w",goss.arthrop$Stratum_use),"Aqu"] = 1
# goss.arthrop[grepl("t",goss.arthrop$Stratum_use),"Arb"] = 1
# goss.arthrop[,c("Fos","Ter","Aqu","Arb")][is.na(goss.arthrop[,c("Fos","Ter","Aqu","Arb")])] = 0
# 	#
# goss.arthrop[,c("Suborder","Author","Feeding_guild","Feeding_guild_short","Feeding_mode","Feeding_specialization",
# 	"Feeding_tissue","Feeding_plant_part","Endophagous_lifestyle","Stratum_use","Stratum_use_short","Remark")] = NULL

# Open species range size (IUCN only!!!)
iucn.files = list.files("data/sp_range_area_km2/")
iucn.ranges = lapply(iucn.files,function(x)
	read.table(paste0("data/sp_range_area_km2/",x),sep="\t",header=TRUE))
names(iucn.ranges) = gsub("_range_area_km2.txt","",iucn.files)

# # Set reference columns
GB.col = c("gbif_accepted","gbif_rank","gbif_genus","gbif_family","gbif_order","gbif_class","gbif_phylum","gbif_IUCN")
new.col = c("gbif_accepted","gbif_rank","Genus","Family","Order","Class","Phylum")

# Open infospecies file on Terrestrial and aquatic
antoine.TerAqu = read.csv("data/infospecies_TerAqu.csv")
antoine.TerAqu$species = to_GenusSpecies(antoine.TerAqu$species)[,2]

# # Format Logghe et al. file
# logghe.list = list.files("./insects/NorthWesternEuropeanArthropods/",full.names=TRUE)
# logghe.list = logghe.list[!grepl("txt|pdf",logghe.list)]
# logghe.list = logghe.list[!grepl("Diptera|Hemiptera|Isopoda",logghe.list)]
# cat.col = c("Wing_development","Partivoltine","Univoltine","Bivoltine","Multivoltine","Overwintering_egg",
# 	"Overwintering_larva","Overwintering_pupa","Overwintering_adult","Diurnality","Trophic_level_larva",
# 	"Trophic_level_adult","Trophic_range_larva","Trophic_range_adult","Hunting_strategy","Habitat_urban",
# 	"Habitat_green_urban","Habitat_agricultural","Habitat_forest_deciduous","Habitat_forest_coniferous",
# 	"Habitat_forest_mixed","Habitat_grassland","Habitat_heathland","Habitat_forest_transitional","Habitat_dunes",
# 	"Habitat_marshes_fresh","Habitat_marshes_salt")
# logghe.new =
# lapply(logghe.list,function(x){
# 	# Open
# 	logghe.dat = as.data.frame(read_excel(x,1))
# 	logghe.dat$Species = gsub("_"," ",logghe.dat$Species)
# 	# Convert to class & aggregate data by species
# 	logghe.dat[,cat.col] = sapply(logghe.dat[,cat.col],as.factor)
# 	logghe.dat = combine_rows(logghe.dat,"Species")
# 	# Format
# 	logghe.dat[is.na(logghe.dat$Size_male_mean),"Size_male_mean"] = ((logghe.dat$Size_male_min+logghe.dat$Size_male_max)/2)[is.na(logghe.dat$Size_male_mean)]
# 	logghe.dat[is.na(logghe.dat$Size_female_mean),"Size_female_mean"] = ((logghe.dat$Size_female_min+logghe.dat$Size_female_max)/2)[is.na(logghe.dat$Size_female_mean)]
# 	logghe.dat[is.na(logghe.dat$Size_min),"Size_min"] = pmin(logghe.dat$Size_male_min,logghe.dat$Size_female_min)[is.na(logghe.dat$Size_min)]
# 	logghe.dat[is.na(logghe.dat$Size_max),"Size_max"] = pmax(logghe.dat$Size_male_max,logghe.dat$Size_female_max)[is.na(logghe.dat$Size_max)]
# 	logghe.dat[is.na(logghe.dat$Size_mean),"Size_mean"] = ((logghe.dat$Size_male_mean+logghe.dat$Size_female_mean)/2)[is.na(logghe.dat$Size_mean)]
# 	logghe.dat[is.na(logghe.dat$Size_mean),"Size_mean"] = ((logghe.dat$Size_min+logghe.dat$Size_max)/2)[is.na(logghe.dat$Size_mean)]
# 	logghe.dat[is.na(logghe.dat$Fecundity_mean),"Fecundity_mean"] = ((logghe.dat$Fecundity_min+logghe.dat$Fecundity_max)/2)[is.na(logghe.dat$Fecundity_mean)]
# 	logghe.dat[is.na(logghe.dat$Development_egg_mean),"Development_egg_mean"] = ((logghe.dat$Development_egg_min+logghe.dat$Development_egg_max)/2)[is.na(logghe.dat$Development_egg_mean)]
# 	logghe.dat[is.na(logghe.dat$Development_larva_mean),"Development_larva_mean"] = ((logghe.dat$Development_larva_min+logghe.dat$Development_larva_max)/2)[is.na(logghe.dat$Development_larva_mean)]
# 	logghe.dat[is.na(logghe.dat$Development_pupa_mean),"Development_pupa_mean"] = ((logghe.dat$Development_pupa_min+logghe.dat$Development_pupa_max)/2)[is.na(logghe.dat$Development_pupa_mean)]
# 	logghe.dat$Longevity = as.numeric(logghe.dat$Longevity) /365
# 	logghe.dat$Habitat_wetlands = logghe.dat$Habitat_marshes_fresh
# 	logghe.dat[logghe.dat$Habitat_marshes_salt%in%1,"Habitat_wetlands"] = 1
# 	logghe.dat$Habitat_forests = logghe.dat$Habitat_forest_deciduous
# 	logghe.dat[logghe.dat$Habitat_forest_coniferous%in%1,"Habitat_forests"] = 1
# 	logghe.dat[logghe.dat$Habitat_forest_mixed%in%1,"Habitat_forests"] = 1
# 	logghe.dat[logghe.dat$Wing_development%in%1,"Wing_development"] = "aptery/brachyptery"
# 	logghe.dat[logghe.dat$Wing_development%in%2,"Wing_development"] = "dimorphy"
# 	logghe.dat[logghe.dat$Wing_development%in%3,"Wing_development"] = "macroptery"
# 	logghe.dat[logghe.dat$Diurnality%in%1,"Diurnality"] = "Diurnal"
# 	logghe.dat[logghe.dat$Diurnality%in%2,"Diurnality"] = "Cathemeral"
# 	logghe.dat[logghe.dat$Diurnality%in%3,"Diurnality"] = "Nocturnal"
# 	logghe.dat[logghe.dat$Trophic_level_larva%in%1,"Trophic_level_larva"] = "Herbivorous"
# 	logghe.dat[logghe.dat$Trophic_level_larva%in%2,"Trophic_level_larva"] = "Palynivorous"
# 	logghe.dat[logghe.dat$Trophic_level_larva%in%3,"Trophic_level_larva"] = "Omnivorous"
# 	logghe.dat[logghe.dat$Trophic_level_larva%in%4,"Trophic_level_larva"] = "Fungivorous"
# 	logghe.dat[logghe.dat$Trophic_level_larva%in%5,"Trophic_level_larva"] = "Carnivorous"
# 	logghe.dat[logghe.dat$Trophic_level_larva%in%6,"Trophic_level_larva"] = "Scavenger"
# 	logghe.dat[logghe.dat$Trophic_level_adult%in%1,"Trophic_level_adult"] = "Herbivorous"
# 	logghe.dat[logghe.dat$Trophic_level_adult%in%2,"Trophic_level_adult"] = "Palynivorous"
# 	logghe.dat[logghe.dat$Trophic_level_adult%in%3,"Trophic_level_adult"] = "Omnivorous"
# 	logghe.dat[logghe.dat$Trophic_level_adult%in%4,"Trophic_level_adult"] = "Fungivorous"
# 	logghe.dat[logghe.dat$Trophic_level_adult%in%5,"Trophic_level_adult"] = "Carnivorous"
# 	logghe.dat[logghe.dat$Trophic_level_adult%in%6,"Trophic_level_adult"] = "Scavenger"
# 	logghe.dat[logghe.dat$Trophic_range_larva%in%1,"Trophic_range_larva"] = "monophagous"
# 	logghe.dat[logghe.dat$Trophic_range_larva%in%2,"Trophic_range_larva"] = "oligophagous"
# 	logghe.dat[logghe.dat$Trophic_range_larva%in%3,"Trophic_range_larva"] = "polyphagous"
# 	logghe.dat[logghe.dat$Trophic_range_adult%in%1,"Trophic_range_adult"] = "monophagous"
# 	logghe.dat[logghe.dat$Trophic_range_adult%in%2,"Trophic_range_adult"] = "oligophagous"
# 	logghe.dat[logghe.dat$Trophic_range_adult%in%3,"Trophic_range_adult"] = "polyphagous"
# 	logghe.dat[!logghe.dat$Trophic_range_adult%in%c(1:3,"oligophagous","polyphagous","monophagous"),"Trophic_range_adult"] = NA
# 	logghe.dat[!logghe.dat$Trophic_range_adult%in%c(1:3,"oligophagous","polyphagous","monophagous"),"Trophic_range_larva"] = NA
# 	# Name
# 	names(logghe.dat)[c(9:11,12,15,18,19,22,23,26,29,32,33,40:45,50:52,55:56,59,62)] = c("BodyLength_mm","BodyLength_min",
# 		"BodyLength_max","BodyLength_male","BodyLength_female","Wing_morphology","Litter_size","Development_total_d",
# 		"Development_egg_mean_d","Development_larva_mean_d","Development_pupa_mean_d","Longevity_y","Semivoltine",
# 		"Overwintering_imago","Activity","Larva_Diet","Imago_Diet","Larva_Diet_range","Imago_Diet_range","Habitat_deciduous",
# 		"Habitat_coniferous","Habitat_mixed","Habitat_transitional","Habitat_sand","Dispersal_ability","Dispersal_km")
# 	logghe.dat$Dispersal_km = logghe.dat$Dispersal_km/1000
# 	# Create additional commmon charact
# 	logghe.dat$Habitat_water = NA
# 	logghe.dat$Habitat_rocks = NA
# 	logghe.dat$Habitat_bushes = NA
# 	logghe.dat$Habitat_pioneer = NA
# 	logghe.dat$Diet_Carniv = NA
# 	logghe.dat$Diet_Scav = NA
# 	logghe.dat$Diet_Fungi = NA
# 	logghe.dat$Diet_Root = NA
# 	logghe.dat$Diet_Seed = NA
# 	logghe.dat$Diet_Leaf = NA
# 	logghe.dat$Diet_Flower = NA
# 	logghe.dat$Diet_Stem = NA
# 	logghe.dat$Diet_Grass = NA
# 	logghe.dat$Diet_Woody = NA
# 	logghe.dat$Diet_Mineral = NA
# 	logghe.dat$Diet_Parasit = NA
# 	logghe.dat$Elev_Planar = NA
# 	logghe.dat$Elev_Colline = NA
# 	logghe.dat$Elev_Montane = NA
# 	logghe.dat$Elev_Subalpine = NA
# 	logghe.dat$Elev_Alpine = NA
# 	logghe.dat$EIV_Light = NA
# 	logghe.dat$EIV_Temp = NA
# 	logghe.dat$EIV_Moisture= NA
# 	# Normalize Dispersal ability
# 	logghe.dat$Dispersal_ability = normalize(logghe.dat$Dispersal_ability)
# 	# Remove columns & return
# 	col.rm = c("Source","Kingdom","Suborder","Size_male_min","Size_female_min","Size_male_max",
# 		"Size_female_max","Fecundity_min","Fecundity_max","Development_egg_min","Development_egg_max",
# 		"Development_larva_min","Development_larva_max","Development_pupa_min","Development_pupa_max",
# 		"Habitat_salt_marshes","Habitat_fresh_marshes","Mobility_wing_load","Mobility_aspect_ratio",
# 		"Mobility_distance","Hunting_strategy")
# 	logghe.dat = logghe.dat[,!names(logghe.dat)%in%col.rm]
# 	return(logghe.dat)
# })
# names(logghe.new) = logghe.list

# # Combine the two lepidoptera files
# logghe.lepi = do.call("rbind",logghe.new[grepl("Lepidoptera",logghe.list)])
# row.names(logghe.lepi) = NULL
# logghe.new = logghe.new[!grepl("Lepidoptera",logghe.new)]
# logghe.new[[6]] = logghe.lepi
# names(logghe.new)[6] = "./insects/NorthWesternEuropeanArthropods/Lepidoptera.xlsx"

# # Choose "logghe" common characteristics + rename
HABit = c("Habitat_water","Habitat_wetlands","Habitat_rocks","Habitat_grassland","Habitat_forests","Habitat_deciduous",
 	"Habitat_coniferous","Habitat_mixed","Habitat_pioneer","Habitat_urban","Habitat_green_urban","Habitat_agricultural",
 	"Habitat_bushes","Habitat_transitional","Habitat_heathland","Habitat_sand")
# logghe.common = c("species","Genus","Family","Order","BodyLength_mm","BodyLength_male","BodyLength_female","BodyLength_min","BodyLength_max",
# 	"Litter_size","Development_total_d","Development_egg_mean_d","Development_larva_mean_d","Development_pupa_mean_d",
# 	"Longevity_y","Semivoltine","Univoltine","Bivoltine","Multivoltine","Overwintering_egg","Overwintering_larva",
# 	"Overwintering_pupa","Overwintering_imago","Activity","Larva_Diet","Imago_Diet","Larva_Diet_range",
# 	"Imago_Diet_range","Diet_Carniv","Diet_Scav","Diet_Fungi","Diet_Root","Diet_Seed","Diet_Leaf","Diet_Flower",
# 	"Diet_Stem","Diet_Grass","Diet_Woody","Fos","Ter","Aqu","Arb","Aer",HABit,
# 	"EIV_Light","EIV_Temp","EIV_Moisture","Elev_Planar","Elev_Colline","Elev_Montane","Elev_Subalpine","Elev_Alpine",
# 	"Dispersal_ability","Dispersal_km","RangeSize_km2","IUCN_status")


### ================================================================
### Mammals
### ================================================================


# # Specifically select Mammals in the s2z and vertlife taxa
# vert.mammals = vertlife[vertlife$Class%in%"Mammalia",]

# # Open the best sources
# mammals.cox = data.frame(read_excel("./mammals_n_birds/Cox_2021_2023/Supplementary Data 2.xlsx"))
# mammals.bmr = data.frame(read_excel("./mammals_n_birds/Genoud_2017/brv12350-sup-0003-tables2.xlsx"))
# mammals.bmr[,c("BMR","BMR.","body.mass")] = apply(mammals.bmr[,c("BMR","BMR.","body.mass")],2,as.numeric)
# mammals.bmr = mammals.bmr[-724,]
# mammals.bmr$Species = paste(mammals.bmr$Species.W.R,"subsp.",mammals.bmr$Subsp.W.R)
# mammals.bmr$Species = gsub(" subsp. NA","",mammals.bmr$Species)
# mammals.soria = read.csv("./mammals_n_birds/Soria_2020/COMBINE_archives/trait_data_reported.csv")
# mammals.soria[all_duplicated(mammals.soria$iucn2020_binomial),"iucn2020_binomial"] =
# 	mammals.soria[all_duplicated(mammals.soria$iucn2020_binomial),"phylacine_binomial"]

# # Convert diet to a one category column diet, and convert to binary
# # (>=70% --->  https://doi.org/10.1111/ele.13898, https://doi.org/10.1038/s41559-019-1070-4)
# q.carnivore = sapply(1:nrow(mammals.cox),function(x) {sum(mammals.cox[x,19:23])>=70})
# q.herbivore = sapply(1:nrow(mammals.cox),function(x) {sum(mammals.cox[x,25:28])>=70})
# q.scavenger = mammals.cox$Diet.Scav>=70
# q.omnivore = q.carnivore + q.herbivore + q.scavenger
# mammals.cox$Diet = NA
# mammals.cox$Diet[q.carnivore] = "Carnivorous"
# mammals.cox$Diet[q.herbivore] = "Herbivorous"
# mammals.cox$Diet[q.omnivore==0] = "Omnivorous"
# mammals.cox$Diet[q.scavenger] = "Scavenger"
# mammals.cox[,19:28][mammals.cox[,19:28]!=0] = 1

# # Format
# names(mammals.cox)[20] = "Diet.Vert"
# mammals.cox$Diet.Vert[mammals.cox$Diet.Vect%in%1] = 1
# mammals.cox$Diet.Vert[mammals.cox$Diet.Vfish%in%1] = 1
# mammals.cox$Diet.Vert[mammals.cox$Diet.Vunk%in%1] = 1
# mammals.cox[,c("Diet.Vect","Diet.Vfish","Diet.Vunk")] = NULL

# # Average BMR data
# sp.bmr = unique(mammals.bmr$Species)
# new.bmr = lapply(sp.bmr,function(x){
# 	bmr.l = mammals.bmr[mammals.bmr$Species%in%x,c("Species","BMR")]
# 	return(data.frame(Scientific=bmr.l[1,1],BMR=mean(bmr.l$BMR)))
# })

# # Merge to the mammals main dataset and keep only interesting columns
# mammals.traits = merge(mammals.cox,do.call("rbind",new.bmr),by.x="Binomial_iucn",by.y="Scientific",all=TRUE)
# cox.merge = c("Binomial_iucn","Order","Family","Genus","Activity_DD","Body_mass_DD","Litter_size_DD",
# 	"Diet","Diet.Inv","Diet.Vert","Diet.Scav","Diet.Fruit","Diet.Nect","Diet.Seed","Diet.Plant",
# 	"Foraging.strata_DD","Habitat_.breadth_DD","BMR")

# # Merge with the Soria dataset and keep infos on longevity and maturity
# mammals.traits2 = merge(mammals.traits,mammals.soria,by.x="Binomial_iucn",by.y="iucn2020_binomial",all=TRUE)
# i1 = is.na(mammals.traits2$Body_mass_DD)
# i2 = is.na(mammals.traits2$Litter_size_DD)
# i3 = is.na(mammals.traits2$Order)
# mammals.traits2[i1,"Body_mass_DD"] = mammals.traits2[i1,"adult_mass_g"]
# mammals.traits2[i2,"Litter_size_DD"] = mammals.traits2[i2,"litter_size_n"]
# mammals.traits2[i3,"Order"] = mammals.traits2[i3,"order"]
# mammals.traits2[i3,"Family"] = mammals.traits2[i3,"family"]
# mammals.traits2[i3,"Genus"] = mammals.traits2[i3,"genus"]
# soria.merge = c(cox.merge,"adult_body_length_mm","max_longevity_d","female_maturity_d",
# 	"age_first_reproduction_d","dispersal_km")
# # Female_maturity_d because here way more infos (amphibians they don't make the distinction)

# # Match potential species with the vertlife database
# mammals.vert = merge(mammals.traits2[,soria.merge],vert.mammals[,vert.traits],
# 	by.x="Binomial_iucn",by.y="Scientific.Name",all=TRUE)
# i1 = is.na(mammals.vert$Activity_DD)
# i2 = is.na(mammals.vert$Body_mass_DD)
# i3 = is.na(mammals.vert$adult_body_length_mm)
# i4 = is.na(mammals.vert$Order.x)
# mammals.vert[i1,"Activity_DD"] = mammals.vert[i1,"Activity"]
# mammals.vert[i2,"Body_mass_DD"] = mammals.vert[i2,"BodyMass_g"]
# mammals.vert[i3,"adult_body_length_mm"] = mammals.vert[i3,"BodyLength_mm"]
# mammals.vert[i4,"Order.x"] = mammals.vert[i4,"Order.y"]
# mammals.vert[i4,"Family.x"] = mammals.vert[i4,"Family.y"]
# mammals.vert[i4,"Genus.x"] = mammals.vert[i4,"Genus.y"]
# mammals.vert[,c("adult_body_length_mm","BodyMass_g","Order.y","Family.y","Genus.y","Activity")] = NULL

# # Convert days to year for longevity and maturity (according to amphibians)
# mammals.vert$max_longevity_d = mammals.vert$max_longevity_d/365
# mammals.vert$female_maturity_d = mammals.vert$female_maturity_d/365
# mammals.vert$age_first_reproduction_d = mammals.vert$age_first_reproduction_d/365

# # New order for names
# names(mammals.vert)[c(1:7,17,19:22)] = c("species","Order","Family","Genus","Activity","BodyMass_g",
# 	"Litter_size","HabitatBreadth","Longevity_max_y","Age_at_maturity_y","First_breeding_y","Dispersal_km")
# names(mammals.vert)[ncol(mammals.vert)] = "IUCN.status"
# names(mammals.vert) = gsub("\\.|_\\.","_",names(mammals.vert))
# names(mammals.vert) = gsub("_DD","",names(mammals.vert))
# not.n = names(mammals.vert)[!(names(mammals.vert)%in%t.tetra)]

# # Change names if necessary
# tGS = to_GenusSpecies(mammals.vert$species)
# mammals.vert$species = tGS[,"CanonicalName"]
# mammals.vert = mammals.vert[!is.na(mammals.vert$species),]
# #mammals.vert = combine_rows(mammals.vert,"species") # Unique names (no need)

# # Add reproduction mode (Viviparous excpet Ornithorynque and four other Echnidé species)
# mammals.vert$Reproductive_mode = "Viviparous"
# mammals.vert[mammals.vert$species%in%"Ornithorhynchus anatinus","Reproductive_mode"] = "Oviparous"
# mammals.vert[mammals.vert$species%in%"Tachyglossus aculeatus","Reproductive_mode"] = "Oviparous"
# mammals.vert[mammals.vert$species%in%"Zaglossus bruijni","Reproductive_mode"] = "Oviparous"
# mammals.vert[mammals.vert$species%in%"Zaglossus attenboroughi","Reproductive_mode"] = "Oviparous"
# mammals.vert[mammals.vert$species%in%"Zaglossus bartoni","Reproductive_mode"] = "Oviparous"

# # Data.from order
# mammals.vert = mammals.vert[,c(t.tetra,not.n)]
# mammals.vert[mammals.vert==""] = NA

# # Convert to numeric
# non.num = c("species","Activity","Diet","IUCN_status","Reproductive_mode","Order","Family","Genus")
# mammals.vert[,!names(mammals.vert)%in%non.num] = sapply(mammals.vert[,!names(mammals.vert)%in%non.num],as.numeric)

# # Add gbif information
# mammals.vert[,GB.col] = NA
# for (i in 1:nrow(mammals.vert)){
#     cat("...sp.",i)
#     gbif.search = get_status(mammals.vert[i,"species"],phylum="Chordata") # Search for accepted names
#     if (!is.null(gbif.search)){
#     	mammals.vert$gbif_accepted[i] = gbif.search$scientificName[1] # Fill in the result
#  		mammals.vert$gbif_rank[i] = gbif.search$rank[1]
#         mammals.vert$gbif_phylum[i] = gbif.search$Phylum[1]
#         mammals.vert$gbif_genus[i] = gbif.search$Genus[1]
#         mammals.vert$gbif_order[i] = gbif.search$Order[1]
#         mammals.vert$gbif_family[i] = gbif.search$Family[1]
#         mammals.vert$gbif_class[i] = gbif.search$Class[1]
#         mammals.vert$gbif_IUCN[i] = gbif.search$IUCN_status[1]
#    	}
# }
# write.table(mammals.vert,"temp_mammals.txt",row.names=FALSE)
mammals.vert = read.table("data/temp/temp_mammals.txt",header=TRUE)

# Format our table
which.gbif = which(names(mammals.vert)%in%GB.col)
mammals.vert = mammals.vert[,c(1,which.gbif,2:(ncol(mammals.vert)-length(which.gbif)))]
mammals.vert[is.na(mammals.vert$gbif_genus),"gbif_genus"] = mammals.vert[is.na(mammals.vert$gbif_genus),"Genus"]
mammals.vert$gbif_genus[is.na(mammals.vert$gbif_genus)] =
	sapply(strsplit(mammals.vert$species[is.na(mammals.vert$gbif_genus)]," "),function(x) x[1])
mammals.vert[is.na(mammals.vert$gbif_family),"gbif_family"] = mammals.vert[is.na(mammals.vert$gbif_family),"Family"]
mammals.vert[is.na(mammals.vert$gbif_order),"gbif_order"] = mammals.vert[is.na(mammals.vert$gbif_order),"Order"]
mammals.vert$gbif_class = "Mammalia"
mammals.vert$gbif_phylum = "Chordata"
mammals.vert[is.na(mammals.vert$IUCN_status),"IUCN_status"] = mammals.vert[is.na(mammals.vert$IUCN_status),"gbif_IUCN"]
mammals.vert[,c("Genus","Family","Order","gbif_IUCN")] = NULL
names(mammals.vert)[2:length(GB.col)] = new.col
gbif.cond = mammals.vert$IUCN_status%in%c("NOT_FOUND","NOT_EVALUATED")
mammals.vert[gbif.cond,"IUCN_status"] = NA

# Extract EU checklist and merge
s2z.mammals = s2z.sp[s2z.sp$group%in%"Mammals",]
mammals.merge1 = merge(s2z.mammals,mammals.vert,by="species")
mammals.vert = mammals.vert[!mammals.vert$species%in%mammals.merge1$species,]
s2z.mammals2 = s2z.mammals[!s2z.mammals$species%in%mammals.merge1$species,]

# Regroup all the available information for the accepted species (mean when continuous values)
mam.ndup = mammals.vert[!(duplicated(mammals.vert$gbif_accepted)|duplicated(mammals.vert$gbif_accepted,fromLast=TRUE)),]
mam.dup = mammals.vert[duplicated(mammals.vert$gbif_accepted)|duplicated(mammals.vert$gbif_accepted,fromLast=TRUE),]
mam.homo = combine_rows(df=mam.dup,ref.col="gbif_accepted",binary=FALSE)
mam.homo = mam.homo[!is.na(mam.homo$gbif_accepted),]

# Combine, attempt another merge and rbind both results
mam.temp = rbind(mam.ndup,mam.homo)
mammals.merge2 = merge(s2z.mammals2,mam.temp,by="gbif_accepted",all.x=TRUE)
names(mammals.merge2)[1:2] = c("gbif_accepted.x","species")
mammals.merge2$gbif_accepted.y = mammals.merge2$gbif_accepted.x
mammals.merge2 = mammals.merge2[,names(mammals.merge1)]
mammals.MM = rbind(mammals.merge1,mammals.merge2)

# Try to fill in mising information fron one column to another
mammals.MM[is.na(mammals.MM$genus),"genus"] = mammals.MM[is.na(mammals.MM$genus),"Genus"]
mammals.MM[is.na(mammals.MM$family),"family"] = mammals.MM[is.na(mammals.MM$family),"Family"]
mammals.MM[is.na(mammals.MM$order),"order"] = mammals.MM[is.na(mammals.MM$order),"Order"]
mammals.MM[is.na(mammals.MM$class),"class"] = mammals.MM[is.na(mammals.MM$class),"Class"]
mammals.MM[is.na(mammals.MM$phylum),"phylum"] = mammals.MM[is.na(mammals.MM$phylum),"Phylum"]
mammals.MM[grepl("unresolved_accepted",mammals.MM$gbif_accepted.x),"gbif_accepted.x"] =
	mammals.MM[grepl("unresolved_accepted",mammals.MM$gbif_accepted.x),"gbif_accepted.y"]
mammals.MM$gbif_accepted.x[is.na(mammals.MM$gbif_accepted.x)] =
	paste0("unresolved_accepted2_", seq_len(sum(is.na(mammals.MM$gbif_accepted.x))))
mammals.MM[is.na(mammals.MM$taxonRank),"taxonRank"] = mammals.MM[is.na(mammals.MM$taxonRank),"gbif_rank"]
mammals.MM[is.na(mammals.MM$iucnRedListCategory),"iucnRedListCategory"] =
	mammals.MM[is.na(mammals.MM$iucnRedListCategory),"IUCN_status"]
toRemove = grepl("\\.y",names(mammals.MM)) |
	names(mammals.MM)%in%c("Genus","Family","Order","Class","Phylum","gbif_rank","IUCN_status")
mammals.MM[,toRemove] = NULL
names(mammals.MM)[1:ncol(s2z.mammals2)] = names(s2z.mammals2)

# Fine tune Ter/Aqu/Arb info (a)
guild.ter = apply(mammals.MM[,sprintf("GUILDE.%d",c(3,10,12,14:15,20:22,24:25))],1,sum)
mammals.MM[guild.ter>0&!is.na(guild.ter),"Ter"] = 1
guild.aqu = apply(mammals.MM[,sprintf("GUILDE.%d",c(1,2,4:9,26))],1,sum)
mammals.MM[guild.aqu>0&!is.na(guild.aqu),"Aqu"] = 1
guild.arb = apply(mammals.MM[,sprintf("GUILDE.%d",c(11,13,16:19,23))],1,sum)
mammals.MM[guild.arb>0&!is.na(guild.arb),"Arb"] = 1
mammals.MM[!is.na(guild.arb),c("Ter","Aqu","Arb")][is.na(mammals.MM[!is.na(guild.arb),c("Ter","Aqu","Arb")])] = 0

# Fine tune habitat simple
hab.wat = apply(mammals.MM[,sprintf("GUILDE.%d",c(1,2,4,5,8))],1,sum)
mammals.MM[hab.wat>0 & !is.na(hab.wat),"Habitat_water"] = 1
mammals.MM[hab.wat%in%0 & !mammals.MM$Habitat_water%in%1,"Habitat_water"] = 0
hab.wet = apply(mammals.MM[,sprintf("GUILDE.%d",c(6,7,9,26))],1,sum)
mammals.MM[hab.wet>0 & !is.na(hab.wet),"Habitat_wetlands"] = 1
mammals.MM[hab.wet%in%0 & !mammals.MM$Habitat_wetlands%in%1,"Habitat_wetlands"] = 0
mammals.MM[mammals.MM[,"GUILDE.21"]>0 & !is.na(mammals.MM[,"GUILDE.21"]),"Habitat_rocks"] = 1
mammals.MM[mammals.MM[,"GUILDE.21"]%in%0 & !mammals.MM$Habitat_rocks%in%1,"Habitat_rocks"] = 0
hab.grass = apply(mammals.MM[,sprintf("GUILDE.%d",c(14,20))],1,sum)
mammals.MM[hab.grass>0 & !is.na(hab.grass),"Habitat_grassland"] = 1
mammals.MM[hab.grass%in%0 & !mammals.MM$Habitat_grassland%in%1,"Habitat_grassland"] = 0
hab.forest = apply(mammals.MM[,sprintf("GUILDE.%d",c(16,17,19))],1,sum)
mammals.MM[hab.forest>0 & !is.na(hab.forest),"Habitat_forests"] = 1
mammals.MM[hab.forest%in%0 & !mammals.MM$Habitat_forests%in%1,"Habitat_forests"] = 0
mammals.MM[mammals.MM[,"GUILDE.17"]>0 & !is.na(mammals.MM[,"GUILDE.17"]),"Habitat_deciduous"] = 1
mammals.MM[mammals.MM[,"GUILDE.17"]%in%0 & !mammals.MM$Habitat_deciduous%in%1,"Habitat_deciduous"] = 0
mammals.MM[mammals.MM[,"GUILDE.19"]>0 & !is.na(mammals.MM[,"GUILDE.19"]),"Habitat_coniferous"] = 1
mammals.MM[mammals.MM[,"GUILDE.19"]%in%0 & !mammals.MM$Habitat_coniferous%in%1,"Habitat_coniferous"] = 0
mammals.MM[mammals.MM[,"GUILDE.16"]>0 & !is.na(mammals.MM[,"GUILDE.16"]),"Habitat_mixed"] = 1
mammals.MM[mammals.MM[,"GUILDE.16"]%in%0 & !mammals.MM$Habitat_mixed%in%1,"Habitat_mixed"] = 0
hab.pioneer = apply(mammals.MM[,sprintf("GUILDE.%d",c(3,10,12,22))],1,sum)
mammals.MM[hab.pioneer>0 & !is.na(hab.pioneer),"Habitat_pioneer"] = 1
mammals.MM[hab.pioneer%in%0 & !mammals.MM$Habitat_pioneer%in%1,"Habitat_pioneer"] = 0
hab.urban = apply(mammals.MM[,sprintf("GUILDE.%d",c(22,24))],1,sum)
mammals.MM[hab.urban>0 & !is.na(hab.urban),"Habitat_urban"] = 1
mammals.MM[hab.urban%in%0 & !mammals.MM$Habitat_urban%in%1,"Habitat_urban"] = 0
mammals.MM[mammals.MM[,"GUILDE.23"]>0 & !is.na(mammals.MM[,"GUILDE.23"]),"Habitat_green_urban"] = 1
mammals.MM[mammals.MM[,"GUILDE.23"]%in%0 & !mammals.MM$Habitat_green_urban%in%1,"Habitat_green_urban"] = 0
hab.agri = apply(mammals.MM[,sprintf("GUILDE.%d",c(11,25))],1,sum)
mammals.MM[hab.agri>0 & !is.na(hab.agri),"Habitat_agricultural"] = 1
mammals.MM[hab.agri%in%0 & !mammals.MM$Habitat_agricultural%in%1,"Habitat_agricultural"] = 0
mammals.MM[mammals.MM[,"GUILDE.13"]>0 & !is.na(mammals.MM[,"GUILDE.13"]),"Habitat_bushes"] = 1
mammals.MM[mammals.MM[,"GUILDE.13"]%in%0 & !mammals.MM$Habitat_bushes%in%1,"Habitat_bushes"] = 0
mammals.MM[mammals.MM[,"GUILDE.15"]>0 & !is.na(mammals.MM[,"GUILDE.15"]),"Habitat_transitional"] = 1
mammals.MM[mammals.MM[,"GUILDE.15"]%in%0 & !mammals.MM$Habitat_transitional%in%1,"Habitat_transitional"] = 0
mammals.MM[mammals.MM[,"GUILDE.18"]>0 & !is.na(mammals.MM[,"GUILDE.18"]),"Habitat_heathland"] = 1
mammals.MM[mammals.MM[,"GUILDE.18"]%in%0 & !mammals.MM$Habitat_heathland%in%1,"Habitat_heathland"] = 0
mammals.MM[,HABit[!HABit%in%names(mammals.MM)]] = NA
mammals.MM[,names(mammals.MM)[grepl("GUILD",names(mammals.MM))]] = NULL

# Fine tune habitat tetrapods
hab2.wat = apply(mammals.MM[,sprintf("MajorHabitat_%d",c(9,10,12,15))],1,sum)
mammals.MM[hab2.wat>0&!is.na(hab2.wat),"Habitat_water"] = 1
mammals.MM[hab2.wat%in%0&is.na(mammals.MM$Habitat_water),"Habitat_water"] = 0
mammals.MM[mammals.MM$MajorHabitat_5>0&!is.na(mammals.MM$MajorHabitat_5),"Habitat_wetlands"] = 1
mammals.MM[mammals.MM$MajorHabitat_5%in%0&is.na(mammals.MM$Habitat_wetlands),"Habitat_wetlands"] = 0
hab2.rock = apply(mammals.MM[,sprintf("MajorHabitat_%d",c(6,7))],1,sum)
mammals.MM[hab2.rock>0&!is.na(hab2.rock),"Habitat_rocks"] = 1
mammals.MM[hab2.rock%in%0&is.na(mammals.MM$Habitat_rocks),"Habitat_rocks"] = 0
mammals.MM[mammals.MM$MajorHabitat_4>0&!is.na(mammals.MM$MajorHabitat_4),"Habitat_grassland"] = 1
mammals.MM[mammals.MM$MajorHabitat_4%in%0&is.na(mammals.MM$Habitat_grassland),"Habitat_grassland"] = 0
mammals.MM[mammals.MM$MajorHabitat_1>0&!is.na(mammals.MM$MajorHabitat_1),"Habitat_forests"] = 1
mammals.MM[mammals.MM$MajorHabitat_1%in%0&is.na(mammals.MM$Habitat_forests),"Habitat_forests"] = 0
mammals.MM[mammals.MM$MajorHabitat_3>0&!is.na(mammals.MM$MajorHabitat_3)%in%1,"Habitat_bushes"] = 1
mammals.MM[mammals.MM$MajorHabitat_3%in%0&is.na(mammals.MM$Habitat_bushes),"Habitat_bushes"] = 0
mammals.MM[mammals.MM$MajorHabitat_13>0&!is.na(mammals.MM$MajorHabitat_13)%in%1,"Habitat_sand"] = 1
mammals.MM[mammals.MM$MajorHabitat_13%in%0&is.na(mammals.MM$Habitat_sand),"Habitat_sand"] = 0
mammals.MM[mammals.MM$MajorHabitat_14>0&!is.na(mammals.MM$MajorHabitat_14),"Habitat_agricultural"] = 1
mammals.MM[mammals.MM$MajorHabitat_14%in%0&is.na(mammals.MM$Habitat_agricultural),"Habitat_agricultural"] = 0
mammals.MM = mammals.MM[,!grepl("MajorHabitat_",names(mammals.MM))]

# Fine tune Ter/Aqu/Arb info (b)
mammals.MM[mammals.MM$Habitat_urban%in%1,"Ter"] = 1
mammals.MM[mammals.MM$Habitat_green_urban%in%1,"Ter"] = 1
mammals.MM[mammals.MM$Habitat_agriculture%in%1,"Ter"] = 1
mammals.MM[mammals.MM$Habitat_deciduous%in%1,"Arb"] = 1
mammals.MM[mammals.MM$Habitat_coniferous%in%1,"Arb"] = 1
mammals.MM[mammals.MM$Habitat_mixed%in%1,"Arb"] = 1
mammals.MM[mammals.MM$Habitat_grassland%in%1,"Ter"] = 1
mammals.MM[mammals.MM$Habitat_heathland%in%1,"Arb"] = 1
mammals.MM[mammals.MM$Habitat_bushes%in%1,"Arb"] = 1
mammals.MM[mammals.MM$Habitat_sand%in%1,"Ter"] = 1
mammals.MM[mammals.MM$Habitat_wetlands%in%1,"Aqu"] = 1
mammals.MM[mammals.MM$Habitat_water%in%1,"Aqu"] = 1
mammals.MM[mammals.MM$Habitat_rocks%in%1,"Ter"] = 1
mammals.MM[mammals.MM$Habitat_pioneer%in%1,"Ter"] = 1
l.temp = mammals.MM[,c("Ter","Aqu","Arb","Aer")]
l.temp [is.na(l.temp)] = 0
l.temp[apply(l.temp,1,function(x) all(x%in%0)),] = NA
mammals.MM[,c("Ter","Aqu","Arb","Aer")] = l.temp

# Aquatic, Terrestrial or Marine
# EcoFresh, EcoTer, EcoMarine columns not necessary

# Fine tune Ter/Aqu info (c)
mammals.MM = mammals.MM[order(mammals.MM$species),]
mf1 = merge(mammals.MM,antoine.TerAqu,by="species",all.x=TRUE)
all(mammals.MM$species == mf1$species) # auto check
mammals.MM[mf1$scheme%in%"terrestrial"&is.na(mammals.MM$Ter),"Ter"] = 1
mammals.MM[mf1$scheme%in%"aquatic",c("Aqu","EcoFresh")] = 1
mammals.MM[mammals.MM$EcoFresh%in%1,"Aqu"] = 1

# Reorder & Save
hab.n = grep("Habitat_",names(mammals.MM))
eco.n = grep("Eco",names(mammals.MM))
col.ref = c("species","scientificName","taxonRank","gbif_status","gbif_accepted","genus","family","order",
	"class","phylum","group","Source","originCH","originEUR","swissRedListCategory","europeanRegionalRedListCategory",
	"iucnRedListCategory","EcoTer","EcoFresh","EcoMar","Fos","Ter","Aqu","Arb","Aer",
	names(mammals.MM[,hab.n]),"RangeSize_km2")
mammals.MM = mammals.MM[,c(col.ref,names(mammals.MM)[!names(mammals.MM)%in%col.ref])]
write.table(mammals.MM,"outputs/raw_traits/non_formated/s2z_raw_traits_mammals.txt",row.names=FALSE)


### ==========================================================================================================================
### Reptiles
### ==========================================================================================================================


# # Specifically select reptiles in the s2z and vertlife taxa
# s2z.reptiles = s2z.sp[s2z.sp$group%in%"Reptilia",]
# vert.reptiles = vertlife[vertlife$Class%in%"Reptilia",]

# # Open the best source and check number of reptiles in the s2z database
# reptiles.meiri = data.frame(read_excel("./reptiles/Meiri & al. 2024 - Supplementary_Table_S1_-_squamBase1.xlsx"))
# b1 = as.numeric(reptiles.meiri$youngest.age.at.first.breeding..female..months.) /12
# b2 = as.numeric(reptiles.meiri$oldest.age.at.first.breeding..female..months.) /12
# reptiles.meiri$First_breeding_y = mapply(sum,b1,b2,MoreArgs=list(na.rm=TRUE))/2
# reptiles.meiri$First_breeding_y[reptiles.meiri$First_breeding_y%in%0] = NA
# reptraits = data.frame(read_excel("./reptiles/ReptTraits dataset v1-2.xlsx",2))
# e1 = as.numeric(reptraits$Minimal.elevation..m.)
# e2 = as.numeric(reptraits$Maximum.elevation..m.)
# reptraits$Elevation = mapply(sum,e1,e2,MoreArgs=list(na.rm=TRUE))/2
# reptraits$Elevation[reptraits$Elevation%in%0] = NA
# names(reptraits)[c(14:16,27,30,37)] = c("AnnuMeanTemp","TempSeasonality","PrecipSeasonality",
# 	"Longevity_max_y","BodyLength_mm","Litter_size")

# # Combine them
# reptiles.meiri2 = merge(reptraits,reptiles.meiri[,c("Species.name..Binomial.","First_breeding_y")],
# 	by.x="Species",by.y="Species.name..Binomial.",all=TRUE)

# # Match potential species with the vertlife database (+ fill some gaps)
# meiri.merge = c("Species","Order","Family","Genus","Elevation","First_breeding_y",
# 	"AnnuMeanTemp","TempSeasonality","PrecipSeasonality","Venomous..yes.or.no.","Diet","Diet..comments",
# 	"Active.time","Dorsal.colour","Dorsal.pattern","Foraging.mode","Pupil.Shape","Fangs","Longevity_max_y",
# 	"Maximum.body.mass..g.","BodyLength_mm","Reproductive.mode","Sex.determining.mechanism..GSD.or.TSD.",
# 	"Litter_size","Mean.Tb")
# reptiles.vert = merge(reptiles.meiri2[,meiri.merge],vert.reptiles[,vert.traits],
# 	by.x="Species",by.y="Scientific.Name",all=TRUE)
# i1 = is.na(reptiles.vert$Active.time)
# i2 = is.na(reptiles.vert$BodyLength_mm.x)
# i4 = is.na(reptiles.vert$Genus.x)
# i5 = is.na(reptiles.vert$Family.x)
# i6 = is.na(reptiles.vert$Order.x)
# i7 = is.na(reptiles.vert$Elevation.x)
# i8 = is.na(reptiles.vert$AnnuMeanTemp.x)
# i9 = is.na(reptiles.vert$TempSeasonality.x)
# i10 = is.na(reptiles.vert$PrecipSeasonality.x)
# i11 = is.na(reptiles.vert$Maximum.body.mass..g.)
# reptiles.vert[i1,"Active.time"] = reptiles.vert[i1,"Activity"]
# reptiles.vert[i2,"BodyLength_mm.x"] = reptiles.vert[i2,"BodyLength_mm.y"]
# reptiles.vert[i4,"Genus.x"] = reptiles.vert[i4,"Genus.y"]
# reptiles.vert[i5,"Family.x"] = reptiles.vert[i5,"Family.y"]
# reptiles.vert[i6,"Order.x"] = reptiles.vert[i6,"Order.y"]
# reptiles.vert[i7,"Elevation.x"] = reptiles.vert[i7,"Elevation.y"]
# reptiles.vert[i8,"AnnuMeanTemp.x"] = reptiles.vert[i8,"AnnuMeanTemp.y"]
# reptiles.vert[i9,"TempSeasonality.x"] = reptiles.vert[i9,"TempSeasonality.y"]
# reptiles.vert[i10,"PrecipSeasonality.x"] = reptiles.vert[i10,"PrecipSeasonality.y"]
# reptiles.vert[i11,"Maximum.body.mass..g."] = reptiles.vert[i11,"BodyMass_g"]
# reptiles.vert[,c("Activity","BodyLength_mm.y","Genus.y","Family.y","Order.y",
# 	"Elevation.y","AnnuMeanTemp.y","TempSeasonality.y","PrecipSeasonality.y","BodyMass_g")] = NULL

# # Fucking diet
# reptiles.vert$Diet_Inv = NA
# reptiles.vert$Diet_Vert = NA
# reptiles.vert$Diet_Scav = NA
# reptiles.vert$Diet_Fruit = NA
# reptiles.vert$Diet_Nect = NA
# reptiles.vert$Diet_Seed = NA
# reptiles.vert$Diet_Plant = NA
# reptiles.vert[reptiles.vert$Diet%in%"Herbivorous","Diet_Plant"] = 1
# reptiles.vert[reptiles.vert$Diet%in%"Herbivorous",c("Diet_Inv","Diet_Vert","Diet_Scav")] = 0
# reptiles.vert[reptiles.vert$Diet%in%"Carnivorous",c("Diet_Inv","Diet_Vert")] = 0 # We don't known which one
# reptiles.vert[reptiles.vert$Diet%in%"Carnivorous",c("Diet_Fruit","Diet_Nect","Diet_Seed","Diet_Plant")] = 0
# invt.str = "insect|insct|arthropod|invertebrate|slug|mollus|grasshop|earthw|spider|beetle|ant|cricket|invrtebr|termite|crab"
# invt.str = paste0(invt.str,"|",str_to_title(invt.str))
# reptiles.vert[grepl(invt.str,reptiles.vert$Diet..comments),"Diet_Inv"] = 1
# vert.str = "amphib|mammal|reptil|lizard|bird|frog|mouse| vertebrat|fish|gecko"
# invt.str = paste0(vert.str,"|",str_to_title(vert.str))
# reptiles.vert[grepl(vert.str,reptiles.vert$Diet..comments),"Diet_Vert"] = 1
# reptiles.vert[grepl("detritu|litter|dead|human|Human",reptiles.vert$Diet..comments),"Diet_Scav"] = 1
# reptiles.vert[grepl("fruit|Fruit|juice|Juice|berry|Berry|berri|Berri",reptiles.vert$Diet..comments),c("Diet_Fruit")] = 1
# reptiles.vert[grepl("nectar|Nectar|flower|Flower|Pollen|pollen",reptiles.vert$Diet..comments),"Diet_Nect"] = 1
# reptiles.vert[grepl("Seed|seed",reptiles.vert$Diet..comments),"Diet_Seed"] = 1
# allz = apply(reptiles.vert[,c("Diet_Inv","Diet_Vert")],1,function(x) all(x%in%0))
# reptiles.vert[reptiles.vert$Diet%in%"Carnivorous"&allz,c("Diet_Inv","Diet_Vert")] = 1
# reptiles.vert[grepl("plant|Plant|Vegeta|vegeta|leaf|Leaf",reptiles.vert$Diet..comments),"Diet_Plant"] = 1
# reptiles.vert[,c("Diet..comments")] = NULL

# # New order for names
# names(reptiles.vert)[c(1:24,57)] = c("species","Order","Family","Genus","Elevation","First_breeding_y",
# 	"AnnuMeanTemp","TempSeasonality","PrecipSeasonality","Venomous","Diet","Activity","Dorsal_colour","Dorsal_pattern",
# 	"Foraging_mode","Shape_pupil","Fangs","Longevity_max_y","BodyMass_g","BodyLength_mm","Reproductive_mode",
# 	"Sex_determinism","Litter_size","Body_temp","IUCN_status")
# names(reptiles.vert) = gsub("\\.|_\\.","_",names(reptiles.vert))

# # Change names if necessary
# tGS = to_GenusSpecies(reptiles.vert$species)
# reptiles.vert$species = tGS[,"CanonicalName"]
# reptiles.vert = reptiles.vert[!is.na(reptiles.vert$species),]
# #reptiles.vert = combine_rows(reptiles.vert,"species") # Unique names (no need)

# # Reorder columns
# reptiles.vert[,t.tetra[!t.tetra%in%names(reptiles.vert)]] = NA
# not.n = names(reptiles.vert)[!(names(reptiles.vert)%in%t.tetra)]
# reptiles.vert = reptiles.vert[,c(t.tetra,not.n)]
# reptiles.vert[reptiles.vert==""] = NA

# # Convert to numeric
# non.num = c("species","Activity","Diet","IUCN_status","Reproductive_mode","Order","Shape_pupil",
# 	"Family","Genus","Fangs","Venomous","Dorsal_colour","Dorsal_pattern","Foraging_mode","Sex_determinism")
# reptiles.vert[,!names(reptiles.vert)%in%non.num] = sapply(reptiles.vert[,!names(reptiles.vert)%in%non.num],as.numeric)

# # Add gbif information
# reptiles.vert[,GB.col] = NA
# for (i in 1:nrow(reptiles.vert)){
#     cat("...sp.",i)
#     gbif.search = get_status(reptiles.vert[i,"species"],phylum="Chordata") # Search for accepted names
#     if (!is.null(gbif.search)){
#     	reptiles.vert$gbif_accepted[i] = gbif.search$scientificName[1]
#     	reptiles.vert$gbif_rank[i] = gbif.search$rank[1]
#         reptiles.vert$gbif_phylum[i] = gbif.search$Phylum[1]
#         reptiles.vert$gbif_genus[i] = gbif.search$Genus[1]
#         reptiles.vert$gbif_order[i] = gbif.search$Order[1]
#         reptiles.vert$gbif_family[i] = gbif.search$Family[1]
#         reptiles.vert$gbif_class[i] = gbif.search$Class[1]
#         reptiles.vert$gbif_IUCN[i] = gbif.search$IUCN_status[1]
#     	}
# }
# write.table(reptiles.vert,"temp_reptiles.txt",row.names=FALSE)
reptiles.vert = read.table("data/temp/temp_reptiles.txt",header=TRUE)

# Format our table
which.gbif = which(names(reptiles.vert)%in%GB.col)
reptiles.vert = reptiles.vert[,c(1,which.gbif,2:(ncol(reptiles.vert)-length(which.gbif)))]
reptiles.vert[is.na(reptiles.vert$gbif_genus),"gbif_genus"] = reptiles.vert[is.na(reptiles.vert$gbif_genus),"Genus"]
reptiles.vert$gbif_genus[is.na(reptiles.vert$gbif_genus)] =
	sapply(strsplit(reptiles.vert$species[is.na(reptiles.vert$gbif_genus)]," "),function(x) x[1])
reptiles.vert[is.na(reptiles.vert$gbif_family),"gbif_family"] = reptiles.vert[is.na(reptiles.vert$gbif_family),"Family"]
reptiles.vert[is.na(reptiles.vert$gbif_order),"gbif_order"] = reptiles.vert[is.na(reptiles.vert$gbif_order),"Order"]
reptiles.vert$gbif_class = "Reptilia"
reptiles.vert$gbif_phylum = "Chordata"
reptiles.vert[is.na(reptiles.vert$IUCN_status),"IUCN_status"] = reptiles.vert[is.na(reptiles.vert$IUCN_status),"gbif_IUCN"]
reptiles.vert[,c("Genus","Family","Order","gbif_IUCN")] = NULL
names(reptiles.vert)[2:length(GB.col)] = new.col
gbif.cond = reptiles.vert$IUCN_status%in%c("NOT_FOUND","NOT_EVALUATED")
reptiles.vert[gbif.cond,"IUCN_status"] = NA

# Extract EU checklist and merge
s2z.reptiles = s2z.sp[s2z.sp$group%in%"Reptiles",]
reptiles.merge1 = merge(s2z.reptiles,reptiles.vert,by="species")
reptiles.vert = reptiles.vert[!reptiles.vert$species%in%reptiles.merge1$species,]
s2z.reptiles2 = s2z.reptiles[!s2z.reptiles$species%in%reptiles.merge1$species,]

# Regroup all the available information for the accepted species (mean when continuous values)
mam.ndup = reptiles.vert[!(duplicated(reptiles.vert$gbif_accepted)|duplicated(reptiles.vert$gbif_accepted,fromLast=TRUE)),]
mam.dup = reptiles.vert[duplicated(reptiles.vert$gbif_accepted)|duplicated(reptiles.vert$gbif_accepted,fromLast=TRUE),]
mam.homo = combine_rows(df=mam.dup,ref.col="gbif_accepted",binary=FALSE)
mam.homo = mam.homo[!is.na(mam.homo$gbif_accepted),]

# Combine, attempt another merge and rbind both results
mam.temp = rbind(mam.ndup,mam.homo)
reptiles.merge2 = merge(s2z.reptiles2,mam.temp,by="gbif_accepted",all.x=TRUE)
names(reptiles.merge2)[1:2] = c("gbif_accepted.x","species")
reptiles.merge2$gbif_accepted.y = reptiles.merge2$gbif_accepted.x
reptiles.merge2 = reptiles.merge2[,names(reptiles.merge1)]
reptiles.MM = rbind(reptiles.merge1,reptiles.merge2)

# Try to fill in mising information fron one column to another
reptiles.MM[is.na(reptiles.MM$genus),"genus"] = reptiles.MM[is.na(reptiles.MM$genus),"Genus"]
reptiles.MM[is.na(reptiles.MM$family),"family"] = reptiles.MM[is.na(reptiles.MM$family),"Family"]
reptiles.MM[is.na(reptiles.MM$order),"order"] = reptiles.MM[is.na(reptiles.MM$order),"Order"]
reptiles.MM[is.na(reptiles.MM$class),"class"] = reptiles.MM[is.na(reptiles.MM$class),"Class"]
reptiles.MM[is.na(reptiles.MM$phylum),"phylum"] = reptiles.MM[is.na(reptiles.MM$phylum),"Phylum"]
reptiles.MM[grepl("unresolved_accepted",reptiles.MM$gbif_accepted.x),"gbif_accepted.x"] =
	reptiles.MM[grepl("unresolved_accepted",reptiles.MM$gbif_accepted.x),"gbif_accepted.y"]
reptiles.MM$gbif_accepted.x[is.na(reptiles.MM$gbif_accepted.x)] =
	paste0("unresolved_accepted2_", seq_len(sum(is.na(reptiles.MM$gbif_accepted.x))))
reptiles.MM[is.na(reptiles.MM$taxonRank),"taxonRank"] = reptiles.MM[is.na(reptiles.MM$taxonRank),"gbif_rank"]
reptiles.MM[is.na(reptiles.MM$iucnRedListCategory),"iucnRedListCategory"] =
	reptiles.MM[is.na(reptiles.MM$iucnRedListCategory),"IUCN_status"]
toRemove = grepl("\\.y",names(reptiles.MM)) |
	names(reptiles.MM)%in%c("Genus","Family","Order","Class","Phylum","gbif_rank","IUCN_status")
reptiles.MM[,toRemove] = NULL
names(reptiles.MM)[1:ncol(s2z.reptiles2)] = names(s2z.reptiles2)

# Fine tune Ter/Aqu/Arb info (a)
guild.ter = apply(reptiles.MM[,sprintf("GUILDE.%d",c(3,10,12,14:15,20:22,24:25))],1,sum)
reptiles.MM[guild.ter>0&!is.na(guild.ter),"Ter"] = 1
guild.aqu = apply(reptiles.MM[,sprintf("GUILDE.%d",c(1,2,4:9,26))],1,sum)
reptiles.MM[guild.aqu>0&!is.na(guild.aqu),"Aqu"] = 1
guild.arb = apply(reptiles.MM[,sprintf("GUILDE.%d",c(11,13,16:19,23))],1,sum)
reptiles.MM[guild.arb>0&!is.na(guild.arb),"Arb"] = 1
reptiles.MM[!is.na(guild.arb),c("Ter","Aqu","Arb")][is.na(reptiles.MM[!is.na(guild.arb),c("Ter","Aqu","Arb")])] = 0

# Fine tune habitat simple
hab.wat = apply(reptiles.MM[,sprintf("GUILDE.%d",c(1,2,4,5,8))],1,sum)
reptiles.MM[hab.wat>0 & !is.na(hab.wat),"Habitat_water"] = 1
reptiles.MM[hab.wat%in%0 & !reptiles.MM$Habitat_water%in%1,"Habitat_water"] = 0
hab.wet = apply(reptiles.MM[,sprintf("GUILDE.%d",c(6,7,9,26))],1,sum)
reptiles.MM[hab.wet>0 & !is.na(hab.wet),"Habitat_wetlands"] = 1
reptiles.MM[hab.wet%in%0 & !reptiles.MM$Habitat_wetlands%in%1,"Habitat_wetlands"] = 0
reptiles.MM[reptiles.MM[,"GUILDE.21"]>0 & !is.na(reptiles.MM[,"GUILDE.21"]),"Habitat_rocks"] = 1
reptiles.MM[reptiles.MM[,"GUILDE.21"]%in%0 & !reptiles.MM$Habitat_rocks%in%1,"Habitat_rocks"] = 0
hab.grass = apply(reptiles.MM[,sprintf("GUILDE.%d",c(14,20))],1,sum)
reptiles.MM[hab.grass>0 & !is.na(hab.grass),"Habitat_grassland"] = 1
reptiles.MM[hab.grass%in%0 & !reptiles.MM$Habitat_grassland%in%1,"Habitat_grassland"] = 0
hab.forest = apply(reptiles.MM[,sprintf("GUILDE.%d",c(16,17,19))],1,sum)
reptiles.MM[hab.forest>0 & !is.na(hab.forest),"Habitat_forests"] = 1
reptiles.MM[hab.forest%in%0 & !reptiles.MM$Habitat_forests%in%1,"Habitat_forests"] = 0
reptiles.MM[reptiles.MM[,"GUILDE.17"]>0 & !is.na(reptiles.MM[,"GUILDE.17"]),"Habitat_deciduous"] = 1
reptiles.MM[reptiles.MM[,"GUILDE.17"]%in%0 & !reptiles.MM$Habitat_deciduous%in%1,"Habitat_deciduous"] = 0
reptiles.MM[reptiles.MM[,"GUILDE.19"]>0 & !is.na(reptiles.MM[,"GUILDE.19"]),"Habitat_coniferous"] = 1
reptiles.MM[reptiles.MM[,"GUILDE.19"]%in%0 & !reptiles.MM$Habitat_coniferous%in%1,"Habitat_coniferous"] = 0
reptiles.MM[reptiles.MM[,"GUILDE.16"]>0 & !is.na(reptiles.MM[,"GUILDE.16"]),"Habitat_mixed"] = 1
reptiles.MM[reptiles.MM[,"GUILDE.16"]%in%0 & !reptiles.MM$Habitat_mixed%in%1,"Habitat_mixed"] = 0
hab.pioneer = apply(reptiles.MM[,sprintf("GUILDE.%d",c(3,10,12,22))],1,sum)
reptiles.MM[hab.pioneer>0 & !is.na(hab.pioneer),"Habitat_pioneer"] = 1
reptiles.MM[hab.pioneer%in%0 & !reptiles.MM$Habitat_pioneer%in%1,"Habitat_pioneer"] = 0
hab.urban = apply(reptiles.MM[,sprintf("GUILDE.%d",c(22,24))],1,sum)
reptiles.MM[hab.urban>0 & !is.na(hab.urban),"Habitat_urban"] = 1
reptiles.MM[hab.urban%in%0 & !reptiles.MM$Habitat_urban%in%1,"Habitat_urban"] = 0
reptiles.MM[reptiles.MM[,"GUILDE.23"]>0 & !is.na(reptiles.MM[,"GUILDE.23"]),"Habitat_green_urban"] = 1
reptiles.MM[reptiles.MM[,"GUILDE.23"]%in%0 & !reptiles.MM$Habitat_green_urban%in%1,"Habitat_green_urban"] = 0
hab.agri = apply(reptiles.MM[,sprintf("GUILDE.%d",c(11,25))],1,sum)
reptiles.MM[hab.agri>0 & !is.na(hab.agri),"Habitat_agricultural"] = 1
reptiles.MM[hab.agri%in%0 & !reptiles.MM$Habitat_agricultural%in%1,"Habitat_agricultural"] = 0
reptiles.MM[reptiles.MM[,"GUILDE.13"]>0 & !is.na(reptiles.MM[,"GUILDE.13"]),"Habitat_bushes"] = 1
reptiles.MM[reptiles.MM[,"GUILDE.13"]%in%0 & !reptiles.MM$Habitat_bushes%in%1,"Habitat_bushes"] = 0
reptiles.MM[reptiles.MM[,"GUILDE.15"]>0 & !is.na(reptiles.MM[,"GUILDE.15"]),"Habitat_transitional"] = 1
reptiles.MM[reptiles.MM[,"GUILDE.15"]%in%0 & !reptiles.MM$Habitat_transitional%in%1,"Habitat_transitional"] = 0
reptiles.MM[reptiles.MM[,"GUILDE.18"]>0 & !is.na(reptiles.MM[,"GUILDE.18"]),"Habitat_heathland"] = 1
reptiles.MM[reptiles.MM[,"GUILDE.18"]%in%0 & !reptiles.MM$Habitat_heathland%in%1,"Habitat_heathland"] = 0
reptiles.MM[,HABit[!HABit%in%names(reptiles.MM)]] = NA
reptiles.MM[,names(reptiles.MM)[grepl("GUILD",names(reptiles.MM))]] = NULL

# Fine tune habitat tetrapods
hab2.wat = apply(reptiles.MM[,sprintf("MajorHabitat_%d",c(9,10,12,15))],1,sum)
reptiles.MM[hab2.wat>0&!is.na(hab2.wat),"Habitat_water"] = 1
reptiles.MM[hab2.wat%in%0&is.na(reptiles.MM$Habitat_water),"Habitat_water"] = 0
reptiles.MM[reptiles.MM$MajorHabitat_5>0&!is.na(reptiles.MM$MajorHabitat_5),"Habitat_wetlands"] = 1
reptiles.MM[reptiles.MM$MajorHabitat_5%in%0&is.na(reptiles.MM$Habitat_wetlands),"Habitat_wetlands"] = 0
hab2.rock = apply(reptiles.MM[,sprintf("MajorHabitat_%d",c(6,7))],1,sum)
reptiles.MM[hab2.rock>0&!is.na(hab2.rock),"Habitat_rocks"] = 1
reptiles.MM[hab2.rock%in%0&is.na(reptiles.MM$Habitat_rocks),"Habitat_rocks"] = 0
reptiles.MM[reptiles.MM$MajorHabitat_4>0&!is.na(reptiles.MM$MajorHabitat_4),"Habitat_grassland"] = 1
reptiles.MM[reptiles.MM$MajorHabitat_4%in%0&is.na(reptiles.MM$Habitat_grassland),"Habitat_grassland"] = 0
reptiles.MM[reptiles.MM$MajorHabitat_1>0&!is.na(reptiles.MM$MajorHabitat_1),"Habitat_forests"] = 1
reptiles.MM[reptiles.MM$MajorHabitat_1%in%0&is.na(reptiles.MM$Habitat_forests),"Habitat_forests"] = 0
reptiles.MM[reptiles.MM$MajorHabitat_3>0&!is.na(reptiles.MM$MajorHabitat_3)%in%1,"Habitat_bushes"] = 1
reptiles.MM[reptiles.MM$MajorHabitat_3%in%0&is.na(reptiles.MM$Habitat_bushes),"Habitat_bushes"] = 0
reptiles.MM[reptiles.MM$MajorHabitat_13>0&!is.na(reptiles.MM$MajorHabitat_13)%in%1,"Habitat_sand"] = 1
reptiles.MM[reptiles.MM$MajorHabitat_13%in%0&is.na(reptiles.MM$Habitat_sand),"Habitat_sand"] = 0
reptiles.MM[reptiles.MM$MajorHabitat_14>0&!is.na(reptiles.MM$MajorHabitat_14),"Habitat_agricultural"] = 1
reptiles.MM[reptiles.MM$MajorHabitat_14%in%0&is.na(reptiles.MM$Habitat_agricultural),"Habitat_agricultural"] = 0
reptiles.MM = reptiles.MM[,!grepl("MajorHabitat_",names(reptiles.MM))]

# Fine tune Ter/Aqu/Arb info (b)
reptiles.MM[reptiles.MM$Habitat_urban%in%1,"Ter"] = 1
reptiles.MM[reptiles.MM$Habitat_green_urban%in%1,"Ter"] = 1
reptiles.MM[reptiles.MM$Habitat_agriculture%in%1,"Ter"] = 1
reptiles.MM[reptiles.MM$Habitat_deciduous%in%1,"Arb"] = 1
reptiles.MM[reptiles.MM$Habitat_coniferous%in%1,"Arb"] = 1
reptiles.MM[reptiles.MM$Habitat_mixed%in%1,"Arb"] = 1
reptiles.MM[reptiles.MM$Habitat_grassland%in%1,"Ter"] = 1
reptiles.MM[reptiles.MM$Habitat_heathland%in%1,"Arb"] = 1
reptiles.MM[reptiles.MM$Habitat_bushes%in%1,"Arb"] = 1
reptiles.MM[reptiles.MM$Habitat_sand%in%1,"Ter"] = 1
reptiles.MM[reptiles.MM$Habitat_wetlands%in%1,"Aqu"] = 1
reptiles.MM[reptiles.MM$Habitat_water%in%1,"Aqu"] = 1
reptiles.MM[reptiles.MM$Habitat_rocks%in%1,"Ter"] = 1
reptiles.MM[reptiles.MM$Habitat_pioneer%in%1,"Ter"] = 1
l.temp = reptiles.MM[,c("Ter","Aqu","Arb","Aer")]
l.temp [is.na(l.temp)] = 0
l.temp[apply(l.temp,1,function(x) all(x%in%0)),] = NA
reptiles.MM[,c("Ter","Aqu","Arb","Aer")] = l.temp

# Aquatic, Terrestrial or Marine
# EcoFresh, EcoTer, EcoMarine columns not necessary

# Fine tune Ter/Aqu info (c)
reptiles.MM = reptiles.MM[order(reptiles.MM$species),]
mf1 = merge(reptiles.MM,antoine.TerAqu,by="species",all.x=TRUE)
all(reptiles.MM$species == mf1$species) # auto check
reptiles.MM[mf1$scheme%in%"terrestrial"&is.na(reptiles.MM$Ter),"Ter"] = 1
reptiles.MM[mf1$scheme%in%"aquatic",c("Aqu","EcoFresh")] = 1
reptiles.MM[reptiles.MM$EcoFresh%in%1,"Aqu"] = 1

# Reorder & Save
hab.n = grep("Habitat_",names(reptiles.MM))
eco.n = grep("Eco",names(reptiles.MM))
col.ref = c("species","scientificName","taxonRank","gbif_status","gbif_accepted","genus","family","order",
	"class","phylum","group","Source","originCH","originEUR","swissRedListCategory","europeanRegionalRedListCategory",
	"iucnRedListCategory","EcoTer","EcoFresh","EcoMar","Fos","Ter","Aqu","Arb","Aer",
	names(reptiles.MM[,hab.n]),"RangeSize_km2")
reptiles.MM = reptiles.MM[,c(col.ref,names(reptiles.MM)[!names(reptiles.MM)%in%col.ref])]
write.table(reptiles.MM,"outputs/raw_traits/non_formated/s2z_raw_traits_reptiles.txt",row.names=FALSE)


### ==========================================================================================================================
### Amphibians
### ==========================================================================================================================


# # Specifically select reptiles in the s2z and vertlife taxa
# vert.amphi = vertlife[vertlife$Class%in%"Amphibia",]

# # Open the best sources
# amphi.bio = read.csv("./amphibians/AmphiBIO_v1.csv")
# amphi.h.anu = read.csv("./amphibians/Huang_2023/Anura.csv")
# amphi.h.cau = read.csv("./amphibians/Huang_2023/Caudata.csv")
# amphi.h.gym = read.csv("./amphibians/Huang_2023/Gymnophiona.csv")

# # Create a new column Activity for amphi.bio
# amphi.bio$Activity = NA
# amphi.bio$Activity[amphi.bio$Diu==1] = "Diurnal"
# amphi.bio$Activity[amphi.bio$Noc==1] = "Nocturnal"
# amphi.bio$Activity[amphi.bio$Noc==1 & amphi.bio$Diu==1] = "Cathemeral"
# amphi.bio$Activity[amphi.bio$Crepu==1] = "Crepuscular"

# # Create a new column Diet for amphi bio
# q.carnivore = sapply(1:nrow(amphi.bio),function(x) {any(amphi.bio[x,14:15]==1)})
# q.herbivore = sapply(1:nrow(amphi.bio),function(x) {any(amphi.bio[x,10:13]==1)})
# q.carnivore[is.na(q.carnivore)] = FALSE
# q.herbivore[is.na(q.herbivore)] = FALSE
# q.omnivore = q.carnivore + q.herbivore
# amphi.bio$Diet = NA
# amphi.bio$Diet[q.carnivore] = "Carnivorous"
# amphi.bio$Diet[q.herbivore] = "Herbivorous"
# amphi.bio$Diet[q.omnivore%in%2] = "Omnivorous"

# # Combine the "h" sources based on the best trait coverages
# common.t = c("Species","Genus","Family","SVL","HL","ED","IOD","IND","EN")
# amphi.ph1 = amphi.h.anu[,common.t]
# amphi.ph2 = amphi.h.cau[,common.t]
# amphi.ph3 = amphi.h.gym[,common.t]
# amphi.PH = rbind(amphi.ph1,amphi.ph2,amphi.ph3)

# # Average values per species (combine_rows equivalent)
# H.sp = unique(amphi.PH$Species)
# H.out =
# lapply(H.sp,function(x){
# 	target = amphi.PH[amphi.PH$Species%in%x,common.t[-c(1:3)]]
# 	tax = amphi.PH[amphi.PH$Species%in%x,common.t[c(1:3)]]
# 	data.frame(tax[1,],t(as.data.frame(apply(target,2,mean,na.rm=TRUE))))
# })
# amphi.H = do.call("rbind",H.out)
# row.names(amphi.H) = NULL

# # Merge both datasets, fill in lacking information and keep only interesting columns
# amphi.traits = merge(amphi.bio,amphi.H,by="Species",all=TRUE)
# i1 = is.na(amphi.traits$Body_size_mm)
# i2 = is.na(amphi.traits$Genus.x)
# amphi.traits[i1,"Body_size_mm"] = amphi.traits[i1,"SVL"]
# amphi.traits[i2,"Genus.x"] = amphi.traits[i2,"Genus.y"]
# amphi.traits[i2,"Family.x"] = amphi.traits[i2,"Family.y"]
# oliveira.merge = c("Species","Genus.x","Family.x","Order","Leaves","Flowers","Seeds","Fruits","Arthro","Vert","Body_mass_g",
# 	"Age_at_maturity_min_y","Age_at_maturity_max_y","Body_size_mm","Longevity_max_y","Litter_size_min_n","Litter_size_max_n",
# 	"Activity","Diet","HL","ED","IOD","IND","EN","Viv")

# # Match potential species with the vertlife database
# amphi.vert = merge(amphi.traits[,oliveira.merge],vert.amphi[,vert.traits],by.x="Species",by.y="Scientific.Name",all=TRUE)
# amphi.vert[amphi.vert$Viv%in%1,"Viv"] = "Viviparous"
# amphi.vert[amphi.vert$Viv%in%0,"Viv"] = "Oviparous"
# i1 = is.na(amphi.vert$Body_mass_g)
# i2 = is.na(amphi.vert$Body_size_mm)
# i3 = is.na(amphi.vert$Activity.x)
# i4 = is.na(amphi.vert$Genus.x)
# amphi.vert[i1,"Body_mass_g"] = amphi.vert[i1,"BodyMass_g"]
# amphi.vert[i2,"Body_size_mm"] = amphi.vert[i2,"BodyLength_mm"]
# amphi.vert[i3,"Activity.x"] = amphi.vert[i3,"Activity.y"]
# amphi.vert[i4,"Genus.x"] = amphi.vert[i4,"Genus"]
# amphi.vert[i4,"Family.x"] = amphi.vert[i4,"Family"]
# amphi.vert[i4,"Order.x"] = amphi.vert[i4,"Order.y"]
# amphi.vert[,c("BodyMass_g","BodyLength_mm","Activity.y","Genus","Family","Order.y")] = NULL

# # Average min/max
# amphi.vert$Age_at_maturity_y = (amphi.vert$Age_at_maturity_min_y+amphi.vert$Age_at_maturity_max_y)/2
# amphi.vert$Litter_size = (amphi.vert$Litter_size_min_n+amphi.vert$Litter_size_max_n)/2
# amphi.vert[,c("Age_at_maturity_min_y","Age_at_maturity_max_y","Litter_size_min_n","Litter_size_max_n")] = NULL

# # New order for names
# names(amphi.vert)[c(1:12,14,16:21,58)] = c("species","Genus","Family","Order","Diet_Plant","Diet_Nect","Diet_Seed",
# 	"Diet_Fruit","Diet_Inv","Diet_Vert","BodyMass_g","BodyLength_mm","Activity","Head_length_mm","Eye_diameter_mm",
# 	"Interorbital_dist_mm","Internarial_dist_mm","Eye_nostril_dist_mm","Reproductive_mode","IUCN_status")
# names(amphi.vert) = gsub("\\.|_\\.","_",names(amphi.vert))

# # Change names if necessary
# tGS = to_GenusSpecies(amphi.vert$species)
# amphi.vert$species = tGS[,"CanonicalName"]
# amphi.vert$species = gsub("\\?","",amphi.vert$species)
# amphi.vert = amphi.vert[!is.na(amphi.vert$species),]
# amphi.vert = combine_rows(amphi.vert,"species") # Unique names now

# # Reorder columns
# amphi.vert[,t.tetra[!t.tetra%in%names(amphi.vert)]] = NA
# not.n = names(amphi.vert)[!(names(amphi.vert)%in%t.tetra)]
# amphi.vert = amphi.vert[,c(t.tetra,not.n)]
# amphi.vert[amphi.vert==""] = NA

# # Convert to numeric
# non.num = c("species","Activity","Diet","IUCN_status","Reproductive_mode","Order","Family","Genus")
# amphi.vert[,!names(amphi.vert)%in%non.num] = sapply(amphi.vert[,!names(amphi.vert)%in%non.num],as.numeric)

# # Fill in additonal logical information
# amphi.vert$Aer = 0
# g.diet = grepl("Diet_",names(amphi.vert))
# na.id = apply(amphi.vert[,g.diet],1,function(x) all(is.na(x)))
# amphi.vert[!na.id,g.diet][is.na(amphi.vert[!na.id,g.diet])] = 0
# amphi.vert$Diet_Scav = NA

# # Add gbif information
# amphi.vert[,GB.col] = NA
# for (i in 1:nrow(amphi.vert)){
#     cat("...sp.",i)
#     gbif.search = get_status(amphi.vert[i,"species"],phylum="Chordata") # Search for accepted names
#     if (!is.null(gbif.search)){
#     	amphi.vert$gbif_accepted[i] = gbif.search$scientificName[1] # Fill in the result
#     	amphi.vert$gbif_rank[i] = gbif.search$rank[1]
#         amphi.vert$gbif_phylum[i] = gbif.search$Phylum[1]
#         amphi.vert$gbif_genus[i] = gbif.search$Genus[1]
#        	amphi.vert$gbif_order[i] = gbif.search$Order[1]
#         amphi.vert$gbif_family[i] = gbif.search$Family[1]
#         amphi.vert$gbif_class[i] = gbif.search$Class[1]
#         amphi.vert$gbif_IUCN[i] = gbif.search$IUCN_status[1]
#      }
# }
# write.table(amphi.vert,"temp_amphi.txt",row.names=FALSE)
amphi.vert = read.table("data/temp/temp_amphi.txt",header=TRUE)

# Format our table
which.gbif = which(names(amphi.vert)%in%GB.col)
amphi.vert = amphi.vert[,c(1,which.gbif,2:(ncol(amphi.vert)-length(which.gbif)))]
amphi.vert[is.na(amphi.vert$gbif_genus),"gbif_genus"] = amphi.vert[is.na(amphi.vert$gbif_genus),"Genus"]
amphi.vert$gbif_genus[is.na(amphi.vert$gbif_genus)] =
	unlist(sapply(strsplit(amphi.vert$species[is.na(amphi.vert$gbif_genus)]," "),function(x) x[1]))
amphi.vert[is.na(amphi.vert$gbif_family),"gbif_family"] = amphi.vert[is.na(amphi.vert$gbif_family),"Family"]
amphi.vert[is.na(amphi.vert$gbif_order),"gbif_order"] = amphi.vert[is.na(amphi.vert$gbif_order),"Order"]
amphi.vert$gbif_class = "Amphibia"
amphi.vert$gbif_phylum = "Chordata"
amphi.vert[is.na(amphi.vert$IUCN_status),"IUCN_status"] = amphi.vert[is.na(amphi.vert$IUCN_status),"gbif_IUCN"]
amphi.vert[,c("Genus","Family","Order","gbif_IUCN")] = NULL
names(amphi.vert)[2:length(GB.col)] = new.col
gbif.cond = amphi.vert$IUCN_status%in%c("NOT_FOUND","NOT_EVALUATED")
amphi.vert[gbif.cond,"IUCN_status"] = NA

# Extract EU checklist and merge
s2z.amphi = s2z.sp[s2z.sp$group%in%"Amphibians",]
amphi.merge1 = merge(s2z.amphi,amphi.vert,by="species")
amphi.vert = amphi.vert[!amphi.vert$species%in%amphi.merge1$species,]
s2z.amphi2 = s2z.amphi[!s2z.amphi$species%in%amphi.merge1$species,]

# Regroup all the available information for the accepted species (mean when continuous values)
mam.ndup = amphi.vert[!(duplicated(amphi.vert$gbif_accepted)|duplicated(amphi.vert$gbif_accepted,fromLast=TRUE)),]
mam.dup = amphi.vert[duplicated(amphi.vert$gbif_accepted)|duplicated(amphi.vert$gbif_accepted,fromLast=TRUE),]
mam.homo = combine_rows(df=mam.dup,ref.col="gbif_accepted",binary=FALSE)
mam.homo = mam.homo[!is.na(mam.homo$gbif_accepted),]

# Combine, attempt another merge and rbind both results
mam.temp = rbind(mam.ndup,mam.homo)
amphi.merge2 = merge(s2z.amphi2,mam.temp,by="gbif_accepted",all.x=TRUE)
names(amphi.merge2)[1:2] = c("gbif_accepted.x","species")
amphi.merge2$gbif_accepted.y = amphi.merge2$gbif_accepted.x
amphi.merge2 = amphi.merge2[,names(amphi.merge1)]
amphi.MM = rbind(amphi.merge1,amphi.merge2)

# Try to fill in mising information fron one column to another
amphi.MM[is.na(amphi.MM$genus),"genus"] = amphi.MM[is.na(amphi.MM$genus),"Genus"]
amphi.MM[is.na(amphi.MM$family),"family"] = amphi.MM[is.na(amphi.MM$family),"Family"]
amphi.MM[is.na(amphi.MM$order),"order"] = amphi.MM[is.na(amphi.MM$order),"Order"]
amphi.MM[is.na(amphi.MM$class),"class"] = amphi.MM[is.na(amphi.MM$class),"Class"]
amphi.MM[is.na(amphi.MM$phylum),"phylum"] = amphi.MM[is.na(amphi.MM$phylum),"Phylum"]
amphi.MM[grepl("unresolved_accepted",amphi.MM$gbif_accepted.x),"gbif_accepted.x"] =
	amphi.MM[grepl("unresolved_accepted",amphi.MM$gbif_accepted.x),"gbif_accepted.y"]
amphi.MM$gbif_accepted.x[is.na(amphi.MM$gbif_accepted.x)] =
	paste0("unresolved_accepted2_", seq_len(sum(is.na(amphi.MM$gbif_accepted.x))))
amphi.MM[is.na(amphi.MM$taxonRank),"taxonRank"] = amphi.MM[is.na(amphi.MM$taxonRank),"gbif_rank"]
amphi.MM[is.na(amphi.MM$iucnRedListCategory),"iucnRedListCategory"] =
	amphi.MM[is.na(amphi.MM$iucnRedListCategory),"IUCN_status"]
toRemove = grepl("\\.y",names(amphi.MM)) |
	names(amphi.MM)%in%c("Genus","Family","Order","Class","Phylum","gbif_rank","IUCN_status")
amphi.MM[,toRemove] = NULL
names(amphi.MM)[1:ncol(s2z.amphi2)] = names(s2z.amphi2)

# Fine tune Ter/Aqu/Arb info (a)
guild.ter = apply(amphi.MM[,sprintf("GUILDE.%d",c(3,10,12,14:15,20:22,24:25))],1,sum)
amphi.MM[guild.ter>0&!is.na(guild.ter),"Ter"] = 1
guild.aqu = apply(amphi.MM[,sprintf("GUILDE.%d",c(1,2,4:9,26))],1,sum)
amphi.MM[guild.aqu>0&!is.na(guild.aqu),"Aqu"] = 1
guild.arb = apply(amphi.MM[,sprintf("GUILDE.%d",c(11,13,16:19,23))],1,sum)
amphi.MM[guild.arb>0&!is.na(guild.arb),"Arb"] = 1
amphi.MM[!is.na(guild.arb),c("Ter","Aqu","Arb")][is.na(amphi.MM[!is.na(guild.arb),c("Ter","Aqu","Arb")])] = 0

# Fine tune habitat simple
hab.wat = apply(amphi.MM[,sprintf("GUILDE.%d",c(1,2,4,5,8))],1,sum)
amphi.MM[hab.wat>0 & !is.na(hab.wat),"Habitat_water"] = 1
amphi.MM[hab.wat%in%0 & !amphi.MM$Habitat_water%in%1,"Habitat_water"] = 0
hab.wet = apply(amphi.MM[,sprintf("GUILDE.%d",c(6,7,9,26))],1,sum)
amphi.MM[hab.wet>0 & !is.na(hab.wet),"Habitat_wetlands"] = 1
amphi.MM[hab.wet%in%0 & !amphi.MM$Habitat_wetlands%in%1,"Habitat_wetlands"] = 0
amphi.MM[amphi.MM[,"GUILDE.21"]>0 & !is.na(amphi.MM[,"GUILDE.21"]),"Habitat_rocks"] = 1
amphi.MM[amphi.MM[,"GUILDE.21"]%in%0 & !amphi.MM$Habitat_rocks%in%1,"Habitat_rocks"] = 0
hab.grass = apply(amphi.MM[,sprintf("GUILDE.%d",c(14,20))],1,sum)
amphi.MM[hab.grass>0 & !is.na(hab.grass),"Habitat_grassland"] = 1
amphi.MM[hab.grass%in%0 & !amphi.MM$Habitat_grassland%in%1,"Habitat_grassland"] = 0
hab.forest = apply(amphi.MM[,sprintf("GUILDE.%d",c(16,17,19))],1,sum)
amphi.MM[hab.forest>0 & !is.na(hab.forest),"Habitat_forests"] = 1
amphi.MM[hab.forest%in%0 & !amphi.MM$Habitat_forests%in%1,"Habitat_forests"] = 0
amphi.MM[amphi.MM[,"GUILDE.17"]>0 & !is.na(amphi.MM[,"GUILDE.17"]),"Habitat_deciduous"] = 1
amphi.MM[amphi.MM[,"GUILDE.17"]%in%0 & !amphi.MM$Habitat_deciduous%in%1,"Habitat_deciduous"] = 0
amphi.MM[amphi.MM[,"GUILDE.19"]>0 & !is.na(amphi.MM[,"GUILDE.19"]),"Habitat_coniferous"] = 1
amphi.MM[amphi.MM[,"GUILDE.19"]%in%0 & !amphi.MM$Habitat_coniferous%in%1,"Habitat_coniferous"] = 0
amphi.MM[amphi.MM[,"GUILDE.16"]>0 & !is.na(amphi.MM[,"GUILDE.16"]),"Habitat_mixed"] = 1
amphi.MM[amphi.MM[,"GUILDE.16"]%in%0 & !amphi.MM$Habitat_mixed%in%1,"Habitat_mixed"] = 0
hab.pioneer = apply(amphi.MM[,sprintf("GUILDE.%d",c(3,10,12,22))],1,sum)
amphi.MM[hab.pioneer>0 & !is.na(hab.pioneer),"Habitat_pioneer"] = 1
amphi.MM[hab.pioneer%in%0 & !amphi.MM$Habitat_pioneer%in%1,"Habitat_pioneer"] = 0
hab.urban = apply(amphi.MM[,sprintf("GUILDE.%d",c(22,24))],1,sum)
amphi.MM[hab.urban>0 & !is.na(hab.urban),"Habitat_urban"] = 1
amphi.MM[hab.urban%in%0 & !amphi.MM$Habitat_urban%in%1,"Habitat_urban"] = 0
amphi.MM[amphi.MM[,"GUILDE.23"]>0 & !is.na(amphi.MM[,"GUILDE.23"]),"Habitat_green_urban"] = 1
amphi.MM[amphi.MM[,"GUILDE.23"]%in%0 & !amphi.MM$Habitat_green_urban%in%1,"Habitat_green_urban"] = 0
hab.agri = apply(amphi.MM[,sprintf("GUILDE.%d",c(11,25))],1,sum)
amphi.MM[hab.agri>0 & !is.na(hab.agri),"Habitat_agricultural"] = 1
amphi.MM[hab.agri%in%0 & !amphi.MM$Habitat_agricultural%in%1,"Habitat_agricultural"] = 0
amphi.MM[amphi.MM[,"GUILDE.13"]>0 & !is.na(amphi.MM[,"GUILDE.13"]),"Habitat_bushes"] = 1
amphi.MM[amphi.MM[,"GUILDE.13"]%in%0 & !amphi.MM$Habitat_bushes%in%1,"Habitat_bushes"] = 0
amphi.MM[amphi.MM[,"GUILDE.15"]>0 & !is.na(amphi.MM[,"GUILDE.15"]),"Habitat_transitional"] = 1
amphi.MM[amphi.MM[,"GUILDE.15"]%in%0 & !amphi.MM$Habitat_transitional%in%1,"Habitat_transitional"] = 0
amphi.MM[amphi.MM[,"GUILDE.18"]>0 & !is.na(amphi.MM[,"GUILDE.18"]),"Habitat_heathland"] = 1
amphi.MM[amphi.MM[,"GUILDE.18"]%in%0 & !amphi.MM$Habitat_heathland%in%1,"Habitat_heathland"] = 0
amphi.MM[,HABit[!HABit%in%names(amphi.MM)]] = NA
amphi.MM[,names(amphi.MM)[grepl("GUILD",names(amphi.MM))]] = NULL

# Fine tune habitat tetrapods
hab2.wat = apply(amphi.MM[,sprintf("MajorHabitat_%d",c(9,10,12,15))],1,sum)
amphi.MM[hab2.wat>0&!is.na(hab2.wat),"Habitat_water"] = 1
amphi.MM[hab2.wat%in%0&is.na(amphi.MM$Habitat_water),"Habitat_water"] = 0
amphi.MM[amphi.MM$MajorHabitat_5>0&!is.na(amphi.MM$MajorHabitat_5),"Habitat_wetlands"] = 1
amphi.MM[amphi.MM$MajorHabitat_5%in%0&is.na(amphi.MM$Habitat_wetlands),"Habitat_wetlands"] = 0
hab2.rock = apply(amphi.MM[,sprintf("MajorHabitat_%d",c(6,7))],1,sum)
amphi.MM[hab2.rock>0&!is.na(hab2.rock),"Habitat_rocks"] = 1
amphi.MM[hab2.rock%in%0&is.na(amphi.MM$Habitat_rocks),"Habitat_rocks"] = 0
amphi.MM[amphi.MM$MajorHabitat_4>0&!is.na(amphi.MM$MajorHabitat_4),"Habitat_grassland"] = 1
amphi.MM[amphi.MM$MajorHabitat_4%in%0&is.na(amphi.MM$Habitat_grassland),"Habitat_grassland"] = 0
amphi.MM[amphi.MM$MajorHabitat_1>0&!is.na(amphi.MM$MajorHabitat_1),"Habitat_forests"] = 1
amphi.MM[amphi.MM$MajorHabitat_1%in%0&is.na(amphi.MM$Habitat_forests),"Habitat_forests"] = 0
amphi.MM[amphi.MM$MajorHabitat_3>0&!is.na(amphi.MM$MajorHabitat_3)%in%1,"Habitat_bushes"] = 1
amphi.MM[amphi.MM$MajorHabitat_3%in%0&is.na(amphi.MM$Habitat_bushes),"Habitat_bushes"] = 0
amphi.MM[amphi.MM$MajorHabitat_13>0&!is.na(amphi.MM$MajorHabitat_13)%in%1,"Habitat_sand"] = 1
amphi.MM[amphi.MM$MajorHabitat_13%in%0&is.na(amphi.MM$Habitat_sand),"Habitat_sand"] = 0
amphi.MM[amphi.MM$MajorHabitat_14>0&!is.na(amphi.MM$MajorHabitat_14),"Habitat_agricultural"] = 1
amphi.MM[amphi.MM$MajorHabitat_14%in%0&is.na(amphi.MM$Habitat_agricultural),"Habitat_agricultural"] = 0
amphi.MM = amphi.MM[,!grepl("MajorHabitat_",names(amphi.MM))]

# Fine tune Ter/Aqu/Arb info (b)
amphi.MM[amphi.MM$Habitat_urban%in%1,"Ter"] = 1
amphi.MM[amphi.MM$Habitat_green_urban%in%1,"Ter"] = 1
amphi.MM[amphi.MM$Habitat_agriculture%in%1,"Ter"] = 1
amphi.MM[amphi.MM$Habitat_deciduous%in%1,"Arb"] = 1
amphi.MM[amphi.MM$Habitat_coniferous%in%1,"Arb"] = 1
amphi.MM[amphi.MM$Habitat_mixed%in%1,"Arb"] = 1
amphi.MM[amphi.MM$Habitat_grassland%in%1,"Ter"] = 1
amphi.MM[amphi.MM$Habitat_heathland%in%1,"Arb"] = 1
amphi.MM[amphi.MM$Habitat_bushes%in%1,"Arb"] = 1
amphi.MM[amphi.MM$Habitat_sand%in%1,"Ter"] = 1
amphi.MM[amphi.MM$Habitat_wetlands%in%1,"Aqu"] = 1
amphi.MM[amphi.MM$Habitat_water%in%1,"Aqu"] = 1
amphi.MM[amphi.MM$Habitat_rocks%in%1,"Ter"] = 1
amphi.MM[amphi.MM$Habitat_pioneer%in%1,"Ter"] = 1
l.temp = amphi.MM[,c("Ter","Aqu","Arb","Aer")]
l.temp [is.na(l.temp)] = 0
l.temp[apply(l.temp,1,function(x) all(x%in%0)),] = NA
amphi.MM[,c("Ter","Aqu","Arb","Aer")] = l.temp

# Aquatic, Terrestrial or Marine
# EcoFresh, EcoTer, EcoMarine columns not necessary

# Fine tune Ter/Aqu info (c)
amphi.MM = amphi.MM[order(amphi.MM$species),]
mf1 = merge(amphi.MM,antoine.TerAqu,by="species",all.x=TRUE)
all(amphi.MM$species == mf1$species) # auto check
amphi.MM[mf1$scheme%in%"terrestrial"&is.na(amphi.MM$Ter),"Ter"] = 1
amphi.MM[mf1$scheme%in%"aquatic",c("Aqu","EcoFresh")] = 1
amphi.MM[amphi.MM$EcoFresh%in%1,"Aqu"] = 1

# Reorder & Save
hab.n = grep("Habitat_",names(amphi.MM))
eco.n = grep("Eco",names(amphi.MM))
col.ref = c("species","scientificName","taxonRank","gbif_status","gbif_accepted","genus","family","order",
	"class","phylum","group","Source","originCH","originEUR","swissRedListCategory","europeanRegionalRedListCategory",
	"iucnRedListCategory","EcoTer","EcoFresh","EcoMar","Fos","Ter","Aqu","Arb","Aer",
	names(amphi.MM[,hab.n]),"RangeSize_km2")
amphi.MM = amphi.MM[,c(col.ref,names(amphi.MM)[!names(amphi.MM)%in%col.ref])]
write.table(amphi.MM,"outputs/raw_traits/non_formated/s2z_raw_traits_amphibians.txt",row.names=FALSE)


### ==========================================================================================================================
### Birds
### ==========================================================================================================================


# # Specifically select aves in the s2z and vertlife taxa
# vert.aves = vertlife[vertlife$Class%in%"Aves",]

# # Open the best sources (remove duplicates-empty for aves.jetz)
# aves.tobias = as.data.frame(read_excel("./mammals_n_birds/Tobias_2022/AVONET Supplementary dataset 1.xlsx",2))
# aves.wilman = read.table("./mammals_n_birds/Wilman_2014/BirdFuncDat.txt",header=TRUE,sep='\t')
# aves.jetz = read.csv2("./mammals_n_birds/Jetzt_2008/data.csv")
# aves.jetz = aves.jetz[!(duplicated(aves.jetz$Species)|duplicated(aves.jetz$Species,fromLast=TRUE)),]

# # Add an activity column for aves.tobias
# aves.wilman$Activity = NA
# aves.wilman$Activity[aves.wilman$Nocturnal==0] = "Diurnal"
# aves.wilman$Activity[aves.wilman$Nocturnal==0.5] = "Cathemeral"
# aves.wilman$Activity[aves.wilman$Nocturnal==1] = "Nocturnal"

# # Rename the diet column in aves.tobias and format wilman information
# names(aves.tobias)[30] = "Diet"
# aves.tobias$Diet[aves.tobias$Diet%in%"Carnivore"] = "Carnivorous"
# aves.tobias$Diet[aves.tobias$Diet%in%"Herbivore"] = "Herbivorous"
# aves.tobias$Diet[aves.tobias$Diet%in%"Omnivore"] = "Omnivorous"
# q.carnivore = sapply(1:nrow(aves.wilman),function(x) {sum(aves.wilman[x,10:14])>=70})
# q.herbivore = sapply(1:nrow(aves.wilman),function(x) {sum(aves.wilman[x,16:19])>=70})
# q.scavenger = aves.wilman$Diet.Scav>=70
# q.omnivore = q.carnivore + q.herbivore + q.scavenger
# aves.wilman$Diet = NA
# aves.wilman$Diet[q.carnivore] = "Carnivorous"
# aves.wilman$Diet[q.herbivore] = "Herbivorous"
# aves.wilman$Diet[q.omnivore==0] = "Omnivorous"
# aves.wilman$Diet[q.scavenger] = "Scavenger"
# aves.wilman[,10:19][aves.wilman[,10:19]!=0] = 1

# # And we only keep/convert the columns that amphibians have!
# names(aves.wilman)[11] = "Diet.Vert"
# aves.wilman$Diet.Vert[aves.wilman$Diet.Vect%in%1] = 1
# aves.wilman$Diet.Vert[aves.wilman$Diet.Vfish%in%1] = 1
# aves.wilman$Diet.Vert[aves.wilman$Diet.Vunk%in%1] = 1
# aves.wilman[,c("Diet.Vect","Diet.Vfish","Diet.Vunk")] = NULL

# # Merge tobias to the wilman and jetz dataset, and keep only interesting columns
# aves.traits1 = merge(aves.tobias,aves.wilman,by.x="Species1",by.y="Scientific",all=TRUE)
# aves.traits2 = merge(aves.traits1,aves.jetz,by.x="Species1",by.y="Species",all=TRUE)
# i1 = is.na(aves.traits2$Diet.x)
# i2 = is.na(aves.traits2$Family1)
# aves.traits2[i1,"Diet.x"] = aves.traits2[i1,"Diet.y"]
# aves.traits2[i2,"Family1"] = aves.traits2[i2,"Family"]
# aves.traits2[i2,"Order1"] = aves.traits2[i2,"Order"]
# towiljetz.merge = c("Species1","Family1","Order1","Beak.Length_Culmen","Beak.Length_Nares","Beak.Width",
# 	"Beak.Depth","Tarsus.Length","Wing.Length","Kipps.Distance","Secondary1","Hand-Wing.Index","Tail.Length",
# 	"Mass","Diet.x","Diet.Inv","Diet.Vert","Diet.Scav","Diet.Fruit","Diet.Nect","Diet.Seed",
# 	"Diet.PlantO","BodyMass.Value","Activity","Clutch_size")

# # Match potential species with the vertlife database
# aves.vert = merge(aves.traits2[,towiljetz.merge],vert.aves[,vert.traits],
# 	by.x="Species1",by.y="Scientific.Name",all=TRUE)
# i1 = is.na(aves.vert$Activity.x)
# i2 = is.na(aves.vert$Mass)
# i3 = is.na(aves.vert$Family1)
# aves.vert[i1,"Activity.x"] = aves.vert[i1,"Activity.y"]
# aves.vert[i2,"Mass"] = aves.vert[i2,"BodyMass.Value"]
# aves.vert[i2,"Mass"] = aves.vert[i2,"BodyMass_g"]
# aves.vert[i3,"Family1"] = aves.vert[i3,"Family"]
# aves.vert[i3,"Order1"] = aves.vert[i3,"Order"]
# aves.vert[,c("BodyMass.Value","BodyMass_g","Activity.y","Family","Order")] = NULL

# # New order for names
# names(aves.vert)[c(1:3,12,14,15,22:24,63)] = c("species","Family","Order","Hand.Wing.Index",
# 	"BodyMass_g","Diet","Diet.Plant","Activity","Litter_size","IUCN.status")
# names(aves.vert) = gsub("\\.|_\\.","_",names(aves.vert))

# # Change names if necessary
# tGS = to_GenusSpecies(aves.vert$species)
# aves.vert$species = tGS[,"CanonicalName"]
# aves.vert = aves.vert[!is.na(aves.vert$species),]
# # aves.vert = combine_rows(aves.vert,"species") # Unique names (no need)

# # Add reproduction mode (Viviparous excpet Ornithorynque and four other Echnidé species)
# aves.vert$Reproductive_mode = "Oviparous"

# # Reorder columns
# aves.vert[,t.tetra[!t.tetra%in%names(aves.vert)]] = NA
# not.n = names(aves.vert)[!(names(aves.vert)%in%t.tetra)]
# aves.vert = aves.vert[,c(t.tetra,not.n)]
# aves.vert[aves.vert==""] = NA

# # Convert to numeric
# non.num = c("species","Activity","Diet","IUCN_status","Reproductive_mode","Order","Family","Genus")
# aves.vert[,!names(aves.vert)%in%non.num] = sapply(aves.vert[,!names(aves.vert)%in%non.num],as.numeric)

# # Add gbif information
# aves.vert[,GB.col] = NA
# for (i in 1:nrow(aves.vert)){
#     cat("...sp.",i)
#     gbif.search = get_status(aves.vert[i,"species"],phylum="Chordata") # Search for accepted names
#     if (!is.null(gbif.search)){
#     	aves.vert$gbif_accepted[i] = gbif.search$scientificName[1]
#     	aves.vert$gbif_rank[i] = gbif.search$rank[1]
#         aves.vert$gbif_phylum[i] = gbif.search$Phylum[1]
#         aves.vert$gbif_genus[i] = gbif.search$Genus[1]
#         aves.vert$gbif_order[i] = gbif.search$Order[1]
#         aves.vert$gbif_family[i] = gbif.search$Family[1]
#         aves.vert$gbif_class[i] = gbif.search$Class[1]
#         aves.vert$gbif_IUCN[i] = gbif.search$IUCN_status[1]
#     }
# }
# write.table(aves.vert,"temp_aves.txt",row.names=FALSE)
aves.vert = read.table("data/temp/temp_aves.txt",header=TRUE)

# Format our table
which.gbif = which(names(aves.vert)%in%GB.col)
aves.vert = aves.vert[,c(1,which.gbif,2:(ncol(aves.vert)-length(which.gbif)))]
aves.vert[is.na(aves.vert$gbif_genus),"gbif_genus"] = aves.vert[is.na(aves.vert$gbif_genus),"Genus"]
aves.vert$gbif_genus[is.na(aves.vert$gbif_genus)] =
	sapply(strsplit(aves.vert$species[is.na(aves.vert$gbif_genus)]," "),function(x) x[1])
aves.vert[is.na(aves.vert$gbif_family),"gbif_family"] = aves.vert[is.na(aves.vert$gbif_family),"Family"]
aves.vert[is.na(aves.vert$gbif_order),"gbif_order"] = aves.vert[is.na(aves.vert$gbif_order),"Order"]
aves.vert$gbif_class = "Aves"
aves.vert$gbif_phylum = "Chordata"
aves.vert[is.na(aves.vert$IUCN_status),"IUCN_status"] = aves.vert[is.na(aves.vert$IUCN_status),"gbif_IUCN"]
aves.vert[,c("Genus","Family","Order","gbif_IUCN")] = NULL
names(aves.vert)[2:length(GB.col)] = new.col
gbif.cond = aves.vert$IUCN_status%in%c("NOT_FOUND","NOT_EVALUATED")
aves.vert[gbif.cond,"IUCN_status"] = NA

# Extract EU checklist and merge
s2z.aves = s2z.sp[s2z.sp$group%in%"Birds",]
aves.merge1 = merge(s2z.aves,aves.vert,by="species")
aves.vert = aves.vert[!aves.vert$species%in%aves.merge1$species,]
s2z.aves2 = s2z.aves[!s2z.aves$species%in%aves.merge1$species,]

# Regroup all the available information for the accepted species (mean when continuous values)
mam.ndup = aves.vert[!(duplicated(aves.vert$gbif_accepted)|duplicated(aves.vert$gbif_accepted,fromLast=TRUE)),]
mam.dup = aves.vert[duplicated(aves.vert$gbif_accepted)|duplicated(aves.vert$gbif_accepted,fromLast=TRUE),]
mam.homo = combine_rows(df=mam.dup,ref.col="gbif_accepted",binary=FALSE)
mam.homo = mam.homo[!is.na(mam.homo$gbif_accepted),]

# Combine, attempt another merge and rbind both results
mam.temp = rbind(mam.ndup,mam.homo)
aves.merge2 = merge(s2z.aves2,mam.temp,by="gbif_accepted",all.x=TRUE)
names(aves.merge2)[1:2] = c("gbif_accepted.x","species")
aves.merge2$gbif_accepted.y = aves.merge2$gbif_accepted.x
aves.merge2 = aves.merge2[,names(aves.merge1)]
aves.MM = rbind(aves.merge1,aves.merge2)

# Try to fill in mising information fron one column to another
aves.MM[is.na(aves.MM$genus),"genus"] = aves.MM[is.na(aves.MM$genus),"Genus"]
aves.MM[is.na(aves.MM$family),"family"] = aves.MM[is.na(aves.MM$family),"Family"]
aves.MM[is.na(aves.MM$order),"order"] = aves.MM[is.na(aves.MM$order),"Order"]
aves.MM[is.na(aves.MM$class),"class"] = aves.MM[is.na(aves.MM$class),"Class"]
aves.MM[is.na(aves.MM$phylum),"phylum"] = aves.MM[is.na(aves.MM$phylum),"Phylum"]
aves.MM[grepl("unresolved_accepted",aves.MM$gbif_accepted.x),"gbif_accepted.x"] =
	aves.MM[grepl("unresolved_accepted",aves.MM$gbif_accepted.x),"gbif_accepted.y"]
aves.MM$gbif_accepted.x[is.na(aves.MM$gbif_accepted.x)] =
	paste0("unresolved_accepted2_", seq_len(sum(is.na(aves.MM$gbif_accepted.x))))
aves.MM[is.na(aves.MM$taxonRank),"taxonRank"] = aves.MM[is.na(aves.MM$taxonRank),"gbif_rank"]
aves.MM[is.na(aves.MM$iucnRedListCategory),"iucnRedListCategory"] =
	aves.MM[is.na(aves.MM$iucnRedListCategory),"IUCN_status"]
toRemove = grepl("\\.y",names(aves.MM)) |
	names(aves.MM)%in%c("Genus","Family","Order","Class","Phylum","gbif_rank","IUCN_status")
aves.MM[,toRemove] = NULL
names(aves.MM)[1:ncol(s2z.aves2)] = names(s2z.aves2)

# Fine tune Ter/Aqu/Arb info (a)
guild.ter = apply(aves.MM[,sprintf("GUILDE.%d",c(3,10,12,14:15,20:22,24:25))],1,sum)
aves.MM[guild.ter>0&!is.na(guild.ter),"Ter"] = 1
guild.aqu = apply(aves.MM[,sprintf("GUILDE.%d",c(1,2,4:9,26))],1,sum)
aves.MM[guild.aqu>0&!is.na(guild.aqu),"Aqu"] = 1
guild.arb = apply(aves.MM[,sprintf("GUILDE.%d",c(11,13,16:19,23))],1,sum)
aves.MM[guild.arb>0&!is.na(guild.arb),"Arb"] = 1
aves.MM[!is.na(guild.arb),c("Ter","Aqu","Arb")][is.na(aves.MM[!is.na(guild.arb),c("Ter","Aqu","Arb")])] = 0

# Fine tune habitat simple
hab.wat = apply(aves.MM[,sprintf("GUILDE.%d",c(1,2,4,5,8))],1,sum)
aves.MM[hab.wat>0 & !is.na(hab.wat),"Habitat_water"] = 1
aves.MM[hab.wat%in%0 & !aves.MM$Habitat_water%in%1,"Habitat_water"] = 0
hab.wet = apply(aves.MM[,sprintf("GUILDE.%d",c(6,7,9,26))],1,sum)
aves.MM[hab.wet>0 & !is.na(hab.wet),"Habitat_wetlands"] = 1
aves.MM[hab.wet%in%0 & !aves.MM$Habitat_wetlands%in%1,"Habitat_wetlands"] = 0
aves.MM[aves.MM[,"GUILDE.21"]>0 & !is.na(aves.MM[,"GUILDE.21"]),"Habitat_rocks"] = 1
aves.MM[aves.MM[,"GUILDE.21"]%in%0 & !aves.MM$Habitat_rocks%in%1,"Habitat_rocks"] = 0
hab.grass = apply(aves.MM[,sprintf("GUILDE.%d",c(14,20))],1,sum)
aves.MM[hab.grass>0 & !is.na(hab.grass),"Habitat_grassland"] = 1
aves.MM[hab.grass%in%0 & !aves.MM$Habitat_grassland%in%1,"Habitat_grassland"] = 0
hab.forest = apply(aves.MM[,sprintf("GUILDE.%d",c(16,17,19))],1,sum)
aves.MM[hab.forest>0 & !is.na(hab.forest),"Habitat_forests"] = 1
aves.MM[hab.forest%in%0 & !aves.MM$Habitat_forests%in%1,"Habitat_forests"] = 0
aves.MM[aves.MM[,"GUILDE.17"]>0 & !is.na(aves.MM[,"GUILDE.17"]),"Habitat_deciduous"] = 1
aves.MM[aves.MM[,"GUILDE.17"]%in%0 & !aves.MM$Habitat_deciduous%in%1,"Habitat_deciduous"] = 0
aves.MM[aves.MM[,"GUILDE.19"]>0 & !is.na(aves.MM[,"GUILDE.19"]),"Habitat_coniferous"] = 1
aves.MM[aves.MM[,"GUILDE.19"]%in%0 & !aves.MM$Habitat_coniferous%in%1,"Habitat_coniferous"] = 0
aves.MM[aves.MM[,"GUILDE.16"]>0 & !is.na(aves.MM[,"GUILDE.16"]),"Habitat_mixed"] = 1
aves.MM[aves.MM[,"GUILDE.16"]%in%0 & !aves.MM$Habitat_mixed%in%1,"Habitat_mixed"] = 0
hab.pioneer = apply(aves.MM[,sprintf("GUILDE.%d",c(3,10,12,22))],1,sum)
aves.MM[hab.pioneer>0 & !is.na(hab.pioneer),"Habitat_pioneer"] = 1
aves.MM[hab.pioneer%in%0 & !aves.MM$Habitat_pioneer%in%1,"Habitat_pioneer"] = 0
hab.urban = apply(aves.MM[,sprintf("GUILDE.%d",c(22,24))],1,sum)
aves.MM[hab.urban>0 & !is.na(hab.urban),"Habitat_urban"] = 1
aves.MM[hab.urban%in%0 & !aves.MM$Habitat_urban%in%1,"Habitat_urban"] = 0
aves.MM[aves.MM[,"GUILDE.23"]>0 & !is.na(aves.MM[,"GUILDE.23"]),"Habitat_green_urban"] = 1
aves.MM[aves.MM[,"GUILDE.23"]%in%0 & !aves.MM$Habitat_green_urban%in%1,"Habitat_green_urban"] = 0
hab.agri = apply(aves.MM[,sprintf("GUILDE.%d",c(11,25))],1,sum)
aves.MM[hab.agri>0 & !is.na(hab.agri),"Habitat_agricultural"] = 1
aves.MM[hab.agri%in%0 & !aves.MM$Habitat_agricultural%in%1,"Habitat_agricultural"] = 0
aves.MM[aves.MM[,"GUILDE.13"]>0 & !is.na(aves.MM[,"GUILDE.13"]),"Habitat_bushes"] = 1
aves.MM[aves.MM[,"GUILDE.13"]%in%0 & !aves.MM$Habitat_bushes%in%1,"Habitat_bushes"] = 0
aves.MM[aves.MM[,"GUILDE.15"]>0 & !is.na(aves.MM[,"GUILDE.15"]),"Habitat_transitional"] = 1
aves.MM[aves.MM[,"GUILDE.15"]%in%0 & !aves.MM$Habitat_transitional%in%1,"Habitat_transitional"] = 0
aves.MM[aves.MM[,"GUILDE.18"]>0 & !is.na(aves.MM[,"GUILDE.18"]),"Habitat_heathland"] = 1
aves.MM[aves.MM[,"GUILDE.18"]%in%0 & !aves.MM$Habitat_heathland%in%1,"Habitat_heathland"] = 0
aves.MM[,HABit[!HABit%in%names(aves.MM)]] = NA
aves.MM[,names(aves.MM)[grepl("GUILD",names(aves.MM))]] = NULL

# Fine tune habitat tetrapods
hab2.wat = apply(aves.MM[,sprintf("MajorHabitat_%d",c(9,10,12,15))],1,sum)
aves.MM[hab2.wat>0&!is.na(hab2.wat),"Habitat_water"] = 1
aves.MM[hab2.wat%in%0&is.na(aves.MM$Habitat_water),"Habitat_water"] = 0
aves.MM[aves.MM$MajorHabitat_5>0&!is.na(aves.MM$MajorHabitat_5),"Habitat_wetlands"] = 1
aves.MM[aves.MM$MajorHabitat_5%in%0&is.na(aves.MM$Habitat_wetlands),"Habitat_wetlands"] = 0
hab2.rock = apply(aves.MM[,sprintf("MajorHabitat_%d",c(6,7))],1,sum)
aves.MM[hab2.rock>0&!is.na(hab2.rock),"Habitat_rocks"] = 1
aves.MM[hab2.rock%in%0&is.na(aves.MM$Habitat_rocks),"Habitat_rocks"] = 0
aves.MM[aves.MM$MajorHabitat_4>0&!is.na(aves.MM$MajorHabitat_4),"Habitat_grassland"] = 1
aves.MM[aves.MM$MajorHabitat_4%in%0&is.na(aves.MM$Habitat_grassland),"Habitat_grassland"] = 0
aves.MM[aves.MM$MajorHabitat_1>0&!is.na(aves.MM$MajorHabitat_1),"Habitat_forests"] = 1
aves.MM[aves.MM$MajorHabitat_1%in%0&is.na(aves.MM$Habitat_forests),"Habitat_forests"] = 0
aves.MM[aves.MM$MajorHabitat_3>0&!is.na(aves.MM$MajorHabitat_3)%in%1,"Habitat_bushes"] = 1
aves.MM[aves.MM$MajorHabitat_3%in%0&is.na(aves.MM$Habitat_bushes),"Habitat_bushes"] = 0
aves.MM[aves.MM$MajorHabitat_13>0&!is.na(aves.MM$MajorHabitat_13)%in%1,"Habitat_sand"] = 1
aves.MM[aves.MM$MajorHabitat_13%in%0&is.na(aves.MM$Habitat_sand),"Habitat_sand"] = 0
aves.MM[aves.MM$MajorHabitat_14>0&!is.na(aves.MM$MajorHabitat_14),"Habitat_agricultural"] = 1
aves.MM[aves.MM$MajorHabitat_14%in%0&is.na(aves.MM$Habitat_agricultural),"Habitat_agricultural"] = 0
aves.MM = aves.MM[,!grepl("MajorHabitat_",names(aves.MM))]

# Fine tune Ter/Aqu/Arb info (b)
aves.MM[aves.MM$Habitat_urban%in%1,"Ter"] = 1
aves.MM[aves.MM$Habitat_green_urban%in%1,"Ter"] = 1
aves.MM[aves.MM$Habitat_agriculture%in%1,"Ter"] = 1
aves.MM[aves.MM$Habitat_deciduous%in%1,"Arb"] = 1
aves.MM[aves.MM$Habitat_coniferous%in%1,"Arb"] = 1
aves.MM[aves.MM$Habitat_mixed%in%1,"Arb"] = 1
aves.MM[aves.MM$Habitat_grassland%in%1,"Ter"] = 1
aves.MM[aves.MM$Habitat_heathland%in%1,"Arb"] = 1
aves.MM[aves.MM$Habitat_bushes%in%1,"Arb"] = 1
aves.MM[aves.MM$Habitat_sand%in%1,"Ter"] = 1
aves.MM[aves.MM$Habitat_wetlands%in%1,"Aqu"] = 1
aves.MM[aves.MM$Habitat_water%in%1,"Aqu"] = 1
aves.MM[aves.MM$Habitat_rocks%in%1,"Ter"] = 1
aves.MM[aves.MM$Habitat_pioneer%in%1,"Ter"] = 1
l.temp = aves.MM[,c("Ter","Aqu","Arb","Aer")]
l.temp [is.na(l.temp)] = 0
l.temp[apply(l.temp,1,function(x) all(x%in%0)),] = NA
aves.MM[,c("Ter","Aqu","Arb","Aer")] = l.temp

# Aquatic, Terrestrial or Marine
# EcoFresh, EcoTer, EcoMarine columns not necessary

# Fine tune Ter/Aqu info (c)
aves.MM = aves.MM[order(aves.MM$species),]
mf1 = merge(aves.MM,antoine.TerAqu,by="species",all.x=TRUE)
all(aves.MM$species == mf1$species) # auto check
aves.MM[mf1$scheme%in%"terrestrial"&is.na(aves.MM$Ter),"Ter"] = 1
aves.MM[mf1$scheme%in%"aquatic",c("Aqu","EcoFresh")] = 1
aves.MM[aves.MM$EcoFresh%in%1,"Aqu"] = 1

# Reorder & Save
hab.n = grep("Habitat_",names(aves.MM))
eco.n = grep("Eco",names(aves.MM))
col.ref = c("species","scientificName","taxonRank","gbif_status","gbif_accepted","genus","family","order",
	"class","phylum","group","Source","originCH","originEUR","swissRedListCategory","europeanRegionalRedListCategory",
	"iucnRedListCategory","EcoTer","EcoFresh","EcoMar","Fos","Ter","Aqu","Arb","Aer",
	names(aves.MM[,hab.n]),"RangeSize_km2")
aves.MM = aves.MM[,c(col.ref,names(aves.MM)[!names(aves.MM)%in%col.ref])]
write.table(aves.MM,"outputs/raw_traits/non_formated/s2z_raw_traits_aves.txt",row.names=FALSE)


### ==========================================================================================================================
### Fishes
### ==========================================================================================================================


# # Open the main traits sources (fresh water ecology?)
# fish.us = as.data.frame(read_excel("./fishes/FishTraits/FishTraits_14.3.xls"))
# fish.us[fish.us==-555] = NA
# fish.us[fish.us==-999] = NA
# fish.us[fish.us==-1] = NA
# fish.us = fish.us[fish.us$NATIVE%in%1,]
# fish.fwe = as.data.frame(read_excel("./fishes/FWE/Freshwaterecology.info.xlsx"),1)
# fish.brosse = read.csv2("./fishes/FishMorph/FISHMORPH_Database.csv")

# # Open range km2
# fish.range = iucn.ranges[grepl("FW_FISH",names(iucn.ranges))][[1]]

# # Remove columns we do not want
# fish.us[,c("SID","ALTSID","NOTES","FID","GID","ITISTSN","COMMONNAME","OTHERNAMES","FamilyNumber",
# 	"NATIVE","NONFEED","C1_3_4_C24","SERIAL","JAN","FEB","MAR","APR","MAY","JUN","JUL",
# 	"AUG","SEP","OCT","NOV","DEC","SEASON","REPSTATE"," C1_3_4_C24","LONGCENTROID",
# 	"LATCENTROID","MUCK","CLAYSILT","SAND","GRAVEL","COBBLE","BOULDER","BEDROCK","VEGETAT",
# 	"DEBRDETR","LWD","PREFLOT","PREFLEN","POTANADR","LACUSTRINE","LOWLAND","UPLAND",
# 	"MONTANE","LARGERIV","SMALLRIV","SPRGSUBT","CREEK","PERIMETER","PATCHES","LATRANGE",
# 	"LONRANGE","REASON","LISTED","LIST1","LIST2","LIST3","LIST4","LIST5","EURYHALINE","EXTINCT")] = NULL
# fish.fwe[,c("Taxagroup","type")] = NULL
# fish.brosse[,c("Suporder","SpecCode","Type.of.illustration","Reference")] = NULL

# # Add logical ones according to previous format and change names
# fish.us$species = paste(fish.us$GENUS,fish.us$SPECIES)
# fish.us$SPECIES = NULL
# fish.us = fish.us[!duplicated(fish.us$species),]
# names(fish.brosse)[3] = "species"
# fish.brosse$Fos = 0
# fish.brosse$Ter = 0
# fish.brosse$Aqu = 1
# fish.brosse$Arb = 0
# fish.brosse$Aer = 0
# names(fish.us)[12] = "BodyLength_max"
# names(fish.brosse)[4] = "BodyLength_max"
# fish.us$BodyLength_max = as.numeric(fish.us$BodyLength_max)*10
# fish.brosse$BodyLength_max = as.numeric(fish.brosse$BodyLength_max)*10

# # Improve the FWE format
# 	# Binary to factor
# fish.fwe$Migration_dist = NA
# fish.fwe[fish.fwe$short%in%1,"Migration_dist"] = "short"
# fish.fwe[fish.fwe$medium%in%1,"Migration_dist"] = "medium"
# fish.fwe[fish.fwe$long%in%1,"Migration_dist"] = "long"
# fish.fwe$Habitat = NA
# fish.fwe[fish.fwe$pel%in%1,"Habitat"] = "pelagic"
# fish.fwe[fish.fwe$ben%in%1,"Habitat"] = "bentic"
# fish.fwe[fish.fwe$dem%in%1,"Habitat"] = "demersal"
# fish.fwe$Salt_habitat = NA
# fish.fwe[fish.fwe$fre%in%1,"Salt_habitat"] = "fre"
# fish.fwe[fish.fwe$fbr%in%1,"Salt_habitat"] = "frb"
# fish.fwe[fish.fwe$brm%in%1,"Salt_habitat"] = "brm"
# fish.fwe[fish.fwe$fbm%in%1,"Salt_habitat"] = "fbm"
# fish.fwe[fish.fwe$fma%in%1,"Salt_habitat"] = "fma"
# fish.fwe$CAT_Longevity_max_y = NA
# fish.fwe[fish.fwe$ls1%in%1,"CAT_Longevity_max_y"] = "ls1"
# fish.fwe[fish.fwe$ls2%in%1,"CAT_Longevity_max_y"] = "ls2"
# fish.fwe[fish.fwe$ls3%in%1,"CAT_Longevity_max_y"] = "ls3"
# fish.fwe$CAT_Age_at_maturity_y = NA
# fish.fwe[fish.fwe$ma1%in%1,"CAT_Age_at_maturity_y"] = "ma1"
# fish.fwe[fish.fwe$ma2%in%1,"CAT_Age_at_maturity_y"] = "ma2"
# fish.fwe[fish.fwe$ma3%in%1,"CAT_Age_at_maturity_y"] = "ma3"
# fish.fwe[fish.fwe$ma4%in%1,"CAT_Age_at_maturity_y"] = "ma4"
# fish.fwe[fish.fwe$ma5%in%1,"CAT_Age_at_maturity_y"] = "ma5"
# fish.fwe$CAT_Litter_size = NA
# fish.fwe[fish.fwe$fe1%in%1,"CAT_Litter_size"] = "fe1"
# fish.fwe[fish.fwe$fe2%in%1,"CAT_Litter_size"] = "fe2"
# fish.fwe[fish.fwe$fe3%in%1,"CAT_Litter_size"] = "fe3"
# fish.fwe[,c("short","medium","long","pel","ben","dem","fre","fbr","brm","fbm","fma",
# 	"ls1","ls2","ls3","ma1","ma2","ma3","ma4","ma5","fe1","fe2","fe3")] = NULL
# 	# Create new columns
# fish.fwe$Diet_Phyto = NA
# fish.fwe$Diet_Plant = NA
# fish.fwe$Diet_Scav = NA
# fish.fwe$Diet_Inv = NA
# fish.fwe$Diet_Vert = NA
# fish.fwe$Diet_Eggs = NA
# fish.fwe$Diet_Parasit = NA
# fish.fwe$Diet_Other = NA
# 	# Correct categories
# fish.fwe[fish.fwe$feeding.diet.adult%in%"invert","feeding.diet.adult"] = "Carnivorous"
# fish.fwe[fish.fwe$feeding.diet.adult%in%"pisci","feeding.diet.adult"] = "Carnivorous"
# fish.fwe[fish.fwe$feeding.diet.adult%in%"detri","feeding.diet.adult"] = "Scavenger"
# fish.fwe[fish.fwe$feeding.diet.adult%in%"phyto","feeding.diet.adult"] = "Herbivorous"
# fish.fwe[fish.fwe$feeding.diet.adult%in%"parasit","feeding.diet.adult"] = "Parasitic"
# fish.fwe[fish.fwe$feeding.diet.adult%in%c("plankt","planct"),"feeding.diet.adult"] = NA
# fish.fwe[fish.fwe$feeding.diet.adult%in%"omni","feeding.diet.adult"] = "Omnivorous"
# fish.fwe[fish.fwe$rheophily.habitat%in%c("rheo A","rheo B"),"rheophily.habitat"] = "rheo"
# fish.fwe[fish.fwe$rheophily.habitat%in%c("rheo A","rheo B"),"rheophily.habitat"] = "rheo"
# fish.fwe[fish.fwe$rheophily.habitat%in%c("rheo A","rheo B"),"rheophily.habitat"] = "rheo"
# 	# Change column names
# names(fish.fwe)[c(2:8)] = c("species","Migration_type","Flow_habitat",
# 	"Feeding_habitat","Reprod_habitat","Spawning_habitat","Diet")

# # Improve the US format (close to FWE)
# 	# Binary to factor
# fish.us$Feeding_habitat = NA
# fish.us[fish.us$BENTHIC%in%1&fish.us$SURWCOL%in%0,"Feeding_habitat"] = "ben"
# fish.us[fish.us$BENTHIC%in%0&fish.us$SURWCOL%in%1,"Feeding_habitat"] = "wc"
# fish.us$Flow_habitat = NA
# fish.us[fish.us$SLOWCURR%in%1,"Flow_habitat"] = "limno"
# fish.us[fish.us$MODCURR%in%1,"Flow_habitat"] = "eury"
# fish.us[fish.us$FASTCURR%in%1,"Flow_habitat"] = "rheo"
# fish.us$Habitat = NA
# fish.us[fish.us$PELAGIC%in%1,"Habitat"] = "pelagic"
# fish.us[fish.us$BENTHIC%in%1,"Habitat"] = "bentic"
# fish.us$Reprod_habitat = NA
# fish.us[fish.us$A_1_1%in%1,"Reprod_habitat"] = "pel"
# fish.us[fish.us$A_1_2%in%1,"Reprod_habitat"] = "lthpel"
# fish.us[fish.us$A_1_3A%in%1,"Reprod_habitat"] = "lith"
# fish.us[fish.us$A_1_3B%in%1,"Reprod_habitat"] = "lith"
# fish.us[fish.us$A_1_3C%in%1,"Reprod_habitat"] = "lith"
# fish.us[fish.us$A_1_4%in%1,"Reprod_habitat"] = "phylith"
# fish.us[fish.us$A_1_5%in%1,"Reprod_habitat"] = "phy"
# fish.us[fish.us$A_1_6%in%1,"Reprod_habitat"] = "psam"
# fish.us[fish.us$A_2_3A%in%1,"Reprod_habitat"] = "lith"
# fish.us[fish.us$A_2_3B%in%1,"Reprod_habitat"] = "lith"
# fish.us[fish.us$A_2_3C%in%1,"Reprod_habitat"] = "lith"
# fish.us[fish.us$A_2_4A%in%1,"Reprod_habitat"] = "spel"
# fish.us[fish.us$A_2_4C%in%1,"Reprod_habitat"] = "spel"
# fish.us[fish.us$B_1_3A%in%1,"Reprod_habitat"] = "lith"
# fish.us[fish.us$B_1_4%in%1,"Reprod_habitat"] = "phy"
# fish.us[fish.us$B_2_2%in%1,"Reprod_habitat"] = "poly"
# fish.us[fish.us$B_2_3A%in%1,"Reprod_habitat"] = "lith"
# fish.us[fish.us$B_2_3B%in%1,"Reprod_habitat"] = "lith"
# fish.us[fish.us$B_2_4%in%1,"Reprod_habitat"] = "ariad"
# fish.us[fish.us$B_2_5%in%1,"Reprod_habitat"] = "phy"
# fish.us[fish.us$B_2_6%in%1,"Reprod_habitat"] = "psam"
# fish.us[fish.us$B_2_7A%in%1,"Reprod_habitat"] = "spel"
# fish.us[fish.us$B_2_7B%in%1,"Reprod_habitat"] = "spel"
# fish.us[fish.us$B_2_7C%in%1,"Reprod_habitat"] = "spel"
# fish.us[,c("BENTHIC","SURWCOL","SLOWCURR","MODCURR","FASTCURR","PELAGIC",
# 	names(fish.us)[grepl("A_|B_",names(fish.us))])] = NULL
# names(fish.us)[names(fish.us)%in%"ALGPHYTO"] = "Diet_Phyto"
# names(fish.us)[names(fish.us)%in%"MACVASCU"] = "Diet_Plant"
# names(fish.us)[names(fish.us)%in%"DETRITUS"] = "Diet_Scav"
# names(fish.us)[names(fish.us)%in%"INVLVFSH"] = "Diet_Inv"
# names(fish.us)[names(fish.us)%in%"FSHCRCRB"] = "Diet_Vert"
# names(fish.us)[names(fish.us)%in%"EGGS"] = "Diet_Eggs"
# names(fish.us)[names(fish.us)%in%"BLOOD"] = "Diet_Parasit"
# names(fish.us)[names(fish.us)%in%"OTHER"] = "Diet_Other"
# 	# Correct categories
# fish.us$Diet = NA
# herbivore = c("Diet_Phyto","Diet_Plant")
# carnivore = c("Diet_Inv","Diet_Vert","Diet_Eggs")
# fish.us[apply(fish.us[,herbivore],1,sum)%in%0,"Diet"] = "Carnivorous"
# fish.us[apply(fish.us[,carnivore],1,sum)%in%0,"Diet"] = "Herbivorous"
# fish.us[fish.us$Diet_Scav%in%1 & fish.us$Diet%in%c("Carnivorous","Herbivorous"),"Diet"] = "Scavenger"
# fish.us[fish.us$Diet_Parasit%in%1,"Diet"] = "Parasitic"
# fish.us[is.na(fish.us$Diet),"Diet"] = "Omnivorous"
# 	# Change column names
# names(fish.us)[11:16] = c("Age_at_maturity_y","Longevity_max_y","Litter_size","MinTemp","MaxTemp","RangeSize_km2")

# # Merge FWE/US and harmonize (columns order too)
# fish.dat = merge(fish.fwe,fish.us,by="species",all=TRUE)
# fish.dat[is.na(fish.dat$Flow_habitat.x),"Flow_habitat.x"] = fish.dat[is.na(fish.dat$Flow_habitat.x),"Flow_habitat.y"]
# fish.dat[is.na(fish.dat$Feeding_habitat.x),"Feeding_habitat.x"] = fish.dat[is.na(fish.dat$Feeding_habitat.x),"Feeding_habitat.y"]
# fish.dat[is.na(fish.dat$Reprod_habitat.x),"Reprod_habitat.x"] = fish.dat[is.na(fish.dat$Reprod_habitat.x),"Reprod_habitat.y"]
# fish.dat[is.na(fish.dat$Diet.x),"Diet.x"] = fish.dat[is.na(fish.dat$Diet.x),"Diet.y"]
# fish.dat[is.na(fish.dat$Habitat.x),"Habitat.x"] = fish.dat[is.na(fish.dat$Habitat.x),"Habitat.y"]
# fish.dat[is.na(fish.dat$Diet_Phyto.x),"Diet_Phyto.x"] = fish.dat[is.na(fish.dat$Diet_Phyto.x),"Diet_Phyto.y"]
# fish.dat[fish.dat$Diet_Phyto.y%in%1,"Diet_Phyto.x"] = 1
# fish.dat[is.na(fish.dat$Diet_Plant.x),"Diet_Plant.x"] = fish.dat[is.na(fish.dat$Diet_Plant.x),"Diet_Plant.y"]
# fish.dat[fish.dat$Diet_Plant.y%in%1,"Diet_Plant.x"] = 1
# fish.dat[is.na(fish.dat$Diet_Scav.x),"Diet_Scav.x"] = fish.dat[is.na(fish.dat$Diet_Scav.x),"Diet_Scav.y"]
# fish.dat[fish.dat$Diet_Scav.y%in%1,"Diet_Scav.x"] = 1
# fish.dat[is.na(fish.dat$Diet_Inv.x),"Diet_Inv.x"] = fish.dat[is.na(fish.dat$Diet_Inv.x),"Diet_Inv.y"]
# fish.dat[fish.dat$Diet_Inv.y%in%1,"Diet_Inv.x"] = 1
# fish.dat[is.na(fish.dat$Diet_Vert.x),"Diet_Vert.x"] = fish.dat[is.na(fish.dat$Diet_Vert.x),"Diet_Vert.y"]
# fish.dat[fish.dat$Diet_Vert.y%in%1,"Diet_Vert.x"] = 1
# fish.dat[is.na(fish.dat$Diet_Eggs.x),"Diet_Eggs.x"] = fish.dat[is.na(fish.dat$Diet_Eggs.x),"Diet_Eggs.y"]
# fish.dat[fish.dat$Diet_Eggs.y%in%1,"Diet_Eggs.x"] = 1
# fish.dat[is.na(fish.dat$Diet_Parasit.x),"Diet_Parasit.x"] = fish.dat[is.na(fish.dat$Diet_Parasit.x),"Diet_Parasit.y"]
# fish.dat[fish.dat$Diet_Parasit.y%in%1,"Diet_Parasit.x"] = 1
# fish.dat[is.na(fish.dat$Diet_Other.x),"Diet_Other.x"] = fish.dat[is.na(fish.dat$Diet_Other.x),"Diet_Other.y"]
# fish.dat[fish.dat$Diet_Other.y%in%1,"Diet_Other.x"] = 1
# fish.dat[,names(fish.dat)[grepl("\\.y",names(fish.dat))]] = NULL
# names(fish.dat) = gsub("\\.x","",names(fish.dat))

# # Do the same with Brosse
# fish.temp = merge(fish.dat,fish.brosse,by="species",all=TRUE)
# fish.temp[is.na(fish.temp$Family.x),"Family.x"] = fish.temp[is.na(fish.temp$Family.x),"Family.y"]
# fish.temp[is.na(fish.temp$BodyLength_max.x),"BodyLength_max.x"] = fish.temp[is.na(fish.temp$BodyLength_max.x),"BodyLength_max.y"]
# fish.temp[,c("Family.y","BodyLength_max.y")] = NULL 
# names(fish.temp)[c(2,23,24)] = c("Family","Genus","BodyLength_max")

# # Keep the IUCN range in priority
# fish.iucn = merge(fish.temp,fish.range[,c("species","range.size.km2")],by="species",all=TRUE)
# fish.iucn[is.na(fish.iucn$range.size.km2),"range.size.km2"] = fish.iucn[is.na(fish.iucn$range.size.km2),"RangeSize_km2"]
# fish.iucn$RangeSize_km2 = fish.iucn$range.size.km2
# fish.iucn$range.size.km2 = NULL

# # Change names if necessary
# tGS = to_GenusSpecies(fish.iucn$species)
# fish.iucn$species = tGS[,"CanonicalName"]
# fish.iucn = fish.iucn[!is.na(fish.iucn$species),]
# # fish.iucn = combine_rows(fish.iucn,"species") # Unique names (no need)
# fish.iucn$IUCN_status = NA

# # Calculate AnnuMeanTemp
# fish.iucn$AnnuMeanTemp = (fish.iucn$MinTemp + fish.iucn$MaxTemp)/2

# # Reorder columns
# fish.vert = fish.iucn[,c("species","Genus","Family","Order","BodyLength_max","BEl","VEp","REs","OGp","RMl","BLs",
# 	"PFv","PFs","CPt","Litter_size","CAT_Litter_size","Age_at_maturity_y","CAT_Age_at_maturity_y","Longevity_max_y",
# 	"CAT_Longevity_max_y","Diet","Diet_Inv","Diet_Vert","Diet_Eggs","Diet_Scav","Diet_Phyto","Diet_Plant",
# 	"Diet_Parasit","Diet_Other","Fos","Ter","Aqu","Arb","Aer","Habitat","Feeding_habitat","Reprod_habitat",
# 	"Spawning_habitat","Flow_habitat","Salt_habitat","MinTemp","MaxTemp","AnnuMeanTemp","Migration_type","Migration_dist",
# 	"RangeSize_km2","IUCN_status")]
# fish.vert[fish.vert==""] = NA

# # Convert to numeric
# non.num = c("species","Genus","Family","Order",names(fish.vert)[grepl("CAT_",names(fish.vert))],"Reprod_habitat",
# 	"Spawning_habitat","Diet","Feeding_habitat","Habitat","Flow_habitat","Migration_type",
# 	"Migration_dist","Salt_habitat","IUCN_status")
# fish.vert[,!names(fish.vert)%in%non.num] = sapply(fish.vert[,!names(fish.vert)%in%non.num],as.numeric)

# # Fill in additonal logical information
# fish.vert$Fos = 0
# fish.vert$Ter = 0
# fish.vert$Aqu = 1
# fish.vert$Arb = 0
# fish.vert$Aer = 0

# # Add gbif information
# fish.vert[,GB.col] = NA
# for (i in 1:nrow(fish.vert)){
#     cat("...sp.",i)
#     gbif.search = get_status(fish.vert[i,"species"],phylum="Chordata") # Search for accepted names
#     if (!is.null(gbif.search)){
#     	fish.vert$gbif_accepted[i] = gbif.search$scientificName[1]
#      	fish.vert$gbif_rank[i] = gbif.search$rank[1]
#         fish.vert$gbif_phylum[i] = gbif.search$Phylum[1]
#         fish.vert$gbif_genus[i] = gbif.search$Genus[1]
#         fish.vert$gbif_order[i] = gbif.search$Order[1]
#         fish.vert$gbif_family[i] = gbif.search$Family[1]
#         fish.vert$gbif_class[i] = gbif.search$Class[1]
#         fish.vert$gbif_IUCN[i] = gbif.search$IUCN_status[1]
#     }
# }
# write.table(fish.vert,"temp_fish.txt",row.names=FALSE)
fish.vert = read.table("data/temp/temp_fish.txt",header=TRUE)

# Format our table
which.gbif = which(names(fish.vert)%in%GB.col)
fish.vert = fish.vert[,c(1,which.gbif,2:(ncol(fish.vert)-length(which.gbif)))]
fish.vert[is.na(fish.vert$gbif_genus),"gbif_genus"] = fish.vert[is.na(fish.vert$gbif_genus),"Genus"]
fish.vert$gbif_genus[is.na(fish.vert$gbif_genus)] =
	sapply(strsplit(fish.vert$species[is.na(fish.vert$gbif_genus)]," "),function(x) x[1])
fish.vert[is.na(fish.vert$gbif_family),"gbif_family"] = fish.vert[is.na(fish.vert$gbif_family),"Family"]
fish.vert[is.na(fish.vert$gbif_order),"gbif_order"] = fish.vert[is.na(fish.vert$gbif_order),"Order"]
fish.vert[fish.vert$gbif_order%in%"Petromyzontiformes","gbif_class"] = "Petromyzonti" # Was already informed in GBIF
fish.vert[is.na(fish.vert$gbif_class),"gbif_class"] = "Actinopterygii"
fish.vert$gbif_phylum = "Chordata"
fish.vert[is.na(fish.vert$IUCN_status),"IUCN_status"] = fish.vert[is.na(fish.vert$IUCN_status),"gbif_IUCN"]
fish.vert[,c("Genus","Family","Order","gbif_IUCN")] = NULL
names(fish.vert)[2:length(GB.col)] = new.col
gbif.cond = fish.vert$IUCN_status%in%c("NOT_FOUND","NOT_EVALUATED")
fish.vert[gbif.cond,"IUCN_status"] = NA

# Extract EU checklist and merge
s2z.fish = s2z.sp[s2z.sp$group%in%"Freshwater_fishes",]
fish.merge1 = merge(s2z.fish,fish.vert,by="species")
fish.vert = fish.vert[!fish.vert$species%in%fish.merge1$species,]
s2z.fish2 = s2z.fish[!s2z.fish$species%in%fish.merge1$species,]

# Regroup all the available information for the accepted species (mean when continuous values)
mam.ndup = fish.vert[!(duplicated(fish.vert$gbif_accepted)|duplicated(fish.vert$gbif_accepted,fromLast=TRUE)),]
mam.dup = fish.vert[duplicated(fish.vert$gbif_accepted)|duplicated(fish.vert$gbif_accepted,fromLast=TRUE),]
mam.homo = combine_rows(df=mam.dup,ref.col="gbif_accepted",binary=FALSE)
mam.homo = mam.homo[!is.na(mam.homo$gbif_accepted),]

# Combine, attempt another merge and rbind both results
mam.temp = rbind(mam.ndup,mam.homo)
fish.merge2 = merge(s2z.fish2,mam.temp,by="gbif_accepted",all.x=TRUE)
names(fish.merge2)[1:2] = c("gbif_accepted.x","species")
fish.merge2$gbif_accepted.y = fish.merge2$gbif_accepted.x
fish.merge2 = fish.merge2[,names(fish.merge1)]
fish.MM = rbind(fish.merge1,fish.merge2)

# Try to fill in mising information fron one column to another
fish.MM[is.na(fish.MM$genus),"genus"] = fish.MM[is.na(fish.MM$genus),"Genus"]
fish.MM[is.na(fish.MM$family),"family"] = fish.MM[is.na(fish.MM$family),"Family"]
fish.MM[is.na(fish.MM$order),"order"] = fish.MM[is.na(fish.MM$order),"Order"]
fish.MM[is.na(fish.MM$class),"class"] = fish.MM[is.na(fish.MM$class),"Class"]
fish.MM[is.na(fish.MM$phylum),"phylum"] = fish.MM[is.na(fish.MM$phylum),"Phylum"]
fish.MM[grepl("unresolved_accepted",fish.MM$gbif_accepted.x),"gbif_accepted.x"] =
	fish.MM[grepl("unresolved_accepted",fish.MM$gbif_accepted.x),"gbif_accepted.y"]
fish.MM$gbif_accepted.x[is.na(fish.MM$gbif_accepted.x)] =
	paste0("unresolved_accepted2_", seq_len(sum(is.na(fish.MM$gbif_accepted.x))))
fish.MM[is.na(fish.MM$taxonRank),"taxonRank"] = fish.MM[is.na(fish.MM$taxonRank),"gbif_rank"]
fish.MM[is.na(fish.MM$iucnRedListCategory),"iucnRedListCategory"] =
	fish.MM[is.na(fish.MM$iucnRedListCategory),"IUCN_status"]
toRemove = grepl("\\.y",names(fish.MM)) |
	names(fish.MM)%in%c("Genus","Family","Order","Class","Phylum","gbif_rank","IUCN_status")
fish.MM[,toRemove] = NULL
names(fish.MM)[1:ncol(s2z.fish2)] = names(s2z.fish2)

# Fine tune Ter/Aqu/Arb info (a)
guild.ter = apply(fish.MM[,sprintf("GUILDE.%d",c(3,10,12,14:15,20:22,24:25))],1,sum)
fish.MM[guild.ter>0&!is.na(guild.ter),"Ter"] = 1
guild.aqu = apply(fish.MM[,sprintf("GUILDE.%d",c(1,2,4:9,26))],1,sum)
fish.MM[guild.aqu>0&!is.na(guild.aqu),"Aqu"] = 1
guild.arb = apply(fish.MM[,sprintf("GUILDE.%d",c(11,13,16:19,23))],1,sum)
fish.MM[guild.arb>0&!is.na(guild.arb),"Arb"] = 1
fish.MM[!is.na(guild.arb),c("Ter","Aqu","Arb")][is.na(fish.MM[!is.na(guild.arb),c("Ter","Aqu","Arb")])] = 0

# Fine tune habitat simple
hab.wat = apply(fish.MM[,sprintf("GUILDE.%d",c(1,2,4,5,8))],1,sum)
fish.MM[hab.wat>0 & !is.na(hab.wat),"Habitat_water"] = 1
fish.MM[hab.wat%in%0 & !fish.MM$Habitat_water%in%1,"Habitat_water"] = 0
hab.wet = apply(fish.MM[,sprintf("GUILDE.%d",c(6,7,9,26))],1,sum)
fish.MM[hab.wet>0 & !is.na(hab.wet),"Habitat_wetlands"] = 1
fish.MM[hab.wet%in%0 & !fish.MM$Habitat_wetlands%in%1,"Habitat_wetlands"] = 0
fish.MM[fish.MM[,"GUILDE.21"]>0 & !is.na(fish.MM[,"GUILDE.21"]),"Habitat_rocks"] = 1
fish.MM[fish.MM[,"GUILDE.21"]%in%0 & !fish.MM$Habitat_rocks%in%1,"Habitat_rocks"] = 0
hab.grass = apply(fish.MM[,sprintf("GUILDE.%d",c(14,20))],1,sum)
fish.MM[hab.grass>0 & !is.na(hab.grass),"Habitat_grassland"] = 1
fish.MM[hab.grass%in%0 & !fish.MM$Habitat_grassland%in%1,"Habitat_grassland"] = 0
hab.forest = apply(fish.MM[,sprintf("GUILDE.%d",c(16,17,19))],1,sum)
fish.MM[hab.forest>0 & !is.na(hab.forest),"Habitat_forests"] = 1
fish.MM[hab.forest%in%0 & !fish.MM$Habitat_forests%in%1,"Habitat_forests"] = 0
fish.MM[fish.MM[,"GUILDE.17"]>0 & !is.na(fish.MM[,"GUILDE.17"]),"Habitat_deciduous"] = 1
fish.MM[fish.MM[,"GUILDE.17"]%in%0 & !fish.MM$Habitat_deciduous%in%1,"Habitat_deciduous"] = 0
fish.MM[fish.MM[,"GUILDE.19"]>0 & !is.na(fish.MM[,"GUILDE.19"]),"Habitat_coniferous"] = 1
fish.MM[fish.MM[,"GUILDE.19"]%in%0 & !fish.MM$Habitat_coniferous%in%1,"Habitat_coniferous"] = 0
fish.MM[fish.MM[,"GUILDE.16"]>0 & !is.na(fish.MM[,"GUILDE.16"]),"Habitat_mixed"] = 1
fish.MM[fish.MM[,"GUILDE.16"]%in%0 & !fish.MM$Habitat_mixed%in%1,"Habitat_mixed"] = 0
hab.pioneer = apply(fish.MM[,sprintf("GUILDE.%d",c(3,10,12,22))],1,sum)
fish.MM[hab.pioneer>0 & !is.na(hab.pioneer),"Habitat_pioneer"] = 1
fish.MM[hab.pioneer%in%0 & !fish.MM$Habitat_pioneer%in%1,"Habitat_pioneer"] = 0
hab.urban = apply(fish.MM[,sprintf("GUILDE.%d",c(22,24))],1,sum)
fish.MM[hab.urban>0 & !is.na(hab.urban),"Habitat_urban"] = 1
fish.MM[hab.urban%in%0 & !fish.MM$Habitat_urban%in%1,"Habitat_urban"] = 0
fish.MM[fish.MM[,"GUILDE.23"]>0 & !is.na(fish.MM[,"GUILDE.23"]),"Habitat_green_urban"] = 1
fish.MM[fish.MM[,"GUILDE.23"]%in%0 & !fish.MM$Habitat_green_urban%in%1,"Habitat_green_urban"] = 0
hab.agri = apply(fish.MM[,sprintf("GUILDE.%d",c(11,25))],1,sum)
fish.MM[hab.agri>0 & !is.na(hab.agri),"Habitat_agricultural"] = 1
fish.MM[hab.agri%in%0 & !fish.MM$Habitat_agricultural%in%1,"Habitat_agricultural"] = 0
fish.MM[fish.MM[,"GUILDE.13"]>0 & !is.na(fish.MM[,"GUILDE.13"]),"Habitat_bushes"] = 1
fish.MM[fish.MM[,"GUILDE.13"]%in%0 & !fish.MM$Habitat_bushes%in%1,"Habitat_bushes"] = 0
fish.MM[fish.MM[,"GUILDE.15"]>0 & !is.na(fish.MM[,"GUILDE.15"]),"Habitat_transitional"] = 1
fish.MM[fish.MM[,"GUILDE.15"]%in%0 & !fish.MM$Habitat_transitional%in%1,"Habitat_transitional"] = 0
fish.MM[fish.MM[,"GUILDE.18"]>0 & !is.na(fish.MM[,"GUILDE.18"]),"Habitat_heathland"] = 1
fish.MM[fish.MM[,"GUILDE.18"]%in%0 & !fish.MM$Habitat_heathland%in%1,"Habitat_heathland"] = 0
fish.MM[,HABit[!HABit%in%names(fish.MM)]] = NA
fish.MM[,names(fish.MM)[grepl("GUILD",names(fish.MM))]] = NULL

# Aquatic, Terrestrial or Marine + others
fish.MM$EcoTer = 0
fish.MM$EcoFresh = 1
fish.MM$EcoMar = NA
fish.MM$Fos = 0
fish.MM$Ter = 0
fish.MM$Aqu = 1
fish.MM$Arb = 0
fish.MM$Aer = 0

# Fine tune Ter/Aqu info (b)
fish.MM = fish.MM[order(fish.MM$species),]
mf1 = merge(fish.MM,antoine.TerAqu,by="species",all.x=TRUE)
all(fish.MM$species == mf1$species) # auto check
fish.MM[mf1$scheme%in%"terrestrial"&is.na(fish.MM$Ter),"Ter"] = 1
fish.MM[mf1$scheme%in%"aquatic",c("Aqu","EcoFresh")] = 1
fish.MM[fish.MM$EcoFresh%in%1,"Aqu"] = 1

# Reorder & Save
hab.n = grep("Habitat_",names(fish.MM))
eco.n = grep("Eco",names(fish.MM))
col.ref = c("species","scientificName","taxonRank","gbif_status","gbif_accepted","genus","family","order",
	"class","phylum","group","Source","originCH","originEUR","swissRedListCategory","europeanRegionalRedListCategory",
	"iucnRedListCategory","EcoTer","EcoFresh","EcoMar","Fos","Ter","Aqu","Arb","Aer",
	names(fish.MM[,hab.n]),"RangeSize_km2")
fish.MM = fish.MM[,c(col.ref,names(fish.MM)[!names(fish.MM)%in%col.ref])]
write.table(fish.MM,"outputs/raw_traits/non_formated/s2z_raw_traits_fishes.txt",row.names=FALSE)


### ==========================================================================================================================
### Plants
### ==========================================================================================================================


# # Open the best sources
# plants.eiv = as.data.frame(read_excel("./plants/EIV/Dengler 2023 - EIVE_Paper_1.0_SM_08.xlsx",2))
# plants.diaz = read.table("./plants/Diaz_2020/Species_mean_traits.txt",sep="\t",header=TRUE)
# new.names = c("species","Genus","Family","Tgroup_angio","Tgroup_plant","Habitat","Woodiness","Growth",
# 	"Leaf_type","Leaf_area_mm2","Nmass_mg.g","LMA_g.m2","Height_m","Diaspore_mass_mg","LDMC_g.g","SSD_mg.mm3")
# names(plants.diaz)[c(2,5:11,15,16,18,20,22,24,28,31)] = new.names
# plants.OA = read.table("./plants/OA_all_sp_traits_ecology.txt")
# names(plants.OA)[c(10,12,14,21:24)] = c("Elevation5","Elevation","Elevation95",
# 	"C.N_perct","Height_cm","LDMC_mg.g","SLA_m2.kg")
# plants.OA$Height_cm = plants.OA$Height_cm/100
# plants.OA$LDMC_mg.g = plants.OA$LDMC_mg.g/1000

# # We add flora indicativa
# plants.FI2 = as.data.frame(read_excel("./plants/Floraindicativa/FloraIndicativa_for_R.xlsx",1))
# # without.aggr = gsub(" aggr.","",plants.FI$Taxon)
# # dup.id = duplicated(without.aggr)|duplicated(without.aggr,fromLast=TRUE)
# # FI.nodup = plants.FI[!dup.id,]
# # FI.dup = plants.FI[dup.id,]
# # FI.dup = FI.dup[!grepl("aggr.",FI.dup$Taxon),]
# # plants.FI2 = rbind(FI.nodup,FI.dup)

# # Open dipsersal
# plants.km = as.data.frame(read_excel("./plants/Lososova_2023/Lososova_et_al_2023_Dispersal_version2_2024-06-14.xlsx",1))
# names(plants.km) = gsub(" ",".",names(plants.km))
# plants.km = plants.km[,c("Taxon","Plant.height.(m)","Seed.mass.(mg)","Minimum.dispersal.(m)","Maximal.dispersal.(m)")]
# plants.km[grepl("<|>",plants.km[,2]),2] = NA

# # Add subsp. particule to OA
# subsp.tar = plants.OA[sapply(strsplit(plants.OA$names," "),length)%in%3,"names"]
# plants.OA[sapply(strsplit(plants.OA$names," "),length)%in%3,"names"] =
# 	sapply(strsplit(subsp.tar," "),function(x) paste(x[1],x[2],"subsp.",x[3]))

# # Format categories
# 	# DIAZ
# plants.diaz[plants.diaz$Tgroup_angio%in%"Angiosperm_Magnoliid","Tgroup_angio"] = "Magnoliid"
# plants.diaz[plants.diaz$Tgroup_angio%in%"Angiosperm_Monocotyledon","Tgroup_angio"] = "Monocotyledon"
# plants.diaz[plants.diaz$Tgroup_angio%in%"Angiosperm_other_Eudicotyledon","Tgroup_angio"] = "Eudicotyledon"
# 	#
# plants.diaz[plants.diaz$Woodiness%in%"non.woody","Woodiness"] = "non_Woody"
# plants.diaz[plants.diaz$Woodiness%in%"semi-woody","Woodiness"] = "semi_Woody"
# plants.diaz[plants.diaz$Woodiness%in%"woody","Woodiness"] = "Woody"
# 	#
# plants.diaz[plants.diaz$Growth%in%"bamboo graminoid","Growth"] = "Bamboo"
# plants.diaz[plants.diaz$Growth%in%"climber","Growth"] = "Climber"
# plants.diaz[plants.diaz$Growth%in%"fern","Growth"] = "Fern"
# plants.diaz[plants.diaz$Growth%in%"herbaceous graminoid","Growth"] = "Herb_gram"
# plants.diaz[plants.diaz$Growth%in%"herbaceous non-graminoid","Growth"] = "Herb_non_gram"
# plants.diaz[plants.diaz$Growth%in%"herbaceous non-graminoid/shrub","Growth"] = "Herb_ng_shrub"
# plants.diaz[plants.diaz$Growth%in%"other","Growth"] = "Other_form"
# plants.diaz[plants.diaz$Growth%in%"shrub","Growth"] = "Shrub"
# plants.diaz[plants.diaz$Growth%in%"shrub/tree","Growth"] = "Shrub_tree"
# plants.diaz[plants.diaz$Growth%in%"succulent","Growth"] = "Succulent"
# plants.diaz[plants.diaz$Growth%in%"tree","Growth"] = "Tree"
# 	# 
# plants.diaz[plants.diaz$Leaf_type%in%"broadleaved","Leaf_type"] = "Broadleaved"
# plants.diaz[plants.diaz$Leaf_type%in%"needleleaved","Leaf_type"] = "Needleleaved"
# plants.diaz[plants.diaz$Leaf_type%in%"photosynthetic stem","Leaf_type"] = "Stem_photo"
# plants.diaz[plants.diaz$Leaf_type%in%"scale-shaped","Leaf_type"] = "Scale_shaped"
# plants.diaz[plants.diaz$Leaf_type%in%"scale-shaped/needleleaved","Leaf_type"] = "Scale_needle"
# 	# FLORA INDICATIVA 
# LF.ref = substring(plants.FI2$LF,1,1)
# plants.FI2$LF1 = NA
# plants.FI2[grepl("z",LF.ref),"LF1"] = "Woody_Chamaephyte"
# plants.FI2[grepl("t",LF.ref),"LF1"] = "Therophyte"
# plants.FI2[grepl("s",LF.ref),"LF1"] = "Pleustophyte"
# plants.FI2[grepl("p",LF.ref),"LF1"] = "Phanerophyte"
# plants.FI2[grepl("n",LF.ref),"LF1"] = "Nanophanerophyte"
# plants.FI2[grepl("k",LF.ref),"LF1"] = "ShortLived_Hemicryptophyte"
# plants.FI2[grepl("h",LF.ref),"LF1"] = "LongLived_Hemicryptophyte"
# plants.FI2[grepl("g",LF.ref),"LF1"] = "Geophyte"
# plants.FI2[grepl("f",LF.ref),"LF1"] = "Chama_Hemi"
# plants.FI2[grepl("e",LF.ref),"LF1"] = "Epiphyte"
# plants.FI2[grepl("d",LF.ref),"LF1"] = "Nanopha_Hemi"
# plants.FI2[grepl("c",LF.ref),"LF1"] = "nWoody_Chamaephyte"
# plants.FI2[grepl("a",LF.ref),"LF1"] = "Hydrophyte"
# LF.ref = substring(plants.FI2$LF,2,3)
# plants.FI2$LF2 = NA
# plants.FI2[grepl("-z",LF.ref),"LF2"] = "Woody_Chamaephyte"
# plants.FI2[grepl("-t",LF.ref),"LF2"] = "Therophyte"
# plants.FI2[grepl("-s",LF.ref),"LF2"] = "Pleustophyte"
# plants.FI2[grepl("-p",LF.ref),"LF2"] = "Phanerophyte"
# plants.FI2[grepl("-n",LF.ref),"LF2"] = "Nanophanerophyte"
# plants.FI2[grepl("-k",LF.ref),"LF2"] = "ShortLived_Hemicryptophyte"
# plants.FI2[grepl("-h",LF.ref),"LF2"] = "LongLived_Hemicryptophyte"
# plants.FI2[grepl("-g",LF.ref),"LF2"] = "Geophyte"
# plants.FI2[grepl("-f",LF.ref),"LF2"] = "Chama_Hemi"
# plants.FI2[grepl("-e",LF.ref),"LF2"] = "Epiphyte"
# plants.FI2[grepl("-d",LF.ref),"LF2"] = "Nanopha_Hemi"
# plants.FI2[grepl("-c",LF.ref),"LF2"] = "nWoody_Chamaephyte"
# plants.FI2[grepl("-a",LF.ref),"LF2"] = "Hydrophyte"
# plants.FI2[is.na(plants.FI2$LF2),"LF2"] = "None"
# LF.ref = substring(plants.FI2$LF,1,6)
# plants.FI2$LF3 = NA
# plants.FI2[grepl(".ff",LF.ref),"LF3"] = "Carnivorous"
# plants.FI2[grepl(".hp",LF.ref),"LF3"] = "Semiparasite"
# plants.FI2[grepl(".li",LF.ref),"LF3"] = "Liane"
# plants.FI2[grepl(".sp",LF.ref),"LF3"] = "Saprophyte"
# plants.FI2[grepl(".vp",LF.ref),"LF3"] = "Parasite"
# plants.FI2[is.na(plants.FI2$LF3),"LF3"] = "None"
# 	#
# plants.FI2$DA = str_trim(gsub("At","",plants.FI2$DA))
# DA.ref = substring(plants.FI2$DA,1,2)
# plants.FI2$DA[DA.ref%in%c("","Kd")] = NA
# plants.FI2$DA1 = NA
# plants.FI2[grepl("Au",DA.ref),"DA1"] = "Autochory"
# plants.FI2[grepl("Bo",DA.ref),"DA1"] = "Anemochory"
# plants.FI2[grepl("Dy",DA.ref),"DA1"] = "Dysochory"
# plants.FI2[grepl("En",DA.ref),"DA1"] = "Endochory"
# plants.FI2[grepl("Ep",DA.ref),"DA1"] = "Epichory"
# plants.FI2[grepl("Hy",DA.ref),"DA1"] = "Hydrochory"
# plants.FI2[grepl("Me",DA.ref),"DA1"] = "Meteorochory"
# plants.FI2[grepl("My",DA.ref),"DA1"] = "Myrmecochory"
# DA.ref = substring(plants.FI2$DA,4,7)
# plants.FI2$DA2 = NA
# plants.FI2[grepl("Au",DA.ref),"DA2"] = "Autochory"
# plants.FI2[grepl("Bo",DA.ref),"DA2"] = "Anemochory"
# plants.FI2[grepl("Dy",DA.ref),"DA2"] = "Dysochory"
# plants.FI2[grepl("En",DA.ref),"DA2"] = "Endochory"
# plants.FI2[grepl("Ep",DA.ref),"DA2"] = "Epichory"
# plants.FI2[grepl("Hy",DA.ref),"DA2"] = "Hydrochory"
# plants.FI2[grepl("Me",DA.ref),"DA2"] = "Meteorochory"
# plants.FI2[grepl("My",DA.ref),"DA2"] = "Myrmecochory"
# plants.FI2[is.na(plants.FI2$DA2),"DA2"] = "None"
# 	#
# plants.FI2[plants.FI2$BZ%in%"-","BZ"] = NA
# 	#
# plants.FI2$LR = gsub("\\?","",plants.FI2$LR)
# hab.split = lapply(plants.FI2[,"LR"],function(x) strsplit(x," ")[[1]])
# hab.uniq = sort(unique(unlist(hab.split)))
# plants.FI2[,hab.uniq] = 0
# plants.FI2[is.na(plants.FI2[,"LR"]),hab.uniq] = NA
# for (i in 1:length(hab.uniq)){
# 	h.pos = sapply(hab.split,function(x) any(x%in%hab.uniq[i]))
# 	plants.FI2[h.pos,hab.uniq[i]] = 1
# }

# # Create new columns for Diaz
# plants.diaz$Fos = 0
# plants.diaz$Ter = 0
# plants.diaz$Aqu = 0
# plants.diaz$Arb = 0
# plants.diaz$Aer = 0
# plants.diaz[plants.diaz$Habitat%in%"terrestrial","Ter"] = 1 # "Ter" by deduction because plants
# plants.diaz[plants.diaz$Habitat%in%"aquatic","Aqu"] = 1
# plants.diaz[plants.diaz$Habitat%in%c("aquatic/semiaquatic","semiaquatic"),c("Aqu","Ter")] = 1
# plants.diaz[plants.diaz$Habitat%in%"",c("Fos","Ter","Aqu","Arb","Aer")] = NA

# # Merge diaz with OA data
# y.names = c("names","sp.pH","sp.N","sp.cal","sp.humid","Elevation5","Elevation","Elevation95",
# 	"alt_class2","geo.class","C.N_perct","Height_cm","LDMC_mg.g","SLA_m2.kg")
# plants.dat = merge(plants.diaz[,c(new.names,"Fos","Ter","Aqu","Arb","Aer")],plants.OA[,y.names],
# 	by.x="species",by.y="names",all=TRUE)
# plants.dat[is.na(plants.dat$Height_m),"Height_m"] = plants.dat[is.na(plants.dat$Height_m),"Height_cm"]
# plants.dat[is.na(plants.dat$LDMC_g.g),"LDMC_g.g"] = plants.dat[is.na(plants.dat$LDMC_g.g),"LDMC_mg.g"]
# plants.dat[,c("Height_cm","LDMC_mg.g")] = NULL

# # Merge new outputs with EIV europe
# y.names = c("TaxonConcept","EIVEres-M","EIVEres-M.nw3","EIVEres-N","EIVEres-N.nw3",
# 	"EIVEres-R","EIVEres-R.nw3","EIVEres-L","EIVEres-L.nw3","EIVEres-T","EIVEres-T.nw3")
# plants.dat2 = merge(plants.dat,plants.eiv[,y.names],by.x="species",by.y="TaxonConcept",all=TRUE)
# names(plants.dat2)[c(22:26,28:30,33:42)] = c("EIVoa_pH","EIVoa_Nitrogen","EIVoa_Calcareous","EIVoa_Moisture",
# 	"Elevation_low","Elevation_up","Elev_class","Geo_class","EIVeu_Moisture","EIVeu_M_range","EIVeu_Nitrogen",
# 	"EIVeu_N_range","EIVeu_pH","EIVeu_pH_range","EIVeu_Light","EIVeu_L_range","EIVeu_Temp","EIVeu_T_range")

# # Merge new outputs with Flora Indicativa (we keep LF, WT, DA, BZ, DG + habitat)
# y.names = c("Taxon","Gattung","Familie","LF1","LF2","LF3","WT","DA1","DA2","BZ","DG",hab.uniq)
# plants.dat3 = merge(plants.dat2,plants.FI2[,y.names],by.x="species",by.y="Taxon",all=TRUE)
# plants.dat3[is.na(plants.dat3$Genus),"Genus"] = plants.dat3[is.na(plants.dat3$Genus),"Gattung"]
# plants.dat3[is.na(plants.dat3$Family),"Family"] = plants.dat3[is.na(plants.dat3$Family),"Familie"]
# plants.dat3[,c("Gattung","Familie")] = NULL
# names(plants.dat3)[43:50] = c("Life_form1","Life_form2","Life_form3","Root_depth",
# 	"Diaspore_dispersal1","Diaspore_dispersal2","Flowering_months","Pop_config")
# names(plants.dat3)[names(plants.dat3)%in%hab.uniq] = paste0("HABfi_",names(plants.dat3)[names(plants.dat3)%in%hab.uniq])

# # Keep the IUCN range in priority
# plants.range = iucn.ranges[grepl("PLANTS",names(iucn.ranges))]
# plants.range = do.call("rbind",plants.range)
# plants.range = plants.range[!duplicated(plants.range$species),]
# plants.iucn = merge(plants.dat3,plants.range[,c("species","range.size.km2")],by="species",all=TRUE)
# plants.iucn$RangeSize_km2 = plants.iucn$range.size.km2

# # Plant dispersal
# plants.disp = merge(plants.iucn,plants.km,by.x="species",by.y="Taxon",all=TRUE)
# plants.disp[is.na(plants.disp$Height_m),"Height_m"] = plants.disp[is.na(plants.disp$Height_m),"Plant.height.(m)"]
# plants.disp[is.na(plants.disp$Diaspore_mass_mg),"Diaspore_mass_mg"] = plants.disp[is.na(plants.disp$Diaspore_mass_mg),"Seed.mass.(mg)"]

# # Change names if necessary
# tGS = to_GenusSpecies(plants.disp$species)
# plants.disp$species = tGS[,"CanonicalName"]
# plants.disp = plants.disp[!is.na(plants.disp$species),]
# plants.disp$IUCN_status = NA
# names(plants.disp)[grepl("dispersal\\.",names(plants.disp))] = c("Dispersal_min_km","Dispersal_max_km")
# plants.disp$Dispersal_min = plants.disp$Dispersal_min_km/1000
# plants.disp$Dispersal_max = plants.disp$Dispersal_max_km/1000
# plants.disp$Dispersal_km = rowMeans(cbind(plants.disp$Dispersal_min,plants.disp$Dispersal_max),na.rm=TRUE)
# plants.disp2 = combine_rows(plants.disp,"species") # Unique names now

# # Reorder columns
# plants.vert = plants.disp2[,c("species","Genus","Family","Tgroup_angio","Tgroup_plant","Life_form1","Life_form2",
# 	"Life_form3","Woodiness","Growth","Height_m","Root_depth","Leaf_type","Leaf_area_mm2","LMA_g.m2","SLA_m2.kg",
# 	"SSD_mg.mm3","LDMC_g.g","Nmass_mg.g","C.N_perct","Diaspore_mass_mg","Diaspore_dispersal1","Diaspore_dispersal2",
# 	"Flowering_months","Pop_config","Habitat","Fos","Ter","Aqu","Arb","Aer",
# 	names(plants.disp2)[grepl("HAB",names(plants.disp2))],"Geo_class","EIVoa_Moisture","EIVeu_Moisture",
# 	"EIVeu_M_range","EIVoa_pH","EIVeu_pH","EIVeu_pH_range","EIVoa_Nitrogen","EIVeu_Nitrogen","EIVeu_N_range",
# 	"EIVoa_Calcareous","EIVeu_Temp","EIVeu_T_range","EIVeu_Light","EIVeu_L_range","Elev_class","Elevation_low",
# 	"Elevation","Elevation_up","Dispersal_min","Dispersal_km","Dispersal_max","RangeSize_km2","IUCN_status")]
# plants.vert[plants.vert==""] = NA

# # Convert to numeric
# non.num = c("species","Genus","Family","Tgroup_angio","Tgroup_plant","Leaf_type","Life_form1","Life_form2",
# 	"Life_form3","Root_depth","Diaspore_dispersal1","Diaspore_dispersal2","Flowering_months","Pop_config",
# 	"Woodiness","Growth","Elev_class","Geo_class","IUCN_status","Habitat")
# plants.vert[,!names(plants.vert)%in%non.num] = sapply(plants.vert[,!names(plants.vert)%in%non.num],as.numeric)

# # Fill in additonal logical information
# plants.vert$Fos = NA
# plants.vert$Arb = NA
# plants.vert$Aer = 0

# Add gbif information
# plants.vert[,GB.col] = NA
# for (i in 1:nrow(plants.vert)){
#     cat("...sp.",i)
#     gbif.search = get_status(plants.vert[i,"species"],phylum="Tracheophyta") # Search for accepted names
#     if (!is.null(gbif.search)){
#      	plants.vert$gbif_accepted[i] = gbif.search$scientificName[1] # Fill in the result
#     	plants.vert$gbif_rank[i] = gbif.search$rank[1]
#         plants.vert$gbif_phylum[i] = gbif.search$Phylum[1]
#         plants.vert$gbif_genus[i] = gbif.search$Genus[1]
#         plants.vert$gbif_order[i] = gbif.search$Order[1]
#         plants.vert$gbif_family[i] = gbif.search$Family[1]
#         plants.vert$gbif_class[i] = gbif.search$Class[1]
#         plants.vert$gbif_IUCN[i] = gbif.search$IUCN_status[1]
#     }
# }
# write.table(plants.vert,"temp_plants.txt",row.names=FALSE)
plants.vert = read.table("data/temp/temp_plants.txt",header=TRUE)

# Remove the Mosses
bryo.vert = read.table("data/temp/temp_bryo.txt",header=TRUE)
plants.vert = plants.vert[!plants.vert$species%in%bryo.vert$species,]

# Format our table
which.gbif = which(names(plants.vert)%in%GB.col)
plants.vert = plants.vert[,c(1,which.gbif,2:(ncol(plants.vert)-length(which.gbif)))]
plants.vert[is.na(plants.vert$gbif_genus),"gbif_genus"] = plants.vert[is.na(plants.vert$gbif_genus),"Genus"]
plants.vert$gbif_genus[is.na(plants.vert$gbif_genus)] =
	sapply(strsplit(plants.vert$species[is.na(plants.vert$gbif_genus)]," "),function(x) x[1])
plants.vert[is.na(plants.vert$gbif_family),"gbif_family"] = plants.vert[is.na(plants.vert$gbif_family),"Family"]
plants.vert[is.na(plants.vert$IUCN_status),"IUCN_status"] = plants.vert[is.na(plants.vert$IUCN_status),"gbif_IUCN"]
plants.vert[,c("Genus","Family","Order","Phylum","gbif_IUCN")] = NULL
names(plants.vert)[2:length(GB.col)] = new.col
gbif.cond = plants.vert$IUCN_status%in%c("NOT_FOUND","NOT_EVALUATED")
plants.vert[gbif.cond,"IUCN_status"] = NA

# Extract EU checklist and merge
s2z.plants = s2z.sp[s2z.sp$group%in%"Vascular_plants",]
plants.merge1 = merge(s2z.plants,plants.vert,by="species")
plants.vert = plants.vert[!plants.vert$species%in%plants.merge1$species,]
s2z.plants2 = s2z.plants[!s2z.plants$species%in%plants.merge1$species,]

# Regroup all the available information for the accepted species (mean when continuous values)
mam.ndup = plants.vert[!(duplicated(plants.vert$gbif_accepted)|duplicated(plants.vert$gbif_accepted,fromLast=TRUE)),]
mam.dup = plants.vert[duplicated(plants.vert$gbif_accepted)|duplicated(plants.vert$gbif_accepted,fromLast=TRUE),]
mam.homo = combine_rows(df=mam.dup,ref.col="gbif_accepted",binary=FALSE)
mam.homo = mam.homo[!is.na(mam.homo$gbif_accepted),]

# Combine, attempt another merge and rbind both results
mam.temp = rbind(mam.ndup,mam.homo)
plants.merge2 = merge(s2z.plants2,mam.temp,by="gbif_accepted",all.x=TRUE)
names(plants.merge2)[1:2] = c("gbif_accepted.x","species")
plants.merge2$gbif_accepted.y = plants.merge2$gbif_accepted.x
plants.merge2 = plants.merge2[,names(plants.merge1)]
plants.MM = rbind(plants.merge1,plants.merge2)

# Add missing taxo
plants.MM[plants.MM$genus %in% "Anemome", c("phylum", "class", "order", "family")] =
  data.frame("Tracheophyta", "Magnoliopsida", "Ranunculales", "Ranunculaceae")
plants.MM[plants.MM$genus %in% "Anythyllis", c("phylum", "class", "order", "family")] =
  data.frame("Tracheophyta", "Magnoliopsida", "Fabales", "Fabaceae")
plants.MM[plants.MM$genus %in% "Hieraciurn", c("phylum", "class", "order", "family")] =
  data.frame("Tracheophyta", "Magnoliopsida", "Asterales", "Asteraceae")
plants.MM[plants.MM$genus %in% "Isotoma", c("phylum", "class", "order", "family")] =
  data.frame("Tracheophyta", "Magnoliopsida", "Asterales", "Campanulaceae")
plants.MM[plants.MM$genus %in% "Symphotrichum", c("phylum", "class", "order", "family")] =
  data.frame("Tracheophyta", "Magnoliopsida", "Asterales", "Asteraceae")

# Try to fill in mising information fron one column to another
plants.MM[is.na(plants.MM$genus),"genus"] = plants.MM[is.na(plants.MM$genus),"Genus"]
plants.MM[is.na(plants.MM$family),"family"] = plants.MM[is.na(plants.MM$family),"Family"]
plants.MM[is.na(plants.MM$order),"order"] = plants.MM[is.na(plants.MM$order),"Order"]
plants.MM[is.na(plants.MM$class),"class"] = plants.MM[is.na(plants.MM$class),"Class"]
plants.MM[is.na(plants.MM$phylum),"phylum"] = plants.MM[is.na(plants.MM$phylum),"Phylum"]
plants.MM[grepl("unresolved_accepted",plants.MM$gbif_accepted.x),"gbif_accepted.x"] =
	plants.MM[grepl("unresolved_accepted",plants.MM$gbif_accepted.x),"gbif_accepted.y"]
plants.MM$gbif_accepted.x[is.na(plants.MM$gbif_accepted.x)] =
	paste0("unresolved_accepted2_", seq_len(sum(is.na(plants.MM$gbif_accepted.x))))
plants.MM[is.na(plants.MM$taxonRank),"taxonRank"] = plants.MM[is.na(plants.MM$taxonRank),"gbif_rank"]
plants.MM[is.na(plants.MM$iucnRedListCategory),"iucnRedListCategory"] =
	plants.MM[is.na(plants.MM$iucnRedListCategory),"IUCN_status"]
toRemove = grepl("\\.y",names(plants.MM)) |
	names(plants.MM)%in%c("Genus","Family","Order","Class","Phylum","gbif_rank","IUCN_status")
plants.MM[,toRemove] = NULL
names(plants.MM)[1:ncol(s2z.plants2)] = names(s2z.plants2)

# Fine tune Ter/Aqu/Arb info (a)
guild.ter = apply(plants.MM[,sprintf("GUILDE.%d",c(3,10,12,14:15,20:22,24:25))],1,sum)
plants.MM[guild.ter>0&!is.na(guild.ter),"Ter"] = 1
guild.aqu = apply(plants.MM[,sprintf("GUILDE.%d",c(1,2,4:9,26))],1,sum)
plants.MM[guild.aqu>0&!is.na(guild.aqu),"Aqu"] = 1
guild.arb = apply(plants.MM[,sprintf("GUILDE.%d",c(11,13,16:19,23))],1,sum)
plants.MM[guild.arb>0&!is.na(guild.arb),"Arb"] = 1

# Fine tune Ter/Aqu/Arb info (b)
guild.ter = apply(plants.MM[,paste0("HABfi_",c("2.1","2.2","2.3","2.4","2.6","2.9","3.1","3.2","3.3","3.4","3.5","6.1","6.2","6.3","6.4","6.5","6.6","6.7","7.4"))],1,sum)
plants.MM[guild.ter>0&!is.na(guild.ter),"Ter"] = 1
guild.aqu = apply(plants.MM[,paste0("HABfi_",c("1.1","1.2","1.3","1.4","2.5","4.1","4.2","5.1","5.2","7.5"))],1,sum)
plants.MM[guild.aqu>0&!is.na(guild.aqu),"Aqu"] = 1
guild.arb = apply(plants.MM[,paste0("HABfi_",c("2.7","2.8","7.1","7.2","7.3","7.6","8.1","8.2","9.1","9.2","9.3","9.4","9.5"))],1,sum)
plants.MM[guild.arb>0&!is.na(guild.arb),"Arb"] = 1
toZ = apply(plants.MM[,c("Ter","Aqu","Arb")],1,function(x) all(is.na(x)))
plants.MM[!toZ,c("Ter","Aqu","Arb")][is.na(plants.MM[!toZ,c("Ter","Aqu","Arb")])] = 0

# Fine tune habitat simple
hab.wat = apply(plants.MM[,sprintf("GUILDE.%d",c(1,2,4,5,8))],1,sum)
plants.MM[hab.wat>0 & !is.na(hab.wat),"Habitat_water"] = 1
plants.MM[hab.wat%in%0 & !plants.MM$Habitat_water%in%1,"Habitat_water"] = 0
hab.wet = apply(plants.MM[,sprintf("GUILDE.%d",c(6,7,9,26))],1,sum)
plants.MM[hab.wet>0 & !is.na(hab.wet),"Habitat_wetlands"] = 1
plants.MM[hab.wet%in%0 & !plants.MM$Habitat_wetlands%in%1,"Habitat_wetlands"] = 0
plants.MM[plants.MM[,"GUILDE.21"]>0 & !is.na(plants.MM[,"GUILDE.21"]),"Habitat_rocks"] = 1
plants.MM[plants.MM[,"GUILDE.21"]%in%0 & !plants.MM$Habitat_rocks%in%1,"Habitat_rocks"] = 0
hab.grass = apply(plants.MM[,sprintf("GUILDE.%d",c(14,20))],1,sum)
plants.MM[hab.grass>0 & !is.na(hab.grass),"Habitat_grassland"] = 1
plants.MM[hab.grass%in%0 & !plants.MM$Habitat_grassland%in%1,"Habitat_grassland"] = 0
hab.forest = apply(plants.MM[,sprintf("GUILDE.%d",c(16,17,19))],1,sum)
plants.MM[hab.forest>0 & !is.na(hab.forest),"Habitat_forests"] = 1
plants.MM[hab.forest%in%0 & !plants.MM$Habitat_forests%in%1,"Habitat_forests"] = 0
plants.MM[plants.MM[,"GUILDE.17"]>0 & !is.na(plants.MM[,"GUILDE.17"]),"Habitat_deciduous"] = 1
plants.MM[plants.MM[,"GUILDE.17"]%in%0 & !plants.MM$Habitat_deciduous%in%1,"Habitat_deciduous"] = 0
plants.MM[plants.MM[,"GUILDE.19"]>0 & !is.na(plants.MM[,"GUILDE.19"]),"Habitat_coniferous"] = 1
plants.MM[plants.MM[,"GUILDE.19"]%in%0 & !plants.MM$Habitat_coniferous%in%1,"Habitat_coniferous"] = 0
plants.MM[plants.MM[,"GUILDE.16"]>0 & !is.na(plants.MM[,"GUILDE.16"]),"Habitat_mixed"] = 1
plants.MM[plants.MM[,"GUILDE.16"]%in%0 & !plants.MM$Habitat_mixed%in%1,"Habitat_mixed"] = 0
hab.pioneer = apply(plants.MM[,sprintf("GUILDE.%d",c(3,10,12,22))],1,sum)
plants.MM[hab.pioneer>0 & !is.na(hab.pioneer),"Habitat_pioneer"] = 1
plants.MM[hab.pioneer%in%0 & !plants.MM$Habitat_pioneer%in%1,"Habitat_pioneer"] = 0
hab.urban = apply(plants.MM[,sprintf("GUILDE.%d",c(22,24))],1,sum)
plants.MM[hab.urban>0 & !is.na(hab.urban),"Habitat_urban"] = 1
plants.MM[hab.urban%in%0 & !plants.MM$Habitat_urban%in%1,"Habitat_urban"] = 0
plants.MM[plants.MM[,"GUILDE.23"]>0 & !is.na(plants.MM[,"GUILDE.23"]),"Habitat_green_urban"] = 1
plants.MM[plants.MM[,"GUILDE.23"]%in%0 & !plants.MM$Habitat_green_urban%in%1,"Habitat_green_urban"] = 0
hab.agri = apply(plants.MM[,sprintf("GUILDE.%d",c(11,25))],1,sum)
plants.MM[hab.agri>0 & !is.na(hab.agri),"Habitat_agricultural"] = 1
plants.MM[hab.agri%in%0 & !plants.MM$Habitat_agricultural%in%1,"Habitat_agricultural"] = 0
plants.MM[plants.MM[,"GUILDE.13"]>0 & !is.na(plants.MM[,"GUILDE.13"]),"Habitat_bushes"] = 1
plants.MM[plants.MM[,"GUILDE.13"]%in%0 & !plants.MM$Habitat_bushes%in%1,"Habitat_bushes"] = 0
plants.MM[plants.MM[,"GUILDE.15"]>0 & !is.na(plants.MM[,"GUILDE.15"]),"Habitat_transitional"] = 1
plants.MM[plants.MM[,"GUILDE.15"]%in%0 & !plants.MM$Habitat_transitional%in%1,"Habitat_transitional"] = 0
plants.MM[plants.MM[,"GUILDE.18"]>0 & !is.na(plants.MM[,"GUILDE.18"]),"Habitat_heathland"] = 1
plants.MM[plants.MM[,"GUILDE.18"]%in%0 & !plants.MM$Habitat_heathland%in%1,"Habitat_heathland"] = 0
plants.MM[,HABit[!HABit%in%names(plants.MM)]] = NA
plants.MM[,names(plants.MM)[grepl("GUILD",names(plants.MM))]] = NULL

# Fine tune habitat flora indicativa
hab2.wat = apply(plants.MM[,paste0("HABfi_",c("1.1","1.2","1.4","2.5","4.1","4.2"))],1,sum)
plants.MM[hab2.wat>0&!is.na(hab2.wat),"Habitat_water"] = 1
plants.MM[hab2.wat%in%0&is.na(plants.MM$Habitat_water),"Habitat_water"] = 0
hab2.agri = apply(plants.MM[,paste0("HABfi_",c("2.1","2.8","2.9"))],1,sum)
plants.MM[hab2.agri>0&!is.na(hab2.agri),"Habitat_agricultural"] = 1
plants.MM[hab2.agri%in%0&is.na(plants.MM$Habitat_agricultural),"Habitat_agricultural"] = 0
hab2.bush = apply(plants.MM[,paste0("HABfi_",c("7.1","7.2","7.3","7.6","8.1"))],1,sum)
plants.MM[hab2.bush>0&!is.na(hab2.bush),"Habitat_bushes"] = 1
plants.MM[hab2.bush%in%0&is.na(plants.MM$Habitat_bushes),"Habitat_bushes"] = 0
plants.MM[plants.MM$HABfi_8.2>0&!is.na(plants.MM$HABfi_8.2),"Habitat_coniferous"] = 1
plants.MM[plants.MM$HABfi_8.2%in%0&is.na(plants.MM$Habitat_coniferous),"Habitat_coniferous"] = 0
plants.MM[plants.MM$HABfi_9.5>0&!is.na(plants.MM$HABfi_9.5),"Habitat_forests"] = 1
plants.MM[plants.MM$HABfi_9.5%in%0&is.na(plants.MM$Habitat_forests),"Habitat_forests"] = 0
plants.MM[plants.MM$HABfi_9.1>0&!is.na(plants.MM$HABfi_9.1),"Habitat_coniferous"] = 1
plants.MM[plants.MM$HABfi_9.1%in%0&is.na(plants.MM$Habitat_coniferous),"Habitat_coniferous"] = 0
plants.MM[plants.MM$HABfi_9.1>0&!is.na(plants.MM$HABfi_9.1),"Habitat_forests"] = 1
plants.MM[plants.MM$HABfi_9.1%in%0&is.na(plants.MM$Habitat_forests),"Habitat_forests"] = 0
plants.MM[plants.MM$HABfi_9.2>0&!is.na(plants.MM$HABfi_9.2),"Habitat_decideous"] = 1
plants.MM[plants.MM$HABfi_9.2%in%0&is.na(plants.MM$Habitat_decideous),"Habitat_decideous"] = 0
plants.MM[plants.MM$HABfi_9.2>0&!is.na(plants.MM$HABfi_9.2),"Habitat_forests"] = 1
plants.MM[plants.MM$HABfi_9.2%in%0&is.na(plants.MM$Habitat_forests),"Habitat_forests"] = 0
hab2.mix = apply(plants.MM[,paste0("HABfi_",c("9.3","9.4"))],1,sum)
plants.MM[hab2.mix>0&!is.na(hab2.mix),"Habitat_mixed"] = 1
plants.MM[hab2.mix%in%0&is.na(plants.MM$Habitat_mixed),"Habitat_mixed"] = 0
plants.MM[hab2.mix>0&!is.na(hab2.mix),"Habitat_forests"] = 1
plants.MM[hab2.mix%in%0&is.na(plants.MM$Habitat_forests),"Habitat_forests"] = 0
hab2.grass = apply(plants.MM[,paste0("HABfi_",c("6.1","6.2","6.3","6.4","6.5","6.6"))],1,sum)
plants.MM[hab2.grass>0&!is.na(hab2.grass),"Habitat_grassland"] = 1
plants.MM[hab2.grass%in%0&is.na(plants.MM$Habitat_grassland),"Habitat_grassland"] = 0
plants.MM[plants.MM$HABfi_2.7>0&!is.na(plants.MM$HABfi_2.7),"Habitat_green_urban"] = 1
plants.MM[plants.MM$HABfi_2.7%in%0&is.na(plants.MM$Habitat_green_urban),"Habitat_green_urban"] = 0
hab2.pio = apply(plants.MM[,paste0("HABfi_",c("2.2","2.4","3.5"))],1,sum)
plants.MM[hab2.pio>0&!is.na(hab2.pio),"Habitat_pioneer"] = 1
plants.MM[hab2.pio%in%0&is.na(plants.MM$Habitat_pioneer),"Habitat_pioneer"] = 0
hab2.rock = apply(plants.MM[,paste0("HABfi_",c("3.2","3.3","3.4"))],1,sum)
plants.MM[hab2.rock>0&!is.na(hab2.rock),"Habitat_rocks"] = 1
plants.MM[hab2.rock%in%0&is.na(plants.MM$Habitat_rocks),"Habitat_rocks"] = 0
plants.MM[plants.MM$HABfi_3.1>0&!is.na(plants.MM$HABfi_3.1),"Habitat_sand"] = 1
plants.MM[plants.MM$HABfi_3.1%in%0&is.na(plants.MM$Habitat_sand),"Habitat_sand"] = 0
hab2.trans = apply(plants.MM[,paste0("HABfi_",c("2.3","2.6","6.7","7.4"))],1,sum)
plants.MM[hab2.trans>0&!is.na(hab2.trans),"Habitat_transitional"] = 1
plants.MM[hab2.trans%in%0&is.na(plants.MM$Habitat_transitional),"Habitat_transitional"] = 0
hab2.wet = apply(plants.MM[,paste0("HABfi_",c("1.3","5.1","5.2","7.5"))],1,sum)
plants.MM[hab2.wet>0&!is.na(hab2.wet),"Habitat_wetlands"] = 1
plants.MM[hab2.wet%in%0&is.na(plants.MM$Habitat_wetlands),"Habitat_wetlands"] = 0
plants.MM = plants.MM[,!grepl("HABfi_",names(plants.MM))]

# # Fine tune Ter/Aqu/Arb info
# plants.MM[plants.MM$Habitat_urban%in%1,"Ter"] = 1
# plants.MM[plants.MM$Habitat_green_urban%in%1,"Ter"] = 1
# plants.MM[plants.MM$Habitat_agriculture%in%1,"Ter"] = 1
# plants.MM[plants.MM$Habitat_deciduous%in%1,"Arb"] = 1
# plants.MM[plants.MM$Habitat_coniferous%in%1,"Arb"] = 1
# plants.MM[plants.MM$Habitat_mixed%in%1,"Arb"] = 1
# plants.MM[plants.MM$Habitat_grassland%in%1,"Ter"] = 1
# plants.MM[plants.MM$Habitat_heathland%in%1,"Arb"] = 1
# plants.MM[plants.MM$Habitat_bushes%in%1,"Arb"] = 1
# plants.MM[plants.MM$Habitat_sand%in%1,"Ter"] = 1
# plants.MM[plants.MM$Habitat_wetlands%in%1,"Aqu"] = 1
# plants.MM[plants.MM$Habitat_water%in%1,"Aqu"] = 1
# plants.MM[plants.MM$Habitat_rocks%in%1,"Ter"] = 1
# plants.MM[plants.MM$Habitat_pioneer%in%1,"Ter"] = 1
# l.temp = plants.MM[,c("Ter","Aqu","Arb")]
# l.temp [is.na(l.temp)] = 0
# l.temp[apply(l.temp,1,function(x) all(x%in%0)),] = NA
# plants.MM[,c("Ter","Aqu","Arb")] = l.temp # No need (indirectly set before)

# Aquatic, Terrestrial or Marine
plants.MM[,"EcoMar"]= NA
plants.MM[grepl("terrestrial",plants.MM$Habitat),"EcoTer"] = 1
plants.MM[grepl("terrestrial",plants.MM$Habitat),c("EcoFresh","EcoMar")] = 0
plants.MM[grepl("aquatic",plants.MM$Habitat),"EcoFresh"] = 1
plants.MM[grepl("aquatic",plants.MM$Habitat),"EcoTer"] = 0
plants.MM[grepl("semiaquatic",plants.MM$Habitat),"EcoTer"] = 1
plants.MM$Aer = 0
plants.MM$Habitat = NULL

# Fine tune Ter/Aqu info (c)
plants.MM = plants.MM[order(plants.MM$species),]
mf1 = merge(plants.MM,antoine.TerAqu,by="species",all.x=TRUE)
all(plants.MM$species == mf1$species) # auto check
plants.MM[mf1$scheme%in%"terrestrial"&is.na(plants.MM$Ter),"Ter"] = 1
plants.MM[mf1$scheme%in%"aquatic",c("Aqu","EcoFresh")] = 1
plants.MM[plants.MM$EcoFresh%in%1,"Aqu"] = 1

# Reorder & Save
hab.n = grep("Habitat_",names(plants.MM))
eco.n = grep("Eco",names(plants.MM))
col.ref = c("species","scientificName","taxonRank","gbif_status","gbif_accepted","genus","family","order",
	"class","phylum","group","Source","originCH","originEUR","swissRedListCategory","europeanRegionalRedListCategory",
	"iucnRedListCategory","EcoTer","EcoFresh","EcoMar","Fos","Ter","Aqu","Arb","Aer",
	names(plants.MM[,hab.n]),"RangeSize_km2")
plants.MM = plants.MM[,c(col.ref,names(plants.MM)[!names(plants.MM)%in%col.ref])]
write.table(plants.MM,"outputs/raw_traits/non_formated/s2z_raw_traits_plants.txt",row.names=FALSE)


### ==========================================================================================================================
### Bryophytes
### ==========================================================================================================================


# # Open the best sources
# bryo.bet = as.data.frame(read_excel("./bryophytes/van_Zuijlen-2023_betdata.xlsx",1))
# bryo.infosp = as.data.frame(read_excel("./bryophytes/bryo-taxa-ecology.xlsx",1))

# # Open range km2
# plants.range = iucn.ranges[grepl("PLANTS",names(iucn.ranges))]
# plants.range = do.call("rbind",plants.range)
# plants.range = plants.range[!duplicated(plants.range$species),]

# # Format BET
# bryo.bet[,c("native","endemic","category","lstrat_e","rK","dmnt","smind","smaxd","sseas","sub_sum",
# 	"dwarfm","indHM","nc","T_diurR","T_iso","T_seas","Tmax_warmM","Tmin_coldM","T_annualR",
# 	"T_wetQ","T_dryQ","T_warmQ","T_coldQ","P_wetM","P_dryM","P_seas","P_wetQ","P_dryQ","P_warmQ",
# 	"P_coldQ","gdd0","gdd5","gdd10","ngd0","ngd5","ngd10","hab_ar","hab_fo","hab_gr","hab_ro",
# 	"hab_sh","hab_we","hem_e","hab_sum","hemeroby","eastlim","biome","AOO_unc","EOO_est","EOO_unc",
# 	"lim_range")] = NULL
# bryo.bet$phylum = str_to_title(bryo.bet$phylum)
# bryo.bet$class = str_to_title(bryo.bet$class)
# bryo.bet$order = str_to_title(bryo.bet$order)
# bryo.bet$family = str_to_title(bryo.bet$family)
# bryo.bet[bryo.bet$lstrat%in%"a","lstrat"] = "annual_shuttle"
# bryo.bet[bryo.bet$lstrat%in%"c","lstrat"] = "colonist"
# bryo.bet[bryo.bet$lstrat%in%"f","lstrat"] = "fugitive"
# bryo.bet[bryo.bet$lstrat%in%"l","lstrat"] = "longLived_shuttle"
# bryo.bet[bryo.bet$lstrat%in%"p","lstrat"] = "perennial"
# bryo.bet[bryo.bet$lstrat%in%"s","lstrat"] = "shortLived_shuttle"
# bryo.bet[bryo.bet$gform%in%"acr","gform"] = "acrocarpous"
# bryo.bet[bryo.bet$gform%in%"fol","gform"] = "foliose"
# bryo.bet[bryo.bet$gform%in%"ple","gform"] = "pleurocarpous"
# bryo.bet[bryo.bet$gform%in%"sph","gform"] = "sphagnum"
# bryo.bet[bryo.bet$gform%in%"tha","gform"] = "thalloid"
# bryo.bet[bryo.bet$sex%in%"D","sex"] = "dioicous"
# bryo.bet[bryo.bet$sex%in%"M","sex"] = "monoicous"
# bryo.bet[bryo.bet$sex%in%"M/D","sex"] = "both"
# bryo.bet[bryo.bet$capspos%in%"erect to inclined","capspos"] = "erect_to_inclined"
# bryo.bet[bryo.bet$capspos%in%"inclined to pendulous","capspos"] = "inclined_to_pendulous"
# bryo.bet[bryo.bet$sfreq%in%0,"sfreq"] = "EU_unknown"
# bryo.bet[bryo.bet$sfreq%in%1,"sfreq"] = "rare"
# bryo.bet[bryo.bet$sfreq%in%2,"sfreq"] = "occasional"
# bryo.bet[bryo.bet$sfreq%in%3,"sfreq"] = "common"
# bryo.bet[bryo.bet$vpfreq%in%1,"vpfreq"] = "rare"
# bryo.bet[bryo.bet$vpfreq%in%2,"vpfreq"] = "occasional"
# bryo.bet[bryo.bet$vpfreq%in%3,"vpfreq"] = "common"
# bryo.bet[bryo.bet$forest%in%1,"forest"] = "closed"
# bryo.bet[bryo.bet$forest%in%2,"forest"] = "edges"
# bryo.bet[bryo.bet$forest%in%3,"forest"] = "mix"
# bryo.bet[bryo.bet$forest%in%4,"forest"] = "open"
# bryo.bet[bryo.bet$indF%in%"x","indF"] = 0
# bryo.bet[bryo.bet$indK%in%"x","indK"] = 0
# bryo.bet[bryo.bet$indL%in%"x","indL"] = 0
# bryo.bet[bryo.bet$indN%in%"x","indN"] = 0
# bryo.bet[bryo.bet$indR%in%"x","indR"] = 0
# bryo.bet[bryo.bet$indT%in%"x","indT"] = 0
# bryo.bet$Fos = 0
# bryo.bet$Ter = 0
# bryo.bet$Aqu = 0
# bryo.bet$Arb = 0
# bryo.bet$Aer = 0
# bryo.bet[bryo.bet$aquatic%in%1,"Aqu"] = 1
# bryo.bet[bryo.bet$aquatic%in%0,"Ter"] = 1 # by deduction because mosses
# bryo.bet[bryo.bet$epiphyte%in%1,"Arb"] = 1
# bryo.bet[bryo.bet$aquatic%in%"NA",c("Fos","Ter","Aqu","Arb","Aer")] = NA
# bryo.bet$class = NULL

# # Format infosp
# 	# Habitat
# hab.split = lapply(bryo.infosp[,"Delarze-Habitat"],function(x) strsplit(x,", |,")[[1]])
# hab.uniq = sort(unique(unlist(hab.split)))
# hab.uniq = na.omit(hab.uniq[!hab.uniq%in%"?"])
# bryo.infosp[,hab.uniq] = 0
# bryo.infosp[is.na(bryo.infosp[,"Delarze-Habitat"]),hab.uniq] = NA
# for (i in 1:length(hab.uniq)){
# 	h.pos = sapply(hab.split,function(x) any(x%in%hab.uniq[i]))
# 	bryo.infosp[h.pos,hab.uniq[i]] = 1
# }
# 	# Substrate
# count.split = lapply(bryo.infosp[,"Substrat Codes (Anzahl)"],function(x) strsplit(x,",")[[1]])
# count.numb = lapply(count.split,function(x) gsub("[^0-9.]","",x))
# count.lett = lapply(count.split,function(x) gsub(" .*","",x))
# count.split2 = lapply(1:length(count.lett),function(x){
# 	u.count = unique(count.lett[[x]])
# 	r.lu = sapply(u.count,function(y) mean(as.numeric(count.numb[[x]][count.lett[[x]]%in%y])))
# 	return(r.lu)
# })
# sub.uniq = sort(unique(unlist(count.lett)))
# bryo.infosp[,sub.uniq] = 0
# bryo.infosp[is.na(bryo.infosp[,"Substrat Codes"]),sub.uniq] = NA
# for (i in 1:length(count.split2)){
# 	if (!is.na(count.split2[[i]][1])){
# 		bryo.infosp[i,names(count.split2[[i]])] = count.split2[[i]]
# 	}
# }
# bryo.infosp[,c("Taxon ID","Delarze-Habitat","Substrat Codes","Substrat Codes (Anzahl)","Substrate",
# 	"Substrate (Anzahl)","Licht Variabilität","Temperatur Variabilität","Feuchte Variabilität",
# 	"Reaktion Variabilität")] = NULL
# 	# Canonical names
# bryo.infosp$species =
# sapply(bryo.infosp$Taxon,function(x){
# 	s1 = strsplit(x," ")[[1]]
# 	if (length(s1)==1 | !s1[2] == tolower(s1[2])) {return(s1[1])}
# 	p1 = paste0(strsplit(x," ")[[1]][1:2],collapse=" ")
# 	p2 = ifelse(any(s1%in%"subsp."),paste("subsp.",s1[which(s1%in%"subsp.")+1]),"")
# 	return(str_trim(paste(p1,p2)))
# })
# 	# Remove unnecessary + anomalies
# bryo.infosp2 = bryo.infosp[!duplicated(bryo.infosp$species),]
# bryo.infosp2$Taxon = NULL
# oneW.id = sapply(strsplit(bryo.infosp2$species," "),length)
# bryo.infosp2 = bryo.infosp2[!oneW.id%in%1,]
# bryo.infosp2 = bryo.infosp2[!bryo.infosp2$species%in%"Genus sp.",]

# # Time to merge
# bryo.dat = merge(bryo.bet,bryo.infosp2,by.x="friendly_name",by.y="species",all=TRUE)
# bryo.dat$Elevation = (bryo.dat$lim_low+bryo.dat$lim_up)/2

# # Keep the IUCN range in priority
# plants.range = iucn.ranges[grepl("PLANTS",names(iucn.ranges))]
# plants.range = do.call("rbind",plants.range)
# plants.range = plants.range[!duplicated(plants.range$species),]
# bryo.iucn = merge(bryo.dat,plants.range[,c("species","range.size.km2")],
# 	by.x="friendly_name",by.y="species",all=TRUE)
# bryo.iucn$RangeSize_km2 = bryo.iucn$range.size.km2
# bryo.iucn$indK = NULL

# # Harmonize names
# names(bryo.iucn)[c(1:46,52:55,137:153)] = c("species","Phylum","Order","Family","Genus","Gam_size_mm","Life_strategy",
# 	"Life_form","Growth","Gen_length_y","Perm_proto","Rhizoids","Mating_type","Spore_size_µm","Sporophyte_freq",
# 	"Seta_max_len_mm","Seta_caps","Peristome","Veg_propagules","Vp_gem","Vp_tub","Vp_bul","Vp_lea","Vp_bra","Vp_freq",
# 	"Vp_max_size_µm","EIVbet_Light","EIVbet_Temp","EIVbet_Moisture","EIVbet_pH","EIVbet_Nutrients",
# 	"EIVbet_Na","Elevation_low","Elevation_up","RangeSize_RM","HUMUSbet_substr","ROCKbet_substr","BARKbet_substr",
# 	"DEADWbet_substr","nWOODYbet_substr","ANIMALbet_substr","Epiphyte","aquatic","FORESTbet_substr","AnnuMeanTemp","AnnuPrecip",
# 	"EIVsb_Light","EIVsb_Temp","EIVsb_Moisture","EIVsb_pH","WATERsb_substr","MUDsb_substr","WETsb_substr","DRYsb_substr",
# 	"HUMUSsb_substr","DEADWsb_substr","BARKsb_substr","nWOODYsb_substr","ANIMALsb_substr","TUFAsb_substr",
# 	"CalROCKsb_substr","SilROCKsb_substr","CalrWETsb_substr","CalrDRYsb_substr","SilrWETsb_substr",
# 	"SilrDRYsb_substr","SPECIALsb_substr")
# names(bryo.iucn)[56:136] = paste0("HABsb_",names(bryo.iucn)[56:136])

# # Change names if necessary
# tGS = to_GenusSpecies(bryo.iucn$species)
# bryo.iucn$species = tGS[,"CanonicalName"]
# bryo.iucn = bryo.iucn[!is.na(bryo.iucn$species),]
# # bryo.iucn = combine_rows(bryo.iucn,"species") # Unique names (no need)
# bryo.iucn$IUCN_status = NA

# # Reorder columns
# bryo.vert = bryo.iucn[,c("species","Genus","Family","Order","Phylum","Growth","Life_form","Life_strategy",
# 	"Epiphyte","aquatic","Seta_caps","Peristome","Rhizoids","Perm_proto","Mating_type","Sporophyte_freq","Gen_length_y",
# 	"Seta_max_len_mm","Gam_size_mm","Spore_size_µm","Veg_propagules","Vp_max_size_µm","Vp_gem","Vp_tub","Vp_bul",
# 	"Vp_lea","Vp_bra","Vp_freq","Fos","Ter","Aqu","Arb","Aer",names(bryo.iucn)[grepl("HAB",names(bryo.iucn))],
# 	"ROCKbet_substr","WETsb_substr","DRYsb_substr","TUFAsb_substr","CalROCKsb_substr","CalrWETsb_substr","CalrDRYsb_substr",
# 	"SilROCKsb_substr","SilrWETsb_substr","SilrDRYsb_substr","HUMUSbet_substr","HUMUSsb_substr","BARKbet_substr",
# 	"BARKsb_substr","DEADWbet_substr","DEADWsb_substr","nWOODYbet_substr","nWOODYsb_substr","ANIMALbet_substr",
# 	"ANIMALsb_substr","FORESTbet_substr","MUDsb_substr","WATERsb_substr","SPECIALsb_substr","EIVbet_Moisture","EIVsb_Moisture",
# 	"EIVbet_pH","EIVsb_pH","EIVbet_Nutrients","EIVbet_Na","EIVbet_Temp","EIVsb_Temp","EIVbet_Light","EIVsb_Light",
# 	"AnnuMeanTemp","AnnuPrecip","Elevation_low","Elevation","Elevation_up","RangeSize_km2","IUCN_status")]
# bryo.vert[bryo.vert==""] = NA

# # Convert to numeric
# non.num = c("species","Genus","Family","Order","Phylum","Growth","Life_form","Life_strategy","Seta_caps",
# 	"Mating_type","Sporophyte_freq","Vp_freq","FORESTbet_substr","IUCN_status")
# bryo.vert[,!names(bryo.vert)%in%non.num] = sapply(bryo.vert[,!names(bryo.vert)%in%non.num],as.numeric)

# # Fill in additonal logical information
# bryo.vert$Fos = NA
# bryo.vert$Aer = 0

# # Add gbif information
# bryo.vert[,GB.col] = NA
# for (i in 1:nrow(bryo.vert)){
#     cat("...sp.",i)
#     gbif.search = get_status(bryo.vert[i,"species"]) # Search for accepted names
#     if (!is.null(gbif.search)){
#     	bryo.vert$gbif_accepted[i] = gbif.search$scientificName[1]
#      	bryo.vert$gbif_rank[i] = gbif.search$rank[1]
#         bryo.vert$gbif_phylum[i] = gbif.search$Phylum[1]
#         bryo.vert$gbif_genus[i] = gbif.search$Genus[1]
#         bryo.vert$gbif_order[i] = gbif.search$Order[1]
#         bryo.vert$gbif_family[i] = gbif.search$Family[1]
#         bryo.vert$gbif_class[i] = gbif.search$Class[1]
#         bryo.vert$gbif_IUCN[i] = gbif.search$IUCN_status[1]
#     }
# }
# write.table(bryo.vert,"temp_bryo.txt",row.names=FALSE)
bryo.vert = read.table("data/temp/temp_bryo.txt",header=TRUE)

# Remove the vascular plants
bryo.vert = bryo.vert[!bryo.vert$gbif_phylum%in%"Tracheophyta",]
bryo.vert = bryo.vert[!is.na(bryo.vert$gbif_phylum),]

# Format our table
which.gbif = which(names(bryo.vert)%in%GB.col)
bryo.vert = bryo.vert[,c(1,which.gbif,2:(ncol(bryo.vert)-length(which.gbif)))]
bryo.vert[is.na(bryo.vert$gbif_genus),"gbif_genus"] = bryo.vert[is.na(bryo.vert$gbif_genus),"Genus"]
bryo.vert$gbif_genus[is.na(bryo.vert$gbif_genus)] =
	unlist(sapply(strsplit(bryo.vert$species[is.na(bryo.vert$gbif_genus)]," "),function(x) x[1]))
bryo.vert[is.na(bryo.vert$gbif_family),"gbif_family"] = bryo.vert[is.na(bryo.vert$gbif_family),"Family"]
bryo.vert[is.na(bryo.vert$gbif_order),"gbif_order"] = bryo.vert[is.na(bryo.vert$gbif_order),"Order"]
bryo.vert[is.na(bryo.vert$gbif_phylum),"gbif_phylum"] = bryo.vert[is.na(bryo.vert$gbif_phylum),"Phylum"]
bryo.vert[is.na(bryo.vert$IUCN_status),"IUCN_status"] = bryo.vert[is.na(bryo.vert$IUCN_status),"gbif_IUCN"]
bryo.vert[,c("Genus","Family","Order","Phylum","gbif_IUCN")] = NULL
names(bryo.vert)[2:length(GB.col)] = new.col
gbif.cond = bryo.vert$IUCN_status%in%c("NOT_FOUND","NOT_EVALUATED")
bryo.vert[gbif.cond,"IUCN_status"] = NA

# Extract EU checklist and merge
s2z.bryo = s2z.sp[s2z.sp$group%in%"Bryophytes",]
bryo.merge1 = merge(s2z.bryo,bryo.vert,by="species")
bryo.vert = bryo.vert[!bryo.vert$species%in%bryo.merge1$species,]
s2z.bryo2 = s2z.bryo[!s2z.bryo$species%in%bryo.merge1$species,]

# Regroup all the available information for the accepted species (mean when continuous values)
mam.ndup = bryo.vert[!(duplicated(bryo.vert$gbif_accepted)|duplicated(bryo.vert$gbif_accepted,fromLast=TRUE)),]
mam.dup = bryo.vert[duplicated(bryo.vert$gbif_accepted)|duplicated(bryo.vert$gbif_accepted,fromLast=TRUE),]
mam.homo = combine_rows(df=mam.dup,ref.col="gbif_accepted",binary=FALSE)
mam.homo = mam.homo[!is.na(mam.homo$gbif_accepted),]

# Combine, attempt another merge and rbind both results
mam.temp = rbind(mam.ndup,mam.homo)
bryo.merge2 = merge(s2z.bryo2,mam.temp,by="gbif_accepted",all.x=TRUE)
names(bryo.merge2)[1:2] = c("gbif_accepted.x","species")
bryo.merge2$gbif_accepted.y = bryo.merge2$gbif_accepted.x
bryo.merge2 = bryo.merge2[,names(bryo.merge1)]
bryo.MM = rbind(bryo.merge1,bryo.merge2)

# Try to fill in mising information fron one column to another
bryo.MM[is.na(bryo.MM$genus),"genus"] = bryo.MM[is.na(bryo.MM$genus),"Genus"]
bryo.MM[is.na(bryo.MM$family),"family"] = bryo.MM[is.na(bryo.MM$family),"Family"]
bryo.MM[is.na(bryo.MM$order),"order"] = bryo.MM[is.na(bryo.MM$order),"Order"]
bryo.MM[is.na(bryo.MM$class),"class"] = bryo.MM[is.na(bryo.MM$class),"Class"]
bryo.MM[is.na(bryo.MM$phylum),"phylum"] = bryo.MM[is.na(bryo.MM$phylum),"Phylum"]
bryo.MM[grepl("unresolved_accepted",bryo.MM$gbif_accepted.x),"gbif_accepted.x"] =
	bryo.MM[grepl("unresolved_accepted",bryo.MM$gbif_accepted.x),"gbif_accepted.y"]
bryo.MM$gbif_accepted.x[is.na(bryo.MM$gbif_accepted.x)] =
	paste0("unresolved_accepted2_", seq_len(sum(is.na(bryo.MM$gbif_accepted.x))))
bryo.MM[is.na(bryo.MM$taxonRank),"taxonRank"] = bryo.MM[is.na(bryo.MM$taxonRank),"gbif_rank"]
bryo.MM[is.na(bryo.MM$iucnRedListCategory),"iucnRedListCategory"] =
	bryo.MM[is.na(bryo.MM$iucnRedListCategory),"IUCN_status"]
toRemove = grepl("\\.y",names(bryo.MM)) |
	names(bryo.MM)%in%c("Genus","Family","Order","Class","Phylum","gbif_rank","IUCN_status")
bryo.MM[,toRemove] = NULL
names(bryo.MM)[1:ncol(s2z.bryo2)] = names(s2z.bryo2)

# Fine tune Ter/Aqu/Arb info (a)
guild.ter = apply(bryo.MM[,sprintf("GUILDE.%d",c(3,10,12,14:15,20:22,24:25))],1,sum)
bryo.MM[guild.ter>0&!is.na(guild.ter),"Ter"] = 1
guild.aqu = apply(bryo.MM[,sprintf("GUILDE.%d",c(1,2,4:9,26))],1,sum)
bryo.MM[guild.aqu>0&!is.na(guild.aqu),"Aqu"] = 1
guild.arb = apply(bryo.MM[,sprintf("GUILDE.%d",c(11,13,16:19,23))],1,sum)
bryo.MM[guild.arb>0&!is.na(guild.arb),"Arb"] = 1

# Fine tune Ter/Aqu/Arb info (b)
sb.t = c("2.2.5","3","3.2","3.2.1","3.2.2","3.3","3.3.1","3.3.2","3.4","3.4.1","3.4.1.2","3.4.1.3","3.4.2","3.4.2.1",
	"4.1","4.1.1","4.1.2","4.2","4.2.1","4.3","4.3.1","4.3.4","4.3.7","4.4","4.4.2","4.5","5.2","7","7.1","7.2","7.2.1",
	"8","8.1.6","8.2","8.2.1","9.2","9.2.4")
guild.ter = apply(bryo.MM[,paste0("HABsb_",sb.t)],1,sum)
bryo.MM[guild.ter>0&!is.na(guild.ter),"Ter"] = 1
sb.w = c("1.1.0","1.1.3","1.2","1.2.1","1.2.2","1.2.3","1.2.4","1.3","1.3.2","1.3.3","2",
	"2.0","2.1","2.1.2","2.2","2.2.2","2.2.3","2.3","2.4","2.4.1","2.5","2.5.1","6.5")
guild.aqu = apply(bryo.MM[,paste0("HABsb_",sb.w)],1,sum)
bryo.MM[guild.aqu>0&!is.na(guild.aqu),"Aqu"] = 1
sb.a = c("5","5.3","5.3.2","5.3.9","5.4","5.4.1","5.4.5","6","6.1","6.2",
	"6.3","6.3.1","6.3.7","6.6","6.6.2","6.6.3","8.1")
guild.arb = apply(bryo.MM[,paste0("HABsb_",sb.a)],1,sum)
bryo.MM[guild.arb>0&!is.na(guild.arb),"Arb"] = 1
toZ = apply(bryo.MM[,c("Ter","Aqu","Arb")],1,function(x) all(is.na(x)))
bryo.MM[!toZ,c("Ter","Aqu","Arb")][is.na(bryo.MM[!toZ,c("Ter","Aqu","Arb")])] = 0

# Fine tune habitat simple
hab.wat = apply(bryo.MM[,sprintf("GUILDE.%d",c(1,2,4,5,8))],1,sum)
bryo.MM[hab.wat>0 & !is.na(hab.wat),"Habitat_water"] = 1
bryo.MM[hab.wat%in%0 & !bryo.MM$Habitat_water%in%1,"Habitat_water"] = 0
hab.wet = apply(bryo.MM[,sprintf("GUILDE.%d",c(6,7,9,26))],1,sum)
bryo.MM[hab.wet>0 & !is.na(hab.wet),"Habitat_wetlands"] = 1
bryo.MM[hab.wet%in%0 & !bryo.MM$Habitat_wetlands%in%1,"Habitat_wetlands"] = 0
bryo.MM[bryo.MM[,"GUILDE.21"]>0 & !is.na(bryo.MM[,"GUILDE.21"]),"Habitat_rocks"] = 1
bryo.MM[bryo.MM[,"GUILDE.21"]%in%0 & !bryo.MM$Habitat_rocks%in%1,"Habitat_rocks"] = 0
hab.grass = apply(bryo.MM[,sprintf("GUILDE.%d",c(14,20))],1,sum)
bryo.MM[hab.grass>0 & !is.na(hab.grass),"Habitat_grassland"] = 1
bryo.MM[hab.grass%in%0 & !bryo.MM$Habitat_grassland%in%1,"Habitat_grassland"] = 0
hab.forest = apply(bryo.MM[,sprintf("GUILDE.%d",c(16,17,19))],1,sum)
bryo.MM[hab.forest>0 & !is.na(hab.forest),"Habitat_forests"] = 1
bryo.MM[hab.forest%in%0 & !bryo.MM$Habitat_forests%in%1,"Habitat_forests"] = 0
bryo.MM[bryo.MM[,"GUILDE.17"]>0 & !is.na(bryo.MM[,"GUILDE.17"]),"Habitat_deciduous"] = 1
bryo.MM[bryo.MM[,"GUILDE.17"]%in%0 & !bryo.MM$Habitat_deciduous%in%1,"Habitat_deciduous"] = 0
bryo.MM[bryo.MM[,"GUILDE.19"]>0 & !is.na(bryo.MM[,"GUILDE.19"]),"Habitat_coniferous"] = 1
bryo.MM[bryo.MM[,"GUILDE.19"]%in%0 & !bryo.MM$Habitat_coniferous%in%1,"Habitat_coniferous"] = 0
bryo.MM[bryo.MM[,"GUILDE.16"]>0 & !is.na(bryo.MM[,"GUILDE.16"]),"Habitat_mixed"] = 1
bryo.MM[bryo.MM[,"GUILDE.16"]%in%0 & !bryo.MM$Habitat_mixed%in%1,"Habitat_mixed"] = 0
hab.pioneer = apply(bryo.MM[,sprintf("GUILDE.%d",c(3,10,12,22))],1,sum)
bryo.MM[hab.pioneer>0 & !is.na(hab.pioneer),"Habitat_pioneer"] = 1
bryo.MM[hab.pioneer%in%0 & !bryo.MM$Habitat_pioneer%in%1,"Habitat_pioneer"] = 0
hab.urban = apply(bryo.MM[,sprintf("GUILDE.%d",c(22,24))],1,sum)
bryo.MM[hab.urban>0 & !is.na(hab.urban),"Habitat_urban"] = 1
bryo.MM[hab.urban%in%0 & !bryo.MM$Habitat_urban%in%1,"Habitat_urban"] = 0
bryo.MM[bryo.MM[,"GUILDE.23"]>0 & !is.na(bryo.MM[,"GUILDE.23"]),"Habitat_green_urban"] = 1
bryo.MM[bryo.MM[,"GUILDE.23"]%in%0 & !bryo.MM$Habitat_green_urban%in%1,"Habitat_green_urban"] = 0
hab.agri = apply(bryo.MM[,sprintf("GUILDE.%d",c(11,25))],1,sum)
bryo.MM[hab.agri>0 & !is.na(hab.agri),"Habitat_agricultural"] = 1
bryo.MM[hab.agri%in%0 & !bryo.MM$Habitat_agricultural%in%1,"Habitat_agricultural"] = 0
bryo.MM[bryo.MM[,"GUILDE.13"]>0 & !is.na(bryo.MM[,"GUILDE.13"]),"Habitat_bushes"] = 1
bryo.MM[bryo.MM[,"GUILDE.13"]%in%0 & !bryo.MM$Habitat_bushes%in%1,"Habitat_bushes"] = 0
bryo.MM[bryo.MM[,"GUILDE.15"]>0 & !is.na(bryo.MM[,"GUILDE.15"]),"Habitat_transitional"] = 1
bryo.MM[bryo.MM[,"GUILDE.15"]%in%0 & !bryo.MM$Habitat_transitional%in%1,"Habitat_transitional"] = 0
bryo.MM[bryo.MM[,"GUILDE.18"]>0 & !is.na(bryo.MM[,"GUILDE.18"]),"Habitat_heathland"] = 1
bryo.MM[bryo.MM[,"GUILDE.18"]%in%0 & !bryo.MM$Habitat_heathland%in%1,"Habitat_heathland"] = 0
bryo.MM[,HABit[!HABit%in%names(bryo.MM)]] = NA
bryo.MM[,names(bryo.MM)[grepl("GUILD",names(bryo.MM))]] = NULL

# Fine tune habitat delarze
hab2.agri = apply(bryo.MM[,paste0("HABsb_",c("8","8.1","8.2","8.2.1"))],1,sum)
bryo.MM[hab2.agri>0&!is.na(hab2.agri),"Habitat_agricultural"] = 1
bryo.MM[hab2.agri%in%0&is.na(bryo.MM$Habitat_agricultural),"Habitat_agricultural"] = 0
hab2.bush = apply(bryo.MM[,paste0("HABsb_",c("5","5.3","5.3.2","5.3.9"))],1,sum)
bryo.MM[hab2.bush>0&!is.na(hab2.bush),"Habitat_bushes"] = 1
bryo.MM[hab2.bush%in%0&is.na(bryo.MM$Habitat_bushes),"Habitat_bushes"] = 0
bryo.MM[bryo.MM$HABsb_5>0&!is.na(bryo.MM$HABsb_5),"Habitat_forests"] = 1
bryo.MM[bryo.MM$HABsb_5%in%0&is.na(bryo.MM$Habitat_forests),"Habitat_forests"] = 0
hab2.coni = apply(bryo.MM[,paste0("HABsb_",c("6.6","6.6.2","6.6.3"))],1,sum)
bryo.MM[hab2.coni>0&!is.na(hab2.coni),"Habitat_coniferous"] = 1
bryo.MM[hab2.coni%in%0&is.na(bryo.MM$Habitat_coniferous),"Habitat_coniferous"] = 0
bryo.MM[hab2.coni>0&!is.na(hab2.coni),"Habitat_forests"] = 1
bryo.MM[hab2.coni%in%0&is.na(bryo.MM$Habitat_forests),"Habitat_forests"] = 0
hab2.deci = apply(bryo.MM[,paste0("HABsb_",c("6.1","6.2","6.3","6.3.1","6.3.7"))],1,sum)
bryo.MM[hab2.deci>0&!is.na(hab2.deci),"Habitat_deciduous"] = 1
bryo.MM[hab2.deci%in%0&is.na(bryo.MM$Habitat_deciduous),"Habitat_deciduous"] = 0
bryo.MM[hab2.deci>0&!is.na(hab2.deci),"Habitat_forests"] = 1
bryo.MM[hab2.deci%in%0&is.na(bryo.MM$Habitat_forests),"Habitat_forests"] = 0
hab2.grass = apply(bryo.MM[,paste0("HABsb_",c("4.2","4.2.1","4.3","4.3.1","4.3.4","4.3.7","4.4","4.4.2","4.5"))],1,sum)
bryo.MM[hab2.grass>0&!is.na(hab2.grass),"Habitat_grassland"] = 1
bryo.MM[hab2.grass%in%0&is.na(bryo.MM$Habitat_grassland),"Habitat_grassland"] = 0
hab2.heath = apply(bryo.MM[,paste0("HABsb_",c("5.4","5.4.1","5.4.5"))],1,sum)
bryo.MM[hab2.heath>0&!is.na(hab2.heath),"Habitat_heathland"] = 1
bryo.MM[hab2.heath%in%0&is.na(bryo.MM$Habitat_heathland),"Habitat_heathland"] = 0
hab2.pion = apply(bryo.MM[,paste0("HABsb_",c("7","7.1","7.2","7.2.1","8.1.6"))],1,sum)
bryo.MM[hab2.pion>0&!is.na(hab2.pion),"Habitat_pioneer"] = 1
bryo.MM[hab2.pion%in%0&is.na(bryo.MM$Habitat_pioneer),"Habitat_pioneer"] = 0
hab2.rocks= apply(bryo.MM[,paste0("HABsb_",
	c("3","3.3","3.3.1","3.3.2","3.4","3.4.1","3.4.1.2","3.4.1.3","3.4.2","3.4.2.1","4.1","4.1.1","4.1.2"))],1,sum)
bryo.MM[hab2.rocks>0&!is.na(hab2.rocks),"Habitat_rocks"] = 1
bryo.MM[hab2.rocks%in%0&is.na(bryo.MM$Habitat_rocks),"Habitat_rocks"] = 0
hab2.sand = apply(bryo.MM[,paste0("HABsb_",c("3.2","3.2.1","3.2.2"))],1,sum)
bryo.MM[hab2.sand>0&!is.na(hab2.sand),"Habitat_sand"] = 1
bryo.MM[hab2.sand%in%0&is.na(bryo.MM$Habitat_sand),"Habitat_sand"] = 0
hab2.trans = apply(bryo.MM[,paste0("HABsb_",c("2.2.5","5.2"))],1,sum)
bryo.MM[hab2.trans>0&!is.na(hab2.trans),"Habitat_transitional"] = 1
bryo.MM[hab2.trans%in%0&is.na(bryo.MM$Habitat_transitional),"Habitat_transitional"] = 0
hab2.urban = apply(bryo.MM[,paste0("HABsb_",c("9.2","9.2.4"))],1,sum)
bryo.MM[hab2.urban>0&!is.na(hab2.urban),"Habitat_urban"] = 1
bryo.MM[hab2.urban%in%0&is.na(bryo.MM$Habitat_urban),"Habitat_urban"] = 0
hab2.wat = apply(bryo.MM[,paste0("HABsb_",c("1.1.0","1.1.3","1.2","1.2.1","1.2.2","1.2.3","1.2.4","1.3","1.3.2","1.3.3"))],1,sum)
bryo.MM[hab2.wat>0&!is.na(hab2.wat),"Habitat_water"] = 1
bryo.MM[hab2.wat%in%0&is.na(bryo.MM$Habitat_water),"Habitat_water"] = 0
hab2.wet = apply(bryo.MM[,paste0("HABsb_",c("2","2.0","2.1","2.1.2","2.2","2.2.2","2.2.3","2.3","2.4","2.4.1","2.5","2.5.1","6.5"))],1,sum)
bryo.MM[hab2.wet>0&!is.na(hab2.wet),"Habitat_wetlands"] = 1
bryo.MM[hab2.wet%in%0&is.na(bryo.MM$Habitat_wetlands),"Habitat_wetlands"] = 0
bryo.MM[,names(bryo.MM)[grepl("HABsb_",names(bryo.MM))]] = NULL

# # Fine tune Ter/Aqu/Arb info
# bryo.MM[bryo.MM$Habitat_urban%in%1,"Ter"] = 1
# bryo.MM[bryo.MM$Habitat_green_urban%in%1,"Ter"] = 1
# bryo.MM[bryo.MM$Habitat_agriculture%in%1,"Ter"] = 1
# bryo.MM[bryo.MM$Habitat_deciduous%in%1,"Arb"] = 1
# bryo.MM[bryo.MM$Habitat_coniferous%in%1,"Arb"] = 1
# bryo.MM[bryo.MM$Habitat_mixed%in%1,"Arb"] = 1
# bryo.MM[bryo.MM$Habitat_grassland%in%1,"Ter"] = 1
# bryo.MM[bryo.MM$Habitat_heathland%in%1,"Arb"] = 1
# bryo.MM[bryo.MM$Habitat_bushes%in%1,"Arb"] = 1
# bryo.MM[bryo.MM$Habitat_sand%in%1,"Ter"] = 1
# bryo.MM[bryo.MM$Habitat_wetlands%in%1,"Aqu"] = 1
# bryo.MM[bryo.MM$Habitat_water%in%1,"Aqu"] = 1
# bryo.MM[bryo.MM$Habitat_rocks%in%1,"Ter"] = 1
# bryo.MM[bryo.MM$Habitat_pioneer%in%1,"Ter"] = 1
# l.temp = bryo.MM[,c("Ter","Aqu","Arb")]
# l.temp [is.na(l.temp)] = 0
# l.temp[apply(l.temp,1,function(x) all(x%in%0)),] = NA
# bryo.MM[,c("Ter","Aqu","Arb")] = l.temp # No need (indirectly set before)

# Aquatic, Terrestrial or Marine
bryo.MM[,"EcoMar"]= NA
bryo.MM[bryo.MM$aquatic%in%1,"EcoFresh"] = 1
bryo.MM[bryo.MM$aquatic%in%1,"EcoTer"] = 0
bryo.MM[bryo.MM$aquatic%in%0,"EcoTer"] = 1
bryo.MM[bryo.MM$aquatic%in%0,"EcoFresh"] = 0
bryo.MM$Aer = 0
bryo.MM$aquatic= NULL

# Fine tune Ter/Aqu info (c)
bryo.MM = bryo.MM[order(bryo.MM$species),]
mf1 = merge(bryo.MM,antoine.TerAqu,by="species",all.x=TRUE)
all(bryo.MM$species == mf1$species) # auto check
bryo.MM[mf1$scheme%in%"terrestrial"&is.na(bryo.MM$Ter),"Ter"] = 1
bryo.MM[mf1$scheme%in%"aquatic",c("Aqu","EcoFresh")] = 1
bryo.MM[bryo.MM$EcoFresh%in%1,"Aqu"] = 1

# Remove water habitat (weird data) + refine forest habitats
bryo.MM[bryo.MM$FORESTbet_substr%in%"edges","Habitat_transitional"] = 1
bryo.MM[bryo.MM$FORESTbet_substr%in%"closed",c("Arb","Habitat_forests")] = 1
bryo.MM[,c("FORESTbet_substr","WATERsb_substr")] = NULL

# Reorder & Save
hab.n = grep("Habitat_",names(bryo.MM))
eco.n = grep("Eco",names(bryo.MM))
col.ref = c("species","scientificName","taxonRank","gbif_status","gbif_accepted","genus","family","order",
	"class","phylum","group","Source","originCH","originEUR","swissRedListCategory","europeanRegionalRedListCategory",
	"iucnRedListCategory","EcoTer","EcoFresh","EcoMar","Fos","Ter","Aqu","Arb","Aer",
	names(bryo.MM[,hab.n]),"RangeSize_km2")
bryo.MM = bryo.MM[,c(col.ref,names(bryo.MM)[!names(bryo.MM)%in%col.ref])]
write.table(bryo.MM,"outputs/raw_traits/non_formated/s2z_raw_traits_bryophytes.txt",row.names=FALSE)


### ==========================================================================================================================
### Spiders
### ==========================================================================================================================


# # Open all trait sources (but combine conservation status)
# spider.csv = list.files("./spider/")
# spider.csv = spider.csv[!grepl(".pdf|x_miha|regl|rndl|eu_subterr",spider.csv)]
# spider.traits = lapply(spider.csv,function(x) read.csv2(paste0("./spider/",x),sep=","))

# # Create trait names
# name.traits = sub("[^_]*_","",spider.csv)
# name.traits = str_to_title(gsub(".csv","",name.traits))
# name.traits[grepl("width|length|height",name.traits)] =
# 	paste0(name.traits[grepl("width|length|height",name.traits)],"_mm")
# name.traits[c(4,6,14,41:42,44)] = paste0(name.traits[c(4,6,14,41:42,44)],"_mm")
# name.traits[57] = paste0(name.traits[57],"_mm3")
# name.traits[c(5,8,12,29,30,35,36,45)] = c("IUCN_status","BodyLength_mm","Activity","Light_class",
# 	"EIV_Light","EIV_Moisture","Moisture_class","RangeSize_km2")

# # Extract one by one the data, average it and sort it by species
# values.out = list()
# for (i in 1:length(name.traits))
# {
# 	# Extract one df, check if the value is a catgeory and get unique names
# 	tar.df = spider.traits[[i]]
# 	tar.df = tar.df[!tar.df$value%in%"access restricted",]
# 	q.val = as.numeric(tar.df$value)
# 	if (!all(is.na(q.val))) {tar.df$value = q.val}

# 	# Add the subspecies
# 	q.sub = !(tar.df$subspecies%in%""|is.na(tar.df$subspecies))
# 	if (any(q.sub)){
# 		str.div = strsplit(tar.df$originalName," ")[q.sub]
# 		tar.df$originalName[q.sub] = sapply(str.div,function(x) paste(x[1],x[2],"subsp.",x[3]))
# 	}

# 	# Homogenize names
# 	tar.df$originalName = gsub("[0-9]+","",tar.df$originalName)
# 	tar.df$originalName = str_squish(gsub("\\s*\\([^\\)]+\\)","",tar.df$originalName))
# 	u.species = tar.df[!duplicated(tar.df$originalName),c("originalName","genus","family")]

# 	# Empty data.frame
# 	out.df = data.frame(species=u.species$originalName,
# 						Genus=u.species$genus,
# 						Family=u.species$family,
# 						names=NA)
# 	names(out.df)[4] = name.traits[i]

# 	# Extract values for each species
# 	for (k in 1:nrow(out.df))
# 	{
# 		f.val = tar.df[tar.df$originalName%in%u.species$originalName[k],"value"]
# 		if (is.numeric(f.val[1])){
# 			val.out = mean(f.val,na.rm=TRUE)

# 		} else {
# 			val.out = raster::modal(f.val,na.rm=TRUE)
# 		}
# 		out.df[k,name.traits[i]] = val.out
# 	}
# 	values.out[[i]] = out.df
# }

# # Merge successively
# all.merge = values.out[[1]]
# for (i in 2:length(values.out)) {
# 	all.merge = merge(all.merge,values.out[[i]],by="species",all=TRUE)
# 	all.merge[is.na(all.merge$Genus.x),"Genus.x"] = all.merge[is.na(all.merge$Genus.x),"Genus.y"]
# 	all.merge[is.na(all.merge$Family.x),"Family.x"] = all.merge[is.na(all.merge$Family.x),"Family.y"]
# 	all.merge[,c("Genus.y","Family.y")] = NULL
# 	names(all.merge)[2:3] = c("Genus","Family")
# }

# # Extract in several columns the microhabitat (no "(" or subspecies I checked)
# microH = read.csv2("./spider/x_miha_microhabitat.csv",sep=",")[,c("originalName","genus","family","value")]
# microH = microH[!microH$value%in%"access restricted",]
# hab.split = lapply(microH$value,function(x) strsplit(x,", ")[[1]])
# hab.uniq = sort(unique(unlist(hab.split)))
# hab.col = gsub(" ","",paste0("HAB_",str_to_title(hab.uniq)))
# microH[,hab.col] = 0
# microH[is.na(microH$value),hab.col] = NA
# for (i in 1:length(hab.uniq)){
# 	h.pos = sapply(hab.split,function(x) any(x%in%hab.uniq[i]))
# 	microH[h.pos,hab.col[i]] = 1
# }

# # Conbine rows of same species and merge to previous data
# microH.u = microH[!duplicated(microH$originalName),]
# names(microH.u)[1] = "species"
# microH.u[,-c(1:4)] = NA
# for (i in 1:nrow(microH.u)){
# 	tar.sp = microH.u$species[i]
# 	ext = microH[microH$originalName%in%tar.sp,-c(1:4)]
# 	ext.sum = apply(ext,2,sum)
# 	ext.sum[ext.sum>=1] = 1
# 	microH.u[microH.u$species%in%tar.sp,-c(1:4)] = ext.sum
# }
# merge.comb = merge(all.merge,microH.u,by="species",all=TRUE)
# merge.comb[is.na(merge.comb$Genus),"Genus"] = merge.comb[is.na(merge.comb$Genus),"genus"]
# merge.comb[is.na(merge.comb$Family),"Family"] = merge.comb[is.na(merge.comb$Family),"family"]
# merge.comb[,c("genus","family")] = NULL
# merge.comb$Fos = 0
# merge.comb$Ter = 0
# merge.comb$Aqu = 0
# merge.comb$Arb = 0
# merge.comb$Aer = 0
# merge.comb[merge.comb$HAB_Caves%in%1,"Fos"] = 1
# merge.comb[merge.comb$HAB_Sand%in%1,"Ter"] = 1
# merge.comb[merge.comb$HAB_Ground%in%1,"Ter"] = 1
# merge.comb[merge.comb$HAB_Litter%in%1,"Ter"] = 1
# merge.comb[merge.comb$HAB_Rocks%in%1,"Ter"] = 1
# merge.comb[merge.comb$HAB_Foliage%in%1,"Arb"] = 1
# merge.comb[merge.comb$HAB_Bark%in%1,"Arb"] = 1
# merge.comb[merge.comb$HAB_Herbs%in%1,"Ter"] = 1
# merge.comb[is.na(merge.comb$value),c("Fos","Ter","Aqu","Arb","Aer")] = NA
# merge.comb$value = NULL

# # Merge with Gossner
# goss.spider = goss.arthrop[goss.arthrop$Order%in%"Araneae",]
# swiss.match = merge(merge.comb,goss.spider,by="species",all=TRUE)
# swiss.match[is.na(swiss.match$Family.x),"Family.x"] = swiss.match[is.na(swiss.match$Family.x),"Family.y"]
# swiss.match[is.na(swiss.match$Fos.x),"Fos.x"] = swiss.match[is.na(swiss.match$Fos.x),"Fos.y"]
# swiss.match[swiss.match$Fos.y%in%1,"Fos.x"] = 1
# swiss.match[is.na(swiss.match$Ter.x),"Ter.x"] = swiss.match[is.na(swiss.match$Ter.x),"Ter.y"]
# swiss.match[swiss.match$Ter.y%in%1,"Ter.x"] = 1
# swiss.match[is.na(swiss.match$Aqu.x),"Aqu.x"] = swiss.match[is.na(swiss.match$Aqu.x),"Aqu.y"]
# swiss.match[swiss.match$Aqu.y%in%1,"Aqu.x"] = 1
# swiss.match[is.na(swiss.match$Arb.x),"Arb.x"] = swiss.match[is.na(swiss.match$Arb.x),"Arb.y"]
# swiss.match[swiss.match$Arb.y%in%1,"Arb.x"] = 1
# swiss.match[is.na(swiss.match$Aer.x),"Aer.x"] = swiss.match[is.na(swiss.match$Aer.x),"Aer.y"]
# swiss.match[swiss.match$Aer.y%in%1,"Aer.x"] = 1
# swiss.match[is.na(swiss.match$BodyLength_mm),"BodyLength_mm"] = swiss.match[is.na(swiss.match$BodyLength_mm),"Body_Size"]
# swiss.match[,c("Body_Size","Family.y","Fos.y","Ter.y","Aqu.y","Arb.y","Aer.y")] = NULL
# names(swiss.match) = gsub("\\.x","",names(swiss.match))
# names(swiss.match)[names(swiss.match)%in%"RangeSize_km2"] = "RangeSize_RM"

# # Merge with Logghe & al. 2025
# logghe.dat = logghe.new[grepl("Arachnida",names(logghe.new))][[1]]
# logghe.m = merge(swiss.match,logghe.dat,by.x="species",by.y="Species",all=TRUE)
# logghe.m[is.na(logghe.m$Family.x),"Family.x"] = logghe.m[is.na(logghe.m$Family.x),"Family.y"]
# logghe.m[is.na(logghe.m$Order.x),"Order.x"] = logghe.m[is.na(logghe.m$Order.x),"Order.y"]
# #logghe.m[is.na(logghe.m$Class.x),"Class.x"] = logghe.m[is.na(logghe.m$Class.x),"Class.y"]
# logghe.m[is.na(logghe.m$Activity.x),"Activity.x"] = logghe.m[is.na(logghe.m$Activity.x),"Activity.y"]
# logghe.m$Activity.x = str_to_title(logghe.m$Activity.x)
# logghe.m[is.na(logghe.m$BodyLength_mm.x),"BodyLength_mm.x"] = logghe.m[is.na(logghe.m$BodyLength_mm.x),"BodyLength_mm.y"]
# logghe.m[is.na(logghe.m$Dispersal_ability.x),"Dispersal_ability.x"] = logghe.m[is.na(logghe.m$Dispersal_ability.x),"Dispersal_ability.y"]
# logghe.m = logghe.m[!grepl("\\.y",names(logghe.m))]
# names(logghe.m) = gsub("\\.x","",names(logghe.m))
# logghe.m[is.na(logghe.m$Imago_Diet),"Imago_Diet"] = logghe.m[is.na(logghe.m$Imago_Diet),"Diet"]
# logghe.m$Diet = NULL
# logghe.m[logghe.m$Imago_Diet%in%"Carnivorous","Diet_Carniv"] = 1
# logghe.m[logghe.m$Imago_Diet%in%"Scavenger","Diet_Scav"] = 1
# logghe.m[logghe.m$Imago_Diet%in%"Fungivorous","Diet_Fungi"] = 1
# logghe.m[logghe.m$Imago_Diet%in%"Herbivorous",c("Diet_Grass","Diet_Leaf")] = 1
# logghe.m[logghe.m$Imago_Diet%in%"Palynivorous","Diet_Flower"] = 1

# # Add new dataset on subterranean spiders!
# sub.spiders = as.data.frame(read_excel("./spider/eu_subterr/Database_Mammola_et_al_2022_Figshare_V2.xlsx"))
# sub.spiders = sub.spiders[!all_duplicated(sub.spiders$Genus_species),]
# spider.final = merge(logghe.m,sub.spiders,by.x="species",by.y="Genus_species",all=TRUE)
# 	# General
# spider.final[is.na(spider.final$Genus.x),"Genus.x"] = spider.final[is.na(spider.final$Genus.x),"Genus.y"]
# spider.final[is.na(spider.final$Family.x),"Family.x"] = spider.final[is.na(spider.final$Family.x),"Family.y"]
# names(spider.final) = gsub("\\.x","",names(spider.final))
# 	# Fos / Ter
# spider.final[spider.final$Ecological_classification%in%"Troglobiont","Fos"] = 1
# spider.final[spider.final$Ecological_classification%in%"Troglobiont","Ter"] = 0
# spider.final[spider.final$Ecological_classification%in%"Troglophile",c("Fos","Ter")] = 1
# names(spider.final)[grepl("Cave_|External|AME_type|AME-ALE|PME-PLE",names(spider.final))] =
# 	c("Cave_deep","Cave_shallow","Cave_outside","Ame_type","Ame-Ale","Pme-Ple")
# 	# BodyLength
# spider.final[is.na(spider.final$BodyLength_mm),"BodyLength_mm"] =
# 	spider.final[is.na(spider.final$BodyLength_mm),"Body_length_avg"]
# spider.final[is.na(spider.final$BodyLength_male),"BodyLength_male"] =
# 	spider.final[is.na(spider.final$BodyLength_male),"Body_length_avg_m"]
# spider.final[is.na(spider.final$BodyLength_female),"BodyLength_female"] =
# 	spider.final[is.na(spider.final$BodyLength_female),"Body_length_avg_f"]
# spider.final$Body_length_min = (as.numeric(spider.final$Body_length_min_m) + as.numeric(spider.final$Body_length_min_f)) / 2
# spider.final[is.na(spider.final$BodyLength_min),"BodyLength_min"] =
# 	spider.final[is.na(spider.final$BodyLength_min),"Body_length_min"]
# spider.final$Body_length_max = (as.numeric(spider.final$Body_length_max_m) + as.numeric(spider.final$Body_length_max_f)) / 2
# spider.final[is.na(spider.final$BodyLength_max),"BodyLength_max"] =
# 	spider.final[is.na(spider.final$BodyLength_max),"Body_length_max"]
# 	# Prosoma
# spider.final[is.na(spider.final$Cephalothor_length_mm),"Cephalothor_length_mm"] =
# 	spider.final[is.na(spider.final$Cephalothor_length_mm),"Prosoma_length_avg"]
# spider.final[is.na(spider.final$Cephalothor_width_mm),"Cephalothor_width_mm"] =
# 	spider.final[is.na(spider.final$Cephalothor_width_mm),"Prosoma_width_avg"]
# spider.final[is.na(spider.final$Cephalothor_height_mm),"Cephalothor_height_mm"] =
# 	spider.final[is.na(spider.final$Cephalothor_height_mm),"Prosoma_height_avg"]
# 	# Femur I
# spider.final[is.na(spider.final$Femur1_length_mm),"Femur1_length_mm"] =
# 	spider.final[is.na(spider.final$Femur1_length_mm),"Femur_I_length_avg"]
# 	# Tibia 
# spider.final[is.na(spider.final$Tibia1_length_mm),"Tibia1_length_mm"] =
# 	spider.final[is.na(spider.final$Tibia1_length_mm),"Tibia_I_length_avg"]
# 	# Fang
# spider.final[is.na(spider.final$Fang_length_mm),"Fang_length_mm"] =
# 	spider.final[is.na(spider.final$Fang_length_mm),"Fang_length_avg"]
# 	# AME, ALE, PME, PLE
# spider.final[is.na(spider.final$Ame_mm),"Ame_mm"] =
# 	spider.final[is.na(spider.final$Ame_mm),"AME"]
# spider.final[is.na(spider.final$Ale_mm),"Ale_mm"] =
# 	spider.final[is.na(spider.final$Ale_mm),"ALE"]
# spider.final[is.na(spider.final$Pme_mm),"Pme_mm"] =
# 	spider.final[is.na(spider.final$Pme_mm),"PME"]
# spider.final[is.na(spider.final$Ple_mm),"Ple_mm"] =
# 	spider.final[is.na(spider.final$Ple_mm),"PLE"]
# 	# Map from Hunting_guild1
# spider.final$Aerial_hunter   [spider.final$Hunting_guild1%in%"aerial hunter"] =  1
# spider.final$Ground_hunter   [spider.final$Hunting_guild1%in%"ground hunter"] =  1
# spider.final$Ambush_hunter   [spider.final$Hunting_guild1%in%"ambush hunter"] =  1
# spider.final$Active_hunter   [spider.final$Hunting_guild1%in%"active hunter"] =  1
# spider.final$Other_hunter    [spider.final$Hunting_guild1%in%"other hunters"] =  1
# spider.final$Food_specialist [spider.final$Hunting_guild1%in%"specialists"] =  1
# spider.final$Orb_web         [spider.final$Hunting_guild1%in%"orb web"] =  1
# spider.final$Space_web       [spider.final$Hunting_guild1%in%"space web"] =  1
# spider.final$Sheet_web       [spider.final$Hunting_guild1%in%"sheet web"] =  1
# spider.final$Sensing_web     [spider.final$Hunting_guild1%in%"sensing web"] = 1
# 	# Map from Hunting_guild2
# spider.final$Aerial_web_builder[spider.final$Hunting_guild2%in%"aerial web builder"] =  1
# spider.final$Burrower        [spider.final$Hunting_guild2%in%"burrower"] =  1
# spider.final$Tube_web        [spider.final$Hunting_guild2%in%"burrower"] =  1
# spider.final$Foliage_runner  [spider.final$Hunting_guild2%in%"foliage runner"] =  1
# spider.final$Ground_runner   [spider.final$Hunting_guild2%in%"ground runner"] =  1
# spider.final$Sheet_web       [spider.final$Hunting_guild2%in%"sheet web builder"] =  1
# spider.final$Capture_web     [spider.final$Hunting_guild2%in%"sheet web builder"] =  1
# spider.final$Ambush_hunter   [spider.final$Hunting_guild2%in%"ambush hunter"] =  1
# 	# Fallback: Guild
# spider.final$Ambush_hunter   [spider.final$Guild%in%"Ambush"] 	  =  1
# spider.final$Ground_hunter   [spider.final$Guild%in%"Ground"]     =  1
# spider.final$Orb_web         [spider.final$Guild%in%"Orb"]        =  1
# spider.final$Space_web       [spider.final$Guild%in%"Space"]      =  1
# spider.final$Sheet_web       [spider.final$Guild%in%"Sheet"]      =  1
# spider.final$Sensing_web     [spider.final$Guild%in%"Sensing"]    =  1
# spider.final$Other_hunter    [spider.final$Guild%in%"Other"]      =  1
# spider.final$Food_specialist [spider.final$Guild%in%"Specialist"] =  1
# 	# Derived umbrella columns
# 		# Capture_web = all web builders
# spider.final$Capture_web[spider.final$Orb_web%in%1 |
#                          spider.final$Space_web%in%1 |
#                          spider.final$Sheet_web%in%1 |
#                          spider.final$Tube_web%in%1] =  1
# spider.final$Capture_web[spider.final$Web_building%in%"yes"] = 1
# 		# Aerial_web_builder = Orb or Space
# spider.final$Aerial_web_builder[spider.final$Orb_web%in%1 |
#                                 spider.final$Space_web%in%1] =  1
# 		# No_web = all hunters
# spider.final$No_web[spider.final$Ambush_hunter%in%1 |
#                     spider.final$Active_hunter%in%1 |
#                     spider.final$Other_hunter%in%1 |
#                     spider.final$Ground_hunter%in%1 |
#                     spider.final$Aerial_hunter%in%1 |
#                     spider.final$Food_specialist%in%1] = 1
#         # Assign 0 to rows with information
# pred.cols = c("Ground_hunter","Active_hunter","Ambush_hunter","Aerial_hunter","Other_hunter",
# 	"Ground_runner","Foliage_runner","Burrower","Food_specialist","Capture_web","Aerial_web_builder",
# 	"Sheet_web","Space_web","Orb_web","Tube_web","Sensing_web","No_web")
# spider.final[,pred.cols][is.na(spider.final[,pred.cols])] = 0
# spider.final[apply(spider.final[,pred.cols],1,function(x) all(x%in%0)),pred.cols] = NA

# # Keep the IUCN range in priority
# spider.range = iucn.ranges[grepl("SPIDER",names(iucn.ranges))][[1]]
# spider.iucn = merge(spider.final,spider.range[,c("species","range.size.km2")],by="species",all=TRUE)
# spider.iucn$RangeSize_km2 = spider.iucn$range.size.km2

# # Change names if necessary
# tGS = to_GenusSpecies(spider.iucn$species)
# spider.iucn$species = tGS[,"CanonicalName"]
# spider.iucn = spider.iucn[!is.na(spider.iucn$species),]
# spider.iucn = combine_rows(spider.iucn,"species") # Unique names now

# # Reorder columns
# spider.vert = spider.iucn[,c(logghe.common,"Stratum","Cave_deep","Cave_shallow","Cave_outside","Mode_locomotion",
# 	"Egg_size_mm","Cribellum","Pigment","Abdomen_length_mm","Abdomen_height_mm","Abdomen_width_mm","Eyes_regression",
# 	"Eyeless","Ame_type","Ame_mm","Ale_mm","Ple_mm","Pme_mm","Ame-Ale","Pme-Ple","Cephalothor_length_mm",
# 	"Cephalothor_height_mm","Cephalothor_width_mm","Leg_elongation","Leg1_length_mm","Leg2_length_mm","Leg3_length_mm",
# 	"Leg4_length_mm","Femur1_length_mm","Femur1_width_mm","Femur2_length_mm","Femur3_length_mm","Femur4_length_mm",
# 	"Femur4_width_mm","Patella1_length_mm","Patella2_length_mm","Patella3_length_mm","Patella4_length_mm",
# 	"Tibia1_length_mm","Tibia1_width_mm","Tibia2_length_mm","Tibia3_length_mm","Tibia4_length_mm","Tibia4_width_mm",
# 	"Metatarsus1_length_mm","Metatarsus2_length_mm","Metatarsus3_length_mm","Metatarsus4_length_mm","Tarsus1_length_mm",
# 	"Tarsus2_length_mm","Tarsus3_length_mm","Tarsus4_length_mm","Ballooning","Fang_length_mm",
# 	"Venom_gland_size_mm3","Prey_size_mm","Prey_diversity",pred.cols,"Moisture_class","Light_class","HAB_Caves","HAB_Sand",
# 	"HAB_Ground","HAB_Litter","HAB_Rocks","HAB_Foliage","HAB_Bark","HAB_Herbs")]
# spider.vert[spider.vert==""] = NA

# # Convert to numeric
# non.num = c("species","Genus","Family","Order","Stratum","Mode_locomotion","Web_building","Ballooning",
# 	"Activity","Cribellum","Larva_Diet","Imago_Diet","Hunting_guild1","Hunting_guild2","Moisture_class","Light_class",
# 	"IUCN_status","Larva_Diet_range","Imago_Diet_range","Pigment","")
# spider.vert[,!names(spider.vert)%in%non.num] = sapply(spider.vert[,!names(spider.vert)%in%non.num],as.numeric)

# # Fill in additonal logical information
# spider.vert$Aer = 0

# # Add gbif information
# spider.vert[,GB.col] = NA
# for (i in 1:nrow(spider.vert)){
#     cat("...sp.",i)
#     gbif.search = get_status(spider.vert[i,"species"],class="Arachnida") # Search for accepted names
#     if (!is.null(gbif.search)){
#     	spider.vert$gbif_accepted[i] = gbif.search$scientificName[1] # Fill in the result
#       	spider.vert$gbif_rank[i] = gbif.search$rank[1]
#         spider.vert$gbif_phylum[i] = gbif.search$Phylum[1]
#         spider.vert$gbif_genus[i] = gbif.search$Genus[1]
#         spider.vert$gbif_order[i] = gbif.search$Order[1]
#         spider.vert$gbif_family[i] = gbif.search$Family[1]
#         spider.vert$gbif_class[i] = gbif.search$Class[1]
#         spider.vert$gbif_IUCN[i] = gbif.search$IUCN_status[1]
#         }
# }
# write.table(spider.vert,"temp_spider.txt",row.names=FALSE)
spider.vert = read.table("data/temp/temp_spider.txt",header=TRUE)

# Remove non spiders
not.spiders = c("Amblypygi","Holothyrida","Pseudoscorpiones","Sarcoptiformes","Schizomida","Scorpiones")
spider.vert = spider.vert[!spider.vert$gbif_order%in%not.spiders,]

# Format our table
which.gbif = which(names(spider.vert)%in%GB.col)
spider.vert = spider.vert[,c(1,which.gbif,2:(ncol(spider.vert)-length(which.gbif)))]
spider.vert[is.na(spider.vert$gbif_genus),"gbif_genus"] = spider.vert[is.na(spider.vert$gbif_genus),"Genus"]
spider.vert$gbif_genus[is.na(spider.vert$gbif_genus)] =
	unlist(sapply(strsplit(spider.vert$species[is.na(spider.vert$gbif_genus)]," "),function(x) x[1]))
spider.vert[is.na(spider.vert$gbif_family),"gbif_family"] = spider.vert[is.na(spider.vert$gbif_family),"Family"]
spider.vert$gbif_order = "Araneae"
spider.vert$gbif_class = "Arachnida"
spider.vert$gbif_phylum = "Arthropoda"
spider.vert[is.na(spider.vert$IUCN_status),"IUCN_status"] = spider.vert[is.na(spider.vert$IUCN_status),"gbif_IUCN"]
spider.vert[,c("Genus","Family","Order","Class","Phylum","gbif_IUCN")] = NULL
names(spider.vert)[2:length(GB.col)] = new.col
gbif.cond = spider.vert$IUCN_status%in%c("NOT_FOUND","NOT_EVALUATED")
spider.vert[gbif.cond,"IUCN_status"] = NA
spider.vert[spider.vert$Alien%in%1,"originEUR"] = "No"

# Extract EU checklist and merge
s2z.spider = s2z.sp[s2z.sp$group%in%"Spiders",]
spider.merge1 = merge(s2z.spider,spider.vert,by="species")
spider.vert = spider.vert[!spider.vert$species%in%spider.merge1$species,]
s2z.spider2 = s2z.spider[!s2z.spider$species%in%spider.merge1$species,]

# Regroup all the available information for the accepted species (mean when continuous values)
mam.ndup = spider.vert[!(duplicated(spider.vert$gbif_accepted)|duplicated(spider.vert$gbif_accepted,fromLast=TRUE)),]
mam.dup = spider.vert[duplicated(spider.vert$gbif_accepted)|duplicated(spider.vert$gbif_accepted,fromLast=TRUE),]
mam.homo = combine_rows(df=mam.dup,ref.col="gbif_accepted",binary=FALSE)
mam.homo = mam.homo[!is.na(mam.homo$gbif_accepted),]

# Combine, attempt another merge and rbind both results
mam.temp = rbind(mam.ndup,mam.homo)
spider.merge2 = merge(s2z.spider2,mam.temp,by="gbif_accepted",all.x=TRUE)
names(spider.merge2)[1:2] = c("gbif_accepted.x","species")
spider.merge2$gbif_accepted.y = spider.merge2$gbif_accepted.x
spider.merge2 = spider.merge2[,names(spider.merge1)]
spider.MM = rbind(spider.merge1,spider.merge2)

# Try to fill in mising information fron one column to another
spider.MM[is.na(spider.MM$genus),"genus"] = spider.MM[is.na(spider.MM$genus),"Genus"]
spider.MM[is.na(spider.MM$family),"family"] = spider.MM[is.na(spider.MM$family),"Family"]
spider.MM[is.na(spider.MM$order),"order"] = spider.MM[is.na(spider.MM$order),"Order"]
spider.MM[is.na(spider.MM$class),"class"] = spider.MM[is.na(spider.MM$class),"Class"]
spider.MM[is.na(spider.MM$phylum),"phylum"] = spider.MM[is.na(spider.MM$phylum),"Phylum"]
spider.MM[grepl("unresolved_accepted",spider.MM$gbif_accepted.x),"gbif_accepted.x"] =
	spider.MM[grepl("unresolved_accepted",spider.MM$gbif_accepted.x),"gbif_accepted.y"]
spider.MM$gbif_accepted.x[is.na(spider.MM$gbif_accepted.x)] =
	paste0("unresolved_accepted2_", seq_len(sum(is.na(spider.MM$gbif_accepted.x))))
spider.MM[is.na(spider.MM$taxonRank),"taxonRank"] = spider.MM[is.na(spider.MM$taxonRank),"gbif_rank"]
spider.MM[is.na(spider.MM$iucnRedListCategory),"iucnRedListCategory"] =
	spider.MM[is.na(spider.MM$iucnRedListCategory),"IUCN_status"]
toRemove = grepl("\\.y",names(spider.MM)) |
	names(spider.MM)%in%c("Genus","Family","Order","Class","Phylum","gbif_rank","IUCN_status")
spider.MM[,toRemove] = NULL
names(spider.MM)[1:ncol(s2z.spider2)] = names(s2z.spider2)

# Fine tune Ter/Aqu/Arb info (a)
guild.ter = apply(spider.MM[,sprintf("GUILDE.%d",c(3,10,12,14:15,20:22,24:25))],1,sum)
spider.MM[guild.ter>0&!is.na(guild.ter),"Ter"] = 1
guild.aqu = apply(spider.MM[,sprintf("GUILDE.%d",c(1,2,4:9,26))],1,sum)
spider.MM[guild.aqu>0&!is.na(guild.aqu),"Aqu"] = 1
guild.arb = apply(spider.MM[,sprintf("GUILDE.%d",c(11,13,16:19,23))],1,sum)
spider.MM[guild.arb>0&!is.na(guild.arb),"Arb"] = 1

# Fine tune habitat simple
hab.wat = apply(spider.MM[,sprintf("GUILDE.%d",c(1,2,4,5,8))],1,sum)
spider.MM[hab.wat>0 & !is.na(hab.wat),"Habitat_water"] = 1
spider.MM[hab.wat%in%0 & !spider.MM$Habitat_water%in%1,"Habitat_water"] = 0
hab.wet = apply(spider.MM[,sprintf("GUILDE.%d",c(6,7,9,26))],1,sum)
spider.MM[hab.wet>0 & !is.na(hab.wet),"Habitat_wetlands"] = 1
spider.MM[hab.wet%in%0 & !spider.MM$Habitat_wetlands%in%1,"Habitat_wetlands"] = 0
spider.MM[spider.MM[,"GUILDE.21"]>0 & !is.na(spider.MM[,"GUILDE.21"]),"Habitat_rocks"] = 1
spider.MM[spider.MM[,"GUILDE.21"]%in%0 & !spider.MM$Habitat_rocks%in%1,"Habitat_rocks"] = 0
hab.grass = apply(spider.MM[,sprintf("GUILDE.%d",c(14,20))],1,sum)
spider.MM[hab.grass>0 & !is.na(hab.grass),"Habitat_grassland"] = 1
spider.MM[hab.grass%in%0 & !spider.MM$Habitat_grassland%in%1,"Habitat_grassland"] = 0
hab.forest = apply(spider.MM[,sprintf("GUILDE.%d",c(16,17,19))],1,sum)
spider.MM[hab.forest>0 & !is.na(hab.forest),"Habitat_forests"] = 1
spider.MM[hab.forest%in%0 & !spider.MM$Habitat_forests%in%1,"Habitat_forests"] = 0
spider.MM[spider.MM[,"GUILDE.17"]>0 & !is.na(spider.MM[,"GUILDE.17"]),"Habitat_deciduous"] = 1
spider.MM[spider.MM[,"GUILDE.17"]%in%0 & !spider.MM$Habitat_deciduous%in%1,"Habitat_deciduous"] = 0
spider.MM[spider.MM[,"GUILDE.19"]>0 & !is.na(spider.MM[,"GUILDE.19"]),"Habitat_coniferous"] = 1
spider.MM[spider.MM[,"GUILDE.19"]%in%0 & !spider.MM$Habitat_coniferous%in%1,"Habitat_coniferous"] = 0
spider.MM[spider.MM[,"GUILDE.16"]>0 & !is.na(spider.MM[,"GUILDE.16"]),"Habitat_mixed"] = 1
spider.MM[spider.MM[,"GUILDE.16"]%in%0 & !spider.MM$Habitat_mixed%in%1,"Habitat_mixed"] = 0
hab.pioneer = apply(spider.MM[,sprintf("GUILDE.%d",c(3,10,12,22))],1,sum)
spider.MM[hab.pioneer>0 & !is.na(hab.pioneer),"Habitat_pioneer"] = 1
spider.MM[hab.pioneer%in%0 & !spider.MM$Habitat_pioneer%in%1,"Habitat_pioneer"] = 0
hab.urban = apply(spider.MM[,sprintf("GUILDE.%d",c(22,24))],1,sum)
spider.MM[hab.urban>0 & !is.na(hab.urban),"Habitat_urban"] = 1
spider.MM[hab.urban%in%0 & !spider.MM$Habitat_urban%in%1,"Habitat_urban"] = 0
spider.MM[spider.MM[,"GUILDE.23"]>0 & !is.na(spider.MM[,"GUILDE.23"]),"Habitat_green_urban"] = 1
spider.MM[spider.MM[,"GUILDE.23"]%in%0 & !spider.MM$Habitat_green_urban%in%1,"Habitat_green_urban"] = 0
hab.agri = apply(spider.MM[,sprintf("GUILDE.%d",c(11,25))],1,sum)
spider.MM[hab.agri>0 & !is.na(hab.agri),"Habitat_agricultural"] = 1
spider.MM[hab.agri%in%0 & !spider.MM$Habitat_agricultural%in%1,"Habitat_agricultural"] = 0
spider.MM[spider.MM[,"GUILDE.13"]>0 & !is.na(spider.MM[,"GUILDE.13"]),"Habitat_bushes"] = 1
spider.MM[spider.MM[,"GUILDE.13"]%in%0 & !spider.MM$Habitat_bushes%in%1,"Habitat_bushes"] = 0
spider.MM[spider.MM[,"GUILDE.15"]>0 & !is.na(spider.MM[,"GUILDE.15"]),"Habitat_transitional"] = 1
spider.MM[spider.MM[,"GUILDE.15"]%in%0 & !spider.MM$Habitat_transitional%in%1,"Habitat_transitional"] = 0
spider.MM[spider.MM[,"GUILDE.18"]>0 & !is.na(spider.MM[,"GUILDE.18"]),"Habitat_heathland"] = 1
spider.MM[spider.MM[,"GUILDE.18"]%in%0 & !spider.MM$Habitat_heathland%in%1,"Habitat_heathland"] = 0
spider.MM[,HABit[!HABit%in%names(spider.MM)]] = NA
spider.MM[,names(spider.MM)[grepl("GUILD",names(spider.MM))]] = NULL

# Fine tune habitat worldspider
#spider.MM[spider.MM$,"Habitat_urban"]
#spider.MM[spider.MM$,"Habitat_green_urban"]
#spider.MM[spider.MM$,"Habitat_agricultural"]
#spider.MM[spider.MM$,"Habitat_deciduous"]
#spider.MM[spider.MM$,"Habitat_coniferous"]
#spider.MM[spider.MM$,"Habitat_mixed"]
spider.MM[spider.MM$HAB_Herbs%in%1,"Habitat_grassland"] = 1
spider.MM[spider.MM$HAB_Herbs%in%0 & is.na(spider.MM$Habitat_grassland),"Habitat_grassland"] = 0
spider.MM[spider.MM$HAB_Foliage%in%1,"Habitat_bushes"] = 1
spider.MM[spider.MM$HAB_Foliage%in%0,"Habitat_bushes"] = 0
#spider.MM[spider.MM$,"Habitat_transitional"]
spider.MM[spider.MM$HAB_Sand%in%1,"Habitat_sand"] = 1
spider.MM[spider.MM$HAB_Sand%in%0  & is.na(spider.MM$Habitat_sand),"Habitat_sand"] = 0
#spider.MM[spider.MM$,"Habitat_wetlands"]
#spider.MM[spider.MM$,"Habitat_water"] # no info for Aqu in world spider trait database
spider.MM[spider.MM$HAB_Rocks%in%1,"Habitat_rocks"] = 1
spider.MM[spider.MM$HAB_Caves%in%1,"Habitat_rocks"] = 1
spider.MM[spider.MM$HAB_Rocks%in%0 & spider.MM$HAB_Caves%in%0,"Habitat_rocks"] = 0
spider.MM = spider.MM[,!grepl("HAB_",names(spider.MM))]

# Fine tune Ter/Aqu/Arb info (b)
spider.MM[spider.MM$Habitat_urban%in%1,"Ter"] = 1
spider.MM[spider.MM$Habitat_green_urban%in%1,"Ter"] = 1
spider.MM[spider.MM$Habitat_agriculture%in%1,"Ter"] = 1
spider.MM[spider.MM$Habitat_deciduous%in%1,"Arb"] = 1
spider.MM[spider.MM$Habitat_coniferous%in%1,"Arb"] = 1
spider.MM[spider.MM$Habitat_mixed%in%1,"Arb"] = 1
spider.MM[spider.MM$Habitat_grassland%in%1,"Ter"] = 1
spider.MM[spider.MM$Habitat_heathland%in%1,"Arb"] = 1
spider.MM[spider.MM$Habitat_bushes%in%1,"Arb"] = 1
spider.MM[spider.MM$Habitat_sand%in%1,"Ter"] = 1
spider.MM[spider.MM$Habitat_wetlands%in%1,"Aqu"] = 1
spider.MM[spider.MM$Habitat_water%in%1,"Aqu"] = 1
spider.MM[spider.MM$Habitat_rocks%in%1,"Ter"] = 1
spider.MM[spider.MM$Habitat_pioneer%in%1,"Ter"] = 1
l.temp = spider.MM[,c("Ter","Aqu","Arb")] # We don't take Fos because no way of knowing if really "0"
l.temp[is.na(l.temp)] = 0
l.temp[apply(l.temp,1,function(x) all(x%in%0)),] = NA
spider.MM[,c("Ter","Aqu","Arb")] = l.temp
spider.MM[spider.MM$Fos%in%1,"Habitat_rocks"] = 1 # Habitat_rocks inlcude caves here

# Aquatic, Terrestrial or Marine
spider.MM[,"EcoMar"]= NA
spider.MM$EcoTer = 1
spider.MM$EcoFresh = 0
spider.MM$EcoMar = 0
spider.MM$Aer = 0
spider.MM[spider.MM$species%in%"Argyroneta aquatica",c("Aqu","EcoFresh")] = 1
spider.MM[spider.MM$species%in%"Argyroneta aquatica","EcoTer"] = 0
spider.MM[spider.MM$genus%in%c("Desis","Paratheuma"),c("Aqu","EcoMar")] = 1 #
spider.MM[spider.MM$genus%in%c("Desis","Paratheuma"),c("EcoTer","EcoFresh")] = 0 # Marine species that we don't have
spider.MM[spider.MM$genus%in%c("Dolomedes","Ancylometes","Nilus","Trechalea","Pirata"),c("Aqu","EcoFresh")] = 1
# we don't have Ancylometes, Nilus & Trechalea

# Fine tune Ter/Aqu info (c)
spider.MM = spider.MM[order(spider.MM$species),]
mf1 = merge(spider.MM,antoine.TerAqu,by="species",all.x=TRUE)
all(spider.MM$species == mf1$species) # auto check
spider.MM[mf1$scheme%in%"terrestrial"&is.na(spider.MM$Ter),"Ter"] = 1
spider.MM[mf1$scheme%in%"aquatic",c("Aqu","EcoFresh")] = 1
spider.MM[spider.MM$EcoFresh%in%1,"Aqu"] = 1

# Reorder & Save
hab.n = grep("Habitat_",names(spider.MM))
eco.n = grep("Eco",names(spider.MM))
col.ref = c("species","scientificName","taxonRank","gbif_status","gbif_accepted","genus","family","order",
	"class","phylum","group","Source","originCH","originEUR","swissRedListCategory","europeanRegionalRedListCategory",
	"iucnRedListCategory","EcoTer","EcoFresh","EcoMar","Fos","Ter","Aqu","Arb","Aer",
	names(spider.MM[,hab.n]),"RangeSize_km2")
spider.MM = spider.MM[,c(col.ref,names(spider.MM)[!names(spider.MM)%in%col.ref])]
write.table(spider.MM,"outputs/raw_traits/non_formated/s2z_raw_traits_spiders.txt",row.names=FALSE)


### ==========================================================================================================================
### Insects
### ==========================================================================================================================


####################################
########### COLEOPTERA #############
####################################


# # Specifically select coleopter in the s2z and vertlife taxa + IUCN ranges
# goss.coleo2 = goss.arthrop[goss.arthrop$Order%in%"Coleoptera",]

# # Open flora indicativa (R version) and add our final modifs
# fi.coleo = as.data.frame(read_excel("./insects/fauna_indicativa/fauna_indicativa_Coleoptera_for_R.xlsx",1,
# 	col_types=c(rep("text",5),rep("numeric",32),"text","numeric",rep("text",2),rep("numeric",8))))
# fi.coleo$species = str_squish(paste(fi.coleo$Genus,fi.coleo$Species,fi.coleo$Subspecies))
# fi.coleo$species = gsub(" NA","",fi.coleo$species)
# HAB.ref = gsub("\\..*","",colnames(fi.coleo)[grepl("Habitat_",colnames(fi.coleo))])
# HAB.dup = unique(HAB.ref[duplicated(HAB.ref)])
# for (i in 1:length(HAB.dup)){fi.coleo[,HAB.dup[i]] = as.numeric(apply(fi.coleo[,grepl(HAB.dup[i],colnames(fi.coleo))],1,sum)>0)}
# fi.coleo[grepl("\\.\\.\\.",names(fi.coleo))] = NULL
# fi.coleo[,c("Species","Subspecies","Author")] = NULL
# fi.coleo$Habitat_forests = fi.coleo$Habitat_deciduous
# fi.coleo[fi.coleo$Habitat_coniferous%in%1,"Habitat_forests"] = 1
# fi.coleo[fi.coleo$Habitat_mixed%in%1,"Habitat_forests"] = 1

# # Open the beetle database and keep interesting columns
# goss.coleo = read.csv("./insects/Beetles_eu/Hagge_etal_saproxylic_beetle_morphological_trait_database_20210426.csv")
# goss.coleo[is.na(goss.coleo$body_length),"body_length"] = goss.coleo[is.na(goss.coleo$body_length),"mean_body_length_literature"]
# goss.coleo$front_leg_length = goss.coleo$front_femur_length + goss.coleo$front_tibia_length
# goss.coleo$mid_leg_length = goss.coleo$mid_femur_length + goss.coleo$mid_tibia_length
# goss.coleo$hind_leg_length = goss.coleo$hind_femur_length + goss.coleo$hind_tibia_length
# goss.coleo = goss.coleo[,c("species","family","colour_lightness","body_length","body_width","body_height","head_length",
# 	"head_width","elytra_length","elytra_width","wing_length","wing_width","wing_area","antenna_length","mandible_length",
# 	"proximal_mandible_width","eye_area","front_leg_length","mid_leg_length","hind_leg_length","hairiness_total")]
# goss.coleo$species = gsub("_"," ",goss.coleo$species)

# # Merge FI with gossner_coleopters
# merge.coleo = merge(fi.coleo,goss.coleo,by="species",all=TRUE)
# merge.coleo[is.na(merge.coleo$BodyLength_mm),"BodyLength_mm"] = merge.coleo[is.na(merge.coleo$BodyLength_mm),"body_length"]
# merge.coleo$body_length = NULL

# # Merge previous with gossner_arthropods
# swiss.match = merge(merge.coleo,goss.coleo2,by="species",all=TRUE)
# swiss.match[is.na(swiss.match$family),"family"] = swiss.match[is.na(swiss.match$family),"Family"]
# swiss.match[is.na(swiss.match$Fos.x),"Fos.x"] = swiss.match[is.na(swiss.match$Fos.x),"Fos.y"]
# swiss.match[is.na(swiss.match$Ter.x),"Ter.x"] = swiss.match[is.na(swiss.match$Ter.x),"Ter.y"]
# swiss.match[is.na(swiss.match$Arb.x),"Arb.x"] = swiss.match[is.na(swiss.match$Arb.x),"Arb.y"]
# swiss.match[is.na(swiss.match$BodyLength_mm),"BodyLength_mm"] = swiss.match[is.na(swiss.match$BodyLength_mm),"Body_Size"]
# swiss.match[,c("Body_Size","Family","Fos.y","Ter.y","Arb.y")] = NULL
# names(swiss.match) = gsub("\\.x","",names(swiss.match))
# swiss.match$EIV_Moisture = normalize(swiss.match$EIV_Moisture)

# # Merge with Logghe & al. 2025
# logghe.dat = logghe.new[grepl("Coleoptera",names(logghe.new))][[1]]
# logghe.m = merge(swiss.match,logghe.dat,by.x="species",by.y="Species",all=TRUE)
# logghe.m[is.na(logghe.m$family),"family"] = logghe.m[is.na(logghe.m$family),"Family"]
# logghe.m$Order.x = "Coleoptera"
# logghe.m[grepl("larva",logghe.m$Overwinter_stage),"Overwintering_larva"] = 1
# logghe.m[grepl("imago",logghe.m$Overwinter_stage),"Overwintering_imago"] = 1
# logghe.m[is.na(logghe.m$BodyLength_mm.x),"BodyLength_mm.x"] = logghe.m[is.na(logghe.m$BodyLength_mm.x),"BodyLength_mm.y"]
# logghe.m[is.na(logghe.m$BodyLength_max.x),"BodyLength_max.x"] = logghe.m[is.na(logghe.m$BodyLength_max.x),"BodyLength_max.y"]
# logghe.m[is.na(logghe.m$BodyLength_min.x),"BodyLength_min.x"] = logghe.m[is.na(logghe.m$BodyLength_min.x),"BodyLength_min.y"]
# logghe.m[is.na(logghe.m$Dispersal_ability.x),"Dispersal_ability.x"] = logghe.m[is.na(logghe.m$Dispersal_ability.x),"Dispersal_ability.y"]
# logghe.m[logghe.m$Wing_morphology.x%in%c("aptery","brachyptery"),"Wing_morphology.x"] = "aptery/brachyptery"
# logghe.m[is.na(logghe.m$Wing_morphology.x),"Wing_morphology.x"] = logghe.m[is.na(logghe.m$Wing_morphology.x),"Wing_morphology.y"]
# logghe.m[is.na(logghe.m$Habitat_urban.x),"Habitat_urban.x"] = logghe.m[is.na(logghe.m$Habitat_urban.x),"Habitat_urban.y"]
# logghe.m[logghe.m$Habitat_urban.y%in%1,"Habitat_urban.x"] = 1
# #logghe.m[is.na(logghe.m$Habitat_green_urban.x),"Habitat_green_urban.x"] = logghe.m[is.na(logghe.m$Habitat_green_urban.x),"Habitat_green_urban.y"]
# logghe.m[is.na(logghe.m$Habitat_agricultural.x),"Habitat_agricultural.x"] = logghe.m[is.na(logghe.m$Habitat_agricultural.x),"Habitat_agricultural.y"]
# logghe.m[logghe.m$Habitat_agricultural.y%in%1,"Habitat_agricultural.x"] = 1
# logghe.m[is.na(logghe.m$Habitat_pioneer.x),"Habitat_pioneer.x"] = logghe.m[is.na(logghe.m$Habitat_pioneer.x),"Habitat_pioneer.y"]
# logghe.m[logghe.m$Habitat_pioneer.y%in%1,"Habitat_pioneer.x"] = 1
# logghe.m[is.na(logghe.m$Habitat_forests.x),"Habitat_forests.x"] = logghe.m[is.na(logghe.m$Habitat_forests.x),"Habitat_forests.y"]
# logghe.m[logghe.m$Habitat_forests.y%in%1,"Habitat_forests.x"] = 1
# logghe.m[is.na(logghe.m$Habitat_deciduous.x),"Habitat_deciduous.x"] = logghe.m[is.na(logghe.m$Habitat_deciduous.x),"Habitat_deciduous.y"]
# logghe.m[logghe.m$Habitat_deciduous.y%in%1,"Habitat_deciduous.x"] = 1
# logghe.m[is.na(logghe.m$Habitat_coniferous.x),"Habitat_coniferous.x"] = logghe.m[is.na(logghe.m$Habitat_coniferous.x),"Habitat_coniferous.y"]
# logghe.m[logghe.m$Habitat_coniferous.y%in%1,"Habitat_coniferous.x"] = 1
# logghe.m[is.na(logghe.m$Habitat_mixed.x),"Habitat_mixed.x"] = logghe.m[is.na(logghe.m$Habitat_mixed.x),"Habitat_mixed.y"]
# logghe.m[logghe.m$Habitat_mixed.y%in%1,"Habitat_mixed.x"] = 1
# logghe.m[is.na(logghe.m$Habitat_grassland.x),"Habitat_grassland.x"] = logghe.m[is.na(logghe.m$Habitat_grassland.x),"Habitat_grassland.y"]
# logghe.m[logghe.m$Habitat_grassland.y%in%1,"Habitat_grassland.x"] = 1
# logghe.m[is.na(logghe.m$Habitat_bushes.x),"Habitat_bushes.x"] = logghe.m[is.na(logghe.m$Habitat_bushes.x),"Habitat_bushes.y"]
# logghe.m[logghe.m$Habitat_bushes.y%in%1,"Habitat_bushes.x"] = 1
# logghe.m[is.na(logghe.m$Habitat_transitional.x),"Habitat_transitional.x"] = logghe.m[is.na(logghe.m$Habitat_transitional.x),"Habitat_transitional.y"]
# logghe.m[logghe.m$Habitat_transitional.y%in%1,"Habitat_transitional.x"] = 1
# logghe.m[is.na(logghe.m$Habitat_wetlands.x),"Habitat_wetlands.x"] = logghe.m[is.na(logghe.m$Habitat_wetlands.x),"Habitat_wetlands.y"]
# logghe.m[logghe.m$Habitat_wetlands.y%in%1,"Habitat_wetlands.x"] = 1
# logghe.m[is.na(logghe.m$Habitat_water.x),"Habitat_water.x"] = logghe.m[is.na(logghe.m$Habitat_water.x),"Habitat_water.y"]
# logghe.m[logghe.m$Habitat_water.y%in%1,"Habitat_water.x"] = 1
# logghe.m[is.na(logghe.m$Habitat_rocks.x),"Habitat_rocks.x"] = logghe.m[is.na(logghe.m$Habitat_rocks.x),"Habitat_rocks.y"]
# logghe.m[logghe.m$Habitat_rocks.y%in%1,"Habitat_rocks.x"] = 1
# logghe.m = logghe.m[!grepl("\\.y",names(logghe.m))]
# names(logghe.m) = gsub("\\.x","",names(logghe.m))
# logghe.m[is.na(logghe.m$Diet),"Diet"] = logghe.m[is.na(logghe.m$Diet),"Imago_Diet"] # Gossner priority for Diet correspd.
# logghe.m[,c("Family","Imago_Diet")] = NULL
# names(logghe.m)[names(logghe.m)%in%"Diet"] = "Imago_Diet"
# logghe.m[logghe.m$Imago_Diet%in%"Carnivorous","Diet_Carniv"] = 1
# logghe.m[logghe.m$Imago_Diet%in%"Scavenger","Diet_Scav"] = 1
# logghe.m[logghe.m$Imago_Diet%in%"Fungivorous","Diet_Fungi"] = 1
# logghe.m[logghe.m$Imago_Diet%in%"Herbivorous",c("Diet_Grass","Diet_Leaf")] = 1
# logghe.m[logghe.m$Imago_Diet%in%"Palynivorous","Diet_Flower"] = 1

# # Keep the IUCN range in priority
# coleo.range = iucn.ranges[grepl("COLEOPTERA",names(iucn.ranges))][[1]]
# coleo.iucn = merge(logghe.m,coleo.range[,c("species","range.size.km2")],by="species",all=TRUE)
# coleo.iucn$RangeSize_km2 = coleo.iucn$range.size.km2

# # Change names if necessary
# tGS = to_GenusSpecies(coleo.iucn$species)
# coleo.iucn$species = tGS[,"CanonicalName"]
# coleo.iucn = coleo.iucn[!is.na(coleo.iucn$species),]
# coleo.iucn$IUCN_status = NA

# # Harmonize names & reorder columns 
# names(coleo.iucn)[33:51] = c("Family","Colour_lightness","BodyWidth_mm","BodyHeight_mm","Head_length_mm","Head_width_mm",
# 	"Elytra_length_mm","Elytra_width_mm","Wing_length_mm","Wing_width_mm","Wing_area_mm2","Antenna_length_mm",
# 	"Mandible_length_mm","Proximal_mandible_width_mm","Eye_area_mm2","Front_leg_length_mm","Mid_leg_length_mm",
# 	"Hind_leg_length_mm","Hairiness_total")
# coleo.vert = coleo.iucn[,c(logghe.common,"Riparian_sp","BodyWidth_mm","BodyHeight_mm","Head_length_mm",
# 	"Head_width_mm","Eye_area_mm2","Antenna_length_mm","Mandible_length_mm","Proximal_mandible_width_mm",
# 	"Front_leg_length_mm","Mid_leg_length_mm","Hind_leg_length_mm","Wing_morphology","Elytra_length_mm",
# 	"Elytra_width_mm","Wing_length_mm","Wing_width_mm","Wing_area_mm2","Hairiness_total","Colour_lightness",
# 	"Biotope_range")]
# coleo.vert[coleo.vert==""] = NA

# # Convert to numeric
# non.num = c("species","Genus","Family","Order","Activity","Larva_Diet","Imago_Diet","Larva_Diet_range","Imago_Diet_range",
# 	"Wing_morphology","Biotope_range","IUCN_status")
# coleo.vert[,!names(coleo.vert)%in%non.num] = sapply(coleo.vert[,!names(coleo.vert)%in%non.num],as.numeric)

# # Add gbif information
# coleo.vert[,GB.col] = NA
# for (i in 1:nrow(coleo.vert)){
#     cat("...sp.",i)
#     gbif.search = get_status(coleo.vert[i,"species"],order="Coleoptera") # Search for accepted names
#     if (!is.null(gbif.search)){
#     	coleo.vert$gbif_accepted[i] = gbif.search$scientificName[1] # Fill in the result
#      	coleo.vert$gbif_rank[i] = gbif.search$rank[1]
#         coleo.vert$gbif_phylum[i] = gbif.search$Phylum[1]
#         coleo.vert$gbif_genus[i] = gbif.search$Genus[1]
#         coleo.vert$gbif_order[i] = gbif.search$Order[1]
#         coleo.vert$gbif_family[i] = gbif.search$Family[1]
#         coleo.vert$gbif_class[i] = gbif.search$Class[1]
#          coleo.vert$gbif_IUCN[i] = gbif.search$IUCN_status[1]
#     }
# }
# write.table(coleo.vert,"temp_coleo.txt",row.names=FALSE)
coleo.vert = read.table("data/temp/temp_coleo.txt",header=TRUE)

# Format our table
which.gbif = which(names(coleo.vert)%in%GB.col)
coleo.vert = coleo.vert[,c(1,which.gbif,2:(ncol(coleo.vert)-length(which.gbif)))]
coleo.vert[is.na(coleo.vert$gbif_genus),"gbif_genus"] = coleo.vert[is.na(coleo.vert$gbif_genus),"Genus"]
coleo.vert$gbif_genus[is.na(coleo.vert$gbif_genus)] =
	sapply(strsplit(coleo.vert$species[is.na(coleo.vert$gbif_genus)]," "),function(x) x[1])
coleo.vert[is.na(coleo.vert$gbif_family),"gbif_family"] = coleo.vert[is.na(coleo.vert$gbif_family),"Family"]
coleo.vert$gbif_order = "Coleoptera"
coleo.vert$gbif_class = "Insecta"
coleo.vert$gbif_phylum = "Arthropoda"
coleo.vert[is.na(coleo.vert$IUCN_status),"IUCN_status"] = coleo.vert[is.na(coleo.vert$IUCN_status),"gbif_IUCN"]
coleo.vert[,c("Genus","Family","Order","gbif_IUCN")] = NULL
names(coleo.vert)[2:length(GB.col)] = new.col
gbif.cond = coleo.vert$IUCN_status%in%c("NOT_FOUND","NOT_EVALUATED")
coleo.vert[gbif.cond,"IUCN_status"] = NA

# Extract EU checklist and merge
s2z.coleo = s2z.sp[s2z.sp$group%in%"Beetles",]
coleo.merge1 = merge(s2z.coleo,coleo.vert,by="species")
coleo.vert = coleo.vert[!coleo.vert$species%in%coleo.merge1$species,]
s2z.coleo2 = s2z.coleo[!s2z.coleo$species%in%coleo.merge1$species,]

# Regroup all the available information for the accepted species (mean when continuous values)
mam.ndup = coleo.vert[!(duplicated(coleo.vert$gbif_accepted)|duplicated(coleo.vert$gbif_accepted,fromLast=TRUE)),]
mam.dup = coleo.vert[duplicated(coleo.vert$gbif_accepted)|duplicated(coleo.vert$gbif_accepted,fromLast=TRUE),]
mam.homo = combine_rows(df=mam.dup,ref.col="gbif_accepted",binary=FALSE)
mam.homo = mam.homo[!is.na(mam.homo$gbif_accepted),]

# Combine, attempt another merge and rbind both results
mam.temp = rbind(mam.ndup,mam.homo)
coleo.merge2 = merge(s2z.coleo2,mam.temp,by="gbif_accepted",all.x=TRUE)
names(coleo.merge2)[1:2] = c("gbif_accepted.x","species")
coleo.merge2$gbif_accepted.y = coleo.merge2$gbif_accepted.x
coleo.merge2 = coleo.merge2[,names(coleo.merge1)]
coleo.MM = rbind(coleo.merge1,coleo.merge2)

# Try to fill in mising information fron one column to another
coleo.MM[is.na(coleo.MM$genus),"genus"] = coleo.MM[is.na(coleo.MM$genus),"Genus"]
coleo.MM[is.na(coleo.MM$family),"family"] = coleo.MM[is.na(coleo.MM$family),"Family"]
coleo.MM[is.na(coleo.MM$order),"order"] = coleo.MM[is.na(coleo.MM$order),"Order"]
coleo.MM[is.na(coleo.MM$class),"class"] = coleo.MM[is.na(coleo.MM$class),"Class"]
coleo.MM[is.na(coleo.MM$phylum),"phylum"] = coleo.MM[is.na(coleo.MM$phylum),"Phylum"]
coleo.MM[grepl("unresolved_accepted",coleo.MM$gbif_accepted.x),"gbif_accepted.x"] =
	coleo.MM[grepl("unresolved_accepted",coleo.MM$gbif_accepted.x),"gbif_accepted.y"]
coleo.MM$gbif_accepted.x[is.na(coleo.MM$gbif_accepted.x)] =
	paste0("unresolved_accepted2_", seq_len(sum(is.na(coleo.MM$gbif_accepted.x))))
coleo.MM[is.na(coleo.MM$taxonRank),"taxonRank"] = coleo.MM[is.na(coleo.MM$taxonRank),"gbif_rank"]
coleo.MM[is.na(coleo.MM$iucnRedListCategory),"iucnRedListCategory"] =
	coleo.MM[is.na(coleo.MM$iucnRedListCategory),"IUCN_status"]
toRemove = grepl("\\.y",names(coleo.MM)) |
	names(coleo.MM)%in%c("Genus","Family","Order","Class","Phylum","gbif_rank","IUCN_status")
coleo.MM[,toRemove] = NULL
names(coleo.MM)[1:ncol(s2z.coleo2)] = names(s2z.coleo2)

# Fine tune Ter/Aqu/Arb info (a)
guild.ter = apply(coleo.MM[,sprintf("GUILDE.%d",c(3,10,12,14:15,20:22,24:25))],1,sum)
coleo.MM[guild.ter>0&!is.na(guild.ter),"Ter"] = 1
guild.aqu = apply(coleo.MM[,sprintf("GUILDE.%d",c(1,2,4:9,26))],1,sum)
coleo.MM[guild.aqu>0&!is.na(guild.aqu),"Aqu"] = 1
guild.arb = apply(coleo.MM[,sprintf("GUILDE.%d",c(11,13,16:19,23))],1,sum)
coleo.MM[guild.arb>0&!is.na(guild.arb),"Arb"] = 1

# Fine tune habitat simple
hab.wat = apply(coleo.MM[,sprintf("GUILDE.%d",c(1,2,4,5,8))],1,sum)
coleo.MM[hab.wat>0 & !is.na(hab.wat),"Habitat_water"] = 1
coleo.MM[hab.wat%in%0 & !coleo.MM$Habitat_water%in%1,"Habitat_water"] = 0
hab.wet = apply(coleo.MM[,sprintf("GUILDE.%d",c(6,7,9,26))],1,sum)
coleo.MM[hab.wet>0 & !is.na(hab.wet),"Habitat_wetlands"] = 1
coleo.MM[hab.wet%in%0 & !coleo.MM$Habitat_wetlands%in%1,"Habitat_wetlands"] = 0
coleo.MM[coleo.MM[,"GUILDE.21"]>0 & !is.na(coleo.MM[,"GUILDE.21"]),"Habitat_rocks"] = 1
coleo.MM[coleo.MM[,"GUILDE.21"]%in%0 & !coleo.MM$Habitat_rocks%in%1,"Habitat_rocks"] = 0
hab.grass = apply(coleo.MM[,sprintf("GUILDE.%d",c(14,20))],1,sum)
coleo.MM[hab.grass>0 & !is.na(hab.grass),"Habitat_grassland"] = 1
coleo.MM[hab.grass%in%0 & !coleo.MM$Habitat_grassland%in%1,"Habitat_grassland"] = 0
hab.forest = apply(coleo.MM[,sprintf("GUILDE.%d",c(16,17,19))],1,sum)
coleo.MM[hab.forest>0 & !is.na(hab.forest),"Habitat_forests"] = 1
coleo.MM[hab.forest%in%0 & !coleo.MM$Habitat_forests%in%1,"Habitat_forests"] = 0
coleo.MM[coleo.MM[,"GUILDE.17"]>0 & !is.na(coleo.MM[,"GUILDE.17"]),"Habitat_deciduous"] = 1
coleo.MM[coleo.MM[,"GUILDE.17"]%in%0 & !coleo.MM$Habitat_deciduous%in%1,"Habitat_deciduous"] = 0
coleo.MM[coleo.MM[,"GUILDE.19"]>0 & !is.na(coleo.MM[,"GUILDE.19"]),"Habitat_coniferous"] = 1
coleo.MM[coleo.MM[,"GUILDE.19"]%in%0 & !coleo.MM$Habitat_coniferous%in%1,"Habitat_coniferous"] = 0
coleo.MM[coleo.MM[,"GUILDE.16"]>0 & !is.na(coleo.MM[,"GUILDE.16"]),"Habitat_mixed"] = 1
coleo.MM[coleo.MM[,"GUILDE.16"]%in%0 & !coleo.MM$Habitat_mixed%in%1,"Habitat_mixed"] = 0
hab.pioneer = apply(coleo.MM[,sprintf("GUILDE.%d",c(3,10,12,22))],1,sum)
coleo.MM[hab.pioneer>0 & !is.na(hab.pioneer),"Habitat_pioneer"] = 1
coleo.MM[hab.pioneer%in%0 & !coleo.MM$Habitat_pioneer%in%1,"Habitat_pioneer"] = 0
hab.urban = apply(coleo.MM[,sprintf("GUILDE.%d",c(22,24))],1,sum)
coleo.MM[hab.urban>0 & !is.na(hab.urban),"Habitat_urban"] = 1
coleo.MM[hab.urban%in%0 & !coleo.MM$Habitat_urban%in%1,"Habitat_urban"] = 0
coleo.MM[coleo.MM[,"GUILDE.23"]>0 & !is.na(coleo.MM[,"GUILDE.23"]),"Habitat_green_urban"] = 1
coleo.MM[coleo.MM[,"GUILDE.23"]%in%0 & !coleo.MM$Habitat_green_urban%in%1,"Habitat_green_urban"] = 0
hab.agri = apply(coleo.MM[,sprintf("GUILDE.%d",c(11,25))],1,sum)
coleo.MM[hab.agri>0 & !is.na(hab.agri),"Habitat_agricultural"] = 1
coleo.MM[hab.agri%in%0 & !coleo.MM$Habitat_agricultural%in%1,"Habitat_agricultural"] = 0
coleo.MM[coleo.MM[,"GUILDE.13"]>0 & !is.na(coleo.MM[,"GUILDE.13"]),"Habitat_bushes"] = 1
coleo.MM[coleo.MM[,"GUILDE.13"]%in%0 & !coleo.MM$Habitat_bushes%in%1,"Habitat_bushes"] = 0
coleo.MM[coleo.MM[,"GUILDE.15"]>0 & !is.na(coleo.MM[,"GUILDE.15"]),"Habitat_transitional"] = 1
coleo.MM[coleo.MM[,"GUILDE.15"]%in%0 & !coleo.MM$Habitat_transitional%in%1,"Habitat_transitional"] = 0
coleo.MM[coleo.MM[,"GUILDE.18"]>0 & !is.na(coleo.MM[,"GUILDE.18"]),"Habitat_heathland"] = 1
coleo.MM[coleo.MM[,"GUILDE.18"]%in%0 & !coleo.MM$Habitat_heathland%in%1,"Habitat_heathland"] = 0
coleo.MM[,HABit[!HABit%in%names(coleo.MM)]] = NA
coleo.MM[,names(coleo.MM)[grepl("GUILD",names(coleo.MM))]] = NULL

# Fine tune Ter/Aqu/Arb info (b)
coleo.MM[coleo.MM$Habitat_urban%in%1,"Ter"] = 1
coleo.MM[coleo.MM$Habitat_green_urban%in%1,"Ter"] = 1
coleo.MM[coleo.MM$Habitat_agriculture%in%1,"Ter"] = 1
coleo.MM[coleo.MM$Habitat_deciduous%in%1,"Arb"] = 1
coleo.MM[coleo.MM$Habitat_coniferous%in%1,"Arb"] = 1
coleo.MM[coleo.MM$Habitat_mixed%in%1,"Arb"] = 1
coleo.MM[coleo.MM$Habitat_grassland%in%1,"Ter"] = 1
coleo.MM[coleo.MM$Habitat_heathland%in%1,"Arb"] = 1
coleo.MM[coleo.MM$Habitat_bushes%in%1,"Arb"] = 1
coleo.MM[coleo.MM$Habitat_sand%in%1,"Ter"] = 1
coleo.MM[coleo.MM$Habitat_wetlands%in%1,"Aqu"] = 1
coleo.MM[coleo.MM$Habitat_water%in%1,"Aqu"] = 1
coleo.MM[coleo.MM$Habitat_rocks%in%1,"Ter"] = 1
coleo.MM[coleo.MM$Habitat_pioneer%in%1,"Ter"] = 1
l.temp = coleo.MM[,c("Ter","Aqu","Arb")]
l.temp [is.na(l.temp)] = 0
l.temp[apply(l.temp,1,function(x) all(x%in%0)),] = NA
coleo.MM[,c("Ter","Aqu","Arb")] = l.temp

# Aquatic, Terrestrial or Marine
coleo.MM[,"EcoMar"]= NA
coleo.MM$EcoTer = 1
coleo.MM$EcoFresh = 0
coleo.MM$EcoMar = 0
coleo.MM[as.numeric(coleo.MM$Riparian_sp)>2&!is.na(coleo.MM$Riparian_sp),"Aqu"] = 1
coleo.MM$Aer = NA # No info
full.aqu = c("Dytiscidae","Elmidae","Gyrinidae","Haliplidae","Noteridae","Amphizoidae","Hygrobiidae","Meruidae","Hydroscaphidae") # Only the first we have
semi.aqu = c("Hydrophilidae","Hydraenidae","Dryopidae","Scirtidae","Psephenidae")
coleo.MM[coleo.MM$family%in%full.aqu,c("Aqu","EcoFresh")] = 1
coleo.MM[coleo.MM$family%in%full.aqu,"EcoTer"] = 0
coleo.MM[coleo.MM$family%in%semi.aqu,c("Aqu","EcoFresh")] = 1

# Fine tune Ter/Aqu info (c)
coleo.MM = coleo.MM[order(coleo.MM$species),]
mf1 = merge(coleo.MM,antoine.TerAqu,by="species",all.x=TRUE)
all(coleo.MM$species == mf1$species) # auto check
coleo.MM[mf1$scheme%in%"terrestrial"&is.na(coleo.MM$Ter),"Ter"] = 1
coleo.MM[mf1$scheme%in%"aquatic",c("Aqu","EcoFresh")] = 1
coleo.MM[coleo.MM$EcoFresh%in%1,"Aqu"] = 1

# Reorder & Save
hab.n = grep("Habitat_",names(coleo.MM))
eco.n = grep("Eco",names(coleo.MM))
col.ref = c("species","scientificName","taxonRank","gbif_status","gbif_accepted","genus","family","order",
	"class","phylum","group","Source","originCH","originEUR","swissRedListCategory","europeanRegionalRedListCategory",
	"iucnRedListCategory","EcoTer","EcoFresh","EcoMar","Fos","Ter","Aqu","Arb","Aer",
	names(coleo.MM[,hab.n]),"RangeSize_km2")
coleo.MM = coleo.MM[,c(col.ref,names(coleo.MM)[!names(coleo.MM)%in%col.ref])]
write.table(coleo.MM,"outputs/raw_traits/non_formated/s2z_raw_traits_coleoptera.txt",row.names=FALSE)


#####################
###### ODONATA ######
#####################


# # Specifically select coleopter in the s2z and open IUCN ranges
# s2z.odonata = s2z.insect[s2z.insect$order%in%"Odonata",]

# # Open flora indicativa (R version) and add our final modifs
# fi.odonata = as.data.frame(read_excel("./insects/fauna_indicativa/fauna_indicativa_Odonata_for_R.xlsx",1,
# 	col_types=c(rep("text",6),rep("numeric",4),"text",rep("numeric",21))))
# fi.odonata$species = str_squish(paste(fi.odonata$Genus,fi.odonata$Species,fi.odonata$Subspecies))
# fi.odonata$species = gsub(" NA","",fi.odonata$species)
# fi.odonata[,c("Species","Subspecies","Author")] = NULL
# col.tar = c("dystroph","oligotroph","mesotroph","eutroph","polytroph","hypertroph")
# EIV_WT = fi.odonata[,col.tar]
# EIV_WT.yes = apply(EIV_WT,1,function(x) length(which(x%in%1)))
# EIV_WT.max = apply(EIV_WT,1,function(x) max(which(x%in%1)))^2
# EIV_WT.max[EIV_WT.max%in%"Inf"] = NA
# fi.odonata$EIV_Water_trophy = normalize(EIV_WT.max-EIV_WT.yes)
# fi.odonata[,col.tar] = NULL
# fi.odonata$EIV_Water_trophy = normalize(fi.odonata$EIV_Water_trophy)
# fi.odonata$EIV_Temp = normalize(fi.odonata$EIV_Temp)

# # Open the odonata database, keep interesting columns and create new binary ones
# dat.odonata = read.csv("./insects/Odonata_pheno/opdb.csv")
# dat.odonata = dat.odonata[,c("GenusSpecies","Genus","Family","body_colors","body_colortypes","body_patterns",
# 	"flight_mode","territoriality","aquatic_habitats","sex_dimorphisms","body_lengths","forewing_lengths",
# 	"hindwing_lengths","has_wing_pigment","wing_pigment_extent_continuous","wing_pigment_pattern",
# 	"wing_pigment_symmetry","wing_pigment_dimorphism","wing_pigment_color")]
# dat.odonata = combine_rows(df=dat.odonata,ref.col="GenusSpecies",binary=TRUE)
# dat.odonata$wing_pigment_symmetry = gsub(" more| pigment","",dat.odonata$wing_pigment_symmetry)
# dat.odonata$wing_pigment_dimorphism = gsub(" dimorphic","",dat.odonata$wing_pigment_dimorphism)

# # Merge FI with odonata data
# merge.odonata = merge(fi.odonata,dat.odonata,by.x="species",by.y="GenusSpecies",all=TRUE)
# merge.odonata[is.na(merge.odonata$BodyLength_mm),"BodyLength_mm"] = merge.odonata[is.na(merge.odonata$BodyLength_mm),"body_lengths"]
# merge.odonata[is.na(merge.odonata$Genus.x),"Genus.x"] = merge.odonata[is.na(merge.odonata$Genus.x),"Genus.y"]
# merge.odonata[is.na(merge.odonata$Genus.x),"Family.x"] = merge.odonata[is.na(merge.odonata$Genus.x),"Family.y"]
# merge.odonata[,c("body_lengths","Genus.y","Family.y")] = NULL

# # Merge with Logghe & al. 2025
# logghe.dat = logghe.new[grepl("Odonata",names(logghe.new))][[1]]
# logghe.m = merge(merge.odonata,logghe.dat,by.x="species",by.y="Species",all=TRUE)
# logghe.m[is.na(logghe.m$Family.x),"Family.x"] = logghe.m[is.na(logghe.m$Family.x),"Family"]
# logghe.m$Family = NULL
# logghe.m$Order = "Odonata"
# #logghe.m[grepl("larva",logghe.m$Overwinter_stage),"Overwintering_larva"] = 1
# #logghe.m[grepl("imago",logghe.m$Overwinter_stage),"Overwintering_imago"] = 1
# logghe.m[is.na(logghe.m$BodyLength_mm.x),"BodyLength_mm.x"] = logghe.m[is.na(logghe.m$BodyLength_mm.x),"BodyLength_mm.y"]
# logghe.m[is.na(logghe.m$BodyLength_max.x),"BodyLength_max.x"] = logghe.m[is.na(logghe.m$BodyLength_max.x),"BodyLength_max.y"]
# logghe.m[is.na(logghe.m$BodyLength_min.x),"BodyLength_min.x"] = logghe.m[is.na(logghe.m$BodyLength_min.x),"BodyLength_min.y"]
# #logghe.m[is.na(logghe.m$Dispersal_ability.x),"Dispersal_ability.x"] = logghe.m[is.na(logghe.m$Dispersal_ability.x),"Dispersal_ability.y"]
# #logghe.m[logghe.m$Wing_morphology.x%in%c("aptery","brachyptery"),"Wing_morphology.x"] = "aptery/brachyptery"
# #logghe.m[is.na(logghe.m$Wing_morphology.x),"Wing_morphology.x"] = logghe.m[is.na(logghe.m$Wing_morphology.x),"Wing_morphology.y"]
# #logghe.m[is.na(logghe.m$Habitat_urban.x),"Habitat_urban.x"] = logghe.m[is.na(logghe.m$Habitat_urban.x),"Habitat_urban.y"]
# #logghe.m[is.na(logghe.m$Habitat_green_urban.x),"Habitat_green_urban.x"] = logghe.m[is.na(logghe.m$Habitat_green_urban.x),"Habitat_green_urban.y"]
# #logghe.m[is.na(logghe.m$Habitat_agricultural.x),"Habitat_agricultural.x"] = logghe.m[is.na(logghe.m$Habitat_agricultural.x),"Habitat_agricultural.y"]
# #logghe.m[is.na(logghe.m$Habitat_pioneer.x),"Habitat_pioneer.x"] = logghe.m[is.na(logghe.m$Habitat_pioneer.x),"Habitat_pioneer.y"]
# #logghe.m[is.na(logghe.m$Habitat_forests.x),"Habitat_forests.x"] = logghe.m[is.na(logghe.m$Habitat_forests.x),"Habitat_forests.y"]
# #logghe.m[is.na(logghe.m$Habitat_deciduous.x),"Habitat_deciduous.x"] = logghe.m[is.na(logghe.m$Habitat_deciduous.x),"Habitat_deciduous.y"]
# #logghe.m[is.na(logghe.m$Habitat_coniferous.x),"Habitat_coniferous.x"] = logghe.m[is.na(logghe.m$Habitat_coniferous.x),"Habitat_coniferous.y"]
# #logghe.m[is.na(logghe.m$Habitat_mixed.x),"Habitat_mixed.x"] = logghe.m[is.na(logghe.m$Habitat_mixed.x),"Habitat_mixed.y"]
# #logghe.m[is.na(logghe.m$Habitat_grassland.x),"Habitat_grassland.x"] = logghe.m[is.na(logghe.m$Habitat_grassland.x),"Habitat_grassland.y"]
# #logghe.m[is.na(logghe.m$Habitat_bushes.x),"Habitat_bushes.x"] = logghe.m[is.na(logghe.m$Habitat_bushes.x),"Habitat_bushes.y"]
# #logghe.m[is.na(logghe.m$Habitat_transitional.x),"Habitat_transitional.x"] = logghe.m[is.na(logghe.m$Habitat_transitional.x),"Habitat_transitional.y"]
# #logghe.m[is.na(logghe.m$Habitat_wetlands.x),"Habitat_wetlands.x"] = logghe.m[is.na(logghe.m$Habitat_wetlands.x),"Habitat_wetlands.y"]
# #logghe.m[is.na(logghe.m$Habitat_water.x),"Habitat_water.x"] = logghe.m[is.na(logghe.m$Habitat_water.x),"Habitat_water.y"]
# #logghe.m[is.na(logghe.m$Habitat_rocks.x),"Habitat_rocks.x"] = logghe.m[is.na(logghe.m$Habitat_rocks.x),"Habitat_rocks.y"]
# logghe.m[,"Habitat_wetlands"] = logghe.m[,"aquatic_habitats.wetland"]
# wat.c = c("aquatic_habitats.ephemeral","aquatic_habitats.lake","aquatic_habitats.pond","aquatic_habitats.river","aquatic_habitats.stream")
# wat.ind = apply(logghe.m[,wat.c],1,sum)
# logghe.m[!is.na(wat.ind)&wat.ind>0,"Habitat_water"] = 1
# logghe.m[!is.na(wat.ind)&wat.ind==0,"Habitat_water"] = 0
# logghe.m = logghe.m[!grepl("\\.y$",names(logghe.m))]
# names(logghe.m) = gsub("\\.x","",names(logghe.m))
# logghe.m$Diet_Carniv = 1
# logghe.m[,c("Diet_Scav","Diet_Fungi","Diet_Root","Diet_Seed","Diet_Leaf",
# 	"Diet_Flower","Diet_Stem","Diet_Grass","Diet_Woody")] = 0
# logghe.m$Imago_Diet = "Carnivorous"
# logghe.m$Larva_Diet = "Carnivorous"

# # Merge with IUCN ranges
# odonata.range = iucn.ranges[grepl("ODONATA",names(iucn.ranges))][[1]]
# odonata.iucn = merge(logghe.m,odonata.range[,c("species","range.size.km2")],by="species",all=TRUE)

# # Change names if necessary
# tGS = to_GenusSpecies(odonata.iucn$species)
# odonata.iucn$species = tGS[,"CanonicalName"]
# odonata.iucn = odonata.iucn[!is.na(odonata.iucn$species),]
# odonata.iucn$IUCN_status = NA

# # Harmonize names & reorder columns 
# names(odonata.iucn)[c(2:3,6:8,25:57,59:67,127)] = c("Genus","Family","Larva_Habitat_stillwaters","Larva_Habitat_watercourses",
# 	"Larva_Habitat_moors","Territoriality","Sex_dimporh","Wing_Pigment","WP_pattern","WP_symmetry","WP_dimorph","Forewing_length_mm","Hindwing_length_mm",
# 	"WPColor_cover","BodyCol_black","BodyCol_blue","BodyCol_brown","BodyCol_gray","BodyCol_green","BodyCol_orange","BodyCol_pink",
# 	"BodyCol_red","BodyCol_violet","BodyCol_white","BodyCol_yellow","BodyColType_pigment","BodyColType_pruinesc","BodyColType_structur",
# 	"BodyPatterns_plain","BodyPatterns_spotted","BodyPatterns_striped","Flight_mode_flier","Flight_mode_percher","Habitat_ephemeral",
# 	"Habitat_lakes","Habitat_ponds","Habitat_rivers","Habitat_streams","WPCol_amber","WPCol_black","WPCol_blue","WPCol_brown",
# 	"WPCol_iredesc","WPCol_orange","WPCol_prunesc","WPCol_red","WPCol_yellow","RangeSize_km2")
# names(odonata.iucn) = gsub("LARVA","Larva",names(odonata.iucn))
# odonata.iucn$Fos = NA
# odonata.iucn$Ter = 1
# odonata.iucn$Aqu = 1
# odonata.iucn$Arb = NA
# odonata.iucn$Aer = 1
# odonata.vert = odonata.iucn[,c(logghe.common,"Riparian_sp","Habitat_ephemeral","Habitat_lakes","Habitat_ponds","Habitat_rivers",
# 	"Habitat_streams","Forewing_length_mm","Hindwing_length_mm","Sex_dimporh","Territoriality","Flight_mode_flier","Flight_mode_percher",
# 	"BodyCol_black","BodyCol_blue","BodyCol_brown","BodyCol_gray","BodyCol_green","BodyCol_orange","BodyCol_pink","BodyCol_red","BodyCol_violet",
# 	"BodyCol_white","BodyCol_yellow","BodyColType_pigment","BodyColType_pruinesc","BodyColType_structur","BodyPatterns_plain","BodyPatterns_spotted",
# 	"BodyPatterns_striped","Wing_Pigment","WP_pattern","WP_symmetry","WP_dimorph","WPColor_cover","WPCol_amber","WPCol_black","WPCol_blue","WPCol_brown",
# 	"WPCol_iredesc","WPCol_orange","WPCol_prunesc","WPCol_red","WPCol_yellow","Larva_BodyLength_mm","Larva_BodyLength_min","Larva_BodyLength_max",
# 	"Larva_Habitat_stillwaters","Larva_Habitat_watercourses","Larva_Habitat_moors","Larva_EIV_Drought","Larva_Biotope_range")]
# odonata.vert[odonata.vert==""] = NA

# # Convert to numeric
# non.num = c("species","Genus","Family","Order","Activity","Larva_Diet","Imago_Diet","Larva_Diet_range","Imago_Diet_range",
# 	"Sex_dimporh","Territoriality","Wing_Pigment","WP_pattern","WP_symmetry","WP_dimorph","Larva_Biotope_range","IUCN_status")
# odonata.vert[,!names(odonata.vert)%in%non.num] = sapply(odonata.vert[,!names(odonata.vert)%in%non.num],as.numeric)

# # Add gbif information
# odonata.vert[,GB.col] = NA
# for (i in 1:nrow(odonata.vert)){
#     cat("...sp.",i)
#     gbif.search = get_status(odonata.vert[i,"species"],order="Odonata") # Search for accepted names
#     if (!is.null(gbif.search)){
#     	odonata.vert$gbif_accepted[i] = gbif.search$scientificName[1]
#     	odonata.vert$gbif_rank[i] = gbif.search$rank[1]
#         odonata.vert$gbif_phylum[i] = gbif.search$Phylum[1]
#         odonata.vert$gbif_genus[i] = gbif.search$Genus[1]
#         odonata.vert$gbif_order[i] = gbif.search$Order[1]
#         odonata.vert$gbif_family[i] = gbif.search$Family[1]
#         odonata.vert$gbif_class[i] = gbif.search$Class[1]
#         odonata.vert$gbif_IUCN[i] = gbif.search$IUCN_status[1]
#     }
# }
# write.table(odonata.vert,"temp_odonata.txt",row.names=FALSE)
odonata.vert = read.table("data/temp/temp_odonata.txt",header=TRUE)

# Format our table
which.gbif = which(names(odonata.vert)%in%GB.col)
odonata.vert = odonata.vert[,c(1,which.gbif,2:(ncol(odonata.vert)-length(which.gbif)))]
odonata.vert[is.na(odonata.vert$gbif_genus),"gbif_genus"] = odonata.vert[is.na(odonata.vert$gbif_genus),"Genus"]
odonata.vert$gbif_genus[is.na(odonata.vert$gbif_genus)] =
	sapply(strsplit(odonata.vert$species[is.na(odonata.vert$gbif_genus)]," "),function(x) x[1])
odonata.vert[is.na(odonata.vert$gbif_family),"gbif_family"] = odonata.vert[is.na(odonata.vert$gbif_family),"Family"]
odonata.vert$gbif_order = "Odonata"
odonata.vert$gbif_class = "Insecta"
odonata.vert$gbif_phylum = "Arthropoda"
odonata.vert[is.na(odonata.vert$IUCN_status),"IUCN_status"] = odonata.vert[is.na(odonata.vert$IUCN_status),"gbif_IUCN"]
odonata.vert[,c("Genus","Family","Order","gbif_IUCN")] = NULL
names(odonata.vert)[2:length(GB.col)] = new.col
gbif.cond = odonata.vert$IUCN_status%in%c("NOT_FOUND","NOT_EVALUATED")
odonata.vert[gbif.cond,"IUCN_status"] = NA

# Extract EU checklist and merge
s2z.odonata = s2z.sp[s2z.sp$group%in%"Dragonflies",]
odonata.merge1 = merge(s2z.odonata,odonata.vert,by="species")
odonata.vert = odonata.vert[!odonata.vert$species%in%odonata.merge1$species,]
s2z.odonata2 = s2z.odonata[!s2z.odonata$species%in%odonata.merge1$species,]

# Regroup all the available information for the accepted species (mean when continuous values)
mam.ndup = odonata.vert[!(duplicated(odonata.vert$gbif_accepted)|duplicated(odonata.vert$gbif_accepted,fromLast=TRUE)),]
mam.dup = odonata.vert[duplicated(odonata.vert$gbif_accepted)|duplicated(odonata.vert$gbif_accepted,fromLast=TRUE),]
mam.homo = combine_rows(df=mam.dup,ref.col="gbif_accepted",binary=FALSE)
mam.homo = mam.homo[!is.na(mam.homo$gbif_accepted),]

# Combine, attempt another merge and rbind both results
mam.temp = rbind(mam.ndup,mam.homo)
odonata.merge2 = merge(s2z.odonata2,mam.temp,by="gbif_accepted",all.x=TRUE)
names(odonata.merge2)[1:2] = c("gbif_accepted.x","species")
odonata.merge2$gbif_accepted.y = odonata.merge2$gbif_accepted.x
odonata.merge2 = odonata.merge2[,names(odonata.merge1)]
odonata.MM = rbind(odonata.merge1,odonata.merge2)

# Try to fill in mising information fron one column to another
odonata.MM[is.na(odonata.MM$genus),"genus"] = odonata.MM[is.na(odonata.MM$genus),"Genus"]
odonata.MM[is.na(odonata.MM$family),"family"] = odonata.MM[is.na(odonata.MM$family),"Family"]
odonata.MM[is.na(odonata.MM$order),"order"] = odonata.MM[is.na(odonata.MM$order),"Order"]
odonata.MM[is.na(odonata.MM$class),"class"] = odonata.MM[is.na(odonata.MM$class),"Class"]
odonata.MM[is.na(odonata.MM$phylum),"phylum"] = odonata.MM[is.na(odonata.MM$phylum),"Phylum"]
odonata.MM[grepl("unresolved_accepted",odonata.MM$gbif_accepted.x),"gbif_accepted.x"] =
	odonata.MM[grepl("unresolved_accepted",odonata.MM$gbif_accepted.x),"gbif_accepted.y"]
odonata.MM$gbif_accepted.x[is.na(odonata.MM$gbif_accepted.x)] =
	paste0("unresolved_accepted2_", seq_len(sum(is.na(odonata.MM$gbif_accepted.x))))
odonata.MM[is.na(odonata.MM$taxonRank),"taxonRank"] = odonata.MM[is.na(odonata.MM$taxonRank),"gbif_rank"]
odonata.MM[is.na(odonata.MM$iucnRedListCategory),"iucnRedListCategory"] =
	odonata.MM[is.na(odonata.MM$iucnRedListCategory),"IUCN_status"]
toRemove = grepl("\\.y",names(odonata.MM)) |
	names(odonata.MM)%in%c("Genus","Family","Order","Class","Phylum","gbif_rank","IUCN_status")
odonata.MM[,toRemove] = NULL
names(odonata.MM)[1:ncol(s2z.odonata2)] = names(s2z.odonata2)

# Ter/Aqu/Aer
odonata.MM$Fos = 0
odonata.MM$Ter = 1 # Not arboreal or fosseal
odonata.MM$Aqu = 1
odonata.MM$Arb = 0
odonata.MM$Aer = 1

# Fine tune habitat simple
hab.wat = apply(odonata.MM[,sprintf("GUILDE.%d",c(1,2,4,5,8))],1,sum)
odonata.MM[hab.wat>0 & !is.na(hab.wat),"Habitat_water"] = 1
odonata.MM[hab.wat%in%0 & !odonata.MM$Habitat_water%in%1,"Habitat_water"] = 0
hab.wet = apply(odonata.MM[,sprintf("GUILDE.%d",c(6,7,9,26))],1,sum)
odonata.MM[hab.wet>0 & !is.na(hab.wet),"Habitat_wetlands"] = 1
odonata.MM[hab.wet%in%0 & !odonata.MM$Habitat_wetlands%in%1,"Habitat_wetlands"] = 0
odonata.MM[odonata.MM[,"GUILDE.21"]>0 & !is.na(odonata.MM[,"GUILDE.21"]),"Habitat_rocks"] = 1
odonata.MM[odonata.MM[,"GUILDE.21"]%in%0 & !odonata.MM$Habitat_rocks%in%1,"Habitat_rocks"] = 0
hab.grass = apply(odonata.MM[,sprintf("GUILDE.%d",c(14,20))],1,sum)
odonata.MM[hab.grass>0 & !is.na(hab.grass),"Habitat_grassland"] = 1
odonata.MM[hab.grass%in%0 & !odonata.MM$Habitat_grassland%in%1,"Habitat_grassland"] = 0
hab.forest = apply(odonata.MM[,sprintf("GUILDE.%d",c(16,17,19))],1,sum)
odonata.MM[hab.forest>0 & !is.na(hab.forest),"Habitat_forests"] = 1
odonata.MM[hab.forest%in%0 & !odonata.MM$Habitat_forests%in%1,"Habitat_forests"] = 0
odonata.MM[odonata.MM[,"GUILDE.17"]>0 & !is.na(odonata.MM[,"GUILDE.17"]),"Habitat_deciduous"] = 1
odonata.MM[odonata.MM[,"GUILDE.17"]%in%0 & !odonata.MM$Habitat_deciduous%in%1,"Habitat_deciduous"] = 0
odonata.MM[odonata.MM[,"GUILDE.19"]>0 & !is.na(odonata.MM[,"GUILDE.19"]),"Habitat_coniferous"] = 1
odonata.MM[odonata.MM[,"GUILDE.19"]%in%0 & !odonata.MM$Habitat_coniferous%in%1,"Habitat_coniferous"] = 0
odonata.MM[odonata.MM[,"GUILDE.16"]>0 & !is.na(odonata.MM[,"GUILDE.16"]),"Habitat_mixed"] = 1
odonata.MM[odonata.MM[,"GUILDE.16"]%in%0 & !odonata.MM$Habitat_mixed%in%1,"Habitat_mixed"] = 0
hab.pioneer = apply(odonata.MM[,sprintf("GUILDE.%d",c(3,10,12,22))],1,sum)
odonata.MM[hab.pioneer>0 & !is.na(hab.pioneer),"Habitat_pioneer"] = 1
odonata.MM[hab.pioneer%in%0 & !odonata.MM$Habitat_pioneer%in%1,"Habitat_pioneer"] = 0
hab.urban = apply(odonata.MM[,sprintf("GUILDE.%d",c(22,24))],1,sum)
odonata.MM[hab.urban>0 & !is.na(hab.urban),"Habitat_urban"] = 1
odonata.MM[hab.urban%in%0 & !odonata.MM$Habitat_urban%in%1,"Habitat_urban"] = 0
odonata.MM[odonata.MM[,"GUILDE.23"]>0 & !is.na(odonata.MM[,"GUILDE.23"]),"Habitat_green_urban"] = 1
odonata.MM[odonata.MM[,"GUILDE.23"]%in%0 & !odonata.MM$Habitat_green_urban%in%1,"Habitat_green_urban"] = 0
hab.agri = apply(odonata.MM[,sprintf("GUILDE.%d",c(11,25))],1,sum)
odonata.MM[hab.agri>0 & !is.na(hab.agri),"Habitat_agricultural"] = 1
odonata.MM[hab.agri%in%0 & !odonata.MM$Habitat_agricultural%in%1,"Habitat_agricultural"] = 0
odonata.MM[odonata.MM[,"GUILDE.13"]>0 & !is.na(odonata.MM[,"GUILDE.13"]),"Habitat_bushes"] = 1
odonata.MM[odonata.MM[,"GUILDE.13"]%in%0 & !odonata.MM$Habitat_bushes%in%1,"Habitat_bushes"] = 0
odonata.MM[odonata.MM[,"GUILDE.15"]>0 & !is.na(odonata.MM[,"GUILDE.15"]),"Habitat_transitional"] = 1
odonata.MM[odonata.MM[,"GUILDE.15"]%in%0 & !odonata.MM$Habitat_transitional%in%1,"Habitat_transitional"] = 0
odonata.MM[odonata.MM[,"GUILDE.18"]>0 & !is.na(odonata.MM[,"GUILDE.18"]),"Habitat_heathland"] = 1
odonata.MM[odonata.MM[,"GUILDE.18"]%in%0 & !odonata.MM$Habitat_heathland%in%1,"Habitat_heathland"] = 0
odonata.MM[,HABit[!HABit%in%names(odonata.MM)]] = NA
odonata.MM[,names(odonata.MM)[grepl("GUILD",names(odonata.MM))]] = NULL

# Fine tune Ter/Aqu/Arb info
odonata.MM[odonata.MM$Habitat_urban%in%1,"Ter"] = 1
odonata.MM[odonata.MM$Habitat_green_urban%in%1,"Ter"] = 1
odonata.MM[odonata.MM$Habitat_agriculture%in%1,"Ter"] = 1
odonata.MM[odonata.MM$Habitat_deciduous%in%1,"Arb"] = 1
odonata.MM[odonata.MM$Habitat_coniferous%in%1,"Arb"] = 1
odonata.MM[odonata.MM$Habitat_mixed%in%1,"Arb"] = 1
odonata.MM[odonata.MM$Habitat_grassland%in%1,"Ter"] = 1
odonata.MM[odonata.MM$Habitat_heathland%in%1,"Arb"] = 1
odonata.MM[odonata.MM$Habitat_bushes%in%1,"Arb"] = 1
odonata.MM[odonata.MM$Habitat_sand%in%1,"Ter"] = 1
odonata.MM[odonata.MM$Habitat_wetlands%in%1,"Aqu"] = 1
odonata.MM[odonata.MM$Habitat_water%in%1,"Aqu"] = 1
odonata.MM[odonata.MM$Habitat_rocks%in%1,"Ter"] = 1
odonata.MM[odonata.MM$Habitat_pioneer%in%1,"Ter"] = 1
l.temp = odonata.MM[,c("Ter","Aqu","Arb")]
l.temp [is.na(l.temp)] = 0
l.temp[apply(l.temp,1,function(x) all(x%in%0)),] = NA
odonata.MM[,c("Ter","Aqu","Arb")] = l.temp

# Aquatic, Terrestrial or Marine
odonata.MM[,c("EcoTer","EcoFresh","EcoMar")]= NA
odonata.MM$EcoTer = 1
odonata.MM$EcoFresh = 1
odonata.MM$EcoMar = 0

# Reorder & Save
hab.n = names(odonata.MM)[grep("^Habitat_",names(odonata.MM))]
hab.n = hab.n[!grepl("ephemeral|lakes|ponds|rivers|streams",hab.n)]
eco.n = grep("Eco",names(odonata.MM))
col.ref = c("species","scientificName","taxonRank","gbif_status","gbif_accepted","genus","family","order",
	"class","phylum","group","Source","originCH","originEUR","swissRedListCategory","europeanRegionalRedListCategory",
	"iucnRedListCategory","EcoTer","EcoFresh","EcoMar","Fos","Ter","Aqu","Arb","Aer",
	names(odonata.MM[,hab.n]),"RangeSize_km2")
odonata.MM = odonata.MM[,c(col.ref,names(odonata.MM)[!names(odonata.MM)%in%col.ref])]
write.table(odonata.MM,"outputs/raw_traits/non_formated/s2z_raw_traits_odonata.txt",row.names=FALSE)


########################
###### ORTHOPTERA ######
########################


# # Specifically select coleopter in the s2z and open IUCN ranges
# goss.ortho = goss.arthrop[goss.arthrop$Order%in%"Orthoptera",]

# # Open flora indicativa (R version) and add our final modifs
# fi.ortho = as.data.frame(read_excel("./insects/fauna_indicativa/fauna_indicativa_Orthoptera_v3_for_R.xlsx",1,
# 	col_types=c(rep("text",6),rep("numeric",38),"text",rep("numeric",61))))
# fi.ortho$species = str_squish(paste(fi.ortho$Genus,fi.ortho$Species,fi.ortho$Subspecies))
# fi.ortho$species = gsub(" NA","",fi.ortho$species)
# fi.ortho[,c("Species","Subspecies","Author")] = NULL
# HAB.ref = gsub("\\..*","",colnames(fi.ortho)[grepl("Habitat_",colnames(fi.ortho))])
# HAB.dup = unique(HAB.ref[duplicated(HAB.ref)])
# for (i in 1:length(HAB.dup)){fi.ortho[,HAB.dup[i]] = as.numeric(apply(fi.ortho[,grepl(HAB.dup[i],colnames(fi.ortho))],1,sum)>0)}
# HAB.ref = gsub("\\..*","",colnames(fi.ortho)[grepl("Ter|Arb",colnames(fi.ortho))])
# HAB.dup = unique(HAB.ref[duplicated(HAB.ref)])
# for (i in 1:length(HAB.dup)){fi.ortho[,HAB.dup[i]] = as.numeric(apply(fi.ortho[,grepl(HAB.dup[i],colnames(fi.ortho))],1,sum)>0)}
# HAB.ref = gsub("\\..*","",colnames(fi.ortho)[grepl("Diet_",colnames(fi.ortho))])
# HAB.dup = unique(HAB.ref[duplicated(HAB.ref)])
# for (i in 1:length(HAB.dup)){fi.ortho[,HAB.dup[i]] = as.numeric(apply(fi.ortho[,grepl(HAB.dup[i],colnames(fi.ortho))],1,sum)>0)}
# fi.ortho[grepl("\\.\\.\\.",names(fi.ortho))] = NULL
# fi.ortho$Flight_capacity = normalize((fi.ortho$flight.male + fi.ortho$flight.female)/2)
# fi.ortho$BodyLength_min = (fi.ortho$female.min + fi.ortho$male.min)/2
# fi.ortho$BodyLength_max = (fi.ortho$male.max + fi.ortho$female.max)/2
# fi.ortho$BodyLength_mm = (fi.ortho$BodyLength_max + fi.ortho$BodyLength_min)/2
# fi.ortho$BodyLength_male = (fi.ortho$male.min + fi.ortho$male.max)/2
# fi.ortho$BodyLength_female = (fi.ortho$female.min + fi.ortho$female.max)/2
# fi.ortho[,c("flight.male","flight.female","male.min","male.max","female.min","female.max")] = NULL
# ref.diet = fi.ortho$Diet%in%1
# fi.ortho[fi.ortho$Diet_Woody%in%1,"Diet"] = "Herbivorous"
# fi.ortho[fi.ortho$Diet_Grass%in%1,"Diet"] = "Herbivorous"
# fi.ortho[fi.ortho$Diet_Scav%in%1,"Diet"] = "Scavenger"
# fi.ortho[fi.ortho$Diet_Parasit%in%1,"Diet"] = "Parasitic"
# fi.ortho[ref.diet,"Diet"] = "Omnivorous"
# fi.ortho[fi.ortho$Diet%in%0,"Diet"] = NA
# col.tar = names(fi.ortho)[grepl("F[0-9]",names(fi.ortho))]
# EIV_M = fi.ortho[,col.tar]
# EIV_M.yes = apply(EIV_M,1,function(x) length(which(x>0)))
# EIV_M.max = apply(EIV_M,1,function(x) max(which(x>0)))^2
# EIV_M.max[EIV_M.max%in%"Inf"] = NA
# fi.ortho$EIV_Moisture = normalize(EIV_M.max-EIV_M.yes)
# fi.ortho[,col.tar] = NULL
# col.tar = names(fi.ortho)[grepl("T[0-9]",names(fi.ortho))]
# EIV_T = fi.ortho[,col.tar]
# EIV_T.yes = apply(EIV_T,1,function(x) length(which(x>0)))
# EIV_T.max = apply(EIV_T,1,function(x) max(which(x>0)))^2
# EIV_T.max[EIV_T.max%in%"Inf"] = NA
# fi.ortho$EIV_Temp = normalize(EIV_T.max-EIV_T.yes)
# fi.ortho[,col.tar] = NULL
# fi.ortho = fi.ortho[,!names(fi.ortho)%in%"removed"]
# fi.ortho[!is.na(fi.ortho$univoltin)&fi.ortho$univoltin>1,"univoltin"] = 1
# fi.ortho[!is.na(fi.ortho$bivoltin)&fi.ortho$bivoltin>1,"bivoltin"] = 1
# fi.ortho[!is.na(fi.ortho$multivoltin)&fi.ortho$multivoltin>1,"multivoltin"] = 1
# fi.ortho[!is.na(fi.ortho$semivoltin)&fi.ortho$semivoltin>1,"semivoltin"] = 1

# # Merge FI with gossner_coleopters
# merge.ortho = merge(fi.ortho,goss.ortho,by="species",all=TRUE)
# merge.ortho[is.na(merge.ortho$Family.x),"Family.x"] = merge.ortho[is.na(merge.ortho$Family.x),"Family.y"]
# merge.ortho[is.na(merge.ortho$Arb.x),"Arb.x"] = merge.ortho[is.na(merge.ortho$Arb.x),"Arb.y"]
# merge.ortho[is.na(merge.ortho$Ter.x),"Ter.x"] = merge.ortho[is.na(merge.ortho$Ter.x),"Ter.y"]
# merge.ortho[is.na(merge.ortho$Diet.x),"Diet.x"] = merge.ortho[is.na(merge.ortho$Diet.x),"Diet.y"]
# merge.ortho[is.na(merge.ortho$Diet_Scav.x),"Diet_Scav.x"] = merge.ortho[is.na(merge.ortho$Diet_Scav.x),"Diet_Scav.y"]
# merge.ortho[merge.ortho$Diet_Scav.y%in%1,"Diet_Scav.x"] = 1
# merge.ortho[is.na(merge.ortho$BodyLength_mm),"BodyLength_mm"] = merge.ortho[is.na(merge.ortho$BodyLength_mm),"Body_Size"]
# merge.ortho[,c("Family.y","Arb.y","Fos.y","Ter.y","Aqu.y","Diet.y","Diet_Scav.y","Aer.y","Body_Size")] = NULL
# names(merge.ortho) = gsub("\\.x","",names(merge.ortho))

# # Resolve dipersal
# n1 = normalize(merge.ortho$Dispersal_ability)
# n2 = normalize(merge.ortho$Dispersal_capacity)
# n3 = normalize(mapply(sum,n1,n2)/2)
# n3[is.na(n3)] = n2[is.na(n3)]
# merge.ortho$Dispersal_ability = n3

# # Merge with Logghe & al. 2025
# logghe.dat = logghe.new[grepl("Orthoptera",names(logghe.new))][[1]]
# logghe.m = merge(merge.ortho,logghe.dat,by.x="species",by.y="Species",all=TRUE)
# logghe.m[is.na(logghe.m$Family.x),"Family.x"] = logghe.m[is.na(logghe.m$Family.x),"Family.y"]
# logghe.m$Order.x = "Orthoptera"
# #logghe.m[grepl("larva",logghe.m$Overwinter_stage),"Overwintering_larva"] = 1
# #logghe.m[grepl("imago",logghe.m$Overwinter_stage),"Overwintering_imago"] = 1
# #logghe.m[is.na(logghe.m$Litter_size.x),"Litter_size.x"] = logghe.m[is.na(logghe.m$Litter_size.x),"Litter_size.y"]
# logghe.m[is.na(logghe.m$BodyLength_mm.x),"BodyLength_mm.x"] = logghe.m[is.na(logghe.m$BodyLength_mm.x),"BodyLength_mm.y"]
# logghe.m[is.na(logghe.m$BodyLength_max.x),"BodyLength_max.x"] = logghe.m[is.na(logghe.m$BodyLength_max.x),"BodyLength_max.y"]
# logghe.m[is.na(logghe.m$BodyLength_min.x),"BodyLength_min.x"] = logghe.m[is.na(logghe.m$BodyLength_min.x),"BodyLength_min.y"]
# logghe.m[is.na(logghe.m$BodyLength_male.x),"BodyLength_male.x"] = logghe.m[is.na(logghe.m$BodyLength_male.x),"BodyLength_male.y"]
# logghe.m[is.na(logghe.m$BodyLength_female.x),"BodyLength_female.x"] = logghe.m[is.na(logghe.m$BodyLength_female.x),"BodyLength_female.y"]
# logghe.m[is.na(logghe.m$Dispersal_ability.x),"Dispersal_ability.x"] = logghe.m[is.na(logghe.m$Dispersal_ability.x),"Dispersal_ability.y"]
# logghe.m[is.na(logghe.m$Univoltine)|logghe.m$Univoltine==0,"Univoltine"] = logghe.m[is.na(logghe.m$Univoltine)|logghe.m$Univoltine==0,"univoltin"]
# logghe.m[is.na(logghe.m$Semivoltine)|logghe.m$Semivoltine==0,"Semivoltine"]  = logghe.m[is.na(logghe.m$Semivoltine)|logghe.m$Semivoltine==0,"semivoltin"]
# logghe.m[is.na(logghe.m$Bivoltine)|logghe.m$Bivoltine==0,"Bivoltine"] = logghe.m[is.na(logghe.m$Bivoltine)|logghe.m$Bivoltine==0,"bivoltin"]
# logghe.m[is.na(logghe.m$Multivoltine)|logghe.m$Multivoltine==0,"Multivoltine"] = logghe.m[is.na(logghe.m$Multivoltine)|logghe.m$Multivoltine==0,"multivoltin"]
# logghe.m[is.na(logghe.m$Habitat_urban.x),"Habitat_urban.x"] = logghe.m[is.na(logghe.m$Habitat_urban.x),"Habitat_urban.y"]
# logghe.m[logghe.m$Habitat_urban.y%in%1,"Habitat_urban.x"] = 1
# #logghe.m[is.na(logghe.m$Habitat_green_urban.x),"Habitat_green_urban.x"] = logghe.m[is.na(logghe.m$Habitat_green_urban.x),"Habitat_green_urban.y"]
# #logghe.m[is.na(logghe.m$Habitat_agricultural.x),"Habitat_agricultural.x"] = logghe.m[is.na(logghe.m$Habitat_agricultural.x),"Habitat_agricultural.y"]
# logghe.m[is.na(logghe.m$Habitat_pioneer.x),"Habitat_pioneer.x"] = logghe.m[is.na(logghe.m$Habitat_pioneer.x),"Habitat_pioneer.y"]
# logghe.m[logghe.m$Habitat_pioneer.y%in%1,"Habitat_pioneer.x"] = 1
# #logghe.m[is.na(logghe.m$Habitat_forests.x),"Habitat_forests.x"] = logghe.m[is.na(logghe.m$Habitat_forests.x),"Habitat_forests.y"]
# #logghe.m[is.na(logghe.m$Habitat_deciduous.x),"Habitat_deciduous.x"] = logghe.m[is.na(logghe.m$Habitat_deciduous.x),"Habitat_deciduous.y"]
# #logghe.m[is.na(logghe.m$Habitat_coniferous.x),"Habitat_coniferous.x"] = logghe.m[is.na(logghe.m$Habitat_coniferous.x),"Habitat_coniferous.y"]
# #logghe.m[is.na(logghe.m$Habitat_mixed.x),"Habitat_mixed.x"] = logghe.m[is.na(logghe.m$Habitat_mixed.x),"Habitat_mixed.y"]
# logghe.m[is.na(logghe.m$Habitat_grassland.x),"Habitat_grassland.x"] = logghe.m[is.na(logghe.m$Habitat_grassland.x),"Habitat_grassland.y"]
# logghe.m[logghe.m$Habitat_grassland.y%in%1,"Habitat_grassland.x"] = 1
# logghe.m[is.na(logghe.m$Habitat_bushes.x),"Habitat_bushes.x"] = logghe.m[is.na(logghe.m$Habitat_bushes.x),"Habitat_bushes.y"]
# logghe.m[logghe.m$Habitat_bushes.y%in%1,"Habitat_bushes.x"] = 1
# logghe.m[is.na(logghe.m$Habitat_transitional.x),"Habitat_transitional.x"] = logghe.m[is.na(logghe.m$Habitat_transitional.x),"Habitat_transitional.y"]
# logghe.m[logghe.m$Habitat_transitional.y%in%1,"Habitat_transitional.x"] = 1
# logghe.m[is.na(logghe.m$Habitat_wetlands.x),"Habitat_wetlands.x"] = logghe.m[is.na(logghe.m$Habitat_wetlands.x),"Habitat_wetlands.y"]
# logghe.m[logghe.m$Habitat_wetlands.y%in%1,"Habitat_wetlands.x"] = 1
# logghe.m[is.na(logghe.m$Habitat_water.x),"Habitat_water.x"] = logghe.m[is.na(logghe.m$Habitat_water.x),"Habitat_water.y"]
# logghe.m[logghe.m$Habitat_water.y%in%1,"Habitat_water.x"] = 1
# logghe.m[is.na(logghe.m$Habitat_rocks.x),"Habitat_rocks.x"] = logghe.m[is.na(logghe.m$Habitat_rocks.x),"Habitat_rocks.y"]
# logghe.m[logghe.m$Habitat_rocks.y%in%1,"Habitat_rocks.x"] = 1
# logghe.m = logghe.m[!grepl("\\.y",names(logghe.m))]
# names(logghe.m) = gsub("\\.x","",names(logghe.m))
# logghe.m[is.na(logghe.m$Imago_Diet),"Imago_Diet"] = logghe.m[is.na(logghe.m$Imago_Diet),"Diet"]
# logghe.m[logghe.m$Imago_Diet%in%"Carnivorous","Diet_Carniv"] = 1
# logghe.m[logghe.m$Imago_Diet%in%"Scavenger","Diet_Scav"] = 1
# logghe.m[logghe.m$Imago_Diet%in%"Fungivorous","Diet_Fungi"] = 1
# logghe.m[logghe.m$Imago_Diet%in%"Herbivorous",c("Diet_leaf","Diet_Grass")] = 1
# logghe.m[logghe.m$Imago_Diet%in%"Palynivorous","Diet_Flower"] = 1

# # Merge with IUCN ranges
# ortho.range = iucn.ranges[grepl("ORTHOPTERA",names(iucn.ranges))][[1]]
# ortho.iucn = merge(logghe.m,ortho.range[,c("species","range.size.km2")],by="species",all=TRUE)
# ortho.iucn$RangeSize_km2 = ortho.iucn$range.size.km2

# # Change names if necessary
# tGS = to_GenusSpecies(ortho.iucn$species)
# ortho.iucn$species = tGS[,"CanonicalName"]
# ortho.iucn = ortho.iucn[!is.na(ortho.iucn$species),]
# ortho.iucn$IUCN_status = NA

# # Harmonize names & reorder columns 
# ortho.vert = ortho.iucn[,c(logghe.common,"Riparian_sp","Meadow_sp","Flight_capacity",
# 	names(ortho.iucn)[grepl("VegForm",names(ortho.iucn))],"Biotope_range")]
# ortho.vert[ortho.vert==""] = NA

# # Convert to numeric
# non.num = c("species","Genus","Family","Order","Activity","Larva_Diet","Imago_Diet",
# 	"Larva_Diet_range","Imago_Diet_range","Biotope_range","IUCN_status")
# ortho.vert[,!names(ortho.vert)%in%non.num] = sapply(ortho.vert[,!names(ortho.vert)%in%non.num],as.numeric)

# # Add gbif information
# ortho.vert[,GB.col] = NA
# for (i in 1:nrow(ortho.vert)){
#     cat("...sp.",i)
#     gbif.search = get_status(ortho.vert[i,"species"],order="Orthoptera") # Search for accepted names
#     if (!is.null(gbif.search)){
#     	ortho.vert$gbif_accepted[i] = gbif.search$scientificName[1] # Fill in the result
#     	ortho.vert$gbif_rank[i] = gbif.search$rank[1]
#         ortho.vert$gbif_phylum[i] = gbif.search$Phylum[1]
#         ortho.vert$gbif_genus[i] = gbif.search$Genus[1]
#         ortho.vert$gbif_order[i] = gbif.search$Order[1]
#         ortho.vert$gbif_family[i] = gbif.search$Family[1]
#         ortho.vert$gbif_class[i] = gbif.search$Class[1]
#         ortho.vert$gbif_IUCN[i] = gbif.search$IUCN_status[1]
#      }
# }
# write.table(ortho.vert,"temp_ortho.txt",row.names=FALSE)
ortho.vert = read.table("data/temp/temp_ortho.txt",header=TRUE)

# Format our table
which.gbif = which(names(ortho.vert)%in%GB.col)
ortho.vert = ortho.vert[,c(1,which.gbif,2:(ncol(ortho.vert)-length(which.gbif)))]
ortho.vert[is.na(ortho.vert$gbif_genus),"gbif_genus"] = ortho.vert[is.na(ortho.vert$gbif_genus),"Genus"]
ortho.vert$gbif_genus[is.na(ortho.vert$gbif_genus)] =
	unlist(sapply(strsplit(ortho.vert$species[is.na(ortho.vert$gbif_genus)]," "),function(x) x[1]))
ortho.vert[is.na(ortho.vert$gbif_family),"gbif_family"] = ortho.vert[is.na(ortho.vert$gbif_family),"Family"]
ortho.vert$gbif_order = "Orthoptera"
ortho.vert$gbif_class = "Insecta"
ortho.vert$gbif_phylum = "Arthropoda"
ortho.vert[is.na(ortho.vert$IUCN_status),"IUCN_status"] = ortho.vert[is.na(ortho.vert$IUCN_status),"gbif_IUCN"]
ortho.vert[,c("Genus","Family","Order","gbif_IUCN")] = NULL
names(ortho.vert)[2:length(GB.col)] = new.col
gbif.cond = ortho.vert$IUCN_status%in%c("NOT_FOUND","NOT_EVALUATED")
ortho.vert[gbif.cond,"IUCN_status"] = NA

# Extract EU checklist and merge
s2z.ortho = s2z.sp[s2z.sp$group%in%"Grasshoppers_Locusts_Crickets",]
ortho.merge1 = merge(s2z.ortho,ortho.vert,by="species")
ortho.vert = ortho.vert[!ortho.vert$species%in%ortho.merge1$species,]
s2z.ortho2 = s2z.ortho[!s2z.ortho$species%in%ortho.merge1$species,]

# Regroup all the available information for the accepted species (mean when continuous values)
mam.ndup = ortho.vert[!(duplicated(ortho.vert$gbif_accepted)|duplicated(ortho.vert$gbif_accepted,fromLast=TRUE)),]
mam.dup = ortho.vert[duplicated(ortho.vert$gbif_accepted)|duplicated(ortho.vert$gbif_accepted,fromLast=TRUE),]
mam.homo = combine_rows(df=mam.dup,ref.col="gbif_accepted",binary=FALSE)
mam.homo = mam.homo[!is.na(mam.homo$gbif_accepted),]

# Combine, attempt another merge and rbind both results
mam.temp = rbind(mam.ndup,mam.homo)
ortho.merge2 = merge(s2z.ortho2,mam.temp,by="gbif_accepted",all.x=TRUE)
names(ortho.merge2)[1:2] = c("gbif_accepted.x","species")
ortho.merge2$gbif_accepted.y = ortho.merge2$gbif_accepted.x
ortho.merge2 = ortho.merge2[,names(ortho.merge1)]
ortho.MM = rbind(ortho.merge1,ortho.merge2)

# Try to fill in mising information fron one column to another
ortho.MM[is.na(ortho.MM$genus),"genus"] = ortho.MM[is.na(ortho.MM$genus),"Genus"]
ortho.MM[is.na(ortho.MM$family),"family"] = ortho.MM[is.na(ortho.MM$family),"Family"]
ortho.MM[is.na(ortho.MM$order),"order"] = ortho.MM[is.na(ortho.MM$order),"Order"]
ortho.MM[is.na(ortho.MM$class),"class"] = ortho.MM[is.na(ortho.MM$class),"Class"]
ortho.MM[is.na(ortho.MM$phylum),"phylum"] = ortho.MM[is.na(ortho.MM$phylum),"Phylum"]
ortho.MM[grepl("unresolved_accepted",ortho.MM$gbif_accepted.x),"gbif_accepted.x"] =
	ortho.MM[grepl("unresolved_accepted",ortho.MM$gbif_accepted.x),"gbif_accepted.y"]
ortho.MM$gbif_accepted.x[is.na(ortho.MM$gbif_accepted.x)] =
	paste0("unresolved_accepted2_", seq_len(sum(is.na(ortho.MM$gbif_accepted.x))))
ortho.MM[is.na(ortho.MM$taxonRank),"taxonRank"] = ortho.MM[is.na(ortho.MM$taxonRank),"gbif_rank"]
ortho.MM[is.na(ortho.MM$iucnRedListCategory),"iucnRedListCategory"] =
	ortho.MM[is.na(ortho.MM$iucnRedListCategory),"IUCN_status"]
toRemove = grepl("\\.y",names(ortho.MM)) |
	names(ortho.MM)%in%c("Genus","Family","Order","Class","Phylum","gbif_rank","IUCN_status")
ortho.MM[,toRemove] = NULL
names(ortho.MM)[1:ncol(s2z.ortho2)] = names(s2z.ortho2)

# Fine tune Ter/Aqu/Arb info (a)
guild.ter = apply(ortho.MM[,sprintf("GUILDE.%d",c(3,10,12,14:15,20:22,24:25))],1,sum)
ortho.MM[guild.ter>0&!is.na(guild.ter),"Ter"] = 1
guild.aqu = apply(ortho.MM[,sprintf("GUILDE.%d",c(1,2,4:9,26))],1,sum)
ortho.MM[guild.aqu>0&!is.na(guild.aqu),"Aqu"] = 1
guild.arb = apply(ortho.MM[,sprintf("GUILDE.%d",c(11,13,16:19,23))],1,sum)
ortho.MM[guild.arb>0&!is.na(guild.arb),"Arb"] = 1

# Fine tune habitat simple
hab.wat = apply(ortho.MM[,sprintf("GUILDE.%d",c(1,2,4,5,8))],1,sum)
ortho.MM[hab.wat>0 & !is.na(hab.wat),"Habitat_water"] = 1
ortho.MM[hab.wat%in%0 & !ortho.MM$Habitat_water%in%1,"Habitat_water"] = 0
hab.wet = apply(ortho.MM[,sprintf("GUILDE.%d",c(6,7,9,26))],1,sum)
ortho.MM[hab.wet>0 & !is.na(hab.wet),"Habitat_wetlands"] = 1
ortho.MM[hab.wet%in%0 & !ortho.MM$Habitat_wetlands%in%1,"Habitat_wetlands"] = 0
ortho.MM[ortho.MM[,"GUILDE.21"]>0 & !is.na(ortho.MM[,"GUILDE.21"]),"Habitat_rocks"] = 1
ortho.MM[ortho.MM[,"GUILDE.21"]%in%0 & !ortho.MM$Habitat_rocks%in%1,"Habitat_rocks"] = 0
hab.grass = apply(ortho.MM[,sprintf("GUILDE.%d",c(14,20))],1,sum)
ortho.MM[hab.grass>0 & !is.na(hab.grass),"Habitat_grassland"] = 1
ortho.MM[hab.grass%in%0 & !ortho.MM$Habitat_grassland%in%1,"Habitat_grassland"] = 0
hab.forest = apply(ortho.MM[,sprintf("GUILDE.%d",c(16,17,19))],1,sum)
ortho.MM[hab.forest>0 & !is.na(hab.forest),"Habitat_forests"] = 1
ortho.MM[hab.forest%in%0 & !ortho.MM$Habitat_forests%in%1,"Habitat_forests"] = 0
ortho.MM[ortho.MM[,"GUILDE.17"]>0 & !is.na(ortho.MM[,"GUILDE.17"]),"Habitat_deciduous"] = 1
ortho.MM[ortho.MM[,"GUILDE.17"]%in%0 & !ortho.MM$Habitat_deciduous%in%1,"Habitat_deciduous"] = 0
ortho.MM[ortho.MM[,"GUILDE.19"]>0 & !is.na(ortho.MM[,"GUILDE.19"]),"Habitat_coniferous"] = 1
ortho.MM[ortho.MM[,"GUILDE.19"]%in%0 & !ortho.MM$Habitat_coniferous%in%1,"Habitat_coniferous"] = 0
ortho.MM[ortho.MM[,"GUILDE.16"]>0 & !is.na(ortho.MM[,"GUILDE.16"]),"Habitat_mixed"] = 1
ortho.MM[ortho.MM[,"GUILDE.16"]%in%0 & !ortho.MM$Habitat_mixed%in%1,"Habitat_mixed"] = 0
hab.pioneer = apply(ortho.MM[,sprintf("GUILDE.%d",c(3,10,12,22))],1,sum)
ortho.MM[hab.pioneer>0 & !is.na(hab.pioneer),"Habitat_pioneer"] = 1
ortho.MM[hab.pioneer%in%0 & !ortho.MM$Habitat_pioneer%in%1,"Habitat_pioneer"] = 0
hab.urban = apply(ortho.MM[,sprintf("GUILDE.%d",c(22,24))],1,sum)
ortho.MM[hab.urban>0 & !is.na(hab.urban),"Habitat_urban"] = 1
ortho.MM[hab.urban%in%0 & !ortho.MM$Habitat_urban%in%1,"Habitat_urban"] = 0
ortho.MM[ortho.MM[,"GUILDE.23"]>0 & !is.na(ortho.MM[,"GUILDE.23"]),"Habitat_green_urban"] = 1
ortho.MM[ortho.MM[,"GUILDE.23"]%in%0 & !ortho.MM$Habitat_green_urban%in%1,"Habitat_green_urban"] = 0
hab.agri = apply(ortho.MM[,sprintf("GUILDE.%d",c(11,25))],1,sum)
ortho.MM[hab.agri>0 & !is.na(hab.agri),"Habitat_agricultural"] = 1
ortho.MM[hab.agri%in%0 & !ortho.MM$Habitat_agricultural%in%1,"Habitat_agricultural"] = 0
ortho.MM[ortho.MM[,"GUILDE.13"]>0 & !is.na(ortho.MM[,"GUILDE.13"]),"Habitat_bushes"] = 1
ortho.MM[ortho.MM[,"GUILDE.13"]%in%0 & !ortho.MM$Habitat_bushes%in%1,"Habitat_bushes"] = 0
ortho.MM[ortho.MM[,"GUILDE.15"]>0 & !is.na(ortho.MM[,"GUILDE.15"]),"Habitat_transitional"] = 1
ortho.MM[ortho.MM[,"GUILDE.15"]%in%0 & !ortho.MM$Habitat_transitional%in%1,"Habitat_transitional"] = 0
ortho.MM[ortho.MM[,"GUILDE.18"]>0 & !is.na(ortho.MM[,"GUILDE.18"]),"Habitat_heathland"] = 1
ortho.MM[ortho.MM[,"GUILDE.18"]%in%0 & !ortho.MM$Habitat_heathland%in%1,"Habitat_heathland"] = 0
ortho.MM[,HABit[!HABit%in%names(ortho.MM)]] = NA
ortho.MM[,names(ortho.MM)[grepl("GUILD",names(ortho.MM))]] = NULL

# Fine tune Ter/Aqu/Arb info (b)
ortho.MM[ortho.MM$Habitat_urban%in%1,"Ter"] = 1
ortho.MM[ortho.MM$Habitat_green_urban%in%1,"Ter"] = 1
ortho.MM[ortho.MM$Habitat_agriculture%in%1,"Ter"] = 1
ortho.MM[ortho.MM$Habitat_deciduous%in%1,"Arb"] = 1
ortho.MM[ortho.MM$Habitat_coniferous%in%1,"Arb"] = 1
ortho.MM[ortho.MM$Habitat_mixed%in%1,"Arb"] = 1
ortho.MM[ortho.MM$Habitat_grassland%in%1,"Ter"] = 1
ortho.MM[ortho.MM$Habitat_heathland%in%1,"Arb"] = 1
ortho.MM[ortho.MM$Habitat_bushes%in%1,"Arb"] = 1
ortho.MM[ortho.MM$Habitat_sand%in%1,"Ter"] = 1
ortho.MM[ortho.MM$Habitat_wetlands%in%1,"Aqu"] = 1
ortho.MM[ortho.MM$Habitat_water%in%1,"Aqu"] = 1
ortho.MM[ortho.MM$Habitat_rocks%in%1,c("Fos","Ter")] = 1 # Rocks include cave for this one
ortho.MM[ortho.MM$Habitat_pioneer%in%1,"Ter"] = 1
l.temp = ortho.MM[,c("Ter","Aqu","Arb")]
l.temp [is.na(l.temp)] = 0
l.temp[apply(l.temp,1,function(x) all(x%in%0)),] = NA
ortho.MM[,c("Ter","Aqu","Arb")] = l.temp
ortho.MM[ortho.MM$Fos%in%1,"Habitat_rocks"] = 1

# Aquatic, Terrestrial or Marine
ortho.MM[,"EcoMar"]= NA
ortho.MM$EcoTer = 1
ortho.MM$EcoFresh = 0
ortho.MM$EcoMar = 0
ortho.MM[as.numeric(ortho.MM$Riparian_sp)>2&!is.na(ortho.MM$Riparian_sp),"Aqu"] = 1
ortho.MM$Aer = NA # No info
ortho.MM[ortho.MM$family%in%"Tridactylidae",c("Aqu","EcoFresh")] = 1

# Fine tune Ter/Aqu info (c)
ortho.MM = ortho.MM[order(ortho.MM$species),]
mf1 = merge(ortho.MM,antoine.TerAqu,by="species",all.x=TRUE)
all(ortho.MM$species == mf1$species) # auto check
ortho.MM[mf1$scheme%in%"terrestrial"&is.na(ortho.MM$Ter),"Ter"] = 1
ortho.MM[mf1$scheme%in%"aquatic",c("Aqu","EcoFresh")] = 1
ortho.MM[ortho.MM$EcoFresh%in%1,"Aqu"] = 1

# Reorder & Save
hab.n = grep("Habitat_",names(ortho.MM))
eco.n = grep("Eco",names(ortho.MM))
col.ref = c("species","scientificName","taxonRank","gbif_status","gbif_accepted","genus","family","order",
	"class","phylum","group","Source","originCH","originEUR","swissRedListCategory","europeanRegionalRedListCategory",
	"iucnRedListCategory","EcoTer","EcoFresh","EcoMar","Fos","Ter","Aqu","Arb","Aer",
	names(ortho.MM[,hab.n]),"RangeSize_km2")
ortho.MM = ortho.MM[,c(col.ref,names(ortho.MM)[!names(ortho.MM)%in%col.ref])]
write.table(ortho.MM,"outputs/raw_traits/non_formated/s2z_raw_traits_orthoptera.txt",row.names=FALSE)


#########################
###### LEPIDOPTERA ######
#########################


# # Open flora indicativa (R version) and add our final modifs
# fi.lepi = as.data.frame(read_excel("./insects/fauna_indicativa/fauna_indicativa_Lepidoptera_for_R.xlsx",1,
# 	col_types=c(rep("text",6),rep("numeric",26),"text",rep("numeric",7),"text",rep("numeric",3),"text",
# 		rep("numeric",11))))
# fi.lepi$species = str_squish(paste(fi.lepi$Genus,fi.lepi$Species,fi.lepi$Subspecies))
# fi.lepi$species = gsub(" NA","",fi.lepi$species)
# fi.lepi[,c("Species","Subspecies","Author")] = NULL
# HAB.ref = gsub("\\..*","",colnames(fi.lepi)[grepl("Habitat_",colnames(fi.lepi))])
# HAB.dup = unique(HAB.ref[duplicated(HAB.ref)])
# for (i in 1:length(HAB.dup)){fi.lepi[,HAB.dup[i]] = as.numeric(apply(fi.lepi[,grepl(HAB.dup[i],colnames(fi.lepi))],1,sum)>0)}
# fi.lepi[grepl("\\.\\.\\.",names(fi.lepi))] = NULL
# col.tar = names(fi.lepi)[grepl("X_",names(fi.lepi))]
# M_M = fi.lepi[,col.tar]
# M_M.yes = apply(M_M,1,function(x) length(which(x>0)))
# M_M.max = apply(M_M,1,function(x) max(which(x>0)))^2
# M_M.max[M_M.max%in%"Inf"] = NA
# fi.lepi$Dispersal_ability = normalize(M_M.max-M_M.yes)
# fi.lepi[,col.tar] = NULL
# fi.lepi[fi.lepi$Overwinter_stage%in%c("Caterpillar","caterpillar"),"Overwinter_stage"] = "larva"
# fi.lepi[fi.lepi$Overwinter_stage%in%"caterpillar-cocoon","Overwinter_stage"] = "cocoon-larva"
# fi.lepi[fi.lepi$Overwinter_stage%in%"cocoon","Overwinter_stage"] = "cocoon"
# fi.lepi[fi.lepi$Overwinter_stage%in%"egg","Overwinter_stage"] = "egg"
# fi.lepi[fi.lepi$Overwinter_stage%in%"egg-caterpillar","Overwinter_stage"] = "egg-larva"
# fi.lepi$EIV_Temp = normalize(fi.lepi$EIV_Temp)
# fi.lepi[fi.lepi$Voltinism%in%0.5,"Voltinism"] = "Semivoltine"
# fi.lepi[fi.lepi$Voltinism%in%1,"Voltinism"] = "Univoltine"
# fi.lepi[fi.lepi$Voltinism%in%2,"Voltinism"] = "Bivoltine"
# fi.lepi[fi.lepi$Voltinism%in%3,"Voltinism"] = "Multivoltine"
# fi.lepi[fi.lepi$LARVA_Diet_range%in%"monophag","LARVA_Diet_range"] = "monophagous"
# fi.lepi[fi.lepi$LARVA_Diet_range%in%c("narrow_oligophag","oligophag"),"LARVA_Diet_range"] = "oligophagous" 
# fi.lepi[fi.lepi$LARVA_Diet_range%in%"polyphag","LARVA_Diet_range"] = "polyphagous"
# fi.lepi[fi.lepi$Litter_size%in%1,"Litter_size"] = "60"
# fi.lepi[fi.lepi$Litter_size%in%2,"Litter_size"] = "95"
# fi.lepi[fi.lepi$Litter_size%in%3,"Litter_size"] = "190"
# fi.lepi[fi.lepi$Litter_size%in%4,"Litter_size"] = "375"
# fi.lepi[fi.lepi$Litter_size%in%5,"Litter_size"] = "750"

# # Open european database of lepitoptera traits
# eu.file = "./insects/European_Butterfly/European_&_Maghreb_Butterfly_Trait_data_v1.2_for_R.xlsx"
# eu.lepi1 = as.data.frame(read_excel(eu.file,1,col_types=c(rep("text",3),rep("numeric",221))))
# eu.lepi2 = as.data.frame(read_excel(eu.file,2,col_types=c(rep("text",6),rep("numeric",2),"text",
# 	rep("numeric",5),rep("text",9),rep("numeric",5),rep("text",6))))
# eu.lepi = data.frame(eu.lepi1,HPSx=eu.lepi2$HPS,HSI=eu.lepi2$HSI)
# GenusSp = gsub("_"," ",eu.lepi[,2])
# split.GS = strsplit(GenusSp," ")
# three.GS = sapply(split.GS,length)%in%3
# eu.lepi$species = GenusSp
# eu.lepi[three.GS,"species"] = sapply(split.GS[three.GS],function(x) paste(x[1],x[2],"subsp.",x[3]))
# keep.hpf = eu.lepi[,names(eu.lepi)[grepl("HPF",names(eu.lepi))]]
# eu.lepi[,c("MainFamilyHost","nFamily")] = NA
# eu.lepi$MainFamilyHost[!is.na(keep.hpf[,1])] = gsub("HPF_","",names(unlist(apply(keep.hpf,1,which.max))))
# eu.lepi$nFamily = apply(keep.hpf,1,function(x) length(x[x>0&!is.na(x)]))
# eu.lepi$Wing_length_mm = (eu.lepi$FoL_var_male_average + eu.lepi$FoL_var_female_average)/2
# eu.lepi$Wingspan_mm = (eu.lepi$WSp_Ts_male_average + eu.lepi$WSp_Ts_female_average)/2
# eu.lepi[eu.lepi$OvS_egg_E%in%1,"OvS_egg_E"] = "egg"
# eu.lepi[eu.lepi$OvS_larva_L%in%1,"OvS_larva_L"] = "larva"
# eu.lepi[eu.lepi$OvS_pupa_P%in%1,"OvS_pupa_P"] = "cocoon"
# eu.lepi[eu.lepi$OvS_adult_A%in%1,"OvS_adult_A"] = "imago"
# eu.lepi$Overwinter_stage = paste0(eu.lepi$OvS_egg_E,"-",eu.lepi$OvS_larva_L,"-",eu.lepi$OvS_pupa_P,"-",eu.lepi$OvS_adult_A)
# eu.lepi$Overwinter_stage = gsub("0-|-0|NA-|-NA","",eu.lepi$Overwinter_stage)
# eu.lepi$Genus = sapply(strsplit(eu.lepi$species," "),function(x) x[1])
# eu.lepi$LARVA_Diet_range = NA
# eu.lepi[eu.lepi[,"HPS_monophage"]%in%1,"LARVA_Diet_range"] = "monophagous"
# eu.lepi[eu.lepi[,"HPS_oligophag..within.one.genus."]%in%1,"LARVA_Diet_range"] = "oligophagous" 
# eu.lepi[eu.lepi[,"HPS_oligophag..within.one.family.."]%in%1,"LARVA_Diet_range"] = "oligophagous"
# eu.lepi[eu.lepi[,"HPS_polyphag"]%in%1,"LARVA_Diet_range"] = "polyphagous"
# names(eu.lepi)[names(eu.lepi)%in%"Vol_biennial_0.5"] = "Semivoltine"
# names(eu.lepi)[names(eu.lepi)%in%"Vol_univoltine_1"] = "Univoltine.a"
# names(eu.lepi)[names(eu.lepi)%in%"Vol_uni.partial2_1.5"] = "Univoltine.b"
# names(eu.lepi)[names(eu.lepi)%in%"Vol_bivoltine_2"] = "Bivoltine"
# names(eu.lepi)[names(eu.lepi)%in%"Vol_multivoltine_3"] = "Multivoltine"
# eu.lepi$Univoltine = eu.lepi$Univoltine.a + eu.lepi$Univoltine.a
# eu.lepi$Univoltine[eu.lepi$Univoltine>0] = 1
# eu.lepi$Univoltine[is.na(eu.lepi$Univoltine.a)&is.na(eu.lepi$Univoltine.b)] = NA
# fos.id1 = apply(eu.lepi[,c("PuL_buried_Bu","LEv_buried_Bu")],1,sum,na.rm=TRUE)
# fos.id2 = apply(eu.lepi[,c("PuL_buried_Bu","LEv_buried_Bu")],1,sum)
# eu.lepi[fos.id1>0,"Fos"] = 1
# eu.lepi[fos.id2%in%0,"Fos"] = 0
# ter.id1 = apply(eu.lepi[,c("PuL_groundlayer_Gl","AdR_Bare.ground_Bg","AdR_short.turf_St","LEv_groundlayer_Gl","AdR_Manmade.structure")],1,sum,na.rm=TRUE)
# ter.id2 = apply(eu.lepi[,c("PuL_groundlayer_Gl","AdR_Bare.ground_Bg","AdR_short.turf_St","LEv_groundlayer_Gl","AdR_Manmade.structure")],1,sum)
# eu.lepi[ter.id1>0,"Ter"] = 1
# eu.lepi[ter.id2%in%0,"Ter"] = 0
# grass.id1 = apply(eu.lepi[,c("PuL_fieldlayer_Gl","AdR_grasses","AdR_short.herb","AdR_tall.herb","LEv_fieldlayer_Fl")],1,sum,na.rm=TRUE)
# grass.id2 = apply(eu.lepi[,c("PuL_fieldlayer_Gl","AdR_grasses","AdR_short.herb","AdR_tall.herb","LEv_fieldlayer_Fl")],1,sum)
# eu.lepi[grass.id1>0,"Habitat_grassland"] = 1
# eu.lepi[grass.id2%in%0,"Habitat_grassland"] = 0
# arb.id1 = apply(eu.lepi[,c("PuL_shrub_layer_Sl","AdR_shrub","LEv_shrublayer_Sl")],1,sum,na.rm=TRUE)
# arb.id2 = apply(eu.lepi[,c("PuL_shrub_layer_Sl","AdR_shrub","LEv_shrublayer_Sl")],1,sum)
# eu.lepi[arb.id1>0,c("Habitat_bushes","Arb")] = 1
# eu.lepi[arb.id2%in%0,"Habitat_bushes"] = 0
# fo.id1 = apply(eu.lepi[,c("PuL_canopy_layer_Cl","AdR_tree.trunk.fence","AdR_tree.canopy","LEv_canopy.layer_Cl")],1,sum,na.rm=TRUE)
# fo.id2 = apply(eu.lepi[,c("PuL_canopy_layer_Cl","AdR_tree.trunk.fence","AdR_tree.canopy","LEv_canopy.layer_Cl")],1,sum)
# eu.lepi[fo.id1>0,c("Habitat_forests","Arb")] = 1
# eu.lepi[fo.id2%in%0,"Habitat_forests"] = 0
# arb.z = arb.id2+fo.id2
# eu.lepi[arb.z%in%0&!is.na(arb.z),"Arb"] = 0
# eu.lepi[eu.lepi$AdR_Manmade.structure>0&!is.na(eu.lepi$AdR_Manmade.structure),"Habitat_urban"] = 1
# eu.lepi[eu.lepi$AdR_Manmade.structure%in%0,"Habitat_urban"] = 0
# eu.lepi[eu.lepi$HPP_flowers_pod_Fl%in%1|eu.lepi$HPP_bud_Bu%in%1,"LARVA_Diet_Flower"] = 1
# eu.lepi[eu.lepi$HPP_flowers_pod_Fl%in%0&eu.lepi$HPP_bud_Bu%in%0,"LARVA_Diet_Flower"] = 0
# eu.lepi[eu.lepi$HPP_leaf_Le%in%1,"LARVA_Diet_Leaf"] = 1
# eu.lepi[eu.lepi$HPP_leaf_Le%in%0,"LARVA_Diet_Leaf"] = 0
# eu.lepi[eu.lepi$HPP_stem_St%in%1,"LARVA_Diet_Stem"] = 1
# eu.lepi[eu.lepi$HPP_stem_St%in%0,"LARVA_Diet_Stem"] = 0
# eu.lepi[eu.lepi$AdF_herb.flower_Hf%in%1|eu.lepi$AdF_shrub.tree.flower_Sf%in%1,"Diet_Flower"] = 1
# eu.lepi[eu.lepi$AdF_herb.flower_Hf%in%0&eu.lepi$AdF_shrub.tree.flower_Sf%in%0,"Diet_Flower"] = 0
# eu.lepi[eu.lepi$AdF_ergot_Er%in%1,"Diet_Fungi"] = 1
# eu.lepi[eu.lepi$AdF_ergot_Er%in%0,"Diet_Fungi"] = 0
# eu.lepi[eu.lepi$AdF_animal_An%in%1,"Diet_Scav"] = 1
# eu.lepi[eu.lepi$AdF_animal_An%in%0,"Diet_Scav"] = 0
# eu.lepi[eu.lepi$AdF_honeydew_Hd%in%1,"ExtraDiet_Honeydew"] = 1
# eu.lepi[eu.lepi$AdF_honeydew_Hd%in%0,"ExtraDiet_Honeydew"] = 0
# eu.lepi[eu.lepi$AdF_sap_Sa%in%1,"ExtraDiet_Sap"] = 1
# eu.lepi[eu.lepi$AdF_sap_Sa%in%0,"ExtraDiet_Sap"] = 0
# eu.lepi[eu.lepi$AdF_decaying.plants_Dp%in%1,"ExtraDiet_Decaying"] = 1
# eu.lepi[eu.lepi$AdF_decaying.plants_Dp%in%0,"ExtraDiet_Decaying"] = 0
# eu.lepi[eu.lepi$AdF_mineral_Mi%in%1,"ExtraDiet_Mineral"] = 1
# eu.lepi[eu.lepi$AdF_mineral_Mi%in%0,"ExtraDiet_Mineral"] = 0
# keep.eu = eu.lepi[,c("species","Genus","Family","Overwinter_stage","Wing_length_mm","Wingspan_mm","Semivoltine","Univoltine","Bivoltine",
# 	"Multivoltine","Fos","Ter","Arb",names(eu.lepi)[grepl("MainFamily|Habitat_|nFamily|Diet_|HPSx|HSI|HPG|BaT",names(eu.lepi))])]

# # Open world database
# world.lepi = read.csv("./insects/LepTraits/consensus.csv",1)
# world.lepi[world.lepi==""] = NA
# world.lepi[,c("WS_L","WS_U","FW_L","FW_U")] = world.lepi[,c("WS_L","WS_U","FW_L","FW_U")]*10
# world.lepi$Wing_length_mm = (world.lepi$FW_L + world.lepi$FW_U)/2
# world.lepi$Wingspan_mm = (world.lepi$WS_L + world.lepi$WS_U)/2
# world.lepi[is.na(world.lepi$PrimaryHostplantFamily),"PrimaryHostplantFamily"] = world.lepi[is.na(world.lepi$PrimaryHostplantFamily),"SoleHostplantFamily"]
# world.lepi[!is.na(world.lepi$SoleHostplantFamily),"SecondaryHostplantFamily"] = "None"
# world.lepi = world.lepi[!duplicated(world.lepi$Species),]
# world.lepi[world.lepi$DiapauseStage%in%c("A","I"),"DiapauseStage"] = "imago"
# world.lepi[world.lepi$DiapauseStage%in%"E","DiapauseStage"] = "egg"
# world.lepi[world.lepi$DiapauseStage%in%"L","DiapauseStage"] = "larva"
# world.lepi[world.lepi$DiapauseStage%in%"P","DiapauseStage"] = "cocoon"
# world.lepi[world.lepi$DiapauseStage%in%"LA","DiapauseStage"] = "larva-imago"
# world.lepi[world.lepi$DiapauseStage%in%c("LE","LLE","LLEE"),"DiapauseStage"] = "egg-larva"
# world.lepi[world.lepi$DiapauseStage%in%"PA","DiapauseStage"] = "cocoon-imago"
# world.lepi[world.lepi$DiapauseStage%in%"PE","DiapauseStage"] = "egg-cocoon"
# world.lepi[world.lepi$DiapauseStage%in%c("PL","PLL","PPL"),"DiapauseStage"] = "larva-cocoon"
# world.lepi[world.lepi$DiapauseStage%in%c("PLA","PPLAAA"),"DiapauseStage"] = "larva-cocoon-imago"
# world.lepi[world.lepi$DiapauseStage%in%c("PLE","PPLE"),"DiapauseStage"] = "larva-cocoon-imago"
# world.lepi[world.lepi$DiapauseStage%in%c("PLEA","PLEAA"),"DiapauseStage"] = "egg-larva-cocoon-imago"
# world.lepi$FlightDuration = normalize(world.lepi$FlightDuration)
# world.lepi[world.lepi$Voltinism%in%"B","Voltinism"] = "Bivoltine"
# world.lepi[world.lepi$Voltinism%in%"U","Voltinism"] = "Univoltine"
# world.lepi[world.lepi$Voltinism%in%"M","Voltinism"] = "Multivoltine"
# world.lepi[,c("EIV_Moisture","Habitat_forests","Habitat_grassland","Habitat_transitional")] = NA
# world.lepi[world.lepi$MoistureAffinity%in%"Xeric-associated (strong)","EIV_Moisture"] = 1
# world.lepi[world.lepi$MoistureAffinity%in%"Xeric-associated (weak)","EIV_Moisture"] = 2
# world.lepi[world.lepi$MoistureAffinity%in%"Mesic-associated (weak)","EIV_Moisture"] = 3
# world.lepi[world.lepi$MoistureAffinity%in%"Mesic-associated (strong)","EIV_Moisture"] = 4
# world.lepi$EIV_Moisture = normalize(world.lepi$EIV_Moisture)
# keep.world = world.lepi[,c("Species","Genus","Family","FlightDuration","DiapauseStage","Voltinism","CanopyAffinity","Habitat_forests",
# 	"NumberOfHostplantFamilies","PrimaryHostplantFamily","SecondaryHostplantFamily","Habitat_grassland","Habitat_transitional",
# 	"Wing_length_mm","Wingspan_mm","EIV_Moisture")]

# # Merge FI and EU
# lepi.merge = merge(fi.lepi,keep.eu,by="species",all=TRUE)
# lepi.merge[is.na(lepi.merge$Family.x),"Family.x"] = lepi.merge[is.na(lepi.merge$Family.x),"Family.y"]
# lepi.merge[is.na(lepi.merge$Genus.x),"Genus.x"] = lepi.merge[is.na(lepi.merge$Genus.x),"Genus.y"]
# lepi.merge[is.na(lepi.merge$LARVA_Diet_Grass),"LARVA_Diet_Grass"] = lepi.merge[is.na(lepi.merge$LARVA_Diet_Grass),"HPG_short.herb_grass_Sh"]
# lepi.merge[lepi.merge$HPG_short.herb_grass_Sh%in%1,"LARVA_Diet_Grass"] = 1
# lepi.merge[is.na(lepi.merge$LARVA_Diet_Herbs),"LARVA_Diet_Herbs"] = lepi.merge[is.na(lepi.merge$LARVA_Diet_Herbs),"HPG_tall.herb..over.30cm.height._Th"]
# lepi.merge[lepi.merge$HPG_tall.herb..over.30cm.height._Th%in%1,"LARVA_Diet_Herbs"] = 1
# lepi.merge[is.na(lepi.merge$LARVA_Diet_Woody),"LARVA_Diet_Woody"] = lepi.merge[is.na(lepi.merge$LARVA_Diet_Woody),"HPG_shrub_Sb"]
# lepi.merge[lepi.merge$HPG_shrub_Sb%in%1,"LARVA_Diet_Woody"] = 1
# lepi.merge[is.na(lepi.merge$LARVA_Diet_Woody),"LARVA_Diet_Woody"] = lepi.merge[is.na(lepi.merge$LARVA_Diet_Woody),"HPG_tree_Tr"]
# lepi.merge[lepi.merge$HPG_tree_Tr%in%1,"LARVA_Diet_Woody"] = 1
# lepi.merge[is.na(lepi.merge$Overwinter_stage.x),"Overwinter_stage.x"] = lepi.merge[is.na(lepi.merge$Overwinter_stage.x),"Overwinter_stage.y"]
# lepi.merge[is.na(lepi.merge$Wing_length_mm.x),"Wing_length_mm.x"] = lepi.merge[is.na(lepi.merge$Wing_length_mm.x),"Wing_length_mm.y"]
# lepi.merge[is.na(lepi.merge$LARVA_Diet_range.x),"LARVA_Diet_range.x"] = lepi.merge[is.na(lepi.merge$LARVA_Diet_range.x),"LARVA_Diet_range.y"]
# lepi.merge[lepi.merge$Voltinism%in%"Bivoltine","Bivoltine"] = 1
# lepi.merge[lepi.merge$Voltinism%in%"Multivoltine","Multivoltine"] = 1
# lepi.merge[lepi.merge$Voltinism%in%"Semivoltine","Semivoltine"] = 1
# lepi.merge[lepi.merge$Voltinism%in%"Univoltine","Univoltine"] = 1
# lepi.merge[is.na(lepi.merge$Habitat_grassland.x),"Habitat_grassland.x"] = lepi.merge[is.na(lepi.merge$Habitat_grassland.x),"Habitat_grassland.y"]
# lepi.merge[lepi.merge$Habitat_grassland.y%in%1,"Habitat_grassland.x"] = 1
# lepi.merge[is.na(lepi.merge$Habitat_bushes.x),"Habitat_bushes.x"] = lepi.merge[is.na(lepi.merge$Habitat_bushes.x),"Habitat_bushes.y"]
# lepi.merge[lepi.merge$Habitat_bushes.y%in%1,"Habitat_bushes.x"] = 1
# lepi.merge[is.na(lepi.merge$Habitat_forests.x),"Habitat_forests.x"] = lepi.merge[is.na(lepi.merge$Habitat_forests.x),"Habitat_forests.y"]
# lepi.merge[lepi.merge$Habitat_forests.y%in%1,"Habitat_forests.x"] = 1
# lepi.merge[,c("Family.y","Genus.y","LARVA_Diet_range.y","Overwinter_stage.y","Wing_length_mm.y","HPG_short.herb_grass_Sh",
# 	"HPG_tall.herb..over.30cm.height._Th","HPG_shrub_Sb","HPG_tree_Tr","Voltinism","Habitat_grassland.y","HPG_non.plant_Np",
# 	"Habitat_forests.y","Habitat_bushes.y","HPG_liana_Li")] = NULL
# names(lepi.merge) = gsub("\\.x","",names(lepi.merge))

# # Merge results with WORLD
# lepi.merge2 = merge(lepi.merge,keep.world,by.x="species",by.y="Species",all=TRUE)
# lepi.merge2[is.na(lepi.merge2$Family.x),"Family.x"] = lepi.merge2[is.na(lepi.merge2$Family.x),"Family.y"]
# lepi.merge2[is.na(lepi.merge2$Genus.x),"Genus.x"] = lepi.merge2[is.na(lepi.merge2$Genus.x),"Genus.y"]
# lepi.merge2[is.na(lepi.merge2$Wing_length_mm.x),"Wing_length_mm.x"] = lepi.merge2[is.na(lepi.merge2$Wing_length_mm.x),"Wing_length_mm.y"]
# lepi.merge2[is.na(lepi.merge2$Wingspan_mm.x),"Wingspan_mm.x"] = lepi.merge2[is.na(lepi.merge2$Wingspan_mm.x),"Wingspan_mm.y"]
# lepi.merge2[is.na(lepi.merge2$nFamily),"nFamily"] = lepi.merge2[is.na(lepi.merge2$nFamily),"NumberOfHostplantFamilies"]
# lepi.merge2[is.na(lepi.merge2$Overwinter_stage),"Overwinter_stage"] = lepi.merge2[is.na(lepi.merge2$Overwinter_stage),"DiapauseStage"]
# lepi.merge2[is.na(lepi.merge2$PrimaryHostplantFamily),"PrimaryHostplantFamily"] = lepi.merge2[is.na(lepi.merge2$PrimaryHostplantFamily),"MainFamilyHost"]
# lepi.merge2[lepi.merge2$Voltinism%in%"Bivoltine","Bivoltine"] = 1
# lepi.merge2[lepi.merge2$Voltinism%in%"Multivoltine","Multivoltine"] = 1
# lepi.merge2[lepi.merge2$Voltinism%in%"Semivoltine","Semivoltine"] = 1
# lepi.merge2[lepi.merge2$Voltinism%in%"Univoltine","Univoltine"] = 1
# lepi.merge2[is.na(lepi.merge2$Habitat_grassland.x),"Habitat_grassland.x"] = lepi.merge2[is.na(lepi.merge2$Habitat_grassland.x),"Habitat_grassland.y"]
# lepi.merge2[lepi.merge2$Habitat_grassland.y%in%1,"Habitat_grassland.x"] = 1
# lepi.merge2[is.na(lepi.merge2$Habitat_forests.x),"Habitat_forests.x"] = lepi.merge2[is.na(lepi.merge2$Habitat_forests.x),"Habitat_forests.y"]
# lepi.merge2[lepi.merge2$Habitat_forests.y%in%1,"Habitat_forests.x"] = 1
# lepi.merge2[,c("Family.y","Genus.y","Wing_length_mm.y","Wingspan_mm.y","NumberOfHostplantFamilies",
# 	"DiapauseStage","MainFamilyHost","Voltinism","Habitat_grassland.y","Habitat_forests.y")] = NULL
# names(lepi.merge2) = gsub("\\.x","",names(lepi.merge2))

# # Merge with Logghe & al. 2025
# logghe.dat = logghe.new[grepl("Lepidoptera",names(logghe.new))][[1]]
# logghe.m = merge(lepi.merge2,logghe.dat,by.x="species",by.y="Species",all=TRUE)
# logghe.m[is.na(logghe.m$BodyLength_mm.x),"BodyLength_mm.x"] = logghe.m[is.na(logghe.m$BodyLength_mm.x),"BodyLength_mm.y"]
# logghe.m[is.na(logghe.m$Litter_size.x),"Litter_size.x"] = logghe.m[is.na(logghe.m$Litter_size.x),"Litter_size.y"]
# logghe.m[is.na(logghe.m$Family.x),"Family.x"] = logghe.m[is.na(logghe.m$Family.x),"Family.y"]
# logghe.m$Order = "Lepidoptera"
# #logghe.m[is.na(logghe.m$EIV_Temp.x),"EIV_Temp.x"] = logghe.m[is.na(logghe.m$EIV_Temp.x),"EIV_Temp.y"]
# #logghe.m[is.na(logghe.m$EIV_Moisture.x),"EIV_Moisture.x"] = logghe.m[is.na(logghe.m$EIV_Moisture.x),"EIV_Moisture.y"]
# logghe.m[is.na(logghe.m$Dispersal_ability.x),"Dispersal_ability.x"] = logghe.m[is.na(logghe.m$Dispersal_ability.x),"Dispersal_ability.y"]
# logghe.m[is.na(logghe.m$Diet_Flower.x),"Diet_Flower.x"] = logghe.m[is.na(logghe.m$Diet_Flower.x),"Diet_Flower.y"]
# logghe.m[logghe.m$Diet_Flower.y%in%1,"Diet_Flower.x"] = 1
# logghe.m[is.na(logghe.m$Diet_Scav.x),"Diet_Scav.x"] = logghe.m[is.na(logghe.m$Diet_Scav.x),"Diet_Scav.y"]
# logghe.m[logghe.m$Diet_Scav.y%in%1,"Diet_Scav.x"] = 1
# logghe.m[is.na(logghe.m$Diet_Fungi.x),"Diet_Fungi.x"] = logghe.m[is.na(logghe.m$Diet_Fungi.x),"Diet_Fungi.y"]
# logghe.m[logghe.m$Diet_Fungi.y%in%1,"Diet_Fungi.x"] = 1
# logghe.m[is.na(logghe.m$Larva_Diet_range),"Larva_Diet_range"] = logghe.m[is.na(logghe.m$Larva_Diet_range),"LARVA_Diet_range"]
# logghe.m[is.na(logghe.m$Semivoltine.x),"Semivoltine.x"] = logghe.m[is.na(logghe.m$Semivoltine.x),"Semivoltine.y"]
# logghe.m[logghe.m$Semivoltine.y%in%1,"Semivoltine.x"] = 1
# logghe.m[is.na(logghe.m$Univoltine.x),"Univoltine.x"] = logghe.m[is.na(logghe.m$Univoltine.x),"Univoltine.y"]
# logghe.m[logghe.m$Univoltine.y%in%1,"Univoltine.x"] = 1
# logghe.m[is.na(logghe.m$Bivoltine.x),"Bivoltine.x"] = logghe.m[is.na(logghe.m$Bivoltine.x),"Bivoltine.y"]
# logghe.m[logghe.m$Bivoltine.y%in%1,"Bivoltine.x"] = 1
# logghe.m[is.na(logghe.m$Multivoltine.x),"Multivoltine.x"] = logghe.m[is.na(logghe.m$Multivoltine.x),"Multivoltine.y"]
# logghe.m[logghe.m$Multivoltine.y%in%1,"Multivoltine.x"] = 1
# logghe.m[grepl("larva",logghe.m$Overwinter_stage),"Overwintering_larva"] = 1
# logghe.m[grepl("imago",logghe.m$Overwinter_stage),"Overwintering_imago"] = 1
# logghe.m[grepl("egg",logghe.m$Overwinter_stage),"Overwintering_egg"] = 1
# logghe.m[grepl("cocoon",logghe.m$Overwinter_stage),"Overwintering_pupa"] = 1
# ow.names = names(logghe.m)[grepl("Overwintering",names(logghe.m))]
# ind.na = apply(logghe.m[,ow.names],1,function(x) !all(is.na(x)))
# logghe.m[ind.na,ow.names][is.na(logghe.m[ind.na,ow.names])] = 0
# logghe.m[is.na(logghe.m$Habitat_urban.x),"Habitat_urban.x"] = logghe.m[is.na(logghe.m$Habitat_urban.x),"Habitat_urban.y"]
# logghe.m[logghe.m$Habitat_urban.y%in%1,"Habitat_urban.x"] = 1
# #logghe.m[is.na(logghe.m$Habitat_green_urban.x),"Habitat_green_urban.x"] = logghe.m[is.na(logghe.m$Habitat_green_urban.x),"Habitat_green_urban.y"]
# logghe.m[is.na(logghe.m$Habitat_agricultural.x),"Habitat_agricultural.x"] = logghe.m[is.na(logghe.m$Habitat_agricultural.x),"Habitat_agricultural.y"]
# logghe.m[logghe.m$Habitat_agricultural.y%in%1,"Habitat_agricultural.x"] = 1
# logghe.m[is.na(logghe.m$Habitat_pioneer.x),"Habitat_pioneer.x"] = logghe.m[is.na(logghe.m$Habitat_pioneer.x),"Habitat_pioneer.y"]
# logghe.m[logghe.m$Habitat_pioneer.y%in%1,"Habitat_pioneer.x"] = 1
# logghe.m[is.na(logghe.m$Habitat_forests.x),"Habitat_forests.x"] = logghe.m[is.na(logghe.m$Habitat_forests.x),"Habitat_forests.y"]
# logghe.m[logghe.m$Habitat_forests.y%in%1,"Habitat_forests.x"] = 1
# #logghe.m[is.na(logghe.m$Habitat_deciduous.x),"Habitat_deciduous.x"] = logghe.m[is.na(logghe.m$Habitat_deciduous.x),"Habitat_deciduous.y"]
# #logghe.m[is.na(logghe.m$Habitat_coniferous.x),"Habitat_coniferous.x"] = logghe.m[is.na(logghe.m$Habitat_coniferous.x),"Habitat_coniferous.y"]
# #logghe.m[is.na(logghe.m$Habitat_mixed.x),"Habitat_mixed.x"] = logghe.m[is.na(logghe.m$Habitat_mixed.x),"Habitat_mixed.y"]
# logghe.m[is.na(logghe.m$Habitat_grassland.x),"Habitat_grassland.x"] = logghe.m[is.na(logghe.m$Habitat_grassland.x),"Habitat_grassland.y"]
# logghe.m[logghe.m$Habitat_grassland.y%in%1,"Habitat_grassland.x"] = 1
# logghe.m[is.na(logghe.m$Habitat_bushes.x),"Habitat_bushes.x"] = logghe.m[is.na(logghe.m$Habitat_bushes.x),"Habitat_bushes.y"]
# logghe.m[logghe.m$Habitat_bushes.y%in%1,"Habitat_bushes.x"] = 1
# logghe.m[is.na(logghe.m$Habitat_transitional.x),"Habitat_transitional.x"] = logghe.m[is.na(logghe.m$Habitat_transitional.x),"Habitat_transitional.y"]
# logghe.m[logghe.m$Habitat_transitional.y%in%1,"Habitat_transitional.x"] = 1
# logghe.m[is.na(logghe.m$Habitat_wetlands.x),"Habitat_wetlands.x"] = logghe.m[is.na(logghe.m$Habitat_wetlands.x),"Habitat_wetlands.y"]
# logghe.m[logghe.m$Habitat_wetlands.y%in%1,"Habitat_wetlands.x"] = 1
# logghe.m[is.na(logghe.m$Habitat_water.x),"Habitat_water.x"] = logghe.m[is.na(logghe.m$Habitat_water.x),"Habitat_water.y"]
# logghe.m[logghe.m$Habitat_water.y%in%1,"Habitat_water.x"] = 1
# logghe.m[is.na(logghe.m$Habitat_rocks.x),"Habitat_rocks.x"] = logghe.m[is.na(logghe.m$Habitat_rocks.x),"Habitat_rocks.y"]
# logghe.m[logghe.m$Habitat_rocks.y%in%1,"Habitat_rocks.x"] = 1
# logghe.m = logghe.m[!grepl("\\.y",names(logghe.m))]
# names(logghe.m) = gsub("\\.x","",names(logghe.m))
# logghe.m[is.na(logghe.m$Imago_Diet)|logghe.m$Imago_Diet%in%c(0,7),"Imago_Diet"] = NA
# # logghe.m[logghe.m$Imago_Diet%in%"Carnivorous","Diet_Carniv"] = 1
# # logghe.m[logghe.m$Imago_Diet%in%"Scavenger","Diet_Scav"] = 1
# # logghe.m[logghe.m$Imago_Diet%in%"Fungivorous","Diet_Fungi"] = 1
# logghe.m[logghe.m$Imago_Diet%in%"Palynivorous","Diet_Flower"] = 1
# logghe.m[,c("Species","LARVA_Diet_range")] = NULL

# # Merge with IUCN ranges
# lepi.range = iucn.ranges[grepl("LEPIDOPTERA",names(iucn.ranges))][[1]]
# lepi.iucn = merge(logghe.m,lepi.range[,c("species","range.size.km2")],by="species",all=TRUE)

# # Change names if necessary
# tGS = to_GenusSpecies(lepi.iucn$species)
# lepi.iucn$species = tGS[,"CanonicalName"]
# lepi.iucn = lepi.iucn[!is.na(lepi.iucn$species),]
# lepi.iucn$IUCN_status = NA

# # Harmonize names & reorder columns 
# names(lepi.iucn)[c(33,41:46,58,105)] = c("Wing_span_mm","Basking_dorsalA","Basking_dorsalR",
# 	"Basking_wings","nGenus","HostPlantIndex","nFamily","Flight_capacity","RangeSize_km2")
# names(lepi.iucn) = gsub("LARVA","Larva",names(lepi.iucn))
# lepi.iucn[lepi.iucn$Diet_Flower%in%1,"Imago_Diet"] = "Palynivorous"
# lepi.iucn[lepi.iucn$Diet_Fungi%in%1,"Imago_Diet"] = "Fungivorous"
# lepi.iucn[lepi.iucn$Diet_Scav%in%1,"Imago_Diet"] = "Scavenger"
# lepi.iucn$Aer = 1
# lepi.iucn$Aqu = NA
# lepi.vert = lepi.iucn[,c(logghe.common,"Riparian_sp","Meadow_sp","CanopyAffinity","BodyWidth_mm",
# 	"Flight_capacity","Wing_length_mm","Wing_width_mm","Wing_span_mm","Basking_dorsalA","Basking_dorsalR",
# 	"Basking_wings","ExtraDiet_Sap","ExtraDiet_Mineral","ExtraDiet_Decaying","ExtraDiet_Honeydew",
# 	"nGenus","nFamily","PrimaryHostplantFamily","SecondaryHostplantFamily","HostPlantIndex","AnnuMeanTemp",
# 	"Biotope_range","Larva_Diet_Flower","Larva_Diet_Leaf","Larva_Diet_Stem","Larva_Diet_Grass",
# 	"Larva_Diet_Herbs","Larva_Diet_Woody")]
# lepi.vert[lepi.vert==""] = NA

# # Convert to numeric
# non.num = c("species","Genus","Family","Order","PrimaryHostplantFamily","SecondaryHostplantFamily","CanopyAffinity",
# 	"Biotope_range","IUCN_status","Activity","Larva_Diet","Imago_Diet","Larva_Diet_range","Imago_Diet_range")
# lepi.vert[,!names(lepi.vert)%in%non.num] = sapply(lepi.vert[,!names(lepi.vert)%in%non.num],as.numeric)

# # Add gbif information
# lepi.vert[,GB.col] = NA
# for (i in 1:nrow(lepi.vert)){
#     cat("...sp.",i)
#     gbif.search = get_status(lepi.vert[i,"species"],order="Lepidoptera") # Search for accepted names
#     if (!is.null(gbif.search)){
#      	lepi.vert$gbif_accepted[i] = gbif.search$scientificName[1] # Fill in the result
#     	lepi.vert$gbif_rank[i] = gbif.search$rank[1]
#         lepi.vert$gbif_phylum[i] = gbif.search$Phylum[1]
#         lepi.vert$gbif_genus[i] = gbif.search$Genus[1]
#         lepi.vert$gbif_order[i] = gbif.search$Order[1]
#         lepi.vert$gbif_family[i] = gbif.search$Family[1]
#         lepi.vert$gbif_class[i] = gbif.search$Class[1]
#         lepi.vert$gbif_IUCN[i] = gbif.search$IUCN_status[1]
#      }
# }
# write.table(lepi.vert,"temp_lepi.txt",row.names=FALSE)
lepi.vert = read.table("data/temp/temp_lepi.txt",header=TRUE)

# Format our table
which.gbif = which(names(lepi.vert)%in%GB.col)
lepi.vert = lepi.vert[,c(1,which.gbif,2:(ncol(lepi.vert)-length(which.gbif)))]
lepi.vert[is.na(lepi.vert$gbif_genus),"gbif_genus"] = lepi.vert[is.na(lepi.vert$gbif_genus),"Genus"]
lepi.vert$gbif_genus[is.na(lepi.vert$gbif_genus)] =
	unlist(sapply(strsplit(lepi.vert$species[is.na(lepi.vert$gbif_genus)]," "),function(x) x[1]))
lepi.vert[is.na(lepi.vert$gbif_family),"gbif_family"] = lepi.vert[is.na(lepi.vert$gbif_family),"Family"]
lepi.vert$gbif_order = "Lepidoptera"
lepi.vert$gbif_class = "Insecta"
lepi.vert$gbif_phylum = "Arthropoda"
lepi.vert[is.na(lepi.vert$IUCN_status),"IUCN_status"] = lepi.vert[is.na(lepi.vert$IUCN_status),"gbif_IUCN"]
lepi.vert[,c("Genus","Family","Order","gbif_IUCN")] = NULL
names(lepi.vert)[2:length(GB.col)] = new.col
gbif.cond = lepi.vert$IUCN_status%in%c("NOT_FOUND","NOT_EVALUATED")
lepi.vert[gbif.cond,"IUCN_status"] = NA

# Extract EU checklist and merge
s2z.lepi = s2z.sp[s2z.sp$group%in%"Butterflies",]
lepi.merge1 = merge(s2z.lepi,lepi.vert,by="species")
lepi.vert = lepi.vert[!lepi.vert$species%in%lepi.merge1$species,]
s2z.lepi2 = s2z.lepi[!s2z.lepi$species%in%lepi.merge1$species,]

# Regroup all the available information for the accepted species (mean when continuous values)
mam.ndup = lepi.vert[!(duplicated(lepi.vert$gbif_accepted)|duplicated(lepi.vert$gbif_accepted,fromLast=TRUE)),]
mam.dup = lepi.vert[duplicated(lepi.vert$gbif_accepted)|duplicated(lepi.vert$gbif_accepted,fromLast=TRUE),]
mam.homo = combine_rows(df=mam.dup,ref.col="gbif_accepted",binary=FALSE)
mam.homo = mam.homo[!is.na(mam.homo$gbif_accepted),]

# Combine, attempt another merge and rbind both results
mam.temp = rbind(mam.ndup,mam.homo)
lepi.merge2 = merge(s2z.lepi2,mam.temp,by="gbif_accepted",all.x=TRUE)
names(lepi.merge2)[1:2] = c("gbif_accepted.x","species")
lepi.merge2$gbif_accepted.y = lepi.merge2$gbif_accepted.x
lepi.merge2 = lepi.merge2[,names(lepi.merge1)]
lepi.MM = rbind(lepi.merge1,lepi.merge2)

# Try to fill in mising information fron one column to another
lepi.MM[is.na(lepi.MM$genus),"genus"] = lepi.MM[is.na(lepi.MM$genus),"Genus"]
lepi.MM[is.na(lepi.MM$family),"family"] = lepi.MM[is.na(lepi.MM$family),"Family"]
lepi.MM[is.na(lepi.MM$order),"order"] = lepi.MM[is.na(lepi.MM$order),"Order"]
lepi.MM[is.na(lepi.MM$class),"class"] = lepi.MM[is.na(lepi.MM$class),"Class"]
lepi.MM[is.na(lepi.MM$phylum),"phylum"] = lepi.MM[is.na(lepi.MM$phylum),"Phylum"]
lepi.MM[grepl("unresolved_accepted",lepi.MM$gbif_accepted.x),"gbif_accepted.x"] =
	lepi.MM[grepl("unresolved_accepted",lepi.MM$gbif_accepted.x),"gbif_accepted.y"]
lepi.MM$gbif_accepted.x[is.na(lepi.MM$gbif_accepted.x)] =
	paste0("unresolved_accepted2_", seq_len(sum(is.na(lepi.MM$gbif_accepted.x))))
lepi.MM[is.na(lepi.MM$taxonRank),"taxonRank"] = lepi.MM[is.na(lepi.MM$taxonRank),"gbif_rank"]
lepi.MM[is.na(lepi.MM$iucnRedListCategory),"iucnRedListCategory"] =
	lepi.MM[is.na(lepi.MM$iucnRedListCategory),"IUCN_status"]
toRemove = grepl("\\.y",names(lepi.MM)) |
	names(lepi.MM)%in%c("Genus","Family","Order","Class","Phylum","gbif_rank","IUCN_status")
lepi.MM[,toRemove] = NULL
names(lepi.MM)[1:ncol(s2z.lepi2)] = names(s2z.lepi2)

# Fine tune Ter/Aqu/Arb info (a)
guild.ter = apply(lepi.MM[,sprintf("GUILDE.%d",c(3,10,12,14:15,20:22,24:25))],1,sum)
lepi.MM[guild.ter>0&!is.na(guild.ter),"Ter"] = 1
guild.aqu = apply(lepi.MM[,sprintf("GUILDE.%d",c(1,2,4:9,26))],1,sum)
lepi.MM[guild.aqu>0&!is.na(guild.aqu),"Aqu"] = 1
guild.arb = apply(lepi.MM[,sprintf("GUILDE.%d",c(11,13,16:19,23))],1,sum)
lepi.MM[guild.arb>0&!is.na(guild.arb),"Arb"] = 1

# Fine tune habitat simple
hab.wat = apply(lepi.MM[,sprintf("GUILDE.%d",c(1,2,4,5,8))],1,sum)
lepi.MM[hab.wat>0 & !is.na(hab.wat),"Habitat_water"] = 1
lepi.MM[hab.wat%in%0 & !lepi.MM$Habitat_water%in%1,"Habitat_water"] = 0
hab.wet = apply(lepi.MM[,sprintf("GUILDE.%d",c(6,7,9,26))],1,sum)
lepi.MM[hab.wet>0 & !is.na(hab.wet),"Habitat_wetlands"] = 1
lepi.MM[hab.wet%in%0 & !lepi.MM$Habitat_wetlands%in%1,"Habitat_wetlands"] = 0
lepi.MM[lepi.MM[,"GUILDE.21"]>0 & !is.na(lepi.MM[,"GUILDE.21"]),"Habitat_rocks"] = 1
lepi.MM[lepi.MM[,"GUILDE.21"]%in%0 & !lepi.MM$Habitat_rocks%in%1,"Habitat_rocks"] = 0
hab.grass = apply(lepi.MM[,sprintf("GUILDE.%d",c(14,20))],1,sum)
lepi.MM[hab.grass>0 & !is.na(hab.grass),"Habitat_grassland"] = 1
lepi.MM[hab.grass%in%0 & !lepi.MM$Habitat_grassland%in%1,"Habitat_grassland"] = 0
hab.forest = apply(lepi.MM[,sprintf("GUILDE.%d",c(16,17,19))],1,sum)
lepi.MM[hab.forest>0 & !is.na(hab.forest),"Habitat_forests"] = 1
lepi.MM[hab.forest%in%0 & !lepi.MM$Habitat_forests%in%1,"Habitat_forests"] = 0
lepi.MM[lepi.MM[,"GUILDE.17"]>0 & !is.na(lepi.MM[,"GUILDE.17"]),"Habitat_deciduous"] = 1
lepi.MM[lepi.MM[,"GUILDE.17"]%in%0 & !lepi.MM$Habitat_deciduous%in%1,"Habitat_deciduous"] = 0
lepi.MM[lepi.MM[,"GUILDE.19"]>0 & !is.na(lepi.MM[,"GUILDE.19"]),"Habitat_coniferous"] = 1
lepi.MM[lepi.MM[,"GUILDE.19"]%in%0 & !lepi.MM$Habitat_coniferous%in%1,"Habitat_coniferous"] = 0
lepi.MM[lepi.MM[,"GUILDE.16"]>0 & !is.na(lepi.MM[,"GUILDE.16"]),"Habitat_mixed"] = 1
lepi.MM[lepi.MM[,"GUILDE.16"]%in%0 & !lepi.MM$Habitat_mixed%in%1,"Habitat_mixed"] = 0
hab.pioneer = apply(lepi.MM[,sprintf("GUILDE.%d",c(3,10,12,22))],1,sum)
lepi.MM[hab.pioneer>0 & !is.na(hab.pioneer),"Habitat_pioneer"] = 1
lepi.MM[hab.pioneer%in%0 & !lepi.MM$Habitat_pioneer%in%1,"Habitat_pioneer"] = 0
hab.urban = apply(lepi.MM[,sprintf("GUILDE.%d",c(22,24))],1,sum)
lepi.MM[hab.urban>0 & !is.na(hab.urban),"Habitat_urban"] = 1
lepi.MM[hab.urban%in%0 & !lepi.MM$Habitat_urban%in%1,"Habitat_urban"] = 0
lepi.MM[lepi.MM[,"GUILDE.23"]>0 & !is.na(lepi.MM[,"GUILDE.23"]),"Habitat_green_urban"] = 1
lepi.MM[lepi.MM[,"GUILDE.23"]%in%0 & !lepi.MM$Habitat_green_urban%in%1,"Habitat_green_urban"] = 0
hab.agri = apply(lepi.MM[,sprintf("GUILDE.%d",c(11,25))],1,sum)
lepi.MM[hab.agri>0 & !is.na(hab.agri),"Habitat_agricultural"] = 1
lepi.MM[hab.agri%in%0 & !lepi.MM$Habitat_agricultural%in%1,"Habitat_agricultural"] = 0
lepi.MM[lepi.MM[,"GUILDE.13"]>0 & !is.na(lepi.MM[,"GUILDE.13"]),"Habitat_bushes"] = 1
lepi.MM[lepi.MM[,"GUILDE.13"]%in%0 & !lepi.MM$Habitat_bushes%in%1,"Habitat_bushes"] = 0
lepi.MM[lepi.MM[,"GUILDE.15"]>0 & !is.na(lepi.MM[,"GUILDE.15"]),"Habitat_transitional"] = 1
lepi.MM[lepi.MM[,"GUILDE.15"]%in%0 & !lepi.MM$Habitat_transitional%in%1,"Habitat_transitional"] = 0
lepi.MM[lepi.MM[,"GUILDE.18"]>0 & !is.na(lepi.MM[,"GUILDE.18"]),"Habitat_heathland"] = 1
lepi.MM[lepi.MM[,"GUILDE.18"]%in%0 & !lepi.MM$Habitat_heathland%in%1,"Habitat_heathland"] = 0
lepi.MM[,HABit[!HABit%in%names(lepi.MM)]] = NA
lepi.MM[,names(lepi.MM)[grepl("GUILD",names(lepi.MM))]] = NULL

# Fine tune Ter/Aqu/Arb info (b)
lepi.MM[lepi.MM$Habitat_urban%in%1,"Ter"] = 1
lepi.MM[lepi.MM$Habitat_green_urban%in%1,"Ter"] = 1
lepi.MM[lepi.MM$Habitat_agriculture%in%1,"Ter"] = 1
lepi.MM[lepi.MM$Habitat_deciduous%in%1,"Arb"] = 1
lepi.MM[lepi.MM$Habitat_coniferous%in%1,"Arb"] = 1
lepi.MM[lepi.MM$Habitat_mixed%in%1,"Arb"] = 1
lepi.MM[lepi.MM$Habitat_grassland%in%1,"Ter"] = 1
lepi.MM[lepi.MM$Habitat_heathland%in%1,"Arb"] = 1
lepi.MM[lepi.MM$Habitat_bushes%in%1,"Arb"] = 1
lepi.MM[lepi.MM$Habitat_sand%in%1,"Ter"] = 1
lepi.MM[lepi.MM$Habitat_wetlands%in%1,"Aqu"] = 1
lepi.MM[lepi.MM$Habitat_water%in%1,"Aqu"] = 1
lepi.MM[lepi.MM$Habitat_rocks%in%1,"Ter"] = 1
lepi.MM[lepi.MM$Habitat_pioneer%in%1,"Ter"] = 1
l.temp = lepi.MM[,c("Ter","Aqu","Arb")]
l.temp [is.na(l.temp)] = 0
l.temp[apply(l.temp,1,function(x) all(x%in%0)),] = NA
lepi.MM[,c("Ter","Aqu","Arb")] = l.temp

# Aquatic, Terrestrial or Marine
lepi.MM[,"EcoMar"]= NA
lepi.MM$EcoTer = 1
lepi.MM$EcoFresh = 0 # See below
lepi.MM$EcoMar = 0
lepi.MM[as.numeric(lepi.MM$Riparian_sp)>2&!is.na(lepi.MM$Riparian_sp),"Aqu"] = 1
lepi.MM$Aer = 1 # No info
#lepi.MM[lepi.MM$family%in%"Crambidae",c("Aqu","EcoFresh")] = 1
#Subfamily "Acentropinae" larva is aquatic (part of family "Crambidae")
# --> only "Cydalima perspectalis" but not aquatic (checked)

# Fine tune Ter/Aqu info (c)
lepi.MM = lepi.MM[order(lepi.MM$species),]
mf1 = merge(lepi.MM,antoine.TerAqu,by="species",all.x=TRUE)
all(lepi.MM$species == mf1$species) # auto check
lepi.MM[mf1$scheme%in%"terrestrial"&is.na(lepi.MM$Ter),"Ter"] = 1
lepi.MM[mf1$scheme%in%"aquatic",c("Aqu","EcoFresh")] = 1
lepi.MM[lepi.MM$EcoFresh%in%1,"Aqu"] = 1

# Reorder & Save
hab.n = grep("Habitat_",names(lepi.MM))
eco.n = grep("Eco",names(lepi.MM))
col.ref = c("species","scientificName","taxonRank","gbif_status","gbif_accepted","genus","family","order",
	"class","phylum","group","Source","originCH","originEUR","swissRedListCategory","europeanRegionalRedListCategory",
	"iucnRedListCategory","EcoTer","EcoFresh","EcoMar","Fos","Ter","Aqu","Arb","Aer",
	names(lepi.MM[,hab.n]),"RangeSize_km2")
lepi.MM = lepi.MM[,c(col.ref,names(lepi.MM)[!names(lepi.MM)%in%col.ref])]
write.table(lepi.MM,"outputs/raw_traits/non_formated/s2z_raw_traits_lepidoptera.txt",row.names=FALSE)


#########################
######### BEES ##########
#########################


# # Open flora indicativa (R version) and add our final modifs
# t.bees = as.data.frame(read_excel("./insects/bee.traits.ch.xlsx",1,col_types=c(rep("text",4),
# 	rep("numeric",2),rep("text",3),rep("numeric",2),rep("text",3),rep("numeric",8),"text",
# 		rep("numeric",4))))
# t.bees[,c("Species","ITD.mean_workers","Pheno_start_BI.WEEKS","Pheno_duration.in.MONTHS",
# 	"Comments","Region","foraging_distance.thd","Sex","body_weigth_estimated","body_weigth_SE",
# 	"body_weigth_CI_Lower","body_weigth_CI_Upper")] = NULL
# names(t.bees)[c(3,4,6,7,9:15)] = c("species","Inter_dist_mm","TongueGuild","Nesting","Body_mass_g","Homing_max_km",
# 	"Foraging_max_km","Comm_max_km","TongueLength_mm","GlossaLength_mm","PrementumLength_mm")

# t.bees$Lecty = tolower(t.bees$Lecty)
# t.bees$TongueGuild = tolower(t.bees$TongueGuild)
# t.bees$Sociality = gsub(" ","_",tolower(t.bees$Sociality))
# t.bees$Nesting = gsub("\\: ","_",t.bees$Nesting)
# t.bees$Nesting = tolower(gsub(" ","_",t.bees$Nesting))
# t.bees[t.bees$Nesting%in%c("cleptoparasite","social_parasite"),"Nesting"] = "parasite"
# t.bees[t.bees$Nesting%in%"carder"|grepl("renter",t.bees$Nesting),"Nesting"] = "cavity-nesting"
# t.bees[grepl("excavator",t.bees$Nesting),"Nesting"] = "ground-nesting"
# t.bees$Lecty[t.bees$Lecty%in%"na"] = NA
# t.bees$TongueGuild[t.bees$TongueGuild%in%"na"] = NA
# t.bees$Sociality[t.bees$Sociality%in%"na"] = NA
# t.bees$Nesting[t.bees$Nesting%in%"na"] = NA
# t.bees$Body_mass_g = t.bees$Body_mass_g/1000

# # Merge with Logghe & al. 2025
# logghe.dat = logghe.new[grepl("Hymenoptera",names(logghe.new))][[1]]
# logghe.m = merge(t.bees,logghe.dat,by.x="species",by.y="Species",all=TRUE)
# logghe.m[is.na(logghe.m$Family.x),"Family.x"] = logghe.m[is.na(logghe.m$Family.x),"Family.y"]
# logghe.m[is.na(logghe.m$Dispersal_km),"Dispersal_km"] = logghe.m[is.na(logghe.m$Dispersal_km),"Homing_max_km"]
# logghe.m$Order = "Hymenoptera"
# logghe.m$Family.y = NULL
# names(logghe.m) = gsub("\\.x","",names(logghe.m))

# # Merge with IUCN ranges
# bee.range = iucn.ranges[grepl("HYMENOPTERA",names(iucn.ranges))][[1]]
# bee.iucn = merge(logghe.m,bee.range[,c("species","range.size.km2")],by="species",all=TRUE)
# bee.iucn$RangeSize_km2 = bee.iucn$range.size.km2

# # Hab
# bee.iucn$Fos = NA
# bee.iucn$Ter = NA # Can be arboreal...
# bee.iucn$Aqu = NA
# bee.iucn$Arb = NA
# bee.iucn$Aer = 1 # Logical

# # Change names if necessary
# tGS = to_GenusSpecies(bee.iucn$species)
# bee.iucn$species = tGS[,"CanonicalName"]
# bee.iucn$IUCN_status = NA

# # Rename
# names(bee.iucn)[c(6,13:15)] = c("Tongue_guild","Tongue_length_mm","Glossa_length_mm","Prementum_length_mm")

# # Harmonize names & reorder columns 
# bees.vert = bee.iucn[,c(logghe.common,"Foraging_max_km","Comm_max_km","Homing_max_km","Sociality",
# 	"Nesting","Body_mass_g","Inter_dist_mm","Tongue_guild","Tongue_length_mm","Glossa_length_mm",
# 	"Prementum_length_mm","Lecty")]
# bees.vert[bees.vert==""] = NA

# # Convert to numeric
# non.num = c("species","Genus","Family","Sociality","Nesting","Tongue_guild","Lecty","Order","IUCN_status",
# 	"Activity","Larva_Diet","Imago_Diet","Larva_Diet_range","Imago_Diet_range")
# bees.vert[,!names(bees.vert)%in%non.num] = sapply(bees.vert[,!names(bees.vert)%in%non.num],as.numeric)

# # Add gbif information
# bees.vert[,GB.col] = NA
# for (i in 1:nrow(bees.vert)){
#     cat("...sp.",i)
#     gbif.search = get_status(bees.vert[i,"species"],order="Hymenoptera") # Search for accepted names
#     if (!is.null(gbif.search)){
#     	bees.vert$gbif_accepted[i] = gbif.search$scientificName[1] # Fill in the result
#     	bees.vert$gbif_rank[i] = gbif.search$rank[1]
#         bees.vert$gbif_phylum[i] = gbif.search$Phylum[1]
#         bees.vert$gbif_genus[i] = gbif.search$Genus[1]
#         bees.vert$gbif_order[i] = gbif.search$Order[1]
#         bees.vert$gbif_family[i] = gbif.search$Family[1]
#         bees.vert$gbif_class[i] = gbif.search$Class[1]
#         bees.vert$gbif_IUCN[i] = gbif.search$IUCN_status[1]
#     }
# }
# write.table(bees.vert,"temp_wasps.txt",row.names=FALSE)
bees.vert = read.table("data/temp/temp_bees_wasps.txt",header=TRUE)

# Format our table
which.gbif = which(names(bees.vert)%in%GB.col)
bees.vert = bees.vert[,c(1,which.gbif,2:(ncol(bees.vert)-length(which.gbif)))]
bees.vert[is.na(bees.vert$gbif_genus),"gbif_genus"] = bees.vert[is.na(bees.vert$gbif_genus),"Genus"]
bees.vert$gbif_genus[is.na(bees.vert$gbif_genus)] =
	unlist(sapply(strsplit(bees.vert$species[is.na(bees.vert$gbif_genus)]," "),function(x) x[1]))
bees.vert[is.na(bees.vert$gbif_family),"gbif_family"] = bees.vert[is.na(bees.vert$gbif_family),"Family"]
bees.vert$gbif_order = "Hymenoptera"
bees.vert$gbif_class = "Insecta"
bees.vert$gbif_phylum = "Arthropoda"
bees.vert[is.na(bees.vert$IUCN_status),"IUCN_status"] = bees.vert[is.na(bees.vert$IUCN_status),"gbif_IUCN"]
bees.vert[,c("Genus","Family","Order","gbif_IUCN")] = NULL
names(bees.vert)[2:length(GB.col)] = new.col
gbif.cond = bees.vert$IUCN_status%in%c("NOT_FOUND","NOT_EVALUATED")
bees.vert[gbif.cond,"IUCN_status"] = NA

# Extract EU checklist and merge
s2z.bees = s2z.sp[s2z.sp$group%in%"Bees"|s2z.sp$group%in%"Wasps",]
bees.merge1 = merge(s2z.bees,bees.vert,by="species")
bees.vert = bees.vert[!bees.vert$species%in%bees.merge1$species,]
s2z.bees2 = s2z.bees[!s2z.bees$species%in%bees.merge1$species,]

# Regroup all the available information for the accepted species (mean when continuous values)
mam.ndup = bees.vert[!(duplicated(bees.vert$gbif_accepted)|duplicated(bees.vert$gbif_accepted,fromLast=TRUE)),]
mam.dup = bees.vert[duplicated(bees.vert$gbif_accepted)|duplicated(bees.vert$gbif_accepted,fromLast=TRUE),]
mam.homo = combine_rows(df=mam.dup,ref.col="gbif_accepted",binary=FALSE)
mam.homo = mam.homo[!is.na(mam.homo$gbif_accepted),]

# Combine, attempt another merge and rbind both results
mam.temp = rbind(mam.ndup,mam.homo)
bees.merge2 = merge(s2z.bees2,mam.temp,by="gbif_accepted",all.x=TRUE)
names(bees.merge2)[1:2] = c("gbif_accepted.x","species")
bees.merge2$gbif_accepted.y = bees.merge2$gbif_accepted.x
bees.merge2 = bees.merge2[,names(bees.merge1)]
bees.MM = rbind(bees.merge1,bees.merge2)

# Try to fill in mising information fron one column to another
bees.MM[is.na(bees.MM$genus),"genus"] = bees.MM[is.na(bees.MM$genus),"Genus"]
bees.MM[is.na(bees.MM$family),"family"] = bees.MM[is.na(bees.MM$family),"Family"]
bees.MM[is.na(bees.MM$order),"order"] = bees.MM[is.na(bees.MM$order),"Order"]
bees.MM[is.na(bees.MM$class),"class"] = bees.MM[is.na(bees.MM$class),"Class"]
bees.MM[is.na(bees.MM$phylum),"phylum"] = bees.MM[is.na(bees.MM$phylum),"Phylum"]
bees.MM[grepl("unresolved_accepted",bees.MM$gbif_accepted.x),"gbif_accepted.x"] =
	bees.MM[grepl("unresolved_accepted",bees.MM$gbif_accepted.x),"gbif_accepted.y"]
bees.MM$gbif_accepted.x[is.na(bees.MM$gbif_accepted.x)] =
	paste0("unresolved_accepted2_", seq_len(sum(is.na(bees.MM$gbif_accepted.x))))
bees.MM[is.na(bees.MM$taxonRank),"taxonRank"] = bees.MM[is.na(bees.MM$taxonRank),"gbif_rank"]
bees.MM[is.na(bees.MM$iucnRedListCategory),"iucnRedListCategory"] =
	bees.MM[is.na(bees.MM$iucnRedListCategory),"IUCN_status"]
toRemove = grepl("\\.y",names(bees.MM)) |
	names(bees.MM)%in%c("Genus","Family","Order","Class","Phylum","gbif_rank","IUCN_status")
bees.MM[,toRemove] = NULL
names(bees.MM)[1:ncol(s2z.bees2)] = names(s2z.bees2)

# Fine tune Ter/Aqu/Arb info (a)
guild.ter = apply(bees.MM[,sprintf("GUILDE.%d",c(3,10,12,14:15,20:22,24:25))],1,sum)
bees.MM[guild.ter>0&!is.na(guild.ter),"Ter"] = 1
guild.aqu = apply(bees.MM[,sprintf("GUILDE.%d",c(1,2,4:9,26))],1,sum)
bees.MM[guild.aqu>0&!is.na(guild.aqu),"Aqu"] = 1
guild.arb = apply(bees.MM[,sprintf("GUILDE.%d",c(11,13,16:19,23))],1,sum)
bees.MM[guild.arb>0&!is.na(guild.arb),"Arb"] = 1

# Fine tune habitat simple
hab.wat = apply(bees.MM[,sprintf("GUILDE.%d",c(1,2,4,5,8))],1,sum)
bees.MM[hab.wat>0 & !is.na(hab.wat),"Habitat_water"] = 1
bees.MM[hab.wat%in%0 & !bees.MM$Habitat_water%in%1,"Habitat_water"] = 0
hab.wet = apply(bees.MM[,sprintf("GUILDE.%d",c(6,7,9,26))],1,sum)
bees.MM[hab.wet>0 & !is.na(hab.wet),"Habitat_wetlands"] = 1
bees.MM[hab.wet%in%0 & !bees.MM$Habitat_wetlands%in%1,"Habitat_wetlands"] = 0
bees.MM[bees.MM[,"GUILDE.21"]>0 & !is.na(bees.MM[,"GUILDE.21"]),"Habitat_rocks"] = 1
bees.MM[bees.MM[,"GUILDE.21"]%in%0 & !bees.MM$Habitat_rocks%in%1,"Habitat_rocks"] = 0
hab.grass = apply(bees.MM[,sprintf("GUILDE.%d",c(14,20))],1,sum)
bees.MM[hab.grass>0 & !is.na(hab.grass),"Habitat_grassland"] = 1
bees.MM[hab.grass%in%0 & !bees.MM$Habitat_grassland%in%1,"Habitat_grassland"] = 0
hab.forest = apply(bees.MM[,sprintf("GUILDE.%d",c(16,17,19))],1,sum)
bees.MM[hab.forest>0 & !is.na(hab.forest),"Habitat_forests"] = 1
bees.MM[hab.forest%in%0 & !bees.MM$Habitat_forests%in%1,"Habitat_forests"] = 0
bees.MM[bees.MM[,"GUILDE.17"]>0 & !is.na(bees.MM[,"GUILDE.17"]),"Habitat_deciduous"] = 1
bees.MM[bees.MM[,"GUILDE.17"]%in%0 & !bees.MM$Habitat_deciduous%in%1,"Habitat_deciduous"] = 0
bees.MM[bees.MM[,"GUILDE.19"]>0 & !is.na(bees.MM[,"GUILDE.19"]),"Habitat_coniferous"] = 1
bees.MM[bees.MM[,"GUILDE.19"]%in%0 & !bees.MM$Habitat_coniferous%in%1,"Habitat_coniferous"] = 0
bees.MM[bees.MM[,"GUILDE.16"]>0 & !is.na(bees.MM[,"GUILDE.16"]),"Habitat_mixed"] = 1
bees.MM[bees.MM[,"GUILDE.16"]%in%0 & !bees.MM$Habitat_mixed%in%1,"Habitat_mixed"] = 0
hab.pioneer = apply(bees.MM[,sprintf("GUILDE.%d",c(3,10,12,22))],1,sum)
bees.MM[hab.pioneer>0 & !is.na(hab.pioneer),"Habitat_pioneer"] = 1
bees.MM[hab.pioneer%in%0 & !bees.MM$Habitat_pioneer%in%1,"Habitat_pioneer"] = 0
hab.urban = apply(bees.MM[,sprintf("GUILDE.%d",c(22,24))],1,sum)
bees.MM[hab.urban>0 & !is.na(hab.urban),"Habitat_urban"] = 1
bees.MM[hab.urban%in%0 & !bees.MM$Habitat_urban%in%1,"Habitat_urban"] = 0
bees.MM[bees.MM[,"GUILDE.23"]>0 & !is.na(bees.MM[,"GUILDE.23"]),"Habitat_green_urban"] = 1
bees.MM[bees.MM[,"GUILDE.23"]%in%0 & !bees.MM$Habitat_green_urban%in%1,"Habitat_green_urban"] = 0
hab.agri = apply(bees.MM[,sprintf("GUILDE.%d",c(11,25))],1,sum)
bees.MM[hab.agri>0 & !is.na(hab.agri),"Habitat_agricultural"] = 1
bees.MM[hab.agri%in%0 & !bees.MM$Habitat_agricultural%in%1,"Habitat_agricultural"] = 0
bees.MM[bees.MM[,"GUILDE.13"]>0 & !is.na(bees.MM[,"GUILDE.13"]),"Habitat_bushes"] = 1
bees.MM[bees.MM[,"GUILDE.13"]%in%0 & !bees.MM$Habitat_bushes%in%1,"Habitat_bushes"] = 0
bees.MM[bees.MM[,"GUILDE.15"]>0 & !is.na(bees.MM[,"GUILDE.15"]),"Habitat_transitional"] = 1
bees.MM[bees.MM[,"GUILDE.15"]%in%0 & !bees.MM$Habitat_transitional%in%1,"Habitat_transitional"] = 0
bees.MM[bees.MM[,"GUILDE.18"]>0 & !is.na(bees.MM[,"GUILDE.18"]),"Habitat_heathland"] = 1
bees.MM[bees.MM[,"GUILDE.18"]%in%0 & !bees.MM$Habitat_heathland%in%1,"Habitat_heathland"] = 0
bees.MM[,HABit[!HABit%in%names(bees.MM)]] = NA
bees.MM[,names(bees.MM)[grepl("GUILD",names(bees.MM))]] = NULL

# Fine tune Ter/Aqu/Arb info (b)
bees.MM[bees.MM$Habitat_urban%in%1,"Ter"] = 1
bees.MM[bees.MM$Habitat_green_urban%in%1,"Ter"] = 1
bees.MM[bees.MM$Habitat_agriculture%in%1,"Ter"] = 1
bees.MM[bees.MM$Habitat_deciduous%in%1,"Arb"] = 1
bees.MM[bees.MM$Habitat_coniferous%in%1,"Arb"] = 1
bees.MM[bees.MM$Habitat_mixed%in%1,"Arb"] = 1
bees.MM[bees.MM$Habitat_grassland%in%1,"Ter"] = 1
bees.MM[bees.MM$Habitat_heathland%in%1,"Arb"] = 1
bees.MM[bees.MM$Habitat_bushes%in%1,"Arb"] = 1
bees.MM[bees.MM$Habitat_sand%in%1,"Ter"] = 1
bees.MM[bees.MM$Habitat_wetlands%in%1,"Aqu"] = 1
bees.MM[bees.MM$Habitat_water%in%1,"Aqu"] = 1
bees.MM[bees.MM$Habitat_rocks%in%1,"Ter"] = 1
bees.MM[bees.MM$Habitat_pioneer%in%1,"Ter"] = 1
l.temp = bees.MM[,c("Ter","Aqu","Arb")]
l.temp [is.na(l.temp)] = 0
l.temp[apply(l.temp,1,function(x) all(x%in%0)),] = NA
bees.MM[,c("Ter","Aqu","Arb")] = l.temp

# Aquatic, Terrestrial or Marine
bees.MM[,"EcoMar"]= NA
bees.MM$EcoTer = 1
bees.MM$EcoFresh = 0 # Logical
bees.MM$EcoMar = 0
bees.MM$Aer = 1

# Fine tune Ter/Aqu info (c)
bees.MM = bees.MM[order(bees.MM$species),]
mf1 = merge(bees.MM,antoine.TerAqu,by="species",all.x=TRUE)
all(bees.MM$species == mf1$species) # auto check
bees.MM[mf1$scheme%in%"terrestrial"&is.na(bees.MM$Ter),"Ter"] = 1
bees.MM[mf1$scheme%in%"aquatic",c("Aqu","EcoFresh")] = 1
bees.MM[bees.MM$EcoFresh%in%1,"Aqu"] = 1

# Reorder & Save
hab.n = grep("Habitat_",names(bees.MM))
eco.n = grep("Eco",names(bees.MM))
col.ref = c("species","scientificName","taxonRank","gbif_status","gbif_accepted","genus","family","order",
	"class","phylum","group","Source","originCH","originEUR","swissRedListCategory","europeanRegionalRedListCategory",
	"iucnRedListCategory","EcoTer","EcoFresh","EcoMar","Fos","Ter","Aqu","Arb","Aer",
	names(bees.MM[,hab.n]),"RangeSize_km2")
bees.MM = bees.MM[,c(col.ref,names(bees.MM)[!names(bees.MM)%in%col.ref])]
write.table(bees.MM,"outputs/raw_traits/non_formated/s2z_raw_traits_bees_wasps.txt",row.names=FALSE)


#########################
######### FUNGI #########
#########################


# # Open SwissFungi first and modify key columns
# swissF = as.data.frame(read_excel("./fungi/2023_10_31_Datenauszug_SwissFungi.xlsx",1))
# swissF[grepl("Algensymbiose",swissF$FUNCTIONAL_GROUP),"FUNCTIONAL_GROUP"] = "lichen"
# swissF[grepl("Ektomykorrhiza",swissF$FUNCTIONAL_GROUP),"FUNCTIONAL_GROUP"] = "ectomycorrhiza"
# swissF[grepl("Ericoide Mykorrhiza",swissF$FUNCTIONAL_GROUP),"FUNCTIONAL_GROUP"] = "ericoid_mycorrhiza"
# swissF[grepl("Parasiten",swissF$FUNCTIONAL_GROUP),"FUNCTIONAL_GROUP"] = "parasite"
# swissF[grepl("Lichenicole Pilze",swissF$FUNCTIONAL_GROUP),"FUNCTIONAL_GROUP"] = "lichenicolous_fungus"
# swissF[grepl("Saprobe",swissF$FUNCTIONAL_GROUP),"FUNCTIONAL_GROUP"] = "saprobe"
# swissF[grepl("VAM/Arbuskuläre",swissF$FUNCTIONAL_GROUP),"FUNCTIONAL_GROUP"] = "arbuscular_mycorrhiza"
# swissF[grepl("in soil",swissF$SUBSTRATE_E),"SUBSTRATE_E"] = "soil_peat"
# swissF[grepl("on animal",swissF$SUBSTRATE_E),"SUBSTRATE_E"] = "animal_n_other"
# swissF[grepl("on cones",swissF$SUBSTRATE_E),"SUBSTRATE_E"] = "tree_cones"
# swissF[grepl("on dung",swissF$SUBSTRATE_E),"SUBSTRATE_E"] = "dung"
# swissF[grepl("on fire",swissF$SUBSTRATE_E),"SUBSTRATE_E"] = "fire_places"
# swissF[grepl("on fungi",swissF$SUBSTRATE_E),"SUBSTRATE_E"] = "fungi"
# swissF[grepl("on litter",swissF$SUBSTRATE_E),"SUBSTRATE_E"] = "above_litter"
# swissF[grepl("on mosses",swissF$SUBSTRATE_E),"SUBSTRATE_E"] = "mosses_lichens"
# swissF[grepl("on roots",swissF$SUBSTRATE_E),"SUBSTRATE_E"] = "roots_rhizomes"
# swissF[grepl("on sandy",swissF$SUBSTRATE_E),"SUBSTRATE_E"] = "sandy_soil"
# swissF[grepl("on wood",swissF$SUBSTRATE_E),"SUBSTRATE_E"] = "wood"
# swissF = swissF[!duplicated(swissF$TAXON),]

# # Open FungalTraits and keep key columns
# fungalT = read.csv("./fungi/FungalTraits/funToTheFun.csv")
# fungalT = fungalT[,c("speciesMatched","trait_name","value")]
# key.col = c("em_expl","spore_width","spore_length","spore_size","sporocarp_N",
# 	"melanin_content","tissue_c","tissue_n","tissue_p","tissue_cn","tissue_cp","tissue_np",
# 	"ascoma_development","ascus_dehiscence","ascoma_type","fruiting_body_size")
# fungalT = fungalT[fungalT$trait_name%in%key.col,]

# # Create a new matrix comprising unique sp. name + traits
# FT.mat = unique_to_df(fungalT)
# FT.mat$ascoma_development = gsub("\\?.*","",FT.mat$ascoma_development)
# FT.mat$ascoma_development = tolower(FT.mat$ascoma_development)
# FT.mat$ascus_dehiscence = gsub("-","_",FT.mat$ascus_dehiscence)
# FT.mat$ascus_dehiscence = tolower(FT.mat$ascus_dehiscence)
# FT.mat$ascoma_type = gsub(" ","_",FT.mat$ascoma_type)
# FT.mat$ascoma_type = tolower(FT.mat$ascoma_type)

# # Merge both databases
# fungi.merge = merge(FT.mat,swissF,by.x="species",by.y="TAXON",all=TRUE)

# # Merge with IUCN ranges
# fungi.range = iucn.ranges[grepl("FUNGI",names(iucn.ranges))][[1]]
# fungi.iucn = merge(fungi.merge,fungi.range[,c("species","range.size.km2")],by="species",all.x=TRUE) #all.x normal
# fungi.iucn$RangeSize_km2 = fungi.iucn$range.size.km2

# # Remove vague species names
# tGS = to_GenusSpecies(fungi.iucn$species)
# fungi.iucn$species = tGS[,"CanonicalName"]
# fungi.iucn$IUCN_status = NA
# fungi.iucn = fungi.iucn[!is.na(fungi.iucn$species),]
# fungi.iucn = fungi.iucn[!duplicated(fungi.iucn$species),]

# # Add habitat columns
# fungi.iucn$Fos = 0
# fungi.iucn$Ter = 0 # Can arboreal, fosseal...
# fungi.iucn$Aqu = 0 # Can be aquatic...
# fungi.iucn$Arb = 0
# fungi.iucn$Aer = 0
# all.cl = c("soil_peat","roots_rhizomes","wood","tree_cones")
# fungi.iucn[!is.na(fungi.iucn$SUBSTRATE_E)&!fungi.iucn$SUBSTRATE_E%in%all.cl,"Ter"] = 1
# fungi.iucn[fungi.iucn$SUBSTRATE_E%in%all.cl[1:2],"Fos"] = 1
# fungi.iucn[fungi.iucn$SUBSTRATE_E%in%all.cl[3:4],"Arb"] = 1
# fungi.iucn[is.na(fungi.iucn$SUBSTRATE_E),c("Ter","Fos","Arb")] = NA

# # Harmonize and reorder columns
# names(fungi.iucn)[c(3:17,20,23)] = c("Spore_width_μm","Spore_length_μm","Spore_size_μm3","Sporocarp_N_perct","Melanin_content_mg.g",
# 	"C_perct","N_perct","P_perct","C.N_perct","C.P_perct","N.P_perct","Ascoma_development","Ascus_dehiscence","Ascoma_type",
# 	"Fruiting_body_size_mm2","Functional_group","Substrate")
# fungi.vert = fungi.iucn[,c("species","Functional_group","Ascoma_type","Ascoma_development","Ascus_dehiscence",
# 	"C_perct","N_perct","P_perct","C.N_perct","C.P_perct","N.P_perct","Sporocarp_N_perct","Melanin_content_mg.g","Spore_width_μm",
# 	"Spore_length_μm","Spore_size_μm3","Fruiting_body_size_mm2","Fos","Ter","Aqu","Arb","Aer","Substrate",
# 	"RangeSize_km2","IUCN_status")]
# fungi.vert[fungi.vert==""] = NA
# fungi.vert$IUCN_status = NA

# # Convert to numeric
# non.num = c("species","Functional_group","Ascoma_type","Ascoma_development","Ascus_dehiscence","Substrate","IUCN_status")
# fungi.vert[,!names(fungi.vert)%in%non.num] = sapply(fungi.vert[,!names(fungi.vert)%in%non.num],as.numeric)

# # Add gbif information
# fungi.vert[,GB.col] = NA
# for (i in 1:nrow(fungi.vert)){
#     cat("...sp.",i)
#     gbif.search = get_status(fungi.vert[i,"species"]) # Search for accepted names
#     if (!is.null(gbif.search)){
#     	fungi.vert$gbif_accepted[i] = gbif.search$scientificName[1] # Fill in the result
#     	fungi.vert$gbif_rank[i] = gbif.search$rank[1]
#         fungi.vert$gbif_phylum[i] = gbif.search$Phylum[1]
#         fungi.vert$gbif_genus[i] = gbif.search$Genus[1]
#         fungi.vert$gbif_order[i] = gbif.search$Order[1]
#         fungi.vert$gbif_family[i] = gbif.search$Family[1]
#          fungi.vert$gbif_class[i] = gbif.search$Class[1]
#         fungi.vert$gbif_IUCN[i] = gbif.search$IUCN_status[1]
#     }
# }
# write.table(fungi.vert,"temp_fungi.txt",row.names=FALSE)
fungi.vert = read.table("data/temp/temp_fungi.txt",header=TRUE)

# Format our table
which.gbif = which(names(fungi.vert)%in%GB.col)
fungi.vert = fungi.vert[,c(1,which.gbif,2:(ncol(fungi.vert)-length(which.gbif)))]
fungi.vert$gbif_genus[is.na(fungi.vert$gbif_genus)] =
	sapply(strsplit(fungi.vert$species[is.na(fungi.vert$gbif_genus)]," "),function(x) x[1])
fungi.vert[is.na(fungi.vert$IUCN_status),"IUCN_status"] = fungi.vert[is.na(fungi.vert$IUCN_status),"gbif_IUCN"]
fungi.vert[,c("Genus","Family","Order","gbif_IUCN")] = NULL
names(fungi.vert)[2:length(GB.col)] = new.col
gbif.cond = fungi.vert$IUCN_status%in%c("NOT_FOUND","NOT_EVALUATED")
fungi.vert[gbif.cond,"IUCN_status"] = NA

# Extract EU checklist and merge
s2z.fungi = s2z.sp[s2z.sp$group%in%"Fungi",]
fungi.merge1 = merge(s2z.fungi,fungi.vert,by="species")
fungi.vert = fungi.vert[!fungi.vert$species%in%fungi.merge1$species,]
s2z.fungi2 = s2z.fungi[!s2z.fungi$species%in%fungi.merge1$species,]

# Regroup all the available information for the accepted species (mean when continuous values)
mam.ndup = fungi.vert[!(duplicated(fungi.vert$gbif_accepted)|duplicated(fungi.vert$gbif_accepted,fromLast=TRUE)),]
mam.dup = fungi.vert[duplicated(fungi.vert$gbif_accepted)|duplicated(fungi.vert$gbif_accepted,fromLast=TRUE),]
mam.homo = combine_rows(df=mam.dup,ref.col="gbif_accepted",binary=FALSE)
mam.homo = mam.homo[!is.na(mam.homo$gbif_accepted),]

# Combine, attempt another merge and rbind both results
mam.temp = rbind(mam.ndup,mam.homo)
fungi.merge2 = merge(s2z.fungi2,mam.temp,by="gbif_accepted",all.x=TRUE)
names(fungi.merge2)[1:2] = c("gbif_accepted.x","species")
fungi.merge2$gbif_accepted.y = fungi.merge2$gbif_accepted.x
fungi.merge2 = fungi.merge2[,names(fungi.merge1)]
fungi.MM = rbind(fungi.merge1,fungi.merge2)

# Try to fill in mising information fron one column to another
fungi.MM[is.na(fungi.MM$genus),"genus"] = fungi.MM[is.na(fungi.MM$genus),"Genus"]
fungi.MM[is.na(fungi.MM$family),"family"] = fungi.MM[is.na(fungi.MM$family),"Family"]
fungi.MM[is.na(fungi.MM$order),"order"] = fungi.MM[is.na(fungi.MM$order),"Order"]
fungi.MM[is.na(fungi.MM$class),"class"] = fungi.MM[is.na(fungi.MM$class),"Class"]
fungi.MM[is.na(fungi.MM$phylum),"phylum"] = fungi.MM[is.na(fungi.MM$phylum),"Phylum"]
fungi.MM[grepl("unresolved_accepted",fungi.MM$gbif_accepted.x),"gbif_accepted.x"] =
	fungi.MM[grepl("unresolved_accepted",fungi.MM$gbif_accepted.x),"gbif_accepted.y"]
fungi.MM$gbif_accepted.x[is.na(fungi.MM$gbif_accepted.x)] =
	paste0("unresolved_accepted2_", seq_len(sum(is.na(fungi.MM$gbif_accepted.x))))
fungi.MM[is.na(fungi.MM$taxonRank),"taxonRank"] = fungi.MM[is.na(fungi.MM$taxonRank),"gbif_rank"]
fungi.MM[is.na(fungi.MM$iucnRedListCategory),"iucnRedListCategory"] =
	fungi.MM[is.na(fungi.MM$iucnRedListCategory),"IUCN_status"]
toRemove = grepl("\\.y",names(fungi.MM)) |
	names(fungi.MM)%in%c("Genus","Family","Order","Class","Phylum","gbif_rank","IUCN_status")
fungi.MM[,toRemove] = NULL
names(fungi.MM)[1:ncol(s2z.fungi2)] = names(s2z.fungi2)

# Fine tune Ter/Aqu/Arb info (a)
guild.ter = apply(fungi.MM[,sprintf("GUILDE.%d",c(3,10,12,14:15,20:22,24:25))],1,sum)
fungi.MM[guild.ter>0&!is.na(guild.ter),"Ter"] = 1
guild.aqu = apply(fungi.MM[,sprintf("GUILDE.%d",c(1,2,4:9,26))],1,sum)
fungi.MM[guild.aqu>0&!is.na(guild.aqu),"Aqu"] = 1
guild.arb = apply(fungi.MM[,sprintf("GUILDE.%d",c(11,13,16:19,23))],1,sum)
fungi.MM[guild.arb>0&!is.na(guild.arb),"Arb"] = 1

# Fine tune habitat simple
hab.wat = apply(fungi.MM[,sprintf("GUILDE.%d",c(1,2,4,5,8))],1,sum)
fungi.MM[hab.wat>0 & !is.na(hab.wat),"Habitat_water"] = 1
fungi.MM[hab.wat%in%0 & !fungi.MM$Habitat_water%in%1,"Habitat_water"] = 0
hab.wet = apply(fungi.MM[,sprintf("GUILDE.%d",c(6,7,9,26))],1,sum)
fungi.MM[hab.wet>0 & !is.na(hab.wet),"Habitat_wetlands"] = 1
fungi.MM[hab.wet%in%0 & !fungi.MM$Habitat_wetlands%in%1,"Habitat_wetlands"] = 0
fungi.MM[fungi.MM[,"GUILDE.21"]>0 & !is.na(fungi.MM[,"GUILDE.21"]),"Habitat_rocks"] = 1
fungi.MM[fungi.MM[,"GUILDE.21"]%in%0 & !fungi.MM$Habitat_rocks%in%1,"Habitat_rocks"] = 0
hab.grass = apply(fungi.MM[,sprintf("GUILDE.%d",c(14,20))],1,sum)
fungi.MM[hab.grass>0 & !is.na(hab.grass),"Habitat_grassland"] = 1
fungi.MM[hab.grass%in%0 & !fungi.MM$Habitat_grassland%in%1,"Habitat_grassland"] = 0
hab.forest = apply(fungi.MM[,sprintf("GUILDE.%d",c(16,17,19))],1,sum)
fungi.MM[hab.forest>0 & !is.na(hab.forest),"Habitat_forests"] = 1
fungi.MM[hab.forest%in%0 & !fungi.MM$Habitat_forests%in%1,"Habitat_forests"] = 0
fungi.MM[fungi.MM[,"GUILDE.17"]>0 & !is.na(fungi.MM[,"GUILDE.17"]),"Habitat_deciduous"] = 1
fungi.MM[fungi.MM[,"GUILDE.17"]%in%0 & !fungi.MM$Habitat_deciduous%in%1,"Habitat_deciduous"] = 0
fungi.MM[fungi.MM[,"GUILDE.19"]>0 & !is.na(fungi.MM[,"GUILDE.19"]),"Habitat_coniferous"] = 1
fungi.MM[fungi.MM[,"GUILDE.19"]%in%0 & !fungi.MM$Habitat_coniferous%in%1,"Habitat_coniferous"] = 0
fungi.MM[fungi.MM[,"GUILDE.16"]>0 & !is.na(fungi.MM[,"GUILDE.16"]),"Habitat_mixed"] = 1
fungi.MM[fungi.MM[,"GUILDE.16"]%in%0 & !fungi.MM$Habitat_mixed%in%1,"Habitat_mixed"] = 0
hab.pioneer = apply(fungi.MM[,sprintf("GUILDE.%d",c(3,10,12,22))],1,sum)
fungi.MM[hab.pioneer>0 & !is.na(hab.pioneer),"Habitat_pioneer"] = 1
fungi.MM[hab.pioneer%in%0 & !fungi.MM$Habitat_pioneer%in%1,"Habitat_pioneer"] = 0
hab.urban = apply(fungi.MM[,sprintf("GUILDE.%d",c(22,24))],1,sum)
fungi.MM[hab.urban>0 & !is.na(hab.urban),"Habitat_urban"] = 1
fungi.MM[hab.urban%in%0 & !fungi.MM$Habitat_urban%in%1,"Habitat_urban"] = 0
fungi.MM[fungi.MM[,"GUILDE.23"]>0 & !is.na(fungi.MM[,"GUILDE.23"]),"Habitat_green_urban"] = 1
fungi.MM[fungi.MM[,"GUILDE.23"]%in%0 & !fungi.MM$Habitat_green_urban%in%1,"Habitat_green_urban"] = 0
hab.agri = apply(fungi.MM[,sprintf("GUILDE.%d",c(11,25))],1,sum)
fungi.MM[hab.agri>0 & !is.na(hab.agri),"Habitat_agricultural"] = 1
fungi.MM[hab.agri%in%0 & !fungi.MM$Habitat_agricultural%in%1,"Habitat_agricultural"] = 0
fungi.MM[fungi.MM[,"GUILDE.13"]>0 & !is.na(fungi.MM[,"GUILDE.13"]),"Habitat_bushes"] = 1
fungi.MM[fungi.MM[,"GUILDE.13"]%in%0 & !fungi.MM$Habitat_bushes%in%1,"Habitat_bushes"] = 0
fungi.MM[fungi.MM[,"GUILDE.15"]>0 & !is.na(fungi.MM[,"GUILDE.15"]),"Habitat_transitional"] = 1
fungi.MM[fungi.MM[,"GUILDE.15"]%in%0 & !fungi.MM$Habitat_transitional%in%1,"Habitat_transitional"] = 0
fungi.MM[fungi.MM[,"GUILDE.18"]>0 & !is.na(fungi.MM[,"GUILDE.18"]),"Habitat_heathland"] = 1
fungi.MM[fungi.MM[,"GUILDE.18"]%in%0 & !fungi.MM$Habitat_heathland%in%1,"Habitat_heathland"] = 0
fungi.MM[,HABit[!HABit%in%names(fungi.MM)]] = NA
fungi.MM[,names(fungi.MM)[grepl("GUILD",names(fungi.MM))]] = NULL

# Fine tune Ter/Aqu/Arb info (b)
fungi.MM[fungi.MM$Habitat_urban%in%1,"Ter"] = 1
fungi.MM[fungi.MM$Habitat_green_urban%in%1,"Ter"] = 1
fungi.MM[fungi.MM$Habitat_agriculture%in%1,"Ter"] = 1
fungi.MM[fungi.MM$Habitat_deciduous%in%1,"Arb"] = 1
fungi.MM[fungi.MM$Habitat_coniferous%in%1,"Arb"] = 1
fungi.MM[fungi.MM$Habitat_mixed%in%1,"Arb"] = 1
fungi.MM[fungi.MM$Habitat_grassland%in%1,"Ter"] = 1
fungi.MM[fungi.MM$Habitat_heathland%in%1,"Arb"] = 1
fungi.MM[fungi.MM$Habitat_bushes%in%1,"Arb"] = 1
fungi.MM[fungi.MM$Habitat_sand%in%1,"Ter"] = 1
fungi.MM[fungi.MM$Habitat_wetlands%in%1,"Aqu"] = 1
fungi.MM[fungi.MM$Habitat_water%in%1,"Aqu"] = 1
fungi.MM[fungi.MM$Habitat_rocks%in%1,"Ter"] = 1
fungi.MM[fungi.MM$Habitat_pioneer%in%1,"Ter"] = 1
l.temp = fungi.MM[,c("Ter","Aqu","Arb")]
l.temp [is.na(l.temp)] = 0
l.temp[apply(l.temp,1,function(x) all(x%in%0)),] = NA
fungi.MM[,c("Ter","Aqu","Arb")] = l.temp

# Aquatic, Terrestrial or Marine
fungi.MM$EcoTer = 1
fungi.MM$EcoFresh = 0 # See below
fungi.MM$EcoMar = NA
fungi.MM$Aer = 0 # No info
fungi.MM$Aqu = 0 # Logical
full.aqu = c("Synchytriaceae","Ancylistaceae","Tricladiaceae","Pythiaceae",
	"Halosphaeriaceae","Juncigenaceae")
semi.aqu = c("Annulatascaceae","Tubeufiaceae","Helotiaceae","Albuginaceae","Peronosporaceae",
	"Savoryellaceae","Lindgomycetaceae","Loramyceataceae","Verrucariaceae")
fungi.MM[fungi.MM$family%in%full.aqu,c("Aqu","EcoFresh")] = 1
fungi.MM[fungi.MM$family%in%full.aqu,"EcoTer"] = 0
fungi.MM[fungi.MM$family%in%semi.aqu,c("Aqu","EcoFresh")] = 1

# Fine tune Ter/Aqu info (c)
fungi.MM = fungi.MM[order(fungi.MM$species),]
mf1 = merge(fungi.MM,antoine.TerAqu,by="species",all.x=TRUE)
all(fungi.MM$species == mf1$species) # auto check
fungi.MM[mf1$scheme%in%"terrestrial"&is.na(fungi.MM$Ter),"Ter"] = 1
fungi.MM[mf1$scheme%in%"aquatic",c("Aqu","EcoFresh")] = 1
fungi.MM[fungi.MM$EcoFresh%in%1,"Aqu"] = 1

# Reorder & Save
hab.n = grep("Habitat_",names(fungi.MM))
eco.n = grep("Eco",names(fungi.MM))
col.ref = c("species","scientificName","taxonRank","gbif_status","gbif_accepted","genus","family","order",
	"class","phylum","group","Source","originCH","originEUR","swissRedListCategory","europeanRegionalRedListCategory",
	"iucnRedListCategory","EcoTer","EcoFresh","EcoMar","Fos","Ter","Aqu","Arb","Aer",
	names(fungi.MM[,hab.n]),"RangeSize_km2")
fungi.MM = fungi.MM[,c(col.ref,names(fungi.MM)[!names(fungi.MM)%in%col.ref])]
write.table(fungi.MM,"outputs/raw_traits/non_formated/s2z_raw_traits_fungi.txt",row.names=FALSE)


#############################
########## MOLLUSCS #########
#############################


# # Open the main traits sources (fresh water ecology?)
# fweG = read.csv("./gasteropods/Molluscs_freshwaterecology_R.csv")
# fweG = fweG[,!grepl("Ref",names(fweG))]
# fi.moll = as.data.frame(read_excel("./gasteropods/fauna_indicativa_Molluscs_for_R.xlsx"))
# eu.redL = read.table("../sp_IUCN_status/European_Red_list_2025_clean.txt",header=TRUE)
# moll.eu = eu.redL[grepl("Molluscs",eu.redL$speciesGroup),]
# moll.eu[,c("Ter","Aqu","Fos","Arb","Aer")] = NA
# moll.eu[grepl("Freshwater",moll.eu$speciesGroup),"EcoTer"] = 0
# moll.eu[grepl("Freshwater",moll.eu$speciesGroup),c("EcoFresh","Aqu")] = 1
# moll.eu[grepl("Terrestrial",moll.eu$speciesGroup),"EcoTer"] = 1
# moll.eu[grepl("Terrestrial",moll.eu$speciesGroup),"EcoFresh"] = 0
# moll.eu$Aer = 0

# # Freshwater ecology format
# names(fweG)[1] = "Class"
# names(fweG)[3] = "species"
# names(fweG)[4:13] = paste0("Stream_pref_",names(fweG)[4:13])
# names(fweG)[19:26] = paste0("Substrate_",names(fweG)[19:26])
# names(fweG)[23] = "Substrate_lit"
# names(fweG)[34:35] = c("AnnuMeanTemp","EIV_T_range")
# fweG$EIV_Temp = normalize(fweG$AnnuMeanTemp)
# names(fweG)[36:40] =  paste0("Salt_pref_",names(fweG)[36:40])
# names(fweG)[41:50] =  paste0("Diet_",names(fweG)[41:50])
# names(fweG)[51:56] =  paste0("Transport_",names(fweG)[51:56])
# names(fweG)[c(50,56)] = c("Diet_oth","Transport_oth")
# fweG$Geo_range = NA
# fweG[fweG$tol%in%1,"Geo_range"] = "very_wide"
# fweG[fweG$mtol%in%1,"Geo_range"] = "wide"
# fweG[fweG$msen%in%1,"Geo_range"] = "moderate"
# fweG[fweG$sen%in%1,"Geo_range"] = "small"
# fweG[fweG$hsen%in%1,"Geo_range"] = "very_small"
# fweG$Flow_habitat = NA
# fweG[fweG$lib%in%1,"Flow_habitat"] = "limnobiont"
# fweG[fweG$lip%in%1,"Flow_habitat"] = "limnophil"
# fweG[fweG$lrp%in%1,"Flow_habitat"] = "limno_to_rheophil"
# fweG[fweG$rlp%in%1,"Flow_habitat"] = "rheo_to_limnophil"
# fweG[fweG$rhp%in%1,"Flow_habitat"] = "rheophil"
# fweG[fweG$rhb%in%1,"Flow_habitat"] = "rheobiont"
# fweG[fweG$ind%in%1,"Flow_habitat"] = "indifferent"
# fweG[,c("tol","mtol","msen","sen","hsen","lib","lip","lrp","rlp","rhp","rhb","ind")] = NULL
# fweG$Fos = NA
# fweG$Ter = 0
# fweG$Aqu = 1 # ALL --> "holo" (Freshwater)
# fweG$Arb = NA
# fweG$Aer = 0
# fweG[,names(fweG)%in%c("holo","mero")] = NULL

# # Fauna indicativa format
# fi.moll$species = str_squish(paste(fi.moll$Genus,fi.moll$Species,fi.moll$Subspecies))
# fi.moll$species = gsub(" NA","",fi.moll$species)
# fi.moll[,c("Species","Subspecies","Author")] = NULL
# toBin = grepl("Shape_|Peri_|Aperture_|Shell_|Reprod_|Diet_",names(fi.moll))
# fi.moll[,toBin][fi.moll[,toBin] > 0 & !fi.moll[,toBin]=="NA"] = 1
# fi.moll[,15:18][fi.moll[,15:18]==0] = NA
# fi.moll[,15:18] = sapply(fi.moll[,15:18],as.numeric)
# 	#BL
# fi.moll$x_BodyLength_mm = apply(fi.moll[,15:18],1,mean,na.rm=TRUE)
# 	#EIV-M
# col.tar = names(fi.moll)[grepl("M_",names(fi.moll))]
# EIV_M = fi.moll[,col.tar]
# EIV_M.yes = apply(EIV_M,1,function(x) length(which(x>0)))
# EIV_M.max = apply(EIV_M,1,function(x) max(which(x>0)))^2
# EIV_M.max[EIV_M.max%in%"Inf"] = NA
# fi.moll$x_EIV_Moisture = normalize(EIV_M.max-EIV_M.yes)
# fi.moll$x_EIV_Moisture[EIV_M[,1]=="NA"] = NA
# fi.moll[,col.tar] = NULL
# 	#EIV-D
# col.tar = names(fi.moll)[grepl("D_",names(fi.moll))]
# EIV_D = fi.moll[,col.tar]
# EIV_D.yes = apply(EIV_D,1,function(x) length(which(x>0)))
# EIV_D.max = apply(EIV_D,1,function(x) max(which(x>0)))^2
# EIV_D.max[EIV_D.max%in%"Inf"] = NA
# fi.moll$x_EIV_Drought = normalize(EIV_D.max-EIV_D.yes)
# fi.moll$x_EIV_Drought[EIV_D[,1]=="NA"] = NA
# fi.moll[,col.tar] = NULL
# 	#EIV-L
# col.tar = names(fi.moll)[grepl("L_",names(fi.moll))]
# EIV_L = fi.moll[,col.tar]
# EIV_L.yes = apply(EIV_L,1,function(x) length(which(x>0)))
# EIV_L.max = apply(EIV_L,1,function(x) max(which(x>0)))^2
# EIV_L.max[EIV_L.max%in%"Inf"] = NA
# fi.moll$x_EIV_Light = normalize(EIV_L.max-EIV_L.yes)
# fi.moll$x_EIV_Light[EIV_L[,1]=="NA"] = NA
# fi.moll[,col.tar] = NULL
# 	#RM
# fi.moll[,c(15:18,26:28,48:50,52:55)] = NULL
# names(fi.moll)[c(15,22,41,42,56:58)] = c("BodyLength_mm","Whorls","Litter_size",
# 	"Longevity_y","EIV_Moisture","EIV_Drought","EIV_Light")
# 	# Ter,Fos,Aqu,Arb,Aer
# fi.moll[,c("Ter","Fos","Aqu","Arb","Aer")] = 0
# fi.moll[grepl("\\b(11|112)\\b",fi.moll$AKTIVITATSZONE),"Ter"] = 1
# fi.moll[grepl("\\b(12|121|122|123|124|212|211)\\b",fi.moll$AKTIVITATSZONE),"Fos"] = 1
# fi.moll[grepl("\\b(21|211|212|22|221|222|31|32|33)\\b",fi.moll$AKTIVITATSZONE),"Aqu"] = 1
# fi.moll[grepl("\\b(111|222)\\b",fi.moll$AKTIVITATSZONE),"Arb"] = 1

# # Merge 1
# moll.vert = merge(fi.moll,fweG,by="species",all=TRUE)
# moll.vert[is.na(moll.vert$Family.x),"Family.x"] = moll.vert[is.na(moll.vert$Family.x),"Family.y"]
# moll.vert[is.na(moll.vert$Class.x),"Class.x"] = moll.vert[is.na(moll.vert$Class.x),"Class.y"]
# moll.vert[is.na(moll.vert$Ter.x),"Ter.x"] = moll.vert[is.na(moll.vert$Ter.x),"Ter.y"]
# moll.vert[is.na(moll.vert$Aqu.x),"Aqu.x"] = moll.vert[is.na(moll.vert$Aqu.x),"Aqu.y"]
# moll.vert[is.na(moll.vert$Arb.x),"Arb.x"] = moll.vert[is.na(moll.vert$Arb.x),"Arb.y"]
# moll.vert[is.na(moll.vert$Fos.x),"Fos.x"] = moll.vert[is.na(moll.vert$Fos.x),"Fos.y"]
# moll.vert[is.na(moll.vert$Aer.x),"Aer.x"] = moll.vert[is.na(moll.vert$Aer.x),"Aer.y"]

# # Merge 2
# moll.vert2 = merge(moll.vert,moll.eu,by="species",all=TRUE)
# moll.vert2[is.na(moll.vert2$Family.x),"Family.x"] = moll.vert2[is.na(moll.vert2$Family.x),"taxonomicRankFamily"]
# moll.vert2[is.na(moll.vert2$Class.x),"Class.x"] = moll.vert2[is.na(moll.vert2$Class.x),"taxonomicRankClass"]
# moll.vert2[is.na(moll.vert2$Ter.x),"Ter.x"] = moll.vert2[is.na(moll.vert2$Ter.x),"Ter"]
# moll.vert2[is.na(moll.vert2$Aqu.x),"Aqu.x"] = moll.vert2[is.na(moll.vert2$Aqu.x),"Aqu"]
# moll.vert2[is.na(moll.vert2$Arb.x),"Arb.x"] = moll.vert2[is.na(moll.vert2$Arb.x),"Arb"]
# moll.vert2[is.na(moll.vert2$Fos.x),"Fos.x"] = moll.vert2[is.na(moll.vert2$Fos.x),"Fos"]
# moll.vert2[is.na(moll.vert2$Aer.x),"Aer.x"] = moll.vert2[is.na(moll.vert2$Aer.x),"Aer"]
# moll.vert2[is.na(moll.vert2$EcoFresh.x),"EcoFresh.x"] = moll.vert2[is.na(moll.vert2$EcoFresh.x),"EcoFresh.y"]
# moll.vert2[is.na(moll.vert2$EcoTer.x),"EcoTer.x"] = moll.vert2[is.na(moll.vert2$EcoTer.x),"EcoTer.y"]
# moll.vert2$Family.x = str_to_title(moll.vert2$Family.x)
# moll.vert2$Class.x = str_to_title(moll.vert2$Class.x)

# # Convert to numeric
# non.num = c("species","Genus","Family.x","Class.x","Litter_size","Sexes","Longevity_y","Whorls",
# 	"Flow_habitat","Geo_range")
# moll.vert2[,!names(moll.vert2)%in%non.num] = sapply(moll.vert2[,!names(moll.vert2)%in%non.num],as.numeric)
# moll.vert2 = moll.vert2[!duplicated(moll.vert2$species),]
# moll.vert2$Aer = 0

# # Solve diet classification
# moll.vert2$Diet_carniv = apply(moll.vert2[,c("Diet_carniv","Diet_alive","Diet_pre")],1,sum,na.rm=TRUE)
# moll.vert2$Diet_leaf = apply(moll.vert2[,c("Diet_min","Diet_shr")],1,sum,na.rm=TRUE)
# moll.vert2$Diet_woody = apply(moll.vert2[,c("Diet_xyl","Diet_litter")],1,sum,na.rm=TRUE)
# moll.vert2$Diet_carniv[!moll.vert2$Diet_carniv%in%0&!is.na(moll.vert2$Diet_carniv)] = 1
# moll.vert2$Diet_leaf[!moll.vert2$Diet_leaf%in%0&!is.na(moll.vert2$Diet_leaf)] = 1
# moll.vert2$Diet_woody[!moll.vert2$Diet_woody%in%0&!is.na(moll.vert2$Diet_woody)] = 1
# moll.vert2$Diet_oth[!moll.vert2$Diet_oth%in%0&!is.na(moll.vert2$Diet_oth)] = 1
# moll.vert2$Diet_gra[!moll.vert2$Diet_gra%in%0&!is.na(moll.vert2$Diet_gra)] = 1
# moll.vert2$Diet_gat[!moll.vert2$Diet_gat%in%0&!is.na(moll.vert2$Diet_gat)] = 1
# moll.vert2$Diet_aff[!moll.vert2$Diet_aff%in%0&!is.na(moll.vert2$Diet_aff)] = 1
# moll.vert2$Diet_pff[!moll.vert2$Diet_pff%in%0&!is.na(moll.vert2$Diet_pff)] = 1
# moll.vert2 = moll.vert2[, c(
#   setdiff(names(moll.vert2), grep("^Diet_", names(moll.vert2), value = TRUE)),
#   c("Diet_carniv", "Diet_scav", "Diet_fungi", "Diet_leaf", "Diet_woody","Diet_detritus","Diet_pleuston",
#   	"Diet_lichen", "Diet_epiphyte", "Diet_endolithic", "Diet_moss", "Diet_algae", "Diet_plant","Diet_par","Diet_oth"),
#   setdiff(grep("^Diet_", names(moll.vert2), value = TRUE),
#           c("Diet_carniv", "Diet_scav", "Diet_fungi", "Diet_leaf", "Diet_woody","Diet_detritus","Diet_pleuston",
#   	"Diet_lichen", "Diet_epiphyte", "Diet_endolithic", "Diet_moss", "Diet_algae", "Diet_plant","Diet_par","Diet_oth"))
# )]

# # Merge with IUCN ranges
# moll.range = iucn.ranges[grepl("MOLLUSCS",names(iucn.ranges))][[1]]
# moll.iucn = merge(moll.vert2,moll.range[,c("species","range.size.km2")],by="species",all=TRUE)
# moll.iucn$RangeSize_km2 = moll.iucn$range.size.km2

# # Change names if necessary
# tGS = to_GenusSpecies(moll.iucn$species)
# moll.iucn$species = tGS[,"CanonicalName"]
# moll.iucn = moll.iucn[!is.na(moll.iucn$species),]
# moll.iucn = moll.iucn[!duplicated(moll.iucn$species),]

# # Remove crappy columns and remove ".x"
# moll.iucn[,c("Class.y","Family.y","Ter.y","Aqu.y","Arb.y","Fos.y","Aer.y","Ter","Aqu",
# 	"Arb","Fos","Aer",names(moll.iucn)[grepl("taxonomicRank",names(moll.iucn))],
# 	"speciesGroup","endemicToEurope","endemicToEu","populationTrend","euRegionalRedListCategory",
# 	"europeanRegionalRedListCategory","Swiss_status","Diet_alive","Diet_pre","Diet_min","Diet_shr",
# 	"Diet_xyl","Diet_litter")] = NULL
# names(moll.iucn) = gsub("\\.x","",names(moll.iucn))

# # Reorder columns (possibily also change names)
# moll.vert = moll.iucn[,c("species","Genus","Family","Class","BodyLength_mm","Litter_size",
# 	"Longevity_y",names(moll.iucn)[grepl("Diet_",names(moll.iucn))],
# 	"EIV_Light","EIV_Temp","EIV_Moisture","EIV_Drought","EIV_T_range",
# 	names(moll.iucn)[grepl("Elev_",names(moll.iucn))],"Sexes",
# 	names(moll.iucn)[grepl("Reprod_",names(moll.iucn))],"Whorls",
# 	names(moll.iucn)[grepl("Shape_",names(moll.iucn))],
# 	names(moll.iucn)[grepl("Shell_cal|Shell_rud",names(moll.iucn))],
# 	names(moll.iucn)[grepl("Shell_close_",names(moll.iucn))],
# 	names(moll.iucn)[grepl("Peri_",names(moll.iucn))],
# 	names(moll.iucn)[grepl("Aperture_",names(moll.iucn))],
# 	names(moll.iucn)[grepl("Transport_",names(moll.iucn))],
# 	names(moll.iucn)[grepl("Stream_",names(moll.iucn))],
# 	"Fos","Ter","Aqu","Arb","Aer","Flow_habitat",
# 	names(moll.iucn)[grepl("Substrate_",names(moll.iucn))],
# 	names(moll.iucn)[grepl("Salt_",names(moll.iucn))],"AnnuMeanTemp","Geo_range","RangeSize_km2",
# 	"EcoFresh","EcoTer")]
# moll.vert[moll.vert == ""] = NA
# moll.vert$IUCN_status = NA

# # Homogenize Diet
# diet_names = names(moll.vert)[grepl("Diet_", names(moll.vert))]
# new_diet_names = sub("_(.)", "_\\U\\1", diet_names, perl = TRUE)
# names(moll.vert)[grepl("Diet_", names(moll.vert))] = new_diet_names
# names(moll.vert)[c(21:22)] = c("Diet_Parasit","Diet_Other")

# # Add gbif information
# moll.vert[,GB.col] = NA
# for (i in 1:nrow(moll.vert)){
#     cat("...sp.",i)
#     gbif.search = get_status(moll.vert[i,"species"]) # Search for accepted names
#     if (!is.null(gbif.search)){
#     	moll.vert$gbif_accepted[i] = gbif.search$scientificName[1] # Fill in the result
#     	moll.vert$gbif_rank[i] = gbif.search$rank[1]
#         moll.vert$gbif_phylum[i] = gbif.search$Phylum[1]
#         moll.vert$gbif_genus[i] = gbif.search$Genus[1]
#         moll.vert$gbif_order[i] = gbif.search$Order[1]
#         moll.vert$gbif_family[i] = gbif.search$Family[1]
#         moll.vert$gbif_class[i] = gbif.search$Class[1]
#         moll.vert$gbif_IUCN[i] = gbif.search$IUCN_status[1]
#     }
# }
# write.table(moll.vert,"temp_molluscs.txt",row.names=FALSE)
moll.vert = read.table("data/temp/temp_molluscs.txt",header=TRUE)

# Remove Copepods
moll.vert = moll.vert[!moll.vert$gbif_class%in%c("Cephalopoda","Copepoda"),]

# Format our table
which.gbif = which(names(moll.vert)%in%GB.col)
moll.vert = moll.vert[,c(1,which.gbif,2:(ncol(moll.vert)-length(which.gbif)))]
moll.vert$gbif_genus[is.na(moll.vert$gbif_genus)] =
	sapply(strsplit(moll.vert$species[is.na(moll.vert$gbif_genus)]," "),function(x) x[1])
moll.vert[is.na(moll.vert$gbif_family),"gbif_family"] = str_to_title(moll.vert[is.na(moll.vert$gbif_family),"Family"])
moll.vert[is.na(moll.vert$gbif_class),"gbif_class"] = str_to_title(moll.vert[is.na(moll.vert$gbif_class),"Class"])
moll.vert[is.na(moll.vert$gbif_genus),"gbif_genus"] = str_to_title(moll.vert[is.na(moll.vert$gbif_genus),"Genus"])
moll.vert$gbif_phylum = "Mollusca"
moll.vert[is.na(moll.vert$IUCN_status),"IUCN_status"] = moll.vert[is.na(moll.vert$IUCN_status),"gbif_IUCN"]
moll.vert[,c("Genus","Family","Order","Class","gbif_IUCN")] = NULL
names(moll.vert)[2:length(GB.col)] = new.col
gbif.cond = moll.vert$IUCN_status%in%c("NOT_FOUND","NOT_EVALUATED")
moll.vert[gbif.cond,"IUCN_status"] = NA

# Extract EU checklist and merge
s2z.moll = s2z.sp[s2z.sp$group%in%"Molluscs",]
moll.merge1 = merge(s2z.moll,moll.vert,by="species")
moll.vert = moll.vert[!moll.vert$species%in%moll.merge1$species,]
s2z.moll2 = s2z.moll[!s2z.moll$species%in%moll.merge1$species,]

# Regroup all the available information for the accepted species (mean when continuous values)
mam.ndup = moll.vert[!(duplicated(moll.vert$gbif_accepted)|duplicated(moll.vert$gbif_accepted,fromLast=TRUE)),]
mam.dup = moll.vert[duplicated(moll.vert$gbif_accepted)|duplicated(moll.vert$gbif_accepted,fromLast=TRUE),]
mam.homo = combine_rows(df=mam.dup,ref.col="gbif_accepted",binary=FALSE)
mam.homo = mam.homo[!is.na(mam.homo$gbif_accepted),]

# Combine, attempt another merge and rbind both results
mam.temp = rbind(mam.ndup,mam.homo)
moll.merge2 = merge(s2z.moll2,mam.temp,by="gbif_accepted",all.x=TRUE)
names(moll.merge2)[1:2] = c("gbif_accepted.x","species")
moll.merge2$gbif_accepted.y = moll.merge2$gbif_accepted.x
moll.merge2 = moll.merge2[,names(moll.merge1)]
moll.MM = rbind(moll.merge1,moll.merge2)

# Try to fill in mising information fron one column to another
moll.MM[is.na(moll.MM$genus),"genus"] = moll.MM[is.na(moll.MM$genus),"Genus"]
moll.MM[is.na(moll.MM$family),"family"] = moll.MM[is.na(moll.MM$family),"Family"]
moll.MM[is.na(moll.MM$order),"order"] = moll.MM[is.na(moll.MM$order),"Order"]
moll.MM[is.na(moll.MM$class),"class"] = moll.MM[is.na(moll.MM$class),"Class"]
moll.MM[is.na(moll.MM$phylum),"phylum"] = moll.MM[is.na(moll.MM$phylum),"Phylum"]
moll.MM[grepl("unresolved_accepted",moll.MM$gbif_accepted.x),"gbif_accepted.x"] =
	moll.MM[grepl("unresolved_accepted",moll.MM$gbif_accepted.x),"gbif_accepted.y"]
moll.MM$gbif_accepted.x[is.na(moll.MM$gbif_accepted.x)] =
	paste0("unresolved_accepted2_", seq_len(sum(is.na(moll.MM$gbif_accepted.x))))
moll.MM[is.na(moll.MM$taxonRank),"taxonRank"] = moll.MM[is.na(moll.MM$taxonRank),"gbif_rank"]
moll.MM[is.na(moll.MM$iucnRedListCategory),"iucnRedListCategory"] =
	moll.MM[is.na(moll.MM$iucnRedListCategory),"IUCN_status"]
toRemove = grepl("\\.y",names(moll.MM)) |
	names(moll.MM)%in%c("Genus","Family","Order","Class","Phylum","gbif_rank","IUCN_status")
moll.MM[,toRemove] = NULL
names(moll.MM)[1:ncol(s2z.moll2)] = names(s2z.moll2)

# Fine tune Ter/Aqu/Arb info (a)
guild.ter = apply(moll.MM[,sprintf("GUILDE.%d",c(3,10,12,14:15,20:22,24:25))],1,sum)
moll.MM[guild.ter>0&!is.na(guild.ter),"Ter"] = 1
guild.aqu = apply(moll.MM[,sprintf("GUILDE.%d",c(1,2,4:9,26))],1,sum)
moll.MM[guild.aqu>0&!is.na(guild.aqu),"Aqu"] = 1
guild.arb = apply(moll.MM[,sprintf("GUILDE.%d",c(11,13,16:19,23))],1,sum)
moll.MM[guild.arb>0&!is.na(guild.arb),"Arb"] = 1

# Fine tune habitat simple
hab.wat = apply(moll.MM[,sprintf("GUILDE.%d",c(1,2,4,5,8))],1,sum)
moll.MM[hab.wat>0 & !is.na(hab.wat),"Habitat_water"] = 1
moll.MM[hab.wat%in%0 & !moll.MM$Habitat_water%in%1,"Habitat_water"] = 0
hab.wet = apply(moll.MM[,sprintf("GUILDE.%d",c(6,7,9,26))],1,sum)
moll.MM[hab.wet>0 & !is.na(hab.wet),"Habitat_wetlands"] = 1
moll.MM[hab.wet%in%0 & !moll.MM$Habitat_wetlands%in%1,"Habitat_wetlands"] = 0
moll.MM[moll.MM[,"GUILDE.21"]>0 & !is.na(moll.MM[,"GUILDE.21"]),"Habitat_rocks"] = 1
moll.MM[moll.MM[,"GUILDE.21"]%in%0 & !moll.MM$Habitat_rocks%in%1,"Habitat_rocks"] = 0
hab.grass = apply(moll.MM[,sprintf("GUILDE.%d",c(14,20))],1,sum)
moll.MM[hab.grass>0 & !is.na(hab.grass),"Habitat_grassland"] = 1
moll.MM[hab.grass%in%0 & !moll.MM$Habitat_grassland%in%1,"Habitat_grassland"] = 0
hab.forest = apply(moll.MM[,sprintf("GUILDE.%d",c(16,17,19))],1,sum)
moll.MM[hab.forest>0 & !is.na(hab.forest),"Habitat_forests"] = 1
moll.MM[hab.forest%in%0 & !moll.MM$Habitat_forests%in%1,"Habitat_forests"] = 0
moll.MM[moll.MM[,"GUILDE.17"]>0 & !is.na(moll.MM[,"GUILDE.17"]),"Habitat_deciduous"] = 1
moll.MM[moll.MM[,"GUILDE.17"]%in%0 & !moll.MM$Habitat_deciduous%in%1,"Habitat_deciduous"] = 0
moll.MM[moll.MM[,"GUILDE.19"]>0 & !is.na(moll.MM[,"GUILDE.19"]),"Habitat_coniferous"] = 1
moll.MM[moll.MM[,"GUILDE.19"]%in%0 & !moll.MM$Habitat_coniferous%in%1,"Habitat_coniferous"] = 0
moll.MM[moll.MM[,"GUILDE.16"]>0 & !is.na(moll.MM[,"GUILDE.16"]),"Habitat_mixed"] = 1
moll.MM[moll.MM[,"GUILDE.16"]%in%0 & !moll.MM$Habitat_mixed%in%1,"Habitat_mixed"] = 0
hab.pioneer = apply(moll.MM[,sprintf("GUILDE.%d",c(3,10,12,22))],1,sum)
moll.MM[hab.pioneer>0 & !is.na(hab.pioneer),"Habitat_pioneer"] = 1
moll.MM[hab.pioneer%in%0 & !moll.MM$Habitat_pioneer%in%1,"Habitat_pioneer"] = 0
hab.urban = apply(moll.MM[,sprintf("GUILDE.%d",c(22,24))],1,sum)
moll.MM[hab.urban>0 & !is.na(hab.urban),"Habitat_urban"] = 1
moll.MM[hab.urban%in%0 & !moll.MM$Habitat_urban%in%1,"Habitat_urban"] = 0
moll.MM[moll.MM[,"GUILDE.23"]>0 & !is.na(moll.MM[,"GUILDE.23"]),"Habitat_green_urban"] = 1
moll.MM[moll.MM[,"GUILDE.23"]%in%0 & !moll.MM$Habitat_green_urban%in%1,"Habitat_green_urban"] = 0
hab.agri = apply(moll.MM[,sprintf("GUILDE.%d",c(11,25))],1,sum)
moll.MM[hab.agri>0 & !is.na(hab.agri),"Habitat_agricultural"] = 1
moll.MM[hab.agri%in%0 & !moll.MM$Habitat_agricultural%in%1,"Habitat_agricultural"] = 0
moll.MM[moll.MM[,"GUILDE.13"]>0 & !is.na(moll.MM[,"GUILDE.13"]),"Habitat_bushes"] = 1
moll.MM[moll.MM[,"GUILDE.13"]%in%0 & !moll.MM$Habitat_bushes%in%1,"Habitat_bushes"] = 0
moll.MM[moll.MM[,"GUILDE.15"]>0 & !is.na(moll.MM[,"GUILDE.15"]),"Habitat_transitional"] = 1
moll.MM[moll.MM[,"GUILDE.15"]%in%0 & !moll.MM$Habitat_transitional%in%1,"Habitat_transitional"] = 0
moll.MM[moll.MM[,"GUILDE.18"]>0 & !is.na(moll.MM[,"GUILDE.18"]),"Habitat_heathland"] = 1
moll.MM[moll.MM[,"GUILDE.18"]%in%0 & !moll.MM$Habitat_heathland%in%1,"Habitat_heathland"] = 0
moll.MM[,HABit[!HABit%in%names(moll.MM)]] = NA
moll.MM[,names(moll.MM)[grepl("GUILD",names(moll.MM))]] = NULL

# Fine tune Ter/Aqu/Arb info (b)
moll.MM[moll.MM$Habitat_urban%in%1,"Ter"] = 1
moll.MM[moll.MM$Habitat_green_urban%in%1,"Ter"] = 1
moll.MM[moll.MM$Habitat_agriculture%in%1,"Ter"] = 1
moll.MM[moll.MM$Habitat_deciduous%in%1,"Arb"] = 1
moll.MM[moll.MM$Habitat_coniferous%in%1,"Arb"] = 1
moll.MM[moll.MM$Habitat_mixed%in%1,"Arb"] = 1
moll.MM[moll.MM$Habitat_grassland%in%1,"Ter"] = 1
moll.MM[moll.MM$Habitat_heathland%in%1,"Arb"] = 1
moll.MM[moll.MM$Habitat_bushes%in%1,"Arb"] = 1
moll.MM[moll.MM$Habitat_sand%in%1,"Ter"] = 1
moll.MM[moll.MM$Habitat_wetlands%in%1,"Aqu"] = 1
moll.MM[moll.MM$Habitat_water%in%1,"Aqu"] = 1
moll.MM[moll.MM$Habitat_rocks%in%1,"Ter"] = 1
moll.MM[moll.MM$Habitat_pioneer%in%1,"Ter"] = 1
l.temp = moll.MM[,c("Ter","Aqu","Arb")]
l.temp [is.na(l.temp)] = 0
l.temp[apply(l.temp,1,function(x) all(x%in%0)),] = NA
moll.MM[,c("Ter","Aqu","Arb")] = l.temp ##### Unbalanced info between Ter/Aqu & Arb

# Aquatic, Terrestrial or Marine (EcoFresh and EcoTer already done)
moll.MM[,"EcoMar"] = NA
moll.MM$Aer = 0

# Fine tune Ter/Aqu info (c)
moll.MM = moll.MM[order(moll.MM$species),]
mf1 = merge(moll.MM,antoine.TerAqu,by="species",all.x=TRUE)
all(moll.MM$species == mf1$species) # auto check
moll.MM[mf1$scheme%in%"terrestrial"&is.na(moll.MM$Ter),"Ter"] = 1
moll.MM[mf1$scheme%in%"aquatic",c("Aqu","EcoFresh")] = 1
moll.MM[moll.MM$EcoFresh%in%1,"Aqu"] = 1

# Reorder & Save
hab.n = grep("Habitat_",names(moll.MM))
eco.n = grep("Eco",names(moll.MM))
col.ref = c("species","scientificName","taxonRank","gbif_status","gbif_accepted","genus","family","order",
	"class","phylum","group","Source","originCH","originEUR","swissRedListCategory","europeanRegionalRedListCategory",
	"iucnRedListCategory","EcoTer","EcoFresh","EcoMar","Fos","Ter","Aqu","Arb","Aer",
	names(moll.MM[,hab.n]),"RangeSize_km2")
moll.MM = moll.MM[,c(col.ref,names(moll.MM)[!names(moll.MM)%in%col.ref])]
write.table(moll.MM,"outputs/raw_traits/non_formated/s2z_raw_traits_molluscs.txt",row.names=FALSE)


#############################
########## LICHENS ##########
#############################


# # File correspondance
# cf = read.table("./fungi/LIAS_SwissLichens_match_n_missed.txt",header=TRUE)

# # Open SwissLichen and merge the file cor ()
# swissL = as.data.frame(read_excel("./fungi/20240411_LICHENS_ALL_TRAITS.xlsx"))
# names(swissL)[c(5,13,15,17,19,21,23,25)] =
# 	c("CH_status","EIV_Light","EIV_Temp","EIV_Cont","EIV_Moisture","EIV_pH","EIV_Eutroph","Substrate")
# swissL$Substrate = gsub(";.*","",swissL$Substrate)
# cf.swissL = merge(cf,swissL,by.x="SwissLichens",by.y="Taxon")
# cf.swissL$EIV_Light = normalize(cf.swissL$EIV_Light)
# cf.swissL$EIV_Temp = normalize(cf.swissL$EIV_Temp)
# cf.swissL$EIV_Moisture = normalize(cf.swissL$EIV_Moisture)
# cf.swissL$EIV_pH = normalize(cf.swissL$EIV_pH)
# cf.swissL$EIV_Eutroph = normalize(cf.swissL$EIV_Eutroph)

# # Open LIAS and format
# ly.f = "Matrix_DiversityDescriptions_LIAS_LIASlight_Switzerland_ppte_2024-07-11_publ.csv"
# lichen = read.csv(file=paste0("./fungi/",ly.f),sep="\t")
# lias.gbif = read.table("./fungi/DescriptorList_LIAS_GBIF_added.txt",header=TRUE)
# toA = to_GenusSpecies(lias.gbif[is.na(lias.gbif$canonicalName),"species"])
# lias.gbif[is.na(lias.gbif$canonicalName),"canonicalName"] = toA[,2]
# lichenG = merge(lichen,lias.gbif,by="ID")

# ## Ascoma
# lc = length(table(gsub("\\].*","",lichenG[,6])))
# NEWC = sprintf("ASCOMA_type%d",1:lc)
# lichenG[,NEWC] = 0
# for (i in 1:length(NEWC)) {lichenG[grepl(paste0("\\[0",i,"\\]"),lichenG[,6]),NEWC[i]] = 1}
# lichenG[is.na(lichenG[,6]),NEWC] = NA
# ## Substrate
# lc = length(table(gsub("\\].*","",lichenG[,7])))
# NEWC = sprintf("Substrate_type%d",1:lc)
# lichenG[,NEWC] = 0
# for (i in 1:length(NEWC)) {lichenG[grepl(paste0("\\[0",i,"\\]"),lichenG[,7]),NEWC[i]] = 1}
# lichenG[is.na(lichenG[,7]),NEWC] = NA
# ## Rhizines type
# lc = length(table(gsub("\\].*","",lichenG[,8])))
# NEWC = sprintf("THA_Rhizines_type%d",1:lc)
# lichenG[,NEWC] = 0
# for (i in 1:length(NEWC)) {lichenG[grepl(paste0("\\[0",i,"\\]"),lichenG[,8]),NEWC[i]] = 1}
# lichenG[is.na(lichenG[,8]),NEWC] = NA
# ## Transversal septa
# NEWC = c("ASCP_TransSepta_lower","ASCP_TransSepta_mean","ASCP_TransSepta_upper")
# lichenG[,NEWC] = NA
# lichenG$ASCP_TransSepta_lower = sapply(lichenG[,9],function(x) strsplit(x,", ")[[1]][2])
# lichenG$ASCP_TransSepta_mean = sapply(lichenG[,9],function(x) strsplit(x,", ")[[1]][3])
# lichenG$ASCP_TransSepta_upper = sapply(lichenG[,9],function(x) strsplit(x,", ")[[1]][4])
# ## Lower surface colour
# NEWC = c("THA_LowCol_black","THA_LowCol_brown","THA_LowCol_white","THA_LowCol_yellow","THA_LowCol_grey",
# 	"THA_LowCol_red","THA_LowCol_green","THA_LowCol_blue")
# lichenG[,NEWC] = 0
# for (i in 1:length(NEWC)) {lichenG[grepl(gsub("THA_LowCol_","",NEWC[i]),lichenG[,10]),NEWC[i]] = 1}
# lichenG[is.na(lichenG[,10]),NEWC] = NA
# ## Cilia
# lichenG$THA_Cilia = lichenG[,12]
# ## Ascopores
# lichenG[,"ASCOPORES_mean"] = NA
# lichenG$ASCOPORES_mean = sapply(lichenG[,14],function(x) strsplit(x,", ")[[1]][5])
# ## Soredia
# lichenG$THA_Soredia = lichenG[,15]
# ## Photosynthetic partners
# lichenG$PPhotobiont = lichenG[,16]
# ## Thallus growth
# lc = length(table(gsub("\\].*","",lichenG[,17])))
# NEWC = sprintf("THA_Growth%d",1:lc)
# lichenG[,NEWC] = 0
# ii = c("01","02","03","04","05","06","07","08","09","10","11","12")
# for (i in 1:length(NEWC)) {lichenG[grepl(paste0("\\[",ii[i],"\\]"),lichenG[,17]),NEWC[i]] = 1}
# lichenG[is.na(lichenG[,17]),NEWC] = NA
# ## Ascoma attachment
# lc = length(table(gsub("\\].*","",lichenG[,18])))
# NEWC = sprintf("ASCM_link%d",1:lc)
# lichenG[,NEWC] = 0
# for (i in 1:length(NEWC)) {lichenG[grepl(paste0("\\[0",i,"\\]"),lichenG[,18]),NEWC[i]] = 1}
# lichenG[is.na(lichenG[,18]),NEWC] = NA
# ## Morphological structure
# lc = length(table(gsub("\\].*","",lichenG[,19])))
# NEWC = sprintf("THA_Morpho_type%d",1:lc)
# lichenG[,NEWC] = 0
# ii = c("01","02","03","04","05","06","07","08","09","10","11","12","13","14")
# for (i in 1:length(NEWC)) {lichenG[grepl(paste0("\\[",ii[i],"\\]"),lichenG[,19]),NEWC[i]] = 1}
# lichenG[is.na(lichenG[,19]),NEWC] = NA
# ## Perispores
# lc = length(table(gsub("\\].*","",lichenG[,20])))
# NEWC = sprintf("ASCP_Perispore_type%d",1:lc)
# lichenG[,NEWC] = 0
# for (i in 1:length(NEWC)) {lichenG[grepl(paste0("\\[0",i,"\\]"),lichenG[,20]),NEWC[i]] = 1}
# lichenG[is.na(lichenG[,20]),NEWC] = NA
# ## Upper surface colour
# NEWC = c("THA_UpCol_black","THA_UpCol_brown","THA_UpCol_white","THA_UpCol_yellow","THA_UpCol_grey",
# 	"THA_UpCol_red","THA_UpCol_green","THA_UpCol_blue")
# lichenG[,NEWC] = 0
# for (i in 1:length(NEWC)) {lichenG[grepl(gsub("THA_UpCol_","",NEWC[i]),lichenG[,23]),NEWC[i]] = 1}
# lichenG[is.na(lichenG[,23]),NEWC] = NA
# ## Thallus type
# lc = length(table(gsub("\\].*","",lichenG[,26])))
# NEWC = sprintf("THALLUS_type%d",1:lc)
# lichenG[,NEWC] = 0
# for (i in 1:length(NEWC)) {lichenG[grepl(paste0("\\[0",i,"\\]"),lichenG[,26]),NEWC[i]] = 1}
# lichenG[is.na(lichenG[,26]),NEWC] = NA
# ## Photosynthetic partners
# lichenG$SPhotobiont = lichenG[,27]
# ## Habitat
# lc = length(table(gsub("\\].*","",lichenG[,29])))
# NEWC = sprintf("Life_type%d",1:lc)
# lichenG[,NEWC] = 0
# for (i in 1:length(NEWC)) {lichenG[grepl(paste0("\\[0",i,"\\]"),lichenG[,29]),NEWC[i]] = 1}
# lichenG[is.na(lichenG[,29]),NEWC] = NA
# ## Photosynthetic partners
# lichenG$THA_Rhizines = lichenG[,30]
# ## Paraphyses
# lichenG$ASCM_Paraphyses = lichenG[,31]
# ## Isidia
# lichenG$THA_Isidia = lichenG[,32]
# ## Septa type
# lc = length(table(gsub("\\].*","",lichenG[,33])))
# NEWC = sprintf("ASCP_Septa_type%d",1:lc)
# lichenG[,NEWC] = 0
# for (i in 1:length(NEWC)) {lichenG[grepl(paste0("\\[0",i,"\\]"),lichenG[,33]),NEWC[i]] = 1}
# lichenG[is.na(lichenG[,33]),NEWC] = NA
# ## Pruinosity
# lichenG$THA_Pruinosity = lichenG[,34]
# ## PPhotobiont type
# lc = length(table(gsub("\\].*","",lichenG[,35])))
# NEWC = sprintf("PPhotobiont_type%d",1:lc)
# lichenG[,NEWC] = 0
# for (i in 1:length(NEWC)) {lichenG[grepl(paste0("\\[0",i,"\\]"),lichenG[,35]),NEWC[i]] = 1}
# lichenG[is.na(lichenG[,35]),NEWC] = NA
# ## Septa
# lichenG$ASCP_Septa = NA
# lichenG[grepl("[1]",lichenG[,36]),"ASCP_Septa"] = "absent"
# lichenG[grepl("[2]",lichenG[,36]),"ASCP_Septa"] = "absent, present"
# lichenG[grepl("[3]",lichenG[,36]),"ASCP_Septa"] = "present"
# ## Pigmentation
# lc = length(table(gsub("\\].*","",lichenG[,39])))
# NEWC = sprintf("ASCP_PigmentCol%d",1:lc)
# lichenG[,NEWC] = 0
# ii = c("01","02","03","04","05","06","07","08","09","10","11","12")
# for (i in 1:length(NEWC)) {lichenG[grepl(paste0("\\[",ii[i],"\\]"),lichenG[,39]),NEWC[i]] = 1}
# lichenG[is.na(lichenG[,39]),NEWC] = NA
# ## Ascomata
# lichenG$Ascomata = lichenG[,43]

# # Format names, remove anomalies and merge LIAS and SwissFungi
# lichenG[233,"canonicalName"] = gsub(" s\\. auct\\.","",lichenG[233,"species"])
# lichenG = lichenG[!duplicated(lichenG$canonicalName),]
# lichenG.merge = merge(lichenG,cf.swissL,by.x="canonicalName",by.y="SwissLichens",all=TRUE)

# # Merge with IUCN ranges
# lichen.range = iucn.ranges[grepl("FUNGI",names(iucn.ranges))][[1]]
# lichen.iucn = merge(lichenG.merge,lichen.range[,c("species","range.size.km2")],
# 	by.x="canonicalName",by.y="species",all.x=TRUE) # .x is normal too
# lichen.iucn$RangeSize_km2 = lichen.iucn$range.size.km2

# # Change names if necessary
# tGS = to_GenusSpecies(lichen.iucn$species)
# lichen.iucn$species = tGS[,"CanonicalName"]
# lichen.iucn = lichen.iucn[!is.na(lichen.iucn$species),]

# # Add Fos/Ter (1)
# lichen.iucn[grepl("E",lichen.iucn$Substrate),"Fos"] = 1
# lichen.iucn[!grepl("E",lichen.iucn$Substrate)&!is.na(lichen.iucn$Substrate),"Fos"] = 0
# lichen.iucn[grepl("G|M",lichen.iucn$Substrate),"Ter"] = 1
# lichen.iucn[!grepl("G|M",lichen.iucn$Substrate)&!is.na(lichen.iucn$Substrate),"Ter"] = 0
# lichen.iucn$Aqu = 0
# lichen.iucn[grepl("R|H",lichen.iucn$Substrate),"Arb"] = 1
# lichen.iucn[!grepl("R|H",lichen.iucn$Substrate)&!is.na(lichen.iucn$Substrate),"Arb"] = 0
# lichen.iucn$Aer = 0

# # Add Fos/Ter (2)
# lichen.iucn[lichen.iucn$Substrate_type2%in%1,"Ter"] = 1
# lichen.iucn[lichen.iucn$Substrate_type2%in%0 & !(lichen.iucn$Ter%in%1),"Ter"] = 0
# lichen.iucn[lichen.iucn$Substrate_type3%in%1,"Ter"] = 1
# lichen.iucn[lichen.iucn$Substrate_type3%in%0 & !(lichen.iucn$Ter%in%1),"Ter"] = 0
# lichen.iucn[lichen.iucn$Substrate_type6%in%1,"Ter"] = 1
# lichen.iucn[lichen.iucn$Substrate_type6%in%0 & !(lichen.iucn$Ter%in%1),"Ter"] = 0
# lichen.iucn[lichen.iucn$Substrate_type7%in%1,"Ter"] = 1
# lichen.iucn[lichen.iucn$Substrate_type7%in%0 & !(lichen.iucn$Ter%in%1),"Ter"] = 0
# lichen.iucn[lichen.iucn$Substrate_type8%in%1,"Ter"] = 1
# lichen.iucn[lichen.iucn$Substrate_type8%in%0 & !(lichen.iucn$Ter%in%1),"Ter"] = 0
# lichen.iucn[lichen.iucn$Substrate_type9%in%1,"Ter"] = 1
# lichen.iucn[lichen.iucn$Substrate_type9%in%0 & !(lichen.iucn$Ter%in%1),"Ter"] = 0
# lichen.iucn[lichen.iucn$Substrate_type4%in%1,"Arb"] = 1
# lichen.iucn[lichen.iucn$Substrate_type4%in%0 & !(lichen.iucn$Arb%in%1),"Arb"] = 0
# lichen.iucn[lichen.iucn$Substrate_type5%in%1,"Arb"] = 1
# lichen.iucn[lichen.iucn$Substrate_type5%in%0 & !(lichen.iucn$Arb%in%1),"Arb"] = 0

# # Sort
# names(lichen.iucn)[c(1,45)] = c("species","rm")
# lichen.vert = lichen.iucn[,c("species","CH_status",
# 	names(lichen.iucn)[grepl("THALLUS",names(lichen.iucn))],
# 	names(lichen.iucn)[grepl("THA_Morpho_type",names(lichen.iucn))],
# 	names(lichen.iucn)[grepl("THA_Growth",names(lichen.iucn))],
# 	"THA_Pruinosity","THA_Cilia",
# 	names(lichen.iucn)[grepl("THA_UpCol_",names(lichen.iucn))],
# 	names(lichen.iucn)[grepl("THA_LowCol_",names(lichen.iucn))],
# 	"THA_Rhizines",
# 	names(lichen.iucn)[grepl("THA_Rhizines_type",names(lichen.iucn))],
# 	"THA_Isidia","THA_Soredia",
# 	"Ascomata",
# 	names(lichen.iucn)[grepl("ASCOMA",names(lichen.iucn))],
# 	names(lichen.iucn)[grepl("ASCM",names(lichen.iucn))],
# 	names(lichen.iucn)[grepl("ASCOPORES",names(lichen.iucn))],
# 	names(lichen.iucn)[grepl("ASCP_PigmentCol",names(lichen.iucn))],
# 	"ASCP_Septa",
# 	names(lichen.iucn)[grepl("ASCP_Septa_type",names(lichen.iucn))],
# 	names(lichen.iucn)[grepl("ASCP_TransSepta",names(lichen.iucn))],
# 	names(lichen.iucn)[grepl("ASCP_Perispore",names(lichen.iucn))],
# 	names(lichen.iucn)[grepl("Photobiont",names(lichen.iucn))],
# 	"Fos","Ter","Aqu","Arb","Aer",
# 	names(lichen.iucn)[grepl("Life_type",names(lichen.iucn))],
# 	"Substrate",
# 	names(lichen.iucn)[grepl("Substrate_type",names(lichen.iucn))],
# 	names(lichen.iucn)[grepl("EIV_",names(lichen.iucn))],
# 	"RangeSize_km2")]
# lichen.vert[lichen.vert ==""] = NA
# lichen.vert$IUCN_status = NA

# # Convert to numeric
# non.num = c("species","CH_status","THA_Pruinosity","THA_Cilia","THA_Rhizines",
# 	"THA_Isidia","THA_Soredia","Ascomata","ASCM_Paraphyses","ASCP_Septa","PPhotobiont",
# 	"SPhotobiont","Substrate","IUCN_status")
# lichen.vert[,!names(lichen.vert)%in%non.num] = sapply(lichen.vert[,!names(lichen.vert)%in%non.num],as.numeric)

# # Add gbif information
# lichen.vert[,GB.col] = NA
# for (i in 1:nrow(lichen.vert)){
#     cat("...sp.",i)
#     gbif.search = get_status(lichen.vert[i,"species"]) # Search for accepted names
#     if (!is.null(gbif.search)){
#     	lichen.vert$gbif_accepted[i] = gbif.search$scientificName[1] # Fill in the result
#     	lichen.vert$gbif_rank[i] = gbif.search$rank[1]
#         lichen.vert$gbif_phylum[i] = gbif.search$Phylum[1]
#         lichen.vert$gbif_genus[i] = gbif.search$Genus[1]
#         lichen.vert$gbif_order[i] = gbif.search$Order[1]
#         lichen.vert$gbif_family[i] = gbif.search$Family[1]
#         lichen.vert$gbif_class[i] = gbif.search$Class[1]
#         lichen.vert$gbif_IUCN[i] = gbif.search$IUCN_status[1]
#     }
# }
# write.table(lichen.vert,"temp_lichens.txt",row.names=FALSE)
lichen.vert = read.table("data/temp/temp_lichens.txt",header=TRUE)

# Format our table
which.gbif = which(names(lichen.vert)%in%GB.col)
lichen.vert = lichen.vert[,c(1,which.gbif,2:(ncol(lichen.vert)-length(which.gbif)))]
lichen.vert$gbif_genus[is.na(lichen.vert$gbif_genus)] =
	sapply(strsplit(lichen.vert$species[is.na(lichen.vert$gbif_genus)]," "),function(x) x[1])
lichen.vert[is.na(lichen.vert$IUCN_status),"IUCN_status"] = lichen.vert[is.na(lichen.vert$IUCN_status),"gbif_IUCN"]
lichen.vert[,c("Genus","Family","Order","gbif_IUCN")] = NULL
names(lichen.vert)[2:length(GB.col)] = new.col
gbif.cond = lichen.vert$IUCN_status%in%c("NOT_FOUND","NOT_EVALUATED")
lichen.vert[gbif.cond,"IUCN_status"] = NA
CH_LR = lichen.vert[,"CH_status"]
lichen.vert[,"CH_status"] = NULL
lichen.vert[,"CH_status"] = CH_LR

# Extract EU checklist and merge
s2z.lichen = s2z.sp[s2z.sp$group%in%"Lichens",]
lichen.merge1 = merge(s2z.lichen,lichen.vert,by="species")
lichen.vert = lichen.vert[!lichen.vert$species%in%lichen.merge1$species,]
s2z.lichen2 = s2z.lichen[!s2z.lichen$species%in%lichen.merge1$species,]

# Regroup all the available information for the accepted species (mean when continuous values)
mam.ndup = lichen.vert[!(duplicated(lichen.vert$gbif_accepted)|duplicated(lichen.vert$gbif_accepted,fromLast=TRUE)),]
mam.dup = lichen.vert[duplicated(lichen.vert$gbif_accepted)|duplicated(lichen.vert$gbif_accepted,fromLast=TRUE),]
mam.homo = combine_rows(df=mam.dup,ref.col="gbif_accepted",binary=FALSE)
mam.homo = mam.homo[!is.na(mam.homo$gbif_accepted),]

# Combine, attempt another merge and rbind both results
mam.temp = rbind(mam.ndup,mam.homo)
lichen.merge2 = merge(s2z.lichen2,mam.temp,by="gbif_accepted",all.x=TRUE)
names(lichen.merge2)[1:2] = c("gbif_accepted.x","species")
lichen.merge2$gbif_accepted.y = lichen.merge2$gbif_accepted.x
lichen.merge2 = lichen.merge2[,names(lichen.merge1)]
lichen.MM = rbind(lichen.merge1,lichen.merge2)

# Try to fill in mising information fron one column to another
lichen.MM[is.na(lichen.MM$genus),"genus"] = lichen.MM[is.na(lichen.MM$genus),"Genus"]
lichen.MM[is.na(lichen.MM$family),"family"] = lichen.MM[is.na(lichen.MM$family),"Family"]
lichen.MM[is.na(lichen.MM$order),"order"] = lichen.MM[is.na(lichen.MM$order),"Order"]
lichen.MM[is.na(lichen.MM$class),"class"] = lichen.MM[is.na(lichen.MM$class),"Class"]
lichen.MM[is.na(lichen.MM$phylum),"phylum"] = lichen.MM[is.na(lichen.MM$phylum),"Phylum"]
lichen.MM[grepl("unresolved_accepted",lichen.MM$gbif_accepted.x),"gbif_accepted.x"] =
	lichen.MM[grepl("unresolved_accepted",lichen.MM$gbif_accepted.x),"gbif_accepted.y"]
lichen.MM$gbif_accepted.x[is.na(lichen.MM$gbif_accepted.x)] =
	paste0("unresolved_accepted2_", seq_len(sum(is.na(lichen.MM$gbif_accepted.x))))
lichen.MM[is.na(lichen.MM$taxonRank),"taxonRank"] = lichen.MM[is.na(lichen.MM$taxonRank),"gbif_rank"]
lichen.MM[is.na(lichen.MM$iucnRedListCategory),"iucnRedListCategory"] =
	lichen.MM[is.na(lichen.MM$iucnRedListCategory),"IUCN_status"]
toRemove = grepl("\\.y",names(lichen.MM)) |
	names(lichen.MM)%in%c("Genus","Family","Order","Class","Phylum","gbif_rank","IUCN_status")
lichen.MM[,toRemove] = NULL
names(lichen.MM)[1:ncol(s2z.lichen2)] = names(s2z.lichen2)

# Fine tune Ter/Aqu/Arb info (a)
guild.ter = apply(lichen.MM[,sprintf("GUILDE.%d",c(3,10,12,14:15,20:22,24:25))],1,sum)
lichen.MM[guild.ter>0&!is.na(guild.ter),"Ter"] = 1
guild.aqu = apply(lichen.MM[,sprintf("GUILDE.%d",c(1,2,4:9,26))],1,sum)
lichen.MM[guild.aqu>0&!is.na(guild.aqu),"Aqu"] = 1
guild.arb = apply(lichen.MM[,sprintf("GUILDE.%d",c(11,13,16:19,23))],1,sum)
lichen.MM[guild.arb>0&!is.na(guild.arb),"Arb"] = 1

# Fine tune habitat simple
hab.wat = apply(lichen.MM[,sprintf("GUILDE.%d",c(1,2,4,5,8))],1,sum)
lichen.MM[hab.wat>0 & !is.na(hab.wat),"Habitat_water"] = 1
lichen.MM[hab.wat%in%0 & !lichen.MM$Habitat_water%in%1,"Habitat_water"] = 0
hab.wet = apply(lichen.MM[,sprintf("GUILDE.%d",c(6,7,9,26))],1,sum)
lichen.MM[hab.wet>0 & !is.na(hab.wet),"Habitat_wetlands"] = 1
lichen.MM[hab.wet%in%0 & !lichen.MM$Habitat_wetlands%in%1,"Habitat_wetlands"] = 0
lichen.MM[lichen.MM[,"GUILDE.21"]>0 & !is.na(lichen.MM[,"GUILDE.21"]),"Habitat_rocks"] = 1
lichen.MM[lichen.MM[,"GUILDE.21"]%in%0 & !lichen.MM$Habitat_rocks%in%1,"Habitat_rocks"] = 0
lichen.MM[lichen.MM$Substrate_type7%in%1,"Habitat_rocks"] = 1
lichen.MM[lichen.MM$Substrate_type7%in%0 & !(lichen.MM$Habitat_rocks%in%1),"Habitat_rocks"] = 0
lichen.MM[lichen.MM$Substrate_type8%in%1,"Habitat_rocks"] = 1
lichen.MM[lichen.MM$Substrate_type8%in%0 & !(lichen.MM$Habitat_rocks%in%1),"Habitat_rocks"] = 0
lichen.MM[lichen.MM$Substrate_type9%in%1,"Habitat_rocks"] = 1
lichen.MM[lichen.MM$Substrate_type9%in%0 & !(lichen.MM$Habitat_rocks%in%1),"Habitat_rocks"] = 0
hab.grass = apply(lichen.MM[,sprintf("GUILDE.%d",c(14,20))],1,sum)
lichen.MM[hab.grass>0 & !is.na(hab.grass),"Habitat_grassland"] = 1
lichen.MM[hab.grass%in%0 & !lichen.MM$Habitat_grassland%in%1,"Habitat_grassland"] = 0
hab.forest = apply(lichen.MM[,sprintf("GUILDE.%d",c(16,17,19))],1,sum)
lichen.MM[hab.forest>0 & !is.na(hab.forest),"Habitat_forests"] = 1
lichen.MM[hab.forest%in%0 & !lichen.MM$Habitat_forests%in%1,"Habitat_forests"] = 0
lichen.MM[lichen.MM[,"GUILDE.17"]>0 & !is.na(lichen.MM[,"GUILDE.17"]),"Habitat_deciduous"] = 1
lichen.MM[lichen.MM[,"GUILDE.17"]%in%0 & !lichen.MM$Habitat_deciduous%in%1,"Habitat_deciduous"] = 0
lichen.MM[lichen.MM[,"GUILDE.19"]>0 & !is.na(lichen.MM[,"GUILDE.19"]),"Habitat_coniferous"] = 1
lichen.MM[lichen.MM[,"GUILDE.19"]%in%0 & !lichen.MM$Habitat_coniferous%in%1,"Habitat_coniferous"] = 0
lichen.MM[lichen.MM[,"GUILDE.16"]>0 & !is.na(lichen.MM[,"GUILDE.16"]),"Habitat_mixed"] = 1
lichen.MM[lichen.MM[,"GUILDE.16"]%in%0 & !lichen.MM$Habitat_mixed%in%1,"Habitat_mixed"] = 0
hab.pioneer = apply(lichen.MM[,sprintf("GUILDE.%d",c(3,10,12,22))],1,sum)
lichen.MM[hab.pioneer>0 & !is.na(hab.pioneer),"Habitat_pioneer"] = 1
lichen.MM[hab.pioneer%in%0 & !lichen.MM$Habitat_pioneer%in%1,"Habitat_pioneer"] = 0
hab.urban = apply(lichen.MM[,sprintf("GUILDE.%d",c(22,24))],1,sum)
lichen.MM[hab.urban>0 & !is.na(hab.urban),"Habitat_urban"] = 1
lichen.MM[hab.urban%in%0 & !lichen.MM$Habitat_urban%in%1,"Habitat_urban"] = 0
lichen.MM[lichen.MM[,"GUILDE.23"]>0 & !is.na(lichen.MM[,"GUILDE.23"]),"Habitat_green_urban"] = 1
lichen.MM[lichen.MM[,"GUILDE.23"]%in%0 & !lichen.MM$Habitat_green_urban%in%1,"Habitat_green_urban"] = 0
hab.agri = apply(lichen.MM[,sprintf("GUILDE.%d",c(11,25))],1,sum)
lichen.MM[hab.agri>0 & !is.na(hab.agri),"Habitat_agricultural"] = 1
lichen.MM[hab.agri%in%0 & !lichen.MM$Habitat_agricultural%in%1,"Habitat_agricultural"] = 0
lichen.MM[lichen.MM[,"GUILDE.13"]>0 & !is.na(lichen.MM[,"GUILDE.13"]),"Habitat_bushes"] = 1
lichen.MM[lichen.MM[,"GUILDE.13"]%in%0 & !lichen.MM$Habitat_bushes%in%1,"Habitat_bushes"] = 0
lichen.MM[lichen.MM[,"GUILDE.15"]>0 & !is.na(lichen.MM[,"GUILDE.15"]),"Habitat_transitional"] = 1
lichen.MM[lichen.MM[,"GUILDE.15"]%in%0 & !lichen.MM$Habitat_transitional%in%1,"Habitat_transitional"] = 0
lichen.MM[lichen.MM[,"GUILDE.18"]>0 & !is.na(lichen.MM[,"GUILDE.18"]),"Habitat_heathland"] = 1
lichen.MM[lichen.MM[,"GUILDE.18"]%in%0 & !lichen.MM$Habitat_heathland%in%1,"Habitat_heathland"] = 0
lichen.MM[,HABit[!HABit%in%names(lichen.MM)]] = NA
lichen.MM[,names(lichen.MM)[grepl("GUILD",names(lichen.MM))]] = NULL

# Fine tune Ter/Aqu/Arb info (b)
lichen.MM[lichen.MM$Habitat_urban%in%1,"Ter"] = 1
lichen.MM[lichen.MM$Habitat_green_urban%in%1,"Ter"] = 1
lichen.MM[lichen.MM$Habitat_agriculture%in%1,"Ter"] = 1
lichen.MM[lichen.MM$Habitat_deciduous%in%1,"Arb"] = 1
lichen.MM[lichen.MM$Habitat_coniferous%in%1,"Arb"] = 1
lichen.MM[lichen.MM$Habitat_mixed%in%1,"Arb"] = 1
lichen.MM[lichen.MM$Habitat_grassland%in%1,"Ter"] = 1
lichen.MM[lichen.MM$Habitat_heathland%in%1,"Arb"] = 1
lichen.MM[lichen.MM$Habitat_bushes%in%1,"Arb"] = 1
lichen.MM[lichen.MM$Habitat_sand%in%1,"Ter"] = 1
lichen.MM[lichen.MM$Habitat_wetlands%in%1,"Aqu"] = 1
lichen.MM[lichen.MM$Habitat_water%in%1,"Aqu"] = 1
lichen.MM[lichen.MM$Habitat_rocks%in%1,"Ter"] = 1
lichen.MM[lichen.MM$Habitat_pioneer%in%1,"Ter"] = 1
l.temp = lichen.MM[,c("Ter","Aqu","Arb")]
l.temp [is.na(l.temp)] = 0
l.temp[apply(l.temp,1,function(x) all(x%in%0)),] = NA
lichen.MM[,c("Ter","Aqu","Arb")] = l.temp

# Aquatic, Terrestrial or Marine
lichen.MM$EcoTer = 1
lichen.MM$EcoFresh = 0 # See below
lichen.MM$EcoMar = 0
fresh = c("Aquacidia","Hydropunctaria","Hydrothyria","Gloeoheppia")
saqua = c("Lichina","Collemopsidium","Wahlenbergiella","Verrucula","Verruculopsis","Seawardiella","Bagliettoa") # same as marin
marin = c("Lichina","Collemopsidium","Wahlenbergiella","Verrucula","Verruculopsis","Seawardiella","Bagliettoa")
ver.saqua = c("Verrucaria aquatilis","Verrucaria halizoa","Verrucaria thalassina","Verrucaria trabicola","Verrucaria hydrela",
	"Verrucaria praetermissa","Verrucaria denudata","Verrucaria papillosa","Verrucaria macrostoma","Verrucaria funckii",
	"Verrucaria muralis","Verrucaria muelleriana","Verrucaria subfuscella")
ver.marin = c("Verrucaria aquatilis","Verrucaria halizoa","Verrucaria thalassina","Verrucaria trabicola","Verrucaria hydrela") # all semi-aqu
lichen.MM[lichen.MM$genus%in%fresh,c("Aqu","EcoFresh")] = 1
lichen.MM[lichen.MM$genus%in%fresh,"EcoTer"] = 0 # Assumed
lichen.MM[lichen.MM$genus%in%marin,c("Aqu","EcoMar","Habitat_water")] = 1
lichen.MM[lichen.MM$species%in%ver.marin,c("Aqu","EcoMar","Habitat_water")] = 1
lichen.MM[lichen.MM$species%in%ver.saqua,c("Aqu","EcoFresh")] = 1
lichen.MM$Aer = 0 # No info

# Fine tune Ter/Aqu info (c)
lichen.MM = lichen.MM[order(lichen.MM$species),]
mf1 = merge(lichen.MM,antoine.TerAqu,by="species",all.x=TRUE)
all(lichen.MM$species == mf1$species) # auto check
lichen.MM[mf1$scheme%in%"terrestrial"&is.na(lichen.MM$Ter),"Ter"] = 1
lichen.MM[mf1$scheme%in%"aquatic",c("Aqu","EcoFresh")] = 1
lichen.MM[lichen.MM$EcoFresh%in%1,"Aqu"] = 1

# Reorder & Save
hab.n = grep("Habitat_",names(lichen.MM))
eco.n = grep("Eco",names(lichen.MM))
col.ref = c("species","scientificName","taxonRank","gbif_status","gbif_accepted","genus","family","order",
	"class","phylum","group","Source","originCH","originEUR","swissRedListCategory","europeanRegionalRedListCategory",
	"iucnRedListCategory","EcoTer","EcoFresh","EcoMar","Fos","Ter","Aqu","Arb","Aer",
	names(lichen.MM[,hab.n]),"RangeSize_km2")
lichen.MM = lichen.MM[,c(col.ref,names(lichen.MM)[!names(lichen.MM)%in%col.ref])]
lichen.MM$EIV_Cont = NULL
write.table(lichen.MM,"outputs/raw_traits/non_formated/s2z_raw_traits_lichens.txt",row.names=FALSE)


#############################
############ EPT ############
#############################


# # Freshwater ecology
# fweG = read.csv("./insects/EPT_freshwaterecology_R.csv")
# fweG = fweG[,!grepl("Ref",names(fweG))]
# names(fweG)[1] = "Order"
# names(fweG)[3] = "species"
# fweG$Fos = NA
# fweG$Ter = 1
# fweG$Aqu = 1
# fweG$Arb = NA
# fweG$Aer = 0
# fweG[,names(fweG)%in%c("holo","mero")] = NULL

# # Merge with IUCN ranges
# ept.range = iucn.ranges[grepl("EPT",names(iucn.ranges))][[1]]
# ept.iucn = merge(fweG,ept.range[,c("species","range.size.km2")],by="species",all=TRUE)
# ept.iucn$RangeSize_km2 = ept.iucn$range.size.km2

# # Change names if necessary
# tGS = to_GenusSpecies(ept.iucn$species)
# ept.iucn$species = tGS[,"CanonicalName"]
# swiss.match = ept.iucn[!is.na(ept.iucn$species),]
# names(swiss.match)[68] = "Longevity_y"

# # Reorder columns (possibily also change names)
# ept.vert = swiss.match[,c("species","Family","Order","Reproductive_mode","Development","Longevity_y",
# 	names(swiss.match)[grepl("Diet_",names(swiss.match))],
# 	"Elev_Planar","Elev_Colline","Elev_Montane","Elev_Subalpine","Elev_Alpine","Elev_Nival",
# 	names(swiss.match)[grepl("Respir_",names(swiss.match))],
# 	"Fos","Ter","Aqu","Arb","Aer",
# 	names(swiss.match)[grepl("Aqu_stage",names(swiss.match))],
# 	names(swiss.match)[grepl("Transport_",names(swiss.match))],
# 	names(swiss.match)[grepl("Stream_",names(swiss.match))],"Flow_habitat",
# 	names(swiss.match)[grepl("Substrate_",names(swiss.match))],
# 	names(swiss.match)[grepl("Salt_pref_",names(swiss.match))],
# 	"Drought","Temp_pref","pH_pref",
# 	"Rhithral_pref","Potamal_pref",
# 	names(swiss.match)[grepl("Emergence_",names(swiss.match))],
# 	"Geo_range","RangeSize_km2")]
# ept.vert[ept.vert==""] = NA
# ept.vert$IUCN_status = NA

# # Solve diet classification
# names(ept.vert)[c(9,14:16)] = c("Diet_woody","Diet_carniv","Diet_parasit","Diet_other")
# ept.vert$Diet_leaf = apply(ept.vert[,c("Diet_min","Diet_shr")],1,sum,na.rm=TRUE)
# ept.vert$Diet_leaf[!ept.vert$Diet_leaf%in%0&!is.na(ept.vert$Diet_leaf)] = 1
# ept.vert$Diet_carniv[!ept.vert$Diet_carniv%in%0&!is.na(ept.vert$Diet_carniv)] = 1
# ept.vert$Diet_other[!ept.vert$Diet_other%in%0&!is.na(ept.vert$Diet_other)] = 1
# ept.vert$Diet_gra[!ept.vert$Diet_gra%in%0&!is.na(ept.vert$Diet_gra)] = 1
# ept.vert$Diet_gat[!ept.vert$Diet_gat%in%0&!is.na(ept.vert$Diet_gat)] = 1
# ept.vert$Diet_aff[!ept.vert$Diet_aff%in%0&!is.na(ept.vert$Diet_aff)] = 1
# ept.vert$Diet_pff[!ept.vert$Diet_pff%in%0&!is.na(ept.vert$Diet_pff)] = 1
# ept.vert = ept.vert[,c(
#   setdiff(names(ept.vert), grep("^Diet_", names(ept.vert), value = TRUE)),
#   c("Diet_carniv", "Diet_leaf", "Diet_woody","Diet_parasit","Diet_other"),
#   setdiff(grep("^Diet_", names(ept.vert), value = TRUE),
#           c("Diet_carniv", "Diet_leaf", "Diet_woody","Diet_parasit","Diet_other"))
# )]

# # Merge with IUCN ranges
# ept.range = iucn.ranges[grepl("EPT",names(iucn.ranges))][[1]]
# ept.iucn = merge(ept.vert,ept.range[,c("species","range.size.km2")],by="species",all=TRUE)
# ept.iucn$RangeSize_km2 = ept.iucn$range.size.km2

# # Change names if necessary
# tGS = to_GenusSpecies(ept.iucn$species)
# ept.iucn$species = tGS[,"CanonicalName"]
# ept.iucn = ept.iucn[!is.na(ept.iucn$species),]
# ept.iucn = ept.iucn[!duplicated(ept.iucn$species),]

# # Remove crappy columns and remove ".x"
# ept.iucn[,c("Diet_pre","Diet_min","Diet_shr","Diet_xyl")] = NULL
# names(ept.iucn) = gsub("\\.x","",names(ept.iucn))

# # Reorder columns (possibily also change names)
# ept.vert = ept.iucn[,c("species","Family","Order","Longevity_y",
# 	names(ept.iucn)[grepl("Diet_",names(ept.iucn))],
# 	"Temp_pref","Drought","pH_pref",
# 	names(ept.iucn)[grepl("Respir_",names(ept.iucn))],
# 	names(ept.iucn)[grepl("Elev_",names(ept.vert))],
# 	"Reproductive_mode","Development",
# 	names(ept.iucn)[grepl("Aqu_stage_",names(ept.iucn))],
# 	names(ept.iucn)[grepl("Transport_",names(ept.iucn))],
# 	names(ept.iucn)[grepl("Stream_pref_",names(ept.iucn))],"Rhithral_pref","Potamal_pref",
# 	"Fos","Ter","Aqu","Arb","Aer","Flow_habitat",
# 	names(ept.iucn)[grepl("Substrate_",names(ept.iucn))],
# 	names(ept.iucn)[grepl("Salt_pref_",names(ept.iucn))],"Geo_range","RangeSize_km2")]
# ept.vert[ept.vert == ""] = NA
# ept.vert$IUCN_status = NA

# # Homogenize Diet
# diet_names = names(ept.vert)[grepl("Diet_", names(ept.vert))]
# new_diet_names = sub("_(.)", "_\\U\\1", diet_names, perl = TRUE)
# names(ept.vert)[grepl("Diet_", names(ept.vert))] = new_diet_names

# # Convert to numeric
# non.num = c("species","Family","Order","Reproductive_mode","Development","Longevity_y","Flow_habitat",
# 	"Drought","Temp_pref","pH_pref","Geo_range","IUCN_status")
# ept.vert[,!names(ept.vert )%in%non.num] = sapply(ept.vert [,!names(ept.vert )%in%non.num],as.numeric)
# ept.vert = ept.vert [!duplicated(ept.vert $species),]
# ept.vert$Family = str_to_title(ept.vert$Family)

# # Add gbif information
# ept.vert[,GB.col] = NA
# for (i in 1:nrow(ept.vert)){
#     cat("...sp.",i)
#     gbif.search = get_status(ept.vert[i,"species"]) # Search for accepted names
#     if (!is.null(gbif.search)){
#     	ept.vert$gbif_accepted[i] = gbif.search$scientificName[1] # Fill in the result
#      	ept.vert$gbif_rank[i] = gbif.search$rank[1]
#         ept.vert$gbif_phylum[i] = gbif.search$Phylum[1]
#         ept.vert$gbif_genus[i] = gbif.search$Genus[1]
#         ept.vert$gbif_order[i] = gbif.search$Order[1]
#         ept.vert$gbif_family[i] = gbif.search$Family[1]
#         ept.vert$gbif_class[i] = gbif.search$Class[1]
#         ept.vert$gbif_IUCN[i] = gbif.search$IUCN_status[1]
#       }
# }
# write.table(ept.vert,"temp_ept.txt",row.names=FALSE)
ept.vert = read.table("data/temp/temp_ept.txt",header=TRUE)

# Format our table
which.gbif = which(names(ept.vert)%in%GB.col)
ept.vert = ept.vert[,c(1,which.gbif,2:(ncol(ept.vert)-length(which.gbif)))]
ept.vert$gbif_genus[is.na(ept.vert$gbif_genus)] =
	sapply(strsplit(ept.vert$species[is.na(ept.vert$gbif_genus)]," "),function(x) x[1])
ept.vert[is.na(ept.vert$gbif_family),"gbif_family"] = ept.vert[is.na(ept.vert$gbif_family),"Family"]
ept.vert[is.na(ept.vert$gbif_class),"gbif_order"] = ept.vert[is.na(ept.vert$gbif_class),"Order"]
ept.vert$Order = NULL
ept.vert$gbif_class = "Insecta"
ept.vert$gbif_phylum = "Arthropoda"
ept.vert[is.na(ept.vert$IUCN_status),"IUCN_status"] = ept.vert[is.na(ept.vert$IUCN_status),"gbif_IUCN"]
ept.vert[,c("Family","Class","gbif_IUCN")] = NULL
names(ept.vert)[2:length(GB.col)] = new.col
gbif.cond = ept.vert$IUCN_status%in%c("NOT_FOUND","NOT_EVALUATED")
ept.vert[gbif.cond,"IUCN_status"] = NA

# Extract EU checklist and merge
s2z.ept = s2z.sp[s2z.sp$group%in%"EPT",]
ept.merge1 = merge(s2z.ept,ept.vert,by="species")
ept.vert = ept.vert[!ept.vert$species%in%ept.merge1$species,]
s2z.ept2 = s2z.ept[!s2z.ept$species%in%ept.merge1$species,]

# Regroup all the available information for the accepted species (mean when continuous values)
mam.ndup = ept.vert[!(duplicated(ept.vert$gbif_accepted)|duplicated(ept.vert$gbif_accepted,fromLast=TRUE)),]
mam.dup = ept.vert[duplicated(ept.vert$gbif_accepted)|duplicated(ept.vert$gbif_accepted,fromLast=TRUE),]
mam.homo = combine_rows(df=mam.dup,ref.col="gbif_accepted",binary=FALSE)
mam.homo = mam.homo[!is.na(mam.homo$gbif_accepted),]

# Combine, attempt another merge and rbind both results
mam.temp = rbind(mam.ndup,mam.homo)
ept.merge2 = merge(s2z.ept2,mam.temp,by="gbif_accepted",all.x=TRUE)
names(ept.merge2)[1:2] = c("gbif_accepted.x","species")
ept.merge2$gbif_accepted.y = ept.merge2$gbif_accepted.x
ept.merge2 = ept.merge2[,names(ept.merge1)]
ept.MM = rbind(ept.merge1,ept.merge2)

# Try to fill in mising information fron one column to another
ept.MM[is.na(ept.MM$genus),"genus"] = ept.MM[is.na(ept.MM$genus),"Genus"]
ept.MM[is.na(ept.MM$family),"family"] = ept.MM[is.na(ept.MM$family),"Family"]
ept.MM[is.na(ept.MM$order),"order"] = ept.MM[is.na(ept.MM$order),"Order"]
ept.MM[is.na(ept.MM$class),"class"] = ept.MM[is.na(ept.MM$class),"Class"]
ept.MM[is.na(ept.MM$phylum),"phylum"] = ept.MM[is.na(ept.MM$phylum),"Phylum"]
ept.MM[grepl("unresolved_accepted",ept.MM$gbif_accepted.x),"gbif_accepted.x"] =
	ept.MM[grepl("unresolved_accepted",ept.MM$gbif_accepted.x),"gbif_accepted.y"]
ept.MM$gbif_accepted.x[is.na(ept.MM$gbif_accepted.x)] =
	paste0("unresolved_accepted2_", seq_len(sum(is.na(ept.MM$gbif_accepted.x))))
ept.MM[is.na(ept.MM$taxonRank),"taxonRank"] = ept.MM[is.na(ept.MM$taxonRank),"gbif_rank"]
ept.MM[is.na(ept.MM$iucnRedListCategory),"iucnRedListCategory"] =
	ept.MM[is.na(ept.MM$iucnRedListCategory),"IUCN_status"]
toRemove = grepl("\\.y",names(ept.MM)) |
	names(ept.MM)%in%c("Genus","Family","Order","Class","Phylum","gbif_rank","IUCN_status")
ept.MM[,toRemove] = NULL
names(ept.MM)[1:ncol(s2z.ept2)] = names(s2z.ept2)

# Fine tune Ter/Aqu/Arb info (a)
guild.ter = apply(ept.MM[,sprintf("GUILDE.%d",c(3,10,12,14:15,20:22,24:25))],1,sum)
ept.MM[guild.ter>0&!is.na(guild.ter),"Ter"] = 1
guild.aqu = apply(ept.MM[,sprintf("GUILDE.%d",c(1,2,4:9,26))],1,sum)
ept.MM[guild.aqu>0&!is.na(guild.aqu),"Aqu"] = 1
guild.arb = apply(ept.MM[,sprintf("GUILDE.%d",c(11,13,16:19,23))],1,sum)
ept.MM[guild.arb>0&!is.na(guild.arb),"Arb"] = 1

# Add missing taxo
ept.MM[ept.MM$species %in% "Baetis macani", c("phylum","class","order","family")] =
  data.frame("Arthropoda", "Insecta", "Ephemeroptera", "Baetidae")
ept.MM[ept.MM$species %in% "Holostomis phalaenoides", c("phylum","class","order","family")] =
  data.frame("Arthropoda", "Insecta", "Trichoptera", "Phryganeidae")
ept.MM[ept.MM$species %in% "Lachlania dencyanna", c("phylum","class","order","family")] =
  data.frame("Arthropoda", "Insecta", "Ephemeroptera", "Oligoneuriidae")
ept.MM[ept.MM$species %in% "Polycentropus flavomaculatus subsp. flavomaculatus", c("phylum","class","order","family")] =
  data.frame("Arthropoda", "Insecta", "Trichoptera", "Polycentropodidae")
ept.MM[ept.MM$species %in% "Ptilocolepus granulatus subsp. granulatus", c("phylum","class","order","family")] =
  data.frame("Arthropoda", "Insecta", "Trichoptera", "Hydroptilidae")
ept.MM[ept.MM$species %in% "Rhabdiopteryx neglecta subsp. neglecta", c("phylum","class","order","family")] =
  data.frame("Arthropoda", "Insecta", "Plecoptera", "Taeniopterygidae")
ept.MM[ept.MM$species %in% "Serratella spinosa subsp. nevadensis", c("phylum","class","order","family")] =
  data.frame("Arthropoda", "Insecta", "Ephemeroptera", "Ephemerellidae")
ept.MM[ept.MM$species %in% "Torleya nazarita", c("phylum","class","order","family")] =
  data.frame("Arthropoda", "Insecta", "Ephemeroptera", "Ephemerellidae")

# Fine tune habitat simple
hab.wat = apply(ept.MM[,sprintf("GUILDE.%d",c(1,2,4,5,8))],1,sum)
ept.MM[hab.wat>0 & !is.na(hab.wat),"Habitat_water"] = 1
ept.MM[hab.wat%in%0 & !ept.MM$Habitat_water%in%1,"Habitat_water"] = 0
hab.wet = apply(ept.MM[,sprintf("GUILDE.%d",c(6,7,9,26))],1,sum)
ept.MM[hab.wet>0 & !is.na(hab.wet),"Habitat_wetlands"] = 1
ept.MM[hab.wet%in%0 & !ept.MM$Habitat_wetlands%in%1,"Habitat_wetlands"] = 0
ept.MM[ept.MM[,"GUILDE.21"]>0 & !is.na(ept.MM[,"GUILDE.21"]),"Habitat_rocks"] = 1
ept.MM[ept.MM[,"GUILDE.21"]%in%0 & !ept.MM$Habitat_rocks%in%1,"Habitat_rocks"] = 0
ept.MM[ept.MM$Substrate_type7%in%1,"Habitat_rocks"] = 1
ept.MM[ept.MM$Substrate_type7%in%0 & !(ept.MM$Habitat_rocks%in%1),"Habitat_rocks"] = 0
ept.MM[ept.MM$Substrate_type8%in%1,"Habitat_rocks"] = 1
ept.MM[ept.MM$Substrate_type8%in%0 & !(ept.MM$Habitat_rocks%in%1),"Habitat_rocks"] = 0
ept.MM[ept.MM$Substrate_type9%in%1,"Habitat_rocks"] = 1
ept.MM[ept.MM$Substrate_type9%in%0 & !(ept.MM$Habitat_rocks%in%1),"Habitat_rocks"] = 0
hab.grass = apply(ept.MM[,sprintf("GUILDE.%d",c(14,20))],1,sum)
ept.MM[hab.grass>0 & !is.na(hab.grass),"Habitat_grassland"] = 1
ept.MM[hab.grass%in%0 & !ept.MM$Habitat_grassland%in%1,"Habitat_grassland"] = 0
hab.forest = apply(ept.MM[,sprintf("GUILDE.%d",c(16,17,19))],1,sum)
ept.MM[hab.forest>0 & !is.na(hab.forest),"Habitat_forests"] = 1
ept.MM[hab.forest%in%0 & !ept.MM$Habitat_forests%in%1,"Habitat_forests"] = 0
ept.MM[ept.MM[,"GUILDE.17"]>0 & !is.na(ept.MM[,"GUILDE.17"]),"Habitat_deciduous"] = 1
ept.MM[ept.MM[,"GUILDE.17"]%in%0 & !ept.MM$Habitat_deciduous%in%1,"Habitat_deciduous"] = 0
ept.MM[ept.MM[,"GUILDE.19"]>0 & !is.na(ept.MM[,"GUILDE.19"]),"Habitat_coniferous"] = 1
ept.MM[ept.MM[,"GUILDE.19"]%in%0 & !ept.MM$Habitat_coniferous%in%1,"Habitat_coniferous"] = 0
ept.MM[ept.MM[,"GUILDE.16"]>0 & !is.na(ept.MM[,"GUILDE.16"]),"Habitat_mixed"] = 1
ept.MM[ept.MM[,"GUILDE.16"]%in%0 & !ept.MM$Habitat_mixed%in%1,"Habitat_mixed"] = 0
hab.pioneer = apply(ept.MM[,sprintf("GUILDE.%d",c(3,10,12,22))],1,sum)
ept.MM[hab.pioneer>0 & !is.na(hab.pioneer),"Habitat_pioneer"] = 1
ept.MM[hab.pioneer%in%0 & !ept.MM$Habitat_pioneer%in%1,"Habitat_pioneer"] = 0
hab.urban = apply(ept.MM[,sprintf("GUILDE.%d",c(22,24))],1,sum)
ept.MM[hab.urban>0 & !is.na(hab.urban),"Habitat_urban"] = 1
ept.MM[hab.urban%in%0 & !ept.MM$Habitat_urban%in%1,"Habitat_urban"] = 0
ept.MM[ept.MM[,"GUILDE.23"]>0 & !is.na(ept.MM[,"GUILDE.23"]),"Habitat_green_urban"] = 1
ept.MM[ept.MM[,"GUILDE.23"]%in%0 & !ept.MM$Habitat_green_urban%in%1,"Habitat_green_urban"] = 0
hab.agri = apply(ept.MM[,sprintf("GUILDE.%d",c(11,25))],1,sum)
ept.MM[hab.agri>0 & !is.na(hab.agri),"Habitat_agricultural"] = 1
ept.MM[hab.agri%in%0 & !ept.MM$Habitat_agricultural%in%1,"Habitat_agricultural"] = 0
ept.MM[ept.MM[,"GUILDE.13"]>0 & !is.na(ept.MM[,"GUILDE.13"]),"Habitat_bushes"] = 1
ept.MM[ept.MM[,"GUILDE.13"]%in%0 & !ept.MM$Habitat_bushes%in%1,"Habitat_bushes"] = 0
ept.MM[ept.MM[,"GUILDE.15"]>0 & !is.na(ept.MM[,"GUILDE.15"]),"Habitat_transitional"] = 1
ept.MM[ept.MM[,"GUILDE.15"]%in%0 & !ept.MM$Habitat_transitional%in%1,"Habitat_transitional"] = 0
ept.MM[ept.MM[,"GUILDE.18"]>0 & !is.na(ept.MM[,"GUILDE.18"]),"Habitat_heathland"] = 1
ept.MM[ept.MM[,"GUILDE.18"]%in%0 & !ept.MM$Habitat_heathland%in%1,"Habitat_heathland"] = 0
ept.MM[,HABit[!HABit%in%names(ept.MM)]] = NA
ept.MM[,names(ept.MM)[grepl("GUILD",names(ept.MM))]] = NULL

# Fine tune Ter/Aqu/Arb info (b)
ept.MM[ept.MM$Habitat_urban%in%1,"Ter"] = 1
ept.MM[ept.MM$Habitat_green_urban%in%1,"Ter"] = 1
ept.MM[ept.MM$Habitat_agriculture%in%1,"Ter"] = 1
ept.MM[ept.MM$Habitat_deciduous%in%1,"Arb"] = 1
ept.MM[ept.MM$Habitat_coniferous%in%1,"Arb"] = 1
ept.MM[ept.MM$Habitat_mixed%in%1,"Arb"] = 1
ept.MM[ept.MM$Habitat_grassland%in%1,"Ter"] = 1
ept.MM[ept.MM$Habitat_heathland%in%1,"Arb"] = 1
ept.MM[ept.MM$Habitat_bushes%in%1,"Arb"] = 1
ept.MM[ept.MM$Habitat_sand%in%1,"Ter"] = 1
ept.MM[ept.MM$Habitat_wetlands%in%1,"Aqu"] = 1
ept.MM[ept.MM$Habitat_water%in%1,"Aqu"] = 1
ept.MM[ept.MM$Habitat_rocks%in%1,"Ter"] = 1
ept.MM[ept.MM$Habitat_pioneer%in%1,"Ter"] = 1
l.temp = ept.MM[,c("Ter","Aqu","Arb")]
l.temp [is.na(l.temp)] = 0
l.temp[apply(l.temp,1,function(x) all(x%in%0)),] = NA
ept.MM[,c("Ter","Aqu","Arb")] = l.temp

# Aquatic, Terrestrial or Marine
ept.MM$EcoTer = 1
ept.MM$EcoFresh = 1
ept.MM$EcoMar = 0
ept.MM$Aer = NA

# Fine tune Ter/Aqu info (c)
ept.MM = ept.MM[order(ept.MM$species),]
mf1 = merge(ept.MM,antoine.TerAqu,by="species",all.x=TRUE)
all(ept.MM$species == mf1$species) # auto check
ept.MM[mf1$scheme%in%"terrestrial"&is.na(ept.MM$Ter),"Ter"] = 1
ept.MM[mf1$scheme%in%"aquatic",c("Aqu","EcoFresh")] = 1
ept.MM[ept.MM$EcoFresh%in%1,"Aqu"] = 1

# Reorder & Save
hab.n = grep("Habitat_",names(ept.MM))
eco.n = grep("Eco",names(ept.MM))
col.ref = c("species","scientificName","taxonRank","gbif_status","gbif_accepted","genus","family","order",
	"class","phylum","group","Source","originCH","originEUR","swissRedListCategory","europeanRegionalRedListCategory",
	"iucnRedListCategory","EcoTer","EcoFresh","EcoMar","Fos","Ter","Aqu","Arb","Aer",
	names(ept.MM[,hab.n]),"RangeSize_km2")
ept.MM = ept.MM[,c(col.ref,names(ept.MM)[!names(ept.MM)%in%col.ref])]
ept.MM$EIV_Cont = NULL
write.table(ept.MM,"outputs/raw_traits/non_formated/s2z_raw_traits_ept.txt",row.names=FALSE)