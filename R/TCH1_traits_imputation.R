# ####################################################################
# TraitCH: Impute species traits
#
# $Date: 2024-05-29
#
# Author: Yohann Chauvier, yohann.chauvier@wsl.ch
# Aquatic Ecology Group
# Swiss Federal Research Institute EAWAG
# 
# Description: We want here to fill the gaps of our trait datasets
# with missRanger. The idea is to use additional phylogenetic
# information. We cannot use phylogenetic trees to capture potential
# phylogenetic signals or co-evolution, we have too many groups. We
# need a straighforward methods that gives equal qualitative information
# for all groups, i.e., using all available taxonomic ranking. We also
# want here to be able to assess how well imputations worked for each
# column of continuous or categorical values (missRanger metrics).
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
library(missRanger)
library(stringr)
library(foreach)
library(data.table)

# Traits files (open)
list.f = list.files("outputs/raw_traits/non_formated")
csv.f = lapply(list.f,function(x)
	read.table(paste0("outputs/raw_traits/non_formated/",x),header=TRUE))

# Change all column names to Title
for (i in 1:length(csv.f)) {
	nn = c("group","species","genus","family","order","class","phylum")
	names(csv.f[[i]])[names(csv.f[[i]])%in%nn] = str_to_title(names(csv.f[[i]])[names(csv.f[[i]])%in%nn])
	str.tar = c("gbif_accepted","gbif_status")
	names(csv.f[[i]])[names(csv.f[[i]])%in%str.tar] = c("GBIF_status","GBIF_accepted")
}


### ==================================================================
### Extent traits values across accepted names (Mean, 0->1 and Mode)
### ==================================================================

######
### Here everthing is commented-out since a bit long to run
######

# # Loop over
# for (i in 1:length(csv.f)) {
#   tar <- data.table(csv.f[[i]])
#   tar[, ROW_ORIG_ID := .I]

#   start_col <- which(colnames(tar) == "originCH")
#   gbif.n <- unique(tar$GBIF_accepted)

#   # Convert factor to character and integer to numeric upfront (without get())
#   for (col in start_col:ncol(tar)) {
#     if (is.factor(tar[[col]])) {
#       tar[[col]] <- as.character(tar[[col]])
#     }
#     if (is.integer(tar[[col]])) {
#       tar[[col]] <- as.numeric(tar[[col]])
#     }
#   }

#   cols_to_fill <- colnames(tar)[start_col:ncol(tar)]

#   for (col in cols_to_fill) {
#     tar[, (col) := {
#       cur_col <- .SD[[1]]
#       if (any(is.na(cur_col)) & !all(is.na(cur_col))) {
#         non_na_vals <- cur_col[!is.na(cur_col)]
#         non_na_vals_lower <- tolower(as.character(non_na_vals))

#         fill_val <- if (all(non_na_vals_lower %in% c("0", "1", "yes", "no", "true", "false"))) {
#           if (any(non_na_vals_lower %in% c("1", "yes", "true"))) {
#             if (is.character(cur_col)) "Yes" else 1
#           } else {
#             if (is.character(cur_col)) "No" else 0
#           }
#         } else if (is.numeric(non_na_vals)) {
#           mean(as.numeric(non_na_vals), na.rm = TRUE)
#         } else {
#           names(sort(table(non_na_vals), decreasing = TRUE))[1]
#         }

#         ifelse(is.na(cur_col), fill_val, cur_col)
#       } else {
#         cur_col
#       }
#     }, by = "GBIF_accepted", .SDcols = col]
#   }

#   # Reorder and make sure that GBIF genus is set
#   setorder(tar, ROW_ORIG_ID)
#   tar[, ROW_ORIG_ID := NULL]
#   n.genus = sapply(strsplit(tar$GBIF_accepted," "),function(x) x[[1]][1])
#   n.genus[grepl("unresolved_",n.genus)] = NA
#   tar$Genus[!is.na(n.genus)] = n.genus[!is.na(n.genus)]

#   # Save
#   write.table(tar, file = paste0("./x_compiled/accepted_optimize/", list.f[i]),row.names=FALSE)
# }

######
### Open outputs
######

csv.f = lapply(1:length(list.f),function(x)
			read.table(paste0("outputs/raw_traits/accepted_optimize/",list.f[x]),header=TRUE))


### ==================================================================
### Imputation process using the taxonomy + other traits
### ==================================================================


# Columns we don't want for imputation
ex.col = c("Species","Source","Group","taxonRank","GBIF_status","scientificName","originCH","originEUR",
	"swissRedListCategory","europeanRegionalRedListCategory","iucnRedListCategory")
iin.col = c("GBIF_accepted","Genus","Family","Order","Class","Phylum")

# Seeds
reP = 25
ss = 1:reP

# Start loop over files
for (i in 1:length(list.f))
{
	cat("#### File",i,"...","\n")

	# Select target columns and convert to factor if needed
	to.imp = csv.f[[i]][,!names(csv.f[[i]])%in%ex.col]
	to.keep = csv.f[[i]][,names(csv.f[[i]])%in%ex.col]
	cat.cols = sapply(to.imp, function(x) is.character(x) | all(na.omit(x)%in%c(0,1)))
	to.imp[cat.cols] = lapply(to.imp[cat.cols], as.factor)

	# Empty evaluation outputs data.frame
	metric.df = as.data.frame(matrix(NA,ncol(to.imp)-length(iin.col),reP+2))
	metric.df[,1] = names(to.imp)[-c(1:length(iin.col))]
	names(metric.df) = c("Traits",sprintf("OBB_pred_error.%d",1:(reP+1)))
	names(metric.df)[grepl(paste0("\\.",reP+1),names(metric.df))] =
		gsub(paste0("\\.",reP+1),"",names(metric.df))[grepl(paste0("\\.",reP+1),names(metric.df))]

	# Set type of metrica-evaluation
	m.type = sapply(to.imp[-c(1:length(iin.col))],class)
	m.type[!m.type%in%"factor"] = "regression"
	m.type[m.type%in%"factor"] = "classification"

	# Loop over traits (CV, calibration, evaluation)
	j.imput = data.frame(to.keep,to.imp)[,names(csv.f[[i]])]
	j.imput[,!names(j.imput)%in%c(ex.col,iin.col)] = NA
	j.l.imput = replicate(reP+1,j.imput,simplify=FALSE)

	# Run model with all data (*20 imputation runs)
	w.one = apply(to.imp,2,function(x) length(which(!is.na(x)))==1)
	keep.temp = to.imp[,w.one,drop=FALSE]
	to.imp[,w.one] = NULL
	f.imput = lapply(1:length(ss),function(x) {
		missRanger(
			to.imp,
			pmm.k=3,
			num.trees=100,
			maxiter=20,
			seed=ss[x],
			data_only=FALSE,
			verbose=1
		)
	})

	# Fill empty templates
	for (j in 1:nrow(metric.df))
	{
		cat("------>Trait",j,"...","\n")

		# Extract target vector and set seed
		trt = metric.df$Traits[j]

		# Fill in raw results
		for (k in 1:length(f.imput)) {

			# Rebind the columns having only one trait
			f.imput[[k]]$data = cbind(f.imput[[k]]$data,keep.temp)

			# Imputed traits
			j.l.imput[[k]][,trt] = f.imput[[k]]$data[,trt]

			# Evaluations
			Tin = trt%in%colnames(f.imput[[k]]$pred_errors)
			if (Tin){
				metric.df[metric.df$Traits%in%trt,k+1] =
					mean(f.imput[[k]]$pred_errors[,trt],na.rm=TRUE)
			}
		}

		# Average the evaluations
		metric.df[metric.df$Traits%in%trt,(reP+2)] =
			mean(unlist(metric.df[metric.df$Traits%in%trt,2:(reP+1)]),na.rm=TRUE)

		# Average the results (last column)
		if (m.type[j]%in%"classification") {
			cat.mat = lapply(f.imput,function(x) x$data[,trt,drop=FALSE])
			j.l.imput[[reP+1]][,trt] = apply(do.call("cbind",cat.mat),1,Mode)
		} else {
			dd2 = do.call("rbind",lapply(f.imput,function(x) x$data[,trt]))
			j.l.imput[[reP+1]][,trt] = apply(dd2,2,mean,na.rm=TRUE)
		}
	}

	# Save imputed data frames
	rep.out = c(sprintf("S_IMP%d_",1:reP),"S_MEAN_")
	lapply(1:length(j.l.imput),function(x){
		write.table(j.l.imput[x],
			paste0("outputs/missRanger_imputed_traits/non_formated/",rep.out[x],list.f[i]),
			row.names=FALSE)
	})

	# Save evaluation data frame
	write.table(metric.df,
		paste0("outputs/missRanger_evaluations/non_formated/S_EVAL_",list.f[i]),
		row.names=FALSE)
}


### ==================================================================
### Remove traits that are not valid for taxa other than wasps
### ==================================================================


# Basically we need to remove traits related to Tongue and Lecty
bw.o = list.files("outputs/missRanger_imputed_traits",full.name=TRUE)
bw.o = bw.o[grepl("S_IMP|S_MEAN",bw.o)]
bw.o = bw.o[grepl("bees_wasps",bw.o)]

# Go
for (i in 1:length(bw.o))
{
	bo = read.table(bw.o[i],header=TRUE)
	bo[grepl("Wasps",bo$Group),c("Tongue_guild","Tongue_length_mm",
		"Glossa_length_mm","Prementum_length_mm","Lecty")] = NA
	write.table(bo,bw.o[i],row.names=FALSE)
}