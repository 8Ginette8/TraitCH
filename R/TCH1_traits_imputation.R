# ###################################################################################
# TraitCH: Impute species traits
#
# $Date: 2025-12-02
#
# Author: Yohann Chauvier, yohann.chauvier@wsl.ch
# Aquatic Ecology Group
# Swiss Federal Research Institute EAWAG
# 
# Description: The raw traits have been compiled but we have missing data.
# We want here to fill the gaps of our trait datasets. Point would be
# to keep as many species as posssible, so let's not use phylogenetic trees to 
# to capture phylogenetic signals but rather Genus/Family/Order/Phylum (also,
# many taxa so phylogenetic trees remain tedious for full completness). We also
# want here to be able to assess how well imputations worked for each column of
# continuous or categorical values.
#
# ###################################################################################

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
library(data.table)

# Traits files (open)
list.f = list.files("data/raw_traits")
csv.f = lapply(list.f,function(x) read.table(paste0("data/raw_traits/",x),header=TRUE))


### ==================================================================
### Imputation process using the taxonomy + other traits
### ==================================================================


# Columns we don't want for imputation
ex.col = c("Species",
	"scientificName",
	"taxonRank",
	"GBIF_status",
	"Group", # redundant
	"Source",
	"originCH",
	"originEUR",
	"swissRedListCategory",
	"europeanRegionalRedListCategory",
	"iucnRedListCategory",
	"MISSING_ALL_TRAITS")
iin.col = c("GBIF_accepted","Genus","Family","Order","Class","Phylum")

# Seeds
reP = 25 # repetitions

# Start loop over files
for (i in 1:length(list.f))
{
	cat("#### File",i,"...","\n")

	# Do it or not
	f.output = list.files("data/missRanger_imputed_traits/")
	c0 = f.output[grepl("S_MEAN_",f.output)]
	if (any(grepl(list.f[i],c0))) {next}

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

	# Set type
	m.type = sapply(to.imp[-c(1:length(iin.col))],class)
	m.type[!m.type%in%"factor"] = "regression"
	m.type[m.type%in%"factor"] = "classification"

	# Templates
	j.imput = data.frame(to.keep,to.imp)[,names(csv.f[[i]])]
	j.imput[,!names(j.imput)%in%c(ex.col,iin.col)] = NA
	j.l.imput = replicate(reP+1,j.imput,simplify=FALSE)

	# Run model with all data (*20 imputation runs)
	w.one = apply(to.imp,2,function(x) length(which(!is.na(x)))==1)
	keep.temp = to.imp[,w.one,drop=FALSE]
	to.imp[,w.one] = NULL
	f.imput = lapply(seq(reP),function(x) {
		missRanger(to.imp,pmm.k=3,num.trees=100,maxiter=20,seed=seq(reP)[x],data_only=FALSE,verbose=1)
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
				metric.df[metric.df$Traits%in%trt,k+1] = mean(f.imput[[k]]$pred_errors[,trt],na.rm=TRUE)
			}
		}

		# Average the evaluations
		metric.df[metric.df$Traits%in%trt,(reP+2)] = mean(unlist(metric.df[metric.df$Traits%in%trt,2:(reP+1)]),na.rm=TRUE)

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
			paste0("data/missRanger_imputed_traits/",rep.out[x],list.f[i]),row.names=FALSE)
	})

	# Save evaluation data frame
	write.table(metric.df,
		paste0("data/missRanger_evaluations/temp/S_EVAL_",list.f[i]),row.names=FALSE)
}


### ==================================================================
### Remove traits that are not valid for taxa other than bees
### ==================================================================


# Basically we need to remove traits related to Tongue and Lecty
bw.o = list.files("data/missRanger_imputed_traits/",full.name=TRUE)
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