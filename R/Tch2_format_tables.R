# ####################################################################
# TraitsCH: Keep only relevant data for the article
#
# $Date: 2024-12-11
#
# Author: Yohann Chauvier, yohann.chauvier@wsl.ch
# Aquatic Ecology Group
# Swiss Federal Research Institute EAWAG
# 
# Description: Basically, we want:
# 1) To flag species with no trait data at all
# 2) Add to the evaluation files how much percent per traits is complete
# 3) to save this in the TraitCH clean folder
#
# ####################################################################

### ==================================================================
### Initialise system
### ==================================================================


# Cleaning
rm(list = ls()); graphics.off()

# max.print
options(max.print=500)


### ==================================================================
### Pre-open and format all target files
### ==================================================================


# Open raw traits
path.raw = "outputs/raw_traits/accepted_optimize/"
lfr = list.files(path.raw)
raw.t = lapply(lfr,function(x) {
     oo = read.table(paste0(path.raw,x),header=TRUE)
     oo$MISSING_ALL_TRAITS = "NO"
     out = oo[order(oo$Species),]
     return(out)
})
names(raw.t) = lfr

# Open imputed traits
path.impu = "outputs/missRanger_imputed_traits/non_formated/"
lfi = list.files(path.impu)
imp.t =
lapply(lfi,function(x){
     oimp = read.table(paste0(path.impu,x),header=TRUE)
     oimp$MISSING_ALL_TRAITS = "NO"
     oimp = oimp[order(oimp$Species),]
     return(oimp)
})
names(imp.t) = lfi

# Open evalluation
path.eval = "outputs/missRanger_evaluations/non_formated/"
lfi = list.files(path.eval)
eval.t =
lapply(lfi,function(x){
     oimp = read.table(paste0(path.eval,x),header=TRUE)
     return(oimp)
})
names(eval.t) = lfi


### ==================================================================
### Process
### ==================================================================


# Info
true.traits = length(c("Species","scientificName","taxonRank","GBIF_status",
     "GBIF_accepted","Genus","Family","Order","Class","Phylum","Group","Source",
     "originCH","originEUR","swissRedListCategory","europeanRegionalRedListCategory",
     "iucnRedListCategory","EcoTer","EcoFresh","EcoMar","Fos","Ter","Aqu","Arb","Aer"))

# Loop over the taxa groups
for (i in 1:length(eval.t))
{
     # Traits (where) + target imputed data
     trait.col = (true.traits+1):(ncol(raw.t[[i]])-1) # We don't account for Eco*etc and Fos*etc
     tar.imput = imp.t[grepl(names(raw.t[i]),names(imp.t))]

     # Task (1) for raw and imputed files
          #
     no.dat = apply(raw.t[[i]][,trait.col],1,function(x) all(is.na(x)))
     raw.t[[i]]$MISSING_ALL_TRAITS[no.dat] = "YES"
          #
     print(all(raw.t[[i]]$Species == tar.imput[[11]]$Species)) # Quick check
     for (j in 1:length(tar.imput)) {tar.imput[[j]]$MISSING_ALL_TRAITS[no.dat] = "YES"}

     # Task (2)
     for.raw.stat = raw.t[[i]][raw.t[[i]]$MISSING_ALL_TRAITS%in%"NO",]
     comp.stat = apply(for.raw.stat,2,function(x) length(x[!is.na(x)])/length(x)*100)
     tt = merge(eval.t[[i]],comp.stat,by.x="Traits",by.y="row.names",all.x=TRUE,sort=FALSE)
     names(tt)[ncol(tt)] = "data_completeness_prct"

     # Homogenize names
     names(raw.t[[i]]) = names(tar.imput[[1]])

     # Task (3)
          #
     write.table(raw.t[[i]],
          paste0("outputs/raw_traits/",names(raw.t)[i]),
          row.names=FALSE)
          #
     write.table(tt,
          paste0("outputs/missRanger_evaluations/",names(eval.t)[i]),
          row.names=FALSE)
          #
     tar.rep = tar.imput[grepl("IMP",names(tar.imput))]
     lapply(1:length(tar.rep),function(x) {
          write.table(tar.rep[[x]],
               paste0("outputs/missRanger_imputed_traits/model_rep/",names(tar.rep)[x]),
               row.names=FALSE)
     })
          #
     tar.mean = tar.imput[grepl("MEAN",names(tar.imput))]
     write.table(tar.mean[[1]],
               paste0("outputs/missRanger_imputed_traits/",names(tar.mean)),
               row.names=FALSE)
}
