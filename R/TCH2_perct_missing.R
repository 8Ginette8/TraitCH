# ###################################################################################
# TraitsCH: Additional information added
#
# $Date: 2025-12-05
#
# Author: Yohann Chauvier, yohann.chauvier@wsl.ch
# Aquatic Ecology Group
# Swiss Federal Research Institute EAWAG
# 
# Description: To add to the evaluation files how much percent per traits is complete
#
# ###################################################################################

### ==================================================================
### Initialise system
### ==================================================================


# Cleaning
rm(list = ls()); graphics.off()

# max.print
options(max.print=500)


### ==================================================================
### Open raw traits and evaluation files
### ==================================================================


# Raw traits
rf = list.files("data/raw_traits")
raw.t = lapply(rf,function(x) {
     read.table(paste0("data/raw_traits/",x),header=TRUE)   
})

# Evaluation
ef = list.files("data/missRanger_evaluations/temp")
eval.t = lapply(ef,function(x) {
     read.table(paste0("data/missRanger_evaluations/temp/",x),header=TRUE)   
})


### ==================================================================
### Process
### ==================================================================


# Loop over the taxa groups
for (i in 1:length(eval.t))
{
     # Process
     for.raw.stat = raw.t[[i]][raw.t[[i]]$MISSING_ALL_TRAITS%in%"NO",]
     comp.stat = apply(for.raw.stat,2,function(x) length(x[!is.na(x)])/length(x)*100)
     tt = merge(eval.t[[i]],comp.stat,by.x="Traits",by.y="row.names",all.x=TRUE,sort=FALSE)
     names(tt)[ncol(tt)] = "data_completeness_prct"

     # Save
     write.table(x = tt,
          file = paste0("data/missRanger_evaluations/",ef[i]),
          row.names = FALSE)
}
