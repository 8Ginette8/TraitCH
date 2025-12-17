# ####################################################################
# TraitCH: Extract IUCN species range areas
#
# $Date: 2025-02-01
#
# Author: Yohann Chauvier, yohann.chauvier@wsl.ch
# Aquatic Ecology Group
# Swiss Federal Research Institute EAWAG
# 
# Description: Script to generate the IUCN maps in figure 2.
# Cannot be run due to copyright restriction. See output folder
# 'data\x_50km_geo_ext' for mapping results. 
# 
# ####################################################################
### ==================================================================
### Initialise system
### ==================================================================


# # Cleaning
# rm(list = ls()); graphics.off()

# # max.print
# options(max.print=500)

# # Library
# library(terra)


# ### ==================================================================
# ### Rrelimanry setps
# ### ==================================================================


# # List of target species
# s2z.p = list.files("../data/sp_traits/x_compiled",full.names=TRUE)
# s2z.p = s2z.p[grepl("s2z_raw",s2z.p)]
# s2z.f = lapply(s2z.p,function(x) read.table(x,header=TRUE)[,"Species"])
# names(s2z.f) = c("AMPHIBIANS","AVES","HYMENOPTERA","PLANTS","COLEOPTERA","EPT3","FISH","FUNGI",
# 	"LEPIDOPTERA","FUNGI","MAMMALS","MOLLUSCS","ODONATA","ORTHOPTERA","PLANTS","REPTILES","SPIDERS")
# s2z.out = gsub("../data/sp_traits/x_compiled/s2z_raw_traits_|.txt","",s2z.p)

# # List of folders
# folders = list.files()[!grepl("_km2",list.files())]
# folders = folders[!grepl("x_scratch|x_50km_geo_ext|x_range_area_km2|.pdf",folders)]

# # Key CRS
# robin = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs"
# wgs84.rast = rast(resolution=c(0.5,0.5))


# ### ==================================================================
# ### Create the trait distribution maps per taxa group
# ### ==================================================================


# # Keep only the ones left to be done
# r.done = gsub("s2z_taxa_ext_|.tif","",list.files("x_50km_geo_ext/"))

# # Loop over
# for (i in 1:length(s2z.f))
# {
# 	cat("\n",s2z.p[i],"\n")

# 	# Next if already done
# 	if (s2z.out[i]%in%r.done) {next}

# 	# Grep the groups we want
# 	group.tar = folders[grepl(names(s2z.f)[i],folders)]
# 	if (identical(group.tar,character(0))) {next}

# 	df.out = list()
# 	n.sp1 = NULL
# 	for (z in 1:length(group.tar))
# 	{
# 		# Group
# 		ggrep = group.tar[z]

# 		# Grep the shapefiles we want
# 		all.files = list.files(ggrep)
# 		all.files = all.files[grepl(".shp|.gpkg",all.files)&!grepl(".xml|pdf",all.files)]

# 		# Second loop over shapefiles
# 		df.out2 = list()
# 		n.sp2 = NULL
# 		for (j in 1:length(all.files))
# 		{
# 			cat("\n","...PART",j,"\n")

# 			# Open and unique species
# 			iucn.poly = vect(paste0("./",ggrep,"/",all.files[j]))
# 			names(iucn.poly) = tolower(names(iucn.poly))

# 			# Exclude regions where species is quite unlikely to occur anymore
# 			iucn.poly = iucn.poly[!iucn.poly$presence%in%c(4,5,6,7),] # We remove all uncertain & extinct related

# 			# Extract s2z species
# 			iucn.s2z = iucn.poly[iucn.poly$sci_name%in%s2z.f[[i]],]
# 			iucn.s2z$presence = 1
# 			if (length(iucn.s2z)==0) {next}

# 			# Unique species
# 			iucn.sp = unique(iucn.s2z$sci_name)

# 			# Get distribution for each species
# 			iucn.range =
# 			lapply(1:length(iucn.sp),function(x){

# 				cat("\r","...",round((x*100)/length(iucn.sp),2),"%","...",sep="")

# 				pol.list = iucn.s2z[iucn.s2z$sci_name%in%iucn.sp[x],]
# 				range.sp = aggregate(pol.list)
# 				range.sp = rasterize(range.sp,wgs84.rast,background=0)
# 				return(range.sp)
# 			})

# 			# Rasterize to world extent (50 x 50 km)
# 			df.out2[[j]] = sum(rast(iucn.range),na.rm=TRUE)
# 			n.sp2[j] = length(iucn.range)
# 		}

# 		# Sum if parts, and save as TIFF
# 		if (!length(df.out2)==0){
# 			df.out[[z]] = sum(rast(df.out2))
# 			n.sp1[z] = sum(n.sp2,na.rm=TRUE)
# 		}
# 	}

# 	# Sum if parts, and save as TIFF
# 	if (!length(df.out)==0){
# 		ras.ff = sum(rast(df.out))
# 		ras.ff = project(ras.ff,robin)
# 		names(ras.ff) = paste0(s2z.out[i]," (n=",sum(n.sp1,na.rm=TRUE),")")
# 		writeRaster(ras.ff,paste0("./x_50km_geo_ext/xs2z_taxa_ext_",s2z.out[i],".tif"),overwrite=TRUE)
# 	}
# }