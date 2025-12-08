# ###################################################################################
# TraitCH: Plot IUCN maps
#
# $Date: 2025-04-20
#
# Author: Yohann Chauvier, yohann.chauvier@wsl.ch
# Aquatic Ecology Group
# Swiss Federal Research Institute EAWAG
# 
# Description: Create new rasters representing the main 8 groups and plot this on
# a same figure with the custom legend
# 
# ###################################################################################

### ==================================================================
### Initialise system
### ==================================================================


# Cleaning
rm(list = ls()); graphics.off()

# max.print
options(max.print=500)

# Library
library(terra)
library(rnaturalearth)
library(magick)

# Functions
scr = list.files("R/functions/",full.names=TRUE)
invisible(lapply(scr, source))


### ==================================================================
### Run world plot
### ==================================================================


# Project Europe map to cool CRS
#robin = "+proj=eqearth +datum=WGS84 +units=m +no_defs"
robin = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs"
spdf.world = vect(ne_countries(scale=10,returnclass="sf"))
spdf.robin = project(spdf.world,robin)
spdf.robin = aggregate(spdf.robin)

# Crop it with Europe's official extent
ext.robin = project(ext(c(-30,55,30,90)),from=crs(spdf.world),to=robin)
eu.robin = crop(spdf.robin,ext.robin)


### ==================================================================
### Plot maps in 8 groups
### (Arthropoda, Bryobiotina, Ichthyofauna, Fungi, Lichen, Mollusca,
### Tetrapoda, Tracheophyta)
### ==================================================================


# Create tiff groups and convert 0 to NAs
	# n = 1,820
arthropoda.l = c("orthoptera","odonata","lepidoptera","coleoptera","bees_wasps","spiders","ept")
arthropoda.r = lapply(arthropoda.l, function(x)
					rast(paste0("data/x_50km_geo_ext/xs2z_taxa_ext_",x,".tif")))
arthropoda.r = sum(rast(arthropoda.r),na.rm=TRUE)
arthropoda.r[arthropoda.r%in%0] = NA
names(arthropoda.r) = "arthropoda"
	# n = 201
bryobiotina.r = rast("data/x_50km_geo_ext/xs2z_taxa_ext_bryophytes.tif")
bryobiotina.r[bryobiotina.r%in%0] = NA
names(bryobiotina.r) = "bryobiotina"
	# n = 493
ichthyofauna.r = rast("data/x_50km_geo_ext/xs2z_taxa_ext_fishes.tif")
ichthyofauna.r[ichthyofauna.r%in%0] = NA
names(ichthyofauna.r) = "ichthyofauna"
	# n = 85
fungi.r = rast("data/x_50km_geo_ext/xs2z_taxa_ext_fungi.tif")
fungi.r[fungi.r%in%0] = NA
names(fungi.r) = "fungi"
	# n = 9
lichen.r = rast("data/x_50km_geo_ext/xs2z_taxa_ext_lichens.tif")
lichen.r[lichen.r%in%0] = NA
names(lichen.r) = "lichen"
	# n = 2596
mollusca.r = rast("data/x_50km_geo_ext/xs2z_taxa_ext_molluscs.tif")
mollusca.r[mollusca.r%in%0] = NA
names(mollusca.r) = "mollusca"
	# n = 1,242
tetrapoda.l = c("amphibians","mammals","reptiles","aves")
tetrapoda.r = lapply(tetrapoda.l, function(x)
				rast(paste0("data/x_50km_geo_ext/xs2z_taxa_ext_",x,".tif")))
tetrapoda.r = sum(rast(tetrapoda.r),na.rm=TRUE)
tetrapoda.r[tetrapoda.r%in%0] = NA
names(tetrapoda.r) = "tetrapoda"
	# n = 2,238
tracheophyta.r = rast("data/x_50km_geo_ext/xs2z_taxa_ext_plants.tif")
tracheophyta.r[tracheophyta.r%in%0] = NA
names(tracheophyta.r) = "tracheophyta"


### ==================================================================
### Plot subplot
### ==================================================================


# Map info
ras.list = rast(
	list(
		arthropoda.r,
		bryobiotina.r,
		ichthyofauna.r,
		fungi.r,
		lichen.r,
		mollusca.r,
		tetrapoda.r,
		tracheophyta.r
		)
	)
ras.eu = crop(ras.list,ext.robin)
ras.eu = disagg(ras.eu,10)
ras.eu = mask(ras.eu,eu.robin)
ras.name = sapply(ras.list,names)
ras.col = colorRampPalette(c("#ad8abd","#ffffbf","#a6dba0","#1b7837"))

# plot individuals map
for (i in 1:nlyr(ras.list))
{
	out.map = paste0("plot/EU_Fig2_map_trait_",ras.name[i],".png")
	png(out.map,width=120,height=80,unit="cm",res=100,pointsize=20)
	par(mfrow=c(1,1),mar=c(2,2,2,2))
	plot(eu.robin,axes=FALSE,col="#efedf5",lwd=10)
	plot(ras.eu[[i]],axes=FALSE,legend=FALSE,col=ras.col(20),add=TRUE)
	plot(eu.robin,add=TRUE,lwd=10)
	plot(as.polygons(ext(eu.robin)),axes=FALSE,add=TRUE,lwd=8)
	dev.off()
}

# Plot legend with cscl
png("plot/EU_Fig2_legend.png",width=100,height=100,unit="cm",res=80,pointsize=80)
par(lwd=15)
plot(10,10,type="n",xlim=c(0,100),ylim=c(0,200),axes=FALSE,xlab="",ylab="")
cscl(ras.col(20),c(55,50,10,190),zrng=c(0.1,1),tickle=-0.1,cx=1.5,
	lablag=-2.5,tria="b",at=seq(0.1,1,0.1),horiz=FALSE,title="Trait Coverage",
	labs=format(seq(0.1,1,0.1)),titlag=1.7)
dev.off()


### ==================================================================
### Assemble for main plot
### ==================================================================


# List of files
f4.lf = list.files("plot/")
f4.lf = f4.lf[grepl("Fig2_map|Fig2_legend",f4.lf)]

# Open
all.png = lapply(f4.lf,function(x){
	image_trim(image_read(paste0("plot/",x)))
})

# Prepare main file
png("plot/EU_Fig2_sub.png",width=120,height=160,unit="cm",res=100,pointsize=20)
par(mfrow=c(1,1),mar=c(2,2,2,2),lwd=1)
plot(1,1,type="n",xlim=c(0,1400),ylim=c(0,1600),axes=FALSE,xlab="",ylab="")

# Plot maps and legend
rasterImage(all.png[[2]],170,1310,585,1560)
rasterImage(all.png[[3]],600,1310,1000,1560)
rasterImage(all.png[[4]],170,1050,585,1300)
rasterImage(all.png[[5]],600,1050,1000,1300)
rasterImage(all.png[[6]],170,790,585,1040)
rasterImage(all.png[[7]],600,790,1000,1040)
rasterImage(all.png[[8]],170,530,585,780)
rasterImage(all.png[[9]],600,530,1000,780)
rasterImage(all.png[[1]],1020,830,1145,1250)

# Save
dev.off()