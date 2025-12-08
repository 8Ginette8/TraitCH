# ###################################################################################
# TraitsCH: Plot figures
# 
# $Date: 2024-27-11
#
# Author: Yohann Chauvier, yohann.chauvier@wsl.ch
# Aquatic Ecology Group
# Swiss Federal Research Institute EAWAG
# 
# Description: Our figures for the data paper
#
# ###################################################################################

### ==================================================================
### Initialise system
### ==================================================================


# Cleaning
rm(list = ls()); graphics.off()

# max.print
options(max.print=500)

# Libraries
library(plotly)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(stringr)
library(reticulate)

# Functions
scr = list.files("R/functions",full.names=TRUE)
invisible(lapply(scr, source))


### ==================================================================
### Assemble the data and add some percentage
### ==================================================================


# Raw trait files
list.traits = list.files("outputs/raw_traits/",full.names=TRUE)
list.traits = list.traits[grepl("s2z_",list.traits)]
open.traits = lapply(list.traits,function(x) read.table(x,header=TRUE))

# Total number of European species
spn = sapply(open.traits,function(x) nrow(x))

# First taxo level
taxa.L1 = gsub("outputs/raw_traits/s2z_raw_traits_|.txt","",list.traits)
taxa.L1[taxa.L1%in%c("amphibians","aves","mammals","reptiles")] = "Tetrapoda"
taxa.L1[taxa.L1%in%"fishes"] = "Pisces"
taxa.L1[taxa.L1%in%c("bees_wasps","coleoptera","ept","lepidoptera","odonata","orthoptera","spiders")] = "Arthropoda"
taxa.L1[taxa.L1%in%c("lichens")] = "Lichen"
taxa.L1[taxa.L1%in%c("fungi")] = "Fungi"
taxa.L1[taxa.L1%in%"plants"] = "Tracheophyta"
taxa.L1[taxa.L1%in%"molluscs"] = "Mollusca"
taxa.L1[taxa.L1%in%"bryophytes"] = "Bryobiotina"

# Second taxo level
taxa.L2 = c("Class","Class","Order","Class","Order","Order","Class","Phylum","Order","Phylum",
    "Class","Class","Order","Order","Class","Class","Order")

# Number of reported species in Europe
taxa.EUsp = data.frame(raw = list.traits,
                       spN_EU = spn,
                       taxa.l1 = taxa.L1,
                       taxa.l2 = taxa.L2)

# Empty data.frame
i.out = list()
for (i in 1:nrow(taxa.EUsp))
{
    # Open file & capture lower taxonomic levels
    rt.all = read.table(taxa.EUsp$raw[i],header=TRUE)
    l2 = unique(rt.all[,taxa.EUsp$taxa.l2[i]])

    # First calculate coverage for each group
        # General
    rt.all.nan = rt.all[rt.all$MISSING_ALL_TRAITS%in%"NO",]
        # % Swiss data coverage
    rt.swiss = rt.all[rt.all$Source%in%c("InfoSpecies","SPEED2ZERO"),]
    rt.swiss.nan = rt.all.nan[rt.all.nan$Source%in%c("InfoSpecies","SPEED2ZERO"),]
    swiss.perct = round(nrow(rt.swiss.nan)*100/nrow(rt.swiss),1)
        # % EU data coverage (I only have the estimated  number of knwon species per group)
    eu.perct = round(nrow(rt.all.nan)*100/nrow(rt.all),1)
        # Handling exceptions here
    if (grepl("_fungi|_lichens",taxa.EUsp$raw[i])) {eu.perct = NA}

    # Second calculate coverage and number of traits for finer levels
    j.out = list()
    for (j in 1:length(l2))
    {
        # Select the group
        rt.tar = rt.all[rt.all[,taxa.L2[i]]%in%l2[j],] 
        
        # Number of traits
        trait.col = 18:(ncol(rt.tar)-1) # Starting from EcoTer
        n.trait = length(trait.col)

        # % data completeness
        rt.nan = rt.tar[rt.tar$MISSING_ALL_TRAITS%in%"NO",]
        nan.trt = round(length(rt.nan[!is.na(rt.nan)])*100/(nrow(rt.nan)*ncol(rt.nan)),1)

        # Number of species and accepted names
        sp.n = length(rt.nan$Species)
        sp.a = length(unique(rt.nan$GBIF_accepted))

        # Store
        j.out[[j]] = data.frame(level1=taxa.EUsp$taxa.l1[i],level2=l2[j],ntrt=n.trait,CH.cover=swiss.perct,
            EEA.cover=eu.perct,data.nan=nan.trt,all.nsp=sp.n,gbif.nsp=sp.a,
            n.eu=nrow(rt.all.nan),n.ch=nrow(rt.swiss.nan))
    }
    # Store
    i.out[[i]] = do.call("rbind",j.out)
}
plot.dat = do.call("rbind",i.out)

# Remove all irrelevant data
plot.dat = plot.dat[!plot.dat$CH.cover%in%0,]
plot.dat = plot.dat[!plot.dat$all.nsp%in%0,]
plot.dat[is.na(plot.dat$level2),"level2"] = "Unknown"
plot.dat[plot.dat$level2%in%"Hymenoptera","level2"] = "Apocrita"

# Avoid duplicated taxa in Fungi and Lichens
plot.dat[plot.dat$level2%in%"Ascomycota"&plot.dat$level1%in%"Lichen","level2"] = "Ascomycota (L)"
plot.dat[plot.dat$level2%in%"Ascomycota"&plot.dat$level1%in%"Fungi","level2"] = "Ascomycota (F)"
plot.dat[plot.dat$level2%in%"Unknown"&plot.dat$level1%in%"Tracheophyta","level2"] = "Unknown (T)"
plot.dat[plot.dat$level2%in%"Unknown"&plot.dat$level1%in%"Fungi","level2"] = "Unknown (F)"


### ==================================================================
### Plot plot Fig. 1a
### ==================================================================


conda_list()
#reticulate::install_miniconda()
#reticulate::conda_create("r-kaleido-env")
#reticulate::py_install("plotly==5.18.0", envname = "r-kaleido-env", pip = TRUE)
#### Old version corrects bug with the one below
#reticulate::py_install("kaleido==0.1.0.post1", envname = "r-kaleido-env", pip = TRUE)
reticulate::use_miniconda("r-kaleido-env",required=TRUE)
reticulate::py_list_packages() # Everything installed correctly!

# Define distinct base colors for each group, ensuring Araneae and Orthoptera are distinct
group.colors = c(
  Arthropoda = "#8dd3c7",
  Bryobiotina = "#b3de69",
  Fungi = "#fb8072",
  Lichen = "#80b1d3",
  Mollusca = "#fdb462",
  Pisces = "#fccde5",
  Tetrapoda = "#e0e03f",
  Tracheophyta = "#cf9bce"
)

# Ref + distinguish level2
sunplot = plot.dat[,c("level1","level2","gbif.nsp")] # gbif to have a more proportionate representtation
sunplot = sunplot[order(sunplot$level1),]

# Generate 47 distinct colors
# Define your major groups and category counts
category.counts = table(sunplot$level1)

# Generate distinct gradients for each group
group.gradients = sapply(names(category.counts), function(group) {
    colorRampPalette(c(group.colors[group], paste0(substr(group.colors[group], 1, 1), "eeeeee")))(category.counts[group])
}, simplify = FALSE)

# Combine all the colors into one vector
all.colors = unlist(group.gradients)

# Create the nested pie chart
outter.plot = plot_ly(sunplot) %>%
  add_pie(
    labels = ~level2, 
    values = ~gbif.nsp, 
    hole = 0.7,
    text = ~paste0(level2," [",gbif.nsp,"]"), 
    textinfo = "label", #text
    name = "Category",
    sort = FALSE,
    type = 'pie',
    showlegend = FALSE,
    marker = list(colors = all.colors),
    textfont = list(size = 18), # Adjust text size
    # insidetextorientation = "radial", # Not needed here but align the text
    domain = list(x = c(0.08, 0.92), y = c(0.08, 0.92))
  ) %>%
    layout(
    margin = list(t = 150, b = 100, l = 100, r = 100)
  ) %>%
    add_pie(
     labels = ~level1, 
     values = ~gbif.nsp, 
     hole = 0.6,
     text = NULL, 
     textinfo = "text",
     name = "Category",
     sort = FALSE,
     type = 'pie',
     showlegend = FALSE,
     marker = list(colors = sapply(all.colors,function(x) paste0(x[1],"50"))),
     #textfont = list(size = 12, family = 'Arial Black'), # Adjust font size here # No text in the inner
     domain = list(x = c(0.206, 0.794), y = c(0.206, 0.794)), # Position of the pie
     rotation = -64.3  # Rotate the inner pie chart by 90 degrees (or any angle)
    ) %>%
      layout(
      margin = list(t = 150, b = 100, l = 100, r = 100)
  )
save_image(outter.plot,scale=4,file="plot/Fig1_part1.png",width=1100,height=1200)

# Plot legend with cscl
png("plot/Fig1_legend.png",width=100,height=100,unit="cm",res=80,pointsize=80)
terra::plot(1,1,xlim=c(0,100),ylim=c(0,100),type="n",axes=FALSE)
legend(0,100,names(group.colors),
      col=group.colors,bty="n",cex=1.8,lwd=100,text.font=2)
dev.off()


### ==================================================================
### Fig. 1b
### ==================================================================


# Reorder and reverse `level2` based on `level1` order
plot.dat.gg = plot.dat %>%
  mutate(level2 = fct_reorder(level2, level1, .fun = min) %>% fct_rev())

# Add group-level color mapping to the data
plot.dat.gg = plot.dat.gg %>%
  mutate(GroupColor = group.colors[level1])

# Prepare data for plotting
traits.data = plot.dat.gg %>%
  select(level2, level1, ntrt, data.nan, GroupColor) %>%
  pivot_longer(cols = c(ntrt, data.nan), names_to = "Metric", values_to = "Value") %>%
  mutate(
    Direction = ifelse(Metric == "ntrt", 1, -1), # Positive for ntrt, negative for data.nan
    BarValue = Value * Direction                 # Assign values for plotting in opposite directions
  )

# Choose a scaling factor to reduce the right-side bars
scaling.factor = 0.5  # You can adjust this value (e.g., 0.5, 0.3) to achieve the desired size

# Apply scaling to the positive BarValues
traits.data$BarValue.scaled = ifelse(traits.data$BarValue > 0, 
                                      traits.data$BarValue * scaling.factor, 
                                      traits.data$BarValue)

# Plot with adjusted bar width, text placement, and axis label sizes
p = ggplot(traits.data, aes(x = level2, y = BarValue.scaled)) +
  geom_bar(
    aes(fill = level1, alpha = Metric), # Different alpha for Metric
    stat = "identity", position = "identity", width = 1  # Slightly reduced bar width
  ) +
  geom_text(
    aes(
      x = level2, y = 0, label = level2  # Position labels at y = 0 (middle of the bars)
    ),
    inherit.aes = FALSE,  # Prevent inheriting other aesthetics
    hjust = 0.5, vjust = 0.5, size = 4.5, fontface = "plain", color = "black"  # Smaller text size for bar labels
  ) +
  coord_flip() +  # Horizontal bars
  scale_fill_manual(values = group.colors) + # Use the custom colors
  scale_alpha_manual(
    values = c(ntrt = 1, data.nan = 0.5) # Fully opaque for ntrt, semi-transparent for data.nan
  ) +
  scale_y_continuous(
    breaks = seq(-100, 100, by = 25),  # Keep the same ticks
    labels = abs(c(seq(-100, 100, by = 25)[1:5], seq(-100, 100, by = 25)[6:9]*2)),
    expand = c(0, 0)
  ) +
  labs(
    x = "",  # Adjusted x-axis label text
    y = "",
    fill = "",
    alpha = ""
  ) +
  guides(alpha = "none") +                # Remove the alpha (Metric) legend only
  theme_minimal(base_size = 25) +
  theme(
    panel.grid = element_blank(),                           # Remove both major and minor gridlines
    legend.position = "none",                               # Remove the entire legend
    axis.text.y = element_blank(),                          # Remove y-axis text
    axis.title.x = element_text(size = 16),                 # Increase the size of the x-axis title
    axis.line.x = element_line(color = "black", size = 0.5),# Add x-axis line
    axis.ticks.x = element_line(color = "black", size = 0.5)# Add ticks on the x-axis
  )

ggsave(
  filename = "plot/Fig1_part2.png",  # File name and format (can also be .jpeg, .tiff, etc.)
  plot = p,                          # Saves the last plot generated
  dpi = 300,                         # Resolution (dots per inch), 300 is standard for high resolution
  width = 10,                        # Width of the plot in inches
  height = 7,                        # Height of the plot in inches
  units = "in",                      # Units for width and height (can also be "cm")
  device = "png"                     # File format, can also use "jpeg", "tiff", etc.
)


### ==================================================================
### Plot 4a
### ==================================================================


# Extract 17 classic trait group and assign to col
taxa = str_to_title(gsub("./raw_traits/s2z_raw_traits_|.txt","",list.traits))
taxa[6] = "EPT"
plot.dat$group = plot.dat$level2
plot.dat[plot.dat$level1%in%"Tracheophyta","group"] = "Tracheophyta"
plot.dat[plot.dat$level2%in%c("Araneae","Opiliones"),"group"] = "Arachnida"
plot.dat[plot.dat$level1%in%"Mollusca","group"] = "Mollusca"
plot.dat[plot.dat$level1%in%"Bryobiotina","group"] = "Bryobiotina"
plot.dat[plot.dat$level1%in%"Fungi","group"] = "Fungi"
plot.dat[plot.dat$level1%in%"Lichen","group"] = "Lichen"
plot.dat[plot.dat$level1%in%"Pisces","group"] = "Pisces"
plot.dat[plot.dat$level2%in%c("Ephemeroptera","Trichoptera","Plecoptera"),"group"] = "EPT"

# Get the summary per group
g2.order = c("Apocrita","Arachnida","Coleoptera","EPT","Lepidoptera","Odonata","Orthoptera",
  "Bryobiotina","Fungi","Lichen","Mollusca","Pisces","Amphibia","Aves","Mammalia","Reptilia","Tracheophyta")
bdata =
lapply(rev(g2.order),function(x){
  sub.dat = plot.dat[plot.dat$group%in%x,c("level1","CH.cover","EEA.cover","all.nsp","data.nan")]
  sub.dat2 = apply(sub.dat[,c("CH.cover","EEA.cover")],2,mean,na.rm=TRUE)
  sub.dat3 = apply(sub.dat[,c("all.nsp"),drop=FALSE],2,sum,na.rm=TRUE)
  sub.dat4 = apply(sub.dat[,c("data.nan"),drop=FALSE],2,mean,na.rm=TRUE)
  coll = group.colors[names(group.colors)%in%sub.dat$level1[1]]
  data.frame(Group=x,CH.cover=sub.dat2[1],EEA.cover=sub.dat2[2],
    n_sp=sub.dat3,color=coll,data.complete=sub.dat4)
})
bdata = do.call("rbind",bdata)

# Handle missing values (NA) by replacing them with 0 or another suitable value
bdata$EEA.cover[is.na(bdata$EEA.cover)] = 0
row.names(bdata) = bdata$Group
bcol = bdata$color
bdata.bp = t(bdata[,c("CH.cover","EEA.cover")])

# Plot first part
png("plot/Fig4_part1.png",width=100,height=70,unit="cm",res=100,pointsize=30)
par(mfrow=c(1,1),mar=c(12,16,0,30),lwd=2,cex=1)
b = barplot(bdata.bp,width=5,beside=TRUE,horiz=TRUE,col=c("#fc927290","#3182bd90"),
  las=2,cex.names=2.5,cex.axis=2.5,xpd=TRUE,yaxt="n",border=NA)
mtext("Checklist coverage (%)",1,6.5,cex=2.5)
mtext(colnames(bdata.bp),2,1,col=bdata$color,at=seq(10,260,15),las=2,cex=2.5,font=2)
dev.off()


### ==================================================================
### Plot 4b
### ==================================================================


# Extract evaluation values for each group
eval.files = list.files("outputs/missRanger_evaluations/",full.names=TRUE)
eval.files = eval.files[grepl("S_EVAL_s2z",eval.files)]

# Open the values for boxplot
box.val = lapply(eval.files,function(x) read.table(x,header=TRUE)$OBB_pred_error)
names(box.val) = c("Amphibia","Aves","Apocrita","Bryobiotina","Coleoptera","EPT",
  "Pisces","Fungi","Lepidoptera","Lichen","Mammalia","Mollusca","Odonata",
  "Orthoptera","Tracheophyta","Reptilia","Arachnida")
box.val = box.val[g2.order]

# Plot
png("plot/Fig4_part2.png",width=100,height=60,unit="cm",res=70,pointsize=30)
par(mfrow=c(1,1),mar=c(2,5,1,6),cex=2.5,font.lab=2,font.axis=1)
b = boxplot(box.val,xlab="",ylab=expression("OBB prediction errors"),col=rev(bdata$color),
  outline=FALSE,range=1.5,ylim=c(0,0.55),xaxt="n",border=NA)
axis(1,at=seq(1,17,2),labels=rep("",length(seq(1,17,2))),tcl=-0.2)
bmin = b$stats[1:2,]
umax = b$stats[4:5,]
medd = b$stats[3,]
lapply(1:length(b$names),function(x) lines(c(x,x),c(bmin[1,x],bmin[2,x]),col="#525252",lwd=12))
lapply(1:length(b$names),function(x) lines(c(x,x),c(umax[1,x],umax[2,x]),col="#525252",lwd=12))
lapply(1:length(b$names),function(x) lines(c(x-0.4,x+0.4),c(medd[x],medd[x]),col="#525252",lwd=18))
dev.off()


### ==================================================================
### Quick stats
### ==================================================================


# Number of traits per group
aggregate(ntrt ~ group, data = plot.dat, FUN = mean)
aggregate(n.eu ~ group, data = plot.dat, FUN = mean)
aggregate(n.ch ~ group, data = plot.dat, FUN = mean)