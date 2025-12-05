to_GenusSpecies = function(x){
	
	# First homogenize
	x = str_squish(gsub("L\\.|\\(L\\.\\)","",x)) # Remove annoying "L."
	x = str_squish(gsub("new Name: |neuer Name: ","",x)) # Remove other shit
	x = str_squish(gsub("\\( |\\(\\: ","",x)) # Remove other shit 2

	# Add space after point
	x = gsub("\\.(?=[A-Za-z])",". ",x,perl=TRUE)
	
	# Split to test
	nsplit = strsplit(x," ")
	xdf = as.data.frame(x)

	# Set UNCERTAIN  when one words or if aff. or cf. etc are found...
	xdf[sapply(nsplit,length)%in%1,"CanonicalName"] = "UNCERTAIN"
	uncertW = sapply(nsplit,function(y){
		a1 = any(y%in%c("aff.","cf.","f.","cf","cult.","aff","s.l.","sl","s.","l.","sect.")) | grepl("\\.",y[1])
		a2 = any(grepl("\\?",y)) | y[2]%in%c("sp","spp","sp.","spp.",".","spec.") | grepl("sp\\.|[0-9]",y[2])
		return(a1|a2)
	})
	xdf[uncertW,"CanonicalName"] = "UNCERTAIN"

	# Keep if "population" or "x" is found within the name
	popW = sapply(nsplit,function(y) any(grepl("population",y) | y%in%c("x","×")))
	xdf[popW,"CanonicalName"] = xdf[popW,"x"]

	# Add an interogation points if second word could be species or author
	sp.or.author = str_to_sentence(xdf$x)
	SorAW = sapply(nsplit,function(y) length(y)%in%2 & grepl("\\.|[[:upper:]]|\\(",y[2])) & is.na(xdf$CanonicalName)
	xdf[SorAW,"CanonicalName"] = str_to_sentence(paste0(xdf[SorAW,"x"],"?"))

	# Look for authors anomalies in second position
	secondW = sapply(nsplit,function(y) grepl("\\.|[[:upper:]]|\\(",y[2])) & is.na(xdf$CanonicalName)
	xdf[secondW,"CanonicalName"] = sapply(nsplit[secondW],function(y) paste(y[-2],collapse=" "))

	# If two words, just transfer the genus-species info
	twoW = sapply(nsplit,function(y) length(y)==2) & is.na(xdf$CanonicalName)
	xdf[twoW,"CanonicalName"] = xdf[twoW,"x"]

	# Keep the "aggr." names
	aggrW = sapply(nsplit,function(y) any(y%in%"aggr.")) & is.na(xdf$CanonicalName)
	xdf[aggrW,"CanonicalName"] = xdf[aggrW,"x"]

	# Keep Genus/species if 3 words and the last is the author
	threeAW = sapply(nsplit,function(y) length(y)==3 & grepl("\\.|[[:upper:]]|\\(",tail(y,1))) & is.na(xdf$CanonicalName) 
	xdf[threeAW,"CanonicalName"] = sapply(nsplit[threeAW],function(y) paste(y[1:2],collapse=" "))

	# Create a subspecies canonical name if the string is found
	ssubp = c("subsp.","subsp","ssp.","ssp","subps.","subps")
	subspW = sapply(nsplit,function(y) any(y%in%ssubp)) & is.na(xdf$CanonicalName)
	xdf[subspW,"CanonicalName"] = sapply(nsplit[subspW], function(y) paste(y[1],y[2],"subsp.",y[which(y%in%ssubp)+1],collapse=" "))

	# Create a var. canonical name if the string is found
	svar = c("var.","var")
	varW = sapply(nsplit,function(y) any(y%in%svar)) & is.na(xdf$CanonicalName)
	xdf[varW,"CanonicalName"] = sapply(nsplit[varW],
		function(y) paste(y[1],y[2],"var.",y[which(y%in%svar)+1],collapse=" "))

	# Create a subsepecies canonical names if 3 words and the subsp. is not informed
	threeW = sapply(nsplit,length)%in%3 & is.na(xdf$CanonicalName)
	xdf[threeW,"CanonicalName"] = sapply(nsplit[threeW],function(y) paste(y[1],y[2],"subsp.",y[3],collapse=" "))

	# Remove the last author name
	lastW = is.na(xdf$CanonicalName)
	lastI = sapply(nsplit,function(y) suppressWarnings(min(which(grepl("\\.|[[:upper:]]|\\(",y[-1]))+1)))
	lastInf = lastI%in%"Inf"
	lastI = lastI[lastW & !lastInf]
	lastW = lastW & !lastInf
	if (any(lastW)){
		xdf[lastW,"CanonicalName"] = sapply(1:length(lastI),
			function(y) paste(nsplit[lastW][[y]][1:(lastI[y]-1)],collapse=" "))

		# New canonical names creates with 3 words needs to be converted to subspecies
		nsplit2 = strsplit(xdf$CanonicalName," ")
		lastW2 = sapply(nsplit2,function(y) length(y)%in%3 & !any(y%in%"aggr.")) & is.na(xdf$CanonicalName)
		xdf[lastW2,"CanonicalName"] = sapply(nsplit2[lastW2],function(y) paste(y[1],y[2],"subsp.",y[3]))
	}

	# Keep species names if all are lowcases except first one (just for string with letters)
	allezW =  grepl("[A:Za:z]",xdf$x) & is.na(xdf$CanonicalName)
	qlow = xdf[allezW,"x"] == str_to_sentence(xdf[allezW,"x"])
	xdf[allezW,][qlow,"CanonicalName"] = xdf[allezW,][qlow,"x"]

	# Transform back UNCERTAIN to NAs
	xdf[xdf$CanonicalName%in%"UNCERTAIN","CanonicalName"] = NA

	# Return
	return(xdf)	
}