# Small function to combine rows (mean for values + keep 1st value for categories + keep 1 if binary)
# (but also can create binary columns from several category values per species)
combine_rows = function(df,ref.col=NULL,fun.v=mean,binary=FALSE){

	# Just to avoid mistake in classes
	df[,ref.col] = as.character(df[,ref.col])

	# Continue
	if (binary){

		# Unique species (but remove NAs) + class checks
		u.sp = na.omit(unique(df[, ref.col]))

	} else {

		# Pre-select what we want to process if non-binary
		df.NA = df[is.na(df[,ref.col]),]
		df.wNA = df[!is.na(df[,ref.col]),]
		tofill = df.wNA[!duplicated(df.wNA[, ref.col]),]
		tar.df = df.wNA[all_duplicated(df.wNA[, ref.col]),]
		u.sp = unique(tar.df[, ref.col])
	}

	# Loop over species for specific
	sp.out =
	lapply(1:length(u.sp), function(x) {

		# Cat to follow progression
		cat("\r","...",round(x*100/length(u.sp),2),"%")

		# Extract
		sp.tar = df[df[, ref.col] %in% u.sp[x], ]

		# If binary = TRUE
		if (binary) {

		    # What class
		    w.cat = sapply(df, class)

		    # First test which info has a unique value for all each sp.
		    cat.info = df[, w.cat %in% c("character", "factor")]
		    out.cat = lapply(u.sp, function(y) {
		        smry.cat = cat.info[cat.info[, ref.col] %in% y, ]
		        return(apply(smry.cat, 2, function(w) length(table(w))))
		    })
		    all.cat = do.call("rbind", out.cat)
		    uniq.cat = apply(all.cat, 2, function(y) length(table(y[!y %in% 0])) == 1)

		    # Keep continuous, multi.cat and uniq.cat columns separately
		    cat.uniq = sp.tar[, names(uniq.cat)[uniq.cat], drop = FALSE]
		    cat.multi = sp.tar[, names(uniq.cat)[!uniq.cat], drop = FALSE]
		    cat.num = sp.tar[, !names(sp.tar) %in% names(cat.info), drop = FALSE]

		    # For unique
		    u.col = apply(cat.uniq, 2, unique)
		    u.col = sapply(u.col, function(y) y[!is.na(y)])
		    w.u = sapply(u.col, function(y) length(y) == 1 | identical(y, character(0)))
		    out = cat.uniq[, names(u.col)[w.u], drop = FALSE]
		    out[1, ] = unlist(lapply(u.col[w.u], function(y) if (identical(y, character(0))) NA else y))

		    # For numeric
		    if (any(w.cat %in% c("numeric", "integer"))) {
		        new.df2 = t(as.data.frame(apply(cat.num, 2, mean, na.rm = TRUE)))
		        i3 = sapply(1:ncol(new.df2), function(y) all(na.omit(new.df2[, y]) %in% c("0", "1", 0, 1)))
		        new.df2[i3][new.df2[i3] > 0 & !is.na(new.df2[i3])] = 1
		        # Combine
		        out = cbind(out[1, , drop = FALSE], new.df2)
		    }

		    # For multi
		    if (any(w.cat %in% c("character", "factor"))) {
		        out.multi = list()
		        for (j in 1:ncol(cat.multi)) {
		            tar.multi = names(cat.multi)[j]
		            multi.val = cat.multi[, tar.multi]
		            tcat = table(df[, tar.multi])
		            tcat[] = NA
		            tcat[multi.val[!is.na(multi.val)]] = 1
		            if (any(tcat %in% 1)) {
		                tcat[is.na(tcat)] = 0
		            }
		            names(tcat) = paste0(names(cat.multi)[j], ".", names(tcat))
		            out.multi[[j]] = tcat
		        }
		        # Combine
		        out = cbind(out[1, , drop = FALSE], t(as.data.frame(unlist(out.multi))))
		    }

		} else {

	        i2 = sapply(sp.tar[1, ], function(y) is.character(y) | is.factor(y))
	        i3 = sapply(1:ncol(sp.tar), function(y) all(na.omit(sp.tar[, y]) %in% c("0", "1", 0, 1)))
	        for (j in 2:nrow(sp.tar)) {
	            sp.tar[1, ][is.na(sp.tar[1, ]) & i2] = sp.tar[j, ][is.na(sp.tar[1, ]) & i2]
	        }
	        val = apply(sp.tar[!i2], 2, fun.v, na.rm = TRUE)
	        out = data.frame(t(val), sp.tar[1, i2, drop = FALSE], check.names = FALSE)
	        out = out[, names(sp.tar)]
	        out[i3][out[i3] > 0 & !is.na(out[i3])] = 1
		}

		return(out) # Ensure this is within the lapply's function
	})

	cat("\n")
	
	if (binary){

		return(do.call("rbind",sp.out))

	} else {

		# Pre-select what we want to process if non-binary
		ff = do.call("rbind",sp.out)
		tofill[tofill[,ref.col]%in%u.sp,] = ff
		return(rbind(tofill,df.NA))
	}
}