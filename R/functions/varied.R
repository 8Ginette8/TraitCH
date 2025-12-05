# Another one to normalize
normalize = function(x) (x-min(x,na.rm=TRUE))/(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))

# Or to obtain the mode
Mode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

# To extract fully duplicates (not just to filter them)
all_duplicated = function(x) {duplicated(x)|duplicated(x,fromLast=TRUE)}