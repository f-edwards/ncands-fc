library(readr)
library(dplyr)

ncands.fwf<-function(dat, key){
	key<-read.csv(key)
	dat<-read_fwf(dat, 
		fwf_widths(key$Length),
		col_types=paste(key$Data.Type, collapse="")
	)
	names(dat)<-key$VarName
	return(dat)
}
