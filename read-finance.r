### read finance data - by state annual survey of state and local govts
library(readr)
library(dplyr)
library(blscrapeR)


setwd("R:/Project/NCANDS/ncands-csv/finance-data/")

### retrieve all files in folder that are csvs
files<-list.files()[grep("txt", list.files())]

rev<-list()
for(i in 1:length(files)){
  rev[[i]]<-read_fwf(files[i], fwf_widths(c(2,1,1,3,1,12,12,1,2),
                     c("state", "level", "blank1",
                                 "item", "blank2", "amount",
                                 "cv", "blank3", "year")),
                     col_types = "cccccddcc", na=".")
  rev[[i]]<-rev[[i]]%>%filter(level=="1")%>%filter(item=="E62")
  rev[[i]]$year<-as.numeric(paste("20", rev[[i]]$year, sep=""))
  rev[[i]]<-rev[[i]]%>%dplyr::select(state,amount,year)%>%
    rename(police.budget=amount)%>%filter(state!="00")
  
}

police.budget<-rev[[1]]
for(i in 2:length(rev)){
  police.budget<-bind_rows(police.budget, rev[[i]])
}

### inflate to final year in data - 2014 dollars here

base_year<-max(police.budget$year)
temp<-tempfile()
download.file("https://download.bls.gov/pub/time.series/cu/cu.data.1.AllItems", 
              temp)
cu_main <- read.table(temp, header = FALSE, sep = "\t", skip = 1, 
                      stringsAsFactors = FALSE, strip.white = TRUE)
colnames(cu_main) <- c("series_id", "year", "period", "value", 
                       "footnote_codes")
cu_main <- subset(cu_main, series_id == "CUSR0000SA0" & period != 
                    "M13" & period != "S01" & period != "S02" & period != 
                    "S03")
cu_main$date <- as.Date(paste(cu_main$year, cu_main$period, 
                              "01", sep = "-"), "%Y-M%m-%d")
cu_main <- cu_main[c("date", "value")]
cu_main$year <- format(cu_main$date, "%Y")
avg.cpi <- aggregate(cu_main$value, by = list(cu_main$year), 
                     FUN = mean)
colnames(avg.cpi) <- c("year", "avg.cpi")
avg.cpi$adj_value <- avg.cpi[, 2]/avg.cpi[as.numeric(which(avg.cpi$year == 
                                                             as.character(base_year))), 2]
avg.cpi$base_year <- as.character(base_year)
avg.cpi$pct_increase <- (1 - avg.cpi$adj_value) * -100
avg.cpi$adj_value <- round(avg.cpi$adj_value, 2)


###Inflation calc
infl<-avg.cpi
infl$year<-as.numeric(infl$year)
infl$adj_inverse<-1/infl$adj_value
police.budget.infl<-left_join(police.budget, infl, by="year")
police.budget.infl$pol.infl<-police.budget.infl$police.budget*police.budget.infl$adj_inverse*1000

## drop unused variables
police.budget.infl<-police.budget.infl%>%dplyr::select(state, year, pol.infl)

### convert numeric state codes (not FIPS) [01, 52] to state names


FIPSconvert<-matrix(data=c("01","01", 
	"02","02", 
	"03","04",
	"04","05",
	"05","06",
	"06","08",
	"07","09",
	"08","10", 
	"09","11", 
	"10","12", 
	"11","13", 
	"12","15", 
	"13","16", 
	"14","17", 
	"15","18", 
	"16","19", 
	"17","20", 
	"18","21", 
	"19","22", 
	"20","23", 
	"21","24", 
	"22","25", 
	"23","26", 
	"24","27", 
	"25","28", 
	"26","29", 
	"27","30", 
	"28","31", 
	"29","32", 
	"30","33", 
	"31","34", 
	"32","35", 
	"33","36", 
	"34","37", 
	"35","38", 
	"36","39", 
	"37","40", 
	"38","41", 
	"39","42", 
	"40","44", 
	"41","45", 
	"42","46", 
	"43","47", 
	"44","48", 
	"45","49", 
	"46","50", 
	"47","51", 
	"48","53", 
	"49","54", 
	"50","55", 
	"51","56"), ncol=2, byrow=TRUE)
FIPSconvert<-as.data.frame(FIPSconvert)
names(FIPSconvert)<-c("state", "FIPS")


police.budget.infl<-left_join(police.budget.infl, FIPSconvert)%>%dplyr::select(-state)%>%rename(state=FIPS)

pop<-read.csv("R:/Project/NCANDS/ncands-csv/seer-pop-state.csv", stringsAsFactors = FALSE, colClasses=c("character", "numeric"))
pop$tot.pop<-as.numeric(pop$tot.pop)
pop$state<-pop$state.fips
pop<-pop%>%dplyr::select(year, state, tot.pop)

police.budget.infl<-left_join(police.budget.infl, pop)
police.budget.infl$pol.infl.pc<-police.budget.infl$pol.infl/police.budget.infl$tot.pop

police.budget.infl<-police.budget.infl%>%dplyr::select(state, pol.infl.pc, year)


write.csv(x=police.budget.infl, file="R:/Project/NCANDS/ncands-csv/police-budget.csv", row.names=FALSE)