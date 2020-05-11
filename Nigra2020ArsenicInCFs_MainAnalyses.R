##------ Arsenic in drinking water in US correctional facilities, 2006-2011 ------##
#   Contact:                                                                      
#   Anne Nigra, ScM, PhD candidate                                                                 
#   Columbia University Mailman School of Public Health                             
#   Environmental Health Sciences                                                   
#   aen2136@cumc.columbia.edu 

#### Some package and fxns ######
library(Hmisc)
library(reshape)
library(ggplot2)
library(plyr)
library(reshape2)
library(RColorBrewer)
library(colorspace)
library(openxlsx)
# Function to convert to confident interval
convert.to.ci <- function( vector )
{
  vector <- paste0( vector[1] , " (" , vector[ 2 ] , "," , vector[ 3 ], ")" ) 
  return( vector )
}

to.prop <- function( vector )
{
  vector <- paste0( vector[1] , " / " , vector[ 2 ] ) 
  return( vector )
}

to.n.prop <- function( vector )
{
  vector <- paste0( vector[1] , " (" , vector[ 2 ],"%) " ) 
  return( vector )
}
to.mean.se <- function( vector )
{
  vector <- paste0( vector[1] , " (" , vector[ 2 ],") " ) 
  return( vector )
}
### end

####### 1. Load and pre-clean EPA Six Year Review 3 data for arsenic #########
# Load EPA Third Six Year Review Data ##
setwd("~/Google Drive/Research/EPAAsTrajectories/Data")
As6YR3importedR<-read.delim2("arsenic.txt", header = TRUE, sep = "\t", dec = ".")
# Available from: https://www.epa.gov/dwsixyearreview/six-year-review-3-compliance-monitoring-data-2006-2011
save("As6YR3importedR", file="As6YR3importedR")
As6YR3<-As6YR3importedR

# Pre-clean some data ##
As6YR3$PWSID <- gsub(" ", "", As6YR3$PWSID) #Removes excess space from PWSID variable
As6YR3$Sample.Collection.Date <- gsub(" 00:00:00.000", "", As6YR3$Sample.Collection.Date) #remove timestamps
describe(As6YR3$Sample.Collection.Date)
As6YR3$Sample.Collection.Date <- substr(As6YR3$Sample.Collection.Date, 0, 10) #only keep first 10characters in character vector (keep date, not time)

# Create new variable converting [As] to ug/L ##
describe(As6YR3$Unit) # All records are reported in mg/L
table(As6YR3$Detect)
#     0      1 
# 162127 135227 

# Evalute all records meeting our search criteria:
prison <- As6YR3[grepl("PRISON", As6YR3$System.Name), ];nrow(prison) #593
correction <- As6YR3[grepl("CORRECTION", As6YR3$System.Name), ];nrow(correction) #648
juvenile <- As6YR3[grepl("JUVENILE", As6YR3$System.Name), ];nrow(juvenile) #23
detention <- As6YR3[grepl("DETENTION", As6YR3$System.Name), ];nrow(detention) #64
penitentiary<- As6YR3[grepl("PENITENTIARY", As6YR3$System.Name), ] # 22
jail<- As6YR3[grepl("JAIL", As6YR3$System.Name), ] # 22
women<- As6YR3[grepl("WOMEN", As6YR3$System.Name), ] # 51

prisons<-rbind(prison,correction,juvenile,detention,penitentiary,women); nrow(prisons) #1401
table(prisons$State.Code)

# WHo are non-CWSs?
othersystems<-As6YR3[which(As6YR3$System.Type!="C"&As6YR3$System.Type!="C   "),] ; dim(othersystems)

# Exclude water systems which are not community water systems (CWS) ##
describe(As6YR3$PWSID) #54845 unique PWSs, 297354 unique records
As6YR3<-As6YR3[which(As6YR3$System.Type=="C"|As6YR3$System.Type=="C   "),] ; dim(As6YR3)
table(As6YR3$System.Type)
describe(As6YR3$PWSID) #37098 unique PWSs, 230265 unique records ; excluded 67089 records from 17747 systemes 

# How many correctional facilities records lost b/c not classified CWS? 
prison <- As6YR3[grepl("PRISON", As6YR3$System.Name), ];nrow(prison) #593
correction <- As6YR3[grepl("CORRECTION", As6YR3$System.Name), ];nrow(correction) #607
juvenile <- As6YR3[grepl("JUVENILE", As6YR3$System.Name), ];nrow(juvenile) #13
detention <- As6YR3[grepl("DETENTION", As6YR3$System.Name), ];nrow(detention) #37
penitentiary<- As6YR3[grepl("PENITENTIARY", As6YR3$System.Name), ];nrow(penitentiary) # 22
jail<- As6YR3[grepl("JAIL", As6YR3$System.Name), ];nrow(jail) # 11
women<- As6YR3[grepl("WOMEN", As6YR3$System.Name), ];nrow(women) # 51
# we will lose some NTNC systems that would qualify  (1401-1323) = 78 records lost to restriction to CWSs
prisons<-rbind(prison,correction,juvenile,detention,penitentiary,women); nrow(prisons) #1323
table(prisons$State.Code)

# Who are the detects reporting values below 1ug/L?
who<-As6YR3[which(As6YR3$Detect==1),]
min(who$Value) #0.0051ug/L
anyprobs<-who[which(who$Value<0.001),] 
describe(anyprobs$Unit) #all in mg/L
describe(anyprobs$Detection.Limit.Value)
describe(anyprobs$Detection.Limit.Unit)
anyprobs$ugLdetection<-NA
anyprobs$ugLdetection[(anyprobs$Detection.Limit.Unit=="UG/L")]<-anyprobs$Detection.Limit.Value
describe(anyprobs$ugLdetection)
anyprobs$ugLdetection[(anyprobs$Detection.Limit.Unit!="UG/L")]<-anyprobs$Detection.Limit.Value*1000
describe(anyprobs$ugLdetection)
describe(anyprobs$Unit)
#We  retain these 6,190 records with detects below 1ug/L and consider them valid

### Handling non-detects
# If the system reports a LOD, we impute LOD/sqrt(2) for values below the LOD
# If a system does not report a LOD, we impute the EPA  method LOD (0.5ug/L) divided by squarert(2) = 0.35
# Any system which reports a ug/L LOD greater than 5 gets replaced wtih 0.35
# If value is non-detect (<1ug/L), it is currently recorded as NA in database

describe(As6YR3$Value)
As6YR3$ugLValue <- 0
As6YR3$ugLValue <- (As6YR3$Value)*1000

detects<-As6YR3[which(As6YR3$Detect==1),] #104,730
nondetects<-As6YR3[which(As6YR3$Detect==0),];dim(nondetects) #125,535

describe(nondetects$Detection.Limit.Code) #Indicates the type of Detection Limit reported
#in the Detection Limit Value column (e.g., the Minimum Reporting Level, Laboratory Reporting Level, etc.) 
describe(nondetects$Detection.Limit.Value)
describe(nondetects$Detection.Limit.Unit)
#some records report Detection.Limit.Codes of MRL, meaning the detection limit reported is the MRL, not the LOD

# if record in 'nondetect' does not have a value for DL limit, impute 0.35 ug/L
nondetects$ugLValue[is.na(nondetects$Detection.Limit.Value)]<-0.35
describe(nondetects$ugLValue) #112123 still not imputed

# if record in 'nondetect' does not have units for Detection limit, impute 0.35 ug/L
nondetects$Detection.Limit.Unit <- gsub(" ", "", nondetects$Detection.Limit.Unit) #Removes excess 
nondetects$ugLValue[nondetects$Detection.Limit.Unit==""]<-0.35
describe(nondetects$ugLValue) #103477 still not imputed

# if record in 'nondetect' is reporting the MRL, impute 0.35 ug/L
nondetects$ugLValue[nondetects$Detection.Limit.Code=="MRL"]<-0.35
describe(nondetects$ugLValue) #47886 still not imputed

# if record in 'nondetect' is reporting the MDL as zero, impute 0.35 ug/L
nondetects$ugLValue[nondetects$Detection.Limit.Code=="MDL"&nondetects$Detection.Limit.Value==0]<-0.35
describe(nondetects$ugLValue) #43254 still not imputed
who<-nondetects[is.na(nondetects$ugLValue),]
describe(who$Detection.Limit.Code)

# if record is missing Detection.Limit.Code, impute 0.35 ug/L (we don't know if this is a DL or MRL)
nondetects$Detection.Limit.Code <- gsub(" ", "", nondetects$Detection.Limit.Code) #Removes excess 
nondetects$ugLValue[nondetects$Detection.Limit.Code==""]<-0.35
describe(nondetects$ugLValue) #36895 still not imputed
who<-nondetects[is.na(nondetects$ugLValue),]
describe(who$Detection.Limit.Code) #All DL or MDL, so all good!
describe(who$Detection.Limit.Unit)
# The rest have a unit (mg or ug) and are the DL; we can impute these now as LOD/sqrt(2)
nondetectsfixed<-nondetects[!is.na(nondetects$ugLValue),]
who<-nondetects[is.na(nondetects$ugLValue),]

# Within 'who' Create ugLOD; these are 36,895 records with specific LODs 
who$ugLOD<-NA
who$ugLOD[who$Detection.Limit.Unit=="MG/L"]<-who$Detection.Limit.Value[who$Detection.Limit.Unit=="MG/L"]*1000
who$ugLOD[who$Detection.Limit.Unit=="UG/L"]<-who$Detection.Limit.Value[who$Detection.Limit.Unit=="UG/L"]
describe(who$ugLOD)

# A problem: 6,639 systems have ugLODs greater than or equal to 5ug/L
howmany<-who[which(who$ugLOD>=5),];dim(howmany)

# Records with LODs greater than 10ug/L likely have reported the wrong units
#Records for WV: (checked http://129.71.204.189:1977/DWWpublic/)
#Records for WI: that are greater than 10 are likely incorrect units (checked https://dnr.wi.gov/dwsviewer/)

describe(howmany$Detection.Limit.Unit)
table(howmany$State.Code)
# 339 in WV; 1204 in WI; 1 in OR; 23 in CT; 2 MT; 4,942 NC;38 AZ
# Extract one row for each PWSID
problems<-howmany[!duplicated(howmany$PWSID),]
write.csv(howmany, file="ProblemLODs.csv") 
# Any files from WV that are greater than 10?
ten<-howmany[which(howmany$ugLOD>10),] #1202
table(ten$State.Code)
who$ugLOD[who$ugLOD>10]<-who$ugLOD[who$ugLOD>10]/1000
howmany2<-who[which(who$ugLOD>=5),] #5,437
##Now we have a ug/L LOD for each record in who, assuming those >10ugL were incorrect units

who$ugLValue[is.na(who$ugLValue)]<-who$ugLOD/1.414214
describe(who$ugLValue) #0 missing
table(who$ugLOD)

#Now, for systems rporting an LOD at 5 ug/L or greater, we replace with 0.35
who$ugLValue[who$ugLOD>=5]<-0.35
who$ugLOD<-NULL
#Merge back together
nondetects<-rbind(nondetectsfixed,who)

#Merge detects back with nondetects
As6YR3<-rbind(nondetects,detects)
describe(As6YR3$ugLValue)
min(As6YR3$ugLValue)
describe(As6YR3$ugLValue)
describe(As6YR3$PWSID) # 230265 total records

# Check date format 
describe(As6YR3$Sample.Collection.Date)
table(As6YR3$Sampling.Point.Type)
table(As6YR3$Source.Type.Code)#Source Type The type of water source, based on whether treatment has taken place.
raw<-As6YR3[which(As6YR3$Source.Type.Code=="RW"),]

# Subset to keep only necessary data ##
myvars<- c("PWSID", "System.Name", "State.Code", "System.Type","Retail.Population.Served","Adjusted.Total.Population.Served",  "Source.Water.Type",  
           "Sample.ID", "Sample.Collection.Date",  "ugLValue","Sampling.Point.Type","Source.Type.Code" )
As6YR3 <- As6YR3[myvars]
My6YR<-As6YR3

# Why no CWS records for Maryland?
md<-As6YR3importedR[which(As6YR3importedR$State.Code=="MD"),]
#View(md)
table(md$System.Type)
# they do not report any records for CWSs

### end



####### 2. Preliminary review of prison data #######

#Reminder of search criteria
prison <- As6YR3[grepl("PRISON", As6YR3$System.Name), ];nrow(prison) #593
correction <- As6YR3[grepl("CORRECTION", As6YR3$System.Name), ];nrow(correction) #607
juvenile <- As6YR3[grepl("JUVENILE", As6YR3$System.Name), ];nrow(juvenile) #13
detention <- As6YR3[grepl("DETENTION", As6YR3$System.Name), ];nrow(detention) #37
penitentiary<- As6YR3[grepl("PENITENTIARY", As6YR3$System.Name), ];nrow(penitentiary) # 22
jail<- As6YR3[grepl("JAIL", As6YR3$System.Name), ];nrow(jail) # 22
women<- As6YR3[grepl("WOMEN", As6YR3$System.Name), ];nrow(women) # 51

prisons<-rbind(prison,correction,juvenile,detention,penitentiary,women)
table(prisons$State.Code)
# 770 in CA; 65 in AZ; 91 in FL

describe(prisons$PWSID) # 170 distinct 
table(prisons$Source.Water.Type)
# GU GU   GW GW   SW SW  
# 6  23 933 341  33  65 

table(prisons$Source.Type.Code)
table(As6YR3$Source.Type.Code)

## How many report raw samples?
raw<-prisons[which(prisons$Source.Type.Code=="RW"),]
describe(raw$PWSID) #32 PWSIDs report raw water samples
myvars<-c("PWSID","System.Name")
raw<-raw[myvars]
raw1<-raw[!duplicated(raw$PWSID),]

## How many report finished samples?
finished<-prisons[which(prisons$Source.Type.Code=="FN"),]
describe(finished$PWSID)
myvars<-c("PWSID","System.Name")
finished<-finished[myvars]
finished1<-finished[!duplicated(finished$PWSID),]

## How many report both raw and finished?
both<-merge(finished1,raw1,by="PWSID") # 12 prison PWSIDs report both raw and finished samples:

### end
####### 3. Assigning finished vs gross yearly mean values #########

# If a PWSID reports raw samples (As6YR3$Source.Type.Code=="RW") and finished samples (As6YR3$Source.Type.Code=="FN") 
# in the same year, keep mean of finished samples if lower than mean of raw samples

## First, yearly-avearges
#Create yearly date -- 
As6YR3$YearlyDate<-format(As6YR3$Sample.Collection.Date, format="%Y")
class(As6YR3$YearlyDate) #remove last 6 characters
As6YR3$YearlyDate <- substr(As6YR3$YearlyDate, 0, 4) #only keep first 10characters in character vector (keep date, not time)
describe(As6YR3$YearlyDate)
table(As6YR3$YearlyDate)

# Create one dataset for each year, and pull CWSs with lower final vs. raw samples:
# 2006
Y06<-As6YR3[which(As6YR3$YearlyDate=="2006"),];Y06<-Y06[which(Y06$Source.Type.Code=="RW"|Y06$Source.Type.Code=="FN"),]
table(Y06$Source.Type.Code)
#        FN    RW     x 
#    0 14819  8234     0 
# Create vector for each PWSID of the average raw and average finished sample in 2006
vector.id <- c( unique(Y06$PWSID)) 
count06 <- as.data.frame(vector.id)
for ( i in unique(Y06$PWSID)){
  raw <- cbind ( mean(Y06$ugLValue[Y06$PWSID==i&Y06$Source.Type.Code=="RW"]) ) 
  fin <- cbind ( mean(Y06$ugLValue[Y06$PWSID==i&Y06$Source.Type.Code=="FN"]) ) 
  count06$rawvalue[count06$vector.id==i] <- raw
  count06$finvalue[count06$vector.id==i] <- fin
  
}
colnames(count06)[colnames(count06)=="vector.id"] <- "PWSID"
# Change NaN to "NA" values; then restrict to systems with both raw and fin available
count06$rawvalue[which(count06$rawvalue=="NaN")]<-NA
count06$finvalue[which(count06$finvalue=="NaN")]<-NA
sen06<-count06[which(!is.na(count06$rawvalue)&!is.na(count06$finvalue)),]
lower06<-sen06[which(sen06$finvalue<sen06$rawvalue),];nrow(lower06) #94 systems with lower finished values than raw values
colnames(lower06)[2] <- "rawvalue06"
colnames(lower06)[3] <- "finvalue06"


# 2007
Y07<-As6YR3[which(As6YR3$YearlyDate=="2007"),];Y07<-Y07[which(Y07$Source.Type.Code=="RW"|Y07$Source.Type.Code=="FN"),]
table(Y07$Source.Type.Code)
#      FN    RW     x 
# 0 15699  9692     0 
# Create vector for each PWSID of the average raw and average finished sample in 2007
vector.id <- c( unique(Y07$PWSID)) 
count07 <- as.data.frame(vector.id)
for ( i in unique(Y07$PWSID)){
  raw <- cbind ( mean(Y07$ugLValue[Y07$PWSID==i&Y07$Source.Type.Code=="RW"]) ) 
  fin <- cbind ( mean(Y07$ugLValue[Y07$PWSID==i&Y07$Source.Type.Code=="FN"]) ) 
  count07$rawvalue[count07$vector.id==i] <- raw
  count07$finvalue[count07$vector.id==i] <- fin
  
}
colnames(count07)[colnames(count07)=="vector.id"] <- "PWSID"
# Change NaN to "NA" values; then restrict to systems with both raw and fin available
count07$rawvalue[which(count07$rawvalue=="NaN")]<-NA
count07$finvalue[which(count07$finvalue=="NaN")]<-NA
sen07<-count07[which(!is.na(count07$rawvalue)&!is.na(count07$finvalue)),]
lower07<-sen07[which(sen07$finvalue<sen07$rawvalue),];nrow(lower07) #145 systems with lower finished values than raw values
colnames(lower07)[2] <- "rawvalue07"
colnames(lower07)[3] <- "finvalue07"
# 2008
Y08<-As6YR3[which(As6YR3$YearlyDate=="2008"),];Y08<-Y08[which(Y08$Source.Type.Code=="RW"|Y08$Source.Type.Code=="FN"),]
table(Y08$Source.Type.Code)
#      FN    RW     x 
# 0 16451  9521     0 
# Create vector for each PWSID of the average raw and average finished sample in 2008
vector.id <- c( unique(Y08$PWSID)) 
count08 <- as.data.frame(vector.id)
for ( i in unique(Y08$PWSID)){
  raw <- cbind ( mean(Y08$ugLValue[Y08$PWSID==i&Y08$Source.Type.Code=="RW"]) ) 
  fin <- cbind ( mean(Y08$ugLValue[Y08$PWSID==i&Y08$Source.Type.Code=="FN"]) ) 
  count08$rawvalue[count08$vector.id==i] <- raw
  count08$finvalue[count08$vector.id==i] <- fin
  
}
colnames(count08)[colnames(count08)=="vector.id"] <- "PWSID"
# Change NaN to "NA" values; then restrict to systems with both raw and fin available
count08$rawvalue[which(count08$rawvalue=="NaN")]<-NA
count08$finvalue[which(count08$finvalue=="NaN")]<-NA
sen08<-count08[which(!is.na(count08$rawvalue)&!is.na(count08$finvalue)),]
lower08<-sen08[which(sen08$finvalue<sen08$rawvalue),];nrow(lower08) #154 systems with lower finished values than raw values
colnames(lower08)[2] <- "rawvalue08"
colnames(lower08)[3] <- "finvalue08"

# 2009
Y09<-As6YR3[which(As6YR3$YearlyDate=="2009"),];Y09<-Y09[which(Y09$Source.Type.Code=="RW"|Y09$Source.Type.Code=="FN"),]
table(Y09$Source.Type.Code)
#      FN    RW     x 
# 0 20197  9578     0
# Create vector for each PWSID of the average raw and average finished sample in 2009
vector.id <- c( unique(Y09$PWSID)) 
count09 <- as.data.frame(vector.id)
for ( i in unique(Y09$PWSID)){
  raw <- cbind ( mean(Y09$ugLValue[Y09$PWSID==i&Y09$Source.Type.Code=="RW"]) ) 
  fin <- cbind ( mean(Y09$ugLValue[Y09$PWSID==i&Y09$Source.Type.Code=="FN"]) ) 
  count09$rawvalue[count09$vector.id==i] <- raw
  count09$finvalue[count09$vector.id==i] <- fin
  
}
colnames(count09)[colnames(count09)=="vector.id"] <- "PWSID"
# Change NaN to "NA" values; then restrict to systems with both raw and fin available
count09$rawvalue[which(count09$rawvalue=="NaN")]<-NA
count09$finvalue[which(count09$finvalue=="NaN")]<-NA
sen09<-count09[which(!is.na(count09$rawvalue)&!is.na(count09$finvalue)),]
lower09<-sen09[which(sen09$finvalue<sen09$rawvalue),];nrow(lower09) #187 systems with lower finished values than raw values
colnames(lower09)[2] <- "rawvalue09"
colnames(lower09)[3] <- "finvalue09"
# 2010
Y10<-As6YR3[which(As6YR3$YearlyDate=="2010"),];Y10<-Y10[which(Y10$Source.Type.Code=="RW"|Y10$Source.Type.Code=="FN"),]
table(Y10$Source.Type.Code)
# 
#      FN    RW     x 
# 0 20322 10287     0 
# Create vector for each PWSID of the average raw and average finished sample in 2010
vector.id <- c( unique(Y10$PWSID)) 
count10 <- as.data.frame(vector.id)
for ( i in unique(Y10$PWSID)){
  raw <- cbind ( mean(Y10$ugLValue[Y10$PWSID==i&Y10$Source.Type.Code=="RW"]) ) 
  fin <- cbind ( mean(Y10$ugLValue[Y10$PWSID==i&Y10$Source.Type.Code=="FN"]) ) 
  count10$rawvalue[count10$vector.id==i] <- raw
  count10$finvalue[count10$vector.id==i] <- fin
  
}
colnames(count10)[colnames(count10)=="vector.id"] <- "PWSID"
# Change NaN to "NA" values; then restrict to systems with both raw and fin available
count10$rawvalue[which(count10$rawvalue=="NaN")]<-NA
count10$finvalue[which(count10$finvalue=="NaN")]<-NA
sen10<-count10[which(!is.na(count10$rawvalue)&!is.na(count10$finvalue)),]
lower10<-sen10[which(sen10$finvalue<sen10$rawvalue),];nrow(lower10) #154  systems with lower finished values than raw values
colnames(lower10)[2] <- "rawvalue10"
colnames(lower10)[3] <- "finvalue10"
# 2011
Y11<-As6YR3[which(As6YR3$YearlyDate=="2011"),];Y11<-Y11[which(Y11$Source.Type.Code=="RW"|Y11$Source.Type.Code=="FN"),]
table(Y11$Source.Type.Code)
#        FN    RW     x 
#    0 20642 10509     0
# Create vector for each PWSID of the average raw and average finished sample in 2011
vector.id <- c( unique(Y11$PWSID)) 
count11 <- as.data.frame(vector.id)
for ( i in unique(Y11$PWSID)){
  raw <- cbind ( mean(Y11$ugLValue[Y11$PWSID==i&Y11$Source.Type.Code=="RW"]) ) 
  fin <- cbind ( mean(Y11$ugLValue[Y11$PWSID==i&Y11$Source.Type.Code=="FN"]) ) 
  count11$rawvalue[count11$vector.id==i] <- raw
  count11$finvalue[count11$vector.id==i] <- fin
  
}
colnames(count11)[colnames(count11)=="vector.id"] <- "PWSID"
# Change NaN to "NA" values; then restrict to systems with both raw and fin available
count11$rawvalue[which(count11$rawvalue=="NaN")]<-NA
count11$finvalue[which(count11$finvalue=="NaN")]<-NA
sen11<-count11[which(!is.na(count11$rawvalue)&!is.na(count11$finvalue)),]
lower11<-sen11[which(sen11$finvalue<sen11$rawvalue),];nrow(lower11) # 153 systems with lower finished values than raw values
colnames(lower11)[2] <- "rawvalue11"
colnames(lower11)[3] <- "finvalue11"

# Save these new yearly means to replace yearly averages calculated below:
lower06$Flag06<-1
lower07$Flag07<-1
lower08$Flag08<-1
lower09$Flag09<-1
lower10$Flag10<-1
lower11$Flag11<-1


# Absolute difference between raw and finished samples
lower06$diff<-lower06$rawvalue06-lower06$finvalue06
lower07$diff<-lower07$rawvalue07-lower07$finvalue07
lower08$diff<-lower08$rawvalue08-lower08$finvalue08
lower09$diff<-lower09$rawvalue09-lower09$finvalue09
lower10$diff<-lower10$rawvalue10-lower10$finvalue10
lower11$diff<-lower11$rawvalue11-lower11$finvalue11

# Ignore those with a difference of 1 ug/L or less 
nrow(lower06);lower06<-lower06[which(lower06$diff>1),];nrow(lower06) #46
nrow(lower07);lower07<-lower07[which(lower07$diff>1),];nrow(lower07) #70
nrow(lower08);lower08<-lower08[which(lower08$diff>1),];nrow(lower08) #73
nrow(lower09);lower09<-lower09[which(lower09$diff>1),];nrow(lower09) #91
nrow(lower10);lower10<-lower10[which(lower10$diff>1),];nrow(lower10) #69
nrow(lower11);lower11<-lower11[which(lower11$diff>1),];nrow(lower11) #71


## Identify the number of raw vs finished samples for these PWSIDs
# 2006
vector.id <- c( unique(Y06$PWSID)) 
count06 <- as.data.frame(vector.id)
for ( i in unique(Y06$PWSID)){
  rawn <- nrow(Y06[which(Y06$PWSID==i&Y06$Source.Type.Code=="RW"),])
  finn <- nrow(Y06[which(Y06$PWSID==i&Y06$Source.Type.Code=="FN"),])
  count06$rawn[count06$vector.id==i] <- rawn
  count06$finn[count06$vector.id==i] <- finn
}
colnames(count06)[colnames(count06)=="vector.id"] <- "PWSID"

# 2007
vector.id <- c( unique(Y07$PWSID)) 
count07 <- as.data.frame(vector.id)
for ( i in unique(Y07$PWSID)){
  rawn <- nrow(Y07[which(Y07$PWSID==i&Y07$Source.Type.Code=="RW"),])
  finn <- nrow(Y07[which(Y07$PWSID==i&Y07$Source.Type.Code=="FN"),])
  count07$rawn[count07$vector.id==i] <- rawn
  count07$finn[count07$vector.id==i] <- finn
}
colnames(count07)[colnames(count07)=="vector.id"] <- "PWSID"

# 2008
vector.id <- c( unique(Y08$PWSID)) 
count08 <- as.data.frame(vector.id)
for ( i in unique(Y08$PWSID)){
  rawn <- nrow(Y08[which(Y08$PWSID==i&Y08$Source.Type.Code=="RW"),])
  finn <- nrow(Y08[which(Y08$PWSID==i&Y08$Source.Type.Code=="FN"),])
  count08$rawn[count08$vector.id==i] <- rawn
  count08$finn[count08$vector.id==i] <- finn
}
colnames(count08)[colnames(count08)=="vector.id"] <- "PWSID"

# 2009
vector.id <- c( unique(Y09$PWSID)) 
count09 <- as.data.frame(vector.id)
for ( i in unique(Y09$PWSID)){
  rawn <- nrow(Y09[which(Y09$PWSID==i&Y09$Source.Type.Code=="RW"),])
  finn <- nrow(Y09[which(Y09$PWSID==i&Y09$Source.Type.Code=="FN"),])
  count09$rawn[count09$vector.id==i] <- rawn
  count09$finn[count09$vector.id==i] <- finn
}
colnames(count09)[colnames(count09)=="vector.id"] <- "PWSID"

# 2010
vector.id <- c( unique(Y10$PWSID)) 
count10 <- as.data.frame(vector.id)
for ( i in unique(Y10$PWSID)){
  rawn <- nrow(Y10[which(Y10$PWSID==i&Y10$Source.Type.Code=="RW"),])
  finn <- nrow(Y10[which(Y10$PWSID==i&Y10$Source.Type.Code=="FN"),])
  count10$rawn[count10$vector.id==i] <- rawn
  count10$finn[count10$vector.id==i] <- finn
}
colnames(count10)[colnames(count10)=="vector.id"] <- "PWSID"

# 2011
vector.id <- c( unique(Y11$PWSID)) 
count11 <- as.data.frame(vector.id)
for ( i in unique(Y11$PWSID)){
  rawn <- nrow(Y11[which(Y11$PWSID==i&Y11$Source.Type.Code=="RW"),])
  finn <- nrow(Y11[which(Y11$PWSID==i&Y11$Source.Type.Code=="FN"),])
  count11$rawn[count11$vector.id==i] <- rawn
  count11$finn[count11$vector.id==i] <- finn
}
colnames(count11)[colnames(count11)=="vector.id"] <- "PWSID"

# Merge these numbers back with lower063 and lower073
lower06<-merge(lower06, count06,by="PWSID",all.x=T)
lower07<-merge(lower07, count07,by="PWSID",all.x=T)
lower08<-merge(lower08, count08,by="PWSID",all.x=T)
lower09<-merge(lower09, count09,by="PWSID",all.x=T)
lower10<-merge(lower10, count10,by="PWSID",all.x=T)
lower11<-merge(lower11, count11,by="PWSID",all.x=T)

## Which of these are prisons?
prisons$IsPrison<-1
myvars<-c("PWSID","IsPrison")
tm<-prisons[myvars]
tm<-tm[!duplicated(tm$PWSID),]

lower06<-merge(lower06,tm,by="PWSID",all.x=T)
lower07<-merge(lower07,tm,by="PWSID",all.x=T)
lower08<-merge(lower08,tm,by="PWSID",all.x=T)
lower09<-merge(lower09,tm,by="PWSID",all.x=T)
lower10<-merge(lower10,tm,by="PWSID",all.x=T)
lower11<-merge(lower11,tm,by="PWSID",all.x=T)
table(lower06$IsPrison) #0
table(lower07$IsPrison) #1:  CA3310802: difference: 32.928596
table(lower08$IsPrison) #1:  CA1805004: difference: 1.828961
table(lower09$IsPrison) #0
table(lower10$IsPrison) #0
table(lower11$IsPrison) #1:  CA1510802: difference: 1.933333

# By year, how many CWSs report both raw and finihsed sampled?
checking<-rbind(sen06,sen07,sen08,sen09,sen10,sen11)
describe(checking$PWSID) # 1,182 distinct PWSIDs reporting both raw and finished samples 

# for a given year
myvars<-c("PWSID","diff")
l6<-lower06[myvars];l7<-lower07[myvars];l8<-lower08[myvars];l9<-lower09[myvars];l10<-lower10[myvars];l11<-lower11[myvars]
checkagain<-rbind(l6,l7,l8,l9,l10,l11)
cc<-checkagain[!duplicated(checkagain$PWSID),];describe(cc$PWSID) #N=294

## Next, 3-yearly avearges:
#Create three-year date
As6YR3$ThreeYearlyDate<-NA
As6YR3$ThreeYearlyDate[(As6YR3$YearlyDate==2006)]<-1
As6YR3$ThreeYearlyDate[(As6YR3$YearlyDate==2007)]<-1
As6YR3$ThreeYearlyDate[(As6YR3$YearlyDate==2008)]<-1
As6YR3$ThreeYearlyDate[(As6YR3$YearlyDate==2009)]<-2
As6YR3$ThreeYearlyDate[(As6YR3$YearlyDate==2010)]<-2
As6YR3$ThreeYearlyDate[(As6YR3$YearlyDate==2011)]<-2
table(As6YR3$ThreeYearlyDate)
describe(As6YR3$ThreeYearlyDate)

# Create one dataset for each 3-year period , and pull CWSs with lower final vs. raw samples:
# Time period 1
Y063<-As6YR3[which(As6YR3$ThreeYearlyDate==1),];Y063<-Y063[which(Y063$Source.Type.Code=="RW"|Y063$Source.Type.Code=="FN"),]
table(Y063$Source.Type.Code)
#          FN    RW     x 
#     0 46969 27447     0   
# Create vector for each PWSID of the average raw and average finished sample in first time period
vector.id <- c( unique(Y063$PWSID)) 
count06 <- as.data.frame(vector.id)
for ( i in unique(Y063$PWSID)){
  raw <- cbind ( mean(Y063$ugLValue[Y063$PWSID==i&Y063$Source.Type.Code=="RW"]) ) 
  fin <- cbind ( mean(Y063$ugLValue[Y063$PWSID==i&Y063$Source.Type.Code=="FN"]) ) 
  count06$rawvalue[count06$vector.id==i] <- raw
  count06$finvalue[count06$vector.id==i] <- fin
  
}
colnames(count06)[colnames(count06)=="vector.id"] <- "PWSID"
# Change NaN to "NA" values; then restrict to systems with both raw and fin available
count06$rawvalue[which(count06$rawvalue=="NaN")]<-NA
count06$finvalue[which(count06$finvalue=="NaN")]<-NA
sen06<-count06[which(!is.na(count06$rawvalue)&!is.na(count06$finvalue)),]
lower063<-sen06[which(sen06$finvalue<sen06$rawvalue),];nrow(lower063) #343 systems with lower finished values than raw values
colnames(lower063)[2] <- "rawvaluetime1"
colnames(lower063)[3] <- "finvaluetime1"

# Time period 2
Y073<-As6YR3[which(As6YR3$ThreeYearlyDate==2),];Y073<-Y073[which(Y073$Source.Type.Code=="RW"|Y073$Source.Type.Code=="FN"),]
table(Y073$Source.Type.Code)
#         FN    RW     x 
#    0 61161 30374     0 
# Create vector for each PWSID of the average raw and average finished sample in second time period
vector.id <- c( unique(Y073$PWSID)) 
count07 <- as.data.frame(vector.id)
for ( i in unique(Y073$PWSID)){
  raw <- cbind ( mean(Y073$ugLValue[Y073$PWSID==i&Y073$Source.Type.Code=="RW"]) ) 
  fin <- cbind ( mean(Y073$ugLValue[Y073$PWSID==i&Y073$Source.Type.Code=="FN"]) ) 
  count07$rawvalue[count07$vector.id==i] <- raw
  count07$finvalue[count07$vector.id==i] <- fin
  
}
colnames(count07)[colnames(count07)=="vector.id"] <- "PWSID"
# Change NaN to "NA" values; then restrict to systems with both raw and fin available
count07$rawvalue[which(count07$rawvalue=="NaN")]<-NA
count07$finvalue[which(count07$finvalue=="NaN")]<-NA
sen07<-count07[which(!is.na(count07$rawvalue)&!is.na(count07$finvalue)),]
lower073<-sen07[which(sen07$finvalue<sen07$rawvalue),];nrow(lower073) #407 systems with lower finished values than raw values
colnames(lower073)[2] <- "rawvaluetime2"
colnames(lower073)[3] <- "finvaluetime2"

# Save these new yearly means to replace yearly averages calculated below:
lower063$Flagtime1<-1
lower073$Flagtime2<-1

# Absolute difference between raw and finished samples
lower063$diff<-lower063$rawvaluetime1-lower063$finvaluetime1
lower073$diff<-lower073$rawvaluetime2-lower073$finvaluetime2

# Ignore those with a difference of 1 ug/L or less 
nrow(lower063);lower063<-lower063[which(lower063$diff>1),];nrow(lower063) #181
nrow(lower073);lower073<-lower073[which(lower073$diff>1),];nrow(lower073) #206

## Identify the number of raw vs finished samples for these PWSIDs
# First time period
vector.id <- c( unique(Y063$PWSID)) 
count <- as.data.frame(vector.id)
for ( i in unique(Y063$PWSID)){
  rawn <- nrow(Y063[which(Y063$PWSID==i&Y063$Source.Type.Code=="RW"),])
  finn <- nrow(Y063[which(Y063$PWSID==i&Y063$Source.Type.Code=="FN"),])
  count$rawn[count$vector.id==i] <- rawn
  count$finn[count$vector.id==i] <- finn
}
colnames(count)[colnames(count)=="vector.id"] <- "PWSID"

# Second time period
vector.id <- c( unique(Y073$PWSID)) 
count2 <- as.data.frame(vector.id)
for ( i in unique(Y073$PWSID)){
  rawn <- nrow(Y073[which(Y073$PWSID==i&Y073$Source.Type.Code=="RW"),])
  finn <- nrow(Y073[which(Y073$PWSID==i&Y073$Source.Type.Code=="FN"),])
  count2$rawn[count2$vector.id==i] <- rawn
  count2$finn[count2$vector.id==i] <- finn
}
colnames(count2)[colnames(count2)=="vector.id"] <- "PWSID"

# Merge these numbers back with lower063 and lower073
lower063<-merge(lower063, count,by="PWSID",all.x=T)
lower073<-merge(lower073, count2,by="PWSID",all.x=T)

## Which of these are prisons?
prisons$IsPrison<-1
myvars<-c("PWSID","IsPrison")
tm<-prisons[myvars]
tm<-tm[!duplicated(tm$PWSID),]

lower063<-merge(lower063,tm,by="PWSID",all.x=T)
lower073<-merge(lower073,tm,by="PWSID",all.x=T)

table(lower063$IsPrison) #2: CA1805004 and CA3310802, same two above in yearly data
table(lower073$IsPrison) #0

## For six-year averages, use the yearly averages corrected 


### end

#######     3.A. Create yearly, 3-year, and 6-year averages, save  ######
### Create yearly and 3-year averages for each CWS, replacing the mean with the finished mean for these systems above^
## First for yearly averages:
t6yr<-My6YR[c("PWSID","Sample.Collection.Date","ugLValue","System.Name","State.Code","System.Type","Retail.Population.Served", "Adjusted.Total.Population.Served",
              "Source.Water.Type", "Sample.ID")]
testing<-t6yr[!duplicated(t6yr[1:2]),] #One row for each sampling day within each PWSID; 150,176

#Create yearly date -- 
My6YR$YearlyDate<-format(My6YR$Sample.Collection.Date, format="%Y")
class(My6YR$YearlyDate) #remove last 6 characters
My6YR$YearlyDate <- substr(My6YR$YearlyDate, 0, 4) #only keep first 10characters in character vector (keep date, not time)
describe(My6YR$YearlyDate)
table(My6YR$YearlyDate)
#Create yearly average - these are biased by sampling protocol / reporting 
My6YR$Yearly <- NA
YearlyAverage <- aggregate(x = My6YR["ugLValue"],
                           by = My6YR[c("PWSID", "YearlyDate")],
                           FUN = "mean")
YearlyAverage
colnames(YearlyAverage)[colnames(YearlyAverage)=="ugLValue"] <- "YearlyAverage"

#Create Yearly database
t6yr<-My6YR[c("PWSID","YearlyDate", "Sample.Collection.Date","ugLValue","System.Name","State.Code","System.Type","Retail.Population.Served","Adjusted.Total.Population.Served",
              "Source.Water.Type", "Sample.ID")]
testing<-t6yr[!duplicated(t6yr[1:2]),] #One row for each sampling year within each PWSID; N= 90,290

# Save yearly averages in "YearlyValues"
YearlyValues<-merge(testing, YearlyAverage, by=c("PWSID", "YearlyDate")); dim(YearlyValues) #[1] 90290    12

## START HERE: maybe rename columns in 06,07, etc so they are not identical for merge
# Merge flags and correct yearly averages back in from above
# Rename each uniquely prior to merge
colnames(lower06)[colnames(lower06)=="diff"] <- "diff06"
colnames(lower07)[colnames(lower07)=="diff"] <- "diff07"
colnames(lower08)[colnames(lower08)=="diff"] <- "diff08"
colnames(lower09)[colnames(lower09)=="diff"] <- "diff09"
colnames(lower10)[colnames(lower10)=="diff"] <- "diff10"
colnames(lower11)[colnames(lower11)=="diff"] <- "diff11"
colnames(lower06)[colnames(lower06)=="rawn"] <- "rawn06"
colnames(lower07)[colnames(lower07)=="rawn"] <- "rawn07"
colnames(lower08)[colnames(lower08)=="rawn"] <- "rawn08"
colnames(lower09)[colnames(lower09)=="rawn"] <- "rawn09"
colnames(lower10)[colnames(lower10)=="rawn"] <- "rawn10"
colnames(lower11)[colnames(lower11)=="rawn"] <- "rawn11"
colnames(lower06)[colnames(lower06)=="finn"] <- "finn06"
colnames(lower07)[colnames(lower07)=="finn"] <- "finn07"
colnames(lower08)[colnames(lower08)=="finn"] <- "finn08"
colnames(lower09)[colnames(lower09)=="finn"] <- "finn09"
colnames(lower10)[colnames(lower10)=="finn"] <- "finn10"
colnames(lower11)[colnames(lower11)=="finn"] <- "finn11"
lower06$IsPrison<-NULL
lower07$IsPrison<-NULL
lower08$IsPrison<-NULL
lower09$IsPrison<-NULL
lower10$IsPrison<-NULL
lower11$IsPrison<-NULL

corrected<-merge(YearlyValues,lower06,by="PWSID",all=T)
corrected<-merge(corrected,lower07,by="PWSID",all=T)
corrected<-merge(corrected,lower08,by="PWSID",all=T)
corrected<-merge(corrected,lower09,by="PWSID",all=T)
corrected<-merge(corrected,lower10,by="PWSID",all=T)
corrected<-merge(corrected,lower11,by="PWSID",all=T)

## Replace yearly average with corrected value
# 2006
corrected$YearlyAverage[which(corrected$YearlyDate=="2006"&corrected$PWSID=="NV0000167")] #[1] 30.7
describe(corrected$YearlyAverage[which(corrected$YearlyDate=="2006")]) 
#corrected$YearlyAverage[which(corrected$YearlyDate == "2006")] 
#     n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95 
# 16584        0     1921    0.752    2.168    3.138    0.350    0.350    0.350    0.350    1.414    5.700    9.338 

#lowest :   0.1000   0.1600   0.1610   0.1650   0.1670, highest: 133.7794 147.0000 158.0000 220.0000 233.8727
corrected$YearlyAverage[which(corrected$YearlyDate=="2006"&corrected$Flag06==1)]<-corrected$finvalue06[which(corrected$YearlyDate=="2006"&corrected$Flag06==1)]
corrected$YearlyAverage[which(corrected$YearlyDate=="2006"&corrected$PWSID=="NV0000167")] #[1] 5.4
describe(corrected$YearlyAverage[which(corrected$YearlyDate=="2006")]) 
#corrected$YearlyAverage[which(corrected$YearlyDate == "2006")] 
#     n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95 
# 16584        0     1869     0.75    2.159    3.126    0.350    0.350    0.350    0.350    1.414    5.679    9.309 
#
# lowest :   0.1000   0.1600   0.1610   0.1650   0.1670, highest: 133.7794 147.0000 158.0000 220.0000 233.8727

# 2007
describe(corrected$YearlyAverage[which(corrected$YearlyDate=="2007")]) 
corrected$YearlyAverage[which(corrected$YearlyDate=="2007"&corrected$Flag07==1)]<-corrected$finvalue07[which(corrected$YearlyDate=="2007"&corrected$Flag07==1)]
# 2008
describe(corrected$YearlyAverage[which(corrected$YearlyDate=="2008")]) 
corrected$YearlyAverage[which(corrected$YearlyDate=="2008"&corrected$Flag08==1)]<-corrected$finvalue08[which(corrected$YearlyDate=="2008"&corrected$Flag08==1)]
# 2009
describe(corrected$YearlyAverage[which(corrected$YearlyDate=="2009")]) 
corrected$YearlyAverage[which(corrected$YearlyDate=="2009"&corrected$Flag09==1)]<-corrected$finvalue09[which(corrected$YearlyDate=="2009"&corrected$Flag09==1)]
# 2010
describe(corrected$YearlyAverage[which(corrected$YearlyDate=="2010")]) 
corrected$YearlyAverage[which(corrected$YearlyDate=="2010"&corrected$Flag10==1)]<-corrected$finvalue10[which(corrected$YearlyDate=="2010"&corrected$Flag10==1)]
# 2011
describe(corrected$YearlyAverage[which(corrected$YearlyDate=="2011")]) 
corrected$YearlyAverage[which(corrected$YearlyDate=="2011"&corrected$Flag11==1)]<-corrected$finvalue11[which(corrected$YearlyDate=="2011"&corrected$Flag11==1)]

## Next, for 3-year averages
My6YR$ThreeYearlyDate<-NA
My6YR$ThreeYearlyDate[(My6YR$YearlyDate==2006)]<-1
My6YR$ThreeYearlyDate[(My6YR$YearlyDate==2007)]<-1
My6YR$ThreeYearlyDate[(My6YR$YearlyDate==2008)]<-1
My6YR$ThreeYearlyDate[(My6YR$YearlyDate==2009)]<-2
My6YR$ThreeYearlyDate[(My6YR$YearlyDate==2010)]<-2
My6YR$ThreeYearlyDate[(My6YR$YearlyDate==2011)]<-2
table(My6YR$ThreeYearlyDate)
describe(My6YR$ThreeYearlyDate)

#Create 3 year average - these are what we believe are most reliable
My6YR$ThreeYearly <- NA
YearlyAverage3 <- aggregate(x = My6YR["ugLValue"],
                            by = My6YR[c("PWSID", "ThreeYearlyDate")],
                            FUN = "mean")
YearlyAverage3
colnames(YearlyAverage3)[colnames(YearlyAverage3)=="ugLValue"] <- "ThreeYearAverage"

#Create Three-year avearge database
t6yr<-My6YR[c("PWSID","ThreeYearlyDate", "Sample.Collection.Date","ugLValue","System.Name","State.Code","System.Type","Retail.Population.Served","Adjusted.Total.Population.Served",
              "Source.Water.Type", "Sample.ID")]
testing<-t6yr[!duplicated(t6yr[1:2]),] #One row for each 3-year sampling time frame within each PWSID; N= 64,425

# Save three year averages in "ThreeYearlyValues"
ThreeYearlyValues<-merge(testing, YearlyAverage3, by=c("PWSID", "ThreeYearlyDate")); dim(ThreeYearlyValues) #[1] 64425    12
# ThreeYearlyValues is the CWS-level database with 3-year averages at the CWS level

# Merge flags and correct yearly averages back in from above
corrected3<-merge(ThreeYearlyValues,lower063,by="PWSID",all=T)
corrected3<-merge(corrected3,lower073,by="PWSID",all=T)

## Replace yearly average with corrected value
# Time period 1
corrected3$ThreeYearAverage[which(corrected3$ThreeYearlyDate==1&corrected3$PWSID=="CA2100579")] #[1] 31.2857
describe(corrected3$ThreeYearAverage[which(corrected3$ThreeYearlyDate==1)]) 
#corrected3$ThreeYearAverage[which(corrected3$ThreeYearlyDate ==      1)] 
#   n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95 
#31395        0     4101    0.786    1.937    2.682    0.350    0.350    0.350    0.350    1.500    5.000    8.029 
#
#lowest :   0.04949746   0.07071066   0.10000000   0.13000000   0.16000000, highest:  97.42222222 134.66666667 141.81818182 286.66666667 453.94782609
corrected3$ThreeYearAverage[which(corrected3$ThreeYearlyDate==1&corrected3$Flagtime1==1)]<-corrected3$finvaluetime1[which(corrected3$ThreeYearlyDate==1&corrected3$Flagtime1==1)]
corrected3$ThreeYearAverage[which(corrected3$ThreeYearlyDate==1&corrected3$PWSID=="CA2100579")] #[1] 3.575489
describe(corrected3$ThreeYearAverage[which(corrected3$ThreeYearlyDate==1)]) 
#corrected3$ThreeYearAverage[which(corrected3$ThreeYearlyDate ==      1)] 
#    n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95 
#31395        0     3948    0.784    1.918    2.654     0.35     0.35     0.35     0.35     1.45     5.00     8.00 
#
#lowest :   0.04949746   0.07071066   0.10000000   0.13000000   0.16000000, highest:  97.42222222 134.66666667 141.81818182 286.66666667 453.94782609

# Time period 2
describe(corrected3$ThreeYearAverage[which(corrected3$ThreeYearlyDate==2)]) 
corrected3$ThreeYearAverage[which(corrected3$ThreeYearlyDate==2&corrected3$Flagtime2==1)]<-corrected3$finvaluetime2[which(corrected3$ThreeYearlyDate==2&corrected3$Flagtime2==1)]

# The databases corrected (yearly averages) and corrected3 (3-year averages) now have these corrected values for analysis

## Next, for six-year average:
# Use "corrected" which has yearly values for each PWSID corrected for source water type
corrected$SixYearlyDate<-1
corrected$SixYearly <- NA
YearlyAverage6 <- aggregate(x = corrected["YearlyAverage"],
                            by = corrected[c("PWSID", "SixYearlyDate")],
                            FUN = "mean")
YearlyAverage6
colnames(YearlyAverage6)[colnames(YearlyAverage6)=="YearlyAverage"] <- "SixYearAverage"

#Create six-year avearge database
t6yr<-corrected[c("PWSID","SixYearlyDate", "Sample.Collection.Date","ugLValue","System.Name","State.Code","System.Type","Retail.Population.Served","Adjusted.Total.Population.Served",
              "Source.Water.Type", "Sample.ID")]
testing<-t6yr[!duplicated(t6yr[1:2]),] #One row for each 3-year sampling time frame within each PWSID; N= 37098

# Save six year averages in "SixYearlyValues"
SixYearlyValues<-merge(testing, YearlyAverage6, by=c("PWSID", "SixYearlyDate")); dim(SixYearlyValues) #[1] 37098


### Reformat yearly averages into wide format ##
## Corrected (yearly)
YearlyValuestestcorrected<-corrected
myvars <- c("PWSID", "YearlyDate","YearlyAverage")
YearlyValuestestcorrected <- YearlyValuestestcorrected[myvars]

wideyearlycorrected<-reshape(YearlyValuestestcorrected, idvar = "PWSID", timevar = "YearlyDate", direction = "wide")
describe(wideyearlycorrected$PWSID) #37,098 PWSIDs with yearly average values


#Reorder variables so the years are in the normal order
wideyearlycorrected <- wideyearlycorrected[c("PWSID", "YearlyAverage.2006",
                                             "YearlyAverage.2007", "YearlyAverage.2008","YearlyAverage.2009","YearlyAverage.2010","YearlyAverage.2011"    
)]

describe(My6YR$PWSID)
myvars<-c("PWSID", "System.Name", "State.Code", "System.Type","Retail.Population.Served", "Adjusted.Total.Population.Served", "Source.Water.Type")
new6yr<-My6YR[myvars]
new6yra<-new6yr[!duplicated(new6yr[1]),]
describe(new6yra$PWSID) #37,098

## Three-year averages:
ThreeYearlyValuestestcorrected<-corrected3
myvars <- c("PWSID", "ThreeYearlyDate","ThreeYearAverage")
ThreeYearlyValuestestcorrected <- ThreeYearlyValuestestcorrected[myvars]

widethreeyearlycorrected<-reshape(ThreeYearlyValuestestcorrected, idvar = "PWSID", timevar = "ThreeYearlyDate", direction = "wide")
describe(widethreeyearlycorrected$PWSID) #37,098 PWSIDs with three-year average values

#REorder variables so the years are in the normal order
widethreeyearlycorrected <- widethreeyearlycorrected[c("PWSID", "ThreeYearAverage.1", "ThreeYearAverage.2" )]

## Six-year averages:
SixYearlyValuestestcorrected<-SixYearlyValues
myvars <- c("PWSID", "SixYearlyDate","SixYearAverage")
SixYearlyValuestestcorrected <- SixYearlyValuestestcorrected[myvars]

widesixyearlycorrected<-reshape(SixYearlyValuestestcorrected, idvar = "PWSID", timevar = "SixYearlyDate", direction = "wide")
describe(widesixyearlycorrected$PWSID) #37,098 PWSIDs with six-year average values

#REorder variables so the years are in the normal order
widesixyearlycorrected <- widesixyearlycorrected[c("PWSID", "SixYearAverage.1" )]

# save all  in .csv and .xlsx
write.csv(wideyearlycorrected, file="~/Google Drive/Research/Prison Water As/wideyearlycorrected.csv")
write.xlsx(wideyearlycorrected, file="~/Google Drive/Research/Prison Water As/wideyearlycorrected.xlsx")
write.csv(widethreeyearlycorrected, file="~/Google Drive/Research/Prison Water As/widethreeyearlycorrected.csv")
write.xlsx(widethreeyearlycorrected, file="~/Google Drive/Research/Prison Water As/widethreeyearlycorrected.xlsx")
write.csv(widesixyearlycorrected, file="~/Google Drive/Research/Prison Water As/widesixyearlycorrected.csv")
write.xlsx(widesixyearlycorrected, file="~/Google Drive/Research/Prison Water As/widesixyearlycorrected.xlsx")

### end
####### 4. Create new vars needed for analysis ########
## Decide which datasets to move forward with: corrected or original?
#Start with original:
pcwsy<-wideyearlycorrected
pcwsy3<-widethreeyearlycorrected
pcwsy6<-widesixyearlycorrected

## Merge back in necessary info
myvars<-c("PWSID","System.Name","State.Code","System.Type","Retail.Population.Served","Adjusted.Total.Population.Served","Source.Water.Type","Source.Type.Code")
tomerge<-As6YR3[myvars]
tomerge<-tomerge[!duplicated(tomerge$PWSID),]
pcwsy<-merge(pcwsy,tomerge,by="PWSID",all.x=T)
pcwsy3<-merge(pcwsy3,tomerge,by="PWSID",all.x=T)
pcwsy6<-merge(pcwsy6,tomerge,by="PWSID",all.x=T)

## Evaluate missingness of state, other info
table(pcwsy$State.Code) # Remove records from American Samoa
pcwsy<-pcwsy[which(pcwsy$State.Code!="AS"),];dim(pcwsy)
pcwsy3<-pcwsy3[which(pcwsy3$State.Code!="AS"),];dim(pcwsy3)
pcwsy6<-pcwsy6[which(pcwsy6$State.Code!="AS"),];dim(pcwsy6)
table(pcwsy$State.Code)
# PWSID NN9935002 is missing State.Code; remove (can't find State info in SDWIS); remove from analysis
pcwsy<-pcwsy[which(pcwsy$PWSID!="NN9935002"),];dim(pcwsy)
pcwsy3<-pcwsy3[which(pcwsy3$PWSID!="NN9935002"),];dim(pcwsy3)
pcwsy6<-pcwsy6[which(pcwsy6$PWSID!="NN9935002"),];dim(pcwsy6)

# Final N = 37,086 CWSs
# How many As records total for our final N of 37,086?
myvars<-c("PWSID")
findat<-pcwsy6[myvars]
cAs6YR<-merge(findat,My6YR,by="PWSID",all.x=T)
describe(cAs6YR$PWSID) #37,086 PWSIDs; total of 230,158 records

## US Region
# Based on Ayotte et al 2017 (Environ Sci Technology), in part
# Also based on US Census Bureau Regions and Divisions
table(pcwsy$State.Code)
pcwsy$Region<-"None"
pcwsy$Region[which(pcwsy$State.Code=="WA"|pcwsy$State.Code=="OR"|pcwsy$State.Code=="ID"|
                     pcwsy$State.Code=="MT"|pcwsy$State.Code=="WY")]<-"PNW" #PacificNW/Moutnain
pcwsy$Region[which(pcwsy$State.Code=="CA"|pcwsy$State.Code=="NV"|pcwsy$State.Code=="UT"|
                     pcwsy$State.Code=="CO"|pcwsy$State.Code=="AZ"|pcwsy$State.Code=="NM"|
                     pcwsy$State.Code=="TX")]<-"SW"#Southwest/Moutnain
pcwsy$Region[which(pcwsy$State.Code=="ND"|pcwsy$State.Code=="SD"|pcwsy$State.Code=="NE"|
                     pcwsy$State.Code=="KS"|pcwsy$State.Code=="MO")]<-"CMW" #CentralMW
pcwsy$Region[which(pcwsy$State.Code=="WI"|pcwsy$State.Code=="IL"|pcwsy$State.Code=="IN"|
                     pcwsy$State.Code=="MI"|pcwsy$State.Code=="OH"|pcwsy$State.Code=="MN"|
                     pcwsy$State.Code=="IA")]<-"EMW" #EasternMW
pcwsy$Region[which(pcwsy$State.Code=="OK"|pcwsy$State.Code=="AR"|pcwsy$State.Code=="LA"|
                     pcwsy$State.Code=="MS"|pcwsy$State.Code=="AL"|pcwsy$State.Code=="FL"|
                     pcwsy$State.Code=="GA"|pcwsy$State.Code=="TN"|pcwsy$State.Code=="KY"|
                     pcwsy$State.Code=="SC"|pcwsy$State.Code=="NC"|pcwsy$State.Code=="VA"|
                     pcwsy$State.Code=="WV")]<-"SE" #Southeast
pcwsy$Region[which(pcwsy$State.Code=="PA"|pcwsy$State.Code=="MD"|pcwsy$State.Code=="DC"
                   |pcwsy$State.Code=="DE"|pcwsy$State.Code=="NY"|pcwsy$State.Code=="NJ"|
                     pcwsy$State.Code=="CT"|pcwsy$State.Code=="RI")]<-"MA" #MidAtlantic
pcwsy$Region[which(pcwsy$State.Code=="MA"|pcwsy$State.Code=="VT"|pcwsy$State.Code=="NH"|
                     pcwsy$State.Code=="ME")]<-"NE"#New England
pcwsy$Region[which(pcwsy$State.Code=="HI"|pcwsy$State.Code=="AK")]<-"AKHI" #AK and HI
table(pcwsy$Region)
#AKHI  CMW  EMW   MA   NE  PNW   SE   SW 
# 489 2671 6098 4929 1745 4488 7916 8750

pcwsy3$Region<-"None"
pcwsy3$Region[which(pcwsy3$State.Code=="WA"|pcwsy3$State.Code=="OR"|pcwsy3$State.Code=="ID"|
                      pcwsy3$State.Code=="MT"|pcwsy3$State.Code=="WY")]<-"PNW" #PacificNW/Moutnain
pcwsy3$Region[which(pcwsy3$State.Code=="CA"|pcwsy3$State.Code=="NV"|pcwsy3$State.Code=="UT"|
                      pcwsy3$State.Code=="CO"|pcwsy3$State.Code=="AZ"|pcwsy3$State.Code=="NM"|
                      pcwsy3$State.Code=="TX")]<-"SW"#Southwest/Moutnain
pcwsy3$Region[which(pcwsy3$State.Code=="ND"|pcwsy3$State.Code=="SD"|pcwsy3$State.Code=="NE"|
                      pcwsy3$State.Code=="KS"|pcwsy3$State.Code=="MO")]<-"CMW" #CentralMW
pcwsy3$Region[which(pcwsy3$State.Code=="WI"|pcwsy3$State.Code=="IL"|pcwsy3$State.Code=="IN"|
                      pcwsy3$State.Code=="MI"|pcwsy3$State.Code=="OH"|pcwsy3$State.Code=="MN"|
                      pcwsy3$State.Code=="IA")]<-"EMW" #EasternMW
pcwsy3$Region[which(pcwsy3$State.Code=="OK"|pcwsy3$State.Code=="AR"|pcwsy3$State.Code=="LA"|
                      pcwsy3$State.Code=="MS"|pcwsy3$State.Code=="AL"|pcwsy3$State.Code=="FL"|
                      pcwsy3$State.Code=="GA"|pcwsy3$State.Code=="TN"|pcwsy3$State.Code=="KY"|
                      pcwsy3$State.Code=="SC"|pcwsy3$State.Code=="NC"|pcwsy3$State.Code=="VA"|
                      pcwsy3$State.Code=="WV")]<-"SE" #Southeast
pcwsy3$Region[which(pcwsy3$State.Code=="PA"|pcwsy3$State.Code=="MD"|pcwsy3$State.Code=="DC"
                    |pcwsy3$State.Code=="DE"|pcwsy3$State.Code=="NY"|pcwsy3$State.Code=="NJ"|
                      pcwsy3$State.Code=="CT"|pcwsy3$State.Code=="RI")]<-"MA" #MidAtlantic
pcwsy3$Region[which(pcwsy3$State.Code=="MA"|pcwsy3$State.Code=="VT"|pcwsy3$State.Code=="NH"|
                      pcwsy3$State.Code=="ME")]<-"NE"#New England
pcwsy3$Region[which(pcwsy3$State.Code=="HI"|pcwsy3$State.Code=="AK")]<-"AKHI" #AK and HI
table(pcwsy3$Region)
#AKHI  CMW  EMW   MA   NE  PNW   SE   SW 
# 489 2671 6098 4929 1745 4488 7916 8750 

pcwsy6$Region<-"None"
pcwsy6$Region[which(pcwsy6$State.Code=="WA"|pcwsy6$State.Code=="OR"|pcwsy6$State.Code=="ID"|
                      pcwsy6$State.Code=="MT"|pcwsy6$State.Code=="WY")]<-"PNW" #PacificNW/Moutnain
pcwsy6$Region[which(pcwsy6$State.Code=="CA"|pcwsy6$State.Code=="NV"|pcwsy6$State.Code=="UT"|
                      pcwsy6$State.Code=="CO"|pcwsy6$State.Code=="AZ"|pcwsy6$State.Code=="NM"|
                      pcwsy6$State.Code=="TX")]<-"SW"#Southwest/Moutnain
pcwsy6$Region[which(pcwsy6$State.Code=="ND"|pcwsy6$State.Code=="SD"|pcwsy6$State.Code=="NE"|
                      pcwsy6$State.Code=="KS"|pcwsy6$State.Code=="MO")]<-"CMW" #CentralMW
pcwsy6$Region[which(pcwsy6$State.Code=="WI"|pcwsy6$State.Code=="IL"|pcwsy6$State.Code=="IN"|
                      pcwsy6$State.Code=="MI"|pcwsy6$State.Code=="OH"|pcwsy6$State.Code=="MN"|
                      pcwsy6$State.Code=="IA")]<-"EMW" #EasternMW
pcwsy6$Region[which(pcwsy6$State.Code=="OK"|pcwsy6$State.Code=="AR"|pcwsy6$State.Code=="LA"|
                      pcwsy6$State.Code=="MS"|pcwsy6$State.Code=="AL"|pcwsy6$State.Code=="FL"|
                      pcwsy6$State.Code=="GA"|pcwsy6$State.Code=="TN"|pcwsy6$State.Code=="KY"|
                      pcwsy6$State.Code=="SC"|pcwsy6$State.Code=="NC"|pcwsy6$State.Code=="VA"|
                      pcwsy6$State.Code=="WV")]<-"SE" #Southeast
pcwsy6$Region[which(pcwsy6$State.Code=="PA"|pcwsy6$State.Code=="MD"|pcwsy6$State.Code=="DC"
                    |pcwsy6$State.Code=="DE"|pcwsy6$State.Code=="NY"|pcwsy6$State.Code=="NJ"|
                      pcwsy6$State.Code=="CT"|pcwsy6$State.Code=="RI")]<-"MA" #MidAtlantic
pcwsy6$Region[which(pcwsy6$State.Code=="MA"|pcwsy6$State.Code=="VT"|pcwsy6$State.Code=="NH"|
                      pcwsy6$State.Code=="ME")]<-"NE"#New England
pcwsy6$Region[which(pcwsy6$State.Code=="HI"|pcwsy6$State.Code=="AK")]<-"AKHI" #AK and HI
table(pcwsy6$Region)


## Prison/jail/correctional facility etc.
# 6-year avearges:
prison <- pcwsy6[grepl("PRISON", pcwsy6$System.Name), ] #14
correction <- pcwsy6[grepl("CORRECTION", pcwsy6$System.Name), ] #125
juvenile <- pcwsy6[grepl("JUVENILE", pcwsy6$System.Name), ] #5
detention <- pcwsy6[grepl("DETENTION", pcwsy6$System.Name), ] #4
jail <- pcwsy6[grepl("JAIL", pcwsy6$System.Name), ] # 4
penitentiary<- pcwsy6[grepl("PENITENTIARY", pcwsy6$System.Name), ] # 5
women<- pcwsy6[grepl("WOMEN", pcwsy6$System.Name), ] # 5
prisons6<-rbind(prison,correction,juvenile,detention,jail,penitentiary,women);nrow(prisons6) #N =162
prisons6$IsPrison<-1
prisons6<-prisons6[!duplicated(prisons6$PWSID),];nrow(prisons6) #155

# 3-year averages:
prison <- pcwsy3[grepl("PRISON", pcwsy3$System.Name), ] #14
correction <- pcwsy3[grepl("CORRECTION", pcwsy3$System.Name), ] #125
juvenile <- pcwsy3[grepl("JUVENILE", pcwsy3$System.Name), ] #5
detention <- pcwsy3[grepl("DETENTION", pcwsy3$System.Name), ] #4
jail <- pcwsy3[grepl("JAIL", pcwsy3$System.Name), ] # 4
penitentiary<- pcwsy3[grepl("PENITENTIARY", pcwsy3$System.Name), ] # 5
women<- pcwsy3[grepl("WOMEN", pcwsy3$System.Name), ] # 5
prisons3<-rbind(prison,correction,juvenile,detention,jail,penitentiary,women) #N =157
prisons3$IsPrison<-1
prisons3<-prisons3[!duplicated(prisons3$PWSID),];nrow(prisons3)

# 1-year avearges:
prison <- pcwsy[grepl("PRISON", pcwsy$System.Name), ] #14
correction <- pcwsy[grepl("CORRECTION", pcwsy$System.Name), ] #125
juvenile <- pcwsy[grepl("JUVENILE", pcwsy$System.Name), ] #5
detention <- pcwsy[grepl("DETENTION", pcwsy$System.Name), ] #4
jail <- pcwsy[grepl("JAIL", pcwsy$System.Name), ] # 4
penitentiary<- pcwsy[grepl("PENITENTIARY", pcwsy$System.Name), ] # 5
women<- pcwsy[grepl("WOMEN", pcwsy$System.Name), ] # 5
prisons1<-rbind(prison,correction,juvenile,detention,jail,penitentiary,women) #N =157
prisons1$IsPrison<-1
prisons1<-prisons1[!duplicated(prisons1$PWSID),];nrow(prisons1) #155

myvars<-c("PWSID","IsPrison")
tomerge<-prisons6[myvars]
pcwsy6<-merge(pcwsy6,tomerge,by="PWSID",all=T)
table(pcwsy6$IsPrison)
pcwsy6$IsPrison[which(is.na(pcwsy6$IsPrison))]<-0
table(pcwsy6$IsPrison)

myvars<-c("PWSID","IsPrison")
tomerge<-prisons3[myvars]
pcwsy3<-merge(pcwsy3,tomerge,by="PWSID",all=T)
table(pcwsy3$IsPrison)
pcwsy3$IsPrison[which(is.na(pcwsy3$IsPrison))]<-0
table(pcwsy3$IsPrison)

myvars<-c("PWSID","IsPrison")
tomerge<-prisons1[myvars]
pcwsy<-merge(pcwsy,tomerge,by="PWSID",all=T)
table(pcwsy$IsPrison)
pcwsy$IsPrison[which(is.na(pcwsy$IsPrison))]<-0
table(pcwsy$IsPrison)


## Round all arsenic concentration values to 2 digits
pcwsy$YearlyAverage.2006<-as.numeric(pcwsy$YearlyAverage.2006)
pcwsy$YearlyAverage.2007<-as.numeric(pcwsy$YearlyAverage.2007)
pcwsy$YearlyAverage.2008<-as.numeric(pcwsy$YearlyAverage.2008)
pcwsy$YearlyAverage.2009<-as.numeric(pcwsy$YearlyAverage.2009)
pcwsy$YearlyAverage.2010<-as.numeric(pcwsy$YearlyAverage.2010)
pcwsy$YearlyAverage.2011<-as.numeric(pcwsy$YearlyAverage.2011)
pcwsy$YearlyAverage.2006 <-round(pcwsy$YearlyAverage.2006, digits=2)
pcwsy$YearlyAverage.2007 <-round(pcwsy$YearlyAverage.2007, digits=2)
pcwsy$YearlyAverage.2008 <-round(pcwsy$YearlyAverage.2008, digits=2)
pcwsy$YearlyAverage.2009 <-round(pcwsy$YearlyAverage.2009, digits=2)
pcwsy$YearlyAverage.2010 <-round(pcwsy$YearlyAverage.2010, digits=2)
pcwsy$YearlyAverage.2011 <-round(pcwsy$YearlyAverage.2011, digits=2)
prisons1$YearlyAverage.2006<-as.numeric(prisons1$YearlyAverage.2006)
prisons1$YearlyAverage.2007<-as.numeric(prisons1$YearlyAverage.2007)
prisons1$YearlyAverage.2008<-as.numeric(prisons1$YearlyAverage.2008)
prisons1$YearlyAverage.2009<-as.numeric(prisons1$YearlyAverage.2009)
prisons1$YearlyAverage.2010<-as.numeric(prisons1$YearlyAverage.2010)
prisons1$YearlyAverage.2011<-as.numeric(prisons1$YearlyAverage.2011)
prisons1$YearlyAverage.2006 <-round(prisons1$YearlyAverage.2006, digits=2)
prisons1$YearlyAverage.2007 <-round(prisons1$YearlyAverage.2007, digits=2)
prisons1$YearlyAverage.2008 <-round(prisons1$YearlyAverage.2008, digits=2)
prisons1$YearlyAverage.2009 <-round(prisons1$YearlyAverage.2009, digits=2)
prisons1$YearlyAverage.2010 <-round(prisons1$YearlyAverage.2010, digits=2)
prisons1$YearlyAverage.2011 <-round(prisons1$YearlyAverage.2011, digits=2)

pcwsy3$ThreeYearAverage.1<-as.numeric(pcwsy3$ThreeYearAverage.1)
pcwsy3$ThreeYearAverage.2<-as.numeric(pcwsy3$ThreeYearAverage.2)
pcwsy3$ThreeYearAverage.1 <-round(pcwsy3$ThreeYearAverage.1, digits=2)
pcwsy3$ThreeYearAverage.2 <-round(pcwsy3$ThreeYearAverage.2, digits=2)
prisons3$ThreeYearAverage.1<-as.numeric(prisons3$ThreeYearAverage.1)
prisons3$ThreeYearAverage.2<-as.numeric(prisons3$ThreeYearAverage.2)
prisons3$ThreeYearAverage.1 <-round(prisons3$ThreeYearAverage.1, digits=2)
prisons3$ThreeYearAverage.2 <-round(prisons3$ThreeYearAverage.2, digits=2)

pcwsy6$SixYearAverage.1<-as.numeric(pcwsy6$SixYearAverage.1)
pcwsy6$SixYearAverage.1 <-round(pcwsy6$SixYearAverage.1, digits=2)
prisons6$SixYearAverage.1<-as.numeric(prisons6$SixYearAverage.1)
prisons6$SixYearAverage.1 <-round(prisons6$SixYearAverage.1, digits=2)

## Rename vars for ease and clarity:
colnames(pcwsy3)[colnames(pcwsy3)=="ThreeYearAverage.1"] <- "ThreeYearAs20062008"
colnames(pcwsy3)[colnames(pcwsy3)=="ThreeYearAverage.2"] <- "ThreeYearAs20092011"
colnames(prisons3)[colnames(prisons3)=="ThreeYearAverage.1"] <- "ThreeYearAs20062008"
colnames(prisons3)[colnames(prisons3)=="ThreeYearAverage.2"] <- "ThreeYearAs20092011"
colnames(pcwsy6)[colnames(pcwsy6)=="SixYearAverage.1"] <- "SixYearAs"
colnames(prisons6)[colnames(prisons6)=="SixYearAverage.1"] <- "SixYearAs"
## Rename database for ease:
cws1<-pcwsy # yearly averages 
cws1$ThreeYearAverage.1<-NULL
cws1$ThreeYearAverage.2<-NULL
cws3<-pcwsy3 # three-year averages
cws6<-pcwsy6 # six year averages

### end
####### 5. Table 1. Distribution of 6-year avearges by prison status and region ######

## Non-Prison CWSs in the non-Southwest (N=28,204)
data1<-cws6[which(cws6$Region!="SW"&cws6$IsPrison==0),];nrow(data1)
nav<-nrow(data1[which(!is.na(data1$SixYearAs)),])
nmiss<-nrow(data1[which(is.na(data1$SixYearAs)),])
pop<-sum(data1$Adjusted.Total.Population.Served)
quant<-as.data.frame(quantile(data1$SixYearAs,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(data1[which(data1$SixYearAs<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(data1[which(data1$SixYearAs>10),])
nmcl<-round(100*(nmcl1/nav),1)
nmclfin<-to.n.prop(c(nmcl1,nmcl))
gm<-exp(mean(log(data1$SixYearAs),na.rm=T));gm<-round(gm,digits=2)
mean<-mean(data1$SixYearAs,na.rm=T);mean<-round(mean,digits=2)
colnames(quant)[1] <- "Other CWSs"
Time1<-rbind(nav,nmiss,pop,quant,nmrl,nmclfin,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[13] <- "% below MRL"
rownames(Time1)[14] <- "N (%) above MCL";rownames(Time1)[15] <- "Geometric mean";rownames(Time1)[16] <- "Arithmetic mean"
nonSWCWSs<-Time1
nonSWCWSs <- rbind(nonSWCWSs, Region= c("non-Southwest"))


## Non-Prison CWSs in the SW (N=8,727)
data2<-cws6[which(cws6$Region=="SW"&cws6$IsPrison==0),];nrow(data2)
nav<-nrow(data2[which(!is.na(data2$SixYearAs)),])
nmiss<-nrow(data2[which(is.na(data2$SixYearAs)),])
pop<-sum(data2$Adjusted.Total.Population.Served)
quant<-as.data.frame(quantile(data2$SixYearAs,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(data2[which(data2$SixYearAs<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(data2[which(data2$SixYearAs>10),])
nmcl<-round(100*(nmcl1/nav),1)
nmclfin<-to.n.prop(c(nmcl1,nmcl))
gm<-exp(mean(log(data2$SixYearAs),na.rm=T));gm<-round(gm,2)
mean<-mean(data2$SixYearAs,na.rm=T);mean<-round(mean,2)
colnames(quant)[1] <- "Other CWSs"
Time1<-rbind(nav,nmiss,pop,quant,nmrl,nmclfin,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[13] <- "% below MRL"
rownames(Time1)[14] <- " N (%) above MCL";rownames(Time1)[15] <- "Geometric mean";rownames(Time1)[16] <- "Arithmetic mean"
SWCWSs<-Time1
SWCWSs <- rbind(SWCWSs, Region= c("Southwest"))

## Prisons in the non-SW (N=132)
data3<-cws6[which(cws6$Region!="SW"&cws6$IsPrison==1),];nrow(data3)
nav<-nrow(data3[which(!is.na(data3$SixYearAs)),])
nmiss<-nrow(data3[which(is.na(data3$SixYearAs)),])
pop<-sum(data3$Adjusted.Total.Population.Served)
quant<-as.data.frame(quantile(data3$SixYearAs,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(data3[which(data3$SixYearAs<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(data3[which(data3$SixYearAs>10),])
nmcl<-round(100*(nmcl1/nav),1)
nmclfin<-to.n.prop(c(nmcl1,nmcl))
gm<-exp(mean(log(data3$SixYearAs),na.rm=T));gm<-round(gm,2)
mean<-mean(data3$SixYearAs,na.rm=T);mean<-round(mean,2)
colnames(quant)[1] <- "Correctional facilities"
Time1<-rbind(nav,nmiss,pop,quant,nmrl,nmclfin,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[13] <- "% below MRL"
rownames(Time1)[14] <- "N (%) above MCL";rownames(Time1)[15] <- "Geometric mean";rownames(Time1)[16] <- "Arithmetic mean"
nonSWPrisons<-Time1
nonSWPrisons <- rbind(nonSWPrisons, Region= c("non-Southwest"))

## Prisons in the SW (N=23)
data4<-cws6[which(cws6$Region=="SW"&cws6$IsPrison==1),];nrow(data4)
nav<-nrow(data4[which(!is.na(data4$SixYearAs)),])
nmiss<-nrow(data4[which(is.na(data4$SixYearAs)),])
pop<-sum(data4$Adjusted.Total.Population.Served)
quant<-as.data.frame(quantile(data4$SixYearAs,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(data4[which(data4$SixYearAs<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(data4[which(data4$SixYearAs>10),])
nmcl<-round(100*(nmcl1/nav),1)
nmclfin<-to.n.prop(c(nmcl1,nmcl))
gm<-exp(mean(log(data4$SixYearAs),na.rm=T));gm<-round(gm,2)
mean<-mean(data4$SixYearAs,na.rm=T);mean<-round(mean,2)
colnames(quant)[1] <- "Correctional facilites"
Time1<-rbind(nav,nmiss,pop,quant,nmrl,nmclfin,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[13] <- "% below MRL"
rownames(Time1)[14] <- "N (%) above MCL";rownames(Time1)[15] <- "Geometric mean";rownames(Time1)[16] <- "Arithmetic mean"
SWPrisons<-Time1
SWPrisons <- rbind(SWPrisons, Region= c("Southwest"))
table(data4$State.Code)

FinalTable1<-cbind(nonSWCWSs,nonSWPrisons,SWCWSs,SWPrisons)
write.xlsx(FinalTable1,row.names=T, file="~/Google Drive/Research/Prison Water As/output/Final Table 1.xlsx")

# List values for SW Prisons in order to report in footnote:
tab<-sort(data4$SixYearAs)
tab

# Are MCL exceedence differences in the SW (prison vs non prison) sig different?
SW<-cws6[which(cws6$Region=="SW"),]
SW$amcl<-0
SW$amcl[which(SW$SixYearAs>10)]<-1

table(SW$amcl);table(SW$IsPrison)
chisq.test(SW$amcl,SW$IsPrison)

fit <- glm(amcl~as.factor(IsPrison),data=SW,family=binomial())
summary(fit)


# Are MCL exceeence differences in the non-SW (prison vs non prison) sig different?
nSW<-cws6[which(cws6$Region!="SW"),]
nSW$amcl<-0
nSW$amcl[which(nSW$SixYearAs>10)]<-1

table(nSW$amcl);table(nSW$IsPrison)
chisq.test(nSW$amcl,nSW$IsPrison)

fit <- glm(amcl~as.factor(IsPrison),data=nSW,family=binomial())
summary(fit)



### end
#######     5.A. Mean (95% CI) arsenic concentrations by status (prison vs non-prison) ######

### Southwest
## 1. Other CWSs (not prisons) in the SW:
SW<-cws6[which(cws6$Region=="SW"),];dim(SW)
SW$notprison<-1
SW$notprison[which(SW$IsPrison==1)]<-0
table(SW$notprison)
fit1 <- glm(SixYearAs ~-1+ notprison, family=gaussian(link="identity"), data=SW)
summary(fit1)
NoPrisonSW<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                                  summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                                  summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))
NoPrisonSW # "3.11 (2.97,3.24)"

## 2. Prisons in the SW:
fit1 <- glm(SixYearAs ~-1+ IsPrison, family=gaussian(link="identity"), data=SW)
summary(fit1)
PrisonsSW<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                                 summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                                 summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))
PrisonsSW #"6.41 (3.48,9.34)"

### Non-Southwest
## 1. Other CWSs (not prisons) in the non-SW:
cws6$notprison<-1
cws6$notprison[which(cws6$IsPrison==1)]<-0
nonSW<-cws6[which(cws6$Region!="SW"),]
table(nonSW$notprison)
fit1 <- glm(SixYearAs ~-1+ notprison, family=gaussian(link="identity"), data=nonSW)
summary(fit1)
NoPrisonNonSW<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                                  summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                                  summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))
NoPrisonNonSW # "1.39 (1.35,1.42)"

## 2. Prisons in the non-SW:
fit1 <- glm(SixYearAs ~-1+ IsPrison, family=gaussian(link="identity"), data=nonSW)
summary(fit1)
PrisonNonSW<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                                  summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                                  summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))
PrisonNonSW #"1.16 (0.58,1.74)"


## Prisons in CA specifically
CA<-cws6[which(cws6$State.Code=="CA"),];dim(CA)
fit1 <- glm(SixYearAs ~-1+ IsPrison, family=gaussian(link="identity"), data=CA)
summary(fit1)
CAprisons<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                                 summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                                 summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))
CAprisons # "7.62 (3.43,11.80)"


## Non-prisons in CA specifically
CAnop<-cws6[which(cws6$State.Code=="CA"&cws6$IsPrison==0),];dim(CAnop) #2,720
fit1 <- glm(SixYearAs ~1, family=gaussian(link="identity"), data=CAnop)
summary(fit1)
CAnoprisons<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                                 summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                                 summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))
CAnoprisons #"3.85 (3.58,4.12)"


### end

#######     5.B. Odds of MCL exceedence comparing prisons to non-prisons ########
## Set-up: Create new covariates for adjustment
# Source water type

cws6$Source.Water.Type <- gsub(" ", "", cws6$Source.Water.Type) #Removes space 
table(cws6$Source.Water.Type)
cws6$Source.Water.Type[which(cws6$Source.Water.Type=="GWP")]<-"GW"
cws6$Source.Water.Type[which(cws6$Source.Water.Type=="GU")]<-"GW"
cws6$Source.Water.Type[which(cws6$Source.Water.Type=="SWP")]<-"SW"
table(cws6$Source.Water.Type)

# Population served size
cws6$popcat<-0
cws6$popcat[cws6$Adjusted.Total.Population.Served<501]<-1
cws6$popcat[501<=cws6$Adjusted.Total.Population.Served&cws6$Adjusted.Total.Population.Served<3301]<-2
cws6$popcat[3301<=cws6$Adjusted.Total.Population.Served&cws6$Adjusted.Total.Population.Served<10001]<-3
cws6$popcat[10001<=cws6$Adjusted.Total.Population.Served&cws6$Adjusted.Total.Population.Served<100001]<-4
cws6$popcat[100001<=cws6$Adjusted.Total.Population.Served]<-5
table(cws6$popcat)

cws6$MCLExc<-0
cws6$MCLExc[which(cws6$SixYearAs>10)]<-1
table(cws6$MCLExc)


# MCL 5 exceedance
cws6$Ex5<-0
cws6$Ex5[which(cws6$SixYearAs>5)]<-1
table(cws6$Ex5)

table(cws6$MCLExc)
table(cws6$Ex5)
sw<-cws6[which(cws6$Region=="SW"),]
table(sw$IsPrison,sw$Ex5)
# How many people served by CWSs prisons in the SW exceeding 5ug/L?
g<-sw[which(sw$Ex5==1&sw$IsPrison==1),]
sum(g$Adjusted.Total.Population.Served) #36264


## Using six-year averages: 
fit <- glm(MCLExc ~ IsPrison, data = cws6, family = "binomial"(link="logit"))
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
resultall6 <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
resultall6
fit <- glm(MCLExc ~ IsPrison+as.factor(Source.Water.Type) +as.factor(popcat), data = cws6, family = "binomial"(link="logit"))
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
resultAdjall6 <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
resultAdjall6#"1.63 (0.72,3.71)"


sw<-cws6[which(cws6$Region=="SW"),]
fit <- glm(MCLExc ~ IsPrison, data = sw, family = "binomial"(link="logit"))
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
resultsw6 <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
resultsw6

fit <- glm(MCLExc ~ IsPrison+as.factor(Source.Water.Type) +as.factor(popcat), data = sw, family = "binomial"(link="logit"))
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
resultAdjsw6 <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
resultAdjsw6#"7.52 (2.86,19.78)"

# Repeat for 5ug/LMCL exceedance:
fit <- glm(Ex5 ~ IsPrison, data = cws6, family = "binomial"(link="logit"))
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
MCL5resultall6 <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
MCL5resultall6 #"1.15 (0.68,1.93)"

fit <- glm(Ex5 ~ IsPrison+as.factor(Source.Water.Type) +as.factor(popcat), data = cws6, family = "binomial"(link="logit"))
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
MCL5Adjresultall6 <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
MCL5Adjresultall6 #""1.2 (0.71,2.03)"

sw<-cws6[which(cws6$Region=="SW"),]
fit <- glm(Ex5 ~ IsPrison, data = sw, family = "binomial"(link="logit"))
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
MCL5resultsw6 <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
MCL5resultsw6 #"2.77 (1.17,6.54)"

fit <- glm(Ex5 ~ IsPrison+as.factor(Source.Water.Type) +as.factor(popcat), data = sw, family = "binomial"(link="logit"))
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
MCL5Adjresultsw6 <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
MCL5Adjresultsw6 #  "2.97 (1.24,7.15)"

overall<-rbind(resultall6,resultAdjall6,MCL5resultall6,MCL5Adjresultall6)
colnames(overall)[1]<-"Overall CWSs"
rownames(overall)[1]<-"Six-year averages, 10ugL crude"
rownames(overall)[2]<-"Six-year averages, 10ugL adjusted"
rownames(overall)[3]<-"Six-year averages, 5ugL crude"
rownames(overall)[4]<-"Six-year averages, 5ugL adjusted"


swresults<-rbind(resultsw6,resultAdjsw6,MCL5resultsw6,MCL5Adjresultsw6)
colnames(swresults)[1]<-"SW CWSs"
rownames(swresults)[1]<-"Six-year averages, 10ugL crude"
rownames(swresults)[2]<-"Six-year averages, 10ugL adjusted"
rownames(swresults)[3]<-"Six-year averages, 5ugL crude"
rownames(swresults)[4]<-"Six-year averages, 5ugL adjusted"


ORResults<-cbind(overall,swresults)
write.xlsx(ORResults,row.names=T, file="~/Google Drive/Research/Prison Water As/output/Table 2 ORs.xlsx")


### end 
####### 6. Figure 1. Raincloud plot ##### ######
## First, define geom_flat_violin function
## Reference for raincloud plots: https://micahallen.org/2018/03/15/introducing-raincloud-plots/
library(ggplot2)
library(dplyr)

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x,
                     xmax = x + width / 2)
            
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data, xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )

## Example:
ggplot(diamonds, aes(cut, carat)) +
  geom_flat_violin() +
  coord_flip()

# First define pretty theme
raincloud_theme = theme(
  text = element_text(size = 14),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.title = element_text(hjust=0.5,lineheight=.8, face="bold", size = 18),
  panel.border = element_blank(),
  #panel.grid.minor = element_blank(),
  #panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

# look at some color options
display.brewer.pal(n = 3, name = 'Dark2')
brewer.pal(n = 3, name = 'Dark2') # "#1B9E77"(All CWSs) "#D95F02" (SW) #7570B3 (prisons)
setwd("~/Google Drive/Research/Prison Water As")
folder.output <- "output"
if( !file.exists( folder.output ) ) {
  dir.create( file.path( folder.output ) )
}


### Figure 1. 
## Compare all 4 groups Prisons in SW, the SW, and then all CWSs in the US:
cws6$F3Group[which(cws6$Region!="SW"&cws6$IsPrison==0)]<-"NoPrisonsNoSW"
cws6$F3Group[which(cws6$Region!="SW"&cws6$IsPrison==1)]<-"PrisonsNoSW"
cws6$F3Group[which(cws6$IsPrison==0&cws6$Region=="SW")]<-"SWNoPrisons"
cws6$F3Group[which(cws6$IsPrison==1&cws6$Region=="SW")]<-"SWPrisons"
table(cws6$F3Group)


library(ggpubr)
pdf(file=paste0( folder.output , "/Figure 1.pdf"), 23, 10)
p1 <- ggplot(data = cws6[which(cws6$F3Group=="NoPrisonsNoSW"|cws6$F3Group=="PrisonsNoSW"),], aes(y = log(SixYearAs), x = as.factor(F3Group), fill = F3Group)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = log(SixYearAs), color = as.factor(F3Group)), position = position_jitter(width = .1), size = 2, alpha = 0.8) +
  geom_boxplot(width = .1,outlier.shape = NA, alpha = 0.5) +
  geom_hline(yintercept=2.302585,linetype = "dotdash", size=0.75,color="tomato4")+
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  coord_flip() +
  theme_bw() +
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=12))+
  raincloud_theme+
  scale_y_continuous(limits=c(-1.61,4), breaks=c(-1.61,-0.488, 0.634,1.756,2.302585,2.878,4.00 ) , labels =c("0.2","0.6","1.9","5.8","10.0","17.8","54.6") )+
  labs(y =expression(paste("Water arsenic concentration, ",mu,"g/L (log scale)")), x = "")+
  scale_x_discrete(labels = c("Other CWSs\n(N=28,204)","Correctional\nfacilities (N=132)"))+
  ggtitle("Non-Southwestern US")+
  annotate(geom="text", x=1.8, y=0.9, label=expression(bold(paste("Mean (95% CI): 1.16 (0.58, 1.74) µg/L"))),color="#D95F02",size=6)+ #prisons not in the SW "1.12 (0.34,1.9)"
  annotate(geom="text", x=0.8, y=0.9, label=expression(bold(paste("Mean (95% CI): 1.39 (1.35, 1.42) µg/L"))),color="#1B9E77",size=6)+ #non-SW CWSs 1.38 (1.33,1.44)
  annotate(geom="text", x=0.49, y=3.0, label=expression(bold(paste("       Maximum\ncontaminant level"))),color="tomato4",size=6)

p2 <- ggplot(data = cws6[which(cws6$F3Group=="SWNoPrisons"|cws6$F3Group=="SWPrisons"),], aes(y = log(SixYearAs), x = as.factor(F3Group), fill = F3Group)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = log(SixYearAs), color = as.factor(F3Group)), position = position_jitter(width = .1), size = 2, alpha = 0.8) +
  geom_boxplot(width = .1,outlier.shape = NA, alpha = 0.5) +
  geom_hline(yintercept=2.302585,linetype = "dotdash", size=0.75,color="tomato4")+
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  coord_flip() +
  theme_bw() +
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=12))+
  raincloud_theme+
  scale_y_continuous(limits=c(-1.61,4), breaks=c(-1.61,-0.488, 0.634,1.756,2.302585,2.878,4.00 ) , labels =c("0.2","0.6","1.9","5.8","10.0","17.8","54.6") )+
  labs(y =expression(paste("Water arsenic concentration, ",mu,"g/L (log scale)")), x = "")+
  scale_x_discrete(labels = c("Other CWSs\n(N=8,727)","Correctional\nfacilities (N=23)"))+
  ggtitle("Southwestern US")+
  annotate(geom="text", x=1.8, y=0.9, label=expression(bold(paste("Mean (95% CI): 6.41 (3.48, 9.34) µg/L"))),color="#D95F02",size=6)+ # Prisons in SW "6.49 (3.56,9.42)"
  annotate(geom="text", x=0.8, y=0.9, label=expression(bold(paste("Mean (95% CI): 3.11 (2.97, 3.24) µg/L"))),color="#1B9E77",size=6)+ #Other CWSs in the SW  "3.11 (2.97,3.24)"
  annotate(geom="text", x=0.49, y=3.0, label=expression(bold(paste("       Maximum\ncontaminant level"))),color="tomato4",size=6)


ggarrange(p1, p2, ncol=2, nrow=1, legend="none")
dev.off()


### end


