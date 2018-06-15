#Load packages
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)

###******************************###
### Setting up the working space ###
###******************************###

#Set working directory
setwd('C:\\Users\\Alvis\\Documents\\GitHub\\DATA5207\\Research Project\\RData')
getwd()


###*********************###
### Reading in the data ###
###*********************###

dv <- read.csv("DV_NSW_by_LGA.csv", header=TRUE, sep=',', na.strings="")
View(dv)
labels <- read.csv("labels.csv", header=TRUE, sep=',', na.strings="")
labels_data <- read.csv("NSW_LGA.csv", header=TRUE, sep=',', na.strings="")

#Top line check of data
#tbl_df(dv)
#View(dv)
summary(dv)     #Summary of data
length(dv)      #Number of column headers
names(dv)       #Column header names
str(dv)         #Structure of data
head(dv, 5)     #Check first five records

summary(labels)     #Summary of data
head(labels, 5)     #Check first five records

length(labels_data)  #Number of column headers


###***********************###
### Manipulating the data ###
###***********************###


#Merge data and labels based on LGA id
data.merge <- merge(dv, labels_data, by="region_id")
#Check export of data in CSV format (too large to preview thoroughly in R)
write.csv(data.merge, 'RData.data.merge.csv')


#Sum monthly domestic violence data into yearly data
#Top line check of data
#tbl_df(data.merge)
length(data.merge)      #Number of column headers
names(data.merge)       #Column header names
str(data.merge)         #Structure of data


data.years <- data.frame( yr1999_dv = apply(data.merge[3:14], 1, sum) ,
                      yr2000_dv = apply(data.merge[15:26], 1, sum), 
                      yr2001_dv = apply(data.merge[27:38], 1, sum),
                      yr2002_dv = apply(data.merge[39:50], 1, sum),
                      yr2003_dv = apply(data.merge[51:62], 1, sum),
                      yr2004_dv = apply(data.merge[63:74], 1, sum),
                      yr2005_dv = apply(data.merge[75:86], 1, sum),
                      yr2006_dv = apply(data.merge[87:98], 1, sum),
                      yr2007_dv = apply(data.merge[99:110], 1, sum),
                      yr2008_dv = apply(data.merge[111:122], 1, sum),
                      yr2009_dv = apply(data.merge[123:134], 1, sum),
                      yr2010_dv = apply(data.merge[135:146], 1, sum),
                      yr2011_dv = apply(data.merge[147:158], 1, sum),
                      yr2012_dv = apply(data.merge[159:170], 1, sum),
                      yr2013_dv = apply(data.merge[171:182], 1, sum),
                      yr2014_dv = apply(data.merge[183:194], 1, sum),
                      yr2015_dv = apply(data.merge[195:206], 1, sum),
                      yr2016_dv = apply(data.merge[207:207], 1, sum),
                      yr2017_dv = apply(data.merge[208:208], 1, sum))

# Check data frame
tbl_df(data.years)
#View(data.years)
head(data.years)
length(data.years)

#Check column totals to original CSV spreadsheet
data.years.check <- colSums(data.years)
data.years.check

#Rename rows to names and region_id labels
data.bind <- cbind(data.years, data.merge[,1:2])
names(data.bind)
#Check export of data in CSV format
write.csv(data.bind, 'RData.data.bind.csv')

#Re-order column labels
data.order.dv <- data.bind[,c(21,20,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]
tbl_df(data.order.dv)
names(data.order.dv)
#Check export of data in CSV format
write.csv(data.order.dv, 'RData.data.order.csv')


#Now we need to match the domestic violence LGA's to the newly mapped LGA's
#So we need to sum the values of the relevant rows that make up these new LGA regions

#Check row and column LGA indicies
nrow(data.order.dv) #140 rows
ncol(data.order.dv) #21 columns
#Check LGA name reference
data.order.dv[,1]

#Sum data for Canterbury-Bankstown (A): Bankstown and Canterbury Councils
#Select only relevant councils
CanterburyBankstown <- data.order.dv[c(6,25),3:21]
#Sum data from relevant councils
CanterburyBankstown_LGA <- rbind(CanterburyBankstown, c(t(apply(CanterburyBankstown[,1:19], 2, sum, na.rm=TRUE))))
#Create new LGA name column
CanterburyBankstown_LGA["LGA"] <- "Canterbury-Bankstown (A)"
#Select summed row
CanterburyBankstown_LGA <- CanterburyBankstown_LGA[3,]
#Reorder columns
CanterburyBankstown_LGA <- CanterburyBankstown_LGA[,c(20,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]
CanterburyBankstown_LGA



#Sum data for Snowy Monaro Regional (A): Bombala, Cooma-Monaro and Snowy River Shires
data.order.dv[,1]
#Bombala Shire Council does not exist in the Domestic Violence data set and hence Cooma-Monaro and Snowy River
#Shires are excluded from the analysis


#Sum data for Hilltops (A): Boorowa Council, Harden Shire and Young Shire
data.order.dv[,1]
#Boorowa Council does not exist in the Domestic Violence data set and hence Harden Shire and Young Shire
#are excluded from the analysis


#Sum data for Central Coast (C) (NSW): Gosford City and Wyong Shire
data.order.dv[,1]
#Select only relevant councils
CentralCoast <- data.order.dv[c(45,138),3:21]
#Sum data from relevant councils
CentralCoast_LGA <- rbind(CentralCoast, c(t(apply(CentralCoast[,1:19], 2, sum, na.rm=TRUE))))
#Create new LGA name column
CentralCoast_LGA["LGA"] <- "Central Coast (C) (NSW)"
#Select summed row
CentralCoast_LGA <- CentralCoast_LGA[3,]
#Reorder columns
CentralCoast_LGA <- CentralCoast_LGA[,c(20,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]
CentralCoast_LGA


#Sum data for Edward River (A): Deniliquin Council and Conargo Shire
data.order.dv[,1]
#Conargo Shire does not exist in the Domestic Violence data set and hence Deniliquin Council is excluded from
#the analysis


#Sum data for Federation (A): Corowa Shire, Lockhart and Urana shires
data.order.dv[,1]
#Urana Shire does not exist in the Domestic Violence data set and hence Corowa and Lockhart Shires are excluded from
#the analysis


#Sum data for Western Plains Regional (A): City of Dubbo and Wellington Council
data.order.dv[,1]
#Select only relevant councils
WesternPlains <- data.order.dv[c(37,131),3:21]
#Sum data from relevant councils
WesternPlains_LGA <- rbind(WesternPlains, c(t(apply(WesternPlains[,1:19], 2, sum, na.rm=TRUE))))
#Create new LGA name column
WesternPlains_LGA["LGA"] <- "Western Plains Regional (A)"
#Select summed row
WesternPlains_LGA <- WesternPlains_LGA[3,]
#Reorder columns
WesternPlains_LGA <- WesternPlains_LGA[,c(20,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]
WesternPlains_LGA


#Sum data for Cumberland (A): parts of Auburn City, the former Parramatta City (Woodville Ward), and Holroyd City Councils
data.order.dv[,1]
#Not possible to break out parts of different councils, so Auburn and Holroyd City Councils are excluded from
#the analysis


#Sum data for Georges River (A): Kogarah City Council and Hurstville City Council
data.order.dv[,1]
#Select only relevant councils
GeorgesRiver <- data.order.dv[c(66,61),3:21]
#Sum data from relevant councils
GeorgesRiver_LGA <- rbind(GeorgesRiver, c(t(apply(GeorgesRiver[,1:19], 2, sum, na.rm=TRUE))))
#Create new LGA name column
GeorgesRiver_LGA["LGA"] <- "Georges River (A)"
#Select summed row
GeorgesRiver_LGA <- GeorgesRiver_LGA[3,]
#Reorder columns
GeorgesRiver_LGA <- GeorgesRiver_LGA[,c(20,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]
GeorgesRiver_LGA


#Sum data for Mid-Coast (A): Gloucester Shire, Great Lakes and City of Greater Taree Councils
data.order.dv[,1]
#Select only relevant councils
MidCoast <- data.order.dv[c(44,47,49),3:21]
#Sum data from relevant councils
MidCoast_LGA <- rbind(MidCoast, c(t(apply(MidCoast[,1:19], 2, sum, na.rm=TRUE))))
#Create new LGA name column
MidCoast_LGA["LGA"] <- "Mid-Coast (A)"
#Select summed row
MidCoast_LGA <- MidCoast_LGA[4,]
#Reorder columns
MidCoast_LGA <- MidCoast_LGA[,c(20,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]
MidCoast_LGA


#Sum data for Armidale Regional (A): Armidale Dumaresq Shire and Guyra Shire
data.order.dv[,1]
#Select only relevant councils
ArmidaleRegional <- data.order.dv[c(2,53),3:21]
#Sum data from relevant councils
ArmidaleRegional_LGA <- rbind(ArmidaleRegional, c(t(apply(ArmidaleRegional[,1:19], 2, sum, na.rm=TRUE))))
#Create new LGA name column
ArmidaleRegional_LGA["LGA"] <- "Armidale Regional (A)"
#Select summed row
ArmidaleRegional_LGA <- ArmidaleRegional_LGA[3,]
#Reorder columns
ArmidaleRegional_LGA <- ArmidaleRegional_LGA[,c(20,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]
ArmidaleRegional_LGA


#Sum data for Inner West (A): Ashfield, Leichhardt, and Marrickville Councils
data.order.dv[,1]
#Select only relevant councils
InnerWest <- data.order.dv[c(3,73,81),3:21]
#Sum data from relevant councils
InnerWest_LGA <- rbind(InnerWest, c(t(apply(InnerWest[,1:19], 2, sum, na.rm=TRUE))))
#Create new LGA name column
InnerWest_LGA["LGA"] <- "Inner West (A)"
#Select summed row
InnerWest_LGA <- InnerWest_LGA[4,]
#Reorder columns
InnerWest_LGA <- InnerWest_LGA[,c(20,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]
InnerWest_LGA


#Sum data for Murrumbidgee (A): Municipality of Jerilderie and Wunnamurra Shire
data.order.dv[,1]
#Does not exist in the Domestic Violence data set and hence Municipality of Jerilderie and Wunnamurra Shire
#are excluded from the analysis


#Sum data for Northern Beaches (A): Manly, Pittwater and Warringah Councils
data.order.dv[,1]
#Select only relevant councils
Northern_Beaches <- data.order.dv[c(80,99,127),3:21]
#Sum data from relevant councils
Northern_Beaches_LGA <- rbind(Northern_Beaches, c(t(apply(Northern_Beaches[,1:19], 2, sum, na.rm=TRUE))))
#Create new LGA name column
Northern_Beaches_LGA["LGA"] <- "Northern Beaches (A)"
#Select summed row
Northern_Beaches_LGA <- Northern_Beaches_LGA[4,]
#Reorder columns
Northern_Beaches_LGA <- Northern_Beaches_LGA[,c(20,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]
Northern_Beaches_LGA


#Sum data for Murray River (A): Murray Shire with Wakool Shire
data.order.dv[,1]
#Select only relevant councils
MurrayRiver <- data.order.dv[c(85,124),3:21]
#Sum data from relevant councils
MurrayRiver_LGA <- rbind(MurrayRiver, c(t(apply(MurrayRiver[,1:19], 2, sum, na.rm=TRUE))))
#Create new LGA name column
MurrayRiver_LGA["LGA"] <- "Murray River (A)"
#Select summed row
MurrayRiver_LGA <- MurrayRiver_LGA[3,]
#Reorder columns
MurrayRiver_LGA <- MurrayRiver_LGA[,c(20,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]
MurrayRiver_LGA


#Sum data for Snowy Valleys (A): Tumbarumba Shire and Tumut Shire councils
data.order.dv[,1]
#Select only relevant councils
SnowyValleys <- data.order.dv[c(117,118),3:21]
#Sum data from relevant councils
SnowyValleys_LGA <- rbind(SnowyValleys, c(t(apply(SnowyValleys[,1:19], 2, sum, na.rm=TRUE))))
#Create new LGA name column
SnowyValleys_LGA["LGA"] <- "Snowy Valleys (A)"
#Select summed row
SnowyValleys_LGA <- SnowyValleys_LGA[3,]
#Reorder columns
SnowyValleys_LGA <- SnowyValleys_LGA[,c(20,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]
SnowyValleys_LGA


#Run data for Queanbeyan-Palerang Regional (A). City of Queanbeyan and Palerang Council,
data.order.dv[,1]
#Select only relevant councils
Queanbeyan <- data.order.dv[c(102,95),3:21]
#Sum data from relevant councils
Queanbeyan_LGA <- rbind(Queanbeyan, c(t(apply(Queanbeyan[,1:19], 2, sum, na.rm=TRUE))))
#Create new LGA name column
Queanbeyan_LGA["LGA"] <- "Queanbeyan-Palerang Regional (A)"
#Select summed row
Queanbeyan_LGA <- Queanbeyan_LGA[3,]
#Reorder columns
Queanbeyan_LGA <- Queanbeyan_LGA[,c(20,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]
Queanbeyan_LGA



#Now we need to remove the old councils from the original data sets so they match the new LGA regions
#View(data.order.dv)
data.order.dv.edit <- data.order.dv[c(-6,-25,-45,-138,-37,-131,-66,-61,-44,-49,-47,-2,-53,-3,-73,-81,-80,
                                     -99,-127,-85,-124,-117,-118,-102,-95),1:21]

#Revised count of rows
#View(data.order.dv.edit)
nrow(data.order.dv.edit) #115 rows
ncol(data.order.dv.edit) #21 columns


#Now that we have summed up the new LGA regions, we need to add in the unique LGA region id and
#add them back into the original data set
CanterburyBankstown_LGA$region_id <- "LGA11570"
#CanterburyBankstown_LGA$LGA <- "Canterbury-Bankstown (A)"
colnames(CanterburyBankstown_LGA)
CanterburyBankstown_LGA <- CanterburyBankstown_LGA[,c(1,21,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)]
CanterburyBankstown_LGA

CentralCoast_LGA$region_id <- "LGA11650"
#CentralCoast_LGA$LGA <- "Central Coast (C) (NSW)"
colnames(CentralCoast_LGA)
CentralCoast_LGA <- CentralCoast_LGA[,c(1,21,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)]
CentralCoast_LGA


WesternPlains_LGA$region_id <- "LGA18230"
#WesternPlains_LGA$LGA <- "Western Plains Regional (A)"
colnames(WesternPlains_LGA)
WesternPlains_LGA <- WesternPlains_LGA[,c(1,21,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)]
WesternPlains_LGA

GeorgesRiver_LGA$region_id <- "LGA12930"
#GeorgesRiver_LGA$LGA <- "Georges River (A)"
colnames(GeorgesRiver_LGA)
GeorgesRiver_LGA <- GeorgesRiver_LGA[,c(1,21,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)]
GeorgesRiver_LGA

MidCoast_LGA$region_id <- "LGA15240"
#MidCoast_LGA$LGA <- "Mid-Coast (A)"
colnames(ArmidaleRegional_LGA)
MidCoast_LGA <- MidCoast_LGA[,c(1,21,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)]
MidCoast_LGA

ArmidaleRegional_LGA$region_id <- "LGA10130"
#ArmidaleRegional_LGA$LGA <- "Armidale Regional (A)"
colnames(ArmidaleRegional_LGA)
ArmidaleRegional_LGA <- ArmidaleRegional_LGA[,c(1,21,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)]
ArmidaleRegional_LGA

InnerWest_LGA$region_id <- "LGA14170"
#InnerWest_LGA$LGA <- "Inner West (A)"
colnames(InnerWest_LGA)
InnerWest_LGA <- InnerWest_LGA[,c(1,21,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)]
InnerWest_LGA

Northern_Beaches_LGA$region_id <- "LGA15990"
#Northern_Beaches_LGA$LGA <- "Northern Beaches (A)"
colnames(Northern_Beaches_LGA)
Northern_Beaches_LGA <- Northern_Beaches_LGA[,c(1,21,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)]
Northern_Beaches_LGA

MurrayRiver_LGA$region_id <- "LGA15520"
#MurrayRiver_LGA$LGA <- "Murray River (A)"
colnames(MurrayRiver_LGA)
MurrayRiver_LGA <- MurrayRiver_LGA[,c(1,21,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)]
MurrayRiver_LGA

SnowyValleys_LGA$region_id <- "LGA17080"
#SnowyValleys_LGA$LGA <- "Snowy Valleys (A)"
colnames(SnowyValleys_LGA)
SnowyValleys_LGA <- SnowyValleys_LGA[,c(1,21,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)]
SnowyValleys_LGA

Queanbeyan_LGA$region_id <- "LGA16490"
#Queanbeyan_LGA$LGA <- "Queanbeyan-Palerang Regional (A)"
colnames(Queanbeyan_LGA)
Queanbeyan_LGA <- Queanbeyan_LGA[,c(1,21,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)]
Queanbeyan_LGA

data.order.dv.new <- rbind(data.order.dv.edit
                           ,CanterburyBankstown_LGA
                           ,CentralCoast_LGA
                           ,WesternPlains_LGA
                           ,GeorgesRiver_LGA
                           ,MidCoast_LGA
                           ,ArmidaleRegional_LGA
                           ,InnerWest_LGA
                           ,Northern_Beaches_LGA
                           ,MurrayRiver_LGA
                           ,SnowyValleys_LGA
                           ,Queanbeyan_LGA
)


#Check new data frame
#View(data.order.dv.new)
nrow(data.order.dv.new) #126 rows
ncol(data.order.dv.new) #21 columns


#Now that we have aggregated the new amalgamated councils into the data set, we now need to remove
#those individual Council regions which could not be matched
rownames(data.order.dv.new)
data.order.dv.final <- data.order.dv.new[-c(2,27,30,32,45,48,65,91,115),]
data.order.dv.final[,1:2]
nrow(data.order.dv.final) #117 rows
ncol(data.order.dv.final) #21 columns
write.csv(data.order.dv.final, 'RData.data.order.dv.final.csv')



### Population count by LGA regions ###
#Due to 2016 council mergers a number of LGA council regions were amalgamated.
#Refer to document with referenced councils.
#An estimated residential population has been provided for each year.
#Because of this amalgamation and the different council regions the original data on the
#domestic violence council areas needs to be updated (merge different rows)
total.pop.count <- read.csv("LGA_Population_Mapping.csv", header=TRUE, sep=',', na.strings="")
total.pop.count <- total.pop.count[,2:21]
tbl_df(total.pop.count)
length(total.pop.count)



#Now we need to inner join the domestic violence data set to the Total Population data set.
#This will allow us to calculate percentages of the population to ensure relativity across LGA regions
#respective of the population
tbl_df(data.order.dv.final)
nrow(data.order.dv.final) #117 rows
ncol(data.order.dv.final) #21 columns


tbl_df(total.pop.count)
nrow(total.pop.count) #128 rows
ncol(total.pop.count) #20 columns


dv_pop <- join(data.order.dv.final, total.pop.count, type="inner", by="region_id")
tbl_df(dv_pop)
#View(dv_pop)
nrow(dv_pop) #114 rows
ncol(dv_pop) #40 columns
write.csv(dv_pop, 'RData.dv_pop.csv')


### Calculate domestic violence incidents as percentage of total population ###
dv_pop$yr1999pct <- (dv_pop$yr1999_dv/dv_pop$yr1999_pop)
dv_pop$yr2000pct <- (dv_pop$yr2000_dv/dv_pop$yr2000_pop)
dv_pop$yr2001pct <- (dv_pop$yr2001_dv/dv_pop$yr2001_pop)
dv_pop$yr2002pct <- (dv_pop$yr2002_dv/dv_pop$yr2002_pop)
dv_pop$yr2003pct <- (dv_pop$yr2003_dv/dv_pop$yr2003_pop)
dv_pop$yr2004pct <- (dv_pop$yr2004_dv/dv_pop$yr2004_pop)
dv_pop$yr2005pct <- (dv_pop$yr2005_dv/dv_pop$yr2005_pop)
dv_pop$yr2006pct <- (dv_pop$yr2006_dv/dv_pop$yr2006_pop)
dv_pop$yr2007pct <- (dv_pop$yr2007_dv/dv_pop$yr2007_pop)
dv_pop$yr2008pct <- (dv_pop$yr2008_dv/dv_pop$yr2008_pop)
dv_pop$yr2009pct <- (dv_pop$yr2009_dv/dv_pop$yr2009_pop)
dv_pop$yr2010pct <- (dv_pop$yr2010_dv/dv_pop$yr2010_pop)
dv_pop$yr2011pct <- (dv_pop$yr2011_dv/dv_pop$yr2011_pop)
dv_pop$yr2012pct <- (dv_pop$yr2012_dv/dv_pop$yr2012_pop)
dv_pop$yr2013pct <- (dv_pop$yr2013_dv/dv_pop$yr2013_pop)
dv_pop$yr2014pct <- (dv_pop$yr2014_dv/dv_pop$yr2014_pop)
dv_pop$yr2015pct <- (dv_pop$yr2015_dv/dv_pop$yr2015_pop)
dv_pop$yr2016pct <- (dv_pop$yr2016_dv/dv_pop$yr2016_pop)
dv_pop$yr2017pct <- (dv_pop$yr2017_dv/dv_pop$yr2017_pop)

#View(dv_pop)
write.csv(dv_pop,'RData.dv_pop.csv')


LGA_labels_2016 <- read.csv('region_id_to_LGA_2016.csv', header=TRUE, sep=',', na.strings="")
#head(LGA_labels_2016)
#ncol(LGA_labels_2016)

#Merge dataset to bring in correct and full LGA label
dv_pop_label <- join(x = dv_pop, y = LGA_labels_2016, type="inner", by='region_id')
#Remove short LGA label
dv_pop_label <- dv_pop_label[,-c(1)]
tbl_df(dv_pop_label)

#Check, identify and remove duplicated rows from joins
ncol(dv_pop_label)
names(dv_pop_label)

dv_pop_label <- unique(dv_pop_label[, 1:59])
duplicated(dv_pop_label) #no duplicated rows
write.csv(dv_pop_label,'RData.dv_pop_label.csv')

#Reorder data frame
dv_pop_order <- dv_pop_label[,c(59,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,27,29,30
                                ,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58)]
#View(dv_pop_order)
names(dv_pop_order)
write.csv(dv_pop_order,'RData.dv_pop_order.csv')



##It is necessary at this point to check the population distributions for each LGA to ensure the numbers
#are not being skewed by very large or very small population areas
#Average population for each LGA across years 1999 to 2015
names(total.pop.count)
total.pop.count$rowmean <- as.integer(round(rowMeans(total.pop.count[,2:20])))

#Investigate LGA populations for outliers
summary <- summary(total.pop.count$rowmean)
summary

#LGA Regions less than 4,000
total.pop.count.filter_LT4k <- total.pop.count[total.pop.count[,'rowmean'] < 4000,]
total.pop.count.filter_LT4k

#LGA Regions greater than 200,000
total.pop.count.filter_GT200k <- total.pop.count[total.pop.count[,'rowmean'] > 200000,]
total.pop.count.filter_GT200k

#Join LGA population outlier together into one dataframe
total.pop.count.filter <- rbind(total.pop.count.filter_LT4k, total.pop.count.filter_GT200k)
total.pop.count.filter
count(total.pop.count.filter) #15 LGA regions
#View(total.pop.count.filter)
total.pop.count.filter <- total.pop.count.filter[,c(1:2)]

#Plot LGA regions
total.pop.count_top40 <- total.pop.count[c(1:40),]
ggplot_top40 <- ggplot(data = total.pop.count_top40, aes(rowmean/1000, y=region_id)) +
        geom_point()#+xlim(0,5)#+xlim(0,370)
ggplot_top40

#Plot LGA regions
total.pop.count_next40 <- total.pop.count[c(41:80),]
ggplot_next40 <- ggplot(data = total.pop.count_next40, aes(x=rowmean/1000, y=region_id)) +
        geom_point()+xlim(0,370)
ggplot_next40

#Plot LGA regions
total.pop.count_last48 <- total.pop.count[c(81:128),]
ggplot_last48 <- ggplot(data = total.pop.count_last48, aes(x=rowmean/1000, y=region_id)) +
        geom_point()+xlim(0,370)
ggplot_last48

#Remove LGA region outliers from domestic violence dataframe
#install.packages("Hmisc")
library("Hmisc")
dv_pop_order[,1:2]
remove <- c('11','13','40','93','97','7','106','108','122','83')
#remove <- c('16','20','64','136','142','12','3','31','43','121')
dv_pop_order = dv_pop_order[which(rownames(dv_pop_order) %nin% remove), ]
nrow(dv_pop_order)
write.csv(dv_pop_order, 'dv_pop_order.csv')



#Next steps are to extract the data values for the Independent variables... demographics
# I will also have to calculate these variables as a percentage of the population to keep relevant
# I will also need to join data and labels together

####NEED TO AGGREGATE THE LGA REGIONS FOR THE INDEPENDENT VARIABLES TO MATCH THE DEPENDENT VARIABLE (DOMESTIC VIOLENCE)
#### ACTION: NEED TO CHANGE THE LABELS_DATA FILE AT THE START - AGGREGATE THE COUNCILS/ MATCH TO DV_POP_ORDER

###
#The LGA regions for the independent variables will now need to match the domestic violence LGA region list, so
#below we will go through and sum the values of the relevant rows that make up these domestic violence LGA regions

#Sum data for Canterbury-Bankstown (A): Bankstown and Canterbury Councils
#Select only relevant councils
CanterburyBankstown <- labels_data[c(7,29),5:7946]
#Sum data from relevant councils
CanterburyBankstown_LGA <- rbind(CanterburyBankstown, c(t(apply(CanterburyBankstown[,1:7942], 2, sum, na.rm=TRUE))))
#Create new LGA name column
CanterburyBankstown_LGA["LGA"] <- "Canterbury-Bankstown (A)"
#Select summed row
CanterburyBankstown_LGA <- CanterburyBankstown_LGA[3,]
#Reorder columns
CanterburyBankstown_LGA <- CanterburyBankstown_LGA[,c(7943,1:7942)]


#Sum data for Central Coast (C) (NSW): Gosford City and Wyong Shire
labels_data[,1:2]
#Select only relevant councils
CentralCoast <- labels_data[c(52,150),5:7946]
#Sum data from relevant councils
CentralCoast_LGA <- rbind(CentralCoast, c(t(apply(CentralCoast[,1:7942], 2, sum, na.rm=TRUE))))
#Create new LGA name column
CentralCoast_LGA["LGA"] <- "Central Coast (C) (NSW)"
#Select summed row
CentralCoast_LGA <- CentralCoast_LGA[3,]
#Reorder columns
CentralCoast_LGA <- CentralCoast_LGA[,c(7943,1:7942)]


#Sum data for Western Plains Regional (A): City of Dubbo and Wellington Council
labels_data[,1:2]
#Select only relevant councils
WesternPlains <- labels_data[c(44,143),5:7946]
#Sum data from relevant councils
WesternPlains_LGA <- rbind(WesternPlains, c(t(apply(WesternPlains[,1:7942], 2, sum, na.rm=TRUE))))
#Create new LGA name column
WesternPlains_LGA["LGA"] <- "Western Plains Regional (A)"
#Select summed row
WesternPlains_LGA <- WesternPlains_LGA[3,]
#Reorder columns
WesternPlains_LGA <- WesternPlains_LGA[,c(7943,1:7942)]


#Sum data for Georges River (A): Kogarah City Council and Hurstville City Council
labels_data[,1:2]
#Select only relevant councils
GeorgesRiver <- labels_data[c(74,68),5:7946]
#Sum data from relevant councils
GeorgesRiver_LGA <- rbind(GeorgesRiver, c(t(apply(GeorgesRiver[,1:7942], 2, sum, na.rm=TRUE))))
#Create new LGA name column
GeorgesRiver_LGA["LGA"] <- "Georges River (A)"
#Select summed row
GeorgesRiver_LGA <- GeorgesRiver_LGA[3,]
#Reorder columns
GeorgesRiver_LGA <- GeorgesRiver_LGA[,c(7943,1:7942)]


#Sum data for Mid-Coast (A): Gloucester Shire, Great Lakes and City of Greater Taree Councils
labels_data[,1:2]
#Select only relevant councils
MidCoast <- labels_data[c(51,54,56),5:7946]
#Sum data from relevant councils
MidCoast_LGA <- rbind(MidCoast, c(t(apply(MidCoast[,1:7942], 2, sum, na.rm=TRUE))))
#Create new LGA name column
MidCoast_LGA["LGA"] <- "Mid-Coast (A)"
#Select summed row
MidCoast_LGA <- MidCoast_LGA[4,]
#Reorder columns
MidCoast_LGA <- MidCoast_LGA[,c(7943,1:7942)]


#Sum data for Armidale Regional (A): Armidale Dumaresq Shire and Guyra Shire
labels_data[,1:2]
#Select only relevant councils
ArmidaleRegional <- labels_data[c(2,60),5:7946]
#Sum data from relevant councils
ArmidaleRegional_LGA <- rbind(ArmidaleRegional, c(t(apply(ArmidaleRegional[,1:7942], 2, sum, na.rm=TRUE))))
#Create new LGA name column
ArmidaleRegional_LGA["LGA"] <- "Armidale Regional (A)"
#Select summed row
ArmidaleRegional_LGA <- ArmidaleRegional_LGA[3,]
#Reorder columns
ArmidaleRegional_LGA <- ArmidaleRegional_LGA[,c(7943,1:7942)]


#Sum data for Inner West (A): Ashfield, Leichhardt, and Marrickville Councils
labels_data[,1:2]
#Select only relevant councils
InnerWest <- labels_data[c(3,81,89),5:7946]
#Sum data from relevant councils
InnerWest_LGA <- rbind(InnerWest, c(t(apply(InnerWest[,1:7942], 2, sum, na.rm=TRUE))))
#Create new LGA name column
InnerWest_LGA["LGA"] <- "Inner West (A)"
#Select summed row
InnerWest_LGA <- InnerWest_LGA[4,]
#Reorder columns
InnerWest_LGA <- InnerWest_LGA[,c(7943,1:7942)]


#Sum data for Northern Beaches (A): Manly, Pittwater and Warringah Councils
labels_data[,1:2]
#Select only relevant councils
Northern_Beaches <- labels_data[c(88,108,139),5:7946]
#Sum data from relevant councils
Northern_Beaches_LGA <- rbind(Northern_Beaches, c(t(apply(Northern_Beaches[,1:7942], 2, sum, na.rm=TRUE))))
#Create new LGA name column
Northern_Beaches_LGA["LGA"] <- "Northern Beaches (A)"
#Select summed row
Northern_Beaches_LGA <- Northern_Beaches_LGA[4,]
#Reorder columns
Northern_Beaches_LGA <- Northern_Beaches_LGA[,c(7943,1:7942)]


#Sum data for Murray River (A): Murray Shire with Wakool Shire
labels_data[,1:2]
#Select only relevant councils
MurrayRiver <- labels_data[c(93,135),5:7946]
#Sum data from relevant councils
MurrayRiver_LGA <- rbind(MurrayRiver, c(t(apply(MurrayRiver[,1:7942], 2, sum, na.rm=TRUE))))
#Create new LGA name column
MurrayRiver_LGA["LGA"] <- "Murray River (A)"
#Select summed row
MurrayRiver_LGA <- MurrayRiver_LGA[3,]
#Reorder columns
MurrayRiver_LGA <- MurrayRiver_LGA[,c(7943,1:7942)]


#Sum data for Snowy Valleys (A): Tumbarumba Shire and Tumut Shire councils
labels_data[,1:2]
#Select only relevant councils
SnowyValleys <- labels_data[c(127,128),5:7946]
#Sum data from relevant councils
SnowyValleys_LGA <- rbind(SnowyValleys, c(t(apply(SnowyValleys[,1:7942], 2, sum, na.rm=TRUE))))
#Create new LGA name column
SnowyValleys_LGA["LGA"] <- "Snowy Valleys (A)"
#Select summed row
SnowyValleys_LGA <- SnowyValleys_LGA[3,]
#Reorder columns
SnowyValleys_LGA <- SnowyValleys_LGA[,c(7943,1:7942)]


#Run data for Queanbeyan-Palerang Regional (A). City of Queanbeyan and Palerang Council,
labels_data[,1:2]
#Select only relevant councils
Queanbeyan <- labels_data[c(111,104),5:7946]
#Sum data from relevant councils
Queanbeyan_LGA <- rbind(Queanbeyan, c(t(apply(Queanbeyan[,1:7942], 2, sum, na.rm=TRUE))))
#Create new LGA name column
Queanbeyan_LGA["LGA"] <- "Queanbeyan-Palerang Regional (A)"
#Select summed row
Queanbeyan_LGA <- Queanbeyan_LGA[3,]
#Reorder columns
Queanbeyan_LGA <- Queanbeyan_LGA[,c(7943,1:7942)]

##Now we need to remove the old councils from the independent data sets so they match the new LGA regions
labels_data_edit <- labels_data[c(-2,-3,-4,-6,-7,-17,-18,-21,-29,-30,-31,-36,-38,-40,-41,-43,-44,-51,-52,-54,
                                -56,-58,-60,-62,-65,-68,-70,-74,-81,-86,-88,-89,-93,-94,-104,-106,-108,-111,
                                -119,-122,-127,-128,-133,-135,-138,-139,-143,-150,-152,-153,-154),c(1:2,5:7946)]

#Revised count of rows
nrow(labels_data_edit) #103 rows
ncol(labels_data_edit) #7944 columns

#Reorder columns
labels_data_edit_order <- labels_data_edit[,c(2,1, 3:7944)]
labels_data_edit_order$LGA <- labels_data_edit_order$label
labels_data_edit_order <- labels_data_edit_order[,c(-1), c(2:7945)]
labels_data_edit_order <- labels_data_edit_order[,c(7944,1:7943)]
write.csv(labels_data_edit_order, 'labels_data_edit_order.csv')


#Now that we have summed up the new LGA regions, we need to add in the unique LGA region id and
#add them back into the original data set
CanterburyBankstown_LGA$region_id <- "LGA11570"
#CanterburyBankstown_LGA$LGA <- "Canterbury-Bankstown (A)"
ncol(CanterburyBankstown_LGA)
#CanterburyBankstown_LGA[,7944]
CanterburyBankstown_LGA <- CanterburyBankstown_LGA[,c(1,7944,2:7943)]
ncol(CanterburyBankstown_LGA)


CentralCoast_LGA$region_id <- "LGA11650"
#CentralCoast_LGA$LGA <- "Central Coast (C) (NSW)"
ncol(CentralCoast_LGA)
#CentralCoast_LGA[,7944]
CentralCoast_LGA <- CentralCoast_LGA[,c(1,7944,2:7943)]

WesternPlains_LGA$region_id <- "LGA18230"
#WesternPlains_LGA$LGA <- "Western Plains Regional (A)"
ncol(WesternPlains_LGA)
#WesternPlains_LGA[,7944]
WesternPlains_LGA <- WesternPlains_LGA[,c(1,7944,2:7943)]

GeorgesRiver_LGA$region_id <- "LGA12930"
#GeorgesRiver_LGA$LGA <- "Georges River (A)"
ncol(GeorgesRiver_LGA)
#GeorgesRiver_LGA[,7944]
GeorgesRiver_LGA <- GeorgesRiver_LGA[,c(1,7944,2:7943)]

MidCoast_LGA$region_id <- "LGA15240"
#MidCoast_LGA$LGA <- "Mid-Coast (A)"
ncol(MidCoast_LGA)
MidCoast_LGA[,7944]
MidCoast_LGA <- MidCoast_LGA[,c(1,7944,2:7943)]

ArmidaleRegional_LGA$region_id <- "LGA10130"
#ArmidaleRegional_LGA$LGA <- "Armidale Regional (A)"
ncol(ArmidaleRegional_LGA)
#ArmidaleRegional_LGA[,7944]
ArmidaleRegional_LGA <- ArmidaleRegional_LGA[,c(1,7944,2:7943)]

InnerWest_LGA$region_id <- "LGA14170"
#InnerWest_LGA$LGA <- "Inner West (A)"
ncol(InnerWest_LGA)
#InnerWest_LGA[,7944]
InnerWest_LGA <- InnerWest_LGA[,c(1,7944,2:7943)]

Northern_Beaches_LGA$region_id <- "LGA15990"
#Northern_Beaches_LGA$LGA <- "Northern Beaches (A)"
ncol(Northern_Beaches_LGA)
#Northern_Beaches_LGA[,7944]
Northern_Beaches_LGA <- Northern_Beaches_LGA[,c(1,7944,2:7943)]

MurrayRiver_LGA$region_id <- "LGA15520"
#MurrayRiver_LGA$LGA <- "Murray River (A)"
ncol(MurrayRiver_LGA)
#MurrayRiver_LGA[,7944]
MurrayRiver_LGA <- MurrayRiver_LGA[,c(1,7944,2:7943)]

SnowyValleys_LGA$region_id <- "LGA17080"
#SnowyValleys_LGA$LGA <- "Snowy Valleys (A)"
ncol(SnowyValleys_LGA)
SnowyValleys_LGA[,7944]
SnowyValleys_LGA <- SnowyValleys_LGA[,c(1,7944,2:7943)]

Queanbeyan_LGA$region_id <- "LGA16490"
#Queanbeyan_LGA$LGA <- "Queanbeyan-Palerang Regional (A)"
ncol(Queanbeyan_LGA)
#Queanbeyan_LGA[,7944]
Queanbeyan_LGA <- Queanbeyan_LGA[,c(1,7944,2:7943)]


#Now we should be able to add in the new LGA regions into the independent variable dataset
labels_data.new <- rbind(labels_data_edit_order
                           ,CanterburyBankstown_LGA
                           ,CentralCoast_LGA
                           ,WesternPlains_LGA
                           ,GeorgesRiver_LGA
                           ,MidCoast_LGA
                           ,ArmidaleRegional_LGA
                           ,InnerWest_LGA
                           ,Northern_Beaches_LGA
                           ,MurrayRiver_LGA
                           ,SnowyValleys_LGA
                           ,Queanbeyan_LGA
                         )

write.csv(labels_data.new, 'labels_data.new.csv')


##It is necessary at this point to check the population distributions for each LGA to ensure the numbers
#are not being skewed by very large or very small population areas
labels_data.new[,1:2]
remove <- c('16','20','64','136','142','12','3','31','43','121')
labels_data.new <- labels_data.new[which(rownames(labels_data.new) %nin% remove), ]
nrow(labels_data.new)
write.csv(labels_data.new, 'labels_data.new.csv')



### Independent variables
#Alcohol related domestic assaults (BOSCAR data set)
#boscar <- read.csv('BOSCAR_ALCOHOL_LGA.csv', header=TRUE, sep=',', na.strings="")
#View(boscar)

#Percentage of Population by age groups (2011 Census)
#Get label descriptors
young_women_labels <- labels[, c(1,3)]
young_women_labels <- filter(young_women_labels, Sequential %in% c("B911", "B912", "B914", "B915", "B923", "B924", "B926", "B927",
                                                                   "B935", "B936", "B938", "B939", "B947", "B948", "B950", "B951",
                                                                   "B959", "B960", "B962", "B963", "B971", "B972", "B974", "B975",
                                                                   "B983", "B984", "B986", "B987", "B995", "B996", "B998", "B999",
                                                                   "B1007",  "B1008","B1010", "B1011", "B1019", "B1020", "B1022",
                                                                   "B1023", "B1040", "B1041", "B1042"
))
#View(young_women_labels)
write.csv(young_women_labels, 'RData.young_women_labels.csv')

#Get data for labels
young_women_data <- labels_data.new[, c("region_id", "LGA", "B911", "B912", "B914", "B915", "B923",
                                        "B924", "B926", "B927", "B935", "B936", "B938", "B939", "B947",
                                        "B948", "B950", "B951", "B959", "B960", "B962", "B963", "B971",
                                        "B972", "B974", "B975", "B983", "B984", "B986", "B987", "B995",
                                        "B996", "B998", "B999", "B1007", "B1008", "B1010", "B1011",
                                        "B1019", "B1020", "B1022", "B1023", "B1040", "B1041", "B1042"
)]
#View(young_women_data)
#Sum data for each row variable
ncol(young_women_data) #42
tbl_df(young_women_data)

#Total Males and Females
young_women_data$males_ppt <- (young_women_data$B1040/young_women_data$B1042)
young_women_data$females_ppt <- (young_women_data$B1041/young_women_data$B1042)
young_women_data <- young_women_data[,c(1:2,46:47)]
write.csv(young_women_data, 'young_women_data.csv')



#Percentage of Population born in non_English speaking countries (2011 Census)
#Get label descriptors
born_overseas_labels <- labels[, c(1,3)]
born_overseas_labels <- filter(born_overseas_labels, Sequential %in% c("B1247", "B1248", "B1358", "B1359"))
#born_overseas_labels

#Get data for labels
born_overseas_data <- labels_data.new[, c("region_id", "LGA", "B1247", "B1248", "B1358", "B1359")]
#View(born_overseas_data)
#Sum data for each row variable
ncol(born_overseas_data)
head(born_overseas_data)
born_overseas_data$Total <- rowSums(born_overseas_data[, c(5:6)])
born_overseas_data$Total <- as.integer(as.double(born_overseas_data$Total))
born_overseas_data$Total <- as.integer(as.double(born_overseas_data$Total))
born_overseas_data$male_birth_os <- (born_overseas_data$B1358-born_overseas_data$B1247)
born_overseas_data$female_birth_os <- (born_overseas_data$B1359-born_overseas_data$B1248)
born_overseas_data$male_birth_au_ppt <- (born_overseas_data$B1247/born_overseas_data$Total)
born_overseas_data$female_birth_au_ppt <- (born_overseas_data$B1248/born_overseas_data$Total)
born_overseas_data$male_birth_os_ppt <- (born_overseas_data$male_birth_os/born_overseas_data$Total)
born_overseas_data$female_birth_os_ppt <- (born_overseas_data$female_birth_os/born_overseas_data$Total)
names(born_overseas_data)
tbl_df(born_overseas_data[,c(1:2,8:11)])


#Percentage of Population who are Indigenous and Non-Indigenous (2011 Census)
#Get label descriptors
indigenous_labels <- labels[, c(1,3)]
indigenous_labels <- filter(indigenous_labels, Sequential %in% c("B1031", "B1032", "B1034", "B1035"))
indigenous_labels

#Get data for labels
indigenous_data <- labels_data.new[, c("region_id", "LGA", "B1031", "B1032", "B1034", "B1035")]
#View(indigenous_data)
#Sum data for each row variable
ncol(indigenous_data)
indigenous_data$Total <- rowSums(indigenous_data[, c(3:6)])
indigenous_data$Total <- as.integer(as.double(indigenous_data$Total))
tbl_df(indigenous_data)

indigenous_data$Total_Indig_Males_ppt <- (indigenous_data$B1031/indigenous_data$Total)
indigenous_data$Total_Indig_Females_ppt <- (indigenous_data$B1032/indigenous_data$Total)
indigenous_data$Total_Non_Indig_Males_ppt <- (indigenous_data$B1034/indigenous_data$Total)
indigenous_data$Total_Non_Indig_Females_ppt <- (indigenous_data$B1035/indigenous_data$Total)


#Percentage of Population who are sole parents (2011 Census)
#Get label descriptors
sole_parents_labels <- labels[, c(1,3)]
sole_parents_labels <- filter(sole_parents_labels, Sequential %in% c("B4953", "B4954", "B4955"))
#sole_parents_labels

#Get data for labels
sole_parents_data <- labels_data.new[, c("region_id", "LGA", "B4953", "B4954", "B4955")]
#View(sole_parents_data)
#Sum data for each row variable
ncol(sole_parents_data)
sole_parents_data$Total <- rowSums(sole_parents_data[, c(3:5)])
sole_parents_data$Total <- as.integer(as.double(sole_parents_data$Total))

sole_parents_data$Coup_Fam_No_Child_ppt <- (sole_parents_data$B4953/sole_parents_data$Total)
sole_parents_data$Coup_Fam_W_Child_ppt <- (sole_parents_data$B4954/sole_parents_data$Total)
sole_parents_data$Sole_Parent_ppt <- (sole_parents_data$B4955/sole_parents_data$Total)


#Percentage of rental population (2011 Census)
#Get label descriptors
rental_labels <- labels[, c(1,3)]
rental_labels <- filter(rental_labels, Sequential %in% c("B5100", "B5106"))
#rental_labels

#Get data for labels
rental_data <- labels_data.new[, c("region_id", "LGA", "B5100", "B5106")]
#View(rental_data)
#Sum data for each row variable
ncol(rental_data)
rental_data$Total <- rowSums(rental_data[, c(3:4)])
rental_data$Total <- as.integer(as.double(rental_data$Total))
tbl_df(rental_data)

rental_data$Private_ppt <- (rental_data$B5100/rental_data$Total)
rental_data$Government_ppt <- (rental_data$B5106/rental_data$Total)


#Percentage of unemployed male population (2011 Census)
#Get label descriptors
unemployment_labels <- labels[, c(1,3)]
unemployment_labels <- filter(unemployment_labels, Sequential %in% c("B5495", "B5496"))
#unemployment_labels

#Get data for labels
unemployment_data <- labels_data.new[, c("region_id", "LGA", "B5495", "B5496")]
#View(unemployment_data)
#Sum data for each row variable
ncol(unemployment_data)
#unemployment_data$Total <- rowSums(unemployment_data[, c(3:4)])
#unemployment_data$Total <- as.integer(as.double(unemployment_data$Total))

unemployment_data$Unemployment_Male_ppt <- (unemployment_data$B5495/100)
unemployment_data$Unemployment_Female_ppt <- (unemployment_data$B5496/100)
head(unemployment_data)

#Percentage of different address one year ago (2011 Census)
#Get label descriptors
address_labels <- labels[, c(1,3)]
address_labels <- filter(address_labels, Sequential %in% c("B5531", "B5532", "B5573", "B5574"))
#address_labels

#Get data for labels
address_data <- labels_data.new[, c("region_id", "LGA", "B5531", "B5532", "B5573", "B5574")]
#View(address_data)
#Sum data for each row variable
ncol(address_data)
address_data$Total <- rowSums(address_data[, c(3:6)])
address_data$Total <- as.integer(as.double(address_data$Total))

address_data$same_add_males_ppt <- (address_data$B5531/address_data$Total)
address_data$same_add_females_ppt <- (address_data$B5532/address_data$Total)
address_data$diff_add_males_ppt <- (address_data$B5573/address_data$Total)
address_data$diff_add_females_ppt <- (address_data$B5574/address_data$Total)


#Percentage of population income levels (2011 Census)
#Get label descriptors
income_labels <- labels[, c(1,3)]
income_labels <- filter(income_labels, Sequential %in% c("B3393", "B3403", "B3413", "B3423", "B3433", "B3443", "B3453", "B3463",
                                                         "B3473", "B3483", "B3493"))
#income_labels

#Get data for labels
income_data <- labels_data.new[, c("region_id", "LGA", "B3393", "B3403", "B3413", "B3423", "B3433", "B3443", "B3453", "B3463", "B3473",
                               "B3483", "B3493")]
#View(income_data)
#Sum data for each row variable
ncol(income_data)
income_data$Total <- rowSums(income_data[, c(3:13)])
income_data$Total <- as.integer(as.double(income_data$Total))

income_data$Nil_Income_ppt <- (income_data$B3393/income_data$Total)
income_data$x100_199_ppt <- (income_data$B3403/income_data$Total)

income_data$x200_299_ppt <- (income_data$B3413/income_data$Total)
income_data$x300_399_ppt <- (income_data$B3423/income_data$Total)
income_data$x400_599ppt <- (income_data$B3433/income_data$Total)
income_data$x600_799ppt <- (income_data$B3443/income_data$Total)

income_data$x800_999ppt <- (income_data$B3453/income_data$Total)
income_data$x1000_1249ppt <- (income_data$B3463/income_data$Total)
income_data$x1250_1499ppt <- (income_data$B3473/income_data$Total)
income_data$x1500_1999ppt <- (income_data$B3483/income_data$Total)
income_data$x2000_plusppt <- (income_data$B3493/income_data$Total)


#Create merged income group
#Weekly Income x200-799
income_data$x200_799 <- rowSums(income_data[,c('B3413','B3423','B3433','B3443')])
income_data$x200_799_ppt <- rowSums(income_data[,c('B3413','B3423','B3433','B3443')]/income_data$Total)

#Create merged income group
#Weekly Income x800-1250
income_data$x800_1249 <- rowSums(income_data[,c('B3453','B3463')])
income_data$x800_1249_ppt <- rowSums(income_data[,c('B3453','B3463')]/income_data$Total)

#Create merged income group
#Weekly Income x1500_plus
income_data$x1500_plus <- rowSums(income_data[,c('B3483','B3493')])
income_data$x1500_plus_ppt <- rowSums(income_data[,c('B3483','B3493')]/income_data$Total)

#Percentage of total payments on disability payments (DSS 2016)
#Get label descriptors
#dss <- read.csv('DSS_DATA_EDIT.csv', header=TRUE, sep=",", na.strings="")
#dss["Total_Payments"] <- rowSums(dss[,3:28])

#Get summed data across all DSS payments for each LGA region
#dss_total <- dss[, c(1:2,29)]
#dss_total <- join(dv_pop_order, dss_total, by="region_id", type="inner")
#View(dss_total)
#names(dss_total)
#dss_total$Total_Payments <- as.integer(as.double(dss_total$Total_Payments))
#dss_total <- dss_total[,c(1:2,57)]

#dss_total$Sum_Total_Payments <- sum(dss_total$Total_Payments)
#dss_total$Total_Payments_ppt <- (dss_total$Total_Payments/dss_total$Sum_Total_Payments)


###Summary of dataframes after data manipulation
#Dependent variables
dv_pop_order

#Independent variables
#Born overseas data
write.csv(born_overseas_data, "born_overseas_data.csv")
#Indigenous data
write.csv(indigenous_data, "indigenous_data.csv")
#sole_parents_data
write.csv(sole_parents_data, "sole_parents_data.csv")
#Rental data
write.csv(rental_data, "rental_data.csv")
#Unemployment data
write.csv(unemployment_data, "unemployment_data.csv")
#Address
#address_data
#Income
write.csv(income_data, "income_data.csv")



###*********************###
### Exploring the data ###
###********************###

##Exploring density distributions for each year for dependent variable of domestic violence
hist_1999 <- hist(dv_pop_order$yr1999pct, freq=FALSE, breaks=50, main="Density Plot Year 1999",
        xlab="Year 1999", col="lightgreen")
curve(dnorm(x, mean=mean(dv_pop_order$yr1999pct), sd=sd(dv_pop_order$yr1999pct)),
        add=TRUE, col="darkblue", lwd=2)

hist_2000 <- hist(dv_pop_order$yr2000pct, freq=FALSE, breaks=50, main="Density Plot Year 2000",
                  xlab="Year 2000", col="lightgreen")
curve(dnorm(x, mean=mean(dv_pop_order$yr2000pct), sd=sd(dv_pop_order$yr2000pct)),
      add=TRUE, col="darkblue", lwd=2)

hist_2001 <- hist(dv_pop_order$yr2001pct, freq=FALSE, breaks=50, main="Density Plot Year 2001",
                  xlab="Year 2001", col="lightgreen")
curve(dnorm(x, mean=mean(dv_pop_order$yr2001pct), sd=sd(dv_pop_order$yr2001pct)),
      add=TRUE, col="darkblue", lwd=2)

hist_2002 <- hist(dv_pop_order$yr2002pct, freq=FALSE, breaks=50, main="Density Plot Year 2002",
                  xlab="Year 2002", col="lightgreen")
curve(dnorm(x, mean=mean(dv_pop_order$yr2002pct), sd=sd(dv_pop_order$yr2002pct)),
      add=TRUE, col="darkblue", lwd=2)

hist_2003 <- hist(dv_pop_order$yr2003pct, freq=FALSE, breaks=50, main="Density Plot Year 2003",
                  xlab="Year 2003", col="lightgreen")
curve(dnorm(x, mean=mean(dv_pop_order$yr2003pct), sd=sd(dv_pop_order$yr2003pct)),
      add=TRUE, col="darkblue", lwd=2)

hist_2004 <- hist(dv_pop_order$yr2004pct, freq=FALSE, breaks=50, main="Density Plot Year 2004",
                  xlab="Year 2004", col="lightgreen")
curve(dnorm(x, mean=mean(dv_pop_order$yr2004pct), sd=sd(dv_pop_order$yr2004pct)),
      add=TRUE, col="darkblue", lwd=2)

hist_2005 <- hist(dv_pop_order$yr2005pct, freq=FALSE, breaks=50, main="Density Plot Year 2005",
                  xlab="Year 2005", col="lightgreen")
curve(dnorm(x, mean=mean(dv_pop_order$yr2005pct), sd=sd(dv_pop_order$yr2005pct)),
      add=TRUE, col="darkblue", lwd=2)

hist_2006 <- hist(dv_pop_order$yr2006pct, freq=FALSE, breaks=50, main="Density Plot Year 2006",
                  xlab="Year 2006", col="lightgreen")
curve(dnorm(x, mean=mean(dv_pop_order$yr2006pct), sd=sd(dv_pop_order$yr2006pct)),
      add=TRUE, col="darkblue", lwd=2)

hist_2007 <- hist(dv_pop_order$yr2007pct, freq=FALSE, breaks=50, main="Density Plot Year 2007",
                  xlab="Year 2007", col="lightgreen")
curve(dnorm(x, mean=mean(dv_pop_order$yr2007pct), sd=sd(dv_pop_order$yr2007pct)),
      add=TRUE, col="darkblue", lwd=2)

hist_2008 <- hist(dv_pop_order$yr2008pct, freq=FALSE, breaks=50, main="Density Plot Year 2008",
                  xlab="Year 2008", col="lightgreen")
curve(dnorm(x, mean=mean(dv_pop_order$yr2008pct), sd=sd(dv_pop_order$yr2008pct)),
      add=TRUE, col="darkblue", lwd=2)

hist_2009 <- hist(dv_pop_order$yr2009pct, freq=FALSE, breaks=50, main="Density Plot Year 2009",
                  xlab="Year 2009", col="lightgreen")
curve(dnorm(x, mean=mean(dv_pop_order$yr2009pct), sd=sd(dv_pop_order$yr2009pct)),
      add=TRUE, col="darkblue", lwd=2)

hist_2010 <- hist(dv_pop_order$yr2010pct, freq=FALSE, breaks=50, main="Density Plot Year 2010",
                  xlab="Year 2010", col="lightgreen")
curve(dnorm(x, mean=mean(dv_pop_order$yr2010pct), sd=sd(dv_pop_order$yr2010pct)),
      add=TRUE, col="darkblue", lwd=2)

hist_2011 <- hist(dv_pop_order$yr2011pct, freq=FALSE, breaks=50, main="Density Plot Year 2011",
                  xlab="Year 2011", col="lightgreen")
curve(dnorm(x, mean=mean(dv_pop_order$yr2011pct), sd=sd(dv_pop_order$yr2011pct)),
      add=TRUE, col="darkblue", lwd=2)

hist_2012 <- hist(dv_pop_order$yr2012pct, freq=FALSE, breaks=50, main="Density Plot Year 2012",
                  xlab="Year 2012", col="lightgreen")
curve(dnorm(x, mean=mean(dv_pop_order$yr2012pct), sd=sd(dv_pop_order$yr2012pct)),
      add=TRUE, col="darkblue", lwd=2)

hist_2013 <- hist(dv_pop_order$yr2013pct, freq=FALSE, breaks=50, main="Density Plot Year 2013",
                  xlab="Year 2013", col="lightgreen")
curve(dnorm(x, mean=mean(dv_pop_order$yr2013pct), sd=sd(dv_pop_order$yr2013pct)),
      add=TRUE, col="darkblue", lwd=2)

hist_2014 <- hist(dv_pop_order$yr2014pct, freq=FALSE, breaks=50, main="Density Plot Year 2014",
                  xlab="Year 2014", col="lightgreen")
curve(dnorm(x, mean=mean(dv_pop_order$yr2014pct), sd=sd(dv_pop_order$yr2014pct)),
      add=TRUE, col="darkblue", lwd=2)

hist_2015 <- hist(dv_pop_order$yr2015pct, freq=FALSE, breaks=50, main="Density Plot Year 2015",
                  xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(dv_pop_order$yr2015pct), sd=sd(dv_pop_order$yr2015pct)),
      add=TRUE, col="darkblue", lwd=2)

hist_2016 <- hist(dv_pop_order$yr2016pct, freq=FALSE, breaks=50, main="Density Plot Year 2016",
                  xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(dv_pop_order$yr2016pct), sd=sd(dv_pop_order$yr2016pct)),
      add=TRUE, col="darkblue", lwd=2)

hist_2017 <- hist(dv_pop_order$yr2017pct, freq=FALSE, breaks=50, main="Density Plot Year 2017",
                  xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(dv_pop_order$yr2017pct), sd=sd(dv_pop_order$yr2017pct)),
      add=TRUE, col="darkblue", lwd=2)

# Check the average percentage of domestic violence offences as a percentage of the
#population over time
dv_pop_order_ppt <- dv_pop_order[,c(1:2, 41:59)]
#View(dv_pop_order_ppt)
dv_pop_mean <- colMeans(dv_pop_order_ppt[sapply(dv_pop_order_ppt, is.numeric)])*100
#Find median of average domestic violence as a percentage of the population
median_average <- median(dv_pop_mean)

plot(dv_pop_mean, type="b", main="Median Average Percentage of NSW \n Population Experiencing Domestic Violence",
     lwd=2, col="darkblue", xlab="Count of Years: 1999 to 2015", ylab="Average",
     xlim=c(0,17), ylim=c(0,0.55))
grid(NULL,NULL, lty = 6, col = "gray")
abline(h = median_average, col="orange", lwd=2, lty=2)
text(2, 0.49, "Median \n Average \n 0.45%", col = "orange", cex=0.8, font=2) 




#Exploring density distributions for independent variables

##Born Australia/ Overseas
#Males born Australia
hist_male_birth_au_ppt <- hist(born_overseas_data$male_birth_au_ppt, freq=FALSE,
                                     breaks=50, main="Density Plot Males Born Australia",
                                     xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(born_overseas_data$male_birth_au_ppt),
            sd=sd(born_overseas_data$male_birth_au_ppt)),
      add=TRUE, col="darkblue", lwd=2)

#Females born Australia
hist_female_birth_au_ppt <- hist(born_overseas_data$female_birth_au_ppt, freq=FALSE,
                               breaks=50, main="Density Plot Females Born Australia",
                               xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(born_overseas_data$female_birth_au_ppt),
            sd=sd(born_overseas_data$female_birth_au_ppt)),
      add=TRUE, col="darkblue", lwd=2)

#Males born Overseas
hist_male_birth_os_ppt <- hist(born_overseas_data$male_birth_os_ppt, freq=FALSE,
                               breaks=50, main="Density Plot Males Born Overseas",
                               xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(born_overseas_data$male_birth_os_ppt),
            sd=sd(born_overseas_data$male_birth_os_ppt)),
      add=TRUE, col="darkblue", lwd=2)

#Females born Overseas
hist_female_birth_os_ppt <- hist(born_overseas_data$female_birth_os_ppt, freq=FALSE,
                               breaks=50, main="Density Plot Females Born Overseas",
                               xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(born_overseas_data$female_birth_os_ppt),
            sd=sd(born_overseas_data$female_birth_os_ppt)),
      add=TRUE, col="darkblue", lwd=2)

##Indigenous/ Non-Indigenous Population
#Indigenous Males
hist_Total_Indig_Males_ppt <- hist(indigenous_data$Total_Indig_Males_ppt, freq=FALSE,
                                 breaks=50, main="Density Plot Indigenous Males",
                                 xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(indigenous_data$Total_Indig_Males_ppt),
            sd=sd(indigenous_data$Total_Indig_Males_ppt)),
      add=TRUE, col="darkblue", lwd=2)

#Indigenous Females
hist_Total_Indig_Females_ppt <- hist(indigenous_data$Total_Indig_Females_ppt, freq=FALSE,
                                   breaks=50, main="Density Plot Indigenous Females",
                                   xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(indigenous_data$Total_Indig_Females_ppt),
            sd=sd(indigenous_data$Total_Indig_Females_ppt)),
      add=TRUE, col="darkblue", lwd=2)

#Non-Indigenous Males
hist_Total_Non_Indig_Males_ppt <- hist(indigenous_data$Total_Non_Indig_Males_ppt, freq=FALSE,
                                   breaks=50, main="Density Plot Non-Indigenous Males",
                                   xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(indigenous_data$Total_Non_Indig_Males_ppt),
            sd=sd(indigenous_data$Total_Non_Indig_Males_ppt)),
      add=TRUE, col="darkblue", lwd=2)

#Non-Indigenous Females
hist_Total_Non_Indig_Females_ppt <- hist(indigenous_data$Total_Non_Indig_Females_ppt, freq=FALSE,
                                       breaks=50, main="Density Plot Non-Indigenous Females",
                                       xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(indigenous_data$Total_Non_Indig_Females_ppt),
            sd=sd(indigenous_data$Total_Non_Indig_Females_ppt)),
      add=TRUE, col="darkblue", lwd=2)


##Family and Sole Parent Families
#Couple/ Family with No Children
hist_Coup_Fam_No_Child_ppt <- hist(sole_parents_data$Coup_Fam_No_Child_ppt, freq=FALSE,
                                   breaks=50, main="Density Plot Couple/Family \n with No Children",
                                   xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(sole_parents_data$Coup_Fam_No_Child_ppt),
            sd=sd(sole_parents_data$Coup_Fam_No_Child_ppt)),
      add=TRUE, col="darkblue", lwd=2)

#Couple/ Family with Children
hist_Coup_Fam_W_Child_ppt <- hist(sole_parents_data$Coup_Fam_W_Child_ppt, freq=FALSE,
                                   breaks=50, main="Density Plot Couple/Family \n with Children",
                                   xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(sole_parents_data$Coup_Fam_W_Child_ppt),
            sd=sd(sole_parents_data$Coup_Fam_W_Child_ppt)),
      add=TRUE, col="darkblue", lwd=2)

#One Parent Family
hist_Sole_Parent_ppt <- hist(sole_parents_data$Sole_Parent_ppt, freq=FALSE,
                                  breaks=50, main="Density Plot Sole Parent Family",
                                  xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(sole_parents_data$Sole_Parent_ppt),
            sd=sd(sole_parents_data$Sole_Parent_ppt)),
      add=TRUE, col="darkblue", lwd=2)


##Rental accommodation
#Government rental
hist_Government_ppt <- hist(rental_data$Government_ppt, freq=FALSE,
                                         breaks=50, main="Density Plot Government Rental",
                                         xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(rental_data$Government_ppt),
            sd=sd(rental_data$Government_ppt)),
      add=TRUE, col="darkblue", lwd=2)

#Private rental
hist_Private_ppt <- hist(rental_data$Private_ppt, freq=FALSE,
                             breaks=50, main="Density Plot Private Rental",
                             xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(rental_data$Private_ppt),
            sd=sd(rental_data$Private_ppt)),
      add=TRUE, col="darkblue", lwd=2)


##Unemployment data
hist_unemp_male_ppt <- hist(unemployment_data$Unemployment_Male_ppt, freq=FALSE,
                         breaks=50, main="Density Plot Unemployment Males",
                         xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(unemployment_data$Unemployment_Male_ppt),
            sd=sd(unemployment_data$Unemployment_Male_ppt)),
      add=TRUE, col="darkblue", lwd=2)

hist_unemp_female_ppt <- hist(unemployment_data$Unemployment_Female_ppt, freq=FALSE,
                            breaks=50, main="Density Plot Unemployment Females",
                            xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(unemployment_data$Unemployment_Female_ppt),
            sd=sd(unemployment_data$Unemployment_Female_ppt)),
      add=TRUE, col="darkblue", lwd=2)


#Address data
hist_same_add_males_ppt <- hist(address_data$same_add_males_ppt, freq=FALSE,
                              breaks=50, main="Density Plot Same Address Males",
                              xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(address_data$same_add_males_ppt),
            sd=sd(address_data$same_add_males_ppt)),
      add=TRUE, col="darkblue", lwd=2)

hist_same_add_females_ppt <- hist(address_data$same_add_females_ppt, freq=FALSE,
                                breaks=50, main="Density Plot Same Address Females",
                                xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(address_data$same_add_females_ppt),
            sd=sd(address_data$same_add_females_ppt)),
      add=TRUE, col="darkblue", lwd=2)

hist_diff_add_males_ppt <- hist(address_data$diff_add_males_ppt, freq=FALSE,
                                  breaks=50, main="Density Plot Different Address Males",
                                  xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(address_data$diff_add_males_ppt),
            sd=sd(address_data$diff_add_males_ppt)),
      add=TRUE, col="darkblue", lwd=2)

hist_diff_add_females_ppt <- hist(address_data$diff_add_females_ppt, freq=FALSE,
                                breaks=50, main="Density Plot Different Address Females",
                                xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(address_data$diff_add_females_ppt),
            sd=sd(address_data$diff_add_females_ppt)),
      add=TRUE, col="darkblue", lwd=2)


##Income data
hist_Nil_Income_ppt <- hist(income_data$Nil_Income_ppt, freq=FALSE,
                                breaks=50, main="Density Plot Nil Income",
                                xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(income_data$Nil_Income_ppt),
            sd=sd(income_data$Nil_Income_ppt)),
      add=TRUE, col="darkblue", lwd=2)

hist_x100_199_ppt <- hist(income_data$x100_199_ppt, freq=FALSE,
                            breaks=50, main="Density Plot $100-$199",
                            xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(income_data$x100_199_ppt),
            sd=sd(income_data$x100_199_ppt)),
      add=TRUE, col="darkblue", lwd=2)

hist_x200_299_ppt <- hist(income_data$x200_299_ppt, freq=FALSE,
                            breaks=50, main="Density Plot $200-$299",
                            xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(income_data$x200_299_ppt),
            sd=sd(income_data$x200_299_ppt)),
      add=TRUE, col="darkblue", lwd=2)

hist_x300_399_ppt <- hist(income_data$x300_399_ppt, freq=FALSE,
                            breaks=50, main="Density Plot $300-$399",
                            xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(income_data$x300_399_ppt),
            sd=sd(income_data$x300_399_ppt)),
      add=TRUE, col="darkblue", lwd=2)

hist_x400_599ppt <- hist(income_data$x400_599ppt, freq=FALSE,
                            breaks=50, main="Density Plot $400-$599",
                            xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(income_data$x400_599ppt),
            sd=sd(income_data$x400_599ppt)),
      add=TRUE, col="darkblue", lwd=2)

hist_x600_799ppt <- hist(income_data$x600_799ppt, freq=FALSE,
                         breaks=50, main="Density Plot $600-$799",
                         xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(income_data$x600_799ppt),
            sd=sd(income_data$x600_799ppt)),
      add=TRUE, col="darkblue", lwd=2)

hist_x800_999ppt <- hist(income_data$x800_999ppt, freq=FALSE,
                         breaks=50, main="Density Plot $800-$999",
                         xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(income_data$x800_999ppt),
            sd=sd(income_data$x800_999ppt)),
      add=TRUE, col="darkblue", lwd=2)

hist_x1000_1249ppt <- hist(income_data$x1000_1249ppt, freq=FALSE,
                         breaks=50, main="Density Plot $1000-$1249",
                         xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(income_data$x1000_1249ppt),
            sd=sd(income_data$x1000_1249ppt)),
      add=TRUE, col="darkblue", lwd=2)

hist_x1250_1499ppt <- hist(income_data$x1250_1499ppt, freq=FALSE,
                           breaks=50, main="Density Plot $1250-$1499",
                           xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(income_data$x1250_1499ppt),
            sd=sd(income_data$x1250_1499ppt)),
      add=TRUE, col="darkblue", lwd=2)

hist_x1500_1999ppt <- hist(income_data$x1500_1999ppt, freq=FALSE,
                           breaks=50, main="Density Plot $1500-$1999",
                           xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(income_data$x1500_1999ppt),
            sd=sd(income_data$x1500_1999ppt)),
      add=TRUE, col="darkblue", lwd=2)

hist_x2000_plusppt <- hist(income_data$x2000_plusppt, freq=FALSE,
                           breaks=50, main="Density Plot $2000_plus",
                           xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(income_data$x2000_plusppt),
            sd=sd(income_data$x2000_plusppt)),
      add=TRUE, col="darkblue", lwd=2)


#DSS Payments
hist_Total_Payments_ppt <- hist(dss_total$Total_Payments_ppt, freq=FALSE,
                           breaks=50, main="Density Plot DSS Payments",
                           xlab="% of Population", col="lightgreen")
curve(dnorm(x, mean=mean(dss_total$Total_Payments_ppt),
            sd=sd(dss_total$Total_Payments_ppt)),
      add=TRUE, col="darkblue", lwd=2)



##Now that we have wrangled and transformed our data we can now look at the association
#between the dependent and independent variables of interest
#Tests for correlation between our independent variables and dependent variable
dv_pop_order_corr <- dv_pop_order[,c(1:2,10)] #Correlation ran over three years of 2006, 2011 and 2016
address_data_corr <- address_data[,c(1,10)]
young_women_data_corr <- young_women_data[,c(1,46, 47)]
unemployment_data_corr <- unemployment_data[,c(1,5)]
indigenous_data_corr <- indigenous_data[,c(1,10)] #Males and Females very highly correlated. Only Males choosen.
born_overseas_data_corr <- born_overseas_data[,c(1,10,12)]
sole_parents_data_corr <- sole_parents_data[,c(1,7:9)]
rental_data_corr <- rental_data[,c(1,7)]
income_data_corr <- income_data[,c(1,27,29,31)]

#young_women_data_corr <- young_women_data[,c(1,89,101)] #Very high correlation to All Indigenous Males so excluded.
names(address_data)
all_data <- join(dv_pop_order_corr, young_women_data_corr, by='region_id', type="inner")
all_data <- join(all_data, address_data_corr, by='region_id', type="inner")
all_data <- join(all_data, unemployment_data_corr, by='region_id', type="inner")
all_data <- join(all_data, indigenous_data_corr, by='region_id', type="inner")
all_data <- join(all_data, born_overseas_data_corr, by='region_id', type="inner")
all_data <- join(all_data, sole_parents_data_corr, by='region_id', type="inner")
all_data <- join(all_data, rental_data_corr, by='region_id', type="inner")
all_data <- join(all_data, income_data_corr, by='region_id', type="inner")
#all_data <- join(all_data, young_women_data_corr, by='region_id', type="inner")
names(all_data)
all_data_cor <- cor(all_data[,c(3:17)])

library(corrplot)
par(mfrow=c(1,1))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(all_data_cor, method="color", col=col(200),  
         type="upper", #order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", #tl.srt=45, #Text label color and rotation
         # Combine with significance
         #p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE
)


#Now that we know the key correlations


#Train/Test/Split
#install.packages("caret")
library(caret)

#Read in the complete Census data 2006 to 2016
born_overseas_data_complete <- read.csv("2006_16_Birth.csv", header=TRUE)
#born_overseas_data_complete <- read.csv("2006_16_Birth_Count.csv", header=TRUE)
unemployment_data_complete <- read.csv("2006_16_Employment.csv", header=TRUE)
income_data_complete <- read.csv("2006_16_Income.csv", header=TRUE)
indigenous_data_complete <- read.csv("2006_16_Indigenous.csv", header=TRUE)
rental_data_complete <- read.csv("2006_16_Rental.csv", header=TRUE)
head(rental_data_complete)
family_data_complete <- read.csv("2006_16_Family.csv", header=TRUE)
names(family_data_complete)

all_model_data <- dv_pop_order[,c(1:20,22:39)]
model_data <- join(all_model_data, born_overseas_data_complete, by="region_id", type="inner")
model_data <- join(model_data, unemployment_data_complete, by="region_id", type="inner")
model_data <- join(model_data, indigenous_data_complete, by="region_id", type="inner")
model_data <- join(model_data, rental_data_complete, by="region_id", type="inner")
model_data <- join(model_data, family_data_complete, by="region_id", type="inner")

nrow(model_data) #104
names(model_data)
model_data <- model_data[,-c(39,62,74,97,109)]


#Domestic Violence
names(dv_pop_order)
colnames(dv_pop_order)[29] <- "yr2006_pop"
model_data_dv <- melt(dv_pop_order[,c(1:2, 10:20)])
#model_data_dv <- melt(dv_pop_order[,c(1:2, 48:58)])
colnames(model_data_dv)[3] <- "Domestic_Violence"
colnames(model_data_dv)[4] <- "Domestic_Violence_Count"
head(model_data_dv)

#LGA Popuation
names(dv_pop_order)
model_data_pop <- melt(dv_pop_order[,c(1:2,29:39)])
colnames(model_data_pop)[3] <- "Population"
colnames(model_data_pop)[4] <- "Population_Count"
head(model_data_pop)

#Males born Overseas
names(born_overseas_data_complete)
head(born_overseas_data)
model_data_overseas_os <- melt(born_overseas_data_complete[,c(1:2,14:24)])
colnames(model_data_overseas_os)[3] <- "M_Born_Overseas"
colnames(model_data_overseas_os)[4] <- "M_Born_Overseas_ppt"
head(model_data_overseas_os)

#Males Unemployment
names(unemployment_data_complete)
model_data_unemployment <- melt(unemployment_data_complete)
colnames(model_data_unemployment)[3] <- "M_Unemployment"
colnames(model_data_unemployment)[4] <- "M_Unemployment_ppt"
head(model_data_unemployment)

#Male Non Indigenous Status
names(indigenous_data_complete)
model_data_non_indigenous <- melt(indigenous_data_complete[,c(1:13)])
colnames(model_data_non_indigenous)[3] <- "M_Non_Indigenous"
colnames(model_data_non_indigenous)[4] <- "M_Non_Indigenous_ppt"
head(model_data_non_indigenous)

#Male Indigenous Status
names(indigenous_data_complete)
model_data_indigenous <- melt(indigenous_data_complete[,c(1:2,14:24)])
colnames(model_data_indigenous)[3] <- "M_Indigenous"
colnames(model_data_indigenous)[4] <- "M_Indigenous_ppt"
head(model_data_indigenous)

#Rental Data
names(rental_data_complete)
model_data_rental <- melt(rental_data_complete)
colnames(model_data_rental)[3] <- "Rental_Govt"
colnames(model_data_rental)[4] <- "Rental_Govt_ppt"
head(model_data_rental)

#Family with Children
names(family_data_complete)
model_data_family_fam_w_child <- melt(family_data_complete[,c(1:13)])
colnames(model_data_family_fam_w_child)[3] <- "Family_w_Child"
colnames(model_data_family_fam_w_child)[4] <- "Family_w_Child_ppt"
head(model_data_family_fam_w_child)

#Sole Parents
names(family_data_complete)
model_data_family_sole_par <- melt(family_data_complete[,c(1:2, 14:24)])
colnames(model_data_family_sole_par)[3] <- "Sole_parent"
colnames(model_data_family_sole_par)[4] <- "Sole_parent_ppt"
head(model_data_family_sole_par)


model_data_final <- cbind(model_data_dv
                        ,model_data_pop
                        ,model_data_overseas_os
                        ,model_data_unemployment
                        ,model_data_non_indigenous
                        ,model_data_indigenous
                        ,model_data_rental
                        ,model_data_family_fam_w_child
                        ,model_data_family_sole_par
                        )
model_data_final <- model_data_final[,-c(5,6,9,10,13,14,17,18,21,22,25,26,29,30,33,34)]
names(model_data_final)
model_data_final <- model_data_final[,-c(3,5,7,9,11:15,17,19)]
names(model_data_final)

#STEP ONE: Create training and testing data partitions based on random shuffle
## 75% of the sample size
#sample_split <- floor(0.70 * nrow(model_data_final))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(model_data_final)), size = sample_split)
dataTrain <- model_data_final[train_ind, ]
dataTest <- model_data_final[-train_ind, ]

#STEP TWO: Fit the model
glm.fit <- glm(Domestic_Violence_Count~
                M_Born_Overseas_ppt
                + M_Unemployment_ppt
                + Rental_Govt_ppt
                + Family_w_Child_ppt
                + Sole_parent_ppt
                + offset(log(Population_Count))
                , family=poisson(link=log)
                #, family=quasipoisson               
                , data = dataTrain
)

summary(glm.fit)

#To calculate the p-value for the deviance goodness of fit test we simply calculate the
#probability to the right of the deviance value for the chi-squared distribution on
#77 degrees of freedom:
dev_value <- pchisq(glm.fit$deviance, df=glm.fit$df.residual, lower.tail=FALSE)
dev_value


print(fitted(glm.fit)) #returns the data after the inverse if the link function is applied
print(predict(glm.fit)) #return the data after the inverse if the link function is applied (to the same scale as the response variable)
print(all.equal(log(fitted(glm.fit)), predict(glm.fit)))


#STEP THREE: Assess the co-efficients of the model

#Residuals
#Anyone can fit a linear model in R.  The real test is analyzing the residuals (the error or the difference between actual and predicted results).
#There are four things we're looking for when analyzing residuals.
#The mean of the errors is zero (and the sum of the errors is zero)
#The distribution of the errors are normal.
#All of the errors are independent.
#Variance of errors is constant (Homoscedastic)
#In R, you pull out the residuals by referencing the model and then the resid variable inside the model.  Using the simple linear regression model (simple.fit) we'll plot a few graphs to help illustrate any problems with the model.

#The smallest p value here is associated with V1 and it also has the highest coefficient increasing odds
#We can use the coef() function to assess the coefficients for this model
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,4] #p values of the coefficient

#Now we can run the anova() function on the model to analyze the table of deviance
#V1, V2, V3, V4 significantly reduce the residual deviance. The other variables seem to improve the model less
anova(glm.fit, test="Chisq")
aov(glm.fit)

#While no exact equivalent to the R2 of linear regression exists, the McFadden R2 index can be used to assess the model fit.
#install.packages("pscl")
library(pscl)
pR2(glm.fit) #Not a bad result at 0.860


#Plot predicted versus residuals
par(mfrow = c(2, 2))
plot(glm.fit)

#Residuals Plot
#Males Born Overseas
layout(matrix(c(1,1,2,3),2,2,byrow=T))
plot(glm.fit$resid ~ dataTrain$M_Born_Overseas_ppt,
     main="Residuals\nfor Poisson Regression",
     xlab="Males Born Overseas", ylab="Residuals")
abline(h=0,lty=2,col="red")
#Histogram of Residuals
hist(glm.fit$resid, main="Histogram of Residuals", ylab="Residuals")
abline(v=0,lty=2,col="red")
#Q-Q Plot
qqnorm(glm.fit$resid)
qqline(glm.fit$resid, col="red")


#Male Unemployment
layout(matrix(c(1,1,2,3),2,2,byrow=T))
plot(glm.fit$resid ~ dataTrain$M_Unemployment_ppt,
     main="Residuals\nfor Poisson Regression",
     xlab="Males Unemployment", ylab="Residuals")
abline(h=0,lty=2,col="red")
#Histogram of Residuals
hist(glm.fit$resid, main="Histogram of Residuals", ylab="Residuals")
abline(v=0,lty=2,col="red")
#Q-Q Plot
qqnorm(glm.fit$resid)
qqline(glm.fit$resid, col="red")


#Rental Government
layout(matrix(c(1,1,2,3),2,2,byrow=T))
plot(glm.fit$resid ~ dataTrain$Rental_Govt_ppt,
     main="Residuals\nfor Poisson Regression",
     xlab="Renting from Government", ylab="Residuals")
abline(h=0,lty=2,col="red")
#Histogram of Residuals
hist(glm.fit$resid, main="Histogram of Residuals", ylab="Residuals")
abline(v=0,lty=2,col="red")
#Q-Q Plot
qqnorm(glm.fit$resid)
qqline(glm.fit$resid, col="red")


#Families with Children
layout(matrix(c(1,1,2,3),2,2,byrow=T))
plot(glm.fit$resid ~ dataTrain$Family_w_Child_ppt,
     main="Residuals\nfor Poisson Regression",
     xlab="Families with Children", ylab="Residuals")
abline(h=0,lty=2,col="red")
#Histogram of Residuals
hist(glm.fit$resid, main="Histogram of Residuals", ylab="Residuals")
abline(v=0,lty=2,col="red")
#Q-Q Plot
qqnorm(glm.fit$resid)
qqline(glm.fit$resid, col="red")


#Single Parent Families
layout(matrix(c(1,1,2,3),2,2,byrow=T))
plot(glm.fit$resid ~ dataTrain$Sole_parent_ppt,
     main="Residuals\nfor Poisson Regression",
     xlab="Single Parent Families", ylab="Residuals")
abline(h=0,lty=2,col="red")
#Histogram of Residuals
hist(glm.fit$resid, main="Histogram of Residuals", ylab="Residuals")
abline(v=0,lty=2,col="red")
#Q-Q Plot
qqnorm(glm.fit$resid)
qqline(glm.fit$resid, col="red")


#install.packages("fBasics")
library(fBasics)
jarqueberaTest(glm.fit$resid)
#Test residuals for normality
#Null Hypothesis: Skewness and Kurtosis are equal to zero
#Residuals X-squared: 8.4099 p Value: 0.01492
#With a p-value of <2.2e-16 we reject the null hypothesis that the skewness and kurtosis of
#residuals are statistically equal to zero.


#Residuals are normally distributed
#The histogram and QQ-plot are the ways to visually evaluate if the residual fit a normal distribution.
#If the histogram looks like a bell-curve it might be normally distributed.
#If the QQ-plot has the vast majority of points on or very near the line, the residuals may be normally distributed.
#The plots don't seem to be very close to a normal distribution, but we can also use a statistical test.
#The Jarque-Bera test (in the fBasics library, which checks if the skewness and kurtosis of your residuals are similar to that of a normal distribution.
#The Null hypothesis of the jarque-bera test is that skewness and kurtosis of your data are both equal to zero (same as the normal distribution).


#STEP. FOUR: Assessing the predictive ability of the model
#In the steps above, we briefly evaluated the fitting of the model, now we would like to see how the model
#is doing when predicting y on a new set of data. By setting the parameter type='response',
#R will output probabilities in the form of P(y=1|X). Our decision boundary will be 0.5.
#If P(y=1|X) > 0.5 then y = 1 otherwise y=0. Note that for some applications different thresholds could
#be a better option.

fitted.results <- predict(glm.fit, newdata = dataTest, type='response')
head(fitted.results)

#Checking the mean of the fitted results versus the actual
fitted_mean <- mean(fitted.results)
actual_mean <- mean(model_data_final[,4])
fitted_mean - actual_mean


##Cross-validation check of the model
#install.packages("mlbench")
library(mlbench)
library(caret)
fold <- createFolds(dataTrain$Domestic_Violence_Count, k=10)

preds <- vector(mode = "list")
for(i in 1:length(fold)){
preds <- predict(glm.fit, train=dataTrain[-fold[[i]], c(5)], test=dataTest[fold[[i]], c(5)],
                cl=model_data_final$region_id[-fold[[i]]], type='response')
}

#Convert to dataframe
preds <- as.data.frame(preds)
head(preds)

#Prediction
output = data.frame(dataTrain, pred=glm.fit$fitted)
colnames(output)[21] <- "Prediction"
output$Difference <- (output$Domestic_Violence_Count-output$Prediction)
output <- output[,c(1:4,21:22)]
output = output[output$Domestic_Violence_Count > 0,]
head(output)
output$Accuracy <- (output$Difference/output$Domestic_Violence_Count)*100
mean(output$Accuracy)


#Plot Actual versus Predicted
par(mfrow = c(1,1))
newdata = dataTest
ggplot(data = output, aes(x=Domestic_Violence_Count, y=Prediction)) +
        geom_point(alpha=0.2, colour="darkblue", size=2.5) +
        labs(x="Actual Domestic Violence Incidents",
             y="Predicted Domestic Violence Incidents") +
        labs(title = "Actual vs Predicted Domestic Violence Incidents") +
        theme(plot.title = element_text(hjust = 0.5, vjust = 3)) +
        labs(caption = "(Based on Census Data 2006, 2011 and 2016)")
