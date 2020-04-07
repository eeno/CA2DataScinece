#Section 1
#A

NI_postcode <- read.csv("NIPostcodes.csv")

#total number of rows
nrow(NI_postcode)
#structure of data frame
str(NI_postcode)
#show top 10 rows
head(NI_postcode, n=10)


#B
#add titles to the attributes
col_names <- c("Organisation Name",
               "Sub-building Name",
               "Building Name",
               "Number",
               "Primary Thorfare",
               "Alt Thorfare",
               "Secondary Thorfare",
               "Locality",
               "Townland",
               "Town",
               "County",
               "Postcode",
               "x-coordinates",
               "y-coordinates",
               "Primary Key ,")

names(NI_postcode)[1:15] <- col_names
str(NI_postcode)


#C
#replace blanks with NA
NI_postcode[NI_postcode == ""] <- NA
str(NI_postcode)


#D
#find the na vlaues in each row
#Use sapply to iterate over all columns and get the sum of the NA values
na_count <-sapply(NI_postcode, function(y) sum(length(which(is.na(y)))))
#convert to dataframe to see the values easier
na_count <- data.frame(na_count)
na_count


#E
#Use subset to move primary key to begining
NI_postcode <- subset(NI_postcode, select=c(15,1:15))


#f
#display onlyinfromation where locality, townland and town conatin Limavady
#using indexing rather than "$" to refernce columns as "grepl" will try and run the comparison operator
#on each of the entries in the columns
Limavady_data <- NI_postcode[grepl("LIMAVADY", NI_postcode[["Locality"]]) & 
                               grepl("LIMAVADY", NI_postcode[["Townland"]]) &
                               grepl("LIMAVADY", NI_postcode[["Town"]]),]

#count rows
nrow(Limavady_data)

#save in a csv
write.csv(Limavady_data,"Limavady.csv")


#g
#save the modified postcode data
write.csv(NI_postcode,"CleanNIPostcodeData.csv")


#Section 2 ----------------------------------------------------------------------------------------------------

#Aamalgamate all the crime data from each csv


# directory where files are located
dir <- "./NI Crime Data"
#list all subfolders
folders <- list.dirs(dir,full.names = TRUE)

#list all files in the subfolders
new_data <- sapply(folders[-1], function(x) {
  df1 <- list.files(x,full.names = TRUE)
}
)

ff <- list.files(dir, pattern = "*.csv", recursive = TRUE, full.names = TRUE)

#read all csv files into a datframe
AllNICrimeData <- do.call(rbind, lapply(ff, read.csv))
 nrow(AllNICrimeData)



#b
str(AllNICrimeData)

#need to drop certain columns
cols_to_drop <- c("Crime.ID", "Reported.By", "Falls.within", "LSOA.code", "LSOA.name",
                  "Last.outcome.category", "Context")


AllNICrimeData <- AllNICrimeData[,!names(AllNICrimeData) %in% cols_to_drop]
str(AllNICrimeData)


#C shorten crime type names

AllNICrimeData$Crime.type <- as.character(AllNICrimeData$Crime.type)

crime_list <- c("Anti-social behaviour","Bicycle theft", "Burglary","Criminal damage and arson",
                "Drugs","Other theft", "Public order","Robbery", "Shoplifting",
                "Theft from the person","Vehicle crime", "Violence and sexual offences")

attach(AllNICrimeData)
AllNICrimeData$Crime.type[Crime.type != crime_list] <- "OTCR"
AllNICrimeData$Crime.type[Crime.type == "Anti-social behaviour"] <- "ASBO"
AllNICrimeData$Crime.type[Crime.type == "Bicycle theft"] <- "BITH"
AllNICrimeData$Crime.type[Crime.type == "Burglary"] <- "BURG"
AllNICrimeData$Crime.type[Crime.type == "Criminal damage and arson"] <- "CDAR"
AllNICrimeData$Crime.type[Crime.type == "Drugs"] <- "DRUG"
AllNICrimeData$Crime.type[Crime.type == "Other Theft"] <- "OTTH"
AllNICrimeData$Crime.type[Crime.type == "Public order"] <- "PUBO"
AllNICrimeData$Crime.type[Crime.type == "Robbery"] <- "ROBY"
AllNICrimeData$Crime.type[Crime.type == "Shoplifting"] <- "SHOP"
AllNICrimeData$Crime.type[Crime.type == "Theft from the person"] <- "THPR"
AllNICrimeData$Crime.type[Crime.type == "Vehicle crime"] <- "VECR"
AllNICrimeData$Crime.type[Crime.type == "Violence and sexual offences"] <- "VISO"
detach(AllNICrimeData)

unique(AllNICrimeData$Crime.type)
#d

#create a frequncy table for crimes
CrimeFreq <- table(AllNICrimeData$Crime.type)

#create a proprtion table of crime
CrimeFreq <- prop.table(CrimeFreq)
CrimeFreq


 plot( CrimeFreq,
        type = "h",
        main = "Frequency by Crime Type", 
        ylab = "Frequency", 
        xlab = "Crime Type",
        col = "blue")

 barplot( height <- CrimeFreq,
       main = "Frequency by Crime Type", 
       ylab = "Frequency", 
       xlab = "Crime Type",
       col = "blue")
 

#factor type column
AllNICrimeData$Crime.type <- factor(AllNICrimeData$Crime.type, ordered = FALSE)

str(AllNICrimeData)


#E
#drop the words on or near from location

#check the unique locations
unique(AllNICrimeData$Location)

drop_words <- c("On or near ")

AllNICrimeData$Location <-  gsub(drop_words, "" ,AllNICrimeData$Location)
#AllNICrimeData$Location <- sapply(1:nrow(AllNICrimeData), function (x) gsub(drop_words, "" ,AllNICrimeData$Location))

#replace blanks wit na
AllNICrimeData$Location[AllNICrimeData$Location == ""] <- NA
sum(is.na(AllNICrimeData$Location))
str(AllNICrimeData$Location)



#f choose 5000 random varaibles and create a function to return town name
new_df <- AllNICrimeData[!is.na(AllNICrimeData$Location),]
set.seed(100)
random_crime_sample <-  new_df[sample(1:nrow(new_df), 5000),]
random_crime_sample
unique(random_crime_sample$Location)





#g create a funtion to combine ni crime data and poulaation and status to the crime data

find_a_town <- function(baseframe, lookupframe){
  
  #assign(baseframe$Location <- as.character(baseframe$Location))
  eval.parent(substitute( baseframe$Location <- assign(baseframe$Location,toupper(baseframe$Location))))
  
  
  
  
  town_Loc <- lookupframe$Town[match(baseframe$Location, lookupframe$Primary.Thorfare)]
  eval.parent(substitute(baseframe["Town"] <- town_Loc))
  
  return(baseframe)
  
}


find_a_town(random_crime_sample, CleanNIPostcodeData)

str(CleanNIPostcodeData)

CleanNIPostcodeData <- read.csv("CleanNIPostcodeData.csv")



get_town_info <- function(df1,df2){
  
  
  
  status_Loc <- df2$STATUS[match(df1$Location, df2$Location)]  
  pop_loc    <- df2$POPULATION[match(df1$Location, df2$Location)]
    eval.parent(substitute(df1["Status"] <- status_Loc))
    eval.parent(substitute(df1["Population"] <- pop_loc))
  
  return(df1)
  
}


get_town_info(AllNICrimeData,village)

stat_Freq <- table(AllNICrimeData$Crime.type,AllNICrimeData$Status)
stat_Freq
prop.table(stat_Freq) * 100




#random_crime_sample <- AllNICrimeData[!is.na(sample(1:nrow(AllNICrimeData),1000,replace = TRUE)),]
random_crime_sample <- subset() 
