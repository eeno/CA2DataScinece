#Section 1
#A

rm(NI_postcode)

#import data
NI_postcode <- read.csv("NIPostcodes.csv", header = FALSE)

#total number of rows
nrow(NI_postcode)
#structure of data frame
str(NI_postcode)
#show top 10 rows
head(NI_postcode, n=10)


#B
#add titles to the attributes

#create a vector of column names
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
 
#use names function to apply all names form col_names vector to dataset
names(NI_postcode)[1:15] <- col_names
str(NI_postcode)


#C
#replace blanks with NA
NI_postcode[NI_postcode == ""] <- NA
str(NI_postcode)


#D
#find the empty vlaues in each column
#Use sapply to iterate over all columns and get the sum of the blank values
blank_count <- sapply(NI_postcode, function(y) sum(length(y[y == ""])))
#convert to dataframe for easy interpratation
blank_count <- data.frame(blank_count)
#view the blank counts
blank_count

#find the na vlaues in each column
#Use sapply to iterate over all columns and get the sum of the NA values
na_count <-sapply(NI_postcode, function(y) sum(length(which(is.na(y)))))
#convert to dataframe for easy interpratation
na_count <- data.frame(na_count)
na_count


#E
#Use subset to move primary key to begining
NI_postcode <- subset(NI_postcode, select=c(15,1:14))
str(NI_postcode)


#f
#display onlyinfromation where locality, townland and town conatin Limavady
#using indexing rather than "$" to refernce columns as "grepl" will try and run the comparison operator
#on each of the entries in the columns
Limavady_data <- NI_postcode[grepl("LIMAVADY", NI_postcode[["Locality"]]) & 
                               grepl("LIMAVADY", NI_postcode[["Townland"]]) &
                               grepl("LIMAVADY", NI_postcode[["Town"]]),]

# reset index
rownames(Limavady_data) <- 1:nrow(Limavady_data)
#count rows
nrow(Limavady_data)

str(Limavady_data)

#save in a csv
write.csv(Limavady_data,"Limavady.csv")


#g
#save the modified postcode data to working directory 
write.csv(NI_postcode,"CleanNIPostcodeData.csv")


#Section 2 ----------------------------------------------------------------------------------------------------

#A amalgamate all the crime data from each csv file

#let user choose the directory
directory <- choose.dir(getwd(), caption = "Select folder")
 

#list all csv files in the chosen direcotry
all_data <- list.files(directory, pattern = "*.csv", recursive = TRUE, full.names = TRUE)

#read all csv files into a datframe using do.call and rbind
AllNICrimeData <- do.call(rbind, lapply(all_data, read.csv))
 
#cont rows
nrow(AllNICrimeData)

str(AllNICrimeData)

#b
#need to drop certain columns, create a vector of column anmes
cols_to_drop <- c("Crime.ID", "Reported.By", "Falls.within", "LSOA.code", "LSOA.name",
                  "Last.outcome.category", "Context")

#drop any column in the "cols_to_drop" vector
AllNICrimeData <- AllNICrimeData[,!names(AllNICrimeData) %in% cols_to_drop]
str(AllNICrimeData)


#C 
#shorten crime type names
#convert crime type column to character to allow name change
AllNICrimeData$Crime.type <- as.character(AllNICrimeData$Crime.type)

#create a vector of crime names mentioned in CA doc
crime_list <- c("Anti-social behaviour","Bicycle theft", "Burglary","Criminal damage and arson",
                "Drugs","Other theft", "Public order","Robbery", "Shoplifting",
                "Theft from the person","Vehicle crime", "Violence and sexual offences")
#atache data frame soto cut out repetitive type
attach(AllNICrimeData)
# if the name is not in the crime_list vector then change the name to "OTCR"
AllNICrimeData$Crime.type[Crime.type != crime_list] <- "OTCR"
#Change the crime type to the abreviations
AllNICrimeData$Crime.type[Crime.type == "Anti-social behaviour"] <- "ASBO"
AllNICrimeData$Crime.type[Crime.type == "Bicycle theft"] <- "BITH"
AllNICrimeData$Crime.type[Crime.type == "Burglary"] <- "BURG"
AllNICrimeData$Crime.type[Crime.type == "Criminal damage and arson"] <- "CDAR"
AllNICrimeData$Crime.type[Crime.type == "Drugs"] <- "DRUG"
AllNICrimeData$Crime.type[Crime.type == "Other theft"] <- "OTTH"
AllNICrimeData$Crime.type[Crime.type == "Public order"] <- "PUBO"
AllNICrimeData$Crime.type[Crime.type == "Robbery"] <- "ROBY"
AllNICrimeData$Crime.type[Crime.type == "Shoplifting"] <- "SHOP"
AllNICrimeData$Crime.type[Crime.type == "Theft from the person"] <- "THPR"
AllNICrimeData$Crime.type[Crime.type == "Vehicle crime"] <- "VECR"
AllNICrimeData$Crime.type[Crime.type == "Violence and sexual offences"] <- "VISO"
#detach dataframe
detach(AllNICrimeData)
 #check unique names to make sure all names were changed
unique(AllNICrimeData$Crime.type)
#d

#create a frequncy table for crimes
CrimeFreq <- table(AllNICrimeData$Crime.type)

#create a proprtion table of crime
CrimeFreq <- prop.table(CrimeFreq)
CrimeFreq


 plot(  table(AllNICrimeData$Crime.type) ,  #CrimeFreq
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
drop_words <- c("On or near ")

#use gsub fuunction to replace the words in drop vector vector with ""
AllNICrimeData$Location <-  gsub(drop_words, "" ,AllNICrimeData$Location)


#replace blank values with na
AllNICrimeData$Location[AllNICrimeData$Location == ""] <- NA



sum(is.na(AllNICrimeData$Location))
str(AllNICrimeData)






#f 
#choose 5000 random varaibles and create a function to return town name that isnt blank
#create a new dataframe with where there are no NA's in the location columns
new_df <- AllNICrimeData[!is.na(AllNICrimeData$Location),]
#set the seed ot 100
set.seed(100)
#sample 5000 random rows from the dataset with no NA's in the location 
#call ne dataframe random_crime_sample
random_crime_sample <-  new_df[sample(1:nrow(new_df), 5000),]

random_crime_sample
unique(random_crime_sample$Location)
unique(random_crime_sample$Crime.type)



#read in cleanNIPostcode csv file
CleanNIPostcodeData <- read.csv("CleanNIPostcodeData.csv")

#custom function to find a town
find_a_town <- function(baseframe, lookupframe){
  
  #convert the location in the lookup in baseframe (random_crime_sample) to upper case 
  #for matching purposes as exact matches will only be returned
  #used eval.parent as R does not allow for staright dataframe manipulation with a custom function
  eval.parent(substitute( baseframe$Location <- assign(baseframe$Location,toupper(baseframe$Location))))
  
  #use macth function to lookup the town in the baseframe (random_crime_sample) against
  #Thotfare in lookupframe (CleanNIPostcodeData)
  town_Loc <- lookupframe$Town[match(baseframe$Location, lookupframe$Primary.Thorfare)]
  #add Town column
  eval.parent(substitute(baseframe["Town"] <- town_Loc))
  
  return(baseframe)
  
}

#call function with a base frame and a lookupp frame
find_a_town(random_crime_sample, CleanNIPostcodeData)

str(random_crime_sample)



#g 

#create a function to combine ni crime data and poulation form a village list 
#to the random crime data
#read in village data
village <- read.csv("VillageList.csv")
#change the irst column to lookup for eas of lookup
names(village)[1] <- "Location"

#custom function to return town info
get_town_info <- function(baseframe,lookupframe){
  #convert the location in the lookup frame (village) to upper case
  lookupframe$Location <- toupper((lookupframe$Location))
  #eval.parent(substitute( baseframe$Location <- assign(baseframe$Location,toupper(baseframe$Location))))
  
  #use macth function to lookup the town in the baseframe (random_crime_sample) against
  #location in lookup frame (village) to return the population value
  pop_loc <- lookupframe$POPULATION[match(baseframe$Town, lookupframe$Location)]
  #add population colum to the baseframe  
  eval.parent(substitute(baseframe["Population"] <- pop_loc))
  
  return(baseframe)
  
}
#call function with baseframe and lookupframe
get_town_info(random_crime_sample,village)


str(random_crime_sample)


#h
#drop the "reported by" column
random_crime_sample <-  subset(random_crime_sample, select=c(1,3:8))
#change "town" column to "City-Town-Village"
names(random_crime_sample)[6] <- "City-Town-Village"

#write the dataframe ot working directory
write.csv(random_crime_sample,"random_crime_sample.csv")


random_crime_sample$Town <- as.character(random_crime_sample$Town)


#i
#split data into belfats data and derry data

#create a dataframe containing belfast data only
Belfast_data <- random_crime_sample[random_crime_sample$`City-Town-Village` == "BELFAST",]
#take only complete cases
Belfast_data <- Belfast_data[complete.cases(Belfast_data$`City-Town-Village`),]
#create a table  of crimetype counts using  belfast data
Belfast_Freq <- table(Belfast_data$Crime.type )

#creare a dataframe containing belfast data only
Derry_data <- random_crime_sample[random_crime_sample$`City-Town-Village` == "LONDONDERRY",]
#take only complete cases
Derry_data <- Derry_data[complete.cases(Derry_data$`City-Town-Village`),]
#create a table  of crimetype counts using  derry data data
Derry_Freq <- table(Derry_data$Crime.type )

str(random_crime_sample)
#using par to display 2 charts side by side
#displaying in a 1X2 array
par(mfrow=c(1,2))

#plot the belfast data
plot(  Belfast_Freq ,  
       type = "h", #plot as histogram
       main = "Frequency by Crime Type Belfast", 
       ylab = "Frequency", 
       xlab = "Crime Type",
       col = "blue")



#plot the derry data
plot(  Derry_Freq ,  
       type = "h", #plot as histogram
       main = "Frequency by Crime Type Derry", 
       ylab = "Frequency", 
       xlab = "Crime Type",
       col = "red")



