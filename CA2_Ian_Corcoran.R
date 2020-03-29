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

Limavady_data <- 


