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
               "Number2",
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

my_na  <- NI_postcode[!complete.cases(NI_postcode),]
nrow(my_na)

NI_postcode[NI_postcode == ""] <- NA
str(NI_postcode)

