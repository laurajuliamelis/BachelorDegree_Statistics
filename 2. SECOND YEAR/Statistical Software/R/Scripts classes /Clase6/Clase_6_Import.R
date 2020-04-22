#########################################
###  Software Estadístic, 11.10.2016  ###
###  Data import from SPSS and EXCEL  ###
#########################################

## Data import from SPSS
## ---------------------
## 1. Using function read.table (of package foreign)
library(foreign)
read.spss("SPSSdata.sav")                     # Looks not so nice!
spssdata <- read.spss("SPSSdata.sav", to.data.frame = T)
spssdata
summary(spssdata)
attributes(spssdata)
attr(spssdata, "variable.labels")


## 2. Using function spss.get (Hmisc)
# install.packages("Hmisc")
library(Hmisc)
spss.get("SPSSdata.sav")
spssdata2 <- spss.get("SPSSdata.sav", lowernames = T, datevars = "bday")
spssdata2
summary(spssdata2)
str(spssdata2)
attributes(spssdata2)

# What about the variable labels?
attr(spssdata2, "variable.labels")
Label(spssdata2)
Label(spssdata)

# Labels can be modified with function label(Hmisc)
label(spssdata2$eda)
label(spssdata2$eda) <- "Edat"
Label(spssdata2)

# Functions label and Label are useful, but there are some inconveniences
fix(spssdata)
fix(spssdata2)      # :-(


## Data import from EXCEL
## ----------------------
## 1. Copy the data into the clipboard and then read them
read.table("clipboard", header = T, sep = "\t")	# "\t" is needed because of empty cells
read.delim("clipboard", header = T)               # read.delim reads the data more easily


## 2. Use of csv.get(Hmisc)
csv.get("CSVdata.csv")
csvdata <- csv.get("CSVdata.csv", sep = ";")
summary(csvdata)
str(csvdata)
# Variable bday is imported as a factor, NOT as date variable
csvdata <- csv.get("CSVdata.csv", sep = ";", datevars = "bday",
                   dateformat = "%d/%m/%Y")
summary(csvdata)
str(csvdata)


## 3. Use of contributed packages such as openxlsx, xlsx, XLConnect, or RODBC
# install.packages("openxlsx")
library(openxlsx)
(xlsxdata <- read.xlsx("EXCELdata.xlsx", 1, detectDates = T))
summary(xlsxdata)
str(xlsxdata)


## 4. Import data via the R-commander
# install.packages(Rcmdr)
library(Rcmdr)


## Data import from SAS
## --------------------
# For SAS data files we could use function sas.get(Hmisc)
# Alternative: use of packkage sas7bdat
# install.packages("sas7bdat")          # If not installed yet
library(sas7bdat)
medic <- read.sas7bdat("med.sas7bdat")
View(medic)
summary(medic)
str(medic)

# Exporting data to SAS
library(foreign)
write.foreign(spssdata2, datafile = "DadesSPSS.sav", codefile = "Example.sas",
              package = "SAS")
