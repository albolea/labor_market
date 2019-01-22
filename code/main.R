# step_1: clean the environment and import the library
# Clear environment
rm(list = ls(all = TRUE))

# check if package is installed, if not installed then load
if(!require(here)){
  install.packages("here")
}
library(here)

if(!require(tidyverse)){
  install.packages("tidyverse")
}
library(tidyverse)

if(!require(tidyverse)){
  install.packages("DataExplorer")
}
library(DataExplorer)
source(here("code","function_convert_CSV_to_vector.R"))
# step_2: import the raw data of ss16pal into the dataframe as "base"
#load the persional data to variable Base
base <- read_csv(here('raw_data','ss16pal.csv'))%>% 
  select(SERIALNO, #Serial Number to link House Hold and Personal data
         AGEP,  # Age: 0#99
         CIT,   # Citizenship status
         COW,   # Class of worker
         DDRS,  # Self#care difficulty
         DEAR,  # Hearing difficulty
         DEYE,  # Vision difficulty
         DOUT,  # Independent living difficulty
         DPHY,  # Ambulatory difficulty
         DRAT,  # Veteran service connected disability rating (percentage)
         DRATX, # Veteran service connected disability rating (checkbox)
         DREM,  # Cognitive difficulty
         ENG,   # Ability to speak English
         JWMNP, # Travel time to work
         JWRIP, # Vehicle occupancy
         JWTR,  # Means of transportation to work
         MAR,   # Marital status
         MIL,   # Military service
         NWLK,  # Looking for work
         SCHL,  # Educational attainment
         SEMP,  # Self-employment income past 12 months (signed)
         SEX,   # Sex
         WAGP,  # Wages or salary income past 12 months
         ESR,   # Employment status recode
         HISP,  # Recoded detailed Hispanic origin
         NATIVITY,  # Nativity
         PERNP, # Total person's earnings
         RAC1P, # Recoded detailed race code
         RAC2P, # Recoded detailed race code
         RAC3P, # Recoded detailed race code
         RACAIAN, # American Indian and Alaska Native recode
         RACASN,  # Asian recode
         RACBLK,  # Black or African American recode
         RACNH, # Native Hawaiian recode
         RACWHT,  # White recode
         WAOB,  # World area of birth
         WKW)   #Weeks worked during past 12 months

# step_3: import the raw data of ss16hal into the dataframe then merger it into "base"
#Link the house hould data
househould <- read_csv(here('raw_data','ss16hal.csv'))%>% 
  select(SERIALNO, #Serial Number to link House Hold and Personal data
         TYPE, # Type of house
         ACCESS, # Access to the Internet
         BROADBND, # Cellular data plan for a smartphone or other mobile device
         COMPOTHX, # Other computer equipment
         HISPEED, #Broadband (high speed) Internet service such as cable, fiber optic, or DSL service
         LAPTOP, # Laptop or desktop
         OTHSVCEX, # Other Internet service
         SATELLITE, # Satellite Internet service
         SMARTPHONE, # Smartphone
         TABLET # Tablet or other portable wireless computer
  )
base <- base %>% left_join(househould) %>% select(-SERIALNO)
rm(househould)

# step_4: a peek of data structure, then convert the data type accordingly.
str(base)

# step_5: convert the categorical data into factor type and numetical data into numeric type. The factorial data in this dataframe will keep its representative code instead of the category name to save the memory space. 
#conver the catagorical variable type
base <- base %>% mutate(CIT = as.factor(CIT),
                        COW = as.factor(COW),
                        DDRS = as.factor(DDRS),
                        DEAR = as.factor(DEAR),
                        DEYE = as.factor(DEYE),
                        DOUT = as.factor(DOUT),
                        DPHY = as.factor(DPHY),
                        DRAT = as.factor(DRAT),
                        DRATX = as.factor(DRATX),
                        DREM = as.factor(DREM),
                        ENG = as.factor(ENG),
                        JWRIP = as.factor(JWRIP),
                        JWTR = as.factor(JWTR),
                        MAR = as.factor(MAR),
                        MIL = as.factor(MIL),
                        NWLK = as.factor(NWLK),
                        SCHL = as.factor(SCHL),
                        SEX = as.factor(SEX),
                        ESR = as.factor(ESR),
                        HISP= as.factor(HISP),
                        RAC1P= as.factor(RAC1P),
                        RAC2P= as.factor(RAC2P),
                        RAC3P= as.factor(RAC3P),
                        RACAIAN = as.factor(RACAIAN),
                        RACASN = as.factor(RACASN),
                        RACBLK = as.factor(RACBLK),
                        RACNH = as.factor(RACNH),
                        RACWHT = as.factor(RACWHT),
                        WAOB = as.factor(WAOB),
                        WKW = as.factor(WKW),
                        TYPE = as.factor(TYPE),
                        ACCESS = as.factor(ACCESS),
                        BROADBND = as.factor(BROADBND),
                        COMPOTHX = as.factor(COMPOTHX),
                        HISPEED = as.factor(HISPEED),
                        LAPTOP = as.factor(LAPTOP),
                        OTHSVCEX = as.factor(OTHSVCEX),
                        SATELLITE = as.factor(SATELLITE),
                        SMARTPHONE = as.factor(SMARTPHONE),
                        TABLET = as.factor(TABLET)
)

#conver the numeric variable type
base <- base %>% mutate(AGEP = as.numeric(AGEP),
                        JWMNP = as.numeric(JWMNP),
                        SEMP= as.numeric(SEMP),
                        WAGP= as.numeric(WAGP),
                        PERNP= as.numeric(PERNP)
)
levels(base$ACCESS) <- convertCSV2Factor("ACCESS")
levels(base$BROADBND) <- convertCSV2Factor("BROADBND")
levels(base$CIT) <- convertCSV2Factor("CIT")
levels(base$COMPOTHX) <- convertCSV2Factor("COMPOTHX")
levels(base$COW) <- convertCSV2Factor("COW")
levels(base$DDRS) <- convertCSV2Factor("DDRS")
levels(base$DEAR) <- convertCSV2Factor("DEAR")
levels(base$DEYE) <- convertCSV2Factor("DEYE")
levels(base$DOUT) <- convertCSV2Factor("DOUT")
levels(base$DOUT) <- convertCSV2Factor("DOUT")
levels(base$DPHY) <- convertCSV2Factor("DPHY")
levels(base$DRAT) <- convertCSV2Factor("DRAT")
levels(base$DRATX) <- convertCSV2Factor("DRATX")
levels(base$DREM) <- convertCSV2Factor("DREM")
levels(base$ENG) <- convertCSV2Factor("ENG")
levels(base$ESR) <- convertCSV2Factor("ESR")
levels(base$HISP) <- convertCSV2Factor("HISP")
levels(base$HISPEED) <- convertCSV2Factor("HISPEED")
levels(base$JWRIP) <- convertCSV2Factor("JWRIP")
levels(base$JWTR) <- convertCSV2Factor("JWTR")
levels(base$LAPTOP) <- convertCSV2Factor("LAPTOP")
levels(base$MAR) <- convertCSV2Factor("MAR")
levels(base$MIL) <- convertCSV2Factor("MIL")
levels(base$NWLK) <- convertCSV2Factor("NWLK")
levels(base$OTHSVCEX) <- convertCSV2Factor("OTHSVCEX")
levels(base$RAC1P) <- convertCSV2Factor("RAC1P")
levels(base$RAC2P) <- convertCSV2Factor("RAC2P")
levels(base$RAC3P) <- convertCSV2Factor("RAC3P")
levels(base$RACAIAN) <- convertCSV2Factor("RACAIAN")
levels(base$RACASN) <- convertCSV2Factor("RACASN")
levels(base$RACBLK) <- convertCSV2Factor("RACBLK")
levels(base$RACNH) <- convertCSV2Factor("RACNH")
levels(base$RACWHT) <- convertCSV2Factor("RACWHT")
levels(base$SATELLITE) <- convertCSV2Factor("SATELLITE")
levels(base$SCHL) <- convertCSV2Factor("SCHL")
levels(base$SEX) <- convertCSV2Factor("SEX")
levels(base$SMARTPHONE) <- convertCSV2Factor("SMARTPHONE")
levels(base$TABLET) <- convertCSV2Factor("TABLET")
levels(base$WAOB) <- convertCSV2Factor("WAOB")
levels(base$WKW) <- convertCSV2Factor("WKW")

# step_6: EDA
## Step_6.1 EDA - Data Structure

str(base)

## Step_6.2 EDA - Data Summary
summary(base)


# Exploratory data analysis

#*Age:
 # According to the graph and table below we see that our data includes people from 0 to 99 years old.
base %>% select(AGEP) %>%  summary()
base %>% ggplot(aes(AGEP)) + geom_histogram()  

#We decided to work with people from 16 to 50 years olds as a better representations of the market labor
base <- base %>%  filter(AGEP>=16 & AGEP <= 50)
base %>% select(AGEP) %>%  summary()
base %>% ggplot(aes(AGEP)) + geom_density()  

#*Labor:  
#  We decided to exclude people:  
#  1. Working without pay in family business or farm  
#  2. Unemployed and last worked 5 years ago or earlier or never worked  
base <- base %>%  filter(as.integer(COW)<8)

#3. Worked less than 50 weeks in the last 12 months
base <- base %>%  filter(as.integer(WKW)==1)


#*Wage:  
base  %>% ggplot(aes(WAGP)) + geom_histogram()

base <- base %>%  filter(WAGP<356000 & WAGP >=5*50*40)
base  %>% ggplot(aes(WAGP)) + geom_histogram(binwidth = 5000)
base  %>% ggplot(aes(y=WAGP)) + geom_density()

View(base$WAGP)
base %>% group_by(TABLET) %>% select(WAGP) %>% summary()

base %>% filter(TABLET=="Yes") %>% select(WAGP) %>% summary() #person does have tablet
base %>% filter(TABLET=="No") %>% select(WAGP) %>% summary() #person doesn't have tablet

base %>%  ggplot(aes(WAGP,color=TABLET)) + geom_density() + 
  labs(title="Earnings x Having Tablet",
       x ="Earnings", y = "Density") +
  scale_color_discrete(name="Person has Tablet",
                       labels=c("Yes", "No", "NA"))

base %>%  ggplot(aes(x=TABLET,y=WAGP,fill=TABLET)) + geom_boxplot()+
  labs(title="Earnings x Having Tablet",
       x ="Earnings", y = "Density") +
  scale_fill_discrete(name="Experimental\nCondition",
                      breaks=c("1", "2", "NA"),
                      labels=c("Yes", "No", "NA"))

