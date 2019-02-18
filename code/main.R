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

if(!require(DataExplorer)){
  install.packages("DataExplorer")
}
library(DataExplorer)

if(!require(lmtest)){
  install.packages("lmtest")
}
library(lmtest)

source(here("code","function_convert_CSV_to_vector.R"))
# step_2: import the raw data of ss16pal into the dataframe as "base"
#load the persional data to variable Base
#load the persional data to variable Base
base <- read_csv(here('raw_data','ss16pal.csv'))%>% 
  dplyr::select(SERIALNO, #Serial Number to link House Hold and Personal data
         AGEP,  # Age: 0#99
         SEMP,  # Self-employment income past 12 months (signed)
         WAGP,  # Wages or salary income past 12 months
         HISP,  # Recoded detailed Hispanic origin
         NATIVITY,  # Nativity
         PERNP, # Total person's earnings
         RAC1P, # Recoded detailed race code
         RACAIAN, # American Indian and Alaska Native recode
         RACASN,  # Asian recode
         RACBLK,  # Black or African American recode
         RACNH, # Native Hawaiian recode
         RACWHT,  # White recode
         SEX,     # SEX
         SCHL,   # Educational attainment
         WAOB,  # World area of birth
         WKW)   #Weeks worked during past 12 months

# step_3: import the raw data of ss16hal into the dataframe then merger it into "base"
#Link the house hould data
#Link the house hould data
househould <- read_csv(here('raw_data','ss16hal.csv'))%>% 
  dplyr::select(SERIALNO, #Serial Number to link House Hold and Personal data
         ACCESS # Access to the Internet
  )
base <- base %>% left_join(househould) %>% dplyr::select(-SERIALNO)
rm(househould)

#conver the catagorical variable type
base <- base %>% mutate(HISP= as.factor(HISP),
                        RAC1P= as.factor(RAC1P),
                        RACAIAN = as.factor(RACAIAN),
                        RACASN = as.factor(RACASN),
                        RACBLK = as.factor(RACBLK),
                        RACNH = as.factor(RACNH),
                        RACWHT = as.factor(RACWHT),
                        SEX = as.factor(SEX),
                        SCHL = as.factor(SCHL),
                        WAOB = as.factor(WAOB),
                        WKW = as.factor(WKW),
                        ACCESS = as.factor(ACCESS),
                        NATIVITY = as.factor(NATIVITY)
)

#conver the numeric variable type
base <- base %>% mutate(AGEP = as.numeric(AGEP),
                        SEMP= as.numeric(SEMP),
                        WAGP= as.numeric(WAGP),
                        PERNP= as.numeric(PERNP))
levels(base$ACCESS) <- convertCSV2Factor("ACCESS")
levels(base$HISP) <- convertCSV2Factor("HISP")
levels(base$RAC1P) <- convertCSV2Factor("RAC1P")
levels(base$RACAIAN) <- convertCSV2Factor("RACAIAN")
levels(base$RACASN) <- convertCSV2Factor("RACASN")
levels(base$RACBLK) <- convertCSV2Factor("RACBLK")
levels(base$RACNH) <- convertCSV2Factor("RACNH")
levels(base$RACWHT) <- convertCSV2Factor("RACWHT")
levels(base$SEX) <- convertCSV2Factor("SEX")
levels(base$SCHL) <- convertCSV2Factor("SCHL")
levels(base$WKW) <- convertCSV2Factor("WKW")
levels(base$WAOB) <- convertCSV2Factor("WAOB")
levels(base$NATIVITY) <- convertCSV2Factor("NATIVITY")


base <- base %>% 
  mutate( age_group = case_when(
    AGEP < 14 ~ "Other ages (exclude)",
    AGEP >= 14 & AGEP <= 62 ~ "Age 14-62 (include)",
    AGEP > 62 ~ "Other ages (exclude)"
  ))
base$age_group <- as.factor(base$age_group)

base <- base %>%  filter(AGEP>=14 & AGEP <= 62)
base$age_group <- NULL
base <- base %>%  filter(WAGP > 0)

base <- base %>% 
  filter(WKW %in% c("50 to 52 weeks worked during past 12 months", 
                    "48 to 49 weeks worked during past 12 months", 
                    "40 to 47 weeks worked during past 12 months"))


base_model <- base %>%  
  filter(!is.na(ACCESS) & ACCESS != "Paid Access") %>%
  mutate(free_access = ACCESS == "Free Access",
         hispanic = HISP != "Not Spanish/Hispanic/Latino",
         black = RACBLK == "Yes",
         female = SEX =="Female",
         low_education = as.integer(SCHL) <= 7,
         some_education = as.integer(SCHL) > 7 & as.integer(SCHL) <= 14,
         high_school = as.integer(SCHL) > 14 & as.integer(SCHL) <= 16,
         undergrad_or_higher = as.integer(SCHL) > 16,
         age = AGEP,
         age_square = AGEP^2
         
  ) 
model_01 <- lm(WAGP ~ free_access, data = base_model)

model_01 %>% summary()

model_02 <- lm(WAGP ~ free_access + hispanic + black + female + AGEP + I(AGEP^2), data = base_model)
model_02 %>% summary()


base_model <- base_model %>% mutate(ln_earnings = log(WAGP))

model_03 <- lm(ln_earnings ~ free_access, data = base_model)

model_03 %>% summary()

model_04 <- lm(ln_earnings ~ free_access + hispanic + black + female + AGEP + I(AGEP^2),
               data = base_model)
model_04 %>% summary()

#Conduct BPG Test
bptest(model_01)
bptest(model_02)
bptest(model_03)
bptest(model_04)

explanatory = c("free_access","hispanic","black", "female" ,"age" , "age_square",
                "some_education" , "high_school" , "undergrad_or_higher")
dependent = "ln_earnings" 
base_model %>%
  summary_factorlist(dependent, explanatory, p=TRUE, add_dependent_label=TRUE)

summary(base_model)
