library(here)
library(tidyverse)

base <- read_csv(here('raw_data','ss16pal.csv'))%>% 
  select(AGEP,  # Age: 0#99
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
         WAOB)  # World area of birth
base <- base %>% mutate(AGEP = as.numeric(AGEP),
                        JWMNP = as.numeric(JWMNP),
                        JWRIP = as.numeric(JWRIP),
                        JWTR = as.numeric(JWTR),
                        SCHL= as.numeric(SCHL),
                        SEMP= as.numeric(SEMP),
                        WAGP= as.numeric(WAGP),
                        HISP= as.numeric(HISP),
                        PERNP= as.numeric(PERNP),
                        RAC2P= as.numeric(RAC2P),
                        RAC3P = as.numeric(RAC3P)
                        )

base %>% summary()
