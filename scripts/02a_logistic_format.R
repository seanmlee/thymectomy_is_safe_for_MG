

# libraries --------------------------------------------------------------------
library(tidyverse)


# data -------------------------------------------------------------------------
nsqip_logistic <- read.csv("out/nsqip.csv", header = TRUE) %>%
  
  # exclude mg patients who were not treated with thymectomy
  mutate(
    not_thy_for_mg = case_when(
      thy == 0 & mg == 1 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  
  filter(
    not_thy_for_mg != 1
  ) %>%
  
  dplyr::select(
    -not_thy_for_mg
  ) %>%
  
  # format mortality variable
  mutate(
    dopertod = ifelse(
      dopertod == -99,
      "Alive",
      "Deceased"
    )
  ) %>%
  
  # filter null and unknown values
  filter(
    if_all(everything(), ~ . != -99),
    age != "90+",
    sex != "NULL",
    sex != "non-binary",
    asaclas != "NULL",
    fnstatus2 != "Unknown",
  ) %>%
  
  # calculate bmi
  mutate(
    bmi = (weight/(height^2)) * 703
  ) %>%
  
  dplyr::select(
    -height,
    -weight
  )



# format class -----------------------------------------------------------------
nsqip_logistic$mg <- as.factor(nsqip_logistic$mg)
nsqip_logistic$age <- as.numeric(nsqip_logistic$age)
nsqip_logistic$sex <- as.factor(nsqip_logistic$sex)
nsqip_logistic$diabetes <- as.factor(nsqip_logistic$diabetes)
nsqip_logistic$hypermed <- as.factor(nsqip_logistic$hypermed)
nsqip_logistic$dialysis <- as.factor(nsqip_logistic$dialysis)
nsqip_logistic$asaclas <- as.factor(nsqip_logistic$asaclas)
nsqip_logistic$steroid <- as.factor(nsqip_logistic$steroid)
nsqip_logistic$othdvt <- as.factor(nsqip_logistic$othdvt)
nsqip_logistic$inout <- as.factor(nsqip_logistic$inout)
nsqip_logistic$failwean <- as.factor(nsqip_logistic$failwean)
nsqip_logistic$dopertod <- as.factor(nsqip_logistic$dopertod)
nsqip_logistic$wndinfd <- as.factor(nsqip_logistic$wndinfd)
nsqip_logistic$supinfec <- as.factor(nsqip_logistic$supinfec)
nsqip_logistic$orgspcssi <- as.factor(nsqip_logistic$orgspcssi)
nsqip_logistic$oupneumo <- as.factor(nsqip_logistic$oupneumo)
nsqip_logistic$failwean <- as.factor(nsqip_logistic$failwean)
nsqip_logistic$cnscva <- as.factor(nsqip_logistic$cnscva)
nsqip_logistic$cdarrest <- as.factor(nsqip_logistic$cdarrest)
nsqip_logistic$cdmi <- as.factor(nsqip_logistic$cdmi)
nsqip_logistic$pulembol <- as.factor(nsqip_logistic$pulembol)
nsqip_logistic$returnor <- as.factor(nsqip_logistic$returnor)
nsqip_logistic$othsysep <- as.factor(nsqip_logistic$othsysep)
nsqip_logistic$reintub <- as.factor(nsqip_logistic$reintub)
nsqip_logistic$othdvt <- as.factor(nsqip_logistic$othdvt)


# format narrow outcome variable -----------------------------------------------
nsqip_logistic <- nsqip_logistic %>%
  
  mutate(
    
    narrow = ifelse(
      cdmi == "Myocardial Infarction" | 
        cdarrest == "Cardiac Arrest Requiring CPR" |
        othdvt != "No Complication" | 
        cnscva == "Stroke/CVA" | 
        dialysis == "Yes" | 
        oupneumo == "Pneumonia" | 
        wndinfd != "No Complication" | 
        orgspcssi == "Organ/Space SSI"
      ,
      1, 
      0
    )
    
  ) 


# format broad outcome variable ------------------------------------------------
nsqip_logistic <- nsqip_logistic %>%
  
  mutate(
    
    broad = ifelse(
      dopertod == "Deceased" | 
        wndinfd != "No Complication" |
        supinfec == "Superficial Incisional SSI" |
        orgspcssi == "Organ/Space SSI" |
        oupneumo == "Pneumonia" | 
        failwean == "On Ventilator greater than 48 Hours" |
        cnscva == "Stroke/CVA" | 
        cdarrest == "Cardiac Arrest Requiring CPR" |
        cdmi == "Myocardial Infarction" |
        pulembol == "Pulmonary Embolism" |
        returnor == "Yes" |
        othsysep == "Sepsis" |
        reintub == "Unplanned Intubation" |
        othdvt != "No Complication"
      ,
      1, 
      0
    )
    
  )

