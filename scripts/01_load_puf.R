library(RSQLite)
library(tidyverse)




# connect db -------------------------------------------------------------------
conn <- dbConnect(
  
  RSQLite::SQLite(), 
  "nsqip.db"
  
  )




# query puf --------------------------------------------------------------------
puf <- dbGetQuery(
  
  conn, 
  "SELECT * 
  FROM puf"
  
  )





# select puf columns -----------------------------------------------------------
puf <- puf %>%
  
  select(
    
    caseid,
    operyr,
    podiag,
    podiag10,
    age,
    sex,
    weight,
    height,
    diabetes,
    hypermed,
    dialysis,
    prcreat,
    asaclas,
    steroid,
    inout,
    optime,
    tothlos,
    fnstatus1,
    fnstatus2,
    hxcopd,
    hxchf,
    hxmi,
    prvpci, prvpcs, hxangina,
    hypermed,
    impsens,
    hxtia, cvano,
    cva,
    hxpvd, restpain,
    
    # outcomes
    dopertod, # days to mortality
    supinfec, # superficial surgical site infection
    wndinfd, # deep wound infection
    dehis, # dehiscence
    pulembol, # pulmonary embolism
    renafail, # acute kidney injury
    dialysis, # dialysis
    urninfec, # urinary tract infection
    cnscva, # cerebrovascular accident
    cdarrest, # cardiac arrest
    cdmi, # myocardial infarction
    othbleed, # bleeding
    othdvt, # deep venous thrombosis
    othsysep, # sepsis
    othseshock, # septic shock
    
    # narrow composite outcome 
    # myocardial infarction
    # cardiac arrest
    # deep venous thrombosis (DVT)
    # cerebrovascular accident
    # acute kidney injury
    # dialysis
    # pneumonia
    # deep wound infection
    orgspcssi, # organ space infection
    
    # broad composite outcome
    # mortality 
    # deep wound infection
    # superficial wound infection or organ/space infection
    oupneumo, # pneumonia
    failwean, # prolonged ventilation
    # acute renal failure
    # cerebrovascular accident
    # *no longer accurate after 2010* coma
    # cardiac arrest
    # myocardial infarc tion
    # pulmonary embolism
    returnor, # reoperation
    transfus, # othbleed? transfus? bleeding requiring transfusion of >4 units
    # *no longer accurate after 2010* graft failure
    # sepsis
    reintub # reintubation
    # DVT
    # ? unplanned readmission
    
  )




# format operyr ----------------------------------------------------------------
puf$operyr <- substring(
  
  puf$operyr, 
  1, 
  4
  
  )




# write puf to .csv ------------------------------------------------------------
write.csv(
  
  puf, 
  "data/puf.csv", 
  row.names = FALSE
  
  )
