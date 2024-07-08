library(tidyverse)




# format kam --------------------------------------------------------------------
puf <- read.csv(
  
  "data/puf.csv", 
  header = TRUE
  
  )


thy <- read.csv(
  
  "data/thy.csv", 
  header = TRUE
  
  )


mg <- read.csv(
  
  "data/mg.csv", 
  header = TRUE
  
  )


thymoma <- read.csv(
  
  "data/thymoma.csv", 
  header = TRUE
  
  )


# join
kam <- left_join(
  
  puf, 
  thy
  
  )

kam <- left_join(
  
  kam, 
  mg
  
  )

kam <- left_join(
  
  kam, 
  thymoma
  
  )


# replace na
kam["thy"][is.na(kam["thy"])] <- 0 
kam["mg"][is.na(kam["mg"])] <- 0 
kam["thymoma"][is.na(kam["thymoma"])] <- 0 


# exclude thymoma
kam <- kam %>%
  
  filter(
    
    thymoma != 1
    
    )


# filter thy == 1
kam <- kam %>%
  
  filter(
    
    thy == 1
    
    )


# remove blank sex
kam <- kam %>% 
  
  filter(
    
    sex != ""
    
    )


# create bmi
kam <- kam %>%
  
  mutate(
    
    bmi = 703 * (weight / height^2)
    
    ) %>%
  
  select(
    
    -weight,
    -height
    
    )




# write to .csv ----------------------------------------------------------------
write.csv(
  
  kam, 
  "data/kam.csv", 
  row.names = FALSE
  
  )
