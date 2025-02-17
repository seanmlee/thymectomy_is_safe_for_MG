

# libraries --------------------------------------------------------------------
library(tidyverse)


# load raw text files ----------------------------------------------------------
puf_2005 <- read.table("nsqip_raw/ACS_NSQIP_PUF_05_06_vr1.txt", sep = "\t", header = TRUE, quote = "") %>% filter(OperYR == "2005")
puf_2006 <- read.table("nsqip_raw/ACS_NSQIP_PUF_05_06_vr1.txt", sep = "\t", header = TRUE, quote = "") %>% filter(OperYR == "2006")
puf_2007 <- read.table("nsqip_raw/ACS_NSQIP_PUF07_TXT.txt", sep = "\t", header = TRUE, quote = "", fill = TRUE)
puf_2008 <- read.table("nsqip_raw/ACS_NSQIP_PUF08_TXT.txt", sep = "\t", header = TRUE, quote = "")
puf_2009 <- read.table("nsqip_raw/ACS_NSQIP_PUF09_TXT.txt", sep = "\t", header = TRUE, quote = "", fill = TRUE)
puf_2010 <- read.table("nsqip_raw/ACS_NSQIP_PUF10_TXT.txt", sep = "\t", header = TRUE, quote = "")
puf_2011 <- read.table("nsqip_raw/ACS_NSQIP_PUF11_TXT.txt", sep = "\t", header = TRUE, quote = "", fill = TRUE)
puf_2012 <- read.table("nsqip_raw/acs_nsqip_puf12.txt", sep = "\t", header = TRUE, quote = "")
puf_2013 <- read.table("nsqip_raw/acs_nsqip_puf13.txt", sep = "\t", header = TRUE, quote = "")
puf_2014 <- read.table("nsqip_raw/acs_nsqip_puf14.txt", sep = "\t", header = TRUE, quote = "")
puf_2015 <- read.table("nsqip_raw/acs_nsqip_puf15_v2.txt", sep = "\t", header = TRUE, quote = "")
puf_2016 <- read.table("nsqip_raw/acs_nsqip_puf16.txt", sep = "\t", header = TRUE, quote = "")
puf_2017 <- read.table("nsqip_raw/acs_nsqip_puf17.txt", sep = "\t", header = TRUE, quote = "")
puf_2018 <- read.table("nsqip_raw/acs_nsqip_puf18_v2.txt", sep = "\t", header = TRUE, quote = "")
puf_2019 <- read.table("nsqip_raw/acs_nsqip_puf19.txt", sep = "\t", header = TRUE, quote = "")
puf_2020 <- read.table("nsqip_raw/acs_nsqip_puf20.txt", sep = "\t", header = TRUE, quote = "")
puf_2021 <- read.table("nsqip_raw/acs_nsqip_puf21.txt", sep = "\t", header = TRUE, quote = "")
puf_2022 <- read.table("nsqip_raw/acs_nsqip_puf22.txt", sep = "\t", header = TRUE, quote = "")
puf_2023 <- read.table("nsqip_raw/acs_nsqip_puf23.txt", sep = "\t", header = TRUE, quote = "")


# make all column names lowercase ----------------------------------------------
names(puf_2005) <- tolower(names(puf_2005))
names(puf_2006) <- tolower(names(puf_2006))
names(puf_2007) <- tolower(names(puf_2007))
names(puf_2008) <- tolower(names(puf_2008))
names(puf_2009) <- tolower(names(puf_2009))
names(puf_2010) <- tolower(names(puf_2010))
names(puf_2011) <- tolower(names(puf_2011))
names(puf_2012) <- tolower(names(puf_2012))
names(puf_2013) <- tolower(names(puf_2013))
names(puf_2014) <- tolower(names(puf_2014))
names(puf_2015) <- tolower(names(puf_2015))
names(puf_2016) <- tolower(names(puf_2016))
names(puf_2017) <- tolower(names(puf_2017))
names(puf_2018) <- tolower(names(puf_2018))
names(puf_2019) <- tolower(names(puf_2019))
names(puf_2020) <- tolower(names(puf_2020))
names(puf_2021) <- tolower(names(puf_2021))
names(puf_2022) <- tolower(names(puf_2022))
names(puf_2023) <- tolower(names(puf_2023))


# filter relevant columns ------------------------------------------------------

# define function to handle column selection
select_columns <- function(df, year) {
  
  # columns to be excluded
  excluded_columns <- c()
  
  # define common columns to include for all years
  common_columns <- c(
    "caseid", 
    "operyr", 
    "cpt",
    "age", 
    "sex", 
    "weight", 
    "height", 
    "diabetes", 
    "hypermed", 
    "dialysis", 
    "prcreat", 
    "asaclas", 
    "steroid", 
    "inout", 
    "optime", 
    "tothlos", 
    "fnstatus2", 
    "hxcopd", 
    "hxchf", 
    "hypermed", 
    "dopertod",
    "supinfec",
    "wndinfd",
    "dehis",
    "pulembol",
    "dialysis",
    "urninfec",
    "cnscva",
    "cdarrest",
    "cdmi",
    "othbleed",
    "othdvt",
    "othsysep",
    "othseshock",
    "orgspcssi",
    "oupneumo",
    "failwean",
    "returnor",
    "reintub"
  )
  
  # for years 2015-2022, include 'podiag10' instead of 'podiag'
  if (year >= 2015) {
    common_columns <- c(common_columns, "podiag10")
  } else {
    common_columns <- c(common_columns, "podiag")
  }
  
  # remove  excluded columns from selection list
  selected_columns <- setdiff(common_columns, excluded_columns)
  
  # ensure that only columns that exist in the dataframe are selected
  selected_columns <- intersect(selected_columns, colnames(df))
  
  # select columns
  df <- df %>%
    dplyr::select(all_of(selected_columns))
  
  return(df)
}


# apply function to each years
puf_2005 <- select_columns(puf_2005, 2005)
puf_2006 <- select_columns(puf_2006, 2006)
puf_2007 <- select_columns(puf_2007, 2007)
puf_2008 <- select_columns(puf_2008, 2008)
puf_2009 <- select_columns(puf_2009, 2009)
puf_2010 <- select_columns(puf_2010, 2010)
puf_2011 <- select_columns(puf_2011, 2011)
puf_2012 <- select_columns(puf_2012, 2012)
puf_2013 <- select_columns(puf_2013, 2013)
puf_2014 <- select_columns(puf_2014, 2014)
puf_2015 <- select_columns(puf_2015, 2015)
puf_2016 <- select_columns(puf_2016, 2016)
puf_2017 <- select_columns(puf_2017, 2017)
puf_2018 <- select_columns(puf_2018, 2018)
puf_2019 <- select_columns(puf_2019, 2019)
puf_2020 <- select_columns(puf_2020, 2020)
puf_2021 <- select_columns(puf_2021, 2021)
puf_2022 <- select_columns(puf_2022, 2022)
puf_2023 <- select_columns(puf_2023, 2023)

rm(select_columns)


# change podiag/podiag10 to icd ------------------------------------------------

# loop through all objects in global environment
for (name in ls(envir = .GlobalEnv)) {
  
  # check if object is a data frame
  if (is.data.frame(get(name))) {
    df <- get(name)
    
    # identify columns named "podiag" or "podiag10" and rename to "icd"
    colnames(df) <- sapply(colnames(df), function(col) {
      if (col == "podiag" || col == "podiag10") {
        return("icd")
      } else {
        return(col)
      }
    })
    
    # assign the updated data frame back to the global environment
    assign(name, df, envir = .GlobalEnv)
  }
}

rm(df)
rm(name)


# rbind all --------------------------------------------------------------------

# get list of all objects in the global environment
objects_in_env <- ls()

# filter to keep only data frames
data_frames <- objects_in_env[sapply(objects_in_env, function(x) is.data.frame(get(x)))]

# combine data frames
nsqip <- do.call(rbind, lapply(data_frames, get))

# get list of all objects in global environment
objects_in_env <- ls()

# remove all objects except combined_df
objects_to_remove <- setdiff(objects_in_env, "nsqip")
rm(list = objects_to_remove)
rm(objects_to_remove)


# assign thy column ------------------------------------------------------------
nsqip <- nsqip %>%
  
  # assign thymectomy
  mutate(
    thy = case_when(
      cpt == 60521 | cpt == 60522 | cpt == 32673 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  
  # assign mg
  mutate(
    mg = ifelse(
      grepl("^(G70|358)", icd), 
      1, 
      0
      )
    ) %>%
  
  # assign thymoma
  mutate(
    thymoma = ifelse(
      grepl("^(C37|164.0|212.6)", icd),
      1, 
      0
    )
  ) %>%
  
  # remove thymoma
  filter(
    thymoma != 1
  ) %>%
  
  # only include patients who received thymectomy or were diagnosed with mg
  filter(
    thy == 1 | mg == 1
  ) %>% 
  
  # remove duplicate caseid
  distinct(
    caseid, 
    .keep_all = TRUE
  )


# write ------------------------------------------------------------------------
write.csv(nsqip, "out/nsqip.csv", row.names = FALSE)
