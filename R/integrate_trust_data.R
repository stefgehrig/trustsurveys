library(tidyverse)
library(foreign)
library(readstata13)
library(countrycode)
library(sjlabelled)

#################################
#### Arabbarometer waves 1-5 ####
#################################
df_arabbaro1 <- read_csv("data/arabbaro/ABI_English.csv")
df_arabbaro2 <- read_csv("data/arabbaro/ABII_English.csv")
df_arabbaro3 <- read_csv("data/arabbaro/ABIII_English.csv")
df_arabbaro4 <- read_csv("data/arabbaro/ABIV_English.csv")
df_arabbaro5 <- read_csv("data/arabbaro/ABV_Release_Data.csv")

surveyvar   <- c("Arabbarometer")
wavevars    <- 1:5
countryvars <- rep("country", 5)
trustvars   <- c("q204", "q103", "q103", "q103", "Q103")
weightvars  <- c("wt", "wt", "wt", "wt", "wt")

df_arabbaro1$wt <- 1 # no weights available
df_arabbaro5$country <- case_when( # survey-specific country codes
  df_arabbaro5$country == 1  ~ "Algeria",
  df_arabbaro5$country == 5  ~ "Egypt",
  df_arabbaro5$country == 7  ~ "Iraq",
  df_arabbaro5$country == 8  ~ "Jordan",
  df_arabbaro5$country == 9  ~ "Kuwait",
  df_arabbaro5$country == 10 ~ "Lebanon",
  df_arabbaro5$country == 11 ~ "Libya",
  df_arabbaro5$country == 13 ~ "Morocco",
  df_arabbaro5$country == 15 ~ "Palestine",
  df_arabbaro5$country == 19 ~ "Sudan",
  df_arabbaro5$country == 21 ~ "Tunisia",
  df_arabbaro5$country == 22 ~ "Yemen"
) 

# merge in long table
df_arabbaro <- map2_dfr(list(df_arabbaro1,
                             df_arabbaro2,
                             df_arabbaro3,
                             df_arabbaro4,
                             df_arabbaro5), 1:5, function(x,i){
  x %>% 
    mutate(country.name = as.character(!!as.symbol(countryvars[[i]])),
           trust = as.character(!!as.symbol(trustvars[[i]])),
           weight = !!as.symbol(weightvars[[i]])) %>% 
    select(country.name, trust, weight) %>% 
    mutate(wave = as.integer(wavevars[[i]]),
           survey = surveyvar)
}) 

# clean variables
df_arabbaro <- df_arabbaro %>% 
  mutate(country.name = countrycode(country.name, "country.name", "country.name"),
         trust = case_when(
           
           grepl("careful", trust) | grepl("are not trustworthy", trust) | trust == "2" ~ 2L,
           grepl("are trustworthy", trust) | grepl("can be trusted", trust) | trust == "1"  ~ 1L,
           TRUE ~ NA_integer_
           
         ))

# estimate means
df_arabbaro <- df_arabbaro %>% 
  group_by(country.name, wave) %>% 
  summarise(survey = first(survey),
            n = n(),
            p_response = sum(!is.na(trust)) / n,
            trust = weighted.mean(trust, w = weight, na.rm = TRUE),
            .groups = "drop")

#####################################
#### Afrobarometer waves 1, 3, 5 ####
#####################################
df_afrobaro1 <- as_tibble(read.spss("data/afrobaro/merged_r1_data.sav", to.data.frame = TRUE))
df_afrobaro3 <- as_tibble(read.spss("data/afrobaro/merged_r3_data.sav", to.data.frame = TRUE))
df_afrobaro5 <- as_tibble(read.spss("data/afrobaro/merged-round-5-data-34-countries-2011-2013-last-update-july-2015.sav", to.data.frame = TRUE))

surveyvar   <- c("Afrobarometer")
wavevars    <- c(1, 3, 5)
countryvars <- c("country", "country", "COUNTRY")
trustvars   <- c("sctrust", "q83", "Q87")
weightvars  <- c("withinwt","withinwt","withinwt")

# merge in long table
df_afrobaro <- map2_dfr(list(df_afrobaro1, df_afrobaro3, df_afrobaro5), 1:3, function(x,i){
  x %>% 
    mutate(country.name = as.character(!!as.symbol(countryvars[[i]])),
           trust = as.character(!!as.symbol(trustvars[[i]])),
           weight = !!as.symbol(weightvars[[i]])) %>% 
    select(country.name, trust, weight) %>% 
    mutate(wave = as.integer(wavevars[[i]]),
           survey = surveyvar)
}) 

# clean variables
df_afrobaro <- df_afrobaro %>% 
  mutate(country.name = countrycode(country.name, "country.name", "country.name"),
         trust = case_when(
           
           grepl("careful", trust) ~ 2L,
           grepl("trust", trust) ~ 1L,
           TRUE ~ NA_integer_
           
         ))

# estimate means
df_afrobaro <- df_afrobaro %>% 
  group_by(country.name, wave) %>% 
  summarise(survey = first(survey),
            n = n(),
            p_response = sum(!is.na(trust)) / n,
            trust = weighted.mean(trust, w = weight, na.rm = TRUE),
            .groups = "drop")

##################################
#### Asianbarometer waves 1-5 ####
##################################
df_asianbaro1      <- as_tibble(read.spss("data/asianbaro/wave1/Wave1_20170906.sav", to.data.frame = TRUE))
df_asianbaro2      <- as_tibble(read.spss("data/asianbaro/wave2/Wave2_20170724.sav", to.data.frame = TRUE))
df_asianbaro3      <- as_tibble(read.spss("data/asianbaro/wave3/ABS3 merge20210506.sav", to.data.frame = TRUE))
df_asianbaro4      <- as_tibble(read.spss("data/asianbaro/wave4/W4_v15_merged20181211_release.sav", to.data.frame = TRUE))
df_asianbaro5_phil <- as_tibble(read.spss("data/asianbaro/wave5/ABS Wave 5 Philippines_Core_merged_20201223_release.sav", to.data.frame = TRUE))
df_asianbaro5_mong <- as_tibble(read.spss("data/asianbaro/wave5/ABS_V_Mongolia_merged_core_20201217_release.sav", to.data.frame = TRUE))
df_asianbaro5_taiw <- as_tibble(read.spss("data/asianbaro/wave5/W5_Taiwan_coreQrelease_20190805.sav", to.data.frame = TRUE))
df_asianbaro5_viet <- as_tibble(read.spss("data/asianbaro/wave5/W5_Vietnam_merged_core_20201215_release.sav", to.data.frame = TRUE))

surveyvar   <- c("Asianbarometer")
wavevars    <- c(1, 2, 3, 4, 5, 5, 5, 5)
countryvars <- c("country","country","country","country","Country","Country","Country","Country")
trustvars   <- c("q024","q23","q23","q23", "Q22", "Q22", "Q22", "Q22")
weightvars  <- c("w_all","w_all","allweight", "w", "W","W","w_final","W")

# merge in long table
df_asianbaro <- map2_dfr(list(df_asianbaro1,     
                              df_asianbaro2,     
                              df_asianbaro3,     
                              df_asianbaro4,     
                              df_asianbaro5_phil,
                              df_asianbaro5_mong,
                              df_asianbaro5_taiw,
                              df_asianbaro5_viet), 1:8, function(x,i){
                                x %>% 
                                  mutate(country.name = as.character(!!as.symbol(countryvars[[i]])),
                                         trust = as.character(!!as.symbol(trustvars[[i]])),
                                         weight = !!as.symbol(weightvars[[i]])) %>% 
                                  select(country.name, trust, weight) %>% 
                                  mutate(wave = as.integer(wavevars[[i]]),
                                         survey = surveyvar)
                              }) 

# clean variables
df_asianbaro <- df_asianbaro %>% 
  mutate(country.name = countrycode(country.name, "country.name", "country.name"),
         trust = case_when(
           
           grepl("careful", trust) ~ 2L,
           grepl("trust", trust) ~ 1L,
           TRUE ~ NA_integer_
           
         ))

# estimate means
df_asianbaro <- df_asianbaro %>% 
  group_by(country.name, wave) %>% 
  summarise(survey = first(survey),
            n = n(),
            p_response = sum(!is.na(trust)) / n,
            trust = weighted.mean(trust, w = weight, na.rm = TRUE),
            .groups = "drop")

#######################################
#### Latinobarometer '96-'20 waves ####
#######################################
df_latinobaro1996 <- as_tibble(read.dta13("data/latinobaro/Latinobarometro_1996_datos_english_v2014_06_27.dta"))
df_latinobaro1997 <- as_tibble(read.dta13("data/latinobaro/Latinobarometro_1997_datos_english_v2014_06_27.dta"))
df_latinobaro1998 <- as_tibble(read.dta13("data/latinobaro/Latinobarometro_1998_datos_english_v2014_06_27.dta"))
df_latinobaro2000 <- as_tibble(read.dta13("data/latinobaro/Latinobarometro_2000_datos_eng_v2014_06_27.dta"))
df_latinobaro2001 <- as_tibble(read.dta13("data/latinobaro/Latinobarometro_2001_datos_english_v2014_06_27.dta"))
df_latinobaro2002 <- as_tibble(read.dta13("data/latinobaro/Latinobarometro_2002_datos_eng_v2014_06_27.dta"))
df_latinobaro2003 <- as_tibble(read.dta13("data/latinobaro/Latinobarometro_2003_datos_eng_v2014_06_27.dta"))
df_latinobaro2004 <- as_tibble(read.dta13("data/latinobaro/Latinobarometro_2004_datos_eng_v2014_06_27.dta"))
df_latinobaro2005 <- as_tibble(read.dta13("data/latinobaro/Latinobarometro_2005_datos_eng_v2014_06_27.dta"))
df_latinobaro2006 <- as_tibble(read.dta13("data/latinobaro/Latinobarometro_2006_datos_eng_v2014_06_27.dta"))
df_latinobaro2007 <- as_tibble(read.dta13("data/latinobaro/Latinobarometro_2007_datos_eng_v2014_06_27.dta"))
df_latinobaro2008 <- as_tibble(read.dta13("data/latinobaro/Latinobarometro_2008_datos_eng_v2014_06_27.dta"))
df_latinobaro2009 <- as_tibble(read.dta13("data/latinobaro/Latinobarometro_2009_datos_eng_v2014_06_27.dta"))
df_latinobaro2010 <- as_tibble(read.dta13("data/latinobaro/Latinobarometro_2010_datos_eng_v2014_06_27.dta"))
df_latinobaro2011 <- as_tibble(read.dta13("data/latinobaro/Latinobarometro_2011_eng.dta"))
df_latinobaro2013 <- as_tibble(read.dta13("data/latinobaro/Latinobarometro2013Eng.dta"))
df_latinobaro2015 <- as_tibble(read.dta13("data/latinobaro/Latinobarometro_2015_Eng.dta"))
df_latinobaro2016 <- as_tibble(read.dta13("data/latinobaro/Latinobarometro2016Eng_v20170205.dta"))
df_latinobaro2017 <- as_tibble(read.dta13("data/latinobaro/Latinobarometro2017Eng_v20180117.dta"))
df_latinobaro2018 <- as_tibble(read.dta13("data/latinobaro/Latinobarometro_2018_Esp_Stata_v20190303.dta"))
df_latinobaro2020 <- as_tibble(read.dta13("data/latinobaro/Latinobarometro_2020_Eng_Stata_v1_0.dta"))

surveyvar   <- c("Latinobarometro")
wavevars    <- c(1996:1998, 2000:2011, 2013, 2015:2018, 2020)
countryvars <- c("pais", "idenpa", "idenpa", "idenpa", "idenpa", "idenpa", "idenpa", "idenpa",
                 "idenpa", "idenpa", "idenpa", "idenpa", "idenpa", "idenpa", "idenpa", "idenpa",
                 "idenpa", "idenpa", "idenpa", "IDENPA", "idenpa")
trustvars   <- c("p12", "sp21", "sp20", "P17ST", "p42st", "p29st", "p20st", "p43st",	
                 "p14st", "p45st", "p23st", "p21wvsst", "p58st", "P55ST", "P25ST",
                 "P29STGBS", "P15STGBS", "P12STGBS", "P13STGBS", "P11STGBS", "p9stgbs")
weightvars  <- c("wt", "wt", "pondera", "wt", "wt", "wt", "wt", "wt",  
                 "wt", "wt", "wt", "wt", "wt", "wt", "wt",
                 "wt", "wt", "wt", "wt", "WT", "wt")

# merge in long table
df_latinobaro <- map2_dfr(list(df_latinobaro1996,
                               df_latinobaro1997,
                               df_latinobaro1998,
                               df_latinobaro2000,
                               df_latinobaro2001,
                               df_latinobaro2002,
                               df_latinobaro2003,
                               df_latinobaro2004,
                               df_latinobaro2005,
                               df_latinobaro2006,
                               df_latinobaro2007,
                               df_latinobaro2008,
                               df_latinobaro2009,
                               df_latinobaro2010,
                               df_latinobaro2011,
                               df_latinobaro2013,
                               df_latinobaro2015,
                               df_latinobaro2016,
                               df_latinobaro2017,
                               df_latinobaro2018,
                               df_latinobaro2020), 1:21, function(x,i){
                                 x %>% 
                                   mutate(country.name = as.character(!!as.symbol(countryvars[[i]])),
                                          trust = as.character(!!as.symbol(trustvars[[i]])),
                                          weight = !!as.symbol(weightvars[[i]])) %>% 
                                   select(country.name, trust, weight) %>% 
                                   mutate(wave = as.integer(wavevars[[i]]),
                                          survey = surveyvar)
                               }) 

# clean variables
df_latinobaro <- df_latinobaro %>% 
  mutate(country.name = case_when(
    
    grepl("Brasil", country.name) ~ "Brazil",
    grepl("xico", country.name) ~ "Mexico",
    grepl("Panam", country.name) ~ "Panama",
    grepl("Per", country.name) ~ "Peru",
    grepl("Domin", country.name) ~ "Dominican Republic",
    TRUE ~ country.name
  )) %>% 
  
  mutate(country.name = countrycode(country.name, "country.name", "country.name"),
         trust = case_when(
           
           grepl("careful", trust) | grepl("cuidadoso", trust) ~ 2L,
           grepl("trust", trust) | grepl("confiar", trust) ~ 1L,
           TRUE ~ NA_integer_
           
         ))

# estimate means
df_latinobaro <- df_latinobaro %>% 
  group_by(country.name, wave) %>% 
  summarise(survey = first(survey),
            n = n(),
            p_response = sum(!is.na(trust)) / n,
            trust = weighted.mean(trust, w = weight, na.rm = TRUE),
            .groups = "drop")

#######################
#### WVS all waves ####
#######################
df_wvs <- readRDS("data/wvs_evs/F00011411_WVS_Trend_1981_2020_R_rds_v2_0/WVS_Trend_v2_0.rds")

# clean variables
df_wvs <- df_wvs %>% 
  mutate(country.name = countrycode(S003, "wvs", "country.name"),
         country.name = case_when(
           S003 == 70  ~ countrycode("Bosnia and Herzegovina", "country.name", "country.name"),
           S003 == 499 ~ countrycode("Montenegro", "country.name", "country.name"),
           S003 == 688 ~ countrycode("Serbia", "country.name", "country.name"),
           S003 == 446 ~ countrycode("Macau SAR", "country.name", "country.name"),
           TRUE ~ country.name),
         survey = "WVS",
         trust = ifelse(A165 < 0, NA, A165)) %>% 
  select(country.name,
         wave = S002,
         survey,
         trust,
         weight = S017) %>% 
  remove_all_labels

# estimate means
df_wvs <- df_wvs %>% 
  group_by(country.name, wave) %>% 
  summarise(survey = first(survey),
            n = n(),
            p_response = sum(!is.na(trust)) / n,
            trust = weighted.mean(trust, w = weight, na.rm = TRUE),
            .groups = "drop")

#######################
#### EVS all waves ####
#######################
df_evs <- as_tibble(read.dta13("data/wvs_evs/ZA7503_v2-0-0.dta/ZA7503_v2-0-0.dta"))

# clean variables
df_evs <- df_evs %>% 
  mutate(country.name = countrycode(S003, "country.name", "country.name"),
         country.name = ifelse(S003 == " Northern Ireland", "Northern Ireland", country.name),
         survey = "EVS",
         trust = case_when(
           grepl("careful", A165) ~ 2L,
           grepl("trust", A165) ~ 1L,
           TRUE ~ NA_integer_
         ),
         wave = case_when(
           S002EVS == "1981-1984" ~ 1L,
           S002EVS == "1990-1993" ~ 2L,
           S002EVS == "1999-2001" ~ 3L,
           S002EVS == "2008-2010" ~ 4L,
           TRUE ~ 5L
         )) %>% 
  select(country.name,
         wave,
         survey,
         trust,
         weight = S017)

# estimate means
df_evs <- df_evs %>% 
  group_by(country.name, wave) %>% 
  summarise(survey = first(survey),
            n = n(),
            p_response = sum(!is.na(trust)) / n,
            trust = weighted.mean(trust, w = weight, na.rm = TRUE),
            .groups = "drop")

#############
#### GPS ####
#############
df_gps <- as_tibble(read.dta13("data/gps/country.dta"))

# clean variables and gather means
df_gps <- df_gps %>%
  mutate(country.name = countrycode(isocode, origin = "iso3c", destination = "country.name"),
         wave = 1,
         survey = "GPS",
         n = NA_real_,
         p_response = NA_real_) %>% 
  select(country.name, wave, survey, n, p_response, trust)

######################################
#### Merge and export all surveys ####
######################################
df_trust <- bind_rows(df_arabbaro,
                      df_afrobaro,
                      df_asianbaro,
                      df_latinobaro,
                      df_wvs,
                      df_evs) %>% 
  mutate(trust = 2-trust,
  ) %>%  # transform into proportion trusting
  arrange(survey, wave, country.name) %>% 
  filter(!is.na(trust)) %>% # some countries in some waves have no non-missing data (n = 3)
  bind_rows(df_gps) %>% 
  mutate(years = case_when(
    
    survey == "Arabbarometer" & wave == 1 ~ "2006-2009",
    survey == "Arabbarometer" & wave == 2 ~ "2010-2011",
    survey == "Arabbarometer" & wave == 3 ~ "2012-2014",
    survey == "Arabbarometer" & wave == 4 ~ "2016-2017",
    survey == "Arabbarometer" & wave == 5 ~ "2018-2019",
    survey == "Afrobarometer" & wave == 1 ~ "1999-2001",
    survey == "Afrobarometer" & wave == 3 ~ "2005",
    survey == "Afrobarometer" & wave == 5 ~ "2011-2013",
    survey == "Asianbarometer" & wave == 1 ~ "2001-2003",
    survey == "Asianbarometer" & wave == 2 ~ "2005-2008",
    survey == "Asianbarometer" & wave == 3 ~ "2010-2012",
    survey == "Asianbarometer" & wave == 4 ~ "2014-2016",
    survey == "Asianbarometer" & wave == 5 ~ "2018-2021",
    survey == "WVS" & wave == 1 ~ "1981-1984",
    survey == "WVS" & wave == 2 ~ "1990-1994",
    survey == "WVS" & wave == 3 ~ "1995-1998",
    survey == "WVS" & wave == 4 ~ "1999-2004",
    survey == "WVS" & wave == 5 ~ "2005-2009",
    survey == "WVS" & wave == 6 ~ "2010-2014",
    survey == "WVS" & wave == 7 ~ "2017-2020",
    survey == "EVS" & wave == 1 ~ "1981-1983",
    survey == "EVS" & wave == 2 ~ "1990-1993",
    survey == "EVS" & wave == 3 ~ "1999-2001",
    survey == "EVS" & wave == 4 ~ "2008-2010",
    survey == "EVS" & wave == 5 ~ "2017-2020",
    survey == "Latinobarometro" ~ as.character(wave),
    survey == "GPS" ~ "2012"
    ))

################
#### export ####
################
write_csv(df_trust, "output/df_trust.csv")
