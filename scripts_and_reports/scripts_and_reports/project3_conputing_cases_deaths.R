# Jyoti Dalal, Isotta Triulzi, Ananthu James et al., COVID-19 Mortality in Women and Men in 
# Sub-Saharan Africa: A Cross sectional study

# For more information: https://www.medrxiv.org/content/10.1101/2021.07.31.21261422v1

library(glue)
library(tidyverse)
library(here)
library(stringr)
library(lubridate)

countries_csv <- list.files("~/analysis_gender_covid_paper/data_cleanCSVs/")
countries_names <- str_trim(str_remove(countries_csv, "_clean.csv"), "both")

fetching_data = function(df, country)
{
  return(mutate(df, country = str_to_sentence(country), patinfo_ageonset_years = as.double(patinfo_ageonset_years), report_classif = toupper(as.character(report_classif)), patcourse_status = toupper(as.character(patcourse_status))) %>% 
           mutate(report_date = if_else(is.na(report_date), as.Date(Lab_resdate), as.Date(report_date) )) %>%
           mutate(report_date = if_else(is.na(report_date), as.Date(consultation_dateHF), as.Date(report_date) )) %>%
           mutate(report_date = if_else(is.na(report_date), as.Date(Lab_datetaken), as.Date(report_date) )) %>% 
           mutate(report_date = if_else(is.na(report_date), as.Date(patcourse_dateonset), as.Date(report_date) )) %>% 
           mutate(report_date = if_else(is.na(report_date), as.Date(patcourse_datedeath), as.Date(report_date) )) %>% 
           mutate(report_date = if_else(is.na(report_date), as.Date(patcourse_datedischarge), as.Date(report_date) ))%>%
           mutate(patcourse_status = if_else((patcourse_status=='ALIVE' | patcourse_status== 'RECOVERED') & !is.na(patcourse_datedeath), 'DEAD', patcourse_status)) %>%
           mutate(patcourse_status = if_else(is.na(patcourse_status) & !is.na(patcourse_datedeath), 'DEAD', patcourse_status)) %>%
           mutate(patcourse_status = if_else(is.na(patcourse_status) & !is.na(patcourse_datedischarge), 'RECOVERED', patcourse_status)) %>%
           mutate(report_classif = if_else(is.na(report_classif) & (sum(is.na(report_classif))==nrow(df)), 'CONFIRMED', report_classif)) %>%
           mutate(
             age_cat = cut(patinfo_ageonset_years, c(-1,39,59,120), 
                           labels=c("0-39","40-59","60+")),
             death = if_else(patcourse_status=='DEAD', 1, 0)
           ) %>%
           filter(report_date <=lubridate::ymd("2020-09-01")) %>%
           filter(report_classif=='CONFIRMED') %>% filter(!is.na(patcourse_status)) %>% 
           filter(!is.na(patinfo_ageonset_years)) %>% filter(!is.na(patinfo_sex))) %>%
    select(country, age_cat, patinfo_sex, death)
}


lines <- list()
data1 <- list()
data <- list()

com_data1 = NA

for (country in countries_names[!grepl('indep', countries_names)]){
  lines[country] <- readLines(paste("~/analysis_gender_covid_paper/data_cleanCSVs/",country, "_clean.csv", sep =''), n = 1)
  
  if(grepl(',', lines[country])){
    print(country)
    print("1")
    data1 <- as.data.frame(read.csv(paste("~/analysis_gender_covid_paper/data_cleanCSVs/", country, "_clean.csv", sep ='')),na.strings =
                             ".") # , separated
    print(country)}
  else {
    print(country)
    print("2")
    data1 <- as.data.frame(read.csv2(paste("~/analysis_gender_covid_paper/data_cleanCSVs/", country, "_clean.csv", sep ='')), na.strings =
                             ".") #} # , separated
    print(country)}
  
  data = fetching_data(data1, country) # getting the confirmed cases
  
  data_M1 = data %>% filter(data$age_cat=='0-39' & data$patinfo_sex=='M') %>% # 0-39 males
    summarize(cases = n(), deaths = sum(death, na.rm=T))
  data_F1 = data %>% filter(data$age_cat=='0-39' &  data$patinfo_sex=='F') %>% # 0-39 females
    summarize(cases = n(), deaths = sum(death, na.rm=T))
  
  data_M2 = data %>% filter(data$age_cat=='40-59' & data$patinfo_sex=='M') %>% # 40-59 males
    summarize(cases = n(), deaths = sum(death, na.rm=T))
  data_F2 = data %>% filter(data$age_cat=='40-59' & data$patinfo_sex=='F') %>% # 40-59 females
    summarize(cases = n(), deaths = sum(death, na.rm=T))
  
  data_M3 = data %>% filter(data$age_cat=='60+' & data$patinfo_sex=='M') %>% # 60+ males
    summarize(cases = n(), deaths = sum(death, na.rm=T))
  data_F3 = data %>% filter(data$age_cat=='60+' & data$patinfo_sex=='F') %>% # 60+ females
    summarize(cases = n(), deaths = sum(death, na.rm=T))
  
  com_data = as.data.frame(rbind(com_data1, c(toupper(country),
                                              data_M1$cases,
                                              data_M1$deaths,
                                              data_F1$cases,
                                              data_F1$deaths,
                                              data_M2$cases,
                                              data_M2$deaths,
                                              data_F2$cases,
                                              data_F2$deaths,
                                              data_M3$cases,
                                              data_M3$deaths,
                                              data_F3$cases,
                                              data_F3$deaths,
                                              sum(data_M1$cases + data_M2$cases + data_M3$cases), 
                                              sum(data_F1$cases + data_F2$cases + data_F3$cases), 
                                              sum(data_M1$deaths + data_M2$deaths + data_M3$deaths),
                                              sum(data_F1$deaths + data_F2$deaths + data_F3$deaths))))
  com_data1 = com_data
  
}

com_data = (com_data[-1,])
#com_data
names(com_data) = c('Country', 'cases_m1','deaths_m1','cases_f1','deaths_f1','cases_m2','deaths_m2',
                    'cases_f2','deaths_f2','cases_m3','deaths_m3','cases_f3','deaths_f3', 'cases_m_t', 
                    'cases_f_t', 'deaths_m_t','deaths_f_t')
write.table(as.data.frame(com_data), file = "~/analysis_gender_covid_paper/data_further_analysis.csv", quote=F, sep=",", row.names= T)
