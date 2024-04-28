
mortality_and_nasc_by_month <- read.csv("~/mortalidade/child_mortality/data/mortality_and_nasc_by_month_2024_v1.csv")

mortality_and_nasc_by_month_clean = subset(mortality_and_nasc_by_month, select = c(year_month_date, LOCAL_CODE, region, UF, NOME.DO.MUNICÍPIO, city_group, all_death, FL_BIRTH) )
mortality_and_nasc_by_month_clean$year_month_date <- as.Date(mortality_and_nasc_by_month_clean$year_month_date)

mortality_and_nasc_by_month_clean <- filter(mortality_and_nasc_by_month_clean, year_month_date >= '1998/01/01')

library(zoo)

#p1 <- control_chart(study_year, last_observation_year, 'Rio de Janeiro', micro_data_csv_clean, "A.2")
#p2 <- control_chart(study_year, last_observation_year, 'Campo Mourão', micro_data_csv_clean, "B.2")
#p3 <- control_chart(study_year, last_observation_year, 'Diamante do Sul', micro_data_csv_clean, "C.2")
#p4 <- control_chart(study_year, last_observation_year, 'Gonçalves Dias', micro_data_csv_clean, "D.2")

source("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/jupyter.R")

load_library("harbinger")
load_library("daltoolbox") 

#### city 1 
mortality_remd <- filter(mortality_and_nasc_by_month_clean, NOME.DO.MUNICÍPIO == 'Rio de Janeiro')
mortality_remd$tax_mortality<- mortality_remd$all_death *1000 / mortality_remd$FL_BIRTH
plot_ts(x = 1:length(mortality_remd$tax_mortality), y = mortality_remd$tax_mortality)
series <- mortality_remd$tax_mortality

model_remd <- hanr_remd()
model_remd <- fit(model_remd, series)
detection <- detect(model_remd, series)

grf <- har_plot(model_remd, series, detection)
plot(grf)

#### city 2

mortality_remd <- filter(mortality_and_nasc_by_month_clean, NOME.DO.MUNICÍPIO == 'Campo Mourão')
mortality_remd$tax_mortality<- mortality_remd$all_death *1000 / mortality_remd$FL_BIRTH
plot_ts(x = 1:length(mortality_remd$tax_mortality), y = mortality_remd$tax_mortality)
series <- mortality_remd$tax_mortality

model_remd <- hanr_remd()
model_remd <- fit(model_remd, series)
detection <- detect(model_remd, series)

grf <- har_plot(model_remd, series, detection)
plot(grf)

#### city 3

mortality_remd <- filter(mortality_and_nasc_by_month_clean, NOME.DO.MUNICÍPIO == 'Diamante do Sul')
mortality_remd$tax_mortality<- mortality_remd$all_death *1000 / mortality_remd$FL_BIRTH
plot_ts(x = 1:length(mortality_remd$tax_mortality), y = mortality_remd$tax_mortality)
series <- mortality_remd$tax_mortality

model_remd <- hanr_remd()
model_remd <- fit(model_remd, series)
detection <- detect(model_remd, series)

grf <- har_plot(model_remd, series, detection)
plot(grf)

#### city 4

mortality_remd <- filter(mortality_and_nasc_by_month_clean, NOME.DO.MUNICÍPIO == 'Gonçalves Dias')
mortality_remd$tax_mortality<- mortality_remd$all_death *1000 / mortality_remd$FL_BIRTH
plot_ts(x = 1:length(mortality_remd$tax_mortality), y = mortality_remd$tax_mortality)
series <- mortality_remd$tax_mortality

model_remd <- hanr_remd()
model_remd <- fit(model_remd, series)
detection <- detect(model_remd, series)

grf <- har_plot(model_remd, series, detection)
plot(grf)


