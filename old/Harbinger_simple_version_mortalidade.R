library(tibble)
library(EventDetectR)
library(magrittr)
micro_data_csv<- read.csv(file = 'mortality_and_nasc_by_month_v3.csv')

micro_data_csv_clean = subset(micro_data_csv, select = c(LOCAL_CODE, LOCAL_NAME, grupo, year_month_date, all_death) )
micro_data_csv_clean$year_month_date <- as.Date(micro_data_csv_clean$year_month_date)

library(dplyr)
library(zoo)
# filtra as cidades atraves dos grupos 
# Sendo Grupo 4 o das maiores cidade e Grupo 1 o das menores
mortality_grupo <- filter(micro_data_csv_clean, grupo == 4)
mortality_grupo <- mortality_grupo %>% group_by(LOCAL_CODE) %>% mutate(cum_death = cumsum(all_death))

mortality_grupo <- mortality_grupo %>% group_by(LOCAL_CODE) %>% mutate(avg_mov_death_12 = rollmean(all_death, k=12, fill=NA, align='right'))
mortality_grupo <- mortality_grupo %>% group_by(LOCAL_CODE) %>% mutate(sum_mov_death_12 = rollsum(all_death, k=12, fill=NA, align='right'))

mortality_grupo <- mortality_grupo %>% group_by(LOCAL_CODE) %>% mutate(avg_mov_death_24 = rollmean(all_death, k=24, fill=NA, align='right'))
mortality_grupo <- mortality_grupo %>% group_by(LOCAL_CODE) %>% mutate(sum_mov_death_24 = rollsum(all_death, k=24, fill=NA, align='right'))


library(ggplot2)
source( "https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/harbinger.R" )

########################## Ponto de atenção 


# OU rodar esse caso queira aplicar o FOR para rodas as cidades do grupo 
lista_cidades <- unique(mortality_grupo$LOCAL_NAME)

# OU rodar esse caso queira aplicar para alguma cidade em específico
lista_cidades <-c("Recife", "Manaus")

for (j in lista_cidades)

{
  CITY_BY_MONTH <- filter(mortality_grupo, LOCAL_NAME == j )
  test <- subset(CITY_BY_MONTH, select=c(year_month_date, all_death))
 
  test <- subset(CITY_BY_MONTH, select=c(year_month_date, all_death))
  events_scp <- evtdet.seminalChangePoint(test, w=24, na.action=na.omit)
  print(evtplot(test,events_scp, mark.cp=TRUE) + ggtitle( paste("all_death - without sum ","- ", j, "- Change Point")))
   
  test <- subset(CITY_BY_MONTH, select=c(year_month_date, cum_death))
  events_scp <- evtdet.seminalChangePoint(test, w=24, na.action=na.omit)
  print(evtplot(test,events_scp, mark.cp=TRUE) + ggtitle( paste("all_death - cum sum ","- ", j, "- Change Point")))
  
  test <- subset(CITY_BY_MONTH, select=c(year_month_date, sum_mov_death_12))
  events_scp <- evtdet.seminalChangePoint(test, w=24, na.action=na.omit)
  print(evtplot(test,events_scp, mark.cp=TRUE) + ggtitle( paste("sum_mov_death_12 ","- ", j, "- Change Point")))
  
  test <- subset(CITY_BY_MONTH, select=c(year_month_date, sum_mov_death_24))
  events_scp <- evtdet.seminalChangePoint(test, w=24, na.action=na.omit)
  print(evtplot(test,events_scp, mark.cp=TRUE) + ggtitle( paste("sum_mov_death_24 ","- ", j, "- Change Point")))
  
}
