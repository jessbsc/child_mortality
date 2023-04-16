library(tibble)
library(EventDetectR)
library(magrittr)
library(dplyr)

micro_data_csv<- read.csv(file = './data/mortality_and_nasc_by_month_v3.csv')

micro_data_csv_clean = subset(micro_data_csv, select = c(LOCAL_CODE, LOCAL_NAME, grupo, year_month_date, all_death, FL_BIRTH) )
micro_data_csv_clean$year_month_date <- as.Date(micro_data_csv_clean$year_month_date)

micro_data_csv_clean <- filter(micro_data_csv_clean, LOCAL_NAME != "")

uf_list<- read.csv(file = './data/cidades_uf.csv')
uf_list$sub_code = substr(uf_list$code,0,6)

# FALTA TRATAR ALGUMAS CIDADES 
micro_data_csv_clean <- merge(x=micro_data_csv_clean, y=uf_list, 
                              by.x=c("LOCAL_NAME", "LOCAL_CODE"), 
                              by.y=c("name", "sub_code"))
#CITY_BY_MONTH <- filter(teste, LOCAL_NAME == "Água Branca")

micro_data_csv_clean$region <- ifelse(micro_data_csv_clean$uf %in%  list('RJ',"SP","MG","ES" ), "Sudeste", ifelse(micro_data_csv_clean$uf %in%  list('RS',"SC","PR"),"Sul",ifelse(micro_data_csv_clean$uf %in%  list('MS',"MT","GO"),"CentroOeste", ifelse(micro_data_csv_clean$uf %in%  list('AM',"AC","RR","RO","AP","TO" ),"Norte","Nordeste")))) 

mortality_grupo <- filter(micro_data_csv_clean, grupo == 4)
mortality_grupo <- filter(mortality_grupo, year_month_date >= '1998/01/01')

library(zoo)
##### taxa geral por grupo e por regiao
nasc_and_death_by_month_without_city <- mortality_grupo %>% group_by(year_month_date, region)

nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>% summarise(
  year_month_date = max(year_month_date),
  region = max(region),
  FL_BIRTH = sum(FL_BIRTH),
  all_death = sum(all_death))

nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>% group_by(region) %>% mutate(cum_death = cumsum(all_death))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>% group_by(region) %>% mutate(cum_nasc = cumsum(FL_BIRTH))

nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>% group_by(region) %>% mutate(sum_mov_death_12 = rollsum(all_death, k=12, fill=NA, align='right'))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>% group_by(region) %>% mutate(sum_mov_nasc_12 = rollsum(FL_BIRTH, k=12, fill=NA, align='right'))

nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>% group_by(region) %>% mutate(sum_mov_death_24 = rollsum(all_death, k=24, fill=NA, align='right'))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>% group_by(region) %>% mutate(sum_mov_nasc_24 = rollsum(FL_BIRTH, k=24, fill=NA, align='right'))

nasc_and_death_by_month_without_city$tax_mortality_grupo_without_trat<- nasc_and_death_by_month_without_city$all_death *1000 / nasc_and_death_by_month_without_city$FL_BIRTH 
nasc_and_death_by_month_without_city$tax_mortality_grupo_cum<- nasc_and_death_by_month_without_city$cum_death *1000  / nasc_and_death_by_month_without_city$cum_nasc
nasc_and_death_by_month_without_city$tax_mortality_grupo_12<- nasc_and_death_by_month_without_city$sum_mov_death_12 *1000  / nasc_and_death_by_month_without_city$sum_mov_nasc_12
nasc_and_death_by_month_without_city$tax_mortality_grupo_24<- nasc_and_death_by_month_without_city$sum_mov_death_24 *1000  / nasc_and_death_by_month_without_city$sum_mov_nasc_24

nasc_and_death_by_month_without_city = subset(nasc_and_death_by_month_without_city, select = c( year_month_date, region, tax_mortality_grupo_cum, tax_mortality_grupo_12, tax_mortality_grupo_24, tax_mortality_grupo_without_trat ) )

#grafico para ver que a linhas estao diferentes por regiao 
library(ggplot2)

print(ggplot(data = nasc_and_death_by_month_without_city, mapping=aes(group=region, y=tax_mortality_grupo_24, x= year_month_date))+ 
        geom_line(aes(color=region))+
        theme(legend.position="top") + theme_bw()
      +  ggtitle( paste("Taxa de mortalidade por região - Grupo 4")))
