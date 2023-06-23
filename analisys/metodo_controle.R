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

micro_data_csv_clean$region <- ifelse(micro_data_csv_clean$uf %in%  list('RJ',"SP","MG","ES" ), "Sudeste", ifelse(micro_data_csv_clean$uf %in%  list('RS',"SC","PR"),"Sul",ifelse(micro_data_csv_clean$uf %in%  list('MS',"MT","GO", "DF"),"CentroOeste", ifelse(micro_data_csv_clean$uf %in%  list('AM',"AC","RR","RO","AP","TO", "PA" ),"Norte","Nordeste")))) 

#write.csv(micro_data_csv_clean, "mortality_and_nasc_by_month_v4.csv", row.names=TRUE)
#unique(micro_data_csv_clean$LOCAL_NAME)


ano_analise <- '2019'
ano_final_media <- '2019'

cidade_estudo <- 'São Paulo'

diagrama_controle(ano_analise, ano_final_media, cidade_estudo, micro_data_csv_clean)




diagrama_controle = function (ano_analise, ano_final_media, cidade_estudo,  data ) {
  
  ano_inicial_media <-  as.numeric(ano_final_media) - 10
  data$month <- format(as.Date(data$year_month_date, format="%d/%m/%Y"),"%m")
  data$year <- format(as.Date(data$year_month_date, format="%d/%m/%Y"),"%Y")
  data$death_per_1000<- data$all_death/1000 
  
  mortality_grupo <- filter(data, year >= ano_inicial_media )
  mortality_grupo <- filter(mortality_grupo, year <= ano_final_media )
  
  mortality_grupo <- filter(mortality_grupo, LOCAL_NAME == cidade_estudo)
  
  nasc_and_death_by_month_without_city <- mortality_grupo %>% group_by(month) %>% summarise(
    month = max(month),
    avg_month = mean(death_per_1000),
    sd_month = sd(death_per_1000)
  )
  
  nasc_and_death_by_month_without_city$lim_superior <- nasc_and_death_by_month_without_city$avg_month + 1.96*nasc_and_death_by_month_without_city$sd_month
  nasc_and_death_by_month_without_city$lim_inferior <- nasc_and_death_by_month_without_city$avg_month - 1.96*nasc_and_death_by_month_without_city$sd_month
  
  mortality_grupo_year <- filter(data, year == ano_analise)
  mortality_grupo_year <- filter(mortality_grupo_year, LOCAL_NAME == cidade_estudo)
  
  mortality_grupo_year_merge <- merge(x=mortality_grupo_year, y=nasc_and_death_by_month_without_city, 
                                      by.x=c("month"), 
                                      by.y=c("month"))
  
  library(ggplot2)
  
  p <- (ggplot(mortality_grupo_year_merge, aes(x=month, group = 1) ) + 
          geom_line(aes(y = avg_month), color = "darkred") 
        + geom_line(aes(y = lim_superior), color = "darkred" , linetype = "dashed") 
        + geom_line(aes(y = lim_inferior), color = "darkred", linetype = "dashed") 
        + geom_line(aes(y = death_per_1000), color = "black") 
        +  ggtitle( paste("Diagrama de Controle - Ano de estudo:", ano_analise, " Range:", ano_final_media,"-", ano_inicial_media, " Cidade:", cidade_estudo ))
        +    labs(x = "Meses",
                  y = "Incidência de mortalidade por 1000 habitantes")+ theme_bw()
  )
  print(p)

}

