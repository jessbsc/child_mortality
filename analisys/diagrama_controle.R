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
  
  mortality_grupo_year_merge$comparacao <-  mortality_grupo_year_merge$death_per_1000*1.3 > mortality_grupo_year_merge$lim_superior
  
  #if  (any(mortality_grupo_year_merge$comparacao)){
  
  library(ggplot2)
  
  p <- (ggplot(mortality_grupo_year_merge, aes(x=month, group = 1) ) + 
          geom_line(aes(y = avg_month), color = "darkred") 
        + geom_line(aes(y = lim_superior), color = "darkred" , linetype = "dashed") 
        + geom_line(aes(y = lim_inferior), color = "darkred", linetype = "dashed") 
        + geom_line(aes(y = death_per_1000), color = "black") 
        +  ggtitle( paste("City: ", cidade_estudo ))
        +    labs(x = "Month",
                  y = "Mortality Rate")+ theme_bw()
  )
  print(p)
  #}
}