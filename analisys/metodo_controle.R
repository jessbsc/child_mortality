library(tibble)
library(EventDetectR)
library(magrittr)
library(dplyr)

micro_data_csv<- read.csv(file = './data/mortality_and_nasc_by_month_v3.csv')

micro_data_csv_clean = subset(micro_data_csv, select = c(LOCAL_CODE, LOCAL_NAME, grupo, year_month_date, all_death, FL_BIRTH) )
micro_data_csv_clean$year_month_date <- as.Date(micro_data_csv_clean$year_month_date)

micro_data_csv_clean <- filter(micro_data_csv_clean, LOCAL_NAME != "")

uf_list<- read.csv(file = './data/cidades_uf_population.csv')
uf_list$sub_code = substr(uf_list$cod_municipio,0,6)

# FALTA TRATAR ALGUMAS CIDADES 
micro_data_csv_clean <- merge(x=micro_data_csv_clean, y=uf_list, 
                              by.x=c("LOCAL_NAME", "LOCAL_CODE"), 
                              by.y=c("nome_municipio", "sub_code"))

micro_data_csv_clean$region <- ifelse(micro_data_csv_clean$uf %in%  list('RJ',"SP","MG","ES" ), "Sudeste", ifelse(micro_data_csv_clean$uf %in%  list('RS',"SC","PR"),"Sul",ifelse(micro_data_csv_clean$uf %in%  list('MS',"MT","GO", "DF"),"CentroOeste", ifelse(micro_data_csv_clean$uf %in%  list('AM',"AC","RR","RO","AP","TO", "PA" ),"Norte","Nordeste")))) 

#write.csv(micro_data_csv_clean, "mortality_and_nasc_by_month_v4.csv", row.names=TRUE)
#unique(micro_data_csv_clean$LOCAL_NAME)


ano_analise <- '2020'
ano_final_media <- '2019'

cidade_estudo_lista <- c('São Paulo', 'Capão do Cipó', 'Campo Mourão', 'Abadia de Goiás','Campos dos Goytacazes', 'Campinas','Rio de Janeiro', 'Afrânio', "Abelardo luz", 'Diamante do Sul' )

#cidade_estudo_lista <- c('Serranópolis', 'Montes Claros de Goiás', 'Trombas', 'Fagundes Varela', 'Diamante do Sul', 'Vila Boa')


#for (cidade_estudo in cidade_estudo_lista) {
#  diagrama_controle(ano_analise, ano_final_media, cidade_estudo, micro_data_csv_clean)
#}

#for (cidade_estudo in cidade_estudo_lista) {
#  historico_cidades(cidade_estudo ,1998, micro_data_csv_clean)
#}
### Para chamar só para um caso 

library(patchwork)
library(ggplot2)
library(gridExtra)

### plot grafico 1 
#install.packages("cowplot")
#library(cowplot)
p1 <- diagrama_controle(ano_analise, ano_final_media, 'Rio de Janeiro', micro_data_csv_clean, "A")
p2 <- diagrama_controle(ano_analise, ano_final_media, 'Campo Mourão', micro_data_csv_clean, "B")
p3 <- diagrama_controle(ano_analise, ano_final_media, 'Diamante do Sul', micro_data_csv_clean, "C")
p4 <- diagrama_controle(ano_analise, ano_final_media, 'Gonçalves Dias', micro_data_csv_clean, "D")

subplot <- p1 + p2 + p3 + p4 + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
print(subplot)

### plot grafico 2

p5 <- historico_cidades('Rio de Janeiro', 2009, 2020, micro_data_csv_clean, "A")
p6 <- historico_cidades('Campo Mourão', 2009, 2020, micro_data_csv_clean, "B")
p7 <- historico_cidades('Diamante do Sul', 2009, 2020, micro_data_csv_clean, "C")
p8 <- historico_cidades('Gonçalves Dias', 2009, 2020, micro_data_csv_clean, "D")

subplot2 <- p5 + p6 + p7 + p8 + plot_layout(guides = 'collect')
print(subplot2)

# created table 1 

cidade_estudo_lista <- c('Rio de Janeiro', 'Campo Mourão', 'Diamante do Sul', 'Gonçalves Dias')

micro_data_csv_clean$year <- format(as.Date(micro_data_csv_clean$year_month_date, format="%d/%m/%Y"),"%Y")
tabela_data <- filter(micro_data_csv_clean, year == '2020' )

tabela_data_agg <- tabela_data %>% group_by(LOCAL_NAME) %>% summarise(
  all_death = sum(all_death),
  FL_BIRTH = sum(FL_BIRTH),
  LOCAL_NAME = max(LOCAL_NAME) 
)

tabela_data_agg_filter <- filter(tabela_data_agg, LOCAL_NAME %in% cidade_estudo_lista)

tabela_data_agg_filter$death_per_1000<- tabela_data_agg_filter$all_death/tabela_data_agg_filter$FL_BIRTH*1000



# created table 2 
micro_data_csv_clean$year <- format(as.Date(micro_data_csv_clean$year_month_date, format="%d/%m/%Y"),"%Y")
tabela_data <- filter(micro_data_csv_clean, year == '2020' )
tabela_data <- 


# Definicao da funcao


diagrama_controle = function (ano_analise, ano_final_media, cidade_estudo,  data , letra) {
  
  aux = 0 
  ano_inicial_media <-  as.numeric(ano_final_media) - 10
  data$month <- format(as.Date(data$year_month_date, format="%d/%m/%Y"),"%m")
  data$year <- format(as.Date(data$year_month_date, format="%d/%m/%Y"),"%Y")
  data$death_per_1000<- data$all_death/data$FL_BIRTH*1000
  
  
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
  
  #mortality_grupo_year_merge$comparacao <-  mortality_grupo_year_merge$death_per_1000*1.1 > mortality_grupo_year_merge$lim_superior

  estado <- unique(mortality_grupo_year$uf)
  
  library(ggplot2)
  
  # Reordenar as categorias
  reordered_categories <- factor(mortality_grupo_year_merge$death_per_1000, levels = c("Observed", "Expected Average", "Upper Limit", "Lower Limit"))
  
  # Criar o gráfico
  p <- ggplot(mortality_grupo_year_merge, aes(x = month, group = 1)) + 
    geom_line(aes(y = death_per_1000, color = "Observed", linetype = "Observed")) +
    geom_line(aes(y = avg_month, color = "Expected Average", linetype = "Expected Average")) +
    geom_line(aes(y = lim_superior, color = "Upper Limit", linetype = "Upper Limit")) +
    geom_line(aes(y = lim_inferior, color = "Lower Limit", linetype = "Lower Limit")) +
    scale_color_manual(
      values = c("Observed" = "black", "Expected Average" = "darkred", "Upper Limit" = "darkred", "Lower Limit" = "darkred"),
      breaks = c("Observed", "Expected Average", "Upper Limit", "Lower Limit"),
      name = ""
    ) +
    scale_linetype_manual(
      values = c("Observed" = "solid", "Expected Average" = "solid", "Upper Limit" = "dashed", "Lower Limit" = "dashed"),
      breaks = c("Observed", "Expected Average", "Upper Limit", "Lower Limit"),
      name = ""
    ) +
    labs(x = "Month", y = "Infant Mortality Rate") +
    ggtitle(paste(letra, "- Locality: ", cidade_estudo, "-", estado)) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.justification = "center"
    )
  
  # Exibir o gráfico
  print(p)
  
  

}

historico_cidades = function (cidade_estudo, ano_inicio, ano_fim, data, letra ) {
  
  data$month <- format(as.Date(data$year_month_date, format="%d/%m/%Y"),"%m")
  data$year <- format(as.Date(data$year_month_date, format="%d/%m/%Y"),"%Y")
  
  mortality_grupo <- filter(data, year >= ano_inicio )
  mortality_grupo <- filter(mortality_grupo, year <= ano_fim )
  
  cidade_analise <- filter(mortality_grupo, LOCAL_NAME == cidade_estudo)

  cidade_analise_group <- cidade_analise %>% group_by(year) %>% summarise(
    year = max(year),
    soma_mortes = sum(all_death), 
    soma_nasc = sum(FL_BIRTH)
  )
  
  cidade_analise_group$death_per_1000 <- cidade_analise_group$soma_mortes / cidade_analise_group$soma_nasc *1000
  
  estado <- unique(cidade_analise$uf)
  
  library(ggplot2)
  
  years_to_show <- c('2010','2012','2014','2016', '2018','2020')
  
  p <- (ggplot(cidade_analise_group, aes(x=year, group = 1) ) + 
        geom_line(aes(y = death_per_1000), color = "darkred") +
        ggtitle(paste(letra, "- Locality: ", cidade_estudo, "-", estado)) +
        labs(x = "Year", y = "Infant Mortality Rate" ) + theme_bw() +
          scale_x_discrete(breaks = years_to_show)
        
  )
  print(p)
  
}

dev.off()
