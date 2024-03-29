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

uf_list_clean = subset(uf_list, select = c(nome_municipio, sub_code, UF, population) )


# FALTA TRATAR ALGUMAS CIDADES 
micro_data_csv_clean <- merge(x=micro_data_csv_clean, y=uf_list_clean, 
                              by.x=c("LOCAL_NAME", "LOCAL_CODE"), 
                              by.y=c("nome_municipio", "sub_code"))

micro_data_csv_clean$region <- ifelse(micro_data_csv_clean$UF %in%  list('RJ',"SP","MG","ES" ), "Sudeste", ifelse(micro_data_csv_clean$UF %in%  list('RS',"SC","PR"),"Sul",ifelse(micro_data_csv_clean$UF %in%  list('MS',"MT","GO", "DF"),"CentroOeste", ifelse(micro_data_csv_clean$UF %in%  list('AM',"AC","RR","RO","AP","TO", "PA" ),"Norte","Nordeste")))) 

write.csv(micro_data_csv_clean, "./data/mortality_and_nasc_by_month_v6.csv", row.names=TRUE)
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
#library(gridExtra)

### plot grafico 1 
#install.packages("cowplot")
#library(cowplot)
p1 <- diagrama_controle(ano_analise, ano_final_media, 'Rio de Janeiro', micro_data_csv_clean, "A.2")
p2 <- diagrama_controle(ano_analise, ano_final_media, 'Campo Mourão', micro_data_csv_clean, "B.2")
p3 <- diagrama_controle(ano_analise, ano_final_media, 'Diamante do Sul', micro_data_csv_clean, "C.2")
p4 <- diagrama_controle(ano_analise, ano_final_media, 'Gonçalves Dias', micro_data_csv_clean, "D.2")

subplot <- p1 + p2 + p3 + p4 + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
print(subplot)

### plot grafico 2

p5 <- historico_cidades('Rio de Janeiro', 2009, 2020, micro_data_csv_clean, "A.1")
p6 <- historico_cidades('Campo Mourão', 2009, 2020, micro_data_csv_clean, "B.1")
p7 <- historico_cidades('Diamante do Sul', 2009, 2020, micro_data_csv_clean, "C.1")
p8 <- historico_cidades('Gonçalves Dias', 2009, 2020, micro_data_csv_clean, "D.1")

subplot2 <-  p5 + p6 + p7 + p8 + plot_layout(guides = 'collect')
print(subplot2)


### plot grafico combine

subplot3 <- p5 + p1  + p6 + p2 + p7 + p3 + p8 + p4 + plot_layout(ncol = 2 , guides = 'collect') & theme(legend.position = 'bottom')
print(subplot3)

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
tabela_data$group <- ifelse(tabela_data$population>=500000, 1, ifelse(tabela_data$population>=100000,2,ifelse(tabela_data$population>=50000,3,ifelse(tabela_data$population>=20000,4,ifelse(tabela_data$population>=10000,5,ifelse(tabela_data$population>=5000,6,7))))))

tabela_data_agg <- tabela_data %>% group_by(LOCAL_NAME) %>% summarise(
  all_death_sum = sum(all_death),
  FL_BIRTH_sum = sum(FL_BIRTH),
  LOCAL_NAME = max(LOCAL_NAME),  
  group = max(group), 
  population = max(population), 
  imr = (sum(all_death)/sum(FL_BIRTH))*1000
)

tabela_data_total <- tabela_data_agg %>% summarise(
  all_death_total = sum(all_death_sum),
  FL_BIRTH_total = sum(FL_BIRTH_sum),
)


tabela_data_agg_group <- tabela_data_agg %>% group_by(group) %>% summarise(
  all_death_sum = sum(all_death_sum),
  FL_BIRTH_sum = sum(FL_BIRTH_sum),
  all_death_avg = mean(all_death_sum),
  FL_BIRTH_avg = mean(FL_BIRTH_sum),
  group = max(group),
  imr_avg = mean(imr), 
  imr_sd = sd(imr),

)

tabela_data_agg_group$imr_group<- tabela_data_agg_group$all_death_sum/tabela_data_agg_group$FL_BIRTH_sum*1000
tabela_data_agg_group$perc_death<- tabela_data_agg_group$all_death_sum/tabela_data_total$all_death_total*100
tabela_data_agg_group$perc_birth<- tabela_data_agg_group$FL_BIRTH_sum/tabela_data_total$FL_BIRTH_total*100


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

  estado <- unique(mortality_grupo_year$UF)
  
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
      values = c("Observed" = "black", "Expected Average" = "darkred", "Upper Limit" = "coral", "Lower Limit" = "darkorange4"),
      breaks = c("Observed", "Expected Average", "Upper Limit", "Lower Limit"),
      name = "Diagram Control"
    ) +
    scale_linetype_manual(
      values = c("Observed" = "solid", "Expected Average" = "solid", "Upper Limit" = "dashed", "Lower Limit" = "dashed"),
      breaks = c("Observed", "Expected Average", "Upper Limit", "Lower Limit"),
      name = "Diagram Control"
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

p8 <- historico_cidades('Rio de Janeiro', 2009, 2020, micro_data_csv_clean, "2.D")


historico_cidades = function (cidade_estudo, ano_inicio, ano_fim, data, letra ) {
  
  data$month <- format(as.Date(data$year_month_date, format="%d/%m/%Y"),"%m")
  data$year <- format(as.Date(data$year_month_date, format="%d/%m/%Y"),"%Y")
  
  mortality_grupo <- filter(data, year >= ano_inicio )
  mortality_grupo <- filter(mortality_grupo, year <= ano_fim )
  
  cidade_analise <- filter(mortality_grupo, LOCAL_NAME == cidade_estudo)
  
  cidade_analise$death_per_1000_month <- cidade_analise$all_death / cidade_analise$FL_BIRTH *1000

  cidade_analise_group <- cidade_analise %>% group_by(year) %>% summarise(
    year = max(year),
    soma_mortes = sum(all_death), 
    soma_nasc = sum(FL_BIRTH)
  )
  
  cidade_analise_group$death_per_1000_year <- cidade_analise_group$soma_mortes / cidade_analise_group$soma_nasc *1000
  
  estado <- unique(cidade_analise$UF)
  
  library(ggplot2)
  
  years_to_show <- c('2010','2012','2014','2016', '2018','2020')
  

  p <- ggplot() +
    # Linha para dados mensais
    geom_line(data = cidade_analise, aes(x = as.Date(paste(year, month, "01", sep = "-")), y = death_per_1000_month, color = "deepskyblue1" )) +
    # Linha para dados anuais
    geom_line(data = cidade_analise_group, aes(x = as.Date(paste(year, "01", "01", sep = "-")), y = death_per_1000_year, color = "blue")) +
    scale_x_date(date_breaks = "2 year", date_labels = "%Y") +  # Rótulos do eixo x como anos
    labs(x = "Month/Year", y = "Infant Mortality Rate") +
    ggtitle(paste(letra, "- Locality: ", cidade_estudo, "-", estado)) +
    scale_color_identity(guide = "legend", labels = c("Yearly", "Monthly")) +  # Mantém as cores nas legendas
    theme_bw() + labs(color = "IMR Historic")  # Remover o título da legenda

  
  print(p)
  
}

ano_inicio<- 2009
ano_fim <- 2020
cidade_estudo <- 'Rio de Janeiro'

micro_data_csv_clean$month <- format(as.Date(micro_data_csv_clean$year_month_date, format="%d/%m/%Y"),"%m")
micro_data_csv_clean$year <- format(as.Date(micro_data_csv_clean$year_month_date, format="%d/%m/%Y"),"%Y")

mortality_grupo <- filter(micro_data_csv_clean, year >= ano_inicio )
mortality_grupo <- filter(mortality_grupo, year <= ano_fim )

cidade_analise <- filter(mortality_grupo, LOCAL_NAME == cidade_estudo)

cidade_analise$death_per_1000_month <- cidade_analise$all_death / cidade_analise$FL_BIRTH *1000

cidade_analise_group <- cidade_analise %>% group_by(year) %>% summarise(
  year = max(year),
  soma_mortes = sum(all_death), 
  soma_nasc = sum(FL_BIRTH)
)

cidade_analise_group$death_per_1000_year <- cidade_analise_group$soma_mortes / cidade_analise_group$soma_nasc *1000


p <- ggplot() +
  # Linha para dados mensais
  geom_line(data = cidade_analise, aes(x = as.Date(paste(year, month, "01", sep = "-")), y = death_per_1000_month), color = "Month_Year") +
  # Linha para dados anuais
  geom_line(data = cidade_analise_group, aes(x = as.Date(paste(year, "01", "01", sep = "-")), y = death_per_1000_year), color = "Year" ) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +  # Rótulos do eixo x como anos
  #ggtitle(paste("A", "- Locality: ", cidade_estudo, "-", estado)) +
  labs(x = "Year", y = "Infant Mortality Rate" ) +
 scale_color_manual(values = c("Month_Year" = "deepskyblue1", "Year" = "blue")) + theme_bw()  # Define as cores das séries na legenda


print(p)


p <- ggplot() +
  # Linha para dados mensais
  geom_line(data = cidade_analise, aes(x = as.Date(paste(year, month, "01", sep = "-")), y = death_per_1000_month, color = "deepskyblue1" )) +
  # Linha para dados anuais
  geom_line(data = cidade_analise_group, aes(x = as.Date(paste(year, "01", "01", sep = "-")), y = death_per_1000_year, color = "blue")) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +  # Rótulos do eixo x como anos
  labs(x = "Year", y = "Infant Mortality Rate") +
  scale_color_identity(guide = "legend", labels = c("Monthly", "Yearly")) +  # Mantém as cores nas legendas
  theme_bw()

print(p)

dev.off()
