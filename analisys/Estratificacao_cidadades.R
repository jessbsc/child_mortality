populacao_censo_2022 <- read.csv("~/mortalidade/child_mortality/data/populacao_censo_2022.csv", sep=";", quote="", na.strings="", stringsAsFactors=TRUE) 

library(dplyr)
# Adicionar uma coluna com o logaritmo da população
populacao_censo_2022$POPULATION <- as.numeric(gsub("\\.", "", trimws(populacao_censo_2022$POPULAÇÃO)))

populacao_censo_2022$log_POPULATION <- log10(populacao_censo_2022$POPULATION)

populacao_censo_2022$region <- ifelse(populacao_censo_2022$UF %in%  list('RJ',"SP","MG","ES" ), "Sudeste", ifelse(populacao_censo_2022$UF %in%  list('RS',"SC","PR"),"Sul",ifelse(populacao_censo_2022$UF %in%  list('MS',"MT","GO", "DF"),"CentroOeste", ifelse(populacao_censo_2022$UF %in%  list('AM',"AC","RR","RO","AP","TO", "PA" ),"Norte","Nordeste")))) 

# Calcular estatísticas descritivas para a população e o logaritmo da população
describe_log <- summarise(populacao_censo_2022, 
                          count = n(),
                          mean = mean(POPULATION),
                          sd = sd(POPULATION),
                          min = min(POPULATION),
                          `1%` = quantile(POPULATION, 0.01),
                          `10%` = quantile(POPULATION, 0.1),
                          `20%` = quantile(POPULATION, 0.2),
                          `30%` = quantile(POPULATION, 0.3),
                          `40%` = quantile(POPULATION, 0.4),
                          `50%` = quantile(POPULATION, 0.5),
                          `60%` = quantile(POPULATION, 0.6),
                          `70%` = quantile(POPULATION, 0.7),
                          `80%` = quantile(POPULATION, 0.8),
                          `90%` = quantile(POPULATION, 0.9),
                          `99%` = quantile(POPULATION, 0.99),
                          log_mean = mean(log_POPULATION),
                          log_sd = sd(log_POPULATION),
                          log_min = min(log_POPULATION),
                          log_max = max(log_POPULATION))

# Visualizar as estatísticas descritivas
#print(describe_log)

populacao_censo_2022$city_group <- cut(populacao_censo_2022$log_POPULATION,
                                       breaks = c(-Inf, describe_log$log_min +1, describe_log$log_min + 2, describe_log$log_min +3, describe_log$log_min + 4, Inf),
                                       labels = c(1, 2, 3, 4, 5))

populacao_censo_2022$cod_geral_completo <- paste0(sprintf("%02d", populacao_censo_2022$COD..UF), sprintf("%05d", populacao_censo_2022$COD..MUNIC))
populacao_censo_2022$cod_geral <- substr(populacao_censo_2022$cod_geral_completo, 1, nchar(populacao_censo_2022$cod_geral_completo) - 1)

