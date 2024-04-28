
###### estratificacao das cidades
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


#### Microdata
micro_data_csv <- read.csv("~/mortalidade/child_mortality/data/microdata_mortality.csv", row.names=1)

#micro_data_csv$DT_DEATH <- sprintf("%06d",micro_data_csv$LOCAL_CODE)
        
micro_data_csv$DT_DEATH = paste(micro_data_csv$YEAR_DEATH, micro_data_csv$MONTH_DEATH , micro_data_csv$DAY_DEATH, sep = "-")
micro_data_csv$DT_DEATH <- as.Date(micro_data_csv$DT_DEATH)
micro_data_csv$week_num = strftime(micro_data_csv$DT_DEATH, format = "%V")
micro_data_csv$year_week = paste(micro_data_csv$YEAR_DEATH, micro_data_csv$week_num, sep = "-")
micro_data_csv$year_month = paste(micro_data_csv$YEAR_DEATH, micro_data_csv$MONTH_DEATH, sep = "-")

micro_data_csv$year_month_date = paste(micro_data_csv$YEAR_DEATH, micro_data_csv$MONTH_DEATH, "01", sep = "-")
micro_data_csv$year_month_date <- as.Date(micro_data_csv$year_month_date)

mortality_by_month <- micro_data_csv %>% group_by(LOCAL_CODE, year_month_date)

mortality_by_month <- mortality_by_month %>% summarise(
  LOCAL_CODE = max(LOCAL_CODE),
  year_month_date = max(year_month_date),
  FL_DEATH_AVOIDABLE = sum(FL_DEATH_AVOIDABLE),
  FL_DEATH_ILLDEFINED = sum(FL_DEATH_ILLDEFINED),
  FL_DEATH_OTHERS = sum(FL_DEATH_OTHERS),
  FL_DEATH_IMMUNOPREV = sum(FL_DEATH_IMMUNOPREV),
  FL_DEATH_CAREPREV_ALL = sum(FL_DEATH_CAREPREV_ALL),
  FL_DEATH_CAREPREV_PREG = sum(FL_DEATH_CAREPREV_PREG),
  FL_DEATH_CAREPREV_DELIV = sum(FL_DEATH_CAREPREV_DELIV),
  FL_DEATH_CAREPREV_CHILD = sum(FL_DEATH_CAREPREV_CHILD),
  FL_DEATH_DTPREV = sum(FL_DEATH_DTPREV),
  FL_DEATH_PROMOPREV = sum(FL_DEATH_PROMOPREV),
  FL_DEATH_PNEUMONIA = sum(FL_DEATH_PNEUMONIA),
  FL_DEATH_IID = sum(FL_DEATH_IID),
  FL_DEATH_MALNUTRITION = sum(FL_DEATH_MALNUTRITION),
  FL_DEATH_COVID19 = sum(FL_DEATH_COVID19)
)

mortality_all_combination <- expand.grid(
  LOCAL_CODE = unique(mortality_by_month$LOCAL_CODE),
  year_month_date = unique(mortality_by_month$year_month_date)
)

mortality_expand <- left_join(mortality_all_combination, mortality_by_month, by = c("LOCAL_CODE", "year_month_date"))

mortality_expand[is.na(mortality_expand)] <- 0

#mortality_estratificado <- merge(x=mortality_expand, y=populacao_censo_2022, 
                                 #by.x=c("LOCAL_CODE"), 
                                 #by.y=c("cod_geral"),  how = "left")


#### validar as rows que nao deram join
#elementos_na_lista1 <- setdiff(unique(mortality_by_month$LOCAL_CODE), unique(populacao_censo_2022$cod_geral))
#elementos_na_lista1
# Info a nivel estado é o que nao da join 
## [1] 110000 120000 130000 140000 150000 160000 170000 210000 220000 230000 240000 250000 260000 270000
## [15] 280000 290000 310000 320000 330000 350000 410000 420000 430000 500000 510000 520000

mortality_estratificado$all_death <- mortality_estratificado$FL_DEATH_AVOIDABLE + mortality_estratificado$FL_DEATH_ILLDEFINED +  mortality_estratificado$FL_DEATH_OTHERS


###### Dados dos nascidos vivos

micro_data_nascimento_csv <- read.csv("~/mortalidade/child_mortality/data/nascimentos_microdata.csv", header=TRUE, row.names=1)
micro_data_nascimento_csv$year_month_date = paste(micro_data_nascimento_csv$YEAR_BIRTH, micro_data_nascimento_csv$MONTH_BIRTH, "01", sep = "-")
micro_data_nascimento_csv$year_month_date <- as.Date(micro_data_nascimento_csv$year_month_date)

nascimento_grupo_by_month <- micro_data_nascimento_csv %>% group_by(LOCAL_CODE, year_month_date)

nascimento_grupo_by_month <- nascimento_grupo_by_month %>% summarise(
  LOCAL_CODE = max(LOCAL_CODE),
  year_month_date = max(year_month_date),
  FL_BIRTH = sum(FL_BIRTH)
) 

nascimento_all_combination <- expand.grid(
  LOCAL_CODE = unique(nascimento_grupo_by_month$LOCAL_CODE),
  year_month_date = unique(nascimento_grupo_by_month$year_month_date)
)
nascimento_expand <- left_join(nascimento_all_combination, nascimento_grupo_by_month, by = c("LOCAL_CODE", "year_month_date"))
nascimento_expand[is.na(nascimento_expand)] <- 0

mortality_nasc_month <- left_join(x=mortality_estratificado, y=nascimento_expand, 
                                  by = c("LOCAL_CODE", "year_month_date"))

##### Fim do dataset base agrupado mensalmente e estratificado 

write.csv(mortality_nasc_month, "~/mortalidade/child_mortality/data/mortality_and_nasc_by_month_2024_v1.csv", row.names=TRUE)

