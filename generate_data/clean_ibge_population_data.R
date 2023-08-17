CD2022_Populacao_Coletada_Imputada_e_Total_Municipio_e_UF <- read_excel("data/CD2022_Populacao_Coletada_Imputada_e_Total_Municipio_e_UF.xlsx", col_types = c("text", "text", "text", "text", "skip", "skip", "text"), skip = 2)



CD2022_Populacao_Coletada_Imputada_e_Total_Municipio_e_UF$cod_municipio_dirty <- paste(CD2022_Populacao_Coletada_Imputada_e_Total_Municipio_e_UF$`COD. UF`, CD2022_Populacao_Coletada_Imputada_e_Total_Municipio_e_UF$`COD. MUNIC`)
CD2022_Populacao_Coletada_Imputada_e_Total_Municipio_e_UF$nome_municipio <- CD2022_Populacao_Coletada_Imputada_e_Total_Municipio_e_UF$`NOME DO MUNICÍPIO`
CD2022_Populacao_Coletada_Imputada_e_Total_Municipio_e_UF$population <- CD2022_Populacao_Coletada_Imputada_e_Total_Municipio_e_UF$`POP. TOTAL`
CD2022_Populacao_Coletada_Imputada_e_Total_Municipio_e_UF$population <- CD2022_Populacao_Coletada_Imputada_e_Total_Municipio_e_UF$`POP. TOTAL`
CD2022_Populacao_Coletada_Imputada_e_Total_Municipio_e_UF$cod_municipio <- gsub('\\s+', '', CD2022_Populacao_Coletada_Imputada_e_Total_Municipio_e_UF$cod_municipio_dirty)

list_uf_population = subset(CD2022_Populacao_Coletada_Imputada_e_Total_Municipio_e_UF, select = c(cod_municipio, UF, nome_municipio, population ) )

write.csv(list_uf_population, "cidades_uf_population.csv", row.names=TRUE)


