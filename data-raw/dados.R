## code to prepare `dados` dataset goes here



## Importação das bases do IPVIO 2021

ipvio_MA <- readxl::read_xlsx("./data-raw/IPVio_2021_MARIO_ANDREAZZA.xlsx")
ipvio_SA <- readxl::read_xlsx("./data-raw/IPVio_2021_SANTO_AMARO.xlsx")

## Limpeza de colunas iniciais 

ipvio_MA <- ipvio_MA[, -c(1:13)] # deletar colunas 1 até 13 
ipvio_SA <- ipvio_SA[, -c(1:13)] # deletar colunas 1 até 13 

## Limpeza de nomes das colunas

glimpse(ipvio_MA)

ipvio_MA <- janitor::clean_names(ipvio_MA)
ipvio_SA <- janitor::clean_names(ipvio_SA)

## Soma dos bancos da PB e do PE

ipvio_2021 <- rbind(ipvio_MA, ipvio_SA)

saveRDS(ipvio_2021, "./ipvio_2021.rds")


usethis::use_data(dados, overwrite = TRUE)
