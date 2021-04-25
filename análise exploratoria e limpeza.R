library(dplyr)
library(tidyverse)
library(janitor)
library(DataExplorer)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(RColorBrewer)
library(ggthemes)

## Importação das bases do IPVIO 2021

ipvio_MA <- readxl::read_xlsx("IPVio_2021_MARIO_ANDREAZZA.xlsx")
ipvio_SA <- readxl::read_xlsx("IPVio_2021_SANTO_AMARO.xlsx")

## Limpeza de colunas iniciais 

ipvio_MA <- ipvio_MA[, -c(1:13)] # deletar colunas 1 até 13 
ipvio_SA <- ipvio_SA[, -c(1:13)] # deletar colunas 1 até 13 

## Limpeza de nomes das colunas

glimpse(ipvio_MA)

ipvio_MA <- clean_names(ipvio_MA)
ipvio_SA <- clean_names(ipvio_SA)

## Soma dos bancos da PB e do PE

ipvio_2021 <- rbind(ipvio_MA, ipvio_SA)

## Exploração automatizada com o pacote "DataExplorer"

## create_report(ipvio_2021, config = configure_report(add_plot_prcomp = FALSE))  (Não rodar nesta pasta)

## Exploração manual

# Bairros

ipvio_2021 %>%
  count(qual_e_o_bairro)%>% 
  ggplot(aes(x=qual_e_o_bairro, y=n, fill=qual_e_o_bairro)) + 
  geom_bar(stat = "identity")+
  geom_label(aes(x = qual_e_o_bairro, y = n/2, label = n)) +
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Bairros dos entrevistados IPVIO 2021",
    subtitle = "Comercial norte é um bairro de Bayeux (PB), assim como Mário Andreazza"
  )

# Gênero

ipvio_2021 %>%
  count(qual_e_o_bairro, sexo_do_participante)%>% 
  ggplot(aes(fill=sexo_do_participante, y=n, x=qual_e_o_bairro)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Gênero dos entrevistados IPVIO 2021 por bairro",
    subtitle = "Comercial norte é um bairro de Bayeux (PB), assim como Mário Andreazza"
  )

ipvio_2021 %>%
  count(sexo_do_participante)%>% 
  ggplot(aes(fill=sexo_do_participante, y=n, x=sexo_do_participante)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_label(aes(x = sexo_do_participante, y = n/2, label = n)) +
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Gênero dos entrevistados IPVIO 2021",
    subtitle = "59% dos respondentes são do sexo feminino, 41% do sexo masculino"
  )


# Orientação sexual

ipvio_2021 %>%
  count(sexo_do_participante, qual_e_a_sua_orientacao_sexual)%>% 
  ggplot(aes(fill=qual_e_a_sua_orientacao_sexual, y=n, x=sexo_do_participante)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Orientação sexual dos entrevistados IPVIO 2021 por gênero"
  )+
  coord_flip()

# faixa etária dos entrevistados

idade <- ipvio_2021  %>%
  group_by(qual_e_o_bairro) %>% 
  count(qual_a_sua_idade)

ipvio_2021  %>%
  ggplot(aes(x=stat=count(qual_a_sua_idade), fill=qual_e_o_bairro)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', binwidth = 5) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

ipvio_2021  %>%
  count(qual_a_sua_idade,qual_e_o_bairro) %>% 
  ggplot(aes(x=qual_a_sua_idade,qual_e_o_bairro, group=qual_e_o_bairro, fill=qual_e_o_bairro)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()

