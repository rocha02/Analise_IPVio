library(dplyr)
library(tidyverse)
library(janitor)
library(DataExplorer)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(RColorBrewer)
library(ggthemes)

ipvio_2021 <- readRDS("ipvio_2021.rds")

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
  theme(panel.background = element_blank())+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Bairros dos entrevistados IPVIO 2021",
    subtitle = "Comercial norte é um bairro de Bayeux (PB), assim como Mário Andreazza",
    fill = "Bairros"
  )

# Unificar os bairros comercial norte e Mário Andreazza

ipvio_2021 <- ipvio_2021 %>%
  mutate(bairro = case_when(
    qual_e_o_bairro %in% c ("Mário Andreazza", "Comercial Norte") ~ "Mário Andreazza/CN", 
    TRUE ~ as.character(qual_e_o_bairro)))


# Gênero

ipvio_2021 %>%
  count(bairro, sexo_do_participante)%>% 
  ggplot(aes(fill=sexo_do_participante, y=n, x=bairro)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Gênero dos entrevistados IPVIO 2021 por bairro"
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

# Criar agrupamentos de idade

ipvio_2021$qual_a_sua_idade <- as.numeric(ipvio_2021$qual_a_sua_idade)

ipvio_2021$grupo_idade <- cut(ipvio_2021$qual_a_sua_idade, 
                       breaks = c(0, 19, 30, 40, 50, 60, 70, 80, 90), 
                       labels = c("13 a 18 anos", "19 a 29 anos", "30 a 39 anos", "40 a 49 anos", 
                                  "50 a 59 anos","60 a 69 anos", "70 a 79 anos", "80 anos ou mais"), 
                       right = FALSE)

teste <- ipvio_2021 %>% 
  select(qual_a_sua_idade, grupo_idade)

ipvio_2021 %>%
  count(bairro, grupo_idade)%>% 
  ggplot(aes(fill=grupo_idade, y=n, x=bairro)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Faixa etária dos entrevistados IPVIO 2021 por bairro",
    subtitle = "Apesar de 14 anos ter sido definida como a idade mínima para responder, tivemos 2 respondentes de 13 anos, um em cada bairro"
  )

ipvio_2021 %>%
  count(sexo_do_participante, grupo_idade)%>% 
  ggplot(aes(fill=grupo_idade, y=n, x=sexo_do_participante)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Faixa etária dos entrevistados IPVIO 2021 por gênero",
    subtitle = "Apesar de 14 anos ter sido definida como a idade mínima para responder, tivemos 2 respondentes de 13 anos, um em cada bairro"
  )

# Cor dos entrevistados

ipvio_2021 %>%
  count(bairro, qual_e_a_sua_cor_da_pele)%>%
  mutate(qual_e_a_sua_cor_da_pele = forcats::fct_reorder(qual_e_a_sua_cor_da_pele, n)) %>%
  ggplot(aes(fill=qual_e_a_sua_cor_da_pele, y=n, x=bairro)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Cor autodeclarada dos entrevistados IPVIO 2021 por bairro"
  )

ipvio_2021 %>%
  count(sexo_do_participante, qual_e_a_sua_cor_da_pele)%>%
  mutate(qual_e_a_sua_cor_da_pele = forcats::fct_reorder(qual_e_a_sua_cor_da_pele, n)) %>%
  ggplot(aes(fill=qual_e_a_sua_cor_da_pele, y=n, x=sexo_do_participante)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Cor autodeclarada dos entrevistados IPVIO 2021 por gênero")+
  coord_flip()

# Estudo

ipvio_2021 %>%
  count(voce_estuda_atualmente, bairro)%>%
  ggplot(aes(fill=voce_estuda_atualmente, y=n, x=bairro)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Atividade escolar atual dos entrevistados IPVIO 2021 por bairro"
  )

ipvio_2021 %>%
  count(grupo_idade, voce_estuda_atualmente)%>%
  ggplot(aes(fill=voce_estuda_atualmente, y=n, x=grupo_idade)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Atividade escolar atual dos entrevistados IPVIO 2021 por faixa etária"
  )

ipvio_2021 %>%
  filter(qual_a_sua_idade < 59) %>% 
  count(grupo_idade, voce_estuda_atualmente, bairro)%>%
  ggplot(aes(fill=voce_estuda_atualmente, y=n, x=grupo_idade)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  facet_wrap(~bairro)+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Atividade escolar atual dos entrevistados IPVIO 2021 por faixa etária e bairro"
  )

ipvio_2021 %>%
  filter(voce_estuda_atualmente=="Sim") %>% 
  count(em_que_serie_ano_voce_esta_atualmente_ou_conseguiu_alcancar_ate_o_momento, bairro)%>%
  mutate(em_que_serie_ano_voce_esta_atualmente_ou_conseguiu_alcancar_ate_o_momento = forcats::fct_reorder
         (em_que_serie_ano_voce_esta_atualmente_ou_conseguiu_alcancar_ate_o_momento, n)) %>%
  ggplot(aes(fill=em_que_serie_ano_voce_esta_atualmente_ou_conseguiu_alcancar_ate_o_momento, y=n, x=bairro)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_ipsum()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Nível escolar atual dos entrevistados IPVIO 2021 por bairro",
    fill = "Escolaridade"
  
  )

# Trabalho remunerado

ipvio_2021 %>%
  count(atualmente_voce_esta_trabalhando_ou_tem_alguma_ocupacao_remunerada, bairro)%>%
  mutate(atualmente_voce_esta_trabalhando_ou_tem_alguma_ocupacao_remunerada = forcats::fct_reorder
         (atualmente_voce_esta_trabalhando_ou_tem_alguma_ocupacao_remunerada, n)) %>%
  ggplot(aes(fill=atualmente_voce_esta_trabalhando_ou_tem_alguma_ocupacao_remunerada, y=n, x=bairro)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_ipsum()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Atividade remunerada atual dos entrevistados IPVIO 2021 por bairro",
    fill = "Atividade"
  )

ipvio_2021 %>%
  count(atualmente_voce_esta_trabalhando_ou_tem_alguma_ocupacao_remunerada, sexo_do_participante)%>%
  mutate(atualmente_voce_esta_trabalhando_ou_tem_alguma_ocupacao_remunerada = forcats::fct_reorder
         (atualmente_voce_esta_trabalhando_ou_tem_alguma_ocupacao_remunerada, n)) %>%
  ggplot(aes(fill=atualmente_voce_esta_trabalhando_ou_tem_alguma_ocupacao_remunerada, y=n, x=sexo_do_participante)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_ipsum()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Atividade remunerada atual dos entrevistados IPVIO 2021 por gênero",
    fill = "Atividade"
  )

ipvio_2021 %>%
  count(atualmente_voce_esta_trabalhando_ou_tem_alguma_ocupacao_remunerada, grupo_idade)%>%
  mutate(atualmente_voce_esta_trabalhando_ou_tem_alguma_ocupacao_remunerada = forcats::fct_reorder
         (atualmente_voce_esta_trabalhando_ou_tem_alguma_ocupacao_remunerada, n)) %>%
  ggplot(aes(fill=atualmente_voce_esta_trabalhando_ou_tem_alguma_ocupacao_remunerada, y=n, x=grupo_idade)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Atividade remunerada atual dos entrevistados IPVIO 2021 por faixa etária",
    fill = "Atividade"
  )

# Efeitos da pandemia

ipvio_2021 %>%
  count(voce_ou_alguem_na_sua_casa_estava_matriculado_em_alguma_escola_ou_faculdade_no_ano_passado, bairro)%>%
  mutate(voce_ou_alguem_na_sua_casa_estava_matriculado_em_alguma_escola_ou_faculdade_no_ano_passado = forcats::fct_reorder
         (voce_ou_alguem_na_sua_casa_estava_matriculado_em_alguma_escola_ou_faculdade_no_ano_passado, n)) %>%
  ggplot(aes(fill=voce_ou_alguem_na_sua_casa_estava_matriculado_em_alguma_escola_ou_faculdade_no_ano_passado, y=n, x=bairro)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Membro da família matriculado em escola/faculdade no ano de 2020 por bairro",
    fill = "Possuí membro da família matriculado"
  )


ipvio_2021 %>%
  filter(voce_ou_alguem_na_sua_casa_estava_matriculado_em_alguma_escola_ou_faculdade_no_ano_passado == "Sim") %>% 
  count(voce_ou_alguem_da_sua_casa_participou_de_aulas_da_sua_escola_faculdade_por_outro_meio_nao_presenciais_em_2020, bairro)%>%
  mutate(voce_ou_alguem_da_sua_casa_participou_de_aulas_da_sua_escola_faculdade_por_outro_meio_nao_presenciais_em_2020 = 
           forcats::fct_reorder(voce_ou_alguem_da_sua_casa_participou_de_aulas_da_sua_escola_faculdade_por_outro_meio_nao_presenciais_em_2020, n)) %>%
  ggplot(aes(fill=voce_ou_alguem_da_sua_casa_participou_de_aulas_da_sua_escola_faculdade_por_outro_meio_nao_presenciais_em_2020, y=n, x=bairro)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Respondente ou alguém da família participou de aulas não-presenciais no ano de 2020 por bairro",
    subtitle = "Apenas dentre aqueles que possuem membro da família matriculado em 2020",
    fill = "Possuí membro da família matriculado"
    
  )

ipvio_2021 %>%
  filter(voce_ou_alguem_na_sua_casa_estava_matriculado_em_alguma_escola_ou_faculdade_no_ano_passado == "Sim") %>% 
  count(voce_ou_alguem_da_sua_casa_participou_de_aulas_da_sua_escola_faculdade_por_outro_meio_nao_presenciais_em_2020, grupo_idade)%>%
  mutate(voce_ou_alguem_da_sua_casa_participou_de_aulas_da_sua_escola_faculdade_por_outro_meio_nao_presenciais_em_2020 = 
           forcats::fct_reorder(voce_ou_alguem_da_sua_casa_participou_de_aulas_da_sua_escola_faculdade_por_outro_meio_nao_presenciais_em_2020, n)) %>%
  ggplot(aes(fill=voce_ou_alguem_da_sua_casa_participou_de_aulas_da_sua_escola_faculdade_por_outro_meio_nao_presenciais_em_2020, y=n, x=grupo_idade)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Respondente ou alguém da família participou de aulas não-presenciais no ano de 2020 por idade",
    subtitle = "Apenas dentre aqueles que possuem membro da família matriculado em 2020",
    fill = "Possuí membro da família matriculado"
    
  )

ipvio_2021 %>%
  filter(voce_ou_alguem_da_sua_casa_participou_de_aulas_da_sua_escola_faculdade_por_outro_meio_nao_presenciais_em_2020 == "Sim") %>% 
  count(voce_ou_alguem_de_sua_casa_teve_alguma_dificuldade_para_acompanhar_as_atividades_nao_presenciais_em_2020, bairro)%>%
  ggplot(aes(fill=voce_ou_alguem_de_sua_casa_teve_alguma_dificuldade_para_acompanhar_as_atividades_nao_presenciais_em_2020, y=n, x=bairro)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Respondente ou alguém da família teve dificuldades para acompanhar as aulas não-presenciais no ano de 2020 por bairro",
    subtitle = "Apenas dentre aqueles que possuem membro da família que participou de aulas não-presenciais em 2020",
    fill = "Enfrentou dificuldades"
    
  )

ipvio_2021 %>%
  filter(voce_ou_alguem_da_sua_casa_participou_de_aulas_da_sua_escola_faculdade_por_outro_meio_nao_presenciais_em_2020 == "Sim") %>% 
  count(voce_ou_alguem_de_sua_casa_teve_alguma_dificuldade_para_acompanhar_as_atividades_nao_presenciais_em_2020, grupo_idade)%>%
  ggplot(aes(fill=voce_ou_alguem_de_sua_casa_teve_alguma_dificuldade_para_acompanhar_as_atividades_nao_presenciais_em_2020, y=n, x=grupo_idade)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Respondente ou alguém da família teve dificuldades para acompanhar as aulas não-presenciais no ano de 2020 por idade",
    subtitle = "Apenas dentre aqueles que possuem membro da família que participou de aulas não-presenciais em 2020",
    fill = "Enfrentou dificuldades"
    
  )

ipvio_2021 %>%
  filter(voce_ou_alguem_na_sua_casa_estava_matriculado_em_alguma_escola_ou_faculdade_no_ano_passado == "Sim") %>% 
  count(como_voce_avalia_a_qualidade_do_ensino_no_ano_de_2020_apos_as_mudancas_devido_a_pandemia_de_covid_19, bairro)%>%
  ggplot(aes(fill=como_voce_avalia_a_qualidade_do_ensino_no_ano_de_2020_apos_as_mudancas_devido_a_pandemia_de_covid_19, y=n, x=bairro)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Avaliação da qualidade do ensino em 2020 após as mudanças pela pandemia por bairro",
    subtitle = "Apenas dentre aqueles que possuem membro da família matriculado em 2020",
    fill = "Avaliação"
    
  )

ipvio_2021 %>%
  filter(voce_ou_alguem_na_sua_casa_estava_matriculado_em_alguma_escola_ou_faculdade_no_ano_passado == "Sim") %>% 
  count(como_voce_ou_alguem_na_sua_casa_encerrou_o_ano_de_2020_em_relacao_a_motivacao_para_continuar_a_estudar, bairro)%>%
  ggplot(aes(fill=como_voce_ou_alguem_na_sua_casa_encerrou_o_ano_de_2020_em_relacao_a_motivacao_para_continuar_a_estudar, y=n, x=bairro)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Situação/motivação para dar continuidade aos estudos por bairro",
    subtitle = "Apenas dentre aqueles que possuem membro da família matriculado em 2020",
    fill = "Situação/motivação"
    
  )

ipvio_2021 %>%
  filter(voce_ou_alguem_na_sua_casa_estava_matriculado_em_alguma_escola_ou_faculdade_no_ano_passado == "Sim") %>% 
  count(como_voce_ou_alguem_na_sua_casa_encerrou_o_ano_de_2020_em_relacao_a_motivacao_para_continuar_a_estudar, grupo_idade)%>%
  ggplot(aes(fill=como_voce_ou_alguem_na_sua_casa_encerrou_o_ano_de_2020_em_relacao_a_motivacao_para_continuar_a_estudar, y=n, x=grupo_idade)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Situação/motivação para dar continuidade aos estudos por idade",
    subtitle = "Apenas dentre aqueles que possuem membro da família matriculado em 2020",
    fill = "Situação/motivação"
    
  )

ipvio_2021 %>%
  filter(!is.na(como_voce_avalia_seu_aprendizado_em_2020)) %>% 
  count(como_voce_avalia_seu_aprendizado_em_2020, grupo_idade)%>%
  mutate(como_voce_avalia_seu_aprendizado_em_2020 = 
           forcats::fct_reorder(como_voce_avalia_seu_aprendizado_em_2020, n)) %>%
  ggplot(aes(fill=como_voce_avalia_seu_aprendizado_em_2020, y=n, x=grupo_idade)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Situação/motivação para dar continuidade aos estudos por idade",
    subtitle = "Apenas dentre aqueles que possuem membro da família matriculado em 2020",
    fill = "Situação/motivação"
    
  )

ipvio_2021 %>%
  filter(!is.na(como_voce_avalia_seu_aprendizado_em_2020)) %>% 
  count(como_voce_avalia_seu_aprendizado_em_2020, bairro)%>%
  mutate(como_voce_avalia_seu_aprendizado_em_2020 = 
           forcats::fct_reorder(como_voce_avalia_seu_aprendizado_em_2020, n)) %>%
  ggplot(aes(fill=como_voce_avalia_seu_aprendizado_em_2020, y=n, x=bairro)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Situação/motivação para dar continuidade aos estudos por idade",
    subtitle = "Apenas dentre aqueles que possuem membro da família matriculado em 2020",
    fill = "Situação/motivação"
    
  )


## Avaliação demais instituições

# Cras

ipvio_2021 %>%
  filter(centro_de_referencia_da_assistencia_social_cras_62 == "Sim") %>% 
  count(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_63, bairro)%>%
  mutate(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_63 = 
           forcats::fct_reorder(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_63, n)) %>%
  ggplot(aes(fill=qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_63, y=n, x=bairro)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Avaliação do Centro de Referência de Assistência Social por bairro",
    subtitle = "Apenas dentre aqueles que acessaram o serviço em 2020",
    fill = "Avaliação"
    
  )

# ubs

ipvio_2021 %>%
  filter(unidade_basica_de_saude_ou_no_hospital_65 == "Sim") %>% 
  count(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_66, bairro)%>%
  mutate(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_66 = 
           forcats::fct_reorder(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_66, n)) %>%
  ggplot(aes(fill=qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_66, y=n, x=bairro)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Avaliação da Unidade Básica de Saúde ou Hospital por bairro",
    subtitle = "Apenas dentre aqueles que acessaram o serviço em 2020",
    fill = "Avaliação"
    
  )

# Escola ou faculdade

ipvio_2021 %>%
  filter(escola_creche_faculdade_ou_universidade_68 == "Sim") %>% 
  count(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_69, bairro)%>%
  mutate(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_69 = 
           forcats::fct_reorder(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_69, n)) %>%
  ggplot(aes(fill=qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_69, y=n, x=bairro)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Avaliação da escola, creche ou faculdade por bairro",
    subtitle = "Apenas dentre aqueles que acessaram o serviço em 2020",
    fill = "Avaliação"
    
  )

# Companhia de água e esgoto

ipvio_2021 %>%
  filter(companhia_de_agua_e_esgoto_71 == "Sim") %>% 
  count(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_72, bairro)%>%
  mutate(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_72 = 
           forcats::fct_reorder(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_72, n)) %>%
  ggplot(aes(fill=qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_72, y=n, x=bairro)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Avaliação da Companhia de Água e Esgoto por bairro",
    subtitle = "Apenas dentre aqueles que acessaram o serviço em 2020",
    fill = "Avaliação"
    
  )

# TRansporte público

ipvio_2021 %>%
  filter(transporte_publico_77 == "Sim") %>% 
  count(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_78, bairro)%>%
  mutate(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_78 = 
           forcats::fct_reorder(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_78, n)) %>%
  ggplot(aes(fill=qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_78, y=n, x=bairro)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Avaliação do transporte público por bairro",
    subtitle = "Apenas dentre aqueles que acessaram o serviço em 2020",
    fill = "Avaliação"
    
  )

# Prefeitura

ipvio_2021 %>%
  filter(prefeitura_83 == "Sim") %>% 
  count(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_84, bairro)%>%
  mutate(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_84 = 
           forcats::fct_reorder(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_84, n)) %>%
  ggplot(aes(fill=qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_84, y=n, x=bairro)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Avaliação dos serviços da prefeitura por bairro",
    subtitle = "Apenas dentre aqueles que acessaram o serviço em 2020",
    fill = "Avaliação"
    
  )


# Polícia Civil

ipvio_2021 %>%
  filter(policia_civil_delegacia_89 == "Sim") %>% 
  count(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_90, bairro)%>%
  mutate(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_90 = 
           forcats::fct_reorder(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_90, n)) %>%
  ggplot(aes(fill=qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_90, y=n, x=bairro)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Avaliação dos serviços da Polícia Civil por bairro",
    subtitle = "Apenas dentre aqueles que acessaram o serviço em 2020",
    fill = "Avaliação"
    
  )

# Polícia Militar

ipvio_2021 %>%
  filter(policia_militar_patrulha_92 == "Sim") %>% 
  count(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_93, bairro)%>%
  mutate(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_93 = 
           forcats::fct_reorder(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_93, n)) %>%
  ggplot(aes(fill=qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_93, y=n, x=bairro)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Avaliação dos serviços da Polícia Militar por bairro",
    subtitle = "Apenas dentre aqueles que acessaram o serviço em 2020",
    fill = "Avaliação"
    
  )

ipvio_2021 %>%
  filter(policia_militar_patrulha_92 == "Sim") %>% 
  count(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_93, grupo_idade, bairro)%>%
  mutate(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_93 = 
           forcats::fct_reorder(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_93, n)) %>%
  ggplot(aes(fill=qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_93, y=n, x=grupo_idade)) + 
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~bairro)+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Avaliação dos serviços da Polícia Militar por faixa etária",
    subtitle = "Apenas dentre aqueles que acessaram o serviço em 2020",
    fill = "Avaliação"
    
  )

ipvio_2021 %>%
  filter(policia_militar_patrulha_92 == "Sim") %>% 
  count(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_93, qual_e_a_sua_cor_da_pele)%>%
  mutate(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_93 = 
           forcats::fct_reorder(qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_93, n)) %>%
  ggplot(aes(fill=qual_e_a_sua_avaliacao_do_servico_prestado_deste_orgao_93, y=n, x=qual_e_a_sua_cor_da_pele)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Avaliação dos serviços da Polícia Militar por cor",
    subtitle = "Apenas dentre aqueles que acessaram o serviço em 2020",
    fill = "Avaliação"
    
  )

## Violência

#Agressão


ipvio_2021 %>%
  count(agressao_fisica_120, bairro)%>%
  mutate(agressao_fisica_120 =  forcats::fct_reorder(agressao_fisica_120, n)) %>%
  ggplot(aes(fill=agressao_fisica_120, y=n, x=bairro)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Presenciou ou sofreu situação de agressão física",
    fill = "Presenciou/sofreu"
    
  )

ipvio_2021 %>%
  count(agressao_fisica_120, sexo_do_participante)%>%
  ggplot(aes(fill=agressao_fisica_120, y=n, x=sexo_do_participante)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Presenciou ou sofreu situação de agressão física por gênero",
    fill = "Presenciou/sofreu"
    
  )


ipvio_2021 %>%
  count(agressao_fisica_120, grupo_idade)%>%
  mutate(agressao_fisica_120 =  forcats::fct_reorder(agressao_fisica_120, n)) %>%
  ggplot(aes(fill=agressao_fisica_120, y=n, x=grupo_idade)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Presenciou ou sofreu situação de agressão física por faixa etária",
    fill = "Presenciou/sofreu"
    
  )

# Ameaça com arma de fogo

ipvio_2021 %>%
  count(ameaca_com_arma_de_fogo_121, bairro)%>%
  mutate(ameaca_com_arma_de_fogo_121 =  forcats::fct_reorder(ameaca_com_arma_de_fogo_121, n)) %>%
  ggplot(aes(fill=ameaca_com_arma_de_fogo_121, y=n, x=bairro)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Presenciou ou sofreu situação de ameaça com arma de fogo por bairro",
    fill = "Presenciou/sofreu"
    
  )

ipvio_2021 %>%
  count(ameaca_com_arma_de_fogo_121, sexo_do_participante)%>%
  mutate(ameaca_com_arma_de_fogo_121 =  forcats::fct_reorder(ameaca_com_arma_de_fogo_121, n)) %>%
  ggplot(aes(fill=ameaca_com_arma_de_fogo_121, y=n, x=sexo_do_participante)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Presenciou ou sofreu situação de ameaça com arma de fogo por gênero",
    fill = "Presenciou/sofreu"
    
  )


ipvio_2021 %>%
  count(ameaca_com_arma_de_fogo_121, grupo_idade)%>%
  mutate(ameaca_com_arma_de_fogo_121 =  forcats::fct_reorder(ameaca_com_arma_de_fogo_121, n)) %>%
  ggplot(aes(fill=ameaca_com_arma_de_fogo_121, y=n, x=grupo_idade)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Presenciou ou sofreu situação de ameaça com arma de fogo por faixa etária",
    fill = "Presenciou/sofreu"
    
  )

# Ameaças verbais ou xingamentos

ipvio_2021 %>%
  count(ameacas_verbais_123, bairro)%>%
  mutate(ameacas_verbais_123 =  forcats::fct_reorder(ameacas_verbais_123, n)) %>%
  ggplot(aes(fill=ameacas_verbais_123, y=n, x=bairro)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Presenciou ou sofreu situação de ameaça verbal ou xingamento por bairro",
    fill = "Presenciou/sofreu"
    
  )

ipvio_2021 %>%
  count(ameacas_verbais_123, sexo_do_participante)%>%
  mutate(ameacas_verbais_123 =  forcats::fct_reorder(ameacas_verbais_123, n)) %>%
  ggplot(aes(fill=ameacas_verbais_123, y=n, x=sexo_do_participante)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Presenciou ou sofreu situação de ameaça verbal ou xingamento por gênero",
    fill = "Presenciou/sofreu"
    
  )


ipvio_2021 %>%
  count(ameacas_verbais_123, grupo_idade)%>%
  mutate(ameacas_verbais_123 =  forcats::fct_reorder(ameacas_verbais_123, n)) %>%
  ggplot(aes(fill=ameacas_verbais_123, y=n, x=grupo_idade)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Presenciou ou sofreu situação de ameaça verbal ou xingamentopor faixa etária",
    fill = "Presenciou/sofreu"
    
  )

# Discriminação racial, de orientação e de genero

ipvio_2021 %>%
  count(discriminacao_racial_ou_cor_de_pele_125, bairro)%>%
  mutate(discriminacao_racial_ou_cor_de_pele_125 =  
           forcats::fct_reorder(discriminacao_racial_ou_cor_de_pele_125, n)) %>%
  ggplot(aes(fill=discriminacao_racial_ou_cor_de_pele_125, y=n, x=bairro)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Presenciou ou sofreu situação de discriminação racial por bairro",
    fill = "Presenciou/sofreu"
    
  )

ipvio_2021 %>%
  count(discriminacao_racial_ou_cor_de_pele_125, qual_e_a_sua_cor_da_pele)%>%
  mutate(discriminacao_racial_ou_cor_de_pele_125 =  
           forcats::fct_reorder(discriminacao_racial_ou_cor_de_pele_125, n)) %>%
  ggplot(aes(fill=discriminacao_racial_ou_cor_de_pele_125, y=n, x=qual_e_a_sua_cor_da_pele)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Presenciou ou sofreu situação de discriminação racial por cor",
    fill = "Presenciou/sofreu"
    
  )


ipvio_2021 %>%
  count(discriminacao_racial_ou_cor_de_pele_125, grupo_idade)%>%
  mutate(discriminacao_racial_ou_cor_de_pele_125 =  
           forcats::fct_reorder(discriminacao_racial_ou_cor_de_pele_125, n)) %>%
  ggplot(aes(fill=discriminacao_racial_ou_cor_de_pele_125, y=n, x=grupo_idade)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Presenciou ou sofreu situação de discriminação racial por idade",
    fill = "Presenciou/sofreu"
    
  )

ipvio_2021 %>%
  count(discriminacao_pela_orientacao_sexual_ou_genero_126, bairro)%>%
  mutate(discriminacao_pela_orientacao_sexual_ou_genero_126 =  
           forcats::fct_reorder(discriminacao_pela_orientacao_sexual_ou_genero_126, n)) %>%
  ggplot(aes(fill=discriminacao_pela_orientacao_sexual_ou_genero_126, y=n, x=bairro)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Presenciou ou sofreu situação de discriminação por orientação sexual e de gênero por bairro",
    fill = "Presenciou/sofreu"
    
  )

ipvio_2021 %>%
  count(discriminacao_pela_orientacao_sexual_ou_genero_126, sexo_do_participante)%>%
  mutate(discriminacao_pela_orientacao_sexual_ou_genero_126 =  
           forcats::fct_reorder(discriminacao_pela_orientacao_sexual_ou_genero_126, n)) %>%
  ggplot(aes(fill=discriminacao_pela_orientacao_sexual_ou_genero_126, y=n, x=sexo_do_participante)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Presenciou ou sofreu situação de discriminação por orientação sexual e de gênero por sexo do respondente",
    fill = "Presenciou/sofreu"
    
  )

ipvio_2021 %>%
  count(discriminacao_pela_orientacao_sexual_ou_genero_126, qual_e_a_sua_orientacao_sexual)%>%
  mutate(discriminacao_pela_orientacao_sexual_ou_genero_126 =  
           forcats::fct_reorder(discriminacao_pela_orientacao_sexual_ou_genero_126, n)) %>%
  ggplot(aes(fill=discriminacao_pela_orientacao_sexual_ou_genero_126, y=n, x=qual_e_a_sua_orientacao_sexual)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Presenciou ou sofreu situação de discriminação por orientação sexual e de gênero por orientação",
    fill = "Presenciou/sofreu"
    
  )


# Abordagem policial


ipvio_2021 %>%
  count(voce_tem_medo_da_abordagem_policial, bairro)%>%
  mutate(voce_tem_medo_da_abordagem_policial =  
           forcats::fct_reorder(voce_tem_medo_da_abordagem_policial, n)) %>%
  ggplot(aes(fill=voce_tem_medo_da_abordagem_policial, y=n, x=bairro)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Medo da abordagem policial por bairro",
    fill = "sentimento"
    
  )

ipvio_2021 %>%
  count(voce_tem_medo_da_abordagem_policial, grupo_idade)%>%
  mutate(voce_tem_medo_da_abordagem_policial =  
           forcats::fct_reorder(voce_tem_medo_da_abordagem_policial, n)) %>%
  ggplot(aes(fill=voce_tem_medo_da_abordagem_policial, y=n, x=grupo_idade)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Medo da abordagem policial por faixa etária",
    fill = "sentimento"
    
  )

ipvio_2021 %>%
  count(voce_tem_medo_da_abordagem_policial, qual_e_a_sua_cor_da_pele)%>%
  mutate(voce_tem_medo_da_abordagem_policial =  
           forcats::fct_reorder(voce_tem_medo_da_abordagem_policial, n)) %>%
  ggplot(aes(fill=voce_tem_medo_da_abordagem_policial, y=n, x=qual_e_a_sua_cor_da_pele)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Medo da abordagem policial por cor",
    fill = "sentimento"
    
  )

ipvio_2021 %>%
  count(voce_tem_medo_da_abordagem_policial, voce_foi_abordado_por_um_policial_no_ultimo_ano)%>%
  mutate(voce_tem_medo_da_abordagem_policial =  
           forcats::fct_reorder(voce_tem_medo_da_abordagem_policial, n)) %>%
  ggplot(aes(fill=voce_tem_medo_da_abordagem_policial, y=n, x=voce_foi_abordado_por_um_policial_no_ultimo_ano)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Medo da abordagem policial dentre os respondentes que foram ou não abordados pela policia em 2020",
    fill = "sentimento"
    
  )

ipvio_2021 %>%
  count(grupo_idade,voce_foi_abordado_por_um_policial_no_ultimo_ano)%>%
  mutate(voce_foi_abordado_por_um_policial_no_ultimo_ano =  
           forcats::fct_reorder(voce_foi_abordado_por_um_policial_no_ultimo_ano, n)) %>%
  ggplot(aes(fill=voce_foi_abordado_por_um_policial_no_ultimo_ano, y=n, x=grupo_idade)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Respondentes que foram ou não abordados pela policia em 2020 por faixa etária",
    fill = "Abordados pela polícia"
    
  )

ipvio_2021 %>%
  count(sexo_do_participante,voce_foi_abordado_por_um_policial_no_ultimo_ano)%>%
  mutate(voce_foi_abordado_por_um_policial_no_ultimo_ano =  
           forcats::fct_reorder(voce_foi_abordado_por_um_policial_no_ultimo_ano, n)) %>%
  ggplot(aes(fill=voce_foi_abordado_por_um_policial_no_ultimo_ano, y=n, x=sexo_do_participante)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Respondentes que foram ou não abordados pela policia em 2020 por gênero",
    fill = "Abordados pela polícia"
    
  )

ipvio_2021 %>%
  count(qual_e_a_sua_cor_da_pele,voce_foi_abordado_por_um_policial_no_ultimo_ano)%>%
  mutate(voce_foi_abordado_por_um_policial_no_ultimo_ano =  
           forcats::fct_reorder(voce_foi_abordado_por_um_policial_no_ultimo_ano, n)) %>%
  ggplot(aes(fill=voce_foi_abordado_por_um_policial_no_ultimo_ano, y=n, x=qual_e_a_sua_cor_da_pele)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Respondentes que foram ou não abordados pela policia em 2020 por cor",
    fill = "Abordados pela polícia"
    
  )

ipvio_2021 %>%
  count(qual_e_a_sua_cor_da_pele,voce_foi_abordado_por_um_policial_no_ultimo_ano, bairro)%>%
  mutate(voce_foi_abordado_por_um_policial_no_ultimo_ano =  
           forcats::fct_reorder(voce_foi_abordado_por_um_policial_no_ultimo_ano, n)) %>%
  ggplot(aes(fill=voce_foi_abordado_por_um_policial_no_ultimo_ano, y=n, x=qual_e_a_sua_cor_da_pele)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n),position=position_dodge(0.9))+
  facet_wrap(~bairro)+
  theme_fivethirtyeight()+
  labs(
    x = "Bairros", y = "Entrevistados",
    title = "Respondentes que foram ou não abordados pela policia em 2020 por cor e bairro",
    fill = "Abordados pela polícia"
    
  )

writexl::write_xlsx(ipvio_2021, "ipvio_2021.xlsx")