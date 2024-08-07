#### 0. CONFIGURAR O AMBIENTE ####
# 0.1 Carregar pacotes necessários ####
library(readxl)
library(tidyverse)
library(dplyr)
library(googledrive)
library(summarytools)
library(kableExtra)
library(psych)
library(officer)
library(ggplot2)
library(ggcorrplot)
library(hrbrthemes)
library(viridis)

# Definir o caminho local do arquivo
caminho_local <- 'C:/Users/Alexandre_Nicolella/Projetos/Em andamento/Usina Pedra/4-Dados e documentos/Dados/Usina da Pedra/historico16_23.xlsx'

# 1. LEITURA DO ARQUIVO  DO EXCEL
  df <- read_excel(caminho_local, 
                   #skip = 1, 
                   sheet = 'BASE_2016_2023')
## Observacoes importantes
  ## 1.1: Retiramos os talhoes com menos de 5 ha
  ## 1.2: Com base no TCH real retiramos os 5% maiores e menores valores.Existima valores acima de 1.000 o que seguramente está errado 
  ## Com isso diminuimos os erros de medidas
    

## 2.  FUNCOES PERSONILIZADAS
# 2.1 Moda
# Função para calcular a moda
moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

## 3. FORMATAR DF

# 3.1 Renomear colunas todas em minúscula
names(df)
df <- df %>% rename(safra=SAFRA,
                    layer_mapa=LAYER_MAPA,
                    unidade=UNIDADE,
                    tipo=TIPO,
                    nm=NM,
                    cod = CÓD,
                    secao = SEÇÃO,
                    talhao = TALHÃO,
                    area = ÁREA,
                    estagio = ESTÁGIO,
                    corte= CORTE,
                    variedade=VARIEDADE,
                    producao_real = `PRODUÇÃO REAL`,
                    tch_real = `TCH REAL`,
                    atr=ATR,
                    atrxproducao = `ATR * PRODUÇÃO`,
                    data_colheita = `DATA DE COLHEITA`,
                    mes_colheita = `MÊS DE COLHEITA`,
                    parceiro = PARCEIRO,
                    t_ha = `TON/HÁ`,
                    kg_atr = `KG ATR`,
                    vatr_safra= `VATR SAFRA`,
                    layer_safra_sec= `Layer safra seção`)


# Vamos manter uma copia banco base inicial - df_base

df_base<- df


# 3.2 Vamos retirar do banco aqueles que tiveram parceria plena
   ## -> Montar dois bancos de parceria plena e parceria somente preço

nrow(df) 

df <- df_base %>%
  filter(!grepl('^p', t_ha, ignore.case = TRUE))%>%
  filter(tipo != 'ARRENDAMENTO')

df_plena<- df_base %>%
  filter(grepl('plena', t_ha, ignore.case = TRUE))



# 3.3 Formatar colunas: indicar tipo de variável

df$tipo <- as.factor(df$tipo)
df$cod <- as.factor(df$cod)
df$talhao <- as.factor(df$talhao)
df$estagio <- as.factor(df$estagio)
df$corte <- as.integer(df$corte)
df$variedade <- as.factor(df$variedade)
df$mes_colheita <- as.double(df$mes_colheita)
df$t_ha <- as.double(df$t_ha)
                    
  # -> Checar se as colunas estão bem formatadas
str(df)
head(df)



############################
########  PARECER ##########
############################

# 4. ANÁLISE DO CONTRATO DE PARCERIA
   ## -> Por questão de comparabilidade vamos trabalhar com os contratos que iniciaram em 2016
    ## ou seja, aqueles que tivram corte 1 em 2016 e vaos analisar o contrato de forma 
    ## consolidada ao longo dos diversos cortes. por questão de comparabilidade, analisaremos
    ## os contratos que tiveram entre 4 e 8 cortes. 4 cortes (2016 a 2019) e 8 cortes (2016 a 2023)
    ## Estimando os valores médios dos contratos que iniciaram em 2016 e 2017 (para esse ano somente até o 7 o corte)


## 4.1 Código Único que estima a presenca do contrato

  ## vamos estabelecer que uma boa aproximação para o contrato seria o parceiro e o código da seção. 
  ## Dessa forma essas duas variáveis cria uma identificcao unica para o contrato
df<-df %>%
  mutate(contrato = paste(cod,"-",parceiro))

  ## -> Vamos retirar os valores 0 de ATR e ATRxProducao e troca-los por nulo para não entrar nos cálculos
df<-df %>%
  mutate(across(c(atr, atrxproducao), ~na_if(., 0)))


## 4.2 Montando o DF 2016-2023
  ## -> Vamos agrupar por contrato, secao e ano
  ## -> somente deixamos aqui aqueles contratos que foram 1o corte em 2016, 2o em 2017 etc
df_contrato<-df %>%
  filter(safra >= 2016 & safra <= 2023) %>%
  group_by(contrato, safra, cod, corte) %>%
  summarise(m_unidade=moda(unidade),
            m_tipo=moda(tipo),
            m_cod=moda(cod),
            sum_prod = sum(producao_real, na.rm = TRUE),
            sum_area = sum(area, na.rm = TRUE),
            av_tch_real = mean(tch_real, na.rm = TRUE),
            av_corte = mean(corte, na.rm = TRUE),
            produtividade = sum(producao_real)/sum(area),
            m_variedade=moda(variedade),
            av_atr = mean(atr, na.rm = TRUE),
            sum_atrxprod = sum(atrxproducao, na.rm = TRUE),
            av_mes_colheita = mean(mes_colheita),
            m_mes_colheita = mean(mes_colheita),
            av_t_ha = mean(t_ha, na.rm = TRUE),
            av_kg_atr = mean(kg_atr, na.rm = TRUE),
            av_vatr_safra = mean(vatr_safra, na.rm = TRUE)
            )  %>%
  filter((safra==2016 & corte==1) |  (safra==2017 & corte==2) | 
           (safra==2018 & corte==3) | (safra==2019 & corte==4) | 
           (safra==2020 & corte==5) | (safra==2021 & corte==6) | 
           (safra==2022 & corte==7) | (safra==2023 & corte==8)
   )

    ## Entretanto há contratos que tem 2o corte em 2017 mas não apresenta 1o corte em 2016
    ## Vamos identificar o contrato que te registro em 2016. 
  ## ->  Identificando contratos com registros na safra de 2016
parceiros_2016 <- df_contrato %>%
  filter(safra == 2016) %>%
  select(contrato) %>%
  distinct()

  ## -> filtrando para quem tem safra 1 em 2016
df_cont_clean <- df_contrato %>%
  filter(contrato %in% parceiros_2016$contrato)

  ## Vamos retirar também aqueles que informam menos do que 4 safras, Tem que ter 
  ## pelo menos a safra 4 em 2019. 

 ## ->  Parceiros com pelo menos 4 cortes 
parceiros_2019 <- df_cont_clean %>%
  filter(safra == 2019) %>%
  select(contrato) %>%
  distinct()

 ## -> filtrando para quem tem safra 4 em 2019
df_cont_clean <- df_cont_clean %>%
  filter(contrato %in% parceiros_2019$contrato)


## 4.3 Resumo do contrato

  ## Montamos a tabela resumo por contrato. Ou seja, o valor médio do contrato ao longo dos anos
  ## Vamos considerar somente os contratos que tiveram entre 4 e 8 cortes. 

resumo_cont<-df_cont_clean %>%
  group_by(contrato) %>%
  summarise(m_unidade=moda(m_unidade),
            m_tipo=moda(m_tipo),
            m_cod=moda(m_cod),
            sum_prod = sum(sum_prod, na.rm = TRUE),
            sum_area = sum(sum_area, na.rm = TRUE),
            av_tch_real = mean(av_tch_real, na.rm = TRUE),
            av_corte = mean(av_corte, na.rm = TRUE),
            produtividade = sum(sum_prod)/sum(sum_area),
            m_variedade=moda(m_variedade),
            av_atr = mean(av_atr, na.rm = TRUE),
            sum_atrxprod = sum(sum_atrxprod, na.rm = TRUE),
            av_mes_colheita = mean(av_mes_colheita),
            m_mes_colheita = mean(m_mes_colheita),
            av_t_ha = mean(av_t_ha, na.rm = TRUE),
            av_kg_atr = mean(av_kg_atr, na.rm = TRUE),
            av_vatr_safra = mean(av_vatr_safra, na.rm = TRUE)
            
            )



# Com a media de corte podemos retomar o número de safra que o contrato teve. 
  ## ->  Para 4 safras (1,2,3,4) o valor médio é 2,5. Para 5 o valor medio e 3....

## Considerando somente quem tem 4 até 8  cortes
resumo_cont_3 <-resumo_cont %>%
  filter(av_corte == 2.5 | av_corte == 3 | av_corte == 3.5 | av_corte == 4 | av_corte == 4.5 )%>%
filter(contrato != "30669 - JOSE MARCIO CAVALHEIRE")

# Valores estranhos para Cavalheire chega no 8 corte com a maior produtividade. 
# Vamos retirar esse contrato, mais de 100 TCH no 8o corte e sempre subindo


## Duas variáveis são importantes aqui. A primeira é o pagamento que é a produção fixa paga 
## vezes o ATR em contrato. Essa seia opagamento recebido. 
## A segunda é o pagamento pleno que é um percentual da produção de ATR por ha
## OU seja, produçãoxATR vezes um percentual. Essa chamaremos de pag_pleno_ha
## o percentual foi estabelecido com base no valor da tonelada paga atualmente pela usina. 
## EX.: Aqueles que recebem 19 ton/ha ou menos ficará com percentual de 19%
## aqueles entre 19 e 23 receberao 24% ..... e assim por diante. 

resumo_cont_3<-resumo_cont_3 %>%
  mutate(atr_prod_ha=produtividade*av_atr)

resumo_cont_3<-resumo_cont_3 %>%
  mutate(pagamento_ha = av_kg_atr*av_t_ha) 

resumo_cont_3<-resumo_cont_3 %>%
  mutate(pleno = case_when(av_t_ha < 19 ~ 0.19,
                           av_t_ha >= 19 & av_t_ha < 23 ~ 0.24,
                           av_t_ha >= 23 & av_t_ha < 27 ~ 0.29,
                           av_t_ha >= 27 ~ 0.34)) 



resumo_cont_3<-resumo_cont_3 %>%
  mutate(pag_pleno_ha = atr_prod_ha*pleno) 


## Diferença entre pgamento pleno e recebido
resumo_cont_3<-resumo_cont_3 %>%
  mutate(dif_pag = pagamento_ha - pag_pleno_ha,
         dif_perc = abs(dif_pag/pagamento_ha))




############################  2017  ##############################################

## 4.2 Montando o DF 2017-2023
## -> Vamos agrupar por contrato, secao e ano
## -> somente deixamos aqui aqueles contratos que foram 1o corte em 2016, 2o em 2017 etc
df_contrato_17<-df %>%
  filter(safra >= 2017 & safra <= 2023) %>%
  group_by(contrato, safra, cod, corte) %>%
  summarise(m_unidade=moda(unidade),
            m_tipo=moda(tipo),
            m_cod=moda(cod),
            sum_prod = sum(producao_real, na.rm = TRUE),
            sum_area = sum(area, na.rm = TRUE),
            av_tch_real = mean(tch_real, na.rm = TRUE),
            av_corte = mean(corte, na.rm = TRUE),
            produtividade = sum(producao_real)/sum(area),
            m_variedade=moda(variedade),
            av_atr = mean(atr, na.rm = TRUE),
            sum_atrxprod = sum(atrxproducao, na.rm = TRUE),
            av_mes_colheita = mean(mes_colheita),
            m_mes_colheita = mean(mes_colheita),
            av_t_ha = mean(t_ha, na.rm = TRUE),
            av_kg_atr = mean(kg_atr, na.rm = TRUE),
            av_vatr_safra = mean(vatr_safra, na.rm = TRUE)
  )  %>%
  filter((safra==2017 & corte==1) |  (safra==2018 & corte==2) | 
           (safra==2019 & corte==3) | (safra==2020 & corte==4) | 
           (safra==2021 & corte==5) | (safra==2022 & corte==6) | 
           (safra==2023 & corte==7)   )



## Entretanto há contratos que tem 2o corte em 2018 mas não apresenta 1o corte em 2017
## Vamos identificar o contrato que te registro em 2017. 
## ->  Identificando contratos com registros na safra de 2017
parceiros_2017 <- df_contrato_17 %>%
  filter(safra == 2017) %>%
  select(contrato) %>%
  distinct()

## -> filtrando para quem tem safra 1 em 2016
df_cont_clean_17 <- df_contrato_17 %>%
  filter(contrato %in% parceiros_2017$contrato)

## Vamos retirar também aqueles que informam menos do que 4 safras, Tem que ter 
## pelo menos a safra 4 em 2019. 

## ->  Parceiros com pelo menos 4 cortes 
parceiros_2020 <- df_cont_clean_17 %>%
  filter(safra == 2020) %>%
  select(contrato) %>%
  distinct()

## -> filtrando para quem tem safra 4 em 2019
df_cont_clean_17 <- df_cont_clean_17 %>%
  filter(contrato %in% parceiros_2020$contrato)


## 4.3 Resumo do contrato

## Montamos a tabela resumo por contrato. Ou seja, o valor médio do contrato ao longo dos anos
## Vamos considerar somente os contratos que tiveram entre 4 e 8 cortes. 

resumo_cont_17<-df_cont_clean_17 %>%
  group_by(contrato) %>%
  summarise(m_unidade=moda(m_unidade),
            m_tipo=moda(m_tipo),
            m_cod=moda(m_cod),
            sum_prod = sum(sum_prod, na.rm = TRUE),
            sum_area = sum(sum_area, na.rm = TRUE),
            av_tch_real = mean(av_tch_real, na.rm = TRUE),
            av_corte = mean(av_corte, na.rm = TRUE),
            produtividade = sum(sum_prod)/sum(sum_area),
            m_variedade=moda(m_variedade),
            av_atr = mean(av_atr, na.rm = TRUE),
            sum_atrxprod = sum(sum_atrxprod, na.rm = TRUE),
            av_mes_colheita = mean(av_mes_colheita),
            m_mes_colheita = mean(m_mes_colheita),
            av_t_ha = mean(av_t_ha, na.rm = TRUE),
            av_kg_atr = mean(av_kg_atr, na.rm = TRUE),
            av_vatr_safra = mean(av_vatr_safra, na.rm = TRUE)
            
  )



# Com a media de corte podemos retomar o número de safra que o contrato teve. 
## ->  Para 4 safras (1,2,3,4) o valor médio é 2,5. Para 5 o valor medio e 3....

## Considerando somente quem tem 4 até 8  cortes
resumo_cont_3_17 <-resumo_cont_17 %>%
  filter(av_corte == 2.5 | av_corte == 3 | av_corte == 3.5 | av_corte == 4 | av_corte == 4.5 )#%>%
  #filter(contrato != "30606 - FUNDAÇÃO EDUCACIONAL DE ITUVERAVA" | contrato !="30371 - ANTONIO CARLOS RIOS CORRAL E OUTRO")

# Valores estranhos para Cavalheire chega no 8 corte com a maior produtividade. 
# Vamos retirar esse contrato, mais de 100 TCH no 8o corte e sempre subindo

descr_produt17 <- resumo_cont_3_17 %>%
  select(produtividade) %>%
  descr(stats = c("mean", "sd", "q1", "med", "q3", "cv", "max", "min"))

print(descr_produt17)


## Criando variáveis

resumo_cont_3_17<-resumo_cont_3_17 %>%
  mutate(atr_prod_ha=produtividade*av_atr) 


resumo_cont_3_17<-resumo_cont_3_17 %>%
  mutate(pagamento_ha = av_kg_atr*av_t_ha) 

resumo_cont_3_17<-resumo_cont_3_17 %>%
  mutate(pleno = case_when(av_t_ha < 19 ~ 0.19,
                           av_t_ha >= 19 & av_t_ha < 23 ~ 0.24,
                           av_t_ha >= 23 & av_t_ha < 27 ~ 0.29,
                           av_t_ha >= 27 ~ 0.34)) 



resumo_cont_3_17<-resumo_cont_3_17 %>%
  mutate(pag_pleno_ha = atr_prod_ha*pleno) 

resumo_cont_3_17<-resumo_cont_3_17 %>%
  mutate(dif_pag = pagamento_ha - pag_pleno_ha,
         dif_perc = abs(dif_pag/pagamento_ha)) 


############ Juntano os dois bancos de dados 16 e 17    ########################

resumo_cont_f<-rbind(resumo_cont_3, resumo_cont_3_17)

resumo_cont_f<-resumo_cont_f %>%
  filter(contrato != "30371 - ANTONIO CARLOS RIOS CORRAL E OUTRO")

## Não possui a safra 3, pula da 2 para a 4 direto. Exluimos esse contrato

################################################################################


## 4.4 ANÁLISE DESCRITIVA DOS CONTRATOS

# Análise geral do dataframe
view(dfSummary(resumo_cont_f))


  ## 4.4.1 Análise da produtividade

# -> Realizar a análise descritiva
descr_produt <- resumo_cont_f %>%
  select(produtividade) %>%
  descr(stats = c("mean", "sd", "q1", "med", "q3", "cv", "max", "min"))

print(descr_produt)

# Formatar os resultados com kableExtra
descr_produt_kable <- descr_produt %>%
  kable("html", caption = "Estatísticas Descritivas da Produtividade") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
# Visualizar a tabela formatada
print(descr_produt_kable)


## Analise dos 80% dos contratos
-1.28*sd(resumo_cont_f$produtividade)+mean(resumo_cont_f$produtividade)
1.28*sd(resumo_cont_f$produtividade)+mean(resumo_cont_f$produtividade)

### Observação: Notamos uma média entre contratos de 77,6 com DP de 8.9. O Cv foi de 11,4% 
##  O que indica uma boa homogeneidade entre contratos. 50% dos contratos ficaram entre 72e 82 ton/ha de média
## 80% dos contratos ficaram entre 66 e 89 ton/ha.


## Análise visual: Correlograma, Histograma e boxplot

# Calcular a matriz de correlação
cor_matrix <- cor(resumo_cont_f %>% 
                    select(-c('contrato', 'm_unidade', 'm_tipo', 'm_cod','m_variedade')), use = "complete.obs")

# Criar o correlograma
ggcorrplot(cor_matrix, 
           method = "circle", 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           colors = c("brown4", "white", "darkblue"), 
           title = "Correlograma das Variáveis",
           ggtheme = theme_classic())

# Criando Histograma
hist_produt<- resumo_cont_f %>% 
  ggplot( aes(x = produtividade)) +
  geom_histogram(binwidth = 5,  color = "darkblue", fill="deepskyblue4", alpha = 0.6, position = 'identity') +
  labs(title = "Histograma Produtividade dos Contratos (t/ha)", x = "Ton/ha", y = "Frequência") +
  theme_classic()
hist_produt

bp_produt <- resumo_cont_f %>% 
  ggplot( aes(x = produtividade)) + 
  geom_boxplot(outlier.colour="brown4",
               outlier.size=2, fill="lightblue", color="darkblue")+
  theme_classic()
 
bp_produt + labs(title="Produtividade dos Contratos",
          x ="Produtividade ton/ha")




## 4.4.2 Análise do ATR

## ATR medio por contrato
## Aqui estamos analisando a quantidade de atr produzido por ha, em kg.
## ATR é a quantidade de açucar que se pode extrair da cana Acucar Total recuperável.

# -> Realizar a análise descritiva
descr_atr <- resumo_cont_f %>%
  select(atr_prod_ha) %>%
  descr(stats = c("mean", "sd", "q1", "med", "q3", "cv", "max", "min"))

print(descr_atr)

# Formatar os resultados com kableExtra
descr_atr_kable <- descr_produt %>%
  kable("html", caption = "Estatísticas Descritivas da ATR por ha (kg)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
# Visualizar a tabela formatada
print(descr_atr_kable)


## Analise dos 80% dos contratos
-1.28*sd(resumo_cont_f$atr_prod_ha)+mean(resumo_cont_f$atr_prod_ha)
1.28*sd(resumo_cont_f$atr_prod_ha)+mean(resumo_cont_f$atr_prod_ha)

### Observação: Notamos uma média entre contratos de 10781.44 com DP de 1355.73. O CV foi de 13% 
##  O que indica uma boa homogeneidade entre contratos em termos de ATR. 50% dos contratos ficaram entre 10.6 e 11.7 ton/ha de média
## 80% dos contratos ficaram entre 9 e 12.5 ton/ha de ATR.


## Análise visual: Correlograma, Histograma e boxplot

# Criando Histograma
hist_atr<- resumo_cont_f %>% 
  ggplot( aes(x = atr_prod_ha)) +
  geom_histogram(binwidth = 500,  color = "darkblue", fill="deepskyblue4", alpha = 0.6, position = 'identity') +
  labs(title = "Histograma ATR por ha por Contrato (kg/ha)", x = "Kg/ha", y = "Frequência") +
  theme_classic()
hist_atr

bp_atr <- resumo_cont_f %>% 
  ggplot( aes(x = atr_prod_ha)) + 
  geom_boxplot(outlier.colour="brown4",
               outlier.size=2, fill="lightblue", color="darkblue")+
  theme_classic()

bp_atr + labs(title="Quilos de ATR por ha dos Contratos",
                 x ="ATR por ha (kg/ha)")



### Mes de colheita é muito importante para o ATR, afeta pouco a produção, mas afeta ATR
## Vejamos a regressão simples abaixo

lm(av_atr ~ m_mes_colheita, data = resumo_cont_f) %>% summary()

### Vamos considerar dois períodos, grupo 1: meses de 6 a 9 e 
   ##grupo 2: meses de 1 a 5 e 10 a 12
resumo_cont_f %>%
  filter(av_mes_colheita>6) %>%
  summarise(mean_atr = mean(av_atr),
            sd_atr = sd(av_atr),
            cv_atr = sd(av_atr)/mean(av_atr),
            n = n())

resumo_cont_f %>%
  filter(av_mes_colheita<=6 ) %>%
  summarise(mean_atr = mean(av_atr),
            sd_atr = sd(av_atr),
            cv_atr = sd(av_atr)/mean(av_atr),
            n = n())




## 5. Simulando recebido e quanto receberia na parceria plena



# -> Realizar a análise descritiva
descr_pag <- resumo_cont_f %>%
  select(pag_pleno_ha,pagamento_ha) %>%
  descr(stats = c("mean", "sd", "q1", "med", "q3", "cv", "max", "min"))

print(descr_pag)

# Formatar os resultados com kableExtra
descr_pag_kable <- descr_produt %>%
  kable("html", caption = "Estatísticas Descritivas da ATR por ha (kg)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
# Visualizar a tabela formatada
print(descr_produt_kable)



hist_pag <- resumo_cont_f %>%
  select( pagamento_ha, pag_pleno_ha) %>%
  pivot_longer(pag_pleno_ha | pagamento_ha, names_to = "tipo", values_to = "pagamento") %>%
  filter(tipo %in% c("pag_pleno_ha", "pagamento_ha")) %>%
  ggplot( aes(x=pagamento, color=tipo, fill=tipo)) +
  geom_density(alpha=0.6) +
  theme_minimal()
hist_pag + scale_fill_manual(values = c("burlywood4", "skyblue3")) +
  scale_color_manual(values = c("burlywood", "skyblue"))+
  labs(title = "Distribuição do ATR recebido Real e pela Parceria Plena Simulada (ATR/ha)",
       x = "ATR por ha", y = "Densidade") +
  theme_classic()


 

# -> Realizar a análise descritiva
dif_pagamento <- resumo_cont_f %>%
  select(dif_pag,dif_perc) %>%
  descr(stats = c("mean", "sd", "q1", "med", "q3", "cv", "max", "min"))

print(dif_pagamento)


hist_dif_perc <- resumo_cont_f %>%
  ggplot( aes(x=dif_perc)) +
  geom_density(alpha=0.4, fill="skyblue3", color='skyblue') +
   xlim(0, 1)+
  theme_minimal()
hist_dif_perc  +labs(title = "Diferença percentual entre o ATR recebido Real e aquele que seria recebido pela Parceria Plena Simulada (ATR/ha)",
       x = "Diferença de ATR por ha (%)", y = "Densidade") +
  theme_classic()

## 82% dos contratos tem diferença de 20% ou menos
# 52% dos contratos tem diferença menor do que 10%

resumo_cont_f %>%
  filter(dif_perc < 0.1) %>%
  count()

resumo_cont_f %>%
count()

114/219






  

































