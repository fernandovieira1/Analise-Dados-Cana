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
library(crosstable)

# Definir o caminho local do arquivo
caminho_local <- 'C:/Users/Alexandre_Nicolella/Projetos/Em andamento/Usina Pedra/4-Dados e documentos/Dados/Usina da Pedra/Histórico de encerrados 2016 - 2023.xlsx'

# 1. LEITURA DO ARQUIVO  DO EXCEL
  df <- read_excel(caminho_local, 
                   #skip = 1, 
                   sheet = 'BASE_2016_2023')
  
original <- read_excel(caminho_local, 
                   skip = 1, 
                   sheet = 'BASE_2016_2023')
## Observacoes importantes
  ## 1.1: Vamos considerar totas as observações 
  ## 1.2: Vamos considerar os valores por seção e ano
  ## Secao é a fazenda que é dividada em talhoes
    

## 2.  FUNCOES PERSONILIZADAS
# 2.1 Moda
# Função para calcular a moda
moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

## 3. FORMATAR DF

# 3.1 Renomear colunas todas em minúscula
names(original)
df <- original %>% rename(safra=SAFRA,
                    layer_mapa=`LAYER MAPA`,
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
   ## -> Por questão de comparabilidade vamos trabalhar com os contratos que iniciaram em 2016 e 2017
    ## ou seja, aqueles que tivram corte 1 em 2016 e 1 corte em 2017. Vamos analisar o contrato de forma 
    ## consolidada ao longo dos diversos cortes. por questão de comparabilidade, analisaremos
    ## os contratos que tiveram entre 4 e 8 cortes. 4 cortes (2016 a 2019) e 8 cortes (2016 a 2023)
    ## Estimando os valores médios dos contratos que iniciaram em 2016 e 2017 (para esse ano somente até o 7 o corte)


## 4.1 Código Único que estima a presenca do contrato

  ## vamos estabelecer que uma boa aproximação para o contrato seria o o cod da secao e os valores de contrato
  # t_ha e kg_Atr. . 
  ## Dessa forma essas duas variáveis cria uma identificcao unica para o contrato
df<-df %>%
  mutate(contrato = paste(cod,t_ha*10000000000+kg_atr*10000)) |> 
  filter(t_ha>=10 & t_ha<=30) |> 
  filter(area>=5)
  
  # vamos eliminar valores estranhos, menores do que 10 e maiore do que 40. 
  
  
  
  ## -> Vamos retirar os valores 0 de ATR e ATRxProducao e troca-los por nulo para não entrar nos cálculos
df<-df %>%
  mutate(across(c(atr, atrxproducao), ~na_if(., 0)))


## 4.2 Montando o DF 2016-2023
  ## -> Vamos agrupar por contrato, secao,  ano e talhao
  ## -> somente deixamos aqui aqueles contratos que foram 1o corte em 2016, 2o em 2017 etc
df_contrato<-df %>%
  filter(safra >= 2016 & safra <= 2023) %>%
  group_by(contrato, safra, corte, talhao) %>%
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
    ## Vamos identificar o contrato que tem registro em 2016. 
  ## ->  Vamos tentar identificar os talhoes que tiveram prmeiro corte em 2016
    ## Por isso juntamos o contrato com o talhão. Com isso garantimos que pegamos 
   # a mesma área e o mesmo acordo comercial ao longo dos anos. 

df_contrato <- df_contrato |> 
  mutate(contrato_t = paste(contrato,talhao))


parceiros_2016 <- df_contrato %>%
  filter(safra == 2016) %>%
  select(contrato_t) %>%
  distinct()

  ## -> filtrando para quem tem safra 1 em 2016
df_cont_clean <- df_contrato %>%
  filter(contrato_t %in% parceiros_2016$contrato_t) 
  

  ## Vamos retirar também aqueles que informam menos do que 4 safras, Tem que ter 
  ## pelo menos a safra 4 em 2019. 

 ## ->  Parceiros com pelo menos 4 cortes 
parceiros_2019 <- df_cont_clean %>%
  filter(safra == 2019) %>%
  select(contrato_t) %>%
  distinct()

 ## -> filtrando para quem tem safra 4 em 2019
df_cont_clean <- df_cont_clean %>%
   filter(contrato_t %in% parceiros_2019$contrato_t)


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
  filter(contrato!="30749 247931219676")

# Valores estranhos para Cavalheire chega no 8 corte com a maior produtividade. 
# Vamos retirar esse contrato, mais de 100 TCH no 8o corte e sempre subindo
# segundo filtre o contrato tinha um tch muito elevado, acima de 200


## Duas variáveis são importantes aqui. A primeira é o pagamento que é a produção fixa paga 
## vezes o ATR em contrato. Essa seria o pagamento recebido. 
## A segunda é o pagamento pleno que é um percentual da produção de ATR por ha
## OU seja, produçãoxATR vezes um percentual. Essa chamaremos de pag_pleno_ha
## o percentual foi estabelecido com base no valor da tonelada paga atualmente pela usina. 
## EX.: Aqueles que recebem 19 ton/ha ou menos ficará com percentual de 19%
## aqueles entre 19 e 23 receberao 23% ..... e assim por diante. 

resumo_cont_3<-resumo_cont_3 %>%
  mutate(atr_prod_ha=produtividade*av_atr)

resumo_cont_3<-resumo_cont_3 %>%
  mutate(pagamento_ha = av_kg_atr*av_t_ha) 

resumo_cont_3<-resumo_cont_3 %>%
  mutate(pleno = case_when(av_t_ha < 19 ~ 0.19,
                           av_t_ha >= 19 & av_t_ha < 23 ~ 0.23,
                           av_t_ha >= 23 & av_t_ha < 27 ~ 0.27,
                           av_t_ha >= 27 ~ 0.30)) 



resumo_cont_3<-resumo_cont_3 %>%
  mutate(pag_pleno_ha = atr_prod_ha*pleno) 


## Diferença entre pgamento pleno e recebido
resumo_cont_3<-resumo_cont_3 %>%
  mutate(dif_pag = pagamento_ha - pag_pleno_ha,
         dif_perc = abs(dif_pag/pagamento_ha))




############################  2017  ##############################################

## 4.2 Montando o DF 2017-2023
## -> Vamos agrupar por contrato, secao e ano
## -> somente deixamos aqui aqueles contratos que foram 1o corte em 2017, 2o em 2018 etc
df_contrato_17<-df %>%
  filter(safra >= 2017 & safra <= 2023) %>%
  group_by(contrato, safra, corte, talhao) %>%
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
## Vamos identificar o contrato que tem registro em 2017. 
## ->  Identificando contratos com registros na safra de 2017 por talhao

df_contrato_17 <- df_contrato_17 |> 
  mutate(contrato_t = paste(contrato,talhao))

parceiros_2017 <- df_contrato_17 %>%
  filter(safra == 2017) %>%
  select(contrato_t) %>%
  distinct()

## -> filtrando para quem tem safra 1 em 2016
df_cont_clean_17 <- df_contrato_17 %>%
  filter(contrato_t %in% parceiros_2017$contrato_t)

## Vamos retirar também aqueles que informam menos do que 4 safras, Tem que ter 
## pelo menos a safra 4 em 2019. 

## ->  Parceiros com pelo menos 4 cortes 
parceiros_2020 <- df_cont_clean_17 %>%
  filter(safra == 2020) %>%
  select(contrato_t) %>%
  distinct()

## -> filtrando para quem tem safra 4 em 2019
df_cont_clean_17 <- df_cont_clean_17 %>%
  filter(contrato_t %in% parceiros_2020$contrato_t)


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
  filter(av_corte == 2.5 | av_corte == 3 | av_corte == 3.5 | av_corte == 4 | av_corte == 4.5 )



###    Criando variáveis

resumo_cont_3_17<-resumo_cont_3_17 %>%
  mutate(atr_prod_ha=produtividade*av_atr) 


resumo_cont_3_17<-resumo_cont_3_17 %>%
  mutate(pagamento_ha = av_kg_atr*av_t_ha) 

resumo_cont_3_17<-resumo_cont_3_17 %>%
  mutate(pleno = case_when(av_t_ha < 19 ~ 0.19,
                           av_t_ha >= 19 & av_t_ha < 23 ~ 0.23,
                           av_t_ha >= 23 & av_t_ha < 27 ~ 0.27,
                           av_t_ha >= 27 ~ 0.30)) 



resumo_cont_3_17<-resumo_cont_3_17 %>%
  mutate(pag_pleno_ha = atr_prod_ha*pleno) 

resumo_cont_3_17<-resumo_cont_3_17 %>%
  mutate(dif_pag = pagamento_ha - pag_pleno_ha,
         dif_perc = abs(dif_pag/pagamento_ha)) 



################################################################################
############ Juntano os dois bancos de dados 16 e 17    ########################

resumo_cont_f<-rbind(resumo_cont_3, resumo_cont_3_17)

resumo_cont_f<-resumo_cont_f |> 
  mutate(produtividade=sum_prod/sum_area)
  
################################################################################



#########   Limpando alguns contratos atípicos ############################

### Contrato: 30789 256201219676  e 30315 227271219676-> tem apenas 4 safras e termina a quarta com mais de 90 toneladas.
  # Não tem a 5 safara. 
#### Contrato: 30365 289261219676   termina 7 corte com mais de 100 tno por ha.
# 30606 227271219676 termina com mais de 90 ton no 4 corte
  # e contrato: 10531 235541219676 operou sem abaixo de 50 t/ha
  # 10380 247931219676 valor discrepante no meio da seria de 120 toneladas por ha. 
#10531 235541219676 valor bem baixo chegando a 30 na penultima e 15 na ultima
#30371 206611219676 oulou o 3o corte

#30553 239671219676 falta 2o corte
#30558 239671219676 falta 2 corte
#50071 196281219676 falta o 30 corte
#50076 161161091900 falta o 30 corte
# 30555 231401219676 primeiro corte deu 7 toneladas



resumo_cont_f<-resumo_cont_f %>%
  filter(!contrato %in% c("30365 289261219676", "10531 235541219676", "10380 247931219676", "10531 235541219676",
                          "30371 206611219676", "30553 239671219676", "30558 239671219676", "50071 196281219676",
                          "50076 161161091900", "30606 227271219676","30789 256201219676","30315 227271219676",
                          "30555 231401219676"))



## 4.4 ANÁLISE DESCRITIVA DOS CONTRATOS

# Análise geral do dataframe
view(dfSummary(resumo_cont_f))


  ## 4.4.1 Análise da produtividade

# -> Realizar a análise descritiva
descr_produt <- resumo_cont_f %>%
  select(produtividade) %>%
  descr(stats = c("mean", "sd", "q1", "med", "q3", "cv", "max", "min"))

print(descr_produt)


## Analise dos 80% dos contratos
-1.28*sd(resumo_cont_f$produtividade)+mean(resumo_cont_f$produtividade)
1.28*sd(resumo_cont_f$produtividade)+mean(resumo_cont_f$produtividade)

### Observação: Notamos uma média entre contratos de 79,07 com DP de 11.00. O Cv foi de 13,9% 
##  O que indica uma boa homogeneidade entre contratos. 50% dos contratos ficaram entre 72 e 86 ton/ha de média
## 80% dos contratos ficaram entre 65 e 93 ton/ha.

quantile(resumo_cont_f$produtividade, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))

quartil_le<-do.call("rbind",tapply(resumo_cont_f$produtividade, resumo_cont_f$pleno, quantile))


## Esse modelo muito simples mostra que o pagamento da usina se relaciona fortemente
# com a produtividade. Isso só é possível se houver fatores fixos que imapctama produtividade
# tal como solo, relevo. E isso não é variável, não tem risco. 

lm(produtividade ~ av_t_ha, data = df_cont_clean) %>% summary()


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

# Criando Histograma e boxplot
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


### Produtividade média por corte

prod_corte_cont<-df_cont_clean %>%
  group_by(av_corte) %>%
  summarise(mean_prod = mean(produtividade),
            sd_prod = sd(produtividade),
            cv_prod = sd(produtividade)/mean(produtividade),
            n = n()) |> 
  filter(av_corte < 7)

ggplot(prod_corte_cont) +
  geom_bar( aes(x=av_corte, y=mean_prod), stat="identity", fill="deepskyblue4", alpha=0.7) +
  geom_errorbar( aes(x=av_corte, ymin=mean_prod-sd_prod, ymax=mean_prod+sd_prod), width=0.4, colour="orange", alpha=0.9, linewidth=1.3)+
  labs(title = "Produtividade dos Contratos por corte (t/ha)", x = "Corte", y = "Média da Produtividade (t/ha)") +
  geom_hline(yintercept = mean(prod_corte_cont$mean_prod), col = "red", lty = 2)+
  theme_classic()



## 4.4.2 Análise do ATR

## ATR medio por contrato
## Aqui estamos analisando a quantidade de atr produzido por ha, em kg.
## ATR é a quantidade de açucar que se pode extrair da cana Acucar Total recuperável.

resumo_cont_f <-resumo_cont_f |> 
  mutate(atr_prod_ha=produtividade*av_atr)

# -> Realizar a análise descritiva
descr_atr <- resumo_cont_f %>%
  select(atr_prod_ha) %>%
  descr(stats = c("mean", "sd", "q1", "med", "q3", "cv", "max", "min"))

print(descr_atr)


## Analise dos 80% dos contratos
-1.28*sd(resumo_cont_f$atr_prod_ha)+mean(resumo_cont_f$atr_prod_ha)
1.28*sd(resumo_cont_f$atr_prod_ha)+mean(resumo_cont_f$atr_prod_ha)

### Observação: Notamos uma média entre contratos de 11055.24 com DP de 1600.37. O CV foi de 14% 
##  O que indica uma boa homogeneidade entre contratos em termos de ATR. 50% dos contratos ficaram entre 10.0 e 11.9 ton/ha de média
## 80% dos contratos ficaram entre 9 e 13.1 ton/ha de ATR.


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
cor(resumo_cont_f$av_atr, resumo_cont_f$m_mes_colheita)


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
  select(dif_pag,dif_perc,pleno) %>%
  descr(stats = c("mean", "sd", "q1", "med", "q3", "cv", "max", "min"))

print(dif_pagamento)

dif_pagamento<-resumo_cont_f %>%
  select(dif_pag,dif_perc,pleno)

with(dif_pagamento, 
     stby(data=dif_pagamento,
          INDICES = pleno, 
          FUN = descr, stats = c("mean", "sd", "q1", "med", "q3", "cv", "max", "min")))


hist_dif_perc <- resumo_cont_f %>%
  ggplot( aes(x=dif_perc)) +
  geom_density(alpha=0.4, fill="skyblue3", color='skyblue') +
   xlim(0, 1)+
  theme_minimal()
hist_dif_perc  +labs(title = "Diferença percentual entre o ATR recebido Real e aquele que seria recebido pela Parceria Plena Simulada (ATR/ha)",
       x = "Diferença de ATR por ha (%)", y = "Densidade") +
  theme_classic()

## 80% dos contratos tem diferença de 20% ou menos
# 50% dos contratos tem diferença menor do que 10%



resumo_cont_f %>%
  filter(dif_perc < 0.1) %>%
  count()

resumo_cont_f %>%
count()

185/231
116/231




  






























