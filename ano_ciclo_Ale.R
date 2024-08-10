##############################################################################
##########    Análise ao longo dos anos  ####################################
#############################################################################

# a ideia aqui é verificar com foi a variação ao longo dos anos no grupo
#

df_ano <- df_base %>%
  filter(!grepl('^p', t_ha, ignore.case = TRUE))%>%
  filter(tipo != 'ARRENDAMENTO')



# 3.3 Formatar colunas: indicar tipo de variável

df_ano$tipo <- as.factor(df_ano$tipo)
df_ano$cod <- as.factor(df_ano$cod)
df_ano$talhao <- as.factor(df_ano$talhao)
df_ano$estagio <- as.factor(df_ano$estagio)
df_ano$corte <- as.integer(df_ano$corte)
df_ano$variedade <- as.factor(df_ano$variedade)
df_ano$mes_colheita <- as.double(df_ano$mes_colheita)
df_ano$t_ha <- as.double(df_ano$t_ha)

# -> Checar se as colunas estão bem formatadas
str(df)
head(df)

## 4.1 Código Único que estima a presenca do contrato

## vamos estabelecer que uma boa aproximação para o contrato seria o o cod da secao e os valores de contrato
# t_ha e kg_Atr. . 
## Dessa forma essas duas variáveis cria uma identificcao unica para o contrato
df_ano<-df_ano %>%
  mutate(contrato = paste(cod,t_ha*10000000000+kg_atr*10000)) |> 
  filter(t_ha>=10 & t_ha<=30) |> 
  filter(area>=5)

# vamos eliminar valores estranhos, menores do que 10 e maiore do que 40. 



## -> Vamos retirar os valores 0 de ATR e ATRxProducao e troca-los por nulo para não entrar nos cálculos
df_ano<-df_ano %>%
  mutate(across(c(atr, atrxproducao), ~na_if(., 0)))


############################
########  PARECER ##########
############################

# 4. ANÁLISE AO LONGO DOS ANOS
## -> Iremos utilizar toda a base e agregar ela por ano de safra. Questão aqui é se temos mais ou menos
## o mesmo ciclo por ano, mas conseguimos verificar isso mais a frente. 



## 4.2 Montando o DF 2016-2023
## -> Vamos agrupar por contrato, secao e ano
## -> somente deixamos aqui aqueles contratos que foram 1o corte em 2016, 2o em 2017 etc
df_ano_res<-df_ano %>%
  group_by(safra) %>%
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
            av_vatr_safra = mean(vatr_safra, na.rm = TRUE),
            desv_media=(produtividade-77.77)/produtividade,
            atr_ha = produtividade*av_atr
            )

df_ano_res %>%
  ggplot( aes(x=safra, y=desv_media)) +
  geom_line(color = "skyblue3", linewidth = 2) +
  geom_point(color = "skyblue4", size = 5)+
  ylim(-1, 1)+
  geom_hline(yintercept = c(0), col = "brown", lty = 2)+
  geom_hline(yintercept = c(-0.1,0.1), col = "burlywood4", lty = 2)+
  annotate(geom="text", x=2017, y=0.15, label="+10% de Desvio", color="burlywood4")+
  annotate(geom="text", x=2017, y=-0.15, label="-10% de Desvio", color="burlywood4")+
  labs(title = "Desvio da média de produtividade por safra",
       x = "Safra",
       y = "Desvio Percentua em Relação a Média da Produtividade")+
  theme_classic()

df_ano_res %>%
  ggplot( aes(x=safra, y=produtividade)) +
  geom_line(color = "skyblue3", linewidth = 2) +
  geom_point(color = "skyblue4", size = 5)+
  ylim(0, 150)+
  geom_hline(yintercept = c(77.77), col = "brown", lty = 2)+
  geom_hline(yintercept = c(85.5,70), col = "burlywood4", lty = 2)+
  annotate(geom="text", x=2016, y=90, label="+10%", color="burlywood4")+
  annotate(geom="text", x=2016, y=68, label="-10%", color="burlywood4")+
  labs(title = "Média de Produtividade por Safra",
       x = "Safra",
       y = "Produtividade em ton/ha")+
  theme_classic()

df_ano_res %>%
  ggplot( aes(x=safra, y=atr_ha)) +
  geom_line(color = "cornsilk3", linewidth = 2) +
  geom_point(color = "cornsilk4", size = 5)+
  ylim(5000, 15000)+
  geom_hline(yintercept = c(10507.63), col = "brown", lty = 2)+
  geom_hline(yintercept = c(9456.9,11558.4), col = "darkolivegreen", lty = 2)+
  annotate(geom="text", x=2016, y=9000, label="+10%", color="darkolivegreen")+
  annotate(geom="text", x=2016, y=12000, label="-10%", color="darkolivegreen")+
  labs(title = "Média de ATR por ha por Safra",
       x = "Safra",
       y = "Produtividade em kg/ha")+
  theme_classic()


#### Agora vamos olhar a mesma safra ao longo dos anos, vamos pegar a safra 2, 3 e 4
# evitaremos a safra 1, pois a cana pode ter varias épocas de plantio
df_safra_cort<-df_ano %>%
  filter(corte %in% c(2,3,4,5)) %>%
  group_by(safra, corte) %>%
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
            av_vatr_safra = mean(vatr_safra, na.rm = TRUE),
            desv_media=(produtividade-69.8)/produtividade, # média para 3 corte
            atr_ha = produtividade*av_atr
  )


df_safra_cort |> 
ggplot( aes(x=safra, y=produtividade, group=corte, color=corte)) +
  geom_line( linewidth = 2) +
  geom_point(color = "skyblue4", size = 5)+
  geom_hline(yintercept = c(95.81, 87.71, 78.39), col = "burlywood", lty = 2)+
  ylim(0,150)+
  annotate(geom="text", x=2018, y=100, label="+10% da Produtividade 2o corte", color="burlywood4")+
  annotate(geom="text", x=2018, y=82, label="-10% da Produtividade 2o corte", color="burlywood4")+
  labs(title = "Produtividade por Safra e por Corte",
       x = "Safra",
       y = "Produtividade (t/ha)",
       colours="corte")+
  theme_classic()


df_safra_cort |> 
  ggplot( aes(x=safra, y=atr_ha, group=corte, color=corte)) +
  geom_line( linewidth = 2) +
  geom_point(color = "skyblue4", size = 5)+
  geom_hline(yintercept = c(10605.5, 11783.9, 12962.3), col = "burlywood", lty = 2)+
  ylim(3000,17000)+
  annotate(geom="text", x=2018, y=13500, label="+10% da Produtividade de ATR 2o corte", color="burlywood4")+
  annotate(geom="text", x=2018, y=10900, label="-10% da Produtividade de ATR 2o corte", color="burlywood4")+
  labs(title = "Produtividade em kg de ATR/ha por Safra e por Corte",
       x = "Safra",
       y = "Produtividade (kg/ha)",
       colours="corte")+
  theme_classic()

apoio<-df_safra_cort |> 
  filter(corte=="2")
mean(apoio$atr_ha)

df_safra_cort |> 
  ggplot( aes(x=safra, y=produtividade, group=corte, color=corte)) +
  geom_line( linewidth = 2) +
  geom_point(color = "skyblue4", size = 5)+
  geom_hline(yintercept = c(62, 69.8, 77), col = "burlywood4", lty = 2,linewidth=1)+
  ylim(0,150)+
  annotate(geom="text", x=2018, y=81, label="+10% da Produtividade do 3o corte", color="burlywood4")+
  annotate(geom="text", x=2018, y=53, label="-10% da Produtividade do 3o corte", color="burlywood4")+
  labs(title = "Produtividade por Corte e Ano de Safra",
       x = "Safra",
       y = "Produtividade (t/ha)",
       colours="corte")+
  theme_classic()



df_safra_cort |> 
  ggplot( aes(x=safra, y=produtividade, group=corte, color=corte)) +
  geom_line( linewidth = 2) +
  geom_point(color = "skyblue4", size = 5)+
  ylim(0,150)+
  labs(title = "Produtividade por Corte e Ano de Safra",
       x = "Safra",
       y = "Produtividade (t/ha)",
       colours="corte")+
  theme_classic()+
facet_grid(cols = vars(corte))




df_safra_cort %>%
  filter(corte==3) %>%
ggplot( aes(x=safra, y=desv_media)) +
  geom_line(color = "skyblue3", linewidth = 2) +
  geom_point(color = "skyblue4", size = 5)+
  ylim(-1, 1)+
  geom_hline(yintercept = c(0), col = "brown", lty = 2)+
  geom_hline(yintercept = c(-0.1,0.1), col = "burlywood4", lty = 2)+
  annotate(geom="text", x=2016, y=0.15, label="+10%", color="burlywood4")+
  annotate(geom="text", x=2016, y=-0.15, label="-10%", color="burlywood4")+
  labs(title = "Desvio da média de produtividade por safra",
       x = "Safra",
       y = "Desvio Percentua em Relação a Média da Produtividade")+
  theme_classic()

df_safra_cort |> 
  filter(corte==2) |>
  group_by(corte) |>
summarise(produtividade=mean(produtividade)) 





##### Análise do efeito de ciclo #######

### Ideia é utilizar o mesmo ano e ver a diferença entre os ciclos

### Cana de ano e cana de ano e meio

freq(df_ano$estagio)

df_ano_ciclo<-df_ano %>%
  filter(estagio %in% c("1A", "1I", "1M", "2", '3', "4", "5", "6", "7", "8"))
freq(df_ano_ciclo$estagio)


df_safra_estagi<-df_ano_ciclo  %>%
  group_by(safra, estagio) %>%
  summarise(m_unidade=moda(unidade),
            m_tipo=moda(tipo),
            m_cod=moda(cod),
            sum_prod = sum(producao_real, na.rm = TRUE),
            sum_area = sum(area, na.rm = TRUE),
            av_tch_real = mean(tch_real, na.rm = TRUE),
            av_corte = mean(corte, na.rm = TRUE),
            produtividade = sum(producao_real)/sum(area),
            sd_produt=sd(producao_real/area),
            m_variedade=moda(variedade),
            av_atr = mean(atr, na.rm = TRUE),
            sum_atrxprod = sum(atrxproducao, na.rm = TRUE),
            av_mes_colheita = mean(mes_colheita),
            m_mes_colheita = mean(mes_colheita),
            av_t_ha = mean(t_ha, na.rm = TRUE),
            av_kg_atr = mean(kg_atr, na.rm = TRUE),
            av_vatr_safra = mean(vatr_safra, na.rm = TRUE),
            desv_media=(produtividade-69.8)/produtividade # média para 3 corte
  )

graf_df_safra_estagi <- df_safra_estagi |> 
  filter(estagio %in% c("1I", "1M", "2", '3', "4", "5"))

ggplot(graf_df_safra_estagi,aes(x=safra, y=produtividade, fill=estagio, colour=estagio)) +
    geom_bar(stat="identity", position="dodge", colour="skyblue4", alpha=0.7) +
    labs(title = "Produtividade por safra e por corte (t/ha)", x = "Corte", y = "Média da Produtividade (t/ha)") +
   geom_hline(yintercept = c(50, 100), col = "burlywood4", lty = 2)+
    theme_classic()+ scale_fill_brewer(palette="Blues")

graf_df_safra_estagi <- df_safra_estagi |> 
  filter(estagio %in% c("2"))
mean(graf_df_safra_estagi$produtividade)
