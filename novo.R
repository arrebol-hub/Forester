library(tidyverse)
library(readxl)
library(skimr)
library(ggplot2)
library(writexl)
library(gridExtra)
library(dplyr)
library(ggstatsplot)
library(ggplot2)
library(pwr)
library(vctrs)
library(pastecs)
library(qqplotr)
library(bootstrap)
library(boot)
library(readxl)
library(nortest)
library(fitdistrplus)
library(goftest)
library(openxlsx)
require(lmtest)
library(ggstatsplot)
library(bestNormalize)
library(tidyr)
library(cowplot)
library(sjPlot)
library(lmfor)




dados <- read_excel("dadosInv_MiguelLeopardi_2505.xlsx", sheet = "Planilha1")



dados$parc <- as.factor(dados$parc)
dados$dap <- dados$CAP/pi

dados <- dados %>%
  filter(!parc %in% c(7, 12, 8, 6))

pv <- dados %>%
  group_by(parc) %>%
  summarise(
    n = n(),
    PV25 = {
      cap_ord <- sort(dap)
      k <- floor(length(cap_ord)/4)
      sum(cap_ord[1:k]) / sum(cap_ord)
    },
    PV50 = {
      cap_ord <- sort(dap)
      k <- floor(length(cap_ord)/2)
      sum(cap_ord[1:k]) / sum(cap_ord)
    },
    PV75 = {
      cap_ord <- sort(dap)
      k <- floor(3*length(cap_ord)/4)
      sum(cap_ord[1:k]) / sum(cap_ord)
    }
  )

pv

write_xlsx(pv, "pv_dap.xlsx")


classificar_diametro <- function(dap) {
  if (dap < 15) {
    return("Pequeno")
  } else if (dap >= 15 & dap <= 25) {
    return("Medio")
  } else {if (dap > 25) 
    return("Grande")
  }
}




dados_proc <- dados %>%
  filter(dap > 0, !is.na(dap), ht > 0, !is.na(ht), .preserve = F) %>%
  mutate(dap_quad = dap^2,
         invDAP = 1/dap,
         invDAP_quad = 1/dap^2,
         ln_dap = log10(dap),
         clasdap = sapply(dap, classificar_diametro)) # Adicionar nova coluna de                                                                 classificação ao data frame


a <- ggbetweenstats(
  data = dados_pred,
  x = clasdap,
  y = vol_est,
  type = 'p',
  pairwise.display = ,
  palette = "Set3",   # ou "Okabe-Ito", "Dark2", etc.
  xlab = "Parcela",
  ylab = "DAP",
  pairwise.comparisons = F
)

a

ggplot2::ggsave(
  filename = "inventario_estrato_dap.png",  # nome do arquivo
  plot = a,                               # objeto do gráfico
  width = 8,                              # largura em polegadas
  height = 6,                             # altura em polegadas
  dpi = 300                               # resolução
)




shapiro.test(dados_proc$ht)
shapiro.test(dados_proc$dap)

modelos <- c(
  "naslund",
  "curtis",
  "meyer",
  "michailoff",
  "power",
  "logistic",
  "weibull",
  "richards",
  "gomperz",
  "prodan"
)

resultado <- data.frame()

for (m in modelos) {
  
  cat("Ajustando", m, "\n")
  
  ajuste <- try(
    fithd(
      d = dados$dap,
      h = dados$ht,
      plot = dados$parc,
      modelName = m,
      varf = 2
    ),
    silent = TRUE
  )
  
  if (!inherits(ajuste, "try-error")) {
    
    resultado <- rbind(
      resultado,
      data.frame(
        Modelo = m,
        AIC = AIC(ajuste),
        BIC = BIC(ajuste),
        LogLik = as.numeric(logLik(ajuste))
      )
    )
    
  }
  
}


resultado[order(resultado$AIC), ]


mod1 <- fithd(
  d = dados_proc$dap,
  h = dados_proc$ht,
  plot = dados_proc$parc,
  modelName = "curtis"
)

dev.off()
plot(mod1)

plot_grid(plot_model(mod1, type = "diag"))

z <- plot_grid(plot_model(mod1, type = "diag"))



ggsave(
  filename = "diagnostico_mod1.png",
  plot = z,
  width = 10,
  height = 8,
  dpi = 300
)



dados_proc$hest_mod1 <- predict(mod1)




# Erro absoluto médio (MAE)

mae <- mean(abs(dados_proc$ht - dados_proc$hest_mod1))

# Erro quadrático médio (RMSE)

rmse <- sqrt(mean((dados_proc$ht - dados_proc$hest_mod1)^2))

# Erro percentual absoluto médio (MAPE)

mape <- mean(abs((dados_proc$ht - dados_proc$hest_mod1) / dados_proc$ht)) * 100


syx_perc <- sqrt(mean((dados_proc$ht - dados_proc$hest_mod1)^2)) /
  mean(dados_proc$ht) * 100

mae
rmse
mape
syx_perc


dev.off()
attach(dados_proc)

plot(dados_proc$ht, dados_proc$hest_mod1)

abline(lm(hest_mod1~ht))


dados$ht_final <- predict(mod1, newdata = dados)


##Estimando a Altura e a Porcentagem do erro residual

##dados_proc$hest_mod1 <- ((coef(mod1)[1] + (coef(mod1)[2]* dados_proc$ln_dap)))
syxr_mod1 <- sqrt((sum((dados_proc$ht - dados_proc$hest_mod1)^2))/(length(dados_proc$ht)-2))/mean(dados_proc$ht)*100

syxr_mod1         

##Calculando o Residuo Individual pra cada estimativa de altura

dados_proc$res_mod1 <- (dados_proc$ht - dados_proc$hest_mod1)
dados_proc$res_mod1_perc <- (dados_proc$ht - dados_proc$hest_mod1)/(dados_proc$ht)*100
dados_proc$dmp <- abs ((dados_proc$ht - dados_proc$hest_mod1)/(dados_proc$ht))*100

shapiro.test(dados_proc$res_mod1)


dev.off()  # Reset the graphics device
ggplot(data = dados_proc, aes(x = res_mod1)) +
  geom_density(fill='blue', color ='black') + xlim(-10,10) 

shapiro.test(dados_proc$res_mod1)

h1 <- ggplot(data = dados_proc, aes(x = ht, y = res_mod1_perc, color = parc))+
  geom_point( size = 3)+
  xlim(5, 25)+ ylim(-50, 50) +
  labs (
    y = "Resíduos (%)",
    x = "Altura (m)",)
h1<- h1+ geom_hline(yintercept = 0, lty="longdash")
h1

h2 <-ggplot(data = dados_proc, aes(x = res_mod1, color = parcela)) +
  geom_density(bins=20, fill = 'blue', color ='blue')+
  labs (
    x = expression(paste("Resíduos (m)")),
    y = "Frequência") + xlim(-10,10)
h2

my_plot <- grid.arrange(h2,h1,nrow = 1, ncol = 2)  

shapiro.test(dados_proc$res_mod1)

ggsave("residuos_altura_sq.png",
       plot = my_plot,
       width = 8.5, 
       height = 5)


# CUBAGEM ########################################


dados_c <- read_excel("dadosInv_MiguelLeopardi_2505.xlsx", 
                      sheet = "cubagem")

dados_c


# Funções


smalian <- function(d1, d2, h){
  a1 <- pi * (d1/200)^2
  a2 <- pi * (d2/200)^2
  ((a1 + a2)/2) * h
}

cone <- function(d, h){
  pi * (d/200)^2 * h / 3
}


# Calcular volume de cada seção



dados_c <- dados_c %>%
  group_by(est, parc, arv) %>%
  arrange(sec, .by_group = TRUE) %>%
  mutate(
    
    prox_d   = lead(d),
    prox_sec = lead(sec),
    h_sec    = prox_sec - sec,
    
    vol_sec = case_when(
      
      # Smalian
      !is.na(prox_d) ~ smalian(d, prox_d, h_sec),
      
      # Cone
      is.na(prox_d) & !is.na(d) & !is.na(h_sec) ~ cone(d, h_sec),
      
      TRUE ~ NA_real_
      
    )
    
  ) %>%
  ungroup()


cubagem <- dados_c %>%
  group_by(est, parc, arv) %>%
  summarise(
    
    volcc = sum(vol_sec, na.rm = TRUE),
    
    dap = d[which.min(abs(sec - 1.3))],
    
    ht = max(sec, na.rm = TRUE),
    
    .groups = "drop"
    
  )

cubagem

ggcorrmat(cubagem)

ggscatterstats(cubagem, ht, volcc)

gg


# Equação de volume ########################################

vol_eq <- cubagem %>%
  group_by(arv)


modelos <- list(
  
  SchumacherHall = log(volcc) ~ log(dap) + log(ht),
  
  Husch = log(volcc) ~ log(dap),
  
  Spurr = volcc ~ I(dap^2 * ht),
  
  Stoate = volcc ~ dap + I(dap^2) + I(dap^2 * ht),
  
  Meyer = volcc ~ dap + I(dap^2) + I(dap * ht) + I(dap^2 * ht),
  
  Honer = log(volcc) ~ log(dap^2 * ht)
)


resultado <- data.frame()

ajustes <- list()

for(nome in names(modelos)){
  
  cat("Ajustando:", nome, "\n")
  
  mod <- try(
    lm(modelos[[nome]], data = vol_eq),
    silent = TRUE
  )
  
  if(inherits(mod, "try-error")) next
  
  ajustes[[nome]] <- mod
  
  # Predição
  pred <- predict(mod)
  
  # Verifica se o modelo está em log
  if(grepl("log\\(volcc\\)", deparse(formula(mod))[2])){
    
    # Correção de Meyer
    fc <- exp(summary(mod)$sigma^2 / 2)
    
    pred <- exp(pred) * fc
    
  }
  
  obs <- vol_eq$volcc
  
  rmse <- sqrt(mean((obs - pred)^2))
  
  mae <- mean(abs(obs - pred))
  
  mape <- mean(abs((obs - pred)/obs))*100
  
  bias <- mean(pred - obs)
  
  syx <- sqrt(sum((obs-pred)^2)/(length(obs)-length(coef(mod))))
  
  syx_perc <- syx/mean(obs)*100
  
  resultado <- rbind(
    resultado,
    data.frame(
      
      Modelo = nome,
      
      R2 = cor(obs,pred)^2,
      
      AIC = AIC(mod),
      
      BIC = BIC(mod),
      
      RMSE = rmse,
      
      MAE = mae,
      
      MAPE = mape,
      
      Bias = bias,
      
      Syx = syx,
      
      Syx_perc = syx_perc
      
    )
  )
  
}



resultado <- resultado %>%
  arrange(RMSE)

resultado

melhor <- resultado %>%
  slice(1)

melhor

mod2 <- ajustes[["SchumacherHall"]]
summary(mod2)
formula(mod2)

par(mfrow=c(2,2))
plot(mod2)



#Se o melhor for logarítmico:

fc <- exp(summary(mod2)$sigma^2 / 2)

dados_proc$vol_est <- exp(
  predict(
    mod2,
    newdata = dados_proc
  )
) * fc

# Caso o melhor modelo não seja logarítmico:
  
  dados_proc$vol_est <- predict(
    mod2,
    newdata = dados_proc
  )


summary(dados_proc$vol_est)
  
  
plot_grid(plot_model(mod2, type = "diag"))
  
summary(mod2)
shapiro.test(mod2$residuals)
par(mfrow=c(2,2))
plot(mod2)

p <- plot_grid(plot_model(mod2, type = "diag"))



ggsave(
  filename = "diagnostico_mod2.png",
  plot = p,
  width = 10,
  height = 8,
  dpi = 300
)

formula(mod2)



##TESTES!!!
shapiro_result <- shapiro.test(mod2$residuals) # se o w der acima de 80 e pouco Ã© pq deu normal
dw_result <- dwtest(mod2) # Ã© bom se der prÃ³ximo de 2
bp_result <- bptest(mod2)

resultados_2 <- data.frame(
  Teste = c("Shapiro-Wilk", "Durbin-Watson", "Breusch-Pagan"),
  Estatistica = c(shapiro_result$statistic, dw_result$statistic,
                  bp_result$statistic),
  Valor_p = c(shapiro_result$p.value, dw_result$p.value, bp_result$p.value)
)

print(resultados_2)   


vol_eq$vol_est <- exp(coef(mod2)[1] + coef(mod2)[2]*log(vol_eq$dap) + coef(mod2)[3]*log(vol_eq$ht))
vol_eq$vol_res_mod2 <- (vol_eq$volcc - vol_eq$vol_est)
vol_eq$vol_res_perc <- ((vol_eq$volcc - vol_eq$vol_est)/(vol_eq$volcc))*100
vol_eq$dmp<-abs((vol_eq$volcc - vol_eq$vol_est)/(vol_eq$volcc))*100

vol_eq$vol_res_perc

dev.off()
attach(vol_eq)
plot(volcc, vol_est)
abline(lm(volcc~vol_est))

mean(vol_eq$dmp)


g3 <-ggplot(data = vol_eq, aes(x = vol_res_mod2)) +
  geom_density(bins = 7, fill='blue', color ='magenta', bw = 0.01) +
  labs (
    x = expression(paste("Residuos m"^3)),
    y = "Frequência")  + xlim(-0.07, 0.1) 
g3 

g4 <-ggplot(data = vol_eq, aes(x = dap, y = vol_res_perc))+
  geom_point(colour="red", size =2) +
  geom_point(shape = 1,size = 2,colour = "blue") +
  xlim(5, 25)+ ylim(-100, 100) +
  labs (
    y = "Resíduos (%)",
    x = "DAP (cm)") 
g4 +  geom_hline(yintercept = 0, lty="longdash")


my_plot_vol<-grid.arrange(g3,g4,nrow = 1, ncol = 2)

ggsave(filename = "residuos_volume_sq.png",
       plot = my_plot_vol,
       width = 8.5, 
       height = 5)


########## QUEBRA O PAU AQUI


dados_pred <- dados
dados_pred$d <- dados_pred$dap
dados_pred$h <- dados_pred$ht

dados_pred <- dados_pred %>% 
  dplyr::filter(!is.na(d))



dados_pred$ht <- predict(mod1, newdata = dados_pred, level = 0)

ggplot(dados_pred, aes(x = ht, fill = parc)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Distribuição das alturas estimadas por parcela",
    x = "Altura estimada (ht_est)",
    y = "Densidade"
  ) +
  theme_minimal()

ggsave("densidade_ht_est.png", width = 8, height = 6, dpi = 300)

dados_pred$d <- dados_pred$dap
dados_pred$h <- dados_pred$ht
dados_proc<-dados %>%
  filter(dap > 0, !is.na(dap)) %>%
  mutate(dap_quad = dap^2,
         invDAP = 1/dap,
         invDAP_quad = 1/dap^2,
         ln_dap = log(dap))


dados_pred$vol_est <- exp(predict(mod2, newdata = dados_pred)) * fc


dados_pred$est <- as.factor(dados_pred$est)

ggplot(dados_pred, aes(x = vol_est, fill = parc)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Distribuição do volume estimado por parcela",
    x = "Volume estimado (vol_est)",
    y = "Densidade"
  ) +
  theme_minimal()

ggsave("densidade_vol_est.png", width = 8, height = 6, dpi = 300)


ggplot(dados_pred, aes(x = dap, fill = parc)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Distribuição do DAP por parcela",
    x = "DAP",
    y = "Densidade"
  ) +
  theme_minimal()


ad.test(dados_pred$vol_est)

summary(dados_pred$vol_est)

median_vol_est <- median(dados_pred$vol_est)
median_vol_est

mean_vol_est <- mean(dados_pred$vol_est)
mean_vol_est

print(dados_pred$vol_est)
ad.test(dados_pred$vol_est)

summary(dados_pred$vol_est)


# Amostragem aleatoria

#i. InformaÃ§Ãµes para Ã¡rea total

inv_total <- dados_pred %>%
  filter(dap>0) %>%   ##Ã¡rvores vivas
  group_by(parc) %>%
  summarize(
    narv_parc = n(),
    narv_ha = narv_parc/0.04,
    vol_parc = round(sum(vol_est),3),
    vol_ha = round(vol_parc/0.04,3),
    vol_total = vol_ha*11.58,            #Ã¡rea total da floresta
    mediana_dap = median(dap, na.rm=T),
    mean_dap = mean(dap, na.rm = T),
    sd_dap = sd(dap, na.rm=T),
    cv_dap = (sd_dap/mean_dap)*100,
    max_dap = max(dap, na.rm=T),
    min_dap = min(dap, na.rm =T),
    mediana_ht = median(ht, na.rm=T),
    mean_ht = mean(ht, na.rm = T),
    sd_ht = sd(ht, na.rm=T),
    cv_ht = (sd_ht/mean_ht)*100,
    max_ht = max(ht, na.rm=T),
    min_ht = min(ht, na.rm =T),
    mediana_vol_est = median(vol_est, na.rm=T),
    mean_vol_est = mean(vol_est, na.rm = T),
    sd_vol_est = sd(vol_est, na.rm=T),
    cv_vol_est = (sd_vol_est/mean_vol_est)*100,
    max_vol_est = max(vol_est, na.rm=T),
    min_vol_est = min(vol_est, na.rm =T),
    sum_vol_est = sum(vol_est, na.rm =T),
  )


area_est <- c(4.2, 3.8, 2, 1.35)

df <- 7 # n de parcelas - 1
conf_level <- 1 - 0.05 # 95%
t_critical <- qt(1 - (1 - conf_level) / 2, df)
t_critical


area_total = 8.78
area_parcela = 0.04 # ideal mínimo = 0.04 ou 400 m²
n_parcelas = 8
t_tabelado = t_critical #n-1

N_value = area_total/area_parcela
N_value



analise_t1 <- inv_total %>%
  filter(vol_parc > 0, vol_ha > 0, !is.na(vol_parc), !is.na(vol_ha)) %>%
  summarise(
    media = mean(vol_ha),
    variancia = var(vol_ha),
    median = median(vol_ha),
    desvpad = sqrt(var(vol_ha)))

summary(analise_t1)

desvpad


var(inv_total$vol_total)

sd(inv_total$vol_total)


variancia <- analise_t1$variancia
variancia

media <- analise_t1$media
media

mediana <- analise_t1$median
mediana

dif<- media-mediana
dif

# populacao finita ou infinita?

fc = (1-(n_parcelas/N_value)) #pop. infinita n/N > 0,05 - consigo amostrar mais que 5%?
fc

s2x = analise_t1$variancia/n_parcelas #variancia da media
s2x

sx = sqrt(s2x) #erro/desvio padrao da media
sx

inv_total



#ignorar que nÃ£o sÃ£o normais
#transformaÃ§Ãµes

#erro de amostragem
# ATENCAOOOO @@@@@@$$$$$%%%%%%% DEFINIR ERROS; CONSERTAR O CÓDIGO. AGORA? NAO!

erro_amostragem_t1 <- (sx*t_tabelado)/analise_t1$media*100
erro_amostragem_t1
# 
# #erro amostral Raimundo
#
erro_amostral <- t_tabelado*(analise_t1$desvpad)/sqrt(8)
erro_amostral

#Intervalo de confianÃ§a por hectare 

IC_sup_ha = (analise_t1$media+sx*t_tabelado)
Ic_inf_ha = (analise_t1$media-sx*t_tabelado)
media_ha = (analise_t1$media)
IC_sup_ha
media_ha
Ic_inf_ha

#Intervalo de confianÃ§a total

area_total

IC_sup_est = (analise_t1$media*area_total+sx*t_tabelado*area_total)
Ic_inf_est = (analise_t1$media*area_total-sx*t_tabelado*area_total)
media_est = (analise_t1$media*area_total)
IC_sup_est
media_est
Ic_inf_est 

limites <- data.frame(Ic_inf_est, media_est, IC_sup_est)
limites

diferencateste <- limites$IC_sup_est - limites$media_est

diferencateste

#Intensidade amostral para pop infinita
## precisao desejada ou erro adimissivel (E)

E = 0.10 * analise_t1$media
E

## numero de parcelas para abrir
n_parcelas_desejado = (t_tabelado^2)*(analise_t1$variancia)/(E^2)
n_parcelas_desejado

variancia



library(ggplot2)

# Criando um intervalo de valores para o eixo X conseguir desenhar a curva
# (Ajuste o "from" e "to" se os valores do seu inventário forem muito maiores ou menores)
eixo_x <- data.frame(x = c(Ic_inf_ha - (2 * sx), IC_sup_ha + (2 * sx)))

densidade_prob <- ggplot(eixo_x, aes(x = x)) +
  # 1. Plota a curva Normal com a sua média e o seu desvio padrão (sx)
  stat_function(fun = dnorm, args = list(mean = media_ha, sd = sx), 
                color = "tomato", linewidth = 1.2) +
  
  # 2. Linha contínua na Média e seu respectivo valor
  geom_vline(xintercept = media_ha, color = "blue", 
             linetype = "solid", linewidth = 1) +
  geom_text(aes(x = media_ha, y = Inf, label = paste0("Média: ", round(media_ha, 2))), 
            color = "blue", vjust = 2, hjust = -0.1, fontface = "bold") +
  
  # 3. Linhas PONTILHADAS nos Intervalos de Confiança e seus valores
  geom_vline(xintercept = Ic_inf_ha, color = "orange", 
             linetype = "dotted", linewidth = 1) +
  geom_text(aes(x = Ic_inf_ha, y = Inf, label = paste0("IC Inf: ", round(Ic_inf_ha, 2))), 
            color = "black", vjust = 4, hjust = -0.1) +
  
  geom_vline(xintercept = IC_sup_ha, color = "black", 
             linetype = "dotted", linewidth = 1) +
  geom_text(aes(x = IC_sup_ha, y = Inf, label = paste0("IC Sup: ", round(IC_sup_ha, 2))), 
            color = "black", vjust = 4, hjust = 1.1) +
  
  # Customização de títulos e eixos (Corrigido as cores no subtítulo também!)
  labs(title = "Curva de Densidade Normal do Inventário",
       subtitle = "Linha Azul = Média | Linhas Laranjas Pontilhadas = IC (Inf / Sup)",
       x = "m³/ha",
       y = "Densidade") +
  theme_minimal()

# Para visualizar o gráfico no RStudio:
densidade_prob


ggsave(filename = "grafico_inventario.png", plot = densidade_prob, 
       width = 8, height = 6, dpi = 300)



library(ggplot2)

# Definindo o desvio padrão que você passou
sd_fixo <- 95.12

# 1. Ajustando o intervalo do eixo X com base nas novas variáveis e no novo desvio padrão
eixo_x <- data.frame(x = c(Ic_inf_est - (2 * sd_fixo), IC_sup_est + (2 * sd_fixo)))

# 2. Construindo o gráfico com as variáveis de estimativa (_est)
densidade_prob_est <- ggplot(eixo_x, aes(x = x)) +
  # Plota a curva Normal com a nova média e o desvio padrão de 95.12
  stat_function(fun = dnorm, args = list(mean = media_est, sd = sd_fixo), 
                color = "tomato", linewidth = 1.2) +
  
  # Linha contínua na Média do inventário
  geom_vline(xintercept = media_est, color = "blue", 
             linetype = "solid", linewidth = 1) +
  geom_text(aes(x = media_est, y = Inf, label = paste0("Média: ", round(media_est, 2))), 
            color = "blue", vjust = 2, hjust = -0.1, fontface = "bold") +
  
  # Linhas PONTILHADAS nos novos Intervalos de Confiança
  geom_vline(xintercept = Ic_inf_est, color = "orange", 
             linetype = "dotted", linewidth = 1) +
  geom_text(aes(x = Ic_inf_est, y = Inf, label = paste0("IC Inf: ", round(Ic_inf_est, 2))), 
            color = "black", vjust = 4, hjust = -0.1) +
  
  geom_vline(xintercept = IC_sup_est, color = "black", 
             linetype = "dotted", linewidth = 1) +
  geom_text(aes(x = IC_sup_est, y = Inf, label = paste0("IC Sup: ", round(IC_sup_est, 2))), 
            color = "black", vjust = 4, hjust = 1.1) +
  
  # Customização de títulos e eixos para o total da estimativa
  labs(title = "Curva de Densidade Normal - Estimativa do Inventário",
       subtitle = "Linha Azul = Média | Linhas Laranjas Pontilhadas = IC (Inf / Sup)",
       x = "Volume Total (m³)",
       y = "Densidade") +
  theme_minimal()

# Para visualizar o gráfico na tela:
densidade_prob_est


ggsave(filename = "grafico_inventario_total1.png", plot = densidade_prob_est, 
       width = 8, height = 6, dpi = 300)



boxplot(dados_pred$vol_est,
        main = "Boxplot do DAP",
        ylab = "DAP (cm)",
        col = "lightblue")



# 1. Definir a amplitude da classe (hc)
# Altere para o valor que desejar (ex: 5 cm ou 10 cm)
hc <- 5 

# 2. Criar os limites das classes de forma automática e arredondada
limite_min <- floor(min(dados$dap, na.rm = TRUE))
limite_max <- ceiling(max(dados$dap, na.rm = TRUE)) + hc
quebras <- seq(from = limite_min - (limite_min %% hc), to = limite_max, by = hc)

# 3. Agrupar a coluna 'dap' em classes diamétricas
# right = FALSE garante o intervalo fechado à esquerda [limite_inf, limite_sup)
classes <- cut(dados$dap, breaks = quebras, right = FALSE)

# 4. Criar a tabela de Frequência Absoluta
tabela_freq <- as.data.frame(table(Classes = classes))
colnames(tabela_freq) <- c("Classe Diamétrica (cm)", "Freq. Absoluta (nº árvores)")

# 5. Calcular o Centro de Classe (Xi)
centros <- quebras[-length(quebras)] + (hc / 2)
tabela_freq$`Centro de Classe (Xi)` <- centros[1:nrow(tabela_freq)]

# Organizar a ordem das colunas para exibição
tabela_freq <- tabela_freq[, c("Classe Diamétrica (cm)", "Centro de Classe (Xi)", "Freq. Absoluta (nº árvores)")]

# Exibir a tabela final no console
print(tabela_freq)

png("histograma_dap1.png", width = 8, height = 6, units = "in", res = 300)

# 6. Gerar o gráfico de distribuição diamétrica (Histograma)
hist(dados$dap, breaks = quebras, right = FALSE, 
     main = "Distribuição Diamétrica da Floresta",
     xlab = "Classes de Diâmetro - DAP (cm)", 
     ylim = range(breaks),
     ylab = "Frequência Absoluta (Número de Árvores)", 
     col = "darkgreen", border = "white")

breaks <- as.vector(c(0,800))
dev.off()


write_xlsx(tabela_freq, "tabela_frequencia_dap1.xlsx")

ggsave("histograma_dap.png", plot = f, width = 8, height = 6, dpi = 300)

write_xlsx(analise_t1, "analise_t1.xlsx")


png("grafico_dap_volume.png", width = 8, height = 6, units = "in", res = 300)

ggplot(dados_pred, aes(x = dap, y = vol_est, color = parc)) +
  geom_point(size = 3) +
  labs(
    title = "Relação entre DAP e Volume Estimado por Parcela",
    x = "Diâmetro à Altura do Peito (DAP) (cm)",
    y = "Volume Estimado (m³)",
    color = "Parcela"
  ) + geom_smooth(method = "lm", se = TRUE, color = "black") +
  theme_minimal()
dev.off()




