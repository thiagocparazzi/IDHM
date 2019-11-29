#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("factoextra")
#install.packages("animation")
#install.packages("rgl")
#install.packages("gridExtra")
#install.packages("gganimate")

library(tidyverse)
library(ggplot2)
library(factoextra)
library(animation)
library(rgl)
library(gridExtra)
library(gganimate)

#lendo arquivo
idhm = read.csv('D:/Dados/Downloads/idhmBrasil.csv', header = TRUE, fileEncoding = 'UTF-8')

#adicionado nova coluna e valores conectados com a coluna UF (Unidade Federativa)
idhm['REGIÃO'] <- NA

idhm$REGIÃO <- ifelse(idhm$UF == 'ACRE' | idhm$UF == 'AMAZONAS' | idhm$UF =='RONDÔNIA' | idhm$UF == 'RORAIMA' | idhm$UF == 'TOCANTINS' | idhm$UF == 'PARÁ' | idhm$UF == 'AMAPÁ', 'NORTE',
                      ifelse(idhm$UF == 'ALAGOAS' | idhm$UF == 'CEARÁ' | idhm$UF == 'BAHIA' | idhm$UF == 'MARANHÃO' | idhm$UF == 'PARAÍBA' | idhm$UF == 'PIAUÍ' | idhm$UF == 'PERNAMBUCO' | idhm$UF == 'RIO GRANDE DO NORTE' | idhm$UF == 'SERGIPE', 'NORDESTE',
                             ifelse(idhm$UF == 'MATO GROSSO' | idhm$UF == 'MATO GROSSO DO SUL' | idhm$UF == 'GOIÁS' | idhm$UF == 'DISTRITO FEDERAL', 'CENTRO OESTE',
                                    ifelse(idhm$UF == 'MINAS GERAIS' | idhm$UF == 'RIO DE JANEIRO' | idhm$UF == 'SÃO PAULO' | idhm$UF == 'ESPÍRITO SANTO', 'SUDESTE',
                                           ifelse(idhm$UF == 'RIO GRANDE DO SUL' | idhm$UF == 'SANTA CATARINA' | idhm$UF == 'PARANÁ', 'SUL',
                                                  NA)))))


#VISUALIZAÇÃO DOS DADOS#

#Gráficos gerais - sem omitir os dados NA
p <- ggplot(idhm, aes(ESPVIDA, IDHM, color = REGIÃO)) + #plotar esse quando estiver geral com os tres anos
  geom_point() + facet_grid(.~ANO) +
  ggtitle('IDHM Geral por Esperança de Vida') +
  xlab('Esperança de Vida') +
  ylab('IDHM Geral') 

p + facet_wrap(~REGIÃO) +
  transition_time(ANO) +
  labs(title = "ANO: {frame_time} - IDHM Geral")

plot(p)

ggplot(idhm, aes(ESPVIDA, IDHM_E, color = REGIÃO)) + #plotar esse quando estiver geral com os tres anos
  geom_point() + facet_grid(.~ANO) +
  ggtitle('IDHM Educação por Esperança de Vida') +
  xlab('Esperança de Vida') +
  ylab('IDHM Educação')

ggplot(idhm, aes(ESPVIDA, IDHM_L, color = REGIÃO)) + #plotar esse quando estiver geral com os tres anos
  geom_point() + facet_grid(.~ANO) +
  ggtitle('IDHM Longevidade por Esperança de Vida') +
  xlab('Esperança de Vida') +
  ylab('IDHM Longevidade')

ggplot(idhm, aes(ESPVIDA, IDHM_R, color = REGIÃO)) + #plotar esse quando estiver geral com os tres anos
  geom_point() + facet_grid(.~ANO) +
  ggtitle('IDHM Renda por Esperança de Vida') +
  xlab('Esperança de Vida') +
  ylab('IDHM Renda')

#Verificando médias de regiões e nacional
idhm91 <- filter(idhm, ANO%in%c('1991'))
idhm91 <- mean(idhm91[["IDHM"]])
N <- filter(idhm, REGIÃO%in%c('NORTE'), ANO%in%c('1991'))
N <- mean(N[["IDHM"]])
ND <- filter(idhm, REGIÃO%in%c('NORDESTE'), ANO%in%c('1991'))
ND <- mean(ND[["IDHM"]])
CO <- filter(idhm, REGIÃO%in%c('CENTRO OESTE'), ANO%in%c('1991'))
CO <- mean(CO[["IDHM"]])
SD <- filter(idhm, REGIÃO%in%c('SUDESTE'), ANO%in%c('1991'))
SD <- mean(SD[["IDHM"]])
S <- filter(idhm, REGIÃO%in%c('SUL'), ANO%in%c('1991'))
S <- mean(S[["IDHM"]])

avg91 <- c(N, ND, CO, SD, S, idhm91)

barplot(avg91, main = "Média do IDHM no Brasil em 1991",
        names.arg = c("Norte", "Nordeste", "C. Oeste", "Sudeste", "Sul", "Brasil"),
        col = "lightgreen")

idhm00 <- filter(idhm, ANO%in%c('2000'))
idhm00 <- mean(idhm00[["IDHM"]])
N <- filter(idhm, REGIÃO%in%c('NORTE'), ANO%in%c('2000'))
N <- mean(N[["IDHM"]])
ND <- filter(idhm, REGIÃO%in%c('NORDESTE'), ANO%in%c('2000'))
ND <- mean(ND[["IDHM"]])
CO <- filter(idhm, REGIÃO%in%c('CENTRO OESTE'), ANO%in%c('2000'))
CO <- mean(CO[["IDHM"]])
SD <- filter(idhm, REGIÃO%in%c('SUDESTE'), ANO%in%c('2000'))
SD <- mean(SD[["IDHM"]])
S <- filter(idhm, REGIÃO%in%c('SUL'), ANO%in%c('2000'))
S <- mean(S[["IDHM"]])

avg00 <- c(N, ND, CO, SD, S, idhm00)

barplot(avg00, main = "Média do IDHM no Brasil em 2000",
        names.arg = c("Norte", "Nordeste", "C. Oeste", "Sudeste", "Sul", "Brasil"),
        col = "lightgreen")

idhm10 <- filter(idhm, ANO%in%c('2010'))
idhm10 <- mean(idhm10[["IDHM"]])
N <- filter(idhm, REGIÃO%in%c('NORTE'), ANO%in%c('2010'))
N <- mean(N[["IDHM"]])
ND <- filter(idhm, REGIÃO%in%c('NORDESTE'), ANO%in%c('2010'))
ND <- mean(ND[["IDHM"]])
CO <- filter(idhm, REGIÃO%in%c('CENTRO OESTE'), ANO%in%c('2010'))
CO <- mean(CO[["IDHM"]])
SD <- filter(idhm, REGIÃO%in%c('SUDESTE'), ANO%in%c('2010'))
SD <- mean(SD[["IDHM"]])
S <- filter(idhm, REGIÃO%in%c('SUL'), ANO%in%c('2010'))
S <- mean(S[["IDHM"]])

avg10 <- c(N, ND, CO, SD, S, idhm10)

barplot(avg10, main = "Média do IDHM no Brasil em 2010",
        names.arg = c("Norte", "Nordeste", "C. Oeste", "Sudeste", "Sul", "Brasil"),
        col = "lightgreen")

idhm = na.omit(idhm)

#Gráficos gerais
#Expectativa de Vida XX Fecundidade Total
ggplot(idhm, aes(ESPVIDA, FECTOT, color = REGIÃO)) +
  geom_point() + facet_grid(.~ANO) + scale_color_brewer(palette="Set1")

ggplot(idhm, aes(ESPVIDA, FECTOT, color = REGIÃO)) +
  geom_point() + facet_wrap(.~REGIÃO) + scale_color_brewer(palette="Set1")

ggplot(idhm, aes(ESPVIDA, FECTOT, color = REGIÃO)) +
  geom_point() + facet_wrap(.~UF) + scale_color_brewer(palette="Set1")

#Taxa de Envelhecimento XX Fecundidade Total
ggplot(idhm, aes(T_ENV, FECTOT, color = REGIÃO)) +
  geom_point() + facet_grid(.~ANO) + scale_color_brewer(palette="Set1")

ggplot(idhm, aes(T_ENV, FECTOT, color = REGIÃO)) +
  geom_point() + facet_wrap(.~REGIÃO) + scale_color_brewer(palette="Set1")

ggplot(idhm, aes(T_ENV, FECTOT, color = REGIÃO)) +
  geom_point() + facet_wrap(.~UF) + scale_color_brewer(palette="Set1")

#Expectativa de Vida XX Expectativa Anos de Estudo
ggplot(idhm, aes(ESPVIDA, E_ANOSESTUDO, color = REGIÃO)) +
  geom_point() + facet_grid(.~ANO) + scale_color_brewer(palette="Set1")

ggplot(idhm, aes(ESPVIDA, E_ANOSESTUDO, color = REGIÃO)) +
  geom_point() + facet_wrap(.~REGIÃO) + scale_color_brewer(palette="Set1")

ggplot(idhm, aes(ESPVIDA, E_ANOSESTUDO, color = REGIÃO)) +
  geom_point() + facet_wrap(.~UF) + scale_color_brewer(palette="Set1")

#Expectativa Anos de Estudo XX IDHM de Educação
ggplot(idhm, aes(IDHM_E, E_ANOSESTUDO, color = REGIÃO)) +
  geom_point() + facet_grid(.~ANO) + scale_color_brewer(palette="Set1")

ggplot(idhm, aes(IDHM_E, E_ANOSESTUDO, color = REGIÃO)) +
  geom_point() + facet_wrap(.~REGIÃO) + scale_color_brewer(palette="Set1")

ggplot(idhm, aes(IDHM_E, E_ANOSESTUDO, color = REGIÃO)) +
  geom_point() + facet_wrap(.~UF) + scale_color_brewer(palette="Set1")

#Índice de Escolaridade XX IDHM de Educação
ggplot(idhm, aes(IDHM_E, I_ESCOLARIDADE, color = REGIÃO)) +
  geom_point() + facet_grid(.~ANO) + scale_color_brewer(palette="Set1")

ggplot(idhm, aes(IDHM_E, I_ESCOLARIDADE, color = REGIÃO)) +
  geom_point() + facet_wrap(.~REGIÃO) + scale_color_brewer(palette="Set1")

ggplot(idhm, aes(IDHM_E, I_ESCOLARIDADE, color = REGIÃO)) +
  geom_point() + facet_wrap(.~UF) + scale_color_brewer(palette="Set1")

#Renda per capita máxima do Primeiro quinto mais pobre XX Quem vive com menos de 70 reais
ggplot(idhm, aes(CORTE1, PIND, color = REGIÃO)) +
  geom_point() + facet_grid(.~ANO) + scale_color_brewer(palette="Set1")

ggplot(idhm, aes(CORTE1, PIND, color = REGIÃO)) +
  geom_point() + facet_wrap(.~REGIÃO) + scale_color_brewer(palette="Set1")

ggplot(idhm, aes(CORTE1, PIND, color = REGIÃO)) +
  geom_point() + facet_wrap(.~UF) + scale_color_brewer(palette="Set1")

#Renda per capita máxima do Segundo quinto mais pobre XX Quem vive com menos de 70 reais
ggplot(idhm, aes(CORTE2, PIND, color = REGIÃO)) +
  geom_point() + facet_grid(.~ANO) + scale_color_brewer(palette="Set1")

ggplot(idhm, aes(CORTE2, PIND, color = REGIÃO)) +
  geom_point() + facet_wrap(.~REGIÃO) + scale_color_brewer(palette="Set1")

ggplot(idhm, aes(CORTE2, PIND, color = REGIÃO)) +
  geom_point() + facet_wrap(.~UF) + scale_color_brewer(palette="Set1")

#Renda per capita máxima do Terceiro quinto mais pobre XX Quem vive com menos de 70 reais
ggplot(idhm, aes(CORTE3, PIND, color = REGIÃO)) +
  geom_point() + facet_grid(.~ANO) + scale_color_brewer(palette="Set1")

ggplot(idhm, aes(CORTE3, PIND, color = REGIÃO)) +
  geom_point() + facet_wrap(.~REGIÃO) + scale_color_brewer(palette="Set1")

ggplot(idhm, aes(CORTE3, PIND, color = REGIÃO)) +
  geom_point() + facet_wrap(.~UF) + scale_color_brewer(palette="Set1")

#Renda per capita máxima do Quarto quinto mais pobre XX Quem vive com menos de 70 reais
ggplot(idhm, aes(CORTE4, PIND, color = REGIÃO)) +
  geom_point() + facet_grid(.~ANO) + scale_color_brewer(palette="Set1")

ggplot(idhm, aes(CORTE4, PIND, color = REGIÃO)) +
  geom_point() + facet_wrap(.~REGIÃO) + scale_color_brewer(palette="Set1")

ggplot(idhm, aes(CORTE4, PIND, color = REGIÃO)) +
  geom_point() + facet_wrap(.~UF) + scale_color_brewer(palette="Set1")

#Índice de desigualdade XX IDHM
ggplot(idhm, aes(GINI, IDHM, color = REGIÃO)) +
  geom_point() + facet_grid(.~ANO) + scale_color_brewer(palette="Set1")

ggplot(idhm, aes(GINI, IDHM, color = REGIÃO)) +
  geom_point() + facet_wrap(.~UF) + scale_color_brewer(palette="Set1")

####################

#CLUSTERIZAÇÃO#

#padronização dos dados numéricos
idhm.scaled <- scale(idhm[, -c(1:8, 241)])

#verificando quantidade ideal de clusters - Elbow Method
fviz_nbclust(idhm.scaled, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

fviz_nbclust(idhm.scaled, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

#plots de comparação
k2 <- kmeans(idhm.scaled, centers = 2)
k3 <- kmeans(idhm.scaled, centers = 3)
k4 <- kmeans(idhm.scaled, centers = 4)

p1 <- fviz_cluster(k2, geom = "point", data = idhm.scaled) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = idhm.scaled) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = idhm.scaled) + ggtitle("k = 4")

grid.arrange(p1, p2, p3, nrow = 2)

#visualização dos clusters
kmeans.ani(idhm.scaled, 2)
cluster <- kmeans(idhm.scaled, 2)
idhm_data <- idhm
idhm_data$cluster <- as.factor(cluster$cluster)

fviz_cluster(cluster, data = idhm.scaled)
plot3d(idhm.scaled, col=idhm_data$cluster, main="k-means clusters")
heatmap(idhm.scaled)

#separando por clusters
###CLUSTER 1
cluster1 <- filter(idhm_data, cluster == 1)

h <- hist(cluster1$ESPVIDA, col = "green", xlab = "Esperança de Vida", ylab = "Frequência", main = "Cluster 1 - Esperança de Vida")
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

h <- hist(cluster1$FECTOT, col = "green", xlab = "Fecundidade Total", ylab = "Frequência", main = "Cluster 1 - Fecundidade Total")
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

h <- hist(cluster1$FECTOT, col = "green", xlab = "Mortalidade Infantil", ylab = "Frequência", main = "Cluster 1 - Mortalidade Infantil")
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

###CLUSTER 2
cluster2 <- filter(idhm_data, cluster == 2)

h <- hist(cluster2$ESPVIDA, col = "red", xlab = "Esperança de Vida", ylab = "Frequência", main = "Cluster 2 - Esperança de Vida")
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

h <- hist(cluster2$FECTOT, col = "red", xlab = "Fecundidade Total", ylab = "Frequência", main = "Cluster 2 - Fecundidade Total")
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

h <- hist(cluster2$FECTOT, col = "red", xlab = "Mortalidade Infantil", ylab = "Frequência", main = "Cluster 2 - Mortalidade Infantil")
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))
