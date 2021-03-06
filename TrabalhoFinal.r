#################################################
## INF-0612 - Trabalho Final
## Nomes: Lucas Heredia
##        Thiago Gomes Marçal Pereira
#################################################


## Load das bibliotecas necessárias
library(ggplot2)
library(gtable)
library(grid)

## Leitura da planilha
names <- c("Horario", "Temperatura", "Vento", "Umidade", "Sensacao")
cepagri <- read.csv("https://www.ic.unicamp.br/~zanoni/cepagri/cepagri.csv", header = FALSE, sep = ";", col.names = names)

## Acertar o formato das datas
cepagri$Horario <- strptime(cepagri[,1], "%d/%m/%Y-%H:%M")
cepagri$Dia <- cepagri$Horario$mday
cepagri$Mes <- cepagri$Horario$mon + 1
cepagri$Ano <- cepagri$Horario$year + 1900

## Filtrar as datas desejadas de acordo com os requisitos
## intervalo de 01/01/2015 a 31/12/2017
cepagri <- cepagri[cepagri$Horario >= "2015-01-01" & cepagri$Horario < "2018-01-01",]


## Tratar os dados
## - Remover dados NA
cepagri <- cepagri [!is.na(cepagri[ , 5]), ]
cepagri[ ,2] <- as.character(cepagri[ ,2])
cepagri[ ,2] <- as.numeric(cepagri[ ,2])

## - Outliers
cepagri <- cepagri[cepagri$Sensacao != 99.9, ]

## - Dados Consecutivos
consecutive <- function(vector , k = 1) {
  n <- length(vector)
  result <- logical(n)
  for (i in (1+k):n)
    if (all(vector [(i-k):(i-1)] == vector[i]))
      result[(i-k):(i-1)] <- TRUE
  for (i in 1:(n-k))
    if (all(vector [(i+1):(i+k)] == vector[i]))
      result[(i+1):(i+k)] <- TRUE
  return(result)
}
filtro <- consecutive(cepagri$Temperatura , 144)
duplicates <- unique(as.Date(cepagri[filtro , 1]))

## - Remover dias com dados consecutivos por não serem confiáveis
cepagri <- cepagri[!is.element(as.Date(cepagri$Horario), duplicates), ]

## Plotar duas curvas no mesmo gráfico, buscando relação entre temperatura e umidade
## como forma de tentativa de identificação de chuvas
## Método de utilização a partir de: http://rpubs.com/kohske/dual_axis_in_ggplot2
plot2yaxis <- function(dataframe) {
  grid.newpage()
  
  # two plots
  graficoChuva <- ggplot(dataframe, aes(x=Horario))
  graficoChuva <- graficoChuva + geom_line(aes(y=Umidade), colour="blue")+ theme_bw()
  graficoChuva2 <- ggplot(dataframe, aes(x=Horario))
  graficoChuva2 <- graficoChuva2 + geom_line(aes(y=Temperatura), colour="red")+ theme_bw() %+replace% 
    theme(panel.background = element_rect(fill = NA)) + xlab("Indice")
  
  # extract gtable
  g1 <- ggplot_gtable(ggplot_build(graficoChuva))
  g2 <- ggplot_gtable(ggplot_build(graficoChuva2))
  
  # overlap the panel of 2nd plot on that of 1st plot
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                       pp$l, pp$b, pp$l)
  
  # axis tweaks
  ia <- which(g2$layout$name == "axis-l")
  ga <- g2$grobs[[ia]]
  ax <- ga$children[[2]]
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
  
  # draw it
  grid.draw(g)
}

## Gráfico de Umidade e Temperatura no Mes de Março (Mês das chuvas?), partindo da 
## suposição que com o aumento da Umidade ao ponto de provocar chuva, a Temperatura 
## diminuiria
cepagriMarco2015 <- cepagri$Horario > "2015-03-01" & cepagri$Horario < "2015-04-01"
cepagriMarco2015 <- cepagri[cepagriMarco2015, ]

cepagriMarco2016 <- cepagri$Horario > "2016-03-01" & cepagri$Horario < "2016-04-01"
cepagriMarco2016 <- cepagri[cepagriMarco2016, ]

cepagriMarco2017 <- cepagri$Horario > "2017-03-01" & cepagri$Horario < "2017-04-01"
cepagriMarco2017 <- cepagri[cepagriMarco2017, ]


plot2yaxis(cepagriMarco2015)
plot2yaxis(cepagriMarco2016)
plot2yaxis(cepagriMarco2017)


## Qual o maior número de dias consecutivos com temperaturas máximas acima de 30oC?
maximas <- function() {
  dias <- unique(as.Date(cepagri$Horario))
  maxTemps <- vector(length = length(dias))
  for (i in 1:length(dias)) {
    maxTemps[i] <- max(cepagri$Temperatura[as.Date(cepagri$Horario) == dias[i]])
  }
  df <- data.frame(dias, maxTemps)
  
  df
}

max <- maximas()

consecutiveHigh <- function(df) {
  maxCount <- 0
  currentCount <- 0
  
  for (i in 1:length(df[,1])) {
    if (df$maxTemps[i] >= 30) {
      if (i > 1 && df$dias[i] == (df$dias[i-1] + 1)) {
        currentCount <- currentCount + 1
      } else {
        currentCount <- 1
      }
      if (currentCount > maxCount) {
        maxCount <- currentCount
      }
    } else {
      currentCount <- 0
    }
  }
  maxCount
}

maxCount <- consecutiveHigh(max)

## Qual o maior número de dias consecutivos com temperaturas mínimas abaixo de 10oC?
minimas <- function() {
  dias <- unique(as.Date(cepagri$Horario))
  minTemps <- vector(length = length(dias))
  for (i in 1:length(dias)) {
    minTemps[i] <- min(cepagri$Temperatura[as.Date(cepagri$Horario) == dias[i]])
  }
  df <- data.frame(dias, minTemps)
  
  df
}

min <- minimas()

consecutiveLow <- function(df) {
  minCount <- 0
  currentCount <- 0
  
  for (i in 1:length(df[,1])) {
    if (df$minTemps[i] <=10) {
      if (i > 1 && df$dias[i] == (df$dias[i-1] + 1)) {
        currentCount <- currentCount + 1
      } else {
        currentCount <- 1
      }
      if (currentCount > minCount) {
        minCount <- currentCount
      }
    } else {
      currentCount <- 0
    }
  }
  minCount
}

minCount <- consecutiveLow(min)


## Qual o número de dias por mês com umidade mínima abaixo de 30%?
## Tabela 1
diasUmidosNoMes <- function(acima = 0, abaixo = 100) {
  umidadeMinimaDiaria <- aggregate(cepagri[,"Umidade"], list(cepagri$Dia, cepagri$Mes, cepagri$Ano), min)
  colnames(umidadeMinimaDiaria) <- c("Dia", "Mes", "Ano", "minUmidade")
  umidade <- umidadeMinimaDiaria[umidadeMinimaDiaria$minUmidade > acima & umidadeMinimaDiaria$minUmidade < abaixo,]
  meses <- c(1:12)
  anos <- c(2015:2017)
  tabela1 <- matrix(nrow=length(meses), ncol=length(anos), dimnames = list(month.name, anos))
  for (i in 1:length(anos)) {
    for (j in meses) {
      diasDoMes <- umidade[umidade$Ano == anos[i] & umidade$Mes == j,]$Dia
      tabela1[j,i] <- length(diasDoMes)
    }
  }
  return(tabela1)
}
umidade30 <- diasUmidosNoMes(abaixo = 30)

diasUmidade30 <- sum(umidade30[-1,])

## Informações sobre os níveis críticos de umidade relativa do ar:
# https://orion.cpa.unicamp.br/artigos-especiais/umidade-do-ar-saude-no-inverno.html
# Estado de atencao - entre 20% e 30%
estadoAtencao <- diasUmidosNoMes(acima = 20, abaixo = 30)
diasEstadoAtencao <- sum(estadoAtencao[-1,])
# Estado de Alerta - entre 12% e 20%
estadoAlerta <- diasUmidosNoMes(acima = 12, abaixo = 20)
diasEstadoAlerta <- sum(estadoAlerta[-1,])
# Estado de Emergencia - abaixo de 12%
estadoEmergencia <- diasUmidosNoMes(abaixo = 12)
diasEstadoEmergencia <- sum(estadoEmergencia[-1,])

## Quão frequentes são os ventos fortes em Campinas, de acordo com a Escala de Beaufort?
# Escala de Beaufort: https://pt.wikipedia.org/wiki/Escala_de_Beaufort
diasDeVentoNoMes <- function(acima = 0, abaixo = 200) {
  ventosMaxDiario <- aggregate(cepagri[,"Vento"], list(cepagri$Dia, cepagri$Mes, cepagri$Ano), max)
  colnames(ventosMaxDiario) <- c("Dia", "Mes", "Ano", "maxVento")
  ventos <- ventosMaxDiario[ventosMaxDiario$maxVento > acima & ventosMaxDiario$maxVento < abaixo,]
  meses <- c(1:12)
  anos <- c(2015:2017)
  tabela1 <- matrix(nrow=length(meses), ncol=length(anos), dimnames = list(month.name, anos))
  for (i in 1:length(anos)) {
    for (j in meses) {
      diasDoMes <- ventos[ventos$Ano == anos[i] & ventos$Mes == j,]$Dia
      tabela1[j,i] <- length(diasDoMes)
    }
  }
  return(tabela1)
}
# Valores dos ventos estão em km/h
ventosFortes <- diasDeVentoNoMes(acima = 50, abaixo = 61)


## Qual é maior amplitude térmica mensal?
maxTempMensal <- aggregate(cepagri[,"Temperatura"], list(cepagri$Mes, cepagri$Ano), max)
colnames(maxTempMensal) <- c("Mes", "Ano", "maxTemp")

minTempMensal <- aggregate(cepagri[,"Temperatura"], list(cepagri$Mes, cepagri$Ano), min)
colnames(minTempMensal) <- c("Mes", "Ano", "minTemp")

ampTermMensal <- data.frame(Mes = factor(month.abb[maxTempMensal$Mes], levels = month.abb), Ano = maxTempMensal$Ano, Amplitude = (maxTempMensal$maxTemp - minTempMensal$minTemp))
amplitudeMensal <- ggplot(ampTermMensal, aes(x = Mes, y = Amplitude, group = Ano, color = Ano)) + 
  geom_line() + geom_point() + facet_wrap(~ Ano, nrow = 3, ncol = 1) + theme(legend.position = "none")
amplitudeMensal

## Qual a média das umidades mínimas diárias, mês a mês?
umidadeMinimaDiaria <- aggregate(cepagri[,"Umidade"], list(cepagri$Dia, cepagri$Mes, cepagri$Ano), min)
colnames(umidadeMinimaDiaria) <- c("Dia", "Mes", "Ano", "min")
mediaUmidadeMinima <- aggregate(umidadeMinimaDiaria[,"min"], list(umidadeMinimaDiaria$Mes, umidadeMinimaDiaria$Ano), mean)
colnames(mediaUmidadeMinima) <- c("Mes", "Ano", "Media")
mediaUmidadeMinima$Mes <- factor(month.abb[mediaUmidadeMinima$Mes], levels = month.abb)
gMediaUmidadeMin <- ggplot(mediaUmidadeMinima, aes(x = Mes, y = Media, group = Ano, fill = Media)) + 
  geom_bar(stat = "identity") + facet_wrap(~ Ano, nrow = 3, ncol = 1) + theme(legend.position = "none")
gMediaUmidadeMin

## Qual é a média dos ventos máximos diários, mês a mês?
ventosMaxDiario <- aggregate(cepagri[,"Vento"], list(cepagri$Dia, cepagri$Mes, cepagri$Ano), max)
colnames(ventosMaxDiario) <- c("Dia", "Mes", "Ano", "max")
mediaVentoMax <- aggregate(ventosMaxDiario[,"max"], list(ventosMaxDiario$Mes, ventosMaxDiario$Ano), mean)
colnames(mediaVentoMax) <- c("Mes", "Ano", "Media")
mediaVentoMax$Mes <- factor(month.abb[mediaVentoMax$Mes], levels = month.abb)
gMediaVentoMax <- ggplot(mediaVentoMax, aes(x = Mes, y = Media, group = Ano, fill = Media)) + 
  geom_bar(stat = "identity") + facet_wrap(~ Ano, nrow = 3, ncol = 1) + theme(legend.position = "none")
gMediaVentoMax
