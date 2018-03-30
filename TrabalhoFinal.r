#################################################
## INF-0612 - Trabalho Final
## Nomes: Lucas Heredia
##        Thiago Gomes Marçal Pereira
#################################################


## Load das bibliotecas necessárias
library(ggplot2)

## Leitura da planilha
names <- c("Horario", "Temperatura", "Vento", "Umidade", "Sensacao")
cepagri <- read.csv("https://www.ic.unicamp.br/~zanoni/cepagri/cepagri.csv", header = FALSE, sep = ";", col.names = names)

## Acertar o formato das datas
cepagri$Horario <- strptime(cepagri[,1], "%d/%m/%Y-%H:%M")

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
      result[i] <- TRUE
  for (i in 1:(n-k))
    if (all(vector [(i+1):(i+k)] == vector[i]))
      result[i] <- TRUE
  return(result)
}
filtro <- consecutive(cepagri$Temperatura , 144)
duplicates <- unique(as.Date(cepagri[filtro , 1]))

## - Remover dias com dados consecutivos
cepagri <- cepagri[!is.element(as.Date(cepagri$Horario), duplicates), ]
