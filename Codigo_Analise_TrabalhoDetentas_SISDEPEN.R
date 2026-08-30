# Bibliotecas Necessarias
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(stringr)
library(ggpubr)
library(geobr)
library(sf)
###########################################################################################################
#Funcoes
filtra_sisdepen1 <- function(data_presa){
  data_filt <- data_presa %>% select(`Referência`,`Nome do Estabelecimento`,UF,
                                    `1.1 Estabelecimento originalmente destinado a pessoa privadas de liberdade do sexo`,
                                    `4.1 População prisional | Presos provisórios (sem condenação) | Justiça Estadual Feminino`,
                                    `4.1 População prisional | Presos provisórios (sem condenação) | Justiça Federal Feminino`,
                                    `4.1 População prisional | Presos provisórios (sem condenação) | Outros(Just. Trab., cível) Feminino`,
                                    `4.1 População prisional | Presos sentenciados - regime fechado | Justiça Estadual Feminino`,
                                    `4.1 População prisional | Presos sentenciados - regime fechado | Justiça Federal Feminino`,
                                    `4.1 População prisional | Presos sentenciados - regime fechado | Outros(Just. Trab., cível) Feminino`,
                                    `4.1 População prisional | Presos sentenciados - regime semiaberto | Justiça Estadual Feminino`,
                                    `4.1 População prisional | Presos sentenciados - regime semiaberto | Justiça Federal Feminino`,
                                    `4.1 População prisional | Presos sentenciados - regime semiaberto | Outros(Just. Trab., cível) Feminino`,
                                    `4.1 População prisional | Presos sentenciados - regime aberto | Justiça Estadual Feminino`,
                                    `4.1 População prisional | Presos sentenciados - regime aberto | Justiça Federal Feminino`,
                                    `4.1 População prisional | Presos sentenciados - regime aberto | Outros(Just. Trab., cível) Feminino`,
                                    `4.1 População prisional | Medida de segurança - internação | Justiça Estadual Feminino`,
                                    `4.1 População prisional | Medida de segurança - internação | Justiça Federal Feminino`,
                                    `4.1 População prisional | Medida de segurança - internação | Outros(Just. Trab., cível) Feminino`,
                                    `4.1 População prisional | Medida de segurança - tratamento ambulatorial | Justiça Estadual Feminino`,
                                    `4.1 População prisional | Medida de segurança - tratamento ambulatorial | Justiça Federal Feminino`,
                                    `4.1 População prisional | Medida de segurança - tratamento ambulatorial | Outros(Just. Trab., cível) Feminino`,
                                    `4.1 População prisional | Total`,
                                    `5.1 Quantidade de pessoas privadas de liberdade por faixa etária | O estabelecimento tem condições de obter estas informações em seus registros?`,
                                    `5.1 Quantidade de pessoas privadas de liberdade por faixa etária | 18 a 24 anos | Feminino`,
                                    `5.1 Quantidade de pessoas privadas de liberdade por faixa etária | 25 a 29 anos | Feminino`,
                                    `5.1 Quantidade de pessoas privadas de liberdade por faixa etária | 30 a 34 anos | Feminino`,
                                    `5.1 Quantidade de pessoas privadas de liberdade por faixa etária | 35 a 45 anos | Feminino`,
                                    `5.1 Quantidade de pessoas privadas de liberdade por faixa etária | 46 a 60 anos | Feminino`,
                                    `5.1 Quantidade de pessoas privadas de liberdade por faixa etária | 61 a 70 anos | Feminino`,
                                    `5.1 Quantidade de pessoas privadas de liberdade por faixa etária | Mais de 70 anos | Feminino`,
                                    `5.1 Quantidade de pessoas privadas de liberdade por faixa etária | Não informado | Feminino`,
                                    `5.2 Quantidade de pessoas privadas de liberdade por cor de pele/raça/etnia | O estabelecimento tem condições de obter estas informações em seus registros?`,
                                    `5.2 Quantidade de pessoas privadas de liberdade por cor de pele/raça/etnia | Branca | Feminino`,
                                    `5.2 Quantidade de pessoas privadas de liberdade por cor de pele/raça/etnia | Preta | Feminino`,
                                    `5.2 Quantidade de pessoas privadas de liberdade por cor de pele/raça/etnia | Parda | Feminino`,
                                    `5.2 Quantidade de pessoas privadas de liberdade por cor de pele/raça/etnia | Amarela | Feminino`,
                                    `5.2 Quantidade de pessoas privadas de liberdade por cor de pele/raça/etnia | Indígena | Feminino`,
                                    `5.2 Quantidade de pessoas privadas de liberdade por cor de pele/raça/etnia | Não informado | Feminino`,
                                    `5.2 Quantidade de pessoas privadas de liberdade por cor de pele/raça/etnia | Feminino | Total`,
                                    `5.6 Quantidade de pessoas privadas de liberdade por grau de instrução | O estabelecimento tem condições de obter estas informações em seus registros?`,
                                    `5.6 Quantidade de pessoas privadas de liberdade por grau de instrução | Analfabeto | Feminino`,
                                    `5.6 Quantidade de pessoas privadas de liberdade por grau de instrução | Alfabetizado (sem cursos regulares) | Feminino`,
                                    `5.6 Quantidade de pessoas privadas de liberdade por grau de instrução | Ensino Fundamental Incompleto | Feminino`,
                                    `5.6 Quantidade de pessoas privadas de liberdade por grau de instrução | Ensino Fundamental Completo | Feminino`,
                                    `5.6 Quantidade de pessoas privadas de liberdade por grau de instrução | Ensino Médio Incompleto | Feminino`,
                                    `5.6 Quantidade de pessoas privadas de liberdade por grau de instrução | Ensino Médio Completo | Feminino`,
                                    `5.6 Quantidade de pessoas privadas de liberdade por grau de instrução | Ensino Superior Incompleto | Feminino`,
                                    `5.6 Quantidade de pessoas privadas de liberdade por grau de instrução | Ensino Superior Completo | Feminino`,
                                    `5.6 Quantidade de pessoas privadas de liberdade por grau de instrução | Ensino acima de Superior Completo | Feminino`,
                                    `5.6 Quantidade de pessoas privadas de liberdade por grau de instrução | Não Informado | Feminino`,
                                    `5.6 Quantidade de pessoas privadas de liberdade por grau de instrução | Feminino`,
                                    `5.9 Faixa etária dos filhos que estão no estabelecimento (aplicável apenas para estabelecimentos com mulheres) | Total`,
                                    `5.10 Número de filhos/as | O estabelecimento tem condições de obter estas informações em seus registros?`,
                                    `5.10 Número de filhos/as | Sem filhos | Feminino`,
                                    `5.10 Número de filhos/as | Sem informação | Feminino`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Existem pessoas privadas de liberdade neste estabelecimento em atividades laborterápicas?`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Quantidade de pessoas em vagas obtidas por meios próprios e/ou sem intervenção do sistema prisional | Setor primário - rural, agrícola e artesanato | Trabalho externo Feminino`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Quantidade de pessoas em vagas obtidas por meios próprios e/ou sem intervenção do sistema prisional | Setor primário - rural, agrícola e artesanato | Trabalho interno Feminino`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Quantidade de pessoas em vagas obtidas por meios próprios e/ou sem intervenção do sistema prisional | Setor secundário - industrial e contrução civil | Trabalho externo Feminino`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Quantidade de pessoas em vagas obtidas por meios próprios e/ou sem intervenção do sistema prisional | Setor secundário - industrial e contrução civil | Trabalho interno Feminino`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Quantidade de pessoas em vagas obtidas por meios próprios e/ou sem intervenção do sistema prisional | Setor terciário - serviços | Trabalho externo Feminino`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Quantidade de pessoas em vagas obtidas por meios próprios e/ou sem intervenção do sistema prisional | Setor terciário - serviços | Trabalho interno Feminino`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Quantidade de pessoas em vagas disponibilizadas pela administração prisional como apoio ao próprio estabelecimento | Trabalho interno Feminino`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Quantidade de pessoas em vagas disponibilizadas pela administração prisional em parceria com a iniciativa privada | Setor primário - rural, agrícola e artesanato | Trabalho externo Feminino`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Quantidade de pessoas em vagas disponibilizadas pela administração prisional em parceria com a iniciativa privada | Setor primário - rural, agrícola e artesanato | Trabalho interno Feminino`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Quantidade de pessoas em vagas disponibilizadas pela administração prisional em parceria com a iniciativa privada | Setor secundário - industrial e contrução civil | Trabalho externo Feminino`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Quantidade de pessoas em vagas disponibilizadas pela administração prisional em parceria com a iniciativa privada | Setor secundário - industrial e contrução civil | Trabalho interno Feminino`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Quantidade de pessoas em vagas disponibilizadas pela administração prisional em parceria com a iniciativa privada | Setor terciário - serviços | Trabalho externo Feminino`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Quantidade de pessoas em vagas disponibilizadas pela administração prisional em parceria com a iniciativa privada | Setor terciário - serviços | Trabalho interno Feminino`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Quantidade de pessoas em vagas disponibilizadas pela administração prisional em parceria com outros órgãos públicos | Setor primário - rural, agrícola e artesanato | Trabalho externo Feminino`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Quantidade de pessoas em vagas disponibilizadas pela administração prisional em parceria com outros órgãos públicos | Setor primário - rural, agrícola e artesanato | Trabalho interno Feminino`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Quantidade de pessoas em vagas disponibilizadas pela administração prisional em parceria com outros órgãos públicos | Setor secundário - industrial e contrução civil | Trabalho externo Feminino`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Quantidade de pessoas em vagas disponibilizadas pela administração prisional em parceria com outros órgãos públicos | Setor secundário - industrial e contrução civil | Trabalho interno Feminino`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Quantidade de pessoas em vagas disponibilizadas pela administração prisional em parceria com outros órgãos públicos | Setor terciário - serviços | Trabalho externo Feminino`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Quantidade de pessoas em vagas disponibilizadas pela administração prisional em parceria com outros órgãos públicos | Setor terciário - serviços | Trabalho interno Feminino`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Quantidade de pessoas em vagas disponibilizadas pela administração prisional em parceria com entidade ou organizações não governamentais sem fins lucrativos | Setor primário - rural, agrícola e artesanato | Trabalho externo Feminino`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Quantidade de pessoas em vagas disponibilizadas pela administração prisional em parceria com entidade ou organizações não governamentais sem fins lucrativos | Setor primário - rural, agrícola e artesanato | Trabalho interno Feminino`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Quantidade de pessoas em vagas disponibilizadas pela administração prisional em parceria com entidade ou organizações não governamentais sem fins lucrativos | Setor secundário - industrial e contrução civil | Trabalho externo Feminino`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Quantidade de pessoas em vagas disponibilizadas pela administração prisional em parceria com entidade ou organizações não governamentais sem fins lucrativos | Setor secundário - industrial e contrução civil | Trabalho interno Feminino`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Quantidade de pessoas em vagas disponibilizadas pela administração prisional em parceria com entidade ou organizações não governamentais sem fins lucrativos | Setor terciário - serviços | Trabalho externo Feminino`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Quantidade de pessoas em vagas disponibilizadas pela administração prisional em parceria com entidade ou organizações não governamentais sem fins lucrativos | Setor terciário - serviços | Trabalho interno Feminino`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Trabalho externo Feminino`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Trabalho interno Feminino`,
                                    `6.2 Quantidade de pessoas privadas de liberdade por remuneração | O estabelecimento tem condições de obter estas informações em seus registros?`,
                                    `6.2 Quantidade de pessoas privadas de liberdade por remuneração | Não recebe | Feminino`,
                                    `6.2 Quantidade de pessoas privadas de liberdade por remuneração | Menos do que 3/4 do salário mínimo mensal | Feminino`,
                                    `6.2 Quantidade de pessoas privadas de liberdade por remuneração | Entre 3/4 e 1 salário mínimo mensal | Feminino`,
                                    `6.2 Quantidade de pessoas privadas de liberdade por remuneração | Entre 1 e 2 salários mínimos mensais | Feminino`,
                                    `6.2 Quantidade de pessoas privadas de liberdade por remuneração | Mais que 2 salários mínimos mensais | Feminino`,
                                    `6.2 Quantidade de pessoas privadas de liberdade por remuneração | Sem informação | Feminino`)
  return(data_filt)
}

filtra_sisdepen2 <- function(data_presa){
  data_filt <- data_presa %>% select(`Referência`,`Nome do Estabelecimento`,UF,
                                    `1.1 Estabelecimento originalmente destinado a pessoa privadas de liberdade do sexo`,
                                    `4.1 População prisional | Presos provisórios (sem condenação) | Justiça Estadual Feminino`,
                                    `4.1 População prisional | Presos provisórios (sem condenação) | Justiça Federal Feminino`,
                                    `4.1 População prisional | Presos provisórios (sem condenação) | Outros(Just. Trab., cível) Feminino`,
                                    `4.1 População prisional | Presos sentenciados - regime fechado | Justiça Estadual Feminino`,
                                    `4.1 População prisional | Presos sentenciados - regime fechado | Justiça Federal Feminino`,
                                    `4.1 População prisional | Presos sentenciados - regime fechado | Outros(Just. Trab., cível) Feminino`,
                                    `4.1 População prisional | Presos sentenciados - regime semiaberto | Justiça Estadual Feminino`,
                                    `4.1 População prisional | Presos sentenciados - regime semiaberto | Justiça Federal Feminino`,
                                    `4.1 População prisional | Presos sentenciados - regime semiaberto | Outros(Just. Trab., cível) Feminino`,
                                    `4.1 População prisional | Presos sentenciados - regime aberto | Justiça Estadual Feminino`,
                                    `4.1 População prisional | Presos sentenciados - regime aberto | Justiça Federal Feminino`,
                                    `4.1 População prisional | Presos sentenciados - regime aberto | Outros(Just. Trab., cível) Feminino`,
                                    `4.1 População prisional | Medida de segurança - internação | Justiça Estadual Feminino`,
                                    `4.1 População prisional | Medida de segurança - internação | Justiça Federal Feminino`,
                                    `4.1 População prisional | Medida de segurança - internação | Outros(Just. Trab., cível) Feminino`,
                                    `4.1 População prisional | Medida de segurança - tratamento ambulatorial | Justiça Estadual Feminino`,
                                    `4.1 População prisional | Medida de segurança - tratamento ambulatorial | Justiça Federal Feminino`,
                                    `4.1 População prisional | Medida de segurança - tratamento ambulatorial | Outros(Just. Trab., cível) Feminino`,
                                    `4.1 População prisional | Total`,
                                    `5.2 Quantidade de pessoas privadas de liberdade por cor de pele/raça/etnia | O estabelecimento tem condições de obter estas informações em seus registros?`,
                                    `5.2 Quantidade de pessoas privadas de liberdade por cor de pele/raça/etnia | Branca | Feminino`,
                                    `5.2 Quantidade de pessoas privadas de liberdade por cor de pele/raça/etnia | Preta | Feminino`,
                                    `5.2 Quantidade de pessoas privadas de liberdade por cor de pele/raça/etnia | Parda | Feminino`,
                                    `5.2 Quantidade de pessoas privadas de liberdade por cor de pele/raça/etnia | Amarela | Feminino`,
                                    `5.2 Quantidade de pessoas privadas de liberdade por cor de pele/raça/etnia | Indígena | Feminino`,
                                    `5.2 Quantidade de pessoas privadas de liberdade por cor de pele/raça/etnia | Não informado | Feminino`,
                                    `5.2 Quantidade de pessoas privadas de liberdade por cor de pele/raça/etnia | Feminino | Total`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Trabalho externo Feminino`,
                                    `6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Trabalho interno Feminino`,
                                    `6.2 Quantidade de pessoas privadas de liberdade por remuneração | O estabelecimento tem condições de obter estas informações em seus registros?`,
                                    `6.2 Quantidade de pessoas privadas de liberdade por remuneração | Não recebe | Feminino`,
                                    `6.2 Quantidade de pessoas privadas de liberdade por remuneração | Menos do que 3/4 do salário mínimo mensal | Feminino`,
                                    `6.2 Quantidade de pessoas privadas de liberdade por remuneração | Entre 3/4 e 1 salário mínimo mensal | Feminino`,
                                    `6.2 Quantidade de pessoas privadas de liberdade por remuneração | Entre 1 e 2 salários mínimos mensais | Feminino`,
                                    `6.2 Quantidade de pessoas privadas de liberdade por remuneração | Mais que 2 salários mínimos mensais | Feminino`,
                                    `6.2 Quantidade de pessoas privadas de liberdade por remuneração | Sem informação | Feminino`)
  
  return(data_filt)
}

filtra_sisdepen_laborterapia <- function(data_presa){
  buf <- c()
  for (i in 1:length(colnames(data_presa))) {
    if (substr(colnames(data_presa)[i],1,3) == "6.1") {
      buf <- append(buf,colnames(data_presa)[i])
    }else{
      next
    }
  }
  buf <- buf[-1]
  data_filt <- data_presa %>% select(buf)
  return(data_filt)
}

conta_presas <- function(data_presa){
  databuf <- data.frame(matrix(NA,nrow = 20,ncol = 1))
  colnames(databuf) <- c("numero")
  nomes <- c("4.1 População prisional | Presos provisórios (sem condenação) | Justiça Estadual Feminino",
             "4.1 População prisional | Presos provisórios (sem condenação) | Justiça Federal Feminino",
             "4.1 População prisional | Presos provisórios (sem condenação) | Outros(Just. Trab., cível) Feminino",
             "4.1 População prisional | Presos sentenciados - regime fechado | Justiça Estadual Feminino",
             "4.1 População prisional | Presos sentenciados - regime fechado | Justiça Federal Feminino",
             "4.1 População prisional | Presos sentenciados - regime fechado | Outros(Just. Trab., cível) Feminino",
             "4.1 População prisional | Presos sentenciados - regime semiaberto | Justiça Estadual Feminino",
             "4.1 População prisional | Presos sentenciados - regime semiaberto | Justiça Federal Feminino",
             "4.1 População prisional | Presos sentenciados - regime semiaberto | Outros(Just. Trab., cível) Feminino",
             "4.1 População prisional | Presos sentenciados - regime aberto | Justiça Estadual Feminino",
             "4.1 População prisional | Presos sentenciados - regime aberto | Justiça Federal Feminino",
             "4.1 População prisional | Presos sentenciados - regime aberto | Outros(Just. Trab., cível) Feminino",
             "4.1 População prisional | Medida de segurança - internação | Justiça Estadual Feminino",
             "4.1 População prisional | Medida de segurança - internação | Justiça Federal Feminino",
             "4.1 População prisional | Medida de segurança - internação | Outros(Just. Trab., cível) Feminino",
             "4.1 População prisional | Medida de segurança - tratamento ambulatorial | Justiça Estadual Feminino",
             "4.1 População prisional | Medida de segurança - tratamento ambulatorial | Justiça Federal Feminino",
             "4.1 População prisional | Medida de segurança - tratamento ambulatorial | Outros(Just. Trab., cível) Feminino",
             "4.1 População prisional | Total","Total Feminino")
  row.names(databuf) <- nomes
  
  for(i in 1:(length(nomes)-1)){
    databuf[nomes[i],1] <- sum(na.omit(data_presa[,nomes[i]]))
  }
  databuf["Total Feminino",1] <- sum(databuf$numero[1:(length(databuf$numero)-2)])
  
  
  databuf2 <- data.frame(matrix(NA,nrow = 8,ncol = 1))
  colnames(databuf2) <- c("numero")
  nomes2 <- c("Presos provisórios (sem condenação) | Feminino",
              "Presos sentenciados - regime fechado | Feminino",
              "Presos sentenciados - regime semiaberto | Feminino",
              "Presos sentenciados - regime aberto | Feminino",
              "Medida de segurança - internação | Feminino",
              "Medida de segurança - tratamento ambulatorial | Feminino",
              "Total","Total Feminino")
  row.names(databuf2) <- nomes2
  
  databuf2["Presos provisórios (sem condenação) | Feminino",1] <- databuf["4.1 População prisional | Presos provisórios (sem condenação) | Justiça Estadual Feminino",1]+
    databuf["4.1 População prisional | Presos provisórios (sem condenação) | Justiça Federal Feminino",1]+ 
    databuf["4.1 População prisional | Presos provisórios (sem condenação) | Outros(Just. Trab., cível) Feminino",1]
  databuf2["Presos sentenciados - regime fechado | Feminino",1] <- databuf["4.1 População prisional | Presos sentenciados - regime fechado | Justiça Estadual Feminino",1]+databuf["4.1 População prisional | Presos sentenciados - regime fechado | Justiça Federal Feminino",1]+databuf["4.1 População prisional | Presos sentenciados - regime fechado | Outros(Just. Trab., cível) Feminino",1]
  databuf2["Presos sentenciados - regime semiaberto | Feminino",1] <- databuf["4.1 População prisional | Presos sentenciados - regime semiaberto | Justiça Estadual Feminino",1]+databuf["4.1 População prisional | Presos sentenciados - regime semiaberto | Justiça Federal Feminino",1]+databuf["4.1 População prisional | Presos sentenciados - regime semiaberto | Outros(Just. Trab., cível) Feminino",1]
  databuf2["Presos sentenciados - regime aberto | Feminino",1] <- databuf["4.1 População prisional | Presos sentenciados - regime aberto | Justiça Estadual Feminino",1]+databuf["4.1 População prisional | Presos sentenciados - regime aberto | Justiça Federal Feminino",1]+databuf["4.1 População prisional | Presos sentenciados - regime aberto | Outros(Just. Trab., cível) Feminino",1]
  databuf2["Medida de segurança - internação | Feminino",1] <- databuf["4.1 População prisional | Medida de segurança - internação | Justiça Estadual Feminino",1]+databuf["4.1 População prisional | Medida de segurança - internação | Justiça Federal Feminino",1]+databuf["4.1 População prisional | Medida de segurança - internação | Outros(Just. Trab., cível) Feminino",1]
  databuf2["Medida de segurança - tratamento ambulatorial | Feminino",1] <- databuf["4.1 População prisional | Medida de segurança - tratamento ambulatorial | Justiça Estadual Feminino",1]+databuf["4.1 População prisional | Medida de segurança - tratamento ambulatorial | Justiça Federal Feminino",1]+databuf["4.1 População prisional | Medida de segurança - tratamento ambulatorial | Outros(Just. Trab., cível) Feminino",1]
  
  
  databuf2["Total",1] <- databuf["4.1 População prisional | Total",1]
  databuf2["Total Feminino",1] <- sum(databuf2$numero[1:(length(databuf2$numero)-2)])
  
  rm(databuf)
  
  return(databuf2)
}

conta_trabalho <- function(data_presa){
  databuf <- data.frame(matrix(NA,nrow = 3,ncol = 1))
  colnames(databuf) <- c("numero")
  nomes <- c("6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Trabalho externo Feminino",
             "6.1 Quantidade de pessoas privadas de liberdade em programas de laborterapia | Trabalho interno Feminino",
             "Total Feminino")
  row.names(databuf) <- nomes
  
  for(i in 1:(length(nomes)-1)){
    databuf[nomes[i],1] <- sum(na.omit(data_presa[,nomes[i]]))
  }
  databuf["Total Feminino",1] <- sum(databuf$numero[1:(length(databuf$numero)-1)])
  
  row.names(databuf) <- c("Trabalho externo Feminino","Trabalho interno Feminino","Total Feminino")
  return(databuf)
}

conta_remuneradas <- function(data_presa){
  databuf <- data_presa %>% filter(!if_all(c(`4.1 População prisional | Presos provisórios (sem condenação) | Justiça Estadual Feminino`,
                                          `4.1 População prisional | Presos provisórios (sem condenação) | Justiça Federal Feminino`,
                                          `4.1 População prisional | Presos provisórios (sem condenação) | Outros(Just. Trab., cível) Feminino`,
                                          `4.1 População prisional | Presos sentenciados - regime fechado | Justiça Estadual Feminino`,
                                          `4.1 População prisional | Presos sentenciados - regime fechado | Justiça Federal Feminino`,
                                          `4.1 População prisional | Presos sentenciados - regime fechado | Outros(Just. Trab., cível) Feminino`,
                                          `4.1 População prisional | Presos sentenciados - regime semiaberto | Justiça Estadual Feminino`,
                                          `4.1 População prisional | Presos sentenciados - regime semiaberto | Justiça Federal Feminino`,
                                          `4.1 População prisional | Presos sentenciados - regime semiaberto | Outros(Just. Trab., cível) Feminino`,
                                          `4.1 População prisional | Presos sentenciados - regime aberto | Justiça Estadual Feminino`,
                                          `4.1 População prisional | Presos sentenciados - regime aberto | Justiça Federal Feminino`,
                                          `4.1 População prisional | Presos sentenciados - regime aberto | Outros(Just. Trab., cível) Feminino`,
                                          `4.1 População prisional | Medida de segurança - internação | Justiça Estadual Feminino`,
                                          `4.1 População prisional | Medida de segurança - internação | Justiça Federal Feminino`,
                                          `4.1 População prisional | Medida de segurança - internação | Outros(Just. Trab., cível) Feminino`,
                                          `4.1 População prisional | Medida de segurança - tratamento ambulatorial | Justiça Estadual Feminino`,
                                          `4.1 População prisional | Medida de segurança - tratamento ambulatorial | Justiça Federal Feminino`,
                                          `4.1 População prisional | Medida de segurança - tratamento ambulatorial | Outros(Just. Trab., cível) Feminino`), is.na))
  
  databuf2 <- databuf %>% select(`6.2 Quantidade de pessoas privadas de liberdade por remuneração | O estabelecimento tem condições de obter estas informações em seus registros?`,
                                    `6.2 Quantidade de pessoas privadas de liberdade por remuneração | Não recebe | Feminino`,
                                    `6.2 Quantidade de pessoas privadas de liberdade por remuneração | Menos do que 3/4 do salário mínimo mensal | Feminino`,
                                    `6.2 Quantidade de pessoas privadas de liberdade por remuneração | Entre 3/4 e 1 salário mínimo mensal | Feminino`,
                                    `6.2 Quantidade de pessoas privadas de liberdade por remuneração | Entre 1 e 2 salários mínimos mensais | Feminino`,
                                    `6.2 Quantidade de pessoas privadas de liberdade por remuneração | Mais que 2 salários mínimos mensais | Feminino`,
                                    `6.2 Quantidade de pessoas privadas de liberdade por remuneração | Sem informação | Feminino`)
  
  databuf <- data.frame(matrix(NA,nrow = 7,ncol = 1))
  colnames(databuf) <- c("numero")
  nomes <- c("6.2 Quantidade de pessoas privadas de liberdade por remuneração | Não recebe | Feminino",
             "6.2 Quantidade de pessoas privadas de liberdade por remuneração | Menos do que 3/4 do salário mínimo mensal | Feminino",
             "6.2 Quantidade de pessoas privadas de liberdade por remuneração | Entre 3/4 e 1 salário mínimo mensal | Feminino",
             "6.2 Quantidade de pessoas privadas de liberdade por remuneração | Entre 1 e 2 salários mínimos mensais | Feminino",
             "6.2 Quantidade de pessoas privadas de liberdade por remuneração | Mais que 2 salários mínimos mensais | Feminino",
             "6.2 Quantidade de pessoas privadas de liberdade por remuneração | Sem informação | Feminino",
             "Total Feminino")
  row.names(databuf) <- nomes
  
  
  for(i in 1:(length(nomes)-1)){
    databuf[nomes[i],1] <- sum(na.omit(databuf2[,nomes[i]]))
  }
  databuf["Total Feminino",1] <- sum(databuf$numero[1:(length(databuf$numero)-1)])
  
  row.names(databuf) <- c("Não recebe | Feminino","Menos do que 3/4 do salário mínimo mensal | Feminino",
                          "Entre 3/4 e 1 salário mínimo mensal | Feminino","Entre 1 e 2 salários mínimos mensais | Feminino",
                          "Mais que 2 salários mínimos mensais | Feminino","Sem informação | Feminino","Total Feminino")
  return(databuf)
}

info_remuneradas <- function(data_presa){
  databuf <- data_presa %>% filter(!if_all(c(`4.1 População prisional | Presos provisórios (sem condenação) | Justiça Estadual Feminino`,
                                             `4.1 População prisional | Presos provisórios (sem condenação) | Justiça Federal Feminino`,
                                             `4.1 População prisional | Presos provisórios (sem condenação) | Outros(Just. Trab., cível) Feminino`,
                                             `4.1 População prisional | Presos sentenciados - regime fechado | Justiça Estadual Feminino`,
                                             `4.1 População prisional | Presos sentenciados - regime fechado | Justiça Federal Feminino`,
                                             `4.1 População prisional | Presos sentenciados - regime fechado | Outros(Just. Trab., cível) Feminino`,
                                             `4.1 População prisional | Presos sentenciados - regime semiaberto | Justiça Estadual Feminino`,
                                             `4.1 População prisional | Presos sentenciados - regime semiaberto | Justiça Federal Feminino`,
                                             `4.1 População prisional | Presos sentenciados - regime semiaberto | Outros(Just. Trab., cível) Feminino`,
                                             `4.1 População prisional | Presos sentenciados - regime aberto | Justiça Estadual Feminino`,
                                             `4.1 População prisional | Presos sentenciados - regime aberto | Justiça Federal Feminino`,
                                             `4.1 População prisional | Presos sentenciados - regime aberto | Outros(Just. Trab., cível) Feminino`,
                                             `4.1 População prisional | Medida de segurança - internação | Justiça Estadual Feminino`,
                                             `4.1 População prisional | Medida de segurança - internação | Justiça Federal Feminino`,
                                             `4.1 População prisional | Medida de segurança - internação | Outros(Just. Trab., cível) Feminino`,
                                             `4.1 População prisional | Medida de segurança - tratamento ambulatorial | Justiça Estadual Feminino`,
                                             `4.1 População prisional | Medida de segurança - tratamento ambulatorial | Justiça Federal Feminino`,
                                             `4.1 População prisional | Medida de segurança - tratamento ambulatorial | Outros(Just. Trab., cível) Feminino`), is.na))
  
  buf <- databuf$`6.2 Quantidade de pessoas privadas de liberdade por remuneração | O estabelecimento tem condições de obter estas informações em seus registros?`
  #unique(buf)
  buf <- as.data.frame(table(buf))
  colnames(buf) <- c("resposta","Qntd presas por remuneracao | tem condições de obter estas informações?")
  row.names(buf) <- buf$resposta
  buf <- buf %>% select(-resposta)
  
  return(buf)
}
###########################################################################################################
#importar dataset, pode ser atraves do icone ou usando read.csv#
file_names <- c("data/17o-ciclo-base-de-dados-2024-2-semestre.csv","data/16o-ciclo-base-de-dados-2024-1-semestre.csv",
                "data/15o-ciclo-base-de-dados-2023-2-semestre.csv","data/14o-ciclo-base-de-dados-2023-1-semestre.csv",
                "data/13o-ciclo-base-de-dados-2022-2-semestre.csv","data/12o-ciclo-base-de-dados-2022-1-semestre.csv",
                "data/11o-ciclo-base-de-dados-2021-2-semestre.csv","data/10o-ciclo-base-de-dados-2021-1-semestre.csv",
                "data/9o-ciclo-base-de-dados-2020-2-semestre.csv","data/8o-ciclo-base-de-dados-2020-1-semestre.csv",
                "data/7o-ciclo-base-de-dados-2019-2-semestre.csv","data/6o-ciclo-base-de-dados-2019-1-semestre.csv",
                "data/5o-ciclo-base-de-dados-2018-2-semestre.csv","data/4o-ciclo-base-de-dados-2018-1-semestre.csv",
                "data/3o-ciclo-base-de-dados-2017-2-semestre.csv","data/2o-ciclo-base-de-dados-2017-1-semestre.csv",
                "data/1o-ciclo-base-de-dados-2016-2-semestre.csv",
                "data/base-de-dados-infopen-2016.csv","data/base-de-dados-infopen-2015.csv","data/base-de-dados-infopen-2014.csv")
dataframes_names <- c("data_24_2","data_24_1","data_23_2","data_23_1","data_22_2","data_22_1","data_21_2","data_21_1",
                      "data_20_2","data_20_1","data_19_2","data_19_1","data_18_2","data_18_1","data_17_2","data_17_1","data_16_2",
                      "data_infopen16","data_infopen15","data_infopen14")

raw_data <- list()
for(i in 1:(length(file_names)-3)){
  raw_data[[dataframes_names[i]]] <- read_delim(file_names[i], delim = ";")
}

all_data_infopen <- list()
for(i in (length(file_names)-2):length(file_names)){
  all_data_infopen[[dataframes_names[i]]] <- read_delim(file_names[i], delim = ";")
}

###########################################################################################################
#Filtragem de dados SISDEPEN

for(i in 1:length(all_data_sisdepen)){
  all_data_sisdepen[[i]] <- filtra_sisdepen2(raw_data[[i]])
}
###############################################
# Conta população feminina SISDEPEN

populacao_feminino <- data.frame(matrix(NA,nrow = 8,ncol = length(names(all_data_sisdepen))))
colnames(populacao_feminino) <- names(all_data_sisdepen)
nomes2 <- c("Presos provisórios (sem condenação) | Feminino",
            "Presos sentenciados - regime fechado | Feminino",
            "Presos sentenciados - regime semiaberto | Feminino",
            "Presos sentenciados - regime aberto | Feminino",
            "Medida de segurança - internação | Feminino",
            "Medida de segurança - tratamento ambulatorial | Feminino",
            "Total Populacao Geral","Total Populacao Feminino")
row.names(populacao_feminino) <- nomes2

for(i in 1:length(all_data_sisdepen)){
  buf <- conta_presas(all_data_sisdepen[[i]])
  populacao_feminino[dataframes_names[i]] <- buf$numero
}
###############################################
# Conta população feminina que trabalha SISDEPEN

pop_trabalho_fem <- data.frame(matrix(NA,nrow = 3,ncol = length(names(all_data_sisdepen))))
colnames(pop_trabalho_fem) <- names(all_data_sisdepen)
nomes2 <- c("Trabalho externo Feminino","Trabalho interno Feminino","Total Trabalho Feminino")
row.names(pop_trabalho_fem) <- nomes2

for(i in 1:length(all_data_sisdepen)){
  buf <- conta_trabalho(all_data_sisdepen[[i]])
  pop_trabalho_fem[dataframes_names[i]] <- buf$numero
}
###############################################
# Conta população feminina que trabalha SISDEPEN
# 
# A função conta_remuneradas() retorna dois dataframes. O primeiro informa quanto dos estabelecimentos que possuem mulheres sob custódia tem condições de informar a situação de remuneração das trabalhadoras.
# O segundo dataframe informa a faixa de remuneração daquelas que se tem informação.

info_trabalho_remunerado <- data.frame(matrix(NA,nrow = 3,ncol = length(names(all_data_sisdepen))))
colnames(info_trabalho_remunerado) <- names(all_data_sisdepen)
nomes2 <- c("Não","Sim, para parte das pessoas privadas de liberdade","Sim, para todas as pessoas privadas de liberdade")
row.names(info_trabalho_remunerado) <- nomes2

remuneracao_trab_fem <- data.frame(matrix(NA,nrow = 7,ncol = length(names(all_data_sisdepen))))
colnames(remuneracao_trab_fem) <- names(all_data_sisdepen)
nomes2 <- c("Não recebe | Feminino","Menos do que 3/4 do salário mínimo mensal | Feminino",
             "Entre 3/4 e 1 salário mínimo mensal | Feminino","Entre 1 e 2 salários mínimos mensais | Feminino",
             "Mais que 2 salários mínimos mensais | Feminino","Sem informação | Feminino","Total Feminino")
row.names(remuneracao_trab_fem) <- nomes2


for(i in 1:length(all_data_sisdepen)){
  buf <- info_remuneradas(all_data_sisdepen[[i]])
 info_trabalho_remunerado[dataframes_names[i]] <- buf$`Qntd presas por remuneracao | tem condições de obter estas informações?`
  buf <- conta_remuneradas(all_data_sisdepen[[i]])
  remuneracao_trab_fem[dataframes_names[i]] <- buf$numero
}
###############################################
populacao <-bind_rows(populacao_feminino,pop_trabalho_fem)
populacao <- populacao[c("Total Populacao Geral","Total Populacao Feminino","Trabalho externo Feminino","Trabalho interno Feminino","Total Trabalho Feminino"),]

populacao["Pop Fem Relativa",] <- populacao["Total Populacao Feminino",] / populacao["Total Populacao Geral",]
populacao["Pop Fem Trabalho Relativa",] <- populacao["Total Trabalho Feminino",] / populacao["Total Populacao Feminino",]

write.csv(populacao,"Populacao_Fem_trabalho_SISDEPEN.csv")
######################################################################################################
######################################################################################################
#  PLOTAR GRÁFICOS
######################################################################################################
######################################################################################################
plots_theme <- theme_bw()+
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 15,face = "bold"),
        axis.title.x = element_text(size = 15,face = "bold"),
        legend.title = element_text(face = "bold",size = 15),
        legend.text = element_text(size = 15))
######################################################################################################
# População feminina trabalhando
######################################################################################################
data_plot <- as.data.frame(t(populacao <- populacao[c("Total Populacao Feminino","Total Trabalho Feminino","Pop Fem Trabalho Relativa"),]))
semester_level <- c("2024/2","2024/1","2023/2","2023/1","2022/2","2022/1","2021/2","2021/1",
                    "2020/2","2020/1","2019/2","2019/1","2018/2","2018/1","2017/2","2017/1","2016/2")
data_plot$Semestre <- semester_level

semester_level <- rev(semester_level)
data_plot <- data_plot %>% mutate(Semestre = factor(Semestre,levels = semester_level))

dat <- data_plot %>% select(-`Pop Fem Trabalho Relativa`,-`Total Trabalho Feminino`)
colnames(dat) <- c("numero","Semestre")
dat$`População Feminina` <- "Total"

dat2 <- data_plot %>% select(-`Pop Fem Trabalho Relativa`,-`Total Populacao Feminino`)
colnames(dat2) <- c("numero","Semestre")
dat2$`População Feminina` <- "Trabalhando"

dat <- bind_rows(dat,dat2)
rm(dat2)
dat <- dat %>% mutate(Semestre = factor(Semestre,levels = semester_level))
row.names(dat) <- dat$Semestre

g <- ggplot(dat, aes(x= Semestre, y=numero, colour = `População Feminina`, group = `População Feminina`))+
  geom_line(linetype = 1, size = 2)+
  geom_point(size = 2,shape = 21, fill = "black")+
  labs(x = "Semestre", y= "Número de Mulheres",title="Relação evolutiva do número de mulheres presas e mulheres presas trabalhando",caption = "Banco de dados: SISDEPEN")+
  plots_theme

g
pop_presaFem_a <-g
ggsave(filename= "Figuras/Evolucao_Presas_trabalho.tiff",plot = g, device = "tiff",
       width = 12,height = 5,units = "in", dpi=150)
######################################################################################################
data_plot <- as.data.frame(t(pop_trabalho_fem))
data_plot$Semestre <- rev(semester_level)
colnames(data_plot) <- c("Trabalho externo","Trabalho interno","Total","Semestre")
dat <- data_plot  %>% pivot_longer(!Semestre, names_to = "Trabalho", values_to = "numero")

dat <- dat %>% mutate(Semestre = factor(Semestre,levels = semester_level))

g <- ggplot(dat, aes(x= Semestre, y=numero, colour = Trabalho, group = Trabalho))+
  geom_line(linetype = 1, size = 2)+
  geom_point(size = 2,shape = 21, fill = "black")+
  labs(x = "Semestre", y= "Número de Mulheres",
       title="Relação evolutiva do número de mulheres trabalhando - externo ou interno",
       caption = "Banco de dados: SISDEPEN")+
  plots_theme

g
pop_presaFem_b <-g
ggsave(filename= "Figuras/Evolucao_Presas_ExternoInterno.tiff",plot = g, device = "tiff",
       width = 12,height = 5,units = "in", dpi=150)
######################################################################################################
data_plot <- as.data.frame((t(populacao)))
data_plot$Semestre <- rev(semester_level)
data_plot <- data_plot %>% select("Pop Fem Trabalho Relativa","Semestre") %>% mutate(Semestre = factor(Semestre,levels = semester_level))
data_plot$Percentual <- data_plot$`Pop Fem Trabalho Relativa`*100

g <- ggplot(data_plot, aes(x= Semestre, y=Percentual,group = 1))+
  geom_line(linetype = 1, size = 1)+
  geom_point(size = 2,shape = 21, fill = "black")+
  labs(x = "Semestre", y= "Percentual (%)",
       title="Percentual de mulheres presas que trabalham no sistema prisonal brasileiro",
       caption = "Banco de dados: SISDEPEN")+
  plots_theme

g
pop_presaFem_c <-g
ggsave(filename= "Figuras/Percentual_Presas_Trabalhando.tiff",plot = g, device = "tiff",
       width = 10,height = 5,units = "in", dpi=150)
######################################################################################################
g <- ggarrange(pop_presaFem_a,
               pop_presaFem_b,
               pop_presaFem_c,
               labels = c("A", "B", "C"),
               ncol = 1, nrow = 3, legend = "right",
               font.label = list(size = 20))
g
ggsave(filename= "Figuras/Percentual_Pop_Presa_Fem.tiff",plot = g, device = "tiff",
       width = 12,height = 12,units = "in", dpi=150)
ggsave(filename= "Figuras/Percentual_Pop_Presa_Fem.png",plot = g, device = "png",
       width = 12,height = 12,units = "in", dpi=150)

rm(pop_presaFem_a,
   pop_presaFem_b,
   pop_presaFem_c,)
######################################################################################################
#Remuneração da população que trabalha
######################################################################################################
data_plot <- as.data.frame(t(remuneracao_trab_fem))
data_plot <- data_plot %>% select(-`Total Feminino`)

semester_level <- c("2024/2","2024/1","2023/2","2023/1","2022/2","2022/1","2021/2","2021/1",
                    "2020/2","2020/1","2019/2","2019/1","2018/2","2018/1","2017/2","2017/1","2016/2")
data_plot$Semestre <- semester_level

semester_level <- rev(semester_level)
data_plot <- data_plot %>% mutate(Semestre = factor(Semestre,levels = semester_level))

salario_minimo <- data_plot %>% select(Semestre)
salario_minimo$MinimoVigente <- c(1412,1412,1320,1320,1212,1212,1100,1100,1045,1045,998,998,954,954,937,937,880)
data_plot$Inflacao <- rev(c(1.49125020,             #2016
                                                        1.46600480,1.45057810,  #2017
                                                        1.42531370,1.39417140,  #2018
                                                        1.36187270,1.34996230, #2019
                                                        1.33677490,1.29416950,  #2020
                                                        1.23711420,1.16867180,  #2021
                                                        1.10722430,1.10355660,  #2022
                                                        1.05879800,1.05418350,  #2023
                                                        1.06239080,1)) 
data_plot <- full_join(data_plot,salario_minimo,by="Semestre")
rm(salario,salario_minimo)

data_plot[1,"Não recebe | Feminino"]

faixas_salario <- colnames(data_plot[1:6])
for(i in 1:length(data_plot$`Não recebe | Feminino`)){
  buf = 0
  total = sum(data_plot[i,2:5])
  buf = data_plot[i,1]*data_plot$MinimoVigente[i]*0 +
    data_plot[i,2]*data_plot$MinimoVigente[i]*0.375 +
    data_plot[i,3]*data_plot$MinimoVigente[i]*0.875 +
    data_plot[i,4]*data_plot$MinimoVigente[i]*1.5 +
    data_plot[i,5]*data_plot$MinimoVigente[i]*2
    
  data_plot[i,"Salario Medio"] = buf/total
  data_plot[i,"Salário Médio Corrigido"] = (buf/total)*data_plot$Inflacao[i]
  data_plot[i,"Remuneradas"] <- 100*(sum(data_plot[i,2:5])/sum(data_plot[i,1:5]))
  data_plot[i,"Não Remuneradas"] <- 100*(data_plot$`Não recebe | Feminino`[i]/sum(data_plot[i,1:5]))
  data_plot[i,"< 75% Sal. Min."] <- 100*(data_plot$`Menos do que 3/4 do salário mínimo mensal | Feminino`[i]/sum(data_plot[i,2:5]))
}
data_plot$PeloMinimo <- 100*(data_plot$`Salario Medio`/data_plot$MinimoVigente)
data_plot$`75% Sal. Min. Corrigido` <- data_plot$MinimoVigente*data_plot$Inflacao*0.75

write.csv(data_plot,"Remuneração_presas.csv")
###########
dat <- data_plot %>% select(c("Semestre",faixas_salario))%>% 
  pivot_longer(!Semestre, names_to = "income", values_to = "numero")

for(i in 1:length(dat$income)){
  dat$income[i] <- substr(dat$income[i], start = 1,stop=(nchar(dat$income[i])-11))
}
income_level <- dat$income[1:6]
dat <- dat %>% mutate(Semestre = factor(Semestre,levels = semester_level))
dat <- dat %>% mutate(income = factor(income,levels = income_level))

g <- ggplot(dat, aes(x= Semestre, y=numero, colour = income, group = income))+
  geom_line(linetype = 1, size = 2)+
  geom_point(size = 2,shape = 21, fill = "black")+
  labs(x = "Semestre", y= "Número de Mulheres",
       title="Relação evolutiva do número de mulheres presas por remuneração",
       caption = "Banco de dados: SISDEPEN")+
  plots_theme +
  theme(legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 20))+
  guides(colour = guide_legend(title = "Remuneração"))

g
a<-g
ggsave(filename= "Figuras/Evolucao_Presas_Remuneracao.tiff",plot = g, device = "tiff",
       width = 12,height = 5,units = "in", dpi=150)
###########
dat <- data_plot %>% select(c("Semestre","Remuneradas","< 75% Sal. Min.")) %>% 
  pivot_longer(!Semestre, names_to = "Percentual", values_to = "numero")
dat <- dat %>% mutate(Semestre = factor(Semestre,levels = semester_level))
dat <- dat %>% mutate(Percentual = factor(Percentual,levels = c("Remuneradas","< 75% Sal. Min.")))

g <- ggplot(dat, aes(x= Semestre, y=numero, colour = Percentual, group = Percentual))+
  geom_line(linetype = 1, size = 2)+
  geom_point(size = 2,shape = 21, fill = "black")+
  labs(x = "Semestre", y= "Percentual (%)",
       title="Percentual de mulheres presas - Remuneração",
       caption = "Banco de dados: SISDEPEN")+
  plots_theme +
  theme(axis.text.x = element_text(angle = 20))+
  guides(colour = guide_legend(title = "Remuneração"))

g
b<-g
ggsave(filename= "Figuras/Percentual_Presas_Remuneração.tiff",plot = b, device = "tiff",
       width = 10,height = 5,units = "in", dpi=150)
###########
dat <- data_plot %>% select(c("Semestre","Salário Médio Corrigido","75% Sal. Min. Corrigido")) %>% 
  pivot_longer(!Semestre, names_to = "Remuneração", values_to = "numero")
dat <- dat %>% mutate(Semestre = factor(Semestre,levels = semester_level))
dat <- dat %>% mutate(`Remuneração` = factor(`Remuneração`,levels = c("Salário Médio Corrigido","75% Sal. Min. Corrigido")))

g <- ggplot(dat, aes(x= Semestre, y=numero, colour =`Remuneração`, group = `Remuneração`))+
  geom_line(linetype = 1, size = 2)+
  geom_point(size = 2,shape = 21, fill = "black")+
  labs(x = "Semestre", y= "Reais (R$)",
       title="Remuneração média de mulheres presas que trabalham remuneradamente",
       caption = "Banco de dados: SISDEPEN")+
  plots_theme +
  theme(axis.text.x = element_text(angle = 20))+
  guides(colour = guide_legend(title = "Remuneração"))+
  scale_colour_manual(values = c("#99CCFF","#FF9966"))

g
c<-g
ggsave(filename= "Figuras/Remuneracao_MEDIA_Presas_SalMin_CorrigidoIPCA.tiff",plot = g, device = "tiff",
       width = 10,height = 5,units = "in", dpi=150)
######################################################################################################
g <- ggarrange(a,
               b,
               c,
               labels = c("A", "B", "C"),
               ncol = 1, nrow = 3, legend = "right",
               font.label = list(size = 20))
g
ggsave(filename= "Figuras/Remuneracao_Geral.tiff",plot = g, device = "tiff",
       width = 12,height = 12,units = "in", dpi=150)
ggsave(filename= "Figuras/Remuneracao_Geral.png",plot = g, device = "png",
       width = 12,height = 12,units = "in", dpi=150)
######################################################################################################
rm(data_presa,databuf,databuf2,bufi,nomes,nomes2,data_filt,df)
######################################################################################################
#Filtragem laborterapia
######################################################################################################
for(i in 1:length(raw_data)){
  all_data_sisdepen[[i]] <- filtra_sisdepen_laborterapia(raw_data[[i]])
}
data_plot <- data_plot %>% select(Semestre)
databuf <- data.frame(matrix(NA,nrow = length(data_plot$Semestre),ncol = length(all_data_sisdepen$data_24_2)+1))
colnames(databuf)<- c("Semestre",colnames(all_data_sisdepen$data_24_2))
databuf$Semestre <- data_plot$Semestre
row.names(databuf) <- row.names(data_plot)

for (i in 1:length(databuf$Semestre)) {
  print(dataframes_names[i])
  databuf[dataframes_names[i],][2:length(databuf[dataframes_names[i],])] <- colSums(na.omit(all_data_sisdepen[[dataframes_names[i]]]))
  print(colSums(na.omit(all_data_sisdepen[[dataframes_names[i]]]))[1])
}
######################################################################################################
# Tipo de vaga e setor
######################################################################################################
data_plot<-as.data.frame(databuf[,c(1,3,5,8,10,13,15,18,21,23,26,28,31,33,36,38,41,43,46,48,51,53,56,58,61,63)])
###################
#Vagas obtidas sem intervenção do sistema prisional
databuf <-data_plot[,c(1,2,3,4,5,6,7)]

databuf$`Setor Primário` <- databuf[[2]]+databuf[[3]]
databuf$`Setor Secundário` <- databuf[[4]]+databuf[[5]]
databuf$`Setor Terciário` <- databuf[[6]]+databuf[[7]]
databuf <-databuf[,c(1,8,9,10)]
databuf$Total <- databuf$`Setor Primário`+databuf$`Setor Secundário`+databuf$`Setor Terciário`

dat <- databuf %>% pivot_longer(!Semestre, names_to = "Setor", values_to = "numero")

dat <- dat %>% mutate(Semestre = factor(Semestre,levels = semester_level))

a <- ggplot(dat, aes(x= Semestre, y=numero, colour = Setor, group = Setor))+
  geom_line(linetype = 1, size = 2)+
  geom_point(size = 2,shape = 21, fill = "black")+
  labs(x = "Semestre", y= "Número de Mulheres",
       title="Sem intervenção do sistema prisional",
       caption = "Banco de dados: SISDEPEN")+
  plots_theme +
  guides(colour = guide_legend(title = "Setor"))
###################
#Vagas disponibilizadas pela adm. prisional em parceria com a iniciativa privada
databuf <-data_plot[,c(1,9,10,11,12,13,14)]

databuf$`Setor Primário` <- databuf[[2]]+databuf[[3]]
databuf$`Setor Secundário` <- databuf[[4]]+databuf[[5]]
databuf$`Setor Terciário` <- databuf[[6]]+databuf[[7]]
databuf <-databuf[,c(1,8,9,10)]
databuf$Total <- databuf$`Setor Primário`+databuf$`Setor Secundário`+databuf$`Setor Terciário`

dat <- databuf %>% pivot_longer(!Semestre, names_to = "Setor", values_to = "numero")

dat <- dat %>% mutate(Semestre = factor(Semestre,levels = semester_level))

b <- ggplot(dat, aes(x= Semestre, y=numero, colour = Setor, group = Setor))+
  geom_line(linetype = 1, size = 2)+
  geom_point(size = 2,shape = 21, fill = "black")+
  labs(x = "Semestre", y= "Número de Mulheres",
       title="Parceria com a iniciativa privada",
       caption = "Banco de dados: SISDEPEN")+
  plots_theme +
  guides(colour = guide_legend(title = "Setor"))
###################
#Vagas disponibilizadas pela adm. prisional em parceria com outros órgãos públicos
databuf <-data_plot[,c(1,15,16,17,18,19,20)]

databuf$`Setor Primário` <- databuf[[2]]+databuf[[3]]
databuf$`Setor Secundário` <- databuf[[4]]+databuf[[5]]
databuf$`Setor Terciário` <- databuf[[6]]+databuf[[7]]
databuf <-databuf[,c(1,8,9,10)]
databuf$Total <- databuf$`Setor Primário`+databuf$`Setor Secundário`+databuf$`Setor Terciário`

dat <- databuf %>% pivot_longer(!Semestre, names_to = "Setor", values_to = "numero")

dat <- dat %>% mutate(Semestre = factor(Semestre,levels = semester_level))

c <- ggplot(dat, aes(x= Semestre, y=numero, colour = Setor, group = Setor))+
  geom_line(linetype = 1, size = 2)+
  geom_point(size = 2,shape = 21, fill = "black")+
  labs(x = "Semestre", y= "Número de Mulheres",
       title="Parceria com outros órgãos públicos",
       caption = "Banco de dados: SISDEPEN")+
  plots_theme +
  guides(colour = guide_legend(title = "Setor"))
###################
#Vagas disponibilizadas pela adm. prisional em parceria entidades ou ONGs sem fins lucrativos
databuf <-data_plot[,c(1,21,22,23,24,25,26)]

databuf$`Setor Primário` <- databuf[[2]]+databuf[[3]]
databuf$`Setor Secundário` <- databuf[[4]]+databuf[[5]]
databuf$`Setor Terciário` <- databuf[[6]]+databuf[[7]]
databuf <-databuf[,c(1,8,9,10)]
databuf$Total <- databuf$`Setor Primário`+databuf$`Setor Secundário`+databuf$`Setor Terciário`

dat <- databuf %>% pivot_longer(!Semestre, names_to = "Setor", values_to = "numero")

dat <- dat %>% mutate(Semestre = factor(Semestre,levels = semester_level))

d <- ggplot(dat, aes(x= Semestre, y=numero, colour = Setor, group = Setor))+
  geom_line(linetype = 1, size = 2)+
  geom_point(size = 2,shape = 21, fill = "black")+
  labs(x = "Semestre", y= "Número de Mulheres",
       title="Parceria com entidades ou ONGs sem fins lucrativos",
       caption = "Banco de dados: SISDEPEN")+
  plots_theme +
  guides(colour = guide_legend(title = "Setor"))
###################
g <- ggarrange(a + labs(title = "Sem intervenção do sistema prisional"),
          b + labs(title = "Parceria com a iniciativa privada"),
          c + labs(title = "Parceria com outros órgãos públicos"),
          d + labs(title = "Parceria com entidades ou ONGs sem fins lucrativos"),
          labels = c("A", "B", "C","D"),
          ncol = 1, nrow = 4, common.legend = FALSE, legend = "right")

g
ggsave(filename= "Figuras/Evolucao_Presas_Setor.tiff",plot = g, device = "tiff",
       width = 12,height = 16,units = "in", dpi=150)
ggsave(filename= "Figuras/Evolucao_Presas_Setor.png",plot = g, device = "png",
       width = 12,height = 16,units = "in", dpi=150)
######################################################################################################
# Evolução Pop Prisional Feminina - por ESTADO
#######################################################################################################Filtragem de 
all_data_sisdepen<-raw_data
for(i in 1:length(all_data_sisdepen)){
  all_data_sisdepen[[i]] <- filtra_sisdepen2(all_data_sisdepen[[i]])
}
###############################################
# Conta população feminina e que trabalha por estado SISDEPEN
estados <- unique(all_data_sisdepen[[1]]$UF)

for (i in 1:length(all_data_sisdepen)) {
  pop_presas_UF <- data.frame(matrix(NA,nrow = length(estados),ncol = 4))
  row.names(pop_presas_UF) <- estados
  nomes2 <- c("População Feminina",
              "População trabalho",
              "Percentual",
              "Semestre")
  colnames(pop_presas_UF) <- nomes2
  pop_presas_UF$UF<-estados
  
  for (j in 1:length(estados)) {
    buf <- conta_presas(all_data_sisdepen[[dataframes_names[i]]]%>% filter(UF==estados[j]))
    pop_presas_UF[estados[j],]$`População Feminina` <- buf["Total Feminino",]
    buf <- conta_trabalho(all_data_sisdepen[[dataframes_names[i]]]%>% filter(UF==estados[j]))
    pop_presas_UF[estados[j],]$`População trabalho` <- buf["Total Feminino",]
    pop_presas_UF[estados[j],]$Percentual <- 100*(pop_presas_UF[estados[j],]$`População trabalho`/pop_presas_UF[estados[j],]$`População Feminina`)
    #pop_presas_UF[estados[j],]$Semestre <- rev(semester_level)[i]
  }
  pop_presas_UF$Semestre <- rev(semester_level)[i]
  if(i==1){
    dat <-pop_presas_UF
  }else{
    dat<-bind_rows(dat,pop_presas_UF)
  }
}

mapa_brasil<-read_state(code="all",year = 2025)
colnames(mapa_brasil) <- c("code_state","UF","name_state","code_region","name_region","geom")

dados_completos = full_join(mapa_brasil,dat,by="UF")
rm(dat,databuf,buf,i,j,mapa_brasil,pop_presas_UF)

dados_completos <- dados_completos %>% mutate(Semestre = factor(Semestre,levels = semester_level))

g<-ggplot()+
  geom_sf(data=dados_completos,color ="white", aes(fill=Percentual))+
  facet_wrap(~Semestre,ncol = 5)+
  scale_fill_gradient(low = "red2",high = "green3",limits = c(0, 100),oob = scales::squish)+
  labs(title="Percentual de mulheres presas que trabalham no sistema prisonal brasileiro - por estado",
       caption = "Banco de dados: SISDEPEN")+
  plots_theme

g
ggsave(filename= "Figuras/Percentual_Presas_Trabalhando_ESTADO.tiff",plot = g, device = "tiff",
       width = 14,height = 10,units = "in", dpi=150)


buf <- dados_completos %>% 
  st_drop_geometry() %>%
  dplyr::select(-c(code_state,name_state,code_region)) %>%
  filter(Semestre == "2024/2" | Semestre =="2016/2") 

buf2 <- buf %>% filter(Semestre == "2024/2") %>% select(-Semestre)
colnames(buf2) <- c("UF","name_region","Pop Fem 2024/2","Pop Trab 2024/2","% 2024/2")

buf3 <- buf %>% filter(Semestre == "2016/2") %>% select(-Semestre)
colnames(buf3) <- c("UF","name_region","Pop Fem 2016/2","Pop Trab 2016/2","% 2016/2")

buf <- left_join(buf2,buf3,by=c("UF","name_region"))

buf$Dif_pp <- buf$`% 2024/2` - buf$`% 2016/2`

write.csv(buf,"Percentual_Presas_Estado_TABELA.csv")
rm(buf,buf2,buf3)
