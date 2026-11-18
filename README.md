# Analise SISDEPEN

**Flávio Gabriel Carazza-Kessler, Daniela Damion**

Novembro, 2026

Repositório com o código e banco de dados utilizado para analisar o trabalho na população prisional feminina.

## Banco de dados

A análise utiliza os dados disponibilizados pelo Sistema de Informações do Departamento Penitenciário Nacional (SISDEPEN). A base de dados do SISDEPEN conta com ciclos semestrais que agregam informações sobre todas as unidades prisionais do Brasil.

As tabelas utilizadas como fonte na pesquisa estão disponíveis na pasta [`data`](data/). Estas fontes foram foram baixadas diretamenta da [base de dados do SISDEPEN](https://www.gov.br/senappen/pt-br/servicos/sisdepen/bases-de-dados).

## Código fonte

O preprocessamento, análise e plotagem dos dados foram realizados em ambiente R. O código fonte utilizado é [`Codigo_Analise_TrabalhoDetentas_SISDEPEN.R`](Codigo_Analise_TrabalhoDetentas_SISDEPEN.R).

As bibliotecas utilizadas como dependência dessa análise são: `readr`, `dplyr`, `ggplot2`, `tidyverse`, `stringr`, `ggpubr`, `geobr`, `sf`.

Tabelas oriundas da análise são disponibilizadas em [`results`](results/).

## Citação

O artigo encontra-se em processo de submissão. Tão logo seja aceito por algum periódico, sera disponibilizado aqui.

## Licença

Este repositório é disponibilizado sob a licença [Creative Commons Attribution 4.0 International (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/). Consulte o arquivo [`LICENSE`](LICENSE) para os termos completos.
