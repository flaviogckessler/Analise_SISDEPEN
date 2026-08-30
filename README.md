# Analise SISDEPEN

**Flávio Gabriel Carazza-Kessler, Daniela Damion**

Agosto, 2026

Repositório com o código e banco de dados utilizado para analisar o trabalho na população prisional feminina.

## Banco de dados

A análise utiliza os dados disponibilizados pelo Sistema de Informações do Departamento Penitenciário Nacional (SISDEPEN). A base de dados do SISDEPEN conta com ciclos semestrais que agregam informações sobre todas as unidades prisionais do Brasil.

As tabelas utilizadas como fonte na pesquisa estão disponíveis na pasta [`data`](data/). Estas fontes foram foram baixadas diretamenta da [base de dados do SISDEPEN](https://www.gov.br/senappen/pt-br/servicos/sisdepen/bases-de-dados).

## Código fonte

O preprocessamento, análise e plotagem dos dados foram realizados em ambiente R. O código fonte utilizado é [`Codigo_Analise_TrabalhoDetentas_SISDEPEN.R`](Codigo_Analise_TrabalhoDetentas_SISDEPEN.R).

As bibliotecas utilizadas como dependência dessa análise são: `readr`, `dplyr`, `ggplot2`, `tidyverse`, `stringr`, `ggpubr`, `geobr`, `sf`.

## Citação

Caso você utilize as análises aqui presentes, por favor cite:

DAMION, Daniela; CARAZZA-KESSLER, Flávio Gabriel. “Mente vazia é oficina do diabo”: uma análise do direito ao trabalho nas prisões femininas brasileiras desde as políticas públicas às estatísticas prisionais. Dilemas: Revista de Estudos de Conflito e Controle Social, v. X, n. X, p. XX–XX, 2026.

```         
@article{DamionCarazzaKessler2026,
  author  = {Damion, Daniela and Carazza-Kessler, Flávio Gabriel},
  title   = {{``Mente vazia é oficina do diabo'': Uma análise do direito ao trabalho nas prisões femininas brasileiras desde as políticas públicas às estatísticas prisionais}},
  journal = {Dilemas: Revista de Estudos de Conflito e Controle Social},
  year    = {2026},
  issn    = {2178-2792}
}
```
