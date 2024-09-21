# BRT Sul e o Acesso a Emprego em Brasília: Uma análise via Modelo de Diferenças em Diferenças

##### Table of Contents  
[Abstract](#abstract)  
[Metodo](#metodo)  


## Abstract

This study examines the impact of the Expresso BRT Sul in Brasília on employment accessibility, utilizing a Difference-in-Differences (DiD) model with data from the Employment and Unemployment Survey (PED - DF). The research focuses on the causal relationship between improved public transport infrastructure and employment outcomes in the peripheral regions of Gama and Santa Maria, following the BRT's inauguration in 2014. The analysis highlights the reduction in travel time and increased accessibility to central areas, evaluating effects on employment probability, wages, working hours, and job formalization.

## Metodo
### Pasta build/R

Para criar a base utilizada nos modelos, os scrips em R contidos em build/R 

1 - empilha ped: lê e junta os microdados públicos de cada ano da PED  
2 - limpa bases: trata e seleciona variáveis de interesse    
3 - cria bases correspondencia malhas: cria a relação entre os microdados e as informações da malha de 2010  
4 - prepara malha: Prepara a malha do Censo de 2000 e prepara base para correspondência    
5 - cria base analises: junta informações espaciais e cria grupos de tratamento e controle  

# Como reproduzir o projeto

O projeto R está organizado em duas pastas principais: a pasta build e a pasta analysis. A pasta build contém os scripts e os dados para construir a base a ser analisada. A pasta analysis reúne todos os resultados, tabelas, gráficos e mapas do projeto.

## Como produzir a base para análises

