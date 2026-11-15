# three-dimensional-parties
Redutos eleitorais em três dimensões 

## PACKAGES 

```R
if (!base::requireNamespace("pacman", quietly = TRUE)) {
  utils::install.packages("pacman")
}

pacman::p_load(
  electionsBR,
  dplyr,
  readr,
  stringr,
  tidyselect
)
```

## DATABASE DESCRIPTION [![MIT license](https://img.shields.io/badge/License-MIT-blue.svg)](https://github.com/ryallmeida/ppe?tab=MIT-1-ov-file) [![Open Data](https://img.shields.io/badge/OPEN-DATA-blue)](https://opendefinition.org/od/2.1/en/)





## Limitações, agenda e considerações finais
- A seção eleitoral não é necessariamente uma unidade espacial estável, visto que ela é uma dimensão/unidade administrativa e não geofráfica, de modo que isso desemboca numa afetação para a competição independentemente da magnitude distrital. A questão é que os dados geográficos associados às seções eleitorais não são necessariamente constantes entre eleições.
- Uma sessão pode: (i) Ter seu local de votação alterado; (ii) ser transferida para outro endereço; (iii) ser agregada a outra seção; (iv) ser desativada; (v) Ser renumerada; (vi) ter seu endereço ou cadastro geográfico corrigido.
- Os eleitores também podem mudar de seção e de zona eleitoral ao longo do tempo. E eles podem: (i) sofrer com a transferência do título eleitoral (reorganização administrativa das seções/zonas e redistribuição de eleitores entre seções/zonas)
- Pede-se que estime-se o quanto essas observações representam dentro do banco de dados, a fim de observar o erro em potencial da visualização. 
- As lideranças dos partidos devem acompanhar e validar esse modelo de visualização. Ou seja, devem confirmar se as mudanças do reduto de seu respectivo partido foi respeitada. 
