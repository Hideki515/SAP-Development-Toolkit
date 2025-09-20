## 2 - Exercício Batch Input

**Objetivo**

Batch input onde a execução do mesmo deverá ser efetuada através de um módulo de função(SE37 - Na apostila tem exemplo). Este irá transformar o batch input do exercício de carga na transação MM01 em um módulo de função que poderá ser reaproveitado em outros programas. 

**Parâmetros de entrada**

* Declarar o parâmetro I_MATNR TYPE MARA-MATNR.
* Declarar o parâmetro I_MAKTX TYPE MAKT-MAKTX.
* Declarar o parâmetro I_MEINS TYPE MARA-MEINS.
* Declarar o parâmetro I_BRGEW TYPE MARA-BRGEW.
* Declarar o parâmetro I_GEWEI TYPE MARA-GEWEI.
* Declarar o parâmetro I_VOLUM TYPE MARA-VOLUM.
* Declarar o parâmetro I_VOLEH TYPE MARA-VOLEH.

**Tables**

* Declarar a tabela T_ERRO like ZSTLOG e flegar o checkbox opcional.
* Declarar a tabela T_SUCESSO like ZSTLOG e flegar o checkbox opcional.

**Processamento (Código-fonte)**

* Efetuar o mapeamento do batch input na MM01.
* Efetuar o call transaction na transação MM01.
* Se der erro no call transaction, gerar pasta de batch input. 
