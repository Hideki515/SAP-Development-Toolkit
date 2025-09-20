Fazer uma tela de seleção com select-options os campos: 

- s\_movnr - (mat\_handling-movnr) 
- s\_whnr  - (storehouse-whnr) 
- s\_matnr – (mat\_handling-matnr) 

Fazer as seleções dos campos: 



|**Campos** |**Descrição** |**Seleção** |
| - | - | - |
|movnr |Número Movimento |Com os valores inseridos no select-options, bucar o campo movnr da tabela mat\_handling |
|branr |Filial |Com whnr da tabela mat\_handling, ir na tabela branch e pegar os campos branr |
|compnr |Empresa |Com whnr da tabela mat\_handling, ir na tabela branch e pegar os campos compnr |
|whnr |Armazém |Com whnr da tabela mat\_handling, ir na tabela storehouse e pegar os campos whnr |
|description |Descrição Armazém |Com whnr da tabela mat\_handling, ir à tabela storehouse e pegar os campos description |
|matnr  |Material |Com matnr da tabela mat\_handling, ir à tabela material e pegar o campo matnr |
|maktx  |Descrição Material |Com matnr da tabela mat\_handling, ir à tabela material e pegar o campo maktx |
|quantity |Quantidade |Com os valores inseridos no select-options, bucar o campo quantity da tabela mat\_handling |
|price |Preço do Material |Com matnr da tabela mat\_handling, ir à tabela material e pegar o campo price |
|valor |Total Movimentado |Fazer os valores quantity \* price para o obter o campo valor |
|doctype |Tipo do Documento |Com os valores inseridos no select-options, bucar o campo doctype da tabela mat\_handling |



|docnr |Número do Documento |Com os valores inseridos no select-options, bucar o campo docnr da tabela mat\_handling |
| - | - | :- |
|movtyp |Tipo de Movimentação |Com os valores inseridos no select-options, bucar o campo movtyp da tabela mat\_handling |
|erdat |<p>Data de Criação do </p><p>Registro </p>|Com os valores inseridos no select-options, bucar o campo erdat da tabela mat\_handling |
|entrytime |Data de Entrada |Com os valores inseridos no select-options, bucar o campo entrytime da tabela mat\_handling |
|ernam |Criado por |Com os valores inseridos no select-options, bucar o campo ernam da tabela mat\_handling |

Exibir os valores utilizando  ALV. 
