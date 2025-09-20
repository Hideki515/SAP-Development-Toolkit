### 1.0. Objetivo
Este procedimento tem como objetivo demonstrar o desenvolvimento do relatório que unificará os dados mestres de materiais, fornecedores e registros infos.

### 1.1. Modelo do relatório
O Registro Info é uma das ferramentas mais importantes do processo de compras. É através dele que temos as informações de valor negociado do material e fornecedor, além de outros dados relevantes como código de imposto, grupo de compras, condições de compras, entre outras opções. O cliente Myralis identificou a necessidade de criar um relatório Z para que possa trazer os dados dos materiais, fornecedores e registro infos.

O relatório deverá ter uma tela de seleção para que os usuários possam realizar uma pesquisa com base nos seguintes campos:

**Campos de seleção:**
- Material: `MARA-MATNR`;
- Tipo do Material: `MARA-MTART`;
- Centro: `EINE-WERKS`;
- Grupo de Mercadorias: `MARA-MATKL`;
- Status do Material: `MARA-MSTAE`;
- Registro Info: `EINA-INFNR`;
- Fornecedor: `LFA1-LIFNR`;
- CNPJ: `LFA1-STCD1`;
- Grupo de Compradores: `EINE-EKGRP`;
- Organização de compras: `EINE-EKORG`;
- Código IVA: `EINE-MWSKZ`;

**Campos do relatório (Tabela + Campo):**
- Reg Info: `EINA-INFNR`;
- Dt. Criação: `EINA- ERDAT`;
- Fornecedor: `LFA1-LIFNR`;
- Descrição: `LFA1- NAME1`;
- Material: `MARA-MATNR`;
- Descrição: `MAKT-MAKTX`;
- Tipo: `MARA-MTART`;
- Status: `MARA-MSTAE`;
- G.Compradores: `EINE-EKGRP`;
- Org. Compras: `EINE-EKORG`;
- G. Mercadorias: `MARA-MATKL`;
- IVA: `EINE-MWSKZ`;
- Centro: `EINE-WERKS`;
- UMB: `MARA-MEINS`;
- Preço líquido: `EINE- NETPR`;
- Moeda: `EINE- WAERS`;
- Doc. Compra: `EINE- EBELN`;
- Dt Pedido: `EINE- DATLB`;