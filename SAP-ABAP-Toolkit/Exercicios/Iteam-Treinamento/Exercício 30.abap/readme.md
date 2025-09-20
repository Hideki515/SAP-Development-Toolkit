## Relatório de Ordens de Venda em Moeda EURO

Este programa gera um relatório de ordens de venda em EURO, consultando e processando dados de diversas tabelas do sistema. O relatório exibe informações sobre as ordens, como número da ordem, data de criação, cliente, valores e status, formatadas de acordo com o layout especificado.

### Seleção de Dados

O programa busca dados nas seguintes tabelas:

* **VBAK:** Ordens de venda
* **VBAP:** Itens de ordem de venda
* **VBELN:** Documento de vendas
* **ERDAT:** Data de criação
* **NETWR:** Valor líquido da ordem
* **KUNNR:** Código do cliente
* **KNA1:** Dados mestre de cliente
* **LIPS:** Entregas
* **VGBEL:** Documento de vendas de referência
* **VGPOS:** Item de documento de vendas de referência
* **MKT:** Material
* **MAKT:** Descrição do material
* **GSBER:** Área de negócios
* **TGSBT:** Descrição da área de negócios

### Processamento

O programa executa as seguintes etapas de processamento:

1. **Leitura e junção de dados:** Lê dados das tabelas VBAK, VBAP, KNA1, LIPS, MAKT, e TGSBT, relacionando-as através de campos chave.
2. **Filtragem de dados:** Seleciona apenas os registros dentro do período especificado (01.01.2008 a 31.12.2008).
3. **Cálculo e formatação:** Calcula o valor total da ordem e formata os dados para exibição no relatório.
4. **Geração de relatório:** Cria um relatório com o layout especificado, incluindo título, cabeçalho, dados das ordens e rodapé.

### Layout do Relatório

O relatório apresenta as seguintes informações:

**Cabeçalho:**

* Título: Relatório de Ordens de Venda em Moeda EURO
* Data e hora de execução do relatório

**Dados da ordem:**

* **T_VBAK-VBELN:** Número da ordem
* **T_VBAK-ERDAT:** Data de criação da ordem
* **T_VBAK-POSNR:** Posição do item na ordem
* **T_VBAK-KUNNR / T_KNA1-NAME1:** Código e nome do cliente
* **T_VBAK-NETWR:** Valor líquido da ordem
* **T_VBAK-MATNR / T_MAKT-MAKTX:** Código e descrição do material
* **T_VBAP-GSBER / T_GSBT-GTEXT:** Área de negócios e descrição
* **T_LIPS-VBELN:** Número da entrega
* **STATUS:** Status da ordem (com semáforo)

**Observações:**

* Campos concatenados: Alguns campos são concatenados para melhor organização do relatório, como o código e nome do cliente, código e descrição do material e área de negócios e descrição.
* Semáforo de status: O campo STATUS exibe um semáforo que indica o status da ordem:
    * Vermelho: NETWR <= 20000
    * Amarelo: 20000 < NETWR <= 40000
    * Verde: NETWR > 40000

### Instruções de Uso

Para executar o programa, siga estas etapas:

1. Configure os parâmetros de seleção, como o período desejado.
2. Execute o programa.
3. O relatório será gerado com as informações das ordens de venda no período especificado.

### Observações

* Este documento fornece uma visão geral do programa e do relatório gerado.
* Para mais detalhes sobre o código e a lógica de programação, consulte o código fonte. 
