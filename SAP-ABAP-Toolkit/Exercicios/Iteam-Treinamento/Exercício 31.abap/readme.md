## Exercício ALV "Relatório de Faturas por Pagador"

### Objetivo

Criar um relatório ALV em ABAP que apresente informações sobre faturas de um determinado pagador, com formatação condicional e campos concatenados.

### Tela de Seleção

A tela de seleção deve conter os seguintes parâmetros:

| PARÂMETRO | SELECT-OPTIONS | TIPO | DESCRIÇÃO | OBSERVAÇÃO |
|---|---|---|---|---|
| S_VBELN |  | VBRK-VBELN | Número Fatura |  |
| S_FKDAT |  | VBRK-FKDAT | Data Criação | Obrigatório |
| P_KUNRG |  | VBAK-KUNRG | Pagador |  |

### Seleção de dados

1. **Selecionar dados da tabela VBRK:**
    - Selecionar os campos `VBELN`, `FKDAT` e `KUNRG`.
    - Condições:
        - `VBELN` dentro do intervalo `S_VBELN`.
        - `FKDAT` dentro do intervalo `S_FKDAT`.
        - `KUNRG` igual a `P_KUNRG`.
    - Armazenar os registros na tabela interna `T_VBRK`.

2. **Selecionar dados da tabela VBRP:**
    - Selecionar os campos `VBELN`, `POSNR`, `MATNR`, `FKIMG`, `VRKME`, `NETWR` e `AUBEL`.
    - Condição: `VBRP-VBELN` igual a `T_VBRK-VBELN`.
    - Armazenar os registros na tabela interna `T_VBRP`.

3. **Selecionar dados da tabela VBAK:**
    - Selecionar o campo `VBELN`.
    - Condição: `VBAK-VBELN` igual a `T_VBRP-AUBEL`.
    - Armazenar os registros na tabela interna `T_VBAK`.

4. **Selecionar dados da tabela KNA1:**
    - Selecionar os campos `KUNNR` e `NAME1`.
    - Condição: `KNA1-KUNNR` igual a `T_VBRK-KUNRG`.
    - Armazenar os registros na tabela interna `T_KNA1`.

5. **Selecionar dados da tabela MAKT:**
    - Selecionar os campos `MATNR` e `MAKTX`.
    - Condição: `MAKT-MATNR` igual a `T_VBRP-MATNR`.
    - Armazenar os registros na tabela interna `T_MAKT`.

### Processamento

1. **Criar tabela interna para o relatório:**
    - Criar a tabela interna `T_VBRP` com os seguintes campos:
        - `T_VBRK-VBELN` (chave)
        - `T_VBRK-FKDAT`
        - `T_VBRK-KUNRG` / `T_KNA1-NAME1` (concatenados)
        - `T_VBRP-POSNR`
        - `T_VBRP-MATNR` / `T_MAKT-MAKTX` (concatenados)
        - `T_VBRP-FKIMG`
        - `T_VBRP-VRKME`
        - `T_VBRP-NETWR`
        - `T_VBRP-AUBEL`
        - `STATUS`

2. **Preencher a tabela interna do relatório:**
    - Percorrer a tabela interna `T_VBRK`.
    - Para cada registro em `T_VBRK`:
        - Ler o registro correspondente em `T_VBRP` (usando `VBELN`).
        - Ler o registro correspondente em `T_VBAK` (usando `AUBEL`).
        - Ler o registro correspondente em `T_KNA1` (usando `KUNRG`).
        - Ler o registro correspondente em `T_MAKT` (usando `MATNR`).
        - Preencher um novo registro na tabela interna `T_VBRP` com os dados coletados.

### Layout do relatório

1. **Campos a serem exibidos:**
    - Todos os campos da tabela interna `T_VBRP` devem ser exibidos no relatório.
2. **Concatenção de campos:**
    - O campo `T_VBRK-KUNRG` / `T_KNA1-NAME1` deve ser exibido como um único campo, com os valores separados por `/`.
    - O campo `T_VBRP-MATNR` / `T_MAKT-MAKTX` deve ser exibido como um único campo, com os valores separados por `-`.
3. **Formatação condicional:**
    - O campo `T_VBRP-NETWR` deve ser formatado como um campo de valor monetário.
    - Os campos `T_VBRK-VBELN` e `T_VBRP-AUBEL` devem ser definidos como **HOTSPOT**.
        - Ao clicar em um hotspot, o sistema deve abrir a transação correspondente (`VA03` para `VBELN` e `VF03` para `AUBEL`).
    - O campo `STATUS` deve ser preenchido da seguinte forma:
        - Se o registro já existir na tabela Z, o campo deve ser preenchido com `verde`.
        - Se o registro for novo, o campo deve ser preenchido com `amarelo`.
    - O campo `T_VBRP-FKIMG` deve ser formatado da seguinte forma:
        - Se o valor for menor que 10, o campo deve ser pintado de `vermelho`.
        - Se o valor for maior ou igual a 10, o campo deve ser pintado de `verde`.

### Observações

- Utilizar o código de transação `ALV` para criar o relatório.
- Utilizar a técnica de `field symbol` para preencher a tabela interna do relatório.
- Utilizar a função `REUSE_ALV_GRID_DISPLAY` para exibir o relatório.

### Saída do relatório

O relatório deve apresentar uma lista de faturas do pagador selecionado, com as informações formatadas conforme descrito acima. 
