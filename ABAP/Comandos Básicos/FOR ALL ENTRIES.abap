*&---------------------------------------------------------------------*
*& Report ZR_FOR_ALL_ENTRIES.
*&---------------------------------------------------------------------*
*& Programa exemplo da utilização do FOR ALL ENTRIES.
*&---------------------------------------------------------------------*
REPORT zr_for_all_entries.

*&---------------------------------------------------------------------*
*& Declaração de tabelas globais
*&---------------------------------------------------------------------*
TABLES : mara, mard.  "Tabelas usadas no programa

*&---------------------------------------------------------------------*
*& Definição de tipos para estrutura dos dados
*&---------------------------------------------------------------------*
TYPES :
  BEGIN OF ty_mara, "Tipo para dados da tabela MARA
    matnr TYPE mara-matnr, "Número do material
    ersda TYPE mara-ersda, "Data de criação
    ernam TYPE mara-ernam, "Usuário que criou
  END OF   ty_mara,

  BEGIN OF ty_makt, "Tipo para dados da tabela MAKT
    matnr TYPE makt-matnr, "Número do material
    spras TYPE makt-spras, "Idioma
    maktx TYPE makt-maktx, "Descrição do material
  END OF   ty_makt,

  BEGIN OF ty_mard, "Tipo para dados da tabela MARD
    matnr TYPE mard-matnr, "Número do material
    werks TYPE mard-werks, "Centro
    lgort TYPE mard-lgort, "Depósito
    pstat TYPE mard-pstat, "Status de armazenamento
  END OF   ty_mard,

  BEGIN OF ty_saida, "Tipo para estrutura de saída
    check TYPE c, "Flag de validação (não utilizado no código atual)
    matnr TYPE mara-matnr, "Número do material
    ersda TYPE mara-matnr, "Data de criação (possível erro no tipo, deveria ser mara-ersda)
    maktx TYPE makt-maktx, "Descrição do material
    werks TYPE mard-werks, "Centro
    lgort TYPE mard-lgort, "Depósito
    pstat TYPE mard-pstat, "Status de armazenamento
  END OF ty_saida.

*&---------------------------------------------------------------------*
*& Áreas de trabalho (work areas) para manipulação de dados
*&---------------------------------------------------------------------*
DATA :
  w_mara  TYPE ty_mara,   "Área de trabalho para MARA
  w_makt  TYPE ty_makt,   "Área de trabalho para MAKT
  w_mard  TYPE ty_mard,   "Área de trabalho para MARD
  w_saida TYPE ty_saida.  "Área de trabalho para saída

*&---------------------------------------------------------------------*
*& Tabelas internas para manipulação de dados
*&---------------------------------------------------------------------*
DATA :
  t_mara  TYPE TABLE OF ty_mara,   "Tabela interna para MARA
  t_makt  TYPE TABLE OF ty_makt,   "Tabela interna para MAKT
  t_mard  TYPE TABLE OF ty_mard,   "Tabela interna para MARD
  t_saida TYPE TABLE OF ty_saida.  "Tabela interna para saída

*&---------------------------------------------------------------------*
*& Tela de seleção
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1. "Bloco para parâmetros de seleção

SELECT-OPTIONS : s_matnr FOR mara-matnr,                           "Faixa de números de materiais
                 s_werks FOR mard-werks NO INTERVALS NO-EXTENSION, "Centro sem intervalos
                 s_lgort FOR mard-lgort NO INTERVALS NO-EXTENSION. "Depósito sem intervalos

SELECTION-SCREEN END OF BLOCK b1.

*&---------------------------------------------------------------------*
*& Evento START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM: zf_selecacao_dados.      "Chama a rotina de seleção de dados
  PERFORM: zf_processamento_dados.  "Chama a rotina de processamento de dados
  PERFORM: zf_exibe_dados.          "Chama a rotina de exibição de dados

*&---------------------------------------------------------------------*
*& Rotina ZF_SELECACAO_DADOS: Seleção de dados do banco
*&---------------------------------------------------------------------*
FORM zf_selecacao_dados.

  SELECT matnr
         ersda
         ernam
  FROM mara
  INTO TABLE t_mara
  UP TO 100 ROWS "Limita a seleção a 100 registros
  WHERE
  matnr IN s_matnr. "Filtra pelos materiais selecionados

  IF t_mara IS NOT INITIAL. "Verifica se houve retorno de dados

    SELECT  matnr
            spras
            maktx
    FROM makt
    INTO TABLE t_makt
    FOR ALL ENTRIES IN t_mara "Evita múltiplos SELECTs
    WHERE
    matnr = t_mara-matnr AND
    spras = sy-langu. "Seleciona descrições no idioma do sistema

    SELECT  matnr
            werks
            lgort
            pstat
    FROM mard
    INTO TABLE t_mard
    FOR ALL ENTRIES IN t_mara
    WHERE
    matnr = t_mara-matnr AND
    werks IN s_werks     AND
    lgort IN s_lgort. "Aplica os filtros de centro e depósito

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Rotina ZF_PROCESSAMENTO_DADOS: Processamento dos dados
*&---------------------------------------------------------------------*
FORM zf_processamento_dados.

  SORT t_makt BY matnr. "Ordena a tabela interna MAKT por número de material

  LOOP AT t_mara INTO w_mara. "Itera pelos materiais selecionados

    w_saida-matnr = w_mara-matnr.
    w_saida-ersda = w_mara-ersda.

    CLEAR w_makt.
    READ TABLE t_makt INTO w_makt WITH KEY matnr = w_mara-matnr BINARY SEARCH. "Busca descrição
    IF sy-subrc = 0.

      w_saida-maktx = w_makt-maktx.

    ENDIF.

    READ TABLE t_mard INTO w_mard WITH KEY matnr = w_mara-matnr BINARY SEARCH. "Busca dados de estoque
    IF sy-subrc = 0.

      w_saida-werks = w_mard-werks.
      w_saida-lgort = w_mard-lgort.
      w_saida-pstat = w_mard-pstat.

    ENDIF.

    APPEND w_saida TO t_saida. "Adiciona o registro processado à tabela de saída

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Rotina ZF_EXIBE_DADOS: Exibição dos dados processados
*&---------------------------------------------------------------------*
FORM zf_exibe_dados.

  LOOP AT t_saida INTO w_saida. "Itera pelos dados de saída

    WRITE : / w_saida. "Escreve os dados no output

  ENDLOOP.

ENDFORM.