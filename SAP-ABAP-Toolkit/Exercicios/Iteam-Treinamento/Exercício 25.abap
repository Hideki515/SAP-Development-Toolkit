*&---------------------------------------------------------------------------------------*
*& REPORT zpr_exec_25_bhs.
*&---------------------------------------------------------------------------------------*
*& Nome:zpr_exec_25_bhs
*& Tipo: Report
*& Objetivo: Elaborar um programa ABAP onde deverá ser criada uma tela de seleção com
*& o Nº conta do Razão como seleção múltipla. Seu tipo se encontra na tabela SKA1.
*& Selecionar na tabela SKA1 todas as Contas do Razão que estiverem de acordo
*& com o campo Nº conta do Razão da tela de seleção e que pertençam ao Plano de
*& Contas ‘INT’, retornando os campos Nº conta do Razão e Data de criação do registro.
*& Para cada registro encontrado na tabela SKA1, selecionar na tabela SKB1 as
*& empresas correspondentes as Contas do Razão encontradas onde o campo Nº conta do
*& Razão relaciona as duas tabelas, retornando os campos Empresa e Nº conta do Razão.
*& Para cada registro encontrado na tabela SKB1, selecionar na tabela T001 os
*& dados das empresas selecionadas, onde o campo Empresa relaciona as duas tabelas e
*& o País seja igual a ‘BR’, retornando os campos Empresa e Denominação da firma ou
*& empresa.
*& Para cada registro retornado da tabela SKA1, selecionar na tabela SKAT sua
*& descrição desde que esta esteja no Código de idioma ‘PT’, Plano de Contas ‘INT’, onde
*& o campo Nº conta do Razão relaciona as duas tabelas, retornando os campos Nº conta
*& do Razão e Texto das contas do Razão.
*& O relatório deve imprimir todas as contas do razão de cada empresa selecionada
*& do País ‘BR’, bem como os dados da empresa e a descrição da conta do razão no
*& Código de idioma ‘PT’. Para cada empresa deverá mostrar um contador de contas do
*& razão e no final do relatório a quantidade de registros encontrados.
*& Imprimir os campos: Empresa, Denominação da firma ou empresa, Nº conta do
*& Razão, Data de criação do registro e Texto das contas do Razão.
*&---------------------------------------------------------------------------------------*

REPORT zpr_exec_25_bhs.

*&-------------------------*
*& Tables
*&-------------------------*
TABLES: ska1.

*&-------------------------*
*& Types
*&-------------------------*
TYPES: BEGIN OF ty_ska1,
         saknr TYPE ska1-saknr,
         ktopl TYPE ska1-ktopl,
         erdat TYPE ska1-erdat,
       END OF ty_ska1.

TYPES: BEGIN OF ty_skb1,
         saknr TYPE skb1-saknr,
         bukrs TYPE skb1-bukrs,
       END OF ty_skb1.

TYPES: BEGIN OF ty_t001,
         bukrs TYPE t001-bukrs,
         butxt TYPE t001-butxt,
         spras TYPE t001-spras,
         land1 TYPE t001-land1,
       END OF ty_t001.

TYPES: BEGIN OF ty_skat,
         spras TYPE skat-spras,
         ktopl TYPE skat-ktopl,
         saknr TYPE skat-saknr,
         txt20 TYPE skat-txt20,
       END OF ty_skat.

TYPES: BEGIN OF ty_saida,
         bukrs TYPE skb1-bukrs,
         butxt TYPE t001-butxt,
         saknr TYPE ska1-saknr,
         ktopl TYPE ska1-ktopl,
         erdat TYPE ska1-erdat,
         land1 TYPE t001-land1,
         txt20 TYPE skat-txt20,
       END OF ty_saida.

*&-------------------------*
*& Work Area
*&-------------------------*
DATA: wa_ska1 TYPE ty_ska1.
DATA: wa_skb1 TYPE ty_skb1.
DATA: wa_t001 TYPE ty_t001.
DATA: wa_skat TYPE ty_skat.
DATA: wa_saida TYPE ty_saida.
DATA: wa_fieldcat TYPE slis_fieldcat_alv. "Passará a formatação das colunas
DATA: wa_layout   TYPE  slis_layout_alv. "Pode passar configurações de layout da tabela

*&-------------------------*
*& Internal Tables
*&-------------------------*
DATA: it_fieldcat TYPE slis_t_fieldcat_alv. "Guardará a formatação das colunas
DATA: it_ska1 TYPE TABLE OF ty_ska1.
DATA: it_skb1 TYPE TABLE OF ty_skb1.
DATA: it_t001 TYPE TABLE OF ty_t001.
DATA: it_skat TYPE TABLE OF ty_skat.
DATA: it_saida TYPE TABLE OF ty_saida.

*&-------------------------*
*& Paramiters Selections
*&-------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

SELECT-OPTIONS: s_saknr FOR ska1-saknr.

SELECTION-SCREEN END OF BLOCK b1.

*&-------------------------*
*& Performs
*&-------------------------*
PERFORM zf_select_data.
PERFORM zf_process_data.
PERFORM zf_display_alv.

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Seleciona os dados
*----------------------------------------------------------------------*
FORM zf_select_data .

  SELECT
    saknr
    ktopl
    erdat
    FROM ska1
    INTO TABLE it_ska1
    WHERE
    saknr IN s_saknr AND
    ktopl = 'INT'.

  IF it_ska1 IS NOT INITIAL.

    SELECT
      saknr
      bukrs
      FROM skb1
      INTO TABLE it_skb1
      FOR ALL ENTRIES IN it_ska1
      WHERE
      saknr = it_ska1-saknr.

  ENDIF.

  IF it_skb1 IS NOT INITIAL.

    SELECT
      bukrs
      butxt
      spras
      land1
      FROM t001
      INTO TABLE it_t001
      FOR ALL ENTRIES IN it_skb1
      WHERE
      bukrs = it_skb1-bukrs AND
      land1 = 'BR'.

  ENDIF.

  IF it_ska1 IS NOT INITIAL.

    SELECT
      spras
      ktopl
      saknr
      txt20
      FROM skat
      INTO TABLE it_skat
      FOR ALL ENTRIES IN it_ska1
      WHERE
      spras = 'P' AND
      ktopl = 'INT'AND
      saknr = it_ska1-saknr.

  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Processa os dados a serem exibidos no alv.
*----------------------------------------------------------------------*
FORM zf_process_data .

  LOOP AT it_ska1 INTO wa_ska1.
    READ TABLE it_skb1 INTO wa_skb1 WITH KEY saknr = wa_ska1-saknr.

    IF sy-subrc = 0.
      READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_skb1-bukrs.
    ENDIF.

    IF sy-subrc = 0.
      READ TABLE it_skat INTO wa_skat WITH KEY saknr = wa_ska1-saknr.
    ENDIF.
  ENDLOOP.

  IF sy-subrc = 0.
    wa_saida-bukrs = wa_skb1-bukrs.
    wa_saida-butxt = wa_t001-butxt.
    wa_saida-saknr = wa_ska1-saknr.
    wa_saida-ktopl = wa_ska1-ktopl.
    wa_saida-erdat = wa_ska1-erdat.
    wa_saida-land1 = wa_t001-land1.
    wa_saida-txt20 = wa_skat-txt20.

    APPEND wa_saida TO it_saida.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Exibe o relátorio ALV com os dados
*----------------------------------------------------------------------*
FORM zf_display_alv .

  wa_fieldcat-fieldname = 'bukrs'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Empresa'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'butxt'.
  wa_fieldcat-seltext_l = 'Denominação da firma ou empresa'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'saknr'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Nº conta do Razão'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'ktopl'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Plano de contas'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'erdat'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Data de criação do registro'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'land1'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'País'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'txt20'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Texto das contas do Razão'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_layout-colwidth_optimize = abap_true.
  wa_layout-zebra = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      i_grid_title       = 'Empresa'
      is_layout          = wa_layout "Aqui passa o Layout do ALV
      it_fieldcat        = it_fieldcat "Aqui é passado a estrutura do ALV
    TABLES
      t_outtab           = it_saida. "Aqui passa os valores a serem exibidos no ALV

  IF sy-subrc NE 0.
    MESSAGE s208(001) DISPLAY LIKE sy-abcde+4(1) WITH text-e02.
  ENDIF.
ENDFORM.