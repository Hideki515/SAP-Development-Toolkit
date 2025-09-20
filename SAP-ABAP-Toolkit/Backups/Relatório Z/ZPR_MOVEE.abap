*&---------------------------------------------------------------------*
*& Report ZPR_MOVEE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpr_movee.

***************
***	TABELAS	***
***************
TABLES: mara, eine, eina, lfa1.

***************
***	 TYPES  ***
***************
TYPES: BEGIN OF ty_dados,
  matnr TYPE mara-matnr,
  mtart TYPE mara-mtart,
  matkl TYPE mara-matkl,
  mstae TYPE mara-mstae,
  meins TYPE mara-meins,
  werks TYPE eine-werks,
  ekgrp TYPE eine-ekgrp,
  ekorg TYPE eine-ekorg,
  mwskz TYPE eine-mwskz,
  netpr TYPE eine-netpr,
  waers TYPE eine-waers,
  ebeln TYPE eine-ebeln,
  datlb TYPE eine-datlb,
  infnr TYPE eina-infnr,
  erdat TYPE eina-erdat,
  lifnr TYPE lfa1-lifnr,
  stcd1 TYPE lfa1-stcd1,
  name1 TYPE lfa1-name1,
  maktx  TYPE makt-maktx,
  END OF ty_dados.

*************************
***	 INTERNAL TABLES  ***
*************************
DATA: it_dados TYPE TABLE OF ty_dados.

*******************
*** WORK AREAS  ***
*******************
DATA: wa_dados TYPE ty_dados.

***************************************
*** PARAMETROS DE SELEÇÃO DE DADOS  ***
***************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
  SELECT-OPTIONS: s_matnr FOR mara-matnr,
                  s_mtart FOR mara-mtart,
                  s_werks FOR eine-werks,
                  s_matkl FOR mara-matkl,
                  s_mstae FOR mara-mstae,
                  s_infnr FOR eina-infnr,
                  s_lifnr FOR lfa1-lifnr,
                  s_cnpj  FOR lfa1-stcd1,
                  s_ekgrp FOR eine-ekgrp,
                  s_ekorg FOR eine-ekorg,
                  s_mwskz FOR eine-mwskz.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
PERFORM zf_select_data.

FORM zf_select_data.

  " Select data from EINA table
  SELECT DISTINCT
    infnr,
    erdat,
    lifnr,
    matnr
  FROM eina
  INTO TABLE @DATA(lt_eina)
  WHERE infnr IN @s_infnr.

  IF lt_eina IS NOT INITIAL.

    " Select data from MARA table
    SELECT
      matnr,
      mtart,
      mstae,
      matkl,
      meins
    FROM mara
    INTO TABLE @DATA(lt_mara)
    FOR ALL ENTRIES IN @lt_eina
    WHERE
      matnr = @lt_eina-matnr AND
      mtart IN @s_mtart AND
      matnr IN @s_matnr AND
      matkl IN @s_matkl AND
      mstae IN @s_mstae.

    " Select data from LFA1 table
    SELECT
      lifnr,
      name1,
      stcd1
    FROM lfa1
    INTO TABLE @DATA(lt_lfa1)
    FOR ALL ENTRIES IN @lt_eina
    WHERE
      lifnr = @lt_eina-lifnr AND
      lifnr IN @s_lifnr AND
      stcd1 IN @s_cnpj.

    " Select data from EINE table
    SELECT
      infnr,
      ekgrp,
      ekorg,
      mwskz,
      werks,
      netpr,
      waers,
      ebeln,
      datlb
    FROM eine
    INTO TABLE @DATA(lt_eine)
    FOR ALL ENTRIES IN @lt_eina
    WHERE
      infnr = @lt_eina-infnr AND
      werks IN @s_werks AND
      ekgrp IN @s_ekgrp AND
      ekorg IN @s_ekorg.

    " Select data from MAKT table
    SELECT
      matnr,
      spras,
      maktx
    FROM makt
    INTO TABLE @DATA(lt_makt)
    FOR ALL ENTRIES IN @lt_eina
    WHERE matnr = @lt_eina-matnr AND spras = 'P'.

    LOOP AT lt_eina INTO DATA(lw_eina).
      READ TABLE lt_mara INTO DATA(lw_mara) WITH KEY matnr = lw_eina-matnr BINARY SEARCH.
      READ TABLE lt_lfa1 INTO DATA(lw_lfa1) WITH KEY lifnr = lw_eina-lifnr BINARY SEARCH.
      READ TABLE lt_eine INTO DATA(lw_eine) WITH KEY infnr = lw_eina-infnr BINARY SEARCH.
      READ TABLE lt_makt INTO DATA(lw_makt) WITH KEY matnr = lw_eina-matnr BINARY SEARCH.

      CLEAR wa_dados.

      " MARA
      wa_dados-matnr = lw_mara-matnr.
      wa_dados-mtart = lw_mara-mtart.
      wa_dados-matkl = lw_mara-matkl.
      wa_dados-mstae = lw_mara-mstae.
      wa_dados-meins = lw_mara-meins.

      " EINE
      wa_dados-werks = lw_eine-werks.
      wa_dados-ekgrp = lw_eine-ekgrp.
      wa_dados-ekorg = lw_eine-ekorg.
      wa_dados-mwskz = lw_eine-mwskz.
      wa_dados-netpr = lw_eine-netpr.
      wa_dados-waers = lw_eine-waers.
      wa_dados-ebeln = lw_eine-ebeln.
      wa_dados-datlb = lw_eine-datlb.

      " EINA
      wa_dados-infnr = lw_eina-infnr.
      wa_dados-erdat = lw_eina-erdat.

      " LFA1
      wa_dados-lifnr = lw_lfa1-lifnr.
      wa_dados-stcd1 = lw_lfa1-stcd1.
      wa_dados-name1 = lw_lfa1-name1.

      " MAKT
      wa_dados-maktx = lw_makt-maktx.

      APPEND wa_dados TO it_dados.

    ENDLOOP.

  ENDIF.

  PERFORM zf_display_alv.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form zf_display_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM zf_display_alv .
  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv.
  DATA: lw_fieldcat TYPE LINE OF slis_t_fieldcat_alv.
  DATA: lw_layout   TYPE slis_layout_alv.

  lw_fieldcat-fieldname = 'matnr'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Material'. "Nome queW vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'mtart'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Tipo do Material'. "Nome queW vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'werks'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Centro'. "Nome queW vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'matkl'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Grupo de Mercadorias'. "Nome queW vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'msatae'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Status do Material'. "Nome queW vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'infnr'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Registro Info'. "Nome queW vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'lifnr'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Fornecedor'. "Nome queW vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'stcd1'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'CNPJ'. "Nome queW vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'ekgrp'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Grupo de Compradores'. "Nome queW vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'ekorg'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Organização de Compras'. "Nome queW vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'mwskz'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Código'. "Nome queW vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'erdat'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Data de Criação'. "Nome queW vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'name1'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Descrição'. "Nome queW vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'maktx'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Descrição'. "Nome queW vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'mtart'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Tipo'. "Nome queW vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'mstae'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Status'. "Nome queW vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'mwskz'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'IVA'. "Nome queW vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'meins'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'UMB'. "Nome queW vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'netpr'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Preço Liquido'. "Nome queW vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'waers'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Moeda'. "Nome queW vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'ebeln'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Doc. Compra'. "Nome queW vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'datlb'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Data Pedido'. "Nome queW vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_layout-colwidth_optimize = abap_true.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      i_grid_title       = 'Relatorio Z'
*     i_save             = sy-abcde(1) "Utilizado para salvar o layout do ALV
*     is_variant         = gv_variant  "Utilizado para salvar o layout do ALV
      is_layout          = lw_layout "Aqui passa o Layout do ALV
      it_fieldcat        = lt_fieldcat "Aqui é passado a estrutura do ALV
    TABLES
      t_outtab           = it_dados. "Aqui passa os valores a serem exibidos no ALV

  IF sy-subrc NE 0.
    MESSAGE s208(00) DISPLAY LIKE sy-abcde+4(1) WITH TEXT-e02.
  ENDIF.
ENDFORM.