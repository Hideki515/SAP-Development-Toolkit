*&---------------------------------------------------------------------------------------*
*& REPORT zpr_exec_22_bhs.
*&---------------------------------------------------------------------------------------*
*& Nome:zpr_exec_22_bhs
*& Tipo: Report
*& Objetivo:   Elaborar um programa ABAP onde deverão ser selecionados na tabela EKKO 
*& os pedidos onde o campo Tipo de Documento de Compras = ‘NB’, retornando os 
*& campos Número do Documento de Compras, Empresa, Data de criação do registro, 
*& Numero Fornecedor, Organização de Compras e Grupo de Compradores. 
*& Para cada pedido encontrado na tabela EKKO selecionar na tabela EKPO apenas 
*& itens em que o campo Material iniciar por ‘T’, onde o Numero do Documento de Compras 
*& relaciona as duas tabelas, retornando os campos Numero do Documento de Compras, 
*& Nº item do documento de compra, Nº do material e Centro. Só imprimir pedidos que 
*& atendam a esta condição. 
*& Na impressão do resultado, efetuar uma quebra no relatório por empresa, onde 
*& deverá ser impresso a quantidade de pedidos encontrada para cada uma das empresas 
*& selecionadas.
*& Imprimir os campos Numero do Documento de Compras, Empresa, Data de 
*& criação do registro, Numero Fornecedor, Organização de Compras e Grupo de 
*& Compradores, Nº item do documento de compra, Nº do material e Centro. 
*&---------------------------------------------------------------------------------------*

REPORT zpr_exec_22_bhs.

*&-------------------------*
*& Tables
*&-------------------------*
TABLES ekko.

*&-------------------------*
*& Types
*&-------------------------*
TYPES: BEGIN OF ty_ekko,
         bsart TYPE ekko-bsart,
         ebeln TYPE ekko-ebeln,
         bukrs TYPE ekko-bukrs,
         bedat TYPE ekko-bedat,
         lifnr TYPE ekko-lifnr,
         ekorg TYPE ekko-ekorg,
         ekgrp TYPE ekko-ekgrp,
       END OF ty_ekko.

TYPES: BEGIN OF ty_ekpo,
         ebeln TYPE ekpo-ebeln,
         ebelp TYPE ekpo-ebelp,
         matnr TYPE ekpo-matnr,
         werks TYPE ekpo-werks,
       END OF ty_ekpo.

TYPES: BEGIN OF ty_saida,
         bukrs TYPE ekko-bukrs,
         bsart TYPE ekko-bsart,
         ebeln TYPE ekko-ebeln,
         bedat TYPE ekko-bedat,
         lifnr TYPE ekko-lifnr,
         ekorg TYPE ekko-ekorg,
         ekgrp TYPE ekko-ekgrp,
         ebelp TYPE ekpo-ebelp,
         matnr TYPE ekpo-matnr,
         werks TYPE ekpo-werks,
       END OF ty_saida.

*&-------------------------*
*& Work Area
*&-------------------------*
DATA: wa_saida TYPE ty_saida.
DATA: wa_ekko TYPE ty_ekko.
DATA: wa_ekpo TYPE ty_ekpo.
DATA: wa_sort TYPE slis_sortinfo_alv.

*&-------------------------*
*& Internal Tables
*&-------------------------*
DATA: it_sort TYPE slis_t_sortinfo_alv.
DATA: it_ekko TYPE TABLE OF ty_ekko.
DATA: it_ekpo TYPE TABLE OF ty_ekpo.
DATA: it_saida TYPE TABLE OF ty_saida.

*&-------------------------*
*& Performs
*&-------------------------*
PERFORM zf_select_data.
PERFORM zf_process_data.
PERFORM zf_sort_alv.
PERFORM zf_display_alv.

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  Seleciona os dados.
*----------------------------------------------------------------------*
FORM zf_select_data .

  SELECT
    bsart
    ebeln
    bukrs
    bedat
    lifnr
    ekorg
    ekgrp
    FROM ekko
    INTO TABLE it_ekko
    WHERE
    bsart = 'NB'
    ORDER BY bukrs.

  IF it_ekko IS NOT INITIAL.
    SELECT
      ebeln
      ebelp
      matnr
      werks
      FROM ekpo
      INTO TABLE it_ekpo
      FOR ALL ENTRIES IN it_ekko
      WHERE matnr LIKE 'T%' AND
      ebeln = it_ekko-ebeln. "Verifica se id é o mesmo id da outra tabela.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  Processa os dados e passa eles para seus respectivos campos da work
* area da tabela interna de saída.
*----------------------------------------------------------------------*
FORM zf_process_data .

  LOOP AT it_ekko INTO wa_ekko. "Realiza um loop na tabela ekko e passa os valores para uma work area.
    READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = wa_ekko-ebeln. "Le e compara o campo chave para verificar se são o mesmo para confirmar se são o mesmo documento.

    IF sy-subrc = 0. "Verifica se não ocorreu erro.
      
      CLEAR wa_saida. "Limpa os valores da work area.
      
      wa_saida-bukrs = wa_ekko-bukrs.
      wa_saida-bsart = wa_ekko-bsart.
      wa_saida-ebeln = wa_ekko-ebeln.
      wa_saida-bedat = wa_ekko-bedat.
      wa_saida-lifnr = wa_ekko-lifnr.
      wa_saida-ekorg = wa_ekko-ekorg.
      wa_saida-ekgrp = wa_ekko-ekgrp.
      wa_saida-ebelp = wa_ekpo-ebelp.
      wa_saida-matnr = wa_ekpo-matnr.
      wa_saida-werks = wa_ekpo-werks.

      APPEND wa_saida TO it_saida. "Passa os valores da work area para a tabela interna.

    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_SORT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  Faz a junção dos campos com o mesmo valor.
*----------------------------------------------------------------------*
FORM zf_sort_alv.
  
  wa_sort-spos = 1.
  wa_sort-fieldname = 'bukrs'.
  wa_sort-tabname = 'it_saida'.
  wa_sort-up = 'X'.
  APPEND wa_sort TO it_sort.
  CLEAR wa_sort.
  
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  zf_display_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  Monta e exibe o relátorio ALV.
*----------------------------------------------------------------------*
FORM zf_display_alv .
  DATA: lw_fieldcat TYPE slis_fieldcat_alv. "Passará a formatação das colunas
  DATA: lw_layout   TYPE slis_layout_alv. "Pode passar configurações de layout da tabela
  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv. "Guardará a formatação das colunas

  lw_fieldcat-fieldname = 'bukrs'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Empresa'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'bsart'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Tipo documento'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'ebeln'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Doc.compras'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'bedat'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Data documento'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'lifnr'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Fornecedor'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'ekorg'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Org.compras'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'ekgrp'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Grupo compras'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'ebelp'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Item'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'matnr'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Material'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'werks'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Centro'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_layout-colwidth_optimize = abap_true.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
*     i_callback_user_command  = 'USER_COMMAND'
*     i_callback_pf_status_set = 'SUB_PF_STATUS'
      i_grid_title       = ' '
      it_sort            = it_sort "Tabela interna com a config do sort.
      is_layout          = lw_layout "Aqui passa o Layout do ALV
      it_fieldcat        = lt_fieldcat "Aqui é passado a estrutura do ALV
    TABLES
      t_outtab           = it_saida. "Aqui passa os valores a serem exibidos no ALV

  IF sy-subrc NE 0.
    MESSAGE s208(00) DISPLAY LIKE sy-abcde+4(1) WITH text-e02.
  ENDIF.

ENDFORM.