*&---------------------------------------------------------------------------------------*
*& REPORT zpr_exec_23_bhs.
*&---------------------------------------------------------------------------------------*
*& Nome:zpr_exec_23_bhs
*& Tipo: Report
*& Objetivo: Elaborar um programa ABAP onde deverão ser selecionados na tabela VBAK 
*& as ordens de venda criadas no mês 02/2008, retornando os campos Documento de 
*& vendas, Data de criação do registro, Tipo de documento de vendas e Emissor da ordem. 
*& Para cada ordem de venda encontrada na tabela VBAK selecionar na tabela 
*& VBAP seus itens onde o campo Documento de vendas relaciona as duas tabelas, 
*& retornando os campos Documento de vendas, Item do documento de vendas, Nº do 
*& material, Quantidade da ordem acumulada em unidade de venda e Valor líquido do item 
*& da ordem na moeda do documento. Imprimir todos os itens de cada ordem de venda. 
*& Na impressão do resultado, efetuar uma quebra no relatório pelo campo Emissor 
*& da Ordem, onde deverá ser impresso a quantidade de ordens encontrada para cada um 
*& dos emissores selecionados. 
*& Imprimir os campos: Emissor da ordem, Tipo de documento de vendas, Data de 
*& criação do registro, Documento de vendas, Item do documento de vendas, Nº do 
*& material, Quantidade da ordem acumulada em unidade de venda e Valor líquido do item 
*& da ordem na moeda do documento.
*&---------------------------------------------------------------------------------------*

REPORT zpr_exec_23_bhs.

*&-------------------------*
*& Tables
*&-------------------------*
TABLES: vbak.

*&-------------------------*
*& Types
*&-------------------------*
TYPES: BEGIN OF ty_vbak,
         vbeln TYPE vbak-vbeln,
         erdat TYPE vbak-erdat,
         auart TYPE vbak-auart,
         kunnr TYPE vbak-kunnr,
       END OF ty_vbak.

TYPES: BEGIN OF ty_vbap,
         vbeln  TYPE vbap-vbeln,
         posnr  TYPE vbap-posnr,
         matnr  TYPE vbap-matnr,
         kwmeng TYPE vbap-kwmeng,
         netwr  TYPE vbap-netwr,
       END OF ty_vbap.

TYPES: BEGIN OF ty_saida,
         kunnr  TYPE vbak-kunnr,
         ordens TYPE i,
         vbeln  TYPE vbak-vbeln,
         erdat  TYPE vbak-erdat,
         auart  TYPE vbak-auart,
         posnr  TYPE vbap-posnr,
         matnr  TYPE vbap-matnr,
         kwmeng TYPE vbap-kwmeng,
         netwr  TYPE vbap-netwr,
       END OF ty_saida.

*&-------------------------*
*& Variables
*&-------------------------*
DATA: gv_count TYPE i VALUE 0.

*&-------------------------*
*& Work Area
*&-------------------------*
DATA: wa_vbak TYPE ty_vbak.
DATA: wa_vbap TYPE ty_vbap.
DATA: wa_saida TYPE ty_saida.
DATA: wa_sort TYPE slis_sortinfo_alv.
DATA: wa_fieldcat TYPE slis_fieldcat_alv. "Passará a formatação das colunas
DATA: wa_layout   TYPE slis_layout_alv. "Pode passar configurações de layout da tabela

*&-------------------------*
*& Internal Table
*&-------------------------*
DATA: it_fieldcat TYPE slis_t_fieldcat_alv. "Guardará a formatação das colunas
DATA: it_sort TYPE slis_t_sortinfo_alv.
DATA: it_vbak TYPE TABLE OF ty_vbak.
DATA: it_vbap TYPE TABLE OF ty_vbap.
DATA: it_saida TYPE TABLE OF ty_saida.

*&-------------------------*
*& Performs
*&-------------------------*
PERFORM: zf_select_data.
PERFORM: zf_process_data.
PERFORM: zf_sort.
PERFORM: zf_display_data.

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Seleciona os dados.
*----------------------------------------------------------------------*
FORM zf_select_data .

  SELECT
    vbeln
    erdat
    auart
    kunnr
    FROM vbak
    INTO TABLE it_vbak
    WHERE
    erdat LIKE '200802%'.

  SELECT
    vbeln
    posnr
    matnr
    kwmeng
    netwr
    FROM vbap
    INTO TABLE it_vbap
    FOR ALL ENTRIES IN it_vbap
    WHERE
    vbeln = it_vbap-vbeln.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Processa os dados.
*----------------------------------------------------------------------*
FORM zf_process_data .

  "Cria Veriáveis Locais.
  DATA: lv_kunnr       TYPE vbak-kunnr.
  DATA: lv_order_count TYPE i.

  lv_order_count = 1.

  "Ordena os registros da tabela interna.
  SORT it_vbak BY kunnr.

  LOOP AT it_vbak INTO wa_vbak.

    IF sy-tabix <> 1.
      lv_order_count = lv_order_count + 1.
    ENDIF.

    IF lv_kunnr NE wa_vbak-kunnr.

      IF sy-tabix <> 1.
        lv_order_count = 1.
      ENDIF.

      lv_kunnr = wa_vbak-kunnr.

    ENDIF.

*    IF wa_vbak-kunnr <> lv_kunnr. "Verifica se o valor atual da work area é direfente do valor da variável atual.
*      lv_kunnr = wa_vbak-kunnr. "Passa o valor da work area para a variável.
*      lv_order_count = 0. "Zera o contador.
*    ELSE.
*      ADD 1 TO  lv_order_count. "Adiciona + 1 no contador.
*    ENDIF.

    READ TABLE it_vbap INTO wa_vbap
      WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH. "Lê a tabela interna conforma os registros sejam do mesmo registro da tabela mestre.

    IF sy-subrc = 0.
      CLEAR wa_saida."Limpa a work area.

      " Preenche a Work Area.
      wa_saida-kunnr  = wa_vbak-kunnr.
      wa_saida-vbeln  = wa_vbak-vbeln.
      wa_saida-erdat  = wa_vbak-erdat.
      wa_saida-auart  = wa_vbak-auart.
      wa_saida-posnr  = wa_vbap-posnr.
      wa_saida-matnr  = wa_vbap-matnr.
      wa_saida-kwmeng = wa_vbap-kwmeng.
      wa_saida-netwr  = wa_vbap-netwr.
      wa_saida-ordens = lv_order_count.

      "Preenche os valores da tabela interna.
      APPEND wa_saida TO it_saida.
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Form para o sort.
*----------------------------------------------------------------------*
FORM zf_sort.

* ordenação primária por 'kunnr' em ordem crescente
  CLEAR wa_sort.
  wa_sort-spos = 1.           " Primeira ordenação
  wa_sort-fieldname = 'kunnr'.
  wa_sort-tabname = 'it_saida'.
  wa_sort-up = 'X'.
  APPEND wa_sort TO it_sort.

* Ordenação secundária por 'ordens' em ordem crescente
  CLEAR wa_sort.
  wa_sort-spos = 2.           " Segunda ordenação
  wa_sort-fieldname = 'ordens'.
  wa_sort-tabname = 'it_saida'.
  wa_sort-up = 'X'.
  APPEND wa_sort TO it_sort.

* Habilitar subtotal para 'kunnr'
  READ TABLE it_fieldcat INTO wa_fieldcat
    WITH KEY fieldname = 'ordens'.
  IF sy-subrc = 0.
    wa_fieldcat-do_sum = 'X'.
    wa_fieldcat-sp_group = 'X'.
    MODIFY it_fieldcat FROM wa_fieldcat INDEX sy-tabix.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Exibe os dados no ALV.
*----------------------------------------------------------------------*
FORM zf_display_data .

  DATA: lo_alv_grid TYPE REF TO cl_gui_alv_grid.
  DATA: lt_data     TYPE TABLE OF ty_saida.

  wa_fieldcat-fieldname = 'kunnr'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Emissor'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'ordens'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Quantidade de Ordens'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'vbeln'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Doc.vendas'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'erdat'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Data de criação do registro'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'auart'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Tipo de documento de vendas'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'posnr'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Item do documento de vendas'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'matnr'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Nº do material'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'kwmeng'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Quantidade da ordem acumulada em unidade de venda'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'netwr'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Valor líquido da ordem na moeda do documento'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_layout-colwidth_optimize = abap_true.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
*     i_callback_user_command  = 'USER_COMMAND'
*     i_callback_pf_status_set = 'SUB_PF_STATUS'
      i_grid_title       = ' '
      it_sort            = it_sort "Tabela interna com a config do sort.
      is_layout          = wa_layout "Aqui passa o Layout do ALV
      it_fieldcat        = it_fieldcat "Aqui é passado a estrutura do ALV
    TABLES
      t_outtab           = it_saida. "Aqui passa os valores a serem exibidos no ALV

  IF sy-subrc NE 0.
    MESSAGE s208(00) DISPLAY LIKE sy-abcde+4(1) WITH text-e02.
  ENDIF.
ENDFORM.