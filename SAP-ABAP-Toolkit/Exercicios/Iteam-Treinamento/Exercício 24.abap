*&---------------------------------------------------------------------------------------*
*& REPORT zpr_exec_24_bhs.
*&---------------------------------------------------------------------------------------*
*& Nome:zpr_exec_24_bhs
*& Tipo: Report
*& Objetivo: Elaborar um programa ABAP onde deverá ser criada uma tela de seleção com
*& o campo Centro como seleção múltipla e o campo Chave do calendário de fábrica como
*& seleção única com o valor default ‘BR’. Os tipos dos campos podem ser encontrados na
*& tabela T001W.
*& Selecionar na tabela T001W todos os Centros que estiverem de acordo com o
*& campo Centro da tela de seleção e que também estejam de acordo com o campo Chave
*& do calendário de fábrica da tela de seleção, retornando os campos Centro, Nome 1, País
*& e Região.
*& Para cada Centro encontrado na tabela T001W, selecionar na tabela MARC os
*& materiais que foram ampliados para este centro onde o campo Centro relaciona as duas
*& tabelas, retornando os campos Nº do material e Centro.
*& Para cada Nº do material encontrado na tabela MARC, selecionar na tabela MAKT
*& sua Denominação desde que estejam no Idioma ‘PT’.
*& O campo Nº do material relaciona as duas tabelas, retornando os campos Nº do
*& material e Denominação.
*& O relatório deverá imprimir os dados de cada centro bem como todos os materiais
*& encontrados para cada centro sua denominação.
*& Ao final da impressão dos materiais deverá ser impressa no relatório a quantidade
*& de materiais encontrados para cada um dos centros selecionados.
*& Imprimir os campos Centro, Nome 1, País, Região, Nº do material e Denominação.
*&---------------------------------------------------------------------------------------*

REPORT zpr_exec_24_bhs.

*&-------------------------*
*& Tables
*&-------------------------*
TABLES: t001w.

*&-------------------------*
*& Types
*&-------------------------*
TYPES: BEGIN OF ty_t001w,
         werks TYPE t001w-werks,
         fabkl TYPE t001w-fabkl,
         name1 TYPE t001w-name1,
         regio TYPE t001w-regio,
         land1 TYPE t001w-land1,
       END OF ty_t001w.

TYPES: BEGIN OF ty_marc,
         werks TYPE marc-werks,
         matnr TYPE marc-matnr,
       END OF ty_marc.

TYPES: BEGIN OF ty_makt,
         matnr TYPE makt-mandt,
         maktx TYPE makt-maktx,
         spras TYPE makt-spras,
       END OF ty_makt.

TYPES: BEGIN OF ty_saida,
         werks TYPE t001w-werks,
         name1 TYPE t001w-name1,
         regio TYPE t001w-regio,
         land1 TYPE t001w-land1,
         matnr TYPE marc-matnr,
         maktx TYPE makt-maktx,
         count TYPE i,
       END OF ty_saida.

*&-------------------------*
*& Work Area
*&-------------------------*
DATA: wa_t001w TYPE ty_t001w.
DATA: wa_marc TYPE ty_marc.
DATA: wa_makt TYPE ty_makt.
DATA: wa_saida TYPE ty_saida.
DATA: wa_fieldcat TYPE slis_fieldcat_alv. "Passará a formatação das colunas
DATA: wa_layout   TYPE slis_layout_alv. "Pode passar configurações de layout da tabela

*&-------------------------*
*& Internal Table
*&-------------------------*
DATA: it_fieldcat TYPE slis_t_fieldcat_alv. "Guardará a formatação das colunas
DATA: it_t001w TYPE TABLE OF ty_t001w.
DATA: it_marc TYPE TABLE OF ty_marc.
DATA: it_makt TYPE TABLE OF ty_makt.
DATA: it_saida TYPE TABLE OF ty_saida.

*&-------------------------*
*& Paramiters Selections
*&-------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

SELECT-OPTIONS: s_werks FOR  t001w-werks,
                s_fabkl FOR t001w-fabkl NO INTERVALS NO-EXTENSION DEFAULT 'BR'.

SELECTION-SCREEN END OF BLOCK b1.

*&-------------------------*
*& Perform
*&-------------------------*
PERFORM zf_select_data.
PERFORM zf_process_data.
PERFORM zf_display_alv.

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Seleciona os dados.
*----------------------------------------------------------------------*
FORM zf_select_data .

  SELECT
    werks
    fabkl
    name1
    regio
    land1
    FROM t001w
    INTO TABLE it_t001w
    WHERE
    werks IN s_werks AND
    fabkl IN s_fabkl.

  SELECT
    werks
      matnr
    FROM marc
    INTO TABLE it_marc
    FOR ALL ENTRIES IN it_t001w
    WHERE
    werks = it_t001w-werks.

  SELECT
    matnr
    maktx
    spras
    FROM makt
    INTO TABLE it_makt
    FOR ALL ENTRIES IN it_marc
    WHERE
    matnr = it_marc-matnr AND
    spras = 'PT'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Processa os dados da tabela interna.
*----------------------------------------------------------------------*
FORM zf_process_data .

  "Ordenação das tabelas.
  SORT it_marc BY werks.
  SORT it_makt BY matnr.

  LOOP AT it_t001w INTO wa_t001w. "Laço de repetição da tabela inerna.
    READ TABLE it_marc INTO wa_marc WITH KEY werks = wa_t001w-werks BINARY SEARCH. "Le os valores do laço atual para uma Work Area.
    IF sy-subrc = 0.
      READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_marc-matnr BINARY SEARCH. "Le os valores do laço atual para uma Work Area.
    ENDIF.

    IF sy-subrc = 0. "Verifica se não ocorreu erro.

      CLEAR wa_saida. "Limpa os valores da Work Area.

      "Passa os valores das Work Area para a Work Area de saída.
      wa_saida-werks = wa_t001w-werks.
      wa_saida-name1 = wa_t001w-name1.
      wa_saida-regio = wa_t001w-regio.
      wa_saida-land1 = wa_t001w-land1.
      wa_saida-matnr = wa_marc-matnr.
      wa_saida-maktx = wa_makt-maktx.

      "Passa os valores da Work Area para a tabela interna.
      APPEND wa_saida TO it_saida.

    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Exibe os valores da tabela interna.
*----------------------------------------------------------------------*
FORM zf_display_alv .
* Cria os variáveis locais.
  DATA: lo_alv_grid TYPE REF TO cl_gui_alv_grid.
  DATA: lt_data     TYPE TABLE OF ty_saida.

  wa_fieldcat-fieldname = 'werks'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Centro'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'name1'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Nome 1'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'regio'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Região'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'land1'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'País'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'matnr'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Nº do material'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'maktx'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Denominação'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_layout-colwidth_optimize = abap_true.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
*     i_callback_user_command  = 'USER_COMMAND'
*     i_callback_pf_status_set = 'SUB_PF_STATUS'
      i_grid_title       = ' '
*     it_sort            = it_sort "Tabela interna com a config do sort.
      is_layout          = wa_layout "Aqui passa o Layout do ALV
      it_fieldcat        = it_fieldcat "Aqui é passado a estrutura do ALV
    TABLES
      t_outtab           = it_saida. "Aqui passa os valores a serem exibidos no ALV

  IF sy-subrc NE 0.
    MESSAGE s208(001) DISPLAY LIKE sy-abcde+4(1) WITH text-e02.
  ENDIF.

ENDFORM.