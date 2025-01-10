*&---------------------------------------------------------------------*
*& Report ZR_BHS_ALV_CLASSE
*&---------------------------------------------------------------------*
*& Estrutura Básica para o ALV de dois níveis por classe.
*&---------------------------------------------------------------------*
REPORT zr_bhs_alv_classe_duplo.

*&---------------------------------------------------------------------*
*& TABLES
*&---------------------------------------------------------------------*
TABLES: sflight.

*&---------------------------------------------------------------------*
*& TYPES
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_sflight,
         carrid     TYPE sflight-carrid,
         connid     TYPE sflight-connid,
         fldate     TYPE sflight-fldate,
         planetype  TYPE sflight-planetype,
         paymentsum TYPE sflight-paymentsum,
       END OF ty_sflight,

       BEGIN OF ty_scarr,
         carrid   TYPE scarr-carrid,
         carrname TYPE scarr-carrname,
         currcode TYPE scarr-currcode,
       END OF ty_scarr,

       BEGIN OF ty_sbook,
         carrid   TYPE sbook-carrid,
         connid   TYPE sbook-connid,
         fldate   TYPE sbook-fldate,
         bookid   TYPE sbook-bookid,
         class    TYPE sbook-class,
         passname TYPE sbook-passname,
       END OF ty_sbook,

       BEGIN OF ty_scustom,
         id   TYPE scustom-id,
         name TYPE scustom-name,
       END OF ty_scustom,

       BEGIN OF ty_spfli,
         carrid   TYPE spfli-carrid,
         connid   TYPE spfli-connid,
         cityfrom TYPE spfli-cityfrom,
         cityto   TYPE spfli-cityto,
         airpfrom TYPE spfli-airpfrom,
         airpto   TYPE spfli-airpto,
       END OF ty_spfli,

       BEGIN OF ty_saida,
         carrid     TYPE sflight-carrid,
         connid     TYPE sflight-connid,
         fldate     TYPE sflight-fldate,
         planetype  TYPE sflight-planetype,
         paymentsum TYPE sflight-paymentsum,
         carrname   TYPE scarr-carrname,
         cityfrom   TYPE spfli-cityfrom,
         cityto     TYPE spfli-cityto,
         airpfrom   TYPE spfli-airpfrom,
         airpto     TYPE spfli-airpto,
       END OF ty_saida,

       BEGIN OF ty_saida2,
         carrid   TYPE sbook-carrid,
         connid   TYPE sbook-connid,
         fldate   TYPE sbook-fldate,
         bookid   TYPE sbook-bookid,
         class    TYPE sbook-class,
         passname TYPE sbook-passname,
       END OF ty_saida2.

*&---------------------------------------------------------------------*
*& INTERNAL TABLE
*&---------------------------------------------------------------------*
DATA:
  t_sflight TYPE TABLE OF ty_sflight,
  t_scarr   TYPE TABLE OF ty_scarr,
  t_spfli   TYPE TABLE OF ty_spfli,
  t_sbook   TYPE TABLE OF ty_sbook,
  t_scustom TYPE TABLE OF ty_scustom,
  t_saida   TYPE TABLE OF ty_saida,
  t_saida2  TYPE TABLE OF ty_saida2,
  t_rows    TYPE lvc_t_row.

*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*
DATA:
  w_sflight TYPE ty_sflight,
  w_scarr   TYPE ty_scarr,
  w_spfli   TYPE ty_spfli,
  w_sbook   TYPE ty_sbook,
  w_scustom TYPE ty_scustom,
  w_saida   TYPE ty_saida,
  w_saida2  TYPE ty_saida2.

*&---------------------------------------------------------------------*
*& VARIABLES
*&---------------------------------------------------------------------*
DATA: v_cont_alv    TYPE REF TO cl_gui_custom_container,
      v_grid_alv    TYPE REF TO cl_gui_alv_grid,
      v_grid_saida2 TYPE REF TO cl_gui_alv_grid,
      v_variant     TYPE disvariant.

*&---------------------------------------------------------------------*
*& CLASS
*&---------------------------------------------------------------------*
CLASS:
  lcl_event_reciver DEFINITION DEFERRED.

*&---------------------------------------------------------------------*
*& CLASS LCL_EVENT_RECIVER DEFINITION
*&---------------------------------------------------------------------*
*& Definição dos métodos a serem utilizados.
*&---------------------------------------------------------------------*
CLASS lcl_event_reciver DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
      handle_double_click_alv
                    FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no,

      handle_toolbar
                    FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,

      handle_menu_button
                    FOR EVENT menu_button OF cl_gui_alv_grid
        IMPORTING e_object e_ucomm,

      handle_user_command
                    FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.

*&---------------------------------------------------------------------*
*& CLASS LCL_EVENT_RECIVER IMPLEMENTATION
*&---------------------------------------------------------------------*
*& Implementação dos métodos a serem utilizados.
*&---------------------------------------------------------------------*
CLASS lcl_event_reciver IMPLEMENTATION.

  METHOD handle_double_click_alv.

    PERFORM: zf_prenche_saida2 USING es_row_no-row_id,
             zf_atualiza_alv CHANGING v_grid_saida2.

  ENDMETHOD.

  METHOD handle_toolbar.

    DATA: w_toolbar TYPE stb_button.

    CLEAR: w_toolbar.
    w_toolbar-butn_type = 3.
    APPEND w_toolbar TO e_object->mt_toolbar.

    CLEAR: w_toolbar.
    w_toolbar-function = 'REPROCESSAR'.
    w_toolbar-icon = icon_refresh.
    w_toolbar-quickinfo = text-c01. "
    w_toolbar-butn_type = 2.
    w_toolbar-disabled = ' '.
    APPEND w_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.

  METHOD handle_menu_button.

    IF e_ucomm = 'REPROCESSAR'.

      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'TESTE1'
          text  = text-c02. " TESTE1
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'TESTE2'
          text  = text-c03. " TESTE2
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'TESTE3'
          text  = text-c04. " TESTE3


    ENDIF.

  ENDMETHOD.

  METHOD handle_user_command.

    CALL METHOD v_grid_alv->get_selected_rows
      IMPORTING
        et_index_rows = t_rows.
    CALL METHOD cl_gui_cfw=>flush.

    IF sy-subrc <> 0.

      MESSAGE 'Erro ao pegar linhas selecionadas' TYPE 'E' DISPLAY LIKE 'E'.

    ENDIF.

    CASE e_ucomm.

      WHEN 'TESTE1'.
        PERFORM zf_escrita USING t_rows.
*        MESSAGE 'Botão 1 Pressionado' TYPE 'S'.

      WHEN 'TESTE2'.
*        PERFORM zf_ocorrencia TABLES t_rows.
        MESSAGE 'Botão 2 Pressionado' TYPE 'S'.

      WHEN 'TESTE3'.
*        PERFORM zf_cte TABLES t_rows.
        MESSAGE 'Botão 3 Pressionado' TYPE 'S'.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*& SELECTION SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.

SELECT-OPTIONS: s_carrid FOR sflight-carrid,
s_fldate FOR sflight-fldate.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t02.

PARAMETERS: p_layout TYPE slis_vari.

SELECTION-SCREEN END OF BLOCK b2.

*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON p_layout.
  PERFORM zf_carrega_layout CHANGING v_variant.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  PERFORM zf_matchcode_layout CHANGING v_variant.


*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM zf_selecao_dados.

END-OF-SELECTION.
  PERFORM zf_processamento_dados.

  CALL SCREEN 9000.

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECAO_DADOS
*&---------------------------------------------------------------------*
*       Seleção dos dados conforme os parâmetros de seleção.
*----------------------------------------------------------------------*
FORM zf_selecao_dados.

  SELECT carrid
         connid
         fldate
         planetype
         paymentsum
  FROM sflight
  INTO TABLE t_sflight
  WHERE
  carrid IN s_carrid AND
  fldate IN s_fldate.

  IF sy-subrc <> 0.

    FREE t_sflight.
    LEAVE LIST-PROCESSING.

  ENDIF.

  IF t_sflight IS NOT INITIAL.

    SELECT carrid
           carrname
           currcode
    FROM scarr
    INTO TABLE t_scarr
    FOR ALL ENTRIES IN t_sflight
    WHERE
    carrid = t_sflight-carrid.

    IF sy-subrc <> 0.

      FREE t_scarr.

    ENDIF.

    SELECT carrid
           connid
           cityfrom
           cityto
           airpfrom
           airpto
    FROM spfli
    INTO TABLE t_spfli
    FOR ALL ENTRIES IN t_sflight
    WHERE
    carrid = t_sflight-carrid AND
    connid = t_sflight-connid.

    IF sy-subrc <> 0.

      FREE t_spfli.

    ENDIF.

    SELECT carrid
           connid
           fldate
           bookid
           class
           passname
      FROM sbook
      INTO TABLE t_sbook
      FOR ALL ENTRIES IN t_sflight
      WHERE
      carrid = t_sflight-carrid AND
      connid = t_sflight-connid AND
      fldate = t_sflight-fldate.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_PROCESSAMENTO_DADOS
*&---------------------------------------------------------------------*
*       Faz o processamento dos dados selecionados.
*----------------------------------------------------------------------*
FORM zf_processamento_dados .

  SORT t_scarr BY carrid.
  SORT t_spfli BY carrid connid.

  LOOP AT t_sflight INTO w_sflight.

    w_saida-carrid     = w_sflight-carrid.
    w_saida-connid     = w_sflight-connid.
    w_saida-fldate     = w_sflight-fldate.
    w_saida-planetype  = w_sflight-planetype.
    w_saida-paymentsum = w_sflight-paymentsum.

    READ TABLE t_scarr INTO w_scarr WITH KEY carrid = w_sflight-carrid BINARY SEARCH.
    IF sy-subrc = 0.

      w_saida-carrname = w_scarr-carrname.

    ENDIF.

    READ TABLE t_spfli INTO w_spfli WITH KEY carrid = w_sflight-carrid
                                             connid = w_sflight-connid BINARY SEARCH.
    IF sy-subrc = 0.

      w_saida-cityfrom = w_spfli-cityfrom.
      w_saida-cityto   = w_spfli-cityto.
      w_saida-airpfrom = w_spfli-airpfrom.
      w_saida-airpto   = w_spfli-airpto.

    ENDIF.

    APPEND w_saida TO t_saida.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_ALV
*&---------------------------------------------------------------------*
*       Monta o ALV.
*----------------------------------------------------------------------*
FORM zf_monta_alv .

  DATA:
        t_fieldcat TYPE TABLE OF lvc_s_fcat.

  DATA:
    w_variant TYPE disvariant,
    w_layout  TYPE lvc_s_layo.

  DATA:
    l_splitter    TYPE REF TO cl_gui_splitter_container,
    l_cont_alv    TYPE REF TO cl_gui_container,
    l_cont_saida2 TYPE REF TO cl_gui_container,
    l_event       TYPE REF TO lcl_event_reciver.

  IF v_cont_alv IS INITIAL.

    CREATE OBJECT v_cont_alv
      EXPORTING
        container_name = 'C_CTNR_ALV'.

    v_cont_alv->set_adjust_design(
    EXPORTING
      adjust_design = cl_gui_custom_container=>adust_design_true
    EXCEPTIONS
      cntl_error = 1
      cntl_system_error = 2
      OTHERS = 3 ).

    CREATE OBJECT l_splitter
      EXPORTING
        parent  = v_cont_alv
        rows    = 2
        columns = 1.

    CALL METHOD l_splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = l_cont_alv.

    CALL METHOD l_splitter->set_row_sash
      EXPORTING
        id    = 1
        type  = cl_gui_splitter_container=>type_movable
        value = cl_gui_splitter_container=>true.

    l_splitter->set_row_height( id = 1 height = 55 ).

    CREATE OBJECT v_grid_alv
      EXPORTING
        i_parent          = l_cont_alv
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    IF sy-subrc = 0.

      PERFORM zf_monta_fieldcat USING 'ZST_BM_SAIDA_SFLIGHT'
                                CHANGING t_fieldcat.

      CREATE OBJECT l_event.
      SET HANDLER: l_event->handle_double_click_alv FOR v_grid_alv,
                   l_event->handle_toolbar          FOR v_grid_alv,
                   l_event->handle_menu_button      FOR v_grid_alv,
                   l_event->handle_user_command     FOR v_grid_alv.

      CLEAR w_layout.
      w_layout-zebra = 'X'.
      w_layout-cwidth_opt = 'X'.
      w_layout-sel_mode = 'A'.

      CLEAR w_variant.
      w_variant-report  = sy-repid.
      w_variant-variant = p_layout.

      CALL METHOD v_grid_alv->set_table_for_first_display
        EXPORTING
          is_layout                     = w_layout
          i_save                        = 'X'
          is_variant                    = w_variant
        CHANGING
          it_outtab                     = t_saida
          it_fieldcatalog               = t_fieldcat
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.

      IF sy-subrc <> 0.

        MESSAGE text-e04 TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 0.

      ENDIF.

      CALL METHOD l_splitter->get_container
        EXPORTING
          row       = 2
          column    = 1
        RECEIVING
          container = l_cont_saida2.

      CREATE OBJECT v_grid_saida2
        EXPORTING
          i_parent          = l_cont_saida2
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.

      IF sy-subrc = 0.

        FREE t_fieldcat.

        PERFORM zf_monta_fieldcat USING 'ZST_BM_SAIDA_SBOOK' CHANGING t_fieldcat.

        CLEAR w_layout.
        w_layout-zebra = 'X'.
        w_layout-cwidth_opt = 'X'.
        w_layout-sel_mode = 'A'.

        CALL METHOD v_grid_saida2->set_table_for_first_display
          EXPORTING
            is_layout                     = w_layout
          CHANGING
            it_outtab                     = t_saida2
            it_fieldcatalog               = t_fieldcat
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4.

        IF sy-subrc <> 0.

          MESSAGE text-e04 TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE TO SCREEN 0.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_FIELDCAT
*&---------------------------------------------------------------------*
*       Monta a tabela fieldcat.
*----------------------------------------------------------------------*
FORM zf_monta_fieldcat USING p_structure TYPE tabname
CHANGING p_fieldcat TYPE lvc_t_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = p_structure
    CHANGING
      ct_fieldcat            = p_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.

    MESSAGE text-e02 TYPE 'S' DISPLAY LIKE 'E'.

  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.

  PERFORM zf_monta_alv.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  CASE sy-ucomm.
    WHEN 'E' OR 'ENDE' OR 'ECAN'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&  FORMS ALV_VARIANT_EXISTENCE
*&---------------------------------------------------------------------*
*& Utilizado para salvar o layout do ALV
*&---------------------------------------------------------------------*
FORM zf_carrega_layout CHANGING p_variant TYPE disvariant.
  CHECK p_layout IS NOT INITIAL.

  p_variant-report  = sy-repid.
  p_variant-variant = p_layout.

  IF p_layout CP '/*'.
    p_variant-username = sy-uname.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
    EXPORTING
      i_save        = sy-abcde(1)
    CHANGING
      cs_variant    = p_variant
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      OTHERS        = 4.

  IF sy-subrc <> 0.

    MESSAGE text-e09 TYPE 'S' DISPLAY LIKE 'E'. "Erro ao buscar variantes existentes
    LEAVE TO LIST-PROCESSING.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&  FORMS ZF_MATCHCODE_LAYOUT
*&---------------------------------------------------------------------*
*& Matchcode para variante existentes.
*&---------------------------------------------------------------------*
FORM zf_matchcode_layout  CHANGING p_variant TYPE disvariant.
  DATA: l_exit(1) TYPE c.

  p_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = p_variant
      i_save     = sy-abcde(1)
    IMPORTING
      e_exit     = l_exit
      es_variant = p_variant
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 2.

    MESSAGE text-e10 TYPE 'S' DISPLAY LIKE 'E'. "Erro ao selecionar variante

  ELSEIF l_exit = space.

    p_layout = p_variant-variant.

  ENDIF.

ENDFORM.

FORM zf_atualiza_alv CHANGING p_alv TYPE REF TO cl_gui_alv_grid.

  DATA:
    w_layout TYPE lvc_s_layo,
    w_stable TYPE lvc_s_stbl.

  CALL METHOD p_alv->get_frontend_layout
    IMPORTING
      es_layout = w_layout.

  w_layout-cwidth_opt = 'X'.
  w_layout-col_opt    = 'X'.

  CALL METHOD p_alv->set_frontend_layout
    EXPORTING
      is_layout = w_layout.

  w_stable-row = 'X'.
  CALL METHOD p_alv->refresh_table_display
    EXPORTING
      is_stable = w_stable.

  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.

FORM zf_prenche_saida2 USING p_index TYPE any.

  CLEAR w_saida.
  READ TABLE t_saida INTO w_saida INDEX p_index.
  IF sy-subrc = 0.

    t_saida2 = t_sbook.

    DELETE t_saida2 WHERE carrid <> w_saida-carrid AND
                          connid <> w_saida-connid AND
                          fldate <> w_saida-fldate.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_ESCRITA
*&---------------------------------------------------------------------*
*       Mostra mensagem com CARRID do VOO selecionado.
*----------------------------------------------------------------------*
FORM zf_escrita  USING  p_t_rows.

  DATA t_row TYPE lvc_t_row.

  t_row = p_t_rows.

  LOOP AT t_row INTO DATA(w_row).

    READ TABLE t_saida INTO w_saida INDEX w_row-index.
    IF sy-subrc = 0.

      MESSAGE w_saida-carrid TYPE 'I'.

    ENDIF.

  ENDLOOP.

ENDFORM.