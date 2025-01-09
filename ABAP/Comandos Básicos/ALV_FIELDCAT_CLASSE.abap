*&---------------------------------------------------------------------*
*& Report ZR_BHS_ALV_CLASSE
*&---------------------------------------------------------------------*
*& Estrutura Básica para o ALV de classe.
*&---------------------------------------------------------------------*
REPORT zr_bhs_alv_classe.

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

       BEGIN OF ty_spfli,
         carrid   TYPE spfli-carrid,
         connid   TYPE spfli-connid,
         cityfrom TYPE spfli-cityfrom,
         cityto   TYPE spfli-cityto,
         airpfrom TYPE spfli-airpfrom,
         airpto   TYPE spfli-airpto,
       END OF ty_spfli,

       BEGIN OF ty_saida,
         carrid    TYPE sflight-carrid,
         fldate    TYPE sflight-fldate,
         planetype TYPE sflight-planetype,
         pamentsum TYPE sflight-paymentsum,
         carrname  TYPE scarr-carrname,
         cityfrom  TYPE spfli-cityfrom,
         cityto    TYPE spfli-cityto,
         airpfrom  TYPE spfli-airpfrom,
         airpto    TYPE spfli-airpto,
       END OF ty_saida.

*&---------------------------------------------------------------------*
*& INTERNAL TABLE
*&---------------------------------------------------------------------*
DATA:
  t_sflight TYPE TABLE OF ty_sflight,
  t_scarr   TYPE TABLE OF ty_scarr,
  t_spfli   TYPE TABLE OF ty_spfli,
  t_saida   TYPE TABLE OF ty_saida.

*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*
DATA:
  w_sflight TYPE ty_sflight,
  w_scarr   TYPE ty_scarr,
  w_spfli   TYPE ty_spfli,
  w_saida   TYPE ty_saida.

*&---------------------------------------------------------------------*
*& VARIABLES
*&---------------------------------------------------------------------*
DATA: v_cont_alv TYPE REF TO cl_gui_custom_container,
      v_grid_alv TYPE REF TO cl_gui_alv_grid,
      v_variant  TYPE disvariant.

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

    DATA(t_sflight_aux) = t_sflight.

    DELETE ADJACENT DUPLICATES FROM t_sflight_aux COMPARING carrid.

    SELECT carrid
           carrname
           currcode
      FROM scarr
      INTO TABLE t_scarr
      FOR ALL ENTRIES IN t_sflight_aux
      WHERE
        carrid = t_sflight_aux-carrid.

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

    w_saida-carrid    = w_sflight-carrid.
    w_saida-fldate    = w_sflight-fldate.
    w_saida-planetype = w_sflight-planetype.

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

  ENDLOOP.

  APPEND w_saida TO t_saida.

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
    l_cont_alv TYPE REF TO cl_gui_container,
    l_event    TYPE REF TO cl_gui_container.

  IF v_cont_alv IS INITIAL.


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

        MESSAGE text-e07 TYPE 'S' DISPLAY LIKE 'E'. "Erro ao exibir ALV.
        LEAVE TO SCREEN 0.

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