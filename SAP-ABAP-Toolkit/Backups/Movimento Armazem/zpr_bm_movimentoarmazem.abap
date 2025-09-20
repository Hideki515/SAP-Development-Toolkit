*&---------------------------------------------------------------------*
*& Report ZPR_BM_MOVIMENTOARMAZEM
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpr_bm_movimentoarmazem.


***************
***	TABELAS	***
***************
TABLES: mat_stock,
        mat_handling,
        storehouse.

*************************
***	 INTERNAL TABLES  ***
*************************
DATA it_estoque TYPE TABLE OF zcds_bm_estoque.

***************************************
*** PARAMETROS DE SELEÇÃO DE DADOS  ***
***************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS: s_movnr FOR mat_handling-movnr,
                s_whnr  FOR storehouse-whnr,
                s_matnr FOR mat_handling-matnr.
SELECTION-SCREEN END OF BLOCK b1.

* START-OF-SELECTION
START-OF-SELECTION.
  PERFORM zf_select_data.

* END-OF-SELECTION
END-OF-SELECTION.

FORM zf_select_data.
  SELECT *
    FROM zcds_bm_movimento
    INTO TABLE @DATA(lt_movimento)
    WHERE
    movnr IN @s_movnr AND
    whnr IN @s_whnr AND
    matnr IN @s_matnr.

  PERFORM zf_display_alv TABLES lt_movimento.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form ZF_DISPLAY_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_MOVIMENTO
*&---------------------------------------------------------------------*
FORM zf_display_alv  TABLES   p_lt_movimento.
  DATA: lt_fieldcat TYPE lvc_t_fcat.
  DATA: lw_layout   TYPE lvc_s_layo.
  DATA: lw_saida TYPE zcds_bm_movimento.

  lw_layout-zebra = abap_true.
  lw_layout-cwidth_opt = abap_true.

* Criação da tabela de fieldcat
  CALL FUNCTION 'STRALAN_FIELDCAT_CREATE'
    EXPORTING
      is_structure = lw_saida
    IMPORTING
      et_fieldcat  = lt_fieldcat.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program = sy-repid
      is_layout_lvc      = lw_layout
      it_fieldcat_lvc    = lt_fieldcat
    TABLES
      t_outtab           = p_lt_movimento
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc NE 0.
    MESSAGE s208(00) DISPLAY LIKE sy-abcde+4(1) WITH TEXT-e02.
  ENDIF.
ENDFORM.