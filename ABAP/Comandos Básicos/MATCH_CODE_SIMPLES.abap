REPORT zpr_bhs_matchcode.

*&---------------------------------------------------------------------*
*&  TYPES
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_search,
         nome   TYPE zde_nome,
         status TYPE zde_status,
       END OF ty_search, " Estrutura personalizada

       ty_search_tab TYPE TABLE OF ty_search,  " Tipo de tabela interna
       ty_return_tab TYPE TABLE OF ddshretval. " Tipo de tabela interna

*&---------------------------------------------------------------------*
*&  INTERNAL TABLES
*&---------------------------------------------------------------------*
DATA:
  it_search TYPE ty_search_tab,
  it_return TYPE ty_return_tab.

*&---------------------------------------------------------------------*
*&  WORK AREA
*&---------------------------------------------------------------------*
DATA:
  w_search TYPE ty_search.

*&---------------------------------------------------------------------*
*&  SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.

PARAMETERS p_status TYPE char1.

SELECTION-SCREEN END OF BLOCK b1.

*&---------------------------------------------------------------------*
*&  AT SELECTION-SCREEN
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_status.
  PERFORM zf_monta_tabela_search.
  PERFORM zf_matchcode_status.

*&---------------------------------------------------------------------*
*&  Form ZF_MONTA_TABELA_SEARCH
*&---------------------------------------------------------------------*
FORM zf_monta_tabela_search.

  FREE it_search.

  PERFORM zf_preenche_matcode USING:
        'Sucesso' 'S',
        'Erro'    'E',
        'Aberto'  'A',
        'Fechado' 'F'.

ENDFORM.

*&---------------------------------------------------------------------*
*&  Form ZF_PREENCHE_MATCODE
*&---------------------------------------------------------------------*
FORM zf_preenche_matcode USING p_nome   TYPE any
                               p_status TYPE any.

  CLEAR w_search.

  w_search-nome = p_nome.
  w_search-status = p_status.

  APPEND w_search TO it_search.

ENDFORM.

*&---------------------------------------------------------------------*
*&  Form ZF_MATCHCODE_STATUS
*&---------------------------------------------------------------------*
FORM zf_matchcode_status.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield         = 'STATUS'
      dynpprog         = sy-repid
      dynpnr           = sy-dynnr
      value_org        = 'S'
      callback_program = sy-repid
    TABLES
      value_tab        = it_search
      return_tab       = it_return
    EXCEPTIONS
      parameter_error  = 1
      no_values_found  = 2
      OTHERS           = 3.

  IF sy-subrc = 0.

    " Preenchimento do parametro com os dados na tabela de retorno
    READ TABLE it_return INTO DATA(lw_return) INDEX 1. " Lê a tabela interna de retorno verificando se é encontrados dados na 1 primeira linha da tabela
    IF sy-subrc = 0. " Verifica se encontrou a linha

    p_status = lw_return-fieldval.

    ENDIF.

  ELSE.

    MESSAGE text-e01 TYPE 'S' DISPLAY LIKE 'E'.

  ENDIF.

ENDFORM.