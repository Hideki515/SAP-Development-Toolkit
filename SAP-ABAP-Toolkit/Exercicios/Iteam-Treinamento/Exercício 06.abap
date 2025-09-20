*&---------------------------------------------------------------------------------------*
*& REPORT zpr_exec_06_bhs.
*&---------------------------------------------------------------------------------------*
*& Nome:zpr_exec_06_bhs
*& Tipo: Report
*& Objetivo: Faça uma rotina (FORM) que imprima o username de todas as pessoas de do
*& treinamento (Veja a tabela USR04 na SE11 e seu conteúdo).
*&---------------------------------------------------------------------------------------*

REPORT zpr_exec_06_bhs.

*&-------------------*
*& Tabelas
*&-------------------*
TABLES: usr04.

*&-------------------*
*& Performs
*&-------------------*
PERFORM zfm_select_data.

*&---------------------------------------------------------------------*
*&      Form  ZFM_SELECT_DATA
*&---------------------------------------------------------------------*
* Seleciona os dados.
*----------------------------------------------------------------------*
FORM zfm_select_data .
  SELECT
    *
  FROM usr04
  INTO TABLE @DATA(t_users).

  PERFORM zfm_display_data USING t_users.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZFM_DISPLAY_DATA
*&---------------------------------------------------------------------*
* Exibe os dados selecionados.
*----------------------------------------------------------------------*
FORM zfm_display_data  USING    p_t_users.
  cl_demo_output=>display( p_t_users ).
ENDFORM.