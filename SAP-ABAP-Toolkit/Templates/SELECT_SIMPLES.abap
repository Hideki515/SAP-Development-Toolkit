*&---------------------------------------------------------------------*
*& Report ZR_SELECT
*&---------------------------------------------------------------------*
*& Select simples
*&---------------------------------------------------------------------*
REPORT zr_select.

*&---------------------------------------------------------------------*
*& TYPES
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_sflight, "Definição da estrutura da tabela
         carrid TYPE sflight-carrid,
         connid TYPE sflight-connid,
         fldate TYPE sflight-fldate,
       END OF ty_sflight.

*&---------------------------------------------------------------------*
*& INTERNAL TABLES
*&---------------------------------------------------------------------*
DATA: t_sflight TYPE TABLE OF ty_sflight. "Definição da tabela interna.

*&---------------------------------------------------------------------*
*& START-OF-SELECTION.
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM zf_selecao_dados. "Chama a rotina que faz a seleção dos dados

*&---------------------------------------------------------------------*
*& END-OF-SELECTION.
*&---------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM zf_exibir_dados. "Chama a rotina que irá exibir os dados.

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECAO_DADOS
*&---------------------------------------------------------------------*
*       Selecao dos dados
*----------------------------------------------------------------------*
FORM zf_selecao_dados .

  SELECT carrid "Campos a serem selecionados
         connid
         fldate
    FROM sflights "Tabela que será feito o select.
    INTO TABLE t_sflight "Tabela interna que irá guardar os dados do select.
    UP TO 10 ROWS. "Defini que serão selecionadas apenas as 10 primeiras linhas.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_EXIBIR_DADOS
*&---------------------------------------------------------------------*
*       Exibe os dados
*----------------------------------------------------------------------*
FORM zf_exibir_dados .

  cl_demo_output=>display( t_sflight ). "Exibe a tabela.

ENDFORM.