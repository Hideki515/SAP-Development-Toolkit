*&---------------------------------------------------------------------------------------*
*& REPORT zpr_exec_15_bhs.
*&---------------------------------------------------------------------------------------*
*& Nome:zpr_exec_15_bhs
*& Tipo: Report
*& Objetivo:  Faça uma rotina que receba dois números (base e expoente) obrigatórios (via
*& parameters). Imprima o resultado da exponenciação.
*&---------------------------------------------------------------------------------------*

REPORT zpr_exec_17_bhs.

*&-------------------------*
*& Paramiters Selection
*&-------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: p_base TYPE i,
            p_exp  TYPE i.
SELECTION-SCREEN END OF BLOCK b1.
*&-------------------------*
*& Variables
*&-------------------------*
DATA gv_result TYPE i.

*&-------------------------*
*& Performs
*&-------------------------*
PERFORM zf_expo.

*&---------------------------------------------------------------------*
*&      Form  ZF_EXPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  Faz a expotenciação e escreve na tela.
*----------------------------------------------------------------------*
FORM zf_expo.
  gv_result = p_base ** p_exp.

  WRITE: 'A potencia é:', gv_result.
ENDFORM.