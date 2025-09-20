*&---------------------------------------------------------------------------------------*
*& REPORT zpr_exec_16_bhs.
*&---------------------------------------------------------------------------------------*
*& Nome:zpr_exec_16_bhs
*& Tipo: Report
*& Objetivo:  Faça uma rotina que receba dois números (via parameters). O primeiro
*& representa um número a ser impresso e o segundo representa o número de casas a
*& serem impressas. Coloque zeros a esquerda caso necessário. Exemplos:
*& a. p_numero = 15 p_casas = 2. Saída = 15
*& b. p_numero = 15 p_casas = 4. Saída = 0015
*& c. p_numero = 15 p_casas = 6. Saída = 000015
*& d. p_numero = 2011 p_casas = 2. Saída = 20
*& e. p_numero = 123456789 p_casas = 10. Saída = 0123456789
*& f. p_numero = 123456789 p_casas = 4. Saída = 1234
*&---------------------------------------------------------------------------------------*

REPORT zpr_exec_16_bhs.

*&-------------------------*
*& Variables
*&-------------------------*
DATA gv_qtd TYPE string.
DATA gv_len TYPE string.
DATA gv_result TYPE string.

*&-------------------------*
*& Paramiters Selection
*&-------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_num(5) TYPE c,
            p_cas(5) TYPE c.
SELECTION-SCREEN END OF BLOCK b1.

*&-------------------------*
*& Performs
*&-------------------------*
START-OF-SELECTION.
  PERFORM zf_colocar.
  WRITE gv_result.

*&---------------------------------------------------------------------*
*&      Form  ZF_COLOCAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_colocar.

  gv_qtd = p_cas.
  gv_len = strlen( gv_qtd ).

  IF gv_qtd LE gv_len.
    gv_qtd = gv_qtd(gv_qtd). "Retorna string contendo o parametro 'p_num', do primeiro caracter até o (qtd)

    "Caso seja maior, realiza um loop para criar quandidade de caracteres '0' nescessários para impressao
  ELSE.
    SUBTRACT gv_len FROM gv_qtd.
    DO gv_qtd TIMES.
      CONCATENATE '0' gv_result INTO gv_result.
    ENDDO.

    CONCATENATE gv_result gv_qtd INTO gv_result.
  ENDIF.
ENDFORM.