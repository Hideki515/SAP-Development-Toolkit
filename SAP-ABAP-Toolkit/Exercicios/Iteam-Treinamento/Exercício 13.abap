*&---------------------------------------------------------------------------------------*
*& REPORT zpr_exec_12_bhs.
*&---------------------------------------------------------------------------------------*
*& Nome:zpr_exec_12_bhs
*& Tipo: Report
*& Objetivo: Faça uma rotina que receba uma workarea e some todos os seus campos
*& numéricos (a workarea deve conter no mínimo 3 campos deste tipo)
*&---------------------------------------------------------------------------------------*

REPORT zpr_exec_13_bhs.

*&-------------------------*
*& Types
*&-------------------------*
*&Cria o type da work area.
TYPES: BEGIN OF ty_data,
         d1(10) TYPE c,
         d2(10) TYPE c,
         d3     TYPE i,
         d4     TYPE i,
         d5     TYPE i,
       END OF ty_data.

*&-------------------------*
*& Work Areas
*&-------------------------*
*&Cria e define o tipo da work area.
DATA: wa_data TYPE ty_data.

*&-------------------------*
*& Variables
*&-------------------------*
DATA: gv_char(10) TYPE c VALUE '0123456789'.
DATA: gv_soma TYPE i.

wa_data-d1 = 'Oi'. "Passa os valores para os campos da work area.
wa_data-d2 = 'E'. "Passa os valores para os campos da work area.
wa_data-d3 = 3. "Passa os valores para os campos da work area.
wa_data-d4 = 4. "Passa os valores para os campos da work area.
wa_data-d5 = 5. "Passa os valores para os campos da work area.

START-OF-SELECTION.

*&-------------------------*
*& Performs
*&-------------------------*
*&Chama o form que seleciona os dados.
  PERFORM zf_verify CHANGING gv_soma.

END-OF-SELECTION.
  WRITE: 'A soma dos valores números é:', gv_soma. "Escreve a mensagem na tela.

*&---------------------------------------------------------------------*
*&      Form  ZF_VERIFY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
* Verfica e soma os valores dos campos que são númerios.
*----------------------------------------------------------------------*
FORM zf_verify  CHANGING VALUE(p_gv_soma) TYPE i.

  DATA temp. "Variável temporária.

  temp = wa_data-d1. "Temp recebe o valor do campo da work area
  IF temp CO gv_char. "Verifica que no campo é do typo número.
    ADD wa_data-d1 TO p_gv_soma."Faz a soma do campo da work area para variável.
  ENDIF.

  temp = wa_data-d2."Temp recebe o valor do campo da work area
  IF temp CO gv_char."Verifica que no campo é do typo número.
    ADD wa_data-d2 TO p_gv_soma. "Faz a soma do campo da work area para variável.
  ENDIF.

  temp = wa_data-d3."Temp recebe o valor do campo da work area
  IF temp CO gv_char."Verifica que no campo é do typo número.
    ADD wa_data-d3 TO p_gv_soma. "Faz a soma do campo da work area para variável.
  ENDIF.

  temp = wa_data-d4."Temp recebe o valor do campo da work area
  IF temp CO gv_char."Verifica que no campo é do typo número.
    ADD wa_data-d4 TO p_gv_soma. "Faz a soma do campo da work area para variável.
  ENDIF.

  temp = wa_data-d5."Temp recebe o valor do campo da work area
  IF temp CO gv_char."Verifica que no campo é do typo número.
    ADD wa_data-d5 TO p_gv_soma. "Faz a soma do campo da work area para variável.
  ENDIF.
ENDFORM.