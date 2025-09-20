*&---------------------------------------------------------------------------------------*
*& REPORT zpr_exec_08_bhs.
*&---------------------------------------------------------------------------------------*
*& Nome:zpr_exec_08_bhs
*& Tipo: Report
*& Objetivo: Faça uma rotina que receba dois números e retorne o maior deles (caso os
*& números sejam iguais retorne o próprio número).
*&---------------------------------------------------------------------------------------*

REPORT zpr_exec_08_bhs.

*& Cria a tela que pede os valores.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_var1 TYPE string, "Campo de entrada para digitar o primeiro valor.
            p_var2 TYPE string. "Campo de entrada para digitar o segundo valor.
SELECTION-SCREEN END OF BLOCK b1.

*& Chama o Form de comparação.
PERFORM zfm_comparador.

*&---------------------------------------------------------------------*
*&      Form  ZFM_COMPARADOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*& Compara e mostra qual é o Maior número entre os dois.
*----------------------------------------------------------------------*
FORM zfm_comparador.

  IF p_var1 > p_var2. "Verifica se o primeiro valor é maior que o segundo.
    WRITE: 'O 1° Número', p_var1, 'é maior que o segundo número', p_var2. "Escreve a mensagem
  ELSEIF p_var2 > p_var1. "Verifica se o segundo valor é maior que o primeiro.
    WRITE: 'O 2° Número', p_var2, 'é maior que o primeiro número', p_var1. "Escreve a mensagem
  ELSEIF p_var1 = p_var2. "Verifica se os dois valore são iguais.
    WRITE: 'Os dois valores são do mesmo valor, n1', p_var1, 'n2', p_var2. "Escreve a mensagem
  ENDIF.

ENDFORM.