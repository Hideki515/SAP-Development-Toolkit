*&---------------------------------------------------------------------------------------*
*& REPORT zpr_exec_09_bhs.
*&---------------------------------------------------------------------------------------*
*& Nome:zpr_exec_09_bhs
*& Tipo: Report
*& Objetivo: Faça uma rotina que receba dois números e retorne um flag (caracter de
*& tamanho 1). Caso os números sejam iguais a flag retornada será ‘X’ e caso contrário a
*& flag será igual a ‘ ‘ (space)
*&---------------------------------------------------------------------------------------*

REPORT zpr_exec_09_bhs.

*&-------------------*
*& Variáveis
*&-------------------*
DATA: flag TYPE c.

*& Cria a tela que pede os valores.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_var1 TYPE string, "Campo de entrada para digitar o primeiro valor.
            p_var2 TYPE string. "Campo de entrada para digitar o segundo valor.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
*&-------------------*
*& Performs
*&-------------------*
*& Chama o Form de comparação.
PERFORM zfm_comparador.
  WRITE: / 'Flag:', flag. "Após perform exibe a mensagem do Flag.
*&---------------------------------------------------------------------*
*&      Form  ZFM_COMPARADOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*& Compara se os dois valores são tem o mesmo valor.
*----------------------------------------------------------------------*
FORM zfm_comparador.

  IF p_var1 = p_var2. "Verifica se os dois valore são iguais.
    WRITE: 'Os dois valores são do mesmo valor, n1', p_var1, 'n2', p_var2. "Escreve a mensagem.
    flag = 'X'. "Passa o valor X para a Flag.
  ELSE.
    flag = ' '. "Passa o valor space para a Flag.
  ENDIF.

ENDFORM.