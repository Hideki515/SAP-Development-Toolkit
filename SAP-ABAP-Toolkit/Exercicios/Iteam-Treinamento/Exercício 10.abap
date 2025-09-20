*&---------------------------------------------------------------------------------------*
*& REPORT zpr_exec_10_bhs.
*&---------------------------------------------------------------------------------------*
*& Nome:zpr_exec_10_bhs
*& Tipo: Report
*& Objetivo: Faça uma rotina que recebe dois números e escreve o resultado da operação
*& [maior_numero / menor_numero] caso os números sejam diferentes e escreva o
*& resultado de [número ^ 2] caso sejam iguais.
*&---------------------------------------------------------------------------------------*

REPORT zpr_exec_10_bhs.

*& Cria a tela que pede os valores.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS: p_n1 TYPE p, "Campo de entrado do valor p_n1.
            p_n2 TYPE p. "Campo de entrado do valor p_n2.

SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
*&-------------------*
*& Performs
*&-------------------*
*& Chama o perform que compara os valores.
  PERFORM zfm_compara.

*&---------------------------------------------------------------------*
*&      Form  ZFM_COMPARA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
* Verifica se os valores são iguais.
*----------------------------------------------------------------------*
FORM zfm_compara .
*&-------------------*
*& Variáveis
*&-------------------*
  data: lv_maior type p.
  DATA: lv_menor TYPE p.

  IF p_n1 <> p_n2. "Verifica se os valores são diferentes.
    IF p_n1 > p_n2. "Verifica se p_n1 é maior que p_n2.
      lv_maior = p_n1. "Passa o valor referente ao maior.
      lv_menor = p_n2. "Passa o valor referente ao menor.
    ELSEIF p_n2 > p_n1. "Verifica se p_n2 é maior que p_n1.
      lv_maior = p_n2. "Passa o valor referente ao maior.
      lv_menor = p_n1. "Passa o valor referente ao menor.
    ENDIF.
    DATA(lv_div) = lv_maior / lv_menor. "Faz a divisão entre o maior e o menor número.
    WRITE: 'A divisão do maior número pelo menor número é:', lv_div. "Escreve a mensagem.
  ELSEIF p_n1 = p_n2. "Verifica se os valores são iguais.
    p_n1 = p_n1 ** 2. "Eleva o valor a 2.
    p_n2 = p_n2 ** 2. "Eleva o valor a 2.
    WRITE: 'Os valores elevados a 2 Ficam n1', p_n1, 'n2', p_n2. "Escreve a mensagem.
  ENDIF.
ENDFORM.