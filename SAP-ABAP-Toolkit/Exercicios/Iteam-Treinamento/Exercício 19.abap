*&---------------------------------------------------------------------------------------*
*& REPORT zpr_exec_15_bhs.
*&---------------------------------------------------------------------------------------*
*& Nome:zpr_exec_15_bhs
*& Tipo: Report
*& Objetivo:  Faça uma rotina que contenha um select-option para um campo numérico e
*& imprimir o resultado da multiplicação de cada número dentro do range por 3.
*&---------------------------------------------------------------------------------------*

REPORT zpr_exec_19_bhs.

*&-------------------------*
*& Tables
*&-------------------------*
TABLES: sflight.

*&-------------------------*
*& Variables
*&-------------------------*
DATA: number TYPE i.
DATA: gv_mult TYPE i.

*&-------------------------*
*& Paramiters Selection
*&-------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS: s_num FOR number NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
*&-------------------------*
*& Performs
*&-------------------------*
  PERFORM zf_mult.

FORM zf_mult.
  LOOP AT s_num INTO DATA(lv_va). "Passa linha por linha do range do campo do select options e guarda em uma work area.
    gv_mult = lv_va-low * 3. "Faz a multiplicação do valor da linha atual da work area.

    WRITE: / gv_mult. "Escreve na tela a mensagem.
  ENDLOOP. "Fim do Loop.
ENDFORM.