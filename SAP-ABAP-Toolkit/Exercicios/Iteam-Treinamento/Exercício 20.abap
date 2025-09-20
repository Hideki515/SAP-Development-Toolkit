*&---------------------------------------------------------------------------------------*
*& REPORT zpr_exec_15_bhs.
*&---------------------------------------------------------------------------------------*
*& Nome:zpr_exec_15_bhs
*& Tipo: Report
*& Objetivo:  Faça uma rotina que contenha um select-option para um campo numérico sem
*& o botão de seleção de ranges múltiplos e, que imprima os números deste range
*& separados por “, “.
*&---------------------------------------------------------------------------------------*

REPORT zpr_exec_20_bhs.

*&-------------------------*
*& Variables
*&-------------------------*
DATA: gv_number TYPE i.
DATA: gv_count TYPE i.
DATA: gv_int TYPE i.

*&-------------------------*
*& Paramiters Selection
*&-------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS: s_number FOR gv_number NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK b1.

*&-------------------------*
*& Performs
*&-------------------------*
PERFORM zf_write.

*&---------------------------------------------------------------------*
*&      Form  ZF_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
* Faz um loop e escreve os valores na tela.
*----------------------------------------------------------------------*
FORM zf_write .
  DATA: lv_initial TYPE c.
  DATA: lv_final TYPE c VALUE IS INITIAL.
  DATA: lv_space TYPE c VALUE IS INITIAL.

  lv_initial = s_number-low. "Passa o valor inicial do range para a variável.
  lv_final = s_number-high. "Passa o valor final do range para a variável.

  DO s_number-high TIMES. "Faz um loop pela quantidade de vezes do final do range.

    IF s_number-low <= lv_final. "Verifica se o valor inicial é menor que o valor final.

      CONCATENATE lv_initial lv_space INTO DATA(lv_result) SEPARATED BY ','. "Concatena a reposta.

      WRITE: lv_result. "Escreve a resposta na tela.

      ADD 1 TO lv_initial. "Adiciona mais 1 na contagem do valor atual.

      IF lv_initial = s_number-high. "Verifica se o valor atual é igual a o último valor do range.

        WRITE: lv_final. "Escreve o valor final na tela.

        EXIT. "Finaliza o loop.
      ENDIF.
    ELSE.

      EXIT. "Finaliza o loop.

    ENDIF.

  ENDDO.

ENDFORM.