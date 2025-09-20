*&---------------------------------------------------------------------------------------*
*& REPORT zpr_exec_18_bhs.
*&---------------------------------------------------------------------------------------*
*& Nome:zpr_exec_18_bhs
*& Tipo: Report
*& Objetivo:  Faça uma rotina que receba uma string e um número (Z) menor ou igual a 20.
*& O programa deve imprimir a string Z vezes com a seguinte saída conforme exemplo:
*& a. String = “Good Food, Good Life”. Z = 20. Saída:
*& b. Linha [sy-?????]: G
*& c. Linha [sy-?????]:Go
*& d. Linha [sy-?????]:Goo
*& e. Linha [sy-?????]:Good
*& f. Linha [sy-?????]:Good (aqui tem um espaço)
*& g. Linha [sy-?????]:Good F
*& h. (...)
*&i. Linha [Z]: Good Food, Good Life
*& Caso Z seja maior que 20 imprimir uma mensagem de erro usando o comando
*& WRITE.
*&---------------------------------------------------------------------------------------*

REPORT zpr_exec_18_bhs.

*&-------------------------*
*& Variables
*&-------------------------*
DATA: gv_string TYPE string.
DATA: gv_lenght TYPE string.
DATA: gv_count TYPE i.

*&-------------------------*
*& Paramiters Selection
*&-------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: p_string(20) TYPE c,
            p_num        TYPE i.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
*&-------------------------*
*& Performs
*&-------------------------*
  PERFORM zf_action.

*&---------------------------------------------------------------------*
*&      Form  ZF_ACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  Verifica e escreve a palavra conforme necessário.
*----------------------------------------------------------------------*
FORM zf_action.

  IF p_num <= 20 AND gv_lenght <= 20. "Verifica se a quantidade de caracteres e linhs digitadas é menor que 20.
    gv_string = p_string.

    gv_lenght = strlen( gv_string ).

    DO p_num TIMES. "Repete e escreve a string conforme a quantidade de linhas digitadas.

      ADD 1 TO gv_count.

      WRITE: / gv_string(gv_count).

    ENDDO.
  ELSE.
    WRITE: / 'Erro a quantidade máxima de caracteres e linhas é 20!'. "Escreve a mensagem de erro por quantidade de linhas execiva.
  ENDIF.

ENDFORM.