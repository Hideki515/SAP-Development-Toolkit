REPORT zpr_exec_14_bhs.

*&-------------------------*
*& Types
*&-------------------------*
*&Cria o type da work area.
TYPES: BEGIN OF ty_data,
         var1(40) TYPE c,
         var2(40) TYPE c,
         var3(40) TYPE c,
         var4     TYPE i,
         var5     TYPE i,
         var6     TYPE i,
       END OF ty_data.

*&-------------------------*
*& Work Areas
*&-------------------------*
*&Cria e define o tipo da work area.
DATA: wa_data TYPE ty_data.

*&-------------------------*
*& Variables
*&-------------------------*
DATA: gv_char(5) TYPE c VALUE 'AEIOU'.
DATA: gv_qtd TYPE i.
DATA: gv_count TYPE i.
DATA: gv_sum TYPE i VALUE 0.
DATA: gv_contl TYPE i.

wa_data-var1 = 'AEIOUAEIOUA'. "Passa valor para work area.
wa_data-var2 = 'BAEIOUAEIOU'. "Passa valor para work area.
wa_data-var3 = 'CAEIOUAEIOU'. "Passa valor para work area.
wa_data-var4 = 1. "Passa valor para work area.
wa_data-var5 = 2. "Passa valor para work area.
wa_data-var6 = 0. "Passa valor para work area.

TRANSLATE: wa_data-var1 TO UPPER CASE,
           wa_data-var2 TO UPPER CASE,
           wa_data-var3 TO UPPER CASE.

START-OF-SELECTION.
*&-------------------------*
*& Performs
*&-------------------------*
PERFORM zf_sum.
PERFORM zf_erase.
PERFORM zf_write.

FORM zf_sum.
  PERFORM zf_zero. "Chama o perform de zerar os contadores.
  gv_qtd = strlen( wa_data-var1 ). "Guarda a qauntidade do tamanho da string.
  DO gv_qtd TIMES. "Repete
    IF wa_data-var1+gv_contl(1) CA gv_char. "Verifica se a letra é vogal
      ADD 1 TO gv_count. "Adiciona um no contador de vogais.
    ENDIF.
    ADD 1 TO gv_contl. "Adiciona um no contador de vezes.
  ENDDO.

  PERFORM zf_zero. "Chama o perform de zerar os contadores.
  gv_qtd = strlen( wa_data-var1 ). "Guarda a qauntidade do tamanho da string.
  DO gv_qtd TIMES. "Repete
    IF wa_data-var2+gv_contl(1) CA gv_char. "Verifica se a letra é vogal
      ADD 1 TO gv_count. "Adiciona um no contador de vogais.
    ENDIF.
    ADD 1 TO gv_contl.
  ENDDO.

  PERFORM zf_zero. "Chama o perform de zerar os contadores.
  gv_qtd = strlen( wa_data-var1 ). "Guarda a qauntidade do tamanho da string.
  DO gv_qtd TIMES. "Repete
    IF wa_data-var3+gv_contl(1) CA gv_char. "Verifica se a letra é vogal
      ADD 1 TO gv_count. "Adiciona um no contador de vogais.
    ENDIF.
    ADD 1 TO gv_contl. "Adiciona um no contador de vezes.
  ENDDO.

  gv_sum = wa_data-var4 + wa_data-var5 + wa_data-var6.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_ZERO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -Zera os contadores.
*----------------------------------------------------------------------*
FORM zf_zero .
  gv_qtd = 0.
  gv_contl = 0.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_ERASE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  Limpa o que está na tela conforme a divisão.
*----------------------------------------------------------------------*
FORM zf_erase .

  IF gv_sum MOD 2 EQ 1.
    CLEAR wa_data-var1.
    CLEAR wa_data-var2.
    CLEAR wa_data-var3.
  ENDIF.

  IF gv_count MOD 2 EQ 0.
    CLEAR wa_data-var4.
    CLEAR wa_data-var5.
    CLEAR wa_data-var6.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  Escreve as mensagens na tela.
*----------------------------------------------------------------------*
FORM zf_write .

  WRITE: / 'String:', wa_data-var1. "Escreve a mensagem na tela.
  WRITE: / 'String:', wa_data-var2. "Escreve a mensagem na tela.
  WRITE: / 'String:', wa_data-var3. "Escreve a mensagem na tela.
  WRITE: / 'Numero:', wa_data-var4. "Escreve a mensagem na tela.
  WRITE: / 'Numero:', wa_data-var5. "Escreve a mensagem na tela.
  WRITE: / 'Numero:', wa_data-var6. "Escreve a mensagem na tela.
  WRITE: / 'Total de Vogais:', gv_count. "Escreve a mensagem na tela.
  WRITE: / 'Total soma números:', gv_sum. "Escreve a mensagem na tela.

ENDFORM.