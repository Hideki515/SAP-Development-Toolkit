*&---------------------------------------------------------------------------------------*
*& REPORT zpr_exec_15_bhs.
*&---------------------------------------------------------------------------------------*
*& Nome:zpr_exec_15_bhs
*& Tipo: Report
*& Objetivo:  Faça uma rotina que receba uma tabela interna e imprima quantos campos
*& estão em branco por linha (o tipo da tabela deve ter no mínimo 4 campos). saída
*& desejada deve ter o template:
*& linha [número da linha] =>[10 caracteres em branco] + [número de campos em
*& branco] + “ campos em branco”.
*&---------------------------------------------------------------------------------------*

REPORT zpr_exec_15_bhs.

*&-------------------------*
*& Types
*&-------------------------*
TYPES: BEGIN OF ty_data,
         carrid   TYPE sflight-carrid,
         connid   TYPE sflight-connid,
         fldate   TYPE sflight-fldate,
         seatsmax TYPE sflight-seatsmax,
       END OF ty_data.

TYPES: BEGIN OF ty_aux,
         linha TYPE i,
         count TYPE i,
       END OF ty_aux.
*&-------------------------*
*& Internal Tables
*&-------------------------*
DATA: it_aux TYPE TABLE OF ty_aux.
DATA: it_data TYPE TABLE OF ty_data.

*&-------------------------*
*& Work Areas
*&-------------------------*

DATA: wa_aux TYPE ty_aux.
DATA: wa_data TYPE ty_data.

*&-------------------------*
*& Variables
*&-------------------------*
DATA: gv_count TYPE i.
DATA: gv_espaco(12) TYPE c VALUE '[          ]'.

START-OF-SELECTION.
*&-------------------------*
*& Performas
*&-------------------------*
  PERFORM zf_select_data.
  PERFORM zf_vazio TABLES it_data.
  PERFORM zf_display.

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
* Seleciona os dados
*----------------------------------------------------------------------*
FORM zf_select_data.

  SELECT
    carrid
    connid
    fldate
    seatsmax
  FROM sflight
  INTO TABLE it_data.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  zf_vazio
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
* Verfica os campos vazios.
*----------------------------------------------------------------------*
FORM zf_vazio TABLES any.
  LOOP AT it_data INTO wa_data.
    IF wa_data-carrid IS INITIAL.
      ADD 1 TO wa_aux-count.
    ENDIF.

    IF wa_data-connid IS INITIAL.
      ADD 1 TO wa_aux-count.
    ENDIF.

    IF wa_data-fldate IS INITIAL.
      ADD 1 TO wa_aux-count.
    ENDIF.

    IF wa_data-seatsmax IS INITIAL.
      ADD 1 TO wa_aux-count.
    ENDIF.

    wa_aux-linha = sy-tabix.

    ADD 1 TO gv_count.

    APPEND wa_aux TO it_aux.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_VERIFY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
* Exibe os valores.
*----------------------------------------------------------------------*
FORM zf_display.
  LOOP AT it_aux INTO wa_aux.
    WRITE: / 'Linha', wa_aux-linha, gv_espaco, wa_aux-count, 'em branco'.
  ENDLOOP.
ENDFORM.