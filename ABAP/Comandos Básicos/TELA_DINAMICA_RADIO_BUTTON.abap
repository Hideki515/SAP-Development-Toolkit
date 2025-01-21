REPORT zpr_bhs_tela_dinamica_radio.

*&---------------------------------------------------------------------*
*&  TABLES
*&---------------------------------------------------------------------*
TABLES :
  ekko,
  ekpo.

*&---------------------------------------------------------------------*
*&  CONSTANTS
*&---------------------------------------------------------------------*
CONSTANTS :
  c_r1 TYPE char2 VALUE 'R1',
  c_r2 TYPE char2 VALUE 'R2',
  c_r3 TYPE char2 VALUE 'R3'.

*&---------------------------------------------------------------------*
*&  SELECTION SCREENS
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.

PARAMETERS:
  p_blc1 RADIOBUTTON GROUP gp01 DEFAULT 'X'USER-COMMAND radio,
  p_blc2 RADIOBUTTON GROUP gp01,
  p_blc3 RADIOBUTTON GROUP gp01.

SELECTION-SCREEN END OF BLOCK b1.


SELECTION-SCREEN BEGIN OF BLOCK b2.

PARAMETERS:
  p_bloc1  TYPE file_table-filename DEFAULT 'Bloco 1' MODIF ID r2.

SELECTION-SCREEN END   OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3.

PARAMETERS:
  p_bloc21 TYPE file_table-filename DEFAULT 'Bloco 21' MODIF ID r1,
  p_bloc22 TYPE file_table-filename DEFAULT 'Bloco 22' MODIF ID r1,
  p_bloc23 TYPE file_table-filename DEFAULT 'Bloco 23' MODIF ID r1.

SELECTION-SCREEN END   OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4.

SELECT-OPTIONS:
  s_ebeln FOR ekko-ebeln MODIF ID r3.

SELECTION-SCREEN END OF BLOCK b4.

*&---------------------------------------------------------------------*
*&  AT SELECTION-SCREEN
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  IF p_blc1 IS NOT INITIAL AND
     p_blc2 IS INITIAL AND
     p_blc3 IS INITIAL.

    PERFORM zf_controla_tela USING c_r1.

  ELSEIF p_blc2 IS NOT INITIAL AND
         p_blc3 IS INITIAL AND
         p_blc1 IS INITIAL.

    PERFORM zf_controla_tela USING c_r2.

  ELSEIF p_blc3 IS NOT INITIAL AND
         p_blc2 IS INITIAL AND
         p_blc1 IS INITIAL.

    PERFORM zf_controla_tela USING c_r3.

  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  zf_controla_tela
*&---------------------------------------------------------------------*
FORM zf_controla_tela USING p_campo TYPE c.

  LOOP AT SCREEN.

    IF screen-group1 NE p_campo AND
       screen-group1 IS NOT INITIAL.

      screen-active = 0.

    ELSE.

      screen-active = 1.

    ENDIF.

    MODIFY SCREEN.

  ENDLOOP.

ENDFORM.