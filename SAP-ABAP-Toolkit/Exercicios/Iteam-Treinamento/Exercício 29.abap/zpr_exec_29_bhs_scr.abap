*&---------------------------------------------------------------------*
*&  Include           ZPR_EXEC_29_BHS_SCR
*&---------------------------------------------------------------------*

*&-------------------------*
*& Paramiters Selections
*&-------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

SELECT-OPTIONS: s_mblnr FOR mkpf-mblnr.

PARAMETERS: p_mjahr TYPE mkpf-mjahr DEFAULT '2008'.

SELECT-OPTIONS s_bwart FOR mseg-bwart.

SELECTION-SCREEN END OF BLOCK b1.