*&---------------------------------------------------------------------*
*&  Include           ZPR_EXEC_31_BHS_SCR
*&---------------------------------------------------------------------*

*&-------------------------*
*& Paramiters Selections
*&-------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

SELECT-OPTIONS: s_vbeln FOR vbrk-vbeln,
                s_fkdat FOR vbrk-fkdat.

PARAMETERS: p_kunrg TYPE vbak-kunnr.

SELECTION-SCREEN END OF BLOCK b1.