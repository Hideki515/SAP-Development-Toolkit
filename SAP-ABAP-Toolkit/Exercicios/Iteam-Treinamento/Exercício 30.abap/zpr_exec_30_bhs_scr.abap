*&---------------------------------------------------------------------*
*&  Include           ZPR_EXEC_30_BHS_SCR
*&---------------------------------------------------------------------*

*&-------------------------*
*& Paramiters Selections
*&-------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

SELECT-OPTIONS: s_vbeln FOR vbak-vbeln,
                s_erdat FOR vbak-erdat OBLIGATORY,
                s_kunnr FOR vbak-kunnr.

SELECTION-SCREEN END OF BLOCK b1.