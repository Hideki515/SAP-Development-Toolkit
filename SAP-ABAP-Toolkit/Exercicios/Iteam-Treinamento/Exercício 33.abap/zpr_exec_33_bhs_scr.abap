*&---------------------------------------------------------------------*
*&  Include           ZPR_EXEC_32_BHS_SCR
*&---------------------------------------------------------------------*

*&-------------------------*
*& Paramters
*&-------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.

PARAMETERS p_loc TYPE rlgrap-filename OBLIGATORY. "Parametro de seleção do local do arquivo.

SELECTION-SCREEN END OF BLOCK b1. "Fim bloco b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t02.

PARAMETERS: rb_txt RADIOBUTTON GROUP rad1 DEFAULT 'X' USER-COMMAND ucomm, "Radio button para arquivo .txt.
            rb_csv RADIOBUTTON GROUP rad1. "Radio button para arquivo .csv.

SELECTION-SCREEN END OF BLOCK b2. "Fim bloco b2.