*&---------------------------------------------------------------------*
*&  Include           ZPR_EXEC_32_BHS_TOP
*&---------------------------------------------------------------------*

*&-------------------------*
*& Types
*&-------------------------*
TYPES: BEGIN OF ty_bebida,
         matnr           TYPE mara-matnr,
         maktx           TYPE makt-maktx,
         meins           TYPE mara-meins,
         brgew           TYPE mara-brgew,
         gewei           TYPE mara-gewei,
         volum           TYPE mara-volum,
         voleh           TYPE mara-voleh,
         status          TYPE c         , " Status do registro (S/E)
         observacao(100) TYPE c         , " Mensagem retornada
       END OF ty_bebida.

TYPES: BEGIN OF ty_entrada,
         matnr TYPE string,
         maktx TYPE string,
         meins TYPE string,
         brgew TYPE string,
         gewei TYPE string,
         volum TYPE string,
         voleh TYPE string,
       END OF ty_entrada.

TYPES: BEGIN OF ty_bdcdata,
         program  TYPE bdcdata-program,
         dynpro   TYPE bdcdata-dynpro,
         dynbegin TYPE bdcdata-dynbegin,
         fnam     TYPE bdcdata-fnam,
         fval     TYPE bdcdata-fval,
       END OF ty_bdcdata.

TYPES: BEGIN OF ty_entrada_txt,
         matnr TYPE char18, " Nº do Material
         maktx TYPE char40, " Descrição do Meterial
         meins TYPE char3,  " Unidade de medida basica
         brgew TYPE char17, " Peso Bruto
         gewei TYPE char3,  " Unidade de Peso
         volum TYPE char17, " Volume
         voleh TYPE char3,  " Unidade de Volume
       END OF ty_entrada_txt.

*&-------------------------*
*& Variables
*&-------------------------*
DATA: gv_filename TYPE string.
DATA: gv_texto    TYPE string.
DATA: gv_mode     TYPE c VALUE 'N'. " Modo BDC (N = Fundo)
DATA: gv_s        TYPE c VALUE 'S'. " Update síncrono
DATA: gv_ok       TYPE i VALUE 0.
DATA: gv_nok      TYPE i VALUE 0.
DATA: gv_lidos    TYPE i.

*&-------------------------*
*& Work Areas
*&-------------------------*
DATA: wa_entrada    TYPE ty_entrada_txt.
DATA: wa_bebida     TYPE ty_bebida.
DATA: wa_bebida_csv TYPE ty_entrada.
DATA: wa_bdcdata    TYPE ty_bdcdata.
DATA: wa_erro       TYPE zstlog.
DATA: wa_sucesso    TYPE zstlog.

*&-------------------------*
*& Internal Tables
*&-------------------------*
DATA: it_bebida     TYPE TABLE OF ty_bebida.
DATA: it_bebida_csv TYPE TABLE OF ty_entrada.
DATA: it_entrada    TYPE TABLE OF ty_entrada_txt.
DATA: it_bdcdata    TYPE TABLE OF ty_bdcdata.
DATA: it_msg        TYPE TABLE OF bdcmsgcoll WITH HEADER LINE.
DATA: it_erro       TYPE TABLE OF zstlog.
DATA: it_sucesso    TYPE TABLE OF zstlog.