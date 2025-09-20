*&---------------------------------------------------------------------*
*&  Include           ZPR_EXEC_29_BHS_TOP
*&---------------------------------------------------------------------*

*&-------------------------*
*& Tables
*&-------------------------*
TABLES: mkpf,
        mseg.

*&-------------------------*
*& Types
*&-------------------------*
TYPE-POOLS: slis.

TYPES: BEGIN OF ty_mkpf,
         mblnr TYPE mkpf-mblnr,
         mjahr TYPE mkpf-mjahr,
         bldat TYPE mkpf-bldat,
       END OF ty_mkpf.

TYPES: BEGIN OF ty_mseg,
         mblnr TYPE mseg-mblnr,
         mjahr TYPE mseg-mjahr,
         zeile TYPE mseg-zeile,
         bwart TYPE mseg-bwart,
         matnr TYPE mseg-matnr,
         werks TYPE mseg-werks,
         lgort TYPE mseg-lgort,
         dmbtr TYPE mseg-dmbtr,
         menge TYPE mseg-menge,
         meins TYPE mseg-meins,
       END OF ty_mseg.

TYPES: BEGIN OF ty_t001w,
         werks TYPE t001w-werks,
         name1 TYPE t001w-name1,
       END OF ty_t001w.

TYPES: BEGIN OF ty_makt,
         matnr TYPE makt-matnr,
         maktx TYPE makt-maktx,
       END OF ty_makt.

TYPES: BEGIN OF ty_t001l,
         werks TYPE t001l-werks,
         lgort TYPE t001l-lgort,
         lgobe TYPE t001l-lgobe,
       END OF ty_t001l.

TYPES: BEGIN OF ty_saida,
         bwart    TYPE mseg-bwart,
         bldat    TYPE mkpf-bldat,
         mblnr    TYPE mseg-mblnr,
         mjahr    TYPE mseg-mjahr,
         zeile    TYPE mseg-zeile,
         mat_mak  TYPE char64,
         wer_nam  TYPE char64,
         ort_obe  TYPE char64,
         menge    TYPE mseg-menge,
         meins    TYPE mseg-meins,
         dmbtr    TYPE mseg-dmbtr,
         valor(8) TYPE p DECIMALS 2,
       END OF ty_saida.

TYPES: BEGIN OF ty_header_alv,
        time TYPE t,
        date TYPE d,
       END OF ty_header_alv.

*&-------------------------*
*& Work Area
*&-------------------------*
DATA: wa_mkpf TYPE ty_mkpf.
DATA: wa_mseg TYPE ty_mseg.
DATA: wa_makt TYPE ty_makt.
DATA: wa_t001w TYPE ty_t001w.
DATA: wa_t001l TYPE ty_t001l.
DATA: wa_saida TYPE ty_saida.
DATA: wa_saida_aux TYPE ty_saida.
DATA: wa_fieldcat TYPE slis_fieldcat_alv. "Passará a formatação das colunas
DATA: wa_layout   TYPE  slis_layout_alv. "Pode passar configurações de layout da tabela
DATA: wa_sort TYPE slis_sortinfo_alv.
DATA: wa_csv TYPE string.
DATA: wa_header_alv TYPE ty_header_alv.
DATA: wa_top_of_page TYPE slis_listheader.

*&-------------------------*
*& Internal Tables
*&-------------------------*
DATA: it_fieldcat TYPE slis_t_fieldcat_alv. "Guardará a formatação das colunas
DATA: it_mkpf TYPE TABLE OF ty_mkpf.
DATA: it_mseg TYPE TABLE OF ty_mseg.
DATA: it_makt TYPE TABLE OF ty_makt.
DATA: it_t001w TYPE TABLE OF ty_t001w.
DATA: it_t001l TYPE TABLE OF ty_t001l.
DATA: it_saida TYPE TABLE OF ty_saida.
DATA: it_saida_aux TYPE TABLE OF ty_saida.
DATA: it_sort TYPE TABLE OF slis_sortinfo_alv.
DATA: it_header_alv TYPE TABLE OF ty_header_alv.
DATA: it_csv TYPE TABLE OF string.
DATA: it_top_of_page TYPE TABLE OF slis_listheader.

*&-------------------------*
*& Variables
*&-------------------------*
DATA: gv_result TYPE string.
DATA: gv_opc TYPE string.

*&-------------------------*
*& Constants
*&-------------------------*

*$--------------*
*$ Tables
*$--------------*
CONSTANTS: c_it_saida TYPE char16 VALUE 'IT_SAIDA'.
CONSTANTS: c_mkpf TYPE char5 VALUE 'MKPF'.
CONSTANTS: c_mseg TYPE char5 VALUE 'MSEG'.
CONSTANTS: c_makt TYPE char5 VALUE 'MAKT'.
CONSTANTS: c_t001w TYPE char5 VALUE 'T001W'.
CONSTANTS: c_t001l TYPE char5 VALUE 'T001L'.

*$--------------*
*$ Simbols
*$--------------*
CONSTANTS: c_x TYPE char2 VALUE 'X'.

*$--------------*
*$ Messages
*$--------------*
CONSTANTS: c_sucess TYPE c VALUE 'S'.
CONSTANTS: c_error TYPE c VALUE 'E'.
CONSTANTS: c_info TYPE c VALUE 'I'.

*$--------------*
*$ Words
*$--------------*
CONSTANTS: c_mblnr TYPE char5 VALUE 'MBLNR'.
CONSTANTS: c_mjahr TYPE char5 VALUE 'MJAHR'.
CONSTANTS: c_zeile TYPE char5 VALUE 'ZEILE'.
CONSTANTS: c_bwart TYPE char5 VALUE 'BWART'.
CONSTANTS: c_matnr TYPE char5 VALUE 'MATNR'.
CONSTANTS: c_werks TYPE char5 VALUE 'WERKS'.
CONSTANTS: c_lgort TYPE char5 VALUE 'LGORT'.
CONSTANTS: c_dmbtr TYPE char5 VALUE 'DMBTR'.
CONSTANTS: c_menge TYPE char5 VALUE 'MENGE'.
CONSTANTS: c_meins TYPE char5 VALUE 'MEINS'.
CONSTANTS: c_bldat TYPE char5 VALUE 'BLDAT'.
CONSTANTS: c_name1 TYPE char5 VALUE 'NAME1'.
CONSTANTS: c_maktx TYPE char5 VALUE 'MAKTX'.
CONSTANTS: c_lgobe TYPE char5 VALUE 'LGOBE'.
CONSTANTS: c_valor TYPE char5 VALUE 'VALOR'.
CONSTANTS: c_mat_mak TYPE char16 VALUE 'mat_mak'.
CONSTANTS: c_wer_nam TYPE char16 VALUE 'wer_nam'.
CONSTANTS: c_ort_obe TYPE char16 VALUE 'ort_obe'.