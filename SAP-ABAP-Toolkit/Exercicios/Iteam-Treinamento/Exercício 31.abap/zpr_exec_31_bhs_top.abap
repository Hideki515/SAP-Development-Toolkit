*&---------------------------------------------------------------------*
*&  Include           ZPR_EXEC_31_BHS_TOP
*&---------------------------------------------------------------------*

*&-------------------------*
*& TYPE-POOLS
*&-------------------------*
TYPE-POOLS: slis.

*&-------------------------*
*& Tables
*&-------------------------*
TABLES: vbrk, ztb_exec_3.

*&-------------------------*
*& Types
*&-------------------------*
TYPES: BEGIN OF ty_vbrk,
         vbeln TYPE vbrk-vbeln,
         fkdat TYPE vbrk-fkdat,
         kunrg TYPE vbrk-kunrg,
       END OF ty_vbrk.

TYPES: BEGIN OF ty_vbrp,
         vbeln TYPE vbrp-vbeln,
         posnr TYPE vbrp-posnr,
         matnr TYPE vbrp-matnr,
         fkimg TYPE vbrp-fkimg,
         vrkme TYPE vbrp-vrkme,
         netwr TYPE vbrp-netwr,
         aubel TYPE vbrp-aubel,
       END OF ty_vbrp.

TYPES: BEGIN OF ty_vbak,
         vbeln TYPE vbak-vbeln,
       END OF ty_vbak.

TYPES: BEGIN OF ty_kna1,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
       END OF ty_kna1.

TYPES: BEGIN OF ty_makt,
         matnr TYPE makt-matnr,
         maktx TYPE makt-maktx,
       END OF ty_makt.

TYPES: BEGIN OF ty_saida,
         mark      TYPE flag,
         vbeln     TYPE vbrk-vbeln,
         posnr     TYPE vbrp-posnr,
         fkdat     TYPE vbrk-fkdat,
         kun_nam   TYPE ztb_exec_3-kun_nam,
         mat_mak   TYPE ztb_exec_3-mat_mak,
         fkimg     TYPE vbrp-fkimg,
         vrkme     TYPE vbrp-vrkme,
         netwr     TYPE vbrp-netwr,
         aubel     TYPE vbrp-aubel,
         status    TYPE char8,
         cellcolor TYPE lvc_t_scol,
       END OF ty_saida.

TYPES: BEGIN OF ty_found,
         vbeln TYPE vbrp-vbeln,
         posnr TYPE vbrp-posnr,
       END OF ty_found.

*&-------------------------*
*& Work Areas
*&-------------------------*
DATA: wa_vbrk   TYPE ty_vbrk.
DATA: wa_vbrp   TYPE ty_vbrp.
DATA: wa_vbak   TYPE ty_vbak.
DATA: wa_kna1   TYPE ty_kna1.
DATA: wa_makt   TYPE ty_makt.
DATA: wa_saida  TYPE ty_saida.
DATA: wa_saida_aux  TYPE ty_saida.
DATA: wa_saida_exp  TYPE ztb_exec_3.
DATA: wa_found TYPE ty_found.
DATA: wa_cellcolor  TYPE lvc_s_scol.

"Config ALV
DATA: wa_sort_alv     TYPE slis_sortinfo_alv.
DATA: wa_top_of_page  TYPE slis_listheader.
DATA: wa_layout_alv   TYPE slis_layout_alv.
DATA: wa_fieldcat_alv TYPE slis_fieldcat_alv.

*&-------------------------*
*& Internal Tables
*&-------------------------*
DATA: it_vbrk  TYPE TABLE OF ty_vbrk.
DATA: it_vbrp  TYPE TABLE OF ty_vbrp.
DATA: it_vbak  TYPE TABLE OF ty_vbak.
DATA: it_kna1  TYPE TABLE OF ty_kna1.
DATA: it_makt  TYPE TABLE OF ty_makt.
DATA: it_saida TYPE TABLE OF ty_saida.
DATA: it_saida_aux TYPE TABLE OF ty_saida.
DATA: it_saida_exp TYPE TABLE OF ztb_exec_3.
DATA: it_found TYPE TABLE OF ty_found.

"Config ALV
DATA: it_top_of_page  TYPE TABLE OF slis_listheader.
DATA: it_sort_alv     TYPE TABLE OF slis_sortinfo_alv.
DATA: it_fieldcat_alv TYPE TABLE OF slis_fieldcat_alv.
DATA: it_layout_alv   TYPE TABLE OF slis_layout_alv.

*&-------------------------*
*& Constants
*&-------------------------*

"Filds
CONSTANTS: c_mark    TYPE char4 VALUE 'MARK'.
CONSTANTS: c_vbeln   TYPE char5 VALUE 'VBELN'.
CONSTANTS: c_posnr   TYPE char5 VALUE 'POSNR'.
CONSTANTS: c_fkdat   TYPE char5 VALUE 'FKDAT'.
CONSTANTS: c_kun_nam TYPE char7 VALUE 'KUN_NAM'.
CONSTANTS: c_mat_mak TYPE char7 VALUE 'MAT_MAK'.
CONSTANTS: c_fkimg   TYPE char5 VALUE 'FKIMG'.
CONSTANTS: c_vrkme   TYPE char5 VALUE 'VRKME'.
CONSTANTS: c_netwr   TYPE char5 VALUE 'NETWR'.
CONSTANTS: c_aubel   TYPE char5 VALUE 'AUBEL'.

"Tables
CONSTANTS: c_tab_saida TYPE char8 VALUE 'IT_SAIDA'.
CONSTANTS: c_tab_vbrp  TYPE char4 VALUE 'VBRP'.
CONSTANTS: c_tab_vbak  TYPE char4 VALUE 'VBAK'.
CONSTANTS: c_tab_vbrk  TYPE char4 VALUE 'VBRK'.
CONSTANTS: c_tab_kna1  TYPE char4 VALUE 'KNA1'.
CONSTANTS: c_tab_makt  TYPE char4 VALUE 'MAKT'.

"Simbols
CONSTANTS: c_x TYPE c VALUE 'X'.

"Message
CONSTANTS: c_sucess TYPE c VALUE 'S'.
CONSTANTS: c_error  TYPE c VALUE 'E'.
CONSTANTS: c_info   TYPE c VALUE 'I'.