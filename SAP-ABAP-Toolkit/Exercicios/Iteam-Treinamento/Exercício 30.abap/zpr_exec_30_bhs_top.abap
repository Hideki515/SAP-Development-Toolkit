*&---------------------------------------------------------------------*
*&  Include           ZPR_EXEC_30_BHS_TOP
*&---------------------------------------------------------------------*

*&-------------------------*
*& Tables
*&-------------------------*
TABLES: vbak.

*&-------------------------*
*& Types
*&-------------------------*
TYPES: BEGIN OF ty_vbak,
         vbeln TYPE vbak-vbeln,
         erdat TYPE vbak-erdat,
         netwr TYPE vbak-netwr,
         kunnr TYPE vbak-kunnr,
       END OF ty_vbak.

TYPES: BEGIN OF ty_vbap,
         vbeln TYPE vbap-vbeln,
         posnr TYPE vbap-posnr,
         matnr TYPE vbap-matnr,
         gsber TYPE vbap-gsber,
       END OF ty_vbap.

TYPES: BEGIN OF ty_lips,
         vbeln TYPE lips-vbeln,
         posnr TYPE lips-posnr,
         vgbel TYPE lips-vgbel,
         vgpos TYPE lips-vgpos,
       END OF ty_lips.

TYPES: BEGIN OF ty_kna1,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
       END OF ty_kna1.

TYPES: BEGIN OF ty_makt,
         matnr TYPE makt-matnr,
         maktx TYPE makt-maktx,
       END OF ty_makt.

TYPES: BEGIN OF ty_tgsbt,
         gsber TYPE tgsbt-gsber,
         gtext TYPE tgsbt-gtext,
       END OF ty_tgsbt.

TYPES: BEGIN OF ty_saida,
         vbeln     TYPE vbak-vbeln,
         erdat     TYPE vbak-erdat,
         posnr     TYPE vbap-posnr,
         kun_nam   TYPE char50,
         netwr     TYPE vbak-netwr,
         mat_mak   TYPE char50,
         gsb_gtx   TYPE char50,
         vbeln_lip TYPE char50,
         status    TYPE char50,
       END OF ty_saida.

*&-------------------------*
*& Work Area
*&-------------------------*
DATA: wa_vbak      TYPE ty_vbak.
DATA: wa_vbap      TYPE ty_vbap.
DATA: wa_lips      TYPE ty_lips.
DATA: wa_kna1      TYPE ty_kna1.
DATA: wa_makt      TYPE ty_makt.
DATA: wa_tgsbt     TYPE ty_tgsbt.
DATA: wa_saida     TYPE ty_saida.
DATA: wa_saida_aux TYPE ty_saida.
DATA: wa_csv       TYPE string.

"Config ALV
DATA: wa_sort_alv     TYPE slis_sortinfo_alv.
DATA: wa_top_of_page  TYPE slis_listheader.
DATA: wa_layout_alv   TYPE slis_layout_alv.
DATA: wa_fieldcat_alv TYPE slis_fieldcat_alv.

*&-------------------------*
*& Internal Tables
*&-------------------------*
DATA: it_vbak       TYPE TABLE OF ty_vbak.
DATA: it_vbap       TYPE TABLE OF ty_vbap.
DATA: it_lips       TYPE TABLE OF ty_lips.
DATA: it_kna1       TYPE TABLE OF ty_kna1.
DATA: it_makt       TYPE TABLE OF ty_makt.
DATA: it_tgsbt      TYPE TABLE OF ty_tgsbt.
DATA: it_saida      TYPE TABLE OF ty_saida.
DATA: it_saida_aux  TYPE TABLE OF ty_saida.
DATA: it_csv        TYPE TABLE OF string.

"Config ALV
DATA: it_top_of_page  TYPE TABLE OF slis_listheader.
DATA: it_sort_alv     TYPE TABLE OF slis_sortinfo_alv.
DATA: it_fieldcat_alv TYPE TABLE OF slis_fieldcat_alv.
DATA: it_layout_alv   TYPE TABLE OF slis_layout_alv.

*&-------------------------*
*& Constants
*&-------------------------*

"Simbols
CONSTANTS: c_x TYPE c VALUE 'X'.

"Messages
CONSTANTS: c_sucess TYPE c VALUE 'S'.
CONSTANTS: c_error  TYPE c VALUE 'E'.
CONSTANTS: c_info   TYPE c VALUE 'I'.

"Farois
CONSTANTS: c_sem_green  TYPE char8 VALUE '@S_TL_G@'.
CONSTANTS: c_sem_yellow TYPE char8 VALUE '@S_TL_Y@'.
CONSTANTS: c_sem_red    TYPE char8 VALUE '@S_TL_R@'.

"Fields
CONSTANTS: c_mark        TYPE char5 VALUE   'MARK'.
CONSTANTS: c_vbeln       TYPE char5 VALUE   'VBELN'.
CONSTANTS: c_erdat       TYPE char5 VALUE   'ERDAT'.
CONSTANTS: c_posnr       TYPE char5 VALUE   'POSNR'.
CONSTANTS: c_kun_nam     TYPE char7 VALUE   'KUN_NAM'.
CONSTANTS: c_netwr       TYPE char5 VALUE   'NETWR'.
CONSTANTS: c_mat_mak     TYPE char7 VALUE   'MAT_MAK'.
CONSTANTS: c_gsb_gtx     TYPE char11 VALUE  'GSB_GTX'.
CONSTANTS: c_vbeln_lip   TYPE char11 VALUE  'VBELN_LIP'.
CONSTANTS: c_status      TYPE char8 VALUE   'STATUS'.

"Tables
CONSTANTS: c_tab_fieldcat TYPE char16 VALUE 'IT_FIELDCAT_ALV'.
CONSTANTS: c_tab_layout   TYPE char13 VALUE 'IT_LAYOUT_ALV'.
CONSTANTS: c_tab_top      TYPE char14 VALUE 'IT_TOP_OF_PAGE'.
CONSTANTS: c_tab_saida    TYPE char9  VALUE 'it_saida'.
CONSTANTS: c_tab_vbak     TYPE char4  VALUE 'VBAK'.
CONSTANTS: c_tab_vbap     TYPE char4  VALUE 'VBAP'.
CONSTANTS: c_tab_lips     TYPE char4  VALUE 'LIPS'.
CONSTANTS: c_tab_kna1     TYPE char4  VALUE 'KNA1'.
CONSTANTS: c_tab_makt     TYPE char4  VALUE 'MAKT'.
CONSTANTS: c_tab_tgsbt    TYPE char4  VALUE 'TGSBT'.
CONSTANTS: c_tab_saida_aux TYPE char14 VALUE 'IT_SAIDA_AUX'.

"Tipos Arquivo
CONSTANTS: c_txt TYPE char5 VALUE '.txt'.
CONSTANTS: c_csv TYPE char5 VALUE '.csv'.