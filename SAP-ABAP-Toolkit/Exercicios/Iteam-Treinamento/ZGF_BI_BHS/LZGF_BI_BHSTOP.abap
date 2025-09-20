FUNCTION-POOL ZGF_BI_BHS.                   "MESSAGE-ID ..

TYPES: BEGIN OF ty_bebida,
         matnr      TYPE mara-matnr,
         maktx      TYPE makt-maktx,
         meins      TYPE mara-meins,
         brgew      TYPE char50,
         gewei      TYPE mara-gewei,
         volum      TYPE char50,
         voleh      TYPE mara-voleh,
         status     TYPE c         , " Status do registro (S/E)
         observacao TYPE char100         , " Mensagem retornada
       END OF ty_bebida.

TYPES: BEGIN OF ty_log,
         status     TYPE c         , " Status do registro (S/E)
         observacao TYPE char100         , " Mensagem retornada
       END OF ty_log.

DATA: gv_texto    TYPE string.
DATA: gv_mode     TYPE c VALUE 'N'. " Modo BDC (N = Fundo)
DATA: gv_s        TYPE c VALUE 'S'. " Update síncrono
DATA: gv_ok       TYPE i VALUE 0.
DATA: gv_nok      TYPE i VALUE 0.
DATA: gv_lidos    TYPE i.

DATA: wa_bebida     TYPE ty_bebida.
DATA: wa_bdcdata    TYPE bdcdata.
DATA: wa_log        TYPE ty_log.

DATA: it_bebida     TYPE TABLE OF ty_bebida.
DATA: it_bdcdata    TYPE TABLE OF bdcdata.
DATA: it_msg        TYPE TABLE OF bdcmsgcoll.