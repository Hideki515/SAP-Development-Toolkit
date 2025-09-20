*&---------------------------------------------------------------------------------------*
*& REPORT zpr_exec_09_bhs.
*&---------------------------------------------------------------------------------------*
*& Nome:zpr_exec_09_bhs
*& Tipo: Report
*& Objetivo: Faça uma rotina que deve conter uma workarea com 5 campos de tipos
*& diferentes ou mais, esta deve ser populada e os seus campos devem ser impressos um
*& em cada linha, separados por duas linhas horizontais.
*&---------------------------------------------------------------------------------------*

REPORT zpr_exec_11_bhs.

*&-------------------------*
*& Types
*&-------------------------*
*&Cria o typo da work area.
TYPES: BEGIN OF ty_dados,
         carrid TYPE sflight-carrid,
         connid TYPE sflight-connid,
         fldate TYPE sflight-fldate,
         price  TYPE sflight-price,
         max    TYPE sflight-seatsmax,
       END OF ty_dados.

*&-------------------------*
*& Work Areas
*&-------------------------*
*&Cria e define o tipo da work area.
DATA wa_data TYPE ty_dados.

*&-------------------------*
*& Performs
*&-------------------------*
*&Chama o form que seleciona os dados.
PERFORM zfm_select_data.

*&---------------------------------------------------------------------*
*& Form ZFM_SELECT_DATA
*&---------------------------------------------------------------------*
*& Seleciona os dados a serem exibidos.
*&---------------------------------------------------------------------*
FORM zfm_select_data.

  SELECT SINGLE"Seleciona os valores da primeira linha.
    carrid
    connid
    fldate
    price
    seatsmax
    FROM sflight "Passa de qual tabela os valores seram selecionados.
    INTO wa_data. "Informa onde os valore seram guardados.

  PERFORM zf_display_alv_sflight. "Chama o form que exibe os dados.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form ZF_ULINE
*&---------------------------------------------------------------------*
*& Coloca dois uline.
*&---------------------------------------------------------------------*
FORM zf_uline.

  ULINE.
  ULINE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form ZF_DISPLAY_ALV_SFLIGHT.
*&---------------------------------------------------------------------*
*& Exibe os valores.
*&---------------------------------------------------------------------*
FORM zf_display_alv_sflight.

  WRITE: 'Companhia Vôo:', wa_data-carrid.
  PERFORM zf_uline.
  WRITE: 'Código Conexão Vôo:', wa_data-connid.
  PERFORM zf_uline.
  WRITE: 'Data Vôo:', wa_data-fldate.
  PERFORM zf_uline.
  WRITE: 'Preo Vôo:', wa_data-price.
  PERFORM zf_uline.
  WRITE: 'Ocupação Max:', wa_data-max.
  PERFORM zf_uline.

ENDFORM.