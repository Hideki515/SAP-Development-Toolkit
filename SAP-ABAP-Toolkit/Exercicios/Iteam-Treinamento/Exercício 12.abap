*&---------------------------------------------------------------------------------------*
*& REPORT zpr_exec_12_bhs.
*&---------------------------------------------------------------------------------------*
*& Nome:zpr_exec_12_bhs
*& Tipo: Report
*& Objetivo: Faça uma rotina que receba uma workarea contendo 5 tipos de dados
*& diferentes e conte quantos campos não estão preenchidos. Imprimir resultado.
*&---------------------------------------------------------------------------------------*

REPORT zpr_exec_12_bhs.

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
*& Variables
*&-------------------------*
DATA gv_qtd(3) TYPE i.

START-OF-SELECTION.
*&-------------------------*
*& Performs
*&-------------------------*
*&Chama o form que seleciona os dados.
  PERFORM zfm_select_data.

END-OF-SELECTION.
  WRITE: 'A quantidade de campos vazios é:', gv_qtd.

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

*  wa_data-carrid = ''.
*  wa_data-connid = ''.
*  wa_data-fldate = ''.
*  wa_data-max = ''.
*  wa_data-price = ''.

  PERFORM zf_count CHANGING gv_qtd. "Chama o form que exibe os dados.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form ZF_COUNT
*&---------------------------------------------------------------------*
*& Verifica quais campos vazios.
*&---------------------------------------------------------------------*
FORM zf_count CHANGING VALUE(gv_qtd).

  IF wa_data-carrid = ''. "Verifica se o campo da work area está vazio.
    ADD 1 TO gv_qtd. "Adiciona mais um na contagem dos campos vazios.
  ENDIF.
  IF wa_data-connid = ''. "Verifica se o campo da work area está vazio.
    ADD 1 TO gv_qtd. "Adiciona mais um na contagem dos campos vazios.
  ENDIF.
  IF wa_data-fldate = ''. "Verifica se o campo da work area está vazio.
    ADD 1 TO gv_qtd. "Adiciona mais um na contagem dos campos vazios.
  ENDIF.
  IF wa_data-price = ''. "Verifica se o campo da work area está vazio.
    ADD 1 TO gv_qtd. "Adiciona mais um na contagem dos campos vazios.
  ENDIF.
  IF wa_data-max = ''. "Verifica se o campo da work area está vazio.
    ADD 1 TO gv_qtd. "Adiciona mais um na contagem dos campos vazios.
  ENDIF.

ENDFORM.