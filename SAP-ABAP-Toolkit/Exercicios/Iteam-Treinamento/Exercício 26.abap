*&---------------------------------------------------------------------------------------*
*& REPORT zpr_exec_25_bhs.
*&---------------------------------------------------------------------------------------*
*& Nome:zpr_exec_25_bhs
*& Tipo: Report
*& Objetivo: Elaborar um programa ABAP onde deverá ser criada uma tela de seleção com
*& Elaborar um programa ABAP onde deverá ser criada uma tela de seleção com
*& o Centro de lucro como seleção múltipla e a Área de contabilidade de custos também
*& como seleção múltipla. Seus tipos se encontram na tabela CEPC.
*& Selecionar na tabela CEPC todos os Centros de lucro e Área de contabilidade de
*& custos que estiverem de acordo com os dois campos da tela de seleção e a Data de
*& validade final for igual a 31.12.9999, retornando os campos Centro de lucro, Data de
*& validade final, Área de contabilidade de custos, Data início validade e Criado por.
*& Para cada registro encontrado na tabela CEPC, selecionar na tabela CEPCT as
*& descrições dos Centros de lucro, onde Código de idioma = ‘PT’ e as duas tabelas são
*& relacionadas pelos campos Centro de lucro, Data de validade final e Área de
*& contabilidade de custos, retornando os campos Centro de lucro, Data de validade final,
*& Área de contabilidade de custos e Texto descritivo.
*& Para cada registro encontrado na tabela CEPC, selecionar na tabela TKA01 as
*& descrições das Áreas de contabilidade de custos encontradas onde, o campo Área de
*& contabilidade de custos relaciona as duas tabelas. Retornar os campos Área de
*& contabilidade de custos e Denominação da área de contabilidade de custos
*& O relatório deve imprimir todos os Centros de Lucro de cada Área de contabilidade
*& de custos selecionada. Para cada Área de contabilidade de custos deverá mostrar um
*& contador de Centros de Lucro e no final do relatório a quantidade de registros
*& encontrados.
*& Imprimir os campos: Área de contabilidade de custos, Denominação da área de
*& contabilidade de custos, Centro de lucro, Texto descritivo, Criado por, Data início
*& validade, Data de validade final.
*&---------------------------------------------------------------------------------------*

REPORT zpr_exec_26_bhs.

*&-------------------------*
*& Tables
*&-------------------------*
TABLES: cepc.

*&-------------------------*
*& Types
*&-------------------------*
TYPES: BEGIN OF ty_cepc,
         prctr TYPE cepc-prctr,
         kokrs TYPE cepc-kokrs,
         datbi TYPE cepc-datbi,
         datab TYPE cepc-datab,
         usnam TYPE cepc-usnam,
       END OF ty_cepc.

TYPES: BEGIN OF ty_cepct,
         prctr TYPE cepct-prctr,
         ltext TYPE cepct-ltext,
         spras TYPE cepct-spras,
       END OF ty_cepct.

TYPES: BEGIN OF ty_tka01,
         kokrs TYPE tka01-kokrs,
         bezei TYPE tka01-bezei,
       END OF ty_tka01.

TYPES: BEGIN OF ty_saida,
         prctr TYPE cepc-prctr,
         kokrs TYPE cepc-kokrs,
         bezei TYPE tka01-bezei,
         datbi TYPE cepc-datbi,
         datab TYPE cepc-datab,
         usnam TYPE cepc-usnam,
         ltext TYPE cepct-ltext,
       END OF ty_saida.

*&-------------------------*
*& Work Area
*&-------------------------*
DATA: wa_cepc TYPE ty_cepc.
DATA: wa_cepct TYPE ty_cepct.
DATA: wa_tka01 TYPE ty_tka01.
DATA: wa_saida TYPE ty_saida.
DATA: wa_fieldcat TYPE slis_fieldcat_alv. "Passará a formatação das colunas
DATA: wa_layout   TYPE  slis_layout_alv. "Pode passar configurações de layout da tabela

*&-------------------------*
*& Internal Tables
*&-------------------------*
DATA: it_fieldcat TYPE slis_t_fieldcat_alv. "Guardará a formatação das colunas
DATA: it_cepc TYPE TABLE OF ty_cepc.
DATA: it_cepct TYPE TABLE OF ty_cepct.
DATA: it_tka01 TYPE TABLE OF ty_tka01.
DATA: it_saida TYPE TABLE OF ty_saida.

*&-------------------------*
*& Paramiters Selections
*&-------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

SELECT-OPTIONS: s_prctr FOR cepc-prctr,
                s_kokrs FOR cepc-kokrs.

SELECTION-SCREEN END OF BLOCK b1.

*&-------------------------*
*& Performs
*&-------------------------*
PERFORM zf_select_data.
PERFORM zf_process_data.
PERFORM zf_display_alv.

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_select_data .

  SELECT
    prctr
    kokrs
    datbi
    datab
    usnam
    FROM cepc
    INTO TABLE it_cepc
    WHERE
    prctr IN s_prctr AND
    kokrs IN s_kokrs AND
    datbi = '99991231'.

  IF it_cepc IS NOT INITIAL.

    SELECT
      prctr
      ltext
      spras
      FROM cepct
      INTO TABLE it_cepct
      FOR ALL ENTRIES IN it_cepc
      WHERE
      prctr = it_cepc-prctr AND
      spras = 'PT'.

  ENDIF.

  IF it_cepc IS NOT INITIAL.

    SELECT
      kokrs
      bezei
      FROM tka01
      INTO TABLE it_tka01
      FOR ALL ENTRIES IN it_cepc
      WHERE
      kokrs = it_cepc-kokrs.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Processa os dados
*----------------------------------------------------------------------*
FORM zf_process_data .

  SORT it_cepc BY prctr.
  SORT it_cepct BY prctr.
  SORT it_tka01 BY kokrs.

  LOOP AT it_cepc INTO wa_cepc.

    READ TABLE it_cepct INTO wa_cepct WITH KEY prctr = wa_cepc-prctr.

    IF sy-subrc = 0.

      READ TABLE it_tka01 INTO wa_tka01 WITH KEY kokrs = wa_cepc-kokrs.

    ENDIF.

    IF sy-subrc = 0.

      wa_saida-prctr = wa_cepc-prctr.
      wa_saida-kokrs = wa_cepc-kokrs.
      wa_saida-bezei = wa_tka01-bezei.
      wa_saida-datbi = wa_cepc-datbi.
      wa_saida-datab = wa_cepc-datab.
      wa_saida-usnam = wa_cepc-usnam.
      wa_saida-ltext = wa_cepct-ltext.

      APPEND wa_saida TO it_saida.

    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Exibe os valores do relatório ALV.
*----------------------------------------------------------------------*
FORM zf_display_alv .
  wa_fieldcat-fieldname = 'prctr'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Centro de lucro'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'kokrs'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Área de contabilidade de custos'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'bezei'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Denominação da área de contabilidade de custos'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'datbi'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Data de validade fina'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'datab'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Data início validade'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'usnam'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Criado por'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'ltext'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Texto descritivo'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_layout-colwidth_optimize = abap_true. "Otima o tamanho da coluna conforme o tamanho da descrição.
  wa_layout-zebra = 'X'. "Faz o relátio ser exibido em zebra.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      i_grid_title       = 'Empresa'
      is_layout          = wa_layout "Aqui passa o Layout do ALV
      it_fieldcat        = it_fieldcat "Aqui é passado a estrutura do ALV
    TABLES
      t_outtab           = it_saida. "Aqui passa os valores a serem exibidos no ALV

  IF sy-subrc NE 0.
    MESSAGE s208(001) DISPLAY LIKE sy-abcde+4(1) WITH text-e02.
  ENDIF.
ENDFORM.