*&---------------------------------------------------------------------------------------*
*& REPORT zpr_exec_28_bhs.
*&---------------------------------------------------------------------------------------*
*& Nome:zpr_exec_28_bhs
*& Tipo: Report
*& Objetivo: Elaborar um programa ABAP onde deverá ser criada uma tela de seleção com
*& o campo Pagador como seleção múltipla, Documento de faturamento como seleção
*& múltipla e Organização de vendas como seleção única e valor default ‘3020’. Seus tipos
*& se encontram na tabela VBRK.
*& Selecionar na tabela VBRK todos os Documentos de faturamento que esteja de
*& acordo com o campo Pagador da tela de seleção, Documento de Faturamento da tela de
*& seleção, Organização de vendas da tela de seleção, Tipo documento de faturamento =
*& ‘F2’ e Moeda do documento SD = ‘USD’. Retornar os campos Documento de
*& Faturamento, Data doc.faturamento p/índice de docs.faturamto e Pagador.
*& Para cada registro encontrado na tabela VBRK, selecionar os itens de faturamento
*& na tabela VBRP onde o campo Documentos de faturamento relaciona as duas tabelas,
*& retornando os campos Documento de faturamento, Item do documento de faturamento,
*& Quantidade faturada efetivamente, Peso líquido, Peso bruto, Valor líquido do item de
*& faturamento em moeda do documento e Nº do material.
*& Para cada registro encontrado na tabela VBRK, selecionar na tabela KNA1 os
*& dados do Pagador onde o campo Pagador da tabela VBRK se relaciona com o campo Nº
*& cliente 1 da tabela KNA1 e o campo Chave do país = ‘US’. Retornar os campos Nº
*& cliente 1, Nome 1, Local, Região (país, estado, província, condado) e Rua e nº.
*& Para cada registro encontrado na tabela VBRP, selecionar na tabela MAKT a
*& descrição dos materiais onde o campo Nº do material relaciona as duas tabelas e o
*& campo Código de idioma = ‘PT’. Retornar os campos Nº do material e Texto breve de
*& material.
*& Para cada Pagador (quebra) encontrado na tabela VBRK o relatório deve exibir
*& seus Documentos de faturamento e itens do documento de faturamento. No final de cada
*& Documento de Faturamento deverá ser apresentada a soma dos campos Quantidade
*& faturada efetivamente, Peso líquido, Peso bruto, Valor líquido do item de faturamento em
*& moeda do documento. No final de cada Pagador também deverá ser apresentada a
*& soma dos campos Quantidade faturada efetivamente, Peso líquido, Peso bruto, Valor
*& líquido do item de faturamento em moeda do documento. Deverá ser exibido um
*& contador com a quantidade de Documento de Faturamento para um mesmo pagador e
*& no final um contador com a quantidade de registros encontrados.
*& Imprimir os campos: Pagador, Documento de Faturamento, Data doc.faturamento
*& p/índice de docs.faturamto, Nome 1, Local, Região, Rua e nº, Item do documento de
*& faturamento, Nº do material, Texto breve de material, Quantidade faturada efetivamente,
*& Peso líquido, Peso bruto e Valor líquido do item de faturamento em moeda do
*& documento.
*&---------------------------------------------------------------------------------------*

REPORT zpr_exec_28_bhs.

*&-------------------------*
*& Tables
*&-------------------------*
TABLES: vbrk.

*&-------------------------*
*& Types
*&-------------------------*
TYPES: BEGIN OF ty_vbrk,
         kunrg TYPE vbrk-kunrg,
         vbeln TYPE vbrk-vbeln,
         vkorg TYPE vbrk-vkorg,
         fkart TYPE vbrk-fkart,
         waerk TYPE vbrk-waerk,
         fkdat TYPE vbrk-fkdat,
       END OF ty_vbrk.

TYPES: BEGIN OF ty_vbrp,
         vbeln TYPE vbrp-vbeln,
         posnr TYPE vbrp-posnr,
         fkimg TYPE vbrp-fkimg,
         ntgew TYPE vbrp-ntgew,
         brgew TYPE vbrp-brgew,
         netwr TYPE vbrp-netwr,
         matnr TYPE vbrp-matnr,
       END OF ty_vbrp.

TYPES: BEGIN OF ty_kna1,
         kunnr TYPE kna1-kunnr,
         land1 TYPE kna1-land1,
         name1 TYPE kna1-name1,
         ort01 TYPE kna1-ort01,
         regio TYPE kna1-regio,
         stras TYPE kna1-stras,
       END OF ty_kna1.

TYPES: BEGIN OF ty_saida,
         kunrg TYPE vbrk-kunrg,
         vbeln TYPE vbrk-vbeln,
         vkorg TYPE vbrk-vkorg,
         fkart TYPE vbrk-fkart,
         fkdat TYPE vbrk-fkdat,
         posnr TYPE vbrp-posnr,
         fkimg TYPE vbrp-fkimg,
         ntgew TYPE vbrp-ntgew,
         brgew TYPE vbrp-brgew,
         netwr TYPE vbrp-netwr,
         matnr TYPE vbrp-matnr,
         name1 TYPE kna1-name1,
         ort01 TYPE kna1-ort01,
         regio TYPE kna1-regio,
       END OF ty_saida.

*&-------------------------*
*& Work Areas
*&-------------------------*
DATA: wa_vbrk TYPE ty_vbrk.
DATA: wa_vbrp TYPE ty_vbrp.
DATA: wa_kna1 TYPE ty_kna1.
DATA: wa_saida TYPE ty_saida.
DATA: wa_fieldcat TYPE slis_fieldcat_alv. "Passará a formatação das colunas
DATA: wa_layout   TYPE  slis_layout_alv. "Pode passar configurações de layout da tabela

*&-------------------------*
*& Internal Tables
*&-------------------------*
DATA: it_fieldcat TYPE slis_t_fieldcat_alv. "Guardará a formatação das colunas
DATA: it_vbrk TYPE TABLE OF ty_vbrk.
DATA: it_vbrp TYPE TABLE OF ty_vbrp.
DATA: it_kna1 TYPE TABLE OF ty_kna1.
DATA: it_saida TYPE TABLE OF ty_saida.

*&-------------------------*
*& Paramiters Selections
*&-------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

SELECT-OPTIONS: s_kunrg FOR vbrk-kunrg,
                s_vbeln FOR vbrk-vbeln,
                s_vkorg FOR vbrk-vkorg NO INTERVALS NO-EXTENSION DEFAULT '3020' OBLIGATORY.

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
*  -->  Seleção dos dados.
*----------------------------------------------------------------------*
FORM zf_select_data .
  SELECT kunrg
         vbeln
         vkorg
         fkart
         waerk
         fkdat
  FROM vbrk
  INTO TABLE it_vbrk
  WHERE
    kunrg IN s_kunrg AND
    vbeln IN s_vbeln AND
    vkorg IN s_vkorg AND
    fkart = 'F2'.

  SELECT vbeln
         posnr
         fkimg
         ntgew
         brgew
         netwr
         matnr
  FROM vbrp
  INTO TABLE it_vbrp
  FOR ALL ENTRIES IN it_vbrk
  WHERE
    vbeln = it_vbrk-vbeln.

  SELECT kunnr
         land1
         name1
         ort01
         regio
         stras
  FROM kna1
  INTO TABLE it_kna1
  FOR ALL ENTRIES IN it_vbrk
  WHERE
  kunnr = it_vbrk-kunrg.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Processamento dos dados selecionas.
*----------------------------------------------------------------------*
FORM zf_process_data .

  SORT it_vbrk BY vbeln.
  SORT it_vbrp BY vbeln.
  SORT it_kna1 BY kunnr.

  LOOP AT it_vbrk INTO wa_vbrk.

    READ TABLE it_vbrp INTO wa_vbrp WITH KEY vbeln = wa_vbrk-vbeln BINARY SEARCH.

    IF sy-subrc = 0.

      READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbrk-kunrg BINARY SEARCH.

    ENDIF.

    IF sy-subrc = 0.

      wa_saida-kunrg = wa_vbrk-kunrg.
      wa_saida-vbeln = wa_vbrk-vbeln.
      wa_saida-vkorg = wa_vbrk-vkorg.
      wa_saida-fkart = wa_vbrk-fkart.
      wa_saida-posnr = wa_vbrp-posnr.
      wa_saida-fkimg = wa_vbrp-fkimg.
      wa_saida-ntgew = wa_vbrp-ntgew.
      wa_saida-brgew = wa_vbrp-brgew.
      wa_saida-netwr = wa_vbrp-netwr.
      wa_saida-matnr = wa_vbrp-matnr.
      wa_saida-name1 = wa_kna1-name1.
      wa_saida-ort01 = wa_kna1-ort01.
      wa_saida-regio = wa_kna1-regio.

      APPEND wa_saida TO it_saida.
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Monta
* Monta e exibe o relátorio ALV
*----------------------------------------------------------------------*
FORM zf_display_alv .
  wa_fieldcat-fieldname = 'kunrg'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Pagador'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'vbeln'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Documento de faturamento'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'vkorg'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Organização de vendas'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'fkart'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Tipo documento de faturamento'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'posnr'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Item do documento de faturamento'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'fkimg'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Quantidade faturada efetivamente'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'ntgew'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Peso líquido'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'brgew'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Peso bruto'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'netwr'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Valor líquido do item de faturamento em moeda do documento'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'matnr'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Nº do material'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'name1'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Nome 1'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'ort01'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Local'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'regio'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Região'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_layout-colwidth_optimize = abap_true. "Otima o tamanho da coluna conforme o tamanho da descrição.
  wa_layout-zebra = 'X'. "Faz o relátio ser exibido em zebra.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      i_grid_title       = ''
      is_layout          = wa_layout "Aqui passa o Layout do ALV
      it_fieldcat        = it_fieldcat "Aqui é passado a estrutura do ALV
    TABLES
      t_outtab           = it_saida. "Aqui passa os valores a serem exibidos no ALV

  IF sy-subrc NE 0.
    MESSAGE s208(001) DISPLAY LIKE sy-abcde+4(1) WITH text-e02.
  ENDIF.
ENDFORM.