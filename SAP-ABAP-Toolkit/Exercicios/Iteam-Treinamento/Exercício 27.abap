*&---------------------------------------------------------------------------------------*
*& REPORT zpr_exec_27_bhs.
*&---------------------------------------------------------------------------------------*
*& Nome:zpr_exec_27_bhs
*& Tipo: Report
*& Objetivo: Elaborar um programa ABAP onde deverá ser criada uma tela de seleção com
*& o campo Fornecimento como seleção múltipla e Itinerário como seleção múltipla. Seus
*& tipos se encontram na tabela LIKP.
*& Selecionar na tabela LIKP todos os Fornecimentos criados pelo usuário
*& ‘MMUELLER’ (Nome do responsável que adicionou o objeto), que sejam do Local de
*& expedição/local de recebimento de mercadoria = ‘1200’, Organização de vendas = ‘1000’
*& e que tenham Itinerários definidos (Itinerário <> branco), onde o campo Fornecimento
*& seja filtrado pelo campo Fornecimento da tela de seleção e o campo Itinerário seja
*& filtrado pelo campo Itinerário da tela de seleção, retornando os campos Fornecimento,
*& Data de criação do registro e Itinerário.
*& Para cada registro encontrado na tabela LIKP, selecionar os Itens de
*& Fornecimento na tabela LIPS onde o campo Fornecimento relaciona as duas tabelas,
*& retornando os campos Fornecimento, Item de remessa, Nº do material, Centro,
*& Quantidade fornecida de fato em UMV e Peso líquido.
*& Para cada registro encontrado na tabela LIKP, selecionar na tabela TVROT as
*& descrições dos Itinerários selecionados desde que estas existam com Código de Idioma
*& ‘PT’, onde o campo Itinerário relaciona as duas tabelas, retornando os campos: Itinerário
*& e Denominação do Itinerário.
*& Para cada Itinerário (quebra) encontrado na tabela LIKP, o relatório deve exibir
*& seus fornecimentos e itens de fornecimento. No final de cada itinerário deverá ser
*& apresentada uma soma dos campos Quantidade fornecida de fato em UMV e Peso
*& líquido. Deverá também ser exibido um contador com a quantidade de registros
*& encontrados.
*& Imprimir os campos: Itinerário, Denominação do Itinerário, Fornecimento, Data de
*& criação do registro, Item de remessa, Nº do material, Centro, Quantidade fornecida de
*& fato em UMV e Peso líquido.
*&---------------------------------------------------------------------------------------*

REPORT zpr_exec_27_bhs.

TABLES: likp.

TYPES: BEGIN OF ty_likp,
         vbeln TYPE likp-vbeln,
         route TYPE likp-route,
         ernam TYPE likp-ernam,
         vstel TYPE likp-vstel,
         vkorg TYPE likp-vkorg,
         aulwe TYPE likp-aulwe,
         erdat TYPE likp-erdat,
       END OF ty_likp.

TYPES: BEGIN OF ty_lips,
         vbeln TYPE lips-vbeln,
         posnr TYPE lips-posnr,
         matnr TYPE lips-matnr,
         werks TYPE lips-werks,
         lfimg TYPE lips-lfimg,
         ntgew TYPE lips-ntgew,
       END OF ty_lips.

TYPES: BEGIN OF ty_tvrot,
         route TYPE tvrot-route,
         bezei TYPE tvrot-bezei,
         spras TYPE tvrot-spras,
       END OF ty_tvrot.

TYPES: BEGIN OF ty_saida,
         route TYPE tvrot-route,
         bezei TYPE tvrot-bezei,
         vbeln TYPE likp-vbeln,
         erdat TYPE likp-erdat,
         posnr TYPE lips-posnr,
         matnr TYPE lips-matnr,
         werks TYPE lips-werks,
         lfimg TYPE lips-lfimg,
         ntgew TYPE lips-ntgew,
       END OF ty_saida.

DATA: wa_likp TYPE ty_likp.
DATA: wa_lips TYPE ty_lips.
DATA: wa_tvrot TYPE ty_tvrot.
DATA: wa_saida TYPE ty_saida.
DATA: wa_fieldcat TYPE slis_fieldcat_alv. "Passará a formatação das colunas
DATA: wa_layout   TYPE  slis_layout_alv. "Pode passar configurações de layout da tabela
DATA: wa_sort TYPE slis_sortinfo_alv.

*&-------------------------*
*& Internal Tables
*&-------------------------*
DATA: it_fieldcat TYPE slis_t_fieldcat_alv. "Guardará a formatação das colunas
DATA: it_sort TYPE TABLE OF slis_sortinfo_alv.
DATA: it_likp TYPE TABLE OF ty_likp.
DATA: it_lips TYPE TABLE OF ty_lips.
DATA: it_tvrot TYPE TABLE OF ty_tvrot.
DATA: it_saida TYPE TABLE OF ty_saida.

*&-------------------------*
*& Selections Paramiters
*&-------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

SELECT-OPTIONS: s_vbeln FOR likp-vbeln,
                s_route FOR likp-route.

SELECTION-SCREEN END OF BLOCK b1.

*&-------------------------*
*& Performs
*&-------------------------*
PERFORM zf_select_data.
PERFORM zf_process_data.
PERFORM zf_sort_alv.
PERFORM zf_display_alv.

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Seleciona os dados.
*----------------------------------------------------------------------*
FORM zf_select_data .
  SELECT
    vbeln
    route
    ernam
    vstel
    vkorg
    aulwe
    erdat
    FROM likp
    INTO TABLE it_likp
    WHERE
    vbeln IN s_vbeln AND
    route IN s_route AND
    ernam = 'MMUELLER' AND
    vstel = '1200' AND
    vkorg = '1000'.

  IF it_likp IS NOT INITIAL.

    SELECT
      vbeln
      posnr
      matnr
      werks
      lfimg
      ntgew
      FROM lips
      INTO TABLE it_lips
      FOR ALL ENTRIES IN it_likp
      WHERE
      vbeln = it_likp-vbeln.

  ENDIF.

  IF it_likp IS NOT INITIAL.

    SELECT
      route
      bezei
      spras
      FROM tvrot
      INTO TABLE it_tvrot
      FOR ALL ENTRIES IN it_likp
      WHERE
      spras = 'PT' AND
      route = it_likp-route.

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

  SORT it_likp BY route.
  SORT it_lips BY vbeln.
  SORT it_tvrot BY route.

  LOOP AT it_likp INTO wa_likp. "Loop passando os dados da linha para uma work area.

    IF wa_likp-route IS INITIAL. "Verifica se o itinerário está vazio.

      CONTINUE. "Pula do laço atual para o próximo.

    ELSE.

      READ TABLE it_lips INTO wa_lips WITH KEY vbeln = wa_likp-vbeln BINARY SEARCH. "Lê uma linha da tabela interna e adiciona a Work Area.

      IF sy-subrc = 0.

        READ TABLE it_tvrot INTO wa_tvrot WITH KEY route = wa_likp-route BINARY SEARCH. "Lê uma linha da tabela interna e adiciona a Work Area.

      ENDIF.

      IF sy-subrc = 0.

        CLEAR wa_saida. "Limpa os valores da Work Area.

        wa_saida-route = wa_tvrot-route.
        wa_saida-bezei = wa_tvrot-bezei.
        wa_saida-vbeln = wa_lips-vbeln.
        wa_saida-erdat = wa_likp-erdat.
        wa_saida-posnr = wa_lips-posnr.
        wa_saida-matnr = wa_lips-matnr.
        wa_saida-werks = wa_lips-werks.
        wa_saida-lfimg = wa_lips-lfimg.
        wa_saida-ntgew = wa_lips-ntgew.

        APPEND wa_saida TO it_saida. "Adiciona os valores da work area para a internal table.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_SORT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Ordenena os valores os valores.
*----------------------------------------------------------------------*
FORM zf_sort_alv .

  wa_sort-spos = 1.
  wa_sort-fieldname = 'route'.
  wa_sort-tabname = 'it_saida'.
  wa_sort-subtot = 'X'.
  wa_sort-up = 'X'.
  APPEND wa_sort TO it_sort.
  CLEAR wa_sort.

  wa_sort-spos = 2.
  wa_sort-fieldname = 'bezei'.
  wa_sort-tabname = 'it_saida'.
  APPEND wa_sort TO it_sort.
  CLEAR wa_sort.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Monta e exibe os valores do alv.
*----------------------------------------------------------------------*
FORM zf_display_alv .

  wa_fieldcat-fieldname = 'route'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Itinerário'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'bezei'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Denominação do itinerário'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'vbeln'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Fornecimento'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'erdat'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Data de criação do registro'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'posnr'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Item de remessa'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'matnr'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Nº do material'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'werks'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Centro'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'lfimg'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Quantidade fornecida de fato, em UMV'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_fieldcat-fieldname = 'NTGEW'. "Nome do campo a ter o nome mudado na exibição do ALV.
  wa_fieldcat-seltext_l = 'Peso líquido'. "Nome que vai aparecer na exibição do relatorio ALV.
*  wa_fieldcat-do_sum = 'X'.
  APPEND wa_fieldcat TO it_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR wa_fieldcat. "limpa o conteúdo da workarea.

  wa_layout-colwidth_optimize = abap_true. "Otima o tamanho da coluna conforme o tamanho da descrição.
  wa_layout-zebra = 'X'. "Faz o relátio ser exibido em zebra.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      i_grid_title       = 'Empresa' "Titúlo do ALV.
      it_sort            = it_sort "Tabela interna com a config do sort.
      is_layout          = wa_layout "Aqui passa o Layout do ALV
      it_fieldcat        = it_fieldcat "Aqui é passado a estrutura do ALV
    TABLES
      t_outtab           = it_saida. "Aqui passa os valores a serem exibidos no ALV

  IF sy-subrc NE 0.
    MESSAGE s208(001) DISPLAY LIKE sy-abcde+4(1) WITH text-e02.
  ENDIF.

ENDFORM.