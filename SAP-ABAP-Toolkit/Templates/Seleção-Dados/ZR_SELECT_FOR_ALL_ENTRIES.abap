*&---------------------------------------------------------------------*
*& Report ZR_SELECT_FOR_ALL_ENTRIES
*&---------------------------------------------------------------------*
*& Exemplo de utilização de SELECT com FOR ALL ENTRIES.
*& O programa seleciona registros da tabela SFLIGHT, SPFLI e SCARR
*& conforme os critérios informados pelo usuário na tela de seleção,
*& realiza o processamento para combinar os dados e exibe o resultado.
*&---------------------------------------------------------------------*
REPORT zr_select_for_all_entries.

*&---------------------------------------------------------------------*
*& TABLES
*&---------------------------------------------------------------------*
TABLES:
  sflight. " Tabela SFLIGHT (voos)

*&---------------------------------------------------------------------*
*& TYPES
*&---------------------------------------------------------------------*
TYPES:
  BEGIN OF y_sflight, " Estrutura para dados de voo
    carrid TYPE sflight-carrid,
    connid TYPE sflight-connid,
    fldate TYPE sflight-fldate,
  END OF y_sflight,

  y_sflight_tab TYPE STANDARD TABLE OF y_sflight, " Tabela interna para SFLIGHT

  BEGIN OF y_scarr, " Estrutura para dados de companhia aérea
    carrid   TYPE scarr-carrid,
    carrname TYPE scarr-carrname,
    currcode TYPE scarr-currcode,
  END OF y_scarr,

  y_scarr_tab TYPE SORTED TABLE OF y_scarr
    WITH NON-UNIQUE KEY primary_key COMPONENTS carrid, " Tabela interna ordenada por carrid

  BEGIN OF y_spfli, " Estrutura para dados de rotas
    carrid    TYPE spfli-carrid,
    connid    TYPE spfli-connid,
    countryfr TYPE spfli-countryfr,
    cityfrom  TYPE spfli-cityfrom,
    countryto TYPE spfli-countryto,
    cityto    TYPE spfli-cityto,
  END OF y_spfli,

  y_spfli_tab TYPE SORTED TABLE OF y_spfli
    WITH NON-UNIQUE KEY primary_key COMPONENTS carrid connid, " Tabela ordenada por carrid e connid

  BEGIN OF y_saida, " Estrutura final de saída combinando SFLIGHT, SCARR e SPFLI
    carrid    TYPE sflight-carrid,
    connid    TYPE sflight-connid,
    carrname  TYPE scarr-carrname,
    currcode  TYPE scarr-currcode,
    countryfr TYPE spfli-countryfr,
    cityfrom  TYPE spfli-cityfrom,
    countryto TYPE spfli-countryto,
    cityto    TYPE spfli-cityto,
  END OF y_saida,

  y_saida_tab TYPE STANDARD TABLE OF y_saida. " Tabela interna de saída final

*&---------------------------------------------------------------------*
*& INTERNAL TABLES
*&---------------------------------------------------------------------*
DATA:
  t_sflight TYPE y_sflight_tab, " Tabela interna para SFLIGHT
  t_scarr   TYPE y_scarr_tab,   " Tabela interna para SCARR
  t_spfli   TYPE y_spfli_tab,   " Tabela interna para SPFLI
  t_saida   TYPE y_saida_tab.   " Tabela interna de saída

*&---------------------------------------------------------------------*
*& SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01. " Bloco de seleção

  SELECT-OPTIONS:
    s_carrid FOR sflight-carrid, " Intervalo de companhias aéreas
    s_connid FOR sflight-connid. " Intervalo de voos

SELECTION-SCREEN END OF BLOCK b1.

*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM zf_selecao_dados. " Seleciona os dados

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM zf_processamento_dados. " Processa os dados selecionados
  PERFORM zf_exibicao_dados.      " Exibe os dados processados

*&---------------------------------------------------------------------*
*& Form zf_selecao_dados
*&---------------------------------------------------------------------*
*& Seleciona dados das tabelas SFLIGHT, SPFLI e SCARR
*& utilizando SELECT e FOR ALL ENTRIES.
*&---------------------------------------------------------------------*
FORM zf_selecao_dados .

  FREE t_sflight. " Limpa tabela interna antes de nova seleção

  " Seleciona voos de SFLIGHT conforme companhia aérea informada
  SELECT carrid
         connid
         fldate
    FROM sflight
    INTO TABLE t_sflight
    WHERE carrid IN s_carrid.

  IF sy-subrc EQ 0. " Verifica se houve registros

    " Seleciona rotas de SPFLI relacionadas aos voos selecionados
    FREE t_spfli.
    SELECT carrid
           connid
           countryfr
           cityfrom
           countryto
           cityto
      FROM spfli
      INTO TABLE t_spfli
      FOR ALL ENTRIES IN t_sflight
      WHERE carrid = t_sflight-carrid
        AND connid = t_sflight-connid.

    " Seleciona informações de companhia aérea de SCARR relacionadas aos voos
    FREE t_scarr.
    SELECT carrid
           carrname
           currcode
      FROM scarr
      INTO TABLE t_scarr
      FOR ALL ENTRIES IN t_sflight
      WHERE carrid = t_sflight-carrid.

  ELSE.

    MESSAGE s208(00) DISPLAY LIKE 'E' WITH TEXT-e01. " Dados não encontrados
    LEAVE LIST-PROCESSING.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_processamento_dados
*&---------------------------------------------------------------------*
*& Combina os dados de SFLIGHT, SCARR e SPFLI em uma estrutura de saída.
*&---------------------------------------------------------------------*
FORM zf_processamento_dados .

  LOOP AT t_sflight INTO DATA(w_sflight). " Percorre cada registro de voo

    " Inicializa estrutura de saída com dados básicos do voo
    DATA(w_saida) = VALUE y_saida( carrid = w_sflight-carrid
                                   connid = w_sflight-connid ).

    " Adiciona informações de rota (SPFLI) se existirem
    READ TABLE t_spfli INTO DATA(w_spfli)
      WITH TABLE KEY primary_key COMPONENTS carrid = w_sflight-carrid
                                            connid = w_sflight-connid.
    IF sy-subrc EQ 0.
      w_saida = VALUE y_saida( BASE w_saida
                                    countryfr = w_spfli-countryfr
                                    cityfrom  = w_spfli-cityfrom
                                    countryto = w_spfli-countryto
                                    cityto    = w_spfli-cityto ).
    ENDIF.

    " Adiciona informações da companhia aérea (SCARR) se existirem
    READ TABLE t_scarr INTO DATA(w_scarr)
      WITH TABLE KEY primary_key COMPONENTS carrid = w_sflight-carrid.
    IF sy-subrc EQ 0.
      w_saida = VALUE y_saida( BASE w_saida
                                    carrname = w_scarr-carrname
                                    currcode = w_scarr-currcode ).
    ENDIF.

    " Armazena resultado final na tabela de saída
    APPEND w_saida TO t_saida.
    CLEAR: w_saida.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_exibicao_dados
*&---------------------------------------------------------------------*
*& Exibe os dados processados na tela.
*& Remove duplicados por carrid e connid antes de exibição.
*&---------------------------------------------------------------------*
FORM zf_exibicao_dados .

  DATA(t_saida_2) = t_saida. " Cria cópia para manipulação de duplicados

  DELETE ADJACENT DUPLICATES FROM t_saida_2 COMPARING carrid connid.

  LOOP AT t_saida_2 INTO DATA(w_saida).
    WRITE: / w_saida-carrid,
             w_saida-connid,
             w_saida-countryfr,
             w_saida-cityfrom,
             w_saida-countryto,
             w_saida-cityto,
             w_saida-carrname,
             w_saida-currcode.
  ENDLOOP.

ENDFORM.