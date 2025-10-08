*&---------------------------------------------------------------------*
*& Report ZR_SELECT_WITH_SELECT_OPTIONS
*&---------------------------------------------------------------------*
*& Exemplo de utilização de SELECT com SELECT-OPTIONS na cláusula WHERE.
*& O programa seleciona registros da tabela SFLIGHT conforme critérios
*& informados pelo usuário na tela de seleção e exibe o resultado.
*&---------------------------------------------------------------------*
REPORT zr_select_with_select_options.

*&---------------------------------------------------------------------*
*& TABLES
*&---------------------------------------------------------------------*
TABLES:
  sflight.              " Declaração da tabela SFLIGHT para acesso aos campos no SELECT-OPTIONS

*&---------------------------------------------------------------------*
*& TYPES
*&---------------------------------------------------------------------*
TYPES:
  BEGIN OF y_sflight,    " Definição de estrutura customizada para armazenar dados selecionados
    carrid TYPE sflight-carrid,   " ID da companhia aérea
    connid TYPE sflight-connid,   " ID do voo
    fldate TYPE sflight-fldate,   " Data do voo
  END OF y_sflight.

*&---------------------------------------------------------------------*
*& INTERNAL TABLES
*&---------------------------------------------------------------------*
DATA:
  t_sflight TYPE STANDARD TABLE OF y_sflight. " Tabela interna para armazenar os registros retornados do SELECT

*&---------------------------------------------------------------------*
*& SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01. " Início do bloco de tela de seleção

  SELECT-OPTIONS:
    s_carrid FOR sflight-carrid,  " Intervalo de companhias aéreas
    s_connid FOR sflight-connid,  " Intervalo de conexões
    s_fldate FOR sflight-fldate.  " Intervalo de datas de voo

SELECTION-SCREEN END OF BLOCK b1. " Fim do bloco da tela de seleção

*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM zf_selecao_dados. " Seleciona os dados conforme critérios informados

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM zf_exibe_dados.  " Exibe os dados selecionados

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECAO_DADOS
*&---------------------------------------------------------------------*
*       Seleciona os dados da tabela SFLIGHT conforme parâmetros da tela
*       de seleção e armazena o resultado na tabela interna T_SFLIGHT.
*----------------------------------------------------------------------*
FORM zf_selecao_dados.

  FREE: t_sflight. " Limpa a tabela interna antes de realizar nova seleção

  SELECT carrid
         connid
         fldate
    FROM sflight
    INTO TABLE t_sflight
    WHERE carrid IN s_carrid
      AND connid IN s_connid
      AND fldate IN s_fldate.

  IF sy-subrc <> 0. " Verifica se o SELECT retornou registros

    MESSAGE s208(00) DISPLAY LIKE 'E' WITH TEXT-e01. " Exibe mensagem de erro padrão
    LEAVE LIST-PROCESSING. " Interrompe o processamento da lista

  ENDIF.

ENDFORM. " zf_selecao_dados

*&---------------------------------------------------------------------*
*&      Form  ZF_EXIBE_DADOS
*&---------------------------------------------------------------------*
*       Exibe os dados da tabela interna T_SFLIGHT utilizando a classe
*       de demonstração CL_DEMO_OUTPUT.
*----------------------------------------------------------------------*
FORM zf_exibe_dados.

  cl_demo_output=>display( t_sflight ). " Exibe os registros selecionados

ENDFORM. " zf_exibe_dados