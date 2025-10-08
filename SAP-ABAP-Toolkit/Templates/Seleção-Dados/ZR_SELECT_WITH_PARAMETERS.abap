*&---------------------------------------------------------------------*
*& Report ZR_SELECT_WITH_PARAMETERS
*&---------------------------------------------------------------------*
*& Exemplo de utilização de SELECT com PARAMETERS na cláusula WHERE.
*& O programa realiza a leitura da tabela SFLIGHT conforme os valores
*& informados em parâmetros de seleção simples (sem intervalos).
*&---------------------------------------------------------------------*
REPORT zr_select_with_parameters.

*&---------------------------------------------------------------------*
*& TYPES
*&---------------------------------------------------------------------*
TYPES:
  BEGIN OF y_sflight,                  " Estrutura customizada para armazenar os registros selecionados
    carrid TYPE sflight-carrid,        " ID da companhia aérea
    connid TYPE sflight-connid,        " ID do voo
    fldate TYPE sflight-fldate,       " Data do voo
  END OF y_sflight.

*&---------------------------------------------------------------------*
*& INTERNAL TABLES
*&---------------------------------------------------------------------*
DATA:
  t_sflight TYPE STANDARD TABLE OF y_sflight. " Tabela interna para armazenar dados retornados do SELECT

*&---------------------------------------------------------------------*
*& SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01. " Início do bloco da tela de seleção

  PARAMETERS:
    p_carrid TYPE sflight-carrid, " Parâmetro para ID da companhia aérea
    p_connid TYPE sflight-connid. " Parâmetro para ID do voo

SELECTION-SCREEN END OF BLOCK b1. " Fim do bloco da tela de seleção

*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM zf_selecao_dados. " Realiza a busca dos dados conforme os parâmetros informados

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM zf_exibe_dados. " Exibe o resultado da seleção

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECAO_DADOS
*&---------------------------------------------------------------------*
*       Seleciona registros da tabela SFLIGHT de acordo com os valores
*       informados nos parâmetros P_CARRID e P_CONNID. Os dados são
*       armazenados na tabela interna T_SFLIGHT.
*----------------------------------------------------------------------*
FORM zf_selecao_dados .

  FREE t_sflight. " Limpa a tabela interna antes de realizar nova seleção

  SELECT carrid
         connid
         fldate
    FROM sflight
    INTO TABLE t_sflight
    WHERE carrid = p_carrid
      AND connid = p_connid.

  IF sy-subrc <> 0. " Verifica se a seleção retornou registros

    MESSAGE s208(00) DISPLAY LIKE 'E' WITH TEXT-e01. " Exibe mensagem de erro padrão
    LEAVE LIST-PROCESSING. " Interrompe a execução da lista (output)

  ENDIF.

ENDFORM. " zf_selecao_dados

*&---------------------------------------------------------------------*
*&      Form  ZF_EXIBE_DADOS
*&---------------------------------------------------------------------*
*       Exibe os dados da tabela interna T_SFLIGHT utilizando a classe
*       de demonstração CL_DEMO_OUTPUT.
*----------------------------------------------------------------------*
FORM zf_exibe_dados .

  cl_demo_output=>display( t_sflight ). " Exibe a tabela interna formatada

ENDFORM. " zf_exibe_dados