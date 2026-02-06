*&---------------------------------------------------------------------*
*& Report ZR_FIELDCAT
*&---------------------------------------------------------------------*
*& Exemplo de como fazer uma ALV Fieldcat Simples
*&---------------------------------------------------------------------*
REPORT zr_fieldcat.

* Declaração da tabela SFLIGHT para uso no programa.
TABLES: sflight.

*&---------------------------------------------------------------------*
*& Types
*&---------------------------------------------------------------------*
TYPES:
  BEGIN OF y_sflight,
    carrid   TYPE sflight-carrid,   "Código da companhia aérea.
    connid   TYPE sflight-connid,   "Número de conexão do voo.
    fldate   TYPE sflight-fldate,   "Data do voo.
    seatsocc TYPE sflight-seatsocc, "Número de assentos ocupados.
  END OF y_sflight,

  y_sflight_tab TYPE STANDARD TABLE OF y_sflight.

*&---------------------------------------------------------------------*
*& Internal Tables
*&---------------------------------------------------------------------*
DATA:
  t_sflight      TYPE y_sflight_tab, "Tabela com os dados filtrados.
  t_fieldcat_alv TYPE slis_t_fieldcat_alv. "Configuração de campos para exibição no ALV.

*&---------------------------------------------------------------------*
*& Work Area
*&---------------------------------------------------------------------*
* Define áreas de trabalho para layout e configuração do Field Catalog.
DATA:
  w_layout_alv   TYPE slis_layout_alv. "Área de trabalho para layout do ALV.

CONSTANTS:
  BEGIN OF c_fieldcat,
    tabname TYPE slis_fieldcat_alv-tabname VALUE 'T_SFLIGHT',
    BEGIN OF fieldnames,
      carrid   TYPE slis_fieldcat_alv-fieldname VALUE 'CARRID',
      connid   TYPE slis_fieldcat_alv-fieldname VALUE 'CONNID',
      fldate   TYPE slis_fieldcat_alv-fieldname VALUE 'FLDATE',
      seatsocc TYPE slis_fieldcat_alv-fieldname VALUE 'SEATSOCC',
    END OF fieldnames,
  END OF c_fieldcat.

*&---------------------------------------------------------------------*
*& Selection-Screen
*&---------------------------------------------------------------------*
* Define a tela de seleção com filtros para os dados.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01. "Título do bloco de seleção.

* Intervalos de seleção para o código da companhia aérea e número de conexão.
  SELECT-OPTIONS:
    s_carrid FOR sflight-carrid, "Código da companhia aérea.
    s_connid FOR sflight-connid. "Número de conexão do voo.

SELECTION-SCREEN END OF BLOCK b1. "Final do bloco de seleção.

*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
* Evento executado após a entrada dos parâmetros de seleção.
START-OF-SELECTION.
  PERFORM zf_selecao_dados. "Chama o FORM para selecionar dados.

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*&---------------------------------------------------------------------*
* Evento final, onde o relatório ALV é montado e exibido.
END-OF-SELECTION.
  PERFORM zf_layout_alv.      "Chama o FORM para configurar o layout do ALV.
  PERFORM zf_mostra_alv.      "Chama o FORM para exibir o ALV.

*&---------------------------------------------------------------------*
*& Form ZF_SELECAO_DADOS
*&---------------------------------------------------------------------*
* Realiza a seleção de dados da tabela SFLIGHT com base nos filtros.
FORM zf_selecao_dados.

  SELECT carrid
         connid
         fldate
         seatsocc
  FROM sflight
  INTO TABLE t_sflight
  WHERE carrid IN s_carrid
    AND connid IN s_connid. "Filtra os dados com base nos intervalos fornecidos.

ENDFORM. "Fim do FORM zf_selecao_dados.

*&---------------------------------------------------------------------*
*& Form ZF_LAYOUT_ALV
*&---------------------------------------------------------------------*
* Configura o layout do ALV, como estilo zebrado e largura das colunas.
FORM zf_layout_alv.

  CLEAR w_layout_alv. "Limpa a área de trabalho para evitar resíduos.
  w_layout_alv-zebra = abap_true. "Ativa o estilo zebrado para melhor visualização.
  w_layout_alv-colwidth_optimize = abap_true. "Ajusta automaticamente a largura das colunas.

ENDFORM. "Fim do FORM zf_layout_alv.

*&---------------------------------------------------------------------*
*& Form ZF_MOSTRA_ALV
*&---------------------------------------------------------------------*
* Monta e exibe o relatório ALV com os dados selecionados.
FORM zf_mostra_alv.

* preenche o field catalog com os campos da tabela interna.
  APPEND VALUE slis_fieldcat_alv( fieldname    = c_fieldcat-fieldnames-carrid
                                  tabname      = c_fieldcat-tabname
                                  reptext_ddic = TEXT-001 ) "Denominação Companhia Aérea
                                  TO t_fieldcat_alv.

  APPEND VALUE slis_fieldcat_alv( fieldname    = c_fieldcat-fieldnames-connid
                                  tabname      = c_fieldcat-tabname
                                  reptext_ddic = TEXT-002 ) "Número de Conexão Voo
                                  TO t_fieldcat_alv.

  APPEND VALUE slis_fieldcat_alv( fieldname    = c_fieldcat-fieldnames-fldate
                                  tabname      = c_fieldcat-tabname
                                  reptext_ddic = TEXT-003 ) "Data do Voo
                                  TO t_fieldcat_alv.

  APPEND VALUE slis_fieldcat_alv( fieldname    = c_fieldcat-fieldnames-seatsocc
                                  tabname      = c_fieldcat-tabname
                                  reptext_ddic = TEXT-004 ) "Ocupação Econômica
                                  TO t_fieldcat_alv.

* Exibe o relatório alv usando a função reuse_alv_grid_display.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid            "Nome do programa atual.
      i_grid_title       = 'Assentos Ocupados' "Título do ALV.
      is_layout          = w_layout_alv       "Configuração de layout.
      it_fieldcat        = t_fieldcat_alv     "Field Catalog configurado.
    TABLES
      t_outtab           = t_sflight "Tabela com os dados a serem exibidos.
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2. "Tratamento de outros erros.

  IF sy-subrc <> 0. "Verifica erros ao chamar a função.
    MESSAGE: TEXT-e01 TYPE 'S' DISPLAY LIKE 'E'. "Exibe mensagem de erro.
    LEAVE TO LIST-PROCESSING. "Retorna para a tela de seleção.
  ENDIF.

ENDFORM. "Fim do FORM zf_mostra_alv.