*&---------------------------------------------------------------------*
*& Report ZR_CUSTOM_ALV
*&---------------------------------------------------------------------*
*& Estrutura ALV Customizado com Perform e Form
*&---------------------------------------------------------------------*
REPORT zr_custom_alv. "Define o nome do programa.

* Declaração da tabela SFLIGHT para uso no programa.
TABLES: sflight.

*&---------------------------------------------------------------------*
*& TYPES
*&---------------------------------------------------------------------*
* Define um tipo estruturado com os campos necessários da tabela SFLIGHT.
TYPES: BEGIN OF ty_sflight,
         carrid   TYPE sflight-carrid,   "Código da companhia aérea.
         connid   TYPE sflight-connid,   "Número de conexão do voo.
         fldate   TYPE sflight-fldate,   "Data do voo.
         seatsocc TYPE sflight-seatsocc, "Número de assentos ocupados.
       END OF ty_sflight.

*&---------------------------------------------------------------------*
*& INTERNAL TABLES
*&---------------------------------------------------------------------*
* Declara uma tabela interna para armazenar os dados da tabela SFLIGHT.
DATA: ti_sflight TYPE TABLE OF ty_sflight. "Tabela com os dados filtrados.

* Declara uma tabela interna para configurar o Field Catalog (catálogo de campos) do ALV.
DATA: ti_fieldcat_alv TYPE TABLE OF slis_fieldcat_alv. "Configuração de campos para exibição no ALV.

*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*
* Define áreas de trabalho para layout e configuração do Field Catalog.
DATA: wa_layout_alv   TYPE slis_layout_alv, "Área de trabalho para layout do ALV.
      wa_fieldcat_alv TYPE slis_fieldcat_alv. "Área de trabalho para definir campos do Field Catalog.

*&---------------------------------------------------------------------*
*& SELECTION-SCREEN
*&---------------------------------------------------------------------*
* Define a tela de seleção com filtros para os dados.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01. "Título do bloco de seleção.

* Intervalos de seleção para o código da companhia aérea e número de conexão.
SELECT-OPTIONS: s_carrid FOR sflight-carrid, "Código da companhia aérea.
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
  INTO TABLE ti_sflight
  WHERE carrid IN s_carrid AND connid IN s_connid. "Filtra os dados com base nos intervalos fornecidos.

ENDFORM. "Fim do FORM zf_selecao_dados.

*&---------------------------------------------------------------------*
*& Form ZF_LAYOUT_ALV
*&---------------------------------------------------------------------*
* Configura o layout do ALV, como estilo zebrado e largura das colunas.
FORM zf_layout_alv.

  CLEAR wa_layout_alv. "Limpa a área de trabalho para evitar resíduos.
  wa_layout_alv-zebra = 'X'. "Ativa o estilo zebrado para melhor visualização.
  wa_layout_alv-colwidth_optimize = 'X'. "Ajusta automaticamente a largura das colunas.

ENDFORM. "Fim do FORM zf_layout_alv.

*&---------------------------------------------------------------------*
*& Form ZF_MOSTRA_ALV
*&---------------------------------------------------------------------*
* Monta e exibe o relatório ALV com os dados selecionados.
FORM zf_mostra_alv.

* preenche o field catalog com os campos da tabela interna.
  CLEAR wa_fieldcat_alv.
  wa_fieldcat_alv-fieldname = 'CARRID'. "Campo da companhia aérea.
  wa_fieldcat_alv-tabname   = 'TI_SFLIGHT'. "Nome da tabela interna.
  wa_fieldcat_alv-seltext_l = 'Denominação Companhia Aérea'. "Descrição do campo.
  APPEND wa_fieldcat_alv TO ti_fieldcat_alv. "Adiciona ao Field Catalog.

  CLEAR wa_fieldcat_alv.
  wa_fieldcat_alv-fieldname = 'CONNID'. "Campo do número de conexão.
  wa_fieldcat_alv-tabname   = 'TI_SFLIGHT'.
  wa_fieldcat_alv-seltext_l = 'Número de Conexão Voo'.
  APPEND wa_fieldcat_alv TO ti_fieldcat_alv.

  CLEAR wa_fieldcat_alv.
  wa_fieldcat_alv-fieldname = 'FLDATE'. "Campo da data do voo.
  wa_fieldcat_alv-tabname   = 'TI_SFLIGHT'.
  wa_fieldcat_alv-seltext_l = 'Data do Voo'.
  APPEND wa_fieldcat_alv TO ti_fieldcat_alv.

  CLEAR wa_fieldcat_alv.
  wa_fieldcat_alv-fieldname = 'SEATSOCC'. "Campo de ocupação econômica.
  wa_fieldcat_alv-tabname   = 'TI_SFLIGHT'.
  wa_fieldcat_alv-seltext_l = 'Ocupação Econômica'.
  APPEND wa_fieldcat_alv TO ti_fieldcat_alv.

* exibe o relatório alv usando a função reuse_alv_grid_display.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid     "Nome do programa atual.
      i_grid_title       = 'Assentos Ocupados' "Título do ALV.
      is_layout          = wa_layout_alv "Configuração de layout.
      it_fieldcat        = ti_fieldcat_alv "Field Catalog configurado.
    TABLES
      t_outtab           = ti_sflight "Tabela com os dados a serem exibidos.
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2. "Tratamento de outros erros.

  IF sy-subrc <> 0. "Verifica erros ao chamar a função.
    MESSAGE: text-e01 TYPE 'S' DISPLAY LIKE 'E'. "Exibe mensagem de erro.
    LEAVE TO LIST-PROCESSING. "Retorna para a tela de seleção.
  ENDIF.

ENDFORM. "Fim do FORM zf_mostra_alv.