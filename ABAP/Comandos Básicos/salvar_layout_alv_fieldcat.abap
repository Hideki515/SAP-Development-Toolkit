*&---------------------------------------------------------------------*
*& Report ZR_BHS_SALVAR_LAYOUT_ALV
*&---------------------------------------------------------------------*
*& Estrutura ALV Customizado com Perform e Form e com personalização do
*& Layout
*&---------------------------------------------------------------------*
REPORT zr_bhs_salvar_layout_alv. "Define o nome do programa.

* Declaração da tabela SFLIGHT para uso no programa.
TABLES: sflight.

*&---------------------------------------------------------------------*
*& TYPES
*&---------------------------------------------------------------------*
* Definição de um tipo estruturado com campos relevantes da tabela SFLIGHT.
TYPES: BEGIN OF ty_sflight,
         carrid   TYPE sflight-carrid,   "Código da companhia aérea.
         connid   TYPE sflight-connid,   "Número de conexão do voo.
         fldate   TYPE sflight-fldate,   "Data do voo.
         seatsocc TYPE sflight-seatsocc, "Número de assentos ocupados.
       END OF ty_sflight.

*&---------------------------------------------------------------------*
*& INTERNAL TABLES
*&---------------------------------------------------------------------*
* Declara uma tabela interna baseada no tipo ty_sflight.
DATA: ti_sflight TYPE TABLE OF ty_sflight. "Armazena os dados da tabela SFLIGHT.

* Declara uma tabela interna para configuração do Field Catalog.
DATA: ti_fieldcat_alv TYPE TABLE OF slis_fieldcat_alv. "Field catalog do ALV.

*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*
* Declara áreas de trabalho (work areas) para layout e Field Catalog.
DATA: wa_layout_alv   TYPE slis_layout_alv, "Configuração de layout do ALV.
      wa_fieldcat_alv TYPE slis_fieldcat_alv. "Configuração de um campo do Field Catalog.

*&---------------------------------------------------------------------*
*& VARIABLES
*&---------------------------------------------------------------------*
DATA: v_variant TYPE disvariant.

*&---------------------------------------------------------------------*
*& SELECTION-SCREEN
*&---------------------------------------------------------------------*
* Define os parâmetros de seleção na tela inicial do programa.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01. "Título do bloco.

SELECT-OPTIONS: s_carrid FOR sflight-carrid, "Intervalo para código da companhia aérea.
                s_connid FOR sflight-connid. "Intervalo para número de conexão.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t02.

PARAMETERS: p_layout TYPE disvariant-variant.

SELECTION-SCREEN END OF BLOCK b2.

*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON p_layout.
  PERFORM zf_carrega_layout CHANGING v_variant.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  PERFORM zf_matchcode_layout CHANGING v_variant.

*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
* Evento que ocorre após a entrada do usuário na tela de seleção.
START-OF-SELECTION.
  PERFORM zf_selecao_dados. "Seleciona os dados com base nos parâmetros.

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*&---------------------------------------------------------------------*
* Evento final para exibição do relatório.
*&---------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM zf_monsta_fieldcat. "Prepara o Field Catalog.
  PERFORM zf_layout_alv.      "Configura o layout do ALV.
  PERFORM zf_mostra_alv.      "Exibe o ALV com os dados.


*&---------------------------------------------------------------------*
*&  FORMS ALV_VARIANT_EXISTENCE
*&---------------------------------------------------------------------*
*& Utilizado para salvar o layout do ALV
*&---------------------------------------------------------------------*
FORM zf_carrega_layout CHANGING p_variant TYPE disvariant.
  CHECK p_layout IS NOT INITIAL.

  p_variant-report  = sy-repid.
  p_variant-variant = p_layout.

  IF p_layout CP '/*'.
    p_variant-username = sy-uname.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
    EXPORTING
      i_save        = sy-abcde(1)
    CHANGING
      cs_variant    = p_variant
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      OTHERS        = 4.

  IF sy-subrc <> 0.

    MESSAGE text-e09 TYPE 'S' DISPLAY LIKE 'E'. "Erro ao buscar variantes existentes
    LEAVE TO LIST-PROCESSING.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&  FORMS ZF_MATCHCODE_LAYOUT
*&---------------------------------------------------------------------*
*& Matchcode para variante existentes.
*&---------------------------------------------------------------------*
FORM zf_matchcode_layout  CHANGING p_variant TYPE disvariant.
  DATA: l_exit(1) TYPE c.

  p_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = p_variant
      i_save     = sy-abcde(1)
    IMPORTING
      e_exit     = l_exit
      es_variant = p_variant
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 2.

    MESSAGE text-e10 TYPE 'S' DISPLAY LIKE 'E'. "Erro ao selecionar variante

  ELSEIF l_exit = space.

    p_layout = p_variant-variant.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form ZF_SELECAO_DADOS
*&---------------------------------------------------------------------*
* Realiza a seleção de dados da tabela SFLIGHT.
*&---------------------------------------------------------------------*
FORM zf_selecao_dados.

  SELECT carrid
         connid
         fldate
         seatsocc
  FROM sflight
  INTO TABLE ti_sflight
  WHERE
  carrid IN s_carrid AND
  connid IN s_connid. "Aplica os filtros fornecidos pelo usuário.

ENDFORM. "Fim do form ZF_SELECAO_DADOS.

*&---------------------------------------------------------------------*
*& Form ZF_MONSTA_FIELDCAT
*&---------------------------------------------------------------------*
* Monta o Field Catalog para o ALV.
*&---------------------------------------------------------------------*
FORM zf_monsta_fieldcat.

  PERFORM zf_preenche_fieldcat USING:
        'TI_SFLIGHT' 'CARRID'   'Código da companhia aérea',
        'TI_SFLIGHT' 'CONNID'   'Número de conexão do voo',
        'TI_SFLIGHT' 'FLDATE'   'Data do voo',
        'TI_SFLIGHT' 'SEATSOCC' 'Ocupação econômica.'. "Adiciona campos ao Field Catalog.

ENDFORM. "Fim do form ZF_MONSTA_FIELDCAT.

*&---------------------------------------------------------------------*
*& Form ZF_PREENCHE_FIELDCAT
*&---------------------------------------------------------------------*
* Adiciona campos ao Field Catalog.
*&---------------------------------------------------------------------*
FORM zf_preenche_fieldcat USING p_tabname "Nome da tabela.
      p_fieldname "Nome do campo.
      p_seltext_l. "Texto de descrição do campo.

  CLEAR wa_fieldcat_alv. "Limpa a work area para evitar dados residuais.

  wa_fieldcat_alv-tabname       = p_tabname.   "Tabela do campo.
  wa_fieldcat_alv-fieldname     = p_fieldname. "Campo a ser exibido.
  wa_fieldcat_alv-seltext_l     = p_seltext_l. "Texto explicativo para o campo.

  APPEND wa_fieldcat_alv TO ti_fieldcat_alv. "Adiciona ao Field Catalog.

ENDFORM. "Fim do form ZF_PREENCHE_FIELDCAT.

*&---------------------------------------------------------------------*
*& Form ZF_LAYOUT_ALV
*&---------------------------------------------------------------------*
* Configura o layout do ALV.
*&---------------------------------------------------------------------*
FORM zf_layout_alv.

  CLEAR wa_layout_alv. "Limpa a área de trabalho do layout.

  wa_layout_alv-zebra = 'X'. "Ativa o estilo zebrado no ALV.
  wa_layout_alv-colwidth_optimize = 'X'. "Ajusta automaticamente a largura das colunas.

ENDFORM. "Fim do form ZF_LAYOUT_ALV.

*&---------------------------------------------------------------------*
*& Form ZF_MOSTRA_ALV
*&---------------------------------------------------------------------*
* Exibe o relatório no formato ALV.
*&---------------------------------------------------------------------*
FORM zf_mostra_alv.

  DATA: w_variant TYPE disvariant.

  CLEAR w_variant.
  w_variant-report = sy-repid.
  w_variant-variant = p_layout.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid     "Nome do programa.
      i_grid_title       = 'Assentos Ocupados' "Título do ALV.
      is_layout          = wa_layout_alv "Configuração do layout.
      it_fieldcat        = ti_fieldcat_alv "Field Catalog configurado.
      i_save             = 'X'
      is_variant         = w_variant
    TABLES
      t_outtab           = ti_sflight "Tabela com os dados a serem exibidos.
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2. "Tratamento de erros.

  IF sy-subrc <> 0. "Verifica erros na chamada da função.

    MESSAGE: text-e01 TYPE 'S' DISPLAY LIKE 'E'. "Exibe mensagem de erro.
    LEAVE TO LIST-PROCESSING. "Retorna para a tela de seleção.

  ENDIF.

ENDFORM. "Fim do form ZF_MOSTRA_ALV.