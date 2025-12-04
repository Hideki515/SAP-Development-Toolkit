*&---------------------------------------------------------------------*
*& Report ZR_DYNAMIC_SELECT
*&---------------------------------------------------------------------*
*& Exemplo de utilização de SELECT DINAMICO na cláusula WHERE.
*&---------------------------------------------------------------------*
REPORT zr_dynamic_select.

*&---------------------------------------------------------------------*
*& TYPES
*&---------------------------------------------------------------------*
TYPES:
  BEGIN OF y_mara,
    matnr TYPE mara-matnr, "N° Material
    matkl TYPE mara-matkl, "Grupo de Mercadorias
    meins TYPE mara-meins, "Unidade de Medida Básica
  END OF y_mara,

  y_mara_tab TYPE STANDARD TABLE OF y_mara.

*&---------------------------------------------------------------------*
*& VARIABLES
*&---------------------------------------------------------------------*
DATA:
  v_where  TYPE string,
  v_campos TYPE string,
  v_dref   TYPE REF TO data.

*&---------------------------------------------------------------------*
*& FIELD-SYMBOLS
*&---------------------------------------------------------------------*
FIELD-SYMBOLS:
  <f_table> TYPE STANDARD TABLE.

*&---------------------------------------------------------------------*
*& CONSTANTS
*&---------------------------------------------------------------------*
CONSTANTS:
  BEGIN OF c_tipos,
    e TYPE char1 VALUE 'E',
  END OF c_tipos.

*&---------------------------------------------------------------------*
*& SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01. "Tela de Seleção

  PARAMETERS:
    p_carrid TYPE spfli-carrid, "Denominação breve da companhia aérea
    p_connid TYPE spfli-connid, "Código da conexão de voo individual
    p_matnr  TYPE mara-matnr,   "N° Material
    p_bukrs  TYPE bkpf-bukrs,   "Empresa
    p_belnr  TYPE bkpf-belnr,   "N° documento de um documento contábil
    p_table  TYPE tabname OBLIGATORY. "Nome da tabela

SELECTION-SCREEN END OF BLOCK b1.

*&---------------------------------------------------------------------*
*& SELECTION-SCREEN
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM zf_validar_tabela.
  PERFORM zf_monta_where.
  PERFORM zf_cria_referencia_dados.
  PERFORM zf_seleciona_dados.

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM zf_exibir_dados.

*&---------------------------------------------------------------------*
*& Form zf_validar_tabela
*&---------------------------------------------------------------------*
*& Verifica se a tabela existe
*&---------------------------------------------------------------------*
FORM zf_validar_tabela .

  DATA:
    l_exists TYPE dd02l-tabname.

  SELECT SINGLE tabname
    FROM dd02l
    INTO @l_exists
    WHERE tabname = @p_table
      AND as4local = 'A'. "Garante que a tabela está ativa

  IF sy-subrc NE 0.

    MESSAGE s208(00) DISPLAY LIKE c_tipos-e WITH p_table TEXT-e01. "Tabela não existe no dicionário ABAP
    LEAVE LIST-PROCESSING.

  ENDIF.

ENDFORM. "zf_validar_tabela

*&---------------------------------------------------------------------*
*& Form zf_monta_where
*&---------------------------------------------------------------------*
*& Monta a claúsula do WHERE e campos a serem selecionados
*&---------------------------------------------------------------------*
FORM zf_monta_where .

  CLEAR:
    v_where,
    v_campos.

  CASE p_table.
    WHEN 'SPFLI'
      OR 'SFLIGHT'.
      IF p_carrid IS NOT INITIAL.

        v_where = |CARRID EQ '{ p_carrid }'|.

      ENDIF.

      IF p_connid IS NOT INITIAL.

        v_where = COND string( WHEN v_where IS INITIAL THEN |CONNID EQ '{ p_connid }'|
                               ELSE v_where && | AND CONNID EQ '{ p_connid }'| ).

      ENDIF.

      v_campos = '*'.

    WHEN 'MARA'.
      IF p_matnr IS NOT INITIAL.

        v_where = |MATNR EQ '{ p_matnr }'|.

      ENDIF.

      v_campos = 'MATNR, MATKL, MEINS'.

    WHEN 'BKPF'
      OR 'BSEG'.
      IF p_bukrs IS NOT INITIAL.

        v_where = |BUKRS EQ '{ p_bukrs }'|.

      ENDIF.

      IF p_belnr IS NOT INITIAL.

        v_where = COND string( WHEN v_where IS INITIAL THEN |BELNR EQ '{ p_belnr }'|
                             ELSE v_where && | AND BELNR EQ '{ p_belnr }'| ).

      ENDIF.

      v_campos = '*'.

    WHEN OTHERS.
      MESSAGE s208(00) DISPLAY LIKE c_tipos-e WITH p_table TEXT-e02. "Tabela não suportada neste exemplo
      LEAVE LIST-PROCESSING.

  ENDCASE.

  IF v_where IS INITIAL.

    MESSAGE s208(00) DISPLAY LIKE c_tipos-e WITH TEXT-e05. "Where não pode ficar vazio
    LEAVE LIST-PROCESSING.

  ENDIF.

ENDFORM. "zf_monta_where

*&---------------------------------------------------------------------*
*& Form zf_cria_referencia_dados
*&---------------------------------------------------------------------*
*& Cria referencia das tabelas e assina o field-symbol
*&---------------------------------------------------------------------*
FORM zf_cria_referencia_dados .

  IF p_table = 'MARA'.

    CREATE DATA v_dref TYPE STANDARD TABLE OF y_mara.

  ELSE.

    CREATE DATA v_dref TYPE STANDARD TABLE OF (p_table).

  ENDIF.

  ASSIGN v_dref->* TO <f_table>.

  IF sy-subrc NE 0.

    MESSAGE s208(00) DISPLAY LIKE c_tipos-e WITH 'Field-Symbol' TEXT-e03. " Erro na atribuição
    LEAVE LIST-PROCESSING.

  ENDIF.

ENDFORM. "zf_cria_referencia_dados

*&---------------------------------------------------------------------*
*& Form zf_seleciona_dados
*&---------------------------------------------------------------------*
*& Seleção dos dados com base nos campos e clásula WHERE das variáveis.
*&---------------------------------------------------------------------*
FORM zf_seleciona_dados .

  IF <f_table> IS ASSIGNED.

    FREE <f_table>.

  ENDIF.

  SELECT (v_campos)
    FROM (p_table)
    INTO TABLE @<f_table>
    WHERE (v_where).

  IF sy-subrc NE 0.

    MESSAGE s208(00) DISPLAY LIKE c_tipos-e WITH sy-subrc TEXT-e03. "Erro na seleção dos dados!
    LEAVE LIST-PROCESSING.

  ENDIF.

ENDFORM. "zf_seleciona_dados

*&---------------------------------------------------------------------*
*& Form zf_exibir_dados
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_exibir_dados .

  IF <f_table> IS ASSIGNED AND <f_table> IS NOT INITIAL.

    cl_demo_output=>display( <f_table> ).

  ELSE.

    MESSAGE s208(00) DISPLAY LIKE c_tipos-e WITH p_table TEXT-e04. "Nenhum dado encontrado!.
    LEAVE LIST-PROCESSING.

  ENDIF.

ENDFORM. "zf_exibir_dados