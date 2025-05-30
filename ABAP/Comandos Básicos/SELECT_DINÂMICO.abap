*&---------------------------------------------------------------------*
*& Report ZR_SELECT_DINAMICO
*&---------------------------------------------------------------------*
REPORT zr_select_dinamico.

*---------------------------------------------------------------------*
* Tipos e Dados Globais
*---------------------------------------------------------------------*
TYPES: BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         mtart TYPE mara-mtart,
         matkl TYPE mara-matkl,
         meins TYPE mara-meins,
       END OF ty_mara.

DATA: v_where  TYPE string,
      v_campos TYPE string,
      dref     TYPE REF TO data.

FIELD-SYMBOLS: <fs_table>  TYPE STANDARD TABLE.

*---------------------------------------------------------------------*
* Tela de Seleção
*---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_carrid TYPE spfli-carrid,
              p_connid TYPE spfli-connid,
              p_matnr  TYPE mara-matnr,
              p_bukrs  TYPE bkpf-bukrs,
              p_belnr  TYPE bkpf-belnr,
              p_table  TYPE tabname.
SELECTION-SCREEN END OF BLOCK b1.

*---------------------------------------------------------------------*
* Processamento Principal
*---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM zf_validar_tabela.
  PERFORM zf_montar_where.
  PERFORM zf_criar_referencia_dados.
  PERFORM zf_executar_select.
  PERFORM zf_exibir_resultado.

*---------------------------------------------------------------------*
* FORM: Validação da tabela
*---------------------------------------------------------------------*
FORM zf_validar_tabela.

  DATA lv_exists TYPE dd02l-tabname.

  SELECT SINGLE tabname
    INTO @lv_exists
    FROM dd02l
    WHERE tabname = @p_table.

  IF sy-subrc <> 0.
    MESSAGE 'Tabela não existe no dicionário ABAP' TYPE 'E'.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
* FORM: Montagem do WHERE e Campos
*---------------------------------------------------------------------*
FORM zf_montar_where.

  CLEAR: v_where, v_campos.

  CASE p_table.

    WHEN 'SPFLI' OR 'SFLIGHT'.
      IF p_carrid IS NOT INITIAL.
        v_where = |CARRID EQ '{ p_carrid }'|.
      ENDIF.

      IF p_connid IS NOT INITIAL.
        v_where = COND string(
                    WHEN v_where IS INITIAL THEN |CONNID EQ '{ p_connid }'|
                    ELSE v_where && | AND CONNID EQ '{ p_connid }'| ).
      ENDIF.

      v_campos = '*'.

    WHEN 'MARA'.
      IF p_matnr IS NOT INITIAL.
        v_where = |MATNR EQ '{ p_matnr }'|.
      ENDIF.

      v_campos = 'MATNR MTART MATKL MEINS'.

    WHEN 'BKPF' OR 'BSEG'.
      IF p_bukrs IS NOT INITIAL.
        v_where = |BUKRS EQ '{ p_bukrs }'|.
      ENDIF.

      IF p_belnr IS NOT INITIAL.
        v_where = COND string(
                    WHEN v_where IS INITIAL THEN |BELNR EQ '{ p_belnr }'|
                    ELSE v_where && | AND BELNR EQ '{ p_belnr }'| ).
      ENDIF.

      v_campos = '*'.

    WHEN OTHERS.
      MESSAGE 'Tabela não suportada neste exemplo' TYPE 'E'.

  ENDCASE.

ENDFORM.

*---------------------------------------------------------------------*
* FORM: Criação da Referência de Dados
*---------------------------------------------------------------------*
FORM zf_criar_referencia_dados.

  IF p_table = 'MARA'.
    CREATE DATA dref TYPE TABLE OF ty_mara.
  ELSE.
    CREATE DATA dref TYPE TABLE OF (p_table).
  ENDIF.

  ASSIGN dref->* TO <fs_table>.

ENDFORM.

*---------------------------------------------------------------------*
* FORM: Execução do SELECT Dinâmico
*---------------------------------------------------------------------*
FORM zf_executar_select.

  SELECT (v_campos)
    FROM (p_table)
    INTO TABLE @<fs_table>
    WHERE (v_where).

ENDFORM.

*---------------------------------------------------------------------*
* FORM: Exibir Resultado
*---------------------------------------------------------------------*
FORM zf_exibir_resultado.

  IF sy-subrc = 0.
    cl_demo_output=>display( <fs_table> ).
  ELSE.
    WRITE: / 'Nenhum dado encontrado.'.
  ENDIF.

ENDFORM.