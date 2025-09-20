*&---------------------------------------------------------------------*
*& Report ZPR_BHS_MATCHCODE2.
*&---------------------------------------------------------------------*
*& Template para código para Match Code com retorno preenchendo mais
*& de um parâmetro.
*&---------------------------------------------------------------------*
REPORT zpr_bhs_matchcode2.

*&---------------------------------------------------------------------*
*&  TYPES
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_scarr,
         carrid   TYPE scarr-carrid,
         carrname TYPE scarr-carrname,
         currcode TYPE scarr-currcode,
       END OF ty_scarr, " Estrutura personalizada

       ty_scarr_tab   TYPE TABLE OF ty_scarr, " Tipo de tabela para tabela interna
       ty_mapping_tab TYPE TABLE OF dselc.    " Tipo de tabela para tabela interna

*&---------------------------------------------------------------------*
*&  INTERNAL TABLES
*&---------------------------------------------------------------------*
DATA:
  it_scarr       TYPE ty_scarr_tab,   " Tabela interna para os dados da seleção
  it_mapping_tab TYPE ty_mapping_tab. " Tabela interna para o mapeamento

*&---------------------------------------------------------------------*
*&  WORK AREAS
*&---------------------------------------------------------------------*
DATA:
  w_mapping TYPE dselc. " Work Area que é usada para preencher os dados na tabela interna.

*&---------------------------------------------------------------------*
*&  SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.

PARAMETERS: p_carrid TYPE scarr-carrid,   " Código da companhia aérea.
            p_name   TYPE scarr-carrname, " Nome da companhia aérea.
            p_code   TYPE scarr-currcode. " Código da moeda.

SELECTION-SCREEN END OF BLOCK b1.

*&---------------------------------------------------------------------*
*&  AT SELECTION-SCREEN
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_carrid.
  PERFORM zf_selecao_dados.
  PERFORM zf_monta_mapping.
  PERFORM zf_matchcode_carrid.

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECAO_DADOS
*&---------------------------------------------------------------------*~
*   Form para seleção de dados da SCARR.
*&---------------------------------------------------------------------*
FORM zf_selecao_dados.

  SELECT carrid
         carrname
         currcode
    FROM scarr
    INTO TABLE it_scarr.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_MAPPING
*&---------------------------------------------------------------------*
*   Monta a tabela interna para o mapeamento.
*&---------------------------------------------------------------------*
FORM zf_monta_mapping.

  FREE it_mapping_tab.

  PERFORM zf_preenche_table_mapping USING:
        'F0001' 'P_CARRID' ,
        'F0002' 'P_NAME'   ,
        'F0003' 'P_CODE'   .

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_PREENCHE_TABLE_MAPPING
*&---------------------------------------------------------------------*
*   Preenche a tabela de mapeamento.
*&---------------------------------------------------------------------*
FORM zf_preenche_table_mapping USING p_fldname TYPE any
                                     p_dyfldname TYPE any.

  CLEAR w_mapping.

  w_mapping-fldname   = p_fldname.
  w_mapping-dyfldname = p_dyfldname.

  APPEND w_mapping TO it_mapping_tab.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_MATCHCODE_CARRID
*&---------------------------------------------------------------------*
*   Chama a função que monta e exibe o match code.
*&---------------------------------------------------------------------*
FORM zf_matchcode_carrid.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'CARRID'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'P_CARRID'
      value_org       = 'S'
    TABLES
      value_tab       = it_scarr
      dynpfld_mapping = it_mapping_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.

    MESSAGE 'Erro na seleção do valor Match Code' TYPE 'S' DISPLAY LIKE 'E'.

  ENDIF.


ENDFORM.