*&---------------------------------------------------------------------*
*& Include          SAPMZTREINO_CALC_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form zf_verifica_click
*&---------------------------------------------------------------------*
*& Verifica qual botão teve a ação de clique.
*&---------------------------------------------------------------------*
FORM zf_verifica_click  USING p_sy_ucomm TYPE sy-ucomm.

  CASE p_sy_ucomm.

    WHEN c_back
      OR c_up
      OR c_canc.

      LEAVE PROGRAM.

    WHEN c_but_01.

      PERFORM zf_preenche_display USING c_1.

    WHEN c_but_02.

      PERFORM zf_preenche_display USING c_2.

    WHEN c_but_03.

      PERFORM zf_preenche_display USING c_3.

    WHEN c_but_04.

      PERFORM zf_preenche_display USING c_4.

    WHEN c_but_05.

      PERFORM zf_preenche_display USING c_5.

    WHEN c_but_06.

      PERFORM zf_preenche_display USING c_6.

    WHEN c_but_07.

      PERFORM zf_preenche_display USING c_7.

    WHEN c_but_08.

      PERFORM zf_preenche_display USING c_8.

    WHEN c_but_09.

      PERFORM zf_preenche_display USING c_9.

    WHEN c_but_0.

      PERFORM zf_preenche_display USING c_0.

    WHEN c_but_c.

      CLEAR:
        v_value1,
        v_value2,
        v_operacao,
        v_display.

    WHEN c_but_ce.

      PERFORM zf_apaga_ultimo_digioto.

    WHEN c_but_add.

      PERFORM zf_preenche_value1 USING c_add.

    WHEN c_but_sub.

      PERFORM zf_preenche_value1 USING c_sub.

    WHEN c_but_div.

      PERFORM zf_preenche_value1 USING c_div.

    WHEN c_but_mul.

      PERFORM zf_preenche_value1 USING c_mul.

    WHEN c_but_eq.

      PERFORM zf_equals USING v_operacao.

  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_preenche_display
*&---------------------------------------------------------------------*
*& Preenchimento do display com os dados.
*&---------------------------------------------------------------------*
FORM zf_preenche_display  USING p_value.

  v_display = | { v_display }{ p_value } |.
  CONDENSE v_display NO-GAPS.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_preenche_value1
*&---------------------------------------------------------------------*
*& Preenchimento da variável value1.
*&---------------------------------------------------------------------*
FORM zf_preenche_value1  USING p_operador.

  CLEAR:
    v_value1,
    v_operacao.

  v_value1 = v_display.

  CLEAR:
    v_display.

  v_operacao = p_operador.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_equals
*&---------------------------------------------------------------------*
*& Ação do botão de igual, onde é realizado a operaçõa
*& aritimética selecionada.
*&---------------------------------------------------------------------*
FORM zf_equals  USING p_operacao.

  IF v_display IS INITIAL.

    v_display = c_0.

  ENDIF.

  CASE p_operacao.
    WHEN c_add.

      CLEAR: v_value2.
      v_value2 = v_display.

      CLEAR: v_result.
      v_result = v_value1 + v_value2.

      CLEAR: v_display.
      v_display = v_result.
      CONDENSE v_display NO-GAPS.

    WHEN c_sub.

      CLEAR: v_value2.
      v_value2 = v_display.

      CLEAR: v_result.
      v_result = v_value1 + v_value2.

      CLEAR: v_display.
      v_display = v_result.
      CONDENSE v_display NO-GAPS.

    WHEN c_div.

      CLEAR: v_value2.
      v_value2 = v_display.

      IF v_value2 EQ c_0.

        MESSAGE TEXT-001 TYPE c_s DISPLAY LIKE c_e. " Divisão por Zero não é permitida.
        RETURN.

        CLEAR: v_display.

      ENDIF.

      CLEAR: v_result.
      v_result = v_value1 / v_value2.

      CLEAR: v_display.
      v_display = v_result.
      CONDENSE v_display NO-GAPS.

    WHEN c_mul.

      CLEAR: v_value2.
      v_value2 = v_display.

      CLEAR: v_result.
      v_result = v_value1 * v_value2.

      CLEAR: v_display.
      v_display = v_result.
      CONDENSE v_display NO-GAPS.

  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_apaga_ultimo_digioto
*&---------------------------------------------------------------------*
*& Apaga o último digito quando presionado o botão CE.
*&---------------------------------------------------------------------*
FORM zf_apaga_ultimo_digioto .

  v_display = substring( val = v_display off = 0 len = strlen( v_display ) - 1 ).

ENDFORM.