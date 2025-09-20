*----------------------------------------------------------------------*
*INCLUDE LZGF_BI_BHSF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZF_CARREGA_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_carrega_bdc .

  PERFORM z_preenche_bdc USING: "Chama o form que vai preencher a tabela BDC passando os valores a serem utilizados.

    'X'    'SAPLMGMM'       '0060',
    ' '    'BDC_OKCODE'     '/00',
    ' '    'RMMG1-MATNR'    wa_bebida-matnr,
    ' '    'RMMG1-MBRSH'    '1',
    ' '    'RMMG1-MTART'    'FGTR'.

  PERFORM z_preenche_bdc USING: "Chama o form que vai preencher a tabela BDC passando os valores a serem utilizados.

    'X'    'SAPLMGMM'                  '0070',
    ' '    'BDC_CURSOR'                'MSICHTAUSW-DYTXT(06)',
    ' '    'BDC_OKCODE'                '=ENTR',
    ' '    'MSICHTAUSW-KZSEL(01)'      'X',
    ' '    'MSICHTAUSW-KZSEL(06)'      'X'.

  PERFORM z_preenche_bdc USING: "Chama o form que vai preencher a tabela BDC passando os valores a serem utilizados.

    'X'    'SAPLMGMM'                  '0080',
    ' '    'BDC_CURSOR'                'RMMG1-WERKS',
    ' '    'BDC_OKCODE'                '=ENTR',
    ' '    'RMMG1-WERKS'               '7000'.

  PERFORM z_preenche_bdc USING: "Chama o form que vai preencher a tabela BDC passando os valores a serem utilizados.

    'X'    'SAPLMGMM'                  '4004',
    ' '    'BDC_OKCODE'                '/00',
    ' '    'MAKT-MAKTX'                wa_bebida-maktx,
    ' '    'MARA-MEINS'                wa_bebida-meins,
    ' '    'MARA-MATKL'                '00804',
    ' '    'MARA-MTPOS_MARA'           'NORM',
    ' '    'BDC_CURSOR'                'MARA-VOLUM',
    ' '    'MARA-BRGEW'                wa_bebida-brgew,
    ' '    'MARA-GEWEI'                wa_bebida-gewei,
    ' '    'MARA-VOLUM'                wa_bebida-volum,
    ' '    'MARA-VOLEH'                wa_bebida-voleh.

  PERFORM z_preenche_bdc USING: "Chama o form que vai preencher a tabela BDC passando os valores a serem utilizados.

  'X'    'SAPLMGMM'                  '4000',
  ' '    'BDC_OKCODE'                '=BU',
  ' '    'MAKT-MAKTX'                wa_bebida-maktx,
  ' '    'MARA-MEINS'                wa_bebida-meins,
  ' '    'MARA-BRGEW'                wa_bebida-brgew,
  ' '    'MARA-GEWEI'                wa_bebida-gewei,
  ' '    'MARC-MTVFP'                '01',
  ' '    'BDC_CURSOR '               'MARC-LADGR',
  ' '    'MARA-TRAGR'                '0005',
  ' '    'MARC-LADGR'                '0003'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_BDC
*&---------------------------------------------------------------------*
*& -> Preenche a tabela BDC conforme os dados passados nos performs.
*&---------------------------------------------------------------------*
FORM z_preenche_bdc USING p_dynbegin
                          p_name
                          p_value.

  wa_bdcdata-dynbegin = p_dynbegin. "Passa o valor passado no p_dynbegin para a work area.
  wa_bdcdata-program = 'SAPLMGMM'. "Passa o valor em Hard Code para a work area.

  IF p_dynbegin = 'X'. "Verifica se o p_dynbegin está marcada.

    wa_bdcdata-dynpro = p_value. "Passa o valor para o campo da work area.

  ELSE. "Senão

    wa_bdcdata-fnam = p_name. "Passa o valor para o campo da work area.
    wa_bdcdata-fval = p_value. "Passa o valor para o campo da work area.

  ENDIF. "Fim if p_dynbegin.

  APPEND wa_bdcdata TO it_bdcdata. "Preenche a tabela interna com os dados guardados na work area.
  CLEAR wa_bdcdata.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_CARREGA_TRANSACAO
*&---------------------------------------------------------------------*
*& -> Chama a transação e verifica se ocorreu erro.
*&---------------------------------------------------------------------*
FORM z_carrega_transacao USING p_bebida TYPE ty_bebida.

  DATA: lv_msgv1 LIKE balm-msgv1.
  DATA: lv_msgv2 LIKE balm-msgv2.
  DATA: lv_msgv3 LIKE balm-msgv3.
  DATA: lv_msgv4 LIKE balm-msgv4.

  CALL TRANSACTION 'MM01' USING it_bdcdata
                          MODE  gv_mode
                          UPDATE gv_s
                          MESSAGES INTO it_msg. "Chama a transação passando os dados armazenados na tabela interna em modo background guardando as messagens em uma tabela interna.

  IF sy-subrc = 0. "Verifica senão ocorreu erro na chamada da transação.

    p_bebida-status = 'S'. "Passa o status de sucesso.

    ADD 1 TO gv_ok. "Adiciona 1 no contador de sucesso.

    TRANSLATE wa_bebida-matnr TO UPPER CASE.

    READ TABLE it_msg INTO DATA(wa_msg) WITH KEY msgtyp = 'S'
                                                 msgv1 = wa_bebida-matnr. "Lê a tabela de mensagem procurando o typo de mensagem de erro.

    lv_msgv1 = wa_msg-msgv1.
    lv_msgv2 = wa_msg-msgv2.
    lv_msgv3 = wa_msg-msgv3.
    lv_msgv4 = wa_msg-msgv4.

    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        language               = 'P'
        msg_id                 = wa_msg-msgid
        msg_no                 = wa_msg-msgnr
        msg_var1               = lv_msgv1
        msg_var2               = lv_msgv2
        msg_var3               = lv_msgv3
        msg_var4               = lv_msgv4
      IMPORTING
        msg_text               = gv_texto
      EXCEPTIONS
        function_not_completed = 1
        message_not_found      = 2
        OTHERS                 = 3.


    IF sy-subrc = 0. "Verifica se a leitura ocorreu com sucesso.

      p_bebida-observacao = gv_texto. "Passa o campo de mensagem como observação.

    ENDIF. "Fim if sy-subrc = 0.

    FREE it_bdcdata.
  ELSE. "Senão

    PERFORM zf_gera_pasta.

    p_bebida-status = 'E'. "Passa o status de erro.

    READ TABLE it_msg INTO wa_msg WITH KEY msgtyp = 'E'. "Lê a tabela de mensagem procurando o typo de mensagem de erro.

    lv_msgv1 = wa_msg-msgv1.
    lv_msgv2 = wa_msg-msgv2.
    lv_msgv3 = wa_msg-msgv3.
    lv_msgv4 = wa_msg-msgv4.

    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        language               = 'P'
        msg_id                 = wa_msg-msgid
        msg_no                 = wa_msg-msgnr
        msg_var1               = lv_msgv1
        msg_var2               = lv_msgv2
        msg_var3               = lv_msgv3
        msg_var4               = lv_msgv4
      IMPORTING
        msg_text               = gv_texto
      EXCEPTIONS
        function_not_completed = 1
        message_not_found      = 2
        OTHERS                 = 3.

    IF sy-subrc = 0. "Verifica se a leitura ocorreu com sucesso.

      p_bebida-observacao = gv_texto. "Passa o campo de mensagem como observação.

    ENDIF. "Fim if sy-subrc = 0.

    ADD 1 TO gv_nok. "Adiciona 1 no contador de erro.
*    APPEND gv_nok TO it_erro.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_GERA_PASTA
*&---------------------------------------------------------------------*
*& -> Gera o log de da transação.
*&---------------------------------------------------------------------*
FORM zf_gera_pasta.
  DATA: l_pasta TYPE apqi-groupid.

  CONCATENATE 'p_' wa_bebida-matnr INTO l_pasta. "Concatena os valores para formar o nome do grupo da pasta log.
  CONDENSE l_pasta NO-GAPS. "Remove os espaços em branco da string.

  CALL FUNCTION 'BDC_OPEN_GROUP' "Chama função de abrir o grupo bdc.
    EXPORTING
      client              = sy-mandt
      group               = l_pasta
      keep                = 'X'
      user                = sy-uname
    EXCEPTIONS
      client_invalid      = 1
      destination_invalid = 2
      group_invalid       = 3
      group_is_locked     = 4
      holddate_invalid    = 5
      internal_error      = 6
      queue_error         = 7
      running             = 8
      system_lock_error   = 9
      user_invalid        = 10
      OTHERS              = 11.

  IF sy-subrc = 0.
    CALL FUNCTION 'BDC_INSERT' "Chama a função de inserir dados na pasta bdc.
      EXPORTING
        tcode            = 'MM01'
      TABLES
        dynprotab        = it_bdcdata
      EXCEPTIONS
        internal_error   = 1
        not_open         = 2
        queue_error      = 3
        tcode_invalid    = 4
        printing_invalid = 5
        posting_invalid  = 6
        OTHERS           = 7.

    IF sy-subrc = 0. "Verifica senão ocorreu erro na insersão na pasta.

      CALL FUNCTION 'BDC_CLOSE_GROUP'. "Fecha a pasta bdc.

    ENDIF. "Fim if sy-subrc = 0.

  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_EXIBE_MENSAGENS
*&---------------------------------------------------------------------*
*& -> Exibe o log
*&---------------------------------------------------------------------*
FORM zf_exibe_mensagens.

*  IF wa_bebida-status = 'E'. "Verifica o status da bebida.
*
*    FORMAT RESET. "Limpa a formatação.
*    FORMAT COLOR COL_NEGATIVE INTENSIFIED ON. "Passa a formatação de vermelho em caso de erro.
*
*  ELSE. "Senão.
*
*    FORMAT RESET. "Limpa a formação.
*    FORMAT COLOR COL_POSITIVE INTENSIFIED ON. "Passa a formatação de verde para caso de sucesso.
*
*  ENDIF. "Fim wa_bebida-status = 'E'.

  CASE wa_bebida-status.
    WHEN 'E'.
      FORMAT RESET. "Limpa a formatação.
      FORMAT COLOR COL_NEGATIVE INTENSIFIED ON. "Passa a formatação de vermelho em caso de erro.
    WHEN 'S'.
      FORMAT RESET. "Limpa a formatação.
      FORMAT COLOR COL_POSITIVE INTENSIFIED ON. "Passa a formatação de vermelho em caso de erro.
    WHEN OTHERS.
  ENDCASE.

  WRITE:/000 sy-vline,
        000 wa_bebida-matnr,
        000 sy-vline,
        000 wa_bebida-maktx,
        000 sy-vline,
        000 wa_bebida-meins,
        000 sy-vline,
        000 wa_bebida-brgew,
        000 sy-vline,
        000 wa_bebida-gewei,
        000 sy-vline,
        000 wa_bebida-volum,
        000 sy-vline,
        000 wa_bebida-voleh,
        000 sy-vline.
*        / sy-uline(lin_size). "Exibe o registro.

  FREE it_bdcdata.
ENDFORM.