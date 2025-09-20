*&---------------------------------------------------------------------*
*&  Include           ZPR_EXEC_32_BHS_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_FILE
*&---------------------------------------------------------------------*
*& -> Seleção do local no qual o arquivo está armazenado.
*&---------------------------------------------------------------------*
FORM zf_select_file.
  DATA: lt_filetable TYPE filetable. "Variavel local do tipo filetable.
  DATA: lv_rc        TYPE i. "Variavel local do tipo inteiro.

  "Chama o metodo de janela de seleção de arquivo.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title = 'Selecione um arquivo' "Nome da janela
    CHANGING
      file_table   = lt_filetable "Guarda a localização do arquivo.
      rc           = lv_rc
    EXCEPTIONS
      OTHERS       = 1.

  IF sy-subrc = 0. "Verifica senão ocorreu erro ao seleciona o arquivo.
    READ TABLE lt_filetable INDEX 1 INTO DATA(ls_file).
    gv_filename = ls_file-filename.
    MESSAGE 'Arquivo selecionado: &1' TYPE 'S' DISPLAY LIKE 'I'.
  ELSE.
    MESSAGE 'Nenhum arquivo selecionado ou ocorreu um erro.' TYPE 'E'.
  ENDIF.

  p_loc = gv_filename.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_UPLOAD_FILE
*&---------------------------------------------------------------------*
*& -> Upload do arquivo.
*&---------------------------------------------------------------------*

FORM zf_upload_file USING p_filename.

  "Chama a função que faz o upload do ar  arquivo.
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename = p_filename "Local do arquivo com nome do arquivo.
    TABLES
      data_tab = it_entrada "Tabela onde os dados dos arquivos serão armazenados
    EXCEPTIONS
      OTHERS   = 1.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  zf_process_file
*&---------------------------------------------------------------------*
*& -> Processamento do arquivo
*&---------------------------------------------------------------------*
FORM zf_process_file.

  IF rb_csv = 'X'. "Verifica se o radio button da extensão do arquivo csv está selecionado.
    LOOP AT it_entrada INTO wa_entrada. "Loop na tabela interna passando os valores do laço atual para uma work area

      CLEAR wa_bebida. "Limpa a work area atual.

      SPLIT wa_entrada AT ',' INTO wa_bebida_csv-matnr
                                   wa_bebida_csv-maktx
                                   wa_bebida_csv-meins
                                   wa_bebida_csv-brgew
                                   wa_bebida_csv-gewei
                                   wa_bebida_csv-volum
                                   wa_bebida_csv-voleh. "Separa a linha conforme ',' em seus respectivos campos.

      APPEND wa_bebida_csv TO it_bebida_csv. "Preenche a tabela interna com os dados da work area.
    ENDLOOP.

    MOVE-CORRESPONDING it_bebida_csv TO it_bebida. "Passa os dados da tabela interna para outra.

  ELSEIF rb_txt = 'X'. "Verifica se o botão de arquivo .txt está selecionado.

    LOOP AT it_entrada INTO wa_entrada. "Loop na tabela interna passando os dados da linha atual para uma work area.

      CLEAR wa_bebida. "Limpa a work area.

      wa_bebida-matnr = wa_entrada+1(18).   "Pega os valores conforme especificado.
      wa_bebida-maktx = wa_entrada+18(40).  "Pega os valores conforme especificado.
      wa_bebida-meins = wa_entrada+58(3).   "Pega os valores conforme especificado.

*      wa_bebida-brgew = wa_entrada+62(16).  "Pega os valores conforme especificado.

      WRITE wa_entrada+62(16) TO wa_bebida-brgew. "Escreve o valor do campo da work area de entrada na work area de saida.
      CONDENSE wa_bebida-brgew. "Condensa os valores tirando os espaços em branco.

      wa_bebida-brgew = '1000'.  "Pega os valores conforme especificado.
      wa_bebida-gewei = wa_entrada+79(3).   "Pega os valores conforme especificado.


      WRITE wa_entrada+82(16) TO wa_bebida-volum. "Escreve o valor do campo da work area de entrada na work area de saida.
      CONDENSE wa_bebida-volum NO-GAPS. "Condensa os valores tirando os espaços em branco.

      wa_bebida-voleh = wa_entrada+99(3).   "Pega os valores conforme especificado.

      APPEND wa_bebida TO it_bebida. "Preenche a tabela interna conforme os valores da work area.

    ENDLOOP. "Fim loop it_entrada into wa_entrada.

  ENDIF. "Fim if.

ENDFORM. "Fim form zf_process_file.

*&---------------------------------------------------------------------*
*&      FORM  ZF_PROCESS_BDC
*&---------------------------------------------------------------------*
*& -> Processa as chamadas das rotinas do BDC
*&---------------------------------------------------------------------*
FORM zf_process_bdc.

  DESCRIBE TABLE it_bebida LINES gv_lidos. "Passa a quantidade de linhas da tabela para a varialvel.

  LOOP AT it_bebida INTO wa_bebida. "Loop na tabela interna passando os dados do laço atual para uma work area.

    CLEAR it_bdcdata. "Limpa a work area.

    PERFORM z_monta_shdb USING wa_bebida. "Chama o form que vai montar o shdb utilizando a work area.
    PERFORM z_carrega_transacao USING wa_bebida. "Chama o form que vai chamar a transação utilizando a work area.
    PERFORM zf_exibe_mensagens. "Chama o form de exibir as mensagens.

  ENDLOOP. "Fim loop it_bebida.
ENDFORM. "Fim form zf_process_bdc.

*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_SHDB
*&---------------------------------------------------------------------*
*& ->Monta o shdb utilizando performs que preenchem os campos da tabela
*& BDC
*&---------------------------------------------------------------------*
FORM z_monta_shdb USING p_bebida TYPE ty_bebida.


  PERFORM z_preenche_bdc USING: "Chama o form que vai preencher a tabela BDC passando os valores a serem utilizados.

  'X'    'SAPLMGMM'       '0060',
*  ' '    'BDC_CURSOR'     'RMMG1-MATNR',
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

  CALL TRANSACTION 'MM01' USING it_bdcdata
                          MODE  gv_mode
                          UPDATE gv_s
                          MESSAGES INTO it_msg. "Chama a transação passando os dados armazenados na tabela interna em modo background guardando as messagens em uma tabela interna.

  IF sy-subrc = 0. "Verifica senão ocorreu erro na chamada da transação.

    p_bebida-status = 'S'. "Passa o status de sucesso.

    ADD 1 TO gv_ok. "Adiciona 1 no contador de sucesso.

    READ TABLE it_msg INTO DATA(wa_msg) WITH KEY msgtyp = 'S'. "Lê a tabela de mensagem procurando o typo de mensagem de erro.

    IF sy-subrc = 0. "Verifica se a leitura ocorreu com sucesso.

      p_bebida-observacao = wa_msg-msgnr. "Passa o campo de mensagem como observação.

    ENDIF. "Fim if sy-subrc = 0.

  ELSE. "Senão

    p_bebida-status = 'E'. "Passa o status de erro.

    READ TABLE it_msg INTO wa_msg WITH KEY msgtyp = 'E'. "Lê a tabela de mensagem procurando o typo de mensagem de erro.

    IF sy-subrc = 0. "Verifica se a leitura ocorreu com sucesso.

      p_bebida-observacao = wa_msg-msgnr. "Passa o campo de mensagem como observação.

    ENDIF. "Fim if sy-subrc = 0.

    ADD 1 TO gv_nok. "Adiciona 1 no contador de erro.

  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_GERA_PASTA
*&---------------------------------------------------------------------*
*& -> Gera o log de da transação.
*&---------------------------------------------------------------------*
FORM zf_gera_pasta CHANGING p_status TYPE c
                            p_observacao TYPE char100.

  DATA: l_pasta TYPE apqi-groupid.

  CONCATENATE 'p_' wa_bebida-matnr INTO l_pasta. "Concatena os valores para formar o nome do grupo da pasta log.
  CONDENSE l_pasta NO-GAPS. "Remove os espaços em branco da string.

  CALL FUNCTION 'BDC_OPEN_GROUP' "Chama função de abrir o grupo bdc.
    EXPORTING
      client = sy-mandt
      group  = l_pasta
      keep   = 'X'
      user   = sy-uname
    EXCEPTIONS
      OTHERS = 11.

  IF sy-subrc = 0.
    CALL FUNCTION 'BDC_INSERT' "Chama a função de inserir dados na pasta bdc.
      EXPORTING
        tcode     = 'MM01'
      TABLES
        dynprotab = it_bdcdata
      EXCEPTIONS
        OTHERS    = 7.

    IF sy-subrc = 0. "Verifica senão ocorreu erro na insersão na pasta.

      CALL FUNCTION 'BDC_CLOSE_GROUP'. "Fecha a pasta bdc.
      p_status = 'S'. "Passa o status de sucesso.
*      ADD 1 TO gv_. "Adiciona 1 no contador de suceso.

      p_observacao = 'Pasta gerada com sucesso: ' && l_pasta. "Passa a observação.

    ELSE. "Senão.

      p_status = 'E'. "Passa o status de erro.
      p_observacao = 'Erro ao gerar a pasta: ' && l_pasta. "Passa a observação.
      ADD 1 TO gv_nok. "Adiciona 1 no contador de erro.

    ENDIF. "Fim if sy-subrc = 0.

  ELSE. "Senão.

    p_status = 'E'. "Passa o status de erro.
    p_observacao = 'Erro ao abrir grupo para a pasta: ' && l_pasta. "Passa a observação.
    ADD 1 TO gv_nok. "Adiciona 1 no contador de erro.

  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_EXIBE_MENSAGENS
*&---------------------------------------------------------------------*
*& -> Exibe o log
*&---------------------------------------------------------------------*
FORM zf_exibe_mensagens.
  DATA: lin_size TYPE i VALUE 123. "Define uma varial local que tem o valor inteiro 123.

  IF wa_bebida-status = 'E'. "Verifica o status da bebida.

    FORMAT RESET. "Limpa a formatação.
    FORMAT COLOR COL_NEGATIVE INTENSIFIED ON. "Passa a formatação de vermelho em caso de erro.

  ELSE. "Senão.

    FORMAT RESET. "Limpa a formação.
    FORMAT COLOR COL_POSITIVE INTENSIFIED ON. "Passa a formatação de verde para caso de sucesso.

  ENDIF. "Fim wa_bebida-status = 'E'.

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
        000 sy-vline,
        / sy-uline(lin_size). "Exibe o registro.
ENDFORM.