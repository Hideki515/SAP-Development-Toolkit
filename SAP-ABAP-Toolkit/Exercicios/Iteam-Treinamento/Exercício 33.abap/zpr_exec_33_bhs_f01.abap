*&---------------------------------------------------------------------*
*&  Include           ZPR_EXEC_32_BHS_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_FILE
*&---------------------------------------------------------------------*
*& -> Seleção do local no qual o arquivo está armazenado.
*&---------------------------------------------------------------------*
FORM zf_select_file CHANGING p_arq TYPE rlgrap-filename.
  DATA: lt_filetable TYPE filetable. "Variavel local do tipo filetable.

* Esta função é utilizada para buscar o arquivo na sua máquina.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    CHANGING
      file_name     = p_arq
    EXCEPTIONS
      mask_too_long = 1
      OTHERS        = 2.

  IF sy-subrc <> 0. "Verifica se ocorreu erro.

    MESSAGE: text-e05 TYPE 'S' DISPLAY LIKE 'E'. "Mensagem de erro ao selecionar arquivo.
    LEAVE LIST-PROCESSING. "Volta o processo.

  ENDIF. "FIM IF sy-subrc <> 0.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_UPLOAD_FILE
*&---------------------------------------------------------------------*
*& -> Upload do arquivo.
*&---------------------------------------------------------------------*
FORM zf_upload_file USING p_arq TYPE rlgrap-filename.
  DATA: lv_filename TYPE string.

  lv_filename = p_arq.
  "Chama a função que faz o upload do ar  arquivo.
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = lv_filename "Local do arquivo com nome do arquivo.
    TABLES
      data_tab                = it_entrada "Tabela onde os dados dos arquivos serão armazenados
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.

  IF sy-subrc <> 0.
    MESSAGE: text-e02 TYPE 'S' DISPLAY LIKE 'E'. "Mensagem de erro ao fazer upload do arquivo.
    LEAVE LIST-PROCESSING. "Volta o processo.
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  zf_process_file
*&---------------------------------------------------------------------*
*& -> Processamento do arquivo
*&---------------------------------------------------------------------*
FORM zf_process_file.

  IF it_entrada IS NOT INITIAL. "Verifica se a tabela está vazia.

    LOOP AT it_entrada INTO wa_entrada. "Loop na tabela interna passando os dados da linha atual para uma work area.

      CLEAR wa_bebida. "Limpa a work area.

      wa_bebida-matnr = wa_entrada-matnr. "Passa o valor da work area de entrada para work area de saída.
      wa_bebida-maktx = wa_entrada-maktx. "Passa o valor da work area de entrada para work area de saída.
      wa_bebida-meins = wa_entrada-meins. "Passa o valor da work area de entrada para work area de saída.
      wa_bebida-brgew = wa_entrada-brgew. "Passa o valor da work area de entrada para work area de saída.
      wa_bebida-gewei = wa_entrada-gewei. "Passa o valor da work area de entrada para work area de saída.
      wa_bebida-volum = wa_entrada-volum. "Passa o valor da work area de entrada para work area de saída.
      wa_bebida-voleh = wa_entrada-voleh. "Passa o valor da work area de entrada para work area de saída.

      APPEND wa_bebida TO it_bebida. "Preenche a tabela interna com os valores da work area.

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

    CALL FUNCTION 'ZFM_EXEC_BI_BHS'
      EXPORTING
        i_matnr    = wa_bebida-matnr
        i_maktx    = wa_bebida-maktx
        i_meins    = wa_bebida-meins
        i_brgew    = wa_bebida-brgew
        i_gewei    = wa_bebida-gewei
        i_volum    = wa_bebida-volum
        i_voleh    = wa_bebida-voleh
      TABLES
        it_erro    = it_erro
        it_sucesso = it_sucesso.

  ENDLOOP. "Fim loop it_bebida.

ENDFORM. "Fim form zf_process_bdc.

FORM zf_message.
  DATA: lv_erro TYPE i. "Variavel local.
  DATA: lv_sucesso TYPE i. "Variavel local.
  DATA: lv_total TYPE i. "Variavel local.

  DESCRIBE TABLE it_bebida LINES lv_total. "Passa a quantidade de registros para contador total.

  LOOP AT it_erro INTO wa_erro.
    ADD 1 TO lv_erro. "Adiciona um no contador de erro.
  ENDLOOP.

  LOOP AT it_sucesso INTO wa_sucesso.
    ADD 1 TO lv_sucesso. "Adiciona um no contador de sucesso.
  ENDLOOP.

  SKIP. "Pula uma linha.

  WRITE: / 'Processamento Concluído.'. "Mensagem de Processamento.
  WRITE: / lv_total, ' registros processados.' COLOR COL_TOTAL. "Quantos registros foram processados.
  WRITE: / lv_sucesso, ' registros processados com sucesso.' COLOR COL_TOTAL. "Mensagem de registros processados com sucesso.
  WRITE: / lv_erro, ' registros com erro.' COLOR COL_TOTAL. "Mensagem de registros processados com sucesso.

  CLEAR lv_erro. "Limpa o contador de errps.
  CLEAR lv_sucesso. "Limpa o contador de sucesso.
ENDFORM.