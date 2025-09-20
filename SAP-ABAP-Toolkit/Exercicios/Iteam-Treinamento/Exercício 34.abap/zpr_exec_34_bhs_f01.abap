*&---------------------------------------------------------------------*
*&  Include           ZPR_EXEC_34_BHS_F01
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
*&      Form  ZF_LAYOUT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Layout do ALV
*----------------------------------------------------------------------*
FORM zf_layout_alv .

  wa_layout_alv-zebra             = 'X'. "Se o relátorio será exibido zebrada.
  wa_layout_alv-colwidth_optimize = 'X'. "Se os campos serão otimizados os tamanhos.
  wa_layout_alv-box_fieldname     = 'MARK'. "O campo de seleção de linhas.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Preenchimento e exibição do relátorio ALV
*----------------------------------------------------------------------*
FORM zf_display_alv .

  CLEAR wa_fieldcat_alv.
  wa_fieldcat_alv-fieldname     = 'mark'.    "Campo da Ti
  wa_fieldcat_alv-tabname       = 'IT_BEBIDA'. "Nome da Ti.
  wa_fieldcat_alv-no_out        = 'X'.
  wa_fieldcat_alv-edit          = 'X'.
  APPEND wa_fieldcat_alv TO it_fieldcat_alv.

  CLEAR wa_fieldcat_alv.
  wa_fieldcat_alv-fieldname     = 'MATNR'.    "Campo da Ti
  wa_fieldcat_alv-tabname       = 'IT_BEBIDA'. "Nome da Ti.
  wa_fieldcat_alv-ref_fieldname = 'MATNR'.    "Campo de Ref.
  wa_fieldcat_alv-ref_tabname   = 'MARA'.     "Tabela de Ref.
  APPEND wa_fieldcat_alv TO it_fieldcat_alv.

  CLEAR wa_fieldcat_alv.
  wa_fieldcat_alv-fieldname     = 'MAKTX'.
  wa_fieldcat_alv-tabname       = 'IT_BEBIDA'.
  wa_fieldcat_alv-ref_fieldname = 'MAKTX'.
  wa_fieldcat_alv-ref_tabname   = 'MAKT'.
  APPEND wa_fieldcat_alv TO it_fieldcat_alv.

  CLEAR wa_fieldcat_alv.
  wa_fieldcat_alv-fieldname     = 'MEINS'.
  wa_fieldcat_alv-tabname       = 'IT_BEBIDA'.
  wa_fieldcat_alv-ref_fieldname = 'MEINS'.
  wa_fieldcat_alv-ref_tabname   = 'MARA'.
  APPEND wa_fieldcat_alv TO it_fieldcat_alv.

  CLEAR wa_fieldcat_alv.
  wa_fieldcat_alv-fieldname     = 'BRGEW'.
  wa_fieldcat_alv-tabname       = 'IT_BEBIDA'.
  wa_fieldcat_alv-ref_fieldname = 'BRGEW'.
  wa_fieldcat_alv-ref_tabname   = 'MARA'.
  APPEND wa_fieldcat_alv TO it_fieldcat_alv.

  CLEAR wa_fieldcat_alv.
  wa_fieldcat_alv-fieldname     = 'GEWEI'.
  wa_fieldcat_alv-tabname       = 'IT_BEBIDA'.
  wa_fieldcat_alv-ref_fieldname = 'GEWEI'.
  wa_fieldcat_alv-ref_tabname   = 'MARA'.
  APPEND wa_fieldcat_alv TO it_fieldcat_alv.

  CLEAR wa_fieldcat_alv.
  wa_fieldcat_alv-fieldname     = 'VOLUM'.
  wa_fieldcat_alv-tabname       = 'IT_BEBIDA'.
  wa_fieldcat_alv-ref_fieldname = 'VOLUM'.
  wa_fieldcat_alv-ref_tabname   = 'MARA'.
  APPEND wa_fieldcat_alv TO it_fieldcat_alv.

  CLEAR wa_fieldcat_alv.
  wa_fieldcat_alv-fieldname     = 'VOLEH'.
  wa_fieldcat_alv-tabname       = 'IT_BEBIDA'.
  wa_fieldcat_alv-ref_fieldname = 'VOLEH'.
  wa_fieldcat_alv-ref_tabname   = 'MARA'.
  APPEND wa_fieldcat_alv TO it_fieldcat_alv.

  CLEAR wa_fieldcat_alv.
  wa_fieldcat_alv-fieldname        = 'MSG'.
  wa_fieldcat_alv-tabname          = 'IT_BEBIDA'.
  wa_fieldcat_alv-reptext_ddic    = 'Observação'. " Descrição do campo
  wa_fieldcat_alv-inttype         = 'C'.          " Tipo do Campo
  wa_fieldcat_alv-outputlen       = 60.           " Tamanho do campo
  APPEND wa_fieldcat_alv TO it_fieldcat_alv.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'ZF_SUB_PF_STATUS'
      i_callback_user_command  = 'ZF_USER_COMMAND'
      i_grid_title             = 'Bebidas'
      is_layout                = wa_layout_alv
      it_fieldcat              = it_fieldcat_alv
    TABLES
      t_outtab                 = it_bebida
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.

    MESSAGE text-e03 TYPE 'S' DISPLAY LIKE 'E'. "Mensagem de erro ao exibir relátorio ALV.
    LEAVE LIST-PROCESSING. "Volta o processo.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_SUB_PF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Seta o status GUI do relátorio.
*----------------------------------------------------------------------*
FORM zf_sub_pf_status USING rt_extab TYPE slis_t_extab.

  SET PF-STATUS 'ZSTATUS'. "Seta o STATUS GUI.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  zf_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Verifica as ações realizadas no relátorio.
*----------------------------------------------------------------------*
FORM zf_user_command USING p_ucomm LIKE sy-ucomm
                           rs_selfield TYPE slis_selfield.

  CASE p_ucomm. "Caso botão selecionado.
    WHEN '&GRAVAR'. "Gravar

      PERFORM zf_select_lines. "Chama perform de linhas selecionadas.

      rs_selfield-refresh = 'X'. "Reinicia o ALV após o processamento.

    WHEN OTHERS. "Caso outros.
  ENDCASE. "FIM CASE p_UCCOM.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_LINES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Verifica as linhas selecionadas no relátorio.
*----------------------------------------------------------------------*
FORM zf_select_lines.

  LOOP AT it_bebida INTO wa_bebida WHERE mark = 'X'. "Faz um loop na tabela de saída verificando os campos que o campo mark está marcado e passa o valor do laço atual para um work area.

    CALL FUNCTION 'ZFM_EXEC_BI_BHS' "Chama a transação de batch input.
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

    SORT it_erro    BY status. "Ordena a tabela de erro por status.
    SORT it_sucesso BY status. "Ordena a tabela de sucesso por status.

    IF sy-subrc = 0. "Verifica erro.
      READ TABLE it_erro INTO wa_erro WITH KEY status = 'E' BINARY SEARCH. "Lê a tabela de erro verificando os status igual a 'E'.

      IF sy-subrc = 0. "Verifica se ocorreu erro.
        wa_bebida-msg = wa_erro-observ. "Passa a observação para a work area de saida.
        wa_bebida-status = 'E'.  "Passa o status de sucesso.

        MODIFY it_bebida FROM wa_bebida. "Modifica a tabela interna com base na work area.
        FREE it_erro. "Limpa a tabela interna de erro.
      ENDIF.

      READ TABLE it_sucesso INTO wa_sucesso WITH KEY status = 'S' BINARY SEARCH. "Lê a tabela de erro verificando os status igual a 'S'.

      IF sy-subrc = 0. "Verifica se ocorreu erro.
        wa_bebida-msg = wa_sucesso-observ. "Passa a observação para a work area de saida.
        wa_bebida-status = 'S'. "Passa o status de sucesso.

        MODIFY it_bebida FROM wa_bebida. "Modifica a tabela interna com base na work area.
        FREE it_sucesso. "Limpa a tabela interna de sucesso.
      ENDIF. "FIM if sy-subrc = 0.
    ENDIF.

  ENDLOOP. "Fim loop at it_bebida.

  IF sy-subrc <> 0. "Verifica se ocorreu erro.

    MESSAGE: text-e01 TYPE 'S' DISPLAY LIKE 'E'. "Menssagem de erro.

  ENDIF. "FIM if sy-subrc <> 0.

ENDFORM.