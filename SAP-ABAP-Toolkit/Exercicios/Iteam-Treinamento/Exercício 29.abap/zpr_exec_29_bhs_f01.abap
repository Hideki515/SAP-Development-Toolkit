*&---------------------------------------------------------------------*
*&  Include           ZPR_EXEC_29_BHS_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Seleciona os dados com base nos parâmetros de seleção.
*----------------------------------------------------------------------*
FORM zf_select_data .

  SELECT mblnr
         mjahr
         bldat
    FROM mkpf
    INTO TABLE it_mkpf
    WHERE
    mblnr IN s_mblnr AND
    mjahr = p_mjahr. "Seleção de dados da tabela mkpf cam base nos parâmetros de seleção.

  IF sy-subrc <> 0. "Em caso de erro exibir a mensagem.

    FREE: it_mkpf.
    MESSAGE: text-e01 TYPE c_sucess DISPLAY LIKE c_error.

  ENDIF.

  IF it_mkpf IS NOT INITIAL. "Verifica se a tabela está vazia.

    SELECT mblnr
         mjahr
         zeile
         bwart
         matnr
         werks
         lgort
         dmbtr
         menge
         meins
    FROM mseg
    INTO TABLE it_mseg
    FOR ALL ENTRIES IN it_mkpf
    WHERE
    mblnr = it_mkpf-mblnr AND
    mjahr = it_mkpf-mjahr AND
    bwart IN s_bwart. "Seleção de dados da tabela mseg cam base nos parâmetros da tabela mkpf.
  ENDIF.

  IF sy-subrc <> 0. "Em caso de erro exibir a mensagem.

    FREE: it_mkpf.
    FREE: it_mseg.
    MESSAGE: text-e01 TYPE c_sucess DISPLAY LIKE c_error.

  ENDIF.

  IF it_mseg IS NOT INITIAL.

    SELECT matnr
    maktx
    FROM makt
    INTO TABLE it_makt
    FOR ALL ENTRIES IN it_mseg
    WHERE
    matnr = it_mseg-matnr AND
    spras = sy-langu. "Seleção de dados da tabela makt cam base nos parâmetros da tabela mseg.

    IF sy-subrc <> 0. "Em caso de erro exibir a mensagem.

      FREE: it_mkpf.
      FREE: it_mseg.
      FREE: it_makt.
      MESSAGE: text-e01 TYPE c_sucess DISPLAY LIKE c_error.

    ENDIF.

    SELECT werks
    name1
    FROM t001w
    INTO TABLE it_t001w
    FOR ALL ENTRIES IN it_mseg
    WHERE
    werks = it_mseg-werks. "Seleção de dados da tabela t001w cam base nos parâmetros da tabela mseg.

    IF sy-subrc <> 0. "Em caso de erro exibir a mensagem.

      FREE: it_mkpf.
      FREE: it_mseg.
      FREE: it_makt.
      FREE: it_t001w.
      MESSAGE: text-e01 TYPE c_sucess DISPLAY LIKE c_error.

    ENDIF.

    SELECT werks
    lgort
    lgobe
    FROM t001l
    INTO TABLE it_t001l
    FOR ALL ENTRIES IN it_mseg
    WHERE
    werks = it_mseg-werks AND
    lgort = it_mseg-lgort. "Seleção de dados da tabela t001l cam base nos parâmetros da tabela mseg.

    IF sy-subrc <> 0. "Em caso de erro exibir a mensagem.

      FREE: it_mkpf.
      FREE: it_mseg.
      FREE: it_makt.
      FREE: it_t001w.
      FREE: it_t001l.

      MESSAGE: text-e01 TYPE c_sucess DISPLAY LIKE c_error.

    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Processa os dados selecionados com base nos parâmetros.
*----------------------------------------------------------------------*
FORM zf_process_data .

  SORT: it_mkpf  BY mblnr
                    mjahr,
        it_mseg  BY mblnr,
        it_makt  BY matnr,
        it_t001w BY werks,
        it_t001l BY werks
                    lgort.

  LOOP AT it_mseg INTO wa_mseg.

    READ TABLE it_mkpf INTO wa_mkpf WITH KEY mblnr = wa_mseg-mblnr
                                             mjahr = wa_mseg-mjahr BINARY SEARCH.

    IF sy-subrc = 0.

      READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_mseg-matnr BINARY SEARCH.

      IF sy-subrc NE 0. "Verifica se o sy-subrc é diferente de zero.
        CONTINUE. "Passa para o próximo registro do loop.
      ENDIF. "Fim sy-subrc NE 0.

      READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_mseg-werks BINARY SEARCH.

      IF sy-subrc NE 0. "Verifica se o sy-subrc é diferente de zero.
        CONTINUE. "Passa para o próximo registro do loop.
      ENDIF. "Fim sy-subrc NE 0.

      READ TABLE it_t001l INTO wa_t001l WITH KEY werks = wa_mseg-werks
                                                 lgort = wa_mseg-lgort BINARY SEARCH.
      IF sy-subrc NE 0. "Verifica se o sy-subrc é diferente de zero.
        CONTINUE. "Passa para o próximo registro do loop.
      ENDIF. "Fim sy-subrc NE 0.

      CLEAR wa_saida. "Limpa a work area de saída.

      CONCATENATE: wa_mseg-matnr wa_makt-maktx INTO wa_saida-mat_mak SEPARATED BY '-'. "Concatena os campos matnr e maktx separados por '-'.
      CONCATENATE: wa_mseg-werks wa_t001w-name1 INTO wa_saida-wer_nam SEPARATED BY '-'. "Concatena os campos werks e name1 separados por '-'.
      CONCATENATE: wa_mseg-lgort wa_t001l-lgobe INTO wa_saida-ort_obe SEPARATED BY '-'. "Concatena os campos lgort e lgobe separados por '-'.

      wa_saida-mblnr = wa_mseg-mblnr. "Preenche o campo da work area de saída.
      wa_saida-mjahr = wa_mseg-mjahr. "Preenche o campo da work area de saída.
      wa_saida-zeile = wa_mseg-zeile. "Preenche o campo da work area de saída.
      wa_saida-bwart = wa_mseg-bwart. "Preenche o campo da work area de saída.
      wa_saida-bldat = wa_mkpf-bldat. "Preenche o campo da work area de saída.
      wa_saida-menge = wa_mseg-menge. "Preenche o campo da work area de saída.
      wa_saida-meins = wa_mseg-meins. "Preenche o campo da work area de saída.
      wa_saida-dmbtr = wa_mseg-dmbtr. "Preenche o campo da work area de saída.

      APPEND wa_saida TO it_saida. "Preenche a tabela interna de saída com base nos valores da work area.
    ENDIF. "Fim IF sy-subrc = 0.

  ENDLOOP. "Fim LOOP AT it_mseg.

  LOOP AT it_saida INTO wa_saida. "Loop na tabela interna de saída passando os valores da linha atual para work area.

    wa_saida-valor = wa_saida-dmbtr / wa_saida-menge. "Calcula o valor do campo da work area.

    MODIFY it_saida FROM wa_saida INDEX sy-tabix. "Modifica o valor da tabela interna com base na work area.

  ENDLOOP. "Fim  LOOP AT it_saida.

ENDFORM.

FORM zf_top_page.

  DATA: lv_data      TYPE char10.
  DATA: lv_hora      TYPE char5.
  DATA: lv_timestamp TYPE char50.

  FREE it_top_of_page. "Limpa os valores dos campos da tabela interna it_top_of_page.

  CONCATENATE sy-datum+6(2)
              sy-datum+4(2)
              sy-datum+0(4)
             INTO  lv_data SEPARATED BY '/'. "Concatena os valores de dia, mês e anos separados por '/'.

  CONCATENATE sy-uzeit+0(2)
              sy-uzeit+2(2)
             INTO lv_hora SEPARATED BY ':'. "Concatena os valores de hora e minutos separados por ':'.

  CONCATENATE sy-repid  '-' lv_data lv_hora INTO lv_timestamp SEPARATED BY space. "Concatena o nome do programa, data e hora separados por espace.

  CLEAR wa_top_of_page. "Limpa a work area wa_top_of_page.
  wa_top_of_page-typ  = 'S'. "Passa o valor de 'S' para o campo typ da work area.
  wa_top_of_page-info = lv_timestamp. "Passa o timestamp para o campo de info da work area.
  APPEND wa_top_of_page TO it_top_of_page. "Preenche a tabela interna com os valores da work area.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE' "Chama a função de exibição de informações no cabeçalho.
    EXPORTING
      it_list_commentary = it_top_of_page. "Passa a tabela com os valores a serem exibidos.

ENDFORM. "Fim FORM zf_top_page.

*&---------------------------------------------------------------------*
*&      Form  ZF_BROKEN_fieldS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Ordenena e quebra os campos.
*----------------------------------------------------------------------*
FORM zf_broken_fields .

  wa_sort-spos = 1.
  wa_sort-fieldname = 'BWART'.
  wa_sort-tabname = 'IT_SAIDA'.
  wa_sort-subtot = 'X'.
  wa_sort-up = 'X'.
  APPEND wa_sort TO it_sort.
  CLEAR wa_sort.

  wa_sort-spos = 2.
  wa_sort-fieldname = 'BLDAT'.
  wa_sort-tabname = 'IT_SAIDA'.
  APPEND wa_sort TO it_sort.
  CLEAR wa_sort.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_ASSEMBLE_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Modelo FieldCat.
*----------------------------------------------------------------------*
FORM zf_assemble_alv .

  PERFORM zf_assemble_fielcat USING:
        c_bwart  c_it_saida  c_bwart c_mseg '' '' '' '',
        c_bldat  c_it_saida  c_bldat c_mkpf '' '' '' '',
        c_mblnr  c_it_saida  c_mblnr c_mseg c_x '' '' '',
        c_mjahr  c_it_saida  c_mjahr c_mseg '' '' '' '',
        c_zeile  c_it_saida  c_zeile c_mseg '' '' '' '',
        c_mat_mak c_it_saida c_mat_mak '' '' '' '' 'Material',
        c_wer_nam c_it_saida c_wer_nam '' '' '' '' 'Centro',
        c_ort_obe c_it_saida c_ort_obe '' '' '' '' 'Depósito',
        c_menge  c_it_saida  c_menge c_mseg '' c_x '' '',
        c_meins  c_it_saida  c_meins c_mseg '' '' '' '',
        c_dmbtr  c_it_saida  c_dmbtr c_mseg '' c_x '' '',
        c_valor  c_it_saida  c_valor '' c_x '' '' 'Valor Únitario'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_ASSEMBLE_FIELCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      --> Preenchendo FieldCat / Declarando os campos que serão
* impresso no ALV.
*----------------------------------------------------------------------*
FORM zf_assemble_fielcat  USING field        TYPE any
                                tab          TYPE any
                                ref_field    TYPE any
                                ref_tab      TYPE any
                                hotspot      TYPE any
                                sum          TYPE any
                                reptext_ddic TYPE any
                                seltext_l    TYPE any.

  CLEAR wa_fieldcat. "Limpa a work area do fieldcat.

  wa_fieldcat-fieldname     = field.        "Preenche o campo fieldname da work area do fieldcat.
  wa_fieldcat-tabname       = tab.          "Preenche o campo tabname da work area do fieldcat.
  wa_fieldcat-ref_fieldname = ref_field.    "Preenche o campo ref_fieldname da work area do fieldcat.
  wa_fieldcat-ref_tabname = ref_tab.        "Preenche o campo ref_tabname da work area do fieldcat.
  wa_fieldcat-hotspot       = hotspot.      "Preenche o campo hostpor da work area do fieldcat.
  wa_fieldcat-do_sum        = sum.          "Preenche o campo do_sum da work area do fieldcat.
  wa_fieldcat-reptext_ddic  = reptext_ddic. "Preenche o campo reptext_ddic da work area do fieldcat.
  wa_fieldcat-seltext_l   = seltext_l.      "Preenche o campo seltext_l da work area do fieldcat.

  APPEND wa_fieldcat TO it_fieldcat. "Prenche a tabela interna com os valores da work area.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Exibe o relátorio ALV
*----------------------------------------------------------------------*
FORM zf_display_alv .

  wa_fieldcat-fieldname = 'mark'.
  wa_fieldcat-edit = abap_on.
  wa_fieldcat-no_out = abap_true.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_layout-colwidth_optimize = abap_true. "Otima o tamanho da coluna conforme o tamanho da descrição.
  wa_layout-zebra = 'X'. "Faz o relátio ser exibido em zebra.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_pf_status_set = 'SUB_PF_STATUS'
      i_callback_top_of_page   = 'ZF_TOP_PAGE'
      i_callback_program       = sy-repid
      i_grid_title             = 'Relatório de Movimentação de Material'
      it_sort                  = it_sort
      is_layout                = wa_layout "Aqui passa o Layout do ALV
      it_fieldcat              = it_fieldcat "Aqui é passado a estrutura do ALV
    TABLES
      t_outtab                 = it_saida "Aqui passa os valores a serem exibidos no ALV
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc NE 0.
    MESSAGE s208(001) DISPLAY LIKE sy-abcde+4(1) WITH text-e02.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  sub_pf_status
*&---------------------------------------------------------------------*
* Chama o status gui do relátorio ALV.
*----------------------------------------------------------------------*
FORM sub_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'ZXPORT'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
* Verifica o sy-ucomm e chama o perform referente.
*----------------------------------------------------------------------*
FORM user_command USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.

  CASE r_ucomm.
    WHEN '&IC1'. "Verifica se o valor da r_ucomm está com o valor '&IC1' por default.
      CASE rs_selfield-fieldname.
        WHEN 'MBLNR'. "Verifica qual o nome da coluna selecionada.

          SET PARAMETER ID 'MBN' FIELD rs_selfield-value.
          SET PARAMETER ID 'MJA' FIELD s_bwart.

          CALL TRANSACTION 'MB1B'.

        WHEN OTHERS.
          " Nenhuma ação específica necessária para outros casos
      ENDCASE.

    WHEN '&TXT'.
      PERFORM zf_export_txt.

    WHEN '&CSV'.
      PERFORM zf_export_csv.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_ROWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Verifica as linhas selecionadas No relátorio ALV.
*----------------------------------------------------------------------*
FORM zf_select_rows.
  DATA: e_grid TYPE REF TO cl_gui_alv_grid.
  DATA: it_selected_rows TYPE lvc_t_row.
  DATA: lv_indice(10) TYPE n.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = e_grid.

  IF e_grid IS BOUND.

    CALL METHOD e_grid->check_changed_data.
    CALL METHOD e_grid->get_selected_rows
      IMPORTING
        et_index_rows = it_selected_rows.

    FREE it_saida_aux. "Limpa o da tabela interna.

    LOOP AT it_selected_rows ASSIGNING FIELD-SYMBOL(<ls_selected>). "Faz um loop na tabela interna it_select_rows e assina ela com field-symbol.
      lv_indice = <ls_selected>-index. "Passa o indice atual para a variável.

      READ TABLE it_saida INDEX lv_indice ASSIGNING FIELD-SYMBOL(<ls_saida>). "Le a tabela interna com base no indice e assina ela.

      IF sy-subrc = 0.
        APPEND <ls_saida> TO it_saida_aux. "Passa o valor do fiel-symbol para a tabela interna de saída auxiliar.
      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  ZF_EXPORT_TXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Exporta os valores do alv para arquivo txt.
*----------------------------------------------------------------------*
FORM zf_export_txt .
  DATA: lv_filename TYPE string.
  DATA: lv_path     TYPE string.
  DATA: lv_fullpath TYPE string.

  PERFORM zf_select_rows. "Chama o form para verificar as linhas selecionadas.

  IF it_saida_aux IS INITIAL. "Verifica se a tabela de saída não está vazia.

    MESSAGE: text-i01 TYPE c_info.

  ELSE.

    CALL METHOD cl_gui_frontend_services=>file_save_dialog "Método que chama a tela de seleção onde salvar o arquivo.
      EXPORTING
        window_title      = 'Save' "Nome da janela.
        file_filter       = 'TEXT FILES (*.TXT)|*.TXT|' "Filtro por tipo de arquivo.
        initial_directory = 'C:\' "Pasta inicial.
      CHANGING
        filename          = lv_filename "Saída do nome do arquivo.
        path              = lv_path "Saída do caminho onde o arquivo será armazenado.
        fullpath          = lv_fullpath. "Saída caminho total que é o caminho mais o nome do arquivo.

    CALL FUNCTION 'GUI_DOWNLOAD' "Função que faz o download do arquivo.
      EXPORTING
        filename                = lv_fullpath "Entrada do nome do arquivo.
        filetype                = 'DAT'
        write_field_separator   = 'X'
      TABLES
        data_tab                = it_saida_aux "Tabela de onde será os dados baixados.
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.

    IF sy-subrc <> 0.
      MESSAGE: text-e02 TYPE c_sucess DISPLAY LIKE c_error.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_EXPORT_CSV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_export_csv .

  DATA: lv_filename TYPE string.
  DATA: lv_path     TYPE string.
  DATA: lv_fullpath TYPE string.

  DATA: lv_temp TYPE string.
  DATA: lv_temp_menge TYPE string.
  DATA: lv_temp_dmtr TYPE string.
  DATA: lv_temp_valor TYPE string.

  PERFORM zf_select_rows. "Chama o form para verificar as linhas selecionadas.

  IF it_saida_aux IS INITIAL. "Verifica se a tabela de saída não está vazia.

    MESSAGE: text-i01 TYPE c_info. "Messagem de Informação para que ele baixe o arquivo

  ELSE.

    LOOP AT it_saida_aux INTO wa_saida_aux."Loop na tabela interna de saída aux passando os valores para work area.

      lv_temp_menge = wa_saida_aux-menge. "Váriveis temporarias.
      lv_temp_dmtr = wa_saida_aux-dmbtr.
      lv_temp_valor = wa_saida_aux-valor.

      CONCATENATE wa_saida_aux-mblnr
                  wa_saida_aux-mjahr
                  wa_saida_aux-zeile
                  wa_saida_aux-bwart
                  wa_saida_aux-bldat
                  lv_temp_menge
                  wa_saida_aux-mat_mak
                  wa_saida_aux-wer_nam
                  wa_saida_aux-ort_obe
                  wa_saida_aux-meins
                  lv_temp_dmtr
                  lv_temp_valor
      INTO wa_csv
      SEPARATED BY ';'. "Concatena os valores separando ele por ;.

      APPEND wa_csv TO it_csv. "Passar os valores da work area para a tabela interna de saída do csv.
    ENDLOOP.

    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        window_title      = 'Save'
        file_filter       = 'EXCEL FILES (*.CSV)|*.CSV|'
        initial_directory = 'C:\'
      CHANGING
        filename          = lv_filename "Saída do nome do arquivo.
        path              = lv_path "Saída do caminho do arquivo.
        fullpath          = lv_fullpath. "Saída do nome completo que é caminho do arquivo junto com o nome do arquivo.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = lv_fullpath "Nome do arquivo
        filetype                = 'ASC'
        write_field_separator   = 'X'
      TABLES
        data_tab                = it_csv "Tabela com dados a serem baixados.
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.

    IF sy-subrc <> 0.
      MESSAGE: text-e02 TYPE c_sucess DISPLAY LIKE c_error.
    ENDIF.

  ENDIF.
ENDFORM.