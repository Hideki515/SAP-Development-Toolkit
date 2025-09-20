*&---------------------------------------------------------------------*
*&  Include           ZPR_EXEC_30_BHS_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Seleciona os dados conforme os parâmetros de seleção.
*----------------------------------------------------------------------*
FORM zf_select_data .

  SELECT vbeln
         erdat
         netwr
         kunnr
    FROM vbak
    INTO TABLE it_vbak
    WHERE
    vbeln IN s_vbeln AND
    erdat IN s_erdat AND
    kunnr IN s_kunnr AND
    auart = 'TA' AND
    waerk = 'EUR'.

  IF sy-subrc <> 0.
    FREE: it_vbak.

    LEAVE LIST-PROCESSING.

    MESSAGE text-e01 TYPE c_sucess DISPLAY LIKE c_error.
  ENDIF.

  IF it_vbak IS NOT INITIAL.
    SELECT vbeln
             posnr
             matnr
             gsber
        FROM vbap
        INTO TABLE it_vbap
        FOR ALL ENTRIES IN it_vbak
        WHERE
        vbeln = it_vbak-vbeln.

    IF sy-subrc <> 0.
      FREE: it_vbak.
      FREE: it_vbap.

      LEAVE LIST-PROCESSING.

      MESSAGE text-e01 TYPE c_sucess DISPLAY LIKE c_error.
    ENDIF.
  ELSE.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF it_vbap IS NOT INITIAL.
    SELECT vbeln
           posnr
           vgbel
           vgpos
      FROM lips
      INTO TABLE it_lips
      FOR ALL ENTRIES IN it_vbap
      WHERE
      vgbel = it_vbap-vbeln AND
      vgpos = it_vbap-posnr AND
      pstyv = 'TAN'.

    IF sy-subrc <> 0.
      FREE: it_vbak.
      FREE: it_vbap.
      FREE: it_lips.

      LEAVE LIST-PROCESSING.

      MESSAGE text-e01 TYPE c_sucess DISPLAY LIKE c_error.
    ENDIF.
  ELSE.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF it_vbak IS NOT INITIAL.
    SELECT kunnr
           name1
      FROM kna1
      INTO TABLE it_kna1
      FOR ALL ENTRIES IN it_vbak
      WHERE
      kunnr = it_vbak-kunnr.

    IF sy-subrc <> 0.
      FREE: it_vbak.
      FREE: it_vbap.
      FREE: it_lips.
      FREE: it_kna1.

      LEAVE LIST-PROCESSING.

      MESSAGE text-e01 TYPE c_sucess DISPLAY LIKE c_error.
    ENDIF.
  ELSE.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF it_vbap IS  NOT INITIAL.
    SELECT matnr
           maktx
     FROM makt
     INTO TABLE it_makt
     FOR ALL ENTRIES IN it_vbap
     WHERE
      matnr = it_vbap-matnr AND
      spras = sy-langu.

    IF sy-subrc <> 0.
      FREE: it_vbak.
      FREE: it_vbap.
      FREE: it_lips.
      FREE: it_kna1.
      FREE: it_makt.

      LEAVE LIST-PROCESSING.

      MESSAGE text-e01 TYPE c_sucess DISPLAY LIKE c_error.
    ENDIF.
  ELSE.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF it_vbap IS NOT INITIAL.
    SELECT gsber
           gtext
      FROM tgsbt
      INTO TABLE it_tgsbt
      FOR ALL ENTRIES IN it_vbap
      WHERE
      gsber = it_vbap-gsber AND
      spras = sy-langu.

    IF sy-subrc <> 0.
      FREE: it_vbak.
      FREE: it_vbap.
      FREE: it_lips.
      FREE: it_kna1.
      FREE: it_makt.
      FREE: it_tgsbt.

      LEAVE LIST-PROCESSING.

      MESSAGE text-e01 TYPE c_sucess DISPLAY LIKE c_error.
    ENDIF.
  ELSE.
    LEAVE LIST-PROCESSING.
  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Processamento dos dados selecionados.
*----------------------------------------------------------------------*
FORM zf_process_data .
  SORT it_vbak BY vbeln.
  SORT it_vbap BY vbeln.
  SORT it_lips BY vbeln
                  vgpos.
  SORT it_kna1 BY kunnr.
  SORT it_makt BY matnr.
  SORT it_tgsbt BY gsber.

  LOOP AT it_vbap INTO wa_vbap.
    READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = wa_vbap-vbeln BINARY SEARCH.

    IF sy-subrc = 0. "Verfica se ocorreu algum erro na leitura na tabela anterior.
      READ TABLE it_lips INTO wa_lips WITH KEY vgbel = wa_vbap-vbeln
                                               vgpos = wa_vbap-posnr BINARY SEARCH. "Lê a tabela e passa os valores para uma work area conforme os parâmetros de seleção.

      IF sy-subrc <> 0. "Verifica se o correu erro.
        CONTINUE. "Pula do laço atual e passa para o próximo laço.

        CLEAR wa_vbap. "Limpa a work area.
        CLEAR wa_vbak. "Limpa a work area.
        CLEAR wa_lips. "Limpa a work area.
      ENDIF.

      READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbak-kunnr BINARY SEARCH. "Lê a tabela e passa os valores para uma work area conforme os parâmetros de seleção.

      IF sy-subrc <> 0. "Verifica se o correu erro.
        CONTINUE. "Pula do laço atual e passa para o próximo laço.

        CLEAR wa_vbap. "Limpa a work area.
        CLEAR wa_vbak. "Limpa a work area.
        CLEAR wa_lips. "Limpa a work area.
        CLEAR wa_kna1. "Limpa a work area.
      ENDIF.

      READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_vbap-matnr BINARY SEARCH. "Lê a tabela e passa os valores para uma work area conforme os parâmetros de seleção.

      IF sy-subrc <> 0. "Verifica se o correu erro.
        CONTINUE. "Pula do laço atual e passa para o próximo laço.
      ENDIF.

      READ TABLE it_tgsbt INTO wa_tgsbt WITH KEY gsber = wa_vbap-gsber BINARY SEARCH. "Lê a tabela e passa os valores para uma work area conforme os parâmetros de seleção.

      IF sy-subrc <> 0. "Verifica se o correu erro.
        CONTINUE. "Pula do laço atual e passa para o próximo laço.

        CLEAR wa_vbap. "Limpa a work area.
        CLEAR wa_vbak. "Limpa a work area.
        CLEAR wa_lips. "Limpa a work area.
        CLEAR wa_kna1. "Limpa a work area.
        CLEAR wa_tgsbt. "Limpa a work area.
      ENDIF.

      CONCATENATE wa_vbak-kunnr wa_kna1-name1  INTO wa_saida-kun_nam SEPARATED BY '-'.
      CONCATENATE wa_vbap-matnr wa_makt-maktx  INTO wa_saida-mat_mak SEPARATED BY '-'.
      CONCATENATE wa_vbap-gsber wa_tgsbt-gtext INTO wa_saida-gsb_gtx SEPARATED BY '-'.

      wa_saida-vbeln     = wa_vbak-vbeln. "Preenche a Work Area de Saída.
      wa_saida-erdat     = wa_vbak-erdat. "Preenche a Work Area de Saída.
      wa_saida-posnr     = wa_vbap-posnr. "Preenche a Work Area de Saída.
      wa_saida-netwr     = wa_vbak-netwr. "Preenche a Work Area de Saída.
      wa_saida-vbeln_lip = wa_lips-vbeln. "Preenche a Work Area de Saída.

      IF wa_vbak-netwr <= 20000. "Verifica se o valor é menor/igual a 20000.

        wa_saida-status = c_sem_red. "Passa o status de semaforo vemelho.

      ELSEIF wa_vbak-netwr > 20000 AND wa_vbak-netwr <= 40000. "Verifica se o valor está entre 20000 a 40000.

        wa_saida-status = c_sem_yellow.  "Passa o status de semaforo amarelo.

      ELSEIF wa_vbak-netwr >= 40000. "Verifica se o valor é maior/igual a 40000.

        wa_saida-status = c_sem_green. "Passa o status de semaforo verde.

      ENDIF.

      APPEND wa_saida TO it_saida. "Preenche a tabela interna com os valores da work area.
      CLEAR wa_saida. "Limpa a Work Area.

    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Criando cabeçalho para o ALV.
*----------------------------------------------------------------------*
FORM zf_top_of_page .
  DATA: lv_data TYPE char12.
  DATA: lv_hora TYPE char12.
  DATA: lv_timestamp TYPE char50.

  FREE: it_top_of_page.

  CONCATENATE sy-datum+6(2)
              sy-datum+4(2)
              sy-datum+0(4)
              INTO  lv_data SEPARATED BY '/'. "Concatena os valores de dia, mês e ano separados por '/'.

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

ENDFORM. "Fim zf_top_of_page.

*&---------------------------------------------------------------------*
*&      Form  ZF_SORT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Ordenena o ALV.
*----------------------------------------------------------------------*
FORM zf_sort_alv .

  wa_sort_alv-spos = 1. "Ordem de ordenação.
  wa_sort_alv-fieldname = c_erdat. "Campo a ser ordenado.
  wa_sort_alv-tabname = c_tab_saida. "Tabela do campo a ser ordenado.
  wa_sort_alv-up = c_x. "Se a ordenação é em ordem cresente.
  APPEND wa_sort_alv TO it_sort_alv. "Preenche a tabela de sort.
  CLEAR wa_sort_alv. "Limpa a Work Area.

  wa_sort_alv-spos = 2. "Ordem de ordenação.
  wa_sort_alv-fieldname = c_kun_nam. "Campo a ser ordenado.
  wa_sort_alv-tabname = c_tab_saida. "Tabela do campo a ser ordenado.
  wa_sort_alv-up = c_x. "Se a ordenação é em ordem cresente.
  APPEND wa_sort_alv TO it_sort_alv. "Preenche a tabela de sort.
  CLEAR wa_sort_alv. "Limpa a Work Area.

  wa_sort_alv-spos = 3. "Ordem de ordenação.
  wa_sort_alv-fieldname = c_gsb_gtx. "Campo a ser ordenado.
  wa_sort_alv-tabname = c_tab_saida. "Tabela do campo a ser ordenado.
  wa_sort_alv-up = c_x. "Se a ordenação é em ordem cresente.
  APPEND wa_sort_alv TO it_sort_alv. "Preenche a tabela de sort.
  CLEAR wa_sort_alv. "Limpa a Work Area.

  wa_sort_alv-spos = 4. "Ordem de ordenação.
  wa_sort_alv-fieldname = c_netwr. "Campo a ser ordenado.
  wa_sort_alv-tabname = c_tab_saida. "Tabela do campo a ser ordenado.
  wa_sort_alv-up = c_x. "Se a ordenação é em ordem cresente.
  wa_sort_alv-subtot = c_x. "Se o campo vai ter subtotal.
  APPEND wa_sort_alv TO it_sort_alv. "Preenche a tabela de sort.
  CLEAR wa_sort_alv. "Limpa a Work Area.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_MOUNT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Modelo FieldCat.
*----------------------------------------------------------------------*
FORM zf_mount_alv .

  PERFORM zf_fill_alv USING:
  c_mark       ''          ''          ''         ''  ''  c_x c_x '' '',
  c_vbeln      c_tab_saida c_vbeln     c_tab_vbak c_x ''  '' ''   '' '',
  c_erdat      c_tab_saida c_erdat     c_tab_vbak ''  ''  '' ''   '' '',
  c_posnr      c_tab_saida c_posnr     c_tab_vbap ''  ''  '' ''   '' '',
  c_kun_nam    c_tab_saida ''          ''         ''  ''  '' ''   '' 'Emissor da ordem',
  c_netwr      c_tab_saida c_netwr     c_tab_vbak ''  c_x '' ''   '' '',
  c_mat_mak    c_tab_saida ''          ''         ''  ''  '' ''   '' 'Nº do material',
  c_gsb_gtx    c_tab_saida ''          ''         ''  ''  '' ''   '' 'Divisão',
  c_vbeln_lip  c_tab_saida c_vbeln_lip c_tab_lips ''  ''  '' ''   '' 'Fornecimento',
  c_status     c_tab_saida ''          ''         ''  ''  '' ''   '' 'Status'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_FILL_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->   Preenchendo FieldCat / Declarando os campos que serão impresso no ALV
*----------------------------------------------------------------------*
FORM zf_fill_alv  USING field        TYPE any
                        tab          TYPE any
                        ref_field    TYPE any
                        ref_tab      TYPE any
                        hostpot      TYPE any
                        sum          TYPE any
                        edit        TYPE any
                        no_out TYPE any
                        reptext_ddic TYPE any
                        seltext_l    TYPE any.

  CLEAR wa_fieldcat_alv.

  wa_fieldcat_alv-fieldname     = field. "Nome da Coluno
  wa_fieldcat_alv-tabname       = tab. "Nome da Tabela.
  wa_fieldcat_alv-ref_fieldname = ref_field. "Nome da Coluna de referencia.
  wa_fieldcat_alv-ref_tabname   = ref_tab. "Nome da tabela de referencia.
  wa_fieldcat_alv-hotspot       = hostpot. "Se a coluna tem hotspot.
  wa_fieldcat_alv-do_sum        = sum. "Se a coluna do ALV tem Sub Total.
  wa_fieldcat_alv-edit          = edit. " Se a coluna é editavel.
  wa_fieldcat_alv-no_out        = no_out. "Se a coluna não tem saída.
  wa_fieldcat_alv-reptext_ddic  = reptext_ddic. "Label da coluna do relatório.
  wa_fieldcat_alv-seltext_l     = seltext_l.

  APPEND wa_fieldcat_alv TO it_fieldcat_alv. "Preenche a tabela do fieldcat.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_LAYOUT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Preenchendo Layout
*----------------------------------------------------------------------*
FORM zf_layout_alv .

  CLEAR wa_layout_alv. "Limpa a work area de layout do alv.

  wa_layout_alv-zebra = c_x. "Faz o relátorio ser exibido zebrada.
  wa_layout_alv-colwidth_optimize = c_x. "Faz as colunas serem otimizadas pelas labels/detalhes.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_display_alv .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_pf_status_set = 'SUB_PF_STATUS'
      i_callback_top_of_page   = 'ZF_TOP_OF_PAGE'
      i_callback_program       = sy-repid
      i_grid_title             = 'Relatório de Ordens de Venda em Moeda EURO'
      it_sort                  = it_sort_alv
      is_layout                = wa_layout_alv "Aqui passa o Layout do ALV
      it_fieldcat              = it_fieldcat_alv "Aqui é passado a estrutura do ALV
    TABLES
      t_outtab                 = it_saida "Aqui passa os valores a serem exibidos no ALV
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.
    MESSAGE: text-e02 TYPE c_sucess DISPLAY LIKE c_error.

    LEAVE LIST-PROCESSING.
  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  sub_pf_status
*&---------------------------------------------------------------------*
* Chama o status gui do relátorio ALV.
*----------------------------------------------------------------------*
FORM sub_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'ZEXPORT'.
ENDFORM.

FORM user_command USING r_ucomm     LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.

  CASE r_ucomm.

    WHEN '&IC1'. "Verifica se o valor da r_ucomm está com o valor '&IC1' por default.
      CASE rs_selfield-fieldname.
        WHEN c_vbeln. "Verifica qual o nome da coluna selecionada.

          SET PARAMETER ID 'AUN' FIELD rs_selfield-value. "Seta o valor do parâmetro ao chamar a trasação.

          CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN. "Chama a transação e pula a primeira tela.

        WHEN c_vbeln_lip.
          SET PARAMETER ID 'VL' FIELD rs_selfield-value."Seta o valor do parâmetro ao chamar a trasação.

          CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN."Chama a transação e pula a primeira tela.

        WHEN OTHERS.
          " Nenhuma ação específica necessária para outros casos
      ENDCASE.

    WHEN '&TXT'. "Verifica se o botão de exportar txt foi selecionado.
      PERFORM zf_export_txt. "Chama o perform de exportar txt.

    WHEN '&CSV'. "Verifica se o botão de ecportar csv foi selecionado.
      PERFORM zf_export_csv. "Chama o perform de exportar csv.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_ROWS
*&---------------------------------------------------------------------*
* Verifica as linhas do relátorio alv selecionadas e as guarda em uma tabela de saída auxiliar
*----------------------------------------------------------------------*
FORM zf_select_rows.
  DATA: e_grid           TYPE REF TO cl_gui_alv_grid.
  DATA: it_selected_rows TYPE lvc_t_row.
  DATA: lv_indice        TYPE n.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = e_grid.

  CALL METHOD e_grid->check_changed_data. "Este método é geralmente chamado em eventos de interação com o usuário, como ao salvar ou validar os dados inseridos.

  CALL METHOD e_grid->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows. "é um método essencial quando você precisa interagir com linhas selecionadas em um grid ALV.

  FREE it_saida_aux. "Limpa a tabela interna de saída.

  LOOP AT it_selected_rows ASSIGNING FIELD-SYMBOL(<ls_selected>).
    lv_indice = <ls_selected>-index. "Realiza um Loop na tela de indices e à assina com fild symbol.

    READ TABLE it_saida INDEX lv_indice ASSIGNING FIELD-SYMBOL(<ls_saida>). "Lê a tabela interna com base no indice a assina com field symbol.

    IF sy-subrc = 0. "Verifica se não ocorreu erros.
      APPEND <ls_saida> TO it_saida_aux. "Preenche a tabela interna de saída auxiliar.
    ENDIF.

  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_EXPORT_TXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Exportas as linhas atuais para um arquivo txt.
*----------------------------------------------------------------------*
FORM zf_export_txt .
  DATA: lv_filename TYPE string.
  DATA: lv_path TYPE string.
  DATA: lv_fullpath TYPE string.

  PERFORM zf_select_rows. "Chama o perform de verificar as linhas selecionadas.

  IF it_saida_aux IS INITIAL. "Verifica se tabela interna de saída auxiliar está vazia.
    MESSAGE: text-i01 TYPE c_info DISPLAY LIKE c_error. "Messagem de que é nessário selecionar ao menos uma linha para realiazar a exportação
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

    IF lv_fullpath IS INITIAL. "Verifica se o valor não é vazio.
      CLEAR lv_filename. "Limpa as variaveis.
      CLEAR lv_path. "Limpa as variaveis.
      CLEAR lv_fullpath. "Limpa as variaveis.
    ENDIF.

    PERFORM zf_download_file TABLES it_saida_aux USING lv_fullpath. "Chama o perform de download passando a tabela e a variavel de saida do arquivo.
  ENDIF.


ENDFORM.

FORM zf_export_csv.
  DATA: lv_filename TYPE string.
  DATA: lv_path TYPE string.
  DATA: lv_fullpath TYPE string.
  DATA: lv_temp_netwr TYPE string.

  PERFORM zf_select_rows. "Verifica as linhas selecionadas.

  IF it_saida_aux IS INITIAL. "Verifica se a tabela interna está vazia.
    MESSAGE: text-i01 TYPE c_info DISPLAY LIKE c_error. "Mensagem informando que é preciso de ao menos um linha selecionada para exportar o arquivo.
  ELSE.

    CALL METHOD cl_gui_frontend_services=>file_save_dialog "Método que chama a tela de seleção onde salvar o arquivo.
      EXPORTING
        window_title      = 'Save' "Nome da janela.
        file_filter       = 'EXCEL FILES (*.csv)|*.csv|' "Filtro por tipo de arquivo.
        initial_directory = 'C:\' "Pasta inicial.
      CHANGING
        filename          = lv_filename "Saída do nome do arquivo.
        path              = lv_path "Saída do caminho onde o arquivo será armazenado.
        fullpath          = lv_fullpath. "Saída caminho total que é o caminho mais o nome do arquivo.

    IF lv_fullpath IS INITIAL. "Verifica se a variavel está vazia.
      CLEAR lv_filename. "Limpa a variável.
      CLEAR lv_path. "Limpa a variável.
      CLEAR lv_fullpath. "Limpa a variável.
    ENDIF.

    LOOP AT it_saida_aux INTO wa_saida_aux. "Loop na tabela de saída auxiliar e passa os valores para uma work area.

      lv_temp_netwr = wa_saida_aux-netwr. "Passa o valor do campo netwr para a variável temp.

      CONCATENATE wa_saida_aux-vbeln
                  wa_saida_aux-erdat
                  wa_saida_aux-posnr
                  wa_saida_aux-kun_nam
                  wa_saida_aux-mat_mak
                  lv_temp_netwr
                  wa_saida_aux-gsb_gtx
                  wa_saida_aux-vbeln_lip
                  INTO wa_csv SEPARATED BY ';'. "Concatena os valores os passando para uma work area e separa os valores por ';'.

      APPEND wa_csv TO it_csv. "Preenche a tabela interna com os valores da work area.

    ENDLOOP.

    PERFORM zf_download_file TABLES it_csv USING lv_fullpath. "Chama o perform de download passando a tabela e a variavel de saida do arquivo.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_CSV  text
*      -->P_LV_FULLPATH  text
*----------------------------------------------------------------------*
FORM zf_download_file  TABLES   p_it
                                  "Introduzir nome correto para <...>
                       USING    p_lv_fullpath TYPE any.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = p_lv_fullpath "Nome completo do arquivo que é caminho + nome_arquivo + extensão arquivo.
      write_field_separator   = ' '
      filetype                = 'ASC'
    TABLES
      data_tab                = p_it "Tabela de saída.
*     FIELDNAMES              =
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
* Implement suitable error handling here
  ENDIF.

ENDFORM.