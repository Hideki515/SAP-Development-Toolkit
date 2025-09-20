*&---------------------------------------------------------------------*
*&  Include             ZPR_EXEC_31_BHS_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Seleciona os dados conforme os parâmetros.
*----------------------------------------------------------------------*
FORM zf_select_data .

  SELECT vbeln
         fkdat
         kunrg
  FROM vbrk
  INTO TABLE it_vbrk
  WHERE
  vbeln IN s_vbeln AND
  fkart = 'F2'     AND
  fkdat IN s_fkdat OR
  kunrg = p_kunrg.


  IF sy-subrc <> 0.
    MESSAGE: text-e01 TYPE c_sucess DISPLAY LIKE c_error.

    LEAVE LIST-PROCESSING.
  ENDIF.

  IF it_vbrk IS NOT INITIAL.

    SELECT vbeln
           posnr
           matnr
           fkimg
           vrkme
           netwr
           aubel
      FROM vbrp
      INTO TABLE it_vbrp
      FOR ALL ENTRIES IN it_vbrk
      WHERE
      vbeln = it_vbrk-vbeln.


    IF sy-subrc <> 0.
      MESSAGE: text-e01 TYPE c_sucess DISPLAY LIKE c_error.

      LEAVE LIST-PROCESSING.
    ENDIF.

  ENDIF.

  IF it_vbrp IS NOT INITIAL.

    SELECT vbeln
      FROM vbak
      INTO TABLE it_vbak
      FOR ALL ENTRIES IN it_vbrp
      WHERE
      vbeln = it_vbrp-aubel.

    IF sy-subrc <> 0.
      MESSAGE: text-e01 TYPE c_sucess DISPLAY LIKE c_error.

      LEAVE LIST-PROCESSING.
    ENDIF.

  ENDIF.

  IF it_vbrk IS NOT INITIAL.

    SELECT kunnr
           name1
      FROM kna1
      INTO TABLE it_kna1
      FOR ALL ENTRIES IN it_vbrk
      WHERE
      kunnr = it_vbrk-kunrg.

    IF sy-subrc <> 0.
      MESSAGE: text-e01 TYPE c_sucess DISPLAY LIKE c_error.

      LEAVE LIST-PROCESSING.
    ENDIF.

  ENDIF.

  IF it_vbrp IS NOT INITIAL.

    SELECT matnr
           maktx
      FROM makt
      INTO TABLE it_makt
      FOR ALL ENTRIES IN it_vbrp
      WHERE
      matnr = it_vbrp-matnr AND
      spras = sy-langu.

    IF sy-subrc <> 0.
      MESSAGE: text-e01 TYPE c_sucess DISPLAY LIKE c_error.

      LEAVE LIST-PROCESSING.
    ENDIF.

  ENDIF.

  IF it_vbrp IS NOT INITIAL.

    SELECT vbeln
           posnr
      FROM ztb_exec_3
      INTO TABLE it_found
      FOR ALL ENTRIES IN it_vbrp
      WHERE
      vbeln = it_vbrp-vbeln AND
      posnr = it_vbrp-posnr.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Processa os dados conforme necessário.
*----------------------------------------------------------------------*
FORM zf_process_data.

  SORT it_vbrp  BY vbeln. "Ordena a tabela.
  SORT it_vbrk  BY vbeln. "Ordena a tabela.
  SORT it_vbak  BY vbeln. "Ordena a tabela.
  SORT it_kna1  BY kunnr. "Ordena a tabela.
  SORT it_makt  BY matnr. "Ordena a tabela.
  SORT it_found BY vbeln. "Ordena a tabela.

  LOOP AT it_vbrp INTO wa_vbrp. "Faz um loop na tabela interna passando os valores do laço atual para uma work area.
    READ TABLE it_vbrk INTO wa_vbrk WITH KEY vbeln = wa_vbrp-vbeln BINARY SEARCH. "Lê a tabela e passa os valore encontrados para uma work area.

    IF sy-subrc = 0. "Verifica se não ocorreu erro.
      READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = wa_vbrp-aubel BINARY SEARCH. "Lê a tabela e passa os valore encontrados para uma work area.

      IF sy-subrc <> 0. "Verifica se ocorreu erro.
        CONTINUE. "Pula para a próximo laço do loop.
      ENDIF. "FIM IF sy-subrc <> 0.

      READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbrk-kunrg BINARY SEARCH. "Lê a tabela e passa os valore encontrados para uma work area.

      IF sy-subrc <> 0. "Verifica se ocorreu erro.
        CONTINUE. "Pula para a próximo laço do loop.
      ENDIF. "FIM IF sy-subrc <> 0.

      READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_vbrp-matnr BINARY SEARCH. "Lê a tabela e passa os valore encontrados para uma work area.

      IF sy-subrc <> 0. "Verifica se ocorreu erro.
        CONTINUE. "Pula para a próximo laço do loop.
      ENDIF. "FIM IF sy-subrc <> 0.

      READ TABLE it_found INTO wa_found WITH KEY vbeln = wa_vbrp-vbeln
                                                 posnr = wa_vbrp-posnr BINARY SEARCH. "Lê a tabela e passa os valore encontrados para uma work area.

      " Definir o status do semáforo
      IF wa_found-vbeln = wa_vbrp-vbeln. "Verifica se o campo encontrado é o mesmo que o atual.

        wa_saida-status = '@S_TL_R@'. " Semáforo vermelho: Registro encontrado

      ELSE.

        wa_saida-status = '@S_TL_G@'. " Semáforo verde: Registro não encontrado

      ENDIF.

      IF wa_vbrp-fkimg < 10.

        wa_cellcolor-fname = 'FKIMG'. "Define o nome do campo da cor.
        wa_cellcolor-color-col = 5. "Define o código da cor.

      ELSE."Senão for.
        wa_cellcolor-fname = 'FKIMG'. "Define o nome do campo da cor.
        wa_cellcolor-color-col = 6. "Define o código da cor.

      ENDIF. "FIM wa_vbrp-fkimg < 10.
      APPEND wa_cellcolor TO wa_saida-cellcolor. "Passa o valor da work area para outra work area.

      IF wa_vbrp-netwr  < 1000. "Verica se o valor é menor que 1000

        wa_cellcolor-fname = 'NETWR'. "Define o nome do campo da cor.
        wa_cellcolor-color-col = 5. "Define o código da cor.

      ELSE.  "Senão for.

        wa_cellcolor-fname = 'NETWR'. "Define o nome do campo da cor.
        wa_cellcolor-color-col = 6. "Define o código da cor.

      ENDIF. "FIM IF wa_vbrp-netwr  < 1000.
      APPEND wa_cellcolor TO wa_saida-cellcolor. "Passa o valor da work area para outra work area.

      CONCATENATE wa_vbrk-kunrg wa_kna1-kunnr INTO wa_saida-kun_nam SEPARATED BY '-'. "Concatena os valores os separando por '-' os os guarda em um campo da work area.
      CONCATENATE wa_vbrp-matnr wa_makt-maktx INTO wa_saida-mat_mak SEPARATED BY '-'. "Concatena os valores os separando por '-' os os guarda em um campo da work area.

      wa_saida-vbeln = wa_vbrk-vbeln. "Recebe o valor do parâmetro.
      wa_saida-posnr = wa_vbrp-posnr. "Recebe o valor do parâmetro.
      wa_saida-fkdat = wa_vbrk-fkdat. "Recebe o valor do parâmetro.
      wa_saida-fkimg = wa_vbrp-fkimg. "Recebe o valor do parâmetro.
      wa_saida-vrkme = wa_vbrp-vrkme. "Recebe o valor do parâmetro.
      wa_saida-netwr = wa_vbrp-netwr. "Recebe o valor do parâmetro.
      wa_saida-aubel = wa_vbrp-aubel. "Recebe o valor do parâmetro.

      APPEND wa_saida TO it_saida. "Apenda o valor da work area na tabela interna.

    ENDIF.
  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  ZF_MOUNT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Monta o relátorio ALV.
*----------------------------------------------------------------------*
FORM zf_mount_alv .
  PERFORM zf_fill_alv USING:
        c_mark    c_tab_saida ''       ''            ''  ''  c_x c_x '', "Passa os valore necessários para o ALV.
        'VBELN'   c_tab_saida 'VBELN'   'ZTB_EXEC_3' c_x ''  ''  '' '', "Passa os valore necessários para o ALV.
        'POSNR'   c_tab_saida 'POSNR'   'ZTB_EXEC_3' ''  ''  ''  '' '', "Passa os valore necessários para o ALV.
        'FKDAT'   c_tab_saida 'FKDAT'   'ZTB_EXEC_3' c_x ''  ''  '' '', "Passa os valore necessários para o ALV.
        'KUN_NAM' c_tab_saida 'KUN_NAM' 'ZTB_EXEC_3' ''  ''  ''  '' '', "Passa os valore necessários para o ALV.
        'MAT_MAK' c_tab_saida 'MAT_MAK' 'ZTB_EXEC_3' ''  ''  ''  '' '', "Passa os valore necessários para o ALV.
        'FKIMG'   c_tab_saida 'FKIMG'   'ZTB_EXEC_3' ''  c_x ''  '' '', "Passa os valore necessários para o ALV.
        'VRKME'   c_tab_saida 'VRKME'   'ZTB_EXEC_3' ''  ''  ''  '' '', "Passa os valore necessários para o ALV.
        'NETWR'   c_tab_saida 'NETWR'   'ZTB_EXEC_3' ''  c_x ''  '' '', "Passa os valore necessários para o ALV.
        'AUBEL'   c_tab_saida 'AUBEL'   'ZTB_EXEC_3' c_x ''  ''  '' '', "Passa os valore necessários para o ALV.
        'STATUS'  c_tab_saida ''        ''          ''   ''  ''  '' 'STATUS'. "Passa os valore necessários para o ALV.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_FILL_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      --> Popula o ALV.
*----------------------------------------------------------------------*
FORM zf_fill_alv  USING    p_fieldname    TYPE any
                           p_tabname      TYPE any
                           p_ref_field    TYPE any
                           p_ref_tab      TYPE any
                           p_hotspot      TYPE any
                           p_do_sum       TYPE any
                           p_edit         TYPE any
                           p_no_out       TYPE any
                           p_seltext_1    TYPE any.

  CLEAR wa_fieldcat_alv.
  wa_fieldcat_alv-fieldname     = p_fieldname. "Recebe o valor do parâmetro.
  wa_fieldcat_alv-tabname       = p_tabname.   "Recebe o valor do parâmetro.
  wa_fieldcat_alv-ref_fieldname = p_ref_field. "Recebe o valor do parâmetro.
  wa_fieldcat_alv-ref_tabname   = p_ref_tab.   "Recebe o valor do parâmetro.
  wa_fieldcat_alv-hotspot       = p_hotspot.   "Recebe o valor do parâmetro.
  wa_fieldcat_alv-do_sum        = p_do_sum.    "Recebe o valor do parâmetro.
  wa_fieldcat_alv-edit          = p_edit.      "Recebe o valor do parâmetro.
  wa_fieldcat_alv-no_out        = p_no_out.    "Recebe o valor do parâmetro.
  wa_fieldcat_alv-seltext_l     = p_seltext_1. "Recebe o valor do parâmetro.
  APPEND wa_fieldcat_alv TO it_fieldcat_alv.   "Passa os valores das work areas para tabela interna.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_LAYOUT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Layout do ALV.
*----------------------------------------------------------------------*
FORM zf_layout_alv .

  wa_layout_alv-colwidth_optimize = c_x. "Se o as linhas do relátorio vai ser otimizadas pelo tamanhos dos dados.
  wa_layout_alv-zebra             = c_x. "Se o relátorio vai ser exibido zebrada.
  wa_layout_alv-coltab_fieldname  = 'CELLCOLOR'. "Nome da coluna com as cores do ALV.
  wa_layout_alv-box_fieldname     = 'MARK'. "Nome da coluna de seleção de linhas do ALV.

ENDFORM. "FIM FORM zf_layout_alv .

*&---------------------------------------------------------------------*
*&      Form  ZF_SORT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Ordenação dos campos do relátorio.
*----------------------------------------------------------------------*
FORM zf_sort_alv.

  wa_sort_alv-spos = 1. "Ordem a ser ordenado.
  wa_sort_alv-fieldname = 'KUN_NAM'. "Nome do campo.
  wa_sort_alv-tabname = c_tab_saida. "Nome da tabela.
  wa_sort_alv-up = c_x. "Se é ordenação é em ordem cresente.
  APPEND wa_sort_alv TO it_sort_alv. "Passa o valor da work area para tabela interna.
  CLEAR wa_sort_alv. "Limpa a work area do sort.

  wa_sort_alv-spos = 2. "Ordem a ser ordenado.
  wa_sort_alv-fieldname = 'FKDAT'. "Nome do campo
  wa_sort_alv-tabname = c_tab_saida. "Nome da tabela.
  wa_sort_alv-up = c_x. "Se é ordenação é em ordem cresente.
  wa_sort_alv-subtot = c_x. "Se a coluna vai ter subtotal.
  APPEND wa_sort_alv TO it_sort_alv. "Passa o valor da work area para tabela interna.
  CLEAR wa_sort_alv. "Limpa a work area do sort.

ENDFORM. "FIM FORM zf_sort_alv.

*&---------------------------------------------------------------------*
*&      Form  ZF_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Header do ALV.
*----------------------------------------------------------------------*
FORM zf_top_of_page.

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

ENDFORM. "FIM FORM zf_top_of_page.

*&---------------------------------------------------------------------*
*&      Form  ZF_DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Exibe o relátorio na tela.
*----------------------------------------------------------------------*
FORM zf_display_alv .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_grid_title             = 'Relatório de Faturas por Pagador'
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'SUB_PF_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_top_of_page   = 'ZF_TOP_OF_PAGE'
      is_layout                = wa_layout_alv
      it_fieldcat              = it_fieldcat_alv
      it_sort                  = it_sort_alv
    TABLES
      t_outtab                 = it_saida
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0. "Verifica se ocorreu erro ao gerar o relátorio.
    MESSAGE: text-e02 TYPE c_sucess DISPLAY LIKE c_error. "Exibe a mensagem de erro.

    LEAVE LIST-PROCESSING. "Volta para a tela de seleção de parâmetros.
  ENDIF.

ENDFORM. "FIM FORM zf_display_alv .

*&---------------------------------------------------------------------*
*&      Form  SUB_PF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Chama o status do relátorio.
*----------------------------------------------------------------------*
FORM sub_pf_status USING rt_extab TYPE slis_t_extab.

  SET PF-STATUS 'ZEXPORT'. "Seta o STATUS GUI do relátorio ALV.

ENDFORM. "FIM FORM sub_pf_status USING rt_extab TYPE slis_t_extab.

*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Verifica a ação do ALV.
*----------------------------------------------------------------------*
FORM user_command USING r_ucomm     LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.

  CASE r_ucomm. "Verifica se o botão ou o hostpot foi utilzado.
    WHEN '&IC1'.
      CASE rs_selfield-fieldname. "Case caso rs_selfield-fieldname igual a:
        WHEN c_vbeln. "Verifica se o campo foi clicado.

          SET PARAMETER ID 'AUN' FIELD rs_selfield-value."Passa o valor da linha com hostpot no parametro.
          CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN. "Chama a transação VA03 pulando a primeira tela.

        WHEN c_fkdat. "Verifica se o campo foi clicado.

          SET PARAMETER ID 'VF' FIELD rs_selfield-value. "Passa o valor da linha com hostpot no parametro.
          CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN. "Chama a transação VA03 pulando a primeira tela.

      ENDCASE. "FIM CASE rs_selfield-fieldname.

    WHEN '&GRAVAR'. "Verifica se o botão foi clicado.

      PERFORM zf_export. "Chama o perform de exportar os dados.

      rs_selfield-refresh = 'X'. "Da Refresh no ALV.

  ENDCASE. "FIM CASE r_ucomm.

ENDFORM. "FIM FORM user_command.

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  Exporta as linhas selecionadas para tabela transparente.
*----------------------------------------------------------------------*
FORM zf_export.

  LOOP AT it_saida INTO wa_saida WHERE mark = 'X'. "Faz um loop nas linhas no qual o FLAG está marcado.
    wa_saida_exp-vbeln    = wa_saida-vbeln.
    wa_saida_exp-posnr    = wa_saida-posnr.
    wa_saida_exp-fkdat    = wa_saida-fkdat.
    wa_saida_exp-kun_nam  = wa_saida-kun_nam.
    wa_saida_exp-mat_mak  = wa_saida-mat_mak.
    wa_saida_exp-fkimg    = wa_saida-fkimg.
    wa_saida_exp-vrkme    = wa_saida-vrkme.
    wa_saida_exp-netwr    = wa_saida-netwr.
    wa_saida_exp-aubel    = wa_saida-aubel.

    IF sy-subrc = 0. "Verifica se não ocorreu erro.

      wa_saida-status = '@S_TL_R@'. "Passa valor de semaforo vermelho.
      MODIFY it_saida FROM wa_saida. "MOdifica a linha da tabela interna.

    ENDIF. "FIM IF sy-subrc = 0.

    MODIFY ztb_exec_3 FROM wa_saida_exp.
  ENDLOOP. "FIM LOOP AT it_saida INTO wa_saida WHERE mark = 'X'.

ENDFORM. "FIM FORM zf_export.