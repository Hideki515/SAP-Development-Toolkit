*&---------------------------------------------------------------------*
*& Report ZPR_BATCH_INPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpr_batch_input.

*&---------------------------------------------------------------------*
*& TYPE-POOLS
*&---------------------------------------------------------------------*
TYPE-POOLS:
  slis. " Tipos globais para módulos de lista genéricos

*&---------------------------------------------------------------------*
*& TYPES
*&---------------------------------------------------------------------*
TYPES:
  BEGIN OF ty_csv,
    campo(500) TYPE c,
  END OF ty_csv,

  BEGIN OF ty_lfa1,
    lifnr      TYPE lfa1-lifnr,
    name1      TYPE lfa1-name1,
    stras      TYPE lfa1-stras,
    ort01      TYPE lfa1-ort01,
    pfach      TYPE lfa1-pfach,
    ort02      TYPE lfa1-ort02,
    regio      TYPE lfa1-regio,
    status     TYPE char1,
    observacao TYPE char100,
  END OF ty_lfa1.

*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*
DATA:
  wa_bdcdata  TYPE bdcdata,
  wa_lfa1     TYPE ty_lfa1,
  wa_msg      TYPE bdcmsgcoll,
  wa_fieldcat TYPE slis_fieldcat_alv.

*&---------------------------------------------------------------------*
*& INTERNAL TABLE
*&---------------------------------------------------------------------*
DATA:
  ti_bdcdata  TYPE TABLE OF bdcdata,
  ti_csv      TYPE TABLE OF ty_csv,
  ti_lfa1     TYPE TABLE OF ty_lfa1,
  ti_msg      TYPE TABLE OF bdcmsgcoll,
  ti_fieldcat TYPE TABLE OF slis_fieldcat_alv.

*&---------------------------------------------------------------------*
*& VARIABLES
*&---------------------------------------------------------------------*
DATA:
  gv_nome_arquivo TYPE string,
  gv_tamanho      TYPE i,
  gv_erro         TYPE c.


*&---------------------------------------------------------------------*
*& CONSTANTS
*&---------------------------------------------------------------------*
CONSTANTS:
  c_window_title_up TYPE string VALUE 'Selecione um arquivo', " Título da Janela de seleçõao do arquivo
  c_separador_csv   TYPE c      VALUE ';'                   , " Separador do CSV
  c_mode            TYPE c      VALUE 'N'                   , " Modo de execução do batch input
  c_e               TYPE c      VALUE 'E'                   , " Erro
  c_s               TYPE c      VALUE 'S'                   , " Sucesso
  c_csv_mai(3)      TYPE c      VALUE 'CSV'                 , " Fim CSV maiusculo
  c_csv_min(3)      TYPE c      VALUE 'csv'                 . " Fim CSV minusculo.

*&---------------------------------------------------------------------*
*& SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN  BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.

  PARAMETERS: p_arq TYPE rlgrap-filename OBLIGATORY. " Caminho arquivo

SELECTION-SCREEN END OF BLOCK b1.

*&---------------------------------------------------------------------*
*& EVENTO AT SELECTION-SCREEN
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_arq. "Cria o matchcode no lado do campo para a sele��o do local do arquivo.
  PERFORM zf_select_arquivo CHANGING p_arq.

AT SELECTION-SCREEN.
  PERFORM zf_valida_arquivo USING p_arq.  " Caminho do arquivo

*&---------------------------------------------------------------------*
*& EVENTO START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  CHECK gv_erro IS INITIAL. " Verifica se a variavel de erro est� vazia.
  PERFORM zf_upload_arquivo USING p_arq.


*&---------------------------------------------------------------------*
*& EVENTO END-OF-SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.

  CHECK gv_erro IS INITIAL.
  PERFORM zf_processamento_arquivo.
  PERFORM zf_processamento_dados.
  PERFORM zf_monta_fieldcat.
  PERFORM zf_exibe_alv.

*&---------------------------------------------------------------------*
*& Form zf_select_arquivo
*&---------------------------------------------------------------------*
*& Sele��o do arquivo
*&---------------------------------------------------------------------*
FORM zf_select_arquivo CHANGING p_arq.

  DATA:
    lt_filetable TYPE filetable. "Variavel local do tipo filetable.

  DATA:
    lv_rc        TYPE i. "Variavel local do tipo inteiro.

  "Chama o metodo de janela de sele��o de arquivo.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title = c_window_title_up "Nome da janela
    CHANGING
      file_table   = lt_filetable "Guarda a localização do arquivo.
      rc           = lv_rc
    EXCEPTIONS
      OTHERS       = 1.

  IF sy-subrc = 0. "Verifica sen�o ocorreu erro ao seleciona o arquivo.

    READ TABLE lt_filetable INDEX 1 INTO DATA(ls_file). " Busca o primeiro registro encontrado e passa oa valor para uma work area
    IF sy-subrc EQ 0.

      p_arq = ls_file-filename. " Preenche o parametro com o caminho do arquivo.

    ENDIF.


  ELSE.

    MESSAGE TEXT-e04 TYPE c_e. " Nenhum arquivo selecionado ou ocorreu um erro.

  ENDIF.

ENDFORM.  " zf_select_arquivo

*&---------------------------------------------------------------------*
*&      Form  ZF_VALIDA_ARQUIVO
*&---------------------------------------------------------------------*
*       Valida arquivo escolhido de acordo com o radiobuttom selecionado
*----------------------------------------------------------------------*
FORM zf_valida_arquivo  USING p_arq TYPE rlgrap-filename.

* Se o caminho e o arquivo foram escolhidos
  IF NOT p_arq IS INITIAL.

*   Procura o tamanho total do caminho  + nome
    CLEAR gv_tamanho.
    gv_tamanho = strlen( p_arq ).
    SUBTRACT 3 FROM gv_tamanho.

*   Verifica se � um arquivo .CSV
    IF p_arq+gv_tamanho(3) NE c_csv_mai AND
       p_arq+gv_tamanho(3) NE c_csv_min.

      gv_erro = abap_true.

      MESSAGE s208(00) WITH TEXT-e03 DISPLAY LIKE c_e. "Arquivo inválido!
      LEAVE LIST-PROCESSING. " Volta para a tela de seleção

    ENDIF.

  ENDIF.

ENDFORM.  " ZF_VALIDA_ARQUIVO

*&---------------------------------------------------------------------*
*& Form zf_upload_arquivo
*&---------------------------------------------------------------------*
*& Upload do arquivo no programa
*&---------------------------------------------------------------------*
FORM zf_upload_arquivo USING p_filename.

  DATA(lv_filename) = CONV string( p_filename ). " Declaraçãoo de uma variável local que recebe a conversão do valor do parametro para string.

  "Chama a função que faz o upload do arquivo.
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename = lv_filename "Local do arquivo com nome do arquivo.
    TABLES
      data_tab = ti_csv "Tabela onde os dados dos arquivos ser�o armazenados
    EXCEPTIONS
      OTHERS   = 1.

  IF sy-subrc NE 0.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDIF.

ENDFORM.  " ZF_UPLOAD_ARQUIVO

*&---------------------------------------------------------------------*
*& Form zf_processamento_arquivo
*&---------------------------------------------------------------------*
*& Prossamento dos dados do arquivo para utiliza��o no programa.
*&---------------------------------------------------------------------*
FORM zf_processamento_arquivo .

  LOOP AT ti_csv INTO DATA(lw_csv).

    CLEAR wa_lfa1.

    SPLIT lw_csv-campo AT c_separador_csv INTO wa_lfa1-name1
                                               wa_lfa1-stras
                                               wa_lfa1-ort01
                                               wa_lfa1-pfach
                                               wa_lfa1-ort02
                                               wa_lfa1-regio. " Separa os campos da linha do csv e os guarda na varíavel dos respectivos camppos da work area.

    APPEND wa_lfa1 TO ti_lfa1. " Apenda os valores da work area para a tabela interna.

    CLEAR lw_csv. " Limpa a work area.

  ENDLOOP.

ENDFORM.  " zf_processamento_arquivo

*&---------------------------------------------------------------------*
*& Form zf_processamento_dados
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_processamento_dados .

  DATA:
    lv_ultimo_numero  TYPE i, " ùltimo número
    lv_completa_zeros TYPE char4, " Valor sequencial com o tipo de dados char4 para o unpack realizar o preenchimento dos zeros a esquerda.
    lv_sequencial_str TYPE string. " Guarda o valor sequencial.

  " Buscar o último valor no banco
  SELECT lifnr
    FROM lfa1
    INTO TABLE @DATA(lt_last_lifnr)
    UP TO 1 ROWS
    WHERE lifnr LIKE 'BHS-%'
    ORDER BY lifnr DESCENDING.

  IF sy-subrc EQ 0.

    READ TABLE lt_last_lifnr ASSIGNING FIELD-SYMBOL(<ls_last_lifnr>) INDEX 1. " BUsca o registro econtrado na tabela e o assina com field-symbol.
    IF sy-subrc EQ 0.

      SPLIT <ls_last_lifnr>-lifnr AT '-' INTO DATA(lv_inicial)
                                                  lv_sequencial_str. " Separa o LIFNR em prefixo e valor sequencial.

      lv_ultimo_numero = lv_sequencial_str. " Passa o valor sequencial para um variavel númerica.

    ENDIF.

  ELSE.

    lv_inicial = `BHS`.

    " Caso nâo for encontrados dados é preenchido o com 1 para ser o valor inicial
    lv_ultimo_numero = 1.

  ENDIF.

  LOOP AT ti_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1>).

    " Incrementa contador
    ADD 1 TO lv_ultimo_numero .

    " Preenche os zeros a esquerda
    UNPACK lv_ultimo_numero TO lv_completa_zeros.

    " Monta novo LIFNR
    DATA(lv_novo_lifnr) = |{ lv_inicial }-{ lv_completa_zeros }|.

    <fs_lfa1>-lifnr = lv_novo_lifnr.

    PERFORM zf_monta_shdb USING <fs_lfa1>.

    PERFORM zf_executa_shdb CHANGING <fs_lfa1>-status
                                     <fs_lfa1>-observacao.

  ENDLOOP.

  UNASSIGN <fs_lfa1>.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zf_preenche_bdcdata
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LW_LFA1
*&---------------------------------------------------------------------*
FORM zf_monta_shdb  USING p_lfa1 TYPE ty_lfa1.

  FREE: ti_bdcdata.

  PERFORM zf_preenche_bdc USING: "Chama o form que vai preencher a tabela BDC passando os valores a serem utilizados.
                                'X'    'SAPMF02K'    '100',
                                ' '    'BDC_CURSOR'  'RF02K-KTOKK',
                                ' '    'BDC_OKCODE'  '/00',
                                ' '    'RF02K-LIFNR' p_lfa1-lifnr,
                                ' '    'RF02K-EKORG' '7000',
                                ' '    'RF02K-KTOKK' 'BR01'.

  PERFORM zf_preenche_bdc USING: "Chama o form que vai preencher a tabela BDC passando os valores a serem utilizados.
                                'X'    'SAPMF02K'   '110',
                                ' '    'BDC_CURSOR' 'LFA1-PSTLZ',
                                ' '    'BDC_OKCODE' '=VW',
                                ' '    'LFA1-NAME1' p_lfa1-name1,
                                ' '    'LFA1-STRAS' p_lfa1-stras,
                                ' '    'LFA1-ORT01' p_lfa1-ort01,
                                ' '    'LFA1-PSTLZ' p_lfa1-pfach,
                                ' '    'LFA1-ORT02' p_lfa1-ort02,
                                ' '    'LFA1-LAND1' 'BR',
                                ' '    'LFA1-REGIO' p_lfa1-regio.

  PERFORM zf_preenche_bdc USING: "Chama o form que vai preencher a tabela BDC passando os valores a serem utilizados.
                                'X'    'SAPMF02K'   '120',
                                ' '    'BDC_CURSOR' 'LFA1-KUNNR',
                                ' '    'BDC_OKCODE' '=VW',
                                ' '    'LFA1-TXJCD' 'SP'.

  PERFORM zf_preenche_bdc USING: "Chama o form que vai preencher a tabela BDC passando os valores a serem utilizados.
                                'X'    'SAPMF02K'   '130',
                                ' '    'BDC_CURSOR' 'LFBK-BANKS(01)',
                                ' '    'BDC_OKCODE' '=VW'.

  PERFORM zf_preenche_bdc USING: "Chama o form que vai preencher a tabela BDC passando os valores a serem utilizados.
                                'X'    'SAPMF02K'   '380',
                                ' '    'BDC_CURSOR' 'KNVK-NAMEV(01)',
                                ' '    'BDC_OKCODE' '=VW'.

  PERFORM zf_preenche_bdc USING: "Chama o form que vai preencher a tabela BDC passando os valores a serem utilizados.
                                'X'    'SAPMF02K'   '310',
                                ' '    'BDC_CURSOR' 'LFM1-ZTERM',
                                ' '    'BDC_OKCODE' '=UPDA',
                                ' '    'LFM1-ZTERM' '0001'.

ENDFORM.  " ZF_MONTA_SHDB

*&---------------------------------------------------------------------*
*&      Form  ZF_PREENCHE_BDC
*&---------------------------------------------------------------------*
*& -> Preenche a tabela BDC conforme os dados passados nos performs.
*&---------------------------------------------------------------------*
FORM zf_preenche_bdc USING p_dynbegin
                           p_name
                           p_value.

  wa_bdcdata-dynbegin = p_dynbegin. "Passa o valor passado no p_dynbegin para a work area.
  wa_bdcdata-program = p_name. "Passa o valor em Hard Code para a work area.

  IF p_dynbegin EQ abap_true. "Verifica se o p_dynbegin está marcada.

    wa_bdcdata-dynpro = p_value. "Passa o valor para o campo da work area.

  ELSE. "Sen�o

    wa_bdcdata-fnam = p_name. "Passa o valor para o campo da work area.
    WRITE p_value TO wa_bdcdata-fval. "Passa o valor para o campo da work area.
    CONDENSE wa_bdcdata-fval NO-GAPS.

  ENDIF. "Fim if p_dynbegin.

  APPEND wa_bdcdata TO ti_bdcdata. "Preenche a tabela interna com os dados guardados na work area.

  CLEAR wa_bdcdata.

ENDFORM.  " ZF_PREENCHE_BDC

*&---------------------------------------------------------------------*
*& Form zf_executa_shdb
*&---------------------------------------------------------------------*
*& Executa o SHBD com conforme o mapeamento
*&---------------------------------------------------------------------*
FORM zf_executa_shdb CHANGING p_status     TYPE char1
                              p_observacao TYPE char100.

  DATA:
    l_msg_id        LIKE t100-arbgb, " Id da msg
    l_msg_no        LIKE t100-msgnr, " N�mero da msg
    l_msg_var1      LIKE balm-msgv1, " Mensagem 1
    l_msg_var2      LIKE balm-msgv2, " Mensagem 2
    l_msg_var3      LIKE balm-msgv3, " Mensagem 3
    l_msg_var4      LIKE balm-msgv4, " Mensagem 4
    l_msg_text(100) TYPE c         . " Mensagem final

  FREE: ti_msg.

  DATA(l_mode) = c_mode. " Modo call transaction

  CALL TRANSACTION 'XK01' USING ti_bdcdata
                          MODE  l_mode
                          MESSAGES INTO ti_msg. "Chama a transa��o passando os dados armazenados na tabela interna em modo background guardando as messagens em uma tabela interna.

* SE OCORRERAM ERROS
  CLEAR wa_msg.

  READ TABLE ti_msg INTO DATA(lw_msg_e) WITH KEY msgtyp = c_e.
  IF sy-subrc EQ 0.

*   Retorna status
    p_status = c_e.
*   Preenche variáveis auxiliares para evitar dump por
*   conflito de tipos de campo na função.
    l_msg_id   = lw_msg_e-msgid.
    l_msg_no   = lw_msg_e-msgnr.
    l_msg_var1 = lw_msg_e-msgv1.
    l_msg_var2 = lw_msg_e-msgv2.
    l_msg_var3 = lw_msg_e-msgv3.
    l_msg_var4 = lw_msg_e-msgv4.
*   MONTA MSG DE ERRO
    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        msg_id                 = l_msg_id
        msg_no                 = l_msg_no
        msg_var1               = l_msg_var1
        msg_var2               = l_msg_var2
        msg_var3               = l_msg_var3
        msg_var4               = l_msg_var4
      IMPORTING
        msg_text               = l_msg_text
      EXCEPTIONS
        function_not_completed = 1
        message_not_found      = 2
        OTHERS                 = 3.

    IF sy-subrc EQ 0.
*     Retorna a mensagem para o relatório
      p_observacao = l_msg_text.
    ENDIF.

  ELSE.

*   Verifica sucessos
    CLEAR wa_msg.

    READ TABLE ti_msg INTO DATA(lw_msg_s) WITH KEY msgtyp = c_s
                                                   msgid  = 'F2'
                                                   msgnr  = '173'.
    IF sy-subrc EQ 0.

*     Retorna status
      p_status = c_s.
*     Preenche variáveis auxiliares
      l_msg_id       = lw_msg_s-msgid.
      l_msg_no       = lw_msg_s-msgnr.
      l_msg_var1     = lw_msg_s-msgv1.
      l_msg_var2     = lw_msg_s-msgv2.
      l_msg_var3     = lw_msg_s-msgv3.
      l_msg_var4     = lw_msg_s-msgv4.
*     MONTA MSG DE SUCESSO
      CALL FUNCTION 'MESSAGE_PREPARE'
        EXPORTING
          msg_id                 = l_msg_id
          msg_no                 = l_msg_no
          msg_var1               = l_msg_var1
          msg_var2               = l_msg_var2
          msg_var3               = l_msg_var3
          msg_var4               = l_msg_var4
        IMPORTING
          msg_text               = l_msg_text
        EXCEPTIONS
          function_not_completed = 1
          message_not_found      = 2
          OTHERS                 = 3.

      IF sy-subrc EQ 0.
*       Retorna a mensagem para o relat�rio
        p_observacao = l_msg_text.
      ENDIF.

    ELSE.

*     Verifica sucessos
      CLEAR wa_msg.
      READ TABLE ti_msg INTO wa_msg WITH KEY msgtyp = c_s.
      IF sy-subrc EQ 0.

*       Retorna status
        p_status = c_s.
*       Preenche variáveis auxiliares
        l_msg_id       = wa_msg-msgid.
        l_msg_no       = wa_msg-msgnr.
        l_msg_var1     = wa_msg-msgv1.
        l_msg_var2     = wa_msg-msgv2.
        l_msg_var3     = wa_msg-msgv3.
        l_msg_var4     = wa_msg-msgv4.
*       MONTA MSG DE SUCESSO
        CALL FUNCTION 'MESSAGE_PREPARE'
          EXPORTING
            msg_id                 = l_msg_id
            msg_no                 = l_msg_no
            msg_var1               = l_msg_var1
            msg_var2               = l_msg_var2
            msg_var3               = l_msg_var3
            msg_var4               = l_msg_var4
          IMPORTING
            msg_text               = l_msg_text
          EXCEPTIONS
            function_not_completed = 1
            message_not_found      = 2
            OTHERS                 = 3.

        IF sy-subrc EQ 0.

*         Retorna a mensagem para o relatório
          p_observacao = l_msg_text.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.  " ZF_EXECUTA_SHDB

*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_FIELDCAT
*&---------------------------------------------------------------------*
*       Monta a tabela de Fieldcat.
*----------------------------------------------------------------------*
FORM zf_monta_fieldcat .

  PERFORM zf_preenche_ti_fieldcat USING:
        'LIFNR'      'TI_LFA1' 'LFA1' 'LIFNR' '',
        'NAME1'      'TI_LFA1' 'LFA1' 'NAME1' '',
        'STRAS'      'TI_LFA1' 'LFA1' 'STRAS' '',
        'ORT01'      'TI_LFA1' 'LFA1' 'ORT01' '',
        'PFACH'      'TI_LFA1' 'LFA1' 'PFACH' '',
        'ORT02'      'TI_LFA1' 'LFA1' 'ORT02' '',
        'REGIO'      'TI_LFA1' 'LFA1' 'REGIO' '',
        'STATUS'     'TI_LFA1' ''     ''      'Status',
        'OBSERVACAO' 'TI_LFA1' ''     ''      'Observacao'.

ENDFORM.  " ZF_MONTA_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  ZF_PREENCHE_TI_FIELDCAT
*&---------------------------------------------------------------------*
*       Preenchimento da tabela de Fieldcat.
*----------------------------------------------------------------------*
FORM zf_preenche_ti_fieldcat  USING  p_fieldname
                                     p_tabname
                                     p_reftab
                                     p_reffield
                                     p_reptext.

  CLEAR:
    wa_fieldcat.

  wa_fieldcat-fieldname     = p_fieldname.
  wa_fieldcat-tabname       = p_tabname.
  wa_fieldcat-ref_fieldname = p_reffield.
  wa_fieldcat-ref_tabname   = p_reftab.
  wa_fieldcat-reptext_ddic  = p_reptext.

  APPEND wa_fieldcat TO  ti_fieldcat.

ENDFORM.  " ZF_MONTA_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  ZF_EXIBE_ALV
*&---------------------------------------------------------------------*
*       Exibição do ALV.
*----------------------------------------------------------------------*
FORM zf_exibe_alv .

  DATA(lw_layout) = VALUE slis_layout_alv( colwidth_optimize = abap_true
                                           zebra             = abap_true ).

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = lw_layout
      it_fieldcat        = ti_fieldcat
    TABLES
      t_outtab           = ti_lfa1
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.

    MESSAGE: TEXT-e02 TYPE c_s DISPLAY LIKE c_e. " Erro ao exibir ALV.
    LEAVE TO LIST-PROCESSING.

  ENDIF.

ENDFORM.  " ZF_EXIBE_ALV