*&---------------------------------------------------------------------------------------*
*& REPORT zpr_exec_21_bhs.
*&---------------------------------------------------------------------------------------*
*& Nome:zpr_exec_21_bhs
*& Tipo: Report
*& Objetivo:  Elaborar um programa ABAP onde deverão ser selecionados na tabela de
*& clientes (KNA1) os clientes que possuírem o campo NOME (KNA1-NAME1) iniciados por
*& VOGAIS.
*& Imprimir os campos Nº cliente 1, Nome 1, Rua e nº, Local e Data de criação do
*& registro e, a quantidade de registros selecionados e exibidos no relatório.
*& Gravar em uma NOVA tabela transparente (tabela do banco de dados) os registros
*& selecionados e os campos exibidos no relatório.
*&---------------------------------------------------------------------------------------*

REPORT zpr_exec_21_bhs.

*&-------------------------*
*& Tables
*&-------------------------*
TABLES: kna1,
        ztb_alv_21.

*&-------------------------*
*& Work Area
*&-------------------------*
DATA: wa_clientes TYPE ztb_alv_21.

*&-------------------------*
*& Internal Tables
*&-------------------------*
DATA: it_clientes TYPE TABLE OF ztb_alv_21.
DATA: it_clientes_aux TYPE TABLE OF ztb_alv_21.

*&-------------------------*
*& Performs
*&-------------------------*
PERFORM zf_select_data.
PERFORM zf_display_alv.

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  Faz a seleção dos dados a serem exibidos no ALV.
*----------------------------------------------------------------------*
FORM zf_select_data.

  SELECT mandt "Faz o select do campos passados e guarda na tabela interna.
         kunnr
         name1
         stras
         erdat
         ort01
    FROM kna1
    INTO TABLE it_clientes
    WHERE "Faz a seleção dos dados guardados na tabela interna.
    name1 LIKE 'A%' OR
    name1 LIKE 'E%' OR
    name1 LIKE 'I%' OR
    name1 LIKE 'O%' OR
    name1 LIKE 'U%'
    ORDER BY kunnr.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_DISPLAY_AVL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
* Faz a exibição dos dados Guardados no ALV.
*----------------------------------------------------------------------*
FORM zf_display_alv.
  DATA: lw_fieldcat TYPE slis_fieldcat_alv. "Passará a formatação das colunas
  DATA: lw_layout   TYPE slis_layout_alv. "Pode passar configurações de layout da tabela
  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv. "Guardará a formatação das colunas

  CLEAR lw_fieldcat.
  lw_fieldcat-fieldname = 'mark'.
  lw_fieldcat-edit = abap_on.
  lw_fieldcat-no_out = abap_true.
  APPEND lw_fieldcat TO lt_fieldcat.
  CLEAR lw_fieldcat.

  lw_fieldcat-fieldname = 'kunnr'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Código Cliente'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'name1'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Nome Cliente'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'ort01'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Local'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'stras'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Rua e nº'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_fieldcat-fieldname = 'erdat'. "Nome do campo a ter o nome mudado na exibição do ALV.
  lw_fieldcat-seltext_l = 'Data Criação Cliente'. "Nome que vai aparecer na exibição do relatorio ALV.
  APPEND lw_fieldcat TO lt_fieldcat. "Adiciona a linha a uma tabela interna.
  CLEAR lw_fieldcat. "limpa o conteúdo da workarea.

  lw_layout-colwidth_optimize = abap_true.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_pf_status_set = 'SUB_PF_STATUS'
      i_grid_title             = 'Clientes'
      is_layout                = lw_layout "Aqui passa o Layout do ALV
      it_fieldcat              = lt_fieldcat "Aqui é passado a estrutura do ALV
    TABLES
      t_outtab                 = it_clientes. "Aqui passa os valores a serem exibidos no ALV

  IF sy-subrc NE 0.
    MESSAGE s208(00) DISPLAY LIKE sy-abcde+4(1) WITH text-e02.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form ZF_SELECT_DATA_MATERIAL
*&---------------------------------------------------------------------*
*& Seleciona os dados a serem exibidos pelo a ALV.
*&---------------------------------------------------------------------*
FORM zf_export.

  DATA: e_grid TYPE REF TO cl_gui_alv_grid.
  DATA: t_selected_rows TYPE lvc_t_row.
  DATA: lr_kunnr TYPE RANGE OF kna1-kunnr.
  DATA: lw_kunnr LIKE LINE OF lr_kunnr .
  DATA: lv_indice(10) TYPE n.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = e_grid.

  CALL METHOD e_grid->check_changed_data.
  CALL METHOD e_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_selected_rows.

  LOOP AT t_selected_rows ASSIGNING FIELD-SYMBOL(<ls_selected>). "Realiza um loop com base na quantidade de linhas selecionadas.
    lv_indice = <ls_selected>-index.
    READ TABLE it_clientes INDEX lv_indice ASSIGNING FIELD-SYMBOL(<ls_clientes>).
    lw_kunnr-sign = 'I'.
    lw_kunnr-option = 'EQ'.
    lw_kunnr-low = <ls_clientes>-kunnr.

    APPEND <ls_clientes> TO it_clientes_aux. "Passa os valores da work area para a tabela interna axíciliar.

  ENDLOOP.

  MODIFY ztb_alv_21 FROM TABLE it_clientes_aux. "Passa os valores da tabela interna para a tabela transparente.
  COMMIT WORK."Executa a alteração no Banco de Dados.

  IF sy-subrc = 0. "Caso o registro ocorra com sucesso mostra mensagem.
    MESSAGE 'Registros Salvos com SUCESSO!' TYPE 'S'. "Exbibe a mensagem de sucesso.
  ENDIF.
ENDFORM.

*& Form para chamar o SAP GUI referente ao ALV.
FORM sub_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN'.
ENDFORM.

*& Form que verifica qual botão do SAP GUI foi pressionada para fazer.
FORM user_command USING r_ucomm LIKE sy-ucomm rs_selfield TYPE slis_selfield.
  IF r_ucomm EQ '&EXPORTA'.
    PERFORM zf_export.
  ENDIF.
ENDFORM.