***************************************
*** PARAMETROS DE SELEÇÃO DE DADOS  ***
***************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

PARAMETERS: p_dir_lg  TYPE localfile. "Campo que pede o Local onde será salvo.

SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_dir_lg.
  PERFORM zf_local USING p_dir_lg.

*--------------------------------------------------------*
*   Form  Z_LOCAL
*--------------------------------------------------------*
*   SELECIONA O LOCAL DA PASTA DESEJADA
*--------------------------------------------------------*
FORM zf_local  USING p_dir TYPE localfile.

  DATA: l_sel_dir     TYPE string.

  CALL METHOD cl_gui_frontend_services=>directory_browse "Metodo que abre o pop-up de selecionar o diretorio
    EXPORTING
      window_title         = 'Local do arquivo' "Titulo do pop-up para selecionar a pasta
*     initial_folder       = '' "Define a pasta no qual o Pop-up abrira
    CHANGING
      selected_folder      = l_sel_dir
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    p_dir = l_sel_dir."Passa o caminho do diretorio para o parametro
  ENDIF.
ENDFORM.