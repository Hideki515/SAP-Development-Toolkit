*&---------------------------------------------------------------------------------------*
*& REPORT zpr_exec_29_bhs.
*&---------------------------------------------------------------------------------------*
*& Código para localfile por MÉTODO
*&---------------------------------------------------------------------------------------*
REPORT zr_bhs_localfile_2.

*&---------------------------------------------------------------------*
*&  DATA
*&---------------------------------------------------------------------*
DATA: v_filename TYPE string.
DATA: v_path     TYPE string.
DATA: v_fullpath TYPE string.

*&---------------------------------------------------------------------*
*&  METHOD
*&---------------------------------------------------------------------*
CALL METHOD cl_gui_frontend_services=>file_save_dialog "Método que chama a tela de seleção onde salvar o arquivo.
  EXPORTING
    window_title      = 'Save' "Nome da janela.
    file_filter       = 'TEXT FILES (*.TXT)|*.TXT|' "Filtro por tipo de arquivo.
    initial_directory = 'C:\' "Pasta inicial.
  CHANGING
    filename          = v_filename "Saída do nome do arquivo.
    path              = v_path "Saída do caminho onde o arquivo será armazenado.
    fullpath          = v_fullpath. "Saída caminho total que é o caminho mais o nome do arquivo.