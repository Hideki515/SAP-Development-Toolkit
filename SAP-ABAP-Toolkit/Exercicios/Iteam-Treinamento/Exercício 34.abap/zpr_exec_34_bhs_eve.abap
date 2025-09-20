*&---------------------------------------------------------------------*
*&  Include           ZPR_EXEC_34_BHS_EVE
*&---------------------------------------------------------------------*

INITIALIZATION.
  CLEAR p_loc.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_loc. "Cria o quadradinho no lado do campo para a seleção do local do arquivo.
  PERFORM zf_select_file CHANGING p_loc. "Chama o form de selecionar o arquivo.

START-OF-SELECTION.
  PERFORM zf_upload_file USING p_loc. "Chama o form de upload do arquivo.

END-OF-SELECTION.
  PERFORM zf_process_file.                  "Chama o form de processamento do arquivo.
  PERFORM zf_layout_alv.                    "Chama o form de layout do ALV.
  PERFORM zf_display_alv.                   "Chama o form de exibir o ALV na tela.