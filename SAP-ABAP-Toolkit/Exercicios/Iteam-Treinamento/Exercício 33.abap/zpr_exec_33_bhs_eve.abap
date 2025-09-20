*&---------------------------------------------------------------------*
*&  Include           ZPR_EXEC_32_BHS_EVE
*&---------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_loc. "Cria o quadradinho no lado do campo para a seleção do local do arquivo.
  PERFORM zf_select_file CHANGING p_loc. "Chama o form de selecionar o arquivo.

START-OF-SELECTION.
  PERFORM zf_upload_file USING p_loc. "Chama o form de upload do arquivo.
  PERFORM zf_process_file. "Chama o form de processamento do arquivo.
  PERFORM zf_process_bdc. "Chama o form de processamento do BDC.
  PERFORM zf_message. "Chama o perform de mensagem.

END-OF-SELECTION.