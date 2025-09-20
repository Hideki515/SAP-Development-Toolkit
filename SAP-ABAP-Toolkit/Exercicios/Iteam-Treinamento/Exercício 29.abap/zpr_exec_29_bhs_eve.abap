*&---------------------------------------------------------------------*
*&  Include           ZPR_EXEC_29_BHS_EVE
*&---------------------------------------------------------------------*

*&-------------------------*
*& Start-Of-Selection
*&-------------------------*
START-OF-SELECTION.
  PERFORM zf_select_data. "Chama o form de seleção de dados.

*&-------------------------*
*& End-Of-Selection
*&-------------------------*
END-OF-SELECTION.
  PERFORM zf_process_data. "Chama o form de processamento dos dados.
  PERFORM zf_broken_fields. "Chama o form de quebra de campo.
  PERFORM zf_assemble_alv. "Chama o form de montar o ALV.
  PERFORM zf_display_alv.  "Chama o form de Exibir o ALV.