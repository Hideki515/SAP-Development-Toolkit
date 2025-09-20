*&---------------------------------------------------------------------*
*&  Include           ZPR_EXEC_31_BHS_EVE
*&---------------------------------------------------------------------*

*&-------------------------*
*& START-OF-SELECTION
*&-------------------------*
START-OF-SELECTION.
*&-------------------------*
*& Performs
*&-------------------------*
  PERFORM zf_select_data. "Chama o Form de selecionar os dados.

*&-------------------------*
*& END-OF-SELECTION.
*&-------------------------*
END-OF-SELECTION.
*&-------------------------*
*& Performs
*&-------------------------*
  PERFORM zf_process_data.  "Chama o Form de Processamento dos dados.
  PERFORM zf_mount_alv.     "Chama o Form de montar o Fieldcat.
  PERFORM zf_layout_alv.    "Chama o Form de Layout do ALV.
  PERFORM zf_sort_alv.      "Chama o Form que ordena o ALV.
  PERFORM zf_display_alv.   "Chama o Form de Exibir o ALV.