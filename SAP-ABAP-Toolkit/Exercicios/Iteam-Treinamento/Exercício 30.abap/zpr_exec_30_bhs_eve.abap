*&---------------------------------------------------------------------*
*&  Include           ZPR_EXEC_30_BHS_EVE
*&---------------------------------------------------------------------*

START-OF-SELECTION.
  PERFORM zf_select_data. "Seleção de dados.

END-OF-SELECTION.
  PERFORM zf_process_data. "Processa os dados.
  PERFORM zf_top_of_page. "Cria o cabeçalho do ALV.
  PERFORM zf_sort_alv. "Ordena os campos e junta eles.
  PERFORM zf_mount_alv. "Monta o ALV.
  PERFORM zf_layout_alv. "Monta o Layout do ALV.
  PERFORM zf_display_alv. "Exibe o ALV.