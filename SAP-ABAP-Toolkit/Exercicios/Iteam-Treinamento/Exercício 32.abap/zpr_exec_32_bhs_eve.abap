*&---------------------------------------------------------------------*
*&  Include           ZPR_EXEC_32_BHS_EVE
*&---------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_loc. "Cria o quadradinho no lado do campo para a seleção do local do arquivo.
  PERFORM zf_select_file. "Chama o form de selecionar o arquivo.

START-OF-SELECTION.
  PERFORM zf_upload_file USING gv_filename. "Chama o form de upload do arquivo.
  PERFORM zf_process_file. "Chama o form de processamento do arquivo.
  PERFORM zf_process_bdc. "Chama o form de processamento do BDC.

*Verifica se o botão de pasta foi acionado para a geração de pasta LOG na SM35.
  IF rb_past = 'X'.
    LOOP AT it_bebida INTO wa_bebida. "Loop na tabela interna passando os dados do laço atual para work area.
      PERFORM zf_gera_pasta CHANGING wa_bebida-status
                                     wa_bebida-observacao. "Chama o form de geração de pasta passando status e observação.

      MODIFY it_bebida FROM wa_bebida TRANSPORTING status observacao. "Modifica a tabela interna transportando status e observação.
    ENDLOOP.
  ENDIF.

  WRITE: / 'Processamento Concluído.'. "Mensagem de Processamento.
  WRITE: / gv_ok, ' registros processados com sucesso.' COLOR COL_TOTAL. "Mensagem de registros processados com sucesso.
  WRITE: / gv_nok, ' registros com erro.' COLOR COL_TOTAL. "Mensagem de registros processados com sucesso.

  SKIP. "Pula linhas.

  WRITE: /  'Para verificar a pasta BDC é para a transação SM35' COLOR COL_TOTAL. "Mensagem de registros processados com sucesso.

END-OF-SELECTION.