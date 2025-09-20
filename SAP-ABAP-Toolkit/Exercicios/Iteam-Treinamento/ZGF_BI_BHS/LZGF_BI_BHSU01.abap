FUNCTION: ZFM_EXEC_BI_BHS.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_MATNR) TYPE  MARA-MATNR
*"     REFERENCE(I_MAKTX) TYPE  MAKT-MAKTX
*"     REFERENCE(I_MEINS) TYPE  MARA-MEINS
*"     REFERENCE(I_BRGEW) TYPE  MARA-BRGEW
*"     REFERENCE(I_GEWEI) TYPE  MARA-GEWEI
*"     REFERENCE(I_VOLUM) TYPE  MARA-VOLUM
*"     REFERENCE(I_VOLEH) TYPE  MARA-VOLEH
*"  TABLES
*"      IT_ERRO STRUCTURE  ZSTLOG OPTIONAL
*"      IT_SUCESSO STRUCTURE  ZSTLOG OPTIONAL
*"----------------------------------------------------------------------

  wa_bebida-matnr = i_matnr.
  wa_bebida-maktx = i_maktx.
  wa_bebida-meins = i_meins.
  wa_bebida-brgew = i_brgew.
  CONDENSE wa_bebida-brgew NO-GAPS.
  wa_bebida-gewei = i_gewei.
  wa_bebida-volum = i_volum.
  CONDENSE wa_bebida-volum NO-GAPS.
  wa_bebida-voleh = i_voleh.

  APPEND wa_bebida TO it_bebida.

  PERFORM zf_carrega_bdc.

  PERFORM z_carrega_transacao USING wa_bebida.

  IF wa_bebida-status = 'S'.

    wa_log-status = wa_bebida-status.
    wa_log-observacao = wa_bebida-observacao.

    APPEND wa_log TO it_sucesso.

  ELSEIF wa_bebida-status = 'E' or wa_bebida-status = 'A'.

    wa_log-status = wa_bebida-status.
    wa_log-observacao = wa_bebida-observacao.

    APPEND wa_log TO it_erro.

    PERFORM zf_gera_pasta.

  ENDIF.

  PERFORM zf_exibe_mensagens.
ENDFUNCTION.