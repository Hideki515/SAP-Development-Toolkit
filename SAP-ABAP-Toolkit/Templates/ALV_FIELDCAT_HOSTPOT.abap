*&---------------------------------------------------------------------*
*& Report ZR_CUSTOM_ALV
*&---------------------------------------------------------------------*
*& Estrutura ALV Customizado com hotspot
*&---------------------------------------------------------------------*
REPORT zr_custom_alv.

*&---------------------------------------------------------------------*
*& TABLES
*&---------------------------------------------------------------------*
TABLES: sflight.

*&---------------------------------------------------------------------*
*& TYPES
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_sflight,
         carrid   TYPE sflight-carrid,
         connid   TYPE sflight-connid,
         fldate   TYPE sflight-fldate,
         seatsocc TYPE sflight-seatsocc,
       END OF ty_sflight.

*&---------------------------------------------------------------------*
*& INTERNAL TABLES
*&---------------------------------------------------------------------*
DATA: ti_sflight TYPE TABLE OF ty_sflight. "Cria a tabela interna para a tabela sflight.

"Config FieldCat
DATA: ti_fieldcat_alv TYPE TABLE OF slis_fieldcat_alv. "Cria a tabela interna para o fieldcat.

*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*
"Config FieldCat
DATA: wa_layout_alv   TYPE slis_layout_alv, "Cria a workarea quer guarda o Layout a ser utilizado no ALV.
      wa_fieldcat_alv TYPE slis_fieldcat_alv. "Cria a workare para a tabela de fieldcat.

*&---------------------------------------------------------------------*
*& SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01. "Parâmetros de seleção

SELECT-OPTIONS: s_carrid FOR sflight-carrid,
                s_connid FOR sflight-connid.

SELECTION-SCREEN END OF BLOCK b1. "FIM b1

*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM zf_selecao_dados. "Chama o form de seleção de dados.

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM zf_monsta_fieldcat. "Chama o form que o fieldcat.
  PERFORM zf_layout_alv. "Chama o form que monta o layout.
  PERFORM zf_mostra_alv. "Chama o form que exibe o ALV.

*&---------------------------------------------------------------------*
*& Form ZF_SELECAO_DADOS
*&---------------------------------------------------------------------*
*& Seleção dos dados.
*&---------------------------------------------------------------------*
FORM zf_selecao_dados.

  SELECT carrid
         connid
         fldate
         seatsocc
  FROM sflight
  INTO TABLE ti_sflight
  WHERE
  carrid IN s_carrid AND
  connid IN s_connid. "Seleciona os dados da tabela sflight seguindo os parâmetros de entradas e os guarda na ti_sflight.

ENDFORM. "FIM zf_selecao_dados

*&---------------------------------------------------------------------*
*& Form ZF_MONSTA_FIELDCAT
*&---------------------------------------------------------------------*
*& Monta tabela fieldcat
*&---------------------------------------------------------------------*
FORM zf_monsta_fieldcat .

  PERFORM zf_preenche_fieldcat USING:
        'TI_SFLIGHT' 'CARRID'   'X' 'Código da companhia aérea',
        'TI_SFLIGHT' 'CONNID'   '' 'Número de conexão do voo',
        'TI_SFLIGHT' 'FLDATE'   '' 'Data do voo',
        'TI_SFLIGHT' 'SEATSOCC' '' 'Ocupação econômica.'. "Chama o form que faz o preenchimento da tabela FieldCat, passando os valores necessarios.

ENDFORM. "FIM zf_monsta_fieldcat

*&---------------------------------------------------------------------*
*& Form ZF_PREENCHE_FIELDCAT
*&---------------------------------------------------------------------*
*& Faz o preenchimento da tabela de fieldcat
*&---------------------------------------------------------------------*
FORM zf_preenche_fieldcat USING p_tabname "Tabela
                                p_fieldname "Campo
                                p_hostpots "Campo vai ter hotspot
                                p_seltext_l. "Texto a ser exibido no nome do campo.

  CLEAR wa_fieldcat_alv. "Limpa a work area do fieldcat.

  wa_fieldcat_alv-tabname       = p_tabname. "Preenche o campo da work area com o valor passado no perform.
  wa_fieldcat_alv-fieldname     = p_fieldname. "Preenche o campo da work area com o valor passado no perform.
  wa_fieldcat_alv-hotspot       = p_hostpots. "Preenche o campo da work area com o valor passado no perform. -> Passa caso ele só se dar clique único no campo já executa caso não pode ser retirado
  wa_fieldcat_alv-seltext_l     = p_seltext_l. "Preenche o campo da work area com o valor passado no perform.

  APPEND wa_fieldcat_alv TO ti_fieldcat_alv. "Adiciona os dados da work area na tabela.

ENDFORM. "FIM zf_preenche_fieldcat

*&---------------------------------------------------------------------*
*& Form ZF_LAYOUT_ALV
*&---------------------------------------------------------------------*
*& Configura o layout do alv
*&---------------------------------------------------------------------*
FORM zf_layout_alv.

  CLEAR wa_layout_alv. "Limpa a work area.

  wa_layout_alv-zebra = 'X'. "Faz o ALV ser exibido zebrada.
  wa_layout_alv-colwidth_optimize = 'X'. "Otimiza o tamanho do valor do campo.

ENDFORM. "FIM ZF_LAYOUT_ALV

*&---------------------------------------------------------------------*
*& Form ZF_MOSTRA_ALV
*&---------------------------------------------------------------------*
*& Exibe os relátorio ALV
*&---------------------------------------------------------------------*
FORM zf_mostra_alv.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = sy-repid
      i_callback_user_command = 'ZF_USE_COMMAND'
      i_grid_title            = 'Assentos Ocupados'
      is_layout               = wa_layout_alv
      it_fieldcat             = ti_fieldcat_alv
    TABLES
      t_outtab                = ti_sflight
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.

  IF sy-subrc <> 0.

    MESSAGE: TEXT-e01 TYPE 'S' DISPLAY LIKE 'E'. "Erro ao gerar Relátorio ALV.
    LEAVE TO LIST-PROCESSING. "Volta para a tela de seleção.

  ENDIF.


ENDFORM. "FIM ZF_MOSTRA_ALV.

*&---------------------------------------------------------------------*
*& Form ZF_USE_COMMAND
*&---------------------------------------------------------------------*
*& Verifica quais ações do ALV foram utilizadas
*&---------------------------------------------------------------------*
FORM zf_use_command USING p_ucomm    LIKE sy-ucomm  "Armazena o código de comando da ação utilizada
                          p_selfield TYPE slis_selfield. "Armazena informações sobre qual linha ou célula foi clicada pelo usuário

  CASE p_ucomm. "Caso p_ucomm

    WHEN '&IC1'. "Verifica se o valor da p_ucomm está com o valor '&IC1' por default.

      CASE p_selfield-fieldname. "Caso p_selfield-fieldname

        WHEN 'CARRID'. "Verifica se é o valor do campo selecionado.

          SET PARAMETER ID 'CAR' FIELD p_selfield-value. "Seta o valor do parâmetro da tela de seleção da transação
          CALL TRANSACTION 'ZFLIFHT' AND SKIP FIRST SCREEN. "Chama a transação e executa ele.

        WHEN OTHERS.
          " Nenhuma ação específica necessária para outros casos
      ENDCASE.

  ENDCASE.

ENDFORM. "FIM zf_use_command
