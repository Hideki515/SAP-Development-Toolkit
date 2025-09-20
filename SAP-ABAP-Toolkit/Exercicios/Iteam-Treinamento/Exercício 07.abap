*&---------------------------------------------------------------------------------------*
*& REPORT zpr_exec_02_bhs.
*&---------------------------------------------------------------------------------------*
*& Nome:zpr_exec_02_bhs
*& Tipo: Report
*& Objetivo: (Leia o help do comando FORM) Faça uma rotina que receba 4 variáveis
*& globais sendo elas do mesmo tipo. Cada variável será recebida de uma maneira
*& diferente: 2 usando a adição USING e 2 usando a adição CHANGING do comando
*& FORM. Em cada situação utilize e omita a adição VALUE. Imprima o conteúdo das
*& variáveis antes da rotina ser chamada, no começo da rotina, no final da rotina e após a
*& sua chamada. Verificar como o conteúdo das variáveis se comporta no debug.
*&---------------------------------------------------------------------------------------*

REPORT zpr_exec_07_bhs.

*&-------------------*
*& Variáveis
*&-------------------*
DATA: gv_var1 TYPE string VALUE 'Valor Inicial Using'.
DATA: gv_var2 TYPE string VALUE 'Valor Inicial Using'.
DATA: gv_var3 TYPE string VALUE 'Valor Inicial Changing'.
DATA: gv_var4 TYPE string VALUE 'Valor Inicial Changing'.

START-OF-SELECTION.
  WRITE: / 'Valores antes do Form:',"Escreve na tela os valores antes do Form.
         / gv_var1,
         / gv_var2,
         / gv_var3,
         / gv_var4.

*& Chama o Form utilizando USING e CHANGING.
  PERFORM zfm_using USING gv_var1 gv_var2
  CHANGING gv_var3 gv_var4.

  SKIP 1. "Pula uma linha na hora de escrever.
  WRITE: "escreve na tela os valores depois do Form.
         / 'Valores Depois do Form:',
         / gv_var1,
         / gv_var2,
         / gv_var3,
         / gv_var4.

*&---------------------------------------------------------------------*
*&      Form  ZFM_USING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_VAR1  text
*      -->P_GV_VAR2  text
*      <--P_GV_VAR3  text
*      <--P_GV_VAR4  text
*----------------------------------------------------------------------*
FORM zfm_using  USING    VALUE(p_gv_var1)
                         p_gv_var2
                CHANGING VALUE(p_gv_var3)
                         p_gv_var4.

  SKIP 1."Pula uma linha
  WRITE: "Escreve na tela os valores no inicio do Form.
     / 'Valores no começo do Form:',
     / p_gv_var1,
     / p_gv_var2,
     / p_gv_var3,
     / p_gv_var4.

*& Passa os novos valores para as variáveis.
  p_gv_var1 = 'Var 1 modificado Using'.
  p_gv_var2 = 'Var 2 modificado Using'.
  p_gv_var3 = 'Var 3 modificado Changing'.
  p_gv_var4 = 'Var 4 modificado Changing'.

  SKIP 1."Valor Inicial Changing'
  WRITE:"Escreve na tela os valores no Fim do Form.
       / 'Valores no final do Form:',
       / p_gv_var1,
       / p_gv_var2,
       / p_gv_var3,
       / p_gv_var4.
ENDFORM.