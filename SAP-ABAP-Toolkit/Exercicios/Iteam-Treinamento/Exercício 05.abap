*&---------------------------------------------------------------------------------------*
*& REPORT ZPR_EXEC_05_BHS.
*&---------------------------------------------------------------------------------------*
*& Nome:ZPR_EXEC_05_BHS
*& Tipo: Report
*& Conte quantas vogais há no nome do usuário executando o programa e imprima
*& o resultado.
*&---------------------------------------------------------------------------------------*

REPORT zpr_exec_05_bhs.

*&-------------------*
*& Tabelas internas
*&-------------------*
DATA: t_caracteres TYPE STANDARD TABLE OF char1 WITH EMPTY KEY.

*&-------------------*
*& Variáveis
*&-------------------*
DATA: v_nome TYPE string.
DATA: v_vogais TYPE c VALUE 0.
DATA: v_l TYPE c.

*& Passa o nome do usuário que executa o programa.
v_nome = sy-uname.

*& Passa todos os caracteres do nome do usuário para Maiscúlo.
TRANSLATE v_nome TO UPPER CASE.

*& Passa os caracteres do nome para a tabela interna.
CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
  EXPORTING
    i_string         = v_nome
    i_tabline_length = 1
  TABLES
    et_table         = t_caracteres.

*& Laço de repetição que passa pelas linhas da tabela dos caracteres
LOOP AT t_caracteres INTO v_l.
  CASE v_l.
    WHEN 'A' or 'E' or 'I' or 'O' or 'U'. "Verifica se no valor possui vogias caso possua adiciona +1 no contador.
      ADD 1 TO v_vogais.
  ENDCASE.
ENDLOOP.

*& Escreve a mensagem na tela.
WRITE: 'O Nome do usuário', v_nome, 'tem :', v_vogais, 'vogais'.