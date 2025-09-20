*&---------------------------------------------------------------------------------------*
*& REPORT zpr_exec_02_bhs.
*&---------------------------------------------------------------------------------------*
*& Nome:zpr_exec_02_bhs
*& Tipo: Report
*& Objetivo: Concatene duas palavras e o mês atual, unindo por “-“ e escreva o resultado.
*&---------------------------------------------------------------------------------------*

REPORT zpr_exec_02_bhs.

*&-------------------*
*& Tabelas internas
*&-------------------*
DATA: T_MONTH_NAMES LIKE TABLE OF T247 WITH HEADER LINE.

*&-------------------*
*& Variáveis
*&-------------------*
DATA: v_dia(2) TYPE c.
DATA: v_mes(10) TYPE c.
DATA: v_ano(4) TYPE c.

*& Pega e converte do valor do mês para o nome do mês.
v_mes = SY-DATUM+4(2).

CALL FUNCTION 'MONTH_NAMES_GET'
 EXPORTING
   LANGUAGE     = SY-LANGU
  TABLES
    MONTH_NAMES = T_MONTH_NAMES.

READ TABLE T_MONTH_NAMES INDEX ( v_mes ).

v_mes = T_MONTH_NAMES-LTX. "Passa o nome convertido do mês para a variável.

*& Pega os valores de dia e ano e guarda nas suas respectivas variáveis.
v_dia = SY-DATUM+6(2).
v_ano = SY-DATUM+(4).

*& Concatena as três variávies e guarda o resultado no variáveil v_result.
CONCATENATE v_dia v_mes v_ano INTO DATA(v_result) SEPARATED BY '-'.

WRITE v_result.