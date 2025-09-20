*&---------------------------------------------------------------------------------------*
*& REPORT ZPR_EXEC_03_BHS.
*&---------------------------------------------------------------------------------------*
*& Nome:ZPR_EXEC_03_BHS
*& Tipo: Report
*& Objetivo: Leia a data atual do sistema e escreva em português a data por extenso.
*&---------------------------------------------------------------------------------------*

REPORT zpr_exec_03_bhs.

*&-------------------*
*& Tabelas internas
*&-------------------*
DATA: t_month_names LIKE TABLE OF t247 WITH HEADER LINE.

*&-------------------*
*& Variáveis
*&-------------------*
DATA: v_dia(2) TYPE c.
DATA: v_mes(10) TYPE c.
DATA: v_ano(4) TYPE c.
DATA: v_nome(52) TYPE c.
DATA: v_diasemana(3) TYPE p.
DATA: v_diaextenso(15) TYPE c.

*& Pega e converte do valor do mês para o nome do mês.
v_mes = sy-datum+4(2).

CALL FUNCTION 'MONTH_NAMES_GET'
  EXPORTING
    language    = sy-langu
  TABLES
    month_names = t_month_names.

READ TABLE t_month_names INDEX ( v_mes ).

v_mes = t_month_names-ltx. "Passa o nome convertido do mês para a variável.

*& Pega os valores de dia e ano e guarda nas suas respectivas variáveis.
v_dia = sy-datum+6(2).
v_ano = sy-datum+(4).

*& Verifica qual dia da semana é correspondete ao Dia.
CALL FUNCTION 'DAY_IN_WEEK'
  EXPORTING
    datum         = sy-datum
 IMPORTING
   WOTNR         = v_diasemana.

*& Verifica o dia da Semana e passa o valor para dia extenso.
case v_diasemana.
  when '1'.
    v_diaextenso = 'Segunda-Feira'.
  WHEN '2'.
    v_diaextenso = 'Terça-Feira'.
  WHEN '3'.
    v_diaextenso = 'Quarta-Feira'.
  when '4'.
    v_diaextenso = 'Quinta-Feira'.
  when '5'.
    v_diaextenso = 'Sexta-Feira'.
  WHEN '6'.
    v_diaextenso = 'Sábado'.
  WHEN '7'.
    v_diaextenso = 'Domingo'.
  when others.
ENDCASE.

*&Concatena os valores em forma de data separados por /.
CONCATENATE v_diaextenso v_mes v_ano INTO data(v_result) SEPARATED BY '/'.

WRITE v_result.