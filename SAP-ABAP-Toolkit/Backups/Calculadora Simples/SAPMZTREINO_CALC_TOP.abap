*&---------------------------------------------------------------------*
*& Include          SAPMZTREINO_CALC_TOP
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Variáveis
*&---------------------------------------------------------------------*
DATA:
  v_display  TYPE char45,
  v_value1   TYPE char45,
  v_value2   TYPE char45,
  v_result   TYPE char45,
  v_operacao TYPE char3.

*&---------------------------------------------------------------------*
*& Constants
*&---------------------------------------------------------------------*
CONSTANTS:
  c_0       TYPE c VALUE '0'          , " Valor 0.
  c_1       TYPE c VALUE '1'          , " Valor 1.
  c_2       TYPE c VALUE '2'          , " Valor 2.
  c_3       TYPE c VALUE '3'          , " Valor 3.
  c_4       TYPE c VALUE '4'          , " Valor 4.
  c_5       TYPE c VALUE '5'          , " Valor 5.
  c_6       TYPE c VALUE '6'          , " Valor 6.
  c_7       TYPE c VALUE '7'          , " Valor 7.
  c_8       TYPE c VALUE '8'          , " Valor 8.
  c_9       TYPE c VALUE '9'          , " Valor 9.
  c_s       TYPE c VALUE 'S'          , " Sucesso
  c_e       TYPE c VALUE 'E'          , " Erro
  c_back    TYPE char5 VALUE '&BACK'  , " Botão de Voltar
  c_UP      TYPE char3 VALUE '&UP'    , " Botão de Exit
  c_CANC    TYPE char5 VALUE '&CANC'  , " Botão de Cancelar
  c_add     TYPE char3 VALUE 'ADD'    , " Adição.
  c_sub     TYPE char3 VALUE 'SUB'    , " Subtração.
  c_mul     TYPE char3 VALUE 'MUL'    , " Multiplicação.
  c_div     TYPE char3 VALUE 'DIV'    , " Divisão.
  c_but_0   TYPE char5 VALUE 'BUT_0'  , " Botão númerico 0
  c_but_01  TYPE char6 VALUE 'BUT_01' , " Botão númerico 1
  c_but_02  TYPE char6 VALUE 'BUT_02' , " Botão númerico 2
  c_but_03  TYPE char6 VALUE 'BUT_03' , " Botão númerico 3
  c_but_04  TYPE char6 VALUE 'BUT_04' , " Botão númerico 4
  c_but_05  TYPE char6 VALUE 'BUT_05' , " Botão númerico 5
  c_but_06  TYPE char6 VALUE 'BUT_06' , " Botão númerico 6
  c_but_07  TYPE char6 VALUE 'BUT_07' , " Botão númerico 7
  c_but_08  TYPE char6 VALUE 'BUT_08' , " Botão númerico 8
  c_but_09  TYPE char6 VALUE 'BUT_09' , " Botão númerico 9
  c_but_c   TYPE char5 VALUE 'BUT_C'  , " Botão Limpar
  c_but_eq  TYPE char6 VALUE 'BUT_EQ' , " Botão Equal
  c_but_ce  TYPE char6 VALUE 'BUT_CE' , " Botão CE
  c_but_add TYPE char7 VALUE 'BUT_ADD', " Botão Adição
  c_but_sub TYPE char7 VALUE 'BUT_SUB', " Botão Subtração
  c_but_mul TYPE char7 VALUE 'BUT_MUL', " Botão Multiplicação
  c_but_div TYPE char7 VALUE 'BUT_DIV'. " Botão Divisão