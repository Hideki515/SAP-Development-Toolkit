*&---------------------------------------------------------------------------------------*
*& REPORT ZPR_EXEC_04_BHS.
*&---------------------------------------------------------------------------------------*
*& Nome:ZPR_EXEC_04_BHS
*& Tipo: Report
*& Leia a hora atual do sistema e escreva o horário em 6 diferentes fusos (3 deles
*& devem ser obrigatoriamente Greenwich, Brasília e o Delhi).
*&---------------------------------------------------------------------------------------*

REPORT zpr_exec_04_bhs.

*&-------------------*
*& Variáveis
*&-------------------*
DATA v_hora TYPE timestampl.
DATA v_t1 TYPE string.
DATA v_t2 TYPE string.
DATA v_t3 TYPE string.
DATA v_t4 TYPE string.
DATA v_t5 TYPE string.
DATA v_t6 TYPE string.

*& Pega a da data e hora do sistema e passa para a variável.
GET TIME STAMP FIELD v_hora.

*& Passa quais os fuso-horários seram utilizados.
v_t1 = 'BRAZIL'.
v_t2 = 'GMTUK'.
v_t3 = 'INDIA'.
v_t4 = 'JAPAN'.
v_t5 = 'EGYPT'.
v_t6 = 'HAW'.

*& Escreve na tela o horário nas respectivos fuso-horário.
WRITE: / 'Brasil:',    v_hora TIME ZONE v_t1,
       / 'Greenwich:', v_hora TIME ZONE v_t2,
       / 'Delhi:',     v_hora TIME ZONE v_t3,
       / 'Japan:',     v_hora TIME ZONE v_t4,
       / 'Egito:',     v_hora TIME ZONE v_t5,
       / 'Hawaii:',    v_hora TIME ZONE v_t6.