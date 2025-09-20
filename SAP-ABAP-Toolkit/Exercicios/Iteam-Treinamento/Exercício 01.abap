*************************************************
***Concatene duas palavras e escreva o resultado.
*************************************************

REPORT zpr_exec_01_bhs.

DATA palavra1(30) TYPE c VALUE 'Bruno'.
DATA palavra2(30) TYPE c VALUE 'Hideki'.

CONCATENATE palavra1 palavra2 INTO DATA(lv_result) SEPARATED BY space.

WRITE lv_result.