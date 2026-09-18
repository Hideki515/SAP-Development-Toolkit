CLASS zcl_singleton DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE . " Para impedir a instancia externa

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      RETURNING VALUE(ro_instance) TYPE REF TO zcl_singleton.

    METHODS set_value
      IMPORTING
        iv_input TYPE string.

    METHODS get_value
      RETURNING VALUE(ev_output) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA mo_instance TYPE REF TO zcl_singleton.

    DATA: v_text TYPE string.


ENDCLASS.

CLASS zcl_singleton IMPLEMENTATION.

  METHOD get_instance.

    " Caso a variável de referência não estiver apontando para nenhum objeto, cria um novo
    IF mo_instance IS NOT BOUND.

      CREATE OBJECT mo_instance.

    ENDIF.

    " Retorno da instância sendo ela nova ou já existente.
    ro_instance = mo_instance.

  ENDMETHOD.

  METHOD get_value.

    ev_output = v_text.

  ENDMETHOD.

  METHOD set_value.

    v_text = iv_input.

  ENDMETHOD.

ENDCLASS.