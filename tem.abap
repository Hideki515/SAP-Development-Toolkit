cl_salv_table=>set_table_for_first_display
          EXPORTING
            is_layout                     = w_layout
          CHANGING
            it_outtab                     = t_conf_mat[]
            it_fieldcatalog               = t_fieldcat[]
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4.
