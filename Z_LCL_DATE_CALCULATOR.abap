REPORT z_lcl_date_calculator.

*----------------------------------------------------------------------*
* Z_LCL_DATE_CALCULATOR class definition.                              *
*----------------------------------------------------------------------*

CLASS z_lcl_date_calculator DEFINITION.

*----------------------------------------------------------------------*
* PUBLIC SECTION                                                       *
*----------------------------------------------------------------------*

  PUBLIC SECTION.

    CLASS-METHODS get_dow
      IMPORTING
        !im_date TYPE d
      RETURNING
        value(re_dow) TYPE i .

    CLASS-METHODS set_date_to_next_tuesday
      IMPORTING
        !im_date TYPE d
      RETURNING
        value(re_date) TYPE d .

    CLASS-METHODS set_date_to_next_next_tuesday
      IMPORTING
        !im_date TYPE d
      RETURNING
        value(re_date) TYPE d .

*----------------------------------------------------------------------*
* PROTECTED SECTION                                                    *
*----------------------------------------------------------------------*

  PROTECTED SECTION.

*----------------------------------------------------------------------*
* PRIVATE SECTION                                                      *
*----------------------------------------------------------------------*

  PRIVATE SECTION.

ENDCLASS.                    "z_lcl_date_calcuator DEFINITION

*----------------------------------------------------------------------*
* Z_LCL_DATE_CALCULATOR class implementation.                          *
*----------------------------------------------------------------------*

CLASS z_lcl_date_calculator IMPLEMENTATION.

*----------------------------------------------------------------------*
* Get day of week (DOW).                                               *
*----------------------------------------------------------------------*

  METHOD get_dow.
    DATA: l_dow TYPE int1.

    IF im_date IS INITIAL.
      EXIT.
    ENDIF.

    l_dow = im_date MOD 7.
    ADD 6 TO l_dow.
    re_dow = l_dow MOD 7.
    ADD 1 TO re_dow.
  ENDMETHOD.                    "get_dow

*----------------------------------------------------------------------*
* Set date to next Tuesday.                                            *
*----------------------------------------------------------------------*

  METHOD set_date_to_next_tuesday.
    DATA: l_date TYPE d,
          l_dow  TYPE i.

    IF im_date IS INITIAL.
      EXIT.
    ENDIF.

    l_date = im_date.

    WHILE l_dow NE 3.
      l_date = l_date + 1.
      l_dow = z_cl_date_calculator=>get_dow( im_date = l_date ).

      IF l_dow = 3.
        re_date = l_date.
        EXIT.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.                    "set_date_to_next_tuesday

*----------------------------------------------------------------------*
* Set date to next, next Tuesday.                                      *
*----------------------------------------------------------------------*

  METHOD set_date_to_next_next_tuesday.
    DATA: l_date TYPE d,
          l_dow  TYPE i.

    IF im_date IS INITIAL.
      EXIT.
    ENDIF.

    l_date = im_date.
    l_dow = z_cl_date_calculator=>get_dow( im_date = l_date ).

    IF l_dow NE 3.
      l_date = z_cl_date_calculator=>set_date_to_next_tuesday( im_date = l_date ).
    ENDIF.

    re_date = l_date + 7.
  ENDMETHOD.                    "set_date_to_next_next_tuesday

ENDCLASS.                    "z_lcl_date_calcuator IMPLEMENTATION
