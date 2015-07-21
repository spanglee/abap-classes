REPORT z_lcl_date.

*----------------------------------------------------------------------*
* Z_CL_DATE class definition.                                          *
*----------------------------------------------------------------------*

CLASS z_cl_date DEFINITION.

*----------------------------------------------------------------------*
* PUBLIC SECTION                                                       *
*----------------------------------------------------------------------*

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        im_date_value TYPE d.

    METHODS add_days
      IMPORTING
        im_days_value TYPE i.

    METHODS add_months
      IMPORTING
        im_months_value TYPE i.

    METHODS add_weeks
      IMPORTING
        im_weeks_value TYPE i.

    METHODS add_years
      IMPORTING
        im_years_value TYPE i.

    METHODS compare
      IMPORTING
        im_date_value TYPE d
      EXPORTING
        ex_comparison_value TYPE int2.

    METHODS get_date
      RETURNING
        value(re_date_value) TYPE d.

    METHODS is_first_day_of_month
      EXPORTING
        ex_is_first_day_of_month TYPE boolean.

    METHODS is_last_day_of_month
      EXPORTING
        ex_is_last_day_of_month TYPE boolean.

    METHODS set_date
      IMPORTING
        im_date_value TYPE d.

    METHODS subtract_days
      IMPORTING
        im_days_value TYPE i.

    METHODS subtract_months
      IMPORTING
        im_months_value TYPE i.

    METHODS subtract_weeks
      IMPORTING
        im_weeks_value TYPE i.

    METHODS subtract_years
      IMPORTING
        im_years_value TYPE i.

*----------------------------------------------------------------------*
* PROTECTED SECTION                                                    *
*----------------------------------------------------------------------*

  PROTECTED SECTION.
    DATA date TYPE d.

    CONSTANTS maximum_date TYPE d VALUE '99991231'.

    CONSTANTS minimum_date TYPE d VALUE '18000101'.

    CONSTANTS true_value TYPE c VALUE 'X'.

    CONSTANTS false_value TYPE c VALUE ' '.

    CLASS-METHODS get_days_in_month
      IMPORTING
        im_month_value TYPE int2
        im_year_value TYPE int2
      EXPORTING
        ex_days_in_month TYPE int2.

    CLASS-METHODS is_leap_year
      IMPORTING
        im_year_value TYPE int2
      EXPORTING
        ex_is_leap_year TYPE boolean.

*----------------------------------------------------------------------*
* PRIVATE SECTION                                                      *
*----------------------------------------------------------------------*

  PRIVATE SECTION.

ENDCLASS.                    "z_cl_date DEFINITION


*----------------------------------------------------------------------*
* Z_CL_DATE class implementation.                                      *
*----------------------------------------------------------------------*

CLASS z_cl_date IMPLEMENTATION.

*----------------------------------------------------------------------*
* CONSTRUCTOR                                                          *
*----------------------------------------------------------------------*

  METHOD constructor.
    me->set_date( im_date_value ).
  ENDMETHOD.                    "CONSTRUCTOR

*----------------------------------------------------------------------*
* ADD_DAYS                                                             *
*----------------------------------------------------------------------*

  METHOD add_days.
    DATA: l_limit TYPE i.

    l_limit = me->maximum_date - me->date.

    IF l_limit < im_days_value.
      me->date = me->maximum_date.
    ELSE.
      ADD im_days_value TO me->date.
    ENDIF.
  ENDMETHOD.                    "ADD_DAYS

*----------------------------------------------------------------------*
* ADD_MONTHS                                                           *
*----------------------------------------------------------------------*

  METHOD add_months.
    DATA: l_month         TYPE int2,
          l_day           TYPE int2,
          l_year          TYPE int2,
          l_days_in_month TYPE int2.

    l_month = me->date+4(2) + im_months_value.
    l_day   = me->date+6(2).
    l_year  = me->date(4).

    WHILE l_month < 1.
      l_month = l_month + 12.
      l_year = l_year - 1.
    ENDWHILE.

    WHILE l_month > 12.
      l_month = l_month - 12.
      l_year = l_year + 1.
    ENDWHILE.

    CALL METHOD me->get_days_in_month
      EXPORTING
        im_month_value   = l_month
        im_year_value    = l_year
      IMPORTING
        ex_days_in_month = l_days_in_month.

    IF l_day > l_days_in_month.
      IF im_months_value < 0.
        l_day = l_days_in_month - ( l_day - l_days_in_month ).
      ELSE.
        l_day = l_day - l_days_in_month.
        l_month = l_month + 1.
        IF l_month > 12.
          l_year = l_year + 1.
          l_month = 1.
        ENDIF.
      ENDIF.
    ENDIF.

    me->date+4(2) = l_month.
    me->date+6(2) = l_day.
    me->date(4)   = l_year.
  ENDMETHOD.                    "ADD_MONTHS

*----------------------------------------------------------------------*
* ADD_WEEKS                                                            *
*----------------------------------------------------------------------*

  METHOD add_weeks.
    DATA: l_days TYPE i.

    l_days = 7 * im_weeks_value.
    me->add_days( l_days ).
  ENDMETHOD.                    "ADD_WEEKS

*----------------------------------------------------------------------*
* ADD_YEARS                                                            *
*----------------------------------------------------------------------*

  METHOD add_years.
    DATA: l_months TYPE i.

    l_months = 12 * im_years_value.
    me->add_months( l_months ).
  ENDMETHOD.                    "ADD_YEARS

*----------------------------------------------------------------------*
* COMPARE                                                              *
*----------------------------------------------------------------------*

  METHOD compare.
    IF im_date_value IS INITIAL.
      ex_comparison_value = -1.
    ENDIF.

    IF me->date < im_date_value.
      ex_comparison_value = 1.
    ELSEIF me->date = im_date_value.
      ex_comparison_value = 0.
    ELSE.
      ex_comparison_value = -1.
    ENDIF.
  ENDMETHOD.                    "COMPARE

*----------------------------------------------------------------------*
* GET_DATE                                                             *
*----------------------------------------------------------------------*

  METHOD get_date.
    re_date_value = me->date.
  ENDMETHOD.                    "GET_DATE

*----------------------------------------------------------------------*
* IS_FIRST_DAY_OF_MONTH                                                *
*----------------------------------------------------------------------*

  METHOD is_first_day_of_month.
    IF me->date+6(2) NE '01'.
      ex_is_first_day_of_month = me->false_value.
    ELSE.
      ex_is_first_day_of_month = me->true_value.
    ENDIF.
  ENDMETHOD.                    "IS_FIRST_DAY_OF_MONTH

*----------------------------------------------------------------------*
* IS_LAST_DAY_OF_MONTH                                                 *
*----------------------------------------------------------------------*

  METHOD is_last_day_of_month.
    DATA: l_date TYPE d.

    CHECK me->date NE me->maximum_date.

    l_date = me->date.
    ADD 1 TO l_date.

    IF l_date+6(2) NE '01'.
      ex_is_last_day_of_month = me->false_value.
    ELSE.
      ex_is_last_day_of_month = me->true_value.
    ENDIF.
  ENDMETHOD.                    "IS_LAST_DAY_OF_MONTH

*----------------------------------------------------------------------*
* SET_DATE                                                             *
*----------------------------------------------------------------------*

  METHOD set_date.
    IF im_date_value IS INITIAL.
      me->date = sy-datum.
    ELSE.
      me->date = im_date_value.
    ENDIF.
  ENDMETHOD.                    "SET_DATE

*----------------------------------------------------------------------*
* SUBTRACT_DAYS                                                        *
*----------------------------------------------------------------------*

  METHOD subtract_days.
    DATA: l_limit TYPE i.

    l_limit = me->minimum_date - me->date.

    IF l_limit > im_days_value.
      me->date = me->minimum_date.
    ELSE.
      SUBTRACT im_days_value FROM me->date.
    ENDIF.
  ENDMETHOD.                    "SUBTRACT_DAYS

*----------------------------------------------------------------------*
* SUBTRACT_MONTHS                                                      *
*----------------------------------------------------------------------*

  METHOD subtract_months.
    DATA: l_month         TYPE int2,
          l_day           TYPE int2,
          l_year          TYPE int2,
          l_days_in_month TYPE int2.

    l_month = me->date+4(2) - im_months_value.
    l_day   = me->date+6(2).
    l_year  = me->date(4).

    WHILE l_month < 1.
      l_month = l_month + 12.
      l_year = l_year - 1.
    ENDWHILE.

    WHILE l_month > 12.
      l_month = l_month - 12.
      l_year = l_year + 1.
    ENDWHILE.

    CALL METHOD me->get_days_in_month
      EXPORTING
        im_month_value   = l_month
        im_year_value    = l_year
      IMPORTING
        ex_days_in_month = l_days_in_month.

    IF l_day > l_days_in_month.
      IF im_months_value < 0.
        l_day = l_days_in_month - ( l_day - l_days_in_month ).
      ELSE.
        l_day = l_day - l_days_in_month.
        l_month = l_month + 1.
        IF l_month > 12.
          l_year = l_year + 1.
          l_month = 1.
        ENDIF.
      ENDIF.
    ENDIF.

    me->date+4(2) = l_month.
    me->date+6(2) = l_day.
    me->date(4)   = l_year.
  ENDMETHOD.                    "SUBTRACT_MONTHS

*----------------------------------------------------------------------*
* SUBTRACT_WEEKS                                                       *
*----------------------------------------------------------------------*

  METHOD subtract_weeks.
    DATA: l_days TYPE i.

    l_days = 7 * im_weeks_value.
    me->subtract_days( l_days ).
  ENDMETHOD.                    "SUBTRACT_WEEKS

*----------------------------------------------------------------------*
* SUBTRACT_YEARS                                                       *
*----------------------------------------------------------------------*

  METHOD subtract_years.
    DATA: l_months TYPE i.

    l_months = 12 * im_years_value.
    me->subtract_months( l_months ).
  ENDMETHOD.                    "SUBTRACT_YEARS

*----------------------------------------------------------------------*
* GET_DAYS_IN_MONTH                                                    *
*----------------------------------------------------------------------*

  METHOD get_days_in_month.
    DATA: l_days         TYPE i,
          l_is_leap_year TYPE boolean.

    CASE im_month_value.
      WHEN 1.
        l_days = 31.
      WHEN 2.
        l_days = 28.
      WHEN 3.
        l_days = 31.
      WHEN 4.
        l_days = 30.
      WHEN 5.
        l_days = 31.
      WHEN 6.
        l_days = 30.
      WHEN 7.
        l_days = 31.
      WHEN 8.
        l_days = 31.
      WHEN 9.
        l_days = 30.
      WHEN 10.
        l_days = 31.
      WHEN 11.
        l_days = 30.
      WHEN 12.
        l_days = 31.
    ENDCASE.

    CALL METHOD z_cl_date=>is_leap_year
      EXPORTING
        im_year_value   = im_year_value
      IMPORTING
        ex_is_leap_year = l_is_leap_year.

    IF im_month_value = 2 AND l_is_leap_year = true_value.
      l_days = l_days + 1.
    ENDIF.

    ex_days_in_month = l_days.
  ENDMETHOD.                    "GET_DAYS_IN_MONTH

*----------------------------------------------------------------------*
* IS_LEAP_YEAR                                                         *
*----------------------------------------------------------------------*

  METHOD is_leap_year.
    DATA: l_leap_year TYPE i.

    l_leap_year = im_year_value MOD 4.
    IF l_leap_year > 0.
      ex_is_leap_year = false_value.
      RETURN.
    ENDIF.

    l_leap_year = im_year_value MOD 100.
    IF l_leap_year > 0.
      ex_is_leap_year = true_value.
      RETURN.
    ENDIF.

    l_leap_year = im_year_value MOD 400.
    IF l_leap_year > 0.
      ex_is_leap_year = false_value.
      RETURN.
    ENDIF.

    ex_is_leap_year = true_value.
  ENDMETHOD.                    "IS_LEAP_YEAR
ENDCLASS.                    "z_cl_date IMPLEMENTATION
