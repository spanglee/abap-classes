REPORT  z_lcl_date_calc.

*----------------------------------------------------------------------*
* Z_CL_DATE_CALC class definition.                                     *
*----------------------------------------------------------------------*

CLASS z_cl_date_calc DEFINITION INHERITING FROM z_cl_date.

*----------------------------------------------------------------------*
* PUBLIC SECTION                                                       *
*----------------------------------------------------------------------*

  PUBLIC SECTION.

    METHODS check_if_leap_year
      IMPORTING
        im_year_value TYPE int2
      EXPORTING
        ex_is_leap_year TYPE boolean.

    METHODS day_of_week
      EXPORTING
        ex_day_of_week TYPE int1.

    METHODS day_of_year
      EXPORTING
        ex_day_of_year TYPE int2.

    METHODS days_in_month
      IMPORTING
        im_month_value TYPE int2
        im_year_value TYPE int2
      EXPORTING
        ex_days_in_month TYPE int2.

    METHODS days_in_year
      EXPORTING
        ex_days_in_year TYPE int2.

    METHODS week_of_month
      EXPORTING
        ex_week_of_month TYPE int1.

    METHODS week_of_year
      EXPORTING
        ex_week_of_year TYPE int2.

*----------------------------------------------------------------------*
* PROTECTED SECTION                                                    *
*----------------------------------------------------------------------*

  PROTECTED SECTION.

*----------------------------------------------------------------------*
* PRIVATE SECTION                                                      *
*----------------------------------------------------------------------*

  PRIVATE SECTION.

ENDCLASS.                    "z_cl_date_calc DEFINITION

*----------------------------------------------------------------------*
* Z_CL_DATE_CALC class implementation.                                 *
*----------------------------------------------------------------------*

CLASS z_cl_date_calc IMPLEMENTATION.

*----------------------------------------------------------------------*
* CHECK_IF_LEAP_YEAR                                                   *
*----------------------------------------------------------------------*

  METHOD check_if_leap_year.
    CALL METHOD z_cl_date=>is_leap_year
      EXPORTING
        im_year_value   = im_year_value
      IMPORTING
        ex_is_leap_year = ex_is_leap_year.
  ENDMETHOD.                    "CHECK_IF_LEAP_YEAR

*----------------------------------------------------------------------*
* DAY_OF_WEEK                                                          *
*----------------------------------------------------------------------*

  METHOD day_of_week.
    DATA: l_dow TYPE int1.

    l_dow = me->date MOD 7.
    ADD 6 TO l_dow.

    ex_day_of_week = l_dow MOD 7.
    ADD 1 TO ex_day_of_week.
  ENDMETHOD.                    "DAY_OF_WEEK

*----------------------------------------------------------------------*
* DAY_OF_YEAR                                                          *
*----------------------------------------------------------------------*

  METHOD day_of_year.
    DATA: l_begin_date TYPE d.

    l_begin_date = me->minimum_date.
    l_begin_date(4) = me->date(4).

    ex_day_of_year = me->date - l_begin_date.
  ENDMETHOD.                    "DAY_OF_YEAR

*----------------------------------------------------------------------*
* DAYS_IN_MONTH                                                        *
*----------------------------------------------------------------------*

  METHOD days_in_month.
    CALL METHOD z_cl_date=>get_days_in_month
      EXPORTING
        im_month_value   = im_month_value
        im_year_value    = im_year_value
      IMPORTING
        ex_days_in_month = ex_days_in_month.
  ENDMETHOD.                    "DAYS_IN_MONTH

*----------------------------------------------------------------------*
* DAYS_IN_YEAR                                                         *
*----------------------------------------------------------------------*

  METHOD days_in_year.
    DATA: l_year         TYPE int2,
          l_is_leap_year TYPE boolean.

    l_year = me->date(4).

    CALL METHOD me->is_leap_year
      EXPORTING
        im_year_value   = l_year
      IMPORTING
        ex_is_leap_year = l_is_leap_year.

    IF l_is_leap_year = me->true_value.
      ex_days_in_year = 366.
    ELSE.
      ex_days_in_year = 365.
    ENDIF.
  ENDMETHOD.                    "DAYS_IN_YEAR

*----------------------------------------------------------------------*
* WEEK_OF_MONTH                                                        *
*----------------------------------------------------------------------*

  METHOD week_of_month.
    DATA: l_days TYPE int2.

    l_days = me->date+6(2).

    ex_week_of_month = TRUNC( l_days / 7 ).

    IF ex_week_of_month < 1.
      ex_week_of_month = 1.
    ENDIF.
  ENDMETHOD.                    "WEEK_OF_MONTH

*----------------------------------------------------------------------*
* WEEK_OF_YEAR                                                         *
*----------------------------------------------------------------------*

  METHOD week_of_year.
    DATA: l_day_of_year TYPE int2.

    CALL METHOD me->day_of_year
      IMPORTING
        ex_day_of_year = l_day_of_year.

    ex_week_of_year = TRUNC( l_day_of_year / 7 ).
  ENDMETHOD.                    "WEEK_OF_YEAR
ENDCLASS.                    "z_cl_date_calc IMPLEMENTATION
