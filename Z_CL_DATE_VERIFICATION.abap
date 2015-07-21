REPORT z_cl_date_verification.

*----------------------------------------------------------------------*
* Program Notes:                                                       *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* START OF SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM verify_base_class_methods.
  PERFORM verify_calc_subclass_methods.
  PERFORM verify_fmt_subclass_methods.

*----------------------------------------------------------------------*
* END OF SELECTION                                                     *
*----------------------------------------------------------------------*
END-OF-SELECTION.

*----------------------------------------------------------------------*
* FORM: verify base class methods.                                     *
*----------------------------------------------------------------------*

FORM verify_base_class_methods.
  SKIP.
  WRITE: / 'Verify Z_CL_DATE (base) class methods:'.
  SKIP.

  PERFORM verify_add_days.
  PERFORM verify_add_months.
  PERFORM verify_add_weeks.
  PERFORM verify_add_years.
  PERFORM verify_compare.
  PERFORM verify_is_first_day_of_month.
  PERFORM verify_is_last_day_of_month.
  PERFORM verify_set_date.
  PERFORM verify_subtract_days.
  PERFORM verify_subtract_months.
  PERFORM verify_subtract_weeks.
  PERFORM verify_subtract_years.
ENDFORM.                    "VERIFY_BASE_CLASS_METHODS

*----------------------------------------------------------------------*
* FORM: verify calculation subclass methods.                           *
*----------------------------------------------------------------------*

FORM verify_calc_subclass_methods.
  SKIP.
  WRITE: / 'Verify Z_CL_DATE subclass Z_CL_DATE_CALC methods:'.
  SKIP.

  PERFORM verify_day_of_week.
  PERFORM verify_day_of_year.
  PERFORM verify_days_in_month.
  PERFORM verify_days_in_year.
  PERFORM verify_week_of_month.
  PERFORM verify_week_of_year.
  PERFORM verify_check_if_leap_year.
ENDFORM.                    "VERIFY_CALC_SUBCLASS_METHODS

*----------------------------------------------------------------------*
* FORM: verify format subclass methods.                                *
*----------------------------------------------------------------------*

FORM verify_fmt_subclass_methods.
  SKIP.
  WRITE: / 'Verify Z_CL_DATE subclass Z_CL_DATE_FMT methods:'.
  SKIP.

  PERFORM verify_set_format.
  PERFORM verify_set_letter_case.
  PERFORM verify_get_formatted_date.
ENDFORM.                    "VERIFY_FMT_SUBCLASS_METHODS

*----------------------------------------------------------------------*
* FORM: verify add days method.                                        *
*----------------------------------------------------------------------*

FORM verify_add_days.
  DATA: r_date TYPE REF TO z_cl_date,
        l_date TYPE d.

  CREATE OBJECT r_date
    EXPORTING
      im_date_value = sy-datum.

  r_date->add_days( 10 ).
  l_date = r_date->get_date( ).

  WRITE: / '  ADD_DAYS (add 10 days to current date):', l_date MM/DD/YYYY.
ENDFORM.                    "VERIFY_ADD_DAYS

*----------------------------------------------------------------------*
* FORM: verify add months method.                                      *
*----------------------------------------------------------------------*

FORM verify_add_months.
  DATA: r_date TYPE REF TO z_cl_date,
        l_date TYPE d.

  CREATE OBJECT r_date
    EXPORTING
      im_date_value = sy-datum.

  r_date->add_months( 2 ).
  l_date = r_date->get_date( ).

  WRITE: / '  ADD_MONTHS (add 2 months to current date):',l_date MM/DD/YYYY.
ENDFORM.                    "VERIFY_ADD_MONTHS

*----------------------------------------------------------------------*
* FORM: verify add weeks method                                        *
*----------------------------------------------------------------------*

FORM verify_add_weeks.
  DATA: r_date TYPE REF TO z_cl_date,
        l_date TYPE d.

  CREATE OBJECT r_date
    EXPORTING
      im_date_value = sy-datum.

  r_date->add_weeks( 3 ).
  l_date = r_date->get_date( ).

  WRITE: / '  ADD_WEEKS (add 3 weeks to current date):', l_date MM/DD/YYYY.
ENDFORM.                    "VERIFY_ADD_WEEKS

*----------------------------------------------------------------------*
* FORM: verify add years method.                                       *
*----------------------------------------------------------------------*

FORM verify_add_years.
  DATA: r_date TYPE REF TO z_cl_date,
        l_date TYPE d.

  CREATE OBJECT r_date
    EXPORTING
      im_date_value = sy-datum.

  r_date->add_years( 2 ).
  l_date = r_date->get_date( ).

  WRITE: / '  ADD_YEARS (add 2 years to current date):', l_date MM/DD/YYYY.
ENDFORM.                    "VERIFY_ADD_YEARS

*----------------------------------------------------------------------*
* FORM: verify compare method.                                         *
*----------------------------------------------------------------------*

FORM verify_compare.
  DATA: r_date       TYPE REF TO z_cl_date,
        l_comparison TYPE int2,
        l_date       TYPE d.

  CREATE OBJECT r_date
    EXPORTING
      im_date_value = sy-datum.

  CALL METHOD r_date->compare
    EXPORTING
      im_date_value       = '20051201'
    IMPORTING
      ex_comparison_value = l_comparison.

  l_date = r_date->get_date( ).

  IF l_comparison EQ -1.
    WRITE: / '  COMPARE (date 12/01/2005): is less than the current date', l_date MM/DD/YYYY.
  ELSEIF l_comparison EQ 0.
    WRITE: / '  COMPARE (date 12/01/2005): is equal to the current date', l_date MM/DD/YYYY.
  ELSE.
    WRITE: / '  COMPARE (date 12/01/2005): is greater than the current date', l_date MM/DD/YYYY.
  ENDIF.
ENDFORM.                    "VERIFY_COMPARE

*----------------------------------------------------------------------*
* FORM: verify is first day of month method.                           *
*----------------------------------------------------------------------*

FORM verify_is_first_day_of_month.
  DATA: r_date                  TYPE REF TO z_cl_date,
        l_is_first_day_of_month TYPE boolean.

  CREATE OBJECT r_date
    EXPORTING
      im_date_value = '20051101'.

  CALL METHOD r_date->is_first_day_of_month
    IMPORTING
      ex_is_first_day_of_month = l_is_first_day_of_month.

  IF l_is_first_day_of_month = 'X'.
    WRITE: / '  IS_FIRST_DAY_OF_MONTH (date 11/01/2005): is first day of month'.
  ELSE.
    WRITE: / '  IS_FIRST_DAY_OF_MONTH (date 11/01/2005): is not the first day of month'.
  ENDIF.
ENDFORM.                    "VERIFY_IS_FIRST_DAY_OF_MONTH

*----------------------------------------------------------------------*
* FORM: verify is last day of month method.                            *
*----------------------------------------------------------------------*

FORM verify_is_last_day_of_month.
  DATA: r_date                 TYPE REF TO z_cl_date,
        l_is_last_day_of_month TYPE boolean.

  CREATE OBJECT r_date
    EXPORTING
      im_date_value = '20051129'.

  CALL METHOD r_date->is_last_day_of_month
    IMPORTING
      ex_is_last_day_of_month = l_is_last_day_of_month.

  IF l_is_last_day_of_month = 'X'.
    WRITE: / '  IS_LAST_DAY_OF_MONTH (date 11/29/2005): is last day of month'.
  ELSE.
    WRITE: / '  IS_LAST_DAY_OF_MONTH (date 11/29/2005): is not the last day of month'.
  ENDIF.
ENDFORM.                    "VERIFY_IS_LAST_DAY_OF_MONTH

*----------------------------------------------------------------------*
* FORM: set date method.                                               *
*----------------------------------------------------------------------*

FORM verify_set_date.
  DATA: r_date TYPE REF TO z_cl_date,
        l_date TYPE d.

  CREATE OBJECT r_date
    EXPORTING
      im_date_value = sy-datum.

  r_date->set_date( '19991001' ).
  l_date = r_date->get_date( ).

  WRITE: / '  SET_DATE (date 10/01/1999):', l_date MM/DD/YYYY.
ENDFORM.                    "VERIFY_SET_DATE

*----------------------------------------------------------------------*
* FORM: verify subtract days method.                                   *
*----------------------------------------------------------------------*

FORM verify_subtract_days.
  DATA: r_date TYPE REF TO z_cl_date,
        l_date TYPE d.

  CREATE OBJECT r_date
    EXPORTING
      im_date_value = sy-datum.

  r_date->subtract_days( 10 ).
  l_date = r_date->get_date( ).

  WRITE: / '  SUBTRACT_DAYS (subtract 10 days from current date):', l_date MM/DD/YYYY.
ENDFORM.                    "VERIFY_SUBTRACT_DAYS

*----------------------------------------------------------------------*
* FORM: verify subtract months method.                                 *
*----------------------------------------------------------------------*

FORM verify_subtract_months.
  DATA: r_date TYPE REF TO z_cl_date,
        l_date TYPE d.

  CREATE OBJECT r_date
    EXPORTING
      im_date_value = sy-datum.

  r_date->subtract_months( 12 ).
  l_date = r_date->get_date( ).

  WRITE: / '  SUBTRACT_MONTHS (subtract 12 months from current date):', l_date MM/DD/YYYY.
ENDFORM.                    "VERIFY_SUBTRACT_MONTHS

*----------------------------------------------------------------------*
* FORM: verify subtract weeks method.                                  *
*----------------------------------------------------------------------*

FORM verify_subtract_weeks.
  DATA: r_date TYPE REF TO z_cl_date,
        l_date TYPE d.

  CREATE OBJECT r_date
    EXPORTING
      im_date_value = sy-datum.

  r_date->subtract_weeks( 2 ).
  l_date = r_date->get_date( ).

  WRITE: / '  SUBTRACT_WEEKS (subtract 2 weeks from current date):', l_date MM/DD/YYYY.
ENDFORM.                    "VERIFY_SUBTRACT_WEEKS

*----------------------------------------------------------------------*
* FORM: verify subtract years method.                                  *
*----------------------------------------------------------------------*

FORM verify_subtract_years.
  DATA: r_date TYPE REF TO z_cl_date,
        l_date TYPE d.

  CREATE OBJECT r_date
    EXPORTING
      im_date_value = sy-datum.

  r_date->subtract_years( 2 ).
  l_date = r_date->get_date( ).

  WRITE: / '  SUBTRACT_YEARS (subtract 2 years from current date):', l_date MM/DD/YYYY.
ENDFORM.                    "VERIFY_SUBTRACT_YEARS

*----------------------------------------------------------------------*
* FORM: verify check if leap year method.                              *
*----------------------------------------------------------------------*

FORM verify_check_if_leap_year.
  DATA: r_date_calc    TYPE REF TO z_cl_date_calc,
        l_is_leap_year TYPE boolean.

  CREATE OBJECT r_date_calc
    EXPORTING
      im_date_value = sy-datum.

  CALL METHOD r_date_calc->check_if_leap_year
    EXPORTING
      im_year_value   = 2004
    IMPORTING
      ex_is_leap_year = l_is_leap_year.

  IF l_is_leap_year = 'X'.
    WRITE: / '  CHECK_IF_LEAP_YEAR (year 2004): is a leap year.'.
  ELSE.
    WRITE: / '  CHECK_IF_LEAP_YEAR (year 2004): is not a leap year.'.
  ENDIF.
ENDFORM.                    "VERIFY_CHECK_IF_LEAP_YEAR

*----------------------------------------------------------------------*
* FORM: verify day of week method.                                     *
*----------------------------------------------------------------------*

FORM verify_day_of_week.
  DATA: r_date_calc   TYPE REF TO z_cl_date_calc,
        l_day_of_week TYPE int1.

  CREATE OBJECT r_date_calc
    EXPORTING
      im_date_value = sy-datum.

  CALL METHOD r_date_calc->day_of_week
    IMPORTING
      ex_day_of_week = l_day_of_week.

  WRITE: / '  DAY_OF_WEEK (current date):', l_day_of_week.
ENDFORM.                    "VERIFY_DAY_OF_WEEK

*----------------------------------------------------------------------*
* FORM: verify day of year method.                                     *
*----------------------------------------------------------------------*

FORM verify_day_of_year.
  DATA: r_date_calc   TYPE REF TO z_cl_date_calc,
        l_day_of_year TYPE int2.

  CREATE OBJECT r_date_calc
    EXPORTING
      im_date_value = sy-datum.

  CALL METHOD r_date_calc->day_of_year
    IMPORTING
      ex_day_of_year = l_day_of_year.

  WRITE: / '  DAY_OF_YEAR (current date):', l_day_of_year.
ENDFORM.                    "VERIFY_DAY_OF_YEAR

*----------------------------------------------------------------------*
* FORM: verify days in month method.                                   *
*----------------------------------------------------------------------*

FORM verify_days_in_month.
  DATA: r_date_calc     TYPE REF TO z_cl_date_calc,
        l_month         TYPE int2,
        l_year          TYPE int2,
        l_days_in_month TYPE int2.

  l_month = sy-datum+4(2).
  l_year  = sy-datum(4).

  CREATE OBJECT r_date_calc
    EXPORTING
      im_date_value = sy-datum.

  CALL METHOD r_date_calc->days_in_month
    EXPORTING
      im_month_value   = l_month
      im_year_value    = l_year
    IMPORTING
      ex_days_in_month = l_days_in_month.

  WRITE: / '  DAYS_IN_MONTH (current date):', l_days_in_month.
ENDFORM.                    "VERIFY_DAYS_IN_MONTH

*----------------------------------------------------------------------*
* FORM: verify days in year method.                                    *
*----------------------------------------------------------------------*

FORM verify_days_in_year.
  DATA: r_date_calc    TYPE REF TO z_cl_date_calc,
        l_days_in_year TYPE int2.

  CREATE OBJECT r_date_calc
    EXPORTING
      im_date_value = sy-datum.

  CALL METHOD r_date_calc->days_in_year
    IMPORTING
      ex_days_in_year = l_days_in_year.

  WRITE: / '  DAYS_IN_YEAR (current date):', l_days_in_year.
ENDFORM.                    "VERIFY_DAYS_IN_YEAR

*----------------------------------------------------------------------*
* FORM: verify week of month method.                                   *
*----------------------------------------------------------------------*

FORM verify_week_of_month.
  DATA: r_date_calc     TYPE REF TO z_cl_date_calc,
        l_week_of_month TYPE int1.

  CREATE OBJECT r_date_calc
    EXPORTING
      im_date_value = sy-datum.

  CALL METHOD r_date_calc->week_of_month
    IMPORTING
      ex_week_of_month = l_week_of_month.

  WRITE: / '  WEEK_OF_MONTH (current date):', l_week_of_month.
ENDFORM.                    "VERIFY_WEEK_OF_MONTH

*----------------------------------------------------------------------*
* FORM: verify week of year method.                                    *
*----------------------------------------------------------------------*

FORM verify_week_of_year.
  DATA: r_date_calc    TYPE REF TO z_cl_date_calc,
        l_week_of_year TYPE int2.

  CREATE OBJECT r_date_calc
    EXPORTING
      im_date_value = sy-datum.

  CALL METHOD r_date_calc->week_of_year
    IMPORTING
      ex_week_of_year = l_week_of_year.

  WRITE: / '  WEEK_OF_YEAR (current date):', l_week_of_year.
ENDFORM.                    "VERIFY_WEEK_OF_YEAR

*----------------------------------------------------------------------*
* FORM: verify set format method.                                      *
*----------------------------------------------------------------------*

FORM verify_set_format.
  DATA: r_date_fmt TYPE REF TO z_cl_date_fmt.

  CREATE OBJECT r_date_fmt
    EXPORTING im_date_value = sy-datum.

  r_date_fmt->set_format( 'FM DD, CCYY' ).
ENDFORM.                    "VERIFY_SET_FORMAT

*----------------------------------------------------------------------*
* FORM: verify set letter case method.                                 *
*----------------------------------------------------------------------*

FORM verify_set_letter_case.
  DATA: r_date_fmt TYPE REF TO z_cl_date_fmt.

  CREATE OBJECT r_date_fmt
    EXPORTING im_date_value = sy-datum.

  r_date_fmt->set_letter_case( 'U' ).
ENDFORM.                    "VERIFY_SET_LETTER_CASE

*----------------------------------------------------------------------*
* FORM: verify get formatted date method.                              *
*----------------------------------------------------------------------*

FORM verify_get_formatted_date.
  DATA: r_date_fmt   TYPE REF TO z_cl_date_fmt,
        l_output(80) TYPE c.

  CREATE OBJECT r_date_fmt
    EXPORTING im_date_value = sy-datum.

  r_date_fmt->set_format( 'FM DD, CCYY' ).

  CALL METHOD r_date_fmt->get_formatted_date
    IMPORTING
      ex_date_string = l_output.

  WRITE: / '  GET_FORMATTED_DATE (format specification FM DD, CCYY):', l_output.
ENDFORM.                    "VERIFY_GET_FORMATTED_DATE
