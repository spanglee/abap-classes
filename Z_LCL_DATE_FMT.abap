REPORT  z_lcl_date_fmt.

*----------------------------------------------------------------------*
* Z_CL_DATE_FMT class definition.                                      *
*----------------------------------------------------------------------*

CLASS z_cl_date_fmt DEFINITION INHERITING FROM z_cl_date.

*----------------------------------------------------------------------*
* PUBLIC SECTION                                                       *
*----------------------------------------------------------------------*

  PUBLIC SECTION.

    METHODS get_formatted_date
      EXPORTING
        ex_date_string TYPE c.

    METHODS set_format
      IMPORTING
        im_format_value TYPE c.

    METHODS set_letter_case
      IMPORTING
        im_letter_case_value TYPE c.

*----------------------------------------------------------------------*
* PROTECTED SECTION                                                    *
*----------------------------------------------------------------------*

  PROTECTED SECTION.

    DATA format_specification TYPE string VALUE 'MM-DD-CCYY'.

    DATA letter_case TYPE c VALUE 'M'.

    CONSTANTS delimiter TYPE c VALUE '~'.

*----------------------------------------------------------------------*
* PRIVATE SECTION                                                      *
*----------------------------------------------------------------------*
  PRIVATE SECTION.

    CLASS-METHODS get_month_name
      IMPORTING
        im_month_number TYPE int2
      EXPORTING
        ex_month_name TYPE c.

    CLASS-METHODS get_short_month_name
      IMPORTING
        im_month_number TYPE int2
      EXPORTING
        ex_month_name TYPE c.

    CLASS-METHODS replace_delimiter
      CHANGING
        ch_string TYPE c.

    CLASS-METHODS set_lower_case
      CHANGING
        ch_string TYPE c.

    CLASS-METHODS set_mixed_case
      CHANGING
        ch_string TYPE c.

    CLASS-METHODS set_upper_case
      CHANGING
        ch_string TYPE c.
ENDCLASS.                    "z_cl_date_fmt DEFINITION

*----------------------------------------------------------------------*
* Z_CL_DATE_FMT class implementation.                                  *
*----------------------------------------------------------------------*

CLASS z_cl_date_fmt IMPLEMENTATION.

*----------------------------------------------------------------------*
* GET_FORMATTED_DATE.                                                  *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* FORMAT_SPECIFICATION attribute is a date format specification string *
* containing the following date specifiers:                            *
*                                                                      *
*   MM  month number of the year                                       *
*   SM  short month name (or abbreviation)                             *
*   FM  full month name                                                *
*   DD  day number of the month                                        *
*   YY  year                                                           *
*   CC  century                                                        *
*                                                                      *
* Date specifiers can include the following separators:                *
*                                                                      *
*   -   dash                                                           *
*   /   slash                                                          *
*   ,   comma                                                          *
*       space                                                          *
*                                                                      *
* FORMAT_SPECIFICATION attribute examples: MM-DD-CCYY or FM DD,CCYY    *
*                                                                      *
* LETTER_CASE attribute is a flag indicating whether the month name    *
* should be either lower-case, upper-case or mixed-case:               *
*                                                                      *
*   L   lower-case                                                     *
*   U   upper-case                                                     *
*   M   mixed-case                                                     *
*----------------------------------------------------------------------*

  METHOD get_formatted_date.
    DATA: l_format(80)     TYPE c,
          l_count          TYPE i VALUE  0,
          l_c              TYPE c,
          l_month          TYPE int2,
          l_month_name(10) TYPE c,
          l_specifier(5)   TYPE c,
          l_output(80)     TYPE c.

    CLEAR l_specifier.
    CLEAR l_output.

    l_month = me->date+4(2).
    l_format = me->format_specification.

    WHILE l_count <= STRLEN( me->format_specification ).
      l_c = l_format+l_count(1).

      IF l_c = '-'.
        CONCATENATE l_output l_c INTO l_output.
      ELSEIF l_c = '/'.
        CONCATENATE l_output l_c INTO l_output.
      ELSEIF l_c = ','.
        CONCATENATE l_output l_c INTO l_output.
      ELSEIF l_c = space.
        CONCATENATE l_output me->delimiter INTO l_output.
      ELSE.
        CONCATENATE l_specifier l_c INTO l_specifier.
        IF l_specifier = 'MM'.
          CONCATENATE l_output date+4(2) INTO l_output.
          CLEAR l_specifier.
        ELSEIF l_specifier = 'FM'.
          CALL METHOD me->get_month_name
            EXPORTING
              im_month_number = l_month
            IMPORTING
              ex_month_name   = l_month_name.
          CONCATENATE l_output l_month_name INTO l_output.
          CLEAR l_specifier.
        ELSEIF l_specifier = 'SM'.
          CALL METHOD me->get_short_month_name
            EXPORTING
              im_month_number = l_month
            IMPORTING
              ex_month_name   = l_month_name.
          CONCATENATE l_output l_month_name INTO l_output.
          CLEAR l_specifier.
        ELSEIF l_specifier = 'DD'.
          CONCATENATE l_output date+6(2) INTO l_output.
          CLEAR l_specifier.
        ELSEIF l_specifier = 'YY'.
          CONCATENATE l_output date+2(2) INTO l_output.
          CLEAR l_specifier.
        ELSEIF l_specifier = 'CC'.
          CONCATENATE l_output date(2) INTO l_output.
          CLEAR l_specifier.
        ENDIF.
      ENDIF.

      l_count = l_count + 1.
    ENDWHILE.

    CALL METHOD me->replace_delimiter
      CHANGING
        ch_string = l_output.

    IF me->letter_case = 'L'.
      CALL METHOD me->set_lower_case
        CHANGING
          ch_string = l_output.
    ELSEIF me->letter_case = 'U'.
      CALL METHOD me->set_upper_case
        CHANGING
          ch_string = l_output.
    ENDIF.

    ex_date_string = l_output.
  ENDMETHOD.                    "GET_FORMATTED_DATE

*----------------------------------------------------------------------*
* SET_FORMAT                                                           *
*----------------------------------------------------------------------*

  METHOD set_format.
    DATA: l_len TYPE i.

    l_len = STRLEN( im_format_value ).
    IF l_len < 1 OR l_len > 15.
      RETURN.
    ENDIF.

    me->format_specification = im_format_value.
    TRANSLATE me->format_specification TO UPPER CASE.
  ENDMETHOD.                    "SET_FORMAT

*----------------------------------------------------------------------*
* SET_LETTER_CASE                                                      *
*----------------------------------------------------------------------*

  METHOD set_letter_case.
    DATA: l_letter_case TYPE c.

    IF im_letter_case_value IS INITIAL.
      l_letter_case = 'U'.
    ELSE.
      l_letter_case = im_letter_case_value.
      TRANSLATE l_letter_case TO UPPER CASE.
      IF l_letter_case NA 'LUM'.
        RETURN.
      ENDIF.
    ENDIF.

    me->letter_case = im_letter_case_value.
  ENDMETHOD.                    "SET_LETTER_CASE

*----------------------------------------------------------------------*
* GET_MONTH_NAME                                                       *
*----------------------------------------------------------------------*

  METHOD get_month_name.
    DATA: month_names TYPE t247.

    SELECT * FROM t247 INTO CORRESPONDING FIELDS OF month_names
      WHERE spras = sy-langu AND mnr = im_month_number.
    ENDSELECT.

    ex_month_name = month_names-ltx.
  ENDMETHOD.                    "GET_MONTH_NAME

*----------------------------------------------------------------------*
* GET_SHORT_MONTH_NAME                                                 *
*----------------------------------------------------------------------*

  METHOD get_short_month_name.
    DATA: month_names TYPE t247.

    SELECT * FROM t247 INTO CORRESPONDING FIELDS OF month_names
       WHERE spras = sy-langu AND mnr = im_month_number.
    ENDSELECT.

    ex_month_name = month_names-ktx.
  ENDMETHOD.                    "GET_SHORT_MONTH_NAME

*----------------------------------------------------------------------*
* REPLACE_DELIMITER                                                    *
*----------------------------------------------------------------------*

  METHOD replace_delimiter.
    IF STRLEN( ch_string ) > 0.
      sy-subrc = 0.
      WHILE sy-subrc = 0.
        REPLACE delimiter WITH space INTO ch_string.
      ENDWHILE.
    ENDIF.
  ENDMETHOD.                    "REPLACE_DELIMITER

*----------------------------------------------------------------------*
* SET_LOWER_CASE                                                       *
*----------------------------------------------------------------------*

  METHOD set_lower_case.
    IF STRLEN( ch_string ) > 0.
      TRANSLATE ch_string TO LOWER CASE.
    ENDIF.
  ENDMETHOD.                    "SET_LOWER_CASE

*----------------------------------------------------------------------*
* SET_MIXED_CASE                                                       *
*----------------------------------------------------------------------*

  METHOD set_mixed_case.
    DATA: l_start_letter TYPE c.

    IF STRLEN( ch_string ) > 0.
      l_start_letter = ch_string(1).
      TRANSLATE ch_string TO LOWER CASE.
    ENDIF.

    ch_string(1) = l_start_letter.
  ENDMETHOD.                    "SET_MIXED_CASE

*----------------------------------------------------------------------*
* SET_UPPER_CASE                                                       *
*----------------------------------------------------------------------*

  METHOD set_upper_case.
    IF STRLEN( ch_string ) > 0.
      TRANSLATE ch_string TO UPPER CASE.
    ENDIF.
  ENDMETHOD.                    "SET_UPPER_CASE
ENDCLASS.                    "z_cl_date_fmt IMPLEMENTATION
