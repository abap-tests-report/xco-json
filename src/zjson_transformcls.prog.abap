*&---------------------------------------------------------------------*
*& Include zjson_transformcls
*&---------------------------------------------------------------------*

CLASS lcx_json DEFINITION INHERITING FROM cx_static_check FINAL.

  PUBLIC SECTION.

    CONSTANTS lcx_json TYPE sotr_conc VALUE '028C0ED2B5601ED78EB6F3368B1E4F9B' ##NO_TEXT.

    DATA error         TYPE string.
    DATA syst_at_raise TYPE syst.

    METHODS constructor
      IMPORTING textid        LIKE textid   OPTIONAL
                !previous     LIKE previous OPTIONAL
                !error        TYPE string   OPTIONAL
                syst_at_raise TYPE syst     OPTIONAL.

    CLASS-METHODS raise_text
      IMPORTING iv_text TYPE clike
      RAISING   lcx_json.

    CLASS-METHODS raise_symsg
      RAISING lcx_json.

    METHODS if_message~get_longtext REDEFINITION.
    METHODS if_message~get_text     REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.



CLASS lcx_json IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( textid   = textid
                        previous = previous ).
    IF textid IS INITIAL.
      me->textid = lcx_json.
    ENDIF.

    me->error         = error.
    me->syst_at_raise = syst_at_raise.

  ENDMETHOD.


  METHOD if_message~get_longtext.

    IF    me->error         IS NOT INITIAL
       OR me->syst_at_raise IS NOT INITIAL.
*--------------------------------------------------------------------*
* If message was supplied explicitly use this as longtext as well
*--------------------------------------------------------------------*
      result = get_text( ).
    ELSE.
*--------------------------------------------------------------------*
* otherwise use standard method to derive text
*--------------------------------------------------------------------*
      result = super->if_message~get_longtext( preserve_newlines = preserve_newlines ).
    ENDIF.
  ENDMETHOD.


  METHOD if_message~get_text.

    IF me->error IS NOT INITIAL.
*--------------------------------------------------------------------*
* If message was supplied explicitly use this
*--------------------------------------------------------------------*
      result = me->error.
    ELSEIF me->syst_at_raise IS NOT INITIAL.
*--------------------------------------------------------------------*
* If message was supplied by syst create messagetext now
*--------------------------------------------------------------------*
      MESSAGE ID syst_at_raise-msgid TYPE syst_at_raise-msgty NUMBER syst_at_raise-msgno
              WITH syst_at_raise-msgv1 syst_at_raise-msgv2 syst_at_raise-msgv3 syst_at_raise-msgv4
              INTO result.
    ELSE.
*--------------------------------------------------------------------*
* otherwise use standard method to derive text
*--------------------------------------------------------------------*
      result = super->if_message~get_text( ).
    ENDIF.
  ENDMETHOD.


  METHOD raise_symsg.
    RAISE EXCEPTION NEW lcx_json( syst_at_raise = syst ).
  ENDMETHOD.


  METHOD raise_text.
    RAISE EXCEPTION NEW lcx_json( error = iv_text ).
  ENDMETHOD.


ENDCLASS.



CLASS lcl_process DEFINITION FINAL.

  PUBLIC SECTION.

    CLASS-METHODS value_help_path.

    METHODS run.

  PRIVATE SECTION.

    METHODS read_file
      RETURNING VALUE(result) TYPE xstring
      RAISING   lcx_json.

    METHODS transform
      IMPORTING !input        TYPE xstring
      RETURNING VALUE(result) TYPE string.

ENDCLASS.



CLASS lcl_process IMPLEMENTATION.


  METHOD value_help_path.

    DATA lv_initial_path TYPE string.
    DATA lt_files        TYPE filetable.
    DATA lv_return_code  TYPE i.

    DATA(lt_dynpfields) = VALUE dynpread_t( ( fieldname = `P_PATH` ) ).

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname               = sy-repid
        dynumb               = sy-dynnr
      TABLES
        dynpfields           = lt_dynpfields
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        invalid_parameter    = 7
        undefind_error       = 8
        double_conversion    = 9
        stepl_not_found      = 10
        OTHERS               = 11.
    IF sy-subrc = 0.
      lv_initial_path = lt_dynpfields[ fieldname = `P_PATH` ]-fieldvalue.
    ENDIF.

    cl_gui_frontend_services=>file_open_dialog( EXPORTING  window_title            = 'Select JSON file'
                                                           file_filter             = 'JSON (*.json)|*.json'
                                                           initial_directory       = lv_initial_path
                                                           multiselection          = abap_false
                                                CHANGING   file_table              = lt_files
                                                           rc                      = lv_return_code
                                                EXCEPTIONS file_open_dialog_failed = 1
                                                           cntl_error              = 2
                                                           error_no_gui            = 3
                                                           not_supported_by_gui    = 4
                                                           OTHERS                  = 5 ).

    p_path = COND #( WHEN sy-subrc = 0 AND lt_files IS INITIAL
                     THEN lv_initial_path
                     ELSE lt_files[ 1 ]-filename ).

  ENDMETHOD.


  METHOD run.

    TRY.
        DATA(json) = read_file( ).
      CATCH lcx_json INTO DATA(json_exception).
        zcl_adu_messages=>create( )->add_exception( json_exception )->display_messages( ).
        RETURN.
    ENDTRY.

    DATA(json_transformed) = transform( json ).

    CALL TRANSFORMATION sjson2html
         SOURCE XML json_transformed
         RESULT XML DATA(lv_json_html).

    cl_abap_browser=>show_html( html_string = cl_abap_codepage=>convert_from( lv_json_html ) ).

  ENDMETHOD.


  METHOD read_file.

    DATA lv_filename    TYPE string.
    DATA lv_filelength  TYPE i.
    DATA lt_binary_data TYPE STANDARD TABLE OF x255 WITH NON-UNIQUE DEFAULT KEY.

    lv_filename = p_path.

    cl_gui_frontend_services=>gui_upload( EXPORTING  filename                = lv_filename
                                                     filetype                = 'BIN'         " We are basically working with zipped directories --> force binary read
                                          IMPORTING  filelength              = lv_filelength
                                          CHANGING   data_tab                = lt_binary_data
                                          EXCEPTIONS file_open_error         = 1
                                                     file_read_error         = 2
                                                     no_batch                = 3
                                                     gui_refuse_filetransfer = 4
                                                     invalid_type            = 5
                                                     no_authority            = 6
                                                     unknown_error           = 7
                                                     bad_data_format         = 8
                                                     header_not_allowed      = 9
                                                     separator_not_allowed   = 10
                                                     header_too_long         = 11
                                                     unknown_dp_error        = 12
                                                     access_denied           = 13
                                                     dp_out_of_memory        = 14
                                                     disk_full               = 15
                                                     dp_timeout              = 16
                                                     not_supported_by_gui    = 17
                                                     error_no_gui            = 18
                                                     OTHERS                  = 19 ).
    IF sy-subrc <> 0.
      lcx_json=>raise_text( 'A problem occurred when reading the file' ).
    ENDIF.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lv_filelength
      IMPORTING
        buffer       = result
      TABLES
        binary_tab   = lt_binary_data.

  ENDMETHOD.


  METHOD transform.

    DATA transformations TYPE sxco_t_json_transformations.

    DATA(json) = xco_cp_json=>data->from_string( xco_cp=>xstring( input
                                        )->as_string( xco_cp_character=>code_page->utf_8
                                        )->value ).

    IF p_bool = abap_true.
      INSERT xco_cp_json=>transformation->boolean_to_abap_bool INTO TABLE transformations.
    ENDIF.

    IF p_cc2u = abap_true.
      INSERT xco_cp_json=>transformation->camel_case_to_underscore INTO TABLE transformations.
    ENDIF.

    IF p_pc2u = abap_true.
      INSERT xco_cp_json=>transformation->pascal_case_to_underscore INTO TABLE transformations.
    ENDIF.

    IF p_u2cc = abap_true.
      INSERT xco_cp_json=>transformation->underscore_to_camel_case INTO TABLE transformations.
    ENDIF.

    IF p_u2pc = abap_true.
      INSERT xco_cp_json=>transformation->underscore_to_pascal_case INTO TABLE transformations.
    ENDIF.

    IF p_tstmp = abap_true.
      INSERT NEW /rhp/cl_json_trans_timestamp( ) INTO TABLE transformations.
    ENDIF.

    RETURN COND #( WHEN transformations IS NOT INITIAL
                   THEN json->apply( transformations )->to_string( )
                   ELSE json->to_string( ) ).

  ENDMETHOD.


ENDCLASS.
