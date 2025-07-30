CLASS zcl_test_json_transformation DEFINITION
  PUBLIC
  INHERITING FROM cl_xco_cp_adt_simple_classrun FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.

    METHODS main REDEFINITION.

  PRIVATE SECTION.

    DATA json TYPE REF TO if_xco_cp_json_data.

    METHODS boolean_to_abap_bool.
    METHODS camel_case_to_underscore.
    METHODS pascal_case_to_underscore.
    METHODS underscore_to_camel_case.
    METHODS underscore_to_pascal_case.

ENDCLASS.



CLASS zcl_test_json_transformation IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    json = xco_cp_json=>data->from_string( |\{| &&
                                           |  "bool": true,| &&
                                           |  "camelCase": "camelCase",| &&
                                           |  "PascalCase": "PascalCase",| &&
                                           |  "under_score": "under_score"| &&
                                           |\}| ).

  ENDMETHOD.


  METHOD main.

    boolean_to_abap_bool( ).
    out->write( `--------------------------------------------------` ).
    camel_case_to_underscore( ).
    out->write( `--------------------------------------------------` ).
    pascal_case_to_underscore( ).
    out->write( `--------------------------------------------------` ).
    underscore_to_camel_case( ).
    out->write( `--------------------------------------------------` ).
    underscore_to_pascal_case( ).

  ENDMETHOD.


  METHOD boolean_to_abap_bool.

    out->plain->write(
        data = json->apply( VALUE #( ( xco_cp_json=>transformation->boolean_to_abap_bool ) ) )->to_string( )
        name = `boolean_to_abap_bool` ).

  ENDMETHOD.


  METHOD camel_case_to_underscore.

    out->plain->write(
        data = json->apply( VALUE #( ( xco_cp_json=>transformation->camel_case_to_underscore ) ) )->to_string( )
        name = `camel_case_to_underscore` ).

  ENDMETHOD.


  METHOD pascal_case_to_underscore.

    out->plain->write(
        data = json->apply( VALUE #( ( xco_cp_json=>transformation->pascal_case_to_underscore ) ) )->to_string( )
        name = `pascal_case_to_underscore` ).

  ENDMETHOD.


  METHOD underscore_to_camel_case.

    out->plain->write(
        data = json->apply( VALUE #( ( xco_cp_json=>transformation->underscore_to_camel_case ) ) )->to_string( )
        name = `underscore_to_camel_case` ).

  ENDMETHOD.


  METHOD underscore_to_pascal_case.

    out->plain->write(
        data = json->apply( VALUE #( ( xco_cp_json=>transformation->underscore_to_pascal_case ) ) )->to_string( )
        name = `underscore_to_pascal_case` ).

  ENDMETHOD.


ENDCLASS.
