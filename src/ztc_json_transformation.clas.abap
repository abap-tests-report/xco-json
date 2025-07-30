"! @testing ztc_json_transformation
CLASS ztc_json_transformation DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PUBLIC SECTION.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA json TYPE REF TO if_xco_cp_json_data.

    METHODS setup.

    METHODS boolean_to_abap_bool      FOR TESTING.
    METHODS camel_case_to_underscore  FOR TESTING.
    METHODS pascal_case_to_underscore FOR TESTING.
    METHODS underscore_to_camel_case  FOR TESTING.
    METHODS underscore_to_pascal_case FOR TESTING.

    METHODS boolean_and_camel_case    FOR TESTING.

ENDCLASS.



CLASS ztc_json_transformation IMPLEMENTATION.


  METHOD setup.

    json = xco_cp_json=>data->from_string( |\{| &&
                                           |  "bool": true,| &&
                                           |  "camelCase": "camelCase",| &&
                                           |  "PascalCase": "PascalCase",| &&
                                           |  "under_score": "under_score"| &&
                                           |\}| ).

  ENDMETHOD.


  METHOD boolean_to_abap_bool.

    cl_abap_unit_assert=>assert_equals(
        exp = `{"bool":"X","camelCase":"camelCase","PascalCase":"PascalCase","under_score":"under_score"}`
        act = json->apply( VALUE #( ( xco_cp_json=>transformation->boolean_to_abap_bool ) ) )->to_string( ) ).

  ENDMETHOD.


  METHOD camel_case_to_underscore.

    cl_abap_unit_assert=>assert_equals(
        exp = `{"bool":true,"camel_case":"camelCase","Pascal_case":"PascalCase","under_score":"under_score"}`
        act = json->apply( VALUE #( ( xco_cp_json=>transformation->camel_case_to_underscore ) ) )->to_string( ) ).

  ENDMETHOD.


  METHOD pascal_case_to_underscore.

    cl_abap_unit_assert=>assert_equals(
        exp = `{"bool":true,"camelCase":"camelCase","pascal_case":"PascalCase","under_score":"under_score"}`
        act = json->apply( VALUE #( ( xco_cp_json=>transformation->pascal_case_to_underscore ) ) )->to_string( ) ).

  ENDMETHOD.


  METHOD underscore_to_camel_case.

    cl_abap_unit_assert=>assert_equals(
        exp = `{"bool":true,"camelCase":"camelCase","PascalCase":"PascalCase","underScore":"under_score"}`
        act = json->apply( VALUE #( ( xco_cp_json=>transformation->underscore_to_camel_case ) ) )->to_string( ) ).

  ENDMETHOD.


  METHOD underscore_to_pascal_case.

    cl_abap_unit_assert=>assert_equals(
        exp = `{"bool":true,"camelCase":"camelCase","PascalCase":"PascalCase","UnderScore":"under_score"}`
        act = json->apply( VALUE #( ( xco_cp_json=>transformation->underscore_to_pascal_case ) ) )->to_string( ) ).

  ENDMETHOD.


  METHOD boolean_and_camel_case.

    DATA(actual) = json->apply( VALUE #( ( xco_cp_json=>transformation->boolean_to_abap_bool )
                                         ( xco_cp_json=>transformation->camel_case_to_underscore ) ) )->to_string( ).

    cl_abap_unit_assert=>assert_equals(
        exp = `{"bool":"X","camel_case":"camelCase","Pascal_case":"PascalCase","under_score":"under_score"}`
        act = actual ).

  ENDMETHOD.


ENDCLASS.
