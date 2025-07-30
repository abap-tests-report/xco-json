*&---------------------------------------------------------------------*
*& Report zjson_transform
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zjson_transform.

PARAMETERS p_path TYPE string LOWER CASE.

PARAMETERS p_bool TYPE abap_bool AS CHECKBOX.
PARAMETERS p_cc2u TYPE abap_bool AS CHECKBOX.
PARAMETERS p_pc2u TYPE abap_bool AS CHECKBOX.
PARAMETERS p_u2cc TYPE abap_bool AS CHECKBOX.
PARAMETERS p_u2pc TYPE abap_bool AS CHECKBOX.
PARAMETERS p_tstmp TYPE abap_bool AS CHECKBOX.

INCLUDE zjson_transformcls.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  lcl_process=>value_help_path( ).

START-OF-SELECTION.
  NEW lcl_process( )->run( ).
