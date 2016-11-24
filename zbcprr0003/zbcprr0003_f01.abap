*&---------------------------------------------------------------------*
*&      Form  LEE_ERRORES
*&---------------------------------------------------------------------*
FORM lee_errores.

  SELECT *
  FROM zbctm0000
  INTO TABLE t_zbctm0000.

  IF p_id IS NOT INITIAL.
    DELETE t_zbctm0000 WHERE id_ejection NE p_id.
  ENDIF.

ENDFORM.                    " LEE_ERRORES

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_RESULTADOS
*&---------------------------------------------------------------------*
FORM mostrar_resultados.

  DATA: ol_cc   TYPE REF TO cl_gui_custom_container,
        ol_alv  TYPE REF TO cl_gui_alv_grid.

  DATA: tl_fieldcat TYPE lvc_t_fcat.

  DATA: vl_cant     TYPE i,
        vl_cant_c   TYPE c LENGTH 3,
        vl_message  TYPE string.

  vl_cant = LINES( t_zbctm0000 ).
  vl_cant_c = vl_cant.

  vl_message = text-m01.
  REPLACE FIRST OCCURRENCE OF '&' IN vl_message WITH vl_cant_c.
  MESSAGE vl_message TYPE 'S'.

  REPLACE '&' IN vl_message WITH vl_cant_c.
  MESSAGE vl_message TYPE 'S'.


  CREATE OBJECT ol_cc
    EXPORTING
      container_name              = 'CC_0100'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CREATE OBJECT ol_alv
    EXPORTING
      i_parent          = ol_cc
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  PERFORM fieldcat CHANGING tl_fieldcat.

  CALL METHOD ol_alv->set_table_for_first_display
    CHANGING
      it_outtab                     = t_salida
      it_fieldcatalog               = tl_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " MOSTRAR_RESULTADOS

*&---------------------------------------------------------------------*
*&      Form  FIELDCAT
*&---------------------------------------------------------------------*
FORM fieldcat  CHANGING pt_fieldcat TYPE lvc_t_fcat.

  DATA: ol_alv_magic TYPE REF TO zcl_alv.

  DATA: wl_salida TYPE ty_salida.

  CREATE OBJECT ol_alv_magic.

  CALL METHOD ol_alv_magic->fill_catalog_merge
    EXPORTING
      structure = wl_salida
      texts     = abap_true
      convexit  = abap_true
      tabname   = 'T_SALIDA'
    IMPORTING
      fieldcat  = pt_fieldcat.

ENDFORM.                    " FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  CUENTA_ERRORES
*&---------------------------------------------------------------------*
FORM cuenta_errores.

  DATA: tl_zbctm0001 TYPE STANDARD TABLE OF zbctm0001.

  DATA: wl_zbctm0001 TYPE zbctm0001,
        wl_salida    TYPE ty_salida.

  CHECK t_zbctm0000 IS NOT INITIAL.

  SELECT *
  FROM zbctm0001
  INTO TABLE tl_zbctm0001
  FOR ALL ENTRIES IN t_zbctm0000
  WHERE obj_name    EQ t_zbctm0000-obj_name
    AND id_ejection EQ t_zbctm0000-id_ejection.

  SORT tl_zbctm0001 BY obj_name    ASCENDING
                       id_ejection ASCENDING
                       counter     DESCENDING.

  DELETE ADJACENT DUPLICATES FROM tl_zbctm0001 COMPARING obj_name id_ejection.

  LOOP AT tl_zbctm0001 INTO wl_zbctm0001.

    wl_salida-cant     = wl_zbctm0001-counter.
    wl_salida-obj_name = wl_zbctm0001-obj_name.
    APPEND wl_salida TO t_salida.

  ENDLOOP.

ENDFORM.                    " CUENTA_ERRORES

*&---------------------------------------------------------------------*
*&      Form  GENERAR_INFORME
*&---------------------------------------------------------------------*
FORM generar_informe.

  DATA: ol_excel         TYPE REF TO zcl_excel,
        ol_worksheet     TYPE REF TO zcl_excel_worksheet,
        ol_cx_excel      TYPE REF TO zcx_excel.

  DATA: tl_cant_modulos  TYPE STANDARD TABLE OF ty_cant_modulos.

  DATA: wl_table_settings  TYPE zexcel_s_table_settings.

  DATA: vl_worksheet_title TYPE zexcel_sheet_title,
        vl_message         TYPE string.

  TRY.

      CREATE OBJECT ol_excel.

      ol_worksheet = ol_excel->get_active_worksheet( ).

      vl_worksheet_title = text-t01.
      ol_worksheet->set_title( vl_worksheet_title ).

      wl_table_settings-top_left_column = 'A'.
      wl_table_settings-top_left_row    = '1'.

      CALL METHOD ol_worksheet->bind_table
        EXPORTING
          ip_table          = t_salida
          is_table_settings = wl_table_settings.

      PERFORM consolidar_modulos CHANGING tl_cant_modulos.

      wl_table_settings-top_left_column = 'D'.
      wl_table_settings-top_left_row    = '1'.

      CALL METHOD ol_worksheet->bind_table
        EXPORTING
          ip_table          = tl_cant_modulos
          is_table_settings = wl_table_settings.

      PERFORM crear_torta USING tl_cant_modulos
                       CHANGING ol_excel.

      PERFORM grabar_excel USING ol_excel.

    CATCH zcx_excel INTO ol_cx_excel.

      vl_message = ol_cx_excel->if_message~get_text( ).

      MESSAGE vl_message TYPE 'A'.

  ENDTRY.

ENDFORM.                    " GENERAR_INFORME

*&---------------------------------------------------------------------*
*&      Form  GRABAR_EXCEL
*&---------------------------------------------------------------------*
FORM grabar_excel  USING po_excel TYPE REF TO zcl_excel.

  DATA: ol_writer          TYPE REF TO zif_excel_writer.

  DATA: tl_rawdata         TYPE solix_tab.

  DATA: vl_bytecount       TYPE i,
        vl_xdata           TYPE xstring,
        vl_filename        TYPE string,
        vl_path            TYPE string,
        vl_fullpath        TYPE string,
        vl_xlsx            TYPE string.

  CREATE OBJECT ol_writer TYPE zcl_excel_writer_2007.

  vl_xdata = ol_writer->write_file( po_excel ).

  tl_rawdata   = cl_bcs_convert=>xstring_to_solix( iv_xstring  = vl_xdata ).
  vl_bytecount = XSTRLEN( vl_xdata ).

  vl_xlsx = text-001.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      default_extension = vl_xlsx
      file_filter       = vl_xlsx
    CHANGING
      filename          = vl_filename
      path              = vl_path
      fullpath          = vl_fullpath.

  cl_gui_frontend_services=>gui_download( EXPORTING bin_filesize = vl_bytecount
                                                    filename     = vl_fullpath
                                                    filetype     = 'BIN'
                                           CHANGING data_tab     = tl_rawdata ).

ENDFORM.                    " GRABAR_EXCEL

*&---------------------------------------------------------------------*
*&      Form  CONSOLIDAR_MODULOS
*&---------------------------------------------------------------------*
FORM consolidar_modulos  CHANGING pt_cant_modulos TYPE ty_t_cant_modulos.

  TYPES: BEGIN OF tyl_tadir,
           devclass  TYPE tadir-devclass,
           obj_name  TYPE tadir-obj_name,
         END OF tyl_tadir.

  DATA: tl_tadir TYPE STANDARD TABLE OF tyl_tadir.

  DATA: wl_tadir        TYPE tyl_tadir,
        wl_cant_modulos TYPE ty_cant_modulos,
        wl_salida       TYPE ty_salida.

  LOOP AT t_salida INTO wl_salida.
    wl_tadir-obj_name = wl_salida-obj_name.
    APPEND wl_tadir TO tl_tadir.
  ENDLOOP.

  SELECT devclass obj_name
  FROM tadir
  INTO TABLE tl_tadir
  FOR ALL ENTRIES IN tl_tadir
  WHERE pgmid    EQ 'R3TR'
    AND obj_name EQ tl_tadir-obj_name.

  SORT tl_tadir BY devclass.

  LOOP AT tl_tadir INTO wl_tadir.

    REPLACE FIRST OCCURRENCE OF 'Z'  IN wl_tadir-devclass WITH ''.
    REPLACE FIRST OCCURRENCE OF 'PD' IN wl_tadir-devclass WITH ''.
    wl_cant_modulos-modulo = wl_tadir-devclass(2).

    CASE wl_cant_modulos-modulo.
      WHEN 'WM'.
        wl_cant_modulos-modulo = 'MM'.
      WHEN 'PM' OR 'QM'.
        wl_cant_modulos-modulo = 'PP'.
    ENDCASE.

    wl_cant_modulos-cant   = 1.
    COLLECT wl_cant_modulos INTO pt_cant_modulos.
    CLEAR wl_cant_modulos.

  ENDLOOP.

ENDFORM.                    " CONSOLIDAR_MODULOS

*&---------------------------------------------------------------------*
*&      Form  CREAR_TORTA
*&---------------------------------------------------------------------*
FORM crear_torta  USING    pt_cant_modulos TYPE ty_t_cant_modulos
                  CHANGING po_excel        TYPE REF TO zcl_excel.

  DATA: ol_worksheet  TYPE REF TO zcl_excel_worksheet,
        ol_drawing    TYPE REF TO zcl_excel_drawing,
        ol_pie        TYPE REF TO zcl_excel_graph_pie.

  DATA: wl_upper  TYPE zexcel_drawing_location,
        wl_lower  TYPE zexcel_drawing_location.

  DATA: vl_to      TYPE zexcel_cell_row,
        vl_sheet   TYPE zexcel_sheet_title,
        vl_sername TYPE string.

  vl_to = LINES( pt_cant_modulos ) + 1.

  CREATE OBJECT ol_pie.

  vl_sheet   = text-t01.
  vl_sername = text-003.

  CALL METHOD ol_pie->create_serie
    EXPORTING
      ip_order        = 0
      ip_sheet        = vl_sheet
      ip_lbl_from_col = 'D'
      ip_lbl_from_row = '2'
      ip_lbl_to_col   = 'D'
      ip_lbl_to_row   = vl_to
      ip_ref_from_col = 'E'
      ip_ref_from_row = '2'
      ip_ref_to_col   = 'E'
      ip_ref_to_row   = vl_to
      ip_sername      = vl_sername.

  " Set style
  ol_pie->set_style( zcl_excel_graph=>c_style_15 ).
  ol_pie->set_show_percent( '1' ).
  ol_pie->set_varycolor( '1' ).

  " Get active sheet (Pie sheet)
  ol_worksheet = po_excel->get_active_worksheet( ).

  vl_sheet = text-002.

  " Create global drawing, set type as pie chart, assign chart, set position and media type
  ol_drawing = ol_worksheet->excel->add_new_drawing(
                    ip_type  = zcl_excel_drawing=>type_chart
                    ip_title = vl_sheet ).

  ol_drawing->graph = ol_pie.
  ol_drawing->graph_type = zcl_excel_drawing=>c_graph_pie.

*  "Set chart position (anchor 2 cells)

  wl_upper-col = 4.
  wl_upper-row = vl_to + 1.

  wl_lower-row = 30.
  wl_lower-col = 12.
  ol_drawing->set_position2(
    EXPORTING
      ip_from   = wl_upper
      ip_to     = wl_lower ).

  ol_drawing->set_media(
    EXPORTING
      ip_media_type = zcl_excel_drawing=>c_media_type_xml ).

  ol_worksheet->add_drawing( ol_drawing ).

ENDFORM.                    " CREAR_TORTA

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.5.5 - E.G.Mellodew. 1998-2016. Sap Release 700
