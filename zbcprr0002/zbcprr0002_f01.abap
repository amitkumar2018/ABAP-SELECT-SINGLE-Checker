*&---------------------------------------------------------------------*
*&      Form  F4_SCREEN_FILE
*&---------------------------------------------------------------------*
*  P_FILE Parameter Match Code
*----------------------------------------------------------------------*
FORM f4_screen_file.

  DATA: tl_file_table TYPE filetable.

  DATA: wl_file LIKE LINE OF tl_file_table.

  DATA: vl_rc           TYPE i,
        vl_window_title TYPE string,
        vl_user_action  TYPE i.

  vl_window_title = text-t01.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = vl_window_title
    CHANGING
      file_table              = tl_file_table
      rc                      = vl_rc
      user_action             = vl_user_action
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CHECK vl_user_action EQ 0.
  CHECK vl_rc NE '-1'.

  READ TABLE tl_file_table INTO wl_file INDEX 1.
  p_file = wl_file-filename.


ENDFORM.                    " F4_SCREEN_FILE

*&---------------------------------------------------------------------*
*&      Form  start_from_file
*&---------------------------------------------------------------------*
*  Fill T_OBJECTS table from the input file
*----------------------------------------------------------------------*
FORM start_from_file.

* SS001 Read the input file.
  PERFORM read_file.

* SS001 Fill the object properties to know the method to read the source code.
  PERFORM fill_object_properties.

ENDFORM.                    "start_from_file

*&---------------------------------------------------------------------*
*&      Form  READ_FILE
*&---------------------------------------------------------------------*
*  Read input file with the object list
*----------------------------------------------------------------------*
FORM read_file.

  DATA: tl_file_content TYPE stringtab.

  DATA: wl_object TYPE LINE OF zbctt0006.

  DATA: vl_content TYPE string.

  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = p_file
    CHANGING
      data_tab                = tl_file_content
    EXCEPTIONS
      file_open_error         = 1
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
      OTHERS                  = 19.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* SS001 Fill the object list with the file content.
  LOOP AT tl_file_content INTO vl_content.
    CLEAR wl_object.
    wl_object-obj_name = vl_content.
    APPEND wl_object TO t_objects.
  ENDLOOP.

  SORT t_objects BY obj_name.
  DELETE ADJACENT DUPLICATES FROM t_objects COMPARING obj_name.

ENDFORM.                    " READ_FILE

*&---------------------------------------------------------------------*
*&      Form  FILL_OBJECT_PROPERTIES
*&---------------------------------------------------------------------*
* Fill the object properties to know the method to read the source code
*----------------------------------------------------------------------*
FORM fill_object_properties.

  DATA: tl_tadir   TYPE SORTED TABLE OF ty_tadir WITH UNIQUE KEY obj_name.

  DATA: wl_tadir   TYPE ty_tadir,
        wl_objects TYPE zbces0006.

  FIELD-SYMBOLS: <fsl_objects> TYPE LINE OF zbctt0006.

  CHECK t_objects IS NOT INITIAL.

  LOOP AT t_objects INTO wl_objects.
    CLEAR wl_tadir.
    wl_tadir-obj_name = wl_objects-obj_name(40).
    APPEND wl_tadir TO tl_tadir.
  ENDLOOP.

* SS001 Read the object type from the object name data (Function Modulo is not found here)
  SELECT pgmid object obj_name delflag
  FROM tadir
  INTO TABLE tl_tadir
  FOR ALL ENTRIES IN tl_tadir
  WHERE obj_name EQ tl_tadir-obj_name.

  CHECK sy-subrc EQ 0.

* SS001 Update the object type in the general table
  LOOP AT t_objects ASSIGNING <fsl_objects>.
    wl_tadir-obj_name = <fsl_objects>-obj_name.
    READ TABLE tl_tadir
    INTO wl_tadir
    WITH KEY obj_name = wl_tadir-obj_name.
    IF sy-subrc EQ 0.
      <fsl_objects>-pgmid  = wl_tadir-pgmid.
      <fsl_objects>-object = wl_tadir-object.
    ELSE.
      <fsl_objects>-pgmid  = 'R3TR'.
      <fsl_objects>-object = 'FUNC'.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " FILL_OBJECT_PROPERTIES

*&---------------------------------------------------------------------*
*&      Form  READ_SOURCE_CODE
*&---------------------------------------------------------------------*
* Read the source code of the objects
*----------------------------------------------------------------------*
FORM read_source_code.

  DATA: ol_exception TYPE REF TO zcx_bapiret2.

  DATA: tl_source    TYPE rsfb_source.

  DATA: wl_message   TYPE bapiret2.

  FIELD-SYMBOLS: <fsl_objects> TYPE LINE OF zbctt0006.

  LOOP AT t_objects ASSIGNING <fsl_objects>.

    TRY.

* SS001 Read the source code of the object. Programs and Function Module have diferent methods
        CASE <fsl_objects>-object.
          WHEN 'PROG'.
            PERFORM read_program_source_code USING <fsl_objects>-obj_name
                                          CHANGING tl_source.
          WHEN 'FUNC'.
            PERFORM read_funcion_source_code USING <fsl_objects>-obj_name
                                          CHANGING tl_source.
          WHEN 'METH'.
            PERFORM read_method_source_code  USING <fsl_objects>-obj_name
                                          CHANGING tl_source.
        ENDCASE.

* SS001 Replace commented lines and dead code
        PERFORM remove_comments CHANGING tl_source.

* SS001 Extract only the SELECT SINGLE statements of the source code
        PERFORM cut_source_code USING <fsl_objects>-object
                                      <fsl_objects>-obj_name
                                      tl_source
                             CHANGING <fsl_objects>-zresult.

      CATCH zcx_bapiret2 INTO ol_exception.

* SS001 Save the exception message in the log
        CALL METHOD ol_exception->get_message
          RECEIVING
            re_message = wl_message.

        APPEND wl_message TO t_messages.

    ENDTRY.

  ENDLOOP.

  DELETE t_objects WHERE zresult IS INITIAL.

ENDFORM.                    " READ_SOURCE_CODE

*&---------------------------------------------------------------------*
*&      Form  READ_PROGRAM_SOURCE_CODE
*&---------------------------------------------------------------------*
*  Read the Program (Main or Include) source code
*----------------------------------------------------------------------*
*      -->PV_OBJECT  Name of the object (Program or Include)
*      <--PT_SOURCE  Source Code
*----------------------------------------------------------------------*
FORM read_program_source_code  USING pv_object TYPE zbces0006-obj_name
                            CHANGING pt_source TYPE rsfb_source
                             RAISING zcx_bapiret2.

  DATA: wl_message TYPE bapiret2.

  READ REPORT pv_object INTO pt_source.
  CHECK sy-subrc NE 0.

  wl_message-type       = 'W'.
  wl_message-id         = 'DS'.
  wl_message-number     = 017.
  wl_message-message_v1 = pv_object.

  RAISE EXCEPTION TYPE zcx_bapiret2
  EXPORTING
    message = wl_message.

ENDFORM.                    " READ_PROGRAM_SOURCE_CODE

*&---------------------------------------------------------------------*
*&      Form  READ_FUNCION_SOURCE_CODE
*&---------------------------------------------------------------------*
*  Read the Function Module source code
*----------------------------------------------------------------------*
*      -->PV_OBJECT  Name of the object (Program or Include)
*      <--PT_SOURCE  Source Code
*----------------------------------------------------------------------*
FORM read_funcion_source_code  USING pv_object TYPE zbces0006-obj_name
                            CHANGING pt_source TYPE rsfb_source
                             RAISING zcx_bapiret2.

  DATA: tl_import_parameter   TYPE STANDARD TABLE OF rsimp,
        tl_changing_parameter TYPE STANDARD TABLE OF rscha,
        tl_export_parameter   TYPE STANDARD TABLE OF rsexp,
        tl_tables_parameter   TYPE STANDARD TABLE OF rstbl,
        tl_exception_list     TYPE STANDARD TABLE OF rsexc,
        tl_documentation      TYPE STANDARD TABLE OF rsfdo,
        tl_source             TYPE STANDARD TABLE OF rssource.

  DATA: wl_message TYPE bapiret2.

  DATA: vl_function TYPE rs38l-name.

  CLEAR pt_source.

  vl_function = pv_object.

  CALL FUNCTION 'RPY_FUNCTIONMODULE_READ_NEW'
    EXPORTING
      functionname       = vl_function
    TABLES
      import_parameter   = tl_import_parameter
      changing_parameter = tl_changing_parameter
      export_parameter   = tl_export_parameter
      tables_parameter   = tl_tables_parameter
      exception_list     = tl_exception_list
      documentation      = tl_documentation
      SOURCE             = tl_source
    CHANGING
      new_source         = pt_source
    EXCEPTIONS
      error_message      = 1
      function_not_found = 2
      invalid_name       = 3
      OTHERS             = 4.

  IF sy-subrc NE 0.

    wl_message-type       = 'W'.
    wl_message-id         = sy-msgid.
    wl_message-number     = sy-msgno.
    wl_message-message_v1 = sy-msgv1.
    wl_message-message_v2 = sy-msgv2.
    wl_message-message_v3 = sy-msgv3.
    wl_message-message_v4 = sy-msgv4.

    RAISE EXCEPTION TYPE zcx_bapiret2
    EXPORTING
      message = wl_message.

  ENDIF.

  IF pt_source IS INITIAL.
    pt_source = tl_source.
  ENDIF.

  CHECK sy-subrc NE 0.

  wl_message-type       = 'W'.
  wl_message-id         = sy-msgid.
  wl_message-number     = sy-msgno.
  wl_message-message_v1 = sy-msgv1.
  wl_message-message_v2 = sy-msgv2.
  wl_message-message_v3 = sy-msgv3.
  wl_message-message_v4 = sy-msgv4.

  RAISE EXCEPTION TYPE zcx_bapiret2
  EXPORTING
    message = wl_message.

ENDFORM.                    " READ_FUNCION_SOURCE_CODE
*&---------------------------------------------------------------------*
*&      Form  CUT_SOURCE_CODE
*&---------------------------------------------------------------------*
*  Keep the SELECT SINGLE matchs of the source code
*----------------------------------------------------------------------*
*      -->PV_OBJECT    Name of the object (Program or Include)
*      -->PV_OBJ_NAME  Name of the object (Program or Include)
*      <--PT_SOURCE    Source Code
*      <--PT_DBMATCHES Name of the database table and the source code line
*----------------------------------------------------------------------*
FORM cut_source_code  USING pv_object    TYPE zbces0006-object
                            pv_obj_name  TYPE zbces0006-obj_name
                            pt_source    TYPE rsfb_source
                   CHANGING pt_zresult   TYPE zbces0006-zresult
                    RAISING zcx_bapiret2.

  DATA: tl_select_single_results TYPE match_result_tab.

  DATA: wl_zresult       TYPE LINE OF zbctt0008,
        wl_source        TYPE LINE OF rsfb_source,
        wl_select_single TYPE LINE OF match_result_tab,
        wl_message       TYPE bapiret2.

* SS001 Find the SELECT SINGLE statements
  FIND ALL OCCURRENCES OF 'SELECT SINGLE' IN TABLE pt_source RESULTS tl_select_single_results.

  LOOP AT tl_select_single_results INTO wl_select_single.

    wl_zresult-line = wl_select_single-line.

* SS001 Extract the SELECT SINGLE statement
    LOOP AT pt_source INTO wl_source FROM wl_select_single-line.

      CONDENSE wl_source.

      CONCATENATE wl_zresult-sentence wl_source INTO wl_zresult-sentence SEPARATED BY space.

      FIND FIRST OCCURRENCE OF '.' IN wl_source IN CHARACTER MODE. " Find the end of the statement
      CHECK sy-subrc EQ 0.

      SET LOCALE LANGUAGE 'E'. " Set language at English... Programming without ñ...
      TRANSLATE wl_zresult-sentence TO UPPER CASE.
      CONDENSE wl_zresult-sentence.

      PERFORM extract_select_information USING wl_zresult-sentence
                                      CHANGING wl_zresult-dbmatches.

      APPEND wl_zresult TO pt_zresult.
      CLEAR wl_zresult.

      EXIT. " At found the end of the statement exit the in loop.
    ENDLOOP.

  ENDLOOP.

  CHECK pt_zresult IS INITIAL.

  wl_message-type       = 'S'.
  wl_message-id         = 'Z2'.
  wl_message-message_v1 = 'SELECT SINGLE'.
  wl_message-message_v2 = pv_obj_name.

  CASE pv_object.
    WHEN 'PROG'.
      wl_message-number     = 115.
    WHEN 'FUNC'.
      wl_message-number     = 116.
    WHEN 'METH'.
      wl_message-number     = 117.
      SPLIT pv_obj_name AT c_method_splitter INTO wl_message-message_v3 wl_message-message_v2.
  ENDCASE.

  RAISE EXCEPTION TYPE zcx_bapiret2
  EXPORTING
    message = wl_message.

ENDFORM.                    " CUT_SOURCE_CODE

*&---------------------------------------------------------------------*
*&      Form  EXTRACT_SELECT_INFORMATION
*&---------------------------------------------------------------------*
*  Extracte the table/s name and WHERE fieldnames from one SELECT statement
*----------------------------------------------------------------------*
*      -->PV_SOURCE_LINE  Source line with the statement
*      -->PT_SOURCE       Source Code
*      <--PT_DBMATCHES    Data Base tables or Views matches
*----------------------------------------------------------------------*
FORM extract_select_information  USING pv_source       TYPE string
                              CHANGING pt_dbmatches    TYPE zbces0008-dbmatches.

  DATA: tl_split      TYPE stringtab,
        tl_view_join  TYPE dd27p_tty.

  DATA: wl_dbmatches  TYPE LINE OF zbces0008-dbmatches,
        wl_view_join  TYPE dd27p.

  DATA: vl_split              TYPE string,
        vl_where_table_name   TYPE string,
        vl_garbage            TYPE string,
        vl_fieldname          TYPE fieldname,
        vl_table_tabix        TYPE i,
        vl_alias_tabix        TYPE i,
        vl_where_tabix        TYPE i,
        vl_tabclass           TYPE dd02l-tabclass.

  SPLIT pv_source AT space INTO TABLE tl_split IN CHARACTER MODE.

  LOOP AT tl_split INTO vl_split WHERE table_line EQ 'FROM'
                                    OR table_line EQ 'JOIN'.

    CLEAR wl_dbmatches.

* Get the table name
    vl_table_tabix = sy-tabix + 1.
    READ TABLE tl_split
    INTO vl_split
    INDEX vl_table_tabix.
    wl_dbmatches-tabname = vl_split.
    vl_where_table_name = vl_split.

    PERFORM get_table_class  USING vl_where_table_name
                          CHANGING vl_tabclass.
    CASE vl_tabclass.
      WHEN 'CLUSTER' OR 'POOL'.
        CONTINUE.
      WHEN 'VIEW'.
        PERFORM get_view_join USING vl_where_table_name
                           CHANGING tl_view_join.
    ENDCASE.

* Check if have alias
    vl_alias_tabix = vl_table_tabix + 1.
    READ TABLE tl_split
    INTO vl_split
    INDEX vl_alias_tabix.
    IF vl_split EQ 'AS'.
* Get the alias name for the WHERE
      vl_alias_tabix = vl_table_tabix + 2.
      READ TABLE tl_split
      INTO vl_split
      INDEX vl_alias_tabix.

      vl_where_table_name = vl_split. " Replace the original name cause are using an alias
    ENDIF.

    READ TABLE tl_split
    INTO vl_split
    WITH KEY table_line = 'WHERE'.
    CHECK sy-subrc EQ 0. " It is not obligatory the WHERE

    vl_where_tabix = sy-tabix + 1.

    WHILE sy-subrc EQ 0.

      READ TABLE tl_split
      INTO vl_split
      INDEX vl_where_tabix.
      CHECK sy-subrc EQ 0.

      vl_where_tabix = vl_where_tabix + 4.

      FIND FIRST OCCURRENCE OF '~' IN vl_split.
      IF sy-subrc EQ 0.
        SPLIT vl_split AT '~' INTO vl_garbage vl_fieldname.
      ELSE.
        vl_fieldname = vl_split.
        vl_garbage   = vl_where_table_name.
      ENDIF.

      sy-subrc = 0.

      IF vl_where_table_name NE vl_garbage. " If the alias name it isn't equal at the WHERE line, skip. But if the name it's the same, the name of the field is stored in VL_FIELDNAME
        CONTINUE.
      ENDIF.

      READ TABLE tl_view_join
      INTO wl_view_join
      WITH KEY viewfield = vl_fieldname.
      IF sy-subrc EQ 0.
        vl_fieldname         = wl_view_join-fieldname.
        wl_dbmatches-tabname = wl_view_join-tabname.
      ENDIF.

      APPEND vl_fieldname TO wl_dbmatches-fieldname.

    ENDWHILE.

    CHECK wl_dbmatches-fieldname IS NOT INITIAL.
    INSERT wl_dbmatches INTO TABLE pt_dbmatches.

  ENDLOOP.

ENDFORM.                    " EXTRACT_SELECT_INFORMATION

*&---------------------------------------------------------------------*
*&      Form  SHOW_LOG
*&---------------------------------------------------------------------*
*  Save and show the log
*----------------------------------------------------------------------*
FORM show_log.

  DATA: ol_log     TYPE REF TO zcl_log_message.

  DATA: tl_log     TYPE rsaos_t_message.

  CREATE OBJECT ol_log
    EXPORTING
      OBJECT          = 'ZBC_ABAP_SCANNER'
      subobject       = 'SELECT_SINGLE'
      aluser          = sy-uname
      altcode         = sy-tcode
      alprog          = sy-repid
      del_before      = abap_false
    EXCEPTIONS
      log_not_created = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  tl_log = zcl_log_message=>convert_message( im_input  = t_messages ).

  CALL METHOD ol_log->add_messages
    EXPORTING
      t_messages       = tl_log
    EXCEPTIONS
      log_not_found    = 1
      msg_inconsistent = 2
      log_is_full      = 3
      OTHERS           = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL METHOD ol_log->save_log
    EXCEPTIONS
      log_not_found    = 1
      save_not_allowed = 2
      numbering_error  = 3
      OTHERS           = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CHECK sy-batch IS INITIAL.

  CALL METHOD ol_log->display_log
    EXCEPTIONS
      profile_inconsistent = 1
      internal_error       = 2
      no_data_available    = 3
      no_authority         = 4
      OTHERS               = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " SHOW_LOG

*&---------------------------------------------------------------------*
*&      Form  READ_TABLES_DESIGN
*&---------------------------------------------------------------------*
* Read the table design including the indexes design
*----------------------------------------------------------------------*
FORM read_tables_design.

  DATA: wl_object       TYPE LINE OF zbctt0006,
        wl_dbmatches    TYPE LINE OF zbctt0005,
        wl_table_design TYPE LINE OF zbctt0007,
        wl_zresult      TYPE LINE OF zbctt0008.

  LOOP AT t_objects INTO wl_object.

    LOOP AT wl_object-zresult INTO wl_zresult.

      LOOP AT wl_zresult-dbmatches INTO wl_dbmatches.

        READ TABLE t_table_design WITH TABLE KEY tabname = wl_dbmatches-tabname TRANSPORTING NO FIELDS.
        CHECK sy-subrc NE 0.
        CLEAR wl_table_design.
        wl_table_design-tabname = wl_dbmatches-tabname.

        CLEAR wl_table_design-dd17v.

        CALL FUNCTION 'DDIF_TABL_GET'
          EXPORTING
            name          = wl_table_design-tabname
          TABLES
            dd03p_tab     = wl_table_design-dd03p
            dd12v_tab     = wl_table_design-dd12v
            dd17v_tab     = wl_table_design-dd17v
          EXCEPTIONS
            illegal_input = 1
            OTHERS        = 2.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

* Exclude the table if this do not have indexes
        CHECK wl_table_design-dd17v IS NOT INITIAL.

* Exclude the not key fields and MANDT fields
        DELETE wl_table_design-dd03p WHERE keyflag EQ abap_false.
        DELETE wl_table_design-dd03p WHERE fieldname EQ 'MANDT'.
        DELETE wl_table_design-dd17v WHERE fieldname EQ 'MANDT'.

        INSERT wl_table_design INTO TABLE t_table_design.

      ENDLOOP.

    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " READ_TABLES_DESIGN

*&---------------------------------------------------------------------*
*&      Form  DISCARD_SQL_ACCESS
*&---------------------------------------------------------------------*
*  Analize the WHERE access with the primary key and the indexes and
*  and discard the access that use de primary key o do not use an index
*----------------------------------------------------------------------*
FORM read_access_where.

  DATA: wl_object       TYPE LINE OF zbctt0006,
        wl_zresult      TYPE LINE OF zbctt0008,
        wl_dbmatches    TYPE LINE OF zbctt0005,
        wl_tabledesign  TYPE LINE OF zbctt0007,
        wl_status       TYPE ty_dictionary_status.

  LOOP AT t_objects INTO wl_object.
    LOOP AT wl_object-zresult INTO wl_zresult.
      LOOP AT wl_zresult-dbmatches INTO wl_dbmatches.

        CLEAR wl_status.

* SS001 Access to the table design (Primary Key and Indexes Design)
        READ TABLE t_table_design
        INTO wl_tabledesign
        WITH TABLE KEY tabname = wl_dbmatches-tabname.
        CHECK sy-subrc EQ 0.

        PERFORM check_primary_key USING wl_dbmatches-fieldname
                                        wl_tabledesign-dd03p
                               CHANGING wl_status-status_key.

        CHECK wl_status-status_key NE 'F'. " Continue if the Primary Key is not full used

        PERFORM check_index USING wl_dbmatches-fieldname
                                  wl_tabledesign-dd12v
                                  wl_tabledesign-dd17v
                         CHANGING wl_status-status_idx
                                  wl_status-indexname.

        CHECK wl_status-status_idx NE 'N'.

        PERFORM fill_result_table USING wl_object-obj_name
                                        wl_dbmatches-tabname
                                        wl_status-indexname
                                        wl_zresult-line.

      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " READ_ACCESS_WHERE

*&---------------------------------------------------------------------*
*&      Form  CHECK_PRIMARY_KEY
*&---------------------------------------------------------------------*
*  Check if this access will be use the primary key
*----------------------------------------------------------------------*
*      -->PT_USED_FIELDS   Table name with key WHERE fields used
*      -->PT_PRIMARY_KEY   Table primary key design of the table to check
*      <--PV_STATUS        Status of use. See _TOP declarations for help
*----------------------------------------------------------------------*
FORM check_primary_key  USING    pt_used_fiels    TYPE zbces0005-fieldname
                                 pt_primary_key   TYPE zbces0007-dd03p
                        CHANGING pv_status        TYPE ty_dictionary_status-status_key.

  DATA: ol_verificator TYPE REF TO data.

  DATA: tl_results     TYPE match_result_tab.

  DATA: wl_primary_key TYPE dd03p.

  DATA: vl_field_tabix  TYPE sy-tabix,
        vl_count_key    TYPE i.

  FIELD-SYMBOLS: <fsl_verificator> TYPE ANY. " Verification variable. Each field mark the status of use of one field

  vl_count_key = LINES( pt_primary_key ).

  CREATE DATA ol_verificator TYPE c LENGTH vl_count_key. " Create a dynamic variable with the length of the table key
  ASSIGN ol_verificator->* TO <fsl_verificator>.

  LOOP AT pt_primary_key INTO wl_primary_key.

    vl_field_tabix = sy-tabix - 1.

    READ TABLE pt_used_fiels
    WITH KEY table_line = wl_primary_key-fieldname
    TRANSPORTING NO FIELDS.
    CHECK sy-subrc EQ 0.

    <fsl_verificator>+vl_field_tabix(1) = abap_on. "Mark the key field used

  ENDLOOP.

  FIND ALL OCCURRENCES OF abap_on IN <fsl_verificator> RESULTS tl_results.

  PERFORM check_usability USING tl_results
                                vl_count_key
                       CHANGING pv_status.

ENDFORM.                    " CHECK_PRIMARY_KEY

*&---------------------------------------------------------------------*
*&      Form  GET_TABLE_CLASS
*&---------------------------------------------------------------------*
*  Get the class table ( TRANSP, CLUSTER, POOL, VIEW )
*----------------------------------------------------------------------*
*      -->PV_TABLENAME     Name of table
*      <--PV_TABLE_CLASS   Class of table
*----------------------------------------------------------------------*
FORM get_table_class  USING pv_tablename    TYPE string
                   CHANGING pv_table_class  TYPE dd02l-tabclass.

  DATA: vl_tablename TYPE dd02l-tabname.

  vl_tablename = pv_tablename.

  SELECT SINGLE tabclass
  FROM dd02l
  INTO pv_table_class
  WHERE tabname  EQ vl_tablename
    AND as4local EQ 'A' " The Active Version
    AND as4vers  EQ 0000.

ENDFORM.                    " GET_TABLE_CLASS

*&---------------------------------------------------------------------*
*&      Form  CHECK_INDEX
*&---------------------------------------------------------------------*
*  Check if an index is used and put the index name
*----------------------------------------------------------------------*
*      -->PT_USED_FIELDS   Table name with key WHERE fields used
*      -->PT_INDEXES       Table primary key design of the table to check
*      <--PV_STATUS        Status of use. See _TOP declarations for help
*      <--PV_INDEXNAME     Name of the used index
*----------------------------------------------------------------------*
FORM check_index  USING    pt_used_fields    TYPE zbces0005-fieldname
                           pt_indexes        TYPE zbces0007-dd12v
                           pt_indexes_format TYPE zbces0007-dd17v
                  CHANGING pv_status         TYPE ty_dictionary_status-status_idx
                           pv_indexname      TYPE zbctm0001-indexname.

  DATA: ol_verificator TYPE REF TO data.

  DATA: tl_results     TYPE match_result_tab.

  DATA: wl_index        TYPE dd12v,
        wl_index_format TYPE dd17v.

  DATA: vl_field_tabix  TYPE sy-tabix,
        vl_status       TYPE ty_dictionary_status-status_idx,
        vl_count_field  TYPE i.

  FIELD-SYMBOLS: <fsl_verificator> TYPE ANY. " Verification variable. Each field mark the status of use of one field

  LOOP AT pt_indexes INTO wl_index.

    CLEAR vl_count_field.
    LOOP AT pt_indexes_format INTO wl_index_format WHERE indexname EQ wl_index-indexname.
      vl_count_field = vl_count_field + 1.
    ENDLOOP.

    CREATE DATA ol_verificator TYPE c LENGTH vl_count_field. " Create a dynamic variable with the length of the index
    ASSIGN ol_verificator->* TO <fsl_verificator>.

    vl_field_tabix = -1.

    LOOP AT pt_indexes_format INTO wl_index_format WHERE indexname EQ wl_index-indexname.

      vl_field_tabix = vl_field_tabix + 1.

      READ TABLE pt_used_fields
      WITH KEY table_line = wl_index_format-fieldname
      TRANSPORTING NO FIELDS.
      CHECK sy-subrc EQ 0.

      <fsl_verificator>+vl_field_tabix(1) = abap_on. "Mark the key field used

    ENDLOOP.

    FIND ALL OCCURRENCES OF abap_on IN <fsl_verificator> RESULTS tl_results.

    PERFORM check_usability USING tl_results
                                  vl_count_field
                         CHANGING vl_status.

    CASE vl_status.
      WHEN 'F'. " If an index is used at full, do not search more.
        pv_status    = vl_status.
        pv_indexname = wl_index-indexname.
        EXIT.
      WHEN 'P'. " Keep the index name proposed for use.
        pv_status    = vl_status.
        pv_indexname = wl_index-indexname.
      WHEN 'N'.
        IF pv_status IS INITIAL. " If a previus index is a proposed, do not leave it
          pv_status = vl_status.
        ENDIF.
    ENDCASE.

  ENDLOOP.

ENDFORM.                    " CHECK_INDEX
*&---------------------------------------------------------------------*
*&      Form  CHECK_USABILITY
*&---------------------------------------------------------------------*
* Check the use result algorithm for primary key or index
*----------------------------------------------------------------------*
*      -->PT_RESULTS      Result of checks
*      -->PV_COUNT_FIELD  Count fields used en the WHERE clause
*      <--PV_STATUS
*----------------------------------------------------------------------*
FORM check_usability  USING    pt_results       TYPE match_result_tab
                               pv_count_field   TYPE i
                      CHANGING pv_status        TYPE zbcde0011.

  DATA: wl_result TYPE LINE OF match_result_tab.

  DATA: vl_field_used TYPE i.

* How to access with the primary key with a key of 3 fields
* XXX = Full Access
* XX0 = Partial Access
* X00 = Partial Access
* X0X = Not Access
* 0X0 = Not Access
* 000 = Not Access

* SS001 First check if the full key is used
  vl_field_used = LINES( pt_results ).
  IF vl_field_used EQ pv_count_field.
    pv_status = 'F'. " Primary Key full used!! Good boy, good boy :)
    EXIT.
  ENDIF.

* SS001 If have not 'X' in the verification variable, no one primary key fields are used
  IF pt_results IS INITIAL.
    pv_status = 'N'. " Primary Key not used
    EXIT.
  ENDIF.

* SS001 If the first primary key field is not used, the primary key is not used
  READ TABLE pt_results
  INTO wl_result
  INDEX 1.
  IF wl_result-offset NE 0.
    pv_status = 'N'. " Primary Key not used
    EXIT.
  ENDIF.

  IF LINES( pt_results ) EQ 1.
    pv_status = 'P'. " Primary key used partialy
    EXIT.
  ENDIF.

* SS001 Check if the primary keys are used continues but not all
  LOOP AT pt_results INTO wl_result FROM 2.
    wl_result-offset = wl_result-offset + 1.
    CHECK wl_result-offset NE sy-tabix.
    pv_status = 'P'. " Primary key used partialy
    EXIT.
  ENDLOOP.

ENDFORM.                    " CHECK_USABILITY

*&---------------------------------------------------------------------*
*&      Form  FILL_RESULT_TABLE
*&---------------------------------------------------------------------*
*  Fill the ZBCTM0000 & ZBCTM0001 Internal Tables
*----------------------------------------------------------------------*
*      -->PV_OBJ_NAME   Name of the object
*      -->PV_TABNAME    Name of the table
*      -->PV_INDEXNAME  Name of the index
*      -->PV_LINE       ABAP line number where is the statement
*----------------------------------------------------------------------*
FORM fill_result_table  USING    pv_obj_name    TYPE zbces0006-obj_name
                                 pv_tabname     TYPE zbces0005-tabname
                                 pv_indexname   TYPE ty_dictionary_status-indexname
                                 pv_line        TYPE zbces0008-line.

  DATA: tl_zbctm0001_this_program TYPE HASHED TABLE OF zbctm0001 WITH UNIQUE DEFAULT KEY.

  DATA: wl_zbctm0000 TYPE zbctm0000,
        wl_zbctm0001 TYPE zbctm0001.

  IF v_id_ejecution IS INITIAL.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '1'
        object                  = 'Z_SEL_SING'
      IMPORTING
        number                  = v_id_ejecution
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

  READ TABLE t_zbctm0000
  INTO wl_zbctm0000
  WITH TABLE KEY obj_name    = pv_obj_name
                 id_ejection = v_id_ejecution.
  IF sy-subrc NE 0.

    wl_zbctm0000-obj_name     = pv_obj_name.
    wl_zbctm0000-id_ejection  = v_id_ejecution.
    wl_zbctm0000-adate        = sy-datum.
    wl_zbctm0000-atime        = sy-uzeit.
    wl_zbctm0000-auser        = sy-uname.
    INSERT wl_zbctm0000 INTO TABLE t_zbctm0000.

  ENDIF.

  wl_zbctm0001-obj_name     = wl_zbctm0000-obj_name.
  wl_zbctm0001-id_ejection  = wl_zbctm0000-id_ejection.
  wl_zbctm0001-tabname      = pv_tabname.
  wl_zbctm0001-line         = pv_line.
  wl_zbctm0001-indexname    = pv_indexname.

  INSERT LINES OF t_zbctm0001 INTO TABLE tl_zbctm0001_this_program.
  DELETE tl_zbctm0001_this_program WHERE obj_name NE pv_obj_name.
  wl_zbctm0001-counter      = LINES( tl_zbctm0001_this_program ) + 1.
  INSERT wl_zbctm0001 INTO TABLE t_zbctm0001.

ENDFORM.                    " FILL_RESULT_TABLE

*&---------------------------------------------------------------------*
*&      Form  REMOVE_COMMENTS
*&---------------------------------------------------------------------*
*  Replace the comments with space
*----------------------------------------------------------------------*
*      <--PT_SOURCE  Source Code
*----------------------------------------------------------------------*
FORM remove_comments  CHANGING pt_source TYPE rsfb_source.

  DATA: vl_comment_position TYPE i.

  FIELD-SYMBOLS: <fsl_source> TYPE LINE OF rsfb_source.

  LOOP AT pt_source ASSIGNING <fsl_source> WHERE table_line IS NOT INITIAL.

    FIND FIRST OCCURRENCE OF '*' IN <fsl_source> MATCH OFFSET vl_comment_position.
    IF  sy-subrc            EQ 0
    AND vl_comment_position EQ 0. " Line commented with *
      CLEAR <fsl_source>.
      CONTINUE.
    ENDIF.

    FIND FIRST OCCURRENCE OF '"' IN <fsl_source> MATCH OFFSET vl_comment_position.
    IF  sy-subrc EQ 0.
      <fsl_source> = <fsl_source>(vl_comment_position).
    ENDIF.

  ENDLOOP.

ENDFORM.                    " REMOVE_COMMENTS

*&---------------------------------------------------------------------*
*&      Form  GET_VIEW_JOIN
*&---------------------------------------------------------------------*
*  Get the View Join Table
*----------------------------------------------------------------------*
*      -->PV_VIEW_NAME  Name of the view
*      <--PT_VIEW_JOIN  Join Structure
*----------------------------------------------------------------------*
FORM get_view_join  USING    pv_view_name   TYPE string
                    CHANGING pt_view_join   TYPE dd27p_tty.

  DATA: vl_view_name TYPE ddobjname.

  vl_view_name = pv_view_name.

  CLEAR pt_view_join.

  CALL FUNCTION 'DDIF_VIEW_GET'
    EXPORTING
      name          = vl_view_name
    TABLES
      dd27p_tab     = pt_view_join
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " GET_VIEW_JOIN

*&---------------------------------------------------------------------*
*&      Form  START_FROM_INPUT
*&---------------------------------------------------------------------*
*  Start the process from Development Class
*----------------------------------------------------------------------*
FORM start_from_input.

* SS001 Get the object list
  PERFORM get_object_list.

ENDFORM.                    " START_FROM_INPUT

*&---------------------------------------------------------------------*
*&      Form  GET_OBJECT_LIST
*&---------------------------------------------------------------------*
*  Get the object list from a Development Class
*----------------------------------------------------------------------*
FORM get_object_list.

  TYPE-POOLS: suni.

  TYPES: BEGIN OF tyl_tfdir,
           funcname TYPE tfdir-funcname,
           pname    TYPE tfdir-pname,
         END OF tyl_tfdir.

  DATA: tl_tadir TYPE ty_t_tadir.

  DATA: wl_object TYPE LINE OF zbctt0006,
        wl_tadir  TYPE ty_tadir.

* SS001 Read the content of the Development Class
  SELECT pgmid object obj_name delflag
  FROM tadir
  INTO TABLE tl_tadir
  WHERE devclass IN s_dvclas
    AND object   IN s_object.

* SS001 Remove deleted objects
  DELETE tl_tadir WHERE delflag EQ abap_true.
  DELETE tl_tadir WHERE object  NE 'PROG' AND object  NE 'FUGR' AND object NE 'CLAS'.

* SS001 Extract Function Modules and Subrutines includes from the Function Groups
  PERFORM break_down_function_group USING tl_tadir.

* SS001 Extract Methods from Classes
  PERFORM break_down_classes USING tl_tadir.

  DELETE tl_tadir WHERE object NE 'PROG'.

  LOOP AT tl_tadir INTO wl_tadir.
    wl_object-pgmid    = wl_tadir-pgmid.
    wl_object-object   = wl_tadir-object.
    wl_object-obj_name = wl_tadir-obj_name.
    APPEND wl_object TO t_objects.
  ENDLOOP.

  SORT t_objects BY object obj_name.
  DELETE ADJACENT DUPLICATES FROM t_objects COMPARING object obj_name.

ENDFORM.                    " GET_OBJECT_LIST

*&---------------------------------------------------------------------*
*&      Form  SAVE_RESULT_IN_TABLE
*&---------------------------------------------------------------------*
* Save the result in the ZBCTM0000 & ZBCTM0001 tables
*----------------------------------------------------------------------*
FORM save_result_in_table.

  INSERT zbctm0000 FROM TABLE t_zbctm0000.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    EXIT.
  ENDIF.

  INSERT zbctm0001 FROM TABLE t_zbctm0001.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    EXIT.
  ENDIF.

  COMMIT WORK AND WAIT.

ENDFORM.                    " SAVE_RESULT_IN_TABLE

*&---------------------------------------------------------------------*
*&      Form  BREAK_DOWN_FUNCTION_GROUP
*&---------------------------------------------------------------------*
* Extract Function Modules and Subrutines includes from the Function Groups
*----------------------------------------------------------------------*
*      -->PT_TADIR  Object List
*----------------------------------------------------------------------*
FORM break_down_function_group  USING pt_tadir TYPE ty_t_tadir.

  DATA: tl_func  TYPE STANDARD TABLE OF suni_funcstruc,
        tl_tadir TYPE ty_t_tadir.

  DATA: wl_tadir  TYPE ty_tadir,
        wl_func   TYPE suni_funcstruc,
        wl_object TYPE LINE OF zbctt0006.

  DATA: vl_group      TYPE rs38l-area,
        vl_incl_numb  TYPE c LENGTH 2.

  tl_tadir = pt_tadir.
  DELETE tl_tadir WHERE object NE 'FUGR'.

* SS001 By each Function Group, get the Function Modules and the Function Group includes
  LOOP AT tl_tadir INTO wl_tadir.

    vl_group = wl_tadir-obj_name.

    CLEAR tl_func.
    CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
      IMPORTING
        functab = tl_func
      CHANGING
        group   = vl_group
      EXCEPTIONS
        OTHERS  = 0.

    DELETE tl_func WHERE funcname(1) NE 'Z'.  " Only Z objects. Ex: Mantain view create two functions named TABLE*

    wl_object-pgmid   = wl_tadir-pgmid.
    wl_object-object  = 'FUNC'.
    LOOP AT tl_func INTO wl_func.
      wl_object-obj_name = wl_func-funcname.
      APPEND wl_object TO t_objects.
    ENDLOOP.

    wl_object-pgmid   = wl_tadir-pgmid.
    wl_object-object  = 'PROG'.

    DO 99 TIMES. " Max number

      vl_incl_numb = sy-index.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = vl_incl_numb
        IMPORTING
          output = vl_incl_numb.

      CONCATENATE 'L' wl_tadir-obj_name 'F' vl_incl_numb INTO wl_object-obj_name.
      CALL FUNCTION 'RS_PROGRAM_CHECK_NAME'
        EXPORTING
          progname                     = wl_object-obj_name(40)
        EXCEPTIONS
          database                     = 1
          database_selections          = 2
          function_group               = 3
          function_include             = 4
          no_customer_function_group   = 5
          no_customer_function_include = 6
          reserved_name                = 7
          reserved_name_customer       = 8
          string_error                 = 9
          string_warning               = 10
          string_length_error          = 11
          types_program                = 12
          reserved_name_menu           = 13
          OTHERS                       = 14.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      APPEND wl_object TO t_objects.

    ENDDO.

  ENDLOOP.

ENDFORM.                    " BREAK_DOWN_FUNCTION_GROUP

*&---------------------------------------------------------------------*
*&      Form  BREAK_DOWN_CLASSES
*&---------------------------------------------------------------------*
*  Extract Methods from Classes
*----------------------------------------------------------------------*
*      -->PT_TADIR  Object List
*----------------------------------------------------------------------*
FORM break_down_classes  USING pt_tadir TYPE ty_t_tadir.

  DATA: tl_tmdir    TYPE STANDARD TABLE OF tmdir,
        tl_tadir    TYPE ty_t_tadir.

  DATA: wl_tadir    TYPE ty_tadir,
        wl_object   TYPE LINE OF zbctt0006,
        wl_tmdir    TYPE tmdir.

  tl_tadir = pt_tadir.
  DELETE tl_tadir WHERE object NE 'CLAS'.

* SS001 By each Function Group, get the Function Modules and the Function Group includes
  LOOP AT tl_tadir INTO wl_tadir.

    wl_tmdir-classname = wl_tadir-obj_name.

    SELECT * " The TMDIR Table has a buffer by the first field (CLASSNAME)
    FROM tmdir
    INTO TABLE tl_tmdir
    WHERE classname EQ wl_tmdir-classname.

    DELETE tl_tmdir WHERE methodname IS INITIAL.

    LOOP AT tl_tmdir INTO wl_tmdir.
      CLEAR wl_object.

      wl_object-pgmid  = wl_tadir-pgmid.
      wl_object-object = 'METH'.

      CONCATENATE wl_tmdir-classname
                  wl_tmdir-methodname
      INTO wl_object-obj_name
      SEPARATED BY c_method_splitter.

      APPEND wl_object TO t_objects.

    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " BREAK_DOWN_CLASSES
*&---------------------------------------------------------------------*
*&      Form  READ_METHOD_SOURCE_CODE
*&---------------------------------------------------------------------*
*  Read the Method Source Code
*----------------------------------------------------------------------*
*      -->PV_OBJECT  Name of the object (Method)
*      <--PT_SOURCE  Source Code
*----------------------------------------------------------------------*
FORM read_method_source_code  USING  pv_object TYPE zbces0006-obj_name
                            CHANGING pt_source TYPE rsfb_source
                             RAISING zcx_bapiret2.

  TYPE-POOLS: seop.

  DATA: tl_includes  TYPE STANDARD TABLE OF seop_method_w_include.

  DATA: wl_message     TYPE bapiret2,
        wl_clskey      TYPE seoclskey,
        wl_cpdkey      TYPE seocpdkey,
        wl_includes    TYPE seop_method_w_include.

  DATA: vl_method_name TYPE seocpdname,
        vl_obj_name    TYPE zbces0006-obj_name.

  SPLIT pv_object AT c_method_splitter INTO wl_clskey-clsname vl_method_name.

  CALL FUNCTION 'SEO_CLASS_GET_METHOD_INCLUDES'
    EXPORTING
      clskey                       = wl_clskey
    IMPORTING
      includes                     = tl_includes
    EXCEPTIONS
      _internal_class_not_existing = 1
      OTHERS                       = 2.
  IF sy-subrc <> 0.

    wl_message-type       = 'W'.
    wl_message-id         = sy-msgid.
    wl_message-number     = sy-msgno.
    wl_message-message_v1 = sy-msgv1.
    wl_message-message_v2 = sy-msgv2.
    wl_message-message_v3 = sy-msgv3.
    wl_message-message_v4 = sy-msgv4.

    RAISE EXCEPTION TYPE zcx_bapiret2
    EXPORTING
      message = wl_message.

  ENDIF.

  wl_cpdkey-clsname = wl_clskey-clsname.
  wl_cpdkey-cpdname = vl_method_name.

  READ TABLE tl_includes
  INTO wl_includes
  WITH KEY cpdkey = wl_cpdkey.

  TRY.
      vl_obj_name = wl_includes-incname.
      PERFORM read_program_source_code USING vl_obj_name
                                    CHANGING pt_source.

    CATCH zcx_bapiret2.

      wl_message-type       = 'W'.
      wl_message-id         = 'OO'.
      wl_message-number     = 275.
      wl_message-message_v1 = wl_clskey-clsname.
      wl_message-message_v2 = vl_method_name.

      RAISE EXCEPTION TYPE zcx_bapiret2
      EXPORTING
        message = wl_message.

  ENDTRY.

ENDFORM.                    " READ_METHOD_SOURCE_CODE

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.5.5 - E.G.Mellodew. 1998-2016. Sap Release 700
