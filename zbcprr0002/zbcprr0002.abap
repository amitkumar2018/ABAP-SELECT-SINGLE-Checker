*----------------------------------------------------------------------*
* Proy/Inc:     Upgrade Oracle
* Funcional:    Juan Sebastián Soto
* Técnico:      Juan Sebastián Soto
* Fecha:        07.08.2014
* Descripción:  Este programa toma un listado de objetos y los analiza
*               en búsqueda de consumo de índices secundarios con la
*               utilización de la sentencia SELECT SINGLE
* Empresa:      Atos
*----------------------------------------------------------------------*


REPORT  zbcprr0002.

INCLUDE zbcprr0002_top.
INCLUDE zbcprr0002_sel.
INCLUDE zbcprr0002_f01.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  PERFORM f4_screen_file.

AT SELECTION-SCREEN.

  CASE abap_true.
    WHEN rb_file.
      CHECK p_file IS INITIAL.
      SET CURSOR FIELD 'P_FILE'.
      MESSAGE e197(ist_ebs).
*   No se han fijado los parámetros obligatorios
    WHEN rb_input.
      CHECK s_dvclas IS INITIAL.
      SET CURSOR FIELD 'S_DVCLAS-LOW'.
      MESSAGE e197(ist_ebs).
*   No se han fijado los parámetros obligatorios
  ENDCASE.

START-OF-SELECTION.

  CASE abap_true.
    WHEN rb_file.
      PERFORM start_from_file.
    WHEN rb_input.
      PERFORM start_from_input.
  ENDCASE.

  PERFORM read_source_code.
  PERFORM read_tables_design.
  PERFORM read_access_where.
  PERFORM save_result_in_table.
  PERFORM show_log.

*Text elements
*----------------------------------------------------------
* B01 Data Source
* B02 Selection Screen
* B03 Aditional parameters
* T01 Choose a file


*Selection texts
*----------------------------------------------------------
* RB_FILE         File
* RB_INPUT         Manual input
* P_FILE D       .
* S_DVCLAS D       .
* S_FROM D       .
* S_OBJECT D       .


*Messages
*----------------------------------------------------------
*
* Message class: IST_EBS
*197   Mandatory parameters are not set

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.5.5 - E.G.Mellodew. 1998-2016. Sap Release 700
