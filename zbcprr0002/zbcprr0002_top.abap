
CONSTANTS: c_method_splitter TYPE c LENGTH 3 VALUE '==>'.

TYPE-POOLS: abap.

TYPES: BEGIN OF ty_screen,
         exdat     TYPE exdat,
         object    TYPE tadir-object,
         devclass  TYPE tadir-devclass,
       END OF ty_screen,
       BEGIN OF ty_tadir,
         pgmid    TYPE tadir-pgmid,
         object   TYPE tadir-object,
         obj_name TYPE tadir-obj_name,
         delflag  TYPE tadir-delflag,
       END OF ty_tadir,
       BEGIN OF ty_dictionary_status,
         status_key   TYPE zbcde0011, " F = Full Key ; P = Parcial continues ; N = Not used
         status_idx   TYPE zbcde0011, " F = Full Key ; P = Parcial continues ; N = Not used
         indexname    TYPE zbctm0001-indexname,
       END OF ty_dictionary_status.

TYPES: ty_t_tadir TYPE STANDARD TABLE OF ty_tadir.

DATA: t_objects       TYPE zbctt0006,
      t_messages      TYPE bapiret2_t,
      t_table_design  TYPE zbctt0007,
      t_zbctm0000     TYPE HASHED TABLE OF zbctm0000 WITH UNIQUE KEY obj_name id_ejection,
      t_zbctm0001     TYPE HASHED TABLE OF zbctm0001 WITH UNIQUE DEFAULT KEY.

DATA: w_screen TYPE ty_screen.

DATA: v_id_ejecution TYPE zbctm0000-id_ejection.

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.5.5 - E.G.Mellodew. 1998-2016. Sap Release 700
