
TYPE-POOLS: abap.

TYPES: BEGIN OF ty_salida,
         obj_name TYPE zbctm0001-obj_name,
         cant     TYPE zbcde0013,
       END OF ty_salida,
       BEGIN OF ty_cant_modulos,
         modulo   TYPE comt_cfgm_engine_trace_module,
         cant     TYPE zbcde0013,
       END OF ty_cant_modulos.

TYPES: ty_t_cant_modulos TYPE STANDARD TABLE OF ty_cant_modulos.

DATA: t_zbctm0000 TYPE STANDARD TABLE OF zbctm0000,
      t_salida    TYPE STANDARD TABLE OF ty_salida.

DATA: v_titulo TYPE syst-title.

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.5.5 - E.G.Mellodew. 1998-2016. Sap Release 700
