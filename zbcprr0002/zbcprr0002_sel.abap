
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.
PARAMETERS: rb_file  RADIOBUTTON GROUP src,
            rb_input RADIOBUTTON GROUP src DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE text-b02.
PARAMETERS:     p_file   TYPE zbcde0008 LOWER CASE.
SELECT-OPTIONS: s_dvclas FOR w_screen-devclass NO INTERVALS NO-EXTENSION,
                s_object FOR w_screen-object.
SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE text-b03.
SELECT-OPTIONS: s_from   FOR w_screen-exdat.
SELECTION-SCREEN END OF BLOCK b03.

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.5.5 - E.G.Mellodew. 1998-2016. Sap Release 700
