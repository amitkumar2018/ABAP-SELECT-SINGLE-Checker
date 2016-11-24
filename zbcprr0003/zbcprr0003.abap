*----------------------------------------------------------------------*
* Proy/Inc:     Upgrade Oracle
* Funcional:    Juan Sebastián Soto
* Técnico:      Juan Sebastián Soto
* Fecha:        12.08.2014
* Descripción:  Este programa toma los resultados de la ejecución de
*               ZBCTM0000.
* Empresa:      Atos
*----------------------------------------------------------------------*

REPORT  zbcprr0003.

INCLUDE zbcprr0003_top.
INCLUDE zbcprr0003_sel.
INCLUDE zbcprr0003_f01.
INCLUDE zbcprr0003_o01.
INCLUDE zbcprr0003_i01.

START-OF-SELECTION.

  PERFORM lee_errores.
  PERFORM cuenta_errores.

  IF ck_xls IS INITIAL.
    CALL SCREEN 0100.
  ELSE.
    PERFORM generar_informe.
  ENDIF.

*GUI Texts
*----------------------------------------------------------
* TT0100 --> &

*Text elements
*----------------------------------------------------------
* 001 Excel File (*.XLSX) *.XLSX
* 002 Graph - Quantity per Module
* 003 Col: QUAN
* M01 & issues objects founded
* T01 Errores each program


*Selection texts
*----------------------------------------------------------
* CK_XLS         Only show report
* P_ID D       .

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.5.5 - E.G.Mellodew. 1998-2016. Sap Release 700
