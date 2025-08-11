      *****************************************************
      *                                                   *
      *   PROGRAMA MENU CICS-DB2 SISTEMA BANCARIO         *
      *                                                   *
      *****************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID. MOVSCOB.
       AUTHOR. CAMILO LOPEZ.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA .
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *****EXEC SQL INCLUDE SQLCA END-EXEC.
        01 SQLCA.
           05 SQLCAID     PIC X(8).
           05 SQLCABC     PIC S9(9) COMP-4.
           05 SQLCODE     PIC S9(9) COMP-4.
           05 SQLERRM.
              49 SQLERRML PIC S9(4) COMP-4.
              49 SQLERRMC PIC X(70).
           05 SQLERRP     PIC X(8).
           05 SQLERRD     OCCURS 6 TIMES
                          PIC S9(9) COMP-4.
           05 SQLWARN.
              10 SQLWARN0 PIC X.
              10 SQLWARN1 PIC X.
              10 SQLWARN2 PIC X.
              10 SQLWARN3 PIC X.
              10 SQLWARN4 PIC X.
              10 SQLWARN5 PIC X.
              10 SQLWARN6 PIC X.
              10 SQLWARN7 PIC X.
           05 SQLEXT.
              10 SQLWARN8 PIC X.
              10 SQLWARN9 PIC X.
              10 SQLWARNA PIC X.
              10 SQLSTATE PIC X(5).
      *****EXEC SQL INCLUDE TATRANS END-EXEC.
      ******************************************************************
      * DCLGEN TABLE(TATRANS)                                          *
      *        LIBRARY(BANCO1.SISTEMA.DCLGEN(TATRANS))                 *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(CL-)                                              *
      *        STRUCTURE(CL-ESTTRANS)                                  *
      *        QUOTE                                                   *
      *        DBCSDELIM(NO)                                           *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
      *****EXEC SQL DECLARE TATRANS TABLE
      *****( ID_TRANSACTION                 INTEGER NOT NULL,
      *****  NUMERO_CUENTA_T                CHAR(10) NOT NULL,
      *****  TIPO_TRANSACCION               CHAR(1) NOT NULL,
      *****  MONTO                          DECIMAL(15, 2),
      *****  FECHA_HORA                     TIMESTAMP NOT NULL
      *****) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TATRANS                            *
      ******************************************************************
       01  CL-ESTTRANS.
      *                       ID_TRANSACTION
           10 CL-ID-TRANSACTION    PIC S9(9) USAGE COMP.
      *                       NUMERO_CUENTA_T
           10 CL-NUMERO-CUENTA-T   PIC X(10).
      *                       TIPO_TRANSACCION
           10 CL-TIPO-TRANSACCION  PIC X(1).
      *                       MONTO
           10 CL-MONTO             PIC S9(13)V9(2) USAGE COMP-3.
      *                       FECHA_HORA
           10 CL-FECHA-HORA        PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
      *
      *--- DECLARACION DE CURSOR PARA LEER SECUENCIALMENTE LA TABLA
      *--- TATRANS
      *
      *****EXEC SQL
      *****     DECLARE C_TATRANS CURSOR FOR
      *****     SELECT
      *****        TIPO_TRANSACCION
      *****       ,MONTO
      *****       ,FECHA_HORA
      *****       ,NUMERO_CUENTA_T
      *****     FROM
      *****        TATRANS
      *****     WHERE
      *****        NUMERO_CUENTA_T = :CL-NUMERO-CUENTA-T
      *****END-EXEC.

       01  SWITCHES.
           03  WS-PRIMERA-FALG           PIC X          VALUE 'N'.
               88 WS-PRIMERA-VEZ                        VALUE 'Y'.
           03 WS-PRFORM                  PIC X          VALUE 'N'.
               88 WS-EXIT-PERFORM                       VALUE 'Y'.
           03 WS-CONTINUAR               PIC X          VALUE 'N'.
               88 WS-EXIT                               VALUE 'Y'.
       01 DB2-ERROR.
          05 DB2-SQLCODE                 PIC S9(9).
          05 DB2-SQLCODE-Z               PIC -ZZZZZZZZ9.
          05 DB2-ERROR-MSG.
             06 DB2-ERR-MSG              PIC X(40).
             06 DB2-ERR-CODE             PIC X(20).
       01 WC-CONSTANTES.
          03 WC-PROGRAMA                 PIC X(8)       VALUE 'MOVSCOB'.
          03 WC-TRANSACCION              PIC X(4)       VALUE 'MOVS'.
          03 WC-CANTIDAD-TRANSACCIONES   PIC S9(9)      COMP-5.
       01  WS-PAGINACION.
          03  WS-INDEX                   PIC 9(2)       VALUE 1.
          03  WS-NUM-PAG                 PIC 9(1).
          03  WS-REGISTROS-TOTALES       PIC 9(2)       VALUE 0.
          03  WS-FILAS-PAG               PIC 9(2)       VALUE 5.
          03  WS-RESTO                   PIC 9(2).
          03  WS-PAG-ACTUAL              PIC 9(1).
          03  WS-PAG-INI                 PIC 9(2).
          03  WS-PAG-FIN                 PIC 9(2).
          03  WS-REL-COUNT               PIC 9(2).
       01  WS-TATRANS-DATA.
          03  WS-NUM-CUENTA       OCCURS 20 TIMES PIC X(10).
          03  WS-TIPO-TRANS       OCCURS 20 TIMES PIC X(1).
          03  WS-MONTO            OCCURS 20 TIMES
                                  PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZZ,ZZ.
          03  WS-FECHA-HORA       OCCURS 20 TIMES PIC X(26).
       01 WS-VARIABLES.
          03  WS-MONTO-VIEW                 PIC X(21).
          03  WS-INDICE                     PIC 9(2).
          03  WS-COUNT                      PIC 9(2).
          03  WS-MONTO-JUST                 PIC X(21).
          03  WS-FECHA-HORA-A               PIC X(26).
          03  WS-FECHA                      PIC X(10).
          03  WS-HORA                       PIC X(8).

       COPY MOVSMPCP.
       COPY DFHAID.
       COPY DDCICS.

        77 SQL-TEMP      PIC X(128).
        77 DSN-TEMP      PIC S9(9)  COMP-4.
        77 DSN-TMP2      PIC S9(18) COMP-3.
        77 DSNNROWS      PIC S9(9)  COMP-4.
        77 DSNNTYPE      PIC S9(4)  COMP-4.
        77 DSNNLEN       PIC S9(4)  COMP-4.
        77 SQL-NULL      PIC S9(9) COMP-4 VALUE +0.
        77 SQL-INIT-FLAG PIC S9(4) COMP-4 VALUE +0.
           88 SQL-INIT-DONE VALUE +1.
        77 SQL-FILE-READ      PIC S9(9) COMP-4 VALUE +2.
        77 SQL-FILE-CREATE    PIC S9(9) COMP-4 VALUE +8.
        77 SQL-FILE-OVERWRITE PIC S9(9) COMP-4 VALUE +16.
        77 SQL-FILE-APPEND    PIC S9(9) COMP-4 VALUE +32.
        01 SQL-PLIST3.
           05 SQL-PLIST-CON   PIC S9(9) COMP-4 VALUE +4210688.
           05 SQL-CALLTYPE    PIC S9(4) COMP-4 VALUE +50.
           05 SQL-PROG-NAME   PIC X(8)  VALUE X'4D4F5653434F4220'.
           05 SQL-TIMESTAMP-1 PIC S9(9) COMP-4 VALUE +472535647.
           05 SQL-TIMESTAMP-2 PIC S9(9) COMP-4 VALUE +274244544.
           05 SQL-SECTION     PIC S9(4) COMP-4 VALUE +1.
           05 SQL-CODEPTR     PIC S9(9) COMP-4.
           05 SQL-VPARMPTR    PIC S9(9) COMP-4 VALUE +0.
           05 SQL-APARMPTR    PIC S9(9) COMP-4 VALUE +0.
           05 FILLER          PIC S9(4) COMP-4 VALUE +1208.
           05 SQL-STMT-TYPE   PIC S9(4) COMP-4 VALUE +3.
           05 SQL-STMT-NUM    PIC S9(9) COMP-4 VALUE +231.
           05 SQL-PLIST-FLG   PIC S9(4) COMP-4 VALUE +0.
           05 FILLER          PIC X(18) VALUE
              X'000000000000000000000000000000000000'.
           05 SQL-PVAR-LIST3.
              10 PRE-SQLDAID  PIC X(8)  VALUE 'SQLDA  �'.
              10 PRE-SQLDABC  PIC S9(9) COMP-4 VALUE +60.
              10 PRE-SQLN     PIC S9(4) COMP-4 VALUE +1.
              10 PRE-SQLLD    PIC S9(4) COMP-4 VALUE +1.
              10 PRE-SQLVAR.
                12 SQLVAR-BASE1.
                  15 SQL-PVAR-TYPE1      PIC S9(4) COMP-4 VALUE +452.
                  15 SQL-PVAR-LEN1       PIC S9(4) COMP-4 VALUE +10.
                  15 SQL-PVAR-ADDRS1.
                     20 SQL-PVAR-ADDR1   PIC S9(9) COMP-4.
                     20 SQL-PVAR-IND1    PIC S9(9) COMP-4.
                  15 SQL-PVAR-NAME1.
                     20 SQL-PVAR-NAMEL1  PIC S9(4) COMP-4 VALUE +0.
                     20 SQL-PVAR-NAMEC1  PIC X(30) VALUE ' '.
        01 SQL-PLIST4.
           05 SQL-PLIST-CON   PIC S9(9) COMP-4 VALUE +4194304.
           05 SQL-CALLTYPE    PIC S9(4) COMP-4 VALUE +45.
           05 SQL-PROG-NAME   PIC X(8)  VALUE X'4D4F5653434F4220'.
           05 SQL-TIMESTAMP-1 PIC S9(9) COMP-4 VALUE +472535647.
           05 SQL-TIMESTAMP-2 PIC S9(9) COMP-4 VALUE +274244544.
           05 SQL-SECTION     PIC S9(4) COMP-4 VALUE +1.
           05 SQL-CODEPTR     PIC S9(9) COMP-4.
           05 SQL-VPARMPTR    PIC S9(9) COMP-4 VALUE +0.
           05 SQL-APARMPTR    PIC S9(9) COMP-4 VALUE +0.
           05 FILLER          PIC S9(4) COMP-4 VALUE +1208.
           05 SQL-STMT-TYPE   PIC S9(4) COMP-4 VALUE +5.
           05 SQL-STMT-NUM    PIC S9(9) COMP-4 VALUE +249.
           05 SQL-PLIST-FLG   PIC S9(4) COMP-4 VALUE +0.
           05 FILLER          PIC X(18) VALUE
              X'000000000000000000000000000000000000'.
        01 SQL-PLIST5.
           05 SQL-PLIST-CON   PIC S9(9) COMP-4 VALUE +4195328.
           05 SQL-CALLTYPE    PIC S9(4) COMP-4 VALUE +30.
           05 SQL-PROG-NAME   PIC X(8)  VALUE X'4D4F5653434F4220'.
           05 SQL-TIMESTAMP-1 PIC S9(9) COMP-4 VALUE +472535647.
           05 SQL-TIMESTAMP-2 PIC S9(9) COMP-4 VALUE +274244544.
           05 SQL-SECTION     PIC S9(4) COMP-4 VALUE +1.
           05 SQL-CODEPTR     PIC S9(9) COMP-4.
           05 SQL-VPARMPTR    PIC S9(9) COMP-4 VALUE +0.
           05 SQL-APARMPTR    PIC S9(9) COMP-4 VALUE +0.
           05 FILLER          PIC S9(4) COMP-4 VALUE +1208.
           05 SQL-STMT-TYPE   PIC S9(4) COMP-4 VALUE +4.
           05 SQL-STMT-NUM    PIC S9(9) COMP-4 VALUE +428.
           05 SQL-PLIST-FLG   PIC S9(4) COMP-4 VALUE +0.
           05 FILLER          PIC X(18) VALUE
              X'000000000000000000000000000000000000'.
           05 SQL-AVAR-LIST5.
              10 PRE-SQLDAID  PIC X(8)  VALUE 'SQLDA  �'.
              10 PRE-SQLDABC  PIC S9(9) COMP-4 VALUE +192.
              10 PRE-SQLN     PIC S9(4) COMP-4 VALUE +4.
              10 PRE-SQLLD    PIC S9(4) COMP-4 VALUE +4.
              10 PRE-SQLVAR.
                12 SQLVAR-BASE1.
                  15 SQL-AVAR-TYPE1      PIC S9(4) COMP-4 VALUE +452.
                  15 SQL-AVAR-LEN1       PIC S9(4) COMP-4 VALUE +1.
                  15 SQL-AVAR-ADDRS1.
                     20 SQL-AVAR-ADDR1   PIC S9(9) COMP-4.
                     20 SQL-AVAR-IND1    PIC S9(9) COMP-4.
                  15 SQL-AVAR-NAME1.
                     20 SQL-AVAR-NAMEL1  PIC S9(4) COMP-4 VALUE +0.
                     20 SQL-AVAR-NAMEC1  PIC X(30) VALUE ' '.
                12 SQLVAR-BASE2.
                  15 SQL-AVAR-TYPE2      PIC S9(4) COMP-4 VALUE +484.
                  15 SQL-AVAR-LEN2       PIC S9(4) COMP-4 VALUE +3842.
                  15 SQL-AVAR-ADDRS2.
                     20 SQL-AVAR-ADDR2   PIC S9(9) COMP-4.
                     20 SQL-AVAR-IND2    PIC S9(9) COMP-4.
                  15 SQL-AVAR-NAME2.
                     20 SQL-AVAR-NAMEL2  PIC S9(4) COMP-4 VALUE +0.
                     20 SQL-AVAR-NAMEC2  PIC X(30) VALUE ' '.
                12 SQLVAR-BASE3.
                  15 SQL-AVAR-TYPE3      PIC S9(4) COMP-4 VALUE +452.
                  15 SQL-AVAR-LEN3       PIC S9(4) COMP-4 VALUE +26.
                  15 SQL-AVAR-ADDRS3.
                     20 SQL-AVAR-ADDR3   PIC S9(9) COMP-4.
                     20 SQL-AVAR-IND3    PIC S9(9) COMP-4.
                  15 SQL-AVAR-NAME3.
                     20 SQL-AVAR-NAMEL3  PIC S9(4) COMP-4 VALUE +0.
                     20 SQL-AVAR-NAMEC3  PIC X(30) VALUE ' '.
                12 SQLVAR-BASE4.
                  15 SQL-AVAR-TYPE4      PIC S9(4) COMP-4 VALUE +452.
                  15 SQL-AVAR-LEN4       PIC S9(4) COMP-4 VALUE +10.
                  15 SQL-AVAR-ADDRS4.
                     20 SQL-AVAR-ADDR4   PIC S9(9) COMP-4.
                     20 SQL-AVAR-IND4    PIC S9(9) COMP-4.
                  15 SQL-AVAR-NAME4.
                     20 SQL-AVAR-NAMEL4  PIC S9(4) COMP-4 VALUE +0.
                     20 SQL-AVAR-NAMEC4  PIC X(30) VALUE ' '.

       LINKAGE SECTION.
       01 DFHCOMMAREA                    PIC X(40).

       PROCEDURE DIVISION.
       DSNSQL SECTION.
       SQL-SKIP.
           GO TO SQL-INIT-END.
       SQL-INITIAL.
           MOVE 1 TO SQL-INIT-FLAG.
           CALL 'DSNHADDR' USING SQL-VPARMPTR OF SQL-PLIST3 SQL-PVAR-LIS
      -    T3.
           CALL 'DSNHADD2' USING SQL-PVAR-ADDRS1 IN
           SQL-PVAR-LIST3 CL-NUMERO-CUENTA-T OF CL-ESTTRANS SQL-NULL
           CALL 'DSNHADDR' USING SQL-CODEPTR OF SQL-PLIST3 SQLCA.
           CALL 'DSNHADDR' USING SQL-CODEPTR OF SQL-PLIST4 SQLCA.
           CALL 'DSNHADDR' USING SQL-APARMPTR OF SQL-PLIST5 SQL-AVAR-LIS
      -    T5.
           CALL 'DSNHADD2' USING SQL-AVAR-ADDRS1 IN
           SQL-AVAR-LIST5 CL-TIPO-TRANSACCION OF CL-ESTTRANS SQL-NULL CL
      -    -MONTO OF CL-ESTTRANS SQL-NULL CL-FECHA-HORA OF CL-ESTTRANS S
      -    QL-NULL CL-NUMERO-CUENTA-T OF CL-ESTTRANS SQL-NULL.
           CALL 'DSNHADDR' USING SQL-CODEPTR OF SQL-PLIST5 SQLCA.
       SQL-INIT-END.
           CONTINUE.

       000-MAIN-LOGIC.
           PERFORM 100-INICIO
           PERFORM 200-PROCESO
           PERFORM 300-RETURN.

       100-INICIO.
      *
      *--- SI SE RECIBE COMMAREA (EIBCALEN > 0), SE COPIA A UNA VARIABLE
      *--- LOCAL, OCURRE CUANDO EL PROGRAMA ES LLAMADO CON XCTL O LINK
      *
           IF EIBCALEN > 0
              MOVE DFHCOMMAREA TO CH-COMMAREA
           END-IF
      *
      *--- SI NO HAY COMMAREA (EIBCALEN = 0) SE INICIALIZA EL COMMAREA
      *--- Y SE ENVIA EL MAPA LIMPIO
      *
           IF EIBCALEN = 0
              MOVE LOW-VALUES TO MOVSMPI
              PERFORM 110-ENVIAR-MAPA-VACIO
              SET WS-PRIMERA-VEZ TO TRUE
              PERFORM 300-RETURN
           END-IF
      *
      *--- HAY COMMAREA.
      *--- EL PROGRAMA HA PODIDO ARRANCAR POR XCTL DESDE OTRO
      *--- PROGRAMA COMO RETORNO ACTUAL.EN ESTE CASO EL CAMPO
      *--- CH-TRANS-RETORNO CONTIENE ALGUN VALOR (TRANSACCIONDE RETORNO)
      *--- EN ESTE CASO SE INICIALIZA EL COMMAREA Y ENVIAMOS
      *--- EL MAPA LIMPIO.
      *
           IF EIBCALEN > 0 AND EIBTRNID NOT = 'MOVS'
              MOVE LOW-VALUES  TO MOVSMPI
              PERFORM  110-ENVIAR-MAPA-VACIO
              SET WS-PRIMERA-VEZ TO TRUE
              PERFORM  300-RETURN
           END-IF.

       110-ENVIAR-MAPA-VACIO.
           EXEC CICS SEND MAP('MOVSMP')
                MAPONLY
                ERASE
                NOHANDLE
           END-EXEC.
      *----------------------------------------------------------------
      *--- PROCESA LA PANTALLA SEGUN TECLA ELEGIDA POR EL USUARIO   ---
      *----------------------------------------------------------------
       200-PROCESO.
           IF WS-PRIMERA-VEZ
              CONTINUE
           ELSE
      *
      *--- RECUPERAMOS EL MAPA DESDE EL TERMINAL
      *
              EXEC CICS RECEIVE
                   MAP('MOVSMP')
                   INTO(MOVSMPI)
                   NOHANDLE
              END-EXEC
      *
      *--- F4   : IR A LA PAGINA ANTERIOR
      *--- F5   : IR A LA PAGINA SIGUIENTE
      *--- ENTER: VALIDAMOS EL MAPA Y SI ES CORRECTO PROCESO ENTER
      *
              EVALUATE EIBAID
                   WHEN DFHPF3
                        PERFORM 216-VOLVER-MENU
                   WHEN DFHPF4
                        PERFORM 219-PAGINA-ANTERIOR
                   WHEN DFHPF5
                        PERFORM 218-PAGINA-SIGUIENTE
                   WHEN DFHENTER
                        PERFORM 210-PROCESAR-DATOS
              END-EVALUATE
           END-IF.

       210-PROCESAR-DATOS.
      *
      *--- VALIDAR CAMPOS DE ENTRADA ANTES DE CONSULTAR DB2
      *
           IF CAMPO1I = LOW-VALUES
              PERFORM  110-ENVIAR-MAPA-VACIO
              PERFORM  300-RETURN
           END-IF
      *
      *--- SI LLEGA AQUÖ, LOS CAMPOS TIENEN DATOS VµLIDOS Y SE
      *--- CONSULTA EL NUMERO DE CUENTA PARA VERIFICAR SU EXISTENCIA.
      *--- TAMBIEN SE LEE SECUENCIALMENTE LOS DATOS DE LA TABLA TATRANS
      *--- PASANDO CAMPO1I A CÑ-NUMERO-CUENTA-T PARA TRAER LAS
      *--- TRANSACCIONES POR NUMERO DE CUENTA.
      *
           MOVE CAMPO1I TO CL-NUMERO-CUENTA-T
      *
      *--- SE PONE ESPACIONS VACIOS A WS-TATRANS-DATA PARA NO
      *--- SOBREESCRIBIR LOS DATOS DE UNA NUEVA CONSULTA SI LA HAY
      *
           MOVE SPACES TO WS-TATRANS-DATA
      *
      *--- SE RESETEA EL SWITCHE WS-CONTINUAR POR SI HAY UNA NUEVA
      *--- CONSULTA, DE LO CONTRARIO NO ENTRARIA AL PERFORM VARYING
      *
           MOVE 'N' TO WS-CONTINUAR
      *
      *--- SE INICIA LA LECTURA DE LA TABLA TATRANS ABRIENDO EL CURSOR.
      *--- WS-INDEX > 20: SOLO SE LEE 20 VECES DE LA TABLA TATRANS DADO
      *--- QUE SOLO SE CREARON 20 ESPACION EN LAS VARIABLES DE
      *--- WS-TATRANS-DATA, SI SE QUIERE LEER MAS ES SOLO MODIFICAR
      *--- LOS OCCURS DE LAS VARIBLES Y MODIFICAR WS-INDEX > 20.
      *
      *****EXEC SQL OPEN C_TATRANS END-EXEC
           PERFORM SQL-INITIAL UNTIL SQL-INIT-DONE
           CALL 'DSNHLI' USING SQL-PLIST3
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-EXIT
                   OR WS-INDEX > 20
              PERFORM 223-SQL-LEER-TRANSACCIONES
              IF SQLCODE = 0
                 ADD 1 TO WS-REGISTROS-TOTALES
                 MOVE CL-TIPO-TRANSACCION TO WS-TIPO-TRANS(WS-INDEX)
                 MOVE CL-MONTO            TO WS-MONTO(WS-INDEX)
                 MOVE CL-FECHA-HORA       TO WS-FECHA-HORA(WS-INDEX)
                 MOVE CL-NUMERO-CUENTA-T  TO WS-NUM-CUENTA(WS-INDEX)
              ELSE
                IF SQLCODE = 100
                   SET WS-EXIT TO TRUE
                ELSE
                   PERFORM 999-FALLO-FICHERO
                END-IF
              END-IF
           END-PERFORM
      *****EXEC SQL CLOSE C_TATRANS END-EXEC
           PERFORM SQL-INITIAL UNTIL SQL-INIT-DONE
           CALL 'DSNHLI' USING SQL-PLIST4
      *
      *--- UNA VES FINALIZADA LA LECTURA DE LA TABLA TATRANS SE INICIA
      *--- WS-PAG-ACTUAL A 1 PARA MOSTRAR LA PAGINA ACTUAL EN CICS
      *--- Y SE CALCULA EL NUMERO DE PAGINAS DEPENDIENDO DE CUANTAS
      *--- FILAS SE LEYERON DE LA TABLA TATRANS.
      *
           MOVE 1 TO WS-PAG-ACTUAL
           PERFORM 217-IMPRIMIR-NUMERO-PAGINAS
      *
      *--- SE INICIA LOS LIMITES PARA SOLO MOSTRAR 5 FILAS EN LA TABLA
      *--- LUEGO SE IMPRIME LAS FILAS EN LA TABLA
      *
           MOVE 1 TO WS-PAG-INI
           MOVE 5 TO WS-PAG-FIN
           PERFORM 211-IMPRIMIR-DATOS-CICS
      *
      *--- FINALIZAMOS CON UN REOTRNO TRANS PARA ENVIAR EL MAPA Y
      *--- DEVOLVER EL CONTROL A CICS
      *
           PERFORM 220-ENVIAR-MAPA
           PERFORM 300-RETURN.
      *
      *-----------------------------------------------------------------
      *--- Este bloque se encarga de imprimir en la pantalla CICS una
      *--- página de datos con un máximo de 5 registros por página.
      *---
      *--- Las variables WS-PAG-INI y WS-PAG-FIN indican el rango de
      *--- registros que se deben mostrar (por ejemplo, del registro 1
      *--- al 5, del 6 al 10, etc.).
      *---
      *--- WS-REL-COUNT es un contador relativo que indica la posición
      *--- del registro dentro de la página actual (de 1 a 5), calculado
      *--- como:
      *---      WS-REL-COUNT = WS-COUNT - WS-PAG-INI + 1
      *---
      *--- Dependiendo del valor de WS-REL-COUNT, el registro formateado
      *--- se envía a la posición correspondiente en la pantalla CICS
      *--- mediante los párrafos 212 a 215, que colocan cada registro
      *--- en una línea específica de la pantalla.
      *--------------------------------------------------------------
      *
       211-IMPRIMIR-DATOS-CICS.
           PERFORM VARYING WS-COUNT FROM WS-PAG-INI  BY 1
                   UNTIL WS-COUNT > WS-PAG-FIN
                   COMPUTE WS-REL-COUNT = WS-COUNT - WS-PAG-INI + 1
                   EVALUATE WS-REL-COUNT
                     WHEN 1
                       PERFORM  211-FORMATEAR-MOVIMIENTOS
                       PERFORM  212-SALIDA-CICS-MOVX1
                     WHEN 2
                       PERFORM  211-FORMATEAR-MOVIMIENTOS
                       PERFORM  213-SALIDA-CICS-MOVX2
                     WHEN 3
                       PERFORM  211-FORMATEAR-MOVIMIENTOS
                       PERFORM  214-SALIDA-CICS-MOVX3
                     WHEN 4
                       PERFORM  211-FORMATEAR-MOVIMIENTOS
                       PERFORM  215-SALIDA-CICS-MOVX4
                     WHEN 5
                       PERFORM  211-FORMATEAR-MOVIMIENTOS
                       PERFORM  215-SALIDA-CICS-MOVX5
                   END-EVALUATE
           END-PERFORM.

       211-FORMATEAR-MOVIMIENTOS.
      *
      *--- FORMATEAR MONTO A LA IZQUIERDA USANDO PERFORM VARIYING
      *--- PARA ELIMINAR LOS ESPACIOS A LA IZQUIERDA Y CORRER EL
      *--- MONTO DE DERECHA A IZQUIERDA.
      *
           MOVE WS-MONTO(WS-COUNT)  TO WS-MONTO-VIEW
           PERFORM VARYING WS-INDICE FROM 1 BY 1 UNTIL WS-INDICE > 17
                   OR WS-EXIT-PERFORM
                   IF WS-MONTO-VIEW(WS-INDICE:1) NOT = SPACE
                      SET WS-EXIT-PERFORM TO TRUE
                   END-IF
           END-PERFORM
           MOVE WS-MONTO-VIEW(WS-INDICE - 1:) TO WS-MONTO-JUST
      *
      *--- FORMATEAR FECHA Y HORA
      *
           MOVE WS-FECHA-HORA(WS-COUNT)  TO WS-FECHA-HORA-A
           MOVE WS-FECHA-HORA-A(1:10)    TO WS-FECHA
           MOVE WS-FECHA-HORA-A(12:8)    TO WS-HORA
      *
      *--- REINICIAMOS EL SWITCHE WS-PERFORM
      *--- PARA FORMATEAR NUEVAMENTE OTRO MONTO
      *
           MOVE 'N' TO WS-PRFORM.

       212-SALIDA-CICS-MOVX1.
           MOVE WS-FECHA                   TO MOV11O
           MOVE WS-HORA                    TO MOV21O
           MOVE WS-TIPO-TRANS(WS-COUNT)    TO MOV31O
           MOVE WS-MONTO-JUST              TO MOV41O.

       213-SALIDA-CICS-MOVX2.
           MOVE WS-FECHA                   TO MOV12O
           MOVE WS-HORA                    TO MOV22O
           MOVE WS-TIPO-TRANS(WS-COUNT)    TO MOV32O
           MOVE WS-MONTO-JUST              TO MOV42O.

       214-SALIDA-CICS-MOVX3.
           MOVE WS-FECHA                   TO MOV13O
           MOVE WS-HORA                    TO MOV23O
           MOVE WS-TIPO-TRANS(WS-COUNT)    TO MOV33O
           MOVE WS-MONTO-JUST              TO MOV43O.

       215-SALIDA-CICS-MOVX4.
           MOVE WS-FECHA                   TO MOV14O
           MOVE WS-HORA                    TO MOV24O
           MOVE WS-TIPO-TRANS(WS-COUNT)    TO MOV34O
           MOVE WS-MONTO-JUST              TO MOV44O.

       215-SALIDA-CICS-MOVX5.
           MOVE WS-FECHA                   TO MOV15O
           MOVE WS-HORA                    TO MOV25O
           MOVE WS-TIPO-TRANS(WS-COUNT)    TO MOV35O
           MOVE WS-MONTO-JUST              TO MOV45O.

       216-VOLVER-MENU.
           MOVE 'MENUPGM' TO CH-XCTL
           MOVE WC-TRANSACCION    TO CH-TRANSACCION
           MOVE WC-TRANSACCION    TO CH-TRANS-RETORNO
           MOVE WC-PROGRAMA       TO CH-PROGRAMA-RETORNO
           PERFORM  221-XCTL-PROGRAMA.

       217-IMPRIMIR-NUMERO-PAGINAS.
           DIVIDE WS-REGISTROS-TOTALES BY WS-FILAS-PAG
               GIVING WS-NUM-PAG
               REMAINDER WS-RESTO
           IF WS-RESTO > 0
              ADD 1 TO WS-NUM-PAG
           END-IF
           MOVE WS-NUM-PAG TO ALLPAGO
           MOVE WS-PAG-ACTUAL TO NUMPAGO.

       218-PAGINA-SIGUIENTE.
           IF WS-PAG-ACTUAL < WS-NUM-PAG
              ADD 5 TO WS-PAG-INI
              ADD 5 TO WS-PAG-FIN
              ADD 1 TO WS-PAG-ACTUAL

              PERFORM 211-IMPRIMIR-DATOS-CICS
              MOVE WS-PAG-ACTUAL TO NUMPAGO
            END-IF
           PERFORM 220-ENVIAR-MAPA
           PERFORM 300-RETURN.

       219-PAGINA-ANTERIOR.
           IF WS-PAG-ACTUAL > 1
              SUBTRACT 5 FROM WS-PAG-INI
              SUBTRACT 5 FROM WS-PAG-FIN
              SUBTRACT 1 FROM WS-PAG-ACTUAL

              PERFORM 211-IMPRIMIR-DATOS-CICS
              MOVE WS-PAG-ACTUAL TO NUMPAGO
           END-IF
           PERFORM 220-ENVIAR-MAPA
           PERFORM 300-RETURN.

       220-ENVIAR-MAPA.
           EXEC CICS SEND
                MAP('MOVSMP')
                ERASE
                FROM(MOVSMPO)
                NOHANDLE
           END-EXEC.

       221-XCTL-PROGRAMA.
           EXEC CICS
                XCTL
                PROGRAM(CH-XCTL )
                COMMAREA(CH-COMMAREA)
           END-EXEC.

       223-SQL-LEER-TRANSACCIONES.
      *****EXEC SQL
      *****     FETCH C_TATRANS INTO
      *****       :CL-TIPO-TRANSACCION
      *****      ,:CL-MONTO
      *****      ,:CL-FECHA-HORA
      *****      ,:CL-NUMERO-CUENTA-T
      ***** END-EXEC.
           PERFORM SQL-INITIAL UNTIL SQL-INIT-DONE
           CALL 'DSNHLI' USING SQL-PLIST5.

       300-RETURN.
           EXEC CICS RETURN
                TRANSID('MOVS')
                COMMAREA(CH-COMMAREA)
           END-EXEC.

       999-FALLO-FICHERO.
           IF SQLCODE >= 100
              MOVE 'CUENTA NO ENCONTRADA' TO  MSGO
              PERFORM 220-ENVIAR-MAPA
              PERFORM 300-RETURN
           ELSE
              PERFORM 999-ERROR-DB2
           END-IF.

       999-ERROR-DB2.
           MOVE SQLCODE        TO DB2-SQLCODE
           MOVE DB2-SQLCODE    TO DB2-SQLCODE-Z
           MOVE DB2-SQLCODE-Z  TO DB2-ERR-CODE
           MOVE SQLERRMC       TO DB2-ERR-MSG.
           MOVE DB2-ERROR      TO MSGO
           MOVE SQLSTATE       TO MSGO(54:)
           MOVE SPACES         TO MSGO
           MOVE 'ERROR DB2: '  TO MSGO(1:11)
           MOVE DB2-SQLCODE-Z  TO MSGO(13:10)
           MOVE DB2-ERR-MSG    TO MSGO(24:30)
           PERFORM 220-ENVIAR-MAPA
           PERFORM 300-RETURN.
