      *****************************************************
      *                                                   *
      *   PROGRAMA DEPOSITO CICS-DB2 SISTEMA BANCARIO     *
      *                                                   *
      *****************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID. DEPOCOB.
       AUTHOR. CAMILO LOPEZ.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
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
      *****EXEC SQL INCLUDE TACUENT END-EXEC.
      ******************************************************************
      * DCLGEN TABLE(TACUENT)                                          *
      *        LIBRARY(BANCO1.SISTEMA.DCLGEN(TACUENT))                 *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(CL-)                                              *
      *        STRUCTURE(CL-ESTCUENT)                                  *
      *        QUOTE                                                   *
      *        DBCSDELIM(NO)                                           *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
      *****EXEC SQL DECLARE TACUENT TABLE
      *****( NUMERO_CUENTA                  CHAR(10) NOT NULL,
      *****  CEDULA_CLIENTE                 CHAR(10) NOT NULL,
      *****  NOMBRE_CLIENTE                 CHAR(50) NOT NULL,
      *****  SALDO                          DECIMAL(15, 2) NOT NULL,
      *****  ESTADO_CUENTA                  CHAR(1) NOT NULL
      *****) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TACUENT                            *
      ******************************************************************
       01  CL-ESTCUENT.
      *                       NUMERO_CUENTA
           10 CL-NUMERO-CUENTA     PIC X(10).
      *                       CEDULA_CLIENTE
           10 CL-CEDULA-CLIENTE    PIC X(10).
      *                       NOMBRE_CLIENTE
           10 CL-NOMBRE-CLIENTE    PIC X(50).
      *                       SALDO
           10 CL-SALDO             PIC S9(13)V9(2) USAGE COMP-3.
      *                       ESTADO_CUENTA
           10 CL-ESTADO-CUENTA     PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
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

       01  WS-AUXILIARES.
           03 WS-CAMPO2I-JUST         PIC X(10)   JUST RIGHT.
           03 WS-CAMPO2I-NUM          PIC 9(10).
       01  SWITCHES.
           03  WS-PRIMERA-FALG        PIC X           VALUE 'N'.
               88 WS-PRIMERA-VEZ                      VALUE 'Y'.
       01 DB2-ERROR.
          05 DB2-SQLCODE              PIC S9(9).
          05 DB2-SQLCODE-Z            PIC -ZZZZZZZZ9.
          05 DB2-ERROR-MSG.
             06 DB2-ERR-MSG           PIC X(40).
             06 DB2-ERR-CODE          PIC X(20).
       01 WC-CONSTANTES.
          03 WC-PROGRAMA              PIC X(8)     VALUE 'DEPOCOB'.
          03 WC-TRANSACCION           PIC X(4)     VALUE 'DEPO'.

       COPY DEPOMPCP.
       COPY DFHAID.
       COPY DFHBMSCA.
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
           05 SQL-PLIST-CON   PIC S9(9) COMP-4 VALUE +4211712.
           05 SQL-CALLTYPE    PIC S9(4) COMP-4 VALUE +30.
           05 SQL-PROG-NAME   PIC X(8)  VALUE X'4445504F434F4220'.
           05 SQL-TIMESTAMP-1 PIC S9(9) COMP-4 VALUE +472467093.
           05 SQL-TIMESTAMP-2 PIC S9(9) COMP-4 VALUE +84731392.
           05 SQL-SECTION     PIC S9(4) COMP-4 VALUE +1.
           05 SQL-CODEPTR     PIC S9(9) COMP-4.
           05 SQL-VPARMPTR    PIC S9(9) COMP-4 VALUE +0.
           05 SQL-APARMPTR    PIC S9(9) COMP-4 VALUE +0.
           05 FILLER          PIC S9(4) COMP-4 VALUE +1208.
           05 SQL-STMT-TYPE   PIC S9(4) COMP-4 VALUE +231.
           05 SQL-STMT-NUM    PIC S9(9) COMP-4 VALUE +253.
           05 SQL-PLIST-FLG   PIC S9(4) COMP-4 VALUE +0.
           05 FILLER          PIC X(18) VALUE
              X'000000000000000000000000000000000000'.
           05 SQL-PVAR-LIST3.
              10 PRE-SQLDAID  PIC X(8)  VALUE 'SQLDA  Ê'.
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
           05 SQL-AVAR-LIST3.
              10 PRE-SQLDAID  PIC X(8)  VALUE 'SQLDA  Ê'.
              10 PRE-SQLDABC  PIC S9(9) COMP-4 VALUE +104.
              10 PRE-SQLN     PIC S9(4) COMP-4 VALUE +2.
              10 PRE-SQLLD    PIC S9(4) COMP-4 VALUE +2.
              10 PRE-SQLVAR.
                12 SQLVAR-BASE1.
                  15 SQL-AVAR-TYPE1      PIC S9(4) COMP-4 VALUE +452.
                  15 SQL-AVAR-LEN1       PIC S9(4) COMP-4 VALUE +10.
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
        01 SQL-PLIST4.
           05 SQL-PLIST-CON   PIC S9(9) COMP-4 VALUE +4210688.
           05 SQL-CALLTYPE    PIC S9(4) COMP-4 VALUE +30.
           05 SQL-PROG-NAME   PIC X(8)  VALUE X'4445504F434F4220'.
           05 SQL-TIMESTAMP-1 PIC S9(9) COMP-4 VALUE +472467093.
           05 SQL-TIMESTAMP-2 PIC S9(9) COMP-4 VALUE +84731392.
           05 SQL-SECTION     PIC S9(4) COMP-4 VALUE +2.
           05 SQL-CODEPTR     PIC S9(9) COMP-4.
           05 SQL-VPARMPTR    PIC S9(9) COMP-4 VALUE +0.
           05 SQL-APARMPTR    PIC S9(9) COMP-4 VALUE +0.
           05 FILLER          PIC S9(4) COMP-4 VALUE +1208.
           05 SQL-STMT-TYPE   PIC S9(4) COMP-4 VALUE +234.
           05 SQL-STMT-NUM    PIC S9(9) COMP-4 VALUE +267.
           05 SQL-PLIST-FLG   PIC S9(4) COMP-4 VALUE +0.
           05 FILLER          PIC X(18) VALUE
              X'000000000000000000000000000000000000'.
           05 SQL-PVAR-LIST4.
              10 PRE-SQLDAID  PIC X(8)  VALUE 'SQLDA  Ê'.
              10 PRE-SQLDABC  PIC S9(9) COMP-4 VALUE +104.
              10 PRE-SQLN     PIC S9(4) COMP-4 VALUE +2.
              10 PRE-SQLLD    PIC S9(4) COMP-4 VALUE +2.
              10 PRE-SQLVAR.
                12 SQLVAR-BASE1.
                  15 SQL-PVAR-TYPE1      PIC S9(4) COMP-4 VALUE +484.
                  15 SQL-PVAR-LEN1       PIC S9(4) COMP-4 VALUE +3842.
                  15 SQL-PVAR-ADDRS1.
                     20 SQL-PVAR-ADDR1   PIC S9(9) COMP-4.
                     20 SQL-PVAR-IND1    PIC S9(9) COMP-4.
                  15 SQL-PVAR-NAME1.
                     20 SQL-PVAR-NAMEL1  PIC S9(4) COMP-4 VALUE +0.
                     20 SQL-PVAR-NAMEC1  PIC X(30) VALUE ' '.
                12 SQLVAR-BASE2.
                  15 SQL-PVAR-TYPE2      PIC S9(4) COMP-4 VALUE +452.
                  15 SQL-PVAR-LEN2       PIC S9(4) COMP-4 VALUE +10.
                  15 SQL-PVAR-ADDRS2.
                     20 SQL-PVAR-ADDR2   PIC S9(9) COMP-4.
                     20 SQL-PVAR-IND2    PIC S9(9) COMP-4.
                  15 SQL-PVAR-NAME2.
                     20 SQL-PVAR-NAMEL2  PIC S9(4) COMP-4 VALUE +0.
                     20 SQL-PVAR-NAMEC2  PIC X(30) VALUE ' '.
        01 SQL-PLIST5.
           05 SQL-PLIST-CON   PIC S9(9) COMP-4 VALUE +4210688.
           05 SQL-CALLTYPE    PIC S9(4) COMP-4 VALUE +30.
           05 SQL-PROG-NAME   PIC X(8)  VALUE X'4445504F434F4220'.
           05 SQL-TIMESTAMP-1 PIC S9(9) COMP-4 VALUE +472467093.
           05 SQL-TIMESTAMP-2 PIC S9(9) COMP-4 VALUE +84731392.
           05 SQL-SECTION     PIC S9(4) COMP-4 VALUE +3.
           05 SQL-CODEPTR     PIC S9(9) COMP-4.
           05 SQL-VPARMPTR    PIC S9(9) COMP-4 VALUE +0.
           05 SQL-APARMPTR    PIC S9(9) COMP-4 VALUE +0.
           05 FILLER          PIC S9(4) COMP-4 VALUE +1208.
           05 SQL-STMT-TYPE   PIC S9(4) COMP-4 VALUE +232.
           05 SQL-STMT-NUM    PIC S9(9) COMP-4 VALUE +280.
           05 SQL-PLIST-FLG   PIC S9(4) COMP-4 VALUE +0.
           05 FILLER          PIC X(18) VALUE
              X'000000000000000000000000000000000000'.
           05 SQL-PVAR-LIST5.
              10 PRE-SQLDAID  PIC X(8)  VALUE 'SQLDA  Ê'.
              10 PRE-SQLDABC  PIC S9(9) COMP-4 VALUE +104.
              10 PRE-SQLN     PIC S9(4) COMP-4 VALUE +2.
              10 PRE-SQLLD    PIC S9(4) COMP-4 VALUE +2.
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
                12 SQLVAR-BASE2.
                  15 SQL-PVAR-TYPE2      PIC S9(4) COMP-4 VALUE +484.
                  15 SQL-PVAR-LEN2       PIC S9(4) COMP-4 VALUE +3842.
                  15 SQL-PVAR-ADDRS2.
                     20 SQL-PVAR-ADDR2   PIC S9(9) COMP-4.
                     20 SQL-PVAR-IND2    PIC S9(9) COMP-4.
                  15 SQL-PVAR-NAME2.
                     20 SQL-PVAR-NAMEL2  PIC S9(4) COMP-4 VALUE +0.
                     20 SQL-PVAR-NAMEC2  PIC X(30) VALUE ' '.

       LINKAGE SECTION.
       01 DFHCOMMAREA                 PIC X(40).

       PROCEDURE DIVISION.
       DSNSQL SECTION.
       SQL-SKIP.
           GO TO SQL-INIT-END.
       SQL-INITIAL.
           MOVE 1 TO SQL-INIT-FLAG.
           CALL 'DSNHADDR' USING SQL-VPARMPTR OF SQL-PLIST3 SQL-PVAR-LIS
      -    T3.
           CALL 'DSNHADD2' USING SQL-PVAR-ADDRS1 IN
           SQL-PVAR-LIST3 CL-NUMERO-CUENTA OF CL-ESTCUENT SQL-NULL
           CALL 'DSNHADDR' USING SQL-APARMPTR OF SQL-PLIST3 SQL-AVAR-LIS
      -    T3.
           CALL 'DSNHADD2' USING SQL-AVAR-ADDRS1 IN
           SQL-AVAR-LIST3 CL-NUMERO-CUENTA OF CL-ESTCUENT SQL-NULL CL-SA
      -    LDO OF CL-ESTCUENT SQL-NULL.
           CALL 'DSNHADDR' USING SQL-CODEPTR OF SQL-PLIST3 SQLCA.
           CALL 'DSNHADDR' USING SQL-VPARMPTR OF SQL-PLIST4 SQL-PVAR-LIS
      -    T4.
           CALL 'DSNHADD2' USING SQL-PVAR-ADDRS1 IN
           SQL-PVAR-LIST4 CL-SALDO OF CL-ESTCUENT SQL-NULL CL-NUMERO-CUE
      -    NTA OF CL-ESTCUENT SQL-NULL
           CALL 'DSNHADDR' USING SQL-CODEPTR OF SQL-PLIST4 SQLCA.
           CALL 'DSNHADDR' USING SQL-VPARMPTR OF SQL-PLIST5 SQL-PVAR-LIS
      -    T5.
           CALL 'DSNHADD2' USING SQL-PVAR-ADDRS1 IN
           SQL-PVAR-LIST5 CL-NUMERO-CUENTA OF CL-ESTCUENT SQL-NULL CL-MO
      -    NTO OF CL-ESTTRANS SQL-NULL
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
              MOVE DFHCOMMAREA  TO  CH-COMMAREA
           END-IF
      *
      *--- SI NO HAY COMMAREA (EIBCALEN = 0) SE INICIALIZA EL COMMAREA
      *--- Y SE ENVIA EL MAPA LIMPIO
      *
           IF EIBCALEN = 0
              MOVE LOW-VALUES TO DEPOBMPI
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
           IF EIBCALEN > 0 AND EIBTRNID NOT = 'DEPO'
              MOVE LOW-VALUES TO DEPOBMPI
              PERFORM 110-ENVIAR-MAPA-VACIO
              SET WS-PRIMERA-VEZ TO TRUE
              PERFORM 300-RETURN
           END-IF.

       110-ENVIAR-MAPA-VACIO.
           EXEC CICS SEND MAP('DEPOBMP')
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
                   MAP('DEPOBMP')
                   INTO(DEPOBMPI)
                   NOHANDLE
              END-EXEC
      *
      *--- ENTER: VALIDAMOS EL MAPA Y SI ES CORRECTO PROCESO ENTER
      *
              EVALUATE EIBAID
                   WHEN DFHPF3
                        PERFORM 216-VOLVER-MENU
                   WHEN DFHENTER
                        PERFORM 210-PROCESAR-DATOS
              END-EVALUATE
           END-IF.

       210-PROCESAR-DATOS.
      *
      *--- VALIDAR CAMPOS DE ENTRADA ANTES DE CONSULTAR DB2
      *
           IF CAMPO1I = LOW-VALUES OR CAMPO2I = LOW-VALUES
              PERFORM 110-ENVIAR-MAPA-VACIO
              PERFORM 300-RETURN
           END-IF
      *
      *--- SI LLEGA AQUÖ, LOS CAMPOS TIENEN DATOS VµLIDOS
      *
           PERFORM 212-CONSULTAR-CUENTA-DB2.

       212-CONSULTAR-CUENTA-DB2.
      *
      *--- CONSULTAR CUENTA EN LA BASE DE DATOS.
      *
           MOVE CAMPO1I TO CL-NUMERO-CUENTA
           PERFORM 222-SQL-CONSULTA
      *
      *--- SUMAR SALDO A CL-SALDO PARA ACTUALIZAR SALDO EN TACUENT
      *
           IF SQLCODE = 0
              PERFORM 213-RELLENAR-CAMPO2I
              ADD WS-CAMPO2I-NUM TO CL-SALDO
              PERFORM 223-SQL-ACTUALIZAR-SALDO
              PERFORM 224-SQL-CREAR-TRANSACCION
           ELSE
              PERFORM 999-FALLO-FICHERO
           END-IF.
      *-------------------------------------------------------------
      *--- CAMPO2I VIENE DE ESTA FORMA 1000______ Y SE NECESITA DE
      *--- ESTA FORMA 0000001000
      *-------------------------------------------------------------
       213-RELLENAR-CAMPO2I.
           UNSTRING CAMPO2I DELIMITED BY '_'
               INTO WS-CAMPO2I-JUST
           END-UNSTRING
           INSPECT WS-CAMPO2I-JUST REPLACING ALL '_'  BY ZEROES
           MOVE WS-CAMPO2I-JUST TO WS-CAMPO2I-NUM.

       216-VOLVER-MENU.
           MOVE 'MENUPGM'        TO CH-XCTL
           MOVE WC-TRANSACCION   TO CH-TRANSACCION
           MOVE WC-TRANSACCION   TO CH-TRANS-RETORNO
           MOVE WC-PROGRAMA      TO CH-PROGRAMA-RETORNO
           PERFORM 221-XCTL-PROGRAMA.

       220-ENVIAR-MAPA.
           EXEC CICS SEND
                MAP('DEPOBMP')
                ERASE
                FROM(DEPOBMPO)
                NOHANDLE
           END-EXEC.

       221-XCTL-PROGRAMA.
           EXEC CICS
                XCTL
                PROGRAM(CH-XCTL)
                COMMAREA(CH-COMMAREA)
           END-EXEC.

       222-SQL-CONSULTA.
      *****EXEC SQL
      *****     SELECT
      *****        NUMERO_CUENTA
      *****       ,SALDO
      *****     INTO
      *****       :CL-NUMERO-CUENTA
      *****      ,:CL-SALDO
      *****     FROM
      *****        TACUENT
      *****     WHERE
      *****        NUMERO_CUENTA = :CL-NUMERO-CUENTA
      *****END-EXEC.
           PERFORM SQL-INITIAL UNTIL SQL-INIT-DONE
           CALL 'DSNHLI' USING SQL-PLIST3.

       223-SQL-ACTUALIZAR-SALDO.
      *****EXEC SQL
      *****     UPDATE TACUENT
      *****     SET    SALDO = :CL-SALDO
      *****     WHERE  NUMERO_CUENTA = :CL-NUMERO-CUENTA
      *****END-EXEC
           PERFORM SQL-INITIAL UNTIL SQL-INIT-DONE
           CALL 'DSNHLI' USING SQL-PLIST4
           IF SQLCODE = 0
              CONTINUE
           ELSE
              PERFORM 999-FALLO-FICHERO
           END-IF.

       224-SQL-CREAR-TRANSACCION.
           MOVE WS-CAMPO2I-NUM TO CL-MONTO
      *****EXEC SQL
      *****     INSERT INTO TATRANS (
      *****         NUMERO_CUENTA_T
      *****        ,TIPO_TRANSACCION
      *****        ,MONTO
      *****        ,FECHA_HORA
      *****     ) VALUES (
      *****         :CL-NUMERO-CUENTA
      *****        ,'D'
      *****        ,:CL-MONTO
      *****        ,CURRENT TIMESTAMP
      *****     )
      *****END-EXEC
           PERFORM SQL-INITIAL UNTIL SQL-INIT-DONE
           CALL 'DSNHLI' USING SQL-PLIST5
           IF SQLCODE = 0
              MOVE 'SALDO ACTUALIZADO CORRECTAMENTE' TO MSGO
              PERFORM 220-ENVIAR-MAPA
              PERFORM 300-RETURN
           ELSE
              PERFORM 999-FALLO-FICHERO
           END-IF.

       300-RETURN.
           EXEC CICS
                RETURN
                TRANSID('DEPO')
                COMMAREA(CH-COMMAREA)
           END-EXEC.

       400-FIN.
           EXEC CICS SEND CONTROL
                ERASE
                FREEKB
           END-EXEC

           EXEC CICS RETURN
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
