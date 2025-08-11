      *****************************************************
      *                                                   *
      *   PROGRAMA CONSULTA CICS-DB2 SISTEMA BANCARIO     *
      *                                                   *
      *****************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID. CLNTCOB.
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

       01  WS-AUXILIARES.
           03 WS-SALDO                PIC ZZZ.ZZZ.ZZZ.ZZZ,ZZZ.
           03 WS-SALDO-FAN            PIC X(15).
           03 WS-SALDO-JUST           PIC X(15).
           03 INDICE                  PIC 9(02).

       01  MI-COMMAREA.
           03  CAMPOINICIO            PIC  X(8).
       01  SWITCHES.
           03  WS-PRIMERA-FALG        PIC X           VALUE 'N'.
               88 WS-PRIMERA-VEZ                      VALUE 'Y'.
           03  WS-PRFORM              PIC X           VALUE 'N'.
               88 WS-EXIT-PERFORM                     VALUE 'Y'.
       01 DB2-ERROR.
          05 DB2-SQLCODE              PIC S9(9).
          05 DB2-SQLCODE-Z            PIC -ZZZZZZZZ9.
          05 DB2-ERROR-MSG.
             06 DB2-ERR-MSG           PIC X(40).
             06 DB2-ERR-CODE          PIC X(20).
       01 WC-CONSTANTES.
          03 WC-PROGRAMA              PIC X(8)     VALUE 'CLNTCOB'.
          03 WC-TRANSACCION           PIC X(4)     VALUE 'CLIE'.

       COPY CONSMPCP.
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
        01 SQL-PLIST2.
           05 SQL-PLIST-CON   PIC S9(9) COMP-4 VALUE +4211712.
           05 SQL-CALLTYPE    PIC S9(4) COMP-4 VALUE +30.
           05 SQL-PROG-NAME   PIC X(8)  VALUE X'434C4E54434F4220'.
           05 SQL-TIMESTAMP-1 PIC S9(9) COMP-4 VALUE +472494542.
           05 SQL-TIMESTAMP-2 PIC S9(9) COMP-4 VALUE +272747648.
           05 SQL-SECTION     PIC S9(4) COMP-4 VALUE +1.
           05 SQL-CODEPTR     PIC S9(9) COMP-4.
           05 SQL-VPARMPTR    PIC S9(9) COMP-4 VALUE +0.
           05 SQL-APARMPTR    PIC S9(9) COMP-4 VALUE +0.
           05 FILLER          PIC S9(4) COMP-4 VALUE +1208.
           05 SQL-STMT-TYPE   PIC S9(4) COMP-4 VALUE +231.
           05 SQL-STMT-NUM    PIC S9(9) COMP-4 VALUE +227.
           05 SQL-PLIST-FLG   PIC S9(4) COMP-4 VALUE +0.
           05 FILLER          PIC X(18) VALUE
              X'000000000000000000000000000000000000'.
           05 SQL-PVAR-LIST2.
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
           05 SQL-AVAR-LIST2.
              10 PRE-SQLDAID  PIC X(8)  VALUE 'SQLDA  Ê'.
              10 PRE-SQLDABC  PIC S9(9) COMP-4 VALUE +236.
              10 PRE-SQLN     PIC S9(4) COMP-4 VALUE +5.
              10 PRE-SQLLD    PIC S9(4) COMP-4 VALUE +5.
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
                  15 SQL-AVAR-TYPE2      PIC S9(4) COMP-4 VALUE +452.
                  15 SQL-AVAR-LEN2       PIC S9(4) COMP-4 VALUE +10.
                  15 SQL-AVAR-ADDRS2.
                     20 SQL-AVAR-ADDR2   PIC S9(9) COMP-4.
                     20 SQL-AVAR-IND2    PIC S9(9) COMP-4.
                  15 SQL-AVAR-NAME2.
                     20 SQL-AVAR-NAMEL2  PIC S9(4) COMP-4 VALUE +0.
                     20 SQL-AVAR-NAMEC2  PIC X(30) VALUE ' '.
                12 SQLVAR-BASE3.
                  15 SQL-AVAR-TYPE3      PIC S9(4) COMP-4 VALUE +452.
                  15 SQL-AVAR-LEN3       PIC S9(4) COMP-4 VALUE +50.
                  15 SQL-AVAR-ADDRS3.
                     20 SQL-AVAR-ADDR3   PIC S9(9) COMP-4.
                     20 SQL-AVAR-IND3    PIC S9(9) COMP-4.
                  15 SQL-AVAR-NAME3.
                     20 SQL-AVAR-NAMEL3  PIC S9(4) COMP-4 VALUE +0.
                     20 SQL-AVAR-NAMEC3  PIC X(30) VALUE ' '.
                12 SQLVAR-BASE4.
                  15 SQL-AVAR-TYPE4      PIC S9(4) COMP-4 VALUE +484.
                  15 SQL-AVAR-LEN4       PIC S9(4) COMP-4 VALUE +3842.
                  15 SQL-AVAR-ADDRS4.
                     20 SQL-AVAR-ADDR4   PIC S9(9) COMP-4.
                     20 SQL-AVAR-IND4    PIC S9(9) COMP-4.
                  15 SQL-AVAR-NAME4.
                     20 SQL-AVAR-NAMEL4  PIC S9(4) COMP-4 VALUE +0.
                     20 SQL-AVAR-NAMEC4  PIC X(30) VALUE ' '.
                12 SQLVAR-BASE5.
                  15 SQL-AVAR-TYPE5      PIC S9(4) COMP-4 VALUE +452.
                  15 SQL-AVAR-LEN5       PIC S9(4) COMP-4 VALUE +1.
                  15 SQL-AVAR-ADDRS5.
                     20 SQL-AVAR-ADDR5   PIC S9(9) COMP-4.
                     20 SQL-AVAR-IND5    PIC S9(9) COMP-4.
                  15 SQL-AVAR-NAME5.
                     20 SQL-AVAR-NAMEL5  PIC S9(4) COMP-4 VALUE +0.
                     20 SQL-AVAR-NAMEC5  PIC X(30) VALUE ' '.

       PROCEDURE DIVISION.
       DSNSQL SECTION.
       SQL-SKIP.
           GO TO SQL-INIT-END.
       SQL-INITIAL.
           MOVE 1 TO SQL-INIT-FLAG.
           CALL 'DSNHADDR' USING SQL-VPARMPTR OF SQL-PLIST2 SQL-PVAR-LIS
      -    T2.
           CALL 'DSNHADD2' USING SQL-PVAR-ADDRS1 IN
           SQL-PVAR-LIST2 CL-NUMERO-CUENTA OF CL-ESTCUENT SQL-NULL
           CALL 'DSNHADDR' USING SQL-APARMPTR OF SQL-PLIST2 SQL-AVAR-LIS
      -    T2.
           CALL 'DSNHADD2' USING SQL-AVAR-ADDRS1 IN
           SQL-AVAR-LIST2 CL-NUMERO-CUENTA OF CL-ESTCUENT SQL-NULL CL-CE
      -    DULA-CLIENTE OF CL-ESTCUENT SQL-NULL CL-NOMBRE-CLIENTE OF CL-
      -    ESTCUENT SQL-NULL CL-SALDO OF CL-ESTCUENT SQL-NULL CL-ESTADO-
      -    CUENTA OF CL-ESTCUENT SQL-NULL.
           CALL 'DSNHADDR' USING SQL-CODEPTR OF SQL-PLIST2 SQLCA.
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
              MOVE LOW-VALUES TO CONSMPI
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
           IF EIBCALEN > 0 AND EIBTRNID NOT = 'CLIE'
              MOVE LOW-VALUES TO CONSMPI
              PERFORM 110-ENVIAR-MAPA-VACIO
              SET WS-PRIMERA-VEZ TO TRUE
              PERFORM 300-RETURN
           END-IF.

       110-ENVIAR-MAPA-VACIO.
           EXEC CICS SEND MAP('CONSMP')
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
                   MAP('CONSMP')
                   INTO(CONSMPI)
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
           IF CAMPO1I = LOW-VALUES
              PERFORM 110-ENVIAR-MAPA-VACIO
              PERFORM 300-RETURN
           END-IF
      *
      *--- SI LLEGA AQUÃ–, LOS CAMPOS TIENEN DATOS VÂµLIDOS
      *
           PERFORM 212-CONSULTAR-CUENTA-DB2.

       212-CONSULTAR-CUENTA-DB2.
      *
      *--- CONSULTAR CUENTA EN LA BASE DE DATOS.
      *
           MOVE CAMPO1I TO CL-NUMERO-CUENTA
           PERFORM 222-SQL-CONSULTA
      *
      *--- SI EXISTE LA CUENTA SE MUESTRAN LOS DATOS DE LA CUENTA
      *
           IF SQLCODE = 0
              PERFORM 213-MOSTRAR-DATOS-CUENTA
           ELSE
              PERFORM 999-FALLO-FICHERO
           END-IF.

       213-MOSTRAR-DATOS-CUENTA.
           MOVE CL-NUMERO-CUENTA  TO  CUENTAO
           MOVE CL-CEDULA-CLIENTE TO  CEDULAO
           MOVE CL-NOMBRE-CLIENTE TO  NOMBREO

           MOVE CL-SALDO          TO  WS-SALDO
           MOVE WS-SALDO          TO  WS-SALDO-FAN
           PERFORM VARYING INDICE FROM 1 BY 1 UNTIL INDICE > 15
                   OR WS-EXIT-PERFORM
                   IF WS-SALDO-FAN(INDICE :1) NOT = SPACE
                      SET WS-EXIT-PERFORM TO TRUE
                   END-IF
            END-PERFORM

           MOVE WS-SALDO-FAN(INDICE - 1:) TO WS-SALDO-JUST
           MOVE WS-SALDO-JUST  TO SALDOO

           MOVE CL-ESTADO-CUENTA  TO  ESTADOO
           PERFORM 220-ENVIAR-MAPA
           PERFORM 300-RETURN.

       216-VOLVER-MENU.
           MOVE 'MENUPGM' TO CH-XCTL
           MOVE WC-TRANSACCION TO CH-TRANSACCION
           MOVE WC-TRANSACCION TO CH-TRANS-RETORNO
           MOVE WC-PROGRAMA    TO CH-PROGRAMA-RETORNO
           PERFORM 221-XCTL-PROGRAMA.

       220-ENVIAR-MAPA.
           EXEC CICS SEND
                MAP('CONSMP')
                ERASE
                FROM(CONSMPO)
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
      *****       ,CEDULA_CLIENTE
      *****       ,NOMBRE_CLIENTE
      *****       ,SALDO
      *****       ,ESTADO_CUENTA
      *****     INTO
      *****       :CL-NUMERO-CUENTA
      *****      ,:CL-CEDULA-CLIENTE
      *****      ,:CL-NOMBRE-CLIENTE
      *****      ,:CL-SALDO
      *****      ,:CL-ESTADO-CUENTA
      *****     FROM
      *****        TACUENT
      *****     WHERE
      *****        NUMERO_CUENTA = :CL-NUMERO-CUENTA
      *****END-EXEC.
           PERFORM SQL-INITIAL UNTIL SQL-INIT-DONE
           CALL 'DSNHLI' USING SQL-PLIST2.

       300-RETURN.
           EXEC CICS RETURN
                TRANSID('CLIE')
                COMMAREA(MI-COMMAREA)
                LENGTH(8)
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


