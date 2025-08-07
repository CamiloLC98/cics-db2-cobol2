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
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE TACUENT END-EXEC.

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

       PROCEDURE DIVISION.
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
           EXEC SQL
                SELECT
                   NUMERO_CUENTA
                  ,CEDULA_CLIENTE
                  ,NOMBRE_CLIENTE
                  ,SALDO
                  ,ESTADO_CUENTA
                INTO
                  :CL-NUMERO-CUENTA
                 ,:CL-CEDULA-CLIENTE
                 ,:CL-NOMBRE-CLIENTE
                 ,:CL-SALDO
                 ,:CL-ESTADO-CUENTA
                FROM
                   TACUENT
                WHERE
                   NUMERO_CUENTA = :CL-NUMERO-CUENTA
           END-EXEC.

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


