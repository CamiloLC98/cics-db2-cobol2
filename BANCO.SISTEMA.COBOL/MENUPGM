      *****************************************************
      *                                                   *
      *   PROGRAMA MENU CICS-DB2 SISTEMA BANCARIO         *
      *                                                   *
      *****************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID. MENUPGM.
       AUTHOR. CAMILO LOPEZ.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  SWITCHES.
           03  WS-PRIMERA-FALG        PIC X           VALUE 'N'.
               88 WS-PRIMERA-VEZ                      VALUE 'Y'.
       01  WS-CONSTANTES.
           03  WS-TRANSACCION         PIC X(4)        VALUE 'MENU'.
           03  WS-PROGRAMA-RETORNO    PIC X(8)        VALUE 'MENUPGM'.

       COPY MENUMPCP.
       COPY DFHAID.
       COPY DFHBMSCA.
       COPY DDCICS.

       LINKAGE SECTION.
       01  DFHCOMMAREA                 PIC X(40).

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
              MOVE DFHCOMMAREA    TO CH-COMMAREA
           END-IF
      *
      *--- SI NO HAY COMMAREA (EIBCALEN = 0) SE INICIALIZA EL COMMAREA
      *--- Y SE ENVIA EL MAPA LIMPIO
      *
           IF EIBCALEN = 0
              MOVE LOW-VALUES TO MENUMPI
              MOVE SPACES TO CH-COMUN
              MOVE SPACES TO CH-TRANS-RETORNO
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
           IF EIBCALEN > 0 AND CH-TRANS-RETORNO NOT = SPACES
              MOVE LOW-VALUES TO MENUMPI
              MOVE SPACES TO CH-COMUN
              PERFORM 110-ENVIAR-MAPA-VACIO
              SET WS-PRIMERA-VEZ TO TRUE
              PERFORM 300-RETURN
           END-IF.

       110-ENVIAR-MAPA-VACIO.
           EXEC CICS SEND MAP('MENUMP')
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
      * RECUPERAMOS EL MAPA DESDE EL TERMINAL
      *
              EXEC CICS RECEIVE
                   MAP('MENUMP')
                   INTO(MENUMPI)
                   NOHANDLE
              END-EXEC
      *
      *--- ENTER: VALIDAMOS EL MAPA Y SI ES CORRECTO PROCESO ENTER
      *
              EVALUATE EIBAID
                   WHEN DFHPF3
                        PERFORM 400-FIN
                   WHEN DFHENTER
                        PERFORM 210-PROCESAR-DATOS
              END-EVALUATE
           END-IF.

       210-PROCESAR-DATOS.
      *----------------------------------------------------------------
      *--- VALIDA LOS DATOS DEL MAPA. IDENTIFICA LA OPCION          ---
      *----------------------------------------------------------------
           EVALUATE CAMPO1I
                WHEN 1
                     PERFORM 211-CONSULTA
                WHEN 2
                     PERFORM 212-DEPOSITO
                WHEN 3
                     PERFORM 213-RETIRO
                WHEN 4
                     PERFORM 214-MOVIMIENTOS
                WHEN 5
                     PERFORM 215-BLOQUEO
                WHEN OTHER
                     PERFORM 110-ENVIAR-MAPA-VACIO
                     PERFORM 300-RETURN
           END-EVALUATE.

      *----------------------------------------------------------------
      *--- LLAMAMOS AL PROGRAMA CONSULTA CLIENTE                    ---
      *----------------------------------------------------------------
       211-CONSULTA.
           MOVE 'CLNTCOB'            TO CH-XCTL
           MOVE WS-TRANSACCION       TO CH-TRANSACCION
           MOVE WS-PROGRAMA-RETORNO  TO CH-PROGRAMA-RETORNO
           MOVE WS-TRANSACCION       TO CH-TRANS-RETORNO
           PERFORM 221-XCTL-PROGRAMA.

      *----------------------------------------------------------------
      *--- LLAMAMOS AL PROGRAMA DEP¢SITO EN CUENTA                  ---
      *----------------------------------------------------------------
       212-DEPOSITO.
           MOVE 'DEPOCOB'            TO CH-XCTL
           MOVE WS-TRANSACCION       TO CH-TRANSACCION
           MOVE WS-PROGRAMA-RETORNO  TO CH-PROGRAMA-RETORNO
           MOVE WS-TRANSACCION       TO CH-TRANS-RETORNO
           PERFORM 221-XCTL-PROGRAMA.

      *----------------------------------------------------------------
      *--- LLAMAMOS AL PROGRAMA RETIRO DESDE CUENTA                 ---
      *----------------------------------------------------------------
       213-RETIRO.
           MOVE 'RETRCOB'            TO CH-XCTL
           MOVE WS-TRANSACCION       TO CH-TRANSACCION
           MOVE WS-PROGRAMA-RETORNO  TO CH-PROGRAMA-RETORNO
           MOVE WS-TRANSACCION       TO CH-TRANS-RETORNO
           PERFORM 221-XCTL-PROGRAMA.

      *----------------------------------------------------------------
      *--- LLAMAMOS AL PROGRAMA VER MOVIMIENTOS                     ---
      *----------------------------------------------------------------
       214-MOVIMIENTOS.
           MOVE 'MOVSCOB'            TO CH-XCTL
           MOVE WS-TRANSACCION       TO CH-TRANSACCION
           MOVE WS-PROGRAMA-RETORNO  TO CH-PROGRAMA-RETORNO
           MOVE WS-TRANSACCION       TO CH-TRANS-RETORNO
           PERFORM 221-XCTL-PROGRAMA.

      *----------------------------------------------------------------
      *--- LLAMAMOS AL PROGRAMA BLOQUEO DE CUENTA                   ---
      *----------------------------------------------------------------
       215-BLOQUEO.
           MOVE 'BLOQCOB'            TO CH-XCTL
           MOVE WS-TRANSACCION       TO CH-TRANSACCION
           MOVE WS-PROGRAMA-RETORNO  TO CH-PROGRAMA-RETORNO
           MOVE WS-TRANSACCION       TO CH-TRANS-RETORNO
           PERFORM 221-XCTL-PROGRAMA.

       220-ENVIAR-MAPA.
           EXEC CICS SEND
                MAP('MENUMP')
                ERASE
                FROM(MENUMPO)
                NOHANDLE
           END-EXEC.

       221-XCTL-PROGRAMA.
           EXEC CICS XCTL
                PROGRAM(CH-XCTL)
                COMMAREA(CH-COMMAREA)
           END-EXEC.

       300-RETURN.
           EXEC CICS RETURN
                TRANSID(WS-TRANSACCION)
                COMMAREA(CH-COMMAREA)
                LENGTH(40)
           END-EXEC.

       400-FIN.
           EXEC CICS SEND CONTROL
                ERASE
                FREEKB
           END-EXEC

           EXEC CICS RETURN END-EXEC.


