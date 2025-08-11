      *****************************************************
      *                                                   *
      *   PROGRAMA RETIRO CICS-DB2 SISTEMA BANCARIO       *
      *                                                   *
      *****************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID. RETRCOB.
       AUTHOR. CAMILO LOPEZ.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE TACUENT END-EXEC.
           EXEC SQL INCLUDE TATRANS END-EXEC.

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
          03 WC-PROGRAMA              PIC X(8)     VALUE 'RETRCOB'.
          03 WC-TRANSACCION           PIC X(4)     VALUE 'RETR'.


       COPY RETRMPCP.
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
              MOVE LOW-VALUES TO RETRMPI
              PERFORM 110-ENVIAR-MAPA-VACIO
              SET WS-PRIMERA-VEZ TO TRUE
              PERFORM 300-RETURN
           END-IF.
      *
      *--- HAY COMMAREA.
      *--- EL PROGRAMA HA PODIDO ARRANCAR POR XCTL DESDE OTRO
      *--- PROGRAMA COMO RETORNO ACTUAL.EN ESTE CASO EL CAMPO
      *--- CH-TRANS-RETORNO CONTIENE ALGUN VALOR (TRANSACCIONDE RETORNO)
      *--- EN ESTE CASO SE INICIALIZA EL COMMAREA Y ENVIAMOS
      *--- EL MAPA LIMPIO.
      *
           IF EIBCALEN > 0 AND EIBTRNID NOT = 'RETR'
              MOVE LOW-VALUES TO RETRMPI
              PERFORM 110-ENVIAR-MAPA-VACIO
              SET WS-PRIMERA-VEZ TO TRUE
              PERFORM 300-RETURN
           END-IF.

       110-ENVIAR-MAPA-VACIO.
           EXEC CICS SEND MAP('RETRMP')
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
                   MAP('RETRMP')
                   INTO(RETRMPO)
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
      *--- SI LLEGA AQUI, LOS CAMPOS TIENEN DATOS VILIDOS
      *  
           PERFORM 212-CONSULTAR-CUENTA-DB2.

       212-CONSULTAR-CUENTA-DB2.
      *
      *--- CONSULTAR CUENTA EN LA BASE DE DATOS.
      * 
           MOVE CAMPO1I TO CL-NUMERO-CUENTA
           PERFORM 222-SQL-CONSULTA
      *
      *--- SI EXISTE LA CUENTA EN LA BASE DE DATOS SE RESTA EL VALOR
      *--- DEL MONTO A CL-SALDO Y, SI EL VALOR DEL MONTO ES MAYOR QUE
      *--- CL-SALDO SE CORTA LA EJECUCION CON 300-RETURN Y SE ENVIA
      *--- UN MESAJE
      *     
           IF SQLCODE = 0 
              PERFORM 213-RELLENAR-CAMPO2I
              IF WS-CAMPO2I-NUM > CL-SALDO 
                 MOVE 'SALDO INSUFICIENTE' TO MSGO 
                 PERFORM 220-ENVIAR-MAPA 
                 PERFORM 300-RETURN 
              END-IF 
              SUBTRACT WS-CAMPO2I-NUM FROM CL-SALDO
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
           INSPECT WS-CAMPO2I-JUST REPLACING ALL '_' BY ZEROES
           MOVE WS-CAMPO2I-JUST TO WS-CAMPO2I-NUM. 

       216-VOLVER-MENU.
           MOVE 'MENUPGM'       TO CH-XCTL
           MOVE WC-TRANSACCION  TO CH-TRANSACCION
           MOVE WC-TRANSACCION  TO CH-TRANS-RETORNO
           MOVE WC-PROGRAMA     TO CH-PROGRAMA-RETORNO
           PERFORM 221-XCTL-PROGRAMA.

       220-ENVIAR-MAPA.
           EXEC CICS SEND
                MAP('RETRMP')
                ERASE
                FROM(RETRMPI)
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
                  ,SALDO 
                INTO
                  :CL-NUMERO-CUENTA
                 ,:CL-SALDO
                FROM
                  TACUENT
                WHERE
                  NUMERO_CUENTA = :CL-NUMERO-CUENTA
           END-EXEC.

       223-SQL-ACTUALIZAR-SALDO.
           EXEC SQL
                UPDATE TACUENT
                SET    SALDO = :CL-SALDO
                WHERE NUMERO_CUENTA = :CL-NUMERO-CUENTA
           END-EXEC
           IF SQLCODE = 0
              CONTINUE
           ELSE   
              PERFORM 999-FALLO-FICHERO
           END-IF.   

       224-SQL-CREAR-TRANSACCION.
           MOVE WS-CAMPO2I-NUM TO CL-MONTO
           EXEC SQL
                INSERT INTO TATRANS(
                    NUMERO_CUENTA_T
                   ,TIPO_TRANSACCION
                   ,MONTO 
                   ,FECHA_HORA
                ) VALUES (
                    :CL-NUMERO-CUENTA
                   ,'R'
                   ,:CL-MONTO
                   ,CURRENT TIMESTAMP
                )
           END-EXEC
           IF SQLCODE = 0
              MOVE 'RETIRO EXITOSO' TO MSGO  
              PERFORM 220-ENVIAR-MAPA
              PERFORM 300-RETURN
           ELSE
              PERFORM 999-FALLO-FICHERO
           END-IF.      

       300-RETURN.
           EXEC CICS
                RETURN
                TRANSID('RETR')
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


