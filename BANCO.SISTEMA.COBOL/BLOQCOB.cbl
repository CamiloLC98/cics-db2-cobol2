      *****************************************************
      *                                                   *
      *   PROGRAMA MENU CICS-DB2 SISTEMA BANCARIO         *
      *                                                   *
      *****************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID. BLOQCOB.
       AUTHOR. CAMILO LOPEZ.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE TACUENT END-EXEC.
           EXEC SQL INCLUDE TAESTLOG END-EXEC.

       01  SWITCHES.
           03  WS-PRIMERA-FALG        PIC X           VALUE 'N'.
               88 WS-PRIMERA-VEZ                      VALUE 'Y'.
       01 WC-CONSTANTES.
          03 WC-PROGRAMA              PIC X(8)     VALUE 'BLOQCOB'.
          03 WC-TRANSACCION           PIC X(4)     VALUE 'BLOQ'.
       01 DB2-ERROR.
          05 DB2-SQLCODE              PIC S9(9).
          05 DB2-SQLCODE-Z            PIC -ZZZZZZZZ9.
          05 DB2-ERROR-MSG.
             06 DB2-ERR-MSG           PIC X(40).
             06 DB2-ERR-CODE          PIC X(20).     

       COPY BLOQMPCP.
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
              MOVE LOW-VALUES TO BLOQMPI
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
           IF EIBCALEN > 0 AND EIBTRNID NOT = 'BLOQ'
              MOVE LOW-VALUES TO BLOQMPI 
              PERFORM 110-ENVIAR-MAPA-VACIO
              SET WS-PRIMERA-VEZ TO TRUE
              PERFORM 300-RETURN
           END-IF.

       110-ENVIAR-MAPA-VACIO.
           EXEC CICS SEND MAP('BLOQMP')
                MAPONLY
                ERASE
                NOHANDLE
           END-EXEC.

       200-PROCESO.
           IF WS-PRIMERA-VEZ
              CONTINUE
           ELSE
      *
      *--- RECUPERAMOS EL MAPA DESDE EL TERMINAL
      *
              EXEC CICS RECEIVE
                   MAP('BLOQMP')
                   INTO(BLOQMPI)
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
           IF CAMPO1I = LOW-VALUES  OR CAMPO2I = LOW-VALUES 
              PERFORM 220-ENVIAR-MAPA
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

           IF SQLCODE = 0
              PERFORM 223-SQL-CONSULTA-ESTADO 
              PERFORM 224-SQL-ACTUALIZAR-ESTADO 
           ELSE
              PERFORM 999-FALLO-FICHERO
           END-IF .
           
       216-VOLVER-MENU.
           MOVE 'MENUPGM'       TO CH-XCTL
           MOVE WC-TRANSACCION  TO CH-TRANSACCION
           MOVE WC-TRANSACCION  TO CH-TRANS-RETORNO
           MOVE WC-PROGRAMA     TO CH-PROGRAMA-RETORNO
           PERFORM 221-XCTL-PROGRAMA.

       220-ENVIAR-MAPA.
           EXEC CICS SEND
                MAP('BLOQMP')
                ERASE
                FROM(BLOQMPO)
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
                INTO
                  :CL-NUMERO-CUENTA
                FROM
                  TACUENT
                WHERE
                  NUMERO_CUENTA = :CL-NUMERO-CUENTA
           END-EXEC.

       223-SQL-CONSULTA-ESTADO.
           MOVE CAMPO1I TO CL-NUMERO-CUENTA-L 
           EXEC SQL
                SELECT
                  NUMERO_CUENTA_L
                 ,ESTADO_CUENTA_L
                 ,ESTADO_NUEVO
                INTO
                  :CL-NUMERO-CUENTA-L
                 ,:CL-ESTADO-CUENTA-L
                 ,:CL-ESTADO-NUEVO
                FROM
                  TAESTLOG
                WHERE
                  NUMERO_CUENTA_L = :CL-NUMERO-CUENTA-L
           END-EXEC
           IF SQLCODE = 0
              CONTINUE
           ELSE 
              PERFORM 999-FALLO-FICHERO 
           END-IF.

       224-SQL-ACTUALIZAR-ESTADO.
           MOVE CAMPO2I          TO CL-ESTADO-NUEVO 
           MOVE CL-ESTADO-NUEVO  TO CL-ESTADO-CUENTA-L 
           MOVE CAMPO1I          TO CL-NUMERO-CUENTA-L 
           EXEC SQL  
                UPDATE TAESTLOG
                SET 
                  ESTADO_CUENTA_L = :CL-ESTADO-CUENTA-L
                 ,ESTADO_NUEVO  = :CL-ESTADO-NUEVO
                 ,FECHA_HORA    =  CURRENT TIMESTAMP
                WHERE
                  NUMERO_CUENTA_L = :CL-NUMERO-CUENTA-L
           END-EXEC
           IF SQLCODE = 0
              MOVE 'ACTUALIZADO CORRECTAMENTE' TO MSGO
              PERFORM 220-ENVIAR-MAPA
              PERFORM 300-RETURN          
           ELSE 
              PERFORM 999-FALLO-FICHERO
           END-IF.
                                
       300-RETURN.
           EXEC CICS RETURN
                TRANSID('BLOQ')
                COMMAREA(CH-COMMAREA )
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
   

