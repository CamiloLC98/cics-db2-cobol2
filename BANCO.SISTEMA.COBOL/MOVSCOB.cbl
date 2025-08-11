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
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE TATRANS END-EXEC.
      *
      *--- DECLARACION DE CURSOR PARA LEER SECUENCIALMENTE LA TABLA
      *--- TATRANS
      *     
           EXEC SQL 
                DECLARE C_TATRANS CURSOR FOR
                SELECT 
                   TIPO_TRANSACCION
                  ,MONTO
                  ,FECHA_HORA
                  ,NUMERO_CUENTA_T
                FROM
                   TATRANS
                WHERE
                   NUMERO_CUENTA_T = :CL-NUMERO-CUENTA-T
           END-EXEC.

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

       LINKAGE SECTION. 
       01 DFHCOMMAREA                    PIC X(40).

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
           EXEC SQL OPEN C_TATRANS END-EXEC
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
           EXEC SQL CLOSE C_TATRANS END-EXEC
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
           EXEC SQL 
                FETCH C_TATRANS INTO
                  :CL-TIPO-TRANSACCION
                 ,:CL-MONTO
                 ,:CL-FECHA-HORA
                 ,:CL-NUMERO-CUENTA-T
            END-EXEC.

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