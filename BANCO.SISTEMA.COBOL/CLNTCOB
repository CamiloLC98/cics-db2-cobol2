      *****************************************************
      *                                                   *
      *   PROGRAMA MENU CICS-DB2 SISTEMA BANCARIO         *
      *                                                   *
      *****************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID. CLNTCOB.
       AUTHOR. CAMILO LOPEZ.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  MI-COMMAREA.
           03  CAMPOINICIO            PIC  X(8).
       01  SWITCHES.
           03  WS-PRIMERA-FALG        PIC X           VALUE 'N'.
               88 WS-PRIMERA-VEZ                      VALUE 'Y'.
       COPY CONSMPCP.
       COPY DFHAID.
       COPY DFHBMSCA.

       PROCEDURE DIVISION.
       000-MAIN-LOGIC.
           PERFORM 100-INICIO
           PERFORM 200-PROCESO
           PERFORM 300-RETURN.

       100-INICIO.
           IF EIBCALEN = 0
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

       200-PROCESO.
           IF WS-PRIMERA-VEZ
              CONTINUE
           ELSE
              EXEC CICS RECEIVE
                   MAP('CONSMP')
                   INTO(CONSMPI)
                   NOHANDLE
              END-EXEC

              EVALUATE EIBAID
                   WHEN DFHPF3
                        PERFORM 400-FIN
                   WHEN DFHENTER
                        PERFORM 400-FIN
              END-EVALUATE
           END-IF.

       220-ENVIAR-MAPA.
           EXEC CICS SEND
                MAP('CONSMP')
                ERASE
                FROM(CONSMPO)
                NOHANDLE
           END-EXEC.


       300-RETURN.
           EXEC CICS RETURN
                TRANSID(EIBTRNID)
                COMMAREA(MI-COMMAREA)
                LENGTH(8)
           END-EXEC.

       400-FIN.
           EXEC CICS SEND CONTROL
                ERASE
                FREEKB
           END-EXEC

           EXEC CICS RETURN
           END-EXEC.

