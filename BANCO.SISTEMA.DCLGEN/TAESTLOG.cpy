      ******************************************************************
      * DCLGEN TABLE(TAESTLOG)                                         *
      *        LIBRARY(BANCO1.SISTEMA.DCLGEN(TAESTLOG))                *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(CL-)                                              *
      *        STRUCTURE(CL-ESTLOG)                                    *
      *        QUOTE                                                   *
      *        DBCSDELIM(NO)                                           *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE TAESTLOG TABLE
           ( ID_LOG                         INTEGER NOT NULL,
             NUMERO_CUENTA_L                CHAR(10) NOT NULL,
             ESTADO_CUENTA                  CHAR(1) NOT NULL,
             ESTADO_NUEVO                   CHAR(1) NOT NULL,
             FECHA_HORA                     TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TAESTLOG                           *
      ******************************************************************
       01  CL-ESTLOG.
      *                       ID_LOG
           10 CL-ID-LOG            PIC S9(9) USAGE COMP.
      *                       NUMERO_CUENTA_L
           10 CL-NUMERO-CUENTA-L   PIC X(10).
      *                       ESTADO_CUENTA
           10 CL-ESTADO-CUENTA     PIC X(1).
      *                       ESTADO_NUEVO
           10 CL-ESTADO-NUEVO      PIC X(1).
      *                       FECHA_HORA
           10 CL-FECHA-HORA        PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
