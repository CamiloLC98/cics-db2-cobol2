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
           EXEC SQL DECLARE TACUENT TABLE
           ( NUMERO_CUENTA                  CHAR(10) NOT NULL,
             CEDULA_CLIENTE                 CHAR(10) NOT NULL,
             NOMBRE_CLIENTE                 CHAR(50) NOT NULL,
             SALDO                          DECIMAL(15, 2) NOT NULL,
             ESTADO_CUENTA                  CHAR(1) NOT NULL
           ) END-EXEC.
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
