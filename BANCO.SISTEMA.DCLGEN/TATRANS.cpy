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
           EXEC SQL DECLARE TATRANS TABLE
           ( ID_TRANSACTION                 INTEGER NOT NULL,
             NUMERO_CUENTA_T                CHAR(10) NOT NULL,
             TIPO_TRANSACCION               CHAR(1) NOT NULL,
             MONTO                          DECIMAL(15, 2),
             FECHA_HORA                     TIMESTAMP NOT NULL
           ) END-EXEC.
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
