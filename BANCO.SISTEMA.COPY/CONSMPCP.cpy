       01  CONSMPI.
           02  FILLER PIC X(12).
           02  CAMPO1L    COMP  PIC  S9(4).
           02  CAMPO1F    PICTURE X.
           02  FILLER REDEFINES CAMPO1F.
             03 CAMPO1A    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  CAMPO1I  PIC X(10).
       01  CONSMPO REDEFINES CONSMPI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  CAMPO1H    PICTURE X.
           02  CAMPO1O  PIC X(10).
