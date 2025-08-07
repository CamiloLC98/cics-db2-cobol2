       01  CONSMPI.
           02  FILLER PIC X(12).
           02  CAMPO1L    COMP  PIC  S9(4).
           02  CAMPO1F    PICTURE X.
           02  FILLER REDEFINES CAMPO1F.
             03 CAMPO1A    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  CAMPO1I  PIC X(10).
           02  CUENTAL    COMP  PIC  S9(4).
           02  CUENTAF    PICTURE X.
           02  FILLER REDEFINES CUENTAF.
             03 CUENTAA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  CUENTAI  PIC X(10).
           02  CEDULAL    COMP  PIC  S9(4).
           02  CEDULAF    PICTURE X.
           02  FILLER REDEFINES CEDULAF.
             03 CEDULAA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  CEDULAI  PIC X(10).
           02  NOMBREL    COMP  PIC  S9(4).
           02  NOMBREF    PICTURE X.
           02  FILLER REDEFINES NOMBREF.
             03 NOMBREA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  NOMBREI  PIC X(30).
           02  SALDOL    COMP  PIC  S9(4).
           02  SALDOF    PICTURE X.
           02  FILLER REDEFINES SALDOF.
             03 SALDOA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  SALDOI  PIC X(15).
           02  ESTADOL    COMP  PIC  S9(4).
           02  ESTADOF    PICTURE X.
           02  FILLER REDEFINES ESTADOF.
             03 ESTADOA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  ESTADOI  PIC X(1).
           02  MSGL    COMP  PIC  S9(4).
           02  MSGF    PICTURE X.
           02  FILLER REDEFINES MSGF.
             03 MSGA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  MSGI  PIC X(60).
       01  CONSMPO REDEFINES CONSMPI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  CAMPO1H    PICTURE X.
           02  CAMPO1O  PIC X(10).
           02  FILLER PICTURE X(3).
           02  CUENTAH    PICTURE X.
           02  CUENTAO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  CEDULAH    PICTURE X.
           02  CEDULAO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  NOMBREH    PICTURE X.
           02  NOMBREO  PIC X(30).
           02  FILLER PICTURE X(3).
           02  SALDOH    PICTURE X.
           02  SALDOO  PIC X(15).
           02  FILLER PICTURE X(3).
           02  ESTADOH    PICTURE X.
           02  ESTADOO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  MSGH    PICTURE X.
           02  MSGO  PIC X(60).
