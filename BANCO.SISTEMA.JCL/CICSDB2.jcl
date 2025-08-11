//CICSDB2  JOB BANCO1,'COMP-CICS-DB2',NOTIFY=&SYSUID,                   00010003
//            MSGCLASS=A,CLASS=A,MSGLEVEL=(1,1),REGION=0M               00020000
//*                                                                     00021000
//* VERSION 1.1                                                         00022000
//*                                                                     00023000
//*================================================================     00030000
//*                                                                     00040000
//SETPG   SET PGM=BLOQCOB                                               00060007
//*                                                                     00060100
//SETPG   SET PLN=BLOQPL                                                00060207
//*                                                                     00061000
//*----------------------------------------------------------------     00070000
//*       DB2 PRECOMPILE                                                00090000
//*----------------------------------------------------------------     00100000
//DB2      EXEC PGM=DSNHPC,                                             00110000
//         PARM='HOST(IBMCOB),XREF,SOURCE,FLAG(W),APOST',               00120000
//         REGION=8M                                                    00130000
//*                                                                     00140000
//STEPLIB  DD  DISP=SHR,DSN=DSN910.DB9G.SDSNEXIT                        00150000
//         DD  DISP=SHR,DSN=DSN910.SDSNLOAD                             00160000
//SYSIN    DD  DSN=BANCO1.SISTEMA.COBOL(&PGM),DISP=SHR                  00170003
//DBRMLIB  DD  DSN=BANCO1.SISTEMA.DBRMLIB(&PGM),DISP=SHR                00180003
//SYSCIN   DD  DSN=BANCO1.SISTEMA.DSNHOUT(&PGM),DISP=SHR                00190003
//SYSLIB   DD  DSN=BANCO1.SISTEMA.COPY,DISP=SHR                         00200003
//         DD  DSN=BANCO1.SISTEMA.DCLGEN,DISP=SHR                       00210003
//         DD  DSN=BANCO1.SISTEMA.COBOL,DISP=SHR                        00220003
//         DD  DSN=DFH410.CICS.SDFHCOB,DISP=SHR                         00230000
//SYSPRINT DD  SYSOUT=A                                                 00240000
//SYSTERM  DD  SYSOUT=A                                                 00250000
//SYSUT1   DD  SPACE=(CYL,(5,1)),UNIT=SYSDA                             00260000
//SYSUT2   DD  SPACE=(CYL,(5,1)),UNIT=SYSDA                             00270000
//*                                                                     00280000
//*----------------------------------------------------------------     00281000
//*       CICS TRANSLATOR                                               00290000
//*----------------------------------------------------------------     00300000
//CICS    EXEC PGM=DFHECP1$,REGION=8M,                                  00310000
//        PARM=(COBOL3,NOSOURCE,SP),                                    00320000
//        COND=(0,NE,DB2)                                               00330000
//*                                                                     00340000
//STEPLIB  DD  DISP=SHR,DSN=DFH410.CICS.SDFHLOAD                        00350000
//DSCTLIB  DD  DISP=SHR,DSN=DFH410.CICS.SDFHCOB                         00360000
//SYSIN    DD  DSN=BANCO1.SISTEMA.DSNHOUT(&PGM),DISP=SHR                00370003
//SYSPUNCH DD  DISP=(NEW,PASS),DSN=&&CICSOUT0,UNIT=SYSDA,               00380000
//             SPACE=(800,(500,500)),DCB=BLKSIZE=400                    00390000
//SYSPRINT DD  SYSOUT=A                                                 00400000
//SYSOUT   DD  SYSOUT=A                                                 00410000
//*                                                                     00420000
//*----------------------------------------------------------------     00421000
//*       COBOL COMPILER                                                00430000
//*----------------------------------------------------------------     00440000
//COB    EXEC PGM=IGYCRCTL,REGION=8M,                                   00450000
//       PARM=(APOST,LIB,DYNAM)                                         00451000
//STEPLIB  DD DSN=IGY420.SIGYCOMP,DISP=SHR                              00530000
//         DD DSN=DFH410.CICS.SDFHLOAD,DISP=SHR                         00540000
//SYSLIB   DD DSN=DFH410.CICS.SDFHCOB,DISP=SHR                          00550000
//         DD DSN=BANCO1.SISTEMA.COBOL,DISP=SHR                         00590003
//         DD DSN=BANCO1.SISTEMA.COPY,DISP=SHR                          00600003
//         DD DSN=BANCO1.SISTEMA.MAPS,DISP=SHR                          00610003
//         DD DSN=BANCO1.SISTEMA.DCLGEN,DISP=SHR                        00620003
//SYSPRINT DD SYSOUT=A                                                  00630000
//SYSIN    DD DSN=&&CICSOUT0,DISP=(OLD,DELETE)                          00640000
//SYSLIN   DD DSN=&&LOADSET,DISP=(NEW,PASS),UNIT=SYSDA,                 00650000
//            SPACE=(800,(500,500))                                     00660000
//SYSUT1   DD UNIT=SYSDA,SPACE=(460,(350,100))                          00670000
//SYSUT2   DD UNIT=SYSDA,SPACE=(460,(350,100))                          00680000
//SYSUT3   DD UNIT=SYSDA,SPACE=(460,(350,100))                          00690000
//SYSUT4   DD UNIT=SYSDA,SPACE=(460,(350,100))                          00700000
//SYSUT5   DD UNIT=SYSDA,SPACE=(460,(350,100))                          00710000
//SYSUT6   DD UNIT=SYSDA,SPACE=(460,(350,100))                          00720000
//SYSUT7   DD UNIT=SYSDA,SPACE=(460,(350,100))                          00730000
//*                                                                     00740000
//*----------------------------------------------------------------     00741000
//*      COPYLINK  (INCLUIR DSNCLI)                                     00750000
//*----------------------------------------------------------------     00760000
//CPLK     EXEC PGM=IEBGENER,REGION=8M,                                 00770000
//       COND=((0,NE,DB2),(0,NE,CICS),(0,NE,COB))                       00780000
//*                                                                     00790000
//SYSUT1   DD DSN=BANCO1.SISTEMA.JCL(DFHEILID),DISP=SHR                 00801003
//SYSUT2   DD DSN=&&COPYLINK,DISP=(NEW,PASS),                           00810000
//            DCB=(LRECL=80,BLKSIZE=400,RECFM=FB),                      00820000
//            UNIT=SYSDA,SPACE=(400,(20,20))                            00830000
//SYSPRINT DD SYSOUT=A                                                  00840000
//SYSIN    DD DUMMY                                                     00850000
//*                                                                     00860000
//*----------------------------------------------------------------     00861000
//*     LINK EDITION                                                    00870000
//*----------------------------------------------------------------     00880000
//LKED   EXEC PGM=IEWL,REGION=8M,                                       00890000
//            PARM='LIST,XREF,CALL,AMODE=31,RMODE=ANY',                 00900000
//       COND=((0,NE,DB2),(0,NE,CICS),(0,NE,COB),(0,NE,CPLK))           00910000
//*                                                                     00920001
//SYSLIB   DD  DSN=DFH410.CICS.SDFHLOAD,DISP=SHR                        00930000
//         DD  DSN=CEE.SCEELKED,DISP=SHR                                00930100
//         DD  DSN=DSN910.DB9G.SDSNEXIT,DISP=SHR                        00931000
//         DD  DSN=DSN910.SDSNLOAD,DISP=SHR                             00932000
//         DD  DSN=TCPIP.SEZATCP,DISP=SHR                               01000000
//         DD  DISP=SHR,DSN=ISP.SISPLOAD                                01010000
//         DD  DISP=SHR,DSN=GDDM.SADMMOD                                01020000
//SYSLIN   DD  DSN=&&COPYLINK,DISP=(OLD,DELETE)                         01030001
//         DD  DSN=&&LOADSET,DISP=(OLD,DELETE)                          01040001
//         DD  DDNAME=SYSIN                                             01050001
//SYSLMOD  DD DSN=BANCO1.SISTEMA.LOAD(&PGM),DISP=SHR                    01060003
//SYSUT1   DD UNIT=SYSDA,DCB=BLKSIZE=1024,                              01070000
//            SPACE=(1024,(200,20))                                     01080000
//SYSPRINT DD SYSOUT=A                                                  01090000
//*                                                                     01091000
//*-----------------------------------------------------------------    01100000
//*        DB2 BIND PACKAGE APPHOL -> DBRM -> COLLECTION 'APPHOL'       01110000
//*-----------------------------------------------------------------    01111000
//BINDPK   EXEC PGM=IKJEFT01,REGION=8M,DYNAMNBR=20,                     01130000
//   COND=((0,NE,DB2),(0,NE,CICS),(0,NE,COB),(0,NE,CPLK),               01140000
//        (0,NE,LKED))                                                  01150000
//*                                                                     01160000
//STEPLIB  DD  DISP=SHR,DSN=DSN910.SDSNLOAD                             01170000
//DBRMLIB  DD  DISP=SHR,DSN=BANCO1.SISTEMA.DBRMLIB                      01190003
//SYSPRINT DD  SYSOUT=A                                                 01200000
//SYSTSPRT DD  SYSOUT=A                                                 01210000
//SYSUDUMP DD  SYSOUT=A                                                 01220000
//SYSOUT   DD  SYSOUT=A                                                 01221000
//REPORT   DD  SYSOUT=A                                                 01222000
//SYSTSIN  DD  DSN=BANCO1.SISTEMA.BIND(&PGM),DISP=SHR                   01231003
//*                                                                     01240000
//*-----------------------------------------------------------------    01250000
//*        DB2 BIND PLAN COLECCION APPHOL = LISTA PACKAGES              01260000
//*-----------------------------------------------------------------    01270000
//*                                                                     01280000
//BINDPL   EXEC PGM=IKJEFT01,REGION=8M,DYNAMNBR=20,                     01290000
//   COND=((0,NE,DB2),(0,NE,CICS),(0,NE,COB),(0,NE,CPLK),               01291000
//        (0,NE,LKED),(0,NE,BINDPK))                                    01292000
//*                                                                     01300000
//STEPLIB  DD  DISP=SHR,DSN=DSN910.SDSNLOAD                             01310000
//         DD  DISP=SHR,DSN=DSN910.DB9G.SDSNEXIT                        01320000
//         DD  DISP=SHR,DSN=BANCO1.SISTEMA.LOAD                         01330003
//DBRMLIB  DD  DISP=SHR,DSN=BANCO1.SISTEMA.DBRMLIB                      01340003
//SYSPRINT DD  SYSOUT=A                                                 01350000
//SYSTSPRT DD  SYSOUT=A                                                 01360000
//SYSTSPRT DD  SYSOUT=A                                                 01370000
//SYSTSIN  DD  DSN=BANCO1.SISTEMA.BIND(&PLN),DISP=SHR                   01380003
//*                                                                     01390000
