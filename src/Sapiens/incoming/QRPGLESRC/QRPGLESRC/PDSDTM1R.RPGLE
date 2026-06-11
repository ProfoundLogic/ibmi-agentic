     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*********************************************************************
    *F** THIS EXIT PROGRAM MUST BE REGISTERED ON THE ISERIES USING WRKREGINF
    *F**   FOR EXIT POINT: QIBM_QZRC_RMT
    *F*
     F** THIS FILE MUST BE ACCESSIBLE FROM THE QZRCSRVS JOBS IN ORDER FOR THIS
     F**  TO WORK
     F***  THE LIBRARY LIST OF THE QZRCSRVS JOBS WHEN ATTACHED TO (DFT JOBD OF U
     F*    SERS) IS USED, SO THE FILE THAT IS LOCATED IN THAT LIBRARY WILL BE WR
     F*    WRITTEN TO.  IF A USER DOES NOT HAVE THIS FILE IN THEIR LIBL AN ERRO
     F*    R WILL OCCUR.  SHOULD PUT A COPY IN QGPL SO NON-PDS USERS ARE NOT
     F*    AFFECTED.  VERY IMPORTANT *******************************************
     F*
     F*   ON ASDP03 (AO) THIS PGM WILL ONLY BE EXECUTED FROM ONE LOCATION (LIB)
     F*    SO, WE MUST BE AWARE OF THIS.  WE WILL MAKE THE PDSOBJ099 THE DEFAULT
     F*
     H*********************************************************************
     H* Revisions:
     H*
     H* 07/06/09  C:33  Seq#: DT146/68158
     H*                 Update date/time stamps
     H*********************************************************************
     F*
     FSAIPP     UF A E           K DISK    USROPN

    *D*
     D*
     D* NECESSARY ARRAY DEFINITIONS FOR TRANSFER FUNCTION
     D* AND REMOTE SQL
     D*
     D TFREQ           S              1    DIM(4096)
     D RSREQ           S              1    DIM(4107)
     D*
     D*
     D PCSDTA          DS
     D  USERID                 1     10
     D  APPLID                11     20
     D*
     D* REMOTE COMMAND AND DISTRIBUTED PROGRAM CALL SERVER:
     D*
     D* QIBM_QZRC_RMT  FORMAT CZRC0100
     D*    RCPGM AND RCLIB ARE NOT USED FOR REMOTE COMMAND CALLS
     D*
     D  RCFMT                 21     28
     D  RCFID                 29     32B 0
     D  RCPGM                 33     42
     D  RCLIB                 43     52
     D  RCNUM                 53     56B 0
     D  RCDATA                57    312
     D*
     D****** QUSRJOBI API FORMAT DEFINITION
     D**  FORMAT USED IS JOBI0600 ONLY
     D* -------------------------------------------------------------
     D* Data Structure for API: Retrieve Job Information
     D* -------------------------------------------------------------
     D*
     D* Parameter 1 - Recieve Variable
     D* ------------------------------
     D*  Eight different formats - only JOBI0200 is used
     D RCVVAR          DS
     D* Format JOBI0100
     D*                                   B   1   40BYTRTN
     D*                                   B   5   80BYTVAL
     D*                                       9  34 QJOBNM
     D*                                       9  18 JOBNAM
     D*                                      19  28 USRNAM
     D*                                      29  34 JOBNBR
     D*                                      35  50 INTID
     D*                                      51  60 JOBSTS
     D*                                      61  61 JOBTYP
     D*                                      62  62 SUBTYP
     D*                                      63  64 RESRV
     D*                                   B  65  680RUNPRI
     D*                                   B  69  720TIMSLI
     D*                                   B  73  760DFTWAI
     D*                                      77  86 PURGE
     D* Format JOBI0150
     D*                                   B   1   40BYTRTN
     D*                                   B   5   80BYTVAL
     D*                                       9  34 QJOBNM
     D*                                       9  18 JOBNAM
     D*                                      19  28 USRNAM
     D*                                      29  34 JOBNBR
     D*                                      35  50 INTID
     D*                                      51  60 JOBSTS
     D*                                      61  61 JOBTYP
     D*                                      62  62 SUBTYP
     D*                                      63  64 RESRV
     D*                                   B  65  680RUNPR1
     D*                                   B  69  720TIMSL1
     D*                                   B  73  760DFTWAI
     D*                                      77  86 PURGE
     D*                                      87  96 TSEPOL
     D*                                   B  97 1000CPUUSD
     D*                                   B 101 1040SYSPOL
     D*                                   B 105 1080MAXCPU
     D*                                   B 109 1120TMPSTG
     D*                                   B 113 1160MAXSTG
     D* Format JOBI0200
     D*                                   B   1   40BYTRTN
     D*                                   B   5   80BYTVAL
     D*                                       9  34 QJOBNM
     D*                                       9  18 JOBNAM
     D*                                      19  28 USRNAM
     D*                                      29  34 JOBNBR
     D*                                      35  50 INTID
     D**                                     51  60 JOBSTS
     D*                                      61  61 JOBTYP
     D*                                      62  62 SUBTYP
     D***                                    63  72 SBSNAM
     D*                                   B  73  760RUNPRI
     D*                                   B  77  800POOLID
     D*                                   B  81  840CPUUSE
     D*                                   B  85  880AUXREQ
     D*                                   B  89  920INTRAN
     D*                                   B  93  960RSPTIM
     D*                                      97  97 FNCTYP
     D*                                      98 107 FNCNAM
     D*                                     108 111 ACTJOB
     D* Format JOBI0300
     D*                                   B   1   40BYTRTN
     D*                                   B   5   80BYTVAL
     D*                                       9  34 QJOBNM
     D*                                       9  18 JOBNAM
     D*                                      19  28 USRNAM
     D*                                      29  34 JOBNBR
     D*                                      35  50 INTID
     D*                                      51  60 JOBSTS
     D*                                      61  61 JOBTYP
     D*                                      62  62 SUBTYP
     D*                                      63  72 JOBQ
     D*                                      73  82 JOBQLI
     D*                                      83  84 JOBQPR
     D*                                      85  94 OUTQ
     D*                                      95 104 OUTQLI
     D*                                     105 106 OUTQPR
     D*                                     107 116 PRTDEV
     D*                                     117 142 SBMJOB
     D*                                     117 126 SBMJO1
     D*                                     127 136 SBMUSR
     D*                                     137 142 SBMNBR
     D*                                     143 152 MSGQ
     D*                                     153 162 MSGQLI
     D*                                     163 172 STSJBQ
     D*                                     173 180 DTJOBQ
     D*                                     181 187 JOBDAT
     D* Format JOBI0400
     D*                                   B   1   40BYTRTN
     D*                                   B   5   80BYTVAL
     D*                                       9  34 QJOBNM
     D*                                       9  18 JOBNAM
     D*                                      19  28 USRNAM
     D*                                      29  34 JOBNBR
     D*                                      35  50 INTID
     D*                                      51  60 JOBSTS
     D*                                      61  61 JOBTYP
     D*                                      62  62 SUBTYP
     D*                                      63  75 ENTSYS
     D*                                      76  88 ACTSYS
     D*                                      89 103 ACCCDE
     D*                                     104 113 JOBDNA
     D*                                     114 123 JOBDLI
     D*                                     124 147 WORKID
     D*                                     148 155 MODNAM
     D*                                     156 165 INQMSG
     D*                                     166 175 LOGCLP
     D*                                     176 185 BRKMSG
     D*                                     186 195 STSMSG
     D*                                     196 208 DEVREC
     D*                                     209 218 DDMCON
     D*                                     219 219 DATSEP
     D*                                     220 223 DATFMT
     D*                                     224 253 PRTTXT
     D*                                     254 279 SBMJOB
     D*                                     254 263 SBMJO1
     D*                                     264 273 SBMUSR
     D*                                     274 279 SBMNBR
     D*                                     280 289 SBMQNA
     D*                                     290 299 SBMQLI
     D*                                     300 300 TIMSEP
     D*                                   B 301 3040CCSID
     D*                                     305 312 SCDDT
     D*                                     313 322 PRTKEY
     D*                                     323 332 SRTSQ
     D*                                     333 342 SRTSQL
     D*                                     343 345 LNGID
     D*                                     346 347 CNTYID
     D*                                     348 348 CMPSTS
     D*                                     349 349 GUIJOB
     D*                                     350 357 JOBSWS
     D*                                     358 367 JOBFUL
     D*                                     368 368 RESERV
     D*                                   B 369 3720JOBMXS
     D*                                   B 373 3760DFTCSD
     D* Format JOBI0500
     D*                                   B   1   40BYTRTN
     D*                                   B   5   80BYTVAL
     D*                                       9  34 QJOBNM
     D*                                       9  18 JOBNAM
     D*                                      19  28 USRNAM
     D*                                      29  34 JOBNBR
     D*                                      35  50 INTID
     D*                                      51  60 JOBSTS
     D*                                      61  61 JOBTYP
     D*                                      62  62 SUBTYP
     D*                                      63  64 RESRV
     D*                                   B  65  680ENDSEV
     D*                                   B  69  720LOGSEV
     D*                                      73  73 LOGLVL
     D*                                      74  83 LOGTXT
     D* Format JOBI0600
     D  BYTRTN                 1      4B 0
     D  BYTVAL                 5      8B 0
     D  QJOBNM                 9     34
     D  JOBNAM                 9     18
     D  USENAM                19     28
     D  JOBNBR                29     34
     D  INTID                 35     50
     D  STATUS                51     60
     D  JOBTYP                61     61
     D  SUBTYP                62     62
     D  JOBSWI                63     70
     D  ENDSTS                71     71
     D  SBSNAM                72     81
     D  SBSDLI                82     91
     D  CURUSR                92    101
     D  DBCS                 102    102
     D  EXTKEY               103    103
     D  CNLKEY               104    104
     D  PRDRC                105    108B 0
     D  USRRC                109    112B 0
     D  PGMRC                113    116B 0
     D  SPCENV               117    126
     D  DEVNAM               127    136
     D  GRPPRF               137    146
     D  SGRPPF               147    296
     D  JUSRID               297    306
     D  JUSRIS               307    307
     D  INIPAD               308    322
     D* Format JOBI0700
     D*                                   B   1   40BYTRTN
     D*                                   B   5   80BYTVAL
     D*                                       9  34 QJOBNM
     D*                                       9  18 JOBNAM
     D*                                      19  28 USRNAM
     D*                                      29  34 JOBNBR
     D*                                      35  50 INTID
     D*                                      51  60 JOBSTS
     D*                                      61  61 JOBTYP
     D*                                      62  62 SUBTYP
     D*                                      63  64 RESRV
     D*                                   B  65  680SYSLIB
     D*                                   B  69  720PRDLIB
     D*                                   B  73  760CURLIB
     D*                                   B  77  800USRLIB
     D*                                      81 336 LIB1
     D*                                     337 553 LIB2
     D*
     D* Parameter 2 - Length of RCVVAR
     D* ------------------------------
     D LGTRCV          DS
     D  BIN4A                  1      4B 0 INZ(322)
     D*
     D* Parameter 3 - Format wanted for RCVVAR (one of 4 possibly)
     D* ----------------------------------------------------------
     D FMTNAM          DS
     D  FORMAT                 1      8    INZ('JOBI0600')
     D*
     D* Parameter 4 - Qualified Job Name
     D* --------------------------------
     D QUALJB          DS
     D  IJOBNM                 1     10
     D  IJOBUS                11     20
     D  IJOBNB                21     26
     D*
     D* Parameter 5 - Internal job
     D* --------------------------
     D INTJOB          DS            16
     D*
     D* Parameter 6 - API Error
     D* -----------------------
     D APIERR          DS           256
     D  APILEN                 1      4B 0 INZ(256)
     D  APIERL                 5      8B 0
     D  APIMSG                 9     15
     D  APIDTA                17    256
     D*
     D***  Begin Add  ***  07/06/09  **************************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  X8DATE                 1      8  0
     D  X8MM                   1      2  0
     D  X8DD                   3      4  0
     D  X8CC                   5      6  0
     D  X8YY                   7      8  0
     D                 DS
     D  X7DATE                 1      7  0
     D  X7CC                   1      1  0
     D  X7YY                   2      3  0
     D  X7MM                   4      5  0
     D  X7DD                   6      7  0
     D***  End   Add  ***  07/06/09  **************************************
     D*
     D***************************************************************
     D*
     D DTMPGM          C                   CONST('DT01CHKLIB')
     D #RMTSV          C                   CONST('*RMTSRV   ')
     D*
     D*** WOULD NEED TO CHANGE THIS TO INTERNALLY DESCRIBED... IF USED AS EXIT
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* EXIT PROGRAM CALL PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    RTNCD             1
     C                   PARM                    PCSDTA
     C*
     C***  Begin Add  ***  07/06/09  **************************************
     C                   TIME                    DSDTTM
     C*
     C     *LIKE         DEFINE    DSTIME        X6TIME
     C                   Z-ADD     DSTIME        X6TIME
     C                   Z-ADD     DSDATE        X8DATE
     C                   Z-ADD     X8MM          X7MM
     C                   Z-ADD     X8DD          X7DD
     C                   Z-ADD     X8YY          X7YY
     C     X8CC          SUB       19            X7CC
     C***  End   Add  ***  07/06/09  **************************************
     C*
     C* INITIALIZE RETURN VALUE TO ACCEPT REQUEST
     C*
     C                   MOVE      '1'           RTNCD
     C*
     C* COMMON PROCESSING
     C*
     C*              COMMON LOGIC GOES HERE
     C*
     C* PROCESS BASED ON SERVER ID
     C*
     C     APPLID        CASEQ     #RMTSV        RMTCMD
     C                   ENDCS
     C*
     C                   SETON                                        LR
     C                   RETURN
     C*
     C* SUBROUTINES
     C*
     C*
     C*
     C* REMOTE COMMAND AND DISTRIBUTED PROGRAM CALL
     C*
     C* IN THIS CASE, THE USER "USERALL" WILL NOT BE ALLOWED
     C* TO EXECUTE ANY REMOTE COMMANDS OR REMOTE PROGRAM CALLS
     C*
     C     RMTCMD        BEGSR
     C                   MOVE      Q1JOBN        XXJOBN            6
     C                   MOVEL     USERID        XXUSER           10
     C                   MOVEL     APPLID        XXAPID           10
     C                   MOVE      RCNUM         XX#PRM           10 0
     C*
    C* -----------------------------------
     C*** SET THE QUALJOB PARM FOR THE API.
     C                   MOVEL     '*    '       IJOBNM
     C***                  MOVELQ1JOB     IJOBNM
     C***                  MOVEL'QSYS'    IJOBUS
     C***                  MOVELQ1JOBN    IJOBNB
     C                   EXSR      RTVJOB
     C*
     C     Q1JOB         IFEQ      'QZRCSRVS'
     C     RCPGM         ANDEQ     DTMPGM
     C*
     C     IPOPN         IFNE      'X'
     C                   OPEN      SAIPP
     C                   MOVE      'X'           IPOPN             1            OPEN FLAG SAIPP
     C                   ENDIF
     C*
     C     IPOPN         IFEQ      'X'
     C     Q1USER        CHAIN     PIP01                              66
     C     *IN66         IFEQ      *OFF
     C                   MOVEL     Q1JOB         IPJOB
     C                   MOVEL     Q1JOBN        IPJOBN
     C                   MOVEL     USENAM        IPJUSR                              JOB USER (QUSER
     C                   MOVEL     Q1USER        IPRUSR                              ACTUAL USERID
     C                   MOVEL     INIPAD        IPIPAD
     **
     C*** DATE/TIME STAMP HERE
     C***  Begin Add  ***  07/06/09  **************************************
     C                   Z-ADD     X6TIME        IPTM
     C                   Z-ADD     X7DATE        IPDT
     C***  End   Add  ***  07/06/09  **************************************
     C*
     C                   UPDATE    PIP01
     C*
     C                   ELSE
     C*
     C                   MOVEL     Q1JOB         IPJOB
     C                   MOVEL     Q1JOBN        IPJOBN
     C                   MOVEL     USENAM        IPJUSR
     C                   MOVEL     Q1USER        IPRUSR
     C                   MOVEL     INIPAD        IPIPAD
     **
     C*** DATE/TIME STAMP HERE
     C***  Begin Add  ***  07/06/09  **************************************
     C                   Z-ADD     X6TIME        IPTM
     C                   Z-ADD     X7DATE        IPDT
     C***  End   Add  ***  07/06/09  **************************************
     C*
     C                   WRITE     PIP01
     C*
     C                   ENDIF                                                      *IN66
     C*
     C                   CLOSE     SAIPP
     C                   MOVE      ' '           IPOPN
     C*
     C                   ENDIF                                                      IPOPN
     **
     C                   ENDIF                                                      DTMPGM AND JOB
     **
     C*** EXIT SPECIFIES VALID TRANSACTION (CONTINUE)
     C                   MOVE      '1'           RTNCD
     C                   ENDSR
     C*
     C*
     C* -------------------------------------------------------------
     C* Retrieve Job Information
     C* -------------------------------------------------------------
     C*
     C     RTVJOB        BEGSR
     C***
     C*
     C                   CALL      'QUSRJOBI'
     C                   PARM                    RCVVAR                         User Space
     C                   PARM                    LGTRCV                         Length of Receiver
     C                   PARM                    FMTNAM                         Format Name
     C                   PARM                    QUALJB                         Qualified Job Name
     C                   PARM      *BLANK        INTJOB           16            Internal Job iden.
     C                   PARM                    APIERR                         API Error
     C* If API-Error, reset RCVVAR
     C* --------------------------
     C     APIERL        IFGT      *ZERO
     C                   RESET                   RCVVAR
     C                   ENDIF
     C*
     C                   ENDSR
     C*
