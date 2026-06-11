     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H* *********************************************************   *****   *
     H* **************************************************************      *
     H*                                                                     *
     H* MODULE:  SYSPLR                                                     *
     H*                                                                     *
     H* LANGUAGE:  RPG                                                      *
     H*                                                                     *
     H* FUNCTION:  THIS APPLICATION WILL CREAT A PF OF SPOOLED FILES        *
     H*         MUST BE CALLED FROM CL THAT DOES OVRDBF TO SPECIFIC MBR     *
     H*                                                                     *
     H* APIs USED:  QUSCRTUS, QUSLSPL, QUSRTVUS, QUSRSPLA                   *
     H*                                                                     *
     H* **************************************************************      *
     H* **************************************************************      *
     FSPOOLL    UF A E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D USRSPC          DS
     D  USNAME                 1     10
     D  USLIB                 11     20
     D TGTDAT          DS
     D  TGTCEN                 1      1
     D  TGTYR                  2      3
     D  TGTMTH                 4      5
     D  TGTDAY                 6      7
     D RCVVAR          DS
     D  OFFSET                 1      4B 0
     D  NOENTR                 9     12B 0
     D  LSTSIZ                13     16B 0
     D RCVAR1          DS
     D  USRNM1                 1     10
     D  OUTQNA                11     30
     D  USRDT1                31     40
     D  FRMTY1                41     50
     D  IJOBID                51     66
     D  ISPLID                67     82
     D RCVAR2          DS
     D  BYTRTN                 1      4B 0
     D  BYTVAL                 5      8B 0
     D  JOBID                  9     24
     D  SPLFID                25     40
     D  JOBNAM                41     50
     D  USRNAM                51     60
     D  JOBNUM                61     66
     D  FILNAM                67     76
     D  FILNUM                77     80B 0
     D  FRMTYP                81     90
     D  USRDTA                91    100
     D  STATUS               101    110
     D  FILAVL               111    120
     D  HLDF                 121    130
     D  SAVF                 131    140
     D  TOTPAG               141    144B 0
     D  PAGWRT               145    148B 0
     D  STRPAG               149    152B 0
     D  ENDPAG               153    156B 0
     D  LASPAG               157    160B 0
     D  RESPRT               161    164B 0
     D  TOTCPY               165    168B 0
     D  CPYLFT               169    172B 0
     D  LPI                  173    176B 0
     D  CPI                  177    180B 0
     D  OUTPRI               181    182
     D  OUTQNM               183    192
     D  OUTQLB               193    202
     D  DATFOP               203    209
     D  DATCEN               203    203
     D  DATYR                204    205
     D  DATMTH               206    207
     D  DATDAY               208    209
     D  TIMFOP               210    215
     D  DEVFNA               216    225
     D  DEVFLB               226    235
     D  PGMOPF               236    245
     D  PGMOPL               246    255
     D  ACCCOD               256    270
     D  PRTTXT               271    300
     D  RCDLEN               301    304B 0
     D  MAXRCD               305    308B 0
     D  DEVCLS               309    318
     D  PRTTYP               319    328
     D  DOCNAM               329    340
     D  FLDNAM               341    404
     D  S36PRC               405    412
     D  PRTFID               413    422
     D  RPLUN                423    423
     D  RPLCHR               424    424
     D  PAGLEN               425    428B 0
     D  PAGWID               429    432B 0
     D  NUMSEP               433    436B 0
     D  OVRLIN               437    440B 0
     D  DBCSDA               441    450
     D  DBCSEC               451    460
     D  DBCSSO               461    470
     D  DBCSCR               471    480
     D  DBCSCI               481    484B 0
     D  GRAPHI               485    494
     D  CODPAG               495    504
     D  FORNAM               505    514
     D  FORLIB               515    524
     D  SRCDRW               525    528B 0
     D  PRTFON               529    538
     D  S36SPL               539    544
     D  PAGROT               545    548B 0
     D  JUSTIF               549    552B 0
     D  PRTBOT               553    562
     D  FLDRCD               563    572
     D  CTLCHR               573    582
     D  ALGFRM               583    592
     D  PRTQUA               593    602
     D  FRMFED               603    612
     D  VOLUME               613    683
     D  FLABID               684    700
     D  EXCTYP               701    710
     D  CHRCOD               711    720
     D  TOTRCD               721    724B 0
     D  PGPSID               725    728B 0
     D  FOVNAM               729    738
     D  FOVLIB               739    748
     D  FOVOFD               749    756P 5
     D  FOVOFA               757    764P 5
     D  BOVNAM               765    774
     D  BOVLIB               775    784
     D  BOVOFD               785    792P 5
     D  BOVOFA               793    800P 5
     D  UOM                  801    810
     D  PAGNAM               811    820
     D  PAGLIB               821    830
     D  LINSPC               831    840
     D  PNTSIZ               841    848P 5
     D                 DS
     D  LENDTA                 1      4B 0
     D  STRPOS                 5      8B 0
     D  SPLF#                  9     12B 0
     D  RCVLE1                13     16B 0
     D  FIL#                  17     22
     D  RCVLE2                23     26B 0
     D                 DS
     D  USSIZE                 1      4B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     ISPLFRCD
     I              FILNUM                      SPFIL#
     I              TOTPAG                      SPTOTP
     I              PAGWRT                      SPPWRT
     I              STRPAG                      SPSPAG
     I              ENDPAG                      SPEPAG
     I              LASPAG                      SPLPAG
     I              TOTCPY                      SPTOTC
     I              CPYLFT                      SPCPYL
     I              RCDLEN                      SPRCDL
     I              PAGLEN                      SPPAGL
     I              PAGWID                      SPPAGW
     I              NUMSEP                      SP#SEP
     I              TOTRCD                      SPTOTR

     C* *********************************************************      ******
     C* *********************************************************      ******
     C*                                                                     *
     C*              OPERABLE CODE STARTS HERE                              *
     C*                                                                     *
     C* *********************************************************      ******
     C* *********************************************************      ******
     C*                                                                     *
     C     *ENTRY        PLIST
     C                   PARM                    USRNME           10
     C                   PARM                    OUTQ             20
     C                   PARM                    FORTYP           10
     C                   PARM                    USRDAT           10
     C                   PARM                    MBRNM            10
     C*
     C                   MOVEL     MBRNM         XUSRN            10
     C                   MOVE      '01'          XUSRN
     C                   MOVEL     'QGPL'        XUSRNL           10
     C                   MOVEL     XUSRN         USRSPC           20
     C                   MOVE      XUSRNL        USRSPC
     C*
     C                   MOVEL     'PF      '    EXTATR           10
     C                   Z-ADD     10000         USSIZ            13 5
     C                   MOVEL     ' '           USINIT            1
     C                   MOVEL     '*ALL'        USAUTH           10
     C                   MOVEL     'SPLF LST'    USTEXT           50
     C                   MOVEL     'SPLF0100'    FMTNME            8
     C                   MOVEL     'SPLA0100'    FMTNM1            8
     C*
     C                   Z-ADD     USSIZ         USSIZ1           15 5
     C                   Z-ADD     USSIZ1        USSIZE
     C                   DUMP
     C                   MOVE      FORTYP        FRMTYP
     C                   MOVE      USRDAT        USRDTA
     C                   Z-ADD     0             DLTCNT           15 0
     C                   TIME                    DSDTTM
     C*                                                                     *
     C* CREATE THE USER SPACE USING THE PARAMETERS FROM THE CL              *
     C* COMMAND.                                                            *
     C*                                                               *
     C                   CALL      'QUSCRTUS'
     C                   PARM                    USRSPC
     C                   PARM                    EXTATR
     C                   PARM                    USSIZE
     C                   PARM                    USINIT
     C                   PARM                    USAUTH
     C                   PARM                    USTEXT
     C*                                                               *
     C* FILL THE USER SPACE JUST CREATED WITH SPOOLED FILES AS        *
     C* DEFINED IN THE CL COMMAND.                                    *
     C*                                                               *
     C                   CALL      'QUSLSPL'
     C                   PARM                    USRSPC
     C                   PARM                    FMTNME
     C                   PARM                    USRNME
     C                   PARM                    OUTQ
     C                   PARM                    FRMTYP
     C                   PARM                    USRDTA           10
     C*                                                               *
     C* ***************************************************************
     C* ***************************************************************
     C*                                                               *
     C*     BEGINNING OF LOOP                                         *
     C*                                                               *
     C* ***************************************************************
     C* ***************************************************************
     C*                                                               *
     C* WITHIN A USER SPACE CONTAINING SPOOLED INFORMATION, YOU CAN   *
     C* OFFSET AND SIZE OF EACH ENTRY AND THE NUMBER OF ENTRIES       *
     C* USING THE QUSRTVUS API.                                       *
     C*                                                               *
     C                   Z-ADD     16            LENDTA
     C                   Z-ADD     125           STRPOS
     C*                                                               *
     C                   CALL      'QUSRTVUS'
     C                   PARM                    USRSPC
     C                   PARM                    STRPOS
     C                   PARM                    LENDTA
     C                   PARM                    RCVVAR
     C*                                                               *
     C* CHECK RCVVAR DATA STRUCTURE FOR NUMBER OF LIST ENTRIES, OFFSET*
     C* TO LIST ENTRIES, AND SIZE OF EACH LIST ENTRY.                 *
     C* INFORMATION NEEDED FOR THE QUSLSPL API IS CONTAINED WITHIN    *
     C* THE FIRST 164 BYTES OF FORMAT SPLI0100.                       *
     C* THIS WILL BE DONE FOR EACH ENTRY IN THE USER SPACE.           *
     C*                                                               *
     C                   Z-ADD     OFFSET        STRPOS
     C                   ADD       1             STRPOS
     C                   Z-ADD     LSTSIZ        LENDTA
     C                   Z-ADD     164           RCVLE1                                     CVD
     C*********            Z-ADD209       RCVLE2                       CVD
     C                   Z-ADD     215           RCVLE2                                     CVD
     C                   Z-ADD     1             COUNT            15 0
     C     COUNT         DOWLE     NOENTR
     C*                                                               *
     C* RETRIEVE THE INFORMATION FROM THE USER SPACE ABOUT THE SPOOLED*
     C* FILE.                                                         *
     C*                                                               *
     C                   CALL      'QUSRTVUS'
     C                   PARM                    USRSPC
     C                   PARM                    STRPOS
     C                   PARM                    LENDTA
     C                   PARM                    RCVAR1
     C*                                                               *
     C* NOW RETRIEVE SPOOLED ATTRIBUTES USING THE INFORMATION IN THE  *
     C* USER SPACE, WHICH WAS RETRIEVED BEFORE THIS COMMENT.          *
     C*                                                               *
     C                   MOVE      IJOBID        JOBID
     C                   MOVE      ISPLID        SPLFID
     C                   MOVE      *BLANKS       JOBINF
     C                   MOVEL     '*INT'        SPLFNM           10
     C                   MOVE      *BLANKS       SPLF#
     C                   MOVEL     '*INT'        JOBINF           26
     C*                                                               *
     C                   CALL      'QUSRSPLA'
     C                   PARM                    RCVAR2
     C                   PARM                    RCVLE2
     C                   PARM                    FMTNM1
     C                   PARM                    JOBINF
     C                   PARM                    JOBID
     C                   PARM                    SPLFID
     C                   PARM                    SPLFNM
     C                   PARM                    SPLF#
     C*                                                               *
     C* CHECK RCVAR1 DATA STRUCTURE FOR DATE FILE OPENED.             *
     C*                                                               *
     C* DELETE SPOOLED FILES THAT ARE OLDER THAN THE TARGET DATE      *
     C* SPECIFIED ON THE COMMAND.  A MESSAGE IS SENT FOR EACH SPOOLED *
     C* FILE DELETED.                                                 *
     C*                                                               *
     C*                                                               *
     C**** ADD THIS NEXT LINE TEMP. (DONT EXECUTE THE SUBR.)
     C                   MOVE      '0'           OPTION            1
     C                   EXSR      WRTRCD
     C*                                                               *
     C* GO  BACK AND PROCESS THE REST OF THE ENTRIES IN THE USER      *
     C* SPACE.                                                        *
     C     LSTSIZ        ADD       STRPOS        STRPOS
     C     1             ADD       COUNT         COUNT
     C                   END
     C* ************************************************************* *
     C* ************************************************************* *
     C*                                                               *
     C*             END OF LOOP                                       *
     C*                                                               *
     C* ************************************************************* *
     C* ************************************************************* *
     C*                                                               *
     C*********
     C* DISPLAY THE SCREEN TO DISPLAY SPOOLED ITEMS.
     C*********
     C*  HERE*************
     C*
     C* AFTER ALL SPOOLED FILES ARE DELETED THAT MEET THE             *
     C* REQUIREMENTS, SEND A FINAL MESSAGE TO THE USER.               *
     C* DELETE THE USER SPACE OBJECT THAT WAS CREATED.                *
     C*                                                               *
     C                   MOVE      '2'           OPTION
     C                   EXSR      CLDLT
     C*                                                               *
     C*                                                               *
     C* ************************************************************* *
     C* ************************************************************* *
     C*                                                               *
     C*                      END OF PROGRAM                           *
     C*                                                               *
     C* ************************************************************* *
     C*********            RETRN
     C                   MOVE      '1'           *INLR
     C*
     C* ************************************************************* *
     C*                                                               *
     C*                  CLDLT  SUBROUTINE                            *
     C*                                                               *
     C* THIS SUBROUTINE CALLS A CL PROGRAM THAT EITHER:               *
     C* -- DELETES A SPOOLED FILE AND SENDS A MESSAGE (OPTION = 1).   *
     C* -- SENDS A MESSAGE AND DELETES A USER SPACE OBJECT            *
     C*    (OPTION = 2).                                              *
     C*                                                               *
     C* ************************************************************* *
     C*                                                               *
     C     CLDLT         BEGSR
     C*                                                               *
     C* KEEP A COUNTER OF HOW MANY SPOOLED FILES ARE DELETED.         *
     C*                                                               *
     C     OPTION        IFEQ      '1'
     C                   ADD       1             DLTCNT
     C                   END
     C****
     C                   MOVE      FILNUM        FIL#
     C                   CALL      'CLDLT'
     C                   PARM                    OPTION
     C                   PARM                    USNAME
     C                   PARM                    USLIB
     C                   PARM                    FILNAM
     C                   PARM                    JOBNUM
     C                   PARM                    USRNAM
     C                   PARM                    JOBNAM
     C                   PARM                    FIL#
     C                   PARM                    FRMTYP
     C                   PARM                    USRDTA
     C                   PARM                    DLTCNT
     C                   ENDSR
     C**************************************************************************
     C*     WRTRCD - Write a record to Spool File Interface Workfile       *
     C**************************************************************************
     C     WRTRCD        BEGSR
     C                   MOVE      '1'           *IN99
     C  N99
     CAN 99              READ      SPLFRCD                                60
     C                   Z-ADD     FILNUM        SPFIL#
     C                   Z-ADD     TOTPAG        SPTOTP
     C                   Z-ADD     PAGWRT        SPPWRT
     C                   Z-ADD     STRPAG        SPSPAG
     C                   Z-ADD     ENDPAG        SPEPAG
     C                   Z-ADD     LASPAG        SPLPAG
     C                   Z-ADD     TOTCPY        SPTOTC
     C                   Z-ADD     CPYLFT        SPCPYL
     C                   Z-ADD     RCDLEN        SPRCDL
     C                   Z-ADD     PAGLEN        SPPAGL
     C                   Z-ADD     PAGWID        SPPAGW
     C                   Z-ADD     NUMSEP        SP#SEP
     C                   Z-ADD     TOTRCD        SPTOTR
     C                   MOVE      'A'           RCDSTS
     C                   MOVEL     Q1USER        LMUSER
     C                   MOVEL     'SYSPLR'      LMPGM
     C                   Z-ADD     DSTIME        LMTIME
     C     DSDATE        MULT      10000.01      LMDT
     C                   WRITE     SPLFRCD
     C                   ENDSR
