     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0681R - COMMISSION DETAIL SUMMARY
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  05/28/98 C:91  Project: WC577  (NO COMMENTS)                 *
     H*           C:33  COMBINED ALL SEARCHES INTO ONE F10 SEARCH KEY *
     H*                 CHANGED SCREENS TO CURRENT STANDARDS          *
     H*****************************************************************
     FW0681F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FWTABL     IF   E           K DISK
     FWMACL8    IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAAP     IF   E             DISK
     F                                     RENAME(PAA01:PAA02)
     FWMEML     IF   E           K DISK
     F                                     INFDS(EMFDS)
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(FNFDS)
     FWMUPL     IF   E           K DISK

     D*
     D*   SCREEN AND SAVE FIELD ARRAYS
     D*
     D COD             S              2    DIM(199)                             COMM CODE
     D TYP             S              1    DIM(199)                             COMM CODE TYPE
     D DES             S             40    DIM(199)                             COMM DESC
     D AMT             S             11  2 DIM(199)                             COMM AMOUNT
     D*
     D*   ROLL UP AND ROLL DOWN SAVE FIELD
     D*
     D KEYC            S              3  0 DIM(2)                               EMPLOYER #
     D*
     D*   ROLL UP AND ROLL DOWN FLAGS
     D*
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D EMFDS           DS
     D  EMRRN                397    400B 0
     D FNFDS           DS
     D  FNRRN                397    400B 0
     D*
     D OPTNM           C                   CONST('Commission Detail -
     D                                     Summary')
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  CD                     1     20
     D                                     DIM(10)                              COMM CODE
     D  SCCD01                 1      2
     D  SCCD02                 3      4
     D  SCCD03                 5      6
     D  SCCD04                 7      8
     D  SCCD05                 9     10
     D  SCCD06                11     12
     D  SCCD07                13     14
     D  SCCD08                15     16
     D  SCCD09                17     18
     D  SCCD10                19     20
     D                 DS
     D  TP                     1     10
     D                                     DIM(10)                              COMM CODE TYPE
     D  SCTP01                 1      1
     D  SCTP02                 2      2
     D  SCTP03                 3      3
     D  SCTP04                 4      4
     D  SCTP05                 5      5
     D  SCTP06                 6      6
     D  SCTP07                 7      7
     D  SCTP08                 8      8
     D  SCTP09                 9      9
     D  SCTP10                10     10
     D                 DS
     D  DS                     1    400
     D                                     DIM(10)                              COMM DESC
     D  SCDS01                 1     40
     D  SCDS02                41     80
     D  SCDS03                81    120
     D  SCDS04               121    160
     D  SCDS05               161    200
     D  SCDS06               201    240
     D  SCDS07               241    280
     D  SCDS08               281    320
     D  SCDS09               321    360
     D  SCDS10               361    400
     D                 DS
     D  AM                     1    110  2
     D                                     DIM(10)                              COMM AMOUNT
     D  SCAM01                 1     11  2
     D  SCAM02                12     22  2
     D  SCAM03                23     33  2
     D  SCAM04                34     44  2
     D  SCAM05                45     55  2
     D  SCAM06                56     66  2
     D  SCAM07                67     77  2
     D  SCAM08                78     88  2
     D  SCAM09                89     99  2
     D  SCAM10               100    110  2
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  GROUP MASTER
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  AGENT COMMISSION FILE
     C*
     C     ACKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCAGT#
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     ACKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCAGT#
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C*  AGENT ASSIGNMENT FILE BY FUND/AGENT
     C*
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCAGT#
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C*  TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*_______________________________________________________________
     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C     *LIKE         DEFINE    CSKEY         XXKEY
     C     *LIKE         DEFINE    @@PRDS        QQPRDS
     C     *LIKE         DEFINE    @@CALL        QQCALL
     C     *LIKE         DEFINE    @@EMP#        QQEMP#
     C     *LIKE         DEFINE    @@DIV         QQDIV
     C     *LIKE         DEFINE    @@AGT#        QQAGT#
     C     *LIKE         DEFINE    @@RTCD        QQRTCD
     C     *LIKE         DEFINE    Q2RECF        SVRECF
     C     *LIKE         DEFINE    EMEFD1        XXPODT
     C     *LIKE         DEFINE    EMEFD1        XXFRDT
     C     *LIKE         DEFINE    EMEFD1        XXTODT
     C     *LIKE         DEFINE    DSTIME        SCTIME
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    ACPROD        XXPROD
     C     *LIKE         DEFINE    ACTIME        XXTIME
     C     *LIKE         DEFINE    ACFYR         XXFYR
     C     *LIKE         DEFINE    SCPBD         XXPBD
     C     *LIKE         DEFINE    SCPPD         XXPPD
     C     *LIKE         DEFINE    SCCBD         XXCBD
     C     *LIKE         DEFINE    SCCPD         XXCPD
     C     *LIKE         DEFINE    SCPBAL        XXPBAL
     C     *LIKE         DEFINE    SCCBAL        XXCBAL
     C*_______________________________________________________________
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@PRDS           40
     C                   PARM                    @@CALL            1
     C                   PARM                    @@EMP#            9
     C                   PARM                    @@DIV             5
     C                   PARM                    @@AGT#            5
     C                   PARM                    @@RTCD            1
     C*_______________________________________________________________
     C*
     C                   CLEAR                   QQPRDS
     C                   CLEAR                   QQCALL
     C                   CLEAR                   QQEMP#
     C                   CLEAR                   QQDIV
     C                   CLEAR                   QQAGT#
     C                   CLEAR                   QQRTCD
     C*
     C     Q1PRMS        IFGE      1
     C                   MOVEL(P)  @@PRDS        QQPRDS
     C                   ENDIF                                                  Q1PRMS>=1
     C*
     C     Q1PRMS        IFGE      2
     C                   MOVEL(P)  @@CALL        QQCALL
     C                   ENDIF                                                  Q1PRMS>=2
     C*
     C     Q1PRMS        IFGE      4
     C                   MOVEL(P)  @@EMP#        QQEMP#
     C                   MOVEL(P)  @@DIV         QQDIV
     C                   ENDIF                                                  Q1PRMS>=4
     C*
     C     Q1PRMS        IFGE      5
     C                   MOVEL(P)  @@AGT#        QQAGT#
     C                   ENDIF                                                  Q1PRMS>=5
     C*
     C     Q1PRMS        IFGE      6
     C                   MOVEL(P)  @@RTCD        QQRTCD
     C                   ENDIF                                                  Q1PRMS>=6
     C*_______________________________________________________________
     C*
     C                   TIME                    DSDTTM
     C*
     C     QQPRDS        IFEQ      *BLANK
     C                   MOVEL(P)  OPTNM         QQPRDS
     C                   ENDIF                                                  QQPRDS=*BLANK
     C*
     C                   MOVEL(P)  QQPRDS        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCPRDS
     C*
     C     QQCALL        COMP      '1'                                    09
     C*_______________________________________________________________
     C*
     C     TOP           TAG
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     Q1USER        CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     *ZERO         UPCO#
     C                   Z-ADD     *ZERO         UPFND
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   Z-ADD     UPCO#         XXCO#
     C                   Z-ADD     UPFND         XXFND
     C*
     C*  GET THE COMPANY AND GROUP RECORDS
     C*
     C     XXCO#         CHAIN     PCO01                              60
     C*
     C                   MOVEL(P)  CONAME        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C                   Z-ADD     XXFND         SCFND
     C                   MOVEL(P)  FNNAME        SCNAME
     C*
     C     QQEMP#        IFEQ      *BLANK
     C     QQDIV         ANDEQ     *BLANK
     C                   MOVE      *ON           *IN15
     C                   Z-ADD     UPDIV         SCDIV
     C                   Z-ADD     UPEMP#        SCEMP#
     C                   ELSE
     C                   MOVE      QQDIV         SCDIV
     C                   MOVE      QQEMP#        SCEMP#
     C                   MOVE      QQAGT#        SCAGT#
     C                   ENDIF                                                  QQEMP#=*BLANK
     C*
     C*  LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C*_______________________________________________________________
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C                   SELECT
     C*
     C     Q2RECF        WHENEQ    'SCR01'
     C                   EXFMT     SCR01
     C*
     C     Q2RECF        WHENEQ    'SCR02'
     C*
     C     XXUP          IFEQ      *ON
     C                   MOVE      *BLANKS       SCUP
     C                   ELSE
     C                   MOVEL     RR(1)         SCUP
     C                   ENDIF                                                  XXUP=*ON
     C*
     C     XXDOWN        IFEQ      *ON
     C                   MOVE      *BLANKS       SCDOWN
     C                   ELSE
     C                   MOVEL     RR(2)         SCDOWN
     C                   ENDIF                                                  XXDOWN=*ON
     C*
     C                   EXFMT     SCR02
     C*
     C                   ENDSL
     C*_______________________________________________________________
     C*
     C*  PROCESS ALL F KEYS
     C*
     C     *IN11         IFEQ      *ON
     C                   EXSR      FKEYS
     C                   ENDIF                                                  *IN11=*ON
     C*_______________________________________________________________
     C*
     C*  GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*_______________________________________________________________
     C*
     C*  PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG                                                    ERRCK1
     C*
     C*  VALIDATE THE AGENT# FOR THE FUND & GET AGENT NAME
     C*
     C     AAKY          CHAIN     PAA01                              23
     C*
     C     *IN23         CABEQ     '1'           STRT
     C*
     C                   MOVEL(P)  AANAME        SCANAM
     C*
     C*  EMP# & DIV IF NOT EQUAL TO 0: VALIDATE EMP#, DIV, & AGT#
     C*
     C                   MOVE      *OFF          *IN30
     C                   CLEAR                   SCENAM
     C                   CLEAR                   SCEDN
     C*
     C     SCEMP#        IFNE      *ZERO
     C     SCDIV         ORNE      *ZERO
     C*
     C*  THE EMPLOYER AND DIV MUST BE ON FILE
     C*
     C                   Z-ADD     SCEMP#        XXEMP#
     C                   Z-ADD     SCDIV         XXDIV
     C*
     C     EMKY          CHAIN     PEM01                              21
     C*
     C     *IN21         CABEQ     '1'           STRT
     C*
     C                   MOVEL(P)  EMNAME        SCENAM
     C*
     C*  COMPRESS EMP#/DIV/NAME
     C*
     C                   CALL      'WA005R'
     C                   PARM                    XXEMP#
     C                   PARM                    XXDIV
     C                   PARM                    EMNAME
     C                   PARM                    XXEDN            80
     C*
     C                   MOVEL(P)  XXEDN         SCEDN
     C*
     C*  VALIDATE THE AGENT NUMBER FOR EMP# AND DIV
     C*
     C     AHKY          CHAIN     PAH01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     SCAGT#        IFEQ      AHAGT#
     C                   LEAVE
     C                   ENDIF                                                  SCAGT#=AHAGT#
     C*
     C     AHKY          READE     PAH01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C     *IN60         CABEQ     *ON           STRT                     26
     C*
     C                   MOVE      *ON           *IN30                          EMP#<>*ZERO
     C*
     C                   ENDIF                                                  SCEMP#<>*ZERO
     C*
     C*  VALIDATE DATE
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C     SRERR         CABEQ     *ON           STRT                     22
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         CABEQ     *ON           STRT                     24
     C*
     C*  DATES MUST BE IN ASCENDING ORDER
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C     XXFRDT        CABGT     XXTODT        STRT                     25
     C*
     C*   INITIALIZE VARIABLES
     C*
     C                   Z-ADD     *ZERO         XXPBD
     C                   Z-ADD     *ZERO         XXCBD
     C                   Z-ADD     *ZERO         XXPPD
     C                   Z-ADD     *ZERO         XXCPD
     C                   Z-ADD     *ZERO         XXPBAL
     C                   Z-ADD     *ZERO         XXCBAL
     C                   CLEAR                   COD
     C                   CLEAR                   TYP
     C                   CLEAR                   DES
     C                   CLEAR                   AMT
     C*
     C*  LOAD STATISTICS
     C*
     C                   MOVE      *BLANKS       XXKEY
     C*
     C     TBKY          SETLL     PCS01
     C*
     C                   Z-ADD     1             X                 3 0
     C*
     C     XXCO#         READE     PCS01                                  51
     C*
     C     *IN51         DOWEQ     *OFF
     C*
     C                   MOVEL     CSKEY         COD(X)
     C                   MOVEL     CSTYPE        TYP(X)
     C                   MOVEL     CSDESC        DES(X)
     C*
     C     XXCO#         READE     PCS01                                  51
     C*
     C     X             IFLT      199
     C                   ADD       1             X
     C                   ELSE
     C                   LEAVE
     C                   ENDIF                                                  X<199
     C*
     C                   ENDDO                                                  *IN51=*OFF
     C*
     C*  CALCULATE PREVIOUS AND CURRENT EARNINGS AND PAYMENTS
     C*
     C     *IN30         IFEQ      *ON
     C                   Z-ADD     SCEMP#        XXEMP#
     C                   Z-ADD     SCDIV         XXDIV
     C     ACKY          CHAIN     PAC01                              60
     C                   ELSE
     C     ACKY2         CHAIN     PAC01                              60
     C                   ENDIF                                                  *IN30=*ON
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C*  PREVIOUS BILLED AND PREVIOUS PAID
     C*
     C     ACPODT        IFLT      XXFRDT
     C*
     C                   SELECT
     C     ACCOTP        WHENEQ    'C'
     C     ACCOTP        OREQ      'A'
     C                   ADD       ACAMT         XXPBD
     C     ACCOTP        WHENEQ    'P'
     C                   ADD       ACAMT         XXPPD
     C     ACCOTP        WHENEQ    'V'
     C                   SUB       ACAMT         XXPPD
     C                   ENDSL
     C*
     C                   ENDIF                                                  ACPODT<XXFRDT
     C*
     C*  CURRENT BILLED AND CURRENT PAID
     C*
     C     ACPODT        IFGE      XXFRDT
     C     ACPODT        ANDLE     XXTODT
     C*
     C                   Z-ADD     1             X
     C*
     C     ACCOCD        LOOKUP    COD(X)                                 60
     C*
     C     *IN60         IFEQ      *ON
     C                   ADD       ACAMT         AMT(X)
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   SELECT
     C     ACCOTP        WHENEQ    'C'
     C     ACCOTP        OREQ      'A'
     C                   ADD       ACAMT         XXCBD
     C     ACCOTP        WHENEQ    'P'
     C                   ADD       ACAMT         XXCPD
     C     ACCOTP        WHENEQ    'V'
     C                   SUB       ACAMT         XXCPD
     C                   ENDSL
     C*
     C                   ENDIF                                                  ACPODT>=XXFRDT
     C*
     C   30ACKY          READE     PAC01                                  60
     C  N30ACKY2         READE     PAC01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C*  CALCULATE PREVIOUS AND CURRENT BALANCE
     C*
     C     XXPBD         SUB       XXPPD         XXPBAL
     C     XXCBD         SUB       XXCPD         XXCBAL
     C*
     C*  LOAD STATS INTO SCREEN VARIABLES
     C*
     C                   Z-ADD     XXPBD         SCPBD
     C                   Z-ADD     XXCBD         SCCBD
     C                   Z-ADD     XXPPD         SCPPD
     C                   Z-ADD     XXCPD         SCCPD
     C                   Z-ADD     XXPBAL        SCPBAL
     C                   Z-ADD     XXCBAL        SCCBAL
     C*
     C*  LOAD THE SEARCH SCREEN
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   Z-ADD     1             X
     C                   MOVE      *ON           *IN60
     C*
     C                   EXSR      LOAD
     C*
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C*
     C                   GOTO      STRT
     C*_______________________________________________________________
     C*
     C*  PROCESS SCREEN 2 - PAYMENT INFORMATION
     C*
     C     SCR02T        TAG                                                    ERRCK2
     C*
     C*  INVALID ROLL REQUEST
     C*
     C                   MOVE      *OFF          *IN20
     C*
     C   18XXUP          CABNE     *BLANK        STRT                 2020
     C*
     C   19XXDOWN        CABNE     *BLANK        STRT                 2020
     C*
     C*  PAGE DOWN REQUEST
     C*
     C     *IN18         IFEQ      *ON
     C                   MOVE      *BLANK        XXDOWN
     C     KEYC(2)       ADD       1             X
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN18=*ON
     C*
     C*  PAGE UP REQUEST
     C*
     C     *IN19         IFEQ      *ON
     C                   MOVE      *BLANK        XXUP
     C     KEYC(1)       SUB       1             X
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN19=*ON
     C*_______________________________________________________________
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG                                                    PGMEND
     C*
     C     Q1PRMS        IFGE      6
     C                   MOVE      QQRTCD        @@RTCD
     C                   ENDIF                                                  Q1PRMS>=6
     C*
     C                   MOVE      *ON           *INLR
     C*_______________________________________________________________
     C*
     C*===============================================================
     C* CLR    - CLEAR SCREEN ARRAYS
     C*===============================================================
     C*
     C     CLR           BEGSR
     C*
     C                   CLEAR                   CD
     C                   CLEAR                   TP
     C                   CLEAR                   DS
     C                   CLEAR                   AM
     C                   Z-ADD     *ZERO         I1                3 0
     C*
     C                   ENDSR                                                  CLR
     C*
     C*===============================================================
     C* FILL   - FILL SCREEN ARRAYS
     C*===============================================================
     C*
     C     FILL          BEGSR
     C*
     C                   MOVEL     COD(X)        CD(I1)
     C                   MOVEL     TYP(X)        TP(I1)
     C                   MOVEL     DES(X)        DS(I1)
     C                   Z-ADD     AMT(X)        AM(I1)
     C                   MOVE      *ON           XXOK              1
     C*
     C                   SELECT
     C     I1            WHENEQ    1
     C                   MOVE      X             KEYC(1)
     C     I1            WHENEQ    10
     C                   MOVE      X             KEYC(2)
     C                   ENDSL
     C*
     C                   ENDSR                                                  FILL
     C*
     C*===============================================================
     C* FKEYS  - PROCESSING FOR F KEYS
     C*===============================================================
     C*
     C     FKEYS         BEGSR
     C*
     C                   SELECT
     C*_______________________________________________________________
     C*
     C*  F3=EXIT
     C*
     C     *INKC         WHENEQ    *ON
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C*_______________________________________________________________
     C*
     C*  F12=CANCEL
     C*
     C     *INKL         WHENEQ    *ON
     C*
     C                   SELECT
     C     Q2RECF        WHENEQ    'SCR01'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C     Q2RECF        WHENEQ    'SCR02'
     C                   MOVE      *BLANK        XXUP              1
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   ENDSL
     C*
     C                   GOTO      STRT
     C*_______________________________________________________________
     C*
     C*  F10=SEARCH
     C*
     C     *INKJ         WHENEQ    *ON
     C                   MOVEL(P)  Q2RECF        SVRECF
     C                   EXSR      KJDO
     C                   MOVEL(P)  SVRECF        Q2RECF
     C                   GOTO      STRT
     C*_______________________________________________________________
     C*
     C*  F13=CHANGE USER PARAMETERS
     C*
     C     *INKM         WHENEQ    *ON
     C*
     C                   CALL      'W0006R'                             99
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C                   GOTO      TOP
     C*_______________________________________________________________
     C*
     C*  PROCESS HELP REQUESTS
     C*
     C     *IN10         WHENEQ    *ON
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C*
     C                   CALL      'M0006C'                             99
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C*
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C*_______________________________________________________________
     C*
     C                   ENDSL
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         CABEQ     '1'           SCR02T
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         CABEQ     '1'           SCR02T
     C*
     C                   ENDSR                                                  FKEYS
     C*
     C*===============================================================
     C* KJDO   - PROCESSING FOR F10
     C*===============================================================
     C*
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*_______________________________________________________________
     C*
     C*  AGENT SEARCH
     C*
     C     HFLD          WHENEQ    'SCAGT#'
     C*
     C     SCFND         IFNE      0
     C     FNKY          CHAIN     PFN01                              60
     C                   Z-ADD     FNRRN         ##RRN
     C                   ENDIF                                                  SCFND EQ 0
     C*
     C                   CALL      'W0040R'
     C*
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM                    ##VIEW            1 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         CABEQ     *ON           STRT                     32     PC @ AGENT
     C*
     C     ##RRN         CHAIN     PAA02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  AAAGT#        SCAGT#
     C                   MOVE      *ON           *IN33                          PC @ EMP
     C                   ELSE
     C                   MOVE      *ON           *IN32                          PC @ AGENT
     C                   ENDIF                                                  *IN60       O
     C*_______________________________________________________________
     C*
     C* EMPLOYER/DIVISION SEARCH
     C*
     C     HFLD          WHENEQ    'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C*
     C     SCFND         IFNE      0
     C     FNKY          CHAIN     PFN01                              60
     C                   Z-ADD     FNRRN         ##RRN2
     C                   ENDIF                                                  SCFND EQ 0
     C*
     C                   CALL      'W0050R'
     C*
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##RRN2            9 0
     C                   PARM                    ##QTE             1
     C                   PARM                    ##VIEW            1 0
     C                   PARM                    ##RRN3            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     HFLD          CABEQ     'SCEMP#'      STRT                     33     PC @ EMP
     C     HFLD          CABEQ     'SCDIV'       STRT                     34     PC @ DIV
     C                   ENDIF                                                  *INKL       O
     C*
     C     ##RRN         CHAIN     PEM02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     EMEMP#        SCEMP#
     C                   MOVEL     EMDIV         SCDIV
     C                   MOVE      *ON           *IN59                          PC @ DATE
     C                   ELSE
     C                   MOVE      *ON           *IN33                          PC @ EMP
     C                   ENDIF                                                  *IN60       O
     C*_______________________________________________________________
     C*
     C*  INVALID CURSOR POSITION FOR SEARCH
     C*
     C                   OTHER
     C*
     C                   MOVE      *ON           *IN35                          EM WC40004
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  KJDO
     C*
     C*===============================================================
     C*  LOAD FORWARD SUBROUTINE
     C*===============================================================
     C*
     C     LOAD          BEGSR
     C*
     C                   EXSR      CLR
     C*
     C     OVER          TAG
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   ADD       1             I1
     C                   MOVE      *OFF          XXOK
     C*
     C     REFIL         TAG
     C*
     C     AMT(X)        IFNE      *ZEROS
     C                   EXSR      FILL
     C                   ENDIF                                                  AMT,X<>*ZEROS
     C*
     C                   ADD       1             X
     C*
     C     COD(X)        IFEQ      *BLANKS
     C     DES(X)        ANDEQ     *BLANKS
     C                   MOVE      *ON           XXUP
     C                   GOTO      DONE
     C                   ENDIF                                                  COD,X=*BLANKS
     C*
     C     XXOK          CABEQ     *OFF          REFIL
     C*
     C     I1            CABLT     10            OVER                   61
     C*
     C*  SCAN FORWARD IN THE ARRAYS TO MAKE SURE ROLL UP IS VALID
     C*
     C     CHKUP         TAG
     C*
     C                   ADD       1             X
     C*
     C     X             IFEQ      200
     C     COD(X)        OREQ      *BLANKS
     C     DES(X)        ANDEQ     *BLANKS
     C                   MOVE      *ON           XXUP
     C                   GOTO      DONE
     C                   ENDIF                                                  X=200
     C*
     C     AMT(X)        CABEQ     *ZERO         CHKUP
     C*
     C     DONE          TAG
     C*
     C                   ENDSR                                                   LOAD
     C*
     C*===============================================================
     C* LOADP  - LOAD BACKWARD SUBROUTINE
     C*===============================================================
     C*
     C     LOADP         BEGSR
     C*
     C                   EXSR      CLR
     C*
     C                   Z-ADD     11            I1
     C*
     C     OVERP         TAG
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   SUB       1             I1
     C                   MOVE      *OFF          XXOK
     C*
     C     REFILP        TAG
     C*
     C     AMT(X)        IFNE      *ZEROS
     C                   EXSR      FILL
     C                   ENDIF                                                  AMT,X<>*ZEROS
     C*
     C                   SUB       1             X
     C*
     C     X             IFEQ      *ZERO
     C                   Z-ADD     1             X
     C                   EXSR      LOAD
     C                   MOVE      *ON           XXDOWN
     C                   GOTO      DONEP
     C                   ENDIF                                                  X=*ZERO
     C*
     C     XXOK          CABEQ     *OFF          REFILP
     C*
     C     I1            CABGT     1             OVERP                61
     C*
     C*  SCAN BACKWARD IN THE ARRAYS TO MAKE SURE ROLL DOWN IS VALID
     C*
     C     CHKDN         TAG
     C*
     C                   SUB       1             X
     C*
     C     X             IFEQ      *ZERO
     C                   MOVE      *ON           XXDOWN
     C                   GOTO      DONEP
     C                   ENDIF                                                  X=*ZERO
     C*
     C     AMT(X)        CABEQ     *ZERO         CHKDN
     C*
     C     DONEP         TAG
     C*
     C                   ENDSR                                                  LOADP
**
  More...

