     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE  W8070RLU07 -  PRINT PROGRAM FOR FORM LUBA-WC-1007
     H*****************************************************************
     H* PROGRAM DESCRIPTION:  THIS PROGRAM IS USED TO PRINT THE       *
     H*  LUBA  SPECIFIC WC-1007 FORM.                                 *
     H*                                                               *
     H* CALLED FROM: W8070CLU07 - OVERRIDE PGM FOR FORM LUBA-WC-1007  *
     H* CALLS TO:  NONE                                               *
     H* PROJECT: 133WET         C:190          01/14/08               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  06/09/20 C:318 Project: SY420                                 *
     H*                                                               *
     H*  03/24/10  C:232  Project: 139WNN                             *
     H*                   Update programs to use the new EDI Control  *
     H*                   API WA076R instead of using WMIXP.          *
     H*                                                               *
     H*  08/20/08  C:53   SUPPORT: 64056                              *
     H*                   HARDCODE DARTS REMARK CODE FOR CLT 133      *
     H*                                                               *
     H*  05/30/08  C:191  PROJECT:133WET PRISS #20                    *
     H*                   GET SUPERVISOR NAME FROM EMPLOYER SUPERVISOR*
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*****************************************************************
     FWMLGL1    IF   E           K DISK
     FWMLGL     UF   E           K DISK
     F                                     RENAME(PLG01:PLG02)
     F*** BEGIN DLT *** 03/24/10 **************************************
     F***WMIXL   IF  E           K        DISK
     F*** END DLT ***** 03/24/10 **************************************
     FWXNIL     IF   E           K DISK
     FWXNIL1    IF   E           K DISK
     F                                     RENAME(PNI01:PNI02)
     FWMFNL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMALL     IF   E           K DISK
     FWMCXL     IF   E           K DISK
     FWMNIL     IF   E           K DISK
     F                                     RENAME(PNI01:PNI04)
     FWMCML1    IF   E           K DISK
     FWMTPL1    IF   E           K DISK
     FWMPDL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSTCTL1    IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWMRPL     IF   E           K DISK
     F*** BEG DEL ****** 04/30/08 ************************************
     F**********WMUPL1  IF  E           K        DISK
     F*** END DEL ****** 04/30/08 ************************************
     FW8070PL7  O    E             PRINTER OFLIND(*IN01)
     F                                     USROPN

     D CNB             S              1    DIM(40) ASCEND                       COMPANY NAME W/ BLAN
     D CN              S              1    DIM(40)                              COMPANY NAME
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D CPY             S             21    DIM(3) CTDATA PERRCD(1)              COPY MSG
     D OVR             S              1    DIM(33)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXZIP                  1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
     D                 DS
     D  XXACTM                 1      6  0
     D  XXTIME                 1      4  0
     D                 DS
     D  TM6                    1      6  0
     D  XXTIMM                 3      6  0
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
     D                 DS
     D  XXSS#                  1      9
     D  XXSS1                  1      3
     D  XXSS2                  4      5
     D  XXSS3                  6      9
     D                 DS
     D  CONM                   1     40
     D                                     DIM(40) ASCEND
     D  CONAME                 1     40
     D                 DS
     D  XXPOL1                 1     17
     D  CMFND                  1      3  0
     D  CMEMP#                 4     12  0
     D  CMFYR                 13     15  0
     D                 DS
     D  XXDTOB                 1      8  0
     D  XXDOBM                 1      2
     D  XXDOBD                 3      4
     D  XXDOBY                 5      8
     D                 DS
     D  XXHOSZ                 1      9
     D  XHOSZ1                 1      5
     D  XHOSZ2                 6      9
     D                 DS
     D  XXTPSZ                 1      9
     D  XTPSZ1                 1      5
     D  XTPSZ2                 6      9
     D                 DS
     D  XXHOSP                 1    116
     D  XXHOSN                 1     40
     D  XXHNM1                 1     20
     D  XXHNM2                21     40
     D  XXHOSA                41     80
     D  XXHAD1                41     60
     D  XXHAD2                61     80
     D  XXHOSC                81    105
     D  XXHOSS               106    107
     D  XXHFSN               108    122
     D*
     D XXOVR           C                   CONST('OVRPRTF QSYSPRT  OUT-
     D                                     Q(          )')
     D XXOVR1          C                   CONST('OVRPRTF QSYSPRT1 OUT-
     D                                     Q(          )')
     D XXOVR2          C                   CONST('OVRPRTF QSYSPRT2 OUT-
     D                                     Q(          )')
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  KEY LIST FOR THE NOI WORKFILE
     C*
     C     NIKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C     NIKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
      *  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     NIKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    X2ST
     C*
     C*
      *  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
      *  KEY LIST FOR THE FUND AUXILIARY FILE
     C*
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
      *  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
      *  KEY LIST FOR THE LOG HISTORY FILE
     C*
     C     LGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXHCPY
     C                   KFLD                    XXJRST
     C                   KFLD                    XXDATE
     C*
     C     LGKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    LGIDT
     C                   KFLD                    LGITM
     C*
      *  KEY LIST FOR THE DARTS REMARK CODE FILE
     C*
     C     CTKY          KLIST
     C                   KFLD                    IXHCPR
     C*
      *  KEY LIST FOR THE EDI CLAIM CONTROL FILE
     C*
     C     IXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    QQJRST
     C                   KFLD                    XXTSET
     C*
      *  TREATING PHYSICIAN
     C*
     C     TPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
      *  PROVDER MASTER
     C*
     C     PDKY          KLIST
     C                   KFLD                    CMHOSF
     C                   KFLD                    CMHOSM
     C*
     C     PDKY1         KLIST
     C                   KFLD                    TPPHYF
     C                   KFLD                    TPPHYM
     C*
      *  KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
      *  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
      *  KEY LIST FOR THE ADDITIONAL LOCATION FILE
     C*
     C     ALKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMCTG1
      *
      *  KEY LIST FOR THE DARTS FILE
     C*
     C     C3KY          KLIST
     C                   KFLD                    QCKEY
     C*
      *   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
      *   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
      *   KEY LIST FOR THE TABLE FILES
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
      *  KEY LIST FOR THE EMPLOYER CARRIER INFO FILE
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C*
     C*
     C   60
     CANN60              MOVE      *OFF          *IN60
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFROI            2
     C                   PARM                    QQJRST            2
     C                   PARM                    QQDATE            8
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
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
      *
     C*** BEG ADD ****** 03/24/10 *************************************
     C     IXAPI         PLIST
     C                   PARM                    XXCO#
     C                   PARM                    QQJRST
     C                   PARM                    XXTSET            3
     C                   PARM                    CMFND
     C                   PARM      *BLANKS       @RTNCD            1
     C                   PARM      *BLANKS       @EDIF             3
     C                   PARM      *BLANKS       IXELT             1
     C                   PARM      *ZEROS        IXELTD            3 0
     C                   PARM      *BLANKS       IXFROI            2
     C                   PARM      *ZEROS        IXSDUE            3 0
     C                   PARM      *BLANKS       IXRLS             1
     C                   PARM      *ZEROS        IXCSDT            7 0
     C                   PARM      *BLANKS       IXEADC            5
     C                   PARM      *BLANKS       IXHCPR            5
     C                   PARM      *BLANKS       IXFNRC            5
     C                   PARM      *BLANKS       IXHCED            1
     C*** END ADD ****** 03/24/10 *************************************
      *
     C*
     C     *LIKE         DEFINE    LGTSET        SVTSET
     C     *LIKE         DEFINE    NICO#         XXCO#
     C     *LIKE         DEFINE    NIDT          XXDATE
     C     *LIKE         DEFINE    LGHCPY        XXHCPY
     C*** BEG DEL ****** 03/24/10 *************************************
     C***        *LIKE     DEFN IXJRST    XXJRST
     C***        *LIKE     DEFN IXTRID    XXTSET
     C*** END DEL ****** 03/24/10 *************************************
     C     *LIKE         DEFINE    NIFND         XXFND
     C     *LIKE         DEFINE    NIFND         SVFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    F2IAUD        XXIAUD
     C     *LIKE         DEFINE    F2SCTC        XXSCTC
     C     *LIKE         DEFINE    F2AREA        XXFARA
     C     *LIKE         DEFINE    F2SPH#        XXFPH#
     C     *LIKE         DEFINE    CXRSRD        XXRMCD
     C     *LIKE         DEFINE    CMDT          XXSYDT
     C     *LIKE         DEFINE    NIST          X2ST
     C     *LIKE         DEFINE    NAKEY         XXKEY
     C     *LIKE         DEFINE    NADESC        XXNATR
     C     *LIKE         DEFINE    PADESC        XXPART
     C     *LIKE         DEFINE    CADESC        XXCAUS
     C     *LIKE         DEFINE    CMDAYS        XXHRWK
     C     *LIKE         DEFINE    CDAREA        XXCLAR
     C     *LIKE         DEFINE    CDPHN#        XXCLPH
     C     *LIKE         DEFINE    CDADR1        XXCLA1
     C     *LIKE         DEFINE    CDADR2        XXCLA2
     C     *LIKE         DEFINE    CDCTY         XXCLCY
     C     *LIKE         DEFINE    CDST          XXCLST
     C     *LIKE         DEFINE    CDZIP         XXCLZP
     C                   CALL      'RTVCLTID'
     C                   PARM                    @@CLT#
     C                   MOVE      *OFF          XXOPN2
     C*
     C                   MOVE      *BLANKS       XXCPY            21
     C                   Z-ADD     0             X                 5 0
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C                   MOVEL     QQFROI        XXHCPY
     C*** BEG CHG ****** 03/24/10 *************************************
     C***                  MOVE QQJRST    XXJRST
     C                   MOVE      QQJRST        XXJRST            2
     C*** END CHG ****** 03/24/10 *************************************
     C                   MOVE      @@FND         XXFND
     C                   MOVE      @@FYR         XXFYR
     C                   MOVE      @@CASE        XXCASE
     C     @@CASE        COMP      *BLANKS                            7474
     C*
      *  CALL SYSTEM API TO GET VARIOUS SYSTEM VALUES.
     C*
     C                   CALL      'SA001R'
     C                   PARM                    @@WCIF            1
     C                   PARM                    @@MCIF            1
     C                   PARM                    @@WCIN            1
     C                   PARM                    @@AR              1
     C                   PARM                    @@NTWK            1
     C                   PARM                    @@UR              1
     C                   PARM                    @@MED             1
     C                   PARM                    @@EDI             1
     C                   PARM                    @@WCDK            1
     C                   PARM                    @@RHAB            1
     C                   PARM                    @@MSS             1
     C                   PARM                    @@FSCH            1
     C                   PARM                    @@PRVB            1
     C                   PARM                    @@NPWC            1
     C                   PARM                    @@OS              1
     C                   PARM                    @@ARLS            1
     C                   PARM                    @@CCOD            1
     C                   PARM                    @@CRVW            1
     C                   PARM                    @@PROM            1
     C                   PARM                    @@MCRV            1
     C                   PARM                    @@FREQ            1
     C                   PARM                    @@CRAC            1
     C                   PARM                    @@PREO            1
     C                   PARM                    @@PARN            1
     C                   PARM                    @@MGRI            1
     C                   PARM                    @@KCIF            1
     C                   PARM                    @@CURS            1
     C                   PARM                    @@PCIB            1
     C                   PARM                    @@CLT#            3
     C                   PARM                    @@UCPF            1
     C                   PARM                    @@DCF             1
     C                   PARM                    @@AUTB            1
     C*
      *   RETRIEVE COMPANY MASTER INFO
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
      *   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      *BLANKS       XXOUTQ           10
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C     OROUTQ        IFEQ      *BLANK
     C                   MOVEL     'QPRINT'      XXOUTQ           10
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       OROUTQ
     C                   MOVE      '1'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C                   MOVEL     OROUTQ        X1OUTQ           10
     C     X1OUTQ        IFEQ      *BLANK
     C                   MOVEL     'QPRINT'      X1OUTQ
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       OROUTQ
     C                   MOVE      '2'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C                   MOVEL     OROUTQ        X2OUTQ           10
     C     X2OUTQ        IFEQ      *BLANK
     C                   MOVEL     'QPRINT'      X2OUTQ
     C                   ENDIF
     C*
      * IF CALLED FROM WW CLAIMS OPT31, READ WMCMP AND PRINT ONE COPY.
     C*
     C     *IN74         IFEQ      *ON
     C                   OPEN      W8070PL7
     C                   MOVE      *ON           XXOPN2
     C                   Z-ADD     3             XXCOPY
     C                   ELSE
     C*
      *  IF THIS PGM WAS CALLED FROM THE DAILY BATCH, WXNIP WILL BE READ
      *  3 TIMES TO PRODUCE 3 COPIES.  IF CALLED FROM THE EXTRACT
      *  OPTION, 1 READ WILL OCCUR.
     C*
     C     QQFROI        IFNE      '  '
     C                   Z-ADD     3             XXCOPY            1 0
     C                   ELSE
     C                   Z-ADD     1             XXCOPY
     C                   END
     C*
     C     NEXT          TAG
     C                   Z-ADD     0             SVFND
     C                   Z-ADD     XXCOPY        X
     C                   MOVE      CPY(X)        XXCPY
     C     X             COMP      1                                      71
     C     X             COMP      2                                      72
     C     X             COMP      3                                      73
     C*
      *   OPEN THE PRINT FILE
     C*
     C     *IN73         IFEQ      *OFF
     C     XXCOPY        IFEQ      1
     C                   OPEN      W8070PL7
     C                   MOVE      *ON           XXOPEN
     C                   END
     C     XXCOPY        IFEQ      2
     C                   OPEN      W8070PL7
     C                   MOVE      *ON           XXOPN1
     C                   OPEN      W8070PL7
     C                   MOVE      *ON           XXOPN2
     C                   END
     C                   END
     C*
     C                   ENDIF
     C*
     C   71NIKY          CHAIN     PNI01                              50
     C   72NIKY          CHAIN     PNI02                              50
     C   73LGKY          CHAIN     PLG01                              50
     C   74CMKY          CHAIN     PCM01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C     *IN71         IFEQ      '1'
     C     NIIDT         ANDNE     XXDATE
     C     *IN72         OREQ      '1'
     C     NIIDT         ANDNE     XXDATE
     C                   GOTO      SKIP
     C                   END
     C     *IN73         IFEQ      *ON
     C     LGPRSQ        IFNE      0
     C     LGSCHP        ORNE      LGEXDT
     C                   GOTO      SKIP
     C                   ENDIF
     C                   ENDIF
     C*
      *   OPEN THE PRINT FILE
     C*
     C     *IN73         IFEQ      *ON
     C     XXOPN2        ANDEQ     *OFF
     C                   OPEN      W8070PL7
     C                   MOVE      *ON           XXOPN2
     C                   END
     C*
     C                   EXSR      SRINIT
     C*
      *  IF THE FUND CHANGED, GET THE NEW FUND INFORMATION.
     C*
     C     XXFND         IFNE      SVFND
     C                   EXSR      FNDBRK
     C                   Z-ADD     XXFND         SVFND
     C                   END
     C*
      *  INDICATE REPORT DATE
     C*
     C                   MOVE      DSDATE        XXRPDT            8 0
     C*
      *  LOAD CLAIMANT INFORMATION
     C*
     C     CMKY          CHAIN     PCM01                              60
     C   60              GOTO      SKIP
     C     CDKY          CHAIN     PCD01                              60
     C   60              GOTO      SKIP
     C                   MOVE      CMJRST        X2ST
     C     NIKY2         CHAIN     PNI04                              60
     C   60              MOVE      *BLANKS       NI1A3
     C   60              MOVE      *BLANKS       NI10A1
     C   60              MOVE      *ZEROS        NIDT4
     C   60              MOVE      *BLANKS       NI1A5
     C   60              MOVE      *BLANKS       NI80A1
     C*
     C*** BEG ADD ****** 08/05/08 ************************************
     C                   Z-ADD     NIPREP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C  N60              MOVEL     SRDTO         XXRPDT
     C*** END DLT ****** 08/05/08 ************************************
     C*
      *  LOAD CLAIMANT SOCIAL SECURITY NUMBER
     C*
     C     CMSS#         IFNE      *BLANKS
     C                   MOVE      CMSS#         XXSS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'XXSS#'       QQNAME           25
     C     XXSS#         PARM      XXSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C                   MOVEL     XXSS1         XXCSS#           11
     C     XXCSS#        CAT       '-':0         XXCSS#
     C     XXCSS#        CAT       XXSS2:0       XXCSS#
     C     XXCSS#        CAT       '-':0         XXCSS#
     C                   MOVE      XXSS3         XXCSS#
     C                   ELSE
     C                   MOVE      *BLANKS       XXSS#
     C                   MOVE      *BLANKS       XXCSS#
     C                   ENDIF
     C*
      *  TIME EMPLOYEE BEGAN WORK
     C*
     C                   MOVE      NITIM2        TM6               6 0
     C                   MOVEL     TM6           XXTIM1            4 0
     C                   MOVEL     XXTIM1        TEST1             2 0
     C     TEST1         IFGE      120000
     C     TEST1         ANDNE     240000
     C                   MOVE      '1'           *IN28
     C                   ELSE
     C                   MOVE      '0'           *IN28
     C                   END
     C     *IN28         IFEQ      *ON
     C                   MOVE      'X'           PRNRPM
     C                   ELSE
     C                   MOVE      'X'           PRNRAM
     C                   ENDIF
     C*
      *  LOAD PURPOSE OF FORM
     C*
     C     '1'           SCAN      NI20A4:1      @@NI20            1 0    75
     C     *IN75         IFEQ      *ON
     C                   MOVE      'X'           X120A4            1
     C                   ENDIF
     C*
     C     '2'           SCAN      NI20A4:1      @@NI20            1 0    76
     C     *IN76         IFEQ      *ON
     C                   MOVE      'X'           X220A4            1
     C                   ENDIF
     C*
     C     '3'           SCAN      NI20A4:1      @@NI20            1 0    77
     C     *IN77         IFEQ      *ON
     C                   MOVE      'X'           X320A4            1
     C                   ENDIF
     C*
     C     '4'           SCAN      NI20A4:1      @@NI20            1 0    78
     C     *IN78         IFEQ      *ON
     C                   MOVE      'X'           X420A4            1
     C                   ENDIF
     C*
     C     '5'           SCAN      NI20A4:1      @@NI20            1 0    79
     C     *IN79         IFEQ      *ON
     C                   MOVE      'X'           X520A4            1
     C                   ENDIF
     C*
     C     '6'           SCAN      NI20A4:1      @@NI20            1 0    80
     C     *IN80         IFEQ      *ON
     C                   MOVE      'X'           X620A4            1
     C                   ENDIF
     C*
     C     '7'           SCAN      NI20A4:1      @@NI20            1 0    81
     C     *IN81         IFEQ      *ON
     C                   MOVE      'X'           X720A4            1
     C                   ENDIF
      *  LOAD CLAIMANT SEX
     C*
     C     CDSEX         IFEQ      'M'
     C                   MOVE      'X'           XXSEXM            1
     C                   MOVE      *BLANK        XXSEXF            1
     C                   ELSE
     C     CDSEX         IFEQ      'F'
     C                   MOVE      'X'           XXSEXF
     C                   MOVE      *BLANK        XXSEXM
     C                   ELSE
     C                   MOVE      *BLANK        XXSEXM
     C                   MOVE      *BLANK        XXSEXF
     C                   ENDIF
     C                   ENDIF
     C*
      *  LOAD CLAIMANT PHONE NUMBER
     C*
     C     CDAREA        IFNE      *ZERO
     C                   MOVE      CDAREA        XXCLAR
     C                   ELSE
     C                   MOVE      *ZERO         XXCLAR
     C                   ENDIF
     C     CDPHN#        IFNE      *ZERO
     C                   MOVE      CDPHN#        XXCLPH
     C                   ELSE
     C                   MOVE      *ZERO         XXCLAR
     C                   MOVE      *ZERO         XXCLPH
     C                   ENDIF
     C*
      *  LOAD THE CLAIMANT ADDRESS
     C*
     C     CDADR1        IFNE      *BLANKS
     C                   MOVE      CDADR1        XXCLA1
     C                   MOVE      CDADR2        XXCLA2
     C                   MOVE      CDCTY         XXCLCY
     C                   MOVE      CDST          XXCLST
     C                   MOVE      CDZIP         XXCLZP
     C                   ELSE
     C                   MOVE      *BLANKS       XXCLA1
     C                   MOVE      *BLANKS       XXCLA2
     C                   MOVE      *BLANKS       XXCLCY
     C                   MOVE      *BLANKS       XXCLST
     C                   MOVE      *ZEROS        XXCLZP
     C                   ENDIF
     C*
      *  DETRRMINE IF INJURY OCCURRED ON EMPLOYER'S PREMISED
     C*
     C     CMEADR        IFEQ      'Y'
     C                   MOVE      'X'           XEADRY            1
     C                   MOVE      *BLANKS       XEADRN            1
     C                   MOVE      *BLANKS       CMACNM
     C                   ELSE
     C     CMEADR        IFEQ      'N'
     C                   MOVE      *BLANKS       XEADRY
     C                   MOVE      'X'           XEADRN
     C                   ELSE
     C                   MOVE      *BLANKS       XEADRY
     C                   MOVE      *BLANKS       XEADRN
     C                   ENDIF
     C                   ENDIF
     C*
      *  LOAD ANSWER TO DID EMPLOYEE WORK AT SAME WAGES
     C*
     C     NI1A2         IFEQ      'Y'
     C                   MOVE      'X'           XXWNDY            1
     C                   MOVE      XXWNDY        PRATWG
     C*****                MOVE NI20A3    PRATA3
     C                   MOVE      *BLANK        XXWNDN            1
     C                   ELSE
     C     NI1A2         IFEQ      'N'
     C                   MOVE      'X'           XXWNDN
     C                   MOVE      XXWNDY        PRATSG
     C                   MOVE      *BLANK        XXWNDY
     C                   ELSE
     C                   MOVE      *BLANK        XXWNDN
     C                   MOVE      *BLANK        XXWNDY
     C                   ENDIF
     C                   ENDIF
     C*
      *  INDICATE LOCATION
     C*
     C                   MOVE      CMACZ1        XXZIPA            5
     C                   MOVE      CMACZ2        XXZIPB            4
     C*
     C     CMEADR        IFEQ      'N'
     C     NI40A3        IFNE      *BLANKS
     C     CMACDR        CAT(P)    NI40A3:2      XXAADR           80
     C                   CAT       CMACTY:2      XXAADR
     C                   ELSE
     C     CMACDR        CAT       CMACTY:2      XXAADR
     C                   ENDIF
     C                   CAT       ',':0         XXAADR
     C****                 CAT  CMACST:1  XXAADR
     C                   CAT       CMAST:1       XXAADR
     C                   CAT       XXZIPA:2      XXAADR
     C                   CAT       '-':0         XXAADR
     C                   CAT       XXZIPB:0      XXAADR
     C                   ELSE
     C                   CLEAR                   XXAADR
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDLSN         XXLAST
     C                   MOVEL     CDMID         XXFRST
     C                   EXSR      SRFLNM
     C                   MOVEL     XXNAME        XXLAST
     C                   MOVEL     CDFSN         XXFRST
     C                   EXSR      SRFLNM
     C                   MOVEL     XXNAME        XXCNAM
     C                   MOVEL     CDCTY         XXCCTY
     C                   MOVEL     CDZIP         XXCZIP
     C                   MOVEL     CMACTY        XXACTY
     C*
     C                   MOVEL     *BLANKS       XXPNAM
     C                   MOVEL     *BLANKS       XXPAD1
     C                   MOVEL     *BLANKS       XXPCSZ
     C                   MOVE      *ZEROS        XXZIPZ
     C                   MOVE      *BLANKS       XXAREA
     C                   MOVEL     *BLANKS       XXPPH#
     C*
      *  GET PHYSICIAN OR HOSPITAL INFORMATION
     C*
     C     TPKY          CHAIN     PTP01                              60
     C     *IN60         IFEQ      *OFF
     C     TPPHYN        IFNE      *BLANKS
     C                   MOVEL     TPPHYN        XXPNAM
     C                   MOVEL     TPPHYA        XXPAD1
     C                   MOVEL(P)  TPPHYC        XXPCSZ
     C     XXPCSZ        CAT       ',':0         XXPCSZ
     C     XXPCSZ        CAT       TPPHYS:1      XXPCSZ
     C                   MOVE      TPPHYZ        XXZIPZ            9 0
     C                   MOVE      TPPHYR        XXAREA
     C                   MOVEL     TPPHYP        XXPPH#
     C                   MOVE      TPPHYP        XXPPH#
     C                   ENDIF
     C*
     C     TPPHYF        IFNE      *ZEROS
     C*
     C     PDKY1         CHAIN     PPD01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     PDFSN         IFNE      *BLANKS
     C     PDFSN         CAT       PDNAME:1      XXPNAM
     C                   ELSE
     C                   MOVEL     PDNAME        XXPNAM
     C                   ENDIF
     C*
     C                   MOVEL(P)  PDADR1        XXPAD1
     C                   MOVEL(P)  PDCTY         XXPCSZ
     C     XXPCSZ        CAT       ',':0         XXPCSZ
     C     XXPCSZ        CAT       PDST:1        XXPCSZ
     C                   Z-ADD     PDZIP         XXZIPZ
     C                   Z-ADD     PDAREA        XXAREA
     C                   Z-ADD     PDPHN#        XXPPH#
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     PDFSN         IFNE      *BLANKS
     C     PDFSN         CAT       PDNAME:1      XXPNAM
     C                   ELSE
     C                   MOVEL     PDNAME        XXPNAM
     C                   ENDIF
     C*
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR2        XXPAD1
     C                   ELSE
     C                   MOVEL(P)  PDADR1        XXPAD1
     C                   ENDIF
     C                   MOVEL(P)  PDCTY         XXPCSZ
     C     XXPCSZ        CAT       ',':0         XXPCSZ
     C     XXPCSZ        CAT       PDST:1        XXPCSZ
     C                   Z-ADD     PDZIP         XXZIPZ
     C                   Z-ADD     PDAREA        XXAREA
     C                   Z-ADD     PDPHN#        XXPPH#
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       XXNATR
     C                   MOVE      *BLANKS       XXPART
     C                   MOVE      *BLANKS       XXCAUS
     C*
      *  Get Name and Address of hospital
     C*
     C                   MOVE      *BLANKS       XXHOSP
     C     CMHOSF        IFGT      *ZEROS
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     PDFSN         XXHFSN
     C                   MOVE      PDNAME        XXHOSN
     C                   MOVE      PDADR1        XXHOSA
     C                   MOVE      PDCTY         XXHOSC
     C                   MOVE      PDST          XXHOSS
     C                   MOVE      PDZIP         XXHOSZ
     C                   MOVEL(P)  PDCTY         XXHCSZ
     C     XXHCSZ        CAT       ',':0         XXHCSZ
     C     XXHCSZ        CAT       PDST:1        XXHCSZ
      *
     C                   MOVE      *BLANKS       XHZIP            10
     C     XHOSZ1        IFNE      *ZERO
     C     XHOSZ2        ANDNE     *ZERO
     C     XHOSZ1        CAT       '-':0         XHZIP
     C     XHZIP         CAT       XHOSZ2:0      XHZIP
     C                   ELSE
     C     XHOSZ1        IFGT      *ZERO
     C                   MOVEL(P)  XHOSZ1        XHZIP
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *BLANK        XXHFSN
     C                   MOVEL     CMHOSN        XXHOSN
     C                   MOVEL     CMHOSA        XXHOSA
     C                   MOVEL     CMHOSC        XXHOSC
     C                   MOVEL     CMHOSS        XXHOSS
      *
     C                   MOVE      CMHOSZ        XXHOSZ
     C                   MOVE      *BLANKS       XHZIP            10
     C     XHOSZ1        IFNE      *ZERO
     C     XHOSZ2        ANDNE     *ZERO
     C     XHOSZ1        CAT       '-':0         XHZIP
     C     XHZIP         CAT       XHOSZ2:0      XHZIP
     C                   ELSE
     C     XHOSZ1        IFGT      *ZERO
     C                   MOVEL(P)  XHOSZ1        XHZIP
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C*
     C                   MOVEL(P)  CMNATR        XXKEY
     C     TBKY          CHAIN     PNA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  NADESC        XXNADS           30
     C                   ELSE
     C                   MOVE(P)   *BLANK        XXNADS
     C                   ENDIF
     C*
      *  RETRIEVE THE PART DESCRIPTION.
     C*
     C                   MOVEL(P)  CMPART        XXKEY
     C     TBKY          CHAIN     PPA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  PADESC        XXBPRT           30
     C                   ELSE
     C                   MOVE(P)   *BLANK        XXBPRT
     C                   ENDIF
     C*
      *   RETRIEVE THE CAUSE DESCRIPTION.
     C*
     C                   MOVEL(P)  CMCAUS        XXKEY
     C     TBKY          CHAIN     PCA01                              60
     C  N60              MOVEL     CADESC        XXCAUS
     C*
      *  WAGE INFORMATION
     C*
     C     CMWGPD        IFNE      *BLANKS
     C     CMWGPD        IFEQ      '6'                                          WAGE PERIOD
     C                   MOVE      'X'           XXWGP1
     C                   ELSE
     C     CMWGPD        IFEQ      '1'
     C                   MOVE      'X'           XXWGP2
     C                   ELSE
     C     CMWGPD        IFEQ      '4'
     C                   MOVE      'X'           XXWGP3
     C                   ELSE
     C                   MOVE      'X'           XXWGP4
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C*  PRINT 'MC' ON LABEL FOR MANAGED CARE CLAIMS
     C*
     C                   MOVE      *BLANKS       XXMC              2
     C     CMMCO         IFEQ      'Y'
     C                   MOVEL     'MC'          XXMC
     C                   END
     C*
     C*
     C*  LOAD THE CITY, STATE AND ZIP FOR THE LABEL
     C*
     C                   MOVE      CDCTY         XXCTY
     C                   MOVE      CDST          XXSTAT
     C                   Z-ADD     CDZIP         XXZIP
     C                   EXSR      SRADDR
     C                   MOVE      *BLANKS       XXCCSZ           40
     C                   MOVEL     XXCSZ         XXCCSZ
     C     CDADR2        COMP      *BLANKS                                41
     C*
     C*  CONVERT ALL DATES WHICH ARE PRINTED.
     C*
     C                   Z-ADD     CMRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXRCVD
     C*
     C                   Z-ADD     CMRTW         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXIRTN
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXACDT
     C*
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXCDOB            8 0
     C*
     C                   Z-ADD     CMDTRP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXDTRP
     C*
     C                   Z-ADD     CMHIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXDOH
     C*
     C                   Z-ADD     CMDTLW        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXDTLW
     C*
     C                   Z-ADD     CMFSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXFSDT
     C*
     C                   Z-ADD     CMDOD         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXDOD
     C*
     C                   Z-ADD     NIDT3         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXIDT3
     C*
     C*  CONVERT THE ACCIDENT TIME
     C*
     C     CMACTM        IFGE      130000
     C     CMACTM        SUB       120000        XXACTM
     C                   ELSE
     C     CMACTM        IFLT      10000
     C                   MOVE      CMACTM        XXWK6A            6
     C                   MOVEL     '12'          XXWK6A
     C                   MOVE      XXWK6A        XXACTM
     C                   ELSE
     C                   Z-ADD     CMACTM        XXACTM
     C                   END
     C                   END
     C     CMACTM        IFGE      120000
     C     CMACTM        ANDNE     240000
     C                   MOVE      '1'           *IN20
     C                   ELSE
     C                   MOVE      '0'           *IN20
     C                   END
     C     *IN20         IFEQ      *ON
     C                   MOVE      'X'           PRTIPM
     C                   ELSE
     C                   MOVE      'X'           PRTIAM
     C                   ENDIF
     C*
     C*  GET THE EMPLOYER INFORMATION
     C*
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     EMNAME        XXENAM
     C                   MOVEL     EMFDNM        XXFDNM
     C                   MOVEL     EMNAOB        XXNAOB
     C*
     C     EMPAD1        IFEQ      *BLANKS
     C                   MOVE      EMMAD1        EMPAD1
     C                   MOVE      EMMAD2        EMPAD2
     C                   MOVE      EMMCTY        EMPCTY
     C                   MOVE      EMMST         EMPST
     C                   Z-ADD     EMMZIP        EMPZIP
     C                   END
     C*
     C                   MOVEL     EMPCTY        XXECTY
     C                   MOVEL     EMPZIP        XXEZIP
     C                   MOVEL     EMPCTY        XXCTY
     C                   MOVE      EMPST         XXSTAT
     C                   Z-ADD     EMPZIP        XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         XXECSZ
     C     EMPAD2        COMP      *BLANKS                                19
     C                   END
     C*
     C*  GET THE LOCATION INFORMATION, IF ANY.
     C*
     C     ALKY          CHAIN     PAL01                              60
     C     *IN60         IFEQ      '0'
     C     ALPAD1        IFNE      *BLANKS
     C                   MOVEL     ALPAD1        XXLAD1
     C                   MOVEL     ALPAD2        XXLAD2
     C                   MOVEL     ALPCTY        XXLCTY
     C                   MOVEL     ALPST         XXLST
     C                   MOVEL     ALPZIP        XXLZIP
     C                   ELSE
     C                   MOVEL     ALMAD1        XXLAD1
     C                   MOVEL     ALMAD2        XXLAD2
     C                   MOVEL     ALMCTY        XXLCTY
     C                   MOVEL     ALMST         XXLST
     C                   MOVEL     ALMZIP        XXLZIP
     C                   END
     C                   END
     C*
     C*  Use the policy # from the Carrier Info file, if it exists.
     C*
     C     RPKY          CHAIN     PRP01                              60
     C   60              MOVE      *BLANKS       RPCAP#
     C     RPCAP#        IFNE      *BLANKS
     C                   MOVEL(P)  RPCAP#        XXPOL#           18
     C                   ELSE
     C                   MOVEL(P)  XXPOL1        XXPOL#
     C                   ENDIF
     C*
     C*  SET INDICATORS FOR ALL BOXES THAT GET MARKED.
     C*
     C     CDSEX         COMP      'F'                                    21
     C     CDSEX         COMP      'M'                                    43
     C     CMPOI         COMP      'N'                                    22
     C     CMPOI         COMP      'Y'                                    44
     C     CMFSAL        COMP      'Y'                                    23
     C     CMRTW         COMP      *ZEROS                                 24
     C     CMEAGR        COMP      'N'                                    29
     C     CMDNY         COMP      'Y'                                    31
     C     NI1A3         COMP      'Y'                                    32
     C     CMFSAL        COMP      'Y'                                    33
     C                   MOVEL     NI10A1        XXCD1
     C     XXCD1         COMP      'TT'                                   34
     C     XXCD1         COMP      '80'                                   35
     C     XXCD1         COMP      'TP'                                   36
     C     XXCD1         COMP      'IB'                                   37
     C     XXCD1         COMP      'PT'                                   38
     C     XXCD1         COMP      'DT'                                   39
     C     NI1A5         COMP      'Y'                                    42
     C                   SETOFF                                           25
     C                   SETOFF                                           26
     C                   SETOFF                                           27
     C                   SETOFF                                           28
     C                   SETOFF                                           30
     C                   SETOFF                                           45
     C     NI1A6         IFEQ      'Y'
     C                   SETOFF                                           31
     C                   SETON                                            46
     C                   ENDIF
     C     XXCD1         COMP      'SO'                                   47
     C*
     C                   Z-ADD     NIDT1         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXDT1A
     C*
     C                   Z-ADD     NIDT2         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXDT2A
     C*
     C                   Z-ADD     CMDBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXDT1
     C*
     C                   Z-ADD     CMFSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXDT2
     C*
     C                   Z-ADD     NIDT4         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXDT4
     C*
     C                   Z-ADD     CMAWW         XX7N1
     C                   Z-ADD     CMCRT         XX7N2
     C     NI40A1        CAT       NI40A2:0      XX80A1
     C*
     C*
     C*  Calculate the # of hours per week.
     C*
     C     CMHRSD        MULT      CMDAYS        XXHRWK
     C*
     C*  Retrieve the name of the supervisor.
     C*
     C*** BEG CHG ****** 05/30/08 ************************************
     C***********CMSUPR    CHAINPUP01                60
     C**N60******          MOVELUPUSNM    PRPRNM
     C                   MOVEL(P)  CMESPV        PRPRNM
     C*** END CHG ****** 05/30/08 ************************************
     C*
     C*
     C*  CLOSE THE LABEL SPOOL FILE IF THE EMPLOYEE COPY WAS PROCESSED.
     C*
     C     *IN73         IFEQ      *ON
     C     XXOPN2        ANDEQ     *ON
     C                   CLOSE     W8070PL7
     C                   MOVE      *OFF          XXOPN2            1
     C                   ENDIF
     C*
     C*  WRITE THE DARTS REMARK, THEN PRINT THE NOI FORM.
     C*
     C  N73              EXSR      DARTS
     C   73              EXSR      SRDART
     C*
     C     *IN74         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C*
     C*  GET THE NEXT RECORD
     C*
     C     SKIP          TAG
     C*
     C     *IN74         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C   71NIKY          READE     PNI01                                  50
     C   72NIKY          READE     PNI02                                  50
     C   73LGKY          READE     PLG01                                  50
     C                   END
     C*
     C                   EXSR      @@MOVE
     C                   WRITE     LUBAREC
     C*
     C*
     C*  CLOSE THE LABEL SPOOL FILE IF THE EMPLOYEE COPY WAS PROCESSED.
     C*
     C   71XXOPEN        IFEQ      *ON
     C                   CLOSE     W8070PL7
     C                   MOVE      *OFF          XXOPEN            1
     C                   ENDIF
     C   72XXOPN1        IFEQ      *ON
     C                   CLOSE     W8070PL7
     C                   MOVE      *OFF          XXOPN1            1
     C                   ENDIF
     C   72XXOPN2        IFEQ      *ON
     C                   CLOSE     W8070PL7
     C                   MOVE      *OFF          XXOPN2            1
     C                   ENDIF
     C*
     C*  PROCESS THE NEXT COPY
     C*
     C     XXCOPY        IFEQ      1
     C                   ADD       1             XXCOPY
     C                   GOTO      NEXT
     C                   END
     C*
     C*  END OF PROGRAM
     C*
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     C* SRINIT SUBROUTINE              ** Subroutine **   S R I N I T *
     C* THIS SUBROUTINE INITIALIZES ALL WORK FIELDS                   *
     C*****************************************************************
     C     SRINIT        BEGSR
     C*
     C                   MOVE      *BLANKS       XXCNAM           40
     C                   MOVE      *BLANKS       XXCCTY           15
     C                   MOVE      *BLANKS       XXCZIP            5
     C                   MOVE      *BLANKS       XXENAM           40
     C                   MOVE      *BLANKS       XXFDNM           30
     C                   MOVE      *BLANKS       XXECTY           15
     C                   MOVE      *BLANKS       XXEZIP            5
     C                   MOVE      *BLANKS       XXLCTY           15
     C                   MOVE      *BLANKS       XXLZIP            5
     C                   MOVE      *BLANKS       XXACTY           15
     C                   MOVE      *BLANKS       XXNAOB           40
     C                   MOVE      *BLANKS       XXLAD1           40
     C                   MOVE      *BLANKS       XXLAD2           40
     C                   MOVE      *BLANKS       XXLST             2
     C                   MOVE      *BLANKS       XXPNAM           35
     C                   MOVE      *BLANKS       XXPAD1           35
     C                   MOVE      *BLANKS       XXPCSZ           25
     C                   MOVE      *BLANKS       XXAREA            3 0
     C                   MOVE      *BLANKS       XXPPH#            7 0
     C                   MOVE      *BLANKS       XXECSZ           40
     C                   Z-ADD     0             XXRCVD            8 0
     C                   Z-ADD     0             XXACDT            8 0
     C                   Z-ADD     0             XXIDT3            8 0
     C                   Z-ADD     0             XXACTM
     C                   Z-ADD     0             XXDTOB            8 0
     C                   Z-ADD     0             XXDTRP            8 0
     C                   Z-ADD     0             XXDOH             8 0
     C                   Z-ADD     0             XXDTLW            8 0
     C                   Z-ADD     0             XXFSDT            8 0
     C                   Z-ADD     0             XXIRTN            8 0
     C                   Z-ADD     0             XXDOD             8 0
     C                   Z-ADD     0             XXDT1             8 0
     C                   Z-ADD     0             XXDT2             8 0
     C                   Z-ADD     0             XXDT4             8 0
     C                   Z-ADD     0             XX7N1             7 2
     C                   Z-ADD     0             XX7N2             7 2
     C                   Z-ADD     0             XXDT1A            8 0
     C                   Z-ADD     0             XXDT2A            8 0
     C                   MOVE      *BLANKS       XX40A1           40
     C                   MOVE      *BLANKS       XXCD1             2
     C                   MOVE      *BLANKS       XX80A1           80
     C                   MOVE      *BLANKS       XXHCSZ           25
     C                   MOVE      *BLANK        XXWGP1            1
     C                   MOVE      *BLANK        XXWGP2            1
     C                   MOVE      *BLANK        XXWGP3            1
     C                   MOVE      *BLANK        XXWGP4            1
     C*
     C     *IN74         IFEQ      *OFF
     C     *IN73         IFEQ      *OFF
     C                   Z-ADD     NIFND         XXFND
     C                   Z-ADD     NIFYR         XXFYR
     C                   Z-ADD     NICASE        XXCASE
     C                   ELSE
     C                   Z-ADD     LGFND         XXFND
     C                   Z-ADD     LGFYR         XXFYR
     C                   Z-ADD     LGCASE        XXCASE
     C                   END
     C                   ENDIF
     C*
     C                   ENDSR
     C*****************************************************************
     C* FNDBRK SUBROUTINE              ** Subroutine **   F N D B R K *
     C* THIS SUBROUTINE GETS THE FUND INFORMATION.                    *
     C*****************************************************************
     C     FNDBRK        BEGSR
     C*
     C*  GET THE CLAIMS CONTROL FILE RECORD
     C*
     C                   MOVE      *BLANKS       XXRMCD
     C     FNKY          CHAIN     PCX01                              60
     C*
     C*   GET REMARK CODE FOR DIVISION DIARY REMARKS.
     C*
     C                   MOVE      *BLANKS       XXRMCD
     C  N60              MOVE      CXRSRD        XXRMCD
     C*
     C*** BEG ADD *** 08/20/08 ****************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    @@CLT#
     C     @@CLT#        IFEQ      '133'
     C                   MOVEL(P)  'CFROI'       XXRMCD
     C                   ENDIF
     C*** END ADD *** 08/20/08 ****************************
     C*
     C*  GET THE FUND NAME AND ADDRESS
     C*
     C                   MOVE      *BLANKS       XXCONM           40
     C                   MOVE      *BLANKS       XCADR1           40
     C                   MOVE      *BLANKS       XCCSZ            40
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVEL     FNNAME        XXFNAM           40
     C     @@CLT#        IFEQ      '044'
     C                   MOVEL     FNANAM        XXFNAM
     C                   ENDIF
     C*
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEA     FNANAM        CNB
     C                   Z-ADD     1             X
     C     *BLANK        LOOKUP    CNB(X)                             60
     C     *IN60         IFEQ      '1'
     C                   MOVEA     CNB(X)        CN
     C                   ELSE
     C                   MOVEA     FNANAM        CN
     C                   END
     C                   MOVEA     CN            XXCONM
     C*
     C                   MOVEL     FNAAD1        XCADR1
     C                   MOVE      FNAST         XXSTAT            2
     C                   Z-ADD     FNAZIP        XXZIP             9 0
     C                   MOVEL     FNACTY        XXCTY            25
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         XCCSZ
     C*
     C                   ELSE
     C*
     C*   LOAD THE CARRIER NAME AND ADDRESS FROM THE COMPANY FILE
     C*
     C                   Z-ADD     1             X
     C     *BLANK        LOOKUP    CONM(X)                            60
     C                   MOVEA     CONM(X)       XXCONM
     C*
     C                   MOVEL     COADR1        XCADR1
     C                   MOVEL     COCTY         XXCTY
     C                   MOVEL     COST          XXSTAT
     C                   Z-ADD     COZIP         XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         XCCSZ
     C*
     C                   ENDIF
     C*
     C     FNSFIN        COMP      'Y'                                    40
     C                   END
     C*
     C*  GET INTERNAL AUDIT NUMBER
     C*
     C                   Z-ADD     0             XXIAUD
     C                   Z-ADD     0             XXSCTC
     C                   Z-ADD     0             XXFARA
     C                   Z-ADD     0             XXFPH#
     C     F2KY          CHAIN     PF201                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     F2IAUD        XXIAUD
     C     @@CLT#        IFEQ      '051'
     C                   MOVEL(P)  EMSEMP        XXIAUD
     C                   ENDIF
     C                   Z-ADD     F2SCTC        XXSCTC
     C                   Z-ADD     F2AREA        XXFARA
     C                   Z-ADD     F2SPH#        XXFPH#
     C                   END
     C*
     C                   ENDSR
     C*****************************************************************
     C* SRADDR SUBROUTINE              ** Subroutine **   S R A D D R *
     C* THIS SUBROUTINE PUTS TOGETHER AN ADDRESS INTO ONE STRING      *
     C* CONTAINING THE CITY, STATE AND ZIP                            *
     C*****************************************************************
     C     SRADDR        BEGSR
     C*
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     XXSTAT        XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVEL     XXCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ            40
     C*
     C                   ENDSR
     C*****************************************************************
     C* SRFLNM SUBROUTINE              ** Subroutine **   S R F L N M *
     C* THIS SUBROUTINE PUTS TOGETHER A NAME, THE FIRST NAME FOLLOWED *
     C* BY THE LAST NAME (JOHN SMITH)                                 *
     C*****************************************************************
     C     SRFLNM        BEGSR
     C*
     C                   MOVE      *BLANKS       XXNAME
     C                   MOVE      '1'           XXTERM
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXNAME           40
     C*
     C                   ENDSR
     C*****************************************************************
     C* DARTS SUBROUTINE               ** Subroutine **   D A R T S   *
     C* THIS SUBROUTINE WRITES A DARTS REMARK FOR EACH NOI FORM       *
     C* PRINTED.                                                      *
     C*****************************************************************
     C     DARTS         BEGSR
      *
      *  ESTABLISH KEY
      *
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      XXFND         QCFND
     C                   MOVE      XXFYR         QCFYR
     C                   MOVE      XXCASE        QCCASE
      *
      *  DO NOT WRITE A DARTS RECORD IF THE REMARK CODE WAS NOT SETUP.
      *
     C     XXRMCD        IFNE      *BLANKS
      *
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C                   MOVEL(P)  'W0070R'      Q#PNAM           10
     C                   MOVE      '000'         Q#SECL            3
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'WC'          Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANK        Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZERO         Q#PIDX            5
     C                   MOVEL(P)  *BLANK        Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     *BLANKS       Q#CODE            2
     C                   MOVEL(P)  *BLANKS       Q#DCID           12
     C                   MOVEL(P)  XXRMCD        Q#RMCD            5
     C                   MOVE      *BLANKS       Q#FOLD           80
     C                   MOVE      *BLANK        Q#DIFL            1
     C                   MOVE      *BLANKS       Q#FL01            1
     C                   MOVE      *BLANKS       Q#FL02            1
     C                   MOVE(P)   DSDATE        Q#CDAT            8
     C                   MOVE(P)   *BLANKS       Q#COM2           19
     C                   MOVEL(P)  *BLANKS       Q#COMM          256
      *
     C   71              MOVEL(P)  CPY(1)        Q#COMM
     C   72              MOVEL(P)  CPY(2)        Q#COMM
      *
      *
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CODE
      *
     C                   ENDIF                                                  XXRMCD
      *
     C*
     C                   ENDSR
     C*****************************************************************
     C* SRDART SUBROUTINE              ** Subroutine **   S R D A R T *
     C* THIS SUBROUTINE WRITES A DARTS REMARK FOR EACH REPORT WHICH   *
     C* IS PROCESSED.                                                 *
     C*****************************************************************
     C     SRDART        BEGSR
     C*
     C*  DON'T CREATE THE DARTS REMARK IF THE HARDCOPY DARTS CODE IS
     C*  BLANK OR INVALID.
     C*
     C                   MOVE      '148'         XXTSET
     C*** BEG CHG ****** 03/24/10 *************************************
     C***        IXKY      CHAINPIX01                60
     C                   CALL      'WA076R'      IXAPI                  99
     C     @RTNCD        IFEQ      'Y'
     C                   MOVE      '0'           *IN60
     C                   ELSE
     C                   MOVE      '1'           *IN60
     C                   ENDIF
     C*** END CHG ****** 03/24/10 *************************************
     C   60              GOTO      SKIP1
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      XXFND         QCFND
     C                   MOVE      XXFYR         QCFYR
     C                   MOVE      XXCASE        QCCASE
      *
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C                   MOVEL(P)  'W0070R'      Q#PNAM           10
     C                   MOVE      '000'         Q#SECL            3
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  LGIUSR        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'WC'          Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANK        Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZERO         Q#PIDX            5
     C                   MOVEL(P)  *BLANK        Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     *BLANKS       Q#CODE            2
     C                   MOVE(P)   DSDATE        Q#CDAT            8
     C                   MOVEL(P)  IXHCPR        Q#RMCD            5
      *
     C     Q#RMCD        CHAIN     PCT01                              60
     C   60              GOTO      SKIP1
      *
      *  GET FOLDER NAME
      *
     C                   MOVE      *BLANKS       QQFND             8
     C                   MOVE      *BLANKS       QQSER             8
     C                   MOVE      *BLANKS       QQSUB             8
     C                   MOVE      *BLANKS       QQFOLD            8
     C                   MOVE      *BLANKS       QQFLDX            7
     C                   MOVE      *BLANKS       QQFPTH           40
     C                   MOVE      *BLANKS       QQTERM            1
      *
     C                   MOVEL     XXFND         QQFND                          for Fund
     C                   MOVEL     'CS'          QQSER                          for Area of Service
     C                   MOVEL     CTTYPE        QQSUB                          for Sub-Area
      *
     C                   MOVEL     'C'           QQFOLD                         means "Member"
     C                   MOVEL     CMCASE        QQFLDX                         CASE for Folder name
     C                   MOVE      QQFLDX        QQFOLD
      *
     C                   CALL      'SRFBLD'
     C                   PARM                    QQFND
     C                   PARM                    QQSER
     C                   PARM                    QQSUB
     C                   PARM                    QQFOLD
     C                   PARM                    QQFPTH                         Built, returned
     C                   PARM                    QQTERM
      *
     C                   MOVEL(P)  QQFPTH        Q#FOLD           80
      *
      *  GET DOCUMENT ID
      *
     C                   MOVE      *BLANKS       QQRT              2
     C                   MOVE      *BLANKS       QQDOC            12
     C                   MOVE      *BLANKS       QQTERM            1
      *
     C                   MOVEL     CTTYPE        QQRT                           Remark Type
     C                   CALL      'SRDBLD'
     C                   PARM                    QQFPTH                         From SRFBLD routine
     C                   PARM                    QQRT                           Rmrk Type for Doc ID
     C                   PARM                    QQDOC                          Built, returned
     C                   PARM                    QQTERM
      *
     C                   MOVEL(P)  QQDOC         Q#DCID           12
     C                   MOVE      *BLANK        Q#DIFL            1
     C                   MOVE      *BLANKS       Q#FL01            1
     C                   MOVE      *BLANKS       Q#FL02            1
     C                   MOVE(P)   *BLANKS       Q#COM2           19
     C                   MOVEL(P)  CPY(3)        Q#COMM          256
      *
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CODE
      *
      *  LOAD THE DARTS SEQ# INTO THE EDI LOG FILE
      *
     C     LGKY1         CHAIN     PLG02                              60
      *
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     *ZERO         LGPRSQ
     C                   MOVE      Q#SEQ#        LGPRSQ
     C                   UPDATE    PLG02
     C                   END
      *
      *  CALL W8070C1FL TO RESOLVE THE OUTPUT INTO AN OFFICE DOCUMENT
      *
     C                   MOVE      Q#DCID        QQDCID           12
     C                   MOVE      Q#FOLD        QQFLDR           80
      *
      *
     C                   CALL      'W8070C1F'
     C                   PARM                    Q1JOB
     C                   PARM                    Q1USER
     C                   PARM                    Q1JOBN
     C                   PARM                    QQDCID
     C                   PARM                    QQFLDR
     C*
     C     SKIP1         TAG
     C                   ENDSR
     C*
     C*SUBROUTINE TO MOVE VARIABLE VALUES TO PRINTER FILE.
     C***************************************************************
     C     @@MOVE        BEGSR
     C                   MOVE      XXCSS#        PRCSS#                         SSN NUMBER
     C                   MOVEL(P)  EMFEIN        PRFEIN                         UI ACCOUNT NUMBER
     C                   MOVEL     XXNADS        PRNADS                         PART OF BODY INJURED
     C                   MOVE      XXTIME        PRTIME
     C                   MOVE      XXACDT        PRACDT
     C                   MOVEL     XXBPRT        PRBPRT
     C                   MOVE      XXDOD         PRDOD
     C                   MOVE      XXZIPZ        PRPZIP
     C                   MOVEL     XXPAD1        PRPAD1
     C                   MOVE      XXSEXM        PRSEXM
     C                   MOVE      XXSEXF        PRSEXF
     C                   MOVE      XXCDOB        PRCDOB
     C                   MOVE      XXDT1         PRDT1
     C                   MOVE      XXDTRP        PRDTRP                         DATE EMP KNEW OF INJ
     C                   MOVE      XXIRTN        PRIRTN
     C                   MOVE      XEADRY        PRADRY
     C                   MOVE      XEADRN        PRADRN
     C                   MOVEL     XXPNAM        PRPNAM
     C                   MOVE      XXPCSZ        PRPCSZ
     C                   MOVEL     XXHOSA        PRHOSA
     C                   MOVEL     XXHOSN        PRHOSN
     C                   MOVEL     XXHOSC        PRHOCT
     C                   MOVEL     XXHOSS        PRHOSS
     C                   MOVE      XXCLAR        PRCLAR
     C                   MOVE      XXCLPH        PRCLPH
     C                   MOVE      XXCLA1        PRCLA1
     C                   MOVE      XXCLA2        PRCLA2
     C                   MOVE      XXCLCY        PRCLCY
     C                   MOVE      XXCLST        PRCLST
     C                   MOVE      XXCLZP        PRCLZP
     C                   MOVE      XXDOH         PRDOH
     C                   MOVE      XXRPDT        PRRPDT                         DATE OF REPORT
     C                   MOVEL     XXENAM        PRENAM
     C                   MOVEL(P)  EMPAD1        PRMAD1
     C                   MOVEL(P)  EMPAD2        PRMAD2
     C                   MOVE(P)   EMPCTY        PRMCTY
     C                   MOVEL(P)  EMPST         PRMST
     C                   MOVE(P)   EMPZIP        PRMZIP
     C                   MOVE(P)   EMAREA        PRAREA
     C                   MOVE(P)   EMPHN#        PRPHN#
     C                   MOVE      XXNAOB        PRNAOB
     C                   MOVE      XXTIMM        PRNRTM
     C                   MOVE      XXDT2A        PRFSDT                         LAST FULL DAY PAID
     C                   MOVEL     NI40A4        PRDEPT                         DEPT/DIV EMPLOYED
     C                   MOVEL(P)  CMOCDS        PROCDS
     C                   MOVEL     XXAADR        PRAADR
     C                   MOVEL     NI20A2        PR20A2
     C                   MOVEL     NI60A3        PRNI63
     C                   MOVEL     NI60A4        PRNI64
     C                   MOVEL     NI80A1        PR80A1
     C                   MOVEL     NI80A2        PR80A2
     C                   MOVE      XXWGP1        PRWGP1
     C                   MOVE      XXWGP2        PRWGP2
     C                   MOVE      XXWGP3        PRWGP3
     C                   MOVE      XXWGP4        PRWGP4
     C                   Z-ADD     XX7N1         PRCMRT
     C                   MOVE      XXIDT3        PRNDT3
     C                   MOVE      X120A4        P120A4
     C                   MOVE      X220A4        P220A4
     C                   MOVE      X320A4        P320A4
     C                   MOVE      X420A4        P420A4
     C                   MOVE      X520A4        P520A4
     C                   MOVE      X620A4        P620A4
     C                   MOVE      X720A4        P720A4
     C                   MOVEL     NI20A3        PRATA3                         PARISH OF INJURY
     C*******              MOVELNIPRNM    PRPRNM           PERSON COMP REPORT
     C                   MOVEL     NI40A1        PR40A1                         EMP. MAILING ADDRESS
     C                   MOVEL     NI25A1        PRNI25                         EMP. MAILING CITY
     C                   MOVEL     NI2A1         PRNI20                         EMP. MAILING STATE
     C                   MOVEL     NI9N2         PRN9N2                         EMP. MAILING ZIP
     C                   ENDSR
     C**************************************************************************
**  COPY MSG
EMPLOYER COPY
EMPLOYEE COPY
DIVISION COPY
