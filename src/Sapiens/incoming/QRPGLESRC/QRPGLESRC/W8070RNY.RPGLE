     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W8070RCA - California NOI Print Program
     H*****************************************************************
     H* Program Description:  This program prints New York NOI        *
     H*                       forms.                                  *
     H*                                                               *
     H* Called from:  W8070CNY                                        *
     H* Calls to:  None                                               *
     H*                                                               *
     H*****************************************************************
     H* Created 07/19/2005 by c:132    Project 129WIX                 *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*   06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H*   10/17/11 C:279 Project: SY195                               *
     H*                  - Modify program to pass/return UNC path     *
     H*                    field and IFS flag to called/calling       *
     H*                    programs.                                  *
     H*                                                               *
     H* 03/24/10 C:232  Project: 139WNN                               *
     H*                 Update programs to use the new EDI Control API*
     H*                 WA076R instead of using WMIXP.                *
     H*                                                               *
     H*****************************************************************
     FWMLGL1    IF   E           K DISK
     FWMLGL     UF   E           K DISK
     F                                     RENAME(PLG01:PLG02)
     FWXNIL     IF   E           K DISK
     FWXNIL1    IF   E           K DISK
     F                                     RENAME(PNI01:PNI02)
     FSMORL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMPDL     IF   E           K DISK
     FWMRPL     IF   E           K DISK
     FWMTPL1    IF   E           K DISK
     FWMCXL     IF   E           K DISK
     FWMNIL     IF   E           K DISK
     F                                     RENAME(PNI01:PNI04)
     FWMC1L     IF   E           K DISK
     F*** BEGIN DLT *** 03/24/10 **************************************
     F***WMIXL   IF  E           K        DISK
     F*** END DLT ***** 03/24/10 **************************************
     FSDCMLD    O    E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FSDCMLA    IF   E           K DISK
     F                                     RENAME(PCM01:PCM03)
     FSTCTL1    IF   E           K DISK
     FWTGTL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     INFDS(DBFDS3)
     FQSYSPRT   O    F  127        PRINTER OFLIND(*INOA)
     F                                     USROPN
     FQSYSPRT1  O    F   85        PRINTER OFLIND(*INOF)
     F                                     USROPN
     FQSYSPRT2  O    F  127        PRINTER OFLIND(*INOG)
     F                                     USROPN

     D*================================================================
     D* Arrays and Tables
     D*================================================================
     D CNB             S              1    DIM(40) ASCEND                       COMPANY NAME W/ BLAN
     D CN              S              1    DIM(40)                              COMPANY NAME
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D CPY             S             21    DIM(3) CTDATA PERRCD(1)              COPY MSG
     D OVR             S              1    DIM(33)
      ***** END ADD ***** 10/17/11 ***********************************
     D*================================================================
     D* Constants
     D*================================================================
     D*
     D XXOVR           C                   CONST('OVRPRTF QSYSPRT  OUT-
     D                                     Q(          )')
     D XXOVR1          C                   CONST('OVRPRTF QSYSPRT1 OUT-
     D                                     Q(          )')
     D XXOVR2          C                   CONST('OVRPRTF QSYSPRT2 OUT-
     D                                     Q(          )')
     D*================================================================
     D* Data Structures
     D*================================================================
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
      ***** BEG ADD ***** 10/17/11 ***********************************
     D RPGM            C                   CONST('RTVDOCROOT')
     D RPGM1           C                   CONST('RTVDOCENV')
      ***** END ADD ***** 10/17/11 ***********************************
     D*================================================================
     D* User Define Data Structures
     D*================================================================
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
     D                 DS
     D  XXZIP                  1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
     D*
     D                 DS
     D  NI10A1                 1     10
     D  WD                     1      7
     D                                     DIM(7)                               DAY OF WK WORKED
     D                 DS
     D  XOWRWK                 1     34
     D  XOMON                  1      1
     D  XOTUE                  6      6
     D  XOWED                 12     12
     D  XOTHU                 17     17
     D  XOFRI                 23     23
     D  XOSAT                 29     29
     D  XOSUN                 34     34

     I*================================================================
     I* File Field Renames
     I*================================================================
     IPCM03
     I              CMKEY                       C3KEY
     I              CMCSID                      C3CSID
     I              CMCOMM                      C3COMM
     I              CMCDAT                      C3CDAT
     I              CMCODE                      C3CODE
     I              CMDCID                      C3DCID
     I              CMPNAM                      C3PNAM
     I              CMTRDT                      C3TRDT
     I              CMTRTM                      C3TRTM
     I              CMSEQ#                      C3SEQ#
     I              CMSECL                      C3SECL
     I              CMSBYL                      C3SBYL
     I              CMSTAT                      C3STAT
     I              CMUSER                      C3USER
     I              CMPGM                       C3PGM
     I              CMTIME                      C3TIME
     I              CMDT                        C3DT
     I              CMIUSR                      C3IUSR
     I              CMFOLD                      C3FOLD
     I              CMDCDT                      C3DCDT
     I              CMDIFL                      C3DIFL
     I              CMAPID                      C3APID
     I              CMIDOC                      C3IDOC
     I              CMIQUE                      C3IQUE
     I              CMIMGF                      C3IMGF
     I              CMIPTH                      C3IPTH
     I              CMPIDX                      C3PIDX
     I              CMQPTY                      C3QPTY
     I              CMDCTP                      C3DCTP
     I              CMIPGM                      C3IPGM
     I              CMSMCD                      C3SMCD
     I              CMFL01                      C3FL01
     I              CMFL02                      C3FL02
     I              CMFL03                      C3FL03
     I              CMFL04                      C3FL04
     I              CMFL05                      C3FL05
     I              CMRMCD                      C3RMCD
     I              CMIDT                       C3IDT
      ***** BEG ADD ***** 10/17/11 ***********************************
     I              CMUNCN                      C3UNCN
     I              CMSHRN                      C3SHRN
     I              CMIFSP                      C3IFSP

     C*================================================================
     C* Parameter lists
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FROI            2
     C                   PARM                    @@JRST            2
     C                   PARM                    @@DATE            8
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
     C*
     C*  TO SIX DIGIT YEAR
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C*** BEG ADD ****** 03/24/10 *************************************
     C     IXAPI         PLIST
     C                   PARM                    KKCO#
     C                   PARM                    KKJRST            2
     C                   PARM                    KKTSET            3
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
     C*================================================================
     C* Key lists
     C*================================================================
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    KKCO#
     C*
     C*  KEY LIST FOR THE NOI WORKFILE
     C*
     C     NIKY          KLIST
     C                   KFLD                    KKCO#
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*  KEY LIST FOR THE FUND AUXILIARY FILE
     C*
     C     F2KY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*  KEY LIST FOR THE LOG HISTORY FILE
     C*
     C     LGKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKHCPY
     C                   KFLD                    KKJRST
     C                   KFLD                    KKDATE
     C*
     C     LGKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    LGIDT
     C                   KFLD                    LGITM
     C*
     C*  KEY LIST FOR THE DARTS REMARK CODE FILE
     C*
     C     CTKY          KLIST
     C                   KFLD                    IXHCPR
     C*
     C*  KEY LIST FOR THE EDI CLAIM CONTROL FILE
     C*
     C     IXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKJRST
     C                   KFLD                    KKTSET
     C*
     C*  KEY LIST FOR THE PAYEE MASTER FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C*  TREATING PHYSICIAN
     C*
     C     TPKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*  KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*  KEY LIST FOR THE CARRIER/STATE REPORTING FILE
     C*
     C     RPKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*  KEY LIST FOR THE ADDITIONAL LOCATION FILE
     C*
     C     ALKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMCTG1
     C*
     C*  KEY LIST FOR THE DARTS FILE
     C*
     C     C3KY          KLIST
     C                   KFLD                    QCKEY
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
      *
      *  GENERIC TABLE FILE
      *
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
      *
      *  TABLE FILE
      *
     C     TBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
      *
      *  Key for User Profile file
      *
     C     UPKY          KLIST
     C                   KFLD                    CMUSER
     C*
     C*================================================================
     C* Like Defines
     C*================================================================
     C     *LIKE         DEFINE    NICO#         KKCO#
     C     *LIKE         DEFINE    NIFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    NIDT          KKDATE
     C     *LIKE         DEFINE    LGHCPY        KKHCPY
     C*** BEG DEL ****** 03/24/10 *************************************
     C***        *LIKE     DEFN IXJRST    KKJRST
     C***        *LIKE     DEFN IXTRID    KKTSET
     C*** END DEL ****** 03/24/10 *************************************
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    PDFEIN        KKFEIN
     C     *LIKE         DEFINE    PDMOD         KKMOD
     C*
     C     *LIKE         DEFINE    NIFND         SVFND
     C     *LIKE         DEFINE    LGTSET        SVTSET
     C*
     C     *LIKE         DEFINE    CMDCID        ##DCID
     C     *LIKE         DEFINE    CMFOLD        ##FLDR
     C*
     C     *LIKE         DEFINE    CMRMCD        XXRMCD
     C     *LIKE         DEFINE    F2SCTC        XXSCTC
     C*
     C*================================================================
     C* Main Processing
     C*================================================================
      ***** BEG ADD ***** 10/17/11 ***********************************
      * GET ROOT
     C                   CALL      RPGM
     C                   PARM                    ROOT             10
      * Find IFS
     C                   CALL      RPGM1
     C                   PARM                    ENV               1
      ***** END ADD ***** 10/17/11 ***********************************
     C                   MOVE      *BLANKS       XXCPY            21
     C                   Z-ADD     0             X                 5 0
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKDATE
     C                   MOVEL     @@FROI        KKHCPY
     C                   MOVE      @@JRST        KKJRST
     C*
     C     Q1PRMS        IFGT      4
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@FYR         KKFYR
     C                   MOVE      @@CASE        KKCASE
     C                   ELSE
     C                   MOVE      *ZEROS        KKFND
     C                   MOVE      *ZEROS        KKFYR
     C                   MOVE      *ZEROS        KKCASE
     C                   ENDIF
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      *BLANKS       XXOUTQ           10
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              99
     C     OROUTQ        IFEQ      *BLANK
     C                   MOVEL     'QPRINT'      XXOUTQ           10
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       OROUTQ
     C                   MOVE      '1'           XXPRT
     C     ORKY          CHAIN     POR01                              99
     C                   MOVEL     OROUTQ        X1OUTQ           10
     C     X1OUTQ        IFEQ      *BLANK
     C                   MOVEL     'QPRINT'      X1OUTQ
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       OROUTQ
     C                   MOVE      '2'           XXPRT
     C     ORKY          CHAIN     POR01                              99
     C                   MOVEL     OROUTQ        X2OUTQ           10
     C     X2OUTQ        IFEQ      *BLANK
     C                   MOVEL     'QPRINT'      X2OUTQ
     C                   ENDIF
     C*
     C*  IF THIS PGM WAS CALLED FROM THE DAILY BATCH, WXNIP WILL BE READ
     C*  3 TIMES TO PRODUCE 3 COPIES.  IF CALLED FROM THE EXTRACT
     C*  OPTION, 1 READ WILL OCCUR.
     C*
     C     KKCASE        IFEQ      *ZEROS
     C     @@FROI        IFNE      '  '
     C                   Z-ADD     3             XXCOPY            1 0
     C                   ELSE
     C                   Z-ADD     1             XXCOPY
     C                   ENDIF
     C                   ENDIF
     C*
     C     1             DO        2
     C*
     C     KKCASE        IFEQ      *ZEROS
     C                   Z-ADD     0             SVFND
     C                   Z-ADD     XXCOPY        X
     C                   MOVE      CPY(X)        XXCPY
     C     X             COMP      1                                      71
     C     X             COMP      2                                      72
     C     X             COMP      3                                      73
     C                   ENDIF
     C*
     C                   SETOFF                                       75
     C     KKCASE        IFNE      *ZEROS
     C                   SETON                                        75
     C                   ENDIF
     C*
     C*   OPEN THE PRINT FILE
     C*
     C     *IN73         IFEQ      *OFF
     C     XXCOPY        IFEQ      1
     C                   OPEN      QSYSPRT
     C                   MOVE      *ON           XXOPEN
     C                   ENDIF
     C     XXCOPY        IFEQ      2
     C                   OPEN      QSYSPRT1
     C                   MOVE      *ON           XXOPN1
     C                   OPEN      QSYSPRT2
     C                   MOVE      *ON           XXOPN2
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN75         IFEQ      *ON
     C                   OPEN      QSYSPRT2
     C                   MOVE      *ON           XXOPN2
     C                   Z-ADD     0             XXCOPY
     C                   MOVE      *BLANKS       XXCPY
     C                   ENDIF
     C*
     C                   SELECT
     C     *IN71         WHENEQ    *ON
     C     NIKY          CHAIN     PNI01                              99
     C     *IN72         WHENEQ    *ON
     C     NIKY          CHAIN     PNI02                              99
     C     *IN73         WHENEQ    *ON
     C     LGKY          CHAIN     PLG01                              99
     C     *IN75         WHENEQ    *ON
     C     CMKY          CHAIN     PCM01                              99
     C                   ENDSL
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   DO
     C*
     C     *IN71         IFEQ      '1'
     C     NIIDT         ANDNE     KKDATE
     C     *IN72         OREQ      '1'
     C     NIIDT         ANDNE     KKDATE
     C                   ITER
     C                   ENDIF
     C     *IN73         IFEQ      *ON
     C     LGPRSQ        IFNE      0
     C     LGSCHP        ORNE      LGEXDT
     C                   ITER
     C                   ENDIF
     C                   ENDIF
     C*
     C*   OPEN THE PRINT FILE
     C*
     C     *IN73         IFEQ      *ON
     C                   OPEN      QSYSPRT2
     C                   MOVE      *ON           XXOPN2
     C                   ENDIF
     C*
     C                   EXSR      SRINIT
     C*
     C*  IF THE FUND CHANGED, GET THE NEW FUND INFORMATION.
     C*
     C     KKFND         IFNE      SVFND
     C                   EXSR      FNDBRK
     C                   Z-ADD     KKFND         SVFND
     C                   ENDIF
     C*
     C*  LOAD CLAIMANT INFORMATION
     C*
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C*
     C     CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C*
     C     CMKY          CHAIN     PNI04                              99
     C     *IN99         IFEQ      *ON
     C                   CLEAR                   PNI04
     C                   ENDIF
     C*
     C     CMKY          CHAIN     PC101                              99
     C     *IN99         IFEQ      *ON
     C                   CLEAR                   PC101
     C                   ENDIF
     C*
     C* GET EMPLOYER INFO
     C*
     C     EMKY          CHAIN     PEM01                              99
     C     *IN99         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C*
     C                   MOVEA     '00000'       *IN(21)
     C                   MOVEA     '00000'       *IN(26)
     C                   MOVEA     '00000'       *IN(31)
     C                   MOVEA     '000000'      *IN(36)
     C*================================================================
     C* Employer Info
     C*================================================================
     C*
     C*  Policy number, Carrier Case number, WCB Case number
     C*
     C*  WCB Case #
     C                   MOVEL     CMACL#        ZZACL#            8
     C                   TESTN                   ZZACL#               69
     C     *IN69         IFEQ      *ON
     C                   MOVE      ZZACL#        XXACL#            8 0          WCB CASE #
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXACL#            8 0          WCB CASE #
     C                   ENDIF                                                  *IN69 = *ON
     C*
     C                   MOVE      CMFND         XXFND             3
     C                   MOVE      CMEMP#        XXEMP#            9
     C                   MOVE      CMFYR         XXFYR             3
     C*
     C     RPKY          CHAIN     PRP01                              99
     C     *IN99         IFEQ      *OFF
     C     RPCAP#        ANDNE     *BLANKS
     C                   MOVEL(P)  RPCAP#        XXCAP#           17            WC Policy #
     C                   ELSE
     C                   MOVEL(P)  XXFND         XXCAP#
     C     XXCAP#        CAT       '-':0         XXCAP#
     C                   CAT       XXEMP#:0      XXCAP#
     C                   CAT       '-':0         XXCAP#
     C                   CAT       XXFYR:0       XXCAP#
     C                   ENDIF
     C*
     C*  Carrier Case #
     C*
     C                   MOVE      CMCASE        XXCASE            7
     C     XXFND         CAT(P)    '-':0         XXCCA#           15
     C                   CAT       XXFYR:0       XXCCA#
     C                   CAT       '-':0         XXCCA#
     C                   CAT       XXCASE:0      XXCCA#
     C*
     C*  Box 1a - Firm name
     C*
     C* EMNAME
     C*
     C*  Box 1b - Mailing Address
     C*
     C                   MOVEL     EMMZIP        XXZIPA            5
     C                   MOVE      EMMZIP        XXZIPB            4
     C*
     C     EMMAD1        IFNE      *BLANKS
     C     EMMAD1        CAT(P)    EMMAD2:2      XXMADR           50
     C                   CAT       EMMCTY:2      XXMADR
     C                   CAT       ',':0         XXMADR
     C                   CAT       EMMST:1       XXMADR
     C                   CAT       XXZIPA:2      XXMADR
     C     XXZIPB        IFNE      '0000'
     C                   CAT       '-':0         XXMADR
     C                   CAT       XXZIPB:0      XXMADR
     C                   ENDIF
     C                   ELSE
     C                   CLEAR                   XXMADR
     C                   ENDIF
     C*
     C*  Box 1c - OSHA Case/File #
     C*
     C                   MOVEL(P)  NI20A2        ZZ20A2            8
     C                   MOVE      *ZEROS        XXNYUI
     C*
     C                   TESTN                   ZZ20A2               69
     C     *IN69         IFEQ      *ON
     C                   MOVE      ZZ20A2        XXNYUI            8 0
     C                   ENDIF
     C* NI20A2
     C*
     C*  Box 1d - Location
     C*
     C                   MOVEL     EMPZIP        XXZIPA            5
     C                   MOVE      EMPZIP        XXZIPB            4
     C                   CLEAR                   XXPADR
     C*
     C     EMPAD1        IFNE      *BLANKS
     C                   MOVEL(P)  EMPAD1        XXPADR           35
     C     EMPAD2        IFNE      *BLANKS
     C                   CAT       EMPAD2:2      XXPADR
     C                   ENDIF
     C                   CAT       EMPCTY:2      XXPADR
     C                   CAT       ',':0         XXPADR
     C                   CAT       EMPST:1       XXPADR
     C                   CAT       XXZIPA:2      XXPADR
     C     XXZIPB        IFNE      '0000'
     C                   CAT       '-':0         XXPADR
     C                   CAT       XXZIPB:0      XXPADR
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL     NI9N2         XXZIPA
     C                   MOVE      NI9N2         XXZIPB
     C*
     C     NI40A1        IFNE      *BLANKS
     C     NI40A1        CAT(P)    NI40A2:2      XXPADR           35
     C                   CAT       NI25A1:2      XXPADR
     C                   CAT       ',':0         XXPADR
     C                   CAT       NI2A1:1       XXPADR
     C                   CAT       XXZIPA:2      XXPADR
     C     XXZIPB        IFNE      '0000'
     C                   CAT       '-':0         XXPADR
     C                   CAT       XXZIPB:0      XXPADR
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Box 1e - Nature of Business
     C*
     C                   MOVEL(P)  'SIC'         KKTBID
     C                   MOVEL(P)  EMSIC         KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTDESC        XXSICD           31
     C                   ELSE
     C                   CLEAR                   XXSICD
     C                   ENDIF
     C*
     C*================================================================
     C* Employee Info
     C*================================================================
     C*
     C*  Box 3a - Name
     C*
     C                   MOVEL(P)  CMFSN         XXCLNM           46
     C     CMMID         IFNE      *BLANKS
     C                   CAT       CMMID:1       XXCLNM
     C                   CAT       '.':0         XXCLNM
     C                   ENDIF
     C                   CAT       CMLSN:1       XXCLNM
     C*
     C*  Soc Sec #
     C***  Begin Chg  ***  06/09/20  *******************************
     C*                  MOVE      *ZEROS        XXSS#             9 0
     C                   MOVE      *BLANKS       XXSS#            21
     C***  End Chg  ***  06/09/20  *******************************
     C                   TESTN                   CMSS#                98
     C     *IN98         IFEQ      *ON
     C***  Begin Chg  ***  06/09/20  *******************************
     C*                  MOVE      CMSS#         XXSS#             9 0
     C                   MOVE      CMSS#         XXSS#
     C***  End Chg  ***  06/09/20  *******************************
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'XXSS#'       QQNAME           25
     C     XXSS#         PARM      XXSS#         QQVAL           100
     C                   EVAL      XXSS#=%SUBST(XXSS#:1:1)+' '+%SUBST(XXSS#:2:1)
     C                             +' '+%SUBST(XXSS#:3:1)+'   '
     C                             +%SUBST(XXSS#:4:1)+' '+%SUBST(XXSS#:5:1)
     C                             +'   '+%SUBST(XXSS#:6:1)+' '
     C                             +%SUBST(XXSS#:7:1)+' '+%SUBST(XXSS#:8:1)
     C                             +' '+%SUBST(XXSS#:9:1)
     C***  End Add  ***  06/09/20  *******************************
     C                   ENDIF
     C*********
     C*********  3         SUBSTCMSS#:1   XXSS1   3 P
     C*********  2         SUBSTCMSS#:4   XXSS2   2 P
     C*********  4         SUBSTCMSS#:6   XXSS3   4 P
     C*********
     C*********  XXSS1     CAT  '-':0     XXSS#  11 P
     C*********            CAT  XXSS2:0   XXSS#
     C*********            CAT  '-':0     XXSS#
     C*********            CAT  XXSS3:0   XXSS#
     C*
     C*  Box 3b - Address
     C*
     C                   MOVEL     CDZIP         XXZIPA            5
     C                   MOVE      CDZIP         XXZIPB            4
     C                   CLEAR                   XXCADR
     C*
     C                   MOVEL(P)  CDADR1        XXCADR           60
     C     CDADR2        IFNE      *BLANKS
     C     XXCADR        CAT       CDADR2:2      XXCADR
     C                   ENDIF
     C                   CAT       CDCTY:2       XXCADR
     C                   CAT       ',':0         XXCADR
     C                   CAT       CDST:1        XXCADR
     C                   CAT       XXZIPA:2      XXCADR
     C     XXZIPB        IFNE      '0000'
     C                   CAT       '-':0         XXCADR
     C                   CAT       XXZIPB:0      XXCADR
     C                   ENDIF
     C*
     C*================================================================
     C* Injury or Illness Info
     C*================================================================
     C*
     C*  - Date of Injury
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C*
     C*  Box 4a - Location where event or exposure
     C*
     C* CMACNM
     C*
     C                   MOVE      CMACZ1        XXZIPA
     C                   MOVE      CMACZ2        XXZIPB
     C*
     C     CMACDR        IFNE      *BLANKS
     C     NI40A3        IFNE      *BLANKS
     C     CMACDR        CAT(P)    NI40A3:2      XXAADR           50
     C                   CAT       CMACTY:2      XXAADR
     C                   ELSE
     C     CMACDR        CAT       CMACTY:2      XXAADR
     C                   ENDIF
     C                   CAT       ',':0         XXAADR
     C                   CAT       CMAST:1       XXAADR
     C                   CAT       XXZIPA:2      XXAADR
     C     XXZIPB        IFNE      '0000'
     C                   CAT       '-':0         XXAADR
     C                   CAT       XXZIPB:0      XXAADR
     C                   ENDIF
     C                   ELSE
     C                   CLEAR                   XXAADR
     C                   ENDIF
     C*
     C*  Box 4b - County
     C*
     C                   MOVEL     NI30A3        XX30A3           20
     C                   MOVEL     NI40A4        XX40A4           10
     C*
     C*  Box 4c - On employer's premises?
     C*
     C                   SETOFF                                       32
     C     CMEADR        IFEQ      'Y'
     C                   SETON                                        32
     C                   ENDIF
     C*
     C*  Box 5 - Time employee began work
     C*
     C                   SETOFF                                       28
     C                   MOVE      NITIM2        TM6               6 0
     C                   MOVEL     TM6           XXTIM2            4 0
     C                   MOVEL     XXTIM2        TEST1
     C     TEST1         IFGE      12
     C                   SETON                                        28
     C     TEST1         IFGT      12
     C                   SUB       1200          XXTIM2
     C                   ENDIF
     C                   ELSE
     C     TEST1         IFEQ      0
     C                   ADD       1200          XXTIM2
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Box 6 - Time of Injury
     C*
     C                   SETOFF                                       27
     C                   MOVEL     CMACTM        XXACTM            4 0
     C                   MOVEL     XXACTM        TEST1             2 0
     C     TEST1         IFGE      12
     C                   SETON                                        27
     C     TEST1         IFGT      12
     C                   SUB       1200          XXACTM
     C                   ENDIF
     C                   ELSE
     C     TEST1         IFEQ      0
     C                   ADD       1200          XXACTM
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Box 7 - Dept where event/exposure occurred
     C*
     C* NI40A4
     C*
     C*  Box 8.a - Date last worked
     C*
     C     CMDTLW        IFNE      *ZEROS
     C                   Z-ADD     CMDTLW        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDTLW            6 0
     C                   ELSE
     C                   CLEAR                   XXDTLW
     C                   ENDIF
     C*
     C*  Box 8b - Paid full days wages for DOI
     C*
     C                   SETOFF                                       30
     C     CMPOI         IFNE      'Y'
     C                   SETON                                        30
     C                   ENDIF
     C*
     C*  Box 9 - Sex
     C*
     C                   SELECT
     C     CDSEX         WHENEQ    'M'
     C                   SETOFF                                       36
     C     CDSEX         WHENEQ    'F'
     C                   SETON                                        36
     C                   ENDSL
     C*
     C*  Box 10 - Date of Birth
     C*
     C     CDDTOB        IFNE      *ZEROS
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDTOB            6 0
     C                   ELSE
     C                   CLEAR                   XXDTOB
     C                   ENDIF
     C*
     C*  Box 11 -  Occupation
     C*
     C* CMOCDS
     C*
     C*  Box 12 - Date of Hire
     C*
     C     CMHIDT        IFNE      *ZEROS
     C                   Z-ADD     CMHIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXHIDT            6 0
     C                   ELSE
     C                   CLEAR                   XXHIDT
     C                   ENDIF
     C*
     C*  Box 13a - Average Earnings per Week
     C*
     C     CMAWW         MULT(H)   1             XXAWW             5 0
     C*
     C*  Box 13b - Total Earnings (52 Weeks)
     C*
     C                   Z-ADD     NI5N1         XX5N1             5 0
     C*
     C*  Box 14a - Employee is FT or PT
     C*
     C                   MOVE      *OFF          *IN37
     C                   MOVE      *OFF          *IN38
     C*
     C     CMESTS        IFEQ      'FT'
     C     CMESTS        OREQ      'ft'
     C                   SETON                                        37
     C                   ENDIF
     C     CMESTS        IFEQ      'PT'
     C     CMESTS        OREQ      'pt'
     C                   SETON                                        38
     C                   ENDIF
     C*
     C*  Box 14b - Injured Employee Work Week
     C*
     C*  The workdays are numbered from 1 (Monday) to 7 (Sunday)
     C                   MOVE      *BLANKS       XOWRWK
     C                   MOVE      '0000000'     *IN(81)
     C                   Z-ADD     *ZEROS        Z                 3 0
     C*
     C     Z             DOUEQ     7
     C                   ADD       1             Z
     C                   SELECT
     C     WD(Z)         WHENEQ    '1'
     C                   MOVE      *ON           *IN81
     C     WD(Z)         WHENEQ    '2'
     C                   MOVE      *ON           *IN82
     C     WD(Z)         WHENEQ    '3'
     C                   MOVE      *ON           *IN83
     C     WD(Z)         WHENEQ    '4'
     C                   MOVE      *ON           *IN84
     C     WD(Z)         WHENEQ    '5'
     C                   MOVE      *ON           *IN85
     C     WD(Z)         WHENEQ    '6'
     C                   MOVE      *ON           *IN86
     C     WD(Z)         WHENEQ    '7'
     C                   MOVE      *ON           *IN87
     C                   OTHER
     C                   ENDSL
     C                   ENDDO
     C*
     C   81              MOVE      'X'           XOMON
     C   82              MOVE      'X'           XOTUE
     C   83              MOVE      'X'           XOWED
     C   84              MOVE      'X'           XOTHU
     C   85              MOVE      'X'           XOFRI
     C   86              MOVE      'X'           XOSAT
     C   87              MOVE      'X'           XOSUN
     C*
     C*  Box 15 - Specific injury/illness and part of body
     C*           affected, medical diagnosis
     C*
     C                   CLEAR                   XXNPC
     C                   MOVEL(P)  CMNATR        KKKEY
     C     TBKY          CHAIN     PNA01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  NADESC        XXNPC            52
     C                   ENDIF
     C*
     C                   MOVEL(P)  CMPART        KKKEY
     C     TBKY          CHAIN     PPA01                              99
     C     *IN99         IFEQ      *OFF
     C     XXNPC         IFEQ      *BLANKS
     C                   MOVEL(P)  PADESC        XXNPC
     C                   ELSE
     C                   CAT       ';':0         XXNPC
     C                   CAT       PADESC:1      XXNPC
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL(P)  CMCAUS        KKKEY
     C     TBKY          CHAIN     PCA01                              99
     C     *IN99         IFEQ      *OFF
     C     XXNPC         IFEQ      *BLANKS
     C                   MOVEL(P)  CADESC        XXNPC
     C                   ELSE
     C                   CAT       ';':0         XXNPC
     C                   CAT       CADESC:1      XXNPC
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Box 16a - Employer Provide Medical Care?
     C*
     C                   SETOFF                                       29
     C     CMPOI         IFEQ      'Y'
     C                   SETON                                        29
     C                   ENDIF
     C*
     C*  Box 16b - Date Employer Provided Medical Care
     C*
     C                   Z-ADD     NIDT1         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXMCDT            8 0
     C*
     C*  Box 17 - Employer Treated in Emergency Room?
     C*
     C                   SETOFF                                       34
     C     NI1A2         IFEQ      'Y'
     C                   SETON                                        34
     C                   ENDIF
     C*
     C*  Box 18 - Hospitalized as inpatient?
     C*
     C                   SETOFF                                       35
     C     NI1A3         IFEQ      'Y'
     C                   SETON                                        35
     C                   ENDIF
     C*
     C*  Box 19a - Name and address of Physician
     C*
     C                   MOVEL     TPPHYZ        XXZIPA
     C                   MOVE      TPPHYZ        XXZIPB
     C                   CLEAR                   XXPHYN
     C                   CLEAR                   XXTADR
     C*
     C                   MOVE      *OFF          *IN98
     C     TPKY          CHAIN     PTP01                              99
     C     *IN99         IFEQ      *OFF
     C     TPPHYF        IFNE      *ZEROS
     C                   MOVE      TPPHYF        KKFEIN
     C                   MOVE      TPPHYM        KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              98
     C     *IN98         IFEQ      *OFF
     C                   MOVEL(P)  PDNAME        XXPHYN           40
     C                   MOVEL(P)  PDADR1        XXTADR           50
     C     PDADR2        IFGT      *BLANKS
     C     XXTADR        CAT       PDADR2:1      XXTADR
     C                   ENDIF                                                  PDAADR2 > *BLNK
     C                   CAT       ',':0         XXTADR
     C                   CAT       PDCTY:1       XXTADR
     C                   CAT       ',':0         XXTADR
     C                   CAT       PDST:1        XXTADR
     C                   MOVE      PDZIP         XXPDZP            9
     C                   CAT       XXPDZP:1      XXTADR
     C                   ENDIF                                                  *IN98 = *OFF
     C                   ELSE
     C                   MOVEL(P)  TPPHYN        XXPHYN
     C     TPPHYA        CAT(P)    TPPHYC:1      XXTADR
     C                   CAT       ',':0         XXTADR
     C                   CAT       TPPHYS:1      XXTADR
     C                   CAT       XXZIPA:1      XXTADR
     C     XXZIPB        IFNE      '0000'
     C                   CAT       '-':0         XXTADR
     C                   CAT       XXZIPB:0      XXTADR
     C                   ENDIF                                                  XXZIPB <> ZEROS
     C                   ENDIF                                                  TPPHYF = ZERO
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C*  Box 19b - Name & Address of Hospital
     C*
     C     *IN35         IFEQ      *ON
     C*
B01  C     CMHOSF        IFNE      *ZEROS
     C*
     C                   Z-ADD     CMHOSF        KKFEIN
     C                   Z-ADD     CMHOSM        KKMOD
     C     PDKY          CHAIN     PPD01                              99
B02  C     *IN99         IFEQ      *OFF
     C                   MOVEL     PDZIP         XXZIPA
     C                   MOVE      PDZIP         XXZIPB
B03  C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR1        XXHOSN
     C     PDADR2        CAT(P)    PDCTY:2       XXHADR
     C                   CAT       ',':0         XXHADR
     C                   CAT       PDST:1        XXHADR
     C                   CAT       XXZIPA:2      XXHADR
B04  C     XXZIPB        IFNE      '0000'
     C                   CAT       '-':0         XXHADR
     C                   CAT       XXZIPB:0      XXHADR
E04  C                   ENDIF
S03  C                   ELSE
B05  C     PDFSN         IFNE      *BLANKS
     C     PDFSN         CAT(P)    PDNAME:1      XXHOSN
     C                   ELSE
     C                   MOVEL(P)  PDNAME        XXHOSN
E05  C                   ENDIF
     C     PDADR1        CAT(P)    PDADR2:2      XXHADR
     C                   CAT       PDCTY:1       XXHADR
     C                   CAT       ',':0         XXHADR
     C                   CAT       PDST:1        XXHADR
     C                   CAT       XXZIPA:2      XXHADR
B06  C     XXZIPB        IFNE      '0000'
     C                   CAT       '-':0         XXHADR
     C                   CAT       XXZIPB:0      XXHADR
E06  C                   ENDIF
E03  C                   ENDIF
S02  C                   ELSE
     C                   CLEAR                   XXHOSN
     C                   CLEAR                   XXHADR
E02  C                   ENDIF
     C*
S01  C                   ELSE
     C*
     C                   MOVEL     CMHOSZ        XXZIPA
     C                   MOVE      CMHOSZ        XXZIPB
     C*
     C                   MOVEL     CMHOSN        XXHOSN           40
     C     CMHOSA        IFNE      *BLANKS
     C     CMHOSA        CAT(P)    CMHOSC:2      XXHADR           50
     C                   CAT       ',':0         XXHADR
     C                   CAT       CMHOSS:1      XXHADR
     C                   CAT       XXZIPA:2      XXHADR
     C     XXZIPB        IFNE      '0000'
     C                   CAT       '-':0         XXHADR
     C                   CAT       XXZIPB:0      XXHADR
     C                   ENDIF
     C                   ELSE
     C                   CLEAR                   XXHADR
     C                   ENDIF
     C                   ENDIF
     C*
E01  C                   ENDIF
     C*
     C*  Box 20.a - If returned to work, check box
     C*
     C                   SETOFF                                       39
     C     CMRTW         IFNE      *ZEROS
     C                   SETON                                        39
     C                   ENDIF
     C*
     C*  Box 20.b - Date returned to work
     C*
     C     CMRTW         IFNE      *ZEROS
     C                   Z-ADD     CMRTW         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXRTW             6 0
     C                   ELSE
     C                   CLEAR                   XXRTW
     C                   ENDIF
     C*
     C*  Box 20.c - Returned at what weekly wage ?
     C*
     C     CMPOWW        MULT(H)   1             XXPOWW            5 0
     C*
     C*  Box 21 - Specific activity employee was
     C*           performing when event occurred
     C*
     C* NI60A1
     C* NI60A2
     C*
     C*  Box 22 - How illness occurred
     C*
     C* NI80A1
     C* NI80A2
     C*
     C*  Box 23 - Object or substance that directly injured employee
     C*
     C     NI60A1        CAT(P)    NI60A2:1      XXOBJ           100
     C*
     C*  Box 24 - Date of Death
     C*
     C                   MOVE      *ZEROS        XXDOD             6 0
     C     CMDOD         IFNE      *ZEROS
     C                   Z-ADD     CMDOD         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDOD
     C                   ENDIF
     C*
     C*  Box 24b - Name & address of Nearest  Relative
     C*
     C                   MOVEL     NI80A5        RELN             40
     C                   MOVE      NI80A5        RELA             40
     C                   MOVE      NI80A6        RELCSZ           40
     C                   MOVEL     RELCSZ        RELC             25
     C                   MOVE      RELCSZ        RELSZ            15
     C                   MOVEL     RELSZ         RELS              2
     C                   MOVE      RELSZ         RELZ             13
     C     RELN          CAT(P)    RELA:2        XXNREL           70
     C                   CAT       RELC:1        XXNREL
     C                   CAT       ',':0         XXNREL
     C                   CAT       RELS:1        XXNREL
     C                   CAT       RELZ:1        XXNREL
     C*  NI80A5
     C*  NI80A6
     C*
     C*  Box 24c - Relationship
     C*
     C                   MOVEL     NI20A3        XXRELS           15
     C*  NI20A3
     C*
     C*  - Date of employer's knowledge/notice of injury
     C*
     C     CMDTRP        IFNE      *ZEROS
     C                   Z-ADD     CMDTRP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDTRP            6 0
     C                   ELSE
     C                   CLEAR                   XXDTRP
     C                   ENDIF
     C*
     C*  - Date of of This Report
     C*
     C*   UDATE
     C*
     C*  Box A  - Name of Employee preparing form/giving info to 3rd Party
     C*
     C*  NI60A7
     C     ','           SCAN      NIPRNM        Y                 3 0
     C     Y             IFNE      0
     C     Y             IFEQ      1
     C                   MOVEL     *BLANKS       XXPRNM           20
     C     19            SUBST     NIPRNM:2      XXPRTI           19
     C                   ELSE
     C     Y             SUB       1             END               3 0
     C     Y             ADD       1             STR               3 0
     C*
     C     END           SUBST     NIPRNM:1      XXPRNM
     C     19            SUBST     NIPRNM:STR    XXPRTI
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Box B  - Title and phone number
     C*
     C                   MOVE      NIPRPH        XXTPHN           10 0
     C*  NI20A4
     C*  NI20A5
     C*
     C*  Box C  - If Report prepare by 3rd Party, Co Name & Address
     C*
     C                   MOVEL     NI80A7        RELN
     C                   MOVE      NI80A7        RELA
     C                   MOVE      NI80A8        RELCSZ
     C                   MOVEL     RELCSZ        RELC
     C                   MOVE      RELCSZ        RELSZ
     C                   MOVEL     RELSZ         RELS
     C                   MOVE      RELSZ         RELZ
     C     RELN          CAT(P)    RELA:2        XX3PTY          110
     C                   CAT       RELC:1        XX3PTY
     C                   CAT       ',':0         XX3PTY
     C                   CAT       RELS:1        XX3PTY
     C                   CAT       RELZ:1        XX3PTY
     C*  NI80A7
     C*  NI80A8
     C*
     C*  Box D  - Third Party Contact Name  Telephone Number & Extn
     C*
     C     NI40A5        CAT(P)    NI30A5        XX3CNM           70
     C*  NI40A2
     C*  NI20A6
     C*
     C*================================================================
     C     *IN71         IFEQ      *ON
     C                   EXCEPT    NOI
     C                   ELSE
     C                   EXCEPT    NOI2
     C                   ENDIF
     C*
     C*  PRINT A LABEL FOR THE EMPLOYEE COPY ONLY.
     C*
     C     *IN72         IFEQ      *ON
     C*
     C                   MOVEL(P)  CDLSN         XXCNAM           50
     C     XXCNAM        CAT       ',':0         XXCNAM
     C     XXCNAM        CAT       CDFSN:1       XXCNAM
     C     XXCNAM        CAT(P)    CDMID:1       XXCNAM
     C*
     C                   MOVE      CDCTY         XXCTY            25
     C                   MOVE      CDST          XXSTAT            2
     C                   Z-ADD     CDZIP         XXZIP             9 0
     C                   EXSR      SRADDR
     C                   MOVE      *BLANKS       XXCCSZ           40
     C                   MOVEL     XXCSZ         XXCCSZ
     C     CDADR2        COMP      *BLANKS                                42
     C*
     C                   MOVE      *BLANKS       XXMC              2
     C     CMMCO         IFEQ      'Y'
     C                   MOVEL     'MC'          XXMC
     C                   ENDIF
     C*
     C                   EXCEPT    LABEL
     C                   ENDIF
     C*
     C*  CLOSE THE LABEL SPOOL FILE IF THE EMPLOYEE COPY WAS PROCESSED.
     C*
     C     *IN73         IFEQ      *ON
     C     XXOPN2        ANDEQ     *ON
     C                   CLOSE     QSYSPRT2
     C                   MOVE      *OFF          XXOPN2            1
     C                   ENDIF
     C*
     C*  WRITE THE DARTS REMARK, THEN PRINT THE NOI FORM.
     C*
     C     *IN75         IFEQ      *OFF
     C     *IN73         IFEQ      *OFF
     C                   EXSR      DARTS
     C                   ELSE
     C                   EXSR      SRDART
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDDO                                                  LEAVE
     C*
     C*  GET THE NEXT RECORD
     C*
     C                   SELECT
     C     *IN71         WHENEQ    *ON
     C     NIKY          READE     PNI01                                  99
     C     *IN72         WHENEQ    *ON
     C     NIKY          READE     PNI02                                  99
     C     *IN73         WHENEQ    *ON
     C     LGKY          READE     PLG01                                  99
     C     *IN75         WHENEQ    *ON
     C                   SETON                                        99
     C                   ENDSL
     C*
     C                   ENDDO
     C*
     C*  CLOSE THE LABEL SPOOL FILE IF THE EMPLOYEE COPY WAS PROCESSED.
     C*
     C   71XXOPEN        IFEQ      *ON
     C                   CLOSE     QSYSPRT
     C                   MOVE      *OFF          XXOPEN            1
     C                   ENDIF
     C   72XXOPN1        IFEQ      *ON
     C                   CLOSE     QSYSPRT1
     C                   MOVE      *OFF          XXOPN1            1
     C                   ENDIF
     C   72XXOPN2        IFEQ      *ON
     C                   CLOSE     QSYSPRT2
     C                   MOVE      *OFF          XXOPN2            1
     C                   ENDIF
     C   75XXOPN2        IFEQ      *ON
     C                   CLOSE     QSYSPRT2
     C                   MOVE      *OFF          XXOPN2
     C                   ENDIF
     C*
     C*  PROCESS THE NEXT COPY
     C*
     C     XXCOPY        IFEQ      1
     C                   ADD       1             XXCOPY
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C*  END OF PROGRAM
     C*
     C                   SETON                                        LR
     C*****************************************************************
     C* SRINIT SUBROUTINE              ** Subroutine **   S R I N I T *
     C* THIS SUBROUTINE INITIALIZES ALL WORK FIELDS                   *
     C*****************************************************************
     C     SRINIT        BEGSR
     C*
     C     *IN75         IFEQ      *OFF
     C     *IN73         IFEQ      *OFF
     C                   Z-ADD     NIFND         KKFND
     C                   Z-ADD     NIFYR         KKFYR
     C                   Z-ADD     NICASE        KKCASE
     C                   ELSE
     C                   Z-ADD     LGFND         KKFND
     C                   Z-ADD     LGFYR         KKFYR
     C                   Z-ADD     LGCASE        KKCASE
     C                   ENDIF
     C                   ENDIF
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
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
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
     C* FNDBRK SUBROUTINE              ** Subroutine **   F N D B R K *
     C* THIS SUBROUTINE GETS THE FUND INFORMATION.                    *
     C*****************************************************************
     C     FNDBRK        BEGSR
     C*
     C*   GET REMARK CODE FOR DIVISION DIARY REMARKS.
     C*
     C     FNKY          CHAIN     PCX01                              99
     C                   MOVE      *BLANKS       XXRMCD
     C     *IN99         IFEQ      *OFF
     C                   MOVE      CXRSRD        XXRMCD
     C                   ENDIF
     C*
     C*  GET THE FUND NAME AND ADDRESS
     C*
     C     FNKY          CHAIN     PFN01                              99
     C*
     C                   MOVEL(P)  FNNAME        XXFNME           40
     C*
     C                   CLEAR                   XXFADR
     C                   MOVEL(P)  FNMAD1        XXFADR           55
     C                   MOVE      FNMZIP        XXFMZP            9
     C     FNMAD2        IFNE      *BLANKS
     C     XXFADR        CAT       FNMAD2:2      XXFADR
     C                   ENDIF
     C                   CAT       FNMCTY:2      XXFADR
     C                   CAT       ',':0         XXFADR
     C                   CAT       FNMST:1       XXFADR
     C                   CAT       XXFMZP:2      XXFADR
     C*
     C*  GET THE CARRIER CODE NUMBER FROM FUND AUXILIARY FILE
     C*
     C     FNKY          CHAIN     PF201                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  F2SCTC        XXSCTC
     C                   ELSE
     C                   MOVE      *ZEROS        XXSCTC
     C                   ENDIF
     C*
     C                   ENDSR
     C*****************************************************************
     C* DARTS SUBROUTINE               ** Subroutine **   D A R T S   *
     C* THIS SUBROUTINE WRITES A DARTS REMARK FOR EACH NOI FORM       *
     C* PRINTED.                                                      *
     C*****************************************************************
     C     DARTS         BEGSR
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANKS       CMKEY
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      KKCO#         QCCO#
     C                   MOVE      KKFND         QCFND
     C                   MOVE      KKFYR         QCFYR
     C                   MOVE      KKCASE        QCCASE
     C*
     C*  DO NOT WRITE A DARTS RECORD IF THE REMARK CODE WAS NOT SETUP.
     C*
     C     XXRMCD        IFNE      *BLANKS
     C*
     C                   CLEAR                   PCM02
     C                   MOVEL     QCKEY         CMKEY
     C                   MOVE      *BLANKS       CMCOMM
     C   71              MOVEL     CPY(1)        CMCOMM
     C   72              MOVEL     CPY(2)        CMCOMM
     C                   MOVE      *BLANKS       CMCSID
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMCDAT
     C                   MOVE      *BLANKS       CMCODE
     C                   MOVE      *BLANKS       CMDCID
     C                   MOVEL     'W0070R'      CMPNAM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMTRDT
      *
     C                   Z-ADD     DSTIME        CMTRTM
     C                   MOVE      *ZEROS        CMSECL
     C                   MOVE      'N'           CMSBYL
     C                   MOVE      *BLANKS       CMSTAT
     C                   MOVEL     Q1USER        CMUSER
     C                   MOVEL     'W8070RFL'    CMPGM
     C                   Z-ADD     DSTIME        CMTIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
      *
     C                   MOVEL     Q1USER        CMIUSR
     C                   MOVE      *BLANKS       CMFOLD
     C                   MOVE      *ZEROS        CMDCDT
     C                   MOVE      *BLANKS       CMDIFL
     C                   MOVE      'WC'          CMAPID
     C                   MOVE      *BLANKS       CMIDOC
     C                   MOVE      *BLANKS       CMIQUE
     C                   MOVE      *BLANKS       CMIMGF
     C                   MOVE      *BLANKS       CMIPTH
     C                   MOVE      *ZEROS        CMPIDX
     C                   MOVE      *BLANKS       CMQPTY
     C                   MOVE      *BLANKS       CMDCTP
     C                   MOVEL     'W8070RFL'    CMIPGM
     C                   MOVE      *BLANKS       CMSMCD
     C                   MOVE      *BLANKS       CMFL01
     C                   MOVE      *BLANKS       CMFL02
     C                   MOVE      *BLANKS       CMFL03
     C                   MOVE      *BLANKS       CMFL04
     C                   MOVE      *BLANKS       CMFL05
     C                   MOVE      XXRMCD        CMRMCD
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMIDT
      ***** BEG ADD ***** 10/17/11 ***********************************
     C                   MOVE      *BLANKS       CMUNCN
     C                   MOVE      *BLANKS       CMIFSP
      ***** END ADD ***** 10/17/11 ***********************************
     C*
     C*  GET LAST SEQ# USED
     C*
     C     C3KY          SETGT     PCM03
     C     C3KY          READPE    PCM03                                  99
     C     *IN99         IFEQ      *OFF
     C     C3SEQ#        ADD       1             CMSEQ#
     C                   ELSE
     C                   Z-ADD     1             CMSEQ#
     C                   ENDIF
     C*
     C                   WRITE     PCM02
     C                   FEOD      SDCMLD
     C                   ENDIF
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
     C                   MOVE      '148'         KKTSET
     C*** BEG CHG ****** 03/24/10 *************************************
     C***        IXKY      CHAINPIX01                99
     C                   CALL      'WA076R'      IXAPI                  60
     C     @RTNCD        IFEQ      'Y'
     C                   MOVE      '0'           *IN99
     C                   ELSE
     C                   MOVE      '1'           *IN99
     C                   ENDIF
     C*** END CHG ****** 03/24/10 *************************************
     C     *IN99         IFEQ      *OFF
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      KKCO#         QCCO#
     C                   MOVE      KKFND         QCFND
     C                   MOVE      KKFYR         QCFYR
     C                   MOVE      KKCASE        QCCASE
     C*
     C*  ESTABLISH RECORD
     C*
     C                   CLEAR                   PCM02
     C                   MOVEL     QCKEY         CMKEY
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMCDAT
     C                   MOVEL     IXHCPR        CMRMCD
     C*
     C     CMRMCD        CHAIN     PCT01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVE      CTSMCD        CMSMCD
     C                   MOVEL     CPY(3)        CMCOMM
     C*
     C*  GET FOLDER NAME
     C*
     C*
     C                   MOVEL     KKFND         ##FND                          for Fund
     C                   MOVEL(P)  'C'           ##FOLD                         means "Member"
     C                   MOVE      CMCASE        ##FOLD
     C*
     C                   CALL      'SRFBLD'
     C                   PARM                    ##FND             8
     C                   PARM      'CS'          ##SER             8
     C                   PARM      CTTYPE        ##SUB             8
     C                   PARM                    ##FOLD            8
     C                   PARM      *BLANKS       ##FPTH           40            Built, returned
     C                   PARM      *BLANKS       ##TERM            1
     C*
     C                   MOVEL(P)  ##FPTH        CMFOLD
     C*
     C*  GET DOCUMENT ID
     C*
     C                   CALL      'SRDBLD'
     C                   PARM                    ##FPTH                         From SRFBLD routine
     C                   PARM      CTTYPE        ##RT              2            Rmrk Type for Doc ID
     C                   PARM      *BLANKS       ##DOC            12            Built, returned
     C                   PARM      *BLANKS       ##TERM
     C*
     C                   MOVEL(P)  ##DOC         CMDCID
      ***** BEG ADD ***** 10/17/11 ***********************************
      *
     C     CMFOLD        IFNE      ' '
     C     CMDCID        ORNE      ' '
     C                   MOVEL     '/'           CMUNCN
     C     CMUNCN        CAT       ROOT:0        CMUNCN
     C     CMUNCN        CAT       '/':0         CMUNCN
     C     CMUNCN        CAT       CMFOLD:0      CMUNCN
     C     CMUNCN        CAT       '/':0         CMUNCN
     C     CMUNCN        CAT       CMDCID:0      CMUNCN
     C                   MOVEL     ENV           CMIFSP
     C                   ELSE
     C                   MOVEL     *BLANKS       CMIFSP
     C                   MOVEL     *BLANKS       CMUNCN
     C                   ENDIF                                                  CMFOLD IFNE ' '
      ***** END ADD ***** 10/17/11 ***********************************
     C                   MOVEL(P)  'W0070R'      CMPNAM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMTRDT
     C*
     C                   Z-ADD     DSTIME        CMTRTM
     C                   MOVE      'N'           CMSBYL
     C                   MOVEL     LGIUSR        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
     C                   Z-ADD     DSTIME        CMTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C*
     C                   MOVEL     LGIUSR        CMIUSR
     C                   MOVE      'WC'          CMAPID
     C                   MOVEL     Q1PGM         CMIPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMIDT
     C*
     C*  GET LAST SEQ# USED
     C*
     C     C3KY          SETGT     PCM03
     C     C3KY          READPE    PCM03                                  99
     C     *IN99         IFEQ      *OFF
     C     C3SEQ#        ADD       1             CMSEQ#
     C                   ELSE
     C                   Z-ADD     1             CMSEQ#
     C                   ENDIF
     C*
     C                   WRITE     PCM02
     C                   FEOD      SDCMLD
     C*
     C*  LOAD THE DARTS SEQ# INTO THE EDI LOG FILE
     C*
     C     LGKY1         CHAIN     PLG02                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     CMSEQ#        LGPRSQ
     C                   UPDATE    PLG02
     C                   ENDIF
     C*
     C*  CALL W8070C1FL TO RESOLVE THE OUTPUT INTO AN OFFICE DOCUMENT
     C*
     C                   CALL      'W8070C1F'
     C                   PARM                    Q1JOB
     C                   PARM                    Q1USER
     C                   PARM                    Q1JOBN
     C                   PARM      CMDCID        ##DCID
     C                   PARM      CMFOLD        ##FLDR
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     OQSYSPRT   E            NOI              01
      *
      *  Claim number, Policy #, Date of Accident, Employee SS#
     O          E            NOI              17
     O                       XXACL#              22 ' & && & & && & & '
     O                       XXCCA#              40
     O                       F2SCTC              56 ' & & & & '
     O                       XXCAP#              75
     O                       XXACDT              92 ' & &&&& & &&& & '
     C***  Begin Chg  ***  06/09/20  *******************************
     O*                      XXSS#              115 ' & & &&& & &&& & & & '
     O                       XXSS#              115
     C***  End Chg  ***  06/09/20  *******************************
      *  Employer Info 1a-b c  Employer Name, Mailing addr, OSHA case#
     O          E            NOI              20
     O                       EMNAME              45
     O                       XXMADR              92
     O                       NI20A1             114
      *  Employer Info 1d-e-f-g Physical Addr, SIC Cde, NY UI #, FEIN
     O          E            NOI              22
     O                       XXPADR              40
     O                       XXSICD              75
     O                       XXNYUI              92 ' & & & & & & && '
     O                       EMFEIN             115 ' & &&& & && & && & & '
      *  Carrier Info 2a-b   Carrier Name & Addr.
     O          E            NOI              25
     O                       XXFNME              45
     O                       XXFADR             113
      *  Employee Info   3a-b  Name & Address
     O          E            NOI              27
     O                       XXCLNM              51
     O                       XXCADR             118
      *  Employee Info   4a-b-c  Accident Addr, County, On Emplr Site?
     O          E            NOI              30
     O                       XXAADR              60
     O                       XX30A3              88
     O               32                         105 'X'
     O              N32                         110 'X'
      *  Employee Info - 5, 6, 7, 8a-b AM/PM, Dept, Last work dte, paid
     O          E            NOI              32
     O                       XXTIM2              16 ' & && & '
     O              N28                          19 'X'
     O                       XXACTM              33 ' & && & '
     O              N27                          36 'X'
     O                       XX40A4              51
     O                       XXDTLW              93 ' & && & && & '
     O              N31                         105 'X'
     O               31                         110 'X'
     O*
      *  Employee Info - 5 Continued Time began Work, time of accident
     O          E            NOI         1
     O               28                          19 'X'
     O               27                          36 'X'
      *  Employee Info - 9, 10, 11, 12  Sex, DOB, Occupation, Hire Dte
     O          E            NOI              34
     O              N36                          11 'X'
     O          E            NOI         1
     O               36                          11 'X'
     O                       XXDTOB              34 ' && && && && && '
     O                       CMOCDS              78
     O                       XXHIDT             115 ' && && && && && '
      * Injury Info - 13a-b, 14a   Avg Earngs, 52 Wks Earngs, FT/PT,
     O          E            NOI         2
     O               37                          67 'X'
      * Injury Info - 14a-b Avg Earngs, 52 Wks Earngs, FT/PT, Work Wk
     O          E            NOI         1
     O                       XXAWW               20 ' &&& && && && '
     O                       XX5N1               48 ' && &&& && && '
     O               38                          67 'X'
     O                       XOWRWK             113
      * Injury Info - 15, 16a-b
     O          E            NOI              40
     O                       XXNPC               62
     O               29                          69 'X'
     O              N29                          75 'X'
     O                       XXMCDT             110 '  /  /    '
      * Injury Info - 17, 18  Employee treated in ER? Hospitalized Overnight?
     O          E            NOI         1
     O               34                          46 'X'
     O               35                         101 'X'
     O              N34                          52 'X'
     O              N35                         107 'X'
      * Injury Info - 19a-b  Physican and Hospital Info
     O          E            NOI              44
     O                       XXPHYN              50
     O                       XXHOSN             102
     O          E            NOI         1
     O                       XXTADR              60
     O                       XXHADR             112
      * Injury Info - 20a-b-c  Employee returned to work? Date? at what wage?
     O          E            NOI              49
     O               39                          18 'X'
     O              N39                          25 'X'
     O                       XXRTW               78 ' && &&& && &&& && '
     O                       XXPOWW             109 '  &&& && && '
      *
      *  21 - What employee was doing when injured ?
     O          E            NOI              54
     O                       NI60A5              70
     O          E            NOI         1
     O                       NI60A6              70
      *  22 - How did accident or exposure occur ?
     O          E            NOI              60
     O                       NI60A3              70
     O          E            NOI         1
     O                       NI60A4              70
      *  23 - Object or substance that directly injured employee
     O          E            NOI              66
     O                       XXOBJ              110
      *  Employee info - 24a-b-c
     O          E            NOI              69
     O                       XXDOD               25 ' && && && && && '
     O                       XXNREL              98
     O                       XXRELS             112
      *  Date Employer/Supvr first knew of injury, Report Date
     O          E            NOI              71
     O                       XXDTRP              34 ' && && && && && '
     O                       UDATE               61 ' && && && && && '
     O*  Employee preparing form, Title and Phone number
     O          E            NOI              74
     O                       XXPRNM              30
     O                       XXPRTI              85
     O                       XXTPHN             105 '0(   )&   -    '
     O                       NI10A2             117
     O*  If report prepared by 3rd party, 3rd party name & address
     O          E            NOI              76
     O                       XX3PTY             120
     O*  Third party contact name and phone number
     O          E            NOI              79
     O                       XX3CNM              79
     O*
     OQSYSPRT1  E            LABEL          1 11
     O                       XXCNAM              50
     O          E            LABEL          1
     O                       CDADR1              45
     O          E      N42   LABEL          1
     O                       CDADR2              45
     O          E            LABEL          1
     O                       XXCCSZ              45
     O                       XXMC                 2
     O*
     OQSYSPRT2  E            NOI2              1
      *
      *  Claim number, Policy #, Date of Accident, Employee SS#
     O          E            NOI2             17
     O                       XXACL#              22 ' & && & & && & & '
     O                       XXCCA#              40
     O                       F2SCTC              56 ' & & & & '
     O                       XXCAP#              75
     O                       XXACDT              92 ' & &&&& & &&& & '
     C***  Begin Chg  ***  06/09/20  *******************************
     O*                      XXSS#              115 ' & & &&& & &&& & & & '
     O                       XXSS#              115
     C***  End Chg  ***  06/09/20  *******************************
      *  Employer Info 1a-b c  Employer Name, Mailing addr, OSHA case#
     O          E            NOI2             20
     O                       EMNAME              45
     O                       XXMADR              92
     O                       NI20A1             114
      *  Employer Info 1d-e-f-g Physical Addr, SIC Cde, NY UI #, FEIN
     O          E            NOI2             22
     O                       XXPADR              40
     O                       XXSICD              75
     O                       XXNYUI              92 ' & & & & & & && '
     O                       EMFEIN             115 ' & &&& & && & && & & '
      *  Carrier Info 2a-b   Carrier Name & Addr.
     O          E            NOI2             25
     O                       XXFNME              45
     O                       XXFADR             113
      *  Employee Info   3a-b  Name & Address
     O          E            NOI2             27
     O                       XXCLNM              51
     O                       XXCADR             118
      *  Employee Info   4a-b-c  Accident Addr, County, On Emplr Site?
     O          E            NOI2             30
     O                       XXAADR              60
     O                       XX30A3              88
     O               32                         105 'X'
     O              N32                         110 'X'
      *  Employee Info - 5, 6, 7, 8a-b AM/PM, Dept, Last work dte, paid
     O          E            NOI2             32
     O                       XXTIM2              16 ' & && & '
     O              N28                          19 'X'
     O                       XXACTM              33 ' & && & '
     O              N27                          36 'X'
     O                       XX40A4              51
     O                       XXDTLW              93 ' & && & && & '
     O              N31                         105 'X'
     O               31                         110 'X'
     O*
      *  Employee Info - 5 Continued Time began Work, time of accident
     O          E            NOI2        1
     O               28                          19 'X'
     O               27                          36 'X'
      *  Employee Info - 9, 10, 11, 12  Sex, DOB, Occupation, Hire Dte
     O          E            NOI2             34
     O              N36                          11 'X'
     O          E            NOI2        1
     O               36                          11 'X'
     O                       XXDTOB              34 ' && && && && && '
     O                       CMOCDS              78
     O                       XXHIDT             115 ' && && && && && '
      * Injury Info - 13a-b, 14a   Avg Earngs, 52 Wks Earngs, FT/PT,
     O          E            NOI2        2
     O               37                          67 'X'
      * Injury Info - 14a-b Avg Earngs, 52 Wks Earngs, FT/PT, Work Wk
     O          E            NOI2        1
     O                       XXAWW               20 ' &&& && && && '
     O                       XX5N1               48 ' && &&& && && '
     O               38                          67 'X'
     O                       XOWRWK             113
      * Injury Info - 15, 16a-b
     O          E            NOI2             40
     O                       XXNPC               62
     O               29                          69 'X'
     O              N29                          75 'X'
     O                       XXMCDT             110 '  /  /    '
      * Injury Info - 17, 18  Employee treated in ER? Hospitalized Overnight?
     O          E            NOI2        1
     O               34                          46 'X'
     O               35                         101 'X'
     O              N34                          52 'X'
     O              N35                         107 'X'
      * Injury Info - 19a-b  Physican and Hospital Info
     O          E            NOI2             44
     O                       XXPHYN              50
     O                       XXHOSN             102
     O          E            NOI2        1
     O                       XXTADR              60
     O                       XXHADR             112
      * Injury Info - 20a-b-c  Employee returned to work? Date? at what wage?
     O          E            NOI2             49
     O               39                          18 'X'
     O              N39                          25 'X'
     O                       XXRTW               78 ' && &&& && &&& && '
     O                       XXPOWW             109 '  &&& && && '
      *
      *  21 - What employee was doing when injured ?
     O          E            NOI2             54
     O                       NI60A5              70
     O          E            NOI2        1
     O                       NI60A6              70
      *  22 - How did accident or exposure occur ?
     O          E            NOI2             60
     O                       NI60A3              70
     O          E            NOI2        1
     O                       NI60A4              70
      *  23 - Object or substance that directly injured employee
     O          E            NOI2             66
     O                       XXOBJ              110
      *  Employee info - 24a-b-c
     O          E            NOI2             69
     O                       XXDOD               25 ' && && && && && '
     O                       XXNREL              98
     O                       XXRELS             112
      *  Date Employer/Supvr first knew of injury, Report Date
     O          E            NOI2             71
     O                       XXDTRP              34 ' && && && && && '
     O                       UDATE               61 ' && && && && && '
     O*  Employee preparing form, Title and Phone number
     O          E            NOI2             74
     O                       XXPRNM              30
     O                       XXPRTI              85
     O                       XXTPHN             105 '0(   )&   -    '
     O                       NI10A2             117
     O*  If report prepared by 3rd party, 3rd party name & address
     O          E            NOI2             76
     O                       XX3PTY             120
     O*  Third party contact name and phone number
     O          E            NOI2             79
     O                       XX3CNM              79
     O*
**  COPY MSG
EMPLOYER COPY
EMPLOYEE COPY
DIVISION COPY
