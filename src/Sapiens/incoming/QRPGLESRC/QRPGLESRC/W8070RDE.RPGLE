     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE W8070RDE - Delaware NOI Print Program
     H*****************************************************************
     H* Program Description:  This program prints Delaware NOI        *
     H*                       forms.                                  *
     H*                                                               *
     H* Called from:  W8070CDE                                        *
     H* Calls to:  None                                               *
     H*                                                               *
     H*****************************************************************
     H* Created 07/16/2008 by c:62 for Project 114DRB                 *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*   06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H*  06/22/15 C:300 Project :114WQR                               *
     H*                 2014 Delaware First Report of Injury          *
     H*                 Use new external printer files                *
     H*                                                               *
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
     FWMCDL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMTPL1    IF   E           K DISK
     FWMPDL     IF   E           K DISK
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
     FWTABL     IF   E           K DISK
     F*                                              KINFDS DBFDS3
      ***** beg add ***** 06/22/15 ***********************************
     FWDF2L     IF   E           K DISK
      ***** beg end ***** 06/22/15 ***********************************
      ***** beg chg ***** 06/22/15 ***********************************
     F*******QSYSPRT   O    F  127        PRINTER OFLIND(*INOA)
     FW8070PDE  O    E             PRINTER
     F                                     USROPN
      ***** end chg ***** 06/22/15 ***********************************
     FQSYSPRT1  O    F   85        PRINTER OFLIND(*INOF)
     F                                     USROPN
      ***** beg chg ***** 06/22/15 ***********************************
     F*******QSYSPRT2  O    F  127        PRINTER OFLIND(*INOG)
     FW8070PDE2 O    E             PRINTER
     F                                     USROPN
      ***** end chg ***** 06/22/15 ***********************************

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
     D TYPE1L          C                   CONST('Private')
     D TYPE2L          C                   CONST('State')
     D TYPE3L          C                   CONST('County')
     D TYPE4L          C                   CONST('City')
     D TYPE5L          C                   CONST('School District')
     D TYPE1U          C                   CONST('PRIVATE')
     D TYPE2U          C                   CONST('STATE')
     D TYPE3U          C                   CONST('COUNTY')
     D TYPE4U          C                   CONST('CITY')
     D TYPE5U          C                   CONST('SCHOOL DISTRICT')
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
     D                 DS
     D  XXCLM#                 1     13
     D  CMFND                  1      3  0
     D  CMFYR                  4      6  0
     D  CMCASE                 7     13  0
     D                 DS
     D  XXEMP#                 1     17
     D  EMFND                  1      3  0
     D  EMEMP#                 4     12  0
     D  EMDIV                 13     17  0
     D                 DS
     D  XXPOL#                 1     20
     D  ELFND                  1      3  0
     D  ELEMP#                 4     12  0
     D  ELDIV                 13     17  0
     D  ELFYR                 18     20  0
      ***** beg add ***** 06/22/15 ***********************************
      * Date of Birth
     D                 DS
     D  XXDTOB                 1      8  0
     D  XXDTOBM                1      2  0
     D  XXDTOBD                3      4  0
     D  XXDTOBY                5      8  0
      * Date of Report
     D                 DS
     D  XXPREP                 1      8  0
     D  XXPREPM                1      2  0
     D  XXPREPD                3      4  0
     D  XXPREPY                5      8  0
      * Date of Injury
     D                 DS
     D  XXACDT                 1      8  0
     D  XXACDTM                1      2  0
     D  XXACDTD                3      4  0
     D  XXACDTY                5      8  0
      * If Employee Back to Work give date
     D                 DS
     D  XXRTW                  1      8  0
     D  XXRTWM                 1      2  0
     D  XXRTWD                 3      4  0
     D  XXRTWY                 5      8  0
      * Date of Death
     D                 DS
     D  XXDOD                  1      8  0
     D  XXDODM                 1      2  0
     D  XXDODD                 3      4  0
     D  XXDODY                 5      8  0
      * Date Employer Knew
     D                 DS
     D  XXDTRP                 1      8  0
     D  XXDTRPM                1      2  0
     D  XXDTRPD                3      4  0
     D  XXDTRPY                5      8  0
      * Date Disability Began
     D                 DS
     D  XXDBDT                 1      8  0
     D  XXDBDTM                1      2  0
     D  XXDBDTD                3      4  0
     D  XXDBDTY                5      8  0
      * Last Full Day Paid Date
     D                 DS
     D  XXDT2                  1      8  0
     D  XXDT2M                 1      2  0
     D  XXDT2D                 3      4  0
     D  XXDT2Y                 5      8  0
      ***** end add ***** 06/22/15 ***********************************

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
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     NIKY4         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    CMJRST
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
     C*  KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
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
     C*  KEY LIST FOR THE PAYEE FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
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
     C     *LIKE         DEFINE    PAKEY         KKKEY
     C*
     C     *LIKE         DEFINE    NIFND         SVFND
     C     *LIKE         DEFINE    LGTSET        SVTSET
     C*
     C     *LIKE         DEFINE    CMDCID        ##DCID
     C     *LIKE         DEFINE    CMFOLD        ##FLDR
     C*
     C     *LIKE         DEFINE    CMRMCD        XXRMCD
     C     *LIKE         DEFINE    PDFEIN        KKFEIN
     C     *LIKE         DEFINE    PDMOD         KKMOD
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
      ***** beg chg ***** 06/22/15 ***********************************
     C*******            OPEN      QSYSPRT
     C                   OPEN      W8070PDE
      ***** end chg ***** 06/22/15 ***********************************
     C                   MOVE      *ON           XXOPEN
     C                   ENDIF
     C     XXCOPY        IFEQ      2
     C                   OPEN      QSYSPRT1
     C                   MOVE      *ON           XXOPN1
      ***** beg chg ***** 06/22/15 ***********************************
     C*******            OPEN      QSYSPRT2
     C                   OPEN      W8070PDE2
      ***** end chg ***** 06/22/15 ***********************************
     C                   MOVE      *ON           XXOPN2
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN75         IFEQ      *ON
      ***** beg chg ***** 06/22/15 ***********************************
     C*******            OPEN      QSYSPRT2
     C                   OPEN      W8070PDE2
      ***** end chg ***** 06/22/15 ***********************************
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
      ***** beg chg ***** 06/22/15 ***********************************
     C*******            OPEN      QSYSPRT2
     C                   OPEN      W8070PDE2
      ***** end chg ***** 06/22/15 ***********************************
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
     C     NIKY4         CHAIN     PNI04                              99
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
     C* GET EMPLOYER DETAIL INFO
     C*
     C     ELKY          CHAIN     PEL01                              99
     C     *IN99         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C*
     C                   MOVEA     '00000'       *IN(21)
     C                   MOVEA     '00000'       *IN(26)
     C                   MOVEA     '00000'       *IN(31)
     C                   MOVEA     '000000'      *IN(36)
     C*================================================================
     C* Top of Page Info
     C*================================================================
     C*
     C*  Case or File No.
     C*
     C*                   XXCLM#
     C*
     C*  Employer's UC Reporting Number
     C*
     C*                   XXEMP#
     C*================================================================
     C* Employee
     C*================================================================
     C*
     C*  Box 1 - Employee Name
     C*
     C*                    CDFSN/CDMID/CDLSN
     C*
     C*  Box 2 - Employee Social Security No.
     C*
     C*                    CDSS#/CDMID/CDLSN
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CDSS#'       QQNAME           25
     C     CDSS#         PARM      CDSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
      ***** beg add ***** 06/22/15 ***********************************
     C                   MOVE      *BLANKS       XXSS#            11
     C                   EVAL      XXSS# = %SUBST(CDSS#:1:3)+'-'+
     C                             %SUBST(CDSS#:4:3)+'-'+%SUBST(CDSS#:7:3)
      ***** end add ***** 06/22/15 ***********************************
     C*
     C*  Box 3 - Employee Address (Include County and Zip Code)
     C*
     C*                    CDADR1/CDADR2/CDCTY/CDST/CDZIP/NI30A4
     C*
     C     CDADR1        CAT(P)    CDADR2:1      XXCADR           60
     C*
     C                   MOVE      CDCTY         XXCTY            25
     C                   MOVE      CDST          XXSTAT            2
     C                   Z-ADD     CDZIP         XXZIP             9 0
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         XXCCS2           60
     C                   CAT       NI30A4:1      XXCCS2
     C*
     C*  Box 4 - Gender
     C*
     C*                    CDSEX
     C*
      ***** beg add ***** 06/22/15 ***********************************
     C                   EVAL      XXMBOX = 'X'
     C                   EVAL      XXFBOX = 'X'
      ***** end add ***** 06/22/15 ***********************************
     C                   SETOFF                                       2021
     C                   SELECT
     C     CDSEX         WHENEQ    'M'
     C                   SETON                                        20
     C     CDSEX         WHENEQ    'F'
     C                   SETON                                        21
     C                   ENDSL
     C*
     C*  Box 5 - Employee Telephone Number (Including Area Code)
     C*
     C*                    CDAREA/CDPHN#
     C*
     C*  Box 6 - Date of Birth
     C*
     C*                    CDDTOB
     C*
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDTOB            8 0
     C*
     C*  Box 7 - Age
     C*
     C*                    NI10A1
     C*
     C     CDDTOB        IFEQ      *ZEROS
     C                   MOVEL     NI10A1        XX10A1            3 0
     C                   ELSE
     C     CMACDT        SUB       CDDTOB        XXAGE             7 0
     C                   MOVEL     XXAGE         XX10A1
     C                   ENDIF
     C*
     C*  Box 8 - Wage
     C*
     C*                    CMAWW
     C*
     C*  Box 9 - Weekly Hours Worked
     C*
     C*                    CMHRSD*CMDAYS
     C*
     C     CMHRSD        MULT      CMDAYS        XXHRSW            3 0
     C*
     C*  Box 10 - Occupation (Regular)
     C*
     C*                    CMOCDS
     C*
     C*  Box 11 - Department or Division Regularly Employed
     C*
     C*                    NI40A4
     C*
     C                   MOVEL     NI40A4        XX40A4           30
     C*
     C*  Box 12 - How long employed?
     C*
     C*                    NI20A3
     C*================================================================
     C* Employer
     C*================================================================
     C*
     C*  Box 13 - Employer Name
     C*
     C*                    EMNAME
     C*
     C*
     C*  Box 14 - Person Making out the Report
     C*
     C*                    NIPRNM
     C*
     C     NIPRNM        IFNE      *BLANKS
     C                   Z-ADD     1             P1                2 0
     C     ','           SCAN      NIPRNM        P2                2 0
     C     P2            SUB       P1            LENGTH            2 0
     C     LENGTH        IFGT      0
     C     LENGTH        SUBST     NIPRNM:P1     XXPRNM           29
     C                   ENDIF
     C*
     C     P2            ADD       1             P1
     C     10            SUBST     NIPRNM:P1     XXPOS            10
     C                   ELSE
     C                   CLEAR                   XXPRNM
     C                   CLEAR                   XXPOS
     C                   ENDIF
     C*
     C*  Box 15 - Employer Physical Addess
     C*
     C*                    EMPAD1/EMPAD2/EMPCTY/EMPST/EMPZIP/EMCNTY
     C*
     C     EMPST         IFNE      *BLANKS
     C     EMPST         CAT(P)    EMCNTY:0      KKKEY
     C                   ELSE
     C     EMMST         CAT(P)    EMCNTY:0      KKKEY
     C                   ENDIF                                                  SCMST<>*BLANKS
      *
     C     TBKY          CHAIN     PCN01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CNDESC        XXECNT           20
     C                   ELSE
     C                   MOVEL     *BLANKS       XXECNT
     C                   ENDIF
     C*
     C     EMPAD1        IFEQ      *BLANKS
     C                   SETON                                        23
     C     EMMAD1        CAT(P)    EMMAD2:1      XXEMAD           50
     C                   MOVE      EMMCTY        XXCTY            25
     C                   MOVE      EMMST         XXSTAT            2
     C                   Z-ADD     EMMZIP        XXZIP             9 0
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         XXEMCS           50
     C                   ELSE
     C*
     C     EMPAD1        CAT(P)    EMPAD2:1      XXEPAD           60
     C                   MOVE      EMPCTY        XXCTY            25
     C                   MOVE      EMPST         XXSTAT            2
     C                   Z-ADD     EMPZIP        XXZIP             9 0
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         XXEPCS           60
     C                   ENDIF
     C*
     C*  Box 16 - Employer Phone Number
     C*
     C*                    EMAREA/EMPHN#
     C*
     C*
     C*  Box 17 - Employer Mailing Adress If Different
     C*
     C*                    EMMAD1/EMMAD2/EMMCTY/EMMST/EMMZIP
     C*
     C                   SETOFF                                       23
     C     EMPAD1        IFNE      *BLANKS
     C     EMMAD1        IFNE      EMPAD1
     C     EMMAD2        ORNE      EMPAD2
     C                   SETON                                        23
     C     EMMAD1        CAT(P)    EMMAD2:1      XXEMAD           50
     C*
     C                   MOVE      EMMCTY        XXCTY            25
     C                   MOVE      EMMST         XXSTAT            2
     C                   Z-ADD     EMMZIP        XXZIP             9 0
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         XXEMCS           50
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C*  Box 18 - Nature of Business
     C*
     C*                    EMNAOB
     C*
     C*================================================================
     C* Dates
     C*================================================================
     C*
     C*  Box 19 - Date of Report
     C*
     C*                    NIPREP
     C*
     C                   Z-ADD     NIPREP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPREP            8 0
     C*
     C*
     C*  Box 20 - Date of Injury and Time
     C*
     C*                    CMACDT/CMACTM
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
     C*
     C                   MOVEL     CMACTM        XXACTM            4 0
     C*
     C                   SETOFF                                       2425
     C     XXACTM        IFGT      1200
     C                   SETON                                        25
     C                   SUB       1200          XXACTM
     C                   ELSE
     C                   SETON                                        24
     C                   ENDIF
     C*
     C*
     C*  Box 21 - Normal Starting Time
     C*
     C*                    NITIM2
     C*
     C                   MOVEL     NITIM2        XXTIM2            4 0
      ***** beg add ***** 06/22/15 ***********************************
     C                   EVAL      XXTIMA = 'x'
     C                   EVAL      XXTIMP = 'x'
     C*
     C                   SETOFF                                       2829
     C                   IF        XXTIM2 <> 0
     C     XXTIM2        IFGT      1200
     C                   SETON                                        29
     C                   SUB       1200          XXTIM2
     C                   ELSE
     C                   SETON                                        28
     C                   ENDIF
     C                   ENDIF
      ***** end add ***** 06/22/15 ***********************************
     C*
     C*
     C*  Box 22 - If Employee Back to Work give date
     C*
     C*                    CMRTW
     C*
     C                   Z-ADD     CMRTW         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXRTW             8 0
     C*
     C*
     C*  Box 23 - At same wage?
     C*
     C*                    NI1A2
     C*
      ***** beg add ***** 06/22/15 ***********************************
     C                   EVAL      XX1A2Y = 'x'
     C                   EVAL      XX1A2N = 'x'
      ***** end add ***** 06/22/15 ***********************************
     C                   SETOFF                                       2627
     C                   SELECT
     C     NI1A2         WHENEQ    'Y'
     C                   SETON                                        26
     C     NI1A2         WHENEQ    'N'
     C                   SETON                                        27
     C                   ENDSL
     C*
     C*  Box 24 - Date of Death
     C*
     C*                    CMDOD
     C*
     C                   Z-ADD     CMDOD         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDOD             8 0
     C*
     C*
     C*  Box 25 - Date Employer Knew
     C*
     C*                    CMDTRP
     C*
     C                   Z-ADD     CMDTRP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDTRP            8 0
     C*
     C*
     C*  Box 26 - Date Disability Began
     C*
     C*                    CMDBDT
     C*
     C                   Z-ADD     CMDBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDBDT            8 0
     C*
     C*
     C*  Box 27 - Last Full Day Paid Date
     C*
     C*                    NIDT2
     C*
     C                   Z-ADD     NIDT2         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT2             8 0
     C*
     C*================================================================
     C* Injury or Disease
     C*================================================================
     C*
     C*  Box 28 - Describe the Injury
     C*
     C*                    CMNATR/CMPART
     C                   MOVEL(P)  CMNATR        KKKEY
     C     TBKY          CHAIN     PNA01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  NADESC        XXNPC           100
     C                   ELSE
     C                   CLEAR                   XXNPC
     C                   ENDIF
     C*
     C                   MOVEL(P)  CMPART        KKKEY
     C     TBKY          CHAIN     PPA01                              99
     C     *IN99         IFEQ      *OFF
     C     XXNPC         IFEQ      *BLANKS
     C                   MOVEL(P)  PADESC        XXNPC           100
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
     C                   MOVEL(P)  CADESC        XXNPC           100
     C                   ELSE
     C                   CAT       ';':0         XXNPC
     C                   CAT       CADESC:1      XXNPC
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C*  Box 29 - Department where Incident Occurred and Process
     C*
     C*                    NI40A8/NI60A5/NI60A6
     C*
     C     NI60A5        CAT(P)    NI60A6:1      XX60A5          100
     C*
     C*================================================================
     C* Occurrence
     C*================================================================
     C*
     C*  Box 30 - List the Equipment/Material and Chemicals
     C*
     C*                    NI60A1/NI60A2
     C*
     C*
     C*  Box 31 - Describe the Employee's Activity
     C*
     C*                    NI60A3/NI60A4
     C*
     C*
     C*  Box 32 - How the injury occurred
     C*
     C*                    CMACDS/CMACD2
     C*
     C*
     C*  Box 33 - Name of Physician
     C*
     C*                    TPPHYN
     C*
     C*
     C*  Box 34 - Physician's Address
     C*
     C*                    TPPHYA/TPPHYC/TPPHYS/TPPHYZ
     C*
     C     TPKY          CHAIN     PTP01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVE      TPPHYC        XXCTY            25
     C                   MOVE      TPPHYS        XXSTAT            2
     C                   Z-ADD     TPPHYZ        XXZIP             9 0
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         XXPCSZ           60
     C                   ELSE
     C*
     C                   CLEAR                   XXPCSZ
     C*
     C                   ENDIF
     C*
     C*
     C*  Box 35 - Hospital Name
     C*
     C*                    CMHOSN
     C*
     C*
     C*  Box 36 - Hospital Address
     C*
     C*                    CMHOSA/CMHOSC/CMHOSS/CMHOSZ
     C*
B01  C     CMHOSF        IFNE      *ZEROS
     C                   Z-ADD     CMHOSF        KKFEIN
     C                   Z-ADD     CMHOSM        KKMOD
     C     PDKY          CHAIN     PPD01                              99
B02  C     *IN99         IFEQ      *OFF
     C                   Z-ADD     PDZIP         CMHOSZ
     C                   MOVEL(P)  PDCTY         CMHOSC
     C                   MOVEL(P)  PDST          CMHOSS
B03  C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR1        CMHOSN
     C                   MOVEL(P)  PDADR2        CMHOSA
S03  C                   ELSE
B04  C     PDFSN         IFNE      *BLANKS
     C     PDFSN         CAT(P)    PDNAME:1      CMHOSN
S04  C                   ELSE
     C                   MOVEL(P)  PDNAME        CMHOSN
E04  C                   ENDIF
     C                   MOVEL(P)  PDADR1        CMHOSA
E03  C                   ENDIF
E02  C                   ENDIF
E01  C                   ENDIF
     C*
     C                   MOVE      CMHOSC        XXCTY            25
     C                   MOVE      CMHOSS        XXSTAT            2
     C                   Z-ADD     CMHOSZ        XXZIP             9 0
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         XXHCSZ           60
     C*
     C*================================================================
     C* Bottom
     C*================================================================
     C*
     C*  Box 37 - Carrier Name and Address
     C*
     C*                    FNNAME/FNPAD1/FNPAD2/FNPCTY/FNPST/FNPZIP
     C*
     C     FNMAD1        IFNE      *BLANKS
     C     FNMAD1        CAT       FNMAD2:2      XXFADR           60
     C*
     C                   MOVE      FNMCTY        XXCTY            25
     C                   MOVE      FNMST         XXSTAT            2
     C                   Z-ADD     FNMZIP        XXZIP             9 0
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         XXFCSZ           60
     C                   ELSE
     C     FNPAD1        CAT       FNPAD2:2      XXFADR
     C                   MOVE      FNPCTY        XXCTY
     C                   MOVE      FNPST         XXSTAT
     C                   Z-ADD     FNPZIP        XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         XXFCSZ
     C                   ENDIF
     C*
     C*  Policy Number
     C*
     C*                    XXPOL#
     C*
      ***** beg add ***** 06/22/15 ***********************************
     C*  New Box 20 - Phone# (including Area Code)
     C*
     C*                    FNPH1A/FNPH1
     C*
     C*  New Box 23, 24 - TPA (If Applicable) and Address
     C*
     C*  If Group serviced by TPA is 'Y', then pass
     C*     Alternate Name and Address in Box 23, 24 and
     C*     pass blanks in Box 19-22
     C*                    FNANAM/FNAAD1/FNAAD2/FNACTY/FNAST/FNAZIP
     C*
     C     FNKY          CHAIN     PF201                              60
     C                   IF        *IN60 = '0' AND
     C                             F2TPA = 'Y'
     C                   MOVE      FNANAM        XXTNAM           50
     C*
     C     FNAAD1        CAT       FNAAD2:2      XXTADR           60
     C*
     C                   MOVE      FNACTY        XXCTY            25
     C                   MOVE      FNAST         XXSTAT            2
     C                   Z-ADD     FNAZIP        XXZIP             9 0
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         XXTCSZ           60
     C*
     C*     Since TPA is 'Y' pass blanks in Box 19-22
     C                   MOVE      *BLANKS       FNNAME
     C                   MOVE      *BLANKS       FNPH1A
     C                   MOVE      *BLANKS       FNPH1
     C                   MOVE      *BLANKS       XXFADR
     C                   MOVE      *BLANKS       XXFCSZ
     C                   MOVE      *BLANKS       XXPOL#
     C                   ENDIF
      ***** end add ***** 06/22/15 ***********************************
     C*
     C*
     C*  Policy Dates
     C*
     C*                    ELPFDT/ELPTDT
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPFDT            8 0
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPTDT            8 0
     C*
     C*================================================================
     C     *IN71         IFEQ      *ON
      ***** beg chg ***** 06/22/15 ***********************************
     C*******            EXCEPT    NOI
     C*******            EXCEPT    BLANK
     C                   WRITE     NOI
      ***** end chg ***** 06/22/15 ***********************************
     C                   ELSE
      ***** beg chg ***** 06/22/15 ***********************************
     C*******            EXCEPT    NOI2
     C*******            EXCEPT    BLANK2
     C                   WRITE     NOI2
      ***** end chg ***** 06/22/15 ***********************************
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
      ***** beg chg ***** 06/22/15 ***********************************
     C*******            CLOSE     QSYSPRT2
     C                   CLOSE     W8070PDE2
      ***** end chg ***** 06/22/15 ***********************************
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
      ***** beg chg ***** 06/22/15 ***********************************
     C*******            CLOSE     QSYSPRT
     C                   CLOSE     W8070PDE
      ***** end chg ***** 06/22/15 ***********************************
     C                   MOVE      *OFF          XXOPEN            1
     C                   ENDIF
     C   72XXOPN1        IFEQ      *ON
     C                   CLOSE     QSYSPRT1
     C                   MOVE      *OFF          XXOPN1            1
     C                   ENDIF
     C   72XXOPN2        IFEQ      *ON
      ***** beg chg ***** 06/22/15 ***********************************
     C*******            CLOSE     QSYSPRT2
     C                   CLOSE     W8070PDE2
      ***** end chg ***** 06/22/15 ***********************************
     C                   MOVE      *OFF          XXOPN2            1
     C                   ENDIF
     C   75XXOPN2        IFEQ      *ON
      ***** beg chg ***** 06/22/15 ***********************************
     C*******            CLOSE     QSYSPRT2
     C                   CLOSE     W8070PDE2
      ***** end chg ***** 06/22/15 ***********************************
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
     C                   MOVEL     'W8070RDE'    CMIPGM
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
     C                   MOVE      *BLANKS       CMUNCN
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
      ***** beg del ***** 06/22/15 ***********************************
     O*******QSYSPRT   E            NOI              01
     O*
     O* TOP SECTION
     O*
     O*******   E            NOI              11
     O*******                XXCLM#             117
     O*******   E            NOI              19
     O*******                XXEMP#             117
     O*
     O* EMPLOYEE SECTION
     O*
     O*******   E            NOI              25
     O*******                CDFSN               41
     O*******                CDMID               49
     O*******                CDLSN               94
     O*******                CDSS#              108
     O*******   E            NOI              27
     O*******        20                          79 'X'
     O*******   E            NOI              28
     O*******                XXCADR              74
     O*******   E            NOI              29
     O*******                XXCCS2              74
     O*******        21                          79 'X'
     O*******                                    90 '('
     O*******                CDAREA        4   +000
     O*******                                  +000 ')'
     O*******                CDPHN#            +001 '   -    '
     O*******   E            NOI              33
     O*******                XXDTOB              27 '  /  /    '
     O*******                XX10A1        4     39
     O*******                CMAWW         4     60
     O*******                XXHRSW        4    100
     O*******   E            NOI              37
     O*******                CMOCDS              54
     O*******                XX40A4              85
     O*******                NI20A3             110
     O*
     O* EMPLOYER SECTION
     O*
     O*******   E            NOI              41
     O*******                EMNAME              55
     O*******                XXPRNM              95
     O*
     O*******   E            NOI              43
     O*******                XXECNT              64
     O*
     O*******   E            NOI              44
     O*******                XXEPAD              76
     O*******                                    81 '('
     O*******                EMAREA        4   +000
     O*******                                  +000 ')'
     O*******                EMPHN#            +001 '   -    '
     O*
     O*******   E            NOI              45
     O*******                XXEPCS              76
     O*
     O*******   E            NOI              48
     O*******        23      XXEMAD              66
     O*******                EMNAOB             110
     O*
     O*******   E            NOI              49
     O*******        23      XXEMCS              66
     O*
     O* DATES SECTION
     O*
     O*******   E            NOI              51
     O*******                XXACDT              41 '  /  /    '
     O*
     O*******   E            NOI              52
     O*******                XXPREP              27 '  /  /    '
     O*******                XXACTM              41 '  :  '
     O*******        24                          43 'x'
     O*******        25                          47 'x'
     O*******                XXTIM2              66 '  :  '
     O*******                XXRTW               89 '  /  /    '
     O*******        26                          94 'x'
     O*******        27                         103 'x'
     O*
     O*******   E            NOI              56
     O*******                XXDOD               32 '  /  /    '
     O*******                XXDTRP              55 '  /  /    '
     O*******                XXDBDT              75 '  /  /    '
     O*******                XXDT2              100 '  /  /    '
     O*
     O* INJURY OR DISEASE SECTION
     O*
     O*******   E            NOI              60
     O*******                XXNPC              114
     O*******   E            NOI              63
     O*******                NI40A8              54
     O*******   E            NOI              64
     O*******                XX60A5             114
     O*
     O* OCCURRENCE SECTION
     O*
     O*******   E            NOI              67
     O*******                NI60A1              74
     O*******   E            NOI              68
     O*******                NI60A2              74
     O*******   E            NOI              72
     O*******                NI60A3              74
     O*******   E            NOI              73
     O*******                NI60A4              74
     O*******   E            NOI              78
     O*******                CMACDS              74
     O*******   E            NOI              79
     O*******                CMACD2              74
     O*******   E            NOI              84
     O*******                TPPHYN              54
     O*******                TPPHYA             107
     O*******   E            NOI              85
     O*******                XXPCSZ             127
     O*******   E            NOI              88
     O*******                CMHOSN              54
     O*******                CMHOSA             107
     O*******   E            NOI              89
     O*******                XXHCSZ             127
     O*
     O* BOTTOM SECTION
     O*
     O*******   E            NOI              95
     O*******                FNNAME              74
     O*******   E            NOI              96
     O*******                XXFADR              74
     O*******   E            NOI              97
     O*******                XXFCSZ              74
     O*******   E            NOI         1    99
     O*******                XXPOL#              90
     O*******                XXPFDT            +002 '  /  /    '
     O*******                                  +001 '-'
     O*******                XXPTDT            +001 '  /  /    '
     O*******   E            NOI         3
     O*******   E            NOI         3
     O*******   E            NOI         3
     O*******   E            NOI         3
     O*******   E            NOI         3
     O*******   E            NOI         3
     O*******   E            NOI         1
     O*******                XXPOS              110
     O*
     O*******QSYSPRT   E            BLANK            01
      ***** end del ***** 06/22/15 ***********************************
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
      ***** beg del ***** 06/22/15 ***********************************
     O*******QSYSPRT2  E            NOI2              1
     O*
     O* TOP SECTION
     O*
     O*******   E            NOI2             11
     O*******                XXCLM#             117
     O*******   E            NOI2             19
     O*******                XXEMP#             117
     O*
     O* EMPLOYEE SECTION
     O*
     O*******   E            NOI2             25
     O*******                CDFSN               41
     O*******                CDMID               49
     O*******                CDLSN               94
     O*******                CDSS#              108
     O*******   E            NOI2             27
     O*******        20                          79 'X'
     O*******   E            NOI2             28
     O*******                XXCADR              74
     O*******   E            NOI2             29
     O*******                XXCCS2              74
     O*******        21                          79 'X'
     O*******                                    90 '('
     O*******                CDAREA        4   +000
     O*******                                  +000 ')'
     O*******                CDPHN#            +001 '   -    '
     O*******   E            NOI2             33
     O*******                XXDTOB              27 '  /  /    '
     O*******                XX10A1        4     39
     O*******                CMAWW         4     60
     O*******                XXHRSW        4    100
     O*******   E            NOI2             37
     O*******                CMOCDS              54
     O*******                XX40A4              85
     O*******                NI20A3             110
     O*
     O* EMPLOYER SECTION
     O*
     O*******   E            NOI2             41
     O*******                EMNAME              55
     O*******                XXPRNM              95
     O*
     O*******   E            NOI2             43
     O*******                XXECNT              64
     O*
     O*******   E            NOI2             44
     O*******                XXEPAD              76
     O*******                                    81 '('
     O*******                EMAREA        4   +000
     O*******                                  +000 ')'
     O*******                EMPHN#            +001 '   -    '
     O*
     O*******   E            NOI2             45
     O*******                XXEPCS              76
     O*
     O*******   E            NOI2             48
     O*******        23      XXEMAD              66
     O*******                EMNAOB             110
     O*
     O*******   E            NOI2             49
     O*******        23      XXEMCS              66
     O*
     O* DATES SECTION
     O*
     O*******   E            NOI2             51
     O*******                XXACDT              41 '  /  /    '
     O*
     O*******   E            NOI2             52
     O*******                XXPREP              27 '  /  /    '
     O*******                XXACTM              41 '  :  '
     O*******        24                          43 'x'
     O*******        25                          47 'x'
     O*******                XXTIM2              66 '  :  '
     O*******                XXRTW               89 '  /  /    '
     O*******        26                          94 'x'
     O*******        27                         103 'x'
     O*
     O*******   E            NOI2             56
     O*******                XXDOD               32 '  /  /    '
     O*******                XXDTRP              55 '  /  /    '
     O*******                XXDBDT              75 '  /  /    '
     O*******                XXDT2              100 '  /  /    '
     O*
     O* INJURY OR DISEASE SECTION
     O*
     O*******   E            NOI2             60
     O*******                XXNPC              114
     O*******   E            NOI2             63
     O*******                NI40A8              54
     O*******   E            NOI2             64
     O*******                XX60A5             114
     O*
     O* OCCURRENCE SECTION
     O*
     O*******   E            NOI2             67
     O*******                NI60A1              74
     O*******   E            NOI2             68
     O*******                NI60A2              74
     O*******   E            NOI2             72
     O*******                NI60A3              74
     O*******   E            NOI2             73
     O*******                NI60A4              74
     O*******   E            NOI2             78
     O*******                CMACDS              74
     O*******   E            NOI2             79
     O*******                CMACD2              74
     O*******   E            NOI2             84
     O*******                TPPHYN              54
     O*******                TPPHYA             107
     O*******   E            NOI2             85
     O*******                XXPCSZ             127
     O*******   E            NOI2             88
     O*******                CMHOSN              54
     O*******                CMHOSA             107
     O*******   E            NOI2             89
     O*******                XXHCSZ             127
     O*
     O* BOTTOM SECTION
     O*
     O*******   E            NOI2             95
     O*******                FNNAME              74
     O*******   E            NOI2             96
     O*******                XXFADR              74
     O*******   E            NOI2             97
     O*******                XXFCSZ              74
     O*******   E            NOI2        1    99
     O*******                XXPOL#              90
     O*******                XXPFDT            +002 '  /  /    '
     O*******                                  +001 '-'
     O*******                XXPTDT            +001 '  /  /    '
     O*******   E            NOI2        3
     O*******   E            NOI2        3
     O*******   E            NOI2        3
     O*******   E            NOI2        3
     O*******   E            NOI2        3
     O*******   E            NOI2        3
     O*******   E            NOI2        1
     O*******                XXPOS              110
     O*
     O*******QSYSPRT2  E            BLANK2           01
      ***** end del ***** 06/22/15 ***********************************
**  COPY MSG
EMPLOYER COPY
EMPLOYEE COPY
DIVISION COPY
