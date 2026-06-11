     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE W8070RCA - California NOI Print Program
     H*****************************************************************
     H* Program Description:  This program prints California NOI      *
     H*                       forms.                                  *
     H*                                                               *
     H* Called from:  W8070CCA                                        *
     H* Calls to:  None                                               *
     H*                                                               *
     H*****************************************************************
     H* Created 01/06/2004 by c:62 for Project 132DAW                 *
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
     H* 04/17/2006 C:62 SUPPORT CI132 52316                           *
     H*                 REPORT CHANGES                                *
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
     F*** BEGIN DLT *** 04/17/06 **************************************
     F****WTGTL   IF  E           K        DISK
     F*** END DLT ***** 04/17/06 **************************************
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
     D TYPE1L          C                   CONST('Private')
     D TYPE2L          C                   CONST('State')
     D TYPE3L          C                   CONST('County')
     D TYPE4L          C                   CONST('City')
     D TYPE5L          C                   CONST('School District')
     D*** BEGIN ADD *** 04/17/06 **************************************
     D TYPE1U          C                   CONST('PRIVATE')
     D TYPE2U          C                   CONST('STATE')
     D TYPE3U          C                   CONST('COUNTY')
     D TYPE4U          C                   CONST('CITY')
     D TYPE5U          C                   CONST('SCHOOL DISTRICT')
     D*** END   ADD *** 04/17/06 **************************************
     D*
     D XXOVR           C                   CONST('OVRPRTF QSYSPRT  OUT-
     D                                     Q(          )')
     D XXOVR1          C                   CONST('OVRPRTF QSYSPRT1 OUT-
     D                                     Q(          )')
     D XXOVR2          C                   CONST('OVRPRTF QSYSPRT2 OUT-
     D                                     Q(          )')
      ***** BEG ADD ***** 10/17/11 ***********************************
     D RPGM            C                   CONST('RTVDOCROOT')
     D RPGM1           C                   CONST('RTVDOCENV')
      ***** END ADD ***** 10/17/11 ***********************************
     D*================================================================
     D* Data Structures
     D*================================================================
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
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
     C****       GTKY      KLIST
     C****                 KFLD           KKCO#
     C****                 KFLD           KKTBID
     C****                 KFLD           KKKEY
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
     C***        *LIKE     DEFN GTTBID    KKTBID
     C     *LIKE         DEFINE    PAKEY         KKKEY
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
     C                   MOVEA     '00000'       *IN(21)
     C                   MOVEA     '00000'       *IN(26)
     C                   MOVEA     '00000'       *IN(31)
     C                   MOVEA     '000000'      *IN(36)
     C*================================================================
     C* Employer Info
     C*================================================================
     C*
     C*  Box 1 - Firm name
     C*
     C* EMNAME
     C*
     C*  Box 1a  - Policy number
     C*
     C     RPKY          CHAIN     PRP01                              99
     C     *IN99         IFEQ      *OFF
     C     RPCAP#        ANDNE     *BLANKS
     C                   MOVEL(P)  RPCAP#        XXCAP#           25
     C                   ELSE
     C                   MOVE      CMFND         XXFND             3
     C                   MOVE      CMEMP#        XXEMP#            9
     C                   MOVE      CMFYR         XXFYR             3
     C     XXFND         CAT(P)    '-':0         XXCAP#
     C                   CAT       XXEMP#:0      XXCAP#
     C                   CAT       '-':0         XXCAP#
     C                   CAT       XXFYR:0       XXCAP#
     C                   ENDIF
     C*
     C*  Box 2 - Mailing Address
     C*
     C                   MOVEL     EMMZIP        XXZIPA            5
     C                   MOVE      EMMZIP        XXZIPB            4
     C*
     C     EMMAD1        IFNE      *BLANKS
     C     EMMAD1        CAT(P)    EMMAD2:2      XXMADR           70
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
     C*  Box 2a  - Phone number
     C*
     C                   MOVE      EMAREA        XXAREA            3
     C                   MOVEL     EMPHN#        XXPHNA            3
     C                   MOVE      EMPHN#        XXPHNB            4
     C*
     C     EMPHN#        IFNE      *ZEROS
     C     EMAREA        IFNE      *ZEROS
     C     '('           CAT(P)    XXAREA:0      XXPHN#           14
     C                   CAT       ')':0         XXPHN#
     C                   CAT       XXPHNA:1      XXPHN#
     C                   CAT       '-':0         XXPHN#
     C                   ELSE
     C     XXPHNA        CAT(P)    '-':0         XXPHN#
     C                   ENDIF
     C                   CAT       XXPHNB:0      XXPHN#
     C                   ELSE
     C                   CLEAR                   XXPHN#
     C                   ENDIF
     C*
     C*  Box 3 - Location
     C*
     C                   MOVEL     NI9N2         XXZIPA
     C                   MOVE      NI9N2         XXZIPB
     C*
     C     NI40A1        IFNE      *BLANKS
     C     NI40A1        CAT(P)    NI40A2:2      XXPADR           70
     C                   CAT       NI25A1:2      XXPADR
     C                   CAT       ',':0         XXPADR
     C                   CAT       NI2A1:1       XXPADR
     C                   CAT       XXZIPA:2      XXPADR
     C     XXZIPB        IFNE      '0000'
     C                   CAT       '-':0         XXPADR
     C                   CAT       XXZIPB:0      XXPADR
     C                   ENDIF
     C                   ELSE
     C                   CLEAR                   XXPADR
     C                   ENDIF
     C*
     C*  Box 3a - Location code
     C*
     C* CMCTG1
     C*
     C*  Box 4 - Nature of Business
     C*
     C*** BEGIN CHG *** 04/17/06 **************************************
     C**********           MOVEL'SIC'     KKTBID    P
     C**********           MOVELEMSIC     KKKEY     P
     C********** GTKY      CHAINPGT01                99
     C********** *IN99     IFEQ *OFF
     C**********           MOVELGTDESC    XXSICD 40
     C**********           ELSE
     C**********           CLEARXXSICD
     C**********           ENDIF
     C                   MOVEL(P)  EMNAOB        XXSICD           40
     C*** END   CHG *** 04/17/06 **************************************
     C*
     C*  Box 5 - State unemployment Insurance acct#
     C*
     C* NI30A2
     C*
     C*  Box 6 - Type of employer
     C*
     C     NI20A1        IFNE      *BLANKS
     C                   SELECT
     C*** BEGIN CHG *** 04/17/06 **************************************
     C********** NI20A1    WHEQ TYPE1
     C     NI20A1        WHENEQ    TYPE1L
     C     NI20A1        OREQ      TYPE1U
     C*** END   CHG *** 04/17/06 **************************************
     C                   SETON                                        21
     C*** BEGIN CHG *** 04/17/06 **************************************
     C********** NI20A1    WHEQ TYPE2
     C     NI20A1        WHENEQ    TYPE2L
     C     NI20A1        OREQ      TYPE2U
     C*** END   CHG *** 04/17/06 **************************************
     C                   SETON                                        22
     C*** BEGIN CHG *** 04/17/06 **************************************
     C********** NI20A1    WHEQ TYPE3
     C     NI20A1        WHENEQ    TYPE3L
     C     NI20A1        OREQ      TYPE3U
     C*** END   CHG *** 04/17/06 **************************************
     C                   SETON                                        23
     C*** BEGIN CHG *** 04/17/06 **************************************
     C********** NI20A1    WHEQ TYPE4
     C     NI20A1        WHENEQ    TYPE4L
     C     NI20A1        OREQ      TYPE4U
     C*** END   CHG *** 04/17/06 **************************************
     C                   SETON                                        24
     C*** BEGIN CHG *** 04/17/06 **************************************
     C********** NI20A1    WHEQ TYPE5
     C     NI20A1        WHENEQ    TYPE5L
     C     NI20A1        OREQ      TYPE5U
     C*** END   CHG *** 04/17/06 **************************************
     C                   SETON                                        25
     C                   OTHER
     C                   SETON                                        26
     C                   MOVEL     NI20A1        XX20A1           12
     C                   ENDSL
     C*
     C*
     C                   ENDIF
     C*================================================================
     C* Injury or Illness Info
     C*================================================================
     C*
     C*  Box 7 - Date of Injury
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
     C*
     C*  Box 8 - Time of Injury
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
     C*  Box 9 - Time employee began work
     C*
     C*** BEGIN CHG *** 04/17/06 **************************************
     C**********           SETOF                     28
     C                   SETOFF                                       44
     C*** END   CHG *** 04/17/06 **************************************
     C                   MOVE      NITIM2        TM6               6 0
     C                   MOVEL     TM6           XXTIM1            4 0
     C                   MOVEL     XXTIM1        TEST1
     C     TEST1         IFGE      12
     C*** BEGIN CHG *** 04/17/06 **************************************
     C**********           SETON                     28
     C                   SETON                                        44
     C*** END   CHG *** 04/17/06 **************************************
     C     TEST1         IFGT      12
     C                   SUB       1200          XXTIM1
     C                   ENDIF
     C                   ELSE
     C     TEST1         IFEQ      0
     C                   ADD       1200          XXTIM1
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Box 10 - Date of Death
     C*
     C                   Z-ADD     CMDOD         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDOD             8 0
     C*
     C*  Box 11 - Unable to work for at least one full
     C*           day after date of injury
     C*
     C                   SETOFF                                       28
     C     CMGT1         IFNE      'Y'
     C                   SETON                                        28
     C                   ENDIF
     C*
     C*  Box 12 - Date last worked
     C*
     C     CMDTLW        IFNE      *ZEROS
     C                   Z-ADD     CMDTLW        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDTLW            8 0
     C                   ELSE
     C                   CLEAR                   XXDTLW
     C                   ENDIF
     C*
     C*  Box 13 - Date returned to work
     C*
     C     CMRTW         IFNE      *ZEROS
     C                   Z-ADD     CMRTW         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXRTW             8 0
     C                   ELSE
     C                   CLEAR                   XXRTW
     C                   ENDIF
     C*
     C*  Box 14 - If still of work, check box
     C*
     C                   SETOFF                                       29
     C     NI1A3         IFEQ      'Y'
     C                   SETON                                        29
     C                   ENDIF
     C*
     C*  Box 15 - Paid full days wages for DOI
     C*
     C                   SETOFF                                       30
     C     CMPOI         IFNE      'Y'
     C                   SETON                                        30
     C                   ENDIF
     C*
     C*  Box 16 - Salary being continued
     C*
     C                   SETOFF                                       31
     C     CMFSAL        IFNE      'Y'
     C                   SETON                                        31
     C                   ENDIF
     C*
     C*  Box 17 - Date of employer's knowledge/notice of injury
     C*
     C     CMDTRP        IFNE      *ZEROS
     C                   Z-ADD     CMDTRP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDTRP            8 0
     C                   ELSE
     C                   CLEAR                   XXDTRP
     C                   ENDIF
     C*
     C*  Box 18 - Date employee was provided claim form
     C*
     C     NIDT1         IFNE      *ZEROS
     C                   Z-ADD     NIDT1         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT1             8 0
     C                   ELSE
     C                   CLEAR                   XXDT1
     C                   ENDIF
     C*
     C*  Box 19 - Specific injury/illness and part of body
     C*           affected, medical diagnosis
     C*
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
     C*  Box 20 - Location where event or exposure
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
     C                   CAT       CMACST:1      XXAADR
     C                   CAT       XXZIPA:2      XXAADR
     C     XXZIPB        IFNE      '0000'
     C                   CAT       '-':0         XXAADR
     C                   CAT       XXZIPB:0      XXAADR
     C                   ENDIF
     C                   ELSE
     C                   CLEAR                   XXAADR
     C                   ENDIF
     C*
     C*  Box 20a - County
     C*
     C                   MOVEL     NI30A3        XX30A3           20
     C*
     C*  Box 21 - On employer's premises?
     C*
     C                   SETOFF                                       32
     C     CMEADR        IFNE      'Y'
     C                   SETON                                        32
     C                   ENDIF
     C*
     C*  Box 22 - Dept where event/exposure occurred
     C*
     C* NI40A4
     C*
     C*  Box 23 - Other workers injured
     C*
     C                   SETOFF                                       33
     C*** BEGIN CHG *** 04/17/06 **************************************
     C********** NI1A4     IFEQ 'Y'
     C     NI1A4         IFNE      'Y'
     C*** END   CHG *** 04/17/06 **************************************
     C                   SETON                                        33
     C                   ENDIF
     C*
     C*  Box 24 - Equipment, etc. used when event occurred
     C*
     C* NI80A4
     C* NI80A5
     C*
     C*  Box 25 - Specific activity employee was
     C*           performing when event occurred
     C*
     C* NI80A1
     C* NI80A2
     C*
     C*  Box 26 - How illness occurred
     C*
     C* CMACDS
     C* CMACD2
     C*
     C*  Box 27 - Name and address of Physician
     C*
     C*** BEGIN DEL *** 04/17/06 **************************************
     C**********           MOVELTPPHYZ    XXZIPA
     C**********           MOVE TPPHYZ    XXZIPB
     C*** END   DEL *** 04/17/06 **************************************
     C*
     C     TPKY          CHAIN     PTP01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C*** BEGIN ADD *** 04/17/06 **************************************
     C                   MOVEL     TPPHYZ        XXZIPA
     C                   MOVE      TPPHYZ        XXZIPB
     C*** END   ADD *** 04/17/06 **************************************
     C*
     C                   MOVEL     TPPHYN        XXPHYN           40
     C     NI40A5        IFNE      *BLANKS
     C     TPPHYA        CAT(P)    NI40A5:1      XXTADR           50
     C                   CAT       TPPHYC:2      XXTADR
     C                   ELSE
     C     TPPHYA        CAT(P)    TPPHYC:1      XXTADR
     C                   ENDIF
     C                   CAT       ',':0         XXTADR
     C                   CAT       TPPHYS:1      XXTADR
     C                   CAT       XXZIPA:2      XXTADR
     C     XXZIPB        IFNE      '0000'
     C                   CAT       '-':0         XXTADR
     C                   CAT       XXZIPB:0      XXTADR
     C                   ENDIF
     C                   ELSE
     C                   CLEAR                   XXPHYN
     C                   CLEAR                   XXTADR
     C                   ENDIF
     C*
     C*  Box 27a - Phone number of physician
     C*
     C                   MOVE      TPPHYR        XXAREA            3
     C                   MOVEL     TPPHYP        XXPHNA            3
     C                   MOVE      TPPHYP        XXPHNB            4
     C*
     C     TPPHYP        IFNE      *ZEROS
     C     TPPHYR        IFNE      *ZEROS
     C     '('           CAT(P)    XXAREA:0      XXPPHN           14
     C                   CAT       ')':0         XXPPHN
     C                   CAT       XXPHNA:1      XXPPHN
     C                   CAT       '-':0         XXPPHN
     C                   ELSE
     C     XXPHNA        CAT(P)    '-':0         XXPPHN
     C                   ENDIF
     C                   CAT       XXPHNB:0      XXPPHN
     C                   ELSE
     C                   CLEAR                   XXPPHN
     C                   ENDIF
     C*
     C*  Box 28 - Hospitalized as inpatient?
     C*
     C                   SETOFF                                       34
     C     NI1A5         IFEQ      'Y'
     C                   SETON                                        34
     C                   ENDIF
     C*
     C     *IN34         IFEQ      *ON
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
     C*  Box 28a - Hospital Phone #
     C*
     C*
     C     CMHOSF        IFNE      *ZEROS
     C*
     C                   Z-ADD     CMHOSF        KKFEIN
     C                   Z-ADD     CMHOSM        KKMOD
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      PDAREA        XXAREA            3
     C                   MOVEL     PDPHN#        XXPHNA            3
     C                   MOVE      PDPHN#        XXPHNB            4
     C*
     C     PDPHN#        IFNE      *ZEROS
     C     PDAREA        IFNE      *ZEROS
     C     '('           CAT(P)    XXAREA:0      XXHPHN           14
     C                   CAT       ')':0         XXHPHN
     C                   CAT       XXPHNA:1      XXHPHN
     C                   CAT       '-':0         XXHPHN
     C                   ELSE
     C     XXPHNA        CAT(P)    '-':0         XXHPHN
     C                   ENDIF
     C                   CAT       XXPHNB:0      XXHPHN
     C                   ELSE
     C                   CLEAR                   XXHPHN
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   MOVE      CMHOSR        XXAREA            3
     C                   MOVEL     CMHOSP        XXPHNA            3
     C                   MOVE      CMHOSP        XXPHNB            4
     C*
     C     CMHOSP        IFNE      *ZEROS
     C     CMHOSR        IFNE      *ZEROS
     C     '('           CAT(P)    XXAREA:0      XXHPHN           14
     C                   CAT       ')':0         XXHPHN
     C                   CAT       XXPHNA:1      XXHPHN
     C                   CAT       '-':0         XXHPHN
     C                   ELSE
     C     XXPHNA        CAT(P)    '-':0         XXHPHN
     C                   ENDIF
     C                   CAT       XXPHNB:0      XXHPHN
     C                   ELSE
     C                   CLEAR                   XXHPHN
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Box 29 - Employer Treated in Emergency Room?
     C*
     C                   SETOFF                                       35
     C     NI1A6         IFNE      'Y'
     C                   SETON                                        35
     C                   ENDIF
     C*
     C*================================================================
     C* Employee Info
     C*================================================================
     C*
     C*  Box 29 - Name
     C*
     C     CMFSN         CAT(P)    CMMID:1       XXCLNM           46
     C     CMMID         IFNE      *BLANKS
     C                   CAT       '.':0         XXCLNM
     C                   ENDIF
     C                   CAT       CMLSN:1       XXCLNM
     C*
     C*  Box 30 - SS#
     C*
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C     3             SUBST(P)  CMSS#:1       XXSS1             3
     C     2             SUBST(P)  CMSS#:4       XXSS2             2
     C     4             SUBST(P)  CMSS#:6       XXSS3             4
     C*
     C     XXSS1         CAT(P)    '-':0         XXSS#            11
     C                   CAT       XXSS2:0       XXSS#
     C                   CAT       '-':0         XXSS#
     C                   CAT       XXSS3:0       XXSS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C*
     C*  Box 31 - Date of Birth
     C*
     C     CDDTOB        IFNE      *ZEROS
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDTOB            8 0
     C                   ELSE
     C                   CLEAR                   XXDTOB
     C                   ENDIF
     C*
     C*  Box 32 - Address
     C*
     C                   MOVEL     CDZIP         XXZIPA            5
     C                   MOVE      CDZIP         XXZIPB            4
     C*
     C     CDADR1        IFNE      *BLANKS
     C*** BEGIN CHG *** 04/17/06 **************************************
     C********** CDADR1    CAT  CDADR1:2  XXCADR 70 P
     C     CDADR1        CAT(P)    CDADR2:2      XXCADR           70
     C*** END   CHG *** 04/17/06 **************************************
     C                   CAT       CDCTY:2       XXCADR
     C                   CAT       ',':0         XXCADR
     C                   CAT       CDST:1        XXCADR
     C                   CAT       XXZIPA:2      XXCADR
     C     XXZIPB        IFNE      '0000'
     C                   CAT       '-':0         XXCADR
     C                   CAT       XXZIPB:0      XXCADR
     C                   ENDIF
     C                   ELSE
     C                   CLEAR                   XXCADR
     C                   ENDIF
     C*
     C*  Box 33 - Phone #
     C*
     C                   MOVE      CDAREA        XXAREA            3
     C                   MOVEL     CDPHN#        XXPHNA            3
     C                   MOVE      CDPHN#        XXPHNB            4
     C*
     C     CDPHN#        IFNE      *ZEROS
     C     CDAREA        IFNE      *ZEROS
     C     '('           CAT(P)    XXAREA:0      XXCPHN           14
     C                   CAT       ')':0         XXCPHN
     C                   CAT       XXPHNA:1      XXCPHN
     C                   CAT       '-':0         XXCPHN
     C                   ELSE
     C     XXPHNA        CAT(P)    '-':0         XXCPHN
     C                   ENDIF
     C                   CAT       XXPHNB:0      XXCPHN
     C                   ELSE
     C                   CLEAR                   XXCPHN
     C                   ENDIF
     C*
     C*  Box 34 - Sex
     C*
     C                   SELECT
     C     CDSEX         WHENEQ    'M'
     C                   SETOFF                                       36
     C     CDSEX         WHENEQ    'F'
     C                   SETON                                        36
     C                   ENDSL
     C*
     C*  Box 35 -  Occupation
     C*
     C* CMOCDS
     C*
     C*  Box 36 - Date of Hire
     C*
     C     CMHIDT        IFNE      *ZEROS
     C                   Z-ADD     CMHIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXHIDT            8 0
     C                   ELSE
     C                   CLEAR                   XXHIDT
     C                   ENDIF
     C*
     C*  Box 37 - Employee usually works
     C*
     C                   Z-ADD     NI3N1         XXWHRS            4 0
     C*
     C*  Box 37a - Employment status
     C*
     C                   SELECT
     C     CMESTS        WHENEQ    '1'
     C     CMESTS        OREQ      'FT'
     C                   SETON                                        37
     C     CMESTS        WHENEQ    '2'
     C     CMESTS        OREQ      'PT'
     C                   SETON                                        38
     C     CMESTS        WHENEQ    '8'
     C     CMESTS        OREQ      'SL'
     C                   SETON                                        39
     C                   OTHER
     C                   SETON                                        40
     C                   ENDSL
     C*
     C*  Box 37b - Class
     C*
     C* CMCLAS
     C*
     C*  Box 38 -  Gross Wages
     C*
     C                   MOVE      NI9N3         CMAWW
     C                   MOVEL     NI40A6        XXPER            10
     C*
     C*  Box 39 -  Other payments not reported
     C*
     C                   SETOFF                                       41
     C     CMOTWK        IFEQ      *ZEROS
     C*** BEGIN ADD *** 04/17/06 **************************************
     C     NI1A7         ANDNE     'Y'
     C*** END   ADD *** 04/17/06 **************************************
     C                   SETON                                        41
     C                   ENDIF
     C*
     C*  Completed by box - name and date
     C*
     C                   MOVEL     NIPRNM        XXPRNM           25
     C*
     C     NIPREP        IFNE      *ZEROS
     C                   Z-ADD     NIPREP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPREP            8 0
     C                   ELSE
     C                   CLEAR                   XXPREP
     C                   ENDIF
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
     C                   MOVEL     *BLANKS       CMUNCN
     C                   MOVEL     *BLANKS       CMIFSP
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
     O          E            NOI              14
     O                       EMNAME              48
     O                       XXCAP#             108
     O          E            NOI              16
     O                       XXMADR              78
     O                       XXPHN#              97
     O          E            NOI              19
     O                       XXPADR              78
     O                       CMCTG1              93
     O          E            NOI              21
     O                       XXSICD              48
     O                       NI30A2             113
     O          E            NOI              23
     O               21                          22 'X'
     O               22                          32 'X'
     O               23                          44 'X'
     O               24                          55 'X'
     O               25                          66 'X'
     O               26                          79 'X'
     O               26      XX20A1             104
     O          E            NOI              26
     O                       XXACDT              23 '  /  /    '
AM   O              N27      XXACTM              32 '  :  '
PM   O               27      XXACTM              40 '  :  '
     O*** BEGIN CHG *** 04/17/06 **************************************
AM   O**********      N28      XXTIM1    63 '  :  '
PM   O**********       28      XXTIM1    72 '  :  '
AM   O              N44      XXTIM1              63 '  :  '
PM   O               44      XXTIM1              72 '  :  '
     O*** END   CHG *** 04/17/06 **************************************
     O                       XXDOD               93 '  /  /    '
     O          E            NOI              28
     O              N28                           9 'X'
     O               28                          15 'X'
     O                       XXDTLW              37 '  /  /    '
     O                       XXRTW               68 '  /  /    '
     O               29                          93 'X'
     O          E            NOI              31
     O              N30                          14 'X'
     O               30                          22 'X'
     O              N31                          29 'X'
     O               31                          38 'X'
     O                       XXDTRP              68 '  /  /    '
     O                       XXDT1               93 '  /  /    '
     O          E            NOI              33
     O                       XXNPC              108
     O          E            NOI              36
     O                       CMACNM              48
     O                       XX30A3              78
     O              N32                          87 'X'
     O               32                          95 'X'
     O          E            NOI              37
     O                       XXAADR              58
     O          E            NOI              39
     O                       NI40A4              48
     O              N33                          72 'X'
     O               33                          88 'X'
     O          E            NOI              41
     O                       NI80A4              88
     O          E            NOI              42
     O                       NI80A5              88
     O          E            NOI              45
     O*** BEGIN CHG *** 04/17/06 **************************************
     O**********               NI80A1    88
     O                       NI80A6              88
     O*** END   CHG *** 04/17/06 **************************************
     O          E            NOI              46
     O*** BEGIN CHG *** 04/17/06 **************************************
     O**********               NI80A2    88
     O                       NI80A7              88
     O*** END   CHG *** 04/17/06 **************************************
     O          E            NOI              49
     O                       CMACDS              68
     O          E            NOI              50
     O                       CMACD2              68
     O          E            NOI              54
     O                       XXPHYN              48
     O                       XXPPHN              97
     O          E            NOI              55
     O                       XXTADR              58
     O          E            NOI              57
     O              N34                          33 'X'
     O               34                          39 'X'
     O                       XXHPHN              97
     O          E            NOI              58
     O                       XXHOSN              48
     O          E            NOI              59
     O                       XXHADR              58
     O              N35                          85 'X'
     O               35                          96 'X'
     O          E            NOI              65
     O                       XXCLNM              54
     O                       XXSS#               69
     O                       XXDTOB              93 '  /  /    '
     O          E            NOI              68
     O                       XXCADR              78
     O                       XXCPHN              97
     O          E            NOI              71
     O              N36                           8 'X'
     O               36                          15 'X'
     O                       CMOCDS              66
     O                       XXHIDT              93 '  /  /    '
     O          E            NOI              73
     O                       CMHRSD        4     10
     O                       CMDAYS        4     27
     O                       XXWHRS        4     41
     O               37                          58 'X'
     O               38                          74 'X'
     O          E            NOI              74
     O               39                          58 'X'
     O               40                          74 'X'
     O                       CMCLAS        4     88
     O          E            NOI              77
     O                       CMAWW         4     36
     O                       XXPER               50
     O              N41                          58 'X'
     O               41                          76 'X'
     O          E            NOI              81
     O                       XXPRNM              35
     O                       XXPREP             117 '  /  /    '
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
     O          E            NOI2             14
     O                       EMNAME              48
     O                       XXCAP#             108
     O          E            NOI2             16
     O                       XXMADR              78
     O                       XXPHN#              97
     O          E            NOI2             19
     O                       XXPADR              78
     O                       CMCTG1              93
     O          E            NOI2             21
     O                       XXSICD              48
     O                       NI30A2             113
     O          E            NOI2             23
     O               21                          22 'X'
     O               22                          32 'X'
     O               23                          44 'X'
     O               24                          55 'X'
     O               25                          66 'X'
     O               26                          79 'X'
     O               26      XX20A1             104
     O          E            NOI2             26
     O                       XXACDT              23 '  /  /    '
AM   O              N27      XXACTM              32 '  :  '
PM   O               27      XXACTM              40 '  :  '
     O*** BEGIN CHG *** 04/17/06 **************************************
AM   O**********      N44      XXTIM1    63 '  :  '
PM   O**********       44      XXTIM1    72 '  :  '
AM   O              N44      XXTIM1              63 '  :  '
PM   O               44      XXTIM1              72 '  :  '
     O*** END   CHG *** 04/17/06 **************************************
     O                       XXDOD               93 '  /  /    '
     O          E            NOI2             28
     O              N28                           9 'X'
     O               28                          15 'X'
     O                       XXDTLW              37 '  /  /    '
     O                       XXRTW               68 '  /  /    '
     O               29                          93 'X'
     O          E            NOI2             31
     O              N30                          14 'X'
     O               30                          22 'X'
     O              N31                          29 'X'
     O               31                          38 'X'
     O                       XXDTRP              68 '  /  /    '
     O                       XXDT1               93 '  /  /    '
     O          E            NOI2             33
     O                       XXNPC              108
     O          E            NOI2             36
     O                       CMACNM              48
     O                       XX30A3              78
     O              N32                          87 'X'
     O               32                          95 'X'
     O          E            NOI2             37
     O                       XXAADR              58
     O          E            NOI2             39
     O                       NI40A4              48
     O              N33                          72 'X'
     O               33                          88 'X'
     O          E            NOI2             41
     O                       NI80A4              88
     O          E            NOI2             42
     O                       NI80A5              88
     O          E            NOI2             45
     O*** BEGIN CHG *** 04/17/06 **************************************
     O**********               NI80A1    88
     O                       NI80A6              88
     O*** END   CHG *** 04/17/06 **************************************
     O          E            NOI2             46
     O*** BEGIN CHG *** 04/17/06 **************************************
     O**********               NI80A2    88
     O                       NI80A7              88
     O*** END   CHG *** 04/17/06 **************************************
     O          E            NOI2             49
     O                       CMACDS              68
     O          E            NOI2             50
     O                       CMACD2              68
     O          E            NOI2             54
     O                       XXPHYN              48
     O                       XXPPHN              97
     O          E            NOI2             55
     O                       XXTADR              58
     O          E            NOI2             57
     O              N34                          33 'X'
     O               34                          39 'X'
     O                       XXHPHN              97
     O          E            NOI2             58
     O                       XXHOSN              48
     O          E            NOI2             59
     O                       XXHADR              58
     O              N35                          85 'X'
     O               35                          96 'X'
     O          E            NOI2             65
     O                       XXCLNM              54
     O                       XXSS#               69
     O                       XXDTOB              93 '  /  /    '
     O          E            NOI2             68
     O                       XXCADR              78
     O                       XXCPHN              97
     O          E            NOI2             71
     O              N36                           8 'X'
     O               36                          15 'X'
     O                       CMOCDS              66
     O                       XXHIDT              93 '  /  /    '
     O          E            NOI2             73
     O                       CMHRSD        4     10
     O                       CMDAYS        4     27
     O                       XXWHRS        4     41
     O               37                          58 'X'
     O               38                          74 'X'
     O          E            NOI2             74
     O               40                          58 'X'
     O               39                          74 'X'
     O                       CMCLAS        4     88
     O          E            NOI2             77
     O                       CMAWW         4     36
     O                       XXPER               50
     O              N41                          58 'X'
     O               41                          76 'X'
     O          E            NOI2             81
     O                       XXPRNM              36
     O                       XXPREP             117 '  /  /    '
**  COPY MSG
EMPLOYER COPY
EMPLOYEE COPY
DIVISION COPY
