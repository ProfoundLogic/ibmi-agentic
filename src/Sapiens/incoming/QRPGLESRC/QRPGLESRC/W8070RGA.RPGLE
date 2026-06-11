     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W8070RGA - GEORGIA NOI PRINT PROGRAM
     H*****************************************************************
     H* PROGRAM DESCRIPTION:  THIS PROGRAM PRINTS GEORGIA NOI FORMS.  *
     H*                                                               *
     H* CALLED FROM:  W8070CGA                                        *
     H* CALLS TO:  NONE                                               *
     H* PROJECT: 91WAF          C:112          12/30/99               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*   06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H*   10/17/11 C:279 Project: SY195                               *
     H*                  - Modify program to pass/return UNC path     *
     H*                    field and IFS flag to called/calling       *
     H*                    programs.                                  *
     H*                                                               *
     H* 03/24/10 C:232 Project: 139WNN                                *
     H*                Update programs to use the new EDI Control API *
     H*                WA076R instead of using WMIXP.                 *
     H*                                                               *
     H* 10/04/07 C:191 Project: 91WOT                                 *
     H*                New version GA FROI form 07/2007               *
     H*                                                               *
     H* 11/20/06 C:185 Project: 91WNT                                 *
     H*                New form                                       *
     H*                                                               *
     H* 07/25/06 C:53  Support: 53754                                 *
     H*                Move code so it is not overwritten.            *
     H*                                                               *
     H* 06/28/06 C:53  Project: 91WNF                                 *
     H*                Add defaults for specific groups               *
     H*                                                               *
     H* 04/07/06 C:168 SUPPORT: WC091 - 51660                         *
     H*                REMOVED EMPLOYER AREA CODE FROM LINE 21, UPDATED
     H*                SPLF QSYSPRT TO BE THAT SAME AS QSYSPRT2       *
     H*                                                               *
     H* 01/26/06 C:168 Project: 91WMT                                 *
     H*                New form                                       *
     H*                                                               *
     H* 11/13/03 C:53  Project: WC1162                                *
     H*                New form                                       *
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
     FSDCMLD    O    E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FSDCMLA    IF   E           K DISK
     F                                     RENAME(PCM01:PCM03)
     FWMNIL     IF   E           K DISK
     F                                     RENAME(PNI01:PNI04)
     FWMCML1    IF   E           K DISK
     FWMC1L     IF   E           K DISK
     FWMTPL1    IF   E           K DISK
     FWMPDL     IF   E           K DISK
     FWMOFL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSTCTL1    IF   E           K DISK
     FSMCOL     IF   E           K DISK
      *  COUNTY CODE FILE
     FWTCNL1    IF   E           K DISK
     F                                     RENAME(PCN01:PCN02)
     FWTABL     IF   E           K DISK
     F                                     INFDS(DBFDS3)
     FWMUPL     IF   E           K DISK
      *** BEG ADD *** 01/26/06 *****************************
     FWMECL1    IF   E           K DISK
      *** END ADD *** 01/26/06 *****************************
     FWDPAL5    IF   E           K DISK
     F                                     RENAME(PPA01:PPA02)
     FWMTHL     IF   E           K DISK
     F*** BEG DEL *** 11/20/06 **************************************
     F*******SYSPRT O   F     139     OA     PRINTER        UC
     F*** END DEL *** 11/20/06 **************************************
     FQSYSPRT1  O    F  139        PRINTER OFLIND(*INOF)
     F                                     USROPN
     F*** BEG DEL *** 11/20/06 **************************************
     F********SYSPRT2O   F     139     OG     PRINTER        UC
     F*** END DEL *** 11/20/06 **************************************
     F*** BEG ADD *** 11/20/06 **************************************
     FW8070O    O    E             PRINTER

     D*** END ADD *** 11/20/06 **************************************
     D CNB             S              1    DIM(40) ASCEND                       COMPANY NAME W/ BLAN
     D CN              S              1    DIM(40)                              COMPANY NAME
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D CPY             S             21    DIM(3) CTDATA PERRCD(1)              COPY MSG
     D OVR             S              1    DIM(33)
      ***** END ADD ***** 10/17/11 ***********************************
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
     D  XXFNAM                 1     60
     D  XXFNM1                 1     30
     D  XXFNM2                31     60
     D                 DS
     D  XXOFNM                 1     40
     D  XXONM1                 1     20
     D  XXONM2                21     40
     D                 DS
     D  XXOFAD                 1     40
     D*** BEG CHG ****** 10/04/07 **********************************************
     D****************                        1  20 XXOFA1
     D  XXOFA1                 1     35
     D****************                       21  40 XXOFA2
     D*** END CHG ****** 10/04/07 **********************************************
     D                 DS
     D  XXCLSS                 1      9
     D  XXCLS1                 1      3
     D  XXCLS2                 4      5
     D  XXCLS3                 6      9
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
      ***** BEG  CHG  01/26/06 ****************************************
     D*                                     108 117 XXHFSN
     D  XXHFSN               108    122
      ***** BEG  CHG  01/26/06 ****************************************
     D                 DS
     D  XXHOSZ                 1      9
     D  XHOSZ1                 1      5
     D  XHOSZ2                 6      9
     D                 DS
     D  XXTPSZ                 1      9
     D  XTPSZ1                 1      5
     D  XTPSZ2                 6      9
     D XXOVR           C                   CONST('OVRPRTF QSYSPRT  OUT-
     D                                     Q(          )')
     D XXOVR1          C                   CONST('OVRPRTF QSYSPRT1 OUT-
     D                                     Q(          )')
     D XXOVR2          C                   CONST('OVRPRTF QSYSPRT2 OUT-
     D                                     Q(          )')
     D*** BEG ADD *** 06/28/06 **************************************
     D XNAME1          C                   CONST('BUILDERS INSURANCE G-
     D                                     ROUP')
     D XNAME2          C                   CONST('ASSOCIATION INSURANC-
     D                                     E')
     D*** END ADD *** 06/28/06 **************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
      ***** BEG ADD ***** 10/17/11 ***********************************
     D RPGM            C                   CONST('RTVDOCROOT')
     D RPGM1           C                   CONST('RTVDOCENV')

     I*
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
      ***** END ADD ***** 10/17/11 ***********************************

     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
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
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE FUND AUXILIARY FILE
     C*
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  KEY LIST FOR THE LOG HISTORY FILE
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
     C*  KEY LIST FOR THE DARTS REMARK CODE FILE
     C*
     C     CTKY          KLIST
     C                   KFLD                    IXHCPR
     C*
     C*  KEY LIST FOR THE EDI CLAIM CONTROL FILE
     C*
     C     IXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    QQJRST
     C                   KFLD                    XXTSET
     C*
     C*  TREATING PHYSICIAN
     C*
     C     TPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  PROVDER MASTER
     C*
     C     PDKY          KLIST
     C                   KFLD                    CMHOSF
     C                   KFLD                    CMHOSM
     C*
     C     PDKY1         KLIST
     C                   KFLD                    TPPHYF
     C                   KFLD                    TPPHYM
     C*
     C*  KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
      *
      *****  BEG ADD  01/26/06 ****************************************
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     ECKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    ##PCNT            1
      *****  END ADD  01/26/06 ****************************************
     C*
     C*  KEY LIST FOR THE ADDITIONAL LOCATION FILE
     C*
     C     ALKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
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
     C*
     C*  KEY LIST FOR THE OFFICE FILE
     C*
     C     OFKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXOFF#
      *
      *  TABLE FILE
      *
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
      *
      *  Key for User Profile file
      *
     C     UPKY          KLIST
     C                   KFLD                    CMUSER
     C*
     C*  KEY LIST FOR THE PAYMENT DETAIL FILE
     C*
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  KEY LIST FOR THE TRANSFER LOST TIME HISTORY
     C*
     C     THKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
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
     C                   PARM                    QQJRST            2
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
     C     *LIKE         DEFINE    CMRMCD        XXRMCD
     C     *LIKE         DEFINE    CMDCID        QQDCID
     C     *LIKE         DEFINE    CMFOLD        QQFLDR
     C     *LIKE         DEFINE    CMDT          XXSYDT
     C     *LIKE         DEFINE    CMOFF#        XXOFF#
     C     *LIKE         DEFINE    COFEIN        XXCOFE
     C     *LIKE         DEFINE    OFST          XXOFST
     C     *LIKE         DEFINE    OFZIP         XXOFZP
     C     *LIKE         DEFINE    OFAREA        XXOFAR
     C     *LIKE         DEFINE    OFPHN#        XXOFPH
     C     *LIKE         DEFINE    CDADR1        XXCLA1
     C     *LIKE         DEFINE    CDADR2        XXCLA2
     C     *LIKE         DEFINE    CDCTY         XXCLCY
     C     *LIKE         DEFINE    CDST          XXCLST
     C     *LIKE         DEFINE    CDZIP         XXCLZP
     C     *LIKE         DEFINE    CDAREA        XXCLAR
     C     *LIKE         DEFINE    CDPHN#        XXCLPH
     C     *LIKE         DEFINE    CM#DEP        XX#DEP
     C     *LIKE         DEFINE    NI5N1         XXTWDB
     C     *LIKE         DEFINE    NI1A1         XXAMPM
     C     *LIKE         DEFINE    NI20A1        XXDYOF
     C     *LIKE         DEFINE    CMAWW         XXPAWW
     C     *LIKE         DEFINE    CMAWW         XXAWW
     C     *LIKE         DEFINE    CMPOWW        XXPOWW
     C     *LIKE         DEFINE    UPTITL        XXTITL
     C     *LIKE         DEFINE    NI5N2         XXWKBN
     C     *LIKE         DEFINE    PAAMT         XXAAMT
     C     *LIKE         DEFINE    NI3N1         XXPERC
     C     *LIKE         DEFINE    NI10A3        XXPART
     C     *LIKE         DEFINE    NI3N2         XXWKS
     C     *LIKE         DEFINE    NI60A1        XXCONT
     C     *LIKE         DEFINE    F2FEIN        XXFEIN
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
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C                   MOVEL     QQFROI        XXHCPY
     C*** BEG CHG ****** 03/24/10 *************************************
     C***                  MOVE QQJRST    XXJRST
     C                   MOVE      QQJRST        XXJRST            2
     C*** END CHG ****** 03/24/10 *************************************
     C     Q1PRMS        IFGT      4
     C                   MOVE      @@FND         XXFND
     C                   MOVE      @@FYR         XXFYR
     C                   MOVE      @@CASE        XXCASE
     C                   MOVE      @@CASE        XCASIN            7
     C                   ELSE
     C                   MOVE      *ZEROS        XXFND
     C                   MOVE      *ZEROS        XXFYR
     C                   MOVE      *ZEROS        XXCASE
     C                   MOVE      *ZEROS        XCASIN
     C                   ENDIF
     C*
     C*   GET THE DISTRIBUTION INFORMATION
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
     C*** BEG ADD *** 06/28/06 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*** END ADD *** 06/28/06 **************************************
     C*
     C*  IF THIS PGM WAS CALLED FROM THE DAILY BATCH, WXNIP WILL BE READ
     C*  3 TIMES TO PRODUCE 3 COPIES.  IF CALLED FROM THE EXTRACT
     C*  OPTION, 1 READ WILL OCCUR.
     C*
     C     XCASIN        IFEQ      '0000000'
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
     C                   END
     C*
     C     XCASIN        COMP      '0000000'                          7575
     C*
     C*   OPEN THE PRINT FILE
     C*
     C     *IN73         IFEQ      *OFF
     C     XXCOPY        IFEQ      1
     C***                  OPEN QSYSPRT
     C                   MOVE      *ON           XXOPEN
     C                   END
     C     XXCOPY        IFEQ      2
     C                   OPEN      QSYSPRT1
     C                   MOVE      *ON           XXOPN1
     C***                  OPEN QSYSPRT2
     C                   MOVE      *ON           XXOPN2
     C                   END
     C                   END
     C*
     C     *IN75         IFEQ      *ON
     C***                  OPEN QSYSPRT2
     C                   MOVE      *ON           XXOPN2
     C                   Z-ADD     0             XXCOPY
     C                   MOVE      *BLANKS       XXCPY
     C                   END
     C*
     C   71NIKY          CHAIN     PNI01                              50
     C   72NIKY          CHAIN     PNI02                              50
     C   73LGKY          CHAIN     PLG01                              50
     C   75CMKY          CHAIN     PCM01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C     *IN71         IFEQ      '1'
     C     NIIDT         ANDNE     XXDATE
     C     *IN72         OREQ      '1'
     C     NIIDT         ANDNE     XXDATE
     C                   GOTO      SKIP
     C                   ENDIF
     C     *IN73         IFEQ      *ON
     C     LGPRSQ        IFNE      0
     C     LGSCHP        ORNE      LGEXDT
     C                   GOTO      SKIP
     C                   ENDIF
     C                   ENDIF
     C*
     C*   OPEN THE PRINT FILE
     C*
     C     *IN73         IFEQ      *ON
     C***                  OPEN QSYSPRT2
     C                   MOVE      *ON           XXOPN2
     C                   END
     C*
     C                   EXSR      SRINIT
     C*
     C*  IF THE FUND CHANGED, GET THE NEW FUND INFORMATION.
     C*
     C     XXFND         IFNE      SVFND
     C                   EXSR      FNDBRK
     C                   Z-ADD     XXFND         SVFND
     C                   END
     C*
     C*** BEG DEL *** 07/25/06 **************************************
     C*** BEG ADD *** 06/28/06 **************************************
     C*********  CLTID     IFEQ '091'
     C*********
     C*********            SELEC
     C*********  XXFND     WHEQ 034
     C*********            MOVEL'40008'   NI5N3     P
     C*********            MOVELXNAME1    XXPREP    P
     C*********
     C*********  XXFND     WHEQ 022
     C*********            MOVEL'10144'   NI5N3     P
     C*********            MOVELXNAME2    XXPREP    P
     C*********            ENDSL
     C*********            ENDIF
     C*** END ADD *** 06/28/06 **************************************
     C*** END DEL *** 07/25/06 **************************************
     C*
     C*  LOAD CLAIMANT INFORMATION
     C*
     C     CMKY          CHAIN     PCM01                              60
     C   60              GOTO      SKIP
     C     CDKY          CHAIN     PCD01                              60
     C   60              GOTO      SKIP
     C     CMKY          CHAIN     PNI04                              60
     C   60              MOVE      *BLANKS       NI1A3
     C   60              MOVE      *BLANKS       NI10A1
     C   60              MOVE      *BLANKS       NI1A5
     C   60              MOVE      *ZEROS        NIDT4
     C   60              MOVE      *BLANKS       NI80A1
     C     CMKY          CHAIN     PC101                              60
     C   60              MOVE      *BLANKS       C1PRTP
     C   60              MOVE      *BLANKS       C1IDES
     C   60              MOVE      *BLANKS       C1PDES
     C   60              MOVE      *BLANKS       C1CNTY
     C   60              MOVE      *BLANKS       C1CDS1
     C   60              MOVE      *BLANKS       C1CDS2
     C   60              MOVE      *BLANKS       C1CDS3
     C   60              MOVE      *ZEROS        C1PRAM
     C   60              MOVE      *ZEROS        C1HRDY
     C   60              MOVE      *ZEROS        C1HRWK
     C   60              MOVE      *ZEROS        C1DYWK
     C*
     C                   MOVE      CMSS#         XXSS#
     C                   MOVE      CMOFF#        XXOFF#
     C*
     C* Load Claimant Name
     C*
     C                   MOVEL(P)  CDLSN         XXCNAM           50
     C     XXCNAM        CAT       ',':0         XXCNAM
     C     XXCNAM        CAT       CDFSN:1       XXCNAM
     C     XXCNAM        CAT(P)    CDMID:1       XXCNAM
     C*
     C* Load claimant Date Of Birth (DOB)
     C*
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXCDOB            8 0
     C*
     C* Load the claimant address
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
     C* Load the accident location address information
     C*
     C*** BEG CHG *** 11/13/03 ****************************
     C***********          MOVE *BLANKS   XXACAD 80
     C                   MOVE      *BLANKS       XXACAD           54
     C*** END CHG *** 11/13/03 ****************************
     C                   MOVEL     CMACDR        XXACAD
     C     XXACAD        CAT       CMACTY:1      XXACAD
     C     XXACAD        CAT       ',':0         XXACAD
     C     XXACAD        CAT       CMAST:1       XXACAD
     C                   MOVE      CMACZ1        XXACZ1            5
     C     XXACAD        CAT       XXACZ1:1      XXACAD
     C     CMACZ2        IFNE      *ZERO
     C                   MOVE      CMACZ2        XXACZ2            4
     C     XXACAD        CAT       '-':0         XXACAD
     C     XXACAD        CAT       XXACZ2:0      XXACAD
     C                   ENDIF
     C*
     C* Load Claimant Social Security Number
     C*
     C     CDSS#         IFNE      *BLANKS
     C                   MOVE      CDSS#         XXCLSS
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'XXCLSS'      QQNAME           25
     C     XXCLSS        PARM      XXCLSS        QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C                   MOVEL     XXCLS1        XXCSS#           11
     C     XXCSS#        CAT       '-':0         XXCSS#
     C     XXCSS#        CAT       XXCLS2:0      XXCSS#
     C     XXCSS#        CAT       '-':0         XXCSS#
     C                   MOVE      XXCLS3        XXCSS#
     C                   ELSE
     C                   MOVE      *BLANKS       XXCLSS
     C                   MOVE      *BLANKS       XXCSS#
     C                   ENDIF
     C*
     C* Load Claimant Phone Number
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
     C* Load Claimant sex
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
     C*** BEG ADD *** 11/13/03 ****************************
     C*
     C* Load Job classification code.
     C*
     C     CMCLAS        IFGT      *ZERO
     C                   MOVE      CMCLAS        XXCLAS            5 0
     C                   ELSE
     C                   MOVE      *ZERO         XXCLAS
     C                   ENDIF
     C*** END ADD *** 11/13/03 ****************************
     C*
     C* Load Claimant number of dependants
     C*
     C     CM#DEP        IFGT      *ZERO
     C                   MOVE      CM#DEP        XX#DEP
     C                   ELSE
     C                   MOVE      *ZERO         XX#DEP
     C                   ENDIF
     C*
     C* Load Accident County NamNI(County state based on place of acc)
     C*
     C     C1CNTY        IFNE      *BLANKS
     C     CMAST         CAT(P)    C1CNTY:0      XXKEY
     C     TBKY          CHAIN     PCN02                              60
     C     *IN60         IFEQ      *OFF
     C*** BEG CHG *** 01/26/06 ****************************
     C*                    MOVE CNDESC    XXCNDE 30
     C                   MOVE      CNDESC        XXCNDE           18
     C*** END CHG *** 01/26/06 ****************************
     C                   ELSE
     C                   MOVE      *BLANKS       XXCNDE
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL     *BLANKS       XXPNAM
     C                   MOVEL     *BLANKS       XXPAD1
     C                   MOVEL     *BLANKS       XXPCSZ
      ***** BEG  ADD 01/26/06 *****************************************
     C                   MOVEL     *BLANKS       XXPCSZ
      ***** END  ADD 01/26/06 *****************************************
     C                   MOVE      *BLANKS       XXAREA
     C                   MOVEL     *BLANKS       XXPPH#
     C*
     C* Load Time Workday begins
     C*
     C                   MOVE      NI5N1         XXTWDB
     C     NI1A1         IFEQ      'A'
     C                   MOVE      'X'           XXAM              1
     C                   MOVE      *BLANK        XXPM              1
     C                   ELSE
     C     NI1A1         IFEQ      'P'
     C                   MOVE      'X'           XXPM
     C                   MOVE      *BLANK        XXAM
     C                   ELSE
     C                   MOVE      *BLANK        XXAM
     C                   MOVE      *BLANK        XXPM
     C                   ENDIF
     C                   ENDIF
     C*
     C* Load Answer to Did employee work the next day
     C*
     C     NI1A2         IFEQ      'Y'
     C                   MOVE      'X'           XXWNDY            1
     C                   MOVE      *BLANK        XXWNDN            1
     C                   ELSE
     C     NI1A2         IFEQ      'N'
     C                   MOVE      'X'           XXWNDN
     C                   MOVE      *BLANK        XXWNDY
     C                   ELSE
     C                   MOVE      *BLANK        XXWNDN
     C                   MOVE      *BLANK        XXWNDY
     C                   ENDIF
     C                   ENDIF
     C*
     C* Load Answer to did employee receive full pay for date of injury
     C*
     C     CMPOI         IFEQ      'Y'
     C                   MOVE      'X'           XXPOIY            1
     C                   MOVE      *BLANK        XXPOIN            1
     C                   ELSE
     C     CMPOI         IFEQ      'N'
     C                   MOVE      'X'           XXPOIN
     C*** BEG CHG *** 11/13/03 ****************************
     C***********          MOVE *BLANK    XXPOIN
     C                   MOVE      *BLANK        XXPOIY
     C*** END CHG *** 11/13/03 ****************************
     C                   ELSE
     C                   MOVE      *BLANK        XXPOIY
     C                   MOVE      *BLANK        XXPOIN
     C                   ENDIF
     C                   ENDIF
     C*
     C* Load Hours worked per day and per week and # of days
     C*
     C     CMHRSD        IFGT      *ZERO
     C                   MOVE      CMHRSD        XXHRSD            3 0
     C                   ELSE
     C                   MOVE      *ZERO         XXHRSD
     C                   ENDIF
     C*
     C     CMHRSD        IFGT      *ZERO
     C     CMDAYS        ANDGT     *ZERO
     C     CMDAYS        MULT      CMHRSD        XXHPWK            3 0
     C                   ELSE
     C                   MOVE      *ZEROS        XXHPWK
     C                   ENDIF
     C*
     C     CMDAYS        IFGT      *ZERO
     C                   MOVE      CMDAYS        XXDAYS            1
     C                   ELSE
     C                   MOVE      *BLANKS       XXDAYS
     C                   ENDIF
     C*
     C* Load normally scheduled off days
     C*
     C     NI20A1        IFNE      *BLANKS
     C                   MOVE      NI20A1        XXDYOF
     C                   ELSE
     C                   MOVE      *BLANKS       XXDYOF
     C                   ENDIF
     C*
     C* Load Pre-injury wages (weekly)
     C*
     C     CMPRWW        IFGT      *ZERO
     C                   Z-ADD     CMPRWW        XXPAWW
     C                   MOVE      'X'           XXPWWF            1
     C                   ELSE
     C                   MOVE      *ZERO         XXPAWW
     C                   MOVE      *BLANK        XXPWWF
     C                   ENDIF
     C*
     C* Load wages (hourly), or if boarding
     C*
     C     CMAWW         IFGT      *ZERO
     C                   Z-ADD     CMAWW         XXAWW
     C                   ELSE
     C                   MOVE      *ZERO         XXAWW
     C                   ENDIF
     C*
     C* Determine if Injury occurred on Employer's premises
     C*
     C     CMEADR        IFEQ      'Y'
     C                   MOVE      'X'           XEADRY            1
     C                   MOVE      *BLANKS       XEADRN            1
     C                   ELSE
     C     CMEADR        IFEQ      'N'
     C                   MOVE      *BLANKS       XEADRY
     C                   MOVE      'X'           XEADRN
     C                   ELSE
     C                   MOVE      *BLANKS       XEADRY
     C                   MOVE      *BLANKS       XEADRN
     C                   ENDIF
     C                   ENDIF
      *
      * Determine the Nature of Injury code and get the description
      *
     C                   MOVEL(P)  CMNATR        XXKEY
      *
     C     TBKY          CHAIN     PNA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  NADESC        XXNADS           19
     C                   ELSE
     C                   MOVE(P)   *BLANK        XXNADS
     C                   ENDIF
      *
      * Identify parts of body involved in injury or illness
      *
     C                   MOVEL(P)  CMPART        XXKEY
      *
     C     TBKY          CHAIN     PPA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  PADESC        XXBPRT           19
     C                   ELSE
     C                   MOVE(P)   *BLANK        XXBPRT
     C                   ENDIF
      *
      * Concatenate Accident Description fields for print
      *
     C     CMACDS        IFNE      *BLANKS
     C     CMACD2        ORNE      *BLANKS
     C*** BEG CHG *** 11/13/03 ****************************
     C*******              MOVELCMACDS    XXACDS 50
     C*******              SUBSTCMACDS:51 XXACD2 70 P
     C*******    XXACD2    CAT  CMACD2:0  XXACD2
     C                   MOVEL(P)  CMACDS        XXACDS           60
     C                   MOVEL(P)  CMACD2        XXACD2           60
     C*** END CHG *** 11/13/03 ****************************
     C                   ELSE
     C                   MOVE      *BLANKS       XXACDS
     C                   MOVE      *BLANKS       XXACD2
     C                   ENDIF
     C*
     C* Load Post-injury wage (weekly)
     C*
     C     CMPOWW        IFGT      *ZERO
     C                   Z-ADD     CMPOWW        XXPOWW
     C                   ELSE
     C                   MOVE      *ZERO         XXPOWW
     C                   ENDIF
     C*
     C* Load Post-injury wage (weekly)
     C*
     C     CMDTH         IFEQ      'Y'
     C     CMDOD         ANDGT     *ZERO
     C                   Z-ADD     CMDOD         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXDOD             8 0
     C                   ELSE
     C                   MOVE      *ZEROS        XXDOD
     C                   ENDIF
     C*
     C* Determine type of initial treatment and check the right box
     C*
     C                   MOVE      *BLANK        XITRT0            1
     C                   MOVE      *BLANK        XITRT1            1
     C                   MOVE      *BLANK        XITRT2            1
     C                   MOVE      *BLANK        XITRT3            1
     C                   MOVE      *BLANK        XITRT4            1
     C                   SELECT
     C     CMITRT        WHENEQ    '0'
     C                   MOVE      'X'           XITRT0
     C     CMITRT        WHENEQ    '1'
     C                   MOVE      'X'           XITRT1
     C     CMITRT        WHENEQ    '2'
     C                   MOVE      'X'           XITRT2
     C     CMITRT        WHENEQ    '3'
     C                   MOVE      'X'           XITRT3
     C     CMITRT        WHENEQ    '4'
     C                   MOVE      'X'           XITRT4
     C                   ENDSL
     C*
     C*  Determine if this is an MCO claim
     C*
     C                   MOVE      *BLANK        XXMCOY            1
     C                   MOVE      *BLANK        XXMCON            1
     C     CMMCO         IFEQ      'Y'
     C                   MOVE      'X'           XXMCOY
     C                   ELSE
     C                   MOVE      'X'           XXMCON
     C                   ENDIF
     C*
     C* GET TPA/CLAIM OFFICE
     C*
     C     OFKY          CHAIN     POF01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      OFNAME        XXOFNM
     C                   MOVEL     OFADR1        XXOFAD
     C                   MOVEL     OFCTY         XXOFCY           15
     C                   MOVE      OFST          XXOFST
     C                   MOVE      OFZIP         XXOFZP
     C                   MOVE      OFAREA        XXOFAR
     C                   MOVE      OFPHN#        XXOFPH
     C                   ELSE
     C                   MOVE      *BLANKS       XXOFNM
     C                   MOVE      *BLANKS       XXOFAD
     C                   MOVE      *BLANKS       XXOFCY
     C                   MOVE      *BLANKS       XXOFST
     C                   MOVE      *ZERO         XXOFZP
     C                   ENDIF
     C*
     C* GET TPA FEIN #
     C*
     C     COKY          CHAIN     PCO01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      COFEIN        XXCOFE
     C                   ELSE
     C                   MOVE      *ZEROS        XXCOFE
     C                   ENDIF
     C*
     C*** BEG ADD ****** 10/04/07 ****************************************
     C                   MOVEL     XXCOFE        PRCOFE
     C*** END ADD ****** 10/04/07 ****************************************
     C*
     C* Get Treating Physician's name and address
     C*
     C     TPKY          SETGT     PTP01
     C     TPKY          READPE    PTP01                                  60
     C     *IN60         IFEQ      *OFF
     C     TPPHYN        IFNE      *BLANKS
     C                   MOVEL     TPPHYN        XXPNAM
     C                   MOVEL     TPPHYA        XXPAD1
     C                   MOVEL(P)  TPPHYC        XXPCSZ
     C     XXPCSZ        CAT       ',':0         XXPCSZ
     C     XXPCSZ        CAT       TPPHYS:1      XXPCSZ
     C                   MOVE      TPPHYZ        XXTPSZ
      *
     C                   MOVE      *BLANKS       XTPZIP           10
     C     XTPSZ1        IFNE      *ZERO
     C     XTPSZ2        ANDNE     *ZERO
     C     XTPSZ1        CAT       '-':0         XTPZIP
     C     XTPZIP        CAT       XHOSZ2:0      XTPZIP
     C                   ELSE
     C     XTPSZ1        IFGT      *ZERO
     C                   MOVEL(P)  XTPSZ1        XTPZIP
     C                   ENDIF
     C                   ENDIF
      *
     C                   MOVE      TPPHYR        XXAREA
     C                   MOVEL     TPPHYP        XXPPH#
     C                   MOVE      TPPHYP        XXPPH#
     C                   ENDIF
     C                   ENDIF
     C*
     C* Get Name and Address of hospital
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
      ***** BEG  ADD  01/26/06  ***************************************
     C                   MOVEL(P)  PDCTY         XXHCSZ
     C     XXHCSZ        CAT       ',':0         XXHCSZ
     C     XXHCSZ        CAT       PDST:1        XXHCSZ
      ***** BEG  ADD  01/26/06  ***************************************
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
     C                   MOVE      CMHOSN        XXHOSN
     C                   MOVE      CMHOSA        XXHOSA
     C                   MOVE      CMHOSC        XXHOSC
     C                   MOVE      CMHOSS        XXHOSS
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
     C* Get Preparer's names of hospital
     C*
     C     CMCLEX        IFNE      *BLANKS
     C                   MOVEL(P)  CMCLEX        XXPREP           40
     C                   ELSE
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C     UPUSNM        ANDNE     *BLANKS
     C                   MOVE      UPUSNM        XXPREP
     C                   ELSE
     C     CMSUPR        IFNE      *BLANKS
     C                   MOVEL(P)  CMSUPR        XXPREP
     C                   ELSE
     C                   MOVE      *BLANKS       XXPREP
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C*** BEG DEL *** 07/25/06 **************************************
     C*** BEG ADD *** 06/28/06 **************************************
     C*********  CLTID     IFEQ '091'
     C*********
     C*********            SELEC
     C*********  XXFND     WHEQ 034
     C*********            MOVELXNAME1    XXPREP    P
     C*********
     C*********  XXFND     WHEQ 022
     C*********            MOVELXNAME2    XXPREP    P
     C*********            ENDSL
     C*********            ENDIF
     C*** END ADD *** 06/28/06 **************************************
     C*** END DEL *** 07/25/06 **************************************
     C*
     C* Get Preparer's Title
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      UPTITL        XXTITL
     C                   ELSE
     C                   MOVE      *BLANKS       XXTITL
     C                   ENDIF
     C*
     C* Indicate report date
     C*
     C                   MOVE      DSDATE        XXRPDT            8 0
     C*
     C*  Load weekly benefit amount
     C*
     C     NI5N2         IFGT      *ZERO
     C                   Z-ADD     NI5N2         XXWKBN
     C                   ELSE
     C                   MOVE      *ZERO         XXWKBN
     C                   ENDIF
     C*
     C* Set Date disability began
     C*
     C     CMDBDT        IFGT      *ZERO
     C                   Z-ADD     CMDBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXDT1
     C                   ELSE
     C                   MOVE      *ZERO         XXDT1
     C                   ENDIF
     C*
     C* Get Date of first payment; find the first comp pmt where
     C* payment type = C and retrive the payment amount
     C*
     C                   MOVE      *ZEROS        XXPADT
     C                   MOVE      *ZEROS        XXAAMT
     C     PAKY          CHAIN     PPA02                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     PATYPE        IFEQ      'C'
     C                   Z-ADD     PATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXPADT            8 0
     C                   MOVE      PAAMT         XXAAMT
     C                   LEAVE
     C                   ENDIF
     C     PAKY          READE     PPA02                                  60
     C                   ENDDO
     C*
     C* Calculate penalty paid amount using all records where PABR2
     C* placenment type = 'A'
     C*
     C                   MOVE      *ZEROS        XPNLTY           11 2
     C     PAKY          SETLL     PPA02                                  60
     C     PAKY          READE     PPA02                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  PABR2         XXKEY
      *
     C     TBKY          CHAIN     PB201                              60
     C     *IN60         IFEQ      *OFF
     C     B2PLCD        IFEQ      'A'
     C                   ADD       PAAMT         XPNLTY
     C                   ENDIF
     C                   ENDIF
     C     PAKY          READE     PPA02                                  60
     C                   ENDDO
     C*
     C* Calculate penalty paid amount using all records where PABR2
     C* placenment type = 'A'
     C*
     C                   MOVE      *BLANKS       XXTHY             1
     C                   MOVE      *BLANKS       XXTHN             1
     C     CMLTFL        IFEQ      'I'
     C     THKY          CHAIN     PTH01                              60
     C     *IN60         DOWEQ     *OFF
     C     THSTAT        IFEQ      'M'
     C                   MOVE      'X'           XXTHY
     C                   LEAVE
     C                   ENDIF
     C     THKY          READE     PTH01                                  60
     C                   ENDDO
     C     XXTHY         IFEQ      *BLANK
     C                   MOVE      'X'           XXTHN
     C                   ENDIF
     C                   ENDIF
     C*
     C* Load Benefits payable dates and type of payments
     C*
     C     NIDT1         IFGT      *ZERO
     C                   Z-ADD     NIDT1         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPAYF            8 0
     C                   ENDIF
     C*
     C     NIDT2         IFGT      *ZERO
     C                   Z-ADD     NIDT2         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPAYT            8 0
     C                   ENDIF
      *
     C                   MOVE      *BLANK        XXTT              1
     C                   MOVE      *BLANK        XXTP              1
     C                   MOVE      *BLANK        XXPP              1
     C     NI10A2        IFEQ      'TT'
     C                   MOVE      'X'           XXTT
     C                   ELSE
     C     NI10A2        IFEQ      'TP'
     C                   MOVE      'X'           XXTP
     C                   ELSE
     C     NI10A2        IFEQ      'PP'
     C                   MOVE      'X'           XXPP
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     NI3N1         IFGT      *ZERO
     C                   MOVE      NI3N1         XXPERC
     C                   ELSE
     C                   MOVE      *ZERO         XXPERC
     C                   ENDIF
     C*
     C     NI10A3        IFNE      *BLANKS
     C                   MOVE      NI10A3        XXPART
     C                   ELSE
     C                   MOVE      *BLANKS       XXPART
     C                   ENDIF
     C*
     C     NI3N2         IFGT      *ZERO
     C                   MOVE      NI3N2         XXWKS
     C                   ELSE
     C                   MOVE      *ZERO         XXWKS
     C                   ENDIF
     C*
     C*  Part C fields Controvert reason and authorized by
     C*
     C     NI60A1        IFNE      *BLANKS
     C                   MOVE      NI60A1        XXCONT
     C                   ELSE
     C                   MOVE      *BLANKS       XXCONT
     C                   ENDIF
     C*
     C*  PRINT 'MC' ON LABEL FOR MANAGED CARE CLAIMS
     C*
     C                   MOVE      *BLANKS       XXMC              2
     C     CMMCO         IFEQ      'Y'
     C                   MOVEL     'MC'          XXMC
     C                   ENDIF
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
     C                   MOVEL(P)  SRDTO         XXDTOB
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
     C                   ENDIF
     C*
     C*  GET THE EMPLOYER INFORMATION
     C*
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      EMNAME        XXENAM
     C                   MOVEL     EMNAOB        XXNAOB
     C*
     C     EMMAD1        IFEQ      *BLANKS
     C                   MOVE      EMPAD1        EMMAD1
     C                   MOVE      EMPAD2        EMMAD2
     C                   MOVE      EMPCTY        EMMCTY
     C                   MOVE      EMPST         EMMST
     C                   Z-ADD     EMPZIP        EMMZIP
     C                   ENDIF
     C     EMPAD1        IFEQ      *BLANKS
     C                   MOVE      EMMAD1        EMPAD1
     C                   MOVE      EMMAD2        EMPAD2
     C                   MOVE      EMMCTY        EMPCTY
     C                   MOVE      EMMST         EMPST
     C                   Z-ADD     EMMZIP        EMPZIP
     C                   ENDIF
      *
      *****  BEG ADD 01/26/06 *****************************************
      *
      * Find the e-mail address for this employer
      *
     C                   MOVEL     '1'           ##PCNT
     C                   MOVEL     *BLANKS       XXEMAL           40
     C     ECKY          CHAIN     PEC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL(P)  ECEMAL        XXEMAL
     C                   ENDIF
      *
      *****  END ADD 01/26/06 *****************************************
      *
     C                   MOVEL     EMPCTY        XXECTY
     C                   MOVEL     EMMCTY        XXCTY
     C                   MOVE      EMMST         XXSTAT
     C                   Z-ADD     EMMZIP        XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         XXECSZ
     C     EMMAD2        COMP      *BLANKS                                19
     C                   ENDIF
     C*
     C*  GET THE LOCATION INFORMATION, IF ANY.
     C*
     C     ALKY          CHAIN     PAL01                              60
     C     *IN60         IFEQ      '0'
     C     ALMAD1        IFEQ      *BLANKS
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
     C                   ENDIF
     C                   ENDIF
     C*
     C*** BEG ADD ****** 10/04/07 ****************************************
     C*  LOAD ACCIDENT COUNTY.
     C*
     C     CMCTG2        IFNE      *BLANKS
     C                   MOVEL(P)  CMCTG2        XXCNTY
     C                   ELSE
     C*
     C     ALCNTY        IFNE      *BLANKS
     C     ALPST         IFNE      *BLANKS
     C     ALPST         CAT(P)    ALCNTY:0      XXKEY
     C                   ELSE
     C     ALMST         CAT(P)    ALCNTY:0      XXKEY
     C                   ENDIF                                                  ALPST<>*BLANKS
     C                   ELSE
     C*
     C     EMPST         IFNE      *BLANKS
     C     EMPST         CAT(P)    EMCNTY:0      XXKEY
     C                   ELSE
     C     EMMST         CAT(P)    EMCNTY:0      XXKEY
     C                   ENDIF                                                  EMPST<>*BLANKS
     C*
     C                   ENDIF
     C*
     C                   MOVEL     *BLANKS       XXCNTY
     C     TBKY          CHAIN     PCN01                              60
     C  N60              MOVEL(P)  CNDESC        XXCNTY
     C*
     C                   ENDIF
     C*** END ADD ****** 10/04/07 ****************************************
     C*
     C*  SET INDICATORS FOR ALL BOXES THAT GET MARKED.
     C*
     C     CMFSAL        COMP      'Y'                                    23
     C     CMRTW         COMP      *ZEROS                                 24
     C     C1PRTP        COMP      'H'                                    25
     C     C1PRTP        COMP      'W'                                    26
     C     C1PRTP        COMP      'D'                                    27
     C     C1PRTP        COMP      'M'                                    28
     C     CMEAGR        COMP      'N'                                    29
     C     C1EMAU        COMP      'N'                                    30
     C     C1EMAU        COMP      'Y'                                    45
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
     C*
     C                   Z-ADD     CMFSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXDT2
     C*
     C                   Z-ADD     NIDT4         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXDT4
     C*
     C                   Z-ADD     CMCRT         XX7N2
     C     NI40A1        CAT       NI40A2:0      XX80A1
     C*
     C*
     C*  GET THE INITIAL REQUESTOR NAME
     C*
     C                   MOVE      *BLANKS       XXUSER           10
     C                   MOVEL     CMIUSR        XXUSER
     C*
     C*** BEG ADD *** 07/25/06 **************************************
     C     CLTID         IFEQ      '091'
     C*
     C                   SELECT
     C     XXFND         WHENEQ    034
     C                   MOVEL(P)  '40008'       NI5N3
     C                   MOVEL(P)  XNAME1        XXPREP
     C*
     C     XXFND         WHENEQ    022
     C                   MOVEL(P)  '10144'       NI5N3
     C                   MOVEL(P)  XNAME2        XXPREP
     C                   ENDSL
     C                   ENDIF
     C*** END ADD *** 07/25/06 **************************************
     C*
     C*** BEG DEL *** 11/20/06 **************************************
     C******  71                EXCPTNOI
     C****** N71                EXCPTNOI2
     C*** END DEL *** 11/20/06 **************************************
     C*** BEG ADD *** 11/20/06 **************************************
     C                   EXSR      @@MOVE
     C                   WRITE     NOI3
     C                   WRITE     NOI4
     C                   WRITE     NOI5
     C*** END ADD *** 11/20/06 **************************************
     C*
     C*  PRINT A LABEL FOR THE EMPLOYEE COPY ONLY.
     C*
     C   72              EXCEPT    LABEL
     C*
     C*  CLOSE THE LABEL SPOOL FILE IF THE EMPLOYEE COPY WAS PROCESSED.
     C*
     C     *IN73         IFEQ      *ON
     C     XXOPN2        ANDEQ     *ON
     C*** BEG DEL *** 11/20/06 **************************************
     C******               CLOSEQSYSPRT2
     C*** END DEL *** 11/20/06 **************************************
     C                   MOVE      *OFF          XXOPN2            1
     C                   ENDIF
     C*
     C*  WRITE THE DARTS REMARK, THEN PRINT THE NOI FORM.
     C*
     C     *IN75         IFEQ      *OFF
     C  N73              EXSR      DARTS
     C   73              EXSR      SRDART
     C                   ENDIF
     C*
     C*  GET THE NEXT RECORD
     C*
     C     SKIP          TAG
     C   71NIKY          READE     PNI01                                  50
     C   72NIKY          READE     PNI02                                  50
     C   73LGKY          READE     PLG01                                  50
     C   75              MOVE      *ON           *IN50
     C                   END
     C*
     C*  CLOSE THE LABEL SPOOL FILE IF THE EMPLOYEE COPY WAS PROCESSED.
     C*
     C   71XXOPEN        IFEQ      *ON
     C*** BEG DEL *** 11/20/06 **************************************
     C******               CLOSEQSYSPRT
     C*** END DEL *** 11/20/06 **************************************
     C                   MOVE      *OFF          XXOPEN            1
     C                   ENDIF
     C   72XXOPN1        IFEQ      *ON
     C                   CLOSE     QSYSPRT1
     C                   MOVE      *OFF          XXOPN1            1
     C                   ENDIF
     C   72XXOPN2        IFEQ      *ON
     C*** BEG DEL *** 11/20/06 **************************************
     C******               CLOSEQSYSPRT2
     C*** ENG DEL *** 11/20/06 **************************************
     C                   MOVE      *OFF          XXOPN2            1
     C                   ENDIF
     C   75XXOPN2        IFEQ      *ON
     C*** BEG DEL *** 11/20/06 **************************************
     C******               CLOSEQSYSPRT2
     C*** END DEL *** 11/20/06 **************************************
     C                   MOVE      *OFF          XXOPN2
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
     C                   MOVE      *BLANKS       XXCCTY           18
     C                   MOVE      *BLANKS       XXENAM           40
     C                   MOVE      *BLANKS       XXNAOB           40
     C                   MOVE      *BLANKS       XXECTY           18
     C                   MOVE      *BLANKS       XXLAD1           40
     C                   MOVE      *BLANKS       XXLAD2           40
     C                   MOVE      *BLANKS       XXLCTY           17
     C                   MOVE      *BLANKS       XXLST             2
     C                   MOVE      *BLANKS       XXLZIP            9 0
     C                   MOVE      *BLANKS       XXACTY           18
     C                   MOVE      *BLANKS       XXPNAM           35
     C                   MOVE      *BLANKS       XXPAD1           35
     C                   MOVE      *BLANKS       XXPCSZ           25
      ***** BEG  ADD  01/26/06 ****************************************
     C                   MOVE      *BLANKS       XXHCSZ           25
      ***** BEG  ADD  01/26/06 ****************************************
     C                   MOVE      *BLANKS       XXAREA            3 0
     C                   MOVE      *BLANKS       XXPPH#            7 0
     C                   MOVE      *BLANKS       XXENAM           40
     C                   MOVE      *BLANKS       XXECSZ           40
     C                   Z-ADD     0             XXRCVD            8 0
     C                   Z-ADD     0             XXACDT            8 0
     C                   Z-ADD     0             XXACTM
     C                   Z-ADD     0             XXDTOB            8 0
     C                   Z-ADD     0             XXDTRP            8 0
     C                   Z-ADD     0             XXDOH             8 0
     C                   Z-ADD     0             XXDTLW            8 0
     C                   Z-ADD     0             XXFSDT            8 0
     C                   Z-ADD     0             XXIRTN            8 0
     C                   Z-ADD     0             XXDOD
     C                   Z-ADD     0             XXDT1             8 0
     C                   Z-ADD     0             XXDT2             8 0
     C                   Z-ADD     0             XXDT4             8 0
     C                   Z-ADD     0             XX7N1             7 2
     C                   Z-ADD     0             XX7N2             7 2
     C                   MOVE      *BLANKS       XX40A1           40
     C                   MOVE      *BLANKS       XXCD1             2
     C                   MOVE      *BLANKS       XX80A1           80
     C*
     C     *IN75         IFEQ      *OFF
     C     *IN73         IFEQ      *OFF
     C                   Z-ADD     NIFND         XXFND
     C                   Z-ADD     NIFYR         XXFYR
     C                   Z-ADD     NICASE        XXCASE
     C                   ELSE
     C                   Z-ADD     LGFND         XXFND
     C                   Z-ADD     LGFYR         XXFYR
     C                   Z-ADD     LGCASE        XXCASE
     C                   END
     C                   END
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
     C     FNKY          CHAIN     PCX01                              60
     C*
     C*   GET REMARK CODE FOR DIVISION DIARY REMARKS.
     C*
     C                   MOVE      *BLANKS       XXRMCD
     C  N60              MOVE      CXRSRD        XXRMCD
     C*
     C*  GET THE FUND NAME AND ADDRESS
     C*
     C                   MOVE      *BLANKS       XXCONM           40
     C                   MOVE      *BLANKS       XCADR1           40
     C                   MOVE      *BLANKS       XCCSZ            40
     C                   MOVE      *BLANKS       XXFNAM
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      FNNAME        XXFNAM
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
     C                   Z-ADD     F2SCTC        XXSCTC
     C                   Z-ADD     F2AREA        XXFARA
     C                   Z-ADD     F2SPH#        XXFPH#
     C                   Z-ADD     F2FEIN        XXFEIN
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
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANKS       CMKEY
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      XXFND         QCFND
     C                   MOVE      XXFYR         QCFYR
     C                   MOVE      XXCASE        QCCASE
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
     C     C3KY          READPE    PCM03                                  60
     C     *IN60         IFEQ      '0'
     C     C3SEQ#        ADD       1             CMSEQ#
     C                   ELSE
     C                   Z-ADD     1             CMSEQ#
     C                   END
     C*
     C                   WRITE     PCM02
     C                   FEOD      SDCMLD
     C                   END
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
     C     CMRMCD        CHAIN     PCT01                              60
     C   60              GOTO      SKIP1
     C*
     C                   MOVE      CTSMCD        CMSMCD
     C                   MOVEL     CPY(3)        CMCOMM
     C*
     C*  GET FOLDER NAME
     C*
     C                   MOVE      *BLANKS       QQFND             8
     C                   MOVE      *BLANKS       QQSER             8
     C                   MOVE      *BLANKS       QQSUB             8
     C                   MOVE      *BLANKS       QQFOLD            8
     C                   MOVE      *BLANKS       QQFLDX            7
     C                   MOVE      *BLANKS       QQFPTH           40
     C                   MOVE      *BLANKS       QQTERM            1
     C*
     C                   MOVEL     XXFND         QQFND                          for Fund
     C                   MOVEL     'CS'          QQSER                          for Area of Service
     C                   MOVEL     CTTYPE        QQSUB                          for Sub-Area
     C*
     C                   MOVEL     'C'           QQFOLD                         means "Member"
     C                   MOVEL     CMCASE        QQFLDX                         CASE for Folder name
     C                   MOVE      QQFLDX        QQFOLD
     C*
     C                   CALL      'SRFBLD'
     C                   PARM                    QQFND
     C                   PARM                    QQSER
     C                   PARM                    QQSUB
     C                   PARM                    QQFOLD
     C                   PARM                    QQFPTH                         Built, returned
     C                   PARM                    QQTERM
     C*
     C                   MOVEL     QQFPTH        CMFOLD
     C*
     C*  GET DOCUMENT ID
     C*
     C                   MOVE      *BLANKS       QQRT              2
     C                   MOVE      *BLANKS       QQDOC            12
     C                   MOVE      *BLANKS       QQTERM            1
     C*
     C                   MOVEL     CTTYPE        QQRT                           Remark Type
     C                   CALL      'SRDBLD'
     C                   PARM                    QQFPTH                         From SRFBLD routine
     C                   PARM                    QQRT                           Rmrk Type for Doc ID
     C                   PARM                    QQDOC                          Built, returned
     C                   PARM                    QQTERM
     C*
     C                   MOVEL     QQDOC         CMDCID
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
     C                   MOVEL     'W0070R'      CMPNAM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMTRDT
     C                   Z-ADD     DSTIME        CMTRTM
     C                   MOVE      'N'           CMSBYL
     C                   MOVEL     LGIUSR        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
     C                   Z-ADD     DSTIME        CMTIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   MOVEL     LGIUSR        CMIUSR
     C                   MOVE      'WC'          CMAPID
     C                   MOVEL     Q1PGM         CMIPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMIDT
     C*
     C*  GET LAST SEQ# USED
     C*
     C     C3KY          SETGT     PCM03
     C     C3KY          READPE    PCM03                                  60
     C     *IN60         IFEQ      '0'
     C     C3SEQ#        ADD       1             CMSEQ#
     C                   ELSE
     C                   Z-ADD     1             CMSEQ#
     C                   END
     C*
     C                   WRITE     PCM02
     C                   FEOD      SDCMLD
     C*
     C*  LOAD THE DARTS SEQ# INTO THE EDI LOG FILE
     C*
     C     LGKY1         CHAIN     PLG02                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     CMSEQ#        LGPRSQ
     C                   UPDATE    PLG02
     C                   END
     C*
     C*  CALL W8070C1FL TO RESOLVE THE OUTPUT INTO AN OFFICE DOCUMENT
     C*
     C                   MOVE      CMDCID        QQDCID
     C                   MOVE      CMFOLD        QQFLDR
     C                   CALL      'W8070C1F'
     C                   PARM                    Q1JOB
     C                   PARM                    Q1USER
     C                   PARM                    Q1JOBN
     C                   PARM                    QQDCID
     C                   PARM                    QQFLDR
     C*
     C     SKIP1         TAG
     C                   ENDSR
     C*** BEG ADD *** 11/20/06 **************************************
     C*
     C*SUBROUTINE TO MOVE VARIABLE VALUES TO PRINTER FILE.
     C***************************************************************
     C     @@MOVE        BEGSR
     C                   MOVE      XXCSS#        PRCSS#
     C                   MOVE      XXACDT        PRACDT
     C                   MOVE      XXSEXM        PRSEXM
     C                   MOVE      XXSEXF        PRSEXF
     C                   MOVE      XXCDOB        PRCDOB
     C                   MOVE      XXCLAR        PRCLAR
     C                   MOVE      XXCLPH        PRCLPH
     C                   MOVE      NI40A3        PR40A3
     C                   MOVE      XXCLA1        PRCLA1
     C                   MOVE      XXCLCY        PRCLCY
     C                   MOVE      XXCLST        PRCLST
     C                   MOVE      XXCLZP        PRCLZP
     C                   MOVE      XXENAM        PRENAM
     C                   MOVE      EMNAIC        PRNAIC
     C                   MOVE      XXNAOB        PRNAOB
     C                   MOVE      EMMAD1        PRMAD1
     C                   MOVE      EMAREA        PRAREA
     C                   MOVE      EMPHN#        PRPHN#
     C                   MOVE      EMFEIN        PRFEIN
     C                   MOVE      EMMAD2        PRMAD2
     C*** BEG CHG ****** 10/04/07 ****************************************
     C                   MOVE      EMMCTY        PRMCTY
     C                   MOVE      EMMST         PRMST
     C                   MOVE      EMMZIP        PRMZIP
     C****************     MOVE XXECSZ    PRECSZ
     C*** END CHG ****** 10/04/07 ****************************************
     C                   MOVE      XXEMAL        PREMAL
     C                   MOVE      XXFNM1        PRFNM1
     C                   MOVE      XXOFA1        PROFA1
     C*** BEG DEL ****** 10/04/07 *********************************************
     C*****************    MOVE XXOFA2    PROFA2
     C*** END DEL ****** 10/04/07 *********************************************
     C                   MOVE      XXONM1        PRONM1
     C                   MOVE      XXOFCY        PROFCY
     C                   MOVE      XXOFST        PROFST
     C                   MOVE      XXOFZP        PROFZP
     C                   MOVE      XXFEIN        P@FEIN
     C                   MOVE      NI5N3         PR5N3
     C                   MOVE      CMFND         PRFND
     C                   MOVE      CMFYR         PRFYR
     C                   MOVE      CMCASE        PRCASE
     C                   MOVE      XXOFAR        PROFAR
     C                   MOVE      XXOFPH        PROFPH
     C                   MOVE      NI40A4        PR40A4
     C                   MOVE      XXDOH         PRDOH
     C                   MOVE      XXCLAS        PRCLAS
     C                   MOVE      XXDAYS        PRDAYS
     C                   MOVE      XXPAWW        PRPAWW
     C                   MOVE      XXPWWF        PRPWWF
     C*** BEG ADD ****** 10/04/07 *********************************************
     C                   MOVE      ' '           PRICDI
     C                   MOVE      ' '           PRICDS
     C                   MOVE      ' '           PRICDG
     C     *IN40         IFEQ      *OFF
     C                   MOVE      'X'           PRICDI
     C                   ELSE
     C                   MOVE      'X'           PRICDS
     C                   ENDIF                                                  *IN40
     C*** END ADD ****** 10/04/07 *********************************************
     C                   MOVE      XXDYOF        PRDYOF
     C                   MOVE      XXTIME        PRTIME
     C     *IN20         IFEQ      *ON
     C                   MOVE      'X'           PRTIPM
     C                   ELSE
     C                   MOVE      'X'           PRTIAM
     C                   ENDIF
     C                   MOVE      XXDTRP        PRDTRP
     C                   MOVE      XXDT1         PRDT1
     C                   MOVE      XXPOIY        PRPOIY
     C                   MOVE      XXPOIN        PRPOIN
     C                   MOVE      XEADRY        PRADRY
     C                   MOVE      XEADRN        PRADRN
     C                   MOVE      XXNADS        PRNADS
     C                   MOVE      XXBPRT        PRBPRT
     C                   MOVE      XXACDS        PRACDS
     C                   MOVE      XXACD2        PRACD2
     C                   MOVE      XXPNAM        PRPNAM
     C                   MOVE      XITRT0        PRTRT0
     C                   MOVE      XXHFSN        PRHFSN
     C                   MOVE      XXIRTN        PRIRTN
     C                   MOVE      XXPAD1        PRPAD1
     C                   MOVE      XXHOSN        PRHOSN
     C                   MOVE      XXPOWW        PRPOWW
     C                   MOVE      XXPCSZ        PRPCSZ
     C                   MOVE      XTPZIP        PRPZIP
     C                   MOVE      XXHOSA        PRHOSA
     C                   MOVE      XXAREA        PR@REA
     C                   MOVE      XXPPH#        PRPPH#
     C                   MOVE      XXHCSZ        PRHCSZ
     C                   MOVE      XXDOD         PRDOD
     C                   MOVE      XXPREP        PRPREP
     C                   MOVE      XXOFAR        PR@FAR
     C                   MOVE      XXOFPH        PR@FPH
     C                   MOVE      XXRPDT        PRRPDT
     C                   MOVE      XXTHY         PRTHY
     C                   MOVE      XXTHN         PRTHN
     C                   MOVE      XXAWW         PRAWW
     C                   MOVE      XXWKBN        PRWKBN
     C                   MOVE      XXDT1         PRDT1@
     C                   MOVE      XXPADT        PRPADT
     C                   MOVE      XXAAMT        PRAAMT
     C                   MOVE      XXPADT        PRPA@T
     C                   MOVE      XPNLTY        PRNLTY
     C                   MOVE      XXPAYF        PRPAYF
     C                   MOVE      XXTT          PRTT
     C                   MOVE      XXTP          PRTP
     C                   MOVE      XXPP          PRPP
     C                   MOVE      XXPERC        PRPERC
     C                   MOVE      XXPART        PRPART
     C                   MOVE      XXWKS         PRWKS
     C                   MOVE      XXPAYT        PRPAYT
     C                   MOVE      NI60A1        PR60A1
     C                   MOVE      *BLANKS       PRXXBK
     C                   ENDSR
     O*** END ADD *** 11/20/06 *****************************
     C******************************************************
     O*** BEG DEL *** 11/13/03 *****************************
 XXX O***QSYSPRT E   01           NOI
     O*******
     O******* E    5           NOI
     O*******                  CMFND    103
     O*******                           104 '-'
     O*******                  CMFYR    107
     O*******                           108 '-'
     O*******                  CMCASE   115
     O******* E    7           NOI
     O*******                  XXFNM1    90
     O*******                  XXONM1   115
     O******* E    8           NOI
     O*******                  XXENAM    44
     O*******                  EMAREA4   49
     O*******                  EMPHN#    58 '0   -    '
     O*******                  XXFNM2    90
     O*******                  XXONM2   115
     O******* E    9           NOI
     O*******                  EMMAD1    55
     O******* E   10           NOI
     O*******                  EMMAD2    55
     O*******                  EMFEIN    80 '    -  -   0'
     O*******                  XXCOFE   109 '0   -  -    '
     O******* E   12           NOI
     O*******                  XXOFA1   115
     O******* E   13           NOI
     O*******                  XXECSZ    44
     O*******                  XXNAOB    81
     O*******                  XXOFA2   115
     O******* E   14           NOI
     O*******                  XXOFCY   109
     O******* E   15           NOI
     O*******                  XXLAD1    50
     O*******                  XXLCTY    75
     O*******                  XXLST     78
     O*******                  XXLZIP    89 '     -    '
     O*******                  XXOFST   103
     O*******                  XXOFZP   115 '     -    '
     O******* E   16           NOI
     O*******                  XXLAD2    50
     O******* E   18           NOI
     O*******                  XXACAD    89
     O*******                  XXOFAR   100 '0(   )'
     O*******                  XXOFPH   109 '0   -    '
     O******* E   20           NOI
     O*******                  XXCNAM    55
     O*******                  XXCDOB    80 '  /  /    '
     O*******                  XXCNDE   120
     O******* E   22           NOI
     O*******                  XXCLA1    52
     O******* E   23           NOI
     O*******                  XXCLA2    52
     O*******                  XXACDT    80 '  /  /    '
     O*******                  XXCSS#   110
     O******* E   25           NOI
     O*******                  XXCLCY    33
     O*******                  XXCLAR    51 '0(   )'
     O*******                  XXCLPH    60 '0   -    '
     O*******                  XX#DEPZ   80
     O******* E   26           NOI
     O*******                  XXCLST    27
     O*******                  XXCLZP    38 '0     -    '
     O******* E   27           NOI
     O*******                  XXTWDB    66 '   :  '
     O******* E   28           NOI
     O*******                  XXSEXM    12
     O*******                  XXSEXF    20
     O*******                  XXTIME    37 '  :  '
     O*******         N20                40 'am'
     O*******          20                40 'pm'
     O*******                  XXAM      48
     O*******                  XXPM      60
     O*******                  XXDTRP    90 '  /  /    '
     O******* E   31           NOI
     O*******                  XXDOH     20 '  /  /    '
     O*******                  XXDT1     76 '  /  /    '
     O******* E   32           NOI
     O*******                  XXWNDY    29
     O*******                  XXWNDN    46
     O*******                  XXPOIY    79
     O*******                  XXPOIN    91
     O******* E   34           NOI
     O*******                  XXHRSDZ   15
     O******* E   35           NOI
     O*******                  XXHPWKZ   15
     O*******                  XXDAYS    38
     O*******                  XXDYOF    67
     O*******                  XXPAWWK   80
     O*******                  XXPWWF    93
     O******* E   38           NOI
     O*******                  XXAWW K   15
     O*******                  XXAWW K   70
     O******* E   40           NOI
     O******* E   41           NOI
     O*******                  XEADRY    26
     O*******                  XEADRN    41
     O*******                  XXNADS    79
     O*******                  XXBPRT    99
     O******* E   43           NOI
     O*******                  XXACDS    97
     O******* E   44           NOI
     O*******                  XXACD2    97
     O******* E   46           NOI
     O*******                  XXIRTN    20 '  /  /    '
     O*******                  XXPOWW2   38 '$'
     O*******                  XXDOD     96 '  /  /    '
     O******* E   49           NOI
     O*******                  XXPNAM    45
     O*******                  XITRT0    55
     O*******                  XXHFSN    84
     O******* E   50           NOI
     O*******                  XXPAD1    45
     O*******                  XITRT1    55
     O*******                  XXHNM1    96
     O******* E   51           NOI
     O*******                  XXPCSZ    35
     O*******                  XTPZIP    46
     O*******                  XITRT2    55
     O*******                  XXHNM2    96
     O******* E   52           NOI
     O*******                  XXHAD1    96
     O******* E   53           NOI
     O*******                  XXAREA4   14
     O*******                  XXPPH#    23 '    -    '
     O*******                  XITRT3    55
     O*******                  XXHAD2    96
     O******* E   54           NOI
     O*******                  XITRT4    55
     O*******                  XXHOSC   101
     O******* E   56           NOI
     O*******                  XXMCOY    64
     O*******                  XXMCON    69
     O*******                  XXHOSS    78
     O*******                  XHZIP     90
     O******* E   58           NOI
     O*******                  XXPREP    47
     O*******                  XXTITL    70
     O*******                  XXOFAR    91 '0(   )'
     O*******                  XXOFPH   100 '0   -    '
     O*******                  XXRPDT   115 '  /  /    '
     O******* E   62           NOI
     O*******                  XXAWW K   31
     O*******                  XXWKBNK   55
     O*******                  XXDT1     84 '  /  /    '
     O*******                  XXPADT   115 '  /  /    '
     O******* E   64           NOI
     O*******                  XXAAMTK   30
     O*******                  XPNLTYK   57
     O*******                  XXTHY    110
     O*******                  XXTHN    117
     O******* E   65           NOI
     O*******                  XXPAYF    45 '  /  /    '
     O******* E   67           NOI
     O*******                  XXTT       6
     O*******                  XXTP      31
     O*******                  XXPP      53
     O*******                  XXPERCZ   80
     O*******                  XXPART    99
     O*******                  XXWKS Z  109
     O******* E   70           NOI
     O*******                  XXPAYT    20 '  /  /    '
     O******* E   73           NOI
     O*******                  XXPREP    60
     O******* E   77           NOI
     O*******                  XXCONT    90
     O******* E   79           NOI
     O*******                  XXPREP    60
     O******* E   83           NOI
     O*******                  XXCPY     62
     O*******                  XXUSER   115
     O******* E    1           NOI
     O*** END DEL *** 11/13/03 *****************************
     O*** BEG ADD *** 04/07/06 *****************************
     O*** BEG DEL *** 11/20/06 **************************************
     O******QSYSPRT E   01           NOI
      *
      * Employee
     O******  E   14           NOI
     O******                   XXSEXM    98
     O******  E   15           NOI
     O******                   CDLSN     55
     O******                   CDFSN     71
     O******                   CDMID     70
     O******                   XXSEXF    98
     O******                   XXCDOB   121 '  /  /    '
     O******  E   17           NOI
     O******                   XXCLA1    52
     O******                   XXCLAR    81 '0(   )'
     O******                   XXCLPH    90 '0   -    '
     O******                   XXCSS#   121
     O******  E   18           NOI
     O******                   XXCLA2    52
     O******  E   19           NOI
     O******                   XXCLCY    37
     O******                   XXCLST    40
     O******                   XXCLZP    60 '0     -    '
     O******                   NI40A3   111
      *
      * Employer
     O******  E   21           NOI
     O******                   XXENAM    65
     O******                   EMAREA4   50
     O******                   EMNAIC    82
     O******                   XXNAOB   132
     O******  E   23           NOI
     O******                   EMMAD1    52
     O******                             77 '('
     O******                   EMAREA4   80
     O******                             81 ')'
     O******                   EMPHN#    90 '0   -    '
     O******                   EMFEIN   121 '0   -  -    '
     O******  E   24           NOI
     O******                   EMMAD2    52
     O******  E   25           NOI
     O******                   XXECSZ    52
     O******                   XXEMAL   111
      *
      * Insurer/Self Insurer
     O******  E   27           NOI
     O******                   XXFNM1    60
     O******                   XXOFA1    91
     O******  E   28           NOI
     O******                   XXOFA2    90
     O******  E   29           NOI
     O******                   XXONM1    50
     O******                   XXOFCY    86
     O******                   XXOFST   103
     O******                   XXOFZP   115 '     -    '
     O******  E   31           NOI
     O******                   NI5N3     25
     O******                   CMFND     39
     O******                             40 '-'
     O******                   CMFYR     43
     O******                             44 '-'
     O******                   CMCASE    51
     O******                   XXOFAR    65 '0(   )'
     O******                   XXOFPH    74 '0   -    '
     O******                   NI40A4   125
      * Employment/Wage
     O******  E   34           NOI
     O******                   XXDOH     42 '  /  /    '
     O******                   XXCLAS    59
     O******                   XXDAYS    85
     O******                   XXPAWWK  113
     O******                   XXPWWF   117
     O******  E   36           NOI
     O******                   XXDYOF    32
      *
      * Injury/Illness & Medical
     O******  E   38           NOI
     O******          N20                56 'X'
     O******  E   39           NOI
     O******                   XXACDT    42 '  /  /    '
     O******                   XXTIME    53 '  :  '
     O******           20                56 'X'
     O******                   XXCNDE    81
     O******                   XXDTRP    97 '  /  /    '
     O******                   XXDT1    121 '  /  /    '
     O******  E   42           NOI
     O******                   XXPOIY    12
     O******                   XXPOIN    23
     O******                   XEADRY    33
     O******                   XEADRN    52
     O******                   XXNADS    69
     O******                   XXBPRT   112
     O******  E   43           NOI
     O******                   XXACDS   139
     O******  E   44           NOI
     O******                   XXIRTN    24 '  /  /    '
     O******                   XXPOWW2   42 '$'
     O******                   XXDOD     66 '  /  /    '
     O******                   XXACDS   139
     O******                   XXACD2   139
      ******
      * Treating Physician/Hospital
     O******  E   46           NOI
     O******                   XXPNAM    47
     O******                   XITRT0    64
     O******                   XXHFSN    99
     O******  E   47           NOI
     O******                   XXPAD1    47
     O******                   XITRT1    64
     O******                   XXHOSN   124
     O******  E   48           NOI
     O******                   XXPCSZ    37
     O******                   XTPZIP    46
     O******                   XITRT2    64
     O******                   XXHOSA   124
     O******  E   49           NOI
     O******                   XXAREA4   15
     O******                   XXPPH#    24 '    -    '
     O******                   XITRT3    64
     O******                   XXHCSZ   109
     O******                   XHOSZ1   122
     O******  E   50           NOI
     O******                   XITRT4    64
      *
      * Preparer
     O******  E   52           NOI
     O******                   XXPREP    57
     O******                   XXOFAR    97 '0(   )'
     O******                   XXOFPH   106 '0   -    '
     O******                   XXRPDT   127 '  /  /    '
      ******
      ****Income Benefits
     O******  E   56           NOI
     O******                   XXTHY     11
     O******                   XXTHN     21
     O******                   XXAWW K   58
     O******                   XXWKBNK   90
     O******                   XXDT1    121 '  /  /    '
     O******  E   59           NOI
     O******                   XXPADT    39 '  /  /    '
     O******                   XXAAMT    71 '         '
     O******                   XXPADT   103 '  /  /    '
     O******                   XPNLTY   127 '           '
     O******  E   61           NOI
     O******                   XXPAYF    50 '  /  /    '
     O******  E   62           NOI
     O******                             12 ' '
     O******  E   63           NOI
     O******                   XXTT      12
     O******                   XXTP      34
     O******                   XXPP      57
     O******                   XXPERCZ   85
     O******                   XXPART   107
     O******                   XXWKS Z  121
     O******  E   64           NOI
     O******                             12 ' '
     O******  E   64           NOI
     O******                   XXPAYT    30 '  /  /    '
     O******  E   69           NOI
     O******                   XXTHY     12
     O******                   XXTHN     22
     O******                   NI60A1    90
     O******  E    1           NOI
     O*** END DEL *** 11/20/06 **************************************
     O*** END ADD *** 04/07/06 *****************************
     O*** END ADD *** 11/13/03 *****************************
     OQSYSPRT1  E            LABEL          1 11
     O                       XXCNAM              50
     O          E            LABEL          1
     O                       CDADR1              45
     O          E      N41   LABEL          1
     O                       CDADR2              45
     O          E            LABEL          1
     O                       XXCCSZ              45
     O                       XXMC                 2
     O*
     O*** BEG DEL *** 11/13/03 *****************************
     O****
     O******* QSYSPRT2E    1           NOI2
     O******* E    5           NOI2
     O*******                  CMFND    103
     O*******                           104 '-'
     O*******                  CMFYR    107
     O*******                           108 '-'
     O*******                  CMCASE   115
     O******* E    7           NOI2
     O*******                  XXFNM1    90
     O*******                  XXONM1   115
     O******* E    8           NOI2
     O*******                  XXENAM    44
     O*******                  EMAREA4   49
     O*******                  EMPHN#    58 '0   -   0'
     O*******                  XXFNM2    90
     O*******                  XXONM2   115
     O******* E    9           NOI2
     O*******                  EMMAD1    55
     O******* E   10           NOI2
     O*******                  EMMAD2    55
     O*******                  EMFEIN    80 '0   -  -    '
     O*******                  XXCOFE   109 '0   -  -    '
     O******* E   12           NOI2
     O*******                  XXOFA1   115
     O******* E   13           NOI2
     O*******                  XXECSZ    44
     O*******                  XXNAOB    81
     O*******                  XXOFA2   115
     O******* E   14           NOI2
     O*******                  XXOFCY   109
     O******* E   15           NOI2
     O*******                  XXLAD1    50
     O*******                  XXLCTY    75
     O*******                  XXLST     78
     O*******                  XXLZIP    89 '     -    '
     O*******                  XXOFST   103
     O*******                  XXOFZP   115 '     -    '
     O******* E   16           NOI2
     O*******                  XXLAD2    50
     O******* E   18           NOI2
     O*******                  XXACAD    89
     O*******                  XXOFAR   100 '0(   )'
     O*******                  XXOFPH   109 '0   -    '
     O******* E   20           NOI2
     O*******                  XXCNAM    55
     O*******                  XXCDOB    80 '  /  /    '
     O*******                  XXCNDE   120
     O******* E   22           NOI2
     O*******                  XXCLA1    52
     O******* E   23           NOI2
     O*******                  XXCLA2    52
     O*******                  XXACDT    80 '  /  /    '
     O*******                  XXCSS#   110
     O******* E   25           NOI2
     O*******                  XXCLCY    33
     O*******                  XXCLAR    51 '0(   )'
     O*******                  XXCLPH    60 '0   -    '
     O*******                  XX#DEPZ   80
     O******* E   26           NOI2
     O*******                  XXCLST    27
     O*******                  XXCLZP    38 '0     -    '
     O******* E   27           NOI2
     O*******                  XXTWDB    66 '   :  '
     O******* E   28           NOI2
     O*******                  XXSEXM    12
     O*******                  XXSEXF    20
     O*******                  XXTIME    37 '  :  '
     O*******         N20                40 'am'
     O*******          20                40 'pm'
     O*******                  XXAM      48
     O*******                  XXPM      60
     O*******                  XXDTRP    90 '  /  /    '
     O******* E   31           NOI2
     O*******                  XXDOH     20 '  /  /    '
     O*******                  XXDT1     76 '  /  /    '
     O******* E   32           NOI2
     O*******                  XXWNDY    29
     O*******                  XXWNDN    46
     O*******                  XXPOIY    79
     O*******                  XXPOIN    91
     O******* E   34           NOI2
     O*******                  XXHRSDZ   15
     O******* E   35           NOI2
     O*******                  XXHPWKZ   15
     O*******                  XXDAYS    38
     O*******                  XXDYOF    67
     O*******                  XXPAWWK   80
     O*******                  XXPWWF    88
     O******* E   38           NOI2
     O*******                  XXAWW K   15
     O*******                  XXAWW K   70
     O******* E   40           NOI2
     O******* E   41           NOI2
     O*******                  XEADRY    26
     O*******                  XEADRN    41
     O*******                  XXNADS    79
     O*******                  XXBPRT    99
     O******* E   43           NOI2
     O*******                  XXACDS    97
     O******* E   44           NOI2
     O*******                  XXACD2    97
     O******* E   46           NOI2
     O*******                  XXIRTN    20 '  /  /    '
     O*******                  XXPOWW2   38 '$'
     O*******                  XXDOD     96 '  /  /    '
     O******* E   49           NOI2
     O*******                  XXPNAM    45
     O*******                  XITRT0    55
     O*******                  XXHFSN    84
     O******* E   50           NOI2
     O*******                  XXPAD1    45
     O*******                  XITRT1    55
     O*******                  XXHNM1    96
     O******* E   51           NOI2
     O*******                  XXPCSZ    35
     O*******                  XTPZIP    46
     O*******                  XITRT2    55
     O*******                  XXHNM2    96
     O******* E   52           NOI2
     O*******                  XXHAD1    96
     O******* E   53           NOI2
     O*******                  XXAREA4   14
     O*******                  XXPPH#    23 '    -    '
     O*******                  XITRT3    55
     O*******                  XXHAD2    96
     O******* E   54           NOI2
     O*******                  XITRT4    55
     O*******                  XXHOSC   101
     O******* E   56           NOI2
     O*******                  XXMCOY    64
     O*******                  XXMCON    69
     O*******                  XXHOSS    78
     O*******                  XHZIP     90
     O******* E   58           NOI2
     O*******                  XXPREP    47
     O*******                  XXTITL    70
     O*******                  XXOFAR    91 '0(   )'
     O*******                  XXOFPH   100 '0   -    '
     O*******                  XXRPDT   115 '  /  /    '
     O******* E   62           NOI2
     O*******                  XXAWWK    31
     O*******                  XXWKBNK   55
     O*******                  XXDT1     84 '  /  /    '
     O*******                  XXPADT   115 '  /  /    '
     O******* E   64           NOI2
     O*******                  XXAAMTK   30
     O*******                  XPNLTYK   57
     O*******                  XXTHY     82
     O*******                  XXTHN     88
     O******* E   65           NOI2
     O*******                  XXPAYF    45 '  /  /    '
     O******* E   67           NOI2
     O*******                  XXTT       6
     O*******                  XXTP      31
     O*******                  XXPP      53
     O*******                  XXPERCZ   80
     O*******                  XXPART    99
     O*******                  XXWKS Z  109
     O******* E   70           NOI2
     O*******                  XXPAYT    20 '  /  /    '
     O******* E   73           NOI2
     O*******                  XXPREP    60
     O******* E   77           NOI2
     O*******                  XXCONT    90
     O******* E   79           NOI2
     O*******                  XXPREP    60
     O******* E   83           NOI2
     O*******                  XXCPY     62
     O*******                  XXUSER   115
     O******* E    1           NOI2
     O*** END DEL *** 11/13/03 *****************************
     O*** BEG ADD *** 01/26/06 *****************************
     O*** BEG DEL *** 11/20/06 *****************************
     O**********QSYSPRT2E    1           NOI2
      *******
      *******  Employee
     O******* E   14           NOI2
     O*******                  XXSEXM    98
     O******* E   15           NOI2
     O*******                  CDLSN     55
     O*******                  CDFSN     71
     O*******                  CDMID     70
     O*******                  XXSEXF    98
     O*******                  XXCDOB   121 '  /  /    '
     O******* E   17           NOI2
     O*******                  XXCLA1    52
     O*******                  XXCLAR    81 '0(   )'
     O*******                  XXCLPH    90 '0   -    '
     O*******                  XXCSS#   121
     O******* E   18           NOI2
     O*******                  XXCLA2    52
     O******* E   19           NOI2
     O*******                  XXCLCY    37
     O*******                  XXCLST    40
     O*******                  XXCLZP    60 '0     -    '
     O*******                  NI40A3   111
      *******
      *******       Employer
     O******* E   21           NOI2
     O*******                  XXENAM    65
     O*******                  EMAREA4   50
     O*******                  EMNAIC    82
     O*******                  XXNAOB   132
     O******* E   23           NOI2
     O*******                  EMMAD1    52
     O*******                            77 '('
     O*******                  EMAREA4   80
     O*******                            81 ')'
     O*******                  EMPHN#    90 '0   -    '
     O*******                  EMFEIN   121 '0   -  -    '
     O******* E   24           NOI2
     O*******                  EMMAD2    52
     O******* E   25           NOI2
     O*******                  XXECSZ    52
     O*******                  XXEMAL   111
      *******
      ******* Insurer/Self Insurer
     O******* E   27           NOI2
     O*******                  XXFNM1    60
     O*******                  XXOFA1    91
     O******* E   28           NOI2
     O*******                  XXOFA2    90
     O******* E   29           NOI2
     O*******                  XXONM1    50
     O*******                  XXOFCY    86
     O*******                  XXOFST   103
     O*******                  XXOFZP   115 '     -    '
     O******* E   31           NOI2
     O*******                  NI5N3     25
     O*******                  CMFND     39
     O*******                            40 '-'
     O*******                  CMFYR     43
     O*******                            44 '-'
     O*******                  CMCASE    51
     O*******                  XXOFAR    65 '0(   )'
     O*******                  XXOFPH    74 '0   -    '
     O*******                  NI40A4   125
      *******      Employment/Wage
     O******* E   34           NOI2
     O*******                  XXDOH     42 '  /  /    '
     O*******                  XXCLAS    59
     O*******                  XXDAYS    85
     O*******                  XXPAWWK  113
     O*******                  XXPWWF   117
     O******* E   36           NOI2
     O*******                  XXDYOF    32
      *******
      *******      Injury/Illness & Medical
     O******* E   38           NOI2
     O*******         N20                56 'X'
     O******* E   39           NOI2
     O*******                  XXACDT    42 '  /  /    '
     O*******                  XXTIME    53 '  :  '
     O*******          20                56 'X'
     O*******                  XXCNDE    81
     O*******                  XXDTRP    97 '  /  /    '
     O*******                  XXDT1    121 '  /  /    '
     O******* E   42           NOI2
     O*******                  XXPOIY    12
     O*******                  XXPOIN    23
     O*******                  XEADRY    33
     O*******                  XEADRN    52
     O*******                  XXNADS    69
     O*******                  XXBPRT   112
     O******* E   43           NOI2
     O*******                  XXACDS   139
     O******* E   44           NOI2
     O*******                  XXIRTN    24 '  /  /    '
     O*******                  XXPOWW2   42 '$'
     O*******                  XXDOD     66 '  /  /    '
     O*******                  XXACDS   139
     O*******                  XXACD2   139
      *******
      *******  Treating Physician/Hospital
     O******* E   46           NOI2
     O*******                  XXPNAM    47
     O*******                  XITRT0    64
     O*******                  XXHFSN    99
     O******* E   47           NOI2
     O*******                  XXPAD1    47
     O*******                  XITRT1    64
     O*******                  XXHOSN   124
     O******* E   48           NOI2
     O*******                  XXPCSZ    37
     O*******                  XTPZIP    46
     O*******                  XITRT2    64
     O*******                  XXHOSA   124
     O******* E   49           NOI2
     O*******                  XXAREA4   15
     O*******                  XXPPH#    24 '    -    '
     O*******                  XITRT3    64
     O*******                  XXHCSZ   109
     O*******                  XHOSZ1   122
     O******* E   50           NOI2
     O*******                  XITRT4    64
      *******
      ******* Preparer
     O******* E   52           NOI2
     O*******                  XXPREP    57
     O*******                  XXOFAR    97 '0(   )'
     O*******                  XXOFPH   106 '0   -    '
     O*******                  XXRPDT   127 '  /  /    '
      *******
      ******* B. Income Benefits
     O******* E   56           NOI2
     O*******                  XXTHY     11
     O*******                  XXTHN     21
     O*******                  XXAWW K   58
     O*******                  XXWKBNK   90
     O*******                  XXDT1    121 '  /  /    '
     O******* E   59           NOI2
     O*******                  XXPADT    39 '  /  /    '
     O*******                  XXAAMT    71 '         '
     O*******                  XXPADT   103 '  /  /    '
     O*******                  XPNLTY   127 '           '
     O******* E   61           NOI2
     O*******                  XXPAYF    50 '  /  /    '
     O******* E   62           NOI2
     O*******                            12 ' '
     O******* E   63           NOI2
     O*******                  XXTT      12
     O*******                  XXTP      34
     O*******                  XXPP      57
     O*******                  XXPERCZ   85
     O*******                  XXPART   107
     O*******                  XXWKS Z  121
     O******* E   64           NOI2
     O*******                            12 ' '
     O******* E   64           NOI2
     O*******                  XXPAYT    30 '  /  /    '
     O******* E   69           NOI2
     O*******                  XXTHY     12
     O*******                  XXTHN     22
     O*******                  NI60A1    90
     O******* E    1           NOI2
     O*** END DEL *** 11/20/06 *****************************
     O*** BEG ADD *** 11/20/06 *****************************
     O******  QSYSPRT2E    1           NOI2
      *
      * Employee
     O******  E   12           NOI2
     O******                   CDLSN     60
     O******                   CDFSN     75
     O******                   CDMID     94
     O******                   XXCSS#   107
     O******                   XXACDT   125 '  /  /    '
     O******  E   15           NOI2
     O******                   XXSEXM    25
     O******  E   16           NOI2
     O******                   XXSEXF    25
     O******                   XXCDOB    45 '  /  /    '
     O******                   XXCLAR    64 '0(   )'
     O******                   XXCLPH    73 '0   -    '
     O******                   NI40A3   125
     O******  E   18           NOI2
     O******                   XXCLA1    50
     O******                   XXCLCY    95
     O******                   XXCLST   103
     O******                   XXCLZP   125 '0     -    '
      ***
      **Employer
     O******  E   21           NOI2
     O******                   XXENAM    65
     O******                   EMNAIC    82
     O******                   XXNAOB   132
     O******  E   23           NOI2
     O******                   EMMAD1    52
     O******                             77 '('
     O******                   EMAREA4   80
     O******                             81 ')'
     O******                   EMPHN#    90 '0   -    '
     O******                   EMFEIN   121 '0   -  -    '
     O******  E   23           NOI2
     O******                   EMMAD2    52
     O******  E   24           NOI2
     O******                   XXECSZ    52
     O******                   XXEMAL   111
      *
      * Insurer/Self Insurer
     O******  E   27           NOI2
     O******                   XXFNM1    60
     O******                   XXOFA1    92
     O******  E   28           NOI2
     O******                   XXOFA2    90
     O******  E   29           NOI2
     O******                   XXONM1    50
     O******                   XXOFCY    85
     O******                   XXOFST    95
     O******                   XXOFZP   107 '     -    '
     O******                   XXFEIN   125
     O******  E   31           NOI2
     O******                   NI5N3     25
     O******                   CMFND     39
     O******                             40 '-'
     O******                   CMFYR     43
     O******                             44 '-'
     O******                   CMCASE    51
     O******                   XXOFAR    65 '0(   )'
     O******                   XXOFPH    74 '0   -    '
     O******                   NI40A4   125
      **Employment/Wage
     O******  E   34           NOI2
     O******                   XXDOH     45 '  /  /    '
     O******                   XXCLAS    59
     O******                   XXDAYS    85
     O******                   XXPAWWK  113
     O******                   XXPWWF   117
     O******  E   35           NOI2
     O******                   XXDYOF    32
      ******
      * Injury/Illness & Medical
     O******  E   37           NOI2
     O******          N20                43 'X'
     O******  E   38           NOI2
     O******                   XXTIME    40 '  :  '
     O******           20                43 'X'
     O******                   XXDTRP    97 '  /  /    '
     O******                   XXDT1    121 '  /  /    '
     O******  E   41           NOI2
     O******                   XXPOIY    13
     O******                   XXPOIN    23
     O******                   XEADRY    33
     O******                   XEADRN    52
     O******                   XXNADS    69
     O******                   XXBPRT   112
     O******  E   43           NOI2
     O******                   XXACDS    70
     O******                   XXACD2   130
     O******  E   43           NOI2
      * Treating Physician/Hospital
     O******  E   45           NOI2
     O******                   XXPNAM    45
     O******                   XITRT0    45
     O******                   XXHFSN    80
     O******                   XXIRTN   127 '  /  /    '
     O******  E   46           NOI2
     O******                   XXPAD1    45
     O******                   XITRT1    45
     O******                   XXHOSN   104
     O******                   XXPOWW2  123 '$'
     O******  E   47           NOI2
     O******                   XXPCSZ    35
     O******                   XTPZIP    42
     O******                   XITRT2    45
     O******                   XXHOSA   104
     O******  E   48           NOI2
     O******                   XXAREA4   15
     O******                   XXPPH#    24 '    -    '
     O******                   XITRT3    45
     O******                   XXHCSZ    89
     O******                   XHOSZ1    96
     O******                   XXDOD    127 '  /  /    '
     O******  E   49           NOI2
     O******                   XITRT4    45
      *
      * Preparer
     O******  E   51           NOI2
     O******                   XXPREP    57
     O******                   XXOFAR    97 '0(   )'
     O******                   XXOFPH   106 '0   -    '
     O******                   XXRPDT   127 '  /  /    '
      ******
      *****Income Benefits
     O******  E   55           NOI2
     O******                   XXTHY     12
     O******                   XXTHN     21
     O******                   XXAWW K   58
     O******                   XXWKBNK   90
     O******                   XXDT1    121 '  /  /    '
     O******  E   57           NOI2
     O******                   XXPADT    39 '  /  /    '
     O******                   XXAAMT    71 '         '
     O******                   XXPADT   103 '  /  /    '
     O******                   XPNLTY   128 '           '
     O******  E   59           NOI2
     O******                   XXPAYF    50 '  /  /    '
     O******  E   60           NOI2
     O******                             12 ' '
     O******  E   61           NOI2
     O******                   XXTT      12
     O******                   XXTP      34
     O******                   XXPP      57
     O******                   XXPERCZ   85
     O******                   XXPART   107
     O******                   XXWKS Z  121
     O******  E   62           NOI2
     O******                             12 ' '
     O******  E   62           NOI2
     O******                   XXPAYT    30 '  /  /    '
     O******  E   68           NOI2
     O******                   NI60A1    70
     O******  E    1           NOI2
     C*** END DEL *** 11/20/06 **************************************
     O*** END ADD *** 11/20/06 *****************************
**  COPY MSG
EMPLOYER COPY
EMPLOYEE COPY
DIVISION COPY
