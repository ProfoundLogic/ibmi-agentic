     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0875R - WORK WITH EDI HISTORY LOG
     H*****************************************************************
     H* PROGRAM: F0875R
     H* DESCRIPTION: WORK WITH EDI LOG HISTORY PROGRAM
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                               *
     H*  03/25/14 C:296 Project: MC376                                *
     H*                 Remove option 7 = Rejection reason.           *
     H*                                                               *
     H*  09/12/02 C:15  Project: KY010                                *
     H*                 Chgd pgm to use KEY1 when loading the screen  *
     H*                 to increase efficiency.                       *
     H*                                                               *
     H*  05/12/00 C:53  Project: MCMNT
     H*                 CHANGED EMPLOYER DIVISION DEFN FROM 3 TO 5.
     H*
     H*  03/01/00 C:53  Project: KY010
     H*                 CORRECTED SUBSET TO GO TO THE EOMB LEVEL.
     H*                 CORRECTED HEADINGS NOT SHOWING.
     H*
     H*  06/21/99 C:31  Project: KY010-2
     H*                 V4: EDI Log error reasons
     H*                 1. Add option 7: Reasons
     H*****************************************************************
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**     N40      837/835 EOMB                                   **
     H**      40      278 Treatment plan                             **
     H**      60      General usage                                  **
     H**      61      General usage                                  **
     H*****************************************************************
     H*
     FF0875F    CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FFELGP     IF   E             DISK
     F                                     RENAME(PLG01:PFRCDF)
     FFELGL2    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PLG01:DBRCDF)
     F*FMLGL7  IF  E           K        DISK
     F*                                              KINFDS DBFDS2
     F*            PLG01                             KRENAMEDBRCDF2
     FFMCMLA    IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMEML     IF   E           K DISK
     FFMF1L     IF   E           K DISK
      * GENERIC TABLE FILE
     FFTGTL     IF   E           K DISK
     F*** BEGIN ADD *** 10/09/97 ***********************************************
     FFATRL     IF   E           K DISK
     FFATIL     IF   E           K DISK
     FFATDL1    IF   E           K DISK
     F*** END ADD *** 10/09/97 *************************************************
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column Headings
     D*** BEGIN CHG *** 06/21/99 **************************************
     D********            OF      1   9  1   OFD    35   Options, Function
     D OF              S              1    DIM(10) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(10) ALT(OF)
     D*** END CHG *** 06/21/99 ****************************************
     D DTA1            S              1    DIM(74)                              Screen Data
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D* /COPY required constants
     D*
     D OPTNM           C                   CONST('EDI Log History')
     D ACDPGM          C                   CONST('F0875R01')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D COMAPI          C                   CONST('FA013R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(5)
     D*
     D* Program specific constants
     D*
     D DL              C                   CONST('* * * DELETED * * *')
     D*
     D** DARTS KEY (SC010R) FOR LINK WITH WCMS
     D*
     D                 DS
     D  DSKEY                  1     40
     D  DSINFO                 1     16  0
     D  DSCO#                  1      3  0
     D  DSFND                  4      6  0
     D  DSFYR                  7      9  0
     D  DSCASE                10     16  0
     D*
     D** DARTS KEY (SC010R) FOR OUTSIDE ACCOUNT
     D*
     D                 DS
     D  DRKEY                  1     40
     D  DRINFO                 1     16  0
     D  DRCO#                  1      3  0
     D  DRSUB#                 4      8  0
     D  DRFND                  9     11  0
     D  DRFYR                 12     14  0
     D  DRCASE                15     21  0
     D*
     D                 DS
     D  XXMDY                  1      8
     D  XXMM                   1      2  0
     D  XXDD                   3      4  0
     D  XXYY                   5      8  0
     D*
     D                 DS
     D  XFMDY                  1     10
     D  XFMM                   1      2
     D  XFS1                   3      3
     D  XFDD                   4      5
     D  XFS2                   6      6
     D  XFYY                   7     10

     C*
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    CMACDT        XXACDT
     C     *LIKE         DEFINE    LGSUB#        XXSUB#
     C     *LIKE         DEFINE    LGFND         XXFND
     C     *LIKE         DEFINE    LGFYR         XXFYR
     C     *LIKE         DEFINE    LGCASE        XXCASE
     C     *LIKE         DEFINE    LGEXDT        XXEXDT
     C     *LIKE         DEFINE    GTTBID        KYTBID
     C     *LIKE         DEFINE    GTKEY         KYKEY
     C     *LIKE         DEFINE    LGCO#         SSCO#
     C     *LIKE         DEFINE    LGSUB#        SSSUB#
     C     *LIKE         DEFINE    LGFND         SSFND
     C     *LIKE         DEFINE    LGFYR         SSFYR
     C     *LIKE         DEFINE    LGCASE        SSCASE
     C     *LIKE         DEFINE    LGTSET        SSTSET
     C     *LIKE         DEFINE    LGEOB#        SSEOB#                         ADD 03/06/00
     C*
     C     *LIKE         DEFINE    @@SUB#        SCSUB#
     C     *LIKE         DEFINE    @@FND         SCFND
     C     *LIKE         DEFINE    @@FYR         SCFYR
     C     *LIKE         DEFINE    @@CASE        SCCASE
     C     *LIKE         DEFINE    @@SS#         SCSS#
     C     *LIKE         DEFINE    @@ACDT        SCACDT
     C*
     C*** MAY NOT NEED AFTER SUB# EMPLOYER # IS DETERMINED NEEDED.
     C*
     C     *LIKE         DEFINE    ##DIV         SCDIV
     C     *LIKE         DEFINE    ##EMNM        SCEMNM
     C     *LIKE         DEFINE    ##EMP#        SCEMP#
     C     *LIKE         DEFINE    ##LSNM        SCLSNM
     C     *LIKE         DEFINE    ##SBNM        SCSBNM
     C     *LIKE         DEFINE    ##SCL#        SCSCL#
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
     C                   PARM                    @@SS#             9
     C                   PARM                    @@ACDT            8 0
     C                   PARM                    @@EOB#            9 0
     C*** BEGIN ADD *** 10/09/97 ***********************************************
     C                   PARM                    @@TSET            3
     C*
     C*          DATCHK    PLIST
     C*                    PARM           SRDTI   80
     C*                    PARM *BLANKS   SRFFMT  8
     C*                    PARM *BLANKS   SRTFMT  8
     C*                    PARM           SRDTO  10
     C*                    PARM           SRERR   1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*          DTMDYY    PLIST
     C*                    PARM           SRDTI
     C*                    PARM '*CYMD'   SRFFMT
     C*                    PARM *BLANKS   SRTFMT
     C*                    PARM           SRDTO
     C*                    PARM           SRERR
     C*** END ADD *** 10/09/97 *************************************************
     C*
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##SBNM           40
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##FYR             3 0
     C                   PARM                    ##CASE            7 0
     C                   PARM                    ##EMP#            9 0
     C                   PARM                    ##DIV             5 0          CHG 10/12/00
     C                   PARM                    ##EMNM           40
     C                   PARM                    ##SS#             9
     C                   PARM                    ##ACDT            8 0
     C                   PARM                    ##LSNM           40
     C                   PARM                    ##SCL#           25
     C                   PARM      SCL1HD        ##L1HD           78
     C                   PARM      SCL2HD        ##L2HD           78
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C*** KEY LIST FOR CASE MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXSS#
     C                   KFLD                    XXACDT
     C                   KFLD                    ##CO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*** KEY LIST FOR CLAIM DEMOGRAPHICS FILE
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#
     C*
     C*** KEY LIST FOR SMUPP FILE
     C     SMKEY         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXSUB#
     C*
     C* KEY LIST FOR LOG FILE WITH EOB#
     C*
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*                    KFLD           XXEOB#
     C*
     C*          KEY2      KLIST
     C*                    KFLD           ##CO#
     C*                    KFLD           XXSUB#
     C*                    KFLD           XXFND
     C*                    KFLD           XXFYR
     C*                    KFLD           XXCASE
     C*                    KFLD           XXEOB#
     C*                    KFLD           XXEXDT
     C*
     C* KEY LIST FOR LOG FILE WITHOUT EOB#
     C     KEY3          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     KEY4          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEXDT
     C*
     C*
     C*** KEY LIST FOR EMPLOYER FILE
     C     EMKEY         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     F1KEY         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C*
     C*   KEY LIST FOR GENERIC TABLE FILE -- FTGTL
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KYTBID
     C                   KFLD                    KYKEY
     C*** BEGIN ADD *** 10/09/97 ***********************************************
     C*
     C*   KEY LIST FOR BWC TREATMENT FILES -- FATIL, FATRL
     C*
     C     TIKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXTX#
     C     *LIKE         DEFINE    TITX#         XXTX#
     C*** END ADD *** 10/09/97 *************************************************
     C*
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C******************** SETON                     11
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   MOVE      @@CO#         SSCO#
     C                   MOVE      @@SUB#        SSSUB#
     C                   MOVE      @@FND         SSFND
     C                   MOVE      @@FYR         SSFYR
     C                   MOVE      @@CASE        SSCASE
     C                   MOVE      @@TSET        SSTSET
     C                   MOVE      @@EOB#        SSEOB#                         ADD 03/06/00
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVEL     *LOVAL        XXEXDT
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   MOVE      SCPST1        XXEXDT
     C                   Z-ADD     XXEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXDT
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXDT
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL(P)  'MTC '        KYTBID
     C                   MOVEL(P)  LGMTC         KYKEY
     C     GTKY          CHAIN     PGT01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        XADESC           15
     C                   ELSE
     C                   MOVE      *BLANKS       XADESC
     C                   ENDIF
     C*
     C*** MOVE TO ALPHA FIELDS FOR MOVE TO SCDATA
     C                   Z-ADD     LGEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEXDT
     C                   MOVEL     SCEXDT        XXMDY
     C                   MOVEL     XXMM          XFMM
     C                   MOVEL     XXDD          XFDD
     C                   MOVEL     XXYY          XFYY
     C                   MOVE      '/'           XFS1
     C                   MOVE      '/'           XFS2
     C                   MOVEL     XFMDY         X1EXDT           10
     C*
     C                   Z-ADD     LGDUE         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDUE             8 0
     C                   MOVEL     XXDUE         XXMDY
     C                   MOVEL     XXMM          XFMM
     C                   MOVEL     XXDD          XFDD
     C                   MOVEL     XXYY          XFYY
     C                   MOVE      '/'           XFS1
     C                   MOVE      '/'           XFS2
     C                   MOVEL     XFMDY         X1DUE            10
     C*
     C                   Z-ADD     LGOTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXSENT            8 0
     C                   MOVEL     XXSENT        XXMDY
     C                   MOVEL     XXMM          XFMM
     C                   MOVEL     XXDD          XFDD
     C                   MOVEL     XXYY          XFYY
     C                   MOVE      '/'           XFS1
     C                   MOVE      '/'           XFS2
     C                   MOVEL     XFMDY         X1SENT           10
     C*
     C                   Z-ADD     LGARCV        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXARCV            8 0
     C                   MOVEL     XXARCV        XXMDY
     C                   MOVEL     XXMM          XFMM
     C                   MOVEL     XXDD          XFDD
     C                   MOVEL     XXYY          XFYY
     C                   MOVE      '/'           XFS1
     C                   MOVE      '/'           XFS2
     C                   MOVEL     XFMDY         X1ARCV           10
     C*
     C*  MOVE DATA TO SCREEN ARRAY
     C                   CLEAR                   DTA1
     C*
     C                   Z-ADD     1             Y                 2 0
     C                   MOVEA     X1EXDT        DTA1(Y)
     C                   ADD       11            Y
     C                   MOVEA     LGTSET        DTA1(Y)
     C                   ADD       4             Y
     C                   MOVEA     LGMTC         DTA1(Y)
     C                   ADD       2             Y
     C                   MOVE      '-'           DTA1(Y)
     C                   ADD       1             Y
     C                   MOVEA     XADESC        DTA1(Y)
     C                   ADD       16            Y
     C                   MOVEA     LGSTS         DTA1(Y)
     C                   ADD       4             Y
     C                   MOVEA     X1DUE         DTA1(Y)
     C                   ADD       11            Y
     C                   MOVEA     X1SENT        DTA1(Y)
     C                   ADD       11            Y
     C                   MOVEA     X1ARCV        DTA1(Y)
     C                   ADD       12            Y
     C                   MOVEA     LGSTAT        DTA1(Y)
     C                   MOVEA(P)  DTA1          SCDATA
     C*** BEGIN ADD *** 06/21/99 **************************************
     C                   MOVEL(P)  LGSTS         SHSTS
     C*** END ADD *** 06/21/99 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C                   SELECT
     C     SCSEL         WHENEQ    '6 '
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C***** Beg Del ***** 03/25/14 ************************************
     C*** BEGIN ADD *** 06/21/99 **************************************
     C*****SCSEL         WHENEQ    '7 '
     C*****              EXSR      OPT7                                         Reasons
     C***** End Del ***** 03/25/14 ************************************
     C*** END ADD *** 06/21/99 ****************************************
     C*
     C     SCSEL         WHENEQ    '14'
     C*
     C     F1KEY         CHAIN     PF101                              99
     C     *IN99         IFEQ      '0'
     C*
     C     F1OUTA        IFEQ      'N'                                          LINKED WITH
     C                   MOVE      *BLANKS       DSKEY                          WCMS
     C                   MOVE      *ZEROS        DSINFO
     C                   MOVE      ##CO#         DSCO#
     C                   MOVE      SCFND         DSFND
     C                   MOVE      SCFYR         DSFYR
     C                   MOVE      SCCASE        DSCASE
     C                   MOVEL     DSKEY         ##KEY            40
     C*
     C                   ELSE
     C                   MOVE      *BLANKS       DRKEY                          STANDALONE
     C                   MOVE      *ZEROS        DRINFO
     C                   MOVE      ##CO#         DRCO#
     C                   MOVE      SCSUB#        DRSUB#
     C                   MOVE      SCFND         DRFND
     C                   MOVE      SCFYR         DRFYR
     C                   MOVE      SCCASE        DRCASE
     C                   MOVEL     DRKEY         ##KEY
     C                   ENDIF                                                  F1OUTA IFEQ
     C*
     C*** BEGIN CHG *** 09/26/97 **************************************
     C*** USING F0270R INSTEAD OF F0075R
     C*********            MOVEL'F0075R'  ##PGM
     C                   MOVEL(P)  'F0270R'      ##PGM
     C*** END CHG ***** 09/26/97 **************************************
     C*
     C                   CALL      'SC010R'
     C                   PARM                    ##KEY
     C                   PARM                    ##PGM
     C                   ENDIF
     C*
     C                   ENDSL
     C                   ENDSR
     C***** Beg Del ***** 03/25/14 ************************************
     C*** BEGIN ADD *** 06/21/99 **************************************
     C*================================================================
     C* OPT 7 - REASONS
     C*================================================================
     C*****OPT7          BEGSR
     C*****SHSTS         IFEQ      'EC'
     C*****SHSTS         OREQ      'EI'
     C*****SHSTS         OREQ      'TE'
     C*****SHSTS         OREQ      'TR'
     C*****              CALL      'F0877R'
     C*****              PARM                    @@CALL            1
     C*****              PARM      SCDBRR        ##RRN             9 0
     C*****              ELSE                                                   STS = 'EC'
     C*****              SETON                                        77
     C*****              ENDIF                                                  STS = 'EC'
     C*****              SETON                                        90
     C*****              ENDSR
     C*** END ADD *** 06/21/99 ****************************************
     C***** End Del ***** 03/25/14 ************************************
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   MOVE      *INKC         @@KC
     C                   MOVE      *INKL         @@KL
     C                   MOVE      '1'           *INLR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C***                  Z-ADD6         SF1PAG  50
     C***                  MOVELOPTNM     ##DATA    P
     C***                  CALL 'SRCENTER'
     C***                  PARM           ##DATA 80
     C***                  PARM 40        ##LENG  30
     C***                  MOVEL##DATA    SCPRDS
     C*
     C                   MOVE      @@SUB#        SCSUB#
     C                   MOVE      @@FND         SCFND
     C                   MOVE      @@FYR         SCFYR
     C                   MOVE      @@CASE        SCCASE
     C                   MOVE      @@SS#         SCSS#
     C                   Z-ADD     @@ACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C                   MOVE      @@EOB#        SCEOB#
     C*
     C** DETERMINE IF CALLED FROM PROVIDER BILLS AND QUALIFY EOB#
     C*
     C     @@EOB#        COMP      *ZEROS                                 41
     C*
     C** SET UP KEY VALUES
     C*
     C                   MOVE      @@CO#         ##CO#
     C*** BEG CHG *** 03/01/00 ***********************
     C*******************  MOVE @@ACDT    XXACDT
     C                   Z-ADD     @@ACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT
     C*** BEG CHG *** 03/01/00 ***********************
     C                   MOVE      SCSS#         XXSS#
     C                   MOVE      SCSUB#        XXSUB#
     C                   MOVE      SCFND         XXFND
     C                   MOVE      SCFYR         XXFYR
     C                   MOVE      SCCASE        XXCASE
     C                   MOVE      SCEOB#        XXEOB#            9 0
     C*
     C** GET CLAIM IMFORMATION
     C*
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      '0'
     C                   Z-ADD     CMEMP#        SCEMP#
     C                   Z-ADD     CMDIV         SCDIV
     C                   MOVEL     CMSCL#        SCSCL#
     C*
     C     SMKEY         CHAIN     PSM01                              98
     C     *IN98         IFEQ      '0'
     C                   MOVEL     SMNAME        SCSBNM
     C                   ENDIF
     C*
     C                   MOVEL     CMSS#         XXSS#
     C*
     C     CDKEY         CHAIN     PCD01                              98
     C     *IN98         IFEQ      '0'
     C                   MOVEL     CDLSN         ##LAST           56
     C                   MOVEL     CDFSN         ##FRST           15
     C                   MOVE      '1'           ##TERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    ##LAST
     C                   PARM                    ##FRST
     C                   PARM                    ##TERM
     C                   MOVEL     ##LAST        SCLSNM
     C                   ELSE
     C                   MOVEL     *BLANKS       SCLSNM
     C                   ENDIF
     C*
     C                   Z-ADD     CMFND         XXFND
     C                   Z-ADD     CMEMP#        XXEMP#
     C                   Z-ADD     CMDIV         XXDIV
     C*
     C     EMKEY         CHAIN     PEM01                              98
     C     *IN98         IFEQ      '0'
     C                   MOVEL     EMNAME        SCEMNM
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN99 IFEQ
     C*
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE
     C*
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C     SCL1HD        PARM      *BLANK        ##L1             78
     C     SCL2HD        PARM      *BLANK        ##L2             78
     C*
     C** PARM VALUES FOR ACDPL
     C*
     C                   MOVE      SCSUB#        ##SUB#
     C                   MOVEL     SCSBNM        ##SBNM
     C                   MOVE      SCFND         ##FND
     C                   MOVE      SCFYR         ##FYR
     C                   MOVE      SCCASE        ##CASE
     C                   MOVE      SCEMP#        ##EMP#
     C                   MOVE      SCDIV         ##DIV
     C                   MOVEL     SCEMNM        ##EMNM
     C                   MOVEL     SCSS#         ##SS#
     C                   MOVE      SCACDT        ##ACDT
     C                   MOVEL     SCLSNM        ##LSNM
     C                   MOVEL     SCSCL#        ##SCL#
     C*
     C*** BEGIN ADD *** 10/09/97 ***********************************************
     C                   SETOFF                                       40
     C     @@TSET        IFNE      '837'
     C                   SETON                                        40
     C                   DO
     C                   MOVE      @@EOB#        XXTX#
     C     TIKY          CHAIN     PTR01                              60
     C   60              LEAVE                                                  VVV LEAVE
     C                   Z-ADD     TRBGDT        SRDTI             8 0
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCBGDT
     C     TIKY          CHAIN     PTI01                              60
     C     *IN60         DOWEQ     *OFF
     C     TIPRIM        IFEQ      'Y'
     C                   SETON                                        60
     C                   MOVEL(P)  TIICD9        SCICD9
     C     TIKY          CHAIN     PTD01                              61
     C  N61TDTYPE        IFEQ      'D'
     C     TDSEQ#        ANDEQ     1
     C                   MOVEL(P)  TDDESC        SCDSC1
     C                   ELSE                                                   TYPE = 'D'
     C                   MOVEL     *BLANKS       SCDSC1
     C                   ENDIF                                                  TYPE = 'D'
     C                   ENDIF                                                  PRIM = 'Y'
     C  N60TIKY          READE     PTI01                                  60
     C                   ENDDO                                                  *IN60 = Off
     C                   ENDDO                                                  End DO loop
     C                   ENDIF                                                  TSET <> 837
     C*** END ADD *** 10/09/97 *************************************************
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*** Begin chg *** 09/12/02 **************************************
     C***21*************** READ DBRCDF                   95
     C   21KEY1          READE     DBRCDF                                 95
     C*** End chg ***** 09/12/02 **************************************
     C***22                READ DBRCDF2                  95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*** Begin chg *** 09/12/02 **************************************
     C***21*************** READPDBRCDF                   95
     C   21KEY1          READPE    DBRCDF                                 95
     C*** End chg ***** 09/12/02 **************************************
     C***22                READPDBRCDF2                  95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY4          SETGT     DBRCDF                             96
     C***22      KEY4      SETGTDBRCDF2              96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
TEMP C   21KEY3          SETLL     DBRCDF                             96
TEMP C***21      KEY4      SETLLDBRCDF               96
     C***22      KEY4      SETLLDBRCDF2              96
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C*****************    MOVE DL        SCDESC
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C     SSCO#         IFNE      LGCO#
     C     SSSUB#        ORNE      LGSUB#
     C     SSFND         ORNE      LGFND
     C     SSFYR         ORNE      LGFYR
     C     SSCASE        ORNE      LGCASE
     C     SSTSET        ORNE      LGTSET
     C     SSEOB#        ORNE      LGEOB#                                       ADD 03/01/00
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  CLT# <> CLT#
     C                   ENDSR
** Column headings
Opt   Date     ID   Code & Desc       Sts  Due Date     Sent     Received  Hld
Opt   Date     ID   Code & Desc       Sts  Due Date     Sent     Received  Hld
** Description--------------MISN
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYN
1066=Hold/Release           YNN
203F3=Exit                  YYN
205F5=Refresh               YNN
206F6=Add                   NNN
1077=Rejection reasons      YYN
212F12=Previous             YYY
3
