     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE W0875R - WORK WITH EDI HISTORY LOG
     H*****************************************************************
     H* PROGRAM: W0875R
     H* DESCRIPTION: WORK WITH EDI LOG HISTORY PROGRAM
     H*****************************************************************
     H* CREATED: 02/19/98  C:77  PROJECT: 87MDU
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:318  Project: SY420
     H*                 Mask SSN
     H*
     H* 06/06/08 C:53  Support: 63593
     H*                Correct access to opt 4.
     H*                                                               *
     H* 10/19/07 C:191 Project:WC1584                                 *
     H*                EDI Release 3,                                 *
     H*                                                               *
     H* 05/01/07 C:185 Project:WC1177                                 *
     H*                EDI Release 3, Added option 1 to Select        *
     H*                Pass back RRN in case selection is made        *
     H*                                                               *
     H* 09/27/06 C:15  Support: WC065/54603                           *
     H*                Add access to opt 4 back.  Clients can use     *
     H*                Menu/Option security to prevent access to this *
     H*                option.                                        *
     H*                                                               *
     H* 02/16/06 C:53  SUPPORT: WC132/51005
     H*                ONLY ALLOW OPT 4 FOR PDSPGMR*
     F*
     F* 08/02/02 C:53 SUPPORT: #33472
     F*               ACTIVATED SCOPL2 TO DISPLAY
     H*
     H* 09/26/00 C:15 Project: WC468  Seq# 24396                      *
     H*               Added option to access PTD/Reduced Earnings     *
     H*               entry pgm.                                      *
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**     N40      837/835 EOMB                                   **
     H**      40      278 Treatment plan                             **
     H**      60      General usage                                  **
     H**      61      General usage                                  **
     H**      93      General Usage                                  **
     H**      94      General usage                                  **
     H*****************************************************************
     H*
     FW0875F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     FWMLGP     IF   E             DISK
     F                                     RENAME(PLG01:PFRCDF)
     FWMLGL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PLG01:DBRCDF)
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     F*** BEG ADD ****** 10/19/07 *************************************
     F                                     INFDS(FIDS02)
     F*** END ADD ****** 10/19/07 *************************************
     FWTGTL     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(1) CTDATA PERRCD(1)              Column Headings
     D*** BEGIN CHG *** 05/01/07 **************************************
     D*********           OF      1  10  1   OFD    35   Options, Function
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
     D*** END   CHG *** 05/01/07 **************************************
     D DTA1            S              1    DIM(73)                              Screen Data
     D*
     D***PY SCOPYWW01E
     D*
     D*** BEG ADD ****** 10/19/07 *************************************
     D FIDS02          DS
     D  Q2RRN#               397    400B 0
     D*** END ADD ****** 10/19/07 *************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  SS#                    1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
     D*
     D***PY SCOPYWW01I
     D*
     D* /COPY required constants
     D*
     D OPTNM           C                   CONST('EDI Log History')
     D ACDPGM          C                   CONST('W0875R01')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('WMESGG')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(10)
     D*
     D* Program specific constants
     D*
     D DL              C                   CONST('* * * DELETED * * *')
     D DASHES          C                   CONST('--/--/----')
     D*
     D                 DS
     D  XXMDYY                 1      8  0
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
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
     C*** BEGIN ADD *** 05/01/07 **************************************
     C                   PARM                    RR#               9 0
     C*** BEGIN ADD *** 05/01/07 **************************************
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*** BEG ADD ****** 10/19/07 *************************************
     C                   PARM      Q2RRN#        ##RRN1            9 0
     C*** END ADD ****** 10/19/07 *************************************
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     *LIKE         DEFINE    LGCO#         KKCO#
     C     *LIKE         DEFINE    LGFND         KKFND
     C     *LIKE         DEFINE    LGFYR         KKFYR
     C     *LIKE         DEFINE    LGCASE        KKCASE
     C     *LIKE         DEFINE    LGIDT         KKIDT
     C     *LIKE         DEFINE    LGITM         KKITM
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKTBKY
     C                   MOVE      *BLANKS       XXMTCD           15
     C*
     C*   KEY LIST FOR EDI LOG FILE
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKIDT
     C                   KFLD                    KKITM
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    KKCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*   KEY LIST FOR THE CLAIM MASTER
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*   KEY LIST FOR GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKTBKY
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
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
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C                   Z-ADD     @@CO#         KKCO#
     C                   Z-ADD     @@FND         KKFND
     C                   Z-ADD     @@FYR         KKFYR
     C                   Z-ADD     @@CASE        KKCASE
     C*
     C                   Z-ADD     *ZEROS        KKIDT
     C                   Z-ADD     *ZEROS        KKITM
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   MOVE      SCPST1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKIDT
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C*** BEGIN DLT *** 04/09/99 **************************************
     C******************** Z-ADDSCIDT     SRDTI
     C******************** CALL 'SRCVTDAT'DTCYMD
     C******************** MOVELSRDTO     KKIDT
     C*** END DLT ***** 04/09/99 **************************************
     C*
     C*** BEGIN ADD *** 04/09/99 **************************************
     C                   Z-ADD     SCIDT         KKIDT
     C*** END ADD ***** 04/09/99 **************************************
     C                   Z-ADD     SCITM         KKITM
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     LGIDT         SCIDT
     C                   Z-ADD     LGITM         SCITM
     C*** BEGIN ADD *** 09/26/00 **************************************
     C                   Z-ADD     LGDSQH        SCDSQH
     C*** END ADD ***** 09/26/00 **************************************
     C*
     C* GET MAINT TYPE CODE DESCRIPTION
     C*
     C                   SELECT
     C     LGTSET        WHENEQ    '148'
     C                   MOVEL     'FMTC'        KKTBID
     C     LGTSET        WHENEQ    'A49'
     C                   MOVEL     'SMTC'        KKTBID
     C                   ENDSL
     C*
     C                   MOVE      *BLANKS       XXMTCD
     C                   MOVEL(P)  LGMTC         KKTBKY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        XXMTCD
     C*
     C*  MOVE TO ALPHA FIELDS FOR MOVE TO SCDATA
     C*
     C     LGDUE         IFNE      *ZEROS
     C*
     C                   Z-ADD     LGDUE         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXMDYY
     C*
     C                   MOVEL     XXMM          XFMM
     C                   MOVEL     XXDD          XFDD
     C                   MOVEL     XXYY          XFYY
     C                   MOVE      '/'           XFS1
     C                   MOVE      '/'           XFS2
     C                   MOVEL     XFMDY         X1DUE            10
     C*
     C                   ELSE
     C*
     C                   MOVEL     DASHES        X1DUE
     C*
     C                   ENDIF
     C*
     C     LGEXDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     LGEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXMDYY
     C*
     C                   MOVEL     XXMM          XFMM
     C                   MOVEL     XXDD          XFDD
     C                   MOVEL     XXYY          XFYY
     C                   MOVE      '/'           XFS1
     C                   MOVE      '/'           XFS2
     C                   MOVEL     XFMDY         X1EXDT           10
     C*
     C                   ELSE
     C*
     C                   MOVEL     DASHES        X1EXDT
     C*
     C                   ENDIF
     C*
     C     LGSENT        IFNE      *ZEROS
     C*
     C                   Z-ADD     LGSENT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXMDYY
     C*
     C                   MOVEL     XXMM          XFMM
     C                   MOVEL     XXDD          XFDD
     C                   MOVEL     XXYY          XFYY
     C                   MOVE      '/'           XFS1
     C                   MOVE      '/'           XFS2
     C                   MOVEL     XFMDY         X1SENT           10
     C*
     C                   ELSE
     C*
     C                   MOVEL     DASHES        X1SENT
     C*
     C                   ENDIF
     C*
     C     LGARCV        IFNE      *ZEROS
     C*
     C                   Z-ADD     LGARCV        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXMDYY
     C*
     C                   MOVEL     XXMM          XFMM
     C                   MOVEL     XXDD          XFDD
     C                   MOVEL     XXYY          XFYY
     C                   MOVE      '/'           XFS1
     C                   MOVE      '/'           XFS2
     C                   MOVEL     XFMDY         X1ARCV           10
     C*
     C                   ELSE
     C*
     C                   MOVEL     DASHES        X1ARCV
     C*
     C                   ENDIF
     C*
     C*  MOVE DATA TO SCREEN ARRAY
     C                   CLEAR                   DTA1
     C*
     C                   Z-ADD     1             Y                 2 0
     C                   MOVEA     LGJST         DTA1(Y)
     C                   Z-ADD     7             Y
     C                   MOVEA     LGTSET        DTA1(Y)
     C                   Z-ADD     14            Y
     C                   MOVEA     LGMTC         DTA1(Y)
     C                   Z-ADD     20            Y
     C                   MOVEA     LGSTS         DTA1(Y)
     C                   Z-ADD     24            Y
     C                   MOVEA     X1DUE         DTA1(Y)
     C                   Z-ADD     36            Y
     C                   MOVEA     X1EXDT        DTA1(Y)
     C                   Z-ADD     48            Y
     C                   MOVEA     X1SENT        DTA1(Y)
     C                   Z-ADD     60            Y
     C                   MOVEA     X1ARCV        DTA1(Y)
     C                   Z-ADD     73            Y
     C                   MOVEA     LGSTAT        DTA1(Y)
     C                   MOVEA(P)  DTA1          SCDATA
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 09/26/00 **************************************
     C*===============================================================
     C* OPT6 - SUBROUTINE FOR OPTION 6 (PTD/Reduced earnings/Recoveries)
     C*===============================================================
     C     OPT6          BEGSR
     C*
     C                   MOVE      @@CO#         Q#CO#
     C                   MOVE      @@FND         Q#FND
     C                   MOVE      @@FYR         Q#FYR
     C                   MOVE      @@CASE        Q#CASE
     C                   MOVE      SCDSQH        Q#SEQ#
     C*
     C                   CALL      'W0072R'                             99
     C                   PARM                    @@CALL
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    Q#CO#             3
     C                   PARM                    Q#FND             3
     C                   PARM                    Q#FYR             3
     C                   PARM                    Q#CASE            7
     C                   PARM                    Q#SEQ#            5
     C*
     C                   ENDSR                                                  OPT6
     C*** END ADD ***** 09/26/00 **************************************
     C*===============================================================
     C* OPT20 - Process option 20=Hold/release
     C*===============================================================
     C     OPT20         BEGSR
     C*
     C                   MOVEL     'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR                                                  OPT20
     C*** BEG ADD ****** 10/19/07 *************************************
     C*===============================================================
     C* OPT01 - Process option 01=Select
     C*===============================================================
     C     OPT01         BEGSR
     C***
     C                   MOVE      SCDBRR        RR#
     C***
     C                   ENDSR                                                  OPT01
     C*** END ADD ****** 10/19/07 *************************************
     C*** BEGIN ADD *** 05/01/07 **************************************
     C*===============================================================
     C* OPT01 - Process option 01=Select
     C*===============================================================
     C*****      OPT01     BEGSR
     C****
     C**                   MOVE SCDBRR    RR#
     C****
     C*****                ENDSR                           OPT01
     C*** END   ADD *** 05/01/07 **************************************
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C*** BEGIN ADD *** 09/26/00 **************************************
     C     SCSEL         CASEQ     '6'           OPT6
     C*** END ADD ***** 09/26/00 **************************************
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
     C*
     C*** BEG ADD ****** 10/19/07 ************************************
     C     Q1PRMS        IFGT      8
     C                   EXSR      OPT01
     C                   ENDIF
     C*** END ADD ****** 10/19/07 ************************************
     C*** BEGIN ADD *** 05/01/07 **************************************
     C****       Q1PRMS    IFGT 8
     C***                  EXSR OPT01
     C****                 ENDIF
     C*** END   ADD *** 05/01/07 **************************************
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   MOVE      '1'           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@FYR         KKFYR
     C                   MOVE      @@CASE        KKCASE
     C*
     C*  Load the screen headings.
     C*
     C                   MOVE      *BLANKS       SCCONM
     C     COKY          CHAIN     PCO01                              99        Company info
     C     *IN99         IFEQ      '0'
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCCONM
     C                   ENDIF
     C*
     C                   Z-ADD     KKFND         SCFND                          Group info
     C     FNKY          CHAIN     PFN01                              99
     C  N99              MOVEL(P)  FNNAME        SCFNAM
     C*
     C*
     C     CMKY          CHAIN     PCM01                              99        Claim info
     C     *IN99         IFEQ      '0'
     C*
     C                   MOVE      CMSS#         SS#                            SS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'SS#'         QQNAME           25
     C     SS#           PARM      SS#           QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C     SS1           CAT(P)    '-':0         SCSS#
     C                   CAT       SS2:0         SCSS#
     C                   CAT       '-':0         SCSS#
     C                   CAT       SS3:0         SCSS#
     C*
     C     CMLSN         CAT(P)    ',':0         SCCNAM                         Claimant name
     C                   CAT       CMFSN:1       SCCNAM
     C                   CAT       CMMID:1       SCCNAM
     C*
     C                   MOVEL(P)  CMFYR         SCCS#                          Case #
     C                   CAT       '-':0         SCCS#
     C                   MOVE      CMCASE        SCCS#
     C*
     C                   MOVEL(P)  CMACL#        SCSCL#                         State claim #
     C*
     C                   Z-ADD     CMACDT        SRDTI                          Accident date
     C                   CALL      'SRCVTDAT'    DTMDYY                          YMD --> MDY
     C                   MOVEL(P)  SRDTO         SCACDT
     C*
     C                   ENDIF
     C*
     C*** BEG ADD **** 02/16/06 ************************************************
     C*** Begin chg *** 06/06/08 **************************************
     C******************   MOVELQ1USER    XXPDSU  3
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*** End chg *** 06/06/08 **************************************
     C*
     C*** Begin del *** 06/06/08 **************************************
     C*********  XXPDSU    IFNE 'PDS'
     C*** Begin add *** 9/27/06 ***************************************
     C*********  XXPDSU    IFEQ 'P132'
     C*** End del *** 06/06/08 **************************************
     C*** Begin add *** 06/06/08 **************************************
     C     CLTID         IFEQ      '132'
     C*** End add *** 06/06/08 **************************************
     C*** End add ***** 9/27/06 ***************************************
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '04'          XXNMBR
     C                   EXSR      UPDOFA
     C*** Begin add *** 9/27/06 ***************************************
     C                   ENDIF
     C*** End add ***** 9/27/06 ***************************************
     C*** Begin del *** 06/06/08 **************************************
     C***********          ENDIF                           WCFL09=Y
     C*** End del *** 06/06/08 **************************************
     C*** END ADD **** 02/16/06 ************************************************
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C   21KEY2          READE     DBRCDF                                 95
     C*
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C   21KEY2          READPE    DBRCDF                                 95
     C*
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C   21KEY1          SETGT     DBRCDF                             96
     C*
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C   21KEY1          SETLL     DBRCDF                             96
     C*
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C*** BEG ADD **** 10/19/07 ************************************************
     C                   MOVEL(P)  DL            SCDATA
     C*** END ADD **** 10/19/07 ************************************************
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*** BEG ADD **** 05/01/07 ************************************************
     C     Q1PRMS        IFGT      8
MBHXXC                   MOVE      SCDBRR        RR#
     C                   ENDIF
     C*** END ADD **** 05/01/07 ************************************************
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C                   ENDSR
** Column headings
St    ID    Code  Sts   Due date    Extracted   Date sent   Received   Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
1066=PTD/Reduced Earnings   YYN
12020=Hold/Release          YNN
203F3=Exit                  NNN
205F5=Refresh               YNN
206F6=Add                   YNN
212F12=Previous             YYY
3
