     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W4100R - MULTIPLE REPORT GENERATION
     H Bnddir('WTBNDDIR')
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H* 03/25/14 C:295 Project: C126WEY                               *
     H*                Replace SA001R and use RTVCLTID                *
     H*                to fetch the CLTID                             *
     H*                                                               *
     H* 06/05/09 C:136 PROJECT: 139WLQ                                *
     H*                DEFAULT DWC FORMS WITH ENTERED CLAIMS          *
     H*                                                               *
     H* 05/25/07 C:191 PROJECT: WC1515                                *
     H*                 Check claims authority                        *
     H*                                                               *
     H* 06/29/06 C:15  Support: WC048/53027                           *
     H*                Added code to use a different table for the    *
     H*                DWC4 Disability Type entries.                  *
     H*                                                               *
     H* 09/21/05 C:132 Project: 48764                                 *
     H*                On screen 08, default the Payments As Of date  *
     H*                to be the system date.                         *
     H*                                                               *
     H* 04/01/05 C:15  Project: WC1276                                *
     H*                Added and removed fields for new DWC forms.    *
     H*                                                               *
     H*  03/08/05 C:53 Support: WC051/45997                           *
     H*                Load LTPIRT from claim file.                   *
     H*                                                               *
     H*  01/09/01 C:15 Support: WC113/25694                           *
     H*                Chgd display file to allow lower case on text  *
     H*                entry fields.                                  *
     H*                                                               *
     H*  07/07/00 C:15 Support: WC044/22681                           *
     H*                Allow entry of decimal for 'Total # of Weeks   *
     H*                of Entitlement'.                               *
     H*                                                               *
     H*  06/13/00 C:15 Support: WC048/23118                           *
     H*                Load the DWC4 'release to work date' with the  *
     H*                correct date.                                  *
     H*                                                               *
     H*  09/30/99 C:15 Project: WC577                                 *
     H*                Changed pgm to allow the users to update       *
     H*                existing requests.                             *
     H*                                                               *
     H*  03/09/99 C:61 PROJ: WC577
     H*              - V3 screen modifications
     H*              - F10=Search
     H*
     H*  10/19/98 C:02 PROJ: 48WLT
     H*                1. SCR07:
     H*                   DEFAULT MMI DATE AND CONTROVERTED FLAG TO
     H*                   WMCMP VALUES
     H*                2. SCR03:
     H*                   DEFAULT MMI DATE, DOD, RTW AND ACTUAL RTW TO
     H*                   WMCMP VALUES
     H*                3. SCR08:
     H*                   REMOVE FULL SALARY FLAG AND DATE.  ADD
     H*                   PAYEMTS AS OF DATE
     H*                                                                *
     H*  07/23/98 C:33 SEQ#: WC048/12782                               *
     H*                RECOMPILED DUE TO CHANGING SCNSS#'S TYPE TO 'M' *
     H*                TO ONLY ALLOW FOR NUMERIC ENTRY.                *
     H*                                                                *
     H*  06/04/98 C:33 SEQ#: WC033/12023                               *
     H*                CHANGE ALL VALUATION DATES TO 7.0               *
     H*  _____________________________________________________________ *
     H*                                                                *
     H*  10/03/91 TMN -REWRITE PGM TO GENERATE THE FOLLOWING FORMS:    *
     H*               .DWC-4  -SCREENS 3 & 4 (NOTICE OF SUSPENSION)    *
     H*               .DWC-12 (OLD DWC-6) -SCRNS 5 & 6 (NT OF DENIAL)  *
     H*               .DWC-3  -SCRN 7 (REQUEST FOR WAGE LOSS/BENEFITS) *
     H*               .DWC-13 (OLD DWC-2) -SCRN 8 (PROGRESS/FINAL RPT) *
     H*                                                                *
     H*  12/27/93 C:10 PROJ: WC287                                     *
     H*                DWC-4, SCREEN 3 -- CHANGED 'RETROACTIVE TO D/A' *
     H*                TO ALLOW BLANK.  REQUIRED EFFECTIVE DATE TO BE  *
     H*                ENTERED IF 'RETROACTIVE TO D/A' IS 'N'.         *
     H*                                                                *
     H*  02/14/95 C:15 PROJ: WC294                                     *
     H*                1. CHANGED SCR03 AND SCR04 DUE TO CHANGES TO    *
     H*                   THE DWC-4 FORM.                              *
     H*                2. REMOVED ENTRIES FROM SCR08 FOR THE DWC-13.   *
     H*                3. DEFAULTED THE 1ST TWO CC: LINES ON SCR04     *
     H*                   AND SCR05 TO BE THE EMP NAME AND ADDRESS.    *
     H*                                                                *
     H*  10/09/97 C:47 PROJ: WC692                                     *
     H*                CHANGED TO USE COMP RATE FROM WMCMP INSTEAD     *
     H*                OF CALCULATING FROM WDRBP.                      *
     H*                                                                *
     H*  PLEASE ADD TO TOP                                             *
     H******************************************************************
      *                                                                *
     FW4100F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWMUPL     IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FWMCML1    IF   E           K DISK
     FWMCMP     IF   E             DISK
     F                                     RENAME(PCM01:PCM02)
     FWMCDL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWALTL     UF A E           K DISK
     FWAL1L     UF A E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     FWMEML     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWDRBL     IF   E           K DISK
     F*** Begin add *** 06/29/06 **************************************
     FWTGTL     IF   E           K DISK
     F*** End add ***** 06/29/06 **************************************
     F*

      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
      ***  End   Add  ***  06/09/20  ****************************
     D***** Beg Add ***** 03/25/14 ***************************************
     D*=====================================================================================
     D* Proto-types
     D*=====================================================================================
     D* Get client ID
     D GetClientID     pr                  ExtPgm('RTVCLTID')
     D   @@CLT#                       3

     D*=====================================================================================
     D* Variables
     D*=====================================================================================
     D @@CLT#          s              3
     D***** End Add ***** 03/25/14 ***************************************

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  TYR                    1      4
     D                                     DIM(4)
     D  SCTYR1                 1      1
     D  SCTYR2                 2      2
     D  SCTYR3                 3      3
     D  SCTYR4                 4      4
     D                 DS
     D  XXCSZ                  1     40
     D  XXCTY                  1     25
     D  XXST                  27     28
     D  XXZIP                 31     39
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND             3 0
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     UPKEY1        KLIST
     C                   KFLD                    CMCLEX
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE            7 0
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#             9
     C*
     C*  PAYEE DEMOGRAPHICS FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    XXFEIN            9 0
     C                   KFLD                    XXMOD             3 0
     C*
     C*   KEY LIST FOR THE FUND DETAIL FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR THE FUND AUXILIARY FILE
     C*
     C     F2KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  PAYMENT FILE
     C*
     C     PAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE            7 0
     C*
     C*  WORK FILE
     C*
     C     LTKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C*
     C     LTKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    SCREQ#
     C*
     C*  WORK FILE
     C*
     C     L1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C*
     C*  KEY LIST FOR THE TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C*** Begin add *** 06/29/06 **************************************
     C*  KEY LIST FOR THE SYSTEM TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY            10
     C*** End add ***** 06/29/06 **************************************
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*  KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    XXFYR
     C*
     C*  KEY LIST FOR THE RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD            3
     C*
     C*  KEY LIST FOR THE STATE COMP FILE
     C*
     C     RBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXSTE             2
     C                   KFLD                    XXEFDT
     C*
     C     RBKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXSTE
     C*
     C     *LIKE         DEFINE    CMACDT        XXEFDT
     C***  BEGIN ADD  ***  06/04/98  *********************************
     C     *LIKE         DEFINE    CMACDT        XXMMID
     C     *LIKE         DEFINE    CMACDT        XXRSCD
     C     *LIKE         DEFINE    CMACDT        XXWLF1
     C     *LIKE         DEFINE    CMACDT        XXWLT1
     C     *LIKE         DEFINE    CMACDT        XXWLF2
     C     *LIKE         DEFINE    CMACDT        XXWLT2
     C     *LIKE         DEFINE    CMACDT        XXMMI3
     C     *LIKE         DEFINE    CMACDT        XXTPDT
     C***  END   ADD  ***  06/04/98  *********************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
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
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    #@FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
     C*** END ADD ****** 05/25/07 ********************************************
     C*
     C*** Begin Chg ***** 03/25/14 ***************************************
     C*** Begin add *** 04/01/05 ***************************************
     C*****              CALL      'SA001R'
     C*****              PARM                    @@WCIF            1
     C*****              PARM                    @@MCIF            1
     C*****              PARM                    @@WCIN            1
     C*****              PARM                    @@AR              1
     C*****              PARM                    @@NTWK            1
     C*****              PARM                    @@UR              1
     C*****              PARM                    @@MED             1
     C*****              PARM                    @@EDI             1
     C*****              PARM                    @@WCDK            1
     C*****              PARM                    @@RHAB            1
     C*****              PARM                    @@MSS             1
     C*****              PARM                    @@FSCH            1
     C*****              PARM                    @@PRVB            1
     C*****              PARM                    @@NPWC            1
     C*****              PARM                    @@OS              1
     C*****              PARM                    @@ARLS            1
     C*****              PARM                    @@CCOD            1
     C*****              PARM                    @@CRVW            1
     C*****              PARM                    @@PROM            1
     C*****              PARM                    @@MCRV            1
     C*****              PARM                    @@FREQ            1
     C*****              PARM                    @@CRAC            1
     C*****              PARM                    @@PREO            1
     C*****              PARM                    @@PARN            1
     C*****              PARM                    @@MGRI            1
     C*****              PARM                    @@KCIF            1
     C*****              PARM                    @@CURS            1
     C*****              PARM                    @@PCIB            1
     C*****              PARM                    @@CLT#            3
     C*****              PARM                    @@UCPF            1
     C*****              PARM                    @@DCF             1
     C*****              PARM                    @@AUTB            1
     C*
      /free
       // To retrieve the client id
       GetClientID(@@CLT#);
      /end-free
     C     @@CLT#        IFEQ      '051'
     C     @@CLT#        OREQ      '126'
     C                   SETON                                            91
     C                   END
     C*** End add ***** 04/01/05 ***************************************
     C*** End Chg ***** 03/25/14 ***************************************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C*
     C     UPKEY         CHAIN     PUP01                              61
     C     *IN61         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C                   MOVE      UPCO#         XXCO#             3 0
     C                   MOVE      UPFND         XXFND             3 0
     C                   Z-ADD     UPFYR         SCFYR
     C*
     C*  GET THE COMPANY AND FUND RECORDS
     C*
     C     COKEY         CHAIN     PCO01                              61
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C     FNKEY         CHAIN     PFN01                              61
     C                   MOVE      UPFND         SCFND
     C                   MOVE      FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
      ***** BEG DEL ******* 03/09/99 **********************************
     C***********          MOVE Q1USER    SCUSER
      ***** END DEL ******* 03/09/99 **********************************
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
      ***** BEG ADD ******* 03/09/99 **********************************
      *
      * Center Screen Prompt in display field
      *
     C                   MOVEL(P)  SCPRDS        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCPRDS
      *
      ***** END ADD ******* 03/09/99 **********************************
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR04'
     C                   EXFMT     SCR04
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR05'
     C                   EXFMT     SCR05
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR06'
     C                   EXFMT     SCR06
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR07'
     C                   EXFMT     SCR07
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR08'
     C                   EXFMT     SCR08
     C                   END
     C*
     C*** Begin add *** 04/01/05 **************************************
     C     Q2RECF        IFEQ      'SCR09'
     C                   EXFMT     SCR09
     C                   ENDIF                                                  Q2RECF = SCR09
     C*** End add ***** 04/01/05 **************************************
     C*
     C*   Process all function keys
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F3=Exit
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   F12=Cancel
     C*
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C     Q2RECF        IFEQ      'SCR03'
     C                   MOVEL     'SCR02'       Q2RECF
     C                   END
     C     Q2RECF        IFEQ      'SCR04'
     C                   MOVEL     'SCR03'       Q2RECF
     C                   END
     C     Q2RECF        IFEQ      'SCR05'
     C                   MOVEL     'SCR02'       Q2RECF
     C                   END
     C     Q2RECF        IFEQ      'SCR06'
     C                   MOVEL     'SCR05'       Q2RECF
     C                   END
     C     Q2RECF        IFEQ      'SCR07'
     C                   MOVEL     'SCR02'       Q2RECF
     C                   END
     C     Q2RECF        IFEQ      'SCR08'
     C                   MOVEL     'SCR02'       Q2RECF
     C                   END
     C*** Begin add *** 04/01/05 **************************************
     C     Q2RECF        IFEQ      'SCR09'
     C                   MOVEL     'SCR02'       Q2RECF
     C                   END
     C*** End add ***** 04/01/05 **************************************
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*  F10=Search
     C*
     C     *INKJ         IFEQ      '1'
      ***** BEG ADD ******* 03/09/99 **********************************
      * ZZZ
     C     Q2RECF        IFEQ      'SCR01'
      *
     C                   SELECT
      *
      * POLICY YEAR
      *
     C     HFLD          WHENEQ    'SCFYR'
     C                   CALL      'W0011R'
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      DBRRN         ##RRN             9 0
     C                   PARM      *ZERO         ##YR              3 0
      *
     C     ##YR          IFNE      *ZERO
     C                   Z-ADD     ##YR          SCFYR
     C                   SETON                                        31
     C                   ENDIF                                                  ##YR
      *
      * CASE
      *
     C     HFLD          WHENEQ    'SCCASE'
     C                   CALL      'W0070R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'CN'          ##SORT            2
     C                   PARM      DBRRN         ##RRN2            9 0
     C                   PARM      SCFYR         ##FYR             3 0
      *
     C     ##RRN         IFEQ      *ZERO
     C                   MOVE      '1'           *IN31
      *
     C                   ELSE
      *
     C     ##RRN         CHAIN     WMCMP                              60
     C                   MOVE      CMFYR         SCFYR
     C                   MOVE      CMCASE        SCCASE
     C                   MOVE      '1'           *IN32
     C                   END
      *
      * REQUEST NUMBER
      *
     C     HFLD          WHENEQ    'SCREQ#'
      *
      *  VALIDATE FUND YEAR
      *
     C                   Z-ADD     SCFYR         XXFYR
     C     F1KEY         CHAIN     PF101                              20
     C     *IN20         CABEQ     '1'           STRT
      *
      *  VALIDATE FYR AND CASE
      *
     C                   MOVE      SCCASE        XXCASE
     C     CMKEY         CHAIN     PCM01                              21
     C     *IN21         CABEQ     '1'           STRT
      *
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         #@FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
      *
     C                   MOVE      SCFYR         #1FYR             3
     C                   MOVE      SCCASE        ##CASE            7
     C                   MOVE      SCREQ#        ##REQ#            5
      *
     C                   CALL      'W4104R'
     C                   PARM                    QQPRDS
     C                   PARM      '2'           ##CALL
     C                   PARM                    #1FYR
     C                   PARM                    ##CASE
     C                   PARM                    ##REQ#
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     ##RTCD        IFEQ      'B'
     C     ##REQ#        OREQ      *ZERO
     C                   MOVE      '1'           *IN32
      *
     C                   ELSE
      *
     C                   MOVE      ##REQ#        SCREQ#
     C                   GOTO      SCR01T
     C                   ENDIF                                                  QQRTCD
      *
     C                   ENDSL
      *
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  Q2RECF=SCR01
      *
      *
      ***** END ADD ******* 03/09/99 **********************************
     C*
     C*** Begin dlt *** 04/01/05 **************************************
     C*  INDEMNITY START TYPE - SCR03
     C*
     C***        Q2RECF    IFEQ 'SCR03'
     C***        SCISTT    ANDEQ'?'
      ***** BEG ADD ******* 03/09/99 **********************************
     C***        HFLD      OREQ 'SCISTT'
      ***** END ADD ******* 03/09/99 **********************************
     C***                  MOVE *BLANK    QQKEY  10
     C***                  CALL 'W2147R1'
     C***                  PARM           QQPRDS
     C***                  PARM           QQCALL
     C***                  PARM           QQKEY
     C***                  PARM           QQRTC   1
     C***                  MOVELQQKEY     SCISTT
     C***        QQRTC     CABEQ'A'       TERM
     C***                  MOVE *BLANK    Q2RECF
     C***                  MOVEL'SCR03'   Q2RECF
     C*
     C***        QQRTC     IFEQ 'B'
     C***        QQKEY     OREQ *BLANK
     C***                  MOVE '1'       *IN41
     C***                  GOTO STRT
     C***                  END
     C*
     C***        QQRTC     IFEQ '*'
     C***                  MOVE '1'       *IN44
     C***                  GOTO STRT
     C***                  END
     C*
     C***                  END
     C*** End dlt ***** 04/01/05 **************************************
     C*
     C*  INDEMNITY SUSPENDED TYPE - SCR03
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C     SCISUT        ANDEQ     '?'
      ***** BEG ADD ******* 03/09/99 **********************************
     C     HFLD          OREQ      'SCISUT'
      ***** END ADD ******* 03/09/99 **********************************
     C                   MOVE      *BLANK        QQKEY            10
     C                   CALL      'W2148R1'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     QQKEY         SCISUT
     C     QQRTC         CABEQ     'A'           TERM
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN45
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  INDEMNITY REINSTATEMENT TYPE - SCR03
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C     SCIRIT        ANDEQ     '?'
      ***** BEG ADD ******* 03/09/99 **********************************
     C     HFLD          OREQ      'SCIRIT'
      ***** END ADD ******* 03/09/99 **********************************
     C                   MOVE      *BLANK        QQKEY
     C                   CALL      'W2147R1'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C                   MOVEL     QQKEY         SCIRIT
     C     QQRTC         CABEQ     'A'           TERM
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN43
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN46
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*** Begin add *** 04/01/05 **************************************
     C*  Benefit adjustment code 1 - SCR04
     C*
     C     HFLD          IFEQ      'SCBAJ1'
     C*
     C                   MOVE      *BLANK        QQKEY
     C                   CALL      'W2149R1'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN71
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVEL     QQKEY         SCBAJ1
     C                   MOVE      '1'           *IN72
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  Benefit adjustment code 2 - SCR04
     C*
     C     HFLD          IFEQ      'SCBAJ2'
     C*
     C                   MOVE      *BLANK        QQKEY
     C                   CALL      'W2149R1'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN72
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVEL     QQKEY         SCBAJ2
     C                   MOVE      '1'           *IN41
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*** End add ***** 04/01/05 **************************************
     C*
     C*  BENEFIT ADJUSTMENT TYPE 1 - SCR04
     C*
     C     Q2RECF        IFEQ      'SCR04'
     C     SCATY1        ANDEQ     '?'
      ***** BEG ADD ******* 03/09/99 **********************************
     C     HFLD          OREQ      'SCATY1'
      ***** END ADD ******* 03/09/99 **********************************
     C*
     C                   MOVE      *BLANKS       QQKEY
     C*** Begin dlt *** 06/29/06 **************************************
     C*****                CALL 'W2149R1'
     C*****                PARM           QQPRDS
     C*****                PARM           QQCALL
     C*****                PARM           QQKEY
     C*****                PARM           QQRTC
     C*** End dlt ***** 06/29/06 **************************************
     C*** Begin add *** 06/29/06 **************************************
     C                   MOVEL(P)  'DSTP'        XXTBID            4
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*** End add ***** 06/29/06 **************************************
     C*
     C                   MOVEL     QQKEY         SCATY1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN41
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  BENEFIT ADJUSTMENT TYPE 2 - SCR04
     C*
     C     Q2RECF        IFEQ      'SCR04'
     C     SCATY2        ANDEQ     '?'
      ***** BEG ADD ******* 03/09/99 **********************************
     C     HFLD          OREQ      'SCATY2'
      ***** END ADD ******* 03/09/99 **********************************
     C                   MOVE      *BLANK        QQKEY
     C*
     C*** Begin dlt *** 06/29/06 **************************************
     C*****                CALL 'W2149R1'
     C*****                PARM           QQPRDS
     C*****                PARM           QQCALL
     C*****                PARM           QQKEY
     C*****                PARM           QQRTC
     C*** End dlt ***** 06/29/06 **************************************
     C*** Begin add *** 06/29/06 **************************************
     C                   MOVEL(P)  'DSTP'        XXTBID            4
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*** End add ***** 06/29/06 **************************************
     C*
     C                   MOVEL     QQKEY         SCATY2
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN43
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              61
     C     *IN61         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C     Q2RECF        CABEQ     'SCR04'       SCR04T
     C     Q2RECF        CABEQ     'SCR05'       SCR05T
     C     Q2RECF        CABEQ     'SCR06'       SCR06T
     C     Q2RECF        CABEQ     'SCR07'       SCR07T
     C     Q2RECF        CABEQ     'SCR08'       SCR08T
     C*** Begin add *** 04/01/05 **************************************
     C     Q2RECF        CABEQ     'SCR09'       SCR09T
     C*** End add ***** 04/01/05 **************************************
     C*
     C*****************************************************************
     C*   PROCESS REQUEST SCREEN 1                                    *
     C*****************************************************************
     C*
     C     SCR01T        TAG
     C*
     C*  VALIDATE FUND YEAR
     C*
     C                   Z-ADD     SCFYR         XXFYR             3 0
     C     F1KEY         CHAIN     PF101                              20
     C     *IN20         CABEQ     '1'           STRT
     C*
     C*  VALIDATE FYR AND CASE
     C*
     C                   MOVE      SCCASE        XXCASE
     C     CMKEY         CHAIN     PCM01                              21
     C     *IN21         CABEQ     '1'           STRT
      *
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         #@FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
     C*
     C*  CHECK THE ACCIDENT DATE
     C*
     C     CMACDT        COMP      940101                               70
     C*
     C*  GET THE EMPLOYER NAME AND MAILING ADDRESS
     C*
     C                   MOVE      *BLANKS       XXENAM           40
     C                   MOVE      *BLANKS       XXADR1           40
     C                   MOVE      *BLANKS       XXADR2           40
     C                   MOVE      *BLANKS       XXCTY
     C                   MOVE      *BLANKS       XXST
     C                   MOVE      *BLANKS       XXZIP
     C     EMKY          CHAIN     PEM01                              60
     C  N60              MOVEL     EMNAME        XXENAM
     C  N60              MOVEL     EMMAD1        XXADR1
     C  N60              MOVEL     EMMAD2        XXADR2
     C  N60              MOVEL     EMMCTY        XXCTY
     C  N60              MOVEL     EMMST         XXST
     C  N60              MOVEL     EMMZIP        XXZIP
     C*
     C*   CHECK FOR REQUEST # AND LOAD SCREENS
     C*
     C     SCREQ#        IFEQ      *ZERO
     C*
     C*   GET NEXT AVAILABLE REQUEST NUMBER FOR CASE
     C*
     C     LTKEY         SETGT     PLT01
     C                   READP     PLT01                                  60
     C  N60              EXCEPT    PLT01E
     C  N60LTCO#         COMP      XXCO#                              6060
     C  N60LTFND         COMP      XXFND                              6060
     C  N60LTFYR         COMP      XXFYR                              6060
     C  N60LTCASE        COMP      XXCASE                             6060
     C*
     C     *IN60         IFEQ      '0'
     C     LTREQ#        ADD       1             SCREQ#
     C                   ELSE
     C                   Z-ADD     1             SCREQ#
     C                   END
     C*
     C*   LOAD BLANK FIELDS
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANKS       SCDWC4
     C                   MOVE      *BLANKS       SCDW12
     C                   MOVE      *BLANKS       SCDWC3
     C                   MOVE      *BLANKS       SCDW13
     C*** Begin add *** 04/01/05 **************************************
     C                   MOVE      *BLANKS       SCDW19
     C*** End add ***** 04/01/05 **************************************
     C                   MOVE      *BLANK        XXDWC4            1
     C                   MOVE      *BLANK        XXDW12            1
     C                   MOVE      *BLANK        XXDWC3            1
     C                   MOVE      *BLANK        XXDW13            1
     C*** Begin add *** 04/01/05 **************************************
     C                   MOVE      *BLANK        XXDW19            1
     C*** End add ***** 04/01/05 **************************************
     C*
     C*  SCREEN 3 FIELDS FOR DWC-4
     C*
     C*** Begin dlt *** 04/01/05 **************************************
     C***                  MOVE *ZEROS    SCISTD           INDEM START DATE
     C***                  MOVE *BLANK    SCISTT           INDEM START TYPE
     C*** End dlt ***** 04/01/05 **************************************
     C                   MOVE      *ZEROS        SCISUD
     C                   MOVE      *BLANK        SCISUT                         & TYPE
     C                   MOVE      *ZEROS        SCIRID                         INDEM REINSTATE DATE
     C                   MOVE      *BLANK        SCIRIT                         & TYPE
     C*** BEG DEL *** 03/08/05 ****************************
     C***********          MOVE *ZEROS    SCPIRT           PI RATING %
     C*** END DEL *** 03/08/05 ****************************
     C*** Begin add *** 04/01/05 **************************
     C                   MOVE      *BLANKS       SCPIRT                         PI RATING %
     C*** End add ***** 04/01/05 **************************
     C                   MOVE      *ZEROS        SCMMID                         MMI DATE
     C                   MOVE      *ZEROS        SCRTWD                         RETURN TO WORK DATE
     C                   MOVE      *ZEROS        SCRRTD                         RLSD TO RTW DATE
     C                   MOVE      *BLANKS       SCRRTF                         RLSD TO RTW RESTRICT
     C                   MOVE      *BLANKS       SCRTWF                         RTW RESTRICTION FLAG
     C                   MOVE      *ZEROS        SCDTHD                         DATE OF DEATH
     C                   MOVE      *BLANK        SCREDA                         RETROACTIVE FLAG
     C                   MOVE      *ZEROS        SCREEF                         RETRO EFF DATE
     C                   MOVE      *ZEROS        SCPAWW                         PREVIOUS AWW
     C                   MOVE      *ZEROS        SCPCRT                         PREVIOUS COMP RATE
     C                   MOVE      *ZEROS        SCAAWW                         AMENDED AWW
     C                   MOVE      *ZEROS        SCACRT                         AMENDED COMP RATE
     C                   MOVE      *ZEROS        SCRCUP                         RECOUPED AMT PER WK
     C                   MOVE      *ZEROS        SCWK#                          # OF WEEKS RECOUPED
     C                   MOVE      *ZEROS        SCPID4                         PI DATE FOR DWC-4
     C                   MOVE      *ZEROS        SCPTD4                         PT DATE FOR DWC-4
     C                   MOVE      *ZEROS        SCPTRT                         PT SUPP RATE
     C                   MOVE      *ZEROS        SCPTEF                         PT SUPP EFF DATE
     C                   MOVE      *ZEROS        SCFISD                         INDMNTY SETTLEMENT D
     C*** BEGIN ADD *** 10/19/98 **************************************
     C                   Z-ADD     CMMMI         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCMMID
     C                   Z-ADD     CMDOD         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTHD
     C*** BEGIN CHG *** 06/13/00 **************************************
     C******************** Z-ADDCMERTW    SRDTI
     C                   Z-ADD     CMRLW         SRDTI
     C*** END CHG ***** 06/13/00 **************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRRTD
     C                   Z-ADD     CMRTW         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRTWD
     C*** END ADD *** 10/19/98 ****************************************
     C*
     C*  SCREEN 4 FIELDS FOR DWC-4
     C                   MOVE      *BLANK        SCCSS#                         CORRECT SS# FLAG
     C                   MOVE      *BLANK        SCCDA                          CORRECT D/A FLAG
     C                   MOVE      *BLANK        SCCNAM                         CORRECT NAME FLAG
     C                   MOVE      *BLANK        SCNSS#                         CORRECT SS#
     C                   MOVE      *ZEROS        SCNDA                          CORRECT D/A
     C                   MOVE      *BLANK        SCNNAM                         CORRECT NAME
     C                   MOVE      *BLANKS       SCRMK1                         REMARKS
     C                   MOVE      *BLANKS       SCRMK2
     C                   MOVE      *BLANKS       SCRMK3
     C                   MOVE      *BLANKS       SCRMK4
     C                   MOVE      *BLANKS       SCCC1                          'COPY SENT TO' 1
     C                   MOVE      *BLANKS       SCCC2                          'COPY SENT TO' 2
     C                   MOVE      *BLANKS       SCCC3                          'COPY SENT TO' 3
     C                   MOVE      *BLANKS       SCCC4                          'COPY SENT TO' 4
     C                   MOVEL     XXENAM        SCCC1
     C                   MOVEL     XXADR1        SCCC2
     C     XXADR2        IFNE      *BLANKS
     C                   MOVEL     XXADR2        SCCC3
     C                   MOVEL     XXCSZ         SCCC4
     C                   ELSE
     C                   MOVEL     XXCSZ         SCCC3
     C                   MOVE      *BLANKS       SCCC4
     C                   END
     C*** Begin add *** 04/01/05 **************************************
     C                   MOVE      *BLANKS       SCBAJ1                         ADJUSTMENT TYPE 1
     C                   MOVE      *BLANKS       SCBAJ2                         ADJUSTMENT TYPE 2
     C*** End add ***** 04/01/05 **************************************
     C                   MOVE      *BLANKS       SCATY1                         ADJUSTMENT TYPE 1
     C                   MOVE      *BLANKS       SCATY2                         ADJUSTMENT TYPE 2
     C                   MOVE      *ZEROS        SCAAM1                         WEEKLY ADJ AMT 1
     C                   MOVE      *ZEROS        SCAAM2                         WEEKLY ADJ AMT 2
     C                   MOVE      *ZEROS        SCAEF1                         ADJ EFF DT 1
     C                   MOVE      *ZEROS        SCAEF2                         ADJ EFF DT 2
     C                   MOVE      *ZEROS        SCACN1                         ADJ END DT 1
     C                   MOVE      *ZEROS        SCACN2                         ADJ END DT 2
     C                   MOVE      *BLANKS       SCSAGT                         CORRECT SRV AGT FLAG
     C                   MOVE      *BLANKS       SCNAGT                         CORRECT SRV AGENT
     C*  SCREEN 5 FIELDS FOR DWC-12 (OLD DWC-6)
     C                   MOVE      *ZEROS        SCRSCD                         RESCINDED DATE
     C                   MOVE      *BLANKS       SCDEN1                         BENEFITS DENIED 1
     C                   MOVE      *BLANKS       SCDEN2                         BENEFITS DENIED 2
     C                   MOVE      *BLANKS       SCDEN3                         BD 3
     C                   MOVE      *BLANKS       SCDEN4                         BD 4
     C                   MOVE      *BLANKS       SCDEN5                         BD 5
     C*** BEG ADD ***** 06/05/09 **************************************
     C                   Z-ADD     CMDRES        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRSCD
     C                   MOVEL     'DENY'        XXTBID
     C                   MOVEL     CMDNY1        SCDEN1
     C     SCDEN1        IFNE      *BLANKS
     C                   MOVEL     SCDEN1        XXKEY
     C     GTKY          CHAIN     PGT01                              20
     C                   CAT       GTDESC:1      SCDEN1
     C                   ENDIF
     C                   MOVEL     CMDNY2        SCDEN2
     C     SCDEN2        IFNE      *BLANKS
     C                   MOVEL     SCDEN2        XXKEY
     C     GTKY          CHAIN     PGT01                              20
     C                   CAT       GTDESC:1      SCDEN2
     C                   ENDIF
     C                   MOVEL     CMDNY3        SCDEN3
     C     SCDEN3        IFNE      *BLANKS
     C                   MOVEL     SCDEN3        XXKEY
     C     GTKY          CHAIN     PGT01                              20
     C                   CAT       GTDESC:1      SCDEN3
     C                   ENDIF
     C                   MOVEL     CMDNY4        SCDEN4
     C     SCDEN4        IFNE      *BLANKS
     C                   MOVEL     SCDEN4        XXKEY
     C     GTKY          CHAIN     PGT01                              20
     C                   CAT       GTDESC:1      SCDEN4
     C                   ENDIF
     C                   MOVEL     CMDNY5        SCDEN5
     C     SCDEN5        IFNE      *BLANKS
     C                   MOVEL     SCDEN5        XXKEY
     C     GTKY          CHAIN     PGT01                              20
     C                   CAT       GTDESC:1      SCDEN5
     C                   ENDIF
     C*** END ADD ***** 06/05/09 **************************************
     C                   MOVE      *BLANKS       SCDCC1                         CC 1  FOR DWC-12
     C                   MOVE      *BLANKS       SCDCC2                         CC 2
     C                   MOVE      *BLANKS       SCDCC3                         CC 3
     C                   MOVE      *BLANKS       SCDCC4                         CC 4
     C                   MOVEL     XXENAM        SCDCC1
     C                   MOVEL     XXADR1        SCDCC2
     C     XXADR2        IFNE      *BLANKS
     C                   MOVEL     XXADR2        SCDCC3
     C                   MOVEL     XXCSZ         SCDCC4
     C                   ELSE
     C                   MOVEL     XXCSZ         SCDCC3
     C                   MOVE      *BLANKS       SCDCC4
     C                   END
     C*  SCREEN 6 FIELDS FOR DWC-12 (OLD DWC-6)
     C                   MOVE      *BLANKS       SCWHY0                         DENIAL REASON 1
     C                   MOVE      *BLANKS       SCWHY1
     C                   MOVE      *BLANKS       SCWHY2
     C                   MOVE      *BLANKS       SCWHY3
     C                   MOVE      *BLANKS       SCWHY4
     C                   MOVE      *BLANKS       SCWHY5
     C                   MOVE      *BLANKS       SCWHY6                         DENIAL REASON 7
     C                   MOVE      *BLANKS       SCWHY7                         REASON 8
     C                   MOVE      *BLANKS       SCWHY8                         REASON 9
     C                   MOVE      *BLANKS       SCWHY9                         REASON 10
      *** BEG ADD ***** 06/05/09 *****************************************
     C     CMDNYN        IFNE      *BLANKS
     C                   Z-ADD     60            QQSIZE
     C                   CALL      'SRLINE'
     C                   PARM      CMDNYN        QQLINE          256
     C                   PARM                    QQSIZE            3 0
     C                   PARM      *BLANKS       QQRMDR          256
     C                   MOVEL     QQLINE        SCWHY0
     C     QQRMDR        IFNE      *BLANKS
     C                   CALL      'SRLINE'
     C                   PARM      QQRMDR        QQLINE
     C                   PARM                    QQSIZE
     C                   PARM      *BLANKS       QQRMDR
     C                   MOVEL     QQLINE        SCWHY1
     C                   ENDIF
     C     QQRMDR        IFNE      *BLANKS
     C                   CALL      'SRLINE'
     C                   PARM      QQRMDR        QQLINE
     C                   PARM                    QQSIZE
     C                   PARM      *BLANKS       QQRMDR
     C                   MOVEL     QQLINE        SCWHY2
     C                   ENDIF
     C     QQRMDR        IFNE      *BLANKS
     C                   CALL      'SRLINE'
     C                   PARM      QQRMDR        QQLINE
     C                   PARM                    QQSIZE
     C                   PARM      *BLANKS       QQRMDR
     C                   MOVEL     QQLINE        SCWHY3
     C                   ENDIF
     C                   ENDIF
      *** END ADD ***** 06/05/09 *****************************************
     C*  SCREEN 7 FIELDS FOR DWC-3
     C                   MOVE      *ZEROS        SCWLF1                         WL WEEK 1 FROM DT
     C                   MOVE      *ZEROS        SCWLT1                         WL WEEK 1 TO DATE
     C                   MOVE      *ZEROS        SCWLF2                         WL WEEK 2 FROM DT
     C                   MOVE      *ZEROS        SCWLT2                         WL WEEK 2 TO DATE
     C                   MOVE      *ZEROS        SCMMI3                         MMI DATE FOR DWC-3
     C                   MOVE      *ZEROS        SCWLR3                         RATING % FOR DWC-3
     C                   MOVE      *BLANKS       SCWL                           WAGE LOSS FLAG
     C                   MOVE      *BLANKS       SCTP                           TEMP PARTIAL FLAG
     C                   MOVE      *BLANKS       SCCV                           CONTROVERTED FLAG
     C*** BEGIN ADD *** 10/19/98 **************************************
     C                   Z-ADD     CMMMI         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCMMI3
     C     CMCVFL        IFEQ      'Y'
     C                   MOVE      'X'           SCCV
     C                   ELSE
     C                   MOVE      *BLANKS       SCCV
     C                   ENDIF
     C*** END ADD *** 10/19/98 ****************************************
     C*  SCREEN 8 FIELDS FOR DWC-13 (OLD DWC-2)
     C                   Z-ADD     CMAWW         SCWAGE
     C                   MOVE      *ZEROS        SCRATE
     C                   MOVE      *BLANKS       SCTYR1
     C                   MOVE      *BLANKS       SCTYR2
     C                   MOVE      *BLANKS       SCTYR3
     C*** BEGIN DEL *** 10/19/98 **************************************
     C**********           MOVE *BLANKS   SCFSAL           FULL SALARY FLAG
     C**********           MOVE *ZEROS    SCFSDT           FS PAID DATE
     C*** END DEL *** 10/19/98 ****************************************
      *
      ***** BEG CHG ***** 09/21/05 ************************************
     C************         MOVE *ZEROS    SCPAOF           FS PAID DATE
     C                   Z-ADD     DSDATE        SCPAOF                         FS PAID DATE
      ***** END CHG ***** 09/21/05 ************************************
      *
     C*** Begin add *** 04/01/05 **************************************
     C*  SCREEN 9 FIELDS FOR DWC-19
     C                   Z-ADD     *ZERO         SCEEFR
     C                   Z-ADD     *ZERO         SCEETO
     C*** End add ***** 04/01/05 **************************************
     C*
     C                   ELSE
     C*
     C*   LOAD SCREEN WITH INFORMATION FOR RECORD
     C*
     C     LTKEY1        CHAIN     PLT01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   ELSE
     C*
     C                   EXCEPT    PLT01E
     C                   MOVE      UA(2)         SCMODE
     C*** BEGIN DLT *** 09/30/99 **************************************
     C*******************  MOVE LTDWC4    SCDWC4
     C*******************  MOVE LTDW12    SCDW12
     C*******************  MOVE LTDWC3    SCDWC3
     C*******************  MOVE LTDW13    SCDW13
     C*******************  MOVE 'Y'       XXDWC4  1
     C*******************  MOVE 'Y'       XXDW12  1
     C*******************  MOVE 'Y'       XXDWC3  1
     C*******************  MOVE 'Y'       XXDW13  1
     C*** END DLT ***** 09/30/99 **************************************
     C*** BEGIN ADD *** 09/30/99 **************************************
     C     LTDWC4        IFEQ      'X'
     C                   MOVE      ' 1'          SCDWC4
     C                   ENDIF
     C     LTDW12        IFEQ      'X'
     C                   MOVE      ' 1'          SCDW12
     C                   ENDIF
     C     LTDWC3        IFEQ      'X'
     C                   MOVE      ' 1'          SCDWC3
     C                   ENDIF
     C     LTDW13        IFEQ      'X'
     C                   MOVE      ' 1'          SCDW13
     C                   ENDIF
     C*** END ADD ***** 09/30/99 **************************************
     C*
     C*  SCREEN 3 FIELDS FOR DWC-4
     C*
     C*** Begin dlt *** 04/01/05 **************************************
     C***                  Z-ADDLTISTD    SRDTI
     C***                  CALL 'SRCVTDAT'DTMDYY
     C***                  MOVELSRDTO     SCISTD
     C***                  MOVE LTISTT    SCISTT           INDEM START TYPE
     C*** End dlt ***** 04/01/05 **************************************
     C                   Z-ADD     LTISUD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCISUD
     C                   MOVE      LTISUT        SCISUT                         & TYPE
     C                   Z-ADD     LTIRID        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCIRID
     C                   MOVE      LTIRIT        SCIRIT                         & TYPE
     C*** BEG DEL *** 03/08/05 ****************************
     C***********          Z-ADDLTPIRT    SCPIRT           PI RATING %
     C*** END DEL *** 03/08/05 ****************************
     C*** Begin add *** 04/01/05 **************************
     C                   MOVEL     LTPIRA        SCPIRT                         PI RATING %
     C*** End add ***** 04/01/05 **************************
     C                   Z-ADD     LTMMID        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCMMID
     C                   Z-ADD     LTRTWD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRTWD
     C                   Z-ADD     LTDTHD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTHD
     C*
     C                   MOVE      LTREDA        SCREDA
     C*
     C                   Z-ADD     LTREEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCREEF
     C                   Z-ADD     LTPAWW        SCPAWW                         PREVIOUS AWW
     C                   Z-ADD     LTPCRT        SCPCRT                         PREVIOUS COMP RATE
     C                   Z-ADD     LTAAWW        SCAAWW                         AMENDED AWW
     C                   Z-ADD     LTACRT        SCACRT                         AMENDED COMP RATE
     C                   Z-ADD     LTRCUP        SCRCUP                         RECOUPED AMT PER WK
     C*** BEGIN CHG *** 07/07/00 **************************************
     C******************** Z-ADDLTWK#     SCWK#            # OF WEEKS RECOUPED
     C                   Z-ADD     LTWKEN        SCWK#                          # OF WEEKS RECOUPED
     C*** END CHG ***** 07/07/00 **************************************
     C                   Z-ADD     LTPID4        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPID4
     C                   Z-ADD     LTPTD4        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPTD4
     C                   Z-ADD     LTPTRT        SCPTRT                         PT SUPP RATE
     C                   Z-ADD     LTPTEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPTEF
     C                   Z-ADD     LTIRCD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRRTD
     C                   MOVE      LTIRCT        SCRRTF                         RLSD TO RTW RESTRICT
     C                   MOVE      LTRTWF        SCRTWF                         RTW RESTRICTION FLAG
     C                   Z-ADD     LTAJDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFISD
     C*
     C*  SCREEN 4 FIELDS FOR DWC-4
     C                   MOVE      LTCSS#        SCCSS#                         CORRECT SS# FLAG
     C                   MOVE      LTCDA         SCCDA                          CORRECT D/A FLAG
     C                   MOVE      LTCNAM        SCCNAM                         CORRECT NAME FLAG
      *****  Begin Chg  ***  06/09/20  **********************
     C*****              MOVEL     LTNSS#        SCNSS#                         CORRECT SS#
      /Free
       SCNSS# = MaskData('SSN':'LTNSS#':LTNSS#);
      /End-Free
      *****  End Chg  ***  06/09/20  **********************
     C                   Z-ADD     LTNDA         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCNDA
     C                   MOVEL     LTNNAM        SCNNAM                         CORRECT NAME
     C                   MOVEL     LTRMK1        SCRMK1                         REMARKS
     C                   MOVEL     LTRMK2        SCRMK2
     C                   MOVEL     LTRMK3        SCRMK3
     C                   MOVEL     LTRMK4        SCRMK4
     C                   MOVEL     LTCC1         SCCC1                          'COPY SENT TO' 1
     C                   MOVEL     LTCC2         SCCC2                          'COPY SENT TO' 2
     C                   MOVEL     LTCC3         SCCC3                          'COPY SENT TO' 3
     C                   MOVEL     LTCC4         SCCC4                          'COPY SENT TO' 4
     C*** Begin add *** 04/01/05 **************************************
     C                   MOVE      LTBAJ1        SCBAJ1                         ADJUSTMENT TYPE 1
     C                   MOVE      LTBAJ2        SCBAJ2                         ADJUSTMENT TYPE 2
     C*** End add ***** 04/01/05 **************************************
     C                   MOVE      LTATY1        SCATY1                         ADJUSTMENT TYPE 1
     C                   MOVE      LTATY2        SCATY2                         ADJUSTMENT TYPE 2
     C                   Z-ADD     LTUNPT        SCAAM1                         WEEKLY ADJ AMT 1
     C                   Z-ADD     LTOVPT        SCAAM2                         WEEKLY ADJ AMT 2
     C                   Z-ADD     LTAEF1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCAEF1
     C                   Z-ADD     LTAEF2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCAEF2
     C                   Z-ADD     LTACN1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACN1
     C                   Z-ADD     LTACN2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACN2
     C                   MOVE      LTSAGT        SCSAGT                         CORRECT SRV AGT FLAG
     C                   MOVE      LTNAGT        SCNAGT                         CORRECT SRV AGENT
     C*
     C*  SCREEN 5 NEW FIELDS FOR DWC-12 (OLD DWC-6)
     C                   Z-ADD     LTRSCD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRSCD
     C                   MOVE      LTDEN1        SCDEN1                         BENEFITS DENIED 1
     C                   MOVEL     LTDEN2        SCDEN2                         BENEFITS DENIED 2
     C                   MOVEL     LTDEN3        SCDEN3                         BD 3
     C                   MOVEL     LTDEN4        SCDEN4                         BD 4
     C                   MOVEL     LTDEN5        SCDEN5                         BD 5
     C                   MOVEL     LTDCC1        SCDCC1                         CC 1  FOR DWC-12
     C                   MOVEL     LTDCC2        SCDCC2                         CC 2
     C                   MOVEL     LTDCC3        SCDCC3                         CC 3
     C                   MOVEL     LTDCC4        SCDCC4                         CC 4
     C*
     C*  SCREEN 6 FIELDS FOR DWC-12
     C                   MOVEL     LTWHY0        SCWHY0                         DENIAL REASON 1
     C                   MOVE      LTWHY1        SCWHY1
     C                   MOVE      LTWHY2        SCWHY2
     C                   MOVE      LTWHY3        SCWHY3
     C                   MOVE      LTWHY4        SCWHY4
     C                   MOVE      LTWHY5        SCWHY5
     C                   MOVEL     LTWHY6        SCWHY6                         DENIAL REASON 7
     C                   MOVEL     LTWHY7        SCWHY7                         REASON 8
     C                   MOVEL     LTWHY8        SCWHY8                         REASON 9
     C                   MOVEL     LTWHY9        SCWHY9                         REASON 10
     C*
     C*  SCREEN 7 NEW FIELDS FOR DWC-3
     C                   Z-ADD     LTWLF1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCWLF1
     C                   Z-ADD     LTWLT1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCWLT1
     C                   Z-ADD     LTWLF2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCWLF2
     C                   Z-ADD     LTWLT2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCWLT2
     C                   Z-ADD     LTMMI3        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCMMI3
     C                   Z-ADD     LTWLR3        SCWLR3                         RATING % FOR DWC-3
     C                   MOVE      LTWL          SCWL                           WAGE LOSS FLAG
     C                   MOVE      LTTP          SCTP                           TEMP PARTIAL FLAG
     C                   MOVE      LTCV          SCCV                           CONTROVERTED FLAG
     C*
     C*  SCREEN 8 NEW FIELDS FOR DWC-13 (OLD DWC-2)
     C                   Z-ADD     CMAWW         SCWAGE
     C                   MOVE      *ZERO         SCRATE
     C                   MOVE      *BLANK        SCTYR1
     C                   MOVE      *BLANK        SCTYR2
     C                   MOVE      *BLANK        SCTYR3
     C     LTRPTP        IFEQ      'I'
     C                   MOVE      'X'           SCTYR1
     C                   END
     C     LTRPTP        IFEQ      'F'
     C                   MOVE      'X'           SCTYR2
     C                   END
     C     LTRPTP        IFEQ      'A'
     C                   MOVE      'X'           SCTYR3
     C                   END
     C*** BEGIN DEL *** 10/19/98 **************************************
     C**********           MOVE LTFSAL    SCFSAL           FULL SALARY FLAG
     C**********           Z-ADDLTFSDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDYY
     C**********           MOVELSRDTO     SCFSDT
     C                   Z-ADD     LTFSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPAOF
     C*** END DEL *** 10/19/98 ****************************************
     C*** Begin add *** 04/01/05 **************************************
     C*  SCREEN 9 NEW FIELDS FOR DWC-19
     C                   Z-ADD     LTEEFR        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEEFR
     C                   Z-ADD     LTEETO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEETO
     C*** End add ***** 04/01/05 **************************************
     C*
     C                   END
     C                   END
     C*
     C*   GET RECORD FOR DWC-13 FIELDS
     C*
     C     L1KEY         CHAIN     PL101                              60
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    PL101E
     C                   Z-ADD     L1TPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTPDT
     C                   ELSE
     C                   Z-ADD     *ZERO         SCTPDT
     C                   END
     C*
     C* * * CALCULATE THE COMPENSATION RATE.
     C*
     C     ELKY          CHAIN     PEL01                              60
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXSTE
     C                   Z-ADD     CMACDT        XXEFDT
     C     RBKY          SETGT     PRB01
     C     RBKY2         READPE    PRB01                                  60
     C     *IN60         IFEQ      *OFF
     C*
     C     RBREPC        DIV       100           XXREPC            5 4
      *** BEG CHG *** 10/09/97 ****************************************
     C***        XXREPC    MULT CMAWW     SCRATE    H
     C                   Z-ADD     CMCRT         SCRATE
      *** END CHG *** 10/09/97 ****************************************
     C*
     C* DETERMINE WHICH PAYMENT CAP TO USE
     C*
     C*
     C     SCRATE        IFGT      RBPACP
     C                   Z-ADD     RBPACP        SCRATE
     C                   ENDIF                                                  SCRATE IFGT
     C*
     C     SCRATE        IFLT      RBMNRT
     C                   Z-ADD     RBMNRT        SCRATE
     C                   ENDIF                                                  SCRATE IFLT
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             SCRATE
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*  GET THE CLAIM DEMOGRAPHICS FILES & LOAD CLAIMANT NAME
     C*
     C     CDKEY         CHAIN     PCD01                              61
     C                   MOVE      *BLANKS       XXCDNM           30
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDFSN         XXFRST
     C                   MOVEL     CDLSN         XXLAST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCDNM
     C                   MOVEL     XXLAST        SCCLNM
     C*
     C*   GET CLAIM EXAMINER NAME
     C*
     C     UPKEY1        CHAIN     PUP02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     UPUSNM        SCCLEX
     C                   ELSE
     C                   MOVE      *BLANK        SCCLEX
     C                   END
     C*
     C                   MOVE      'N'           XXPROC            1
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C* PROCESS SCREEN 2 - CLAIM INFORMATION
     C*
     C     SCR02T        TAG
     C*
     C     *IN09         CABEQ     '1'           INQ2
     C*
     C*   DON'T RECHECK SCREEN VALUES WHILE GOING THROUGH EACH SCREEN OF INPUT
     C*
     C     XXPROC        IFNE      'Y'
     C*
     C*  VALIDATE DWC-4 SELECTION FOR NOTICE OF ACTION/CHANGE
     C*
     C*** BEGIN CHG *** 09/30/99 **************************************
     C********** SCDWC4    IFNE 'X'
     C     SCDWC4        IFNE      '1 '
     C     SCDWC4        ANDNE     ' 1'
     C*** END CHG ***** 09/30/99 **************************************
     C     SCDWC4        ANDNE     *BLANKS
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE DWC-12 SELECTION FOR NOTICE OF DENIAL
     C*
     C*** BEGIN CHG *** 09/30/99 **************************************
     C********** SCDW12    IFNE 'X'
     C     SCDW12        IFNE      '1 '
     C     SCDW12        ANDNE     ' 1'
     C*** END CHG ***** 09/30/99 **************************************
     C     SCDW12        ANDNE     *BLANKS
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE DWC-3 SELECTION FOR WAGE LOSS/TEMP. PARTIAL BENEFITS
     C*
     C*** BEGIN CHG *** 09/30/99 **************************************
     C********** SCDWC3    IFNE 'X'
     C     SCDWC3        IFNE      '1 '
     C     SCDWC3        ANDNE     ' 1'
     C*** END CHG ***** 09/30/99 **************************************
     C     SCDWC3        ANDNE     *BLANKS
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE DWC-13 SELECTION FOR PROGRESS/FINAL REPORT
     C*
     C*** BEGIN CHG *** 09/30/99 **************************************
     C********** SCDW13    IFNE 'X'
     C     SCDW13        IFNE      '1 '
     C     SCDW13        ANDNE     ' 1'
     C*** END CHG ***** 09/30/99 **************************************
     C     SCDW13        ANDNE     *BLANKS
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C*** Begin add *** 04/01/05 **************************************
     C*  VALIDATE DWC-19 SELECTION FOR EMPLOYEE EARNINGS REPORT
     C*
     C     SCDW19        IFNE      '1 '
     C     SCDW19        ANDNE     ' 1'
     C     SCDW19        ANDNE     *BLANK
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C*** End add ***** 04/01/05 **************************************
     C*
     C*   MUST PICK AT LEAST ONE FORM
     C*
     C     SCDWC4        IFEQ      *BLANKS
     C     SCDW12        ANDEQ     *BLANKS
     C     SCDWC3        ANDEQ     *BLANKS
     C     SCDW13        ANDEQ     *BLANKS
     C*** Begin add *** 04/01/05 **************************************
     C     SCDW19        ANDEQ     *BLANK
     C*** End add ***** 04/01/05 **************************************
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C*   SET FLAG FOR SCREEN PROCESS - LOOP THRU SCREENS IN ADD MODE
     C*
     C*** BEGIN DLT *** 09/30/99 **************************************
     C********** SCMODE    IFEQ UA,1
     C*** END DLT ***** 09/30/99 **************************************
     C                   MOVE      'Y'           XXPROC            1
     C*** BEGIN DLT *** 09/30/99 **************************************
     C******************** END
     C*** END DLT ***** 09/30/99 **************************************
     C*
     C                   END                                                    XXPROC LOOP
     C*
     C*  CHECK FOR ALL INPUT FIELD TO BE UPDATED IF PICKED
     C*
     C*** BEGIN CHG *** 09/30/99 **************************************
     C********** SCDWC4    IFEQ 'X'
     C     SCDWC4        IFEQ      '1 '
     C     XXDWC4        ANDEQ     *BLANK
     C     SCDWC4        OREQ      ' 1'
     C*** END CHG ***** 09/30/99 **************************************
     C     XXDWC4        ANDEQ     *BLANK
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*** BEGIN CHG *** 09/30/99 **************************************
     C********** SCDW12    IFEQ 'X'
     C     SCDW12        IFEQ      '1 '
     C     XXDW12        ANDEQ     *BLANK
     C     SCDW12        OREQ      ' 1'
     C*** END CHG ***** 09/30/99 **************************************
     C     XXDW12        ANDEQ     *BLANK
     C                   MOVEL     'SCR05'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*** BEGIN CHG *** 09/30/99 **************************************
     C********** SCDWC3    IFEQ 'X'
     C     SCDWC3        IFEQ      '1 '
     C     XXDWC3        ANDEQ     *BLANK
     C     SCDWC3        OREQ      ' 1'
     C*** END CHG ***** 09/30/99 **************************************
     C     XXDWC3        ANDEQ     *BLANK
     C                   MOVEL     'SCR07'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*** BEGIN CHG *** 09/30/99 **************************************
     C********** SCDW13    IFEQ 'X'
     C     SCDW13        IFEQ      '1 '
     C     XXDW13        ANDEQ     *BLANK
     C     SCDW13        OREQ      ' 1'
     C*** END CHG ***** 09/30/99 **************************************
     C     XXDW13        ANDEQ     *BLANK
     C                   MOVEL     'SCR08'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*** Begin add *** 04/01/05 **************************************
     C     SCDW19        IFEQ      '1 '
     C     XXDW19        ANDEQ     *BLANK
     C     SCDW19        OREQ      ' 1'
     C     XXDW19        ANDEQ     *BLANK
     C                   MOVEL     'SCR09'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  SCDW19 = 'X'
     C*** End add ***** 04/01/05 **************************************
     C*
     C*   ALL SCREENS PICKED HAVE BEEN VALIDATED - ADD UPDATE RECORD
     C*
     C     LTKEY1        CHAIN     PLT01                              60
     C                   Z-ADD     XXCO#         LTCO#
     C                   Z-ADD     SCFND         LTFND
     C                   Z-ADD     SCFYR         LTFYR
     C                   Z-ADD     SCCASE        LTCASE
     C                   Z-ADD     SCREQ#        LTREQ#
     C                   MOVE      *BLANKS       LTAUTO
     C*** BEGIN DLT *** 09/30/99 **************************************
     C*******************  MOVE SCDWC4    LTDWC4
     C*******************  MOVE SCDW12    LTDW12
     C*******************  MOVE SCDWC3    LTDWC3
     C*******************  MOVE SCDW13    LTDW13
     C*** END DLT ***** 09/30/99 **************************************
     C*** BEGIN ADD *** 09/30/99 **************************************
     C                   MOVE      *BLANKS       LTDWC4
     C                   MOVE      *BLANKS       LTDW12
     C                   MOVE      *BLANKS       LTDWC3
     C                   MOVE      *BLANKS       LTDW13
     C     SCDWC4        IFEQ      '1 '
     C     SCDWC4        OREQ      ' 1'
     C                   MOVE      'X'           LTDWC4
     C                   ENDIF
     C     SCDW12        IFEQ      '1 '
     C     SCDW12        OREQ      ' 1'
     C                   MOVE      'X'           LTDW12
     C                   ENDIF
     C     SCDWC3        IFEQ      '1 '
     C     SCDWC3        OREQ      ' 1'
     C                   MOVE      'X'           LTDWC3
     C                   ENDIF
     C     SCDW13        IFEQ      '1 '
     C     SCDW13        OREQ      ' 1'
     C                   MOVE      'X'           LTDW13
     C                   ENDIF
     C*** END ADD ***** 09/30/99 **************************************
     C*** Begin add *** 04/01/05 **************************************
     C     SCDW19        IFEQ      '1 '
     C     SCDW19        OREQ      ' 1'
     C                   MOVE      'X'           LTDW19
     C                   Z-ADD     SCEEFR        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTEEFR
     C                   Z-ADD     SCEETO        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTEETO
     C                   ELSE
     C                   Z-ADD     0             LTEEFR
     C                   Z-ADD     0             LTEETO
     C                   ENDIF
     C*** End add ***** 04/01/05 **************************************
     C*
     C*  LOAD DWC-4 FIELDS
     C*
     C*** BEGIN CHG *** 09/30/99 **************************************
     C********** SCDWC4    IFEQ 'X'
     C     SCDWC4        IFEQ      '1 '
     C     SCDWC4        OREQ      ' 1'
     C*** END CHG ***** 09/30/99 **************************************
     C*** Begin dlt *** 04/01/05 **************************************
     C***                  Z-ADDSCISTD    SRDTI
     C***                  CALL 'SRCVTDAT'DTCYMD
     C***                  MOVELSRDTO     LTISTD
     C***                  MOVE SCISTT    LTISTT
     C*** End dlt ***** 04/01/05 **************************************
     C                   Z-ADD     SCISUD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTISUD
     C                   MOVE      SCISUT        LTISUT
     C                   Z-ADD     SCIRID        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTIRID
     C                   MOVE      SCIRIT        LTIRIT
     C*** BEG CHG *** 03/08/05 ****************************
     C***********          Z-ADDSCPIRT    LTPIRT
     C                   Z-ADD     CMIPCT        LTPIRT
     C*** END CHG *** 03/08/05 ****************************
     C*** Begin add *** 04/01/05 **************************
     C                   MOVEL     SCPIRT        LTPIRA
     C*** End add ***** 04/01/05 **************************
     C                   Z-ADD     SCMMID        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTMMID
     C                   Z-ADD     SCRTWD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTRTWD
     C                   Z-ADD     SCDTHD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTDTHD
     C                   MOVE      SCREDA        LTREDA
     C                   Z-ADD     SCREEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTREEF
     C                   Z-ADD     SCPAWW        LTPAWW
     C                   Z-ADD     SCPCRT        LTPCRT
     C                   Z-ADD     SCAAWW        LTAAWW
     C                   Z-ADD     SCACRT        LTACRT
     C                   Z-ADD     SCRCUP        LTRCUP
     C                   Z-ADD     SCWK#         LTWK#
     C*** BEGIN ADD *** 07/07/00 **************************************
     C                   Z-ADD     SCWK#         LTWKEN
     C*** END ADD ***** 07/07/00 **************************************
     C                   Z-ADD     SCPID4        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTPID4
     C                   Z-ADD     SCPTD4        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTPTD4
     C                   Z-ADD     SCPTRT        LTPTRT
     C                   Z-ADD     SCPTEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTPTEF
     C                   Z-ADD     0             LTSSRC
     C                   Z-ADD     0             LTSSOF
     C                   Z-ADD     0             LTSSEF
     C                   MOVE      SCCSS#        LTCSS#
     C                   MOVE      SCCDA         LTCDA
     C                   MOVE      SCCNAM        LTCNAM
      *****  Begin Chg  ***  06/09/20  **********************
     C*****              MOVE      SCNSS#        LTNSS#
      /Free
       LTNSS# = UnMaskData('SSN':'LTNSS#':SCNSS#);
      /End-Free
      *****  End Chg  ***  06/09/20  **********************
     C                   Z-ADD     SCNDA         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTNDA
     C                   MOVEL     SCNNAM        LTNNAM
     C                   MOVEL     SCCC1         LTCC1
     C                   MOVEL     SCCC2         LTCC2
     C                   MOVEL     SCCC3         LTCC3
     C                   MOVEL     SCCC4         LTCC4
     C                   MOVEL     SCRMK1        LTRMK1
     C                   MOVEL     SCRMK2        LTRMK2
     C                   MOVEL     SCRMK3        LTRMK3
     C                   MOVEL     SCRMK4        LTRMK4
     C                   Z-ADD     SCRRTD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTIRCD
     C                   MOVE      SCRRTF        LTIRCT
     C                   MOVE      SCRTWF        LTRTWF
     C                   Z-ADD     SCFISD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTAJDT
     C*** Begin add *** 04/01/05 **************************************
     C                   MOVE      SCBAJ1        LTBAJ1
     C                   MOVE      SCBAJ2        LTBAJ2
     C*** End add ***** 04/01/05 **************************************
     C                   MOVE      SCATY1        LTATY1
     C                   MOVE      SCATY2        LTATY2
     C                   Z-ADD     SCAAM1        LTUNPT
     C                   Z-ADD     SCAAM2        LTOVPT
     C                   Z-ADD     SCAEF1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTAEF1
     C                   Z-ADD     SCAEF2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTAEF2
     C                   Z-ADD     SCACN1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTACN1
     C                   Z-ADD     SCACN2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTACN2
     C                   MOVE      SCSAGT        LTSAGT
     C                   MOVE      SCNAGT        LTNAGT
     C                   ELSE
     C                   MOVE      *ZEROS        LTISTD
     C                   MOVE      *BLANK        LTISTT
     C                   MOVE      *ZEROS        LTISUD
     C                   MOVE      *BLANK        LTISUT
     C                   MOVE      *ZEROS        LTIRID
     C                   MOVE      *BLANK        LTIRIT
     C                   MOVE      *ZEROS        LTIRCD
     C                   MOVE      *BLANK        LTIRCT
     C                   MOVE      *ZEROS        LTMMID
     C*** BEG CHG *** 03/08/05 ****************************
     C***********          MOVE *ZEROS    LTPIRT
     C                   MOVE      CMIPCT        LTPIRT
     C*** END CHG *** 03/08/05 ****************************
     C                   MOVE      *ZEROS        LTRTWD
     C                   MOVE      *ZEROS        LTDTHD
     C                   MOVE      *BLANK        LTREDA
     C                   MOVE      *ZEROS        LTREEF
     C                   MOVE      *ZEROS        LTPAWW
     C                   MOVE      *ZEROS        LTPCRT
     C                   MOVE      *ZEROS        LTAAWW
     C                   MOVE      *ZEROS        LTACRT
     C                   MOVE      *ZEROS        LTUNPT
     C                   MOVE      *ZEROS        LTAJDT
     C                   MOVE      *ZEROS        LTOVPT
     C                   MOVE      *ZEROS        LTRCUP
     C                   MOVE      *ZEROS        LTWK#
     C                   MOVE      *ZEROS        LTPID4
     C                   MOVE      *ZEROS        LTPTD4
     C                   MOVE      *ZEROS        LTPTRT
     C                   MOVE      *ZEROS        LTPTEF
     C                   MOVE      *ZEROS        LTSSRC
     C                   MOVE      *ZEROS        LTSSOF
     C                   MOVE      *ZEROS        LTSSEF
     C                   MOVE      *BLANK        LTCSS#
     C                   MOVE      *BLANK        LTCDA
     C                   MOVE      *BLANK        LTCNAM
     C                   MOVE      *BLANKS       LTNSS#
     C                   MOVE      *ZEROS        LTNDA
     C                   MOVE      *BLANKS       LTNNAM
     C                   MOVE      *BLANKS       LTCC1
     C                   MOVE      *BLANKS       LTCC2
     C                   MOVE      *BLANKS       LTCC3
     C                   MOVE      *BLANKS       LTCC4
     C                   MOVE      *BLANKS       LTRMK1
     C                   MOVE      *BLANKS       LTRMK2
     C                   MOVE      *BLANKS       LTRMK3
     C                   MOVE      *BLANKS       LTRMK4
     C                   MOVE      *BLANKS       LTRTWF
     C                   MOVE      *BLANKS       LTATY1
     C                   MOVE      *BLANKS       LTATY2
     C                   MOVE      *ZEROS        LTAEF1
     C                   MOVE      *ZEROS        LTAEF2
     C                   MOVE      *ZEROS        LTACN1
     C                   MOVE      *ZEROS        LTACN2
     C                   MOVE      *BLANKS       LTSAGT
     C                   MOVE      *BLANKS       LTNAGT
     C*** BEGIN ADD *** 07/07/00 **************************************
     C                   MOVE      *ZEROS        LTWKEN
     C*** END ADD ***** 07/07/00 **************************************
     C                   END
     C*
     C*  MOVE INFO FOR NOTICE OF DENIAL DWC-12
     C*
     C*** BEGIN CHG *** 09/30/99 **************************************
     C********** SCDW12    IFEQ 'X'
     C     SCDW12        IFEQ      '1 '
     C     SCDW12        OREQ      ' 1'
     C*** END CHG ***** 09/30/99 **************************************
     C*
     C                   Z-ADD     SCRSCD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTRSCD
     C                   MOVE      SCDEN1        LTDEN1
     C                   MOVEL     SCDEN2        LTDEN2
     C                   MOVEL     SCDEN3        LTDEN3
     C                   MOVEL     SCDEN4        LTDEN4
     C                   MOVEL     SCDEN5        LTDEN5
     C                   MOVEL     SCDCC1        LTDCC1
     C                   MOVEL     SCDCC2        LTDCC2
     C                   MOVEL     SCDCC3        LTDCC3
     C                   MOVEL     SCDCC4        LTDCC4
     C                   MOVEL     SCWHY0        LTWHY0
     C                   MOVE      SCWHY1        LTWHY1
     C                   MOVE      SCWHY2        LTWHY2
     C                   MOVE      SCWHY3        LTWHY3
     C                   MOVE      SCWHY4        LTWHY4
     C                   MOVE      SCWHY5        LTWHY5
     C                   MOVEL     SCWHY6        LTWHY6
     C                   MOVEL     SCWHY7        LTWHY7
     C                   MOVEL     SCWHY8        LTWHY8
     C                   MOVEL     SCWHY9        LTWHY9
     C                   ELSE
     C*
     C                   MOVE      *ZEROS        LTRSCD
     C                   MOVE      *BLANKS       LTDEN1
     C                   MOVE      *BLANKS       LTDEN2
     C                   MOVE      *BLANKS       LTDEN3
     C                   MOVE      *BLANKS       LTDEN4
     C                   MOVE      *BLANKS       LTDEN5
     C                   MOVE      *BLANKS       LTDCC1
     C                   MOVE      *BLANKS       LTDCC2
     C                   MOVE      *BLANKS       LTDCC3
     C                   MOVE      *BLANKS       LTDCC4
     C                   MOVE      *BLANKS       LTWHY0
     C                   MOVE      *BLANKS       LTWHY1
     C                   MOVE      *BLANKS       LTWHY2
     C                   MOVE      *BLANKS       LTWHY3
     C                   MOVE      *BLANKS       LTWHY4
     C                   MOVE      *BLANKS       LTWHY5
     C                   MOVE      *BLANKS       LTWHY6
     C                   MOVE      *BLANKS       LTWHY7
     C                   MOVE      *BLANKS       LTWHY8
     C                   MOVE      *BLANKS       LTWHY9
     C                   END
     C*
     C*   LOAD INFO FOR WAGE LOSS DWC-3
     C*
     C*** BEGIN CHG *** 09/30/99 **************************************
     C********** SCDWC3    IFEQ 'X'
     C     SCDWC3        IFEQ      '1 '
     C     SCDWC3        OREQ      ' 1'
     C*** END CHG ***** 09/30/99 **************************************
     C                   Z-ADD     SCWLF1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTWLF1
     C                   Z-ADD     SCWLT1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTWLT1
     C                   Z-ADD     SCWLF2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTWLF2
     C                   Z-ADD     SCWLT2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTWLT2
     C                   Z-ADD     SCMMI3        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTMMI3
     C                   Z-ADD     SCWLR3        LTWLR3
     C                   MOVE      SCWL          LTWL
     C                   MOVE      SCTP          LTTP
     C                   MOVE      SCCV          LTCV
     C*
     C                   ELSE
     C                   Z-ADD     *ZERO         LTWLF1
     C                   Z-ADD     *ZERO         LTWLT1
     C                   Z-ADD     *ZERO         LTWLF2
     C                   Z-ADD     *ZERO         LTWLT2
     C                   Z-ADD     *ZERO         LTMMI3
     C                   Z-ADD     *ZERO         LTWLR3
     C                   MOVE      *BLANK        LTWL
     C                   MOVE      *BLANK        LTTP
     C                   MOVE      *BLANK        LTCV
     C                   END
     C*
     C*   LOAD INFORMATION FOR PROGRESS/FINAL REPORT DWC-13
     C*
     C*** BEGIN CHG *** 09/30/99 **************************************
     C********** SCDW13    IFEQ 'X'
     C     SCDW13        IFEQ      '1 '
     C     SCDW13        OREQ      ' 1'
     C*** END CHG ***** 09/30/99 **************************************
     C     SCTYR1        IFEQ      'X'
     C                   MOVE      'I'           LTRPTP
     C                   END
     C     SCTYR2        IFEQ      'X'
     C                   MOVE      'F'           LTRPTP
     C                   END
     C     SCTYR3        IFEQ      'X'
     C                   MOVE      'A'           LTRPTP
     C                   END
     C                   MOVE      *BLANKS       LTMDBO
     C*** BEGIN CHG *** 10/19/98 **************************************
     C**********           MOVE SCFSAL    LTFSAL
     C**********           Z-ADDSCFSDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     LTFSDT
     C                   Z-ADD     SCPAOF        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTFSDT
     C*** END CHG *** 10/19/98 ****************************************
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       LTRPTP
     C                   MOVE      *BLANKS       LTMDBO
     C                   MOVE      *BLANK        LTFSAL
     C                   Z-ADD     *ZEROS        LTFSDT
     C                   END
     C*
     C*   LOAD MAINTENANCE FIELDS
     C*
     C                   MOVE      *BLANK        LTSTAT
     C                   MOVE      Q1USER        LTUSER
     C                   MOVE      Q1PGM         LTPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTDT
     C                   Z-ADD     DSTIME        LTTIME
     C                   MOVE      CMCLEX        LTCLEX
     C*
     C*   UPDATE/ADD RECORD FOR WALTP
     C*
     C     *IN60         IFEQ      '1'
     C                   WRITE     PLT01
     C                   ELSE
     C                   UPDATE    PLT01
     C                   END
     C*
     C*   UPDATE/ADD RECORD FOR WAL1P DWC-13 FIELDS IF SELECTED
     C*
     C*** BEGIN CHG *** 09/30/99 **************************************
     C********** SCDW13    IFEQ 'X'
     C     SCDW13        IFEQ      '1 '
     C     SCDW13        OREQ      ' 1'
     C*** END CHG ***** 09/30/99 **************************************
     C*
     C     L1KEY         CHAIN     PL101                              60
     C                   Z-ADD     XXCO#         L1CO#
     C                   Z-ADD     SCFND         L1FND
     C                   Z-ADD     SCFYR         L1FYR
     C                   Z-ADD     SCCASE        L1CASE
     C                   Z-ADD     SCTPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         L1TPDT
     C                   Z-ADD     0             L1PTDT
     C                   Z-ADD     0             L1WLDT
     C                   Z-ADD     0             L1PIDT
     C                   Z-ADD     0             L1WODT
     C                   MOVE      *BLANK        L1STAT
     C                   MOVE      Q1USER        L1USER
     C                   MOVE      Q1PGM         L1PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         L1DT
     C                   Z-ADD     DSTIME        LTTIME
     C*
     C     *IN60         IFEQ      '1'
     C                   WRITE     PL101
     C                   ELSE
     C                   UPDATE    PL101
     C                   END
     C*
     C                   END
     C*
     C     INQ2          TAG
     C                   MOVE      *ZEROS        SCFYR             3 0
     C                   MOVE      *ZEROS        SCCASE
     C                   MOVE      *ZEROS        SCREQ#
     C                   MOVE      'N'           XXPROC
     C*** BEGIN ADD *** 09/30/99 **************************************
     C                   MOVE      *BLANK        XXDWC4
     C                   MOVE      *BLANK        XXDW12
     C                   MOVE      *BLANK        XXDWC3
     C                   MOVE      *BLANK        XXDW13
     C*** END ADD ***** 09/30/99 **************************************
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C*****************************************************************
     C* PROCESS SCREEN 3 - DWC-4 INFO                                 *
     C*****************************************************************
     C*
     C     SCR03T        TAG
     C*
     C     *IN09         CABEQ     '1'           INQ3
     C*
     C*** Begin dlt *** 04/01/05 **************************************
     C*  VALIDATE INDEMNITY DATES & TYPES IF ENTERED
     C*
     C***        SCISTD    IFNE *ZERO                      STARTED DATE
     C***                  Z-ADDSCISTD    SRDTI
     C***                  CALL 'SRCVTDAT'DATCHK
     C***                  MOVELSRDTO     SCISTD
     C***        SRERR     CABEQ'1'       STRT           20
     C***                  END
     C*
     C***        SCISTT    IFNE *BLANK                     STARTED TYPE
     C***                  MOVE *BLANKS   XXKEY
     C***                  MOVELSCISTT    XXKEY
     C***        TBKY      CHAINPID01                21
     C***        *IN21     CABEQ'1'       STRT
     C***                  END
     C*** End dlt ***** 04/01/05 **************************************
     C*
     C     SCISUD        IFNE      *ZERO                                        SUSPENDED DATE
     C                   Z-ADD     SCISUD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCISUD
     C     SRERR         CABEQ     '1'           STRT                     22
     C                   END
     C*
     C     SCISUT        IFNE      *BLANK                                       SUSPENDED TYPE
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCISUT        XXKEY
     C     TBKY          CHAIN     PIS01                              23
     C     *IN23         CABEQ     '1'           STRT
     C                   END
     C*
     C     SCIRID        IFNE      *ZERO                                        REINSTATED DATE
     C                   Z-ADD     SCIRID        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCIRID
     C     SRERR         CABEQ     '1'           STRT                     24
     C                   END
     C*
     C     SCIRIT        IFNE      *BLANK                                       REINSTATED TYPE
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCIRIT        XXKEY
     C     TBKY          CHAIN     PID01                              25
     C     *IN25         CABEQ     '1'           STRT
     C                   END
     C*
     C*  MMI DATE MUST BE VALID & >= DATE OF ACCIDENT
     C*
     C     SCMMID        IFNE      *ZERO
     C                   Z-ADD     SCMMID        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCMMID
     C     SRERR         CABEQ     '1'           STRT                     28     INVALID DT
     C*
     C                   Z-ADD     SCMMID        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C***  BEGIN CHG  ***  06/04/98  *********************************
     C**********           MOVELSRDTO     XXMMID  60
     C                   MOVEL     SRDTO         XXMMID
     C***  END   CHG  ***  06/04/98  *********************************
     C     XXMMID        CABLT     CMACDT        STRT                   37       DT < D/A
     C                   END
     C*
     C*  PI RATING CANNOT BE LESS THAN ZERO
     C*
     C*** BEG DEL *** 03/08/05 ****************************
     C********** SCPIRT    CABLT*ZERO     STRT         38
     C*** END DEL *** 03/08/05 ****************************
     C*
     C*  VALIDATE RELEASED TO RETURN TO WORK DATE IF ENTERED
     C*
     C     SCRRTD        IFNE      *ZERO
     C                   Z-ADD     SCRRTD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRRTD
     C     SRERR         CABEQ     '1'           STRT                     26
     C                   END
     C*
     C*  RESTRICTION FLAG MUST BE Y OR N IF SCRRTD IS ENTERED.
     C*
     C     SCRRTD        IFNE      *ZERO
     C     SCRRTF        ANDNE     'Y'
     C     SCRRTF        ANDNE     'N'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE RETURN TO WORK DATE IF ENTERED
     C*
     C     SCRTWD        IFNE      *ZERO
     C                   Z-ADD     SCRTWD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRTWD
     C     SRERR         CABEQ     '1'           STRT                     29
     C                   END
     C*
     C*  RESTRICTION FLAG MUST BE Y OR N IF SCRTWD IS ENTERED.
     C*
     C     SCRTWD        IFNE      *ZERO
     C     SCRTWF        ANDNE     'Y'
     C     SCRTWF        ANDNE     'N'
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE FINAL INDEMNITY SETTLEMENT DATE IF ENTERED.
     C*
     C     SCFISD        IFNE      *ZERO
     C                   Z-ADD     SCFISD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFISD
     C     SRERR         CABEQ     '1'           STRT                     39
     C                   END
     C*
     C*  VALIDATE DATE OF DEATH IF ENTERED
     C*
     C     SCDTHD        IFNE      *ZERO
     C                   Z-ADD     SCDTHD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTHD
     C     SRERR         CABEQ     '1'           STRT                     30
     C                   END
     C*
     C*  VALIDATE RETROACTIVE TO D/A FLAG AND EFFECTIVE DATE
     C*
     C     SCREDA        IFNE      *BLANK                                       ADD 12/27/93
     C     SCREDA        IFEQ      'N'
     C     SCREEF        CABEQ     *ZEROS        STRT                     32     MUST ENTER DT
     C                   ELSE
     C     SCREDA        CABNE     'Y'           STRT                 3131       INVALID FLAG
     C                   END
     C                   END                                                    ADD 12/27/93
     C*
     C     SCREEF        IFNE      *ZEROS
     C                   Z-ADD     SCREEF        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCREEF
     C     SRERR         CABEQ     '1'           STRT                     32     INVALID DATE
     C                   END
     C*
     C*
     C*  VALIDATE PERM IMPAIR. PAID DATE IF ENTERED
     C*
     C     SCPID4        IFNE      *ZERO
     C                   Z-ADD     SCPID4        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPID4
     C     SRERR         CABEQ     '1'           STRT                     34
     C                   END
     C*
     C*  VALIDATE PERM TOTAL A/A DATE IF ENTERED
     C*
     C     SCPTD4        IFNE      *ZERO
     C                   Z-ADD     SCPTD4        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPTD4
     C     SRERR         CABEQ     '1'           STRT                     35
     C                   END
     C*
     C*  VALIDATE PT SUPPLEMENTAL EFFECTIVE DATE IF ENTERED
     C*
     C     SCPTEF        IFNE      *ZERO
     C                   Z-ADD     SCPTEF        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPTEF
     C     SRERR         CABEQ     '1'           STRT                     36
     C                   END
     C*
     C     INQ3          TAG
     C*
     C*  GO TO SCREEN 4 FOR MORE DWC-4 INFO
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   GOTO      STRT
     C*
     C*****************************************************************
     C* PROCESS SCREEN 4 - MORE OF DWC-4, NOTICE OF ACTION/CHANGE     *
     C*****************************************************************
     C*
     C     SCR04T        TAG
     C*
     C     *IN09         CABEQ     '1'           INQ4
     C*** Begin add *** 04/01/05 **************************************
     C*
     C*  VALIDATE Benefit ADJUSTMENT TYPE 1 IF ENTERED
     C*
     C     SCBAJ1        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCBAJ1        XXKEY
     C     TBKY          CHAIN     PBA01                              73
     C     *IN20         CABEQ     '1'           STRT
     C                   END
     C*
     C*  VALIDATE Benefit ADJUSTMENT TYPE 2 IF ENTERED
     C*
     C     SCBAJ2        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCBAJ2        XXKEY
     C     TBKY          CHAIN     PBA01                              74
     C     *IN30         CABEQ     '1'           STRT
     C                   END
     C*** End add ***** 04/01/05 **************************************
     C*
     C*  VALIDATE ADJUSTMENT TYPE 1 IF ENTERED
     C*
     C     SCATY1        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCATY1        XXKEY
     C*** Begin dlt *** 06/29/06 **************************************
     C*********  TBKY      CHAINPBA01                20
     C*** End dlt ***** 06/29/06 **************************************
     C*** Begin add *** 06/29/06 **************************************
     C                   MOVEL     'DSTP'        XXTBID
     C     GTKY          CHAIN     PGT01                              20
     C*** End add ***** 06/29/06 **************************************
     C     *IN20         CABEQ     '1'           STRT
     C                   END
     C*
     C*  VALIDATE ADJUSTMENT TYPE 2 IF ENTERED
     C*
     C     SCATY2        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCATY2        XXKEY
     C*** Begin dlt *** 06/29/06 **************************************
     C*********  TBKY      CHAINPBA01                30
     C*** End dlt ***** 06/29/06 **************************************
     C*** Begin add *** 06/29/06 **************************************
     C                   MOVEL     'DSTP'        XXTBID
     C     GTKY          CHAIN     PGT01                              30
     C*** End add ***** 06/29/06 **************************************
     C     *IN30         CABEQ     '1'           STRT
     C                   END
     C*
     C*  VALIDATE ADJUSTMENT EFFECTIVE DATE 1 IF ENTERED
     C*
     C     SCAEF1        IFNE      *ZERO
     C                   Z-ADD     SCAEF1        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCAEF1
     C     SRERR         CABEQ     '1'           STRT                     31
     C                   END
     C*
     C*  VALIDATE ADJUSTMENT EFFECTIVE DATE 2 IF ENTERED
     C*
     C     SCAEF2        IFNE      *ZERO
     C                   Z-ADD     SCAEF2        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCAEF2
     C     SRERR         CABEQ     '1'           STRT                     32
     C                   END
     C*
     C*  VALIDATE ADJUSTMENT END DATE 1 IF ENTERED
     C*
     C     SCACN1        IFNE      *ZERO
     C                   Z-ADD     SCACN1        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCACN1
     C     SRERR         CABEQ     '1'           STRT                     33
     C                   END
     C*
     C*  VALIDATE ADJUSTMENT END DATE 2 IF ENTERED
     C*
     C     SCACN2        IFNE      *ZERO
     C                   Z-ADD     SCACN2        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCACN2
     C     SRERR         CABEQ     '1'           STRT                     34
     C                   END
     C*
     C*  VALIDATE CORRECTION OF SOCIAL SECURITY FLAG & FIELD
     C*
     C     SCCSS#        IFNE      'X'                                          FLAG MUST BE 'X'
     C     SCCSS#        ANDNE     *BLANK                                       ...OR BLANK
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C     SCCSS#        IFEQ      *BLANK                                       FLAG MUST BE MARKED
     C     SCNSS#        ANDNE     *BLANK                                       ...IF SS# IS ENTERED
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C     SCCSS#        IFNE      *BLANK                                       IF FLAG IS MARKED
     C     SCNSS#        ANDEQ     *BLANK                                       ...MUST ENTER SS#
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE CORRECTION OF DATE OF ACCIDENT FLAG & FIELD
     C*
     C     SCCDA         IFNE      'X'                                          FLAG MUST BE 'X'
     C     SCCDA         ANDNE     *BLANK                                       ...OR BLANK
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C     SCCDA         IFEQ      *BLANK                                       FLAG MUST BE MARKED
     C     SCNDA         ANDNE     *ZERO                                        ...IF D/A IS ENTERED
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C     SCCDA         IFNE      *BLANK                                       IF FLAG IS MARKED
     C     SCNDA         ANDEQ     *ZERO                                        ...MUST ENTER D/A
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C     SCNDA         IFNE      *ZERO                                        DATE MUST BE VALID
     C                   Z-ADD     SCNDA         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCNDA
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  VALIDATE CORRECTION OF NAME FLAG & FIELD
     C*
     C     SCCNAM        IFNE      'X'                                          FLAG MUST BE 'X'
     C     SCCNAM        ANDNE     *BLANK                                       ...OR BLANK
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C     SCCNAM        IFEQ      *BLANK                                       FLAG MUST BE MARKED
     C     SCNNAM        ANDNE     *BLANK                                       ...IF NAME IS ENTERE
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C*
     C     SCCNAM        IFNE      *BLANK                                       IF FLAG IS MARKED
     C     SCNNAM        ANDEQ     *BLANK                                       ...MUST ENTER NAME
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE CORRECTION OF CARRIER/SERVICING AGENT
     C*
     C     SCSAGT        IFNE      'X'                                          FLAG MUST BE 'X'
     C     SCSAGT        ANDNE     *BLANK                                       ...OR BLANK
     C                   MOVE      '1'           *IN35
     C                   GOTO      STRT
     C                   END
     C*
     C     SCSAGT        IFEQ      *BLANK                                       FLAG MUST BE MARKED
     C     SCNAGT        ANDNE     *BLANK                                       ...IF NAME IS ENTERE
     C                   MOVE      '1'           *IN36
     C                   GOTO      STRT
     C                   END
     C*
     C     SCSAGT        IFNE      *BLANK                                       IF FLAG IS MARKED
     C     SCNAGT        ANDEQ     *BLANK                                       ...MUST ENTER NAME
     C                   MOVE      '1'           *IN37
     C                   GOTO      STRT
     C                   END
     C*
     C     INQ4          TAG
     C*
     C                   MOVE      'Y'           XXDWC4
     C*** BEGIN DLT *** 09/30/99 **************************************
     C******************** MOVE 'X'       SCDWC4
     C*** END DLT ***** 09/30/99 **************************************
     C*
     C*   IF LOOPING THRU SCREENS GOTO SCRO2T TAG
     C*
     C     XXPROC        IFEQ      'Y'
     C                   GOTO      SCR02T
     C                   END
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C*****************************************************************
     C* PROCESS SCREEN 5 - DWC-12 NOTICE OF DENIAL                    *
     C*****************************************************************
     C*
     C     SCR05T        TAG
     C*
     C     *IN09         CABEQ     '1'           INQ5
     C*
     C*  IF ENTERED, RESCINDED DATE MUST BE VALID & >= DATE OF ACCIDENT
     C*
     C     SCRSCD        IFNE      *ZERO
     C                   Z-ADD     SCRSCD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRSCD
     C     SRERR         CABEQ     '1'           STRT                     21
     C*
     C                   Z-ADD     SCRSCD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C***  BEGIN CHG  ***  06/04/98  *********************************
     C**********           MOVELSRDTO     XXRSCD  60
     C                   MOVEL     SRDTO         XXRSCD
     C***  END   CHG  ***  06/04/98  *********************************
     C     XXRSCD        CABLT     CMACDT        STRT                   22       DT < D/A
     C                   END
     C*
     C     INQ5          TAG
     C*
     C*   GO TO SCREEN 6 FOR MORE DWC-12 INFORMATION
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR06'       Q2RECF
     C                   GOTO      STRT
     C*
     C*****************************************************************
     C* PROCESS SCREEN 6 - MORE OF DWC-12, NOTICE OF DENIAL           *
     C*****************************************************************
     C*
     C     SCR06T        TAG
     C*
     C     *IN09         CABEQ     '1'           INQ6
     C*
     C*  NO EDITING
     C*
     C     INQ6          TAG
     C*
     C                   MOVE      'Y'           XXDW12
     C*** BEGIN DLT *** 09/30/99 **************************************
     C******************** MOVE 'X'       SCDW12
     C*** END DLT ***** 09/30/99 **************************************
     C*
     C*   IF LOOPING THRU SCREENS GOTO SCRO2T TAG
     C*
     C     XXPROC        IFEQ      'Y'
     C                   GOTO      SCR02T
     C                   END
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C*****************************************************************
     C* PROCESS SCREEN 7 - DWC-3 LOSS OF WAGES/BENEFITS               *
     C*****************************************************************
     C*
     C     SCR07T        TAG
     C*
     C     *IN09         CABEQ     '1'           INQ7
     C*
     C     INQ7          TAG
     C*
     C                   MOVE      *ZEROS        XXWLF1
     C                   MOVE      *ZEROS        XXWLT1
     C                   MOVE      *ZEROS        XXWLF2
     C                   MOVE      *ZEROS        XXWLT2
     C*
     C*  WEEK 1: FROM DATE MUST BE VALID & >= DATE OF ACCIDENT IF ENT  ERED
     C*
     C     SCWLF1        IFNE      *ZERO
     C                   Z-ADD     SCWLF1        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCWLF1
     C     SRERR         CABEQ     '1'           STRT                     20
     C*
     C                   Z-ADD     SCWLF1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C***  BEGIN CHG  ***  06/04/98  *********************************
     C**********           MOVELSRDTO     XXWLF1  60
     C                   MOVEL     SRDTO         XXWLF1
     C***  END   CHG  ***  06/04/98  *********************************
     C     XXWLF1        CABLT     CMACDT        STRT                   28       DT < D/A
     C*
     C*   MUST ENTER FROM DATE IF TO DATE IS ENTERED.
     C*
     C                   ELSE
     C     SCWLT1        CABNE     *ZERO         STRT                 2020
     C                   END
     C*
     C*   WEEK 1: TO DATE MUST BE VALID
     C*
     C     SCWLT1        IFNE      *ZERO
     C                   Z-ADD     SCWLT1        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCWLT1
     C     SRERR         CABEQ     '1'           STRT                     21
     C*
     C                   Z-ADD     SCWLT1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C***  BEGIN CHG  ***  06/04/98  *********************************
     C**********           MOVELSRDTO     XXWLT1  60
     C                   MOVEL     SRDTO         XXWLT1
     C***  END   CHG  ***  06/04/98  *********************************
     C                   END
     C*
     C*  WEEK 1: DATES MUST BE IN ASCENDING ORDER
     C*
     C     XXWLF1        IFGT      XXWLT1
     C                   MOVE      '1'           *IN22
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*   WEEK 2: FROM DATE MUST BE VALID & >= DATE OF ACCIDENT IF ENTERED
     C*
     C     SCWLF2        IFNE      *ZERO
     C                   Z-ADD     SCWLF2        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCWLF2
     C     SRERR         CABEQ     '1'           STRT                     24
     C*
     C                   Z-ADD     SCWLF2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C***  BEGIN CHG  ***  06/04/98  *********************************
     C**********           MOVELSRDTO     XXWLF2  60
     C                   MOVEL     SRDTO         XXWLF2
     C***  BEGIN CHG  ***  06/04/98  *********************************
     C     XXWLF2        CABLT     CMACDT        STRT                   35       DT < D/A
     C*
     C*   MUST ENTER FROM DATE IF TO DATE IS ENTERED.
     C*
     C                   ELSE
     C     SCWLT2        CABNE     *ZERO         STRT                 2424
     C                   END
     C*
     C*   WEEK 2: TO DATE MUST BE VALID
     C*
     C     SCWLT2        IFNE      *ZERO
     C                   Z-ADD     SCWLT2        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCWLT2
     C     SRERR         CABEQ     '1'           STRT                     25
     C*
     C                   Z-ADD     SCWLT2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C***  BEGIN CHG  ***  06/04/98  *********************************
     C**********           MOVELSRDTO     XXWLT2  60
     C                   MOVEL     SRDTO         XXWLT2
     C***  END   CHG  ***  06/04/98  *********************************
     C                   END
     C*
     C*  WEEK 2: DATES MUST BE IN ASCENDING ORDER
     C*
     C     XXWLF2        IFGT      XXWLT2
     C                   MOVE      '1'           *IN26
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*  WEEK 1 TO DATE & WEEK 2 FROM DATE MUST BE IN ASCENDING ORDER
     C*
     C     XXWLT1        IFNE      *ZERO
     C     XXWLF2        ANDNE     *ZERO
     C     XXWLT1        ANDGT     XXWLF2
     C                   MOVE      '1'           *IN23
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C*  WAGE LOSS FLAG MUST BE 'X' OR BLANK
     C*
     C     SCWL          IFNE      'X'
     C     SCWL          ANDNE     *BLANK
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C*  MMI DATE MUST BE VALID & >= DATE OF ACCIDENT
     C*
     C     SCMMI3        IFNE      *ZERO
     C                   Z-ADD     SCMMI3        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCMMI3
     C     SRERR         CABEQ     '1'           STRT                     29
     C*
     C                   Z-ADD     SCMMI3        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C***  BEGIN CHG  ***  06/04/98  *********************************
     C**********           MOVELSRDTO     XXMMI3  60
     C                   MOVEL     SRDTO         XXMMI3
     C***  END   CHG  ***  06/04/98  *********************************
     C     XXMMI3        CABLT     CMACDT        STRT                   30
     C                   END
     C*
     C*  WL RATING PERCENTAGE CANNOT BE LESS THAN ZERO
     C*
     C     SCWLR3        CABLT     *ZERO         STRT                   31
     C*
     C*  TEMPORARY PARTIAL FLAG MUST BE 'X' OR BLANK
     C*
     C     SCTP          IFNE      'X'
     C     SCTP          ANDNE     *BLANK
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C*
     C*  CONTROVERTED FLAG MUST BE 'X' OR BLANK
     C*
     C     SCCV          IFNE      'X'
     C     SCCV          ANDNE     *BLANK
     C                   MOVE      '1'           *IN34
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      'Y'           XXDWC3
     C*** BEGIN DLT *** 09/30/99 **************************************
     C******************** MOVE 'X'       SCDWC3
     C*** END DLT ***** 09/30/99 **************************************
     C*
     C*   IF LOOPING THRU SCREENS GOTO SCRO2T TAG
     C*
     C     XXPROC        IFEQ      'Y'
     C                   GOTO      SCR02T
     C                   END
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C*****************************************************************
     C* PROCESS SCREEN 8 - PROGRESS/FINAL REPORT                      *
     C*****************************************************************
     C*
     C     SCR08T        TAG
     C*
     C     *IN09         CABEQ     '1'           INQ8
     C*
     C*  TYPE OF REPORT FIELDS MUST BE "X" OR BLANK
     C*
     C     SCTYR1        IFNE      'X'
     C     SCTYR1        ANDNE     *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C     SCTYR2        IFNE      'X'
     C     SCTYR2        ANDNE     *BLANK
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C     SCTYR3        IFNE      'X'
     C     SCTYR3        ANDNE     *BLANK
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*   MUST PICK A TYPE OF REPORT
     C*
     C     SCTYR1        IFEQ      *BLANK
     C     SCTYR2        ANDEQ     *BLANK
     C     SCTYR3        ANDEQ     *BLANK
     C                   MOVEA     '1111'        *IN(24)
     C                   GOTO      STRT
     C                   END
     C*
     C*   MUST PICK ONLY ONE TYPE OF REPORT
     C*
     C                   Z-ADD     1             X                 5 0
     C     'X'           LOOKUP    TYR(X)                                 50
     C     *IN50         IFEQ      '1'
     C     X             ANDLT     4
     C     X             ADD       1             Y                 5 0
     C     'X'           LOOKUP    TYR(Y)                                 50
     C     *IN50         IFEQ      '1'
     C     X             ADD       27            XX                5 0
     C     Y             ADD       27            YY                5 0
     C                   MOVE      '1'           *IN(XX)
     C                   MOVE      '1'           *IN(YY)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*** BEGIN CHG *** 10/19/98 **************************************
     C*
     C*   FULL SALARY FLAG MUST BE 'Y' OR 'N'                          E BLANK
     C*
     C********** SCFSAL    IFNE 'Y'
     C********** SCFSAL    ANDNE'N'
     C**********           MOVE '1'       *IN32
     C**********           GOTO STRT
     C**********           END
     C*
     C*   VALIDATE FULL SALARY DATE IF ENTERED
     C*
     C********** SCFSDT    IFNE *ZERO
     C**********           Z-ADDSCFSDT    SRDTI
     C**********           CALL 'SRCVTDAT'DATCHK
     C**********           MOVELSRDTO     SCFSDT
     C********** SRERR     CABEQ'1'       STRT           33
     C**********           END
     C*
     C*   VALIDATE PAYMENTS AS OF DATE
     C*
     C     SCPAOF        IFNE      *ZERO
     C                   Z-ADD     SCPAOF        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPAOF
     C     SRERR         CABEQ     '1'           STRT                     33
     C                   END
     C*** END CHG *** 10/19/98 ****************************************
     C*
     C*
     C*
     C     SCTPDT        IFNE      *ZERO
     C                   Z-ADD     SCTPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTPDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN40
     C                   GOTO      STRT
     C                   END
     C*
     C                   Z-ADD     SCTPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C***  BEGIN CHG  ***  06/04/98  *********************************
     C**********           MOVELSRDTO     XXTPDT  60
     C                   MOVEL     SRDTO         XXTPDT
     C***  END   CHG  ***  06/04/98  *********************************
     C     XXTPDT        CABLT     CMACDT        STRT                   41
     C                   END
     C*
     C*
     C*
     C     INQ8          TAG
     C*
     C                   MOVE      'Y'           XXDW13
     C*** BEGIN DLT *** 09/30/99 **************************************
     C******************** MOVE 'X'       SCDW13
     C*** END DLT ***** 09/30/99 **************************************
     C*
     C*   IF LOOPING THRU SCREENS GOTO SCRO2T TAG
     C*
     C     XXPROC        IFEQ      'Y'
     C                   GOTO      SCR02T
     C                   END
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*** Begin add *** 04/01/05 **************************************
     C*****************************************************************
     C* PROCESS SCREEN 9 - EMPLOYEE EARNINGS REPORT                   *
     C*****************************************************************
     C*
     C     SCR09T        TAG
     C*
     C     *IN09         CABEQ     '1'           INQ9
     C*
     C** MAKE SURE THAT IF TO DATE IS ENTERED, SO IS FROM DATE
     C*
     C     SCEETO        IFNE      *ZERO
     C     SCEEFR        ANDEQ     *ZERO
     C                   MOVE      '1'           *IN52
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C** VALIDATE FROM DATE
     C     SCEEFR        IFNE      *ZERO
     C                   Z-ADD     SCEEFR        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEEFR
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN49
     C                   GOTO      STRT
     C                   ENDIF                                                  SCEEFR ENTERED
     C*
     C                   Z-ADD     SCEEFR        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEEFR            7 0
     C*
     C** VALIDATE TO DATE
     C*
     C                   Z-ADD     SCEETO        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEETO
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN53
     C                   GOTO      STRT
     C                   ENDIF                                                  SCEETO ENTERED
     C*
     C                   Z-ADD     SCEEFR        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEETO            7 0
     C*
     C                   Z-ADD     SCEETO        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEETO            7 0
     C*
     C** VALIDATE RANGE
     C     XXEETO        CABLT     XXEEFR        STRT                   51
     C*
     C                   ENDIF                                                  SCEEFR NOT *BLANK
     C*
     C     INQ9          TAG
     C*
     C                   MOVE      'Y'           XXDW19
     C*
     C*   IF LOOPING THRU SCREENS GOTO SCRO2T TAG
     C*
     C     XXPROC        IFEQ      'Y'
     C                   GOTO      SCR02T
     C                   END
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C*** End add ***** 04/01/05 **************************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPLT01     E            PLT01E
     OPL101     E            PL101E
**  TABLE UA
-- ADD --
- UPDATE -
