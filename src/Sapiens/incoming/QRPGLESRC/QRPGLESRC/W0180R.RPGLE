     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*****************************************************************
     H/TITLE W0180R - COMMISSION FILE MAINTENANCE
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/14/14 C:33  Project : 114WPE                               *
     H*                Add plan type.                                 *
     H*                                                               *
     H* 04/17/12 C:255 Project : WC2044                               *
     H*                Remove Code added for WC997 project dated      *
     H*                09/20/07.                                      *
     H*                                                               *
     H* 01/22/10 C:246 PROJECT : 139WNQ                               *
     H*                Include Tertiary Agent on the Validation.      *
     H*                                                               *
     H* 09/09/09 C:33  WC997  WA071/69568                             *
     H*                Do not require a check# for EFT payment voids. *
     H*                                                               *
     H* 04/01/08 C:189 WC997                                          *
     H*                IMPLEMENT EFT - AGENT COMMISSION PAYMENTS      *
     H*                (ADDITIONAL CHANGES FOR CONTROL FLAG WCFL17)   *
     H*                                                               *
     H* 03/19/08 C:189 WC997                                          *
     H*                IMPLEMENT EFT - AGENT COMMISSION PAYMENTS      *
     H*                (ADDITIONAL CHANGES FOR CONTROL FLAG WCFL38)   *
     H*                                                               *
     H*  10/08/07 C:136 SUPPORT                                       *
     H*                 CORRECT SEARCH ROUTINES                       *
     H*                                                               *
     H* 09/20/07 C:136 PROJECT: WC997                                 *
     H*                IMLEMENT EFT - AGENT COMMISSION CHECKS         *
     H*                                                               *
     H*  12/21/06 C:183  PROJECT: 114WND                              *
     H*                  Add a record to WMPHP.                       *
     H*                                                               *
     H*  11/28/06 C:132  PROJECT: WC1423                              *
     H*                  ADD Display for Secondary Agent Flag on      *
     H*                  Screen TWO.                                  *
     H*                                                               *
     H*  02/02/06 C:33   SUPPORT:  CLT129  ON-SITE                    *
     H*                  CORRECTLY DISPLAY NEGATIVE AMOUNT ENTRIES    *
     H*                                                               *
     H*  04/23/02 C:132 SEQ#: WA048/32181                             *
     H*                 Add Edit on Void Commission, to Require a     *
     H*                 Check number on second screen. Msg: WCR3159   *
     H*                                                               *
     H*  03/06/02 C:132 PROJECT WC1071                                *
     H*                 Add Producer Cancellation Date Edit           *
     H*                                                               *
     H* 02/01/02 C:90 PROJECT: WC1071                                 *
     H*               Check producer for inactive "I" status.         *
     H*                                                               *
     H* 07/26/01 C:90 SEQ#: WA113/28376                               *
     H*               Validate the secondary agent number along with  *
     H*               the primary agent number for the emp# and div   *
     H*               for the fund year.                              *
     H*                                                               *
     H* 01/03/00 C:33 SEQ#: WA095/21209                               *
     H*               LOAD THE CORRECT DATE FORMAT TO WMCAP.          *
     H*                                                               *
     H* 03/12/99 C:33 PROJECT: WC577                                  *
     H*               ADD CHECK# TO SCREEN                            *
     H*                                                               *
     H* 09/23/98 C:33 SEQ#: WA101/13872                               *
     H*               ONLY WRITE TO WMACP                             *
     H*                                                               *
     H* 09/21/98 C:33 SEQ#: WA101/13872                               *
     H*               CORRECTLY DISPLAY NEGATIVE AMOUNT ENTRIES       *
     H*                                                               *
     H* 06/15/98 C:90 PROJECT WC577                                   *
     H*               CHANGED SCREENS TO CURRENT STANDARDS            *
     H*                                                               *
     H* 05/22/98 C:91 PROJECT WC577                                   *
     H*               COMBINED SEARCHES INTO ONE F10 KEY SEARCH       *
     H*               CHANGED SCREENS TO CURRENT STANDARDS            *
     H*                                                               *
     H* 01/09/98 C:80 PROJECT# WC806                                  *
     H*               ADDED TRANSACTION EDIT OPTION (F17).  EXPANDED  *
     H*               DATA STRUCTURE XXDATA TO ACCOMODATE 8 DIGIT     *
     H*               DATE.                                           *
     H*                                                               *
     H* 03/28/96 C:11 PROJECT#: WC508                                 *
     H*               FIX RECORD LOCK ON SMCOP.                       *
     H*                                                               *
     H* 07/12/96 C:11 PROJECT#: WC537                                 *
     H*               PAYMENTS AND VOIDS MUST BE ENTERED AT THE AGENT *
     H*               LEVEL, WHICH MEANS THE EMPLOYER# AND PRODUCER#  *
     H*               MUST BE BLANK.                                  *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*  == SCR02 ==                                                  *
     H*   20 - 24                                                     *
     H*   26   WCR1318 - EMPLOYER # MUST BE BLANK FOR P OR V TYPES    *
     H*  == SCR02 ==                                                  *
     H*   20 - 24                                                     *
     H*   25   WCR0602 - PRODUCER # MUST BE BLANK FOR P OR V TYPES    *
     H*****************************************************************
     FW0180F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDB)
     FWDF1L     IF   E           K DISK
     FWMACL     IF A E           K DISK
     FSMCOL     UF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWMAAL1    IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWMAPL     IF   E           K DISK
     FWMAPP     IF   E             DISK
     F                                     RENAME(PAP01:PAP02)
     FWMAGL     IF   E           K DISK
     F                                     INFDS(DBFDS1)
     FWTABL     IF   E           K DISK
     FWMAAP     IF   E             DISK
     F                                     RENAME(PAA01:PAA02)
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
      *** Beg Del ***** 04/17/12 *************************************
     F*** BEG ADD ***** 09/20/07 *************************************
     ******FWMEFL4    IF   E           K DISK
     F*****WMBIL6    IF   E           K DISK
     F*** END ADD ***** 09/20/07 *************************************
      *** End Del ***** 04/17/12 *************************************
     FWMEXL     IF   E           K DISK
     FWMPHP     O    E             DISK
      *** Beg Del ***** 04/17/12 *************************************
     F***  BEGIN ADD  ***  09/20/07  *********************************
     F*****SSYLF     IF   E           K DISK
     F*****                                IGNORE(SSY01)
     F*****                                IGNORE(SPC01)
     F*****                                IGNORE(SRC01)
     F*****                                IGNORE(SBC01)
     F*****                                IGNORE(SCS01)
     F***  END   ADD  ***  09/20/07  *********************************
      *** End Del ***** 04/17/12 *************************************
     D UA              S             10    DIM(1) CTDATA PERRCD(1)
     D MG              S             40    DIM(1) CTDATA PERRCD(1)              MESSAGES
     D*
     D OPTNM           C                   CONST('Agent Commission Mai-
     D                                     ntenance')
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS1          DS
     D  DBRRN1               397    400B 0
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXDATA                 1     80
     D  XXTYPE                 1      1
     D  XXCO#                  2      4
     D  XXUSER                 5     14
     D  XXTIME                15     20
     D  XXDATE                21     28
     D  UPOUTQ                29     38
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D DBFDB           DS
     D  DBRRB                397    400B 0
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C*  FUND DETAIL FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C*  AGENT ASSIGNMENT FILE BY FUND/AGENT
     C*
     C     AAKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCAGT#
      *
      *** Beg Del ***** 04/17/12 *************************************
      *** BEG ADD ***** 09/20/07 **********************************
     C*****BIKY          KLIST
     C*****              KFLD                    UPCO#
     C*****              KFLD                    KKAPPL
     C*****              KFLD                    KKFND
     C*****              KFLD                    KKCKTP
     C*****              KFLD                    KKACCT
      *****
     C*****BIKY1         KLIST
     C*****              KFLD                    UPCO#
     C*****              KFLD                    KKAPPL
     C*****              KFLD                    KKFND
     C*****              KFLD                    KKCKTP
     C*****              KFLD                    KKACCT
     C*****              KFLD                    KKEFDT
      *****
     C*****EFKY          KLIST
     C*****              KFLD                    UPCO#
     C*****              KFLD                    KKFND
     C*****              KFLD                    KKAGT#
      *****
     C*****EFKY1         KLIST
     C*****              KFLD                    UPCO#
     C*****              KFLD                    KKFND
     C*****              KFLD                    KKAGT#            5 0
     C*****              KFLD                    KKEFDT            7 0
      *** END ADD ***** 09/20/07 **********************************
      *** End Del ***** 04/17/12 *************************************
     C* ADMIN/IDARTS FILE
     C     ZKXY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXFYR                          RLS 2
     C*
     C*  AGENT PRODUCER FILE
     C*
     C     APKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C                   KFLD                    SCPROD
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C*  AGENT COMMISSIONS FILE
     C*
     C     ACKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCAGT#
     C                   KFLD                    SCPROD
     C                   KFLD                    X1PODT
     C*
     C*  TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY
     C*
     C     *LIKE         DEFINE    QQPRDS        XXPRDS
     C     *LIKE         DEFINE    QQCALL        XXCALL
     C     *LIKE         DEFINE    ACPODT        X1PODT
     C     *LIKE         DEFINE    SCPODT        XXPODT
     C     *LIKE         DEFINE    SCAMT         SVAMT
     C     *LIKE         DEFINE    SCTOCL        SVTOCL
      *** Beg Del ***** 04/17/12 *************************************
     C***  BEGIN ADD  ***  09/20/07  *********************************
     C******LIKE         DEFINE    SCACCT        ##ACCT
     C******LIKE         DEFINE    SCCKTP        SVCKTP
     C******LIKE         DEFINE    *IN60         FIRST
     C******LIKE         DEFINE    SCAGT#        SVAGT#
     C******LIKE         DEFINE    SCFND         SVFND
     C******LIKE         DEFINE    SCPODT        SVPODT
     C******LIKE         DEFINE    BIAPPL        KKAPPL
     C******LIKE         DEFINE    BICKTP        KKCKTP
     C******LIKE         DEFINE    BIACCT        KKACCT
     C******LIKE         DEFINE    SCFND         KKFND
     C***  END   ADD  ***  09/20/07  *********************************
      *** End Del ***** 04/17/12 *************************************
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
     C                   CLEAR                   XXPRDS
     C*
     C     Q1PRMS        IFGE      1
     C                   MOVE(P)   QQPRDS        XXPRDS
     C                   ENDIF                                                  Q1PRMS>=1
     C*
     C     Q1PRMS        IFGE      2
     C                   MOVE      QQCALL        XXCALL
     C                   ENDIF                                                  Q1PRMS>=2
      *** Beg Del ***** 04/17/12 *************************************
     C***  BEGIN ADD  ***  09/20/07  *********************************
     C*****'WC'          CHAIN     SWC01                              60
     C*****
     C***  BEG   CHG  ***  04/01/08  *********************************
     C***  BEG   CHG  ***  03/19/08  *********************************
     C********** WCFL17    COMP 'Y'                      45
     C********** WCFL38    COMP 'Y'                      45
     C*****WCFL17        COMP      'Y'                                    45
     C***  END   CHG  ***  03/19/08  *********************************
     C***  END   CHG  ***  04/01/08  *********************************
     C***  Begin Add  ***  11/04/08  **************************************
     C*****              MOVE      *IN45         EFT               1
     C***  End   Add  ***  11/04/08  **************************************
     C***  END   CHG  ***  09/20/07  *********************************
      *** End Del ***** 04/17/12 *************************************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C     COKEY         CHAIN(N)  PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD           80
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVE      FNFND         SCFND
     C                   MOVE      FNNAME        SCNAME
     C*
     C                   MOVE      UPFYR         SCFYR
     C                   Z-ADD     SCFYR         XXFYR
     C                   MOVE      UPEMP#        SCEMP#
     C                   MOVE      UPDIV         SCDIV
     C*
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXPODT
     C*
     C                   MOVE      XXPODT        SCPODT
     C                   Z-ADD     1             Z                 2 0
     C*
     C     XXPRDS        IFEQ      *BLANKS
     C                   MOVEL(P)  OPTNM         XXPRDS
     C                   ENDIF                                                  XXPRDS=*BLANKS
     C*
     C                   MOVEL(P)  XXPRDS        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C*
     C     XXCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *** Beg Del ***** 04/17/12 *************************************
     C***  BEGIN ADD  ***  09/20/07  *********************************
     C*******            MOVE      *ON           FIRST
     C***  END   ADD  ***  09/20/07  *********************************
      *** End Del ***** 04/17/12 *************************************
     C                   SETON                                        64
     C     ZKXY1         CHAIN     PEX01                              99
     C*
     C  N99EX3AGT        IFEQ      'Y'
     C*
     C                   SETOFF                                       64
     C*
     C                   ENDIF                                                  EXA3FL EQ 'Y'
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
     C*
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 03 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
      * AGENT  *
     C*
     C     *INKJ         IFEQ      *ON
     C     Q2RECF        IFEQ      'SCR01'
     C*
      *********************
      * GROUP YEAR SEARCH *
      *********************
     C     HFLD          IFEQ      'SCFYR'
     C*
     C     SCFND         IFNE      0
     C     FNKEY         CHAIN     PFN01                              60
     C                   Z-ADD     DBRRB         ##RRN
     C                   ENDIF                                                  SCFND EQ 0
     C*
     C                   CALL      'W0011R'
     C*
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      *ZEROS        ##YR              3 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *IN27
     C                   ENDIF
     C*
     C     ##YR          IFNE      *ZEROS
     C                   MOVE      ##YR          SCFYR
     C                   MOVE      *ON           *IN28                          PC @ SCAGT#
     C                   ELSE
     C                   MOVE      *ON           *IN27                          PC @ SCFYR
     C                   ENDIF                                                  *IN60
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCFYR
     C*
      *******************
      * AGENT # SEARCH  *
      *******************
     C     HFLD          IFEQ      'SCAGT#'
     C*
     C                   CLEAR                   ##RRN2
     C     SCFND         IFNE      0
     C     FNKEY         CHAIN     PFN01                              60
     C                   Z-ADD     DBRRB         ##RRN2
     C                   ENDIF                                                  SCFND EQ 0
     C*
     C                   CALL      'W0040R'
     C*
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN2            9 0
     C                   PARM                    ##VIEW            1 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         CABEQ     *ON           STRT                     28     PC @ AGENT
     C*
     C     ##RRN2        CHAIN     PAA02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  AAAGT#        SCAGT#
     C                   MOVE      *ON           *IN29                          PC @ DATE
     C                   ELSE
     C                   MOVE      *ON           *IN28                          PC @ AGENT
     C                   ENDIF                                                  *IN60       O
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCAGT#
     C*
      *****************************
      * EMPLOYER/DIVISION SEARCH  *
      *****************************
     C     HFLD          IFEQ      'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C*
     C                   CLEAR                   ##RRN
     C                   CLEAR                   ##RRN2
     C                   CLEAR                   ##RRN3
     C*
     C     SCFND         IFNE      0
     C     FNKEY         CHAIN     PFN01                              60
     C                   Z-ADD     DBRRB         ##RRN2
     C                   ENDIF                                                  SCFND EQ 0
     C*
     C                   CALL      'W0050R'
     C*
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM                    ##RRN2            9 0
     C                   PARM                    ##QTE             1
     C                   PARM                    ##VIEW            1 0
     C                   PARM                    ##RRN3            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     HFLD          CABEQ     'SCEMP#'      STRT                     30     PC @ EMP
     C     HFLD          CABEQ     'SCDIV'       STRT                     31     PC @ EMP
     C                   ENDIF                                                  *INKL       O
     C*
     C     ##RRN         CHAIN     PEM02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     EMEMP#        SCEMP#
     C                   MOVEL     EMDIV         SCDIV
     C                   MOVE      *ON           *IN32                          PC @ SCCOCD
     C                   ELSE
     C                   MOVE      *ON           *IN30                          PC @ EMP
     C                   ENDIF                                                  *IN60       O
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCEMP#/SCDIV
     C*
      ***************************
      * COMMISSION CODE SEARCH  *
      ***************************
     C*    TABLE FILE SEARCH - COMMISSION CODE
     C*
     C     HFLD          IFEQ      'SCCOCD'
     C*
     C                   MOVE      '0'           *IN25
     C*
     C                   MOVE      *BLANK        QQKEY            10
     C*
     C                   CALL      'W2201R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    QQKEY            10
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC
     C*
     C     *INKL         IFEQ      *ON
     C     QQKEY         OREQ      *BLANKS
     C                   MOVE      *ON           *IN32
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVEL     QQKEY         SCCOCD
     C                   MOVE      *ON           *IN49
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL
     C*
     C                   ENDIF                                                  SCCOCD
     C*
      *** Beg Del ***** 04/17/12 *************************************
     C***  BEGIN ADD  ***  09/20/07  *********************************
     C*****HFLD          IFEQ      'SCACCT'
     C*****HFLD          OREQ      'SCCKTP'
     C*****
     C*****              MOVE      UPCO#         ##CO#
     C*****              MOVE      SCFND         ##FND
     C*****
     C*****              CALL      'W2420R'
     C*****              PARM      '2'           ##CALL            1
     C******INKC         PARM                    ##KC              1
     C******INKL         PARM                    ##KL              1
     C*****              PARM                    ##CO#             3
     C*****              PARM                    ##FND             3
     C*****              PARM      *BLANKS       ##ACCT
     C*****              PARM      *ZEROS        ##EFDT            8
     C*****              PARM      'AG'          ##APPL            2
     C*****              PARM      *BLANKS       ##CKTP           10
     C*****
     C******INKC         CABEQ     *ON           TERM
     C*****
     C******INKL         IFEQ      *ON
     C*****##ACCT        OREQ      *BLANKS
     C*****              MOVE      *ON           *IN46
     C*****              ELSE
     C*****              MOVEL(P)  ##ACCT        SCACCT
     C*****              MOVEL(P)  ##CKTP        SCCKTP
     C*****              MOVE      *OFF          FIRST
     C*****              Z-ADD     SCFND         SVFND
     C*****              Z-ADD     SCAGT#        SVAGT#
     C*****              Z-ADD     SCPODT        SVPODT
     C*****              ENDIF                                                  *INKL=*ON
     C*****
     C*****              MOVEL(P)  'SCR01'       Q2RECF
     C*****              GOTO      STRT
     C*****
     C*****              ENDIF                                                  HFLD=SCACCT
     C***  END   ADD  ***  09/20/07  *********************************
      *** End Del ***** 04/17/12 *************************************
     C                   MOVE      *ON           *IN33                          EM MC40004
     C*
     C                   ENDIF                                                  SCR01
     C*
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C*****************************
     C* PRODUCER SEARCH LIST CALL *
     C*****************************
     C     HFLD          IFEQ      'SCPROD'
     C*
     C                   CALL      'W0042R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      DBRRN1        ##RRN             9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZERO
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PAP02                              99
     C*
     C                   MOVE      APPROD        SCPROD
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKL
     C*
     C                   ENDIF                                                  HFLD SCPROD
     C*
     C                   ENDIF                                                  SCR02
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKJ
     C                   ENDIF                                                  *IN11
      **********************
      * F12 = PRIOR SCREEN *
      **********************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   ENDIF                                                  *IN11
     C*
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
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   GOTO      STRT01
     C                   END
     C*
     C*
     C*
     C* ADD CMD 17 FOR EDIT REQUEST
     C*
     C*   SUBMIT THE EDIT REQUEST
     C*
     C     *INKR         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   MOVE      'G'           XXTYPE
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        XXUSER
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      DSTIME        XXTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   CALL      'QSNDDTAQ'
     C                   PARM      'WEDITQ'      XXQNM            10
     C                   PARM      '*LIBL'       XXLIB            10
     C                   PARM      80            XXFLDL            5 0
     C                   PARM                    XXDATA
     C*
     C                   MOVE      MG(1)         SCMSG
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  Q2RECF IFEQ 'SCR01'
     C*
     C                   ENDIF                                                  *INKR IFEQ '1'
     C*
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*
     C*  VALIDATE THE FUND YEAR ENTERED.
     C*
     C                   Z-ADD     SCFYR         XXFYR             3 0
     C     F1KEY         CHAIN     PF101                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE AGENT# FOR THE FUND & GET AGENT NAME
     C*
     C     AAKEY         CHAIN     PAA01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C     AGKEY         CHAIN     PAG01                              60
     C                   MOVE      AGNAME        SCAGNM
     C*  EMP# & DIV IFNE 0: VALIDATE EMP#, DIV, AGT#
     C*
     C     SCEMP#        IFNE      0
     C     SCDIV         ORNE      0
     C*
     C*  THE EMPLOYER AND DIV MUST BE ON FILE
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE AGENT NUMBER FOR EMP# AND DIV FOR THE FUND YEAR
     C*
     C     AHKEY         CHAIN     PAH01                              60
     C*                                                    RLS 2
     C*
     C                   MOVE      'N'           XXFOND            1            RLS 2
     C     XXFOND        DOWNE     'Y'                                          |
     C     *IN60         ANDEQ     '0'                                          |
     C     SCAGT#        IFEQ      AHAGT#                                       |
     C     SCAGT#        OREQ      AHSAGT                                       |
     C     SCAGT#        OREQ      AH3AGT                                       |
     C                   MOVE      'Y'           XXFOND                         |
     C                   MOVE      '1'           *IN60                          |
     C                   END                                                    |
     C     *IN60         IFEQ      '0'                                          |
     C     AHKEY         READE     PAH01                                  60    |
     C                   END                                                    |
     C                   END                                                    |
     C*                                                    |
     C     XXFOND        CABEQ     'N'           STRT                     23    |
     C*
     C                   END
     C*
     C*  VALIDATE POST DATE ENTERED
     C*
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPODT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE COMMISSION CODE
     C*
     C                   MOVEL     SCCOCD        XXKEY            10
     C     TBKEY         CHAIN     PCS01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C     CSSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*   COMMISSION PAYMENTS & VOIDS CANNOT BE ENTERED AT THE EMPLOYER LEVEL.
     C*
     C     CSTYPE        IFEQ      'P'                                          PAYMENTS
     C     CSTYPE        OREQ      'V'                                          VOIDS
     C     SCEMP#        IFNE      *ZERO
     C     SCDIV         ORNE      *ZERO
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
      *** Beg Del ***** 04/17/12 *************************************
     C*****EFT           IFEQ      *ON
     C***  BEGIN ADD  ***  09/20/07  *********************************
     C******
     C*  DEFAULT BANK ACCOUNT
     C******
     C*****FIRST         IFEQ      *ON
     C*****SCACCT        ANDEQ     *BLANK
     C*****SCFND         ORNE      SVFND
     C*****FIRST         ANDEQ     *OFF
     C*****SCAGT#        ORNE      SVAGT#
     C*****FIRST         ANDEQ     *OFF
     C*****SCPODT        ORNE      SVPODT
     C*****FIRST         ANDEQ     *OFF
     C*****              Z-ADD     SCFND         SVFND
     C*****              Z-ADD     SCAGT#        SVAGT#
     C*****              Z-ADD     SCPODT        SVPODT
     C*****              MOVE      *OFF          FIRST
     C*****              MOVE      *ON           *IN46
     C*****              Z-ADD     SCFND         KKFND
     C*****              Z-ADD     SCAGT#        KKAGT#
     C*****              Z-ADD     SCPODT        SRDTI
     C*****              CALL      'SRCVTDAT'    DTCYMD
     C*****              MOVEL     SRDTO         KKEFDT
     C*****              MOVEL     SCCKTP        SVCKTP
     C*****EFKY1         SETGT     PEF01
     C*****EFKY          READPE    PEF01                                  60
     C******IN60         IFEQ      *OFF
     C*****              MOVEL(P)  'EFT'         SCCKTP
     C*****              ELSE
     C*****              MOVE      *BLANKS       SCCKTP
      *  IF DIFFERENT BANK CODES ARE ASSIGNED TO 'AG'ENT BANK APPLICATION TYPES
      *  WA023R1 WOULD BE CALLED FROM THIS POINT TO DEFAULT VALUES WITH.
     C*****              ENDIF
     C*****
     C*****SVCKTP        IFNE      *BLANKS
     C*****SVCKTP        ANDNE     'EFT'
     C*****              MOVEL     SVCKTP        SCCKTP
     C*****              ENDIF
     C*****
     C*****              MOVE      FNCO#         #CO#                           Company
     C*****              MOVE      FNFND         #FND                           Group #
     C*****              MOVE      SCPODT        ##SRDT
     C*****              CALL      'WA023R'
     C*****              PARM                    #CO#              3            Company
     C*****              PARM                    #FND              3            Group #
     C*****              PARM      'AG'          ##APPL            2            Applic Code
     C*****              PARM                    ##SRDT            8            MMDDCCYY
     C*****              PARM                    ##ACCT                         Bank Acct #
     C*****              PARM      'C'           ##CALL            1            (V)oucher/(C)heck
     C*****              PARM      *BLANK        ##HDCY            1            Hardcopy Y/N
     C*****              PARM      *BLANK        ##CHKQ           10            Check/Voucher OUTQ
     C*****              PARM      *BLANK        ##FTYP           10            Form Type
     C*****              PARM      *BLANK        ##RTCD            1            Return Code
     C*****              PARM      *ZERO         ##DT              7            WMBIP Eff. DT CYYMMD
     C*****              PARM      *BLANK        ##CPYQ           10
     C*****              PARM      SCCKTP        ##CKTP           10
     C*****
     C*****##ACCT        IFNE      *BLANK
     C*****              MOVEL(P)  ##ACCT        SCACCT
     C*****              GOTO      STRT
     C*****              ENDIF                                                  ##RTCD=*BLANK
     C*****
     C*****              ENDIF                                                  SCACCT = *BLANKS
     C*****
     C*****VALIDATE BANK ACCOUNT
     C*****
     C*****              MOVE      FNFND         KKFND                          Group #
     C*****              MOVEL     'AG'          KKAPPL
     C*****              MOVEL     SCCKTP        KKCKTP
     C*****              MOVEL     SCACCT        KKACCT
     C*****              Z-ADD     SCPODT        SRDTI
     C*****              CALL      'SRCVTDAT'    DTCYMD
     C*****              MOVEL     SRDTO         KKEFDT
     C*****
     C*****BIKY1         SETLL     PBI01
     C*****BIKY          READE     PBI01                                  60
     C*****BISTAT        DOWEQ     'I'
     C*****BIKY          READE     PBI01                                  60
     C*****              ENDDO
     C*****
     C******IN60         IFEQ      *ON                                          NOT FOUND
     C*****              MOVE      *ON           *IN48
     C*****              GOTO      STRT
     C*****              ENDIF                                                  ##RTCD
      *
      *  WARN THAT AGENT IS NOT SET UP FOR EFT
      *
     C*****SCCKTP        IFNE      SVCKTP
     C*****              MOVEL     SCCKTP        SVCKTP
     C*****              Z-ADD     SCFND         KKFND
     C*****              Z-ADD     SCAGT#        KKAGT#
     C*****              Z-ADD     SCPODT        SRDTI
     C*****              CALL      'SRCVTDAT'    DTCYMD
     C*****              MOVEL     SRDTO         KKEFDT
     C*****EFKY1         SETGT     PEF01
     C*****EFKY          READPE    PEF01                                  60
     C******IN60         IFEQ      *ON
     C*****EFEXDT        ORLE      KKEFDT
     C*****EFEXDT        ANDNE     *ZERO
     C*****SCCKTP        IFEQ      'EFT'
     C*****              EXFMT     EFTERR
     C*****              MOVEL(P)  'SCR01'       Q2RECF
     C*****              ENDIF
     C*****              ENDIF
     C*****              ENDIF
     C*** END ADD *******  09/20/07  *********************************
     C*
     C******             ENDIF
      *** End Del ***** 04/17/12 *************************************
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      CSTYPE        SCCOTP
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANK        SCAMT
     C                   Z-ADD     SCAMT         SVAMT
     C                   CLEAR                   SCCHCK
     C                   MOVE      *BLANK        SCTOCL
     C                   Z-ADD     SCTOCL        SVTOCL
     C                   MOVE      *BLANK        SCPROD
     C                   MOVE      CSDESC        SCDESC
     C***  Begin Add  ***  10/14/14  **************************************
     C                   Clear                   scPlan
     C***  End   Add  ***  10/14/14  **************************************
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C     *IN09         CABEQ     '1'           NOEDT
     C*
     C     SVAMT         IFNE      SCAMT
     C                   Z-ADD     SCAMT         SVAMT
     C                   MOVE      *ON           *IN31
     C                   GOTO      STRT
     C                   ENDIF                                                  SVAMT<>SCAMT
     C*
     C     SCAGT2        IFNE      *BLANK
     C     SCAGT2        ANDNE     'Y'
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   ENDIF                                                  SCAGT2 <> "Y"
     C     SCAGT3        IFNE      *BLANK
     C     SCAGT3        ANDNE     'Y'
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   ENDIF                                                  SCAGT3 <> "Y"
     C*
     C     SVTOCL        IFNE      SCTOCL
     C                   Z-ADD     SCTOCL        SVTOCL
     C                   MOVE      *ON           *IN32
     C                   GOTO      STRT
     C                   ENDIF                                                  SVTOCL<>SCTOCL
     C*
     C*  AMOUNT CAN NOT BE EQUAL TO 0
     C*
     C     SCAMT         IFEQ      0
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK # MUST BE PRESENT ON VOID COMMISSION PAYMENT
     C*
      *** Beg Del ***** 04/17/12 *************************************
     C*****SCCKTP        IFNE      'EFT'
      *** End Del ***** 04/17/12 *************************************
     C     SCCOTP        IFEQ      'V'
     C     SCCHCK        ANDLE     *ZERO
     C                   MOVE      *ON           *IN78
     C                   GOTO      STRT
     C                   ENDIF
      *** Beg Del ***** 04/17/12 *************************************
     C*****              ENDIF
      *** End Del ***** 04/17/12 *************************************
     C*
     C*
     C*  CHECK # CAN NOT BE LESS THAN *ZERO
     C*
     C     SCCHCK        CABLT     *ZERO         STRT                   79
     C*
     C*  VALIDATE THE PRODUCER NUMBER
     C*
     C     SCEMP#        IFEQ      0
     C     SCPROD        ANDNE     *ZERO
     C*
     C*   PRODUCER MUST BE ZERO FOR COMMISSION PAYMENTS & VOIDS.
     C*
     C     SCCOTP        IFEQ      'P'                                          PAYMENTS
     C     SCCOTP        OREQ      'V'                                          VOIDS
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C     SCCOTP        IFNE      'A'
     C     SCCOTP        ANDNE     'P'
     C     SCCOTP        ANDNE     'V'
     C     SCPROD        ANDEQ     *ZERO
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C     SCPROD        IFNE      *ZERO
     C     APKEY         CHAIN     PAP01                              60
     C     *IN60         IFEQ      '1'
     C     APSTAT        OREQ      'I'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  DESCRIPTION CAN NOT BE BLANK
     C*
     C     SCDESC        IFEQ      *BLANK
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDTE1            7 0
     C                   MOVE      XXDTE1        XXDTE             6 0
     C*
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1PODT
     C*
     C* IF HAVE PRODUCER CANCEL DATE, CHECK AGAINST POST DATE
     C     APCXDT        IFGT      *ZEROS
     C     APCXDT        ANDLE     X1PODT
     C                   MOVE      *ON           *IN34
     C                   GOTO      STRT
     C                   END
     C*
     C     ACKEY         CHAIN     PAC01                              40
     C                   MOVE      UPCO#         ACCO#
     C                   MOVE      SCFND         ACFND
     C                   MOVE      SCFYR         ACFYR
     C                   MOVE      SCEMP#        ACEMP#
     C                   MOVE      SCDIV         ACDIV
     C                   MOVE      SCAGT#        ACAGT#
     C*
     C                   MOVE      SCAGT2        ACAGT2
     C                   MOVE      SCAGT3        ACAGT3
     C*
     C                   MOVE      SCPROD        ACPROD
     C***  Begin Add  ***  10/14/14  **************************************
     C                   Eval      ACplan = scPlan
     C***  End   Add  ***  10/14/14  **************************************
     C                   MOVE      X1PODT        ACPODT
     C                   Z-ADD     XXDTE1        ACTRDT
     C                   MOVE      SCTOCL        ACTOCL
     C                   MOVE      *ZEROS        ACRATE
     C                   MOVE      SCAMT         ACAMT
     C                   Z-ADD     SCCHCK        ACCHCK
     C                   MOVE      SCCOCD        ACCOCD
     C                   MOVE      SCCOTP        ACCOTP
     C                   MOVE      SCDESC        ACDESC
     C                   MOVE      CSSTAT        ACSTAT
     C                   MOVE      Q1USER        ACUSER
     C                   MOVE      Q1PGM         ACPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*
     C                   MOVE      Q1USER        ACIUSR
     C                   MOVE      Q1PGM         ACIPGM
     C                   MOVEL     SRDTO         ACIDT
     C     DSTIME        ADD       Z             ACITM
     C                   ADD       1             Z
     C                   MOVEL     SRDTO         ACDT
     C     DSTIME        ADD       Z             ACTIME
     C                   ADD       1             Z
     C*
     C     COKEY         CHAIN     PCO01                              60        JJJ 9-19-88
     C                   ADD       1             COBUL2                         |
     C                   Z-ADD     COBUL2        ACBUL2                         |
     C                   UPDATE    PCO01                                        |
     C                   WRITE     PAC01
     C                   CLEAR                   PPH01
     C                   MOVE      UPCO#         PHCO#
     C                   MOVE      SCFND         PHFND
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
      *
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI
     C                   PARM      '     '       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C                   MOVEL     SRDTO         PHDT
      *
     C                   MOVE      DSTIME        PHTIME
     C                   MOVE      Q1USER        PHUSER
     C                   MOVE      Q1PGM         PHPGM
      *
     C                   Z-ADD     Q1DT          SRDTI
      *
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C                   MOVEL     SRDTO         PHIDT
     C                   MOVE      Q1TIME        PHITIM
     C                   MOVE      Q1USER        PHIUSR
     C                   MOVE      Q1PGM         PHIPGM
     C                   MOVE      'A'           PHTYPE
      *
     C                   Z-ADD     SCPODT        SRDTI
      *
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI
     C                   PARM      '     '       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C                   MOVEL     SRDTO         PHPODT
      *
     C                   MOVE      SCCOCD        PHCOCD
     C                   MOVE      SCCOTP        PHCOTP
     C                   MOVE      SCDESC        PHCODS
     C                   MOVE      SCFYR         PHFYR
     C                   MOVE      SCCHCK        PHCHK#
     C                   MOVE      SCAMT         PHAMT
     C                   MOVE      SCTOCL        PHPREM
     C                   MOVE      SCAGT#        PHAGT#
     C                   MOVE      SCAGNM        PHAGNM
     C                   MOVE      SCPROD        PHPROD
     C     APKEY         CHAIN     PAP01                              60
     C     *IN60         IFEQ      '0'
     C     APLSN         CAT       ', '          PHPRNM
     C     APFSN         CAT       PHPRNM        PHPRNM
     C                   ELSE
     C                   MOVE      ' '           PHPRNM
     C                   ENDIF
     C                   MOVE      SCEMP#        PHEMP#
     C                   MOVE      SCDIV         PHDIV
     C     EMKEY         CHAIN     WMEML                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      EMNAME        PHEMNM
     C                   ELSE
     C                   MOVE      ' '           PHEMNM
     C                   ENDIF
     C                   WRITE     PPH01
     C     NOEDT         TAG
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     0             SCAGT#
     C                   MOVE      *BLANK        SCCOCD
     C                   MOVE      *BLANK        SCMODE
     C                   GOTO      STRT
     C*
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
**  TABLE UA
-- ADD --
**
Edit request has been submitted
