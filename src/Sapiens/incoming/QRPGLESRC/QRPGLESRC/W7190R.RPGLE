     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE W7190R - Prompt for Employer's Experience Report
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM ALLOWS THE USER TO ENTER THE PARTICULAR    **
     H**     DATA: FUND YR, DATE, EMPLOYER, AGENT WITHIN  A          **
     H**     SPECIFIED SELECTED CRITERIA  WITHIN SELECTED DATA SORT  **
     H**     FOR EMPLOYER'S EXPERIENCE REPORT.                       **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     MENU                                                    **
     H**   CALLS TO:                                                 **
     H**     W7190C                                                  **
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  02/14/14 C:279 Project: 129WSB  Priss: 24                    *
     H*                 Corrected field "Lost time/Medical/Both" edit.*
     H*                                                               *
     H*  11/22/13 C:279 Project: 129WSB                               *
     H*                 - Remove field "Lost time/Medical/Both"       *
     H*                 - Add field "Exclude Claim Type"              *
     H*                                                               *
     H*  11/01/13 C:287 Support: SEQ84035                             *
     H*                 Set checkbox fields to default 'N' in startup *
     H*                 subroutine SR1ST.                             *
     H*                                                               *
     H*  11/18/08 C:136 PROJECT: WC1670                               *
     H*                 PULL CLIENT SPECIFIC CODING FOR 71WHI INTO    *
     H*                 BASE. THIS WILL MAKE THE LOST TIME, TOTAL     *
     H*                 AMOUNT INCURRED, GREATER THAN/LESS THAN,      *
     H*                 SORT SEQUENCE                                 *
     H*                                                               *
     H*  03/13/08  C:191 PROJECT-132WIT                               *
     H*                  Fix date parameter length in LDA             *
     H*                                                               *
     H*  10/24/07  C:189 PROJECT-132WIT                               *
     H*                  Automate Audit Req/Loss Exp/Darts Aggregate  *
     H*                                                               *
     H*  08/10/07  C:53  PROJECT: SY157                               *
     H*                  CORRECT CURSOR POS ON AGENT                  *
     H*                  CORRECT INDICATOR ASSIGNMENT ON LT FLAG ERROR*
     H*                                                               *
     H*  04/02/07  C:62  SUP#: WC099/49323                            *
     H*                  ADD AGENT SEARCH.                            *
     H*                                                               *
     H* 03/01/07 C:61  PROJECT 132WHJ                                 *
     H*              - Change SS# select from (Y/ ) to (Y/N/4).       *
     H*              - Non-display and default SS# to '4' for clt132. *
     H*                                                               *
     H*  09/28/06 C: 132 WA099 - 54289                                *
     H*                  Change SS# Select from (Y/N) to  (Y/ )       *
     H*                                                               *
     H*  09/20/06 C: 58 PROJECT: 71WHI                                *
     H*           Add LTFL, GT/LT-Incurred and seqence D + Z fields
     H*                   == FOR CLIENT 71 ONLY ==
     H*                                                               *
     H*  06/21/05 C: 58 PROJECT: WC1318                               *
     H*                 Add parameter 'Print SS#.'                    *
     H*                                                               *
     H*  05/14/01 C:136 PROJECT: WC577                                *
     H*                 Display location of output file.              *
     H*                                                               *
     H* 07/08/98  C:33  SEQ#: WC082/12431                             *
     H*                 DO NOT ALLOW POLICY YEARS THAT ARE NOT VALID  *
     H*                 FOR THE EMPLOYER.                             *
     H*                                                               *
     H* 05/29/98  C:33  SEQ#: WC078/11786                             *
     H*                -USE SCDIV AS AN ALPHA FIELD TO VERIFY THAT THE*
     H*                 USER WOULD LIKE ONE OR ALL DIVISIONS.         *
     H*                -PASS THE GROUP RRN TO THE EMPLOYER SEARCH.    *
     H*                                                               *
     H* 12/23/97  C:35  PROJECT: WC577                                *
     H*                 DEFAULT DATE TO CURRENT DATE INSTEAD OF USER  *
     H*                 PARMS PRINT DATE.                             *
     H*                                                               *
     H* 11/03/97  C:58  PROJECT: WC577                                *
     H*                 V3 Implementation                             *
     H*                                                               *
     H*****************************************************************
     H*** PROGRAMMER NOTE - IF YOU ADD PARMS TO CALL OF W7190C        *
     H***                   YOU MUST CHANGE MBR PROFILE PGM W7730C    *
     H*                                                               *
     H**** BEGIN ADD ***** 06/21/05  *********************************
     H*    FOR 'PRINT SS#', MISSING PARM WILL DEFAULT TO 'N' AND THE
     H*  SS# WILL NO LONGER APPEAR ON REPORT.  IF CLIENTS NEED IT,
     H*  W7730C AND/OR ITS CALLER CAN BE MODIFIED TO FORCE 'Y' -- OR
     H*  TO ENABLE USER CHOICE.
     H**** END ADD ******* 06/21/05  *********************************
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*     11   -      COMMAND KEY DEPRESSED                         *
     H*     22   -      OPEN CASE SELECTION NOT EQUAL 'Y' OR 'N'      *
     H*     23   -      CLOSE CASE SELECTION NOT EQUAL 'Y' OR 'N'     *
     H*     24   -      INVALID SEQUENCE CODE                         *
     H*     25   -      INVALID FUND YEAR                             *
     H*     26   -      INVALID EMPLOYER                              *
     H*     28   -      INVALID AGENT                                 *
     H*     29   -      SORT SELECTION EQUAL '1', '2'   OR BLANK      *
     H*     30   -      SORT SELECTION EQUAL '1' BUT ONLY ONE AGENT   *
     H*     31   -      Print SS# flag is invalid
     H*** BEG CHG *** 08/10/07 ****************************
     H******32   -      LTFL not L, M, B
     H*     72   -      LTFL not L, M, B
     H*** END CHG *** 08/10/07 ****************************
     H*     33   -      TINC < zero
     H*     34   -      GTLT not G or L
     H*     35   -      EMPLOYER W/NO CLAIMS <> 'Y' OR 'N'            *
     H*     36   -      REMINDER:BLANK DIV FIELD SPECIFIES ALL DIV    *
     H*    N37   -      FIRST TIME THRU OR SCDIV HAS CHANGED          *
     H*     40   -      TINC > 0 and GTLT not blank mutually inclusive
     H*     60   -      GENERAL CHAINING                              *
     H*
     H*     91   -      CLIENT 71 BEING PROCESSED
     H************************************************************************
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    SMCOL (PCO01): CO                                        **
     H**    WMUPL (PUP01): USER                                      **
     H**    WMFNL (PFN01): CO/FND                                    **
     H**    WMEML (PEM01): CO#/FND/EMP#/DIV                          **
     H**    WMAAL1 (PAA01):CO#/FND/AGT#                              **
     H**    WDF1L (PF101): CO#/FND/FYR                               **
     H************************************************************************
     FW7190F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F*
     FWMFNL     IF   E           K DISK
     F***  BEGIN ADD  ***  05/29/98  *********************************
     F                                     INFDS(FNFDS)
     F***  END   ADD  ***  05/29/98  *********************************
     F* * * Group File
     F*
     FWDF1L     IF   E           K DISK
     F* * * Fund Detail File
     F*
     FSMCOL     IF   E           K DISK
     F* * * Company File
     F*
     FWMUPL     IF   E           K DISK
     F* * * USER PARAMETER FILE
     F*
     F* * * EMPLOYER FILE
     F*
     FWMEML     IF   E           K DISK
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
     F***  BEGIN ADD  ***  07/08/98  *********************************
     FWDELL1    IF   E           K DISK
     F***  END   ADD  ***  07/08/98  *********************************
     F*
     FWMAAL1    IF   E           K DISK
     F*** BEGIN ADD *** 04/02/07 **************************************
     FWMAAP     IF   E             DISK
     F                                     RENAME(PAA01:PAA01P)
     F*** BEG   ADD *** 10/24/07 **************************************
     FSTCTL1    IF   E           K DISK
     F*** END   ADD *** 10/24/07 **************************************
     F*** END   ADD *** 04/02/07 **************************************
     F* * * AGENT ASSIGNMENT FILE

     D*
     D****** beg add ******** 11/22/13 *************************************
     D qqLtFl          s              1
     D****** end add ******** 11/22/13 *************************************
     D***  BEGIN ADD  ***  05/29/98  *********************************
     D***  BEG   ADD  ***  10/24/07  *********************************
     D*
     D LDA            UDS
     D  DDCO#                  1      3
     D  DDFND                  4      6
     D*** BEG CHG ****** 03/13/08 **********************************************
     D*************                           7  13 DDDTE
     D*************                          14  16 DDFYR
     D*************                          17  17 DDSELO
     D*************                          18  18 DDSELC
     D*************                          19  21 DDMNTH
     D*************                          22  22 DDSORT
     D*************                          23  32 DDOUTQ
     D*************                          33  33 DDMODE
     D*************                          34  42 DDEMP#
     D*************                          43  47 DDDIV
     D*************                          48  48 DDSRGN
     D*************                          49  53 DDAGT#
     D*************                          54  54 DDSAGT
     D*************                          55  55 DDEMPC
     D*************                          56  56 DDPRSS
     D*************                          57  57 DDLTFL
     D*************                          58  70 DDTINC
     D*************                          71  71 DDGTLT
     D*************                          72  76 DDRMK
     D  DDDTE                  7     14
     D  DDFYR                 15     17
     D  DDSELO                18     18
     D  DDSELC                19     19
     D  DDMNTH                20     22
     D  DDSORT                23     23
     D  DDOUTQ                24     33
     D  DDMODE                34     34
     D  DDEMP#                35     43
     D  DDDIV                 44     48
     D  DDSRGN                49     49
     D  DDAGT#                50     54
     D  DDSAGT                55     55
     D  DDEMPC                56     56
     D  DDPRSS                57     57
     D  DDLTFL                58     58
     D  DDTINC                59     71
     D  DDGTLT                72     72
     D  DDRMK                 73     77
     D*** END CHG ****** 03/13/08 ************************************
     D***  END   ADD  ***  10/24/07  *********************************
     D*
     D FNFDS           DS
     D  FNRRN                397    400B 0
     D*
     D OPTNM           C                   CONST('Employer Experience -
     D                                     Report')
     D*** BEG   ADD *** 10/24/07 **************************************
     D OPTNM1          C                   CONST('-Auto Agg')
     D*** END   ADD *** 10/24/07 **************************************
     D***  END   ADD  ***  05/29/98  *********************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D***  BEGIN ADD  ***  05/29/98  *********************************
     D                 DS
     D  SCDIV                  1      5
     D  XXDIV1                 1      1
     D  XXDIV2                 2      2
     D  XXDIV3                 3      3
     D  XXDIV4                 4      4
     D  XXDIV5                 5      5
     D***  END   ADD  ***  05/29/98  *********************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DATA                   1    194
     D  SCFFND                 1      3  0

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
     C*   KEY LIST FOR FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND             3 0
     C*
     C*   KEY LIST FOR THE FUND DETAIL
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR THE EMPLOYER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    XXDIV
     C     EMKY2         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCEMP#
     C***  BEGIN ADD  ***  07/08/98  *********************************
     C*
     C*   EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCEMP#
     C                   KFLD                    XXDIV
     C*
     C     ELKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCEMP#
     C***  END   ADD  ***  07/08/98  *********************************
     C*
     C*   KEY LIST FOR THE AGENT ASSIGNMENT FILE
     C*
     C     AGKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCAGT#
     C*
     C     *LIKE         DEFINE    EMEMP#        XZEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C***  BEGIN ADD  ***  05/29/98  *********************************
     C     *LIKE         DEFINE    SCDIV         XZDIV
     C***  END   ADD  ***  05/29/98  *********************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C*** BEG   ADD *** 10/24/07 **************************************
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@EMP#            9 0
     C                   PARM                    @@DIV             5 0
     C                   PARM                    @@FYR             3 0
     F*** END   ADD *** 10/24/07 **************************************
     C*
     C***  BEGIN ADD  ***  05/29/98  *********************************
     C     Q1PRMS        IFGE      1
     C                   MOVE      QQCALL        XXCALL            1
     C                   ENDIF                                                  Q1PRMS>=1
     C***  END   ADD  ***  05/29/98  *********************************
     C*
     C*** BEG   ADD *** 10/24/07 **************************************
     C     XXCALL        IFEQ      '*'
     C                   MOVE      *ON           *IN69
     C                   Z-ADD     @@FND         SCFND
     C                   Z-ADD     @@EMP#        SCEMP#
     C                   MOVEL     @@DIV         SCDIV
     C                   Z-ADD     @@FYR         SCFYR
     C                   ENDIF                                                  Q1PRMS>=1
     F*** END   ADD *** 10/24/07 **************************************
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
     C* FIRST CYCLE PROCESSING
     C*
      **** BEGIN CHG **** 12/23/97 *********************************************
     C******* N11          EXSR SR1ST
     C                   EXSR      SR1ST
      **** END   CHG **** 12/23/97 *********************************************
     C*
     C     STRT01        TAG
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*** BEG ADD ******** 05/14/01 ********************************************
     C*
     C*   DISPLAY LOCATION OF OUTPUT FILE
     C*
     C                   CALL      'SA030R'
     C                   PARM                    QQSUFX            3
     C     'PDSRPT'      CAT(P)    QQSUFX        XXRPTL           10
     C                   CAT(P)    '.':0         XXRPTL
     C                   MOVE      XXRPTL        SCRPTL
     C*
     C*** END ADD ******** 05/14/01 ********************************************
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F3=Exit
     C*
     C     *INKC         IFEQ      '1'
     C*** BEG   ADD *** 10/24/07 **************************************
     C     Q1PRMS        IFGE      2
     C                   MOVE      'C'           QQCALL
     C                   ENDIF
     C*** END   ADD *** 10/24/07 **************************************
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     F*** BEG   ADD *** 10/24/07 **************************************
     C*
     C*   F12 =CANCEL
     C*
     C     *IN69         IFEQ      '1'
     C     *INKL         IFEQ      '1'
     C     Q1PRMS        IFGE      2
     C                   MOVE      'L'           QQCALL
     C                   ENDIF
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C                   END
     F*** END   ADD *** 10/24/07 **************************************
     C*   F10 = SEARCH
     C     *INKJ         IFEQ      '1'
     C*
     C*     Searches for screen 1
     C     Q2RECF        IFEQ      'SCR01'
     C     HREC          ANDEQ     'SCR01'
     C*
     C*    Employer Search
     C***  BEGIN ADD  ***  10/24/07  *********************************
     C     *IN69         IFEQ      *OFF
     C***  END   ADD  ***  10/24/07  *********************************
     C     HFLD          IFEQ      'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
      *
     C                   CALL      'W0050R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##MODE            1
     C                   PARM      0             ##RRN             9 0
     C***  BEG   CHG  ***  05/29/98  *********************************
     C**********           PARM 0         ##RRN2  90
     C                   PARM      FNRRN         ##RRN2            9 0
     C***  END   CHG  ***  05/29/98  *********************************
     C                   PARM      'W'           ##QTE             1
     C                   PARM      2             ##VIEW            1 0
     C                   PARM      0             ##RRN3            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C     ##RRN         CHAIN     PEM02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C*** BEG CHG *** 08/10/07 ****************************
     C                   MOVE      *ON           *IN32
     C                   ELSE
     C                   MOVE      *ON           *IN41
     C*** END CHG *** 08/10/07 ****************************
     C                   ENDIF                                                  *IN60 IFEQ
     C*** BEG ADD *** 08/10/07 ****************************
     C                   ELSE
     C                   MOVE      *ON           *IN41
     C*** END ADD *** 08/10/07 ****************************
     C                   ENDIF                                                  *INKL IFEQ
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  HFLD=EMP/DIV
     C***  BEG   ADD  ***  10/24/07  *********************************
     C                   ENDIF                                                  HFLD=EMP/DIV
     C***  END   ADD  ***  10/24/07  *********************************
     C*** BEGIN ADD *** 04/02/07 **************************************
     C*** BEG   ADD *** 10/24/07 **************************************
     C*
     C*  TABLE SEARCH - DARTS REMARK CODE
     C*
     C     HFLD          IFEQ      'SCDRMK'
     C*
     C*
     C*                    MOVE *ZERO     QQRRN
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        QQRTCD            1
     C     ##RMCD        IFNE      *BLANKS
     C                   MOVE      ##RMCD        SCDRMK
     C                   MOVE      *ON           *IN47
     C                   ELSE
     C                   MOVE      *ON           *IN62
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 10/24/07 **************************************
     C*
     C*  AGENT SEARCH
     C*
     C     HFLD          IFEQ      'SCAGT#'
     C*
     C                   CALL      'W0040R'                             99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      '2'           ##CALL            1
     C                   PARM      FNRRN         ##RRN
     C*
     C     ##RRN         CHAIN     WMAAP                              60
     C*
     C     ##RRN         IFEQ      *ZEROS
     C                   MOVE      *ON           *IN32
     C                   ELSE
     C                   Z-ADD     AAAGT#        SCAGT#
     C                   MOVE      *ON           *IN33
     C                   ENDIF                                                  ##RRN=*ZEROS
     C                   ENDIF
     C*** END   ADD *** 04/02/07 **************************************
     C*
     C     HFLD          IFNE      'SCEMP#'
     C     HFLD          ANDNE     'SCDIV'
     C*** BEGIN ADD *** 04/02/07 **************************************
     C     HFLD          ANDNE     'SCAGT#'
     C*** END   ADD *** 04/02/07 **************************************
     F*** BEG   ADD *** 10/24/07 **************************************
     C     HFLD          ANDNE     'SCDRMK'
     C*** END   ADD *** 10/24/07 **************************************
     C                   MOVE      *ON           *IN46
     C                   ENDIF                                                   HFLD <>FRGN
     C*
     C                   ENDIF                                                   HREC= SCR01
     C                   ENDIF                                                   *INKJ *ON
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
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C******     *INKM     IFEQ '1'
     C     *INKM         IFEQ      '1'
     C     *IN69         ANDEQ     '0'
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*** BEG   ADD *** 10/24/07 **************************************
     C     XXCALL        IFNE      '*'
     C*** END   ADD *** 10/24/07 **************************************
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPFYR         SCFYR
     C*** BEG   ADD *** 10/24/07 **************************************
     C                   ENDIF
     C*** END   ADD *** 10/24/07 **************************************
     C*
      **** BEGIN DEL **** 12/23/97 *********************************************
     C******************** Z-ADDUPPRDT    SRDTI
     C******************** CALL 'SRCVTDAT'DTMDYY
     C******************** MOVELSRDTO     SCDTE
      **** END   DEL **** 12/23/97 *********************************************
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  GET THE FUND RECORD
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   END
     C                   GOTO      STRT01
     C*
     C                   END                                                    11=valid key
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  VALIDATE THE FUND YEAR FOR THE FUND
     C*          F1KEY ----------------- CO#, FND, FYR
     C*
     C                   MOVE      SCFYR         XXFYR             3 0
     C     XXFYR         IFNE      *ZERO
     C     F1KEY         CHAIN     PF101                              25
     C     *IN25         CABEQ     '1'           STRT
     C                   END
     C*
     C*  VALIDATE DATE
     C*
     C                   Z-ADD     SCDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTE
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*  VALIDATE EMPLOYER IF ENTERED
     C*
     C*** BEG   ADD *** 10/24/07 **************************************
     C     *IN69         IFNE      '1'
     C***  END   ADD  ***  10/24/07  *********************************
     C     SCEMP#        IFNE      *ZERO
     C*
     C***  BEGIN CHG  ***  05/29/98  *********************************
     C********** SCDIV     IFNE *ZEROS
     C     SCDIV         IFNE      *BLANKS
     C*
     C*  BE TO LOAD THE CORRECT VALUE IF FIELD EXIT WAS NOT PRESSSED
     C*
     C     XXDIV5        DOWEQ     *BLANK
     C                   MOVE      XXDIV4        XXDIV5
     C                   MOVE      XXDIV3        XXDIV4
     C                   MOVE      XXDIV2        XXDIV3
     C                   MOVE      XXDIV1        XXDIV2
     C                   CLEAR                   XXDIV1
     C                   ENDDO                                                  XXDIV5=*BLANK
     C***  END   CHG  ***  05/29/98  *********************************
     C*
     C                   MOVE      SCDIV         XXDIV
     C*
     C     EMKEY         CHAIN     PEM01                              26
     C*
     C                   ELSE
     C     EMKY2         CHAIN     PEM01                              26
     C                   ENDIF                                                  SCDIV<>*BLANKS
     C*
     C     *IN26         CABEQ     '1'           STRT
     C                   END
     C*
     C*
     C     SCEMP#        IFEQ      *ZEROS
     C***  BEGIN CHG  ***  05/29/98  *********************************
     C********** SCDIV     ANDNE*ZEROS
     C     SCDIV         ANDNE     *BLANKS
     C***  END   CHG  ***  05/29/98  *********************************
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   ENDIF                                                  SCEMP#=*ZEROS
     C*
     C** CHECK FOR DIV=*BLANK FIRST TIME THRU AND SEND MESSAGE
     C*
     C     SCEMP#        IFNE      *ZEROS
     C*
     C     SCDIV         IFNE      XZDIV
     C     SCEMP#        ORNE      XZEMP#
     C                   MOVE      *OFF          *IN37
     C***  BEGIN CHG  ***  05/29/98  *********************************
     C**********           MOVE SCDIV     XZDIV   50
     C                   MOVE      SCDIV         XZDIV
     C***  END   CHG  ***  05/29/98  *********************************
     C                   MOVE      SCEMP#        XZEMP#
     C                   ENDIF                                                  SCDIV<>XZDIV
     C*
     C***  BEGIN CHG  ***  05/29/98  *********************************
     C**N37***** SCDIV     IFEQ *ZERO
     C  N37SCDIV         IFEQ      *BLANKS
     C***  END   CHG  ***  05/29/98  *********************************
     C                   MOVE      *ON           *IN36
     C                   MOVE      *ON           *IN37
     C                   MOVE      SCDIV         XZDIV
     C                   MOVE      SCEMP#        XZEMP#
     C                   GOTO      STRT
     C                   ENDIF                                                  SCDIV=*BLANKS
     C*
     C***  BEGIN ADD  ***  07/08/98  *********************************
     C     SCFYR         IFNE      *ZEROS
     C*
     C     SCDIV         IFNE      *BLANKS
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         CABEQ     *ON           STRT                     38
     C                   ELSE
     C     ELKY1         CHAIN     PEL01                              60
     C     *IN60         CABEQ     *ON           STRT                     39
     C                   ENDIF                                                  SCDIV<>*BLANKS
     C*
     C                   ENDIF                                                  SCFYR<>*ZEROS
     C***  END   ADD  ***  07/08/98  *********************************
     C*
     C                   ENDIF                                                  SCEMP#<>*ZEROS
     C*** BEG   ADD *** 10/24/07 **************************************
     C                   ENDIF
     C*** END   ADD *** 10/24/07 **************************************
     C*  VALIDATE AGENT # IF ENTERED
     C*
     C     SCAGT#        IFNE      *ZERO
     C     AGKEY         CHAIN     PAA01                              28
     C     *IN28         CABEQ     '1'           STRT
     C                   END
     C*
     C*     VALIDATE AGENT SORT SELECTION
     C*
     C     SCSAGT        IFNE      '1'
     C     SCSAGT        ANDNE     '2'
     C     SCSAGT        ANDNE     ' '
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE SORT CRITERIA
     C*
     C     SCAGT#        IFNE      *ZERO
     C     SCSAGT        ANDEQ     '1'
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE SELECTION CRITERIA
     C*
     C     SCSELO        IFNE      'Y'
     C     SCSELO        ANDNE     'N'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE SELECTION CRITERIA
     C*
     C     SCSELC        IFNE      'Y'
     C     SCSELC        ANDNE     'N'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE NO CLAIM EMPLOYER FLAG
     C*
     C     SCEMPC        IFNE      'Y'
     C     SCEMPC        ANDNE     'N'
     C                   MOVE      '1'           *IN35
     C                   GOTO      STRT
     C                   END
     C*
     C***  BEGIN ADD  ***  09/20/06  *********************************
     C*** BEG DEL ***** 11/18/08 *************************************
     C****FOR*CLIENT 71 ONLY
     C********** CLTID     IFEQ '071'
     C*** END DEL ***** 11/18/08 *************************************
     C*
     C*  VALIDATE NO CLAIM EMPLOYER FLAG
     C*
     C****** beg chg ******** 11/22/13 *********************************
     C******     SCLTFL        IFNE      'L'
     C******     SCLTFL        ANDNE     'M'
     C******     SCLTFL        ANDNE     'B'
     C*** BEG CHG *** 08/10/07 ****************************
     C***********          MOVE *ON       *IN32
     C******             MOVE      *ON           *IN72
     C
      /free
       msExCt = ' ';
       //****** beg chg ******* 02/14/14 ******************************
       // If %scan('N':scExCt) <> 0 and (%scan('L':scExCt) <> 0  or
       //  %scan('M':scExCt) <> 0 and %scan('R':scExCt) <> 0);
       If %scan('N':scExCt) <> 0 and (%scan('L':scExCt) <> 0  or
        %scan('M':scExCt) <> 0 or %scan('R':scExCt) <> 0);
       //****** end chg ******* 02/14/14 ******************************
          msExCt = 'Blanks cannot be selected as multiple options.';
       Endif;

       If msExCt = ' ' and %scan('L':scExCt) <> 0 and %scan('M':scExCt) <> 0
        and %scan('R':scExCt) <> 0;
          msExCt = 'No claim type records will be displayed on report.';
       Endif;

       If msExCt <> ' ';
          *in72 = *on;
      /end-free
     C****** end chg ******** 11/22/13 *********************************
     C*** END CHG *** 08/10/07 ****************************
     C                   GOTO      STRT
     C                   ENDIF                                                  SCLTFL<>L/M/B
     C*
     C*  TOTAL INCURRED CANNOT BE LESS THAN *ZERO
     C*
     C     SCTINC        IFLT      *ZERO
     C                   MOVE      *ON           *IN33
     C                   GOTO      STRT
     C                   ENDIF                                                  SCTINC<*ZERO
     C*
     C*  TOTAL INCURRED AND GT/LT ARE MUTUALLY INCLUSIVE
     C*
     C     SCTINC        IFEQ      *ZERO
     C     SCGTLT        ANDNE     *BLANKS
     C     SCTINC        ORNE      *ZERO
     C     SCGTLT        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN40
     C                   GOTO      STRT
     C                   ENDIF                                                  SCTINC=*ZERO
     C*
     C*  VALIDATE GREATER THAN/LESS THAN FLAG
     C*
     C     SCGTLT        IFNE      'L'
     C     SCGTLT        ANDNE     'G'
     C     SCGTLT        ANDNE     *BLANK
     C                   MOVE      *ON           *IN34
     C                   GOTO      STRT
     C                   ENDIF                                                  SCGTLT<>L/G
     C*
     C*** BEG DEL ***** 11/18/08 *************************************
     C***********          ENDIF                             CLTID = 071
     C*** END DEL ***** 11/18/08 *************************************
     C***  END   ADD  ***  09/20/06  *********************************
     C*
     C*** BEG   ADD *** 10/24/07 **************************************
     C*
     C*  VALIDATE THE DARTS REMARK CODE
     C*
     C     *IN69         IFEQ      *ON
     C     SCDRMK        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN48
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN29 = *ON
     C                   ENDIF                                                  *IN29 = *ON
     C*
     C     SCDRMK        IFNE      *BLANKS
     C     SCDRMK        CHAIN     PCT01                              48
     C     *IN48         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN28 = *ON
     C                   ENDIF                                                  SCDRMK <> *BLANKS
     C*
     C*** END   ADD *** 10/24/07 **************************************
     C*  VALIDATE SEQUENCE CRITERIA
     C*
     C***  BEGIN ADD  ***  09/20/06  *********************************
     C*** BEG DEL ***** 11/18/08 *************************************
     C*****First allow broader set of values for CLT 071
     C********** CLTID     IFEQ '071'
     C*** END DEL ***** 11/18/08 *************************************
     C     SCSEQ         IFNE      'C'
     C     SCSEQ         ANDNE     'A'
     C     SCSEQ         ANDNE     'S'
     C     SCSEQ         ANDNE     'J'
     C     SCSEQ         ANDNE     '1'
     C     SCSEQ         ANDNE     '2'
     C     SCSEQ         ANDNE     'N'
     C     SCSEQ         ANDNE     'D'
     C     SCSEQ         ANDNE     'Z'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   ENDIF                                                  e SEQ<>C ...
     C*
     C*** BEG DEL ***** 11/18/08 *************************************
     C***********          ELSE                            X ID=071
     C*****END***ADD  ***  09/20/06  *********************************
     C***********
     C*****Then*allow narrow set of values for base clients.
     C***********SCSEQ     IFNE 'C'
     C***********SCSEQ     ANDNE'A'
     C***********SCSEQ     ANDNE'S'
     C***********SCSEQ     ANDNE'J'
     C***********SCSEQ     ANDNE'1'
     C***********SCSEQ     ANDNE'2'
     C***********SCSEQ     ANDNE'N'
     C***********          MOVE '1'       *IN24
     C***********          GOTO STRT
     C***********          END
     C***********
     C*****BEGIN*ADD  ***  09/20/06  *********************************
     C***********          ENDIF                           e ID=071
     C***  END   ADD  ***  09/20/06  *********************************
     C*** END DEL ***** 11/18/08 *************************************
     C*
     C**** BEGIN ADD ***** 06/21/05  *********************************
     C*  VALIDATE PRINT SS FLAG
      ***** BEG DEL ***** 09/28/06 ***********************************
     C*********  SCPRSS    IFEQ ' '
     C*********            MOVE 'N'       SCPRSS
     C*********            ENDIF                           e.PRSS = ' '
      ***** END DEL ***** 09/28/06 ***********************************
      ***** BEG DEL ******* 03/01/07 **********************************
     C*********  SCPRSS    IFNE 'Y'
      ******BEG CHG ***** 09/28/06 ***********************************
     C*********  SCPRSS    ANDNE'N'
     C*********  SCPRSS    ANDNE*BLANKS
      ******END CHG ***** 09/28/06 ***********************************
     C*********            MOVE '1'       *IN31
     C*********            GOTO STRT
     C*********            END
     C**** END ADD ******* 06/21/05  *********************************
      ***** END DEL ******* 03/01/07 **********************************
      ***** BEG ADD ******* 03/01/07 **********************************
      *
     C     CLTID         IFEQ      '132'
     C                   MOVE      '4'           SCPRSS
      *
     C                   ELSE
      *
     C     SCPRSS        IFNE      'Y'
     C     SCPRSS        ANDNE     'N'
     C     SCPRSS        ANDNE     '4'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   ENDIF                                                  SCPRSS
      *
     C                   ENDIF                                                  CLT132
      *
      ***** END ADD ******* 03/01/07 **********************************
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C     SCSAGT        IFEQ      '1'
     C                   MOVE      'Y'           QQSAGT            1
     C                   MOVE      'N'           QQSRGN            1
     C                   ELSE
     C     SCSAGT        IFEQ      '2'
     C                   MOVE      'N'           QQSAGT
     C                   MOVE      'Y'           QQSRGN
     C                   ELSE
     C     SCSAGT        IFEQ      ' '
     C                   MOVE      'N'           QQSAGT
     C                   MOVE      'N'           QQSRGN
     C                   END
     C                   END
     C                   END
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCDTE         QQDTE             8
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCSELO        QQSELO            1
     C                   MOVE      SCSELC        QQSELC            1
     C                   MOVE      SCMNTH        QQMNTH            3
     C                   MOVE      SCSEQ         QQSORT            1
     C                   MOVE      SCEMP#        QQEMP#            9
     C                   MOVE      SCDIV         QQDIV             5
     C                   MOVE      SCAGT#        QQAGT#            5
     C                   MOVE      SCEMPC        QQEMPC            1
     C**** BEGIN ADD ***** 06/21/05  *********************************
     C                   MOVE      SCPRSS        QQPRSS            1
     C**** END ADD ******* 06/21/05  *********************************
     C***  BEGIN ADD  ***  09/20/06  *********************************
     C                   MOVE      *BLANKS       QQTINC
     C*** BEG CHG ***** 11/18/08 *************************************
     C*   FOR CLIENT 71 ONLY
     C*********  CLTID     IFEQ '071'
     C*********            MOVE SCTINC    QQTINC
     C*********            ENDIF                             CLTID = 071
     C                   MOVE      SCTINC        QQTINC
     C*** END CHG ***** 11/18/08 *************************************
     C***  END   ADD  ***  09/20/06  *********************************
     C*** BEG   ADD ****** 10/24/07 **************************************
     C     *IN69         IFEQ      '1'
     C     *DTAARA       DEFINE    *LDA          LDA
     C                   IN        LDA
     C                   MOVE      QQCO#         DDCO#
     C                   MOVE      QQFND         DDFND
     C                   MOVE      QQDTE         DDDTE
     C                   MOVE      QQFYR         DDFYR
     C                   MOVE      QQSELO        DDSELO
     C                   MOVE      QQSELC        DDSELC
     C                   MOVE      QQMNTH        DDMNTH
     C                   MOVE      QQSORT        DDSORT
     C                   MOVE      UPOUTQ        DDOUTQ
     C                   MOVE      '*'           DDMODE
     C                   MOVE      QQEMP#        DDEMP#
     C                   MOVE      QQDIV         DDDIV
     C                   MOVE      QQSRGN        DDSRGN
     C                   MOVE      QQAGT#        DDAGT#
     C                   MOVE      QQSAGT        DDSAGT
     C                   MOVE      QQEMPC        DDEMPC
     C                   MOVE      QQPRSS        DDPRSS
     C                   MOVE      QQLTFL        DDLTFL            1
     C                   MOVE      QQTINC        DDTINC           13
     C                   MOVE      QQGTLT        DDGTLT            1
     C                   MOVE      SCDRMK        DDRMK             5
     C                   OUT       LDA
     C                   ELSE
     C*** END   ADD ****** 10/24/07 **************************************
     C****** beg add ******** 11/22/13 *************************************
      /free
       Select;
       When %scan(',':scExCt) = 0 and (%scan('N':scExCt) <> 0 or
        %scan('L':scExCt) <> 0 or %scan('M':scExCt) <> 0 or
        %scan('R':scExCt) <> 0);
          qqLtFl = scExCt;

       When %scan('L':scExCt) <> 0 and %scan('M':scExCt) <> 0;
          qqLtFl = 'r';

       When %scan('L':scExCt) <> 0 and %scan('R':scExCt) <> 0;
          qqLtFl = 'm';

       When %scan('M':scExCt) <> 0 and %scan('R':scExCt) <> 0;
          qqLtFl = 'l';
       Endsl;
      /end-free
     C****** end add ******** 11/22/13 *************************************
     C                   CALL      'W7190C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQDTE
     C                   PARM                    QQFYR
     C                   PARM                    QQSELO
     C                   PARM                    QQSELC
     C                   PARM                    QQMNTH
     C                   PARM                    QQSORT
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           MODE              1
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQSRGN
     C                   PARM                    QQAGT#
     C                   PARM                    QQSAGT
     C                   PARM                    QQEMPC
     C**** BEGIN ADD ***** 06/21/05  *********************************
     C                   PARM                    QQPRSS
     C**** END ADD ******* 06/21/05  *********************************
     C***  BEGIN ADD  ***  09/20/06  *********************************
     C****** beg chg ******** 11/22/13 *************************************
     C******             PARM      SCLTFL        QQLTFL            1
     C                   PARM                    QQLTFL
     C****** end chg ******** 11/22/13 *************************************
     C                   PARM                    QQTINC           13
     C                   PARM      SCGTLT        QQGTLT            1
     C***  END   ADD  ***  09/20/06  *********************************
     C*** BEG   ADD ****** 10/24/07 **************************************
     C                   PARM      SCDRMK        QQRMK             5
     C*** END   ADD ****** 10/24/07 **************************************
     C*
     C                   ENDIF
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C*
     C***  Begin Add  ***  10/24/07  **************************************
     C                   SELECT
     C     XXCALL        WHENEQ    '*'
     C                   RETURN
     C                   OTHER
     C***  End   Add  ***  10/24/07  **************************************
     C                   MOVE      '1'           *INLR
     C***  Begin Add  ***  10/24/07  **************************************
     C                   ENDSL
     C***  End   Add  ***  10/24/07  **************************************
     C**
     C*==================================================================*
     C*   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
     C*==================================================================*
     C*
     CSR   SR1ST         BEGSR
     C*
     C                   TIME                    DSDTTM
     C*
     C***  BEGIN ADD  ***  09/20/06  *********************************
     C***ESTABLISH CLIENT NUMBER
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*** BEG DEL ***** 11/18/08 *************************************
     C********** CLTID     IFEQ '071'
     C**********           MOVE *ON       *IN91
     C**********           ELSE
     C**********           MOVE *OFF      *IN91
     C**********           ENDIF
     C*** END DEL ***** 11/18/08 *************************************
     C***  END   ADD  ***  09/20/06  *********************************
      *
      ***** BEG ADD ******* 03/01/07 **********************************
     C     CLTID         COMP      '132'                                  71
      ***** END ADD ******* 03/01/07 **********************************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*** BEG   ADD *** 10/24/07 **************************************
     C     XXCALL        IFNE      '*'
     C*** END   ADD *** 10/24/07 **************************************
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPFYR         SCFYR
     C*** BEG   ADD *** 10/24/07 **************************************
     C                   ENDIF
     C*** END   ADD *** 10/24/07 **************************************
     C*
      **** BEGIN CHG **** 12/23/97 *********************************************
     C******************** Z-ADDUPPRDT    SRDTI
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C******************** CALL 'SRCVTDAT'DTMDYY
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTE
      **** END   CHG **** 12/23/97 *********************************************
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  GET THE FUND RECORD
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C*
     C***  BEGIN CHG  ***  05/29/98  *********************************
     C**********           MOVE Q1USER    SCUSER
     C**********           MOVE QQPRDS    SCPRDS
     C*** BEG   ADD *** 10/24/07 **************************************
     C     XXCALL        IFNE      '*'
     C*** END   ADD *** 10/24/07 **************************************
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   ELSE
     C*** BEG   ADD *** 10/24/07 **************************************
     C     OPTNM         CAT(P)    OPTNM1:0      ##DATA
     C                   ENDIF                                                  Q1PRMS>=1
     C*** END   ADD *** 10/24/07 **************************************
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C***  END   CHG  ***  05/29/98  *********************************
     C*
     C* * * INITIALIZE FIELDS FOR OUTPUT TO SCREEN
     C*
     C                   MOVE      UPCO#         XXCO#             3 0
     C*
     C                   MOVE      '1'           *IN11                          1ST TIME INDIC.
     C*** Beg Add **** 11/01/13 *** SEQ84035 **************************
     C***Initialize checkboxes to blanks by placing 'N' in program fields
     C                   MOVE      'N'           SCSELO                         Open Claims
     C                   MOVE      'N'           SCSELC                         Closed claims
     C                   MOVE      'N'           SCEMPC                         Emp no claims
     C*** End Add **** 11/01/13 *** SEQ84035 **************************
     C*
     C                   ENDSR
     C*
