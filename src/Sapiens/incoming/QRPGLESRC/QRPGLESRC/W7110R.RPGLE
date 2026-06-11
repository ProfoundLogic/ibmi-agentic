     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7110R - DAILY CHECK EDIT BY USER
     H*****************************************************************
     H*  INDICATORS:                                                  *
     H*                                                               *
     H*    30 - DISPLAY RELEASED PAYMENTS SUB-HEADING                 *
     H*    31 - DISPLAY DELETED  PAYMENTS SUB-HEADING                 *
     H*    32 - DISPLAY PENDED   PAYMENTS SUB-HEADING                 *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H*  12/16/11 C:062  PROJECT: WC099 78745                         *
     H*                  GET REQUESTOR TO PRINT FOR EACH STATUS       *
     H*                                                               *
     H*  02/10/10 C:246  PROJECT: C133WIJ                             *
     H*                  ADD JURISDICTION STATE IN QSYSPRT            *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  02/20/09 C:191  SUPPORT: WA045 / 67243                       *
     H*                  REQUESTOR NAME NOT PRINTING AT LCTA          *
     H*                                                               *
     H*  01/30/09 C:53   SUPPORT: 66840
     H*                  MOVE CODE TO CORRECT DETAIL FROM BEING SKIPPED
     H*
     H*  08/13/07 C:53   PROJECT: SY157
     H*                  CORRECT READ ISSUE
     H*                                                               *
     H* 05/25/07 C:191 PROJECT: WC1515                                *
     H*                 Check claims authority                        *
     H*                 for no-auth, do not include claimant name     *
     H*                                                               *
     H*  06/20/07 C:53 114WMF  BACK TO BASE-114WGF                    *
     H*  12/19/03  C:53  PROJECT: 114WGF                              *
     H*                  CHANGE SORT FROM USER/GROUP TO GROUP/USER    *
     H*                                                               *
     H*  06/11/98  C:33  SEQ#: WC033/12145                            *
     H*                  USE RPRQST FOR REQUESTOR INSTEAD OF RPUSER   *
     H*                                                               *
     H*  06/10/98  C:33  SEQ#: WC033/12144                            *
     H*                  BE SURE ROUTING PAGE 'TITLE' AND HEADING     *
     H*                  'TITLE' MATCH                                *
     H*                                                               *
     H*  06/09/98  C:58  PROJECT# WC577                               *
     H*                  Added newpage heading to status break.       *
     H*                                                               *
     H*  01/09/98  C:80  PROJECT# WC420                               *
     H*                  MODIFIED PROGRAM TO PROCESS 8 DIGIT DATE     *
     H*                  FORMAT, 9 DIGIT EMPLOYER AND 5 DIGIT         *
     H*                  DIVISION.                                    *
     H*****************************************************************
     F***  BEGIN CHG  ***  06/11/98  *********************************
     F**********WARPL7  IF  E           K        DISK
     FWARPLG    IF   E           K DISK
     F***  END   CHG  ***  06/11/98  *********************************
     F*** BEG ADD *** 12/19/03 *********************************
     FWARPLJ    IF   E           K DISK
     F                                     RENAME(PRP01:PRP02)
     F*** END ADD *** 12/19/03 *********************************
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMPCL     IF   E           K DISK
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     F                                     USROPN
     F*

     D***  BEGIN CHG  ***  06/10/98  *********************************
     D**********          TITL    1   1 60
     D GTITL           C                   CONST('GROUP CHECK EDIT')
     D***  END   CHG  ***  06/10/98  *********************************
     D RELSTL          C                   CONST('********** RELEASED -
     D                                     PAYMENTS **********')
     D*
     D DELTTL          C                   CONST('********** DELETED P-
     D                                     AYMENTS ***********')
     D*
     D PENDTL          C                   CONST('********** PENDED PA-
     D                                     YMENTS ************')
     D*
     D*** BEG ADD *** 12/19/03 *********************************
     D*
     D NF              C                   CONST('NOT FOUND')
     D*** END ADD *** 12/19/03 *********************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** BEG ADD ****** 05/25/07 **********************************
     D XXATTX          C                   CONST('*** NO AUTHORITY ***')
      *** END ADD ****** 05/25/07 **********************************

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C***  BEGIN CHG  ***  06/11/98  *********************************
     C**********           KFLD           RPUSER
     C                   KFLD                    RPRQST
     C***  END   CHG  ***  06/11/98  *********************************
     C*                                                    MLS 2/26/88
     C     UPKEY1        KLIST                                                       "
     C                   KFLD                    Q1USER                              "
     C*
     C*   KEY LIST FOR THE CLAIMS FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C*
     C*   KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*  REQUEST PAYMENT FILE
     C*
     C     RPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    QQUSER
     C*
     C*  REQUEST PAYMENT FILE
     C*
     C     RPKY1         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  COMP PAYMENT CONTROL FILE KEY LIST
     C*
     C     PCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    RPFND
     C*
     C/EJECT
     C*
     C     *LIKE         DEFINE    RPTRDT        XXDATE
     C     *LIKE         DEFINE    RPTRDT        XXTRDT
     C***  BEGIN ADD  ***  06/11/98  *********************************
     C     *LIKE         DEFINE    RPRQST        XHRQST
     C***  END   ADD  ***  06/11/98  *********************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C*** BEGIN CHG *** 01/09/98 **************************************
     C******************** PARM           QQDATE  6
     C                   PARM                    QQDATE            8
     C*** END   CHG *** 01/09/98 **************************************
     C                   PARM                    QQUSER           10
     C                   PARM                    QQQUE            10            MLS 2/26/88
     C                   PARM                    QQCKTP            1
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
      *** BEG ADD *** 12/19/03 **************************************
     C*
     C* Check data client data area
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       CLTID             3
     C     CLTID         IFEQ      '114'
     C                   MOVE      *ON           *IN05                          114 OUTPUT
     C                   ELSE
     C                   MOVE      *OFF          *IN05
     C                   ENDIF
      *** END ADD *** 12/19/03 **************************************
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
     C*** END ADD ****** 05/25/07 ********************************************
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C*** BEGIN CHG *** 01/09/98 **************************************
     C******************** MOVE QQDATE    X1DATE  60
     C                   MOVE      QQDATE        X1DATE            8 0
     C*** END   CHG *** 01/09/98 **************************************
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C     QQQUE         IFNE      ' '                                          *MLS 2/26/88
     C                   SETON                                        09        *DEMAND MODE
     C                   ELSE                                                   *
     C                   SETON                                        08        *BATCH MODE
     C                   END                                                    *
     C*
     C*  GET THE COMPANY RECORD.
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C* CENTER TEXT IN FIELD CONAME
     C*
     C                   MOVEL     CONAME        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XX1NAM           40
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C                   TIME                    DSDTTM
     C*
     C*** BEGIN ADD *** 01/09/98 **************************************
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXSYDT            8 0
     C*** END   ADD *** 01/09/98 **************************************
     C*
     C* * * CHECK TO SEE IF PROCESSING ALL USERS OR JUST ONE.
     C     QQUSER        COMP      *BLANK                                 15    ON=ALL USERS
     C*
     C/EJECT
     C*** BEG ADD *** 12/19/03 *********************************
     C     CLTID         IFEQ      '114'
     C     RPKY1         CHAIN     PRP02                              40
     C                   ELSE
     C*** END ADD *** 12/19/03 *********************************
     C  N15RPKEY         CHAIN     PRP01                              40        INDIV. USER
     C   15RPKY1         CHAIN     PRP01                              40        ALL USERS
     C*** BEG ADD *** 12/19/03 *********************************
     C                   ENDIF
     C*** END ADD *** 12/19/03 *********************************
     C*
     C     *IN40         DOWEQ     '0'
     C*
     C*  THE RECORD BEING PROCESSED MUST BE THE SAME TYPE AS THE
     C*  SELECTED REPORT TYPE (GROUP OR DAILY).
     C*
     C     RPPGTP        IFNE      QQCKTP
     C                   GOTO      SKIP
     C                   END
     C*** BEG ADD *** 12/19/03 *********************************
     C*
     C     CLTID         IFEQ      '114'
     C  N15RPRQST        CABNE     QQUSER        SKIP
     C                   ENDIF
     C*
     C*** END ADD *** 12/19/03 *********************************
     C*   LOOK AT THE COMP PMT CONTROL FILE TO GET THE # OF DAYS IN
     C*   ADVANCE FUTURE REQUESTS SHOULD APPEAR ON THIS REPORT.
     C*
     C     PCKY          CHAIN     PPC01                              60
     C*
     C                   Z-ADD     RPTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1TRDT            8 0
     C*
     C     *IN60         IFEQ      '0'
     C     RPSTAT        ANDEQ     'P'
     C                   Z-ADD     X1TRDT        SRDATE
     C                   Z-ADD     PCEDTD        SRDAYS
     C                   MOVE      *BLANK        SRERR
     C                   MOVE      *BLANK        SRTERM
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDATE            8 0
     C                   PARM                    SRDAYS            5 0
     C                   PARM                    SRERR             1
     C                   PARM                    SRTERM            1
     C                   MOVE      SRDATE        X1TRDT
     C                   END
     C*
     C                   Z-ADD     X1TRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTRDT
     C*
     C*** BEG DEL *** 01/30/09 *********************************
     C****BEG*ADD *** 12/19/03 *********************************
     C********** CLTID     IFEQ '114'
     C********** XXTRDT    IFLE XXDATE
     C********** RPSTAT    OREQ 'D'
     C**********
     C**N11*****           EXSR SR1ST
     C**********
     C********** RPFND     CASNEXHFND     SRFBRK
     C**********           ENDCS
     C********** RPRQST    IFEQ XHRQST
     C********** RPSTAT    CASNEXHSTAT    SRSBRK
     C**********           ENDCS
     C**********
     C**********           ELSE
     C********** RPRQST    CASNEXHRQST    SRUBRK
     C**********           ENDCS
     C**********           ENDIF
     C**********           ENDIF
     C**********
     C**********           ELSE
     C****END*ADD *** 12/19/03 *********************************
     C*** END DEL *** 01/30/09 *********************************
     C     XXTRDT        IFLE      XXDATE
     C     RPSTAT        OREQ      'D'
     C*** BEG ADD *** 01/30/09 *********************************
     C     CLTID         IFEQ      '114'
     C********** XXTRDT    IFLE XXDATE
     C********** RPSTAT    OREQ 'D'
     C*
     C  N11              EXSR      SR1ST
     C*
     C     RPFND         CASNE     XHFND         SRFBRK
     C                   ENDCS
     C     RPRQST        IFEQ      XHRQST
     C     RPSTAT        CASNE     XHSTAT        SRSBRK
     C                   ENDCS
     C*
     C                   ELSE
     C     RPRQST        CASNE     XHRQST        SRUBRK
     C                   ENDCS
     C                   ENDIF
     C***********          ENDIF
     C*
     C                   ELSE
     C*** END ADD *** 01/30/09 *********************************
     C*
     C  N11              EXSR      SR1ST
     C*
     C***  BEGIN CHG  ***  06/11/98  *********************************
     C********** RPUSER    CASNEXHUSER    SRUBRK
     C     RPRQST        CASNE     XHRQST        SRUBRK
     C***  END   CHG  ***  06/11/98  *********************************
     C                   END
     C     RPSTAT        CASNE     XHSTAT        SRSBRK
     C                   END
     C     RPFND         CASNE     XHFND         SRFBRK
     C                   END
     C*** BEG ADD *** 12/19/03 *********************************
     C                   ENDIF
     C*** END ADD *** 12/19/03 *********************************
     C*
     C* * *  DETAIL ROUTINE
     C*
     C     RPSTAT        IFEQ      ' '
     C                   ADD       1             XXCHKS            5 0
     C                   ADD       RPAMT         XXCAMT            9 2
     C                   ELSE
     C     RPSTAT        IFEQ      'D'
     C                   ADD       1             XXDELS            5 0
     C                   ADD       RPAMT         XXDAMT            9 2
     C                   END
     C     RPSTAT        IFEQ      'P'
     C                   ADD       1             XXPEND            5 0
     C                   ADD       RPAMT         XXPAMT            9 2
     C                   END
     C                   END
     C*
     C                   ADD       1             XXFCHK            5 0
     C                   ADD       RPAMT         XXFAMT            9 2                 ¦
     C                   MOVE      RPFYR         XXYR              2 0
     C*
     C*** BEGIN CHG *** 01/09/98 **************************************
     C******************** Z-ADDRPFRDT    SRDTI
     C******************** CALL 'SRCVTDAT'DTMDY
     C******************** MOVELSRDTO     XXFRDT  60
     C********************
     C******************** Z-ADDRPTODT    SRDTI
     C******************** CALL 'SRCVTDAT'DTMDY
     C******************** MOVELSRDTO     XXTODT  60
     C*
     C                   Z-ADD     RPFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRDT            8 0
     C*
     C                   Z-ADD     RPTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTODT            8 0
     C*** END   CHG *** 01/09/98 **************************************
     C*
     C                   MOVEL     *BLANK        XXLSN            15
     C                   MOVEL     *ZERO         CMEMP#
     C                   MOVEL     *ZERO         CMDIV
     C     CMKEY         CHAIN     PCM01                              44
     C     *IN44         IFEQ      '0'
     C******BEG ADD  02/10/10******************************
     C                   MOVE      CMJRST        XXST              2
     C******END ADD  02/10/10******************************
     C     CDKEY         CHAIN     PCD01                              45
     C     *IN45         IFEQ      '0'
     C                   MOVE      *BLANKS       QQLSN
     C                   MOVE      *BLANKS       QQFSN
     C                   MOVEL     CDLSN         QQLSN
     C                   MOVEL     CDFSN         QQFSN
     C                   CALL      'SRNAME'
     C                   PARM                    QQLSN            56
     C                   PARM                    QQFSN            15
     C                   PARM                    QQTERM            1
     C                   MOVEL     QQLSN         XXLSN
     C                   END
      *
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *BLANKS       XXLSN
     C                   MOVEL(P)  XXATTX        XXLSN
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
      *
     C                   END
     C*
     C                   MOVEL     RPPNAM        XXNAME           15
     C*
     C*** BEGIN CHG *** 01/09/98 **************************************
     C******************** Z-ADDRPTRDT    SRDTI
     C******************** CALL 'SRCVTDAT'DTMDY
     C******************** MOVELSRDTO     X2TRDT  60
     C                   Z-ADD     RPTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X2TRDT            8 0
     C*** END   CHG *** 01/09/98 **************************************
     C*
     C* * * CHECK TO SEE IF OVERFLOW OCCURED ON LAST OUTPUT
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXSR      SRSBHD
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C* * * PRINT DETAIL LINES
     C                   EXCEPT    DETL
     C*
     C                   END
     C     SKIP          TAG
     C*
     C*** BEG ADD *** 12/19/03 *********************************
     C     CLTID         IFEQ      '114'
     C*** BEG CHG *** 08/13/07 *********************************
     C*********  RPKY1     CHAINPRP02                40
     C     RPKY1         READE     PRP02                                  40
     C*** END CHG *** 08/13/07 *********************************
     C                   ELSE
     C*** END ADD *** 12/19/03 *********************************
     C  N15RPKEY         READE     PRP01                                  40
     C   15RPKY1         READE     PRP01                                  40
     C*** BEG ADD *** 12/19/03 *********************************
     C                   ENDIF
     C*** END ADD *** 12/19/03 *********************************
     C*
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C*** BEG ADD *** 12/19/03 *********************************
     C     CLTID         IFEQ      '114'
     C   11              EXSR      SRFBRK
     C                   ELSE
     C*** END ADD *** 12/19/03 *********************************
     C   11              EXSR      SRUBRK
     C*** BEG ADD *** 12/19/03 *********************************
     C                   ENDIF
     C*** END ADD *** 12/19/03 *********************************
     C*
     C/EJECT
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       If %open(qsysprt);
       xxPrt = '0';
       Chain orKey POR01;
          If %found;
             Except Route;
          Endif;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*==================================================================*
     C*   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
     C*==================================================================*
     C*
     CSR   SR1ST         BEGSR
     C*
     C* * * INITIALIZE LEVEL BREAK HOLD FIELDS
     C***  BEGIN CHG  ***  06/11/98  *********************************
     C**********           MOVE RPUSER    XHUSER 10
     C                   MOVEL(P)  RPRQST        XHRQST
     C***  END   CHG  ***  06/11/98  *********************************
     C                   MOVE      RPSTAT        XHSTAT            1
     C                   MOVE      RPFND         XHFND             3 0
     C*
     C*
     C* * * GET USER NAME FROM USER PARM FILE
     C     UPKEY         CHAIN     PUP01                              43
     C     *IN43         IFEQ      '0'
     C                   MOVEL     UPUSNM        XUNAME           40
     C                   MOVEL     UPSTAF        XUSTAF            3
     C                   ELSE
     C                   MOVEL     *BLANKS       XUNAME
     C                   MOVEL     *BLANKS       XUSTAF            3
     C                   END
     C*
     C* * * OPEN PRINT FILE
     C                   OPEN      QSYSPRT
     C*
     C* * * PRINT 1ST PAGE HEADINGS
     C*                                                    MLS 2/26/88
     C*   CHAIN TO THE USER PARAMETER FILE                      "
     C*                                                         '
     C     UPKEY1        CHAIN     PUP01                              60             "
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C***  BEGIN ADD  ***  06/10/98  *********************************
     C     QQCKTP        IFEQ      'G'
     C                   MOVEL(P)  GTITL         ORTITL
     C                   ENDIF                                                  QQCKTP=G
     C***  END   ADD  ***  06/10/98  *********************************
     C***  BEGIN CHG  ***  06/10/98  *********************************
     C**********
     C**CENTER*TEXT IN FIELD ORTITL
     C**********
     C**********           MOVELORTITL    ##DATA
     C**********
     C**********           CALL 'SRCENTER'
     C**********           PARM           ##DATA 80
     C**********           PARM 60        ##LENG  30
     C**********
     C**********           MOVEL##DATA    XXTITL 60
     C*
     C*  Left Justify Title in Field
     C*
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C*
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C*
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF                                                  N<=60
     C*
     C                   ENDIF                                                  ORTITL<>*BLANKS
     C***  END   CHG  ***  06/10/98  *********************************
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C***  BEGIN DEL  ***  06/10/98  *********************************
     C********** QQCKTP    IFEQ 'G'
     C**********           MOVE TITL,1    ORTITL
     C**********           END
     C***  END   DEL  ***  06/10/98  *********************************
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C***  BEGIN CHG  ***  06/10/98  *********************************
     C*
     C* CENTER TEXT IN FIELD ORTITL
     C*
     C                   MOVEL     ORTITL        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XXTITL           60
     C***  END   CHG  ***  06/10/98  *********************************
     C                   END
     C*
     C                   EXCEPT    HEAD
     C                   EXSR      SRSBHD
     C*
     C                   MOVE      '1'           *IN11                          1ST TIME INDIC.
     C*
     C                   ENDSR
     C/EJECT
     C*==================================================================*
     C*  Level Break on User               ** Subroutine **  S R U B R K *
     C*==================================================================*
     C*
     CSR   SRUBRK        BEGSR
     C*
     C*** BEG ADD *** 12/19/03 *********************************
     C     CLTID         IFNE      '114'
     C*** END ADD *** 12/19/03 *********************************
     C                   EXSR      SRFBRK
     C*** BEG ADD *** 12/19/03 *********************************
     C                   ENDIF
     C*** END ADD *** 12/19/03 *********************************
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C                   EXCEPT    USER
     C*
     C*
     C     XHSTAT        IFEQ      ' '
     C                   EXCEPT    U1TOTL
     C                   END
     C*
     C* DELETED CHECK TOTALS
     C     XHSTAT        IFEQ      'D'
     C                   EXCEPT    U2TOTL
     C                   END
     C*
     C* PENDED CHECK TOTALS
     C     XHSTAT        IFEQ      'P'
     C                   EXCEPT    U5TOTL
     C                   END
     C*
     C  NLR              DO
     C*** BEG ADD *** 12/19/03 *********************************
     C     CLTID         IFNE      '114'
     C*** END ADD *** 12/19/03 *********************************
     C                   EXCEPT    HEAD                                         NEW PAGE
     C                   EXSR      SRSBHD
     C*** BEG ADD *** 12/19/03 *********************************
     C                   ENDIF
     C*** END ADD *** 12/19/03 *********************************
     C*
     C                   Z-ADD     0             XXCHKS            5 0
     C                   Z-ADD     0             XXCAMT            9 2
     C                   Z-ADD     0             XXDELS            5 0
     C                   Z-ADD     0             XXDAMT            9 2
     C                   Z-ADD     0             XXPEND            5 0
     C                   Z-ADD     0             XXPAMT            9 2
     C*
     C* * * GET USER NAME FROM USER PARM FILE
     C     UPKEY         CHAIN     PUP01                              43
     C*
     C     *IN43         IFEQ      '0'
     C                   MOVEL     UPUSNM        XUNAME
     C                   MOVEL     UPSTAF        XUSTAF
     C                   ELSE
     C                   MOVEL     *BLANKS       XUNAME
     C                   MOVEL     *BLANKS       XUSTAF
     C                   END
     C*
     C*** BEG ADD *** 12/19/03 *********************************
     C     CLTID         IFEQ      '114'
     C     *IN20         IFNE      *ON
     C                   EXCEPT    HEAD                                         NEW PAGE
     C                   EXSR      SRSBHD
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 12/19/03 *********************************
     C* * * MOVE USER NAME TO HOLD FIELD
     C***  BEGIN CHG  ***  06/11/98  *********************************
     C**********           MOVELRPUSER    XHUSER
     C                   MOVEL(P)  RPRQST        XHRQST
     C***  END   CHG  ***  06/11/98  *********************************
     C                   MOVE      RPSTAT        XHSTAT
     C                   END
     C*
     C                   ENDSR
     C/EJECT
     C*==================================================================*
     C*  Level Break on Status             ** Subroutine **  S R S B R K *
     C*==================================================================*
     C*
     CSR   SRSBRK        BEGSR
     C*
     C*** BEG ADD *** 12/19/03 *********************************
     C     CLTID         IFNE      '114'
     C*** END ADD *** 12/19/03 *********************************
     C                   EXSR      SRFBRK
     C*** BEG ADD *** 12/19/03 *********************************
     C                   ELSE
     C                   EXCEPT    USER
     C                   ENDIF
     C*** END ADD *** 12/19/03 *********************************
     C*** BEGIN ADD *** 12/16/11 **************************************
     C                   EXCEPT    USER
     C*** END   ADD *** 12/16/11 **************************************
     C*
     C     XHSTAT        IFEQ      ' '
     C                   EXCEPT    U1TOTL
     C                   END
     C*
     C* DELETED CHECK TOTALS
     C     XHSTAT        IFEQ      'D'
     C                   EXCEPT    U2TOTL
     C                   END
     C*
     C* PENDED CHECK TOTALS
     C     XHSTAT        IFEQ      'P'
     C                   EXCEPT    U5TOTL
     C                   END
     C*
     C                   MOVE      RPSTAT        XHSTAT
     C*
     C*** BEGIN ADD *** 06/09/98 **************************************
     C                   EXCEPT    HEAD
     C*** END ADD ***** 06/09/98 **************************************
     C                   EXSR      SRSBHD
     C*
     C                   ENDSR
     C*
     C*==================================================================*
     C*  Level Break on GROUP              ** Subroutine **  S R F B R K *
     C*==================================================================*
     C*
     CSR   SRFBRK        BEGSR
     C*** BEG ADD *** 12/19/03 *********************************
     C     CLTID         IFEQ      '114'
     C                   MOVE      *ON           *IN20
     C                   EXSR      SRUBRK
     C                   MOVE      *OFF          *IN20
     C                   ENDIF
     C*** END ADD *** 12/19/03 *********************************
     C*
     C     XXFAMT        IFGT      0
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C     XHSTAT        IFEQ      ' '
     C                   EXCEPT    U3TOTL
     C                   ELSE
     C     XHSTAT        IFEQ      'D'
     C                   EXCEPT    U4TOTL
     C                   ELSE
     C                   EXCEPT    U6TOTL
     C                   END                                                    XHSTAT NE 'D'
     C                   END                                                    XHSTAT EQ ' '
     C                   END                                                    XXFAMT GT 0
     C*
     C                   Z-ADD     0             XXFCHK
     C                   Z-ADD     0             XXFAMT
     C                   MOVE      RPFND         XHFND
     C*** BEG ADD *** 12/19/03 *********************************
     C     CLTID         IFEQ      '114'
     C     *INLR         IFNE      *ON
     C                   EXCEPT    HEAD                                         NEW PAGE
     C                   EXSR      SRSBHD
     C                   ENDIF
     C                   MOVE      RPSTAT        XHSTAT
     C                   ENDIF
     C*** END ADD *** 12/19/03 *********************************
     C*
     C                   ENDSR
     C*
     C*==================================================================*
     C*  DISPLAY A SUB-HEADING TO PRINT THE TYPE OF CHECK REQUEST
     C*==================================================================*
     C*
     C     SRSBHD        BEGSR
     C*
     C                   MOVEA     '000'         *IN(30)
     C*
     C     RPSTAT        IFEQ      ' '
     C                   MOVE      '1'           *IN30
     C                   ELSE
     C     RPSTAT        IFEQ      'D'
     C                   MOVE      '1'           *IN31
     C                   END
     C     RPSTAT        IFEQ      'P'
     C                   MOVE      '1'           *IN32
     C                   END
     C                   END
     C                   EXCEPT    SUBHD
     C*
     C                   ENDSR
     O*
     O/EJECT
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O***  BEGIN CHG  ***  01/28/10  *********************************
     O****    E   60           ROUTE
     O****                               23 '(C) COPYRIGHT PROGRESSI'
     O****                               46 'VE DATA SOLUTIONS. INC,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O*** BEGIN CHG *** 01/09/98 **************************************
     O************************ CONAME    85
     O                       XX1NAM              96
     O*** END   CHG *** 01/09/98 **************************************
     O*** BEGIN CHG *** 01/09/98 **************************************
     O************************ DSDATEY  132
     O                       XXSYDT             152 '  /  /    '
     O*** END   CHG *** 01/09/98 **************************************
     O          E            HEAD           2
     O                       Q1PGM               10
     O*** BEGIN CHG *** 01/09/98 **************************************
     O************************ ORTITL    96
     O                       XXTITL             106
     O*** END   CHG *** 01/09/98 **************************************
     O*** BEGIN CHG *** 01/09/98 **************************************
     O********************************* 126 'PAGE:'
     O************************ PAGE  Z  132
     O                                          146 'PAGE:'
     O                       PAGE          Z    152
     O*** END   CHG *** 01/09/98 **************************************
     O          E            HEAD           1
     O*** BEG   CHG *** 02/10/10 **************************************
     O**                                 60 'ENTRY/'
     O                                           30 'JURISDICTION'
     O                                           74 'ENTRY/'
     O*** END   CHG *** 02/10/10 **************************************
     O          E            HEAD           2
     O*** BEGIN CHG *** 01/09/98 **************************************
     O**********************************  3 'FND'
     O**********************************  6 'YR'
     O********************************** 13 'CASE'
     O********************************** 20 'EMPLR'
     O********************************** 24 'DIV'
     O********************************** 33 'CLAIMANT'
     O********************************** 49 'POST DT'
     O********************************** 56 'FROM'
     O********************************** 64 'TO'
     O********************************** 72 'WKS'
     O********************************** 75 'DA'
     O********************************** 79 'BR2'
     O********************************** 82 'PA'
     O********************************** 85 'NC'
     O********************************** 91 'PAYEE'
     O********************************** 99 'MOD'
     O**********************************111 'PAYABLE TO:'
     O**********************************131 'AMOUNT'
     O*
     O**********BEG CHG 02/10/10*******************************
     O********************************    3 'GRP'
     O********************************    7 'YR'
     O********************************   13 'CASE'
     O********************************   25 'EMPLR'
     O********************************   32 'DIV'
     O********************************   42 'CLAIMANT'
     O********************************   60 'POST DT'
     O********************************   70 'FROM'
     O********************************   81 'TO'
     O********************************   90 'WKS'
     O********************************   94 'DA'
     O********************************   99 'BR2'
     O********************************  103 'PA'
     O********************************  107 'NC'
     O********************************  116 'PAYEE'
     O********************************  123 'MOD'
     O********************************  136 'PAYABLE TO:'
     O********************************  151 'AMOUNT'
     O                                            3 'GRP'
     O                                            7 'YR'
     O                                           13 'CASE'
     O                                           29 'STATE'
     O                                           37 'EMPLR'
     O                                           45 'DIV'
     O                                           60 'CLAIMANT'
     O                                           80 'POST DT'
     O                                           90 'FROM'
     O                                          102 'TO'
     O                                          114 'WKS'
     O                                          120 'DA'
     O                                          125 'BR2'
     O                                          131 'PA'
     O                                          137 'NC'
     O                                          153 'PAYEE'
     O                                          162 'MOD'
     O                                          180 'PAYABLE TO:'
     O                                          194 'AMOUNT'
     O*** END   CHG *** 02/10/10 **************************************
     O*** END   CHG *** 01/09/98 **************************************
     O          E            DETL           1
     O*** BEGIN CHG *** 01/09/98 **************************************
     O************************ RPFND Z    3
     O************************            4 '-'
     O************************ XXYR       6
     O************************            7 '-'
     O************************ RPCASE    14
     O************************ CMEMP#Z   24
     O************************ CMDIV Z   30
     O************************ XXLSN     46
     O************************ X2TRDTY   55
     O************************ XXFRDTY   64
     O************************ XXTODTY   73
     O************************ RPWKS Z   78
     O************************ RPDAYSZ   81
     O************************ RPBR2     85
     O************************ RPTRCD    88
     O************************ RPNCCI    91
     O************************ RPFEIN   101
     O************************ RPMOD Z  105
     O************************ XXNAME   121
     O************************ RPAMT J  138
     O*
     O                       RPFND         Z      3
     O                                            4 '-'
     O                       RPFYR                7
     O                                            8 '-'
     O                       RPCASE              15
     O*** BEGIN CHG *** 02/10/10 **************************************
     O*********************    CMEMP#Z   25
     O*********************    CMDIV Z   32
     O*********************    XXLSN     49
     O*********************    X2TRDTY   61
     O*********************    XXFRDTY   73
     O*********************    XXTODTY   85
     O*********************    RPWKS Z   90
     O*********************    RPDAYSZ   94
     O*********************    RPBR2     99
     O*********************    RPTRCD   103
     O*********************    RPNCCI   107
     O*********************    RPFEIN   118
     O*********************    RPMOD Z  123
     O*********************    XXNAME   140
     O*********************    RPAMT J  152
     O                       XXST                27
     O                       CMEMP#        Z     37
     O                       CMDIV         Z     45
     O                       XXLSN               67
     O                       X2TRDT        Y     81
     O                       XXFRDT        Y     94
     O                       XXTODT        Y    106
     O                       RPWKS         Z    113
     O                       RPDAYS        Z    119
     O                       RPBR2              125
     O                       RPTRCD             131
     O                       RPNCCI             137
     O                       RPFEIN             153
     O                       RPMOD         Z    162
     O                       XXNAME             184
     O                       RPAMT         J    195
     O*** END   CHG *** 02/10/10 **************************************
     O*** END   CHG *** 01/09/98 **************************************
     O*
     O          E            SUBHD       1  2
     O               30      RELSTL              40
     O               31      DELTTL              40
     O               32      PENDTL              40
     O*** BEG ADD *** 12/19/03 *********************************
     O               05      XUSTAF              45
     O               05      XUNAME              87
     O*** END ADD *** 12/19/03 *********************************
     O*
     O*** BEG CHG *** 12/19/03 *********************************
     O          E   N05      USER        0  0
     O*** BEG ADD *** 02/20/09 *********************************
     O                                           10 'REQUESTOR:'
     O                       XUSTAF              14
     O                       XUNAME              55
     O*** END ADD *** 02/20/09 *********************************
     O          E    05      USER        1  0
     O******  E 00             USER
     O*** END CHG *** 12/19/03 *********************************
     O                                           10 'REQUESTOR:'
     O                       XUSTAF              14
     O                       XUNAME              55
     O          E            U1TOTL         2
     O                                           67 'TOTAL COUNT'
     O                                           88 'OF RELEASED CHECKS -'
     O                       XXCHKS              94 '   0 '
     O                                          117 'TOTAL CHECKS'
     O                       XXCAMT        J    132
     O*
     O          E            U2TOTL         2
     O                                           70 'TOTAL COUNT OF'
     O                                           88 'DELETED RECORDS -'
     O                       XXDELS              94 '   0 '
     O                                          117 'TOTAL DELETED RECORDS'
     O                       XXDAMT        J    132
     O*
     O          E            U5TOTL         2
     O                                           72 'TOTAL COUNT OF'
     O                                           88 'PENDED CHECKS -'
     O                       XXPEND              94 '   0 '
     O                                          117 'TOTAL PENDED CHECKS'
     O                       XXPAMT        J    132
     O*
     O*** BEG CHG *** 12/19/03 *********************************
     O          E   N05      U3TOTL      2  2
     O          E    05      U3TOTL      0  3
     O******  E 22             U3TOTL
     O*** END CHG *** 12/19/03 *********************************
     O*** BEGIN CHG *** 01/09/98 **************************************
     O********************************** 56 'FUND'
     O                                           56 'GROUP:'
     O*** END   CHG *** 01/09/98 **************************************
     O                       XHFND         Z     60
     O                                           70 'COUNT OF'
     O                                           88 'RELEASED CHECKS -'
     O                       XXFCHK              94 '   0 '
     O                                          117 'TOTAL CHECKS'
     O                       XXFAMT        J    132
     O*
     O*** BEG CHG *** 12/19/03 *********************************
     O          E   N05      U4TOTL      2  2
     O          E    05      U4TOTL      0  3
     O******  E 22             U4TOTL
     O*** END CHG *** 12/19/03 *********************************
     O*** BEGIN CHG *** 01/09/98 **************************************
     O********************************** 56 'FUND'
     O                                           56 'GROUP:'
     O*** END   CHG *** 01/09/98 **************************************
     O                       XHFND         Z     60
     O                                           78 'COUNT OF DELETED'
     O                                           88 'RECORDS -'
     O                       XXFCHK              94 '   0 '
     O                                          117 'TOTAL DELETED RECORDS'
     O                       XXFAMT        J    132
     O*
     O*** BEG CHG *** 12/19/03 *********************************
     O******  E 22             U6TOTL
     O          E   N05      U6TOTL      2  2
     O          E    05      U6TOTL      0  3
     O*** END CHG *** 12/19/03 *********************************
     O*** BEGIN CHG *** 01/09/98 **************************************
     O********************************** 56 'FUND'
     O                                           56 'GROUP:'
     O*** END   CHG *** 01/09/98 **************************************
     O                       XHFND         Z     60
     O                                           79 'COUNT OF PENDED'
     O                                           88 'CHECKS -'
     O                       XXFCHK              94 '   0 '
     O                                          117 'TOTAL CHECKS'
     O                       XXFAMT        J    132
