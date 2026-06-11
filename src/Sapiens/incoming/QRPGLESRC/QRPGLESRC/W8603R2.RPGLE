     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W8603R2 - DAILY CLAIMS REGISTER WORKFILE BUILD
     H*****************************************************************
     H*  PROGRAMMER: C:80                                             *
     H*  CREATED   : 10/21/97         PROJECT: WC673                  *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  05/25/07 C:191 PROJECT: WC1515                               *
     H*                 Check claims authority                        *
     H*                 for no-auth, do not include claimant info     *
     H*                                                               *
     H*  04/08/99 C:33  SEQ#: WC096/17370                             *
     H*                 CLEAR WORKFILE FIELDS PRIOR TO LOADING        *
     H*                                                               *
     H*  11/04/97 C:80  PROJECT:WC673                                 *
     H*                 ADDED ADDITIONAL KEY LIST BASED SOLELY ON CO# *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60     GENERAL READ/CHAIN                                    *
     H*                                                               *
     H*****************************************************************
     F*
     FWMCML3    IF   E           K DISK
     F* * * DAILY CLAIM REGISTER WORKFILE
     FWXDCL     O    E           K DISK
     F* * * CLAIM HISTORY FILE
     FWMCHL     IF   E           K DISK
     F* * * CLAIMANT DEMOGRAPHICS FILE
     FWMCDL     IF   E           K DISK
     F*
      *** BEG ADD ****** 05/25/07 **********************************

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D XXATTX          C                   CONST('*** NO AUTHORITY ***')
      *** END ADD ****** 05/25/07 **********************************

     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    X1ENTD
     C*
      ********** BEGIN ADD **** 11/04/97 ******************************
     C     CMKY1         KLIST
     C                   KFLD                    XXCO#
      ********** END   ADD **** 11/04/97 ******************************
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C     CHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    CMCASE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
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
     C     *LIKE         DEFINE    CMENTD        X1TODT
     C     *LIKE         DEFINE    CMENTD        X1ENTD
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFRDT        XXENTD            8 0
     C                   MOVE      QQTODT        XXTODT            8 0
     C*
     C*  CONVERT DATES BEFORE CHAIN
     C*
     C                   Z-ADD     XXENTD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1ENTD
     C*
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1TODT
     C*
     C     CMKEY         SETLL     PCM01                              50  60
      *
      ********** BEGIN CHG **** 11/04/97 ******************************
     C******************** READEPCM01                    60
     C     CMKY1         READE     PCM01                                  60
      ********** END   CHG **** 11/04/97 ******************************
      *
     C     *IN60         DOWEQ     *OFF
     C     X1TODT        ANDGE     CMENTD
     C*
     C***  BEGIN ADD  ***  04/08/99  *********************************
     C                   CLEAR                   PDC01
     C***  END   ADD  ***  04/08/99  *********************************
     C*
     C                   MOVE      CMFND         XXFND             3 0
     C                   MOVE      CMFYR         XXFYR             3 0
     C*
     C*  LOAD FIELDS FROM WMCML3 INTO WXDCL
     C*
     C                   MOVE      CMCO#         DCCO#
     C                   MOVE      CMFND         DCFND
     C                   MOVE      CMFYR         DCFYR
     C                   MOVE      CMCASE        DCCASE
     C                   MOVE      CMCLEX        DCCLEX
     C                   MOVE      CMACDT        DCACDT
     C                   MOVE      CMENTD        DCENTD
     C                   MOVE      CMRCVD        DCRCVD
     C                   MOVE      CMIUSR        DCIUSR
     C                   MOVE      CMEMP#        DCEMP#
     C                   MOVE      CMDIV         DCDIV
     C                   MOVE      CMLTFL        DCLTFL
     C                   MOVE      CMNCCI        DCNCCI
     C*
     C                   MOVE      *BLANK        QQLAST           56
     C                   MOVE      *BLANK        QQFRST           15
     C                   MOVE      *BLANK        XXCNAM           30
     C*
     C*   CHAIN TO THE DEMOGRAPHICS FILE
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLAST
     C                   MOVEL     CDFSN         QQFRST
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        DCNAME
     C                   END
     C*
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
     C                   MOVE      *BLANKS       DCNAME
     C                   MOVEL     XXATTX        DCNAME
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
     C*
     C*  FIND THE MOST RECENT RECORD IN CLAIM HISTORY FILE
     C*
     C     CMSTAT        IFEQ      'C'
     C*
     C                   MOVE      *ZEROS        DCCLOS
     C     CHKEY         SETGT     PCH01                              60
     C     CHKEY         READPE    PCH01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      CHEFDT        DCCLOS
     C*
     C                   ENDIF                                                  *IN60 IFEQ *OFF
     C*
     C                   ENDIF                                                  CMSTAT IFEQ 'C'
     C*
     C                   WRITE     PDC01                                61
      *
      ********** BEGIN CHG **** 11/04/97 ******************************
     C******************** READ PCM01                    60
     C     CMKY1         READE     PCM01                                  60
      ********** END   CHG **** 11/04/97 ******************************
      *
     C*
     C                   ENDDO                                                  *IN60 DOEQ *OFF
     C*
     C                   MOVE      *ON           *INLR
