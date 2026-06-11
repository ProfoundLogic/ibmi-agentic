     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W4215R - CHECK RECONCILIATION DETAIL FILE MAINTENANCE
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  10/20/14 C:267 Support: 85613                                *
     H*                Update all claims transaction for a group      *
     H*                check with reconciliation date when the        *
     H*                group check is cleared.                        *
     H*                                                               *
     H*  05/20/04 C:53 PROJECT: WC1139                                *
     H*                UPDATE PARCDT BASED ON SELECTION.              *
     H*                                                               *
     H*  08/30/02 C:15 Project: WC577                                 *
     H*                Chgd F24 to F04.                               *
     H*                                                               *
     H* 01/24/01 C:61  SUPPORT WA099 25700                            *
     H*              - After "F7=Calc book reconciliation",           *
     H*                 - If F12 pressed then return to SCR01.        *
     H*                 - If F3 pressed then exit program.            *
     H*              - Correct 'F10=Search'.                          *
     H*              - After "F9=Display cleared checks",             *
     H*                 - If F12 pressed then return to SCR01.        *
     H*                 - If F3 pressed then exit program.            *
     H*                                                               *
     H*  12/08/99 C:118 PROJECT: WC577
     H*                 V3 display file modifications.
     H*
     H*  10/30/97 c:30 Project: WC415                                 *
     H*           W4220R PARM QQCLL assignment changed.               *
     H*           Assign QQCALL value to QQCLL.                       *
     H*                                                               *
     H*  09/04/97 C:30 Project:  WC659                                *
     H*           Version 3 changes.  Remove WMBAP.  Use WSCKP.       *
     H*                                                               *
     H* _____________________________________________________________ *
     H*                                                               *
     H*  4/04/91  JJJ   - ADDED WDCRL5 TO SELECT ONLY RECORDS WITH    *
     H*                   STATUS EQUAL TO BLANK FOR MAINTENANCE SCRN  *
     H*                   THIS WAS DONE TO REDUCE RESPONSE TIME ON    *
     H*                   ROLLING FUNCTIONS.                          *
     H*  01/30/92 WLD  PROJECT WC114                                  *
     H*                DON'T ALLOW USER TO CHANGE FIELDS              *
     H*****************************************************************
     FW4215F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDCRL5    UF A E           K DISK
     FWDCRL1    UF   E           K DISK
     F                                     RENAME(PCR01:PCR02)
      *** BEGIN CHG *** 09/04/97 ********************************
     F****WMBAL   IF  E           K        DISK
      *
      *  Bank Information file
     FWMBIL4    IF   E           K DISK
      *
      *  Bank Account Master file
     FWSCKL     IF   E           K DISK
      *** END   CHG *** 09/04/97 ********************************
     F*** BEG ADD *** 05/20/04 ****************************
     FWDPAL4    UF   E           K DISK
     F*** END ADD *** 05/20/04 ****************************

     F*** BEG ADD *** 10/20/14 ****************************
     D TOTAMT          S              9  2 INZ
     F*** END ADD *** 10/20/14 ****************************
     D DA              S              7  0 DIM(11)
     D TM              S              6  0 DIM(11)
     D OAT             S              1    DIM(11)
     D ODT             S              8  0 DIM(11)
     D OCK             S              7  0 DIM(11)
     D OCD             S              8  0 DIM(11)
     D OTP             S              1    DIM(11)
     D OAM             S              9  2 DIM(11)
     D ODS             S             30    DIM(11)
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEYC            S              7  0 DIM(2)
     D KEYD            S              7  0 DIM(2)
     D KEYT            S              6  0 DIM(2)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  AT                     1     11
     D                                     DIM(11)
     D  SCAT01                 1      1
     D  SCAT02                 2      2
     D  SCAT03                 3      3
     D  SCAT04                 4      4
     D  SCAT05                 5      5
     D  SCAT06                 6      6
     D  SCAT07                 7      7
     D  SCAT08                 8      8
     D  SCAT09                 9      9
     D  SCAT10                10     10
     D  SCAT11                11     11
     D                 DS
     D  DT                     1    088  0
     D                                     DIM(11)
     D  SCDT01               001    008  0
     D  SCDT02               009    016  0
     D  SCDT03               017    024  0
     D  SCDT04               025    032  0
     D  SCDT05               033    040  0
     D  SCDT06               041    048  0
     D  SCDT07               049    056  0
     D  SCDT08               057    064  0
     D  SCDT09               065    072  0
     D  SCDT10               073    080  0
     D  SCDT11               081    088  0
     D                 DS
     D  CK                     1     77  0
     D                                     DIM(11)
     D  SCCK01                 1      7  0
     D  SCCK02                 8     14  0
     D  SCCK03                15     21  0
     D  SCCK04                22     28  0
     D  SCCK05                29     35  0
     D  SCCK06                36     42  0
     D  SCCK07                43     49  0
     D  SCCK08                50     56  0
     D  SCCK09                57     63  0
     D  SCCK10                64     70  0
     D  SCCK11                71     77  0
     D                 DS
     D  CD                     1    088  0
     D                                     DIM(11)
     D  SCCD01               001    008  0
     D  SCCD02               009    016  0
     D  SCCD03               017    024  0
     D  SCCD04               025    032  0
     D  SCCD05               033    040  0
     D  SCCD06               041    048  0
     D  SCCD07               049    056  0
     D  SCCD08               057    064  0
     D  SCCD09               065    072  0
     D  SCCD10               073    080  0
     D  SCCD11               081    088  0
     D                 DS
     D  TP                     1     11
     D                                     DIM(11)
     D  SCTP01                 1      1
     D  SCTP02                 2      2
     D  SCTP03                 3      3
     D  SCTP04                 4      4
     D  SCTP05                 5      5
     D  SCTP06                 6      6
     D  SCTP07                 7      7
     D  SCTP08                 8      8
     D  SCTP09                 9      9
     D  SCTP10                10     10
     D  SCTP11                11     11
     D                 DS
     D  AM                     1     99  2
     D                                     DIM(11)
     D  SCAM01                 1      9  2
     D  SCAM02                10     18  2
     D  SCAM03                19     27  2
     D  SCAM04                28     36  2
     D  SCAM05                37     45  2
     D  SCAM06                46     54  2
     D  SCAM07                55     63  2
     D  SCAM08                64     72  2
     D  SCAM09                73     81  2
     D  SCAM10                82     90  2
     D  SCAM11                91     99  2
     D                 DS
     D  DS                     1    330
     D                                     DIM(11)
     D  SCDS01                 1     30
     D  SCDS02                31     60
     D  SCDS03                61     90
     D  SCDS04                91    120
     D  SCDS05               121    150
     D  SCDS06               151    180
     D  SCDS07               181    210
     D  SCDS08               211    240
     D  SCDS09               241    270
     D  SCDS10               271    300
     D  SCDS11               301    330
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  CHECK RECONCILIATION DETAIL FILE
     C*
     C     CRKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCACCT
     C*
     C     CRKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCACCT
     C                   KFLD                    XXCHK#            7 0
     C                   KFLD                    XXDT
     C                   KFLD                    XXTIME            6 0
     C*
     C     CRKY2         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCACCT
     C                   KFLD                    XXCHK#
     C*
      *** BEGIN CHG *** 09/04/97 ***********************************************
     C***BANK ACCOUNT FILE
     C***
     C***        BAKEY     KLIST
     C***                  KFLD           UPCO#
     C***                  KFLD           SCFND
     C***                  KFLD           SCACCT
      *
      *  Bank Information file
      *
     C     BIKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXAPPL            2
     C                   KFLD                    SCACCT
     C                   KFLD                    SCFND
      *
      *  Bank Account Master file
      *
     C     CKKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCACCT
     C*
     C*** BEG ADD *** 05/20/04 ****************************
     C*
     C     PAKY          KLIST
     C                   KFLD                    CRCO#
     C                   KFLD                    CRFND
     C                   KFLD                    CRCHK#
     C*
     C*** END ADD *** 05/20/04 ****************************
     C*
     C     *LIKE         DEFINE    CRDT          XXDT
     C     *LIKE         DEFINE    CRRVDT        XXRDT
     C     *LIKE         DEFINE    CRRVDT        XXRVDT
     C     *LIKE         DEFINE    CRACDT        XXACDT
     C     *LIKE         DEFINE    CRACDT        XXSVDT
      *
      *** END   CHG *** 09/04/97 ***********************************************
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
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
      *** BEGIN ADD *** 09/04/97 **********************************************
     C                   MOVE      'WC'          XXAPPL
      *** END   ADD *** 09/04/97 **********************************************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     UPFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         SCDATE
     C*
     C                   MOVE      *BLANKS       SCACCT
     C                   MOVE      Q1USER        SCUSER
     C                   MOVEL(P)  QQPRDS        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
     C     QQCALL        COMP      '1'                                    09
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
     C     XXUP          IFEQ      '1'
     C                   MOVE      *BLANKS       SCUP
     C                   ELSE
     C                   MOVEL     RR(1)         SCUP
     C                   END
     C     XXDOWN        IFEQ      '1'
     C                   MOVE      *BLANKS       SCDOWN
     C                   ELSE
     C                   MOVEL     RR(2)         SCDOWN
     C                   END
     C                   EXFMT     SCR02
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C***
     C*** F03=EXIT
     C***
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C***
     C*** F12=CANCEL
     C***
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
      *** BEGIN CHG *** 11/13/97 ***********************************************
     C***
     C*** F10=SEARCH
     C***
     C     *INKJ         IFEQ      *ON
      *
     C     HFLD          IFEQ      'SCACCT'
     C*
     C                   MOVE      UPCO#         ##CO#             3
     C                   MOVE      SCFND         ##FND             3
      *
     C                   CALL      'W2420R'                                     Bank Info search
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C     *INKL         PARM      *INKL         ##KL              1
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM      *BLANKS       ##ACCT           20
     C                   PARM      *ZERO         ##EFDT            8
     C                   PARM      'WC'          ##APPL            2
      ***** BEG CHG ******* 01/24/01 **********************************
     C**************       PARM *BLANKS   ##CKTP  2
     C                   PARM      *BLANKS       ##CKTP           10
      ***** END CHG ******* 01/24/01 **********************************
      *
     C     ##ACCT        IFEQ      *BLANK
      ***** BEG DEL ******* 01/24/01 **********************************
     C*************        MOVE *BLANK    SCACCT
      ***** END DEL ******* 01/24/01 **********************************
     C                   ELSE
     C                   MOVE      ##ACCT        SCACCT
     C                   ENDIF
      *
      *** END   CHG *** 09/04/97 ***********************************************
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   ENDIF                                                  SCCOYR=HFLD=SCCOCA
      *** END   CHG *** 11/13/97 ***********************************************
     C*
     C* F7=CALCULATE BANK RECONCILIATION
     C*
     C     *INKG         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*     Validate Bank ACCT#
      *** BEGIN CHG *** 09/04/97 *************************************
     C*          BAKEY     CHAINPBA01                20
      *
     C     BIKEY         CHAIN     PBI01                              20
      *
     C     *IN20         DOWEQ     *OFF
     C     BISTAT        ANDEQ     'I'
     C     BIKEY         READE     PBI01                                  20
     C                   ENDDO
      *
      *** END   CHG *** 09/04/97 *************************************
      *
     C     *IN20         IFEQ      '1'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
      *
      *** BEGIN ADD *** 09/04/97 *************************************
     C     CKKEY         CHAIN     PCK01                              22
      *
     C     *IN22         IFEQ      *ON
     C     CKCRUP        ORNE      'Y'
     C                   MOVE      *ON           *IN22
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
      *
      *** END   ADD *** 09/04/97 *************************************
      *
     C*
      *** BEGIN CHG *** 10/30/97 *************************************
     C*                    MOVE '1'       QQCLL   1
     C                   MOVE      QQCALL        QQCLL             1
      *** END   CHG *** 10/30/97 *************************************
     C                   MOVE      UPCO#         QQCO#             3
     C                   CALL      'W4220R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCLL
     C                   PARM                    QQCO#
     C                   PARM                    SCACCT
      ***** BEG ADD ******* 01/24/01 **********************************
     C     *INKC         PARM                    QQKC              1
     C                   PARM                    QQKL              1
      *
      *
     C   KC              GOTO      TERM
      ***** END ADD ******* 01/24/01 **********************************
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C* F9=DISPLAY CLEARED CHECKS
     C*
     C     *INKI         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C*     VALIDATE ACCT#
      *** BEGIN CHG *** 09/04/97 *************************************
     C********** BAKEY     CHAINPBA01                20
      *
     C     BIKEY         CHAIN     PBI01                              20
      *
     C     *IN20         DOWEQ     *OFF
     C     BISTAT        ANDEQ     'I'
     C     BIKEY         READE     PBI01                                  20
     C                   ENDDO
      *
      *** END   CHG *** 09/04/97 *************************************
      *
     C     *IN20         IFEQ      '1'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
      *
      *** BEGIN ADD *** 09/04/97 *************************************
     C     CKKEY         CHAIN     PCK01                              22
      *
     C     *IN22         IFEQ      *ON
     C     CKCRUP        ORNE      'Y'
     C                   MOVE      *ON           *IN22
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
      *
      *** END   ADD *** 09/04/97 *************************************
      *
     C*
     C                   MOVE      '2'           QQRTCD            1
     C                   CALL      'W4216R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCACCT
     C                   PARM                    QQRTCD
      ***** BEG ADD ******* 01/24/01 **********************************
     C     *INKC         PARM                    QQKC              1
     C                   PARM                    QQKL              1
      *
      *
     C   KC              GOTO      TERM
      ***** END ADD ******* 01/24/01 **********************************
     C*
     C     QQRTCD        IFEQ      'A'
     C                   GOTO      TERM
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   ROLL UP REQUEST
     C*
     C     *IN18         CABEQ     '1'           SCR02T
     C*
     C*   ROLL DOWN REQUEST
     C*
     C     *IN19         CABEQ     '1'           SCR02T
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
     C                   MOVE      *BLANK        SCCONM
     C                   MOVEL     *ZERO         SCFND
     C                   MOVEL     *BLANK        SCNAME
     C                   MOVE      *BLANKS       SCACCT
     C                   Z-ADD     *ZERO         SCDATE
     C*
     C                   ELSE
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C     FNKEY         CHAIN     PFN01                              60
     C***** BEGIN ADD **** 12/08/99 ***********************************
     C     *IN60         IFEQ      *OFF
     C***** END   ADD **** 12/08/99 ***********************************
     C                   MOVEL     UPFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C                   MOVE      *BLANKS       SCACCT
     C                   Z-ADD     DSDATE        SCDATE
     C***** BEGIN ADD **** 12/08/99 ***********************************
     C                   ELSE
     C                   MOVEL     *BLANKS       SCFND
     C                   MOVEL     *BLANKS       SCNAME
     C                   ENDIF                                                  *IN60=OFF
     C***** END   ADD **** 12/08/99 ***********************************
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C* F24=DELETE TRANSACTION - ONLY THOSE ENTERED THRU THE
     C*            CHECK DETAIL MAINTENANCE
     C*
     C*** Begin chg *** 08/30/02 **************************************
     C********** *INKY     IFEQ '1'
     C     *INKD         IFEQ      *ON
     C*** End chg ***** 08/30/02 **************************************
     C                   EXSR      RITE
     C                   Z-ADD     1             X                 2 0
     C     'D'           LOOKUP    AT(X)                                  60
     C     *IN60         IFEQ      '1'
     C     X             DOWLT     12
     C*
     C     AT(X)         IFEQ      'D'
     C*
     C*  IF DELETING A REVERSAL, NEED TO POSSIBLY UPDATE STATUS ON
     C*  ORIGINAL CHECK.
     C*
     C     TP(X)         IFEQ      'Z'
     C*
     C                   Z-ADD     0             XXRDT
     C                   MOVE      *BLANKS       XXRVTP            1
     C                   Z-ADD     0             XXRVDT
     C                   Z-ADD     CK(X)         XXCHK#
     C                   Z-ADD     DA(X)         XXDT
     C                   Z-ADD     TM(X)         XXTIME
     C     CRKY1         CHAIN     PCR01                              60
     C     *IN60         IFEQ      '0'
     C*** BEGIN ADD C04 WC231  04/22/93
     C                   EXCEPT    PCR01E
     C*** END   ADD C04 WC231  04/23/93
     C                   Z-ADD     CRRVDT        XXRVDT
     C                   MOVE      CRRVTP        XXRVTP
     C     CRKY2         CHAIN     PCR02                              60
     C     *IN60         DOWEQ     '0'
     C     CRACTP        ANDNE     'V'
     C     CRACTP        ANDNE     'S'
     C     CRACTP        ANDNE     'X'
     C*** BEGIN ADD C04 WC231  04/22/93
     C                   EXCEPT    PCR02E
     C*** END   ADD C04 WC231  04/23/93
     C     CRKY2         READE     PCR02                                  60
     C                   END
     C     *IN60         IFEQ      '0'
     C                   MOVE      XXRVTP        CRACTP
     C                   Z-ADD     XXRVDT        CRACDT
     C     CRRDT2        IFNE      0
     C                   Z-ADD     CRRDT2        XXRDT
     C                   ELSE
     C                   Z-ADD     CRRDT1        XXRDT
     C                   END
     C     CRACDT        IFLE      XXRDT
     C                   MOVE      CRACTP        CRSTAT
     C                   END
     C                   UPDATE    PCR02
     C                   END
     C                   END
     C*
     C                   END
     C*
     C                   Z-ADD     CK(X)         XXCHK#
     C                   Z-ADD     DA(X)         XXDT
     C                   Z-ADD     TM(X)         XXTIME
     C     CRKY1         CHAIN     PCR01                              60
     C     *IN60         IFEQ      '0'
     C                   DELETE    PCR01
     C                   END
     C                   END
     C                   ADD       1             X
     C*
     C                   END
     C                   END
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   MOVE      KEYC(1)       SCCHK#
     C                   GOTO      REPOS
     C                   END
     C*
     C                   END
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
     C*  VALIDATE THE ACCOUNT NUMBER
     C*
      *** BEGIN CHG *** 09/04/97 *************************************
     C***********BAKEY     CHAINPBA01                20
      *
     C     BIKEY         CHAIN     PBI01                              20
      *
     C     *IN20         DOWEQ     *OFF
     C     BISTAT        ANDEQ     'I'
     C     BIKEY         READE     PBI01                                  20
     C                   ENDDO
      *
      *** END   CHG *** 09/04/97 *************************************
      *
     C     *IN20         IFEQ      '1'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
      *
      *** BEGIN ADD *** 09/04/97 *************************************
     C     CKKEY         CHAIN     PCK01                              22
      *
     C     *IN22         IFEQ      *ON
     C     CKCRUP        ORNE      'Y'
     C                   MOVE      *ON           *IN22
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
      *
      *** END   ADD *** 09/04/97 *************************************
     C*
     C*  THE DATE MUST BE VALID
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDATE
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   Z-ADD     0             SCCHK#
     C                   MOVE      '1'           *IN57
     C                   GOTO      REPOS
     C*
     C*  PROCESS REQUEST SCREEN
     C*
     C     SCR02T        TAG
     C*
     C*  INVALID ROLL REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   EXSR      RITE
     C     XXUP          IFNE      *BLANK
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     *IN19         IFEQ      '1'
     C                   EXSR      RITE
     C     XXDOWN        IFNE      *BLANK
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      *BLANK        XXDOWN
     C                   Z-ADD     KEYC(2)       XXCHK#
     C                   Z-ADD     KEYD(2)       XXDT
     C                   Z-ADD     KEYT(2)       XXTIME
     C     CRKY1         CHAIN     PCR01                              60
     C     CRKEY         READE     PCR01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEYC(1)       XXCHK#
     C                   Z-ADD     KEYD(1)       XXDT
     C                   Z-ADD     KEYT(1)       XXTIME
     C     CRKY1         CHAIN     PCR01                              60
     C                   READP     PCR01                                  60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCCHK#        IFNE      *ZERO
     C                   EXSR      RITE
     C     REPOS         TAG
     C                   MOVE      *BLANK        KEYC
     C                   MOVE      *BLANK        KEYD
     C                   MOVE      *BLANK        KEYT
     C                   MOVE      *BLANK        XXUP
     C   57              MOVE      '1'           XXDOWN
     C  N57              MOVE      *BLANK        XXDOWN
     C                   MOVE      '0'           *IN57
     C                   Z-ADD     SCCHK#        XXCHK#
     C                   MOVE      *BLANK        SCCHK#
     C                   Z-ADD     0             XXDT
     C                   Z-ADD     0             XXTIME
     C     CRKY1         SETLL     PCR01
     C     CRKEY         READE     PCR01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  IF ENTER WAS PRESSED EXECUTE THE RITE SUBROUTINE
     C*
     C                   EXSR      RITE
     C*
     C*
     C*  RETURN TO SCREEN 1
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*  CLEAR THE SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C*             CLEAR SCREEN ARRAYS
     C                   MOVE      *BLANKS       AT
     C                   MOVE      *BLANKS       DT
     C                   MOVE      *BLANKS       CK
     C                   MOVE      *BLANKS       CD
     C                   MOVE      *BLANKS       TP
     C                   MOVE      *BLANKS       AM
     C                   MOVE      *BLANKS       DS
     C                   MOVE      *BLANKS       DA
     C                   MOVE      *BLANKS       TM
     C*             CLEAR HOLD ARRAYS
     C                   MOVE      *BLANKS       OAT
     C                   MOVE      *BLANKS       ODT
     C                   MOVE      *BLANKS       OCK
     C                   MOVE      *BLANKS       OCD
     C                   MOVE      *BLANKS       OTP
     C                   MOVE      *BLANKS       OAM
     C                   MOVE      *BLANKS       ODS
     C                   Z-ADD     *ZERO         I1                2 0          ARRAY INDEX
     C                   MOVEA     '0000000'     *IN(65)                        EXISTING RCD
     C                   MOVEA     '0000'        *IN(72)                        EXISTING RCD
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C     I1            ADD       64            X                 2 0
     C                   MOVE      '1'           *IN(X)
     C                   MOVE      CRACTP        AT(I1)
     C     CRACTP        IFEQ      *BLANK
     C                   Z-ADD     SCDATE        DT(I1)
     C                   ELSE
     C*
     C                   Z-ADD     CRACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DT(I1)
     C*
     C                   END
     C                   Z-ADD     CRCHK#        CK(I1)
     C*
     C                   Z-ADD     CRCKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CD(I1)
     C*
     C                   MOVE      CRRTP         TP(I1)
     C                   Z-ADD     CRCKAM        AM(I1)
     C                   MOVEL     CRDESC        DS(I1)
     C                   Z-ADD     CRDT          DA(I1)
     C                   Z-ADD     CRTIME        TM(I1)
     C                   EXCEPT    PCR01E
     C*               FILL HOLD ARRAYS
     C                   MOVE      AT(I1)        OAT(I1)
     C                   MOVE      DT(I1)        ODT(I1)
     C                   MOVE      CK(I1)        OCK(I1)
     C                   MOVE      CD(I1)        OCD(I1)
     C                   MOVE      TP(I1)        OTP(I1)
     C                   MOVE      AM(I1)        OAM(I1)
     C                   MOVE      DS(I1)        ODS(I1)
     C                   ENDSR
     C*
     C*  LOAD FORWARD SUBROUTINE
     C*
     C     LOAD          BEGSR
     C                   EXSR      CLR
     C     OVER          TAG
     C*
     C*  END OF FILE
     C*
     C   60              MOVE      '1'           XXUP              1
     C   60              GOTO      DONE
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     CRSTAT        IFEQ      *BLANK
     C                   ADD       1             I1
     C                   EXSR      FILL
     C                   END
     C     CRKEY         READE     PCR01                                  60
     C   60              DO
     C     I1            IFLT      11
     C                   MOVE      '1'           XXUP
     C                   END
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      11                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C  N60              EXCEPT    PCR01E
     C     I1            IFNE      0
     C                   Z-ADD     CK(1)         KEYC(1)
     C                   Z-ADD     CK(11)        KEYC(2)
     C                   Z-ADD     DA(1)         KEYD(1)
     C                   Z-ADD     DA(11)        KEYD(2)
     C                   Z-ADD     TM(1)         KEYT(1)
     C                   Z-ADD     TM(11)        KEYT(2)
     C                   END
     C                   ENDSR
     C*
     C*  LOAD BACKWARD SUBROUTINE
     C*
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     12            I1
     C     OVERP         TAG
     C*
     C*  BEGINNING OF FILE
     C*
     C     *IN60         IFEQ      '1'
     C                   EXSR      LOADB
     C                   GOTO      DONEP
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     CRSTAT        IFEQ      *BLANKS
     C                   SUB       1             I1
     C                   EXSR      FILL
     C                   END
     C                   READP     PCR01                                  60
     C  N60              DO
     C     CRCO#         IFNE      UPCO#
     C     CRACCT        ORNE      SCACCT
     C                   MOVE      '1'           *IN60
     C                   END
     C                   END
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C  N60              EXCEPT    PCR01E
     C                   MOVEL     CK(1)         KEYC(1)
     C                   MOVEL     CK(11)        KEYC(2)
     C                   Z-ADD     DA(1)         KEYD(1)
     C                   Z-ADD     DA(11)        KEYD(2)
     C                   Z-ADD     TM(1)         KEYT(1)
     C                   Z-ADD     TM(11)        KEYT(2)
     C                   ENDSR
     C*
     C*  LOAD FROM BEGINNING OF FILE SUBROUTINE
     C*
     C     LOADB         BEGSR
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     *ZERO         XXCHK#
     C                   Z-ADD     0             XXDT
     C                   Z-ADD     0             XXTIME
     C     CRKY1         SETLL     PCR01
     C     CRKEY         READE     PCR01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C     CRSTAT        IFEQ      *BLANK
     C                   ADD       1             I1
     C                   EXSR      FILL
     C                   END
     C     CRKEY         READE     PCR01                                  60
     C   60              DO
     C     I1            IFLT      11
     C                   MOVE      '1'           XXUP
     C                   END
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      11                                   61
     C   61              GOTO      OVERB
     C     DONEB         TAG
     C                   ENDSR
     C**************************************************************
     C*     RITE SUBROUTINE - UPDATE RECORDS                       *
     C**************************************************************
     C     RITE          BEGSR
     C*
     C                   DO        11            X                 2 0
     C     X             ADD       44            X2                2 0          ERROR
     C     X             ADD       64            X3                2 0          NOT NEW RCD
     C*
     C*   COMPARE HOLD FIELDS FROM ORIGINAL SCREEN TO THE CURRENT
     C*   FIELDS. IF THE FIELDS CHANGED, EDIT & UPDATE OR WRITE.
     C*
     C     AT(X)         IFNE      OAT(X)
     C     DT(X)         ORNE      ODT(X)
     C     CK(X)         ORNE      OCK(X)
     C     CD(X)         ORNE      OCD(X)
     C     TP(X)         ORNE      OTP(X)
     C     AM(X)         ORNE      OAM(X)
     C     DS(X)         ORNE      ODS(X)
     C*
     C*   ACTIVITY MUST BE X,V,S,D, OR BLANK
     C*
     C     AT(X)         IFNE      'X'
     C     AT(X)         ANDNE     'V'
     C     AT(X)         ANDNE     'S'
     C     AT(X)         ANDNE     'D'
     C     AT(X)         ANDNE     *BLANK
     C                   MOVE      '1'           *IN(X2)
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*   IF ACTIVITY TYPE = D, THEN CMD 19 MUST BE ON
     C*
     C     AT(X)         IFEQ      'D'
     C*** Begin chg *** 08/30/02 **************************************
     C********** *INKY     ANDEQ'0'
     C     *INKD         ANDEQ     '0'
     C*** End chg ***** 08/30/02 **************************************
     C                   MOVE      '1'           *IN56
     C                   MOVE      '1'           *IN(X2)
     C                   GOTO      STRT
     C                   END
     C*
     C*   IF DELETING, THE ORIGINAL TRANS MUST HAVE BEEN KEYED THRU
     C*   THE CHECK MAINTENANCE OPTION.  A TRANS WRITTEN FROM THE
     C*   PAYMENT DETAIL FILE (WDPAP) CANNOT BE DELETED.
     C*   THE TRANSACTION CANNOT BE DELETED IF ALREADY RECONCILED.
     C*
     C     AT(X)         IFEQ      'D'
     C*** Begin chg *** 08/30/02 **************************************
     C********** *INKY     ANDEQ'1'
     C     *INKD         ANDEQ     '1'
     C*** End chg ***** 08/30/02 **************************************
     C*
     C                   Z-ADD     CK(X)         XXCHK#
     C                   Z-ADD     DA(X)         XXDT
     C                   Z-ADD     TM(X)         XXTIME
     C     CRKY1         CHAIN     PCR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CRKY          XXSEQ#            5 0
     C     XXSEQ#        IFNE      0
     C                   MOVE      '1'           *IN33
     C                   MOVE      '1'           *IN(X2)
     C                   GOTO      STRT
     C                   END
     C     CRRDT1        IFNE      0
     C                   MOVE      '1'           *IN58
     C                   MOVE      '1'           *IN(X2)
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   GOTO      RITEN
     C*
     C                   END
     C*
     C*   VOIDS AND STOP PAY CAN ONLY BE PERFORMED ON CHECKS
     C*
     C     AT(X)         IFEQ      'V'
     C     AT(X)         OREQ      'S'
     C     TP(X)         IFNE      'C'
     C                   MOVE      '1'           *IN(X2)
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   EDIT ACTIVITY STATUS FOR REVERSAL. CAN ONLY BE 'D' OR 'X'.
     C*
     C     TP(X)         IFEQ      'Z'
     C     AT(X)         ANDNE     'X'
     C     AT(X)         ANDNE     'D'
     C                   MOVE      '1'           *IN(X2)
     C                   MOVE      '1'           *IN57
     C                   GOTO      STRT
     C                   END
     C*
     C*   ACTIVITY DATE MUST BE ENTERED IF ACT TYPE IS NOT BLANK
     C*
     C     AT(X)         IFNE      *BLANK
     C     DT(X)         COMP      0                                      21
     C     *IN21         IFEQ      '1'
     C                   MOVE      '1'           *IN(X2)
     C                   GOTO      STRT
     C                   END
     C*
     C                   Z-ADD     DT(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         DT(X)
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN(X2)
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   EDIT CHECK#, CHECK DATE, TYPE, AMOUNT, & DESC IF NEW ENTRY
     C*
     C     *IN(X3)       IFNE      '1'
     C*
     C*   CHECK NUMBER CANNOT BE LESS THAN 0
     C*
     C     CK(X)         COMP      0                                    23
     C     *IN23         IFEQ      '1'
     C                   MOVE      '1'           *IN(X2)
     C                   GOTO      STRT
     C                   END
     C*
     C*   CHECK DAT MUST BE A VALID DATE
     C*
     C                   Z-ADD     CD(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         CD(X)
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN(X2)
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*  TYPE MUST BE C, D, B, A, R, N, I, T, Z, OR E.
     C*
     C     TP(X)         IFNE      'C'
     C     TP(X)         ANDNE     'D'
     C     TP(X)         ANDNE     'B'
     C     TP(X)         ANDNE     'A'
     C     TP(X)         ANDNE     'R'
     C     TP(X)         ANDNE     'N'
     C     TP(X)         ANDNE     'I'
     C     TP(X)         ANDNE     'T'
     C     TP(X)         ANDNE     'Z'
     C     TP(X)         ANDNE     'E'
     C                   MOVE      '1'           *IN26
     C                   MOVE      '1'           *IN(X2)
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK# MUST BE ENTERED IF TYPE IS C, Z, OR N.
     C*
     C     TP(X)         IFEQ      'C'
     C     TP(X)         OREQ      'Z'
     C     TP(X)         OREQ      'N'
     C     CK(X)         IFEQ      0
     C                   MOVE      '1'           *IN24
     C                   MOVE      '1'           *IN(X2)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  IF REVERSAL TYPE, CHECK# ENTERED MUST EXIST FOR VOID/STOP PAY
     C*  IF NO ERROR, LOAD REVERSAL SAVE FIELDS
     C*
     C     TP(X)         IFEQ      'Z'
     C                   Z-ADD     CK(X)         XXCHK#
     C     CRKY2         CHAIN     PCR02                              31
     C     *IN31         DOWEQ     '0'
     C     CRACTP        ANDNE     'V'
     C     CRACTP        ANDNE     'S'
     C     CRKY2         READE     PCR02                                  31
     C                   END
     C     *IN31         IFEQ      '1'
     C                   MOVE      '1'           *IN(X2)
     C                   GOTO      STRT
     C                   END
     C                   Z-ADD     CRCKAM        XXAMT             9 2
     C                   MOVE      CRACTP        XXACTP            1
     C                   Z-ADD     CRACDT        XXACDT
     C                   MOVE      CRSTAT        XXSVST            1
     C     CRRDT2        IFNE      0
     C                   Z-ADD     CRRDT2        XXSVDT
     C                   ELSE
     C                   Z-ADD     CRRDT1        XXSVDT
     C                   END
     C                   END
     C*
     C*  AMOUNT CANNOT BE 0
     C*
     C     AM(X)         IFEQ      0
     C                   MOVE      '1'           *IN27
     C                   MOVE      '1'           *IN(X2)
     C                   GOTO      STRT
     C                   END
     C*
     C*  IF REVERSAL, AMOUNT MUST BE SAME AS THE EXISTING CHECK
     C*
     C     TP(X)         IFEQ      'Z'
     C     AM(X)         ANDNE     XXAMT
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN(X2)
     C                   GOTO      STRT
     C                   END
     C*
     C*  PAYEE/DESCRIPTION CANNOT BE BLANK
     C*
     C     DS(X)         IFEQ      *BLANKS
     C                   MOVE      '1'           *IN28
     C                   MOVE      '1'           *IN(X2)
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    -NEW ENTRY
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C                   Z-ADD     CK(X)         XXCHK#
     C                   Z-ADD     DA(X)         XXDT
     C                   Z-ADD     TM(X)         XXTIME
     C     CRKY1         CHAIN     PCR01                              60
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      UPCO#         CRCO#
     C                   MOVEL     SCACCT        CRACCT
     C                   Z-ADD     CK(X)         CRCHK#
     C*
     C                   Z-ADD     CD(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRCKDT
     C*
     C                   Z-ADD     AM(X)         CRCKAM
     C                   MOVE      *BLANKS       CRKY
     C                   MOVE      TP(X)         CRRTP
     C                   Z-ADD     0             CRRDT1
     C                   Z-ADD     0             CRRDT2
     C                   MOVEL     DS(X)         CRDESC
     C                   Z-ADD     0             CRBUL1
     C                   MOVE      *BLANKS       CRSTAT
     C     TP(X)         IFNE      'Z'
     C*
     C                   MOVE      'N'           CRRVFL
     C                   MOVE      *BLANKS       CRRVTP
     C                   Z-ADD     0             CRRVDT
     C                   MOVE      *BLANKS       CRSVST
     C                   Z-ADD     0             CRSVDT
     C                   Z-ADD     0             CRRVTR
     C*
     C                   ELSE
     C*
     C*  IF REVERSAL ENTRY, GET THE ACTIVITY CODE & DATE FROM CHECK
     C*
     C                   MOVE      'N'           CRRVFL
     C                   MOVE      XXACTP        CRRVTP
     C                   Z-ADD     XXACDT        CRRVDT
     C                   MOVE      XXSVST        CRSVST
     C                   Z-ADD     XXSVDT        CRSVDT
     C*
     C                   Z-ADD     DT(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRRVTR
     C*
     C                   END
     C                   END
     C*
     C*** BEG ADD *** 05/20/04 ****************************
     C*
     C*** INIT VALID CHK REC INDICATOR
     C*
     C                   MOVE      *OFF          *IN15
     C*
     C     AT(X)         IFEQ      'X'
     C     CRRTP         ANDEQ     'C'
     C     AT(X)         OREQ      ' '
     C     CRRTP         ANDEQ     'C'
     C*
     C*** BEG ADD *** 10/20/14 ****************************
     C                   CLEAR                   TOTAMT
     C*** END ADD *** 10/20/14 ****************************
     C*
     C**** ACCESS THE PAYMENT FILE
     C*
     C     PAKY          CHAIN(N)  PPA01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C*** BEG ADD *** 10/20/14 ****************************
      ** check if group check
     C     PAACCT        IFEQ      CRACCT
     C     PAPGTP        ANDEQ     'G'
     C                   ADD       PAAMT         TOTAMT
     C                   MOVE      *ON           *IN15
     C                   ELSE
     C*** END ADD *** 10/20/14 ****************************
     C*
     C     PAACCT        IFEQ      CRACCT
     C     PAAMT         ANDEQ     CRCKAM
     C                   MOVE      *ON           *IN15
     C                   ENDIF
     C*** BEG ADD *** 10/20/14 ****************************
     C                   ENDIF
     C*** END ADD *** 10/20/14 ****************************
     C*
     C     PAKY          READE(N)  PPA01                                  99
     C*
     C                   ENDDO
     C                   ENDIF
     C*
     C*** END ADD *** 05/20/04 ****************************
     C*
     C                   MOVE      AT(X)         CRACTP
     C     AT(X)         IFEQ      *BLANKS
     C                   Z-ADD     0             CRACDT
     C                   ELSE
     C*
     C                   Z-ADD     DT(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRACDT
     C*
     C                   END
     C                   MOVE      Q1USER        CRUSER
     C                   MOVE      Q1PGM         CRPGM
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        CRTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRDT
     C*
     C*** BEG ADD *** 05/20/04 ****************************
     C*
     C**** UPDATE THE PAYMENT FILE
     C*
     C     *IN15         IFEQ      *ON
     C*
     C     PAKY          CHAIN     PPA01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*** BEG CHG *** 10/20/14 ****************************
      ** process all transactions for a group check
     C                   IF        (PAACCT  =   CRACCT  AND
     C                              PAPGTP  =   'G'     AND
     C                              TOTAMT  =   CRCKAM) OR
     C                             (PAACCT  =   CRACCT  AND
     C                              PAAMT   =   CRCKAM)
     C*
     C**** PAACCT        IFEQ      CRACCT
     C**** PAAMT         ANDEQ     CRCKAM
     C*** END CHG *** 10/20/14 ****************************
     C*
     C     AT(X)         IFEQ      *BLANKS
     C                   Z-ADD     0             PARCDT
     C                   ELSE
     C                   Z-ADD     CRACDT        PARCDT
     C                   ENDIF
     C*
     C                   MOVE      CRUSER        PAUSER
     C                   MOVE      CRPGM         PAPGM
     C                   Z-ADD     CRTIME        PATIME
     C                   Z-ADD     CRDT          PADT
     C*
     C                   UPDATE    PPA01
     C                   ENDIF
     C*
     C     PAKY          READE     PPA01                                  99
     C*
     C                   ENDDO
     C                   ENDIF
     C*
     C*** END ADD *** 05/20/04 ****************************
     C*
     C*  UPDATE OR WRITE TO CHECK RECONCILIATION DETAIL FILE
     C*
     C     *IN60         IFEQ      '0'
     C                   UPDATE    PCR01
     C                   ELSE
     C                   WRITE     PCR01
     C                   END
     C*
     C*  IF REVERSAL, GET ORIGINAL CHECK & RELEASE STATUS
     C*
     C     TP(X)         IFEQ      'Z'
     C                   Z-ADD     CK(X)         XXCHK#
     C     CRKY2         CHAIN     PCR02                              31
     C     *IN31         DOWEQ     '0'
     C     CRACTP        ANDNE     'V'
     C     CRACTP        ANDNE     'S'
     C     CRKY2         READE     PCR02                                  31
     C                   END
     C     *IN31         IFEQ      '0'
     C     CRSTAT        ANDNE     *BLANK
     C                   MOVE      *BLANK        CRSTAT
     C                   UPDATE    PCR02
     C                   END
     C                   END
     C*
     C*  MOVE NEW FIELDS TO OLD FIELDS
     C*
     C                   MOVE      AT(X)         OAT(X)
     C                   MOVE      DT(X)         ODT(X)
     C                   MOVE      CK(X)         OCK(X)
     C                   MOVE      CD(X)         OCD(X)
     C                   MOVE      TP(X)         OTP(X)
     C                   MOVE      AM(X)         OAM(X)
     C                   MOVE      DS(X)         ODS(X)
     C                   Z-ADD     CRDT          DA(X)
     C                   Z-ADD     CRTIME        TM(X)
     C*
     C                   END                                                    OLD = NEW
     C                   END                                                    DO 11 X
     C*
     C*  ALLOW ROLL UP IF ADDING NEW PAYROLLS AND NEED MORE ENTRY LINES
     C*
     C     *IN18         IFEQ      '1'
     C     AM(11)        ANDNE     0
     C*
     C     XXUP          IFNE      *BLANKS
     C     CRKEY         SETGT     PCR01
     C                   READP     PCR01                                  60
     C                   Z-ADD     CRCHK#        KEYC(2)
     C                   Z-ADD     CRDT          KEYD(2)
     C                   Z-ADD     CRTIME        KEYT(2)
     C                   END
     C                   Z-ADD     KEYC(2)       KEYC(1)
     C                   Z-ADD     KEYD(2)       KEYD(1)
     C                   Z-ADD     KEYT(2)       KEYT(1)
     C                   MOVE      *BLANK        XXUP
     C                   END
     C*
     C     RITEN         ENDSR
     OPCR01     E            PCR01E
     O****BEGIN ADD C04 04/22/93
     OPCR02     E            PCR02E
     O****END   ADD C04 04/22/93
**
ROLL UP
ROLL DOWN
