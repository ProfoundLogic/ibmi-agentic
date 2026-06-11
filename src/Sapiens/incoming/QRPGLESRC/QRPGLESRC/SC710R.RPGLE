     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H* TITLE SC710R - REMARKS PRINT - PRINT PROGRAM                  *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  08/17/98  C:98  PROJECT: DR059                               *
     H*                  CHANGE REPORT FORMAT TO MATCH THAT OF CURRENT*
     H*                  B70 VERSION                                  *
     H*                                                               *
     H*  06/12/98  C:33  SEQ#: DR081/11680                            *
     H*                  PRINT HEADINGS IN PROPER ORDER.              *
     H* _____________________________________________________________ *
     H*                                                               *
     H*  02/08/92 LJD   PROJ: SC088                                   *
     H*                 1.  Print Additional Remarks w/ Major Remark  *
     H*                 2.  Print Key Definition instead of Key       *
     H*                 3.  Print Output Routing with selection/sort  *
     H*                                                               *
     H*  03/23/92 WLD   Proj: AJP                                     *
     H*                 1.  Changed 2A Remark Code to 5A.             *
     H*                                                               *
     H*  05/01/92 WLD   Proj: SC088                                   *
     H*                 1.  Changed report so that when record with a *
     H*                     new key is printing, the value of the key *
     H*                     is printed.                               *
     H*                                                               *
     H*  06/22/92 KRR   PROJ:                                         *
     H*                 1.  CHANGE TO USE THE OUTPUT ROUTING RECORD   *
     H*                                                               *
     H*  02/01/94 C:25  PROJ: DR002                                   *
     H*                 1.  ADDED SUMMARY CODE TO ROUTING SHEET       *
     H*                                                               *
     H*  05/18/94 C:20  PROJ: DRMNT                                   *
     H*                 1.  MODIFICATION TO PRINT NAME WHEN KEY IS    *
     H*                     PRINTED.  CHANGED FROM HEAD TO NKEY       *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       OF      OVERFLOW FOR PRINTING FOR REPORT                *
     H*                                                               *
     H*       08      BATCH MODE                                      *
     H*       09      DEMAND MODE                                     *
     H*       20      PRINT XXNAME IF NOT EQUAL TO BLANKS             *
     H*       50      READ/DOWEQ OF OPEN QUERY REMARKS DETAIL FILE    *
     H*       60      GENERAL USAGE (CHAIN)                           *
     H*       70      USER PRIMARY SORT ORDER - PRINT HEADING         *
      *****************************************************************
     FSDCML1    IF   E           K DISK
     FSMARL     IF   E           K DISK
     FSMSLL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     F**** BEGIN CHANGE **** 08/17/98 *********************************
     F******* O   F      85     OF     PRINTER      KINFDS PRTDS
     FQSYSPRT   O    F   95        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOF)
     F**** END   CHANGE **** 08/17/98 *********************************
     F*

     D SRT1            S              2    DIM(8) CTDATA PERRCD(1)              SORT SEQUENCE TITLES
     D SRT2            S             25    DIM(8) ALT(SRT1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  ADDITIONAL REMARKS FILE
     C     ARKY          KLIST
     C                   KFLD                    HLKEY
     C                   KFLD                    HLSEQ#
     C                   KFLD                    HLASQ#
     C*
     C     ARKY2         KLIST
     C                   KFLD                    HLKEY
     C                   KFLD                    HLSEQ#
     C*
     C*   SECURITY FILE
     C     SLKY          KLIST
     C                   KFLD                    $$USER
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE.
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQKEY            40
     C                   PARM                    QQUSER           10
     C                   PARM                    QQFRDT            7
     C                   PARM                    QQTODT            7
     C                   PARM                    QQSMCD            4
     C                   PARM                    QQRMTY            5
     C                   PARM                    QQDCID           12
     C                   PARM                    QQSORT            2
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
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
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     *LIKE         DEFINE    ARKEY         HLKEY
     C     *LIKE         DEFINE    ARSEQ#        HLSEQ#
     C     *LIKE         DEFINE    ARASQ#        HLASQ#
     C     *LIKE         DEFINE    ARCM75        XXCM75
     C     *LIKE         DEFINE    CMIUSR        XXUSER
     C     *LIKE         DEFINE    CMIUSR        RSUSER
     C     *LIKE         DEFINE    CMDCID        RSDCID
     C     *LIKE         DEFINE    Q1USER        $$USER
     C     *LIKE         DEFINE    CMKEY         SVKEY
     C                   MOVE      *BLANK        XXKEY            79
     C                   MOVE      *BLANK        XXNAME           79
     C*
     C     QQUSER        IFNE      *BLANKS
     C                   MOVEL     QQUSER        XXUSER
     C                   MOVE      '1'           *IN70
     C                   ELSE                                                   ELS QQUSER NE *BLANK
     C                   MOVE      *BLANKS       XXUSER
     C                   MOVE      '0'           *IN70
     C                   END                                                    END QQUSER NE *BLANK
     C*
     C* Establish Routing Sheet Information
     C*
     C                   MOVE      *BLANK        RSKEY            80
     C                   MOVE      *BLANK        RSKYDF           80
     C                   MOVE      *BLANK        RSUSER
     C                   MOVE      *BLANK        RSFRDT
     C                   MOVE      *BLANK        RSTODT
     C                   MOVE      *BLANK        RSSMCD            4
     C                   MOVE      *BLANK        RSRMTY            5
     C                   MOVE      *BLANK        RSDCID
     C                   MOVE      *BLANK        RSSORT           25
     C     QQKEY         IFNE      *BLANK
     C                   MOVEL     QQKEY         RSKEY
     C                   ELSE
     C                   MOVEL     '*ALL'        RSKEY
     C                   END
     C     QQUSER        IFNE      *BLANK
     C                   MOVEL     QQUSER        RSUSER
     C                   ELSE
     C                   MOVEL     '*ALL'        RSUSER
     C                   END
     C     QQFRDT        IFEQ      '0000101'
     C     QQTODT        ANDEQ     '1991231'
     C                   MOVE      '1'           *IN71
     C                   ELSE
     C                   MOVE      '0'           *IN71
     C                   MOVEL     QQFRDT        RSFRDT            7 0
     C*
     C                   Z-ADD     RSFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         RXFRDT            8 0
     C*
     C                   MOVEL     QQTODT        RSTODT            7 0
     C*
     C                   Z-ADD     RSTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         RXTODT            8 0
     C                   END
     C*
     C     QQSMCD        IFNE      *BLANK
     C                   MOVEL     QQSMCD        RSSMCD
     C                   ELSE
     C                   MOVEL     '*ALL'        RSSMCD
     C                   END
     C     QQRMTY        IFNE      *BLANK
     C                   MOVEL     QQRMTY        RSRMTY
     C                   ELSE
     C                   MOVEL     '*ALL'        RSRMTY
     C                   END
     C     QQDCID        IFNE      *BLANK
     C                   MOVEL     QQDCID        RSDCID
     C                   ELSE
     C                   MOVEL     '*ALL'        RSDCID
     C                   END
     C                   Z-ADD     1             X                 5 0
     C     QQSORT        LOOKUP    SRT1(X)                                60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     SRT2(X)       RSSORT
     C                   END
     C*
     C*  INITIALIZE WORK FIELDS
     C*
     C                   TIME                    DSDTTM
     C*
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C                   READ      PCM01                                  50
     C*
     C*** Build Key Definition here
     C                   MOVEL     CMPNAM        X#PNAM
     C                   MOVEL     CMKEY         X#KEY
     C                   MOVE      *BLANKS       X#NAME
     C                   MOVE      ' '           X#RTCD
     C                   CALL      'SRKCVT1'
     C                   PARM                    X#PNAM           10
     C                   PARM                    X#KEY            79
     C                   PARM                    X#NAME           79
     C                   PARM                    X#RTCD            1
     C*
     C     X#RTCD        IFNE      'N'
     C                   MOVEL     X#KEY         XXKEY
     C     RSKEY         IFNE      '*ALL'
     C                   MOVEL     X#KEY         RSKEY
     C                   END
     C                   ELSE
     C                   MOVEL     CMKEY         XXKEY
     C                   END
     C*
     C                   MOVE      *BLANKS       SVKEY
     C                   MOVEL     CMKEY         SVKEY
     C*
     C                   MOVEL     X#NAME        XXNAME
     C     XXNAME        IFEQ      *BLANKS
     C                   MOVE      '0'           *IN20
     C                   ELSE
     C                   MOVE      '1'           *IN20
     C                   END
     C     RSKEY         IFNE      '*ALL'
     C                   MOVEL     X#NAME        RSKYDF
     C                   END
     C*
     C*   GET THE DISTRIBUTION INFORMATION.
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION, IF A VALID RECORD IS FOUND.
     C*
     C     *IN60         IFEQ      '0'
     C     ORSTAT        ANDEQ     'A'
     C     Q1USER        CHAIN     PUP01                              60
     C***  BEGIN ADD  ***  06/12/98  *********************************
     C*
     C*  Left Justify Title in Field
     C*
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C*
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF                                                  N<=60
     C***  END   ADD  ***  06/12/98  *********************************
      ***** BEG ADD ***** 01/28/10 ***********************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
      ***** END ADD ***** 01/28/10 ***********************************
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C********           EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C***  BEGIN ADD  ***  06/12/98  *********************************
     C*
     C*  Center Text in Field
     C*
     C                   MOVEL     ORTITL        ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        ORTITL
     C***  END   ADD  ***  06/12/98  *********************************
     C*
     C                   END
     C                   EXCEPT    HEAD
     C*
     C     *IN50         IFEQ      '1'
     C                   EXCEPT    ERRMSG
     C                   GOTO      TERM
     C                   END
     C*
     C*
     C     *IN50         DOWEQ     '0'
     C* ================================================================
     C* Only print remarks that this user has authority to
     C                   MOVEL     Q1USER        $$USER
     C     SLKY          CHAIN     PSL01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     *BLANKS       $$USER
     C     SLKY          CHAIN     PSL01                              60
     C     *IN60         IFEQ      '1'
     C                   EXCEPT    ERRMS2
     C                   GOTO      TERM
     C                   END                                                    END 1 *IN60 - *BLANK
     C                   END                                                    END 1 *IN60 - Q1USER
     C*
     C     CMSECL        IFLE      SLSECL
     C* ================================================================
     C* Security passed - continue with remark
     C*
     C*** Build Key Definition here
     C                   MOVEL     CMPNAM        X#PNAM
     C                   MOVEL     CMKEY         X#KEY
     C                   MOVE      *BLANKS       X#NAME
     C                   MOVE      ' '           X#RTCD
     C                   CALL      'SRKCVT1'
     C                   PARM                    X#PNAM           10
     C                   PARM                    X#KEY            79
     C                   PARM                    X#NAME           79
     C                   PARM                    X#RTCD            1
     C*
     C                   MOVE      *BLANKS       XXKEY
     C*
     C     X#RTCD        IFNE      'N'
     C                   MOVEL     X#KEY         XXKEY
     C                   ELSE
     C                   MOVEL     CMKEY         XXKEY
     C                   END
     C*
     C                   MOVEL     X#NAME        XXNAME
     C     XXNAME        IFEQ      *BLANKS
     C                   MOVE      '0'           *IN20
     C                   ELSE
     C                   MOVE      '1'           *IN20
     C                   END
     C*
     C                   Z-ADD     CMCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXCDAT            6 0
     C*
     C                   MOVE      *BLANKS       X1COMM
     C                   MOVE      *BLANKS       X2COMM
     C                   MOVE      *BLANKS       XXCM1
     C                   MOVE      *BLANKS       XXCM2
     C                   MOVE      *BLANKS       XXCM3
     C                   MOVE      *BLANKS       XXCM4
     C*
     C                   MOVEL     CMCOMM        X1COMM          150
     C                   MOVE      CMCOMM        X2COMM          125
     C*
     C                   MOVEL     X1COMM        XXCM1            75
     C                   MOVE      X1COMM        XXCM2            75
     C*
     C                   MOVEL     X2COMM        XXCM3            75
     C                   MOVE      X2COMM        XXCM4            50
     C*
     C     CMKEY         IFNE      SVKEY
     C     *INOF         ANDNE     '1'
     C**** BEGIN CHANGE **** 08/17/98 *********************************
     C*************        EXCPTNKEY
     C     XXLINE        IFLE      55
     C                   EXCEPT    NKEY
     C                   ELSE
     C                   MOVE      '1'           *INOF
     C                   ENDIF                                                  XXLINE LE 55
     C**** END   CHANGE **** 08/17/98 *********************************
     C                   ENDIF                                                  CMKEY NE SVKEY
     C*
     C                   MOVE      *BLANKS       SVKEY
     C                   MOVEL     CMKEY         SVKEY
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C*** BEGIN ADD *** 08/17/98 **************************************
     C                   EXCEPT    DET1
     C                   EXCEPT    DET2
     C*** END   ADD *** 08/17/98 **************************************
     C                   MOVE      '0'           *INOF
     C**** BEGIN CHANGE **** 08/17/98 *********************************
     C****************     ENDIF                           *INOF EQ '1'
     C****************     EXCPTDET1
     C****************     EXCPTDET2
     C                   ELSE
     C     XXLINE        IFLE      59
     C                   EXCEPT    DET1
     C                   EXCEPT    DET2
     C                   ELSE
     C                   MOVE      '1'           *INOF
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET1
     C                   EXCEPT    DET2
     C                   MOVE      '0'           *INOF
     C                   ENDIF                                                  XXLINE LE 59
     C                   ENDIF                                                  *INOF EQ '1'
     C**** BEGIN END    **** 08/17/98 *********************************
     C*
     C     *INOF         IFEQ      '1'
     C*** BEGIN ADD *** 08/17/98 **************************************
     C     XXCM2         ANDNE     *BLANKS
     C*** END   ADD *** 08/17/98 **************************************
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET1
     C                   MOVE      '0'           *INOF
     C                   ENDIF                                                  *INOF EQ '1'
     C     XXCM2         IFNE      *BLANKS
     C                   EXCEPT    DET3
     C                   ENDIF                                                  XXCM2 NE *BLANK
     C*
     C     *INOF         IFEQ      '1'
     C*** BEGIN ADD *** 08/17/98 **************************************
     C     XXCM3         ANDNE     *BLANKS
     C*** END   ADD *** 08/17/98 **************************************
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET1
     C                   MOVE      '0'           *INOF
     C                   ENDIF                                                  *INOF EQ '1'
     C     XXCM3         IFNE      *BLANKS
     C                   EXCEPT    DET4
     C                   ENDIF                                                  XXCM3 NE *BLANK
     C*
     C     *INOF         IFEQ      '1'
     C*** BEGIN ADD *** 08/17/98 **************************************
     C     XXCM4         ANDNE     *BLANKS
     C*** END   ADD *** 08/17/98 **************************************
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET1
     C                   MOVE      '0'           *INOF
     C                   ENDIF                                                  *INOF EQ '1'
     C     XXCM4         IFNE      *BLANKS
     C                   EXCEPT    DET5
     C                   ENDIF                                                  XXCM4 NE *BLANK
     C*
     C* Get Additional Remarks and Print them
     C*
     C                   MOVE      '0'           XXONE
     C                   MOVE      *BLANKS       HLKEY
     C                   MOVE      *ZEROS        HLSEQ#
     C                   MOVE      *ZEROS        HLASQ#
     C                   MOVEL     CMKEY         HLKEY
     C                   MOVE      CMSEQ#        HLSEQ#
     C     ARKY          SETLL     PAR01
     C     ARKY2         READE     PAR01                                  61
     C     *IN61         DOWEQ     '0'
     C*
     C     XXONE         IFEQ      '0'
     C     ARCM75        ANDNE     *BLANKS
     C                   MOVE      '1'           XXONE             1
     C                   MOVEL     ARCM75        XXCM75
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET1
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C                   EXCEPT    DET6
     C                   ELSE                                                    IFEQXXONE
     C*
     C     ARCM75        IFNE      *BLANKS
     C                   MOVEL     ARCM75        XXCM75
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET1
     C                   EXCEPT    DET6
     C                   MOVE      '0'           *INOF
     C                   ELSE
     C                   EXCEPT    DET7
     C                   END                                                    IFEQ*INOF
     C*
     C                   END                                                    IFEQXXCM75
     C                   END                                                    IFEQXXONE
     C*
     C     ARKY2         READE     PAR01                                  61
     C                   END                                                    *IN61DOWEQ
     C*
     C                   END                                                    END CMSECL LE SLSECL
     C*
     C                   READ      PCM01                                  50
     C                   END                                                    END DOWEQ 50
     C*
     C*** END PROGRAM
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found and orStat = 'A';
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     O*************************************************************************
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
     O          E            ROUTE          1
     O                                           20 'KEY          :'
     O          E            ROUTE          1
     O                       RSKEY               80
     O          E   N20      ROUTE          1
     O          E    20      ROUTE          2
     O                       RSKYDF              80
     O*
     O          E            ROUTE          2
     O                                           20 'USER         :'
     O                       RSUSER              31
     O*
     O          E            ROUTE          2
     O               71                          25 '*ALL'
     O                                           20 'DATE         :'
     O              N71      RXFRDT              31 '  /  /    '
     O              N71                          34 'TO'
     O              N71      RXTODT              45 '  /  /    '
     O*
     O          E            ROUTE          2
     O                                           20 'SUMMARY CODE :'
     O                       RSSMCD              25
     O*
     O          E            ROUTE          2
     O                                           20 'REMARK TYPE  :'
     O                       RSRMTY              26
     O*
     O          E            ROUTE          2
     O                                           20 'DOCUMENT ID  :'
     O                       RSDCID              33
     O*
     O          E            ROUTE          1
     O                                           20 'SORT SEQUENCE:'
     O                       RSSORT              46
     O*
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O****                               13 '(C) COPYRIGHT'
     O****                               30 'PROGRESSIVE DATA'
     O****                               51 'SOLUTIONS, INC. 1992'
     O****                             +  1 '- 1996'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      ***** END CHG ****** 02/02/06 ***********************************
     OQSYSPRT   E            HEAD           1 06
     O**** BEGIN CHANGE **** 08/17/98 *********************************
     O*****************        DSDATEY    8
     O*****************                  43 'DATABASE AND REMARKS'
     O*****************                  59 'TRACKING SYSTEM'
     O*****************        DSTIME    80 '  :  :  '
     O                       DSDATE        Y     13
     O                                           56 'DATABASE AND REMARKS'
     O                                           72 'TRACKING SYSTEM'
     O                       DSTIME              95 '  :  :  '
     O**** END   CHANGE **** 08/17/98 *********************************
     O          E            HEAD           2
     O**** BEGIN CHANGE **** 08/17/98 *********************************
     O*****************                   6 'SC710R'
     O*****************                  48 'REMARKS LISTING'
     O*****************                  75 'PAGE:'
     O*****************        PAGE      80
     O                                           11 'SC710R'
     O                                           61 'REMARKS LISTING'
     O                                           90 'PAGE:'
     O                       PAGE                95
     O**** END   CHANGE **** 08/17/98 *********************************
     O          E    70      HEAD           2
     O**** BEGIN CHANGE **** 08/17/98 *********************************
     O****************                    5 'USER:'
     O****************         XXUSER    16
     O                                           10 'USER:'
     O                       XXUSER              21
     O**** END   CHANGE **** 08/17/98 *********************************
     O***  BEGIN DEL  ***  06/12/98  *********************************
     O********E* 1             HEAD
     O**********               XXKEY     79
     O***  END   DEL  ***  06/12/98  *********************************
     O***  BEGIN ADD  ***  08/17/98  *********************************
     O          E            HEAD           1
     O                       XXKEY               84
     O***  END   ADD  ***  08/17/98  *********************************
     O          E   N20      HEAD           1
     O          E    20      HEAD           2
     O**** BEGIN CHANGE **** 08/17/98 *********************************
     O***************          XXNAME    79
     O                                           10 'NAME:'
     O                       XXNAME              84
     O**** END   CHANGE **** 08/17/98 *********************************
     O          E            HEAD           1
     O**** BEGIN CHANGE **** 08/17/98 *********************************
     O*************                       7 'REMARKS'
     O*************                      17 'REMARKS'
     O*************                      28 'DOCUMENT'
     O                                           11 'REMARK'
     O                                           20 'REMARK'
     O                                           51 'REMARK'
     O**** END   CHANGE **** 08/17/98 *********************************
     O          E            HEAD           1
     O**** BEGIN CHANGE **** 08/17/98 *********************************
     O****************                    6 'DATE'
     O****************                   16 'TYPE'
     O****************                   24 'ID'
     O                                           10 'DATE'
     O                                           19 'TYPE'
     O**** END   CHANGE **** 08/17/98 *********************************
     O***  BEGIN ADD  ***  06/12/98  *********************************
     O**** BEGIN DELETE **** 08/17/98 *********************************
     O****    E  1             HEAD
     O****                     XXKEY     79
     O**** END   CHANGE **** 08/17/98 *********************************
     O***  END   ADD  ***  06/12/98  *********************************
     O**** BEGIN CHANGE **** 08/17/98 *********************************
     O*****   E 11             NKEY
     O*****                    XXKEY     79
     O          E            NKEY        2  1
     O                       XXKEY               84
     O**** END   CHANGE **** 08/17/98 *********************************
     O          E   N20      NKEY           1
     O          E    20      NKEY           2
     O**** BEGIN CHANGE **** 08/17/98 *********************************
     O************             XXNAME    79
     O                                           10 'NAME:'
     O                       XXNAME              84
     O**** END   CHANGE **** 08/17/98 *********************************
     O          E            DET1        1  1
     O**** BEGIN CHANGE **** 08/17/98 *********************************
     O***************          XXCDAT     8 '  /  /  '
     O***************          CMRMCD    16
     O***************          CMDCID    32
     O                       XXCDAT              13 '  /  /  '
     O                       CMRMCD              21
     O**** END   CHANGE **** 08/17/98 *********************************
     O          E            DET2           1
     O**** BEGIN CHANGE **** 08/17/98 *********************************
     O*******                  XXCM1     80
     O                       XXCM1               95
     O**** END   CHANGE **** 08/17/98 *********************************
     O          E            DET3           1
     O**** BEGIN CHANGE **** 08/17/98 *********************************
     O*******                  XXCM2     80
     O                       XXCM2               95
     O**** END   CHANGE **** 08/17/98 *********************************
     O          E            DET4           1
     O**** BEGIN CHANGE **** 08/17/98 *********************************
     O*******                  XXCM3     80
     O                       XXCM3               95
     O**** END   CHANGE **** 08/17/98 *********************************
     O          E            DET5           1
     O**** BEGIN CHANGE **** 08/17/98 *********************************
     O*******                  XXCM4     55
     O                       XXCM4               75
     O**** END   CHANGE **** 08/17/98 *********************************
     O          E            DET6           1
     O**** BEGIN CHANGE **** 08/17/98 *********************************
     O*******                  XXCM75    80
     O                       XXCM75              95
     O**** END   CHANGE **** 08/17/98 *********************************
     O          E            DET7           1
     O**** BEGIN CHANGE **** 08/17/98 *********************************
     O*******                  XXCM75    80
     O                       XXCM75              95
     O**** END   CHANGE **** 08/17/98 *********************************
     O*
     O          E            ERRMSG      3
     O**** BEGIN CHANGE **** 08/17/98 *********************************
     O************                       19 '*** NO RECORDS MEET'
     O************                       41 'SELECTED CRITERIA ***'
     O                                           24 '*** NO RECORDS MEET'
     O                                           46 'SELECTED CRITERIA ***'
     O**** END   CHANGE **** 08/17/98 *********************************
     O*
     O          E            ERRMS2      3  1
     O**** BEGIN CHANGE **** 08/17/98 *********************************
     O**********                         23 '** YOU ARE NOT ENROLLED'
     O**********                         47 'IN DARTS SECURITY FILE.'
     O                                           28 '** YOU ARE NOT ENROLLED'
     O                                           52 'IN DARTS SECURITY FILE.'
     O**** END   CHANGE **** 08/17/98 *********************************
     O          E            ERRMS2         1
     O**** BEGIN CHANGE **** 08/17/98 *********************************
     O**********                         27 'SEE SYSTEM ADMINISTRATOR'
     O**********                         52 'FOR FURTHER INFORMATION.'
     O                                           32 'SEE SYSTEM ADMINISTRATOR'
     O                                           57 'FOR FURTHER INFORMATION.'
     O**** END   CHANGE **** 08/17/98 *********************************
**  SRT1/SRT2 - SORT SEQUENCE TITLES
KYKY - KEY
KDKD - KEY/DATE
KRKR - KEY/REMARK TYPE
KOKO - KEY/DOCUMENT ID
UKUK - USER/KEY
UDUD - USER/DATE
URUR - USER/REMARK TYPE
UOUO - USER/DOCUMENT ID
