     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE WA002R - GET AGENT AND PRODUCER INFORMATION
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL GATHER AGENT AND PRODUCER    *
     H*                INFORMATION TO BE USED BY OTHER PROGRAMS.      *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 04/02/97  PROGRAMMER: C:33  PROJECT: 60AND          *
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 09/21/16 C:136 Support: 43065
     H*                If the agent number is passed in, retrieve
     H*                the agent history for that agent even if
     H*                inactive. Change implemented for the
     H*                Agent Activity Report
     H*
     H* 01/15/10 C:61  Project: 139DMQ
     H*                Pass back RRN WMAHP.
     H*
     H* 12/21/09 C:33  Project: CI080                                 *
     H*                Return Agent History Effective date found.     *
     H*
     H*  5/22/09 C:062 Project: 133WGW
     H*                Add Control Flag Parameter.
     H*
     H*  3/05/07 C:002 Project: DR214
     H*                IDARTS USEAGE - ALLOW LR INSTEAD OF RETRN
     H*
     H* 12/20/06 C:185 Project: 132WGJ
     H*                Producer's Email Address.
     H*
     H* 05/01/06 C:33  Project: 132WFE                                *
     H*                Add parm to pass agent Rate #.                 *
     H*                                                               *
     H* 10/15/01 C:61  PROJECT WC970                                  *
     H*                Pass Agent Rate info back from WMARP.          *
     H*                                                               *
     H*  08/10/00 C:33 SEQ#: WA109/23913                              *
     H*                CHECK FOR INACTIVE RATE CODES.                 *
     H*                                                               *
     H*  07/19/99  C:33  PROJECT: WC899                               *
     H*                  GET PLAN TYPE FOR AGENT.                     *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     F***  Begin Chg  ***  05/22/09  **************************************
     F**********WMAHL   IF  E           K        DISK
     FWMAHL     IF   E           K DISK    USROPN
      ***** BEG ADD ******* 01/15/10 **********************************
     F                                     INFDS(AHFDS)
      ***** END ADD ******* 01/15/10 **********************************
     F***  End   Chg  ***  05/22/09  **************************************
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWMAPL     IF   E           K DISK
     F***  BEGIN ADD  ***  07/19/99  *********************************
     FWMARL     IF   E           K DISK
     F***  END   ADD  ***  07/19/99  *********************************
      *
      *
      ***** BEG ADD ******* 01/15/10 **********************************

     D AHFDS           DS
     D  AHRRN                397    400B 0
      ***** END ADD ******* 01/15/10 **********************************
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D***  BEGIN ADD  ***  07/19/99  *********************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D***  END   ADD  ***  07/19/99  *********************************
     D*
     D                 DS
     D  ZIP                    1     10
     D  XXZIP1                 1      5
     D  DASH                   6      6
     D  XXZIP2                 7      9

     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C***  BEGIN ADD  ***  07/19/99  *********************************
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C***  END   ADD  ***  07/19/99  *********************************
     C*
     C     *ENTRY        PLIST
1    C                   PARM                    @@CO#             3 0
2    C                   PARM                    @@FND             3 0
3    C                   PARM                    @@EMP#            9 0
4    C                   PARM                    @@DIV             5 0
5    C                   PARM                    @@FYR             3 0
6    C                   PARM                    @@AGT#            5 0
7    C                   PARM                    @@ANAM           60
8    C                   PARM                    @@PROD            5 0
9    C                   PARM                    @@PNAM           60
10   C                   PARM                    @@AREA            3 0
11   C                   PARM                    @@PHON            7 0
     C***  BEGIN ADD  ***  07/19/99  *********************************
12   C                   PARM                    @@PLAN            1
13   C                   PARM                    @@DATE            8 0
     C***  END   ADD  ***  07/19/99  *********************************
      ***** BEG ADD ******* 10/15/01 **********************************
14   C                   PARM                    @@NWRT            5 2          New Rate
15   C                   PARM                    @@RNRT            5 2          Renew Rate
      ***** END ADD ******* 10/15/01 **********************************
     C***  Begin Add  ***  05/01/06  **************************************
16   C                   PARM                    @@RT#             3 0
     C***  End   Add  ***  05/01/06  **************************************
     C***  Begin Add  ***  12/20/06  **************************************
17   C                   PARM                    @@PEML           60
     C***  End   Add  ***  12/20/06  **************************************
     C***  Begin Add  ***  05/22/09  **************************************
18   C                   PARM                    @@CAFL            1
     C***  End   Add  ***  05/22/09  **************************************
      ***** BEG ADD ******* 01/15/10 **********************************
19   C                   PARM                    @AHRRN            9 0          RRN WMAHP
      ***** END ADD ******* 01/15/10 **********************************
     C*
     C*  NOTE: CO#/FND NEED TO HAVE VALUES PASSED TO THEM.
     C*        IF EMP#/DIV ARE ENTERED:
     C*          -IF FYR IS ENTERED THAT FYR WILL BE USED.
     C*          -IF FYR IS ZERO THE MOST RECENT FYR WILL BE USED
     C*        IF EMP#/DIV ARE NOT ENTERED AGT# NEEDS TO BE PASSED
     C*        TO GET ANAM (AGENT NAME)
     C*        PROD/PNAM/AREA/PHON ONLY NEED TO BE INCLUDED IF NEEDED.
     C*        IF THE PLAN TYPE IS NEEDED IT WILL NEED TO BE PASSED IN
     C*        AS A '*', OTHERWISE THE PLAN WILL BE RETURNED AS *BLANK
     C*          - IF THE PLAN TYPE IS REQUESTED, THE AGENT HISTORY
     C*            RECORD AS OF THE DATE PASSED IN WILL BE USED FOR THE
     C*            RATE.  IF THE DATE IS NOT PASSED IN THE SYSTEM DATE
     C*            WILL BE USED.  THE DATE WILL NOT BE USED UNLESS THE
     C*            PLAN IS REQUESTED.
     C*          - IF PLAN TYPE REQUESTED '*', NEW AND RENEW RATES FROM
     C*            WMARP WILL BE RETURNED.
     C*
     C*
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@EMP#
     C                   KFLD                    @@DIV
     C                   KFLD                    @@FYR
     C*
     C     AHKY1         KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@EMP#
     C                   KFLD                    @@DIV
     C*
     C***  BEGIN ADD  ***  07/19/99  *********************************
     C     AHKY2         KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@EMP#
     C                   KFLD                    @@DIV
     C                   KFLD                    @@FYR
     C                   KFLD                    X7DATE
     C***  END   ADD  ***  07/19/99  *********************************
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@AGT#
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*  AGENT PRODUCER FILE
     C*
     C     APKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    @@PROD
     C***  BEGIN ADD  ***  07/19/99  *********************************
     C*
     C*  AGENT RATE FILE
     C*
     C     ARKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    AHFND
     C                   KFLD                    AHRT#
     C*
     C     ARKY1         KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    AHFND
     C                   KFLD                    AHRT#
     C                   KFLD                    AHEFDT
     C***  END   ADD  ***  07/19/99  *********************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    AHAGT#        XXAGT#
     C     *LIKE         DEFINE    AHPROD        XXPROD
     C     *LIKE         DEFINE    AGNAME        XXANAM
     C     *LIKE         DEFINE    AAFEIN        XXFEIN
     C     *LIKE         DEFINE    AAMOD         XXMOD
     C     *LIKE         DEFINE    AGMAD1        XXMAD1
     C     *LIKE         DEFINE    AGPAD1        XXPAD1
     C     *LIKE         DEFINE    AHEFDT        XXEFDT
     C     *LIKE         DEFINE    AHRT#         XXRT#
     C     *LIKE         DEFINE    AHAGRC        XXAGRC
     C***  END   ADD  ***  07/19/99  *********************************
     C     *LIKE         DEFINE    @@PLAN        XXPLAN
     C     *LIKE         DEFINE    @@DATE        X8DATE
     C     *LIKE         DEFINE    AREFDT        X7DATE
     C***  END   ADD  ***  07/19/99  *********************************
     C*
     C*================================================================
     C*  Normal Programming Area
     C*================================================================
     C*
     C***  Begin Chg  ***  05/22/09  **************************************
     C                   OPEN      WMAHL
     C***  End   Chg  ***  05/22/09  **************************************
     C**** START ADD *** 3/05/07 ***********************************
     C                   MOVEL     *BLANKS       X@LR
     C                   MOVEL     @@ANAM        X@LR              2
     C     X@LR          IFEQ      'LR'
     C                   MOVEL     *BLANKS       @@ANAM
     C                   ENDIF
     C**** END ADD *** 3/05/07 ***********************************
     C***  BEGIN ADD  ***  07/19/99  *********************************
     C                   TIME                    DSDTTM
     C*
     C                   CLEAR                   XXPLAN
     C*
     C     Q1PRMS        IFGE      12
     C                   MOVE      @@PLAN        XXPLAN
     C                   CLEAR                   @@PLAN
     C                   ENDIF                                                  Q1PRMS>=12
     C*
     C                   CLEAR                   X8DATE
     C*
     C     Q1PRMS        IFGE      13
     C                   Z-ADD     @@DATE        X8DATE
     C                   ENDIF                                                  Q1PRMS>=13
     C*
     C     XXPLAN        IFEQ      '*'
     C     X8DATE        ANDEQ     *ZERO
     C                   Z-ADD     DSDATE        X8DATE
     C                   ENDIF                                                  XXPLAN=*
     C*
     C                   Z-ADD     X8DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7DATE
     C***  END   ADD  ***  07/19/99  *********************************
     C*
     C     @@FYR         IFLE      *ZERO
     C                   MOVE      *HIVAL        @@FYR
     C                   ENDIF                                                  @@FYR<=*ZERO
     C*
     C*  GET AGENT RECORDS
     C*
     C***  BEGIN ADD  ***  07/19/99  *********************************
     C                   SELECT
     C     XXPLAN        WHENEQ    '*'
     C     @@FYR         ANDNE     *HIVAL
     C     AHKY2         SETGT     PAH01
     C                   OTHER
     C***  END   ADD  ***  07/19/99  *********************************
     C     AHKY          SETGT     PAH01
     C***  BEGIN ADD  ***  07/19/99  *********************************
     C                   ENDSL
     C***  END   ADD  ***  07/19/99  *********************************
     C*
     C     AHKY1         READPE    PAH01                                  60
     C*
      *** beg chg ***** 09/21/16 *************************************
     C******IN60*        DOWEQ     *OFF
     C*****AHSTAT        ANDEQ     'I'
     C***********        READP     PAH01                                  60
     C***********        ENDDO                                                  *IN60=*OFF/AHSTAT=I
      /free
       Select;
       When @@agt# = 0;
          Dow Not(%Eof) and AHstat = 'I';
             Readp pah01;
          Enddo;
          *In60 = %Eof;
       Other;
          Dow Not(%Eof) and AHagt# <> @@agt#;
             Readpe ahky1 pah01;
          Enddo;
          *In60 = %Eof;
       Endsl;
      /end-free
      *** end chg ***** 09/21/16 *************************************
     C*
     C                   MOVE      'N'           ASSIGN
     C*
     C     *IN60         IFEQ      *OFF
     C     @@CO#         ANDEQ     AHCO#
     C     @@FND         ANDEQ     AHFND
     C     @@EMP#        ANDEQ     AHEMP#
     C     @@DIV         ANDEQ     AHDIV
     C*
     C                   MOVE      'Y'           ASSIGN            1
     C                   Z-ADD     AHAGT#        @@AGT#
     C*
     C     Q1PRMS        IFGE      8
     C                   Z-ADD     AHPROD        @@PROD
     C                   ENDIF                                                  Q1PRMS>=8
     C*
     C***  Begin Add  ***  12/21/09  **************************************
     C     Q1PRMS        IFGE      13
     C                   Z-ADD     AHEFDT        @@DATE
     C                   ENDIF                                                  Q1PRMS>=13
     C***  End   Add  ***  12/21/09  **************************************
     C*
     C                   Z-ADD     AHEFDT        XXEFDT
     C                   Z-ADD     AHRT#         XXRT#
     C                   Z-ADD     AHAGRC        XXAGRC
     C***  BEGIN ADD  ***  07/19/99  *********************************
     C     Q1PRMS        IFGE      18
     C                   MOVEL     AHCAF1        @@CAFL
     C                   ENDIF
     C***  END   ADD  ***  07/19/99  *********************************
     C*
     C***  BEGIN ADD  ***  07/19/99  *********************************
     C     XXPLAN        IFEQ      '*'
     C*
     C     ARKY1         SETGT     PAR01
     C     ARKY          READPE    PAR01                                  60
     C*
     C***  BEGIN ADD  ***  08/10/00  *********************************
     C     *IN60         DOWEQ     *OFF
     C     ARSTAT        ANDEQ     'I'
     C     ARKY          READPE    PAR01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C***  END   ADD  ***  08/10/00  *********************************
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     ARPLAN        @@PLAN
     C                   ENDIF                                                  *IN60=*OFF
     C*
      ***** BEG ADD ******* 10/15/01 **********************************
     C     Q1PRMS        IFGE      14
     C                   Z-ADD     ARNWRT        @@NWRT                         New Rate
     C                   ENDIF                                                  Q1PRMS>=14
      *
     C     Q1PRMS        IFGE      15
     C                   Z-ADD     ARRNRT        @@RNRT                         Renew Rate
     C                   ENDIF                                                  Q1PRMS>=15
      *
     C***  Begin Add  ***  05/01/06  **************************************
     C     Q1PRMS        IFGE      16
     C                   Z-ADD     ARRT#         @@RT#
     C                   ENDIF                                                  Q1PRMS>=16
     C***  End   Add  ***  05/01/06  **************************************
      *
      *
      ***** END ADD ******* 10/15/01 **********************************
     C                   ENDIF                                                  XXPLAN=*
     C***  END   ADD  ***  07/19/99  *********************************
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C     @@EMP#        IFEQ      *ZERO
     C                   MOVE      'Y'           ASSIGN
     C                   ENDIF                                                  @@EMP#=*ZERO
     C*
     C     ASSIGN        IFEQ      'Y'
     C*
     C     AAKY          CHAIN     PAA01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     AAFEIN        XXFEIN
     C                   MOVEL     AAMOD         XXMOD
     C                   ELSE
     C                   MOVE      *ZEROS        XXFEIN
     C                   MOVE      *ZEROS        XXMOD
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C     AGKY          CHAIN     PAG01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     AGNAME        @@ANAM
     C                   MOVEL     AGMAD1        XXMAD1
     C                   MOVEL     AGPAD1        XXPAD1
     C*
     C     Q1PRMS        IFGT      9
     C                   Z-ADD     AGARE1        @@AREA
     C                   Z-ADD     AGPHN1        @@PHON
     C                   ENDIF                                                  Q1PRMS>9
     C*
     C                   CLEAR                   ZIP
     C                   MOVEL     AGMZIP        XXZIP1
     C                   MOVE      AGMZIP        XXZIP2
     C     XXZIP2        IFEQ      *ZEROS
     C                   MOVEL     *BLANKS       XXZIP2
     C                   ELSE
     C                   MOVEL     '-'           DASH
     C                   ENDIF                                                  XXZIP2=*ZEROS
     C*
     C                   MOVEL     *BLANKS       XXMCSZ           60
     C     AGMCTY        CAT       ',':0         XXMCSZ
     C     XXMCSZ        CAT       AGMST:1       XXMCSZ
     C     XXMCSZ        CAT       ZIP:2         XXMCSZ
     C*
     C                   CLEAR                   ZIP
     C                   MOVEL     AGPZIP        XXZIP1
     C                   MOVE      AGPZIP        XXZIP2
     C     XXZIP2        IFEQ      *ZEROS
     C                   MOVEL     *BLANKS       XXZIP2
     C                   ELSE
     C                   MOVEL     '-'           DASH
     C                   ENDIF                                                  XXZIP2=*ZEROS
     C*
     C                   MOVEL     *BLANKS       XXPCSZ           60
     C     AGPCTY        CAT       ',':0         XXPCSZ
     C     XXPCSZ        CAT       AGPST:1       XXPCSZ
     C     XXPCSZ        CAT       ZIP:2         XXPCSZ
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       @@ANAM
     C                   MOVE      *BLANKS       XXMAD1
     C                   MOVE      *BLANKS       XXMCSZ
     C                   MOVE      *BLANKS       XXPAD1
     C                   MOVE      *BLANKS       XXPCSZ
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        @@AGT#
     C                   MOVEL     *BLANKS       @@ANAM
     C*
     C     Q1PRMS        IFGE      8
     C                   Z-ADD     *ZEROS        @@PROD
     C                   ENDIF                                                  Q1PRMS>=8
     C*
     C                   ENDIF                                                  ASSIGN=Y
     C*
     C     Q1PRMS        IFGE      9
     C*
     C*  GET PRODUCER RECORDS
     C*
     C                   MOVEL     *BLANKS       @@PNAM
     C*
     C     APKY          CHAIN     PAP01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     APLSN         CAT       ',':0         @@PNAM
     C     @@PNAM        CAT       APFSN:1       @@PNAM
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  Q1PRMS>=9
     C***  Begin Add  ***  12/20/06  **************************************
     C*
     C*  GET Producer's Email Address
     C*
     C     Q1PRMS        IFGE      17
     C                   MOVEL     *BLANKS       @@PEML
     C                   MOVEL     APEMAL        @@PEML
     C                   ENDIF                                                  Q1PRMS>=17
     C*
     C***  End   Add  ***  12/20/06  **************************************
      ***** BEG ADD ******* 01/15/10 **********************************
     C*
     C     Q1PRMS        IFGE      19
     C                   Z-ADD     AHRRN         @AHRRN
     C                   ENDIF                                                  Q1PRMS>=19
      ***** END ADD ******* 01/15/10 **********************************
     C*
     C***  Begin Add  ***  05/22/09  **************************************
     C                   CLOSE     WMAHL
     C***  End   Add  ***  05/22/09  **************************************
     C*** START ADD *** 3/05/07 ***************************************
     C     X@LR          IFEQ      'LR'
     C                   MOVE      *ON           *INLR
     C                   ENDIF
     C*** END ADD *** 3/05/07 *****************************************
     C                   RETURN
     C*
