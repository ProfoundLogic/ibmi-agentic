     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F8048R - Paid Date File Program
     H*
     H*****************************************************************
     H*  Description - Populates the paid Date File (FAU4P)           *
     H*                                                               *
     H*  Called from: F8048C - Paid Date File-Override Pgm            *
     H*  Calls to   : None                                            *
     H*                                                               *
     H*****************************************************************
     H*  Created on 08/28/2006 by c:53 for Project 132WFP             *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*****************************************************************
     FFAEAL17   IF   E           K DISK
     FFAEOL4    UF   E           K DISK
     FFAU4P     O    E             DISK
     FFAU5P     O    E             DISK
     F*
      /COPY SCOPYWW01D
     D*
     D***PY SCOPYWW01I
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  CLM#                   1     13
     D  XXFND                  1      3  0
     D  XXFYR                  4      6  0
     D  XXCASE                 7     13  0
     D                 DS
     D  XFPAID                 1     11  0
     D  XFDOLR                 1      9  0
     D  XFCENT                10     11  0
     D                 DS
     D  XTPAID                 1     10
     D  XTDOLR                 1      7
     D  XTDECP                 8      8
     D  XTCENT                 9     10

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@DATE            8
     C                   PARM                    @@DCID           12
     C                   PARM                    @@DCIW           12            WELLPOINT
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*YYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*MDYY'       SRFFMT            8
     C                   PARM      '*MDY'        SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     EOKEY         KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*
     C*================================================================
     C*  LIKE defines
     C*================================================================
     C*
     C*
     C* EXECUTE MAIN SUBROUTINE
     C*
     C                   EXSR      MAINSR
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*
     C*================================================================
     C* MAINSR - MAIN SUBROUTINE
     C*================================================================
     C     MAINSR        BEGSR
     C*
     C* Let's start reading thru these checks
     C*
     C                   Z-ADD     *ZEROS        COUNT4            9 0
     C                   Z-ADD     *ZEROS        COUNT5            9 0
     C*
     C                   READ      PEA01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     EOKEY         CHAIN     PEO01                              98
     C*
     C     *IN98         IFEQ      *OFF
     C     EOTAPE        ANDNE     'S'
     C*
     C                   SELECT
     C     EONTWK        WHENEQ    'COMP '
     C                   EXSR      WRTU4
     C*
     C     EONTWK        WHENEQ    'BC   '
     C                   EXSR      WRTU5
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C                   READ      PEA01                                  99
     C                   ENDDO
     C*
     C                   EXSR      PGMEND
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of Program Processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*** DOC ID FOR FAU4P - COMP IQ
     C*
     C                   MOVEL     *BLANKS       @@DCID
     C     COUNT4        IFGT      *ZEROS
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         DATE              6
     C     'PC'          CAT(P)    DATE:0        @@DCID
     C                   CAT       '.DAT':0      @@DCID
     C                   ENDIF
     C*
     C*** DOC ID FOR FAU5P - WELLPOINT
     C*
     C                   MOVEL     *BLANKS       @@DCIW
     C     COUNT5        IFGT      *ZEROS
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         DATE              6
     C     'PW'          CAT(P)    DATE:0        @@DCIW
     C                   CAT       '.TXT':0      @@DCIW
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTU4 - Write the Data Record
     C*================================================================
     C     WRTU4         BEGSR
     C*
     C                   CLEAR                   PU401
     C*
     C                   MOVEL(P)  EASINV        U4TRID
     C                   MOVEL     EAWCPD        U4WCPD
     C                   MOVE      EAPAID        XFPAID
     C*** TRANSLATE
     C                   MOVE      XFDOLR        XTDOLR
     C                   MOVE      '.'           XTDECP
     C                   MOVE      XFCENT        XTCENT
     C                   MOVE      XTPAID        U4PAID
     C*
     C                   ADD       1             COUNT4
     C*
     C                   WRITE     PU401
     C*
     C                   MOVE      'S'           EOTAPE
     C                   UPDATE    PEO01
     C*
     C                   ENDSR
     C*================================================================
     C* WRTU5 - Write the Data Record
     C*================================================================
     C     WRTU5         BEGSR
     C*
     C                   CLEAR                   PU501
     C*
     C                   MOVEL(P)  EASINV        U5TRID
     C                   Z-ADD     EAPAID        U5PAID
     C*
     C                   MOVEL     EAWCPD        U5WCPD
     C*
     C                   MOVE      EACO#         #1CO#
     C                   MOVE      EASUB#        #1SUB#
     C                   MOVE      EAFND         #1FND
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM      *BLANKS       #1WCF#            3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C*
     C                   MOVEL     #1WCF#        XXFND
     C                   MOVEL     EAFYR         XXFYR
     C                   MOVEL     EACASE        XXCASE
     C*
     C                   MOVEL(P)  CLM#          U5CLM#
     C                   MOVEL(P)  'CWST'        U5CO#
     C                   MOVEL     'B'           U5TYPE
     C                   MOVEL     'N'           U5CYCL
     C*
     C                   ADD       1             COUNT5
     C*
     C                   WRITE     PU501
     C*
     C                   MOVE      'S'           EOTAPE
     C                   UPDATE    PEO01
     C*
     C                   ENDSR
