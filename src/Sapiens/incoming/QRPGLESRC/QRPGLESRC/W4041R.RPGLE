     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W4041R - PRINT AGENT LABELS
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  07/24/08 C:61 PROJECT: 46WGC                                 *
     H*                Enable program to be called from Mass CX       *
     H*                process.                                       *
     H*                                                               *
     H*  03/23/03 C:09 PROJECT: WC1124                                *
     H*                LABELS NOW PRINT @ 15 CPI AND WILL NOW PRINT   *
     H*                THE FULL NAME AND ADDRESS                      *
     H*                                                               *
     H*  12/05/97 C:85 PROJECT: WC577                                 *
     H*                DELETED "AMM" INITIALS FROM COMMENTS.          *
     H*                                                               *
     H*  10/30/97 C:85 PROJECT: WC577                                 *
     H*                VERSION 3 REPORT STANDARDS.                    *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
      ***** BEG ADD ******* 07/24/08 **********************************
      *
      * Mass CX work file
     FWXCNL7    IF   E           K DISK
      ***** END ADD ******* 07/24/08 **********************************
     FWMAGL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAPL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
     FQSYSPRT1  O    F  132        PRINTER OFLIND(*INOF)

     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D                 DS
     D  AGMZIP                 1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQAGT#            5
     C                   PARM                    QQALL             1
     C                   PARM                    QQCMFL            1
     C                   PARM                    QQMLFL            1
     C                   PARM                    QQPRFL            1
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQPHON            1
     C                   PARM                    QQPDFL            1
      ***** BEG ADD ******* 07/24/08 **********************************
     C                   PARM                    QQBAT#            9
      ***** END ADD ******* 07/24/08 **********************************
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQAGT#        XXAGT#            5 0
     C                   MOVE      QQALL         XXALL             1
     C                   MOVE      QQCMFL        XXCMFL            1
     C                   MOVE      QQMLFL        XXMLFL            1
     C                   MOVE      QQPRFL        XXPRFL            1
     C                   MOVE      QQTYPE        XXTYPE            1
     C                   MOVE      QQPHON        XXPHON            1
     C                   MOVE      QQPDFL        XXPDFL            1
      ***** BEG ADD ******* 07/24/08 **********************************
     C                   MOVE      QQBAT#        XXBAT#            9 0
      ***** END ADD ******* 07/24/08 **********************************
     C*
     C     XXPHON        COMP      'Y'                                    26
     C*
     C     XXTYPE        IFEQ      'L'
     C                   MOVE      '1'           *IN25
     C                   ELSE
     C                   MOVE      '0'           *IN25
     C                   END
      ***** BEG ADD ******* 07/24/08 **********************************
      *
      *
      *============================================================
      * KLIST
      *============================================================
      *
      * Mass CX Work File
      *
     C     CNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXBAT#
      ***** END ADD ******* 07/24/08 **********************************
     C*
     C*   KEY LIST FOR THE AGENT ASSIGNMENT MASTER FILE
     C*
     C     AAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     AAKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#
     C*
     C*   KEY LIST FOR THE AGENT MASTER FILE
     C*
     C     AGKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C*   KEY LIST FOR THE AGENT ASSIGNMENT MASTER FILE
     C*
     C     APKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
      ***** BEG ADD ******* 07/24/08 **********************************
      *
     C     XXBAT#        IFNE      *ZERO
     C                   Z-ADD     *ZERO         SVAGT#            5 0
     C     CNKY          SETLL     PCN01
     C                   EXSR      NXTAGT
      *
     C                   ELSE
      *
      ***** END ADD ******* 07/24/08 **********************************
     C*
     C*   START READING THE AGENT ASSIGNMENT MASTER FILE
     C*
     C     XXAGT#        IFEQ      0
     C     AAKEY         CHAIN     PAA01                              60
     C                   MOVE      '1'           *IN12
     C                   ELSE
     C     AAKEY1        CHAIN     PAA01                              60
     C                   MOVE      '0'           *IN12
     C                   END
      ***** BEG ADD ******* 07/24/08 **********************************
      *
     C                   ENDIF
      ***** END ADD ******* 07/24/08 **********************************
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C*   DETERMINE IF THE AGENTS PRODUCERS SHOULD BE PRINTED
     C*
     C                   MOVE      'N'           XXPRNT            1
     C*
     C*
     C*** BEGIN ADD *** 10/30/97 **************************************
     C*
     C     XXALL         IFEQ      'Y'
     C     XXAGT#        ORNE      0
     C                   MOVE      'Y'           XXPRNT
     C*
     C                   ELSE
     C*
     C*** END   ADD *** 10/30/97 **************************************
     C*
     C*
     C     XXCMFL        IFEQ      'Y'
     C     AACMFL        ANDEQ     'Y'
     C                   MOVE      'Y'           XXPRNT
     C                   END
     C     XXMLFL        IFEQ      'Y'
     C     AAMLFL        ANDEQ     'Y'
     C                   MOVE      'Y'           XXPRNT
     C                   END
     C     XXPRFL        IFEQ      'Y'
     C     AAPRFL        ANDEQ     'Y'
     C                   MOVE      'Y'           XXPRNT
     C                   END
     C*
     C*
     C*** BEGIN ADD *** 10/30/97 **************************************
     C*
     C                   END
     C*
     C*** END   ADD *** 10/30/97 **************************************
     C*
     C*
     C     XXPRNT        IFEQ      'Y'
     C     AASTAT        ANDEQ     'A'
     C*
     C*   GET THE AGENT MASTER FILE
     C*
     C     AGKEY         CHAIN     PAG01                              60
     C     *IN60         IFEQ      '0'
     C     AGSTAT        ANDEQ     'A'
     C*
     C*   MOVE NAME AND ADDRESS INTO SMALLER FIELDS
     C*
     C                   MOVE      *BLANKS       XXNAME
     C                   MOVE      *BLANKS       XXADDR
     C*** BEGIN ADD *** 03/23/03 **************************************
     C*********************MOVELAGNAME    XXNAME 30
     C*********************MOVELAGMAD1    XXADDR 30
     C                   MOVEL     AGNAME        XXNAME           40
     C                   MOVEL     AGMAD1        XXADDR           40
     C*** END ADD ***** 03/23/03 **************************************
     C*
     C*   LOAD CITY, STATE, & ZIP FOR THE MAILING ADDRESS
     C*
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     AGMST         XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     AGMCTY        XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C*** BEGIN CHG *** 03/23/03 **************************************
     C*********************MOVELXXLAST    XXMCSZ 30
     C                   MOVEL     XXLAST        XXMCSZ           40
     C*** END CHG ***** 03/23/03 **************************************
     C*
     C     *IN26         IFEQ      '1'
     C                   MOVE      *BLANKS       XXPHNO           10 0
     C                   MOVE      AGPHN1        XXPHNO
     C                   MOVEL     AGARE1        XXPHNO
     C                   END
     C*   GENERATE PRODUCER LABEL
     C                   MOVE      *BLANKS       XXPNME
     C     XXPDFL        IFEQ      'Y'
     C*
     C*   GET ALL PRODUCERS FOR THE AGENT & PRINT LABEL
     C*
     C     APKEY         CHAIN     PAP01                              60
     C     *IN60         DOWEQ     '0'
     C     APSTAT        IFEQ      'A'
     C                   MOVE      *BLANKS       XXPNME
     C                   MOVEL     APLSN         QQLAST           56
     C                   MOVEL     APFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        XXPNME           25
     C  N25              EXCEPT    REG
     C   25              EXCEPT    LOGO
     C                   END
     C     APKEY         READE     PAP01                                  60
     C                   END
     C                   ELSE
     C  N25              EXCEPT    REG
     C   25              EXCEPT    LOGO
     C                   END
     C*
     C                   END
     C                   END
     C*
      ***** BEG ADD ******* 07/24/08 **********************************
      *
     C     XXBAT#        IFNE      *ZERO
     C                   EXSR      NXTAGT
      *
     C                   ELSE
      *
      ***** END ADD ******* 07/24/08 **********************************
     C   12AAKEY         READE     PAA01                                  60
     C  N12AAKEY1        READE     PAA01                                  60
      ***** BEG ADD ******* 07/24/08 **********************************
     C                   ENDIF
      *
      ***** END ADD ******* 07/24/08 **********************************
     C                   END
     C*
     C                   MOVE      '1'           *INLR
      ***** BEG ADD ******* 07/24/08 **********************************
      *
      *
      *============================================================
      * NXTAGT - Get next Agent Assignment Record.
      *============================================================
     C     NXTAGT        BEGSR
      *
     C     *IN60         DOUEQ     *ON
     C     XXAGT#        ORNE      SVAGT#
     C     XXAGT#        ANDNE     *ZERO
     C     CNKY          READE     PCN01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C                   CALL      'WA002R'
     C                   PARM      CNCO#         #1CO#             3 0
     C                   PARM      CNFND         #1FND             3 0
     C                   PARM      CNEMP#        #1EMP#            9 0
     C                   PARM      CNDIV         #1DIV             5 0
     C                   PARM      CNFYR         #1FYR             3 0
     C     XXAGT#        PARM      *ZERO         #1AGT#            5 0
     C                   PARM                    #1ANAM           60
      *
     C     AAKEY1        CHAIN     PAA01                              61
     C   61              ITER
     C                   ENDIF
      *
     C                   ENDDO
      *
     C                   Z-ADD     XXAGT#        SVAGT#
     C                   ENDSR                                                  NXTAGT
      ***** END ADD ******* 07/24/08 **********************************
     C*
     C*** BEGIN CHG *** 03/23/03 **************************************
     OQSYSPRT   E            REG            1
     O*************************XXPNME    25
     O*************************AAAGT#Z   31
     O                       XXPNME              35
     O                       AAAGT#        Z     41
     O          E            REG            1
     O*************************XXNAME    30
     O                       XXNAME              40
     O          E            REG            1
     O*************************XXADDR    30
     O                       XXADDR              40
     O          E            REG            1
     O*************************XXMCSZ    30
     O                       XXMCSZ              40
     O          E            REG            2
     O*****************26******XXPHNO    30 '0(   )&   -    '
     O               26      XXPHNO              40 '0(   )&   -    '
     O*
     OQSYSPRT1  E            LOGO           1 13
     O*************************XXPNME    25
     O*************************AAAGT#Z   31
     O                       XXPNME              35
     O                       AAAGT#        Z     41
     O          E            LOGO           1
     O*************************XXNAME    30
     O                       XXNAME              40
     O          E            LOGO           1
     O*************************XXADDR    30
     O                       XXADDR              40
     O          E            LOGO           1
     O*************************XXMCSZ    30
     O                       XXMCSZ              40
     O          E            LOGO           2
     O*****************26******XXPHNO    30 '0(   )&   -    '
     O               26      XXPHNO              40 '0(   )&   -    '
     O*** END ADD ***** 03/23/03 **************************************
     O*
