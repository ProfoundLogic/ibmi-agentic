     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE SRDISC - CHK FOR DISCOUNT SPREAD
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                       *
     H**                                                              *
     H**      THIS PROGRAM IS CALLED BY VARIOUS BILLING PROGRAMS TO   *
     H**      DETERMINE IF THE DISCOUNT IS SPREAD ON A PARTICULAR     *
     H**      EMPLOYER.                                               *
     H**                                                              *
     H**      RETURN CODE:      'Y' MULT DIV EXIST AND DISCOUNT IS    *
     H**                            SPREAD.                           *
     H**                        'N' NO DISCOUNT SPREAD                *
     H**                                                              *
     H**                                                              *
     H* CALLED FROM:                                                  *
     H*   W8001R  - ORIGINAL BILL PROMPT                              *
     H*   W7710R  - AUDITED  BILL PROMPT                              *
     H*   W8010R  - ORIGINAL BILL PREM CALC                           *
     H*   W7720R  - AUDITED  BILL PREM CALC                           *
     H* CALLS TO:                                                     *
     H*                                                               *
     H*                                                               *
     H* PROGRAMMER: JACKIE JANOWIAK                                   *
     H* CREATED:    01/19/92            PROJECT: WC111                *
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60  GENERAL CHAIN (READ) INDICATOR                            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 07/29/15 C:65  Support: Phase#36318 (CareWest)                *
     H*                Update activiation group from QILE to *CALLER  *
     H*                as this program was not picking up file        *
     H*                overrides from the revised bill.               *
     H*                                                               *
     H* 03/16/09 C:189 SUPPORT: WA114 67474                           *
     H*                COUNTER INCORRECTLY INITIALIZED RETURNING      *
     H*                QQSPRD = 'Y' ALWAYS EVEN IT HAS ONLY 1 DIV     *
     H*                                                               *
     H* 02/28/08 C:136 SUPPORT: WA132 62410                           *
     H*                SET THE SPREAD DISC FLAG IF MULTIPLE DIVS      *
     H*                EXIST AND ONE OF THE DIVISIONS HAS A DISC      *
     H*                CODE.                                          *
     H*                                                               *
     H* 04/24/97 C:65 PROJECT: WC655                                  *
     H*               FIND FIRST EXISTING DIVISION RATHER THAN DIV 0  *
     H*               FOR SPREADING DISC.                             *
     H*****************************************************************
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SSY01)
     FWDELL1    IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  WORK COMP CONTROL FILE
     C*
     C     WCKY          KLIST
     C                   KFLD                    XCKEY
     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQSPRD            1
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    WCKEY         XCKEY
     C*
     C*    SET UP AND INITALIZE
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      'N'           QQSPRD
     C*
     C*  GET SYSTEM VALUE FOR SPREAD DISCOUNT FLAG & CONTINUE IF 'Y'
     C*
     C                   MOVE      *BLANKS       XCKEY
     C                   MOVEL     'WC'          XCKEY
     C     WCKY          CHAIN     SWC01                              60
     C     *IN60         IFEQ      '0'
     C     WCFL02        ANDEQ     'Y'
     C*
     C*  GET DISCOUNT VALUE FOR THIS EMPLR, DIV = 0
     C*
      *** BEG CHG ***** 02/28/08 **************************************
     C*********            Z-ADD0         XXDIV
     C****BEGIN CHG ***  04/24/97  ************************************
     C*********  ELKY      CHAINPEL01                60
     C*********  ELKY1     CHAINPEL01                60
     C****END*CHG *****  04/24/97  ************************************
     C*********  *IN60     IFEQ '0'
     C*********  ELDISC    ANDNE*BLANK
     C*********
     C***SEE*IF ANOTHER DIVISION EXISTS
     C*********
     C*********  ELKY1     READEPEL01                    60
     C*********  *IN60     IFEQ '0'
     C*********            MOVE 'Y'       QQSPRD
     C*********            END
     C*********
     C*********            END
      *
      *** BEG CHG ***** 03/16/09 **************************************
     C***************      Z-ADD1         CNT     70
     C                   Z-ADD     0             CNT               7 0
      *** END CHG ***** 03/16/09 **************************************
     C                   MOVE      *OFF          SDFL              1
     C     ELKY1         SETLL     PEL01
     C     ELKY1         READE     PEL01                                  60
     C     *IN60         DOWEQ     *OFF
     C                   ADD       1             CNT
     C     ELDISC        IFNE      *BLANK
     C                   MOVE      *ON           XXSDFL            1
     C                   ENDIF
     C     XXSDFL        IFEQ      *ON
     C     CNT           ANDGT     1
     C                   MOVE      'Y'           QQSPRD
     C                   LEAVE
     C                   ENDIF
     C     ELKY1         READE     PEL01                                  60
     C                   ENDDO
      *** END CHG ***** 02/28/08 **************************************
     C*
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C*
