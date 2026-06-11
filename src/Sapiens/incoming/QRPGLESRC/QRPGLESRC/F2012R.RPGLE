     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      * *****************************************************************
      /TITLE F2012R - DIAGNOSIS CODE SELECTION
      * *****************************************************************
      *  Description:  THIS PROGRAM WILL DISPLAY A WINDOW TO ALLOW      *
      *                THE USER TO SELECT WHETHER TO GET THE DIAGNOSIS
      *                CODE FROM THE CLAIM'S DIAGNOSIS CODES OR
      *                THE ICD9 FILE.
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *                                                                 *
      *  Called From:  U0060R01                                         *
      *                                                                 *
      *  Calls To   :  F0275R - CLAIM'S DIAGNOSIS CODES                 *
      *                F2010R - ICD9 DIAGNOSIS FILE                     *
      *                                                                 *
      *  Created    :   1/15/98  C:47  Project:  87MCB                  *
      *                                                                 *
      * *****************************************************************
      *  Revisions:                                                     *
      *                                                                 *
      *  01/29/15 C:282 Support:WA025 32758                             *
      *                 Added the q1prms handling in the program.       *
      *                                                                 *
      *  08/05/13 C:277 PROJECT:MC329 Phase 2                           *
      *                 Add ICD10 Fuctionality                          *
      *                                                                 *
      *  05/25/00 C:62 PROJECT:MC205
      *                DEFAULT SELECTION TO ICD9 CODE.
      *                                                                 *
      *  07/01/99 C:62 PROJECT:MC207
      *                KEEP FROM MAKING A RECURSIVE CALL ON F0275R.
      *                                                                 *
      * *****************************************************************
      *  Indicators:                                                    *
      *                                                                 *
      *    60 - General File Access                                     *
      * *****************************************************************
     FF2012F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
      *

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D*** Beg Add *** 08/05/13 ****************************************
     D xxicd10         s              1
     D*** End Add *** 08/05/13 ****************************************
     D*** Beg Add *** 01/29/15 ****************************************
     D @@Icd10         s              1
     D*** End Add *** 01/29/15 ****************************************
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

     C     *ENTRY        PLIST
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@ICD9
     C                   PARM                    @@LOST
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@RRN2            9 0          RRN FMCMP
     C*** Beg Add *** 08/05/13 ****************************************
     C*** Beg CHG *** 01/29/15 ****************************************
     C*****              PARM                    XXICD10
     C                   PARM                    @@Icd10
     C*** End CHG *** 01/29/15 ****************************************
      *
      *** Beg Add *** 01/29/15 ****************************************
      /Free
       Clear xxIcd10;
       If q1Prms > 7;
          xxIcd10 = @@Icd10;
       EndIf;
      /End-Free
      *** End Add *** 01/29/15 ****************************************
     C                   MOVEL     Q1PGM         SCPGM
     C                   MOVEL     '1'           SCICD9
     C                   SETON                                        31
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1';
          *in71 = *on;
       Else;
          *in71 = *off;
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
      *
     C     *INKL         DOUEQ     *ON
      *
     C     @@ICD9        IFNE      *BLANK
     C     @@RRN         ORNE      *ZERO
     C                   LEAVE
     C                   END
      *
     C                   EXFMT     WDW01
     C                   MOVEA     '000'         *IN(71)
     C                   MOVEA     '0'           *IN(31)
      *
     C     *INKL         IFEQ      *OFF
      *
     C                   SELECT
      *
     C     SCCDC         WHENEQ    '1'
     C     SCICD9        ANDEQ     '1'
     C                   MOVE      *ON           *IN71                          SELECTED BOTH
      *
     C     SCCDC         WHENNE    '1'
     C     SCCDC         ANDNE     *BLANK
     C                   MOVE      *ON           *IN72                          INVALID CODE
      *
     C     SCICD9        WHENNE    '1'
     C     SCICD9        ANDNE     *BLANK
     C                   MOVE      *ON           *IN73                          INVALID CODE
      *
      * CLAIM'S DIAGNOSIS CODES
      *
     C     SCCDC         WHENEQ    '1'
     C*
     C* CHECK TO SEE IF F0275R IS ON THE CALL STACK.
     C*
     C*
     C                   CALL      'SRCHKPGM'                           99
     C                   PARM      'F0275R'      XXPGM            10
     C                   PARM      *BLANKS       XXERR             1
     C*
     C                   SETOFF                                       16
     C                   MOVEL     *BLANKS       ##RTN
     C*
     C* IF IT IS ON THE STACK THEN CREATE A DUPLICATE IN QTEMP
     C*
     C     XXERR         IFEQ      '1'
     C     SCCDC         ANDEQ     '1'
     C*
     C                   CALL      'FA711C'
     C                   PARM                    ##RTN             1
     C                   PARM      'F0275R'      ##FNM            10
     C                   PARM      'F0275Q'      ##TNM            10
     C*
     C                   SETON                                        16
     C*
     C                   ENDIF
     C*
     C     *IN16         IFEQ      *OFF
     C                   CALL      'F0275R'
     C                   PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      'S'           @@CALL            1
     C                   PARM                    @@RRN             9 0          RRN FADAP
     C                   PARM                    @@RRN2            9 0          RRN FMCMP
     C                   MOVE      *BLANK        SCCDC
     C*
     C                   ELSE
     C*
     C     ##RTN         IFEQ      *BLANKS
     C*
     C                   CALL      'F0275Q'
     C                   PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      'S'           @@CALL            1
     C                   PARM                    @@RRN             9 0          RRN FADAP
     C                   PARM                    @@RRN2            9 0          RRN FMCMP
     C                   MOVE      *BLANK        SCCDC
     C*
     C                   ENDIF
     C*
     C                   ENDIF
      *
      * ICD9 DIAGNOSIS FILE
      *
     C     SCICD9        WHENEQ    '1'
     C*** Beg Add *** 08/05/13 ****************************************
     C                   If        xxicd10 = '1'
     C                   CALL      'F2554R'
     C                   PARM      '2'           @@CALL
     C                   PARM                    @@KC
     C     *INKL         PARM                    @@KL
     C                   PARM                    @@ICD9
     C                   PARM                    @@LOST
     C                   Else
     C*** End Add *** 08/05/13 ****************************************
     C                   CALL      'F2010R'
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C*** Beg Chg *** 08/05/13 ****************************************
     C********           PARM                    @@ICD9            5
     C                   PARM                    @@ICD9            7
     C*** End Chg *** 08/05/13 ****************************************
     C                   PARM                    @@LOST            1
     C*** Beg Add *** 08/05/13 ****************************************
     C                   Endif
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVE      *BLANK        SCICD9
     C*
     C                   ENDSL
      *
      * CLEAR 'SEARCHING' MSG
     C                   CALL      'SRRMVMSG'                           99
      *
     C                   END                                                    *INKL
      *
     C                   ENDDO
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
     C     @@ICD9        IFEQ      *BLANK
     C     @@RRN         ANDEQ     *ZERO
     C                   MOVE      '1'           @@RTCD
     C                   END
      *
     C                   MOVE      *ON           *INLR
     C                   RETURN
