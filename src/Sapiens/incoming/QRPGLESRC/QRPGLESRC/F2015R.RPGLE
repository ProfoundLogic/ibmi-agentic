     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      *******************************************************************
      /TITLE F2015R - PROCEDURE CODE SELECTION - 2
      *******************************************************************
      *  Description:  THIS PROGRAM WILL DISPLAY A WINDOW TO ALLOW      *
      *                THE USER TO SELECT WHETHER TO GET THE PROCEDURE
      *                CODE FROM THE PROCEDURE CODE FILE FOR THE STATE
      *                OR THE REASONABLE & CUSTOMARY FILE.
      *                                                                - -
      *  Called From:  N91DER AND N91D7R                                *
      *                                                                 *
      *  Calls To   :  F2305R - PROCEDURE CODE SEARCH                   *
      *                F2010R - ICD9 PROCEDURE CODE SEARCH              *
      *                                                                 *
      *  Created    :   9/20/00  C:62  Project:  NW068                  *
      *******************************************************************
      *  Revisions:                                                     *
      *                                                                 *
      *  04/09/2007 C:62 PROJECT 25MOD                                  *
      *                  ADD PARM TO DEFAULT VALUE.                     *
      *                                                                 *
      *******************************************************************
      *  Indicators:                                                    *
      *                                                                 *
      *    99 - General File Access                                     *
      *******************************************************************
     FF2015F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FFMPRP     IF   E             DISK    USROPN
     FFMPRL     IF   E           K DISK    USROPN
     F                                     RENAME(PPR01:PPR02)
     FFMPRL1    IF   E           K DISK    USROPN
     F                                     RENAME(PPR01:PPR03)
      *

     D QCA             S             56    DIM(6) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D*
     D                 DS
     D  @@CMD                  1     56
     D  @@ST                  38     39
      *
     D                 DS
     D  @@CMD2                 1     56
     D  @@ST2                 38     39
      *
     D                 DS
     D  @@CMD3                 1     56
     D  @@ST3                 40     41
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@PROC            5
     C                   PARM                    @@RTCD            1
     C*** BEGIN ADD *** 04/09/07 **************************************
     C                   PARM                    @@DFT             1
     C*** END   ADD *** 04/09/07 **************************************
     C*
     C*  PROCEDURE CODE FILE
     C*
     C     PRKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXCPCD                         ##CPT4
     C*
     C     PRKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXDSC1
      *
     C     *LIKE         DEFINE    PRCPT4        XXCPCD                         ##CPT4
     C     *LIKE         DEFINE    PRDSC1        XXDSC1                         CPT4 DESC
     C*
     C                   CALL      'FA007R'
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  GET ADJUDICATION STATE
      *
     C                   MOVEL     'UC'          SCPRST
      *
      * ---------------------------------------------------------------
      *
     C     *INKL         DOUEQ     *ON
      *
     C     ##CPT4        IFNE      *BLANKS
     C     ##ICD9        ORNE      *BLANKS
     C                   LEAVE
     C                   END
     C*
     C                   MOVEL     '1'           SCPROF
     C*
      *
     C*** BEGIN ADD *** 04/09/07 **************************************
     C     Q1PRMS        IFGT      4
     C     @@DFT         ANDEQ     *BLANKS
     C     Q1PRMS        OREQ      4
     C*** END   ADD *** 04/09/07 **************************************
     C                   EXFMT     WDW01
     C*** BEGIN ADD *** 04/09/07 **************************************
     C                   ELSE
     C                   SELECT
     C     @@DFT         WHENEQ    '1'
     C                   MOVEL     '1'           SCPROF
     C     @@DFT         WHENEQ    '2'
     C                   MOVEL     '1'           SCICDF
     C                   ENDSL
     C                   ENDIF
     C*** END   ADD *** 04/09/07 **************************************
     C                   MOVEA     '000'         *IN(71)
      *
     C     *INKL         IFEQ      *OFF
      *
     C                   SELECT
      *
     C     SCPROF        WHENEQ    '1'
     C     SCICDF        ANDEQ     '1'
     C                   MOVE      *ON           *IN71                          SELECTED BOTH
      *
     C     SCPROF        WHENNE    '1'
     C     SCPROF        ANDNE     *BLANK
     C                   MOVE      *ON           *IN72                          INVALID CODE
      *
     C     SCICDF        WHENNE    '1'
     C     SCICDF        ANDNE     *BLANK
     C                   MOVE      *ON           *IN73                          INVALID CODE
      *
      * PROCEDURE CODES
      *
     C     SCPROF        WHENEQ    '1'
     C*
     C*   CLOSE FILE IF OPENED
     C*
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FMPRP
     C                   CLOSE     FMPRL
     C                   CLOSE     FMPRL1
     C                   MOVE      ' '           OPNFLG            1
     C                   END                                                    END OPNFLG=1
     C*
     C*   DELETE OVERRIDE
     C*
     C     OVRFLG        IFEQ      '1'
     C*
     C*   DELETE OVERRIDE TO FMPRP, FMPRL & FMPRL1
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH
     C*
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH
     C*
     C                   MOVE      ' '           OVRFLG            1
     C                   END                                                    END OVRFLG=1
     C*
     C     OVRFLG        IFEQ      ' '
     C*
     C*   OVERRIDE TO FMPRP, FMPRL & FMPRL1
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(2)        @@CMD
     C                   MOVEL     SCPRST        @@ST
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH
     C*
     C                   MOVEL     QCA(4)        @@CMD2
     C                   MOVEL     SCPRST        @@ST2
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD2
     C                   PARM                    LENGTH
     C*
     C                   MOVEL     QCA(6)        @@CMD3
     C                   MOVEL     SCPRST        @@ST3
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD3
     C                   PARM                    LENGTH
     C*
     C                   MOVE      '1'           OVRFLG            1
     C                   END                                                    END OVRFLG=0
     C*
     C*   OPEN FILE FOR PROCESSING
     C*
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FMPRP
     C                   OPEN      FMPRL
     C                   OPEN      FMPRL1
     C                   MOVE      '1'           OPNFLG
     C                   END                                                    END OPNFLG= ' '
     C*
     C                   CALL      'F2305R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      SCPRST        ##ST              2
     C                   PARM      *BLANKS       ##CPT4           15
     C                   PARM      *BLANKS       ##DESC           40
     C*
     C                   MOVE      *BLANK        SCPROF
     C*
     C     ##CPT4        IFEQ      *BLANKS
     C     *INKL         ANDEQ     *OFF
     C*
     C                   MOVEL     *ON           *INKL
     C*
     C                   ENDIF
      *
      * REASONABLE & CUSTOMARY SEARCH
      *
     C     SCICDF        WHENEQ    '1'
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##ICD9            5
     C                   PARM      *BLANKS       ##LOST            1
     C*
     C     ##ICD9        IFEQ      *BLANKS
     C     *INKL         ANDEQ     *OFF
     C*
     C                   MOVEL     *ON           *INKL
     C*
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        SCICDF
     C*
     C                   ENDSL
      *
      * CLEAR 'SEARCHING' MSG
     C                   CALL      'SRRMVMSG'                           99
      *
     C                   END
      *
     C                   ENDDO
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
     C     ##CPT4        IFEQ      *BLANKS
     C     ##ICD9        ANDEQ     *BLANKS
     C                   MOVE      '1'           @@RTCD
     C                   END
      *
     C     ##CPT4        IFNE      *BLANKS
     C                   MOVEL(P)  ##CPT4        @@PROC
     C                   MOVEL(P)  'C'           @@RTCD
     C                   END
      *
     C     ##ICD9        IFNE      *BLANKS
     C                   MOVEL(P)  ##ICD9        @@PROC
     C                   MOVEL(P)  'I'           @@RTCD
     C                   END
      *
      *  THE FOLLOWING STMT IS USED TO SATISFY THE COMPILER;
      *  FMPRP & FMPRL1 MUST BE REFERENCED BY AN INPUT SPEC.
      *  A CONDITION IS USED TO KEEP FROM ACCESSING THESE
      *  INPUT OPERATIONS, SINCE THESE FILES MAY BE CLOSED.
      *
     C*
     C     *IN99         IFEQ      *ON
     C     *IN99         ANDEQ     *OFF
     C*
     C     PRKY          SETLL     FMPRL
     C     PRKY2         SETLL     FMPRL1
     C     1             CHAIN     PPR01                              99
     C*
     C                   ENDIF
      *
     C                   MOVE      *ON           *INLR
     C                   RETURN
**  QCMDEXC TABLE
DLTOVR FILE(FMPRP)
OVRDBF FILE(FMPRP) TOFILE(*LIBL/FMPRP  )
DLTOVR FILE(FMPRL)
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
DLTOVR FILE(FMPRL1)
OVRDBF FILE(FMPRL1) TOFILE(*LIBL/FMPRL1  )
