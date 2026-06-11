     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      * *****************************************************************
      /TITLE F2017R - FILE SELECTION WINDOW
      * *****************************************************************
      *  Description:  THIS PROGRAM WILL DISPLAY A WINDOW TO ALLOW      *
      *                THE USER TO SELECT WHICH FILE TO USE FOR
      *                SEARCHING/DISPLAYING
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *                                                                 *
      *  Called From:  F0363RFL                                         *
      *                                                                 *
      *  Calls To   :  F2305R - PROC CODE SEARCH                        *
      *                F2320R - SERVICE CODE SEARCH                     *
      *                                                                 *
      *  Created    :  12/12/03  C:79  Project:  FL048                  *
      *******************************************************************
      * NOTE:                                                           *
      *    @@RTCD = "E" - FAEOP RRN PASSED IN TO THIS PROGRAM.          *
      *    @@RTCD = "C" - PROCEDURE CODE FILE WAS SELECTED.             *
      *    @@RTCD = "S" - SERVICE CODE FILE WAS SELECTED.               *
      *******************************************************************
      *  Revisions:                                                     *
      * *****************************************************************
      *  Indicators:                                                    *
      *                                                                 *
      *    99 - General File Access                                     *
      * *****************************************************************
     FF2017F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
      * Service Code file
     FFMSVP     IF   E             DISK    USROPN
      * Service Code file
     FFMSVL1    IF   E           K DISK    USROPN
     F                                     RENAME(PSV01:PSV02)
      * Service Code file
     FFMSVL2    IF   E           K DISK    USROPN
     F                                     RENAME(PSV01:PSV03)
      * CPT Code file
     FFMPRP     IF   E             DISK    USROPN
      * CPT Code file
     FFMPRL     IF   E           K DISK    USROPN
     F                                     RENAME(PPR01:PPR02)
      *

     D QCA             S             56    DIM(14) CTDATA PERRCD(1)             CMD EXEC/QCMDEXC
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
     D  @@ST2                 40     41
     D                 DS
     D  @@SVCD                 1     11
     D  XXCDE#                 4      8
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@PROC           11
     C                   PARM                    @@PROD           50
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@AST             2
     C*
     C*  SERVICE CODE FILE
     C*
     C     SVKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    XXCPCD                         ##SVCD
     C                   KFLD                    KKEFDT
     C*
     C     SVKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKDESC
     C*
     C*  CPT CODE FILE
     C*
     C     PRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCPT4
      *
     C     *LIKE         DEFINE    SVCMSV        XXCPCD
     C     *LIKE         DEFINE    SVDESC        KKDESC
     C     *LIKE         DEFINE    SVEFDT        KKEFDT
     C     *LIKE         DEFINE    SVCO#         KKCO#
     C     *LIKE         DEFINE    PRCPT4        KKCPT4
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  GET ADJUDICATION STATE
      *
     C                   MOVEL     Q1PGM         SCPGM
     C                   MOVEL     *BLANKS       @@RTCD
      *
      * ---------------------------------------------------------------
      *
     C     *INKL         DOUEQ     *ON
      *
     C     ##SVCD        IFNE      *BLANK
     C                   LEAVE
     C                   END
     C                   MOVEL     '1'           SCCPF
      *
     C                   EXFMT     WDW01
     C                   MOVEA     '000'         *IN(71)
      *
     C     *INKL         IFEQ      *OFF
      *
     C                   SELECT
      *
     C     SCSVOF        WHENEQ    '1'
     C     SCCPF         ANDEQ     '1'
      *
     C                   MOVE      *ON           *IN71                          SELECTED TWO
      *
     C     SCSVOF        WHENNE    '1'
     C     SCSVOF        ANDNE     *BLANK
     C                   MOVE      *ON           *IN72                          INVALID CODE
      *
     C     SCCPF         WHENNE    '1'
     C     SCCPF         ANDNE     *BLANK
     C                   MOVE      *ON           *IN74                          INVALID CODE
      *
      * SERVICE CODES
      *
     C     SCSVOF        WHENEQ    '1'
     C*
     C*   CLOSE FILE IF OPENED
     C*
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FMSVP
     C                   CLOSE     FMSVL1
     C                   CLOSE     FMSVL2
     C                   MOVE      ' '           OPNFLG            1
     C                   END                                                    END OPNFLG=1
     C*
     C*   DELETE OVERRIDE
     C*
     C     OVRFLG        IFEQ      '1'
     C*
     C*   DELETE OVERRIDE TO FMSVP, FMSVL1, FMSVL2
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'                              99
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*
     C                   CALL      'QCMDEXC'                              99
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH
     C*
     C                   CALL      'QCMDEXC'                              99
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH
     C*
     C                   MOVE      ' '           OVRFLG            1
     C                   END                                                    END OVRFLG=1
     C*
     C     OVRFLG        IFEQ      ' '
     C*
     C*   OVERRIDE TO FMSVP, FMSVL1, FMSVL2
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(2)        @@CMD
     C                   MOVEL     @@AST         @@ST
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH
     C*
     C                   MOVEL     QCA(4)        @@CMD2
     C                   MOVEL     @@AST         @@ST2
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD2
     C                   PARM                    LENGTH
     C*
     C                   MOVEL     QCA(6)        @@CMD2
     C                   MOVEL     @@AST         @@ST2
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD2
     C                   PARM                    LENGTH
     C*
     C                   MOVE      '1'           OVRFLG            1
     C                   END                                                    END OVRFLG=0
     C*
     C*   OPEN FILE FOR PROCESSING
     C*
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FMSVP
     C                   OPEN      FMSVL1
     C                   OPEN      FMSVL2
     C                   MOVE      '1'           OPNFLG
     C                   END                                                    END OPNFLG= ' '
     C*
     C                   CALL      'F2320R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      @@AST         ##ST              2
     C                   PARM      *BLANKS       ##SVCD           15
     C                   PARM      *BLANKS       ##DESC           40
     C*
     C                   MOVE      *BLANK        SCSVOF
     C*
     C     ##SVCD        IFEQ      *BLANKS
     C     *INKL         ANDEQ     *OFF
     C*
     C                   MOVEL     *ON           *INKL
     C*
     C                   ENDIF
      *
      * CPT CODES
      *
     C     SCCPF         WHENEQ    '1'
     C*
     C*   CLOSE FILE IF OPENED
     C*
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FMPRP
     C                   CLOSE     FMPRL
     C                   MOVE      ' '           OPNFLG            1
     C                   END                                                    END OPNFLG=1
     C*
     C*   DELETE OVERRIDE
     C*
     C     OVRFLG        IFEQ      '1'
     C*
     C*   DELETE OVERRIDE TO FMPRP, FMPRL
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'                              99
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C*
     C                   CALL      'QCMDEXC'                              99
     C                   PARM                    QCA(9)
     C                   PARM                    LENGTH
     C*
     C                   MOVE      ' '           OVRFLG            1
     C                   END                                                    END OVRFLG=1
     C*
     C     OVRFLG        IFEQ      ' '
     C*
     C*   OVERRIDE TO FMPRP, FMPRL
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(8)        @@CMD
     C                   MOVEL     @@AST         @@ST
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH
     C*
     C                   MOVEL     QCA(10)       @@CMD
     C                   MOVEL     @@AST         @@ST2
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH
     C*
     C*   OPEN FILE FOR PROCESSING
     C*
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FMPRP
     C                   OPEN      FMPRL
     C                   MOVE      '1'           OPNFLG
     C                   END                                                    END OPNFLG= ' '
      *
      * CPT SEARCH
      *
     C                   CALL      'F2305R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      @@AST         ##ST              2
     C                   PARM      *BLANKS       ##CPT4           15
     C                   PARM      *BLANKS       ##DESC           40
     C*
     C                   MOVE      *BLANKS       SCCPF
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C                   MOVEL     *ON           *INKL
     C*
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        SCCPF
     C*
     C                   ENDIF
      *
      * CLEAR 'SEARCHING' MSG
     C                   CALL      'SRRMVMSG'                           99
      *
     C                   ENDSL
      *
     C                   ENDIF
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
     C     ##SVCD        IFEQ      *BLANK
     C     ##CPT4        ANDEQ     *BLANK
     C                   MOVE      '1'           @@RTCD
     C                   END
      *
     C     ##SVCD        IFNE      *BLANK
     C                   MOVEL(P)  ##SVCD        @@PROC
     C                   MOVEL(P)  ##DESC        @@PROD
     C                   END
      *
     C     ##CPT4        IFNE      *BLANK
     C                   MOVEL(P)  ##CPT4        XXCDE#
     C                   MOVEL(P)  @@SVCD        @@PROC
     C                   MOVEL(P)  ##DESC        @@PROD
     C                   MOVE      'C'           @@RTCD
     C                   END
     C*
     C                   ENDDO
      *
      *  THE FOLLOWING STMT IS USED TO SATISFY THE COMPILER;
      *  FILES MUST BE REFERENCED BY AN INPUT SPEC.
      *  A CONDITION IS USED TO KEEP FROM ACCESSING THESE
      *  INPUT OPERATIONS, SINCE THESE FILES MAY BE CLOSED.
      *
     C*
     C     *IN99         IFEQ      *ON
     C     *IN99         ANDEQ     *OFF
     C*
     C                   READ      PSV01                                  99
     C*
     C     SVKY          CHAIN     PSV02                              99
     C*
     C     SVKY1         CHAIN     PSV03                              99
     C*
     C                   READ      PPR01                                  99
     C*
     C     PRKY          CHAIN     PPR02                              99
     C*
     C                   ENDIF
      *
     C                   MOVE      *ON           *INLR
     C                   RETURN
**  QCMDEXC TABLE
DLTOVR FILE(FMSVP)
OVRDBF FILE(FMSVP) TOFILE(*LIBL/FMSVP  )
DLTOVR FILE(FMSVL1)
OVRDBF FILE(FMSVL1) TOFILE(*LIBL/FMSVL1  )
DLTOVR FILE(FMSVL2)
OVRDBF FILE(FMSVL2) TOFILE(*LIBL/FMSVL2  )
DLTOVR FILE(FMPRP)
OVRDBF FILE(FMPRP) TOFILE(*LIBL/FMPRP  )
DLTOVR FILE(FMPRL)
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
