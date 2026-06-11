     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  WA727R - WCMC INFO SCREEN                   C:53 06/01/11    *
     H*                                                               *
     H*  CALLED FROM: VARIOUS PGMS                                    *
     H*  CALLS TO:                                                    *
     H*****************************************************************
     H*  DESCRIPTION: DISPLAYS A MESSAGE IN THE ARRAYS BASED ON A     *
     H*               PARM BEING PASSED IN                            *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 00/00/000C:000 Project                                        *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FWA727F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*

     D MG1             S             60    DIM(2) CTDATA PERRCD(1)              MSG1
     D MG2             S             60    DIM(2) CTDATA PERRCD(1)              MSG2
     D MG3             S             60    DIM(2) CTDATA PERRCD(1)              MSG3
     D MG4             S             60    DIM(2) CTDATA PERRCD(1)              MSG4
     D MG5             S             60    DIM(2) CTDATA PERRCD(1)              MSG5
     D MG6             S             60    DIM(2) CTDATA PERRCD(1)              MSG6
     D MG7             S             60    DIM(2) CTDATA PERRCD(1)              MSG7
     D MG8             S             60    DIM(2) CTDATA PERRCD(1)              MSG8
     D MG9             S             60    DIM(2) CTDATA PERRCD(1)              MSG9
     D MG0             S             60    DIM(2) CTDATA PERRCD(1)              MSG0
     D MGA             S             60    DIM(2) CTDATA PERRCD(1)              MSGA
     D MGB             S             60    DIM(2) CTDATA PERRCD(1)              MSGB
     D MGC             S             60    DIM(2) CTDATA PERRCD(1)              MSGC
     D DTA             S              1    DIM(60)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  SCMA                   1    120
     D                                     DIM(2)                               SCDATA
     D  SCMA01                 1     60
     D  SCMA02                61    120
     D*
     D CHGNUM          C                   CONST('SA020R')
     D TITL1           C                   CONST('Informational wind-
     D                                     ow.')
     D TITL2           C                   CONST('Response required')

     I*

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@MGID            1
     C                   PARM      SCRESP        @@RTCD            1
     C                   PARM                    @@KL              1
     C*
     C     CHGPL         PLIST
     C                   PARM                    QQNUM            80
     C                   PARM                    QQMASK           10
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     @@MGID        IFEQ      '5'
     C                   MOVE      *ON           *IN20
     C                   ENDIF
     C*
     C                   EXSR      MSG
     C*
     C                   EXFMT     SCR01
     C*
     C     @@MGID        IFEQ      '5'
     C                   MOVE      '1'           DISPLY
     C*
     C     DISPLY        DOWEQ     '1'
     C                   MOVE      *OFF          *IN71
     C     SCRESP        IFNE      'Y'
     C     SCRESP        ANDNE     'N'
     C                   MOVE      *ON           *IN71
     C                   EXFMT     SCR01
     C                   ITER
     C                   ELSE
     C                   MOVE      '0'           DISPLY            1
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*
     C*=====================================================
     C     MSG           BEGSR
     C*=====================================================
     C*
     C                   EXSR      TITLE
     C*
     C                   DO        2             I                 2 0
     C*
     C                   SELECT
     C     @@MGID        WHENEQ    '1'
     C                   MOVEA     MG1(I)        SCMA(I)
     C     @@MGID        WHENEQ    '2'
     C                   MOVEA     MG2(I)        SCMA(I)
     C     @@MGID        WHENEQ    '3'
     C                   MOVEA     MG3(I)        SCMA(I)
     C     @@MGID        WHENEQ    '4'
     C                   MOVEA     MG4(I)        SCMA(I)
     C     @@MGID        WHENEQ    '5'
     C                   MOVEA     MG5(I)        SCMA(I)
     C     @@MGID        WHENEQ    '6'
     C                   MOVEA     MG6(I)        SCMA(I)
     C     @@MGID        WHENEQ    '7'
     C                   MOVEA     MG7(I)        SCMA(I)
     C     @@MGID        WHENEQ    '8'
     C                   MOVEA     MG8(I)        SCMA(I)
     C     @@MGID        WHENEQ    '9'
     C                   MOVEA     MG9(I)        SCMA(I)
     C     @@MGID        WHENEQ    '0'
     C                   MOVEA     MG0(I)        SCMA(I)
     C     @@MGID        WHENEQ    'A'
     C                   MOVEA     MGA(I)        SCMA(I)
     C     @@MGID        WHENEQ    'B'
     C                   MOVEA     MGB(I)        SCMA(I)
     C     @@MGID        WHENEQ    'C'
     C                   MOVEA     MGC(I)        SCMA(I)
     C                   ENDSL
     C                   ENDDO
     C                   ENDSR
     C*
     C*=====================================================
     C     TITLE         BEGSR
     C*=====================================================
     C     @@MGID        IFNE      '5'
     C                   MOVEL     TITL1         ##DATA
     C                   ELSE
     C                   MOVEL     TITL2         ##DATA
     C                   ENDIF
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCTITL
     C                   ENDSR
     C*
     C*=====================================================
     O*
** MG1
          NPW - Fringe Benefits are not available
                to school district employees
** MG2
  Please select the Calc key before exiting this function

** MG3
            Please select the Calc or Submit key
                before exiting this function
** MG4
         This function is not available unless the
              Multiple Employer flag is yes
** MG5
  You are about to approve the selected wage calculation.
      Are you sure you wish to approve this record?
** MG6
     Information must be entered on the TIB's or IIB's
                 screen before calculating
** MG7
Incomplete or no control data found for this specific year.
            No calculation has been performed.
** MG8
  Values entered are not valid for a specific calculation.
            No calculation has been performed.
** MG9
  A revision has been created.  Please refresh the display.

** MG0
         This function is not available unless the
              NPW-Fring Benefits flag is yes
** MGA
 The NPW-fringe benefits flag is N but the NPW is not zero.
               Please review the information.
** MGB
The Multiple employers flag is N but the ME AWW is not zero.
               Please review the information.
** MGC
         This function is not available unless the
                  # of weeks is entered
