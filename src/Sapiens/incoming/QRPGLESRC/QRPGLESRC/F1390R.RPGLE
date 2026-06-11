     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     F*
     F/TITLE F1390R - CodeReview Decision Window
     F*******************************************************************
     F*                                                                 *
     F*  Called From:  F0390R                                           *
     F*  Calls To   :  None                                             *
     F*                                                                 *
     F*******************************************************************
     F* Created on 06/04/2002 by C:62 for Project 114MDC                *
     F*******************************************************************
     F* Revisions:                                                      *
     F*
     F*  05/09/13 C:279 Redmine: 2052
     F*                 Modified to default checkbox to 'N'.
     H*
     H*  06/22/12 C:280 PROJECT : 114MER
     H*                 convert to ile
     F*                                                                 *
     F*******************************************************************
     FF1390F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D FIDS01        E DS                  EXTNAME(SZQ2P)

     I*

     C     *ENTRY        PLIST
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@SBCR            1
     C*
     C                   EXSR      PGMSTR
     C*
     C     *IN90         DOUEQ     *OFF
     C                   EXFMT     WDW01
     C*
     C     *INKL         IFEQ      *OFF
     C                   EXSR      ERRCK
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   EXSR      PGMEND
     C*
     C                   eval      *inlr = *on
     C                   RETURN
     C*****************************************************************
     C* ERRCK - Error Check Routine
     C*****************************************************************
     C     ERRCK         BEGSR
     C*
     C                   SETOFF                                       90
     C*
     C* Must be 'Y' or 'N'
     C*
     C     SCSBCR        IFNE      'Y'
     C     SCSBCR        ANDNE     'N'
     C*
     C                   SETON                                        7190
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*****************************************************************
     C* PGMEND - Program End Routine
     C*****************************************************************
     C     PGMEND        BEGSR
     C*
     C     *INKL         IFEQ      *OFF
     C                   MOVEL(P)  SCSBCR        @@SBCR
     C                   ENDIF
     C*
     C                   ENDSR
     C*****************************************************************
     C* PGMSTR - Program Initialization
     C*****************************************************************
     C     PGMSTR        BEGSR
     C*
     C                   SETOFF                                       90
     C                   MOVEL(P)  Q1PGM         SCPGM
     C                   MOVEL(P)  Q1USER        SCUSER
     C*
     C                   CALL      'FA007R'
     C                   PARM      *ZEROS        ##CO#             3 0
     C                   PARM      *ZEROS        ##FND             3 0
     C                   PARM      *BLANKS       ##CONM           40
     C                   PARM      *BLANKS       ##FNAM           40
     C*
     C                   MOVEL     ##CONM        SCCONM
     C*
     C****** Beg Add ******** 05/09/13 ********************
     C                   IF        SCSBCR = ' '
     C                   EVAL      SCSBCR = 'N'
     C                   ENDIF
     C****** End Add ******** 05/09/13 ********************
     C                   ENDSR
