     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  WA720R - WCMC INFO SCREEN                   C:53 02/03/05    *
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
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FWA720F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*

     D MG1             S             60    DIM(7) CTDATA PERRCD(1)              MSG1
     D**                  MG2     1   7 60               MSG2
     D**                  MG3     1   7 60               MSG3
     D**                  MG4     1   7 60               MSG4
     D**                  MG5     1   7 60               MSG5
     D DTA             S              1    DIM(60)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  SCMA                   1    240
     D                                     DIM(4)                               SCDATA
     D  SCMA01                 1     60
     D  SCMA02                61    120
     D  SCMA03               121    180
     D  SCMA04               181    240
     D                 DS
     D  SCMB                   1    180
     D                                     DIM(3)                               SCDATA
     D  SCMB01                 1     60
     D  SCMB02                61    120
     D  SCMB03               121    180
     D*********                             181 240 SCMB04
     D*
     D CHGNUM          C                   CONST('SA020R')
     D TITL2           C                   CONST('This is an informa-
     D                                     tion window.')

     I*

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@MGID            1
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
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
     C     FNKEY         KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C*
     C******
     C     @@MGID        IFEQ      '1'
     C                   SETOFF                                       21
     C                   ENDIF
     C*
     C     @@MGID        CASEQ     '1'           MSG1
     C****       @@MGID    CASEQ'2'       MSG2
     C****       @@MGID    CASEQ'3'       MSG3
     C****       @@MGID    CASEQ'4'       MSG4
     C****       @@MGID    CASEQ'5'       MSG4
     C****       @@MGID    CASEQ'6'       MSG5
     C                   ENDCS
     C*
     C                   EXFMT     SCR01
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*
     C*=====================================================
     C     MSG1          BEGSR
     C*=====================================================
     C*
     C                   EXSR      TITLE
     C*
     C                   DO        7             I                 2 0
     C*
     C     I             IFLE      4
     C                   MOVEA     MG1(I)        SCMA(I)
     C                   ELSE
     C*
     C     I             SUB       4             X                 2 0
     C                   MOVEA     MG1(I)        SCMB(X)
     C                   ENDIF
     C                   ENDDO
     C                   ENDSR
     C*
     C*=====================================================
     C     MSG2          BEGSR
     C*=====================================================
     C                   ENDSR
     C*
     C*=====================================================
     C     MSG3          BEGSR
     C*=====================================================
     C                   ENDSR
     C*
     C*=====================================================
     C     MSG4          BEGSR
     C*=====================================================
     C                   ENDSR
     C*=====================================================
     C     MSG5          BEGSR
     C*=====================================================
     C                   ENDSR
     C*
     C*=====================================================
     C     TITLE         BEGSR
     C*=====================================================
     C                   MOVEL     TITL2         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCTITL
     C                   ENDSR
     C*
     C*=====================================================
     O*
** MG1


    Case is dismissed or settled, payments are not allowed.




