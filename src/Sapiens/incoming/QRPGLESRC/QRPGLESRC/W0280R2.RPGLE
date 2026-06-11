     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*   REVISIONS:
     H*
     H*****************************************************************
     H*
     FSPLCPY0   IF   E             DISK
     F*
     F*

     D TBL             S             20    DIM(1) CTDATA PERRCD(1)
     D                 DS
     D  SPLFLD                 1    202
     D  FIND                   4     23
     D*
     D  MINPRM                65     67
     D  MAXPRM                73     75
     D                 DS
     D  ###                    1      3
     D*   One, two, or three digits printed
     D  PRM1D                  1      1
     D  PRM2D                  1      2
     D  PRM3D                  1      3
     D*   Low order 1 or 2 positions may be blank
     D  L1BLNK                 3      3
     D  L2BLNK                 2      3
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C*
     C     *ENTRY        PLIST
     C                   PARM                    #PRMS             5
     C                   PARM                    RTCD              1
     C                   MOVE      '0'           *IN99
     C                   MOVEA     TBL           YES              20
     C*
     C                   READ      SPL001                                 60
     C     *IN60         DOWEQ     '0'
     C     FIND          IFEQ      YES
     C     MAXPRM        IFNE      *BLANKS
     C                   MOVE      MAXPRM        ###               3
     C                   ELSE
     C                   MOVE      MINPRM        ###
     C                   ENDIF
     C*
     C* Align the left-adjusted printed data
     C*  If two blanks, take first digit only
     C     L2BLNK        IFEQ      *BLANKS
     C                   MOVE      PRM1D         #PRMS5            5 0
     C*
     C*  If one blank, take 2 digits
     C                   ELSE
     C     L1BLNK        IFEQ      *BLANKS
     C                   MOVE      PRM2D         #PRMS5
     C*
     C*  No blanks, take 3 digits
     C                   ELSE
     C                   MOVE      PRM3D         #PRMS5
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      #PRMS5        #PRMS
     C*
     C                   MOVE      '0'           RTCD
     C                   GOTO      ENDIT
     C                   ENDIF
     C                   READ      SPL001                                 60
     C                   ENDDO
     C*
     C     ENDIT         TAG
     C                   MOVE      '1'           *INLR
     C*
**
Number of parameters
