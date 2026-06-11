     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W0051R51 - CLT139 MISC CHANGE END - ADD
     H*****************************************************************
     H*  DESCRIPTION:  CALLS CLT139 MISC CHANGE END WORK WITH PGM     *
     H*                TO ADD MID-TERM MISC CHG ENDORSEMENT           *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0051R1  - EMPLOYER MID TERM ENDORSEMENTS                  *
     H*  CALLS TO:                                                    *
     H*    SRMISCI  - MISC CHG END WORK WITH                          *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/13/09 C:61  PROJECT 139WLR                                 *
     H*                CREATED.                                       *
     H*                                                               *
     H*****************************************************************

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@RRN             9 0          WMEMP RRN
     C                   PARM                    @@RRN1            9 0          WDELP RRN
     C                   PARM                    @@RRN2            9 0          WTMYP RRN
     C                   PARM                    @@ENDT            8 0
      *
     C                   CALL      'SRMISCI'                            99
     C     @@KC          PARM                    ##KC              1
     C     @@KL          PARM                    ##KL              1
     C                   PARM      *BLANKS       ##CALL            1
     C                   PARM      0             ##RRN             9 0          WLMSP
     C                   PARM      @@RRN1        ##RRN2            9 0          WDELP
     C                   PARM      Q1PGM         ##PGM            10            Program
     C                   PARM      'A'           ##WTYP            1            ADD MID-TERM
      *
     C                   MOVE      *ON           *INLR
     C                   RETURN
      *
