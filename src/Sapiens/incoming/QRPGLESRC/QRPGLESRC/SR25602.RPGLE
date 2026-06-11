      *===============================================================*
     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     H/TITLE SR25602 - MT MONTANA SAFETY ENDORSEMENT
      *===============================================================*
      * WC250602 - Montana Safety Endorsement                         *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      * Created   : 12/03/99                                          *
      * Programmer: C:90                                              *
      * Project   : 48WTK                                             *
      *===============================================================*
      * REVISIONS:                                                    *
      *                                                               *
      * 10/21/10 C:220 Project : WC1951                               *
      *                - Recompiled and converted to RPGLE to comply  *
      *                with new WXFMP file                            *
      *                - Changed the copybook SCOPYEN01I to ILE       *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*
      * Notes:                                                        *
      *                                                               *
      *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      *                                                               *
      *  QQMODE = 1 - Called to determine if form should be attached. *
      *  QQMODE = 2 - Called to print endorsement.                    *
      *  QQMODE = 3 - Called for additional input.                    *
      *  QQMODE = 4 - Called for mid-term processing.                 *
      *===============================================================*
      ** Copybook F specs
     F/COPY SCOPYEN01F
      *=========================================================================
      *  Arrays
      *=========================================================================
      *
      *
      *=========================================================================
      *  /COPY required constants
      *=========================================================================
      ** Form ID
     D qqFORM          C                   CONST('WC250602  ')
      ** Additional entry program
     D ADDPGM          C                   const('SRXXXXXXXX')
     D******** Beg Add **** 10/21/10 ***************************************
     D/COPY SCOPYEN01D
     D******** End Add **** 10/21/10 ***************************************
      *=========================================================================
      *  *LIKE DEFN definitions
      *=========================================================================
      *
      *
      *=========================================================================
      *  Data structures and constants
      *=========================================================================
      ** Copybook I specs
     I******** Beg Del **** 10/21/10 ***************************************
     I*/COPY SCOPYEN01I
     I******** End Del **** 10/21/10 ***************************************
     C******** Beg Add **** 10/21/10 ***************************************
     *=========================================================================
     *  PLIST definitions
     *=========================================================================
    
     ** Entry parameters
     C     *entry        plist
     C                   parm                    qqco#             3
     C                   parm                    qqfnd             3
     C                   parm                    qqemp#            9
     C                   parm                    qqdiv             5
     C                   parm                    qqfyr             3
     C                   parm                    qqmode            1
     C                   parm                    qqrrn             9 0
     C                   parm                    qqmod2            1
     C******** End Add **** 10/21/10 ***************************************
      *=========================================================================
      *  KLIST definitions
      *=========================================================================
      *=========================================================================
      *  Main
      *=========================================================================
      ** Copybook C specs
     C/COPY SCOPYEN01C
      *
      *=========================================================================
      *  OTHINF - Subroutine to load form specific information & write record
      *=========================================================================
     C     OTHINF        BEGSR
      *
      ** Write the output record
     C                   WRITE     PFM01
      *
     C                   ENDSR
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     PGMSTR        BEGSR
      *
      *
     C                   ENDSR                                                  PGMSTR
      *=========================================================================
      *  PGMEND - End of program processing
      *=========================================================================
     C     PGMEND        BEGSR
      *
      *
     C                   ENDSR                                                  PGMEND
      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
     C     VALID         BEGSR
      *
     C     RSKY          CHAIN     PRS01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     RSJRST        ANDEQ     ENST
      *
      ** Found a division with a matching jurisdiction state
     C                   MOVE      *ON           CRTFRM
     C                   ENDIF                                                  *IN60 = *OFF
      *
     C                   ENDSR                                                  VALID
