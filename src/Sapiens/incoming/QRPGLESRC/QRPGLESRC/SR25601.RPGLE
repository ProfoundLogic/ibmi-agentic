     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     H/define CONVERT
      *===============================================================*
     H/TITLE SR25601 - MT MONTANA CANCELATION AND NONRENEWAL ENDORSE.
      *===============================================================*
      * WC250601 - Montana Cancelationrand Nonrenewal Endorsement     *
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

     D qqFORM          C                   CONST('WC250601  ')
      ** Additional entry program
     D ADDPGM          C                   CONST('xxxxx     ')
      *=========================================================================
      *  *LIKE DEFN definitions
      *=========================================================================
      *
      *
      *=========================================================================
      *  Data structures and constants
      *=========================================================================
      ** Copybook I specs
     D/COPY SCOPYEN01D
      *
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
