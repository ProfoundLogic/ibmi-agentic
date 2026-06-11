     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *===============================================================*
     H/TITLE CT-1 - MANAGED CARE PROGRAM ENDORSEMENT
      *===============================================================*
      * CT-1 - Managed Care Program Endorsement                       *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      * Created   : 10/30/00                                          *
      * Programmer: C:90                                              *
      * Project   : 96WDO                                             *
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

     D XXFORM          C                   CONST('CT-1      ')
      ** Additional entry program
     D ADDPGM          C                   CONST('SRCT1     ')
      ** Copybook D specs
     I/COPY SCOPYEN01D
      *=========================================================================
      *  *LIKE DEFN definitions
      *=========================================================================
      *
      *
      *=========================================================================
      *  Data structures and constants
      *=========================================================================
      ** Copybook I specs
     ****I/COPY SCOPYEN01I
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
      *
     ** Additional input program parameters
     C     addpl         pList
     C                   parm      *blanks       qqprds
     C                   parm      *blanks       qqcall
     C                   parm                    qqco#
     C                   parm                    qqfnd
     C                   parm                    qqemp#
     C                   parm                    qqdiv
     C                   parm                    qqfyr
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
     C     ELASP2        ANDNE     *ZEROS
      *
      ** Found a division with a matching jurisdiction state
     C                   MOVE      *ON           CRTFRM
     C                   ENDIF                                                  *IN60 = *OFF
      *
     C                   ENDSR                                                  VALID
