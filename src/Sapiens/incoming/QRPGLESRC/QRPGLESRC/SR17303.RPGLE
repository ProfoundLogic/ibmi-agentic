     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H bnddir('WTBNDDIR')
     H/TITLE SR17303 - WC170303 - LA Louisiana Duty to Defend Endorse
      *===============================================================*
      * WC170303 - LA Louisiana Duty to Defend Endorsement            *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      * Created   : 01/19/01                                          *
      * Programmer: C:90                                              *
      * Project   : CM027                                             *
      *===============================================================*
      * REVISIONS:                                                    *
      *                                                               *
      * 12/16/16 C:180 Project SY371 scriptura.                       *
      *                Add Scriptura interface.                       *
      *                                                               *
      * 07/14/11 C:061 Support 77336                                  *
      *              - Convert source to RPGIV.                       *
      *              - Do not attach for flat cancelled divisions.    *
      *                                                               *
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
      *
      *=========================================================================
      *  Arrays
      *=========================================================================
      *
      *
      **** Begin Add **** 12/16/16 *********************************************
      * Entry
     D SR17303         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR17303         PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1
      **** End   Add **** 12/16/16 *********************************************
      *=========================================================================
      *  /COPY required constants
      *=========================================================================
      ** Form ID
      *** Begin Change *** 12/16/16 ********************************************
     D*** XXFORM          C                   CONST('WC170303  ')
     D QQFORM          S             15A   inz('WC170303  ')
      *** End   Change *** 12/16/16 ********************************************
      ******* beg del ****** 07/14/11 *********************************
      ******* Additional entry program
     D******* ADDPGM          C                   CONST('          ')
      ******* Copybook I specs
     D*******/COPY SCOPYEN01I
      ******* end del ****** 07/14/11 *********************************
      ******* beg add ****** 07/14/11 *********************************

      ** Additional entry program
     D ADDPGM          C                   CONST('SRXXXXXXX ')

     D/COPY SCOPYEN01D


     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     ** Entry parameters
      ** Begin Delete *******   12/16/16 ***************************************
     C***  *entry        PLIST
     C***                parm                    qqco#
     C***                parm                    qqfnd
     C***                parm                    qqemp#
     C***                parm                    qqdiv
     C***                parm                    qqfyr
     C***                parm                    qqmode
     C***                parm                    qqrrn
     C***                parm                    qqmod2
    
     ** Additional input program parameters
     C***  addpl         pList
     C***                parm      *blanks       qqprds
     C***                parm      *blanks       qqcall
     C***                parm                    qqco#
     C***                parm                    qqfnd
     C***                parm                    qqemp#
     C***                parm                    qqdiv
     C***                parm                    qqfyr
     *** End   Delete *******   12/16/16 ***************************************
    
      ******* end add ****** 07/14/11 *********************************
      *
      *=========================================================================
      *  KLIST definitions
      *=========================================================================
      *
      *
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
      *** Begin Delete *** 12/16/16 ********************************************
     C*****              WRITE     PFM01
      *** End   Delete *** 12/16/16 ********************************************
      *
     C                   ENDSR
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     PGMSTR        BEGSR
      /Free
       RunningSRtext = *ON;
      /End-Free
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
