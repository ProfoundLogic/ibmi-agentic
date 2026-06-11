      *===============================================================*
     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H bnddir('WTBNDDIR')
     H/TITLE SR08601 - DC DISTRICT OF COLUMBIA CANCELATION ENDORSEMENT
      *===============================================================*
      * WC080601 - District of Columbia Cancelation Endorsement       *
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
      * 10/31/22 C:333 Project: WC2834 (Recompile)                    *
      *                Endorsement Recognition of Cancellation        *
      *                                                               *
      * 12/16/16 C:180 Project SY371 scriptura.                       *
      *                Add Scriptura interface.                       *
      *                                                               *
      * 07/14/11 C:061 Support 77336
      *              - Convert source to RPGIV.
      *              - Do not attach for flat cancelled divisions.
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
      **** Begin Add **** 12/16/16 *********************************************
      * Entry
     D SR08601         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR08601         PI
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
      *  Arrays
      *=========================================================================
      *
      *
      *=========================================================================
      *  /COPY required constants
      *=========================================================================
      ** Form ID
      *** Begin Change *** 12/16/16 ********************************************
     D*** XXFORM          C                   CONST('WC080601  ')
     D QQFORM          S             15A   inz('WC080601  ')
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
     C***  *entry        plist
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
     C***                WRITE     PFM01
      *** End   Delete *** 12/16/16 ********************************************
      *
     C                   ENDSR
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     PGMSTR        BEGSR
      /Free
       //**** Begin Add **** 12/29/16 **************************************
        RunningSRtext = *On;
       //**** End   Add **** 12/29/16 **************************************
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
