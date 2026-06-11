      *===============================================================*
     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     H/TITLE SR28301 - NH NEW HAMPSHIRE WC AND EMPLOYERS LIABILITY
      *===============================================================*
      * WC280301 - New Hampshire WC and Employers Liability Coverage  *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      * Created   : 12/21/99                                          *
      * Programmer: C:90                                              *
      * Project   : 48WTK                                             *
      *===============================================================*
      * REVISIONS:                                                    *
      *
      * 05/22/13 C:190 Support 82319
      *                Corrected error, pgm was blowing up when
      *                users attempt to add the "WC280301".
      *
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
      *
      ** Group Detail File
     FWDF1L     IF   E           K DISK    USROPN
      *
      ** Additional Information File
     FWXFMP1    IF   E           K DISK    USROPN
     F                                     RENAME(PFM01:PFM02)
      *=========================================================================
      *  Arrays
      *=========================================================================
      *
      *
      *=========================================================================
      *  /COPY required constants
      *=========================================================================
      ** Form ID
     D qqFORM          C                   CONST('WC280301  ')
      ******* beg del ****** 07/14/11 *********************************
      ******* Additional entry program
     D******* ADDPGM          C                   CONST('          ')
      ******* Copybook I specs
     D*******/COPY SCOPYEN01I
      ******* end del ****** 07/14/11 *********************************
      ******* beg add ****** 07/14/11 *********************************

      ** Additional entry program
      ******* beg chg ****** 05/22/13 *********************************
     D****** ADDPGM          C                   CONST('SRXXXXXXX ')
     D ADDPGM          C                   CONST('SR28301I  ')
      ******* end chg ****** 05/22/13 *********************************

     D/COPY SCOPYEN01D


     *=========================================================================
     *  PLIST definitions
     *=========================================================================

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
    
      ******* end add ****** 07/14/11 *********************************
      *
      *=========================================================================
      *  KLIST definitions
      *=========================================================================
      ** Group Detail file
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
      *
      ** Additional Input file
     C     FMKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    xxFORM
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
     C                   OPEN      WXFMP1
     C                   OPEN      WDF1L
      *
     C     FMKY3         CHAIN     PFM02                              60
     C     F1KY          CHAIN     PF101                              60
      *
      ** Move the custom fields to print
     C                   Z-ADD     F3A701        FMA701
     C                   Z-ADD     F3A521        FMA521
     C                   Z-ADD     F3A921        FMA921
     C                   Z-ADD     F3A702        FMA702
     C                   Z-ADD     F3A522        FMA522
     C                   Z-ADD     F3A922        FMA922
     C                   Z-ADD     F3A703        FMA703
     C                   Z-ADD     F3A523        FMA523
     C                   Z-ADD     F3A923        FMA923
      ** Write the output record
     C                   WRITE     PFM01
      *
     C                   CLOSE     WXFMP1
     C                   CLOSE     WDF1L
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
