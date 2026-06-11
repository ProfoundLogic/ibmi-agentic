     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H bnddir('WTBNDDIR')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     *===============================================================*
     *===============================================================*
     H/TITLE SR00324 - WC000324 - OTHER STATES ENDORSEMENT
     *===============================================================*
     * WC000324 - Other States Endorsement                           *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 09/13/99                                          *
     * Programmer: C:113                                             *
     * Project   : 48WRI                                             *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * XX/XX/XX C:XXX PROJECT: XXXXX                                 *
     *                XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
     *                                                               *
      * 12/16/16 C:180 PROJECT: SY371                                 *
      *                Scriptura interface                            *
      *                                                               *
     * ------------------------------------------------------------- *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS            *
     * ------------------------------------------------------------- *
     *===============================================================*
     * Notes:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
     *  This is a manual attach endorsement with additional input.   *
     *                                                               *
     *  QQMODE = 1 - Called to determine if form should be attached. *
     *  QQMODE = 2 - Called to print endorsement.                    *
     *  QQMODE = 3 - Called for additional input.                    *
     *  QQMODE = 4 - Called for mid-term processing.                 *
     *                                                               *
     *===============================================================*
     ** Copybook F specs
    F/COPY SCOPYEN01F
      *
     FWXFMP1    IF   E           K DISK
     F                                     RENAME(PFM01:PFM02)
     *=========================================================================
     *  Arrays
     *=========================================================================
      *
      *
     D SR00324         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR00324         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
     ** Form ID
      **** Begin change *** 12/16/16 ******************************************
     D**** XXFORM          C                   CONST('WC000324  ')
     D QQFORM          S             15A   INZ('WC000324  ')
      **** End   change *** 12/16/16 ******************************************
     ** Additional entry program
     D ADDPGM          C                   CONST('SR00324I  ')
     *=========================================================================
     *  Data structures and constants
     *=========================================================================
     ** Copybook I specs
    D/COPY SCOPYEN01D
      *
     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
      *
      *
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
     ** Additional Input file

     C     F3KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      **** Begin Change **** 12/16/16 *****************************************
     C*******                   KFLD                    XKFORM
     C                   KFLD                    XXFORM
      **** End   Change **** 12/16/16 *****************************************
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs
    C/COPY SCOPYEN01C
      *
      *=========================================================================
      *
     ** Additional input program parameters
      ****** Begin Delete ****** 12/16/16 **************************************
     C***  addpl         pList
     C***                parm      *blanks       qqprds
     C***                parm      *blanks       qqcall
     C***                parm                    qqco#
     C***                parm                    qqfnd
     C***                parm                    qqemp#
     C***                parm                    qqdiv
     C***                parm                    qqfyr
      ****** End   Delete ****** 12/16/16 **************************************
      *
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     OTHINF        BEGSR
      *
     C     F3KY          CHAIN     WXFMP1                             60
     ** Move the additional input fields
     C                   MOVEL     F3NM01        FMNM01
     C                   MOVEL     F3NM02        FMNM02
     C                   MOVEL     F3NM03        FMNM03
      *
     ** Write the output record.
      **** Begin Delete **** 12/16/16 *****************************************
     C***                   WRITE     PFM01
      **** End Delete **** 12/16/16 *****************************************
      **** Begin Add  **** 12/16/16 *****************************************
     C                   EXSR      WriteWorkFile
      **** End   Add  **** 12/16/16 *****************************************
      *
     C                   ENDSR
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     PGMSTR        BEGSR
      *
     C     F3KY          CHAIN     WXFMP1                             60
      **** Beg Add ***** 12/16/16 ****************************************
      /Free
       RunningSRtext = *ON;
      /End-Free
      **** End Add ***** 12/16/16 ****************************************
      *
     C                   ENDSR                                                  PGMSTR
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     PGMEND        BEGSR
      *
      *
     C                   ENDSR                                                  PGMEND
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     VALID         BEGSR
      *
     ** This is a manual attach endorsement, so set on CRTFRM.
     C                   MOVE      *ON           CRTFRM
      *
     C                   ENDSR                                                  VALID
