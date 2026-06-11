      *===============================================================*
     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H bnddir('WTBNDDIR')
     H/TITLE SR45604 - VA CONTRACTING CLASSIFICATION PREMIUM ADJUSTMENT
      *===============================================================*
      * WC450604 - Virginia Contracting Classification Premium Adjust *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      * Created   : 12/30/99                                          *
      * Programmer: C:90                                              *
      * Project   : WA096/21162                                       *
      *===============================================================*
      * REVISIONS:                                                    *
      *
      * 12/29/16 C:180 Project SY371 scriptura.
      *                Add Scriptura interface.
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
     FWMEPL     IF   E           K DISK
     FWMPRL1    IF   E           K DISK
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
      **** Begin Add **** 12/29/16 *********************************************
      * Entry
     D SR45604         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR45604         PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1
      **** End   Add **** 12/29/16 *********************************************
      ** Form ID
      *** Begin Change *** 12/29/16 ********************************************
     D**** XXFORM          C                   CONST('WC450604  ')
     D QQFORM          S             15A   inz('WC450604  ')
      *** End   Change *** 12/29/16 ********************************************
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
      ** Begin Delete *******   12/29/16 ***************************************
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
     *** End   Delete *******   12/29/16 ***************************************
    
      ******* end add ****** 07/14/11 *********************************
      *
      *=========================================================================
      *  KLIST definitions
      *=========================================================================
     C*
     C*  Employer Payroll Master file
     C*
     C     EPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  Payroll Master file
     C*
     C     PRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    EPCLAS
     C                   KFLD                    ELPFDT
     C*
     C     PRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    EPCLAS
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
      *** Begin Delete *** 12/29/16 ********************************************
     C***                WRITE     PFM01
      *** End   Delete *** 12/29/16 ********************************************
      *
     C                   ENDSR
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     PGMSTR        BEGSR
      *
      /Free
       //**** Begin Add *** 12/29/16 **************************************
       RunningSRtext = *On;
       //**** End   Add *** 12/29/16 **************************************
      /End-Free
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
     C*
     C*  Contracting class credit must be zero
     C*
     C     ELASP1        IFEQ      *ZERO
     C*
     C*  Look for payroll records that have a contracting classification
     C*
     C     EPKY          SETLL     PEP01
     C     EPKY          READE     PEP01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     EPSTAT        IFNE      'I'
     C*
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     PRCCFL        ANDEQ     'Y'
     C                   MOVE      *ON           CRTFRM
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C                   ENDIF                                                  EPSTAT <> 'I'
     C*
     C     EPKY          READE     PEP01                                  60
     C                   ENDDO
     C*
     C     CRTFRM        IFEQ      *ON
     C                   ENDIF
     C*
     C                   ENDIF                                                  ELASP1 = *ZE
     C*
      *
      ** Found a division with a matching jurisdiction state
     C                   ENDIF                                                  *IN60 = *OFF
      *
     C                   ENDSR                                                  VALID
