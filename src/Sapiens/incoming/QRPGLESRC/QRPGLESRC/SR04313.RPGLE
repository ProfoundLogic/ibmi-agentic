     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR04313 - Labor Contractor as Named Insured
     *===============================================================*
     * WC040313 - Endorsement Limiting and Restricting this Insurance*
     *            Labor Contractor as Named Insured                  *
     *            Exclusion of Leased Employees                      *
     *                                                               *
      *   Single Manual attached endorsement                          *
     *   with no additional information entry needed                 *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
      * 03/02/17 c:62 Project WC2481
      *               Add PEO logic
     *                                                               *
     * 04/08/11  C:190 Project WC1967                                *
     *                 Attach if 'Employee Leasing ID' has value     *
     *                  '2' or '3'.                                  *
     *                                                               *
     * 06/09/08  C:61  Project 136WDS                                *
     *                 Created.                                      *
     *                                                               *
     *===============================================================*
     * Notes:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
     *                                                               *
     *  QQMODE = 1 - Called to determine if form should be attached. *
     *  QQMODE = 2 - Called to print endorsement.                    *
     *  QQMODE = 3 - Called for additional input.                    *
     *  QQMODE = 4 - Called for mid-term processing.                 *
     *===============================================================*
    
    
     ** Copybook F specs
     F/COPY SCOPYEN01F
    
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================
     C*** begin add ***  03/02/17  ***************************************
     D SR04313         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR04313         PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1
     C*** end add *****  03/02/17  ***************************************

     ** Copybook D specs
     D/COPY SCOPYEN01D
    
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     C*** begin chg ***  03/02/17  ***************************************
     D***** xxform          c                   const('WC040313  ')
     D qqform          s             15a   inz('WC040313  ')
     C*** end chg *****  03/02/17  ***************************************
    
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
    
    
     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
    
    
     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     ** Entry parameters
     C*** begin del ***  03/02/17  ***************************************
     C**** *entry        plist
     C****               parm                    qqco#
     C****               parm                    qqfnd
     C****               parm                    qqemp#
     C****               parm                    qqdiv
     C****               parm                    qqfyr
     C****               parm                    qqmode
     C****               parm                    qqrrn
     C****               parm                    qqmod2
    
     ** Additional input program parameters
     C**** addpl         pList
     C****               parm      *blanks       qqprds
     C****               parm      *blanks       qqcall
     C****               parm                    qqco#
     C****               parm                    qqfnd
     C****               parm                    qqemp#
     C****               parm                    qqdiv
     C****               parm                    qqfyr
     C*** end del *****  03/02/17  ***************************************
    
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
      ***** Beg add ******* 04/08/11 **********************************
     c     emky2         klist
     c                   KFld                    elco#
     c                   KFld                    elfnd
     c                   KFld                    elemp#
     c                   KFld                    eldiv
      ***** Eng add ******* 04/08/11 **********************************
    
    
     *=========================================================================
     *  Main
     *=========================================================================
    
     ** Copybook C specs
     C/COPY SCOPYEN01C
    
    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
    
     ** Write the output record
     C*** Begin Del *** 03/02/17 **************************************
     C**********         write     pfm01
     C*** End   Del *** 03/02/17 **************************************

     C                   endsr
    
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     C                   endsr
    
    
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr
    
    
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
    
      ***** Beg add ******* 04/08/11 **********************************
     C*** Begin Del *** 03/02/17 **************************************
     C*****rsky*         chain     prs01                              60
    
     ** Found a division with a matching jurisdiction state
     C**********         if        *in60 = *off
     C**********                   and rsjrst = enst
     c*****emky2         chain     pem01
     c**********         if        (%found) and (emlsid = '2' or emlsid = '3')
      ***** End add ******* 04/08/11 **********************************
     C**********         move      *on           crtfrm
      ***** Beg add ******* 04/08/11 **********************************
     C**********         endif                                                  emp_ls = '2' or '3'
     C**********         endif
      ***** end add ******* 04/08/11 **********************************         *in60 = *off
     C*** End   Del *** 03/02/17 **************************************
     C*** Begin Add *** 03/02/17 **************************************
     C*
     C* Check to see if PEO group
     C*
     C                   If        crtFrm = *Off
     C                   ExSr      GetFnd
     C                   If        FNpeoF <> ' '
     C                   Eval      crtFrm = *On
     C                   EndIf
     C                   EndIf
     C*** End   Add *** 03/02/17 **************************************
    
     C                   endsr
    
