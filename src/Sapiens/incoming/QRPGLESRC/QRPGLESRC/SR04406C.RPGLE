      *===============================================================*
      *  (C) Copyright Progressive Data Solutions, Inc., 2001
      *===============================================================*
     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H bnddir('WTBNDDIR')
    H/TITLE SR04406C - CA - Farm Schedule Endorsement
     *===============================================================*
     * WC040406C                                                     *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 01/08/09                                          *
     * Programmer: C:190                                             *
     * Project   : WC1704                                            *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
      * 12/19/16 C:180 Project SY371 scriptura                        *
     *                                                               *
     * ------------------------------------------------------------- *
     *  NOTE: Please add comments to the top of revisions.           *
     * ------------------------------------------------------------- *
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
     ** Copybook F specs
     F/COPY SCOPYEN01F
    
     * Employer Payroll
     Fwmepl7    if   e           k disk
    
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================

      **** Begin Add **** 12/19/16 *********************************************
      * Entry
     D SR04406C        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR04406C        PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1
      **** End   Add **** 12/19/16 *********************************************
     ** Copybook D specs
     D/COPY SCOPYEN01D
    
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
      **** Begin Change *** 12/19/16 ********************************
     D*** xxform          c                   const('WC040406C ')
     D qqform          c                   const('WC040406C      ')
      **** End   Change *** 12/19/16 ********************************
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
      ****** BEGIN DELETE **** 12/19/16 ****************************
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
      ****** END   DELETE **** 12/19/16 ****************************
    
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
     *
     *  Employer payroll
     *
     C     epky          klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elfyr
     C                   kfld                    elemp#
     C                   kfld                    eldiv
    
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
      *** BEGIN DELETE ****** 12/19/16 *************************************
     C***                write     pfm01
      *** END   DELETE ****** 12/19/16 *************************************
     C                   endsr
    
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
      /Free
       //**** Begin Add  **** 12/19/16 ************************************
          RunningSRtext = *On;
       //**** End   Add  **** 12/19/16 ************************************
      /End-Free
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
     C     rsky          chain     prs01                              60
    
     C                   if        *in60 = *off
     C                             and rsjrst = enst
     C     epky          chain     pep01                              60
    
     C                   dow       *in60 = *off
    
     C                   if        (epclas = 00792)
     C                             or (epclas = 0079)
     C                             or (epclas = 0044)
     C                             or (epclas = 0036)
     C                             or (epclas = 00382)
     C                             or (epclas = 0038)
     C                             or (epclas = 0171)
     C                             or (epclas = 0016)
     C                             or (epclas = 0045)
     C                             or (epclas = 0041)
     C                             or (epclas = 0034)
     C                             or (epclas = 00341)
     C                             or (epclas = 00342)
     C                             or (epclas = 0038)
     C                             or (epclas = 00381)
     C                             or (epclas = 00791)
     C                             or (epclas = 0079)
     C                             or (epclas = 0172)
     C                             or (epclas = 0040)
     C                             or (epclas = 0005)
     C                             or (epclas = 0035)
     C                             or (epclas = 0050)
     C                   move      *on           crtfrm
     c                   leave
     c                   endif

     C     epky          reade     pep01                                  60
     C                   enddo

     C                   endif
    
     C                   endsr
    
    
