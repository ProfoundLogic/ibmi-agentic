     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H bnddir('WTBNDDIR')
     *
    H/TITLE SR41402 - TN - Pending Loss Cost and Assigned Risk Rate
     *===============================================================*
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 08/30/05                                          *
     * Programmer: C:61                                              *
     * Project   : CM027                                             *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
      * 10/19/22 C:342 Project: WC2834 (Recompile)                    *
      *                Endorsement Recognition of Cancellation        *
     *                                                               *
      * 12/29/16 C:180 Project SY371 scriptura.                       *
      *                Add Scriptura interface.                       *
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
     ** Copybook F specs
     F/COPY SCOPYEN01F
    
      * Endorsement Statistical Info
     FWMNXL     if   e           k disk
    
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================

      **** Begin Add **** 12/29/16 *********************************************
      * Entry
     D SR41402         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR41402         PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1
      **** End   Add **** 12/29/16 *********************************************
     ** Copybook D specs
     D/COPY SCOPYEN01D
    
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
      *** Begin Change *** 12/29/16 ********************************************
     D*** xxform          c                   const('WC410402  ')
     D QQform          S             15A    INZ('WC410402  ')
      *** End   Change *** 12/29/16 ********************************************
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
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
      *
      *  Endorsement Form Statistical Info
      *
     c     nxky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    rsjrst
     c                   kfld                    elpfdt
      *
     c     nxky1         klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    rsjrst
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
      *** Begin Delete *** 12/29/16 ********************************************
     C***                write     pfm01
      *** End   Delete *** 12/29/16 ********************************************
     C                   endsr
    
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
      /Free
       //**** Begin Add *** 12/29/16 **************************************
       RunningSRtext = *On;
       //**** End   Add *** 12/29/16 **************************************
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
     c     valid         begsr
    
     c     rsky          chain     prs01                              60
    
     c                   if        *in60 = *off
     c                             and rsjrst = 'TN'

      *  Check for pending rate change
    
     c     nxky          setgt     pnx01
     c     nxky1         readpe    pnx01
      *
     c                   dow       (not %eof)
     c                             and (nxstat = 'I')
     c     nxky1         readpe    PNX01
     c                   enddo

      *
      *  A VALID RECORD WAS FOUND, THERE IS A PENDING RATE CHG, THE
      *  PENDING RATE CHG DATE IS NOT ZERO, AND IS LESS THAN/EQUAL TO
      *  THE POLICY FROM DATE.
      *
     c                   if        (not %eof)
     c                             and (nxprcf = 'Y')
     c                             and (nxprcd <> 0)
     c                             and (nxprcd <= elpfdt)
     c                   move      *on           crtfrm
     c                   endif
    
     c                   endif                                                  rsjrst = TN
    
     c                   endsr
    
