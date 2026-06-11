     * (C) Copyright Progressive Data Solutions, Inc., 2004
     *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR04406B - CA - Farm Schedule Endorsement
     *===============================================================*
     * WC040406B                                                     *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 05/16/08                                          *
     * Programmer: C:197                                             *
     * Project   : C132WKZ                                           *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
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

     ** Copybook D specs
     D/COPY SCOPYEN01D
    
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     D qqform          c                   const('WC040406B ')
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
     C                   write     pfm01
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
