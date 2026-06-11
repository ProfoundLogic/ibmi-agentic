     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR29406W - Update NJ Premium Discount Form
     *===============================================================*
     * WC290406W - Update NJ Premium Discount Form                   *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 11/27/09                                          *
     * Programmer: C:190                                             *
     * Project   : WC1863                                            *
     *===============================================================*
      * Revised:
      *
     *                 It should attached to all NJ coverage.        *
     *                                                               *
      *
     * 12/17/10  C:220  Support: 75076                               *
      *                    Form should attached if deductible discount
      *                    Applies for CSI.
      *
      *
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
     *
     * Employer Policy Year
     Fwmepl     if   e           k disk
     *
     *
     Fwmprl1    if   e           k disk
     **** beg add *****  12/17/10  ************************************
     *  Deductible Discount Code Table
     Fwtddl2    IF   E           K DISK
     **** end add *****  12/17/10  ************************************
     *
     ** Copybook F specs
     F/COPY SCOPYEN01F
     *
     *
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     ** Copybook D specs
     D/COPY SCOPYEN01D


     *=========================================================================
     *  /COPY required constants
     *=========================================================================
     ** Form ID
     D qqform          c                   const('WC290406W ')
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')


     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
     *
     *
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
      *
     **** beg add *****  12/17/10  ************************************
     C     ddky          klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    rsjrst
     C                   kfld                    eldcde
     C                   kfld                    xxddt             7 0
      *
     C     ddky1         klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    rsjrst
     C                   kfld                    eldcde
     **** end add *****  12/17/10  ************************************
     *
     *
     *
     *
     *
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs
     C/COPY SCOPYEN01C
     *
     *
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
     *
     C*
     ** Write the output record
     C                   write     pfm01
     C                   endsr
    
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     *
     C*** beg add *****  12/17/10  ************************************
     c                   call      'RTVCLTID'
    c                   parm                    cltid             3
     C*** end add *****  12/17/10  ************************************
     C                   endsr
    
    
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     *
     C                   endsr
    
    
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
     *
     C     rsky          chain     prs01                              60
     *
     C                   if        *in60 = *off
     C                             and rsjrst = enst
      *
     C                   move      *on           crtfrm
     *
     c*** beg add *****  12/17/10  ************************************
      *
     c                   if        cltid = '114'
      *
     C                   move      elco#         q1co#
     C                   move      elfnd         q1fnd
     C                   move      eldcef        q2date
     C                   move      elfyr         q1fyr
     C                   move      elemp#        q1emp#
     C                   move      eldiv         q1div
     C*
     C                   call      'SREFFD'
     C                   parm                    q1co#             3
     C                   parm                    q1fnd             3
     C                   parm                    q1fyr             3
     C                   parm                    q1emp#            9
     C                   parm                    q1div             5
     C                   parm                    q2date            7
     C*
     C                   move      q2date        xxddt
     c*
     c     ddky          setgt     pdd01
     c     ddky1         readpe    pdd01
     c                   if        %found
     c                             and (ddampo <> 0)
     *
     c                   move      *on           crtfrm
     *
     c                   else
     c                   move      *off          crtfrm
     c                   endif
     *
     c                   endif
     c*** end add *****  12/17/10  ************************************
     *
     C                   endif
     *
     C                   endsr
    
    
