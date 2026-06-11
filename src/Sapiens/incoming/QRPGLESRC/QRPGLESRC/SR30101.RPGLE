     * (C) Copyright Fiserv, Inc., 2000 - 2007
     *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR30101 - Terrorism Risk Insurance Extension Act End - NM
     *===============================================================*
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 03/27/08                                          *
     * Programmer: C:190                                             *
     * Project   : WC1667                                            *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * 07/18/08 C:190 SEQ#64396                                      *
      *                Changed the processing for the Terrorism Endor-*
     *                sement so that it attaches without checking    *
     *                Foreign or Domestic Terrorism End Stat Code    *
      *                in Stat Rpt/Rating.                            *
     *                                                               *
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
     * Copybook F specs
     F/COPY SCOPYEN01F
    
      ** Statistical Reporting/Rating File
     Fwdrrl     if   e           k disk
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     * Copybook D specs
     D/COPY SCOPYEN01D

      *


     *=========================================================================
     *  /COPY required constants
     *=========================================================================

     * Form ID
     D qqform          c                   const('WC300101  ')

     * Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')



     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
    
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
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
      *
      *  Statistical Reporting/Rating
      *
     C     rrky          KList
     C                   KFld                    ELCO#
     C                   KFld                    ELFND
     C                   KFld                    RSJRST
     C                   KFld                    ELPFDT
      *
     C     rrky1         KList
     C                   KFld                    ELCO#
     C                   KFld                    ELFND
     C                   KFld                    RSJRST
    
    
    
     *=========================================================================
     *  Main
     *=========================================================================
    
     * Copybook C specs
     C/COPY SCOPYEN01C
    
     *=========================================================================
     *  CKSTAT - Check if endorsement approved for state
     *=========================================================================
     c     ckstat        begsr
    
     c                   move      'N'           approved_state    1
    
     c                   if        (rsjrst = 'NM')
    
     c                   eval      approved_state = 'Y'

     c                   endif

     c                   endsr
    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
    
     ** Write the output record
     C                   write     pfm01
    
     C                   endsr
    
    
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     C*
    
     C                   endsr
    
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
     c     valid         begsr
    
     c     rsky          chain     prs01                              60
     c                   exsr      ckstat
    
     c                   if        (*in60 = *off)
     c                             and (approved_state = 'Y')
     c
     c
     c     rrky          setgt     prr01
     c     rrky1         readpe    prr01                                  60
     *
     *  Attach endorsement if non-zero terror rate for jurisdiction state
     *
     c                   if        (*in60 = *off)
    C*** BEG CHG ******* 07/18/08 ******************************************
     c                             and (rrterc <> *Zeros)
     c**********                   and (rrterr <> *Zeros))
    C*** END CHG ******* 07/18/08 ******************************************
     c                   move      *on           crtfrm
     c                   endif
     c                   endif
    
     c                   endsr
