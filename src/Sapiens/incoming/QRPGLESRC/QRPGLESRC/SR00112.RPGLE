     * (C) Copyright Fiserv, Inc., 2003 - 2008')
     *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR00112 - Notice of Change to Terrorism Risk Act Endorse
     *===============================================================*
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 09/28/04                                          *
     * Programmer: C:61                                              *
     * Support   : WA109 43900                                       *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * 09/22/08 C:190 WC1657 - (Additional changes)                  *
      *                Changed the processing for the Terrorism Endor-*
     *                sement so that it attaches without checking    *
     *                Foreign or Domestic Terrorism End Stat Code    *
      *                in Stat Rpt/Rating.                            *
     *                                                               *
      * 11/30/05 C:61  support wa114 50097
     *                Sunset 01/01/06 Terror Risk Act
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
    
    
    
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
     D qqform          c                   const('WC000112  ')

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
    
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
     C     valid         begsr
     ****** beg add **** 11/30/05 ***************************************
     C                   if        (elpfdt < 1060101)
     ****** end add **** 11/30/05 ***************************************
     *
     C                   if        elpfdt >= 1050101
     C     rsky          chain     prs01                              60
     *
     C                   if        (*in60 = *off)
     C                             and (rsjrst <> 'PA')
     C     rrky          setgt     prr01
     C     rrky1         readpe    prr01                                  60
     *
     *  Attach endorsement if non-zero terror rate for jurisdiction state
     *
     C     *in60         ifeq      *off
    C*** BEG CHG ******* 09/22/08 ******************************************
     C     rrterc        andne     *zero
     C*****rrterr        andne     *zero
    C*** END CHG ******* 09/22/08 ******************************************
     C                   move      *on           crtfrm
     C                   endif
     *
     C                   endif
    
     C                   endif
     ****** beg add **** 11/30/05 ***************************************
     *
     C                   endif
     ****** end add **** 11/30/05 ***************************************
     *
     C                   endsr
    
    
    
