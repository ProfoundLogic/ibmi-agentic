     * (C) Copyright Fiserv, Inc., 2000 - 2007
     *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR00113A - Terrorism Risk Insurance Extension Act Endorse
     *===============================================================*
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     *              - Policies with Effective Dates (ELPFDT) on or   *
     *                after /01/2008 will receive the WC000113A where*
     *                as policies with dates prior still receive the *
     *                WC000113 form                                  *
     *                                                               *
     * Created   : 02/15/08                                          *
     * Programmer: C:197                                             *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * 08/04/08 C:190 WC1657                                         *
     *                Change the "logic" to look for a Stat Code     *
     *                (instead of a percentage or Terrorism amount)  *
     *                                                               *
     * 07/18/08 C:190 SEQ#64396                                      *
      *                Changed the processing for the Terrorism Endor-*
     *                sement so that it attaches without checking    *
     *                Foreign or Domestic Terrorism End Stat Code    *
      *                in Stat Rpt/Rating.                            *
     *                                                               *
      * 07/11/08  C:61  Project: WC1530
      *                 Change so attachment criteria controlled in
     *                 Endorsement Approved/Excluded States file
     *                 WTESP.
     *
     *   04/04/08: Changes needed to attach this form to all Policies*
     *             If processing for CLT 133                         *
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
     D qqform          c                   const('WC000113A ')

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
     *  CKSTAT - Check if endorsement approved for state                   d
     *=========================================================================
     c     ckstat        begsr
    
      ******* beg del ****** 07/11/08 *********************************
     c*********          move      'N'           approved_state    1
     *********
     c*********          if        (rsjrst <> 'FL')
     c*********                    and (rsjrst <> 'PA')
     c*********                    and (rsjrst <> 'VA')
     c*********          eval      approved_state = 'Y'
      *********
     c*********          endif
      ******* end del ****** 07/11/08 *********************************
      ******* beg add ****** 07/11/08 *********************************
     c                   move      'Y'           approved_state    1
      ******* end add ****** 07/11/08 *********************************

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
    C*** BEG ADD ******* 04/04/08 ******************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
    C*** END ADD ******* 04/04/08 ******************************************
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
     c                             and (elpfdt >= 1080101)
     c                             and (approved_state = 'Y')
     c
     c
     c     rrky          setgt     prr01
     c     rrky1         readpe    prr01                                  60
     *
     *  Attach endorsement if non-zero terror rate for jurisdiction state
     *
    C*** BEG CHG ******* 04/03/08 ******************************************
     c**********         if        (*in60 = *off)
     c**********                   and (rrterr <> *Zeros)
     c                   if        ((*in60 = *off)
    C*** BEG CHG ******* 07/18/08 ******************************************
     c                             and (rrterc <> *Zeros))
     c**********                   and (rrterr <> *Zeros))
    C*** END CHG ******* 07/18/08 ******************************************
    C*** BEG CHG ******* 08/04/08 ******************************************
     c**********                   or cltid = '133'
    C*** END CHG ******* 08/04/08 ******************************************
    C*** END CHG ******* 04/03/08 ******************************************
     c                   move      *on           crtfrm
     c                   endif
     c                   endif
    
     c                   endsr
