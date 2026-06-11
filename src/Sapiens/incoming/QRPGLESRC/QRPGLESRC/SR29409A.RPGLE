     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *===============================================================*
    H/TITLE SR29409A - NJ - Managed Care Endorsement
     *===============================================================
     * WC290409A - New Jersey Managed Care Endorsement
     *
     * PROGRAM DESCRIPTION:
     *
     *   Determines if this endorsement form should be attached to
     *   the policy and creates data in the Endorsement Form
     *   Workfile (WXFMP) prior to a form merge.
     *
     * CREATED   : 05/18/00
     * PROGRAMMER: C:113
     * PROJECT   : 95WEP
     *===============================================================
     * REVISIONS:
     *                                                               *
     * 11/03/22 C:333 Project: WC2834                                *
      *                Endorsement Recognition of Cancellation
     *                                                               *
     * 09/29/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.
      *               -No comments.
     *                                                               *
     * 07/27/07 C:184 PROJECT: 139/SUPP59155                         *
     *                Add Mananged Care code 9873 as valid Managed   *
     *                Care code stat element.                        *
     *                                                               *
     * 06/04/07 C:189 PROJECT: 099/SUPP#58380                        *
     *                Endorsement print/attach programs should not   *
      *                write/update WXPEP records if single           *
     *                endorsement print request                      *
     *                                                               *
     *                                                               *
     *  04/30/01 C:65  PROJECT: 114WGH/114WBK                        *
     *                 Add processing to write endorsement data to   *
     *                 the WXPEP reporting file.                     *
     *
     * 05/27/04 C:61  Project: 114WGH
     *              - Comare percentages instead of amounts.
      *              - Use Policy FROM date instead of
      *                MCO Group effective date.
     *
     * 08/24/01  C:90 PROJECT: CM027 WA107/28711                     *
     *                Read thru all divisions within the OTHINF      *
     *                subroutine.                                    *
     *                                                               *
     * 07/20/01 C:135 Project: seq# 27807
     *                Add Mananged Care code 9843 as valid Managed Care
     *                Code stat element.
     *
     * 11/20/00 C:09  PROJECT: SEQ# 25111
     *                Display the MCO Group Code effective date instead
     *                of the Managed care effective date in employer
     *                detail.
     *
     * 08/03/00 C:09  PROJECT: CM027
     *                Need to determine which stat reporting element
     *                is being used to issue a Managed Care Credit.
     *                The statistical reporting code for 'NJ' is 9874.
     *                Determine which stat element has this code.
     *
     * -------------------------------------------------------------
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.
     * -------------------------------------------------------------
     *===============================================================
     * NOTES:
     *
     *  Utilizes the endorsement copybook (SCOPYEN01F/D/C).
     *
     *  qqmode = 1 - Called to determine if form should be attached.
     *  qqmode = 2 - Called to print endorsement.
     *  qqmode = 3 - Called for additional input.
     *  qqmode = 4 - Called for midterm processing.
     *===============================================================

     ** Copybook F specs
     F/COPY SCOPYEN01F
     Fwdrrl     if   e           k disk
     Fwmmgl     if   e           k disk
     Fwxpel     uf   e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     ** Copybook D specs
     D/COPY SCOPYEN01D
     D/COPY WWCPOLDAT4
     *=========================================================================
     *  Copybook required constants
     *=========================================================================

     ** Form ID
     D qqForm          s             15a   inz('WC290409A ')
     D xxunid          s                   like(peunid)
     D x1end#          s                   like(peend#)

     ** Additional entry program
      * Dummy place holder
     D addpgm          c                   const('SR29409I  ')
     D xxcram          s             11  2

      *entry parameters
     D SR29409A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR29409A        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

     *=========================================================================
     *  KLIST definitions
     *=========================================================================
    
     *=========================================================================
     *  Main
     *=========================================================================
    
     ** Copybook C specs (PLISTs, KLISTs and mainline)
     C/COPY SCOPYEN01C
     ** Stat rpt/rating file
     C     rrky          KList
     C                   KFld                    elco#
     C                   KFld                    elfnd
     C                   KFld                    rsjrst
     C                   KFld                    elpfdt
     C     rrky1         KList
     C                   KFld                    elco#
     C                   KFld                    elfnd
     C                   KFld                    rsjrst
     ** MCO group code file
     C     mgky1         KList
     C                   KFld                    elco#
     C                   KFld                    elfnd
     C                   KFld                    elmcgp
     C                   KFld                    elpfdt
     C     mgky          KList
     C                   KFld                    elco#
     C                   KFld                    elfnd
     C                   KFld                    elmcgp
     C     pekey         KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxdt              7 0
     C     pekey1        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C     pekey2        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxunid
     C                   KFld                    x1end#
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information
     *=========================================================================
     C     othinf        begsr
      /Free
       WriteInCopybook = *Off;
      /End-Free
     c                   clear                   E00409
     c                   z-add     0             xxiefp
     c                   z-add     0             xxiprp
     C     elky2         chain     pel02                              60
    
     C                   dow       *in60 = *off
       //***** Beg Add *** 11/03/22 ******************************
          xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
          If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
       //***** End Add *** 11/03/22 ******************************
     ** Get the state reporting division state
     C     rsky          chain     prs01                              20
     C                   if        *in60 = *off
     C                             and rsjrst = enst
     ** Get the stat reporting/rating information
     C     rrky          setgt     prr01
     C     rrky1         readpe    prr01                                  65

     C                   eval      fmc401 = *blanks
     C                   eval      fma521 = *zeros
     C                   eval      fmdt01 = *zeros

    C*  Retrieve the MCO group name
    C*
     C     mgky1         setll     pmg01
     C     mgky          reade     pmg01                                  60
     C                   if        *in60 = *off and mgmco <> *blanks
     C                   eval      fmc401 = mgname

     C                   select
    C*
    C*  Check if Managed Care Credit is assigned under Subj Prem Cr 1A
     C                   when      (rr1acd = 9874 or rr1acd = 9843 or
     C                             rr1acd = 9873) and rrdspc <> *Zeros
     C                   eval      fma521 = elaspm
     C                   eval      srdti = elpfdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         fmdt01
    C*
    C*  Check if Managed Care Credit is assigned under Subj Prem Cr 1B
     C                   when      (rr1bcd = 9874 or rr1bcd = 9843 or
     C                             rr1bcd = 9873) and rrdspc <> *Zeros
     C                   eval      fma521 = elasps
     C                   eval      srdti = elpfdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         fmdt01
    C*
    C*  Check if Managed Care Credit is assigned under Deviation #1
     C                   when      (rrdv1c = 9874 or rrdv1c= 9843 or
     C                             rrdv1c= 9873) and rrdspc <> *Zeros
     C                   eval      fma521 = elasp2
     C                   eval      srdti = elpfdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         fmdt01
    C*
    C*  Check if Managed Care Credit is assigned under Deviation #2
     C                   when      (rrdv2c = 9874 or rrdv2c = 9843 or
     C                             rrdv2c = 9873) and rrdspc <> *Zeros
     C                   eval      fma521 = eldev2
     C                   eval      srdti = elpfdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         fmdt01
    C*
     C                   endsl
    
      /Free
       //* Write the output record
       Exsr WriteWorkFile;
      /End-Free
     C                   leave
      *
     C                   endif
      *
     C                   endif
      *
          //***** Beg Add *** 11/03/22 ******************************
          Endif;
          //***** End Add *** 11/03/22 ******************************
     C     elky2         reade     pel02                                  60
     C                   enddo

      *  If printing a policy page.  (Not called from W1054R)

     c                   call      'SRCHKPGM'
     c                   parm      'W1054R   '   #1pgm            10
     c                   parm      *BLANK        #1flag            1

     C                   if        #1flag = '0'

      *
      * Update the endorsement reporting file with the proper data.
      *
      * Find the unique ID number.
      *
     C                   MOVE      'P'           X1TYPE            1
     C                   Z-ADD     0             xxunid
     C*
     c                   move      *hival        xxdt
     C     pekey         setgt     ppe02
     C     pekey1        readpe    ppe02                                  60
     C                   If        *in60 = *off
     C                   z-add     peunid        xxunid
     C                   endif
     C*
     C                   movel     xxform        x1end#
     c     pekey2        chain     ppe01                              60
      *
      * Copy data into the record.
      *
     C                   If        *in60 = *off
      *
     c                   z-add     fma521        xxiprp
     c                   movel     fmc401        xxinop
     C                   eval      srdti = fmdt01
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxdate            7 0
     C                   move      xxdate        xxiefp
      *
     c                   move      *blanks       pedata
     c                   movel     E00409        pedata
     c                   movel     q1user        peuser
     c                   movel     q1pgm         pepgm
     c                   z-add     dstime        petime
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         pedt
      *
     C                   update    ppe01
      *
     C                   endif                                                  *in60 ifeq off
      *
     C                   endif
    
     C                   endsr
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     C                   endsr
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
    
     C     rrky          setgt     prr01
     C     rrky1         readpe    prr01                                  65
     C
     C                   eval      xxcram = 0
    
     C                   if        *in65 = *off
     C
     C                   select
    C*
    C*  Check if Managed Care Credit is assigned under Subj Prem Cr 1A
     C                   when      (rr1acd = 9874 or rr1acd = 9843 or
     C                             rr1acd = 9873) and rrdspc <> *Zeros
     C                   eval      xxcram = elaspm
    C*
    C*  Check if Managed Care Credit is assigned under Subj Prem Cr 1B
     C                   when      (rr1bcd = 9874 or rr1bcd = 9843 or
     C                             rr1bcd = 9873) and rrdspc <> *Zeros
     C                   eval      xxcram = elasps
    C*
    C*  Check if Managed Care Credit is assigned under Deviation #1
     C                   when      (rrdv1c = 9874 or rrdv1c = 9843 or
     C                             rrdv1c= 9873) and rrdspc <> *Zeros
     C                   eval      xxcram = elasp2
    C*
    C*  Check if Managed Care Credit is assigned under Deviation #2
     C                   when      (rrdv2c = 9874 or rrdv2c = 9843 or
     C                             rrdv2c= 9873) and rrdspc <> *Zeros
     C                   eval      xxcram = eldev2
    C*
     C                   endsl
    C
     C                   if        xxcram <> *zeros
     ** Found a division with a matching jurisdiction state and a non-zero
      ** managed care credit.
     C                   move      *on           crtfrm
     C                   endif

     C                   endif
    
     C                   endsr
