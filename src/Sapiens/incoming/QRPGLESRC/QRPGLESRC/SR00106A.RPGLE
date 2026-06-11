     Hcopyright('(c) 2025 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *===============================================================*
    H/TITLE SR00106A - Longshore and Harbor WC Act Converage Endorsement
     *===============================================================*
     * WC000106A - Longshore & Harbor Workers' Comp Act Coverage     *
     *             Endorsement                                       *
     *                                                               *
     * PROGRAM DESCRIPTION:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * CREATED   : 10/06/98                                          *
     * PROGRAMMER: C:114                                             *
     * PROJECT   : CM027                                             *
     *                                                               *
     * REWRITE   : 12/02/99                                          *
     * PROGRAMMER: C:113                                             *
     * PROJECT   : CM027                                             *
     *===============================================================*
     *  REVISIONS:                                                   *
      *                                                               *
      * 08/21/25 C:333 Support: 148/117703  (Recompile)               *
      *                For single print endorsements provide specific *
      *                group and employer so that correct sig is found*
      *                                                               *
      * 01/27/23 C:291 Project: WC2863                                *
      *                State Include/Exclude Endorsement Print Control*
      *                                                               *
     * 10/18/22 C:333 Project: WC2834                                *
      *                Endorsement Recognition of Cancellation        *
      *                                                               *
     * 04/19/19 C:300 Support: 77030 (LUBA)                          *
      *                1) Correct Validation Criteria                 *
      *                2) Resolve WC000106A form to iDARTS under      *
      *                division 0 (Changes in SCOPYEN01C copybook)    *
      *                                                               *
     * 02/15/19 C:300 Support: 77030 (LUBA) (Recompile)              *
      *                Printing end from Work with End Forms          *
      *                Changes in SCOPYEN01C copybook                 *
      *                                                                        *
     * 09/29/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.
      *               -No comments.
     *                                                               *
     * 10/19/07 C:184 PROJECT: WC1595                                *
     *                This endorsement will automatically attach to  *
      *                any policy that has harbor classes or has      *
      *                classes defined as having harbor exposure      *
      *                                                               *
     * 06/04/07 C:189 PROJECT: 099/SUPP#58380                        *
     *                Endorsement print/attach programs should not   *
      *                write/update WXPEP records if single           *
     *                endorsement print request                      *
     *                                                               *
     *                                                               *
     * 01/22/04 C:65  PROJECT: 114WAK/SUPP#40498                     *
     *                Change processing to get current date/time id  *
      *                from chaining out to wxpel1 in date/time order.*
     *                Populate maintenance fields on wxpep.          *
     *                                                               *
     * 09/09/03 C:65  PROJECT: 114WAK/SUPP#37919                     *
     *                CHANGE PROCESSING TO GET CURRENT TIME/DATE ID. *
     *                                                               *
     *  04/30/01 C:65  PROJECT: 114WAK                               *
     *                 Add processing to write endorsement data to   *
     *                 the WXPEP reporting file for PA/DE/NJ states. *
     *                                                               *
     *  08/25/00 C:09  PROJECT: CM027                                *
     *                 Correct Form ID #                             *
     *                                                               *
     *  12/02/99 C:113 PROJECT: CM027                                *
     *                 Rewrite to use RPGIV and copybook.            *
     *                                                               *
     *  04/29/99 C:113 PROJECT:                                      *
     *                 If F3 or F12 is pressed, pass back an 'F'     *
     *                 through QQCALL.                               *
     *                                                               *
     *  10/09/98 C:09 PROJECT CM027                                  *
     *                ONLY ACCUMULATE PREMIUM FOR A GIVEN FUND YEAR. *
     *                                                               *
     * ------------------------------------------------------------- *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
     * ------------------------------------------------------------- *
     *===============================================================*
     * NOTES:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/D/C).          *
     *                                                               *
     *  qqmode = 1 - Called to determine if form should be attached. *
     *  qqmode = 2 - Called to print endorsement.                    *
     *  qqmode = 3 - Called for additional input.                    *
     *  qqmode = 4 - Called for midterm processing.                  *
     *===============================================================*
     Fwxpel     uf a e           k disk
     Fststl3    if   e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)
     FWMEPL9    if   e           k disk
     FWMPRL1    if   e           k disk
     FWDHEL     if   e           k disk

     ** Copybook F specs
     F/COPY SCOPYEN01F
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
     D qqForm          s             15a   inz('WC000106A ')
     D xxunid          s                   like(peunid)
     D x1end#          s                   like(peend#)

     ** Additional entry program
     D addpgm          c                   const('SRXXXXXX  ')
     D*
     D                 DS
     D  #state                        7    dim(54)
     D  stat                          2    overlay(#state:1)
     D  statp                         5  3 overlay(#state:*next)
     D*
      *entry parameters
     D SR00106A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR00106A        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
     ** Additional Input file
     C     fmKey3        klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxForm

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

     C     stky          KList
     C                   KFld                    xxkey            10
     C*
     C* SUBMISSION HISTORY FILE
     C*
     C     SHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR

     C                   KFLD                    X1TYPE
     c     epkey         klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELFYR
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV

     c     prky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELRTCD
     c                   kfld                    EPCLAS
     c                   kfld                    ELPFDT

     c     prky1         klist
     c                   kfld                    ELCO#
     c                   kfld                    ELRTCD
     c                   kfld                    EPCLAS

     c     Heky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    RSJRST
     c                   kfld                    ELPFDT
     C
     c     Heky1         klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    RSJRST
     c
     C
     c     elky1         klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELFYR
     c                   kfld                    ELEMP#
    
     *=========================================================================
     *  Main
     *=========================================================================
    
     ** Copybook C specs (PLISTs, KLISTs and mainline)
     C/COPY SCOPYEN01C
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information
     *=========================================================================
     C     othinf        begsr
      /Free
       WriteInCopybook = *Off;
      /End-Free
    
     c                   z-add     0             i
     c     elky1         setll     pel02
     c     elky1         reade     pel02                                  60
     c*
     c                   Dow       *in60 = *off
     c*
        //***** Beg Add *** 10/18/22 ******************************
        xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
        If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
        //***** End Add *** 10/18/22 ******************************
     c*
     c     rsky          chain     prs01                              60
    c*
     C                   if        *in60 = *off
      //*** Beg Add ***** 01/27/23 *****************************
       Exsr ChkApprExclSt;
       If isApprExclSt = *On;
      //*** End Add ***** 01/27/23 *****************************
     C*
     c     epKey         setll     pep01
     c     epKey         reade     pep01                                  60
     c*
     c                   Dow       *in60 = *off

     c                   If        epstat <> 'I'
     c*
     c     prky          setgt     ppr01
     c     prky1         readpe    ppr01                                  60
     c*
     c                   If        *in60 = *off and prstat = 'A'
     c
     c                   If        Epspec ='F' or
     c                             Prlhcd ='Y'
     C*
     c     heky          setgt     phe01
     c     heky1         readpe    phe01                                  60
     c*
     C                   if        *in60 = *off
     c*
     c                   eval      i = 1
     c     hest          lookup    stat(i)                                60
     c                   if        *In60 = *off
     c                   eval      i = 1
     c                   Dow       i <= 54
     c*
     c                   if        stat(i) = ' '
     c                   eval      stat(i) = hest
     c                   eval      statp(i) = helhnf
     c                   leave
     c                   endif
     c                   eval      i = i+1
     c                   enddo
     c                   endif
    
     C                   endif
     c*
     C                   endif
     c*
     C                   endif
     c*
     C                   endif
     c*
     c     epKey         reade     pep01                                  60
     c*
     c                   enddo
     c
      //*** Beg Add ***** 01/27/23 *****************************
       Endif;
      //*** End Add ***** 01/27/23 *****************************
     c                   endif
        //***** Beg Add *** 10/18/22 ******************************
        Endif;
        //***** End Add *** 10/18/22 ******************************
     c     elky1         reade     pel02                                  60
     c                   enddo
     c* write records in out file.
     c                   eval      i = 1
     c                   sorta     #state
     c                   dow       i <= 54
     c*
     c                   if        stat(i) <> ' '
     c*1st record
     c                   eval      fmc101 = stat(i)
     c                   eval      fma521 = statp(i)
     c                   eval      i=i+1
     c*2nd record
     c                   if        i <= 54
     c                   eval      fmc102 = stat(i)
     c                   eval      fma522 = statp(i)
     c                   eval      i=i+1
     c                   endif
     c*3rd record
     c                   if        i <= 54
     c                   eval      fmc103 = stat(i)
     c                   eval      fma523 = statp(i)
     c                   eval      i=i+1
     c                   endif
     c*4th record
     c                   if        i <= 54
     c                   eval      fmc104 = stat(i)
     c                   eval      fma524 = statp(i)
     c                   eval      i=i+1
     c                   endif
     c*5th record
     c                   if        i <= 54
     c                   eval      fmc105 = stat(i)
     c                   eval      fma525 = statp(i)
     c                   eval      i=i+1
     c                   endif
     c*6th record
     c                   if        i <= 54
     c                   eval      fmc106 = stat(i)
     c                   eval      fma526 = statp(i)
     c                   eval      i=i+1
     c                   endif
     c*7th record
     c                   if        i <= 54
     c                   eval      fmc107 = stat(i)
     c                   eval      fma527 = statp(i)
     c                   eval      i=i+1
     c                   endif
     c*8th record
     c                   if        i <= 54
     c                   eval      fmc108 = stat(i)
     c                   eval      fma528 = statp(i)
     c                   eval      i=i+1
     c                   endif
     c*9th record
     c                   if        i <= 54
     c                   eval      fmc109 = stat(i)
     c                   eval      fma529 = statp(i)
     c                   eval      i=i+1
     c                   endif
     c*10th record
     c                   if        i <= 54
     c                   eval      fmc110 = stat(i)
     c                   eval      fma52a = statp(i)
     c                   eval      i=i+1
     c                   endif
     c*

     c                   EXSR      updrpt
      /Free
       //* Write the output record
       Exsr WriteWorkFile;
      /End-Free
     c                   Exsr      clrFld
     c                   endif
     c*
     c                   eval      i = i+1
     c                   enddo
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
     c     epKey         setll     pep01
     c     epKey         reade     pep01                                  60
     c*
     c                   Dow       *in60 = *off

     c                   If        epstat <> 'I'
     c*
     c     prky          setgt     ppr01
     c     prky1         readpe    ppr01                                  60
     c*
     c                   If        *in60 = *off and prstat = 'A'
     c
     c                   If        Epspec ='F' or
     c                             Prlhcd ='Y'
     c
     C     rsky          chain     prs01                              60
    
     C                   if        *in60 = *off
     C*
     c     heky          setgt     phe01
     c***** beg chg ***** 04/19/19 **************************
     c*****              readp     phe01                                  60
     c     heky1         readpe    phe01                                  60
     c***** end chg ***** 04/19/19 **************************
     c*
     C                   if        *in60 = *off
     C*
      ** This is a manual attach endorsement with additional input.
     C                   move      *on           crtfrm
     C                   endif
     c*
     C                   endif
     c*
     C                   endif
     c*
     C                   endif
     c*
     C                   endif
     c*
     c     epKey         reade     pep01                                  60
     c*
     c                   enddo
    
     C                   endsr
     *=========================================================================
     *  UPDPRT - Subroutine to update WXPEL
     *=========================================================================
     C     updrpt        begsr
     C*
      *  If printing a policy page.  (Not called from W1054R)

     c                   call      'SRCHKPGM'
     c                   parm      'W1054R   '   #1pgm            10
     c                   parm      *BLANK        #1flag            1

     C                   if        #1flag = '0'
      *
      * Update the endorsement reporting file with the proper data.
      *
      *
      * Find the unique ID number.
      *
     C                   MOVE      'P'           X1TYPE            1
     C                   Z-ADD     0             xxunid
     c                   move      *hival        xxdt
     C     pekey         setgt     ppe02
     C     pekey1        readpe    ppe02                                  60
     C                   If        *in60 = *off
     C                   z-add     peunid        xxunid
     C                   endif
     C                   movel     xxform        x1end#
     c     pekey2        chain     ppe01                              60
      *
      * Copy data into the record.
      *
     C                   If        *in60 = *off
      *
     c                   move      *zeros        E00106
      *
     c                   z-add     fma521        xxp01
     c                   z-add     fma522        xxp02
     c                   z-add     fma523        xxp03
     c                   z-add     fma524        xxp04
     c                   z-add     fma525        xxp05
     c                   z-add     fma526        xxp06
     c                   z-add     fma527        xxp07
     c                   z-add     fma528        xxp08
     c                   z-add     fma529        xxp09
     c                   z-add     fma52a        xxp10
      *
     C                   movel     fmc101        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxs01
     c                   endif
     C                   movel     fmc102        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxs02
     c                   endif
     C                   movel     fmc103        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxs03
     c                   endif
     C                   movel     fmc104        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxs04
     c                   endif
     C                   movel     fmc105        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxs05
     c                   endif
     C                   movel     fmc106        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxs06
     c                   endif
     C                   movel     fmc107        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxs07
     c                   endif
     C                   movel     fmc108        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxs08
     c                   endif
     C                   movel     fmc109        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxs09
     c                   endif
     C                   movel     fmc110        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxs10
     c                   endif
      *
     c                   move      *blanks       pedata
     c                   movel     E00106        pedata
     c                   movel     q1user        peuser
     c                   movel     q1pgm         pepgm
     c                   z-add     dstime        petime
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         pedt
      *
     C                   update    ppe01
     C                   endif
      *
     C                   endif
     C                   endsr
     *=========================================================================
     *  CLRFLD - Subroutine to clear fields
     *=========================================================================
     C     clrfld        begsr
     C*
     c                   move      *blanks       fmc101
     c                   move      *blanks       fmc102
     c                   move      *blanks       fmc103
     c                   move      *blanks       fmc104
     c                   move      *blanks       fmc105
     c                   move      *blanks       fmc106
     c                   move      *blanks       fmc107
     c                   move      *blanks       fmc108
     c                   move      *blanks       fmc109
     c                   move      *blanks       fmc110
     c                   move      *Zeros        fma521
     c                   move      *Zeros        fma522
     c                   move      *Zeros        fma523
     c                   move      *Zeros        fma524
     c                   move      *Zeros        fma525
     c                   move      *Zeros        fma526
     c                   move      *Zeros        fma527
     c                   move      *Zeros        fma528
     c                   move      *Zeros        fma529
     c                   move      *Zeros        fma52a
      *
     C                   endsr
    
