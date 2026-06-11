     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR04493 - CA Minimum Payroll for Officers or Partners End
     *===============================================================*
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     *===============================================================*
     * REVISIONS:
      *                                                               *
     * 10/31/22 C:333 Project: WC2834                                *
      *                Endorsement Recognition of Cancellation
      *                                                               *
      * 02/28/20 C:180 Support: 87315
      *                Remove loop(s) on Policy Detail.
      *
      * 12/23/19 C:180 Support: 72461
      *                Convert back to Auto-Attach.
     *
     * 06/20/18 C:062 Project: WC2576
     *                Add call to additional input program.
      *                Convert to Manual Attach
     *
     * 01/11/11 C:190 Support:75410
     *                Corrected Valid subroutine.
     *
     * 01/22/10 C:220 Support:WC1439/71195
     *                Created.
     *
     * 07/03/06 C:61  PROJECT WC1439
     *                Created.
     *
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
     F*** Begin Del *** 06/21/18 **********************************
      ** Endorsement Forms Information
     F*** wmnxl     if   e           k disk
    
      ** Endorsement Forms Document Print Work File
     F*** wxfmp3    o    e             disk    usropn
     F*** End   Del *** 06/21/18 **********************************
      *** Begin Delete *** 12/23/19 *******************************
     F*** Begin Add *** 06/21/18 **********************************
     F***wxfmp1    if   e           k disk    usropn
     F***                                     rename(pfm01:pfm02)
      *** End   Delete *** 12/23/19 *******************************
     F*** End   Add *** 06/21/18 **********************************
     F*** Begin Add *** 12/23/19 **********************************
      ** Endorsement Forms Information
     Fwmnxl     if   e           k disk
     F*** End   Add *** 12/23/19 **********************************
    
    
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     * Copybook D specs
     D/COPY SCOPYEN01D

     d xxdat7          s                   like(elpfdt)


     *=========================================================================
     *  /COPY required constants
     *=========================================================================

     * Form ID
     D qqform          c                   const('WC040493  ')

     * Additional entry program
     D addpgm          c                   const('SR04493I  ')



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
     C     nxky          KList
     C                   KFld                    ELCO#
     C                   KFld                    ELFND
     C                   KFld                    RSJRST
     C                   KFld                    XXDAT7
      *
     C     nxky1         KList
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
     C*** Begin Del *** 06/21/18 **********************************
     ** open work file
     C**********         open      wxfmp3
    
    C*****initialize
     C**********         clear                   pfm03
     c**********         movel(p)  xxform        fmform
    
     c***** Beg Add  **** 01/22/10  *****************************************
     c**********         z-add     elpfdt        xxdat7
     c***** End Add  **** 01/22/10  *****************************************
    
     ** Gather Info to print on Form
     c*****rsky*         chain     prs01
    
     c**********         if        %found
     c**********                   and rsjrst = 'CA'

     c*****nxky*         setgt     pnx01
     c*****nxky1         readpe    pnx01

     c**********         dow       (not %eof)
     c**********                   and nxstat = 'I'
     c*****nxky1         readpe    pnx01
     c**********         enddo

     c**********         if        not %eof
     c**********         eval      fmam01 = nxllmt
     c**********         eval      fmam02 = nxhlmt
     C**********         write     pfm03
     c**********         write     pfm01
     c**********         endif
    C**********
     c**********         endif
    
     *
     ** Close the endorsement form workfile
     C**********         close     wxfmp3
     C*** End   Del *** 06/21/18 **********************************
     C*** Begin Add *** 06/21/18 **********************************
       //*** Begin Change *** 12/23/19 *********************************
       //   open wxfmp1;
       //   chain (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv:xxForm) wxfmp1;
       //   FMam01 = F3am01;
       //   FMam02 = F3am02;
       //   close wxfmp1;
       //C     elky2         Chain     pel02                              20
       //C                   Dow       *in20 = *off
       //C     rsky          Chain     prs01                              20
      *
      /Free
       setll elky2 pel02;
       reade elky2 pel02;
       Dow not %eof(WDELL1);
       //***** Beg Add *** 10/31/22 ******************************
          xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
          If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
       //***** End Add *** 10/31/22 ******************************
             Chain rsky prs01;
             If %found(WMRSL) and rsjrst = 'CA';
                Exsr Valid;
                If not %eof(WMNXL);
                   FMam01 =  NXllmt;
                   FMam02 =  NXhlmt;
                   Exsr WriteWorkFile;
                   Leave;
                Endif;
             Endif;
          //***** Beg Add *** 10/31/22 ******************************
          Endif;
          //***** End Add *** 10/31/22 ******************************
          reade elky2 pel02;
       Enddo;
      /End-Free
     C*** End   Add *** 06/21/18 **********************************
       //C     elky2         ReadE     pel02                                  20
       //C                   EndDo
       //*** End    Change *** 12/23/19 *********************************
    
     C                   endsr
    
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr

     c                   time                    dsdttm

     c***** Beg Del  **** 01/22/10  *****************************************
     c********           eval      srdti = dsdate
     c********           call      'SRCVTDAT'    dtcymd
     c********           movel     srdto         xxdat7
     c***** End Del  **** 01/22/10  *****************************************
     /Free
       //*** Begin Add *** 12/23/19 ***********************************
       WriteInCopybook = *Off;
       //*** End n Add *** 12/23/19 ***********************************
      /End-Free
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
     **** Begin Change *** 12/23/19 ********************************************
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     c*     valid         begsr
    
     C*** Begin Chg *** 06/21/18 **********************************
     c*****rsky*         chain     prs01
    
     c**********         if        %found
     c**********                   and rsjrst = 'CA'

     c***** Beg Add  **** 01/11/11  *****************************************
     c**********         z-add     elpfdt        xxdat7
     c***** End Add  **** 01/11/11  *****************************************

     c*****nxky*         setgt     pnx01
     c*****nxky1         readpe    pnx01

     c**********         dow       (not %eof)
     c**********                   and nxstat = 'I'
     c*****nxky1         readpe    pnx01
     c**********         enddo

     c**********         if        not %eof
     c**********         move      *on           crtfrm
     c**********         endif
    
     c**********         endif
     c*                   move      *on           crtfrm
     C*** End   Chg *** 06/21/18 **********************************
     c*                   endsr
      /Free
       //-----------------------------------------------------------------------
       //   VALID - Subroutine to attach this form to the policy
       //-----------------------------------------------------------------------
       Begsr Valid;
       xxdat7 = elpfdt;
       setgt nxky pnx01;
       readpe nxky1 pnx01;

       Dow not %eof(wmnxl) and nxstat = 'I';
          readpe nxky1 pnx01;
       Enddo;

       If not %eof(wmnxl);
          crtfrm = *On;
       Endif;
       Endsr;
     /End-Free
     **** End Change *** 12/23/19 ********************************************
