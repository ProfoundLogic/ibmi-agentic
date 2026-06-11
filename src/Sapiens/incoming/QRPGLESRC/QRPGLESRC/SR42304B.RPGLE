     H Copyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     H/TITLE SR42304B - TX - Waiver to Recover Endorsement
      *------------------------------------------------------------- *
      * WC420304B - Texas Waiver of Our Right to Recover from Others  *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      * Created   : 04/24/14                                          *
      * Programmer: C: 282                                            *
      * Project   : WC2270                                            *
      *------------------------------------------------------------- *
      *Revisions:                                                    *
      *                                                               *
      * 08/21/25 C:333 Support: 148/117703  (Recompile)               *
      *                For single print endorsements provide specific *
      *                group and employer so that correct sig is found*
      *                                                               *
      * 05/24/23 C:333 Project: 148WEC                                *
      *                Auto Attach Waiver Form                        *
      *                                                               *
      * 10/19/22 C:342 Project: WC2834 (Recompile)                    *
      *                Endorsement Recognition of Cancellation        *
      *                                                               *
      * 02/02/22 C:136 Support: 99485                                 *
      *                Correct code added for WC2757.                 *
      *                F3500A1 is populated by SR42304BI and has the  *
      *                blanket waiver description. For specific       *
      *                waivers F3C401 and F3C402 has the operation    *
      *                description.                                   *
      *                                                               *
      * 02/02/21 C:136 Project: WC2757 Add Blanket Waiver             *
      *                Default verbiage for blanket waivers.          *
      *                Add F3 and F12 parms for midterm endorsements. *
      *                Change logic in Valid to match SR00313         *
      *                                                               *
      * 01/16/20 C:136 Support: 80310                                 *
      *!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*
      *!!! ALL THE OBJECTS FOR THIS SUPPORT ITEM MUST GO TOGETHER  !!!*
      *!!! AND FIX80310 MUST BE RUN.                               !!!*
      *!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*
      *               -Make sure midterm endorsement has a policy     *
      *                page endorsement if not already on the policy. *
      *               -Populate form name to LDA so W0815R can        *
      *                filter waiver detail by form.                  *
      *               -Ensure only waiver detail for this endorsement *
      *                prints on the schedule.                        *
      *                                                               *
      * 10/31/19 C:136 Support: 81117                                 *
      *                Pgm is writing an extra WXFMP1 record.         *
      *                                                               *
      * 01/16/19 C:300 Support: 73970 (LUBA) (Recompile)              *
      *                Printing end from Work with End Forms          *
      *                Changes in SCOPYEN01C copybook                 *
     *                                                               *
     * 09/15/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.
      *               -No comments.
      *                                                               *
      * 09/11/14 C:282 Support: 86622                                 *
      *                Added the condition to only print Waivers      *
      *                for TX state.                                  *
      *                                                               *
      *------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of revisions.           *
      *------------------------------------------------------------- *
      *Notes:                                                        *
      *                                                               *
      *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      *                                                               *
      *  QQMODE = 1 - Called to determine if form should be attached. *
      *  QQMODE = 2 - Called to print endorsement.                    *
      *  QQMODE = 3 - Called for additional input.                    *
      *  QQMODE = 4 - Called for mid-term processing.                 *
      *------------------------------------------------------------- *
      **Copybook F specs
     F/COPY SCOPYEN01F
      **Additional Information File
     Fwxfmp1    if   e           k disk    usropn
     F                                     rename(pfm01:pfm02)
      **Waiver Information File
     Fwmwvl     if   e           k disk
      *** beg add ***** 02/02/21 *********************
     Fwmwvl7    if   e           k disk
      *** end add ***** 02/02/21 *********************
     F                                     Rename(pwv01:pwv07)
     Fwmmula    if   e           k disk    rename(pmu01:pmu0a)
      *------------------------------------------------------------- *
      * Definition Specs
      *------------------------------------------------------------- *
     D/COPY SCOPYEN01D

     D qqForm          s             15a   inz('WC420304B ')
     D addpgm          c                   const('SR42304BI ')
     D xxefdt          s                   like(wvefdt)

      *entry parameters
     D SR42304B        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR42304B        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
      *------------------------------------------------------------- *
      * KLIST definitions
      *------------------------------------------------------------- *
    
      **Additional Input file
     C     fmky3         Klist
     C                   Kfld                    xxco#
     C                   Kfld                    xxfnd
     C                   Kfld                    xxfyr
     C                   Kfld                    xxemp#
     C                   Kfld                    xxdiv
     C                   Kfld                    xxForm
    
      **Waiver Information file
     C     wvky          Klist
     C                   Kfld                    xxco#
     C                   Kfld                    xxfnd
     C                   Kfld                    xxfyr
     C                   Kfld                    xxemp#
      *------------------------------------------------------------- *
      * Main
      *------------------------------------------------------------- *
     C/COPY SCOPYEN01C
      *---------------------------------------------------------------------- *
      * OTHINF -Subroutine to load form specific information & write record
      *---------------------------------------------------------------------- *
     C     othinf        Begsr
      /Free
       //*** Beg Del ***** 05/24/23 *******************************
          //*** WriteInCopybook = *Off;
       //*** End Del ***** 05/24/23 *******************************
      /End-Free
    
     C                   Open      wxfmp1
     C     fmky3         Chain     pfm02                              60
    
     C                   If        f3fl03 = 'S'
     C                   Move      'X'           fmfl01
     C                   Move      *blank        fmfl02
     C                   Else
     C                   Move      *blank        fmfl01
     C                   Move      'X'           fmfl02
     C                   Endif

     C                   Eval      fmc407 = f3c401
     C                   Eval      fmc408 = f3c402
     C                   Eval      fma921 = f3a921
     C                   Eval      fma521 = f3a521
     C                   Eval      fma923 = f3a923
    
      /Free
       Setll wvKy pwv01;
       Dou %Eof(WMWVL);
          Reade wvKy pwv01;
          If %Eof;
             Leave;
          Endif;
          If WVstat = 'I' Or WVenm <> qqForm;
             Iter;
          Endif;
          If LDenda <> *Blanks And LDenda <> *Zeros;
             If xxEnd# <> WVend#;
                Iter;
             Endif;
          Endif;

     C     wvname        Cat       wvnam2:0      fm2001
     C                   Movel     wvadr1        fmc402

     C                   If        wvadr2 <> *blank
     C                   Movel     wvadr2        fmc403
     C                   Move      wvzip         xxzip
     C     wvcty         Cat       ',':0         fmc404
     C                   Cat       wvst:1        fmc404
     C                   Cat       xxzip1:1      fmc404

     C                   If        xxzip2 <> *blank
     C                             and xxzip2 <> *zero
     C                   Cat       '-':0         fmc404
     C                   Cat       xxzip2:0      fmc404
     C                   Endif

       //*** beg del ***** 02/02/22 *********************
     C***                   Movel     wvproj        fmc405
     C***                   Movel     wvprj2        fmc406
       //*** beg del ***** 02/02/22 *********************

     C                   Else

     C                   Move      wvzip         xxzip
     C     wvcty         Cat       ',':0         fmc403
     C                   Cat       wvst:1        fmc403
     C                   Cat       xxzip1:1      fmc403

     C                   If        xxzip2 <> *blank
     C                             and xxzip2 <> *zero
     C                   Cat       '-':0         fmc403
     C                   Cat       xxzip2:0      fmc403
     C                   Endif
      *** beg del ***** 02/02/21 ***************************
     C***                   Movel     wvproj        fmc404
     C***                   Movel     wvprj2        fmc405
     C***                   Movel     *blanks       fmc406
      *** end del ***** 02/02/21 ***************************

     C                   Endif                                                  wvadr2 <> blanks
      /Free
       //*** beg chg ***** 02/02/22 *********************
       //*** beg add ***** 02/02/21 *********************
        // Load operations description
       // FMc601 = WVproj;
       // FMc602 = WVprj2;
       // FMblk01 = WVblkPrjDs;
       //*** end add ***** 02/02/21 *********************
        FMblk01 = F3500A1;
       //*** end chg ***** 02/02/22 *********************
          //* Write the output record
        Exsr WriteWorkFile;
       //*** Beg Add ***** 05/24/23 *******************************
          WriteInCopybook = *Off;
       //*** End Add ***** 05/24/23 *******************************
      /End-Free

     C                   Move      *blanks       fm2001
     C                   Move      *blanks       fmc402
     C                   Move      *blanks       fmc403
     C                   Move      *blanks       fmc404
     C                   Move      *blanks       fmc405
     C                   Move      *blanks       fmc406
      *** beg add ***** 02/02/21 **************************
       FMblk01 = *Blanks;
      *** end add ***** 02/02/21 **************************

     C                   Enddo

     C                   Close     wxfmp1

      /Free
       // Ensure waiver policy page endorsement exists
       If xxEnd# > 0;
          Chain (xxCo#:xxFnd:xxEmp#:xxFyr:xxForm:0) pmu0a;
          If Not(%Found);
             MUend# = 0;
             MUseq# = 0;
             Open WMMUL4;
             Write pmu01;
             Close WMMUL4;
          Endif;
       Endif;
      /End-Free

     C                   Endsr
      *---------------------------------------------------------------------- *
      * PGMSTR -Beginning of program processing
      *---------------------------------------------------------------------- *
     C     pgmstr        Begsr

      /Free
       In svLDA;
       //*** beg add ***** 02/02/21 *************
       ldStSpcfFlg = 'Y';
       //*** end add ***** 02/02/21 *************
       ldWaiverFrm = xxForm;
       Out svLDA;
      /End-Free
    
     C                   Endsr
      *---------------------------------------------------------------------- *
      * PGMEND -End of program processing
      *---------------------------------------------------------------------- *
       //*** beg chg ***** 02/02/21 **************************
     C***     pgmend        Begsr
     C***                   Endsr
      /Free
       // Return 'F' to WTPOLMEND if TX Waiver is canceled in SR42304BI
       Begsr PgmEnd;
        If qqCall = 'F';
           qqMod2 = 'F';
        Endif;
       Endsr;
      /End-Free
       //*** end chg ***** 02/02/21 **************************
      *---------------------------------------------------------------------- *
      * VALID -Subroutine to attach this form to the policy
      *---------------------------------------------------------------------- *
      *** beg chg ***** 02/02/21 ***************************
     C***  valid         Begsr
     C***     wvky          Setll     pwv01
     C***     wvky          Reade     pwv01                                  60
     C***     *in60         Doweq     *off
     C***                   If        wvstat <> 'I'
     C***                   Move      *on           crtfrm
     C***                   Leave
     C***                   Endif
     C***     wvky          Reade     pwv01                                  60
     C***                   Enddo
     C***                Endsr
      // Need crtFrm = *On for midterm endorsements
       // WVStat no longer used. Excluded states applicable to
       //  NCCI forms only. Just need to find one record
       //  added by the midterm endorsement.
       Begsr Valid;
        Chain (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr:LDend#) pwv07;
        If %Found;
           crtFrm = *On;
        Endif;
       Endsr;
      *** end chg ***** 02/02/21 ***************************

