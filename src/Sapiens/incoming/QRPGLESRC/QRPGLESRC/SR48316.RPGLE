     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')
    
     /TITLE SR48316 - WI Employee Leasing Company Client Termination p/a
     *===============================================================*
     * PROGRAM DESCRIPTION:                                          *
     *                                                               *
     *  Print/Attach for Multiple Manual with Additional Input       *
     *  Endorsement (data stored in file specifed for endorsement)   *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     *===============================================================*
     * REVISIONS:                                                    *
      //                                                              *
      // 05/13/24 C:300 Support: 115141 (AmWins)                      *
      //                Change date format from mmddyy to yymmdd      *
      //                                                              *
      // 01/02/24 C:342 Project: WC2938                               *
      //                WC2938 - WCPOLS ED Record Type                *
      //                                                              *
      // 10/19/22 C:342 Project: WC2834 (Recompile)                   *
      //                Endorsement Recognition of Cancellation       *
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      //                                                               *
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

     Fwledl     if   e           k disk    prefix(p_)
      *** Begin Add *** 01/02/24 *****************************
     Fwxpel     uf a e           k disk
     Fwxpel1    if   e           k disk    rename(ppe01:ppe02)
      *** End   Add *** 01/02/24 *****************************

      /COPY SCOPYEN01F

     D xxdat2          s              8  0
     D qqForm          s             15    inz('WC480316  ')

      /COPY SCOPYEN01D
      *** Begin Add *** 01/02/24 *****************************************
     D/Copy wPrPeo
     D/COPY SPRSTRING
     D/Copy WWCPOLDAT4
     D xxunid          s                   like(PEUNID)
     D x1end#          s                   like(PEEND#)

     DSrCvtDAt         pr                   ExtPgm('SRCVTDAT')
     D srDti                          8  0
     D srFfmt                         8
     D srTfmt                         8
     D srDto                         10
     D srErr                          1
      *** End   Add *** 01/02/24 *****************************************

     D addpgm          c                   const('SR48316I  ')

     D SR48316         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR48316         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

     C     edky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxfyr
     C                   kfld                    xxform
     C*
     C     edky1         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxfyr
     C                   kfld                    xxform
     C                   kfld                    xxend#
    
      /COPY SCOPYEN01C
     C*** Begin Add ***  01/02/24  *************************************
     C     PEKEY         KLIST
     C                   KFLD                    xxco#
     C                   KFLD                    xxfnd
     C                   KFLD                    xxemp#
     C                   KFLD                    xxfyr
     C                   KFLD                    xxdt              7 0

     C     PEKEY1        KLIST
     C                   KFLD                    xxco#
     C                   KFLD                    xxfnd
     C                   KFLD                    xxemp#
     C                   KFLD                    xxfyr

     C     PEKEY2        KLIST
     C                   KFLD                    xxco#
     C                   KFLD                    xxfnd
     C                   KFLD                    xxemp#
     C                   KFLD                    xxfyr
     C                   KFLD                    xxunid
     C                   KFLD                    x1end#
     C*** End   Add ***  01/02/24  *************************************

      *=========================================================================
      *  OTHINF - Subroutine to load form specific information
      *=========================================================================
     C     othinf        begsr

       //*** Begin Add *** 01/02/24 *********************************
       i = 1;
       //*** End   Add *** 01/02/24 *********************************
     C     edky          setll     ped01
     C     edky          reade     ped01                                  60
    
     C                   dow       not %eof
      *       move information to WXFMP fields to print on endorsement
     C                   eval      fm2010= (p_fm1f01)+ (p_fm1f02)
     C                   eval      fm2011= (p_fm1f03)+ (p_fm1f04)
      * Line 1 will be populated if 1st line is empty and second line not blank
     C                   if        fm2010 = *blanks
     C                   eval      fm2010= (p_fm1f03)+ (p_fm1f04)
     C                   eval      fm2011=*blanks
     C                   endif

     C                   movel(p)  p_fm1f01      fm1f01
     C                   movel(p)  p_fm1f02      fm1f02
     C                   movel(p)  p_fm1f03      fm1f03
     C                   movel(p)  p_fm1f04      fm1f04

     C                   movel(p)  p_fm2001      fm2001
     C                   movel(p)  p_fm2002      fm2002
     C                   movel(p)  p_fm2003      fm2003

     C                   movel(p)  p_fm2004      fm2004
     C                   movel(p)  p_fm2005      fm2005
     C                   movel(p)  p_fm2006      fm2006
     C                   movel(p)  p_fm2007      fm2007
     C                   movel(p)  p_fm2008      fm2008
     C                   movel(p)  p_fm2009      fm2009

     c                   eval      xxdat2= *zeros
     c                   eval      srdti = p_fma701
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         xxdat2

     c                   if        xxdat2 <> *zeros
     c                   eval      FMC101 = %editc(xxdat2:'Y')
     c                   else
     c                   eval      FMC101 = *blanks
     c                   endif

     c                   eval      xxdat2= *zeros
     c                   eval      srdti = p_fma702
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         xxdat2

     c                   if        xxdat2 <> *zeros
     c                   eval      FMC102 = %editc(xxdat2:'Y')
     c                   else
     c                   eval      FMC102 = *blanks
     c                   endif

     c
      /Free
         WriteInCopybook = *Off;
         Exsr WriteWorkFile;
         //*** Begin Add *** 01/02/24 *********************************
         //* Get latest UNID
         //*
         xxDT = *hival;
         Setgt pekey ppe02;
         Readpe(n) pekey1 ppe02;
         If not %eof;
            xxUnId = PEunid;
            x1End# = %trim(qqform);
            Chain pekey2 ppe01;    //Copy data into record
            If %found;
               Clear e48316;

               x10EmpLname  = %trim(P_FM1f01) + %trim(P_FM1f02);
               x10Cltname   = %trim(P_FM2001);
               x10EntRcvF   = %trim(P_FM2004);

               srDto = *blanks;
               srDti  = P_FMa701;
               srFfmt = '*CYMD';
               //*** Begin Chg *** 05/13/24 *********************************
               //*** srTfmt = '*MDY';
               srTfmt = '*YMD';
               //*** End   Chg *** 05/13/24 *********************************
               SrCvtDAt(srDti:srFfmt:srTfmt:srDto:srErr);
               If srDto <> *blanks;
                 x10TEffDt = %Dec(srDto:6:0);
               Else;
                 x10TEffDt = 0;
               Endif;

               srDto = *blanks;
               srDti  = P_FMa702;
               SrCvtDAt(srDti:srFfmt:srTfmt:srDto:srErr);
               If srDto <> *blanks;
                 x10DtSent = %Dec(srDto:6:0);
               Else;
                 x10DtSent = 0;
               Endif;

               PEdata = %trim(e48316);

               PEName = %trim(ExtrAlphaNum(PEName));
               PEuser = q1User;
               PEpgm = q1Pgm;
               PEtime = %Dec(%Char(%Time():*iso0):6:0);
               PEdt = %Dec(%Char(%Date():*cymd0):7:0);

               If i = 1;
                  Update ppe01;
                  i = i + 1;
               Else;
                  PEiusr = q1User;
                  PEipgm = q1Pgm;
                  PEitim = %Dec(%Char(%Time():*iso0):6:0);
                  PEidt = %Dec(%Char(%Date():*cymd0):7:0);
                  Write ppe01; //WXPEP
               Endif;
            Endif;
         Endif;
              //*** End   Add *** 01/02/24 *************************
      /End-Free
     C                   write     pfm01
      *       clear WXFMP data fields used above.  Do NOT clear entire WXFMP
      *       record.  Example:
     C                   clear                   fm1f01
     C                   clear                   fm1f02
     C                   clear                   fmdt01
     C                   clear                   fmdt02
     C                   clear                   fm2001
     C                   clear                   fm2002
     C                   clear                   fm2003
     C                   clear                   fm2004
     C                   clear                   fm2005
     C                   clear                   fm2006
     C                   clear                   fm2007
     C                   clear                   fm2008
     C                   clear                   fm2009
     C                   clear                   fm2010
     C                   clear                   fm2011
    
      *
     C     edky          reade     ped01                                  60
     C                   enddo
    
     C                   endsr
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     pgmstr        begsr
      /Free
         UseDfltAddparms = *Off;
      /End-Free
     C                   endsr
      *=========================================================================
      *  PGMEND - End of program processing
      *=========================================================================
     C     pgmend        begsr
     C                   endsr
      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
     C     valid         begsr
     C                   move      *on           crtfrm
     C                   endsr
