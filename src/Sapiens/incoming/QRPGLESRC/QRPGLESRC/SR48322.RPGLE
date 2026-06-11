     *===============================================================*
     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     *===============================================================*
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR48322 - WI - Employee Leasing Company Leased Emp Incl.
     *===============================================================*
     * WC480322 - WI - Employee Leasing Company Leased Emp Incl.     *
     *                                                               *
     * PROGRAM DESCRIPTION:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * CREATED   : 08/05/21                                          *
     * PROGRAMMER: C:079                                             *
     * PROJECT   : WC2814                                            *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     *  08/21/25 C:333 Support: 148/117703  (Recompile)              *
     *                 For single print endorsements provide specific*
     *                 group and employer so that correct sig is found
     *                                                               *
     *  10/19/22 C:342 Project: WC2834 (Recompile)                   *
     *                 Endorsement Recognition of Cancellation       *
     *                                                               *
     *  10/25/22 C:344 103957                                        *
     *                 Populate the EI record type data into the     *
     *                 PEDATA field in WXPEP record.                 *
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

     ** Copybook F specs
     F/COPY SCOPYEN01F
      *** Begin Add *** 10/25/22 *** 103957 ***************************
     FWXPEL     UF A E           K DISK
     FWXPEL1    IF   E           K DISK    RENAME(PPE01:PPE02)
      *** End   Add *** 10/25/22 *** 103957 ***************************
     fwxfml12   if   e           k Disk    Rename(pfm01:pfm02)
     *=========================================================================
     *  Definition Specs
     *=========================================================================
     d strZip          s             12
     d strCity         s                   Like(FMeAd1)
     d strSt           s                   Like(FMeAd2)
     ** Copybook D specs
     D/COPY SCOPYEN01D
      *** Begin Add *** 10/25/22 *** 103957 *******************************
     D/Copy wPrPeo
     D/COPY SPRSTRING
     D/Copy WWCPOLDAT4
     D xxunid          s                   like(PEUNID)
     D x1end#          s                   like(PEEND#)
      *** End   Add *** 10/25/22 *** 103957 *******************************
     *=========================================================================
     *  Copybook required constants
     *=========================================================================

     ** Form ID
     D qqForm          c                   const('WC480322  ')

     ** Additional entry program
     D addpgm          c                   const('SR48322I  ')
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
     C*** Begin Add ***  10/25/22  *** 103957 **************************
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
     C*** End   Add ***  10/25/22  *** 103957 **************************
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information
     *=========================================================================
       Begsr OthInf;

        Exsr OpenWorkFile;
        //---------------------------------------------------------------
        //Manual Attach: Write the output record for Manual attached form
        //---------------------------------------------------------------

        i = 1;
        If ENmnAu = 'M';
           Setll (xxCo#:xxFnd:xxEmp#:xxFyr:xxForm) pfm02;
           Reade (xxCo#:xxFnd:xxEmp#:xxFyr:xxForm) pfm02;
           Dow not %eof(wxfml12);
              FM2001 = F3500A1;  // Labor Contractor
              FMfein = F3fein;   // FEIN
              FMcPl# = F3cPl#;   // Carrier Policy Number
              FMeAd1 = F3eAd1;   // Address 1
              FMeAd2 = F3eAd2;   // Address 2
              // City
              If F3c301 = *blanks;
                 strCity = '0';
              Else;
                 strCity = F3c301;
              Endif;
              // State
              If F3emSt = *blanks;
                 strSt = '0';
              Else;
                 strSt = F3emSt;
              Endif;
              // Zip Code
              If F3am01 <> 0;
                strZip = %Char(F3am01*100);
                strZip = %Subst(strZip:1:5) + '-' + %Subst(strZip:6:4);
              Else;
                 strZip = '0';
              Endif;
              FMeAd3 = %Trim(strCity) + ',' + %Trim(strSt) + ',' +
               %Trim(strZip);
              Exsr WriteMultiRcd;
              //*** Begin Add *** 10/25/22 *** 103957 **********************
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
                    Clear e48322;
                    xiCName = %trim(ExtrAlphaNum(FM2001)); // Labor Contr
                    xiCPol# = %subst(FMcPl#:1:18); // Carr Policy#
                    xiCFein = %editc(FMfein : 'X'); // FEIN

                    // Address
                    If FMeAd2 <> ' ';
                       xiCAdr = %trim(FMeAd1) + ' ' + %trim(FMeAd2);
                    Else;
                       xiCAdr = %trim(FMeAd1);
                    Endif;

                    xiCCity = strCity; // City
                    xiCSt   = strSt;   // State
                    xiCZip  = strZip; // Zip Code

                    PEData = %trim(e48322);

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
              //*** End   Add *** 10/25/22 *** 103957 **************
              Reade (xxCo#:xxFnd:xxEmp#:xxFyr:xxForm) pfm02;
           Enddo;
        Endif;

        Exsr CloseWorkFile;

       Endsr;
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
       writeInCopybook = *off;
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
     C                   move      *on           crtfrm
    
     C                   endsr
