     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR00405  - Policy Period Endorsement
       //===============================================================*
       // WC000405                                                      *
       //                                                               *
       // Program Description:                                          *
       //                                                               *
       //   Determines if this endorsement form should be attached to   *
       //   the policy and creates data in the Endorsement Form         *
       //   Workfile (WXFMP) prior to a form merge.                     *
       //                                                               *
       //===============================================================*
       // REVISIONS:                                                    *
       //                                                               *
       // 08/21/25 C:333 Support: 148/117703  (Recompile)               *
       //                For single print endorsements provide specific *
       //                group and employer so that correct sig is found*
       //                                                               *
       // 11/15/22 C:291 Project: WC2981                                *
       //                Add processing to write endorsement data to    *
       //                the WXPEP reporting file.                      *
       //                                                               *
       // 10/26/22 C:333 Project: WC2834 (Recompile)                    *
       //                Endorsement Recognition of Cancellation.       *
       //                                                               *
       // 07/17/16 C:302 Project: SY371                                 *
       //                Scriptura phase 2                              *
       //                Clean up Program/Copybook                      *
       //                                                               *
       //===============================================================*
       // Notes:                                                        *
       //                                                               *
       //  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
       //                                                               *
       //  QQMODE = 1 - Called to determine if form should be attached. *
       //  QQMODE = 2 - Called to print endorsement.                    *
       //  QQMODE = 3 - Called for additional input.                    *
       //  QQMODE = 4 - Called for mid-term processing.                 *
       //===============================================================*

      /COPY SCOPYEN01F

     Fwxfmp1    if   e           k disk    usropn
     F                                     rename(pfm01:pfm02)
      *** Beg Add ***** 11/15/22 *************************************
     Fwxpel     uf a e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)

      /copy WWCPOLDAT4
      *** End Add ***** 11/15/22 *************************************
      /COPY SCOPYEN01D

     D qqForm          s             15A   Inz('WC000405  ')
     D addpgm          c                   const('SR00405I  ')

     D SR00405         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR00405         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      *** Beg Add ***** 11/15/22 *************************************
     D* Check Pgm
     D SrChkPgm        pr                  ExtPgm('SRCHKPGM')
     D                               10
     D                                1

     D #1flag          s              1a
     D xxdt            s              7  0
     D xxunid          s                   like(PEunid)
     D x1end#          s                   like(PEend#)
     D x1Type          s              1

     C     pekey         KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxdt

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
      *** End Add ***** 11/15/22 *************************************
      /COPY SCOPYEN01C

      /Free

       //=========================================================================
       //  OTHINF - Subroutine to load form specific information & write record
       //=========================================================================
       Begsr othinf;

          Open wxfmp1;

          Chain (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv:xxForm) wxfmp1;
          //* Move the custom fields to print
          FMdt01 = F3dt01;
          FMdt02 = F3dt02;
          FMdt03 = F3dt03;
          FMdt04 = F3dt04;
          FMdt05 = F3dt05;
          FMdt06 = F3dt06;

          Close wxfmp1;

          //*** Beg Add *** 11/15/22 **************************************
          //Report the Information to WCPOLS: Write/Update WXPEP
          SrChkPgm(#1pgm:#1flag);
          If #1flag = '0';
             //Update the endorsement reporting file with the proper data.
             //Find the unique ID number.
             x1Type = 'P';
             xxDt = *hival;
             xxUnid = *zeros;
             Setgt pekey ppe02;
             Readpe pekey1 ppe02;
             If not %Eof(wxpel1);
                xxUnid = PEUnid;
             Endif;
             x1End# = xxForm;
             Chain pekey2 ppe01;
             If %found(wxpel);
                E00405 = *All'0';
                If F3dt01 > 0;
                   x5EfDt1 = %Char(%Date(F3dt01:*usa):*ymd0);
                Endif;
                If F3dt02 > 0;
                   x5EfDt2 = %Char(%Date(F3dt02:*usa):*ymd0);
                Endif;
                If F3dt03 > 0;
                   x5EfDt3 = %Char(%Date(F3dt03:*usa):*ymd0);
                Endif;
                If F3dt04 > 0;
                   x5ExDt1 = %Char(%Date(F3dt04:*usa):*ymd0);
                Endif;
                If F3dt05 > 0;
                   x5ExDt2 = %Char(%Date(F3dt05:*usa):*ymd0);
                Endif;
                If F3dt06 > 0;
                   x5ExDt3 = %Char(%Date(F3dt06:*usa):*ymd0);
                Endif;
                Clear PEdata;
                PEData = E00405;
                PEuser = q1user;
                PEpgm  = q1pgm;
                PEtime = %Dec(%Char(%Time():*iso0):6:0);
                PEdt   = %Dec(%Char(%Date():*cymd0):7:0);
                Update ppe01;
             Endif;
          Endif;
          //*** Beg Add *** 11/15/22 **************************************

       Endsr;
       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       Begsr pgmstr;
       Endsr;
       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       Begsr pgmend;
       Endsr;
       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       Begsr valid;
          crtFrm = *On;
       Endsr;

      /End-Free
