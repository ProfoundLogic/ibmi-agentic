     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR17304  - LA - PEO Endorsement
      //===============================================================*
      // WC170304  - LOUISIANA PROFESSIONAL EMPLOYER ORGANIZATION      *
      //             ENDORSEMENT                                       *
      // PROGRAM DESCRIPTION:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //===============================================================*
      // Revisions:                                                    *
      //                                                               *
      // 04/27/23 C:291 Support: 96512                                 *
      //                Only attach to PEO policies when set as        *
      //                auto-attach and load the state as 'LA'.        *
      //                                                               *
      // 11/03/22 C:333 Project: WC2834 (Recompile)                    *
      //                Endorsement Recognition of Cancellation        *
      //                                                               *
      // 05/30/18 C:136 Support: 63952                                 *
      //                Auto populate OTHINF so that endorsement can   *
      //                auto attach (Sunz only)                        *
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      //                                                               *
      //===============================================================*
      // NOTES:                                                        *
      //                                                               *
      //  Utilizes the endorsement copybook (SCOPYEN01F/D/C).          *
      //                                                               *
      //  qqmode = 1 - Called to determine if form should be attached. *
      //  qqmode = 2 - Called to print endorsement.                    *
      //  qqmode = 3 - Called for additional input.                    *
      //  qqmode = 4 - Called for midterm processing.                  *
      //===============================================================*
     Fwlpel2    if   e           k disk

      /COPY SCOPYEN01F

     D                 ds
     D state                          2
     D  fmfl01                        1    overlay(state)
     D  fmfl02                        1    overlay(state:2)
     D  fmcd01                        2    overlay(state)

      /COPY SCOPYEN01D
      /Copy wprPeo

     D qqForm          s             15A   Inz('WC170304  ')
     D addpgm          c                   const('SR17304M  ')

     D SR17304         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR17304         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      /COPY SCOPYEN01C

      /Free

       //=========================================================================
       //  OTHINF - Subroutine to load form specific information
       //=========================================================================
       Begsr othinf;

          //*** Beg Add ***** 04/27/23 ********************
          If ENMNAU = 'M';
          //*** End Add ***** 04/27/23 ********************
             setll (xxCo#:xxFnd:xxFyr:xxEmp#) wlpel2;
             reade (xxCo#:xxFnd:xxFyr:xxEmp#) wlpel2;

             If Not %EOF(wlpel2);

                FMnm01 = LPnam1;
                FMc401 = LPadr1;

                FMc402 = LPadr2;
                xxZip = %Char(LPzip);
                FMc403 = %Trim(LPcty) + ', ' + %Trim(LPst) + ' '
                         + %Trim(xxZip1);
                If xxzip2 <> *Blank And xxZip2 <> *zero;
                   FMc403 = FMc403 + '-' + xxZip2;
                Endif;

                state = LPemst;
                FMc404 = LPproj;

                If fmc402 = *blank;
                   fmc402 = fmc403;
                   fmc403 = *blank;
                Endif;
             //*** Beg Add ***** 04/27/23 ********************
             Endif;
             //*** End Add ***** 04/27/23 ********************

          Else;
             dsPeoInfo = GetPeoInfo(xxco#:xxfnd);
             FMnm01 = dsPeoInfo.Name;
             FMc401 = dsPeoInfo.Address1;
             FMc402 = dsPeoInfo.Address2;
             FMc403 = dsPeoInfo.Address3;
             If FMc402 = *blank;
                FMc402 = fmc403;
                FMc403 = *blank;
             Endif;
             FMc404 = 'Any and All';
             //*** Beg Add ***** 04/27/23 ********************
             state = 'LA';
             //*** End Add ***** 04/27/23 ********************

          Endif;

       Endsr;
       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       Begsr pgmstr;
          Exsr getfnd;
          UseDfltAddparms = *Off;
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
          //*** Beg Chg ***** 04/27/23 ********************
          //***crtFrm = *On;
          Select;
          When ENMNAU = 'M';
             crtFrm = *On;
          When ENMNAU = 'A';
             If FNPEOF <> *Blank;
                crtFrm = *On;
             Endif;
          Endsl;
          //*** End Chg ***** 04/27/23 ********************
       Endsr;

      /End-Free
