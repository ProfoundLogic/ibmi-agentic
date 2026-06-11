     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR17305  - LA - PEO Exclusion Endorsement
      //===============================================================*
      // WC170305  - LOUISIANA PROFESSIONAL EMPLOYER ORGANIZATION      *
      //             EXCLUSION ENDORSEMENT                             *
      // PROGRAM DESCRIPTION:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //===============================================================*
      // Revisions:                                                    *
      //                                                               *
      // 11/03/22 C:333 Project: WC2834 (Recompile)                    *
      //                Endorsement Recognition of Cancellation        *
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

     Fwlpol2    if   e           k disk

      /COPY SCOPYEN01F

      /COPY SCOPYEN01D

     D qqForm          s             15A   Inz('WC170305  ')
     D addpgm          c                   const('SR17305M  ')

     D SR17305         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR17305         PI
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

          setll (xxCo#:xxFnd:xxFyr:xxEmp#) wlpol2;
          reade (xxCo#:xxFnd:xxFyr:xxEmp#) wlpol2;

          If Not %EOF(wlpol2);

             FMc401 = LEcnam;
             FMc402 = LEadr1;

             FMc403 = LEadr2;
             xxZip = %Char(LEzip);
             FMc404 = %Trim(LEcty) + ', ' + %Trim(LEst) + ' '
                      + %Trim(xxZip1);
             If xxzip2 <> *Blank And xxZip2 <> *zero;
                FMc404 = FMc404 + '-' + xxZip2;
             Endif;

             If fmc403 = *blank;
                fmc403 = fmc404;
                fmc404 = *blank;
             Endif;

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
          crtFrm = *On;
       Endsr;

      /End-Free
