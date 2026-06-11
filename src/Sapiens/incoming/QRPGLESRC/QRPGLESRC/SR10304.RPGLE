     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR10304 - PEO Extension Endorsement
      //===============================================================*
      // WC100304
      //
      // PROGRAM DESCRIPTION:
      //   MULTIPLE MANUAL ADDITIONAL INFO stored in WDL3P
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      //===============================================================*
      // REVISIONS:                                                    *
      //                                                               *
      // 11/01/22 C:333 Project: WC2834 (Recompile)                    *
      //                Endorsement Recognition of Cancellation        *
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

     Fwdl3l     if   e           k disk

      /COPY SCOPYEN01F

     D                 ds
     D xxproj                        60
     D  fmc404                       40    overlay(xxproj:1)
     D  fmc204                       20    overlay(xxproj:41)

      /COPY SCOPYEN01D

     D qqForm          s             15A   Inz('WC100304  ')

     D addpgm          c                   const('SR10304I  ')

     D SR10304         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR10304         PI
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
       begsr othInf;
          //* Write the output record
          setll (xxCo#:xxFnd:xxFyr:xxEmp#) wdl3l;
          reade (xxCo#:xxFnd:xxFyr:xxEmp#) wdl3l;

          Dow Not %EOF(wdl3l);
             FMnm01 = L3nam1;
             FMc401 = L3adr1;
             FMc402 = L3adr2;
             xxZip = %Char(L3Zip);
             FMc403 = %Trim(L3cty) + ', ' + %Trim(L3st) + ' '
                      + %Trim(xxZip1);
             If xxzip2 <> *Blank And xxZip2 <> *zero;
                FMc403 = FMc403 + '-' + xxZip2;
             Endif;

             If fmc402 = *blank;
                fmc402 = fmc403;
                fmc403 = *blank;
             Endif;

             FMc101 = L3emst;
             xxProj = L3proj;

             WriteInCopybook = *Off;
             Exsr WriteWorkFile;

             reade (xxCo#:xxFnd:xxFyr:xxEmp#) wdl3l;
          Enddo;

       Endsr;

       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       begsr pgmstr;
          UseDfltAddParms = *off;
       endsr;
       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       begsr pgmend;
       endsr;
       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       begsr valid;
          //* This is a manual attach endorsement with additional input
          crtFrm = *On;
       endsr;

      /End-free
