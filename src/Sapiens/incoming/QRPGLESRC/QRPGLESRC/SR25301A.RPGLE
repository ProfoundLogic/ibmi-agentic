     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')
      /TITLE SR25301A - MT Employee Leasing/Professional Employer End.
      //===============================================================*
      // WC250301A  - Montana Employee Leasing/Professional Employer   *
      //              Endorsement                                      *
      // PROGRAM DESCRIPTION:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //===============================================================*
      // Revisions:                                                    *
      //                                                               *
      // 11/03/22 C:333 Project: WC2834                                *
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
      /COPY SCOPYEN01F
      /COPY SCOPYEN01D
      /COPY wPRPEO

     D qqForm          s             15A   Inz('WC250301A ')
     D addpgm          c                   const('SRXXXXXXXX')

     D SR25301A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR25301A        PI
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
       Begsr Othinf;

       Setll (xxco#:xxfnd:xxfyr:xxemp#) pel02;
       Reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;
       Dow Not %Eof;
       //***** Beg Add *** 11/03/22 ******************************
          xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
          If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
       //***** End Add *** 11/03/22 ******************************
             Chain (Elco#:ELrtcd) prs01;
             If RSjrst = ENst;
                dsCLTinfo = GetCltInfo(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);
                FM2001 = dsCltInfo.fedName;
                FM2002 = dsCltInfo.name;
                FMead1 = dsCltInfo.address1;
                FMead2 = dsCltInfo.address2;
                FMead3 = dsCltInfo.address3;
                Exsr WriteWorkFile;
             Endif;
          //***** Beg Add *** 11/03/22 ******************************
          Endif;
          //***** End Add *** 11/03/22 ******************************
       Reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;
       Enddo;

       Endsr;
       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       Begsr Pgmstr;
        WriteInCopybook = *Off;
       Endsr;
       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       Begsr Pgmend;

       Endsr;
       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       Begsr Valid;
       crtFrm = *On;
       Endsr;
      /End-Free
