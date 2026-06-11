     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR12303A - IL Employee Leasing Endorsement
       //===============================================================*
       //                                                               *
       //    PROGRAM DESCRIPTION:                                       *
       //                                                               *
       //       THIS PROGRAM IS CALLED TO:                              *
       //         1) DETERMINE IF THIS ENDORSEMENT FORM SHOULD BE       *
       //            ATTACHED TO THE POLICY.                            *
       //         2) CREATE DATA INTO THE ENDORSEMENT FORM WORKFILE     *
       //            (WXFMP) PRIOR TO A FORM MERGE.                     *
       //                                                               *
       //                                                               *
       //****************************************************************
       //                                                               *
       // REVISIONS:                                                    *
       //                                                              *
       // 11/02/22 C:333 Project: WC2834                                *
       //                Endorsement Recognition of Cancellation        *
       //                                                              *
       // 10/06/17 C:062 Project: 150WAP                                *
       //                Remove peo stuff from valid                    *
       //                                                               *
       // 07/21/17 C:180 Project: 150WAP                                *
       //                Handle groups with combined PEOs.              *
       //                                                               *
       // 03/20/17  C:180 Project SY371                                 *
       //                 Make auto attach.                             *
       //                                                               *
       // 07/17/16 C:302 Project: SY371                                 *
       //                Scriptura phase 2                              *
       //                Clean up Program/Copybook                      *
       //****************************************************************

      /COPY SCOPYEN01F

      /COPY SCOPYEN01D
      /COPY WPRPEO
      //***** Beg Del *** 11/02/22 **********************************
      ***/Copy WPREMPLR
      //***** End Del *** 11/02/22 **********************************

     D qqForm          s             15A   Inz('WC120303A ')

     D addpgm          c                   const('SRXXXXXXX ')

     D SR12303A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR12303A        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1


      /COPY SCOPYEN01C

      *=========================================================================
      *  OTHINF - Subroutine to load form specific information
      *=========================================================================
     C     othinf        begsr
      /Free
       //Get Client's information

       setll (xxco#:xxfnd:xxfyr:xxemp#) pel02;
       reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;

       Dow not %eof;
       //***** Beg Add *** 11/02/22 ******************************
          xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
          If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
       //***** End Add *** 11/02/22 ******************************
             Chain (Elco#:ELrtcd) prs01;

             Select;
             When FNpeof = 'M' and ELemp# = 1;
             When FNpeof = 'S' and ELdiv = 0;

             Other;
                If RSjrst = ENst;
                   If ELpfdt <> GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
                      dsCLTinfo = GetCltInfo(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);
                      FM2001 = dsCltInfo.fedName;
                      FM2002 = dsCltInfo.name;
                      FMead1 = dsCltInfo.address1;
                      FMead2 = dsCltInfo.address2;
                      FMead3 = dsCltInfo.address3;
                      FMfein = %dec(dsCltInfo.fein:9:0);
                      Exsr WriteWorkFile;
                   Endif;
                Endif;
             Endsl;
          //***** Beg Add *** 11/02/22 ******************************
          Endif;
          //***** End Add *** 11/02/22 ******************************

          reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;
          Enddo;
      /End-Free
     C                   endsr
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     pgmstr        begsr
      /Free
          UseDfltAddParms = *Off;
          WriteInCopybook = *Off;
      /End-Free
     C                   endsr
      *=========================================================================
      *  PGMEND - End of program processing
      *=========================================================================
     C     pgmend        begsr
     C                   endsr
      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
      /Free
       Begsr Valid;
       //*** Begin Chg *** 10/06/17 *********************************************
       //  Exsr GetFnd;
       //  If fnpeof <> *blanks;
       //  //*** Begin Add **** 07/21/17 ***************************************
       //     Select;
       //     // Combined PEO Group - Master PEO client
       //     When FNpeof = 'C' and ELemp# = 1 and ELdiv <> *Zero;
       //        Crtfrm = *on;
       //
       //     // Combined PEO Group - MCP client
       //     When FNpeof = 'C' and ELemp# <> 1;
       //        Crtfrm = *on;
       //
       //     //Single Master PEO Group  - PEO client
       //     When FNpeof = 'S' and ELdiv <> *Zero;
       //        Crtfrm = *on;
       //
       //     //Single MCP PEO Group - MCP client
       //     When FNpeof = 'M' and ELemp# <> 1;
       //        Crtfrm = *on;
       //     Endsl;
       //  //*** End   Add **** 07/21/17 ***************************************
       //  Endif;
           crtfrm = *On;
       //*** End   Chg *** 10/06/17 *********************************************
       Endsr;
      /End-Free
