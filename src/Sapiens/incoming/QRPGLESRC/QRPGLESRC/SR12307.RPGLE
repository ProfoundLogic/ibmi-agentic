     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR12307 - IL Exclusions endorsement
      //===============================================================*
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      //===============================================================*
      // REVISIONS:                                                    *
      //                                                               *
      // 11/02/22 C:333 Project: WC2834 (Recompile)                    *
      //                Endorsement Recognition of Cancellation        *
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of revisions.           *
      // ------------------------------------------------------------- *
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

     FWledl     if   e           k disk
     F                                     Prefix(T)

      //* Form ID
     D qqForm          s             15A   Inz('WC120307  ')
     d xxFrm1          s                   like(FMform)

      //* Additional entry program
     D ADDPGM          C                   CONST('SR12307I  ')

      /COPY SCOPYEN01D

     D SR12307         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR12307         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      /Free

        /COPY SCOPYEN01C

       //=========================================================================
       //  OTHINF - Subroutine to load form specific information & write record
       //=========================================================================

       Begsr OTHINF;

          WriteInCopybook = *Off;

          XXfrm1 = XXform;
          Setll (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr:xxFrm1) wledl;
          Reade (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr:xxFrm1) wledl;

          Dow Not %Eof(wledl);

             FM2001 =  TFM2001;
             FM2002 =  TFM2002;
             FM2003 =  TFM2003;
             FM2004 =  TFM2004;
             FM2005 =  TFM2005;
             FM2006 =  TFM2006;
             FM2007 =  TFM2007;
             FM2008 =  TFM2008;
             FM2009 =  TFM2009;
             FM2010 =  TFM2010;
             FM2011 =  TFM2011;
             FM2012 =  TFM2012;
             FM2013 =  TFM2013;
             FM2014 =  TFM2014;
             FM2015 =  TFM2015;
             FM2016 =  TFM2016;
             FM2017 =  TFM2017;
             FM2018 =  TFM2018;
             FM2019 =  TFM2019;
             FM2020 =  TFM2020;
             FM2021 =  TFM2021;
             FM2022 =  TFM2022;
             FM2023 =  TFM2023;
             FM2024 =  TFM2024;
             FM2025 =  TFM2025;
             FM2026 =  TFM2026;
             FM2027 =  TFM2027;
             FM2028 =  TFM2028;

             // Write the output record in WXFMP.
             Exsr WriteWorkFile;

             Reade (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr:xxFrm1) wledl;

          EndDo;
       Endsr;
       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       Begsr PGMSTR;
       Endsr;
       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       Begsr PGMEND;
       Endsr;
       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       Begsr VALID;
          crtFrm = *on;
       Endsr;
      /End-free
