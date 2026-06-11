     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')

     H/TITLE SR52601A - HI - Medical Benefits Deductible Endorsement
     //=======================================================================*
     // WC520601.A - HI Medical Benefits Deductible Endorsement               *
     //                                                                       *
     // Program Description:                                                  *
     //                                                                       *
     //   Determines if this endorsement form should be attached to           *
     //   the policy and creates data in the Endorsement Form                 *
     //   Workfile (WXFMP) prior to a form merge.                             *
     //                                                                       *
     // Created   : 10/16/14                                                  *
     // Programmer: C:282                                                     *
     // Project   : WC2309                                                    *
     //=======================================================================*
     // REVISIONS:                                                            *
     *                                                                        *
     * 10/19/22 C:342 Project: WC2834                                         *
     *                Endorsement Recognition of Cancellation                 *
     *                                                                        *
     * 09/15/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.
      *               -No comments.
     //=======================================================================*
     // Notes:                                                                *
     //                                                                       *
     //  Utilizes the endorsement copybook (SCOPYEN01F/I/C).                  *
     //                                                                       *
     //  QQMODE = 1 - Called to determine if form should be attached.         *
     //  QQMODE = 2 - Called to print endorsement.                            *
     //  QQMODE = 3 - Called for additional input.                            *
     //  QQMODE = 4 - Called for mid-term processing.                         *
     //=======================================================================*
      //* Copybook F specs
     Fwtddl2    if   e           k disk
     Fwtabl     if   e           k disk    include(pbs01)
     Fwmldl1    if   e           k disk
     Fwtrtp     if   e           k disk
     F/COPY SCOPYEN01F
     //========================================================================
     //  Data structures and constants
     //========================================================================
     //* Copybook D specs
     D/COPY SCOPYEN01D
     //========================================================================
     //  /COPY required constants
     //========================================================================
     //* Form ID
     D qqForm          s             15a   inz('WC520601A ')
     //* Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
     D xxampo          s                   like(FMam01)
     D xxamag          s                   like(FMam01)
     D xxDedSize       s              1

      * Reserve code for this policy
     D ReserveCodesL   DS
     D  LDR101                        2a
     D  LDR102                        2a
     D  LDR103                        2a
     D  LDR104                        2a
     D  LDR105                        2a
     D  LDR106                        2a
     D  LDR107                        2a
     D  LDR108                        2a
     D  LDR109                        2a
     D  LDR110                        2a
     D   LRC                          2a   Overlay(ReserveCodesL) Dim(10)

     D ReserveCodes    DS
     D  DDDC01                        2a
     D  DDDC02                        2a
     D  DDDC03                        2a
     D  DDDC04                        2a
     D  DDDC05                        2a
     D  DDDC06                        2a
     D  DDDC07                        2a
     D  DDDC08                        2a
     D  DDDC09                        2a
     D  DDDC10                        2a
     D   RC                           2a   Overlay(ReserveCodes) Dim(10)

     D MedicalCodes    s              2a   Dim(50)

     D SR52601A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR52601A        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

     //========================================================================
     //  Prototypes
     //========================================================================
     D rtvDedSize      pr             1
     D  co#                                like(ELco#)
     D  bscd                               like(ELbscd)
     D rtvSmDedInfo    pr
     D  ampo                               like(FMam01)
     D  amag                               like(FMam01)
     D rtvLgDedInfo    pr
     D  ampo                               like(FMam01)
     D  amag                               like(FMam01)
     D EffectiveDate   pr                       extpgm('SREFFD')
     D  qqCo#                         3
     D  qqFnd                         3
     D  q1Fyr                         3
     D  q1Emp#                        9
     D  q1Div                         5
     D  q2Date                        7

     D*--------------------------------------------------
     D* Procedure name: LoadMedicalCodes
     D* Purpose:        Load reserve codes type M
     D* Returns:
     D*--------------------------------------------------
     D LoadMedicalCodes...
     D                 PR

      /Free

       //=====================================================================
       //  Main
       //=====================================================================

       //* Copybook C specs
        /COPY SCOPYEN01C

       //=====================================================================
       //  OTHINF - Subroutine to load form specific information & write record
       //=====================================================================
       begsr othinf;
    
          WriteInCopybook = *Off;
    
          setll (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
          reade (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
    
          Dow not %EOF;
             //***** Beg Add *** 10/19/22 ******************************
             xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
             If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
             //***** End Add *** 10/19/22 ******************************
    
             xxDedSize = rtvDedSize (ELco#:ELbscd);
             Select;
             When xxDedSize = 'L';
                rtvLgDedInfo (xxAmpo: xxAmag);
             When xxDedSize = 'S';
                rtvSmDedInfo (xxAmpo: xxAmag);
             EndSl;

             If xxAmpo <> 0;
                FMa701 = xxAmpo;
                // Write the output record in WXFMP.
                Exsr WriteWorkFile;
                Leave;
             Endif;
    
             //***** Beg Add *** 10/19/22 ******************************
             Endif;
             //***** End Add *** 10/19/22 ******************************
             Reade (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
          Enddo;
    
       Endsr;
       //=====================================================================
       //  PGMSTR - Beginning of program processing
       //=====================================================================
       Begsr pgmstr;
          LoadMedicalCodes();
       Endsr;
       //=====================================================================
       //  PGMEND - End of program processing
       //=====================================================================
       Begsr pgmend;
       Endsr;
       //=====================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=====================================================================
       Begsr valid;
    
             xxDedSize = rtvDedSize (ELco#:ELbscd);
             Select;
             When xxDedSize = 'L';
                rtvLgDedInfo (xxAmpo: xxAmag);
             When xxDedSize = 'S';
                rtvSmDedInfo (xxAmpo: xxAmag);
             Endsl;
             If xxAmpo <> 0;
                crtfrm = *On;
             Endif;
    
       Endsr;

      /End-free
       //=====================================================================
       // Query whether policy is small or large deductible
       //=====================================================================
     P rtvDedSize      b

     D rtvDedSize      pi             1
     D  co#                                like(ELco#)
     D  bscd                               like(ELbscd)

     D kkco#           s                   like(BSco#)
     D kkkey           s                   like(BSkey)

      /Free
       Chain (xxCo#:ELrtcd) prs01;
       If not(%found);
          RSjrst = *blank;
          Return ' ';
       Endif;

       kkco# = co#;
       kkkey = %Char(bscd);
       Chain (kkco#:kkkey) pbs01;
       Select;
       When BSlpgm = 'L';
          Return 'L';
       Other;
          Return 'S';
       Endsl;

      /End-free
     P rtvDedSize      e

     //========================================================================
     // rtvSmDedInfo  - retrieves DDAMPO and DDAMAG
     //========================================================================
     P rtvSmDedInfo    b
     D rtvSmDedInfo    pi
     D  ampo                               like(FMam01)
     D  amag                               like(FMam01)
     D  kkefdt         s                   Like(DDefdt)
     D  q1Emp#         s              9
     D  q1Div          s              5
     D  q1Fyr          s              3
     D  q2Date         s              7

     D  Medical        s               N
     D  Another        s               N
     D  index          s              2  0
      /Free

       qqCo# = %EditC(ELco#:'X');
       qqFnd = %EditC(ELfnd:'X');
       q1Emp# = %EditC(ELemp#:'X');
       q1Div = %EditC(ELdiv:'X');
       q1Fyr = %EditC(ELfyr:'X');
       q2Date = %EditC(ELdcef:'X');

       EffectiveDate(qqCo#:qqFnd:Q1Fyr:q1Emp#:q1Div:q2Date);

       kkefdt = %Dec(q2date:7:0);
       Setgt (ELco#:ELfnd:RSjrst:ELdcde:kkefdt) pdd01;
       Readpe (ELco#:ELfnd:RSjrst:ELdcde) pdd01;
       Dow not(%eof) and DDstat <> 'A';
          Readpe (ELco#:ELfnd:RSjrst:ELdcde) pdd01;
       Enddo;

       If not %EOF;
          //  Check to see what kind of reserve codes the deductible has
          //  If the reserve code is in the RC array, that means, it is
          //  a medical Reverve Code.
          //  If the reserve code is not in the RC array, that means, it is
          //  another type of Reverve Code.
          For index = 1 to 10;
             If RC(index) <> *Blanks;
                If %lookup(RC(index):MedicalCodes) > *ZEROS;
                   Medical = *On;
                Else;
                   Another = *On;
                Endif;
             Endif;
          Endfor;

          If Medical and not Another;
                ampo = DDampo;
                amag = DDamag;
           Endif;
       Endif;

      /End-free
     P rtvSmDedInfo    e
     //========================================================================
     // rtvLgDedInfo - retrieves LDAMPO and LDAMAG
     //========================================================================
     P rtvLgDedInfo    b
     D rtvLgDedInfo    pi
     D  ampo                               like(FMam01)
     D  amag                               like(FMam01)

     D  Medical        s               N
     D  Another        s               N
     D  index          s              2  0
      /Free

       Chain (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pld01;
       If %found;
          //  Check to see what kind of reserve codes the deductible has
          //  If the reserve code is in the LRC array, that means, it is
          //  a medical Reverve Code.
          //  If the reserve code is not in the LRC array, that means, it is
          //  another type of Reverve Code.
          For index = 1 to 10;
             If LRC(index) <> *Blanks;
                If %lookup(LRC(index):MedicalCodes) > *ZEROS;
                   Medical = *On;
                Else;
                   Another = *On;
                Endif;
             Endif;
          Endfor;

          If Medical and not Another;
             ampo = LDamp1;
             amag = LDamg1;
          Endif;
       Endif;

      /End-free
     P rtvLgDedInfo    e

      //--------------------------------------------------
      // Procedure name: LoadMedicalCodes
      // Purpose:        Load reserve codes type 'M'
      //--------------------------------------------------
     P LoadMedicalCodes...
     P                 B
     D LoadMedicalCodes...
     D                 PI
     D index           s              2  0 Inz(1)
      /Free

       Read prt01;
       Dow not %EOF;
          If RTtype = 'M';
             MedicalCodes(index) = RTkey;
             index += 1;
          Endif;
         Read prt01;
       Enddo;
      /End-free
     P LoadMedicalCodes...
     P                 E
