     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')

    H/TITLE SR42605 - TX - Deductible Endorsement
     //=======================================================================*
     // WC420605 - Texas Deductible Endorsement                               *
     //                                                                       *
     // Program Description:                                                  *
     //                                                                       *
     //   Determines if this endorsement form should be attached to           *
     //   the policy and creates data in the Endorsement Form                 *
     //   Workfile (WXFMP) prior to a form merge.                             *
     //                                                                       *
     // Created   : 05/12/14                                                  *
     // Programmer: C:302                                                     *
     // Project   : WC2269                                                    *
     //=======================================================================*
     // REVISIONS:                                                            *
      *                                                                        *
      * 10/19/22 C:342 Project: WC2834                                         *
      *                Endorsement Recognition of Cancellation                 *
      *                                                                        *
      * 05/11/22 C:65  Project: Phw#101990 (Sunz)                              *
      *                Only process states that match the one the endorsement  *
      *                setup indicates.                                        *
      *                                                                        *
      * 03/16/21 C:65  Project: Phw#93467 (Sunz)                               *
      *                Implement logic to store data for type 43 rcds.         *
     *                                                                        *
      * 01/27/20 C:180 Project WC2726
      *                Texas Ded Endorsement Attachment Controls      *
     *                                                               *
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
     //* Copybook F specs
     Fwtddl2    if   e           k disk
     Fwtabl     if   e           k disk    include(pbs01)
     Fwmldl1    if   e           k disk
     Fwtrtp     if   e           k disk
     F*** begin add ***  03/16/21  ***************************************
     Fwxpel     uf a e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)
     Fststl3    if   e           k disk
     F*** end add *****  03/16/21  ***************************************
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
     D qqForm          s             15a   inz('WC420605  ')
     //* Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
     D xxampo          s                   like(FMam01)
     D xxamag          s                   like(FMam01)
     D xxDedSize       s              1
     d*** begin add ***  03/16/21  ***************************************
     D xxunid          s                   like(peunid)
     D x1end#          s                   like(peend#)
     d xxpremPct       s             12  9 inz(0)
     d xxlossSub       s              2  0 inz(0)
     d xxbasisDed      s              2  0 inz(0)
     d xxdedPct        s              2  0 inz(0)
     d xxdedAmt        s              9  0 inz(0)
     d xxdedAgg        s              9  0 inz(0)
     d xxdedClas       s              5  0 inz(0)
     d xxdedNet        s              1    inz(' ')
     d xxdedVerb       s              1    inz(' ')
     d xxPlan          s              1    inz(' ')
     d*** end add *****  03/16/21  ***************************************

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

      *entry parameters
     D SR42605         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR42605         PI
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
     d*** begin add ***  03/16/21  ***************************************
      //***** Beg Del *** 10/19/22 **********************************
     D****/COPY WPREMPLR
      //***** End Del *** 10/19/22 **********************************
      /COPY WWCPOLDAT4

     C     stky          KList
     C                   KFld                    xxkey            10
     C     pekey         KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxdt              7 0
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
     d*** end add *****  03/16/21  ***************************************
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
          //*** begin add ***  05/11/22  *************************************
           Chain (xxCo#:ELrtcd) prs01;
    
           If %Found and RSjrst = ENst;
          //*** end add *****  05/11/22  *************************************
             xxDedSize = rtvDedSize (ELco#:ELbscd);
             Select;
             When xxDedSize = 'L';
                rtvLgDedInfo (xxAmpo: xxAmag);
             When xxDedSize = 'S';
                rtvSmDedInfo (xxAmpo: xxAmag);
             EndSl;

             If xxAmpo <> 0;
                FMa701 = xxAmpo;
                //* Write the output record
                Exsr WriteWorkFile;
             //*** Begin Add *** 03/16/21 *********************
                Exsr WriteWCPOLS;
             //*** End Add ***** 03/16/21 *********************
                Leave;
             Endif;
          //*** begin add ***  05/11/22  *************************************
           Endif;
          //*** end add *****  05/11/22  *************************************
    
             //***** Beg Add *** 10/19/22 ******************************
             Endif;
             //***** End Add *** 10/19/22 ******************************
             Reade (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
          Enddo;
    
       Endsr;
      //=======================================================================
      //  PGMSTR - Beginning of program processing
      //=======================================================================
       Begsr pgmstr;
          LoadMedicalCodes();
       Endsr;
      //=======================================================================
      //  PGMEND - End of program processing
      //=======================================================================
       Begsr pgmend;
       Endsr;
      //=======================================================================
      //  VALID - Subroutine to attach this form to the policy
      //=======================================================================
       Begsr valid;
    
          Chain (xxCo#:ELrtcd) prs01;
    
          If %Found and RSjrst = ENst;
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
          Endif;
    
       Endsr;

      /End-free
     c*** begin add ***  03/16/21  ******************************************
      *=========================================================================
      *  WriteWCPOLS - record information for electronic data record
      *=========================================================================
     C     WriteWCPOLS   Begsr
      *  If printing a policy page.  (Not called from W1054R)

     c                   call      'SRCHKPGM'
     c                   parm      'W1054R   '   #1pgm            10
     c                   parm      *BLANK        #1flag            1

     C                   if        #1flag = '0'

     C                   Clear                   xxpremPct
     C                   Clear                   xxlossSub
     C                   Clear                   xxbasisDed
     C                   Clear                   xxdedPct
     C                   Clear                   xxdedAmt
     C                   Clear                   xxdedAgg
     C                   Clear                   xxdedClas
     C                   Clear                   xxdedNet
     C                   Clear                   xxdedVerb
     C                   Clear                   xxPlan
      /free
       xxPlan = RtvDeductible(elco#:elfnd:elemp#:eldiv:elfyr:xxPremPct:
        xxDedAmt:xxDedAgg:xxLossSub:xxBasisDed:xxDedNet:xxDedVerb:
        xxDedClas:xxDedPct);
      /end-free
      *
      * Find the unique ID number.
      *
     C                   MOVE      'P'           X1TYPE            1
     C                   Z-ADD     0             xxunid
     C*
     c                   move      *hival        xxdt
     C     pekey         setgt     ppe02
     C     pekey1        readpe    ppe02                                  60
     C                   If        *in60 = *off
     C                   z-add     peunid        xxunid
     C                   endif
     C                   movel     xxform        x1end#
     c     pekey2        chain     ppe01                              60
      *
      * Copy data into the record.
      *
     C                   If        *in60 = *off
      *
     c                   movel(p)  rsjrst        xxkey
     c     stky          chain     pst01                              60
     c                   if        *in60 = *off
     c                   move      stnst         PEST
     c                   endif
      *
     c                   clear                   E00403
     C                   eval      xxdac = xxDedAmt
     C                   eval      xxprp = xxPremPct
     C                   eval      xxlsd = xxLossSub
     C                   eval      xxbdc = xxBasisDed
     C                   eval      xxdpc = xxDedPct
     C                   eval      xxdag = xxDedAgg
     c                   move      *blanks       pedata
     c                   movel     E00403        pedata
     c                   movel     q1user        peuser
     c                   movel     q1pgm         pepgm
     c                   z-add     dstime        petime
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         pedt
      *
     C                   Update    ppe01
      *
     C                   Endif                                                  *in60 ifeq off
      *
     C                   Endif
      *
     C                   Endsr
      *=========================================================================
     c*** end add *****  03/16/21  ******************************************
      //=======================================================================
      // Query whether policy is small or large deductible
      //=======================================================================
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
    P rtvDedSize      e

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

       //*** Begin Change *** 01/27/20 *************************************
       //If not %EOF;

       Medical = *Off;
       Another = *Off;
       ampo = *Zeros;
       amag = *Zeros;

       If not %EOF and  DDplan = '01';
       //*** End   Change *** 01/27/20 *************************************
          //  Check to see what kind of reserve codes the deductible has
          //  If the reserve code is in the RC array, that means, it is
          //  a medical Reverve Code.
          //  If the reserve code is not in the RC array, that means, it is
           // another type of Reverve Code;
           For index = 1 to 10;
             If RC(index) <> *Blanks;
                If %lookup(RC(index):MedicalCodes) > *ZEROS;
                   Medical = *On;
                Else;
                   Another = *On;
                Endif;
             Endif;
          Endfor;

          If Medical and Another;
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
       //*** Begin Change *** 01/27/20 *******************************
       //If %found;
       Medical = *Off;
       Another = *Off;
       ampo = *Zeros;
       amag = *Zeros;
       If %found and (LDPLN1 = '01' or LDPLN2 = '01');
       //*** End   Change *** 01/27/20 *******************************
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

          If Medical and Another;
             ampo = LDamp1;
             amag = LDamg1;
          Endif;

       Endif;

      /End-free
    P rtvLgDedInfo    e

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
             Index += 1;
          Endif;
          Read prt01;
       Enddo;
      /End-free
     P LoadMedicalCodes...
     P                 E

