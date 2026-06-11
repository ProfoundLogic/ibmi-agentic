     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SRLRGDED - Deductible Form Attach/Print Program
     *===============================================================*
     * PROGRAM DESCRIPTION:                                          *
     *                                                               *
     *  Any Deductible Form                                          *
     *                                                               *
     *  Print/Attach for Multiple Manual with Additional Input       *
     *  Endorsement (data stored in file specifed for endorsement)   *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     *===============================================================*
     * Created on 04/12/2018 by c:62 for Project 150WCX              *
     *===============================================================*
     * Revisions:                                                    *
      //                                                               *
      // 02/04/25 C:340 Project: 148WEW                                *
      //                Add Form WC990660 for AmWins.                  *
      //                                                              *
      // 08/28/24 C:340 Project: 150WGA (Sunz)                        *
      //                WC995410: XML and Type 43 Rcds.               *
      //                                                              *
      // 01/27/23 C:291 Project: WC2863                                *
      //                State Include/Exclude Endorsement Print Control*
      //                                                               *
      // 12/05/22 C:291 Project: 153WAC                               *
      //                Add Form WC991011 for Vital Services.         *
      //                                                              *
      // 09/21/22 C:291 Project: 150WFT                               *
      //                Add Form WC990403 and WC992003.               *
      //                                                              *
      // 02/09/22 C:062 Project 150WFQ                                *
      //                Add WC995203 and WC990678.                    *
      //                                                              *
      // 11/18/20 C:180 Project WC2757                                *
      //                Recompiled only.                              *
      //                                                              *
      // 09/22/20 C:136 Support: 91588                                *
      //                Revert percent field to use LDADPx.           *
      //                                                              *
      // 03/23/20 C:136 Project: 150WEI (80839) Not commented         *
      //               -Apply excluded states logic to printing of    *
      //                schedule. Rewrite of OTHINF                   *
      //               -Electronic report for each state on policy.   *
      //               -Also changed percent field to use LDPCTx      *
      //                instead of LDADPx.                            *
      //               -Sunz request to print just one form per       *
      //                policy. Implemented logic to specify form by  *
      //                client.                                       *
      //                                                              *
      // 01/21/20 C:136 Project: 150WEI                               *
      //                Update for new forms                          *
      //                                                              *
      // 02/28/19 C:295 Support: 76362                                *
      //                Large Deductible endorsement duplicating      *
      //                on policies                                   *
      //                                                              *
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
     fwtabl     if   e           k disk    Include(pbs01) Rename(pbs01:pbs00)
     fwmldl1    if   e           k disk
     *=========================================================================
     *  Definition Specs
     *=========================================================================
     D SRLRGDED        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                               15

     D* Determine if policy is large deductible
     D SRLRGDED        PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqForm                       15

     D* Determine if Large or Small Deductible
     D RtvDedType      pr             1                                         Returns S/L

     D* Retrieve Large Deductible Amounts
     D RtvLgDedInfo    pr
     D  ampo                               like(FMAM01)
     D  amag                               like(FMAM02)
     D  pctag                              like(LDPCT1)

     ** Copybook D specs

     D/COPY SCOPYEN01D
     D/COPY WPRPEO
      //*** Beg Del ***** 01/27/23 **************************
     D***/Copy WprEmplr
      //*** End Del ***** 01/27/23 **************************
     D/COPY SPRSTRING
     D/Copy wWCPOLDAT4

     *=========================================================================
     *  Copybook required constants
     *=========================================================================
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXX  ')
     D qqRrn           s              9  0
     D qqMod2          s              1
     D xxAmpo          s                   Like(FMAM01)
     D xxAmag          s                   Like(FMAM02)
     D xxPctAg         s                   like(LDPCT1)
     d aryLdSt         s              2    dim(50)
     d intIdx          s              3  0
     d flgApprOrExcl   s              1
     d cltIdForms      s             11    ctdata dim(50)
     *=========================================================================
     *  Main
     *=========================================================================
     C/COPY SCOPYEN01C
      /Free
       //========================================================================
       //  OthInf - Subroutine to load form specific information
       //========================================================================
       BegSR OthInf;

        flgApprOrExcl = *Blank;
        Chain (MUco#:MUform) pes01;
        If %Found;
           flgApprOrExcl = ESapex;
        EndIf;

        Setll (xxco#:xxfnd:xxfyr:xxemp#) pel02;
        Dou %Eof(WDELL1);
           Reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;
           If %Eof;
              Leave;
           Endif;

          // Write out one record per state
           Chain (Elco#:ELrtcd) prs01;
           intIdx = %LookUp(RSjrst:aryLdSt);
           If intIdx <> 0;
              Iter;
           Else;
              intIdx = %LookUp(*Blanks:aryLdSt);
              aryLdSt(intIdx) = RSjrst;
           EndIf;

          // Validate state against group setup
           Select;
           When ENst <> *Blanks And RSjrst <> ENst;
              Iter;
           //*** Beg Chg ***** 01/27/23 *****************************
           //***When ENst = *Blanks And flgApprOrExcl <> *Blank;
           //***   Chain (MUco#:MUform:RSjrst) pes01;
           //***   Select;
           //***   When %Found And flgApprOrExcl = 'A';
           //***   When Not(%Found) And flgApprOrExcl = 'E';
           //***   Other;
           //***      Iter;
           //***   EndSl;
           When ENst = *Blanks;
              Exsr ChkApprExclSt;
              If isApprExclSt = *Off;
                 Iter;
              Endif;
           //*** End Chg ***** 01/27/23 *****************************
           EndSl;

          // Validate large deductible is set up on policy for state
           If RtvDedType() = 'L';
              RtvLgDedInfo(FMam01:FMam02:xxPctAg);
              FMam03 = FMam01;
              FMform = xxForm;
              FMA521 = %DECH(xxPctAg:5:2);
              Exsr WriteWorkFile;
           Endif;

          // Leave after one form prints if cltID and form are in
          //  cltIdForms CTDATA
           If %LookUp(cltID + %Subst(qqForm:1:8):cltIdForms) <> 0;
              Leave;
           Endif;

        EndDo;

       EndSR;
      //=======================================================================
      // PgmStr - Beginning of program processing
      //=======================================================================
       BegSR PgmStr;
        RunningSRText   = *Off;
        WriteInCopybook = *Off;
       EndSR;
      //=======================================================================
      // PgmEnd - End of program processing

      //=======================================================================
       BegSR PgmEnd;
       EndSR;
      //=======================================================================
      // Valid - Subroutine to attach this form to the policy
      //=======================================================================
       Begsr Valid;
        If RtvDedType() = 'L';
           RtvLgDedInfo(xxAmPO:xxAmAg:xxPctAg);
           If xxAmpo <> 0 ;
              crtFrm = *on;
           Endif;
        Endif;
       Endsr;
      /End-Free
      *=========================================================================
      * Query whether policy is small or large deductible
      *=========================================================================
     p RtvDedType      b
     d RtvDedType      pi             1
     d billCode        s                   like(BSkey)
      /Free
       billCode = %EditC(ELbsCd:'X');
       Chain (ELco#:billCode) pbs00;
       If %Found(wtabl);
          Select;
          When BSlpgm = 'L';
             Return 'L';
          Other;
             Return 'S';
          Endsl;
       Else;
          Return ' ';
       Endif;
      /End-Free
     p RtvDedType      e
      *=========================================================================
      * RtvLgDedInfo - retrieves LDAMPO and LDAMAG
      *=========================================================================
     p RtvLgDedInfo    b
     d RtvLgDedInfo    pi
     d  ampo                               like(FMAM01)
     d  amag                               like(FMAM02)
     d  pctag                              like(LDPCT1)
      /Free
       Chain (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pld01;
       If %Found;
          Select;
          When ELpfdt >= LDefd1 and (Elpfdt < LDexp1 or LDexp1 = *zeros);
             ampo  = LDamp1;
             amag  = LDamg1;
             pctag = LDadp1;
          When ELpfdt >= LDefd2 and (Elpfdt < LDexp2 or LDexp2 = *zeros);
             ampo  = LDamp2;
             amag  = LDamg2;
             pctag = LDadp2;
          Other;
             ampo  = *zeros;
             amag  = *zeros;
             pctag = *zeros;
          Endsl;
       Else;
          ampo  = *zeros;
          amag  = *zeros;
          pctag = *zeros;
       Endif;
      /End-Free
     p RtvLgDedInfo    e
** cltIdForms
150WC120602
150WC290604
150WC320302
150WC990203
150WC990303
150WC990403
150WC990603
150WC990608
150WC990609
150WC990610
150WC990611
150WC990612
150WC990678
150WC992002
150WC992003
150WC992518
150WC992818
150WC992918
150WC993501
150WC994103
150WC994118
150WC994203
150WC994218
150WC994518
150WC995203
150WC995402
153WC991011
150WC995410
148WC990660
