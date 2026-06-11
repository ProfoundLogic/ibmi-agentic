     H Copyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H bnddir('WTBNDDIR')
     H dftactgrp(*no) actgrp(*caller)
     *========================================================================
     * REVISIONS:
      *
      * 01/07/25 C:079 Support: 150/118204
      *                Fix data for V25R0
      *
     *========================================================================
     fwmwvl     uf   e           k disk    rename(pwv01:pwv00)
     fwdell1    if   e           k disk    rename(pel01:pel01)
     fwtesl     if   e           k disk    rename(pes01:pes00)
     fwmrsl     if   e           k disk    rename(prs01:prs00)

      /copy sPrSql
      /copy wPrEmplr

       Dcl-S maxElements Packed(5) Inz(20);
       Dcl-S svCo#  Like(WVco#);
       Dcl-S svFnd  Like(WVFnd);
       Dcl-S #OfRtvRows Like(maxElements);

       Dcl-Ds arrayWvrForms Qualified Dim(20);
        chrWvrState Char(2);
        chrWvrForm Char(15);
        intWvrEditDate Packed(7:0);
        intWvrReplDate Packed(7:0);
        intWvrAdopDate Packed(7:0);
       End-Ds;

       Exec Sql Set Option Commit = *NONE;

       Dou %Eof;
          Read pwv00;
          If %Eof;
             Leave;
          EndIf;
          Chain (WVco#:WVfnd:WVfyr:WVemp#:WVdiv) pel01;
          If Not(%Found);
             Iter;
          Endif;
          Chain (ELco#:ELrtcd) prs00;
          If Not(%Found);
             Iter;
          Endif;
          If WVenm <> *Blanks And WVemst <> *Blanks;
             Iter;
          EndIf;
          If svCo#  <> WVco#
           Or svFnd <> WVfnd;
             svCo#  = WVco#;
             svFnd  = WVfnd;
             RtvGroupWaiverForms();
          Endif;
          If WVenm = *Blanks;
             WVenm = SelectWaiverForm(RSjrst);
          Endif;
          If WVemst = *Blanks;
             WVemst = RSjrst;
          Endif;
          WVuser = 'FIX80310';
          Update pwv00;
       Enddo;

       *Inlr = *On;
       Return;
       //=============================================
       Dcl-Proc SelectWaiverForm;
       //=============================================
       Dcl-Pi *N Char(15);
        inState Char(2);
       End-Pi;

       Dcl-S ix Packed(2);

       For ix = 1 to #OfRtvRows;
          If ELpfdt < arrayWvrForms(ix).intWvrEditDate;
             Iter;
          EndIf;
          If arrayWvrForms(ix).intWvrReplDate <> 0
           And arrayWvrForms(ix).intWvrReplDate <= ELpfdt;
             Iter;
          EndIf;
          If arrayWvrForms(ix).intWvrAdopDate > ELpfdt;
             Iter;
          EndIf;
         // Check for state specific forms
          Select;
          When arrayWvrForms(ix).chrWvrState = *Blanks;
          When arrayWvrForms(ix).chrWvrState = inState;
             Return arrayWvrForms(ix).chrWvrForm;
          Other;
             Iter;
          EndSl;
         // Check for excluded states if waiver is not state specific
          Chain (WVco#:arrayWvrForms(ix).chrWvrForm:inState) pes00;
          Select;
          When Not(%Found);
          When ESaDte > ELpfdt;
             Iter;
          When ESexdt <> 0 And ESexdt < ELpfdt;
             Iter;
          When ESapex = 'E';
             Iter;
          EndSl;
          Return arrayWvrForms(ix).chrWvrForm;
       EndFor;

       Return *Blanks;
       End-Proc;
       //=============================================
       Dcl-Proc RtvGroupWaiverForms;
       //=============================================
       // Populate waiver form info into arrayWvrForms
       Exec Sql
        Declare EN1 Cursor For
        Select ENst, ENform, ENeddt, ENrpdt, ENadte
         From WTENP
         Where ENco# = :WVco#
          And ENfnd = :WVfnd
          And ENtype = 'Waiver'
         Order By ENst Desc
         For Read Only;

       Exec Sql Open EN1;
       Exec Sql
        Fetch Next
        From EN1
        For :maxElements Rows
        Into :arrayWvrForms;
       If IsValidFetch(sqlStt);
          #OfRtvRows = SQLER3;
       Endif;
       Exec Sql Close EN1;

       End-Proc;
