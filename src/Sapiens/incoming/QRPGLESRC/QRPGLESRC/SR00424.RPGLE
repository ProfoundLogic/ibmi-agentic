     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller) BndDir('WTBNDDIR')
     H/TITLE SR00424 - NCCI Uncooperative Audit Endorsement
      //===============================================================*
      // WC000424 - NCCI                                               *
      //                                                               *
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      // Created   : 09/06/16                                          *
      // Programmer: C:298                                             *
      // Project   : WC2070                                            *
      //===============================================================*
      // REVISIONS:                                                    *
      //                                                             *
      // 11/03/23 C:079  Support: 114373                             *
      //                 - Use Approved/Excluded States logic in     *
      //                   Copybook                                  *
      //                                                             *
      // 10/27/22 C:333  Project: WC2834                             *
      //                 - Endorsement Recognition of Cancellation   *
      //                                                             *
      // 05/09/17 C:180  Project: 93WLH/WC2481                       *
      //                 - Populate WXPEP record.                    *
      //                 - Scrub the name and address fields for     *
      //                   punctuation                               *
      //
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
     fwdrrl     if   e           k disk
     fwxfmp3    o    e           k disk
     f*** Begin Add ***  05/09/17  *************************************
     FWXPEL     UF a E           K DISK
     FWXPEL1    IF   E           K DISK    RENAME(PPE01:PPE02)
     f*** End Add *****  05/09/17  *************************************

      //* Copybook F specs
     f/copy scopyen01f

      //* Copybook D specs
     d/copy scopyen01d
      //***** Beg Del *** 10/27/22 ******************************
     d***/copy wPrEmplr
      //***** End Del *** 10/27/22 ******************************
     f*** Begin Add ***  05/09/17  *************************************
     D/COPY SPRSTRING
     D/Copy wWCPOLDAT4
     D xxunid          s                   like(PEUNID)
     D x1end#          s                   like(PEEND#)
     d xxFirst         s              1a
     d Char4           s              4a
     d idx             s              5  0 inz
     f*** End Add *****  05/09/17  *************************************

     d qqForm          s             15a   inz('WC000424  ')
     d addState        s               n
     d intMult         s              5  4 inz
     d intANCstatC     s              5  0 inz
     d intStRptDiv     s              5  0 inz
     d intStRptEffDt   s              7  0 inz
     d isAprdOrExcd    s              1
     d j               s              2  0 inz
     d n               s              2  0 inz
     d x               s              2  0 inz
     d y               s              2  0 inz
     d z               s              2  0 inz
     d pos             s              2  0 inz
     d xxRateSt        s              2a   inz

     d excStArr        s              2    dim(60) inz
     d mltArr          s              5  4 dim(60) inz(*zero)
     d prmArr          s             11  2 dim(60) inz(*zero)
     d stArry          s              2    dim(60) inz

      // Print states on Schedule
     d                 ds
     d  stWfArry               1     20    dim(10)
     d  FMcd01                 1      2
     d  FMcd02                 3      4
     d  FMcd03                 5      6
     d  FMcd04                 7      8
     d  FMcd05                 9     10
     d  FMcd06                11     12
     d  FMcd07                13     14
     d  FMcd08                15     16
     d  FMcd09                17     18
     d  FMcd10                19     20

      // Print premium on Schedule
     d                 ds
     d  prmWfArr               1    110  2 dim(10)
     d  FMam01                 1     11  2
     d  FMam02                12     22  2
     d  FMam03                23     33  2
     d  FMam04                34     44  2
     d  FMam05                45     55  2
     d  FMam06                56     66  2
     d  FMam07                67     77  2
     d  FMam08                78     88  2
     d  FMam09                89     99  2
     d  FMam10               100    110  2

      // Print multiplier on Schedule
     d                 ds
     d  mltWfArr               1     50  4 dim(10)
     d  FMa541                 1      5  4
     d  FMa542                 6     10  4
     d  FMa543                11     15  4
     d  FMa544                16     20  4
     d  FMa545                21     25  4
     d  FMa546                26     30  4
     d  FMa547                31     35  4
     d  FMa548                36     40  4
     d  FMa549                41     45  4
     d  FMa54A                46     50  4

      //* Additional entry program
     d addPgm          c                   const('SRXXXXXXX ')

      *Entry
     d SR00424         pr
     d                                3
     d                                3
     d                                9
     d                                5
     d                                3
     d                                1
     d                                9  0
     d                                1

     d SR00424         pi
     d  qqCo#                         3
     d  qqFnd                         3
     d  qqEmp#                        9
     d  qqDiv                         5
     d  qqFyr                         3
     d  qqMode                        1
     d  qqRrn                         9  0
     d  qqMod2                        1

      //=======================================================
      //  Main
      //=======================================================

     f*** Begin Add ***  05/09/17  *************************************
     C     PEKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXDT              7 0

     C     PEKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR

     C     PEKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXUNID
     C                   KFLD                    X1END#
     f*** End Add *****  05/09/17  *************************************
      //* Copybook C specs (KLISTS and mainline)
     c/copy scopyen01c

      //=======================================================
      //  OTHINF - Subroutine to load form specific information
      //=======================================================
      /Free
       Begsr Othinf;

       // Begin add ***  05/09/17  ****************************************
       // Get latest UNID
          xxdt = *hival;
          Setgt pekey ppe02;
          Readpe(n) pekey1 ppe02;
          If not %eof;
              xxunid = PEunid;
          Endif;
          xxFirst = ' ';
          x1end# = %trim(qqform);
       // End   Add *** 05/09/17 ******************************************

          intStRptDiv = GetStRptDv(xxCO#:xxFND:xxEMP#);
          Chain (xxCo#:xxFnd:xxEmp#:intStRptDiv:xxFyr) pel01;
          intStRptEffDt = ELpfdt;
       // Get the excluded state list
          Exsr GetExcStates;
          Setll (xxCo#:xxfnd:xxfyr:xxemp#) wdell1;
          Reade (xxCo#:xxfnd:xxfyr:xxemp#) wdell1;
          Dow not(%eof);
          //***** Beg Add *** 10/27/22 ******************************
             xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
             If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
          //***** End Add *** 10/27/22 ******************************
                xxRateSt = GetRatingState(xxco#:xxfnd:xxemp#:ELdiv:Elfyr);
                intMult = *zeros;
                intANCstatC = *zeros;
                Setgt (ELco#:ELfnd:xxRateSt:intStRptEffdt) prr01;
                Readpe (ELco#:ELfnd:xxRateSt) prr01;
                If not %eof;
                   intMult = RRancm;
                   intANCstatC = RRancs;
                Endif;

       // Check if the endorsement is applicable to the state
                //*** Beg Chg *** 11/03/23 ************************************
                //***If isAprdOrExcd = 'A';
                //***   n = %lookup(xxRateSt:excStArr);
                //***   If n = 0 or intMult = 0 or intANCstatC = 0;
                //***      Reade (xxCo#:xxfnd:xxfyr:xxemp#) wdell1;
                //***      Iter;
                //***   Endif;
                //***Endif;
                //***If isAprdOrExcd = 'E';
                //***   n = %lookup(xxRateSt:excStArr);
                //***   If n <> 0 or intMult = 0 or intANCstatC = 0;
                //***      Reade (xxCo#:xxfnd:xxfyr:xxemp#) wdell1;
                //***      Iter;
                //***   Endif;
                //***Endif;
                Exsr ChkApprExclSt;
                If isApprExclSt = *off;
                   Reade (xxCo#:xxfnd:xxfyr:xxemp#) wdell1;
                   Iter;
                Endif;
                //*** End Chg *** 11/03/23 ************************************

       // Accumulate premium into premium array
                Exsr Search_St;
                If addState = *on;
                   prmArr(pos) = ELanpr;
                   stArry(pos) = xxRateSt;
                   mltArr(pos) = intMult;
                Else;
                   prmArr(pos) = prmArr(pos) + ELanpr;
                Endif;
           //***** Beg Add *** 10/27/22 ******************************
             Endif;
           //***** End Add *** 10/27/22 ******************************
             Reade (xxCo#:xxfnd:xxfyr:xxemp#) wdell1;
          Enddo;

       // Populate schedule fields
          y = 1;
          For x = 1 to 5;
             If stArry(y) <> *blank;
                Eval stWfArry = %subarr(stArry:y:10);
                Eval prmWfArr = %subarr(prmArr:y:10);
                Eval mltWfArr = %subarr(mltArr:y:10);
                Exsr WriteWorkFile;
                Write pfm03;
             Endif;
             y = (x * 10) + 1;
          Endfor;

       // Begin Add *** 05/09/17 ******************************
       // Copy data into the WCPOLS record.

       // Initialize WCPOLS data
          idx = *zeros;
          Clear e00424;
          x0ANCMult1  = *zeros;
          x0ANCMult2  = *zeros;
          x0ANCMult3  = *zeros;

          Chain(n) pekey2 ppe01;          //Chain for retrieval

          If %found;

             For z = 1 to 50;

                If stArry(z) <> *blanks;
                   idx = idx + 1;

                   Char4  = %Editc(MltArr(z):'X');

                   Select;
                   When idx = 1;
                      x0State1    = stArry(z);
                      x0ANCbasis1 = %Editc(prmarr(z):'J');
                      x0ANCMult1  = %int(Char4);
                   When idx = 2;
                      x0State2    = stArry(z);
                      x0ANCbasis2 = %Editc(prmarr(z):'J');
                      x0ANCMult2  = %int(Char4);
                   When idx = 3;
                      x0State3    = stArry(z);
                      x0ANCbasis3 = %Editc(prmarr(z):'J');
                      x0ANCMult3  = %int(Char4);
                      idx = *zeros;

                      If xxFirst = ' ';
                         Chain pekey2 ppe01;     // Chain for Update
                         PEData = e00424;
                         PEName = %trim(ExtrAlphaNum(PEName));
                         Update ppe01;
                         xxFirst = 'N';
                      Else;
                         PEData = e00424;
                         PEName = %trim(ExtrAlphaNum(PEName));
                         Write ppe01;
                      Endif;

                      Clear e00424;
                      x0ANCMult1  = *zeros;
                      x0ANCMult2  = *zeros;
                      x0ANCMult3  = *zeros;
                   Endsl;
                Endif;
             Endfor;
          Endif;

          If idx > *zeros;

             If xxFirst = ' ';
                Chain pekey2 ppe01;     // Chain for Update
                PEData = e00424;
                PEName = %trim(ExtrAlphaNum(PEName));
                Update ppe01;
                xxFirst = 'N';
             Else;
                PEData = e00424;
                PEName = %trim(ExtrAlphaNum(PEName));
                Write ppe01;
             Endif;

             Clear e00424;
             x0ANCMult1  = *zeros;
             x0ANCMult2  = *zeros;
             x0ANCMult3  = *zeros;
          Endif;
       // End   Add *** 05/09/17 ******************************

       Endsr;
       //======================================================
       //  PGMSTR - Beginning of program processing
       //======================================================
       Begsr Pgmstr;
          writeInCopyBook = *off;
       Endsr;
       //======================================================
       //  PGMEND - End of program processing
       //======================================================
       Begsr Pgmend;
       Endsr;
       //======================================================
       //  VALID - Subroutine to attach this form to the policy
       //======================================================
       Begsr valid;

          Setgt  (ELco#:ELfnd:RSjrst:ELpfdt) prr01;
          Readpe (ELco#:ELfnd:RSjrst) prr01;
          If Not(%Eof) and RRancs <> 0 and RRancm <> 0;
             //*** Beg Add *** 11/03/23 ******************************
             Exsr ChkApprExclSt;
             If isApprExclSt = *On;
             //*** End Add *** 11/03/23 ******************************
                crtFrm = *on;
             //*** Beg Add *** 11/03/23 ******************************
             Endif;
             //*** End Add *** 11/03/23 ******************************
          Endif;

       Endsr;
       //======================================================
       //  Search_St: Find element for state or add a new one
       //======================================================
       Begsr Search_St;

          pos = %lookup(xxRateSt:stArry);
          If pos = 0;
             addState = *on;
             pos = %lookup(*blank:stArry);
          Else;
             addState = *off;
          Endif;

       Endsr;
       //======================================================
       //  GetExcStates : Populate excluded state array
       //======================================================
       Begsr GetExcStates;

          isAprdOrExcd = *Blanks;
          j = 1;

        // Check if list is Approved or Excluded
          Chain (xxco#:xxform) pes01;
          If %Found(wtesl);
             isAprdOrExcd = ESapex;
          EndIf;

        // Populate excluded state array
          Setll (xxco#:xxform) pes01;
          Reade (xxco#:xxform) pes01;
          Dow Not(%Eof);
            excStArr(j) = ESst;
            j=j+1;
          Reade (xxco#:xxform) pes01;
          Enddo;

       Endsr;
      /End-Free
