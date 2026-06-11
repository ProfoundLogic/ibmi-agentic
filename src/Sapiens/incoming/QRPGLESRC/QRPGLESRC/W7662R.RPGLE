     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H/TITLE W7662R - CALCULATED VS. BILLED PREMIUM RPT BUILD WORKFILE
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      * 04/07/14 C:302 SUPPORT: 84602                                 *
      *                Re-write program and make it free-form         *
      *                correct an issue where the inner loops         *
      *                reading WMEAP file where reseting the          *
      *                data in the program.                           *
      *                                                               *
      *****************************************************************
     FWDELL3    if   e           k disk
     FWDELL1    if   e           k disk    Rename(pel01:pel02)
     FWMEAL8    if   e           k disk
     FWMXAL     if   e           k disk
     FWMEML     if   e           k disk
     FWTABL     if   e           k disk    Ignore(ppc01)
     FSMCOL     if   e           k disk
     FWMFNL     if   e           k disk
     FWXPCL     if a e           k disk
     FWMUPL     if   e           k disk
     FWMAHL     if   e           k disk
     FWMAAL1    if   e           k disk
     FWMAGL     if   e           k disk

     D psds          ESDS                  ExtName(szq1p)

     D                 DS
     D  dsdttm                 1     14  0
     D  dstime                 1      6  0
     D  dsdate                 7     14  0
     D                 DS
     D  cx                            7  0 Dim(5) Ascend
     D  EMcad1                 1      7  0
     D  EMcad2                 8     14  0
     D  EMcad3                15     21  0
     D  EMcad4                22     28  0
     D  EMcad5                29     35  0
     D                 DS
     D  ef                            7  0 Dim(5) Ascend
     D  EMefd1                 1      7  0
     D  EMefd2                 8     14  0
     D  EMefd3                15     21  0
     D  EMefd4                22     28  0
     D  EMefd5                29     35  0
     D                 DS
     D  arcd                         14
     D  ar                            2     Overlay(arcd) dim(7) Ascend

     D  xxCo#          S                   Like(ELco#)
     D  xxFnd          S                   Like(ELfnd)
     D  xxFyr          S                   Like(ELfyr)
     D  xxEmp#         S                   Like(ELemp#)
     D  xxDiv          S                   Like(ELdiv)
     D  xxAnpr         S                   Like(ELaapr)
     D  xxPrem         S                   Like(ELaapr)
     D  xxTdt          S              8  0
     D  xxFdt          S              8  0
     D  xxTodt         S              7  0
     D  xxFrdt         S              7  0
     D  srDto          S             10
     D  xxSrac         S              1
     D  srErr          S              1
     D  YES            C                   Const('Y')
     D  NO             C                   Const('N')

     D W7662R          PR
     D                                3
     D                                3
     D                                8
     D                                8
     D                                1
     D                               14

     D W7662R          PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqFrdt                        8
     D  qqTodt                        8
     D  qqSrac                        1
     D  qqArcd                       14

     D*--------------------------------------------------
     D* Procedure name: skipRecord
     D* Purpose:        Check to see if we need to skip this record
     D* Returns:
     D*--------------------------------------------------
     D SkipRecord      PR              N

     D*--------------------------------------------------
     D* Procedure name: CalculatePremium
     D* Purpose:        Calculate premium amount
     D* Returns:
     D*--------------------------------------------------
     D CalculatePremium...
     D                 PR

     D*--------------------------------------------------
     D* Procedure name: CalculateARAmount
     D* Purpose:        Calculate the A/R Amount
     D* Returns:
     D*--------------------------------------------------
     D CalculateARAmount...
     D                 PR

     D*--------------------------------------------------
     D* Procedure name: WriteRecord
     D* Purpose:        Write record to work file
     D* Returns:
     D*--------------------------------------------------
     D WriteRecord     PR

      /COPY sPrDate

      /Free
       dsTime = %Dec(%Time():*ISO);
       dsDate = %Dec(%Date():*iso);
       xxCo# = %Dec(qqCo#:3:0);
       xxFnd = %Dec(qqFnd:3:0);
       xxFdt = %Dec(qqFrDt:8:0);
       xxTdt = %Dec(qqToDt:8:0);
       xxSrac = qqSrac;
       arCd =  qqArCd;

       Chain (q1user) WMUPL;

       xxfrdt = DateTo7(CvtToDate(xxfdt));
       xxtodt = DateTo7(CvtToDate(xxtdt));

       Chain (xxCo#) pco01;
       Chain (xxCo#:xxFnd) pfn01;


        //  Start main loop

       Setll (xxCo#:xxFnd:xxfrdt) pel01;
       Reade (xxCo#:xxFnd) pel01;
       Dow Not %eof And ELpfdt <= xxToDt;

          xxFyr = ELfyr;
          xxEmp# = ELemp#;
          If Not SkipRecord();
             CalculatePremium();
             CalculateARAmount();
          Endif;

          BSsefl = *Blanks;
          Chain (xxCo#:%Char(ELbscd)) pbs01;

          //  do not include employers that are s/r and not yet audited
          //  If indicated by prompt selection
          If (xxAnpr <> xxPrem) and
           not(xxSrac = YES and BSsefl = 'S' and ELfaf <> 'A');
             WriteRecord();
          Endif;


          Reade (xxCo#:xxFnd) pel01;
       Enddo;

       *inlr = *On;

      /End-Free
     P*--------------------------------------------------
     P* Procedure name: skipRecord
     P* Purpose:        Check to see if we need to skip this record
     P* Returns:
     P*--------------------------------------------------
     P SkipRecord      B
     D SkipRecord      PI              N
     D intDiv          S              5  0 Inz(*hival)
      /Free

          //  exclude any accounts that are in a "not billed" status
          If ELfaf = *Blanks;
             Return *On;
          Endif;

          //  If sumarized check to see if already sumarized and processed
          If ELarsm = YES;
             Chain (xxCo#:xxFnd:ELemp#:intDiv:ELfyr) ppc01;

             If %Found;
                Return *On;
             Endif;

          Endif;

        Return *Off;

      /End-Free
     P SkipRecord      E


     P*--------------------------------------------------
     P* Procedure name: CalculatePremium
     P* Purpose:        Calculate premium amount
     P* Returns:
     P*--------------------------------------------------
     P CalculatePremium...
     P                 B
     D CalculatePremium...
     D                 PI
      /Free

       xxAnpr = 0;

       //  calculate premium for employer with summarized a/r
       If ELarsm = YES;
          Setll (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;

          If %Equal;
             Reade (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
             Dow not %eof(wdell1);

                If elarsm = YES;

                   If ELfaf = 'A';
                      xxAnpr = xxAnpr + ELaapr + ELstam;
                   Else;
                      xxAnPr = xxAnPr + ELanPr;
                   Endif;

                Endif;

                Reade (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
             Enddo;

          Endif;

       Else;

          //  calculate premium for employer without summarized a/r
          If ELfaf = 'A';
             xxAnpr = xxAnpr + ELaapr + ELstam;
          Else;
             xxAnpr = xxAnpr + ELanpr;
          Endif;

       Endif;

      /End-Free
     P CalculatePremium...
     P                 E

     P*--------------------------------------------------
     P* Procedure name: CalculateARAmount
     P* Purpose:        Calculate the A/R Amount
     P* Returns:
     P*--------------------------------------------------
     P CalculateARAmount...
     P                 B
     D CalculateARAmount...
     D                 PI
      /Free

          xxPrem = *Zeros;

          //  calculate a/r  for employer with summarized a/r

          If ELarsm = YES;

             Setll (xxCo#:xxFnd:xxFyr:xxEmp#) pea01;

             If %Equal;
                Reade (xxCo#:xxFnd:xxFyr:xxEmp#) pea01;
                Dow Not %Eof(WMEAL8);
                   xxEmp# = EAemp#;
                   xxDiv = EAdiv;
                   xxFyr = EAfyr;
                   Chain (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv) pel02;
                   If %Found And ELarsm = YES;

                      If  %Lookup(EAarcd:ar:1) = 0;

                         If EAarty = 'B' or EAarty = 'A' or  EAarty = 'D';
                            xxPrem +=  EAamt;
                         Endif;

                      Endif;

                   Endif;
                   Reade (xxCo#:xxFnd:xxFyr:xxEmp#) pea01;
                Enddo;

             Endif;

             Setll (xxCo#:xxFnd:xxFyr:ELemp#) pxa01;

             If %Equal;
                Reade (xxCo#:xxFnd:xxFyr:ELemp#) pxa01;
                Dow Not %eof;
                   Chain (xxCo#:xxFnd:xxFyr:ELemp#:XAdiv) pel02;
                   If %Found And ELarsm = YES And ELfaf <> 'A';

                      If %Lookup(XAarcd:ar:1) = 0;

                         If XAarty = 'B' or XAarty = 'A' or XAarty = 'D';
                            xxPrem += XAamt;
                         Endif;

                      Endif;
                   Endif;
                   Reade (xxCo#:xxFnd:xxFyr:ELemp#) pxa01;
                Enddo;
             Endif;

          Else;

             //  calculate for employer without summarized a/r
             Setll (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv) pea01;

             If %equal;
                Reade (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv) pea01;
                Dow not %eof(wmeal8);
                   If  %Lookup(EAarcd:ar:1) = 0;

                      If (EAarty = 'B' or EAarty = 'A') or EAarty = 'D';
                         xxPrem += EAamt;
                      Endif;

                   Endif;
                   Reade (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv) pea01;
                Enddo;
             Endif;

             Setll (xxCo#:xxFnd:xxFyr:elemp#:xxDiv) pxa01;
             If %equal;
                Reade (xxCo#:xxFnd:xxFyr:elemp#:xxDiv) pxa01;
                Dow not %eof;
                   If elfaf <> 'A';

                      If %Lookup(XAarcd:ar:1) = 0;

                         If XAarty = 'B' or XAarty = 'A' or XAarty = 'D';
                            xxPrem += XAamt;
                         Endif;

                      Endif;

                   Endif;
                   Reade (xxCo#:xxFnd:xxFyr:elemp#:xxDiv) pxa01 ;
                Enddo;
             Endif;
          Endif;

      /End-Free
     P CalculateARAmount...
     P                 E


     P*--------------------------------------------------
     P* Procedure name: WriteRecord
     P* Purpose:        Write record to work file
     P* Returns:
     P*--------------------------------------------------
     P WriteRecord     B
     D WriteRecord     PI
     D intIndex        S              3I 0
      /Free
          Setll (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr) ppc01;
          If Not %Equal;

             Chain (xxCo#:xxFnd:xxEmp#:xxDiv) pem01;

             intIndex = %Lookup(*Zeros:ef);
             intIndex = intIndex - 1;
             If intIndex <= 0;
                intIndex = 5;
             Endif;

             Chain (xxCo#:%Char(ELbscd)) pbs01;
             Chain (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr) pah01;
             Chain (xxCo#:xxFnd:AHagt#) paa01;
             Chain (xxCo#:AAfein:AAmod) pag01;
             Clear ppc01;
             PCco# = xxCo#;
             PCfnd = xxFnd;
             PCemp# = ememp#;
             PCdiv = EMdiv;
             PCfyr = ELfyr;
             PCenam = EMname;
             PCagt# = AHagt#;
             PCagnm = AGname;
             PCpfdt = ELpfdt;
             PCptdt = ELptdt;
             PCcxdt = cx(intIndex);
             PCcprm = xxAnpr;
             PCbprm = xxPrem;
             PCdiff = xxAnpr - xxPrem;
             PCauds = ELfaf;
             PCbscd = ELbscd;
             PCbcds = BSdesc;
             PCbsfl = BSsefl;
             If ELarsm = YES;
                PCdiv = *hival;
                xxDiv = *hival;
             Endif;
             Write ppc01;

          Endif;

      /End-Free
     P WriteRecord     E

