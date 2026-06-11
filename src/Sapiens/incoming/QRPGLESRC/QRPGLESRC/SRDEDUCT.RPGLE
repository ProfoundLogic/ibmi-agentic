       CTL-OPT COPYRIGHT('(c) 2022 Sapiens. All Rights Reserved.');
       CTL-OPT dftactgrp(*no) ACTGRP(*caller) BNDDIR('WTBNDDIR');
       //******************************************************************
       //                                                                 *
       //  SRDEDUCT -  Generic Deductible Endorsement.                    *
       //                                                                 *
       //  Description:                                                   *
       //                                                                 *
       //  This is a generic deductible endorsement program.              *
       //                                                                 *
       //                                                                 *
       //  Created: 11/16/20  C:180  Project:  WC2775                     *
       //                                                                 *
       //******************************************************************
       //  Revisions:                                                     *
       //                                                                 *
       // 11/03/22 C:333 Project: WC2834                                  *
       //                Endorsement Recognition of Cancellation          *
       //                                                                 *
       // 01/29/21 C:180 Support: 93990 (LUBA)                            *
       //                Monitor error when running SendToScriptura       *
       //                Changes were made in Copy Book SCOPYEN01C        *
       //                                                                 *
       //                                                                 *
       //******************************************************************
       Dcl-Pi *N;
        qqCo#  char(3);
        qqFnd  char(3);
        qqEmp# char(9);
        qqDiv  char(5);
        qqFyr  char(3);
        qqMode char(1);
        qqForm char(15);
       End-Pi;

       Dcl-F wtabl disk usage(*input) keyed include(pbs01) rename(pbs01:pbs00);
       Dcl-F wmldl1 disk usage(*input) keyed;
       Dcl-F wtddl2 disk usage(*input) keyed;

      /Free
       //F and D copy books
      /Copy sCopyEn01f
      /Copy sCopyEn01d
      /Copy wPrPeo
      //***** Beg Del *** 11/03/22 **********************************
      ***/Copy wPrEmplr
      //***** End Del *** 11/03/22 **********************************
      /Copy sPrString
      /Copy wWcPolDat4
      /End-Free

       //Stand Alone
       Dcl-S intAmPo  like(FMam01);
       Dcl-S intAmAg  like(FMam01);
       Dcl-S qqRrn packed(9:0);
       Dcl-S qqMod2 char(1);

       //Constants
       Dcl-c addpgm const('SRXXXXXX  ');

       //External Prototypes
       Dcl-Pr SREFFD  EXTPGM;
        strCo#   char(3);
        strFnd   char(3);
        strFyr   char(3);
        strEmp#  char(9);
        strDiv   char(5);
        strDate  char(7);
       End-Pr;

      /Free
       //C copy books
      /Copy sCopyEn01c
      /End-Free

       //*-------------------------------------------------------------
       //* Valid - Determine whether to attach this form to the policy
       //*-------------------------------------------------------------
       Begsr Valid;
        If RtvDedType() = 'S';
           Clear intAmPo;
           Clear intAmAg;
           RtvSmDedInfo (intAmPo:intAmAg);
           If intAmPo > 0;
              crtFrm = *On;
           Endif;
        Endif;
       Endsr;

       //*-------------------------------------------------------------
       //* OthInf - Load form specific information
       //*-------------------------------------------------------------
       Begsr OthInf;
        Setll (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
        Dou %eof(WDELL1);
           Reade (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
           If %Eof(WDELL1);
              Leave;
           Endif;
           //***** Beg Add *** 11/03/22 ******************************
           xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
           If xxCxDate <= ELpfdt and xxCxDate <> *zeros;
              Iter;
           Endif;
           //***** End Add *** 11/03/22 ******************************

           Chain (xxCo#:ELrtcd) prs01;
           If ENst <> RSjrst
            And ENst <> *Blanks;
              Iter;
           Endif;

           Clear intAmPo;
           Clear intAmAg;
           RtvSmDedInfo (intAmPo:intAmAg);
           If intAmPo > 0;
              FMam01 = intAmPo;
              Exsr WriteWorkFile;
              Leave;
           Endif;
        Enddo;
       Endsr;
       //--------------------------------------------------------------
       // PgmStr - Beginning of program processing
       //--------------------------------------------------------------
       Begsr PgmStr;
        RunningSRText   = *Off;
        WriteInCopybook = *Off;
       Endsr;

       //--------------------------------------------------------------
       // PgmEnd - End of program processing
       //--------------------------------------------------------------
       Begsr PgmEnd;
        Chain inXML XMLTEML1;
        If %found(XMLTEML1);
           XTtype = 'Deductible Endorsement';
           Update XT100;
        Endif;
       Endsr;

       //*-------------------------------------------------------------
       //* RtvDedType - Retrieves Deductible Type (Small or Large)
       //*-------------------------------------------------------------
       Dcl-Proc RtvDedType;
        Dcl-Pi *N char(1);
        End-Pi;

        Dcl-S strBillCode like(BSkey);

        strBillCode = %Editc(ELbsCd:'X');
        Chain (ELco#:strbillCode) pbs00;

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

       End-Proc;
       //*-------------------------------------------------------------
       //* RtvSmDedInfo - Retrieves DDAMPO and DDAMAG
       //*-------------------------------------------------------------
       Dcl-Proc RtvSmDedInfo;
        Dcl-Pi *N;
         @@AmPo  packed(11:2);
         @@AmAg  packed(11:2);
        End-Pi;

        Dcl-S kkEfDt packed(7:0);
        Dcl-S inCo#   char(3);
        Dcl-S inFnd   char(3);
        Dcl-S inFyr   char(3);
        Dcl-S inEmp#  char(9);
        Dcl-S inDiv   char(5);
        Dcl-s outDate  char(7);

        inCo#  = %Editc(ELco#:'X');
        inFnd  = %Editc(ELfnd:'X');
        inFyr  = %Editc(ELfyr:'X');
        inEmp# = %Editc(ELemp#:'X');
        inDiv  = %Editc(ELdiv:'X');
        outDate = %Editc(ELdcEf:'X');

        SREFFD(inCo#:inFnd:inFyr:inEmp#:inDiv:outDate);
        kkEfDt = %int(outDate);

        Setgt (ELco#:ELfnd:RSjrst:ELdcDe:kkEfDt) pdd01;
        ReadPe (ELco#:ELfnd:RSjrst:ELdcDe) pdd01;
        Dow not %eof(WTDDL2) and DDstat <> 'A';
           ReadPe (ELco#:ELfnd:RSjrst:ELdcDe) pdd01;
        Enddo;

        If not %eof(WTDDL2);
           @@AmPo = DDampo;
           @@AmAg = DDamag;
        Endif;

       End-Proc;
