      *========================================================================
     h Copyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h/define   PROFOUNDUI
     h bnddir('WTBNDDIR')
      *========================================================================
     h/Title SCFQDARTS - Printable Quote
      *========================================================================
      * Description:
      *
      *
      *
      *  Called From:
      *
      *  Calls To:
      *  PrintBills override:W8002C
      *
      *=====================================================================
      * Created on 08/27/2013 by c:190 for Project 93QKN
      *========================================================================
      * Revisions:
      *                                                                   *
      *  04/01/14 C:282 Support FQ099 85204                               *
      *                   Added the handling for data center client.      *
      *                                                                   *
      *  01/08/14 C:283 Support FQ093 84510                               *
      *                 Correct the Parameters for call to SCFQSBM.       *
      *
      *========================================================================
     fwdell     if   e           k disk    rename(pel01:pel00)
     fwdelp     if   e             disk    rename(pel01:pel0p)
     fwmexl     if   e           k disk    rename(pex01:pex00)
     fwmupl     if   e           k disk    rename(pup01:pup00)
     fwtabl     if   e           k disk

      *=========================================================================
      * Prototypes
      *=========================================================================

     dwa002r           pr                  extpgm('WA002R')
     d ##co                                like(ELco#)  const
     d ##fnd#                              like(ELfnd)  const
     d ##emp                               like(ELemp#) const
     d ##div#                              like(ELdiv)  const
     d ##fyr                               like(ELfyr)  const
     d ##agt#                         5  0              const
     d ##agt_name                    60                 const
     d ##prod                         5  0              const
     d ##prod_nm                     60                 const
     d ##area_cd                      3  0              const
     d ##phone#                       7  0              const
     d ##plan                              like(##plan)
     d ##asOfDate                          like(##asOfDate) const

      * Getclient ID
     dGetClientID      pr                  extpgm('RTVCLTID')
     d outCltID                       3

      * Check Object
     dGetObject        pr                  extpgm('SRCHKOBJ')
     d outName                       10
     d outType                        7
     d outRtcd                        1
     d outName         s             10
     d outType         s              7
     d outRtcd         s              1

     dqcmdExc          pr                  extpgm('QCMDEXC')
     d command                             const like(strCommand)
     d length                        15  5 const

     dscfqsbm          pr                  extpgm('SCFQSBM')
     d xxCo#                          3
     d xxFnd                          3
     d xxFyr                          3
     d xxEmp#                         9
     d xxDiv                          5
     d qqDate                         8
     d xxEsCd                         2
     d xxCom1                         2
     d xxCom2                         2
     d xxCom3                         2
     d qqDue                          8
     d xxDep                          1
     d xxStyl                         1
     d qqCall                         1
     d qqOvr                          1
     d xxSrCd                         2
     d xxAjCd                         2
     d qqQte                          1
     d qqOutQ                        10
     d xxRnb                          1
     d xxDart                         1
     d xxHard                         1
     d xxAgtO                         1
     d xxFqt                          1
     d xxRmCd                         5
     d qqAr                           2
     d qqEfDt                         8
     d qqFcm1                        30
     d qqFcm2                        30
     d qqFcm3                        30
     **** begin chg ***** 01/08/14 ************************************
     d***************qqAr2                          2
     d qqAr2                          2
     **** end   chg ***** 01/08/14 ************************************
     d qqFQKey                       25
     d qqdrtcount                     5

     d createDtaQ      pr                  extpgm('SCCRTDTQKD')
     d pDataq                        10
     d pDataqLib                     10
     d pMaxLength                     5  0
     d pText                         20
     d pRtnCode                       1
      *=========================================================================
      * Variable Definitions
      *=========================================================================

     d pDataQ          s             10a   inz('BILFQDTAQ')
     **** begin chg ***** 04/01/14 ************************************
     d***** pDataQLib       s             10a   inz('PDSOBJ')
     d pDataQLib       s             10a
     **** end   chg ***** 04/01/14 ************************************
     d pMaxLength      s              5  0 inz(512)
     d pRtnCode        s              1a
     d pEntLength      s              5  0 inz(512)
     d pText           s             20a   inz('DATAQ FOR FASTQUOTE')
     d pPath           s            256a

     d @@Rrn           s              9  0
     d @@Key           s             25
     d @@Co#           s              3
     d @@Fnd           s              3
     d @@EMP#          s              9
     d @@Div           s              5
     d @@Fyr           s              3
     d @@drtCount      s              5
     d command         s            255    inz(*blanks)
     d ClientID        s              3
     d hPath           s            256a
     d length          s             15  5
     d line3           s             78a
     d qqDiv           s              5  0
     d qqDate          s              8
     d qqEmp#          s              9  0
     d qqFnd           s              3  0
     d qqFyr           s              3  0
     d qqQte           s              1
     d qqCall          s              1
     d qqCo#           s              3  0
     d qqEnv           s              1
     d qqyrFlag        s              1
     d qqFcm1          s             30
     d qqFcm2          s             30
     d qqFcm3          s             30
     d qqDue           s              8
     d qqAr            s              2
     **** begin chg ***** 01/08/14 ************************************
     d*************qqAr2           s              2
     d qqAr2           s              2
     **** end   chg ***** 01/08/14 ************************************
     d qqFQKey         s             25
     d qqdrtcount      s              5
     d qqEfDt          s              8
     d qqOvr           s              1
     d qqOutQ          s             10
     d tempDate        s              8  0
     d tempDt          s              8  0
     d tempDiv         s              5  0
     d tempDue         s              2  0
     d tempCo#         s              3  0
     d tempFnd         s              3  0
     d tempEmp#        s              9  0
     d strCommand      s            255
     d date8           s              8  0
     d xxRmcd          s              5
     d xxFqt           s              1
     d xxAgtO          s              1
     d xxHard          s              1
     d xxDart          s              1
     d xxRnb           s              1
     d xxAjcd          s              2
     d xxSrcd          s              2
     d xxDep           s              1
     d xxCom1          s              2
     d xxCom2          s              2
     d xxCom3          s              2
     d xxStyl          s              1
     d xxDue           s              8  0
     d xxToDte         s              8  0
     d xxAcct          s              1
     d xxDiv           s              5
     d xxDays          s              5  0
     d xxDate          s              8  0
     d xxEmp#          s              9
     d xxFnd           s              3
     d xxFyr           s              3
     d xxDt            s              7  0
     d xxDt2           s              7  0
     d xxCo#           s              3
     d xxEsCd          s              2
     d uncnPath        S            255a

     d ##co#           s              3
     d ##fnd           s              3
     d ##emp#          s              9
     d ##div           s              5
     d ##odr           s              1
     d ##nodr          s              1
     d ##aodr          s              1
     d ##plan          s              1
     d ##adrl          s              3  0
     d ##naml          s              3  0
     d ##fedl          s              3  0
     d ##peo1          s             57
     d ##peo2          s             57
     d ##pgfl          s              1
     d ##conl          s              3  0
     d ##asOfDate      s              8  0

     d title           c                   const('Print All Remarks Process')

     d fids01        e ds                  extName(szq2p)
     d psds          esds                  extName(szq1p)

     d/copy sPrDate
     d/copy wPrEmplr
    
      *=========================================================================
      * KList Definitions
      *=========================================================================

     c     keyWMEXL      KList
     c                   KFld                    qqCo#
     c                   KFld                    qqFnd

     c     keyWDELL      KList
     c                   KFld                    qqCo#
     c                   KFld                    qqFnd
     c                   KFld                    qqEmp#
     c                   KFld                    qqDiv
     c                   KFld                    qqFyr

     c     keyWTABL      KList
     c                   KFld                    qqCo#
     c                   KFld                    xxKey            10
     c
      *  User parameter file
     c     keyWMUPL      KList
     c                   KFld                    q1User
     c
      *=========================================================================
      * PLIST Definitions
      *=========================================================================
     c     *entry        plist
     c                   parm                    @@Key
     c                   parm                    @@Co#
     c                   parm                    @@Fnd
     c                   parm                    @@Emp#
     c                   parm                    @@Div
     c                   parm                    @@Fyr
     c                   parm                    @@drtCount

      *=========================================================================
      * Main Processing
      *=========================================================================
     **** beg add ***** 04/01/14 ************************************
     c                   Call      'SA030R'
     c                   Parm      *BLANKS       QQSUFX            3
     c                   Parm      *BLANKS       QQFLRP           80
     **** end add ***** 04/01/14 ************************************
      /free
        Exsr CrtDtQ;
        Exsr BldWrkFld;
        Exsr CalPrtBlsOvr;
        @@drtCount = qqdrtcount;
        return;

       //=============================================================
       // Building work fields and variables
       //=============================================================
        begSr BldWrkFld;

        qqCo#  = %int(@@Co#);
        xxCo#  = @@Co#;
        xxFnd  = @@Fnd;
        xxFyr  = @@Fyr;
        xxEmp# = @@Emp#;
        xxDiv  = @@Div;
        qqFnd  = %int(@@Fnd);
        qqEmp# = %int(@@Emp#);
        qqDiv  = %int(@@Div);
        qqFyr  = %int(@@Fyr);
        qqCall = '2';
        qqQte  = 'Q';

        // Default posting date to billing start date if called from
        // work with, else dafautl to current system date.

        GetClientID(ClientID);
        xxDate = DateTo8(SystemDate);
        xxAcct = *blanks;

        ##plan = '*';
        qqENV  = 'Q';
        qqyrFlag = ' ';
        @@Rrn = GetEmpYrRR(qqFyr:qqEmp#:qqDiv:qqFnd:qqCo#:qqyrFlag:qqEnv);
        Chain @@Rrn pel0p;
        If %found;
           ##asOfDate = dtMDYY(ELptdt);
        Endif;

        tempCo#  = %int(@@Co#);
        tempFnd  = %int(@@Fnd);
        tempEmp# = %int(@@Emp#);
        tempDiv  = %int(@@Div);
        WA002R(tempCo#:tempFnd:tempEmp#:tempDiv:qqFyr:0:' ':0:' ':0:0:
         ##plan:##asOfDate);

        If ##Plan = 'A';
           xxAcct = 'Y';
        Endif;
        Chain keyWMEXL pex00;
        If %found;
           If xxACCT = 'Y';
              xxDAYS = EXaoid;
           Else;
              xxDAYS = EXrvid;
           Endif;
           xxKey = %EditC(ELbscd:'X');
           Chain keyWTABL pbs01;
           If BSSEFL = 'S';
              tempDue = EXrvid;
           Endif;

           //Posting date (revised interim days plus the present date)

           xxToDte = dtFuture(dateTo8(SystemDate()):xxDays);
           If xxToDte <> 0;
              tempDt = xxDate;
           Endif;
        Endif;

        If ClientID = '132';
           date8 = dtMDYY(ELbsdt);
           xxToDte = dtFuture(date8:xxDays);
           If xxTodte <> 0;
              tempDt = date8;
           Endif;
        Else;
           tempDate = dtMDYY(ELbsdt);
        Endif;

        If tempDt <> *zero;
           xxDt = dtCYMD(tempDt);
        Else;
           tempDate = DateTo8(SystemDate);
        Endif;

        If ClientID = '132';
           xxDt2 = dtCYMD(tempDate);
           If xxDt > xxDt2;
              tempDate = tempDt;
           Endif;
        Else;
           If xxDt > ELbsdt;
              tempDate = tempDt;
           Endif;
        Endif;

        // move numeric value into a character string
        qqDate = %EditC(tempDate:'X');


        // change default for xxDEP to 'Y' and xxFqt to 'N'
        xxFqt  = 'N';
        xxDart = 'Y';
        xxHard = 'Y';
        xxAgtO = 'N';
        xxRnb  = ' ';
        xxDep  = 'Y';
        xxCOM1 = *blanks;
        xxCOM2 = *blanks;
        xxCOM3 = *blanks;

        // get employer detail for billing schedule
        xxStyl = *blanks;

        If tempEmp# <> *zero;
           Chain keyWDELL pel00;
           If %found;
              xxKey = %EditC(ELbscd:'X');
              Chain keyWTABL pbs01;
              If %found;
                 xxStyl = BSsefl;
              Endif;
           Endif;
        Endif;

        If (qqCall = '2' or qqCall = '3' or qqCall = '4') and qqQte = 'Q';
           Chain keyWMEXL pex00;
           If %found;
              xxEsCd = EXescd;
              xxSrCd = EXsrcd;
              xxAjCd = EXdacd;
              xxRmCd = EXorg;
           Else;
              xxEsCd = *blanks;
              xxSrCd = *blanks;
              xxAjCd = *blanks;
              xxRmCd = *blanks;
           Endif;
        Endif;

        Chain keyWMEXL pex00;
        If %found and qqQte = 'Q';
           xxRmCd = EXqte;
        Endif;

        If EX3rdb = 'Y';
           xxRmCd = *blanks;
           xxDart = 'N';
        Endif;

        If xxRmCd = *blanks;
           xxDart = 'N';
        Endif;

        If qqCall = '3';
           xxHard = 'N';
           xxAgtO = 'N';
        Endif;

        // move numeric value(2 0) into another longer numeric value(8 0)
        xxDue = tempDue;
        // to move numeric value into a character string
        qqDue = %EditC(xxDue:'X');
        qqOvr = *blanks;
        Chain keyWMUPL pup00;
        If %found;
           qqOutQ = upOutQ;
        Endif;

        qqAr   = *blanks;
       // qqAr2  = *blanks;
        qqFcm1 = *blanks;
        qqFcm2 = *blanks;
        qqFcm3 = *blanks;
        qqEfDt = '00000000';

        endSr;

       //=============================================================
       // Creating a Dataque if it does not exists
       //=============================================================
        begSr CrtDtQ;
           //  beg add ***** 04/01/14 ************************************
               pDataqLib = 'PDSOBJ' + QQSUFX;
           //  end add ***** 04/01/14 ************************************
           callP createDtaQ(pDataq:pDataqLib:pMaxLength:pText:pRtnCode);
        endSr;
       //=============================================================
       // Calling PrintBills override:w8002c
       //=============================================================

        begSr CalPrtBlsOvr;

           qqFQKey = @@key;
        //*** begin chg ***** 01/08/14 ************************************
m       //********scfqsbm(xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv:qqDate:xxEsCd:
        //********xxCom1:xxCom2:xxCom3:qqDue:xxDep:xxStyl:qqCall:
        //********qqOvr:xxSrCd:xxAjCd:qqQte:qqOutQ:xxRnb:xxDart:
        //********xxHard:xxAgtO:xxFqt:xxRmCd:qqAr:qqEfDt:qqFcm1:
        //********qqFcm2:qqFcm3:qqFQKey:qqdrtcount);
m          scfqsbm(xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv:qqDate:xxEsCd:
            xxCom1:xxCom2:xxCom3:qqDue:xxDep:xxStyl:qqCall:
            qqOvr:xxSrCd:xxAjCd:qqQte:qqOutQ:xxRnb:xxDart:
            xxHard:xxAgtO:xxFqt:xxRmCd:qqAr:qqEfDt:qqFcm1:
            qqFcm2:qqFcm3:qqAr2:qqFQKey:qqdrtcount);
        //*** end   chg ***** 01/08/14 ************************************

        endSr;

      /end-free
