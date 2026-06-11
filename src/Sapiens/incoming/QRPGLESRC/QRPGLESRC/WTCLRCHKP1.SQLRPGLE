     hCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H*Option(*NoDebugio:*SrcStmt)
     H DftActGrp(*No) ActGrp(*Caller)
     H/Title WTCLRCHKP - Cleared Check Import Process
     H Bnddir('WTBNDDIR')
     H*===============================================================
     H* Created on 06/11/24 by C:340 for Project 152WCB
     H*===============================================================
     H* Revisions
     H*
     H*===============================================================

    d*===================================================================
    d* Prototypes
    d*===================================================================
     d RtvCltId        pr                  ExtPgm('RTVCLTID')
     d  strCltId                      3

     d/copy sPrCompany
     d/copy sPrDate
     d/copy sPrGenPop
     d/copy sPrLetter
     d/copy sPrMessage
     d/copy sPrRunCmd
     d/copy sPrSQL
     d/copy sPrXML
     d/copy wPrEmplr
     d/copy wPrUsrPrms

    d*===================================================================
    d* Work Fields
    d*===================================================================
     d intCase         s              7s 0 Inz
     d intCo#          s              3s 0 Inz
     d intDiv          s              5s 0 Inz
     d intFnd          s              3s 0 Inz
     d intFyr          s              3s 0 Inz
     d intAmt          s              9s 2 Inz
     d intChk#         s              7s 0 Inz
     d intPos          s              2s 0 Inz
     d ChrChk#         s              7a   Inz
     d ChrClrDt        s             10a   Inz
     d intChkClrDt     s              7s 0 Inz
     d loopSqlStt      s                   Like(sqlStt)
     d nullInds        s              5i 0 dim(10)
     d strCltId        s              3
     d strSql          s           2700a   inz

    d*===================================================================
    d* Data Structures
    d*===================================================================
     d fids01        e ds                  extname(szq2p)
     d psds          esds                  extname(szq1p)

     d arrARAP         s              5s 4 Dim(3) Inz
     d arrExpMod       s              5s 4 Dim(3) Inz
     d arrExpModDate   s              7s 0 Dim(3) Inz
     d arrNotFinal     s              1a   Dim(3) Inz

     d dsClrChkData    ds                  Inz  Qualified
     d  Chk#                          7a
     d  Inv#                          7a
     d  Cached                       10a
     d  Claim#                       15a
     d  FEIN                          9a
     d  Amt                           9a

     d dsClaim#        ds                  Inz  Qualified
     d  ClmFnd                 1      3
     d  ClmFyr                 5      7
     d  ClmCas                 9     15

     d dsWDPAP       e ds                  extName(WDELP)
     d dsClrCk       e ds                  extName(WDRRP)

    d*===================================================================
    d* Main
    d*===================================================================
      /Free

       ProgramStart();
       MainProcess();
       ProgramEnd();

      /End-Free

      //===================================================================
      // MainProcess - Main process procedure
      //===================================================================
     p MainProcess     b
     d MainProcess     pi

        Exec Sql Declare C1 Cursor for
          Select CLChk#, CLInv#, CLCashed, CLClaim#, CLFEIN, CLAmt
          From   WXCLRCHKP
          Order by CLClaim#;

        Exec Sql Open C1;

        // Fetch records from Cleared Check File and Update WDPAP with Cashed Date
        Reset dsClrChkData;
        loopSqlStt = SqlStt;
        Dou Not IsValidFetch(LoopSqlStt);
           Exec SQL
           Fetch Next From C1 into :dsClrChkData :NullInds;
           loopSqlStt = SqlStt;
           If isValidFetch(loopSqlStt);
              // Perform edits
              UpdateWDPAP(*Off);
           // Invalid Fetch.  Leave the loop.
           Else;
              Leave;
           Endif;
           Reset dsClrChkData;
        Enddo;

        Exec Sql Close C1;

     p MainProcess     e

      //===================================================================
      // ProgramEnd - End Program
      //===================================================================
     p ProgramEnd      b
     d ProgramEnd      pi

        *inlr = *on;
        Return;

     p ProgramEnd      e

      //===================================================================
      // ProgramStart - Initial Program
      //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

        // Exec Sql Set Option commit = *None;

        // Get Company #
        intCo# = UserCo#(Q1User);

        //Retrieve Client-ID
        RtvCltID(strCltId);

     p ProgramStart    e
      //===================================================================
      // UpdateWDELP - Update WDELP with Experience Mod Information
      //===================================================================
     p UpdateWDPAP     b
     d UpdateWDPAP     pi
     d   inDone                        n   Const

     d  hldEffDate     s              7  0
     d  i              s              5  0
     d  orgPos         s              5  0                                      Original Position
     d  pos            s              5  0

       // Extract Values
          dsClaim#    = dsClrChkData.Claim#;
          ChrChk#     = dsClrChkData.Chk#;
          ChrClrDt    = dsClrChkData.Cached;

          Monitor;
             If dsClaim#.ClmFnd <> ' ';
                intFnd = %Dec(%Trim(dsClaim#.ClmFnd):3:0);
             Else;
                intFnd = 0;
             EndIf;
          On-Error;
             intFnd = 0;
          EndMon;

          Monitor;
             If dsClaim#.ClmFyr <> ' ';
                intFyr = %Dec(%Trim(dsClaim#.ClmFyr):3:0);
             Else;
                intFyr = 0;
             EndIf;
          On-Error;
             intFyr = 0;
          EndMon;

          Monitor;
             If dsClaim#.ClmCas <> ' ';
                intCase = %Dec(%Trim(dsClaim#.ClmCas):7:0);
             Else;
                intCase = 0;
             EndIf;
          On-Error;
             intCase = 0;
          EndMon;

          Monitor;
             If ChrChk# <> ' ';
                intChk# = %Dec(ChrChk#:7:0);
             Else;
                intChk# = 0;
             EndIf;
          On-Error;
             intChk# = 0;
          EndMon;

          Monitor;
             If ChrClrDt <> ' ';
                intChkClrDt = %Dec(%Char(%Date(ChrClrDt:*USA/):*CYMD0):7:0);
             Else;
                intChkClrDt = 0;
             EndIf;
          On-Error;
             intChkClrDt = 0;
          EndMon;

          Monitor;
             If dsClrChkData.Amt <> ' ';
                intPos = %Scan(',':dsClrChkData.Amt);
                Dow intPos <> 0;
                    dsClrChkData.Amt = %replace('':dsClrChkData.Amt:intPos:1);
                    intPos = %Scan(',':dsClrChkData.Amt);
                EndDo;
                intPos = %Scan('$':dsClrChkData.Amt);
                Dow intPos <> 0;
                    dsClrChkData.Amt = %replace('':dsClrChkData.Amt:intPos:1);
                    intPos = %Scan('$':dsClrChkData.Amt);
                EndDo;
                If dsClrChkData.Amt <> ' ';
                   intAmt = %Dec(dsClrChkData.Amt:9:2);
                EndIf;
             Else;
                intAmt = 0;
             EndIf;
          On-Error;
             intAmt = 0;
          EndMon;

       // Update Cleared Check Date
          If intChkClrDt <> 0;
          Exec Sql
             Update WDPAP
             Set PAClearDt = :intChkClrDt
             Where PAco# = :intCo# and PAFnd = :IntFnd and PAFyr = :intFyr and
                   PACase = :IntCase and PAChk# = :intChk# and PAAmt = :intAmt;
          EndIf;

     p UpdateWDPAP     e
