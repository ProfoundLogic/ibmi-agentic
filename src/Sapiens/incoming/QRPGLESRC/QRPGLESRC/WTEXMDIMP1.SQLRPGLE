     hCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H*Option(*NoDebugio:*SrcStmt)
     H DftActGrp(*No) ActGrp(*Caller)
     H/Title WTEXMDIMP - Experience Mod/ARAP Import Process
     H Bnddir('WTBNDDIR')
     H*===============================================================
     H* Created on 04/04/22 by c:62 for Project 129WWZ
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
     d arap1           s              5s 4
     d arap2           s              5s 4
     d arap3           s              5s 4
     d chrRateState    s              2a
     d expMod1         s              5s 4
     d expMod2         s              5s 4
     d expMod3         s              5s 4
     d expModDate1     s              7s 0
     d expModDate2     s              7s 0
     d expModDate3     s              7s 0
     d isFirstRcd      s               n   Inz(*On)
     d hldDiv          s              5s 0 Inz
     d hldEmp#         s              9s 0 Inz
     d hldFnd          s              3s 0 Inz
     d hldFyr          s              3s 0 Inz
     d index           s              5s 0 Inz
     d intARAP         s              5s 4 Inz
     d intCo#          s              3s 0 Inz
     d intDiv          s              5s 0 Inz
     d intEffDate7     s              7s 0 Inz
     d intEffDate8     s              8s 0 Inz
     d intEmp#         s              9s 0 Inz
     d intExpMod       s              5s 4 Inz
     d intFnd          s              3s 0 Inz
     d intFyr          s              3s 0 Inz
     d intPolEffDt     s              7s 0 Inz
     d loopSqlStt      s                   Like(sqlStt)
     d notFinal1       s              1a
     d notFinal2       s              1a
     d notFinal3       s              1a
     d nullInds        s              5i 0 dim(10)
     d strCltId        s              3
     d strSql          s           2700a   inz
     d sysDate7        s              7s 0
     d sysTime6        s              6s 0

    d*===================================================================
    d* Data Structures
    d*===================================================================
     d fids01        e ds                  extname(szq2p)
     d psds          esds                  extname(szq1p)

     d arrARAP         s              5s 4 Dim(3) Inz
     d arrExpMod       s              5s 4 Dim(3) Inz
     d arrExpModDate   s              7s 0 Dim(3) Inz
     d arrNotFinal     s              1a   Dim(3) Inz

     d dsExpModArap    ds                  Inz  Qualified
     d  format                        1a
     d  #ofRecs                       9a
     d  fnd                           3a
     d  emp#                          9a
     d  div                           5a
     d  year                          3a
     d  mod                           6a
     d  effDate                       8a
     d  arap                          6a
     d  notFinal                      1a
     d  filler                       59a

     d dsExpModInfo    ds                  Qualified Dim(99) Inz
     d  expMod                        5s 4
     d  expModDate                    7s 0
     d  arap                          5s 4
     d  notFinal                      1a

     d dsWDELP       e ds                  extName(WDELP)
     d dsWDRRP       e ds                  extName(WDRRP)

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

      /Free

        Clear dsExpModInfo;

        Exec Sql Declare C1 Cursor for
          Select * from WXEXMDIMP
          Where EXformat = 'D' And EXrcdError <> 'Y'
          Order by EXfnd, EXemp#, EXdiv, EXfyr, EXefDt;

        Exec Sql Open C1;

        // Fetch records from Exp Mod file and update WDELP
        Reset dsExpModArap;
        loopSqlStt = SqlStt;
        Dou Not IsValidFetch(LoopSqlStt);
           Exec SQL
           Fetch Next From C1 into :dsExpModArap :NullInds;
           loopSqlStt = SqlStt;
           If isValidFetch(loopSqlStt);
              // Perform edits
              UpdateWDELP(*Off);
           // Invalid Fetch.  Leave the loop.
           Else;
              UpdateWDELP(*On);
              Leave;
           Endif;
           Reset dsExpModArap;
        Enddo;

        Exec Sql Close C1;

      /End-Free

      /End-Free

     p MainProcess     e

      //===================================================================
      // ProgramEnd - End Program
      //===================================================================
     p ProgramEnd      b
     d ProgramEnd      pi
      /Free

        *inlr = *on;
        Return;

      /End-free
     p ProgramEnd      e

      //===================================================================
      // ProgramStart - Initial Program
      //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free

        // Exec Sql Set Option commit = *None;

        // Get Company #
        intCo# = UserCo#(Q1User);

        //Retrieve Client-ID
        RtvCltID(strCltId);

      /End-Free

     p ProgramStart    e
      //===================================================================
      // UpdateWDELP - Update WDELP with Experience Mod Information
      //===================================================================
     p UpdateWDELP     b
     d UpdateWDELP     pi
     d   inDone                        n   Const

     d  hldEffDate     s              7  0
     d  i              s              5  0
     d  orgPos         s              5  0                                      Original Position
     d  pos            s              5  0
      /Free

       // Extract Values
       If inDone = *Off;
          intFnd      = %Dec(%Trim(dsExpModArap.fnd):3:0);
          intEmp#     = %Dec(%Trim(dsExpModArap.emp#):9:0);
          intDiv      = %Dec(%Trim(dsExpModArap.div):5:0);
          intFyr      = %Dec(%Trim(dsExpModArap.year):3:0);

          intEffDate8 = %Dec(%Trim(dsExpModArap.effDate):8:0);
          intEffDate7 = dtCYMD(intEffDate8);

          If dsExpModArap.mod = ' ';
             intExpMod = 0;
          Else;
             intExpMod = %Dec(%Trim(dsExpModArap.mod):5:4);
          EndIf;

          If dsExpModArap.arap = ' ';
             intARAP = 0;
          Else;
             intARAP = %Dec(%Trim(dsExpModArap.arap):5:4);
          EndIf;
       EndIf;

       // Populate hold fields for first record.
       If isfirstRcd;
          hldFnd     = intFnd;
          hldEmp#    = intEmp#;
          hldDiv     = intDiv;
          hldFyr     = intFyr;
          isfirstRcd = *Off;
       EndIf;

       // Update once last record for a policy is read
       If intFnd <> hldFnd Or intEmp# <> hldEmp# Or intDiv <> hldDiv Or
          intFyr <> hldFyr Or inDone = *On;

          // Hold Policy's Experience Modifier Information
          ExSr HoldExpModInfo;

          // Determine how to assign new values
          ExSr AssignExpMod;

          // Populate work fields
          sysDate7    = DateTo7(systemDate());
          sysTime6    = TimeTo6(systemtime());

          // Update WDELP
          Exec Sql
             Update WDELP
             Set ELexm1 = :expMod1,  ELexm2 = :expMod2,  ELexm3 = :expMod3,
                 ELemd1 = :expModDate1,  ELemd2 = :expModDate2,
                 ELemd3 = :expModDate3,
                 ELarp1 = :arap1,  ELarp2 = :arap2,  ELarp3 = :arap3,
                 ELexMd1NF = :notFinal1,  ELexMd2NF = :notFinal2,
                 ELexMd3NF = :notFinal3,
                 ELpgm = :Q1pgm, ELuser = :Q1user, ELdt = :sysDate7,
                 ELtime = :sysTime6
             Where ELco# = :intCo# And ELfnd = :hldFnd And ELemp# = :hldEmp# And
                   ELdiv = :hldDiv And ELfyr = :hldFyr;

          // Reset key fields and other hold fields
          ExSr ResetKeyFields;

       Else;
          index += 1;
          dsExpModInfo(index).expMod     = intExpMod;
          dsExpModInfo(index).expModDate = intEffDate7;
          dsExpModInfo(index).arap       = intARAP;
          dsExpModInfo(index).notFinal   = dsExpModArap.notFinal;
       EndIf;

       //===================================================================
       // AssignExpMod - Determines how to apply the experience mod changes
       //===================================================================
       BegSR AssignExpMod;

          // Clear work fields
          Clear arap1;
          Clear arap2;
          Clear arap3;
          Clear expMod1;
          Clear expMod2;
          Clear expMod3;
          Clear expModDate1;
          Clear expModDate2;
          Clear expModDate3;
          Clear hldEffDate;
          Clear notFinal1;
          Clear notFinal2;
          Clear notFinal3;

          // Assign experience modifier changes
          // Load current policy values
          arap1       = arrARAP(1);
          arap2       = arrARAP(2);
          arap3       = arrARAP(3);
          expMod1     = arrExpmod(1);
          expMod2     = arrExpmod(2);
          expMod3     = arrExpmod(3);
          expModDate1 = arrExpModDate(1);
          expModDate2 = arrExpModDate(2);
          expModDate3 = arrExpModDate(3);
          notFinal1   = arrNotFinal(1);
          notFinal2   = arrNotFinal(2);
          notFinal3   = arrNotFinal(3);

          // Load changes
          // Find next available spot
          pos    = %LookUp(0:arrExpModDate);
          orgPos = pos;

          // Read thru imported exp mod changes
          For i=1 to 99;
             If dsExpModInfo(i).expModDate <> 0;

                // If the effective date is used again then update the
                // same experience mod slot
                If hldEffDate = dsExpModInfo(i).expModDate;
                   pos -= 1;
                EndIf;

                // If experience mod 1 slot is not populated then load
                // even if effective date is less than policy from date.
                If orgPos = 1 And dsExpModInfo(i).expModDate < intPolEffDt;
                   If dsExpModInfo(i).expModDate > hldEffDate And
                      hldEffDate <> 0;
                      pos -= 1;
                   Else;
                      If arrExpModDate(1) <> 0;
                         iter;
                      EndIf;
                   Endif;
                EndIf;

                Select;
                When pos = 1;
                   arap1       = dsExpModInfo(i).arap;
                   expMod1     = dsExpModInfo(i).expMod;
                   expModDate1 = dsExpModInfo(i).expModDate;
                   notFinal1   = dsExpModInfo(i).notFinal;
                When pos = 2;
                   arap2       = dsExpModInfo(i).arap;
                   expMod2     = dsExpModInfo(i).expMod;
                   expModDate2 = dsExpModInfo(i).expModDate;
                   notFinal2   = dsExpModInfo(i).notFinal;
                When pos = 3;
                   arap3       = dsExpModInfo(i).arap;
                   expMod3     = dsExpModInfo(i).expMod;
                   expModDate3 = dsExpModInfo(i).expModDate;
                   notFinal3   = dsExpModInfo(i).notFinal;
                EndSl;
             Else;
                Leave;
             EndIf;
             // Increment and save
             hldEffDate = dsExpModInfo(i).expModDate;
             pos += 1;
          EndFor;

       EndSR;

       //===================================================================
       // HoldExpModInfo - Hold the Policy's Experience Mod Information
       //===================================================================
       BegSR HoldExpModInfo;

          // Access Policy
          Clear dsWDELP;
          Exec Sql
             Select *
             Into :dsWDELP
             From WDELP
             Where ELco# = :intCo# And ELfnd = :hldFnd And ELemp# = :hldEmp# And
                   ELdiv = :hldDiv And ELfyr = :hldFyr;

          // Hold Policy's Experience Modifier Information
          arrExpMod(1)     = ELexM1;
          arrExpMod(2)     = ELexM2;
          arrExpMod(3)     = ELexM3;
          arrExpModDate(1) = ELemd1;
          arrExpModDate(2) = ELemd2;
          arrExpModDate(3) = ELemd3;
          arrArap(1)       = ELARP1;
          arrArap(2)       = ELARP2;
          arrArap(3)       = ELARP3;
          ArrNotFinal(1)   = ELexMd1NF;
          ArrNotFinal(2)   = ELexMd2NF;
          ArrNotFinal(3)   = ELexMd3NF;

          // Get the rating state
          chrRateState = GetRatingState(intCo#:hldFnd:hldEmp#:hldDiv:hldFyr);

          // Save the Policy from Date
          intPolEffDt = ELpfDt;

       EndSR;

       //===================================================================
       // ResetKeyFields - Resets the key fields used to determine if
       //                  the policy has changed
       //===================================================================
       BegSR ResetKeyFields;

          hldFnd  = intFnd;
          hldEmp# = intEmp#;
          hldDiv  = intDiv;
          hldFyr  = intFyr;

          Clear arrARAP;
          Clear arrExpMod;
          Clear arrExpModDate;
          Clear arrNotFinal;
          Clear chrRateState;
          Clear dsExpModInfo;
          Clear hldEffDate;
          Clear index;
          Clear intPolEffDt;

          If inDone = *Off;
             intEffDate8 = %Dec(%Trim(dsExpModArap.effDate):8:0);
             intEffDate7 = dtCYMD(intEffDate8);

             If dsExpModArap.mod = ' ';
                intExpMod = 0;
             Else;
                intExpMod = %Dec(%Trim(dsExpModArap.mod):5:4);
             EndIf;

             If dsExpModArap.arap = ' ';
                intARAP = 0;
             Else;
                intARAP = %Dec(%Trim(dsExpModArap.arap):5:4);
             EndIf;

             index += 1;
             dsExpModInfo(index).expMod     = intExpMod;
             dsExpModInfo(index).expModDate = intEffDate7;
             dsExpModInfo(index).arap       = intARAP;
             dsExpModInfo(index).notFinal   = dsExpModArap.notFinal;
          EndIf;

       EndSR;

      /End-Free

     p UpdateWDELP     e
