     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller) bnddir('STBNDDIR')
     H/define CONVERT
     //=====================================================================
     //=====================================================================
     h/TITLE ISIM53R - OptaComp Import Program
     //=====================================================================
     // Description:  This program creates a DARTS key to return and updates
     //   the line of business apps database.
     //
     //<!-- This program should be modified to incorporate your specIfic
     //     database and DARTS key needs.  -->
     //
     // Created   : 09/09/13
     // Programmer: C:33
     // Project   : 114WJC
      //  This is a modified version of ISIM03R.
     //
     //=====================================================================
     // Revisions:
     //
     // 06/19/18 C:062 Project: 152WAB
     //                Apply client prefix to path statement.
     //
     // 03/08/18 C:079 Project: 114MJB
     //                Use document suffix passed in instead of .TIF
     //
     // 03/30/16 C:062 Support : 40443(114)
     //                Update import file.
     //
     // 09/09/15 C:212 Project : 114MTD
     //                retrofitted 09/30/13 changes from V13R2 QA
     //                for conflict resolution
     // 09/20/13 C:02 Project: 114WJC
     //          C:33 Call the Claims Exit Routing routine if indexed
      //                parm RTNCOD for IS824R is set as 'X' if we're doing an index import
      //
     // 09/30/13 C:191 Project: 114WJC
      //                Lock dataarea optbase at start of the process
      //                Release lock at the end of the process
     //=====================================================================
     fOPTIMPT   UF   E           K DISK    rename(pop01:pimp01)
     f
      //
     //=====================================================================
     // constant definitions
     //=====================================================================
     // Image API's
     D CpyDlo          pr                  ExtPgm('IS801C')
     d                               12    const
     d                               80    const
     d                               12
     d                               80
     d                                1
     d                              255    options(*nopass:*varsize) const
     D DltDlo          pr                  ExtPgm('IS802C')
     d                               12    const
     d                               80    const
     d                                1
     d                              255    options(*nopass:*varsize) const

     d GetCltPrefix    pr                  ExtPgm('SA030R')
     d                                3
     d                               80

     d GtDcIx          pr                  ExtPgm('IS823R')
     d                                2
     d                                6
     d                                3
     d                                4
     d                               12
     d                               12
     d                                8
     d                               12
     d                               21
     d                                1
     D GtDcTp          pr                  ExtPgm('IS822R')
     d                                8
     d                                5
     d                               10
     d                                3
     d                               40
     d                               10
     d                                5
     d                                3
     d                               10
     d                                2
     d                               10
     d                               12
     d                                1
     D GTDOCN          C                   const('IS825R')
     D IncIdx          pr                  ExtPgm('IS826R')
     d                                2
     d                                1    const
     d                                1    const
     d                                1
     D UpBchs          pr                  ExtPgm('IS820R')
     d                               12
     d                                2
     d                                8
     d                               10
     d                                8
     d                                6
     d                                5
     d                                5
     d                                5
     d                                5
     d                                3
     d                                1
     d                               10
     d                               52
     d                               40
     d                                1
     D UpDocI          pr                  ExtPgm('IS824R')
     d                                2
     d                                6
     d                                4
     d                                8
     d                               12
     d                                5
     d                                8
     d                               12
     d                                5
     d                                8
     d                                6
     d                               12
     d                                4
     d                               40
     d                                5
     d                                1
     D***  Begin Add  ***  09/20/13  *****************************************
     D UserIndexExit   pr                  ExtPgm('ISE002C')
     d  docQueueId                   10
     d  toUser                       10    const
     d  userId                       10
     d  docId                        12
     d  dartKey                      40
     d  dartSeq#                      5
     d  returnCode                    6
     D***  End   Add  ***  09/20/13  *****************************************

     D***  Beg   Add  ***  09/30/13  *****************************************
     DQCMDEXC          pr                  ExtPgm('QCMDEXC')
     D command                             const like(strcommand)
     D length                        15  5 const
     D***  End   Add  ***  09/30/13  *****************************************

     D RtvDocRoot      pr                  ExtPgm('RTVDOCROOT')
     d                               10
     // Program to process additional information
     D ExtPgm          pr                  ExtPgm('ISIM53R1')
     d  data                               like(impDta)
     d                                7
     d                               40
      //
     //* Status
     D STSCMP          C                   const('C')
     D STSERR          C                   const('E')
     D STSIDX          C                   const('I')
     D STSQUE          C                   const('Q')
      //
      //
     //=====================================================================
     // Data Structure, Table & Array definitions
     //=====================================================================
     //* Program information
     D PSDS          ESDS                  ExtName(SZQ1P)
     d impDta        E DS                  ExtName(OPTIMPT)

     D***  Beg   Add  ***  09/30/13  *****************************************
     DstrCommand       s            255
     D***  End   Add  ***  09/30/13  *****************************************
     d##UncN           s            255
     d##Date           s              8
     d##Time           s              6
     d#PgTDc           s              4
     d#Bytes           s              5
     dbcErDs           s             52
     ddBchSq           s              4
     ddocSfl           s              1
     dnewDcF           s             80
     dpBchSt           s              2
     droot             s             10
     doldDir           s             80
     dnumPgD           s              3  0
     dmaxDoc           s              5  0
     daNumDc           s              5  0
     daNumPg           s              5  0
     deNumDc           s              5  0
     deNumPg           s              5  0
     dfirst            s              1n
     dmadBch           s              1n
     ddocSeq           s              6
     dsuffix           s              3
     dbchSeq           s              4
     dbtDcId           s             12
     dbtBchI           s             12
     dimgFld           s              8
     dimgSfl           s             12
     dnDocFl           s             21
     drmkCd            s              5
     dfilSys           s             10
     ddelim            s              3
     ddocDsc           s             40
     didxQue           s             10
     d#DcBch           s              5
     dsecLvl           s              3
     didxPgm           s             10
     dbchSts           s              2
     dimpPgm           s             10
     dimpDir           s             12
     drtnCod           s              1
     drtnCo            s              7
     daField           s             40
     da#Docs           s              5
     de#Docs           s              5
     da#Page           s              5
     de#Page           s              5
     d#PgPDc           s              3
     d#@doc            s             12
     d#@flr            s             80
     d##flr            s             80
     ddartKy           s             40
     ddartSq           s              5
     dsvBSts           s              2
     D***  Begin Add  ***  09/20/13  *****************************************
     dreturnCode       s              6    inz
     ddocQueueId       s             10    inz
     Dcimpf            s              1a
     DcimpDoc          s             12a
     D***  End   Add  ***  09/20/13  *****************************************
     D***  Beg Add *** 03/08/18 **********************************************
     DxxPos            s              3  0
     D***  Beg Add *** 03/08/18 **********************************************
     dprefix           s              3    inz
     dprefixFolder     s             80    inz

     d/copy sPrDate
     d*** Beg Add *** 03/08/18 *******************************************
     d/copy sPrModule
     d*** End Add *** 03/08/18 *******************************************
     //=====================================================================
     // PList definitions
     //=====================================================================
     // Entry parms

     C     *ENTRY        PLIST
     C                   PARM                    @WKSID            2            Work Station Id
     C                   PARM                    @DCTYP            8            Doc Type
     C                   PARM                    @RTNCD            1            Return Code
      //*
     //=====================================================================
     // KList definitions
     //=====================================================================
     c     keyOPTIMPT    klist
     C                   kfld                    @DCTYP
      //
     //=====================================================================
     // Main
     //=====================================================================
     //* Initalization
      /Free
       Exsr SrInit;

       // Calculate the estimated number of documents and pages
       Exsr NumDoc;

       // Get record from the database
       Setll keyOPTIMPT pimp01;
       // Do while non-qued and non-indexed entries exist
       dou %eof or aNumDc >= maxDoc;
          Reade keyOPTIMPT pimp01;
          // Only do If the record has not been indexed or queued
          If not %eof and OPstat = *blank;
             ##Date = %editc(DateTo8(SystemDate):'Z');
             ##Time = %editc(TimeTo6(SystemTime):'Z');
              //* If this is the first entry, write a batch record
             If first;
                Exsr SrFirst;
             EndIf;
             oldDir = OPsFld;
             // Perform the user specIfic processing
             Exsr Import;
             // Process the document
             Clear rtnCod;
             Exsr PrcDoc;
            // Update the Ascent record with 'Q', 'I' or 'E'
             // If no erros from PrcDoc then do SR.
             If rtnCod = *blank;
        //*** Begin Del *** 03/30/16 ********************************
        //******Exsr UpdDbf;
        //*** End   Del *** 03/30/16 ********************************
       //*** begin add 09/20/13 **************************
               // IF the Darts Key is not blank (and valid) we will call an exit
               //  Program here (ISIM03RV) to allow additional processing of index
               //  Information (Diary Flag/user, Description, Dates, etc)
               //  If we wrote a dart record, the seq# would be > *zeros
               If dartKy <> *blanks and dartSq <> *zeros;
                  // EXECUTE THE USER INDEX EXIT HERE
                  docQueueId = @dcTyp;
                  Callp(e) UserIndexExit(docQueueId:*blanks:q1User:cimpDoc:
                   dartKy:dartSq:returnCode);
               Endif;
       //*** end add 09/20/13 *****************************
                // Clear the dartKy field here
                Clear dartKy;
                Clear dartSq;
             EndIf;
          EndIf;
       Enddo;

       Exsr Finish;

       // End the program
       *inlr = *on;

      //=====================================================================
      // Finish - DO clean up stuff
      //=====================================================================
       Begsr Finish;

        //* If a batch was made, do stuff
        If madBch;
           a#Docs = %editc(aNumDc:'Z');
           a#Page = %editc(aNumPg:'Z');
           e#Docs = %editc(eNumDc:'Z');
           e#Page = %editc(eNumPg:'Z');
           ##Date = %editc(DateTo8(SystemDate):'Z');
           ##Time = %editc(TimeTo6(SystemTime):'Z');
           // Update the batch status record
           pBchSt = svBSts;
           Callp UpBchs(btBchI:pBchSt:@DcTyp:q1User:##Date:##Time:
            e#Docs:a#Docs:e#Page:a#Page:#PgPDc:docSfl:q1Pgm:BcERds:
            docDsc:rtnCod);
           // Increment the index info
           Callp IncIdx(@WksId:'0':'1':rtnCod);
        EndIf;
       //****** Beg Add ****** 09/30/13 ***************************
        Monitor;
          strCommand = 'DLCOBJ OBJ((OPTBASE *DTAARA *EXCL))';
          Callp QCMDEXC(strCommand:%len(strCommand));
        On-Error;
        EndMon;
       //****** End Add ****** 09/30/13 ***************************
         //
       Endsr;                                                                 //Finish

      //=====================================================================
      // Import - Do Import stuff
      //=====================================================================
       Begsr Import;
         //
        //<!-- You must move the created DARTS key to this field If you want
        //     your document indexed. -->
         //
         // CALL THE ExtPgm TO PROCESS THE REQUEST AND RETURN THE KEY TO
         // TO WRITE (INDEX) THE APP
         //
        Clear rtnCo;
        Clear aField;

        // Only process if incoming DART key is not already blanks
        If OPkey <> *blanks;
           Callp ExtPgm(impDta:rtnCo:aField);
           If rtnCo = *blanks;
              dartKy = aField;
           Else;
              Clear dartKy;
           Endif;
        EndIf;

        // Fill batch info to recordds processed regardless of indexed or not
        //OPsDat = Dt%dec(##Date:8:0);
        //OPsTim = %dec(##Time:6:0);

       Endsr;                                                                 //Import

      //=====================================================================
      // NumDoc - Calculate the estimated number of documents
      //=====================================================================
       Begsr NumDoc;

        Setll keyOPTIMPT pimp01;
        //* Do while entries exist
        dou %eof or aNumDc >= maxDoc;
           Reade keyOPTIMPT pimp01;
           // Only count If the record has not been indexed or queued
           If not %eof and OPstat = *blank;
              // Increment total number of docs processed
              eNumDc += 1;
              // Increment total number of pages
              eNumPg += numPgd;
           EndIf;
        Enddo;
        e#Docs = %editc(eNumDc:'Z');
        e#Page = %editc(eNumPg:'Z');

       Endsr;

      //=====================================================================
      // PrcDoc - Process the document
      //=====================================================================
       Begsr PrcDoc;

        Callp RtvDocRoot(root);
        //*** Beg Add *** 03/08/18 **************************************
        If isModEnabled('Use Image Extension');
           xxPos = %Scan('.':OPdcId:1);
           If xxPos <> 0;
              suffix = %Subst(OPdcId:xxPos + 1:4);
           Else;
              suffix = *blanks;
           Endif;
        Endif;
        // Get Doc index info
        Callp GtDcIx(@WksId:docSeq:suffix:bchSeq:btDcId:btBchI:imgFld:imgSfl:
         nDocFl:rtnCod);

        // If the program ended abnormally due to rollover error then
        // pass that erroe code to IS800R
        If rtnCod = 'S';
           @RtnCd = rtnCod;
           Return;
        EndIf;

        dBchSq = bchSeq;
        svBSts = bchSts;
        //*** End Add *** 03/08/18 **************************************

        OPsFld = '/' + %trim(@DcTyp);
        If prefixFolder = ' ';
           ##UncN = '/' + %trim(root) + %trim(OPsFld) + '/' + %trim(OPdcId);
        Else;
           ##UncN = '/' + %trim(root) + '/' + %trim(prefixFolder) +
              %trim(OPsFld) + '/' + %trim(OPdcId);
        EndIf;

        // Copy the doc to a built name/folder
        ##Flr = nDocFl;
        Callp CpyDlo(*blanks:*blanks:btDcId:##Flr:rtnCod:##UncN);

        // Clear Info For OptaComp Import File
        Clear #@doc;
        Clear #@flr;
        If rtnCod <> *blank;
           Leavesr;
        Endif;

        // Save Info for Opatcomp Import doc
        #@doc = btDcId;
        #@flr = ##flr;

        // Update the document info record
        dartSq = '00000';
         //****** 09/20/2013 ************************************
        if dartky <> *blanks;
          cimpf='X';
         else;
          cimpf=' ' ;
        endif        ;
        rtncod=cimpf;
        //*** Begin Add *** 03/30/16 ********************************
        Exsr UpdDbf;
        //*** End   Add *** 03/30/16 ********************************
         //****** 09/20/2013 ************************************
        Callp UpDocI(@WksId:docSeq:dBchSq:imgFld:imgSfl:a#Docs:@DcTyp:btDcId:
         #Bytes:##Date:##Time:btBchI:#PgTDc:dartKy:dartSq:rtnCod);
         //*** 09/20/13 ***********************
         //***   save the docid before it's changed below
           cimpDoc=BTDCID;
         //*** end add 9/20/2013 **********

        //*** Beg Add *** 03/08/18 **************************************
        If Not isModEnabled('Use Image Extension');
        //*** End Add *** 03/08/18 **************************************
        // Get Doc index info
           Callp GtDcIx(@WksId:docSeq:suffix:bchSeq:btDcId:btBchI:imgFld:imgSfl:
           nDocFl:rtnCod);

        // If the program ended abnormally due to rollover error then
        // pass that erroe code to IS800R
           If rtnCod = 'S';
              @RtnCd = rtnCod;
              Return;
           EndIf;
        //*** Beg Add *** 03/08/18 **************************************
        Endif;
        //*** End Add *** 03/08/18 **************************************

        //* Delete the original document
        Callp DltDlo(*blanks:*blanks:rtnCod:##UncN);

        // Increment total number of docs processed
        aNumDc += 1;
        // Increment total number of pages
        aNumPg += numPgD;

       Endsr;                                                                 //PrcDoc

      //=====================================================================
      // SrFirst - Processing the first time around
      //=====================================================================
       Begsr SrFirst;

        first = *off;
        madBch = *on;
        pBchSt = 'SC';

        // Create the batch record
        Callp UpBchs(btBchI:pBchSt:@DcTyp:q1User:##Date:##Time:
         e#Docs:a#Docs:e#Page:a#Page:#PgPDc:docSfl:q1Pgm:BcERds:
         docDsc:rtnCod);

       Endsr;                                                                 //SrFirst

      //=====================================================================
      // SrInit - Initialization subroutine
      //=====================================================================
       Begsr SrInit;

       //****** Beg Add ****** 09/30/13 ***************************
        Monitor;
          strCommand = 'ALCOBJ OBJ((OPTBASE *DTAARA *EXCL))';
          Callp QCMDEXC(strCommand:%len(strCommand));
        On-Error;
          Return;
        EndMon;
       //****** End Add ****** 09/30/13 ***************************
        // Get Doc type info
        Clear rtnCod;
        callp GtDcTp(@DcTyp:rmkCd:filSys:delim:docDsc:idxQue:#DcBch:secLvl:
         idxPgm:bchSts:impPgm:impDir:rtnCod);
        numPgD = %dec(delim:3:0);
        #PgpDc = %editc(numPgD:'Z');
        #PgtDc = %editc(numPgD:'Z');
        maxDoc = %dec(#DcBch:5:0);
        If maxDoc = 0;
           maxDoc = *hival;
           maxDoc -= 1;
        EndIf;

        // Get Doc index info
         Clear rtnCod;
         Callp GtDcIx(@WksId:docSeq:suffix:bchSeq:btDcId:btBchI:imgFld:imgSfl:
         nDocFl:rtnCod);

        // If the program ended abnormally due to rollover error then
        //  pass that error code to IS800R
         If rtnCod = 'S';
           @RtnCd = rtnCod;
           Return;
         EndIf;

         dBchSq = bchSeq;
         svBSts = bchSts;

        // Initialize control flags
        first = *on;
        madBch = *off;

        // Get the Client Prefix
        GetCltPrefix(prefix:prefixFolder);

       Endsr;

      //=====================================================================
      // UpdDbf - Update the Ascent database file with the status
      //=====================================================================
       Begsr UpdDbf;

        If dartKy = *blanks;
           OPstat = stsQue;
        Else;
           OPstat = stsIdx;
        EndIf;

        OPsDoc = #@Doc;
        OPsFld = #@Flr;
        OPuser = q1User;
        OPpgm  = q1Pgm;
        OPtime = %dec(##Time:6:0);
        OPdt   = dtCYMD(%dec(##Date:8:0));

        Update pimp01;

       Endsr;                                                                 //UpdDbf
      /End-free
