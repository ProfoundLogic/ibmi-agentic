     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h/title SR12307I- IL Exclusions

      //****************************************************************
      //  PROGRAM DESCRIPTION:                                         *
      //    THIS PROGRAM PROMPTS THE USER TO ENTER ADDITIONAL INFO FOR *
      //    ENDORSEMENT WC120307.                                      *
      //                                                               *
      //  PROGRAMMER: C:302                                            *
      //  CREATED:    05/26/15               PROJECT: WC2366           *
      //                                                               *
      //****************************************************************
      //                                                               *
      //  THIS PROGRAM COLLECTS CUSTOM DATA FOR ENDORSEMENTS AND       *
      //  STORES IT IN FILE WLEDP, USING GENERIC DATA FIELDS.  EACH    *
      //  CUSTOMIZED FORM USES THESE FIELDS DIFFERENTLY.  THIS IS A    *
      //  LIST OF THE AVAILABLE GENERIC FIELD FORMATS.                 *
      //                                                               *
      //  FMA701           AMOUNT    7.0                               *
      //  FMC401                    40   CHARACTER                     *
      //  FMC401                    40   CHARACTER                     *
      //  FMC201                    20   CHARACTER                     *
      //  FMC101                    10   CHARACTER                     *
      //  FMC102                    10   CHARACTER                     *
      //  FMC103                    10   CHARACTER                     *
      //                                                               *
      //****************************************************************
      //  REVISIONS:                                                   *
      //                                                               *
      //   03/23/16 C:308 Project: Elevate                             *
      //                  Genericize the toolbar programming           *
      //                                                               *
      //   01/07/16 C:65  Project: WC2401                              *
      //                  Recompile                                    *
      //                                                               *
      //   09/17/15 C:302 Project: WC2254                              *
      //                  Recompile                                    *
      //****************************************************************

     fSR12307f  cf   e             workstn extfile('SR12307F')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflEndr:rrnEndr)
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
      *****Begin Add*****  03/23/16  *************************************
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)
      ***** End Add *****  03/23/16  *************************************

     fWmeml     if   e           k disk
     fWdell     if   e           k disk
     fWledl     uf   e           k disk    rename(ped01:ped00)
     f                                     Infds(dbfds1)
     fWledp     uf a e             disk    rename(ped01:ped0p)

     d psds          esds                  extname(szq1p)

     d dbfds1          ds
     d  dbrrn1               397    400b 0
     d                 ds

     d @@TabIndex      s              5
     d xxCo#           s                   like(FMco#)
     d xxFnd           s                   like(FMfnd)
     d xxEmp#          s                   like(FMemp#)
     d xxDiv           s                   like(FMdiv)
     d xxFyr           s                   like(FMfyr)
     d xxFrm1          s                   like(FMform)
     d scfnd           s                   like(ELfnd)
     d scfyr           s                   like(ELfyr)
     d scemp#          s                   like(ELemp#)
     d scdiv           s                   like(ELdiv)
     d clientID        s              3    inz('000')
     d inMode          s              1
     d inFound         s               n
     d inTabIndex      s              5  0
     d intCo#          s              3  0
     d intTmp          s              3  0
     d xxRrn           s              9  0
     d ##Rrn#          s              9  0
     d ##CltId         s              3
     d isMenuAvbl      s               n   inz(*on)
     d DSIndex         s              5  0
     d FldIndex        s              5  0
     d xxForm          c                   Const('WC120307  ')

      * Exclusion name in file.
     D ExclusionDS     DS
     D  FM2001                      200a
     D  FM2002                      200a
     D  FM2003                      200a
     D  FM2004                      200a
     D  FM2005                      200a
     D  FM2006                      200a
     D  FM2007                      200a
     D  FM2008                      200a
     D  FM2009                      200a
     D  FM2010                      200a
     D  FM2011                      200a
     D  FM2012                      200a
     D  FM2013                      200a
     D  FM2014                      200a
     D  FM2015                      200a
     D  FM2016                      200a
     D  FM2017                      200a
     D  FM2018                      200a
     D  FM2019                      200a
     D  FM2020                      200a
     D  FM2021                      200a
     D  FM2022                      200a
     D  FM2023                      200a
     D  FM2024                      200a
     D  FM2025                      200a
     D  FM2026                      200a
     D  FM2027                      200a
     D  FM2028                      200a
     D   fm                         200a   Overlay(ExclusionDS) Dim(28)

     D AllExclusionDS...
     D                 ds                  Qualified Dim(10)
     D   fields                     200A   Dim(28)

      //****************************************************************
      // Prototype Declaration
      //****************************************************************

     D SR12307I        PR
     D                               40
     D                                1
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3

     D SR12307I        PI
     D  x1prds                       40
     D  x1Mode                        1
     d  x1Co#                         3
     d  x1Fnd                         3
     D  x1Emp#                        9
     D  x1Div                         5
     D  x1Fyr                         3

      // Clear Subfiles
     d ClrSubFiles     pr

      // Edit/Add Endorse Record
     d EdtAddEndr      pr

      // Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d ##CltId                        3

      // Load Endorse Value grid
     d LoadEndrGrid    pr

      // Main Processing
     d Main            pr

      // Add/Del/Edit Endorse Value record
     d ProcEdtDelAdd   pr
     d inMode                         1A   Const
     d inRecord                       3  0 value
     d inField                        3  0 value

      // Process other options
     d ProcOther       pr

      // Program Start
     d ProgramStart    pr

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrString
     d/copy sPrCompany
     d/copy sCopyPolPr
     d/copy wPrAgent
     d/copy wPrProduce
     d/copy wPrGroup
     d/copy wPrRateSet
     d/copy wPrEmpLr
     d/copy wPrCarrier
     d/copy wPrUsrPrms
    
      /Free

       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       xxCo#  =  %Int(x1Co#);
       xxFnd  =  %Int(x1Fnd);
       xxEmp# =  %Int(x1Emp#);
       xxDiv  =  %Int(x1Div);
       xxFyr  =  %Int(x1Fyr);

       // Load Action panel with the Company Name.
       intCo# = userCo#(q1user);

       // Get the client id
       GetClientID(##CltId);

       tabClick = *Off;

       // Read the Employer Detail File
       Chain (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr) wdell;

       // Load Policy Action Bar
       LoadActionBar(isMenuAvbl);

       // Process until exit button is pressed.
       Dou ExitNow = *On ;

          // Clear Sub-files
          ClrSubfiles();

          // Load Endorse Grid
          LoadEndrGrid();
          Clear intTmp;

          // Process Endorse Screen
          TabHeight = 880;
          APnlHeight = TabHeight + 60;
          Write rcdPolPnl;
          Write rcdTabEndr;
          Exfmt rcdEndr;
          Read rcdPolPnl;
          Read rcdTabEndr;

          Select;
          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;
          When BtnRefresh = *On;
             BtnRefresh =*Off;
          // Add/Edit Detail Record
          When btnEndrAdd = *on;
             ProcEdtDelAdd('A':intTmp:intTmp);
          When btnEndrCan = *on;
             Leave;
          Other;
             ProcOther();
          EndSl;

       Enddo;

      /End-Free

     p Main            e

       //===================================================================
       // ProcEdtDelAdd - Edit/Del/Add Endorse Record
       //===================================================================
     p ProcEdtDelAdd   b
     d ProcEdtDelAdd   pi
     d inMode                         1A   Const
     d inRecord                       3  0 value
     d inField                        3  0 value

      /Free

       // Load Action panel with the Company Name.
       intCo# = userCo#(q1user);

       // Get the client id
       GetClientID(##CltId);

       tabClick = *Off;

       Clear RcdAdEndr;
       btnAddRcd = *off;
       btnCanRcd = *off;
       indDel = *Off;

       // Process to Delete Endorse Record
       If inMode = 'D';
          scExclusio = AllExclusionDS(inRecord).fields(inField);
          AllExclusionDS(inRecord).fields(inField) = *Blanks;
          indDel = *on;
          intCanLf = 630;
       Endif;

       // Processing to Edit Endorse Record
       If inMode = 'C';
          scExclusio = AllExclusionDS(inRecord).fields(inField);

          // Set button description.
          scBtnDsc = 'Save/Edit Next';
          intCanLf = 520;

       Endif;
       // Processing to Add Endorse Record
       If inMode = 'A';

          scExclusio = *Blanks;
          // Set button description.
          scBtnDsc = 'Save/Add Next';
          intCanLf = 520;
          inFound = *Off;

          For DSIndex = 1 to 10;
             For FldIndex = 1 to 28;
                If  AllExclusionDS(DSIndex).fields(FldIndex) =  *Blanks;
                   inRecord = DSIndex;
                   inField = FldIndex;
                   inFound = *on;
                   Leave;
                EndIf;
             EndFor;
             If inFound;
                Leave;
             Endif;
          EndFor;

       Endif;

       Dou (BtnCanRcd  = *On) or (BtnAddRcd = *on) Or (BtnAdNxRcd  = *Off);

          Exfmt rcdAdEndr;

          Select;
             When BtnAddRcd = *on or BtnAdNxRcd = *on;
                If inMode <> 'D';
                   AllExclusionDS(inRecord).fields(inField) = scExclusio;
                EndIf;
                EdtAddEndr();

                If inMode = 'A';
                   inFound = *Off;
                   For DSIndex = 1 to 10;
                      For FldIndex = 1 to 28;
                         If  AllExclusionDS(DSIndex).fields(FldIndex) =
                              *Blanks;
                            inRecord = DSIndex;
                            inField = FldIndex;
                            inFound = *on;
                            Leave;
                         EndIf;
                      EndFor;
                      If inFound;
                         Leave;
                      Endif;
                   EndFor;
                   scExclusio = *Blanks;
                   If Not inFound;
                      Leave;
                   Endif;
                EndIf;

                If inMode = 'C';
                   If inField = 28;
                      inField = 1;
                      inRecord = inRecord + 1;
                   Else;
                      inField = inField + 1;
                   EndIf;
                   If inRecord > 10 or
                        AllExclusionDS(inRecord).fields(inField) =  *Blanks;
                      Leave;
                   Endif;
                   scExclusio = AllExclusionDS(inRecord).fields(inField);
                EndIf;

             When BtnCanRcd = *on;
                BtnCanRcd = *off;
                Leave;
          Endsl;
       Enddo;

      /End-Free

     p ProcEdtDelAdd   e

       //===================================================================
       // EdtAddEndr -  Upd/Add Endorse Record in WXFMP1 Endorse File.
       //===================================================================
     p EdtAddEndr      b
     d EdtAddEndr      pi
     d addIndex        s              3  0

      /Free

       Dou not %found(wledl);
          Delete (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr:xxFrm1) wledl;
       Enddo;

       Clear ped0p;
       FMco# = xxCo#;
       FMFnd = xxFnd;
       FMemp# = xxEmp#;
       FMdiv = xxDiv;
       FMfyr = xxFyr;
       FMform = xxFrm1;
       FMseq# = 0;

       FMpgm  = q1pgm;
       FMuser = q1user;
       FMdt   = DateTo7(SystemDate);
       FMtime = TimeTo6(SystemTime);
       FMipgm = q1pgm;
       FMiusr = q1user;
       FMidt  = DateTo7(SystemDate);

       addIndex = 0;

       For DSIndex = 1 to 10;
          For FldIndex = 1 to 28;

             If  AllExclusionDS(DSIndex).fields(FldIndex) <> *Blanks;
                addIndex = addIndex + 1;
                fm(addIndex) = AllExclusionDS(DSIndex).fields(FldIndex);
                If addIndex = 28;
                   Write ped0p;
                   Clear fm;
                   addIndex = *zeros;
                   FMseq# = FMseq# + 1;
                Endif;
             EndIf;

          EndFor;

       EndFor;

       If addIndex > *zeros;
          Write ped0p;
       Endif;

      /End-Free

     p EdtAddEndr      e

       //===================================================================
       // ProcOther - Process Other Options
       //===================================================================
     p ProcOther       b
     d ProcOther       pi

      /Free

       // Edit Endorse Record

       ReadC sflEndr;

       If scEndrEdt  = *On;
          scEndrEdt  = *Off;
          ProcEdtDelAdd('C':scRecord:scField);
       Endif;

       If scEndrDel = *On;
          scEndrDel = *Off;
          ProcEdtDelAdd('D':scRecord:scField);
       Endif;

      /End-Free

     p ProcOther       e

       //===================================================================
       // Load Endorse Value Detail Grid
       //===================================================================
     p LoadEndrGrid    b
     d LoadEndrGrid    pi

      /Free

       // Clear the subfile.
       Clear sflEndr;
       clrEndr = *Off;
       Clear AllExclusionDS;

       If inMode = 'D';
          dspVisb = *Off;
       Else;
          dspVisb = *On;
       EndIF;

       dspEndrSub = *On;
       dspEndrCtl = *On;
       clear dbrrn1;

       xxFrm1 =  xxForm;

       Setll (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr:xxFrm1) wledl;
       Reade (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr:xxFrm1) wledl;

       DSIndex = 0;
       Dow Not %Eof(wledl);

          DSIndex = DSIndex + 1;

          For FldIndex = 1 to 28;

             AllExclusionDS(DSIndex).fields(FldIndex) =  fm(FldIndex);

          Endfor;

          Reade (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr:xxFrm1) wledl;

       Enddo;

       For DSIndex = 1 to 10;

          For FldIndex = 1 to 28;

             If AllExclusionDS(DSIndex).fields(FldIndex) <> *blanks;
                scExclusio = AllExclusionDS(DSIndex).fields(FldIndex);
                scEndrRrn# = dbrrn1;
                scRecord = DSIndex;
                scField = FldIndex;
                rrnEndr = rrnEndr + 1;
                Write sflEndr;
             EndIf;

          Endfor;

       EndFor;

       rrnEndr = 1;

      /End-Free

     p LoadEndrGrid    e

       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     p ClrSubFiles     b
     d ClrSubFiles     pi

      /Free

       rrnEndr = 0;
       clrEndr = *On;
       dspEndrSub  = *Off;
       dspEndrCtl  = *Off;
       Write rcdEndr;

      /End-Free

     p ClrSubfiles     e

       //===================================================================
      // Utility policy procedure definitions
      /COPY SCOPYPOLPI
