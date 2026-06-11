     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H/TITLE F2555R - Florida County Codes
     H DFTNAME(F2555R)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H/DEFINE   PROFOUNDUI
     H***** Beg Chg *** 04/20/17 ****************************************
     H*** BNDDIR('WTBNDDIR')
     H BNDDIR('STBNDDIR')
     H***** End Chg *** 04/20/17 ****************************************
     h**********************************************************************
      *===============================================================*
      *  Created   : 01/16/15                                         *
      *  Programmer: C:296                                            *
      *  Project   : FL079                                            *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      * 04/20/17  C:212  Project :                                    *
      *                  Corrected to use STBNDDIR                    *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*

     fF2555F    cf   e             workstn extfile('F2555F')
     f                                     HANDLER('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflCtyCd:rrnCtyCd)
     fftctypfl  uf a e             disk
     fftctyl1fl if   e           k disk    rename(pct01:pct02)
     fftctyl2fl uf a e           k disk    rename(pct01:pct03)
     f                                     infds(dbfds1)
     fftgtl     if   e           k disk
     fststl3    if   e           k disk    rename(pst01:pst03)
     fftccp     if   e           k disk

     d* procedure definitions
     d*
     d F2013R          pr                  extpgm('F2013R')
     d ##kc                           1
     d ##kl                           1
     d ##rrn                          9  0
     d ##proc                         5
     d ##prod                        50
     d ##rtcd                         1
     d ##ast                          2

     d F2521R          pr                  extpgm('F2521R')
     d ##call                         1
     d ##kc                           1
     d ##kl                           1
     d ##ast                          2
     d ##rrn                          9  0

     d*
     d RunCommand      pr                  extPgm('QCMDEXC')
     d inCmd                       2750    const
     d inCmdLen                      15  5 const

     d* data structures
     d*
     d psds          esds                  extname(szq1p)

     d dbfds1          ds
     d  rrnftctyp            397    400b 0

     d                 ds
     d  @@cmd                  1     56
     d  @@st                  38     39

     d* array definitions
     d*
     d qca             s             56    dim(6) ctdata perrcd(1)              qcmdexc

     d* standalone variables
     d*
     d ##kc            s              1    inz
     d ##kl            s              1    inz
     d ##rrn           s              9  0 inz
     d ##proc          s              5    inz
     d ##prod          s             50    inz
     d ##rtcd          s              1    inz
     d ##ast           s              2    inz
     d ##call          s              1    inz

     D inCmd           s           2750
     D inCmdLen        s             15  5

     d ##mode          s              1    inz
     d addMode         s               n   inz(*off)
     d chgMode         s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d isValid         s               n   inz(*off)
     d mode            s              1    inz
     d process         s               n   inz(*off)
     d xxRrn           s              9  0 inz
     d xxProcCd        s              5  0 inz
     d xxKey           s             10    inz
     d yyKey           s              5p 0 inz
     d @@mode          s              1
     d company         s              3p 0 inz
     d efdtky          s                   like(Ctyefdt) inz
     d varLen          s              2  0
     d varLenCty       s              2  0
     d varCtyCnty      s                   like(CtyCnty)
     d varCtyCntyU     s                   like(CtyCnty)
     d CtyCntyU        s                   like(CtyCnty)
     d varCtyCode      s                   like(CtyCode)
     d varCtyEfdt      s                   like(CtyEfdt)
     d varEffdt        s                   like(CtyEfdt)
     d uc              c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     d lc              c                   'abcdefghijklmnopqrstuvwxyz'

     d* Clear Subfiles
     d ClrSubFiles     pr

     d* load county code grid
     dloadCountyCode   pr

     d* main Processing
     d main            pr

     d* program Start
     d programStart    pr
     d processMe                       n

     d* process addition
     d processAdd      pr

     d* process other options
     d procOther       pr

     d* Validate Add/Edit screen
     d Validate        pr

     d* Add details
     d addDetails      pr

     d* Move database record to screen
     d MoveDbSc        pr

     d*/copy sPrDate
     d*/copy sPrString

     d*/copy wPrUsrPrms

     c* Entry of Program.
     c     *Entry        Plist
     c                   Parm                    @@call            1            call
     c                   Parm                    @@kc              1
     c                   Parm                    @@kl              1
     c                   Parm                    @@sta             2

      /free

       // main program processing
          xxRrn = 1;

       // Program Mode.
       If %parms >= 1;
          mode = @@call;
       Endif;

       // Tab Index Mode.
       TabIndex = 0;

       Select;
       When mode = 'D';
          dspMode = *On;
          dspVisb = *Off;
       When mode = 'A';
          addMode = *On;
          dspVisb = *On;
       When mode = 'C';
          chgMode= *On;
          dspVisb = *On;
       Endsl;

       ProgramStart(process);

       If process;
          Main();
          //@@rrn = %Editc(xxRRN:'X');
       Endif;

       *inlr = *on;
       Return;

      /end-free
       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     P ClrSubFiles     B
     D ClrSubFiles     PI
      /Free

       rrnCtyCd = 0;
       clrCCSub = *On;
       dspCCSub = *Off;
       dspCCCtl = *Off;
       Write RcdCtyCd;

      /End-Free
     P ClrSubfiles     E
       //===================================================================
       // Load Rightcoding Information
       //===================================================================
     PloadCountyCode   B
     dloadCountyCode   PI
      /free

       // set subfile indicators
       clrCCSub = *Off;
       dspCCSub = *On;
       dspCCCtl = *On;

       // read file and load the subfile
       Setll *start ftctyl2fl;
       Read pct03;
       dow not %eof(ftctyl2fl);
          clear sflCtyCd;

          scCCEffDat = %date(CtyEfDt:*cymd);
          scCtyCd    = CtyCode;
          scCounty   = CtyCnty;
          scMod      = CtygMod;
          scCCStat   = CtyStat;

          scRrnCtyCd = rrnftctyp;

          rrnCtyCd   = rrnCtyCd   + 1;
          If rrnCtyCd   = 1;
             scCCRowSl = 'A';
          Else;
             clear scCCRowSl;
          Endif;
          If mode = 'D';
             dspVisb = *Off;
          Else;
             dspVisb = *On;
          Endif;

          Write sflCtyCd;

          Read pct03;
       Enddo;

       rrnCtyCd   = 1;

      /end-free
     PloadCountyCode   E
      //===================================================================
       // Main - Main processing
      //===================================================================
     P Main            B
     d Main            PI

      /free

       // Process until exit button is pressed.
       Dou ExitNow = *On;

          // Clear Sub-files
          ClrSubfiles();

          // Load Rightcoding Information
          LoadCountyCode ();

          // Process Rightcoding screen.
          Exsr ExfmtScreens;

          Select;
          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          When btnRefresh = *On;
             btnRefresh =*Off;

          // Add button processing
          When scCCAdd = *on;
             clear RcdAddCC;
             ##mode = 'A';
             BtnAddRcd = *off;
             BtnCanRcd = *off;
             processAdd();

          // Other options processing
          Other;
             procOther();

          Endsl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

       When TabIndex = 0;
          TabHeight = 1050;
          Write rcdTabPan;
          Exfmt RcdCtyCd  ;
          Read rcdTabPan;
       EndSl;

       Endsr;

      /end-free
     P Main            E
       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     P ProgramStart    B
     d ProgramStart    PI
     d processMe                       n

      /free

       // If Rrn is passed, verify
       If xxRrn > *zeros;

         // Chain to County Code File for Program.
         Chain(n) xxRrn pct01;

         // Process only if valid record found.
         If not %found(ftctypfl);
          processme = *Off;
          Return;
         Endif;

       Endif;

       processMe = *on;

       TabIndex = 0;

       // get company of the user
       // company = UserCo#(q1user);

       Return;

      /end-free
     P ProgramStart    E
       //===================================================================
       // processAdd - process addition
       //===================================================================
     P processAdd      B
     d processAdd      PI
      /free

       isValid  = *on;

       // processing for Edit mode
       If ##mode = 'C';

        // effective date should be disabled in edit mode
          dsbAddEfDt = *on;

        // state should be disabled in edit mode
          dsbAddCty = *on;

          varEffdt = %dec(%char(scCCEffDat:*cymd0):7:0);
          Chain (scCtyCd: varEffdt) pct03;
          MoveDbSc();

       Endif;

       // processing for Add mode
       If ##mode = 'A';

        // effective date should not be disabled in edit mode
          dsbAddEfDt = *off;

        // state should not be disabled in edit mode
          dsbAddCty = *off;

       Endif;

       Dou BtnCanRcd = *on or (BtnAddRcd = *on and isValid = *on);
          Exfmt rcdAddCC;
          isValid = *on;
          //scAddCCdFo = *off;
          scAddCtyFo = *off;
          Select;

          When BtnAddRcd = *on;
             Validate();
             If isValid = *on;
                addDetails();
             Endif;

          When BtnCanRcd = *on;
             BtnCanRcd = *off;
             Leave;

          Other;
          Endsl;
       Enddo;
      /end-free
     P processAdd      E
       //===================================================================
       // procOther - process other options
       //===================================================================
     P procOther       B
     d procOther       PI
      /free

       ReadC sflCtyCd  ;

       Select;

       // processing for Edit link
       When scCCEdt = *on;
          scCCEdt = *off;
          clear RcdAddCC;
          ##mode = 'C';
          BtnAddRcd = *off;
          BtnCanRcd = *off;
          processAdd();

       // processing for Delete link
       When scCCDlt = *on;
          scCCDlt = *off;
          varEffdt = %dec(%char(scCCEffDat:*cymd0):7:0);
          Chain (scCtyCd: varEffdt) pct03;
          delete pct03;

       // processing for Change Status link
       When scCCChgSts = *on;
          scCCChgSts = *off;
          varEffdt = %dec(%char(scCCEffDat:*cymd0):7:0);
          Chain (scCtyCd: varEffdt) pct03;
          if scCCStat = 'A';
             CtyStat = 'I';
          else;
             CtyStat = 'A';
          endif;
          update pct03;
       Endsl;

      /end-free
     P procOther       E
       //===================================================================
       // Validate - Validate Add/Edit screen
       //===================================================================
     P Validate        B
     d Validate        PI
     c
      /free

       isValid  = *on;
       erAddEffDt = *off;
       //erAddCtyCd = *off;
       erAddCty  = *off;
       erAddMod  = *off;
       erAddSts  = *off;

       If ##mode = 'A';

         // Effective Date is a required entry
         If %char(scAddEffDt) = *blanks or scAddEffDt = *loval;
            msAddEffDt = 'MCR0503';
            erAddEffDt = *on;
            isValid  = *off;
            return;
         Endif;

         // Verify if the combination entered is not already present
         If isValid  = *on;
            varlen = %scan('-':scAddCty);
               If varLen <> 0;
                  varLenCty = %len(scAddCty);
                  varCtyCode = %subst(scAddCty:1:(varLen - 1));
                  varCtyEfdt = %dec(%char(scAddEffDt:*cymd0):7:0);
                  varCtyCnty = %subst(scAddCty:(varLen + 1):
                   (varLenCty - varLen));
                  varCtyCntyU = %xlate(LC : UC : varCtyCnty);
               Else;
                  msgAddCty = 'Please select correct combination of +
                   County code and county from list';
                  errAddCty = *on;
                  isValid = *off;
                  return;
               Endif;

            Chain (varCtyCode) pct03;
            if %found(ftctyl2fl);
               CtyCntyU = %xlate(LC : UC : CtyCnty);
               if CtyCntyU <> varCtyCntyU;
                  msgAddCty = 'Please select correct combination of +
                   County code and county from list';
                  errAddCty = *on;
                  isValid = *off;
                  return;
               Endif;
            Endif;

            Setll (varCtyCode:varCtyEfdt) pct03;
            if %equal(ftctyl2fl);
               msgAddefDt = 'County code already present for this effective +
                date';
               errAddefDt = *on;
               isValid = *off;
               return;
            Endif;
         Endif;

       EndIf;


       // Validate Status.
       If scAddSts <> 'A' and scAddSts <> 'I' and scAddSts <> *blanks;
          MSGADDSTS = 'Valid valus for Status are A and I';
          ERRADDSTS = *on;
          isValid  = *off;
          return;
       Endif;

       // County Code is a required entry
       If scAddCty = *blanks;
          MSADDCTY  = 'WCR0503';
          erAddCty  = *on;
          isValid  = *off;
          return;
       Endif;


      /end-free
     P Validate        E
       //===================================================================
       // addDetails - Add Details
       //===================================================================
     P addDetails      B
     d addDetails      PI
     c
      /free

       varEffdt = %dec(%char(scCCEffDat:*cymd0):7:0);
       Chain (scCtyCd: varEffdt) pct03;

       CtyEfdt = %dec(%char(scAddEffDt:*cymd0):7:0);
       CtygMod = scAddMod ;

       If scAddSts <> *blanks;
          CtyStat = scAddSts ;
       Else;
          CtyStat = 'I';
       Endif;

       If ##mode = 'A';
          varlen = %scan('-':scAddCty);
          varLenCty = %len(scAddCty);
          CtyCode = %subst(scAddCty:1:(varLen - 1));
          CtyCnty = %subst(scAddCty:(varLen + 1): (varLenCty - varLen));
          CtyUser = q1user;
          CtyPgm  = q1pgm;
          CtyDt   = %Dec(%Date():*cymd);
          CtyTime = %Dec(%Time():*hms);
          CtyIUsr = q1user;
          CtyIPgm = q1pgm;
          CtyIDt  = %Dec(%Date():*cymd);
          CtyITim = %Dec(%Time():*hms);

          Write pct03;

       Else;
          CtyUser = q1user;
          CtyPgm  = q1pgm;
          CtyDt   = %Dec(%Date():*cymd);
          CtyTime = %Dec(%Time():*hms);
          Update pct03;
       Endif;

      /end-free
     P addDetails      E
       //===================================================================
       // MoveDbSc - Move database record to screen
       //===================================================================
     P MoveDbSc        B
     d MoveDbSc        PI
     c
      /free

       scAddEffDt = %date(CtyEfDt:*cymd);
       //scAddCtyCd = CtyCode;
       scAddCty   = CtyCnty;
       scAddMod   = CtygMod;
       scAddSts   = CtyStat;

      /end-free
     P MoveDbSc        E
       //===================================================================

**  QCMDEXC TABLE
DLTOVR FILE(FTFEP)
OVRDBF FILE(FTFEP) TOFILE(*LIBL/FTFEP  )
DLTOVR FILE(FTFEL)
OVRDBF FILE(FTFEL) TOFILE(*LIBL/FTFEL  )
DLTOVR FILE(FMPRL)
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
