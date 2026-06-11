     H Copyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H/TITLE WTPOLARINQ - Policy A/R Inquiry (Profound)
     H DFTNAME(WTPOLARINQ)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP('QILE')
     H/DEFINE   PROFOUNDUI
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
    *===============================================================*
    *  Created   : 03/07/12                                         *
    *  Programmer: C:190                                            *
    *  Project   : WC2126                                           *
    *===============================================================*
    *  Revisions:                                                   *
    *                                                               *
    * ------------------------------------------------------------- *
    *  NOTE: Please add comments to the top of Revisions.           *
    * ------------------------------------------------------------- *
    *===============================================================*

     fWTPOLSHL  cf   e             workstn extfile('WTPOLSHL')
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)

     fWDELP     IF   E             disk    rename(PEL01:PEL0P)

      /copy SCOPYPOLPR

     D PSDS          ESDS                  extname(SZQ1P)

     d xxRrn           s              9  0
     D addMode         s               n   inz(*off)
     D chgMode         s               n   inz(*off)
     D dspMode         s               n   inz(*off)
     D ClientID        s              3    inz('000')
     D Process         s               n   inz(*off)

     d* Proto-types

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d* Program Start
     d ProgramStart    pr
     d ProcessMe                       n

     d* Main Processing
     d Main            pr

     d* Load Selection fields
     d LoadSelection   pr

     d* Load Detail fields
     d LoadDetail      pr

     d/copy sPrDate
     d/copy wPrAgent
     d/copy wPrProduce
     d/copy wPrGroup
     d/copy wPrRateSet
     d/copy wPrEmplr
     d/copy wPrCarrier

     c*
     c* Entry of Program.
     c*
     c     *Entry        PLIST
     c                   parm                    @@rrn             9            wdelp
     c                   parm                    @@mode            1            mode
     c
      /free

       // Relative Record # for employer detail.
       If %parms >= 1;
         xxRrn = %int(@@rrn);
       Else;
         xxRrn = 3386;
       Endif;

       // Program Mode.
       If %parms >= 2;
       Select;
       When @@mode='D';
         dspMode=*on;
       When @@mode='A';
         addMode=*on;
       When @@mode='C';
         chgMode=*on;
       Endsl;
       Endif;

       GetClientID(ClientID);

       ProgramStart(Process);

       If Process;
         Main();
       Endif;


       *InLr = *On;
      /end-free

       //***************************************************************************//
       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     P ProgramStart    B
     D ProgramStart    PI
     D ProcessMe                       n
      /free


       // Chain to Employer Detail for Program.
       Chain xxRrn PEL0P;

       // Process only if valid employer detail record found.
       If not %found;
         processme = *Off;
         Return;
       Endif;

         scCo#  = ELco#;
         scFnd  = ELfnd;
         scEmp# = ELemp#;
         scDiv  = ELdiv;
         scFyr  = ELfyr;

         kkDiv = ELdiv;
         kkFyr = ELfyr;

         ProcessMe = *on;
         Return;

      /end-free
     P ProgramStart    E

       //***************************************************************************//
       //===================================================================
       // Main - Main processing
       //===================================================================
     P Main            B
     D Main            PI
      /free

       //*  Process until exit button is pressed.

       DoU ExitNow = *On;


       //*  Load action bar
         LoadActionBar();

       //*  Load detail panel
         LoadDetail();

       //*  Process Action pannel bar.
         Write rcdActPnl;

       //*  Process second screen.
         Read rcdActPnl;

       //* Menu Choices

         If scSelOpt <> *blanks;
           Menu();
           Clear scSelOpt;
         Endif;

       //* Popup Windows

         Popup();

       Enddo;

      /end-free
     P Main            E

       //===================================================================
       // Load Policy Summary - Load the policy summary fields.
       //===================================================================
     P LoadDetail      B
     D LoadDetail      PI
      /free


      /end-free
     P LoadDetail      E


       //***************************************************************************//
     c*
      * utility procedure definitions
      /COPY SCOPYPOLPI
