     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H/TITLE CVTMODCMS - Fix program to add new CMS modifiers to state files
     H DFTNAME(CVTMODCMS)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP('QILE')
     h**********************************************************************
      *===============================================================*
      *  Created   : 03/16/20                                         *
      *  Programmer: C:212                                            *
      *  Project   : FS585                                            *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      *  07/31/20 C: 079 Support: FS585/90431                         *
      *                  Add modifier QW. Use SMCOL.                  *
      *                                                               *
      *  07/02/20 C: 079 Support: 99/89971                            *
      *                  Omit non states (NW)                         *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*

     fftabl     uf a e           k disk    usropn
     fststl     if   e           k disk
     f*** Beg Chg *** 07/31/20 *********************************************
     f***smcop     if   e             disk
     fsmcol     if   e           k disk
     f*** End Chg *** 07/31/20 *********************************************

     d*
     d RunCommand      pr                  extPgm('QCMDEXC')
     d inCmd                       2750    const
     d inCmdLen                      15  5 const

     d* data structures
     d*
     d psds          esds                  extname(szq1p)

     d                 ds
     d  @@cmd                  1     56
     d  @@st                  38     39

     d* array definitions
     d*
     d qca             s             56    dim(2) ctdata perrcd(1)              qcmdexc

     d* standalone variables
     d*

     D inCmd           s           2750    inz
     D inCmdLen        s             15  5 inz
     D indErr          s               n   inz(*off)

      /free

       // main program processing

       // read through state file and process for each state
       Setll *loval pst01;
       Read ststl;
       DoW not %eof(ststl);
       //*** Beg Chg *** 07/02/20 ******************************************
       //**If STkey <> 'CK' and STkey <> 'CR';
          If STkey <> 'CK' and STkey <> 'CR' and STkey <> 'NW';
       //*** End Chg *** 07/02/20 ******************************************
             exsr ovrState;
             If indErr = *off;
       //*** Beg Chg *** 07/31/20 ******************************************
       //****** Setll *start smcop;
                Setll *Loval pco01;
       //*** End Chg *** 07/31/20 ******************************************
                Read pco01;
                // process for each company
       //*** Beg Chg *** 07/31/20 ******************************************
       //*****  DoW not %eof(smcop);
                DoW not %eof(smcol);
       //*** End Chg *** 07/31/20 ******************************************

                   // pass in Mods to be written
                    wrtModPM(COco#:'XE');
                    wrtModPM(COco#:'XS');
                    wrtModPM(COco#:'XP');
                    wrtModPM(COco#:'XU');
                    wrtModPM(COco#:'95');
                    //*** Beg Add *** 07/31/20 *****************************
                    wrtModPM(COco#:'QW');
                    //*** End Add *** 07/31/20 *****************************
                Read pco01;
                EndDo;
             EndIf;
          EndIf;
          Read ststl;
       EndDo;

       monitor;
        Unlock FTABL;
       on-error;
       endmon;

       *inlr = *on;

       //===================================================================
       // OvrState - Override State
       //===================================================================
       BegSr OvrState;

          indErr = *off;
         // if open close file
          if %open(FTABL);
             close FTABL;
          endif;
         // delete existing override
          inCmdlen = 56;
          inCmd = QCA(1);
          monitor;
           callp RunCommand(inCmd:inCmdLen);
          on-error;
          endmon;
         // override with state file
          @@cmd = QCA(2);
          @@st  = STkey;
          inCmd = @@cmd;
          monitor;
           callp RunCommand(inCmd:inCmdLen);
          // open file for use
           if not %open(FTABL);
          //*** Beg Add *** 07/02/20 ***************************************
              Monitor;
          //*** End Add *** 07/02/20 ***************************************
              open FTABL;
          //*** Beg Add *** 07/02/20 ***************************************
              On-error;
                 indErr = *on;
              endmon;
          //*** End Add *** 07/02/20 ***************************************
           endif;
          on-error;
           indErr = *on;
          endmon;

       EndSr;
      /End-Free

      *//===================================================================
      *// WrtModPM - Write Modifier to FTPMPxx file
      *//===================================================================
     P WrtModPM        B
     D WrtModPM        PI
     D  inCo#                         3  0 const
     D  inMod                         2a   const
      *
     D xxKey           s             10a   inz
      /Free

       xxKey = inMod;
       Chain (inCo#:xxKey) ppm01;
       If not %found(ftabl);
          PMco# = inCo#;
          PMkey = inMod;
          Select;
           When inMod = 'XE';
            PMdesc = 'Separate Encounter: A service that is di';
           When inMod = 'XS';
            PMdesc = 'Separate Structure: A service that is di';
           When inMod = 'XP';
            PMdesc = 'Separate Practitioner: A service that is';
           When inMod = 'XU';
            PMdesc = 'Unusual Non-Overlapping Service: The use';
           When inMod = '95';
            PMdesc = 'Synchronous telemedicine service rendere';
           //*** Beg Add *** 07/31/20 **************************************
           When inMod = 'QW';
            PMdesc = 'Lab service is a CLIA (Clinical Laborato';
           //*** End Add *** 07/31/20 **************************************
          Other;
          Endsl;
          PMperc = 0;
          PMsurg = *blanks;
          PMprof = *blanks;
          PMstat = 'A';
          PMuser = q1user;
          PMpgm  = q1pgm;
          PMtime = %dec(%char(%time():*iso0):6:0);
          PMdt   = %dec(%char(%date():*cymd0):7:0);
          PMiusr = q1user;
          PMidt = %dec(%char(%date():*cymd0):7:0);

          Write ppm01;
       EndIf;

      /End-Free
     P WrtModPM        E
       //===================================================================

**  QCMDEXC TABLE
DLTOVR FILE(FTABL)
OVRDBF FILE(FTABL) TOFILE(*LIBL/FTABL  )
