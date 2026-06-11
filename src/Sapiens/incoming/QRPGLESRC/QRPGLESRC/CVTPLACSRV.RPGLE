     Hcopyright('(c) 2021 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H/TITLE CVTPLACSRV - Fix program to add 1 new Place of Service 02 -
     H/TITLE              Telemedicine.
     H DFTNAME(CVTPLACSRV)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP('QILE')
     h**********************************************************************
      *===============================================================*
      *  Created   : 04/27/21                                         *
      *  Programmer: C:295                                            *
      *  Project   : FS611                                            *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*

     fftpsl     uf a e           k disk    usropn
     fststl     if   e           k disk
     fsmcol     if   e           k disk

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
          If STkey <> 'CK' and STkey <> 'CR' ;
             exsr ovrState;
             If indErr = *off;
                Setll *Loval pco01;
                Read pco01;
                DoW not %eof(smcol);
                    Chain (COCO#: 02) pps01;
                    If not %found(ftpsl);
                       PSco#  = COco#;
                       PSkey  = '  ';
                       PSpos  =  02 ;
                       PSdesc = 'Telemedicine';
                       PS1a1  = *blanks;
                       PS1a2  = *blanks;
                       PS1a3  = *blanks;
                       PS1a4  = *blanks;
                       PS1a5  = *blanks;
                       PS6n1  = 0;
                       PS6n2  = 0;
                       PS6n3  = 0;
                       PS6n4  = 0;
                       PS6n5  = 0;
                       PS40a1 = *blanks;
                       PSstat = 'A';
                       PSuser = Q1user;
                       PSpgm  = Q1pgm;
                       PStime = %dec(%char(%time():*iso0):6:0);
                       PSdt   = %dec(%char(%date():*cymd0):7:0);
                       PSiusr = q1user;
                       PSidt  = %dec(%char(%date():*cymd0):7:0);
                       write pps01;
                       Endif;
                Read pco01;
                EndDo;
             EndIf;
          EndIf;
          Read ststl;
        EndDo;

       monitor;
        Unlock FTPSL;
       on-error;
       endmon;

       *inlr = *on;

       //===================================================================
       // OvrState - Override State
       //===================================================================
       BegSr OvrState;

          indErr = *off;
         // if open close file
          if %open(FTPSL);
             close FTPSL;
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
           if not %open(FTPSL);
              Monitor;
              open FTPSL;
              On-error;
                 indErr = *on;
              endmon;
           endif;
          on-error;
           indErr = *on;
          endmon;

       EndSr;
      /End-Free


**  QCMDEXC TABLE
DLTOVR FILE(FTPSL)
OVRDBF FILE(FTPSL) TOFILE(*LIBL/FTPSL  )
