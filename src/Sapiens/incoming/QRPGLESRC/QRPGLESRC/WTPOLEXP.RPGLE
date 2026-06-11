     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H**************************************************************************************
     H*/TITLE WTPOLEXP - Policy extract - Populate Work File
    H**************************************************************************************
    H*  Description: Populates WXGSP workfile for Policy extract                          *
    H*                                                                                    *
    H*  Called from: job schedular                                                        *
    H*                                                                                    *
    H*  Calls to: WA001R                                                                  *
    H*                                                                                    *
    H*  Created: C:267  Project: C150WAT  08/08/16                       *                *
    H**************************************************************************************
    H*  Revisions:                                                                        *
     h*                                                                                    *
    H*                                                                                    *
    H**************************************************************************************
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwxgsp     uf a e             disk    rename(pgs01:pgs0p)
    F                                     usropn
     Fwxgsl     if   e           k disk    rename(pgs01:pgsl1)
    F*
    F*  Fund master file
     Fwmfnl     if   e           k disk    rename(pfn01:pfnl1)
     Fwdf2l     if   e           k disk
      * co/tpac
     fwmtcl     if   e           k disk    rename(ptc01:ptcl1)
     f*
     fwmupl     if   e           k disk    rename(pup01:pupl1)
      *
      * System program status
     d psds          esds                  extname(szq1p)
      *
     d QCmdExc         pr                  extpgm('QCMDEXC')
     d   command                           const like(strCommand)
     d   length                      15  5 const
      *
     d wtpolexpc       pr                  extpgm('WTPOLEXPC')
     d   ##CO#                        3
     d   ##MBR                       10
     d   ##EXLPRM                     1
     d   ##EXLPYR                     1
     d   ##PTHNAM                   256
     d   ##SITEIP                    15
     d   ##SITEFLD                   80
     d   ##USID                      10
     d   ##PSWD                      20
      *
     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)
      *
     d xxCO            s              3a   inz
     d xxjobn          s              3a   inz
     d xxTime          s              6a   inz
     d strCommand      s            255
     d sys_date        s               d   datfmt(*usa)
     d sys_date7       s              7  0
     d NewMbr          s             10    inz
     D iOpen           s               n   inz(*off)
     D xxpath          s            258a
      *
     c* Key list for fund master
     c*
     c     fnky          klist
     c                   kfld                    f2co#
     c                   kfld                    f2fnd

     c                   time                    dsdttm
     c     *usa          move      dsdate        sys_date
     c     *cymd         move      sys_date      sys_date7
      /Free
       // get user company
       chain q1user pupl1;
       xxCO = %editc(upco#:'X');

       // submit job to create extract for EACH TPA (all groups within company)
       chain upco# ptcl1;

       If %found;
        DoW not %Eof;
          If TCATGN <> 'Y';           //auto generate policy export
             ReadE upco# ptcl1;
             Iter;
          Endif;

          ExSr crtNewMbr;          // crt NEW mbr for THIS TPA/CO#
          ExSr OpnOvrDbf;          // override to new memeber; open file

          // process next TPA (co#, fnd) record
          SetLL tcco# pf201;
          ReadE tcco# pf201;
          Dow Not %Eof (WDF2L);
             If F2tpac <> ' ';
                If TCtpac = F2tpac;
                   Chain fnky PFNL1;        // get fund description
                   ExSr WrtWxgsp;           // Write out to WXGSP
                Endif;
             Endif;
             ReadE tcco# WDF2L;
          Enddo;

          //Submit policy export job for THIS TPA/CO#
          ExSr  ClsOvrDbf;         // close overrid
          ExSr  SbmPolExp;         // submit job for THIS TPA
          ReadE upco# ptcl1;
        Enddo;
       EndIf;

       *Inlr = *On;
       Return;

       //----------------------------------------------------//
       // WrtWxgsp                                           //
       //----------------------------------------------------//
       BegSr WrtWxgsp;
          clear pgs0p;
          gsco# = F2CO#;
          gsfnd = F2FND;
          gsname = %Trim(fnname);
          gsiefl = 'I';
          Write pgs0p;
       EndSr;

       //----------------------------------------------------//
       // CrtNewMbr                                          //
       //----------------------------------------------------//
       BegSr CrtNewMbr;
          //crt member name
          xxTime = %char(%time():*hms0);
          xxjobn = %subst(%Editc(Q1JOBN:'X'):4:3);
          NewMbr = 'J' + xxjobn  + xxTime;
       EndSr;

       //----------------------------------------------------//
       // OpnOvrDbf                                          //
       //----------------------------------------------------//
       BegSr OpnOvrDbf;
          // ADD PF Memeber
          Monitor;
          clear strCommand;
          strCommand = 'ADDPFM FILE(*libl/WXGSP) '+
           'MBR(' + %trim(NewMbr) + ') ';
          Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
          Endmon;

          // ADD LF Memeber
          Monitor;
          clear strCommand;
          strCommand = 'ADDLFM FILE(*libl/WXGSL) '+
           'MBR(' + %trim(NewMbr) + ') ' +
           'DTAMBRS((WXGSP (' + %trim(NewMbr) + ')))';
          Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
          Endmon;

          // Override file for that member
          Monitor;
          clear strCommand;
          strCommand = 'OVRDBF FILE(WXGSP) TOFILE(*libl/WXGSP) '+
           'MBR('  + %trim(NewMbr) + ') OVRSCOPE(*JOB)';
          Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
          Endmon;

          If not %Open(WXGSP);
             Open WXGSP;
             iOpen = *On;
          Endif;
       EndSr;

       //----------------------------------------------------//
       // ClsOvrDbf                                          //
       //----------------------------------------------------//
       BegSr ClsOvrDbf;
          //If open, then close it
          If %Open(WXGSP) And iOpen;
            Close WXGSP;
            iOpen = *Off;
          Endif;

          Monitor;
             clear strCommand;
             strCommand = 'DLTOVR FILE(WXGSP)';
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
          Endmon;
       EndSr;

       //----------------------------------------------------//
       // SbmPolExp                                          //
       //----------------------------------------------------//
       BegSr SbmPolExp;

          xxpath = tcpath;
          xxpath = %replace('##':xxpath:257);
          Callp wtpolexpc(xxCO:NewMbr:tceprm:tceprl:
                  xxpath:tcip:tcfldr:tcusid:tcspsw);
       EndSr;

      /End-Free
      ******************************************************************
