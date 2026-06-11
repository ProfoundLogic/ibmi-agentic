     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     h DftActGrp(*No) actgrp(*CALLER)
     h Option(*NoDebugIO: *SrcStmt)
     h BndDir('STBNDDIR')
     H/TITLE SIHUBCLN - Interface request log clean up
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:  Portal interface request log clean up   *
     H*                                                               *
     H* CALLED BY: Daily batch - should be a submitted job            *
     H*                                                               *
     H* CREATED: 08/06/19  C:33  Project: SY406                       *
     H*                                                               *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************

     ddecLogDays       s             10i 0 inz
     dstrCmd           s            255    varying
     dstrLogDays       s             10    varying
     dstrSuffix        s              3
     dtimeStamp        s               z   inz

     D/copy sPrAtrium
     D/copy sPrModule
     D/copy sPrRunCmd

     dRtvSuffix        pr                  extPgm('RTVSUFFIX')
     d inSuffix                       3    const

      /Free
       Exec sql
        set option commit = *none;
       RtvSuffix(strSuffix);
       strLogDays = GetModuleProperty('Portal Module':'Log Days');
       Monitor;
          decLogDays = %int(strLogDays);
       On-error;
       Endmon;
       If decLogDays = *zeros;
          decLogDays = 180;
       Endif;
       timeStamp = %timestamp() - %days(decLogDays);
       CleanUp('PDSCGI'+strSuffix);
       CleanUp('CGIPDN');
       *inlr = *on;
      /End-free
       //===========================
     PCleanUp          b
     DCleanUp          pi
     D inCgiLib                      10    varying const

     ddsLogFile        ds                  inz qualified
     d name                          10
     d lib                           10
      /Free
       strCmd = 'ADDLIBLE LIB(' + inCgiLib + ') POSITION(*AFTER QTEMP)';
       Monitor;
          RunCmd(strCmd:%len(strCmd));
       On-error;
       Endmon;
       Exec sql
        delete from REQUEST_LOG
         where REQUEST_TIMESTAMP < :timeStamp;
       Exec sql
        select SYS_TNAME, SYS_DNAME into :dsLogFile
         from SYSTABLES
         where TABLE_NAME = 'REQUEST_LOG' and TABLE_SCHEMA = :inCgilib;
       strCmd = 'RGZPFM ' +
        'FILE(' + %trim(dsLogFile.lib) + '/' + %trim(dsLogFile.name) + ')';
       Monitor;
          RunCmd(strCmd:%len(strCmd));
       On-error;
       Endmon;
      /End-free
     PCleanUp          e
