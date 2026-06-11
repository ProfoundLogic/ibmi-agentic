     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H**********************************************************************
     H/TITLE WTRPTEXPGM - Program to test EXITPGM parameters
     H Option(*SrcStmt : *NoDebugIO)
     H dftactgrp(*no) actgrp(*CALLER)
     h**********************************************************************
      *================================================================
      * Revisions:                                                    *
      * 02/06/24  C:291  Project: C102WJI                             *
      *                  This program is called from STFILECHG program*
      *                  that calls program defiend in Exit program   *
      *                  parameter.                                   *
      *                                                               *
      *================================================================
     d dsData          ds
     d  type                          1
     d  data                      32764
     d dsChgOld        ds                  likeds(dsData)
     d dsChgNew        ds                  likeds(dsData)

      *entry
     dWTRPTEXPGM       pr                  Extpgm('WTRPTEXPGM')
     d inTrgName                    128
     d inUser                        10
     d inPgm                         10
     d inLibFile                     10
     d inLibObj                      10
     d inLibEnv                      10
     d inChgOld                   32765a
     d inChgNew                   32765a
     d inRptOld                   32765a
     d inRptNew                   32765a

     dWTRPTEXPGM       pi
     d inTrgName                    128
     d inUser                        10
     d inPgm                         10
     d inLibFile                     10
     d inLibObj                      10
     d inLibEnv                      10
     d inChgOld                   32765a
     d inChgNew                   32765a
     d inRptOld                   32765a
     d inRptNew                   32765a

       Exec SQL
        set option commit = *none;

       dsChgOld = inChgOld;
       dsChgNew = inChgNew;
       If dsChgOld.data = dsChgNew.data;
          Return;
       Endif;

       Exec sql
        Insert into WXEXTPGM (EXTRGNAME, EXUSER, EXPGM, EXLIBFILE,
        EXLIBOBJ, EXLIBENV, EXCHGOLD, EXCHGNEW, EXRPTOLD, EXRPTNEW)
        Values (
         :inTrgName, :inUser, :inPgm, :inLibFile, :inLibObj, :inLibEnv,
         :inChgOld, :inChgNew, :inRptOld, :inRptNew);

       *Inlr = *On;

