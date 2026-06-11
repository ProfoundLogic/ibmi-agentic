     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H bnddir('QC2LE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE FIXCLMNMS - Report/Update Name Mismatch between Demographics & Claim master
     H*****************************************************************
     H* DESCRIPTION: The program will list all Claims with Names mismatch
     H* with Demographis. First it will list all duplicate records in
     H* Demographics file itself.
     H*****************************************************************
     H** REVISIONS:
     H*
     H* 06/01/14  C:242 PROJECT: original
     H*****************************************************************

     FWMCML     UF A E           K DISK    Usropn
     FFIXCLMNMS2UF A E           K DISK

      *-------- Prototypes (entry/return parms) --------------------------*
     DClearFiles       PR
     DCDPDuplicates    PR
     DCMPMismatch      PR
      *-------------------------------------------------------------------*
     D PSDS           SDS
     D  PROC             *PROC
     D  STATUS           *STATUS
     D  ROUTINE          *ROUTINE
     D  PARMS            *PARMS

     D FIXCLMNMS1DS    DS                  inz
     D  C1DSS#                 1      9
     D  C1DLSN                10     39
     D  C1DFSN                40     54
     D  C1DMID                55     55

     D FIXCLMNMS2DS    DS                  inz
     D  C2DSS#                 1      9
     D  C2DLSN                10     39
     D  C2DFSN                40     54
     D  C2DMID                55     55
     D  C2CO#                 56     57P 0
     D  C2FND                 58     59P 0
     D  C2FYR                 60     61P 0
     D  C2EMP#                62     66P 0
     D  C2DIV                 67     69P 0
     D  C2CASE                70     73P 0
     D  C2MLSN                74    103
     D  C2MFSN               104    118
     D  C2MMID               119    119

     D @@Mode          S             10
     D strDate         S             10D
     D strTime         S              8T
     D @SQLcode        S             10I 0
     D @SQLstate       S              5

     C     *entry        Plist
     C                   Parm                    @@Mode
      /free
        strDate = %Date();
        strTime = %Time();
        Exec Sql
          Set Option
            Commit = *None,
            DatFmt = *ISO;
        if @@Mode = 'UPDATE';
           open WMCML;
        Endif;

        C2Sts = @@Mode;
        C2Date = %Date();
        C2Time = %Time();

        ClearFiles();
        CDPDuplicates();
        CMPMismatch();

        if @@Mode = 'UPDATE';
           close WMCML;
        Endif;
        *inlr = *on;
        return;
      /end-free

      *//===================================================================
      *// ClearFiles - Clear internal Work files FIXCLMNMS1 & FIXCLMNMS2
      *//===================================================================
     P ClearFiles      B
     D ClearFiles      PI
      /free
         Exec SQL
           Delete from FIXCLMNMS1;
         @SQLcode = SQLCODE;
         @SQLstate= SQLSTATE;

         Exec SQL
           Delete from FIXCLMNMS2;
         @SQLcode = SQLCODE;
         @SQLstate= SQLSTATE;

      /end-free
     P  ClearFiles     E
      *//===================================================================
      *// CDPDuplicates - List Duplicate SSNs in WMCDP
      *//===================================================================
     P CDPDuplicates   B
     D CDPDuplicates   PI
      /free

         Exec SQL
           Insert Into FIXCLMNMS1
            (C1SS#, C1LSN, C1FSN, C1MID)
            SELECT CDSS#, CDLSN, CDFSN, CDMID
            FROM wmcdp
            WHERE cdss# in
            (SELECT CDSS# FROM wmcdp
             GROUP BY CDSS# HAVING count(*) > 1 ) ;
         @SQLcode = SQLCODE;
         @SQLstate= SQLSTATE;

      /end-free
     P  CDPDuplicates  E
      *//===================================================================
      *// CMPMismatch - List Names Mismatch in WMCMP
      *//===================================================================
     P CMPMismatch     B
     D CMPMismatch     PI

     D*** FIXCLMNMS2DS  E DS                  extname(FIXCLMNMS2)
      /free
         Exec SQL
           Declare MisMtchCSR Cursor for
             SELECT a.CDSS#, a.CDLSN, a.CDFSN, a.CDMID,
              b.CMCO#, b.CMFND, b.CMFYR, b.CMEMP#, b.CMDIV,
              b.CMCASE, b.CMLSN, b.CMFSN, b.CMMID
             FROM wmcdp a, wmcmp b
             WHERE a.CDSS#= b.CMSS# and
              a.CDSS# not in (Select C1SS# from FIXCLMNMS1) and
              (a.CDLSN <> b.CmLSN or a.CDFSN <> b.CmFSN or a.CDMID <> b.CmMID);
         Exec SQL
           Open MisMtchCSR;
         Exec SQL
           Fetch MisMtchCSR into :FIXCLMNMS2DS;
         @SQLcode = SQLCODE;
         @SQLstate= SQLSTATE;
         Dow SQLCOD = 0;
           If @@Mode = 'UPDATE'; //Update mode, Set CMP Name same as CDP
             CMLSN = C2DLSN;
             CMFSN = C2DFSN;
             CMMID = C2DMID;
             Update pcm01;
           Endif;
           Write PC201;

           Exec SQL
             Fetch MisMtchCSR into :FIXCLMNMS2DS;
           @SQLcode = SQLCODE;
           @SQLstate= SQLSTATE;
         Enddo;

      /end-free
     P  CMPMismatch    E
      *//===================================================================
