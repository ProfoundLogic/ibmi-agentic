     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio: *srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('WTBNDDIR')
      *================================================================
     H* Update WMCMP/CMCATA with value 12
     H*
     H* Project : WC2852 C:291  Date: 04/18/22.
     H*
     H* Called from: SYPOSTPGM2
     H* Calls to:    None
     H*
     H*================================================================
     H* Revisions:
     H*
     H*****************************************************************
     D sysDt7          s              7  0
     D sysTime6        s              6  0
     D psds          esds                  extname(szq1p)

     D/copy sprdate

      /Free
       Exec SQL
        set option commit = *none;

        sysDt7 = DateTo7(SystemDate);
        sysTime6 = TimeTo6(SystemTime);

       Exec SQL
        Update wmcmp
         set CMcata = 12, CMuser = :q1user,
         CMpgm = :q1pgm, CMDt = :sysDt7, CMtime = :sysTime6
         where CMnatr = '83' and CMCAUS = '83' and CMACDT > 1191201;
       *inlr = *on;
      /End-free
