     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
      * Description:  This pgm updates the company file to indicate   *
      *               activation of project WC2700                    *
      *                                                               *
      * Created by:   120    03/09/20      Project: WC2700            *
      *****************************************************************
      *
     FSMCOP     UF   E             DISK
       Read SMCOP;
       Dow not %eof(SMCOP);
          COwc2700 = %dec(%date():*cymd);
          Update PCO01;
          Read SMCOP;
       Enddo;
       *inlr = *On;
