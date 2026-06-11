     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     Fwmfnl     IF   E           K DISK
     FWTXCL     IF A E           K DISK

     C*
      /free
       Read pfn01;
       Dow not %eof;
          If fnfnd <> 990;
             Setll (fnco#:990) pxc01;
             Reade (fnco#:990) pxc01;
             Dow not %eof;
                xcfnd = fnfnd;
                Write pxc01;
                Reade (fnco#:990) pxc01;
             Enddo;
          Endif;
          Read pfn01;
       Enddo;

      /end-free
     C*
     C                   MOVE      '1'           *INLR
