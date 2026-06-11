     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio: *srcstmt)
     H dftactgrp(*no) actgrp(*caller)
      *================================================================
     H* Load the WTELECAT table file for: Arkansas, Illinois, Kentucky,
     H*  Mississippi, Missouri, Tennessee
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
     DIndex            s              5  0
     DstrState         s              2a
     DstrDesc          s             40a
     DintStartDt       s              7s 0
     DintEndDt         s              7s 0
     DintCode          s              5s 0
     D#ofStates        c                   6
     DaryState         s              2    dim(#ofStates) ctdata perrcd(1)

      /Free
       Exec SQL
        set option commit = *none;

       intStartDt = 1211210;
       intEndDt   = 1211211;
       intCode    = 13;
       For Index = 1 to #ofStates;
          strState = aryState(Index);
          strDesc  = %trim('NCCI ELE Severe Storms in ' + aryState(Index));
          Exec Sql
           Insert Into WTELECAT
           (CACODE, CADESC, CAST, CASTDT, CAENDT)
           Values(:intCode, :strDesc, :strState, :intStartDt, :intEndDt);
       Endfor;
       *inlr = *on;
      /End-free
**
AR
IL
KY
MS
MO
TN
