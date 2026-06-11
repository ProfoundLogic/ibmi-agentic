     HCopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      *--------------------------------------------------------------------*
      *  Description:  This Program Will Display Object Descriptions
      *
      *  Created on 12/04/18 by C:180 for Support 74223
      *
      *  This pgm takes 1 parameter:                                              *
      *  Client 6A                                                                *
      *
      *  This pgm calls DSPOBJDL with 3 parameters:                               *
      *  Client 6A                                                                *
      *  File Name 10A                                                            *
      *  Mode 1A  ( '1' = Create new File, '0' = Add to existing file)            *
      *--------------------------------------------------------------------*
      * Revisions:
      * 10/15/20  C:180   Support:84293
      *                   Handle exception files.
      *                                                                    *
      * 07/24/20  C:180  SUPPORT 90330                                     *
      *                  Add file WXPWP                                    *
      *                                                                    *
      *--------------------------------------------------------------------*
      *
      * *Entry
      *
     D DspObjdR        PR
     D  inClient                      6

     D DspObjdR        PI
     D inClient                       6

      *
      * Stand Alone
      *
     D FN              S             20    DIM(68) CTDATA PERRCD(1)
     D ix              S              3  0 INZ
     D Client          S              6    INZ

     D DspObjdL        PR                  EXTPGM('DSPOBJDL')
     D                                6    Const
     D                               10    Const
     D                                1    Const
     D                               10    Const  OPTIONS(*NOPASS)

       Client = inClient;

      //Process Regular files
       DspObjdL(Client:%Subst(FN(1):1:10):'1':%subst(FN(1):11:10));
       For ix = 2 to 66;
          DspObjdL(Client:%subst(FN(ix):1:10):'0':%subst(FN(ix):11:10));
       Endfor;

       *inlr = *on;
**
fxisdp
fxishp
sxccp
sxcneobp
sxcpop
sxcpsp
sxdcnteobp
sxdeceobp
sxdgeobp
sxdmp
sxdnteobp
sxdop
sxdop1
sxdop2
sxdteobp
sxfrp
sxfteobp
sxgdeobp
sxgneobp
sxgteobp
sxhdeobp
sxpcp
sxphp     V6R5QADTA
sxphpclm
sxphpclmt
sxphpdtl
sxphprpt
sxphprptt
sxrheobp
sxrteobp
sxrwpamt
sxrwpben
sxrwprpt
sxrwprsv
sxrwpsec
sxsmeobp
sxsneobp
sxtoteobp
sxtrpout0
sxtrpout1
sxtrpout2
waisp
wxacpstmt1
wxacpstmt2
wxacpstmt3
wxacpstmt4
wxacpstmt5
wxacpstmt6
wxacpstmtc
wxagp1
wxbip
wxbip1
wxbip2
wxc7p
wxchp1
wxcip1    V7R0QADTA
wxeup1
wxisp
wxl8p
wxmip1
wxmip2
wxmlenp1
wxp2p
wxp3p
wxp4p
wxpwp
