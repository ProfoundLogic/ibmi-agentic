     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H bnddir('WTBNDDIR')
     H/define WTQLRRN
    H*********************************************************************
     H/title WTQLRRN - Returen Live/Quote RRN
    H*********************************************************************
     H*
     H*
     H* Created: 10/16/12   C:190  Project: WC2116
    H*********************************************************************
     H* Revisions:
     h*
     h* 12/07/22 C:300 Support: 104084 (LUBA)
     h*               -OVRDBF WDELL to LIVE library causing error in
     h*                copying renewal pending quotes
     h**********************************************************************
     H*
     H*
     Fwdell     if   e           k disk    rename(pel01:pel00)  usropn
     F                                     infds(ELfds)
     Fwdellq    if   e           k disk    rename(pel01:pel00q) usropn
     F                                     infds(ELfdsQ)

     DQCMDEXC          pr                  ExtPgm('QCMDEXC')
     D command                             const like(strCommand)
     D length                        15  5 const
     D*
     D ELfds           ds
     D  ELrrn                397    400b 0
     D ELfdsQ          ds
     D  ELrrnQ               397    400b 0
     DstrCommand       s            255
     D/copy wPrEmplr

     c* Entry of Program.
     c*
     c     *Entry        Plist
     c                   Parm                    co#               3 0
     c                   Parm                    fnd               3 0
     c                   Parm                    emp#              9 0
     C                   Parm                    div               5 0
     C                   Parm                    env               1
     C                   Parm                    rrn               9 0

      /Free
       Select;
       When Env = 'W';
          strCommand = 'OVRDBF FILE(WDELLQ) TOFILE(' +
           %trim(RtvQuoteLib('*QTE')) + '/WDELLQ)';
          Callp QCMDEXC(strCommand:%len(strCommand));
          Open WDELLQ;
          Clear ELrrnQ;
          Setgt (co#:Fnd:Emp#:Div) pel00q;
          Readpe (co#:Fnd:Emp#:Div) pel00q;
          RRN = ELrrnQ;
          //*** beg add ***** 12/07/22 *********************
          strCommand = 'DLTOVR FILE(WDELLQ)';
          Callp QCMDEXC(strCommand:%len(strCommand));
          Close WDELLQ;
          //*** end add ***** 12/07/22 *********************
       When Env = 'Q';
          strCommand = 'OVRDBF FILE(WDELL) TOFILE(' +
           %trim(RtvQuoteLib('*LIV')) + '/WDELL)';
          Callp QCMDEXC(strCommand:%len(strCommand));
          Open WDELL;
          Clear ELrrn;
          Setgt (Co#:Fnd:Emp#:Div) pel00;
          Readpe (Co#:Fnd:Emp#:Div) pel00;
          RRN = ELrrn;
          //*** beg add ***** 12/07/22 *********************
          strCommand = 'DLTOVR FILE(WDELL)';
          Callp QCMDEXC(strCommand:%len(strCommand));
          Close WDELL;
          //*** end add ***** 12/07/22 *********************
       Endsl;

       *inLr = *On;
       Return;

      /End-Free


