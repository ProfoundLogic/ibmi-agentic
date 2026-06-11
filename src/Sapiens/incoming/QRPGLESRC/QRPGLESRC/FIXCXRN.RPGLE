     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H/TITLE FIXCXRN - Fix Blank Agent Number in Cancellation/Reinstatement file WDCXP.
     H DFTNAME(FIXCXRN)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*Caller)
     H BNDDIR('FIXCXRNBND')
     h**********************************************************************
    *===============================================================*
    *  Created   : 08/03/13                                         *
    *  Programmer: C:283                                            *
    *  Support   : WA102 81317                                      *
    *===============================================================*
    *  Revisions:                                                   *
    *                                                               *
    *===============================================================*

     fwdcxl     uf   e           k disk    rename(pcx01:pcx00)
     fwdxhl     uf   e           k disk    rename(pxh01:pxh00)

     d PSDS          ESDS                  extName(SZQ1P)

     d ##Co#           s              3
     d ##Fnd           s              3
     d ##Fyr           s              3
     d ##Emp#          s              9
     d ##Div           s              5
     d @@Agt#          s              5  0
     d @@Anam          s             60
     d @@Prod          s              5  0
     d @@Pnam          s             60

     d AgtAPI          pr                  extpgm('WA002R')
     d   ##Co#                        3  0
     d   ##Fnd                        3  0
     d   ##Emp#                       9  0
     d   ##Div                        5  0
     d   ##Fyr                        3  0
     d   @@Agt#                       5  0
     d   @@Anam                      60
     d   @@Prod                       5  0
     d   @@Pnam                      60


     d/Copy SPRDATE

      /free

       Setll *Loval PCX00;
       Read  PCX00;

       Dow Not %Eof(WDCXL);
          If CXagt# = *Zeros and CXpgm = 'WTPOLCXRN';
             Exsr AGTUPD;
          Endif;

          Read PCX00;
       Enddo;

       *InLr = *On;

       Begsr AGTUPD;

             @@Agt# = 0;
             @@Anam = ' ';
             @@Prod = 0;
             @@Pnam = ' ';
             AgtAPI(CXCo#:CXfnd:CXemp#:CXdiv:CXfyr:@@Agt#:
                     @@Anam:@@Prod:@@Pnam);
             CXagt# = @@Agt#;

             CXipgm = Q1pgm;
             CXiusr = Q1user;
             CXitm  = Timeto6(SystemTime());
             CXidt  = Dateto7(SystemDate());
             Update PCX00;

             Setgt  XHKY PXH00;
             ReadPE XHKY PXH00;

             Dow Not %Eof(WDXHL);
                If XHagt# = *Zeros;
                   Exsr HSTUPD;
                Endif;
                ReadPE XHKY PXH00;
             Enddo;
       Endsr;

       Begsr HSTUPD;
             XHagt# = @@Agt#;
             XHpgm  = Q1pgm;
             XHuser = Q1user;
             XHtime = Timeto6(SystemTime());
             XHdt   = Dateto7(SystemDate());
             Update PXH00;
       Endsr;

      /end-free

     c     *INzsr        Begsr

     c* Entry of Program.
     c*
     c* Key List's

      *  KEY LIST FOR THE CANCELLATION HISTORY FILE
      *
     c     XHKY          KLIST
     c                   Kfld                    CXco#
     c                   Kfld                    CXfnd
     c                   Kfld                    CXfyr
     c                   Kfld                    CXemp#
     c                   Kfld                    CXdiv
     c                   Kfld                    CXreq#

     c                   Endsr                                                  *INzsr

