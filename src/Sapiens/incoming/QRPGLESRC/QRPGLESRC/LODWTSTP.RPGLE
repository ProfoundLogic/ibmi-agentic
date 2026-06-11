     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H bnddir('WTBNDDIR')

    H/TITLE LODSTSSP - Load default values for WTSTP
     H*********************************************************************
     H*  Created:  01/14/15    C:282   Project: WC2334
    H*********************************************************************
     H* REVISED:
     H*
     H* xx/xx/xx C:xxx PROJECT xxxxxx
     H*                xxxxx
    H*********************************************************************
     Fsmcop     if   e             disk    rename(pco01:pco0p)
     Fwtstl     uf a e           k disk    rename(pst01:pst0p)

     d #ofFiles        c                   9
     d fileDesc        s             16    dim(#ofFiles) ctdata perrcd(1)

     d intCo#          s              3  0
     d charState       s              2
     d intCode         s              5  0

     dfileInfo         ds
     d Seq                            7a
     d   blank01                      1a
     d St                                  like(STstate)
     d   blank02                      1a
     d StCd                           5a

     d i1              s              3  0

     d psds          esds                  extname(szq1p)

     d/copy sPrDate

      **************************************************************
      * KLIST
      **************************************************************
     c     keyWTSTL      klist
     c                   kfld                    intCo#
     c                   kfld                    charState
     c                   kfld                    intCode
      *====================================================================
      * Main
      *====================================================================
      /free
       // Reading the record from System company file (SMCOP)
       Setll  1  pco0p;
       Read pco0p;
       Dow Not %EOF(SMcop);
          Exsr  Process;
       Read pco0p;
       Enddo;
       *InLr = *On;
      /end-free
      *====================================================================
      * Process each company
      *====================================================================
      /free
       Begsr Process;

          intCo# = COco#;
          i1 = 1;
          Dow #ofFiles >= i1;
             fileInfo = fileDesc(i1);
             charState = St;
             intCode  =  %Int(StCd);

             // Skip if the record already exist.
             Chain keyWTSTL pst0p;
             If %Found(WTstl);
                i1 = i1 + 1;
                Iter;
             Else;
                // Write record in Satet Code file WTSTP from array.
                Exsr  WrRcdWTstp;
             EndIf;

          i1 = i1 + 1;
          EndDo;

       Endsr;
      /end-free
      *====================================================================
      * Write records to WTSTP
      *====================================================================
      /free
       Begsr WrRcdWTstp;

        Clear pst0p;

        STco#   = intCo#;
        STstate = charState;
        STcode  = intCode;
        STflag  = 'Y';
        STstat  = 'A';

        STipgm = q1pgm;
        STiusr = q1user;
        STidt  = DateTo7(SystemDate);
        STitm  = TimeTo6(SystemTime);
        STpgm  = q1pgm;
        STuser = q1user;
        STdt   = DateTo7(SystemDate);
        STtime = TimeTo6(SystemTime);

        Write  pst0p;

       Endsr;
      /end-free
      *====================================================================
** Seq#|St|StCd
     1  NJ 0932                                                                  ** Assessment
     2  NJ 9740                                                                  ** Foreign Terr.
     3  NJ 9741                                                                  ** Dosmestic Terr.
     4  NY 0932                                                                  ** Assessment
     5  NY 9740                                                                  ** Foreign Terr.
     6  NY 9741                                                                  ** Dosmestic Terr.
     7  PA 0938                                                                  ** Assessment
     8  PA 9740                                                                  ** Foreign Terr.
     9  PA 9741                                                                  ** Dosmestic Terr.
