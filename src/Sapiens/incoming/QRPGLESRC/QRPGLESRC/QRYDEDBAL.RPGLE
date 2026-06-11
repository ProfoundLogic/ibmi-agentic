     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/title QRYDEDBAL - Reports policies with deductible imbalances
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
      *==========================================================
      * You can't call this pgm directly. You must call QRYDEDBALC.
      * Accepts parms co/date/fnd/emp/div/fyr and writes out
      *  following errors if encountered.
      *  For both deductible plans:
      *  - Deductible amount billed > Payment Total by reserve type
      *     if not flagged for total incurred
      *  - Deductible amount billed > Total Incurred by reserve type
      *     if flagged for total incurred
      *  - Payments <> Claims Paid Out amt by reserve type
      *  - Reserve History <> Claims RR + Claims AR by reserve type
      *  - Deductible billed in A/R and Deductible Detail not equal
      *  For net deductible plans the following are added:
      *  - Amount recovered in deductible detail <> Payment Recoveries
      *     by reserve type
      *  - Post Net Recoveries <> Deductible Cash in A/R
      *  - Deductible Amt Rcvrd > Payment Dtl Recoveries by rsv type
      *  - Payments - Recoveries <> Claims Paid Out by reserve type
      *  - Amount to recover in Post net <> Deductible Dtl Pended Amt
      *==========================================================
      * co/fnd/fyr/case/seq
     fwdpal     if   e           k disk    rename(ppa01:ppa00)
      * co/fnd/fyr/case
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
      * co/fnd/emp/div/fyr/case
     fwmcml     if   e           k disk    rename(pcm01:pcm00)
      * co/fnd/fyr/case/bfyr/podt/sort
     fwdddl1    if   e           k disk
      * co/fnd/emp/div/fyr
     fwdell1    if   e           k disk
      * co/fnd/emp/div/fyr
     fwmldl1    if   e           k disk
      * co/fnd/fyr/case
     fwmrhl1    if   e           k disk
      * co/fnd/st/code/efdt
     fwtddl2    if   e           k disk    rename(pdd01:pdd02)
      * co/fnd/emp/div/fyr/trdt
     fwmeal     if   e           k disk    rename(pea01:pea00)
     fwadql     if   e           k disk    rename(pdq01:pdq00)
     fqrydedbalfo    e             disk

     d*entry
     d QryDedBal       pr
     d  @@co#                         3
     d  @@fnd                         3
     d  @@emp#                        9
     d  @@div                         5
     d  @@fyr                         3
     d QryDedBal       pi
     d  @@co#                         3
     d  @@fnd                         3
     d  @@emp#                        9
     d  @@div                         5
     d  @@fyr                         3
      *======================================================
      *prototypes
      *======================================================
      /copy SprDate
      /copy WprEmplr
     d ChkStack        pr                  extpgm('SRCHKPGM')
     d  qqPgmName                    10    const
     d  qqFound                       1
     d GetDedBuckets   pr
     d GetDedType      pr             1
     d GetPayBuckets   pr
     d GetAR           pr
     d GetDQ           pr
     d GetValue        pr                  like(PAamt)
     d  type                               like(PAtype)
     d  amount                             like(PAamt)
     d PopErrorFile    pr
     d WrtErrors       pr
     d  dedType                       1
      *======================================================
      * Stand Alone
      *======================================================
     d clmCmpPay       s                   like(PAamt)
     d clmExpPay       s                   like(PAamt)
     d clmLegPay       s                   like(PAamt)
     d clmMedPay       s                   like(PAamt)
     d clmCmpRcv       s                   like(PAamt)
     d clmExpRcv       s                   like(PAamt)
     d clmLegRcv       s                   like(PAamt)
     d clmMedRcv       s                   like(PAamt)
     d dedCmpBil       s                   like(DDamtb)
     d dedCmpPnd       s                   like(DDamtp)
     d dedCmpRcv       s                   like(DDamtr)
     d dedMedBil       s                   like(DDamtb)
     d dedMedPnd       s                   like(DDamtp)
     d dedMedRcv       s                   like(DDamtr)
     d dedExpBil       s                   like(DDamtb)
     d dedExpPnd       s                   like(DDamtp)
     d dedExpRcv       s                   like(DDamtr)
     d dedLegBil       s                   like(DDamtb)
     d dedLegPnd       s                   like(DDamtp)
     d dedLegRcv       s                   like(DDamtr)
     d flgIncurd       s              1
     d totDedBil       s             +2    like(DDamtb)
     d totDedPnd       s             +2    like(DDamtp)
     d totDedRcv       s             +2    like(DDamtr)
     d qqFound         s              1
     d processRcd      s               n
     d svFnd           s                   like(PAfnd)
     d totARBild       s             +2    like(EAamt)
     d totARCash       s             +2    like(EAamt)
     d totDQamt        s             +2    like(DQamt)
     d totPaymnt       s             +2    like(DQamt)
     d xxTrDt          s                   like(PAtrdt)
     d xxCo#           s                   like(PAco#)
     d xxDedType       s              1
     d xxFnd           s                   like(PAfnd)
     d xxEmp#          s                   like(CMemp#)
     d xxDiv           s                   like(CMdiv)
     d xxFyr           s                   like(PAfyr)
     d xxAmt           s                   like(PAamt)
     d xxRsvCode       s                   like(PArecd)
      *======================================================
      * data structures/arrays
      *======================================================
     d                 ds
     d  cmrt                          2    dim(10)
     d  CMrt01                 1      2
     d  CMrt02                 3      4
     d  CMrt03                 5      6
     d  CMrt04                 7      8
     d  CMrt05                 9     10
     d  CMrt06                11     12
     d  CMrt07                13     14
     d  CMrt08                15     16
     d  CMrt09                17     18
     d  CMrt10                19     20

     d                 ds
     d  dbrt                          2    dim(10)
     d  DBrt01                 1      2
     d  DBrt02                 3      4
     d  DBrt03                 5      6
     d  DBrt04                 7      8
     d  DBrt05                 9     10
     d  DBrt06                11     12
     d  DBrt07                13     14
     d  DBrt08                15     16
     d  DBrt09                17     18
     d  DBrt10                19     20

     d                 ds
     d  fndc                          2    dim(10)
     d  FNrs01                 1      2
     d  FNrs02                 3      4
     d  FNrs03                 5      6
     d  FNrs04                 7      8
     d  FNrs05                 9     10
     d  FNrs06                11     12
     d  FNrs07                13     14
     d  FNrs08                15     16
     d  FNrs09                17     18
     d  FNrs10                19     20

     d                 ds
     d  cmar                          9  2 dim(10)
     d  CMar01                 1      9  2
     d  CMar02                10     18  2
     d  CMar03                19     27  2
     d  CMar04                28     36  2
     d  CMar05                37     45  2
     d  CMar06                46     54  2
     d  CMar07                55     63  2
     d  CMar08                64     72  2
     d  CMar09                73     81  2
     d  CMar10                82     90  2

     d                 ds
     d  cmrr                          9  2 dim(10)
     d  CMrr01                 1      9  2
     d  CMrr02                10     18  2
     d  CMrr03                19     27  2
     d  CMrr04                28     36  2
     d  CMrr05                37     45  2
     d  CMrr06                46     54  2
     d  CMrr07                55     63  2
     d  CMrr08                64     72  2
     d  CMrr09                73     81  2
     d  CMrr10                82     90  2

     d                 ds
     d  dbar                         11  2 dim(10)
     d  DBar01                 1     11  2
     d  DBar02                12     22  2
     d  DBar03                23     33  2
     d  DBar04                34     44  2
     d  DBar05                45     55  2
     d  DBar06                56     66  2
     d  DBar07                67     77  2
     d  DBar08                78     88  2
     d  DBar09                89     99  2
     d  DBar10               100    110  2

     d                 ds
     d  dbrr                         11  2 dim(10)
     d  DBrr01                 1     11  2
     d  DBrr02                12     22  2
     d  DBrr03                23     33  2
     d  DBrr04                34     44  2
     d  DBrr05                45     55  2
     d  DBrr06                56     66  2
     d  DBrr07                67     77  2
     d  DBrr08                78     88  2
     d  DBrr09                89     99  2
     d  DBrr10               100    110  2

     d                 ds
     d  dbrh                         11  2 dim(10)
     d  DBrh01                 1     11  2
     d  DBrh02                12     22  2
     d  DBrh03                23     33  2
     d  DBrh04                34     44  2
     d  DBrh05                45     55  2
     d  DBrh06                56     66  2
     d  DBrh07                67     77  2
     d  DBrh08                78     88  2
     d  DBrh09                89     99  2
     d  DBrh10               100    110  2

     d                 ds
     d  rhrt                          2    dim(10)
     d  RHrt01                 1      2
     d  RHrt02                 3      4
     d  RHrt03                 5      6
     d  RHrt04                 7      8
     d  RHrt05                 9     10
     d  RHrt06                11     12
     d  RHrt07                13     14
     d  RHrt08                15     16
     d  RHrt09                17     18
     d  RHrt10                19     20

     d                 ds
     d  rhsu                          9  2 dim(10)
     d  RHsu01                 1      9  2
     d  RHsu02                10     18  2
     d  RHsu03                19     27  2
     d  RHsu04                28     36  2
     d  RHsu05                37     45  2
     d  RHsu06                46     54  2
     d  RHsu07                55     63  2
     d  RHsu08                64     72  2
     d  RHsu09                73     81  2
     d  RHsu10                82     90  2

     d                 ds
     d  dbpa                         11  2 dim(10)
     d  DBpa01                 1     11  2
     d  DBpa02                12     22  2
     d  DBpa03                23     33  2
     d  DBpa04                34     44  2
     d  DBpa05                45     55  2
     d  DBpa06                56     66  2
     d  DBpa07                67     77  2
     d  DBpa08                78     88  2
     d  DBpa09                89     99  2
     d  DBpa10               100    110  2

     d                 ds
     d  dbpr                         11  2 dim(10)
     d  DBpr01                 1     11  2
     d  DBpr02                12     22  2
     d  DBpr03                23     33  2
     d  DBpr04                34     44  2
     d  DBpr05                45     55  2
     d  DBpr06                56     66  2
     d  DBpr07                67     77  2
     d  DBpr08                78     88  2
     d  DBpr09                89     99  2
     d  DBpr10               100    110  2

     d                 ds
     d  dbdb                         11  2 dim(10)
     d  DBdb01                 1     11  2
     d  DBdb02                12     22  2
     d  DBdb03                23     33  2
     d  DBdb04                34     44  2
     d  DBdb05                45     55  2
     d  DBdb06                56     66  2
     d  DBdb07                67     77  2
     d  DBdb08                78     88  2
     d  DBdb09                89     99  2
     d  DBdb10               100    110  2

     d                 ds
     d  dbdr                         11  2 dim(10)
     d  DBdr01                 1     11  2
     d  DBdr02                12     22  2
     d  DBdr03                23     33  2
     d  DBdr04                34     44  2
     d  DBdr05                45     55  2
     d  DBdr06                56     66  2
     d  DBdr07                67     77  2
     d  DBdr08                78     88  2
     d  DBdr09                89     99  2
     d  DBdr10               100    110  2

     d                 ds
     d  dbdp                         11  2 dim(10)
     d  DBdp01                 1     11  2
     d  DBdp02                12     22  2
     d  DBdp03                23     33  2
     d  DBdp04                34     44  2
     d  DBdp05                45     55  2
     d  DBdp06                56     66  2
     d  DBdp07                67     77  2
     d  DBdp08                78     88  2
     d  DBdp09                89     99  2
     d  DBdp10               100    110  2

     d                 ds
     d  ddp                    1     44  2
     d                                     dim(4)
     d  totCmpDdP              1     11  2
     d  totMedDdP             12     22  2
     d  totExpDdP             23     33  2
     d  totLegDdP             34     44  2

     d                 ds
     d  mst                    1     44  2
     d                                     dim(4)
     d  totMstCmp              1     11  2
     d  totMstMed             12     22  2
     d  totMstExp             23     33  2
     d  totMstLeg             34     44  2

     d                 ds
     d  rem                    1     44  2
     d                                     dim(4)
     d  totMstCmR              1     11  2
     d  totMstMeR             12     22  2
     d  totMstExR             23     33  2
     d  totMstLeR             34     44  2

     d psds          esds                  extname(SZQ1P)

      /free
       ChkStack('QRYDEDBALC':qqFound);
       If qqFound <> '1';
          *inlr = *on;
          Return;
       Endif;
       xxCo#  = %int(@@co#);
       If @@fnd <> *blank;
          xxFnd  = %int(@@fnd);
       Endif;
       If @@emp# <> *blank;
          xxEmp# = %int(@@emp#);
       Endif;
       If @@div <> *blank;
          xxDiv  = %int(@@div);
       Endif;
       If @@fyr <> *blank;
          xxFyr  = %int(@@fyr);
       Endif;

       Select;
       When @@div <> *blank;
          Setll (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv) pel01;
          Reade (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv) pel01;
       When xxEmp# <> 0;
          Setll (xxCo#:xxFnd:xxFyr:xxEmp#) pel01;
          Reade (xxCo#:xxFnd:xxFyr:xxEmp#) pel01;
       When xxFyr <> 0;
          Setll (xxCo#:xxFnd:xxFyr) pel01;
          Reade (xxCo#:xxFnd:xxFyr) pel01;
       When xxFnd <> 0;
          Setll (xxCo#:xxFnd) pel01;
          Reade (xxCo#:xxFnd) pel01;
       Other;
          Setll (xxCo#) pel01;
          Reade (xxCo#) pel01;
       Endsl;
       Dow not(%eof);
          If svFnd <> ELfnd;
             svFnd  = ELfnd;
             Chain (ELco#:ELfnd) pfn00;
             DBrt   = FNdc;
          Endif;
          xxDedType = GetDedType();
          If xxDedType <> *blank;
             WrtErrors(xxDedType);
          Endif;
          Select;
          When @@div <> *blank;
             Reade (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv) pel01;
          When xxEmp# <> 0;
             Reade (xxCo#:xxFnd:xxFyr:xxEmp#) pel01;
          When xxFyr <> 0;
             Reade (xxCo#:xxFnd:xxFyr) pel01;
          When xxFnd <> 0;
             Reade (xxCo#:xxFnd) pel01;
          Other;
             Reade (xxCo#) pel01;
          Endsl;
       Enddo;
       *Inlr = *On;
       Return;
      /end-free
      *=====================================================
     p GetDedType      b
     d GetDedType      pi             1
     d state           s              2
      /free
       If ELdcde <> *blank;
          state = GetRatingState(ELco#:ELfnd:Elemp#:ELdiv:ELfyr);
          Setgt (ELco#:ELfnd:state:ELdcde:ELpfdt) pdd02;
          Readpe (ELco#:ELfnd:state:ELdcde) pdd02;
          Select;
          When %Eof = *on;
             Return *blank;
          Other;
             Return DDtype; // Either 'G' or 'N' for Gross or Net
             flgIncurd =  DDtoti;
          Endsl;
       Else;
          Chain (ELco#:ELfnd:Elemp#:ELdiv:ELfyr) pld01;
          Select;
          When not(%found);
             Return *blank;
          Other;
             Return LDtyp1; //Either 'G' or 'N' for Gross or Net
             flgIncurd =  LDtot1;
          Endsl;
       Endif;
      /end-free
     p GetDedType      e
      *====================================================
     p GetAr           b
     d GetAr           pi
      /free
       // OPNQRY statement filters A/R for deductible ctgy
       DBarBl = 0;
       DBarPd = 0;
       Setll (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pea00;
       Reade (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pea00;
       Dow not (%eof);
          Select;
          When EAarty  = 'F';
             DBarBl += EAamt;
          When EAarty = 'Q';
             DBarPd += EAamt;
          Endsl;
       Reade (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pea00;
       Enddo;
      /end-free
     p GetAR           e
      *==================================================
     p GetDQ           b
      /free
       DBdqAm = 0;
       DBdqRm = 0;
       Setll (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pdq00;
       Reade (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pdq00;
       Dow not (%eof);
          DBdqAm += DQamt;
          DBdqRm += DQamtr;
       Reade (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pdq00;
       Enddo;
      /end-free
     p GetDQ           e
      *==================================================
     p GetDedBuckets   b
     d GetDedBuckets   pi
     d y               s              2  0
      /free
       Clear dbdb;
       Clear dbdr;
       Clear dbdp;
       Setll (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pdd01;
       Reade (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pdd01;
       Dow Not(%Eof);
          y = %lookup(DDrecd:dbrt);
          If y <> 0;
             dbdb(y) += DDAmtB;
             dbdr(y) += DDAmtR;
             dbdp(y) += DDAmtP;
          Endif;
       Reade (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pdd01;
       Enddo;
      /end-free
     p GetDedBuckets   e
      *==================================================
     p GetPayBuckets   b
     d GetPayBuckets   pi
     d x               s              2  0
     d y               s              2  0
      /free
       Clear DBar;
       Clear DBrr;
       Clear DBpa;
       Clear DBpr;
       Clear DBrh;
       Eval  DBar = 0;
       Eval  DBrr = 0;
       Eval  DBpa = 0;
       Eval  DBpr = 0;
       Eval  DBrh = 0;
       Setll (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pcm00;
       Reade (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pcm00;
       Dow Not(%Eof);
          For x = 1 to 10;
             y = %lookup(cmrt(x):dbrt);
             If y <> 0 and cmrt(x) <> *blanks;
                DBar(y) += CMar(x);
                DBrr(y) += CMrr(x);
             Endif;
          Endfor;
          Setll (CMco#:CMfnd:CMfyr:CMcase) ppa00;
          Reade (CMco#:CMfnd:CMfyr:CMcase) ppa00;
          Dow Not(%Eof);
             y = %lookup(PArecd:dbrt);
             Select;
             When y = 0;
             When Pabr2 = FNcop1 or
              Pabr2 = FNmep1 or
              Pabr2 = FNlep1 or
              Pabr2 = FNexp1;
                dbpr(y) -= GetValue(PAtype:PAamt);
             Other;
                dbpa(y) += GetValue(PAtype:PAamt);
             Endsl;
          Reade (CMco#:CMfnd:CMfyr:CMcase) ppa00;
          Enddo;
          Setll (CMco#:CMfnd:CMfyr:CMcase) prh01;
          Reade (CMco#:CMfnd:CMfyr:CMcase) prh01;
          Dow Not(%Eof);
             For x = 1 to 10;
                y = %lookup(rhrt(x):dbrt);
                If y <> 0 and rhrt(x) <> *blanks;
                   dbrh(y) += rhsu(x);
                Endif;
             Endfor;
          Reade (CMco#:CMfnd:CMfyr:CMcase) prh01;
          Enddo;
       Reade (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pcm00;
       Enddo;
      /end-free
     p GetPayBuckets   e
      *==================================================
     p GetValue        b
     d GetValue        pi                  like(PAamt)
     d  type                               like(PAtype)
     d  amount                             like(PAamt)
      /free
       If type = 'R' or type = 'S' or type = 'V';
          Return -amount;
       Else;
          Return amount;
       Endif;
      /end-free
     p GetValue        e
      *==================================================
     p PopErrorFile    b
      /free
       DBco#  = ELco#;
       DBfnd  = ELfnd;
       DBemp# = ELemp#;
       DBdiv  = ELdiv;
       DBfyr  = ELfyr;
       DBtype = xxDedType;
       DBtoti = flgIncurd;
       DBuser = Q1user;
       DBpgm  = Q1pgm;
       DBdt   = Dateto7(SystemDate());
       DBtime = Timeto6(SystemTime());
       DBrt   = FNdc;
       Return;
      /end-free
     p PopErrorFile    e
      *=====================================================
     p WrtErrors       b
     d WrtErrors       pi
     d  dedType                       1
     d x               s              2  0
     d y               s              2  0
      /free
       PopErrorFile();
       GetPayBuckets();
       GetDedBuckets();
       GetAR();
       GetDQ();
       For x = 1 to 10;
        // AR + RR = Reserve History
          If dbar(x) + dbrr(x) <> dbrh(x);
             DBerr = dbrt(x) + ' total incurred and reserve history disagree.';
             Write pdbq01;
          Endif;
        // Ded billed too high
          Select;
          When DBtoti = ' ' and (dbdb(x) > dbpa(x));
             DBerr = dbrt(x) + ' ded billed > payments.';
             Write pdbq01;
          When DBtoti = 'Y' and (dbdb(x) > (dbar(x) + dbrr(x)));
             DBerr = dbrt(x) + ' ded billed > total incurred';
             Write pdbq01;
          Endsl;
        // Payments - Recoveries <> CMAR
          If dbpa(x) - dbpr(x) <> dbar(x);
             DBerr = dbrt(x) + ' payments <> claims master paid out';
             Write pdbq01;
          Endif;
        // CMAR < 0
          If cmar(x) < 0;
             DBerr = dbrt(x) + ' claims master paid out < 0';
             Write pdbq01;
          Endif;
        // Rsv Hist < 0
          If dbrh(x) < 0;
             DBerr = dbrt(x) + ' rsv history < 0';
             Write pdbq01;
          Endif;
        // Next set of edits for Net Deductible
          If DBtype = 'G';
             Iter;
          Endif;
        // Ded Dtl Recovered <> Payment Recoveries
          If DBdr(x) <> dbpr(x);
             DBerr = dbrt(x) + ' ded dtl recovered <> payments recovered.';
             Write pdbq01;
          Endif;
        // Payment Recovery sum can't be negative
          If DBpr(x) < 0;
             DBerr = dbrt(x) + ' payments recovered < 0';
             Write pdbq01;
          Endif;
        // Ded Dtl Billed <> Ded Dtl Recovered + Pending
          If DBdb(x) <> (DBdr(x) + DBdp(x));
             DBerr = dbrt(x) + ' ded dtl bill <> ded dtl rcvr + pending.';
             Write pdbq01;
          Endif;
       Endfor;
        // Total ded dtl billed <> A/R ded billed
       If %xfoot(dbdb) <> DBarbl;
          DBerr = 'Ded Dtl billed <> A/R billed';
          Write pdbq01;
       Endif;
        // Total post net chk amt <> A/R ded cash with rem amt <> 0
       If DBtype = 'N' and DBdqam <> DBarpd;
          DBerr = 'Post net check amt <> ded cash in A/R';
          Write pdbq01;
       Endif;
        // Total payment recoveries > A/R ded cash
       If DBtype = 'N' and %xfoot(dbpr) > DBarpd;
          DBerr = 'Payment recoveries > ded cash in A/R';
          Write pdbq01;
       Endif;
        // Total post net remaining < 0
       If DBtype = 'N' and DBdqrm < 0;
          DBerr = 'Post net remaining amount < 0';
          Write pdbq01;
       Endif;
      /end-free
     p WrtErrors       e
