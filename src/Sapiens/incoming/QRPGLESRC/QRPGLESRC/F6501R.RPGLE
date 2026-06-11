     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT

      ******************************************************************
      *   F6501R - Upload PerformRx Bill Transfer Program 1
      *
      ******************************************************************
      *   Description - Upload PerformRx Bill Transfer                 *
      *                                                                *
      *   Called from: F6500C                                          *
      *   Calls to   :                                                 *
      *                                                                *
      ******************************************************************
      *   Created on 04/12/2016 by c:212 for Project 114MXO            *
      ******************************************************************
      *   Revisions:                                                   *
      *                                                                *
      *                                                                *
      ******************************************************************
     Ffak0p     uf   e             disk
     Ffak1l1    o    e           k disk
     Ffak1l3    if   e           k disk    rename (pk101:pk103)
     Ffak9l1    uf   e           k disk
     Ffak9l2    if   e           k disk    rename (pk901:pk902)

      ** procedure definition
     d*
     d F6502R          pr                  ExtPgm('F6502R')
     d ##mbr                         10
     d ##bch                          7

     d ComAPI          pr                  ExtPgm('FA007R')
     d ##CO#                          3  0
     d ##FND                          3  0
     d ##CONM                        40
     d ##FNAM                        40
     d ##SUB#                         5  0
     d ##FYR                          3  0
     d*

      ** copybook declaration
      /copy SCOPYWW01D

      ** data structures declaration
     d*
     d detail_ds     e ds                  extname(FAK1P)

      ** standalone declaration
     d ##rrn           s              9  0 inz
     d ##bch           s              7    inz
     d xxbch#          s              7    inz
     d xxrectyp        s              1    inz
     d xxcnt           s              5s 0 inz
     d xxtcnt          s              5s 0 inz
     d xxcnttyp5       s              5s 0 inz
     d xxdup           s              5s 0 inz
     d IndDupRun       s               n   inz(*off)
     d ##CO#           s              3  0
     d ##FND           s              3  0
     d ##CONM          s             40
     d ##FNAM          s             40
     d ##SUB#          s              5  0
     d ##FYR           s              3  0

      ** PLIST definitions

     d F6501R          pr                  Extpgm('F6501R')
     d  @@MBR#                       10

     d F6501R          pi
     d  @@MBR#                       10

     c     k1dupky       klist
     c                   kfld                    K1RX#
     c                   kfld                    K1BCH#
     c                   kfld                    K1FDAT
     c                   kfld                    K1PAID
     c                   kfld                    K1AMTADJ
     c                   kfld                    K1ADJDT
     c                   kfld                    K1INCOSPD
     c                   kfld                    K1DISPF
     c                   kfld                    K1CICS
     c                   kfld                    K1CLMTYP
     c                   kfld                    K1DTPD
     c                   kfld                    K1CRN
     c                   kfld                    K1PATID
     c                   kfld                    K1REFUSED
     c                   kfld                    K1NDC

      *------------------------------------------*
      *  Main Processing                         *
      *------------------------------------------*
      /Free

         ComAPI(##CO#:##FND:##CONM:##FNAM:##SUB#:##FYR);

         If @@mbr# <> *blanks;
           // check if duplicate record
           indduprun = *off;
           xxcnttyp5 = *zeros;
           xxtcnt = *zeros;
           Read pk001;
           Dow not %eof (fak0p);
             xxrectyp = %subst(k0data:1:1);

             select;
              when xxrectyp = '1';
               xxbch# = %subst(k0data:12:7);
              when xxrectyp = '3';
               xxbch# = %subst(k0data:12:7);
              when xxrectyp = '5';
               xxbch# = %subst(k0data:10:7);
              when xxrectyp = '7';
               xxbch# = %subst(k0data:17:7);
             endsl;

             if xxrectyp = '1';
               // check if same batch exists in FAK9P
               setll xxbch# pk902;
               if %equal (fak9l2);
                 indduprun = *on;
               endIf;
             endif;

             xxtcnt += 1;
             if xxrectyp = '5';
               xxcnttyp5 += 1;
             endif;
             read pk001;
           Enddo;

           If indduprun = *on;
             // update fak9p
             exsr updat9;
           Else;
             xxcnt = *zeros;
             xxdup = *zeros;
             // read fak0p records
             Setll *Start fak0p;
             Read pk001;

             Dow not %eof (fak0p);
               xxrectyp = %subst(k0data:1:1);
               xxbch# = %subst(k0data:10:7);

               select;
                when xxrectyp = '5';
                  // write to fak1p
                  exsr writer;
                other;
               endsl;

               // update record process flag
               k0prfl = 'Y';
               update pk001;
               read pk001;
             Enddo;

             // update fak9p
             exsr updat9;

             // update system files
             ##bch = k9bch#;
             ##rrn = *zeros;
             callp F6502R (@@mbr# : ##bch);

           EndIf;
         EndIf;

         *inlr = *on;
       //================================================================
       // WRITER - PerformRx pharmacy payment pile (FAK1P).
       //================================================================
       begsr writer;

         clear pk101;

         xxcnt += 1;

         detail_ds = k0data;

         Setll k1dupky pk103;
         if %equal(fak1l3);
           xxdup += 1;
         else;
           k1mbr# = @@mbr#;
           k1uldt = %dec(%char(%date():*cymd0):7:0);
           k1ultm = %dec(%char(%time():*iso0):6:0);
           k1prdt = k1uldt;
           k1prtm = %dec(%char(%time():*iso0):6:0);
           k1prus = q1user;

           Monitor;
           If %dec(%char(K1CO#):3:0)= 0;
             K1CO#  = *Zeros;
           EndIf;
           On-Error;
             K1CO#  = *Zeros;
           EndMon;

           Monitor;
           If %dec(%char(K1SUB#):5:0)= 0;
             K1SUB# = *Zeros;
           EndIf;
           On-Error;
             K1SUB# = *Zeros;
           EndMon;

           Monitor;
           If %dec(%char(K1FND):3:0)= 0;
             K1FND  = *Zeros;
           EndIf;
           On-Error;
             K1FND  = *Zeros;
           EndMon;

           Monitor;
           If %dec(%char(K1FYR):3:0)= 0;
             K1FYR  = *Zeros;
           EndIf;
           On-Error;
             K1FYR  = *Zeros;
           EndMon;

           Monitor;
           If %dec(%char(K1CASE):7:0)= 0;
             K1CASE = *Zeros;
           EndIf;
           On-Error;
             K1CASE = *Zeros;
           EndMon;

           Monitor;
           If %dec(%char(K1ACDT):7:0)= 0;
             K1ACDT = *Zeros;
           EndIf;
           On-Error;
             K1ACDT = *Zeros;
           EndMon;

           Monitor;
           If %dec(%char(K1PFEI):9:0)= 0;
             K1PFEI = *Zeros;
           EndIf;
           On-Error;
             K1PFEI = *Zeros;
           EndMon;

           Monitor;
           If %dec(%char(K1MOD):3:0)= 0;
             K1MOD  = *Zeros;
           EndIf;
           On-Error;
             K1MOD  = *Zeros;
           EndMon;

           Monitor;
           If %dec(%char(K1EOB#):9:0)= 0;
             K1EOB# = *Zeros;
           EndIf;
           On-Error;
             K1EOB# = *Zeros;
           EndMon;

           Monitor;
           If %dec(%char(K1RCN):3:0)= 0;
             K1RCN  = *Zeros;
           EndIf;
           On-Error;
             K1RCN  = *Zeros;
           EndMon;

           Monitor;
           If %dec(%char(K1DLTM):6:0)= 0;
             K1DLTM = *Zeros;
           EndIf;
           On-Error;
             K1DLTM = *Zeros;
           EndMon;

           Monitor;
           If %dec(%char(K1DLDT):7:0)= 0;
             K1DLDT = *Zeros;
           EndIf;
           On-Error;
             K1DLDT = *Zeros;
           EndMon;

           write pk101;
         endif;

       endsr;

       //================================================================
       // UPDAT9 - Update PerformRx pharmacy file (FAK9P).
       //================================================================
       begsr updat9;

         Chain @@mbr# pk901;
         If %found (fak9l1);
           If indduprun = *on;
             k9bch# = k1bch#;
             k9cnt = xxcnttyp5;
             k9dup = xxcnttyp5;
           Else;
             k9recid = '1';
             k9prc# = '0000002222';
             k9bch# = k1bch#;
             k9fill1 = *blanks;
             k9cnt = xxcnt;
             k9trct = *zeros;
             k9dup = xxdup;
             k9err = *zeros;
           EndIf;

           k9uldt = %dec(%char(%date():*cymd0):7:0);
           k9ultm = %dec(%char(%time():*iso0):6:0);
           k9user = q1user;
           k9pgm = q1pgm;
           k9dt = %dec(%char(%date():*cymd0):7:0);
           k9time = %dec(%char(%time():*iso0):6:0);
           k9prdt = %dec(%char(%date():*cymd0):7:0);
           k9prtm = %dec(%char(%time():*iso0):6:0);
           k9iusr = q1user;
           k9ipgm = q1pgm;
           k9idt = k9dt;
           k9itim = k9time;

           update pk901;
           xxcnt = *zeros;
         EndIf;
       endsr;
      /End-Free
