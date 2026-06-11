     Hcopyright('(c) 2021 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
       //********************************************************************
       //TITLE WTPOLARD - Group Select - Populate Work File
       //********************************************************************
       //  Description: Submits jobs for specific funds              .      *
       //  Called from: WTPOLARDC                                           *
       //  Calls to: W7944c                                                 *
       //  Created: C:280  Project: 129WTM         11/25/14                 *
       //*************************************************************************************
       //  Revisions:                                                                        *
       //
       // 11/06/23  C:65   Project: WC2934 (Phw#115701)
       //                  Add Batch Type parameter and Pass Batch Type to
       //                  W7380C - pass blanks to include all record types.
       //
       // 12/29/21 C:136 Support: 99764
       //                Rewrite program to match spec's.
       //                Changes not commented.
       //
       // 06/03/15 C:280 PHASEWARE: 35149
       //                Change Parms for report
       //**************************************************************************************
     fwzdbp     if   e             disk
     fwdf1l1    if   e           k disk    rename(pf101:pf101)
       Dcl-Pi EntryProgram ExtPgm('WTPOLARD');
        inRunDate Char(8);
       End-Pi;
       Dcl-S ctGrp Char(3) Dim(#OfGrps) ctdata perrcd(1);
       Dcl-C #OfGrps 6;
       Dcl-S intRunDate7 Packed(7:0);
       Dcl-S idx Packed(3:0);
     D isoDate         s               d   datfmt(*iso)
     D                 DS
     D dbsdt                   1      8  0
     D sdt                     1      8
     d cltID           s              3
01   D ##co#           s              3    Inz('777')
 2   D ##fnd_fr        s              3    Inz('000')
 3   D ##fnd_to        s              3    Inz('000')
 4   D ##yr_fr         s              3    Inz('000')
 5   D ##yr_to         s              3    Inz('000')
 6   D ##ardt_fr       s              8
 7   D ##ardt_to       s              8
 8   D ##ena           s              1    Inz('Y')
 9   D ##sort          s              1    Inz('E')
10   D ##agt#          s              5    Inz('00000')
 1   D ##emp#          s              9    Inz('000000000')
 2   D ##div           s              5
 3   D ##plan          s              1    Inz('A')
 4   D ##ctyp          s             10
 5   D ##ar_code       s              1
 6   D ##arcd          s             60
 7   D ##report        s              1    Inz('D')
 8   D ##output        s              1    Inz('F')
 9   D ##outq          s             10
20   D ##arpt          s              1    Inz('N')
 1   D ##btyp          s              1
     D*** begin add ***  06/03/24  ******************************************
     D ##batchType     s             20    Inz(' ')
     D*** end add *****  06/03/24  ******************************************
 2   D ##cmnth         s              1
     DGetClientID      pr                  ExtPgm('RTVCLTID')
     D cltid                          3
     DCpyMthFile       pr                  ExtPgm('W7380C')
01   D ##co#                          3
 2   D ##fnd_fr                       3
 3   D ##fnd_to                       3
 4   D ##yr_fr                        3
 5   D ##yr_to                        3
 6   D ##ardt_fr                      8
 7   D ##ardt_to                      8
 8   D ##ena                          1
 9   D ##sort                         1
10   D ##agt#                         5
 1   D ##emp#                         9
 2   D ##div                          5
 3   D ##plan                         1
 4   D ##ctyp                        10
 5   D ##ar_code                      1
 6   D ##arcd                        60
 7   D ##report                       1
 8   D ##output                       1
 9   D ##outq                        10
20   D ##arpt                         1
 1   D ##btyp                         1
 2   D ##cmnth                        1
     D*** begin add ***  06/03/24  ******************************************
     D ##batchType                   20
     D*** end add *****  06/03/24  ******************************************
       //===============================================================
       // Main Line
       //===============================================================
       GetClientID(cltID);
       If cltID <> '129';
          *Inlr = *On;
          Return;
       Endif;
       Exsr ProgramStart;
       For idx = 1 to #OfGrps;
          ##fnd_fr = ctGrp(idx);
          ##fnd_to = ctGrp(idx);
          ##Yr_Fr =
           GetStartFyr(%Int(##Co#):%Int(ctGrp(idx)):intRunDate7);
          CpyMthFile(##co#:
           ##fnd_fr:
           ##fnd_to:
           ##yr_fr:
           ##yr_to:
           ##ardt_fr:
           ##ardt_to:
           ##ena:
           ##sort:
           ##agt#:
           ##emp#:
           ##div:
           ##plan:
           ##ctyp:
           ##ar_code:
           ##arcd:
           ##report:
           ##output:
           ##outq:
           ##arpt:
           ##btyp:
       //*** begin chg ***  06/03/24  ******************************************
       //*****    ##cmnth);
           ##cmnth:
           ##batchType);
       //*** end chg *****  06/03/24  ******************************************
       Endfor;
       *Inlr = *On;
       //==================================================
       Begsr ProgramStart;
       //==================================================
        If %Parms() > 0;
           ##ardt_to = inRunDate;
        Else;
           Read wzdbp;
           ##ardt_to = sdt;
        Endif;
        isoDate = %Date(##ardt_to:*Usa0);
        intRunDate7  = %Dec(isoDate:*Cymd);
        // Start Date of A/R Dtl Rpt is 11/01 of
        // the year 3 years prior to run date...
        isoDate = isoDate - %Years(3);
        ##ardt_fr = %Char(isodate:*Usa0);
        %Subst(##ardt_fr : 1 : 2) = '11';
        %Subst(##ardt_fr : 3 : 2) = '01';
       Endsr;
        //==================================================
       Dcl-Proc GetStartFyr;
       //==================================================
        Dcl-Pi *N Char(3);
         inCo# Packed(7:0) Const;
         inFnd Packed(3:0) Const;
         inRunDate Packed(7:0) Const;
        End-Pi;
        Setgt (inCo#:inFnd:inRunDate) pf101;
        Readpe (inCo#:inFnd) pf101;
        Return %Char(F1fyr - 2);
       End-Proc;
** Specified Groups
014
018
019
020
021
030
