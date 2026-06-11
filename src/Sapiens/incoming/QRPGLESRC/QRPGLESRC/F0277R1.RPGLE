     H* (c) 2012 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H* PROGRAM: F0277R1
     H* DESCRIPTION: WORK WITH PROVIDERS/SPECIALISTS
     H****************************************************************
     H* INDICATORS:                                                  *
     H****************************************************************
     H* REVISIONS:                                                   *
     H*                                                              *
     H*                                                           *
     H****************************************************************
     Ff0277f1   cf   e             workstn
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     Ffappp     if   e             disk
     F                                     rename(ppp01:pfrcdf)
     F*  Primary logical file definition (for displaying items)
     Ffappl9    if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(ppp01:dbrcdf)
     Ffmcmp     if   e             disk
     Ffmpdl3    if   e           k disk
     Ffmpdl1    if   e           k disk    rename(ppd01:ppd02)
      /COPY SCOPYWW01D
     d*
     d hd              s             78    dim(2) ctdata perrcd(1)              Column headings
     d of              s              1    dim(12) ctdata perrcd(1)             Options, Function
     d ofd             s             35    dim(12) alt(of)
     d*
     d                 ds
     d  x1stdt                        8  0
     d  X1stdd                 1      2
     d  X1stmm                 3      4
     d  X1styy                 5      8
     d*
     d                 ds
     d  x1endt                        8  0
     d  x1endd                 1      2
     d  x1enmm                 3      4
     d  x1enyy                 5      8
     d*
     d  xxbgdt         s              8  0
     d  xxendt         s              8  0
     d  ##rrn          s              9  0
     d  xxmode         s              1    inz('D')
     d inWCIF          s              1
     d inMCIF          s              1
     d inWCIN          s              1
     d inAR            s              1
     d inNTWK          s              1
     d inUR            s              1
     d inMED           s              1
     d inEDI           s              1
     d inWCDK          s              1
     d inRTCD          s              1
     d inCO#           s              3
     d inSUB#          s              5
     d inFND           s              3
     d inFYR           s              3
     d inCASE          s              7
     d outL1           s             78
     d outL2           s             78
     d inkc            s              1
     d inkl            s              1
     d inmode          s              1
     d inrrn           s              9  0
     d inWDCK          s              1
     d inKEY           s             40
     d infein          s              9
     d inmod           s              3
     d outrrn          s              9  0
     d*
     ddtCheck          PR             8  0
     d numDate8                       8  0 Const
     d dateType                       8    const options(*nopass)
     d*
     d RtnRrn          Pr                  ExtPgm('FA006R')
     d infein                         9    const
     d inmod                          3    const
     d outrrn                         9  0 const
     d*
     d PrvMntDsp       Pr                  ExtPgm('F0032R01')
     d inkc                           1    const
     d inkl                           1    const
     d inmode                         1    const
     d inrrn                          9  0 Const
     d*
     d BldDartKey      Pr                  ExtPgm('FA703R')
     d inRTCD                         1    const
     d inWCIN                         1    const
     d inWDCK                         1    const
     d inCO#                          3    const
     d inSUB#                         5    const
     d inFND                          3    const
     d inFYR                          3    const
     d inCASE                         7    const
     d inKEY                         40    const
     d*
     d RtvSysVal       Pr                  ExtPgm('SA001R')
     d inWCIF                         1    const
     d inMCIF                         1    const
     d inWCIN                         1    const
     d inAR                           1    const
     d inNTWK                         1    const
     d inUR                           1    const
     d inMED                          1    const
     d inEDI                          1    const
     d inWCDK                         1    const
     d*
     d CrtLinHeading   Pr                  ExtPgm('FA702R')
     d inRTCD                         1    const
     d inCO#                          3    const
     d inSUB#                         5    const
     d inFND                          3    const
     d inFYR                          3    const
     d inCASE                         7    const
     d outL1                         78    const
     d outL2                         78    const
     d*
     d*================================================================
     d*  /COPY required constants
     d*================================================================
     d optnm           c                   const('Case Providers')
     d slmsgi          c                   const('SYS0001')
     d slmsgf          c                   const('FSMESG')
     d acdpgm          c                   const('F0277R01')
     d comapi          c                   const('FA007R')
     d uspapi          c                   const('F0006R')
     d infomx          c                   const(1)
     d viewmx          c                   const(1)
     d sf1pag          c                   const(9)
     d*
     d* Program specific constants
     d*
     d dl              c                   const('* * * DELETED * * *')
     c*/COPY SCOPYWW01C
     c*================================================================
     c*  PLIST definitions
     c*================================================================
     c     *entry        plist
     c                   parm      *inkc         @@kc              1
     c                   parm      *inkl         @@kl              1
     c                   parm                    @@call            1
     c                   parm                    @@rrn             9 0          FAPPP
     c                   parm                    @@rrn2            9 0          FMCMP
     c                   parm                    @@mode            1
     c*
     c*  Add/change/display program parameter list
     c*
     c     acdpl         plist
     c     *inkc         parm      '0'           ##kc              1
     c     *inkl         parm      '0'           ##kl              1
     c                   parm                    ##mode            1
     c                   parm      scdbrr        ##rrn             9 0
     c                   parm      @@rrn2        ##rrn2            9 0
     c*
     c*  Company file/fund (group) retrieve program parameter list
     c*
     c     compl         plist
     c                   parm                    ##co#             3 0
     c                   parm                    ##fnd             3 0
     c                   parm                    ##conm           40
     c                   parm                    ##fnam           40
     c*
     c*================================================================
     c*  KLIST definitions
     c*================================================================
     c*
     c     key1          klist
     c                   kfld                    kkco#
     c                   kfld                    kksub#
     c                   kfld                    kkfnd
     c                   kfld                    kkfyr
     c                   kfld                    kkcase
     c                   kfld                    kkbgdt
     c                   kfld                    kkendt
     c*
     c     key2          klist
     c                   kfld                    kkco#
     c                   kfld                    kksub#
     c                   kfld                    kkfnd
     c                   kfld                    kkfyr
     c                   kfld                    kkcase
     c*
     c     pdky          klist
     c                   kfld                    kkfein
     c                   kfld                    kkmod
     c*
     c     pdky1         klist
     c                   kfld                    kkname
     c*
     c*================================================================
     c*  *LIKE DEFN statements
     c*================================================================
     c     *like         define    ppco#         kkco#
     c     *like         define    ppsub#        kksub#
     c     *like         define    ppfnd         kkfnd
     c     *like         define    ppfyr         kkfyr
     c     *like         define    ppcase        kkcase
     c     *like         define    ppfein        kkfein
     c     *like         define    ppmod         kkmod
     c     *like         define    ppbgdt        kkbgdt
     c     *like         define    ppendt        kkendt
     c     *like         define    pdname        kkname
     c*
     c/COPY SCOPYWW01C
     c*================================================================
        //CHKSSM - Set indicator 11 for subset message

      /free
       BegSr chkssm;

          If sssts <> *Blanks Or ssrstr <> *blanks;
            *in11 = *On;
          EndIf;
       EndSr;

        //ERRCK1 - Additional errors on subfile selection

       BegSr ErrCk1;
       EndSr;

        //ERRCK2 - Error checking for the subset screen

       BegSr ErrCk2;

         If *in90 = '1';
           leavesr;
         Endif;

        // CHECK ACTIVE/INACTIVE

         If sssts <> *blanks and sssts <> 'A'
            and sssts <> 'I' ;
            *In90 = *On;
            *In74 = *On;
            If *in90 = *On;
              leavesr;
            Endif;
         Endif;
          //CHECK RESTRICTIONS EQUAL TO Y/N

         If ssrstr <> *Blanks  and ssrstr <> 'Y'
           and ssrstr <> 'N';

           *In90 = *On;
           *In75 = *On;

             If *in90 = *On;
               leavesr;
             Endif;

         Endif;

       Endsr;

          //ERRCK3 - Used to determine errors on the third screen

       BegSr ErrCk3;
       EndSr;

        //ERRCK4 - Used to determine errors on the fourth screen

       BegSr ErrCk4;
       EndSr;

        //ERRCK5 - Used to determine errors on the fifth screen

       BegSr ErrCk5;
       EndSr;

        //DLTPRE - Used for any processing before delete occurs

       BegSr DltPre;
       EndSr;

        //UPDATR - Used to update data base record

       BegSr Updatr;
       EndSr;

        //WRITER - Used to write data base record

       BegSr Writer;
       EndSr;

        //DSPLYR - After displaying information

       BegSr Dsplyr;
       EndSr;

        //OTHERR - Used for processing other modes

       BegSr Otherr;
       EndSr;

        //PREOTH - Pre-processing for other modes

       BegSr PreOth;
       EndSr;


        //INZSST - Initialize the subset fields

       BegSr InzSst;
       EndSr;

        //KEYINZ - Initialize key values for program start

       BegSr KeyInz;

        //GET CLAIM MASTER RECORD.

          chain @@rrn2 pcm01;

            If %found(fmcmp);

              kkco#  = cmco#;
              kksub# = cmsub#;
              kkfnd  = cmfnd;
              kkfyr  = cmfyr;
              kkcase = cmcase;
              kkfein = *loval;
              kkmod  = *loval;
              kkbgdt = *hival;
              kkendt = *loval;

            Endif;

       EndSr;

        //KEYSET - Set key values when doing "position to"

       BegSr KeySet;
         If Scpst1 <> *Blanks;
           kkname = Scpst1;
           chain pdky1 ppd02;
             If %Found(FMPDL1);
               kkfein = pdfein;
               kkmod  = pdmod;
             Endif;
         Endif;
       EndSr;

        //KEYSFL - Set key values from subfile records for paging

       BegSr KeySfl;

         kkfein = *loval;
         kkmod  = *loval;

       EndSr;

        //KNDO - F14=DARTS

       BegSr KnDo;

          inco#  = %char(##co#);
          insub# = %char(cmsub#);
          infnd  = %char(cmfnd);
          infyr  = %char(cmfyr);
          incase = %char(cmcase);

           callp BldDartKey(inrtcd:inwcin:inwdck:inco#:insub#:infnd:infyr:
                          incase:inkey);

          *in90 = *On;

       EndSr;

        //MVDBSC - Move data base fields to screen fields

       BegSr MvDbSc;
          //GET ONLY ACTIVE RECORDS

           kkfein = ppfein;
           kkmod  = ppmod;

             If SCFEIN  = 0;
               *In16 = *On;
               Scname = pp40a1;
               ScModc = *Blanks;
             Endif;

       //fetch record from FMPDP
             chain pdky ppd01;
               If %Found(FMPDL3);
                 scpnm = %subst(pdname:1:25);
               Else;
                 scpnm = %subst(pp40a1:1:25);
               Endif;
           schdl1 = HD(1) ;
           xxbgdt = dtcheck(ppbgdt);
           xxendt = dtcheck(ppendt);
           x1stdt = xxbgdt;
           x1endt = xxendt;
           scdata = scpnm + '     ' +
                    pppcp    + '        ' +
                    x1stdd + '/' + x1stmm + '/' + x1styy +
                    '          '  +
                    x1endd + '/' + x1enmm + '/' + x1enyy;

       EndSr;

        //OTHFKS - Process other function keys

       BegSr OthFks;

          If *inKn = *on;
            Exsr KnDo;
          Endif;

       EndSr;

        //OPT7 - OPTION 7 - PROVIDER MAINTENANCE

       BegSr Opt7;

           If ppfein <> 0;

         // CALL API TO OBTAIN FMPDP RRN

              outrrn = 0;
              infein = %char(ppfein);
              inmod  = %char(ppmod);
               rtnRrn(infein: inmod: outrrn);
               inrrn = outrrn;
         // CALL PROVIDER MAINTENANCE IN DISPLAY MODE
               callp PrvMntDsp(*inkc:*inkl:xxmode:inrrn);

           Else;

             *in80 = *on;
             *in90 = *on;

           EndIf;


       EndSr;
        //OTHOPT - Process other options

       BegSr OthOpt;

         If scsel = '7';
         //call provider maintenance
           Exsr Opt7;
         Endif;
       EndSr;

        //PGMEND - End of program processing

       BegSr PgmEnd;
           *inlr = *On;
       EndSr;

       //PGMSTR - Start of program processing

       BegSr PgmStr;

          callp RtvSysVal(inWCIF:inMCIF:inWCIN:inAR:inNTWK:inUR:inMED:
                         inEDI:inWCDK);

        //CALL API TO CREATE A TWO LINE HEADING

            inCO# = %editc(CMCO#:'X');
            inSUB#= %editc(CMSUB#:'X');
            inFND = %editc(CMFND:'X');
            inFYR = %editc(CMFYR:'X');
            inCASE= %editc(CMCASE:'X');
            outL1 = *Blanks;
            outL2 = *Blanks;

            CrtLinHeading(inRTCD:inCO#:inSUB#:inFND:inFYR:inCASE:
                         outL1:outL2);
              SCL1HD = outL1;
              SCL2HD = outL2;


       EndSr;

          //READ   - Read next record from file

       BegSr Read;

         If *in21 = *On;
           Reade Key2 DBRCDF;

            If Not %Eof(Fappl9);
              *in95 = *off;
            Else;
              *in95 = *on;
            Endif;

         Endif;

       EndSr;

        //READP  - Read previous record from file

       BegSr Readp;

         If *in21 = *On;
           Readpe Key2 DBRCDF;

            If Not %Eof(Fappl9);
              *in95 = *off;
            Else;
              *in95 = *on;
            Endif;

         Endif;

       EndSr;

        //SETGT  - Set greater than on file

       BegSr SetGt;

         If *in21 = *On;
           SetGt Key1 DBRCDF;

            If %Found(Fappl9);
              *in96 = *off;
            Else;
              *in96 = *on;
            Endif;

         Endif;
       EndSr;

        //SETLL  - Set lower limits on file

       BegSr Setll;

         If *in21 = *On;
           Setll Key1 DBRCDF;

            If %found(Fappl9);
              *in96 = *off;
            Else;
              *in96 = *on;
            Endif;

         Endif;
       EndSr;

        //SF1UPD - Update subfile record when item is deleted

       BegSr SF1UPD;

         SCNAME = DL;

       EndSr;

        //SLCTRD - Move fields for selected record to parameters

       BegSr SLCTRD;

         @@RRN = SCDBRR;

       EndSr;

        //SUBSET - Determine if record should be put in subfile

       BegSr SubSet;

        //IF MODE IS PASSED IN AND IS 'Y', ONLY DISPLAY PROVIDERS WITH
        //FEDERAL ID#S

         If Q1PRMS > 5 and @@MODE = 'Y' and PPFEIN = 0;
           MEETSS = *Off;
         Endif;

         If Sssts <> *Blanks and Sssts <> ppsts;
           MEETSS = *Off;
         Endif;

         If Ssrstr <> *Blanks and ssrstr <> PPRSTR;
           MEETSS = *Off;
         Endif;

       EndSr;
      /end-free
** Column headings
Opt Treating physician name   PCP(Y/N) Begin treatment date End treatment date

** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
1077=Provider Inquiry       YYN
203F3=Exit                  YYN
205F5=Refresh               YNN
206F6=Add                   YNN
212F12=Cancel               YYY
214F14=DARTS                YYY
217F17=Subset               YYY
3
