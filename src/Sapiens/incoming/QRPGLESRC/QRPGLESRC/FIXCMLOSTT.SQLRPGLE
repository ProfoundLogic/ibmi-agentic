     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H/TITLE - Fix WMCMP/CMlost and WDPAP for Lost Days Calculation.
     H*****************************************************************
     H*  Created: 01/02/15   C:290     Support: 87306
     H*****************************************************************
     H* Information: This fix program gets compsuite install date as  *
     H* input and performs the following operations in below mentioned*
     H* sequence:                                                     *
     H*                                                               *
     H* 1- Fetch data into outfile WDPAPout from WDPAP based upon the *
     H*    selection criteria.                                        *
     H* 2- Update WDPAP based upon the selection criteria.            *
     H* 3- Update WARPP based upon the selection criteria.            *
     H* 4- Finally use the values of WDPAPOUT and Update WMCMP if     *
     H*    there is a difference in old week/days and calculated new  *
     H*    week/days of WDPAPout.                                     *
     H*                                                               *
     H*****************************************************************
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     fwdpap     uf   e           k disk    RENAME(PPA01:PPA00)
     fwarpp     uf   e           k disk    RENAME(PRP01:PRP00)
     fwmcml1    uf   e           k disk
     fwdpapout  uf a e           k disk
     Ffixpv1p   if   e           k disk
     Fwmupl     if   e           k disk

     dwdpapDS        E DS                  EXTNAME(WDPAPout:ppa01)

     dstrSQLStmt       s           1200    Inz(' ')
     dQ                C                   CONST(x'7D')

     dvarfloor         s              3  0 Inz(0)
     dvarmod           s              3  0 Inz(0)
     dvarwks           s              3  0 Inz(0)
     dvardays          s              3  0 Inz(0)
     dPAFNDprv         s              3  0 Inz(0)
     dPAFYRprv         s              3  0 Inz(0)
     dPACASEprv        s              7  0 Inz(0)
     dkey              s            100
     dcltID            s              3A
     d@@CO#            s              3  0

     dFixCMLostt       PR                  EXTPGM('FIXCMLOSTT')
     d@@date                          7  0

     d*Entry
     dFixCMLostt       PI
     d@@date                          7  0

      *  Program Information
     d progstatus     sds
     d  parms            *parms
     d  progname         *proc
     d  errmsgID              40     46
     d  errmsg                91    169
     d  jobname              244    253
     d  userid               254    263
     d  jobnumber            264    269

     d RtvCltIdP       pr                  ExtPgm('RTVCLTID')
     d cltID                          3A

      /Free
       // Set SQL options
          Exec SQl
               set OPTION
               commit = *none,
               datfmt = *ISO;

          Chain(n) userid WMUPL;
          If %found(WMUPL);
             @@CO# = UPco#;
          Endif;

          RtvCltIdP(cltID);
          chain (cltID) PIN01;
          If %found(FIXPV1P);
          key = INKEY;
          Endif;


        // PAYMENT DETAIL FILE(WDPAP) record processing for WDPAPout

          strSQLStmt = 'select  floor ' +
           ' ( (days(date(substr(char(19000000+patodt),5,2) ||' +Q+'/'+Q  +
           '|| substr(char(19000000+patodt),7,2) || ' +Q+ '/''' + ' || ' +
           'substr(char(19000000+patodt),1,4))) - ' +
           'days(date(substr(char(19000000+pafrdt),5,2) || ' +Q+'/'+Q+ ' || '
         + 'substr(char(19000000+pafrdt),7,2) || ' +Q+'/'+Q+ ' || ' +
           'substr(char(19000000+pafrdt),1,4)))+1)/7), mod( ' +
           '(days(date(substr(char(19000000+patodt),5,2) || ' +Q+'/'+Q+ ' || '
                    + 'substr(char(19000000+patodt),7,2) || ' +Q+'/'+Q+ ' || '
                    + 'substr(char(19000000+patodt),1,4))) - ' +
           'days(date(substr(char(19000000+pafrdt),5,2) || ' + Q+'/'+Q+ ' || '
                   + 'substr(char(19000000+pafrdt),7,2) || ' + Q+'/'+Q+ ' || '
                   + 'substr(char(19000000+pafrdt),1,4)))+1),7), pawks, ' +
           'padays, pafnd, pafyr, pacase From WDPAP where ' +
           ' paretp = ' +Q+'C'+Q + 'and ((pawks <> 0 or padays <> 0) or'
           + '(pawks = 0 and padays = 0 and pabr2 not in(' + %Trim(key) +
             '))) and paidt >=' + %Trim(%CHAR(@@date));

      /end-free

     c/Exec SQl
     c+   Prepare SQL_STMT from :strSQLStmt
     c/End-Exec

     c/Exec SQl
     c+   Declare cursor1 sensitive scroll cursor
     c+   for SQL_STMT
     c/End-Exec

     c/Exec SQl
     c+   Open cursor1
     c/End-Exec

      /free

       // Insert the above selected data into WDPAPOUT file

          Exec SQL fetch next from cursor1 into :wdpapDS;
          DoW (sqlstt < '02000');
             write ppa01;
             Exec SQL fetch next from cursor1 into :wdpapDS;
          Enddo;

      /end-free

     c/Exec SQl
     c+   Close cursor1
     c/End-Exec

     c*  // Update the PAYMENT DETAIL FILE(WDPAP) based on the condition

     c/Exec sql
     c+    Update WDPAP set pawks = floor(
     c+       (days(date(substr(char(19000000+patodt),5,2) || '/'
     c+       || substr(char(19000000+patodt),7,2) ||  '/' ||
     c+       substr(char(19000000+patodt),1,4))) -
     c+       days(date(substr(char(19000000+pafrdt),5,2) || '/' ||
     c+       substr(char(19000000+pafrdt),7,2) || '/' ||
     c+       substr(char(19000000+pafrdt),1,4)))+1)/7),
     c+       padays = mod(
     c+       (days(date(substr(char(19000000+patodt),5,2) || '/' ||
     c+       substr(char(19000000+patodt),7,2) || '/' ||
     c+       substr(char(19000000+patodt),1,4))) -
     c+       days(date(substr(char(19000000+pafrdt),5,2) || '/' ||
     c+       substr(char(19000000+pafrdt),7,2) || '/' ||
     c+       substr(char(19000000+pafrdt),1,4)))+1),7)
     c+       where paretp = 'C' and ((pawks <> 0 or padays <> 0) or
     c+             (pawks = 0 and padays = 0 and pabr2 not in(:key)))
     c+             and paidt >= :@@date
     c/End-Exec

      /free

              if sqlcod <> 0;
                 dsply sqlcode;
              endif;

       // Update the REQUEST PAYMENT FILE(WARPP) based on the condition

      /end-free

     c/Exec sql
     c+    Update WARPP set rpwks = floor(
     c+       (days(date(substr(char(19000000+rptodt),5,2) || '/'
     c+       || substr(char(19000000+rptodt),7,2) ||  '/' ||
     c+       substr(char(19000000+rptodt),1,4))) -
     c+       days(date(substr(char(19000000+rpfrdt),5,2) || '/' ||
     c+       substr(char(19000000+rpfrdt),7,2) || '/' ||
     c+       substr(char(19000000+rpfrdt),1,4)))+1)/7),
     c+       rpdays = mod(
     c+       (days(date(substr(char(19000000+rptodt),5,2) || '/' ||
     c+       substr(char(19000000+rptodt),7,2) || '/' ||
     c+       substr(char(19000000+rptodt),1,4))) -
     c+       days(date(substr(char(19000000+rpfrdt),5,2) || '/' ||
     c+       substr(char(19000000+rpfrdt),7,2) || '/' ||
     c+       substr(char(19000000+rpfrdt),1,4)))+1),7)
     c+       where rpretp = 'C' and ((rpwks <> 0 or rpdays <> 0) or
     c+             (rpwks = 0 and rpdays = 0 and rpbr2 not in(:key)))
     c+             and rpidt >= :@@date
     c/End-Exec

      /free

              if sqlcod <> 0;
                 dsply sqlcode;
              endif;

       // Now, we have all old and new weak and days so calculate cmlost from Claim file(WMCMP)

        Setll *start wdpapout;
        Read PPA01;
        PAFNDprv = PAFND;
        PAFYRprv = PAFYR;
        PACASEprv = PACASE;
        DoW Not %EOF(wdpapout);
           If PAFND=PAFNDprv and PAFYR=PAFYRprv and PACASE=PACASEprv;
              varfloor = varfloor + pafloor;
              varmod = varmod + pamod;
              varwks = varwks + PAwks;
              vardays = vardays + PAdays;
           Else;
       // process only if there is a change in Previous and new Week/Days
              If varfloor<>varwks or varmod<>vardays;
                 CMco# = @@CO#;
                 CMfnd = PAfndprv;
                 CMfyr = Pafyrprv;
                 CMcase = PAcaseprv;
       // Read WMCMP for previous fnd/year/case and Update CMLOST if required.
                 Chain (CMco#:Cmfnd:CMfyr:Cmcase) wmcml1;
                 If %found(wmcml1);
                    cmlost=cmlost-((varwks*7)+ vardays)+((varfloor*7)+varmod);
                    update PCM01;
                 Endif;
              Endif;

       // Store this FND/YEAR/CASE and repeat the process of cumulating the
       // old/new week and days.

              varfloor = 0;
              varmod = 0;
              varwks = 0;
              vardays = 0;
              PAFNDprv = PAFND;
              PAFYRprv = PAFYR;
              PACASEprv = PACASE;
              varfloor = varfloor + pafloor;
              varmod = varmod + pamod;
              varwks = varwks + PAwks;
              vardays = vardays + PAdays;
           Endif;

       // Read the next record from the PAYMENT DETAIL outFILE(WDPAPout)

        Read PPA01;
        Enddo;
       *InLr = *On;

      /End-Free
