    Hcopyright('(c) 2025 StoneRiver, Inc. All rights reserved. -
    H StoneRiver is a trademark of StoneRiver, Inc.')
    H option(*nodebugio:*srcstmt)
    H dftactgrp(*no) actgrp(*caller)
    H*****************************************************************
    H*  F8041R - Upload Bills - Transfer Program 1                   *
    H*                                                               *
    H*  Called from: F8041C  - Upload Bills - Override               *
    H*  Calls to   : F8042R  - Upload Bills - Transfer Pgm 2         *
    H*                                                               *
    H*****************************************************************
    H*  Created on 04/20/2004 by c:62 for project 51MFN              *
    H*****************************************************************
    H*  Revisions:                                                   *
    H*                                                               *
    H*  03/26/25 C:342   Project: 152MCF                             *
    H*                   Bill Data File - ACK Response File Req      *
    H*                                                               *
    H*  05/10/22 C:333   Project: MC442                              *
    H*                   Writing Dupilcate Bill IDs to file FADVP    *
    H*                                                               *
    H*  02/10/20 C:079   Support: 152/86698                          *
    H*                   Add duplicate recon logic                   *
    H*                                                               *
    H*  09/12/18 C:079   Support: 99/73314                           *
    H*                   Rename PU202 record format fields           *
    H*                                                               *
    H*  11/17/17 C:079   Project 114MWU                              *
    H*                   Remove 114MWU from V17R4 - Hard Remove      *
    H*                                                               *
    H*  12/23/13 C:296   PROJECT  129MSD                             *
    H*                   BACK TO BASE INITIATIVE-MANAGED CARE -      *
    H*                   Add a parm to indicate whether              *
    H*                   diaries are created (@@CMPY).               *
    H*                                                               *
    H*  12/06/13  C:079  Support: MC114 84260                        *
    H*                   Check Parms before comparing OptaComp parm  *
    H*                                                               *
    H*  11/01/13  C:274  Support: MC114 83764                        *
    H*                   Populate process date and time.             *
    H*                                                               *
    H*  10/10/13  C:079  Support: MC114 83764                        *
    H*                   Load key field value for line#              *
    H*                                                               *
    H*  10/02/13  C:062  Support: MC114 83669                        *
    H*                   Don't validate claim if not OCR             *
    H*                                                               *
    H*  09/13/13  C:079  Project: 114WJC                             *
    H*                   ACS OCR - Fix Claim Number Editing          *
    H*                                                               *
    H*  08/26/13  C:079  Support: MC133 83092                        *
    H*                   Allow writing FAU1P if soft deleted         *
    H*                                                               *
    H*  07/29/13  C:079  Support: 82944                              *
    H*                   Do not write duplicate FAU2P records        *
    H*                                                               *
    H*  05/06/13  C:282  Project: 114WJC                             *
    H*                   ACS OCR                                     *
    H*                                                               *
    H*  02/21/12  C:280  project: 133mmc                             *
    H*                   populate new fields                         *
    H*                                                               *
    H*  01/10/12  C:251  Project: 144MCE                             *
    H*                   Re-compiled.                                *
    H*                                                               *
    H*  06/10/11  C:198  Project: 139MQF                             *
    H*                   Re-compiled.                                *
    H*                                                               *
    H*  06/30/09  C:227  Support: 68989                              *
    H*                   Managed care reconsideration populating     *
    H*                   wrong amount in the system.                 *
    H*                                                               *
    H*  10/16/07  C:062  PROJECT: MC307                              *
    H*                   COMPILE ONLY                                *
    H*                                                               *
    H*  08/01/07  C:193  PROJECT: MC299                              *
    H*                   RECORD TRANSMISSION VERIFICATION FILES      *
    H*                   (IMPORT/EXPORT TO P2P)                      *
    H*                                                               *
     H*  12/12/06  C:062  PROJECT: 144MAB                             *
     H*                   CONDITION CALL OF F8043R.                   *
    H*                                                               *
     H*  07/14/06  C:079  PROJECT: MC279                              *
     H*                   USE 4 NEW EXPANDED EOB CODE/NOTE FIELDS     *
    H*                                                               *
     H*  04/05/06  C:163  PROJECT: WC132 51861                        *
     H*                   ADD CONDITION FOR WRITING PROVIDER RECORDS  *
     H*                                                               *
    H*  10/25/05 C:163 PROJECT 136MBR - Recompile only.              *
     H*                 Managed Care/Prov Bill Interface Process      *
    H*                                                               *
    H*  08/18/05 C:62 PROJECT 132MDN                                 *
    H*                INCLUDE NETWORK IN DUPLICATE PROCESSING        *
    H*                                                               *
    H*****************************************************************
    H*  Indicators used:                                             *
    H*                                                               *
    H*****************************************************************
    F*
    F*  Physical file definition (for maintaining record)
     Ffau0l     uf   e             disk
     Ffau1p     o    e             disk
     Ffau2p     o    e             disk
     Ffau9p     uf   e             disk
     Ffau1l6    if   e           k disk
     F                                     rename(pu101:pu102)
    F*** BEGIN ADD *** 08/01/07 **************************************
     Ffauhp     o    e             disk
    F*** END ADD ***** 08/01/07 **************************************
      *** BEG ADD *** 05/06/13 **************************************
      *** BEG CHG *** 09/13/13 **************************************
     f***wmcml1    if   e           k disk
     ffmcml1    if   e           k disk
      *** END CHG *** 09/13/13 **************************************
      *** END ADD *** 05/06/13 **************************************
    F*** Beg Add ***** 07/29/13 **************************************
    F*** Beg Chg ***** 09/12/18 **************************************
     F***fau2l3    if   e           k disk    rename(pu201:pu202)
     Ffau2l3    if   e           k disk    rename(pu201:pu202) Prefix(P2:2)
    F*** End Chg ***** 09/12/18 **************************************
    F*** End Add ***** 07/29/13 **************************************
    F*** Beg Add ***** 05/10/22 **************************************
     FFADVL1    if a e           k disk    rename(PDV01:pdvl01)
    F*** End Add ***** 05/10/22 **************************************
     f*** Beg Add *** 03/26/25 **************************************
     Ffxackp    if a e             disk    rename(pac01:pac0p) UsrOpn
     f*** End Add *** 03/26/25 **************************************
    F*
     D i9i             s              1    dim(10)
     D i9o             s              1    dim(10)
     D ini             s              1    dim(18)
     D ino             s              1    dim(18)
     D oc              s              1    dim(500)
     D fids01        e ds                  extname(szq2p)
     D psds          esds                  extname(szq1p)
    D*================================================================
    D* Constants
    D*================================================================
     D lo              c                   const('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D hi              c                   const('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D*** Beg Add *** 09/13/13 *********************************************
     D COMAPI          C                   CONST('FA007R')
     D*** End Add *** 09/13/13 *********************************************
    D*================================================================
    D*  User defined data structures
    D*================================================================
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    D*** BEGIN CHG *** 08/18/05 **************************************
    D**********  DS
    D**********                              1  16 TEST
    D**********                              1   1 XXTYPE
    D**********                              2  16 KKTRID
     D                 ds
     D test                         119
     D  xxtype                        1    overlay(test)
     D  kktrid                       15    overlay(test:2)
     D  kkntwk                        5    overlay(test:115)
    D*** END   CHG *** 08/18/05 **************************************
     D                 ds
     D test2                          6
     D  zipa                          5    overlay(test2)
     D   zip1                         1    overlay(zipa)
     D   zip2                         1    overlay(zipa:2)
     D   zip3                         1    overlay(zipa:3)
     D   zip4                         1    overlay(zipa:4)
     D   zip5                         1    overlay(zipa:5)
     D  rcnf                          1    overlay(test2:6)
     D header        e ds                  extname(fau1p)
     D detail        e ds                  extname(fau2p)
    D*** BEGIN ADD *** 08/01/07 **************************************
     D  utest          ds
     D  uutype                        1    overlay(utest)
     D  uubat#                      500    overlay(utest:2)
    D*** END ADD ***** 08/01/07 **************************************
     D*** BEG ADD *** 06/30/09 *********************************************
     D                 ds
     D  value                         9
     D  sign                   1      1
     D  amount                 2      9  2
     D*** END ADD *** 06/30/09 *********************************************
     D*** Beg Add *** 09/13/13 *********************************************
     D                 DS
     D  CLAIM#                 1     20
     D  GROUP                  1      3
     D  DASH1                  4      4
     D  YEAR                   5      7
     D  DASH2                  8      8
     D  CASE                   9     15
     D*** End Add *** 09/13/13 *********************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##name          s             10
     D ##rrn           s              9  0
     D #1rrn           s              9  0
     D @@mbr#          s             10
     D @@name          s             10
     D @@rrn           s              9
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D xxdcnt          s              9  0
     D xxtcnt          s              9  0
     D data500         s            500
     d*** BEGIN ADD *** 12/12/06 **************************************
     D c144            s              1
     d*** end   ADD *** 12/12/06 **************************************
      *** BEG ADD *** 05/06/13 **************************************
     d charCmpy        s              1
     d isOcrFlg        s              1
     d @@CMPY          s              1
      *** END ADD *** 05/06/13 **************************************
    D*** Beg Add ***** 07/29/13 **************************************
     D xxRlin#         s              8a
     D xxLin#          s              8a
     D kklin#          s                   like(u2lin#)
    D*** End Add ***** 07/29/13 **************************************
     D*** Beg Add *** 09/13/13 **************************************
     D kkco#           s                   like(cmCo#)
     D kkSub#          s                   like(cmSub#)
     D kkFnd           s                   like(cmFnd)
     D kkFyr           s                   like(cmFyr)
     D kkCase          s                   like(cmCase)
     D*** End Add *** 09/13/13 **************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*** Beg Add *** 03/26/25 **************************************
       Dcl-S strCommand Char(700);
       Dcl-S strPath Char(80);
       Dcl-S strPath2 VarChar(80);
       Dcl-S strDocRoot Char(10);
       Dcl-S sysDt10 Char(10);
       Dcl-S sysTm6  like(U9ultm);
       Dcl-S sysTm8  Char(8);
       Dcl-S suffix  Char(3);
      * Get Root
     d RtvDocRoot      pr                  extpgm('RTVDOCROOT')
     d StrDocRoot                    10

     d QCmdExc         pr                  extpgm('QCMDEXC')
     d command                             const like(strCommand)
     d length                        15  5 const

     d RtvSuffix       pr                  ExtPgm('SA030R')
     d   suffix                       3

     dRunSQLStmt       pr                  ExtPgm('SYRUNSQL')
     d sqlStmt                      550

     dstrSQLStmt       s            550
     D*** End Add *** 03/26/25 **************************************
    I*================================================================
    I*  /COPY required constants
    I*================================================================
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@rrn
     C                   parm                    @@name
     C                   parm                    @@mbr#
     *** BEG ADD *** 05/06/13 **************************************
     C                   parm                    @@cmpy
     *** END ADD *** 05/06/13 **************************************
    C*
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtmdy2        plist
     C                   parm                    srdti
     C                   parm      '*YYMD'       srffmt
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*** Beg Add *** 09/13/13 *********************************************
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*** End Add *** 09/13/13 *********************************************
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     u1ky          klist
     C                   kfld                    kktrid
    C*** BEGIN ADD *** 08/18/05 **************************************
     C                   kfld                    kkntwk
    C*** END   ADD *** 08/18/05 **************************************
     *** BEG ADD *** 05/06/13 **************************************
     C     keyWMCML1     klist
     C                   kfld                    U1Co#
     C                   kfld                    U1Fnd
     C                   kfld                    U1Fyr
     C                   kfld                    U1Case
     *** END ADD *** 05/06/13 **************************************
     *** BEG ADD *** 05/06/13 **************************************
     C*** Beg Add *** 09/13/13 **************************************
     C     keyFMCML1     klist
     C                   kfld                    kkCo#
     C                   kfld                    kkSub#
     C                   kfld                    kkFnd
     C                   kfld                    kkFyr
     C                   kfld                    kkCase
     C*** End Add *** 09/13/13 **************************************
     *** END ADD *** 05/06/13 **************************************
    C*** Beg Add ***** 07/29/13 **************************************
     C     u2ky          klist
     C                   kfld                    kktrid
     C                   kfld                    kklin#
    C*** End Add ***** 07/29/13 **************************************
    C*** Beg Add ***** 05/10/22 **************************************
     C     d1ky          klist
     C                   kfld                    @@mbr#
     C                   kfld                    kktrid
     C                   kfld                    U1ULDT
    C*** End Add ***** 05/10/22 **************************************
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*
    C*****************************************************************
    C*
    C* EXECUTE MAIN SUBROUTINE
    C*
     C                   exsr      mainsr
    C*
    C*================================================================
    C* MAINSR - MAIN SUBROUTINE
    C*================================================================
     C     mainsr        begsr
    C*
     C                   exsr      pgmstr
    C*
     C                   read      pu001                                  98
     C                   if        *in98 = *off
     C                   dow       *in98 = *off
     C                   exsr      mvffwk
    C*** BEGIN ADD *** 08/01/07 **************************************
     C                   exsr      mvffuh
    C*** END ADD ***** 08/01/07 **************************************
     C                   read      pu001                                  98
     C                   if        *in98 = *on
     C                   exsr      updat9
     C                   endif
     C                   enddo
     C                   else
     C                   exsr      updat9
     C                   endif
    C*
     C                   exsr      pgmend
    C*
     C                   endsr
    C*================================================================
    C* MVFFWK - Move flat file field to work files
    C*================================================================
     C     mvffwk        begsr
    C*
     C                   movel(p)  u0data        test
     C                   movel(p)  u0data        data500
     C                   if        xxtype = '1'
     C                   move      data500       test2
     C                   eval      xxtcnt = xxtcnt + 1
     C                   endif
    C*
    C* Write if trans does not exist or if exists and recon or soft delete
    C*
     C     u1ky          chain     pu102                              99
     C                   if        *in99 = *on
     C                             or *in99 = *off
     C                             and rcnf = 'Y'
     C*** Beg Add *** 02/10/20 *********************************************
     C                             and %Subst(U0data:2263:15) <> U1oTrId
     C*** End Add *** 02/10/20 *********************************************
     C*** Beg Add *** 08/26/13 *************************************.*******
     C                             or *in99 = *off
     C                             and u1prfl = 'D'
     C                             and u1dldt <> 0
     C*** End Add *** 08/26/13 *********************************************
     C                   exsr      writer
     C                   else
     C                   if        xxtype = '1'
     C                   eval      xxdcnt = xxdcnt + 1
    C*** Beg Add ***** 05/10/22 **************************************
     C                   exsr      mvdupid
    C*** End Add ***** 05/10/22 **************************************
     C                   endif
     C                   endif
    C*
     C                   exsr      updatr
    C*
     C                   endsr
    C*** BEGIN ADD *** 08/01/07 **************************************
     C*================================================================
    C* MVFFUH - Move flat file field to batch file FAUHP
     C*================================================================
     C     mvffuh        begsr
     C*
     C                   movel(p)  u0data        utest
     C                   if        uutype = 'H'
     C                   eval      uhtype = uutype
     c*
     c                   exsr      stripbad
     c*
     C                   eval      uhbat# = uubat#
     C                   eval      uhname = @@name
     C                   eval      uhprfl = u0prfl
     C*
     C                   write     puh01
     C                   endif
     C*
     C                   endsr
    C*** END ADD ***** 08/01/07 **************************************
    C*** BEGIN ADD *** 05/10/22 **************************************
     C*================================================================
    C* MVDUPID - Move Duplicate Bill IDs to file FADVP
     C*================================================================
     C     mvdupid       begsr
      /Free
       Chain  d1ky pdvl01;
       If not %found(FADVL1);
          DVmbr# = @@MBR#;
          DVvendId = KKtrid;
          DVuser = q1user;
          DVpgm = q1pgm;
      /End-Free
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
      /free
          Dvdt = %dec(srdto:7:0);
          Dvuldt = %dec(srdto:7:0);
          Dvidt = %dec(srdto:7:0);
          DVtime = dstime;
          DViusr = q1user;
          DVipgm = q1pgm;
          DVitim = dstime;
          write pdvl01;
       endif;
      /End-Free
     C                   Endsr
    C*** END ADD ***** 05/10/22 **************************************
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
    C*
    C* Check and write missing Provider records
    C*
    C*** BEGIN CHG *** 04/05/06 **************************************
     C****************** call      'F8043R'
     C*** BEGIN CHG *** 12/12/06 **************************************
     C**********         if        ##clt# <> '132'
     C                   if        ##cprv = 'Y'
     C*** END   CHG *** 12/12/06 **************************************
     C                   call      'F8043R'
     C                   endif
    C*** END   CHG *** 04/05/06 **************************************
    C*
    C* Transfer records to PDS
    C*
     *** BEG ADD *** 05/06/13 **************************************
      /Free
       // Only call if OCR flag is not passed.
       // Beg Chg *** 12/23/13 **************************************
       // If isOcrFlg = ' ';
       // Beg Chg *** 01/13/13 **************************************
       // If isOcrFlg <> 'O';
       If isOcrFlg <> 'Y';
       // End Chg *** 01/13/13 **************************************
       // End Chg *** 12/23/13 **************************************
      /End-Free
     *** END ADD *** 05/06/13 **************************************
     C                   call      'F8042R'
     C                   parm      @@name        ##name
     C                   parm      #1rrn         ##rrn
     C*** Beg Add *** 12/23/13 **************************************
     C                   parm      *Zeros        rrn2              9 0
     C                   parm      *Blanks       prvc              1
     C                   parm                    @@CMPY            1
     C*** End Add *** 12/23/13 **************************************
     *** BEG ADD *** 05/06/13 **************************************
      /Free
       EndIf;
      /End-Free
     *** END ADD *** 05/06/13 **************************************
    C*
       // Beg Add *** 03/26/25 **************************************
       If #1Rrn > 0 and ##clt# = '152';
          Exsr CreateAckCsv;
       Endif;
       // End Add *** 03/26/25 **************************************
     C                   eval      *inlr = *on
    C*
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
    C*
     C                   move      @@rrn         #1rrn
     *** BEG ADD *** 05/06/13 **************************************
      /Free
       If q1prms > 3;
       // Beg Add *** 12/23/13 **************************************
         If @@Cmpy = 'O';
       // End Add *** 12/23/13 **************************************
           charCmpy = @@Cmpy;
           isOcrFlg = 'Y';
       // Beg Add *** 12/23/13 **************************************
         Endif;
       // End Add *** 12/23/13 **************************************
       EndIf;
      /End-Free
     *** END ADD *** 05/06/13 **************************************
     C                   eval      xxtcnt = *zeros
     C                   eval      xxdcnt = *zeros
    C*** BEGIN ADD *** 04/05/06 **************************************
     C                   call      'RTVCLTID'
     C                   parm                    ##clt#            3
    C*** END   ADD *** 04/05/06 **************************************
     C*** BEGIN ADD *** 12/12/06 **************************************
     C*
     C* GET SYSTEM INFO
     C*
     C                   call      'SA001R'
     C                   parm      *blanks       ##wcif            1
     C                   parm      *blanks       ##mcif            1
     C                   parm      *blanks       ##wcin            1
     C                   parm      *blanks       ##ar              1
     C                   parm      *blanks       ##ntwk            1
     C                   parm      *blanks       ##ur              1
     C                   parm      *blanks       ##med             1
     C                   parm      *blanks       ##edi             1
     C                   parm      *blanks       ##wcdk            1
     C                   parm      *blanks       ##rhab            1
     C                   parm      *blanks       ##mss             1
     C                   parm      *blanks       ##fsch            1
     C                   parm      *blanks       ##prvb            1
     C                   parm      *blanks       ##npwc            1
15   C                   parm      *blanks       ##os              1
16   C                   parm      *blanks       ##arls            1
17   C                   parm      *blanks       ##ccod            1        ag
18   C                   parm      *blanks       ##crvw            1
19   C                   parm      *blanks       ##prom            1
20   C                   parm      *blanks       ##mcrv            1
21   C                   parm      *blanks       ##freq            1
22   C                   parm      *blanks       ##crac            1
23   C                   parm      *blanks       ##preo            1
24   C                   parm      *blanks       ##parn            1
25   C                   parm      *blanks       ##mgri            1
26   C                   parm      *blanks       ##kcif            1        o
27   C                   parm      *blanks       ##curs            1        or
28   C                   parm      *blanks       ##pcib            1        ch
29   C                   parm      *blanks       ##clt#            3
30   C                   parm      *blanks       ##ucpf            1
31   C                   parm      *blanks       ##dcf             1
32   C                   parm      *blanks       ##autb            1
33   C                   parm      *blanks       ##deif            1
34   C                   parm      *blanks       ##aure            1
35   C                   parm      *blanks       ##cred            1
36   C                   parm      *blanks       ##idrt            1
37   C                   parm      *blanks       ##cprv            1
     C*** END   ADD *** 12/12/06 **************************************
    C*
     C                   endsr
     C*================================================================
    C* STRIPBAD - Remove bad characters
     C*================================================================
     C     stripbad      begsr
     C*
     C                   CLEAR                   OC
     C                   MOVEA     uubat#        OC
      *
     C     1             DO        500           X                 3 0
     C     OC(X)         IFLT      X'40'
      *
     C     OC(X)         IFNE      X'08'
     C     OC(X)         ANDNE     X'09'
     C                   MOVE      X'40'         OC(X)
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     OC(X)         IFGT      X'F9'
     C                   MOVE      X'40'         OC(X)
     C                   ENDIF
     C                   ENDDO
     C                   MOVEA     OC            uubat#
     C*
     C                   endsr
    C*================================================================
    C* UPDATR - Update AMC Bill Flatfile (FAU0P).
    C*================================================================
     C     updatr        begsr
    C*
     C                   movel(p)  'Y'           u0prfl
     C                   update    pu001
    C*
     C                   endsr
    C*================================================================
    C* UPDAT9 - Update EOS Bill Transmission File (FAU9P).
    C*================================================================
     C     updat9        begsr
    C*
     C     #1rrn         chain     pu901                              99
     C                   if        *in99 = *off
     C                   if        u9cvdt = *zeros
    C*
     C                   eval      u9tcnt = xxtcnt
     C                   eval      u9dcnt = xxdcnt
     C                   movel(p)  q1user        u9user
     C                   movel(p)  q1pgm         u9pgm
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         u9cvdt
     C                   movel     srdto         u9dt
     C                   eval      u9cvtm = dstime
     C                   eval      u9time = dstime
     C*** Beg   ADD *** 11/01/13 **************************************
     C*** Beg   Add *** 12/06/13 **************************************
      /Free
                        If q1prms > 3;
      /End-Free
     C*** End   Add *** 12/06/13 **************************************
     C                   if        @@CMPY = 'O'
     C                   movel     srdto         u9prdt
     C                   eval      u9prtm = dstime
     C                   endif
     C*** Beg   Add *** 12/06/13 **************************************
      /Free
                        endif;
      /End-Free
     C*** End   Add *** 12/06/13 **************************************
     C*** End   ADD *** 11/01/13 **************************************
    C*
     C                   endif
     C                   update    pu901
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
    C*
     C                   select
     C                   when      xxtype = '1'
     C                   if        zip1 = *blanks
     C                             or zip2 = *blanks
     C                             or zip3 = *blanks
     C                             or zip4 = *blanks
     C                             or zip5 = *blanks
     C                   move      '00000'       zipa
     C                   endif
     C                   move      test2         data500
     C                   clear                   pu101
     c                   if        %subst(u0data:1045:9)=*blanks
     c                   eval      %subst(u0data:1045:9)='000000000'
     c                   endif
     C                   movel     data500       header
     C*** BEG ADD *** 07/14/06 *********************************************
     C                   eval      %subst(header:666) = %subst(u0data:501)
     C*** END ADD *** 07/14/06 *********************************************
     c                   if        u1oth2=*blanks
     c                   eval      u1oth2='00000000000'
     c                   endif
     C*** BEG ADD *** 06/30/09 *********************************************
     C                   eval      value = %subst(header:238:9)
     C                   if        sign = '-'
     C                   z-sub     amount        u1cost
     C                   else
     C                   z-add     amount        u1cost
     C                   endif
     C*** ND ADD *** 06/30/09 *********************************************
     C                   movel(p)  @@name        u1name
     C                   movel(p)  @@mbr#        u1mbr#
    C*
    C*** BEG ADD *** 02/21/12 *********************************************
    C*
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         u1uldt
     C                   eval      u1ultm = dstime
    C*
    C*** END ADD *** 02/21/12 *********************************************
    C*
     C*** Beg Add *** 09/13/13 *********************************************
     C                   CALL      COMAPI        COMPL                  99
      /Free
       If isOcrFlg<>' ';
          claim# = u1clm#;
          kkco# = ##CO#;
          kksub# = ##sub#;
          U1prfl = 'O';

          Monitor;
            kkfnd = %Dec(group:3:0);
              On-Error 105;
                 u1prfl = 'X';
          EndMon;
          Monitor;
            kkfyr = %Dec(year:3:0);
              On-Error 105;
                 u1prfl = 'X';
          EndMon;
          Monitor;
            kkcase= %Dec(case:7:0);
              On-Error 105;
                 u1prfl = 'X';
          EndMon;

          If u1prfl <> 'X';
            Chain keyfMCML1 pcm01;
               If Not %Found(fmcml1);
                 U1prfl = 'X';
               EndIf;
          EndIf;
       EndIf;
      /End-Free
     C*** End Add *** 09/13/13 *********************************************
     *** BEG ADD *** 05/06/13 **************************************
     C*** Beg Del *** 09/13/13 *********************************************
       //**
       //Free***
       //If*isOcrFlg = 'Y';
       //***U1prfl = 'O';
       //***Chain keyWMCML1 pcm01;
       //******If Not %Found(wmcml1);
       //******U1prfl = 'X';
       //******EndIf;
       //EndIf;*
       //End-Free
     C*** End Del *** 09/13/13 *********************************************
     *** END ADD *** 05/06/13 **************************************
     C                   write     pu101
     C                   when      xxtype = '2'
     C                   clear                   pu201
     C                   movel     data500       detail
     C                   eval      %subst(detail:510) = %subst(u0data:501)
     c                   if        u2oth2=*blanks
     c                   eval      u2oth2='00000000000'
     c                   endif
    C*** Beg Add ***** 07/29/13 **************************************
     C                   eval      xxRlin# = %subst(u0data:199:8)
     C                   eval      xxlin# = %subst(u0data:17:8)
    C*** Beg Add ***** 10/10/13 **************************************
      /Free
                         Monitor;
                          kklin# = %Dec(xxlin#:8:0);
                          On-Error 105;
                         EndMon;
      /End-Free
    C*** End Add ***** 10/10/13 **************************************
     C     u2ky          chain     pu202                              99
     C                   if        *in99 = *on or
     C                             *in99 = *off and xxRlin# <> '00000000'
     C*** Beg Add *** 08/26/13 *********************************************
     C                             or *in99 = *off and u2prfl = 'D'
     C*** End Add *** 08/26/13 *********************************************
    C*** End Add ***** 07/29/13 **************************************
     C                   write     pu201
    C*** Beg Add ***** 07/29/13 **************************************
     C                   endif
    C*** End Add ***** 07/29/13 **************************************
     C                   endsl
    C*
     C                   endsr
       // Beg Add *** 03/26/25 **************************************
       Begsr CreateAckCsv;
        chain #1rrn Fau9p;
        If %found(Fau9p);

           Monitor;
            strCommand = 'SYDUPFILE FROM(FXACKP) TO(QTEMP/FXACKP) DATA(*NO)';
            Callp QCMDEXC(strCommand:%len(strCommand));

            clear strCommand;
            strCommand = 'OVRDBF FILE(FXACKP) TOFILE(' +
            'QTEMP/FXACKP)';
            Callp QCMDEXC(strCommand:%len(strCommand));
           On-error;
           Endmon;

           //Write to Fxackp file
           IF %open(fxackp);
              Close fxackp;
           ENDIF;
           Open fxackp;
           Clear pac0p;
           ACkmbr# = U9mbr#;
           Monitor;
            ACkuldt = %scanrpl('/':'-':%char(%date(U9uldt:*cymd):*usa));
            ACkcvdt = %scanrpl('/':'-':%char(%date(U9cvdt:*cymd):*usa));
            ACkprdt = %scanrpl('/':'-':%char(%date(U9prdt:*cymd):*usa));
           On-error;
           Endmon;
           ACkultm = %subst(%editc(U9ultm:'X'):1:2) + '-' +
                     %subst(%editc(U9ultm:'X'):3:2) + '-' +
                     %subst(%editc(U9ultm:'X'):5:2);
           ACkcvtm = %subst(%editc(U9cvtm:'X'):1:2) + '-' +
                     %subst(%editc(U9cvtm:'X'):3:2) + '-' +
                     %subst(%editc(U9cvtm:'X'):5:2);
           ACkprtm = %subst(%editc(U9prtm:'X'):1:2) + '-' +
                     %subst(%editc(U9prtm:'X'):3:2) + '-' +
                     %subst(%editc(U9prtm:'X'):5:2);
           ACktcnt = %char(U9tcnt);
           ACkdcnt = %char(U9dcnt);
           ACkcpcnt= %char(U9ccnt);
           ACkrcnt = %char(U9rcnt);
           ACkucnt = %char(U9ucnt);
           write pac0p;

           // Build the path name
           strDocRoot = *blanks;
           RtvSuffix(suffix);
           Callp RtvDocRoot(strDocRoot);
           If strDocRoot <> *blanks;
              strPath = '/' + %trim(strDocRoot) + '/PDS/' + suffix +
                         '/Export/';
           Else;
              strPath = '/PDS/' + suffix + '/Export/';
           Endif;

           sysTm6   = %dec(%Char(%Time():*Hms0):6:0);
           sysTm8   = %subst(%editc(sysTm6:'X'):1:2) + '-' +
                      %subst(%editc(sysTm6:'X'):3:2) + '-' +
                      %subst(%editc(sysTm6:'X'):5:2);
           sysDt10  = %scanrpl('/':'-':%Char(%Date():*usa));
           strPath2 = 'Bill Import Ack ' + %Trim(sysDt10) +
                      ' ' + %Trim(sysTm8) + '.csv';
           strPath  = %trim(strPath) + %trim(strPath2);

           Monitor;
            strSQLStmt= 'Create View Qtemp.Fxackpvw as ' +
                       'Select ' +
              'ACKMBR# as "Member Name", ' +
              'ACKULDT as "Date File Copied From Folder", ' +
              'ACKULTM as "Time File Copied From Folder", ' +
              'ACKCVDT as "Date File Converted", ' +
              'ACKCVTM as "Time File Converted", ' +
              'ACKPRDT as "Date File Processed", ' +
              'ACKPRTM as "Time File Processed", ' +
              'ACKTCNT as "Header Record Count", ' +
              'ACKDCNT as "Duplicate Bill ID Count", ' +
              'ACKCPCNT as "Claim or Provider Error Count", ' +
              'ACKRCNT  as "Reconsideration Error Count", ' +
              'ACKUCNT  as "Bills Uploaded Count" ' +
              'From Qtemp.Fxackp';
            Callp RunSQLStmt(strSQLStmt);

            // Create CSV File
            strcommand = 'CpyToImpf FromFile(qtemp/fxackpvw) ToStmF(' +
                     '''' + %trim(strPath) + '''' + ') ' +
                     'MbrOpt(*Replace) STMFAUT(*InDir) StmfCCSID(*PCASCII) ' +
                     'RcdDlm(*CRLF) STRDLM(*None) ' +
                     'RmvBlank(*trailing) AddColNam(*SQL)';

            Callp QCMDEXC(strCommand:%len(strCommand));
           On-error;
           Endmon;

           Monitor;
            if %open(fxackp);
               close fxackp;
            endif;
            strCommand = 'DltOvr File(fxackp)';
            Callp QCMDEXC(strCommand:%len(strCommand));
           On-error;
           Endmon;

        Endif;
       Endsr;
       // End Add *** 03/26/25 **************************************
