     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H**********************************************************************
     H/TITLE N9601R - Process Detail Charges to Correct Rate/Discount Code *
     H**********************************************************************
     H* Description:  This program will be used to Retrieve EOB code       *
     H*               Description.                                         *
     H*                                                                    *
     H*  Called From: FA713R,  FA735R, N96DAR, N96DWR                      *
     H*                                                                    *
     H*  Calls To:    None                                                 *
     H*                                                                    *
     H**********************************************************************
     H* Created on 04/30/2013 by C:284   for  Project 147MAS               *
     H**********************************************************************
     H* Revisions:                                                         *
     H*
     H*  03/16/14 C:280 Support 85089
     H*                 Correct issue with override not allowed
     H*                                                                    *
     H**********************************************************************
     FFTLPL1    if   e           k disk
      * License Panel Rate Code Table - Logical
     FNMBDL     if   e           k disk
      * NTWK (all system) billing rate/disc master - Logical
     FFAEUL     if   e           k disk
      * EOMB Hosp/UB Header File - Logical
     FFAEHL     if   e           k disk
      * EOMB Phys/HCFA Header File - Logical
     FFDCALL    if   e           k disk
      * Detail Charge File
     F*
     F*** Beg Add *** 03/16/14 **************************************
     F*
     FFTABL     IF   E           K DISK    IGNORE(PAC01)
     F                                     IGNORE(PEB01)
     F                                     IGNORE(PPM01)
     F                                     IGNORE(PPA01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PPS01)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PFT01)
     F                                     IGNORE(PTS01)
     F*
     F*** End Add *** 03/16/14 **************************************
     F*
      **********************************************************************
      *Procedure defination
      *Date conversion program
      *
     Dsrcvtdat         pr                  Extpgm('SRCVTDAT')
     Dsrdti                           8  0
     Dsrffmt                          8
     Dsrtfmt                          8
     Dsrdto                          10
     Dsrerr                           1
      * Calculate Network Discount for DU
      *
     DCalcNtwkDiscDU   pr                  extpgm('N96DUR')
     DQQCO#                           3
     DQQSUB#                          5
     DQQFUND                          3
     DQQFYR                           3
     DQQCASE                          7
     DQQFEIN                          9
     DQQMOD                           3
     DQQFDAT                          8
     DQQDSEQ                          5
     DQQCODE                          3
     DQQCEDT                          8
     DQQSYS                           1
     DQQFL1                           1
     DQQFL2                           1
     DQQFL3                           1
     DQQFL4                           1
     DQQFL5                           1
     DQQDS1                          10
     DQQDS2                          10
     DQQDS3                          10
     DQQDS4                          10
     DQQDS5                          10
     DQQRTCD                          1
      * Calculate Network Discount for DA
      *
     DCalcNtwkDiscDA   pr                  extpgm('N96DAR')
     DQQCO#                           3
     DQQSUB#                          5
     DQQFUND                          3
     DQQFYR                           3
     DQQCASE                          7
     DQQFEIN                          9
     DQQMOD                           3
     DQQFDAT                          8
     DQQDSEQ                          5
     DQQCODE                          3
     DQQCEDT                          8
     DQQSYS                           1
     DQQFL1                           1
     DQQFL2                           1
     DQQFL3                           1
     DQQFL4                           1
     DQQFL5                           1
     DQQDS1                          10
     DQQDS2                          10
     DQQDS3                          10
     DQQDS4                          10
     DQQDS5                          10
     DQQRTCD                          1
      * Calculate Network Discount for DW
      *
     DCalcNtwkDiscDW   pr                  extpgm('N96DWR')
     DQQCO#                           3
     DQQSUB#                          5
     DQQFUND                          3
     DQQFYR                           3
     DQQCASE                          7
     DQQFEIN                          9
     DQQMOD                           3
     DQQFDAT                          8
     DQQDSEQ                          5
     DQQCODE                          3
     DQQCEDT                          8
     DQQSYS                           1
     DQQFL1                           1
     DQQFL2                           1
     DQQFL3                           1
     DQQFL4                           1
     DQQFL5                           1
     DQQDS1                          10
     DQQDS2                          10
     DQQDS3                          10
     DQQDS4                          10
     DQQDS5                          10
     DQQRTCD                          1
      * Calculate Network Discount for DT
      *
     DCalcNtwkDiscDT   pr                  extpgm('N96DTR')
     DQQCO#                           3
     DQQSUB#                          5
     DQQFUND                          3
     DQQFYR                           3
     DQQCASE                          7
     DQQFEIN                          9
     DQQMOD                           3
     DQQFDAT                          8
     DQQDSEQ                          5
     DQQCODE                          3
     DQQCEDT                          8
     DQQSYS                           1
     DQQFL1                           1
     DQQFL2                           1
     DQQFL3                           1
     DQQFL4                           1
     DQQFL5                           1
     DQQDS1                          10
     DQQDS2                          10
     DQQDS3                          10
     DQQDS4                          10
     DQQDS5                          10
     DQQRTCD                          1
     DQQmode                          1
     DQQalow                          9  2
      * Calculate Network Discount for DV
      *
     DCalcNtwkDiscDV   pr                  extpgm('N96DVR')
     DQQCO#                           3
     DQQSUB#                          5
     DQQFUND                          3
     DQQFYR                           3
     DQQCASE                          7
     DQQFEIN                          9
     DQQMOD                           3
     DQQFDAT                          8
     DQQDSEQ                          5
     DQQCODE                          3
     DQQCEDT                          8
     DQQSYS                           1
     DQQFL1                           1
     DQQFL2                           1
     DQQFL3                           1
     DQQFL4                           1
     DQQFL5                           1
     DQQDS1                          10
     DQQDS2                          10
     DQQDS3                          10
     DQQDS4                          10
     DQQDS5                          10
     DQQRTCD                          1
     DKKEOB#                          9  0
     DKKRCN                           3  0
      * Calculate Network Discount for DB
      *
     DCalcNtwkDiscDB   pr                  extpgm('N96DBR')
     DQQCO#                           3
     DQQSUB#                          5
     DQQFUND                          3
     DQQFYR                           3
     DQQCASE                          7
     DQQFEIN                          9
     DQQMOD                           3
     DQQFDAT                          8
     DQQDSEQ                          5
     DQQCODE                          3
     DQQCEDT                          8
     DQQSYS                           1
     DQQFL1                           1
     DQQFL2                           1
     DQQFL3                           1
     DQQFL4                           1
     DQQFL5                           1
     DQQDS1                          10
     DQQDS2                          10
     DQQDS3                          10
     DQQDS4                          10
     DQQDS5                          10
     DQQRTCD                          1
      * Stand Alone Variables
      *
     D xxpgm           s              6
     D xxtype          s                   like(bdtype)
     D kkco#           s                   like(euco#)
     D kksub#          s                   like(eusub#)
     D kkfund          s                   like(eufnd)
     D kkfyr           s                   like(eufyr)
     D kkcase          s                   like(eucase)
     D kkeob#          s                   like(eueob#)
     D kkedat          s              7  0
     D kkfein          s              9  0
     D kkmod           s              3  0
     D kkrcn           s              3  0
     D qqalow          s              9  2
     D qqcedt          s              8
     D qqcode          s              3
     D qqdseq          s              5
     D qqfdat          s              8
     D qqmode          s              1
     D srdti           s              8  0
     D srffmt          s              8    Inz('*CYMD')
     D srtfmt          s              8    Inz(*blanks)
     D srdto           s             10
     D srerr           s              1    Inz(*blanks)
      **********************************************************************
      * Key list for file FAEUL
     C     euky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfund
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C*
     C* key list for file NMBDL
     C*    bdky          klist
     C*                  kfld                    lpcode
     C*                  kfld                    kkedat
     C*
     C     caky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfund
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C                   kfld                    kkeob#
     C*
     C*** Beg Add *** 03/16/14 **************************************
     C*
     C*  CLAIM STATUS CODE TABLE FILE
     C*
     C     TCSKY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    TABKEY           10
     C*
     C*** End Add *** 03/16/14 **************************************
     C*
     C*
     C     *entry        plist
     C                   parm                    qqco#             3
     C                   parm                    qqsub#            5
     C                   parm                    qqfund            3
     C                   parm                    qqfyr             3
     C                   parm                    qqcase            7
     C                   parm                    qqfein            9
     C                   parm                    qqmod             3
     C                   parm                    qqeob#            9
     C                   parm                    qqedat            8
     C                   parm                    qqnwcd            5
     C                   parm                    qqsys             1
     C                   parm                    qqfl1             1
     C                   parm                    qqfl2             1
     C                   parm                    qqfl3             1
     C                   parm                    qqfl4             1
     C                   parm                    qqfl5             1
     C                   parm                    qqds1            10
     C                   parm                    qqds2            10
     C                   parm                    qqds3            10
     C                   parm                    qqds4            10
     C                   parm                    qqds5            10
     C                   parm                    qqbtyp            1
     C                   parm                    qqrtcd            1
     C*
      *
      /Free
       kkco# = %dec(qqco#:3:0);
       kksub# = %dec(qqsub#:5:0);
       kkfund = %dec(qqfund:3:0);
       kkfyr = %dec(qqfyr:3:0);
       kkcase = %dec(qqcase:7:0);
       kkeob# = %dec(qqeob#:9:0);
       kkfein = %dec(qqfein:9:0);
       kkmod = %dec(qqmod:3:0);
       chain caky1 pca01;
       Dow %found(fdcall) and not %eof(fdcall);

      // beg add 03/16/14

          xxco# = kkco#;
          tabkey = *blanks;
          tabkey = cacscd;

          chain tcsky ftabl;

          if %found(ftabl);

          if csovrd <> 'Y';

      // end add 03/16/14

          kkedat = cafdat;
          evalr qqdseq = %char(cadseq);
          srdti = cafdat;
          srcvtdat(srdti:srffmt:srtfmt:srdto:srerr);
          qqfdat = srdto;
          if qqbtyp = 'H';
             chain euky peu01;
             if %found(faeul);
                setgt (euflic:kkedat) plp01;
                readpe (euflic) plp01;
                if Not %Eof(ftlpl1);
                   chain (lpcode) pbd01;
                   if %found(nmbdl);
                      xxtype = bdtype;
                      qqcode = bdcode;
                   endif;
                endif;
             endif;
          else;
             chain euky peh01;
             if %found(faehl);
                setgt (ehflic:kkedat) plp01;
                readpe (ehflic) plp01;
                if Not %Eof(ftlpl1);
                   chain (lpcode) pbd01;
                   if %found(nmbdl);
                      xxtype = bdtype;
                      qqcode = bdcode;
                   endif;
                endif;
             endif;
          endif;

          qqcedt = *blanks;
          qqsys = *blanks;
          qqfl1 = *blanks;
          qqfl2 = *blanks;
          qqfl3 = *blanks;
          qqfl4 = *blanks;
          qqfl5 = *blanks;
          qqds1 = *blanks;
          qqds2 = *blanks;
          qqds3 = *blanks;
          qqds4 = *blanks;
          qqds5 = *blanks;
          qqrtcd = *blanks;
          qqmode = *blanks;
          qqalow = *zeros;

          xxpgm = 'N96' + xxtype + 'R';
          select;
             when xxpgm = 'N96DTR';
                CalcNtwkDiscDT(qqco#:qqsub#:qqfund:qqfyr:qqcase:qqfein:
                               qqmod:qqfdat:qqdseq:qqcode:qqcedt:qqsys:
                               qqfl1:qqfl2:qqfl3:qqfl4:qqfl5:qqds1:qqds2:
                               qqds3:qqds4:qqds5:qqrtcd:qqmode:qqalow);

             when xxpgm = 'N96DVR';
                CalcNtwkDiscDV(qqco#:qqsub#:qqfund:qqfyr:qqcase:qqfein:
                               qqmod:qqfdat:qqdseq:qqcode:qqcedt:qqsys:
                               qqfl1:qqfl2:qqfl3:qqfl4:qqfl5:qqds1:qqds2:
                               qqds3:qqds4:qqds5:qqrtcd:kkeob#:kkrcn);

             when xxpgm = 'N96DUR';
                CalcNtwkDiscDU(qqco#:qqsub#:qqfund:qqfyr:qqcase:qqfein:
                               qqmod:qqfdat:qqdseq:qqcode:qqcedt:qqsys:
                               qqfl1:qqfl2:qqfl3:qqfl4:qqfl5:qqds1:qqds2:
                               qqds3:qqds4:qqds5:qqrtcd);

             when xxpgm = 'N96DWR';
                CalcNtwkDiscDW(qqco#:qqsub#:qqfund:qqfyr:qqcase:qqfein:
                               qqmod:qqfdat:qqdseq:qqcode:qqcedt:qqsys:
                               qqfl1:qqfl2:qqfl3:qqfl4:qqfl5:qqds1:qqds2:
                               qqds3:qqds4:qqds5:qqrtcd);

             when xxpgm = 'N96DAR';
                CalcNtwkDiscDA(qqco#:qqsub#:qqfund:qqfyr:qqcase:qqfein:
                               qqmod:qqfdat:qqdseq:qqcode:qqcedt:qqsys:
                               qqfl1:qqfl2:qqfl3:qqfl4:qqfl5:qqds1:qqds2:
                               qqds3:qqds4:qqds5:qqrtcd);

             when xxpgm = 'N96DBR';
                CalcNtwkDiscDB(qqco#:qqsub#:qqfund:qqfyr:qqcase:qqfein:
                               qqmod:qqfdat:qqdseq:qqcode:qqcedt:qqsys:
                               qqfl1:qqfl2:qqfl3:qqfl4:qqfl5:qqds1:qqds2:
                               qqds3:qqds4:qqds5:qqrtcd);

          endsl;

      // beg add 03/16/14

          endif;

          endif;

      // end add 03/16/14

          reade caky1 pca01;

       enddo;
       *inlr = *on;
       /End-Free
