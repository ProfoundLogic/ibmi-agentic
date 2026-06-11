     //****************************************************************
     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)

    H/TITLE FA786R - Bill Import Line Validation API
    
     ******************************************************************
     *                                                                *
     *   CREATED: 11/22/16                                            *
     *            C:212  Project: 25MUJ - Bill Import Process - Add   *
     *                                   Detail Edit                  *
     ******************************************************************
     *  Revisions:                                                    *
     *                                                                *
     *                                                                *
     ******************************************************************

     Ffdelp     o    e           k disk
     Ffaeop     if   e             disk
     Ffaeolg    uf   e           k disk    rename(peo01:peo0g)
     F                                     Prefix(EG:2)
     Ffdcal5    uf   e           k disk
     Ffaeul     if   e           k disk
     Ffddhl     if   e           k disk
     Ffddul     if   e           k disk
     Ffdddl     if   e           k disk
     Ffddpl     if   e           k disk
     Fftpsl     if   e           k disk
     Fftabl     if   e           k disk    ignore(PPS01)
     Ffmrxl     if   e           k disk
     Fftgtl     if   e           k disk
     Ffmprl     if   e           k disk    usropn
     Ffttsl1    if   e           k disk    rename(pts01:pts02)
     Fftapl     if   e           k disk
     Ffmmgl1    if   e           k disk

     d* procedure definitions
     d*
     d RunCommand      pr                  extPgm('QCMDEXC')
     d inCmd                       2750    const
     d inCmdLen                      15  5 const

     d* array definitions
     d*
     d qca             s             56    dim(6) ctdata perrcd(1)              qcmdexc
    
     d* data structure declaration
     D psds          esds                  extname(szq1p)

     d                 ds
     d  @@cmd                  1     56
     d  @@st                  38     39
    
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm:1)
     D  dsdate                        8  0 overlay(dsdttm:7)
    
     D comapi          C                   const('FA007R')
    
     Dk1cmsv           ds
     D k1rvcd                  1      3
     D k1rate                  4      8
     D k1unts                  9     11
    
      //================================================================
      //  Standalone declarations
      //================================================================
    
     D kkco#           s                   like(EOCO#)
     D kksub#          s                   like(EOSUB#)
     D kkfnd           s                   like(EOFND)
     D kkfyr           s                   like(EOFYR)
     D kkcase          s                   like(EOCASE)
     D kkeob#          s                   like(EOEOB#)
     D kkdseq          s                   like(cadseq)
     D kkpscd          s                   like(pspos)
     D kkkey           s                   like(cskey)
     D k1efdt          s                   like(srdto )
     D kktbid          s                   like(gttbid)
     D kkkey1          s                   like(gtkey)
     D kkndc           s                   like(dpndc)
     D ERRcode         s                   like(elcode)
     D ERRfld          s                   like(elfld)
     D ERRvalu         s                   like(elvalu)
     D kkprcd          s                   like(prcpt4)
     D kkfdat          s                   like(prefdt)
     D kktier          s                   like(mgtier) inz
     D ##co#           s              3  0
     D ##fnd           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##sub#          s              5  0
     D ##fyr           s              3  0
     D isfound         s               n   inz(*off)
     D isBErr          s               n   Inz(*Off)
     D curdt70         s              7s 0 inz

     D inCmd           s           2750
     D inCmdLen        s             15  5
    
      //================================================================
      //  KLIST/PLIST definitions
      //================================================================
    
      // KEY LIST FOR DETAIL CHARGE FILE
    
     C     caky          klist
     C                   kfld                    eoco#
     C                   kfld                    eosub#
     C                   kfld                    eofnd
     C                   kfld                    eofyr
     C                   kfld                    eocase
     C                   kfld                    eoeob#
    
      // KEY LIST FOR TRANSACTION DETAIL FILES
    
     C     ddky          klist
     C                   kfld                    eoco#
     C                   kfld                    eosub#
     C                   kfld                    eofnd
     C                   kfld                    eofyr
     C                   kfld                    eocase
     C                   kfld                    eoeob#
     C                   kfld                    kkdseq
    
      // KEY LIST FOR PLACE OF SERVICE
    
     C     psky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkpscd

      // GENERIC TABLE KEY

     C     tbky          klist
     C                   kfld                    ##CO#
     C                   kfld                    kkkEY

      // GENERIC TABLE KEY

     C     gtky          klist
     C                   kfld                    ##CO#
     C                   kfld                    kktbid
     C                   kfld                    kkkey1

     C     gtky2         klist
     C                   kfld                    ##CO#
     C                   kfld                    kktbid

      //  Key list for procedure code file

     C     rxky          klist
     C                   kfld                    ##CO#
     C                   kfld                    kkndc

      // Key list for Service Codes File

     C     svky1         klist
     C                   kfld                    ##CO#
     C                   kfld                    k1cmsv
     C                   kfld                    k1efdt

     C     svky2         klist
     C                   kfld                    ##CO#
     C                   kfld                    k1cmsv

     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C                   parm                    ##sub#
     C                   parm                    ##fyr

     C     datck2        plist
     C                   parm                    srdti             8 0
     C                   parm      '*CYMD'       srffmt            8
     C                   parm      *blanks       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1

     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr

     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr

      //  KEY LIST FOR PROCEDURE CODE FILE

     C     PRKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKPRCD
     C                   KFLD                    KKFDAT

     C     PRKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKPRCD

     C     *entry        plist
     C                   parm                    @@mode            1
     C                   parm                    @@KL              1
     C                   parm                    @@rrn             9 0

      //================================================================
      //  Main
      //================================================================
    
      // Initialize and get FAEOP record from RRN passed to program.

      /FREE
       Exsr srinit;
       Exsr geteop;
    
       // Override files to State specific
       Exsr ovrfile;
    
       // Loop through detail. Check for errors based on bill type, write errors to FDELP.
    
       If *in90 = *off;
          exsr getcap;
       Endif;
    
       Dow *in90 = *off;
          Clear *all pel01;
          isBErr = *Off;

          Select;
          When eobtyp = 'N';
             exsr edtphy;
          When eobtyp = 'H';
             exsr edthos;
          When eobtyp = 'D';
             exsr edtden;
          When eobtyp = 'X';
             exsr edtpha;
          Endsl;

          If isBErr = *On;
             Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) peo0g;
             If %Found(faeolg);
                EGberr = 'Y';
                Update peo0g %Fields(EGberr);
             EndIf;

             CAberr = 'Y';
             Update pca01 %Fields(CAberr);
          EndIf;
    
          Reade caky pca01;
          *IN90 = %EOF;
       Enddo;
    
       *inlr = *on;
    
       //================================================================
       // srinit - Initialization
       //================================================================
    
       Begsr srinit;
    
      /END-FREE
     C                   CALL      'SA009R'
     C                   PARM      *ZEROS        ##EOBD            3 0
     C                   PARM      *BLANKS       ##EOBI            1
     C                   PARM      *BLANKS       ##ARIN            1
     C                   PARM      *BLANKS       ##ACIS            1
     C                   PARM      *BLANKS       ##PAY0            1
     C                   PARM      *BLANKS       ##SDTS            1

     C                   Call      comapi        compl
      /FREE
          kkco# = ##co#;
          curdt70 = %dec(%char(%date():*cymd0):7:0);
    
       Endsr;
    
       //================================================================
       // geteop - Get FAEOP
       //================================================================
    
       Begsr geteop;
    
          Chain @@rrn PEO01;
          *IN90 = NOT %FOUND;
    
       Endsr;
    
       //================================================================
       // getcap - Get FDCAP
       //================================================================
    
       Begsr getcap;

          Chain caky pca01;
          *IN90 = NOT %FOUND;
    
       Endsr;
    
       //================================================================
       // getdhp - Get FDDHP - Physican
       //================================================================
    
       Begsr getdhp;
    
          kkdseq = cadseq;

          Chain ddky pdh01;
          *IN90 = NOT %FOUND;
    
       Endsr;
    
       //================================================================
       // geteup - Get FAEUP- Hospital Header
       //================================================================
    
       Begsr geteup;
    
          Chain caky peu01;
          *IN90 = NOT %FOUND;
    
       Endsr;
    
       //================================================================
       // getdup - Get FDDUP - Hospital
       //================================================================
    
       Begsr getdup;
    
          kkdseq = cadseq;

          Chain ddky pdu01;
          *IN90 = NOT %FOUND;
    
       Endsr;
    
       //================================================================
       // getddp - Get FDDHP - Dental
       //================================================================
    
       Begsr getddp;
    
          kkdseq = cadseq;

          Chain ddky pdd01;
          *IN90 = NOT %FOUND;
    
       Endsr;
    
       //================================================================
       // getdpp - Get FDDPP - Pharmacy
       //================================================================
    
       Begsr getdpp;
    
          kkdseq = cadseq;

          Chain ddky pdp01;
          *IN90 = NOT %FOUND;
    
       Endsr;
    
       //================================================================
       // edtphy - Edit Physician billtype
       //================================================================
    
       Begsr edtphy;
    
          // Get physician detail
    
          Exsr getdhp;

          // Type of Service must be valid

          If DHtos <> *blanks;
             kkkey = DHtos;
             Setll (eoco#:kkkey) pts02;
             If not %equal (fttsl1);
                ERRcode = 'T01';
                ERRfld = 'DHTOS';
                ERRvalu = DHtos;
                Exsr wrterr;
             Endif;
          Endif;
    
          // From-date must not be zero, validate.
    
          If CAfdat = *zeros;
             ERRcode = 'E01';
             ERRfld = 'CAFDAT';
             ERRvalu = *blanks;
             exsr wrterr;
          Else;

             srdti = CAfdat;
             srdto = *blanks;
             srerr = *blanks;
      /END-FREE
     C                   call      'SRCVTDAT'    datck2
      /FREE
             If srerr =  '1';
                ERRcode = 'E02';
                ERRfld = 'CAFDAT';
      /END-FREE
     C                   move      CAfdat        ERRvalu
      /FREE
                Exsr wrterr;
             Else;
    
                // From-date cannot be less than accident date
    
                If CAfdat < EOacdt;
                   ERRcode = 'E03';
                   ERRfld = 'CAFDAT';
      /END-FREE
     C                   move      CAfdat        ERRvalu
      /FREE
                   Exsr wrterr;
                Endif;
    
                // From-date cannot be a future date
                // (based on flag SFBPF/FBAL04 postion 8, Y = allow future)
    
                If ##sdts <> 'Y';
                   If CAfdat > curdt70;
                      ERRcode = 'E03';
                      ERRfld = 'CAFDAT';
      /END-FREE
     C                   move      CAfdat        ERRvalu
      /FREE
                      Exsr wrterr;
                   Endif;
                Endif;

             Endif;
          Endif;
    
          // Validate place of service
    
          kkco# = ##co#;
      /END-FREE
     C                   Move      DHpos         kkpscd
      /FREE
          Chain psky pps01;
          *IN99 = NOT %FOUND;
          If (*in99 = *on) or
                 (*in99 = *off and PSstat = 'I');
             ERRcode = 'E04';
             ERRfld = 'DHPOS';
             ERRvalu = DHpos;
             Exsr wrterr;
          Endif;
    
          // Procedure code must be present and valid

          If DHprcd = *blanks;
             ERRcode = 'Z01';
             ERRfld = 'DHPRCD';
             ERRvalu = DHprcd;
             exsr wrterr;
          Else;
             Setgt (EOco#:DHprcd:CAfdat) ppr01;
             Readpe (EOco#:DHprcd) ppr01;
             If %Eof (fmprl);
                ERRcode = 'Z01';
                ERRfld = 'DHPRCD';
                ERRvalu = DHprcd;
                Exsr wrterr;
             Endif;
          Endif;

          // Billed Procedure code must be present and valid

          If DHbprc = *blanks;
             ERRcode = 'Z01';
             ERRfld = 'DHBPRC';
             ERRvalu = DHbprc;
             Exsr wrterr;
          Else;
             Setgt (EOco#:DHbprc:CAfdat) ppr01;
             Readpe (EOco#:DHbprc) ppr01;
             If %Eof (fmprl);
                ERRcode = 'Z01';
                ERRfld = 'DHBPRC';
                ERRvalu = DHbprc;
                Exsr wrterr;
             Endif;
          Endif;

          // Body Area valid

          If DHarea <> *blanks;
             kkkey = DHarea;
             Setll (EOco#:kkkey) ppa01;
             If Not %Equal (ftabl);
                ERRcode = 'T01';
                ERRfld = 'DHAREA';
                ERRvalu = dharea;
                Exsr wrterr;
             Endif;
          Endif;

          // Validate modifier 1
    
          If DHmod1 <> *blanks;
             kkkey = DHmod1;
             Chain tbky ppm01;
             *IN99 = NOT %FOUND;
             If (*in99 = *on) or
                    (*in99 = *off and pmstat = 'I');
                ERRcode = 'E04';
                ERRfld = 'DHMOD1';
                ERRvalu = DHmod1;
                Exsr wrterr;
             Endif;
          Endif;

          // Validate modifier 2
    
          If DHmod2 <> *blanks;
             kkkey = DHmod2;
             Chain tbky ppm01;
             *IN99 = NOT %FOUND;
             If (*in99 = *on) or
                    (*in99 = *off and PMstat = 'I');
                ERRcode = 'E04';
                ERRfld = 'DHMOD2';
                ERRvalu = DHmod2;
                Exsr wrterr;
             Endif;
          Endif;

          // Validate modifier 3
    
          If DHmod3 <> *blanks;
             kkkey = DHmod3;
             Chain tbky ppm01;
             *IN99 = NOT %FOUND;
             If (*in99 = *on) or
                    (*in99 = *off and pmstat = 'I');
                ERRcode = 'E04';
                ERRfld = 'DHMOD3';
                ERRvalu = DHmod3;
                Exsr wrterr;
             Endif;
          Endif;

          // Validate modifier 4
    
          If DHmod4 <> *blanks;
             kkkey = DHmod4;
             Chain tbky ppm01;
             *IN99 = NOT %FOUND;
             If (*in99 = *on) or
                    (*in99 = *off and PMstat = 'I');
                ERRcode = 'E04';
                ERRfld = 'DHMOD4';
                ERRvalu = DHmod4;
                Exsr wrterr;
             Endif;
          Endif;

          // One of the diagnostic cross-references field must be selected with an 'X'
    
          If DHdxr1 <> 'X'  and
                 DHdxr2 <> 'X'  and
                 DHdxr3 <> 'X'  and
                 DHdxr4 <> 'X';
             ERRcode = 'E05';
             ERRfld = 'DHDXR1';
             ERRvalu = DHdxr1;
             Exsr wrterr;
             ERRcode = 'E05';
             ERRfld = 'DHDXR2';
             ERRvalu = DHdxr2;
             Exsr wrterr;
             ERRcode = 'E05';
             ERRfld = 'DHDXR3';
             ERRvalu = DHdxr3;
             Exsr wrterr;
             ERRcode = 'E05';
             ERRfld = 'DHDXR4';
             ERRvalu = DHdxr4;
             Exsr wrterr;
          Endif;

          // Charge Amount must be greater than zero

          If CAchrg <= 0;
             ERRcode = 'E01';
             ERRfld = 'CACHRG';
      /END-FREE
     C                   move      CAchrg        ERRvalu
      /FREE
             Exsr wrterr;
          Endif;

          // Units of the charges must be greater than zero

          If CAunts < 1;
             ERRcode = 'E01';
             ERRfld = 'CAUNTS';
      /END-FREE
     C                   move      CAunts        ERRvalu
      /FREE
             Exsr wrterr;
          Endif;

          // Validate NDC code if present

          If DHndc <> *blanks;
             kkndc = DHndc;
             Chain rxky prx01;
             *IN99 = NOT %FOUND;
             If *in99 = *on;
                ERRcode = 'C03';
                ERRfld = 'DHNDC';
                ERRvalu = DHndc;
                Exsr wrterr;
             Endif;
          Endif;

       Endsr;
    
       //================================================================
       // edthos - Edit Hospital billtype
       //================================================================
    
       Begsr edthos;
    
          // Get hospital header
    
          Exsr geteup;
    
          // Get hospital detail
    
          Exsr getdup;
    
          // From-date must not be zero and a valid date.
    
          If CAfdat = *zeros;
             ERRcode = 'E01';
             ERRfld = 'CAFDAT';
      /END-FREE
     C                   move      CAfdat        ERRvalu
      /FREE
             Exsr wrterr;
          Else;

             srdti = CAfdat;
             srdto = *blanks;
             srerr = *blanks;
      /END-FREE
     C                   Call      'SRCVTDAT'    datck2
      /FREE
             If srerr =  '1';
                ERRcode = 'E02';
                ERRfld = 'CAFDAT';
      /END-FREE
     C                   move      CAfdat        ERRvalu
      /FREE
                Exsr wrterr;
             Else;
                // From-date cannot be a future date
                // (based on flag SFBPF/FBAL04 postion 8, Y = allow future)
    
                If ##sdts <> 'Y';
                   If CAfdat > curdt70;
                      ERRcode = 'E03';
                      ERRfld = 'CAFDAT';
      /END-FREE
     C                   move      CAfdat        ERRvalu
      /FREE
                      Exsr wrterr;
                   Endif;
                Endif;

             Endif;
          Endif;
    
          // Validate place of service
    
          kkco# = ##co#;
      /END-FREE
     C                   move      DUpos         kkpscd
      /FREE
          Chain psky pps01;
          *IN99 = NOT %FOUND;
          If (*in99 = *on) or
                 (*in99 = *off and PSstat = 'I');
             ERRcode = 'E04';
             ERRfld = 'DUPOS';
             ERRvalu = DUpos;
             Exsr wrterr;
          Endif;
    
          // Must have at least one: Revenue code / HCPCS code
    
          If DUrvcd = *zeros and DUrate = *blanks;
             ERRcode = 'E01';
             ERRfld = 'DURVCD';
      /END-FREE
     C                   move      DUrvcd        ERRvalu
      /FREE
             Exsr wrterr;
          Endif;
    
          // The Code must be valid
    
          If DUrate <> *blanks;
             Setgt (DUrate:CAfdat) pap01;
             Readpe (DUrate) pap01;
             If %Eof (ftapl);
                // check in fmmgp
                kktier = '0';
                Setgt (DUrate:kktier:CAfdat) pmg01;
                Readpe (DUrate:kktier) pmg01;
                If %Eof (fmmgl1);
                   ERRcode = 'C05';
                   ERRfld = 'DURATE';
      /END-FREE
     C                   move      DUrate        ERRvalu
      /FREE
                   Exsr wrterr;
                Endif;
             Endif;
          Endif;
    
          // The Description must be entered
    
          If DUdesc = *blanks;
             ERRcode = 'S01';
             ERRfld = 'DUDESC';
      /END-FREE
     C                   move      DUdesc        ERRvalu
      /FREE
             Exsr wrterr;
          Endif;
    
          // Validate procedure modifier
    
          If DUpmod <> *blanks;
             kkkey = DUpmod;
             Chain tbky PPM01;
             *IN99 = NOT %FOUND;
             If *in99 = *on;
                ERRcode = 'E04';
                ERRfld = 'DUPMOD';
                ERRvalu = DUpmod;
                Exsr wrterr;
             Endif;
          Endif;
    
          // Modifier 1 should be valid
    
          If DUmod2 <> *blanks;
             kkkey = DUmod2;
             Chain tbky PPM01;
             *IN99 = NOT %FOUND;
             If *in99 = *on;
                ERRcode = 'M02';
                ERRfld = 'DUMOD2';
                ERRvalu = DUmod2;
                Exsr wrterr;
             Endif;
          Endif;

          // Charge Amount must be greater than zero

          If CAchrg <= 0;
             ERRcode = 'E01';
             ERRfld = 'CACHRG';
      /END-FREE
     C                   move      CAchrg        ERRvalu
      /FREE
             Exsr wrterr;
          Endif;

          // Units should be greater than 0
          If DUunts < 1;
             ERRcode = 'U01';
             ERRfld = 'DUUNTS';
      /END-FREE
     C                   move      DUunts        ERRvalu
      /FREE
             Exsr wrterr;
          Endif;
    
       Endsr;
    
       //================================================================
       // edtden - Edit Dental billtype
       //================================================================
    
       Begsr edtden;
    
          // Get dental detail
    
          Exsr getddp;
    
          // From-date must not be zero and should be a valid date.
    
          If CAfdat = *zeros;
             ERRcode = 'A01';
             ERRfld = 'CAFDAT';
      /END-FREE
     C                   move      CAfdat        ERRvalu
      /FREE
             Exsr wrterr;
          Else;

             srdti = CAfdat;
             srdto = *blanks;
             srerr = *blanks;
      /END-FREE
     C                   Call      'SRCVTDAT'    datck2
      /FREE
             If srerr =  '1';
                ERRcode = 'E02';
                ERRfld = 'CAFDAT';
      /END-FREE
     C                   move      CAfdat        ERRvalu
      /FREE
                Exsr wrterr;
             Else;

                // From-date cannot be a future date
                // (based on flag SFBPF/FBAL04 postion 8, Y = allow future)
    
                If ##sdts <> 'Y';
                   If CAfdat > curdt70;
                      ERRcode = 'E03';
                      ERRfld = 'CAFDAT';
      /END-FREE
     C                   move      CAfdat        ERRvalu
      /FREE
                      Exsr wrterr;
                   Endif;
                Endif;
    
                // Validate place of service
    
                kkco# = ##co#;
      /END-FREE
     C                   move      DDpos         kkpscd
      /FREE
                Chain psky pps01;
                *IN99 = NOT %FOUND;
                If (*in99 = *on) or
                       (*in99 = *off and PSSTAT = 'I');
                   ERRcode = 'E04';
                   ERRfld = 'DDPOS';
                   ERRvalu = DDpos;
                   Exsr wrterr;
                Endif;
             Endif;
          Endif;
    
          // Procedure code cannot be blanks

          If DDprcd = *blanks;
             ERRcode = 'E01';
             ERRfld = 'DDPRCD';
             ERRvalu = DDprcd;
             Exsr wrterr;
          Endif;
    
          // Procedure code must be valid

          If DDprcd <> *blanks;
             Setgt (EOco#:DDprcd:CAfdat) ppr01;
             Readpe (EOco#:DDprcd) ppr01;
             If %Eof (fmprl);
                ERRcode = 'Z02';
                ERRfld = 'DDPRCD';
                ERRvalu = ddprcd;
                Exsr wrterr;
             Endif;
          Endif;
    
          // Billed code must be valid

          If DDbprc <> *blanks;
             Setgt (EOco#:DDbprc:CAfdat) ppr01;
             Readpe (EOco#:DDbprc) ppr01;
             If %eof (fmprl);
                ERRcode = 'Z02';
                ERRfld = 'DDBPRC';
                ERRvalu = DDbprc;
                Exsr wrterr;
             Endif;
          Endif;
    
          // Charge Amount must be greater than zero

          If CAchrg <= 0;
             ERRcode = 'E01';
             ERRfld = 'CACHRG';
      /END-FREE
     C                   move      CAchrg        ERRvalu
      /FREE
             Exsr wrterr;
          Endif;

          // Validate Tooth numbers

          If DDpart <> *blanks;
             kkkey1 = DDpart;
             kktbid = 'TOTH';
             Chain(n) gtky pgt01;
             If Not %found(ftgtl);
                ERRcode = 'Z02';
                ERRfld = 'DDPART';
                ERRvalu = DDpart;
                Exsr wrterr;
             Endif;
          Endif;

          If DDtoto <> *blanks;
             kkkey1 = DDtoto;
             kktbid = 'TOTH';
             Chain(n) gtky pgt01;
             If not %found(ftgtl);
                ERRcode = 'Z02';
                ERRfld = 'DDTOTO';
                ERRvalu = DDtoto;
                Exsr wrterr;
             Endif;
          Endif;

          If DDto01 <> *blanks;
             kkkey1 = DDto01;
             kktbid = 'TOTH';
             Chain(n) gtky pgt01;
             If Not %found(ftgtl);
                ERRcode = 'Z02';
                ERRfld = 'DDTO01';
                ERRvalu = DDto01;
                Exsr wrterr;
             Endif;
          Endif;

          If DDto02 <> *blanks;
             kkkey1 = DDto02;
             kktbid = 'TOTH';
             Chain(n) gtky pgt01;
             If not %found(ftgtl);
                ERRcode = 'Z02';
                ERRfld = 'DDTO02';
                ERRvalu = DDto02;
                Exsr wrterr;
             Endif;
          Endif;

          If DDto03 <> *blanks;
             kkkey1 = DDto03;
             kktbid = 'TOTH';
             Chain(n) gtky pgt01;
             If Not %found(ftgtl);
                ERRcode = 'Z02';
                ERRfld = 'DDTO03';
                ERRvalu = DDto03;
                Exsr wrterr;
             Endif;
          Endif;

          If DDto04 <> *blanks;
             kkkey1 = DDto04;
             kktbid = 'TOTH';
             Chain(n) gtky pgt01;
             If Not %found(ftgtl);
                ERRcode = 'Z02';
                ERRfld = 'DDTO04';
                ERRvalu = DDto04;
                Exsr wrterr;
             Endif;
          Endif;

          If DDto05 <> *blanks;
             kkkey1 = DDto05;
             kktbid = 'TOTH';
             Chain(n) gtky pgt01;
             If Not %found(ftgtl);
                ERRcode = 'Z02';
                ERRfld = 'DDTO05';
                ERRvalu = DDto05;
                Exsr wrterr;
             Endif;
          Endif;

          If DDto06 <> *blanks;
             kkkey1 = DDto06;
             kktbid = 'TOTH';
             Chain(n) gtky pgt01;
             If not %found(ftgtl);
                ERRcode = 'Z02';
                ERRfld = 'DDTO06';
                ERRvalu = DDto06;
                Exsr wrterr;
             Endif;
          Endif;

          If DDto07 <> *blanks;
             kkkey1 = DDto07;
             kktbid = 'TOTH';
             Chain(n) gtky pgt01;
             If not %found(ftgtl);
                ERRcode = 'Z02';
                ERRfld = 'DDTO07';
                ERRvalu = DDto07;
                Exsr wrterr;
             Endif;
          Endif;

          If DDto08 <> *blanks;
             kkkey1 = DDto08;
             kktbid = 'TOTH';
             chain(n) gtky pgt01;
             if not %found(ftgtl);
                ERRcode = 'Z02';
                ERRfld = 'DDTO08';
                ERRvalu = DDto08;
                Exsr wrterr;
             Endif;
          Endif;

          If DDto09 <> *blanks;
             kkkey1 = DDto09;
             kktbid = 'TOTH';
             Chain(n) gtky pgt01;
             If Not %found(ftgtl);
                ERRcode = 'Z02';
                ERRfld = 'DDTO09';
                ERRvalu = DDto09;
                Exsr wrterr;
             Endif;
          Endif;

          If DDto10 <> *blanks;
             kkkey1 = DDto10;
             kktbid = 'TOTH';
             Chain(n) gtky pgt01;
             If Not %found(ftgtl);
                ERRcode = 'Z02';
                ERRfld = 'DDTO10';
                ERRvalu = DDto10;
                Exsr wrterr;
             Endif;
          Endif;
    
       Endsr;
    
       //================================================================
       // edtpha - Edit Pharmacy billtype
       //================================================================
    
       Begsr edtpha;
    
          // Get pharmacy detail
    
          Exsr getdpp;
    
          // From-date must not be zero, validate.
    
          If CAfdat = *zeros;
             ERRcode = 'E01';
             ERRfld = 'CAFDAT';
             ERRvalu = *blanks;
             exsr wrterr;
          Else;

             srdti = CAfdat;
             srdto = *blanks;
             srerr = *blanks;
      /END-FREE
     C                   call      'SRCVTDAT'    datck2
      /FREE
             If srerr =  '1';
                ERRcode = 'E02';
                ERRfld = 'CAFDAT';
      /END-FREE
     C                   move      CAfdat        ERRvalu
      /FREE
                Exsr wrterr;
             Else;
    
                // From-date cannot be a future date
                // (based on flag SFBPF/FBAL04 postion 8, Y = allow future)
    
                If ##sdts <> 'Y';
                   If CAfdat > curdt70;
                      ERRcode = 'E03';
                      ERRfld = 'CAFDAT';
      /END-FREE
     C                   move      CAfdat        ERRvalu
      /FREE
                      Exsr wrterr;
                   Endif;
                Endif;

             Endif;
          Endif;
    
          // Fill-date must not be zero and a valid date.
    
          If DPrxfd = *zeros;
             ERRcode = 'A01';
             ERRfld = 'DPRXFD';
      /END-FREE
     C                   move      DPrxfd        ERRvalu
      /FREE
             Exsr wrterr;
          Else;

             srdti = DPrxfd;
             srdto = *blanks;
             srerr = *blanks;
      /END-FREE
     C                   Call      'SRCVTDAT'    datck2
      /FREE
             If srerr =  '1';
                ERRcode = 'A02';
                ERRfld = 'DPRXFD';
      /END-FREE
     C                   move      DPrxfd        ERRvalu
      /FREE
                Exsr wrterr;
             Else;
    
                // Fill-date cannot be less accident date
    
                If dprxfd < eoacdt;
                   ERRcode = 'E03';
                   ERRfld = 'DPRXFD';
      /END-FREE
     C                   move      DPrxfd        ERRvalu
      /FREE
                   Exsr wrterr;
                Endif;
             Endif;
          Endif;
    
          // Validate NDC code

          If DPndc = *blanks;
             ERRcode = 'E01';
             ERRfld = 'DPNDC';
             ERRvalu = DPndc;
             Exsr wrterr;
          Else;
             kkndc = DPndc;
             Chain rxky prx01;
             *IN99 = NOT %FOUND;
             If *in99 = *on;
                ERRcode = 'E04';
                ERRfld = 'DPNDC';
                ERRvalu = DPndc;
                Exsr wrterr;
             Endif;
          Endif;

          // Description must be enteted

          If DPdesc = *blanks;
             ERRcode = 'S01';
             ERRfld = 'DPDESC';
             ERRvalu = DPdesc;
             Exsr wrterr;
          Endif;

          // Units must be entered

          If DPunts < 1;
             ERRcode = 'U01';
             ERRfld = 'DPUNTS';
             ERRvalu = %char(DPunts);
             Exsr wrterr;
          Endif;

          // Days Supply must be entered

          If DPrxdy < 1;
             ERRcode = 'T01';
             ERRfld = 'DPRXDY';
             ERRvalu = %char(DPrxdy);
             Exsr wrterr;
          Endif;
    
          // Charge Amount must be greater than zero

          If CAchrg <= 0;
             ERRcode = 'E01';
             ERRfld = 'CACHRG';
      /END-FREE
     C                   move      CAchrg        ERRvalu
      /FREE
             Exsr wrterr;
          Endif;

          // FLORIDA specific edits

          If EOast = 'FL';                                                    //beg FL specific

             //   RX prescription number cannot be blank

             If DPrx# = *blanks;
                ERRcode = 'A01';
                ERRfld = 'DPRX#';
                ERRvalu = DPrx#;
                Exsr wrterr;
             Endif;

             // Days supply cannot be zero

             If DPrxdy <= *zeros;
                ERRcode = 'A01';
                ERRfld = 'DPRXDY';
      /END-FREE
     C                   move      DPrxdy        ERRvalu
      /FREE
                Exsr wrterr;
             Endif;

          Endif;                                                              //end FL specific

       Endsr;
    
       //================================================================
       // ovrfile - Override files to state specific
       //================================================================
    
       Begsr ovrfile;

         // if open close file
          if %open(FMPRL);
             close FMPRL;
          endif;
         // delete existing override
          inCmdlen = 56;
          inCmd = QCA(1);
          monitor;
           callp RunCommand(inCmd:inCmdLen);
          on-error;
          endmon;
         // override with state file
          @@cmd = qca(2);
          @@st  = EOast;
          inCmd = @@cmd;
          monitor;
           callp RunCommand(inCmd:inCmdLen);
          // open file for use
           if not %open(FMPRL);
              open FMPRL;
           endif;
          on-error;
          endmon;

         // if open close file
          if %open(FTPSL);
             close FTPSL;
          endif;
         // delete existing override
          inCmdlen = 56;
          inCmd = QCA(3);
          monitor;
           callp RunCommand(inCmd:inCmdLen);
          on-error;
          endmon;
         // override with state file
          @@cmd = qca(4);
          @@st  = EOast;
          inCmd = @@cmd;
          monitor;
           callp RunCommand(inCmd:inCmdLen);
          // open file for use
           if not %open(FTPSL);
              open FTPSL;
           endif;
          on-error;
          endmon;

         // if open close file
          if %open(FTABL);
             close FTABL;
          endif;
         // delete existing override
          inCmdlen = 56;
          inCmd = QCA(5);
          monitor;
           callp RunCommand(inCmd:inCmdLen);
          on-error;
          endmon;
         // override with state file
          @@cmd = qca(6);
          @@st  = EOast;
          inCmd = @@cmd;
          monitor;
           callp RunCommand(inCmd:inCmdLen);
          // open file for use
           if not %open(FTABL);
              open FTABL;
           endif;
          on-error;
          endmon;

       Endsr;

       //================================================================
       // wrterr - Write error to FDELP
       //================================================================

       Begsr wrterr;

          isBErr = *On;
          ELeob# = CAeob#;
          ELdseq = CAdseq;
          ELlin# = CAlin#;
          ELast  = EOast;
          ELbtyp = EObtyp;
          ELcode = ERRcode;
          ELfld  = ERRfld;
          ELvalu = ERRvalu;
          ELnval = *blanks;
          ELprfl = *blanks;
          ELprdt = *loval;
          ELupld = *loval;
          ELstat = CASTAT;
          ELuser = *blanks;
          ELpgm  = *blanks;
          ELtime = *loval;
          ELdt   = *loval;
          ELiUSR = q1user;
          ELipgm = q1pgm;
      /END-FREE
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
      /FREE
          srdto = *blanks;
          srerr = *blanks;
      /END-FREE
     C                   call      'SRCVTDAT'    dtcymd
      /FREE
          ELitim = dstime;
      /END-FREE
     C                   movel     srdto         ELidt

      /FREE
          Write pel01;

       Endsr;

      /END-FREE

       //================================================================

**  QCMDEXC TABLE
DLTOVR FILE(FMPRL)
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
DLTOVR FILE(FTPSL)
OVRDBF FILE(FTPSL) TOFILE(*LIBL/FTPSL  )
DLTOVR FILE(FTABL)
OVRDBF FILE(FTABL) TOFILE(*LIBL/FTABL  )
