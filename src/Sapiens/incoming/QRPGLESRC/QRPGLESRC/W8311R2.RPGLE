     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H/TITLE W8311R - DCI Data Export Sampler                         *
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION: DCI Data Export Sampler                  *
     H*                                                               *
     H* CALLED BY: W8311R1                                            *
     H*                                                               *
     H* CALLS TO: NONE                                                *
     H*                                                               *
     H* CREATED: 05/21/09  C:191 PROJECT WC1792                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/23/15 C:062  Support: 37929(133)                           *
     H*                 If indemnity check to see if thee are comp    *
     H*                 reserves.                                     *
     H*                                                               *
     H*****************************************************************
    F*  Primary logical file definition
     Fwmcml1    if   e           k disk
     Fwxf1l     if   e           k disk
     Fwdpal     if   e           k disk
     Fwtabl     if   e           k disk
     F                                     ignore(PAT01)
     F                                     ignore(PSP01)
     F                                     ignore(PNC01)
     F                                     ignore(PNA01)
     F                                     ignore(PPA01)
     F                                     ignore(PCA01)
     F                                     ignore(PRC01)
     F*** Begin Del *** 10/23/15 ******************************
     F**********                           ignore(PRT01)
     F*** End   Del *** 10/23/15 ******************************
     F                                     ignore(PPC01)
     F*************************************ignore(PB201)
     F                                     ignore(PB701)
     F                                     ignore(PBS01)
     F                                     ignore(PTT01)
     F                                     ignore(PCC01)
     F                                     ignore(PBC01)
     F                                     ignore(PCS01)
     F                                     ignore(PCN01)
     F                                     ignore(PRS01)
     F                                     ignore(PLR01)
     F                                     ignore(PVS01)
     F                                     ignore(PTR01)
     F                                     ignore(PHY01)
     F                                     ignore(PSY01)
     F                                     ignore(PBA01)
     F                                     ignore(PID01)
     F                                     ignore(PIS01)
     F*** Begin Add *** 10/23/15 ******************************
     F                                     Rename(prt01:prt00)
     F*** End   Add *** 10/23/15 ******************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields
    D*---------------------------------------------------------------------
      *
     D xxrnd           S              3  0
     D xxrnd2          S              2  0
     D xxrnd1          S              1  0
     D xxkey           S             10
     D xxseed          S                   like(stdint) inz(0)
     D*
     D StdPacked       S             15P 5                                      Packed
     D StdInt          S             10I 0                                      Integer
     D StdDouble       S              8F                                        Double Precision
     D StdFC           S             12A                                        Feedback Code
     D seednum         S                   LIKE(StdInt)
     D randnum         S                   LIKE(StdDouble)
     D feedbackcode    S                   LIKE(StdFC)
     D*** Begin Add *** 10/23/15 *******************************************
     D i               s              2S 0
     D*** End   Add *** 10/23/15 *******************************************
      *
    D*---------------------------------------------------------------------
    D* Data Structures
    D*---------------------------------------------------------------------
    D*
     D xxflags         ds             6
     D  xxstat                        1    overlay(xxflags:1)
     D  xxpdci                        1    overlay(xxflags:2)
     D  xxopni                        1    overlay(xxflags:3)
     D  xxcnpt                        1    overlay(xxflags:4)
     D  xxcpnt                        1    overlay(xxflags:5)
     D  xxsamp                        1    overlay(xxflags:6)
     D*
     D*** Begin Add *** 10/23/15 ***************************************
     D                 ds
     D arrRT                   1     20    dim(10)
     D  CMrt01                 1      2
     D  CMrt02                 3      4
     D  CMrt03                 5      6
     D  CMrt04                 7      8
     D  CMrt05                 9     10
     D  CMrt06                11     12
     D  CMrt07                13     14
     D  CMrt08                15     16
     D  CMrt09                17     18
     D  CMrt10                19     20
     D*
     D* Reserve change
     D* Payment remaining reserve
     D                 ds
     D arrRR                   1     90  2 dim(10)
     D  CMrr01                 1      9  2
     D  CMrr02                10     18  2
     D  CMrr03                19     27  2
     D  CMrr04                28     36  2
     D  CMrr05                37     45  2
     D  CMrr06                46     54  2
     D  CMrr07                55     63  2
     D  CMrr08                64     72  2
     D  CMrr09                73     81  2
     D  CMrr10                82     90  2
     D*
     D* Payment against reserve
     D                 ds
     D arrAR                   1     90  2 dim(10)
     D  CMar01                 1      9  2
     D  CMar02                10     18  2
     D  CMar03                19     27  2
     D  CMar04                28     36  2
     D  CMar05                37     45  2
     D  CMar06                46     54  2
     D  CMar07                55     63  2
     D  CMar08                64     72  2
     D  CMar09                73     81  2
     D  CMar10                82     90  2
     D*** End   Add *** 10/23/15 ***************************************
    D*---------------------------------------------------------------------
    D* Procedure prototype
    D*---------------------------------------------------------------------
      * Random number generator API.
      *
     D GenRndNbr       PR                  extproc('CEERAN0')
     D PrSeed                              LIKE(StdInt)
     D PrRandnum                           LIKE(StdDouble)
     D PrFC                                LIKE(StdFC)
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@co#             3
     C                   parm                    @@fnd             3
     C                   parm                    @@fyr             3
     C                   parm                    @@case            7
     C                   parm                    @@rnd             3
     C                   parm                    @@flags           6
     C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C* Key list for Claim File
     C     cmky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxcase
     C*
     C* Key list for DCI control file
     C     f1ky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    cmjrst
     C*
     C* Key list for Payment history file
     C     paky          klist
     C                   kfld                    xxco#
     C                   kfld                    cmfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
     C*
     C* Key list for PV1 table file
     C     b2ky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxkey
     C*
    C*================================================================
    C*  MAIN PROGRAM
    C*================================================================
     C                   move      @@co#         xxco#             3 0
     C                   move      @@fnd         xxfnd             3 0
     C                   move      @@fyr         xxfyr             3 0
     C                   move      @@case        xxcase            7 0
     C*
     C                   move      *zeros        @@rnd
     C                   move      *blanks       @@flags
     C*
     C* Set all flags to defaults
     C                   eval      xxstat = 'O'
     C                   eval      xxpdci = 'N'
     C                   eval      xxopni = 'N'
     C                   eval      xxcnpt = 'N'
     C                   eval      xxcpnt = 'N'
     C                   eval      xxsamp = 'N'
     C*
     C* Get Claims record
     C     cmky          chain     pcm01
     C                   if        %found
     C* Generate and assign random number
     C                   exsr      srGenRnd
     C* Perform Primary Sampling
     C                   exsr      srPriSmpl
     C* Perform Stratified Sampling
     C                   exsr      srStrSmpl
     C                   endif                                                  %found(pcm01)
     C*
     C* done process; return Sampling flags and random no.
     C                   move      xxrnd         @@rnd
     C                   move      xxflags       @@flags
     C*
     C                   seton                                        lr
     C*** Begin Add *** 10/23/15 ***************************************
    C*================================================================
    C*  CheckComp - Check to see Compensation reserves were Setup
    C*================================================================
     C     CheckComp     begsr
     C*
      /free
       // Only Check for Compensation reserves if Indemnity Claim
       If CMltFl = 'I';
          // Indemnity Claims that have no Compensation reserves
          // is to be treated as a Medical claim.
          CMltFl = 'M';
          For i = 1 to 10;
             If arrRT(i) <> ' ';
                Chain (CMco#:arrRT(i)) prt00;
                If %Found(WTABL) And RTtype = 'C';
                   If (arrRR(i) + arrAr(i)) <> 0;
                      // This is a true Indemnity Claim
                      CMltFl = 'I';
                      Leave;
                   EndIf;
                EndIf;
             EndIf;
          EndFor;
       EndIf;
      /end-free
     C                   EndSr
     C*** End   Add *** 10/23/15 ***************************************
    C*================================================================
    C*  srGenRnd - Generate Random Number
    C*================================================================
     C     srGenRnd      begsr
     C*
     C* Set seed value for random number:
     C* Seed=0; will base random number on current system time - That wud work for us! :)
     C                   eval      seednum = xxseed
      * Generate random number
     C                   callp     GenRndNbr(seednum:randnum:feedbackcode)
     C                   eval      xxseed = seednum
     C* We need in three digits (0 to 999) - for file
     C                   eval      xxrnd = %int(randnum * 1000)
     C* And last two for for comparison!
     C                   move      xxrnd         xxrnd2            2 0
     C*
     C                   endsr
    C*================================================================
    C*  srPriSmpl - Primary Sampling Process
    C*================================================================
     C     srPriSmpl     begsr
     C*
     C* Set Omit flag
     C*----- If Claim is medical only
     C*** Begin Add *** 10/23/15 ***************************************
     C* See if Indenity claim has Compensation Reserves.
     C                   ExSr      CheckComp
     C*** End   Add *** 10/23/15 ***************************************
     C                   if        cmltfl = 'M'
     C                   leavesr
     C                   endif                                                  cmltfl = 'M'
     C*
     C*----- If Claim closed with no payments
     C                   if        cmstat = 'C'
     C     paky          setll     ppa01
     C                   if        not %equal
     C                   leavesr
     C                   endif                                                  not %equal
     C                   endif                                                  cmltfl = 'M'
     C*
     C*----- USL&H claim
     C                   if        cmuslh = 'Y'
     C                   leavesr
     C                   endif                                                  cmuslg = 'Y'
     C*
     C* State sampling ratio
     C     f1ky          chain     pf101
     C*----- Check random number (last two digits) against state sampling ratio
     C                   if        not %found or xxrnd2 > f1rtio
     C                   leavesr
     C                   endif                                                  xxrnd2 > f1rtio
     C*
     C* Reached here, so selected in primary sampling - set flags
     C                   eval      xxstat = 'S'
     C                   eval      xxpdci = 'Y'
     C*
     C                   endsr
    C*================================================================
    C*  srStrSmpl- Stratified Sampling Process
    C*================================================================
     C     srStrSmpl     begsr
     C*
     C* Process only STAT = 'S' claims
     C                   if        xxstat <> 'S'
     C                   leavesr
     C                   endif                                                  xxstat <> 'S'
     C*
     C* All open claims to be included
     C                   if        cmstat = 'O'
     C                   eval      xxopni = 'Y'
     C                   leavesr
     C                   endif                                                  cmstat = 'O'
     C*
     C* for closed claims - determine benefit types associated with the payments
     C                   if        cmstat = 'C'
     C* Default to omit
     C                   eval      xxstat = 'O'
     C* Get payment record to classify as non-Temp or Temp
     C     paky          setgt     ppa01
     C                   dou       %eof(wdpal)
     C     paky          readpe    ppa01
     C                   if        %eof(wdpal)
     C                   leave
     C                   endif                                                  %eof(wdpal)
     C*
     C* Get benefit type from table file
     C                   movel(p)  pabr2         xxkey
     C     b2ky          chain     pb201
     C                   if        %found and b2bnft <> *blanks
     C*
     C*-- Classify as non-Temp Total/partial, Temp Total/partial
     C                   if        b2bnft = '05' or b2bnft = '07'
     C* Closed - Temp total/partial
     C                   eval      xxcnpt = 'Y'
     C*
     C* Select only those with 6 as first digit of random number
     C                   movel     xxrnd         xxrnd1            1 0
     C*
     C                   if        xxrnd1 = 6
     C                   eval      xxsamp = 'Y'
     C                   eval      xxstat = 'S'
     C                   endif                                                  xxrnd1=6
     C* Leave the loop we found temp
     C                   leave
     C                   endif                                                  b2bnft = '05'
     C*
     C* Closed Non-Temp total/partial - Select All
     C                   if        b2bnft='01' or b2bnft='02' or b2bnft='03'
     C                             or b2bnft='04' or b2bnft='08' or b2bnft='09'
     C                             or b2bnft='50'
     C                   eval      xxcpnt = 'Y'
     C                   eval      xxstat = 'S'
     C* Leave the loop we found non-temp
     C                   leave
     C                   endif                                                  b2bnft='01'
     C*
     C                   endif                                                  %found(pb201)
     C*
     C                   enddo                                                  %eof(wdpal)
     C*
     C                   endif                                                  cmstat = 'C'
     C*
     C                   endsr
