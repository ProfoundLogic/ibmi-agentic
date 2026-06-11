     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H/TITLE W8311R3 - DCI Extract Selection 09/2009                  *
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION: DCI Data Export Sampler                  *
     H*                                                               *
     H* CALLED BY: W8311R1                                            *
     H*                                                               *
     H* CALLS TO: NONE                                                *
     H*                                                               *
     H* CREATED: 03/08/11  C:220 PROJECT WC1987                       *
     H*                    C:191                                      *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/10/16 C:062  Support: 41403(133)                           *
     H*                 Process death and total disability if         *
     H*                 omitted.                                      *
     H*                                                               *
     H* 10/23/15 C:062  Support: 37929(133)                           *
     H*                 If indemnity check to see if thee are comp    *
     H*                 reserves.                                     *
     H*                                                               *
     H* 05/14/15 C:062  Support: 33002(133)                           *
     H*                 Skip items that were marked as omitted.       *
     H*                                                               *
     H* 08/25/14 C:283  Support: WC133 85974                          *
     H*                 Reject the already Closed and Reported claims.*
     H*                                                               *
     H* 05/01/14 C:279  Project: WC2174                               *
     H*                 Exclude Non-compensable and fraudulent claims *
     H*                 from reporting.                               *
     H*                                                               *
     H* 09/12/12 C:283  Support: WC133 80615                          *
     H*                 Change the logic to always report a Claim     *
     H*                 which has been initially reported.            *
     H*                 Claim having the Last Reported Level > 0      *
     H*                 should not undergo sampling by Random number  *
     H*                 and should be selected for DCI extract.       *
     H*                                                               *
     H*****************************************************************
    F*  Primary logical file definition
     Fwmcml1    if   e           k disk
     Fwxf1l     if   e           k disk
     F***** Beg Chg ***** 09/12/12 *****************************************
     Fwddil1    if   e           k disk
     F***** End Chg ***** 09/12/12 *****************************************
      ***** beg add ***** 05/01/14 *****************************************
     fwmlll     if   e           k disk    rename(pll01:pll00)
      ***** end add ***** 05/01/14 *****************************************
     F*** Begin Add *** 10/23/15 *******************************************
     fwtabl     if   e           k disk    rename(prt01:prt00) include(prt01)
     F*** End   Add *** 10/23/15 *******************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields
    D*---------------------------------------------------------------------
      *
     D xxrnd           S              3  0
     D xxrtop          S              3  0
     D xxrtcl          S              3  0
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
     C***** Beg Chg ***** 09/12/12 *****************************************
     C* Key list for DCI Claim History File
     C     diky          klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
     C*
     C***** End Chg ***** 09/12/12 *****************************************
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
     C* Perform Sampling
     C                   exsr      srSampling
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
    C*  srSampling - Sampling Process
    C*================================================================
     C     srSampling    begsr
     C*
      /free
       //****** beg chg ******** 05/01/14 ***********************************

       // Check in the DCI Claim History file if the Claim has ever been reported
       // earlier, then select that claim without any more validations and leave
       // the subroutine. DIRPLV gives the last reporting level of the Claim.

       Chain diky pdi01;
       If %found(wddil1);
          //*** Begin Add *** 05/14/15 **************************************
          // Skip items that have been marked as omitted
          If DIstat = 'O';
             //*** Begin Chg *** 05/10/16 ********************************
             //**********leaveSr;
             If CMncci <> '01' And CMncci <> '02';
                LeaveSR;
             EndIf;
             //*** End   Chg *** 05/10/16 ********************************
          Endif;
          //*** End   Add *** 05/14/15 **************************************
          If DIrplv > 0;
             xxSamp = 'S';
          Endif;
       //****** beg add ******** 08/25/14 ***********************************
          If DIstat = 'C' and DIrplv > 0;
             xxStat = 'C';
             xxSamp = 'N';
             leaveSr;
          Endif;
       //****** end add ******** 08/25/14 ***********************************
       Endif;

       Chain diky pll00;
       If %found(wmlll) and (LLfrau ='02' or LLmtst = '05');
          If xxSamp = 'S';
             xxStat = 'D';
          Else;
             xxStat = 'O';
          Endif;
          leaveSr;
       Endif;
       //****** end chg ******** 05/01/14 ***********************************
      /end-free
      *
     C* Set Omit flag
     C*----- If Claim is medical only
     C*** Begin Add *** 10/23/15 ***************************************
     C* See if Indenity claim has Compensation Reserves.
     C                   ExSr      CheckComp
     C*** End   Add *** 10/23/15 ***************************************
     C                   if        cmltfl = 'M'
     C                   eval      xxstat = 'O'
     C                   leavesr
     C                   endif                                                  cmltfl = 'M'
     C*
     C*----- USL&H claim
     C                   if        cmuslh = 'Y'
     C                   eval      xxstat = 'O'
     C                   leavesr
     C                   endif                                                  cmuslg = 'Y'
     C*
     C                   eval      xxpdci = 'Y'
     C*
     C*----- All death and permanent total disability claims are required to be reported
     C*----- and should not be subject to sampling
     C                   if        cmncci = '01' or cmncci = '02'
     C                   eval      xxstat = 'S'
     C                   leavesr
     C                   endif                                                  cmncci = '01'
     C*
     C* For TX state select all open claims
     C                   if        cmjrst = 'TX' and cmstat = 'O'
     C                   eval      xxstat = 'S'
     C                   leavesr
     C                   endif                                                  cmstat = 'O'
      ****** beg chg ********** 05/01/14 ***********************************
     C***** Beg Chg ***** 09/12/12 *****************************************
      *** Check in the DCI Claim History file if the Claim has ever been reported
      *** earlier, then select that claim without any more validations and leave
      *** the subroutine. DIRPLV gives the last reporting level of the Claim.
      ***
     C*****diky          chain     pdi01
     C*****              if        %found(wddil1)
      *
     C*****              if        dirplv > 0
     C*****              eval      xxstat = 'S'
     C*****              eval      xxsamp = 'S'
     C*****              leavesr
     C*****              endif                                                  dirplv > 0
      *
      /free
       If xxSamp = 'S';
          xxStat = 'S';
          leaveSr;
       //****** end chg ********** 05/01/14 ***********************************
      /end-free
     C                   endif                                                  %found(wddil1)
     C***** End Chg ***** 09/12/12 *****************************************
     C
     C* Generate and assign random number
     C                   exsr      srGenRnd
     C*
     C* State sampling ratio
     C     f1ky          chain     pf101
     C*----- Check random number against state sampling ratio
     C                   if        not %found
     C                   leavesr
     C                   else
     C*
     C* The sampling ratio is multiplied by 1000 and the result is subtracted by 1
     C* e.g. 30% times 1000 minus 1 is 299 (0.30*1000=300-1=299)
     C* so claims with random number between 000 - 299 will be selected
     C*
     C                   eval      xxrtop = (f1rtop * 10) - 1
     C                   eval      xxrtcl = (f1rtcl * 10) - 1
     C*
     C* TX open claim should not reach here, still adding condition for clarity
     C                   if        cmstat = 'O' and xxrnd <= xxrtop
     C                             and cmjrst <>'TX'
     C                   eval      xxstat = 'S'
     C                   eval      xxsamp = 'S'
     C                   leavesr
     C                   endif
     C
     C                   if        cmstat = 'C' and xxrnd <= xxrtcl
     C                   eval      xxstat = 'S'
     C                   eval      xxsamp = 'S'
     C                   leavesr
     C                   endif
     C*
     C                   endif                                                  not %found
     C*
     C                   endsr
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
     C*
     C                   endsr
