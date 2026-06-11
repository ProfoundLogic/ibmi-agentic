     hCopyright('(c) 2023 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR00406A - LOAD ENDORSEMENT FORM WORKFILE FOR FORM WC000406A
      //===============================================================*
      // WC000406 - Premium Discount Endorsement                       *
      //                                                               *
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //===============================================================*
      // REVISIONS:                                                    *
      // ------------------------------------------------------------- *
      //  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
      // ------------------------------------------------------------- *
      //                                                               *
      // 01/27/23 C:291 Project: WC2863                                *
      //                State Include/Exclude Endorsement Print Control*
      //                                                               *
      // 10/26/22 C:333 Project: WC2834                                *
      //                Endorsement Recognition of Cancellation        *
      //                                                               *
      // 11/30/18 C:180 Support:75049 X-REF 74036                      *
      //                - Prevent division by zero.                    *
      //                - Reflect actual policy premium discount for   *
      //                  the Avg Pct disc, not the value in the disc  *
      //                  tabl.                                        *
      //                                                               *
      // 11/06/18 C:180 Support 74036                                  *
      //                Reflect actual policy premium discount for the *
      //                Avg Pct disc, not the value in the disc table. *
      //                                                               *
      // 04/17/18 C:65  PHW#70390                                      *
      //                Pull average discount value from wdelp         *
      //                calculated discount percentage field.          *
      //                                                               *
      // 03/07/18 C:291 PHW63874                                       *
      //                Auto attach to any policy that has the discount*
      //                type assigned even if discount amount is zero. *
      //                And attach to the policy if single percentage  *
      //                is assigned and discount type is blank.        *
      //                                                               *
      // 01/19/18 C:136 WC2556                                         *
      //                Correct divide by zero issue                   *
      //                                                               *
      // 12/19/17 C:33  Support: 51563                                 *
      //               -Auto-attach if any division has premium        *
      //                 discount.                                     *
      //                                                               *
      // 11/09/17 C:267 Project: WC2556                                *
      //              - Add calculation for Average Percent            *
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      //===============================================================*
      // Notes:                                                        *
      //                                                               *
      //  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      //                                                               *
      //  QQMODE = 1 - Called to determine if form should be attached. *
      //  QQMODE = 2 - Called to print endorsement.                    *
      //  QQMODE = 3 - Called for additional input.                    *
      //  QQMODE = 4 - Called for mid-term processing.                 *
      //===============================================================*

     F/COPY SCOPYEN01F

     FWMDSL1    IF   E           K DISK
     FWXPEL     UF A E           K DISK
     FSTSTL3    IF   E           K DISK
     FWXPEL1    IF   E           K DISK
     F                                     RENAME(PPE01:PPE02)

       //***  Begin Add  ***  12/19/17  ***********************************
     D isProcessSt     s              1n   inz
       //***  End   Add  ***  12/19/17  ***********************************
     D ST              S              2    DIM(50)
     D qqFORM          s             15A   INZ('WC000406A ')
     D ADDPGM          C                   CONST('SRXXXXXXX ')

      /COPY SCOPYEN01D
      /COPY WWCPOLDAT3

      ** Limits
     D                 DS
     D  LM                     1    220  2
     D                                     DIM(20)
     D  FMAM01                 1     11  2
     D  FMAM02                12     22  2
     D  FMAM03                23     33  2
     D  FMAM04                34     44  2
     D  FMAM05                45     55  2
     D  FMAM06                56     66  2
     D  FMAM07                67     77  2
     D  FMAM08                78     88  2
     D  FMAM09                89     99  2
     D  FMAM10               100    110  2
     D  FMAM11               111    121  2
     D  FMAM12               122    132  2
     D  FMAM13               133    143  2
     D  FMAM14               144    154  2
     D  FMAM15               155    165  2
     D  FMAM16               166    176  2
     D  FMAM17               177    187  2
     D  FMAM18               188    198  2
     D  FMAM19               199    209  2
     D  FMAM20               210    220  2
      ** Percentages
     D                 DS
     D  PC                     1    100  2
     D                                     DIM(20)
     D  FMA521                 1      5  2
     D  FMA522                 6     10  2
     D  FMA523                11     15  2
     D  FMA524                16     20  2
     D  FMA525                21     25  2
     D  FMA526                26     30  2
     D  FMA527                31     35  2
     D  FMA528                36     40  2
     D  FMA529                41     45  2
     D  FMA52A                46     50  2
     D  FM5211                51     55  2
     D  FM5212                56     60  2
     D  FM5213                61     65  2
     D  FM5214                66     70  2
     D  FM5215                71     75  2
     D  FM5216                76     80  2
     D  FM5217                81     85  2
     D  FM5218                86     90  2
     D  FM5219                91     95  2
     D  FM5220                96    100  2
      ** States to print
     D                 DS
     D  SP                     1     10
     D                                     DIM(5)
     D  FMFL01                 1      1
     D  FMFL02                 2      2
     D  FMFL03                 3      3
     D  FMFL04                 4      4
     D  FMFL05                 5      5
     D  FMFL06                 6      6
     D  FMFL07                 7      7
     D  FMFL08                 8      8
     D  FMFL09                 9      9
     D  FMFL10                10     10
     D  FMCD01                 1      2
     D  FMCD02                 3      4
     D  FMCD03                 5      6
     D  FMCD04                 7      8
     D  FMCD05                 9     10
     D                 DS
     D  XXDP                   1     28  0
     D  XZDA1                  1      4  0
     D  XZDP1                  5      7  1
     D  XZDA2                  8     11  0
     D  XZDP2                 12     14  1
     D  XZDA3                 15     18  0
     D  XZDP3                 19     21  1
     D  XZDA4                 22     25  0
     D  XZDP4                 26     28  1

     D SR00406A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR00406A        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

     C     *LIKE         DEFINE    ELPFDT        K1PFDT
     C     *LIKE         DEFINE    DSEFDT        XXEFDT
     C     *LIKE         DEFINE    RSRTCD        XXRTCD
     C     *LIKE         DEFINE    DSTYPE        XXTYPE
     C     *LIKE         DEFINE    PEUNID        XXUNID
     C     *LIKE         DEFINE    PEEND#        X1END#
     C     *LIKE         DEFINE    DSPCT         wkPCT
     C     *LIKE         DEFINE    DSPCT         wkAvgPCT
     C     *LIKE         DEFINE    IX            wkCNT

      ** Stock Discount file
     C     DSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RSJRST
     C                   KFLD                    ELDISC
     C                   KFLD                    XXTYPE
     C                   KFLD                    K1PFDT

     C     DSKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RSJRST
     C                   KFLD                    ELDISC

     C     DSKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RSJRST
     C                   KFLD                    ELDISC
     C                   KFLD                    XXTYPE
     C                   KFLD                    XXEFDT
     C     PEKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXDT              7 0
     C     PEKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C     PEKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXUNID
     C                   KFLD                    X1END#
     C     STKY          KLIST
     C                   KFLD                    XXKEY            10
      * SUBMISSION HISTORY FILE
     C     SHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    X1TYPE

      /COPY SCOPYEN01C

      *=========================================================================
      *  OTHINF - Subroutine to load form specific information
      *=========================================================================
     C     OTHINF        BEGSR
      *  If printing a policy page.  (Not called from W1054R)
     C                   CALL      'SRCHKPGM'
     C                   PARM      'W1054R'      #1PGM            10
     C                   PARM      *BLANK        #1FLAG            1

     C                   CLEAR                   ST
     C                   Z-ADD     0             IX                2 0
     C                   Z-ADD     0             JX                2 0
     C                   MOVE      *ZEROS        LM
     C                   Z-ADD     *ZEROS        PC
     C                   MOVE      'A'           XXTYPE
     C                   MOVE      *BLANK        XXFRST            1
     C                   MOVE      *BLANK        XXASP3            1
     C                   CLEAR                   E00406
     C                   MOVE      *ZEROS        XXDP
     C                   MOVE      *ZEROS        XXAPD
     C                   MOVE      *ZEROS        XXDP1
     C                   MOVE      *ZEROS        XXDP2
     C                   MOVE      *ZEROS        XXDP3
     C                   MOVE      *ZEROS        XXDP4
     C                   MOVE      *ZEROS        XXDP5
     C                   MOVE      *ZEROS        XXDP6
     C                   MOVE      *ZEROS        XXST1
     C                   MOVE      *ZEROS        XXST2
     C                   MOVE      *ZEROS        XXST3
     C                   MOVE      *ZEROS        XXST4
     C                   MOVE      *ZEROS        XXST5
     C                   MOVE      *ZEROS        XXST6
     C                   Z-ADD     0             DX                2 0
     C                   MOVE      'P'           X1TYPE            1
     C                   Z-ADD     0             XXUNID

     C                   MOVE      *HIVAL        XXDT
     C     PEKEY         SETGT     PPE02
     C     PEKEY1        READPE    PPE02                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     PEUNID        XXUNID
     C                   ENDIF
      *  If it is not using the Stock Discount table, just get the
      *  percentage from employer detail.
      ***  Beg Chg  ***  03/07/18  **********************************
      ***  ELDISC        IFEQ      *BLANKS
      ***  ELEDC3        ANDNE     *ZEROS
     C                   If        ELasp3 <> *zero                              Single discount %
      ***  End Chg  ***  03/07/18  **********************************

     C                   Z-ADD     ELASP3        FM5221
     C                   MOVE      'Y'           XXASP3            1
      *  This is to force a write when done
     C                   ADD       1             IX

     C                   ELSE
      *  Check all divisions for a discount code
     C     ELKY2         CHAIN     PEL02                              60

     C     *IN60         DOWEQ     *OFF
          //***** Beg Add *** 10/26/22 ******************************
             xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
             If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
          //***** End Add *** 10/26/22 ******************************
     C     EMCAD1        IFGT      ELPFDT
     C     EMCAD1        OREQ      *ZEROS

      ***  Beg Chg  ***  03/07/18  **********************************
      ***  ELEDC3        IFNE      *ZEROS
     C                   If        ELdisc <> *blank
      ***  End Chg  ***  03/07/18  **********************************

     C                   MOVE      ELRTCD        XXRTCD

     C     RSKY          CHAIN     PRS01                              60
      //*** Beg Add ***** 01/27/23 ***********************************
       Exsr ChkApprExclSt;
       If isApprExclSt = *On;
      //*** End Add ***** 01/27/23 ***********************************
      *  If the state hasn't been found in this employer before
      *  then add it to the array and write a WXFMP record.
     C     RSJRST        LOOKUP    ST                                     30

       //***  Begin Chg  ***  12/19/17  ***********************************
     C******IN30         IFEQ      *OFF
     C*****RSJRST        ANDNE     'NJ'
     C*****RSJRST        ANDNE     'PA'
      /Free
       Exsr ChkStat;

       Select;
       When *in30;
          isProcessSt = *off;
       When attach <> 'Y';
          isProcessSt = *off;
       Other;
          isProcessSt = *on;
       Endsl;

       If isProcessSt;
      /End-free
       //***  End   Chg  ***  12/19/17  ***********************************

     C                   Z-ADD     1             I
     C     *BLANKS       LOOKUP    ST(I)                                  31
     C                   MOVEL     RSJRST        ST(I)
      *  Increment counters.
     C                   ADD       1             IX
     C     IX            MULT      4             JX
     C                   SUB       3             JX
     C                   ADD       1             DX

      *  Move state into state array.

     C                   MOVEL     RSJRST        SP(IX)

      *  Retrieve the most current effective date prior to policy

     C                   MOVE      ELDCEF        #1DATE
     C                   MOVE      ELCO#         #1CO#
     C                   MOVE      ELFND         #1FND
     C                   MOVE      ELFYR         #1FYR
     C                   MOVE      ELEMP#        #1EMP#
     C                   MOVE      ELDIV         #1DIV

     C                   CALL      'SREFFD'
     C                   PARM                    #1CO#             3
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1EMP#            9
     C                   PARM                    #1DIV             5
     C                   PARM                    #1DATE            7

     C                   MOVE      #1DATE        K1PFDT

     C     DSKY          SETGT     PDS01
     C     DSKY1         READPE    PDS01                                  60

     C     *IN60         IFEQ      *OFF
     C                   MOVE      DSEFDT        XXEFDT
     C                   ENDIF
     C     DSTYPE        IFNE      'A'
     C                   Z-ADD     ELASP3        FM5221
     C                   ENDIF
      *  Get the 1st Discount Code
     C     DSKY2         SETLL     PDS01
     C     DSKY2         READE     PDS01                                  60

     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     DSLMT         LM(JX)
     C                   Z-ADD     DSPCT         PC(JX)
     C                   Z-ADD     DSLMT         XX70              7 0
     C                   MOVEL     XX70          XZDA1
     C                   Z-ADD     DSPCT         XZDP1
     C                   EVAL      wkPCT = wkPCT + DSPCT
     C                   ADD       1             wkCNT
     C                   ADD       1             JX

      *  Get the 2nd Discount Code

     C     DSKY2         READE     PDS01                                  60

     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     DSLMT         LM(JX)
     C                   Z-ADD     DSPCT         PC(JX)
     C                   Z-ADD     DSLMT         XX70              7 0
     C                   MOVEL     XX70          XZDA2
     C                   Z-ADD     DSPCT         XZDP2
     C                   EVAL      wkPCT = wkPCT + DSPCT
     C                   ADD       1             wkCNT
     C                   ADD       1             JX

      *  Get the 3rd Discount Code

     C     DSKY2         READE     PDS01                                  60

     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     DSLMT         LM(JX)
     C                   Z-ADD     DSPCT         PC(JX)
     C                   Z-ADD     DSLMT         XX70              7 0
     C                   MOVEL     XX70          XZDA3
     C                   Z-ADD     DSPCT         XZDP3
     C                   EVAL      wkPCT = wkPCT + DSPCT
     C                   ADD       1             wkCNT
     C                   ADD       1             JX

      *  Get the 4th Discount Code

     C     DSKY2         READE     PDS01                                  60

     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     DSPCT         PC(JX)
     C                   Z-ADD     DSLMT         XX70              7 0
     C                   MOVEL     XX70          XZDA4
     C                   Z-ADD     DSPCT         XZDP4
     C                   EVAL      wkPCT = wkPCT + DSPCT
     C                   ADD       1             wkCNT
     C                   ELSE
     C                   Z-ADD     9999          XZDA4
     C                   Z-ADD     99.9          XZDP4
     C                   ENDIF

     C                   ELSE
     C                   Z-ADD     9999          XZDA3
     C                   Z-ADD     99.9          XZDP3
     C                   ENDIF

     C                   ELSE
     C                   Z-ADD     9999          XZDA2
     C                   Z-ADD     99.9          XZDP2
     C                   ENDIF

     C                   ELSE
     C                   Z-ADD     9999          XZDA1
     C                   Z-ADD     99.9          XZDP1
     C                   ENDIF
      *
      *  Calculate average percentage
      *
       //*** Begin Change *** 11/30/18 *************************************
      *** beg chg ***** 01/19/18 ***************************
     C***********        EVAL      wkAvgPCT = wkPCT / wkCNT
      /Free
         //If wkCnt <> 0;
         //   wkAvgPct = wkPct/wkCnt;
         //Else;
         //    wkAvgPct = 0;
         //Endif;
      /End-Free
      *** end chg ***** 01/19/18 ***************************
      /Free
          //C                   Z-ADD     wkAvgPCT      FM5221
          //*** Begin Change *** 11/06/18 ***************************
          //*
          //C*** begin add ***  04/17/18  *******************************
          //C                   If        ELDisp <> 0
          //C                   Z-add     ELDisp        FM5221
          //C                   Endif
          //C*** end add *****  04/17/18  *******************************
       FM5221 = ELdisp;
       //*** End   Change *** 11/06/18 ***************************
       //*** End   Change *** 11/30/18 *************************************
      /End-Free
      *
      *  If page is full, write & setup for more names
     C     DX            IFEQ      1
     C                   MOVEL(P)  RSJRST        XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      STNST         XXST1
     C                   ENDIF
     C                   MOVE      XXDP          XXDP1
     C                   ENDIF
     C     DX            IFEQ      2
     C                   MOVEL(P)  RSJRST        XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      STNST         XXST2
     C                   ENDIF
     C                   MOVE      XXDP          XXDP2
     C                   ENDIF
     C     DX            IFEQ      3
     C                   MOVEL(P)  RSJRST        XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      STNST         XXST3
     C                   ENDIF
     C                   MOVE      XXDP          XXDP3
     C                   ENDIF
     C     DX            IFEQ      4
     C                   MOVEL(P)  RSJRST        XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      STNST         XXST4
     C                   ENDIF
     C                   MOVE      XXDP          XXDP4
     C                   ENDIF
     C     DX            IFEQ      5
     C                   MOVEL(P)  RSJRST        XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      STNST         XXST5
     C                   ENDIF
     C                   MOVE      XXDP          XXDP5
     C                   ENDIF
     C     DX            IFEQ      6
     C                   MOVEL(P)  RSJRST        XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      STNST         XXST6
     C                   ENDIF
     C                   MOVE      XXDP          XXDP6
     C                   ENDIF
     C                   MOVE      *ZEROS        XXDP

     C     DX            IFEQ      6
      * Update the endorsement reporting file with the proper data.
      * Find the unique ID number.
     C                   MOVEL     XXFORM        X1END#
     C     PEKEY2        CHAIN     PPE01                              60
      * Copy data into the record.
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  E00406        PEDATA
      *  If printing a policy page.  (Not called from W1054R)
     C     #1FLAG        IFEQ      '0'

     C     XXFRST        IFEQ      *BLANK
     C                   UPDATE    PPE01
     C                   ELSE
     C                   WRITE     PPE01
     C                   ENDIF

     C                   ENDIF

     C                   MOVE      'N'           XXFRST
     C                   CLEAR                   E00406
     C                   MOVE      *ZEROS        XXDP1
     C                   MOVE      *ZEROS        XXAPD
     C                   MOVE      *ZEROS        XXDP2
     C                   MOVE      *ZEROS        XXDP3
     C                   MOVE      *ZEROS        XXDP4
     C                   MOVE      *ZEROS        XXDP5
     C                   MOVE      *ZEROS        XXDP6
     C                   MOVE      *ZEROS        XXST1
     C                   MOVE      *ZEROS        XXST2
     C                   MOVE      *ZEROS        XXST3
     C                   MOVE      *ZEROS        XXST4
     C                   MOVE      *ZEROS        XXST5
     C                   MOVE      *ZEROS        XXST6
     C                   Z-ADD     0             DX
     C                   ENDIF

     C                   ENDIF
     C     IX            IFEQ      5
      /Free
          WriteInCopybook = *Off;
          Exsr WriteWorkFile;
      /End-Free
     C                   MOVE      *ZEROS        LM
     C                   Z-ADD     *ZEROS        PC
     C                   MOVE      *BLANKS       SP
     C                   Z-ADD     0             IX
     C                   Z-ADD     0             JX
     C                   Z-ADD     0             wkPCT
     C                   Z-ADD     0             wkCNT
     C                   ENDIF

     C                   ENDIF
      //***** Beg Add *** 01/27/23 ******************************
       Endif;
      //***** End Add *** 01/27/23 ******************************

     C                   ENDIF
     C                   ENDIF
           //***** Beg Add *** 10/26/22 ******************************
             Endif;
           //***** End Add *** 10/26/22 ******************************
      *  Get the next division.
     C     ELKY2         READE     PEL02                                  60

     C                   ENDDO

     C                   ENDIF
      *  Write the output record
     C     IX            IFNE      *ZERO
      /Free
          WriteInCopybook = *Off;
          Exsr WriteWorkFile;
      /End-Free
     C                   ENDIF
     C     DX            IFNE      *ZERO

      * Update the endorsement reporting file with the proper data.
      * Find the unique ID number.
     C                   MOVEL     XXFORM        X1END#
     C     PEKEY2        CHAIN     PPE01                              60
      * Copy data into the record.
     C     *IN60         IFEQ      *OFF
     C     XXASP3        IFNE      *BLANK
     C                   Z-ADD     ELASP3        XXAPD
     C                   ENDIF
     C                   MOVEL(P)  E00406        PEDATA

      *  If printing a policy page.  (Not called from W1054R)

     C     #1FLAG        IFEQ      '0'

     C     XXFRST        IFEQ      *BLANK
     C                   UPDATE    PPE01
     C                   ELSE
     C                   WRITE     PPE01
     C                   ENDIF

     C                   ENDIF

     C                   ENDIF

     C                   ENDIF

     C                   ENDSR
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     PGMSTR        BEGSR
     C                   ENDSR
      *=========================================================================
      *  PGMEND - End of program processing
      *=========================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
     C     VALID         BEGSR

       //***  Begin Chg  ***  12/19/17  **********************************
     C*****EMCAD1        IFGT      ELPFDT
     C*****EMCAD1        OREQ      *ZEROS
     C*****ELEDC3        IFNE      *ZEROS
      **********
     C*****RSJRST        IFNE      'NJ'
     C*****RSJRST        ANDNE     'PA'
      **********
     C**********         MOVE      *ON           CRTFRM            1
     C**********         ENDIF
     C**********         ENDIF
     C**********         ENDIF
      /Free
       Select;
       When EMcaD1 <> *zero and EMcaD1 <= ELpfDt;
          Leavesr;
       //***  Beg Chg  ***  03/07/18  **********************************
       //When ELedC3 = *zeros;
       When ELdisc = *blank and ELasp3 = 0;                 //No discount entered
       //***  End Chg  ***  03/07/18  **********************************
          Leavesr;
       Endsl;
       crtFrm = *on;
      /End-free
       //***  End   Chg  ***  12/19/17  **********************************

     C                   ENDSR
