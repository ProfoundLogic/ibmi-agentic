     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H/TITLE W8501R - CONTRACTORS CLASS CALCULATION
    H*****************************************************************
      * Revised:
     H*                                                               *
     H* 06/22/18 C:65  Project: 150WAL                                *
     H*                Change rounding on territory factor to round   *
     H*                after applying both LCM and territory.         *
      *                                                               *
      * 07/26/17 C:62  Project: 150WAL                                *
      *                Add Territorial Differential Factor Process.   *
      *                                                               *
      * 04/01/08 C:183 PROJECT: WC1619                                *
      *                Call SREFFD to set the effective date to be    *
      *                used on SETGT.                                 *
      *
      * 08/10/05 C:136  Project: WC1136
      *                 Add Loss Cost Multiplier to Payroll Rate.
      *
      * 07/13/04  C:61  Project: WC1173
      *               - Only calculate average hourly wage if
      *                 Contractor Class.
      *
      * 06/16/04  C:61  Project: WC1173
      *               - Change to get EXPERIENCE MOD data using API.
      *
      * 01/27/04  C:61  Project: WC1173
      *               - Converted to RPG4
      *               - Get Contractor Credit factor from table file
    H*                                                               *
    H*  10/02/98 C:65  PROJECT: SUPPORT WA091/14106                  *
    H*                 CHAIN TO THE PAYROLL CLASS FILE WITH ELPFDT   *
    H*                 AND ELRTCD, NOT EPEFDT.                       *
    H*                                                               *
    H*****************************************************************
     Fwdell1    uf   e           k disk
     Fwmccl     uf   e           k disk
     *** BEGIN CHG ***  10/02/98  ************************************
     ***WMPRL   IF  E           K        DISK
     ***WMEPL3  IF  E           K        DISK
     Fwmprl1    if   e           k disk
     *** END CHG *****  10/02/98  ************************************
    F*** BEG ADD *****  04/01/08  ************************************
     FWMEPL1    if   e           k disk
    F*** END ADD *****  04/01/08  ************************************
      ******* beg add ****** 01/27/04 *********************************
     *
     * Contractor Credit Table
     Fwtcdl     if   e           k disk
     *
     * Rate Set Code Table
     Fwmrsl     if   e           k disk
     *
     ****** beg del ***** 06/16/04 ***********************************
     ******** Employer Experiece Mod Work File
     F******** exxtl1    if   e           k disk
     ****** end del ***** 06/16/04 ***********************************
      ******* end add ****** 01/27/04 *********************************
     *
     *
      ******* beg del ****** 01/27/04 *********************************
     D*wg******        s              5  2 dim(24) ctdata perrcd(1) ascend
     D*cr******        s              2  0 dim(24) alt(wg)
      ******* end del ****** 01/27/04 *********************************
     D                 ds
     D dsdttm                        12  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        6  0 overlay(dsdttm:7)
     D psds          esds                  extname(szq1p)
     *
     *
     *---------------------------------------------------------------------
     * Stand Alone Fields - TOP
     *---------------------------------------------------------------------
     D qqco#           s              3
     D qqdiv           s              5
     D qqemp#          s              9
     D qqfnd           s              3
     D qqfyr           s              3
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D x               s              2  0
     D xx              s              2  0
     D xxclas          s              5  0
     D xxco#           s              3  0
      ******* beg chg ****** 01/27/04 *********************************
     D******* xxcrpc          s              5  2
     D xxcrpc          s             11  5
      ******* end chg ****** 01/27/04 *********************************
     D xxctot          s             11  2
     D xxdiv           s                   like(eldiv)
     D xxefdt          s                   like(elpfdt)
     D xxemp#          s                   like(elemp#)
     D xxfnd           s              3  0
     D xxfyr           s              3  0
     D xxmnpr          s             11  4
     D xxmtot          s             11  2
     D xxpcnt          s              5  4
     D xxrtcd          s                   like(elrtcd)
      ******* beg add ****** 01/27/04 *********************************
     D xxjrst          s                   like(rsjrst)
     D Z               s             15  5
     D CF              s             15  5
      ******* end add ****** 01/27/04 *********************************
      *** beg add ***** 08/10/05 **************************************
     d #1co#           s              3    inz(*blanks)
     d #1fnd           s              3    inz(*blanks)
     d ##rtcd          s              3    inz(*blanks)
     d ##clas          s              5    inz(*blanks)
     d ##efdt          s              7    inz(*blanks)
     d ##lcm           s              5    inz(*blanks)
     d xxlcm           s              5  4
      *** end add ***** 08/10/05 **************************************
      *** Begin Add ***** 07/26/17 ***********************************************
     d* Get the Territorial Differential Factor
     d GetTDF          pr                  extpgm('WA101R')
     d  inCompany#                    3  0
     d  inGroup#                      3  0
     d  inEmployer#                   9  0
     d  inDivision                    5  0
     d  inYear                        3  0
     d  outTDF                        5  4
     d  outTDF         s              5  4
      *** End   Add ***** 07/26/17 ***********************************************
     *---------------------------------------------------------------------
     * Stand Alone Fields - BOTTOM
     *---------------------------------------------------------------------
      ******* beg add ****** 01/27/04 *********************************
     *
     *  Contractor Credit Factor Table
     *
     C     cdky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxjrst
     *
     C     cdky1         klist
     C                   kfld                    xxco#
     C                   kfld                    xxjrst
     C                   kfld                    ccahw
     *
     *  Rate Code
     *
     C     rsky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxrtcd
     *
     ****** beg del ***** 06/16/04 ***********************************
     **********
     ***Employer Experience Mod Work File
     **********
    C*****xtky*         klist
    C**********         kfld                    ccco#
    C**********         kfld                    ccfnd
    C**********         kfld                    ccemp#
     **********
    C*****xtky1         klist
    C**********         kfld                    ccco#
    C**********         kfld                    ccfnd
    C**********         kfld                    ccemp#
    C**********         kfld                    xxdt7
     ****** end del ***** 06/16/04 ***********************************
     *
      ******* end add ****** 01/27/04 *********************************
     *
     *   KEY LIST FOR EMPLOYER DETAIL FILE
     *
     C     elkey         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     *
     *   KEY LIST FOR CONTRACTORS CLASS FILE
     *
     C     cckey         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     *
     C     ccky2         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     *
     *   KEY LIST FOR PAYROLL CLASSIFICATION FILE
     *
     *** BEGIN CHG ***  10/02/98  ************************************
     *****      PRKEY     KLIST
     *****                KFLD           XXCO#
     *****                KFLD           XXCLAS
     *****                KFLD           XXEFDT
     *****      EPKY      KLIST
     *****                KFLD           XXCO#
     *****                KFLD           XXCLAS
     *****                KFLD           XXFND
     *****                KFLD           XXEMP#
     *****                KFLD           XXDIV
     *****                KFLD           XXFYR
     *
     C     prkey         klist
     C                   kfld                    xxco#
     C                   kfld                    xxrtcd
     C                   kfld                    xxclas
     C                   kfld                    xxefdt
     *** END CHG *****  10/02/98  ************************************
     *
     *** BEGIN CHG ***  10/02/98  ************************************
     *****      *LIKE     DEFN EPEFDT    XXEFDT
     *** END CHG *****  10/02/98  ************************************
    C*** BEG ADD *****  04/01/08  ************************************
    C     EPKY          KLIST
    C                   KFLD                    XXCO#
    C                   KFLD                    XXFND
    C                   KFLD                    XXFYR
    C                   KFLD                    XXEMP#
    C                   KFLD                    XXDIV
    C                   KFLD                    XXCLAS
    C*** END ADD *****  04/01/08  ************************************
     *
     *
     *
     C     *entry        plist
     C                   parm                    qqco#
     C                   parm                    qqfnd
     C                   parm                    qqfyr
     C                   parm                    qqemp#
     C                   parm                    qqdiv
     *
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     *
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     *
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     *
      ******* beg add ****** 01/27/04 *********************************
     *
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxdt7             7 0
     *
      ******* end add ****** 01/27/04 *********************************
     ****** beg add ***** 06/16/04 ***********************************
     *
     * Determine if Experience Module installed
     *
     C                   call      'SA038C'
     C                   parm      *blank        expmod            1
    
     ****** end add ***** 06/16/04 ***********************************
    
     C                   move      qqco#         xxco#
     C                   move      qqfnd         xxfnd
     C                   move      qqfyr         xxfyr
     C                   move      qqemp#        xxemp#
     C                   move      qqdiv         xxdiv
     *
     C                   eval      *in20 = xxemp# = 0
     C                   move      *off          *in61
     *
     *   START READING THE CONTRACTORS CLASS FILE: IF EMP# = 0 CHAIN
     *   WITH CO#, FND, & FYR; ELSE CHAIN USING EMP# & DIV
     *
     C                   if        *in20 = *on
     C     ccky2         chain     pcc01                              60
     C                   endif
     *
     C                   if        *in20 = *off
     C     cckey         chain     pcc01                              60
     C                   endif
     *
     C                   if        *in60 = *off
     *
     C                   dow       *in61 = *off
     C                   eval      xxctot = 0
     C                   eval      xxmtot = 0
     C                   eval      xxpcnt = 0
     *
     C                   if        *in20 = *on
     C                   eval      xxemp# = ccemp#
     C                   eval      xxdiv = ccdiv
     C                   endif
     *
     *** BEGIN ADD ***  10/02/98  ************************************
     C     elkey         chain     pel01                              40
     *
     C                   if        *in40 = *off
     C                   except    pel01e
     C                   eval      xxefdt = elpfdt
     C                   move      elrtcd        xxrtcd
     *
     C                   else
     *
     C                   eval      xxefdt = 0
     C                   clear                   xxrtcd
     C                   endif
     *** END ADD *****  10/02/98  ************************************
       //*** Begin Add *** 07/26/17 ************************************
      /free
       // Get the Territorial Differential Factor
       GetTDF(ELco#:ELfnd:ELemp#:ELdiv:ELfYr:outTDF);
      /end-free
       //*** End   Add *** 07/26/17 ************************************
     *
     C                   dow       *in60 = *off
     *
     C                   if        ccpamt <> 0
     C                   eval      xxmnpr = 0
     C                   eval      xxcrpc = 0
     *
     *   CALCULATE AVG. HOURLY WAGE
     *
     ****** beg del ***** 07/13/04 ***********************************
    C**********         if        cchrs <> 0
    C**********         eval(h)   ccahw = ccpamt / cchrs
    C**********         endif
     ****** end del ***** 07/13/04 ***********************************
     *
     *   GET MOST CURRENT RATE FROM PAYMENT FILE
     *
     C                   clear                   xxclas
     C                   move      ccclas        xxclas
     *
     *** BEGIN DEL ***  10/02/98  ************************************
     *****      EPKY      CHAINPEP01                69
     *****      *IN69     IFEQ *OFF
     *****                Z-ADDEPEFDT    XXEFDT
     *****                ENDIF
     *** END DEL *****  10/02/98  ************************************
     *
     C*** BEG ADD *****  04/01/08  *******************************************
    C     EPKY          CHAIN     PEP01                              69
     C                   Z-ADD     0             XXEFDT
     C                   MOVE(P)   ELCO#         #1CO#             3
     C                   MOVE(P)   ELFND         #1FND             3
     C                   MOVE(P)   ELFYR         #1FYR             3
     C                   MOVE(P)   ELEMP#        #1EMP#            9
     C                   MOVE(P)   ELDIV         #1DIV             5
     C                   MOVE(P)   EPEFDT        #1DATE            7
     C                   MOVE(P)   'P'           #1TYPE            1
     C                   MOVE(P)   XXCLAS        #1CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #1CO#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1EMP#
     C                   PARM                    #1DIV
     C                   PARM                    #1DATE
     C                   PARM                    #1TYPE
     C                   PARM                    #1CLAS
      *
     C                   MOVE      #1DATE        XXEFDT
     C*** END ADD *****  04/01/08  *******************************************
     C     prkey         setgt     ppr01
     C                   readp     ppr01                                  50
     *
     C                   if        *in50 = *off
     C                   eval      *in50 = prco# <> xxco#
     C                   endif
     *
     C                   if        *in50 = *off
     C                   eval      *in50 = prclas <> xxclas
     C                   endif
     *
     *** BEGIN ADD ***  10/02/98  ************************************
     C                   if        *in50 = *off
     C                   eval      *in50 = prrtcd <> xxrtcd
     C                   endif
     *** END ADD *****  10/02/98  ************************************
     *
     C  n50              do
     *
     *   CALCULATE MANUAL PREMIUM
     *
      *** beg add ***** 08/10/05 ***************************************
     c                   move      xxclas        ##clas
     c                   move      xxefdt        ##efdt

     c                   call      'SRLCM'
     c                   parm      qqco#         #1co#
     c                   parm      qqfnd         #1fnd
     c                   parm      xxrtcd        ##rtcd
     c                   parm                    ##clas
     c                   parm                    ##efdt
     c                   parm                    ##lcm

     c                   move      ##lcm         xxlcm
     C*** begin chg ***  06/22/18  *************************************
     c*****              eval(h)   prrt = prrt * xxlcm
       //*** Begin Add *** 07/26/17 ************************************
      /free
       // Apply Territorial Differential Factor
       //***If outTDF > 0;
       //***   PRrt = %DecH(PRrt * outTDF:7:2);
       //***EndIf;
      /end-free
       //*** End   Add *** 07/26/17 ************************************
     C                   If        outTDF = 0
     C                   Eval(h)   prrt = xxlcm * prrt
     C                   Else
     C                   Eval(h)   prrt = outTDF * xxlcm * prrt
     C                   Endif
     C*** end chg *****  06/22/18  *************************************
     C                   eval      xxmnpr = ccpamt / 100
     C                   eval(h)   ccmnpr = xxmnpr * prrt
     *
     *   ACCUMULATE MANUAL PREMIUM
     *
     C                   eval      xxmtot = xxmtot + ccmnpr
     *
     *   IF RECORD IS CONTRACTOR TYPE, THEN ZERO OUT PERCENTAGE FIELD
     *   ELSE GET CREDIT PERCENT & CALC. CREDIT AMT.
     *
     C                   if        prccfl = 'N'
     C                   eval      cccrpc = 0
     C                   eval      cccamt = 0
     *
     C                   else
     *
      ******* beg del ****** 01/27/04 *********************************
     C*********          eval      x = 1
     C*****ccahw         lookup    wg(x)                              50
     *********
     C*********          if        *in50 = *on
     *********
     C*********          if        x <> 1
     C*********          eval      x = x - 1
     C*********          endif
     *********
     C*********          eval      xxcrpc = cr(x) / 100
     C*********          eval(h)   cccamt = xxcrpc * ccmnpr
     C*********          eval      cccrpc = cr(x)
     C*********          endif
      ******* end del ****** 01/27/04 *********************************
      ******* beg add ****** 01/27/04 *********************************
     ****** beg add ***** 07/13/04 ***********************************
     C                   if        cchrs <> 0

     C                   if        (ccpamt / cchrs) > 999
     C                   eval      ccahw = 999.0
     c                   else
     C                   eval(h)   ccahw = ccpamt / cchrs
     C                   endif
      *
     C                   endif
     ****** end add ***** 07/13/04 ***********************************
      *
      *  Get Contractor Credit Factor
      *
     C                   exsr      getfactor
      *
      ******* end add ****** 01/27/04 *********************************
     C                   endif
     *
     C                   eval      ccrt = prrt
     C                   move      q1user        ccuser
     C                   move      q1pgm         ccpgm
     C                   time                    dsdttm
     C                   eval      cctime = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         ccdt
     *
     C                   eval      xxctot = xxctot + cccamt
     *
     *   UPDATE CONTRACTORS CLASS FILE
     *
     C                   update    pcc01
     C                   enddo
     *
     C                   endif
     *
     *   GET NEXT RECORD
     *
     C                   if        *in20 = *on
     C     ccky2         reade     pcc01                                  62
     *
     C                   if        ccemp# <> xxemp#
     C                             or ccdiv <> xxdiv
     C                             or *in62 <> *off
     C                   move      *on           *in60
     C                   endif
     *
     C                   endif
     *
     C                   if        *in20 = *off
     C     cckey         reade     pcc01                                  60
     C                   endif
     *
     C                   enddo
     *
     *   CALCULATE CREDIT PERCENTAGE & UPDATE EMPLOYER DETAIL
     *
     C                   if        xxmtot <> 0
     C     elkey         chain     pel01                              50
     *
     C                   if        *in50 = *off
     C                   eval(h)   xxpcnt = xxctot / xxmtot
     C                   eval      elasp1 = xxpcnt * 100
     C                   move      elasp1        xx
     *
     C                   if        xx >= 50
     C                   eval      elasp1 = elasp1 + 1
     C                   endif
     *
     C                   move      '00'          elasp1
     C                   move      q1user        eluser
     C                   move      q1pgm         elpgm
     C                   time                    dsdttm
     C                   eval      eltime = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         eldt
     C                   update    pel01
     C                   endif
     *
     C                   endif
     *
     C                   if        *in20 = *off
     C                   move      *on           *in61
     C                   endif
     *
     C   20              if        *in62 = *on
     C                   move      *on           *in61
     C                   else
     C                   move      *off          *in60
     C                   endif
     *
     C                   enddo
     *
     C                   endif
     *   END OF PROGRAM
     C                   move      *on           *inlr
      ******* beg add ****** 01/27/04 *********************************
     *
     *
     *
      *===============================================================
     *  GetFactor - Get contractor credit factor from table
      *===============================================================
     c     GetFactor     begsr
     *
     *  Get rating state
     *
     c     rsky          chain     prs01
     *
     c                   if        %found
     c                   eval      xxjrst = rsjrst
     c                   else
     c                   eval      xxjrst = 'XX'
     c                   endif
     ****** beg add ***** 06/16/04 ***********************************

     c                   if        (xxjrst = 'NY') and (expmod <> 'Y')
     c                   eval      xxjrst = 'XX'
     c                   endif
     ****** end add ***** 06/16/04 ***********************************
     *
     *  Check entry in Contractor Credit Factor Table for State
     *
     c     cdky          chain     pcd01
     *
     c                   if        %found
     c     cdky1         setll     pcd01
     c     cdky          readpe    pcd01
     *
     c                   if        %eof
     c     cdky          chain     pcd01
     c                   endif
     *
     c                   else
     *
     *  no entry for state, use default
     *
     c                   eval      xxjrst = 'XX'
     c     cdky          chain     pcd01
     *
     c                   if        %found
     c     cdky1         setll     pcd01
     c     cdky          readpe    pcd01
     *
     c                   if        %eof
     c     cdky          chain     pcd01
     c                   endif
     *
     c                   else
     *
     c                   clear                   pcd01
     c                   endif
     *
     c                   endif
     *
     *  Process by Contractor Credit Factor by Rating state
     *
     c                   select
      *
      *  New York
      *
     c                   when      xxjrst = 'NY'
     C                   eval      cccrpc = 0
     *
     ****** beg del ***** 06/16/04 ***********************************
    c*****xtky1         setgt     pxx01
    c*****xtky*         readpe    pxx01                                  60
     **********
    c**********         if        not %eof
    c**********         eval      Z =  (XTEPLS + XTWVWV * XTEXEX) /
    c**********                        (XTEXLS + XTBALL)
    c**********         eval      CF = cdfact * Z
    c**********         eval      cccrpc = cdfact - CF
    c**********         endif
     ****** end del ***** 06/16/04 ***********************************
     ****** beg add ***** 06/16/04 ***********************************
     *
     * Get Experience Module values
     *
     C                   call      'EA001R'
     C                   parm      ccco#         ##co#             3 0
     C                   parm      ccfnd         ##fnd             3 0
     C                   parm      ccemp#        ##emp#            9 0
     C                   parm      xxdt7         ##date            7 0
     C                   parm      *zero         ##exls            9 0
     C                   parm      *zero         ##epls            9 0
     C                   parm      *zero         ##exex            9 0
     C                   parm      *zero         ##wvwv            3 2
     C                   parm      *zero         ##ball           11 0
     C                   parm      *blank        ##err             1
    
    
     c                   if        ##err = '0'
    
     c                   if        (##exls + ##ball) <> 0
     c                   eval      Z =  (##EPLS + ##WVWV * ##EXEX) /
     c                                  (##EXLS + ##BALL)
    
     c                   eval      CF = cdfact * Z
     c                   eval      cccrpc = cdfact - CF
     c                   endif
    
     c                   endif
     ****** end add ***** 06/16/04 ***********************************
      *
      *  Default state
      *
     c                   other
     C                   eval      cccrpc = cdfact
     *
     C                   endsl
     *
     *
     C                   eval      xxcrpc = cccrpc / 100
     C                   eval(h)   cccamt = xxcrpc * ccmnpr
     *
     c                   endsr
     *
     *
     *
      ******* end add ****** 01/27/04 *********************************
    O*** BEGIN ADD ***  10/02/98  ************************************
     Opel01     e            pel01e
    O*** END ADD *****  10/02/98  ************************************
      *****
      *****0000000
      *****0100005
      *****0115106
      *****0117607
      *****0120108
      *****0122609
      *****0125110
      *****0127611
      *****0130112
      *****0132613
      *****0135114
      *****0137615
      *****0140116
      *****0142617
      *****0145118
      *****0147619
      *****0150120
      *****0155121
      *****0160122
      *****0165123
      *****0170124
      *****0175125
      *****9999900
