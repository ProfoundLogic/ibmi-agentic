     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    h** ======================================================================================== **
    h/TITLE W8562R1 - Zurich extract control report
     h option(*nodebugio:*srcstmt)
     H DFTACTGRP(*NO) ACTGRP(*caller)
    h* ========================================================================================= **
    h*                                                                                            *
     h*  Called from:  W8562C1                                                                     *
     h*                                                                                            *
     h*  Created: 07/21/11 by C:190 for project: 114WZU                                            *
    h** ======================================================================================== **
    h* Revisions:                                                                                 *
     h*
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     h* 08/26/14 C:298 Project: SY354
     h*                -Implement Routing Page Changes.
     h*
     h* 09/09/11 C:191 Support: WA114 77946
     h*                Base control report on format 40 and 50
     h*                No Comments added, just changed
     h*
    h** ======================================================================================== **
      *                                                                                            *
     fwzz3l     if   e           k disk
     fwzz4l     if   e           k disk
     fwzz5l     if   e           k disk
     fwmfnl     if   e           k disk
     Fsmorl     if   e           k disk
     Fsmupl     if   e           k disk
     fqsysprt   o    f   85        printer oflind(*inof)
      *=============================================================================================
      * Stand alone fields
      *=============================================================================================
     d kkco#           s                   like(fnco#)
     d kkfnd           s                   like(fnfnd)
     d x3acct          s                   like(fnfnd)
     dxxmedt           s              8  0
     d xxfname         s                   like(fnname)
     d xxtpa           s                   like(z3tpa)
     d xxacct          s                   like(fnfnd)
     dxxplam           s             17  2
     dxxtpla           s             17  2
     dxxtotp           s             17  2
     dxxtotr           s             17  2
     D ##data          s             80
     D qqkey           s             40
     D qqque           s             10
     D n               s              3  0
     D ##leng          s              3  0
     D xxdata          s             60
     D xxprt           s              1
     D xxsign          s              1
      *
      *=============================================================================================
      * Data structures
      *=============================================================================================

     d psds          esds                  extname(szq1p)
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd

      *=============================================================================================
      * KLISTS
      *=============================================================================================
      *
     c     fnky          klist
     c                   kfld                    kkco#
     c                   kfld                    x3acct
      *
     c     z3ky          klist
     c                   kfld                    z3tpa
     c                   kfld                    z3acct
     c*
    C*   KEY LIST FOR THE OVERRIDE FILE.
    C*
     C     orkey         klist
     C                   kfld                    q1pgm
     C                   kfld                    xxprt

     C     dtcymd1       plist
     C                   parm                    srdti1            8 0
     C                   parm      '*CYMD'       srffmt            8
     C                   parm      '*YYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
      *=============================================================================================
      * PLISTS
      *=============================================================================================
     c     *entry        plist
     c                   parm                    @@medt            7
     c                   parm                    @@co#             3 0

      *
      *=============================================================================================
      * Main loop
      *=============================================================================================
     c*
     c                   move      @@medt        srdti1
     c                   call      'SRCVTDAT'    dtcymd1
     c                   Movel     srdto         xxmedt
     c* Initialize report
     c                   exsr      srInit
     c*
     c                   eval      xxtpa = 0
     c                   eval      xxacct = 0
     c*
     c                   read      pz301
     c                   dow       not %eof(wzz3l)
     c*
     c                   eval      x3acct = %int(%subst(z3acct:1:3))
     c*
     c                   if        (z3tpa <> xxtpa or x3acct <> xxacct)
     c* tpa or acct changed get total paid and total recoveries
     c                   exsr      srGetAmounts
     c*
     c                   if        xxplam <> 0 or xxtpla <> 0
     c                   exsr      srprint
     c                   endif
     c*
     c                   eval      xxtpa =  z3tpa
     c                   eval      xxacct = x3acct
     c                   endif
     c*
     c                   read      pz301
     c                   enddo
      *
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   seton                                        lr
     c*
      *=============================================================================================
     c***************************************************************************************
     c* SrGetAmounts - Retrieve total paid and recovery amount for account
     c***************************************************************************************
     c     srGetAmounts  begsr
     c*
     C                   eval      xxplam = *zeros
     C                   eval      xxtpla = *zeros
     c* Retrieve total paid from record type 40
     c     z3ky          setll     pz401
     c     z3ky          reade     pz401
     c                   dow       not %eof(wzz4l)
     c                   eval      xxsign = %subst(z4plam:1:1)
     c                   movel     *zeros        x1plam           17 2
     c                   movel     0             z4plam
     c                   if        xxsign = '+'
     c                   move      z4plam        x1plam
     c                   eval      xxplam = xxplam + x1plam
     c                   else
     c                   move      z4plam        x1plam
     c                   eval      xxplam = xxplam - x1plam
     c                   endif
     c     z3ky          reade     pz401
     c                   enddo

     c* Retrieve total recoveries from record type 50
     c     z3ky          setll     pz501
     c     z3ky          reade     pz501
     c                   dow       not %eof(wzz5l)
     c                   eval      xxsign = %subst(z5tpla:1:1)
     c                   movel     *zeros        x1tpla           17 2
     c                   movel     0             z5tpla
     c                   if        xxsign = '+'
     c                   move      z5tpla        x1tpla
     c                   eval      xxtpla = xxtpla + x1tpla
     c                   else
     c                   move      z5tpla        x1tpla
     c                   eval      xxtpla = xxtpla - x1tpla
     c                   endif
     c     z3ky          reade     pz501
     c                   enddo
     c*
     c                   endsr
     c***************************************************************************************
     c* SrInit - Initialize report
     c***************************************************************************************
     c     srInit        begsr
     c*
    C*   Get the distribution information.
    C*
     C                   move      '0'           xxprt
     C     orkey         chain     por01                              60
    C*
    C*   Print the distribution Information, if a valid record is found.
    C*
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     c*
     C                   if        *in60 = *off
     C                             and orstat = 'A'
     C     q1user        chain     pup01                              60
    C*
    C*  Left Justify Title in Field
    C*
     C                   move(p)   ortitl        xxdata
    C*
     C     ' '           check     xxdata        n
    C*
     C                   if        n <= 60
     C                   subst(p)  xxdata:n      ortitl
     C                   endif
    C*
      ***** Beg Del ***** 08/26/14 *************************************************
     C***********        except    route
      ***** End Del ***** 08/26/14 *************************************************
    C*
    C*  Center Text in Field
    C*
     C                   movel     ortitl        ##data
    C*
     C                   call      'SRCENTER'                           99
     C                   parm                    ##data
     C                   parm      60            ##leng
    C*
     C                   movel     ##data        ortitl
    C*
     C                   endif

     c                   except    header
     c*
     c                   endsr
     c***************************************************************************************
     c* SrPrint - Print control report format
     c***************************************************************************************
     c     srprint       begsr
     c                   move      @@co#         kkco#
     c                   eval      xxfname = *blanks
     c                   eval      xxtotp  = *zeros
     c                   eval      xxtotr  = *zeros
     c     fnky          chain     pfn01                              60
     c                   if        %found
     c                   eval      xxfname = fnname
     c                   eval      xxtotp  = xxplam
     c                   eval      xxtotr  = xxtpla
     c                   if        *inof
     c                   except    header
     C                   eval      *inof = *off
     c                   endif
     c                   except    detail
     c                   endif
     c                   endsr
      *=============================================================================================
     Oqsysprt   e            route          1 06
      ***** Beg Del ***** 08/26/14 *************************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********e            route          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********e            route          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********e            route          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********e            route          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********e            route          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********e            route          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********e            route          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********e            route          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********e            route          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********e            route          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********e            route          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********e            route          2
     O**********                                 15 'ROUTE TO:'
     O**********             upusnm              67
      ***** End Del ***** 08/26/14 *************************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          e            route          2
     O                                           15 'ROUTE TO:'
     O                       upusnm              67
      ***** End Add ***** 08/05/15 ************************************
     O          e            route          2
     O                                           19 'REPORT TITLE:'
     O                       ortitl              80
      ***** Beg Del ***** 08/26/14 *************************************************
     O**********e            route          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             orpnam              30
     O**********                                 36 'FILE:'
     O**********             orfil#              38
     O**********                                 46 'COPIES:'
     O**********             orcopy        z     50
     O**********                                 56 'HOLD:'
     O**********             orhold              61
     O**********e            route          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             qqque               30
     O**********                                 37 'FORMS:'
     O**********             orform              48
     O**********                                 56 'SAVE:'
     O**********             orsave              61
      ***** End Del ***** 08/26/14 *************************************************

     O          e            route          1
     O                                           24 'REPORT SELECTIONS:'

     O          e            route          1
     O                                           19 'Valuation'
     O                                           29 'Period:'
     O                       XXmedt              46 '    -  -  -'

     O          e            route          2
    
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     Oqsysprt   e            header         1 05
     O                                           12 '************'
     O                                           24 '************'
     O                                           36 '************'
     O                                           48 '************'
     O                                           60 '************'
     O                                           72 '************'
     O                                           84 '************'
     O          e            header      2  1
     O                                           32 'ZURICH'
     O                                           40 'MONTHLY'
     O                                           48 'CONTROL'
     O                                           55 'REPORT'
     O          e            header      2  1
     O                                           12 '************'
     O                                           24 '************'
     O                                           36 '************'
     O                                           48 '************'
     O                                           60 '************'
     O                                           72 '************'
     O                                           84 '************'
     O          e            detail      5  3
     O                                           18 'VALUATION'
     O                                           26 'PERIOD:'
     O                       xxmedt              46 '    -  -  -'
     O          e            detail         1
     O                                           20 'ACCOUNT:'
     O                       xxfname             81
     O          e            detail         1
     O                                           20 'PAID:'
     O                       xxtotp        J     43
     O          e            detail         1
     O                                           20 'RECOVERIES:'
     O                       xxtotr        J     43
