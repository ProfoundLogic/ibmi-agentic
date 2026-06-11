     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
    H*
     H/TITLE W7483R  - Medicare Cumulative TIN Extract Program
    H**************************************************************************************
    H*  Description: Medicare 2nd pay extract process - Generate data file based on       *
    H*                                                                                    *
    H*  Created:   08/26/11                                                               *
    H*  Project:   WC2029                                                                 *
    H*  Programer: C:53                                                                   *
    H*                                                                                    *
    H*  Called from: W7483C1 (Override CL)                                                *
    H*                                                                                    *
    H*  Calls to: W7483C2                                                                 *
    H*                                                                                    *
    H**************************************************************************************
    H*  Revisions:                                                                        *
    H*                                                                                    *
     H*  11/11/11 C:53 - Support 78326.
    H*                  Exclude inactive records                                          *
    H*                                                                                    *
    H**************************************************************************************
     Fwxgsl     if   e           k disk
     Fwdf2l     if   e           k disk
     Fwmofl     if   e           k disk
     Fssylf     if   e           k disk
     Fwxt1p     o  a e           k disk
     D psds          esds                  extname(szq1p)
     D pth           e ds                  extname(med2ngth)
     D ptd           e ds                  extname(med2ngtd)
     D ptt           e ds                  extname(med2ngtt)
     D*
     D                 ds
     D dsdttm                        12  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        6  0 overlay(dsdttm:7)
     D*
     D xxco#           s                   like(f2co#)
     D xxfnd           s                   like(f2fnd)
     D svfnd           s                   like(f2fnd)
     D x1date          s                   like(f2dt)
     D xxfein          s                   like(f2fein)
     D xxdate          s              8  0
     D xxcnt           s              7  0
     D of#             s              3  0
     D rreFeinOff      s             23
     D yyrre#          s              9
     D yyfein          s              9
     D ofa             s             23    dim(500)
     D*
     D*  KEY LIST FOR Group Master File
     C*
     C     FNKY          KLIST
     C                   KFLD                    xxco#
     C                   KFLD                    xxfnd
     C*
     C*  KEY LIST FOR Group Auxiliary File
     C*
     C     F2KY          KLIST
     C                   KFLD                    xxco#
     C                   KFLD                    xxfnd
    C*
    C*  Key list for OFFICE MASTER File
    C*
     C     ofky          klist
     C                   kfld                    xxco#
    C*
     C* Date call structures
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*** check for install flag
     C*
     C                   move      'WC'          kkKey             2
     C                   clear                   swc01
     C     kkKey         chain(n)  swc01
     C*
     C                   if        WCFL49 <> 'Y'
     C                   move      *on           *inlr
     C                   return
     C                   endif
     C*
     C                   time                    dsdttm
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1DATE
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   move      *blank        xxfrst            1
     C                   z-add     0             xxcnt
     C                   z-add     0             svfnd
     C*
     C* Begin reading group select file to process all selected groups.
     C*
     C     *loval        setll     pGS01
     C                   read      pgs01                                  60
     C                   move      gsco#         xxco#
     C*
     C                   dow       *in60 = *off
     C*
     C                   if        gsiefl = 'I'
     C                   move      gsfnd         xxfnd
     C*
     C* Set group # to zero for backup member name for multiple group files.
     C*
     C                   if        gsfnd <> svfnd
     C                   z-add     0             svfnd
     C                   endif
     C*
     C* Write records for the TIN extract file.
     C*
     C     f2ky          chain     pf201                              62
     C                   if        *in62 = *off
     C*
     C* Write header record if not done.
     C*
     C                   if        xxfrst = *blank
     C                   exsr      wrthdr
     C                   move      'Y'           xxfrst
     C                   z-add     xxfnd         svfnd
     C                   endif
     C*
     C                   exsr      wrtdtl
     C*
     C                   endif                                                  gsiefl = 'I'
     C                   endif                                                  gsiefl = 'I'
     C     xxco#         reade     pgs01                                  60
     C                   enddo                                                  dow *in60 = *off
     C*
     C                   exsr      wrttrl
     C*
     C* Copy data file to backup as a member in WXT1PSAV.
     C*
     C                   call      'W7483C2'
     C                   parm                    @@fnd             3
     C*
     C                   eval      *inlr=*on
     C*
     C*******************************************************************************************
     C*  Write header for the TIN extract file.
     C*******************************************************************************************
     C     wrthdr        begsr
     C*
     C* Write TIN file header record.
     C*
     C                   clear                   pth
     C*
     C                   move      'NGTH'        thrid
     C                   move      'NGHPTIN'     thrft
     C                   move      xxdate        thsdt
     C                   move      f2rre#        thrre
     C*
     C                   clear                   tdata
     C                   movel     pth           tdata
     C                   write     tpay
     C*
     C                   z-add     0             xxcnt
     C*
     C                   endsr                                                  wrttrl subroutine
     C*******************************************************************************************
     C*  Write detail records for the TIN extract file.
     C*******************************************************************************************
     C     wrtdtl        begsr
     C*
     C* Write TIN Detail record for each office
     C*
     C     *loval        setll     pof01
     C     xxco#         reade     pof01                                  60
     C*
     C                   dow       *in60 = *off
     C                   if        *in60 = *off
     C*** beg add *** 11/11/11 ***************************************
     C                               and ofstat = 'A'
     C*** end add *** 11/11/11 ***************************************
     c*
     c* Store RRE#, FEIN and OFF#
     c*
     C                   move      ofofc#        xxofc#            9 0
     c                   eval      rreFeinOff=%editc(f2rre#:'X') +
     c                             %editc(f2fein:'X') + ofofc#
     c*
     c* Store office # in array for TIN file.
     c*
     c                   z-add     1             x                 3 0
     c     rreFeinOff    lookup    ofa(x)                                 61
     c                   if        *in61 = *off
     c                   add       1             of#
     c                   move      rreFeinOff    ofa(of#)
     C*
     C* Write TIN file detail record.
     C*
     C                   clear                   ptd
     C*
     C                   move      'NGTD'        tdrid
     C                   move      f2rre#        tdrre
     C                   move      f2fein        tdfein
     C*
     C                   if        ofofc# <>  ' '
     C                   eval      tdstid = %editc(xxofc#:'X')
     C                   else
     C                   movel     *blanks       tdstid
     C                   endif
     C                   movel     ofname        tdname
     C                   movel     ofadr1        tdadr1
     C                   movel     ofadr2        tdadr2
     C                   movel     ofcty         tdcty
     C                   movel     ofst          tdst
     C                   movel     ofzip         tdzip
     C                   move      ofzip         tdzip4
     C*
     C                   clear                   tdata
     C                   movel     ptd           tdata
     C                   write     tpay
     C                   add       1             xxcnt
     C                   endif                                                  if *in60 = *off
     c                   endif
     C*
     C     ofky          reade     pof01                                  60
     C                   enddo
     C     enddtl        endsr                                                  wrttrl subroutine
     C*******************************************************************************************
     C*  Write trailer for the TIN extract file.
     C*******************************************************************************************
     C     wrttrl        begsr
     C*
     C* Write TIN file trailer record.
     C*
     C                   clear                   ptt
     C*
     C                   move      'NGTT'        ttrid
     C                   move      'NGHPTIN'     ttrft
     C                   move      xxdate        ttsdt
     C                   move      f2rre#        ttrre
     C                   z-add     xxcnt         ttcnt
     C*
     C                   clear                   tdata
     C                   movel     ptt           tdata
     C                   write     tpay
     C*
     C                   endsr                                                  wrttrl subroutine
