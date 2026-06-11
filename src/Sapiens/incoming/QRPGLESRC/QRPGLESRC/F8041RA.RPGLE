    H*  (C) Copyright Progressive Data Solutions, Inc., 2006         *
    H*                                                               *
    H*  F8041RA- Upload Bills - Transfer Program 1                   *
    H*****************************************************************
    H*                                                               *
    H*  Called from: F8041C  - Upload Bills - Override               *
    H*  Calls to   : F8042RA - Upload Bills - Transfer Pgm 2         *
    H*                                                               *
    H*****************************************************************
    H*  Created on 12/27/2005 by c:163 for project 136MBR            *
     H*          Copied from F8041R.                                  *
     H*                                                               *
    H*  Created on 04/20/2004 by c:62 for project 51MFN              *
    H*****************************************************************
    H*  Revisions:                                                   *
    H*                                                               *
    H*  05/13/14  C:296  Redmine: 4168                               *
    H*                   Populate U1ULDT and U1ULTM fields.          *
    H*                                                               *
    H*  07/29/13  C:079  Support: 82944                              *
    H*                   Do not write duplicate FAU2P records        *
    H*                                                               *
     H*  10/09/07 C:47  Project: 136MBR                               *
     H*                 Clear key field.                              *
    H*                                                               *
     H*  10/25/05 C:163 Project: 136MBR                               *
     H*                 Managed Care/Prov Bill Interface Process      *
     H*                                                               *
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
    F*** Beg Add ***** 07/29/13 **************************************
     Ffau2l3    if   e           k disk    rename(pu201:pu202)
    F*** End Add ***** 07/29/13 **************************************
    F*
     D i9i             s              1    dim(10)
     D i9o             s              1    dim(10)
     D ini             s              1    dim(18)
     D ino             s              1    dim(18)
     D fids01        e ds                  extname(szq2p)
     D psds          esds                  extname(szq1p)
    D*================================================================
    D* Constants
    D*================================================================
     D lo              c                   const('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D hi              c                   const('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
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
    D*** BEG   CHG *** 10/09/07 **************************************
     D**kkntwk                        5    overlay(test:115)
     D  xxntwk                        5    overlay(test:115)
    D*** END   CHG *** 10/09/07 **************************************
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
    D*** Beg Add ***** 07/29/13 **************************************
     D xxRlin#         s              8a
     D xxLin#          s              8a
     D kklin#          s                   like(u2lin#)
    D*** End Add ***** 07/29/13 **************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
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
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     u1ky          klist
     C                   kfld                    kktrid
    C*** BEGIN ADD *** 08/18/05 **************************************
     C                   kfld                    kkntwk
    C*** END   ADD *** 08/18/05 **************************************
    C*** Beg Add ***** 07/29/13 **************************************
     C     u2ky          klist
     C                   kfld                    kktrid
     C                   kfld                    kklin#
    C*** End Add ***** 07/29/13 **************************************
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
    C*** BEG   ADD *** 10/09/07 ****************************************
     C                   clear                   kkntwk            5
    C*** END   ADD *** 10/09/07 ****************************************
    C*
     C                   if        xxtype = '1'
    C*** BEG   ADD *** 10/09/07 ****************************************
     C                   eval      kkntwk = xxntwk
    C*** END   ADD *** 10/09/07 ****************************************
     C                   move      data500       test2
     C                   eval      xxtcnt = xxtcnt + 1
     C                   endif
    C*
     C     u1ky          chain     pu102                              99
     C                   if        *in99 = *on
     C                             or *in99 = *off
     C                             and rcnf = 'Y'
     C                   exsr      writer
     C                   else
     C                   if        xxtype = '1'
     C                   eval      xxdcnt = xxdcnt + 1
     C                   endif
     C                   endif
    C*
     C                   exsr      updatr
    C*
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
    C*
    C* Check and write missing Provider records
    C*
     C                   call      'F8043R'
    C*
    C* Transfer records to PDS
    C*
     C                   call      'F8042RA'
     C                   parm      @@name        ##name
     C                   parm      #1rrn         ##rrn
    C*
     C                   eval      *inlr = *on
    C*
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
    C*
     C                   move      @@rrn         #1rrn
     C                   eval      xxtcnt = *zeros
     C                   eval      xxdcnt = *zeros
    C*
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
     C                   movel     data500       header
     C                   eval      %subst(header:666) =
     C                             %subst(u0data:501)
     C                   movel(p)  @@name        u1name
     C                   movel(p)  @@mbr#        u1mbr#
     C****** Beg Add ***** 05/13/14 *****************************
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         u1uldt
     C                   eval      u1ultm = dstime
     C****** End Add ***** 05/13/14 *****************************
     C                   write     pu101
     C                   when      xxtype = '2'
     C                   clear                   pu201
     C                   movel     data500       detail
     C                   eval      %subst(detail:510) =
     C                             %subst(u0data:501)
    C*** Beg Add ***** 07/29/13 **************************************
     C                   eval      xxRlin# = %subst(u0data:199:8)
     C                   eval      xxLin# = %subst(u0data:17:8)
     C                   move      xxLin#        kklin#
     C     u2ky          chain     pu202                              99
     C                   if        *in99 = *on or
     C                             *in99 = *off and xxRlin# <> '00000000'
    C*** End Add ***** 07/29/13 **************************************
     C                   write     pu201
    C*** Beg Add ***** 07/29/13 **************************************
     C                   endif
    C*** End Add ***** 07/29/13 **************************************
     C                   endsl
    C*
     C                   endsr
