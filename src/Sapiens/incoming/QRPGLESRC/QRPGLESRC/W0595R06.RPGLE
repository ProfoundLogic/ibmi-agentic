     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
    F*
     Fwmwcl     if   e           k disk
     F                                     rename(pwc01:lwc01)
     Fwmwcp     uf a e             disk
     Fwatil     if   e           k disk
     F                                     rename(pti01:lti01)
     Fwatip     uf a e             disk
     Fwaf1l     if   e           k disk
     F                                     rename(pf101:lf101)
     Fwaf1p     uf a e             disk
     Fwaf2l     if   e           k disk
     F                                     rename(pf201:lf201)
     Fwaf2p     uf a e             disk
     Fwamel     if   e           k disk
     F                                     rename(pme01:lme01)
     Fwamep     uf a e             disk
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D psds          esds                  extname(szq1p)
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D kkco#           s                   like(wcco#)
     D kkfnd           s                   like(wcfnd)
     D kkfyr           s                   like(wcfyr)
     D kkcase          s                   like(wccase)
     D kkseq#          s                   like(wcseq#)
     D nseq#           s                   like(wcseq#)
     D sqseq#          s                   like(wcseq#)
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@kc
     C                   parm                    @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C*
     C     xxkey         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkseq#
     C*
     C     xxkeyp        klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C*
     C     sqkey         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    sqseq#
     C*
      *
    C*================================================================
    C*  Mainline
    C*================================================================
     C                   exsr      pgmstr
     C                   exsr      getdtm
     C*
     C                   exsr      duprcd
     C*
     C                   exsr      updatr
     C                   exsr      pgmend
     C*
    C*================================================================
    C* DUPRCD - Duplicate secondary records
    C*================================================================
     C     duprcd        begsr
     C*
     C     xxkey         setll     lti01
     C     *in99         doueq     *on
     C     xxkey         reade     lti01                                  99
B01  C                   if        *in99 = *off
     C                   eval      tiseq# = nseq#
     C                   write     pti01
     C                   endif
     C                   enddo
     C*
     C     xxkey         setll     lf101
     C     *in99         doueq     *on
     C     xxkey         reade     lf101                                  99
B01  C                   if        *in99 = *off
     C                   eval      f1seq# = nseq#
     C                   write     pf101
     C                   endif
     C                   enddo
     C*
     C     xxkey         setll     lf201
     C     *in99         doueq     *on
     C     xxkey         reade     lf201                                  99
B01  C                   if        *in99 = *off
     C                   eval      f2seq# = nseq#
     C                   write     pf201
     C                   endif
     C                   enddo
     C*
     C     xxkey         setll     lme01
     C     *in99         doueq     *on
     C     xxkey         reade     lme01                                  99
B01  C                   if        *in99 = *off
     C                   eval      meseq# = nseq#
     C                   write     pme01
     C                   endif
     C                   enddo
     C*
     C                   endsr
     C*================================================================
     C*  GETDTM - Gets date/time
     C*================================================================
     C     getdtm        begsr
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   endsr
     C*================================================================
     C*  MVMNTF - Move maintenance fields
     C*================================================================
     C     mvmntf        begsr
     C                   movel     q1user        wcuser
     C                   movel     q1pgm         wcpgm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         wcdt
     C                   movel     dstime        wctm
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   eval      *inlr = *on
     C                   return
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C* Get original master rcd info
     C     @@rrn         chain     pwc01                              99
B01  C                   if        *in99 = *off
     C                   move      wcco#         kkco#
     C                   move      wcfnd         kkfnd
     C                   move      wcfyr         kkfyr
     C                   move      wccase        kkcase
     C                   move      wcseq#        kkseq#
     C                   endif
     C*
     C* Get next seq#
     C                   move      *hival        sqseq#
     C     sqkey         setgt     lwc01
     C     xxkeyp        readpe    lwc01                                9999
B01  C                   if        *in99 = *off
     C                   eval      nseq# = wcseq# + 1
E01  C                   else
     C                   eval      nseq# = nseq# + 1
E01  C                   endif
     C*
     C                   endsr
    C*================================================================
    C* UPDATR - Update original record
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     pwc01                              99
     C*
B01  C                   if        *in99 = *off
     C                   exsr      writer
     C     @@rrn         chain     pwc01                              99
     C                   eval      wcrvsd = 'Y'
     C*
     C                   update    pwc01
E01  C                   endif
     C*
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C*
     C                   eval      wcsts = 'P'                                  Rsrv  wrksht status
     C                   eval      wccalc = *blank                              Rsrv  wrksht status
     C                   eval      wcdedt = 0                                   Rsrv  wrksht status
     C                   eval      wctaww = 0                                   Rsrv  wrksht status
     C                   eval      wciaww = 0                                   Rsrv  wrksht status
     C                   eval      wctr70 = 0                                   TIB comp rate @ 70%
     C                   eval      wctr75 = 0                                   TIB comp rate @ 75%
     C                   eval      wcirat = 0                                   IIB comp rate
     C                   eval      wcseq# = nseq#                               Rsrv  wrksht status
     C                   eval      wcorgs = kkseq#                              Rsrv  wrksht status
     C* Init user
     C*
     C                   movel     q1user        wciusr
     C                   movel     q1pgm         wcipgm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         wcidt
     C                   movel     dstime        wcitm
     C*
     C                   exsr      mvmntf
     C* Entry date
     C                   movel     srdto         wcendt
     C*
     C                   write     pwc01
     C                   feod      wmwcp
     C*
     C                   endsr
