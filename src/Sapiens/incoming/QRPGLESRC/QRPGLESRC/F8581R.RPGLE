    H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2008
     H/TITLE F8581R
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H**************************************************************************
    H*  F8581R - Coventry EOR Financial Process - Transfer program 1
    H*
     H**************************************************************************
    H*  Description - Updates conversion date/time in FIX9P since
    H*                CPYFRMPCD succeeded.
    H*
    H*  Called from: F8581C - Coventry EOR Financial Process - Copy Doc     to
    H*  Calls to   : F8582R - Coventry EOR Financial Process - Transfer     pro
    H*
     H**************************************************************************
    H*  Created on 02/18/2008 by C:062 for Project 129MQN
     H**************************************************************************
    H*  Revisions:
    H*
     H**************************************************************************
     Ffix0l1    uf   e             disk
     Ffix1p     o    e             disk
     Ffix2p     o    e             disk
     Ffix3p     o    e             disk
     Ffix4p     o    e             disk
     Ffix5p     o    e             disk
     Ffix6p     o    e             disk
     Ffix7p     o    e             disk
     Ffix8p     o    e             disk
     Ffixap     o    e             disk
     Ffixbp     o    e             disk
     Ffixcp     o    e             disk
     Ffix9p     uf   e             disk
    F*
    D*
     D/COPY SCOPYWW01I
    D*================================================================
    D* Program constants.
    D*================================================================
     D comapi          c                   const('FA007R')
    D*================================================================
    D*  User defined data structures
    D*================================================================
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    D*
     D                 ds
     D recona                         2
     D recon                          2  0 overlay(recona)
    D*
     D x1hfilehd     e ds                  extname(fix1p)
     D x2bpclaim     e ds                  extname(fix2p)
     D x3dprovdr     e ds                  extname(fix3p)
     D x4fbillhd     e ds                  extname(fix4p)
     D x5gbilldt     e ds                  extname(fix5p)
     D x6jmshdrl     e ds                  extname(fix6p)
     D x7kmstext     e ds                  extname(fix7p)
     D x8mdiagno     e ds                  extname(fix8p)
     D xanbillsu     e ds                  extname(fixap)
     D xbtfiletr     e ds                  extname(fixbp)
     D xczsmr        e ds                  extname(fixcp)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##dcid          s             12
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##fyr           s              3  0
     D ##rrn           s              9  0
     D ##sub#          s              5  0
     D @@copy          s              1
     D @@dcid          s             12
     D @@mbr#          s             10
     D @@rrn           s              9  0
     D wrtrec          s              1
     D recrm           s              3  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D test            s              1
     D xxprfl          s                   like(x9prfl)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@rrn
     C                   parm                    @@dcid
     C                   parm                    @@mbr#
     C                   parm                    @@copy
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
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C                   parm                    ##sub#
     C                   parm                    ##fyr
    C*================================================================
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
     C                   clear                   xxprfl
     C                   call      comapi        compl                  99
    C*
B01  C                   if        @@copy = 'Y'
     C                   read      px001                                  99
B02  C                   if        *in99 = *off
     C*
B03  C                   if        %subst(x0data:19:5) =  'H301 '
     C*
B04  C                   dow       *in99 = *off
     C*
B03  C                   if        %subst(x0data:19:5) =  'H301 ' or
B03  C                             %subst(x0data:18:5) =  'T301 '
     C                   eval      wrtrec = *on
     C                   else
B03  C                   if        %subst(x0data:15:5) =  'B301 '
    C*
     C                   eval      recona = %subst(x0data:13:2)
     C                   eval      wrtrec = *off
    C*
     C                   if        recon  = 0 or %rem(recon:2) = 0
     C                   eval      wrtrec = *on
     C                   endif
     C*
     C                   endif
     C                   endif
     C*
     C                   if        wrtrec = *on
     C                   exsr      writer
     C                   endif
     C*
     C                   movel     'Y'           x0prfl
     C                   update    px001
     C                   read      px001                                  99
E04  C                   enddo
    C*
X03  C                   else
    C*
     C                   eval      xxprfl ='E'
    C*
E03  C                   endif
    C*
     C                   exsr      updat9
    C*
E02  C                   endif
E01  C                   endif
    C*
    C* UPDATE WCMC FILES
    C*
     C                   call      'F8582R'
     C                   parm      @@dcid        ##dcid
     C                   parm      @@rrn         ##rrn
    C*
     C                   eval      *inlr = *on
    C*
     C                   endsr
    C*================================================================
    C* UPDAT9 - Update FIRST HEALTH Bill Transmission File (FAX9P).
    C*================================================================
     C     updat9        begsr
    C*
     C     @@rrn         chain     px901                              99
     C                   if        *in99 = *off
    C*
     C                   if        x9cvdt = *zeros
    C*
     C                   movel(p)  q1user        x9user
     C                   movel(p)  q1pgm         x9pgm
    C*
     C                   time                    dsdttm
    C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         x9cvdt
     C                   movel     srdto         x9dt
    C*
     C                   eval      x9cvtm = dstime
     C                   endif
    C*
     C                   eval      x9prfl = xxprfl
     C                   update    px901
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRITER - FIRSR HEALTH Bill Process Files (FAXnP).
    C*================================================================
     C     writer        begsr
    C*
     C                   select
    C*                  ----------------------------------------------
     C                   when      %subst(x0data:19:5)  = 'H301 '
     C                   clear                   px101
     C                   movel     x0data        x1hfilehd
     C                   movel(p)  @@dcid        x1dcid
     C                   movel(p)  @@mbr#        x1mbr#
    C*
     C                   write     px101
    C*                  ----------------------------------------------
     C                   when      %subst(x0data:15:5)  = 'B301 '
     C                   clear                   px201
     C                   movel     x0data        x2bpclaim
    C*
     C                   write     px201
    C*                  ----------------------------------------------
     C                   when      %subst(x0data:15:5)  = 'D301 '
     C                   clear                   px301
     C                   movel     x0data        x3dprovdr
    C*
     C                   write     px301
    C*                  ----------------------------------------------
     C                   when      %subst(x0data:15:5)  = 'F301 '
     C                   clear                   px401
     C                   movel     x0data        x4fbillhd
     C                   movel(p)  @@dcid        x4dcid
     C                   movel(p)  @@mbr#        x4mbr#
    C*
     C                   write     px401
    C*                  ----------------------------------------------
     C                   when      %subst(x0data:15:5)  = 'G301 '
     C                   clear                   px501
     C                   movel     x0data        x5gbilldt
    C*
     C                   write     px501
    C*                  ----------------------------------------------
     C                   when      %subst(x0data:15:5)  = 'J301 '
     C                   clear                   px601
     C                   movel     x0data        x6jmshdrl
    C*
     C                   write     px601
    C*                  ----------------------------------------------
     C                   when      %subst(x0data:15:5)  = 'K301 '
     C                   clear                   px701
     C                   movel     x0data        x7kmstext
    C*
     C                   write     px701
    C*                  ----------------------------------------------
     C                   when      %subst(x0data:15:5)  = 'M301 '
     C                   clear                   px801
     C                   movel     x0data        x8mdiagno
    C*
     C                   write     px801
    C*                  ----------------------------------------------
     C                   when      %subst(x0data:15:5)  = 'N301 '
     C                   clear                   pxa01
     C                   movel     x0data        xanbillsu
    C*
     C                   write     pxa01
    C*                  ----------------------------------------------
     C                   when      %subst(x0data:18:5)  = 'T301 '
     C                   clear                   pxb01
     C                   movel     x0data        xbtfiletr
     C                   movel(p)  @@dcid        xbdcid
     C                   movel(p)  @@mbr#        xbmbr#
    C*
     C                   write     pxb01
    C*                  ----------------------------------------------
     C                   when      %subst(x0data:15:5)  = 'C301 '
     C                   clear                   pxc01
     C                   movel     x0data        xczsmr
    C*
     C                   write     pxc01
     C                   endsl
    C*
     C                   endsr
