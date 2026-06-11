    H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2006
     H/TITLE F8281R
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H**************************************************************************
    H*  F8281R - First Health EOR Financial Process - Transfer program 1
    H*
     H**************************************************************************
    H*  Description - Updates conversion date/time in FAX9P since
    H*                CPYFRMPCD succeeded.
    H*
    H*  Called from: F8281C - First Health EOR Financial Process - Copy Doc to
    H*  Calls to   : F8282R - First Health EOR Financial Process - Transfer pro
    H*
     H**************************************************************************
    H*  Created on 05/24/2006 by C:163 for Project 139MCV from client 139
    H*  Based on F8261R from Client 135
     H**************************************************************************
    H*  Revisions:
    H*
     H**************************************************************************
     Ffax0l1    uf   e             disk
     Ffax1p     o    e             disk
     Ffax2p     o    e             disk
     Ffax3p     o    e             disk
     Ffax4p     o    e             disk
     Ffax5p     o    e             disk
     Ffax6p     o    e             disk
     Ffax7p     o    e             disk
     Ffax8p     o    e             disk
     Ffaxap     o    e             disk
     Ffaxbp     o    e             disk
     Ffax9p     uf   e             disk
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
     D x1hfilehd     e ds                  extname(fax1p)
     D x2bpclaim     e ds                  extname(fax2p)
     D x3dprovdr     e ds                  extname(fax3p)
     D x4fbillhd     e ds                  extname(fax4p)
     D x5gbilldt     e ds                  extname(fax5p)
     D x6jmshdrl     e ds                  extname(fax6p)
     D x7kmstext     e ds                  extname(fax7p)
     D x8mdiagno     e ds                  extname(fax8p)
     D xanbillsu     e ds                  extname(faxap)
     D xbtfiletr     e ds                  extname(faxbp)
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
B03  C                   if        %subst(x0data:19:5) =  'H001 '
     C*
B04  C                   dow       *in99 = *off
     C*
B03  C                   if        %subst(x0data:19:5) =  'H001 ' or
B03  C                             %subst(x0data:18:5) =  'T001 '
     C                   eval      wrtrec = *on
     C                   else
B03  C                   if        %subst(x0data:15:5) =  'B001 '
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
     C                   call      'F8282R'
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
     C                   when      %subst(x0data:19:5)  = 'H001 '
     C                   clear                   px101
     C                   movel     x0data        x1hfilehd
     C                   movel(p)  @@dcid        x1dcid
     C                   movel(p)  @@mbr#        x1mbr#
    C*
     C                   write     px101
    C*                  ----------------------------------------------
     C                   when      %subst(x0data:15:5)  = 'B001 '
     C                   clear                   px201
     C                   movel     x0data        x2bpclaim
    C*
     C                   write     px201
    C*                  ----------------------------------------------
     C                   when      %subst(x0data:15:5)  = 'D001 '
     C                   clear                   px301
     C                   movel     x0data        x3dprovdr
    C*
     C                   write     px301
    C*                  ----------------------------------------------
     C                   when      %subst(x0data:15:5)  = 'F001 '
     C                   clear                   px401
     C                   movel     x0data        x4fbillhd
     C                   movel(p)  @@dcid        x4dcid
     C                   movel(p)  @@mbr#        x4mbr#
    C*
     C                   write     px401
    C*                  ----------------------------------------------
     C                   when      %subst(x0data:15:5)  = 'G001 '
     C                   clear                   px501
     C                   movel     x0data        x5gbilldt
    C*
     C                   write     px501
    C*                  ----------------------------------------------
     C                   when      %subst(x0data:15:5)  = 'J001 '
     C                   clear                   px601
     C                   movel     x0data        x6jmshdrl
    C*
     C                   write     px601
    C*                  ----------------------------------------------
     C                   when      %subst(x0data:15:5)  = 'K001 '
     C                   clear                   px701
     C                   movel     x0data        x7kmstext
    C*
     C                   write     px701
    C*                  ----------------------------------------------
     C                   when      %subst(x0data:15:5)  = 'M001 '
     C                   clear                   px801
     C                   movel     x0data        x8mdiagno
    C*
     C                   write     px801
    C*                  ----------------------------------------------
     C                   when      %subst(x0data:15:5)  = 'N001 '
     C                   clear                   pxa01
     C                   movel     x0data        xanbillsu
    C*
     C                   write     pxa01
    C*                  ----------------------------------------------
     C                   when      %subst(x0data:18:5)  = 'T001 '
     C                   clear                   pxb01
     C                   movel     x0data        xbtfiletr
     C                   movel(p)  @@dcid        xbdcid
     C                   movel(p)  @@mbr#        xbmbr#
    C*
     C                   write     pxb01
    C*                  ----------------------------------------------
     C                   endsl
    C*
     C                   endsr
