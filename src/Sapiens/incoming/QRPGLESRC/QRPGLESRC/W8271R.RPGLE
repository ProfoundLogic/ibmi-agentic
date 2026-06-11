    H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2006
     H/TITLE W8271R
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H**************************************************************************
    H*  W8271R     : First Health Claims Ack - Transfer Program 1
    H*
    H*  Description: Updates conversion date/time in WZC9P since
    H*               CPYFRMPCD succeeded.
    H*
    H*  Called from: W8271C - First Health Claims Ack - Copy Doc to WZC0P Mbr
    H*  Calls to   : W8272R - First Health Claims Ack - Transfer Program 2
    H*
     H**************************************************************************
     H*  Created on 05/24/2006 by C:163 for Project 139MCV from client 139
     H**************************************************************************
    H*  Revisions:
    H*
     H**************************************************************************
     Fwzc0l     uf   e             disk
     Fwzc1p     o    e             disk
     Fwzc2p     o    e             disk
     Fwzc3p     o    e             disk
     Fwzc9p     uf   e             disk
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
     D header        e ds                  extname(wzc1p)
     D detail        e ds                  extname(wzc2p)
     D trailer       e ds                  extname(wzc3p)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D $$co#           s              3
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
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D test            s              1
     D xx#err          s                   like(c9#err)
     D xxprfl          s                   like(c9prfl)
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
     C                   if        @@copy = 'Y'
     C                   read      pc001                                  99
     C                   if        *in99 = *off
     C*
     C                   movel     c0data        test
     C                   if        test <> 'A'
     C                   eval      xxprfl ='E'
     C                   eval      *in99  =*on
     C                   endif
     C*
     C                   dow       *in99 = *off
     C                   exsr      writer
     C                   movel     'Y'           c0prfl
     C                   update    pc001
     C                   read      pc001                                  99
     C                   enddo
     C                   endif
     C                   exsr      updat9
     C                   endif
    C*
     C                   eval      *inlr = *on
    C*
     C                   endsr
    C*================================================================
    C* UPDAT9 - Update First Health Claims Ack Transmission File (wzc9p).
    C*================================================================
     C     updat9        begsr
    C*
     C     @@rrn         chain     pc901                              99
     C                   if        *in99 = *off
    C*
     C                   if        c9cvdt = *zeros
    C*
     C                   movel(p)  q1user        c9user
     C                   movel(p)  q1pgm         c9pgm
    C*
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
    C*
     C                   movel     srdto         c9cvdt
     C                   eval      c9cvtm = dstime
     C*
     C                   movel     srdto         c9prdt
     C                   eval      c9prtm = dstime
     C*
     C                   movel     srdto         c9dt
     C                   eval      c9time = dstime
     C*
     C                   endif
    C*
     C                   eval      c9#err = xx#err
     C                   eval      c9prfl = xxprfl
     C                   update    pc901
    C*
    C* Copy document to First Health folder (FIRSTHEA)
    C*
     C                   move      ##co#         $$co#
     C                   call      'W8272C'
     C                   parm                    $$co#
     C                   parm      @@dcid        ##dcid
    C*
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRITER - First Health Claims Ack Process File (wzc1p).
    C*================================================================
     C     writer        begsr
    C*
     C                   movel     c0data        test
    C*
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
    C*
     C                   select
    C*
     C                   when      test = 'A'
     C                   clear                   pc101
     C                   clear                   xx#err
     C                   movel     c0data        header
     C                   movel(p)  @@dcid        c1dcid
     C                   movel(p)  @@mbr#        c1mbr#
     C                   movel     srdto         c1prdt
     C                   movel     'Y'           c1prfl
     C                   write     pc101
    C*
     C                   if        c1trid = *blanks
     C                   eval      xxprfl ='T'
     C                   endif
    C*
     C                   when      test = 'E'
     C                   clear                   pc201
     C                   movel     c0data        detail
     C                   movel(p)  @@dcid        c2dcid
     C                   movel(p)  @@mbr#        c2mbr#
     C                   movel     srdto         c2prdt
     C                   movel     'E'           c2prfl
     C                   eval      xx#err = xx#err + 1
     C                   write     pc201
    C*
     C                   when      test = 'Z'
     C                   clear                   pc301
     C                   movel     c0data        trailer
     C                   movel(p)  @@dcid        c3dcid
     C                   movel(p)  @@mbr#        c3mbr#
     C                   movel     srdto         c3prdt
     C                   movel     'Y'           c3prfl
     C                   write     pc301
    C*
     C                   endsl
    C*
     C                   endsr
