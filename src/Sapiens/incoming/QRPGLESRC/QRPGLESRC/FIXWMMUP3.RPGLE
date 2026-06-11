     *========================================================================*
     * (C) Copyright Stoneriver, Inc 2011.                                    *
     *========================================================================*
    H/TITLE FIXWMMUP3 - Reattach missing endorsements
     *========================================================================*
     * FIXWMMUP3 -Fix program for WMMUP.                                      *
     *                                                                        *
     * PROGRAM DESCRIPTION:                                                   *
     *   This program will read through employer detail, and if a policy has  *
     *   had a policy page run, it will determine if any of the policy page   *
     *   attached endorsements (such as WC040002 - CA Schedule of names)      *
     *   are missing and re-attach them.                                      *
     *                                                                        *
     * CREATED   : 10/26/11                                                   *
     * PROGRAMMER: C:65                                                       *
     * PROJECT   : SUPPORT #76082                                             *
     *========================================================================*
     FWMEML     IF   E           K Disk
     FWDELL     IF   E           K Disk
     Fwmmul5    IF a e           k disk
     Fwmrsl     if   e           k disk
     Fwmanl     if   e           k disk
     Fwmall     if   e           k disk
     Fwmshl1    if   e           k disk
     D
     D psds          esds                  extname(szq1p)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D
     D xx4001          c                   const('WC040001A')
     D xxFD01A         c                   const('Information Page')
     D xxfd01          c                   const('CA INFORMATION PAGE')
     D xxfd02          c                   const('CA SCHEDULE OF NAMED-
     D                                      INSUREDS')
     D xxfd03          c                   const('CA SCHEDULE OF LOCAT-
     D                                     IONS')
     D xxfd04          c                   const('CA SCHEDULE OF FORMS')
     D xxfd05          c                   const('CA SCHEDULE OF OPERA-
     D                                     TIONS')
     D xxFD55          c                   'Item 4-Ext of -
     D                                     Information Page'
     D xxFD56          c                   'Item 3D-Schedule of -
     D                                     Forms & Endorsements'
     D xxFD57          c                   'Item 1-Schedule of -
     D                                     Locations'
     D xxFD58          c                   'Item 1-Schedule of -
     D                                     Named Insureds'
     D xxtype          S                   Like(shtype)
     D cltid           s              3
     D xxform          s                   like(muform)
     D xxdesc          s                   like(mudesc)
     D xxend#          s                   like(muend#)
     D xxEnSt          s                   like(MUENST)
     D sysDt7          s              7  0
     D xxpa2           s              1
     D xxrtcd          s                   like(elrtcd)
     D xxanam          s              1
     D xxaloc          s              1
     D prtCA           s              1n
     D prtFL           s              1n
     D prtNJ           s              1n

     *========================================================================*
     * PLIST Definitions
     *========================================================================*

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
     *========================================================================*
     * KLIST definitions
     *========================================================================*
     ** Employer Detail file
     C     elky          KList
     C                   KFld                    EMCO#
     C                   KFld                    EMFND
     C                   KFld                    EMEMP#
     C                   KFld                    EMDIV

     ** Submission History file
     C     shky          KList
     C                   KFld                    EMCO#
     C                   KFld                    EMFND
     C                   KFld                    EMEMP#
     C                   KFld                    ELFYR
     C                   KFld                    XXTYPE
    C* Endorsement file
     C     muky1         klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elfyr
     C                   kfld                    elemp#
     C                   kfld                    xxform
     C                   kfld                    xxend#
    C* Rate Set Code file
     C     rsky          klist
     C                   kfld                    elco#
     C                   kfld                    xxrtcd
     *
     C     anky2         klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#

     *========================================================================*
     * Main
     *========================================================================*
     C                   call      'RTVCLTID'
     C                   parm                    cltid

     c                   move      'P'           XXTYPE
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT7
     c
      ** Check all employers.
     C                   read      pem01

B1   C                   DoW       not %eof

      ** Only check the state reporting division.
B2   C                   If        EMSTRP = 'Y'

      ** Given policy year.
     C     elky          Chain     PEL01                              60
B3   C                   dow       *in60 = *off

     C     shky          Chain     psh01                              60
     c                   if        *in60 = *off
     c                             and shstat = '3' or shstat = '4'
     c
     C                   move      elrtcd        xxrtcd
     C     rsky          chain     prs01                              60
     c
     c                   move      *off          *in95
     c                   move      *off          *in75
     C                   clear                   xxpa2
     C                   clear                   prtCA
     C                   clear                   prtNJ
     C                   clear                   prtFL
     c
     c                   if        *in60 = *off
     c                             and rsjrst = 'PA'
     c                             and ((elfnd = 2)or(elfnd = 101))
     c                             and cltid = '114'
     C                   move      'Y'           xxpa2
     C                   endif
     c
     c                   if        *in60 = *off
     c                             and rsjrst = 'CA'
     c                   eval      prtCA = *on
     C                   eval      *in75 = *on
     C                   endif
     c
     c                   if        *in60 = *off
     c                             and rsjrst = 'FL'
     c                   eval      prtFL = *on
     C                   endif
     c
     c                   if        *in60 = *off
     c                             and rsjrst = 'NJ'
     c                   eval      prtNJ = *on
     C                   endif
     c
     C                   if        prtCA = *on
     c                             or xxpa2 = 'Y'
     C                   move      *on           *in95
     C                   endif
     c
     C                   clear                   xxanam
     C                   clear                   xxaloc
     c
     C     anky2         chain     pan01                              60
     *
     C                   dow       *in60 = *off
     *
     C                   if        anstat = 'A'
     C                             and anefdt < elptdt
     *
     C                   if        andldt = *zero
     C                             or andldt > elpfdt
     C                             and andldt > anefdt                          not flat cancelled
     C
     C                   move      'Y'           xxanam
     *
     C                   endif
     *
     C                   endif
     *
     C     anky2         reade     pan01                                  60
     *
     C                   enddo
     c*
     C     anky2         chain     pal01                              60
     C                   dow       *in60 = *off
    C*
     C                   if        alefdt < elptdt
     C                             and ((aldldt > alefdt                         not flat cancelled
     C                             and aldldt > elpfdt) or aldldt = 0)
     c                             and alncci = 'Y'
     C                   move      'Y'           xxaloc
     *
     C                   endif
     *
     C     anky2         reade     pal01                                  60
     *
     C                   enddo
     c
     c* Check for attached endorsements and re-attach.
     c                   exsr      check
     c
     c                   endif                                                  *in60 = *off (wmshp)
     c
     c     elky          reade     pel01                                  60
     c                   enddo                                                  employer detail loop
     c
     c                   endif                                                  emstrp = 'Y'
     c
     C                   read      pem01
     c                   enddo                                                  employer master loop


     C                   Eval      *inlr = *on
     *========================================================================*
     c*
     c* Check - Verify if policy page attached endorsements are present.
     c*
     *========================================================================*
     c     check         begsr
     c
     C                   if        cltid = '132'
    C*
     C                   if        elfnd = 105
     C                             or elfnd = 106
     C                             or elfnd = 107
    C*
     C                   movel(p)  'DISCLOSR'    xxform
     C                   eval      xxdesc = 'PVT. POLICY & DISCLOSR NOTICE'
     C                   eval      xxend# = 0
     C                   eval      xxEnSt = '**'
    C*
     C     muky1         chain     pmu01                              60
     C                   if        *in60 = *on
     C                   exsr      attmu
     C                   endif
     C*
     C                   endif
     C                   endif
     C*
     C                   if        *in95 = *on
     C                             and *in75 = *on
    C*
    C* ADD CA DECLARATION PAGE FORM
    C*
     C                   movel(p)  xx4001        xxform
     C                   movel(p)  xxfd01        xxdesc
     C                   eval      xxend# = 0
    C*
     C     muky1         chain     pmu01                              60
     C                   if        *in60 = *on
     C                   exsr      attmu
     C                   endif
    C*
    C* ADD CA SCHEDULE OF NAMED INSUREDS FORM
    C*
     C                   movel(p)  'WC040002'    xxform
     C                   movel(p)  xxfd02        xxdesc
     C                   eval      xxend# = 0
    C*
     C                   if        xxanam = 'Y'
    C*
     C     muky1         chain     pmu01                              60
     C                   if        *in60 = *on
     C                   exsr      attmu
     C                   endif
    C*
     C                   endif
    C*
    C* ADD CA SCHEDULE OF LOCATIONS FORM
    C*
     C                   movel(p)  'WC040003'    xxform
     C                   movel(p)  xxfd03        xxdesc
     C                   eval      xxend# = 0
    C*
     C                   if        xxaloc = 'Y'
    C*
     C     muky1         chain     pmu01                              60
     C                   if        *in60 = *on
     C                   exsr      attmu
     C                   endif
    C*
     C                   endif
    C*
    C* ADD CA SCHEDULE OF FORMS FORM
    C*
     C                   movel(p)  'WC040004'    xxform
     C                   movel(p)  xxfd04        xxdesc
     C                   eval      xxend# = 0
    C*
     C     muky1         chain     pmu01                              60
     C                   if        *in60 = *on
     C                   exsr      attmu
     C                   endif
    C*
    C* ADD CA SCHEDULE OF OPERATIONS FORM
    C*
     C                   movel(p)  'WC040005'    xxform
     C                   movel(p)  xxfd05        xxdesc
     C                   eval      xxend# = 0
    C*
     C     muky1         chain     pmu01                              60
     C                   if        *in60 = *on
     C                   exsr      attmu
     C                   endif
    C*
     C                   endif
     C*
     C                   select
     C                   when      cltID = '139'
     C                   exsr      Add139End
     C                   when      cltID = '114'
     C                   exsr      Add114End
     C                   endsl
     c
     c                   endsr
    C*====================================================================
     C     attmu         begsr
    C*
     C                   move      elco#         muco#
     C                   move      elfnd         mufnd
     C                   move      elemp#        muemp#
     C                   move      eldiv         mudiv
     C                   move      elfyr         mufyr
     C                   clear                   muend#
     C                   clear                   museq#
     C                   movel     xxform        muform
     C*
     C                   select
     C                   when      xxEnSt = '**'
     C                   eval      MUENST = *blanks
     C                   when      xxEnSt <> *blanks
     C                   eval      MUENST = xxEnSt
     C                   other
     C                   movel     'CA'          muenst
     C                   endsl
     C*
     C                   movel     xxdesc        mudesc
     C                   move      elpfdt        mueedt
     C                   movel     'I'           muexfl
     C                   movel     'A'           mumnau
     C                   movel     'N'           muprnt
     C                   movel     'A'           mustat
     C                   movel     q1user        muuser
     C                   movel     q1pgm         mupgm
    C*
     C                   eval      MUDT   = sysDt7
     C                   eval      MUTIME = dstime
    C*
     C                   movel     q1user        muiusr
     C                   movel     q1pgm         muipgm
     C                   move      mudt          muidt
    C*
    C*  WRITE A RECORD TO THE EMPLOYER ATTACHED ENDORSEMENT FORMS FILE
    C*
     C                   write     pmu01
    C*
     C                   endsr
    C*====================================================================
    C* Add Endorsments for CLT139
    C*====================================================================
     C     Add139End     begsr
    C*
     C                   eval      xxForm = 'WC000001A'
     C                   if        prtNJ
     C                   eval      xxForm = 'WC000001B'
     C                   endif
     C                   eval      xxDesc = xxFD01A
     C                   eval      xxEnd# = 0
     C                   eval      xxEnSt = '**'
     C     muky1         chain     pmu01                              60
     C                   if        *in60 = *on
     C                   exsr      attmu
     C                   endif
    C*
     C                   if        prtFL
     C                   eval      xxEnSt = 'FL'
     C                   exsr      AddFLEnd
     C                   endif
    C*
     C                   clear                   xxEnSt
    C*
     C                   endsr
    C*
    C*====================================================================
    C* Add FL Endorsements
    C*====================================================================
     C     AddFLEnd      begsr
    C*
     C                   eval      xxForm = 'WC990655'
     C                   eval      xxDesc = xxFD55
     C                   eval      xxend# = 0
     C     muky1         chain     pmu01                              60
     C                   if        *in60 = *on
     C                   exsr      attmu
     C                   endif
    C*
     C                   eval      xxForm = 'WC990656'
     C                   eval      xxDesc = xxFD56
     C                   eval      xxend# = 0
     C     muky1         chain     pmu01                              60
     C                   if        *in60 = *on
     C                   exsr      attmu
     C                   endif
    C*
     C                   if        xxanam <> 'Y'
     C                   eval      xxForm = 'WC990657'
     C                   eval      xxDesc = xxFD57
     C                   eval      xxend# = 0
     C     muky1         chain     pmu01                              60
     C                   if        *in60 = *on
     C                   exsr      attmu
     C                   endif
     C                   endif

     C                   if        xxanam = 'Y'
     C                   eval      xxForm = 'WC990658'
     C                   eval      xxDesc = xxFD58
     C                   eval      xxend# = 0
     C     muky1         chain     pmu01                              60
     C                   if        *in60 = *on
     C                   exsr      attmu
     C                   endif
     C                   endif
    C*
     C                   endsr
     *====================================================================
     * Add Endorsments for CLT114
     *====================================================================
     C     Add114End     begsr
     *
     *  Add clt114 group 2 form
     *
     c                   if        cltid = '114'
     c                             and xxpa2 = 'Y'
     c                             and ((elfnd = 2) or (elfnd = 101))
     c                   movel(p)  'ACT477LIAF1' xxform
     c     'ACT 477 LIAB'cat(p)    'ILITY':0     xxdesc
     c                   cat(p)    'FORM 1':1    xxdesc
     c                   eval      xxend# = 0
     c     muky1         chain     pmu01
     *
     c                   if        not %found
     c                   exsr      attmu
     c                   endif

     c                   endif

     c                   endsr
     *
     *====================================================================
