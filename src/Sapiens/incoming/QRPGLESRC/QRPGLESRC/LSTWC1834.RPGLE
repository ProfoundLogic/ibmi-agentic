      * (C) Copyright StoneRiver, Inc., 2008                                                       *
      *============================================================================================*
      /title LSTWC1834 - List Policies Eligible for FL 07/01/09 Decrease                           *
     H option(*nodebugio: *srcstmt)                                                                *
     H debug(*yes)                                                                                 *
      *============================================================================================*
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/10/10 C:189 SUPPORT: 71947                                 *
     H*                 Client wants the ouput in excel with some     *
     H*                 extras fields                                 *
     H*                                                               *
     H*****************************************************************
     fwdell     if   e           k disk    rename(pel01 : pel0l)
     fwmeml     if   e           k disk    rename(pem01 : pem0l)
     fwmrsl     if   e           k disk    rename(prs01 : prs0l)
     fsmcol     if   e           k disk
     **********BEG ADD 3/10/10**************************************************
     fflpof     o    e           k disk
     **********END ADD 3/10/10**************************************************
     fqsysprt   o    f  180        printer oflind(*inof)
     d psds          esds                  extname(szq1p)
     d status          s             20
     d sys_date        s               d   datfmt(*usa)
     d sys_time        s               t   timfmt(*hms:)
     c     rsky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELRTCD
     c     emky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
     c     *MDY          move      Q1DT          sys_date
     c                   move      Q1TIME        sys_time
     c                   move      *all'_'       line            108
     c                   eval      *inof = *on
     c                   read      pel0l
     c                   dow       not(%eof)
     c                   clear                   xxflrc            1
     c                   clear                   xxandt
     c                   if        ELANDT <> 0
     c                   z-add     ELANDT        xxandt            7 0
     c                   add       10000         xxandt
     c                   endif
     c     rsky          chain     prs0l
     c                   move      rsjrst        xxst              2
     c     xxst          ifeq      'FL'
     c                   select
     c     ELPFDT        whenlt    1090701
     c     xxandt        andge     1090401
     c     xxandt        andlt     1090701
     c     ELPTDT        andgt     1090701
     c                   move      'Y'           xxflrc            1
     c     ELPFDT        whenge    1090401
     c     ELPFDT        andlt     1090701
     c     ELANDT        andge     1090401
     c     ELANDT        andlt     1090701
     c                   move      'Y'           xxflrc
     c     ELPFDT        whenge    1090401
     c     ELPFDT        andlt     1090701
     C     ELPTDT        andgt     1090701
     c     ELANDT        andeq     0
     c                   move      'Y'           xxflrc
     c                   endsl
     c     xxflrc        ifeq      'Y'
     c                   if        *inof = *on
     C     ELCO#         chain     smcol
     c                   except    header
     c                   eval      *inof = *off
     c                   endif
     c     emky          chain     pem0l
     c                   select
     c                   when      ELFAF = ' '
     c                   eval      status = 'Not billed'
     c                   when      ELFAF = 'A'
     c                   eval      status = 'Audited'
     c                   when      ELFAF = 'E'
     c                   eval      status = 'Estimated'
     c                   when      ELFAF = 'R'
     c                   eval      status = 'Revised'
     c                   endsl
     **********BEG ADD 3/10/10**************************************************
     c                   exsr      polrcd
     **********END ADD 3/10/10**************************************************
     c                   except    detail
     c                   endif
     c                   endif
     c                   read      pel0l
     c                   enddo
     c                   seton                                        lr
     **********BEG ADD 3/10/10**************************************************
     C*
     C     polrcd        begsr
     C                   eval      lpfnd = elfnd
     C                   eval      lpemp#= elemp#
     C                   eval      lpdiv = eldiv
     C                   eval      lpfyr = elfyr
     C                   eval      lpname = emname
     C                   eval      lpsts  = status
     C                   eval      lpmad1 = emmad1
     C                   eval      lpmad2 = emmad2
     C                   eval      lpmcty = emmcty
     C                   eval      lpmst  = emmst
     C                   eval      lpmzip = emmzip
     C                   eval      lpbscd = elbscd
     C                   write     pof01
     C                   endsr
     C*
     **********END ADD 3/10/10**************************************************
     oqsysprt   e            header         1 06
     O                       sys_time
     o                       CONAME              60
     O                       sys_date           101
     o          e            header         2
     o                       q1pgm
     o                                           60 'List of FL Policies Needin-
     o                                              g Rate Change'
     o                                           95 'PAGE:'
     o                       page          z    101
     o          e            header      0  0
     o                       line
     o          e            header      0  2
     o                                         +  2 'Grp'
     o                                         +  3 'Employer'
     o                                         +  4 'Div'
     o                                         +  2 'Fyr'
     o                                         +  2 'Employer Name'
     o                                         + 29 'Status'
     o          e            detail         1
     o                       ELFND         z   +  2
     o                       ELEMP#        z   +  2
     o                       ELDIV         3   +  2
     o                       ELFYR         z   +  2
     o                       EMNAME            +  2
     o                       status            +  2
