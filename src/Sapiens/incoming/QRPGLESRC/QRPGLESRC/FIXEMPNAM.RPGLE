     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*********************************************************************
    H/TITLE FIXEMPNAM - FIX EMPLOYER NAMES
    H*********************************************************************
    H*  DESCRIPTION:  THIS PROGRAM WILL LOAD THE PROPER NAMES TO THE     *
    H*                QUOTE TRACKING AND LOCATION FILE.                  *
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*    COMMAND LINE                                                   *
    H*  CALLS TO:                                                        *
    H*                                                                   *
    H*                                                                   *
    H*  CREATED: 08/24/99  PROGRAMMER: C:33   SEQ#: WA091/19392          *
    H*********************************************************************
    H* REVISIONS:                                                        *
    H*                                                                   *
     H*  04/13/09 C:191 PROJECT 114WTU                                    *
     H*                 Load additional Entity/Location name field        *
     H*                 (ALNAM2) when updating Entity/Location            *
     H*                 records with the Employer Name.                   *
    H*                                                                   *
    H*********************************************************************
     Fwmeml     if   e           k disk
     Fwaqtl7    uf   e           k disk
     Fwmall4    uf   e           k disk
     Fqsysprt   O    f  132        printer oflind(*inof)
     Fqsysprt1  O    f  132        printer oflind(*inoa)
    D*
     D psds          esds                  extname(szq1p)
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     D @@update        s              1
     D @@co#           s              3
     D @@fnd           s              3
     D update          s                   like(@@update)
     D xxco#           s                   like(EMCO#)
     D xxfnd           s                   like(EMFND)
     D xxqte           s                   like(QTSTAT)
     D svname          s                   like(QTNAME)
     D aldldt_usa      s               D   datfmt(*USA)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    I*
     C/COPY SCOPYDTPL
    C*
    C*  EMPLOYER MASTER FILE
    C*
     C     emky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
    C*
    C*  AGENT QUOTE FILE
    C*
     C     qtky          klist
     C                   kfld                    EMCO#
     C                   kfld                    EMFND
     C                   kfld                    EMEMP#
     C                   kfld                    EMDIV
     C                   kfld                    xxqte
    C*
    C*  ENTITY/LOCATION FILE
    C*
     C     alky          klist
     C                   kfld                    EMCO#
     C                   kfld                    EMFND
     C                   kfld                    EMEMP#
     C                   kfld                    EMDIV
    C*___________________________________________________________________
    C*
     C     *entry        plist
     C                   parm                    @@update
     C                   parm                    @@co#
     C                   parm                    @@fnd
    C*
     C                   if        q1prms >= 1
     C                   move      @@update      update
     C                   else
     C                   move      'N'           update
     C                   endif
    C*
     C                   if        q1prms >= 2
     C                   move      @@co#         xxco#
     C                   move      *on           *in40
     C                   endif
    C*
     C                   if        q1prms >= 3
     C                   move      @@fnd         xxfnd
     C                   move      *on           *in41
     C                   endif
    C*
     C                   move      'W'           xxqte
    C*
     C                   move      *date         sys_date
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C                   except    head_qt
     C                   except    head_al
    C*___________________________________________________________________
    C*
     C                   select
     C                   when      *in40 = *off and *in41 = *off
     C                   read      pem01                                  99
     C                   when      *in40 = *on  and *in41 = *off
     C     xxco#         chain     pem01                              99
     C                   when      *in40 = *on  and *in41 = *on
     C     emky          chain     pem01                              99
     C                   endsl
    C*
     C                   dow       *in99 = *off
     C*
     C                   movel(p)  EMNAME        svname
    C*
    C*  UPDATE THE QUOTE TRACKING FILE
    C*
     C     qtky          chain     pqt01                              60
    C*
     C                   dow       *in60 = *off
    C*
     C                   if        QTNAME <> svname
     C                             or QTCHAP <> EMCHAP
     C*
     C                   if        *inof = *on
     C                   move      *off          *inof
     C                   except    head_qt
     C                   endif
     C*
     C                   except    detail_qt
    C*
     C                   adddur    1:*s          sys_time
    C*
     C                   movel(p)  EMNAME        QTNAME
     C                   movel(p)  EMCHAP        QTCHAP
    C*
     C                   movel(p)  q1user        QTUSER
     C                   movel(p)  q1pgm         QTPGM
     C     *cymd         move      sys_date      QTDT
     C                   move      sys_time      QTTIME
    C*
     C                   if        update = 'Y'
     C                   update    pqt01
     C                   endif
    C*
     C                   endif
    C*
     C     qtky          reade     pqt01                                  60
    C*
     C                   enddo
    C*
    C*  UPDATE ALL LOCATIONS IN ENTITY/LOCATION FILE                  HAVE
    C*  NOTE: THE LOGICAL USED (WMALL4) SELECTS ALELTP = 'L'.
    C*
     C     alky          chain     pal01                              60
    C*
     C                   dow       *in60 = *off
     C*
     C                   clear                   aldldt_usa
     C*
     C                   if        ALDLDT <> *zero
     C     *cymd         move      ALDLDT        aldldt_usa
     C                   endif
    C*
     C                   if        (ALDLDT = *zero or aldldt_usa > sys_date)
    C*** BEG CHG **** 04/13/09 *******************************************************
     C*******************           and ALNAME <> EMNAME
     C                              and (ALNAME <> EMNAME
     C                              or ALNAM2 <> EMNAM2)
    C*** END CHG **** 04/13/09 *******************************************************
     C*
     C                   if        *inoa = *on
     C                   move      *off          *inoa
     C                   except    head_al
     C                   endif
     C*
     C                   except    detail_al
    C*
     C                   adddur    1:*s          sys_time
    C*
     C                   movel(p)  EMNAME        ALNAME
    C*** BEG ADD **** 04/13/09 *******************************************************
     C                   movel(p)  EMNAM2        ALNAM2
    C*** END ADD **** 04/13/09 *******************************************************
    C*
     C                   movel(p)  q1user        ALUSER
     C                   movel(p)  q1pgm         ALPGM
     C     *cymd         move      sys_date      ALDT
     C                   move      sys_time      ALTIME
    C*
     C                   if        update = 'Y'
     C                   update    pal01
     C                   endif
    C*
     C                   endif
    C*
     C     alky          reade     pal01                                  60
    C*
     C                   enddo
    C*
     C                   select
     C                   when      *in40 = *off and *in41 = *off
     C                   read      pem01                                  99
     C                   when      *in40 = *on  and *in41 = *off
     C     xxco#         reade     pem01                                  99
     C                   when      *in40 = *on  and *in41 = *on
     C     emky          reade     pem01                                  99
     C                   endsl
    C*
E02  C                   enddo
    C*___________________________________________________________________
    C*
     C                   move      *on           *inlr
    C*
     OQSYSPRT   E            head_qt        2 03
     O                                           72 'Quote Tracking Changes'
     O          E            head_qt        2
     O                                              'Grp'
     O                                         +001 ' Employer'
     O                                         +001 '  Div'
     O                                         +001 'Employer Name'
     O                                         +013 'Quote Tracking Name'
     O                                         +010 'EMCHAP'
     O                                         +001 'QTCHAP'
     O          E            detail_qt      1
     O                       EMFND         z
     O                       EMEMP#        z   +001
     O                       EMDIV         z   +001
     O                       svname            +001
     O                       QTNAME            +001
     O                       EMCHAP        z   +004
     O                       QTCHAP        z   +004
     OQSYSPRT1  E            head_al        2 03
     O                                           72 'Location File Changes'
     O          E            head_al        2
     O                                              'Grp'
     O                                         +001 ' Employer'
     O                                         +001 '  Div'
     O                                         +001 'Employer Name'
     O          E            head_al        2
     O                                         +028 'Location Name'
     O          E            detail_al      1
     O                       EMFND         z
     O                       EMEMP#        z   +001
     O                       EMDIV         z   +001
     O                       EMNAME            +001
     O                       ALNAME            +001
