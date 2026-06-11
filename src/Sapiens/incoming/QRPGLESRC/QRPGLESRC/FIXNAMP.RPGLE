    H*  (C) Copyright Progressive Data Solutions, Inc., 2006
    H*********************************************************************
    H/TITLE FIXNAMP - Populate new personal name fields for name files.
    H*********************************************************************
    H* CALLED FROM:                                                      *
    H*   COMMAND LINE                                                    *
    H* CALLS TO:                                                         *
    H*   NONE                                                            *
    H*                                                                   *
    H* CREATED:  04/25/06     C:65     PROJECT: 132WAY                   *
    H*********************************************************************
    H* REVISIONS:                                                        *
    H*                                                                   *
    H*********************************************************************

     FWMEML     uf   e           k disk
     FWMALL     uf   e           k disk
     FWMANL     uf   e           k disk
     Fqsysprt   O    f  132        printer oflind(*inof)
     Fqsysprt1  O    f  132        printer oflind(*inoa)
     Dpsds           esds                  extname(szq1p)
     Dsys_date         s               d   datfmt(*usa)
     D xxname          s                   like(emname)
     D xxnam2          s                   like(emnam2)
     D @@update        s              1
     D lo              c                   const('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D up              c                   const('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
    C*
    C*  ENTITY/LOCATION FILE
    C*
     C     alky          klist
     C                   kfld                    XXCO#
     C                   kfld                    XXFND
     C                   kfld                    XXEMP#
     C                   kfld                    XXDIV
    C*
    C* ADDITIONAL NAMED INSURED FILE
    C*
     C     anky          klist
     C                   kfld                    XXCO#
     C                   kfld                    XXFND
     C                   kfld                    XXEMP#
     C                   kfld                    XXDIV

     C     *entry        plist
     C                   parm                    @@update
    C*
    C* Set up update mode.
    C*
     C                   move      *off          *in66
    C*
     c                   if        @@update = 'Y'
     c                   move      *on           *in66
     c                   endif
    C*
     C     *usa          move      *date         sys_date
     c                   move      *on           *inof
     c                   move      *on           *inoa
     c*
     c* Read through all employer master.
     c*
     C                   read      pem01                                  60
     C                   dow       not *in60

     C                   z-add     EMCO#         XXCO#             3 0
     C                   z-add     EMFND         XXFND             3 0
     C                   z-add     EMEMP#        XXEMP#            9 0
     C                   z-add     EMDIV         XXDIV             5 0

     c                   move      *blank        xxupd             1
     c*
     c* Check if Employer name should be updated as a personal name.
     c*
     c     ',':1         scan      emname                                 70
     c     ',':1         scan      emnam2                                 71

     c                   if        *in70 = *on or *in71 = *on

     C     lo:up         xlate(p)  emname        xxname
     C     lo:up         xlate(p)  emnam2        xxnam2

     c     ', INC':5     scan      xxname                                 72
     c  n72',INC':4      scan      xxname                                 72
     c  n72', LLC':5     scan      xxname                                 72
     c  n72',LLC':4      scan      xxname                                 72
     c     ', INC':5     scan      xxnam2                                 73
     c  n73',INC':4      scan      xxnam2                                 73
     c  n73', LLC':5     scan      xxnam2                                 73
     c  n73',LLC':4      scan      xxnam2                                 73

     C                   move      'WMEMP'       XXFILE            5
     C                   z-add     0             XXLOC             5 0
     C                   movel(p)  EMNAME        SVNAME          100
     c                   move      EMNAM2        SVNAME
     c*
     c* Mark name for update if not incorporated.
     c*
     c                   if        *in72 = *off and *in73 = *off

     c                   move      'Y'           emnamp
     c                   move      'Y'           xxupd

     c                   if        *inof = *on
     c                   except    head
     c                   move      *off          *inof
     c                   endif
     c                   except    detail

     c                   else

     c                   if        *inoa = *on
     c                   except    headn
     c                   move      *off          *inoa
     c                   endif
     c                   except    detailn

     c                   endif

     c                   endif
     c*
     c* Check if Employer Federal name should be updated as a personal name.
     c*
     c     ',':1         scan      emfdnm                                 70
     c     ',':1         scan      emfdn2                                 71

     c                   if        *in70 = *on or *in71 = *on

     C     lo:up         xlate(p)  emfdnm        xxname
     C     lo:up         xlate(p)  emfdn2        xxnam2

     c     ', INC':5     scan      xxname                                 72
     c  n72',INC':4      scan      xxname                                 72
     c  n72', LLC':5     scan      xxname                                 72
     c  n72',LLC':4      scan      xxname                                 72
     c     ', INC':5     scan      xxnam2                                 73
     c  n73',INC':4      scan      xxnam2                                 73
     c  n73', LLC':5     scan      xxnam2                                 73
     c  n73',LLC':4      scan      xxnam2                                 73

     C                   move      'WMEMP'       XXFILE            5
     C                   z-add     0             XXLOC             5 0
     C                   movel(p)  EMFDNM        SVNAME          100
     c                   move      EMFDN2        SVNAME
     c*
     c* Mark name for update if not incorporated.
     c*
     c                   if        *in72 = *off and *in73 = *off

     c                   move      'Y'           emfdnp
     c                   move      'Y'           xxupd

     c                   if        *inof = *on
     c                   except    head
     c                   move      *off          *inof
     c                   endif
     c                   except    detail

     c                   else

     c                   if        *inoa = *on
     c                   except    headn
     c                   move      *off          *inoa
     c                   endif
     c                   except    detailn

     c                   endif

     c                   endif
     c*
     c* Update employer detail with new flag settings.
     c*
     c                   if        xxupd = 'Y'

     C     *cymd         move      sys_date      EMDT
     C                   time                    EMTIME
     C                   eval      EMUSER = Q1USER
     C                   eval      EMPGM  = Q1PGM

     C   66              update    pem01

     C                   endif
     c*
     c* Check additional locations now within WMALP.
     c*
     c     alky          chain     pal01                              50
     c     *in50         doweq     *off

     c                   move      *blank        xxupd             1
     c*
     c* Check if name should be updated as a personal name.
     c*
     c     ',':1         scan      alname                                 70

     c                   if        *in70 = *on

     C     lo:up         xlate(p)  alname        xxname

     c     ', INC':5     scan      xxname                                 72
     c  n72',INC':4      scan      xxname                                 72
     c  n72', LLC':5     scan      xxname                                 72
     c  n72',LLC':4      scan      xxname                                 72

     C                   move      'WMALP'       XXFILE            5
     C                   move      ALELID        XXLOC             5 0
     C                   movel(p)  ALNAME        SVNAME          100
     c*
     c* Mark name for update if not incorporated.
     c*
     c                   if        *in72 = *off

     c                   move      'Y'           alnamp
     c                   move      'Y'           xxupd

     c                   if        *inof = *on
     c                   except    head
     c                   move      *off          *inof
     c                   endif
     c                   except    detail

     c                   else

     c                   if        *inoa = *on
     c                   except    headn
     c                   move      *off          *inoa
     c                   endif
     c                   except    detailn

     c                   endif

     c                   endif
     c*
     c* Check if Federal name should be updated as a personal name.
     c*
     c     ',':1         scan      alfdnm                                 70

     c                   if        *in70 = *on

     C     lo:up         xlate(p)  alfdnm        xxname

     c     ', INC':5     scan      xxname                                 72
     c  n72',INC':4      scan      xxname                                 72
     c  n72', LLC':5     scan      xxname                                 72
     c  n72',LLC':4      scan      xxname                                 72

     C                   move      'WMALP'       XXFILE            5
     C                   move      ALELID        XXLOC             5 0
     C                   movel(p)  ALFDNM        SVNAME          100
     c*
     c* Mark name for update if not incorporated.
     c*
     c                   if        *in72 = *off

     c                   move      'Y'           alfdnp
     c                   move      'Y'           xxupd

     c                   if        *inof = *on
     c                   except    head
     c                   move      *off          *inof
     c                   endif
     c                   except    detail

     c                   else

     c                   if        *inoa = *on
     c                   except    headn
     c                   move      *off          *inoa
     c                   endif
     c                   except    detailn

     c                   endif

     c                   endif
     c*
     c* Update additional location/entity with new flag settings.
     c*
     c                   if        xxupd = 'Y'

     C     *cymd         move      sys_date      ALDT
     C                   time                    ALTIME
     C                   eval      ALUSER = Q1USER
     C                   eval      ALPGM  = Q1PGM

     C   66              update    pal01

     C                   endif
     c*
     c* Read next location/entity.
     c*
     c     alky          reade     pal01                                  50
     c                   enddo
     c*
     c* Check additional named insureds now within WMANP.
     c*
     c     anky          chain     pan01                              50
     c     *in50         doweq     *off

     c                   move      *blank        xxupd             1
     c*
     c* Check if Employer name should be updated as a personal name.
     c*
     c     ',':1         scan      anname                                 70
     c     ',':1         scan      annam2                                 71

     c                   if        *in70 = *on or *in71 = *on

     C     lo:up         xlate(p)  anname        xxname
     C     lo:up         xlate(p)  annam2        xxnam2

     c     ', INC':5     scan      xxname                                 72
     c  n72',INC':4      scan      xxname                                 72
     c  n72', LLC':5     scan      xxname                                 72
     c  n72',LLC':4      scan      xxname                                 72
     c     ', INC':5     scan      xxnam2                                 73
     c  n73',INC':4      scan      xxnam2                                 73
     c  n73', LLC':5     scan      xxnam2                                 73
     c  n73',LLC':4      scan      xxnam2                                 73

     C                   move      'WMANP'       XXFILE            5
     C                   z-add     0             XXLOC             5 0
     C                   movel(p)  ANNAME        SVNAME          100
     c                   movel(p)  ANNAM2        XXNM60           60
     c                   move      XXNM60        SVNAME
     c*
     c* Mark name for update if not incorporated.
     c*
     c                   if        *in72 = *off and *in73 = *off

     c                   move      'Y'           annamp
     c                   move      'Y'           xxupd

     c                   if        *inof = *on
     c                   except    head
     c                   move      *off          *inof
     c                   endif
     c                   except    detail

     c                   else

     c                   if        *inoa = *on
     c                   except    headn
     c                   move      *off          *inoa
     c                   endif
     c                   except    detailn

     c                   endif

     c                   endif
     c*
     c* Update additional named insured with new flag settings.
     c*
     c                   if        xxupd = 'Y'

     C     *cymd         move      sys_date      ANDT
     C                   time                    ANTIME
     C                   eval      ANUSER = Q1USER
     C                   eval      ANPGM  = Q1PGM

     C   66              update    pan01

     C                   endif
     c*
     c* Read next named insured.
     c*
     c     anky          reade     pan01                                  50
     c                   enddo

     c*
     c* Read next employer.
     c*
     C                   read      pem01                                  60

     C                   enddo
     C*
     C* End program.
     C*
     C                   eval      *inlr = *on
    C*
     OQSYSPRT   E            head           2 03
     O                                           72 'Personal Name Report'
     O          E            head           2
     O                                              'Grp'
     O                                         +001 ' Employer'
     O                                         +001 '  Div'
     O                                         +001 ' Loc#'
     O                                         +001 ' File'
     O                                         +001 'Name'
     O          E            detail         1
     O                       XXFND         z
     O                       XXEMP#        z   +001
     O                       XXDIV         z   +001
     O                       XXLOC         z   +001
     O                       XXFILE            +001
     O                       SVNAME            +001
    C*
     OQSYSPRT1  E            headn          2 03
     O                                           72 'Commercial Name Report'
     O          E            headn          2
     O                                              'Grp'
     O                                         +001 ' Employer'
     O                                         +001 '  Div'
     O                                         +001 ' Loc#'
     O                                         +001 ' File'
     O                                         +001 'Name'
     O          E            detailn        1
     O                       XXFND         z
     O                       XXEMP#        z   +001
     O                       XXDIV         z   +001
     O                       XXLOC         z   +001
     O                       XXFILE            +001
     O                       SVNAME            +001
