     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR04303 - CA OFFICERS AND DIRECTORS COVERAGE/EXCLUSION
      //===============================================================*
      // WC040303 - California Officers and Directors Coverage/Exclus. *
      //                                                               *
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      //===============================================================*
      // REVISIONS:                                                    *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*
      // Notes:                                                        *
      //                                                               *
      //  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      //                                                               *
      //  QQMODE = 1 - Called to determine if form should be attached. *
      //  QQMODE = 2 - Called to print endorsement.                    *
      //  QQMODE = 3 - Called for additional input.                    *
      //  QQMODE = 4 - Called for mid-term processing.                 *
      //===============================================================*

      /COPY SCOPYEN01F

     Fwmool     if   e           k disk
     Fwxpel     uf a e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)

     D ix              s              2  0
     D x               s              2  0
     D xxnm            s             40
     D xxunid          s                   like(peunid)
     D x1end#          s                   like(peend#)

     D                 ds
     D  nm                     1    400
     D                                     dim(10)
     D  FMC401                 1     40
     D  FMC402                41     80
     D  FMC403                81    120
     D  FMC404               121    160
     D  FMC405               161    200
     D  FMC406               201    240
     D  FMC407               241    280
     D  FMC408               281    320
     D  FMC409               321    360
     D  FMC410               361    400
     D                 ds
     D  off                    1    200
     D                                     dim(10)
     D  FMC201                 1     20
     D  FMC202                21     40
     D  FMC203                41     60
     D  FMC204                61     80
     D  FMC205                81    100
     D  FMC206               101    120
     D  FMC207               121    140
     D  FMC208               141    160
     D  FMC209               161    180
     D  FMC210               181    200

      /COPY SCOPYEN01D

      /COPY WWCPOLDAT4

     D qqForm          s             15A   Inz('WC040303  ')
     D addpgm          c                   const('SRXXXXXXXX')
     d qqMod2          s              1
     d qqRRN           s              9  0

     D SR04303         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                1

     D SR04303         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qq303                         1

     C     ooky          klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
     C                   kfld                    eldiv


     C     emky1         KList
     C                   KFld                    elco#
     C                   KFld                    elfnd
     C                   KFld                    elemp#
     C                   KFld                    eldiv

     C     pekey         KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxdt              7 0
     C     pekey1        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C     pekey2        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxunid
     C                   KFld                    x1end#

      /COPY SCOPYEN01C

      *=========================================================================
      *  OTHINF - Subroutine to load form specific information & write record
      *=========================================================================
     C     othinf        begsr

      /Free
          WriteInCopybook = *Off;
      /End-Free

     C                   eval      ix = *zero
     C                   z-add     0             i
     c                   clear                   E04303
     c                   move      *blank        xxfrst            1
     C                   clear                   nm
     C                   clear                   off
     C     elky2         chain     pel02                              60

     C                   dow       *in60 = *off

     C     emky1         Chain     pem01

     c                   if        %found and (
     c                             (emtpob = '3')
     c                             or (emtpob = '4')
     c                             or ((cltid = '136') and (emtpob = 'A'))
     c                             )
     C     rsky          chain     prs01                              60

     C                   if        *in60 = *off
     C                             and rsjrst = enst
     C     ooky          setll     poo01
     C     ooky          reade     poo01                                  60

     C                   dow       *in60 = *off

     C                   if        OOEXFL = 'E'
     C                             and ((OOEXDT = 0) or (ooexdt > elpfdt))
     C                             and OOSTAT <> 'I'
     C     OOOFSN        cat(p)    OOOLSN:1      xxnm
     C                   eval      x = 1
     C     xxnm          lookup    nm(x)                                  65

     C                   if        *in65 = *off
     C                   eval      ix = ix + 1
     C                   movel     xxnm          nm(ix)
     C                   movel     OOTITL        off(ix)
     c                   eval      i = i + 1
     c                   if        i = 1
     c
     c                   if        ootitl <> *blanks
     c     ootitl        cat       xxnm:1        x3nm1
     c                   else
     c                   movel     xxnm          x3nm1
     c                   endif
     c
     c                   endif
     c                   if        i = 2
     c
     c                   if        ootitl <> *blanks
     c     ootitl        cat       xxnm:1        x3nm2
     c                   else
     c                   movel     xxnm          x3nm2
     c                   endif
     c
     c                   endif
     c                   if        i = 3
     c
     c                   if        ootitl <> *blanks
     c     ootitl        cat       xxnm:1        x3nm3
     c                   else
     c                   movel     xxnm          x3nm3
     c                   endif
     c
     c                   endif
     c                   if        i = 3

     c                   call      'SRCHKPGM'
     c                   parm      'W1054R   '   #1pgm            10
     c                   parm      *BLANK        #1flag            1

     C                   if        #1flag = '0'
      * Update the endorsement reporting file with the proper data.
     C                   MOVE      'P'           X1TYPE            1
     C                   Z-ADD     0             xxunid
     c                   move      *hival        xxdt
     C     pekey         setgt     ppe02
     C     pekey1        readpe    ppe02                                  60
     C                   If        *in60 = *off
     C                   z-add     peunid        xxunid
     C                   endif
     C                   movel     xxform        x1end#
     c     pekey2        chain     ppe01                              60
      * Copy data into the record.
     C                   If        *in60 = *off
     c                   move      *blanks       pedata
     c                   movel     E04303        pedata
     c                   movel     q1user        peuser
     c                   movel     q1pgm         pepgm
     c                   z-add     dstime        petime
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         pedt
     C                   if        xxfrst = *blank
     C                   update    ppe01
     C                   move      'y'           xxfrst
     c                   else
     c                   movel     q1user        peiusr
     c                   movel     q1pgm         peipgm
     c                   z-add     dstime        peitim
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         peidt
     C                   write     ppe01
     C                   endif
     C                   z-add     0             i
     c                   clear                   E04303
     C                   endif
     C                   endif
     C                   endif
     C                   endif

     C                   if        ix = 10
      /Free
          Exsr WriteWorkFile;
      /End-Free
     c                   eval      ix = 0
     C                   clear                   nm
     C                   clear                   off
     C                   endif

     C                   endif

     C     ooky          reade     poo01                                  60
     C                   enddo

     C                   endif

     C                   endif

     C     elky2         reade     pel02                                  60
     C                   enddo
     C                   if        ix <> *zero
      /Free
          Exsr WriteWorkFile;
      /End-Free
     C                   endif
     c                   if        i <> 0

     c                   call      'SRCHKPGM'
     c                   parm      'W1054R   '   #1pgm            10
     c                   parm      *BLANK        #1flag            1

     C                   if        #1flag = '0'

      * Update the endorsement reporting file with the proper data.
     C                   MOVE      'P'           X1TYPE            1
     C                   Z-ADD     0             xxunid
     c                   move      *hival        xxdt
     C     pekey         setgt     ppe02
     C     pekey1        readpe    ppe02                                  60
     C                   If        *in60 = *off
     C                   z-add     peunid        xxunid
     C                   endif
     C                   movel     xxform        x1end#
     c     pekey2        chain     ppe01                              60
      * Copy data into the record.
     C                   If        *in60 = *off
     c                   move      *blanks       pedata
     c                   movel     E04303        pedata
     c                   movel     q1user        peuser
     c                   movel     q1pgm         pepgm
     c                   z-add     dstime        petime
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         pedt
     C                   if        xxfrst = *blank
     C                   update    ppe01
     C                   move      'y'           xxfrst
     c                   else
     c                   movel     q1user        peiusr
     c                   movel     q1pgm         peipgm
     c                   z-add     dstime        peitim
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         peidt
     C                   write     ppe01
     C                   endif
     C                   endif
     C                   endif
     C                   endif

     C                   endsr
      *========================================================================
      *  PGMSTR - Beginning of program processing
      *========================================================================
     C     pgmstr        begsr
     C                   endsr
      *========================================================================
      *  PGMEND - End of program processing
      *========================================================================
     C     pgmend        begsr
     C                   endsr
      *========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *========================================================================
     C     valid         begsr
     c                   if        q1prms >= 7
     C                   move      *off          qq303
     c                   endif

     C     emky1         Chain     pem01

     c                   if        %found and (
     c                             (emtpob = '3')
     c                             or (emtpob = '4')
     c                             or ((cltid = '136') and (emtpob = 'A'))
     c                             )
     C     rsky          chain     prs01                              60
     C                   if        *in60 = *off
     C                             and rsjrst = enst
     C     ooky          setll     poo01
     C     ooky          reade     poo01                                  60
     C                   dow       *in60 = *off
     C                   if        OOEXFL = 'E'
     C                             and ((OOEXDT = 0) or (ooexdt > elpfdt))
     C                             and OOSTAT <> 'I'
      ** Found a division with a matching jurisdiction state
     c                   if        q1prms >= 7
     c                   move      *on           qq303
     c                   endif

     C                   move      *on           crtfrm

     C                   leave
     C                   endif
     C     ooky          reade     poo01                                  60
     C                   enddo
     C                   endif

     C                   endif

     C                   endsr
