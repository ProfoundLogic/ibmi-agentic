     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *===============================================================*
    H/TITLE SR37310 - PA - Exclusion of Executive Officers Endorsement
     *===============================================================*
     * WC370310.B - Pennsylvania Exclusion of Executive Officers     *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 02/15/01                                          *
     * Programmer: C:90                                              *
     * Project   : CM027                                             *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * 09/29/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.
      *               -No comments.
     *                                                               *
     * 08/17/15 C:295 Project :WC2332 Priss# 35799                   *
     *                To handle the existing records with different  *
     *                data type  as per previous WCPOLS BI Record    *
     *                                                               *
     * 05/25/15 C:295 Project :WC2332                                *
     *                Delete all references to XXS#1 and XXS#2       *
     *                Changes for to WCPOLS BI Record                *
     *                                                               *
     * 05/01/09 C:61  SUPPORT WA114 68275                            *
     *                Allow for more than 10 Excluded Owner/Officers.*
     *                                                               *
     *  09/25/08 C:65  PROJECT: Supp#65318                           *
     *                 Only attach officers that are effective prior *
     *                 to expiration date of policy year.            *
     *                                                               *
     * 06/04/07 C:189 PROJECT: 099/SUPP#58380                        *
     *                Endorsement print/attach programs should not   *
      *                write/update WXPEP records if single           *
     *                endorsement print request                      *
     *                                                               *
     * 06/11/07 C:184 PROJECT:WA089/58392                            *
     *                 Allied policy endorsement problems            *
     *                                                               *
     *  05/23/05 C:65  PROJECT: 114WAK                               *
     *                 Add processing to write endorsement data to   *
     *                 the WXPEP reporting file.                     *
     *                                                               *
     * 07/13/04  C:61 PROJECT: CM027 WA114/43137                     *
     *                Read thru all employer detail records when     *
     *                collecting other info.                         *
     *                                                               *
     * 08/24/01  C:90 PROJECT: CM027 WA107/28711                     *
     *                Read thru all divisions within the OTHINF      *
     *                subroutine.                                    *
     *                                                               *
     * ------------------------------------------------------------- *
     *  NOTE: Please add comments to the top of revisions.           *
     * ------------------------------------------------------------- *
     *===============================================================*
     * Notes:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
     *                                                               *
     *  QQMODE = 1 - Called to determine if form should be attached. *
     *  QQMODE = 2 - Called to print endorsement.                    *
     *  QQMODE = 3 - Called for additional input.                    *
     *  QQMODE = 4 - Called for mid-term processing.                 *
     *===============================================================*
     ** Copybook F specs
     F/COPY SCOPYEN01F
     Fwmool     if   e           k disk
     Fwxpel     uf a e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     D ix              s              2  0
     D x               s              2  0
     D xxnm            s             40
     D xxunid          s                   like(peunid)
     D x1end#          s                   like(peend#)

     D xxPgNm          s             10
     D Count           s              3  0
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
      *
     D                 ds
     D  ss                     1    100
     D                                     dim(10)
     D  FMC101                 1     10
     D  FMC102                11     20
     D  FMC103                21     30
     D  FMC104                31     40
     D  FMC105                41     50
     D  FMC106                51     60
     D  FMC107                61     70
     D  FMC108                71     80
     D  FMC109                81     90
     D  FMC110                91    100
      *
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
      *
     D                 ds
     D  pct                    1     50  2
     D                                     dim(10)
     D  FMA521                 1      5  2
     D  FMA522                 6     10  2
     D  FMA523                11     15  2
     D  FMA524                16     20  2
     D  FMA525                21     25  2
     D  FMA526                26     30  2
     D  FMA527                31     35  2
     D  FMA528                36     40  2
     D  FMA529                41     45  2
     D  FMA52A                46     50  2
     ** Copybook D specs
     D/COPY SCOPYEN01D
     D/COPY WWCPOLDAT4
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
     ** Form ID
     D qqForm          s             15a   inz('WC370310B ')
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')

      *entry parameters
     D SR37310         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR37310         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
    C*
     C     ooky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C*                  kfld                    xxdiv
     C                   kfld                    eldiv
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
    C*
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs
     C/COPY SCOPYEN01C
     *
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
      /Free
          WriteInCopybook = *Off;
      /End-Free
     *
     C                   eval      ix = *zero
     C                   z-add     0             i
     c                   clear                   E37310
     c                   z-add     0             xxpo1
     c                   z-add     0             xxPo3
     c                   z-add     0             xxpo2
     c                   move      *blank        xxfrst            1
     C                   clear                   nm
     C                   clear                   ss
     C                   clear                   off
     C                   clear                   pct
     *
     C     elky2         chain     pel02                              60
    
     C                   dow       *in60 = *off
     C     rsky          chain     prs01                              60
     *
     C                   if        *in60 = *off
     C                             and rsjrst = enst
      *
     C     ooky          setll     poo01
     C     ooky          reade     poo01                                  60
      *
     C                   dow       *in60 = *off
      *
     C                   if        (OOEXFL = 'E') and
     C                             ((OOEXDT = *zeros) or (ooexdt > elpfdt))
     C                             and (OOTITL <> *blanks)
     C                             and (OOEFDT < elptdt)

     C     OOOFSN        cat(p)    OOOLSN:1      xxnm
     C                   eval      x = 1
     C     xxnm          lookup    nm(x)                                  65
     C                   if        *in65 = *off
     C                   eval      ix = ix + 1
     C                   movel     xxnm          nm(ix)
     C                   movel     OOOSS#        ss(ix)
     C                   movel     OOTITL        off(ix)
     C                   movel     OOOFPC        pct(ix)
     c                   eval      i = i + 1
     c*
     c                   if        i = 1
     c                   movel     xxnm          x4nm1
     c                   movel     ootitl        xxof1
     C                   z-add     OOOFPC        xxpo1
     c                   endif
     c*
     c                   if        i = 2
     c                   movel     xxnm          x4nm2
     c                   movel     ootitl        xxof2
     C                   z-add     OOOFPC        xxpo2
     c                   endif
     c
     c                   if        i = 3
     c                   movel     xxNm          x4Nm3
     c                   movel     OOtitl        xxOf3
     C                   z-add     OOofpc        xxPo3
     c                   endif
     c
     c                   if        i = 3
      *  If printing a policy page.  (Not called from W1054R)

     c                   call      'SRCHKPGM'
     c                   parm      'W1054R   '   #1pgm            10
     c                   parm      *BLANK        #1flag            1

     C                   if        #1flag = '0'
      *
      * Update the endorsement reporting file with the proper data.
      *
     C                   MOVE      'P'           X1TYPE            1
     C                   Z-ADD     0             xxunid
     C*
     c                   move      *hival        xxdt
     C     pekey         setgt     ppe02
     C     pekey1        readpe    ppe02                                  60
     C                   If        *in60 = *off
     C                   z-add     peunid        xxunid
     C                   endif
     C                   movel     xxform        x1end#
     c     pekey2        chain     ppe01                              60
      *
      * Copy data into the record.
      *
     C                   If        *in60 = *off
      *
     c                   move      *blanks       pedata
     c                   movel     E37310        pedata
     c                   movel     q1user        peuser
     c                   movel     q1pgm         pepgm
     c                   z-add     dstime        petime
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         pedt
      *
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
     C                   write     ppe01                                        i = 1
     C                   endif
     C                   z-add     0             i
     c                   clear                   E37310
     c                   z-add     0             xxpo1
     c                   z-add     0             xxpo2
     c                   z-add     0             xxPo3
      *
     C                   endif                                                  *in60 ifeq off
      *
     C                   endif
     C                   endif                                                  *in60 ifeq off
      *

     C                   endif

     C                   endif

     c                   if        ix = 10
     c                   If        wcpolsUpdate <> '1'
      /Free
          //* Write the output record
          Exsr WriteWorkFile;
      /End-Free
     c                   Endif
     c                   clear                   nm
     c                   clear                   ss
     c                   clear                   off
     c                   clear                   pct
     c                   eval      ix = *zero
     c                   endif
      *
     C     ooky          reade     poo01                                  60
     C                   enddo

     C                   endif
     C     elky2         reade     pel02                                  60
     C                   enddo
     *
     ** Write the output record.
     C                   if        ix <> *zero
     c                   If        wcpolsUpdate = '0'
      /Free
          //* Write the output record
          Exsr WriteWorkFile;
      /End-Free
     C                   endif
     C                   endif
     c                   if        i <> 0
      *  If printing a policy page.  (Not called from W1054R)

     c                   call      'SRCHKPGM'
     c                   parm      'W1054R   '   #1pgm            10
     c                   parm      *BLANK        #1flag            1

     C                   if        #1flag = '0'
      *
      * Update the endorsement reporting file with the proper data.
      *
     C                   MOVE      'P'           X1TYPE            1
     C                   Z-ADD     0             xxunid
     C*
     c                   move      *hival        xxdt
     C     pekey         setgt     ppe02
     C     pekey1        readpe    ppe02                                  60
     C                   If        *in60 = *off
     C                   z-add     peunid        xxunid
     C                   endif
     C                   movel     xxform        x1end#
     c     pekey2        chain     ppe01                              60
      *
      * Copy data into the record.
      *
     C                   If        *in60 = *off
      *
     c                   move      *blanks       pedata
     c                   movel     E37310        pedata
     c                   movel     q1user        peuser
     c                   movel     q1pgm         pepgm
     c                   z-add     dstime        petime
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         pedt
      *
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
     C                   write     ppe01                                        i = 1
     C                   endif
      *
     C                   endif                                                  *in60 ifeq off
     C                   endif
      *
     C                   endif                                                  *in60 ifeq off
      *
     C                   endsr
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     *
     C                   clear                   wcpolsUpdate      1
      *
     C                   call      'SRCHKPGM'
     C                   parm      'W7417R'      xxPgNm
     C                   parm      *Blanks       #17flag           1
      *
     C                   call      'SRCHKPGM'
     C                   parm      'W7413R'      xxPgNm
     C                   parm      *Blanks       #13flag           1
      *
     C                   If        qqmode    = '2'
     C                             and #17flag   = '1'
     C                             and #13flag  = '0'
     C                   Eval      wcpolsUpdate = '1'
     C                   Endif
      *
      *  Update the endorsement reporting file with blank endorsement data
      *  where we have data with previous WCIO standards.
      *
     C                   If        wcpolsUpdate = '1'
     C                   move      *hival        xxdt
     C     pekey         setgt     ppe02
     C     pekey1        readpe    ppe02                                  60
     C                   If        *in60 = *off
     C                   z-add     peunid        xxunid
     C                   endif
     C                   movel     xxform        x1end#
     C                   Exsr      DelPEP
     C                   Endif                                                --wcpolsUpdate = '1'
     *
     C                   endsr
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     *
     *
     C                   endsr
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
      *
     C     ooky          setll     poo01
     C     ooky          reade     poo01                                  60
      *
     C                   dow       *in60 = *off
      *
     C                   if        (OOEXFL = 'E') and
     C                             ((OOEXDT = *zeros) or (ooexdt > elpfdt))
     C                             and (OOTITL <> *blanks)
     C                             and (OOEFDT < elptdt)
     C                   move      *on           crtfrm
     C                   leave
     C                   endif
      *
     C     ooky          reade     poo01                                  60
     C                   enddo
     *
     C                   endsr
     *=========================================================================
     *  DELPEP - Delete/update the old record processed
     *=========================================================================
     C     Delpep        Begsr
     *
     *
     C                   Eval      Count = 0
     C     pekey2        Setll     ppe01
     C     pekey2        Reade     ppe01                                  60
      *
     C                   Dow       *in60 = *off
     C                   Eval      Count = Count +1
     C                   If        Count  < 2
Eval C                   Eval      Pedata = *blanks
     C                   Update    ppe01
     C                   Else
      *
     C                   Delete    ppe01
     C                   Endif
     C     pekey2        Reade     ppe01                                  60
     C                   Enddo
     C                   Endsr
     *=========================================================================
