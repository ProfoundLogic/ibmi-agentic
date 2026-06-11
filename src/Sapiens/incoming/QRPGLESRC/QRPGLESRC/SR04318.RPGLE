     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/TITLE SR04318 - CA - LLC Company Coverage Exclusion Endorsment
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
      * =======================================================================*
      *WC040318- Limited Liability Company Coverage / Exclusion Endorsement  *
      *                                                                        *
      * Program Description:                                                   *
      *                                                                        *
      *   Determines if this endorsement form should be attached to            *
      *   the policy and creates data in the Endorsement Form                  *
      *   Workfile (WXFMP) prior to a form merge.                              *
      *                                                                        *
      * Created   : 09/10/13                                                   *
      * Programmer: C:287                                                      *
      * Project   :WC2219                                                    *
      * =======================================================================*
      *Revisions:                                                            *
      *                                                                        *
     * 09/15/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.
      *               -No comments.
      *                                                                        *
      * ========================================================================
      *Notes:                                                                *
      *                                                                        *
      *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).                   *
      *                                                                        *
      *  QQMODE = 1 - Called to determine if form should be attached.          *
      *  QQMODE = 2 - Called to print endorsement.                             *
      *  QQMODE = 3 - Called for additional input.                             *
      *  QQMODE = 4 - Called for mid-term processing.                          *
      * ========================================================================
    
      **Copybook F specs
     F/COPY SCOPYEN01F
    
      *Owner Officer
     Fwmool     if   e           k disk
     Fwxpel     uf a e           k disk
     Fwxpel1    if   e           k disk
     F                                     Rename(ppe01:ppe02)
    
      * ========================================================================
      * Definition Specs
      * ========================================================================

     D ix              s              2  0
     D x               s              2  0
     D xxnm            s             40
     D xxunid          s                   Like(peunid)
     D x1end#          s                   Like(peend#)

     D  NMDS           ds
     D  FMC401
     D  FMC402
     D  FMC403
     D  FMC404
     D  FMC405
     D  FMC406
     D  FMC407
     D  FMC408
     D  FMC409
     D  FMC410
     D  NM                                 Overlay(NMDS)
     D                                     Like(FMC401) Dim(10)

     D  TLDS           ds
     D  FMC301
     D  FMC302
     D  FMC303
     D  FMC304
     D  FMC305
     D  FMC306
     D  FMC307
     D  FMC308
     D  FMC309
     D  FMC310
     D  TL                                 Overlay(TLDS)
     D                                     Like(FMC301) Dim(10)


      **Copybook D specs
     D/COPY SCOPYEN01D
     D/COPY WWCPOLDAT4
    
    
      *=========================================================================
      * /COPY required constants
      *=========================================================================
    
      **Form ID
     D qqForm          s             15a   inz('WC040318  ')
    
      **Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')

     *entry parameters
     D SR04318         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                1
     D SR04318         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qq302a                        1

     d* needed for copybook
     d qqMod2          s              1
     d qqRRN           s              9  0
    
    
      *=========================================================================
      * KLIST definitions
      *=========================================================================

      **Owner Officer

     C     ooky          Klist
     C                   Kfld                    elco#
     C                   Kfld                    elfnd
     C                   Kfld                    elemp#
     C                   Kfld                    eldiv

      **Employer Master file

     C     emky1         Klist
     C                   Kfld                    elco#
     C                   Kfld                    elfnd
     C                   Kfld                    elemp#
     C                   Kfld                    eldiv
    
     C     pekey         Klist
     C                   Kfld                    xxco#
     C                   Kfld                    xxfnd
     C                   Kfld                    xxemp#
     C                   Kfld                    xxfyr
     C                   Kfld                    xxdt              7 0

     C     pekey1        Klist
     C                   Kfld                    xxco#
     C                   Kfld                    xxfnd
     C                   Kfld                    xxemp#
     C                   Kfld                    xxfyr

     C     pekey2        Klist
     C                   Kfld                    xxco#
     C                   Kfld                    xxfnd
     C                   Kfld                    xxemp#
     C                   Kfld                    xxfyr
     C                   Kfld                    xxunid
     C                   Kfld                    x1end#
    
      *=========================================================================
      * Main
      *=========================================================================
    
      **Copybook C specs
     C/COPY SCOPYEN01C
    
      *=========================================================================
      * OTHINF -Subroutine to load form specific information & write record
      *=========================================================================
     C     othinf        Begsr
    
     C                   Eval      ix = *zero
     C                   Z-add     0             i2                7 0
     C                   Clear                   E04302
     C                   Move      *Blank        xxfrst            1
     C                   Clear                   nm
     C                   Clear                   tl
     C     elky2         Chain     pel02                              60
    
     C                   Dow       *In60 = *off

     C     emky1         Chain     pem01

     C                   If        %Found(wmeml)
     C                             and (emtpob = 'A')
     C     rsky          Chain     prs01                              60
    
     C                   If        *In60 = *off
     C                             and rsjrst = enst
     C     ooky          Setll     poo01
     C     ooky          Reade     poo01                                  60

     C                   Dow       *In60 = *off

     C                   If        OOEXFL = 'E'
     C                             and ((OOEXDT = 0) or (ooexdt > elpfdt))
     C                             and OOSTAT <> 'I'
     C     OOOFSN        Cat(p)    OOOLSN:1      xxnm
     C                   Eval      x = 1
     C     xxnm          Lookup    nm(x)                                  65

     C                   If        *In65 = *off
     C                   Eval      ix = ix + 1
     C                   Movel     xxnm          nm(ix)
     C                   Movel     ootitl        tl(ix)
     C                   Eval      i2 = i2 + 1

     C                   If        i2 = 1

     C                   If        ootitl <> *blanks
     C     ootitl        Cat       xxnm:1        x2nm1
     C                   Else
     C                   Movel     xxnm          x2nm1
     C                   Endif

     C                   Endif

     C                   If        i2 = 2

     C                   If        ootitl <> *blanks
     C     ootitl        Cat       xxnm:1        x2nm2
     C                   Else
     C                   Movel     xxnm          x2nm2
     C                   Endif

     C                   Endif
     C                   If        i2 = 3

     C                   If        ootitl <> *blanks
     C     ootitl        Cat       xxnm:1        x2nm3
     C                   Else
     C                   Movel     xxnm          x2nm3
     C                   Endif

     C                   Endif
     C                   If        i2 = 3
      **If printing a policy page.  (Not called from W1054R)

     C                   Call      'SRCHKPGM'
     C                   Parm      'W1054R   '   #1pgm            10
     C                   Parm      *BLANK        #1flag            1

     C                   If        #1flag = '0'

      **Update the endorsement reporting file with the proper data.

     C                   Move      'P'           X1TYPE            1
     C                   Z-add     0             xxunid

     C                   Move      *Hival        xxdt
     C     pekey         Setgt     ppe02
     C     pekey1        Readpe    ppe02                                  60
     C                   If        *In60 = *off
     C                   Z-add     peunid        xxunid
     C                   Endif
     C                   Movel     xxform        x1end#
     C     pekey2        Chain     ppe01                              60

      **Copy data into the record.

     C                   If        *In60 = *off

     C                   Move      *Blanks       pedata
     C                   Movel     E04302        pedata
     C                   Movel     q1user        peuser
     C                   Movel     q1pgm         pepgm
     C                   Z-add     dstime        petime
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         pedt

     C                   If        xxfrst = *blank
     C                   Update    ppe01
     C                   Move      'y'           xxfrst
     C                   Else
     C                   Movel     q1user        peiusr
     C                   Movel     q1pgm         peipgm
     C                   Z-add     dstime        peitim
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         peidt
     C                   Write     ppe01
     C                   Endif
     C                   Z-add     0             i2
     C                   Clear                   E04302

     C                   Endif                                                  *in60 ifeq off
     C                   Endif

     C                   Endif                                                  *in60 ifeq off

     C                   Endif                                                  *in65

     C                   If        ix = 10
      /Free
       //* Write the output record
       WriteInCopybook = *Off;
       Exsr WriteWorkFile;
      /End-Free
     C                   Eval      ix = 0
     C                   Clear                   nm
     C                   Clear                   tl
     C                   Endif

     C                   Endif

     C     ooky          Reade     poo01                                  60
     C                   Enddo

     C                   Endif                                                  RSJRST = ENST

     C                   Endif                                                  RSJRST = ENST

     C     elky2         Reade     pel02                                  60
     C                   Enddo
    
     C                   If        ix <> *zero
      /Free
       //* Write the output record
       WriteInCopybook = *Off;
       Exsr WriteWorkFile;
      /End-Free
     C                   Endif
     C                   If        i2 <> 0
      **If printing a poLicy page.  (Not called from W1054R)

     C                   Call      'SRCHKPGM'
     C                   Parm      'W1054R   '   #1pgm            10
     C                   Parm      *Blank        #1flag            1

     C                   If        #1flag = '0'

      **Update the endorsement reporting file with the proper data.

     C                   Move      'P'           X1TYPE            1
     C                   Z-add     0             xxunid
     C                   Move      *Hival        xxdt
     C     pekey         Setgt     ppe02
     C     pekey1        Readpe    ppe02                                  60
     C                   If        *In60 = *off
     C                   Z-add     peunid        xxunid
     C                   Endif
     C                   Movel     xxform        x1end#
     C     pekey2        Chain     ppe01                              60

      **Copy data into the record.

     C                   If        *In60 = *off

     C                   Move      *blanks       pedata
     C                   Movel     E04302        pedata
     C                   Movel     q1user        peuser
     C                   Movel     q1pgm         pepgm
     C                   Z-add     dstime        petime
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         pedt

     C                   If        xxfrst = *blank
     C                   Update    ppe01
     C                   Move      'y'           xxfrst
     C                   Else
     C                   Movel     q1user        peiusr
     C                   Movel     q1pgm         peipgm
     C                   Z-add     dstime        peitim
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         peidt
     C                   Write     ppe01
     C                   Endif

     C                   Endif                                                  *in60 ifeq off

     C                   Endif
     C                   Endif                                                  *in60 ifeq off
    
     C                   Endsr
    
    
      *=========================================================================
      * PGMSTR -Beginning of program processing
      *=========================================================================
     C     pgmstr        Begsr

     C                   Endsr
    
    
      *=========================================================================
      * PGMEND -End of program processing
      *=========================================================================
     C     pgmend        Begsr

     C                   Endsr
    
    
      *=========================================================================
      * VALID -Subroutine to attach this form to the policy
      *=========================================================================
     C     valid         Begsr
     C                   If        q1prms >= 7
     C                   Move      *Off          qq302a
     C                   Endif

     C     emky1         Chain     pem01
     C                   If        %found(wmeml) and (emtpob = 'A')
    
     C     rsky          Chain     prs01                              60
    
     C                   If        *In60 = *off
     C                             and rsjrst = enst
    
      **Found a division with a matching jurisdiction state
    
     C     ooky          Setll     poo01
     C     ooky          Reade     poo01                                  60
    
     C                   Dow       *In60 = *off
    
     C                   If        OOEXFL = 'E'
     C                             and ((OOEXDT = 0) or (ooexdt > elpfdt))
     C                             and OOSTAT <> 'I'
    
     C                   If        q1prms >= 7
     C                   Move      *On           qq302a
     C                   Endif

     C                   Move      *On           crtfrm

     C                   Leave
     C                   Endif
    
     C     ooky          Reade     poo01                                  60
     C                   Enddo
    
     C                   Endif

     C                   Endif
    
     C                   Endsr
    
