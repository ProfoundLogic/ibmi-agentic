     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *===============================================================*
    H/TITLE SR31306 - NY - Inclusion of Executive Officers Endorsement
     *===============================================================*
     * WC310306.A - New York Inclusion of Executive Officers Endorse *
     *                                                               *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 05/09/01                                          *
     * Programmer: C:90                                              *
     * Project   : CM027                                             *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * 08/01/02  C:09 PROJECT: CM027 WA109/33687                     *
     *                Generate one forms record (WXFMP) for each     *
     *                set of 10 included executive officers read.    *
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
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     D ix              s              2  0
     D x               s              2  0
     D xxnm            s             40

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
     ** Copybook D specs
     D/COPY SCOPYEN01D
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
     ** Form ID
     D qqForm          s             15a   inz('WC310306A ')
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')

      *entry parameters
     D SR31306         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR31306         PI
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
     C                   kfld                    xxdiv
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
     C*
      /Free
          WriteInCopybook = *Off;
      /End-Free
     C                   eval      ix = *zero
     C                   clear                   nm
     C                   clear                   off
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
     C                   if        OOEXFL = 'I'
     C                             and OOTITL <> *blanks

     C     OOOFSN        cat(p)    OOOLSN:1      xxnm
     C                   eval      x = 1
     C     xxnm          lookup    nm(x)                                  65
     C                   if        *in65 = *off
     C                   eval      ix = ix + 1
     C                   movel     xxnm          nm(ix)
     C                   movel     OOTITL        off(ix)
     C                   if        ix = 10
      /Free
          //* Write the output record
          Exsr WriteWorkFile;
      /End-Free
     C                   eval      ix = *zero
     C                   clear                   nm
     C                   clear                   off
     C                   endif

     C                   endif                                                  *in65

     C                   endif

     C     ooky          reade     poo01                                  60
     C                   enddo
     ** Write the output record
     *
     C                   endif
     C     elky2         reade     pel02                                  60
     C                   enddo
     *
     ** Write the output record.
     C                   if        ix <> *zero
      /Free
          //* Write the output record
          Exsr WriteWorkFile;
      /End-Free
     C                   endif
     *
     C                   endsr
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     *
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
     *
     ** Found a division with a matching jurisdiction state
     *
     C     ooky          setll     poo01
     C     ooky          reade     poo01                                  60
      *
     C                   dow       *in60 = *off
      *
     C                   if        OOEXFL = 'I'
     C                             and OOTITL <> *blanks
     *
     C                   move      *on           crtfrm
     C                   leave
     *
     C                   endif
     *
     C     ooky          reade     poo01                                  60
     C                   enddo
     *
     C                   endsr
