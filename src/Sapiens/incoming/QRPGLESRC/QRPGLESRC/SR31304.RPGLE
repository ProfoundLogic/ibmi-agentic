     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *
    H/TITLE SR31304 - NY - Executive Officers Exclusion Endorsement
     *===============================================================*
     * WC310304 - New York Executive Officers Exclusion Endorsement  *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 11/12/01                                          *
     * Programmer: C:90                                              *
     * Project   : CM027                                             *
     *===============================================================*
      * Revised:
      *
      * 07/14/11 C:061 Support 77336
      *              - Convert source to RPGIV.
      *              - Do not attach for flat cancelled divisions.
      *
      * 05/17/10  C:190 Support 72153
      *                 This form should be pulling information that  *
      *                 is setup at the 'NY' division, not the State  *
      *                 Reporting division.                           *
      *
      * 12/15/04  C:61  Support wa129 44726
      *                 Do not attach if Sole Proprietor or Partner.
      *                 (Attach WC310316 instead)
      *
      * 01/06/03  C:61  Support OP129 40678
      *                 Allow for printing more than 10 Owner Officers.
      *
      *
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
     *
     ** Copybook F specs
     F/COPY SCOPYEN01F
     Fwmool     if   e           k disk
    
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     D ix              s              2  0
     D x               s              2  0
     D xxnm            s             40

     D                 ds
     D  nm                     1    640
     D                                     dim(16)
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
     D  FMC411               401    440
     D  FMC412               441    480
     D  FMC413               481    520
     D  FMC414               521    560
     D  FMC415               561    600
     D  FMC416               601    640
      *
      *
     D                 ds
     D  off                    1    320
     D                                     dim(16)
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
     D  FMC211               201    220
     D  FMC212               221    240
     D  FMC213               241    260
     D  FMC214               261    280
     D  FMC215               281    300
     D  FMC216               301    320
      *
     ** Copybook D specs
      /COPY SCOPYEN01D
      *
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
     ** Form ID
     D qqForm          s             15a   inz('WC310304  ')
      *
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
      *
      *entry parameters
     D SR31304         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR31304         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
    C*
     C     ooky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    eldiv
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
     *
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
     C                   if        OOEXFL = 'E'
     C                             and OOTITL <> *blanks

     C     OOOFSN        cat(p)    OOOLSN:1      xxnm
     C                   eval      x = 1
     C     xxnm          lookup    nm(x)                                  65
     C                   if        *in65 = *off
     C                   eval      ix = ix + 1
     C                   movel     xxnm          nm(ix)
     C                   movel     OOTITL        off(ix)

     C                   endif                                                  *in65

     C                   endif

     C                   if        ix = 16
      /Free
          //* Write the output record
          Exsr WriteWorkFile;
      /End-Free
     C                   clear                   ix
     C                   clear                   nm
     C                   clear                   off
     C                   endif                                                  *in65

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
     *  $GMSTR - Beginning of program processing
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
      * If type of business is Sole Prop or Partner, Then process
      *
     C                   if        (EMTPOB <> '1') and (EMTPOB <> '2')
     C     rsky          chain     prs01                              60
     *
     C                   if        *in60 = *off
     C                             and rsjrst = enst
     *
     ** Found a division with a matching jurisdiction state
     *
     C     ooky          setll     poo01
     C     ooky          reade     poo01                                  60
      *
     C                   dow       *in60 = *off
      *
     C                   if        OOEXFL = 'E'
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
     C                   endif
     *
     C                   endif                                                  End EMTPOB <> 1,2
     *
     C                   endsr
