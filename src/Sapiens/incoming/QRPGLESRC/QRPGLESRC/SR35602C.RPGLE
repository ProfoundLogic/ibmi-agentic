     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR35602C - Oklahoma Benefits Deductible
       //===============================================================*
       // WC350602C                                                     *
       //    Oklahoma Benefits Deductible Endorsement                   *
       //                                                               *
       // Program Description:                                          *
       //                                                               *
       //   Determines if this endorsement form should be attached to   *
       //   the policy and creates data in the Endorsement Form         *
       //   Workfile (WXFMP) prior to a form merge.                     *
       //                                                               *
       //===============================================================*
       // REVISIONS:                                                    *
       // 01/30/17 C:180 Support: 41157                                 *
       //                                                               *
       // 07/17/16 C:302 Project: SY371                                 *
       //                Scriptura phase 2                              *
       //                Clean up Program/Copybook                      *
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

     FWTDDL2    IF   E           K DISK
     FWTRTL1    IF   E           K DISK
     Fwtabl     if   e           k disk    include(pbs01)
     Fwmldl1    if   e           k disk
     D/COPY SCOPYEN01D
     D  RTYPAL         S              1    DIM(10)
     D  RTYPM          S              1  0 INZ
     D  RTYPI          S              1  0 INZ
     D
     D qqForm          s             15A   Inz('WC350602C ')
     D addpgm          c                   const('SRXXXXXXXX')
     D xxampo          s                   like(FMAM01)
     D xxamag          s                   like(FMAM01)
     D xxDedSize       s              1
     D rtvDedSize      pr             1
     D  co#                                like(ELCO#)
     D  bscd                               like(ELBSCD)
     D rtvSmDedInfo    pr
     D  ampo                               like(FMAM01)
     D  amag                               like(FMAM01)
     D rtvLgDedInfo    pr
     D  ampo                               like(FMAM01)
     D  amag                               like(FMAM01)

     D SR35602C        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR35602C        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

     C     DDKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    RSJRST
     C                   KFLD                    ELDCDE
     C                   KFLD                    XXDT              7 0

     C     DDKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    RSJRST
     C                   KFLD                    ELDCDE
      * Reserve type  File Key
     C     RTKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    XXKEY

     C     *LIKE         DEFINE    RTKEY         XXKEY

      /COPY SCOPYEN01C

      *=========================================================================
      * OTHINF- Subroutine to load form specific information & write record
      *=========================================================================
     C     othinf        begsr
      *  Check each division for a matching jurisdiction state
     C     elky2         chain     pel02                              60

     C     *in60         doweq     *OFF
     C                   Eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     C                   Select
     C                   When      xxDedSize = 'L'
     C                   Callp     rtvLgDedInfo (xxAmpo: xxAmag)
     C                   When      xxDedSize = 'S'
     C                   Callp     rtvSmDedInfo (xxAmpo: xxAmag)
     C                   Endsl

     C                   EXSR      RSVCOD
      *Load Medical and Indemnity Deductible Amount
     C                   If        RTYPM >= 1 and RTYPI >=1
     C                   Z-ADD     XXAMPO        FMA701
     C                   ENDIF
      *Load Medical Deductible Amount
     C                   If        RTYPM >= 1 and RTYPI = 0
     C                   Z-ADD     XXAMPO        FMA702
     C                   ENDIF
      *Load Indemnity Deductible Amount
     C                   If        RTYPI >= 1 and RTYPM = 0
     C                   Z-ADD     XXAMPO        FMA703
     C                   ENDIF

     C                   If        xxAmpo <> 0
     C                   Eval      crtfrm = *on
     C                   Leave
     C                   Endif
     C
     C     ELKY2         READE     PEL02                                  60
     C                   ENDDO

     C                   endsr
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     pgmstr        begsr
     C                   endsr
      *=========================================================================
      *  PGMEND - End of program processing
      *=========================================================================
     C     pgmend        begsr
     C                   endsr
      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
     C     valid         begsr

     c                   eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     c                   select
     c                   when      xxDedSize = 'L'
     c                   callp     rtvLgDedInfo (xxAmpo: xxAmag)
     c                   when      xxDedSize = 'S'
     c                   callp     rtvSmDedInfo (xxAmpo: xxAmag)
     c                   endsl
     c                   if        xxAmpo <> 0
     c                   eval      crtfrm = *on
     c                   endif

     C                   endsr
      ********************************************************************
      *  RSVCOD - LOAD NON-BLANK "RESERVE CODES"
      ********************************************************************
     C     RSVCOD        BEGSR
     C*  Check each non-blank "Reserve Codes".
     C
     C                   Z-ADD     0             I
     C                   CLEAR                   RTYPAL
     C                   EVAL      I = I + 1
     C                   IF        DDDC01 <> *BLANKS
     C
     C                   MOVEL     DDDC01        XXKEY
     C     RTKY          CHAIN     PRT01                              80
     C                   IF        *IN80 = *OFF AND RTSTAT = 'A'
     C                   EVAL      RTYPAL(I) = RTTYPE
     C                   ENDIF
     C
     C                   ENDIF
     C
     C                   EVAL      I = I + 1
     C                   IF        DDDC02 <> *BLANKS
     C
     C                   MOVEL     DDDC02        XXKEY
     C     RTKY          CHAIN     PRT01                              80
     C                   IF        *IN80 = *OFF AND RTSTAT = 'A'
     C                   EVAL      RTYPAL(I) = RTTYPE
     C                   ENDIF
     C
     C                   ENDIF
     C
     C                   EVAL      I = I + 1
     C                   IF        DDDC03 <> *BLANKS
     C
     C                   MOVEL     DDDC03        XXKEY
     C     RTKY          CHAIN     PRT01                              80
     C                   IF        *IN80 = *OFF AND RTSTAT = 'A'
     C                   EVAL      RTYPAL(I) = RTTYPE
     C                   ENDIF
     C
     C                   ENDIF
     C
     C                   EVAL      I = I + 1
     C                   IF        DDDC04 <> *BLANKS
     C
     C                   MOVEL     DDDC04        XXKEY
     C     RTKY          CHAIN     PRT01                              80
     C                   IF        *IN80 = *OFF AND RTSTAT = 'A'
     C                   EVAL      RTYPAL(I) = RTTYPE
     C                   ENDIF
     C
     C                   ENDIF
     C
     C                   EVAL      I = I + 1
     C                   IF        DDDC05 <> *BLANKS
     C
     C                   MOVEL     DDDC05        XXKEY
     C     RTKY          CHAIN     PRT01                              80
     C                   IF        *IN80 = *OFF AND RTSTAT = 'A'
     C                   EVAL      RTYPAL(I) = RTTYPE
     C                   ENDIF
     C
     C                   ENDIF
     C
     C                   EVAL      I = I + 1
     C                   IF        DDDC06 <> *BLANKS
     C
     C                   MOVEL     DDDC06        XXKEY
     C     RTKY          CHAIN     PRT01                              80
     C                   IF        *IN80 = *OFF AND RTSTAT = 'A'
     C                   EVAL      RTYPAL(I) = RTTYPE
     C                   ENDIF
     C
     C                   ENDIF
     C
     C                   EVAL      I = I + 1
     C                   IF        DDDC07 <> *BLANKS
     C
     C                   MOVEL     DDDC07        XXKEY
     C     RTKY          CHAIN     PRT01                              80
     C                   IF        *IN80 = *OFF AND RTSTAT = 'A'
     C                   EVAL      RTYPAL(I) = RTTYPE
     C                   ENDIF
     C
     C                   ENDIF
     C
     C                   EVAL      I = I + 1
     C                   IF        DDDC08 <> *BLANKS
     C
     C                   MOVEL     DDDC08        XXKEY
     C     RTKY          CHAIN     PRT01                              80
     C                   IF        *IN80 = *OFF AND RTSTAT = 'A'
     C                   EVAL      RTYPAL(I) = RTTYPE
     C                   ENDIF
     C
     C                   ENDIF
     C
     C                   EVAL      I = I + 1
     C                   IF        DDDC09 <> *BLANKS
     C
     C                   MOVEL     DDDC09        XXKEY
     C     RTKY          CHAIN     PRT01                              80
     C                   IF        *IN80 = *OFF AND RTSTAT = 'A'
     C                   EVAL      RTYPAL(I) = RTTYPE
     C                   ENDIF
     C
     C                   ENDIF
     C
     C                   EVAL      I = I + 1
     C                   IF        DDDC10 <> *BLANKS
     C
     C                   MOVEL     DDDC10        XXKEY
     C     RTKY          CHAIN     PRT01                              80
     C                   IF        *IN80 = *OFF AND RTSTAT = 'A'
     C                   EVAL      RTYPAL(I) = RTTYPE
     C                   ENDIF
     C
     C                   ENDIF
     C*  Load and Count Reserve Type.
     C                   Z-ADD     0             I
     C                   DOW       I < 10
     C
     C                   EVAL      I = I+1
     C
     C                   IF        RTYPAL(I) = 'M'
     C                   EVAL      RTYPM = RTYPM +1
     C                   ENDIF
     C
     C                   IF        RTYPAL(I) = 'I'
     C                   EVAL      RTYPI = RTYPI +1
     C                   ENDIF
     C
     C                   ENDDO
     C
     C                   ENDSR
      *=========================================================================
      * rtvDedSize - Query whether policy is small or large deductible
      *=========================================================================
     p rtvDedSize      b

     d rtvDedSize      pi             1
     d  co#                                like(ELCO#)
     d  bscd                               like(ELBSCD)

     d kkco#           s                   like(BSCO#)
     d kkkey           s                   like(BSKEY)

     c     bsky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkkey

     c     rsky          chain     prs01
     c                   if        not(%found)
     c                   eval      RSJRST = *blank
     c                   return    ' '
     c                   endif

     c                   eval      kkco# = co#
     c                   movel(p)  bscd          kkkey
     c     bsky          chain     pbs01
     c                   select
     c                   when      BSLPGM = 'L'
     c                   return    'L'
     c                   other
     c                   return    'S'
     c                   endsl

     p rtvDedSize      e
      *=========================================================================
      * rtvSmDedInfo  - retrieves DDAMPO and DDAMAG
      *=========================================================================
     p rtvSmDedInfo    b
     d rtvSmDedInfo    pi
     d  ampo                               like(FMAM01)
     d  amag                               like(FMAM01)

     c                   move      ELCO#         qqco#
     c                   move      ELFND         qqfnd
     c                   move      ELEMP#        q1emp#
     c                   move      ELDIV         q1div
     c                   move      ELFYR         q1fyr
     c                   move      ELDCEF        q2date

     c                   call      'SREFFD'
     c                   parm                    qqco#             3
     c                   parm                    qqfnd             3
     c                   parm                    q1fyr             3
     c                   parm                    q1emp#            9
     c                   parm                    q1div             5
     c                   parm                    q2date            7

     c                   move      q2date        xxdt
     c     ddky          setgt     pdd01
     c     ddky1         readpe    pdd01
     c                   dow       not(%eof) and DDSTAT <> 'A'
     c     ddky1         readpe    pdd01
     c                   enddo

     c                   if        not(%eof)
     c                   eval      ampo = DDAMPO
     c                   eval      amag = DDAMAG
     c                   endif

     p rtvSmDedInfo    e
      *=========================================================================
      * rtvLgDedInfo - retrieves LDAMPO and LDAMAG
      *=========================================================================
     p rtvLgDedInfo    b
     d rtvLgDedInfo    pi
     d  ampo                               like(FMAM01)
     d  amag                               like(FMAM01)

     c     elky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
     c                   kfld                    ELFYR

     c     elky          chain     pld01
     c                   if        %found
     c                   eval      ampo = LDAMP1
     c                   eval      amag = LDAMG1
     c                   endif

     p rtvLgDedInfo    e
