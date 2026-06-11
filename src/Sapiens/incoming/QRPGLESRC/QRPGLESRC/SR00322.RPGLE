     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR00322 - LOAD ENDORSEMENT FORM WORKFILE FOR FORM WC000322
      //===============================================================*
      // WC000322 - Employee Leasing Client Exclusion Endorsement      *
      //                                                               *
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      //===============================================================*
      // REVISIONS:                                                    *
      //                                                               *
      // 10/26/22 C:333 Project: WC2834 (Recompile)                    *
      //                Endorsement Recognition of Cancellation.       *
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
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
     FWXFMP1    IF   E           K DISK
     F                                     Rename(PFM01:PFM02)

      /COPY SCOPYEN01D

     D qqForm          s             15A   Inz('WC000322  ')

     D ADDPGM          C                   CONST('SR00322I  ')

     D SR00322         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR00322         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1


     C     FMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFRM1

      /COPY SCOPYEN01C

      *=========================================================================
      *  OTHINF - Subroutine to load form specific information
      *=========================================================================
     C     OTHINF        BEGSR
      *
     C                   MOVEL(P)  XXFORM        XXFRM1           15
     C     FMKY          CHAIN     PFM02                              60
     C                   MOVEL     F3C201        FMC201
     C                   MOVEL     F3C401        FMC401
      *
     C                   MOVEL     F3C102        XXZP1A            5
     C                   MOVE      F3C102        XXZP1B            4
     C     XXZP1B        IFNE      *ZEROS
     C     XXZP1A        CAT       '-':0         XXZP1            10
     C     XXZP1         CAT       XXZP1B:0      XXZP1
     C                   ELSE
     C                   MOVEL     XXZP1A        XXZP1
     C                   ENDIF
     C     F3C402        IFNE      *BLANKS
     C                   MOVEL     F3C402        FMC402
     C     F3C202        CAT       ',':0         FMC403
     C     FMC403        CAT       F3C101:1      FMC403
     C     FMC403        CAT       XXZP1:1       FMC403
     C                   ELSE
     C     F3C202        CAT       ',':0         FMC402
     C     FMC402        CAT       F3C101:1      FMC402
     C     FMC402        CAT       XXZP1:1       FMC402
     C                   ENDIF
      *
     C                   MOVEL     F3C203        FMC203
     C                   MOVEL     F3C403        FMC404
     C                   MOVEL     F3C104        XXZP2A            5
     C                   MOVE      F3C104        XXZP2B            4
     C     XXZP2B        IFNE      *ZEROS
     C     XXZP2A        CAT       '-':0         XXZP2            10
     C     XXZP2         CAT       XXZP2B:0      XXZP2
     C                   ELSE
     C                   MOVEL     XXZP2A        XXZP2
     C                   ENDIF
     C     F3C404        IFNE      *BLANKS
     C                   MOVEL     F3C404        FMC405
     C     F3C204        CAT       ',':0         FMC406
     C     FMC406        CAT       F3C103:1      FMC406
     C     FMC406        CAT       XXZP2:1       FMC406
     C                   ELSE
     C     F3C204        CAT       ',':0         FMC405
     C     FMC405        CAT       F3C103:1      FMC405
     C     FMC405        CAT       XXZP2:1       FMC405
     C                   ENDIF
      *
     C                   MOVEL     F3C205        FMC205
     C                   MOVEL     F3C405        FMC407
     C                   MOVEL     F3C106        XXZP3A            5
     C                   MOVE      F3C106        XXZP3B            4
     C     XXZP3B        IFNE      *ZEROS
     C     XXZP3A        CAT       '-':0         XXZP3            10
     C     XXZP3         CAT       XXZP3B:0      XXZP3
     C                   ELSE
     C                   MOVEL     XXZP3A        XXZP3
     C                   ENDIF
     C     F3C406        IFNE      *BLANKS
     C                   MOVEL     F3C406        FMC408
     C     F3C206        CAT       ',':0         FMC409
     C     FMC409        CAT       F3C105:1      FMC409
     C     FMC409        CAT       XXZP3:1       FMC409
     C                   ELSE
     C     F3C206        CAT       ',':0         FMC408
     C     FMC408        CAT       F3C105:1      FMC408
     C     FMC408        CAT       XXZP3:1       FMC408
     C                   ENDIF
      *
     C                   MOVEL     F3C207        FMC207
     C                   MOVEL     F3C407        FMC410
     C                   MOVEL     F3C108        XXZP4A            5
     C                   MOVE      F3C108        XXZP4B            4
     C     XXZP4B        IFNE      *ZEROS
     C     XXZP4A        CAT       '-':0         XXZP4            10
     C     XXZP4         CAT       XXZP4B:0      XXZP4
     C                   ELSE
     C                   MOVEL     XXZP4A        XXZP4
     C                   ENDIF
     C     F3C408        IFNE      *BLANKS
     C                   MOVEL     F3C408        FMC411
     C     F3C208        CAT       ',':0         FMC412
     C     FMC412        CAT       F3C107:1      FMC412
     C     FMC412        CAT       XXZP4:1       FMC412
     C                   ELSE
     C     F3C208        CAT       ',':0         FMC411
     C     FMC411        CAT       F3C107:1      FMC411
     C     FMC411        CAT       XXZP4:1       FMC411
     C                   ENDIF
      *
     C                   ENDSR
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     PGMSTR        BEGSR
     C                   ENDSR                                                  PGMSTR
      *=========================================================================
      *  PGMEND - End of program processing
      *=========================================================================
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
     C     VALID         BEGSR
     C                   MOVE      *ON           CRTFRM            1            FOUND ONE
     C                   ENDSR                                                  VALID
