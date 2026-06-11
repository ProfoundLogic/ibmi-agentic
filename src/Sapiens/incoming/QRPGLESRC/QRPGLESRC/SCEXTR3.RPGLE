     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H/TITLE SCEXTR3 - SELECT COMPANY TO PROCESS
     H*
     H*****************************************************************
     H* Created on 07/27/12 by c:280 for Project 114IHH               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FSCEXTF3   CF   E             WORKSTN
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     D*
     D OPTNAM          C                   CONST('Darts Extrac-
     D                                     t - SIS CLAIMS')
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@INKC            1
     C                   PARM                    @@COMP            2
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              99
     C*
     C     *IN99         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKY          CHAIN     PCO01                              99
     C*
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVEL(P)  OPTNAM        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C                   dow       not *inkc
     C*
     C                   EXFMT     SCR01
     C*
     C                   if        *inkc
     C                   eval      @@inkc = *on
     C                   leave
     C                   endif
     C*
     C*   validate company
     C*
     C                   if        scco# = *zeros
     C                   eval      *in10 = *on
     C                   else
     C                   move      scco#         @@comp
     C                   leave
     C                   endif
     C*
     C                   enddo
     C*
     C                   eval      *inlr = *on
     C*
