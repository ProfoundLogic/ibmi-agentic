     H**********************************************************************
     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
    H**********************************************************************
    H* PROGRAM: RMVUPLMBRR
    H* DESCRIPTION: REMOVE A MEMBER FROM THE BILL UPLOAD FILES
     H*
    H* This program will retrieve the company number then remove records
    H* in the Upload files: FXU0P, FAU1P, FAU2P, and FAU9P. It clears the
     H* source invoice number in both EOB files: FAEOP and FAEAP.  This is
     H* all based on a particular member name that is passed in.
     H**********************************************************************
     H* CREATED: 08/04/10 C: 079 SUPPORT: 73795
     H**********************************************************************
    H*  REVISIONS:                                                        *
     H*                                                                    *
    H*  06/18/20 C: 079 Support: 99/89658                                 *
    H*                  Use FAU9PARC not FAU9P to archive member          *
     H**********************************************************************
     F*
     FFXU0P     UF   E             DISK
     FFAU1P     UF   E             DISK
     FFAU2L3    UF   E           K DISK
     F*** Beg Chg *** 06/18/20 *********************************************
     F***FAU9L2    UF A E           K DISK
     FFAU9L2    UF   E           K DISK
     F*** End Chg *** 06/18/20 *********************************************
     FFAEOL31   UF   E           K DISK
     FFAEAL1    UF   E           K DISK
     FFAU9PARC  UF A E           K DISK    Rename(pu901:pu902)
     F*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
      *
     D psds          esds                  extname(szq1p)
      *
     D                 ds
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
HITXXD @@member        s             10A
HITXXD xxmember        s             10A
HITXXD xxmemberP       s             10A
HITXXD kktrid          s                   like(u2trid)
HITXXD kkmbr#          s                   like(u9mbr#)
HITXXD kkco#           s                   like(eoco#)
HITXXD kksub#          s                   like(eosub#)
HITXXD kkfnd           s                   like(eofnd)
HITXXD kkfyr           s                   like(eofyr)
HITXXD kkcase          s                   like(eocase)
HITXXD kkeob#          s                   like(eoeob#)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
HITXXC     *entry        plist
  |  C                   parm                    @@member
     C*
HITXXC     eoky          klist
  |  C                   kfld                    kkco#
  |  C                   kfld                    kkeob#
     C*
HITXXC     eaky          klist
  |  C                   kfld                    kkco#
  |  C                   kfld                    kksub#
  |  C                   kfld                    kkfnd
  |  C                   kfld                    kkfyr
  |  C                   kfld                    kkcase
  |  C                   kfld                    kkeob#
     C*
HITXXC     u2ky          klist
  |  C                   kfld                    kktrid
     C*
HITXXC     u9ky          klist
  |  C                   kfld                    kkmbr#
     C*
     C*=====================================================================
     C*MAIN PROGRAM
     C*=====================================================================
     C*
     C                   eval      xxmember = @@member
     C                   eval      xxmemberP = 'P' + %subst(xxmember:2:9)
     C                   time                    dsdttm
     C*=====================================================================
     C* Delete records in FXU0P
     C*=====================================================================
     C*
     C                   Read      fxu0p                                  99
     C*
     C                   dow       *in99 = *off
     C*
     C                   if        mlname = xxmemberP
     C                   delete    fxu0p
     C                   endif
     C*
     C                   Read      fxu0p                                  99
     C*
     C                   enddo
     C*=====================================================================
     C* Delete records in Header File, FAU1P and Detail File, FAU2P
     C*=====================================================================
     C                   Read      pu101                                  99
     C*
     C                   dow       *in99 = *off
     C*
     C                   if        u1mbr# = xxmember
     C*
     C                   eval      kktrid = u1trid
     C*
     C     u2ky          Chain     pu201                              99
     C*
     C                   dow       *in99 = *off
     C                   delete    pu201
     C     u2ky          reade     pu201                                  99
     C                   enddo
     C*
     C* Clear the source invoice number
     C*
     C                   eval      kkco#  = u1co#
     C                   eval      kksub# = u1sub#
     C                   eval      kkfnd  = u1fnd
     C                   eval      kkfyr  = u1fyr
     C                   eval      kkcase = u1case
     C                   eval      kkeob# = u1eob#
     C*
     C     eaky          chain     pea01                              99
     C                   dow       *in99 = *off
     C                   eval      easinv = *blanks
     C                   movel     q1pgm         eapgm
     C                   z-add     dstime        eatime
     C                   z-add     dsdate        SRDTI
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         eadt
     C                   update    pea01
     C     eaky          reade     pea01                                  99
     C                   enddo
     C*
     C     eoky          chain     peo01                              99
     C                   dow       *in99 = *off
     C                   eval      eosinv = *blanks
     C                   movel     q1pgm         eopgm
     C                   z-add     dstime        eotime
     C                   z-add     dsdate        SRDTI
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         eodt
     C                   update    peo01
     C     eoky          reade     peo01                                  99
     C                   enddo
     C*
     C* Delete the FAU1P record
     C*
     C                   delete    pu101
     C*
     C                   endif
     C*
     C                   Read      pu101                                  99
     C*
     C                   enddo
     C*=====================================================================
     C* Delete records in Transaction History File, FAU9P
     C*=====================================================================
     C                   eval      kkmbr# = xxmember
     C*
     C     u9ky          chain     pu901                              99
     C*
     C                   dow       *in99 = *off
     C                   eval      u9mbr# = 'A' + %subst(u9mbr#:2:9)
     C                   eval      u9name = u9mbr#
     C                   eval      u9user = q1user
     C                   MOVEL     Q1USER        U9USER
     C                   MOVEL     Q1USER        U9IUSR
     C                   MOVEL     Q1PGM         U9PGM
     C                   MOVEL     Q1PGM         U9IPGM
     C                   Z-ADD     DSTIME        U9TIME
     C                   Z-ADD     DSTIME        U9ITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         U9DT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         U9IDT
     C*** Beg Chg *** 06/18/20 ********************************************
     C****************   write     pu901
     C                   write     pu902
     C*** End Chg *** 06/18/20 ********************************************
     C                   delete    pu901
     C     u9ky          Reade     pu901                                  99
     C                   enddo
     C*
     C                   EVAL      *INLR=*ON
     C*
     C                   Return
