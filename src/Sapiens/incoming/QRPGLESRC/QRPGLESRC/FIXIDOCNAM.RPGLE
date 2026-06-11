     H* (C) Copyright Progressive Data Solutions, 2005
     H/TITLE FIXEOBSRC                                                                      *
     H**************************************************************************************
     H*  Description:  Convert Lower Case Names to Upper                                   *
     H*                                                                                    *
     H*  Created: 12/15/05   Programmer C:79    Project:129MMP                             *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     FFAW1P     UF   E             DISK
     FFAW2P     UF   E             DISK
     FFAEOL31   UF   E           K DISK
     FSMUPL     IF   E           K DISK

     D*
     D kkco#           s                   like(upco#)
     D kkeob#          s                   like(eoeob#)
     D kkuser          s                   like(upuser)
     D xxidoc          s                   like(eosinv)
     D lo              s             26    inz('abcdefghijklmnopqrstuvwxyz')
     D up              s             26    inz('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
     D*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0

     DPSDS           ESDS                  Extname(SZQ1P)
     C*
     C     eoky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkeob#
     C*
     C     upky          klist
     C                   kfld                    kkuser
     C*
     C*----------------------------------------------------------------------
     C*  Get the file company number
     C*----------------------------------------------------------------------
     C*
     C                   exsr      pgmstr
     C*
     C*----------------------------------------------------------------------
     C*  Read through FAW1P and FAEOP files
     C*----------------------------------------------------------------------
     C                   read      pw101                                  99
     C*
     C                   dow       (*in99 = *off)
     C*
     C* Convert W1TRID, W1IDOC from lower to upper case
     C*
     C     LO:UP         xlate     w1idoc        xxidoc
     C                   eval      w1trid = xxidoc
     C                   eval      w1idoc = xxidoc
     C*
     C                   update    pw101
     C*
     C                   eval      kkeob# = w1eob#
     C*
     C* Convert EOIDOC, EOSINV from lower to upper case
     C*
     C*
     C     eoky          chain     peo01                              99
     C                   if        not *in99
     C     LO:UP         xlate     eoidoc        xxidoc
     C                   eval      eoidoc = xxidoc
     C                   eval      eosinv = xxidoc
     C                   update    peo01
     C                   endif
     C*
     C                   read      pw101                                  99
     C*
     C                   enddo
     C*
     C*----------------------------------------------------------------------
     C*  Read through FAW2P file
     C*----------------------------------------------------------------------
     C*
     C* Convert W2TRID
     C*
     C                   read      pw201                                  99
     C*
     C                   dow       (*in99 = *off)
     C*
     C     LO:UP         xlate     w2trid        xxidoc
     C                   eval      w2trid = xxidoc
     C*
     C                   update    pw201
     C*
     C                   read      pw201                                  99
     C*
     C                   enddo
     C*
     C                   eval      *inlr=*on
     C*
     C                   Return
     C*
     C*
     C*
     C**********************************************************************
     C     pgmstr        begsr
     C**********************************************************************
     C*
     C                   eval      kkuser = q1user
 2   C     upky          chain     pup01                              99
     C*
     C                   if        not *in99
     C                   eval      kkco# = upco#
     C                   endif
     C*
     C                   endsr
     C*
