    H*****************************************************************
    H*Copyright('(c) 2023 Sapiens. All Rights Reserved.')
    H*****************************************************************
    H/TITLE FA742R - DELETE MESSAGES/EOB CODES
    H*****************************************************************
    H**   PROGRAM DESCRIPTION:                                      **
    H**     This program removes adjudication messages from FDMGP   **
    H**     (Bill Entry Message File) and EOB Codes from FDDEP.     **
    H**                                                             **
    H*      This program supports the following adjudications       **
    H**        . State Adjudication                                 **
    H**        . Pre/Re-Certification                               **
    H**        . CCI                                                **
    H**        . CodeReview                                         **
    H**                                                             **
    H**   PROGRAMMER: C:79                                          **
    H**   CREATED: 11/03/04         PROJECT: MC257                  **
    H**                                                             **
    H**   CALLED FROM:                                              **
    H**     U0066R - Pre-cert program                               **
    H**                                                             **
    H**   CALLS TO:                                                 **
    H**     NONE                                                    **
    H**                                                             **
    H*****************************************************************
    H** REVISIONS:                                                  **
     H*  07/18/23 C:342 Project: NY060                                *
     H*                 NY Compliance: CARC/RARC Update               *
     H*                                                               *
    H*****************************************************************
    H*
     Ffdmgl4    uf   e           k disk
     Ffddel3    uf   e           k disk
    D*
    D* ---------------------------------------------------------------
    D* Tables and arrays
    D* ---------------------------------------------------------------
    D*
     D pc              s              1    dim(5)
    D*
    D* ---------------------------------------------------------------
    D* External System Information Data Structure
    D* ---------------------------------------------------------------
     D fids01        e ds                  extname(szq2p)
     D psds          esds                  extname(szq1p)
    D*
    D*----------------------------------------------------------------
    D* System date and time
    D*----------------------------------------------------------------
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@case          s              7
     D @@co#           s              3
     D @@del1          s              1
     D @@del2          s              1
     D @@del3          s              1
     D @@del4          s              1
     D @@del5          s              1
     D @@eob#          s              9
     D @@fnd           s              3
     D @@fyr           s              3
     D @@rcn           s              3
     D @@sub#          s              5
     D kkcase          s                   like(mgcase)
     D kkco#           s                   like(mgco#)
     D kkeob#          s                   like(mgeob#)
     D kkfnd           s                   like(mgfnd)
     D kkfyr           s                   like(mgfyr)
     D kkrcn           s                   like(mgrcn)
     D kksub#          s                   like(mgsub#)
     D xxmsg#          s              5
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     /EJECT
    C*----------------------------------------------------------------
    C*        Entry parm list
    C*----------------------------------------------------------------
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@sub#
     C                   parm                    @@fnd
     C                   parm                    @@fyr
     C                   parm                    @@case
     C                   parm                    @@eob#
     C                   parm                    @@rcn
     C                   parm                    @@del1
     C                   parm                    @@del2
     C                   parm                    @@del3
     C                   parm                    @@del4
     C                   parm                    @@del5
    C*
    C* ---------------------------------------------------------------
    C*        Key Lists
    C* ---------------------------------------------------------------
    C*          PMG01 - Bill Entry Message File by DCS#
     C     mgky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrcn
    C*
     C     deky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrcn
    C*
    C*
    C*================================================================
    C*================================================================
    C*          M A I N L I N E
    C*================================================================
    C*================================================================
    C*
    C*    Execute SR00 for initialization
    C*    -------------------------------
     C                   exsr      sr00
    C*
    C*  Read all records for bill
    C*
     C     mgky          chain     pmg01                              40
     C                   dow       *in40 = *off
    C*
    C*    Delete record if match on a parm value:
    C*    Values are: C - Correct Coding
    C*                P - Pre-Certification
    C*                S - State Adjudication
    C*
     C                   move      mgmsg#        xxmsg#
     C     mgsrc         lookup    pc                                     41
     C                   if        *in41 = *on
     C                             and mgmsg# <> 'NW00001'
     C                   delete    pmg01
     C                   else
     C                   except    rpmg01
     C                   endif
    C*
     C     mgky          reade     pmg01                                  40
     C                   enddo
    C*
    C* DELETE EOB CODES; ONLY IF SOURCE IS A=ADJUDICATION OR N=NETWORK
    C*
     C     deky          chain     pde01                              99
     C                   dow       *in99 = *off
     C                   if        deesrc = 'A'
     C**** Beg Chg *** 07/18/23 **************************************
     C****                         and deeob <> *blanks
     C                             and deeobcd <> *blanks
     C                             or deesrc = 'N'
     C****                         and deeob <> *blanks
     C                             and deeobcd <> *blanks
     C                             or deesrc = 'C'
     C****                         and deeob <> *blanks
     C                             and deeobcd <> *blanks
     C                             or deesrc = 'P'
     C****                         and deeob <> *blanks
     C                             and deeobcd <> *blanks
     C**** End Chg *** 07/18/23 **************************************
     C     deesrc        lookup    pc                                     41
     C*
     C                   if        *in41 = *on
     C                   delete    pde01
     C                   endif
     C*
     C                   else
     C                   except    rpde01
     C                   endif
     C     deky          reade     pde01                                  99
     C                   enddo
    C*
    C*  End of program
     C                   eval      *inlr = *on
    C*
    C*================================================================
    C*================================================================
    C*          S U B R O U T I N E S
    C*================================================================
    C*================================================================
    C*  SR00    Initialization Subroutine
    C*================================================================
    C*
    C*          Get time
    C*          ------------------------------------------------------
     C     sr00          begsr
     C                   time                    dsdttm
    C*
    C*          Move parameters to key fields
    C*          ------------------------------------------------------
     C                   move      @@co#         kkco#
     C                   move      @@sub#        kksub#
     C                   move      @@fnd         kkfnd
     C                   move      @@fyr         kkfyr
     C                   move      @@case        kkcase
     C                   move      @@eob#        kkeob#
     C                   move      @@rcn         kkrcn
    C*
    C*          Move delete parameters to array
    C*          ------------------------------------------------------
     C                   move      @@del1        pc(1)
     C                   move      @@del2        pc(2)
     C                   move      @@del3        pc(3)
     C                   move      @@del4        pc(4)
     C                   move      @@del5        pc(5)
    C*
    C*          End of subroutine SR00
    C*          ------------------------------------------------------
     C     sr00x         endsr
     Opmg01     e            rpmg01
     Opde01     e            rpde01
    H*****************************************************************
