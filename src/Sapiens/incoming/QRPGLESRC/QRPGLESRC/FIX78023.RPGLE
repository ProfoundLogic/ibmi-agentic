     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/TITLE FIX78023                                                                      *
     H**************************************************************************************
     H*  Description:  fix program to blank out the network flag for bills inadvertently   *
     H*                sent to Procura                                                     *
     H*                                                                                    *
     H* Created: 09/29/11  Programmer C:280  Project:       Sup Seq#:78023                 *
     H*                                                                                    *
     H**************************************************************************************
     H*                                                                                    *
     H*  Programmer notes:   There is one report generated and it shows                    *
     H*                      which records were deleted in FAEAP.                          *
     H*                                                                                    *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     H*                                                                                    *
     FFAEAP     UF   E             DISK
     F*
     F* EOMB Header Amount Transaction File
     F*
     F*FAESL4    UF   E           K DISK
     FFAESL7    UF   E           K DISK
     F*
     F* EOMB HEADER BY CO#/SUB#/FND/INV/EOB#/SYS
     F*
     FQSYSPRT   O    F  132        printer oflind(*inof)
     F*
     D*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     D*
     DPSDS           ESDS                  Extname(SZQ1P)
     D*
     D                 ds
     D dsdttm                        12  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        6  0 overlay(dsdttm:7)
     C*
     C* FAEAL4 Key List
     C*
     C     esky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkblst
     C*
     C* added field for l7
     C*
     C                   kfld                    kkstcd
     C*
     C* Define Fields
     C*
     C     *like         define    EACO#         kkco#
     C     *like         define    EASUB#        kksub#
     C     *like         define    EAFND         kkfnd
     C     *like         define    EAFYR         kkfyr
     C     *like         define    EACASE        kkcase
     C     *like         define    EAEOB#        kkeob#
     C     *like         define    ESBLST        kkblst
     C     *like         define    ESSTCD        kkstcd
     C*
     C* Paramater for Update
     C*
     C     *entry        plist
     C                   parm                    Updt              1
     C*
     C                   z-add     0             xxcount1          7 0
     C                   z-add     0             xxcount2          7 0
     C*
     C*----------------------------------------------------------------------
     C*  Read through FAEOP file
     C*----------------------------------------------------------------------
     C*
     C* Read records from EOMB Header Amount Transaction File
     C*
     C                   Read      PEA01                                  99
     C*
b01  C                   Dow       *in99 = *off
     C*
     C                   eval       kkco#  = EACO#
     C                   eval       kksub# = EASUB#
     C                   eval       kkfnd  = EAFND
     C                   eval       kkfyr  = EAFYR
     C                   eval       kkcase = EACASE
     C                   eval       kkeob# = EAEOB#
     C                   eval       kkblst = 'P'
     C                   eval       kkstcd = 'PRO'
     C*
     C* Get EOMB Status Change Transaction File
     C*
     C     esky          chain     PES01                              99
     C*
     C* if not found
     C*
b02  C                   if        *in99 = *on
     C*
     C* Update Mode ?
     C*
b03  C                   If        Updt = 'Y'
     C*
     C* Clear sent flag
     C*
     C                   eval      eansnt = *blanks
     C*
     C                   time                    dsdttm
     C*
     C                   eval      EAUSER = q1user
     C                   eval      EAPGM  = q1pgm
     C                   eval      EATIME = dstime
     C                   eval      EADT   = dsdate
     C*
     C                   update    pea01
     C*
e03  C                   Endif
     C*
b03  C                   if        *inof = *on
     C                   except    header
     C                   eval      *inof = *off
e03  C                   endif
     C*
     C                   Except    detail1
     C*
     C* Count records that cleared flag
     C*
     C                   Eval      xxcount1 = xxcount1 + 1
     C*
     C                   Eval      *in90 = *off
     C*
b03  C                   If        *in90 = *on
     C*
     C* Read records from EOMB Status Change Transaction File
     C*
     C     esky          Setll     PES01
     C     esky          Reade     PES01                                  98
     C*
b04  C                   Dow       *in98 = *off
     C*
b05  C                   if        esreas = 'SENT TO PROCURA'
     C*
b06  C                   if        *inof = *on
     C                   except    header
     C                   eval      *inof = *off
e06  C                   endif
     C*
     C* Count records that deleted records
     C*
     C                   Eval      xxcount2 = xxcount2 + 1
     C*
     C                   Except    detail2
     C*
     C* Update Mode ?
     C*
b06  C                   If        Updt = 'Y'
     C*
     C                   Delete    PES01
     C*
e06  C                   Endif
     C*
e05  C                   Endif
     C*
     C     esky          Reade     PES01                                  98
     C*
e04  C                   enddo
     C*
e03  C                   Endif
     C*
e02  C                   Endif
     C*
     C* Read records from EOMB Header Amount Transaction File
     C*
     C                   Read      PEA01                                  99
     C*
e01  C                   enddo
     C*
     C                   except    footer
     C*
     C                   Eval      *inlr = *on
     C*
     C**********************************************************************
     C* *inzsr - Initialization
     C**********************************************************************
     C*
     C     *inzsr        begsr
     C*
     C                   except    header
     C*
     C                   endsr
     C*
     C**********************************************************************
     O*
     Oqsysprt   e            header      3  1
     O                                           61 'Clear Sent FLag FAEAP'
     O                                           86 'and Delete FAESP Records'
     O          e            header         2
     O                                           13 'Company #'
     O                                           28 'Subscriber'
     O                                           38 'Group'
     O                                           47 'Year'
     O                                           57 'Case #'
     O                                           68 'Eomb #'
     O                                           85 'Network Flag'
     O*
     O          e            detail1        1
     O                       eaco#         z      9
     O                       easub#        z     26
     O                       eafnd         z     37
     O                       eafyr               48
     O                       eacase        z     57
     O                       eaeob#        z     69
     O                       eansnt              79
     O*
     O          e            detail2        1
     O                       esco#         z      9
     O                       essub#        z     26
     O                       esfnd         z     37
     O                       esfyr               48
     O                       escase        z     57
     O                       eseob#        z     69
     O                                          113 'Deleted'
     O*
     O          e            footer      2
     O                                           27 'Total records updated'
     O                       xxcount1      j     36
     O                                           60 'Total records deleted'
     O                       xxcount2      j     69
